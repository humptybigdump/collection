# This code is to give idea on workings of Lubachevski_Stillinger Algorithm
# Note that other methods and classes(RSA,Sphere etc..) are not included here

class LubaStillSpherePacker(object):

    def __init__(self, dims, maxiter=5000):
        self.maxiter = maxiter
        self.stringwidth = len(str(self.maxiter))
        self.targetVolumeFraction = 0.
        self.numberOfSpheres = 0
        self.singleSphereTargetVolume = 0.
        self.cell = MicrostructureCell(dims)

    def rescaleRadiiToVolumeFraction(self, targetVolFrac):
        factor = np.sqrt(targetVolFrac / self.cell.currentVolumeFraction)
        print(f'rescaling factor {factor}')
        for sphere in self.cell.spherelist:
            sphere.radius *= factor
            sphere.volume *= factor ** 3

    def solveCollision(self, a, b, c):
        d = b ** 2 - 4 * a * c
        if ((b <= 0 or a < 0) and d >= 0):
            result = (-b - np.sqrt(d)) / (2 * a)
            return result
        else:
            return 'no collision'

    # compute sphere collision times using NON-PERIODIC distances
    # PERIODICITY is ensured via cell boundary conditions and periodic sphere images
    def computeMinimumSphereCollisionTime(self):

        minSphereCollisionTime = math.inf
        collidingSpheres = []

        # compare all spheres, including periodic images
        for i in range(len(self.cell.spherelist)):
            for j in range(i + 1, len(self.cell.spherelist)):

                differencePosition = self.cell.spherelist[i].position - self.cell.spherelist[j].position
                differenceVelocity = self.cell.spherelist[i].velocity - self.cell.spherelist[j].velocity

                a = np.linalg.norm(differenceVelocity) ** 2 - (
                            self.cell.spherelist[i].growthRate + self.cell.spherelist[j].growthRate) ** 2
                b = 2 * np.dot(differencePosition, differenceVelocity) - 2 * (
                            self.cell.spherelist[i].growthRate + self.cell.spherelist[j].growthRate) * (
                                self.cell.spherelist[i].radius + self.cell.spherelist[j].radius + self.minimumDistance)
                c = np.linalg.norm(differencePosition) ** 2 - (
                            self.cell.spherelist[i].radius + self.cell.spherelist[j].radius + self.minimumDistance) ** 2

                minNew = self.solveCollision(a, b, c)

                if (minNew == 'no collision' or minNew <= 0.):
                    pass  # no collision >> do nothing
                elif (minNew < minSphereCollisionTime):
                    minSphereCollisionTime = minNew
                    collidingSpheres.clear()
                    collidingSpheres.append(self.cell.spherelist[i])
                    collidingSpheres.append(self.cell.spherelist[j])

        return minSphereCollisionTime, collidingSpheres

    def computeMinimumBoundaryContactTime(self):

        boundaryContactTime = np.zeros(6)
        minBoundaryContactTime = math.inf
        contactingSphere = []
        contactingBoundary = {'bound': 10, 'side': 10}

        for sphere in self.cell.spherelist:
            if (sphere.status == 'primary'):
                for i in range(3):
                    # contact with "0"-boundaries
                    boundaryContactTime[i] = (sphere.radius - sphere.position[i]) / (
                                sphere.velocity[i] - sphere.growthRate)
                    # contact with "L_i"-boundaries
                    boundaryContactTime[i + 3] = (self.cell.dims[i] - sphere.radius - sphere.position[i]) / (
                                sphere.velocity[i] + sphere.growthRate)

            boundaryContactTime[boundaryContactTime <= 0.] = math.inf
            minNewIndex = np.argmin(boundaryContactTime)
            minNew = boundaryContactTime[minNewIndex]

            if (minNew < minBoundaryContactTime):
                minBoundaryContactTime = minNew
                if (minNewIndex < 3):  # "O"-boundaries
                    contactingBoundary['bound'] = minNewIndex
                    contactingBoundary['side'] = -1
                elif (minNewIndex >= 3):  # "L_i"-boundaries
                    contactingBoundary['bound'] = minNewIndex % 3
                    contactingBoundary['side'] = +1
                else:
                    raise Exception('Something wrong when computing minimum boundary contact time')
                contactingSphere.clear()
                contactingSphere.append(sphere)

        return minBoundaryContactTime, contactingSphere, contactingBoundary

    def getVelocities(self, collidingSpheres):

        sphereZERO = collidingSpheres[0]
        sphereONE = collidingSpheres[1]

        differenceCollision = sphereZERO.position - sphereONE.position
        collisionDirection = differenceCollision / np.linalg.norm(differenceCollision)

        velocityZERO_p = np.dot(sphereZERO.velocity, collisionDirection) * collisionDirection
        velocityZERO_t = sphereZERO.velocity - velocityZERO_p
        velocityONE_p = np.dot(sphereONE.velocity, collisionDirection) * collisionDirection
        velocityONE_t = sphereONE.velocity - velocityONE_p

        newVelocities = []
        newVelocities.append(
            velocityONE_p + (sphereZERO.growthRate + sphereONE.growthRate) * collisionDirection + velocityZERO_t)
        newVelocities.append(
            velocityZERO_p - (sphereZERO.growthRate + sphereONE.growthRate) * collisionDirection + velocityONE_t)

        return newVelocities

    def updateVelocities(self, collidingSpheres):

        newVelocities = self.getVelocities(collidingSpheres)

        for sphere in self.cell.spherelist:
            if (sphere.identity == collidingSpheres[0].identity):
                sphere.velocity = newVelocities[0]
            elif (sphere.identity == collidingSpheres[1].identity):
                sphere.velocity = newVelocities[1]
            else:
                pass  # do nothing

    def createSphereImage(self, sphere, boundary, boundarySide, additionalMovement=np.zeros(3)):

        newSphere = copy.deepcopy(sphere)
        newSphere.status = 'secondary'
        # if contact with negative boundary side: image position +L_i
        # if contact with positive boundary side: image position -L_i
        newSphere.position[boundary] -= boundarySide * self.cell.dims[boundary]
        newSphere.position -= additionalMovement
        self.cell.spherelist.append(newSphere)

    # assumption: sphere that crossed boundaries on one side cannot cross boundaries on the other side (e.g. x+ / x- boundary)
    # therefore a maximum of 7 copies (8 spheres in total) could be needed (instead of a total of 27)
    def createPeriodicSphereImages(self, contactingSphere, contactingBoundary):

        boundariesCrossed = int(np.sum(list(map(abs, contactingSphere[0].imageID))))

        # no image yet >> make one image
        if (boundariesCrossed == 0):
            # image one
            contactingSphere[0].imageID[contactingBoundary['bound']] = contactingBoundary['side']
            self.createSphereImage(contactingSphere[0], contactingBoundary['bound'], contactingBoundary['side'])

        # one existing image >> make two more images >> four spheres in total
        elif (boundariesCrossed == 1):
            oldBoundary = np.where(abs(contactingSphere[0].imageID) == 1)[0][0]

            if (oldBoundary == contactingBoundary['bound']):
                # print(f'Already crossed boundary {oldBoundary}')
                pass
            else:
                # image one
                contactingSphere[0].imageID[contactingBoundary['bound']] = contactingBoundary['side']
                self.createSphereImage(contactingSphere[0], contactingBoundary['bound'], contactingBoundary['side'])
                # image two
                additionalMovement = np.zeros(3)
                additionalMovement[oldBoundary] = contactingSphere[0].imageID[oldBoundary] * self.cell.dims[oldBoundary]
                self.createSphereImage(contactingSphere[0], contactingBoundary['bound'], contactingBoundary['side'],
                                       additionalMovement)

        # three existing images >> make four more images >> eight spheres in total
        elif (boundariesCrossed == 2):
            oldBoundaries = np.where(abs(contactingSphere[0].imageID) == 1)[0]
            oldNOTBoundary = np.where(contactingSphere[0].imageID == 0)[0]

            if (oldNOTBoundary != contactingBoundary['bound']):
                # print(f'Already crossed boundaries {oldBoundaries}')
                pass
            else:
                # image one
                contactingSphere[0].imageID[contactingBoundary['bound']] = contactingBoundary['side']
                self.createSphereImage(contactingSphere[0], contactingBoundary['bound'], contactingBoundary['side'])
                # image two
                additionalMovement = np.zeros(3)
                additionalMovement[oldBoundaries[0]] = contactingSphere[0].imageID[oldBoundaries[0]] * self.cell.dims[
                    oldBoundaries[0]]
                self.createSphereImage(contactingSphere[0], contactingBoundary['bound'], contactingBoundary['side'],
                                       additionalMovement)
                # image three
                additionalMovement = np.zeros(3)
                additionalMovement[oldBoundaries[1]] = contactingSphere[0].imageID[oldBoundaries[1]] * self.cell.dims[
                    oldBoundaries[1]]
                self.createSphereImage(contactingSphere[0], contactingBoundary['bound'], contactingBoundary['side'],
                                       additionalMovement)
                # image four
                additionalMovement = np.zeros(3)
                additionalMovement[oldBoundaries[0]] = contactingSphere[0].imageID[oldBoundaries[0]] * self.cell.dims[
                    oldBoundaries[0]]
                additionalMovement[oldBoundaries[1]] = contactingSphere[0].imageID[oldBoundaries[1]] * self.cell.dims[
                    oldBoundaries[1]]
                self.createSphereImage(contactingSphere[0], contactingBoundary['bound'], contactingBoundary['side'],
                                       additionalMovement)

        # seven existing images >> no more
        elif (boundariesCrossed == 3):
            # print('Already crossed all boundaries')
            pass

    def redefinePrimarySpheres(self):

        for sphere in self.cell.spherelist:
            for i in range(len(self.cell.dims)):
                if (sphere.position[i] > self.cell.dims[i]):
                    sphere.status = 'secondary'
                    sphere.imageID[i] = 1
                elif (sphere.position[i] < 0):
                    sphere.status = 'secondary'
                    sphere.imageID[i] = -1
                else:
                    sphere.imageID[i] = 0
            if (sphere.imageID[0] == 0 and sphere.imageID[1] == 0 and sphere.imageID[2] == 0):
                sphere.status = 'primary'

    def deleteImages(self):
        self.cell.spherelist = [sphere for sphere in self.cell.spherelist if sphere.status == 'primary']

    # for debugging
    def doublecheckCollision(self):

        undetectedCollisions = False
        for i in range(len(self.cell.spherelist)):
            for j in range(i + 1, len(self.cell.spherelist)):
                # distanceVector = self.cell.periodicDistanceVector(self.cell.spherelist[i].position, self.cell.spherelist[j].position)
                distanceVector = self.cell.spherelist[i].position - self.cell.spherelist[j].position
                distance = np.linalg.norm(distanceVector)
                if (distance < self.cell.spherelist[i].radius + self.cell.spherelist[j].radius):
                    undetectedCollisions = True
                    print('We have undetected collisions')
                    break

        return undetectedCollisions

    def packSpheres(self, targetVolumeFraction, numberOfSpheres, minimumDistance=1., growthRate=0.1, growth='uniform'):

        # ----------------------------------------------------------------------
        # initialization
        # ----------------------------------------------------------------------

        # read input
        assert targetVolumeFraction <= 1
        self.targetVolumeFraction = targetVolumeFraction

        assert isinstance(numberOfSpheres, int)
        self.numberOfSpheres = numberOfSpheres

        self.growthRate = growthRate
        self.minimumDistance = minimumDistance
        # for debugging only .. and only if growth rate is equal for all spheres
        self.singleSphereTargetVolume = self.cell.volume * self.targetVolumeFraction / self.numberOfSpheres

        print('===================================================================================================')
        print('LUBACHEVSKY-STILLINGER SPHEREPACKER (molecular dynamics)')
        print('===================================================================================================')
        print(
            f"target volume fraction: {100. * self.targetVolumeFraction:>3.2f} %\t target number of spheres {self.numberOfSpheres} \t target cell dimensions {self.cell.dims}")
        print('---------------------------------------------------------------------------------------------------')

        # initialize sphere packing using RSA
        print("Initialization via RSA")
        InitialPacking = RSASpherePacker(self.cell.dims)
        successful = InitialPacking.packSpheres(volume_fraction=0.0, number_of_spheres=self.numberOfSpheres,
                                                minimum_distance=self.minimumDistance)

        # add all spheres from initial spherelist from RSA to LS spherelist
        if (successful == True):
            self.cell.spherelist.extend(InitialPacking.cell.spherelist)
            print("Initialization via RSA successful!")
        else:
            successful = False
            raise Exception("Initialization via RSA failed!")

        # initialize sphere attributes
        for sphere in self.cell.spherelist:
            # velocities randomly in range [-1,1]
            sphere.velocity = np.random.uniform(-1, 1, (3))
            sphere.radius = 0.
            if (growth == 'uniform'):
                sphere.growthRate = self.growthRate
            elif (growth == 'random'):
                sphere.growthRate = self.growthRate * np.random.random(1)[0]
            else:
                raise Exception("Only uniform and random growth possible")

        # ----------------------------------------------------------------------
        # molecular dynamics
        # ----------------------------------------------------------------------

        # move spheres, compute collisions and boundary contacts
        currentVolumeFraction = self.cell.currentVolumeFraction
        iteration = 0
        iterSphere = 0
        iterBoundary = 0

        while (currentVolumeFraction < self.targetVolumeFraction):

            minSphereCollisionTime, collidingSpheres = self.computeMinimumSphereCollisionTime()
            minBoundaryContactTime, contactingSphere, contactingBoundary = self.computeMinimumBoundaryContactTime()
            timeStep = min(minSphereCollisionTime, minBoundaryContactTime)
            # print(f'time step {timeStep:.4f}')

            # update sphere attributes
            for sphere in self.cell.spherelist:
                sphere.position += timeStep * sphere.velocity
                sphere.radius += timeStep * sphere.growthRate
                sphere.volume = 4. / 3. * math.pi * sphere.radius ** 3

            # sphere collision >> update velocities of colliding spheres and images
            if (timeStep == minSphereCollisionTime):
                self.updateVelocities(collidingSpheres)
                iterSphere += 1

            # boundary contact >> copy sphere periodically
            elif (timeStep == minBoundaryContactTime and contactingSphere[0].status == 'primary'):
                self.createPeriodicSphereImages(contactingSphere, contactingBoundary)
                iterBoundary += 1

            # error
            else:
                raise Exception('Damn .. something went wrong with the minimum time (*_*) ')

            iteration = iterSphere + iterBoundary

            # for debugging only
            # self.doublecheckCollision()

            # update volume fraction
            currentVolumeFraction = self.cell.currentVolumeFraction
            print(
                f"vol frac: {100. * currentVolumeFraction:>5.2f} %\t time step {timeStep:>6.5f}\t sphere colli {iterSphere:>3}\t boundary cont {iterBoundary:>3}\t iter {iteration:>3}")

            # check iterations
            if (iteration == self.maxiter):
                raise Exception('Maximum iterations exceeded (*_*) ')

        # ----------------------------------------------------------------------
        # post-processing
        # ----------------------------------------------------------------------

        # redefine primary spheres and delete images
        self.redefinePrimarySpheres()
        self.deleteImages()

        # final rescaling
        self.rescaleRadiiToVolumeFraction(self.targetVolumeFraction)

        # print it baby!
        print('---------------------------------------------------------------------------------------------------')
        print(
            f"final volume fraction: {100. * currentVolumeFraction:>3.2f} %\t final number of spheres {len(self.cell.spherelist)}\t final cell dimensions {self.cell.dims}")
        print('---------------------------------------------------------------------------------------------------')
        self.cell.spherelist.sort(key=lambda sphere: sphere.identity, reverse=False)
        for sphere in self.cell.spherelist:
            print(
                f'ID: {sphere.identity:>3}\t radius: {sphere.radius:>3.2f}\t position: {sphere.position[0]:>3.2f} {sphere.position[1]:>3.2f} {sphere.position[2]:>3.2f}\t status: {sphere.status}')

        return successful