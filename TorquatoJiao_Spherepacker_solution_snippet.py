def shrinkByLinearPrograming(self, minimumDistance,
                             shrinkingFactorMax=0.01,  # 0.02
                             deltaDispMaxRelative=3,  # 0.1
                             influenceSphereParameter=100 / 26,
                             use_r=False):
    #######################################################################
    ## remarks in analogy to MCM script:
    ## >> to have a minimization problem, we do *(-1) on the original problem
    ## >> shrinkingFactor = 1 - mu
    ## >> here, we solve for "-mu", to have a minimization problem
    ## >> therefore, solution vector x[0] = -mu
    ## >> shrinkingFactor = 1 - x[0]
    ## >> unknowns are mu and displacements xsi
    ## >> note the ordering of the xsi components for matrix A
    #######################################################################

    # number of unknowns
    # 1*shrinking + numberOfSpheres*(deltaX+deltaY+deltaZ)
    # MCM speaking: mu + numberOfSpheres*(xsi[0]+xsi[1]+xsi[2])
    n = 1 + 3 * self.numberOfSpheres

    # number of constraints
    # possible interactions of spheres with spheres (Interpenetration constraints)
    #   Possibly, these will not be filled completely, this assumes comparision of all spheres!
    mSphere = int(0.5 * self.numberOfSpheres * (self.numberOfSpheres - 1))
    # constraints in 3D w.r.t. xsi (displacement of spheres) (Upper and lower bound)
    mBounds = 6 * self.numberOfSpheres
    # +1 due to constraint at mu
    m = mSphere + mBounds + 1

    # assemble optimization system
    # c^Tx -> min
    # w.r.t. A_ub x <= b_ub
    # c due to optimization .. c^Tx just gives mu
    c = np.zeros((n))
    c[0] = 1
    A_ub = np.zeros((m, n))
    b_ub = np.zeros((m))

    counter = 0
    # (I) fill first rows of A and b with interpenetration constraints
    # compare all spheres
    # original_constraint = False
    # while not original_constraint:
    for i in range(len(self.cell.spherelist)):
        for j in range(i + 1, len(self.cell.spherelist)):

            # distance between positions
            distanceVector = self.cell.periodicDistanceVector(self.cell.spherelist[i].position,
                                                              self.cell.spherelist[j].position)
            distanceNorm = np.linalg.norm(distanceVector)
            # radii (if spheres would have different radii)
            radius_i = self.cell.spherelist[i].radius
            radius_j = self.cell.spherelist[j].radius
            # influence sphere
            influenceSphere = (radius_i + radius_j + minimumDistance) * influenceSphereParameter

            # inside influence sphere
            if (distanceNorm < influenceSphere):
                A_ub[counter, 0] = -0.5 * distanceNorm ** 2
                #  sphere i
                A_ub[counter, 1 + i] = -distanceVector[0]
                A_ub[counter, 1 + self.numberOfSpheres + i] = -distanceVector[1]
                A_ub[counter, 1 + 2 * self.numberOfSpheres + i] = -distanceVector[2]
                # sphere j
                A_ub[counter, 1 + j] = distanceVector[0]
                A_ub[counter, 1 + self.numberOfSpheres + j] = distanceVector[1]
                A_ub[counter, 1 + 2 * self.numberOfSpheres + j] = distanceVector[2]
                # right-hand-side (see Eq. (19))
                if use_r:
                    r_ij = 24 * shrinkingFactorMax * shrinkingFactorMax * distanceNorm
                else:
                    r_ij = 0.
                b_ub[counter] = -0.5 * ((radius_i + radius_j + minimumDistance) ** 2 - distanceNorm ** 2) + r_ij
                # Here we could introduce factor R
                # ... see Torquato-Jiao Eq. (20)
                # count
                counter += 1

    # (II) fill A and b with row for maximum mu
    A_ub[counter, 0] = -1
    b_ub[counter] = shrinkingFactorMax

    # (III) fill final rows of A and b with xsi boundaries
    # boundaries of maximum displacements (xsi) in 3D
    for i in range(len(self.cell.spherelist)):
        deltaDispMax = deltaDispMaxRelative * self.cell.spherelist[i].radius

        A_ub[counter + 1 + i, 1 + i] = 1
        b_ub[counter + 1 + i] = deltaDispMax

        A_ub[counter + 1 + self.numberOfSpheres + i, 1 + i] = -1
        b_ub[counter + 1 + self.numberOfSpheres + i] = deltaDispMax

        A_ub[counter + 1 + 2 * self.numberOfSpheres + i, 1 + self.numberOfSpheres + i] = 1
        b_ub[counter + 1 + 2 * self.numberOfSpheres + i] = deltaDispMax

        A_ub[counter + 1 + 3 * self.numberOfSpheres + i, 1 + self.numberOfSpheres + i] = -1
        b_ub[counter + 1 + 3 * self.numberOfSpheres + i] = deltaDispMax

        A_ub[counter + 1 + 4 * self.numberOfSpheres + i, 1 + 2 * self.numberOfSpheres + i] = 1
        b_ub[counter + 1 + 4 * self.numberOfSpheres + i] = deltaDispMax

        A_ub[counter + 1 + 5 * self.numberOfSpheres + i, 1 + 2 * self.numberOfSpheres + i] = -1
        b_ub[counter + 1 + 5 * self.numberOfSpheres + i] = deltaDispMax

    # reduce size of the system
    #        A_ub = np.copy(A_ub[:counter+2+6*self.numberOfSpheres,:])
    #        b_ub = np.copy(b_ub[:counter+2+6*self.numberOfSpheres])

    # solve optimization system
    from cvxopt import matrix, solvers

    A_cvx = matrix(A_ub)
    b_cvx = matrix(b_ub)
    c_cvx = matrix(c)

    solvers.options['show_progress'] = False
    solvers.options['abstol'] = 1e-7
    solvers.options['reltol'] = 1e-7
    solvers.options['feastol'] = 1e-7
    solvers.options['maxiters'] = 1000
    solution = solvers.lp(c_cvx, A_cvx, b_cvx)
    # assert solution['status'] == 'optimal'

    x = solution['x']
    # original_constraint= self.check_original_constraint(x)
    # compute shrinking factor as 1 - mu
    shrinkingFactor = 1 + x[0]

    # translation
    for i in range(len(self.cell.spherelist)):
        self.cell.spherelist[i].position[0] += x[1 + i]
        self.cell.spherelist[i].position[1] += x[1 + self.numberOfSpheres + i]
        self.cell.spherelist[i].position[2] += x[1 + 2 * self.numberOfSpheres + i]

        self.getBackInside(self.cell.dims, self.cell.spherelist[i].position)

    # shrinking
    self.rescaleByFactor(shrinkingFactor)

def packSpheres(self, targetVolumeFraction, numberOfSpheres, minimumDistance=5.):

    self.targetVolumeFraction = targetVolumeFraction  # value between [0,1]
    self.numberOfSpheres = numberOfSpheres

    print('===================================================================================================')
    print('TORQUATO-JIAO SPHEREPACKER (adaptive shrinking cell)')
    print('===================================================================================================')
    print(
        f"target volume fraction: {100. * self.targetVolumeFraction:>3.2f} %\t target number of spheres {self.numberOfSpheres} \t target cell dimensions {self.cell.dims}")
    print('---------------------------------------------------------------------------------------------------')

    # initialize sphere positions with low volume fraction via RSA
    self.initializeWithRSA(RSAvolumeFraction=0.01, minDistance=minimumDistance)

    # get current volume fraction
    currentVolumeFraction = self.cell.currentVolumeFraction

    # shrink cell to reach target volume fraction
    iteration = 0

    print(
        f"iter: {iteration:<{self.stringwidth}}\t volume fraction: {100. * currentVolumeFraction:.2f} %\t dimensions: {self.cell.dims}")

    while (currentVolumeFraction < self.targetVolumeFraction):

        iteration += 1

        if (iteration > self.maxiter):
            raise Exception("Maximum iterations exceeded!")

        self.shrinkByLinearPrograming(minimumDistance)

        currentVolumeFraction = self.cell.currentVolumeFraction
        self.compare_all_spheres()
        print(
            f"iter: {iteration:<{self.stringwidth}}\t volume fraction: {100. * currentVolumeFraction:.2f} %\t dimensions: {self.cell.dims}")
    #self.compare_all_spheres()
    # final rescaling
    self.rescaleToVolumeFraction(self.targetVolumeFraction)
    for i in range(len(self.cell.spherelist)):
        self.getBackInside(self.cell.dims, self.cell.spherelist[i].position)
    currentVolumeFraction = self.cell.currentVolumeFraction
    for i in range(3):

        self.cell.dims[i] = int(self.cell.dims[i])
    print('---------------------------------------------------------------------------------------------------')
    print(f"final volume fraction: {100. * currentVolumeFraction:.2f} %\t final dimensions: {self.cell.dims}")

    successful = True
    return successful

def compare_all_spheres(self):
    # TODO Check for collisions
    collision_detected = False
    collision_list = []
    for i, first_sphere in enumerate(self.cell.spherelist):
        for j in range(i + 1, len(self.cell.spherelist)):
            second_sphere = self.cell.spherelist[j]
            distance_vector = self.cell.periodicDistanceVector(first_sphere.position, second_sphere.position)
            distance = np.linalg.norm(distance_vector)

            if distance < (first_sphere.radius + second_sphere.radius):
                collision_detected = True
                collision_list.append((i, j, -distance + (first_sphere.radius + second_sphere.radius)))
    if collision_detected:
        print(f'Collisions detected on pairs: {collision_list}')