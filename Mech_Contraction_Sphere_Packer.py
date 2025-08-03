###############################################################################
###                               IMPORT                                    ###
###############################################################################

import math
import time

import SimpleITK as sitk
import numpy as np


###############################################################################
###                                                                         ###
###                           SPHERE PACKER                                 ###
###                                                                         ###
###############################################################################

###############################################################################
###                          MICROSTRUCTURE                                 ###
###############################################################################

class MicrostructureCell(object):

    @property  # access method as if it was an attribute
    def currentVolumeFraction(self):

        current_inclusion_volume = 0.
        for item in self.spherelist:
            current_inclusion_volume += item.volume

        return current_inclusion_volume / self.volume

    def __init__(self, dims):
        self.inputDims = np.array(dims)
        self.dims = np.array(dims)
        self.spatial_dims = len(self.dims)
        assert len(self.dims) == 3, 'only 3D implemented'
        self.volume = np.prod(self.dims)
        self.spherelist = []
        self.number_of_plots = 0

    def periodicDistanceVector(self, p1, p2):
        dp = p1 - p2
        periodicDistanceVector = dp - np.rint(dp / self.dims) * self.dims
        return periodicDistanceVector

    # check collision of new_sphere to all existing spheres
    def checkCollision(self, new_sphere, minimum_distance=0.):

        collision_detected = False
        for sphere in self.spherelist:
            distance_vector = self.periodicDistanceVector(new_sphere.position, sphere.position)
            distance = np.linalg.norm(distance_vector)

            if distance < (new_sphere.radius + sphere.radius + minimum_distance):
                collision_detected = True
                break

        return collision_detected

    def addSphere(self, new_sphere):
        self.spherelist.append(new_sphere)

    def voxelize_and_output(self):
        print('Voxelizing...')
        MyVoxelizer = Voxelizer(MySpherePacker.cell)
        IndexArray = MyVoxelizer.generateVoxelArray(voxel_length=0.5)
        MyWriter = MhaWriter()
        MyWriter.writeArrayToMha(f"microstructure_spheres_wfail{self.number_of_plots}.mha", IndexArray)
        self.number_of_plots += 1


###############################################################################
###                               SPHERE                                    ###
###############################################################################

class Sphere(object):

    @classmethod
    def createFromVolume(cls, position, volume):
        radius = (3. / 4. * volume / math.pi) ** (1. / 3.)
        return cls(position, radius)

    @classmethod
    def createFromRadius(cls, position, radius):
        volume = 4. / 3. * math.pi * radius ** 3.
        return cls(position, volume)

    def __init__(self, position, radius):
        self.position = np.array(position)
        self.positionVelocity = np.zeros(len(self.position))
        self.radius = radius
        self.volume = 4. / 3. * math.pi * self.radius ** 3


###############################################################################
###                MECHANICAL CONTRACTION ALGORITHM                         ###
###                     (Williams-Philipse)                                 ###
###############################################################################

### Possible improvements
### - "backtracking" >> adaptive adjustment of the increase in volume fraction
###                     if no solution was found
### - verlet lists

class MechContSpherePacker(object):

    def __init__(self, dims, maxiter=100000):
        self.numberOfSpheres = 0
        self.targetVolumeFraction = 0.
        self.maxiter = maxiter
        self.stringwidth = len(str(self.maxiter))
        self.cell = MicrostructureCell(dims)

    def placeNewSphereAtRandom(self):
        randomPosition = np.random.random(3) * self.cell.dims
        return Sphere.createFromVolume(randomPosition, self.singleSphereVolume)

    def rescaleByFactor(self, factor):
        for i in range(len(dims)):
            self.cell.dims[i] *= factor
        self.cell.volume = np.prod(self.cell.dims)
        for i in range(len(self.cell.spherelist)):
            self.cell.spherelist[i].position *= factor

    def rescaleToVolumeFraction(self, targetVolFrac):
        factor = np.sqrt(targetVolFrac / (self.cell.currentVolumeFraction))
        #        print(">> scaling factor " + str(round(1./factor,5)))
        self.rescaleByFactor(1. / factor)

    def getBackInside(self, dims, *values):
        for i in range(len(dims)):
            while (values[0][i] <= 0):
                values[0][i] += dims[i]
            while (values[0][i] > dims[i]):
                values[0][i] -= dims[i]

    def removeOverlap(self, stepsize=0.50001, factor=1.0):

        totalEnergy = 0.
        hits = 1
        iteration = 0

        # purge old velocities
        for i in range(len(self.cell.spherelist)):
            self.cell.spherelist[i].positionVelocity = 0

        while (hits > 0 and iteration < self.maxiter):

            iteration += 1
            hits = 0
            totalEnergy = 0.

            # distance between pairs of spheres
            for i in range(len(self.cell.spherelist)):
                for j in range(i + 1, len(self.cell.spherelist)):

                    # distance between positions
                    distanceVector = self.cell.periodicDistanceVector(self.cell.spherelist[i].position,
                                                                      self.cell.spherelist[j].position)
                    distanceNorm = np.linalg.norm(distanceVector)
                    # radii (if spheres would have different radii)
                    radius_i = factor * self.cell.spherelist[i].radius
                    radius_j = factor * self.cell.spherelist[j].radius
                    # delta between spheres
                    delta = distanceNorm - radius_i - radius_j - self.minimumDistance

                    # overlap detected
                    if (delta < 0.):
                        hits += 1
                        totalEnergy += delta ** 2
                        # normalize distanceVector and use as velocity via delta
                        distanceVector *= (-delta / distanceNorm)
                        # define velocity to move spheres apart
                        self.cell.spherelist[i].positionVelocity += distanceVector
                        self.cell.spherelist[j].positionVelocity -= distanceVector

            # move positions
            for i in range(len(self.cell.spherelist)):
                self.cell.spherelist[i].position += stepsize * self.cell.spherelist[i].positionVelocity
                self.getBackInside(self.cell.dims, self.cell.spherelist[i].position)

        #            print(f"iter: {iteration:<{self.stringwidth}}\t hits: {hits:<{5}}\t energy: {totalEnergy:.3f}")

        successful = True

        # check max iteration reached
        if (iteration == self.maxiter):
            successful = False
            raise Exception('could not place circles at ' + str(self.cell.currentVolumeFraction) + ' %')

        return successful

    def packSpheres(self, targetVolumeFraction, numberOfSpheres, minimumDistance=1., numberOfVolFracs=1000):

        self.minimumDistance = minimumDistance
        self.numberOfSpheres = numberOfSpheres
        self.targetVolumeFraction = targetVolumeFraction  # value between [0,1]
        self.singleSphereVolume = self.cell.volume * self.targetVolumeFraction / (self.numberOfSpheres)

        print('===================================================================================================')
        print('WILLIAMS-PHILIPSE SPHEREPACKER (mechanical contraction)')
        print('===================================================================================================')
        print(
            f"target volume fraction: {100. * self.targetVolumeFraction:.2f} %\t initial dimensions: {self.cell.dims}")
        print('---------------------------------------------------------------------------------------------------')

        # genereate spheres with random midopoints in 3D
        for i in range(self.numberOfSpheres):
            new_sphere = self.placeNewSphereAtRandom()
            self.cell.addSphere(new_sphere)

        # make discrete volume fractions for spheres
        volumeFractionList = []
        currentVolFrac = 0.
        volFracStep = targetVolumeFraction / numberOfVolFracs
        while currentVolFrac < self.targetVolumeFraction:
            currentVolFrac += volFracStep
            if currentVolFrac >= self.targetVolumeFraction:
                self.targetVolumeFraction = currentVolFrac
            volumeFractionList += [currentVolFrac]

        # pack spheres via contraction to always meet current volume fraction
        for currentVolFrac in volumeFractionList:
            print("rescaling to " + str(round(100. * currentVolFrac, 2)) + " % volume fraction")
            self.rescaleToVolumeFraction(currentVolFrac)
            successful = self.removeOverlap()
            # if currentVolFrac>0.39:
            #     self.cell.voxelize_and_output()
        print('---------------------------------------------------------------------------------------------------')
        print(f"final volume fraction: {100. * currentVolFrac:.2f} %\t final dimensions: {self.cell.dims}")

        return successful


###############################################################################
###                                                                         ###
###                              VOXELIZER                                  ###
###         (this might be given to the students, see Voxelizer.py)         ###
###                                                                         ###
###############################################################################

class Voxelizer(object):

    def __init__(self, microstructure_cell):
        self.cell = microstructure_cell
        self.voxel_array_shape = (1, 1, 1)
        self.voxel_array = np.zeros((1, 1, 1), dtype=np.uint8)

    def generateVoxelArray(self, voxel_length=1):

        self.voxel_array_shape = tuple([int(dim / voxel_length) for dim in self.cell.dims])

        self.voxel_array = np.zeros(self.voxel_array_shape, dtype=np.uint8)

        start = time.time()

        spheres_to_voxelize = self.cell.spherelist.copy()
        number_of_spheres = len(spheres_to_voxelize)
        number_of_voxelized_spheres = 0
        percentage_of_voxelized_spheres = 0.
        stringwidth = len(str(number_of_spheres))

        while spheres_to_voxelize:
            sphere = spheres_to_voxelize.pop()
            self.voxelizeSphereFast(sphere, voxel_length)
            number_of_voxelized_spheres += 1
            percentage_of_voxelized_spheres = number_of_voxelized_spheres / number_of_spheres * 100

            print(
                f"sphere: {number_of_voxelized_spheres:<{stringwidth}}\t progress: {percentage_of_voxelized_spheres:.3f} %")

        end = time.time()

        print("Voxelizing finished after:", round(end - start, 2), "s")

        volume_fraction = np.sum(self.voxel_array) / np.prod(self.voxel_array_shape)
        print("Volume fraction after voxelizing:", round(100. * volume_fraction, 2), "%")

        return self.voxel_array

    def voxelizeSphere(self, sphere, voxel_length):

        for i in range(self.voxel_array_shape[0]):
            for j in range(self.voxel_array_shape[1]):
                for k in range(self.voxel_array_shape[2]):

                    voxel_position = np.array((i, j, k)) * voxel_length

                    distance_vector = self.cell.periodicDistanceVector(sphere.position, voxel_position)
                    distance = np.linalg.norm(distance_vector)

                    if distance < sphere.radius:
                        self.voxel_array[i, j, k] = 1

    def voxelizeSphereFast(self, sphere, voxel_length):

        index = tuple(np.floor(sphere.position / voxel_length).astype(np.int))
        index_list = [index]

        while len(index_list) > 0:

            index = index_list.pop(0)
            i, j, k = index

            if not self.voxel_array[i, j, k] == 1:

                voxel_position = np.array(index) * voxel_length

                distance_vector = self.cell.periodicDistanceVector(sphere.position, voxel_position)
                distance = np.linalg.norm(distance_vector)

                if distance < sphere.radius:
                    self.voxel_array[i, j, k] = 1
                    index_list += self._makeNewIndices(index)

    def _makeNewIndices(self, index):
        new_index_list = []

        for dim in range(3):
            new_index = list(index)
            new_index[dim] = (new_index[dim] + 1) % self.voxel_array_shape[dim]
            new_index_list.append(tuple(new_index))

            new_index = list(index)
            new_index[dim] = (new_index[dim] - 1) % self.voxel_array_shape[dim]
            new_index_list.append(tuple(new_index))

        return new_index_list


###############################################################################
###                            MHA WRITER                                   ###
###############################################################################

class MhaWriter(object):

    def writeArrayToMha(self, filename, voxel_array):
        image = sitk.GetImageFromArray(voxel_array.T)
        image.SetSpacing(np.array([1., 1., 1.]))
        sitk.WriteImage(image, filename, True)


###############################################################################
###                                                                         ###
###                       "MAIN" PRORAM WHEN RUN                            ###
###                                                                         ###
###############################################################################

if __name__ == "__main__":

    dims = (100, 100, 100)

    ###========================================================================
    ### Williams-Philipse

    MySpherePacker = MechContSpherePacker(dims)
    successful = MySpherePacker.packSpheres(targetVolumeFraction=0.2,
                                            numberOfSpheres=20,
                                            minimumDistance=0.2,
                                            numberOfVolFracs=20)

    ###========================================================================
    ### Voxelizer

    if successful:
        print('===================================================================================================')
        print("packing successful >> initiate voxelizer")
        print('===================================================================================================')
        MyVoxelizer = Voxelizer(MySpherePacker.cell)
        IndexArray = MyVoxelizer.generateVoxelArray(voxel_length=1)
        MyWriter = MhaWriter()
        MyWriter.writeArrayToMha("microstructure_spheres_alok.mha", IndexArray)
