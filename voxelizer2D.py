#########################################################################################
#                                                                                       #
#                                     INFORMATION                                       #
#              Voxelizer for microstructures with spherical inclusions                  #
#                                                                                       #
#########################################################################################
#
# Compatibility with cell object:
#
# As input, the Voxelizer needs a cell object with the following
# attributes/functions (mind spelling and capitalization):
# - dims: Array or tuple of the form (Lx,Ly,Lz) with the cell dimensions
# - spherelist: list of spheres in the microstructure (see below)
# - periodicDistanceVector(p1, p2): Function to compute the periodic
#                                   distance between two points, p1, p2
#                                   given as arrays with ([x,y,z]) coords
#
# Compatibility with sphere object:
#
# The sphere objects in the spherelist of the cell should have
# the following attributes (mind spelling and capitalization):
# - position: Array with the ([x,y,z]) coords of the sphere's center
# - radius: Double with value of the sphere's radius
#
# How to use:
#
# MyVoxelizer = Voxelizer(cell)                                  # init with cell
# IndexArray = MyVoxelizer.generateVoxelArray(voxel_length = 1)  # voxelize with given resolution

# The returned IndexArray is a 3-dimensional integer array where 1s
# mark the position of the spheres and 0s elsewhere.



#########################################################################################
#                                                                                       #
#                                     INFORMATION                                       #
#                                    Writing output                                     #
#                                                                                       #
#########################################################################################
#
# Use the MhaWriter class to write the output
# NOTE: For writing .mha files the SimpleITK package is needed
#       which is not included in Anaconda.
#       To install the package on your private system, enter the following command in your console:
#       conda install -c simpleitk simpleitk
#
# How to use:
#
# MyWriter = MhaWriter()                                # helper class for .mha output
# MyWriter.writeArrayToMha("filename.mha", IndexArray)  # creates .mha output



#########################################################################################
#                                                                                       #
#                                       IMPORT                                          #
#                                                                                       #
#########################################################################################

import numpy as np
import SimpleITK as sitk
import time



###############################################################################
###                                                                         ###
###                              VOXELIZER (2D)                             ###
###         (this might be given to the students, see Voxelizer.py)         ###
###                                                                         ###
###############################################################################

class Voxelizer(object):

    def __init__(self, microstructure_cell):
        self.cell = microstructure_cell
        self.voxel_array_shape = (1,1)
        self.voxel_array = np.zeros((1,1), dtype = np.uint8)

    def generateVoxelArray(self, voxel_length = 1):

        self.voxel_array_shape = tuple([int(dim/voxel_length) for dim in self.cell.dims])

        self.voxel_array = np.zeros(self.voxel_array_shape, dtype = np.uint8)

        start = time.time()

        circles_to_voxelize = self.cell.circlelist.copy()
        number_of_circles = len(circles_to_voxelize)
        number_of_voxelized_circles = 0
        percentage_of_voxelized_circles = 0.
        stringwidth = len(str(number_of_circles))
        i=0
        while circles_to_voxelize:
            circle = circles_to_voxelize.pop()
            print(f"percentage before: {np.sum(self.voxel_array)}")
            self.voxelizeCircleFast(circle, voxel_length)
            print(f"percentage after: {np.sum(self.voxel_array)}")
            number_of_voxelized_circles += 1
            percentage_of_voxelized_circles = number_of_voxelized_circles/number_of_circles * 100
            print(f"circle {circle}: {number_of_voxelized_circles:<{stringwidth}}\t progress: {percentage_of_voxelized_circles:.3f}%")
            MyWriter = MhaWriter()
            MyWriter.writeArrayToMha(f"microstructure_circles{i}.mha", self.voxel_array)
            i+=1
        end = time.time()

        print("Voxelizing finished after:", round(end-start,2), "s")

        volume_fraction = np.sum(self.voxel_array) / np.prod(self.voxel_array_shape)
        print("Volume fraction after voxelizing:", round(100.*volume_fraction,2), "%")

        return self.voxel_array

    def voxelizeCircle(self, circle, voxel_length):

        for i in range(self.voxel_array_shape[0]):
            for j in range(self.voxel_array_shape[1]):

                voxel_position = np.array((i,j)) * voxel_length

                distance_vector = self.cell.periodicDistanceVector(circle.position, voxel_position)
                distance = np.linalg.norm(distance_vector)

                if distance < circle.radius:
                    self.voxel_array[i,j] = 1

    def voxelizeCircleFast(self, circle, voxel_length):

        index = tuple(np.floor(circle.position/voxel_length).astype(np.int))
        index_list = [index]
        temp_voxel_array = np.zeros(self.voxel_array.shape,dtype=np.uint8)
        while len(index_list) > 0:

            index = index_list.pop(0)
            i,j = index

            if not temp_voxel_array[i,j] == 1:

                voxel_position = np.array(index) * voxel_length

                distance_vector = self.cell.periodicDistanceVector(circle.position, voxel_position)
                distance = np.linalg.norm(distance_vector)

                if distance < circle.radius:
                    temp_voxel_array[i,j] = 1
                    index_list += self._makeNewIndices(index)
        self.voxel_array+=temp_voxel_array
    def _makeNewIndices(self, index):
        new_index_list = []

        for dim in range(2):
            new_index = list(index)
            new_index[dim] = (new_index[dim]+1)%self.voxel_array_shape[dim]
            new_index_list.append(tuple(new_index))

            new_index = list(index)
            new_index[dim] = (new_index[dim]-1)%self.voxel_array_shape[dim]
            new_index_list.append(tuple(new_index))

        return new_index_list



###############################################################################
###                            MHA WRITER                                   ###
###############################################################################

class MhaWriter(object):

    def writeArrayToMha(self,filename, voxel_array):
        image = sitk.GetImageFromArray(voxel_array.T)
        image.SetSpacing(np.array([1.,1.]))
        sitk.WriteImage(image,filename,True)

