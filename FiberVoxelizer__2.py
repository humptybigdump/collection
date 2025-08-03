###############################################################################
###                               IMPORT                                    ###
###############################################################################

import math
import time
import SimpleITK as sitk
import numpy as np
###############################################################################
###                                                                         ###
###                              YOUR CODE HERE                             ###
###                                                                         ###
###############################################################################

class MicrostructureCell:
    raise Exception('Implement me!')
class Fiber:
    raise Exception('Implement me, too!')
class RSAFiberPacker:
    raise Exception('Bad case of FOMO!')

###############################################################################
###                                                                         ###
###                              VOXELIZER                                  ###
###                                                                         ###
###############################################################################

class Voxelizer:
    """
    Voxelize fibers into a voxel array. Usage:
        MyVoxelizer = Voxelizer(MyCell)
        IndexArray = MyVoxelizer.generate_voxel_array(voxel_length=1)
    """

    def __init__(self, microstructure_cell):
        self.cell = microstructure_cell
        self.voxel_array_shape = (1, 1, 1)
        self.voxel_array = np.zeros((1, 1, 1), dtype=np.uint8)

    def generate_voxel_array(self, voxel_length=1):
        """
        Generate the voxel array.
        :param voxel_length:
        :return:
        """

        self.voxel_array_shape = tuple([int(dim / voxel_length) for dim in self.cell.dims])

        self.voxel_array = np.zeros(self.voxel_array_shape, dtype=np.uint8)

        start = time.time()

        particles_to_voxelize = self.cell.particles.copy()
        number_of_spheres = len(particles_to_voxelize)
        number_of_voxelized_spheres = 0
        stringwidth = len(str(number_of_spheres))

        while particles_to_voxelize:
            particle = particles_to_voxelize.pop()
            self.voxelize_fiber_prairie_fire(particle, voxel_length)
            number_of_voxelized_spheres += 1
            percentage_of_voxelized_spheres = number_of_voxelized_spheres / number_of_spheres * 100
            print(
                f"Fiber: {number_of_voxelized_spheres:<{stringwidth}}\t "
                f"progress: {percentage_of_voxelized_spheres:.3f}%")
        end = time.time()
        print("Voxelizing finished after:", round(end - start, 2), "s")
        volume_fraction = np.sum(self.voxel_array) / np.prod(self.voxel_array_shape)
        print("Volume fraction after voxelizing:", round(100. * volume_fraction, 2), "%")

        return self.voxel_array

    def voxelize_fiber(self, fiber, voxel_length, debug=False):
        """
        Voxelize a fiber. The fiber needs the properties
            fiber.length
            fiber.position
            fiber.orientation
            fiber.radius

        Very slow! Use only if your room is too cold.

        :param fiber:
        :param voxel_length:
        :param debug:
        :return:
        """
        if debug:
            print(f'Voxilizing fiber at {fiber.position} with orientation {fiber.orientation} '
                  f'(l={np.linalg.norm(fiber.orientation)}) and length {fiber.length}')
        for i in range(self.voxel_array_shape[0]):
            for j in range(self.voxel_array_shape[1]):
                for k in range(self.voxel_array_shape[2]):
                    voxel_position = np.array((i, j, k)) * voxel_length
                    point_fiber = self.point_in_fiber(fiber, voxel_position)
                    if point_fiber:
                        self.voxel_array[i, j, k] = 1

    def voxelize_fiber_prairie_fire(self, fiber, voxel_length):
        """
        Voxelize a fiber using a prarie fire algorithm. The fiber needs the properties
            fiber.length
            fiber.position
            fiber.orientation
            fiber.radius

        Way more effient than the naive implementation.

        :param fiber:
        :param voxel_length:
        :param debug:
        :return:
        """
        voxels_checked = np.zeros(self.voxel_array.shape, dtype=bool)  # Check for and mark overlap
        # Start fire at the center of the fiber
        index = tuple(np.floor(fiber.position / voxel_length).astype(np.int))
        index_list = [index]
        while len(index_list) > 0:
            # Spread the fire from the initial voxel until there is
            #  no flammable voxel (=inside the fiber) anymore
            index = index_list.pop(0)
            i, j, k = index
            if not voxels_checked[i, j, k]:  # Check only unburnt voxels
                voxel_position = np.array(index) * voxel_length
                if self.point_in_fiber(fiber, voxel_position):
                    voxels_checked[i, j, k] = 1
                    if self.voxel_array[i, j, k] > 0:
                        self.voxel_array[i, j, k] = 2
                    else:
                        self.voxel_array[i, j, k] = 1
                    index_list += self._makeNewIndices(index)  # Find all voxels around current voxel

    def point_in_fiber(self, fiber, voxel_position):
        """
        Returns whether a point is inside a fiber on a periodic cell.
        General idea from
        https://stackoverflow.com/questions/47932955/how-to-check-if-a-3d-point-is-inside-a-cylinder
        but adapted to the periodic cell.

        :param fiber:
        :param voxel_position:
        :return:
        """

        pt1 = self.cell.get_back_inside(fiber.position + fiber.length / 2 * fiber.orientation)
        pt2 = self.cell.get_back_inside(fiber.position - fiber.length / 2 * fiber.orientation)
        vec = self.cell.periodicDistanceVector(pt1, pt2)
        is_behind_pt1 = np.dot(self.cell.periodicDistanceVector(voxel_position, pt1), vec) <= 0
        is_behind_pt2 = np.dot(self.cell.periodicDistanceVector(voxel_position, pt2), vec) >= 0
        is_in_radius = np.linalg.norm(np.cross(self.cell.periodicDistanceVector(voxel_position, pt1), vec)) \
                       <= fiber.radius * fiber.length
        is_in_fiber = is_behind_pt1 and is_behind_pt2 and is_in_radius
        return is_in_fiber


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

class MhaWriter:

    def writeArrayToMha(self, filename, voxel_array):
        image = sitk.GetImageFromArray(voxel_array.T)
        image.SetSpacing(np.array([1., 1., 1.]))
        sitk.WriteImage(image, filename, True)


###############################################################################
###                       MAIN                                              ###
###############################################################################

if __name__ == "__main__":

    dims = (50, 50, 50)  # (100, 100, 100)

    ###========================================================================
    ### RSA

    MyFiberPacker = RSAFiberPacker(dims)
    successful = MyFiberPacker.pack_fibers(20, 20, 0.2)
    ###========================================================================
    ### Voxelizer

    if successful:
        print('===================================================================================================')
        print("packing successful >> initiate voxelizer")
        print('===================================================================================================')
        MyVoxelizer = Voxelizer(MyFiberPacker.cell)
        IndexArray = MyVoxelizer.generate_voxel_array(voxel_length=0.25)
        MyWriter = MhaWriter()
        MyWriter.writeArrayToMha("microstructure_fibers.mha", IndexArray)
