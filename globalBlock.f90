
!> contains global variables to make them visible to different program units
    module globalBlock
      use inData
      implicit none
      type(layer) :: layers(maxLayers), water                           ! structures containing layer information
      type(vectorOfX) :: eb, deb,dLoad                                  ! interslice forces and external distributed loads
      type(controls) :: o                                               ! structure with inf. about number of slices, computation method,...
      type(slice), allocatable :: slices(:)                             ! list of slices
      type(spiral) :: spi                                               ! structure containing a list segmets that aproximates the spiral
      logical :: ok
      end module globalBlock
