!> Module inData contains the definitions of the structures layer,  VectorOfX, controls,   slice and spiral.
!> Methods for filling such structures from input file are also given.

    MODULE inData
      use generalTools
      implicit none
      integer, parameter :: maxReadPoints = 50, maxSlices = 100,  maxlayers = 4, maxSpiralPoints = 200   !< maxima  for static declarations of tables

 !> contains data describing roof geometry and soil properties of a horizontal layer, can be used to define water table
      type layer
         real(8) ,dimension(maxReadPoints,2) :: xx  !< coordinates of the points defining the top boundary of a layer
         integer :: nxx                             !< number of points defining the top boundary
         real(8) :: c                               !< cohesion
         real(8) :: phi                            !<  friction(deg),
         real(8) :: gammar,gammad                  !<   saturated unit weight, dry unit weight
      end type layer

!>   contains a list of vectors v with  a corresponding  apprlication coordinates $x_i$ that is  $ vi(xi)
!>   moreover vi( islice ) is a list interpolated from v(x) for  slices
      type VectorOfX
         integer :: nv                              !< number of vectors that have been read
         real(8) ,dimension(maxReadPoints,2) :: v   !< list of  vectors    v
         real(8) ,dimension(maxReadPoints) :: x     !< list of $x$ coordinates at which vectors are applied
         integer :: ni                              !< number of slices (or slices+1)
         real(8) ,dimension(maxSlices,2) :: vi      !< a representative  vector for slice i (from interpolation)

      end type VectorOfX


 !> contains global options, type of calculation
      type controls
         integer :: nLayers, nSlices, nSpiralPts
         real(8) :: r, dr                           !< r  = radius of the spiral at  theta=0 ,   d r  = initial variation of $r$ (used for minimization of FOS)
         real(8) ,dimension(2) ::  xx , dxx         !< xx= (xc,yc)  center of the spiral, dxx=$( dxc, dyc)$ initial variation of the center (used for minimization of FOS)
         real(8) ::    psi, F, L, lambdamax         !< psi = dilatancy angle(deg), F = current factor of safety, L = current lambda
         real(8) ,dimension(2) ::  thetaLimits      !< = theta_min, theta_max  maximum and minimum angle within the spiral is defined as a sequence of segments
         character(15) :: method  !< four 3-letter keys  separated by a dot
                                  !< 1) GLE or FEL  2)  EFF or TOT  3) OPT or NOP  4) GLO or SEC
                                  !< e.g. FEL.EFF.OPT.GLO means Fellenius + Effective anaylsis + Optimization  + Solution of the global system of equations
         character(80) :: title
      end type controls

!>  contains all data related to a slice: geometry of a quadrilateral, unit vectors
      type slice
         real(8),dimension(4,2):: points, normals, tangents   !< each slice(also the first and last slice) described  by 4 corner points + 4 normal + 4 tangent vectors
         real(8) :: phi,c, area,lengths(4), F=1,lambda=1,dload,uL,uR,uB,uT  !< strength parameters at the bottom and magnitude of the forces
         real(8) ::  weight,cohesion,EL,ER,Q,dE
         real(8),dimension(2) :: ebLref, ebRref,ebL,ebR,debRef,deb,qb !< unit directions of the interslice forces and of the reaction force  Q
      end type slice

!> the slip surface is approximated by a sequence of linear segments
        type spiral
           integer :: np   !< number of points defining the spiral
           real(8), dimension(maxSpiralPoints,2) :: p  !< coordinates of the points defining the spiral
           real(8) :: cb(2), r0, psi,xxIn(2), xxOut(2), thetaIn,thetaOut   !< center c =  (xc,yc), radius r0, and dilatancy psi of the spiral
        end type spiral

      CONTAINS  !******************************************************************************************************************************************

!> reads from  iIn  a single structure of type layer
      function readLayer( iIn )
      integer :: i, iIn
      type (layer) :: readLayer, L
      read(iIn,*) L%nxx
      L%xx(:,:) = 0
      do i=1,L%nxx
         read(iIn,*) L%xx(i,:)
      enddo
      read(iIn,*) L%c,L%phi,L%gammad,L%gammar
      L%phi = L%phi*degree
      readLayer = L
      end function readLayer
 !----------------------------------------------

!> reads from iIn and returns an structure of type VectorOfX
      function readVectorOfX( iIn )
      integer :: i, iIn
      type (VectorOfX) ::  readVectorOfX, A
      read(iIn,*)  A%nv
      A%x(:) = 0
      do i=1, A%nv
         read(iIn,*)  A%x(i),    A%v(i,:)
      enddo
      readVectorOfX = A
      end function readVectorOfX
 !----------------------------------------------

!> reads the unit iIn and returns the structure with controls
      function readControls( iIn )
      integer :: i, iIn
      type (controls) :: readControls,o
      read(iIn,*) o%title
      read(iIn,*) o%method
      read(iIn,*)  o%nLayers,  o%nSlices
      read(iIn,*)  o%r, o%dr, o%xx(:), o%dxx(:), o%psi,o%F, o%L  ;  o%psi = o%psi*degree
      readControls = o
      end function readControls
      !----------------------------------------------------------------

!> reads from inputfilename:
!>         layers  (of type layer)and
!>         water (of type layer)  and
!>         dload (of type vectorofx)  and
!>         directions of interslice forces Ev (for FEL) or their differences dEv  (for GLE)
      subroutine readInput(inputfilename, layers, water, dload, Ev,dEv, o)
      integer :: ilay
      type(layer) :: layers(maxLayers), water
      type (VectorOfX) :: Ev, dEv, dLoad
      type(controls) :: o
      character(40) :: inputfilename
      open(1,err=901,file=inputfilename,status='old')
      o =  readControls( 1 )
      do ilay = 1, o%nLayers
         layers(ilay) =  readLayer(1 )
      enddo
      water  =   readLayer( 1 )
      dload = readVectorOfX( 1 )
      if(o%method(1:3) == 'FEL') Ev = readVectorOfX(1)
      if(o%method(1:3) == 'GLE') dEv = readVectorOfX(1)
      close(1)
      return
  901 stop 'readInput cannot open input file'
      end subroutine readInput
     !----------------------------------------------------------------

!>  writes a plain text file with the location of the spiral, the factor of safety and the forces on each slice
      subroutine writeOutput(outputfileName,slices,n,o)
      character(40) :: outputfilename
      integer, intent(in) :: n
      type(slice),intent(in) :: slices(n)
      type(controls), intent(in) :: o
      integer :: i
      open(3,file=outputfilename)
      write(3,'(A,A)') 'TITLE: ', o%title
      write(3,'(A19,g14.7)') 'FACTOR OF SAFETY = ',slices(1)%F
      write(3,'(3(A,g12.5))') 'SLIP SURFACE: xc =', o%xx(1),'yc =',o%xx(2),'r0 =',o%r
      write(3,'(A)') 'THE FORCES ACTING ON EACH SLICE ARE:'
      write(3,'(A5,5(A8,6X))') 'SLICE', 'W', 'C', 'Q','El','Er'
      do i=1,n
         write(3,'(I4,5g14.5)')i, slices(i)%Weight , slices(i)%cohesion,slices(i)%Q, slices(i)%El,slices(i)%Er
      end do
      close(3)
      end subroutine writeOutput


      END MODULE  inData
