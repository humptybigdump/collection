
 !> Main program calls everything performs calculation writing  output to output.txt  and pictures to   slopeTest-Forces.ps and    slopeTest-Geometry.ps
 !> SpiralSople computes the factor of safety $F$ against sliding along a logarithmic spiral surface. The program includes
 !>  the definition of safety by Fellenius and by the GLE method (both using slices).

    !----------------------------------------------------------------
      program spiralSlope  ! {\large \dred Main Program}
      use globalBlock  ! definition of global variables
      use psTools      ! tools to produce postscript files for graphic results
      implicit none
      character(80) :: inFile,outFile
      real(8) :: getFactorOfSafety       ! a function
      real(8) :: FOS,maxDist
      inFile= 'input.txt' ;    outFile = 'output.txt'
      call readInput(infile,layers,water,dload,eb,deb,o)
      allocate(slices(o%nSlices))
      call psPrepareGeometryFile(o%title,layers(1))  ! set the scale and open the file where the slope geometry will be printed.

      spi%cb = o%xx ;  spi%r0 = o%r
      FOS = getFactorOfSafety(spi%cb(1),spi%cb(2),spi%r0)

      write(*,*) 'Factor of safety =' , FOS
      call writeOutput(outfile,slices,o%nSlices,o)  ! output file as plain text containing factor of safety, spiral and slice forces
      call psDrawSlope(1,layers,o%nLayers,slices,o%nSlices,o )
      call psPrepareForcesFile(o%title,slices,o%nSlices) ! set the scale and open the file where the slice forces will be printed.
      call psDrawPolygonOfForces(2,slices,o%nSlices )
	  write(*,*) 'Results in:  '//trim(outfile)//', '//trim(o%title)//'-Geometry.ps, and '//trim(o%title)//'-Forces.ps'
      end program spiralSlope

 !----------------------------------------------------------------

!> Target  function (to be minimized by Amoeba or sth. similar)
!>  returns the factor of safety for a given sliding surface described by its center $(x_c,y_c)$ and its radius $r_0$.
!>  If the calculation does fails, the return value is F=9.9d9.
!>  Information about geometry of the slope, material properties, etc. is taken from  the module globalBlock.

      function getFactorOfSafety(xc,yc,r0)
      use globalBlock       !   global variables
      use geometricTools    !   module for geometric manipulation
      use psTools           !  produce eps files with geometry in case of error
      use NLSolver          !   module with the solution according to the computation method
      implicit none
      real(8),intent(in) :: xc,yc,r0
      real(8) :: getFactorOfSafety,FS
      real(8) :: slopePts(layers(1)%nxx,2), interPts(2,2),dx,lambda
      real(8), allocatable :: xslices(:), topPts(:,:), bottomPts(:,:)
      integer :: nsp, i
      getFactorOfSafety = 9.9d9                ! this value is returned in case of error
      nsp = layers(1)%nxx                      ! number of points describing the slope surface. layers(1) = top layer
      slopePts(:,:) = layers(1)%xx(1:nsp,:)
      o%thetaLimits = [1/2,2]*pi               ! the spiral  will be approximated between thetaLimits  as a set of linear segments
      o%nSpiralPts = 100                       ! number of linear segments definining the spiral
      spi = generate_spiral( [xc,yc], r0, o )  ! spi is the structure containing the segmets that define the spiral
      call findSlopeSpiralIntersections(spi,slopePts,nsP,interPts,ok)
      if( .not. ok) call Report('Slope-Spiral-Intersection-Problem')
      allocate(    xslices(o%nslices+1), topPts(o%nslices+1,2),bottomPts(o%nslices+1,2)     )    ! top and bottom points of the slices

      dx = (interPts(2,1) -interPts(1,1))/o%nSlices             ! thickness of each slide from the total horizontal distance between first and last slice
      xSlices =  interPts(1,1) + [0:o%nSlices:1]*dx             ! list of $x$ coordinates of the left sides of all slices

      call findTopPoints(slopePts,nsp,xSlices, o%nSlices,topPts,ok)  ! intersections of the slices with the slope surface
      if (.not. ok)   call Report('Top-Point-Problem')

      call findBottomPoints(spi,xSlices,o%nSlices,topPts,bottomPts,ok) ! intersections of the slices with the sliding surface
      if (.not. ok)  call Report('Bottom-Point-Problem')

      if(dload%nv.ne. 0) call interpolateVectorOfX(dLoad,topPts,o%nSlices)
      select case(o%method(1:3))
         case('FEL') ;  call interpolateVectorOfX(eb,xSlices,o%nSlices)    ! find the inclination of eb for each slice
         case('GLE') ;  call interpolateVectorOfX(deb,xSlices,o%nSlices-1) ! find the inclination of deb for each slice
      end select
      do i=1,o%nslices
         call setSliceGeometry(topPts,bottomPts,o%nslices,slices(i),i)     ! compute area, normal and tangent vectors, lengths, etc. of the slice
         call setSliceParameters(layers,o%nLayers,slices(i),i)             ! set unit weight, cohesion and friction for the slice
         call setSliceRefInterSlice(eb,deb,slices(i),i,o%method(1:3))      ! input interslice force inclination is considered as the reference incl.
         call setSliceConstLoads(layers,o%nlayers,water,dload,slices(i),i) ! compute external loads acting on the slice (not implemented yet)
         call updateSliceFAndLambda(o%F,o%L,slices(i),i,o%method(1:3))     ! ajust the inclination of $\qb, \eb$ to the current $F$ and $\lambda$
      end do
        select case(o%method(1:3))
         case('FEL')
            if(o%method(13:15) .eq. 'GLO') call NLSFellenius(slices,o%nSlices ,FS )                 ! Fellenius global solution
            if(o%method(13:15) .eq. 'SEQ') call solveFelleniusSequential(slices, o%nSlices, FS )    ! Fellenius sequencial solution
         case('GLE')
            if(o%method(13:15) .eq. 'GLO') call NLSGLE(slices,o%nSlices,  FS, lambda)                 ! GLE global solution
            if(o%method(13:15) .eq. 'SEQ') call solveGLESequential(slices, o%nSlices,  FS, lambda)    ! GLE sequential solution
         end select

      getFactorOfSafety = FS
      return

      contains ! private functions see all variables in the main function

      subroutine  Report(aProblem) ! -----------------------------------------------------
      character(*) :: aProblem
      select case( trim(aProblem) )
        case('Slope-Spiral-Intersection-Problem')
         write(*,*) 'getFactorOfSafety warning:  cannot find intersection between Slope and Spiral'
         call  psDrawLines(1,slopePts,nsp) ! plot slope surface in the postscript file
         call  psDrawLines(1,spi%p,spi%np) ! plot segments defining the spiral in the postscript file
         stop 'Slope-Spiral-Intersection-Problem '

        case('Top-Point-Problem')
         write(*,*) 'Warning: findTopPoint was not successful'
         call  psDrawLines(1,slopePts,nsp)
         call  psDrawLines(1,spi%p,spi%np)
         stop 'Top-Point-Problem'

        case('Bottom-Point-Problem' )
        write(*,*) 'Warning: findBottomPoint was not successful'
         call  psDrawLines(1,slopePts,nsp)
         call  psDrawLines(1,spi%p,spi%np)
         stop 'Bottom-Point-Problem'
        case default
        write(*,*) 'Report with unknown Problem', aProblem
        stop 'Reported  unknown Problem in Raport in  getFactorOfSafety'
        end select

      end subroutine  Report

      end function getFactorOfSafety
      !----------------------------------------------------------------
