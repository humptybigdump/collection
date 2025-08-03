 !
!> geometric routines:    intersectionSegmentSegment, generate_spiral, intersectionSpiralSegment etc

      module geometricTools
      use inData
      use generalTools
      implicit none

      contains  ! ************************************************************************************


!> returns the intersection (x,y)  between the segment 1 (from point f1 to t1) and segment 2 (from point f2 to t2)
      subroutine intersectionSegmentSegment(f1,t1,f2,t2,intersection,ok)
      real(8),dimension(2),intent(in) :: f1,t1,f2,t2
      real(8),dimension(2),intent(out) :: intersection
      logical,intent(out) :: ok
      real(8) :: s1(2),s2(2), u1,u2

      s1 = rotate90CCW(t1-f1);   s2 = rotate90CCW(t2-f2)
        if( abs(dot_product(s1,s2)) .approx. 1 ) then;  ok = .false.;  return; endif   ! segments are parallel
        u1 = dot_product((f2-f1), s2)/(dot_product(t1-f1,s2))
        u2 = dot_product((f1-f2), s1)/(dot_product(t2-f2,s1))
      if( (u1 .between. [0,1])  .and.  (u2 .between. [0,1]) ) then;  intersection= (1-u1)*f1 + u1*t1; ok = .true.; return; end if

      end subroutine intersectionSegmentSegment
      !----------------------------------------------------------------

!> returns a structure of type spiral with  o%nSpiralPts laying on the logarithmic spiral
      function  generate_spiral( cb, r0, o )
      real(8), intent(in) :: cb(2), r0
      type(controls), intent(in) :: o
      type(spiral)  generate_spiral
      real(8) :: k,dt, t
      integer :: i
      generate_spiral%cb = cb
      generate_spiral%r0 = r0
      generate_spiral%np = o%nSpiralPts
      k = Tan(o%psi)   !  complicated if more layers with more dilatancies
      dt = (o%thetaLimits(2) -o%thetaLimits(1) ) /(o%nSpiralPts-1)
      do i=1,o%nSpiralPts
         t = o%thetaLimits(1) + (i-1)*dt
         generate_spiral%p(i,:) = cb +  [ cos(t), sin(t)] * r0* exp(k* t)
      enddo
      end function  generate_spiral
      !----------------------------------------------------------------

!> returns the intersection  (x,y)   between a segmet (from p1 to p2) and the spiral structure spi
      subroutine intersectionSpiralSegment(spi,p1,p2,intersection,ok)
        type(spiral),intent(in) :: spi
        real(8),dimension(2),intent(in) :: p1,p2
        real(8), intent(out) :: intersection(2)
        logical,intent(out) :: ok
        integer :: i
        ok = .false.
        do i=1,spi%np-1
           call intersectionSegmentSegment(p1,p2,spi%p(i,:),spi%p(i+1,:),intersection,ok)
             if(ok) return
        end do
      end subroutine intersectionSpiralSegment
      !----------------------------------------------------------------

!> returns the two intersections interPts between slope surface (slopePts) and the spiral structure spi
      subroutine findSlopeSpiralIntersections(spi,slopePts, nslopePts, interPts, ok)
      type(spiral),intent(in) :: spi
      integer, intent(in) :: nslopePts
      real(8),intent(in) :: slopePts(nslopePts,2)
      real(8),intent(out) :: interPts(2,2)
      logical,intent(out) :: ok
      real(8) :: intersectionPoint(2)
      integer :: i, kint

       kint = 0 ! counter of intersection points
         do i=1,nSlopePts-1 ! i=left point of the slope segment
            call intersectionSpiralSegment(spi,slopePts(i,:),slopePts(i+1,:),intersectionPoint,ok)
            if( ok ) then
               kint = kint +1 ; interPts(kint,:)  = intersectionPoint ;  if( kint == 2) return    ! finish when second intersection found
            end if
         end do
      ok = .false.
      end subroutine findSlopeSpiralIntersections
      !----------------------------------------------------------------

 !> returns a list of intersection points between the slope surface and the vertical slice boundaries
      subroutine findTopPoints(slopePts,nslopePts,xSlices, nSlices,topPts,ok)
      integer,intent(in) :: nslopePts, nslices !< number of point of the slope ; number of slices
      real(8),intent(in) :: slopePts(nslopePts,2),xSlices(nSlices+1)
      real(8),intent(out) :: topPts(nSlices+1,2)
      logical,intent(out) :: ok
      real(8) :: interPoint(2), x,x1,x2,alpha
      integer :: iseg,islice,itp
      ok = .true.
      itp = 0
      do islice=1,nSlices+1
         x = xSlices(iSlice)
         do iseg=1,nslopePts-1
            x1 = slopePts(iseg,1)
            x2 = slopePts(iseg+1,1)
                  if ( x  .between. [x1,x2]  ) then   ! the segment does not include its end point
                     alpha = (x-x1)/(x2-x1)
                     topPts(iSlice,:)=  (1-alpha)*slopePts(iseg,:)+alpha* slopePts(iseg+1,:)
                     itp = itp+1
                     exit  ! top point found. move to the next slice
                  end if
         end do
      end do
      if(itp .ne. nSlices+1) ok = .false.
      return
      end subroutine findTopPoints
      !----------------------------------------------------------------

!> returns an intersection (xint,yint) between the spiral spi and the vertical line x=const with the smalles value of yint
      subroutine  intersection_spiral_X_low(spi, x, intersection, ok)
      type( spiral ), intent(in) :: spi
      real(8), intent(in) :: x
      logical, intent(out) :: ok
      real(8), intent(out) :: intersection(2)
        real(8) :: alpha
        integer :: i
        ok = .false.
        do i=1,spi%np-1
           if( x .between.    [ spi%p(i,1) ,  spi%p(i+1,1) ] ) then
                alpha = (x - spi%p(i,1) ) / ( spi%p(i+1,1) - spi%p(i,1) )
                intersection  = (1-alpha) *spi%p(i,:) +  alpha*spi%p(i+1,:)
                ok = .true.
                return
             end if
        end do
      end subroutine intersection_spiral_X_low
      !----------------------------------------------------------------

!> returns an intersection(xint,yint) between the spiral spi and the vertical line x=const with the largest value of yint
      subroutine  intersection_spiral_X_high(spi, x, intersection, ok)
      type( spiral ), intent(in) :: spi
      real(8), intent(in) :: x
        logical, intent(out) :: ok
      real(8), intent(out) :: intersection(2)
        real(8) :: alpha
        integer :: i
        ok = .false.
        do i=1,spi%np-1
           if( x .between. [ spi%p(i+1,1), spi%p(i,1)] ) then   !  originally was slightly different:  spi%p(i,1) >= x  .and.  x > spi%p(i+1,1)
            alpha = (x - spi%p(i,1) ) / ( spi%p(i+1,1) - spi%p(i,1) )
                  intersection  = (1-alpha)*spi%p(i,:)+ alpha* spi%p(i+1,:)
                  ok = .true.
                  return
             end if
        end do
      end subroutine intersection_spiral_X_high
      !----------------------------------------------------------------


!> returns a list of intersection points between the slip surface and the vertical slice boundaries
!> if the slice boundary intersects the spiral twice, then a error is issued
      subroutine findBottomPoints(spi,xSlices,nSlices,topPts,bottomPts,ok)
      type( spiral ), intent(in) :: spi
      integer,intent(in) :: nslices
      real(8),intent(in) :: xSlices(nSlices+1)
      real(8),intent(in) :: topPts(nSlices+1,2)
      real(8),intent(out) :: bottomPts(nSlices+1,2)
      logical,intent(out) :: ok
      real(8) :: intersection(2)
      integer :: i
      do i=1,nSlices+1
         call intersection_spiral_X_high(spi, xSlices(i), intersection, ok)
         if(ok .and. intersection(2) < topPts(i,2)) then
            ok = .false.
            return   ! spiral intersects the slice twice
         end if
         call intersection_spiral_X_low(spi, xSlices(i), intersection, ok)
         if(.not. ok) return
         bottomPts(i,:) = intersection
      end do
      end subroutine findBottomPoints
      !----------------------------------------------------------------

!> set the values of the normal and tangent unit vectors and the length of each edge of the slice
      subroutine setSliceGeometry(topPts,bottomPts,nSlice,s,iSlice)
      integer,intent(in) :: nSlice,iSlice
      real(8),intent(in):: topPts(nSlice+1,2),bottomPts(nSlice+1,2)
      type(slice),intent(inout) :: s
      real(8) ::  p1(2),p2(2)
      integer :: i
      s%points(1:2,:) = bottomPts(iSlice:iSlice+1,:)
      s%points(3,:)   = topPts(iSlice+1,:)
      s%points(4,:)   = topPts(iSlice,:)
      do i=1,4
         p1=s%points(i,:)
         if(i<4) p2=s%points(i+1,:)
         if(i==4) p2=s%points(1,:)
         s%lengths(i) = norm(p1-p2)
         s%tangents(i,:) = normalized(p1-p2)
         s%normals(i,:) = -rotate90CCW(s%tangents(i,:))
      end do
      s%area = getArea(s%points,4)
      end subroutine setSliceGeometry
      !----------------------------------------------------------------

!> this can be complicated when many layers are used
      subroutine setSliceParameters(layers,nLayers,s,iSlice)
      implicit none
      integer,intent(in) :: nLayers,iSlice
      type(layer),intent(in) :: layers(nLayers)
      type(slice),intent(inout) :: s
      s%phi = layers(1)%phi
      s%c = layers(1)%c
      end subroutine setSliceParameters
      !----------------------------------------------------------------

!> saves the inclinations of the interslice forces given in input as the reference inclinations.
!> the actual inclinations are found by modifiying the reference inclinations with  lambda .
      subroutine setSliceRefInterSlice(ebRef,debRef,s,iSlice,method)

      implicit none
      integer,intent(in) :: iSlice
      type(vectorOfX),intent(in) :: ebRef ,debRef
      type(slice),intent(inout):: s
      character(3),intent(in) :: method
      select case(method(1:3))
         case('FEL')
            s%ebLref=normalized(ebRef%vi(iSlice,:))
            s%ebRref=normalized(ebRef%vi(iSlice+1,:))
         case('GLE')
            s%debRef=normalized(debRef%vi(iSlice,:))
      end select
      end subroutine setSliceRefInterSlice
      !----------------------------------------------------------------

!> not implemented  for external loads
      subroutine setSliceConstLoads(layers,nlayers,water,dload,s,iSlice)
      implicit none
      integer,intent(in) :: nlayers,iSlice
      type(layer),intent(in) :: layers(nlayers), water
      type(vectorOfX),intent(in) :: dload
      type(slice),intent(inout) :: s
      s%weight = s%area*layers(1)%gammad
      end subroutine setSliceConstLoads
      !----------------------------------------------------------------

 !> makes the inclination of the interslice forces, friction force and the cohesion force consistent with the current  F  and  lambda
       subroutine updateSliceFAndLambda(F,Lambda,s,iSlice,method)
      implicit none
      real(8),intent(in) :: F,Lambda
      character(3),intent(in) :: method
      type(slice),intent(inout) :: s
      integer,intent(in) :: iSlice
      s%F = F
      s%Lambda = lambda
      s%cohesion = s%c*s%lengths(1)/F
      s%qb = normalized( s%normals(1,:) + s%tangents(1,:)*tan(s%phi)/F  )
      select case (method)
         case ('FEL')
            s%ebL = normalized([s%ebLRef(1),  Lambda*s%ebLRef(2)])
            s%ebR = normalized([s%ebRRef(1),  Lambda*s%ebRRef(2)])
         case ('GLE')
            if( s%debRef(2) .approx. 0) then
               write(*,*) 'Warning: horizontal dEi (independent of lambda)'
            end if
            s%deb = normalized([s%debRef(1),  Lambda*s%debRef(2)])
      end select
      end subroutine updateSliceFAndLambda
      !----------------------------------------------------------------

 !> returns the maximum value of  lambda  allowed according to Coulomb failure criterium
      function getLambdaMax(s,n)
      implicit none
      integer,intent(in) :: n
      type(slice),intent(in) :: s(n)
      real(8) getLambdaMax, lambda
      integer :: i
      getLambdaMax = 9.0d9
      do i=1,n
         lambda = s(i)%debRef(1)*tan(s(i)%phi)/s(i)%debRef(2)
         if(abs(lambda)<getLambdaMax)  getLambdaMax = abs(lambda)
      end do
      end function getLambdaMax
      !----------------------------------------------------------------

 !> interpolates the value the vectors vox(xSlices) at xSlices from its values vox(xInput) at xInput
      subroutine interpolateVectorOfX(vox,xSlices,nSlices)
      type(vectorOfX), intent(inout) :: vox
      integer,intent(in) :: nSlices
      real(8),intent(in) :: xSlices(nSlices+1)
      real(8) :: x,xa,xb,ya,yb,y
      integer :: i,pos,kcomp
      vox%vi(:,:) = 0
      do i=1,nSlices+1
         x = xSlices(i)  ! usually in the middle of the i-th slice
         pos = getPositionInList(vox%x,vox%nv,x)
         if((pos==0) .or. (pos == vox%nv)) cycle
         xa = vox%x(pos)
         xb = vox%x(pos+1)
         do kcomp=1,2
            ya = vox%v(pos,kcomp)
            yb = vox%v(pos+1,kcomp)
            y = ya + ((yb-ya)/(xb-xa))*(x-xa)
            vox%vi(i,kcomp) = y
         end do
      end do
      end subroutine interpolateVectorOfX
      !----------------------------------------------------------------
      end module geometricTools



