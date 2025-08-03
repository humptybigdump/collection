 !>   subtroutines to solve the non-linear system of equations
     module NLSolver
      use inData
      use geometricTools
      implicit none

     contains !*****************************************************************************

!> finds iteratively the factor of safety $F$ from the global system of equations using the Newton-Rapson method.
!> The jacobi matrix is found approximately.
!> In case of multiple solutions, an acceptable one is found, starting from different inital guesses.
      subroutine NLSFellenius(slices,n ,F)
      integer, intent(in) ::  n         !<number of slices
      real(8), intent(out) :: F         ! < factor of safety
      type(slice),intent(inout) :: slices(n)
      real(8)  :: K(2*n,2*n),R(2*n)
      integer :: iter,itry,i
      integer,parameter:: nmaxIter=30,ntry=5
      real(8)  :: x, y, dx, ydy, yp, corr
      real(8),parameter :: tol=1.0d-8
      character(1) :: method ='F'
      x = 2.0; dx = 0.001
      do itry=1,ntry               ! < loop over attempts
          Do iter=1,nmaxIter        !<  loop over iterations
             call Assemble(n,x+dx,1.0d0,slices,method,K,R)    !   x= $F$
             call solveLinSys(K,R)
             ydy = R(2*n)
                   call Assemble(n,x,1.0d0,slices,method,K,R)  !   x= $ F$
             call solveLinSys(K,R)
             y = R(2*n)
             yp = (ydy - y)/dx;
             if(Abs(yp)<1.0d-8) stop 'NLSFellenius: Jacobi = zero'
               corr = -y/yp; x = x+corr
             if(x<0) x = (x -corr) + (x -corr)*sign(0.1d0,corr)  !  <  F<0 : try a  smaller correction in the same direction
             if(abs(y)<tol) exit
          end do  ! end iterations loop
          if(iter==nmaxIter) stop 'NLSFellenius: Too many iterations'
            if(all(R(:)>-1.0d-8)) exit
            ! tension in Qi or Ei
            x = 1.6d0*x            ! look for another root starting from a new initial guess
      end do ! end attempts loop
      if(itry==ntry) stop 'NLSFellenius: Too many attempts'
      F = x ! save the components of R into the Q and Er forces of the slices
      do i=1,n -1
          slices(i)%Q = R( 2*i -1 )
          slices(i)%ER = R( 2*i )
          slices(i+1)%EL = slices(i)%ER
      end do
      slices(n)%Q = R( 2*n -1 )
      slices(n)%ER = R( 2*n )
      end subroutine NLSFellenius
      !----------------------------------------------------------------

!> returns the global system  of equations   K . x =  R
!> from the contributions of each slice.  K   and  R  depend on the solution method (Fellenius or GLE).
      subroutine Assemble(n,F,Lamb,slices,method,K,R)
      integer, intent(in) :: n
      real(8), intent(in) :: F,Lamb  !< facor of safety and lambda for GLE method
      type(slice),intent(inout) :: slices(n)
      character(1) :: method
      real(8), intent(out) :: K(2*n,2*n) , R(2*n)
      integer :: i,i1,i2,i0
      real(8) ::  K22(2,2), K23(2,3),R2(2)               ! local variables
      real(8),dimension(3,3)::  aa, bb ! local matrices
       K(:,:) = 0 ; R(:)=0
      do i=1,n ! loop over slices
         select case( method )
         case('F') ! Fellenius
           call getFelleniusSliceContrib(i,slices(i),F,K23,R2)
           i1 = 2*(i-1)+1; i2= i1+1 ; i0 = i1-1
           if(i==1) then
              K(1:2,1:2) =   K23(1:2,2:3)
           else
              K(i1:i2,i0:i2) =   K23
           end if
           R(i1:i2) = R2
         case('G')  ! GLE
           call getGLESliceContrib(i,slices(i),F,lamb , K22,R2)
           i1 = 2*(i-1)+1; i2= i1+1
           K(i1:i2,i1:i2) =   K22
           R(i1:i2) = R2
        case  default
        stop 'error in Assemble: the method is neither F nor G'
        end select
      end do ! finished loop over slices
      end subroutine Assemble
      !----------------------------------------------------------------

!> finds iteratively the factor of safety  F  and lambda  from the global system with Newton-Rapson.
!> The jacobi matrix is found approximately.
!>  In case of multiple solutions, the acceptable solution is found starting  from different inital guesses.
      subroutine NLSGLE(slices, n,  F,Lamb)
      integer, intent(in) ::  n
      real(8), intent(out) :: F,lamb    !< facor of safety and lambda for GLE method
      type(slice),intent(inout) :: slices(n)
      real(8) :: K(2*n,2*n),R(2*n)
      integer,parameter:: nmaxIter=30,ntry=5
      real(8),parameter :: tol=1.0d-8
      integer:: i,i1,i2,i0,iter,itry
      real(8) :: x,y,dx,dy,z(2), zdz(2),dzdx(2),dzdy(2), dEs(n,2),corr(2), jacobian(2,2),xnew,ynew,aux(2),lambdaMax
      character(1) :: method ='G'
      x = 0.2 ; y=1; dx = 0.001d0; dy = 0.01d0
      lambdaMax=getLambdaMax(slices,n)  ! maximum allowable lambda according to Coulomb criterium
      do itry=1,ntry  ! loop over attempts
       Do iter=1,nmaxIter  ! loop over iterations
          call Assemble(n,x,y,slices,method,K,R)     ! x=F,y=lambda
          do i=1,n  !  remember directions of dE before destroyed in la\_gesv
             i1 = (i-1)*2 + 1 ; i2 = i1+1
             dEs(i,:) = K(i1:i2,i1)
          enddo
          call solveLinSys(K,R)
          z =  matmul(R(1:2*n:2), dEs)
            call Assemble(n,x+dx,y,slices,method,K,R)
          do i=1,n  !  remember directions of dE before destroyed in la\_gesv
             i1 = (i-1)*2 + 1 ; i2 = i1+1
             dEs(i,:) = K(i1:i2,i1)
          enddo
          call solveLinSys(K,R)
          zdz =  matmul(R(1:2*n:2), dEs)
          dzdx = (zdz - z)/dx

          call Assemble(n,x,y+dy,slices,method,K,R)
          do i=1,n  !  remember directions of dE before destroyed in la\_gesv
             i1 = (i-1)*2 + 1 ; i2 = i1+1
             dEs(i,:) = K(i1:i2,i1)
          enddo
          call solveLinSys(K,R)
          zdz  =  matmul(R(1:2*n:2), dEs)
          dzdy = (zdz - z)/dy
          jacobian(: ,1)  = dzdx; jacobian(: ,2)  = dzdy;
          call inv22(jacobian) ; corr  = - matmul(jacobian,z)
          xnew = x + corr(1); ynew = y + corr(2)
          if(xnew<0) xnew = x + x*sign(0.1d0,corr(1))  ! $F<0$: try an smaller correction in the same direction
          if(abs(ynew)>lambdaMax) ynew = y + y*sign(0.1d0,corr(2))  !  lambda > lambda_max : try an smaller correction in the same direction
            if(norm(z) < tol) exit
          x = xnew
          y = ynew
       enddo     ! end of iterations loop
         call Assemble(n,x,y,slices,method,K,R)
         call solveLinSys(K,R)
       if (all(R(2:n-1:2)>0))  exit  !because  $Q_i > 0$ is ok (otherwise  choose another starting point )
       x = 1.6d0*x
      end do  ! end of attempts loop
       if(itry .ge. ntry) stop 'NLSGLE: Too many attempts'
       F = x ! pass the values of R to the slice forces Er and Q
       Lamb = y
       slices(1)%EL = 0
       slices(1)%ebL = 0
       do i=1,n
          slices(i)%Q = R( 2*i )
          slices(i)%dE = R( 2*i -1)
          aux = slices(i)%EL*slices(i)%ebL  - slices(i)%dE*slices(i)%deb
          slices(i)%ebR = normalized(aux)
          slices(i)%ER = norm(aux)
          if(i==n) exit
          slices(i+1)%EL = slices(i)%ER
          slices(i+1)%ebL = slices(i)%ebR
       end do
      end subroutine NLSGLE
      !----------------------------------------------------------------

 !> returns the contribution K23 to the global system matrix and R2 to the global RHS  from the slice $i$ according to the Fellenius method
      subroutine getFelleniusSliceContrib(i,s,F,K23,R2)
      integer,intent(in) :: i
      type(slice), intent(inout) :: s
      real(8),intent(in) :: F
      real(8), intent(out) :: K23(2,3),R2(2)
      real(8),dimension(2) :: ww,cc
      character(3) :: method
      method = 'FEL'
      call updateSliceFAndLambda(F,1.0d0,s,i,method)
      ww = s%weight*[0.0d0,-1.0d0]
      cc = s%cohesion*s%tangents(1,:)
      K23(:,1) = s%ebL(:)
      K23(:,2) = s%qb(:)
      K23(:,3) = -s%ebR(:)
      R2(:) = -(ww+cc)
      end subroutine getFelleniusSliceContrib
      !----------------------------------------------------------------

 !> returns the contribution K22 to the global system matrix and R2  to the global RHS   from the slice $i$ according to the GLE method.
      subroutine getGLESliceContrib(i,s,F,lambda,K22,R2)
      integer,intent(in) :: i
      type(slice), intent(inout) :: s
      real(8),intent(in) :: F,lambda
      real(8), intent(out) :: K22(2,2),R2(2)
      real(8),dimension(2) :: ww,cc
      character(3) :: method
      method = 'GLE'
      call updateSliceFAndLambda(F,lambda,s,i,method)
      ww = s%weight*[0.0d0,-1.0d0]
      cc = s%cohesion*s%tangents(1,:)
      K22(:,1) = s%deb(:)
      K22(:,2) = s%qb(:)
      R2(:) = -(ww+cc)
      end subroutine getGLESliceContrib
      !----------------------------------------------------------------


!> finds the factor of safety F iteratively using the Newton-Rapson. The slices are solved  for a given $F$ sequentially starting from slice 1 with El1=0.
!> The jacobi is found approximately.
!> In case of multiple solutions  find an acceptable one  starting from different inital guesses.
      subroutine solveFelleniusSequential(slices,n,F )
      integer,intent(in) :: n
      type(slice), intent(inout) :: slices(n)
      real(8),intent(out) :: F
      real(8),parameter :: tol=1.0d-8
      real(8) :: dF, R, RdR,dRdF,corr
      integer,parameter:: nmaxIter=30,ntry=5
      integer :: iter,itry
      F = 2.0d0
      dF = 0.001d0
      do itry=1,ntry             ! loop over attempts
            do iter=1,nmaxIter   ! loop over iterations
                 call solveFelleniusSlices(slices,n,F+dF)
                 RdR = slices(n)%ER
                 call solveFelleniusSlices(slices,n,F)
                 R = slices(n)%ER
                 dRdF = (RdR - R)/dF
                 corr = -R/dRdF
                 F = F +corr
                 if(F<0) F = (F -corr) + (F -corr)*sign(0.1d0,corr)  ! F<0: try an smaller correction in the same direction
                 if(abs(R)<tol) exit
            end do   ! end loop over iterations
          if(iter==nmaxIter) stop 'solveFelleniusSequential: Too many iterations'
          if(all(slices(:)%Q>0) .and.  all(slices(:)%ER>0)) exit
           !tension in Qi or Ei
           F = 1.6d0*F  ! find another minimum starting from a larger value of F
      end do ! end loop over attempts
      if(itry==ntry) stop 'solveFelleniusSequential: Too many attempts'
      end subroutine solveFelleniusSequential
      !----------------------------------------------------------------

!>  finds Er and Q of each slice for a given F starting from the slice 1 with $El_1 =0$.
      subroutine solveFelleniusSlices(s,n,F)
      integer, intent(in) :: n
      type(slice),intent(inout) :: s(n)
      real(8),intent(in) :: F
      real(8),dimension(2) :: db,ww,cc,eeL
      integer :: i
      do i=1,n
         call updateSliceFAndLambda(F,1.0d0,s(i),i,'FEL')
         db(:) = rotate90CCW(s(i)%qb)
         if(i==1) s(i)%EL = 0
         if(i>1) s(i)%EL = s(i-1)%ER
         ww = s(i)%weight*[0.0d0,-1.0d0]
         cc = s(i)%cohesion*s(i)%tangents(1,:)
         eeL = s(i)%EL*s(i)%ebL
         s(i)%ER = dot_product((ww + eeL + cc), db) / dot_product(s(i)%ebR,db)
         s(i)%Q = -dot_product((ww + eeL - s(i)%ER*s(i)%ebR + cc) , s(i)%qb)
      end do
      end subroutine solveFelleniusSlices
      !----------------------------------------------------------------

!> finds the factor of safety $F$ and  lambda  iteratively using the Newton-Rapson method.
!> The slices are solved  for a given F and lambda sequentially starting from slice 1.
!> The jacobi matrix is found approximately.
!> In case of multiple solutions find an acceptable one  starting from different inital guesses.
      subroutine solveGLESequential(slices,n,F,lambda )
      integer,intent(in) :: n
      type(slice), intent(inout) :: slices(n)
      real(8),intent(out) :: F,lambda
      real(8),parameter :: tol=1.0d-8
      real(8) :: dF, R(2), dRdF(2),dRdlambda(2),corr(2), jacobian(2,2),&
     &dlambda,Fnew,LambdaNew,aux(2), lambdaMax
      integer,parameter:: nmaxIter=30,ntry=5
      integer :: iter,itry, i
      lambdaMax=getLambdaMax(slices,n)
      F = 2; dF = 0.001d0
      lambda = 1 ; dlambda = 0.01d0
      do itry=1,ntry  ! loop over attempts
         do iter=1,nmaxIter ! loop over iterations
            call solveGLESlices(slices,n,F,lambda)
            R(:) = getSumDE(slices,n)
            call solveGLESlices(slices,n,F+dF,lambda)
            dRdF(:) = (getSumDE(slices,n)  - R(:) )/dF
            call solveGLESlices(slices,n,F,lambda+dlambda)
            dRdlambda(:) = (getSumDE(slices,n) -R(:))/dlambda
            jacobian(:,1) = dRdF(:); jacobian(:,2) = dRdlambda(:)
            call inv22(jacobian) ; corr  = - matmul(jacobian,R)
            Fnew = F + corr(1); lambdanew = lambda + corr(2)
            if(Fnew<0) Fnew = F + F*sign(0.1d0,corr(1))  ! F<0: try an smaller correction in the same direction
            if(abs(lambdanew)>lambdaMax) lambdanew = lambda + sign(0.1d0,corr(2))  ! lambda>lambdamax: try an smaller correction in the same direction
            if(norm(R) < tol) exit
            F = Fnew
            lambda = lambdanew
         end do ! end loop over iterations
         if(iter==nmaxIter) stop 'solveGLESequential: Too many iterations'
         call solveGLESlices(slices,n,F,lambda)  ! set the correct values of F and lambda. otherwise solution would correspond to F, lambda+dlambda
         if(all(slices(:)%Q>0)) exit !  Qi must be positive
         F = 1.6d0*F  ! find another minimum starting from a larger value of F
      end do  ! end loop over attempts
      if(itry==ntry) stop 'solveGLESequential: Too many attempts'
      slices(1)%EL = 0
      slices(1)%ebL = 0
      do i=1,n     ! find Er and El  from dE
          aux = slices(i)%EL*slices(i)%ebL  - slices(i)%dE*slices(i)%deb
          slices(i)%ebR = normalized(aux)
          slices(i)%ER = norm(aux)
          if(i==n) exit
          slices(i+1)%EL = slices(i)%ER
          slices(i+1)%ebL = slices(i)%ebR
      end do
      end subroutine solveGLESequential
      !----------------------------------------------------------------


!> finds dE and Q of each slice for a given F starting from the slice 1.
      subroutine solveGLESlices(s,n,F,lambda)
      integer, intent(in) :: n
      type(slice),intent(inout) :: s(n)
      real(8),intent(in) :: F,lambda
      real(8),dimension(2) :: db,ww,cc
      integer :: i
      do i=1,n
         call updateSliceFAndLambda(F,lambda,s(i),i,'GLE')
         db(:) = rotate90CCW(s(i)%qb)
         ww = s(i)%weight*[0.0d0,-1.0d0]
         cc = s(i)%cohesion*s(i)%tangents(1,:)
         s(i)%dE = -dot_product((ww + cc), db) / dot_product(s(i)%deb,db)
         s(i)%Q = -dot_product((ww + s(i)%dE*s(i)%deb + cc) , s(i)%qb)
      end do
      end subroutine solveGLESlices
      !----------------------------------------------------------------

!> returns the sum of all interslice forces DE  (for GLE Sequential method)
      function getSumDE(slices,n)
      integer, intent(in) :: n
      type(slice), intent(in) :: slices(n)
      real(8) getSumDE(2)                ! sum of all interslice forces DE
      integer :: i
      getSumDE = 0
      do i=1,n
         getSumDE = getSumDE + slices(i)%dE*slices(i)%deb
      end do
      end function getSumDE
      !----------------------------------------------------------------


      end module NLSolver
