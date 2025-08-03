!>   general purpose routines: outerprod swap approx and linear solver
    module generalTools
      implicit none
      real(8),parameter :: pi=3.141592653589793238d0, degree= 3.141592653589793238d0/180.0d0

!> logical function eg. if(a .approx. b)  a and b can be real or integer
      interface operator(.approx.)
         module procedure approxRI,approxIR,approxRR
    end interface

 !> logical functions between
      interface operator(.between.)
         module procedure betweenII, betweenRR, betweenRI
    end interface

 !> outer product of vectors, e.g. $C_{ij}= a_i b_j$  components of a and b can be real or integer
      INTERFACE outerprod
         MODULE PROCEDURE outerprod_ii ,outerprod_dd
      END INTERFACE

!> swap scalars or vectors swap(a,b) -> a takes the value of  b and b takes the value of a
      INTERFACE swap
         MODULE PROCEDURE swapii, swapdd, swapd, swapi
      END INTERFACE

!> solve a linear system A . x = b where the b can be a vector or a matrix containing many RHS vectors
      INTERFACE solveLinSys
         MODULE PROCEDURE gaussjMM , gaussjMV
      END INTERFACE

     contains !********************************************************************************************


       logical function betweenII(x, r)
        integer,intent(in) :: x; integer,intent(in):: r(2)
        betweenII = ( r(1) <= x .and. x <  r(2) )
       end function betweenII

       logical function betweenRR(x, r)
        real(8),intent(in) :: x; real(8),intent(in) :: r(2)
        betweenRR = (r(1) <= x .and. x <  r(2))
       end  function betweenRR

       logical function betweenRI(x, r)
       real(8),intent(in) :: x; integer,intent(in):: r(2)
        betweenRI = ( r(1) <= x .and. x <  r(2))
       end  function betweenRI


!> compares real a with real b
     function approxRR(a,b)   !compares real a with real b
      real(8), intent(in) :: a,b
      logical :: approxRR
      approxRR = ( abs( a - b ) <= tiny(a) )
      end function approxRR
      !----------------------------------------------------------------

 !> compares real a with integer b
      function approxRI(a,b)
      real(8), intent(in) :: a
      integer, intent(in) :: b
      logical :: approxRI
      approxRI = abs( a - b )   <= tiny(a)
      end function approxRI
      !----------------------------------------------------------------

!> compares integer a with real b
      function approxIR(a,b)
      integer, intent(in) :: a
      real(8), intent(in) :: b
      logical :: approxIR
      approxIR = abs( a - b ) <= tiny(b)
      end function approxIR
      !----------------------------------------------------------------

 !>  swaps real vectors a and b
      SUBROUTINE swapdd(a,b)  ! swaps real vectors a and b
      REAL(8), DIMENSION(:), INTENT(INOUT) :: a,b
      REAL(8), DIMENSION(SIZE(a)) :: dum
      dum=a;  a=b;  b=dum
      END SUBROUTINE swapdd
      !----------------------------------------------------------------

 !> swaps integer vectors a and b
      SUBROUTINE swapii(a,b) ! swaps integer vectors a and b
      integer, DIMENSION(:), INTENT(INOUT) :: a,b
      integer, DIMENSION(SIZE(a)) :: dum
      dum=a;  a=b;  b=dum
      END SUBROUTINE swapii
      !----------------------------------------------------------------

 !> swaps real scalars a and b
      SUBROUTINE swapd(a,b)
      REAL(8) , INTENT(INOUT) :: a,b
      REAL(8) :: dum
     dum=a;  a=b;  b=dum
      END SUBROUTINE swapd
      !----------------------------------------------------------------

!>  swaps integer scalars a and b
      SUBROUTINE swapi(a,b) ! swaps integer scalars a and b
      integer,  INTENT(INOUT) :: a,b
      integer :: dum
      dum=a;  a=b;  b=dum
      END SUBROUTINE swapi
      !----------------------------------------------------------------

!> $C_{ij} = a_i b_j $ where $a$ and $b$  are reals
      FUNCTION outerprod_dd (a,b)   ! $C_{ij} = a_i b_j $ where $a$ and $b$  are reals
      REAL(8), DIMENSION(:), INTENT(IN)   :: a,b
      REAL(8), DIMENSION(size(a),size(b)) :: outerprod_dd
      outerprod_dd  = spread(a,dim=2,ncopies=size(b)) * spread(b,dim=1,ncopies=size(a))
      END FUNCTION outerprod_dd
      !----------------------------------------------------------------

 !>  $C_{ij} = a_i b_j $  where $a$ and $b$  are integers
      FUNCTION outerprod_ii(a,b)   !  $C_{ij} = a_i b_j $  where $a$ and $b$  are integers
      integer, DIMENSION(:), INTENT(IN)   :: a,b
      integer, DIMENSION(size(a),size(b)) :: outerprod_ii
      outerprod_ii  = spread(a,dim=2,ncopies=size(b)) * spread(b,dim=1,ncopies=size(a))
      END FUNCTION outerprod_ii
      !----------------------------------------------------------------


 !>  $C_{ij} = a_i .and. b_j $  where $a$ and $b$  are logical and multiplication is logical
      FUNCTION outerand(a,b)
      LOGICAL, DIMENSION(:), INTENT(IN) :: a,b
      LOGICAL, DIMENSION(size(a),size(b)) :: outerand
      outerand = spread(a,dim=2,ncopies=size(b)) .and. spread(b,dim=1,ncopies=size(a))
      END FUNCTION outerand
      !----------------------------------------------------------------

!> norm of a vector  v of dimension
      function norm(v)
      real(8),intent(in) :: v(2)
      real(8)  :: norm
      norm = sqrt( dot_product(v,v) )
      end function norm
      !----------------------------------------------------------------

!> normalization of a vector   v  of dimension 2
      function normalized(v)
      real(8),intent(in) :: v(2)
      real(8) :: normalized(2), aux
      normalized(:) = 0
      aux = norm(v)
      if(aux .approx. 0) return
      normalized = v/aux
      end function normalized
      !----------------------------------------------------------------

!> returns the area of a polygon described by the n points. The points must be ordered CCW.
      function getArea(points,n)
      integer,intent(in) :: n
      real(8), intent(in) :: points(n,2)
      real(8) :: getArea, extra(n+1,2), sum
      integer :: i
      extra(1:n,:) = points(1:n,:);  extra(n+1,:) = points(1,:)
      sum=0
      do i=1,n;       sum= sum +extra(i,1)*extra(i+1,2) - extra(i+1,1)*extra(i,2)
      end do
      getArea= sum/2.0d0
      end function getArea
      !----------------------------------------------------------------

!> rotates v  by $90°$  CCW arround z axis
      function rotate90CCW(v)
      real(8),intent(in) :: v(2)
      real(8) :: rotate90CCW(2), Rb(2,2)
      Rb(:,:) = reshape([0.0d0,1.0d0,-1.0d0,0.0d0],[2,2])
      rotate90CCW = matmul(Rb , v)
      end function rotate90CCW
      !----------------------------------------------------------------

 !> rotates v  by  alpha Degrees CCW arround z axis
      function rotateAlphaCCW(v,alpha)
      real(8),intent(in) :: v(2),alpha
      real(8) :: rotateAlphaCCW(2)
      real(8) :: Rb(2,2),s,c,angle
      angle = alpha*degree
      s = sin(angle);  c = cos(angle);  Rb(:,:) = reshape([c,s,-s,c],[2,2])
      rotateAlphaCCW = matmul(Rb , v)
      end function rotateAlphaCCW
      !----------------------------------------------------------------

!> returns the position of the largest i such that    list(i) < x
      function getPositionInList(list,n,x)
      integer,intent(in) :: n
      real(8),intent(in) :: list(n),x
      integer :: getPositionInList , i
      getPositionInList=0
      do i=1,n
         if(  x<list(i)  )  return
         getPositionInList = i
      end do
      end function getPositionInList
      !----------------------------------------------------------------

!> returns the inverse of a 2x2 matrix
      subroutine inv22(a)
      real(8), intent(inout) :: a(2,2)
      real(8) :: det
      det = -a(1,2)*a(2,1)  + a(1,1)*a(2,2)
      if(det  .approx. 0) stop 'Error in inv22: det(a)=0'
      a(:,:) = reshape([ a(2,2), -a(2,1), -a(1,2), a(1,1) ],[2,2] ) /det
      end subroutine inv22
      !----------------------------------------------------------------

!> solves the linear system   A.X=M    where  M  is a matrix contains many RHS vectors   the returned solutions X overrides M
      SUBROUTINE gaussjMM(a,b)
      REAL(8), DIMENSION(:,:), INTENT(INOUT) :: a,b
      INTEGER, DIMENSION(size(a,1)) :: ipiv,indxr,indxc
      LOGICAL, DIMENSION(size(a,1)) :: lpiv
      REAL(8) :: pivinv
      REAL(8), DIMENSION(size(a,1)) :: dumc
      INTEGER , TARGET :: irc(2)
      INTEGER  :: i,l,n
      INTEGER, POINTER :: irow,icol
      n = size(a,1)
      if(n /= size(a,2) .or. n /=size(b,1)  ) stop'gaussj: incompatible dimensions'
      irow => irc(1)
      icol => irc(2)
      ipiv=0
      do i=1,n
            lpiv = (ipiv == 0)
            irc=maxloc(abs(a),outerand(lpiv,lpiv))
            ipiv(icol)=ipiv(icol)+1
            if (ipiv(icol) > 1) stop'gaussj: singular matrix (1)'
            if (irow /= icol) then
                  call swap(a(irow,:),a(icol,:))
                  call swap(b(irow,:),b(icol,:))
            end if
            indxr(i)=irow
            indxc(i)=icol
            if (a(icol,icol) == 0.0)       stop'gaussj: singular matrix (2)'
            pivinv=1.0d0/a(icol,icol)
            a(icol,icol)=1.0
            a(icol,:)=a(icol,:)*pivinv
            b(icol,:)=b(icol,:)*pivinv
            dumc=a(:,icol)
            a(:,icol)=0.0
            a(icol,icol)=pivinv
            a(1:icol-1,:)=a(1:icol-1,:)-outerprod(dumc(1:icol-1),a(icol,:))
            b(1:icol-1,:)=b(1:icol-1,:)-outerprod(dumc(1:icol-1),b(icol,:))
            a(icol+1:,:)=a(icol+1:,:)-outerprod(dumc(icol+1:),a(icol,:))
            b(icol+1:,:)=b(icol+1:,:)-outerprod(dumc(icol+1:),b(icol,:))
      end do
      do l=n,1,-1
            call swap(a(:,indxr(l)),a(:,indxc(l)))
      end do
      END SUBROUTINE gaussjMM
      !----------------------------------------------------------------

!> solves the linear system M . x = \b   (b is a vector). The returned solution x overrides b
      SUBROUTINE gaussjMV(a,b)
      REAL(8), DIMENSION(:,:), INTENT(INOUT) :: a
      REAL(8), DIMENSION(:), INTENT(INOUT) ::  b
      INTEGER, DIMENSION(size(a,1)) :: ipiv,indxr,indxc
      LOGICAL, DIMENSION(size(a,1)) :: lpiv
      REAL(8) :: pivinv
      REAL(8), DIMENSION(size(a,1)) :: dumc
      INTEGER , TARGET :: irc(2)
      INTEGER  :: i,l,n
      INTEGER, POINTER :: irow,icol
      n = size(a,1)
      if(n /= size(a,2) .or. n /=size(b)  ) stop'gaussj: incompatible dimensions'
      irow => irc(1)
      icol => irc(2)
      ipiv=0
      do i=1,n
            lpiv = (ipiv == 0)
            irc=maxloc(abs(a),outerand(lpiv,lpiv))
            ipiv(icol)=ipiv(icol)+1
            if (ipiv(icol) > 1) stop'gaussj: singular matrix (1)'
            if (irow /= icol) then
                  call swap(a(irow,:),a(icol,:))
                  call swap(b(irow),b(icol))
            end if
            indxr(i)=irow
            indxc(i)=icol
            if (a(icol,icol) == 0.0)       stop'gaussj: singular matrix (2)'
            pivinv=1.0d0/a(icol,icol)
            a(icol,icol)=1.0
            a(icol,:)=a(icol,:)*pivinv
            b(icol)=b(icol)*pivinv
            dumc=a(:,icol)
            a(:,icol)=0.0
            a(icol,icol)=pivinv
            a(1:icol-1,:)=a(1:icol-1,:)-outerprod(dumc(1:icol-1),a(icol,:))
            b(1:icol-1)=b(1:icol-1)- dumc(1:icol-1)* b(icol)
            a(icol+1:,:)=a(icol+1:,:)-outerprod(dumc(icol+1:),a(icol,:))
            b(icol+1:)=b(icol+1:) - dumc(icol+1:)*b(icol )
      end do
      do l=n,1,-1
            call swap(a(:,indxr(l)),a(:,indxc(l)))
      end do
      END SUBROUTINE gaussjMV
      !----------------------------------------------------------------

      end module generalTools
