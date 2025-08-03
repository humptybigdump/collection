 PROGRAM elasticbeam  ! freeform version 2017
   !    USE LA\_PRECISION, ONLY: WP => DP                          ! LAPACK choose version double precision
   !    USE F95\_LAPACK , ONLY:  LA\_SYGV                           ! From  LAPACK  pick the routine LA\_SYGV
 USE MKL95_PRECISION, ONLY: WP => DP                              ! MKL-LAPACK choose version double precision
 USE MKL95_LAPACK, ONLY:   SYGV                                   ! From  MKL-LAPACK pick the routine LA\_SYGV
 USE basicTools
 IMPLICIT NONE
 INTEGER :: nel,nload,ielem,status,nk,ik,jk,iload,isup,nsup,IDOF, i,j,ia,ib,INFO,nspring,iterm
 REAL(8) :: Ltot,EA,EI,rhoA,k_value,prescValue,Gmod,nu,Lele,C1,C2,  Kwinkler,uy,x
 REAL(8), ALLOCATABLE, DIMENSION(:) :: load,disp,W2spectrum       ! Dynamic-size vectors
 REAL(8), ALLOCATABLE,DIMENSION(:,:) :: Gstiff,Gmass              ! Dynamic-size matrix
 REAL(8), DIMENSION(3,3) :: K11,K12,K21,K22,M11,M12,M21,M22       ! Matrix(3,3) variables
 REAL(8), DIMENSION(6,6) :: Kelem                                 ! Matrix(6,6) variables
 CHARACTER(1) :: bedding                                          ! indicates the kind of bedding
 integer, parameter :: mterms=10 ;   real(8) :: L(mterms), L0     ! max number of summand in any MPC  and coefficients the current  MPC equation
 integer :: nMPC,  nterms,  MPCnode(mterms) , iDOFs(mterms), g(mterms)    ! number of MPCs ,  number of summands  in the current MPC, nodes of summands in  MPC,  local and global DOFs of summands in the current MPC

 OPEN(99, ERR=999, FILE = 'beam.inp', STATUS='OLD')    ! Open the input file. If there is an error, go to line 999. Status='old' The file must exist before attempting to open it
 READ(99, *) nel                  ! Starts reading the input file. The first line contains the number of elements nel
 nk = nel + 1                      ! nk: number of nodes = number of elements + 1
 ALLOCATE(Gstiff(nk*3,nk*3),Gmass(nk*3,nk*3), load(nk*3),disp(nk*3),W2spectrum(nk*3) )
 Gstiff(:,:) = 0 ;    Gmass(:,:) = 0; load(:) = 0;    disp(:) = 0;    W2spectrum(:) = 0    ! allocate and initialize global matrices

 READ(99, *) Ltot; READ(99, *) EA; READ(99, *) EI; READ(99, *) rhoA     ! Ltot: total length of the beam ,   EA EI = Stiffness, Inertia
 Lele = Ltot/nel                                                          ! Lele: length of each element
 READ(99, *) nload                                                          ! nload: number of loads applied on the nodes
 do iload = 1, nload ;   READ(99, *) ik, load((ik-1)*3+1:(ik-1)*3+3) ; enddo  ! ik=loaded node; Force in x, Force in y, Momentum

 CALL elemStiff(K11,K12,K21,K22,EI,EA,Lele)        ! Calculates the stiffness matrix for each element. Output: K11,K12,K21,K22
 CALL elemMass(M11,M12,M21,M22,rhoA,Lele)          ! Calculates the mass matrix for each element. Output: M11,M12,M21,M22
 CALL assembleBlocks(Gstiff,K11,K12,K21,K22,nk)    ! Assembles the global stiffness. Output: Gstiff
 CALL assembleBlocks(Gmass,M11,M12,M21,M22,nk)     ! Assembles the global mass matrix. Output: Mstiff

 READ(99, *) bedding, prescValue                            ! Type of subgrade model (Winkler or Green) and its stiffness value
 SELECT CASE (bedding)
   CASE('G') ;   CALL addGreenStiffness(Gstiff,prescValue,nk,Lele)        ! add Green stiffness  to the global stiffness
   CASE('W') ;  CALL addWinklerStiffness(Gstiff,prescValue,nk,Lele)     ! add Winkler stiffness  to the global stiffness
 END SELECT

 READ(99, *) nsup                                 ! nsup: number of supports
 DO isup = 1, nsup
  READ(99, *) ik, iDOF, prescValue                ! ik:  restricted node; iDOF: Degree of freedom;  prescValue = prescribed movement
   ia = (ik - 1)*3 + iDOF
   Gstiff(ia,ia) = Gstiff(ia,ia) + largeNumber    ! Modifies the corresponding diagonal term of the global stiffness matrix
   load(ia) = load(ia) +largeNumber*prescValue
 END DO

 READ(99, *) nspring                              !  nspring: number of springs acting as restrictions
 DO isup = 1, nspring
   READ(99, *) ik, iDOF, k_value                 ! ik=  restricted node number; iDOF=Degree of freedom ; k\_value= spring stiffness
   ia = (ik - 1)*3 + iDOF
   Gstiff(ia,ia) = Gstiff(ia,ia) + k_value
 END DO


READ(99, *) nMPC   !-----------------------------------------------MPC-via-equation-----
 write(77,'(i2,a,a)') nMPC, '  MPCs  in form: nnodes, inode,','idof, Lambda, inode, idof, Lambda, ... Lambda 0 '
 do iterm=1,nMPC
   READ(99, *) nterms
   L(:)= 0;  g(:) = 0;   MPCnode = 0;  iDOF = 0
   READ(99, *) (MPCnode(i), iDOFs(i), L(i), i=1,nterms),L0;  write(77,*)(MPCnode(i),iDOFs(i), L(i), i=1,nterms),L0
   forall(i=1:nterms) g(i) = (MPCnode(i)-1)*3 + iDOFs(i)
   forall(i=1:nterms) load( g(i) ) =  load(g(i)) + L(i)*L0*largeNumber
   forall(i=1:nterms,j=1:nterms) gstiff( g(i),g(j) ) = gstiff( g(i),g(j) ) +  L(i)*L(j)*largeNumber
 enddo  ! next MPC------------------------------end-of--MPC-via-equation-----

 OPEN(77, file = 'output.txt')
 ok=write_Mma('Gstiff', Gstiff,77) ;  ok=write_Mma('GMass',GMass,77)   ! Write the matrices  for check in  Mma

 CALL symSolve(Gstiff,load,disp,nk*3)                             ! Finds the nodal displacements and rotations

  WRITE(77,*) 'Nodal displacementes and rotations'
  WRITE(77,*) ' ik      Ux                 Uy            theta'
  DO ik=1, nk;        WRITE(77, '(I3,2H  ,3(g15.8,2H  ))') ik, disp(3*ik-2:3*ik);    END DO

 ! call the Lapack routine for symmetric generalised eigenvalue problems.
 ! CALL LA\_SYGV(Gstiff, Gmass, W2spectrum, 1, 'V')  ! LA\_SYGV: 'V' storesthe eigenvectors  in columns of Gstiff.
  call SYGV(Gstiff, Gmass, W2spectrum, 1, 'V')
  WRITE(77,*) 'W2spectrum - EigenValues'  ;   WRITE(77,'(15(g15.8,2H  ))') W2spectrum
  WRITE(77,*) 'EigenVectors' ;               WRITE(77,'(15(g15.8,3H   ))') transpose(Gstiff)

  DO ik=1, 3*nk
    WRITE(77,'(/,12H Eigenvalue: ,g15.8)') , W2spectrum(ik)        ! W2spectrum: eigenvalues of the solution
    WRITE(77,*) ' ik       Ux                Uy              theta'
    DO i=1, nk;  WRITE(77,'(I3,2H  ,3(g15.8,2H ))') i, Gstiff(3*i-2:3*i,ik);  END DO  ! Gstiff = eigenvectors of the solution (in columns)
  END DO

   DO ik=1, 3*nk                                                   ! Procedure to interpolate the displacements along the beam
     WRITE(77,'(/,12H Eigenvalue: ,g15.8)') , W2spectrum(ik)
     WRITE(77,*) '//nc'
     DO i=0, 100
 	  x = i*Ltot/100
       CALL verticaldispl(uy,nk,Lele,x,Gstiff(:,ik))               ! output: uy. A plot can be produced with x,uy
       WRITE(77,'(f10.5,2H   ,f15.8,2H  )') x, uy
     END DO
   END DO

  CLOSE(77)
  CLOSE(99)
  STOP
999    STOP 'I cannot open file beam.inp'
      END PROGRAM elasticbeam
 !===============================================================
  SUBROUTINE elemStiff(K11,K12,K21,K22,EI,EA,L)         ! calculates the element stiffness matrix. Output:K11,K12,K21,K22
    IMPLICIT NONE
    REAL(8), DIMENSION(3,3) :: K11,K12,K21,K22
    REAL(8) :: EA,EI,L,A,B,C,D
    A= EA/L;   B = 12.0d0*EI/L**3;  C = 6*EI/L**2;  D = 2*EI/L
    K11 = RESHAPE([A,0.0d0,0.0d0,0.0d0,B,C,0.0d0,C,2*D],[3,3])
    K12 = RESHAPE([-A,0.0d0,0.0d0,0.0d0,-B,-C,0.0d0,C,D],[3,3])
    K21 = TRANSPOSE(K12)
    K22 = RESHAPE([A,0.0d0,0.0d0,0.0d0,B,-C,0.0d0,-C,2*D],[3,3])
  END SUBROUTINE elemStiff
 !===============================================================
  SUBROUTINE elemMass(M11,M12,M21,M22,rhoA,L)         ! calculates the element mass matrix. Output:M11,M12,M21,M22
    IMPLICIT NONE
    REAL(8), DIMENSION(3,3) :: M11,M12,M21,M22
    REAL(8) :: rhoA,L,A,B,C,D,aux
    aux =  rhoA*L/420
    M11 = aux*RESHAPE([140.0d0,0.0d0,0.0d0,0.0d0,156.0d0,22*L,0.0d0,22*L,4*L*L],[3,3])
    M12 = aux*RESHAPE([70.0d0,0.0d0,0.0d0,0.0d0,54.0d0, 13*L,0.0d0,-13*L,-3*L*L],[3,3])
    M21 = TRANSPOSE(M12)
    M22 = aux*RESHAPE([140.0d0,0.0d0,0.0d0,0.0d0,156.0d0,-22*L,0.0d0,-22*L,4*L*L],[3,3])
  END SUBROUTINE elemMass
 !===============================================================
 SUBROUTINE assembleBlocks(Gstiff,K11,K12,K21,K22,nk)
   IMPLICIT NONE
   REAL(8), DIMENSION(3,3) :: K11,K12,K21,K22
   REAL(8), DIMENSION(nk*3,nk*3) :: Gstiff
   INTEGER :: nk,nel,iel,ia,ib,ic,id
   nel = nk - 1
   DO iel=1,nel
     ia = (iel-1)*3 + 1
         ib = ia + 3
         Gstiff(ia:ia+2,ia:ia+2) = Gstiff(ia:ia+2,ia:ia+2) + K11
         Gstiff(ia:ia+2,ib:ib+2) = Gstiff(ia:ia+2,ib:ib+2) + K12
         Gstiff(ib:ib+2,ia:ia+2) = Gstiff(ib:ib+2,ia:ia+2) + K21
         Gstiff(ib:ib+2,ib:ib+2) = Gstiff(ib:ib+2,ib:ib+2) + K22
       END DO
  END SUBROUTINE assembleBlocks
 !===============================================================
  SUBROUTINE symSolve(gstiff,load,disp,n)
    IMPLICIT NONE
    REAL(8) :: a(n,n+1),gstiff(n,n),load(n),disp(n)
    REAL(8) :: c, d
    INTEGER :: i,j,k,m,n
    m = n+1
    a(1:n,1:n)=gstiff
    a(1:n,m) = load
    DO 1 i = 1, n
    c = a(i,i)
    a(i,i) = c - 1.0d0
    DO 1 k = i+1, m
    d = a(i,k) / c
    DO 1 j = 1, n
    a(j,k) = a(j,k) - d*a(j,i)
 1   continue
    disp = a(1:n,m)
    RETURN
  END SUBROUTINE symSolve
 !===============================================================
  SUBROUTINE inverseSym(aa,n)
    IMPLICIT NONE
    REAL(8) :: a(n,2*n),aa(n,n)
    REAL(8) :: c, d
    INTEGER :: i,j,k,m,n
    m = 2*n ;  a = 0 ;   a(1:n,1:n)=aa
    do i=1,n
    a(i,n+i) = 1
    enddo
    DO 1 i = 1, n
    c = a(i,i)
    a(i,i) = c - 1.0d0
    DO 1 k = i+1, m
    d = a(i,k) / c
    DO 1 j = 1, n
    a(j,k) = a(j,k) - d*a(j,i)
 1   continue
    aa = a(1:n,n+1:2*n)
    RETURN
   END SUBROUTINE inverseSym
 !===============================================================
 SUBROUTINE addGreenStiffness(Gstiff,value,nk,Lele)
 IMPLICIT NONE
 REAL(8) :: Gstiff(3*nk,3*nk), value,Compliance(nk,nk),C1,C2,Lele
 REAL(8), PARAMETER :: pi  = 3.141592653589793238462643d0
 INTEGER :: i,j,nk,ik,jk,ia,ib
  C1 = 2/(4*pi*Lele*value) ;   C2 = 4*C1
  forall(i=1:nk, j=1:nk, i /= j)  Compliance(i,j) = C1/ABS(i-j)
  forall(i=1:nk ) Compliance(i,i) = C2
  CALL inverseSym(Compliance,nk)
  forall(ik=1:nk,  jk=1:nk) Gstiff((ik-1)*3+2,(jk-1)*3+2)= Gstiff((ik-1)*3+2,(jk-1)*3+2) + Compliance(ik,jk)
 END SUBROUTINE addGreenStiffness
 !===============================================================
 SUBROUTINE addWinklerStiffness(Gstiff,Kwinkler,nk,Lele)
 IMPLICIT NONE
 REAL(8) :: Gstiff(3*nk,3*nk), Kwinkler, C2,Lele
 INTEGER :: i,j,nk,ik,ia
  C2 = Lele**2*Kwinkler
  forall( ik=1:nk)  Gstiff((ik-1)*3 + 2,(ik-1)*3 + 2) = Gstiff((ik-1)*3 + 2,(ik-1)*3 + 2) + C2
  END SUBROUTINE addWinklerStiffness
 !===============================================================
 SUBROUTINE verticaldispl(uy,nk,Lele,x,eigenvector)    ! Output: uy vector with the vertical displ. interpolated  between nodes
 IMPLICIT NONE
 REAL(8) ::uy,Lele,x,xi,dxdxi,x1,x2,N1,N2,N3,N4,u1,u2,theta1,theta2
 REAL(8) :: eigenvector(3*nk)                                      ! Input: Eigenvector for the ith eigenvalue
 INTEGER :: nk,elem,kA,kE
   uy = 0.0d0
   IF (MOD(x,Lele) == 0) elem = INT(x/Lele)
   IF (MOD(x,Lele) /= 0) elem = INT(x/Lele)+1
   IF (x == 0) elem = INT(x/Lele)+1

   x1 = Lele*(elem-1)
   x2 = Lele*elem
   xi = (x - (x2+x1)/2)/((x2-x1)/2)
   dxdxi = (x2-x1)/2
   N1 = (2.0d0 - 3*xi + xi*xi*xi)/4.0d0
   N2 = (1.0d0 - xi - xi*xi + xi*xi*xi)/4.0d0
   N3 = (2.0d0 + 3*xi - xi*xi*xi)/4.0d0
   N4 = (-1.0d0 - xi + xi*xi + xi*xi*xi)/4.0d0
   kA = elem
   kE = elem + 1
   u1 = eigenvector(3*kA - 1)
   u2 = eigenvector(3*kE - 1)
   theta1 = eigenvector(3*kA)
   theta2 = eigenvector(3*kE)
   uy = N1*u1 + N2*theta1*dxdxi + N3*u2 + N4*theta2*dxdxi
  END SUBROUTINE verticaldispl
! =====================================================================================
