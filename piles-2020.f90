
    ! The Elements of Programming Style, by Brian W. Kernighan and P. J. Plauger:  1974

    ! Write clearly — don't be too clever.     KISS  
    ! Say what you mean, simply and directly.
    ! Use library functions whenever feasible.
    ! Avoid too many temporary variables.
    ! Write clearly — don't sacrifice clarity for efficiency.
    ! Let the machine do the dirty work.
    ! Replace repetitive expressions by calls to common functions.
    ! Parenthesize to avoid ambiguity.
    ! Choose variable names that won't be confused.
    ! Avoid unnecessary branches.
    ! If a logical expression is hard to understand, try transforming it.
    ! Choose a data representation that makes the program simple.
    ! Write first in easy-to-understand pseudo language; then translate into whatever language you have to use.
    ! Modularize. Use procedures and functions.
    ! Avoid gotos completely if you can keep the program readable.
    ! Don't patch bad code — rewrite it.
    ! Write and test a big program in small pieces.
    ! Use recursive procedures for recursively-defined data structures.
    ! Test input for plausibility and validity.
    ! Make sure input doesn't violate the limits of the program.
    ! Terminate input by end-of-file marker, not by count.
    ! Identify bad input; recover if possible.
    ! Make input easy to prepare and output self-explanatory.
    ! Use uniform input formats.
    ! Make input easy to proofread.
    ! Use self-identifying input. Allow defaults. Echo both on output.
    ! Make sure all variables are initialized before use.
    ! Don't stop at one bug.
    ! Use debugging compilers.
    ! Watch out for off-by-one errors.
    ! Take care to branch the right way on equality.
    ! Be careful if a loop exits to the same place from the middle and the bottom.
    ! Make sure your code does "nothing" gracefully.
    ! Test programs at their boundary values.
    ! Check some answers by hand.
    ! 10.0 times 0.1 is hardly ever 1.0.
    ! 7/8 is zero while 7.0/8.0 is not zero.
    ! Don't compare floating point numbers solely for equality.
    ! Make it right before you make it faster.
    ! Make it fail-safe before you make it faster.
    ! Make it clear before you make it faster.
    ! Don't sacrifice clarity for small gains in efficiency.
    ! Let your compiler do the simple optimizations.
    ! Don't strain to re-use code; reorganize instead.
    ! Make sure special cases are truly special.
    ! Keep it simple to make it faster.
    ! Don't diddle code to make it faster — find a better algorithm.
    ! Instrument your programs. Measure before making efficiency changes.
    ! Make sure comments and code agree.
    ! Don't just echo the code with comments — make every comment count.
    ! Don't comment bad code — rewrite it.
    ! Use variable names that mean something.
    ! Use statement labels that mean something.
    ! Format a program to help the reader understand it.
    ! Document your data layouts.
    ! Don't over-comment

!=============================================================================================================




! Copyright (C)  2007-2013  Andrzej Niemunis
!
! piles-2013.f90 is free software; you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation; either version 2 of the License, or
! (at your option) any later version.
!
! piles-2013.f90 is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

!
! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,USA.




       PROGRAM elasticPiles
       IMPLICIT NONE
       integer, parameter :: npele=80;
       integer, parameter :: npnode = npele+1, npdof = npnode*3
       INTEGER :: nelcap,   &                                            ! Number of elements of cap beam
     &            nload,    &                                            ! Number of concentrated loads on cap nodes
     &            nk,        &                                            ! Number of nodes of cab beam nk = nelcap +1
     &            ik,        &
     &            iload,     &
     &            isup,      &
     &            nsup,      &                                            ! Number of restrictions or supports in the cap beam
     &            iDOF,      &
     &            i,         &
     &            ia,         &
     &            nspring,    &                                           ! Number of springs acting as restrictions on cap beam
     &            npiles                                                  ! Number of piles
       REAL(8) :: EA,EI,      &                                           ! Stiffness * Area, Stiffness * Inertia (for reading)
     &            k_value,     &
     &            avalue,       &
     &            Lele                                                    ! Length of element (for reading)

      real(8), dimension(3):: globalForces, localForces                  ! Vector of displacements in local coord. (at piles head)
                                                                          ! forces on pile in global / local coord. (at piles head)


      REAL(8), ALLOCATABLE,DIMENSION(:) ::      load, disp                ! global Loads  and displacements:(3*nk)
      REAL(8), ALLOCATABLE,DIMENSION(:,:) ::    Gstiff                    ! Global stiffness matrix of cap beam:(3*nk,3*nk)
      REAL(8), ALLOCATABLE,DIMENSION(:,:,:) ::  fPStiff                   ! full stiffnesses of individual piles
      REAL(8), ALLOCATABLE,DIMENSION(:,:) ::    PileProps
      REAL(8), ALLOCATABLE,DIMENSION(:,: ,:) :: headStiffList, RotList   !  head stiffness of piles in local coord: (npiles,3,3)
                                                                         !  and rotations (local -> global)
       REAL(8), DIMENSION(3,3) :: K11,K12,K21,K22, Headstiff ,Rot
       REAL(8), DIMENSION(npele,3) :: Mom
       REAL(8), PARAMETER :: great=10.0d10                              ! Parameters = fixed values
       INTEGER :: iel,ipile,inode                                       ! counters
       INTEGER ,ALLOCATABLE, DIMENSION(:) :: inodeList                  ! List of nodes which conect piles and cap beam
       REAL(8) ::  Dpile, tilt_cw , h11,  h_Wink, a_WinkT, b_WinkT, a_WinkA, b_WinkA, Apoint
       CHARACTER(1) :: bedding , connection                             ! Identifier 'W': Winkler or  'G': Green
       character(80) ::  jobTitle

       OPEN(99, ERR=999, FILE = 'piles.inp', STATUS='OLD')              ! The file must exist
       OPEN(77, file = 'output.txt')                                    ! Writing of the output file with the results

       READ(99 ,'(a80)')  jobTitle
       write(* ,'(a80)')  jobTitle
       write(77,'(a80)')  jobTitle
       READ(99, *) nelcap                                               ! nelcap: number of elements of cap beam
       nk = nelcap + 1                                                  ! nk: number of cap nodes = number of cap elements + 1
       write(77,'(a,i3,a)')  'Horiz. cap Beam with ' ,nelcap, ' elements: L,EA, EI (from the left, nodes generated automat.)'
       ALLOCATE(Gstiff(nk*3,nk*3),load(nk*3),disp(nk*3))                ! size of Gstiff, load, disp, 3 dofs (ux,uy,rot)per node
       Gstiff(:,:) = 0; load(:) = 0;  disp(:) = 0                                  ! Initialize the above defined vectors and matrix
       DO iel=1,nelcap                                                  ! Loop to read information of cap beam elements
        READ(99, *) Lele, EA, EI                                        ! Length, Stiffness * Area, Stiffness * Inertia
        write(77,'(i3,a,3g12.4)')  iel,': ',  Lele,EA, EI
        CALL elemStiff(K11,K12,K21,K22,   EI,EA,Lele)                      ! Calculates the stiffness matrix for each element.                                                                !    Output: K11,K12,K21,K22
        CALL assembleStiff(Gstiff,K11,K12,K21,K22,iel,nk)               ! Assamble the stiffness K11,K12,K21,K22 of element iel
       ENDDO                                                            !    on global stiffnes Gstiff

      READ(99, *) npiles                                                ! Read the number of piles
      write(77,'(a,i3,a)')  'Properties of ' ,npiles, ' piles: (A=axial P=pin C=clamp) '
      write(77,'(a,i3,a)')  '    inode, APC,   Lele,    Dpile,    EA,           EI,      tilt_cw      h_Wink,  a_WinkT,  b_WinkT,  a_WinkA,  b_WinkA,  Apoint'
      ALLOCATE(inodeList(npiles))
      Allocate(headStiffList(npiles,3,3), RotList(npiles,3,3),fPStiff(npiles,npdof,npdof),PileProps(npiles,11) )
      headStiffList(:,:,:) = 0; RotList(:,:,:)=0; fPStiff(:,:,:) = 0; PileProps(:,:) = 0

      DO ipile=1,npiles                                                 ! nel = number of piles (with npele elements each)
        READ(99,*) inode, connection, Lele, Dpile, EA,  EI, tilt_cw     ! Initial node (x=0), connection( P= pinned, C=clamped),  Length, Diameter, Stiffness * Area,
                                                                        !  Stiffness * Inertia, tilt(clockwise) in radians
        READ(99,*)  h_Wink, a_WinkT, b_WinkT, a_WinkA, b_WinkA, Apoint  ! $h_B$, $a_T$, $b_T$. Trans. Winkler coef. $k_T=a_T + b_T(z-h_B)$
                                                                        ! $a_A$, $b_A$. Axial Winkler coef. $k_A= a_A + b_A(z-h_B)$
                                                                        ! $A_p$  Winkler coef. at foot of pile $k_P=\frac14 \phi D^2 A_p$
       PileProps(ipile,:) = [Lele, Dpile,EA,EI,tilt_cw,h_Wink,a_WinkT, b_WinkT, a_WinkA, b_WinkA, Apoint]
        write(77,'(i2,a2,i5,a5,11g11.4)') ipile, ': ',   inode, connection, PileProps(ipile,:)
       CALL getHeadStiffness(ipile, PileProps(ipile,:) ,Headstiff, fPStiff(ipile,:,:) )
       select case( connection )
         case('A');   h11 = Headstiff(1,1);  Headstiff=0;  Headstiff(1,1) = h11  !  Only axial force from  pile
         case('P');  call staticCondensation( Headstiff, 3,3 ) !  Pin, no bending moments from pile
         case('C');  continue !  Clamped connection nothing to do
         case  default ;  stop 'error, unknown connection case '
       end select
       CALL rotateHeadStiffness(Headstiff,tilt_cw,Rot)
       CALL addHeadStiffness(Gstiff,Headstiff,inode,nk)
        inodeList(ipile)=inode; RotList(ipile,:,:)=Rot; headStiffList(ipile,:,:)=Headstiff  !  remember pile
      ENDDO

       READ(99, *) nload                                                ! nload: number of loads applied on the cap nodes
        write(77,'(i3,a)')  nload, ' concentrated load(s) : inode, Fx, Fny, M '
       DO iload = 1, nload                                              ! Loop over number of loads
       READ(99, *) ik, load((ik-1)*3+1:(ik-1)*3+3)                      ! loaded node ik and load = (F\_x, F\_y, Mom) there
       write(77, '(i4,3g12.4)') ik, load((ik-1)*3+1:(ik-1)*3+3)
       END DO

       READ(99, *) bedding, avalue                                       ! Type  (W=Winkler or G=Green) and stiffness value                                                                        !
        write(77,'(a,a2,a,g12.4)')   'Vertical bedding of the cap beam: type=',bedding, ' stiff=', avalue
       SELECT CASE (bedding)
         CASE('G') ; CALL addGreenStiffness(Gstiff,avalue,nk,Lele)         ! Modifies the global stiffness by adding the                                                                          !    Green stiffness. Output: modified Gstiff
         CASE('W') ; CALL addWinklerStiffness(Gstiff,avalue,nk,Lele)       ! Modifies the global stiffness by adding the                                                                       !    Winkler stiffness. Output: modified Gstiff
       END SELECT

       READ(99, *) nsup                                                  ! nsup: number of restrictions or supports in the beam
       write(77,'(i2,a)')   nsup,' constraint(s) in form: inode, idof, value '
       DO isup = 1, nsup
        READ(99, *) ik, iDOF, avalue                                     ! ik=restricted node, iDOF= resticted dof,  restriction; (0: no movement)
         write(77, '(a,2i4,g12.4)') '      ', ik, iDOF, avalue
         ia = (ik - 1)*3 + iDOF
         Gstiff(ia,ia) = Gstiff(ia,ia) + great ; load(ia) = load(ia) + great*avalue  !  restriction by penalty
       END DO

       READ(99, *) nspring                                              ! nspring: number of springs
       write(77,'(i2,a)')   nspring,' el. support(s) in form: inode, idof, k-value '
       DO isup = 1, nspring
         READ(99, *) ik, iDOF, k_value                                  ! ik: springed node, iDOF: springed dof, stiffness of spring
         write(77, '(a,2i4,g12.4)') '      ', ik, iDOF, k_value
         ia = (ik - 1)*3 + iDOF
         Gstiff(ia,ia) = Gstiff(ia,ia) + k_value                        ! Modifies the corresponding position in the global
       END DO

       CALL symSolve(Gstiff,load,disp,nk*3)    ! Finds the nodal displacements and rotations
       WRITE(77,*) 'Global displacementes  of nodes of the cap beam '
       WRITE(77,*) ' ik      U_X               U_Y            rotat'
       WRITE(77, '(I3,2h   ,3g15.6,2H  )') (ik, disp(3*ik-2:3*ik) , ik=1, nk)
       WRITE(77,*) 'Forces acting on piles (in glob/local coord.)'
       WRITE(77,'(A88)')'ipile  ik     F_X         F_Y            M             F_x             F_y            M'

      DO ipile=1,npiles                      !  displacements of heads from global to local
        ik = inodelist(ipile)
         globalForces =  matmul(headStiffList(ipile,:,:), disp(3*ik-2:3*ik))
         localForces =  matmul(transpose(RotList(ipile,:,:)), globalForces  )
         WRITE(77, '(I3,1H ,I5,1H ,6(g12.4,2H  ))') ipile,ik,globalForces,localForces
      END DO

     DO ipile=1,npiles                      !  displacements of heads from global to local
        ik = inodelist(ipile)
         globalForces =  matmul(headStiffList(ipile,:,:), disp(3*ik-2:3*ik))
         localForces =  matmul(transpose(RotList(ipile,:,:)), globalForces  )
         call PilePostProc(fPStiff(ipile,:,:),localForces, PileProps(ipile,:),Mom)! Bending moments in pile
          write(77,'(A30,i3,A20)') 'Internal forces in pile No', ipile, 'in columns: x,M,Q'
          write(77,'(3g12.3)') (Mom(i,:), i=1,npele)
      END DO
      write(*,*) 'computation finished, see results in the  file output.txt '

       CLOSE(77)
       CLOSE(99)
       STOP  ' finished successfully I hope'
999    STOP 'I cannot open the file piles.inp'
      END PROGRAM elasticPiles
!======================================================================================

      subroutine PilePostProc(Gstiff, HeadLoad, props, Mom )  ! return Bending moments for a given pile
      implicit none
      integer, parameter :: npele=80; integer, parameter :: npnode = npele+1, npdof = npnode*3
      real(8), intent(in) :: Gstiff(npdof,npdof),HeadLoad(3),props(11)   ! props = [Lele, Dpile,EA,EI,tilt\_cw,h\_Wink,a\_WinkT, b\_WinkT,a\_WinkA,b\_WinkA,Apoint]
      real(8), intent(out) :: Mom(npele,3)
      real(8), dimension(npdof) :: u ,RHS
      real(8) ::  Lele, Dpile,EA,EI,tilt_cw,h_Wink, a_WinkT, b_WinkT
      real(8) ::  u1,u2,f1,f2,z1,z2,kT1,kT2,eM,Qu
      integer :: ie
      RHS = 0 ; RHS(1:3) = HeadLoad ;call symSolve(Gstiff,RHS,u,npdof)   ! get nodal displacements in the pile
      Lele = props(1)/npele; Dpile=props(2); EA= props(3); EI = props(4); tilt_cw = props(5); h_Wink = props(6)
      a_WinkT =props(7);   b_WinkT=props(8)
      do ie=1,npele           ! loop over elements of pile
           u1 = u(3*(ie-1)+2)  !  rotations and displacements of element nodes
           f1 = u(3*(ie-1)+3)
           u2 = u(3*ie+2)
           f2 = u(3*ie+3)
           kT1 = 0; kT2 = 0; z1 = (ie-1)*Lele*cos(tilt_cw) ; z2 = ie*Lele*cos(tilt_cw)  ! transveral bedding
           if (z1 > h_Wink)  kT1 =  ( a_WinkT +  b_WinkT  * (z1 - h_Wink) ) * Dpile*Lele
           if (z1 > h_Wink)  kT2 =  ( a_WinkT +  b_WinkT  * (z2 - h_Wink) ) * Dpile*Lele
           eM  = (48* EI *( f1 - f2) - Lele**3 *(kT1*u1 + kT2 *u2)  ) /(48*Lele)
           Qu = (-240*EI *(f1 * Lele + f2*Lele + 2*u1 - 2*u2) + Lele**4*(kT1*u1 - kT2*u2) ) /(40*Lele**3)
           Mom(ie,:) = [ (z1 + z2)/2, eM, Qu ]
      enddo
      end subroutine PilePostProc
!======================================================================================
      subroutine getHeadStiffness(ipile, p, Headstiff, Gstiff )  ! divide pile into npele elements, return head stiffness
      implicit none
      integer, parameter :: npele=80
      integer, parameter :: npnode = npele+1, npdof = npnode*3 ! Gstiff is returned for postprocessing only
      real(8), intent(in):: p(11)
      real(8), dimension(3,3), intent(out) ::   Headstiff
      real(8), dimension(npdof,npdof), intent(out) :: Gstiff
       real(8), dimension(3,3) ::  K11,K12,K21,K22
      real(8), dimension(npdof) :: RHS,  disp
      real(8) ::  Lpile, Dpile, EA, EI, tilt_cw, h_Wink,a_WinkT, b_WinkT, a_WinkA, b_WinkA, Apoint, Lele
      real(8) :: C11,C22,C23,C32,C33,det,kA,kT,x,z
      real(8), parameter:: Pi=3.14159
      integer :: ik,     iel, iA, iT, iP ,ipile
        Lpile = p(1);  Dpile= p(2);     EA= p(3);         EI= p(4);    tilt_cw= p(5);     h_Wink= p(6);
       a_WinkT= p(7);   b_WinkT= p(8);  a_WinkA= p(9);    b_WinkA= p(10);    Apoint= p(11);
      if(Lpile *cos(tilt_cw) <= h_Wink) then
        write(*, '(a,i2,a)') 'warning: pile ', ipile,' will be ignored because bedding starts too deep'
        HeadStiff = 0
        return
      endif
      Lele = Lpile/npele
      CALL elemStiff(K11,K12,K21,K22, EI,EA,Lele)      ! Calculates single el. stiffness K11,K12,K21,K22
      Gstiff = 0.0d0
      do iel=1,npele
        CALL assembleStiff(Gstiff,K11,K12,K21,K22,iel,npnode)
      enddo
      do ik = 1,npnode
          x = (ik-1)*Lele
          z = x*cos(tilt_cw)
          kT = 0
          kA = 0
          if (z > h_Wink) then
            kT =  ( a_WinkT +  b_WinkT  * (z - h_Wink) ) * Dpile*Lele
            kA =  ( a_WinkA +  b_WinkA  * (z - h_Wink) ) * Dpile*Lele
          endif
          iA = (ik- 1)*3 +  1
          iT = (ik- 1)*3 +  2
          Gstiff(iA,iA) =  Gstiff(iA,iA) + kA
          Gstiff(iT,iT) =  Gstiff(iT,iT) + kT
      enddo
        iP = (npnode-1)*3 + 1
        Gstiff(iP,iP) =  Gstiff(iP,iP) + Apoint * Pi*Dpile*Dpile/4.0d0
        RHS = 0.0d0;  RHS(1) = 1                             ! displacements  due to axal force N=1 at head
        call symSolve(Gstiff,RHS,disp,npdof);  C11 = disp(1)
        RHS  = 0; RHS(2) = 1                                ! displacements  due to lateral force H=1 at head
        call symSolve(Gstiff,RHS,disp,npdof);  C22 = disp(2); C32 = disp(3)
        RHS  = 0; RHS(3) = 1                                ! displacements  due to moment M=1 at head
        call symSolve(Gstiff,RHS,disp,npdof);  C23 = disp(2); C33 = disp(3)
        if(abs(C32-C23)/(abs(C32) + abs(C23)) > 1.d-3)  stop'error: unsymmetric Head'
        det = C22*C33 - C23*C32
        HeadStiff = reshape((/ det/C11,0.d0,0.d0, 0.d0,C33,-C23, 0.d0,-C23,C22/),(/3,3/) ) / det

      end subroutine getHeadStiffness
!======================================================================================
       subroutine rotateHeadStiffness(Headstiff,tilt_cw,Rot)
       implicit none
       real(8) :: tilt_cw, Headstiff(3,3), Rot(3,3), RT(3,3), c,s
       real(8), parameter :: pi = 3.1415926535897932385d0
       c = cos(tilt_cw + pi/2)
       s = sin(tilt_cw + pi/2)
       Rot = reshape( (/ c,-s,0.d0,   s,c,0.d0 ,  0.d0,0.0d0,1.d0/),(/3,3/) ) ! v = R.V with v=local vector and V=global vector
       RT = Transpose(Rot)
       Headstiff =  matmul(Rot,matmul(Headstiff, RT))
       end subroutine rotateHeadStiffness
!======================================================================================
      subroutine addHeadStiffness(Gstiff,Headstiff,ik,nk)
        IMPLICIT NONE
       INTEGER :: nk,ik,ia
        REAL(8), DIMENSION(3,3) ::  Headstiff
        REAL(8), DIMENSION(nk*3,nk*3) :: Gstiff
          ia = (ik-1)*3 + 1
          Gstiff(ia:ia+2,ia:ia+2) = Gstiff(ia:ia+2,ia:ia+2) + Headstiff
      end subroutine addHeadStiffness

!======================================================================================
      SUBROUTINE elemStiff(K11,K12,K21,K22,EI,EA,L)      ! calculates the element stiffness matrices K11,K12,K21,K22
        IMPLICIT NONE
        REAL(8), DIMENSION(3,3),intent(out) :: K11,K12,K21,K22
        REAL(8), intent(in) :: EA,EI,L
        REAL(8) :: A,B,C,D
        A= EA/L
        B = 12.0d0*EI/L**3
        C = 6*EI/L**2
        D = 2*EI/L
        K11 = RESHAPE([A,0.0d0,0.0d0,0.0d0,B,C,0.0d0,C,2*D],[3,3])
        K12 = RESHAPE([-A,0.0d0,0.0d0,0.0d0,-B,-C,0.0d0,C,D],[3,3])
        K21 = TRANSPOSE(K12)
        K22 = RESHAPE([A,0.0d0,0.0d0,0.0d0,B,-C,0.0d0,-C,2*D],[3,3])
      END SUBROUTINE elemStiff
!======================================================================================
      SUBROUTINE assembleStiff(Gstiff,K11,K12,K21,K22,iel,nk) ! for identical elements in line
        IMPLICIT NONE
       INTEGER :: nk,nel,iel,ia,ib
        REAL(8), DIMENSION(3,3) :: K11,K12,K21,K22
        REAL(8), DIMENSION(nk*3,nk*3) :: Gstiff
          nel = nk - 1
          ia = (iel-1)*3 + 1
          ib = ia + 3
          Gstiff(ia:ia+2,ia:ia+2) = Gstiff(ia:ia+2,ia:ia+2) + K11
          Gstiff(ia:ia+2,ib:ib+2) = Gstiff(ia:ia+2,ib:ib+2) + K12
          Gstiff(ib:ib+2,ia:ia+2) = Gstiff(ib:ib+2,ia:ia+2) + K21
          Gstiff(ib:ib+2,ib:ib+2) = Gstiff(ib:ib+2,ib:ib+2) + K22
       END SUBROUTINE assembleStiff
!======================================================================================
      SUBROUTINE symSolve(gstiff,load,disp,n)
        IMPLICIT NONE
        INTEGER,intent(in)  :: n
        REAL(8),intent(in) :: gstiff(n,n),load(n)
        REAL(8),intent(out) :: disp(n)
        INTEGER :: i,j,k,m
        REAL(8) :: a(n,n+1),c, d
        m = n+1
        a(1:n,1:n)=gstiff
        a(1:n,m) = load
        DO 100 i = 1, n
        c = a(i,i)
        if(c==0) stop 'error c=0 in symSolve'
        a(i,i) = c - 1.0d0
        DO 100 k = i+1, m
        d = a(i,k) / c
        DO 100 j = 1, n
        a(j,k) = a(j,k) - d*a(j,i)
  100   continue
        disp = a(1:n,m)
        RETURN
      END SUBROUTINE symSolve
!======================================================================================
      SUBROUTINE inverseSym(aa,n)
        IMPLICIT NONE
        INTEGER :: i,j,k,m,n
        REAL(8) :: a(n,2*n),aa(n,n)
        REAL(8) :: c, d
        m = 2*n
        a = 0
        a(1:n,1:n)=aa
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
!======================================================================================
      SUBROUTINE addGreenStiffness(Gstiff,avalue,nk,Lele)
      IMPLICIT NONE
      INTEGER :: i,j,nk,ik,jk,ia,ib
      REAL(8) :: Gstiff(3*nk,3*nk), avalue,Compliance(nk,nk),C1,C2,Lele
      REAL(8), PARAMETER :: pi  = 3.141592653589793238462643d0
       C1 = 2/(4*pi*Lele*avalue)
       C2 = 4*C1
       DO i=1,nk
         DO j=1,i-1
           Compliance(i,j) = C1/ABS(i-j)
           Compliance(j,i) = C1/ABS(j-i)
         END DO
         Compliance(i,i) = C2
       END DO
       CALL inverseSym(Compliance,nk)
       DO ik=1,nk
         DO jk=1,nk
           ia = (ik-1)*3 + 2
           ib = (jk-1)*3 + 2
           Gstiff(ia,ib) = Gstiff(ia,ib) + Compliance(ik,jk)
         END DO
       END DO
      END SUBROUTINE addGreenStiffness
!======================================================================================
      SUBROUTINE addWinklerStiffness(Gstiff,Kwinkler,nk,Lele)
      IMPLICIT NONE
      INTEGER :: nk,ik,ia
      REAL(8) :: Gstiff(3*nk,3*nk), Kwinkler, C2,Lele
       C2 = Lele**2*Kwinkler
       DO ik=1,nk
           ia = (ik-1)*3 + 2
           Gstiff(ia,ia) = Gstiff(ia,ia) + C2
       END DO
      END SUBROUTINE addWinklerStiffness
!======================================================================================
      subroutine StaticCondensation(K,n,ired) ! only a single dof can be condensed
      implicit none
      real(8):: K(n,n),out(n-1,n-1), K11(n-1,n-1),K12(  n-1), K21( n-1), K22
      integer:: ired,n
      call swapcolumns(K,n,ired,n) ! move condensed row to the bottom and  column to the right edge
      call swaprows(K,n,ired,n)
      K11=K(1:n-1,1:n-1)
      K12=K(n,1:n-1)
      K21=K(1:n-1,n)
      K22=K(n,n)
      if (K22==0) stop'error in static condensation'
      call outerTimes(K12,n-1,K21,n-1,out)
      K11=K11-out/K22
      K=0
      K(1:n-1,1:n-1)=K11
      call swapcolumns(K,n,ired,n) ! move back condensed row   and  column
      call swaprows(K,n,ired,n)
      end subroutine StaticCondensation
! =====================================================================================
      subroutine swaprows(K,n,i,j)
      implicit none
      real(8)::K(n,n),Ri(n)
      integer :: i,j,n
      if (i==j ) return
      Ri=K(i,:)
      K(i,:)=K(j,:)
      K(j,:)=Ri
      end subroutine swaprows
! =====================================================================================
      subroutine swapcolumns(K,n,i,j)
      implicit none
      real(8)::K(n,n),Ci(n)
      integer :: i,j,n
      if (i==j ) return
      Ci=K(:,i)
      K(:,i)=K(:,j)
      K(:,j)=Ci
      end subroutine swapcolumns
! =====================================================================================
      subroutine outerTimes(a, m, b, n, c)
      implicit none
      integer, intent(in) :: m,n
      real(8),intent(in):: a(m),b(n)
      real(8), intent(out):: c(m,n)
      integer :: i,j
      forall( i=1:m, j=1:n ) c(i,j) = a(i)*b(j)
      end subroutine outerTimes
!======================================================================================
