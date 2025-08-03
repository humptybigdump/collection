program accBeam                         !  to be compiled with gfortran -o acc  basicTools.f90 accBeam.f90

    use basicTools                      !  from a separate file

    implicit none
    integer, parameter :: nelem=100, nnods=nelem+1, ndofn=3,  ndofs=nnods*ndofn         !ndofn = number of grades of freedom per node, ndofs= number degree freedom of complete system
   
     real(8), parameter ::  Ltot =20.0d0, eW = 0.4
     real(8), parameter :: EA=1.0d6, EI=1.0d4, Le=Ltot/nelem                 
     real(8),dimension(3,3) :: K11,K12,K21,K22
     real(8), dimension(ndofs,ndofs) :: Kglob
    real(8), dimension(ndofs) :: Rglob, Uglob 
     real(8), dimension(nnods) :: Uacc , xx 
    integer :: i1, io1, i2, io2, ielem, dof1, i, j, dofm,im 
    real(8) :: kBed, scale


    call getBeamElement( EA, EI, Le, K11,K12,K21,K22 )    ! the same for all elements 







    Kglob(:,:) = 0.0d0 ! !Global Stifness
    Rglob(:) = 0.0d0   ! Belastungsvektor
    Uglob(:) = 0.0d0   !Verschiebungsvektor

    do ielem=1,nelem
        i1=ielem
        i2=ielem+1
        io1 = 3*(i1-1)+1
        io2 = 3*(i2-1)+1

        Kglob(io1:io1+2, io1:io1+2) = Kglob(io1:io1+2, io1:io1+2) + K11
        Kglob(io1:io1+2, io2:io2+2) = Kglob(io1:io1+2, io2:io2+2) + K12
        Kglob(io2:io2+2, io1:io1+2) = Kglob(io2:io2+2, io1:io1+2) + K21
        Kglob(io2:io2+2, io2:io2+2) = Kglob(io2:io2+2, io2:io2+2) + K22
    enddo

    call fixDOFs(1,1,  1,1,  Kglob, ndofs)

    ! bedding

      kBed = 100*eW*Le;
      dof1 = (nnods-1)* 3 + 2
      Kglob(2,2) =   Kglob(2,2) + kBed/2
      Kglob(dof1,dof1) =   Kglob(dof1,dof1) + kBed/2

      do  i = 2,nnods-1,1 ;
           j = (i-1)*3 + 2
          Kglob(j,j) =    Kglob(j,j) + kBed
      enddo

      dofm =((nnods/2)-1)*3 + 2
      Rglob(dofm) = -10.0


      forall(i=1:nnods) xx(i) =   (i-1)*Le  
      im = nnods/2; 
      scale =  1.0d0/ nnods / nnods
      forall(i=1:nnods)  Uacc(i) = -0.1d0 * scale* (i-im)**2      ! accumulated  settlement of soil due to cyclic loading; here it is prescribed

      Rglob(2:ndofs:3) = Rglob(2:ndofs:3) + Uacc(:)*kBed

    call SymSolve(Kglob, Rglob, Uglob, ndofs)     !Ruft Solver für Gleichungssystem auf

    open(10, file='accBeam.m')
    ok= write_Mma('xx',xx, 10)
    ok =  write_Mma('Uacc',Uacc, 10)
    ok =  write_Mma('Uglob',Uglob(2:ndofs:3), 10)
    close(10)
    
    Write(*,*) 'finished, results in file accBeam.m'

end program accBeam



    subroutine fixDOFs(  nodefrom ,  nodeto, doffrom, dofto, Kglob, ndofs )
    implicit none
    integer, intent(in) ::    nodefrom , nodeto, doffrom, dofto, ndofs
    real(8), intent(inout) ::  Kglob(ndofs,ndofs)
    integer ::  inode, idof, i
    real(8), parameter :: GZ=1.0d12     !GZ Große Zahl für Penalty Funktion
    do  inode=  nodefrom,  nodeto
    do  idof= doffrom,dofto
         i=(inode-1)*3 + idof
         if(i .le. 0 .or.  i>ndofs ) stop 'fixDOFs error: DOF to be fixed not in the range '
         Kglob(i,i) = Kglob(i,1) + GZ
    enddo; enddo 
    end subroutine fixDOFs



    subroutine  getBeamElement(EA, EI, L, K11,K12,K21,K22)
    implicit none
    real(8), parameter ::  Z=0.0d0
    real(8), intent(in) :: EA, EI, L
    real(8), dimension(3,3), intent(out) ::  K11,K12,K21,K22
    real(8) :: A,B,C,D                  !  local variables
    A=EA/L
    B=12*EI/L**3
    C=6*EI/L**2
    D=2*EI/L
    K11=transpose(reshape([A,Z,Z,  Z,B,C,   Z,C,2*D], [3,3]) )
    K22=transpose(reshape([A,Z,Z,  Z,B,-C,   Z,-C,2*D], [3,3]) )
    K12=transpose(reshape([-A,Z,Z,  Z,-B,C,   Z,-C,D], [3,3]) )
    K21=transpose(K12)
    end subroutine  getBeamElement





 SUBROUTINE symSolve(gstiff,load,disp,n)
    IMPLICIT NONE
    REAL(8) :: a(n,n+1),gstiff(n,n),load(n),disp(n)
    REAL(8) :: c, d
    INTEGER :: i,j,k,m,n
    m = n+1
    a(1:n,1:n)=gstiff
    a(1:n,m) = load
    DO i = 1, n
    c = a(i,i)
    a(i,i) = c - 1.0d0
    DO k = i+1, m
    d = a(i,k) / c
    DO j = 1, n
    a(j,k) = a(j,k) - d*a(j,i)
    enddo
    enddo
    enddo
    disp = a(1:n,m)
    RETURN
  END SUBROUTINE symSolve
