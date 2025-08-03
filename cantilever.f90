program beam
    !Groß und Kleinschreibung ist egal im Editor bei fortran
    implicit none !Es wird hiermit gefordert, dass jede Variable deklariert werden muss
    
    integer, parameter :: nelem=3, nnods=nelem+1, ndofn=3,  ndofs=nnods*ndofn                        !ndofn = number of grades of freedom per node, ndofs= number degree freedom of complete system
    integer :: i1, io1, i2, io2, ielem
    real(8), parameter :: EA=1.0d6, EI=1.0d4, L=2.0d0, Z=0.0d0, GZ=1.0d12                       !GZ Große Zahl für Penalty Funktion
    real(8) :: A,B,C,D
	real(8),dimension(3,3) :: K11,K12,K21,K22
    real(8), dimension(6,6) :: KKe
    real(8), dimension(ndofs,ndofs) :: Kglob
    real(8), dimension(ndofs) :: Rglob, Uglob
    
    A=EA/L
    B=12*EI/L**3
    C=6*EI/L**2
    D=2*EI/L
    
    K11=transpose(reshape([A,Z,Z,  Z,B,C,   Z,C,2*D], [3,3]) )
    K22=transpose(reshape([A,Z,Z,  Z,B,-C,   Z,-C,2*D], [3,3]) )
    K12=transpose(reshape([-A,Z,Z,  Z,-B,C,   Z,-C,D], [3,3]) )
    K21=transpose(K12)
    
    !top = Transpose( reshape( [1,2,  2,3,   3,4] ,[2,3]) )      Topologiematrix, aber unnötig wenn von links anch rechts Knoten und Elemente nummeriert werden
    
    
    !Global Stifness
    Kglob(:,:) = 0.0d0 !Alle Spalten und Zeilen werden 0 gesetzt von Kglob= global Stifness
    Rglob(:) = 0.0d0  ! Belastungsvektor
    Uglob(:) = 0.0d0!Verschiebungsvektor
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
    
    Kglob(1,1) = Kglob(1,1) + GZ
    Kglob(2,2) = Kglob(2,2) + GZ
    Kglob(3,3) = Kglob(3,3) + GZ
    
    Rglob(ndofs-1) = -100.0d0
    
    call SymSolve(Kglob, Rglob, Uglob, ndofs)     !Ruft Solver für Gleichungssystem auf
    
    write(*,*) 'uy=', Uglob(ndofs-1), 'analytisch uy=', -100*(L*nelem)**3/3.0d0/EI
    
    
    
end program beam


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