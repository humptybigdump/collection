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

