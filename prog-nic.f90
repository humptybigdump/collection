program nic  
implicit none 
integer ,dimension(2,2) :: CC, DD, dCC 
integer :: det, getDet   

CC = transpose( reshape([1,2,1,2], [2,2] ) ) 
dCC = transpose( reshape([1,1,0,0], [2,2] ) )  

call update(CC , dCC )

 det= getDet(CC)
 
write(*,'(2I4)') Transpose( CC ) 
write(*,*)  '   det = ', det  

 end program nic

subroutine update( A, B)
implicit none 
integer, dimension(2,2), intent(inout) :: A 
integer, dimension(2,2), intent(in) :: B 
integer :: i,j
do i=1,2 
do j=1,2
A(i,j) = A(i,j) + B(i,j)
enddo 
enddo 

end subroutine update 

 function getDet(A)
implicit none 
integer, dimension(2,2) :: A
integer :: getDet 
getDet = A(1,1) *  A(2,2) -  A(1,2) *  A(2,1)   
end function getDet 
