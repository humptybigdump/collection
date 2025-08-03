   MODULE basicTools      ! Module  exports  matrix or vector to Mma
    implicit none
    logical :: ok
    real(8), parameter :: smallNumber = 1.0d-10, largeNumber = 1.0d10

   interface write_Mma    ! polymorphic
      module procedure    writeII2mma, writeRR2mma, writeI2mma, writeR2mma
   end interface

   private   writeII2mma, writeRR2mma, writeI2mma, writeR2mma

   CONTAINS

     logical function  writeII2mma(outname, xout, ichannel)  !  output integer matrix xout in Mma format.
     character(*) :: outname                                  !  outname = {{ ... will be written
     integer, dimension(:,:) :: xout
     integer, optional :: ichannel                             ! Target ichannel must be opened by the caller
     integer :: iout
     integer :: i, n, m
     m = size(xout,2); n = size(xout,1)
     iout=6; if( present(ichannel) ) iout= ichannel
     write(iout,*) outname, '={'
     do i=1,n-1
     write(iout,'(1h{,10000(i5,1h,),i5,2h},,1h+)', advance='no')   xout(i,1:m-1)
     write(iout,'( i5,2h}, )')   xout(i,m)
     enddo
     i=n
      write(iout,'(1h{,10000(i5,1h,),i5,2h},)', advance='no')   xout(i,1:m-1)
      write(iout,'( i5,4h} }; )')   xout(i,m)
     writeII2mma = .true.
    end function writeII2mma

     logical function  writeRR2mma(outname, xout, ichannel) !  output real matrix xout in Mma format.
     character(*) :: outname
     real(8), dimension(:,:) :: xout
     integer, optional :: ichannel    ! Target ichannel must be opened by the caller
     integer :: iout
     integer :: i,  n, m
     m = size(xout,2); n = size(xout,1)
     iout=6; if( present(ichannel) ) iout= ichannel
     write(iout,*) outname, '={'
     do i=1,n-1
     write(iout,'(1h{,10000(f25.8,1h,),f25.8,2h},,1h+)', advance='no')   xout(i,1:m-1)
     write(iout,'( g20.8,2h}, )')   xout(i,m)
     enddo
     i=n
      write(iout,'(1h{,10000(f25.8,1h,),f25.8,2h},)', advance='no')   xout(i,1:m-1)
      write(iout,'( f25.8,4h} }; )')   xout(i,m)
     writeRR2mma = .true.
    end function writeRR2mma

    logical function  writeI2mma(outname, xout, ichannel)  !  output integer vector xout in Mma format.
     character(*) :: outname
     integer, dimension(:) ::xout
     integer, optional :: ichannel                       ! Target ichannel must be opened by the caller
     integer :: iout,   m
     iout=6; if( present(ichannel) ) iout= ichannel
     write(iout,'(a,a)', advance='no' ) outname, '={'
     m = size(xout)
     write(iout,'(  10000(i5,1h,) )', advance='no')   xout(1:m-1)
     write(iout,'(        i5,3h };  )' )   xout(m)
     writeI2mma= .true.
     end function writeI2mma

     logical function  writeR2mma(outname, xout, ichannel) !  output real vector xout in Mma format.
     character(*) :: outname
     real(8), dimension(:) ::xout
     integer, optional :: ichannel                     ! Target ichannel must be opened by the caller
     integer :: iout,   m
     iout=6; if( present(ichannel) ) iout= ichannel
     write(iout,'(a,a)', advance='no') outname, '={'
     m = size(xout)
     write(iout,'(  10000(f25.8,1h,) )', advance='no')   xout(1:m-1)
     write(iout,'(        f25.8,3h };  )' )   xout(m)
     writeR2mma= .true.
     end function writeR2mma

     end MODULE basicTools

