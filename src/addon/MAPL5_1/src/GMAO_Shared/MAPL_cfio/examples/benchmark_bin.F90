!
! Similar to benchmark_sdf, but write flat FORTRAN sequential binaries instead.
!

    program test

#include "benchmark.h"

    use ESMF_CFIOMOD

!   define ESMF_CFIO,  ESMF_CFIOVarInfo, and ESMF_CFIOGrid objects

    integer :: t, i, j, k, ios, lu=20

    integer, parameter :: im=IM_, jm=JM_, km=KM_
 
    logical :: passed = .true.

!   variables and coordinates

    real :: ps(im,jm), ts(im,jm), tmpu(im,jm,km) 
    real :: u(im,jm,km), v(im,jm,km), q(im,jm,km) 
    real :: lon(im), lat(jm), lev(km)
    real :: dlat, dlon, sig, dsig

!   Define Coordinate variables

    dlat = 180./(jm-1)
    dlon = 360./im
    dsig = 1. / (km-1)
    do i =1, im
      lon(i) = -180 + (i-1)*dlon
    end do
    do j =1, jm
      lat(j) = -90 + (j-1)*dlat
    end do
    do k = 1, km
       sig = (k-1) * dsig
       lev(k) = 0.01 + sig * (1000.-0.01)
    end do

! put some test data into the variables

     ps = 1000.
     ts = 273.
     tmpu = 300.
     u = 10.
     v = 5.
     q = 20.

! Write data to the output file

  open(lu,file='benchmark_out.bin',form='unformatted',iostat=ios)
  if ( ios /= 0 ) then
       print *, "benchmark_bin: NOT Passed"
       call exit(1)
  endif

  write(lu) lon
  write(lu) lat
  write(lu) lev

  do t = 1, 8
     write(lu) ps
     write(lu) ts
     write(lu) tmpu
     write(lu) u
     write(lu) v
     write(lu) q
  end do

! Close the file

   close(lu)

   if ( passed ) then
       print *, "benchmark_bin: Passed"
   else
       print *, "benchmark_bin: NOT Passed"
   end if

   stop

   end 
