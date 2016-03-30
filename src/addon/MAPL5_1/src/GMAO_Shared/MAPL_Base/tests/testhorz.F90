use MAPL_HorzTransformMod

implicit none

integer, parameter :: imIN=144, imOUT=72
integer, parameter :: jmIN=91,  jmOUT=46
real, parameter :: pi=3.14159
integer :: rc

real a(imin,jmin), b(imout,jmout)
integer i,j

type(MAPL_HorzTransform) :: Trans


do j=1,jmin
do i=1,imin
a(i,j) = sin(4.*pi*(i-1)/imin)*sin(pi*(j-1)/(jmin-1))
end do
end do

a(20:80,20:30) = -999.


open(10,file='inputarr',form='unformatted',status='unknown')
write(10) a
close(10)


call MAPL_HorzTransformCreate  (Trans,           &
                                       imin,  jmin,   &
                                       imout, jmout,  &
!                                       .false.,0,      &
                                                    rc=rc)

print *, rc

call MAPL_HorzTransformRun(Trans,a,b,undef=-999.,rc=rc)

open(10,file='outputarr',form='unformatted',status='unknown')
write(10) b
close(10)

call MAPL_HorzTransformDestroy  (Trans)

stop
end

