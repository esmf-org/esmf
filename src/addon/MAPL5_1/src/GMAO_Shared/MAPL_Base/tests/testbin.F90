
use HorzBinMod

type (HorzBinTransform) :: T
integer :: rc

!integer, parameter :: IMout=360*3, JMout=180*4+1, IMin=720, JMin=361
!integer, parameter :: IMin=360*3, JMin=180*4+1, IMout=720, JMout=360
integer, parameter :: IMin=144, JMin=91, IMout=72, JMout=46
real :: ain(IMin,jmin), aout(IMout,JMout)

do j=1,jmin
do i=1,IMin
ain(i,j) = sin(2*2.*3.1415926*(i-10)/float(IMin))*sin(-0.5*3.1415926 + 3.1415926*(j-1)/float(JMin))
end do
end do


call system_clock(ic0)
CALL HorzBinTransformCreate(T, IMin, JMin, IMout, jmout, rc)
if(rc/=0) then
print *, rc
stop __LINE__
endif
call system_clock(ic1,icr)
print *, 'Create transform: ',float(ic1-ic0)/float(icr)

call system_clock(ic0)
do l=1,72
call  HorzBinTransformRun (T, ain, aout,   rc=rc)
if(rc/=0) then
print *, rc
stop __LINE__
endif
 enddo
call system_clock(ic1,icr)
print *, 'Do 72 fields',float(ic1-ic0)/float(icr)
print *, rc

open(10,file='ain',form='unformatted',status='unknown')
write(10) ain
open(20,file='aout',form='unformatted',status='unknown')
write(20) aout


CALL HorzBinTransformDestroy(T, rc)
if(rc/=0) then
print *, rc
stop __LINE__
endif

CALL HorzBinTransformCreate(T, IMin, JMin, IMout, jmout, rc)
if(rc/=0) then
print *, rc
stop __LINE__
endif


call system_clock(ic0)
do l=1,72
call  HorzBinTransformRun (T, ain, aout, 1.e20,  rc=rc)
if(rc/=0) then
print *, rc
stop __LINE__
endif
enddo
call system_clock(ic1,icr)
print *, 'Do 72 fields',float(ic1-ic0)/float(icr)
print *, rc

write(10) ain
write(20) aout

aout = 2.e20

call system_clock(ic0)
do l=1,72
call  HorzBinTransformRun (ain, aout,1.e20,  rc=rc)
if(rc/=0) then
print *, rc
stop __LINE__
endif
enddo
call system_clock(ic1,icr)
print *, 'Do 72 fields',float(ic1-ic0)/float(icr)
print *, rc


write(10) ain
write(20) aout

stop

end

