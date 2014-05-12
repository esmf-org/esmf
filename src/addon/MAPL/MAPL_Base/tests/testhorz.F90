! +-======-+ 
!  Copyright (c) 2003-2007 United States Government as represented by 
!  the Admistrator of the National Aeronautics and Space Administration.  
!  All Rights Reserved.
!  
!  THIS OPEN  SOURCE  AGREEMENT  ("AGREEMENT") DEFINES  THE  RIGHTS  OF USE,
!  REPRODUCTION,  DISTRIBUTION,  MODIFICATION AND REDISTRIBUTION OF CERTAIN 
!  COMPUTER SOFTWARE ORIGINALLY RELEASED BY THE UNITED STATES GOVERNMENT AS 
!  REPRESENTED BY THE GOVERNMENT AGENCY LISTED BELOW ("GOVERNMENT AGENCY").  
!  THE UNITED STATES GOVERNMENT, AS REPRESENTED BY GOVERNMENT AGENCY, IS AN 
!  INTENDED  THIRD-PARTY  BENEFICIARY  OF  ALL  SUBSEQUENT DISTRIBUTIONS OR 
!  REDISTRIBUTIONS  OF THE  SUBJECT  SOFTWARE.  ANYONE WHO USES, REPRODUCES, 
!  DISTRIBUTES, MODIFIES  OR REDISTRIBUTES THE SUBJECT SOFTWARE, AS DEFINED 
!  HEREIN, OR ANY PART THEREOF,  IS,  BY THAT ACTION, ACCEPTING IN FULL THE 
!  RESPONSIBILITIES AND OBLIGATIONS CONTAINED IN THIS AGREEMENT.
!  
!  Government Agency: National Aeronautics and Space Administration
!  Government Agency Original Software Designation: GSC-15354-1
!  Government Agency Original Software Title:  GEOS-5 GCM Modeling Software
!  User Registration Requested.  Please Visit http://opensource.gsfc.nasa.gov
!  Government Agency Point of Contact for Original Software:  
!  			Dale Hithon, SRA Assistant, (301) 286-2691
!  
! +-======-+ 
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

