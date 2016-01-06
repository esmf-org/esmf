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

USE mapl_sortMod

integer*8 :: a(5), c(5)
integer*4 :: a4(5), c4(5)
integer*4 ::  b1(5,3), b2(3,5)
integer*4 ::  d1(5,3), d2(3,5)


a = (/5,3,8,6,-7/)
do j=1,3
b1(:,j) = a
b2(j,:) = a
enddo
a4 = a


!!$c=a
!!$call qsort(c,d2,5,0)
!!$print *, c
!!$
!!$c=a
!!$d2=b2
!!$call qsort(c,d2,5,3)
!!$print *, c
!!$
!!$do j=1,3
!!$print *, j, '===', d2(j,:)
!!$end do
!!$
!!$
!!$c  = a
!!$d1 = b1
!!$call qsort(c,d1,5,-3)
!!$print *, c
!!$do j=1,3
!!$print *, j, '===', d1(:,j)
!!$end do


C=A
d1=b1
call MAPL_SORT(C,d1,dim=1)
PRINT *, C
do j=1,3
print *, j, '===', d1(:,j)
end do

stop
end
