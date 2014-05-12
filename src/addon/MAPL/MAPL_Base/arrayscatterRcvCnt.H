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
! $Id: arrayscatterRcvCnt.H,v 1.3 2013-01-17 14:54:55 atrayano Exp $

#ifdef NAME_
#undef NAME_
#endif

#define NAME_ ArrayScatterRcvCnt_

#include "overload.macro"

  subroutine SUB_(local_array, global_array, sendCounts, vm, srcPe, rc)
    TYPE_(kind=EKIND_),         intent(  OUT) :: local_array  DIMENSIONS_
    TYPE_(kind=EKIND_), target, intent(IN   ) :: global_array DIMENSIONS_
    integer                                   :: sendcounts(:)
    type(ESMF_VM)                             :: vm
    integer, optional,  intent(IN   )         :: srcPe
    integer, optional, intent(   OUT)         :: rc
    
! Local variables

    integer                               :: status
    character(len=ESMF_MAXSTR)            :: IAm='ArrayScatterRcvCnt'

    integer, allocatable, dimension(:)    :: displs
    integer                               :: nDEs
    integer                               :: recvcount
    
    integer                               :: I
    integer                               :: IM1 ! 'I minus 1'
    integer                               :: deId ! index of my PE
    integer                               :: srcDE
    
! This version works only for 1D arrays! 

    ASSERT_(RANK_ == 1)

    if(present(srcPe)) then
      srcDE = srcPe 
    else
      srcDE = MAPL_Root
    end if

    call ESMF_VMGet(vm, localPet=deId, petCount=ndes, rc=status)
    VERIFY_(STATUS)

    ASSERT_(size(sendCounts) == nDEs)

    allocate (displs(0:nDEs), stat=status)
    VERIFY_(STATUS)

    displs(0) = 0
    do I = 1,nDEs
       IM1 = I - 1
       displs(I) = displs(IM1) + sendcounts(I)
    enddo

! Count I will recieve

    recvcount = sendcounts(deId+1)

    call ESMF_VMScatterV(vm, global_array, sendcounts, displs, &
                         local_array, recvcount, srcDE, rc=status)
    VERIFY_(STATUS)

    deallocate(displs, stat=status)
    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)
  end subroutine SUB_

#undef NAME_

#undef DIMENSIONS_
#undef RANK_
#undef VARTYPE_
