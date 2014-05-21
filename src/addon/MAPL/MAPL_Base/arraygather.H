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
! $Id: arraygather.H,v 1.10.124.2 2013-03-12 15:21:00 atrayano Exp $

#ifdef NAME_
#undef NAME_
#endif

#define NAME_ ArrayGather_

#include "overload.macro"

  subroutine SUB_(local_array, global_array, grid, mask, depe, hw, rc)
    TYPE_(kind=EKIND_), intent(IN   )            :: local_array DIMENSIONS_
    TYPE_(kind=EKIND_), intent(  OUT)            :: global_array DIMENSIONS_
    type (ESMF_Grid)      :: grid
    integer, optional,  intent(IN   )   :: mask(:)
    integer, optional,  intent(IN   )   :: depe
    integer, optional,  intent(IN   )   :: hw
    integer, optional, intent(OUT)     :: rc
    
! Local variables

    integer                               :: status
    character(len=ESMF_MAXSTR)            :: IAm='ArrayGather'

    type (ESMF_DELayout)  :: layout
    type (ESMF_DistGrid)  :: distGrid
    integer,               allocatable    :: AL(:,:)
    integer,               allocatable    :: AU(:,:)
    integer, allocatable, dimension(:)            :: recvcounts, displs, kk
    integer                                       :: nDEs
    integer                                       :: sendcount
    
    integer                                       :: I, J, K, II, JJ, LX
    integer                                       :: de, deId
    integer                                       :: NX, NY
    integer                                       :: I1, IN
    integer                                       :: ibeg,iend
    integer                                       :: gridRank
#if (RANK_ > 1)
    integer                                       :: J1, JN
    integer                                       :: k1, k2
    integer                                       :: jbeg,jend
#endif
    integer                                       :: ISZ, JSZ
    integer                                       :: destPE, myhw
    TYPE_(kind=EKIND_), allocatable               :: var(:)
    integer                               :: deList(1)
    
! Works only on 1D and 2D arrays
! Note: for tile variables the gridRank is 1 
! and the case RANK_=2 needs additional attention 

    ASSERT_(RANK_ <= 2)

    ISZ = size(GLOBAL_ARRAY,1)

#if (RANK_ == 2)
    JSZ = size(GLOBAL_ARRAY,2)
#else
    JSZ = 1
#endif

    if(present(depe)) then
      destPE = depe 
    else
      destPE = MAPL_Root
    end if

    if(present(hw)) then
      myhw = hw
    else
      myhw = 0
    end if

    call ESMF_GridGet    (GRID,   dimCount=gridRank, rc=STATUS);VERIFY_(STATUS)
    call ESMF_GridGet    (GRID,   distGrid=distGrid, rc=STATUS);VERIFY_(STATUS)
    call ESMF_DistGridGet(distGRID, delayout=layout, rc=STATUS);VERIFY_(STATUS)
    call ESMF_DELayoutGet(layout, deCount =nDEs, localDeList=deList, rc=status)
    VERIFY_(STATUS)
    deId = deList(1)

    allocate (AL(gridRank,0:nDEs-1),  stat=status)
    VERIFY_(STATUS)
    allocate (AU(gridRank,0:nDEs-1),  stat=status)
    VERIFY_(STATUS)

    call ESMF_DistGridGet(distgrid, &
         minIndexPDe=AL, maxIndexPDe=AU, rc=status)
    VERIFY_(STATUS)

    allocate (recvcounts(nDEs), displs(0:nDEs), stat=status)
    VERIFY_(STATUS)

    allocate(VAR(0:size(GLOBAL_ARRAY)-1), stat=status)
    VERIFY_(STATUS)

    displs(0) = 0
#if (RANK_ > 1)
    if (gridRank == 1) then
       J1 = lbound(local_array,2)
       JN = ubound(local_array,2)
    endif
#endif
    do I = 1,nDEs
       J = I - 1
       de = J
       I1 = AL(1,J)
       IN = AU(1,J)
#if (RANK_ > 1)
       if (gridRank > 1) then
          J1 = AL(2,J)
          JN = AU(2,J)
       end if
       recvcounts(I) = (IN - I1 + 1) * (JN - J1 + 1)
#else
       recvcounts(I) = (IN - I1 + 1)
#endif
       if (de == deId) then
          sendcount = recvcounts(I)      ! Count I will send
          ibeg = 1+myhw
          iend = IN-I1+1+myhw
#if (RANK_ > 1)
          jbeg = 1+myhw
          jend = JN-J1+1+myhw
#endif
       endif
       displs(I) = displs(J) + recvcounts(I)
    enddo

    if (present(mask) .or. myHW == 0) then
       call MAPL_CommsGatherV(layout, local_array, sendcount, &
                              var, recvcounts, displs, destPE, status)
    else
#if (RANK_ > 1)
       call MAPL_CommsGatherV(layout, local_array(ibeg:iend,jbeg:jend), &
                              sendcount, var, recvcounts, displs, destPE, &
                              status)
#else
       call MAPL_CommsGatherV(layout, local_array(ibeg:iend), sendcount, &
                              var, recvcounts, displs, destPE, status)
#endif
    end if
    VERIFY_(STATUS)

    if (deId == destPE) then
       if (present(mask)) then
          allocate(KK (0:nDEs-1        ), stat=status)
          VERIFY_(STATUS)
          KK = DISPLS(0:nDEs-1)

          do I=1,ISZ
             K = MASK(I)
             II = KK(K)
#if (RANK_ == 1)
             GLOBAL_ARRAY(I) = VAR(II)
#else
             LX = AU(1,K) - AL(1,K) + 1 
             do J=1,JSZ
                GLOBAL_ARRAY(I,J) = VAR(II+LX*(J-1))
             end do
#endif
             KK(MASK(I)) = KK(MASK(I)) + 1 
          end do

          deallocate(KK, stat=status)
          VERIFY_(STATUS)
       else
#if (RANK_ == 1)
          global_array = var ! ALT: I am not sure if this is correct
#else
          do I = 0,nDEs-1
             I1 = AL(1,I)
             IN = AU(1,I)
             J1 = AL(2,I)
             JN = AU(2,I)

             K = displs(I)
             do JJ=J1,JN
                do II=I1,IN
                   global_array(II,JJ) = var(K)
                   K = K+1
                end do
             end do
          end do
#endif
       end if ! if (present(mask))
    end if

    deallocate(VAR, stat=status)
    VERIFY_(STATUS)
    deallocate(recvcounts, displs, AU, AL, stat=status)
    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)
  end subroutine SUB_

#undef NAME_

#undef DIMENSIONS_
#undef RANK_
#undef VARTYPE_
