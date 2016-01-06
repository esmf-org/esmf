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
! $Id: allgatherv.H,v 1.2 2008-12-11 14:45:02 trayanov Exp $

#ifdef NAME_
#undef NAME_
#endif

#define NAME_ MAPL_CommsAllGatherV_

#include "overload.macro"

  subroutine SUB_ (layout, sendbuf, sendcnt, &
                   recvbuf, recvcnts, displs, RC)
    type (ESMF_DELayout)                         :: layout
    TYPE_(kind=EKIND_), intent(IN   )            :: sendbuf DIMENSIONS_
    integer,            intent(IN   )            :: sendcnt
    TYPE_(kind=EKIND_), intent(INOUT)            :: recvbuf DIMENSIONS_
    integer,            intent(IN   )            :: recvcnts(:)
    integer,            intent(IN   )            :: displs(:)
    integer         ,   intent(  OUT),  optional :: RC

    character(len=ESMF_MAXSTR), parameter :: IAM='MAPL_CommsAllGatherV'
    integer                               :: status
    integer                               :: comm
    type(ESMF_VM)                         :: vm

    call ESMF_DELayoutGet(layout, vm=vm, rc=status)
    VERIFY_(STATUS)

    call ESMF_VMGet(vm, mpiCommunicator=COMM, rc=status)
    VERIFY_(STATUS)

    call mpi_allgatherv ( sendbuf, sendcnt,  MPITYPE_, &
                          recvbuf, recvcnts, displs, MPITYPE_, &
                          comm, status )
    VERIFY_(STATUS)


    RETURN_(ESMF_SUCCESS)
  END SUBROUTINE SUB_

#undef NAME_

#undef DIMENSIONS_
#undef RANK_
#undef VARTYPE_

