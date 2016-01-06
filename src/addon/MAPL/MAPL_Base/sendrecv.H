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
! $Id: sendrecv.H,v 1.3 2008-08-28 20:24:07 trayanov Exp $

#ifdef NAME_
#undef NAME_
#endif

#define NAME_ MAPL_CommsSendRecv_

#include "overload.macro"

  subroutine SUB_ ( layout, SENDBUF, SENDCOUNT, DEST, RECVBUF, RECVCOUNT, SOURCE, RC)

    type (ESMF_DELayout)                         :: layout
#if (VARTYPE_ > 0)
    TYPE_(kind=EKIND_), intent(INOUT)            :: sendbuf DIMENSIONS_
    TYPE_(kind=EKIND_), intent(INOUT)            :: recvbuf DIMENSIONS_
#else
    character(LEN=*),   intent(INOUT)            :: sendbuf DIMENSIONS_
    character(LEN=*),   intent(INOUT)            :: recvbuf DIMENSIONS_
#endif

    integer,            intent(in   )            :: sendcount
    integer,            intent(in   )            :: recvcount
    integer,            intent(in   )            :: DEST
    integer,            intent(in   )            :: SOURCE
    integer         ,   intent(  out),  optional :: RC

    character(len=ESMF_MAXSTR), parameter :: IAM='MAPL_SendRecv'
    integer                               :: status
    integer                               :: mpstatus(MPI_STATUS_SIZE)
    integer                               :: ierr
    integer                               :: comm
    type(ESMF_VM)                         :: vm

    call ESMF_DELayoutGet(layout, vm=vm, rc=status)
    VERIFY_(STATUS)

    call ESMF_VMGet(vm, mpiCommunicator=COMM, rc=status)
    VERIFY_(STATUS)

    call MPI_SendRecv( sendbuf, sendcount, MPITYPE_, dest,   msg_tag, &
                       recvbuf, recvcount, MPITYPE_, source, msg_tag, &
                       COMM, mpstatus, IERR )
    VERIFY_(IERR)
  
    RETURN_(ESMF_SUCCESS)
  END SUBROUTINE SUB_

#undef NAME_

#undef DIMENSIONS_
#undef RANK_
#undef VARTYPE_


