! $Id: ESMF_StateReconcile.F90,v 1.1 2004/11/01 23:40:27 nscollins Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!==============================================================================
!
#define ESMF_FILENAME "ESMF_StateReconcile.F90"
!
!     ESMF StateReconcile module
      module ESMF_StateReconcileMod
!
!==============================================================================
!
! This file contains the State class definition and all State
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
!------------------------------------------------------------------------------
#include "ESMF.h"
!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_StateMod - Data exchange between components
!
! !DESCRIPTION:
!
! The code in this file implements the Fortran function and subroutine 
!  interfaces to the {\tt State} class and associated data structures.
!
!
! !USES:
      use ESMF_BaseTypesMod
      use ESMF_BaseMod
      use ESMF_IOSpecMod
      use ESMF_LogErrMod
      use ESMF_VMMod
      use ESMF_ArrayMod
      use ESMF_ArrayGetMod
      use ESMF_FieldMod
      use ESMF_BundleMod
      use ESMF_XformMod
      use ESMF_StateTypesMod
      implicit none

!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_StateReconcile          ! make consistent for concurrent apps

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_StateReconcile.F90,v 1.1 2004/11/01 23:40:27 nscollins Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================


!==============================================================================

      contains

!==============================================================================


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateReconcile"
!BOPI
! !IROUTINE: ESMF_StateReconcile -- Reconcile the internal data from a State
!
! !INTERFACE:
      subroutine ESMF_StateReconcile(state, vm, options, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state
      type(ESMF_VM), intent(in) :: vm
      character (len = *), intent(in), optional :: options              
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Called on an {\tt ESMF\_State} which might contain ESMF objects
!     that have not been created on all the {\tt PET}s which the
!     coupler runs on.  For example, if a coupler is operating on data
!     which was created by another component which was running on a subset
!     of the coupler's {\tt PET}s, the coupler must make this call first
!     before operating on any data inside that {\tt ESMF\_State}.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!       {\tt ESMF\_State} to reconcile.
!     \item[vm]
!       {\tt ESMF\_VM} for this {\tt ESMF\_Component}.
!     \item[options]
!       Currently unused.  Here for possible future expansion in the
!       options for the reconciliation process.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
    integer :: pets, mypet, i, j, k, localrc
    integer(ESMF_KIND_I4), allocatable, dimension(:) :: idsend, idrecv
    integer(ESMF_KIND_I4), allocatable, dimension(:) :: objsend, objrecv
    integer(ESMF_KIND_I4) :: sendcount(1), recvcount(1)
    type(ESMF_StateItem), pointer :: stateitem

    ! each PET broadcasts the object ID lists and compares them to what
    ! they get back.   eventually, hash the ID lists so we can send a
    ! single number instead of having to scan the list each time.
     

    ! TODO: for now, broadcast the object counts
    sendcount(1) = state%statep%datacount 
    if (sendcount(1) .gt. 0) then
        allocate(idsend(sendcount(1)), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                   "Allocating buffer for local ID list", &
                                       ESMF_CONTEXT, rc)) return
        allocate(objsend(sendcount(1)), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                   "Allocating buffer for local obj list", &
                                       ESMF_CONTEXT, rc)) return
    endif
    do i=1, state%statep%datacount
        stateitem => state%statep%datalist(i)
        select case (stateitem%otype%ot)
           case (ESMF_STATEITEM_BUNDLE%ot)
             call c_ESMC_GetID(stateitem%datap%bp, idsend(i), localrc)
             objsend(i) = ESMF_ID_BUNDLE%objectID
           case (ESMF_STATEITEM_FIELD%ot)
             call c_ESMC_GetID(stateitem%datap%fp, idsend(i), localrc)
             objsend(i) = ESMF_ID_FIELD%objectID
           case (ESMF_STATEITEM_ARRAY%ot)
             call c_ESMC_GetID(stateitem%datap%ap, idsend(i), localrc)
             objsend(i) = ESMF_ID_ARRAY%objectID
           case (ESMF_STATEITEM_STATE%ot)
             call c_ESMC_GetID(stateitem%datap%spp, idsend(i), localrc)
             objsend(i) = ESMF_ID_STATE%objectID
           case (ESMF_STATEITEM_NAME%ot)
             print *, "placeholder name"
             idsend(i) = -1
             localrc = ESMF_SUCCESS
           case (ESMF_STATEITEM_INDIRECT%ot)
             print *, "field inside a bundle"
             idsend(i) = -2
             localrc = ESMF_SUCCESS
           case (ESMF_STATEITEM_UNKNOWN%ot)
             print *, "unknown type"
             idsend(i) = -3
             localrc = ESMF_SUCCESS
        end select
        if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return
    enddo
       
    ! get total num pets.
    call ESMF_VMGet(vm, localPet=mypet, petCount=pets, rc=rc)

    ! for i=0, npets-1, except us, send object count to each
    do j = 0, pets-1
       ! each takes turns sending to all, everyone else receives
       if (mypet .eq. j) then
           print *, j, "sends to everyone else"
           do i = 0, pets-1
               if (i .eq. j) cycle
               print *, "calling send to", i
               call ESMF_VMSend(vm, sendcount, 1, i, rc=localrc)
               if (ESMF_LogMsgFoundError(localrc, &
                                         ESMF_ERR_PASSTHRU, &
                                         ESMF_CONTEXT, rc)) return
               call ESMF_VMRecv(vm, recvcount, 1, i, rc=localrc)
               if (ESMF_LogMsgFoundError(localrc, &
                                         ESMF_ERR_PASSTHRU, &
                                         ESMF_CONTEXT, rc)) return
               if (recvcount(1) .ne. sendcount(1)) then
                   print *, "object counts not same; more needed", &
                          sendcount(1), " .ne. ", recvcount(1)

                   if (sendcount(1) .gt. 0) then
                       call ESMF_VMSend(vm, idsend, sendcount(1), i, rc=localrc)
                       if (ESMF_LogMsgFoundError(localrc, &
                                                 ESMF_ERR_PASSTHRU, &
                                                 ESMF_CONTEXT, rc)) return
                       call ESMF_VMSend(vm, objsend, sendcount(1), i, rc=localrc)
                       if (ESMF_LogMsgFoundError(localrc, &
                                                 ESMF_ERR_PASSTHRU, &
                                                 ESMF_CONTEXT, rc)) return

                   endif
               endif
           enddo
       else
           print *, mypet, "receives from", j
           call ESMF_VMRecv(vm, recvcount, 1, j, rc=localrc)
           if (ESMF_LogMsgFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
           call ESMF_VMSend(vm, sendcount, 1, j, rc=localrc)
           if (ESMF_LogMsgFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return

           if (recvcount(1) .ne. sendcount(1)) then
               print *, "object counts not same; more needed", &
                          sendcount(1), " .ne. ", recvcount(1)

               if (recvcount(1) .gt. 0) then
                   allocate(idrecv(recvcount(1)), stat=localrc)
                   if (ESMF_LogMsgFoundAllocError(localrc, &
                                       "Allocating buffer for local ID list", &
                                       ESMF_CONTEXT, rc)) return
                   allocate(objrecv(recvcount(1)), stat=localrc)
                   if (ESMF_LogMsgFoundAllocError(localrc, &
                                       "Allocating buffer for local obj list", &
                                       ESMF_CONTEXT, rc)) return
     
                   call ESMF_VMRecv(vm, idrecv, recvcount(1), j, rc=localrc)
                   if (ESMF_LogMsgFoundError(localrc, &
                                             ESMF_ERR_PASSTHRU, &
                                             ESMF_CONTEXT, rc)) return
                   call ESMF_VMRecv(vm, objrecv, recvcount(1), j, rc=localrc)
                   if (ESMF_LogMsgFoundError(localrc, &
                                             ESMF_ERR_PASSTHRU, &
                                             ESMF_CONTEXT, rc)) return

               endif
               do k=1, sendcount(1)
                 print *, "i am", mypet, " my send ids and objs are:", &
                                k, idsend(k), objsend(k)
               enddo
               do k=1, recvcount(1)
                 print *, "i am", mypet, " my recv ids and objs are:", &
                                k, idrecv(k), objrecv(k)
               enddo

               if (recvcount(1) .gt. 0) then
                   deallocate(idrecv, stat=localrc)
                   if (ESMF_LogMsgFoundAllocError(localrc, &
                                  "Deallocating buffer for local ID list", &
                                   ESMF_CONTEXT, rc)) return
                   deallocate(objrecv, stat=localrc)
                   if (ESMF_LogMsgFoundAllocError(localrc, &
                                  "Deallocating buffer for local obj list", &
                                   ESMF_CONTEXT, rc)) return
               endif
           endif
       endif
    enddo

    if (sendcount(1) .gt. 0) then
        deallocate(idsend, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                 "Deallocating buffer for local ID list", &
                                       ESMF_CONTEXT, rc)) return
        deallocate(objsend, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                 "Deallocating buffer for local obj list", &
                                       ESMF_CONTEXT, rc)) return
    endif

    if (present(rc)) rc = ESMF_SUCCESS

 
    end subroutine ESMF_StateReconcile


end module ESMF_StateReconcileMod


