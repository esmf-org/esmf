! $Id: ESMF_StateReconcile.F90,v 1.7 2004/12/01 18:32:30 nscollins Exp $
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
! This file contains the State Reconcile class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
!------------------------------------------------------------------------------
#include "ESMF.h"
!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_StateReconcileMod - Data exchange within a component
!
! !DESCRIPTION:
!
! The code in this file implements the Fortran function and subroutine 
!  interfaces to ensure that {\tt ESMF\_State} data is consistent across
!  all PETs.  The intended use is by components that have subcomponents 
!  which run on subsets of the coupler's PET list.
!  Objects that have been created on only a subset of the PETs cannot be
!  identified to methods like Regrid or Redistribution since they have no 
!  valid handles to identify them.  The code here communicates the missing
!  object information to other PETs in the current VM.
!
!
! !USES:
      use ESMF_BaseTypesMod
      use ESMF_BaseMod
      use ESMF_LogErrMod
      use ESMF_VMMod
      use ESMF_ArrayMod
      use ESMF_ArrayGetMod
      use ESMF_ArrayCreateMod
      use ESMF_FieldMod
      use ESMF_BundleMod
      use ESMF_StateTypesMod
      use ESMF_StateMod
      implicit none

      integer :: bufsize = 8192

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
       private
!------------------------------------------------------------------------------
!     ! ESMF_StateItemInfo
!
!     ! Simple descriptor block for objects in a State

      type ESMF_StateItemInfo
      sequence
      private
        type(ESMF_StateItemInfo), dimension(:), pointer :: childList
        type(ESMF_StateItemInfo), dimension(:), pointer :: attrList
        type(ESMF_StateItemInfo), pointer :: originalObject
    ! TODO: these need to be folded in somehow.  temp test.
    integer(ESMF_KIND_I4) :: mycount, theircount
    integer(ESMF_KIND_I4), pointer, dimension(:) :: idsend, idrecv
    integer(ESMF_KIND_I4), pointer, dimension(:) :: objsend, objrecv
    integer(ESMF_KIND_I4), pointer, dimension(:,:) :: blindsend, blindrecv
        integer :: blockType   ! new obj, dup, or end marker
        integer :: objType     ! ESMF object type
        integer :: objID       ! must be unique! (get from base class)
        integer :: childCount  !
      end type

      integer, parameter :: ESMF_BT_NEWOBJ = 1, &
                            ESMF_BT_DUPOBJ = 2, &
                            ESMF_BT_ENDMARKER = 3

      ! obj types from base class, same with IDs

!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_StateReconcile          ! make consistent for concurrent apps

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_StateReconcile.F90,v 1.7 2004/12/01 18:32:30 nscollins Exp $'

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
!     Must be called on an {\tt ESMF\_State} which might contain ESMF objects
!     that have not been created on all the {\tt PET}s on which an
!     {\tt ESMF\_Component} runs.  
!     For example, if a coupler is operating on data
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
    integer :: localrc
    type(ESMF_StateItemInfo), dimension(:), pointer :: stateinfo

    ! each PET broadcasts the object ID lists and compares them to what
    ! they get back.   missing objects are sent so they can be recreated
    ! on the PETs without those objects.  eventually, we might want to
    ! hash the ID lists so we can send a single number (or short list of
    ! numbers) instead of having to build and send the list each time.
     
    nullify(stateinfo)
    call ESMF_StateInfoBuild(state, stateinfo, vm, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                              ESMF_ERR_PASSTHRU, &
                              ESMF_CONTEXT, rc)) return
    
    call ESMF_StateProxyCreate(state, stateinfo, vm, rc)
    if (ESMF_LogMsgFoundError(localrc, &
                              ESMF_ERR_PASSTHRU, &
                              ESMF_CONTEXT, rc)) return

    call ESMF_StateInfoDrop(stateinfo, rc=rc)
    if (ESMF_LogMsgFoundError(localrc, &
                              ESMF_ERR_PASSTHRU, &
                              ESMF_CONTEXT, rc)) return

    if (present(rc)) rc = ESMF_SUCCESS

 
    end subroutine ESMF_StateReconcile


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateInfoBuild"
!BOPI
! !IROUTINE: ESMF_StateInfoBuild -- Collect information for contained objects
!
! !INTERFACE:
      subroutine ESMF_StateInfoBuild(state, stateInfoList, vm, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state
      type(ESMF_StateItemInfo), dimension(:), pointer :: stateInfoList
      type(ESMF_VM), intent(in) :: vm
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!       {\tt ESMF\_State} to collect information from.
!     \item[stateInfoList]
!       Array of info blocks, one for each object in the {\tt ESMF\_State}.
!     \item[vm]
!       The current {\tt ESMF\_VM} (virtual machine).  All PETs in this
!       {\tt ESMF\_VM} will exchange information about objects which might
!       only be known to one or more PETs, and ensure all PETs in this VM
!       have a consistent view of the object list in this {\tt ESMF\_State}.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
    integer :: i, localrc
    type(ESMF_StateItem), pointer :: stateitem
    type(ESMF_StateItemInfo), pointer :: si
    type(ESMF_State) :: wrapper
    integer(ESMF_KIND_I4), pointer, dimension(:) :: bptr
    integer :: offset

    ! make some initial space
    allocate(stateInfoList(32), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, &
                                   "Allocating buffer for ID list", &
                                   ESMF_CONTEXT, rc)) return
    allocate(stateInfoList(1)%childList(32), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, &
                                   "Allocating buffer for child ID list", &
                                   ESMF_CONTEXT, rc)) return


    ! shortname for use in the code below
    si => stateInfoList(1)

    si%mycount = state%statep%datacount 
    if (si%mycount .gt. 0) then
        allocate(si%idsend(si%mycount), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                   "Allocating buffer for local ID list", &
                                       ESMF_CONTEXT, rc)) return
        allocate(si%objsend(si%mycount), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                   "Allocating buffer for local obj list", &
                                       ESMF_CONTEXT, rc)) return
        allocate(si%blindsend(bufsize, si%mycount), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                   "Allocating buffer for local buf list", &
                                       ESMF_CONTEXT, rc)) return
    endif

    si%mycount = state%statep%datacount 
    do i=1, state%statep%datacount
        stateitem => state%statep%datalist(i)
        offset = 0
        select case (stateitem%otype%ot)
           case (ESMF_STATEITEM_BUNDLE%ot)
             call c_ESMC_GetID(stateitem%datap%bp%btypep, si%idsend(i), localrc)
             si%objsend(i) = ESMF_ID_BUNDLE%objectID
             bptr => si%blindsend(:,i)
             call ESMF_BundleSerialize(stateitem%datap%bp, bptr, bufsize, &
                                       offset, localrc)
            print *, "setting bundle, obj=", si%objsend(i), " id=", si%idsend(i)

           case (ESMF_STATEITEM_FIELD%ot)
             call c_ESMC_GetID(stateitem%datap%fp%ftypep, si%idsend(i), localrc)
             si%objsend(i) = ESMF_ID_FIELD%objectID
             bptr => si%blindsend(:,i)
             call ESMF_FieldSerialize(stateitem%datap%fp, bptr, &
                                       bufsize, offset, rc)
            print *, "setting field, obj=", si%objsend(i), " id=", si%idsend(i)

           case (ESMF_STATEITEM_ARRAY%ot)
             call c_ESMC_GetID(stateitem%datap%ap, si%idsend(i), localrc)
             si%objsend(i) = ESMF_ID_ARRAY%objectID
             bptr => si%blindsend(:,i)
             call c_ESMC_ArraySerializeNoData(stateitem%datap%ap, bptr(1), &
                                       bufsize, offset, rc)
            print *, "setting array, obj=", si%objsend(i), " id=", si%idsend(i)

           case (ESMF_STATEITEM_STATE%ot)
             call c_ESMC_GetID(stateitem%datap%spp, si%idsend(i), localrc)
             si%objsend(i) = ESMF_ID_STATE%objectID
             bptr => si%blindsend(:,i)
             wrapper%statep => stateitem%datap%spp
             call ESMF_StateSerialize(wrapper, bptr, bufsize, offset, rc)
            print *, "setting state, obj=", si%objsend(i), " id=", si%idsend(i)

           case (ESMF_STATEITEM_NAME%ot)
             print *, "placeholder name"
             si%idsend(i) = -1
             si%objsend(i) = 0
             bptr => si%blindsend(:,i)
             call c_ESMC_StringSerialize(stateitem%namep, bptr(1), bufsize, offset, rc)
            print *, "setting placeholder, name=", stateitem%namep
             localrc = ESMF_SUCCESS
           case (ESMF_STATEITEM_INDIRECT%ot)
             print *, "field inside a bundle"
             si%idsend(i) = -2
             si%objsend(i) = 0
             bptr => si%blindsend(:,i)
             call c_ESMC_StringSerialize(stateitem%namep, bptr(1), bufsize, offset, rc)
            print *, "setting field-in-bundle, name=", stateitem%namep
             localrc = ESMF_SUCCESS
           case (ESMF_STATEITEM_UNKNOWN%ot)
             print *, "unknown type"
             si%idsend(i) = -3
             si%objsend(i) = 0
             bptr => si%blindsend(:,i)
             call c_ESMC_StringSerialize(stateitem%namep, bptr(1), bufsize, offset, rc)
            print *, "setting unknown, name=", stateitem%namep
             localrc = ESMF_SUCCESS
        end select
        if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return
    enddo
       
    if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_StateInfoBuild

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateInfoDrop"
!BOPI
! !IROUTINE: ESMF_StateInfoDrop -- Discard information for contained objects
!
! !INTERFACE:
      subroutine ESMF_StateInfoDrop(stateInfoList, rc)
!
! !ARGUMENTS:
      type(ESMF_StateItemInfo), dimension(:), pointer :: stateInfoList
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!
!     The arguments are:
!     \begin{description}
!     \item[stateInfoList]
!       Array of info blocks, one for each object in the {\tt ESMF\_State}.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
    integer :: localrc
    type(ESMF_StateItemInfo), pointer :: si

    ! shortname for use in the code below
    si => stateInfoList(1)

    if (si%mycount .gt. 0) then
        deallocate(si%idsend, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                 "Deallocating buffer for local ID list", &
                                       ESMF_CONTEXT, rc)) return
        deallocate(si%objsend, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                 "Deallocating buffer for local obj list", &
                                       ESMF_CONTEXT, rc)) return
        deallocate(si%blindsend, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                 "Deallocating buffer for local buf list", &
                                       ESMF_CONTEXT, rc)) return
    endif

    ! and now the list itself
    deallocate(stateInfoList(1)%childList, stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, &
                                   "Deallocating buffer for child ID list", &
                                   ESMF_CONTEXT, rc)) return
    deallocate(stateInfoList, stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, &
                                  "DeaAllocating buffer for state ID list", &
                                   ESMF_CONTEXT, rc)) return
    nullify(stateInfoList)

    if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_StateInfoDrop

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateProxyCreate"
!BOPI
! !IROUTINE: ESMF_StateProxyCreate -- Create missing objects
!
! !INTERFACE:
      subroutine ESMF_StateProxyCreate(state, stateInfoList, vm, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state
      type(ESMF_StateItemInfo), dimension(:), pointer :: stateInfoList
      type(ESMF_VM), intent(in) :: vm
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!       {\tt ESMF\_State} to add info into.
!     \item[stateInfoList]
!       Array of info blocks, one for each object in the {\tt ESMF\_State}.
!     \item[vm]
!       The current {\tt ESMF\_VM} (virtual machine).  All PETs in this
!       {\tt ESMF\_VM} will exchange information about objects which might
!       only be known to one or more PETs, and ensure all PETs in this VM
!       have a consistent view of the object list in this {\tt ESMF\_State}.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
    integer :: pets, mypet, i, j, k, l, m, localrc
    integer(ESMF_KIND_I4) :: count(1)
    type(ESMF_State) :: substate
    type(ESMF_Bundle) :: bundle
    type(ESMF_Field) :: field
    type(ESMF_Array) :: array
    character(len=bufsize) :: thisname
    integer(ESMF_KIND_I4), pointer, dimension(:) :: bptr
    logical :: ihave
    type(ESMF_StateItemInfo), pointer :: si
    integer :: offset

    ! shorthand for the code below
    si => stateInfoList(1)

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
               ! count must be integer array
               count(1) = si%mycount
               call ESMF_VMSend(vm, count, 1, i, rc=localrc)
               if (ESMF_LogMsgFoundError(localrc, &
                                         ESMF_ERR_PASSTHRU, &
                                         ESMF_CONTEXT, rc)) return
               call ESMF_VMRecv(vm, count, 1, i, rc=localrc)
               if (ESMF_LogMsgFoundError(localrc, &
                                         ESMF_ERR_PASSTHRU, &
                                         ESMF_CONTEXT, rc)) return
               si%theircount = count(1)
               if (si%theircount .ne. si%mycount) then
                   print *, "object counts not same; ", &
                          si%mycount, " .ne. ", si%theircount
               endif

               if (si%mycount .gt. 0) then
                   call ESMF_VMSend(vm, si%idsend, si%mycount, i, rc=localrc)
                   if (ESMF_LogMsgFoundError(localrc, &
                                             ESMF_ERR_PASSTHRU, &
                                             ESMF_CONTEXT, rc)) return
                   call ESMF_VMSend(vm, si%objsend, si%mycount, i, rc=localrc)
                   if (ESMF_LogMsgFoundError(localrc, &
                                             ESMF_ERR_PASSTHRU, &
                                             ESMF_CONTEXT, rc)) return

                   do m = 1, si%mycount
                     bptr => si%blindsend(:,m)
                     call ESMF_VMSend(vm, bptr, bufsize, i, rc=localrc)
                     print *, "back from buf send, localrc=", localrc
                     if (ESMF_LogMsgFoundError(localrc, &
                                               ESMF_ERR_PASSTHRU, &
                                               ESMF_CONTEXT, rc)) return

                   enddo
               endif
           enddo
       else  ! i was not the sender this time, so i am receiving
           print *, mypet, "receives from", j
           call ESMF_VMRecv(vm, count, 1, j, rc=localrc)
           if (ESMF_LogMsgFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
           si%theircount = count(1)

           count(1) = si%mycount
           call ESMF_VMSend(vm, count, 1, j, rc=localrc)
           if (ESMF_LogMsgFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return

           if (si%theircount .ne. si%mycount) then
               print *, "object counts not same; ", &
                          si%mycount, " .ne. ", si%theircount
           endif

           if (si%theircount .gt. 0) then
               allocate(si%idrecv(si%theircount), stat=localrc)
               if (ESMF_LogMsgFoundAllocError(localrc, &
                                   "Allocating buffer for local ID list", &
                                   ESMF_CONTEXT, rc)) return
               allocate(si%objrecv(si%theircount), stat=localrc)
               if (ESMF_LogMsgFoundAllocError(localrc, &
                                   "Allocating buffer for local obj list", &
                                   ESMF_CONTEXT, rc)) return
    
               allocate(si%blindrecv(bufsize, si%theircount), stat=localrc)
               if (ESMF_LogMsgFoundAllocError(localrc, &
                                   "Allocating buffer for local buf list", &
                                   ESMF_CONTEXT, rc)) return
   
               call ESMF_VMRecv(vm, si%idrecv, si%theircount, j, rc=localrc)
               if (ESMF_LogMsgFoundError(localrc, &
                                         ESMF_ERR_PASSTHRU, &
                                         ESMF_CONTEXT, rc)) return
               call ESMF_VMRecv(vm, si%objrecv, si%theircount, j, rc=localrc)
               if (ESMF_LogMsgFoundError(localrc, &
                                         ESMF_ERR_PASSTHRU, &
                                         ESMF_CONTEXT, rc)) return

               do m = 1, si%theircount
                   bptr => si%blindrecv(:,m)
                   call ESMF_VMRecv(vm, bptr, bufsize, j, rc=localrc)
                   print *, "got buf ", m, " localrc=", localrc
                   if (ESMF_LogMsgFoundError(localrc, &
                                             ESMF_ERR_PASSTHRU, &
                                             ESMF_CONTEXT, rc)) return
               enddo
           endif

           do k=1, si%mycount
             print *, "i am", mypet, " my send ids and objs are:", &
                            k, si%idsend(k), si%objsend(k)
           enddo
           do k=1, si%theircount
             print *, "i am", mypet, " my recv ids and objs are:", &
                            k, si%idrecv(k), si%objrecv(k)
           enddo

           !!! TODO: 
           !!!   make a combined object id list here, so only one copy of
           !!!   the missing object is sent.

           !!offset = 0  !TODO: wrong place, yes??
           do k=1, si%theircount
             ihave = .false.
             do l=1, si%mycount
                 if (si%idrecv(k) .eq. si%idsend(l)) then 
                     ihave = .true.
                     exit
                 endif
             enddo
             if (.not. ihave) then
                offset = 0  
                select case (si%objrecv(k))
                   case (ESMF_ID_BUNDLE%objectID)
                    print *, "need to create proxy bundle, id=", si%idrecv(k)
                    bptr => si%blindrecv(:,k)
                    bundle = ESMF_BundleDeserialize(bptr, offset, localrc)
                    call ESMF_StateAddBundle(state, bundle, rc=localrc)

                   case (ESMF_ID_FIELD%objectID)
                    print *, "need to create proxy field, id=", si%idrecv(k)
                    bptr => si%blindrecv(:,k)
                    field = ESMF_FieldDeserialize(bptr, offset, localrc)
                    call ESMF_StateAddField(state, field, rc=localrc)

                   case (ESMF_ID_ARRAY%objectID)
                    print *, "need to create proxy array, id=", si%idrecv(k)
                    bptr => si%blindrecv(:,k)
                    call c_ESMC_ArrayDeserializeNoData(array, bptr, offset, localrc)
                    call ESMF_StateAddArray(state, array, rc=localrc)

                   case (ESMF_ID_STATE%objectID)
                    print *, "need to create proxy state, id=", si%idrecv(k)
                    bptr => si%blindrecv(:,k)
                    substate = ESMF_StateDeserialize(bptr, offset, localrc)
                    call ESMF_StateAddState(state, substate, rc=localrc)

                   case (ESMF_STATEITEM_NAME%ot)
                     print *, "placeholder name"
                     call c_ESMC_StringDeserialize(thisname, &
                                                   bptr(1), offset, localrc)
                     call ESMF_StateAddNameOnly(state, thisname, rc=localrc)
         
                   case (ESMF_STATEITEM_INDIRECT%ot)
                     print *, "field inside a bundle"
                     call c_ESMC_StringDeserialize(thisname, &
                                                   bptr(1), offset, localrc)
                     ! do nothing here
            
                   case (ESMF_STATEITEM_UNKNOWN%ot)
                     print *, "unknown type"
                     call c_ESMC_StringDeserialize(thisname, &
                                                   bptr(1), offset, localrc)
                     ! do nothing here

                   case default
                    print *, "not needed yet, id=", si%idrecv(k)
                end select
             endif
           enddo

           if (si%theircount .gt. 0) then
               deallocate(si%idrecv, stat=localrc)
               if (ESMF_LogMsgFoundAllocError(localrc, &
                              "Deallocating buffer for local ID list", &
                               ESMF_CONTEXT, rc)) return
               deallocate(si%objrecv, stat=localrc)
               if (ESMF_LogMsgFoundAllocError(localrc, &
                              "Deallocating buffer for local obj list", &
                               ESMF_CONTEXT, rc)) return
               deallocate(si%blindrecv, stat=localrc)
               if (ESMF_LogMsgFoundAllocError(localrc, &
                              "Deallocating buffer for local buf list", &
                               ESMF_CONTEXT, rc)) return
           endif
       endif
    enddo


    if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_StateProxyCreate


end module ESMF_StateReconcileMod


