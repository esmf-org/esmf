! $Id: ESMF_StateReconcile.F90,v 1.3 2004/11/18 20:47:40 nscollins Exp $
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
    ! TODO: these need to be folded in somehow.  temp test.
    integer(ESMF_KIND_I4) :: sendcount, recvcount
    integer(ESMF_KIND_I4), pointer, dimension(:) :: idsend, idrecv
    integer(ESMF_KIND_I4), pointer, dimension(:) :: objsend, objrecv
    integer(ESMF_KIND_I4), pointer, dimension(:) :: attrsend, attrrecv
    character(len=ESMF_MAXSTR), pointer, dimension(:) :: namesend, namerecv
    character(len=4096), pointer, dimension(:) :: blindsend, blindrecv
        integer :: blockType   ! new obj, dup, or end marker
        integer :: objType     ! ESMF object type
        integer :: objID       ! must be unique! (get from base class)
        integer :: attrCount   ! count of number of attributes
        integer :: childCount  !
        type(ESMF_StateItemInfo), dimension(:), pointer :: childList
        type(ESMF_StateItemInfo), dimension(:), pointer :: attrList
        type(ESMF_StateItemInfo), pointer :: originalObject
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
      '$Id: ESMF_StateReconcile.F90,v 1.3 2004/11/18 20:47:40 nscollins Exp $'

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

    ! make some initial space
    allocate(stateInfoList(32), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, &
                                   "Allocating buffer for ID list", &
                                   ESMF_CONTEXT, rc)) return
    allocate(stateInfoList(1)%childList(32), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, &
                                   "Allocating buffer for child ID list", &
                                   ESMF_CONTEXT, rc)) return

    ! TODO: this should be integrated with the info list blocks,
    !  but for now, brute force this to get something working.
    !

    ! shortname for use in the code below
    si => stateInfoList(1)

    si%sendcount = state%statep%datacount 
    if (si%sendcount .gt. 0) then
        allocate(si%idsend(si%sendcount), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                   "Allocating buffer for local ID list", &
                                       ESMF_CONTEXT, rc)) return
        allocate(si%objsend(si%sendcount), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                   "Allocating buffer for local obj list", &
                                       ESMF_CONTEXT, rc)) return
        allocate(si%attrsend(si%sendcount), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                   "Allocating buffer for local attr counts", &
                                       ESMF_CONTEXT, rc)) return
        allocate(si%namesend(si%sendcount), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                   "Allocating buffer for local name list", &
                                       ESMF_CONTEXT, rc)) return
        allocate(si%blindsend(si%sendcount), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                   "Allocating buffer for local buf list", &
                                       ESMF_CONTEXT, rc)) return
    endif

    si%sendcount = state%statep%datacount 
    do i=1, state%statep%datacount
        stateitem => state%statep%datalist(i)
        select case (stateitem%otype%ot)
           case (ESMF_STATEITEM_BUNDLE%ot)
             call c_ESMC_GetID(stateitem%datap%bp%btypep, si%idsend(i), localrc)
             call c_ESMC_AttributeGetCount(stateitem%datap%bp%btypep, &
                                           si%attrsend(i), localrc)
             call ESMF_BundleGet(stateitem%datap%bp, name=si%namesend(i), rc=localrc)
             si%objsend(i) = ESMF_ID_BUNDLE%objectID
             !call c_ESMC_BaseSerialize(stateitem%datap%bp%btypep, &
             !                        si%blindsend(i), 4096, 0, rc)
            print *, "getting bundle, obj=", si%objsend(i), " id=", si%idsend(i)
           case (ESMF_STATEITEM_FIELD%ot)
             call c_ESMC_GetID(stateitem%datap%fp%ftypep, si%idsend(i), localrc)
             call c_ESMC_AttributeGetCount(stateitem%datap%fp%ftypep, si%attrsend(i), localrc)
             call ESMF_FieldGet(stateitem%datap%fp, name=si%namesend(i), rc=localrc)
             si%objsend(i) = ESMF_ID_FIELD%objectID
             !call c_ESMC_BaseSerialize(stateitem%datap%fp%ftypep, &
             !                        si%blindsend(i), 4096, 0, rc)
            print *, "getting field, obj=", si%objsend(i), " id=", si%idsend(i)
           case (ESMF_STATEITEM_ARRAY%ot)
             call c_ESMC_GetID(stateitem%datap%ap, si%idsend(i), localrc)
             call c_ESMC_AttributeGetCount(stateitem%datap%ap, si%attrsend(i), localrc)
             call ESMF_ArrayGet(stateitem%datap%ap, name=si%namesend(i), rc=localrc)
             !call c_ESMC_BaseSerialize(stateitem%datap%ap, &
             !                        si%blindsend(i), 4096, 0, rc)
             si%objsend(i) = ESMF_ID_ARRAY%objectID
            print *, "getting array, obj=", si%objsend(i), " id=", si%idsend(i)
           case (ESMF_STATEITEM_STATE%ot)
             call c_ESMC_GetID(stateitem%datap%spp, si%idsend(i), localrc)
             call c_ESMC_AttributeGetCount(stateitem%datap%spp, si%attrsend(i), localrc)
             si%namesend(i) = stateitem%namep
             !call c_ESMC_BaseSerialize(stateitem%datap%spp, &
             !                        si%blindsend(i), 4096, 0, rc)
             si%objsend(i) = ESMF_ID_STATE%objectID
            print *, "getting state, obj=", si%objsend(i), " id=", si%idsend(i)
           case (ESMF_STATEITEM_NAME%ot)
             print *, "placeholder name"
             si%idsend(i) = -1
             si%objsend(i) = 0
             si%attrsend(i) = 0
             si%namesend(i) = stateitem%namep
             localrc = ESMF_SUCCESS
           case (ESMF_STATEITEM_INDIRECT%ot)
             print *, "field inside a bundle"
             si%idsend(i) = -2
             si%objsend(i) = 0
             si%attrsend(i) = 0
             si%namesend(i) = stateitem%namep
             localrc = ESMF_SUCCESS
           case (ESMF_STATEITEM_UNKNOWN%ot)
             print *, "unknown type"
             si%idsend(i) = -3
             si%objsend(i) = 0
             si%attrsend(i) = 0
             si%namesend(i) = stateitem%namep
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

    if (si%sendcount .gt. 0) then
        deallocate(si%idsend, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                 "Deallocating buffer for local ID list", &
                                       ESMF_CONTEXT, rc)) return
        deallocate(si%objsend, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                 "Deallocating buffer for local obj list", &
                                       ESMF_CONTEXT, rc)) return
        deallocate(si%attrsend, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                 "Deallocating buffer for local attr list", &
                                       ESMF_CONTEXT, rc)) return
        deallocate(si%namesend, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                 "Deallocating buffer for local name list", &
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
    character(len=4096) :: thisname
    logical :: ihave
    type(ESMF_StateItemInfo), pointer :: si

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
               count(1) = si%sendcount
               call ESMF_VMSend(vm, count, 1, i, rc=localrc)
               if (ESMF_LogMsgFoundError(localrc, &
                                         ESMF_ERR_PASSTHRU, &
                                         ESMF_CONTEXT, rc)) return
               call ESMF_VMRecv(vm, count, 1, i, rc=localrc)
               if (ESMF_LogMsgFoundError(localrc, &
                                         ESMF_ERR_PASSTHRU, &
                                         ESMF_CONTEXT, rc)) return
               si%recvcount = count(1)
               if (si%recvcount .ne. si%sendcount) then
                   print *, "object counts not same; more needed", &
                          si%sendcount, " .ne. ", si%recvcount

                   if (si%sendcount .gt. 0) then
                       call ESMF_VMSend(vm, si%idsend, si%sendcount, i, rc=localrc)
                       if (ESMF_LogMsgFoundError(localrc, &
                                                 ESMF_ERR_PASSTHRU, &
                                                 ESMF_CONTEXT, rc)) return
                       call ESMF_VMSend(vm, si%objsend, si%sendcount, i, rc=localrc)
                       if (ESMF_LogMsgFoundError(localrc, &
                                                 ESMF_ERR_PASSTHRU, &
                                                 ESMF_CONTEXT, rc)) return
                       call ESMF_VMSend(vm, si%attrsend, si%sendcount, i, rc=localrc)
                       if (ESMF_LogMsgFoundError(localrc, &
                                                 ESMF_ERR_PASSTHRU, &
                                                 ESMF_CONTEXT, rc)) return

                       do m = 1, si%sendcount
                       thisname = si%namesend(m)
                       print *, "ready to send name", m, "(", &
                                   trim(si%namesend(m)), ") to", i
                       call ESMF_VMSend(vm, thisname, ESMF_MAXSTR, i, rc=localrc)
                       print *, "back from send, localrc=", localrc
                           if (ESMF_LogMsgFoundError(localrc, &
                                                 ESMF_ERR_PASSTHRU, &
                                                 ESMF_CONTEXT, rc)) return

                       !thisname = si%blindsend(m)
                       !call ESMF_VMSend(vm, si%blindsend(m), 4096, i, rc=localrc)
                       !if (ESMF_LogMsgFoundError(localrc, &
                       !                          ESMF_ERR_PASSTHRU, &
                       !                          ESMF_CONTEXT, rc)) return
                       enddo

                   endif
               endif
           enddo
       else
           print *, mypet, "receives from", j
           call ESMF_VMRecv(vm, count, 1, j, rc=localrc)
           if (ESMF_LogMsgFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
           si%recvcount = count(1)

           count(1) = si%sendcount
           call ESMF_VMSend(vm, count, 1, j, rc=localrc)
           if (ESMF_LogMsgFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return

           if (si%recvcount .ne. si%sendcount) then
               print *, "object counts not same; more needed", &
                          si%sendcount, " .ne. ", si%recvcount

               if (si%recvcount .gt. 0) then
                   allocate(si%idrecv(si%recvcount), stat=localrc)
                   if (ESMF_LogMsgFoundAllocError(localrc, &
                                       "Allocating buffer for local ID list", &
                                       ESMF_CONTEXT, rc)) return
                   allocate(si%objrecv(si%recvcount), stat=localrc)
                   if (ESMF_LogMsgFoundAllocError(localrc, &
                                       "Allocating buffer for local obj list", &
                                       ESMF_CONTEXT, rc)) return
                   allocate(si%attrrecv(si%recvcount), stat=localrc)
                   if (ESMF_LogMsgFoundAllocError(localrc, &
                                       "Allocating buffer for local attr list", &
                                       ESMF_CONTEXT, rc)) return
                   allocate(si%namerecv(si%recvcount), stat=localrc)
                   if (ESMF_LogMsgFoundAllocError(localrc, &
                                       "Allocating buffer for local name list", &
                                       ESMF_CONTEXT, rc)) return
     
                   call ESMF_VMRecv(vm, si%idrecv, si%recvcount, j, rc=localrc)
                   if (ESMF_LogMsgFoundError(localrc, &
                                             ESMF_ERR_PASSTHRU, &
                                             ESMF_CONTEXT, rc)) return
                   call ESMF_VMRecv(vm, si%objrecv, si%recvcount, j, rc=localrc)
                   if (ESMF_LogMsgFoundError(localrc, &
                                             ESMF_ERR_PASSTHRU, &
                                             ESMF_CONTEXT, rc)) return
                   call ESMF_VMRecv(vm, si%attrrecv, si%recvcount, j, rc=localrc)
                   if (ESMF_LogMsgFoundError(localrc, &
                                             ESMF_ERR_PASSTHRU, &
                                             ESMF_CONTEXT, rc)) return

                   do m = 1, si%recvcount
                       print *, "ready to get recv name", m, "from", j
                     call ESMF_VMRecv(vm, thisname, ESMF_MAXSTR, j, rc=localrc)
                       si%namerecv(m) = thisname
                       print *, "got name and localrc=", localrc, &
                                    trim(si%namerecv(m))
                       if (ESMF_LogMsgFoundError(localrc, &
                                                 ESMF_ERR_PASSTHRU, &
                                                 ESMF_CONTEXT, rc)) return
                       !call ESMF_VMRecv(vm, thisname, 4096, i, rc=localrc)
                       !si%blindrecv(m) = thisname
                       !if (ESMF_LogMsgFoundError(localrc, &
                       !                          ESMF_ERR_PASSTHRU, &
                       !                          ESMF_CONTEXT, rc)) return
                   enddo
               endif
               do k=1, si%sendcount
                 print *, "i am", mypet, " my send ids and objs are:", &
                                k, si%idsend(k), si%objsend(k)
               enddo
               do k=1, si%recvcount
                 print *, "i am", mypet, " my recv ids and objs are:", &
                                k, si%idrecv(k), si%objrecv(k)
               enddo

               do k=1, si%recvcount
                 ihave = .false.
                 do l=1, si%sendcount
                     if (si%idrecv(k) .eq. si%idsend(l)) then 
                         ihave = .true.
                         exit
                     endif
                 enddo
                 if (.not. ihave) then
                    select case (si%objrecv(k))
                       case (ESMF_ID_BUNDLE%objectID)
                        print *, "need to create proxy bundle, id=", si%idrecv(k)
                        bundle = ESMF_BundleCreate(name=si%namerecv(k), rc=localrc)
                        call c_ESMC_SetID(bundle%btypep, si%idrecv(k), localrc)
                        call ESMF_StateAddBundle(state, bundle, rc=localrc)
                       case (ESMF_ID_FIELD%objectID)
                        print *, "need to create proxy field, id=", si%idrecv(k)
                        field = ESMF_FieldCreateNoData(name=si%namerecv(k), rc=localrc)
                        call c_ESMC_SetID(field%ftypep, si%idrecv(k), localrc)
                        call ESMF_StateAddField(state, field, rc=localrc)
                       case (ESMF_ID_ARRAY%objectID)
                        print *, "need to create proxy array, id=", si%idrecv(k)
                        array = ESMF_ArrayCreate(2, ESMF_DATA_REAL, ESMF_R8, &
                                                 (/ 2,2 /), rc=localrc) 
                        call ESMF_ArraySet(array, name=si%namerecv(k), rc=localrc)
                        call c_ESMC_SetID(array, si%idrecv(k), localrc)
                        call ESMF_StateAddArray(state, array, rc=localrc)
                       case (ESMF_ID_STATE%objectID)
                        print *, "need to create proxy state, id=", si%idrecv(k)
                        substate = ESMF_StateCreate(si%namerecv(k), rc=localrc)
                        call c_ESMC_SetID(substate%statep, si%idrecv(k), localrc)
                        call ESMF_StateAddState(state, substate, rc=localrc)
                       case default
                        print *, "not needed yet, id=", si%idrecv(k)
                    end select
                    if (si%attrrecv(k) .gt. 0) print *, "with", si%attrrecv(k), "attributes"
                 endif
               enddo

               if (si%recvcount .gt. 0) then
                   deallocate(si%idrecv, stat=localrc)
                   if (ESMF_LogMsgFoundAllocError(localrc, &
                                  "Deallocating buffer for local ID list", &
                                   ESMF_CONTEXT, rc)) return
                   deallocate(si%objrecv, stat=localrc)
                   if (ESMF_LogMsgFoundAllocError(localrc, &
                                  "Deallocating buffer for local obj list", &
                                   ESMF_CONTEXT, rc)) return
                   deallocate(si%attrrecv, stat=localrc)
                   if (ESMF_LogMsgFoundAllocError(localrc, &
                                  "Deallocating buffer for local attr list", &
                                   ESMF_CONTEXT, rc)) return
                   deallocate(si%namerecv, stat=localrc)
                   if (ESMF_LogMsgFoundAllocError(localrc, &
                                  "Deallocating buffer for local name list", &
                                   ESMF_CONTEXT, rc)) return
               endif
           endif
       endif
    enddo


    if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_StateProxyCreate


end module ESMF_StateReconcileMod


