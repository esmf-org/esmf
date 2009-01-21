! $Id: ESMF_StateReconcile.F90,v 1.42.2.6 2009/01/21 21:25:25 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
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
      use ESMF_UtilTypesMod
      use ESMF_LogErrMod
      use ESMF_BaseMod
      use ESMF_VMMod
      use ESMF_ArrayMod
      use ESMF_ArrayBundleMod
      use ESMF_FieldMod
      use ESMF_FieldBundleMod
      use ESMF_StateTypesMod
      use ESMF_StateMod
      use ESMF_InitMacrosMod
      implicit none

      integer :: bufsize = 102400   ! 100 kB buffer

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
        ! TODO: these need to be integrated in a better fashion.
        integer(ESMF_KIND_I4) :: mycount, theircount
        integer(ESMF_KIND_I4), pointer, dimension(:) :: idsend, idrecv
        type(ESMF_VMId), pointer, dimension(:) :: vmidsend, vmidrecv
        integer(ESMF_KIND_I4), pointer, dimension(:) :: objsend, objrecv
        integer(ESMF_KIND_I4), pointer, dimension(:,:) :: blindsend, blindrecv
        ! TODO: longer term, build a linked list or tree of objects.
        !type(ESMF_StateItemInfo), pointer :: originalObject
        !integer :: blockType   ! new obj, dup, or end marker
        !integer :: objType     ! ESMF object type
        !integer :: objID       ! must be unique! (get from base class)
        !integer :: childCount
      end type

      integer, parameter :: ESMF_BT_NEWOBJ = 1, &
                            ESMF_BT_DUPOBJ = 2, &
                            ESMF_BT_ENDMARKER = 3

      ! obj types from base class, same with IDs

!TODO: This does not work as expected on systems which parse out comments
! before doing substitution.  (it replaces DEBUG with nothing.)
! The alternative is:  #define DEBUG  if (.false.) then
! but i am not sure if that is recognized at compile time and avoided
! or if it introduces a bunch of run-time tests.  (hopefully the former.)
! For now, just comment out the whole mess.
!#if 0
!#define DEBUG  print *, mypet, 
!#else
!#define DEBUG !!
!#define DEBUG if (.false.) then
!#endif



!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_StateReconcile          ! make consistent for concurrent apps

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_StateReconcile.F90,v 1.42.2.6 2009/01/21 21:25:25 cdeluca Exp $'

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
!BOP
! !IROUTINE: ESMF_StateReconcile -- Reconcile State data across all PETs in a VM
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
!     Must be called for any {\tt ESMF\_State} which contains ESMF objects
!     that have not been created on all the {\tt PET}s of the currently
!     running {\tt ESMF\_Component}.  
!     For example, if a coupler is operating on data
!     which was created by another component that ran on only a subset
!     of the coupler's {\tt PET}s, the coupler must make this call first
!     before operating on any data inside that {\tt ESMF\_State}.
!     After calling {\tt ESMF\_StateReconcile} all {\tt PET}s will have
!     a common view of all objects contained in this {\tt ESMF\_State}.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!       {\tt ESMF\_State} to reconcile.
!     \item[vm]
!       {\tt ESMF\_VM} for this {\tt ESMF\_Component}.
!     \item[{[options]}]
!       Currently unused.  Here for possible future expansion in the
!       options for the reconciliation process.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
    integer :: localrc
    type(ESMF_StateItemInfo), dimension(:), pointer :: stateinfo


    ! check input variables
    ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit,vm,rc)

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL


    ! This turns off the fast option on Regrid; it is working now for
    !  exclusive components, but if there is any reason we should turn
    !  it back off, here is how to do it.
    !!domainOption = 0

    ! Each PET broadcasts the object ID lists and compares them to what
    ! they get back.   Missing objects are sent so they can be recreated
    ! on the PETs without those objects.  Eventually we might want to
    ! hash the ID lists so we can send a single number (or short list of
    ! numbers) instead of having to build and send the list each time.
     

    ! This recursively descends the state objects and collects information
    ! about each one.
    nullify(stateinfo)
    call ESMF_StateInfoBuild(state, stateinfo, vm, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                              ESMF_ERR_PASSTHRU, &
                              ESMF_CONTEXT, rc)) return
    
    ! This one sends missing objects from the PETs which contain them
    ! to the PETs which do not.
    call ESMF_StateProxyCreate(state, stateinfo, vm, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                              ESMF_ERR_PASSTHRU, &
                              ESMF_CONTEXT, rc)) return

    ! This frees resources which were allocated during the building of
    ! the information blocks during the InfoBuild call.
    call ESMF_StateInfoDrop(stateinfo, rc=localrc)
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
!       This routine allocates and/or extends the array of blocks, so it
!       can come in as a NULL pointer and will return allocated and filled
!       with data.
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
    integer :: offset, mypet
    type(ESMF_VMId) :: VMdummyID

    ! check input variables
    ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit,vm,rc)

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL


    ! get total num pets.  this is not needed by the code, just the debug
    ! messages below.
    call ESMF_VMGet(vm, localPet=mypet, rc=rc)

    ! Get the VM ID of the state to use as a dummy in the code below
    call c_ESMC_GetVMId(state%statep, VMdummyID, localrc)

    ! make some initial space
    ! TODO: the current code only uses the first entry and hangs everything
    ! onto it.  eventually, have an info block for each distinct object in
    ! the state. 
    allocate(stateInfoList(4), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, &
                                   "Allocating buffer for ID list", &
                                   ESMF_CONTEXT, rc)) return
    allocate(stateInfoList(1)%childList(4), stat=localrc)
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
        allocate(si%vmidsend(si%mycount), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                   "Allocating buffer for local VM ID list", &
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
           case (ESMF_STATEITEM_FIELDBUNDLE%ot)
             call c_ESMC_GetID(stateitem%datap%fbp%btypep, si%idsend(i), localrc)
             call c_ESMC_GetVMId(stateitem%datap%fbp%btypep, si%vmidsend(i), localrc)
             si%objsend(i) = ESMF_ID_FIELDBUNDLE%objectID
             bptr => si%blindsend(:,i)
             call ESMF_FieldBundleSerialize(stateitem%datap%fbp, bptr, bufsize, &
                                       offset, localrc)
!!DEBUG "serialized bundle, obj=", si%objsend(i), " id=", si%idsend(i)

           case (ESMF_STATEITEM_FIELD%ot)
             call c_ESMC_GetID(stateitem%datap%fp%ftypep, si%idsend(i), localrc)
             call c_ESMC_GetVMId(stateitem%datap%fp%ftypep, si%vmidsend(i), localrc)
             si%objsend(i) = ESMF_ID_FIELD%objectID
             bptr => si%blindsend(:,i)
             call ESMF_FieldSerialize(stateitem%datap%fp, bptr, &
                                       bufsize, offset, localrc)
!!DEBUG "serialized field, obj=", si%objsend(i), " id=", si%idsend(i)

           case (ESMF_STATEITEM_ARRAY%ot)
             call c_ESMC_GetID(stateitem%datap%ap, si%idsend(i), localrc)
             call c_ESMC_GetVMId(stateitem%datap%ap, si%vmidsend(i), localrc)
             si%objsend(i) = ESMF_ID_ARRAY%objectID
             bptr => si%blindsend(:,i)
             call c_ESMC_ArraySerialize(stateitem%datap%ap, bptr(1), &
                                       bufsize, offset, localrc)
!!DEBUG "serialized array, obj=", si%objsend(i), " id=", si%idsend(i)

           case (ESMF_STATEITEM_ARRAYBUNDLE%ot)
             call c_ESMC_GetID(stateitem%datap%abp, si%idsend(i), localrc)
             call c_ESMC_GetVMId(stateitem%datap%abp, si%vmidsend(i), localrc)
             si%objsend(i) = ESMF_ID_ARRAYBUNDLE%objectID
             bptr => si%blindsend(:,i)
             call c_ESMC_ArrayBundleSerialize(stateitem%datap%abp, bptr(1), &
                                       bufsize, offset, localrc)
!!DEBUG "serialized arraybundle, obj=", si%objsend(i), " id=", si%idsend(i)

           case (ESMF_STATEITEM_STATE%ot)
             call c_ESMC_GetID(stateitem%datap%spp, si%idsend(i), localrc)
             call c_ESMC_GetVMId(stateitem%datap%spp, si%vmidsend(i), localrc)
             si%objsend(i) = ESMF_ID_STATE%objectID
             bptr => si%blindsend(:,i)
             wrapper%statep => stateitem%datap%spp
             call ESMF_StateSerialize(wrapper, bptr, bufsize, offset, localrc)
!!DEBUG "serialized substate, obj=", si%objsend(i), " id=", si%idsend(i)

           case (ESMF_STATEITEM_NAME%ot)
             si%idsend(i) = -1
             ! TODO: decide what this should be.
             si%vmidsend(i) = VMdummyID
             si%objsend(i) = ESMF_STATEITEM_NAME%ot
             bptr => si%blindsend(:,i)
             call c_ESMC_StringSerialize(stateitem%namep, bptr(1), bufsize, offset, localrc)
!!DEBUG "serialized placeholder, name=", trim(stateitem%namep)
             localrc = ESMF_SUCCESS
           case (ESMF_STATEITEM_INDIRECT%ot)
             si%idsend(i) = -2
             si%vmidsend(i) = VMdummyID
             si%objsend(i) = ESMF_STATEITEM_NAME%ot
             bptr => si%blindsend(:,i)
             call c_ESMC_StringSerialize(stateitem%namep, bptr(1), bufsize, offset, localrc)
!!DEBUG "serialized field-in-bundle, name=", trim(stateitem%namep)
             localrc = ESMF_SUCCESS
           case (ESMF_STATEITEM_UNKNOWN%ot)
             si%idsend(i) = -3
             si%vmidsend(i) = VMdummyID
             si%objsend(i) = ESMF_STATEITEM_NAME%ot
             bptr => si%blindsend(:,i)
             call c_ESMC_StringSerialize(stateitem%namep, bptr(1), bufsize, offset, localrc)
!!DEBUG "serialized unknown type, name=", trim(stateitem%namep)
             localrc = ESMF_SUCCESS
        end select

        if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) then
        
!!DEBUG "error -- i, offset, bufsize = ", i, offset, bufsize

           ! TODO: this is a bit too late; if offset has moved past the end
           ! of the buffer, we've already written over memory that is not ours.
           ! but better late than never??
           if (offset > bufsize) then
               call ESMF_LogMsgSetError(ESMF_RC_INTNRL_INCONS, &
                         "Too many objects in State for Reconcile to handle", &
                                        ESMF_CONTEXT, rc)
           endif
        ! either way, return here.
        return
        endif

!!DEUBG "i, offset, bufsize = ", i, offset, bufsize
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
!     Called after the communication is complete, this routine frees
!     any space allocated during the traversal of the state list and
!     communication of the object information.
!
!     The arguments are:
!     \begin{description}
!     \item[stateInfoList]
!       Array of info blocks, one for each object in the {\tt ESMF\_State}.
!       Allocated by previous code, this routine traverses the blocks
!       and frees all allocated space.  If no errors, this pointer 
!       returns nullified.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
    integer :: localrc
    type(ESMF_StateItemInfo), pointer :: si

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! shortname for use in the code below
    si => stateInfoList(1)

    ! The receive buffers are allocated and freed during the sending of
    ! objects.  What remains allocated are the send buffers which still
    ! need to be freed.  TODO: this needs to be better integrated with the
    ! object tree code below.
    if (si%mycount .gt. 0) then
        deallocate(si%idsend, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                 "Deallocating buffer for local ID list", &
                                       ESMF_CONTEXT, rc)) return
        deallocate(si%vmidsend, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                 "Deallocating buffer for local VM ID list", &
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

    ! The tree of objects - currently we are only using the first entry
    ! and creating blocks all attached to it.
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
!     Traverse the state object and build blocks for all subobjects.
!     Communicate with all other PETs in this {\tt ESMF\_VM} and determine
!     if any objects are unknown to other PETs.  If so, communicate enough
!     information to build proxy objects, which contain no local data, but
!     contain information about the size and distribution of the data on
!     other PETs.  Also communicate attribute information so the remote
!     object can be queried on any PET and return consistent informaion.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!       {\tt ESMF\_State} to add info into.
!     \item[stateInfoList]
!       Array of info blocks, one for each object in the {\tt ESMF\_State}.
!       This was built during the call to {\tt ESMF\_StateInfoBuild()}, and
!       is released by a call to {\tt ESMF\_StateInfoDrop()}.
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
    type(ESMF_FieldBundle) :: bundle
    type(ESMF_Field) :: field
    type(ESMF_Array) :: array
    type(ESMF_ArrayBundle) :: arraybundle
    character(len=ESMF_MAXSTR) :: thisname
    integer(ESMF_KIND_I4), pointer, dimension(:) :: bptr
    logical :: ihave
    type(ESMF_VMId) :: temp_vmid
    type(ESMF_StateItemInfo), pointer :: si
    integer :: offset

    ! check input variables
    ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit,vm,rc)

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! shorthand for the code below
    si => stateInfoList(1)

    ! get total num pets.
    call ESMF_VMGet(vm, localPet=mypet, petCount=pets, rc=rc)

    ! for i=0, npets-1, except us, send object count to each
    do j = 0, pets-1
!!DEBUG "Outer loop, j = ", j
       ! each takes turns sending to all, everyone else receives
       if (mypet .eq. j) then
           ! i am the sender, send in turn to each other pet
!!DEBUG "I am the sender this time, mypet = j"
           do i = 0, pets-1
               if (i .eq. j) cycle
!!DEBUG "I am sending to ", i
               ! count must be integer array
               count(1) = si%mycount
               call ESMF_VMSend(vm, count, 1, i, rc=localrc)
               if (ESMF_LogMsgFoundError(localrc, &
                                         ESMF_ERR_PASSTHRU, &
                                         ESMF_CONTEXT, rc)) return
!!DEBUG "completed send of obj count to ", i

               ! TODO: this can be removed.  
               !call ESMF_VMRecv(vm, count, 1, i, rc=localrc)
               !if (ESMF_LogMsgFoundError(localrc, &
               !                          ESMF_ERR_PASSTHRU, &
               !                          ESMF_CONTEXT, rc)) return
               !si%theircount = count(1)
               !if (si%theircount .ne. si%mycount) then
               !   print *, "object counts not same; ", &
               !           si%mycount, " .ne. ", si%theircount
               !endif
               ! at this point i know how many objects they have

        
               ! TODO:
               ! this code goes ahead and sends everything preemptively.
               ! once the code is working, change this so it only sends
               ! object numbers which appear in my list and not in theirs.

               if (si%mycount .gt. 0) then
!!DEBUG "i have ", si%mycount, "objects to send now"
                   call ESMF_VMSend(vm, si%idsend, si%mycount, i, rc=localrc)
                   if (ESMF_LogMsgFoundError(localrc, &
                                             ESMF_ERR_PASSTHRU, &
                                             ESMF_CONTEXT, rc)) return
!!DEBUG "completed send of id list"
                   call ESMF_VMSend(vm, si%objsend, si%mycount, i, rc=localrc)
                   if (ESMF_LogMsgFoundError(localrc, &
                                             ESMF_ERR_PASSTHRU, &
                                             ESMF_CONTEXT, rc)) return
!!DEBUG "completed send of obj type list"
                   do k = 1, si%mycount
                       temp_vmid = si%vmidsend(k)
                       call ESMF_VMSendVMId(vm, temp_vmid, i, rc=localrc)
                       if (ESMF_LogMsgFoundError(localrc, &
                                                 ESMF_ERR_PASSTHRU, &
                                                 ESMF_CONTEXT, rc)) return
!!DEBUG "completed send of vmid ", k
                   enddo
!!DEBUG "completed send of all vmids"

                   do m = 1, si%mycount
                     bptr => si%blindsend(:,m)
                     call ESMF_VMSend(vm, bptr, bufsize, i, rc=localrc)
                     if (ESMF_LogMsgFoundError(localrc, &
                                               ESMF_ERR_PASSTHRU, &
                                               ESMF_CONTEXT, rc)) return
!!DEBUG "completed send of serialized buffer ", m
                   enddo
!!DEBUG "completed send of all serialize buffers"
               endif
!!DEBUG "Done sending object information to ", i
               ! done sending to the next pet
           enddo
       else  ! i was not the sender this time, so i am receiving
!!DEBUG "I am receiving information from ", j
           call ESMF_VMRecv(vm, count, 1, j, rc=localrc)
           if (ESMF_LogMsgFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
!!DEBUG "completed recv of object count from ", j, "cnt =", count(1)
           si%theircount = count(1)

           !count(1) = si%mycount
           !call ESMF_VMSend(vm, count, 1, j, rc=localrc)
           !if (ESMF_LogMsgFoundError(localrc, &
           !                          ESMF_ERR_PASSTHRU, &
           !                          ESMF_CONTEXT, rc)) return
           ! 
           ! if (si%theircount .ne. si%mycount) then
           !     !print *, "object counts not same; ", &
           !               si%mycount, " .ne. ", si%theircount
           ! endif
           ! at this point, i know how many objects they have.

           ! TODO:
           ! go ahead and receive all objects preemptively and then decide
           ! if we need them.  once this code is working, only receive
           ! missing objects.

           if (si%theircount .gt. 0) then
!!DEBUG "pet ", j, " has ", si%theircount, " objs to send me"
               allocate(si%idrecv(si%theircount), stat=localrc)
               if (ESMF_LogMsgFoundAllocError(localrc, &
                                   "Allocating buffer for local ID list", &
                                   ESMF_CONTEXT, rc)) return
               allocate(si%vmidrecv(si%theircount), stat=localrc)
               if (ESMF_LogMsgFoundAllocError(localrc, &
                                   "Allocating buffer for local VM ID list", &
                                   ESMF_CONTEXT, rc)) return
               allocate(si%objrecv(si%theircount), stat=localrc)
               if (ESMF_LogMsgFoundAllocError(localrc, &
                                   "Allocating buffer for local obj list", &
                                   ESMF_CONTEXT, rc)) return
    
               allocate(si%blindrecv(bufsize, si%theircount), stat=localrc)
               if (ESMF_LogMsgFoundAllocError(localrc, &
                                   "Allocating buffer for local buf list", &
                                   ESMF_CONTEXT, rc)) return
!!DEBUG "allocated space to receive object information"
   
!!DEBUG "ready to receive id list from ", j
               call ESMF_VMRecv(vm, si%idrecv, si%theircount, j, rc=localrc)
               if (ESMF_LogMsgFoundError(localrc, &
                                         ESMF_ERR_PASSTHRU, &
                                         ESMF_CONTEXT, rc)) return
!!DEBUG "ready to receive obj id list from ", j
               call ESMF_VMRecv(vm, si%objrecv, si%theircount, j, rc=localrc)
               if (ESMF_LogMsgFoundError(localrc, &
                                         ESMF_ERR_PASSTHRU, &
                                         ESMF_CONTEXT, rc)) return

!!DEBUG "ready to start receive loop for vm ids"
               do k = 1, si%theircount
                   call ESMF_VMIdCreate(temp_vmid)
                   call ESMF_VMRecvVMId(vm, temp_vmid, j, rc=localrc)
                   if (ESMF_LogMsgFoundError(localrc, &
                                             ESMF_ERR_PASSTHRU, &
                                             ESMF_CONTEXT, rc)) return
!!DEBUG "received vm id ", k
                   si%vmidrecv(k) = temp_vmid
               enddo

!!DEBUG "ready to start receive loop for serialized object buffers from ", j
               do m = 1, si%theircount
                   bptr => si%blindrecv(:,m)
                   call ESMF_VMRecv(vm, bptr, bufsize, j, rc=localrc)
                   if (ESMF_LogMsgFoundError(localrc, &
                                             ESMF_ERR_PASSTHRU, &
                                             ESMF_CONTEXT, rc)) return
!!DEBUG "received serialized object buffer ", m, "from ", j
               enddo
           endif
!!DEBUG "end of object receive loop, at this point we have all info from ", j

           ! at this point we have all their objects here in our address
           ! space.  we just need to sort thru the lists and figure out
           ! if there are any which are missing from our list.

           do k=1, si%mycount
!!DEBUG  " num, send id and obj id", k, si%idsend(k), si%objsend(k)
           enddo
           do k=1, si%theircount
!!DEBUG  " num, recv id and obj id", k, si%idrecv(k), si%objrecv(k)
           enddo

           !!! TODO: 
           !!!   make a combined object id list here, so only one copy of
           !!!   the missing object is sent.

!!DEBUG "ready to check object matches given list from ", j
           do k=1, si%theircount
!!DEBUG " checking remote id for object ", k, "value is ", si%idrecv(k)
             ihave = .false.
             do l=1, si%mycount
!!DEBUG " checking local id for object ", l, "value is ", si%idsend(l)
                ! cannot just print a vmid, have to call real print routine
                !print *, "vm compare says ", ESMF_VMIdCompare(si%vmidrecv(k), si%vmidsend(l)) 
                if ((si%idrecv(k) .eq. si%idsend(l)) &
                .and. & 
     (ESMF_VMIdCompare(si%vmidrecv(k), si%vmidsend(l)) .eq. ESMF_TRUE)) then 
                     ihave = .true.
!!DEBUG "  objects match, no need to create proxy"
                     exit
                 endif
             enddo
!!DEBUG "  end of match loop for remote object ", k, "ihave flag is ", ihave
             if (.not. ihave) then
!!DEBUG " need to create local proxy object"
                offset = 0  
                select case (si%objrecv(k))
                   case (ESMF_ID_FIELDBUNDLE%objectID)
!!DEBUG "need to create proxy bundle, remote id=", si%idrecv(k)
                    bptr => si%blindrecv(:,k)
                    bundle = ESMF_FieldBundleDeserialize(vm, bptr, offset, localrc)
!!DEBUG "created bundle, ready to set id and add to local state"
                    call c_ESMC_SetVMId(bundle%btypep, si%vmidrecv(k), localrc)
                    call ESMF_StateAdd(state, bundle, proxyflag=.true., &
                      rc=localrc)
!!DEBUG "bundle added to state"

                   case (ESMF_ID_FIELD%objectID)
!!DEBUG "need to create proxy field, remote id=", si%idrecv(k)
                    bptr => si%blindrecv(:,k)
                    field = ESMF_FieldDeserialize(vm, bptr, offset, localrc)
!!DEBUG "created field, ready to set id and add to local state"
                    call c_ESMC_SetVMId(field%ftypep, si%vmidrecv(k), localrc)
                    call ESMF_StateAdd(state, field, proxyflag=.true., &
                      rc=localrc)
!!DEBUG "field added to state"

                   case (ESMF_ID_ARRAY%objectID)
!!DEBUG "need to create proxy array, remote id=", si%idrecv(k)
                    bptr => si%blindrecv(:,k)
                    call c_ESMC_ArrayDeserialize(array, bptr, offset, localrc)
                    ! Set init code
                    call ESMF_ArraySetInitCreated(array, rc=localrc)
!!DEBUG "created array, ready to set id and add to local state"
                    call c_ESMC_SetVMId(array, si%vmidrecv(k), localrc)
                    call ESMF_StateAdd(state, array, proxyflag=.true., &
                      rc=localrc)
!!DEBUG "array added to state"

                   case (ESMF_ID_ARRAYBUNDLE%objectID)
!!DEBUG "need to create proxy arraybundle, remote id=", si%idrecv(k)
                    bptr => si%blindrecv(:,k)
                    call c_ESMC_ArrayBundleDeserialize(arraybundle, bptr, &
                      offset, localrc)
                    ! Set init code
                    call ESMF_ArrayBundleSetInitCreated(arraybundle, rc=localrc)
!!DEBUG "created arraybundle, ready to set id and add to local state"
                    call c_ESMC_SetVMId(arraybundle, si%vmidrecv(k), localrc)
                    call ESMF_StateAdd(state, arraybundle, &
                      proxyflag=.true., rc=localrc)
!!DEBUG "arraybundle added to state"

                   case (ESMF_ID_STATE%objectID)
!!DEBUG "need to create proxy substate, remote id=", si%idrecv(k)
                    bptr => si%blindrecv(:,k)
                    substate = ESMF_StateDeserialize(vm, bptr, offset, localrc)
!!DEBUG "created substate, ready to set id and add to local state"
                    call c_ESMC_SetVMId(substate%statep, si%vmidrecv(k), localrc)
                    call ESMF_StateAdd(state, substate, proxyflag=.true., &
                      rc=localrc)
!!DEBUG "substate added to state"

                   case (ESMF_STATEITEM_NAME%ot)
!!DEBUG "need to create proxy placeholder name, remote id=", si%idrecv(k)
                     call c_ESMC_StringDeserialize(thisname, &
                                                   bptr(1), offset, localrc)
!!DEBUG "created string, ready to add to local state"
                     call ESMF_StateAdd(state, thisname, rc=localrc)
!!DEBUG "placeholder added to state"
         
                   case (ESMF_STATEITEM_INDIRECT%ot)
                     !print *, "field inside a bundle"
                     call c_ESMC_StringDeserialize(thisname, &
                                                   bptr(1), offset, localrc)
                     ! do nothing here
            
                   case (ESMF_STATEITEM_UNKNOWN%ot)
                     print *, "WARNING: unknown type"
                     call c_ESMC_StringDeserialize(thisname, &
                                                   bptr(1), offset, localrc)
                     ! do nothing here

                   case default
                    print *, "WARNING: unexpected type"
                end select
             endif
           enddo

!!DEBUG "end of proxy create section"

           if (si%theircount .gt. 0) then
!!DEBUG "the remote pet had sent us objects; remove temp space now"
               deallocate(si%idrecv, stat=localrc)
               if (ESMF_LogMsgFoundAllocError(localrc, &
                              "Deallocating buffer for local ID list", &
                               ESMF_CONTEXT, rc)) return
               do k = 1, si%theircount
                   call ESMF_VMIdDestroy(si%vmidrecv(k))
               enddo
               deallocate(si%vmidrecv, stat=localrc)
               if (ESMF_LogMsgFoundAllocError(localrc, &
                              "Deallocating buffer for local VM ID list", &
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
!!DEBUG "done deleting local space"
           
           ! TODO:
           ! and now, i have a different local object list.   brute force
           ! rebuild my send list.   Once this code is stable, revisit this
           ! and make a way to add the new object into my "known object"
           ! list without reserializing all objects.

!!DEBUG "drop old list and rebuild new one for our local objects"
           call ESMF_StateInfoDrop(stateInfoList, rc=localrc)
           if (ESMF_LogMsgFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return

           call ESMF_StateInfoBuild(state, stateInfoList, vm, localrc)
           if (ESMF_LogMsgFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return

!!DEBUG "reset pointer to state list"
           si => stateInfoList(1)

       endif   ! sender vs receiver
!!DEBUG "bottom of loop"
    enddo


!!DEBUG "end of state proxy create"
    if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_StateProxyCreate


end module ESMF_StateReconcileMod


