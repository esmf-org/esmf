! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research, 
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
!  which run on subsets of the couplers PET list.
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
      use ESMF_FieldGetMod
      use ESMF_FieldBundleMod
      use ESMF_StateTypesMod
      use ESMF_StateMod
      use ESMF_StateContainerMod
      use ESMF_StateItemMod
      use ESMF_InitMacrosMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
       private
!------------------------------------------------------------------------------
!     ! ESMF_StateItemInfo
!
!     ! Simple descriptor block for objects in a State

      type ESMF_StateItemInfo
#ifndef ESMF_SEQUENCE_BUG
#ifndef ESMF_NO_SEQUENCE
        sequence
#endif
#endif
        private
        type(ESMF_StateItemWrap), pointer :: siwrap(:)
        type(ESMF_StateItemInfo), pointer :: childList(:)
        type(ESMF_StateItemInfo), pointer :: attrList(:)
        ! TODO: these need to be integrated in a better fashion.
        integer(ESMF_KIND_I4) :: mycount, theircount
        integer(ESMF_KIND_I4),    pointer :: idsend(:),   idrecv(:)
        type(ESMF_VMId),          pointer :: vmidsend(:), vmidrecv(:)
        integer(ESMF_KIND_I4),    pointer :: objsend(:),  objrecv(:)
        character,                pointer :: blindsend(:,:), blindrecv(:,:)
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

      public ESMF_StateReconcile_v1       ! make consistent for concurrent apps

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id$'

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
! !IROUTINE: ESMF_StateReconcile -- Reconcile State data across all PETs in a VM
!
! !INTERFACE:
      subroutine ESMF_StateReconcile_v1 (state, vm, attreconflag, rc)
!
! !ARGUMENTS:
      type(ESMF_State),            intent(inout)         :: state
      type(ESMF_VM),               intent(in),  optional :: vm
      type(ESMF_AttReconcileFlag), intent(in),  optional :: attreconflag
      integer,                     intent(out), optional :: rc               
!
!
! !DESCRIPTION:
!     Must be called for any {\tt ESMF\_State} which contains ESMF objects
!     that have not been created on all the {\tt PET}s of the currently
!     running {\tt ESMF\_Component}.  
!     For example, if a coupler is operating on data
!     which was created by another component that ran on only a subset
!     of the couplers {\tt PET}s, the coupler must make this call first
!     before operating on any data inside that {\tt ESMF\_State}.
!     After calling {\tt ESMF\_StateReconcile} all {\tt PET}s will have
!     a common view of all objects contained in this {\tt ESMF\_State}.
!     The option to reconcile the metadata associated with the objects
!     contained in this {\tt ESMF\_State} also exists.  The default behavior
!     for this capability is to {\it not} reconcile metadata unless told
!     otherwise.
!
!     This call is collective across the specified VM.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!       {\tt ESMF\_State} to reconcile.
!     \item[{[vm]}]
!       {\tt ESMF\_VM} for this {\tt ESMF\_Component}.  By default, it is set to the current vm.
!     \item[{[attreconflag]}]
!       Flag to tell if Attribute reconciliation is to be done as well as data reconciliation.
!       This flag is documented in section \ref{const:attreconcile}.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
    integer :: localrc
    type(ESMF_VM) :: localvm
    type(ESMF_StateItemInfo), pointer :: stateinfo(:)
    type(ESMF_AttReconcileFlag) :: lattreconflag

    ! check input variables
    ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit,vm,rc)

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    if (present (vm)) then
      localvm = vm
    else
      call ESMF_VMGetCurrent(vm=localvm, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                             ESMF_ERR_PASSTHRU, &
                             ESMF_CONTEXT, rcToReturn=rc)) return
    end if

#if 0
    ! First remove all empty nested States from State
    ! Doing this leads to much lower (factor petCount) complexity of the 
    ! current ProxyCreate() code.
    call ESMF_StateZapEmptyNests(state, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
                              ESMF_ERR_PASSTHRU, &
                              ESMF_CONTEXT, rcToReturn=rc)) return
#endif

    ! Remove all of the proxy objects in the State so that when
    ! re-reconciling, any removed items will not get re-proxied.
    call ESMF_StateZapProxies(state, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! This turns off the fast option on Regrid; it is working now for
    !  exclusive components, but if there is any reason we should turn
    !  it back off, here is how to do it.
    !!domainOption = 0

    ! Each PET broadcasts the object ID lists and compares them to what
    ! they get back.   Missing objects are sent so they can be recreated
    ! on the PETs without those objects.  Eventually we might want to
    ! hash the ID lists so we can send a single number (or short list of
    ! numbers) instead of having to build and send the list each time.
     
    ! Set the optional ESMF_AttReconcileFlag
    if(present(attreconflag)) then
      lattreconflag = attreconflag
    else
      lattreconflag = ESMF_ATTRECONCILE_OFF
    endif

    ! This recursively descends the state objects and collects information
    ! about each one.
    nullify(stateinfo)
    call ESMF_StateInfoBuild(state, stateinfo, localvm, lattreconflag, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
                              ESMF_ERR_PASSTHRU, &
                              ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! This one sends missing objects from the PETs which contain them
    ! to the PETs which do not.
    call ESMF_StateProxyCreate(state, stateinfo, localvm, lattreconflag, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
                              ESMF_ERR_PASSTHRU, &
                              ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! This frees resources which were allocated during the building of
    ! the information blocks during the InfoBuild call.
    call ESMF_StateInfoDrop(stateinfo, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
                              ESMF_ERR_PASSTHRU, &
                              ESMF_CONTEXT, rcToReturn=rc)) return

    ! Reset the change flags in the Attribute hierarchy
    if (lattreconflag%value == ESMF_ATTRECONCILE_ON%value) then
      call c_ESMC_AttributeUpdateReset(state%statep%base, localrc);
      if (ESMF_LogFoundError(localrc, &
                              ESMF_ERR_PASSTHRU, &
                              ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    call ESMF_ReconcileZappedProxies(state, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! Only clear the reconcileneeded flag at this State level.
    ! Otherwise, there would be a problem with nested States that
    ! are aliased into other States.
    state%statep%reconcileneededflag = .false.

    if (present(rc)) rc = ESMF_SUCCESS

 
    end subroutine ESMF_StateReconcile_v1


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateInfoBuild"
!BOPI
! !IROUTINE: ESMF_StateInfoBuild -- Collect information for contained objects
!
! !INTERFACE:
      subroutine ESMF_StateInfoBuild(state, stateInfoList, vm, attreconflag, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state
      type(ESMF_StateItemInfo), pointer :: stateInfoList(:)
      type(ESMF_VM), intent(in) :: vm
      type(ESMF_AttReconcileFlag), intent(in) :: attreconflag        
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
!     \item[{[attreconflag]}]
!       Flag to tell if Attribute reconciliation is to be done as well as data reconciliation
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
    integer :: i, localrc, attreconstart
    type(ESMF_StateItem), pointer :: stateitem
    type(ESMF_StateItemInfo), pointer :: si
    type(ESMF_State) :: wrapper
    character, pointer :: bptr(:)
    integer :: offset, mypet
    type(ESMF_VMId) :: VMdummyID

    type(ESMF_InquireFlag) :: inqflag
    integer :: itemcount
    integer :: lbufsize, maxbufsize
    integer :: pass
    integer :: memstat

    ! check input variables
    ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit,vm,rc)

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL


    ! get total num pets.  this is not needed by the code, just the debug
    ! messages below.
    call ESMF_VMGet(vm, localPet=mypet, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
                              msg="Accessing the VM", &
                              ESMF_CONTEXT, rcToReturn=rc)) return

    ! Get the VM ID of the state to use as a dummy in the code below
    call c_ESMC_GetVMId(state%statep, VMdummyID, localrc)
    if (ESMF_LogFoundError(localrc, &
                              msg="Getting VM ID", &
                              ESMF_CONTEXT, rcToReturn=rc)) return

    ! make some initial space
    ! TODO: the current code only uses the first entry and hangs everything
    ! onto it.  eventually, have an info block for each distinct object in
    ! the state. 
    allocate(stateInfoList(4), stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, &
                                   msg="Allocating buffer for ID list", &
                                   ESMF_CONTEXT, rcToReturn=rc)) return
    allocate(stateInfoList(1)%childList(4), stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, &
                                   msg="Allocating buffer for child ID list", &
                                   ESMF_CONTEXT, rcToReturn=rc)) return


    ! shortname for use in the code below
    si => stateInfoList(1)

    si%siwrap => null ()
    call ESMF_ContainerGet (state%statep%stateContainer,  &
        itemCount=itemcount, itemList=si%siwrap, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        msg="Getting item pointers", &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! (+1) to allow top level State space to reconcile Attributes
    if (attreconflag == ESMF_ATTRECONCILE_ON) then
      si%mycount = itemcount + 1
    else
      si%mycount = itemcount
    endif
    if (si%mycount .gt. 0) then
        allocate(si%idsend(si%mycount), stat=memstat)
        if (ESMF_LogFoundAllocError(memstat, &
                                   msg="Allocating buffer for local ID list", &
                                       ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(si%vmidsend(si%mycount), stat=memstat)
        if (ESMF_LogFoundAllocError(memstat, &
                                   msg="Allocating buffer for local VM ID list", &
                                       ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(si%objsend(si%mycount), stat=memstat)
        if (ESMF_LogFoundAllocError(memstat, &
                                   msg="Allocating buffer for local obj list", &
                                       ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    do, pass=1,2

      select case (pass)
      case (1)
        ! inquire to find the size of the largest serialization buffer
        ! needed.
        inqflag = ESMF_INQUIREONLY
        maxbufsize = 0
        lbufsize = 1
        allocate (si%blindsend(1,si%mycount), stat=memstat)
        if (ESMF_LogFoundAllocError(memstat, &
                                 msg="Allocating inquiry pass dummy buffer", &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
      case (2)
        ! Allocate the buffer, and do the serialization for real
        inqflag = ESMF_NOINQUIRE
!DEBUG        print *, 'ESMF_StateInfoBuild: leading buffer dimension =', maxbufsize
        lbufsize = maxbufsize

        if (si%mycount > 0) then
          allocate(si%blindsend(lbufsize, si%mycount), stat=memstat)
          if (ESMF_LogFoundAllocError(memstat, &
                               msg="Allocating buffer for local buf list", &
                                   ESMF_CONTEXT, rcToReturn=rc)) return
        end if
      end select

    ! (+1) to allow top level State space to reconcile Attributes
    if (attreconflag == ESMF_ATTRECONCILE_ON) then
      si%mycount = itemcount + 1
      offset = 0
      call c_ESMC_GetID(state%statep%base, si%idsend(1), localrc)
      if (ESMF_LogFoundError(localrc, &
                             msg="Top level get ID", &
                             ESMF_CONTEXT, rcToReturn=rc)) return

      call c_ESMC_GetVMId(state%statep%base, si%vmidsend(1), localrc)
      if (ESMF_LogFoundError(localrc, &
                             msg="Top level get VM ID", &
                             ESMF_CONTEXT, rcToReturn=rc)) return

      si%objsend(1) = ESMF_ID_BASE%objectID
      bptr => si%blindsend(:,1)
      call c_ESMC_BaseSerialize(state%statep%base, bptr(1), lbufsize, offset, &
        attreconflag, inqflag, localrc)
      if (ESMF_LogFoundError(localrc, &
                             msg="Top level Base serialize", &
                             ESMF_CONTEXT, rcToReturn=rc)) return

      maxbufsize = offset
      attreconstart = 2
    else
      si%mycount = itemcount
      attreconstart = 1
    endif

    !  start from 2, top level State is number 1
    do i=attreconstart, si%mycount
        ! i-1 because we are starting from 2, all else should be i
        if (attreconflag == ESMF_ATTRECONCILE_ON) then
          stateitem => si%siwrap(i-1)%si
        else
          stateitem => si%siwrap(i)%si
        endif
        offset = 0
        select case (stateitem%otype%ot)
           case (ESMF_STATEITEM_FIELDBUNDLE%ot)
             call c_ESMC_GetID(stateitem%datap%fbp%this, si%idsend(i), localrc)
             if (ESMF_LogFoundError(localrc, &
                             msg="nested fieldbundle get ID", &
                             ESMF_CONTEXT, rcToReturn=rc)) return

             call c_ESMC_GetVMId(stateitem%datap%fbp%this, si%vmidsend(i), localrc)
             if (ESMF_LogFoundError(localrc, &
                             msg="nested fieldbundle get VM ID", &
                             ESMF_CONTEXT, rcToReturn=rc)) return

             si%objsend(i) = ESMF_ID_FIELDBUNDLE%objectID
             bptr => si%blindsend(:,i)
             call ESMF_FieldBundleSerialize(stateitem%datap%fbp, bptr, lbufsize, &
                                       offset, attreconflag=attreconflag, &
                                       inquireflag=inqflag, rc=localrc)
             if (ESMF_LogFoundError(localrc, &
                             msg="nested fieldbundle serialize", &
                             ESMF_CONTEXT, rcToReturn=rc)) return

!!DEBUG "serialized fieldbundle, obj=", si%objsend(i), " id=", si%idsend(i)

           case (ESMF_STATEITEM_FIELD%ot)
             call c_ESMC_GetID(stateitem%datap%fp%ftypep, si%idsend(i), localrc)
             if (ESMF_LogFoundError(localrc, &
                             msg="nested Field get ID", &
                             ESMF_CONTEXT, rcToReturn=rc)) return

             call c_ESMC_GetVMId(stateitem%datap%fp%ftypep, si%vmidsend(i), localrc)
             if (ESMF_LogFoundError(localrc, &
                             msg="nested Field get VM ID", &
                             ESMF_CONTEXT, rcToReturn=rc)) return

             si%objsend(i) = ESMF_ID_FIELD%objectID
             bptr => si%blindsend(:,i)
             call ESMF_FieldSerialize(stateitem%datap%fp, bptr, lbufsize, &
                                       offset, attreconflag=attreconflag, &
                                       inquireflag=inqflag, rc=localrc)
             if (ESMF_LogFoundError(localrc, &
                             msg="nested Field serialize", &
                             ESMF_CONTEXT, rcToReturn=rc)) return

!!DEBUG "serialized field, obj=", si%objsend(i), " id=", si%idsend(i)

           case (ESMF_STATEITEM_ARRAY%ot)
             call c_ESMC_GetID(stateitem%datap%ap, si%idsend(i), localrc)
             if (ESMF_LogFoundError(localrc, &
                             msg="nested Array get ID", &
                             ESMF_CONTEXT, rcToReturn=rc)) return

             call c_ESMC_GetVMId(stateitem%datap%ap, si%vmidsend(i), localrc)
             if (ESMF_LogFoundError(localrc, &
                             msg="nested Array get VM ID", &
                             ESMF_CONTEXT, rcToReturn=rc)) return

             si%objsend(i) = ESMF_ID_ARRAY%objectID
             bptr => si%blindsend(:,i)
             call c_ESMC_ArraySerialize(stateitem%datap%ap, bptr(1), &
                                       lbufsize, offset, attreconflag, &
                                       inqflag, localrc)
             if (ESMF_LogFoundError(localrc, &
                             msg="nested Array serialize", &
                             ESMF_CONTEXT, rcToReturn=rc)) return

!!DEBUG "serialized array, obj=", si%objsend(i), " id=", si%idsend(i)

           case (ESMF_STATEITEM_ARRAYBUNDLE%ot)
             call c_ESMC_GetID(stateitem%datap%abp, si%idsend(i), localrc)
             if (ESMF_LogFoundError(localrc, &
                             msg="nested arraybundle get ID", &
                             ESMF_CONTEXT, rcToReturn=rc)) return

             call c_ESMC_GetVMId(stateitem%datap%abp, si%vmidsend(i), localrc)
             if (ESMF_LogFoundError(localrc, &
                             msg="nested arraybundle get VM ID", &
                             ESMF_CONTEXT, rcToReturn=rc)) return

             si%objsend(i) = ESMF_ID_ARRAYBUNDLE%objectID
             bptr => si%blindsend(:,i)
             call c_ESMC_ArrayBundleSerialize(stateitem%datap%abp, bptr(1), &
                                       lbufsize, offset, attreconflag, &
                                       inqflag, localrc)
             if (ESMF_LogFoundError(localrc, &
                             msg="nested arraybundle serialize", &
                             ESMF_CONTEXT, rcToReturn=rc)) return

!!DEBUG "serialized arraybundle, obj=", si%objsend(i), " id=", si%idsend(i)

           case (ESMF_STATEITEM_STATE%ot)
             call c_ESMC_GetID(stateitem%datap%spp, si%idsend(i), localrc)
             if (ESMF_LogFoundError(localrc, &
                             msg="nested State get ID", &
                             ESMF_CONTEXT, rcToReturn=rc)) return

             call c_ESMC_GetVMId(stateitem%datap%spp, si%vmidsend(i), localrc)
             if (ESMF_LogFoundError(localrc, &
                             msg="nested State get VM ID", &
                             ESMF_CONTEXT, rcToReturn=rc)) return

             si%objsend(i) = ESMF_ID_STATE%objectID
             bptr => si%blindsend(:,i)
             wrapper%statep => stateitem%datap%spp
             ESMF_INIT_SET_CREATED(wrapper)
             call ESMF_StateSerialize(wrapper, bptr, lbufsize, offset, &
              attreconflag=attreconflag, inquireflag=inqflag, rc=localrc)
             if (ESMF_LogFoundError(localrc, &
                             msg="nested State serialize", &
                             ESMF_CONTEXT, rcToReturn=rc)) return

!!DEBUG "serialized substate, obj=", si%objsend(i), " id=", si%idsend(i)

#if 0
           case (ESMF_STATEITEM_NAME%ot)
             si%idsend(i) = -1
             ! TODO: decide what this should be.
             si%vmidsend(i) = VMdummyID
             si%objsend(i) = ESMF_STATEITEM_NAME%ot
             bptr => si%blindsend(:,i)
             call c_ESMC_StringSerialize(stateitem%namep, bptr(1), lbufsize, offset, &
               inqflag, localrc)
             if (ESMF_LogFoundError(localrc, &
                             msg="nested string serialize", &
                             ESMF_CONTEXT, rcToReturn=rc)) return

!!DEBUG "serialized placeholder, name=", trim(stateitem%namep)
             localrc = ESMF_SUCCESS

           case (ESMF_STATEITEM_INDIRECT%ot)
             si%idsend(i) = -2
             si%vmidsend(i) = VMdummyID
             si%objsend(i) = ESMF_STATEITEM_NAME%ot
             bptr => si%blindsend(:,i)
             call c_ESMC_StringSerialize(stateitem%namep, bptr(1), lbufsize, offset, &
               inqflag, localrc)
             if (ESMF_LogFoundError(localrc, &
                             msg="nested string serialize (indirect)", &
                             ESMF_CONTEXT, rcToReturn=rc)) return

!!DEBUG "serialized field-in-fieldbundle, name=", trim(stateitem%namep)
             localrc = ESMF_SUCCESS
#endif

           case (ESMF_STATEITEM_UNKNOWN%ot)
             si%idsend(i) = -3
             si%vmidsend(i) = VMdummyID
#if 0
             si%objsend(i) = ESMF_STATEITEM_NAME%ot
#else
             si%objsend(i) = ESMF_STATEITEM_UNKNOWN%ot
#endif
             bptr => si%blindsend(:,i)
             call c_ESMC_StringSerialize(stateitem%namep, bptr(1), lbufsize, offset, &
               inqflag, localrc)
             if (ESMF_LogFoundError(localrc, &
                             msg="nested unknown type serialize", &
                             ESMF_CONTEXT, rcToReturn=rc)) return

!!DEBUG "serialized unknown type, name=", trim(stateitem%namep)
             localrc = ESMF_SUCCESS
        end select

        if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) then
        
!!DEBUG "error -- i, offset, lbufsize = ", i, offset, lbufsize

           ! TODO: this is a bit too late; if offset has moved past the end
           ! of the buffer, we have already written over memory that is not ours.
           ! but better late than never??
           if (offset > lbufsize) then
               call ESMF_LogSetError(rcToCheck=ESMF_RC_INTNRL_INCONS, &
                         msg="Too many objects in State for Reconcile to handle", &
                                        ESMF_CONTEXT, rcToReturn=rc)
           endif
        ! either way, return here.
        return
        endif

!!DEBUG        print *, "ESMF_StateInfoBuild: i, offset, lbufsize = ", i, offset, lbufsize
        maxbufsize = max (maxbufsize, offset)
      end do

      if (pass == 1) then
        deallocate (si%blindsend, stat=memstat)
        if (ESMF_LogFoundDeAllocError(memstat, &
                             msg="Deallocating dummy buffer", &
                                 ESMF_CONTEXT, rcToReturn=rc)) return
      end if

    end do ! pass
 
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
    integer :: memstat

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
        deallocate(si%idsend, stat=memstat)
        if (ESMF_LogFoundDeallocError(memstat, &
                                 msg="Deallocating buffer for local ID list", &
                                       ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(si%vmidsend, stat=memstat)
        if (ESMF_LogFoundDeallocError(memstat, &
                                 msg="Deallocating buffer for local VM ID list", &
                                       ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(si%objsend, stat=memstat)
        if (ESMF_LogFoundDeallocError(memstat, &
                                 msg="Deallocating buffer for local obj list", &
                                       ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(si%blindsend, stat=memstat)
        if (ESMF_LogFoundDeallocError(memstat, &
                                 msg="Deallocating buffer for local buf list", &
                                       ESMF_CONTEXT, rcToReturn=rc)) return
    endif
         
    if (associated(si%siwrap)) then
        deallocate(si%siwrap, stat=memstat)
        if (ESMF_LogFoundDeallocError(memstat, &
                                 msg="Deallocating item pointers", &
                                       ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! The tree of objects - currently we are only using the first entry
    ! and creating blocks all attached to it.
    deallocate(stateInfoList(1)%childList, stat=memstat)
    if (ESMF_LogFoundDeallocError(memstat, &
                                   msg="Deallocating buffer for child ID list", &
                                   ESMF_CONTEXT, rcToReturn=rc)) return
    deallocate(stateInfoList, stat=memstat)
    if (ESMF_LogFoundDeallocError(memstat, &
                                  msg="DeaAllocating buffer for state ID list", &
                                   ESMF_CONTEXT, rcToReturn=rc)) return
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
      subroutine ESMF_StateProxyCreate(state, stateInfoList, vm, attreconflag, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state
      type(ESMF_StateItemInfo), dimension(:), pointer :: stateInfoList
      type(ESMF_VM), intent(in) :: vm
      type(ESMF_AttReconcileFlag), intent(in) :: attreconflag        
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
!     \item[{[attreconflag]}]
!       Flag to tell if Attribute reconciliation is to be done as well as data reconciliation
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
    integer :: pets, mypet, j, k, l, localrc, attreconstart
    integer(ESMF_KIND_I4) :: objcount, bsbufsize
    integer(ESMF_KIND_I4) :: comm_ints(2)
    type(ESMF_State) :: substate
    type(ESMF_Base) :: base
    type(ESMF_FieldBundle) :: fieldbundle
    type(ESMF_Field) :: field
    type(ESMF_Array) :: array
    type(ESMF_ArrayBundle) :: arraybundle
    character(len=ESMF_MAXSTR) :: thisname
    character, pointer, dimension(:) :: bptr
    logical :: ihave
    type(ESMF_StateItemInfo), pointer :: si
    integer :: offset, myOrigCount
    logical :: i_send, i_recv
    integer :: memstat

    ! check input variables
    ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit,vm,rc)

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! shorthand for the code below
    si => stateInfoList(1)

    ! get total num pets.
    call ESMF_VMGet(vm, localPet=mypet, petCount=pets, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
                   ESMF_ERR_PASSTHRU, &
                   ESMF_CONTEXT, rcToReturn=rc)) return

!TODO: gjt: commented this out as a temp. work-around after container integration
!    myOrigCount = si%mycount  ! my original count on entering this routine

petloop:  &
    do, j = 0, pets-1
!!DEBUG "Outer loop, j = ", j

       !TODO: gjt: This moved down here as a temporary work-around after the
       !TODO: gjt: container integration into State. The issue is that with 
       !TODO: gjt: the container underpinning new elements are inserted
       !TODO: gjt: alphabetically rather than appended at the end. This means
       !TODO: gjt: that when ESMF_StateInfoBuild() is called (below) it will
       !TODO: gjt: potentially mess up what is in the first myOrigCount
       !TODO: gjt: elements. The long term solution is to _not_ call 
       !TODO: gjt: ESMF_StateInfoBuild() over and over while receiving, but
       !TODO: gjt: instead use StateAdd() with relaxedflag when adding received
       !TODO: gjt: objects to the State.
       myOrigCount = si%mycount  ! current count on send side
       
       ! each PET takes turns broadcasting to all

       i_send = mypet == j
       i_recv = mypet /= j

       ! First, broadcast object count and blindsend buffer size.
       comm_ints(1) = myOrigCount
       comm_ints(2) = size (si%blindsend, dim=1)
       call ESMF_VMBroadcast (vm, comm_ints, count=size (comm_ints), &
                      rootPet=j, rc=localrc)
       if (ESMF_LogFoundError(localrc, &
                      ESMF_ERR_PASSTHRU, &
                      ESMF_CONTEXT, rcToReturn=rc)) return

       objcount  = comm_ints(1)
       if (i_recv)  &
           si%theircount = objcount
       bsbufsize = comm_ints(2)

       if (objcount > 0) then

           ! Broadcast the local object IDs
           if (i_send) then
               call ESMF_VMBroadcast (vm, si%idsend, count=myOrigCount, rootPet=j, rc=localrc)
           else
               allocate(si%idrecv(si%theircount), stat=memstat)
               if (ESMF_LogFoundAllocError(memstat, &
                              msg="Allocating buffer for local ID list", &
                              ESMF_CONTEXT, rcToReturn=rc)) return
               call ESMF_VMBroadcast (vm, si%idrecv, count=si%theircount, rootPet=j, rc=localrc)
           end if
           if (ESMF_LogFoundError(localrc, &
                          ESMF_ERR_PASSTHRU, &
                          ESMF_CONTEXT, rcToReturn=rc)) return
!!DEBUG "completed broadcast of id list"

           ! Broadcast the object types
           if (i_send) then
               call ESMF_VMBroadcast (vm, si%objsend, count=myOrigCount, rootPet=j, rc=localrc)
           else
               allocate(si%objrecv(si%theircount), stat=memstat)
               if (ESMF_LogFoundAllocError(memstat, &
                              msg="Allocating buffer for local obj list", &
                              ESMF_CONTEXT, rcToReturn=rc)) return
               call ESMF_VMBroadcast (vm, si%objrecv, count=si%theircount, rootPet=j, rc=localrc)
           end if
           if (ESMF_LogFoundError(localrc, &
                          ESMF_ERR_PASSTHRU, &
                          ESMF_CONTEXT, rcToReturn=rc)) return
!!DEBUG "completed broadcast of object type list"

           ! Broadcast VMIds
           if (i_send) then
               call ESMF_VMBcastVMId(vm, si%vmidsend, count=myOrigCount, &
                   rootPet=j, rc=localrc)
           else
               allocate(si%vmidrecv(si%theircount), stat=memstat)
               if (ESMF_LogFoundAllocError(memstat, &
                              msg="Allocating buffer for local VM ID list", &
                              ESMF_CONTEXT, rcToReturn=rc)) return
               call ESMF_VMIdCreate (si%vmidrecv(1:si%theircount))
               call ESMF_VMBcastVMId(vm, si%vmidrecv, count=si%theircount, &
                   rootPet=j, rc=localrc)
           end if
           if (ESMF_LogFoundError(localrc, &
                          ESMF_ERR_PASSTHRU, &
                          ESMF_CONTEXT, rcToReturn=rc)) return

           ! Broadcast serialized object buffers
           if (i_send) then
               call ESMF_VMBroadcast (vm, si%blindsend, bsbufsize*myOrigCount, rootPet=j, rc=localrc)
           else
               allocate(si%blindrecv(bsbufsize, si%theircount), stat=memstat)
               if (ESMF_LogFoundAllocError(memstat, &
                              msg="Allocating buffer for local buf list", &
                              ESMF_CONTEXT, rcToReturn=rc)) return
               call ESMF_VMBroadcast (vm, si%blindrecv, size (si%blindrecv), rootPet=j, rc=localrc)
           end if
           if (ESMF_LogFoundError(localrc, &
                          ESMF_ERR_PASSTHRU, &
                          ESMF_CONTEXT, rcToReturn=rc)) return

       end if ! objcount > 0

       if (i_recv) then

           ! at this point we have all their objects here in our address
           ! space.  we just need to sort thru the lists and figure out
           ! if there are any which are missing from our list.

#if defined (DEBUG)
           do k=1, si%mycount
!!DEBUG  " num, send id and obj id", k, si%idsend(k), si%objsend(k)
           enddo
           do k=1, si%theircount
!!DEBUG  " num, recv id and obj id", k, si%idrecv(k), si%objrecv(k)
           enddo
#endif

        ! unpack the top level State Attributes first
        if (attreconflag == ESMF_ATTRECONCILE_ON) then
          if (si%objrecv(1) == ESMF_ID_BASE%objectID) then
            bptr => si%blindrecv(:,1)
            offset = 0
            call c_ESMC_BaseDeserialize(base, bptr, offset, &
              attreconflag, localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return

            call ESMF_BaseSetInitCreated(base, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return

            call c_ESMC_AttributeCopy(base, state%statep%base, &
              ESMF_ATTCOPY_VALUE, localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return

          endif
          attreconstart = 2
        else
          attreconstart = 1
        endif

           !!! TODO: 
           !!!   make a combined object id list here, so only one copy of
           !!!   the missing object is sent.

           ! k from 2 because the top level State was number 1

itemloop:  do k=attreconstart, si%theircount
!!DEBUG " checking remote id for object ", k, "value is ", si%idrecv(k)
             ihave = .false.
             
#define IHAVEOLD
#ifdef IHAVEOLD
             do l=1, si%mycount
!!DEBUG " checking local id for object ", l, "value is ", si%idsend(l)
                ! cannot just print a vmid, have to call real print routine
                !print *, "vm compare says ", ESMF_VMIdCompare(si%vmidrecv(k), si%vmidsend(l)) 
                if ((si%idrecv(k) .eq. si%idsend(l)) &
                .and. & 
     ESMF_VMIdCompare(si%vmidrecv(k), si%vmidsend(l)) ) then
                          ihave = .true.
!!DEBUG "  objects match, no need to create proxy"
                     exit
                 endif
             enddo
#else
             ! Only search back a fixed number of times (20), otherwise simply
             ! add object. This prevents the following loop from becoming more
             ! and more expensive.
             
             !gjt -> turning this fixed size search on does not really help
             ! for the test code I was using. I believe that the complexity of
             ! the StateAdd() calls is also O(si%mycount)! Consequently this
             ! search here doesn't really matter much (from a complexity/
             ! scaling standpoint). It just comes in as an extra bit of factor!
             
             llow = max(1, si%mycount - 20)
             do l=si%mycount, llow, -1
               if ((si%idrecv(k) .eq. si%idsend(l)) .and. & 
     ESMF_VMIdCompare(si%vmidrecv(k), si%vmidsend(l)) ) then
                 ihave = .true.
                 exit
               endif
             enddo
#endif
             
!!DEBUG "  end of match loop for remote object ", k, "ihave flag is ", ihave
             if (.not. ihave) then
             
!!DEBUG " need to create local proxy object"
                offset = 0  
                bptr => si%blindrecv(:,k)
                select case (si%objrecv(k))
                   case (ESMF_ID_FIELDBUNDLE%objectID)

!!DEBUG "need to create proxy fieldbundle, remote id=", si%idrecv(k)
                    fieldbundle = ESMF_FieldBundleDeserialize(bptr, offset, &
                      attreconflag=attreconflag, rc=localrc)
                    if (ESMF_LogFoundError(localrc, &
                             msg="nested fieldbundle deserialize", &
                             ESMF_CONTEXT, rcToReturn=rc)) return

!!DEBUG "created fieldbundle, ready to set id and add to local state"
                    call c_ESMC_SetVMId(fieldbundle%this, si%vmidrecv(k), localrc)
                    if (ESMF_LogFoundError(localrc, &
                             msg="nested fieldbundle SetVMId call", &
                             ESMF_CONTEXT, rcToReturn=rc)) return

                    call ESMF_StateAdd(state, fieldbundle, &
                        addflag=.true., proxyflag=.true.,  &
                        rc=localrc)
                    if (ESMF_LogFoundError(localrc, &
                             msg="nested fieldbundle add to local state", &
                             ESMF_CONTEXT, rcToReturn=rc)) return
!!DEBUG "fieldbundle added to state"

                   case (ESMF_ID_FIELD%objectID)
!!DEBUG "need to create proxy field, remote id=", si%idrecv(k)
                    field = ESMF_FieldDeserialize(bptr, offset, &
                      attreconflag=attreconflag, rc=localrc)
                    if (ESMF_LogFoundError(localrc, &
                             msg="nested Field deserialize", &
                             ESMF_CONTEXT, rcToReturn=rc)) return

!!DEBUG "created field, ready to set id and add to local state"
                    call c_ESMC_SetVMId(field%ftypep, si%vmidrecv(k), localrc)
                    if (ESMF_LogFoundError(localrc, &
                             msg="nested Field SetVMId call", &
                             ESMF_CONTEXT, rcToReturn=rc)) return

                    call ESMF_StateAdd(state, field,      &
                        addflag=.true., proxyflag=.true., &
                        rc=localrc)
                    if (ESMF_LogFoundError(localrc, &
                             msg="nested Field add to local state", &
                             ESMF_CONTEXT, rcToReturn=rc)) return
!!DEBUG "field added to state"

                   case (ESMF_ID_ARRAY%objectID)
!!DEBUG "need to create proxy array, remote id=", si%idrecv(k)
                    call c_ESMC_ArrayDeserialize(array, bptr, offset, &
                      attreconflag, localrc)
                    if (ESMF_LogFoundError(localrc, &
                             msg="nested Array deserialize", &
                             ESMF_CONTEXT, rcToReturn=rc)) return

                    ! Set init code
                    call ESMF_ArraySetInitCreated(array, rc=localrc)
                    if (ESMF_LogFoundError(localrc, &
                             msg="Array SetInit call", &
                             ESMF_CONTEXT, rcToReturn=rc)) return

!!DEBUG "created array, ready to set id and add to local state"
                    call c_ESMC_SetVMId(array, si%vmidrecv(k), localrc)
                    if (ESMF_LogFoundError(localrc, &
                             msg="nested Array SetVMId call", &
                             ESMF_CONTEXT, rcToReturn=rc)) return

                    call ESMF_StateAdd(state, array,      &
                        addflag=.true., proxyflag=.true., &
                        rc=localrc)
                    if (ESMF_LogFoundError(localrc, &
                             msg="nested Array add to local state", &
                             ESMF_CONTEXT, rcToReturn=rc)) return
!!DEBUG "array added to state"

                   case (ESMF_ID_ARRAYBUNDLE%objectID)
!!DEBUG "need to create proxy arraybundle, remote id=", si%idrecv(k)
                    call c_ESMC_ArrayBundleDeserialize(arraybundle, bptr, &
                      offset, attreconflag, localrc)
                    if (ESMF_LogFoundError(localrc, &
                             msg="nested arraybundle deserialize", &
                             ESMF_CONTEXT, rcToReturn=rc)) return

                    ! Set init code
                    call ESMF_ArrayBundleSetInitCreated(arraybundle, rc=localrc)
                    if (ESMF_LogFoundError(localrc, &
                             msg="arraybundle SetInit call", &
                             ESMF_CONTEXT, rcToReturn=rc)) return

!!DEBUG "created arraybundle, ready to set id and add to local state"
                    call c_ESMC_SetVMId(arraybundle, si%vmidrecv(k), localrc)
                    if (ESMF_LogFoundError(localrc, &
                             msg="nested arraybundle SetVMId call", &
                             ESMF_CONTEXT, rcToReturn=rc)) return

                    call ESMF_StateAdd(state, arraybundle, &
                        addflag=.true., proxyflag=.true.,  &
                        rc=localrc)
                    if (ESMF_LogFoundError(localrc, &
                             msg="nested arraybundle add to local state", &
                             ESMF_CONTEXT, rcToReturn=rc)) return
!!DEBUG "arraybundle added to state"

                   case (ESMF_ID_STATE%objectID)
!!DEBUG "need to create proxy substate, remote id=", si%idrecv(k)
                    substate = ESMF_StateDeserialize(vm, bptr, offset, &
                      attreconflag=attreconflag, rc=localrc)
                    if (ESMF_LogFoundError(localrc, &
                             msg="nested Substate deserialize", &
                             ESMF_CONTEXT, rcToReturn=rc)) return

!!DEBUG "created substate, ready to set id and add to local state"
                    call c_ESMC_SetVMId(substate%statep, si%vmidrecv(k), localrc)
                    if (ESMF_LogFoundError(localrc, &
                             msg="nested Substate SetVMId call", &
                             ESMF_CONTEXT, rcToReturn=rc)) return

                    call ESMF_StateAdd(state, substate,   &
                        addflag=.true., proxyflag=.true., &
                        rc=localrc)
                    if (ESMF_LogFoundError(localrc, &
                             msg="nested Substate add to local state", &
                             ESMF_CONTEXT, rcToReturn=rc)) return
!!DEBUG "substate added to state"
         
                  case (ESMF_STATEITEM_UNKNOWN%ot)
                    print *, "WARNING: unknown type"
                    call c_ESMC_StringDeserialize(thisname, &
                                                   bptr(1), offset, localrc)
                    if (ESMF_LogFoundError(localrc, &
                             msg="nested 'unknown' deserialize", &
                             ESMF_CONTEXT, rcToReturn=rc)) return

                     ! do nothing here

                   case default
                    print *, "WARNING: unexpected type"
                end select
             endif
           enddo itemloop

!!DEBUG "end of proxy create section"

           if (si%theircount > 0) then
!!DEBUG "the remote pet had sent us objects; remove temp space now"
               deallocate(si%idrecv, stat=memstat)
               if (ESMF_LogFoundDeAllocError(memstat, &
                              msg="Deallocating buffer for local ID list", &
                               ESMF_CONTEXT, rcToReturn=rc)) return
               call ESMF_VMIdDestroy(si%vmidrecv(1:si%theircount))
               deallocate(si%vmidrecv, stat=memstat)
               if (ESMF_LogFoundAllocError(memstat, &
                              msg="Deallocating buffer for local VM ID list", &
                               ESMF_CONTEXT, rcToReturn=rc)) return
               deallocate(si%objrecv, stat=memstat)
               if (ESMF_LogFoundAllocError(memstat, &
                              msg="Deallocating buffer for local obj list", &
                               ESMF_CONTEXT, rcToReturn=rc)) return
               deallocate(si%blindrecv, stat=memstat)
               if (ESMF_LogFoundAllocError(memstat, &
                              msg="Deallocating buffer for local buf list", &
                               ESMF_CONTEXT, rcToReturn=rc)) return
           endif
!!DEBUG "done deleting local space"
           
           ! TODO:
           ! and now, i have a different local object list.   brute force
           ! rebuild my send list.   Once this code is stable, revisit this
           ! and make a way to add the new object into my "known object"
           ! list without reserializing all objects.

!!DEBUG "drop old list and rebuild new one for our local objects"
           call ESMF_StateInfoDrop(stateInfoList, rc=localrc)
           if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return

           call ESMF_StateInfoBuild(state, stateInfoList, vm, attreconflag, localrc)
           if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return

!!DEBUG "reset pointer to state list"
           si => stateInfoList(1)

       endif   ! receiver

!!DEBUG "bottom of loop"
    enddo petloop ! source PET number

!!DEBUG "end of state proxy create"
    if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_StateProxyCreate


!------------------------------------------------------------------------------
#if 0
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateZapEmptyNests"
!BOPI
! !IROUTINE: ESMF_StateZapEmptyNests -- Zap empty nested States from State
!
! !INTERFACE:
    subroutine ESMF_StateZapEmptyNests(state, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state
      integer, intent(out), optional :: rc               

      integer :: localrc, i, iwrt
!     integer :: oldcount
      type (ESMF_StateClass), pointer :: stypep
      logical :: emptyNest
      type (ESMF_Logical) :: linkChange

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      stypep => state%statep
      
      iwrt = 1 ! initialize
      do i=1, stypep%datacount
        emptyNest = .false. ! reset
        if (stypep%datalist(i)%otype%ot == ESMF_STATEITEM_STATE%ot) then
          ! found nested State
          if (stypep%datalist(i)%datap%spp%datacount == 0) then
            ! nested State is empty
            emptyNest = .true.
            linkChange = ESMF_TRUE
            call c_ESMC_AttributeLinkRemove(stypep%base, stypep%datalist(i)%datap%spp%base, &
              linkChange, localrc)
           if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return
          endif
!print *, "gjt: zap empty nest in State"
        endif
        if (.not. emptyNest) then
          if (iwrt < i) then
            stypep%datalist(iwrt) = stypep%datalist(i)
          endif
          iwrt = iwrt + 1
        endif
      enddo
!oldcount = stypep%datacount
      stypep%datacount = iwrt-1
!print *, "gjt: reduced objects in State from", oldcount, " to ", stypep%datacount
      
      if (present(rc)) rc = ESMF_SUCCESS
    end subroutine ESMF_StateZapEmptyNests
#endif

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateZapProxies"
!BOPI
! !IROUTINE: ESMF_StateZapProxies -- Zap proxies from State
!
! !INTERFACE:
    subroutine ESMF_StateZapProxies(state, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout)         :: state
      integer,          intent(out), optional :: rc
!
! !DESCRIPTION:
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!       {\tt ESMF\_State} to clear proxies out of.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
      integer :: localrc, i
      type(ESMF_StateClass),    pointer :: stypep
      type(ESMF_StateItemWrap), pointer :: itemList(:)
      character(len=ESMF_MAXSTR) :: thisname

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      stypep => state%statep

      itemList => null ()
      call ESMF_ContainerGet(container=stypep%stateContainer, itemList=itemList, &
          rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

      if (associated(itemList)) then
        do i=1, size(itemList)
          if (itemList(i)%si%proxyFlag) then
            call ESMF_StateItemGet(itemList(i)%si, name=thisname, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            call ESMF_StateRemove (state, itemName=thisname, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          end if
        end do
!        deallocate(itemList, stat=memstat)
!        if (ESMF_LogFoundDeallocError(memstat, &
!            ESMF_ERR_PASSTHRU, &
!            ESMF_CONTEXT, rcToReturn=rc)) return
      endif

      stypep%zapList => itemList ! hang on for ESMF_ReconcileZappedProxies()

      if (present(rc)) rc = ESMF_SUCCESS
    end subroutine ESMF_StateZapProxies

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ReconcileZappedProxies"
!BOPI
! !IROUTINE: ESMF_ReconcileZappedProxies -- Conditionally restore zapped proxies
!
! !INTERFACE:
  subroutine ESMF_ReconcileZappedProxies(state, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout)         :: state
      integer,          intent(out), optional :: rc
!
! !DESCRIPTION:
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!       {\tt ESMF\_State}
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
    integer :: localrc, i, k
    integer :: memstat
    type(ESMF_StateClass),    pointer :: stypep
    type(ESMF_StateItemWrap), pointer :: itemList(:)
    type(ESMF_StateItemWrap), pointer :: zapList(:)
    character(len=ESMF_MAXSTR)  :: thisname
    character(len=ESMF_MAXSTR)  :: name
    type(ESMF_Field)            :: tempField

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    stypep => state%statep
    zapList => stypep%zapList
    
    itemList => null ()
    call ESMF_ContainerGet(container=stypep%stateContainer, itemList=itemList, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

!print *, "ESMF_ReconcileZappedProxies() looking"

    if (associated(itemList).and.associated(zapList)) then
      do i=1, size(itemList)
        if (itemList(i)%si%proxyFlag .and. &
          itemList(i)%si%otype==ESMF_STATEITEM_FIELD) then
          call ESMF_StateItemGet(itemList(i)%si, name=thisname, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          
          do k=1, size(zapList)
            if (zapList(k)%si%otype==ESMF_STATEITEM_FIELD) then
              call ESMF_FieldGet(zapList(k)%si%datap%fp, name=name, rc=localrc)
              if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) &
                return
!print *, "ESMF_ReconcileZappedProxies() checking: ", trim(name)
              if (trim(name) == trim(thisname)) then
!print *, "ESMF_ReconcileZappedProxies() found: ", trim(name)
                ! Bend pointers and copy contents to result in the desired
                ! behavior for re-reconcile. From a user perspective of
                ! Reconcile() proxies should persist when a State is 
                ! re-reconciled, and the same proxies are needed. Basically
                ! a user should be able to hang on to a proxy.
                tempField%ftypep => itemList(i)%si%datap%fp%ftypep
                zapList(k)%si%datap%fp%ftypep = itemList(i)%si%datap%fp%ftypep
                itemList(i)%si%datap%fp%ftypep => zapList(k)%si%datap%fp%ftypep
                zapList(k)%si%datap%fp%ftypep => tempField%ftypep
              endif
            endif
          enddo
        end if
      end do
      deallocate(itemList, stat=memstat)
      if (ESMF_LogFoundDeallocError(memstat, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_ReconcileZappedProxies

end module ESMF_StateReconcileMod


