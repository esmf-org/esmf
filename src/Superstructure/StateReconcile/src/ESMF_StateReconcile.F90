! $Id$
!
! Earth System Modeling Framework
! Copyright (c) 2002-2024, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================
!
#define ESMF_FILENAME "ESMF_StateReconcile.F90"
!
#define RECONCILE_LOG_on
#define RECONCILE_ZAP_LOG_off
!
! ESMF StateReconcile module
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

! TODO: Ensure buffer offsets are aligned between items until the various
! type-specific methods are totally reliable.  (Array in particular.)
#define ALIGN_FIX 1

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
  use ESMF_BaseMod
  use ESMF_InitMacrosMod
  use ESMF_IOUtilMod
  use ESMF_UtilMod
  use ESMF_LogErrMod
  use ESMF_StateMod
  use ESMF_StateContainerMod
  use ESMF_StateItemMod
  use ESMF_StateTypesMod
  use ESMF_VMMod
  use ESMF_UtilTypesMod

  use ESMF_ArrayMod
  use ESMF_ArrayBundleMod
  use ESMF_FieldMod
  use ESMF_FieldGetMod
  use ESMF_FieldCreateMod
  use ESMF_FieldBundleMod
  use ESMF_RHandleMod

  use ESMF_TraceMod

  use ESMF_InfoMod
  use ESMF_InfoCacheMod

  implicit none
  private

!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

  public :: ESMF_StateReconcile  ! make State consistent for concurrent apps

  ! These are only public for unit testing.  They are not intended
  ! to be called by ESMF users.
  ! public :: ESMF_ReconcileDeserialize, ESMF_ReconcileSerialize
  ! public :: ESMF_ReconcileSendItems
  public :: ESMF_ReconcileExchgAttributes

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
  '$Id$'
!==============================================================================

! !PRIVATE TYPES:
!------------------------------------------------------------------------------

! ! ESMF_NeedsBuffer
  type ESMF_NeedsBuffer
    logical, pointer :: needs(:) => null ()
  end type

! ! ESMF_ReconcileIDInfo
!
! ! ID/VMId pair, plus other global PET info

  type ESMF_ReconcileIDInfo
    integer,         pointer :: id(:) => null ()
    integer,         pointer :: vmid(:) => null ()
    logical,         pointer :: needed(:) => null ()
    character,       pointer :: item_buffer(:) => null ()
  end type

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================


!==============================================================================
!
! Misc
!
!==============================================================================

  logical, parameter :: trace=.false.
  logical, parameter :: debug=.false.

contains

!==============================================================================

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateReconcile"
!BOP
! !IROUTINE: ESMF_StateReconcile -- Reconcile State data across all PETs in a VM
!
! !INTERFACE:
  subroutine ESMF_StateReconcile(state, keywordEnforcer, vm, checkflag, rc)
!
! !ARGUMENTS:
    type(ESMF_State),            intent(inout)         :: state
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_VM),               intent(in),  optional :: vm
    logical,                     intent(in),  optional :: checkflag
    integer,                     intent(out), optional :: rc
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
!
!     This call is collective across the specified VM.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!       {\tt ESMF\_State} to reconcile.
!     \item[{[vm]}]
!       {\tt ESMF\_VM} across which to reconcile. The default is the
!       current VM.
!     \item [{[checkflag]}]
!       If set to {\tt .TRUE.} the reconciled State object will be checked
!       for consistency across PETs before returning. Any detected issues will
!       be indicated in {\tt rc}. Set {\tt checkflag} to {\tt .FALSE.} in order
!       to achieve highest performance. The default is {\tt .FALSE.}.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

    integer                     :: localrc
    type(ESMF_VM)               :: localvm
    logical                     :: isNoop, isFlag, localCheckFlag

    logical, parameter :: profile = .true.

    ! check input variables
    ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit,vm,rc)

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    localCheckFlag = .false.  ! default
    if (present(checkFlag)) localCheckFlag = checkFlag

!TODO: turn this .true. when working on StateReoncile, so all tests validate!
!localCheckFlag = .true. ! force checking

    if (present (vm)) then
      localvm = vm
    else
      call ESMF_VMGetCurrent(vm=localvm, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

#ifdef RECONCILE_LOG_on
    block
      character(ESMF_MAXSTR)  :: stateName
      call ESMF_StateGet(state, name=stateName, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
      call ESMF_LogWrite("StateReconcile() for State: "//trim(stateName), &
        ESMF_LOGMSG_DEBUG, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    end block
#endif

#if 0
    block
      type(ESMF_InfoDescribe)   :: idesc
      ! Log a JSON State representation -----------------------------------------
      call idesc%Initialize(createInfo=.true., addObjectInfo=.true., rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
      call idesc%Update(state, "", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_LogWrite("state_json_before_reconcile="//ESMF_InfoDump(idesc%info), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
      call idesc%Destroy(rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
    end block
#endif

#if 0
    ! cleaner timings below, eliminating issue due to different times PETs enter
    ! BUT: only enable this for testing purposes
    call ESMF_VMBarrier(localvm, rc=localrc)
#endif

    ! Determine whether there is anything to be Reconciled at all.
    ! If not then return as quickly as possible

    if (profile) then
      call ESMF_TraceRegionEnter("ESMF_StateReconcileIsNoop", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif

    call ESMF_StateReconcileIsNoop(state, vm=localvm, isNoop=isNoop, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    if (profile) then
      call ESMF_TraceRegionExit("ESMF_StateReconcileIsNoop", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif

#ifdef RECONCILE_LOG_on
    block
      character(160):: msgStr
      write(msgStr,*) "StateReconcile() isNoop: ", isNoop
      call ESMF_LogWrite(msgStr, ESMF_LOGMSG_DEBUG, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    end block
#endif

    if (isNoop) then
      ! successful early return because of NOOP condition
      if (present(rc)) rc = ESMF_SUCCESS
      return
    endif

    ! Each PET broadcasts the object ID lists and compares them to what
    ! they get back.   Missing objects are sent so they can be recreated
    ! on the PETs without those objects as "proxy" objects.  Eventually
    ! we might want to hash the ID lists so we can send a single number
    ! (or short list of numbers) instead of having to build and send the
    ! list each time.

    if (profile) then
      call ESMF_TraceRegionEnter("ESMF_StateReconcile_driver", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif

    call ESMF_StateReconcile_driver(state, vm=localvm, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    if (profile) then
      call ESMF_TraceRegionExit("ESMF_StateReconcile_driver", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif

    if (profile) then
      call ESMF_TraceRegionEnter("ESMF_InfoCacheReassembleFields", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif

    ! Traverse the State hierarchy and fix Field references to a shared geometry
    call ESMF_InfoCacheReassembleFields(state, state, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

    ! Traverse the state hierarchy and remove reconcile-specific attributes
    call ESMF_InfoCacheReassembleFieldsFinalize(state, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

    if (profile) then
      call ESMF_TraceRegionExit("ESMF_InfoCacheReassembleFields", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif

    if (localCheckFlag) then
      if (profile) then
        call ESMF_TraceRegionEnter("JSON cross PET check", rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
      endif
      block
        type(ESMF_InfoDescribe)   :: idesc
        character(:), allocatable :: jsonStr, testStr
        integer                   :: size(1), localPet
        ! Log a JSON State representation -----------------------------------------
        call idesc%Initialize(createInfo=.true., addObjectInfo=.true., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
        call idesc%Update(state, "", rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
        jsonStr = "state_json_after_reassemble="//ESMF_InfoDump(idesc%info)
#if 1
        call ESMF_LogWrite(jsonStr, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
#endif
        call idesc%Destroy(rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
#if 1
        ! check match across all PETs of VM
        size(1) = len(jsonStr)
        call ESMF_VMBroadcast(localvm, size, count=1, rootPet=0, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_VMGet(localvm, localPet=localPet, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
        if (localPet==0) then
          call ESMF_VMBroadcast(localvm, jsonStr, count=size(1), rootPet=0, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
        else
          allocate(character(len=size(1))::testStr)
          call ESMF_VMBroadcast(localvm, testStr, count=size(1), rootPet=0, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
          if (testStr/=jsonStr) then
            ! not a perfect match -> see if the differences are acceptable
            ! these are differences in the values of attributes, which show up in
            ! the bundled esmf and nuopc test cases... these diffs are begnin.
            isFlag = ESMF_UtilStringDiffMatch(jsonStr, testStr, &
              minusStringList = ["None        ", &
                                 "All         ", &
                                 "1           ", &
                                 "2           ", &
                                 "            ", &
                                 "            ", &
                                 "M           ", &
                                 "DEF         ", &
                                 "UL          ", &
                                 "            ", &
                                 "driverChild "  &
                                 ], &
              plusStringList  = ["All    ", &
                                 "None   ", &
                                 "2      ", &
                                 "1      ", &
                                 "DEF    ", &
                                 "UL     ", &
                                 "       ", &
                                 "       ", &
                                 "       ", &
                                 "M      ", &
                                 "DEFAULT"  &
                                 ], rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, &
              rcToReturn=rc)) return
            if (.not.isFlag) then
              ! found unexpected/unacceptable differences
              call ESMF_LogSetError(ESMF_RC_INTNRL_INCONS, &
                msg="StateReconcile() failed!! Not all PETs hold same content!!", &
                ESMF_CONTEXT, rcToReturn=rc)
              return
            endif
          endif
        endif
#endif
      end block
      if (profile) then
        call ESMF_TraceRegionExit("JSON cross PET check", rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
      endif
    endif

    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_StateReconcile

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateReconcileIsNoop"
!BOPI
! !IROUTINE: ESMF_StateReconcileIsNoop
!
! !INTERFACE:
    subroutine ESMF_StateReconcileIsNoop(state, vm, isNoop, rc)
!
! !ARGUMENTS:
      type (ESMF_State), intent(inout) :: state
      type (ESMF_VM),    intent(in)    :: vm
      logical,           intent(out)   :: isNoop
      integer,           intent(out)   :: rc
!
! !DESCRIPTION:
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!       {\tt ESMF\_State} to be reconciled.
!     \item[vm]
!       The current {\tt ESMF\_VM} (virtual machine).
!     \item[isNoop]
!       Return {\tt .true.} if no reconcile is needed, {\tt .false.} otherwise.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOPI
    integer                 :: localrc
    type(ESMF_VMId)         :: vmId
    logical                 :: isNoopLoc
    integer                 :: isNoopLocInt(1), isNoopInt(1)

    logical, parameter      :: profile = .true.

    localrc = ESMF_RC_NOT_IMPL

    isNoop = .false.  ! assume reconcile is needed

    call ESMF_VMGetVMId(vm, vmId=vmId, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, &
      rcToReturn=rc)) return

    if (profile) then
      call ESMF_TraceRegionEnter("StateReconcileIsNoopLoc", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif

    call StateReconcileIsNoopLoc(state, isNoopLoc=isNoopLoc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, &
      rcToReturn=rc)) return

    if (profile) then
      call ESMF_TraceRegionExit("StateReconcileIsNoopLoc", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif

    isNoopLocInt(1) = 0
    if (isNoopLoc) isNoopLocInt(1) = 1

    if (profile) then
      call ESMF_TraceRegionEnter("ESMF_VMAllReduce", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif

    ! logical AND reduction, only 1 if all incoming 1
    call ESMF_VMAllReduce(vm, isNoopLocInt, isNoopInt, 1, ESMF_REDUCE_MIN, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, &
      rcToReturn=rc)) return

    if (profile) then
      call ESMF_TraceRegionExit("ESMF_VMAllReduce", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif

    isNoop = (isNoopInt(1)==1)  ! globally consistent result

    ! return successfully
    rc = ESMF_SUCCESS

  contains

    recursive subroutine StateReconcileIsNoopLoc(stateR, isNoopLoc, rc)
      type(ESMF_State), intent(in)    :: stateR
      logical,          intent(out)   :: isNoopLoc
      integer,          intent(out)   :: rc
      ! - local variables
      integer                     :: localrc
      integer                     :: itemCount, item, fieldCount, arrayCount, i
      character(ESMF_MAXSTR), allocatable     :: itemNameList(:)
      type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
      type(ESMF_State)            :: nestedState
      type(ESMF_Field)            :: field
      type(ESMF_Field), allocatable :: fieldList(:)
      type(ESMF_FieldBundle)      :: fieldbundle
      type(ESMF_Array)            :: array
      type(ESMF_Array), allocatable :: arrayList(:)
      type(ESMF_ArrayBundle)      :: arraybundle
      type(ESMF_RouteHandle)      :: routehandle
      type(ESMF_VM)               :: vmItem
      type(ESMF_VMId)             :: vmIdItem
      type(ESMF_Pointer)          :: thisItem
      logical                     :: isFlag

      localrc = ESMF_RC_NOT_IMPL

      isNoopLoc = .true.

      ! query
      call ESMF_StateGet(stateR, itemCount=itemCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      if (itemCount > 0) then
        allocate(itemNameList(itemCount))
        allocate(itemTypeList(itemCount))
        call ESMF_StateGet(stateR, itemNameList=itemNameList, &
          itemtypeList=itemtypeList, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        do item=1, itemCount
          ! access the VM of the item, using appropriate API
          if ((itemtypeList(item) == ESMF_STATEITEM_STATE)) then
            call ESMF_StateGet(stateR, itemName=itemNameList(item), &
              nestedState=nestedState, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_StateGet(nestedState, vm=vmItem, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
            ! recursion into nested state
            call StateReconcileIsNoopLoc(stateR=nestedState, &
              isNoopLoc=isNoopLoc, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
          else if (itemtypeList(item) == ESMF_STATEITEM_FIELD) then
            call ESMF_StateGet(stateR, itemName=itemNameList(item), &
              field=field, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(field, vm=vmItem, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
          else if (itemtypeList(item) == ESMF_STATEITEM_FIELDBUNDLE) then
            call ESMF_StateGet(stateR, itemName=itemNameList(item), &
              fieldbundle=fieldbundle, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldBundleGet(fieldbundle, fieldCount=fieldCount, &
              isPacked=isFlag, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
            if (.not.isFlag) then
              ! not a packed fieldbundle -> check each field item
              allocate(fieldList(fieldCount))
              call ESMF_FieldBundleGet(fieldbundle, fieldList=fieldList, &
                rc=localrc)
              if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
              do i=1, fieldCount
                call ESMF_FieldGet(fieldList(i), vm=vmItem, rc=localrc)
                if (ESMF_LogFoundError(localrc, &
                  ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return
                call ESMF_VMGetVMId(vmItem, vmId=vmIdItem, rc=localrc)
                if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, &
                  rcToReturn=rc)) return
                isNoopLoc = ESMF_VMIdCompare(vmIdItem, vmId, keyOnly=.true., &
                  rc=localrc)
                if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, &
                  rcToReturn=rc)) return
                if (.not.isNoopLoc) exit  ! exit for .false.
              enddo
              deallocate(fieldList)
            endif
            call ESMF_FieldBundleGet(fieldbundle, vm=vmItem, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
          else if (itemtypeList(item) == ESMF_STATEITEM_ARRAY) then
            call ESMF_StateGet(stateR, itemName=itemNameList(item), &
              array=array, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_ArrayGet(array, vm=vmItem, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
          else if (itemtypeList(item) == ESMF_STATEITEM_ARRAYBUNDLE) then
            call ESMF_StateGet(stateR, itemName=itemNameList(item), &
              arraybundle=arraybundle, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_ArrayBundleGet(arraybundle, arrayCount=arrayCount, &
              rc=localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
            allocate(arrayList(arrayCount))
            call ESMF_ArrayBundleGet(arraybundle, arrayList=arrayList, &
              rc=localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
            do i=1, arrayCount
              call ESMF_ArrayGet(arrayList(i), vm=vmItem, rc=localrc)
              if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
              call ESMF_VMGetVMId(vmItem, vmId=vmIdItem, rc=localrc)
              if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, &
                rcToReturn=rc)) return
              isNoopLoc = ESMF_VMIdCompare(vmIdItem, vmId, keyOnly=.true., &
                rc=localrc)
              if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, &
                rcToReturn=rc)) return
              if (.not.isNoopLoc) exit  ! exit for .false.
            enddo
            deallocate(arrayList)
            call ESMF_ArrayBundleGet(arraybundle, vm=vmItem, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
          else if (itemtypeList(item) == ESMF_STATEITEM_ROUTEHANDLE) then
            call ESMF_StateGet(stateR, itemName=itemNameList(item), &
              routehandle=routehandle, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_RouteHandleGet(routehandle, vm=vmItem, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
          endif

!call ESMF_LogWrite("processing "//trim(itemNameList(item)), ESMF_LOGMSG_DEBUG, rc=localrc)

          call ESMF_VMGetThis(vmItem, thisItem, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, &
            rcToReturn=rc)) return

          if (thisItem == ESMF_NULL_POINTER) isNoopLoc = .false.  ! found proxy

          ! exit for .false. already from proxy, recursive state, or bundles
          if (.not.isNoopLoc) exit

          call ESMF_VMGetVMId(vmItem, vmId=vmIdItem, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, &
            rcToReturn=rc)) return

          isNoopLoc = ESMF_VMIdCompare(vmIdItem, vmId, keyOnly=.true., &
            rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, &
            rcToReturn=rc)) return
#if 0
block
  character(160) :: msgStr
  call ESMF_VMIdLog(vmIdItem, prefix="vmIdItem: ", rc=localrc)
  write(msgStr,*) "isNoopLoc: ", isNoopLoc
  call ESMF_LogWrite(msgStr, ESMF_LOGMSG_DEBUG, rc=localrc)
end block
#endif
          if (.not.isNoopLoc) exit  ! exit for .false.

          vmId = vmIdItem  ! more likely to hit pointer comparison this way

        enddo

        deallocate(itemNameList)
        deallocate(itemTypeList)
      endif

    end subroutine StateReconcileIsNoopLoc

  end subroutine ESMF_StateReconcileIsNoop

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateReconcile_driver"
!BOPI
! !IROUTINE: ESMF_StateReconcile_driver
!
! !INTERFACE:
    subroutine ESMF_StateReconcile_driver(state, vm, rc)
!
! !ARGUMENTS:
      type (ESMF_State), intent(inout) :: state
      type (ESMF_VM),    intent(in)    :: vm
      integer,           intent(out)   :: rc
!
! !DESCRIPTION:
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!       {\tt ESMF\_State} to collect information from.
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
!EOPI
    integer :: localrc
    integer :: memstat
    integer :: localPet, petCount

    integer, pointer :: nitems_buf(:)
    type (ESMF_StateItemWrap), pointer :: siwrap(:)

    integer,         pointer :: ids_send(:)
    type(ESMF_VMId), pointer :: vmids_send(:)
    integer, allocatable, target :: vmintids_send(:)

    type(ESMF_ReconcileIDInfo), allocatable :: id_info(:)

    logical, pointer :: recvd_needs_matrix(:,:)

    type(ESMF_CharPtr), allocatable :: items_recv(:)
    character, pointer :: buffer_recv(:)

    integer :: i

    logical, parameter :: debug = .false.
    logical, parameter :: meminfo = .false.
    logical, parameter :: trace = .false.
    logical, parameter :: profile = .true.

    type(ESMF_VMId), allocatable, target :: vmIdMap(:)
    type(ESMF_VMId), pointer             :: vmIdMap_ptr(:)
    type(ESMF_VMId), pointer             :: vmIdSingleComp
    logical                              :: singleCompCaseFlag
    integer                              :: singleCompCaseFlagInt(1)
    integer                              :: singleCompCaseInt(1)
    integer                              :: singleCompIndex

    type(ESMF_AttReconcileFlag)          :: attreconflag

    type(ESMF_InfoCache) :: info_cache

    ! -------------------------------------------------------------------------
    localrc = ESMF_RC_NOT_IMPL

    ! Attributes must be reconciled to de-duplicate Field geometry proxies
    attreconflag = ESMF_ATTRECONCILE_ON

    if (meminfo) call ESMF_VMLogMemInfo("entering ESMF_StateReconcile_driver")

    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    if (debug) then
      call ESMF_StateLog(state, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

    ! -------------------------------------------------------------------------
    ! (0) Interchange item counts between PETs.  Set up counts/displacements
    ! -------------------------------------------------------------------------
    if (profile) then
      call ESMF_TraceRegionEnter("(0) Interchange item counts", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    ! -------------------------------------------------------------------------
    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ': *** Step 0 - Initialize item counts and siwrappers')
    end if
    siwrap     => null ()
    nitems_buf => null ()
    call ESMF_ReconcileInitialize (state, vm,  &
        siwrap=siwrap, nitems_all=nitems_buf, rc=localrc)
    if (debug)  &
        localrc = ESMF_ReconcileAllRC (vm, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    ! -------------------------------------------------------------------------
    if (profile) then
      call ESMF_TraceRegionExit("(0) Interchange item counts", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    ! -------------------------------------------------------------------------
    if (meminfo) call ESMF_VMLogMemInfo ("after (0) Interchange item counts")

    ! -------------------------------------------------------------------------
    ! (1) Each PET constructs its send arrays containing local Id
    ! and VMId info for each object contained in the State.
    ! Note that element zero is reserved for the State itself.
    ! -------------------------------------------------------------------------
    if (profile) then
      call ESMF_TraceRegionEnter("(1) Construct send arrays", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    ! -------------------------------------------------------------------------
    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ': *** Step 1.0 - Build send arrays')
    end if
    ids_send   => null ()
    vmids_send => null ()
    if (profile) then
      call ESMF_TraceRegionEnter("ESMF_ReconcileGetStateIDInfo", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    call ESMF_ReconcileGetStateIDInfo (state, siwrap,  &
          id=  ids_send,  &
        vmid=vmids_send,  &
        rc=localrc)
    if (debug)  &
        localrc = ESMF_ReconcileAllRC (vm, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    if (profile) then
      call ESMF_TraceRegionExit("ESMF_ReconcileGetStateIDInfo", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif

    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ': *** Step 1.1 - Translate VM identifiers to integers')
    end if

    ! Translate VmId objects to an integer representation to minimize memory
    ! usage. This is also beneficial for performance.
    if (profile) then
      call ESMF_TraceRegionEnter("ESMF_VMTranslateVMId", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    call ESMF_VMTranslateVMId(vm, vmIds=vmids_send, ids=vmintids_send, &
      vmIdMap=vmIdMap, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT,  &
      rcToReturn=rc)) return
    vmIdMap_ptr => vmIdMap
    if (profile) then
      call ESMF_TraceRegionExit("ESMF_VMTranslateVMId", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif

    ! VM integer ids should always start with 1
    if (profile) then
      call ESMF_TraceRegionEnter("Check vmIntIds", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    if (any(vmintids_send(:) <= 0)) then
      call ESMF_LogSetError(ESMF_RC_INTNRL_INCONS, &
        msg="All integer VM ids must be greater than 0!", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif
    if (profile) then
      call ESMF_TraceRegionExit("Check vmIntIds", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif

    ! Use the translated VM ids information to make decision about the case
    if (profile) then
      call ESMF_TraceRegionEnter("Decide between cases", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    ! Decide between SingleComp and MultiComp case
    singleCompCaseFlag = .false.
    nullify(vmIdSingleComp)
    if (size(vmIdMap)==1) then
      singleCompCaseFlag = all(vmintids_send(1:)==1)
      if (singleCompCaseFlag) vmIdSingleComp => vmIdMap(1)
    else if (size(vmIdMap)==2) then
      singleCompCaseFlag = all(vmintids_send(1:)==1) &
        .or.all(vmintids_send(1:)==2)
      if (singleCompCaseFlag) then
        ! singleCompIndex could be 1 or 2, however, cannot simply look this up
        ! in vmintids_send(1), because on PETs that do not have objects it only
        ! stores vmintids_send(0), which holds the index into vmIdMap of the
        ! executing VM. Since there are only two possible values, the correct
        ! singleCompIndex must be "the other one". Therefore, look at
        ! vmintids_send(0), which is valid on all PETs, add 1 mod 2. But since
        ! indexing into vmIdMap starts at 1 the add 1 happens _after_ the mod 2.
        singleCompIndex = mod(vmintids_send(0),2)+1
        vmIdSingleComp => vmIdMap(singleCompIndex)
      endif
    endif

#ifdef RECONCILE_LOG_on
    block
      character(160):: msgStr
      write(msgStr,*) "ESMF_StateReconcile_driver() size(vmids_send): ", &
        size(vmids_send)
      call ESMF_LogWrite(msgStr, ESMF_LOGMSG_DEBUG, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
      write(msgStr,*) "ESMF_StateReconcile_driver() size(vmIdMap): ", &
        size(vmIdMap)
      call ESMF_LogWrite(msgStr, ESMF_LOGMSG_DEBUG, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
      write(msgStr,*) "ESMF_StateReconcile_driver() size(vmintids_send): ", &
        size(vmintids_send)
      call ESMF_LogWrite(msgStr, ESMF_LOGMSG_DEBUG, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
      write(msgStr,*) "ESMF_StateReconcile_driver() local-singleCompCaseFlag: ", &
        singleCompCaseFlag
      call ESMF_LogWrite(msgStr, ESMF_LOGMSG_DEBUG, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    end block
#endif

    ! ensure global consistency of the final result
    singleCompCaseFlagInt(1) = 0
    if (singleCompCaseFlag) singleCompCaseFlagInt(1) = 1
    ! logical AND reduction, only 1 if all incoming 1
    call ESMF_VMAllReduce(vm, singleCompCaseFlagInt, singleCompCaseInt, 1, &
      ESMF_REDUCE_MIN, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, &
      rcToReturn=rc)) return
    singleCompCaseFlag = (singleCompCaseInt(1)==1)  ! globally consistent result

#ifdef RECONCILE_LOG_on
    block
      character(160):: msgStr
      write(msgStr,*) "ESMF_StateReconcile_driver() global-singleCompCaseFlag: ", &
        singleCompCaseFlag
      call ESMF_LogWrite(msgStr, ESMF_LOGMSG_DEBUG, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    end block
#endif

    if (profile) then
      call ESMF_TraceRegionExit("Decide between cases", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif

    ! -------------------------------------------------------------------------
    if (profile) then
      call ESMF_TraceRegionExit("(1) Construct send arrays", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    ! -------------------------------------------------------------------------
    if (meminfo) call ESMF_VMLogMemInfo ("after (1) Construct send arrays")

#if 0
    block
      type(ESMF_InfoDescribe)   :: idesc
      ! Log a JSON State representation -----------------------------------------
      call idesc%Initialize(createInfo=.true., addObjectInfo=.true., &
        vmIdMap=vmIdMap_ptr, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
      call idesc%Update(state, "", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_LogWrite("state_json_after_vmid="//ESMF_InfoDump(idesc%info), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
      call idesc%Destroy(rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
    end block
#endif

    ! -------------------------------------------------------------------------
    if (profile) then
      call ESMF_TraceRegionEnter("(2) Update Field metadata", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    ! -------------------------------------------------------------------------
    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ': *** Step 2.0 - Update Field metadata for unique geometries')
    end if

    ! Update Field metadata for unique geometries. This will traverse the state
    ! hierarchy adding reconcile-specific attributes that will find unique
    ! geometry objects and maintain sufficient information to re-establish
    ! references once the objects have been communicated and deserialized.
    ! -------------------------------------------------------------------------
    if (profile) then
      call ESMF_TraceRegionEnter("info_cache for unique geometries", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif

    call info_cache%Initialize(localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

    call info_cache%UpdateFields(state, vmIdMap=vmIdMap_ptr, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

    call info_cache%Destroy(localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

    if (profile) then
      call ESMF_TraceRegionExit("info_cache for unique geometries", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif

    ! -------------------------------------------------------------------------
    if (profile) then
      call ESMF_TraceRegionExit("(2) Update Field metadata", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    ! -------------------------------------------------------------------------
    if (meminfo) call ESMF_VMLogMemInfo ("after (2) Update Field metadata")

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!TODO: Remove this once done with testing!
!singleCompCaseFlag = .false.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (singleCompCaseFlag) then
      ! CASE: a single component interacting with a state
      ! ------------------------------------------------------------------------
      if (profile) then
        call ESMF_TraceRegionEnter("(2<) ESMF_ReconcileSingleCompCase", rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
      endif
      ! ------------------------------------------------------------------------
      call ESMF_ReconcileSingleCompCase(state, vm=vm, vmId=vmIdSingleComp, &
        attreconflag=attreconflag, siwrap=siwrap, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
      ! ------------------------------------------------------------------------
      if (profile) then
        call ESMF_TraceRegionExit("(2<) ESMF_ReconcileSingleCompCase", rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
      endif
      ! ------------------------------------------------------------------------
    else
      ! CASE: multiple components interacting with a state
      ! ------------------------------------------------------------------------
      if (profile) then
        call ESMF_TraceRegionEnter("(2<) ESMF_ReconcileMultiCompCase", rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
      endif
      ! ------------------------------------------------------------------------
      call ESMF_ReconcileMultiCompCase()
      ! ------------------------------------------------------------------------
      if (profile) then
        call ESMF_TraceRegionExit("(2<) ESMF_ReconcileMultiCompCase", rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
      endif
      ! ------------------------------------------------------------------------
    endif

    ! Clean up

    ! -------------------------------------------------------------------------
    if (profile) then
      call ESMF_TraceRegionEnter("(X) Clean-up", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    ! -------------------------------------------------------------------------
    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ': *** Step X.0 - Clean-up')
    end if

    if (associated (ids_send)) then
      deallocate (ids_send, vmids_send, stat=memstat)
      if (ESMF_LogFoundDeallocError(memstat, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

    call ESMF_VMIdDestroy(vmIdMap, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

    deallocate (vmIdMap, stat=memstat)
    if (ESMF_LogFoundDeallocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    if (associated (siwrap)) then
      deallocate (siwrap, stat=memstat)
      if (ESMF_LogFoundDeallocError(memstat, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

    deallocate (nitems_buf, stat=memstat)
    if (ESMF_LogFoundDeallocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    ! -------------------------------------------------------------------------
    if (profile) then
      call ESMF_TraceRegionExit("(X) Clean-up", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    ! -------------------------------------------------------------------------
    if (meminfo) call ESMF_VMLogMemInfo ("after (X) Clean-up")

    ! -------------------------------------------------------------------------
    if (profile) then
      call ESMF_TraceRegionEnter("(X+1) Reconcile Zapped Proxies", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    ! -------------------------------------------------------------------------
    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ': *** Step X+1.0 - Reconcile Zapped Proxies')
    end if
    call ESMF_ReconcileZappedProxies(state, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    ! -------------------------------------------------------------------------
    if (profile) then
      call ESMF_TraceRegionExit("(X+1) Reconcile Zapped Proxies", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    ! -------------------------------------------------------------------------
    if (meminfo) call ESMF_VMLogMemInfo ("(X+1) Reconcile Zapped Proxies")

    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD // ': Complete!')
      call ESMF_VMBarrier (vm)
    end if
    rc = ESMF_SUCCESS

    if (meminfo) call ESMF_VMLogMemInfo ("exiting ESMF_StateReconcile_driver")

  contains

  subroutine ESMF_ReconcileMultiCompCase()

    ! -------------------------------------------------------------------------
    ! (3) All PETs send their items Ids and VMIds to all the other PETs,
    ! then create local directories of which PETs have which ids/VMIds.
    ! -------------------------------------------------------------------------
    if (profile) then
      call ESMF_TraceRegionEnter("(3) Send arrays exchange", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    ! -------------------------------------------------------------------------
    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ': *** Step 3 - Exchange Ids/VMIds')
    end if
    allocate (id_info(0:petCount-1), stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    call ESMF_ReconcileExchgIDInfo (vm,  &
        nitems_buf=nitems_buf,  &
        id=ids_send,  &
        vmid=vmintids_send,  &
        id_info=id_info, &
        rc=localrc)
    if (debug)  &
        localrc = ESMF_ReconcileAllRC (vm, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    ! -------------------------------------------------------------------------
    if (profile) then
      call ESMF_TraceRegionExit("(3) Send arrays exchange", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    ! -------------------------------------------------------------------------
    if (meminfo) call ESMF_VMLogMemInfo ("after (3) Send arrays exchange")

! At this point, each PET knows what items can be found on all of
! the other PETs.  The id_info array has global PET info in it.

    ! -------------------------------------------------------------------------
    ! (4) Construct needs list.  Receiving PETs compare IDs and VMIds
    ! in their send ID/VMId array with what was received from the
    ! currently-being-processed sending PET.  Note that multiple PETs
    ! can 'offer' an item.
    ! -------------------------------------------------------------------------
    if (profile) then
      call ESMF_TraceRegionEnter("(4) Construct needs list", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    ! -------------------------------------------------------------------------
    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ': *** Step 4 - Compare and create needs arrays')
    end if
    call ESMF_ReconcileCompareNeeds (vm,  &
          id=  ids_send,  &
        vmid=vmintids_send,  &
        id_info=id_info,  &
        rc=localrc)
    if (debug)  &
        localrc = ESMF_ReconcileAllRC (vm, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    ! -------------------------------------------------------------------------
    if (profile) then
      call ESMF_TraceRegionExit("(4) Construct needs list", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    ! -------------------------------------------------------------------------
    if (meminfo) call ESMF_VMLogMemInfo ("after (4) Construct needs list")

    ! -------------------------------------------------------------------------
    ! (5) Communicate needs back to the offering PETs.
    ! Send to each offering PET a buffer containing 'needed' array
    ! specifying which items are needed.  The array is the same size as,
    ! and corresponds to, the ID and VMId arrays that were previously
    ! offered.
    ! -------------------------------------------------------------------------
    if (profile) then
      call ESMF_TraceRegionEnter("(5) Communicate needs back", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    ! -------------------------------------------------------------------------
    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ': *** Step 5 - Exchange needs')
    end if
    recvd_needs_matrix => null ()
    call ESMF_ReconcileExchgNeeds (vm,  &
        id_info=id_info,  &
        recv_needs=recvd_needs_matrix,  &
        rc=localrc)
    if (debug)  &
        localrc = ESMF_ReconcileAllRC (vm, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    ! -------------------------------------------------------------------------
    if (profile) then
      call ESMF_TraceRegionExit("(5) Communicate needs back", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    ! -------------------------------------------------------------------------
    if (meminfo) call ESMF_VMLogMemInfo ("after (5) Communicate needs back")

    ! -------------------------------------------------------------------------
    ! (6) Serialize needed objects
    ! -------------------------------------------------------------------------
    if (profile) then
      call ESMF_TraceRegionEnter("(6) Serialize needed objects", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    ! -------------------------------------------------------------------------
    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ': *** Step 6 - Serialize needs', ask=.false.)
    end if
    call ESMF_ReconcileSerialize (state, vm, siwrap, &
        needs_list=recvd_needs_matrix, &
        attreconflag=attreconflag,  &
        id_info=id_info,  &
        rc=localrc)
    if (debug)  &
        localrc = ESMF_ReconcileAllRC (vm, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    deallocate (recvd_needs_matrix, stat=memstat)
    if (ESMF_LogFoundDeallocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    ! -------------------------------------------------------------------------
    if (profile) then
      call ESMF_TraceRegionExit("(6) Serialize needed objects", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    ! -------------------------------------------------------------------------
    if (meminfo) call ESMF_VMLogMemInfo ("after (6) Serialize needed objects")

    ! -------------------------------------------------------------------------
    ! (7) Send/receive serialized objects to whoever needed them
    ! -------------------------------------------------------------------------
    if (profile) then
      call ESMF_TraceRegionEnter("(7) Send/receive serialized objects", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    ! -------------------------------------------------------------------------
    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ': *** Step 7 - Exchange serialized objects')
    end if
    allocate (items_recv(0:petCount-1), stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    buffer_recv => null ()
    call ESMF_ReconcileExchgItems (vm,  &
        id_info=id_info,  &
        recv_items=items_recv,  &  ! %cptr aliased to portions of buffer_recv
        recv_buffer=buffer_recv,  &
        rc=localrc)
    if (debug)  &
        localrc = ESMF_ReconcileAllRC (vm, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    ! -------------------------------------------------------------------------
    if (profile) then
      call ESMF_TraceRegionExit("(7) Send/receive serialized objects", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    ! -------------------------------------------------------------------------
    if (meminfo) call ESMF_VMLogMemInfo ("after (7) Send/receive serialized objects")

    ! -------------------------------------------------------------------------
    ! (8) Deserialize received objects and create proxies (recurse on
    !     nested States as needed)
    ! -------------------------------------------------------------------------
    if (profile) then
      call ESMF_TraceRegionEnter("(8) Deserialize received objects and create proxies", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    ! -------------------------------------------------------------------------
    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ': *** Step 8 - Deserialize needs')
    end if
    do, i=0, petCount-1
      if (debug) then
        write (*, '(a,i0,a,i0,a,l1)')  &
            '   PET ', localPet, ': Deserializing from PET ', i,  &
            ', associated (items_recv(i)%cptr) =', associated (items_recv(i)%cptr)
      end if
      if (associated (items_recv(i)%cptr)) then
        if (debug) then
          print *, '    items_recv(', lbound (items_recv(i)%cptr),  &
              ':', ubound (items_recv(i)%cptr), ')'
            end if
        call ESMF_ReconcileDeserialize (state, vm, &
            obj_buffer=items_recv(i)%cptr, &
            attreconflag=attreconflag, &
            rc=localrc)
      else
        localrc = ESMF_SUCCESS
      end if
      if (debug)  &
          localrc = ESMF_ReconcileAllRC (vm, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end do
    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ': *** Step 8 - Complete')
    end if

    ! -------------------------------------------------------------------------
    if (profile) then
      call ESMF_TraceRegionExit("(8) Deserialize received objects and create proxies", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    ! -------------------------------------------------------------------------
    if (meminfo) call ESMF_VMLogMemInfo ("after (8) Deserialize received objects and create proxies")

    ! Clean up

    if (associated (buffer_recv)) then
      deallocate (buffer_recv, stat=memstat)
      if (ESMF_LogFoundDeallocError (memstat, ESMF_ERR_PASSTHRU,  &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

    do, i=0, ubound (id_info, 1)
      if (associated (id_info(i)%id)) then
        deallocate (id_info(i)%id, id_info(i)%vmid, id_info(i)%needed,  &
            stat=memstat)
        if (ESMF_LogFoundDeallocError(memstat, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return
      end if
      if (associated (id_info(i)%item_buffer)) then
        deallocate (id_info(i)%item_buffer,  &
            stat=memstat)
        if (ESMF_LogFoundDeallocError(memstat, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return
      end if
    end do
    deallocate (id_info, stat=memstat)
    if (ESMF_LogFoundDeallocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

  end subroutine ESMF_ReconcileMultiCompCase

  end subroutine ESMF_StateReconcile_driver

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ReconcileSingleCompCase"
!BOPI
! !IROUTINE: ESMF_ReconcileSingleCompCase
!
! !INTERFACE:
  subroutine ESMF_ReconcileSingleCompCase(state, vm, vmId, attreconflag, siwrap, rc)
!
! !ARGUMENTS:
    type(ESMF_State),                     intent(inout)  :: state
    type(ESMF_VM),                        intent(in)     :: vm
    type(ESMF_VMId),             pointer, intent(in)     :: vmId
    type(ESMF_AttReconcileFlag),          intent(in)     :: attreconflag
    type(ESMF_StateItemWrap),    pointer, intent(in)     :: siwrap(:)
    integer,                              intent(out)    :: rc
!
! !DESCRIPTION:
!
!   Handle the single component reconciliation case. This is the expected
!   situation under NUOPC rules.
!
!   The arguments are:
!   \begin{description}
!   \item[state]
!     The {\tt ESMF\_State} to reconcile.
!   \item[vm]
!     The {\tt ESMF\_VM} object across which the state is reconciled.
!   \item[vmId]
!     The {\tt ESMF\_VMId} of the single component who ownes all objects present
!     in the state.
!   \item[attreconflag]
!     Flag indicating whether attributes need to be reconciled.
!   \item[siwrap]
!     List of local state items.
!   \item[rc]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!EOPI

    integer :: localrc
    integer :: petCount, localPet, rootVas, rootPet, vas
    integer :: sizeBuffer(1)
    logical :: isFlag
    character, pointer :: buffer(:)

    rc = ESMF_SUCCESS

    call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT,  &
      rcToReturn=rc)) return

    call ESMF_VMIdGet(vmId, leftMostOnBit=rootVas, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT,  &
      rcToReturn=rc)) return

    ! search for PET in VM that executes on rootVas
    do rootPet=0, petCount-1
      call ESMF_VMGet(vm, pet=rootPet, vas=vas, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
      if (vas==rootVas) exit  ! found
    enddo
    if (rootPet==petCount) then
      call ESMF_LogSetError(ESMF_RC_INTNRL_INCONS, &
        msg="Could not find PET that executes on the identified VAS", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

#ifdef RECONCILE_LOG_on
    block
      character(160)  :: msgStr
      write(msgStr,*) "SingleCompCase rootVas=", rootVas
      call ESMF_LogWrite(msgStr, ESMF_LOGMSG_DEBUG, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
      write(msgStr,*) "SingleCompCase rootPet=", rootPet
      call ESMF_LogWrite(msgStr, ESMF_LOGMSG_DEBUG, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    end block
#endif

    ! Serialize on rootPet
    if (localPet==rootPet) then
      call ESMF_ReconcileSerializeAll(state, vm, siwrap, attreconflag, &
        buffer, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      sizeBuffer(1) = size(buffer)
    endif

    ! Broadcast buffer across all PETs
    call ESMF_VMBroadcast(vm, sizeBuffer, count=1, rootPet=rootPet, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (localPet/=rootPet) allocate(buffer(sizeBuffer(1)))

    call ESMF_VMBroadcast(vm, buffer, count=sizeBuffer(1), rootPet=rootPet, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! determine if local PET is active under the vmId
    call ESMF_VMIdGet(vmId, isLocalPetActive=isFlag, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

#ifdef RECONCILE_LOG_on
    block
      character(160)  :: msgStr
      write(msgStr,*) "SingleCompCase PET active isFlag=", isFlag
      call ESMF_LogWrite(msgStr, ESMF_LOGMSG_DEBUG, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    end block
#endif

    ! only inactive PETs deserialize the buffer received from rootPet
    if (.not.isFlag) then
      call ESMF_ReconcileDeserializeAll(state, vm, buffer, attreconflag, &
        rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! Get rid of buffer
    deallocate(buffer)

  end subroutine ESMF_ReconcileSingleCompCase

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ReconcileCompareNeeds"
!BOPI
! !IROUTINE: ESMF_ReconcileCompareNeeds
!
! !INTERFACE:
  subroutine ESMF_ReconcileCompareNeeds (vm, id, vmid, id_info, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),     intent(in)   :: vm
    integer,           intent(in)   :: id(0:)
    integer,           intent(in)   :: vmid(0:)
    type(ESMF_ReconcileIDInfo), intent(inout) :: id_info(0:)
    integer,           intent(out)  :: rc
!
! !DESCRIPTION:
!
!  Calculates which PETs have items that this PET needs.  When a given item is
!  offered by multiple PETs, a heuristic is used to determine which PET will
!  provide it in order to try to avoid 'hot spotting' the offering PET.
!
!   The arguments are:
!   \begin{description}
!   \item[vm]
!     The current {\tt ESMF\_VM} (virtual machine).
!   \item[id]
!     The object ids of this PETs State itself (in element 0) and the items
!     contained within it.  It does not return the IDs of nested State
!     items.
!   \item[vmid]
!     The integer VMIds of this PETs State itself (in element 0) and the items
!     contained within it.  It does not return the IDs of nested State
!     items.
!   \item[id_info]
!     Array of arrays of global VMId info.  Upon input, the array has a size
!     of numPets, and each element points to Id/VMId arrays.  Returns 'needed'
!     flag for each desired object.
!   \item[rc]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!EOPI

    integer :: localrc
    integer :: memstat
    integer :: localPet, petCount
    integer :: i, j, k
    logical :: needed
    character(ESMF_MAXSTR) :: msgstring

    type NeedsList_t
      integer          :: id
      integer          :: vmid
      logical, pointer :: offerers(:) => null ()
      integer, pointer :: position(:) => null ()
      type(NeedsList_t), pointer :: next => null ()
    end type

    type(NeedsList_t), pointer :: needs_list

    logical, parameter :: debug = .false.

    ! Sanity checks

    call ESMF_VMGet (vm, localPet=localPet, petCount=petCount, rc=localrc)
    if (ESMF_LogFoundError (localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    if (size (id) /= size (vmid)) then
      if (ESMF_LogFoundError (ESMF_RC_INTNRL_INCONS, msg='size (id) /= size (vmid)', &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

    if (size (id_info) /= petCount) then
      if (ESMF_LogFoundError (ESMF_RC_INTNRL_INCONS, msg='size (id_info) /= petCount', &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

    if (debug) then
      print *, '  PET ', localPet, ': id/vmid sizes =', size (id), size (vmid)
    end if

! Check other PETs contents to see if there are objects this PET needs

! When 'needed' ID/VMId pairs are found, create a list of 'offering' PETs who can
! provide it.

    needs_list => null ()

! call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
!     ': computing id_info%needed')
    do, i=0, petCount-1
      id_info(i)%needed = .false.
      if (i == localPet) cycle

      do, j = 1, ubound (id_info(i)%id, 1)
        needed = .true.
! print *, '  PET', localPet, ': setting needed to .true.', j, k
        do, k = 1, ubound (id, 1)
          if (id(k) == id_info(i)%id(j)) then
            if (vmid(k) == id_info(i)%vmid(j)) then
! print *, '  PET', localPet, ': setting needed to .false.', j, k
              needed = .false.
              exit
            end if
          end if
        end do

        if (needed) then
! print *, '  PET', localPet, ': calling insert, associated =', associated (needs_list)
          call needs_list_insert (needs_list, pet_1=i,  &
                id_1=id_info(i)%id(j),  &
              vmid_1=id_info(i)%vmid(j),  &
              position=j, rc_1=localrc)
          if (ESMF_LogFoundError (localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return
        end if
      end do
    end do

    if (debug) call needs_list_print (needs_list)

    ! Go through the list of needed IDs/VMIds and select an offerer for each.

    call needs_list_select (needs_list, id_info)

    call needs_list_deallocate (needs_list, rc_1=localrc)
    if (ESMF_LogFoundError (localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    if (associated (needs_list)) then
      deallocate (needs_list, stat=memstat)
      if (ESMF_LogFoundDeallocError (memstat, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if


    if (debug) then
      do, j=0, petCount-1
        if (j == localPet) then
          do, i=0, ubound (id_info, 1)
            write (msgstring,'(2a,i0,a,i0,a)') ESMF_METHOD,  &
                ': pet', j, ': id_info%needed(',i,') ='
            write (6,*) msgstring, id_info(i)%needed
            call ESMF_UtilIOUnitFlush (ESMF_UtilIOStdout)
          end do
        end if
        call ESMF_VMBarrier (vm)
      end do
    end if

    rc = localrc

  contains

    recursive subroutine needs_list_deallocate (needs_list_1, rc_1)
      type(NeedsList_t),  pointer :: needs_list_1  ! intent(inout)
      integer :: rc_1

      integer :: localrc_1
      integer :: memstat_1

      if (.not. associated (needs_list_1)) then
        rc_1 = ESMF_SUCCESS
        return
      end if

      deallocate (  &
          needs_list_1%offerers,  &
          needs_list_1%position,  &
          stat=memstat_1)
      if (ESMF_LogFoundDeallocError (memstat_1, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc_1)) return

      if (associated (needs_list_1%next)) then
! print *, 'pet', localPet, ': needs_list_deallocate: recursing'
        call needs_list_deallocate (needs_list_1%next, rc_1=localrc_1)
        if (ESMF_LogFoundError (localrc_1, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc_1)) return
        deallocate (needs_list_1%next,  &
            stat=memstat_1)
        if (ESMF_LogFoundDeallocError (memstat_1, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc_1)) return
      end if

      rc_1 = ESMF_SUCCESS

    end subroutine needs_list_deallocate

    subroutine needs_list_insert (needs_list_1, pet_1,  &
        id_1, vmid_1, position, rc_1)
      type(NeedsList_t),  pointer :: needs_list_1  ! intent(inout)
      integer,         intent(in) :: pet_1
      integer,         intent(in) :: id_1
      integer,         intent(in) :: vmid_1
      integer,         intent(in) :: position
      integer,         intent(out):: rc_1

      type(NeedsList_t), pointer :: needslist_p
      integer :: memstat_1

    ! Called when a Id/VMId is offered by some remote PET, and is needed
    ! by the local PET.
    !
    ! If the Id/VMId is not in the needs list, create a new needs_list
    ! entry.   If it is present, add that this PET is also offering it.

      rc_1 = ESMF_SUCCESS

      if (.not. associated (needs_list_1)) then
! print *, 'pet', localPet, ': needs_list_insert: creating needs_list_1'
        allocate (needs_list_1, stat=memstat_1)
        if (ESMF_LogFoundAllocError (memstat_1, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc_1)) return
        allocate (  &
            needs_list_1%offerers(0:petCount-1),  &
            needs_list_1%position(0:petCount-1),  &
            stat=memstat_1)
        if (ESMF_LogFoundAllocError (memstat_1, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc_1)) return
        needs_list_1%offerers = .false.
        needs_list_1%position = 0
        needs_list_1%id = id_1
        needs_list_1%vmid = vmid_1

        needs_list_1%offerers(pet_1) = .true.
        needs_list_1%position(pet_1) = position
        return
      end if

      needslist_p => needs_list_1
      do
        if (id_1 == needslist_p%id .and.  &
            vmid_1 == needslist_p%vmid) then
! print *, 'pet', localPet, ': needs_list_insert: marking match and returing'
          needslist_p%offerers(pet_1) = .true.
          needslist_p%position(pet_1) = position
          return
        end if

        if (.not. associated (needslist_p%next)) exit
! print *, 'pet', localPet, ': needs_list_insert: advancing to next entry'
        needslist_p => needslist_p%next
      end do

      ! At the end of the list, but no matches found.  So add new entry.

! print *, 'pet', localPet, ': needs_list_insert: creating needslist_p entry'
      allocate (needslist_p%next, stat=memstat_1)
      if (ESMF_LogFoundAllocError (memstat_1, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc_1)) return
      needslist_p => needslist_p%next
      allocate (  &
          needslist_p%offerers(0:petCount-1),  &
          needslist_p%position(0:petCount-1),  &
          stat=memstat_1)
      if (ESMF_LogFoundAllocError (memstat_1, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc_1)) return
      needslist_p%offerers = .false.
      needslist_p%position = 0
      needslist_p%id = id_1
      needslist_p%vmid = vmid_1

      needslist_p%offerers(pet_1) = .true.
      needslist_p%position(pet_1) = position

    end subroutine needs_list_insert

    subroutine needs_list_print (needs_list_1)
      type(NeedsList_t),  pointer :: needs_list_1  ! intent(in)

      type(NeedsList_t), pointer :: needs_list_next
      integer :: i

      call ESMF_UtilIOUnitFlush (ESMF_UtilIOStdout)
      call ESMF_VMBarrier (vm)
      do, i=0, petCount-1
        if (i == localPet) then
          if (associated (needs_list_1)) then
            needs_list_next => needs_list_1
            do
              print *, 'PET', localPet, ': offerers =', needs_list_next%offerers,  &
                  ', position =', needs_list_next%position
              if (.not. associated (needs_list_next%next)) exit
              needs_list_next => needs_list_next%next
            end do
          else
            print *, 'PET', localPet, ': Needs list empty'
          end if
          call ESMF_UtilIOUnitFlush (ESMF_UtilIOStdout)
        end if
        call ESMF_VMBarrier (vm)
      end do

    end subroutine needs_list_print

    subroutine needs_list_select (needs_list_1, id_info_1)
      type(needsList_t),          pointer       :: needs_list_1  ! intent(in)
      type(ESMF_ReconcileIDInfo), intent(inout) :: id_info_1(0:)

      ! For each needed Id/VMId pair, select an offering PET and set it in
      ! the id_info_array.

      type(needsList_t), pointer :: needslist_p
      integer :: i, idx
      integer :: offer_first, offer_last
      logical :: found_first
      real :: rand_nos(0:petCount-1)

      needslist_p => needs_list_1

#if 1
      ! Try to load distribute by starting at a point in the offerer list
      ! bounded by the first and last offering PETs, and using a hash based
      ! on PETs position in a pseudo-random number table.
      call random_number (rand_nos)

      do
        if (.not. associated (needslist_p)) exit
        ! Find first and last offering PETs
        offer_first = 0
        offer_last = petCount-1
        found_first = .false.
        do, i=0, petCount-1
          if (needslist_p%offerers(i)) then
            if (.not. found_first) then
              offer_first = i
              found_first = .true.
            end if
            offer_last = i
          end if
        end do

        ! Use a hash to select a starting index between the bounds
        idx = int (rand_nos(localPet) * (offer_last-offer_first) + offer_first)
! print *, 'pet', localPet, ': offer_first, offer_last, starting idx =', offer_first, offer_last, idx
        do, i=0, petCount-1
          if (needslist_p%offerers(idx)) then
! print *, 'pet', localPet, ': needs_list_select: setting position', idx, ' to true'
            id_info_1(idx)%needed(needslist_p%position(idx)) = .true.
            exit
          end if
          idx = mod (idx+1, petCount)
        end do
        needslist_p => needslist_p%next
      end do

#else
      ! Simply select the first offering PET.
      do
        if (.not. associated (needslist_p)) exit
        do, i=0, petCount-1
          if (needslist_p%offerers(i)) then
! print *, 'pet', localPet, ': needs_list_select: setting position', i, ' to true'
            id_info_1(i)%needed(needslist_p%position(i)) = .true.
            exit
          end if
        end do
        needslist_p => needslist_p%next
      end do
#endif

    end subroutine needs_list_select

  end subroutine ESMF_ReconcileCompareNeeds

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ReconcileDeserialize"
!BOPI
! !IROUTINE: ESMF_ReconcileDeserialize
!
! !INTERFACE:
  subroutine ESMF_ReconcileDeserialize (state, vm, obj_buffer, attreconflag, rc)
!
! !ARGUMENTS:
    type (ESMF_State), intent(inout):: state
    type (ESMF_VM),    intent(in)   :: vm
    character,         pointer      :: obj_buffer(:)    ! intent(in)
    type(ESMF_AttReconcileFlag),intent(in)   :: attreconflag
    integer,           intent(out)  :: rc
!
! !DESCRIPTION:
!   Builds proxy items for each of the items in the buffer.
!
!   The arguments are:
!   \begin{description}
!   \item[state]
!     {\tt ESMF\_State} to add proxy objects to.
!   \item[vm]
!     {\tt ESMF\_VM} to use.
!   \item[obj_buffer]
!     Buffer of serialized State objects (intent(in))
!   \item[attreconflag]
!     Flag to indicate attribute reconciliation.
!   \item[rc]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!EOPI

    integer :: localrc
    integer :: memstat

    type(ESMF_FieldBundle) :: fieldbundle
    type(ESMF_Field) :: field
    type(ESMF_Array) :: array
    type(ESMF_ArrayBundle) :: arraybundle
    type(ESMF_State) :: substate

    integer :: buffer_offset
    integer :: needs_count
    integer, allocatable :: offset_table(:), type_table(:)

    integer :: i, idx
    integer :: stateitem_type
    character(ESMF_MAXSTR) :: errstring

    integer :: localPet

    logical, parameter :: debug = .false.
    logical, parameter :: trace = .false.

    ! Sanity checks
    call ESMF_VMGet (vm, localPet=localPet, rc=localrc)
    if (ESMF_LogFoundError (localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    if (trace) then
      print *, '    pet', localPet,  &
          ': *** Step 0 - sanity checks'
    end if

    needs_count = transfer (  &
        source=obj_buffer(0:ESMF_SIZEOF_DEFINT-1),  &
        mold  =needs_count)
    if (debug) then
      print *, ESMF_METHOD, ': PET', localPet, ', needs_count =', needs_count
    end if

    ! -------------------------------------------------------------------------

    ! Deserialize offset and type tables
    if (debug) then
      print *, ESMF_METHOD, ': buffer offset/type table:'
    end if
    allocate (offset_table(needs_count), type_table(needs_count), stat=memstat)
    if (ESMF_LogFoundAllocError (memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    idx = 2 * ESMF_SIZEOF_DEFINT  ! Start after needs_count and pad
    do, i=1, needs_count
#if !defined (__G95__)
      offset_table(i) = transfer (  &
          obj_buffer(idx:idx+ESMF_SIZEOF_DEFINT-1),  &
          mold=needs_count)
      idx = idx + ESMF_SIZEOF_DEFINT
      type_table(i) = transfer (  &
          obj_buffer(idx:idx+ESMF_SIZEOF_DEFINT-1),  &
          mold=needs_count)
      idx = idx + ESMF_SIZEOF_DEFINT
#else
      ! g95 snapshots prior to April 4, 2010 have a bug in TRANSFER.
      ! The following works around it.
      offset_table(i) = ESMF_Reconcile_g95_getint (  &
          obj_buffer(idx:idx*ESMF_SIZEOF_DEFINT-1))
      idx = idx + ESMF_SIZEOF_DEFINT
      type_table(i) = ESMF_Reconcile_g95_getint (  &
          obj_buffer(idx:idx*ESMF_SIZEOF_DEFINT-1))
      idx = idx + ESMF_SIZEOF_DEFINT
#endif
      if (debug) then
        print *, '   ', i, ':', offset_table(i), type_table(i)
      end if
    end do

    ! Deserialize items
    if (trace) then
      print *, '    pet', localPet,  &
          ': *** Step 1 - main deserialization loop'
    end if
    buffer_offset = ESMF_SIZEOF_DEFINT * (2 + 2*needs_count) ! Skip past count, pad, and offset/type tables
    do, i=1, needs_count

      ! Item type
      stateitem_type = type_table(i)
      if (debug) then
        print *, ESMF_METHOD,  &
            ': stateitem_type =', stateitem_type, ', offset =', buffer_offset
      end if

      ! Item itself
      buffer_offset = offset_table(i)
      select case (stateitem_type)
        case (ESMF_STATEITEM_FIELDBUNDLE%ot)
          if (debug) then
            print *, "deserializing FieldBundle, offset =", buffer_offset
          end if
          fieldbundle = ESMF_FieldBundleDeserialize(obj_buffer, buffer_offset, &
              attreconflag=attreconflag, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          call ESMF_StateAdd(state, fieldbundle, &
              addflag=.true., proxyflag=.true.,  &
              rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

        case (ESMF_STATEITEM_FIELD%ot)
          if (debug) then
            print *, "deserializing Field, offset =", buffer_offset
          end if
          field = ESMF_FieldDeserialize(obj_buffer, buffer_offset, &
              attreconflag=attreconflag, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          if (debug) then
            print *, "created field, ready to add to local state"
          end if

          call ESMF_StateAdd(state, field,      &
              addflag=.true., proxyflag=.true., &
              rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

        case (ESMF_STATEITEM_ARRAY%ot)
          if (debug) then
            print *, "    PET", localPet,  &
                ": deserializing Array, offset =", buffer_offset
          end if
          call c_ESMC_ArrayDeserialize(array, obj_buffer, buffer_offset, &
              attreconflag, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          ! Set init code
          call ESMF_ArraySetInitCreated(array, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          call ESMF_StateAdd(state, array,      &
              addflag=.true., proxyflag=.true., &
              rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

        case (ESMF_STATEITEM_ARRAYBUNDLE%ot)
          if (debug) then
            print *, "deserializing ArrayBundle, offset =", buffer_offset
          end if
          call c_ESMC_ArrayBundleDeserialize(arraybundle, obj_buffer, &
              buffer_offset, attreconflag, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          ! Set init code
          call ESMF_ArrayBundleSetInitCreated(arraybundle, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          call ESMF_StateAdd(state, arraybundle, &
              addflag=.true., proxyflag=.true.,  &
              rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

        case (ESMF_STATEITEM_STATE%ot)
          if (debug) then
            print *, "deserializing nested State, offset =", buffer_offset
          end if
          substate = ESMF_StateDeserialize(vm, obj_buffer, buffer_offset, &
              attreconflag=attreconflag, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          call ESMF_StateAdd(state, substate,   &
              addflag=.true., proxyflag=.true., &
              rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

        case (ESMF_STATEITEM_UNKNOWN%ot)
          write (errstring, '(a,i0)') 'can''t deserialize unknown type: ', stateitem_type
          if (ESMF_LogFoundError(ESMF_RC_INTNRL_INCONS, msg=errstring, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

        case default
          write (errstring, '(a,i0)') 'can''t deserialize unsupported type: ', stateitem_type
          if (ESMF_LogFoundError(ESMF_RC_INTNRL_INCONS, msg=errstring, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return
      end select

#if 0
    ! Use offset from table in case of an early exit from a deserialize method

#if !defined (__G95__)
    buffer_offset = transfer (  &
        source=obj_buffer((i+1)*ESMF_SIZEOF_DEFINT:(i+2)*ESMF_SIZEOF_DEFINT-1),  &
        mold  =i)
#else
      ! g95 snapshots prior to April 4, 2010 have a bug in TRANSFER.
      ! The following works around it.
    buffer_offset = ESMF_Reconcile_g95_getint (  &
        source=obj_buffer((i+1)*ESMF_SIZEOF_DEFINT:(i+2)*ESMF_SIZEOF_DEFINT-1))
#endif

      if (debug) then
        print *, '   buffer offset after item loop =', buffer_offset
      end if
#endif

    end do ! needs_count

    if (trace) then
      print *, '    pet', localPet,  &
          ': *** Deserialization complete'
    end if

    rc = ESMF_SUCCESS

  end subroutine ESMF_ReconcileDeserialize


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ReconcileDeserializeAll"
!BOPI
! !IROUTINE: ESMF_ReconcileDeserializeAll

! !INTERFACE:
  subroutine ESMF_ReconcileDeserializeAll(state, vm, buffer, attreconflag, rc)
!
! !ARGUMENTS:
    type (ESMF_State), intent(inout):: state
    type (ESMF_VM),    intent(in)   :: vm
    character, pointer              :: buffer(:)    ! intent(in)
    type(ESMF_AttReconcileFlag),intent(in)   :: attreconflag
    integer,           intent(out)  :: rc
!
! !DESCRIPTION:
!   Builds proxy items for each of the items in the buffer.
!
!   The arguments are:
!   \begin{description}
!   \item[state]
!     {\tt ESMF\_State} to add proxy objects to.
!   \item[vm]
!     {\tt ESMF\_VM} to use.
!   \item[buffer]
!     Buffer of serialized State objects (intent(in))
!   \item[attreconflag]
!     Flag to indicate attribute reconciliation.
!   \item[rc]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!EOPI

    integer :: localrc
    integer :: memstat

    type(ESMF_FieldBundle) :: fieldbundle
    type(ESMF_Field) :: field
    type(ESMF_Array) :: array
    type(ESMF_ArrayBundle) :: arraybundle
    type(ESMF_State) :: substate

    integer :: stateitem_type
    character(ESMF_MAXSTR) :: errstring
    character(ESMF_MAXSTR) :: name
    integer :: localPet
    logical, parameter :: debug = .false.
    logical, parameter :: trace = .false.

    integer :: item, numNewItems
    integer :: itemType
    integer :: sizeBuffer, posBuffer  

! XMRKX !
    
    ! VM information for debug output
    call ESMF_VMGet (vm, localPet=localPet, rc=localrc)
    if (ESMF_LogFoundError (localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    
    ! Set start position of buffer
    posBuffer = 1

    ! Get the number of items to add
    numNewItems = transfer (  &
        source=buffer(posBuffer:posBuffer+ESMF_SIZEOF_DEFINT-1),  &
        mold  = numNewItems)
    posBuffer = posbuffer + ESMF_SIZEOF_DEFINT
    
    ! Loop getting new items
    do item=1, numNewItems

       ! Get item type
       itemType = transfer (  &
            source=buffer(posBuffer:posBuffer+ESMF_SIZEOF_DEFINT-1),  &
            mold  = itemType)
       posBuffer = posbuffer + ESMF_SIZEOF_DEFINT

       ! Get items
       select case (itemType)
        case (ESMF_STATEITEM_FIELDBUNDLE%ot)
          if (debug) then
            print *, "deserializing FieldBundle, pos =",posBuffer
          end if
          fieldbundle = ESMF_FieldBundleDeserialize(buffer, posBuffer, &
              attreconflag=attreconflag, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          call ESMF_StateAdd(state, fieldbundle, &
              addflag=.true., proxyflag=.true.,  &
              rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

        case (ESMF_STATEITEM_FIELD%ot)
          if (debug) then
            print *, "deserializing Field, pos =", posBuffer
          end if
          field = ESMF_FieldDeserialize(buffer, posBuffer, &
              attreconflag=attreconflag, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          ! Debug
          call ESMF_FieldGet(field, name=name, rc=localrc) 
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          if (debug) then
            print *, "created field, ready to add to local state"
          end if

          call ESMF_StateAdd(state, field,      &
              addflag=.true., proxyflag=.true., &
              rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

        case (ESMF_STATEITEM_ARRAY%ot)
          if (debug) then
            print *, "    PET", localPet,  &
                ": deserializing Array pos =",posBuffer
          end if
          call c_ESMC_ArrayDeserialize(array, buffer, posBuffer, &
              attreconflag, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          ! Set init code
          call ESMF_ArraySetInitCreated(array, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          call ESMF_StateAdd(state, array,      &
              addflag=.true., proxyflag=.true., &
              rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

        case (ESMF_STATEITEM_ARRAYBUNDLE%ot)
          if (debug) then
            print *, "deserializing ArrayBundle pos =",posBuffer
          end if
          call c_ESMC_ArrayBundleDeserialize(arraybundle, buffer, posBuffer, &
              attreconflag, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          ! Set init code
          call ESMF_ArrayBundleSetInitCreated(arraybundle, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          call ESMF_StateAdd(state, arraybundle, &
              addflag=.true., proxyflag=.true.,  &
              rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

        case (ESMF_STATEITEM_STATE%ot)
          if (debug) then
            print *, "deserializing nested State pos =",posBuffer
          end if
          substate = ESMF_StateDeserialize(vm,  buffer, posBuffer, &
              attreconflag=attreconflag, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          call ESMF_StateAdd(state, substate,   &
              addflag=.true., proxyflag=.true., &
              rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

        case (ESMF_STATEITEM_UNKNOWN%ot)
          write (errstring, '(a,i0)') 'can''t deserialize unknown type: ', itemType
          if (ESMF_LogFoundError(ESMF_RC_INTNRL_INCONS, msg=errstring, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

        case default
          write (errstring, '(a,i0)') 'can''t deserialize unsupported type: ', itemType
          if (ESMF_LogFoundError(ESMF_RC_INTNRL_INCONS, msg=errstring, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return
      end select
       
    enddo


    ! Return success
    rc = ESMF_SUCCESS

  end subroutine ESMF_ReconcileDeserializeAll

  
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ReconcileExchgAttributes"
!BOPI
! !IROUTINE: ESMF_ReconcileExchgAttributes
!
! !INTERFACE:
  subroutine ESMF_ReconcileExchgAttributes (state, vm, rc)
!
! !ARGUMENTS:
    type(ESMF_State),  intent(inout):: state
    type(ESMF_VM),     intent(in)   :: vm
    integer,           intent(out)  :: rc
!
! !DESCRIPTION:
!
!  Exchange attributes on the base of the State itself.
!
!   The arguments are:
!   \begin{description}
!   \item[state]
!     {\tt ESMF\_State} to add proxy objects to.
!   \item[vm]
!     The current {\tt ESMF\_VM} (virtual machine).
!   \item[rc]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!EOPI
    integer :: localrc
    integer :: memstat

    type(ESMF_Base), pointer :: base
    type(ESMF_Base) :: base_temp
    character, allocatable :: buffer_local(:), buffer_recv(:)
    integer,   allocatable :: recv_sizes(:), recv_offsets(:)
    integer :: buffer_size(1)

    integer :: i, pass
    integer :: localPet, petCount
    integer :: offset
    type(ESMF_InquireFlag) :: inqflag
    type(ESMF_Info) :: base_info, base_temp_info

    logical, parameter :: debug = .false.
    logical, parameter :: profile = .true.

    rc = ESMF_RC_NOT_IMPL

    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    base => state%statep%base

    ! Serialize the Base attributes
    if (profile) then
      call ESMF_TraceRegionEnter("Serialize the Base attributes", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    do, pass = 1, 2
      select case (pass)
      case (1)
      ! Pass 1 finds the required buffer length to serialize any attributes.
        allocate (buffer_local(4), stat=memstat)  ! Dummy to avoid null pointer derefs
        if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return
        inqflag = ESMF_INQUIREONLY

      case (2)
        ! Pass 2 allocates the buffer and performs the actual serialization.
        deallocate (buffer_local, stat=memstat)
        if (ESMF_LogFoundDeallocError(memstat, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return

        allocate (buffer_local(0:offset-1), stat=memstat)
        if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return
        buffer_local = achar (0)
        inqflag = ESMF_NOINQUIRE
      end select

      offset = 0
      call ESMF_BaseSerialize (base, buffer_local, offset,  &
          ESMF_ATTRECONCILE_ON, inqflag,  &
          rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return

    end do ! pass
    if (profile) then
      call ESMF_TraceRegionExit("Serialize the Base attributes", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif

    ! Exchange serialized buffer sizes
    if (profile) then
      call ESMF_TraceRegionEnter("Exchange serialized buffer sizes", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    allocate (recv_sizes(0:petCount-1), stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    buffer_size(1) = offset

    if (profile) then
      call ESMF_TraceRegionEnter("ESMF_VMAllGather", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    call ESMF_VMAllGather (vm,  &
        sendData=buffer_size, recvData=recv_sizes,  &
        count=1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    if (profile) then
      call ESMF_TraceRegionExit("ESMF_VMAllGather", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif

    if (profile) then
      call ESMF_TraceRegionExit("Exchange serialized buffer sizes", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    if (debug) then
      print *, ESMF_METHOD,  &
          ':  PET', localPet, ': Base sizes   recved are:', recv_sizes
    end if

    ! Exchange serialized buffers
    if (profile) then
      call ESMF_TraceRegionEnter("Exchange serialized buffers", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    allocate (  &
        buffer_recv(0:sum (recv_sizes)-1),  &
        recv_offsets(0:petCount-1), stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    recv_offsets(0) = 0
    do, i=1, petCount-1
      recv_offsets(i) = recv_offsets(i-1)+recv_sizes(i-1)
    end do

    if (debug) then
      print *, ESMF_METHOD,  &
          ':  PET', localPet, ': Base offsets recved are:', recv_offsets
    end if

    if (profile) then
      call ESMF_TraceRegionEnter("ESMF_VMAllGatherV", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    call ESMF_VMAllGatherV (vm,  &
        sendData=buffer_local(:buffer_size(1)-1), sendCount=buffer_size(1),  &
        recvData=buffer_recv, recvCounts=recv_sizes, recvOffsets=recv_offsets,  &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    if (profile) then
      call ESMF_TraceRegionExit("ESMF_VMAllGatherV", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif

    if (profile) then
      call ESMF_TraceRegionExit("Exchange serialized buffers", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif

    ! Update local Base
    if (profile) then
      call ESMF_TraceRegionEnter("Update local Base", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    do, i=0, petCount-1
      if (i /= localPet) then
        base_temp = ESMF_BaseDeserializeWoGarbage(buffer_recv, &
          offset=recv_offsets(i), attreconflag=ESMF_ATTRECONCILE_ON, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_BaseSetInitCreated(base_temp, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_InfoGetFromBase(base_temp, base_temp_info, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_InfoGetFromBase(base, base_info, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_InfoUpdate(base_info, base_temp_info, recursive=.true., &
          rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_BaseDestroyWoGarbage(base_temp, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      end if
    end do
    if (profile) then
      call ESMF_TraceRegionExit("Update local Base", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif

    deallocate(buffer_local)
    deallocate(recv_sizes)
    deallocate(recv_offsets)
    deallocate(buffer_recv)

    rc = ESMF_SUCCESS

  end subroutine ESMF_ReconcileExchgAttributes

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ReconcileExchgIDInfo"
!BOPI
! !IROUTINE: ESMF_ReconcileExchgIDInfo
!
! !INTERFACE:
  subroutine ESMF_ReconcileExchgIDInfo (vm,  &
      nitems_buf, id, vmid, id_info, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),          intent(in)  :: vm
    integer,                intent(in)  :: nitems_buf(0:)
    integer,                intent(in)  :: id(0:)
    integer,                intent(in)  :: vmid(0:)
    type(ESMF_ReconcileIDInfo), intent(inout) :: id_info(0:)
    integer,                intent(out) :: rc
!
! !DESCRIPTION:
!
!  Dense AlltoAll of all Ids and VMIds from every PET to every PET.
!
!   The arguments are:
!   \begin{description}
!   \item[vm]
!     The current {\tt ESMF\_VM} (virtual machine).
!   \item[nitems_buf]
!     Number of items on each PET.
!   \item[id]
!     The object ids of this PETs State itself (in element 0) and the items
!     contained within it.  It does not return the IDs of nested State
!     items.
!   \item[vmid]
!     The object integer VMIds of this PET's State itself (in element 0) and the
!     items contained within it.  It does not return the IDs of nested State items.
!   \item[id_info]
!     Array of arrays of global VMId info.  Array has a size of numPets, and each
!     element points to Id/VMId arrays.  Also returns which objects are not
!     present on the current PET.
!   \item[rc]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!EOPI

    integer :: localrc
    integer :: localPet, petCount
    integer :: send_pet
    integer, allocatable :: counts_buf_send(:), counts_buf_recv(:)
    integer, allocatable :: displs_buf_send(:), displs_buf_recv(:)
    integer :: i, ipos
    integer :: memstat

    integer, allocatable :: id_recv(:), vm_intids_recv(:)

    logical, parameter :: debug = .false.
    logical, parameter :: meminfo = .false.
    character(len=ESMF_MAXSTR) :: logmsg

    localrc = ESMF_RC_NOT_IMPL

    if (meminfo) call ESMF_VMLogMemInfo ('entering ESMF_ReconcileExchgIDInfo')

    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    ! Sanity checks

    if (size (id) /= size (vmid)) then
      if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, ESMF_ERR_PASSTHRU,  &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

    if (size (id_info) /= petCount) then
      if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, ESMF_ERR_PASSTHRU,  &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

    if (size (nitems_buf) /= petCount) then
      if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, ESMF_ERR_PASSTHRU,  &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

#if 0
    ! Log some information about the number of items in the state.
    write(logmsg, *) nitems_buf
    call ESMF_LogWrite("nitems_buf="//trim(logmsg))
    ! Log the current integer VM identifiers
    write(logmsg, *) "vmid=", vmid
    call ESMF_LogWrite(trim(logmsg))
    ! Log the current integer Base identifiers
    write(logmsg, *) "id=", id
    call ESMF_LogWrite(trim(logmsg))
#endif

    ! Broadcast each Id to all the other PETs.  Since the number of items per
    ! PET can vary, use AllToAllV.
    do, i=0, petCount-1
      allocate (  &
          id_info(i)%  id  (0:nitems_buf(i)), &
          id_info(i)%vmid  (0:nitems_buf(i)), &
          id_info(i)%needed(  nitems_buf(i)), &
          stat=memstat)
      if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
      id_info(i)%needed = .false.
    end do

    if (meminfo) call ESMF_VMLogMemInfo ('tp ESMF_ReconcileExchgIDInfo - after id_info allocate and VMIDCreate')

    ! First, compute counts and displacements for AllToAllV calls.  Note that
    ! sending displacements are always zero, since each PET is broadcasting

    allocate (counts_buf_send(0:petCount-1), displs_buf_send(0:petCount-1),  &
              counts_buf_recv(0:petCount-1), displs_buf_recv(0:petCount-1),  &
              stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    ! Add 1 to take the State itself (element 0) into account
    counts_buf_send = nitems_buf(localPet) + 1
    counts_buf_recv = nitems_buf + 1

    displs_buf_send    = 0 ! Always zero, since local PET is broadcasting
    displs_buf_recv(0) = 0
    do, i=1, petCount-1
      displs_buf_recv(i) = displs_buf_recv(i-1) + counts_buf_recv(i-1)
    end do

    if (debug) then
      do, i=0, petCount-1
        if (i == localPet) then
          write (6,*) ESMF_METHOD, ': pet', localPet, ': counts_buf_send =', counts_buf_send
          write (6,*) ESMF_METHOD, ': pet', localPet, ': displs_buf_send =', displs_buf_send
          write (6,*) ESMF_METHOD, ': pet', localPet, ': counts_buf_recv =', counts_buf_recv
          write (6,*) ESMF_METHOD, ': pet', localPet, ': displs_buf_recv =', displs_buf_recv
          call ESMF_UtilIOUnitFlush (ESMF_UtilIOStdout)
        end if
        call ESMF_VMBarrier (vm)
      end do
    end if

    ! Exchange Base Ids -------------------------------------------------------

    ! NOTE: The base id exchange could be combined with the integer VM Id
    ! exchange below. Historically, they were separate because the VM Ids were
    ! character strings. Now that the VM Ids used in reconcile are integers, they
    ! could be both be exhchanged in the same AllToAll call.

    allocate(id_recv(0:sum (counts_buf_recv+1)-1), stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    if (debug) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ':   Exchanging Ids   (using ESMF_VMAllGatherV)')
    end if
    call ESMF_VMAllGatherV (vm,  &
        sendData=id     , sendCount =size (id),  &
        recvData=id_recv, recvCounts=counts_buf_recv, recvOffsets=displs_buf_recv,  &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    if (meminfo) call ESMF_VMLogMemInfo ('tp ESMF_ReconcileExchgIDInfo - after VMAllGatherV for Base Ids')

    ipos = 0
    do, i=0, petCount-1
      id_info(i)%id = id_recv(ipos:ipos+counts_buf_recv(i)-1)
      ipos = ipos + counts_buf_recv(i)
    end do

    ! Exchange VMIds ----------------------------------------------------------

    allocate(vm_intids_recv(0:sum (counts_buf_recv+1)-1), stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    call ESMF_VMAllGatherV (vm,  &
        sendData=vmid, sendCount=size(vmid),  &
        recvData=vm_intids_recv, recvCounts=counts_buf_recv, recvOffsets=displs_buf_recv,  &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    if (meminfo) call ESMF_VMLogMemInfo ('tp ESMF_ReconcileExchgIDInfo - after VMAllGatherV for Base Ids')

    ipos = 0
    do, i=0, petCount-1
      id_info(i)%vmid = vm_intids_recv(ipos:ipos+counts_buf_recv(i)-1)
      ipos = ipos + counts_buf_recv(i)
    end do

!    if (debug) then
!      do, j=0, petCount-1
!       if (j == localPet) then
!         do, i=0, ubound (id_info, 1)
!           write (6,*) 'pet', j, ': id_info%id     =', id_info(i)%id
!           call ESMF_UtilIOUnitFlush (ESMF_UtilIOStdout)
!         end do
!       end if
!       call ESMF_VMBarrier (vm)
!      end do
!    end if

! NOTE: This code is a non-collective approach to the base and VM id exchanges.
! It will probably not be used, but AllToAll calls are notoriously problematic
! with some MPI implementations.
#if 0
    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ':   Exchanging VMIds (using ESMF_VMAllGatherVMId)')
    end if

    allocate (vmid_recv(0:sum (counts_buf_recv+1)-1),  &
        stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    call ESMF_VMIdCreate (vmid_recv, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    call ESMF_VMAllGatherV (vm,  &
        sendData=vmid, sendCount=size (vmid),  &
        recvData=vmid_recv, recvCounts=counts_buf_recv, recvOffsets=displs_buf_recv,  &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    ipos = 0
    do, i=0, petCount-1
      call ESMF_VMIdCopy (  &
          dest  =id_info(i)%vmid,  &
          source=vmid_recv(ipos:ipos+counts_buf_recv(i)-1),  &
          rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
      ipos = ipos + counts_buf_recv(i)
    end do
!else
! VMBcastVMId version
    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ':   Exchanging VMIds (using ESMF_VMBcastVMId)')
    end if
    if (debug) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ':     VMIdCopying...')
    end if
    call ESMF_VMIdCopy (  &
        dest=id_info(localPet)%vmid,  &
        source=vmid,  &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    do, send_pet=0, petCount-1
      if (debug) then
        call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
            ':     broadcasting VMId, using rootPet ' // iToS (send_pet),  &
            ask=.false.)
      end if
      call ESMF_VMBcastVMId (vm,  &
          bcstData=id_info(send_pet)%vmid,  &
          count=size (id_info(send_pet)%vmid),  &
          rootPet=send_pet, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end do
#endif

    rc = localrc

    if (meminfo) call ESMF_VMLogMemInfo ('exiting ESMF_ReconcileExchgIDInfo')

  end subroutine ESMF_ReconcileExchgIDInfo

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ReconcileExchgItems"
!BOPI
! !IROUTINE: ESMF_ReconcileExchgItems
!
! !INTERFACE:
  subroutine ESMF_ReconcileExchgItems (vm, id_info, recv_items, recv_buffer, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)  :: vm
    type(ESMF_ReconcileIDInfo), intent(in)  :: id_info(0:)
    type(ESMF_CharPtr),         intent(out) :: recv_items(0:)
    character,                  pointer     :: recv_buffer(:) ! intent(out)
    integer,                    intent(out) :: rc
!
! !DESCRIPTION:
!
!  Performs alltoallv communications of serialized data from offering PETs
!  to PETs requesting items.
!
!   The arguments are:
!   \begin{description}
!   \item[vm]
!     The current {\tt ESMF\_VM} (virtual machine).
!   \item[id_info]
!     Array of arrays of global VMId info.
!   \item[recv_items]
!     Array of arrays of serialized item data.
!   \item[rc]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!EOPI

    integer :: localrc
    integer :: memstat
    integer :: localPet, petCount
    integer :: i
    integer :: itemcount, itemcount_global, itemcount_local
    integer :: offset_pos

    integer,   allocatable :: counts_recv(:),  counts_send(:)
    integer,   allocatable :: offsets_recv(:), offsets_send(:)
    character, allocatable :: buffer_send(:)

    character, pointer :: cptr_tmp(:)

    logical, parameter :: debug = .false.
    logical, parameter :: meminfo = .false.
    logical, parameter :: profile = .true.

    character(len=ESMF_MAXSTR) :: logmsg

    ! -------------------------------------------------------------------------

    if (meminfo) call ESMF_VMLogMemInfo("entering ESMF_ReconcileExchgItems")

    localrc = ESMF_RC_NOT_IMPL

    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    if (size (id_info) /= petCount) then
      if (ESMF_LogFoundError(ESMF_RC_INTNRL_INCONS, &
          msg="size (id_info) /= petCount", &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

    if (size (recv_items) /= petCount) then
      if (ESMF_LogFoundError(ESMF_RC_INTNRL_INCONS, &
          msg="size (recv_items) /= petCount", &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

!   Set up send counts, offsets, and buffer.

    allocate (  &
        counts_send (0:petCount-1),  &
        offsets_send(0:petCount-1),  &
        stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    do, i=0, petCount-1
      if (associated (id_info(i)%item_buffer)) then
        counts_send(i) = size (id_info(i)%item_buffer)
      else
        counts_send(i) = 0
      end if
    end do

    itemcount_local = counts_send(localPet)
    itemcount_global = sum (counts_send)

    allocate (  &
        buffer_send(0:max (0,itemcount_global-1)),  &
        stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    offset_pos = 0
    do, i=0, petCount-1
      itemcount = counts_send(i)
      offsets_send(i) = offset_pos
      if (associated (id_info(i)%item_buffer)) then
        buffer_send(offset_pos:offset_pos+itemcount-1) = id_info(i)%item_buffer
      end if
      offset_pos = offset_pos + itemcount
    end do

!   Set up recv counts, offsets, and buffer.  Since there will be a different
!   buffer size from each remote PET, an AllToAll communication is necessary
!   for PETs to exchange the buffer sizes they are sending to each other.

    allocate (  &
        counts_recv(0:petCount-1),  &
        stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    if (profile) then
      call ESMF_TraceRegionEnter("ESMF_VMAllToAll", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    call ESMF_VMAllToAll (vm,  &
        sendData=counts_send, sendCount=1,  &
        recvData=counts_recv, recvCount=1,  &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    if (profile) then
      call ESMF_TraceRegionExit("ESMF_VMAllToAll", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    if (debug) then
      print *, ESMF_METHOD, ': PET', localPet, ': serialized buffer sizes',  &
          ': counts_send =', counts_send,  &
          ', counts_recv =', counts_recv
    end if

    allocate (  &
        offsets_recv(0:petCount-1),  &
        recv_buffer(0:max (0, sum (counts_recv)-1)),  &
        stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    offset_pos = 0
    do, i=0, petCount-1
      itemcount = counts_recv(i)
      offsets_recv(i) = offset_pos
      offset_pos = offset_pos + itemcount
    end do

#if 0
    write(logmsg, *) SIZE(buffer_send)
    call ESMF_LogWrite("SIZE(buffer_send)="//TRIM(logmsg), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
    write(logmsg, *) SIZE(recv_buffer)
    call ESMF_LogWrite("SIZE(recv_buffer)="//TRIM(logmsg), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
#endif

    ! AlltoAllV

    if (profile) then
      call ESMF_TraceRegionEnter("ESMF_VMAllToAllV", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    call ESMF_VMAllToAllV (vm,  &
        sendData=buffer_send, sendCounts=counts_send, sendOffsets=offsets_send,  &
        recvData=recv_buffer, recvCounts=counts_recv, recvOffsets=offsets_recv,  &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    if (profile) then
      call ESMF_TraceRegionExit("ESMF_VMAllToAllV", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif

    if (meminfo) call ESMF_VMLogMemInfo("tp ESMF_ReconcileExchgItems: after ESMF_VMAllToAllV")

    deallocate (buffer_send, counts_send, offsets_send,  &
        stat=memstat)
    if (ESMF_LogFoundDeallocError (memstat, ESMF_ERR_PASSTHRU,  &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    ! Copy recv buffers into recv_items

    do, i=0, petCount-1
      itemcount = counts_recv(i)
      if (itemcount > 0) then
        offset_pos = offsets_recv(i)
#if 1
      ! Fortran 2003 version
        recv_items(i)%cptr(0:) => recv_buffer(offset_pos:offset_pos+itemcount-1)
#else
      ! Fortran 90/95 version
        cptr_tmp => recv_buffer(offset_pos:offset_pos+itemcount-1)
      ! cptr_tmp is 1-based.  Convert to 0-based.
        call ptr_assoc_zero (cptr_tmp, itemcount, recv_items(i)%cptr)
!       print *, 'associated cptr(', lbound (recv_items(i)%cptr,1), ':', ubound (recv_items(i)%cptr,1), ')'
#endif
      else
        recv_items(i)%cptr => null ()
      end if
    end do

    rc = localrc

    if (meminfo) call ESMF_VMLogMemInfo("exiting ESMF_ReconcileExchgItems")

  contains

    subroutine ptr_assoc_zero (cbuffer, itemcount, cptr)
      integer,   intent(in)   :: itemcount
      character, intent(in), target :: cbuffer(0:itemcount-1)
      character, pointer      :: cptr(:)

      cptr => cbuffer

    end subroutine

  end subroutine ESMF_ReconcileExchgItems

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ReconcileExchgNeeds"
!BOPI
! !IROUTINE: ESMF_ReconcileExchgNeeds
!
! !INTERFACE:
  subroutine ESMF_ReconcileExchgNeeds (vm, id_info, recv_needs, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)  :: vm
    type(ESMF_ReconcileIDInfo), intent(in)  :: id_info(0:)
    logical,                    pointer     :: recv_needs(:,:) ! intent(out)
    integer,                    intent(out) :: rc
!
! !DESCRIPTION:
!
!  Performs alltoallv communications from needy PETs to PETs which offer
!  items they need.
!
!   The arguments are:
!   \begin{description}
!   \item[vm]
!     The current {\tt ESMF\_VM} (virtual machine).
!   \item[id_info]
!     Array of arrays of global VMId info.  The 'needed' flags indicate
!     which items are needed from which offering PETs.
!   \item[recv_needs]
!     Array of needy PETs and their needs.  If a flag is set, the PET
!     needs the item.
!   \item[rc]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!EOPI

    integer :: localrc
    integer :: memstat
    integer :: localPet, petCount
    integer :: i
    integer :: itemcount, itemcount_global, itemcount_local
    integer :: offset_pos

    integer, allocatable :: counts_recv(:),  counts_send(:)
    integer, allocatable :: offsets_recv(:), offsets_send(:)
    logical, allocatable :: buffer_recv(:),  buffer_send(:)

    character(ESMF_MAXSTR) :: msgstring

    logical, parameter :: debug = .false.
    logical, parameter :: profile = .true.

    localrc = ESMF_RC_NOT_IMPL

    if (associated (recv_needs)) then
      if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, ESMF_ERR_PASSTHRU,  &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    if (size (id_info) /= petCount) then
      if (ESMF_LogFoundError(ESMF_RC_INTNRL_INCONS, &
          msg="size (id_info) /= petCount", &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

!   Set up send counts, offsets, and buffer.  Note that each remote PET
!   can have differing numbers of items to offer.

    allocate (  &
        counts_send (0:petCount-1),  &
        offsets_send(0:petCount-1),  &
        stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    do, i=0, petCount-1
      counts_send(i) = size (id_info(i)%needed)
    end do

    itemcount_local = counts_send(localPet)
    itemcount_global = sum (counts_send)

    allocate (  &
        buffer_send(0:itemcount_global-1),  &
        stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    offset_pos = 0
    do, i=0, petCount-1
      itemcount = counts_send(i)
      offsets_send(i) = offset_pos
      buffer_send(offset_pos:offset_pos+itemcount-1) = id_info(i)%needed
      offset_pos = offset_pos + itemcount
    end do

!   Each remote PET should return a buffer that is the same
!   size as the number of items on the local PET.  So the recv_needs
!   buffer can be a simple rectangular matrix of which PETs need
!   which of my items.

    allocate (  &
        counts_recv (0:petCount-1),  &
        offsets_recv(0:petCount-1),  &
        buffer_recv(0:itemcount_local*petCount-1),  &
        recv_needs(itemcount_local,0:petCount-1), stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    counts_recv = itemcount_local
    offsets_recv = itemcount_local * (/ (i,i=0, petCount-1) /)
    buffer_recv = .false.

    ! AlltoAllV

    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ': calling VMAllToAll')
    end if

    if (profile) then
      call ESMF_TraceRegionEnter("ESMF_VMAllToAllV", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif

    call ESMF_VMAllToAllV (vm,  &
        sendData=buffer_send, sendCounts=counts_send, sendOffsets=offsets_send, &
        recvData=buffer_recv, recvCounts=counts_recv, recvOffsets=offsets_recv, &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    if (profile) then
      call ESMF_TraceRegionExit("ESMF_VMAllToAllV", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif

    ! Copy recv buffers into recv_needs

    do, i=0, petCount-1
      itemcount = counts_recv(i)
      offset_pos = offsets_recv(i)
      recv_needs(:,i) = buffer_recv(offset_pos:offset_pos+itemcount-1)
    end do

    if (debug) then
      do, i=0, petCount-1
        write (msgstring,'(a,i0,a,i0,a)')  &
            '  PET ', localPet, ': needs that PET ', i, ' requested are:'
        write (6,*) trim (msgstring), recv_needs(:,i)
        call ESMF_UtilIOUnitFlush (ESMF_UtilIOStdout)
      end do
    end if

    rc = localrc

  end subroutine ESMF_ReconcileExchgNeeds

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ReconcileGetStateIDInfo"
!BOPI
! !IROUTINE: ESMF_ReconcileGetStateIDInfo
!
! !INTERFACE:
  subroutine ESMF_ReconcileGetStateIDInfo (state, siwrap, id, vmid, rc)
!
! !ARGUMENTS:
    type (ESMF_State), intent(in)  :: state
    type(ESMF_StateItemWrap), pointer :: siwrap(:)! intent(in)
    integer,           pointer     :: id(:)       ! intent(out)
    type(ESMF_VMId),   pointer     :: vmid(:)     ! intent(out)
    integer,           intent(out) :: rc
!
! !DESCRIPTION:
!
!   The arguments are:
!   \begin{description}
!   \item[state]
!     {\tt ESMF\_State} to collect information from.
!   \item[siwrap]
!     Pointers to the items in the State
!   \item[id]
!     The object ids of the State itself (in element 0) and the items
!     contained within it.  It does not return the IDs of nested State
!     items.
!   \item[vmid]
!     The object VMIds of the State itself (in element 0) and the items
!     contained within it.  It does not return the IDs of nested State
!     items.  Note that since VMId is a deep object class, the vmid array
!     has aliases to existing VMId objects, rather than copies of them.
!   \item[rc]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!EOPI

    type(ESMF_Array),           pointer :: arrayp
    type(ESMF_ArrayBundle),     pointer :: abundlep
    type(ESMF_FieldType),       pointer :: fieldp
    type(ESMF_FieldBundleType), pointer :: fbundlep
    type(ESMF_RouteHandle),     pointer :: rhandlep
    type(ESMF_StateClass),      pointer :: statep

    integer :: localrc
    integer :: i
    integer :: memstat
    integer :: nitems

    localrc = ESMF_RC_NOT_IMPL

    if (associated (id) .or. associated (vmid)) then
      if (ESMF_LogFoundError(ESMF_RC_ARG_BAD,  &
          ESMF_ERR_PASSTHRU,  &
          ESMF_CONTEXT, rcToReturn=rc)) return
    end if

    if (associated (siwrap)) then
      nitems = size (siwrap)
    else
      nitems = 0
    end if

    allocate (  &
              id(0:nitems),  &
            vmid(0:nitems),  &
        stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

! Element 0s are for the State itself

    statep => state%statep

    call ESMF_BaseGetID(statep%base, id(0), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    call ESMF_BaseGetVMId(statep%base, vmid(0), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

! Loop over each item

    do, i=1, nitems

      select case (siwrap(i)%si%otype%ot)

      case (ESMF_STATEITEM_ARRAY%ot)
        arrayp => siwrap(i)%si%datap%ap

        call c_ESMC_GetID(arrayp, id(i), localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return

        call c_ESMC_GetVMId(arrayp, vmid(i), localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return

      case (ESMF_STATEITEM_ARRAYBUNDLE%ot)
        abundlep => siwrap(i)%si%datap%abp

        call c_ESMC_GetID(abundlep, id(i), localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return

        call c_ESMC_GetVMId(abundlep, vmid(i), localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return

      case (ESMF_STATEITEM_FIELD%ot)
        fieldp => siwrap(i)%si%datap%fp%ftypep

        call ESMF_BaseGetID(fieldp%base, id(i), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return

        call ESMF_BaseGetVMID(fieldp%base, vmid(i), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return

      case (ESMF_STATEITEM_FIELDBUNDLE%ot)
        fbundlep => siwrap(i)%si%datap%fbp%this

        call ESMF_BaseGetID(fbundlep%base, id(i), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return

        call ESMF_BaseGetVMID(fbundlep%base, vmid(i), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return

      case (ESMF_STATEITEM_ROUTEHANDLE%ot)
        rhandlep => siwrap(i)%si%datap%rp

        call c_ESMC_GetID(rhandlep, id(i), localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return

        call c_ESMC_GetVMId(rhandlep, vmid(i), localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return

      case (ESMF_STATEITEM_STATE%ot)
        statep => siwrap(i)%si%datap%spp

        call ESMF_BaseGetID(statep%base, id(i), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return

        call ESMF_BaseGetVMID(statep%base, vmid(i), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return

      case default
        if (ESMF_LogFoundError(ESMF_RC_INTNRL_INCONS, &
            msg="Unknown State item type", &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return

      end select

    end do

    rc = ESMF_SUCCESS

  end subroutine ESMF_ReconcileGetStateIDInfo

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ReconcileInitialize"
!BOPI
! !IROUTINE: ESMF_ReconcileInitialize
!
! !INTERFACE:
  subroutine ESMF_ReconcileInitialize (state, vm,  &
      siwrap, nitems_all, rc)
!
! !ARGUMENTS:
    type (ESMF_State), intent(inout)   :: state
    type (ESMF_VM),    intent(in)      :: vm
    type (ESMF_StateItemWrap), pointer :: siwrap(:)     ! intent(out)
    integer,                   pointer :: nitems_all(:) ! intent(out)
    integer,           intent(out)     :: rc
!
! !DESCRIPTION:
!
!   The arguments are:
!   \begin{description}
!   \item[state]
!     {\tt ESMF\_State} to collect information from.
!   \item[rc]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!EOPI

    integer :: localrc
    integer :: memstat
    integer :: nitems_local(1)
    integer :: localPet, petCount

    logical, parameter :: profile = .true.

    localrc = ESMF_RC_NOT_IMPL

    if (associated (siwrap) .or. associated (nitems_all)) then
      if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    ! Brute force removal of all existing proxies from the State
    ! to handle the re-reconcile case.  If State items were removed
    ! between reconciles, there should be no proxies for them.
    !
    ! TODO: Consider maintaining a flag in the state.  Perform
    ! a communication step to see if any removals have taken place.
    ! Conditionally zap the proxies depending on whether it is actually
    ! needed.
    if (profile) then
      call ESMF_TraceRegionEnter("ESMF_ReconcileZapProxies", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    call ESMF_ReconcileZapProxies (state, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    if (profile) then
      call ESMF_TraceRegionExit("ESMF_ReconcileZapProxies", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif

    ! Obtain local PET item list
    siwrap => null ()
    call ESMF_ContainerGet (state%statep%stateContainer,  &
        itemList=siwrap, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    if (associated (siwrap)) then
      nitems_local(1) = size (siwrap)
    else
      nitems_local(1) = 0
    end if

    ! All PETs send their item counts to all the other PETs for recv array sizing.
    allocate (nitems_all(0:petCount-1), stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    if (profile) then
      call ESMF_TraceRegionEnter("ESMF_VMAllGather", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif
    call ESMF_VMAllGather (vm,  &
        sendData=nitems_local, recvData=nitems_all,  &
        count=1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    if (profile) then
      call ESMF_TraceRegionExit("ESMF_VMAllGather", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    endif

  end subroutine ESMF_ReconcileInitialize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ReconcileSerialize"
!BOPI
! !IROUTINE: ESMF_ReconcileSerialize
!
! !INTERFACE:
  subroutine ESMF_ReconcileSerialize (state, vm, siwrap,  &
      needs_list, attreconflag,  &
      id_info, rc)
!
! !ARGUMENTS:
    type (ESMF_State),          intent(in)  :: state
    type (ESMF_VM),             intent(in)  :: vm
    type (ESMF_StateItemWrap),  intent(in)  :: siwrap(:)
    logical,                    intent(in)  :: needs_list(:,0:)
    type(ESMF_AttReconcileFlag),intent(in)  :: attreconflag
    type(ESMF_ReconcileIDInfo), intent(inout) :: id_info(0:)
    integer,                    intent(out) :: rc
!
! !DESCRIPTION:
!
!   The arguments are:
!   \begin{description}
!   \item[state]
!     {\tt ESMF\_State} to collect information from.
!   \item[siwrap]
!     State items in the state.
!   \item[needs\_list]
!     List of State items that need to be sent to other PETs
!   \item[attreconflag]
!     Flag to indicate attribute reconciliation.
!   \item[id\_info]
!     IDInfo array containing buffers of serialized State objects (intent(out))
!   \item[rc]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!EOPI

    integer :: localrc
    integer :: memstat

    type PetNeeds_t
      logical :: needed = .false.
      integer :: obj_type
      character(1), pointer :: obj_buffer(:) => null ()
      integer :: buffer_size = 0 ! Actual space used in obj_buffer.  May be
                                 ! smaller than size(obj_buffer)
    end type

    type(PetNeeds_t), allocatable :: pet_needs(:)

    character(1), pointer :: obj_buffer(:)
    integer, allocatable :: type_table(:)
    type(ESMF_StateItem), pointer :: stateitem
    type(ESMF_InquireFlag) :: inqflag
    type(ESMF_State) :: wrapper

    integer :: buffer_offset
    integer :: needs_count
    integer :: item, nitems
    integer :: lbufsize
    integer :: pass

    character(ESMF_MAXSTR) :: errstring
    integer :: i
    integer :: localPet, petCount, pet

    logical, parameter :: debug=.false.
    logical, parameter :: trace=.false.
    character(len=ESMF_MAXSTR) :: logmsg
    integer :: needs_count_debug

    localrc = ESMF_RC_NOT_IMPL

    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

! Sanity check: siwrap and needs list must be the same size.

    if (ubound (siwrap, 1) /= ubound (needs_list, 1)) then
      write (errstring, '(a,i0,a,i0)')  &
          'siwrap ubound =', ubound (siwrap, 1),  &
          '/= needs_list ubound =', ubound (needs_list, 1)
      call ESMF_LogWrite (msg=errstring, logmsgFlag=ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (ESMF_LogFoundError(ESMF_RC_INTNRL_INCONS, &
          msg="ubound (siwrap) /= ubound (needs_list)", &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if
    nitems = size (siwrap)

    ! Find the union of all the needs for this PET.
    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ': *** Step 1 - Find union of needs')
    end if
    allocate (pet_needs(nitems),  &
        stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    do, i=1, nitems
      pet_needs(i)%needed = any (needs_list(i,:))
    end do
    if (debug) then
      print *, '    PET', localPet,  &
          ': needed_items array: ', pet_needs%needed
    end if

  ! Serialize all needed objects
    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ': *** Step 2 - Serialize all needed objects')
    end if
    allocate (type_table(nitems),  &
        stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
  item_loop:  &
    do, item = 1, nitems

  ! Make two passes through each needed item.  The first time to calculate
  ! the size of the buffer, and the second time to perform the actual
  ! serialization.

      if (.not. pet_needs(item)%needed) cycle item_loop

    pass_loop:  &
      do, pass = 1, 2
        select case (pass)
        ! Pass 1 finds the required buffer length to serialize the item.
        case (1)
          ! Allocate a very small buffer to avoid possible null pointer
          ! references in the serialization routines.
          inqflag = ESMF_INQUIREONLY
          allocate (obj_buffer(0:ESMF_SIZEOF_DEFINT-1), stat=memstat)
          if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return
          buffer_offset = 0

        ! Pass 2 performs the actual serialization.
        case (2)
          inqflag = ESMF_NOINQUIRE
          deallocate (obj_buffer, stat=memstat)
          if (ESMF_LogFoundDeallocError(memstat, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          if (debug) then
            print *, ESMF_METHOD, ': allocating obj_buffer bounds = (0:', buffer_offset-1, ')'
          end if

          allocate (obj_buffer(0:buffer_offset-1), stat=memstat)
          if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return
          buffer_offset = 0

        end select

        lbufsize = size (obj_buffer)

        stateitem => siwrap(item)%si
        type_table(item) = stateitem%otype%ot

        ! serialize item
        select case (stateitem%otype%ot)

          case (ESMF_STATEITEM_FIELDBUNDLE%ot)
            if (debug) then
              print *, '    PET', localPet,  &
                  ': serializing FieldBundle, pass =', pass, ', offset =', buffer_offset
            end if
            call ESMF_FieldBundleSerialize(stateitem%datap%fbp,  &
                obj_buffer, lbufsize, buffer_offset,  &
                attreconflag=attreconflag, inquireflag=inqflag,  &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT,  &
                rcToReturn=rc)) return

          case (ESMF_STATEITEM_FIELD%ot)
            if (debug) then
              print *, '    PET', localPet,  &
                  ': serializing Field, pass =', pass, ', offset =', buffer_offset
            end if
            call ESMF_FieldSerialize(stateitem%datap%fp,  &
                obj_buffer, lbufsize, buffer_offset,  &
                attreconflag=attreconflag, inquireflag=inqflag,  &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT,  &
                rcToReturn=rc)) return

          case (ESMF_STATEITEM_ARRAY%ot)
            if (debug) then
              print *, '    PET', localPet,  &
                  ': serialized Array, pass =', pass, ', offset =', buffer_offset
            end if
            call c_ESMC_ArraySerialize(stateitem%datap%ap,  &
                obj_buffer, lbufsize, buffer_offset,  &
                attreconflag, inqflag,  &
                localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT,  &
                rcToReturn=rc)) return

          case (ESMF_STATEITEM_ARRAYBUNDLE%ot)
            if (debug) then
              print *, '    PET', localPet,  &
                  ': serializing ArrayBundle, pass =', pass, ', offset =', buffer_offset
            end if
            call c_ESMC_ArrayBundleSerialize(stateitem%datap%abp,  &
                obj_buffer, lbufsize, buffer_offset,  &
                attreconflag, inqflag,  &
                localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT,  &
                rcToReturn=rc)) return

          case (ESMF_STATEITEM_STATE%ot)
            if (debug) then
              print *, '    PET', localPet,  &
                  ': serializing subState, pass =', pass, ', offset =', buffer_offset
            end if
            wrapper%statep => stateitem%datap%spp
            ESMF_INIT_SET_CREATED(wrapper)
            call ESMF_StateSerialize(wrapper,  &
                obj_buffer, lbufsize, buffer_offset,  &
                attreconflag=attreconflag, inquireflag=inqflag,  &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT,  &
                rcToReturn=rc)) return

          case (ESMF_STATEITEM_ROUTEHANDLE%ot)
            if (debug) then
              print *, '    PET', localPet,  &
                  ': ignoring RouteHandle, pass =', pass
            end if
          ! Do nothing for RouteHandles.  There is no need to reconcile them.


          case (ESMF_STATEITEM_UNKNOWN%ot)
            if (debug) then
              print *, ESMF_METHOD, ': serializing unknown: ', trim (stateitem%namep)
            end if
            call c_ESMC_StringSerialize(stateitem%namep,  &
                obj_buffer, lbufsize, buffer_offset,  &
                inqflag,  &
                localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT,  &
                rcToReturn=rc)) return

          case default
            localrc = ESMF_RC_INTNRL_INCONS
            if (debug) then
              print *, '    PET', localPet,  &
                  ': serialization error in default case.  Returning ESMF_RC_INTNRL_INCONS'
            end if

        end select

        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return

#if defined (ALIGN_FIX)
        buffer_offset = ((buffer_offset+7)/8)*8
#endif

        if (debug) then
          print *, '    PET', localPet,  &
              ': item serialized, pass =', pass, ', new offset =', buffer_offset,  &
              merge (" (calc'ed)", " (actual) ", pass == 1)
        end if

      end do pass_loop

      pet_needs(item)%obj_buffer => obj_buffer
      pet_needs(item)%buffer_size = buffer_offset
      obj_buffer => null ()

    end do item_loop

    if (debug)  &
        print *, ESMF_METHOD, ': buffer_sizes =', pet_needs(:)%buffer_size

! For each PET, create a buffer containing its serialized needs.  The buffer
! consists of a count of items, a table of the offsets (in bytes) of each
! serialized item, and the serialized items themselves.

    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ': *** Step 3 - Create per-PET serialized buffers')
    end if
    do, pet=0, petCount-1
      needs_count = count (needs_list(:,pet))
      if (debug .and. needs_count > 0) then
        print *, '    PET', localPet,  &
            ': needs_count =', needs_count, ', for PET', pet
      end if
      if (needs_count == 0) then
        id_info(pet)%item_buffer => null ()
        cycle
      end if

      ! Calculate size needed for serialized item buffer, including
      ! space for needs_count, and size/type table
      buffer_offset = ESMF_SIZEOF_DEFINT * (2 + needs_count*2)
      do, item=1, nitems
        if (needs_list(item, pet))  &
          buffer_offset = buffer_offset + pet_needs(item)%buffer_size
      end do

      if (debug) then
        print *, '    PET', localPet,  &
            ': computed buffer_offset =', buffer_offset, ', for PET', pet
      end if

      ! Fill serialized item buffer

      allocate (id_info(pet)%item_buffer(0:buffer_offset-1),  &
          stat=memstat)
      if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
      obj_buffer => id_info(pet)%item_buffer

      if (debug) then
        write(logmsg, *) "ESMF_SIZEOF_DEFINT=", ESMF_SIZEOF_DEFINT
        call ESMF_LogWrite(trim(logmsg))
        write(logmsg, *) "needs_count=", needs_count
        call ESMF_LogWrite(trim(logmsg))
      end if

      obj_buffer(0:ESMF_SIZEOF_DEFINT-1) = transfer (  &
          source=needs_count,  &
          mold  =obj_buffer(0:ESMF_SIZEOF_DEFINT-1))

      if (debug) then
        needs_count_debug = transfer(source=obj_buffer(0:ESMF_SIZEOF_DEFINT-1), &
          mold=needs_count_debug)
        write(logmsg, *) "needs_count_debug=", needs_count_debug
        call ESMF_LogWrite(trim(logmsg))
      end if

      ! space for needs_count, padding, and size/type table
      buffer_offset = ESMF_SIZEOF_DEFINT * (2 + needs_count*2)

      i = 2 * ESMF_SIZEOF_DEFINT  ! space for needs_count and a pad
      do, item=1, nitems
        if (.not. needs_list(item, pet)) cycle
        lbufsize = pet_needs(item)%buffer_size
        if (lbufsize == 0) cycle

        if (debug) then
          print *, '    PET', localPet,  &
              ': packing at buffer_offset =', buffer_offset, ', for PET', pet,  &
              ', item =', item
        end if
        obj_buffer(i:i+ESMF_SIZEOF_DEFINT-1) =  &  ! Buffer offset
            transfer (  &
            source=buffer_offset,  &
            mold  =obj_buffer(0:ESMF_SIZEOF_DEFINT-1))
        i = i + ESMF_SIZEOF_DEFINT

        obj_buffer(i:i+ESMF_SIZEOF_DEFINT-1) =  &  ! Item type
            transfer (  &
            source=type_table(item),  &
            mold  =obj_buffer(0:ESMF_SIZEOF_DEFINT-1))
        i = i + ESMF_SIZEOF_DEFINT

        obj_buffer(buffer_offset:buffer_offset+lbufsize-1) =  &  ! Serialized item
            pet_needs(item)%obj_buffer(:lbufsize-1)
        buffer_offset = buffer_offset + lbufsize
      end do ! items

    end do ! pets

    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ': *** Step 4 - Deallocate memory')
    end if

    if (allocated (pet_needs)) then
      do, i=1, nitems
        if (associated (pet_needs(i)%obj_buffer)) then
          deallocate (pet_needs(i)%obj_buffer, stat=memstat)
          if (ESMF_LogFoundDeallocError(memstat, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return
        end if
      end do

      deallocate (pet_needs, stat=memstat)
      if (ESMF_LogFoundDeallocError(memstat, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

    rc = ESMF_SUCCESS

  end subroutine ESMF_ReconcileSerialize


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ReconcileSerializeAll"
!BOPI
! !IROUTINE: ESMF_ReconcileSerializeAll
!
! !INTERFACE:
  subroutine ESMF_ReconcileSerializeAll(state, vm, siwrap,  &
      attreconflag, buffer, rc)
!
! !ARGUMENTS:
    type (ESMF_State),          intent(in)  :: state
    type (ESMF_VM),             intent(in)  :: vm
    type (ESMF_StateItemWrap),  intent(in)  :: siwrap(:)
    type(ESMF_AttReconcileFlag),intent(in)  :: attreconflag
    character, pointer                      :: buffer(:)
    integer,                    intent(out) :: rc
!
! !DESCRIPTION:
!
!   The arguments are:
!   \begin{description}
!   \item[state]
!     {\tt ESMF\_State} to collect information from.
!   \item[siwrap]
!     State items in the state.
!   \item[needs\_list]
!     List of State items that need to be sent to other PETs
!   \item[attreconflag]
!     Flag to indicate attribute reconciliation.
!   \item[rc]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!EOPI

    integer :: localrc
    integer :: memstat
    integer :: item,numStateItems
    type(ESMF_StateItem), pointer :: stateItem
    type(ESMF_State) :: wrapper
    integer :: itemType
    integer :: sizeBuffer, posBuffer
    character, pointer :: fakeBuffer(:) ! Fake buffer for passing when getting sizes
    integer :: sizeFakeBuffer
    integer :: itemSize
    type(ESMF_InquireFlag) :: inqflag
    integer :: localPet, petCount, pet

    ! XMRKX !
       
    ! Init to not implemented
    localrc = ESMF_RC_NOT_IMPL

    ! Get vm info
    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    
   ! Get the number of items
   numStateItems = size (siwrap)

   
   !!!!! Calculate buffer size !!!!!
    
   ! Start with number of items
   sizeBuffer=ESMF_SIZEOF_DEFINT

   ! Allocate a fake buffer for passing in when asking for size
   allocate(fakeBuffer(ESMF_SIZEOF_DEFINT))
   
   ! Fake buffer size
   sizeFakeBuffer=size(fakeBuffer)
   
   ! Set flag to only check size
   inqflag = ESMF_INQUIREONLY
   
   ! Loop State items computing size
   do item=1,numStateItems

      ! Get one State Item
      stateItem => siwrap(item)%si

      ! Get item type
      itemType = stateitem%otype%ot

      ! Add item type's size
      sizeBuffer = sizeBuffer + ESMF_SIZEOF_DEFINT

      ! Init itemSize to 0, so when we ask for the offset,
      ! we are also getting the size
      itemSize=0

      ! Get size of item to serialize
      select case (itemType)
          case (ESMF_STATEITEM_FIELDBUNDLE%ot)
            call ESMF_FieldBundleSerialize(stateItem%datap%fbp,  &
                fakeBuffer, sizeFakeBuffer, itemSize,  &
                attreconflag=attreconflag, inquireflag=inqflag,  &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT,  &
                rcToReturn=rc)) return
            if (debug) then
               print *, '    PET', localPet,  &
                    ' Getting FieldBundle size=',itemSize
            end if
            
          case (ESMF_STATEITEM_FIELD%ot)
            call ESMF_FieldSerialize(stateItem%datap%fp,  &
                fakeBuffer, sizeFakeBuffer, itemSize,  &
                attreconflag=attreconflag, inquireflag=inqflag,  &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT,  &
                rcToReturn=rc)) return
            if (debug) then
               print *, '    PET', localPet,  &
                    ' Getting Field size=',itemSize
            end if
    
          case (ESMF_STATEITEM_ARRAY%ot)
            call c_ESMC_ArraySerialize(stateitem%datap%ap,  &
                fakeBuffer, sizeFakeBuffer, itemSize,  &
                attreconflag, inqflag,  &
                localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT,  &
                rcToReturn=rc)) return
            if (debug) then
               print *, '    PET', localPet,  &
                    ' Getting Array size=',itemSize
            end if
            
          case (ESMF_STATEITEM_ARRAYBUNDLE%ot)
            call c_ESMC_ArrayBundleSerialize(stateitem%datap%abp,  &
                 fakeBuffer, sizeFakeBuffer, itemSize,  &
                 attreconflag, inqflag,  &
                 localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT,  &
                rcToReturn=rc)) return
            if (debug) then
               print *, '    PET', localPet,  &
                    ' Getting ArrayBundle size=',itemSize
            end if
            
          case (ESMF_STATEITEM_STATE%ot)
            wrapper%statep => stateitem%datap%spp
            ESMF_INIT_SET_CREATED(wrapper)
            call ESMF_StateSerialize(wrapper,  &
                 fakeBuffer, sizeFakeBuffer, itemSize,  &
                attreconflag=attreconflag, inquireflag=inqflag,  &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT,  &
                rcToReturn=rc)) return
                        if (debug) then
               print *, '    PET', localPet,  &
                    ' Getting State size=',itemSize
            end if

          case (ESMF_STATEITEM_ROUTEHANDLE%ot)
             ! Do nothing for RouteHandles.  There is no need to reconcile them.
             
          case (ESMF_STATEITEM_UNKNOWN%ot)
            call c_ESMC_StringSerialize(stateitem%namep,  &
                 fakeBuffer, sizeFakeBuffer, itemSize,  &
                 inqflag, localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                 ESMF_CONTEXT,  &
                 rcToReturn=rc)) return
            if (debug) then
               print *, '    PET', localPet,  &
                    ' Getting Unknown size=',itemSize
            end if
            
          case default
             if (ESMF_LogFoundError(ESMF_RC_INTNRL_INCONS, &
                  msg="Unrecognized item type.", &
                  ESMF_CONTEXT,  &
                  rcToReturn=rc)) return
        end select

        ! Update buffer size by itemSize
        sizeBuffer = sizeBuffer + itemSize        
   enddo

   ! Get rid of fakeBuffer
   deallocate(fakeBuffer)


   
   !!!!! Allocate buffer to serialize into !!!!!
   allocate(buffer(sizeBuffer), stat=memstat)
   if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return


   
   !!!!! Serialize information into buffer !!!!!

   ! Start position of buffer
   posBuffer = 1

   ! Put item count in buffer
   buffer(posBuffer:posBuffer+ESMF_SIZEOF_DEFINT-1) = transfer (  &
          source=numStateItems,  &
          mold=buffer(1:ESMF_SIZEOF_DEFINT))
   posBuffer = posbuffer + ESMF_SIZEOF_DEFINT

   ! Set flag to actually serialize
   inqflag = ESMF_NOINQUIRE
   
   ! Loop State items adding to buffer
   do item=1,numStateItems

      ! Get one State Item
      stateItem => siwrap(item)%si

      ! Get item type
      itemType = stateitem%otype%ot

      ! Add item type to buffer
      buffer(posBuffer:posBuffer+ESMF_SIZEOF_DEFINT-1) = transfer (&
          source=itemType,  &
          mold  =buffer(1:ESMF_SIZEOF_DEFINT))
      posBuffer = posbuffer + ESMF_SIZEOF_DEFINT

      ! Add serialized items
      select case (itemType)
         case (ESMF_STATEITEM_FIELDBUNDLE%ot)
         if (debug) then
            print *, '    PET', localPet,  &
                 ' Getting FieldBundle pos=',posBuffer
         end if
         call ESMF_FieldBundleSerialize(stateItem%datap%fbp,  &
                buffer, sizeBuffer, posBuffer,  &
                attreconflag=attreconflag, inquireflag=inqflag,  &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT,  &
                rcToReturn=rc)) return
            
         case (ESMF_STATEITEM_FIELD%ot)
            if (debug) then
               print *, '    PET', localPet,  &
                    ' Getting Field pos=',posBuffer
            end if            
            call ESMF_FieldSerialize(stateItem%datap%fp,  &
                  buffer, sizeBuffer, posBuffer,  &
                attreconflag=attreconflag, inquireflag=inqflag,  &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT,  &
                rcToReturn=rc)) return
         
         case (ESMF_STATEITEM_ARRAY%ot)
            if (debug) then
               print *, '    PET', localPet,  &
                    ' Getting Array pos=',posBuffer
            end if
            call c_ESMC_ArraySerialize(stateitem%datap%ap,  &
                  buffer, sizeBuffer, posBuffer,  &
                attreconflag, inqflag,  &
                localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT,  &
                rcToReturn=rc)) return
            
         case (ESMF_STATEITEM_ARRAYBUNDLE%ot)
            if (debug) then
               print *, '    PET', localPet,  &
                    ' Getting ArrayBundle pos=',posBuffer
            end if
            call c_ESMC_ArrayBundleSerialize(stateitem%datap%abp,  &
                  buffer, sizeBuffer, posBuffer,  &
                 attreconflag, inqflag,  &
                 localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT,  &
                rcToReturn=rc)) return
            
         case (ESMF_STATEITEM_STATE%ot)
            if (debug) then
               print *, '    PET', localPet,  &
                    ' Getting State pos=',posBuffer
            end if
            wrapper%statep => stateitem%datap%spp
            ESMF_INIT_SET_CREATED(wrapper)
            call ESMF_StateSerialize(wrapper,  &
                  buffer, sizeBuffer, posBuffer,  &
                attreconflag=attreconflag, inquireflag=inqflag,  &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT,  &
                rcToReturn=rc)) return
            

          case (ESMF_STATEITEM_ROUTEHANDLE%ot)
             ! Do nothing for RouteHandles.  There is no need to reconcile them.
             
          case (ESMF_STATEITEM_UNKNOWN%ot)
             if (debug) then
                print *, '    PET', localPet,  &
                     ' Getting Unknown pos=',posBuffer
             end if
             call c_ESMC_StringSerialize(stateitem%namep,  &
                  buffer, sizeBuffer, posBuffer,  &
                 inqflag, localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                 ESMF_CONTEXT,  &
                 rcToReturn=rc)) return

            
          case default
             if (ESMF_LogFoundError(ESMF_RC_INTNRL_INCONS, &
                  msg="Unrecognized item type.", &
                  ESMF_CONTEXT,  &
                  rcToReturn=rc)) return
        end select      
   enddo
   
    ! Return success
    rc = ESMF_SUCCESS

  end subroutine ESMF_ReconcileSerializeAll


   
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ReconcileZapProxies"
!BOPI
! !IROUTINE: ESMF_ReconcileZapProxies -- Zap proxies from State
!
! !INTERFACE:
    subroutine ESMF_ReconcileZapProxies(state, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout)         :: state
      integer,          intent(out), optional :: rc
!
! !DESCRIPTION:
!     Proxy objects are identified and removed from the State. Information about
!   the zapped proxies is kept in the State for later handling during the
!   companion method ESMF_ReconcileZappedProxies().
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!       The State from which to zap proxies.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
      integer                             :: localrc, i
      type(ESMF_StateClass),      pointer :: stypep
      type(ESMF_StateItemWrap),   pointer :: itemList(:)
      character(len=ESMF_MAXSTR)          :: thisname
      type(ESMF_FieldType),       pointer :: fieldp
      type(ESMF_FieldBundleType), pointer :: fbpthis
#ifdef RECONCILE_ZAP_LOG_on
      character(len=160)                  :: msgString
#endif

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      stypep => state%statep

#ifdef RECONCILE_ZAP_LOG_on
      call ESMF_VMLogGarbageInfo(prefix="ZapProxies bef: ", &
        logMsgFlag=ESMF_LOGMSG_DEBUG, rc=localrc)
#endif

      itemList => null ()
      call ESMF_ContainerGet(container=stypep%stateContainer, itemList=itemList, &
        rc=localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      stypep%zapFlag => null()
          
      if (associated(itemList)) then
        allocate(stypep%zapFlag(size(itemList)))
        stypep%zapFlag = .false.
        ! Zap all proxies
        do i=1, size(itemList)
          ! First ensure the proxyFlag for the current object in the local
          ! State is set correctly. Each object internally (in Base) knows
          ! if it was created as a proxy or not. However, the local State
          ! might not correctly track this if the proxy object was created
          ! under a different State and then copied over into this State.
          ! This can happen e.g. under NUOPC when Fields or FieldBuindles
          ! from a smaller VM are shared with a larger VM. Then on the extra
          ! PETs the sharing protocol creates proxies using a temporary State
          ! which then are copied into the actual import/exportState seen in
          ! a later Reconcile.
          ! The only use cases are Field and FieldBundle objects under State.
          if (itemList(i)%si%otype==ESMF_STATEITEM_FIELD) then
            fieldp => itemList(i)%si%datap%fp%ftypep
            call ESMF_StateItemGet(itemList(i)%si, name=thisname, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return

#ifdef RECONCILE_ZAP_LOG_on
write(msgString,*) "ESMF_ReconcileZapProxies Field proxyFlag(old): "//&
  trim(thisname), itemList(i)%si%proxyFlag
call ESMF_LogWrite(msgString, ESMF_LOGMSG_DEBUG, rc=localrc)
#endif

            ! determine proxyFlag from Base level
            itemList(i)%si%proxyFlag = ESMF_IsProxy(fieldp%base, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

#ifdef RECONCILE_ZAP_LOG_on
write(msgString,*) "ESMF_ReconcileZapProxies Field proxyFlag(new): "//&
  trim(thisname), itemList(i)%si%proxyFlag
call ESMF_LogWrite(msgString, ESMF_LOGMSG_DEBUG, rc=localrc)
#endif

          else if (itemList(i)%si%otype==ESMF_STATEITEM_FIELDBUNDLE) then
            fbpthis => itemList(i)%si%datap%fbp%this
            call ESMF_StateItemGet(itemList(i)%si, name=thisname, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return

#ifdef RECONCILE_ZAP_LOG_on
write(msgString,*) "ESMF_ReconcileZapProxies FieldBundle proxyFlag(old): "//&
  trim(thisname), itemList(i)%si%proxyFlag
call ESMF_LogWrite(msgString, ESMF_LOGMSG_DEBUG, rc=localrc)
#endif

            ! determine proxyFlag from Base level
            itemList(i)%si%proxyFlag = ESMF_IsProxy(fbpthis%base, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

#ifdef RECONCILE_ZAP_LOG_on
write(msgString,*) "ESMF_ReconcileZapProxies FieldBundle proxyFlag(new): "//&
  trim(thisname), itemList(i)%si%proxyFlag
call ESMF_LogWrite(msgString, ESMF_LOGMSG_DEBUG, rc=localrc)
#endif

          endif
          ! Now the local State proxyFlag is consistent and can be used to
          ! determine whether the current object needs to be zapped or not.
          if (itemList(i)%si%proxyFlag) then
            stypep%zapFlag(i) = .true.  ! keep record about zapping
            call ESMF_StateItemGet(itemList(i)%si, name=thisname, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
            ! Remove proxy object from state using its name. This is safe b/c
            ! a state only allows items with unique names.
            call ESMF_StateRemove (state, itemNameList=(/thisname/), rc=localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
          end if
        end do
      endif

      stypep%zapList => itemList ! hang on for ESMF_ReconcileZappedProxies()

#ifdef RECONCILE_ZAP_LOG_on
      call ESMF_VMLogGarbageInfo(prefix="ZapProxies aft: ", &
        logMsgFlag=ESMF_LOGMSG_DEBUG, rc=localrc)
#endif

      if (present(rc)) rc = ESMF_SUCCESS
    end subroutine ESMF_ReconcileZapProxies

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
!     Proxy objects that have been zapped from the State during a previous
!   call to the companion method ESMF_ReconcileZapProxies() must now be handled
!   after the reconcile work is done. Potentially new proxies have been
!   created, and the internal information of those new proxies must be copied
!   under the old proxy wrappers, then old wrappers must replace the new ones
!   inside the State. Finally the old inside members must be cleaned up.
!   There is also a chance that zapped proxy are not associated with new proxy
!   counter parts (e.g. if the actual objects have been removed from the State).
!   In that case the old proxy object must be cleaned up for good by removing
!   it from the garbage collection.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!       The State from which proxies have been zapped and must either be
!       restored or completely removed from the garbage collection.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
    integer                           :: localrc, i, k
    integer                           :: memstat
    type(ESMF_StateClass),    pointer :: stypep
    type(ESMF_StateItemWrap), pointer :: itemList(:)
    type(ESMF_StateItemWrap), pointer :: zapList(:)
    logical,                  pointer :: zapFlag(:)
    character(len=ESMF_MAXSTR)        :: thisname
    character(len=ESMF_MAXSTR)        :: name
    type(ESMF_Field)                  :: tempField
    type(ESMF_FieldType)              :: tempFieldType
    type(ESMF_FieldBundle)            :: tempFB
    type(ESMF_FieldBundleType)        :: tempFBType

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    stypep => state%statep
    zapList => stypep%zapList
    zapFlag => stypep%zapFlag

    itemList => null ()
    call ESMF_ContainerGet(container=stypep%stateContainer, itemList=itemList, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

#ifdef RECONCILE_ZAP_LOG_on
    call ESMF_VMLogGarbageInfo(prefix="ZappedProxies bef: ", &
      logMsgFlag=ESMF_LOGMSG_DEBUG, rc=localrc)
#endif

    if (associated(itemList).and.associated(zapList)) then
#ifdef RECONCILE_ZAP_LOG_on
      call ESMF_LogWrite("ESMF_ReconcileZappedProxies(): have lists", &
        ESMF_LOGMSG_DEBUG, rc=localrc)
#endif

      do i=1, size(itemList)
        if (itemList(i)%si%proxyFlag .and. &
          (itemList(i)%si%otype==ESMF_STATEITEM_FIELD .or. &
           itemList(i)%si%otype==ESMF_STATEITEM_FIELDBUNDLE )) then
          call ESMF_StateItemGet(itemList(i)%si, name=thisname, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
#ifdef RECONCILE_ZAP_LOG_on
call ESMF_LogWrite("ESMF_ReconcileZappedProxies(): found a proxy field: "//&
  trim(thisname), ESMF_LOGMSG_DEBUG, rc=localrc)
#endif
          do k=1, size(zapList)
#ifdef RECONCILE_ZAP_LOG_on
call ESMF_LogWrite("ESMF_ReconcileZappedProxies(): scanning zapList", &
  ESMF_LOGMSG_DEBUG, rc=localrc)
#endif
            if (associated (zapList(k)%si)) then
#ifdef RECONCILE_ZAP_LOG_on
call ESMF_LogWrite("ESMF_ReconcileZappedProxies(): found associated zapList object", &
  ESMF_LOGMSG_DEBUG, rc=localrc)
#endif
              ! Note that only Fields and FieldBundles receive the restoration
              ! treatment, and therefore persist during repeated Reconcile()
              ! calls. The method only works for these two types carried by
              ! State, because they are deep Fortran classes, and there is an
              ! extra layer of indirection that is used by the implemented
              ! approach. For deep C++ implemented classes this extra layer of
              ! indirection is missing, and the method would not work.
              ! For practical cases under NUOPC only Field and FieldBundle
              ! objects are relevant direct objects handled under State, and
              ! therefore the implemented method suffices.
              if (zapList(k)%si%otype==ESMF_STATEITEM_FIELD) then
                call ESMF_FieldGet(zapList(k)%si%datap%fp, name=name, rc=localrc)
                if (ESMF_LogFoundError(localrc, &
                  ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) &
                  return
#ifdef RECONCILE_ZAP_LOG_on
call ESMF_LogWrite("ESMF_ReconcileZappedProxies(): checking Field: "//trim(name), &
  ESMF_LOGMSG_DEBUG, rc=localrc)
#endif
                if (name == thisname) then
                  zapFlag(k) = .false.  ! indicate that proxy has been restored
#ifdef RECONCILE_ZAP_LOG_on
call ESMF_LogWrite("ESMF_ReconcileZappedProxies(): found Field: "//trim(name), &
  ESMF_LOGMSG_DEBUG, rc=localrc)
#endif
                  ! Bend pointers and copy contents to result in the desired
                  ! behavior for re-reconcile. From a user perspective of
                  ! Reconcile() proxies should persist when a State is
                  ! re-reconciled, and the same proxies are needed. Basically
                  ! a user should be able to hang on to a proxy.
                  tempField%ftypep => itemList(i)%si%datap%fp%ftypep
                  tempFieldType = zapList(k)%si%datap%fp%ftypep
                  zapList(k)%si%datap%fp%ftypep = itemList(i)%si%datap%fp%ftypep
                  itemList(i)%si%datap%fp%ftypep => zapList(k)%si%datap%fp%ftypep
                  tempField%ftypep = tempFieldType
                  ! Finally destroy the old Field internals
                  ESMF_INIT_SET_CREATED(tempField)
                  call ESMF_FieldDestroy(tempField, noGarbage=.true., rc=localrc)
                  if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) &
                    return
                  ! deallocate the associated StateItem
                  deallocate(zapList(k)%si)
                end if
              else if (zapList(k)%si%otype==ESMF_STATEITEM_FIELDBUNDLE) then
                call ESMF_FieldBundleGet(zapList(k)%si%datap%fbp, name=name, rc=localrc)
                if (ESMF_LogFoundError(localrc, &
                  ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) &
                  return
#ifdef RECONCILE_ZAP_LOG_on
call ESMF_LogWrite("ESMF_ReconcileZappedProxies(): checking FieldBundle: "//trim(name), &
  ESMF_LOGMSG_DEBUG, rc=localrc)
#endif
                if (name == thisname) then
                  zapFlag(k) = .false.  ! indicate that proxy has been restored
#ifdef RECONCILE_ZAP_LOG_on
call ESMF_LogWrite("ESMF_ReconcileZappedProxies(): found FieldBundle: "//trim(name), &
  ESMF_LOGMSG_DEBUG, rc=localrc)
#endif
                  ! Bend pointers and copy contents to result in the desired
                  ! behavior for re-reconcile. From a user perspective of
                  ! Reconcile() proxies should persist when a State is
                  ! re-reconciled, and the same proxies are needed. Basically
                  ! a user should be able to hang on to a proxy.
                  tempFB%this => itemList(i)%si%datap%fbp%this
                  call ESMF_FieldBundleTypeDeepCopy(zapList(k)%si%datap%fbp%this, tempFBType)
                  call ESMF_FieldBundleTypeDeepCopy(itemList(i)%si%datap%fbp%this, zapList(k)%si%datap%fbp%this)
                  itemList(i)%si%datap%fbp%this => zapList(k)%si%datap%fbp%this
                  call ESMF_FieldBundleTypeDeepCopy(tempFBType, tempFB%this)
                  ! Finally destroy the old FieldBundle internals
                  ESMF_INIT_SET_CREATED(tempFB)
                  call ESMF_FieldBundleDestroy(tempFB, noGarbage=.true., &
                    rc=localrc)
                  if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) &
                    return
                  ! deallocate the associated StateItem
                  deallocate(zapList(k)%si)
                end if

              end if
            end if
          end do ! k
        end if
      end do ! i
    endif

    ! Completely destroy any zapped proxies that did not get restored.
    ! This applies to all zapped proxy objects, regardless of type.
    if (associated(zapFlag)) then
      do k=1, size(zapFlag)
        if (zapFlag(k)) then
          if (zapList(k)%si%otype==ESMF_STATEITEM_FIELD) then
            call ESMF_FieldDestroy(zapList(k)%si%datap%fp, &
              noGarbage=.true., rc=localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) &
              return
          else if (zapList(k)%si%otype==ESMF_STATEITEM_FIELDBUNDLE) then
            call ESMF_FieldBundleDestroy(zapList(k)%si%datap%fbp, &
              noGarbage=.true., rc=localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) &
              return
          endif
          ! deallocate the associated StateItem
          if (associated(zapList(k)%si)) deallocate(zapList(k)%si)
        endif
      end do ! k
    endif

#ifdef RECONCILE_ZAP_LOG_on
    call ESMF_VMLogGarbageInfo(prefix="ZappedProxies aft: ", &
      logMsgFlag=ESMF_LOGMSG_DEBUG, rc=localrc)
#endif

    if (associated (itemList)) then
      deallocate(itemList, stat=memstat)
      if (ESMF_LogFoundDeallocError(memstat, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    end if

    if (associated (zapList)) then
      deallocate(zapList, stat=memstat)
      if (ESMF_LogFoundDeallocError(memstat, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      stypep%zapList => null()
    end if

    if (associated (zapFlag)) then
      deallocate(zapFlag, stat=memstat)
      if (ESMF_LogFoundDeallocError(memstat, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      stypep%zapFlag => null()
    end if

    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_ReconcileZappedProxies

!------------------------------------------------------------------------------
#if defined (__G95__)
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_Reconcileg95_getint"
    function ESMF_Reconcile_g95_getint (source) result (i)
      character, intent(in) :: source(:)
      integer :: i

      ! Workaround routine for g95 TRANSFER bug.

      i = transfer (source=source, mold=i)

    end function ESMF_Reconcile_g95_getint
#endif

!------------------------------------------------------------------------------
! Debugging and support procedures
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ReconcileAllRC"
  function ESMF_ReconcileAllRC (vm, rc) result (rc_return)
    type(ESMF_VM), intent(in) :: vm
    integer,       intent(in) :: rc
    integer                   :: rc_return

    integer :: rc_send(1)
    integer, allocatable :: rc_all(:)
    integer :: localPet, petCount

    call ESMF_VMGet(vm, localpet=localPet, petCount=petCount)
    allocate (rc_all(petCount))

    rc_send = rc
    call ESMF_VMGather (vm,  &
        sendData=rc_send, recvData=rc_all, count=1,  &
        rootPet=0)
    call ESMF_VMBroadcast (vm,  &
        bcstData=rc_all, count=petCount,  &
        rootPet=0)

    rc_return = rc
    if (any (rc_all /= ESMF_SUCCESS)) then
      rc_return = merge (ESMF_FAILURE, rc, rc == ESMF_SUCCESS)
    end if


  end function ESMF_ReconcileAllRC

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ReconcileDebugPrint"
  subroutine ESMF_ReconcileDebugPrint (text, multitext, ask, rc)
    use ESMF_IOUtilMod
    character(*), intent(in),  optional :: text
    character(*), intent(in),  optional :: multitext
    logical,      intent(in),  optional :: ask
    integer,      intent(out), optional :: rc

    type(ESMF_VM) :: vm
    integer :: localrc
    integer :: localPet, petCount
    character(16) :: answer
    character(10) :: time
    logical :: localask

    call ESMF_VMGetCurrent(vm=vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    localask = .false.
    if (present (ask)) then
      localask = ask
    end if

    if (present (text)) then
#if 0
      call ESMF_UtilIOUnitFlush (ESMF_UtilIOStdout)
      call ESMF_VMBarrier (vm)
      if (localPet == 0) then
        call date_and_time (time=time)
        write (ESMF_UtilIOStdout,*)  &
          time(1:2), ':', time(3:4), ':', time(5:), ': ', text
        call ESMF_UtilIOUnitflush (ESMF_UtilIOStdout)
      end if
      call ESMF_VMBarrier (vm)
#else
      call ESMF_LogWrite(trim(text), ESMF_LOGMSG_DEBUG, rc=rc)
#endif
    end if

    if (present (multitext)) then
#if 0
      write (ESMF_UtilIOStdout,*) multitext
      call ESMF_UtilIOUnitFlush (ESMF_UtilIOStdout)
      call ESMF_VMBarrier (vm)
#else
      call ESMF_LogWrite(trim(multitext), ESMF_LOGMSG_DEBUG, rc=rc)
#endif
    end if

    if (localask) then
      if (localPet == 0) then
        write (ESMF_UtilIOStdout,'(a)') 'Proceed?'
        call ESMF_UtilIOUnitFlush (ESMF_UtilIOStdout)
        read (ESMF_UtilIOStdin,'(a)') answer
      end if
      call ESMF_VMBarrier (vm)
    end if

  end subroutine ESMF_ReconcileDebugPrint

  pure function iTos_len (i)
    integer, intent(in) :: i
    integer :: iTos_len

    character(16) :: string

    write (string,'(i16)') i
    iTos_len = len_trim (adjustl (string))

  end function iTos_len

  function iTos (i)
    integer, intent(in) :: i
    character(iTos_len (i)) :: iTos

    character(16) :: string

    write (string,'(i16)') i
    iTos = adjustl (string)

  end function iTos

end module ESMF_StateReconcileMod
