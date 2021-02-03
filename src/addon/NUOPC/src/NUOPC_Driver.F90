! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define FILENAME "src/addon/NUOPC/src/NUOPC_Driver.F90"
!==============================================================================

module NUOPC_Driver

  !-----------------------------------------------------------------------------
  ! Generic Driver Component
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_RunSequenceDef
  use NUOPC_Connector, only: cplSS => SetServices, NUOPC_ConnectorSet
  
  implicit none
  
  private
  
  public &
    SetVM, &
    SetServices, &
    routine_Run

  public &
    label_ModifyInitializePhaseMap, &
    label_ModifyCplLists, &
    label_SetModelServices, &
    label_SetRunSequence, &
    label_ExecuteRunSequence, &
    label_Finalize, &
    label_SetRunClock
  
  character(*), parameter :: &
    label_InternalState = "Driver_InternalState"
  character(*), parameter :: &
    label_SetModelServices = "Driver_SetModelServices"
  character(*), parameter :: &
    label_SetRunSequence = "Driver_SetRunSequence"
  character(*), parameter :: &
    label_ExecuteRunSequence = "Driver_ExecuteRunSequence"
  character(*), parameter :: &
    label_ModifyInitializePhaseMap = "Driver_ModifyInitializePhaseMap"
  character(*), parameter :: &
    label_ModifyCplLists = "Driver_ModifyCplLists"
  character(*), parameter :: &
    label_Finalize = "Driver_Finalize"
  character(*), parameter :: &
    label_SetRunClock = "Driver_SetRunClock"

  type type_InternalStateStruct
    integer                           :: modelCount
    ! - static references to child components
    type(ESMF_GridComp), pointer      :: modelComp(:)
    type(ESMF_Clock),    pointer      :: initClock(:)
    type(ESMF_State),    pointer      :: modelIS(:), modelES(:)
    type(ESMF_PtrInt1D), pointer      :: modelPetLists(:)
    type(ESMF_CplComp),  pointer      :: connectorComp(:,:)
    type(ESMF_PtrInt1D), pointer      :: connectorPetLists(:,:)
    ! - dynamic references to child components
    type(ESMF_Container)              :: componentMap
    type(ESMF_Container)              :: connectorMap
    ! - run sequence
    type(NUOPC_RunSequence), pointer  :: runSeq(:)  ! size may increase dynamic.
    integer                           :: runPhaseToRunSeqMap(10)
    ! - clock
    type(ESMF_Clock)                  :: parentClock  ! clock of the parent
    ! - temporary variables
    type(type_PhaseMapParser), pointer:: modelPhaseMap(:)
    type(type_PhaseMapParser), pointer:: connectorPhaseMap(:,:)
    ! - flags
    logical                           :: firstTimeDataInit
    logical                           :: dataDepAllComplete
    logical                           :: legacyReady
  end type

  type type_InternalState
    type(type_InternalStateStruct), pointer :: wrap
  end type
  
  type type_PhaseMapParser
    integer                                            :: phaseCount
    integer, pointer                                   :: phaseValue(:)
    character(len=NUOPC_PhaseMapStringLength), pointer :: phases(:)
    character(len=NUOPC_PhaseMapStringLength), pointer :: phaseKey(:)
  end type
  
  ! Generic methods
  public NUOPC_DriverAddComp
  public NUOPC_DriverAddRunElement
  public NUOPC_DriverEgestRunSequence
  public NUOPC_DriverGet
  public NUOPC_DriverGetComp
  public NUOPC_DriverIngestRunSequence
  public NUOPC_DriverNewRunSequence
  public NUOPC_DriverPrint
  public NUOPC_DriverSetRunSequence
  
  ! interface blocks
  !---------------------------------------------
  interface NUOPC_DriverAddComp
    module procedure NUOPC_DriverAddGridComp
    module procedure NUOPC_DriverAddGridCompSO
    module procedure NUOPC_DriverAddCplComp
  end interface
  !---------------------------------------------
  interface NUOPC_DriverAddRunElement
    module procedure NUOPC_DriverAddRunElementMPL
    module procedure NUOPC_DriverAddRunElementCPL
    module procedure NUOPC_DriverAddRunElementL
  end interface
  !---------------------------------------------
  interface NUOPC_DriverGetComp
    module procedure NUOPC_DriverGetGridComp
    module procedure NUOPC_DriverGetCplComp
    module procedure NUOPC_DriverGetAllGridComp
    module procedure NUOPC_DriverGetAllCplComp
  end interface
  
  ! Internal drived types
  !---------------------------------------------
  type ComponentMapEntryT
    character(len=400)              :: label
    type(ESMF_GridComp)             :: component
    integer, pointer                :: petList(:)
  end type
  type ComponentMapEntry
    type(ComponentMapEntryT), pointer :: wrap
  end type
  !---------------------------------------------
  type ConnectorMapEntryT
    character(len=400)              :: label
    type(ESMF_CplComp)              :: connector
    integer, pointer                :: petList(:)
  end type
  type ConnectorMapEntry
    type(ConnectorMapEntryT), pointer :: wrap
  end type
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  recursive subroutine SetVM(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS
  
  end subroutine

  !-----------------------------------------------------------------------------
  
  recursive subroutine SetServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc
    
    ! local variables
    character(ESMF_MAXSTR):: name

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! add standard NUOPC GridComp Attribute Package to the Model
    call NUOPC_CompAttributeInit(driver, kind="Driver", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

#ifndef NO_COMP_SPECIFIC_COMPLIANCE_CHECK
    ! set the ESMF compliance checker register Attribute
    call ESMF_AttributeSet(driver, name="ESMF_RUNTIME_COMPLIANCEICREGISTER", &
      value="NUOPC_Driver_ComplianceICR", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
#endif

    ! Initialize phases

    ! Phase 0 requires use of ESMF method.
    call ESMF_GridCompSetEntryPoint(driver, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! Explicitly claim initialize phase 1 to be able to call into Driver
    ! simply via a single ESMF_GridCompInitialize() from the application level.
    call ESMF_GridCompSetEntryPoint(driver, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP1, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! - upward implement IPDv02:
    call NUOPC_CompSetEntryPoint(driver, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv02p1"/), userRoutine=InitializeIPDv02p1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(driver, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv02p3"/), userRoutine=InitializeIPDv02p3, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(driver, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv02p5"/), userRoutine=InitializeIPDv02p5, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! - upward implement External IPD:
    call NUOPC_CompSetEntryPoint(driver, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/label_ExternalAdvertise/), &
      userRoutine=InitializeExternalAdvertise, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(driver, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/label_ExternalRealize/), &
      userRoutine=InitializeIPDv02p3, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(driver, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/label_ExternalDataInit/), &
      userRoutine=InitializeIPDv02p5, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! Run phases
    call NUOPC_CompSetEntryPoint(driver, ESMF_METHOD_RUN, &
      phaseLabelList=(/"RunPhase1"/), userRoutine=routine_Run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! Run specialization
    call NUOPC_CompSpecialize(driver, specLabel=label_SetRunClock, &
      specRoutine=SetRunClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
      
    ! Run specialization
    call NUOPC_CompSpecialize(driver, specLabel=label_ExecuteRunSequence, &
      specRoutine=ExecuteRunSequence, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
      
    ! Finalize phases
    call NUOPC_CompSetEntryPoint(driver, ESMF_METHOD_FINALIZE, &
      phaseLabelList=(/"FinalizePhase1"/), userRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(driver, ESMF_METHOD_FINALIZE, &
      phaseLabelList=(/label_ExternalReset/), userRoutine=FinalizeReset, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
      
    ! - downward implement IPDv05:
    call NUOPC_CompSetInternalEntryPoint(driver, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv05p1"/), userRoutine=IInitAdvertise, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call NUOPC_CompSetInternalEntryPoint(driver, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv05p2"/), userRoutine=IInitAdvertiseFinish, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call NUOPC_CompSetInternalEntryPoint(driver, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv05p3"/), userRoutine=IInitModifyCplLists, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call NUOPC_CompSetInternalEntryPoint(driver, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv05p4"/), userRoutine=IInitCheck, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call NUOPC_CompSetInternalEntryPoint(driver, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv05p6"/), userRoutine=IInitRealize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call NUOPC_CompSetInternalEntryPoint(driver, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv05p8"/), userRoutine=InternalInitializeComplete, &
      rc=rc)  ! Using IPDv > 02 here forces inward dependency resolution loop
              ! even if none of the child components use IPDv02 or higher.
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
          
    ! Set IPDvX attribute
    call NUOPC_CompAttributeSet(driver, name="IPDvX", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

  end subroutine
  
  !-----------------------------------------------------------------------------

  recursive subroutine InitializeP0(driver, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: driver
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    
    ! local variables
    character(*), parameter   :: rName="InitializeP0"
    character(ESMF_MAXSTR)    :: name
    integer                   :: verbosity, profiling
    character(ESMF_MAXSTR)    :: ipdvxAttr

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(driver, name=name, verbosity=verbosity, &
      profiling=profiling, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle profiling
    if (btest(profiling,9)) then
      call ESMF_TraceRegionEnter("Leading Barrier", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_VMBarrier(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_TraceRegionExit("Leading Barrier", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    if (btest(profiling,0)) then
      call ESMF_TraceRegionEnter(rName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    ! intro
    call NUOPC_LogIntro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! determine whether the component is compatible with IPDvX
    call NUOPC_CompAttributeGet(driver, name="IPDvX", value=ipdvxAttr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
#if 0
    call ESMF_LogWrite("ipdvxAttr: "//ipdvxAttr, ESMF_LOGMSG_DEBUG, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
#endif

    ! NOOP, because only single IPD version entry points are being used by
    ! this implementation on both the upward and downward sides. 
    ! -> No explicit filtering of phaseLabels needed here.

    ! extro
    call NUOPC_LogExtro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle profiling
    if (btest(profiling,0)) then
      call ESMF_TraceRegionExit(rName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

  end subroutine
  
  !-----------------------------------------------------------------------------

  recursive subroutine InitializeP1(driver, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: driver
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    
    ! local variables
    character(*), parameter   :: rName="InitializeP1"
    character(ESMF_MAXSTR)    :: name
    integer                   :: verbosity, profiling
    type(type_InternalState)  :: is
    logical                   :: isSet

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(driver, name=name, verbosity=verbosity, &
      profiling=profiling, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle profiling
    if (btest(profiling,9)) then
      call ESMF_TraceRegionEnter("Leading Barrier", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_VMBarrier(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_TraceRegionExit("Leading Barrier", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    if (btest(profiling,0)) then
      call ESMF_TraceRegionEnter(rName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    ! intro
    call NUOPC_LogIntro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! check if HierarchyProtocol attribute was set
    call NUOPC_CompAttributeGet(driver, name="HierarchyProtocol", isSet=isSet, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    if (.not.isSet) then
      ! turn hierarchy support to connect outside NUOPC
      call NUOPC_CompAttributeSet(driver, &
        name="HierarchyProtocol", value="ConnectProvidedFields", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif

    ! call the actual initialize routines
    call InitializeIPDv02p1(driver, importState, exportState, clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call InitializeIPDv02p3(driver, importState, exportState, clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call InitializeIPDv02p5(driver, importState, exportState, clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! check for dead-lock condition in hierarchical data dependency resolution
    if (.not.is%wrap%dataDepAllComplete) then
      ! this indicates a dead-lock condition
      call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
        msg="Initialize data-dependency resolution loop "// &
        "has entered a dead-lock situation.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
      return  ! bail out
    endif

    ! extro
    call NUOPC_LogExtro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle profiling
    if (btest(profiling,0)) then
      call ESMF_TraceRegionExit(rName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

  end subroutine
  
  !-----------------------------------------------------------------------------

  recursive subroutine InitializeExternalAdvertise(driver, importState, &
    exportState, clock, rc)
    type(ESMF_GridComp)   :: driver
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    
    ! local variables
    character(*), parameter   :: rName="InitializeExternalAdvertise"
    character(ESMF_MAXSTR)    :: name
    integer                   :: verbosity, profiling
    type(type_InternalState)  :: is
    logical                   :: isSet

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(driver, name=name, verbosity=verbosity, &
      profiling=profiling, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle profiling
    if (btest(profiling,9)) then
      call ESMF_TraceRegionEnter("Leading Barrier", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_VMBarrier(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_TraceRegionExit("Leading Barrier", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    if (btest(profiling,0)) then
      call ESMF_TraceRegionEnter(rName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    ! intro
    call NUOPC_LogIntro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! check if HierarchyProtocol attribute was set
    call NUOPC_CompAttributeGet(driver, name="HierarchyProtocol", isSet=isSet, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    if (.not.isSet) then
      ! turn hierarchy support to connect outside NUOPC
      call NUOPC_CompAttributeSet(driver, &
        name="HierarchyProtocol", value="ConnectProvidedFields", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif

    ! call the actual initialize routine
    call InitializeIPDv02p1(driver, importState, exportState, clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      
    ! extro
    call NUOPC_LogExtro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle profiling
    if (btest(profiling,0)) then
      call ESMF_TraceRegionExit(rName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

  end subroutine
  
  !-----------------------------------------------------------------------------

  recursive subroutine InitializeIPDv02p1(driver, importState, exportState, &
    clock, rc)
    type(ESMF_GridComp)  :: driver
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    character(*), parameter   :: rName="InitializeIPDv02p1"
    integer                   :: userrc, stat
    type(type_InternalState)  :: is
    logical                   :: clockIsPresent
    type(ESMF_Clock)          :: internalClock
    integer                   :: i, j, k, l, cIndex
    character(ESMF_MAXSTR)    :: iString, jString, lString
    character(ESMF_MAXSTR)    :: compName, stateName
    integer, pointer          :: i_petList(:), j_petList(:), c_petList(:)
    logical                   :: existflag
    logical                   :: clockIsCreated
    logical                   :: stateIsCreated
    logical                   :: areServicesSet
    logical                   :: needConnector
    integer                   :: rootPet, rootVas
    type(ESMF_VM)             :: vm
    character(ESMF_MAXSTR)    :: name, valueString
    character(ESMF_MAXSTR)    :: msgString, pLabel
    integer                   :: phase
    integer                   :: verbosity, vInherit, profiling
    character(len=10)         :: vString
    character(len=400)        :: namespace  ! long engough for component label
    type(ComponentMapEntry)   :: cmEntry
    type(ESMF_GridComp), pointer :: compList(:)
    type(ESMF_CplComp)        :: connector
    character(len=80)         :: srcCompLabel
    character(len=80)         :: dstCompLabel
    character(len=80)         :: hierarchyProtocol
    type(ESMF_PtrInt1D), pointer :: petLists(:)
    integer, pointer          :: petList(:)

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(driver, name=name, verbosity=verbosity, &
      profiling=profiling, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! handle profiling
    if (btest(profiling,9)) then
      call ESMF_TraceRegionEnter("Leading Barrier", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_VMBarrier(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_TraceRegionExit("Leading Barrier", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    if (btest(profiling,0)) then
      call ESMF_TraceRegionEnter(rName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    ! intro
    call NUOPC_LogIntro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle verbosity
    if (btest(verbosity,8)) then
      call ESMF_GridCompGet(driver, currentPhase=phase, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
      call NUOPC_CompSearchRevPhaseMap(driver, ESMF_METHOD_INITIALIZE, &
        phaseIndex=phase, phaseLabel=pLabel, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      if (len_trim(pLabel)==0) pLabel="<none>"
      call ESMF_GridCompGet(driver, clockIsPresent=clockIsPresent, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
      if (clockIsPresent) then
        call ESMF_GridCompGet(driver, clock=internalClock, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) &
          return  ! bail out
        call ESMF_ClockPrint(internalClock, options="currTime", &
          preString=">>>"//trim(name)//&
          ": entered Initialize (phase="//trim(adjustl(pLabel))// &
          ") with current time: ", unit=msgString, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      else
        write(msgString,"(A)") ">>>"//trim(name)//&
          ": entered Initialize (phase="//trim(adjustl(pLabel))// &
          ") without valid internal Clock."
      endif
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    ! allocate memory for the internal state and set it in the Component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal state memory failed.", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call ESMF_UserCompSetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! test whether internal Clock has already been set in the Component
    call ESMF_GridCompGet(driver, clockIsPresent=clockIsPresent, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    clockIsCreated = ESMF_ClockIsCreated(clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    if (.not.clockIsPresent .and. clockIsCreated) then
      ! set the internal Clock as a copy of the incoming Clock by a default
      call NUOPC_CompSetClock(driver, clock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif
    
    ! prepare members in the internal state
    is%wrap%firstTimeDataInit  = .true.
    is%wrap%dataDepAllComplete = .true.
    is%wrap%componentMap = ESMF_ContainerCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call ESMF_ContainerGarbageOn(is%wrap%componentMap, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    is%wrap%modelCount = 0 ! reset
    is%wrap%connectorMap = ESMF_ContainerCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call ESMF_ContainerGarbageOn(is%wrap%connectorMap, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! clearly set state of legacy data structure that may be accessed
    is%wrap%legacyReady = .false.
        
    ! SPECIALIZE by calling into attached method to SetServices for modelComps
    if (btest(profiling,1)) then
      call ESMF_TraceRegionEnter("label_SetModelServices")
    endif
    call ESMF_MethodExecute(driver, label=label_SetModelServices, &
      userRc=userrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=userrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (btest(profiling,1)) then
      call ESMF_TraceRegionExit("label_SetModelServices")
    endif
      
    ! --> adding and moving code here that supports legacy data structures
    ! --> for now, but after the SetModelServices has been called, and
    ! --> the modelCount is now known.
    
    ! allocate lists inside the internal state according to modelCount
    allocate(is%wrap%modelPetLists(0:is%wrap%modelCount), &
      is%wrap%connectorPetLists(0:is%wrap%modelCount,0:is%wrap%modelCount), &
      is%wrap%modelComp(0:is%wrap%modelCount), &
      is%wrap%initClock(0:is%wrap%modelCount), &
      is%wrap%modelIS(0:is%wrap%modelCount), &
      is%wrap%modelES(0:is%wrap%modelCount), &
      is%wrap%connectorComp(0:is%wrap%modelCount,0:is%wrap%modelCount), &
      stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal state memory failed.", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
      
    ! nullify all of the petLists
    do i=0, is%wrap%modelCount
      nullify(is%wrap%modelPetLists(i)%ptr)
      do j=0, is%wrap%modelCount
        nullify(is%wrap%connectorPetLists(i,j)%ptr)
      enddo
    enddo
    
    ! allocate PhaseMaps
    allocate(is%wrap%modelPhaseMap(0:is%wrap%modelCount), &
      is%wrap%connectorPhaseMap(0:is%wrap%modelCount, 0:is%wrap%modelCount), &
      stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of temporary data structure.", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! associated modelComps and create their import and export States + connectorComps
    nullify(compList, petLists)
    call NUOPC_DriverGetComp(driver, compList, petLists, rc=rc)    
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! determine whether this driver is plugged into a parent NUOPC driver or not
    call NUOPC_CompAttributeGet(driver, name="CompLabel", &
      value=srcCompLabel, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (trim(srcCompLabel)=="_uninitialized") then
      ! this driver is not plugged into a NUOPC parent
      ! the ESMF component name to be used as NUOPC CompLabel
      call ESMF_GridCompGet(driver, name=srcCompLabel, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return
      call NUOPC_CompAttributeSet(driver, name="CompLabel", &
        value=trim(srcCompLabel), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return
    endif
    
    do i=0, is%wrap%modelCount
      write (iString, *) i
      
      nullify(i_petList)
      if (i==0) then
      
        is%wrap%modelComp(0) = driver     ! driver itself is in slot 0
        is%wrap%modelIS(0) = importState  ! driver import State
        is%wrap%modelES(0) = exportState  ! driver export State
        
      else if (i>0) then
      
        is%wrap%modelPetLists(i)%ptr => petLists(i)%ptr
        i_petList => is%wrap%modelPetLists(i)%ptr

        ! for now put a component alias into the legacy data structure until all
        ! dependencies have been removed
        is%wrap%modelComp(i) = compList(i) ! set the alias
        
        ! for now create the States here ... in the long run may be moved?
        
        stateName = "modelComp "//trim(adjustl(iString))//" Import State"
        
        if (btest(verbosity,14)) then
          write (msgString,"(A)") trim(name)//&
            " - Creating state: "//trim(stateName)
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
        endif

        is%wrap%modelIS(i) = ESMF_StateCreate(name=trim(stateName), &
          stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out

        stateName = "modelComp "//trim(adjustl(iString))//" Export State"
        
        if (btest(verbosity,14)) then
          write (msgString,"(A)") trim(name)//&
            " - Creating state: "//trim(stateName)
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
        endif

        is%wrap%modelES(i) = ESMF_StateCreate(name=trim(stateName), &
          stateintent=ESMF_STATEINTENT_EXPORT, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        
      endif
      
      ! set rootVas Attribute on the States to help during AttributeUpdate
      ! should be done for driver-self and models
      rootPet = 0   ! initialize
      if (associated(i_petList)) rootPet = i_petList(1)
      ! need to translate rootPet->rootVas because connector petList may
      ! scamble PETs across VASs
      call ESMF_GridCompGet(driver, vm=vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_VMGet(vm, rootPet, vas=rootVas, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      stateIsCreated = ESMF_StateIsCreated(is%wrap%modelIS(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return
      if (stateIsCreated) then
        call ESMF_AttributeSet(is%wrap%modelIS(i), name="rootVas", &
          value=rootVas, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
      endif
      stateIsCreated = ESMF_StateIsCreated(is%wrap%modelES(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return
      if (stateIsCreated) then
        call ESMF_AttributeSet(is%wrap%modelES(i), name="rootVas", &
          value=rootVas, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif

      ! initialize the modelPhaseMap pointer members
      nullify(is%wrap%modelPhaseMap(i)%phaseValue)
      nullify(is%wrap%modelPhaseMap(i)%phases)
      nullify(is%wrap%modelPhaseMap(i)%phaseKey)
        
      ! associate connectorComps
      do j=0, is%wrap%modelCount
        write (jString, *) j
        ! for now put a component alias into the legacy data structure until all
        ! dependencies have been removed
        if (i==0) then
          ! driver-self
          call NUOPC_CompAttributeGet(driver, name="CompLabel", &
            value=srcCompLabel, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        else
          ! actual child component
          call NUOPC_CompAttributeGet(compList(i), name="CompLabel", &
            value=srcCompLabel, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        endif
        if (j==0) then
          ! driver-self
          call NUOPC_CompAttributeGet(driver, name="CompLabel", &
            value=dstCompLabel, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        else
          call NUOPC_CompAttributeGet(compList(j), name="CompLabel", &
            value=dstCompLabel, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        endif

        ! now look up the associated connector, okay if it is not a valid object
        ! invalid components are detectable by the CompAreServicesSet() method
        call NUOPC_DriverGetComp(driver, srcCompLabel, dstCompLabel, &
          comp=connector, petList=petList, relaxedflag=.true., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        
        ! potentially the connector must be created here to/from driver-self
        call NUOPC_CompAttributeGet(driver, name="HierarchyProtocol", &
          isSet=needConnector, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        needConnector=.not.needConnector  ! default to do connection
        if (.not.needConnector) then
          ! inspect the HierarchyProtocol attribute to see if it requests a
          ! connection
          call NUOPC_CompAttributeGet(driver, name="HierarchyProtocol", &
            value=hierarchyProtocol, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (trim(hierarchyProtocol)=="PushUpAllExportsAndUnsatisfiedImports" &
            .or. trim(hierarchyProtocol)=="ConnectProvidedFields" &
            .or. trim(hierarchyProtocol)=="Explorer") then
            needConnector = .true.
          endif
        endif
        areServicesSet = NUOPC_CompAreServicesSet(connector, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        if (.not.areServicesSet.and.(i==0.or.j==0)) then
          ! the connector was not added by the user level code SetModelServices
          ! and this involves the driver itself -> maybe automatic connector add
          if (.not.(i==0.and.j==0)) then
            ! not a driver-to-driver-self connection, which has no known purpose
            if (i==0) then
              needConnector = needConnector.and. &
                ESMF_StateIsCreated(is%wrap%modelIS(i), rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return
            else
              ! j==0
              needConnector = needConnector.and. &
                ESMF_StateIsCreated(is%wrap%modelES(j), rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return
            endif
            if (needConnector) then
              ! driver import or export States exist and connection requested
              ! -> automatic connector add
              call NUOPC_DriverAddComp(driver, &
                srcCompLabel=srcCompLabel, dstCompLabel=dstCompLabel, &
                compSetServicesRoutine=cplSS, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail
              ! retrieve the component with petList
              call NUOPC_DriverGetComp(driver, srcCompLabel, dstCompLabel, &
                comp=connector, petList=petList, relaxedflag=.true., rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail
              ! automatically created connectors inherit Verbosity from parent
              call NUOPC_CompAttributeGet(driver, name="Verbosity", &
                value=valueString, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail
              if (trim(valueString)=="max" .or. trim(valueString)=="high" .or. &
                trim(valueString)=="low" .or. trim(valueString)=="off") then
                ! directly inherit presets
                vString = trim(valueString)
              else
                ! not a preset level: lower 8-bit of parent's verbosity setting
                vInherit = ibits(verbosity,0,8)
                write(vString,"(I10)") vInherit
              endif
              if (btest(verbosity,13)) then
                write (msgString,"(A)") trim(name)//&
                  " - Setting Verbosity on created component to: "// &
                  trim(vString)
                call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, &
                  file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                  return  ! bail out
              endif
              call NUOPC_CompAttributeSet(connector, name="Verbosity", &
                value=vString, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail
              ! automatically created connectors inherit Profiling from parent
              call NUOPC_CompAttributeGet(driver, name="Profiling", &
                value=valueString, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail
              if (trim(valueString)=="max" .or. trim(valueString)=="high" .or. &
                trim(valueString)=="low" .or. trim(valueString)=="off") then
                ! directly inherit presets
                vString = trim(valueString)
              else
                ! not a preset level: lower 16-bit of parent's profiling setting
                vInherit = ibits(profiling,0,16)
                write(vString,"(I10)") vInherit
              endif
              if (btest(verbosity,13)) then
                write (msgString,"(A)") trim(name)//&
                  " - Setting Profiling on created component to: "// &
                  trim(vString)
                call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, &
                  file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                  return  ! bail out
              endif
              call NUOPC_CompAttributeSet(connector, name="Profiling", &
                value=vString, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail
            endif
          endif
        endif
        
        is%wrap%connectorComp(i,j) = connector ! set the alias
        is%wrap%connectorPetLists(i,j)%ptr => petList
        
        ! initialize the connectorPhaseMap pointer members
        nullify(is%wrap%connectorPhaseMap(i,j)%phaseValue)
        nullify(is%wrap%connectorPhaseMap(i,j)%phases)
        nullify(is%wrap%connectorPhaseMap(i,j)%phaseKey)
      enddo
    enddo

    deallocate(compList, petLists)

    ! query Component for its Clock (set during specialization)
    call ESMF_GridCompGet(driver, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    
    ! initialize the default Run Sequence...
    nullify(is%wrap%runSeq) ! initialize
    is%wrap%runPhaseToRunSeqMap = 0 ! initialize
    
    ! add one run sequence element
    call NUOPC_RunSequenceAdd(is%wrap%runSeq, 1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! set the upper most slot runClock as alias of Driver internalClock
    call NUOPC_RunSequenceSet(is%wrap%runSeq(1), internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! map run phase 1 to first run sequence element
    is%wrap%runPhaseToRunSeqMap(1) = 1
    
    ! add run elements to the run sequence
    ! ... 1st block: connectors between all of the model components
    do i=1, is%wrap%modelCount
      do j=1, is%wrap%modelCount
        if (j==i) cycle ! skip self connection
        call NUOPC_RunElementAdd(is%wrap%runSeq(1), i=i, j=j, phase=1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      enddo
    enddo
    ! ... 2nd block: all of the model components
    do i=1, is%wrap%modelCount
      call NUOPC_RunElementAdd(is%wrap%runSeq(1), i=i, j=-1, phase=1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      ! Store a reference if the clock to be used during model initialize
      is%wrap%initClock(i) = is%wrap%runSeq(1)%clock
    enddo

    ! now the component labels are available -> create States with Namespace
    do i=0, is%wrap%modelCount
      if (i > 0) then
        ! have component label available for namespace
        call ESMF_ContainerGetUDTByIndex(is%wrap%componentMap, i, &
          cmEntry, ESMF_ITEMORDER_ADDORDER, rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        namespace=cmEntry%wrap%label
      else
        ! in the old style (pre v7) there are no component labels availabl
        namespace="DEFAULT" ! cannot be empty for sake of AttributeSet()
      endif
      ! add State level attributes, set the namespace according to comp label
      stateIsCreated = ESMF_StateIsCreated(is%wrap%modelIS(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return
      if (stateIsCreated) then
        call NUOPC_InitAttributes(is%wrap%modelIS(i), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        call NUOPC_SetAttribute(is%wrap%modelIS(i), &
          name="Namespace", value=trim(namespace), &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
      ! add State level attributes, set the namespace according to comp label
      stateIsCreated = ESMF_StateIsCreated(is%wrap%modelES(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return
      if (stateIsCreated) then
        call NUOPC_InitAttributes(is%wrap%modelES(i), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        call NUOPC_SetAttribute(is%wrap%modelES(i), &
          name="Namespace", value=trim(namespace), &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
    enddo
      
    ! -> NUOPC Initialize Sequence requires presence of InitP0 for every 
    ! -> Model and Connector component, where they must set the
    ! -> "InitializePhaseMap" metadata.
    ! InitP0: modelComps  -> before SetRunSequence because RunPhaseMap needed
    call loopModelComps(phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! InitP0: connectorComps -> before SetRunSequence b/c RunPhaseMap needed
    call loopConnectorComps(phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    ! clearly set state of legacy data structure that may be accessed
    is%wrap%legacyReady = .true.

    ! SPECIALIZE by calling into optional attached method that sets RunSequence
    if (btest(profiling,1)) then
      call ESMF_TraceRegionEnter("label_SetRunSequence")
    endif
    call ESMF_MethodExecute(driver, label=label_SetRunSequence, &
      existflag=existflag, userRc=userrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=userrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (btest(profiling,1)) then
      call ESMF_TraceRegionExit("label_SetRunSequence")
    endif

    ! SPECIALIZE by calling into optional attached method allowing modification
    ! of the "InitializePhaseMap" metadata.
    if (btest(profiling,1)) then
      call ESMF_TraceRegionEnter("label_ModifyInitializePhaseMap")
    endif
    call ESMF_MethodExecute(driver, label=label_ModifyInitializePhaseMap, &
      existflag=existflag, userRc=userrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=userrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (btest(profiling,1)) then
      call ESMF_TraceRegionExit("label_ModifyInitializePhaseMap")
    endif
    
    ! Ingest the InitializePhaseMap
    do i=0, is%wrap%modelCount
      areServicesSet = &
        NUOPC_CompAreServicesSet(is%wrap%modelComp(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      if (areServicesSet) then
        ! setup modelPhaseMap
        call setupModelPhaseMap(i=i, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
      do j=0, is%wrap%modelCount
        areServicesSet = &
          NUOPC_CompAreServicesSet(is%wrap%connectorComp(i,j), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        if (areServicesSet) then
          ! setup connectorPhaseMap
          call setupConnectorPhaseMap(i=i, j=j, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
        endif
      enddo
    enddo

    ! -> Encode the NUOPC IPDv00, IPDv01, IPDv02, IPDv03, IPDv04, IPDv05, IPDvX

    ! modelComps
    call loopModelCompsS(driver, phaseString="IPDv00p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(driver, phaseString="IPDv01p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(driver, phaseString="IPDv02p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(driver, phaseString="IPDv03p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(driver, phaseString="IPDv04p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(driver, phaseString="IPDv05p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(driver, phaseString="IPDvXp01", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! connectorComps
    call loopConnectorCompsS(driver, phaseString="IPDv05p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(driver, phaseString="IPDvXp01", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    ! modelComps (new for IPDv05)
    call loopModelCompsS(driver, phaseString="IPDv05p2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(driver, phaseString="IPDvXp02", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! connectorComps
    call loopConnectorCompsS(driver, phaseString="IPDv00p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(driver, phaseString="IPDv01p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(driver, phaseString="IPDv02p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(driver, phaseString="IPDv03p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(driver, phaseString="IPDv04p1a", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(driver, phaseString="IPDv04p1b", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! warning - this ordering only works (with the two above) if we
    ! prevent mixing IPD versions in the same connector
    call loopConnectorCompsS(driver, phaseString="IPDv05p2a", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(driver, phaseString="IPDv05p2b", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(driver, phaseString="IPDvXp02a", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(driver, phaseString="IPDvXp02b", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    ! modelComps
    ! moved down one level
    !call loopModelCompsS(driver, phaseString="IPDv00p2", rc=rc)
    !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    !  line=__LINE__, file=trim(name)//":"//FILENAME)) &
    !  return  ! bail out
    call loopModelCompsS(driver, phaseString="IPDv01p2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(driver, phaseString="IPDv02p2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(driver, phaseString="IPDv03p2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(driver, phaseString="IPDv04p2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(driver, phaseString="IPDv05p3", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(driver, phaseString="IPDvXp03", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! connectorComps
    call loopConnectorCompsS(driver, phaseString="IPDv00p2a", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(driver, phaseString="IPDv01p2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(driver, phaseString="IPDv02p2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(driver, phaseString="IPDv03p2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(driver, phaseString="IPDv04p2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(driver, phaseString="IPDv05p3", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(driver, phaseString="IPDvXp03", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
      
    ! Before returning the driver must clean up its own importState, which 
    ! may have Fields advertised that do not have a ConsumerConnection set.
    ! These are Fields that during the negotiation between driver children
    ! were mirrored into the driver importState, but then subsequently were
    ! resolved among the children themselves (sibling-to-sibling). Therefore
    ! they should not remain in the parent importState. Leaving them in the
    ! parent State, while not connected with a child anylonger, would lead to
    ! issues during GeomTransfer.
    stateIsCreated = ESMF_StateIsCreated(importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    if (stateIsCreated) then
      ! call into routine that removes fields without ConsumerConnection set
      call rmFieldsWoConsumerConnection(importState, name=name, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif

    ! handle verbosity
    if (btest(verbosity,8)) then
      call ESMF_GridCompGet(driver, clockIsPresent=clockIsPresent, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
      if (clockIsPresent) then
        call ESMF_GridCompGet(driver, clock=internalClock, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) &
          return  ! bail out
        call ESMF_ClockPrint(internalClock, options="currTime", &
          preString="<<<"//trim(name)//&
          ": leaving Initialize (phase="//trim(adjustl(pLabel))// &
          ") with current time: ", unit=msgString, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      else
        write(msgString,"(A)") "<<<"//trim(name)//&
          ": leaving Initialize (phase="//trim(adjustl(pLabel))// &
          ") without valid internal Clock."
      endif
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    ! extro
    call NUOPC_LogExtro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle profiling
    if (btest(profiling,0)) then
      call ESMF_TraceRegionExit(rName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    contains !----------------------------------------------------------------
    
      recursive subroutine loopModelComps(phase, rc)
        ! only to be used for phase=0
        integer, intent(in)     :: phase
        integer, intent(out)    :: rc
        integer                 :: i
        logical                 :: areServicesSet
        character(ESMF_MAXSTR)  :: iString, pLabel
        rc = ESMF_SUCCESS
        do i=1, is%wrap%modelCount
          write (iString, *) i
          areServicesSet = &
            NUOPC_CompAreServicesSet(is%wrap%modelComp(i), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          if (areServicesSet) then
            if (phase==0) then
              pLabel="0"
            else
              call NUOPC_CompSearchRevPhaseMap(is%wrap%modelComp(i), &
                ESMF_METHOD_INITIALIZE, phaseIndex=phase, phaseLabel=pLabel, &
                rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return
            endif
            call ESMF_GridCompGet(is%wrap%modelComp(i), name=compName, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
            call ESMF_GridCompInitialize(is%wrap%modelComp(i), &
              importState=is%wrap%modelIS(i), exportState=is%wrap%modelES(i),&
              clock=is%wrap%initClock(i), phase=phase, userRc=userrc, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg="NUOPC Incompatible: "//&
              "Failed calling phase "// &
              trim(adjustl(pLabel))//" Initialize for modelComp "// &
              trim(adjustl(iString))//": "//trim(compName), &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
            if (ESMF_LogFoundError(rcToCheck=userrc, msg="Phase '"// &
              trim(adjustl(pLabel))//"' Initialize for modelComp "// &
              trim(adjustl(iString))//": "//trim(compName)// &
              " did not return ESMF_SUCCESS", &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
            ! need to update the Component attributes across all PETs
            if (associated(is%wrap%modelPetLists(i)%ptr)) then
              call ESMF_AttributeUpdate(is%wrap%modelComp(i), vm, &
                rootList=is%wrap%modelPetLists(i)%ptr(1:1), rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            else
              call ESMF_AttributeUpdate(is%wrap%modelComp(i), vm, &
                rootList=(/0/), rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
          endif
        enddo
      end subroutine

      recursive subroutine loopConnectorComps(phase, rc)
        ! only to be used for phase=0
        integer, intent(in)     :: phase
        integer, intent(out)    :: rc
        integer                 :: i, j
        logical                 :: areServicesSet
        character(ESMF_MAXSTR)  :: iString, jString, pLabel
        type(ESMF_State)        :: imState, exState

        rc = ESMF_SUCCESS
        do i=0, is%wrap%modelCount
          write (iString, *) i
          do j=0, is%wrap%modelCount
            write (jString, *) j
            areServicesSet = &
              NUOPC_CompAreServicesSet(is%wrap%connectorComp(i,j), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return
            if (areServicesSet) then
              if (phase==0) then
                pLabel="0"
              else
                call NUOPC_CompSearchRevPhaseMap(is%wrap%connectorComp(i,j), &
                  ESMF_METHOD_INITIALIZE, phaseIndex=phase, phaseLabel=pLabel, &
                  rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//FILENAME)) return
              endif
              if (i==0) then
                ! connect to the driver's import State
                imState=is%wrap%modelIS(0)
              else
                imState=is%wrap%modelES(i)
              endif
              if (j==0) then
                ! connect to the driver's export State
                exState=is%wrap%modelES(0)
              else
                exState=is%wrap%modelIS(j)
              endif
              call ESMF_CplCompGet(is%wrap%connectorComp(i,j), name=compName, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
              call ESMF_CplCompInitialize(is%wrap%connectorComp(i,j), &
                importState=imState, exportState=exState, &
                clock=internalClock, phase=phase, userRc=userrc, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase "// &
                trim(adjustl(pLabel))//" Initialize for connectorComp "// &
                trim(adjustl(iString))//" -> "//trim(adjustl(jString))//": "// &
                trim(compName), &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
              if (ESMF_LogFoundError(rcToCheck=userrc, msg="Phase '"// &
                trim(adjustl(pLabel))//"' Initialize for connectorComp "// &
                trim(adjustl(iString))//" -> "//trim(adjustl(jString))//": "// &
                trim(compName)//" did not return ESMF_SUCCESS", &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
              ! need to update the Component attributes across all PETs
              if (associated(is%wrap%connectorPetLists(i,j)%ptr)) then
                call ESMF_AttributeUpdate(is%wrap%connectorComp(i,j), vm, &
                  rootList=is%wrap%connectorPetLists(i,j)%ptr(1:1), rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc))&
                  return  ! bail out
              else
                call ESMF_AttributeUpdate(is%wrap%connectorComp(i,j), vm, &
                  rootList=(/0/), rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc))&
                  return  ! bail out
              endif
            endif
          enddo
        enddo
      end subroutine

      recursive subroutine setupModelPhaseMap(i, rc)
        integer, intent(in)     :: i
        integer, intent(out)    :: rc
        integer                 :: k, phaseCount, stat, ind
        character(len=NUOPC_PhaseMapStringLength) :: tempString
        character(len=40)        :: attributeName
        rc = ESMF_SUCCESS
        ! set the attributeName according to who this is for
        if (i==0) then
          ! for the driver itself
          attributeName = "InternalInitializePhaseMap"
        else
          ! for the children of the driver
          attributeName = "InitializePhaseMap"
        endif
        ! obtain number of initPhases from the Model Attributes
        call NUOPC_CompAttributeGet(is%wrap%modelComp(i), &
          name=trim(attributeName), itemCount=phaseCount, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) &
          return  ! bail out
        ! allocate pointer variables
        is%wrap%modelPhaseMap(i)%phaseCount = phaseCount
        allocate(is%wrap%modelPhaseMap(i)%phases(phaseCount), &
          is%wrap%modelPhaseMap(i)%phaseValue(phaseCount), &
          is%wrap%modelPhaseMap(i)%phaseKey(phaseCount), &
          stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of temporary data structure.", &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        ! conditionally obtain initPhases list from the Model Attributes
        if (phaseCount > 0) then
          call NUOPC_CompAttributeGet(is%wrap%modelComp(i), &
            name=trim(attributeName), &
            valueList=is%wrap%modelPhaseMap(i)%phases, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) &
            return  ! bail out
        endif
        
        ! disect the phase string into Key and Value
        do k=1, is%wrap%modelPhaseMap(i)%phaseCount
          tempString = is%wrap%modelPhaseMap(i)%phases(k)
          ind = index(trim(tempString), "=")
          is%wrap%modelPhaseMap(i)%phaseKey(k) = tempString(1:ind-1)
          read (tempString(ind+1:len(tempString)), "(i4)") &
            is%wrap%modelPhaseMap(i)%phaseValue(k)
!    print *, "setupModelPhaseMap", k, ":", trim(tempString), " ", &
!    trim(is%wrap%modelPhaseMap(i)%phaseKey(k)), &
!    is%wrap%modelPhaseMap(i)%phaseValue(k)
        enddo
      end subroutine

      recursive subroutine setupConnectorPhaseMap(i, j, rc)
        integer, intent(in)     :: i, j
        integer, intent(out)    :: rc
        integer                 :: k, phaseCount, stat, ind
        character(len=NUOPC_PhaseMapStringLength) :: tempString
        rc = ESMF_SUCCESS
        ! obtain number of initPhases from the Model Attributes
        call NUOPC_CompAttributeGet(is%wrap%connectorComp(i,j), &
          name="InitializePhaseMap", itemCount=phaseCount, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) &
          return  ! bail out
        ! allocate pointer variables
        is%wrap%connectorPhaseMap(i,j)%phaseCount = phaseCount
        allocate(is%wrap%connectorPhaseMap(i,j)%phases(phaseCount), &
          is%wrap%connectorPhaseMap(i,j)%phaseValue(phaseCount), &
          is%wrap%connectorPhaseMap(i,j)%phaseKey(phaseCount), &
          stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of temporary data structure.", &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        ! obtain initPhases list from the Model Attributes
        call NUOPC_CompAttributeGet(is%wrap%connectorComp(i,j), &
          name="InitializePhaseMap", &
          valueList=is%wrap%connectorPhaseMap(i,j)%phases, &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) &
          return  ! bail out
        ! disect the phase string into Key and Value
        do k=1, is%wrap%connectorPhaseMap(i,j)%phaseCount
          tempString = is%wrap%connectorPhaseMap(i,j)%phases(k)
          ind = index(trim(tempString), "=")
          is%wrap%connectorPhaseMap(i,j)%phaseKey(k) = tempString(1:ind-1)
          read (tempString(ind+1:len(tempString)), "(i4)") &
            is%wrap%connectorPhaseMap(i,j)%phaseValue(k)
!print *, "setupConnectorPhaseMap", k, ":", trim(tempString), " ", &
!  trim(connectorPhaseMap(i,j)%phaseKey(k)), connectorPhaseMap(i,j)%phaseValue(k)
        enddo
      end subroutine
      
  end subroutine
  
  !-----------------------------------------------------------------------------

  recursive subroutine InitializeIPDv02p3(driver, importState, exportState, &
    clock, rc)
    type(ESMF_GridComp)  :: driver
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    character(*), parameter   :: rName="InitializeIPDv02p3"
    character(ESMF_MAXSTR)    :: name
    integer                   :: verbosity, profiling
    type(ESMF_Clock)          :: internalClock
    logical                   :: clockIsPresent
    character(ESMF_MAXSTR)    :: msgString, pLabel
    integer                   :: phase

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(driver, name=name, verbosity=verbosity, &
      profiling=profiling, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle profiling
    if (btest(profiling,9)) then
      call ESMF_TraceRegionEnter("Leading Barrier", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_VMBarrier(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_TraceRegionExit("Leading Barrier", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    if (btest(profiling,0)) then
      call ESMF_TraceRegionEnter(rName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    ! intro
    call NUOPC_LogIntro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle verbosity
    if (btest(verbosity,8)) then
      call ESMF_GridCompGet(driver, currentPhase=phase, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
      call NUOPC_CompSearchRevPhaseMap(driver, ESMF_METHOD_INITIALIZE, &
        phaseIndex=phase, phaseLabel=pLabel, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      if (len_trim(pLabel)==0) pLabel="<none>"
      call ESMF_GridCompGet(driver, clockIsPresent=clockIsPresent, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
      if (clockIsPresent) then
        call ESMF_GridCompGet(driver, clock=internalClock, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) &
          return  ! bail out
        call ESMF_ClockPrint(internalClock, options="currTime", &
          preString=">>>"//trim(name)//&
          ": entered Initialize (phase="//trim(adjustl(pLabel))// &
          ") with current time: ", unit=msgString, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      else
        write(msgString,"(A)") ">>>"//trim(name)//&
          ": entered Initialize (phase="//trim(adjustl(pLabel))// &
          ") without valid internal Clock."
      endif
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    ! connectorComps
    call loopConnectorCompsS(driver, phaseString="IPDv00p2b", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    ! modelComps
    call loopModelCompsS(driver, phaseString="IPDv00p2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(driver, phaseString="IPDv01p3", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(driver, phaseString="IPDv02p3", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(driver, phaseString="IPDv03p3", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(driver, phaseString="IPDv04p3", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(driver, phaseString="IPDv05p4", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(driver, phaseString="IPDvXp04", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! connectorComps
    call loopConnectorCompsS(driver, phaseString="IPDv03p3", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(driver, phaseString="IPDv04p3", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(driver, phaseString="IPDv05p4", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(driver, phaseString="IPDvXp04", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    ! modelComps
    call loopModelCompsS(driver, phaseString="IPDv03p4", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(driver, phaseString="IPDv04p4", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(driver, phaseString="IPDv05p5", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(driver, phaseString="IPDvXp05", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! connectorComps
    call loopConnectorCompsS(driver, phaseString="IPDv03p4", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(driver, phaseString="IPDv04p4", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(driver, phaseString="IPDv05p5", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(driver, phaseString="IPDvXp05", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    ! modelComps
    call loopModelCompsS(driver, phaseString="IPDv03p5", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(driver, phaseString="IPDv04p5", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(driver, phaseString="IPDv05p6", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(driver, phaseString="IPDvXp06", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! connectorComps
    call loopConnectorCompsS(driver, phaseString="IPDv01p3a", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(driver, phaseString="IPDv01p3b", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(driver, phaseString="IPDv02p3a", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(driver, phaseString="IPDv02p3b", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(driver, phaseString="IPDv03p5a", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(driver, phaseString="IPDv03p5b", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(driver, phaseString="IPDv04p5a", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(driver, phaseString="IPDv04p5b", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(driver, phaseString="IPDv05p6a", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(driver, phaseString="IPDv05p6b", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(driver, phaseString="IPDvXp06a", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(driver, phaseString="IPDvXp06b", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    ! modelComps
    call loopModelCompsS(driver, phaseString="IPDv00p3", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(driver, phaseString="IPDv01p4", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(driver, phaseString="IPDv02p4", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(driver, phaseString="IPDv03p6", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(driver, phaseString="IPDv04p6", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(driver, phaseString="IPDv05p7", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(driver, phaseString="IPDvXp07", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! connectorComps
    ! nothing to do

    ! handle verbosity
    if (btest(verbosity,8)) then
      call ESMF_GridCompGet(driver, clockIsPresent=clockIsPresent, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
      if (clockIsPresent) then
        call ESMF_GridCompGet(driver, clock=internalClock, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) &
          return  ! bail out
        call ESMF_ClockPrint(internalClock, options="currTime", &
          preString="<<<"//trim(name)//&
          ": leaving Initialize (phase="//trim(adjustl(pLabel))// &
          ") with current time: ", unit=msgString, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      else
        write(msgString,"(A)") "<<<"//trim(name)//&
          ": leaving Initialize (phase="//trim(adjustl(pLabel))// &
          ") without valid internal Clock."
      endif
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    ! extro
    call NUOPC_LogExtro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle profiling
    if (btest(profiling,0)) then
      call ESMF_TraceRegionExit(rName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

  end subroutine
  
  !-----------------------------------------------------------------------------

  recursive subroutine InitializeIPDv02p5(driver, importState, exportState, &
    clock, rc)
    type(ESMF_GridComp)   :: driver
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    
    ! local variables
    character(*), parameter   :: rName="InitializeIPDv02p5"
    integer                   :: userrc
    type(ESMF_Clock)          :: internalClock
    logical                   :: clockIsPresent
    logical                   :: clockIsCreated
    type(ESMF_Time)           :: currTime
    character(ESMF_MAXSTR)    :: oldDataComplete, newDataComplete
    integer                   :: oldUpdatedCount, newUpdatedCount
    logical                   :: allUpdated
    logical                   :: stateIsCreated
    character(ESMF_MAXSTR)    :: name
    integer                   :: verbosity, profiling
    character(ESMF_MAXSTR)    :: msgString, pLabel
    integer                   :: phase

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(driver, name=name, verbosity=verbosity, &
      profiling=profiling, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle profiling
    if (btest(profiling,9)) then
      call ESMF_TraceRegionEnter("Leading Barrier", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_VMBarrier(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_TraceRegionExit("Leading Barrier", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    if (btest(profiling,0)) then
      call ESMF_TraceRegionEnter(rName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    ! intro
    call NUOPC_LogIntro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle verbosity
    if (btest(verbosity,8)) then
      call ESMF_GridCompGet(driver, currentPhase=phase, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
      call NUOPC_CompSearchRevPhaseMap(driver, ESMF_METHOD_INITIALIZE, &
        phaseIndex=phase, phaseLabel=pLabel, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      if (len_trim(pLabel)==0) pLabel="<none>"
      call ESMF_GridCompGet(driver, clockIsPresent=clockIsPresent, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
      if (clockIsPresent) then
        call ESMF_GridCompGet(driver, clock=internalClock, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) &
          return  ! bail out
        call ESMF_ClockPrint(internalClock, options="currTime", &
          preString=">>>"//trim(name)//&
          ": entered Initialize (phase="//trim(adjustl(pLabel))// &
          ") with current time: ", unit=msgString, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      else
        write(msgString,"(A)") ">>>"//trim(name)//&
          ": entered Initialize (phase="//trim(adjustl(pLabel))// &
          ") without valid internal Clock."
      endif
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    ! if the incoming clock is valid, then use to set currTime on internalClock
    clockIsCreated = ESMF_ClockIsCreated(clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (clockIsCreated) then
      ! reset the currTime of the internalClock
      call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      call ESMF_GridCompGet(driver, clock=internalClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
      call ESMF_ClockSet(internalClock, currTime=currTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif

    ! see if this driver was called with a valid exportState
    stateIsCreated = ESMF_StateIsCreated(exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    if (stateIsCreated) then
      ! check how many Fields in the exportState have "Updated" set
      ! to "true" BEFORE calling the DataInitialize
      allUpdated = NUOPC_IsUpdated(exportState, count=oldUpdatedCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      ! get the value of the "InitializeDataComplete" attribute
      call NUOPC_CompAttributeGet(driver, name="InitializeDataComplete", &
        value=oldDataComplete, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
      ! optionally log info
      if (btest(verbosity,11)) then
        write(msgString, "(A,l,A,I4)") trim(name)//&
          ": InitializeDataComplete='"//trim(oldDataComplete)//&
          "', allUpdated=", allUpdated, ", updatedCount=", oldUpdatedCount
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      endif
    else
      ! no valid exportState -> top level driver
      if (btest(verbosity,11)) then
        call ESMF_LogWrite(trim(name)//&
          ": this is a top level driver, "// &
          "no 'Updated' and 'InitializeDataComplete' info on this level.", &
          ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      endif
    endif

    ! calling into the DataInitialize could be implemented via MethodExecute,
    ! as for NUOPC_Model, but currently there seems to be no reason for this.
    ! -> call directly into the subroutine
    if (btest(profiling,1)) then
      call ESMF_TraceRegionEnter("InitializeIPDv02p5Data")
    endif
    call InitializeIPDv02p5Data(driver, importState, exportState, clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    if (btest(profiling,1)) then
      call ESMF_TraceRegionExit("InitializeIPDv02p5Data")
    endif

    ! see if this driver was called with a valid exportState
    stateIsCreated = ESMF_StateIsCreated(exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    if (stateIsCreated) then
      ! re-set the "InitializeDataProgress" attribute to "false"
      call NUOPC_CompAttributeSet(driver, &
        name="InitializeDataProgress", value="false", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      ! check how many Fields in the exportState have "Updated" set
      ! to "true" AFTER calling the DataInitialize
      allUpdated = NUOPC_IsUpdated(exportState, count=newUpdatedCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      ! see if number of updated export fields went up
      if (newUpdatedCount > oldUpdatedCount) then
        ! there are more Fields now that have "Updated" set "true"
        ! -> set "InitializeDataProgress" attribute "true"
        call NUOPC_CompAttributeSet(driver, &
          name="InitializeDataProgress", value="true", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
      endif
      ! get the value of the "InitializeDataComplete" attribute
      call NUOPC_CompAttributeGet(driver, name="InitializeDataComplete", &
        value=newDataComplete, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
      ! see if the "InitializeDataComplete" attribute has changed
      if (trim(newDataComplete) /= trim(oldDataComplete)) then
        ! there was a change in the "InitializeDataComplete" attribute setting
        ! -> set "InitializeDataProgress" attribute "true"
        call NUOPC_CompAttributeSet(driver, &
          name="InitializeDataProgress", value="true", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
      endif
    
      ! optionally log info
      if (btest(verbosity,11)) then
        write(msgString, "(A,l,A,I4)") trim(name)//&
          ": InitializeDataComplete='"//trim(newDataComplete)//&
          "', allUpdated=", allUpdated, ", updatedCount=", newUpdatedCount
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      endif

      ! correct setting of timestamps
      call ESMF_GridCompGet(driver, clock=internalClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
      if (allUpdated) then
        ! update timestamp on all the export Fields
        call NUOPC_SetTimestamp(exportState, internalClock, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) &
          return  ! bail out
      else
        ! update timestamp on only those export Fields that have the 
        ! "Updated" attribute set to "true"
        call NUOPC_SetTimestamp(exportState, internalClock, &
          selective=.true., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) &
          return  ! bail out
      endif
    endif

    ! handle verbosity
    if (btest(verbosity,8)) then
      call ESMF_GridCompGet(driver, clockIsPresent=clockIsPresent, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
      if (clockIsPresent) then
        call ESMF_GridCompGet(driver, clock=internalClock, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) &
          return  ! bail out
        call ESMF_ClockPrint(internalClock, options="currTime", &
          preString="<<<"//trim(name)//&
          ": leaving Initialize (phase="//trim(adjustl(pLabel))// &
          ") with current time: ", unit=msgString, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      else
        write(msgString,"(A)") "<<<"//trim(name)//&
          ": leaving Initialize (phase="//trim(adjustl(pLabel))// &
          ") without valid internal Clock."
      endif
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    ! extro
    call NUOPC_LogExtro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle profiling
    if (btest(profiling,0)) then
      call ESMF_TraceRegionExit(rName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  recursive subroutine InitializeIPDv02p5Data(driver, importState, exportState,&
    clock, rc)
    type(ESMF_GridComp)  :: driver
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    character(*), parameter   :: rName="InitializeIPDv02p5Data"
    type(type_InternalState)  :: is
    type(ESMF_VM)             :: vm
    character(ESMF_MAXSTR)    :: name, valueString
    integer                   :: verbosity, profiling
    logical                   :: execFlag, execFlagCollect
    integer                   :: execFlagIntReduced, execFlagInt
    character(len=160)        :: msgString

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(driver, name=name, verbosity=verbosity, &
      profiling=profiling, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle profiling
    if (btest(profiling,9)) then
      call ESMF_TraceRegionEnter("Leading Barrier", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_VMBarrier(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_TraceRegionExit("Leading Barrier", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    if (btest(profiling,0)) then
      call ESMF_TraceRegionEnter(rName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    ! intro
    call NUOPC_LogIntro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! get the vm
    call ESMF_GridCompGet(driver, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! modelComps
    if (is%wrap%firstTimeDataInit) then
      ! IPDv < 02 data initialize phase only called once
      call loopModelCompsS(driver, phaseString="IPDv00p4", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
      call loopModelCompsS(driver, phaseString="IPDv01p5", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
      is%wrap%firstTimeDataInit=.false. ! set guard flag for next time
    endif
    execFlagCollect = .false.
    call loopModelCompsS(driver, phaseString="IPDv02p5", execFlag=execFlag, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    execFlagCollect = execFlagCollect.or.execFlag
    call loopModelCompsS(driver, phaseString="IPDv03p7", execFlag=execFlag, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    execFlagCollect = execFlagCollect.or.execFlag
    call loopModelCompsS(driver, phaseString="IPDv04p7", execFlag=execFlag, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    execFlagCollect = execFlagCollect.or.execFlag
    call loopModelCompsS(driver, phaseString="IPDv05p8", execFlag=execFlag, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    execFlagCollect = execFlagCollect.or.execFlag
    call loopModelCompsS(driver, phaseString="IPDvXp08", execFlag=execFlag, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    execFlagCollect = execFlagCollect.or.execFlag

    ! deal with the fact that the executing component may not be across all PETs
    execFlagInt = 0
    if (execFlagCollect) execFlagInt = 1

    call ESMF_VMAllFullReduce(vm, sendData=(/execFlagInt/), &
      recvData=execFlagIntReduced, count=1, reduceflag=ESMF_REDUCE_SUM, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    if (execFlagIntReduced>0) execFlag = .true.
      
    ! now all PETs have the same execFlag setting for a consistent decision
    if (execFlag) then
      ! there were model components with IPDv02p5, IPDv03p7, IPDv04p7, 
      ! IPDv05p8, or IPDvXp08 -->> resolve data dependencies by entering loop
      if (btest(verbosity,11)) then
        call ESMF_LogWrite(trim(name)//&
          ": components present that trigger loopDataDependentInitialize().", &
          ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      endif
      
      call loopDataDependentInitialize(driver, is%wrap%dataDepAllComplete, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out

      if (btest(verbosity,11)) then
        write(msgString, "(A,l)") trim(name)//&
          ": loopDataDependentInitialize() returned with dataDepAllComplete: ",&
          is%wrap%dataDepAllComplete
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      endif
    endif
    
    ! set the InitializeDataComplete attribute
    valueString="false"
    if (is%wrap%dataDepAllComplete) valueString="true"
    
    call NUOPC_CompAttributeSet(driver, &
      name="InitializeDataComplete", value=valueString, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! extro
    call NUOPC_LogExtro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle profiling
    if (btest(profiling,0)) then
      call ESMF_TraceRegionExit(rName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  recursive subroutine loopModelCompsS(driver, phaseString, execFlag, rc)
    ! only to be used for phase>0
    type(ESMF_GridComp)     :: driver
    character(*), intent(in):: phaseString
    logical, intent(out), optional :: execFlag ! .true. if at least one executed
    integer, intent(out)    :: rc
    ! local variables
    integer                 :: phase, i, k, userrc
    character(ESMF_MAXSTR)  :: iString, pLabel
    type(type_InternalState):: is
    character(ESMF_MAXSTR)  :: name, compName
    type(ESMF_Clock)        :: internalClock
    logical                 :: internalflag
    logical                 :: areServicesSet
    ! initialize out arguments
    rc = ESMF_SUCCESS
    if (present(execFlag)) execFlag = .false.
    ! query the component for info
    call NUOPC_CompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    ! query Component for clock
    call ESMF_GridCompGet(driver, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    ! loop through all the model components
    do i=0, is%wrap%modelCount
      write (iString, *) i
      areServicesSet = &
        NUOPC_CompAreServicesSet(is%wrap%modelComp(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      if (areServicesSet) then
        ! translate NUOPC logical phase to ESMF actual phase
        phase = 0 ! zero is reserved, use it here to see if need to skip
        do k=1, is%wrap%modelPhaseMap(i)%phaseCount
          if (trim(is%wrap%modelPhaseMap(i)%phaseKey(k)) &
            == trim(phaseString)) &
            phase = is%wrap%modelPhaseMap(i)%phaseValue(k)
        enddo
        if (phase == 0) cycle ! skip to next i
        if (i==0) then
          internalflag=.true.
        else
          internalflag=.false.
        endif
        call NUOPC_CompSearchRevPhaseMap(is%wrap%modelComp(i), &
          ESMF_METHOD_INITIALIZE, internalflag=internalflag, &
          phaseIndex=phase, phaseLabel=pLabel, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        ! attempt to make the actual call to initialize
        call ESMF_GridCompGet(is%wrap%modelComp(i), name=compName, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        call ESMF_GridCompInitialize(is%wrap%modelComp(i), &
          importState=is%wrap%modelIS(i), exportState=is%wrap%modelES(i), &
          clock=is%wrap%initClock(i), phase=phase, userRc=userrc, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase '"// &
          trim(adjustl(pLabel))//"' Initialize for modelComp "// &
          trim(adjustl(iString))//": "//trim(compName), &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        if (ESMF_LogFoundError(rcToCheck=userrc, msg="Phase '"// &
          trim(adjustl(pLabel))//"' Initialize for modelComp "// &
          trim(adjustl(iString))//": "//trim(compName)// &
          " did not return ESMF_SUCCESS", &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        if (present(execFlag)) execFlag = .true. ! at least this model executed
        if (.not.internalflag) then
          ! Ensure that Attributes are consistent across all the PETs of the
          ! component that just executed.
          call consistentComponentAttributes(is%wrap%modelComp(i), &
            is%wrap%modelPetLists(i)%ptr, name, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        endif
      endif
    enddo
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine consistentComponentAttributes(comp, petList, name, rc)
    type(ESMF_GridComp)     :: comp
    integer, pointer        :: petList(:)
    character(*)            :: name
    integer, intent(out)    :: rc
    ! local variables
    type(ESMF_VM)           :: vm
    ! initialize out arguments
    rc = ESMF_SUCCESS

    !TODO: The Update() is only needed if there are child PETs that are
    !TODO: going to pause for PE-reuse via user level threading. Figure
    !TODO: out how to detect this, and make Update() call conditional.

    !TODO: Should be calling with all master PETs (those processes that go
    !TODO: on to execute child code), for better Update() performance. For
    !TODO: now just call with first PET as root, because that always will
    !TODO: work (because first PET always is passed to child component).

    call ESMF_VMGetCurrent(vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (associated(petList)) then
      ! use the petList to restrict the number of PETs across which the
      ! update is synchronizing
      call ESMF_AttributeUpdate(comp, vm, &
        rootList=petList(1:1), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    else
      ! no petList was specified -> update across all PETs
      call ESMF_AttributeUpdate(comp, vm, &
        rootList=(/0/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  recursive subroutine loopConnectorCompsS(driver, phaseString, execFlag, rc)
    ! only to be used for phase>0
    type(ESMF_GridComp)     :: driver
    character(*), intent(in):: phaseString
    logical, intent(out), optional :: execFlag ! .true. if at least one executed
    integer, intent(out)    :: rc
    ! local variables
    integer                 :: phase, i, ii, j, k, userrc
    character(ESMF_MAXSTR)  :: iString, jString, pLabel
    type(ESMF_State)        :: imState, exState
    type(type_InternalState):: is
    character(ESMF_MAXSTR)  :: name, compName
    type(ESMF_Clock)        :: internalClock
    logical                 :: areServicesSet
    ! initialize out arguments
    rc = ESMF_SUCCESS
    if (present(execFlag)) execFlag = .false.
    ! query the component for info
    call NUOPC_CompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    ! query Component for clock
    call ESMF_GridCompGet(driver, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    ! loop through all the model components
    do ii=1, is%wrap%modelCount+1
      i=mod(ii,is%wrap%modelCount+1)
      write (iString, *) i
      do j=0, is%wrap%modelCount
        write (jString, *) j
        areServicesSet = &
          NUOPC_CompAreServicesSet(is%wrap%connectorComp(i,j), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        if (areServicesSet) then
          ! translate NUOPC logical phase to ESMF actual phase
          phase = 0 ! zero is reserved, use it here to see if need to skip
          do k=1, is%wrap%connectorPhaseMap(i,j)%phaseCount
            if (trim(is%wrap%connectorPhaseMap(i,j)%phaseKey(k)) &
              == trim(phaseString)) &
              phase = is%wrap%connectorPhaseMap(i,j)%phaseValue(k)
          enddo
          if (phase == 0) cycle ! skip to next j
          call NUOPC_CompSearchRevPhaseMap(is%wrap%connectorComp(i,j), &
            ESMF_METHOD_INITIALIZE, phaseIndex=phase, phaseLabel=pLabel, &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (i==0) then
            ! connect to the drivers import State
            call ESMF_GridCompGet(driver, importState=imState, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) &
              return  ! bail out
          else
            imState=is%wrap%modelES(i)
          endif
          if (j==0) then
            ! connect to the drivers export State
            call ESMF_GridCompGet(driver, exportState=exState, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) &
              return  ! bail out
          else
            exState=is%wrap%modelIS(j)
          endif
          ! attempt to make the actual call to initialize
          call ESMF_CplCompGet(is%wrap%connectorComp(i,j), name=compName, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          call ESMF_CplCompInitialize(is%wrap%connectorComp(i,j), &
            importState=imState, exportState=exState, &
            clock=internalClock, phase=phase, userRc=userrc, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase '"// &
            trim(adjustl(pLabel))//"' Initialize for connectorComp "// &
            trim(adjustl(iString))//" -> "//trim(adjustl(jString))//": "// &
            trim(compName), &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          if (ESMF_LogFoundError(rcToCheck=userrc, msg="Phase '"// &
            trim(adjustl(pLabel))//"' Initialize for connectorComp "// &
            trim(adjustl(iString))//" -> "//trim(adjustl(jString))//": "// &
            trim(compName)//" did not return ESMF_SUCCESS", &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          if (present(execFlag)) execFlag = .true. ! at least this connector executed for phaseString
        endif
      enddo
    enddo
  end subroutine

  !-----------------------------------------------------------------------------

  recursive subroutine loopDataDependentInitialize(driver, dataDepAllComplete, rc)
    ! resolve data dependencies
    type(ESMF_GridComp)             :: driver
    logical, optional, intent(out)  :: dataDepAllComplete
    integer, intent(out)            :: rc
    
    ! local variables
    character(*), parameter         :: rName="loopDataDependentInitialize"
    integer                         :: phase, i, j, k, cphase, userrc
    character(ESMF_MAXSTR)          :: iString, jString, pString, valueString
    character(ESMF_MAXSTR)          :: cpString
    type(ESMF_State)                :: imState, exState
    logical                         :: allComplete, someProgress
    logical                         :: gridCompIsPetLocal
    logical                         :: areModelServicesSet
    logical                         :: areConnectorServicesSet
    integer                         :: petCount
    integer                         :: helperIn, helperOut
    type(type_InternalState)        :: is
    character(ESMF_MAXSTR)          :: name, compName
    type(ESMF_Clock)                :: internalClock
    type(ESMF_VM)                   :: vm
    integer                         :: verbosity, profiling
    character(len=160)              :: msgString

    ! initialize out arguments
    rc = ESMF_SUCCESS
    
    ! query the component for info
    call NUOPC_CompGet(driver, name=name, verbosity=verbosity, &
      profiling=profiling, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle profiling
    if (btest(profiling,9)) then
      call ESMF_TraceRegionEnter("Leading Barrier", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_VMBarrier(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_TraceRegionExit("Leading Barrier", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    if (btest(profiling,0)) then
      call ESMF_TraceRegionEnter(rName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    ! intro
    call NUOPC_LogIntro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! get the vm, etc.
    call ESMF_GridCompGet(driver, vm=vm, petCount=petCount, &
      clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    ! data-dependency resolution loop
    do
      allComplete = .true.    ! prime -> one that isn't complete can toggle
      someProgress = .false.  ! prime -> one that made progress can toggle
      
      ! loop through all the model components
      do i=0, is%wrap%modelCount
        write (iString, *) i
        areModelServicesSet = &
          NUOPC_CompAreServicesSet(is%wrap%modelComp(i), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) &
          return  ! bail out
        if (areModelServicesSet) then
          
          call ESMF_GridCompGet(is%wrap%modelComp(i), name=compName, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out

          ! translate NUOPC logical phase to ESMF actual phase
          phase = 0 ! zero is reserved, use it here to see if need to skip
          do k=1, is%wrap%modelPhaseMap(i)%phaseCount
            if ((trim(is%wrap%modelPhaseMap(i)%phaseKey(k))==trim("IPDv02p5")).or. &
              (trim(is%wrap%modelPhaseMap(i)%phaseKey(k)) == trim("IPDv03p7")).or. &
              (trim(is%wrap%modelPhaseMap(i)%phaseKey(k)) == trim("IPDv04p7")).or. &
              (trim(is%wrap%modelPhaseMap(i)%phaseKey(k)) == trim("IPDv05p8")).or. &
              (trim(is%wrap%modelPhaseMap(i)%phaseKey(k)) == trim("IPDvXp08"))) then
              phase = is%wrap%modelPhaseMap(i)%phaseValue(k)
            endif
          enddo
          
          ! make sure there is a consistent view across all PETs
          call ESMF_VMAllFullReduce(vm, sendData=(/phase/), &
            recvData=helperOut, count=1, reduceflag=ESMF_REDUCE_SUM, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) &
            return  ! bail out
          
          if (helperOut == 0) cycle ! skip to next i since no phase found
          write (pString, *) phase
          
          ! check model InitializeDataComplete Attribute to see if complete
          call NUOPC_CompAttributeGet(is%wrap%modelComp(i), &
            name="InitializeDataComplete", value=valueString, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) &
            return  ! bail out
          
          if (btest(verbosity,11)) then
            write(msgString, "(A,I4,A)") &
              trim(name)//": component ", i, "="//trim(compName)//&
              ", dataComplete (local): "//trim(valueString)
            call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          endif

          ! preconditioned input variables considering petList of component
          helperIn = 1  ! initialize
          gridCompIsPetLocal = ESMF_GridCompIsPetLocal(is%wrap%modelComp(i), &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) &
            return  ! bail out
          if (gridCompIsPetLocal) then
            ! evaluate "InitializeDataComplete" on PETs in petList
            if (trim(valueString)=="false") helperIn = 0
          endif

          ! implement a logical AND operation based on REDUCE_SUM
          call ESMF_VMAllFullReduce(vm, sendData=(/helperIn/), &
            recvData=helperOut, count=1, reduceflag=ESMF_REDUCE_SUM, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) &
            return  ! bail out

          if (btest(verbosity,11)) then
            write(msgString, "(A,I4,A,L)") &
              trim(name)//": component ", i, "="//trim(compName)//&
              ", dataComplete (global): ", (helperOut==petCount)
            call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          endif

          if (helperOut == petCount) cycle ! skip to next i
          allComplete = .false. ! hit toggles -> prevents exit on outer loop
          
          ! else try to Run() all of the Connectors to model i
          cphase = 1  ! for now assume Run() only does phase 1
          do j=0, is%wrap%modelCount
            areConnectorServicesSet = &
              NUOPC_CompAreServicesSet(is%wrap%connectorComp(j,i), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) &
              return  ! bail out
            if (areConnectorServicesSet) then
              write (jString, *) j
              write (cpString, *) cphase
              if (j==0) then
                ! connect to the drivers import State
                call ESMF_GridCompGet(driver, importState=imState, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//FILENAME)) &
                  return  ! bail out
              else
                imState=is%wrap%modelES(j)
              endif
              if (i==0) then
                ! connect to the drivers export State
                call ESMF_GridCompGet(driver, exportState=exState, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//FILENAME)) &
                  return  ! bail out
              else
                exState=is%wrap%modelIS(i)
              endif
              call ESMF_CplCompRun(is%wrap%connectorComp(j,i), &
                importState=imState, exportState=exState, &
                clock=internalClock, phase=cphase, userRc=userrc, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, &
                msg="Failed calling phase "//trim(adjustl(cpString))// &
                " Run for connectorComp "//trim(adjustl(jString))// &
                " -> "//trim(adjustl(iString)), &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
              if (ESMF_LogFoundError(rcToCheck=userrc,  msg="Phase '"// &
                trim(adjustl(cpString))//"' Run for connectorComp "// &
                trim(adjustl(jString))//" -> "//trim(adjustl(iString))// &
                " did not return ESMF_SUCCESS", &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
          enddo
          
          ! attempt to make the actual call to initialize for model i
          call ESMF_GridCompInitialize(is%wrap%modelComp(i), &
            importState=is%wrap%modelIS(i), exportState=is%wrap%modelES(i), &
            clock=is%wrap%initClock(i), phase=phase, userRc=userrc, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase "// &
            trim(adjustl(pString))//" Initialize for modelComp "// &
            trim(adjustl(iString))//": "//trim(compName), &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          if (ESMF_LogFoundError(rcToCheck=userrc, msg="Phase '"// &
            trim(adjustl(pString))//"' Initialize for modelComp "// &
            trim(adjustl(iString))//": "//trim(compName)// &
            " did not return ESMF_SUCCESS", &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
            
          if (i>0) then
            ! Ensure that Attributes are consistent across all the PETs of the
            ! component that just executed.
            call consistentComponentAttributes(is%wrap%modelComp(i), &
              is%wrap%modelPetLists(i)%ptr, name, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          endif

          ! check model InitializeDataProgress Attribute if progress made
          call NUOPC_CompAttributeGet(is%wrap%modelComp(i), &
            name="InitializeDataProgress", value=valueString, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) &
            return  ! bail out
          
          if (btest(verbosity,11)) then
            write(msgString, "(A,I4,A)") &
              trim(name)//": component ", i, "="//trim(compName)//&
              ", someProgress (local): "//trim(valueString)
            call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          endif

          ! make sure there is a consistent view across all PETs
          helperIn = 0
          if (trim(valueString)=="true") helperIn = 1
          call ESMF_VMAllFullReduce(vm, sendData=(/helperIn/), &
            recvData=helperOut, count=1, reduceflag=ESMF_REDUCE_SUM, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) &
            return  ! bail out
            
          if (helperOut > 0) someProgress=.true. ! toggle flag
            
          if (btest(verbosity,11)) then
            write(msgString, "(A,I4,A,L)") &
              trim(name)//": component ", i, "="//trim(compName)//&
              ", someProgress (global): ", someProgress
            call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          endif

        endif
      enddo
      
      if (present(dataDepAllComplete)) dataDepAllComplete=allComplete

      if (btest(verbosity,11)) then
        write(msgString, "(A,l,A,l,A,l)") &
          trim(name)//": someProgress=", someProgress, ", allComplete=", &
          allComplete, ", present(dataDepAllComplete)=", &
          present(dataDepAllComplete)
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      endif

      ! check if all Components with IPDv02p5 are InitializeDataComplete
      if (allComplete) then
        if (btest(verbosity,11)) then
          call ESMF_LogWrite(trim(name)//&
            ": finished resolving initialize data dependencies on this level.",&
            ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        endif
        exit ! break out of data-dependency resolution loop
      endif
      
      if (.not.someProgress) then
        if (present(dataDepAllComplete)) then
          if (btest(verbosity,11)) then
            call ESMF_LogWrite(trim(name)//&
              ": not finished resolving initialize data dependencies on "//&
              "this level, must return to upper level, and come back.",&
              ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        endif
          exit ! break out of loop
        endif
        ! else, dead-lock situation identified
        call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
          msg="Initialize data-dependency resolution loop "// &
          "has entered a dead-lock situation.", &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
        return  ! bail out of data-dependency resolution loop, prevent lock
      endif
      
    enddo
    
    ! extro
    call NUOPC_LogExtro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle profiling
    if (btest(profiling,0)) then
      call ESMF_TraceRegionExit(rName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  recursive subroutine routine_Run(driver, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: driver
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    character(*), parameter         :: rName="Run"
    integer                         :: userrc
    type(type_InternalState)        :: is
    type(ESMF_Clock)                :: internalClock, activeClock
    logical                         :: existflag
    integer                         :: i, j, phase, runPhase, runSeqIndex
    character(ESMF_MAXSTR)          :: iString, jString, pLabel
    character(ESMF_MAXSTR)          :: msgString, timeString
    type(NUOPC_RunElement), pointer :: runElement
    character(ESMF_MAXSTR)          :: name
    integer                         :: verbosity, profiling
    integer                         :: indentCount
    integer                         :: loopLevel, loopLevelPrev
    integer                         :: levelMember, levelMemberPrev
    integer                         :: loopIteration, loopIterationPrev

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(driver, name=name, verbosity=verbosity, &
      profiling=profiling, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    ! handle profiling
    if (btest(profiling,10)) then
      call ESMF_TraceRegionEnter("Leading Barrier", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_VMBarrier(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_TraceRegionExit("Leading Barrier", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    if (btest(profiling,3)) then
      call ESMF_TraceRegionEnter(rName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    ! intro
    call NUOPC_LogIntro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! query Component for its clock and currentPhase
    call ESMF_GridCompGet(driver, clock=internalClock, currentPhase=runPhase, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! conditionally output info to Log file
    if (btest(verbosity,9)) then
      call NUOPC_CompSearchRevPhaseMap(driver, ESMF_METHOD_RUN, &
        phaseIndex=runPhase, phaseLabel=pLabel, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      call ESMF_ClockPrint(internalClock, options="currTime", &
        preString=">>>"//trim(name)//&
        " entered Run (phase="//trim(adjustl(pLabel))// &
        ") with current time: ", unit=msgString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    
    ! set the driverClock member in the internal state
    ! this is the incoming clock from the parent driver
    ! do this in alignment with model and mediator components
    is%wrap%parentClock = clock
    
    ! SPECIALIZE required: label_SetRunClock
    if (btest(profiling,4)) then
      call ESMF_TraceRegionEnter("label_SetRunClock")
    endif
    ! -> first check for the label with phase index
    call ESMF_MethodExecute(driver, label=label_SetRunClock, index=runPhase, &
      existflag=existflag, userRc=userrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=userrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (.not.existflag) then
      ! -> next check for the label without phase index
      call ESMF_MethodExecute(driver, label=label_SetRunClock, &
        userRc=userrc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      if (ESMF_LogFoundError(rcToCheck=userrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    if (btest(profiling,4)) then
      call ESMF_TraceRegionExit("label_SetRunClock")
    endif
    
    if (btest(verbosity,12)) then
      call ESMF_ClockPrint(internalClock, options="currTime", &
        preString=trim(name)//": after lable_SetRunClock, current time: ", &
        unit=msgString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    ! initialize the activeClock in case any connectors are executed before
    ! the RunSequence executed
    activeClock = internalClock
    
    ! execute all connectors from driver (parent) to its children
    !TODO: see ticket #3614786 about making this connector execution conditional
    i=0       ! from parent
    phase=1   ! use phase 1
    do j=1, is%wrap%modelCount
      call routine_executeCplComp(is, i, j, phase, activeClock, name, &
        userrc=userrc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    enddo

    ! SPECIALIZE required: label_ExecuteRunClock
    if (btest(profiling,4)) then
      call ESMF_TraceRegionEnter("label_ExecuteRunSequence")
    endif
    ! -> first check for the label with phase index
    call ESMF_MethodExecute(driver, label=label_ExecuteRunSequence, &
      index=phase, existflag=existflag, userRc=userrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=userrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (.not.existflag) then
      ! -> next check for the label without phase index
      call ESMF_MethodExecute(driver, label=label_ExecuteRunSequence, &
        userRc=userrc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      if (ESMF_LogFoundError(rcToCheck=userrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    if (btest(profiling,4)) then
      call ESMF_TraceRegionExit("label_ExecuteRunSequence")
    endif

    ! execute all connectors to driver (parent) from its children
    !TODO: see ticket #3614786 about making this connector execution conditional
    j=0       ! to parent
    phase=1   ! use phase 1
    do i=1, is%wrap%modelCount
      call routine_executeCplComp(is, i, j, phase, activeClock, name, &
        userrc=userrc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    enddo
    
    ! conditionally output info to Log file
    if (btest(verbosity,9)) then
      call ESMF_ClockPrint(internalClock, options="currTime", &
        preString="<<<"//trim(name)//&
        " - leaving Run with current time: ", unit=msgString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    ! extro
    call NUOPC_LogExtro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle profiling
    if (btest(profiling,3)) then
      call ESMF_TraceRegionExit(rName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

  end subroutine
  
  !-----------------------------------------------------------------------------
  
  subroutine routine_executeGridComp(is, i, phase, activeClock, name, userrc, rc)
    type(type_InternalState)        :: is
    integer,          intent(in)    :: i, phase
    type(ESMF_Clock), intent(inout) :: activeClock
    character(ESMF_MAXSTR), intent(in) :: name
    integer,          intent(out)   :: userrc, rc
    ! local variables
    logical                         :: areServicesSet
    character(ESMF_MAXSTR)          :: iString, pLabel
    character(ESMF_MAXSTR)          :: msgString
    logical                         :: internalFlag

    areServicesSet = &
      NUOPC_CompAreServicesSet(is%wrap%modelComp(i), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (areServicesSet) then
      write (iString, *) i
      internalFlag = .false.
      if (i==0) internalFlag = .true. ! driver self
      call NUOPC_CompSearchRevPhaseMap(is%wrap%modelComp(i), &
        ESMF_METHOD_RUN, phaseIndex=phase, phaseLabel=pLabel, &
        internalFlag=internalFlag, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

      call ESMF_GridCompRun(is%wrap%modelComp(i), &
        importState=is%wrap%modelIS(i), exportState=is%wrap%modelES(i), &
        clock=activeClock, phase=phase, userRc=userrc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, &
        msg="Failed calling phase "//trim(adjustl(pLabel))// &
        " Run for modelComp "//trim(adjustl(iString)), &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      if (ESMF_LogFoundError(rcToCheck=userrc, msg="Phase '"// &
        trim(adjustl(pLabel))//"' Run for modelComp "// &
        trim(adjustl(iString))//" did not return ESMF_SUCCESS", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

  end subroutine

  !-----------------------------------------------------------------------------
  
  subroutine routine_executeCplComp(is, i, j, phase, activeClock, name, userrc, rc)
    type(type_InternalState)        :: is
    integer,          intent(in)    :: i, j, phase
    type(ESMF_Clock), intent(inout) :: activeClock
    character(ESMF_MAXSTR), intent(in) :: name
    integer,          intent(out)   :: userrc, rc
    ! local variables
    logical                         :: areServicesSet
    character(ESMF_MAXSTR)          :: iString, jString, pLabel
    type(ESMF_State)                :: imState, exState
    character(ESMF_MAXSTR)          :: msgString
    
    areServicesSet = &
      NUOPC_CompAreServicesSet(is%wrap%connectorComp(i,j), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (areServicesSet) then
      write (iString, *) i
      write (jString, *) j
      call NUOPC_CompSearchRevPhaseMap(is%wrap%connectorComp(i,j), &
        ESMF_METHOD_RUN, phaseIndex=phase, phaseLabel=pLabel, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      if (i==0) then
        ! connect to the driver's import State
        imState=is%wrap%modelIS(0)
      else
        imState=is%wrap%modelES(i)
      endif
      if (j==0) then
        ! connect to the driver's export State
        exState=is%wrap%modelES(0)
      else
        exState=is%wrap%modelIS(j)
      endif
      
      call ESMF_CplCompRun(is%wrap%connectorComp(i,j), &
        importState=imState, exportState=exState, &
        clock=activeClock, phase=phase, userRc=userrc, rc=rc)
          
      if (ESMF_LogFoundError(rcToCheck=rc, &
        msg="Failed calling phase "//trim(adjustl(pLabel))// &
        " Run for connectorComp "//trim(adjustl(iString))// &
        " -> "//trim(adjustl(jString)), &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      if (ESMF_LogFoundError(rcToCheck=userrc,  msg="Phase '"// &
        trim(adjustl(pLabel))//"' Run for connectorComp "// &
        trim(adjustl(iString))//" -> "//trim(adjustl(jString))// &
        " did not return ESMF_SUCCESS", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  recursive subroutine SetRunClock(driver, rc)
    type(ESMF_GridComp)   :: driver
    integer, intent(out)  :: rc
    
    ! Set the internal clock according to the incoming driver clock.
    ! Implement the default explicit timekeeping rules.
    
    ! local variables
    character(ESMF_MAXSTR)    :: name
    type(ESMF_Clock)          :: parentClock
    logical                   :: clockIsCreated

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! query component for parent clock
    call NUOPC_DriverGet(driver, parentClock=parentClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! see if the parent clock can be used
    clockIsCreated = ESMF_ClockIsCreated(parentClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    if (clockIsCreated) then
      ! check and set the driver clock against the parent clock
      call NUOPC_CompCheckSetClock(driver, parentClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif
          
  end subroutine
    
  !-----------------------------------------------------------------------------
  
  recursive subroutine ExecuteRunSequence(driver, rc)
    type(ESMF_GridComp)   :: driver
    integer, intent(out)  :: rc
    
    ! Set the internal clock according to the incoming driver clock.
    ! Implement the default explicit timekeeping rules.
    
    ! local variables
    integer                         :: userrc
    type(type_InternalState)        :: is
    character(ESMF_MAXSTR)          :: name
    character(ESMF_MAXSTR)          :: msgString, timeString
    type(NUOPC_RunElement), pointer :: runElement
    type(ESMF_Clock)                :: internalClock, activeClock
    integer                         :: i, j, phase, runPhase, runSeqIndex
    integer                         :: verbosity, profiling
    integer                         :: indentCount
    integer                         :: loopLevel, loopLevelPrev
    integer                         :: levelMember, levelMemberPrev
    integer                         :: loopIteration, loopIterationPrev

    rc = ESMF_SUCCESS

    ! query Component for its clock and currentPhase
    call NUOPC_CompGet(driver, name=name, verbosity=verbosity, &
      profiling=profiling, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! handle profiling
    if (btest(profiling,5)) then
      call ESMF_TraceRegionEnter("ExecuteRunSequence", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    ! query Component for its clock and currentPhase
    call ESMF_GridCompGet(driver, clock=internalClock, currentPhase=runPhase, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! query the component for info
    call NUOPC_CompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! query component for its internal state
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! initialize the activeClock
    activeClock = internalClock

    ! determine the correct run sequence index for the current runPhase    
    runSeqIndex = is%wrap%runPhaseToRunSeqMap(runPhase)
    
    if (btest(verbosity,12)) then
      call ESMF_LogWrite(trim(name)//": begin -------> RunSequence.", &
        ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_LogGet(indentCount=indentCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      call ESMF_LogSet(indentCount=indentCount+2, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif
    
    ! initialize "Prev" variables
    loopLevelPrev = 0
    levelMemberPrev = 0
    loopIterationPrev = 0

    ! use RunSequence iterator to execute the actual time stepping loop
    nullify(runElement) ! prepare runElement for iterator use
    do while (NUOPC_RunSequenceIterate(is%wrap%runSeq, runSeqIndex, &
      runElement, rc=rc))
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out

      if (.not.ESMF_ClockEQAlias(activeClock,runElement%runSeq%clock)) then
        ! set activeClock
        activeClock = runElement%runSeq%clock
      endif

      if (btest(verbosity,12)) then
        loopLevel = runElement%runSeq%loopLevel
        levelMember = runElement%runSeq%levelMember
        loopIteration = runElement%runSeq%loopIteration
        if ((loopLevel/=loopLevelPrev).or.(levelMember/=levelMemberPrev).or.&
          (loopIteration/=loopIterationPrev)) then
          ! found a time loop event -> need to log
          ! update the "Prev' variables
          loopLevelPrev = loopLevel
          levelMemberPrev = levelMember
          loopIterationPrev = loopIteration
          ! write iteration info to Log
          write(msgString,"(A,I4,A,I4,A,I4)") &
            trim(name)//": RunSequence event loopLevel=", &
            runElement%runSeq%loopLevel, "  levelMember=", &
            runElement%runSeq%levelMember, "  loopIteration=", &
            runElement%runSeq%loopIteration
          call ESMF_ClockPrint(activeClock, options="currTime", &
            preString=trim(msgString)//", current time: ", &
            unit=timeString, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          call ESMF_LogWrite(timeString, ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
        endif
      endif
      
      ! now interpret and act on the current runElement
      i = runElement%i
      phase = runElement%phase
      if (runElement%j >= 0) then
        ! connector component: i -> j
        if (btest(profiling,5)) then
          call ESMF_TraceRegionEnter("routine_executeCplComp", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
        endif
        j = runElement%j
        call routine_executeCplComp(is, i, j, phase, activeClock, name, &
          userrc=userrc, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        if (btest(profiling,5)) then
          call ESMF_TraceRegionExit("routine_executeCplComp", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
        endif
      else
        ! model, mediator, or driver component
        if (btest(profiling,5)) then
          call ESMF_TraceRegionEnter("routine_executeGridComp", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
        endif
        call routine_executeGridComp(is, i, phase, activeClock, name, &
          userrc=userrc, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        if (btest(profiling,5)) then
          call ESMF_TraceRegionExit("routine_executeGridComp", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
        endif
      endif

    enddo
    ! check RC of the NUOPC_RunSequenceIterate() call
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    if (btest(verbosity,12)) then
      call ESMF_LogGet(indentCount=indentCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      call ESMF_LogSet(indentCount=indentCount-2, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      write(msgString,"(A)") &
        trim(name)//": end <--------- RunSequence"
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    ! handle profiling
    if (btest(profiling,5)) then
      call ESMF_TraceRegionExit("ExecuteRunSequence", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

  end subroutine
    
  !-----------------------------------------------------------------------------

  recursive subroutine Finalize(driver, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: driver
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    character(*), parameter   :: rName="Finalize"
    integer                   :: urc, stat
    type(type_InternalState)  :: is
    type(ESMF_Clock)          :: internalClock
    integer                   :: i, j, itemCount
    type(ComponentMapEntry)   :: cmEntry
    type(ConnectorMapEntry)   :: cnEntry
    character(ESMF_MAXSTR)    :: iString, jString
    logical                   :: existflag
    logical                   :: areServicesSet
    character(ESMF_MAXSTR)    :: name, compName
    integer                   :: verbosity, profiling
    type(ESMF_GridComp), pointer  :: compList(:)
    type(ESMF_CplComp), pointer   :: connectorList(:)
    type(ESMF_PtrInt1D), pointer  :: petLists(:)
    
    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(driver, name=name, verbosity=verbosity, &
      profiling=profiling, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle profiling
    if (btest(profiling,11)) then
      call ESMF_TraceRegionEnter("Leading Barrier", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_VMBarrier(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_TraceRegionExit("Leading Barrier", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    if (btest(profiling,6)) then
      call ESMF_TraceRegionEnter(rName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    ! intro
    call NUOPC_LogIntro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! query Component for its Clock
    call ESMF_GridCompGet(driver, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! Finalize: connectorComps
    do i=0, is%wrap%modelCount
      write (iString, *) i
      do j=0, is%wrap%modelCount
        write (jString, *) j
        areServicesSet = &
          NUOPC_CompAreServicesSet(is%wrap%connectorComp(i,j), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        if (areServicesSet) then
          if (btest(verbosity,13)) then
            call ESMF_CplCompGet(is%wrap%connectorComp(i,j), name=compName, &
              rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
            call ESMF_LogWrite("Calling Finalize phase 1 for connectorComp: "// &
              trim(compName), ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
          endif
          call ESMF_CplCompFinalize(is%wrap%connectorComp(i,j), &
            importState=is%wrap%modelES(i), exportState=is%wrap%modelIS(j), &
            clock=internalClock, phase=1, userRc=urc, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase 1 "// &
            "Finalize for connectorComp "// &
            trim(adjustl(iString))//" -> "//trim(adjustl(jString)), &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          if (ESMF_LogFoundError(rcToCheck=urc, msg="Phase 1 "// &
            "Finalize for connectorComp "// &
            trim(adjustl(iString))//" -> "//trim(adjustl(jString))// &
            " did not return ESMF_SUCCESS", &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
        endif
      enddo
    enddo

    ! Finalize: modelComps
    do i=1, is%wrap%modelCount
      write (iString, *) i
      areServicesSet = &
        NUOPC_CompAreServicesSet(is%wrap%modelComp(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      if (areServicesSet) then
        if (btest(verbosity,13)) then
          call ESMF_GridCompGet(is%wrap%modelComp(i), name=compName, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          call ESMF_LogWrite("Calling Finalize phase 1 for modelComp: "// &
            trim(compName), ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
        endif
        call ESMF_GridCompFinalize(is%wrap%modelComp(i), &
          importState=is%wrap%modelIS(i), exportState=is%wrap%modelES(i), &
          clock=internalClock, phase=1, userRc=urc, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase 1 "// &
          "Finalize for modelComp "//trim(adjustl(iString)), &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        if (ESMF_LogFoundError(rcToCheck=urc, msg="Phase 1 "// &
          "Finalize for modelComp "//trim(adjustl(iString))//" did not "// &
          "return ESMF_SUCCESS", &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
    enddo

    ! SPECIALIZE by calling into optional attached method
    if (btest(profiling,7)) then
      call ESMF_TraceRegionEnter("label_Finalize")
    endif
    call ESMF_MethodExecute(driver, label=label_Finalize, existflag=existflag, &
      userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (btest(profiling,7)) then
      call ESMF_TraceRegionExit("label_Finalize")
    endif

    ! destroy components in the compList and their import and export States,
    ! and also petLists that were set by the user (and ownership transferred)
    nullify(compList)
    call NUOPC_DriverGetComp(driver, compList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    do i=1, size(compList)
      if (btest(verbosity,13).or.btest(verbosity,14)) then
        call ESMF_GridCompGet(compList(i), name=compName, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
      if (btest(verbosity,13)) then
        call ESMF_LogWrite("Delete modelComp: "//trim(compName), &
          ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
      call ESMF_GridCompDestroy(compList(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      if (btest(verbosity,14)) then
        call ESMF_LogWrite("Delete Import-/Export State for modelComp: "// &
          trim(compName), ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
      call ESMF_StateDestroy(is%wrap%modelIS(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_StateDestroy(is%wrap%modelES(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      if (associated(is%wrap%modelPetLists(i)%ptr)) then
        deallocate(is%wrap%modelPetLists(i)%ptr, stat=stat)
        if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
          msg="Deallocation of transferred model petList failed.", &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
    enddo
    deallocate(compList)
    
    ! destroy components in the connectorList 
    ! and also petLists for which ownership was transferred
    nullify(connectorList)
    nullify(petLists)
    call NUOPC_DriverGetComp(driver, connectorList, petLists, rc=rc)    
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    do i=1, size(connectorList)
      if (btest(verbosity,13)) then
        call ESMF_CplCompGet(connectorList(i), name=compName, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        call ESMF_LogWrite("Delete connectorComp: "//trim(compName), &
          ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
      call ESMF_CplCompDestroy(connectorList(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      if (associated(petLists(i)%ptr)) then
        deallocate(petLists(i)%ptr, stat=stat)
        if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
          msg="Deallocation of transferred connector petList failed.", &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
    enddo
    deallocate(petLists)
    deallocate(connectorList)
    
    ! destroy componentMap
    call ESMF_ContainerGet(is%wrap%componentMap, itemCount=itemCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    do i=1, itemCount
      call ESMF_ContainerGetUDTByIndex(is%wrap%componentMap, i, cmEntry, &
        ESMF_ITEMORDER_ADDORDER, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      deallocate(cmEntry%wrap, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg="Deallocation of cmEntry failed.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    enddo
    call ESMF_ContainerDestroy(is%wrap%componentMap, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    ! destroy connectorMap
    call ESMF_ContainerGet(is%wrap%connectorMap, itemCount=itemCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    do i=1, itemCount
      call ESMF_ContainerGetUDTByIndex(is%wrap%connectorMap, i, cnEntry, &
        ESMF_ITEMORDER_ADDORDER, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      deallocate(cnEntry%wrap, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg="Deallocation of cnEntry failed.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    enddo
    call ESMF_ContainerDestroy(is%wrap%connectorMap, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! deallocate lists inside the internal state
    deallocate(is%wrap%modelPetLists, is%wrap%connectorPetLists, &
      is%wrap%modelComp, is%wrap%initClock, is%wrap%modelIS, is%wrap%modelES, &
      is%wrap%connectorComp, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
      
    ! deallocate temporary phase maps in the internal state
    do i=0, is%wrap%modelCount
      do j=0, is%wrap%modelCount
        if (j==i) cycle ! skip
        if (associated(is%wrap%connectorPhaseMap(i,j)%phaseValue)) &
          deallocate(is%wrap%connectorPhaseMap(i,j)%phaseValue)
        if (associated(is%wrap%connectorPhaseMap(i,j)%phases)) &
          deallocate(is%wrap%connectorPhaseMap(i,j)%phases)
        if (associated(is%wrap%connectorPhaseMap(i,j)%phaseKey)) &
          deallocate(is%wrap%connectorPhaseMap(i,j)%phaseKey)
      enddo
      if (associated(is%wrap%modelPhaseMap(i)%phaseValue)) &
        deallocate(is%wrap%modelPhaseMap(i)%phaseValue)
      if (associated(is%wrap%modelPhaseMap(i)%phases)) &
        deallocate(is%wrap%modelPhaseMap(i)%phases)
      if (associated(is%wrap%modelPhaseMap(i)%phaseKey)) &
        deallocate(is%wrap%modelPhaseMap(i)%phaseKey)
    enddo
    deallocate(is%wrap%connectorPhaseMap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of connectorPhaseMap failed.", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    deallocate(is%wrap%modelPhaseMap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of modelPhaseMap failed.", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! deallocate run sequence data structures
    call NUOPC_RunSequenceDeallocate(is%wrap%runSeq, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! deallocate internal state memory
    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! extro
    call NUOPC_LogExtro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle profiling
    if (btest(profiling,6)) then
      call ESMF_TraceRegionExit(rName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  recursive subroutine FinalizeReset(driver, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: driver
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    character(*), parameter   :: rName="FinalizeReset"
    integer                   :: urc, stat
    type(type_InternalState)  :: is
    type(ESMF_Clock)          :: internalClock
    integer                   :: i, j, itemCount
    type(ComponentMapEntry)   :: cmEntry
    type(ConnectorMapEntry)   :: cnEntry
    character(ESMF_MAXSTR)    :: iString, jString
    logical                   :: existflag
    logical                   :: areServicesSet
    character(ESMF_MAXSTR)    :: name
    integer                   :: verbosity, profiling
    type(ESMF_GridComp), pointer  :: compList(:)
    type(ESMF_CplComp), pointer   :: connectorList(:)
    type(ESMF_PtrInt1D), pointer  :: petLists(:)
    
    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(driver, name=name, verbosity=verbosity, &
      profiling=profiling, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle profiling
    if (btest(profiling,11)) then
      call ESMF_TraceRegionEnter("Leading Barrier", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_VMBarrier(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_TraceRegionExit("Leading Barrier", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    if (btest(profiling,6)) then
      call ESMF_TraceRegionEnter(rName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    ! intro
    call NUOPC_LogIntro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! query Component for its Clock
    call ESMF_GridCompGet(driver, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
#if 0
    ! Finalize: connectorComps
    do i=0, is%wrap%modelCount
      write (iString, *) i
      do j=0, is%wrap%modelCount
        write (jString, *) j
        areServicesSet = &
          NUOPC_CompAreServicesSet(is%wrap%connectorComp(i,j), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        if (areServicesSet) then
          call ESMF_CplCompFinalize(is%wrap%connectorComp(i,j), &
            importState=is%wrap%modelES(i), exportState=is%wrap%modelIS(j), &
            clock=internalClock, phase=1, userRc=urc, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase 1 "// &
            "Finalize for connectorComp "// &
            trim(adjustl(iString))//" -> "//trim(adjustl(jString)), &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          if (ESMF_LogFoundError(rcToCheck=urc, msg="Phase 1 "// &
            "Finalize for connectorComp "// &
            trim(adjustl(iString))//" -> "//trim(adjustl(jString)) &
            " did not return ESMF_SUCCESS", &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
        endif
      enddo
    enddo

    ! Finalize: modelComps
    do i=1, is%wrap%modelCount
      write (iString, *) i
        areServicesSet = &
          NUOPC_CompAreServicesSet(is%wrap%modelComp(i), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      if (areServicesSet) then
        call ESMF_GridCompFinalize(is%wrap%modelComp(i), &
          importState=is%wrap%modelIS(i), exportState=is%wrap%modelES(i), &
          clock=internalClock, phase=1, userRc=urc, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase 1 "// &
          "Finalize for modelComp "//trim(adjustl(iString)), &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        if (ESMF_LogFoundError(rcToCheck=urc, msg="Phase 1 "// &
          "Finalize for modelComp "//trim(adjustl(iString))//" did not "// &
          "return ESMF_SUCCESS", &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
    enddo
    
    ! SPECIALIZE by calling into optional attached method
    if (btest(profiling,7)) then
      call ESMF_TraceRegionEnter("label_Finalize")
    endif
    call ESMF_MethodExecute(driver, label=label_Finalize, existflag=existflag, &
      userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (btest(profiling,7)) then
      call ESMF_TraceRegionExit("label_Finalize")
    endif
#endif

    ! reset flags in the internal state
    is%wrap%firstTimeDataInit  = .true.
    is%wrap%dataDepAllComplete = .true.
    
    ! reset attributes on the component
    call NUOPC_CompAttributeSet(driver, &
      name="InitializeDataComplete", value="false", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(driver, &
      name="InitializeDataProgress", value="false", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
      
    ! extro
    call NUOPC_LogExtro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle profiling
    if (btest(profiling,6)) then
      call ESMF_TraceRegionExit(rName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_DriverAddComp - Add a GridComp child to a Driver
!
! !INTERFACE:
  ! Private name; call using NUOPC_DriverAddComp()
  recursive subroutine NUOPC_DriverAddGridComp(driver, compLabel, &
    compSetServicesRoutine, compSetVMRoutine, petList, info, comp, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                        :: driver
    character(len=*),    intent(in)            :: compLabel
    interface
      recursive subroutine compSetServicesRoutine(gridcomp, rc)
        use ESMF
        implicit none
        type(ESMF_GridComp)        :: gridcomp ! must not be optional
        integer, intent(out)       :: rc       ! must not be optional
      end subroutine
    end interface
    interface
      recursive subroutine compSetVMRoutine(gridcomp, rc)
        use ESMF
        implicit none
        type(ESMF_GridComp)        :: gridcomp ! must not be optional
        integer, intent(out)       :: rc       ! must not be optional
      end subroutine
    end interface
    optional                                   :: compSetVMRoutine
    integer,             intent(in),  optional :: petList(:)
    type(ESMF_Info),     intent(in),  optional :: info
    type(ESMF_GridComp), intent(out), optional :: comp
    integer,             intent(out), optional :: rc 
!
! !DESCRIPTION:
! Create and add a GridComp (i.e. Model, Mediator, or Driver) as a child 
! component to a Driver. The component is created on the provided {\tt petList},
! or by default across all of the Driver PETs.
!
! The specified {\tt compSetServicesRoutine()} is called back immediately after
! the new child component has been created internally. Very little around the
! component is set up at that time (e.g. NUOPC component attributes will not be
! available). The routine should therefore be very light weight, with the sole
! purpose of setting the entry points of the component -- typically by deriving 
! from a generic component followed by the appropriate specilizations.
!
! If provided, the {\tt compSetVMRoutine()} is called back before the 
! {\tt compSetServicesRoutine()}. This allows the child component to set
! aspects of its own VM, such as threading or the PE distribution among PETs.
!
! The {\tt info} argument can be used to pass custom attributes to the child
! component. These attributes are available on the component when
! {\tt compSetVMRoutine()} and {\tt compSetServicesRoutine()} are called.
! The attributes provided in {\tt info} are {\em copied} onto the child
! component. This allows the same {\tt info} object to be used for multiple
! child components without conflict.
!
! The {\tt compLabel} must uniquely identify the child component within the
! context of the Driver component.
!
! If the {\tt comp} argument is specified, it will reference the newly created
! component on return.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                         :: localrc
    integer                         :: userrc
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is
    type(ComponentMapEntry)         :: cmEntry
    integer                         :: stat, i
    character(ESMF_MAXSTR)          :: msgString, lString
    integer                         :: verbosity
    type(ESMF_Info)                 :: infoh

    if (present(rc)) rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(driver, name=name, verbosity=verbosity, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! Add another component to the componentMap with associated compLabel
    allocate(cmEntry%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(stat, msg="allocating cmEntry", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    is%wrap%modelCount = is%wrap%modelCount + 1
    i = is%wrap%modelCount
    cmEntry%wrap%label = trim(compLabel)
    cmEntry%wrap%component = ESMF_GridCompCreate(name=trim(compLabel), &
      petList=petList, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    if (present(petList)) then
      allocate(cmEntry%wrap%petList(size(petList)))
      cmEntry%wrap%petList = petList  ! copy the petList elements
    else
      nullify(cmEntry%wrap%petList) ! invalidate the petList
    endif
  
    if (btest(verbosity,13)) then
      if (associated(cmEntry%wrap%petList)) then
        write (lString, *) size(cmEntry%wrap%petList)
        write (msgString,"(A)") trim(name)//&
          " - Creating model component "//trim(cmEntry%wrap%label)//&
          " with petList of size "//trim(adjustl(lString))//" :"
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        call NUOPC_LogPetList(cmEntry%wrap%petList, name=name, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      else
        write (msgString,"(A)") trim(name)//" - Creating model component "//&
          trim(cmEntry%wrap%label)//" without petList."
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
    endif

    call ESMF_ContainerAddUDT(is%wrap%componentMap, trim(compLabel), &
      cmEntry, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! optionally copy Attributes from info object to the newly created component
    if (present(info)) then
      call ESMF_InfoGetFromHost(cmEntry%wrap%component, infoh, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_InfoSet(infoh, "", info, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    ! optionally call the SetVM on the added component
    if (present(compSetVMRoutine)) then
      call ESMF_GridCompSetVM(cmEntry%wrap%component, &
        compSetVMRoutine, userRc=userrc, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      if (ESMF_LogFoundError(rcToCheck=userrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
      
    ! Call the SetServices on the added component
    call ESMF_GridCompSetServices(cmEntry%wrap%component, &
      compSetServicesRoutine, userRc=userrc, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=userrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
      
    ! Set the CompLabel attribute
    call NUOPC_CompAttributeSet(cmEntry%wrap%component, &
      name="CompLabel", value=trim(cmEntry%wrap%label), rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! Optionally return the added component
    if (present(comp)) comp = cmEntry%wrap%component
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_DriverAddComp - Add a GridComp child from shared object to a Driver
!
! !INTERFACE:
  ! Private name; call using NUOPC_DriverAddComp()
  recursive subroutine NUOPC_DriverAddGridCompSO(driver, compLabel, &
    sharedObj, petList, info, comp, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                        :: driver
    character(len=*),    intent(in)            :: compLabel
    character(len=*),    intent(in),  optional :: sharedObj
    integer,             intent(in),  optional :: petList(:)
    type(ESMF_Info),     intent(in),  optional :: info
    type(ESMF_GridComp), intent(out), optional :: comp
    integer,             intent(out), optional :: rc
!
! !DESCRIPTION:
! Create and add a GridComp (i.e. Model, Mediator, or Driver) as a child 
! component to a Driver. The component is created on the provided {\tt petList},
! or by default across all of the Driver PETs. 
!
! The {\tt SetServices()} routine in the {\tt sharedObj} is called back
! immediately after the
! new child component has been created internally. Very little around the
! component is set up at that time (e.g. NUOPC component attributes will not be
! available). The routine should therefore be very light weight, with the sole
! purpose of setting the entry points of the component -- typically by deriving 
! from a generic component followed by the appropriate specilizations.
!
! The {\tt info} argument can be used to pass custom attributes to the child
! component. These attributes are available on the component when
! {\tt compSetVMRoutine()} and {\tt compSetServicesRoutine()} are called.
! The attributes provided in {\tt info} are {\em copied} onto the child
! component. This allows the same {\tt info} object to be used for multiple
! child components without conflict.
!
! The {\tt compLabel} must uniquely identify the child component within the
! context of the Driver component.
!
! If the {\tt comp} argument is specified, it will reference the newly created
! component on return.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                         :: localrc
    integer                         :: userrc
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is
    type(ComponentMapEntry)         :: cmEntry
    integer                         :: stat, i
    character(ESMF_MAXSTR)          :: msgString, lString
    integer                         :: verbosity
    type(ESMF_Info)                 :: infoh

    if (present(rc)) rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(driver, name=name, verbosity=verbosity, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! Add another component to the componentMap with associated compLabel
    allocate(cmEntry%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(stat, msg="allocating cmEntry", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    is%wrap%modelCount = is%wrap%modelCount + 1
    i = is%wrap%modelCount
    cmEntry%wrap%label = trim(compLabel)
    cmEntry%wrap%component = ESMF_GridCompCreate(name=trim(compLabel), &
      petList=petList, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    if (present(petList)) then
      allocate(cmEntry%wrap%petList(size(petList)))
      cmEntry%wrap%petList = petList  ! copy the petList elements
    else
      nullify(cmEntry%wrap%petList) ! invalidate the petList
    endif
  
    if (btest(verbosity,13)) then
      if (associated(cmEntry%wrap%petList)) then
        write (lString, *) size(cmEntry%wrap%petList)
        write (msgString,"(A)") trim(name)//&
          " - Creating model component "//trim(cmEntry%wrap%label)//&
          " with petList of size "//trim(adjustl(lString))//" :"
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        call NUOPC_LogPetList(cmEntry%wrap%petList, name=name, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      else
        write (msgString,"(A)") trim(name)//" - Creating model component "//&
          trim(cmEntry%wrap%label)//" without petList."
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
    endif

    call ESMF_ContainerAddUDT(is%wrap%componentMap, trim(compLabel), &
      cmEntry, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! optionally copy Attributes from info object to the newly created component
    if (present(info)) then
      call ESMF_InfoGetFromHost(cmEntry%wrap%component, infoh, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_InfoSet(infoh, "", info, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    ! Call the SetServices on the added component
    call NUOPC_CompSetServices(cmEntry%wrap%component, &
      sharedObj=sharedObj, userRc=userrc, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=userrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
      
    ! Set the CompLabel attribute
    call NUOPC_CompAttributeSet(cmEntry%wrap%component, &
      name="CompLabel", value=trim(cmEntry%wrap%label), rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! Optionally return the added component
    if (present(comp)) comp = cmEntry%wrap%component
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_DriverAddComp - Add a CplComp child to a Driver
!
! !INTERFACE:
  ! Private name; call using NUOPC_DriverAddComp()
  recursive subroutine NUOPC_DriverAddCplComp(driver, srcCompLabel, &
    dstCompLabel, compSetServicesRoutine, compSetVMRoutine, petList, info, &
    comp, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                        :: driver
    character(len=*),    intent(in)            :: srcCompLabel
    character(len=*),    intent(in)            :: dstCompLabel
    interface
      recursive subroutine compSetServicesRoutine(cplcomp, rc)
        use ESMF
        implicit none
        type(ESMF_CplComp)         :: cplcomp  ! must not be optional
        integer, intent(out)       :: rc       ! must not be optional
      end subroutine
    end interface
    interface
      recursive subroutine compSetVMRoutine(cplcomp, rc)
        use ESMF
        implicit none
        type(ESMF_CplComp)         :: cplcomp  ! must not be optional
        integer, intent(out)       :: rc       ! must not be optional
      end subroutine
    end interface
    optional                                   :: compSetVMRoutine
    integer, target,     intent(in),  optional :: petList(:)
    type(ESMF_Info),     intent(in),  optional :: info
    type(ESMF_CplComp),  intent(out), optional :: comp
    integer,             intent(out), optional :: rc 
!
! !DESCRIPTION:
! Create and add a CplComp (i.e. Connector) as a child component to a Driver.
! The component is created on the provided {\tt petList}, or by default across 
! the union of PETs of the components indicated by {\tt srcCompLabel}
! and {\tt dstCompLabel}.
!
! The specified {\tt SetServices()} routine is called back immediately after the
! new child component has been created internally. Very little around the
! component is set up at that time (e.g. NUOPC component attributes will not be
! available). The routine should therefore be very light weight, with the sole
! purpose of setting the entry points of the component -- typically by deriving 
! from a generic component followed by the appropriate specilizations.
!
! The {\tt info} argument can be used to pass custom attributes to the child
! component. These attributes are available on the component when
! {\tt compSetVMRoutine()} and {\tt compSetServicesRoutine()} are called.
! The attributes provided in {\tt info} are {\em copied} onto the child
! component. This allows the same {\tt info} object to be used for multiple
! child components without conflict.
!
! The {\tt compLabel} must uniquely identify the child component within the 
! context of the Driver component.
!
! If the {\tt comp} argument is specified, it will reference the newly created
! component on return.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                         :: localrc
    integer                         :: userrc
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is
    type(ConnectorMapEntry)         :: cmEntry
    integer                         :: stat, src, dst
    type(ESMF_GridComp)             :: srcComp, dstComp
    integer, pointer                :: connectorPetList(:)
    integer, pointer                :: connectorPetListTemp(:)
    integer, pointer                :: connectorPetListTemp2(:)
    integer, pointer                :: srcPetList(:), dstPetList(:)
    integer                         :: k, l, cIndex
    character(ESMF_MAXSTR)          :: msgString, lString
    type(ESMF_VM)                   :: vm
    logical                         :: isPresent
    integer                         :: verbosity
    type(ESMF_Info)                 :: infoh

    if (present(rc)) rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(driver, name=name, verbosity=verbosity, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! set up connectorPetList
    nullify(connectorPetList)     ! invalidate
    nullify(connectorPetListTemp) ! invalidate
    if (present(petList)) then
      ! explict petList was provided
      connectorPetList => petList ! point to the provided petList
    else
      ! figure out the default union petList.... if necessary
      call NUOPC_DriverGetComp(driver, srcCompLabel, petList=srcPetList, &
        rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call NUOPC_DriverGetComp(driver, dstCompLabel, petList=dstPetList, &
        rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      if (associated(srcPetList).and.associated(dstPetList)) then
        ! must construct the union petList
        allocate(connectorPetListTemp(size(srcPetList)+size(dstPetList)), &
          stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation #1 of connector petList failed.", &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        cIndex = size(srcPetList)
        connectorPetListTemp(1:cIndex) = srcPetList(:) ! copy all src PETs
        ! there is no guarantee of order, no way to optimize construction
        cIndex = cIndex+1
        do k=1, size(dstPetList)
          ! append element k in dstPetList to connectorPetList if not yet in
          do l=1, size(srcPetList)
            if (connectorPetListTemp(l) == dstPetList(k)) exit
          enddo
          if (l == size(srcPetList) + 1) then
            ! append element
            connectorPetListTemp(cIndex) = dstPetList(k)
            cIndex = cIndex + 1
          endif
        enddo
        connectorPetList => connectorPetListTemp(1:cIndex-1)
      endif
    endif
    
    ! Add another connector to the connectorMap with associated compLabel
    allocate(cmEntry%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(stat, msg="allocating cmEntry", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    cmEntry%wrap%label = trim(srcCompLabel)//"-TO-"//trim(dstCompLabel)
    
    if (associated(connectorPetList)) then
      ! connectorPetList was either explicitly provided or constructed
      if (btest(verbosity,13)) then
        write (lString, *) size(connectorPetList)
        write (msgString,"(A)") trim(name)//&
          " - Creating connector component "//trim(cmEntry%wrap%label)//&
          " with petList of size "//trim(adjustl(lString))//" :"
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        call NUOPC_LogPetList(connectorPetList, name=name, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
      cmEntry%wrap%connector = ESMF_CplCompCreate(&
        name=trim(cmEntry%wrap%label), petList=connectorPetList, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    else
      ! create the connector without petList
      if (btest(verbosity,13)) then
        write (msgString,"(A)") trim(name) &
          //" - Creating connector component "// &
          trim(cmEntry%wrap%label)//" without petList."
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
      cmEntry%wrap%connector = ESMF_CplCompCreate(&
        name=trim(cmEntry%wrap%label), rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    
    ! store the petList 
    nullify(connectorPetListTemp2)  ! invalidate
    if (associated(connectorPetList)) then
      ! either provided or constructed connectorPetList
      allocate(connectorPetListTemp2(size(connectorPetList)))
      connectorPetListTemp2 = connectorPetList  ! copy contents
    endif
    cmEntry%wrap%petList => connectorPetListTemp2  ! transferring ownership

    ! clean-up
    if (associated(connectorPetListTemp)) then
      ! clean-up
      deallocate(connectorPetListTemp, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg="Deallocation of connector petList failed.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    ! add the new connector component into the connectorMap
    call ESMF_ContainerAddUDT(is%wrap%connectorMap, trim(cmEntry%wrap%label), &
      cmEntry, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! access the srcComp and dstComp and there VMs in order to set them in conn.
    call NUOPC_DriverGetComp(driver, srcCompLabel, srcComp, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call NUOPC_DriverGetComp(driver, dstCompLabel, dstComp, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! The following block, accessing the legacy static data structures is only
    ! here in order to support adding connectors after these data structures
    ! have been set up. 
    if (is%wrap%legacyReady) then
      ! Figuring out the index into the modelComp array.
      !TODO: This is a pretty involved look-up, and future implementation will
      !TODO: fully eliminate the static arrays modelComp and connectorComp, 
      !TODO: removing the need to do this look-up here.
      do src=0, is%wrap%modelCount
        if (is%wrap%modelComp(src)==srcComp) exit ! found the match
      enddo
      do dst=0, is%wrap%modelCount
        if (is%wrap%modelComp(dst)==dstComp) exit ! found the match
      enddo
      if (src > is%wrap%modelCount) then
        ! component could not be identified -> consider relaxedFlag
        ! bail out with error
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="src component could not be identified.", &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
        return  ! bail out
      endif
      if (dst > is%wrap%modelCount) then
        ! component could not be identified -> consider relaxedFlag
        ! bail out with error
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="dst component could not be identified.", &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
        return  ! bail out
      endif
      ! also add the new connector component to the legacy data structures:
      is%wrap%connectorComp(src,dst) = cmEntry%wrap%connector ! set the alias
      is%wrap%connectorPetLists(src,dst)%ptr => cmEntry%wrap%petList
    endif

    ! optionally copy Attributes from info object to the newly created component
    if (present(info)) then
      call ESMF_InfoGetFromHost(cmEntry%wrap%connector, infoh, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_InfoSet(infoh, "", info, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    ! optionally call the SetVM on the added component
    if (present(compSetVMRoutine)) then
      call ESMF_CplCompSetVM(cmEntry%wrap%connector, &
        compSetVMRoutine, userRc=userrc, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      if (ESMF_LogFoundError(rcToCheck=userrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    ! Call the SetServices on the added connector
    call ESMF_CplCompSetServices(cmEntry%wrap%connector, &
      compSetServicesRoutine, userRc=userrc, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=userrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! If legacy data structures are already set up, then here must also
    ! ensure phase 0 is called for this connector, and attributes are set
    if (is%wrap%legacyReady) then
      call ESMF_CplCompInitialize(is%wrap%connectorComp(src,dst), &
        importState=is%wrap%modelES(src), exportState=is%wrap%modelIS(dst), &
        phase=0, userRc=userrc, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      if (ESMF_LogFoundError(rcToCheck=userrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      ! need to update the Component attributes across all PETs
      call ESMF_VMGetCurrent(vm, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      if (associated(is%wrap%connectorPetLists(src,dst)%ptr)) then
        call ESMF_AttributeUpdate(is%wrap%connectorComp(src,dst), vm, &
          rootList=is%wrap%connectorPetLists(src,dst)%ptr(1:1), rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc))&
          return  ! bail out
      else
        call ESMF_AttributeUpdate(is%wrap%connectorComp(src,dst), vm, &
          rootList=(/0/), rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc))&
          return  ! bail out
      endif
    endif
    ! must set the srcVM and dstVM inside Connector in case those are needed,
    ! e.g. when Connector advertises on behalf of src/dst during mirroring
    call ESMF_GridCompGet(srcComp, vmIsPresent=isPresent, vm=vm, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (isPresent) then
      call NUOPC_ConnectorSet(cmEntry%wrap%connector, srcVM=vm, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    call ESMF_GridCompGet(dstComp, vmIsPresent=isPresent, vm=vm, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (isPresent) then
      call NUOPC_ConnectorSet(cmEntry%wrap%connector, dstVM=vm, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    ! Optionally return the added connector
    if (present(comp)) comp = cmEntry%wrap%connector
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_DriverAddRunElement - Add RunElement for Model, Mediator, or Driver
!
! !INTERFACE:
  ! Private name; call using NUOPC_DriverAddRunElement()
  recursive subroutine NUOPC_DriverAddRunElementMPL(driver, slot, compLabel, &
    keywordEnforcer, phaseLabel, relaxedflag, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                        :: driver
    integer,             intent(in)            :: slot
    character(len=*),    intent(in)            :: compLabel
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len=*),    intent(in),  optional :: phaseLabel
    logical,             intent(in),  optional :: relaxedflag
    integer,             intent(out), optional :: rc 
!
! !DESCRIPTION:
! Add an element associated with a Model, Mediator, or Driver component to the
! run sequence of the Driver. The component must have been added to the Driver,
! and associated with {\tt compLabel} prior to this call.
!
! If {\tt phaseLabel} was not specified, the first entry in the
! {\tt RunPhaseMap} attribute of the referenced component will be used to 
! determine the run phase of the added element.
!
! By default an error is returned if no component is associated with the 
! specified {\tt compLabel}. This error can be suppressed by setting
! {\tt relaxedflag=.true.}, and no entry will be added to the run sequence.
!
! The {\tt slot} number identifies the run sequence time slot in case multiple
! sequences are available. Slots start counting from 1.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                         :: localrc
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is
    integer                         :: iComp, i
    type(ESMF_GridComp)             :: comp
    integer                         :: phase
    logical                         :: relaxed
    logical                         :: internalFlag
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    relaxed = .false.
    if (present(relaxedflag)) relaxed=relaxedflag
    
    ! query the component for info
    call NUOPC_CompGet(driver, name=name, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! check if slot index is valid
    if (slot<=0 .or. slot>size(is%wrap%runSeq)) then
      ! bail out with error
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="Slot index is out of bounds.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
      return  ! bail out
    endif

    ! Figuring out the index into the modelComp array.
    !TODO: This is a pretty involved look-up, and future implementation will
    !TODO: fully eliminate the static array modelComp,
    !TODO: removing the need to do this look-up here.
    call NUOPC_DriverGetComp(driver, trim(compLabel), comp, &
      relaxedflag=relaxedflag, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    ! iComp below should start with 0 in case driver itself is match
    do iComp=0, is%wrap%modelCount
      if (is%wrap%modelComp(iComp)==comp) exit  ! match found
    enddo
    if (iComp > is%wrap%modelCount) then
      ! component could not be identified -> consider relaxedFlag
      if (relaxed) then
        ! bail out without error
        return  ! bail out
      else
        ! bail out with error
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="component could not be identified.", &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
        return  ! bail out
      endif
    endif
    
    ! consider driver self
    internalFlag = .false.
    if (iComp==0) internalFlag = .true.
    
    ! Figure out the phase index
    call NUOPC_CompSearchPhaseMap(comp, methodflag=ESMF_METHOD_RUN, &
      phaseLabel=phaseLabel, phaseIndex=phase, internalFlag=internalFlag, &
      rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! check the result of the seach
    if (phase < 0) then
      ! phase could not be identified -> consider relaxedFlag
      if (relaxed) then
        ! bail out without error
        return  ! bail out
      else
        ! bail out with error
        if (present(phaseLabel)) then
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="run phase: '"//trim(phaseLabel)//"' could not be identified.",&
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
        else
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="run phase without label could not be identified.", &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
        endif
        return  ! bail out
      endif
    endif
    
    ! Actually add the RunElement for the identified component
    call NUOPC_RunElementAddComp(is%wrap%runSeq(slot), i=iComp, phase=phase, &
      rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! Store a reference to the first runClock under which this model is called
    ! during RunSequence execution, in order to use it for model initialize
    is%wrap%initClock(iComp) = is%wrap%runSeq(slot)%clock
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_DriverAddRunElement - Add RunElement for Connector
!
! !INTERFACE:
  ! Private name; call using NUOPC_DriverAddRunElement()
  recursive subroutine NUOPC_DriverAddRunElementCPL(driver, slot, srcCompLabel,&
    dstCompLabel, keywordEnforcer, phaseLabel, relaxedflag, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                        :: driver
    integer,             intent(in)            :: slot
    character(len=*),    intent(in)            :: srcCompLabel
    character(len=*),    intent(in)            :: dstCompLabel
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len=*),    intent(in),  optional :: phaseLabel
    logical,             intent(in),  optional :: relaxedflag
    integer,             intent(out), optional :: rc 
!
! !DESCRIPTION:
! Add an element associated with a Connector component to the
! run sequence of the Driver. The component must have been added to the Driver,
! and associated with {\tt srcCompLabel} and {\tt dstCompLabel} prior to this
! call.
!
! If {\tt phaseLabel} was not specified, the first entry in the
! {\tt RunPhaseMap} attribute of the referenced component will be used to 
! determine the run phase of the added element.
!
! By default an error is returned if no component is associated with the 
! specified {\tt compLabel}. This error can be suppressed by setting
! {\tt relaxedflag=.true.}, and no entry will be added to the run sequence.
!
! The {\tt slot} number identifies the run sequence time slot in case multiple
! sequences are available. Slots start counting from 1.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                         :: localrc
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is
    integer                         :: src, dst, i
    type(ESMF_GridComp)             :: srcComp, dstComp
    type(ESMF_CplComp)              :: comp
    integer                         :: phase
    logical                         :: relaxed
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    relaxed = .false.
    if (present(relaxedflag)) relaxed=relaxedflag
    
    ! query the component for info
    call NUOPC_CompGet(driver, name=name, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! check if slot index is valid
    if (slot<=0 .or. slot>size(is%wrap%runSeq)) then
      ! bail out with error
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="Slot index is out of bounds.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
      return  ! bail out
    endif

    ! Figuring out the index into the modelComp array.
    !TODO: This is a pretty involved look-up, and future implementation will
    !TODO: fully eliminate the static arrays modelComp and connectorComp, 
    !TODO: removing the need to do this look-up here.
    call NUOPC_DriverGetComp(driver, srcCompLabel, srcComp, &
      relaxedflag=relaxedflag, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call NUOPC_DriverGetComp(driver, dstCompLabel, dstComp, &
      relaxedflag=relaxedflag, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    do src=0, is%wrap%modelCount
      if (is%wrap%modelComp(src)==srcComp) exit ! found the match
    enddo
    do dst=0, is%wrap%modelCount
      if (is%wrap%modelComp(dst)==dstComp) exit ! found the match
    enddo
    if (src > is%wrap%modelCount) then
      ! component could not be identified -> consider relaxedFlag
      if (relaxed) then
        ! bail out without error
        return  ! bail out
      else
        ! bail out with error
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="src component could not be identified.", &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
        return  ! bail out
      endif
    endif
    if (dst > is%wrap%modelCount) then
      ! component could not be identified -> consider relaxedFlag
      if (relaxed) then
        ! bail out without error
        return  ! bail out
      else
        ! bail out with error
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="dst component could not be identified.", &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
        return  ! bail out
      endif
    endif
    
    ! Need the actual Connector component
    call NUOPC_DriverGetComp(driver, srcCompLabel, dstCompLabel, comp, &
      relaxedflag=relaxedflag, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! Figure out the phase index
    call NUOPC_CompSearchPhaseMap(comp, methodflag=ESMF_METHOD_RUN, &
      phaseLabel=phaseLabel, phaseIndex=phase, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! check the result of the seach
    if (phase < 0) then
      ! phase could not be identified -> consider relaxedFlag
      if (relaxed) then
        ! bail out without error
        return  ! bail out
      else
        ! bail out with error
        if (present(phaseLabel)) then
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="run phase: '"//trim(phaseLabel)//"' could not be identified.",&
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
        else
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="run phase without label could not be identified.", &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
        endif
        return  ! bail out
      endif
    endif
    
    ! Actually add the RunElement for the identified Connector component
    call NUOPC_RunElementAddComp(is%wrap%runSeq(slot), i=src, j=dst, &
      phase=phase, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_DriverAddRunElement - Add RunElement that links to another slot
!
! !INTERFACE:
  ! Private name; call using NUOPC_DriverAddRunElement()
  recursive subroutine NUOPC_DriverAddRunElementL(driver, slot, linkSlot, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                        :: driver
    integer,             intent(in)            :: slot
    integer,             intent(in)            :: linkSlot
    integer,             intent(out), optional :: rc 
!
! !DESCRIPTION:
! Add an element to the run sequence of the Driver that links to the time slot
! indicated by {\tt linkSlot}.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                         :: localrc
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is

    if (present(rc)) rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(driver, name=name, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! check if slot index is valid
    if (slot<=0 .or. slot>size(is%wrap%runSeq)) then
      ! bail out with error
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="Slot index is out of bounds.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
      return  ! bail out
    endif

    ! Actually add the RunElement for the identified component
    call NUOPC_RunElementAddLink(is%wrap%runSeq(slot), slot=linkSlot, &
      rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_DriverEgestRunSequence - Egest the run sequence as FreeFormat
!
! !INTERFACE:
  recursive subroutine NUOPC_DriverEgestRunSequence(driver, freeFormat, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                           :: driver
    type(NUOPC_FreeFormat), intent(out)           :: freeFormat
    integer,                intent(out), optional :: rc 
!
! !DESCRIPTION:
! Egest the run sequence stored in the driver as a FreeFormat object. It is the
! caller's responsibility to destroy the created freeFormat object.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                         :: localrc
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is

    if (present(rc)) rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(driver, name=name, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    !TODO: actually create a FreeFormat object with contents. For now just empty
    freeFormat = NUOPC_FreeFormatCreate(rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_DriverGet - Get info from a Driver
!
! !INTERFACE:
  ! Private name; call using NUOPC_DriverGet()
  recursive subroutine NUOPC_DriverGet(driver, slotCount, parentClock, &
    importState, exportState, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                        :: driver
    integer,             intent(out), optional :: slotCount
    type(ESMF_Clock),    intent(out), optional :: parentClock
    type(ESMF_State),    intent(out), optional :: importState
    type(ESMF_State),    intent(out), optional :: exportState
    integer,             intent(out), optional :: rc 
!
! !DESCRIPTION:
!   Access Driver information.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                         :: localrc
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is

    if (present(rc)) rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(driver, name=name, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! slotCount
    if (present(slotCount)) then
      slotCount = size(is%wrap%runSeq)
    endif
    
    ! parentClock
    if (present(parentClock)) then
      parentClock = is%wrap%parentClock
    endif
    
    ! remaining arguments
    call ESMF_GridCompGet(driver, importState=importState, &
      exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_DriverGetComp - Get a GridComp child from a Driver
!
! !INTERFACE:
  ! Private name; call using NUOPC_DriverGetComp()
  recursive subroutine NUOPC_DriverGetGridComp(driver, compLabel, comp, petList, &
    importState, exportState, relaxedflag, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                        :: driver
    character(len=*),    intent(in)            :: compLabel
    type(ESMF_GridComp), intent(out), optional :: comp
    integer,             pointer,     optional :: petList(:)
    type(ESMF_State),    intent(out), optional :: importState
    type(ESMF_State),    intent(out), optional :: exportState
    logical,             intent(in),  optional :: relaxedflag
    integer,             intent(out), optional :: rc 
!
! !DESCRIPTION:
! Query the Driver for a GridComp (i.e. Model, Mediator, or Driver) child 
! component that was added under {\tt compLabel}.
!
! If provided, the {\tt petList} argument will be associated with the petList
! that was used to create the referenced component. This pointer must not be
! deallocated by the user!
!
! By default an error is returned if no component is associated with the 
! specified {\tt compLabel}. This error can be suppressed by setting
! {\tt relaxedflag=.true.}, and unassociated arguments will be returned.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                         :: localrc
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is
    type(ComponentMapEntry)         :: cmEntry
    logical                         :: relaxed, getFlag, foundFlag
    character(ESMF_MAXSTR)          :: driverCompLabel

    if (present(rc)) rc = ESMF_SUCCESS

    relaxed = .false.
    if (present(relaxedflag)) relaxed=relaxedflag

    ! query the component for info
    call NUOPC_CompGet(driver, name=name, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! determine whether compLabel exists in the drivers map    
    call ESMF_ContainerGet(is%wrap%componentMap, trim(compLabel), &
      isPresent=foundFlag, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! alternative exit condition if driver itself matches compLabel
    if (.not.foundFlag) then
      call NUOPC_CompAttributeGet(driver, name="CompLabel", &
        value=driverCompLabel, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      if (trim(compLabel) == trim(driverCompLabel)) then
        ! the driver itself is the searched for component
        if (present(comp)) comp = driver
        if (present(petList)) petList => null()
        return
      endif
    endif

    ! consider relaxed mode
    getFlag = .true.
    if (relaxed) getFlag = foundFlag
    
    ! Conditionally access the entry in componentMap
    if (getFlag) then
      call ESMF_ContainerGetUDT(is%wrap%componentMap, trim(compLabel), &
        cmEntry, localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      if (present(comp)) comp = cmEntry%wrap%component
      if (present(petList)) petList => cmEntry%wrap%petList
      if (present(importState) .or. present(exportState)) then
        call ESMF_GridCompGet(cmEntry%wrap%component, importState=importState, &
          exportState=exportState, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
    else
      ! return nullified arguments
      if (present(comp)) comp%compp => null()
      if (present(petList)) petList => null()
    endif
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_DriverGetComp - Get a CplComp child from a Driver
!
! !INTERFACE:
  ! Private name; call using NUOPC_DriverGetComp()
  recursive subroutine NUOPC_DriverGetCplComp(driver, srcCompLabel, &
    dstCompLabel, comp, petList, relaxedflag, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                        :: driver
    character(len=*),    intent(in)            :: srcCompLabel
    character(len=*),    intent(in)            :: dstCompLabel
    type(ESMF_CplComp),  intent(out), optional :: comp
    integer,             pointer    , optional :: petList(:)
    logical,             intent(in),  optional :: relaxedflag
    integer,             intent(out), optional :: rc 
!
! !DESCRIPTION:
! Query the Driver for a CplComp (i.e. Connector) child 
! component that was added under {\tt compLabel}.
!
! If provided, the {\tt petList} argument will be associated with the petList
! that was used to create the referenced component. This pointer must not be
! deallocated by the user!
!
! By default an error is returned if no component is associated with the 
! specified {\tt compLabel}. This error can be suppressed by setting
! {\tt relaxedflag=.true.}, and unassociated arguments will be returned.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                         :: localrc
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is
    type(ConnectorMapEntry)         :: cmEntry
    logical                         :: relaxed, getFlag

    if (present(rc)) rc = ESMF_SUCCESS

    relaxed = .false.
    if (present(relaxedflag)) relaxed=relaxedflag

    ! query the component for info
    call NUOPC_CompGet(driver, name=name, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! consider relaxed mode
    getFlag = .true.
    if (relaxed) then
      call ESMF_ContainerGet(is%wrap%connectorMap, &
        trim(srcCompLabel)//"-TO-"//trim(dstCompLabel), &
        isPresent=getFlag, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    
    ! Conditionally access the entry in componentMap
    if (getFlag) then
      call ESMF_ContainerGetUDT(is%wrap%connectorMap, &
        trim(srcCompLabel)//"-TO-"//trim(dstCompLabel), cmEntry, localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      if (present(comp)) comp = cmEntry%wrap%connector
      if (present(petList)) petList => cmEntry%wrap%petList
    else
      ! return nullified arguments
      if (present(comp)) comp%compp => null()
      if (present(petList)) petList => null()
    endif
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_DriverGetComp - Get all the GridComp child components from a Driver
!
! !INTERFACE:
  ! Private name; call using NUOPC_DriverGetComp()
  recursive subroutine NUOPC_DriverGetAllGridComp(driver, compList, petLists, &
    rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                        :: driver
    type(ESMF_GridComp), pointer, optional     :: compList(:)
    type(ESMF_PtrInt1D), pointer, optional     :: petLists(:)
    integer,             intent(out), optional :: rc 
!
! !DESCRIPTION:
! Get all the GridComp (i.e. Model, Mediator, or Driver) child components from a
! Driver. The incoming {\tt compList} and {\tt petLists} arguments must be 
! unassociated. This means that the user code must explicitly call
! {\tt nullify()} or use the {\tt => null()} syntax on the variables passed in
! as the actual arguments. On return it becomes the responsibility of the caller
! to deallocate any associated {\tt compList} and {\tt petLists} arguments.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                         :: localrc
    integer                         :: stat
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is
    type(ComponentMapEntry)         :: cmEntry
    integer                         :: mapCount
    integer                         :: i

    if (present(rc)) rc = ESMF_SUCCESS

    ! check the incoming pointer
    if (associated(compList)) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="compList must enter unassociated", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
      return  ! bail out
    endif

    ! query the component for info
    call NUOPC_CompGet(driver, name=name, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! get basic information about map
    call ESMF_ContainerGet(is%wrap%componentMap, itemCount=mapCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
      
    ! allocate memory for the compList
    if (present(compList)) then
      allocate(compList(mapCount), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of compList failed.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    
    ! allocate memory for the petLists
    if (present(petLists)) then
      allocate(petLists(mapCount), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of petLists failed.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    
    ! fill the compList and/or petLists
    if (present(compList) .or. present(petLists)) then
      do i=1, mapCount
        call ESMF_ContainerGetUDTByIndex(is%wrap%componentMap, i, &
          cmEntry, ESMF_ITEMORDER_ADDORDER, localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        if (present(compList)) compList(i) = cmEntry%wrap%component
        if (present(petLists)) petLists(i)%ptr => cmEntry%wrap%petList
      enddo
    endif
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_DriverGetComp - Get all the CplComp child components from a Driver
!
! !INTERFACE:
  ! Private name; call using NUOPC_DriverGetComp()
  recursive subroutine NUOPC_DriverGetAllCplComp(driver, compList, petLists, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                        :: driver
    type(ESMF_CplComp),  pointer               :: compList(:)
    type(ESMF_PtrInt1D), pointer, optional     :: petLists(:)
    integer,             intent(out), optional :: rc 
!
! !DESCRIPTION:
! Get all the CplComp (i.e. Connector) child components from a
! Driver. The incoming {\tt compList} and {\tt petLists} arguments must be 
! unassociated. This means that the user code must explicitly call
! {\tt nullify()} or use the {\tt => null()} syntax on the variables passed in
! as the actual arguments. On return it becomes the responsibility of the caller
! to deallocate any associated {\tt compList} and {\tt petLists} arguments.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                         :: localrc
    integer                         :: stat
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is
    type(ConnectorMapEntry)         :: cmEntry
    integer                         :: mapCount
    integer                         :: i

    if (present(rc)) rc = ESMF_SUCCESS

    ! check the incoming pointer
    if (associated(compList)) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="compList must enter unassociated", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
      return  ! bail out
    endif

    ! query the component for info
    call NUOPC_CompGet(driver, name=name, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! get basic information about map
    call ESMF_ContainerGet(is%wrap%connectorMap, itemCount=mapCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
      
    ! allocate memory for the compList
    allocate(compList(mapCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of compList failed.", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! allocate memory for the petLists
    if (present(petLists)) then
      allocate(petLists(mapCount), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of petLists failed.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    
    ! fill the compList and optionally petLists
    do i=1, mapCount
      call ESMF_ContainerGetUDTByIndex(is%wrap%connectorMap, i, &
        cmEntry, ESMF_ITEMORDER_ADDORDER, localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      compList(i) = cmEntry%wrap%connector
      if (present(petLists)) petLists(i)%ptr => cmEntry%wrap%petList
    enddo
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_DriverIngestRunSequence - Ingest the run sequence from FreeFormat
!
! !INTERFACE:
  recursive subroutine NUOPC_DriverIngestRunSequence(driver, freeFormat, &
    autoAddConnectors, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                           :: driver
    type(NUOPC_FreeFormat), intent(in), target    :: freeFormat
    logical,                intent(in),  optional :: autoAddConnectors
    integer,                intent(out), optional :: rc 
!
! !DESCRIPTION:
! Ingest the run sequence from a FreeFormat object and replace the 
! run sequence currently held by the driver. Every line in 
! {\tt freeFormat} corresponds to either a component run sequence element, or 
! is part of a time loop defintion.
!
! Component run sequence elements define the run method of a single component.
! The lines are interpreted sequentially, however, components will execute 
! concurrently as long as this is not prevented by data-dependencies or
! overlapping petLists.
!
! Each line specifies the precise run method phase for a single component 
! instance. For model, mediator, and driver components the format is this:
!
! \begin{verbatim}
!   compLabel [phaseLabel]
! \end{verbatim}
! Here {\tt compLabel} is the label by which the component instance is known to
! the driver. It is optionally followed a {\tt phaseLabel} identifying a
! specific run phase. An example of calling the run phase of the ATM instance 
! that contains the "fast" processes, and is labeled {\tt fast}:
!
! \begin{verbatim}
!   ATM fast
! \end{verbatim}
! By default, i.e. without {\tt phaseLabel}, the first
! registered run method of the component is used.
!
! The format for connector components is different. It looks like this:
!
! \begin{verbatim}
!   srcCompLabel -> dstCompLabel [connectionOptions]
! \end{verbatim}
! A connector instance is uniquely known by the two components it connects, 
! i.e. by {\tt srcCompLabel} and {\tt dstCompLabel}. The syntax requires that
! the token {\tt ->} be specified between source and destination. Optionally
! {\tt connectionOptions} can be supplied using the format discussed 
! under section \ref{connection_options}. The connection options are set
! as attribute {\tt ConnectionOptions} on the respective connector component.
!
! An example of executing the connector
! instance that transfers fields from the ATM component to the OCN component,
! using redistribution for remapping:
! 
! \begin{verbatim}
!   ATM -> OCN :remapMethod=redist
! \end{verbatim}
!
! By default {\tt autoAddConnectors} is {\tt .false.}, which means that all 
! components referenced in the {\tt freeFormat} run sequence, including 
! connectors, must already be available as child components of the {\tt driver}
! component. An error will be returned if this is not the case. 
! However, when {\tt autoAddConnectors} is set to {\tt .true.}, connector
! components encountered in the run sequence that are no already present in 
! the {\tt driver} will be added automatically. The default 
! {\tt NUOPC\_Connector} implementation is used for all automatically added
! connector instances.
!
! Lines that contain a time loop definition have the general format:
!
! \begin{verbatim}
!   @{timeStep|*}[:runDuration]
!     ...
!     ...
!   @
! \end{verbatim}
! Both {\tt timeStep} and {\tt runDuration} are numbers in units of seconds.
! Time loops can be nested and concatenated.
!
! A wildcard "*" character can be specified in place of an actual {\tt timeStep}
! number. In this case the {\tt timeStep} of the associated run clock object
! is set to be equal to the {\tt timeStep} of the time loop one level up in the
! loop nesting hierarchy.
! If a wildcard time step is used for a single outer time loop in the run
! sequence, then the associated run clock is identical to the driver clock and
! must be set explicitly by the driver code, or its parent component.
!
! The {\tt runDuration} specification is optional. If omitted, the duration of
! the associated run clock is set to the {\tt timeStep} of the time loop one
! level up in the loop nesting hierarchy. This ensures that for a single
! nested time loop, the loop returns to the parent loop level at the appropriate
! time.
!
! A simple example of a single time loop with one hour timestep:
!
! \begin{verbatim}
!   @3600
!     ...
!     ...
!   @
! \end{verbatim}
! Each time loop has its own associated clock object. NUOPC manages these clock
! objects, i.e. their creation and destruction, as well as {\tt startTime}, 
! {\tt endTime}, {\tt timeStep} adjustments during the execution. The outer 
! most time loop of the run sequence is a special case. It uses the driver 
! clock itself. If a single outer most loop is defined in the run sequence
! provided by {\tt freeFormat}, this loop becomes the driver loop level 
! directly. Therefore, setting the {\tt timeStep} or {\tt runDuration} for
! the outer most time loop results modifiying the driver clock itself.
! However, for cases with concatenated loops on the upper level of 
! the run sequence in {\tt freeFormat}, a single outer loop is added
! automatically during ingestion, and the driver clock is used for this loop 
! instead.
!
! A more complex run sequence example, that shows component run
! sequence elements outside of time loops, a nested time loop, time step 
! wildcards, explicit duration specifications, and concatenated time loops:
! 
! \begin{verbatim}
!   @100:800
!     ATM -> OCN
!     OCN -> ATM
!     ATM
!     OCN
!     @*
!       OCN -> EXTOCN
!       EXTOCN
!     @
!   @
!   ATM -> EXTATM
!   EXTATM
!   @100:1000
!     ATM -> OCN
!     OCN -> ATM
!     ATM
!     OCN
!   @
! \end{verbatim}
! Here the {\tt timeStep} of the first time loop is explicitly chosen at
! $100s$. The {\tt runDuration} is explicitly set to $800s$. The first time
! loop steps the current time forward for $800s$, for each iteration executing
! ATM-OCN coupling, followed by the nested loop that calls the 
! {\tt OCN -> EXTOCN} and {\tt EXTOCN} components. The nested loop uses a 
!  wildcard {\tt timeStep} and therefore is
! identical to the parent loop level {\tt timeStep} of $100s$. The nested
! {\tt runDuration} is not specified and therefore also defaults to the parent
! time step of $100s$. In other words, the nested loop is executed exactly once
! for every parent loop iteration.
!
! After $800s$ the first time loop is exited, and followed by explicit calls to
! {\tt ATM -> EXTAMT} and {\tt EXTATM} components. Finally the second time loop
! is entered for another $1000s$ {\tt runDuration}. The {\tt timeStep} is again
! explicitly set to $100s$. The second time loop only implements ATM-OCN
! coupling, and no coupling to EXTOCN is implemented. Finally, after $1800s$
! the sequence returns to the driver level loop.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                                         :: localrc
    character(ESMF_MAXSTR)                          :: name, valueString
    type(type_InternalState)                        :: is
    integer                                         :: i, lineCount, tokenCount
    character(len=NUOPC_FreeFormatLen), allocatable :: tokenList(:)
    integer, allocatable                            :: slotStack(:)
    character(len=NUOPC_FreeFormatLen)              :: tempString
    type(ESMF_TimeInterval)                         :: timeStep, runDuration
    type(ESMF_Clock)                                :: internalClock, runClock
    integer                                         :: level, slot, slotHWM
    integer                                         :: slotCount, topLoops
    integer                                         :: colonIndex
    logical                                         :: compIsCreated
    logical                                         :: haveTimeStep
    logical                                         :: haveRunDuration
    real(ESMF_KIND_R8)                              :: seconds
    logical                                         :: optAutoAddConnectors
    type(ESMF_CplComp)                              :: conn
    integer                                         :: verbosity, vInherit
    integer                                         :: profiling
    character(len=10)                               :: vString
    logical                                         :: needDriverTopLoop
    type(NUOPC_FreeFormat), target                  :: freeFormatTemp
    type(NUOPC_FreeFormat), pointer                 :: freeFormatPtr
    integer                                         :: aSec, bSec
    character(len=160)                              :: msgString
    character(len=80)                               :: aString, bString
    logical                                         :: zeroSkip
    integer                                         :: zeroSkipLevel
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    optAutoAddConnectors = .false. ! default
    if (present(autoAddConnectors)) optAutoAddConnectors = autoAddConnectors

    ! query the component for info
    call NUOPC_CompGet(driver, name=name, verbosity=verbosity, &
      profiling=profiling, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! access the FreeFormat lineCount
    call NUOPC_FreeFormatGet(freeFormat, lineCount=lineCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! determine slotCount and potentially automatically add connectors
    ! also detect if a driver top loop is needed
    slotCount = 0
    level = 0
    topLoops = 0
    needDriverTopLoop = .false.
    do i=1, lineCount
      call NUOPC_FreeFormatGetLine(freeFormat, line=i, tokenCount=tokenCount, &
        rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      allocate(tokenList(tokenCount))
      call NUOPC_FreeFormatGetLine(freeFormat, line=i, tokenList=tokenList, &
        rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      if (tokenCount == 1) then
        if (index(trim(tokenList(1)),"@") == 1) then
          ! start or end of a time loop
          slotCount = slotCount + 1
          if (len_trim(tokenList(1))>1) then
            ! start of a time loop
            if (level==0) topLoops = topLoops + 1 ! count top loop
            level = level + 1
          else
            ! end of a time loop
            level = level - 1
          endif
        else
          ! some other element
          if (level==0) needDriverTopLoop = .true.  ! element outside top loop
        endif
      else
        if (level==0) needDriverTopLoop = .true.  ! element outside top loop
        if ((tokenCount == 3) .or. (tokenCount == 4)) then
          ! a connector if the second element is "->"
          if (trim(tokenList(2)) /= "->") then
            call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
              msg="Configuration line incorrectly formatted.", &
              line=__LINE__, &
              file=trim(name)//":"//FILENAME, rcToReturn=rc)
            return  ! bail out
          endif
          ! determine whether this connector component already exists
          call NUOPC_DriverGetCplComp(driver, srcCompLabel=trim(tokenList(1)), &
            dstCompLabel=trim(tokenList(3)), comp=conn, relaxedflag=.true., &
            rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          compIsCreated = ESMF_CplCompIsCreated(conn, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          if (.not.compIsCreated) then
            if (.not.optAutoAddConnectors) then
              call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
                msg="Connector must exist if not setting autoAddConnectors.", &
                line=__LINE__, &
                file=trim(name)//":"//FILENAME, rcToReturn=rc)
              return  ! bail out
            endif
            ! this is a new connector component that needs be added to driver
            call NUOPC_DriverAddComp(driver, srcCompLabel=trim(tokenList(1)), &
              dstCompLabel=trim(tokenList(3)), compSetServicesRoutine=cplSS, &
              comp=conn, rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU,&
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
            ! automatically created connectors inherit Verbosity from parent
            call NUOPC_CompAttributeGet(driver, name="Verbosity", &
              value=valueString, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail
            if (trim(valueString)=="max" .or. trim(valueString)=="high" .or. &
              trim(valueString)=="low" .or. trim(valueString)=="off") then
              ! directly inherit presets
              vString = trim(valueString)
            else
              ! not a preset level: lower 8-bit of parent's Verbosity setting
              vInherit = ibits(verbosity,0,8)
              write(vString,"(I10)") vInherit
            endif
            if (btest(verbosity,13)) then
              write (msgString,"(A)") trim(name)//&
                " - Setting Verbosity on created component to: "// &
                trim(vString)
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
            call NUOPC_CompAttributeSet(conn, name="Verbosity", value=vString, &
              rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU,&
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail
            ! automatically created connectors inherit Profiling from parent
            call NUOPC_CompAttributeGet(driver, name="Profiling", &
              value=valueString, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail
            if (trim(valueString)=="max" .or. trim(valueString)=="high" .or. &
              trim(valueString)=="low" .or. trim(valueString)=="off") then
              ! directly inherit presets
              vString = trim(valueString)
            else
              ! not a preset level: lower 16-bit of parent's Profiling setting
              vInherit = ibits(profiling,0,16)
              write(vString,"(I10)") vInherit
            endif
            if (btest(verbosity,13)) then
              write (msgString,"(A)") trim(name)//&
                " - Setting Profiling on created component to: "// &
                trim(vString)
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
            call NUOPC_CompAttributeSet(conn, name="Profiling", value=vString, &
              rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU,&
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail
          endif
          ! optionally additional options
          if (tokenCount == 4) then
            ! there are additional connection options specified
            ! -> set as Attribute on the connector object
            call NUOPC_CompAttributeSet(conn, name="ConnectionOptions", &
              value=trim(tokenList(4)), rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU,&
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
          endif
        endif
      endif
      deallocate(tokenList)
    enddo

    ! sanity check
    if (mod(slotCount,2)/=0) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="RunSequence has unbalanced '@' tokens.", &
        line=__LINE__, &
        file=trim(name)//":"//FILENAME, rcToReturn=rc)
      return  ! bail out
    endif

    ! OR together the already set needDriverTopLoop, with topLoops condition
    if (topLoops>1) needDriverTopLoop = .true.

    ! Conditionally add driver top loop level
    if (needDriverTopLoop) then
      ! add slots to hold the driver top loop
      lineCount = lineCount + 2
      slotCount = slotCount + 2
      ! make a copy of the incoming FreeFormat object, then add 2 '@' lines
      freeFormatTemp = NUOPC_FreeFormatCreate(freeFormat, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      ! add at the beginning
      call NUOPC_FreeFormatAdd(freeFormatTemp, stringList=(/"@*"/), line=1, &
        rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      ! add at the end
      call NUOPC_FreeFormatAdd(freeFormatTemp, stringList=(/"@"/), rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      ! now start using the modified FreeFormat object
      freeFormatPtr => freeFormatTemp
    else
      ! keep using the unmodified incoming FreeFormat object
      freeFormatPtr => freeFormat
    endif
    
    ! Finish up setting the appropriate slotCount
    slotCount = slotCount / 2     ! divide by two because double counted "@"
    
    ! sanity check
    if (slotCount < 1) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="RunSequence at this point must have at least one time slot.", &
        line=__LINE__, &
        file=trim(name)//":"//FILENAME, rcToReturn=rc)
      return  ! bail out
    endif

    ! allocate the slotStack
    allocate(slotStack(slotCount))

    ! Replace the default RunSequence with a customized one
    call NUOPC_DriverNewRunSequence(driver, slotCount=slotCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! Get driver internalClock
    call ESMF_GridCompGet(driver, clock=internalClock, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! actual ingestion of run sequence, constructing NUOPC representation
    level = 0
    slot = 0
    slotHWM = 0
    zeroSkip = .false.
    do i=1, lineCount
      call NUOPC_FreeFormatGetLine(freeFormatPtr, line=i, &
        tokenCount=tokenCount, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      if (allocated(tokenList)) deallocate(tokenList) ! for zeroSkip cycle case
      allocate(tokenList(tokenCount))
      call NUOPC_FreeFormatGetLine(freeFormatPtr, line=i, tokenList=tokenList, &
        rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      
      ! process the configuration line
      if ((tokenCount < 1) .or. (tokenCount > 4)) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="Configuration line incorrectly formatted.", &
          line=__LINE__, &
          file=trim(name)//":"//FILENAME, rcToReturn=rc)
        return  ! bail out
      elseif (tokenCount == 1) then
        ! either a model or a time step indicator
        if (index(trim(tokenList(1)),"@") == 1) then
          ! found a time step indicator
          tempString=trim(tokenList(1))
          if (len_trim(tempString) > 1) then
            ! entering new time loop level
            level = level + 1
            if (zeroSkip) cycle ! go to next line ---^
            colonIndex = index(tempString,":")
            haveRunDuration = .false. ! reset
            if (colonIndex>0) then
              ! a runDuration is present
              haveRunDuration = .true.
              read(tempString(colonIndex+1:len_trim(tempString)), *) seconds
#if 0
              print *, "found runDuration indicator: ", seconds
#endif
              call ESMF_TimeIntervalSet(runDuration, s_r8=seconds, rc=localrc)
              if (ESMF_LogFoundError(rcToCheck=localrc, &
                msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, &
                file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
              tempString = tempString(1:colonIndex-1) ! truncate at ":"
              ! if runDuration comes in at zero, then skip to end of time loop
              if (abs(seconds)<=tiny(seconds)) then
                zeroSkip = .true.
                zeroSkipLevel = level
                cycle ! go to next line ---^
              endif
            endif
            slotStack(level)=slot
            slot = slotHWM + 1
            slotHWM = slotHWM + 1
            if (slot>1) then
              ! Insert the link to a new slot
              call NUOPC_DriverAddRunElement(driver, slot=slotStack(level), &
                linkSlot=slot, rc=localrc)
              if (ESMF_LogFoundError(rcToCheck=localrc, &
                msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, &
                file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
            haveTimeStep = .false. ! reset
            if (index(tempString,"*") == 2) then
              ! a wildcard indicating to default the timeStep to the parent
              ! timeStep. It may later be reset by user code during Driver
              ! initialization
              if (slotStack(level)==0) then
                call ESMF_ClockGet(internalClock, timeStep=timeStep, rc=localrc)
                if (ESMF_LogFoundError(rcToCheck=localrc, &
                  msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, &
                  file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                  return  ! bail out
              else
                call ESMF_ClockGet(is%wrap%runSeq(slotStack(level))%clock, &
                  timeStep=timeStep, rc=localrc)
                if (ESMF_LogFoundError(rcToCheck=localrc, &
                  msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, &
                  file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                  return  ! bail out
              endif
              haveTimeStep = .true. ! set
            else
              ! assume that what follows the "@" is actually a number
              read(tempString(2:len(tempString)), *) seconds
#if 0
              print *, "found timeStep indicator: ", seconds
#endif
              call ESMF_TimeIntervalSet(timeStep, s_r8=seconds, rc=localrc)
              if (ESMF_LogFoundError(rcToCheck=localrc, &
                msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, &
                file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
              haveTimeStep = .true. ! set
            endif
            ! see if timeStep and runDuration are compatible
            if (haveTimeStep .and. haveRunDuration) then
              if (ceiling(runDuration/timeStep) /= floor(runDuration/timeStep))&
                then
                call ESMF_TimeIntervalGet(timeStep, s=aSec, rc=localrc)
                if (ESMF_LogFoundError(rcToCheck=localrc, &
                  msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, &
                  file=FILENAME, &
                  rcToReturn=rc)) &
                  return  ! bail out
                call ESMF_TimeIntervalGet(runDuration, s=bSec, rc=localrc)
                if (ESMF_LogFoundError(rcToCheck=localrc, &
                  msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, &
                  file=FILENAME, &
                  rcToReturn=rc)) &
                  return  ! bail out
                write (aString, *) aSec
                write (bString, *) bSec
                write (msgString,"(A)") "timeStep="//&
                  trim(adjustl(aString))//&
                  "s is not a divisor of runDuration="//&
                  trim(adjustl(bString))//"s"
                call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg=msgString, &
                  line=__LINE__, &
                  file=FILENAME, &
                  rcToReturn=rc)
                return  ! bail out
              endif
            endif
            if (slot==1) then
              ! this is the top time-loop, runClock is alias to driver clock
              runClock = internalClock
            else
              ! create a new Clock for this slot, starting as driver clock copy
              runClock = ESMF_ClockCreate(internalClock, rc=localrc)
              if (ESMF_LogFoundError(rcToCheck=localrc, &
                msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, &
                file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
            if (haveTimeStep) then
              ! set timeStep
              call ESMF_ClockSet(runClock, timeStep=timeStep, rc=localrc)
              if (ESMF_LogFoundError(rcToCheck=localrc, &
                msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, &
                file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
            if (haveRunDuration) then
              ! set runDuration
              call ESMF_ClockSet(runClock, runDuration=runDuration, rc=localrc)
              if (ESMF_LogFoundError(rcToCheck=localrc, &
                msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, &
                file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
            ! set runClock 
            call NUOPC_DriverSetRunSequence(driver, slot=slot, clock=runClock, &
              rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, &
              msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, &
              file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
          else
            if (zeroSkip) then
              if (level==zeroSkipLevel) zeroSkip = .false.
              level = level - 1
              cycle ! go to next line ---^
            endif
            ! exiting time loop level
            slot = slotStack(level)
            level = level - 1
          endif
        else
          if (zeroSkip) cycle ! go to next line ---^
          ! found a model
          call NUOPC_DriverAddRunElement(driver, slot=slot, &
            compLabel=trim(tokenList(1)), rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=trim(name)//":"//FILENAME, &
            rcToReturn=rc)) &
            return  ! bail out
        endif
      elseif (tokenCount == 2) then
        if (zeroSkip) cycle ! go to next line ---^
        ! a model with a specific phase label
        call NUOPC_DriverAddRunElement(driver, slot=slot, &
          compLabel=trim(tokenList(1)), phaseLabel=trim(tokenList(2)), &
          rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=trim(name)//":"//FILENAME, &
          rcToReturn=rc)) &
          return  ! bail out
      elseif ((tokenCount == 3) .or. (tokenCount == 4)) then
        if (zeroSkip) cycle ! go to next line ---^
        ! a connector if the second element is "->"
        if (trim(tokenList(2)) /= "->") then
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="Configuration line incorrectly formatted.", &
            line=__LINE__, &
            file=trim(name)//":"//FILENAME, rcToReturn=rc)
          return  ! bail out
        endif
        call NUOPC_DriverAddRunElement(driver, slot=slot, &
          srcCompLabel=trim(tokenList(1)), dstCompLabel=trim(tokenList(3)), &
          rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=trim(name)//":"//FILENAME, &
          rcToReturn=rc)) &
          return  ! bail out
      endif    
      
      ! clean-up
      deallocate(tokenList)
    enddo
    ! clean-up
    if (allocated(tokenList)) deallocate(tokenList) ! for zeroSkip cycle case
    deallocate(slotStack)

    if (needDriverTopLoop) then
      ! destroy the temporary FreeFormat object copy
      call NUOPC_FreeFormatDestroy(freeFormatTemp, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=trim(name)//":"//FILENAME, &
        rcToReturn=rc)) &
        return  ! bail out
    endif

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_DriverNewRunSequence - Replace the run sequence in a Driver
!
! !INTERFACE:
  recursive subroutine NUOPC_DriverNewRunSequence(driver, slotCount, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                        :: driver
    integer,             intent(in)            :: slotCount
    integer,             intent(out), optional :: rc 
!
! !DESCRIPTION:
! Replace the current run sequence of the Driver with a new one that has 
! {\tt slotCount} slots. Each slot uses its own clock for time keeping.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                         :: localrc
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is

    if (present(rc)) rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(driver, name=name, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! Deallocate the current RunSequence and add a new one with slotCount
    call NUOPC_RunSequenceDeallocate(is%wrap%runSeq, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call NUOPC_RunSequenceAdd(is%wrap%runSeq, slotCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_DriverPrint - Print internal Driver information
!
! !INTERFACE:
  recursive subroutine NUOPC_DriverPrint(driver, orderflag, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                        :: driver
    logical,             intent(in),  optional :: orderflag
    integer,             intent(out), optional :: rc 
!
! !DESCRIPTION:
! Print internal Driver information. If {\tt orderflag} is provided and set
! to {\tt .true.}, the output is ordered from lowest to highest PET. Setting 
! this flag makes the method collective.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                         :: localrc
    character(ESMF_MAXSTR)          :: name
    type(ESMF_VM)                   :: vm
    logical                         :: forceOrder
    type(type_InternalState)        :: is
    type(ComponentMapEntry)         :: cmEntry
    type(ConnectorMapEntry)         :: cnEntry
    integer                         :: componentMapCount, connectorMapCount
    integer                         :: i, pet, petCount, localPet

    if (present(rc)) rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(driver, name=name, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! get the vm
    call ESMF_GridCompGet(driver, vm=vm, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! get basic information about componentMap and connectorMap
    call ESMF_ContainerGet(is%wrap%componentMap, itemCount=componentMapCount, &
      rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call ESMF_ContainerGet(is%wrap%connectorMap, itemCount=connectorMapCount, &
      rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! deal with ordering
    forceOrder = .false.  ! default
    if (present(orderflag)) then
      forceOrder = orderflag
    endif
    call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    do pet=0, petCount-1
      if (pet == localPet) then
      
        ! Print header
        print *, "NUOPC_DriverPrint for PET:", localPet

        ! Print information about the Model, Mediator, Driver child components
        print *, "  Model, Mediator, and Driver components, in the order"
        print *, "  that they were added to the Driver:"
        do i=1, componentMapCount
          call ESMF_ContainerGetUDTByIndex(is%wrap%componentMap, i, &
            cmEntry, ESMF_ITEMORDER_ADDORDER, localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          print *, i,":  ", trim(cmEntry%wrap%label)
        enddo
        
        ! Print information about the Connector components
        print *, "  Connector components, in the order"
        print *, "  that they were added to the Driver:"
        do i=1, connectorMapCount
          call ESMF_ContainerGetUDTByIndex(is%wrap%connectorMap, i, &
            cnEntry, ESMF_ITEMORDER_ADDORDER, localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          print *, i,":  ", trim(cnEntry%wrap%label)
        enddo
        
        ! Print the RunSequence
        call NUOPC_RunSequencePrint(is%wrap%runSeq, logflag=.false., rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out

      endif
      if (forceOrder) then
        call ESMF_VMBarrier(vm, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
    enddo
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_DriverSetRunSequence - Set internals of RunSequence slot
!
! !INTERFACE:
  ! Private name; call using NUOPC_DriverSetRunSequence()
  recursive subroutine NUOPC_DriverSetRunSequence(driver, slot, clock, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                        :: driver
    integer,             intent(in)            :: slot
    type(ESMF_Clock),    intent(in)            :: clock
    integer,             intent(out), optional :: rc 
!
! !DESCRIPTION:
! Set the {\tt clock} in the run sequence under {\tt slot} of the Driver.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                         :: localrc
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is

    if (present(rc)) rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(driver, name=name, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! check if slot index is valid
    if (slot<=0 .or. slot>size(is%wrap%runSeq)) then
      ! bail out with error
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="Slot index is out of bounds.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
      return  ! bail out
    endif

    ! Set clock of the selected RunSequence slot
    call NUOPC_RunSequenceSet(is%wrap%runSeq(slot), clock=clock, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------

  recursive subroutine IInitAdvertise(driver, importState, exportState, clock, &
    rc)
    type(ESMF_GridComp)  :: driver
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    character(*), parameter   :: rName="IInitAdvertise"
    character(ESMF_MAXSTR)    :: name
    integer                   :: verbosity, profiling
    logical                   :: stateIsCreated
    logical                   :: isSet, needMirror
    character(len=80)         :: hierarchyProtocol

    rc = ESMF_SUCCESS
    
    ! query the component for info
    call NUOPC_CompGet(driver, name=name, verbosity=verbosity, &
      profiling=profiling, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle profiling
    if (btest(profiling,9)) then
      call ESMF_TraceRegionEnter("Leading Barrier", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_VMBarrier(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_TraceRegionExit("Leading Barrier", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    if (btest(profiling,0)) then
      call ESMF_TraceRegionEnter(rName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    ! intro
    call NUOPC_LogIntro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    call NUOPC_CompAttributeGet(driver, name="HierarchyProtocol", &
      isSet=isSet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    needMirror = .not.isSet  ! by default request mirroring
    
    if (.not.needMirror) then
      ! see if HieraryProtocol attribute explicitly requests mirroring
      call NUOPC_CompAttributeGet(driver, name="HierarchyProtocol", &
        value=hierarchyProtocol, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      if (trim(hierarchyProtocol)=="PushUpAllExportsAndUnsatisfiedImports" &
        .or. trim(hierarchyProtocol)=="Explorer") &
        needMirror = .true.
    endif

    stateIsCreated = ESMF_StateIsCreated(importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    if (needMirror.and.stateIsCreated) then
      ! request that connectors transfer all fields into the importState
      call NUOPC_SetAttribute(importState, name="FieldTransferPolicy", &
        value="transferAll", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif
    
    stateIsCreated = ESMF_StateIsCreated(exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    if (needMirror.and.stateIsCreated) then
      ! request that connectors transfer all fields into the exportState
      call NUOPC_SetAttribute(exportState, name="FieldTransferPolicy", &
        value="transferAll", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif

    ! extro
    call NUOPC_LogExtro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle profiling
    if (btest(profiling,0)) then
      call ESMF_TraceRegionExit(rName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

  end subroutine
  
  !-----------------------------------------------------------------------------

  recursive subroutine IInitAdvertiseFinish(driver, importState, exportState, &
    clock, rc)
    type(ESMF_GridComp)  :: driver
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    character(*), parameter   :: rName="IInitAdvertiseFinish"
    character(ESMF_MAXSTR)    :: name
    integer                   :: verbosity, profiling
    logical                   :: stateIsCreated
    logical                   :: isSet, setSharePolicy
    character(len=80)         :: hierarchyProtocol

    rc = ESMF_SUCCESS
    
    ! query the component for info
    call NUOPC_CompGet(driver, name=name, verbosity=verbosity, &
      profiling=profiling, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle profiling
    if (btest(profiling,9)) then
      call ESMF_TraceRegionEnter("Leading Barrier", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_VMBarrier(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_TraceRegionExit("Leading Barrier", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    if (btest(profiling,0)) then
      call ESMF_TraceRegionEnter(rName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    ! intro
    call NUOPC_LogIntro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    call NUOPC_CompAttributeGet(driver, name="HierarchyProtocol", &
      isSet=isSet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    setSharePolicy = .not.isSet  ! by default request checking

    if (.not.setSharePolicy) then
      ! see if HieraryProtocol attribute explicitly requests mirroring
      call NUOPC_CompAttributeGet(driver, name="HierarchyProtocol", &
        value=hierarchyProtocol, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      if (trim(hierarchyProtocol)=="PushUpAllExportsAndUnsatisfiedImports") &
        setSharePolicy = .true.
    endif

    stateIsCreated = ESMF_StateIsCreated(importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    if (stateIsCreated) then
      ! reset FieldTransferPolicy to prevent interaction w upper hierarchy layer
      call NUOPC_SetAttribute(importState, name="FieldTransferPolicy", &
        value="transferNone", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      if (setSharePolicy) then
        ! set the SharePolicy on all the Fields in the state
        call setSharePolicies(importState, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      endif
    endif
    
    stateIsCreated = ESMF_StateIsCreated(exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    if (stateIsCreated) then
      ! reset FieldTransferPolicy to prevent interaction w upper hierarchy layer
      call NUOPC_SetAttribute(exportState, name="FieldTransferPolicy", &
        value="transferNone", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      if (setSharePolicy) then
        ! set the SharePolicy on all the Fields in the state
        call setSharePolicies(exportState, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      endif
    endif

    ! extro
    call NUOPC_LogExtro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle profiling
    if (btest(profiling,0)) then
      call ESMF_TraceRegionExit(rName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

  contains
  
    !---------------------------------------------------------------------------

    recursive subroutine setSharePolicies(state, rc)
      type(ESMF_State)     :: state
      integer, intent(out) :: rc
      
      ! local variables    
      integer                         :: i
      type(ESMF_Field), pointer       :: fieldList(:)
      character(ESMF_MAXSTR), pointer :: itemNameList(:)

      rc = ESMF_SUCCESS

      nullify(fieldList)
      nullify(itemNameList)
      call NUOPC_GetStateMemberLists(state, itemNameList=itemNameList, &
        fieldList=fieldList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      if (associated(fieldList)) then
        do i=1, size(fieldList)
          ! Set the SharePolicy attributes
          call NUOPC_SetAttribute(fieldList(i), name="SharePolicyGeomObject", &
            value="share", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          call NUOPC_SetAttribute(fieldList(i), name="SharePolicyField", &
            value="share", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        enddo
      endif
      if (associated(fieldList)) deallocate(fieldList)
      if (associated(itemNameList)) deallocate(itemNameList)
      
    end subroutine

    !---------------------------------------------------------------------------

  end subroutine

  !-----------------------------------------------------------------------------

  recursive subroutine IInitModifyCplLists(driver, importState, exportState, &
    clock, rc)
    type(ESMF_GridComp)  :: driver
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    character(*), parameter   :: rName="IInitModifyCplLists"
    character(ESMF_MAXSTR)    :: name, connectorName
    integer                   :: verbosity, profiling
    integer                   :: userrc
    logical                   :: existflag
    logical                   :: areServicesSet
    type(ESMF_CplComp)        :: connector
    integer                   :: i
    type(type_InternalState)  :: is
    type(ESMF_CplComp), pointer   :: connectorList(:)
    character(len=400)            :: value
    logical                       :: isSet
    logical                       :: forceConsumerConnection

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(driver, name=name, verbosity=verbosity, &
      profiling=profiling, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle profiling
    if (btest(profiling,9)) then
      call ESMF_TraceRegionEnter("Leading Barrier", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_VMBarrier(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_TraceRegionExit("Leading Barrier", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    if (btest(profiling,0)) then
      call ESMF_TraceRegionEnter(rName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    ! intro
    call NUOPC_LogIntro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! see whether consumerConnection must be forced due to hierarchy protocol
    call NUOPC_CompAttributeGet(driver, name="HierarchyProtocol", &
      isSet=isSet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    forceConsumerConnection = isSet  ! default hierarchy protocol does not force
    
    if (isSet) then
      ! Check the HierarchyProtocol to make the decision about forcing
      call NUOPC_CompAttributeGet(driver, name="HierarchyProtocol", &
        value=value, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      if (trim(value)=="ConnectProvidedFields") &
        forceConsumerConnection = .true.
    endif

    ! add REMAPMETHOD=redist option to all of the CplList entries for all
    ! Connectors to/from driver-self
    do i=1, is%wrap%modelCount
      ! connector to-driver-self
      connector = is%wrap%connectorComp(i,0)
      areServicesSet = &
        NUOPC_CompAreServicesSet(connector, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      if (areServicesSet) then
        if (btest(verbosity,4)) then
          call ESMF_CplCompGet(connector, name=connectorName, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          call ESMF_LogWrite(trim(name)// &
            ": calling into modifyCplList() with driver self exportState for "// &
            trim(connectorName)//":", ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
        endif
        call modifyCplList(connector, exportState, ":REMAPMETHOD=redist", &
          forceConsumerConnection=forceConsumerConnection, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
    enddo
    do i=1, is%wrap%modelCount
      ! connector from-driver-self
      connector = is%wrap%connectorComp(0,i)
      areServicesSet = &
        NUOPC_CompAreServicesSet(connector, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      if (areServicesSet) then
        if (btest(verbosity,4)) then
          call ESMF_CplCompGet(connector, name=connectorName, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          call ESMF_LogWrite(trim(name)// &
            ": calling into modifyCplList() with driver self importState for "// &
            trim(connectorName)//":", ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
        endif
        call modifyCplList(connector, importState, ":REMAPMETHOD=redist", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
    enddo
    
    ! each connector to add connection options to its CplList as per Attribute
    nullify(connectorList)
    call NUOPC_DriverGetComp(driver, connectorList, rc=rc)    
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    do i=1, size(connectorList)
      call NUOPC_CompAttributeGet(connectorList(i), name="ConnectionOptions", &
        value=value, isSet=isSet, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      if (isSet) then
        call addCplListOption(connectorList(i), value, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
    enddo
    deallocate(connectorList)

    ! SPECIALIZE by calling into optional attached method allowing modification
    ! of the "CplList" metadata on child Connectors.
    if (btest(profiling,1)) then
      call ESMF_TraceRegionEnter("label_ModifyCplLists")
    endif
    call ESMF_MethodExecute(driver, label=label_ModifyCplLists, &
      existflag=existflag, userRc=userrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=userrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (btest(profiling,1)) then
      call ESMF_TraceRegionExit("label_ModifyCplLists")
    endif
      
    ! extro
    call NUOPC_LogExtro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle profiling
    if (btest(profiling,0)) then
      call ESMF_TraceRegionExit(rName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

  contains
  
    !---------------------------------------------------------------------------

    recursive subroutine modifyCplList(connector, state, appendString, &
      forceConsumerConnection, rc)
      type(ESMF_CplComp)              :: connector
      type(ESMF_State)                :: state ! driver state connector interacts
      character(len=*)                :: appendString
      logical, optional               :: forceConsumerConnection
      integer, intent(out)            :: rc
      ! local variables
      integer                         :: j, jj, stat
      integer                         :: cplListSize, cplSetListSize
      character(len=400), allocatable :: cplList(:), cplSetList(:)
      character(ESMF_MAXSTR), pointer :: chopStringList(:)
      character(ESMF_MAXSTR)          :: cplName
      character(ESMF_MAXSTR), pointer :: stateStandardNameList(:)
      type(ESMF_Field),       pointer :: stateFieldList(:)
      character(ESMF_MAXSTR), pointer :: stateCplSetList(:)
      logical                         :: match, connected
      logical                         :: producerConnected, consumerConnected
      logical                         :: forceConsumerConnectionOpt
      character(ESMF_MAXSTR)          :: msgString
      
      ! get the cplList Attribute
      call NUOPC_CompAttributeGet(connector, name="CplList", &
        itemCount=cplListSize, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      ! get the cplSetList Attribute
      call NUOPC_CompAttributeGet(connector, name="CplSetList", &
        itemCount=cplSetListSize, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      ! check the cplSetList size
      if (cplListSize /= cplSetListSize) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="Bad internal error - CplSetList size must match CplList size!",&
          line=__LINE__, file=trim(name)//":"//FILENAME, &
          rcToReturn=rc)
        return  ! bail out
      endif
      ! deal with optional forceConsumerConnection argument
      forceConsumerConnectionOpt = .false. ! default
      if (present(forceConsumerConnection)) &
        forceConsumerConnectionOpt = forceConsumerConnection
      if (cplListSize>0) then
        ! there are entries in the cplList
        allocate(cplList(cplListSize), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of internal cplList() failed.", &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        call NUOPC_CompAttributeGet(connector, name="CplList", &
          valueList=cplList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        allocate(cplSetList(cplSetListSize), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of internal cplList() failed.", &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        call NUOPC_CompAttributeGet(connector, name="CplSetList", &
          valueList=cplSetList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        ! get the state std lists
        nullify(stateStandardNameList)
        nullify(stateFieldList)
        nullify(stateCplSetList)
        call NUOPC_GetStateMemberLists(state, stateStandardNameList, &
          fieldList=stateFieldList, cplSetList=stateCplSetList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        ! go through all of the entries in the cplList
        nullify(chopStringList)
        do j=1, cplListSize
          call NUOPC_ChopString(cplList(j), chopChar=":", &
            chopStringList=chopStringList, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          cplName = chopStringList(1) ! first part is the standard name
          deallocate(chopStringList)
          ! look for the associated field in the state
          do jj=1, size(stateFieldList)
            ! must consider cplSet
            if (.NOT.(cplSetList(j).EQ.stateCplSetList(jj))) cycle
            match = NUOPC_FieldDictionaryMatchSyno( &
              stateStandardNameList(jj), cplName, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (match) then
              ! optionally force the consumerConnection
              if (forceConsumerConnectionOpt) then
                call NUOPC_SetAttribute(stateFieldList(jj), &
                  name="ConsumerConnection", value="true", rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
              endif
              ! inspect the connected status of the associated field
              call checkConnection(stateFieldList(jj), connected, &
                producerConnected, consumerConnected, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
              if (btest(verbosity,4)) then
                write(msgString,*) trim(cplName)//&
                  " connected:", connected, &
                  " producerConnected:", producerConnected, &
                  " consumerConnected:", consumerConnected
                call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                  return  ! bail out
              endif
            endif
          enddo
          ! set remapping to redist
          cplList(j) = trim(cplList(j))//trim(appendString)
        enddo
        ! store the modified cplList in CplList attribute of connector
        call NUOPC_CompAttributeSet(connector, &
          name="CplList", valueList=cplList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        ! clean-up
        if (associated(stateStandardNameList)) deallocate(stateStandardNameList)
        if (associated(stateFieldList)) deallocate(stateFieldList)
        if (associated(stateCplSetList)) deallocate(stateCplSetList)
        deallocate(cplList, cplSetList)
      endif
    end subroutine

    !---------------------------------------------------------------------------

    recursive subroutine addCplListOption(connector, appendString, rc)
      type(ESMF_CplComp)              :: connector
      character(len=*)                :: appendString
      integer, intent(out)            :: rc
      ! local variables
      integer                         :: j, cplListSize
      character(len=400), allocatable :: cplList(:)
      call NUOPC_CompAttributeGet(connector, name="CplList", &
        itemCount=cplListSize, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      if (cplListSize>0) then
        allocate(cplList(cplListSize))
        call NUOPC_CompAttributeGet(connector, name="CplList", &
          valueList=cplList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        ! go through all of the entries in the cplList and add options
        do j=1, cplListSize
          ! set remapping options
          cplList(j) = trim(cplList(j))//trim(appendString)
        enddo
        ! store the modified cplList in CplList attribute of connector
        call NUOPC_CompAttributeSet(connector, &
          name="CplList", valueList=cplList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        deallocate(cplList)
      endif
    end subroutine

  end subroutine

  !-----------------------------------------------------------------------------

  recursive subroutine IInitCheck(driver, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: driver
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    character(*), parameter   :: rName="IInitCheck"
    character(ESMF_MAXSTR)    :: name, connectorName
    integer                   :: verbosity, profiling
    logical                   :: stateIsCreated
    logical                   :: isSet, checkImport
    character(len=80)         :: hierarchyProtocol
    logical                   :: areServicesSet
    type(ESMF_CplComp)        :: connector
    integer                   :: i
    type(type_InternalState)  :: is

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(driver, name=name, verbosity=verbosity, &
      profiling=profiling, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle profiling
    if (btest(profiling,9)) then
      call ESMF_TraceRegionEnter("Leading Barrier", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_VMBarrier(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_TraceRegionExit("Leading Barrier", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    if (btest(profiling,0)) then
      call ESMF_TraceRegionEnter(rName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    ! intro
    call NUOPC_LogIntro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! first deal with the cleanup depending on connected status
    ! Connectors to/from driver-self
    do i=1, is%wrap%modelCount
      ! connector to-driver-self
      connector = is%wrap%connectorComp(i,0)
      areServicesSet = &
        NUOPC_CompAreServicesSet(connector, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      if (areServicesSet) then
        if (btest(verbosity,4)) then
          call ESMF_CplCompGet(connector, name=connectorName, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          call ESMF_LogWrite(trim(name)// &
            ": calling into cleanupCplList() with driver self exportState for "// &
            trim(connectorName)//":", ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
        endif
        call cleanupCplList(connector, exportState, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
    enddo
    do i=1, is%wrap%modelCount
      ! connector from-driver-self
      connector = is%wrap%connectorComp(0,i)
      areServicesSet = &
        NUOPC_CompAreServicesSet(connector, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      if (areServicesSet) then
        if (btest(verbosity,4)) then
          call ESMF_CplCompGet(connector, name=connectorName, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          call ESMF_LogWrite(trim(name)// &
            ": calling into cleanupCplList() with driver self importState for "// &
            trim(connectorName)//":", ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
        endif
        call cleanupCplList(connector, importState, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
    enddo

    ! Now go into checking the states wrt connected status...

    call NUOPC_CompAttributeGet(driver, name="HierarchyProtocol", &
      isSet=isSet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    checkImport = .not.isSet  ! by default request checking
    
    if (.not.checkImport) then
      ! Check the HierarchyProtocol to make the decision about checking the
      ! connectedness of fields in the importState. 
      ! E.g. for explorer application there should be no checking because it is
      ! the hierarchy driver itself that interacts with the child.
      call NUOPC_CompAttributeGet(driver, name="HierarchyProtocol", &
        value=hierarchyProtocol, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      if (trim(hierarchyProtocol)=="PushUpAllExportsAndUnsatisfiedImports") &
        checkImport = .true.
    endif

    stateIsCreated = ESMF_StateIsCreated(importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    if (checkImport.and.stateIsCreated) then
      ! - check all connected fields in importState
      call checkConnections(importState, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif
    
    stateIsCreated = ESMF_StateIsCreated(exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    if (stateIsCreated) then
      ! - check all connected fields in exportState
      call checkConnections(exportState, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif
  
    ! extro
    call NUOPC_LogExtro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle profiling
    if (btest(profiling,0)) then
      call ESMF_TraceRegionExit(rName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

  contains
  
    !---------------------------------------------------------------------------

    recursive subroutine cleanupCplList(connector, state, rc)
      type(ESMF_CplComp)              :: connector
      type(ESMF_State)                :: state ! driver state connector interacts
      integer, intent(out)            :: rc
      ! local variables
      integer                         :: j, jj, jjj, stat
      integer                         :: cplListSize, cplSetListSize
      character(len=400), allocatable :: cplList(:), cplSetList(:)
      character(len=400), allocatable :: cplListNew(:), cplSetListNew(:)
      character(ESMF_MAXSTR), pointer :: chopStringList(:)
      character(ESMF_MAXSTR)          :: cplName, fieldName
      character(ESMF_MAXSTR), pointer :: stateStandardNameList(:)
      type(ESMF_Field),       pointer :: stateFieldList(:)
      type(ESMF_State),       pointer :: stateStateList(:)
      character(ESMF_MAXSTR), pointer :: stateCplSetList(:)
      logical                         :: match, connected
      logical                         :: producerConnected, consumerConnected
      character(ESMF_MAXSTR)          :: msgString
      
      ! get the cplList Attribute
      call NUOPC_CompAttributeGet(connector, name="CplList", &
        itemCount=cplListSize, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      ! get the cplSetList Attribute
      call NUOPC_CompAttributeGet(connector, name="CplSetList", &
        itemCount=cplSetListSize, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      ! check the cplSetList size
      if (cplListSize /= cplSetListSize) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="Bad internal error - CplSetList size must match CplList size!",&
          line=__LINE__, file=trim(name)//":"//FILENAME, &
          rcToReturn=rc)
        return  ! bail out
      endif
      if (cplListSize>0) then
        ! there are entries in the cplList
        allocate(cplList(cplListSize), cplListNew(cplListSize), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of internal cplList() failed.", &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        call NUOPC_CompAttributeGet(connector, name="CplList", &
          valueList=cplList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        allocate(cplSetList(cplSetListSize), cplSetListNew(cplSetListSize), &
          stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of internal cplList() failed.", &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        call NUOPC_CompAttributeGet(connector, name="CplSetList", &
          valueList=cplSetList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        ! get the state std lists
        nullify(stateStandardNameList)
        nullify(stateFieldList)
        nullify(stateStateList)
        nullify(stateCplSetList)
        call NUOPC_GetStateMemberLists(state, stateStandardNameList, &
          fieldList=stateFieldList, stateList=stateStateList, &
          cplSetList=stateCplSetList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        ! go through all of the entries in the cplList
        nullify(chopStringList)
        jjj=0 ! initialize
        do j=1, cplListSize
          call NUOPC_ChopString(cplList(j), chopChar=":", &
            chopStringList=chopStringList, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          cplName = chopStringList(1) ! first part is the standard name
          deallocate(chopStringList)
          ! look for the associated field in the state
          do jj=1, size(stateFieldList)
            ! must consider cplSet
            if (.NOT.(cplSetList(j).EQ.stateCplSetList(jj))) cycle
            match = NUOPC_FieldDictionaryMatchSyno( &
              stateStandardNameList(jj), cplName, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (match) then
              ! inspect the connected status of the associated field
              call checkConnection(stateFieldList(jj), connected, &
                producerConnected, consumerConnected, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
              if (btest(verbosity,4)) then
                write(msgString,*) trim(cplName)// &
                  " in CplSet: ", trim(cplSetList(j)), &
                  " connected:", connected, &
                  " producerConnected:", producerConnected, &
                  " consumerConnected:", consumerConnected
                call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                  return  ! bail out
              endif
              if (connected.and. .not.consumerConnected) then
                ! remove the field from the state
                call ESMF_FieldGet(stateFieldList(jj), name=fieldName, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                  return  ! bail out
                if (btest(verbosity,4)) then
                  write(msgString,*) "- removing field: ", trim(fieldName), &
                    " in CplSet: ", trim(cplSetList(j))
                  call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
                  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                    return  ! bail out
                endif
                call ESMF_StateRemove(stateStateList(jj), &
                  itemNameList=(/fieldName/), rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                  return  ! bail out
              else
                ! copy the CplList and CplSetList entries to new lists
                jjj = jjj+1
                cplListNew(jjj) = cplList(j)
                cplSetListNew(jjj) = cplSetList(j)
              endif
            endif
          enddo
        enddo
        ! store the cplListNew and cplSetListNew in the connector
        if (jjj==0) then
          ! special treatment for the case where no elements are left in list
          call NUOPC_CompAttributeReset(connector, &
            attrList=(/"CplList"/), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          call NUOPC_CompAttributeReset(connector, &
            attrList=(/"CplSetList"/), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
        else
          call NUOPC_CompAttributeSet(connector, &
            name="CplList", valueList=cplListNew(1:jjj), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          call NUOPC_CompAttributeSet(connector, &
            name="CplSetList", valueList=cplSetListNew(1:jjj), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
        endif
        ! clean-up
        if (associated(stateStandardNameList)) deallocate(stateStandardNameList)
        if (associated(stateFieldList)) deallocate(stateFieldList)
        if (associated(stateStateList)) deallocate(stateStateList)
        if (associated(stateCplSetList)) deallocate(stateCplSetList)
        deallocate(cplList, cplSetList)
        deallocate(cplListNew, cplSetListNew)
      endif
    end subroutine

    !---------------------------------------------------------------------------

    recursive subroutine checkConnections(state, rc)
      type(ESMF_State)     :: state
      integer, intent(out) :: rc
      
      ! local variables    
      integer                         :: j
      type(ESMF_Field), pointer       :: fieldList(:)
      character(ESMF_MAXSTR), pointer :: itemNameList(:)
      logical                         :: connected
      logical                         :: producerConnected, consumerConnected
      character(ESMF_MAXSTR)          :: stateName, msgString

      rc = ESMF_SUCCESS

      nullify(fieldList)
      nullify(itemNameList)
      call NUOPC_GetStateMemberLists(state, itemNameList=itemNameList, &
        fieldList=fieldList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      if (associated(fieldList)) then
        do j=1, size(fieldList)
          call checkConnection(fieldList(j), connected, producerConnected, &
            consumerConnected, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (btest(verbosity,4)) then
            write(msgString,*) trim(itemNameList(j))//&
              " connected:", connected, &
              " producerConnected:", producerConnected, &
              " consumerConnected:", consumerConnected
            call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
          endif
          if (connected .and. .not.producerConnected) then
            ! a connected field in a Driver state must have a ProducerConnection
            ! -> bail with error
            call ESMF_StateGet(state, name=stateName, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
              msg="Connected Field in Driver State "//trim(stateName)//&
              " must have ProducerConnection: "//trim(itemNameList(j)), &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
            return ! bail out
          endif
        enddo
      endif
      if (associated(fieldList)) deallocate(fieldList)
      if (associated(itemNameList)) deallocate(itemNameList)
      
    end subroutine

    !---------------------------------------------------------------------------
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  recursive subroutine checkConnection(field, connected, producerConnected, &
    consumerConnected, rc)
    type(ESMF_Field), intent(in)  :: field
    logical, intent(out)          :: connected
    logical, intent(out)          :: producerConnected
    logical, intent(out)          :: consumerConnected
    integer, intent(out)          :: rc
    ! local variables
    character(len=80)             :: value
    
    rc = ESMF_SUCCESS
    
    call NUOPC_GetAttribute(field, name="Connected", &
      value=value, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    connected = (value=="true")
    call NUOPC_GetAttribute(field, name="ProducerConnection", &
      value=value, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    producerConnected = (value/="open")
    call NUOPC_GetAttribute(field, name="ConsumerConnection", &
      value=value, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    consumerConnected = (value/="open")
  end subroutine

  !-----------------------------------------------------------------------------

  recursive subroutine rmFieldsWoConsumerConnection(state, name, rc)
    type(ESMF_State)     :: state
    character(len=*)     :: name
    integer, intent(out) :: rc
    
    ! local variables    
    integer                         :: i
    type(ESMF_Field), pointer       :: fieldList(:)
    character(ESMF_MAXSTR), pointer :: itemNameList(:)
    type(ESMF_State),       pointer :: stateList(:)
    logical                         :: consumerConnected
    character(ESMF_MAXSTR)          :: stateName, fieldName
    character(len=80)               :: value

    rc = ESMF_SUCCESS

    nullify(fieldList)
    nullify(itemNameList)
    nullify(stateList)
    call NUOPC_GetStateMemberLists(state, itemNameList=itemNameList, &
      fieldList=fieldList, stateList=stateList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (associated(fieldList)) then
      do i=1, size(fieldList)
        call NUOPC_GetAttribute(fieldList(i), name="ConsumerConnection", &
          value=value, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        consumerConnected = (value/="open")
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        if (.not.consumerConnected) then
          ! this field's ConsumerConnection is not set -> remove it from state
          call ESMF_FieldGet(fieldList(i), name=fieldName, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          call ESMF_StateRemove(stateList(i), (/fieldName/), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        endif
      enddo
    endif
    if (associated(fieldList)) deallocate(fieldList)
    if (associated(itemNameList)) deallocate(itemNameList)
    if (associated(stateList)) deallocate(stateList)
    
  end subroutine

  !-----------------------------------------------------------------------------

  recursive subroutine IInitRealize(driver, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: driver
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    character(*), parameter   :: rName="IInitRealize"
    character(ESMF_MAXSTR)    :: name
    integer                   :: verbosity, profiling
    logical                   :: stateIsCreated

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(driver, name=name, verbosity=verbosity, &
      profiling=profiling, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle profiling
    if (btest(profiling,9)) then
      call ESMF_TraceRegionEnter("Leading Barrier", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_VMBarrier(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_TraceRegionExit("Leading Barrier", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    if (btest(profiling,0)) then
      call ESMF_TraceRegionEnter(rName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    ! intro
    call NUOPC_LogIntro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    stateIsCreated = ESMF_StateIsCreated(importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    if (stateIsCreated) then
      ! - complete all the fields in the importState
      call completeAllFields(importState, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif

    stateIsCreated = ESMF_StateIsCreated(exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    if (stateIsCreated) then
      ! - complete all the fields in the exportState
      call completeAllFields(exportState, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif

    ! extro
    call NUOPC_LogExtro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle profiling
    if (btest(profiling,0)) then
      call ESMF_TraceRegionExit(rName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

  contains
  
    !---------------------------------------------------------------------------

    recursive subroutine completeAllFields(state, rc)
      type(ESMF_State)     :: state
      integer, intent(out) :: rc
      
      ! local variables    
      integer                         :: i
      type(ESMF_Field), pointer       :: fieldList(:)
      character(ESMF_MAXSTR), pointer :: itemNameList(:)
      type(ESMF_State),       pointer :: stateList(:)
      integer                         :: itemCount, stat
      integer                         :: ulbCount, uubCount
      logical                         :: isPresent
      integer(ESMF_KIND_I4), pointer  :: ungriddedLBound(:), ungriddedUBound(:)
      integer(ESMF_KIND_I4), pointer  :: gridToFieldMap(:)
      integer                         :: tk
      type(ESMF_TypeKind_Flag)        :: tkf
      character(len=80)               :: value

      rc = ESMF_SUCCESS

      nullify(fieldList)
      nullify(itemNameList)
      nullify(stateList)
      call NUOPC_GetStateMemberLists(state, itemNameList=itemNameList, &
        fieldList=fieldList, stateList=stateList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      if (associated(fieldList)) then
        do i=1, size(fieldList)
          
          ! See if the Field is shared. If so, don't need to do anything
          ! here, because the Connector will have realized the Fields 
          ! automatically, using reference sharing. Otherwise realize here.
          
          call NUOPC_GetAttribute(fieldList(i), name="ShareStatusField", &
            value=value, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          
          if (trim(value)=="not shared") then
            ! not shared -> must complete the field here
            call NUOPC_Realize(stateList(i), fieldName=itemNameList(i), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          endif

        enddo
      endif
      if (associated(fieldList)) deallocate(fieldList)
      if (associated(itemNameList)) deallocate(itemNameList)
      if (associated(stateList)) deallocate(stateList)
      
    end subroutine

    !---------------------------------------------------------------------------

  end subroutine
  
  !-----------------------------------------------------------------------------

  recursive subroutine InternalInitializeComplete(driver, importState, &
    exportState, clock, rc)
    type(ESMF_GridComp)  :: driver
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    character(*), parameter       :: rName="InternalInitializeComplete"
    character(ESMF_MAXSTR)        :: name, valueString
    integer                       :: verbosity, profiling
    type(ESMF_Clock)              :: internalClock
    type(ESMF_Time)               :: time
    type(ESMF_Field), allocatable :: fieldList(:)
    character(ESMF_MAXSTR)        :: fieldName
    integer                       :: i
    logical                       :: stateIsCreated
    logical                       :: isAtTime

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(driver, name=name, verbosity=verbosity, &
      profiling=profiling, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle profiling
    if (btest(profiling,9)) then
      call ESMF_TraceRegionEnter("Leading Barrier", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_VMBarrier(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_TraceRegionExit("Leading Barrier", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    if (btest(profiling,0)) then
      call ESMF_TraceRegionEnter(rName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    ! intro
    call NUOPC_LogIntro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! conditionally handle how InitializeDataComplete is set
    stateIsCreated = ESMF_StateIsCreated(exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    if (stateIsCreated) then
      call ESMF_GridCompGet(driver, clock=internalClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
      call ESMF_ClockGet(internalClock, currTime=time, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
      isAtTime = NUOPC_IsAtTime(exportState, time, fieldList=fieldList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out

      if (isAtTime) then
        ! indicate that data initialization is complete 
        ! (breaking out of init-loop)
        call NUOPC_CompAttributeSet(driver, &
          name="InitializeDataComplete", value="true", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) &
          return  ! bail out
        if (btest(verbosity,11)) then
          call ESMF_LogWrite(trim(name)//&
            ": all fields in exportState at expected time.", &
            ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        endif
      else
        if (btest(verbosity,11)) then
          do i=1, size(fieldList)
            call ESMF_FieldGet(fieldList(i), name=fieldName, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) &
              return  ! bail out
            call ESMF_LogWrite(trim(name)//&
              ": field in exportState not at expected time: "&
              //trim(fieldName), ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) &
              return  ! bail out
          enddo
        endif
        deallocate(fieldList)
      endif
    else
      ! indicate that data initialization is complete 
      ! (breaking out of init-loop)
      call NUOPC_CompAttributeSet(driver, &
        name="InitializeDataComplete", value="true", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
    endif

    if (btest(verbosity,11)) then
      call NUOPC_CompAttributeGet(driver, name="InitializeDataComplete", &
        value=valueString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
      call ESMF_LogWrite(trim(name)//": InitializeDataComplete='"// &
        trim(valueString)//"'", ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif

    ! extro
    call NUOPC_LogExtro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle profiling
    if (btest(profiling,0)) then
      call ESMF_TraceRegionExit(rName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

  end subroutine

  !-----------------------------------------------------------------------------

end module
