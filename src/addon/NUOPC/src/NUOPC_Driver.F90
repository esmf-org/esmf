! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define FILENAME "src/addon/NUOPC/src/NUOPC_Driver.F90"
!==============================================================================

#define DEBUG_off

module NUOPC_Driver

  !-----------------------------------------------------------------------------
  ! Generic Driver Component
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_RunSequenceDef
  use NUOPC_Connector, only: cplSS => SetServices
  
  implicit none
  
  private
  
  public &
    SetServices, &
    routine_Run

  public &
    type_PetList
    
  public &
    label_ModifyInitializePhaseMap, &
    label_ModifyCplLists, &
    label_SetModelServices, &
    label_SetRunSequence, &
    label_Finalize, &
    label_SetRunClock
  
  character(*), parameter :: &
    label_InternalState = "Driver_InternalState"
  character(*), parameter :: &
    label_SetModelServices = "Driver_SetModelServices"
  character(*), parameter :: &
    label_SetRunSequence = "Driver_SetRunSequence"
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
    type(ESMF_State),    pointer      :: modelIS(:), modelES(:)
    type(type_PetList),  pointer      :: modelPetLists(:)
    type(ESMF_CplComp),  pointer      :: connectorComp(:,:)
    type(type_PetList),  pointer      :: connectorPetLists(:,:)
    ! - dynamic references to child components
    type(ESMF_Container)              :: componentMap
    type(ESMF_Container)              :: connectorMap
    ! - run sequence
    type(NUOPC_RunSequence), pointer  :: runSeq(:)  ! size may increase dynamic.
    integer                           :: runPhaseToRunSeqMap(10)
    ! - clock
    type(ESMF_Clock)                  :: driverClock  ! clock of the parent
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
  
  type type_PetList
    integer, pointer :: petList(:)  ! lists that are set here transfer ownership
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
    character(len=160)              :: label
    type(ESMF_GridComp)             :: component
    integer, pointer                :: petList(:)
  end type
  type ComponentMapEntry
    type(ComponentMapEntryT), pointer :: wrap
  end type
  !---------------------------------------------
  type ConnectorMapEntryT
    character(len=330)              :: label
    type(ESMF_CplComp)              :: connector
    integer, pointer                :: petList(:)
  end type
  type ConnectorMapEntry
    type(ConnectorMapEntryT), pointer :: wrap
  end type
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  recursive subroutine SetServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc
    
    ! local variables
    character(ESMF_MAXSTR):: name

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(driver, name=name, rc=rc)
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
    
    ! Claim initialize phase 1 to be able to call into Driver simply via
    ! a single ESMF_GridCompInitialize() from application level.
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
    
    ! Run phases
    call NUOPC_CompSetEntryPoint(driver, ESMF_METHOD_RUN, &
      phaseLabelList=(/"RunPhase1"/), userRoutine=routine_Run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! Run specialization
    call ESMF_MethodAdd(driver, label=label_SetRunClock, &
      userRoutine=SetRunClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
      
    ! Finalize phases
    call NUOPC_CompSetEntryPoint(driver, ESMF_METHOD_FINALIZE, &
      phaseLabelList=(/"FinalizePhase1"/), userRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
      
    ! - downward implement IPDv05:
    call NUOPC_CompSetInternalEntryPoint(driver, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv05p1"/), userRoutine=IInitAdvertize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call NUOPC_CompSetInternalEntryPoint(driver, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv05p2"/), userRoutine=IInitAdvertizeFinish, rc=rc)
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
          
  end subroutine
  
  !-----------------------------------------------------------------------------

  recursive subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    
    rc = ESMF_SUCCESS

    ! NOOP, because only single IPD version entry points set.

  end subroutine
  
  !-----------------------------------------------------------------------------

  recursive subroutine InitializeP1(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    
    ! local variables
    character(ESMF_MAXSTR)    :: name
    type(type_InternalState)  :: is

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(gcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! call the actual initialize routines
    call InitializeIPDv02p1(gcomp, importState, exportState, clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call InitializeIPDv02p3(gcomp, importState, exportState, clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call InitializeIPDv02p5(gcomp, importState, exportState, clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
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

  end subroutine
  
  !-----------------------------------------------------------------------------

  recursive subroutine InitializeIPDv02p1(gcomp, importState, exportState, &
    clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    integer                   :: localrc, stat
    type(type_InternalState)  :: is
    logical                   :: clockIsPresent
    type(ESMF_Clock)          :: internalClock
    integer                   :: i, j, k, l, cIndex
    character(ESMF_MAXSTR)    :: iString, jString, lString, compName, msgString
    character(ESMF_MAXSTR)    :: petListBuffer(100)
    integer                   :: lineCount
    integer, pointer          :: i_petList(:), j_petList(:), c_petList(:)
    logical                   :: existflag
    integer                   :: rootPet, rootVas
    type(ESMF_VM)             :: vm
    character(ESMF_MAXSTR)    :: name, verbosityString
    character(len=160)        :: namespace  ! long engough for component label
    type(ComponentMapEntry)   :: cmEntry
    type(ESMF_GridComp), pointer :: compList(:)
    type(ESMF_CplComp)        :: connector
    character(len=80)         :: srcCompLabel
    character(len=80)         :: dstCompLabel
    type(type_PetList), pointer :: petLists(:)
    integer, pointer          :: petList(:)

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(gcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! determine verbosity
    call NUOPC_CompAttributeGet(gcomp, name="Verbosity", value=verbosityString,&
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! allocate memory for the internal state and set it in the Component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal state memory failed.", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call ESMF_UserCompSetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! test whether internal Clock has already been set in the Component
    call ESMF_GridCompGet(gcomp, clockIsPresent=clockIsPresent, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    if (.not.clockIsPresent .and. ESMF_ClockIsCreated(clock)) then
      ! set the internal Clock as a copy of the incoming Clock by a default
      call NUOPC_CompSetClock(gcomp, clock, rc=rc)
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
    call ESMF_MethodExecute(gcomp, label=label_SetModelServices, &
      userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
      
    ! --> adding and moving code here that supports legacy data structures
    ! --> for now, but after the SetModelServices has been called, and
    ! --> the modelCount is now known.
    
    ! allocate lists inside the internal state according to modelCount
    allocate(is%wrap%modelPetLists(0:is%wrap%modelCount), &
      is%wrap%connectorPetLists(0:is%wrap%modelCount,0:is%wrap%modelCount), &
      is%wrap%modelComp(0:is%wrap%modelCount), &
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
      nullify(is%wrap%modelPetLists(i)%petList)
      do j=0, is%wrap%modelCount
        nullify(is%wrap%connectorPetLists(i,j)%petList)
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
    call NUOPC_DriverGetComp(gcomp, compList, petLists, rc=rc)    
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      
    do i=0, is%wrap%modelCount
      write (iString, *) i
      
      nullify(i_petList)
      if (i==0) then
      
        is%wrap%modelComp(0) = gcomp      ! driver itself is in slot 0
        is%wrap%modelIS(0) = importState  ! driver import State
        is%wrap%modelES(0) = exportState  ! driver export State
        
      else if (i>0) then
      
        is%wrap%modelPetLists(i)%petList => petLists(i)%petList
        i_petList => is%wrap%modelPetLists(i)%petList

        ! for now put a component alias into the legacy data structure until all
        ! dependencies have been removed
        is%wrap%modelComp(i) = compList(i) ! set the alias
        
        ! for now create the States here ... in the long run may be moved?
        
        is%wrap%modelIS(i) = ESMF_StateCreate(name="modelComp "// &
          trim(adjustl(iString))//" Import State", &
          stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out

        is%wrap%modelES(i) = ESMF_StateCreate(name="modelComp "// &
          trim(adjustl(iString))//" Export State", &
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
      call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_VMGet(vm, rootPet, vas=rootVas, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      if (ESMF_StateIsCreated(is%wrap%modelIS(i))) then
        call ESMF_AttributeSet(is%wrap%modelIS(i), name="rootVas", &
          value=rootVas, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
      endif
      if (ESMF_StateIsCreated(is%wrap%modelES(i))) then
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
          call NUOPC_CompAttributeGet(gcomp, name="CompLabel", &
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
          call NUOPC_CompAttributeGet(gcomp, name="CompLabel", &
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
        call NUOPC_DriverGetComp(gcomp, srcCompLabel, dstCompLabel, &
          comp=connector, petList=petList, relaxedflag=.true., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        
        ! potentially the connector must be created here
        if (.not.NUOPC_CompAreServicesSet(connector).and.(i==0.or.j==0)) then
          ! the connector was not added by the user level code SetModelServices
          ! and this involves the driver itself -> maybe automatic connector add
          if (.not.(i==0.and.j==0)) then
            ! not a driver-to-driver-self connection
            if (.not.(trim(srcCompLabel)=="_uninitialized").and. &
              .not.(trim(dstCompLabel)=="_uninitialized")) then
              ! the driver is a child of a parent driver
              ! -> automatic connector add
              call NUOPC_DriverAddComp(gcomp, &
                srcCompLabel=srcCompLabel, dstCompLabel=dstCompLabel, &
                compSetServicesRoutine=cplSS, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail
              ! retrieve the component with petList
              call NUOPC_DriverGetComp(gcomp, srcCompLabel, dstCompLabel, &
                comp=connector, petList=petList, relaxedflag=.true., rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail
              ! automatically created components inherit verbosity setting
              call NUOPC_CompAttributeSet(connector, name="Verbosity", &
                value=verbosityString, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail
            endif
          endif
        endif
        
        is%wrap%connectorComp(i,j) = connector ! set the alias
        is%wrap%connectorPetLists(i,j)%petList => petList
        
        ! initialize the connectorPhaseMap pointer members
        nullify(is%wrap%connectorPhaseMap(i,j)%phaseValue)
        nullify(is%wrap%connectorPhaseMap(i,j)%phases)
        nullify(is%wrap%connectorPhaseMap(i,j)%phaseKey)
      enddo
    enddo

    deallocate(compList, petLists)

    ! initialize the default Run Sequence...
    nullify(is%wrap%runSeq) ! initialize
    is%wrap%runPhaseToRunSeqMap = 0 ! initialize
    
    ! add one run sequence element
    call NUOPC_RunSequenceAdd(is%wrap%runSeq, 1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! map run phase 1 to first run sequence element
    is%wrap%runPhaseToRunSeqMap(1) = 1
    
    ! add run elements to the one run sequence element
    ! ... 1st block: connectors driver -> all of its model components
    !                connectors between all of the model components
    do i=0, is%wrap%modelCount
      do j=1, is%wrap%modelCount
        if (j==i) cycle ! skip self connection
        call NUOPC_RunElementAdd(is%wrap%runSeq(1), i=i, j=j, phase=1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      enddo
    enddo
    ! ... 2nd block: model components
    do i=1, is%wrap%modelCount
      call NUOPC_RunElementAdd(is%wrap%runSeq(1), i=i, j=-1, phase=1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    enddo
    ! ... 3rd block: connectors all of model components -> driver
    do i=1, is%wrap%modelCount
      j=0
      call NUOPC_RunElementAdd(is%wrap%runSeq(1), i=i, j=j, phase=1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
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
      if (ESMF_StateIsCreated(is%wrap%modelIS(i))) then
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
      if (ESMF_StateIsCreated(is%wrap%modelES(i))) then
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
      
    ! query Component for its Clock (set during specialization)
    call ESMF_GridCompGet(gcomp, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    
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
    call ESMF_MethodExecute(gcomp, label=label_SetRunSequence, &
      existflag=existflag, userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! SPECIALIZE by calling into optional attached method allowing modification
    ! of the "InitializePhaseMap" metadata.
    call ESMF_MethodExecute(gcomp, label=label_ModifyInitializePhaseMap, &
      existflag=existflag, userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! Ingest the InitializePhaseMap
    do i=0, is%wrap%modelCount
      if (NUOPC_CompAreServicesSet(is%wrap%modelComp(i))) then
        ! setup modelPhaseMap
        call setupModelPhaseMap(i=i, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
      do j=0, is%wrap%modelCount
        if (NUOPC_CompAreServicesSet(is%wrap%connectorComp(i,j))) then
          ! setup connectorPhaseMap
          call setupConnectorPhaseMap(i=i, j=j, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
        endif
      enddo
    enddo

    ! -> Now encode the NUOPC IPDv00, IPDv01, IPDv02, IPDv03, IPDv04, IPDv05
      
    ! modelComps
    call loopModelCompsS(gcomp, phaseString="IPDv00p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(gcomp, phaseString="IPDv01p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(gcomp, phaseString="IPDv02p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(gcomp, phaseString="IPDv03p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(gcomp, phaseString="IPDv04p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(gcomp, phaseString="IPDv05p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! connectorComps
    call loopConnectorCompsS(gcomp, phaseString="IPDv05p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    ! modelComps (new for IPDv05)
    call loopModelCompsS(gcomp, phaseString="IPDv05p2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! connectorComps
    call loopConnectorCompsS(gcomp, phaseString="IPDv00p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(gcomp, phaseString="IPDv01p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(gcomp, phaseString="IPDv02p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(gcomp, phaseString="IPDv03p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(gcomp, phaseString="IPDv04p1a", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(gcomp, phaseString="IPDv04p1b", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! warning - this ordering only works (with the two above) if we
    ! prevent mixing IPD versions in the same connector
    call loopConnectorCompsS(gcomp, phaseString="IPDv05p2a", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(gcomp, phaseString="IPDv05p2b", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    ! modelComps
    ! moved down one level
    !call loopModelCompsS(gcomp, phaseString="IPDv00p2", rc=rc)
    !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    !  line=__LINE__, file=trim(name)//":"//FILENAME)) &
    !  return  ! bail out
    call loopModelCompsS(gcomp, phaseString="IPDv01p2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(gcomp, phaseString="IPDv02p2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(gcomp, phaseString="IPDv03p2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(gcomp, phaseString="IPDv04p2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(gcomp, phaseString="IPDv05p3", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! connectorComps
    call loopConnectorCompsS(gcomp, phaseString="IPDv00p2a", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(gcomp, phaseString="IPDv01p2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(gcomp, phaseString="IPDv02p2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(gcomp, phaseString="IPDv03p2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(gcomp, phaseString="IPDv04p2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(gcomp, phaseString="IPDv05p3", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
      
    ! Before returning the driver must clean up its own importState, which 
    ! may have Fields advertized that do not have a ConsumerConnection set.
    ! These are Fields that during the negotiation between driver children
    ! were mirrored into the driver importState, but then subsequently were
    ! resolved among the children themselves (sibling-to-sibling). Therefore
    ! they should not remain in the parent importState. Leaving them in the
    ! parent State, while not connected with a child anylonger, would lead to
    ! issues during GeomTransfer.
    if (ESMF_StateIsCreated(importState, rc=rc)) then
      ! call into routine that removes fields without ConsumerConnection set
      call rmFieldsWoConsumerConnection(importState, name=name, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif

    contains !----------------------------------------------------------------
    
      recursive subroutine loopModelComps(phase, rc)
        ! only to be used for phase=0
        integer, intent(in)     :: phase
        integer, intent(out)    :: rc
        integer                 :: i
        character(ESMF_MAXSTR)  :: iString, pLabel
        rc = ESMF_SUCCESS
        do i=1, is%wrap%modelCount
          write (iString, *) i
          if (NUOPC_CompAreServicesSet(is%wrap%modelComp(i))) then
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
              clock=internalClock, phase=phase, userRc=localrc, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg="NUOPC Incompatible: "//&
              "Failed calling phase "// &
              trim(adjustl(pLabel))//" Initialize for modelComp "// &
              trim(adjustl(iString))//": "//trim(compName), &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
            if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase '"// &
              trim(adjustl(pLabel))//"' Initialize for modelComp "// &
              trim(adjustl(iString))//": "//trim(compName)// &
              " did not return ESMF_SUCCESS", &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
            ! need to update the Component attributes across all PETs
            if (associated(is%wrap%modelPetLists(i)%petList)) then
              call ESMF_AttributeUpdate(is%wrap%modelComp(i), vm, &
                rootList=is%wrap%modelPetLists(i)%petList, reconcile=.true., &
                rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            else
              call ESMF_AttributeUpdate(is%wrap%modelComp(i), vm, &
                rootList=(/0/), reconcile=.true., rc=rc)
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
        character(ESMF_MAXSTR)  :: iString, jString, pLabel
        type(ESMF_State)        :: imState, exState

        rc = ESMF_SUCCESS
        do i=0, is%wrap%modelCount
          write (iString, *) i
          do j=0, is%wrap%modelCount
            write (jString, *) j
            if (NUOPC_CompAreServicesSet(is%wrap%connectorComp(i,j))) then
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
                imState=importState
              else
                imState=is%wrap%modelES(i)
              endif
              if (j==0) then
                ! connect to the driver's export State
                exState=exportState
              else
                exState=is%wrap%modelIS(j)
              endif
              call ESMF_CplCompGet(is%wrap%connectorComp(i,j), name=compName, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
              call ESMF_CplCompInitialize(is%wrap%connectorComp(i,j), &
                importState=imState, exportState=exState, &
                clock=internalClock, phase=phase, userRc=localrc, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase "// &
                trim(adjustl(pLabel))//" Initialize for connectorComp "// &
                trim(adjustl(iString))//" -> "//trim(adjustl(jString))//": "// &
                trim(compName), &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
              if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase '"// &
                trim(adjustl(pLabel))//"' Initialize for connectorComp "// &
                trim(adjustl(iString))//" -> "//trim(adjustl(jString))//": "// &
                trim(compName)//" did not return ESMF_SUCCESS", &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
              ! need to update the Component attributes across all PETs
              if (associated(is%wrap%connectorPetLists(i,j)%petList)) then
                call ESMF_AttributeUpdate(is%wrap%connectorComp(i,j), vm, &
                  rootList=is%wrap%connectorPetLists(i,j)%petList, &
                  reconcile=.true., rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc))&
                  return  ! bail out
              else
                call ESMF_AttributeUpdate(is%wrap%connectorComp(i,j), vm, &
                  rootList=(/0/), reconcile=.true., rc=rc)
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

  recursive subroutine InitializeIPDv02p3(gcomp, importState, exportState, &
    clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    type(type_InternalState)  :: is
    integer                   :: i, j
    type(ESMF_VM)             :: vm
    character(ESMF_MAXSTR)    :: name

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(gcomp, name=name, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! connectorComps
    call loopConnectorCompsS(gcomp, phaseString="IPDv00p2b", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    ! modelComps
    call loopModelCompsS(gcomp, phaseString="IPDv00p2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(gcomp, phaseString="IPDv01p3", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(gcomp, phaseString="IPDv02p3", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(gcomp, phaseString="IPDv03p3", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(gcomp, phaseString="IPDv04p3", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(gcomp, phaseString="IPDv05p4", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! connectorComps
    call loopConnectorCompsS(gcomp, phaseString="IPDv03p3", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(gcomp, phaseString="IPDv04p3", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(gcomp, phaseString="IPDv05p4", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    ! modelComps
    call loopModelCompsS(gcomp, phaseString="IPDv03p4", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(gcomp, phaseString="IPDv04p4", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(gcomp, phaseString="IPDv05p5", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! connectorComps
    call loopConnectorCompsS(gcomp, phaseString="IPDv03p4", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(gcomp, phaseString="IPDv04p4", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(gcomp, phaseString="IPDv05p5", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    ! modelComps
    call loopModelCompsS(gcomp, phaseString="IPDv03p5", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(gcomp, phaseString="IPDv04p5", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(gcomp, phaseString="IPDv05p6", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! connectorComps
    call loopConnectorCompsS(gcomp, phaseString="IPDv01p3a", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(gcomp, phaseString="IPDv01p3b", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(gcomp, phaseString="IPDv02p3a", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(gcomp, phaseString="IPDv02p3b", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(gcomp, phaseString="IPDv03p5a", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(gcomp, phaseString="IPDv03p5b", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(gcomp, phaseString="IPDv04p5a", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(gcomp, phaseString="IPDv04p5b", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(gcomp, phaseString="IPDv05p6a", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(gcomp, phaseString="IPDv05p6b", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    ! modelComps
    call loopModelCompsS(gcomp, phaseString="IPDv00p3", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(gcomp, phaseString="IPDv01p4", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(gcomp, phaseString="IPDv02p4", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(gcomp, phaseString="IPDv03p6", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(gcomp, phaseString="IPDv04p6", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(gcomp, phaseString="IPDv05p7", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! connectorComps
    ! nothing to do

  end subroutine
  
  !-----------------------------------------------------------------------------

  recursive subroutine InitializeIPDv02p5Data(gcomp, importState, exportState, &
    clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    type(type_InternalState)  :: is
    integer                   :: i, j
    type(ESMF_VM)             :: vm
    character(ESMF_MAXSTR)    :: name, value
    logical                   :: execFlag, execFlagCollect
    integer                   :: execFlagIntReduced, execFlagInt

    character(ESMF_MAXSTR)    :: msgString

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(gcomp, name=name, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

#ifdef DEBUG
call ESMF_LogWrite(trim(name)//": Entering InitializeIPDv02p5Data", &
  ESMF_LOGMSG_INFO)
#endif

    ! modelComps
    if (is%wrap%firstTimeDataInit) then
      ! IPDv < 02 data initialize phase only called once
      call loopModelCompsS(gcomp, phaseString="IPDv00p4", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
      call loopModelCompsS(gcomp, phaseString="IPDv01p5", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
      is%wrap%firstTimeDataInit=.false. ! set guard flag for next time
    endif
    execFlagCollect = .false.
    call loopModelCompsS(gcomp, phaseString="IPDv02p5", execFlag=execFlag, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    execFlagCollect = execFlagCollect.or.execFlag
    call loopModelCompsS(gcomp, phaseString="IPDv03p7", execFlag=execFlag, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    execFlagCollect = execFlagCollect.or.execFlag
    call loopModelCompsS(gcomp, phaseString="IPDv04p7", execFlag=execFlag, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    execFlagCollect = execFlagCollect.or.execFlag
    call loopModelCompsS(gcomp, phaseString="IPDv05p8", execFlag=execFlag, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    execFlagCollect = execFlagCollect.or.execFlag

    ! deal with the fact that the IPDv02p5 component may not be across all PETs
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
      ! or IPDv05p8 -->> resolve data dependencies by entering loop
      call loopDataDependentInitialize(gcomp, is%wrap%dataDepAllComplete, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out

#ifdef DEBUG
write(msgString,*) trim(name)//": finished loopDataDependentInitialize(): ", &
  is%wrap%dataDepAllComplete
call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
#endif

    endif
    
    ! set the InitializeDataComplete attribute
    value="false"
    if (is%wrap%dataDepAllComplete) value="true"
    
    call NUOPC_CompAttributeSet(gcomp, &
      name="InitializeDataComplete", value=value, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

#define DEBUGPRINT____disable
#ifdef DEBUGPRINT
    ! print the entire runSeq structure
    call NUOPC_RunSequencePrint(is%wrap%runSeq, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
#endif

#ifdef DEBUG
call ESMF_LogWrite(trim(name)//": Exiting InitializeIPDv02p5Data", &
  ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

  recursive subroutine loopModelCompsS(gcomp, phaseString, execFlag, rc)
    ! only to be used for phase>0
    type(ESMF_GridComp)     :: gcomp
    character(*), intent(in):: phaseString
    logical, intent(out), optional :: execFlag ! .true. if at least one executed
    integer, intent(out)    :: rc
    ! local variables
    integer                 :: phase, i, k, localrc
    character(ESMF_MAXSTR)  :: iString, pLabel
    type(type_InternalState):: is
    character(ESMF_MAXSTR)  :: name, compName
    type(ESMF_Clock)        :: internalClock
    logical                 :: internalflag
    ! initialize out arguments
    rc = ESMF_SUCCESS
    if (present(execFlag)) execFlag = .false.
    ! query the Component for info
    call ESMF_GridCompGet(gcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    ! query Component for clock
    call ESMF_GridCompGet(gcomp, clock=internalClock, rc=rc)
    ! loop through all the model components
    do i=0, is%wrap%modelCount
      write (iString, *) i
      if (NUOPC_CompAreServicesSet(is%wrap%modelComp(i))) then
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
          clock=internalClock, phase=phase, userRc=localrc, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase '"// &
          trim(adjustl(pLabel))//"' Initialize for modelComp "// &
          trim(adjustl(iString))//": "//trim(compName), &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase '"// &
          trim(adjustl(pLabel))//"' Initialize for modelComp "// &
          trim(adjustl(iString))//": "//trim(compName)// &
          " did not return ESMF_SUCCESS", &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        if (present(execFlag)) execFlag = .true. ! at least this model executed for phaseString
      endif
    enddo
  end subroutine

  !-----------------------------------------------------------------------------

  recursive subroutine loopConnectorCompsS(gcomp, phaseString, execFlag, rc)
    ! only to be used for phase>0
    type(ESMF_GridComp)     :: gcomp
    character(*), intent(in):: phaseString
    logical, intent(out), optional :: execFlag ! .true. if at least one executed
    integer, intent(out)    :: rc
    ! local variables
    integer                 :: phase, i, ii, j, k, localrc
    character(ESMF_MAXSTR)  :: iString, jString, pLabel
    type(ESMF_State)        :: imState, exState
    type(type_InternalState):: is
    character(ESMF_MAXSTR)  :: name, compName
    type(ESMF_Clock)        :: internalClock
    ! initialize out arguments
    rc = ESMF_SUCCESS
    if (present(execFlag)) execFlag = .false.
    ! query the Component for info
    call ESMF_GridCompGet(gcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    ! query Component for clock
    call ESMF_GridCompGet(gcomp, clock=internalClock, rc=rc)
    ! loop through all the model components
    do ii=1, is%wrap%modelCount+1
      i=mod(ii,is%wrap%modelCount+1)
      write (iString, *) i
      do j=0, is%wrap%modelCount
        write (jString, *) j
        if (NUOPC_CompAreServicesSet(is%wrap%connectorComp(i,j))) then
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
            call ESMF_GridCompGet(gcomp, importState=imState, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) &
              return  ! bail out
          else
            imState=is%wrap%modelES(i)
          endif
          if (j==0) then
            ! connect to the drivers export State
            call ESMF_GridCompGet(gcomp, exportState=exState, rc=rc)
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
            clock=internalClock, phase=phase, userRc=localrc, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase '"// &
            trim(adjustl(pLabel))//"' Initialize for connectorComp "// &
            trim(adjustl(iString))//" -> "//trim(adjustl(jString))//": "// &
            trim(compName), &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase '"// &
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

  recursive subroutine loopDataDependentInitialize(gcomp, dataDepAllCompete, rc)
    ! resolve data dependencies
    type(ESMF_GridComp)             :: gcomp
    logical, optional, intent(out)  :: dataDepAllCompete
    integer, intent(out)            :: rc
    ! local variables
    integer                         :: phase, i, j, k, cphase, localrc
    character(ESMF_MAXSTR)          :: iString, jString, pString, valueString
    character(ESMF_MAXSTR)          :: cpString
    character(len=*), parameter     :: phaseString = "IPDv02p5"
    type(ESMF_State)                :: imState, exState
    logical                         :: allComplete, someProgress
    integer                         :: petCount
    integer                         :: helperIn, helperOut
    type(type_InternalState)        :: is
    character(ESMF_MAXSTR)          :: name, compName
    type(ESMF_Clock)                :: internalClock
    type(ESMF_VM)                   :: vm

    character(800)          :: msgString

    ! initialize out arguments
    rc = ESMF_SUCCESS
    
    ! query the Component for info
    call ESMF_GridCompGet(gcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    call ESMF_GridCompGet(gcomp, vm=vm, petCount=petCount, &
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
        if (NUOPC_CompAreServicesSet(is%wrap%modelComp(i))) then
          
          ! translate NUOPC logical phase to ESMF actual phase
          phase = 0 ! zero is reserved, use it here to see if need to skip
          do k=1, is%wrap%modelPhaseMap(i)%phaseCount
            if ((trim(is%wrap%modelPhaseMap(i)%phaseKey(k))==trim("IPDv02p5")).or. &
              (trim(is%wrap%modelPhaseMap(i)%phaseKey(k)) == trim("IPDv03p7")).or. &
              (trim(is%wrap%modelPhaseMap(i)%phaseKey(k)) == trim("IPDv04p7")).or. &
              (trim(is%wrap%modelPhaseMap(i)%phaseKey(k)) == trim("IPDv05p8"))) then
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
          
#ifdef DEBUG
write(msgString,*) trim(name)//": inside loopDataDependentInitialize(): ", &
  "component: ", i,", dataComplete: ", trim(valueString)
call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
#endif

          ! preconditioned input variables considering petList of component
          helperIn = 1  ! initialize
          if (ESMF_GridCompIsPetLocal(is%wrap%modelComp(i))) then
            ! evaluate "InitializeDataComplete" on PETs in petList
            if (trim(valueString)=="false") helperIn = 0
          endif

          ! implement a logical AND operation based on REDUCE_SUM
          call ESMF_VMAllFullReduce(vm, sendData=(/helperIn/), &
            recvData=helperOut, count=1, reduceflag=ESMF_REDUCE_SUM, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) &
            return  ! bail out

          if (helperOut == petCount) cycle ! skip to next i
          allComplete = .false. ! hit toggles -> prevents exit on outer loop
          
          ! else try to Run() all of the Connectors to model i
          cphase = 1  ! for now assume Run() only does phase 1
          do j=0, is%wrap%modelCount
            if (NUOPC_CompAreServicesSet(is%wrap%connectorComp(j,i))) then
              write (jString, *) j
              write (cpString, *) cphase
              if (j==0) then
                ! connect to the drivers import State
                call ESMF_GridCompGet(gcomp, importState=imState, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//FILENAME)) &
                  return  ! bail out
              else
                imState=is%wrap%modelES(j)
              endif
              if (i==0) then
                ! connect to the drivers export State
                call ESMF_GridCompGet(gcomp, exportState=exState, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//FILENAME)) &
                  return  ! bail out
              else
                exState=is%wrap%modelIS(i)
              endif
              call ESMF_CplCompRun(is%wrap%connectorComp(j,i), &
                importState=imState, exportState=exState, &
                clock=internalClock, phase=cphase, userRc=localrc, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, &
                msg="Failed calling phase "//trim(adjustl(cpString))// &
                " Run for connectorComp "//trim(adjustl(jString))// &
                " -> "//trim(adjustl(iString)), &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
              if (ESMF_LogFoundError(rcToCheck=localrc,  msg="Phase '"// &
                trim(adjustl(cpString))//"' Run for connectorComp "// &
                trim(adjustl(jString))//" -> "//trim(adjustl(iString))// &
                " did not return ESMF_SUCCESS", &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
          enddo
          
          ! attempt to make the actual call to initialize for model i
          call ESMF_GridCompGet(is%wrap%modelComp(i), name=compName, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          call ESMF_GridCompInitialize(is%wrap%modelComp(i), &
            importState=is%wrap%modelIS(i), exportState=is%wrap%modelES(i), &
            clock=internalClock, phase=phase, userRc=localrc, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase "// &
            trim(adjustl(pString))//" Initialize for modelComp "// &
            trim(adjustl(iString))//": "//trim(compName), &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase '"// &
            trim(adjustl(pString))//"' Initialize for modelComp "// &
            trim(adjustl(iString))//": "//trim(compName)// &
            " did not return ESMF_SUCCESS", &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
            
          ! check model InitializeDataProgress Attribute if progress made
          call NUOPC_CompAttributeGet(is%wrap%modelComp(i), &
            name="InitializeDataProgress", value=valueString, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) &
            return  ! bail out
          
          ! make sure there is a consistent view across all PETs
          helperIn = 0
          if (trim(valueString)=="true") helperIn = 1
          call ESMF_VMAllFullReduce(vm, sendData=(/helperIn/), &
            recvData=helperOut, count=1, reduceflag=ESMF_REDUCE_SUM, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) &
            return  ! bail out
            
          if (helperOut > 0) someProgress=.true. ! toggle flag
            
        endif
      enddo
      
      if (present(dataDepAllCompete)) dataDepAllCompete=allComplete

      ! check if all Components with IPDv02p5 are InitializeDataComplete
      if (allComplete) exit ! break out of data-dependency resolution loop
      
      if (.not.someProgress) then
        if (present(dataDepAllCompete)) exit ! break out of loop
        ! else, dead-lock situation identified
        call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
          msg="Initialize data-dependency resolution loop "// &
          "has entered a dead-lock situation.", &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
        return  ! bail out of data-dependency resolution loop, prevent lock
      endif
      
    enddo
    
  end subroutine

  !-----------------------------------------------------------------------------

  recursive subroutine routine_Run(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    integer                         :: localrc
    type(type_InternalState)        :: is
    type(ESMF_Clock)                :: internalClock
    logical                         :: existflag
    integer                         :: i, j, phase, runPhase, runSeqIndex
    character(ESMF_MAXSTR)          :: iString, jString, pLabel
    character(ESMF_MAXSTR)          :: msgString, valueString
    type(NUOPC_RunElement), pointer :: runElement
    type(ESMF_State)                :: imState, exState
    character(ESMF_MAXSTR)          :: name, compName, profileString
    integer                         :: verbosity
    integer                         :: profiling
    integer                         :: runElementCounter, runLoopCounter
    integer                         :: runElementCounterMax
    real(ESMF_KIND_R8)              :: timeBase, timeStart, timeStop
    real(ESMF_KIND_R8)              :: timeSum(1000)  ! room for 1000 elements
    logical                         :: loopFlag

    rc = ESMF_SUCCESS

#define NUOPC_DRIVER_TRACE__OFF
#ifdef NUOPC_DRIVER_TRACE
    call ESMF_TraceRegionEnter("NUOPC_Driver:Run")
#endif
    
    ! get the name and currentPhase
    call ESMF_GridCompGet(gcomp, name=name, currentPhase=runPhase, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    
    ! determine profiling
    call NUOPC_CompAttributeGet(gcomp, name="Profiling", value=valueString, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    profiling = ESMF_UtilString2Int(valueString, &
      specialStringList=(/"high", "max "/), specialValueList=(/255, 255/), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! determine verbosity
    call NUOPC_CompAttributeGet(gcomp, name="Verbosity", value=valueString, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    verbosity = ESMF_UtilString2Int(valueString, &
      specialStringList=(/"high", "max "/), specialValueList=(/255, 255/), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! query Component for its Clock
    call ESMF_GridCompGet(gcomp, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! conditionally output diagnostic to Log file
    if (btest(verbosity,0)) then
      call NUOPC_CompSearchRevPhaseMap(gcomp, ESMF_METHOD_RUN, &
        phaseIndex=phase, phaseLabel=pLabel, rc=rc)
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
    if (btest(profiling,0)) then
      call ESMF_VMWtime(timeBase, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_VMWtime(timeStart, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      runElementCounter=0
      runLoopCounter=1
      write (msgString, "( A, ES16.8, A, I6, '/', I6 )" ) &
        "RunProfile time=   ", timeStart-timeBase, &
        "   runLoop/runElement=", runLoopCounter, runElementCounter
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      timeSum(:) = 0._ESMF_KIND_R8  ! zero out the time accumulator
    endif  
    
    ! set the driverClock member in the internal state
    is%wrap%driverClock = clock
    
    ! SPECIALIZE required: label_SetRunClock
    ! -> first check for the label with phase index
    call ESMF_MethodExecute(gcomp, label=label_SetRunClock, index=phase, &
      existflag=existflag, userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (.not.existflag) then
      ! -> next check for the label without phase index
      call ESMF_MethodExecute(gcomp, label=label_SetRunClock, &
        userRc=localrc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    
    ! determine the correct run sequence index for the current runPhase    
    runSeqIndex = is%wrap%runPhaseToRunSeqMap(runPhase)
    
    ! alias the component's internalClock in this runPhase run sequence
    call NUOPC_RunSequenceSet(is%wrap%runSeq(runSeqIndex), internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! use RunSequence iterator to execute the actual time stepping loop
    nullify(runElement) ! prepare runElement for iterator use
    do while (NUOPC_RunSequenceIterate(is%wrap%runSeq, runSeqIndex, &
      runElement, loopFlag=loopFlag, rc=rc))
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      
      ! now interpret and act on the current runElement
#ifdef DEBUGPRINT
      print *, runElement%i, runElement%j, runElement%phase
      call NUOPC_ClockPrintCurrTime(runElement%runSeq%clock, &
        "NUOPC_Driver.Run() RunSequence iterator loop at current time: ", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
#endif
      
      if (btest(profiling,0)) then
        if (loopFlag) then
          ! increment the loop counter
          runLoopCounter=runLoopCounter+1
          runElementCounter = 0
        endif
      endif
      
      i = runElement%i
      phase = runElement%phase
      internalClock = runElement%runSeq%clock
      if (runElement%j >= 0) then
        ! connector component
        j = runElement%j
        if (NUOPC_CompAreServicesSet(is%wrap%connectorComp(i,j))) then
          write (iString, *) i
          write (jString, *) j
          call NUOPC_CompSearchRevPhaseMap(is%wrap%connectorComp(i,j), &
            ESMF_METHOD_RUN, phaseIndex=phase, phaseLabel=pLabel, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (i==0) then
            ! connect to the driver's import State
            imState=importState
          else
            imState=is%wrap%modelES(i)
          endif
          if (j==0) then
            ! connect to the driver's export State
            exState=exportState
          else
            exState=is%wrap%modelIS(j)
          endif
          
          if (btest(profiling,0)) then
            call ESMF_CplCompGet(is%wrap%connectorComp(i,j), name=compName, &
              rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
            write (profileString, *) "PROF:", trim(compName), ".Run(phase=", &
              trim(adjustl(pLabel)), ")"

            call ESMF_VMWtime(timeStart, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
            runElementCounter=runElementCounter+1
            write (msgString, "( A40,A,ES16.8,A,I6,'/',I6 )" ) &
              trim(profileString), &
              " time=", timeStart-timeBase, &
              "   runLoop/runElement=", runLoopCounter, runElementCounter
            call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
          endif

#ifdef NUOPC_DRIVER_TRACE
          call ESMF_TraceRegionEnter("NUOPC_Driver:CplComp:Callthrough")
#endif          
          call ESMF_CplCompRun(is%wrap%connectorComp(i,j), &
            importState=imState, exportState=exState, &
            clock=internalClock, phase=phase, userRc=localrc, rc=rc)
#ifdef NUOPC_DRIVER_TRACE
          call ESMF_TraceRegionExit("NUOPC_Driver:CplComp:Callthrough")
#endif
          
          if (ESMF_LogFoundError(rcToCheck=rc, &
            msg="Failed calling phase "//trim(adjustl(pLabel))// &
            " Run for connectorComp "//trim(adjustl(iString))// &
            " -> "//trim(adjustl(jString)), &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          if (ESMF_LogFoundError(rcToCheck=localrc,  msg="Phase '"// &
            trim(adjustl(pLabel))//"' Run for connectorComp "// &
            trim(adjustl(iString))//" -> "//trim(adjustl(jString))// &
            " did not return ESMF_SUCCESS", &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
            
          if (btest(profiling,0)) then
            call ESMF_VMWtime(timeStop, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
            write (msgString, "( A40,A,ES16.8,A,I6,'/',I6,A,ES16.8)" ) &
              trim(profileString), &
              " time=", timeStop-timeBase, &
              "   runLoop/runElement=", runLoopCounter, runElementCounter, &
              "   diffTime=", timeStop-timeStart
            call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
            ! time accumulation
            timeSum(runElementCounter)=timeSum(runElementCounter)+&
              timeStop-timeStart
          endif
            
        endif
      else
        ! model or mediator component
        if (NUOPC_CompAreServicesSet(is%wrap%modelComp(i))) then
          write (iString, *) i
          call NUOPC_CompSearchRevPhaseMap(is%wrap%modelComp(i), &
            ESMF_METHOD_RUN, phaseIndex=phase, phaseLabel=pLabel, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          
          if (btest(profiling,0) .and. &
            ESMF_GridCompIsPetLocal(is%wrap%modelComp(i))) then
            call ESMF_GridCompGet(is%wrap%modelComp(i), name=compName, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
            write (profileString, *) "PROF:", trim(compName), ".Run(phase=", &
              trim(adjustl(pLabel)), ")"

            call ESMF_VMWtime(timeStart, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
            runElementCounter=runElementCounter+1
            write (msgString, "( A40,A,ES16.8,A,I6,'/',I6 )" ) &
              trim(profileString), &
              " time=", timeStart-timeBase, &
              "   runLoop/runElement=", runLoopCounter, runElementCounter
            call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
          endif

#ifdef NUOPC_DRIVER_TRACE
          call ESMF_TraceRegionEnter("NUOPC_Driver:GridComp:Callthrough")
#endif    
          call ESMF_GridCompRun(is%wrap%modelComp(i), &
            importState=is%wrap%modelIS(i), exportState=is%wrap%modelES(i), &
            clock=internalClock, phase=phase, userRc=localrc, rc=rc)
#ifdef NUOPC_DRIVER_TRACE
          call ESMF_TraceRegionExit("NUOPC_Driver:GridComp:Callthrough")
#endif
          if (ESMF_LogFoundError(rcToCheck=rc, &
            msg="Failed calling phase "//trim(adjustl(pLabel))// &
            " Run for modelComp "//trim(adjustl(iString)), &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase '"// &
            trim(adjustl(pLabel))//"' Run for modelComp "// &
            trim(adjustl(iString))//" did not return ESMF_SUCCESS", &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          
          if (btest(profiling,0) .and. &
            ESMF_GridCompIsPetLocal(is%wrap%modelComp(i))) then
            call ESMF_VMWtime(timeStop, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
            write (msgString, "( A40,A,ES16.8,A,I6,'/',I6,A,ES16.8)" ) &
              trim(profileString), &
              " time=", timeStop-timeBase, &
              "   runLoop/runElement=", runLoopCounter, runElementCounter, &
              "   diffTime=", timeStop-timeStart
            call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
            ! time accumulation
            timeSum(runElementCounter)=timeSum(runElementCounter)+&
              timeStop-timeStart
          endif
            
        endif
      endif

    enddo
    ! check RC of the NUOPC_RunSequenceIterate() call
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! conditionally output diagnostic to Log file
    if (btest(verbosity,0)) then
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
    if (btest(profiling,0)) then
      ! store element counter high water mark and reset
      runElementCounterMax = runElementCounter
      ! profile output
      write (msgString, *) "RunSequence Profile:"
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      write (msgString, *) "  total number of loops executed:   ", &
        runLoopCounter
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      write (msgString, *) "  total number of elements per loop:", &
        runElementCounterMax
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      do runElementCounter=1, runElementCounterMax
        write (msgString, "( A,I6,A,ES16.8 )") &
          "  timeSum(", runElementCounter,")=", timeSum(runElementCounter)
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      enddo
    endif

#ifdef NUOPC_DRIVER_TRACE
    call ESMF_TraceRegionExit("NUOPC_Driver:Run")
#endif
    
  end subroutine
  
  !-----------------------------------------------------------------------------
  
  recursive subroutine SetRunClock(gcomp, rc)
    type(ESMF_GridComp)   :: gcomp
    integer, intent(out)  :: rc
    
    ! Set the internal clock according to the incoming driver clock.
    ! Implement the default explicit timekeeping rules.
    
    ! local variables
    type(type_InternalState)  :: is
    character(ESMF_MAXSTR):: name

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(gcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! query component for its internal state
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    if (ESMF_ClockIsCreated(is%wrap%driverClock)) then
      ! check and set the model clock against the driver clock
      call NUOPC_CompCheckSetClock(gcomp, is%wrap%driverClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif
          
  end subroutine
    
  !-----------------------------------------------------------------------------

  recursive subroutine Finalize(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    integer                   :: localrc, stat
    type(type_InternalState)  :: is
    type(ESMF_Clock)          :: internalClock
    integer                   :: i, j, itemCount
    type(ComponentMapEntry)   :: cmEntry
    type(ConnectorMapEntry)   :: cnEntry
    character(ESMF_MAXSTR)    :: iString, jString
    logical                   :: existflag
    character(ESMF_MAXSTR)    :: name
    type(ESMF_GridComp), pointer  :: compList(:)
    type(ESMF_CplComp), pointer   :: connectorList(:)
    
    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(gcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! query Component for its Clock
    call ESMF_GridCompGet(gcomp, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
      
    ! Finalize: connectorComps
    do i=0, is%wrap%modelCount
      write (iString, *) i
      do j=0, is%wrap%modelCount
        write (jString, *) j
        if (NUOPC_CompAreServicesSet(is%wrap%connectorComp(i,j))) then
          call ESMF_CplCompFinalize(is%wrap%connectorComp(i,j), &
            importState=is%wrap%modelES(i), exportState=is%wrap%modelIS(j), &
            clock=internalClock, phase=1, userRc=localrc, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase 1 "// &
            "Finalize for connectorComp "// &
            trim(adjustl(iString))//" -> "//trim(adjustl(jString)), &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase 1 "// &
            "Finalize for connectorComp "// &
            trim(adjustl(iString))//" -> "//trim(adjustl(jString))//" did not "// &
            "return ESMF_SUCCESS", &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
        endif
      enddo
    enddo

    ! Finalize: modelComps
    do i=1, is%wrap%modelCount
      write (iString, *) i
      if (NUOPC_CompAreServicesSet(is%wrap%modelComp(i))) then
        call ESMF_GridCompFinalize(is%wrap%modelComp(i), &
          importState=is%wrap%modelIS(i), exportState=is%wrap%modelES(i), &
          clock=internalClock, phase=1, userRc=localrc, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase 1 "// &
          "Finalize for modelComp "//trim(adjustl(iString)), &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase 1 "// &
          "Finalize for modelComp "//trim(adjustl(iString))//" did not "// &
          "return ESMF_SUCCESS", &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
    enddo
    
    ! SPECIALIZE by calling into optional attached method
    call ESMF_MethodExecute(gcomp, label=label_Finalize, existflag=existflag, &
      userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! destroy components in the componentMap and their import and export States + connectorComps
    ! and also petLists that were set by the user (and ownership transferred)
    nullify(compList)
    call NUOPC_DriverGetComp(gcomp, compList, rc=rc)    
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    do i=1, size(compList)
      call ESMF_GridCompDestroy(compList(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_StateDestroy(is%wrap%modelIS(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_StateDestroy(is%wrap%modelES(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      if (associated(is%wrap%modelPetLists(i)%petList)) then
        deallocate(is%wrap%modelPetLists(i)%petList, stat=stat)
        if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
          msg="Deallocation of transferred model petList failed.", &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
    enddo
    deallocate(compList)
    
    ! destroy components in the componentMap and their import and export States + connectorComps
    nullify(connectorList)
    call NUOPC_DriverGetComp(gcomp, connectorList, rc)    
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    do i=1, size(connectorList)
      call ESMF_CplCompDestroy(connectorList(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    enddo
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
      is%wrap%modelComp, is%wrap%modelIS, is%wrap%modelES, &
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
    compSetServicesRoutine, petList, comp, rc)
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
    integer,             intent(in),  optional :: petList(:)
    type(ESMF_GridComp), intent(out), optional :: comp
    integer,             intent(out), optional :: rc 
!
! !DESCRIPTION:
! Create and add a GridComp (i.e. Model, Mediator, or Driver) as a child 
! component to a Driver. The component is created on the provided {\tt petList},
! or by default across all of the Driver PETs.
!
! The specified {\tt SetServices()} routine is called back immediately after the
! new child component has been created internally. Very little around the
! component is set up at that time (e.g. component attributes are not 
! available). The routine should therefore be very light weight, with the sole
! purpose of setting the entry points of the component -- typically by deriving 
! from a generic component followed by the appropriate specilizations.
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
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is
    type(ComponentMapEntry)         :: cmEntry
    integer                         :: stat, i, k, lineCount
    character(ESMF_MAXSTR)          :: petListBuffer(100)
    character(ESMF_MAXSTR)          :: msgString, lString

    if (present(rc)) rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
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
      petList=petList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    if (present(petList)) then
      allocate(cmEntry%wrap%petList(size(petList)))
      cmEntry%wrap%petList = petList  ! copy the petList elements
    else
      nullify(cmEntry%wrap%petList) ! invalidate the petList
    endif
  
    if (associated(cmEntry%wrap%petList)) then
      write (lString, *) size(cmEntry%wrap%petList)
      write (msgString,"(A)") trim(name)//&
        " - Creating model component "//trim(cmEntry%wrap%label)//&
        " with petList of size "//trim(adjustl(lString))//" :"
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      
      if (size(cmEntry%wrap%petList) <= 1000) then
        ! have the resources to print the entire petList
        write (petListBuffer, "(10I7)") cmEntry%wrap%petList
        lineCount = size(cmEntry%wrap%petList)/10
        if ((size(cmEntry%wrap%petList)/10)*10 /= size(cmEntry%wrap%petList)) &
          lineCount = lineCount + 1
        do k=1, lineCount
          call ESMF_LogWrite(petListBuffer(k), ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
        enddo
      endif
    else
      write (msgString,"(A)") trim(name)//" - Creating model component "//&
        trim(cmEntry%wrap%label)//" without petList."
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    call ESMF_ContainerAddUDT(is%wrap%componentMap, trim(compLabel), &
      cmEntry, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! add standard NUOPC GridComp Attribute Package to the modelComp
    call NUOPC_CompAttributeInit(cmEntry%wrap%component, kind="Model", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! Call the SetServices on the added component
    call ESMF_GridCompSetServices(cmEntry%wrap%component, &
      compSetServicesRoutine, userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
      
    ! Set the CompLabel attribute
    call NUOPC_CompAttributeSet(cmEntry%wrap%component, &
      name="CompLabel", value=trim(cmEntry%wrap%label), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
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
    sharedObj, petList, comp, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                        :: driver
    character(len=*),    intent(in)            :: compLabel
    character(len=*),    intent(in),  optional :: sharedObj
    integer,             intent(in),  optional :: petList(:)
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
! component is set up at that time (e.g. component attributes are not 
! available). The routine should therefore be very light weight, with the sole
! purpose of setting the entry points of the component -- typically by deriving 
! from a generic component followed by the appropriate specilizations.
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
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is
    type(ComponentMapEntry)         :: cmEntry
    integer                         :: stat, i, k, lineCount
    character(ESMF_MAXSTR)          :: petListBuffer(100)
    character(ESMF_MAXSTR)          :: msgString, lString

    if (present(rc)) rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
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
      petList=petList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    if (present(petList)) then
      allocate(cmEntry%wrap%petList(size(petList)))
      cmEntry%wrap%petList = petList  ! copy the petList elements
    else
      nullify(cmEntry%wrap%petList) ! invalidate the petList
    endif
  
    if (associated(cmEntry%wrap%petList)) then
      write (lString, *) size(cmEntry%wrap%petList)
      write (msgString,"(A)") trim(name)//&
        " - Creating model component "//trim(cmEntry%wrap%label)//&
        " with petList of size "//trim(adjustl(lString))//" :"
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      
      if (size(cmEntry%wrap%petList) <= 1000) then
        ! have the resources to print the entire petList
        write (petListBuffer, "(10I7)") cmEntry%wrap%petList
        lineCount = size(cmEntry%wrap%petList)/10
        if ((size(cmEntry%wrap%petList)/10)*10 /= size(cmEntry%wrap%petList)) &
          lineCount = lineCount + 1
        do k=1, lineCount
          call ESMF_LogWrite(petListBuffer(k), ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
        enddo
      endif
    else
      write (msgString,"(A)") trim(name)//" - Creating model component "//&
        trim(cmEntry%wrap%label)//" without petList."
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

    call ESMF_ContainerAddUDT(is%wrap%componentMap, trim(compLabel), &
      cmEntry, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! add standard NUOPC GridComp Attribute Package to the modelComp
    call NUOPC_CompAttributeInit(cmEntry%wrap%component, kind="Model", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! Call the SetServices on the added component
    call NUOPC_CompSetServices(cmEntry%wrap%component, &
      sharedObj=sharedObj, userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
      
    ! Set the CompLabel attribute
    call NUOPC_CompAttributeSet(cmEntry%wrap%component, &
      name="CompLabel", value=trim(cmEntry%wrap%label), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
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
    dstCompLabel, compSetServicesRoutine, petList, comp, rc)
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
    integer, target,     intent(in),  optional :: petList(:)
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
! component is set up at that time (e.g. component attributes are not 
! available). The routine should therefore be very light weight, with the sole
! purpose of setting the entry points of the component -- typically by deriving 
! from a generic component followed by the appropriate specilizations.
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
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is
    type(ConnectorMapEntry)         :: cmEntry
    integer                         :: stat, src, dst
    type(ESMF_GridComp)             :: srcComp, dstComp
    integer, pointer                :: connectorPetList(:)
    integer, pointer                :: connectorPetListTemp(:)
    integer, pointer                :: connectorPetListTemp2(:)
    integer, pointer                :: srcPetList(:), dstPetList(:)
    integer                         :: k, l, cIndex, lineCount
    character(ESMF_MAXSTR)          :: petListBuffer(100)
    character(ESMF_MAXSTR)          :: msgString, lString
    type(ESMF_VM)                   :: vm

    if (present(rc)) rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
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
      call NUOPC_DriverGetComp(driver, srcCompLabel, petList=srcPetList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call NUOPC_DriverGetComp(driver, dstCompLabel, petList=dstPetList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
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
      write (lString, *) size(connectorPetList)
      write (msgString,"(A)") trim(name)//&
        " - Creating connector component "//trim(cmEntry%wrap%label)//&
        " with petList of size "//trim(adjustl(lString))//" :"
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      if (size(connectorPetList) <= 1000) then
        ! have the resources to print the entire petList
        write (petListBuffer, "(10I7)") connectorPetList
        lineCount = size(connectorPetList)/10
        if ((size(connectorPetList)/10)*10 /= size(connectorPetList)) &
          lineCount = lineCount + 1
        do k=1, lineCount
          call ESMF_LogWrite(petListBuffer(k), ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
        enddo
      endif
      cmEntry%wrap%connector = ESMF_CplCompCreate(&
        name=trim(cmEntry%wrap%label), petList=connectorPetList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    else
      ! create the connector without petList
      write (msgString,"(A)") trim(name)//" - Creating connector component "//&
        trim(cmEntry%wrap%label)//" without petList."
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      cmEntry%wrap%connector = ESMF_CplCompCreate(&
        name=trim(cmEntry%wrap%label), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
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
      cmEntry, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
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
      call NUOPC_DriverGetComp(driver, srcCompLabel, srcComp, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call NUOPC_DriverGetComp(driver, dstCompLabel, dstComp, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
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
      is%wrap%connectorPetLists(src,dst)%petList => cmEntry%wrap%petList
    endif

    ! add standard NUOPC CplComp Attribute Package to the connectorComp
    call NUOPC_CompAttributeInit(cmEntry%wrap%connector, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! Call the SetServices on the added connector
    call ESMF_CplCompSetServices(cmEntry%wrap%connector, &
      compSetServicesRoutine, userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! If legacy data structures are already set up, then here must also
    ! ensure phase 0 is called for this connector, and attributes are set
    if (is%wrap%legacyReady) then
      call ESMF_CplCompInitialize(is%wrap%connectorComp(src,dst), &
        importState=is%wrap%modelES(src), exportState=is%wrap%modelIS(dst), &
        phase=0, userRc=localrc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      ! need to update the Component attributes across all PETs
      call ESMF_VMGetCurrent(vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      if (associated(is%wrap%connectorPetLists(src,dst)%petList)) then
        call ESMF_AttributeUpdate(is%wrap%connectorComp(src,dst), vm, &
          rootList=is%wrap%connectorPetLists(src,dst)%petList, &
          reconcile=.true., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc))&
          return  ! bail out
      else
        call ESMF_AttributeUpdate(is%wrap%connectorComp(src,dst), vm, &
          rootList=(/0/), reconcile=.true., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc))&
          return  ! bail out
      endif
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
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is
    integer                         :: iComp, i
    type(ESMF_GridComp)             :: comp
    integer                         :: phase
    logical                         :: relaxed
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    relaxed = .false.
    if (present(relaxedflag)) relaxed=relaxedflag
    
    ! query the Component for info
    call ESMF_GridCompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
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
      relaxedflag=relaxedflag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
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
    
    ! Figure out the phase index
    call NUOPC_CompSearchPhaseMap(comp, methodflag=ESMF_METHOD_RUN, &
      phaseLabel=phaseLabel, phaseIndex=phase, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
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
            msg="run phase: '"//trim(phaseLabel)//"' could not be identified.", &
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
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
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
    
    ! query the Component for info
    call ESMF_GridCompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! Figuring out the index into the modelComp array.
    !TODO: This is a pretty involved look-up, and future implementation will
    !TODO: fully eliminate the static arrays modelComp and connectorComp, 
    !TODO: removing the need to do this look-up here.
    call NUOPC_DriverGetComp(driver, srcCompLabel, srcComp, &
      relaxedflag=relaxedflag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call NUOPC_DriverGetComp(driver, dstCompLabel, dstComp, &
      relaxedflag=relaxedflag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
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
      relaxedflag=relaxedflag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! Figure out the phase index
    call NUOPC_CompSearchPhaseMap(comp, methodflag=ESMF_METHOD_RUN, &
      phaseLabel=phaseLabel, phaseIndex=phase, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
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
            msg="run phase: '"//trim(phaseLabel)//"' could not be identified.", &
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
      phase=phase, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
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
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is

    if (present(rc)) rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! Actually add the RunElement for the identified component
    call NUOPC_RunElementAddLink(is%wrap%runSeq(slot), slot=linkSlot, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
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
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is

    if (present(rc)) rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    !TODO: actually create a FreeFormat object with contents. For now just empty
    freeFormat = NUOPC_FreeFormatCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
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
    relaxedflag, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                        :: driver
    character(len=*),    intent(in)            :: compLabel
    type(ESMF_GridComp), intent(out), optional :: comp
    integer,             pointer,     optional :: petList(:)
    logical,             intent(in),  optional :: relaxedflag
    integer,             intent(out), optional :: rc 
!
! !DESCRIPTION:
! Query the Driver for a GridComp (i.e. Model, Mediator, or Driver) child 
! component that was added under {\tt compLabel}.
!
! If provided, the {\tt petList} argument will be associated with the petList
! that was used to create the referenced component.
!
! By default an error is returned if no component is associated with the 
! specified {\tt compLabel}. This error can be suppressed by setting
! {\tt relaxedflag=.true.}, and unassociated arguments will be returned.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is
    type(ComponentMapEntry)         :: cmEntry
    logical                         :: relaxed, getFlag, foundFlag
    character(ESMF_MAXSTR)          :: driverCompLabel

    if (present(rc)) rc = ESMF_SUCCESS

    relaxed = .false.
    if (present(relaxedflag)) relaxed=relaxedflag

    ! query the Component for info
    call ESMF_GridCompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! determine whether compLabel exists in the drivers map    
    call ESMF_ContainerGet(is%wrap%componentMap, trim(compLabel), &
      isPresent=foundFlag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! alternative exit condition if driver itself matches compLabel
    if (.not.foundFlag) then
      call NUOPC_CompAttributeGet(driver, name="CompLabel", &
        value=driverCompLabel, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
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
        cmEntry, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      if (present(comp)) comp = cmEntry%wrap%component
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
! that was used to create the referenced component.
!
! By default an error is returned if no component is associated with the 
! specified {\tt compLabel}. This error can be suppressed by setting
! {\tt relaxedflag=.true.}, and unassociated arguments will be returned.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is
    type(ConnectorMapEntry)         :: cmEntry
    logical                         :: relaxed, getFlag

    if (present(rc)) rc = ESMF_SUCCESS

    relaxed = .false.
    if (present(relaxedflag)) relaxed=relaxedflag

    ! query the Component for info
    call ESMF_GridCompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! consider relaxed mode
    getFlag = .true.
    if (relaxed) then
      call ESMF_ContainerGet(is%wrap%connectorMap, &
        trim(srcCompLabel)//"-TO-"//trim(dstCompLabel), &
        isPresent=getFlag, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    
    ! Conditionally access the entry in componentMap
    if (getFlag) then
      call ESMF_ContainerGetUDT(is%wrap%connectorMap, &
        trim(srcCompLabel)//"-TO-"//trim(dstCompLabel), cmEntry, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
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
    type(type_petList),  pointer, optional     :: petLists(:)
    integer,             intent(out), optional :: rc 
!
! !DESCRIPTION:
! Get all the GridComp (i.e. Model, Mediator, or Driver) child components from a
! Driver. The incoming {\tt compList} and {\tt petLists} arguments must be 
! unassociated. On return it becomes the responsibility of the caller to 
! deallocate the associated {\tt compList} and {\tt petLists} arguments
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
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

    ! query the Component for info
    call ESMF_GridCompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! get basic information about map
    call ESMF_ContainerGet(is%wrap%componentMap, itemCount=mapCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
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
          cmEntry, ESMF_ITEMORDER_ADDORDER, rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        if (present(compList)) compList(i) = cmEntry%wrap%component
        if (present(petLists)) petLists(i)%petList => cmEntry%wrap%petList
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
  recursive subroutine NUOPC_DriverGetAllCplComp(driver, compList, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                        :: driver
    type(ESMF_CplComp),  pointer               :: compList(:)
    integer,             intent(out), optional :: rc 
!
! !DESCRIPTION:
! Get all the CplComp (i.e. Connector) child components from a
! Driver. The incoming {\tt compList} and {\tt petLists} arguments must be 
! unassociated. On return it becomes the responsibility of the caller to 
! deallocate the associated {\tt compList} and {\tt petLists} arguments
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
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

    ! query the Component for info
    call ESMF_GridCompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! get basic information about map
    call ESMF_ContainerGet(is%wrap%connectorMap, itemCount=mapCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
      
    ! allocate memory for the compList
    allocate(compList(mapCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of compList failed.", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! fill the compList
    do i=1, mapCount
      call ESMF_ContainerGetUDTByIndex(is%wrap%connectorMap, i, &
        cmEntry, ESMF_ITEMORDER_ADDORDER, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      compList(i) = cmEntry%wrap%connector
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
    type(NUOPC_FreeFormat), intent(in)            :: freeFormat
    logical,                intent(in),  optional :: autoAddConnectors
    integer,                intent(out), optional :: rc 
!
! !DESCRIPTION:
! Ingest the run sequence from a FreeFormat object. Every line in 
! {\tt freeFormat} corresponds to either a single run sequence element, or 
! contains the time step information of a loop. The default of 
! {\tt autoAddConnectors} is {\tt .false.}, which means that all components
! referenced in the {\tt freeFormat} run sequence, including connectors, must
! already be available as child components of the {\tt driver} component, or
! else an error will be returned. When {\tt autoAddConnectors}
! is set to {\tt .true.}, connector components encountered in the run sequence
! that are no already present in the {\tt driver} will be added automatically.
! The default {\tt NUOPC\_Connector} implementation will be used for all
! automatically added connector instances.
!
! Time loops in the run sequence start with a line in {\tt freeFormat} that
! begins with the $@$ symbol, followed by the timestep in seconds. A line with
! a single $@$ symbol closes the time loop. A simple example for a loop with 
! 1 hour timestep:
!
! \begin{verbatim}
!   @3600
!     ...
!     ...
!   @
! \end{verbatim}
! Time loops can be nested.
!
! The lines between the time loop markers define the sequence in which the
! run methods of the components are called. Note that components will execute 
! concurrently as long as this is not prevented by data-dependencies or
! overlapping petLists.
!
! Each line specifies the precise
! run method phase for a single component instance. For model, mediator, and 
! driver components the format is this:
!
! \begin{verbatim}
!   compLabel [phaseLabel]
! \end{verbatim}
! Here {\tt compLabel} is the label by which the component instance is known to
! the driver. It is optionally followed a {\tt phaseLabel} identifying a
! specific run phase. An example of calling the run phase of the ATM instance 
! that contains the fast processes:
!
! \begin{verbatim}
!   ATM fast
! \end{verbatim}
! By default, i.e. without {\tt phaseLabel}, the first
! registered run method of the component is used.
!
! The format for connector components is different. It is defined like this:
!
! \begin{verbatim}
!   srcCompLabel -> dstCompLabel [connectionOptions]
! \end{verbatim}
! A connector instance is uniquely known by the two components it connects, 
! i.e. by {\tt srcCompLabel} and {\tt dstCompLabel}. The syntax requires that
! the token {\tt ->} be specified between source and destination. Optionally
! {\tt connectionOptions} can be supplied using the same format discussed 
! under section \ref{connection_options}. An example of executing the connector
! instance that transfers fields from the ATM component to the OCN component,
! using redistribution for remapping:
! 
! \begin{verbatim}
!   ATM -> OCN :remapMethod=redist
! \end{verbatim}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)                          :: name
    type(type_InternalState)                        :: is
    integer                                         :: i, lineCount, tokenCount
    character(len=NUOPC_FreeFormatLen), allocatable :: tokenList(:)
    integer, allocatable                            :: slotStack(:)
    character(len=NUOPC_FreeFormatLen)              :: tempString
    type(ESMF_TimeInterval)                         :: timeStep
    type(ESMF_Clock)                                :: internalClock, subClock
    integer                                         :: level, slot, slotHWM
    integer                                         :: slotCount
    real(ESMF_KIND_R8)                              :: seconds
    logical                                         :: optAutoAddConnectors
    type(ESMF_CplComp)                              :: conn

    if (present(rc)) rc = ESMF_SUCCESS
    
    optAutoAddConnectors = .false. ! default
    if (present(autoAddConnectors)) optAutoAddConnectors = autoAddConnectors

    ! query the Component for info
    call ESMF_GridCompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! access the FreeFormat lineCount
    call NUOPC_FreeFormatGet(freeFormat, lineCount=lineCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! determine slotCount and potentially automatically add connectors
    slotCount = 0
    do i=1, lineCount
      call NUOPC_FreeFormatGetLine(freeFormat, line=i, tokenCount=tokenCount, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      allocate(tokenList(tokenCount))
      call NUOPC_FreeFormatGetLine(freeFormat, line=i, tokenList=tokenList, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      if (tokenCount == 1) then
        if (index(trim(tokenList(1)),"@") == 1) then
          slotCount = slotCount + 1
        endif
      elseif (optAutoAddConnectors .and. &
        ((tokenCount == 3) .or. (tokenCount == 4))) then
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
          dstCompLabel=trim(tokenList(3)), comp=conn, relaxedflag=.true., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        if (.not.ESMF_CplCompIsCreated(conn)) then
          ! this is a new connector component that needs to be added to driver
          call NUOPC_DriverAddComp(driver, &
            srcCompLabel=trim(tokenList(1)), dstCompLabel=trim(tokenList(3)), &
            compSetServicesRoutine=cplSS, comp=conn, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          if (tokenCount == 4) then
            ! there are additional connection options specified
            ! -> set as Attribute for now on the connector object
            call ESMF_AttributeSet(conn, name="ConnectionOptions", &
              value=trim(tokenList(4)), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
          endif
        endif
      endif
      deallocate(tokenList)
    enddo
    slotCount = (slotCount+1) / 2
    slotCount = max(slotCount, 1) ! at least one slot

    allocate(slotStack(slotCount))

    ! Replace the default RunSequence with a customized one
    call NUOPC_DriverNewRunSequence(driver, slotCount=slotCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    ! Get driver internalClock
    call ESMF_GridCompGet(driver, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    level = 0
    slot = 0
    slotHWM = 0
    do i=1, lineCount
      call NUOPC_FreeFormatGetLine(freeFormat, line=i, tokenCount=tokenCount, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      allocate(tokenList(tokenCount))
      call NUOPC_FreeFormatGetLine(freeFormat, line=i, tokenList=tokenList, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
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
          ! time step indicator
          tempString=trim(tokenList(1))
          if (len(trim(tempString)) > 1) then
            ! entering new time loop level
            level = level + 1
            slotStack(level)=slot
            slot = slotHWM + 1
            slotHWM = slotHWM + 1
            read(tempString(2:len(tempString)), *) seconds
            print *, "found time step indicator: ", seconds
            call ESMF_TimeIntervalSet(timeStep, s_r8=seconds, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=trim(name)//":"//FILENAME)) &
              return  ! bail out
            if (slot==1) then
              ! Set the timeStep of the internalClock
              call ESMF_ClockSet(internalClock, timeStep=timeStep, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=trim(name)//":"//FILENAME)) &
                return  ! bail out
            else
              ! Insert the link to a new slot, and set the timeStep
              call NUOPC_DriverAddRunElement(driver, slot=slotStack(level), &
                linkSlot=slot, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
              subClock = ESMF_ClockCreate(internalClock, rc=rc)  ! make a copy first
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
              call ESMF_ClockSet(subClock, timeStep=timeStep, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
              call NUOPC_DriverSetRunSequence(driver, slot=slot, &
                clock=subClock, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            endif
          else
            ! exiting time loop level
            slot = slotStack(level)
            level = level - 1
          endif
        else
          ! model
          slot = max(slot, 1) ! model outside of a time loop
          call NUOPC_DriverAddRunElement(driver, slot=slot, &
            compLabel=trim(tokenList(1)), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=trim(name)//":"//FILENAME)) &
            return  ! bail out
        endif
      elseif (tokenCount == 2) then
        ! a model with a specific phase label
        call NUOPC_DriverAddRunElement(driver, slot=slot, &
          compLabel=trim(tokenList(1)), phaseLabel=trim(tokenList(2)), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=trim(name)//":"//FILENAME)) &
          return  ! bail out
      elseif ((tokenCount == 3) .or. (tokenCount == 4)) then
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
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=trim(name)//":"//FILENAME)) &
          return  ! bail out
      endif    
      
      ! clean-up
      deallocate(tokenList)
    enddo
    ! clean-up
    deallocate(slotStack)
    
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
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is

    if (present(rc)) rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! Deallocate the current RunSequence and add a new one with slotCount
    call NUOPC_RunSequenceDeallocate(is%wrap%runSeq, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call NUOPC_RunSequenceAdd(is%wrap%runSeq, slotCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
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
    character(ESMF_MAXSTR)          :: name
    type(ESMF_VM)                   :: vm
    logical                         :: forceOrder
    type(type_InternalState)        :: is
    type(ComponentMapEntry)         :: cmEntry
    type(ConnectorMapEntry)         :: cnEntry
    integer                         :: componentMapCount, connectorMapCount
    integer                         :: i, pet, petCount, localPet

    if (present(rc)) rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(driver, name=name, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! get basic information about componentMap and connectorMap
    call ESMF_ContainerGet(is%wrap%componentMap, itemCount=componentMapCount, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call ESMF_ContainerGet(is%wrap%connectorMap, itemCount=connectorMapCount, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! deal with ordering
    forceOrder = .false.  ! default
    if (present(orderflag)) then
      forceOrder = orderflag
    endif
    call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
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
            cmEntry, ESMF_ITEMORDER_ADDORDER, rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          print *, i,":  ", trim(cmEntry%wrap%label)
        enddo
        
        ! Print information about the Connector components
        print *, "  Connector components, in the order"
        print *, "  that they were added to the Driver:"
        do i=1, connectorMapCount
          call ESMF_ContainerGetUDTByIndex(is%wrap%connectorMap, i, &
            cnEntry, ESMF_ITEMORDER_ADDORDER, rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          print *, i,":  ", trim(cnEntry%wrap%label)
        enddo
        
        ! Print the RunSequence
        call NUOPC_RunSequencePrint(is%wrap%runSeq, logflag=.false., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out

      endif
      if (forceOrder) then
        call ESMF_VMBarrier(vm, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
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
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is

    if (present(rc)) rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! Set clock of the selected RunSequence slot
    call NUOPC_RunSequenceSet(is%wrap%runSeq(slot), clock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  recursive subroutine IInitAdvertize(driver, importState, exportState, clock, &
    rc)
    type(ESMF_GridComp)  :: driver
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    character(ESMF_MAXSTR)        :: name

    rc = ESMF_SUCCESS
    
    ! query the Component for info
    call ESMF_GridCompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    if (ESMF_StateIsCreated(importState, rc=rc)) then
      ! request that connectors transfer all fields into the importState
      call NUOPC_SetAttribute(importState, name="FieldTransferPolicy", &
        value="transferAll", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif
    
    if (ESMF_StateIsCreated(exportState, rc=rc)) then
      ! request that connectors transfer all fields into the exportState
      call NUOPC_SetAttribute(exportState, name="FieldTransferPolicy", &
        value="transferAll", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif

  end subroutine
  
  !-----------------------------------------------------------------------------

  recursive subroutine IInitAdvertizeFinish(driver, importState, exportState, &
    clock, rc)
    type(ESMF_GridComp)  :: driver
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    character(ESMF_MAXSTR)        :: name

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    if (ESMF_StateIsCreated(importState, rc=rc)) then
      ! reset FieldTransferPolicy to prevent interaction w upper hierarchy layer
      call NUOPC_SetAttribute(importState, name="FieldTransferPolicy", &
        value="transferNone", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif
    
    if (ESMF_StateIsCreated(exportState, rc=rc)) then
      ! reset FieldTransferPolicy to prevent interaction w upper hierarchy layer
      call NUOPC_SetAttribute(exportState, name="FieldTransferPolicy", &
        value="transferNone", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  recursive subroutine IInitModifyCplLists(driver, importState, exportState, &
    clock, rc)
    type(ESMF_GridComp)  :: driver
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    character(ESMF_MAXSTR)          :: name
    integer                         :: localrc
    logical                         :: existflag
    type(ESMF_CplComp)              :: connector
    integer                         :: i
    type(type_InternalState)        :: is

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! add REMAPMETHOD=redist option to all of the CplList entries for all
    ! Connectors to/from driver-self
    do i=1, is%wrap%modelCount
      connector = is%wrap%connectorComp(i,0)
      if (NUOPC_CompAreServicesSet(connector)) then
        call addCplListOption(connector, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
    enddo
    do i=1, is%wrap%modelCount
      connector = is%wrap%connectorComp(0,i)
      if (NUOPC_CompAreServicesSet(connector)) then
        call addCplListOption(connector, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
    enddo
    
    ! SPECIALIZE by calling into optional attached method allowing modification
    ! of the "CplList" metadata on child Connectors.
    call ESMF_MethodExecute(driver, label=label_ModifyCplLists, &
      existflag=existflag, userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
      
  contains
  
    recursive subroutine addCplListOption(connector, rc)
      type(ESMF_CplComp)              :: connector
      integer, intent(out)            :: rc
      ! local variables
      integer                         :: j, cplListSize
      character(len=160), allocatable :: cplList(:)
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
          ! switch remapping to redist
          cplList(j) = trim(cplList(j))//":REMAPMETHOD=redist"
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
    character(ESMF_MAXSTR)        :: name

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    if (ESMF_StateIsCreated(importState, rc=rc)) then
      ! - check that all connected fields in importState have producer
      call checkProducerConnection(importState, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif
    
    if (ESMF_StateIsCreated(exportState, rc=rc)) then
      ! - check that all connected fields in exportState have producer
      call checkProducerConnection(exportState, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif
  
  contains
  
    !---------------------------------------------------------------------------

    recursive subroutine checkProducerConnection(state, rc)
      type(ESMF_State)     :: state
      integer, intent(out) :: rc
      
      ! local variables    
      integer                         :: i
      type(ESMF_Field), pointer       :: fieldList(:)
      character(ESMF_MAXSTR), pointer :: itemNameList(:)
      logical                         :: connected
      logical                         :: producerConnected, consumerConnected
      character(ESMF_MAXSTR)          :: stateName

      rc = ESMF_SUCCESS

      nullify(fieldList)
      nullify(itemNameList)
      call NUOPC_GetStateMemberLists(state, itemNameList=itemNameList, &
        fieldList=fieldList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      if (associated(fieldList)) then
        do i=1, size(fieldList)
          call checkConnections(fieldList(i), connected, producerConnected, &
            consumerConnected, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (connected .and. .not.producerConnected) then
            ! a connected field in a Driver state must have a ProducerConnection
            call ESMF_StateGet(state, name=stateName, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
              msg="Connected Field in Driver State "//trim(stateName)//&
              " must have ProducerConnection: "//trim(itemNameList(i)), &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
            return ! bail out
          endif
        enddo
      endif
      if (associated(fieldList)) deallocate(fieldList)
      if (associated(itemNameList)) deallocate(itemNameList)
      
    end subroutine

    !---------------------------------------------------------------------------

    recursive subroutine checkConnections(field, connected, producerConnected, &
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
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      connected = (value=="true")
      call NUOPC_GetAttribute(field, name="ProducerConnection", &
        value=value, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      producerConnected = (value/="open")
      call NUOPC_GetAttribute(field, name="ConsumerConnection", &
        value=value, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      consumerConnected = (value/="open")
    end subroutine
    
    !---------------------------------------------------------------------------

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
    logical                         :: consumerConnected
    character(ESMF_MAXSTR)          :: stateName, fieldName
    character(len=80)               :: value

    rc = ESMF_SUCCESS

    nullify(fieldList)
    nullify(itemNameList)
    call NUOPC_GetStateMemberLists(state, itemNameList=itemNameList, &
      fieldList=fieldList, rc=rc)
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
            call ESMF_StateRemove(state, (/fieldName/), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        endif
      enddo
    endif
    if (associated(fieldList)) deallocate(fieldList)
    if (associated(itemNameList)) deallocate(itemNameList)
    
  end subroutine

  !-----------------------------------------------------------------------------

  recursive subroutine IInitRealize(driver, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: driver
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    character(ESMF_MAXSTR)        :: name

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    if (ESMF_StateIsCreated(importState, rc=rc)) then
      ! - complete all the fields in the importState
      call completeAllFields(importState, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif

    if (ESMF_StateIsCreated(exportState, rc=rc)) then
      ! - complete all the fields in the exportState
      call completeAllFields(exportState, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
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
      integer                         :: itemCount, stat
      integer                         :: ulbCount, uubCount
      logical                         :: isPresent
      integer(ESMF_KIND_I4), pointer  :: ungriddedLBound(:), ungriddedUBound(:)
      integer(ESMF_KIND_I4), pointer  :: gridToFieldMap(:)
      
      rc = ESMF_SUCCESS

      nullify(fieldList)
      nullify(itemNameList)
      call NUOPC_GetStateMemberLists(state, itemNameList=itemNameList, &
        fieldList=fieldList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      if (associated(fieldList)) then
        do i=1, size(fieldList)
          ! the transferred Grid is already set, allocate memory for data
          !TODO: This assumes R8, etc. Make this 
          !TODO: more general to handle all field cases.

          ! GridToFieldMap attribute
          call ESMF_AttributeGet(fieldList(i), name="GridToFieldMap", &
            convention="NUOPC", purpose="Instance", &
            itemCount=itemCount, isPresent=isPresent, &
            attnestflag=ESMF_ATTNEST_ON, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (.not. isPresent) then
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
              msg="Cannot realize field "//trim(itemNameList(i))//&
              " because GridToFieldMap attribute is not present", &
              line=__LINE__, file=trim(name)//":"//FILENAME, &
              rcToReturn=rc)
            return
          endif
          allocate(gridToFieldMap(itemCount), stat=stat)
          if (ESMF_LogFoundAllocError(statusToCheck=stat, &
            msg="Allocation of internal gridToFieldMap failed.", &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          call ESMF_AttributeGet(fieldList(i), &
            name="GridToFieldMap", valueList=gridToFieldMap, &
            convention="NUOPC", purpose="Instance", &
            attnestflag=ESMF_ATTNEST_ON, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, &
            msg="Cannot realize field "//trim(itemNameList(i))// &
            " because error obtaining GridToFieldMap attribute.", &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

          ! UngriddedLBound, UngriddedUBound attributes
          call ESMF_AttributeGet(fieldList(i), name="UngriddedLBound", &
               convention="NUOPC", purpose="Instance", &
               itemCount=ulbCount, isPresent=isPresent, &
               attnestflag=ESMF_ATTNEST_ON, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
               line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (isPresent .and. ulbCount > 0) then           
             call ESMF_AttributeGet(fieldList(i), name="UngriddedUBound", &
               convention="NUOPC", purpose="Instance", &
               itemCount=uubCount, isPresent=isPresent, &
               attnestflag=ESMF_ATTNEST_ON, rc=rc)
             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                 line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
             if (.not. isPresent .or. ulbCount /= uubCount) then
                call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
                     msg="Field "//trim(itemNameList(i))//&
                     " has inconsistent UngriddedLBound/UngriddedUBound attributes", &
                     line=__LINE__, file=trim(name)//":"//FILENAME, &
                     rcToReturn=rc)
                return
             endif
             allocate(ungriddedLBound(ulbCount), &
                  ungriddedUBound(uubCount), stat=stat)
             if (ESMF_LogFoundAllocError(statusToCheck=stat, &
                  msg="Allocation of internal ungriddedLBound/ungriddedUBound failed.", &
                  line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                  return  ! bail out
             call ESMF_AttributeGet(fieldList(i), &
                  name="UngriddedLBound", valueList=ungriddedLBound, &
                  convention="NUOPC", purpose="Instance", &
                  attnestflag=ESMF_ATTNEST_ON, rc=rc)
             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
             call ESMF_AttributeGet(fieldList(i), &
                  name="UngriddedUBound", valueList=ungriddedUBound, &
                  convention="NUOPC", purpose="Instance", &
                  attnestflag=ESMF_ATTNEST_ON, rc=rc)
             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out                          
             ! create field with ungridded dims
             !print *, "Creating field with ungridded dims ", ungriddedLBound(:), ungriddedUBound(:)
             call ESMF_FieldEmptyComplete(fieldList(i), &
                  gridToFieldMap=gridToFieldMap, typekind=ESMF_TYPEKIND_R8, &
                  ungriddedLBound=ungriddedLBound, &
                  ungriddedUBound=ungriddedUBound, &
                  rc=rc)
             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
             deallocate(ungriddedLBound, ungriddedUBound)
          else   
             ! create field with no ungridded dims
             call ESMF_FieldEmptyComplete(fieldList(i), &
                  gridToFieldMap=gridToFieldMap, typekind=ESMF_TYPEKIND_R8, rc=rc)
             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          endif      
          deallocate(gridToFieldMap)
       enddo
      endif
      if (associated(fieldList)) deallocate(fieldList)
      if (associated(itemNameList)) deallocate(itemNameList)
      
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
    character(ESMF_MAXSTR)        :: name
    type(ESMF_Clock)              :: internalClock
    type(ESMF_Time)               :: time
    type(ESMF_Field), allocatable :: fieldList(:)
    character(ESMF_MAXSTR)        :: fieldName
    integer                       :: i

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

#ifdef DEBUG
call ESMF_LogWrite(trim(name)//": Entering InternalInitializeComplete", &
  ESMF_LOGMSG_INFO)
#endif

    ! conditionally handle how InitializeDataComplete is set
    if (ESMF_StateIsCreated(exportState, rc=rc)) then
      call ESMF_GridCompGet(driver, clock=internalClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
      call ESMF_ClockGet(internalClock, currTime=time, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
      if (NUOPC_IsAtTime(exportState, time, fieldList=fieldList, rc=rc)) then
        ! indicate that data initialization is complete 
        ! (breaking out of init-loop)
        call NUOPC_CompAttributeSet(driver, &
          name="InitializeDataComplete", value="true", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) &
          return  ! bail out
#ifdef DEBUG
      else
        do i=1, size(fieldList)
          call ESMF_FieldGet(fieldList(i), name=fieldName, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) &
            return  ! bail out
          call ESMF_LogWrite(trim(name)//": Field not at expected time: "&
            //trim(fieldName), ESMF_LOGMSG_WARNING)
        enddo
        deallocate(fieldList)
#endif
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
    
#ifdef DEBUG
call ESMF_LogWrite(trim(name)//": Exiting InternalInitializeComplete", &
  ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

  recursive subroutine InitializeIPDv02p5(gcomp, importState, exportState, &
    clock, rc)
    ! direct copy of the InitializeP5 routine in NUOPC_Model!!!!
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    
    ! local variables
    integer               :: localrc
    type(ESMF_Clock)      :: internalClock
    logical               :: existflag
    character(ESMF_MAXSTR):: name, oldDataComplete, newDataComplete
    integer               :: oldUpdatedCount, newUpdatedCount
    logical               :: allUpdated

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(gcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

! For a Driver may be called without valid states
if (ESMF_StateIsCreated(importState).and.ESMF_StateIsCreated(exportState)) then
    ! check how many Fields in the exportState have the "Updated" Attribute set
    ! to "true" BEFORE calling the DataInitialize
    allUpdated = NUOPC_IsUpdated(exportState, count=oldUpdatedCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! get the value of the "InitializeDataComplete" Attribute
    call NUOPC_CompAttributeGet(gcomp, name="InitializeDataComplete", &
      value=oldDataComplete, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
endif

#if 0
    ! Initialize component data structures, including its export Fields,
    ! only connected Fields reside in exportState at this time.
    ! Expect the component to set "InitializeDataComplete" Attribute when done.
    ! SPECIALIZE by calling into attached method to fill initial data
    call ESMF_MethodExecute(gcomp, label=label_DataInitialize, &
      existflag=existflag, userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, &
      rcToReturn=rc)) &
      return  ! bail out
#else
! It could be implemented via DataInitialize (as for NUOPC_Model), but for now
! I just use a direct call into subroutine...
    call InitializeIPDv02p5Data(gcomp, importState, exportState, clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
#endif

! For a Driver may be called without valid states
if (ESMF_StateIsCreated(importState).and.ESMF_StateIsCreated(exportState)) then
    ! re-set the "InitializeDataProgress" Attribute to "false"
    call NUOPC_CompAttributeSet(gcomp, &
      name="InitializeDataProgress", value="false", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! check how many Fields in the exportState have the "Updated" Attribute set
    ! to "true" AFTER calling the DataInitialize
    allUpdated = NUOPC_IsUpdated(exportState, count=newUpdatedCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      
    ! see if number of updated export fields went up
    if (newUpdatedCount > oldUpdatedCount) then
      ! there are more Fields now that have their "Updated" Attribute set "true"
      ! -> set "InitializeDataProgress" Attribute "true"
      call NUOPC_CompAttributeSet(gcomp, &
        name="InitializeDataProgress", value="true", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    endif
    
    ! get the value of the "InitializeDataComplete" Attribute
    call NUOPC_CompAttributeGet(gcomp, name="InitializeDataComplete", &
      value=newDataComplete, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    
    ! see if the "InitializeDataComplete" Attribute has changed
    if (trim(newDataComplete) /= trim(oldDataComplete)) then
      ! there was a change in the "InitializeDataComplete" Attribute setting
      ! -> set "InitializeDataProgress" Attribute "true"
      call NUOPC_CompAttributeSet(gcomp, &
        name="InitializeDataProgress", value="true", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    endif
    
    ! correct setting of timestamps
    call ESMF_GridCompGet(gcomp, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    if (allUpdated) then
      ! update timestamp on all the export Fields
      call NUOPC_UpdateTimestamp(exportState, internalClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
    else
      ! update timestamp on only those export Fields that have the 
      ! "Updated" Attribute set to "true"
      call NUOPC_UpdateTimestamp(exportState, internalClock, &
        selective=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
    endif
endif
  end subroutine

  !-----------------------------------------------------------------------------

end module
