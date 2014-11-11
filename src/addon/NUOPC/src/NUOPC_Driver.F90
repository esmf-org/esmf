! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2014, University Corporation for Atmospheric Research, 
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

  implicit none
  
  private
  
  public routine_SetServices
  public type_InternalState, type_InternalStateStruct
  public type_PetList
  public label_InternalState
  public label_SetModelCount, label_SetModelPetLists
  public label_ModifyInitializePhaseMap
  public label_SetModelServices, label_SetRunSequence, label_Finalize
  public label_SetRunClock
  
  character(*), parameter :: &
    label_InternalState = "Driver_InternalState"
  character(*), parameter :: &
    label_SetModelCount = "Driver_SetModelCount"
  character(*), parameter :: &
    label_SetModelPetLists = "Driver_SetModelPetLists"
  character(*), parameter :: &
    label_SetModelServices = "Driver_SetModelServices"
  character(*), parameter :: &
    label_SetRunSequence = "Driver_SetRunSequence"
  character(*), parameter :: &
    label_ModifyInitializePhaseMap = "Driver_ModifyInitializePhaseMap"
  character(*), parameter :: &
    label_Finalize = "Driver_Finalize"
  character(*), parameter :: &
    label_SetRunClock = "Driver_SetRunClock"

  type type_InternalStateStruct
    integer                           :: modelCount
    type(type_PetList),  pointer      :: modelPetLists(:)
    type(type_PetList),  pointer      :: connectorPetLists(:,:)
    !--- private members ----------------------------------------
    ! - new style members
    type(ESMF_Container)              :: componentMap
    type(ESMF_Container)              :: connectorMap
    integer                           :: componentMapCount
    logical                           :: newStyleFlag
    ! - old style members
    type(ESMF_GridComp), pointer      :: modelComp(:)
    type(ESMF_State),    pointer      :: modelIS(:), modelES(:)
    type(ESMF_CplComp),  pointer      :: connectorComp(:,:)
    ! - common members
    type(NUOPC_RunSequence), pointer  :: runSeq(:)  ! size may increase dynamic.
    integer                           :: runPhaseToRunSeqMap(10)
    type(ESMF_Clock)                  :: driverClock  ! clock of the parent
  end type

  type type_InternalState
    type(type_InternalStateStruct), pointer :: wrap
  end type
  
  type type_PetList
    integer, pointer :: petList(:)  ! lists that are set here transfer ownership
  end type
  
  type PhaseMapParser
    integer                                            :: phaseCount
    integer, pointer                                   :: phaseValue(:)
    character(len=NUOPC_PhaseMapStringLength), pointer :: phases(:)
    character(len=NUOPC_PhaseMapStringLength), pointer :: phaseKey(:)
  end type
  
  ! Generic methods
  public NUOPC_DriverAddComp
  public NUOPC_DriverAddRunElement
  public NUOPC_DriverGetComp
  public NUOPC_DriverNewRunSequence
  public NUOPC_DriverPrint
  public NUOPC_DriverSetRunSequence
  public NUOPC_DriverSet
  public NUOPC_DriverSetModel
  
  ! interface blocks
  !---------------------------------------------
  interface NUOPC_DriverAddComp
    module procedure NUOPC_DriverAddGridComp
    module procedure NUOPC_DriverAddCplComp
  end interface
  !---------------------------------------------
  interface NUOPC_DriverAddRunElement
    module procedure NUOPC_DriverAddRunElementM
    module procedure NUOPC_DriverAddRunElementMPL
    module procedure NUOPC_DriverAddRunElementC
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
  end type
  type ComponentMapEntry
    type(ComponentMapEntryT), pointer :: wrap
  end type
  !---------------------------------------------
  type ConnectorMapEntryT
    character(len=330)              :: label
    type(ESMF_CplComp)              :: connector
  end type
  type ConnectorMapEntry
    type(ConnectorMapEntryT), pointer :: wrap
  end type
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine routine_SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    character(ESMF_MAXSTR):: name

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(gcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! add standard NUOPC GridComp Attribute Package to the Model
    call NUOPC_CompAttributeAdd(gcomp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! Initialize phases

    ! Phase 0 requires use of ESMF method.
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! For backward compatibility with v6 API the sequence of the following
    ! NUOPC_CompSetEntryPoint() calls is critical to produce the old default
    ! InitializePhaseMap.

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p1", "IPDv01p1", "IPDv02p1", "IPDv03p1"/), &
      userRoutine=InitializeP1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! Run phases
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"RunPhase1"/), userRoutine=Run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! Run specialization
    call ESMF_MethodAdd(gcomp, label=label_SetRunClock, &
      userRoutine=SetRunClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
      
    ! Finalize phases
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_FINALIZE, &
      phaseLabelList=(/"FinalizePhase1"/), userRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
          
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    
    ! local variables
    character(ESMF_MAXSTR):: name

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(gcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! filter all other entries but those of type IPDv00
    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv00p"/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  recursive subroutine InitializeP1(gcomp, importState, exportState, clock, rc)
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
    type(PhaseMapParser), allocatable ::  modelPhaseMap(:)
    type(PhaseMapParser), allocatable ::  connectorPhaseMap(:,:)
    character(ESMF_MAXSTR)    :: name
    character(len=160)        :: namespace  ! long engough for component label
    logical                   :: execFlag, execFlagCollect
    integer                   :: execFlagIntReduced, execFlagInt
    type(ComponentMapEntry)   :: cmEntry

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(gcomp, name=name, rc=rc)
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
      
    if (.not.clockIsPresent .and. NUOPC_IsCreated(clock)) then
      ! set the internal Clock as a copy of the incoming Clock by a default
      call NUOPC_CompSetClock(gcomp, clock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif

    ! SPECIALIZE by calling into attached method to set modelCount
    call ESMF_MethodExecute(gcomp, label=label_SetModelCount, &
      userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
      
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
    allocate(modelPhaseMap(0:is%wrap%modelCount), &
      connectorPhaseMap(0:is%wrap%modelCount, 0:is%wrap%modelCount), &
      stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of temporary data structure.", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! SPECIALIZE by calling into optional attached method to set petLists
    call ESMF_MethodExecute(gcomp, label=label_SetModelPetLists, &
      existflag=existflag, userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! prepare the new style members in the internal state
    is%wrap%newStyleFlag = .false.  ! old style by default
    is%wrap%componentMap = ESMF_ContainerCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call ESMF_ContainerGarbageOn(is%wrap%componentMap, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    is%wrap%componentMapCount = 0 ! reset
    is%wrap%connectorMap = ESMF_ContainerCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call ESMF_ContainerGarbageOn(is%wrap%connectorMap, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! create modelComps and their import and export States + connectorComps
    do i=0, is%wrap%modelCount
      write (iString, *) i
      
      i_petList => is%wrap%modelPetLists(i)%petList
      
      if (i==0) then
      
        is%wrap%modelComp(0) = gcomp      ! driver itself is in slot 0
        is%wrap%modelIS(0) = importState  ! driver import State
        is%wrap%modelES(0) = exportState  ! driver export State
        
      else if (i>0) then
      
        if (associated(i_petList)) then
          write (lString, *) size(i_petList)
          write (msgString,"(A)") trim(name)//&
            " - Creating model component #"//trim(adjustl(iString))//&
            " with petList of size "//trim(adjustl(lString))//" :"
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          
          if (size(i_petList) <= 1000) then
            ! have the resources to print the entire petList
            write (petListBuffer, "(10I7)") i_petList
            lineCount = size(i_petList)/10
            if ((size(i_petList)/10)*10 /= size(i_petList)) &
              lineCount = lineCount + 1
            do k=1, lineCount
              call ESMF_LogWrite(petListBuffer(k), ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            enddo
          endif
            
          is%wrap%modelComp(i) = ESMF_GridCompCreate(name="modelComp "// &
            trim(adjustl(iString)), petList=i_petList, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
        else
          write (msgString,"(A)") trim(name)//" - Creating model component #"//&
            trim(adjustl(iString))//" without petList."
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          is%wrap%modelComp(i) = ESMF_GridCompCreate(name="modelComp "// &
            trim(adjustl(iString)), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
        endif
        
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
        
        ! set rootVas Attribute on the States to help during AttributeUpdate
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
        call ESMF_AttributeSet(is%wrap%modelIS(i), name="rootVas", &
          value=rootVas, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        call ESMF_AttributeSet(is%wrap%modelES(i), name="rootVas", &
          value=rootVas, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out

        ! add standard NUOPC GridComp Attribute Package to the modelComp
        call NUOPC_CompAttributeAdd(is%wrap%modelComp(i), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
          
      endif
      
      ! initialize the modelPhaseMap pointer members
      nullify(modelPhaseMap(i)%phaseValue)
      nullify(modelPhaseMap(i)%phases)
      nullify(modelPhaseMap(i)%phaseKey)
        
      ! create connectorComps
      do j=0, is%wrap%modelCount
        write (jString, *) j
        
        c_petList => is%wrap%connectorPetLists(i,j)%petList
        if (.not.associated(c_petList)) then
          ! see if a default c_petList must be constructed
          j_petList => is%wrap%modelPetLists(j)%petList
          if (associated(i_petList).and.associated(j_petList)) then
            ! construct the union petList of model i and j for i->j connector
            allocate(c_petList(size(i_petlist)+size(j_petList)), stat=stat)
            if (ESMF_LogFoundAllocError(statusToCheck=stat, &
              msg="Allocation #1 of connector petList failed.", &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
            c_petList = i_petList ! copy contents i_petList start of c_petList
            ! there is no guarantee of order, no way to optimize construction
            cIndex = size(i_petList) + 1
            do k=1, size(j_petList)
              ! append element k in j_petList to c_petList if not yet present
              do l=1, size(i_petList)
                if (c_petList(l) == j_petList(k)) exit
              enddo
              if (l == size(i_petList) + 1) then
                ! append element
                c_petList(cIndex) = j_petList(k)
                cIndex = cIndex + 1
              endif
            enddo
            if (cIndex-1 < size(c_petList)) then
              ! shrink the size of c_petList to just what it needs to be
              allocate(is%wrap%connectorPetLists(i,j)%petList(cIndex-1), &
                stat=stat)
              if (ESMF_LogFoundAllocError(statusToCheck=stat, &
                msg="Allocation #2 of connector petList failed.", &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
              is%wrap%connectorPetLists(i,j)%petList = c_petList(1:cIndex-1)
              deallocate(c_petList, stat=stat)
              if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
                msg="Deallocation of connector petList failed.", &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
              c_petList => is%wrap%connectorPetLists(i,j)%petList
            else
              ! c_petList is already the right size
              is%wrap%connectorPetLists(i,j)%petList => c_petList
            endif
          endif
        endif

        if (associated(c_petList)) then
          write (lString, *) size(c_petList)
          write (msgString,"(A)") trim(name)//&
            " - Creating connector component "//trim(adjustl(iString))//&
            " -> "//trim(adjustl(jString))//" with petList of size "//&
            trim(adjustl(lString))//" :"
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          
          if (size(c_petList) <= 1000) then
            ! have the resources to print the entire petList
            write (petListBuffer, "(10I7)") c_petList
            lineCount = size(c_petList)/10
            if ((size(c_petList)/10)*10 /= size(c_petList)) &
              lineCount = lineCount + 1
            do k=1, lineCount
              call ESMF_LogWrite(petListBuffer(k), ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            enddo
          endif
            
          is%wrap%connectorComp(i,j) = ESMF_CplCompCreate(name="connectorComp "//&
            trim(adjustl(iString))//" -> "//trim(adjustl(jString)), &
            petList=c_petList, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        else
          write (msgString,"(A)") trim(name)//" - Creating connector component "//&
            trim(adjustl(iString))//" -> "//trim(adjustl(jString))//&
            " without petList."
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          is%wrap%connectorComp(i,j) = ESMF_CplCompCreate(name="connectorComp "//&
            trim(adjustl(iString))//" -> "//trim(adjustl(jString)), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        endif
        
        ! add standard NUOPC CplComp Attribute Package to the connectorComp
        call NUOPC_CompAttributeAdd(is%wrap%connectorComp(i,j), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out

        ! initialize the connectorPhaseMap pointer members
        nullify(connectorPhaseMap(i,j)%phaseValue)
        nullify(connectorPhaseMap(i,j)%phases)
        nullify(connectorPhaseMap(i,j)%phaseKey)
      enddo
    enddo

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
    ! ... 2nd block: connectors all of model components -> driver
    do i=1, is%wrap%modelCount
      j=0
      call NUOPC_RunElementAdd(is%wrap%runSeq(1), i=i, j=j, phase=1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    enddo
    
    ! SPECIALIZE by calling into attached method to SetServices for modelComps
    call ESMF_MethodExecute(gcomp, label=label_SetModelServices, &
      userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! now the component labels are available -> create States with Namespace
    do i=1, is%wrap%modelCount
      if (is%wrap%newStyleFlag) then
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
      call NUOPC_StateAttributeAdd(is%wrap%modelIS(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call NUOPC_StateAttributeSet(is%wrap%modelIS(i), &
        name="Namespace", value=trim(namespace), &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      ! add State level attributes, set the namespace according to comp label
      call NUOPC_StateAttributeAdd(is%wrap%modelES(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call NUOPC_StateAttributeSet(is%wrap%modelES(i), &
        name="Namespace", value=trim(namespace), &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    enddo
      
    ! query Component for its Clock (set during specialization)
    call ESMF_GridCompGet(gcomp, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    
    ! -> NUOPC Initialize Sequence requires presence of InitP0 for every 
    ! -> Model and Connector component, where they must set the
    ! -> "InitializePhaseMap" metadata.
      
    ! InitP0: modelComps
    call loopModelComps(phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! InitP0: connectorComps
    call loopConnectorComps(phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
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
    
    ! SPECIALIZE by calling into optional attached method that sets RunSequence
    call ESMF_MethodExecute(gcomp, label=label_SetRunSequence, &
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
          ! setup modelPhaseMap
          call setupConnectorPhaseMap(i=i, j=j, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
        endif
      enddo
    enddo

    ! -> Now encode the NUOPC IPDv00, IPDv01, IPDv02, IPDv03, IPDv04
      
    ! modelComps
    call loopModelCompsS(phaseString="IPDv00p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(phaseString="IPDv01p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(phaseString="IPDv02p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(phaseString="IPDv03p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(phaseString="IPDv04p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! connectorComps
    call loopConnectorCompsS(phaseString="IPDv00p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(phaseString="IPDv01p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(phaseString="IPDv02p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(phaseString="IPDv03p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(phaseString="IPDv04p1a", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(phaseString="IPDv04p1b", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! modelComps
    call loopModelCompsS(phaseString="IPDv01p2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(phaseString="IPDv02p2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(phaseString="IPDv03p2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(phaseString="IPDv04p2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! connectorComps
    call loopConnectorCompsS(phaseString="IPDv00p2a", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(phaseString="IPDv00p2b", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(phaseString="IPDv01p2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(phaseString="IPDv02p2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(phaseString="IPDv03p2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(phaseString="IPDv04p2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! modelComps
    call loopModelCompsS(phaseString="IPDv00p2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(phaseString="IPDv01p3", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(phaseString="IPDv02p3", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(phaseString="IPDv03p3", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(phaseString="IPDv04p3", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! connectorComps
    call loopConnectorCompsS(phaseString="IPDv03p3", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(phaseString="IPDv04p3", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! modelComps
    call loopModelCompsS(phaseString="IPDv03p4", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(phaseString="IPDv04p4", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! connectorComps
    call loopConnectorCompsS(phaseString="IPDv03p4", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(phaseString="IPDv04p4", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! modelComps
    call loopModelCompsS(phaseString="IPDv03p5", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(phaseString="IPDv04p5", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! connectorComps
    call loopConnectorCompsS(phaseString="IPDv01p3a", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(phaseString="IPDv01p3b", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(phaseString="IPDv02p3a", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(phaseString="IPDv02p3b", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(phaseString="IPDv03p5a", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(phaseString="IPDv03p5b", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(phaseString="IPDv04p5a", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(phaseString="IPDv04p5b", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! modelComps
    call loopModelCompsS(phaseString="IPDv00p3", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(phaseString="IPDv01p4", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(phaseString="IPDv02p4", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(phaseString="IPDv03p6", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(phaseString="IPDv04p6", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! modelComps
    call loopModelCompsS(phaseString="IPDv00p4", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(phaseString="IPDv01p5", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    execFlagCollect = .false.
    call loopModelCompsS(phaseString="IPDv02p5", execFlag=execFlag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    execFlagCollect = execFlagCollect.or.execFlag
    call loopModelCompsS(phaseString="IPDv03p7", execFlag=execFlag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    execFlagCollect = execFlagCollect.or.execFlag
    call loopModelCompsS(phaseString="IPDv04p7", execFlag=execFlag, rc=rc)
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
      ! there were model components with IPDv02p5 or IPDv03p7
      !  -> resolve data dependencies by entering loop
      call loopDataDependentInitialize(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
    endif
    
#define DEBUGPRINT____disable
#ifdef DEBUGPRINT
    ! print the entire runSeq structure
    call NUOPC_RunSequencePrint(is%wrap%runSeq, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
#endif

    ! local garbage collection -> PhaseMap pointer members
    do i=0, is%wrap%modelCount
      do j=0, is%wrap%modelCount
        if (j==i) cycle ! skip
        if (associated(connectorPhaseMap(i,j)%phaseValue)) &
          deallocate(connectorPhaseMap(i,j)%phaseValue)
        if (associated(connectorPhaseMap(i,j)%phases)) &
          deallocate(connectorPhaseMap(i,j)%phases)
        if (associated(connectorPhaseMap(i,j)%phaseKey)) &
          deallocate(connectorPhaseMap(i,j)%phaseKey)
      enddo
      if (associated(modelPhaseMap(i)%phaseValue)) &
        deallocate(modelPhaseMap(i)%phaseValue)
      if (associated(modelPhaseMap(i)%phases)) &
        deallocate(modelPhaseMap(i)%phases)
      if (associated(modelPhaseMap(i)%phaseKey)) &
        deallocate(modelPhaseMap(i)%phaseKey)
    enddo

    contains !----------------------------------------------------------------
    
      recursive subroutine loopModelComps(phase, rc)
        ! only to be used for phase=0
        integer, intent(in)     :: phase
        integer, intent(out)    :: rc
        integer                 :: i
        character(ESMF_MAXSTR)  :: iString, pString
        rc = ESMF_SUCCESS
        write (pString, *) phase
        do i=1, is%wrap%modelCount
          write (iString, *) i
          if (NUOPC_CompAreServicesSet(is%wrap%modelComp(i))) then
            call ESMF_GridCompGet(is%wrap%modelComp(i), name=compName, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
            call ESMF_GridCompInitialize(is%wrap%modelComp(i), &
              importState=is%wrap%modelIS(i), exportState=is%wrap%modelES(i),&
              clock=internalClock, phase=phase, userRc=localrc, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg="NUOPC Incompatible: "//&
              "Failed calling phase "// &
              trim(adjustl(pString))//" Initialize for modelComp "// &
              trim(adjustl(iString))//": "//trim(compName), &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
            if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase "// &
              trim(adjustl(pString))//" Initialize for modelComp "// &
              trim(adjustl(iString))//": "//trim(compName)// &
              " did not return ESMF_SUCCESS", &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
            ! need to update the Component attributes across all PETs
            if (associated(is%wrap%modelPetLists(i)%petList)) then
              call ESMF_AttributeUpdate(is%wrap%modelComp(i), vm, &
                rootList=is%wrap%modelPetLists(i)%petList, rc=rc)
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
        character(ESMF_MAXSTR)  :: iString, jString, pString
        type(ESMF_State)        :: imState, exState
        rc = ESMF_SUCCESS
        write (pString, *) phase
        do i=0, is%wrap%modelCount
          write (iString, *) i
          do j=0, is%wrap%modelCount
            write (jString, *) j
            if (NUOPC_CompAreServicesSet(is%wrap%connectorComp(i,j))) then
              if (i==0) then
                ! connect to the drivers import State
                imState=importState
              else
                imState=is%wrap%modelES(i)
              endif
              if (j==0) then
                ! connect to the drivers export State
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
                trim(adjustl(pString))//" Initialize for connectorComp "// &
                trim(adjustl(iString))//" -> "//trim(adjustl(jString))//": "// &
                trim(compName), &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
              if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase "// &
                trim(adjustl(pString))//" Initialize for connectorComp "// &
                trim(adjustl(iString))//" -> "//trim(adjustl(jString))//": "// &
                trim(compName)//" did not return ESMF_SUCCESS", &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
              ! need to update the Component attributes across all PETs
              if (associated(is%wrap%connectorPetLists(i,j)%petList)) then
                call ESMF_AttributeUpdate(is%wrap%connectorComp(i,j), vm, &
                  rootList=is%wrap%connectorPetLists(i,j)%petList, rc=rc)
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
        call ESMF_AttributeGet(is%wrap%modelComp(i), &
          name=trim(attributeName), &
          itemCount=phaseCount, &
          convention="NUOPC", purpose="General", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) &
          return  ! bail out
        ! allocate pointer variables
        modelPhaseMap(i)%phaseCount = phaseCount
        allocate(modelPhaseMap(i)%phases(phaseCount), &
          modelPhaseMap(i)%phaseValue(phaseCount), &
          modelPhaseMap(i)%phaseKey(phaseCount), &
          stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of temporary data structure.", &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        ! conditionally obtain initPhases list from the Model Attributes
        if (phaseCount > 0) then
          call ESMF_AttributeGet(is%wrap%modelComp(i), &
            name=trim(attributeName), &
            valueList=modelPhaseMap(i)%phases, &
            convention="NUOPC", purpose="General", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) &
            return  ! bail out
        endif
        ! disect the phase string into Key and Value
        do k=1, modelPhaseMap(i)%phaseCount
          tempString = modelPhaseMap(i)%phases(k)
          ind = index(trim(tempString), "=")
          modelPhaseMap(i)%phaseKey(k) = tempString(1:ind-1)
          read (tempString(ind+1:len(tempString)), "(i4)") &
            modelPhaseMap(i)%phaseValue(k)
!print *, "setupModelPhaseMap", k, ":", trim(tempString), " ", &
!  trim(modelPhaseMap(i)%phaseKey(k)), modelPhaseMap(i)%phaseValue(k)
        enddo
      end subroutine

      recursive subroutine setupConnectorPhaseMap(i, j, rc)
        integer, intent(in)     :: i, j
        integer, intent(out)    :: rc
        integer                 :: k, phaseCount, stat, ind
        character(len=NUOPC_PhaseMapStringLength) :: tempString
        rc = ESMF_SUCCESS
        ! obtain number of initPhases from the Model Attributes
        call ESMF_AttributeGet(is%wrap%connectorComp(i,j), &
          name="InitializePhaseMap", &
          itemCount=phaseCount, &
          convention="NUOPC", purpose="General", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) &
          return  ! bail out
        ! allocate pointer variables
        connectorPhaseMap(i,j)%phaseCount = phaseCount
        allocate(connectorPhaseMap(i,j)%phases(phaseCount), &
          connectorPhaseMap(i,j)%phaseValue(phaseCount), &
          connectorPhaseMap(i,j)%phaseKey(phaseCount), &
          stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of temporary data structure.", &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        ! obtain initPhases list from the Model Attributes
        call ESMF_AttributeGet(is%wrap%connectorComp(i,j), &
          name="InitializePhaseMap", &
          valueList=connectorPhaseMap(i,j)%phases, &
          convention="NUOPC", purpose="General", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) &
          return  ! bail out
        ! disect the phase string into Key and Value
        do k=1, connectorPhaseMap(i,j)%phaseCount
          tempString = connectorPhaseMap(i,j)%phases(k)
          ind = index(trim(tempString), "=")
          connectorPhaseMap(i,j)%phaseKey(k) = tempString(1:ind-1)
          read (tempString(ind+1:len(tempString)), "(i4)") &
            connectorPhaseMap(i,j)%phaseValue(k)
!print *, "setupConnectorPhaseMap", k, ":", trim(tempString), " ", &
!  trim(connectorPhaseMap(i,j)%phaseKey(k)), connectorPhaseMap(i,j)%phaseValue(k)
        enddo
      end subroutine

      recursive subroutine loopModelCompsS(phaseString, execFlag, rc)
        ! only to be used for phase>0
        character(*), intent(in):: phaseString
        logical, intent(out), optional :: execFlag ! .true. if at least one executed
        integer, intent(out)    :: rc
        integer                 :: phase, i, k
        character(ESMF_MAXSTR)  :: iString, pString
        ! initialize out arguments
        rc = ESMF_SUCCESS
        if (present(execFlag)) execFlag = .false.
        ! loop through all the model components
        do i=0, is%wrap%modelCount
          write (iString, *) i
          if (NUOPC_CompAreServicesSet(is%wrap%modelComp(i))) then
            ! translate NUOPC logical phase to ESMF actual phase
            phase = 0 ! zero is reserved, use it here to see if need to skip
            do k=1, modelPhaseMap(i)%phaseCount
              if (trim(modelPhaseMap(i)%phaseKey(k)) == trim(phaseString)) &
                phase = modelPhaseMap(i)%phaseValue(k)
            enddo
            if (phase == 0) cycle ! skip to next i
            write (pString, *) phase
            ! attempt to make the actual call to initialize
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
            if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase "// &
              trim(adjustl(pString))//" Initialize for modelComp "// &
              trim(adjustl(iString))//": "//trim(compName)// &
              " did not return ESMF_SUCCESS", &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
            if (present(execFlag)) execFlag = .true. ! at least this model executed for phaseString
          endif
        enddo
      end subroutine

      recursive subroutine loopConnectorCompsS(phaseString, execFlag, rc)
        ! only to be used for phase>0
        character(*), intent(in):: phaseString
        logical, intent(out), optional :: execFlag ! .true. if at least one executed
        integer, intent(out)    :: rc
        integer                 :: phase, i, j, k
        character(ESMF_MAXSTR)  :: iString, jString, pString
        type(ESMF_State)        :: imState, exState
        rc = ESMF_SUCCESS
        ! initialize out arguments
        rc = ESMF_SUCCESS
        if (present(execFlag)) execFlag = .false.
        ! loop through all the model components
        do i=0, is%wrap%modelCount
          write (iString, *) i
          do j=0, is%wrap%modelCount
            write (jString, *) j
            if (NUOPC_CompAreServicesSet(is%wrap%connectorComp(i,j))) then
              ! translate NUOPC logical phase to ESMF actual phase
              phase = 0 ! zero is reserved, use it here to see if need to skip
              do k=1, connectorPhaseMap(i,j)%phaseCount
                if (trim(connectorPhaseMap(i,j)%phaseKey(k)) == trim(phaseString)) &
                  phase = connectorPhaseMap(i,j)%phaseValue(k)
              enddo
              if (phase == 0) cycle ! skip to next j
              write (pString, *) phase
              if (i==0) then
                ! connect to the drivers import State
                imState=importState
              else
                imState=is%wrap%modelES(i)
              endif
              if (j==0) then
                ! connect to the drivers export State
                exState=exportState
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
              if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase "// &
                trim(adjustl(pString))//" Initialize for connectorComp "// &
                trim(adjustl(iString))//" -> "//trim(adjustl(jString))//": "// &
                trim(compName), &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
              if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase "// &
                trim(adjustl(pString))//" Initialize for connectorComp "// &
                trim(adjustl(iString))//" -> "//trim(adjustl(jString))//": "// &
                trim(compName)//" did not return ESMF_SUCCESS", &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
              if (present(execFlag)) execFlag = .true. ! at least this connector executed for phaseString
            endif
          enddo
        enddo
      end subroutine

      recursive subroutine loopDataDependentInitialize(rc)
        ! resolve data dependencies
        integer, intent(out)        :: rc
        integer                     :: phase, i, j, k, cphase
        character(ESMF_MAXSTR)      :: iString, jString, pString, valueString
        character(ESMF_MAXSTR)      :: cpString
        character(len=*), parameter :: phaseString = "IPDv02p5"
        type(ESMF_State)            :: imState, exState
        logical                     :: allComplete, someProgress
        integer                     :: petCount
        integer                     :: helperIn, helperOut
        ! initialize out arguments
        rc = ESMF_SUCCESS
        
        call ESMF_VMGet(vm, petCount=petCount, rc=rc)
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
              do k=1, modelPhaseMap(i)%phaseCount
                if ((trim(modelPhaseMap(i)%phaseKey(k))==trim("IPDv02p5")).or. &
                  (trim(modelPhaseMap(i)%phaseKey(k)) == trim("IPDv03p7")).or. &
                  (trim(modelPhaseMap(i)%phaseKey(k)) == trim("IPDv04p7"))) then
                  phase = modelPhaseMap(i)%phaseValue(k)
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
              call ESMF_AttributeGet(is%wrap%modelComp(i), &
                name="InitializeDataComplete", value=valueString, &
                convention="NUOPC",  purpose="General", rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) &
                return  ! bail out
              
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
                    imState=importState
                  else
                    imState=is%wrap%modelES(j)
                  endif
                  if (i==0) then
                    ! connect to the drivers export State
                    exState=exportState
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
                  if (ESMF_LogFoundError(rcToCheck=localrc,  msg="Phase "// &
                    trim(adjustl(cpString))//" Run for connectorComp "// &
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
              if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase "// &
                trim(adjustl(pString))//" Initialize for modelComp "// &
                trim(adjustl(iString))//": "//trim(compName)// &
                " did not return ESMF_SUCCESS", &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
                
              ! check model InitializeDataProgress Attribute if progress made
              call ESMF_AttributeGet(is%wrap%modelComp(i), &
                name="InitializeDataProgress", value=valueString, &
                convention="NUOPC",  purpose="General", rc=rc)
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
          
          ! check if all Components with IPDv02p5 are InitializeDataComplete
          if (allComplete) exit ! break out of data-dependency resolution loop
          
          if (.not.someProgress) then
            ! dead-lock situation identified
            call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
              msg="Initialize data-dependency resolution loop "// &
              "has entered a dead-lock situation.", &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
            return  ! bail out of data-dependency resolution loop, prevent lock
          endif
          
        enddo
        
      end subroutine
      
  end subroutine
  
  !-----------------------------------------------------------------------------

  recursive subroutine Run(gcomp, importState, exportState, clock, rc)
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
    character(ESMF_MAXSTR)          :: iString, jString, pString
    character(ESMF_MAXSTR)          :: msgString, valueString
    type(NUOPC_RunElement), pointer :: runElement
    type(ESMF_State)                :: imState, exState
    character(ESMF_MAXSTR)          :: name
    logical                         :: verbose
    character(ESMF_MAXSTR)          :: defaultvalue

    rc = ESMF_SUCCESS
    
    ! get the name and currentPhase
    call ESMF_GridCompGet(gcomp, name=name, currentPhase=runPhase, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    
    ! determine verbosity
    defaultvalue = "high"
    call ESMF_AttributeGet(gcomp, name="Verbosity", value=valueString, &
      defaultvalue=defaultvalue, convention="NUOPC", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    if (trim(valueString)=="high") then
      verbose = .true.
    else
      verbose = .false.
    endif
    
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
    if (verbose) then
      call NUOPC_ClockPrintCurrTime(internalClock, ">>>"// &
        trim(name)//" - entered Run with current time: ", msgString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
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
    do while (NUOPC_RunSequenceIterate(is%wrap%runSeq, runSeqIndex, runElement,&
      rc=rc))
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
      
      i = runElement%i
      phase = runElement%phase
      internalClock = runElement%runSeq%clock
      if (runElement%j >= 0) then
        ! connector component
        j = runElement%j
        if (NUOPC_CompAreServicesSet(is%wrap%connectorComp(i,j))) then
          write (iString, *) i
          write (jString, *) j
          write (pString, *) phase
          if (i==0) then
            ! connect to the drivers import State
            imState=importState
          else
            imState=is%wrap%modelES(i)
          endif
          if (j==0) then
            ! connect to the drivers export State
            exState=exportState
          else
            exState=is%wrap%modelIS(j)
          endif
          call ESMF_CplCompRun(is%wrap%connectorComp(i,j), &
            importState=imState, exportState=exState, &
            clock=internalClock, phase=phase, userRc=localrc, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, &
            msg="Failed calling phase "//trim(adjustl(pString))// &
            " Run for connectorComp "//trim(adjustl(iString))// &
            " -> "//trim(adjustl(jString)), &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          if (ESMF_LogFoundError(rcToCheck=localrc,  msg="Phase "// &
            trim(adjustl(pString))//" Run for connectorComp "// &
            trim(adjustl(iString))//" -> "//trim(adjustl(jString))// &
            " did not return ESMF_SUCCESS", &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
        endif
      else
        ! model or mediator component
        if (NUOPC_CompAreServicesSet(is%wrap%modelComp(i))) then
          write (iString, *) i
          write (pString, *) phase
          call ESMF_GridCompRun(is%wrap%modelComp(i), &
            importState=is%wrap%modelIS(i), exportState=is%wrap%modelES(i), &
            clock=internalClock, phase=phase, userRc=localrc, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, &
            msg="Failed calling phase "//trim(adjustl(pString))// &
            " Run for modelComp "//trim(adjustl(iString)), &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase "// &
            trim(adjustl(pString))//" Run for modelComp "// &
            trim(adjustl(iString))//" did not return ESMF_SUCCESS", &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
        endif
      endif

    enddo
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! conditionally output diagnostic to Log file
    if (verbose) then
      call NUOPC_ClockPrintCurrTime(internalClock, "<<<"// &
        trim(name)//" - leaving Run with current time: ", msgString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    
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
    
    if (NUOPC_IsCreated(is%wrap%driverClock)) then
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
    integer                   :: i, j
    character(ESMF_MAXSTR)    :: iString, jString
    logical                   :: existflag
    character(ESMF_MAXSTR):: name

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(gcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! SPECIALIZE by calling into optional attached method
    call ESMF_MethodExecute(gcomp, label=label_Finalize, existflag=existflag, &
      userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

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
    
    ! destroy modelComps and their import and export States + connectorComps
    ! and also petLists that were set by the user (and ownership transferred)
    do i=1, is%wrap%modelCount
      call ESMF_GridCompDestroy(is%wrap%modelComp(i), rc=rc)
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
      do j=1, is%wrap%modelCount
        call ESMF_CplCompDestroy(is%wrap%connectorComp(j,i), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
        if (associated(is%wrap%connectorPetLists(j,i)%petList)) then
          deallocate(is%wrap%connectorPetLists(j,i)%petList, stat=stat)
          if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
            msg="Deallocation of transferred connector petList failed.", &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
        endif
      enddo
    enddo
    
    ! destroy the new style members
    call ESMF_ContainerDestroy(is%wrap%componentMap, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
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
  subroutine NUOPC_DriverAddGridComp(driver, compLabel, &
    compSetServicesRoutine, comp, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                        :: driver
    character(len=*),    intent(in)            :: compLabel
    interface
      subroutine compSetServicesRoutine(gridcomp, rc)
        use ESMF
        implicit none
        type(ESMF_GridComp)        :: gridcomp ! must not be optional
        integer, intent(out)       :: rc       ! must not be optional
      end subroutine
    end interface
    type(ESMF_GridComp), intent(out), optional :: comp
    integer,             intent(out), optional :: rc 
!
! !DESCRIPTION:
! Add a GridComp (i.e. Model, Mediator, or Driver) as a child component to a 
! Driver. The {\tt compLabel} must uniquely identify the child component within 
! the context of the Driver component.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                         :: localrc
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is
    type(ComponentMapEntry)         :: cmEntry
    integer                         :: stat, i

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
    
    ! Entering this call means that the new style members are being used
    is%wrap%newStyleFlag = .true.
    
    ! Add another component to the componentMap with associated compLabel
    allocate(cmEntry%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(stat, msg="allocating cmEntry", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    is%wrap%componentMapCount = is%wrap%componentMapCount + 1
    i = is%wrap%componentMapCount
    cmEntry%wrap%label = trim(compLabel)
    cmEntry%wrap%component = is%wrap%modelComp(i)
    call ESMF_ContainerAddUDT(is%wrap%componentMap, trim(compLabel), &
      cmEntry, rc)
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
      
    ! Set the default name of the component to be the label (may be overwr.)
    call ESMF_GridCompSet(cmEntry%wrap%component, &
      name=trim(cmEntry%wrap%label), rc=rc)
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
  subroutine NUOPC_DriverAddCplComp(driver, srcCompLabel, dstCompLabel, &
    compSetServicesRoutine, comp, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                        :: driver
    character(len=*),    intent(in)            :: srcCompLabel
    character(len=*),    intent(in)            :: dstCompLabel
    interface
      subroutine compSetServicesRoutine(cplcomp, rc)
        use ESMF
        implicit none
        type(ESMF_CplComp)         :: cplcomp  ! must not be optional
        integer, intent(out)       :: rc       ! must not be optional
      end subroutine
    end interface
    type(ESMF_CplComp),  intent(out), optional :: comp
    integer,             intent(out), optional :: rc 
!
! !DESCRIPTION:
! Add a CplComp (i.e. Model, Mediator, or Driver) as a child component to a 
! Driver.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                         :: localrc
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is
    type(ConnectorMapEntry)         :: cmEntry
    integer                         :: stat, src, dst
    type(ESMF_GridComp)             :: srcComp, dstComp

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
    
    ! Entering this call means that the new style members are being used
    is%wrap%newStyleFlag = .true.
    
    ! Figuring out the src/dst index into the connectorComp array.
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
    do src=1, is%wrap%modelCount
      if (is%wrap%modelComp(src)==srcComp) exit ! found the match
    enddo
    do dst=1, is%wrap%modelCount
      if (is%wrap%modelComp(dst)==dstComp) exit ! found the match
    enddo
    
    ! Add another connector to the connectorMap with associated compLabel
    allocate(cmEntry%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(stat, msg="allocating cmEntry", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    cmEntry%wrap%label = trim(srcCompLabel)//"-TO-"//trim(dstCompLabel)
    cmEntry%wrap%connector = is%wrap%connectorComp(src,dst)
    call ESMF_ContainerAddUDT(is%wrap%connectorMap, trim(cmEntry%wrap%label), &
      cmEntry, rc)
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
    
    ! Set the default name of the component to be the label (may be overwr.)
    call ESMF_CplCompSet(cmEntry%wrap%connector, &
      name=trim(cmEntry%wrap%label), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

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
  subroutine NUOPC_DriverAddRunElementM(driver, slot, compLabel, phase, &
    relaxedflag, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                        :: driver
    integer,             intent(in)            :: slot
    character(len=*),    intent(in)            :: compLabel
    integer,             intent(in)            :: phase
    logical,             intent(in),  optional :: relaxedflag
    integer,             intent(out), optional :: rc 
!
! !DESCRIPTION:
! Add a RunElement for a Model, Mediator, or Driver to the RunSequence of the 
! Driver. 
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is
    integer                         :: iComp
    type(ESMF_GridComp)             :: comp
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
    !TODO: fully eliminate the static array modelComp,
    !TODO: removing the need to do this look-up here.
    call NUOPC_DriverGetComp(driver, trim(compLabel), comp, relaxedflag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    do iComp=1, is%wrap%modelCount
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
! !IROUTINE: NUOPC_DriverAddRunElement - Add RunElement for Model, Mediator, or Driver
!
! !INTERFACE:
  ! Private name; call using NUOPC_DriverAddRunElement()
  subroutine NUOPC_DriverAddRunElementMPL(driver, slot, compLabel, &
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
! Add a RunElement for a Model, Mediator, or Driver to the RunSequence of the 
! Driver. If {\tt phaseLabel} was not specified, the first entry in the 
! {\tt RunPhaseMap} attribute will be used to determine the phase.
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
    
    ! Figuring out the index into the modelComp array.
    !TODO: This is a pretty involved look-up, and future implementation will
    !TODO: fully eliminate the static array modelComp,
    !TODO: removing the need to do this look-up here.
    call NUOPC_DriverGetComp(driver, trim(compLabel), comp, relaxedflag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    do iComp=1, is%wrap%modelCount
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
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="run phase: '"//trim(phaseLabel)//"' could not be identified.", &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
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
  subroutine NUOPC_DriverAddRunElementC(driver, slot, srcCompLabel, &
    dstCompLabel, phase, relaxedflag, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                        :: driver
    integer,             intent(in)            :: slot
    character(len=*),    intent(in)            :: srcCompLabel
    character(len=*),    intent(in)            :: dstCompLabel
    integer,             intent(in)            :: phase
    logical,             intent(in),  optional :: relaxedflag
    integer,             intent(out), optional :: rc 
!
! !DESCRIPTION:
! Add a RunElement for a Connector to the RunSequence of the Driver.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is
    integer                         :: src, dst
    type(ESMF_GridComp)             :: srcComp, dstComp
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
    call NUOPC_DriverGetComp(driver, srcCompLabel, srcComp, relaxedflag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call NUOPC_DriverGetComp(driver, dstCompLabel, dstComp, relaxedflag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    do src=1, is%wrap%modelCount
      if (is%wrap%modelComp(src)==srcComp) exit ! found the match
    enddo
    do dst=1, is%wrap%modelCount
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
! !IROUTINE: NUOPC_DriverAddRunElement - Add RunElement for Connector
!
! !INTERFACE:
  ! Private name; call using NUOPC_DriverAddRunElement()
  subroutine NUOPC_DriverAddRunElementCPL(driver, slot, srcCompLabel, &
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
! Add a RunElement for a Connector to the RunSequence of the Driver.
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
    call NUOPC_DriverGetComp(driver, srcCompLabel, srcComp, relaxedflag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call NUOPC_DriverGetComp(driver, dstCompLabel, dstComp, relaxedflag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    do src=1, is%wrap%modelCount
      if (is%wrap%modelComp(src)==srcComp) exit ! found the match
    enddo
    do dst=1, is%wrap%modelCount
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
      relaxedflag, rc=rc)
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
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="run phase: '"//trim(phaseLabel)//"' could not be identified.", &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
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
  subroutine NUOPC_DriverAddRunElementL(driver, slot, linkSlot, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                        :: driver
    integer,             intent(in)            :: slot
    integer,             intent(in)            :: linkSlot
    integer,             intent(out), optional :: rc 
!
! !DESCRIPTION:
! Add a RunElement that links to another RunSequence slot.
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
! !IROUTINE: NUOPC_DriverGetComp - Get a GridComp child from a Driver
!
! !INTERFACE:
  ! Private name; call using NUOPC_DriverGetComp()
  subroutine NUOPC_DriverGetGridComp(driver, compLabel, comp, relaxedflag, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                        :: driver
    character(len=*),    intent(in)            :: compLabel
    type(ESMF_GridComp), intent(out)           :: comp
    logical,             intent(in),  optional :: relaxedflag
    integer,             intent(out), optional :: rc 
!
! !DESCRIPTION:
! Get a GridComp (i.e. Model, Mediator, or Driver) child component from a 
! Driver.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is
    type(ComponentMapEntry)         :: cmEntry
    logical                         :: getFlag

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
    
    ! consider relaxed mode
    getFlag = .true.
    if (present(relaxedflag)) then
      call ESMF_ContainerGet(is%wrap%componentMap, trim(compLabel), &
        isPresent=getFlag, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    
    ! Conditionally access the entry in componentMap
    if (getFlag) then
      call ESMF_ContainerGetUDT(is%wrap%componentMap, trim(compLabel), &
        cmEntry, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      ! return the identified component
      comp = cmEntry%wrap%component
    else
      ! return a nullified component
      comp%compp => null()
    endif
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_DriverGetComp - Get a CplComp child from a Driver
!
! !INTERFACE:
  ! Private name; call using NUOPC_DriverGetComp()
  subroutine NUOPC_DriverGetCplComp(driver, srcCompLabel, dstCompLabel, &
    comp, relaxedflag, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                        :: driver
    character(len=*),    intent(in)            :: srcCompLabel
    character(len=*),    intent(in)            :: dstCompLabel
    type(ESMF_CplComp),  intent(out)           :: comp
    logical,             intent(in),  optional :: relaxedflag
    integer,             intent(out), optional :: rc 
!
! !DESCRIPTION:
! Get a CplComp (i.e. Connector) child component from a Driver.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is
    type(ConnectorMapEntry)         :: cmEntry
    logical                         :: getFlag

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
    
    ! consider relaxed mode
    getFlag = .true.
    if (present(relaxedflag)) then
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
      ! return the identified component
      comp = cmEntry%wrap%connector
    else
      ! return a nullified component
      comp%compp => null()
    endif
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_DriverGetComp - Get all the GridComp child components from a Driver
!
! !INTERFACE:
  ! Private name; call using NUOPC_DriverGetComp()
  subroutine NUOPC_DriverGetAllGridComp(driver, compList, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                        :: driver
    type(ESMF_GridComp), pointer               :: compList(:)
    integer,             intent(out), optional :: rc 
!
! !DESCRIPTION:
! Get all the GridComp (i.e. Model, Mediator, or Driver) child components from a
! Driver. The incoming {\tt compList} argument must be unassociated. On return
! it becomes the responsibility of the caller to deallocate the associated
! {\tt compList} argument.
! 
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
        line=__LINE__, file=trim(name)//":"//FILENAME)
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
    allocate(compList(mapCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of compList failed.", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! fill the compList
    do i=1, mapCount
      call ESMF_ContainerGetUDTByIndex(is%wrap%componentMap, i, &
        cmEntry, ESMF_ITEMORDER_ADDORDER, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      compList(i) = cmEntry%wrap%component
    enddo
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_DriverGetComp - Get all the CplComp child components from a Driver
!
! !INTERFACE:
  ! Private name; call using NUOPC_DriverGetComp()
  subroutine NUOPC_DriverGetAllCplComp(driver, compList, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                        :: driver
    type(ESMF_CplComp),  pointer               :: compList(:)
    integer,             intent(out), optional :: rc 
!
! !DESCRIPTION:
! Get all the CplComp (i.e. Connector) child components from a
! Driver. The incoming {\tt compList} argument must be unassociated. On return
! it becomes the responsibility of the caller to deallocate the associated
! {\tt compList} argument.
! 
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
        line=__LINE__, file=trim(name)//":"//FILENAME)
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
! !IROUTINE: NUOPC_DriverNewRunSequence - Replace current RunSequence with a new one
!
! !INTERFACE:
  subroutine NUOPC_DriverNewRunSequence(driver, slotCount, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                        :: driver
    integer,             intent(in)            :: slotCount
    integer,             intent(out), optional :: rc 
!
! !DESCRIPTION:
! Replace the current RunSequence of the Driver with a new one that has 
! {\tt slotCount} slots. Each slot uses its own Clock for time keeping.
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
  subroutine NUOPC_DriverPrint(driver, orderflag, rc)
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
  subroutine NUOPC_DriverSetRunSequence(driver, slot, clock, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                        :: driver
    integer,             intent(in)            :: slot
    type(ESMF_Clock),    intent(in)            :: clock
    integer,             intent(out), optional :: rc 
!
! !DESCRIPTION:
! Set internals of RunSequence slot.
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

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_DriverSet - Set Driver internals
!
! !INTERFACE:
  subroutine NUOPC_DriverSet(driver, modelCount, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                        :: driver
    integer,             intent(in),  optional :: modelCount
    integer,             intent(out), optional :: rc 
!
! !DESCRIPTION:
! Set the number of model, mediator, and driver components that can be added to
! this driver component.
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
    
    ! optionally set the modelCount
    if (present(modelCount)) then
      is%wrap%modelCount = modelCount
    endif
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_DriverSetModel - Set Driver internals model specific
!
! !INTERFACE:
  subroutine NUOPC_DriverSetModel(driver, compIndex, petList, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                        :: driver
    integer,             intent(in)            :: compIndex
    integer,             intent(in)            :: petList(:)
    integer,             intent(out), optional :: rc 
!
! !DESCRIPTION:
! Set the petList for a specific Model, Mediator, or Driver child component.
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
    
    ! sanity checking of compIndex
    if ((compIndex<1) .or. (compIndex>is%wrap%modelCount)) then
      call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
        msg="compIndex is out of bounds.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
      return  ! bail out
    endif
    
    ! Set up the associated petList member in the internal state and transfer
    allocate(is%wrap%modelPetLists(compIndex)%petList(size(petList)))
    is%wrap%modelPetLists(compIndex)%petList(:) = petList(:)  ! transfer entries
    
  end subroutine
  !-----------------------------------------------------------------------------

end module
