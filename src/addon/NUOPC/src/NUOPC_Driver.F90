! $Id$

#define FILENAME "src/addon/NUOPC/NUOPC_Driver.F90"

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
  public label_SetModelServices, label_Finalize
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
    type(ESMF_GridComp), pointer      :: modelComp(:)
    type(ESMF_State),    pointer      :: modelIS(:), modelES(:)
    type(ESMF_CplComp),  pointer      :: connectorComp(:,:)
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
    
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP1, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=Run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
      
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_FINALIZE, &
      userRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
      
    call ESMF_MethodAdd(gcomp, label=label_SetRunClock, &
      userRoutine=SetRunClock, rc=rc)
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
    character(len=NUOPC_PhaseMapStringLength) :: initPhases(1)
    character(ESMF_MAXSTR):: name

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(gcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    initPhases(1) = "IPDv00p1=1"
    
    call ESMF_AttributeSet(gcomp, &
      name="InitializePhaseMap", valueList=initPhases, &
      convention="NUOPC", purpose="General", rc=rc)
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
    logical                   :: execFlag

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
      call NUOPC_GridCompSetClock(gcomp, clock, rc=rc)
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
        call NUOPC_GridCompAttributeAdd(is%wrap%modelComp(i), rc=rc)
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
        call NUOPC_CplCompAttributeAdd(is%wrap%connectorComp(i,j), rc=rc)
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

    ! Ingest the InitializePhaseMap
    do i=0, is%wrap%modelCount
      if (NUOPC_GridCompAreServicesSet(is%wrap%modelComp(i))) then
        ! setup modelPhaseMap
        call setupModelPhaseMap(i=i, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
      do j=0, is%wrap%modelCount
        if (NUOPC_CplCompAreServicesSet(is%wrap%connectorComp(i,j))) then
          ! setup modelPhaseMap
          call setupConnectorPhaseMap(i=i, j=j, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
        endif
      enddo
    enddo

    ! -> Now encode the NUOPC IPDv00, IPDv01, IPDv02:
      
    ! modelComps
    call loopModelCompsS(phaseString="IPDv00p1", execFlag=execFlag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(phaseString="IPDv01p1", execFlag=execFlag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(phaseString="IPDv02p1", execFlag=execFlag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! connectorComps
    call loopConnectorCompsS(phaseString="IPDv00p1", execFlag=execFlag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(phaseString="IPDv01p1", execFlag=execFlag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(phaseString="IPDv02p1", execFlag=execFlag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! modelComps
    call loopModelCompsS(phaseString="IPDv01p2", execFlag=execFlag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(phaseString="IPDv02p2", execFlag=execFlag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! connectorComps
    call loopConnectorCompsS(phaseString="IPDv01p2", execFlag=execFlag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(phaseString="IPDv02p2", execFlag=execFlag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! modelComps
    call loopModelCompsS(phaseString="IPDv00p2", execFlag=execFlag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(phaseString="IPDv01p3", execFlag=execFlag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(phaseString="IPDv02p3", execFlag=execFlag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! connectorComps
    call loopConnectorCompsS(phaseString="IPDv00p2", execFlag=execFlag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(phaseString="IPDv01p3", execFlag=execFlag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(phaseString="IPDv02p3", execFlag=execFlag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! modelComps
    call loopModelCompsS(phaseString="IPDv00p3", execFlag=execFlag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(phaseString="IPDv01p4", execFlag=execFlag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(phaseString="IPDv02p4", execFlag=execFlag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! modelComps
    call loopModelCompsS(phaseString="IPDv00p4", execFlag=execFlag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(phaseString="IPDv01p5", execFlag=execFlag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call loopModelCompsS(phaseString="IPDv02p5", execFlag=execFlag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    if (execFlag) then
      ! there were model components with IPDv02p5 -> resolve data dependencies
      call loopDataDependentInitialize(vm, rc=rc)
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
          if (NUOPC_GridCompAreServicesSet(is%wrap%modelComp(i))) then
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
            if (NUOPC_CplCompAreServicesSet(is%wrap%connectorComp(i,j))) then
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
          ind = index (trim(tempString), "=")
          modelPhaseMap(i)%phaseKey(k) = tempString(1:ind-1)
          read (tempString(ind+1:ind+2), "(i1)") modelPhaseMap(i)%phaseValue(k)
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
          ind = index (trim(tempString), "=")
          connectorPhaseMap(i,j)%phaseKey(k) = tempString(1:ind-1)
          read (tempString(ind+1:ind+2), "(i1)") connectorPhaseMap(i,j)%phaseValue(k)
!print *, "setupConnectorPhaseMap", k, ":", trim(tempString), " ", &
!  trim(connectorPhaseMap(i,j)%phaseKey(k)), connectorPhaseMap(i,j)%phaseValue(k)
        enddo
      end subroutine

      recursive subroutine loopModelCompsS(phaseString, execFlag, rc)
        ! only to be used for phase>0
        character(*), intent(in):: phaseString
        logical, intent(out)    :: execFlag ! .true. if at least one executed
        integer, intent(out)    :: rc
        integer                 :: phase, i, k
        character(ESMF_MAXSTR)  :: iString, pString
        ! initialize out arguments
        rc = ESMF_SUCCESS
        execFlag = .false.
        ! loop through all the model components
        do i=0, is%wrap%modelCount
          write (iString, *) i
          if (NUOPC_GridCompAreServicesSet(is%wrap%modelComp(i))) then
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
            execFlag = .true. ! at least this model executed for phaseString
          endif
        enddo
      end subroutine

      recursive subroutine loopConnectorCompsS(phaseString, execFlag, rc)
        ! only to be used for phase>0
        character(*), intent(in):: phaseString
        logical, intent(out)    :: execFlag ! .true. if at least one executed
        integer, intent(out)    :: rc
        integer                 :: phase, i, j, k
        character(ESMF_MAXSTR)  :: iString, jString, pString
        type(ESMF_State)        :: imState, exState
        rc = ESMF_SUCCESS
        ! initialize out arguments
        rc = ESMF_SUCCESS
        execFlag = .false.
        ! loop through all the model components
        do i=0, is%wrap%modelCount
          write (iString, *) i
          do j=0, is%wrap%modelCount
            write (jString, *) j
            if (NUOPC_CplCompAreServicesSet(is%wrap%connectorComp(i,j))) then
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
              execFlag = .true. ! at least this connector executed for phaseString
            endif
          enddo
        enddo
      end subroutine

      recursive subroutine loopDataDependentInitialize(vm, rc)
        ! resolve data dependencies
        type(ESMF_VM)               :: vm
        integer, intent(out)        :: rc
        integer                     :: phaseLocal, phaseGlobal, i, j, k, n=0, cphase
        character(ESMF_MAXSTR)      :: iString, jString, pString, valueString
        character(ESMF_MAXSTR)      :: cpString, nString
        character(len=*), parameter :: phaseString = "IPDv02p5"
        type(ESMF_State)            :: imState, exState
        logical                     :: allComplete, someProgress
        integer (ESMF_KIND_I4)      :: iCompleteLocal, iCompleteGlobal
        integer (ESMF_KIND_I4)      :: iProgressLocal, iProgressGlobal
        logical                     :: modServSet, conServSet
!character(ESMF_MAXSTR):: msgString, pfxString, iName, jName                              !DEBUG
!pfxString = 'DEBUG: '//trim(name)//': DataDependentInitialize('                          !DEBUG
        ! initialize out arguments
        rc = ESMF_SUCCESS

        ! data-dependency resolution loop -> TODO: prevent endless condition!!!
        ddr_loop: do

          n = n+1
          write (nString, *) n
          allComplete = .true.    ! prime -> one that isn't complete can toggle
          someProgress = .false.  ! prime -> one that made progress can toggle

          ! loop through all the model components
          i_loop: do i=0, is%wrap%modelCount
            write (iString, *) i
            modServSet = NUOPC_GridCompAreServicesSet(is%wrap%modelComp(i))

!call ESMF_GridCompGet(is%wrap%modelComp(i), name=iName, rc=rc)                           !DEBUG
!if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &                         !DEBUG
!  line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &                       !DEBUG
!  return  ! bail out                                                                     !DEBUG
!write(msgString,'(a,l3)') trim(adjustl(pfxString))//trim(adjustl(nString))//'): ' &      !DEBUG
!//trim(adjustl(iName))//': modServSet: ',modServSet                                      !DEBUG
!call ESMF_LogWrite(trim(adjustl(msgString)), ESMF_LOGMSG_INFO)                           !DEBUG
!call ESMF_LogFlush()                                                                     !DEBUG

            if (modServSet) then

              ! translate NUOPC logical phase to ESMF actual phase
              phaseLocal = 0 ! zero is reserved, use it here to see if need to skip
              do k=1, modelPhaseMap(i)%phaseCount
                if (trim(modelPhaseMap(i)%phaseKey(k)) == trim(phaseString)) &
                  phaseLocal = modelPhaseMap(i)%phaseValue(k)
              enddo
              write (pString, *) phaseLocal
              call ESMF_VMAllFullReduce(vm, (/phaseLocal/), phaseGlobal, 1, &
                ESMF_REDUCE_SUM, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) &
                return  ! bail out

!write(msgString,'(a,i3)') trim(adjustl(pfxString))//trim(adjustl(nString))//'): ' &      !DEBUG
!//trim(adjustl(iName))//': phaseCount: ',modelPhaseMap(i)%phaseCount                     !DEBUG
!call ESMF_LogWrite(trim(adjustl(msgString)), ESMF_LOGMSG_INFO)                           !DEBUG
!call ESMF_LogFlush()                                                                     !DEBUG
!do k=1, modelPhaseMap(i)%phaseCount                                                      !DEBUG
!write(msgString,'(a,i3)') trim(adjustl(pfxString))//trim(adjustl(nString))//'): ' &      !DEBUG
!//trim(adjustl(iName))//': phaseKey/Value: ' &                                           !DEBUG
!//trim(modelPhaseMap(i)%phaseKey(k)),modelPhaseMap(i)%phaseValue(k)                      !DEBUG
!call ESMF_LogWrite(trim(adjustl(msgString)), ESMF_LOGMSG_INFO)                           !DEBUG
!call ESMF_LogFlush()                                                                     !DEBUG
!enddo                                                                                    !DEBUG

              ! if IPDv02p5, then check model InitializeDataComplete Attribute
              ! to see if complete
              iCompleteLocal = 0
              if (phaseLocal /= 0) then
                call ESMF_AttributeGet(is%wrap%modelComp(i), &
                  name="InitializeDataComplete", value=valueString, &
                  convention="NUOPC",  purpose="General", rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//FILENAME)) &
                  return  ! bail out
                if (trim(valueString)=="true") iCompleteLocal = 1
              endif
              call ESMF_VMAllFullReduce(vm, (/iCompleteLocal/), iCompleteGlobal, 1, &
                ESMF_REDUCE_SUM, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) &
                return  ! bail out

!write(msgString,'(a,2i4)') trim(adjustl(pfxString))//trim(adjustl(nString))//'): ' &     !DEBUG
!//trim(adjustl(iName))//': IntializeDataComplete: ' &                                    !DEBUG
!//trim(adjustl(pString))//': ',iCompleteLocal,iCompleteGlobal                            !DEBUG
!call ESMF_LogWrite(trim(adjustl(msgString)), ESMF_LOGMSG_INFO)                           !DEBUG
!call ESMF_LogFlush()                                                                     !DEBUG

              if (phaseGlobal == 0 .or. iCompleteGlobal /= 0) cycle i_loop ! skip to next i
              allComplete = .false. ! hit toggles -> prevents exit on outer loop

              ! else try to Run() all of the Connectors to model i
              j_loop: do j=0, is%wrap%modelCount

                write (jString, *) j
                cphase = 1  ! for now assume Run() only does phase 1
                write (cpString, *) cphase
                conServSet = NUOPC_CplCompAreServicesSet(is%wrap%connectorComp(j,i))

!call ESMF_GridCompGet(is%wrap%modelComp(j), name=jName, rc=rc)                           !DEBUG
!if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &                         !DEBUG
!  line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &                       !DEBUG
!  return  ! bail out                                                                     !DEBUG
!write(msgString,'(a,l3)') trim(adjustl(pfxString))//trim(adjustl(nString))//'): ' &      !DEBUG
!//trim(adjustl(iName))//':    conServSet: ' &                                            !DEBUG
!//trim(adjustl(jName))//' -> '//trim(adjustl(iName))//': ',conServSet                    !DEBUG
!call ESMF_LogWrite(trim(adjustl(msgString)), ESMF_LOGMSG_INFO)                           !DEBUG
!call ESMF_LogFlush()                                                                     !DEBUG

                if (conServSet) then

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

!write(msgString,'(a,l3)') trim(adjustl(pfxString))//trim(adjustl(nString))//'): ' &      !DEBUG
!//trim(adjustl(iName))//': connectorComp: ' &                                            !DEBUG
!//trim(adjustl(jName))//' -> '//trim(adjustl(iName))                                     !DEBUG
!call ESMF_LogWrite(trim(adjustl(msgString)), ESMF_LOGMSG_INFO)                           !DEBUG
!call ESMF_LogFlush()                                                                     !DEBUG

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

                endif !conServSet

              enddo j_loop

              ! attempt to make the actual call to initialize for model i
              if (phaseLocal /= 0) then
!write(msgString,'(a)') trim(adjustl(pfxString))//trim(adjustl(nString))//'): ' &         !DEBUG
!//trim(adjustl(iName))//': '//': GridCompInitialize: '//trim(adjustl(pString))           !DEBUG
!call ESMF_LogWrite(trim(adjustl(msgString)), ESMF_LOGMSG_INFO)                           !DEBUG
!call ESMF_LogFlush()                                                                     !DEBUG
                call ESMF_GridCompGet(is%wrap%modelComp(i), name=compName, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                  return  ! bail out
                call ESMF_GridCompInitialize(is%wrap%modelComp(i), &
                  importState=is%wrap%modelIS(i), exportState=is%wrap%modelES(i), &
                  clock=internalClock, phase=phaseLocal, userRc=localrc, rc=rc)
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
              endif
                
              ! check model InitializeDataProgress Attribute if progress made
              iProgressLocal = 0
              if (phaseLocal /= 0) then
                call ESMF_AttributeGet(is%wrap%modelComp(i), &
                  name="InitializeDataProgress", value=valueString, &
                  convention="NUOPC",  purpose="General", rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//FILENAME)) &
                  return  ! bail out
                if (trim(valueString)=="true") iProgressLocal = 1
              endif
              call ESMF_VMAllFullReduce(vm, (/iProgressLocal/), iProgressGlobal, 1, &
                ESMF_REDUCE_SUM, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) &
                return  ! bail out

!write(msgString,'(a,2i4)') trim(adjustl(pfxString))//trim(adjustl(nString))//'): ' &     !DEBUG
!//trim(adjustl(iName))//': '//': IntializeDataProgress: ' &                              !DEBUG
!//trim(adjustl(pString))//': ',iProgressLocal,iProgressGlobal                            !DEBUG
!call ESMF_LogWrite(trim(adjustl(msgString)), ESMF_LOGMSG_INFO)                           !DEBUG
!call ESMF_LogFlush()                                                                     !DEBUG

              if (iProgressGlobal /= 0) someProgress = .true. ! toggle progress flag

            endif !modServSet

          enddo i_loop

!write(msgString,'(a,l3)') trim(adjustl(pfxString))//trim(adjustl(nString))//'): ' &      !DEBUG
!//'allComplete: ',allComplete                                                            !DEBUG
!call ESMF_LogWrite(trim(adjustl(msgString)), ESMF_LOGMSG_INFO)                           !DEBUG
!call ESMF_LogFlush()                                                                     !DEBUG
!write(msgString,'(a,l3)') trim(adjustl(pfxString))//trim(adjustl(nString))//'): ' &      !DEBUG
!//'someProgress: ',someProgress                                                          !DEBUG
!call ESMF_LogWrite(trim(adjustl(msgString)), ESMF_LOGMSG_INFO)                           !DEBUG
!call ESMF_LogFlush()                                                                     !DEBUG

          ! check if all Components with IPDv02p5 are InitializeDataComplete
          if (allComplete) exit ! break out of data-dependency resolution loop

          if (.not.someProgress) then
            ! dead-lock situation identified
            call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
              msg="Initialize data-dependency resolution loop "// &
              "has entered a dead-lock situation.", &
              line=__LINE__, file=FILENAME, rcToReturn=rc)
            return  ! bail out of data-dependency resolution loop, prevent lock
          endif

        enddo ddr_loop

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
    
    ! query Component for this internal State
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
        if (NUOPC_CplCompAreServicesSet(is%wrap%connectorComp(i,j))) then
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
        if (NUOPC_GridCompAreServicesSet(is%wrap%modelComp(i))) then
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
      call NUOPC_GridCompCheckSetClock(gcomp, is%wrap%driverClock, rc=rc)
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
    
    ! query Component for this internal State
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
        if (NUOPC_CplCompAreServicesSet(is%wrap%connectorComp(i,j))) then
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
      if (NUOPC_GridCompAreServicesSet(is%wrap%modelComp(i))) then
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
  
end module
