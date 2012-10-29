! $Id: NUOPC_Driver.F90,v 1.12 2012/10/29 22:13:49 theurich Exp $

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
  public label_SetModelServices, label_Finalize
  
  character(*), parameter :: &
    label_InternalState = "Driver_InternalState"
  character(*), parameter :: &
    label_SetModelCount = "Driver_SetModelCount"
  character(*), parameter :: &
    label_SetModelPetLists = "Driver_SetModelPetLists"
  character(*), parameter :: &
    label_SetModelServices = "Driver_SetModelServices"
  character(*), parameter :: &
    label_Finalize = "Driver_Finalize"
    
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
    
    rc = ESMF_SUCCESS
    
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP1, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=Run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_FINALIZE, &
      userRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    
    ! local variables    
    character(len=NUOPC_PhaseMapStringLength) :: initPhases(1)
    
    rc = ESMF_SUCCESS

    initPhases(1) = "IPDv00p1=1"
    
    call ESMF_AttributeSet(gcomp, &
      name="InitializePhaseMap", valueList=initPhases, &
      convention="NUOPC", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    
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
    character(ESMF_MAXSTR)    :: iString, jString, compName, msgString
    character(ESMF_MAXSTR)    :: petListBuffer(100)
    integer, pointer          :: i_petList(:), j_petList(:), c_petList(:)
    logical                   :: existflag
    integer                   :: rootPet, rootVas
    type(ESMF_VM)             :: vm
    type(PhaseMapParser), allocatable ::  modelPhaseMap(:)
    type(PhaseMapParser), allocatable ::  connectorPhaseMap(:,:)

    rc = ESMF_SUCCESS
    
    ! allocate memory for the internal state and set it in the Component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal state memory failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    call ESMF_UserCompSetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    
    ! test whether internal Clock has already been set in the Component
    call ESMF_GridCompGet(gcomp, clockIsPresent=clockIsPresent, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    if (.not.clockIsPresent .and. NUOPC_IsCreated(clock)) then
      ! set the internal Clock as a copy of the incoming Clock by a default
      call NUOPC_GridCompSetClock(gcomp, clock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    endif

    ! SPECIALIZE by calling into attached method to set modelCount
    call ESMF_MethodExecute(gcomp, label=label_SetModelCount, &
      userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    ! allocate lists inside the internal state according to modelCount
    allocate(is%wrap%modelPetLists(is%wrap%modelCount), &
      is%wrap%connectorPetLists(0:is%wrap%modelCount,0:is%wrap%modelCount), &
      is%wrap%modelComp(is%wrap%modelCount), &
      is%wrap%modelIS(is%wrap%modelCount), is%wrap%modelES(is%wrap%modelCount),&
      is%wrap%connectorComp(0:is%wrap%modelCount,0:is%wrap%modelCount), &
      stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal state memory failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      
    ! nullify all of the petLists
    do i=0, is%wrap%modelCount
      if (i>0) &
        nullify(is%wrap%modelPetLists(i)%petList)
      do j=0, is%wrap%modelCount
        nullify(is%wrap%connectorPetLists(i,j)%petList)
      enddo
    enddo
    
    ! allocate PhaseMaps
    allocate(modelPhaseMap(is%wrap%modelCount), &
      connectorPhaseMap(0:is%wrap%modelCount, 0:is%wrap%modelCount), &
      stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of temporary data structure.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    ! SPECIALIZE by calling into optional attached method to set petLists
    call ESMF_MethodExecute(gcomp, label=label_SetModelPetLists, &
      existflag=existflag, userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    ! create modelComps and their import and export States + connectorComps
    do i=0, is%wrap%modelCount
      write (iString, *) i
      
      if (i>0) then
      
        i_petList => is%wrap%modelPetLists(i)%petList
        if (associated(i_petList)) then
          write (msgString, *) "Creating model component #"// &
            trim(adjustl(iString))//" with petList of size ",size(i_petList)," :"
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
          
          if (size(i_petList) <= 1000) then
            ! have the resources to print the entire petList
            write (petListBuffer, "(10I7)") i_petList
            do k=1, size(i_petList)/10 + 1
              call ESMF_LogWrite(petListBuffer(k), ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
            enddo
          endif
            
          is%wrap%modelComp(i) = ESMF_GridCompCreate(name="modelComp "// &
            trim(adjustl(iString)), petList=i_petList, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        else
          write (msgString, *) "Creating model component #"// &
            trim(adjustl(iString))//" without petList."
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
          is%wrap%modelComp(i) = ESMF_GridCompCreate(name="modelComp "// &
            trim(adjustl(iString)), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        endif
        
        is%wrap%modelIS(i) = ESMF_StateCreate(name="modelComp "// &
          trim(adjustl(iString))//" Import State", &
          stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
          
        is%wrap%modelES(i) = ESMF_StateCreate(name="modelComp "// &
          trim(adjustl(iString))//" Export State", &
          stateintent=ESMF_STATEINTENT_EXPORT, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
          
        ! set rootVas Attribute on the States to help during AttributeUpdate
        rootPet = 0   ! initialize
        if (associated(i_petList)) rootPet = i_petList(1)
        ! need to translate rootPet->rootVas because connector petList may
        ! scamble PETs across VASs
        call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        call ESMF_VMGet(vm, rootPet, vas=rootVas, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        call ESMF_AttributeSet(is%wrap%modelIS(i), name="rootVas", &
          value=rootVas, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        call ESMF_AttributeSet(is%wrap%modelES(i), name="rootVas", &
          value=rootVas, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
          
        ! add standard NUOPC GridComp Attribute Package to the modelComp
        call NUOPC_GridCompAttributeAdd(is%wrap%modelComp(i), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
          
        ! initialize the modelPhaseMap pointer members
        nullify(modelPhaseMap(i)%phaseValue)
        nullify(modelPhaseMap(i)%phases)
        nullify(modelPhaseMap(i)%phaseKey)
        
      endif
      
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
              line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
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
                line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
              is%wrap%connectorPetLists(i,j)%petList = c_petList(1:cIndex-1)
              deallocate(c_petList, stat=stat)
              if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
                msg="Deallocation of connector petList failed.", &
                line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
              c_petList => is%wrap%connectorPetLists(i,j)%petList
            else
              ! c_petList is already the right size
              is%wrap%connectorPetLists(i,j)%petList => c_petList
            endif
          endif
        endif

        if (associated(c_petList)) then
          write (msgString, *) "Creating connector component "//&
            trim(adjustl(iString))//" -> "//trim(adjustl(jString))//&
            " with petList of size ",size(c_petList)," :"
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
          
          if (size(c_petList) <= 1000) then
            ! have the resources to print the entire petList
            write (petListBuffer, "(10I7)") c_petList
            do k=1, size(c_petList)/10 + 1
              call ESMF_LogWrite(petListBuffer(k), ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
            enddo
          endif
            
          is%wrap%connectorComp(i,j) = ESMF_CplCompCreate(name="connectorComp "//&
            trim(adjustl(iString))//" -> "//trim(adjustl(jString)), &
            petList=c_petList, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME)) return  ! bail out
        else
          write (msgString, *) "Creating connector component "//&
            trim(adjustl(iString))//" -> "//trim(adjustl(jString))//&
            " without petList."
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
          is%wrap%connectorComp(i,j) = ESMF_CplCompCreate(name="connectorComp "//&
            trim(adjustl(iString))//" -> "//trim(adjustl(jString)), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME)) return  ! bail out
        endif
        
        ! add standard NUOPC CplComp Attribute Package to the connectorComp
        call NUOPC_CplCompAttributeAdd(is%wrap%connectorComp(i,j), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        
        ! initialize the modelPhaseMap pointer members
        nullify(connectorPhaseMap(i,j)%phaseValue)
        nullify(connectorPhaseMap(i,j)%phases)
        nullify(connectorPhaseMap(i,j)%phaseKey)
      enddo
    enddo

    ! initialize the default Run Sequence: grouped connectors before models
    nullify(is%wrap%runSeq) ! initialize
    is%wrap%runPhaseToRunSeqMap = 0 ! initialize
    
    ! add one run sequence element
    call NUOPC_RunSequenceAdd(is%wrap%runSeq, 1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    
    ! map run phase 1 to first run sequence element
    is%wrap%runPhaseToRunSeqMap(1) = 1
    
    ! add run elements to the one run sequence element
    do i=1, is%wrap%modelCount
      do j=1, is%wrap%modelCount
        if (j==i) cycle ! skip self connection
        call NUOPC_RunElementAdd(is%wrap%runSeq(1), i=i, j=j, phase=1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
      enddo
    enddo
    do i=1, is%wrap%modelCount
      call NUOPC_RunElementAdd(is%wrap%runSeq(1), i=i, j=0, phase=1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    enddo
    
    ! SPECIALIZE by calling into attached method to SetServices for modelComps
    call ESMF_MethodExecute(gcomp, label=label_SetModelServices, &
      existflag=existflag, userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      
    ! query Component for its Clock (set during specialization)
    call ESMF_GridCompGet(gcomp, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    ! -> NUOPC Initialize Sequence requires presence of InitP0 for every 
    ! -> Model and Connector component, where they must set the
    ! -> "InitializePhaseMap" metadata.
      
    ! InitP0: modelComps
    call loopModelComps(phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    ! InitP0: connectorComps
    call loopConnectorComps(phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out


#if 1
    ! -> Now encode the NUOPC Initialize Sequence version 00 and 01:
      
    ! modelComps
    call loopModelCompsS(phaseString="IPDv00p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call loopModelCompsS(phaseString="IPDv01p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    ! connectorComps
    call loopConnectorCompsS(phaseString="IPDv00p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(phaseString="IPDv01p1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    ! modelComps
    call loopModelCompsS(phaseString="IPDv01p2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    ! connectorComps
    call loopConnectorCompsS(phaseString="IPDv01p2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    ! modelComps
    call loopModelCompsS(phaseString="IPDv00p2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call loopModelCompsS(phaseString="IPDv01p3", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    ! connectorComps
    call loopConnectorCompsS(phaseString="IPDv00p2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call loopConnectorCompsS(phaseString="IPDv01p3", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    ! modelComps
    call loopModelCompsS(phaseString="IPDv00p3", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call loopModelCompsS(phaseString="IPDv01p4", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    ! modelComps
    call loopModelCompsS(phaseString="IPDv00p4", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call loopModelCompsS(phaseString="IPDv01p5", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
#else

    ! InitP1: modelComps
    call loopModelComps(phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    ! InitP1: connectorComps
    call loopConnectorComps(phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    ! InitP2: modelComps
    call loopModelComps(phase=2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    ! InitP2: connectorComps
    call loopConnectorComps(phase=2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    ! InitP3: modelComps
    call loopModelComps(phase=3, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    ! InitP4: modelComps
    call loopModelComps(phase=4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

      
#endif

#define DEBUGPRINT____disable
#ifdef DEBUGPRINT
    ! print the entire runSeq structure
    call NUOPC_RunSequencePrint(is%wrap%runSeq, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
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
      if (i>0) then
        if (associated(modelPhaseMap(i)%phaseValue)) &
          deallocate(modelPhaseMap(i)%phaseValue)
        if (associated(modelPhaseMap(i)%phases)) &
          deallocate(modelPhaseMap(i)%phases)
        if (associated(modelPhaseMap(i)%phaseKey)) &
          deallocate(modelPhaseMap(i)%phaseKey)
      endif
    enddo

    contains !----------------------------------------------------------------
    
      recursive subroutine loopModelComps(phase, rc)
        ! only to be used for phase=0 anymore!!
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
              line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
            call ESMF_GridCompInitialize(is%wrap%modelComp(i), &
              importState=is%wrap%modelIS(i), exportState=is%wrap%modelES(i), &
              clock=internalClock, phase=phase, userRc=localrc, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg="NUOPC Incompatible: Failed calling phase "// &
              trim(adjustl(pString))//" Initialize for modelComp "// &
              trim(adjustl(iString))//": "//trim(compName), &
              line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
            if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase "// &
              trim(adjustl(pString))//" Initialize for modelComp "// &
              trim(adjustl(iString))//": "//trim(compName)// &
              " did not return ESMF_SUCCESS", &
              line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
            if (phase == 0) then
              ! setup modelPhaseMap
              call setupModelPhaseMap(i=i, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
            endif
          endif
        enddo
      end subroutine

      recursive subroutine loopConnectorComps(phase, rc)
        ! only to be used for phase=0 anymore!!
        integer, intent(in)     :: phase
        integer, intent(out)    :: rc
        integer                 :: i, j
        character(ESMF_MAXSTR)  :: iString, jString, pString
        rc = ESMF_SUCCESS
        write (pString, *) phase
        do i=0, is%wrap%modelCount
          write (iString, *) i
          do j=0, is%wrap%modelCount
            write (jString, *) j
            if (NUOPC_CplCompAreServicesSet(is%wrap%connectorComp(i,j))) then
              call ESMF_CplCompGet(is%wrap%connectorComp(i,j), name=compName, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
              call ESMF_CplCompInitialize(is%wrap%connectorComp(i,j), &
                importState=is%wrap%modelES(i), exportState=is%wrap%modelIS(j), &
                clock=internalClock, phase=phase, userRc=localrc, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase "// &
                trim(adjustl(pString))//" Initialize for connectorComp "// &
                trim(adjustl(iString))//" -> "//trim(adjustl(jString))//": "// &
                trim(compName), &
                line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
              if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase "// &
                trim(adjustl(pString))//" Initialize for connectorComp "// &
                trim(adjustl(iString))//" -> "//trim(adjustl(jString))//": "// &
                trim(compName)//" did not return ESMF_SUCCESS", &
                line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
              if (phase == 0) then
                ! setup modelPhaseMap
                call setupConnectorPhaseMap(i=i, j=j, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
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
        rc = ESMF_SUCCESS
        ! obtain number of initPhases from the Model Attributes
        call ESMF_AttributeGet(is%wrap%modelComp(i), &
          name="InitializePhaseMap", &
          itemCount=phaseCount, &
          convention="NUOPC", purpose="General", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
        ! allocate pointer variables
        modelPhaseMap(i)%phaseCount = phaseCount
        allocate(modelPhaseMap(i)%phases(phaseCount), &
          modelPhaseMap(i)%phaseValue(phaseCount), &
          modelPhaseMap(i)%phaseKey(phaseCount), &
          stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of temporary data structure.", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        ! obtain initPhases list from the Model Attributes
        call ESMF_AttributeGet(is%wrap%modelComp(i), &
          name="InitializePhaseMap", &
          valueList=modelPhaseMap(i)%phases, &
          convention="NUOPC", purpose="General", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
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
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
        ! allocate pointer variables
        connectorPhaseMap(i,j)%phaseCount = phaseCount
        allocate(connectorPhaseMap(i,j)%phases(phaseCount), &
          connectorPhaseMap(i,j)%phaseValue(phaseCount), &
          connectorPhaseMap(i,j)%phaseKey(phaseCount), &
          stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of temporary data structure.", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        ! obtain initPhases list from the Model Attributes
        call ESMF_AttributeGet(is%wrap%connectorComp(i,j), &
          name="InitializePhaseMap", &
          valueList=connectorPhaseMap(i,j)%phases, &
          convention="NUOPC", purpose="General", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
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

      recursive subroutine loopModelCompsS(phaseString, rc)
        ! only to be used for phase=0 anymore!!
        character(*), intent(in):: phaseString
        integer, intent(out)    :: rc
        integer                 :: phase, i
        character(ESMF_MAXSTR)  :: iString, pString
        rc = ESMF_SUCCESS
        do i=1, is%wrap%modelCount
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
              line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
            call ESMF_GridCompInitialize(is%wrap%modelComp(i), &
              importState=is%wrap%modelIS(i), exportState=is%wrap%modelES(i), &
              clock=internalClock, phase=phase, userRc=localrc, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase "// &
              trim(adjustl(pString))//" Initialize for modelComp "// &
              trim(adjustl(iString))//": "//trim(compName), &
              line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
            if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase "// &
              trim(adjustl(pString))//" Initialize for modelComp "// &
              trim(adjustl(iString))//": "//trim(compName)// &
              " did not return ESMF_SUCCESS", &
              line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
            if (phase == 0) then
              ! setup modelPhaseMap
              call setupModelPhaseMap(i=i, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
            endif
          endif
        enddo
      end subroutine

      recursive subroutine loopConnectorCompsS(phaseString, rc)
        ! only to be used for phase=0 anymore!!
        character(*), intent(in):: phaseString
        integer, intent(out)    :: rc
        integer                 :: phase, i, j
        character(ESMF_MAXSTR)  :: iString, jString, pString
        rc = ESMF_SUCCESS
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
              ! attempt to make the actual call to initialize
              call ESMF_CplCompGet(is%wrap%connectorComp(i,j), name=compName, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
              call ESMF_CplCompInitialize(is%wrap%connectorComp(i,j), &
                importState=is%wrap%modelES(i), exportState=is%wrap%modelIS(j), &
                clock=internalClock, phase=phase, userRc=localrc, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase "// &
                trim(adjustl(pString))//" Initialize for connectorComp "// &
                trim(adjustl(iString))//" -> "//trim(adjustl(jString))//": "// &
                trim(compName), &
                line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
              if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase "// &
                trim(adjustl(pString))//" Initialize for connectorComp "// &
                trim(adjustl(iString))//" -> "//trim(adjustl(jString))//": "// &
                trim(compName)//" did not return ESMF_SUCCESS", &
                line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
              if (phase == 0) then
                ! setup modelPhaseMap
                call setupConnectorPhaseMap(i=i, j=j, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
              endif
            endif
          enddo
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
    integer                         :: i, j, phase, runPhase, runSeqIndex
    character(ESMF_MAXSTR)          :: iString, jString, pString, msgString
    type(NUOPC_RunElement), pointer :: runElement
    character(ESMF_MAXSTR)          :: modelName

    rc = ESMF_SUCCESS
    
    call ESMF_GridCompGet(gcomp, name=modelName, currentPhase=runPhase, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    ! query Component for its Clock
    call ESMF_GridCompGet(gcomp, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    
    ! query Component for this internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    call NUOPC_ClockPrintCurrTime(internalClock, ">>>"// &
      trim(modelName)//" entered Run with current time: ", msgString, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    ! determine the correct run sequence index for the current runPhase    
    runSeqIndex = is%wrap%runPhaseToRunSeqMap(runPhase)
    
    ! alias the component's internalClock in this runPhase run sequence
    call NUOPC_RunSequenceSet(is%wrap%runSeq(runSeqIndex), internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    
    ! use RunSequence iterator to execute the actual time stepping loop
    nullify(runElement) ! prepare runElement for iterator use
    do while (NUOPC_RunSequenceIterate(is%wrap%runSeq, runSeqIndex, runElement,&
      rc=rc))
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      
      ! now interpret and act on the current runElement
#ifdef DEBUGPRINT
      print *, runElement%i, runElement%j, runElement%phase
      call NUOPC_ClockPrintCurrTime(runElement%runSeq%clock, &
        "NUOPC_Driver.Run() RunSequence iterator loop at current time: ", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
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
          call ESMF_CplCompRun(is%wrap%connectorComp(i,j), &
            importState=is%wrap%modelES(i), exportState=is%wrap%modelIS(j), &
            clock=internalClock, phase=phase, userRc=localrc, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, &
            msg="Failed calling phase "//trim(adjustl(pString))// &
            " Run for connectorComp "//trim(adjustl(iString))// &
            " -> "//trim(adjustl(jString)), &
            line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
          if (ESMF_LogFoundError(rcToCheck=localrc,  msg="Phase "// &
            trim(adjustl(pString))//" Run for connectorComp "// &
            trim(adjustl(iString))//" -> "//trim(adjustl(jString))// &
            " did not return ESMF_SUCCESS", &
            line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
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
            line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
          if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase "// &
            trim(adjustl(pString))//" Run for modelComp "// &
            trim(adjustl(iString))//" did not return ESMF_SUCCESS", &
            line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        endif
      endif

    enddo
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    
    call NUOPC_ClockPrintCurrTime(internalClock, "<<<"// &
      trim(modelName)//" leaving Run with current time: ", msgString, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    
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

    rc = ESMF_SUCCESS
    
    ! SPECIALIZE by calling into optional attached method
    call ESMF_MethodExecute(gcomp, label=label_Finalize, existflag=existflag, &
      userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    ! query Component for its Clock
    call ESMF_GridCompGet(gcomp, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    
    ! query Component for this internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      
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
            line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
          if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase 1 "// &
            "Finalize for connectorComp "// &
            trim(adjustl(iString))//" -> "//trim(adjustl(jString))//" did not "// &
            "return ESMF_SUCCESS", &
            line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
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
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase 1 "// &
          "Finalize for modelComp "//trim(adjustl(iString))//" did not "// &
          "return ESMF_SUCCESS", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      endif
    enddo
    
    ! destroy modelComps and their import and export States + connectorComps
    ! and also petLists that were set by the user (and ownership transferred)
    do i=1, is%wrap%modelCount
      call ESMF_GridCompDestroy(is%wrap%modelComp(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      call ESMF_StateDestroy(is%wrap%modelIS(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      call ESMF_StateDestroy(is%wrap%modelES(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      if (associated(is%wrap%modelPetLists(i)%petList)) then
        deallocate(is%wrap%modelPetLists(i)%petList, stat=stat)
        if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
          msg="Deallocation of transferred model petList failed.", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      endif
      do j=1, is%wrap%modelCount
        call ESMF_CplCompDestroy(is%wrap%connectorComp(j,i), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        if (associated(is%wrap%connectorPetLists(j,i)%petList)) then
          deallocate(is%wrap%connectorPetLists(j,i)%petList, stat=stat)
          if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
            msg="Deallocation of transferred connector petList failed.", &
            line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        endif
      enddo
    enddo
    
    ! deallocate lists inside the internal state
    deallocate(is%wrap%modelPetLists, is%wrap%connectorPetLists, &
      is%wrap%modelComp, is%wrap%modelIS, is%wrap%modelES, &
      is%wrap%connectorComp, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      
    ! deallocate run sequence data structures
    call NUOPC_RunSequenceDeallocate(is%wrap%runSeq, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! deallocate internal state memory
    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      
  end subroutine
  
end module
