! $Id: NUOPC_DriverExplicit.F90,v 1.14 2011/08/19 18:21:07 theurich Exp $

#define FILENAME "src/addon/NUOPC/NUOPC_DriverExplicit.F90"

module NUOPC_DriverExplicit

  !-----------------------------------------------------------------------------
  ! Generic Driver Component with explicit time stepping
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC

  implicit none
  
  private
  
  public routine_SetServices
  public routine_AddRunElement, routine_DeallocateRunSequence
  public type_InternalState, type_InternalStateStruct, type_RunElement
  public label_InternalState
  public label_SetModelCount, label_SetModelServices, label_Finalize
  
  character(*), parameter :: &
    label_InternalState = "DriverExplicit_InternalState"
  character(*), parameter :: &
    label_SetModelCount = "DriverExplicit_SetModelCount"
  character(*), parameter :: &
    label_SetModelServices = "DriverExplicit_SetModelServices"
  character(*), parameter :: &
    label_Finalize = "DriverExplicit_Finalize"
    
  type type_InternalStateStruct
    integer                         :: modelCount
    type(ESMF_GridComp), pointer    :: modelComp(:)
    type(ESMF_State),    pointer    :: modelIS(:), modelES(:)
    type(ESMF_CplComp),  pointer    :: connectorComp(:,:)
    type(type_RunElement), pointer  :: runSequence
  end type

  type type_InternalState
    type(type_InternalStateStruct), pointer :: wrap
  end type
  
  type type_RunElement
    integer :: i ! model component index, or src model index if connector
    integer :: j ! if 0 then model component, if > 0 then connector for i->j
    integer :: phase  ! run phase
    type(type_RunElement), pointer :: next ! next RunElement in linked list
  end type
  
  interface routine_AddRunElement
    module procedure AddRunElementIJP
    module procedure AddRunElementType
  end interface

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine routine_SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=Initialize, rc=rc)
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

  subroutine Initialize(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    integer                   :: localrc, stat
    type(type_InternalState)  :: is
    logical                   :: clockIsPresent
    type(ESMF_Clock)          :: internalClock
    integer                   :: i, j
    character(ESMF_MAXSTR)    :: iString, jString, compName
    type(type_RunElement), pointer  :: runElement

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
    allocate(is%wrap%modelComp(is%wrap%modelCount), &
      is%wrap%modelIS(is%wrap%modelCount), is%wrap%modelES(is%wrap%modelCount),&
      is%wrap%connectorComp(is%wrap%modelCount,is%wrap%modelCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal state memory failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    ! create modelComps and their import and export States + connectorComps
    do i=1, is%wrap%modelCount
      !TODO: there should be petList members in the internal State that 
      !TODO: can be specialized and would be used here
      write (iString, *) i
      is%wrap%modelComp(i) = ESMF_GridCompCreate(name="modelComp "// &
        trim(adjustl(iString)), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

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
        
      do j=1, is%wrap%modelCount
        if (j==i) cycle ! skip self connection
        write (jString, *) j
        is%wrap%connectorComp(i,j) = ESMF_CplCompCreate(name="connectorComp "//&
          trim(adjustl(iString))//" -> "//trim(adjustl(jString)), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
      enddo
    enddo
    
    ! initialize the default Run Sequence: grouped connectors before models
    nullify(is%wrap%runSequence)  ! initialize
    do i=1, is%wrap%modelCount
      do j=1, is%wrap%modelCount
        if (j==i) cycle ! skip self connection
        call routine_AddRunElement(gcomp, i=i, j=j, phase=1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
      enddo
    enddo
    do i=1, is%wrap%modelCount
      call routine_AddRunElement(gcomp, i=i, j=0, phase=1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    enddo
    
    ! SPECIALIZE by calling into attached method to SetServices for modelComps
    call ESMF_MethodExecute(gcomp, label=label_SetModelServices, &
      userRc=localrc, rc=rc)
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
      
    ! InitP0: modelComps
    do i=1, is%wrap%modelCount
      write (iString, *) i
      if (NUOPC_GridCompAreServicesSet(is%wrap%modelComp(i))) then
        call ESMF_GridCompGet(is%wrap%modelComp(i), name=compName, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        call ESMF_GridCompInitialize(is%wrap%modelComp(i), &
          importState=is%wrap%modelIS(i), exportState=is%wrap%modelES(i), &
          clock=internalClock, phase=0, userRc=localrc, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase 0 "// &
          "Initialize for modelComp "//trim(adjustl(iString))//": "// &
          trim(compName), &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase 0 "// &
          "Initialize for modelComp "//trim(adjustl(iString))//": "// &
          trim(compName)//" did not return ESMF_SUCCESS", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      endif
    enddo
    
    ! InitP0: connectorComps
    do i=1, is%wrap%modelCount
      write (iString, *) i
      do j=1, is%wrap%modelCount
        if (j==i) cycle ! skip self connection
        write (jString, *) j
        if (NUOPC_CplCompAreServicesSet(is%wrap%connectorComp(i,j))) then
          call ESMF_CplCompGet(is%wrap%connectorComp(i,j), name=compName, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
          call ESMF_CplCompInitialize(is%wrap%connectorComp(i,j), &
            importState=is%wrap%modelES(i), exportState=is%wrap%modelIS(j), &
            clock=internalClock, phase=0, userRc=localrc, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase 0 "// &
            "Initialize for connectorComp "// &
            trim(adjustl(iString))//" -> "//trim(adjustl(jString))//": "// &
            trim(compName), &
            line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
          if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase 0 "// &
            "Initialize for connectorComp "// &
            trim(adjustl(iString))//" -> "//trim(adjustl(jString))//": "// &
            trim(compName)//" did not return ESMF_SUCCESS", &
            line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        endif
      enddo
    enddo

    ! InitP1: modelComps
    do i=1, is%wrap%modelCount
      write (iString, *) i
      if (NUOPC_GridCompAreServicesSet(is%wrap%modelComp(i))) then
        call ESMF_GridCompGet(is%wrap%modelComp(i), name=compName, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        call ESMF_GridCompInitialize(is%wrap%modelComp(i), &
          importState=is%wrap%modelIS(i), exportState=is%wrap%modelES(i), &
          clock=internalClock, phase=1, userRc=localrc, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase 1 "// &
          "Initialize for modelComp "//trim(adjustl(iString))//": "// &
          trim(compName), &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase 1 "// &
          "Initialize for modelComp "//trim(adjustl(iString))//": "// &
          trim(compName)//" did not return ESMF_SUCCESS", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      endif
    enddo
    
    ! InitP1: connectorComps
    do i=1, is%wrap%modelCount
      write (iString, *) i
      do j=1, is%wrap%modelCount
        if (j==i) cycle ! skip self connection
        write (jString, *) j
        if (NUOPC_CplCompAreServicesSet(is%wrap%connectorComp(i,j))) then
          call ESMF_CplCompGet(is%wrap%connectorComp(i,j), name=compName, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
          call ESMF_CplCompInitialize(is%wrap%connectorComp(i,j), &
            importState=is%wrap%modelES(i), exportState=is%wrap%modelIS(j), &
            clock=internalClock, phase=1, userRc=localrc, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase 1 "// &
            "Initialize for connectorComp "// &
            trim(adjustl(iString))//" -> "//trim(adjustl(jString))//": "// &
            trim(compName), &
            line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
          if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase 1 "// &
            "Initialize for connectorComp "// &
            trim(adjustl(iString))//" -> "//trim(adjustl(jString))//": "// &
            trim(compName)//" did not return ESMF_SUCCESS", &
            line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        endif
      enddo
    enddo

    ! InitP2: modelComps
    do i=1, is%wrap%modelCount
      write (iString, *) i
      if (NUOPC_GridCompAreServicesSet(is%wrap%modelComp(i))) then
        call ESMF_GridCompGet(is%wrap%modelComp(i), name=compName, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        call ESMF_GridCompInitialize(is%wrap%modelComp(i), &
          importState=is%wrap%modelIS(i), exportState=is%wrap%modelES(i), &
          clock=internalClock, phase=2, userRc=localrc, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase 2 "// &
          "Initialize for modelComp "//trim(adjustl(iString))//": "// &
          trim(compName), &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase 2 "// &
          "Initialize for modelComp "//trim(adjustl(iString))//": "// &
          trim(compName)//" did not return ESMF_SUCCESS", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      endif
    enddo
    
    ! InitP3: modelComps
    do i=1, is%wrap%modelCount
      write (iString, *) i
      if (NUOPC_GridCompAreServicesSet(is%wrap%modelComp(i))) then
        call ESMF_GridCompGet(is%wrap%modelComp(i), name=compName, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        call ESMF_GridCompInitialize(is%wrap%modelComp(i), &
          importState=is%wrap%modelIS(i), exportState=is%wrap%modelES(i), &
          clock=internalClock, phase=3, userRc=localrc, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase 3 "// &
          "Initialize for modelComp "//trim(adjustl(iString))//": "// &
          trim(compName), &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase 3 "// &
          "Initialize for modelComp "//trim(adjustl(iString))//": "// &
          trim(compName)//" did not return ESMF_SUCCESS", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      endif
    enddo
        
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine Run(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    integer                   :: localrc
    type(type_InternalState)  :: is
    type(ESMF_Clock)          :: internalClock
    integer                   :: i, j, phase
    character(ESMF_MAXSTR)    :: iString, jString, pString
    type(type_RunElement), pointer  :: runElement
    character(ESMF_MAXSTR)    :: modelName

    rc = ESMF_SUCCESS
    
    call ESMF_GridCompGet(gcomp, name=modelName, rc=rc)
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
      trim(modelName)//" entered Run with current time: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    ! time stepping loop
    do while (.not. ESMF_ClockIsStopTime(internalClock, rc=rc))
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        
      ! execute Run Sequence
      if (associated(is%wrap%runSequence)) then
        runElement => is%wrap%runSequence
        do while (associated(runElement))
          i = runElement%i
          phase = runElement%phase
          if (runElement%j > 0) then
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
          ! advance to next element in Run Sequence
          runElement => runElement%next
        enddo
      endif

      ! advance to next time step
      call ESMF_ClockAdvance(internalClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    
      call NUOPC_ClockPrintCurrTime(internalClock, &
        trim(modelName)//" time stepping loop, current time: ", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
        
    enddo ! end of time stepping loop
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    
    call NUOPC_ClockPrintCurrTime(internalClock, ">>>"// &
      trim(modelName)//" leaving Run with current time: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine Finalize(gcomp, importState, exportState, clock, rc)
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
    do i=1, is%wrap%modelCount
      write (iString, *) i
      do j=1, is%wrap%modelCount
        if (j==i) cycle ! skip self connection
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
    do i=1, is%wrap%modelCount
      write (iString, *) i
      call ESMF_GridCompDestroy(is%wrap%modelComp(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      call ESMF_StateDestroy(is%wrap%modelIS(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      call ESMF_StateDestroy(is%wrap%modelES(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      do j=1, is%wrap%modelCount
        if (j==i) cycle ! skip self connection
        write (jString, *) j
        call ESMF_CplCompDestroy(is%wrap%connectorComp(i,j), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      enddo
    enddo

    ! deallocate lists inside the internal state
    deallocate(is%wrap%modelComp, is%wrap%modelIS, is%wrap%modelES, &
      is%wrap%connectorComp, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      
    ! deallocate RunSequence
    call routine_DeallocateRunSequence(gcomp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    ! deallocate internal state memory
    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine AddRunElementIJP(gcomp, i, j, phase, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(in)  :: i, j, phase
    integer, intent(out) :: rc
    ! local variables
    integer                   :: stat
    type(type_RunElement), pointer  :: runElement
    allocate(runElement, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of RunElement for RunSequence.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    runElement%i = i
    runElement%j = j
    runElement%phase = phase
    call routine_AddRunElement(gcomp, element=runElement, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine AddRunElementType(gcomp, element, rc)
    type(ESMF_GridComp)  :: gcomp
    type(type_RunElement), pointer:: element
    integer, intent(out) :: rc
    ! local variables
    type(type_InternalState)  :: is
    type(type_RunElement), pointer  :: runElement
    ! query Component for this internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    ! find the last element in current list and point to added element
    if (.not.associated(is%wrap%runSequence)) then
      is%wrap%runSequence => element
    else
      runElement => is%wrap%runSequence
      do while (associated(runElement%next))
        runElement => runElement%next
      enddo
      runElement%next => element
    endif
    nullify(element%next) ! make sure to terminate the linked list properly
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine routine_DeallocateRunSequence(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    ! local variables
    integer                   :: stat
    type(type_InternalState)  :: is
    type(type_RunElement), pointer  :: runElement, runElementNext
    ! query Component for this internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    ! deallocate RunSequence
    if (associated(is%wrap%runSequence)) then
      runElement => is%wrap%runSequence
      do while (associated(runElement))
        runElementNext => runElement%next
        deallocate(runElement, stat=stat)
        if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
          msg="Deallocation of runElement in runSequence.", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        runElement => runElementNext
      enddo
      nullify(is%wrap%runSequence)  ! ensure well defined pointer status
    endif
  end subroutine

end module
