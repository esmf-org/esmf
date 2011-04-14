! $Id: NUOPC_DriverExplicit.F90,v 1.2 2011/04/14 15:12:53 theurich Exp $
module NUOPC_DriverExplicit

  !-----------------------------------------------------------------------------
  ! Generic Driver Component with explicit time stepping
  !-----------------------------------------------------------------------------

  use ESMF_Mod
  use NUOPC

  implicit none
  
  private
  
  public SetServices, InternalState, InternalStateStruct
  public ConnectorPlacing, connectorPlacingPreModel, connectorPlacingPostModel
  public ConnectorGrouping, connectorGroupingNone, &
    connectorGroupingPreModel, connectorGroupingPostModel
  
  type ConnectorPlacing
    integer :: value
  end type
  
  type(ConnectorPlacing), parameter :: &
    connectorPlacingPreModel   = ConnectorPlacing(1), &
    connectorPlacingPostModel  = ConnectorPlacing(2)
    
  type ConnectorGrouping
    integer :: value
  end type
  
  type(ConnectorGrouping), parameter :: &
    connectorGroupingNone      = connectorGrouping(0), &
    connectorGroupingPreModel  = connectorGrouping(1), &
    connectorGroupingPostModel = connectorGrouping(2)
  
  type InternalStateStruct
    integer                         :: modelCount
    type(ESMF_GridComp), pointer    :: modelComp(:)
    type(ESMF_State),    pointer    :: modelIS(:), modelES(:)
    type(ConnectorGrouping)         :: connectorGrouping
    type(ConnectorPlacing), pointer :: connectorPlacing(:)
    type(ESMF_CplComp),  pointer    :: connectorComp(:,:)
  end type

  type InternalState
    type(InternalStateStruct), pointer :: wrap
  end type

  interface operator (==)
    module procedure CP_eq
    module procedure CG_eq
  end interface

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  function CP_eq(a, b)
    logical CP_eq
    type(ConnectorPlacing), intent(in) :: a, b
    CP_eq = (a%value == b%value)
  end function

  function CG_eq(a, b)
    logical CG_eq
    type(ConnectorGrouping), intent(in) :: a, b
    CG_eq = (a%value == b%value)
  end function
  
  !-----------------------------------------------------------------------------

  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETINIT, &
      userRoutine=Initialize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETRUN, &
      userRoutine=Run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETFINAL, &
      userRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine Initialize(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    integer                 :: localrc, stat
    type(InternalState)     :: is
    type(ESMF_Clock)        :: internalClock
    integer                 :: i, j
    character(ESMF_MAXSTR)  :: iString, jString, compName

    rc = ESMF_SUCCESS
    
    ! allocate memory for the internal state and set it in the Component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal state memory failed.", &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out
    call ESMF_UserCompSetInternalState(gcomp, "NUOPC_DriverExplicit", is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! default setting for connectorGrouping
    is%wrap%connectorGrouping = connectorGroupingPreModel
      
    ! SPECIALIZE by calling into attached method to set modelCount
    call ESMF_MethodExecute(gcomp, label="DriverExplicit_SetModelCount", &
      userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRPASS, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOG_ERRPASS, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out

    ! allocate lists inside the internal state according to modelCount
    allocate(is%wrap%modelComp(is%wrap%modelCount), &
      is%wrap%modelIS(is%wrap%modelCount), is%wrap%modelES(is%wrap%modelCount),&
      is%wrap%connectorPlacing(is%wrap%modelCount), &
      is%wrap%connectorComp(is%wrap%modelCount,is%wrap%modelCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal state memory failed.", &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out

    ! create modelComps and their import and export States + connectorComps
    do i=1, is%wrap%modelCount
      !TODO: there should be petList members in the internal State that 
      !TODO: can be specialized and would be used here
      write (iString, *) i
      is%wrap%modelComp(i) = ESMF_GridCompCreate(name="modelComp "// &
        trim(adjustl(iString)), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      is%wrap%modelIS(i) = ESMF_StateCreate(name="modelComp "// &
        trim(adjustl(iString))//" Import State", &
        statetype=ESMF_STATE_IMPORT, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      is%wrap%modelES(i) = ESMF_StateCreate(name="modelComp "// &
        trim(adjustl(iString))//" Export State", &
        statetype=ESMF_STATE_EXPORT, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
        
      ! default setting for connectorPlacing
      is%wrap%connectorPlacing(i) = connectorPlacingPreModel
        
      do j=1, is%wrap%modelCount
        if (j==i) cycle ! skip self connection
        write (jString, *) j
        is%wrap%connectorComp(i,j) = ESMF_CplCompCreate(name="connectorComp "//&
          trim(adjustl(iString))//" -> "//trim(adjustl(jString)), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      enddo
    enddo
    
    ! SPECIALIZE by calling into attached method to SetServices for modelComps
    call ESMF_MethodExecute(gcomp, label="DriverExplicit_SetModelServices", &
      userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRPASS, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOG_ERRPASS, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out
      
    ! query Component for its Clock (set during specialization)
    call ESMF_GridCompGet(gcomp, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! InitP0: modelComps
    do i=1, is%wrap%modelCount
      write (iString, *) i
      if (NUOPC_GridCompAreServicesSet(is%wrap%modelComp(i))) then
        call ESMF_GridCompGet(is%wrap%modelComp(i), name=compName, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_GridCompInitialize(is%wrap%modelComp(i), &
          importState=is%wrap%modelIS(i), exportState=is%wrap%modelES(i), &
          clock=internalClock, phase=0, userRc=localrc, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase 0 "// &
          "Initialize for modelComp "//trim(adjustl(iString))//": "// &
          trim(compName), &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase 0 "// &
          "Initialize for modelComp "//trim(adjustl(iString))//": "// &
          trim(compName)//" did not return ESMF_SUCCESS", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)) &
          return  ! bail out
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
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          call ESMF_CplCompInitialize(is%wrap%connectorComp(i,j), &
            importState=is%wrap%modelES(i), exportState=is%wrap%modelIS(j), &
            clock=internalClock, phase=0, userRc=localrc, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase 0 "// &
            "Initialize for connectorComp "// &
            trim(adjustl(iString))//" -> "//trim(adjustl(jString))//": "// &
            trim(compName), &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase 0 "// &
            "Initialize for connectorComp "// &
            trim(adjustl(iString))//" -> "//trim(adjustl(jString))//": "// &
            trim(compName)//" did not return ESMF_SUCCESS", &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc)) &
            return  ! bail out
        endif
      enddo
    enddo

    ! InitP1: modelComps
    do i=1, is%wrap%modelCount
      write (iString, *) i
      if (NUOPC_GridCompAreServicesSet(is%wrap%modelComp(i))) then
        call ESMF_GridCompGet(is%wrap%modelComp(i), name=compName, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_GridCompInitialize(is%wrap%modelComp(i), &
          importState=is%wrap%modelIS(i), exportState=is%wrap%modelES(i), &
          clock=internalClock, phase=1, userRc=localrc, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase 1 "// &
          "Initialize for modelComp "//trim(adjustl(iString))//": "// &
          trim(compName), &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase 1 "// &
          "Initialize for modelComp "//trim(adjustl(iString))//": "// &
          trim(compName)//" did not return ESMF_SUCCESS", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)) &
          return  ! bail out
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
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          call ESMF_CplCompInitialize(is%wrap%connectorComp(i,j), &
            importState=is%wrap%modelES(i), exportState=is%wrap%modelIS(j), &
            clock=internalClock, phase=1, userRc=localrc, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase 1 "// &
            "Initialize for connectorComp "// &
            trim(adjustl(iString))//" -> "//trim(adjustl(jString))//": "// &
            trim(compName), &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase 1 "// &
            "Initialize for connectorComp "// &
            trim(adjustl(iString))//" -> "//trim(adjustl(jString))//": "// &
            trim(compName)//" did not return ESMF_SUCCESS", &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc)) &
            return  ! bail out
        endif
      enddo
    enddo

    ! InitP2: modelComps
    do i=1, is%wrap%modelCount
      write (iString, *) i
      if (NUOPC_GridCompAreServicesSet(is%wrap%modelComp(i))) then
        call ESMF_GridCompGet(is%wrap%modelComp(i), name=compName, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_GridCompInitialize(is%wrap%modelComp(i), &
          importState=is%wrap%modelIS(i), exportState=is%wrap%modelES(i), &
          clock=internalClock, phase=2, userRc=localrc, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase 2 "// &
          "Initialize for modelComp "//trim(adjustl(iString))//": "// &
          trim(compName), &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase 2 "// &
          "Initialize for modelComp "//trim(adjustl(iString))//": "// &
          trim(compName)//" did not return ESMF_SUCCESS", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)) &
          return  ! bail out
      endif
    enddo
    
    ! InitP3: modelComps
    do i=1, is%wrap%modelCount
      write (iString, *) i
      if (NUOPC_GridCompAreServicesSet(is%wrap%modelComp(i))) then
        call ESMF_GridCompGet(is%wrap%modelComp(i), name=compName, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_GridCompInitialize(is%wrap%modelComp(i), &
          importState=is%wrap%modelIS(i), exportState=is%wrap%modelES(i), &
          clock=internalClock, phase=3, userRc=localrc, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase 3 "// &
          "Initialize for modelComp "//trim(adjustl(iString))//": "// &
          trim(compName), &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase 3 "// &
          "Initialize for modelComp "//trim(adjustl(iString))//": "// &
          trim(compName)//" did not return ESMF_SUCCESS", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)) &
          return  ! bail out
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
    integer                 :: localrc
    type(InternalState)     :: is
    type(ESMF_Clock)        :: internalClock
    integer                 :: i, j
    character(ESMF_MAXSTR)  :: iString, jString

    rc = ESMF_SUCCESS
    
    ! query Component for its Clock
    call ESMF_GridCompGet(gcomp, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! query Component for this internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, "NUOPC_DriverExplicit", is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! time stepping loop
    do while (.not. ESMF_ClockIsStopTime(internalClock, rc=rc))
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      if (is%wrap%connectorGrouping == connectorGroupingPreModel) then
        ! Run: connectorComps before the model components
        do i=1, is%wrap%modelCount
          write (iString, *) i
          do j=1, is%wrap%modelCount
            if (j==i) cycle ! skip self connection
            write (jString, *) j
            if (NUOPC_CplCompAreServicesSet(is%wrap%connectorComp(i,j))) then
              call ESMF_CplCompRun(is%wrap%connectorComp(i,j), &
                importState=is%wrap%modelES(i), exportState=is%wrap%modelIS(j), &
                clock=internalClock, phase=1, userRc=localrc, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, &
                msg="Failed calling phase 1 Run for connectorComp "// &
                trim(adjustl(iString))//" -> "//trim(adjustl(jString)), &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              if (ESMF_LogFoundError(rcToCheck=localrc, &
                msg="Phase 1 Run for connectorComp "// &
                trim(adjustl(iString))//" -> "//trim(adjustl(jString))// &
                " did not return ESMF_SUCCESS", &
                line=__LINE__, &
                file=__FILE__, &
                rcToReturn=rc)) &
                return  ! bail out
            endif
          enddo
        enddo
      endif

      ! Run: modelComps
      do i=1, is%wrap%modelCount
        write (iString, *) i
        if (is%wrap%connectorGrouping == connectorGroupingNone .and. &
          is%wrap%connectorPlacing(i) == connectorPlacingPreModel) then
          do j=1, is%wrap%modelCount
            if (j==i) cycle ! skip self connection
            write (jString, *) j
            if (NUOPC_CplCompAreServicesSet(is%wrap%connectorComp(i,j))) then
              call ESMF_CplCompRun(is%wrap%connectorComp(j,i), &
                importState=is%wrap%modelES(j), exportState=is%wrap%modelIS(i), &
                clock=internalClock, phase=1, userRc=localrc, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, &
                msg="Failed calling phase 1 Run for connectorComp "// &
                trim(adjustl(jString))//" -> "//trim(adjustl(iString)), &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              if (ESMF_LogFoundError(rcToCheck=localrc, &
                msg="Phase 1 Run for connectorComp "// &
                trim(adjustl(jString))//" -> "//trim(adjustl(iString))// &
                " did not return ESMF_SUCCESS", &
                line=__LINE__, &
                file=__FILE__, &
                rcToReturn=rc)) &
                return  ! bail out
            endif
          enddo
        endif
        if (NUOPC_GridCompAreServicesSet(is%wrap%modelComp(i))) then
          call ESMF_GridCompRun(is%wrap%modelComp(i), &
            importState=is%wrap%modelIS(i), exportState=is%wrap%modelES(i), &
            clock=internalClock, phase=1, userRc=localrc, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase 1 "// &
            "Run for modelComp "//trim(adjustl(iString)), &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase 1 "// &
            "Run for modelComp "//trim(adjustl(iString))//" did not "// &
            "return ESMF_SUCCESS", &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc)) &
            return  ! bail out
        endif
        if (is%wrap%connectorGrouping == connectorGroupingNone .and. &
          is%wrap%connectorPlacing(i) == connectorPlacingPostModel) then
          do j=1, is%wrap%modelCount
            if (j==i) cycle ! skip self connection
            write (jString, *) j
            if (NUOPC_CplCompAreServicesSet(is%wrap%connectorComp(i,j))) then
              call ESMF_CplCompRun(is%wrap%connectorComp(i,j), &
                importState=is%wrap%modelES(i), exportState=is%wrap%modelIS(j), &
                clock=internalClock, phase=1, userRc=localrc, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, &
                msg="Failed calling phase 1 Run for connectorComp "// &
                trim(adjustl(iString))//" -> "//trim(adjustl(jString)), &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              if (ESMF_LogFoundError(rcToCheck=localrc, &
                msg="Phase 1 Run for connectorComp "// &
                trim(adjustl(iString))//" -> "//trim(adjustl(jString))// &
                " did not return ESMF_SUCCESS", &
                line=__LINE__, &
                file=__FILE__, &
                rcToReturn=rc)) &
                return  ! bail out
            endif
          enddo
        endif
      enddo
    
      if (is%wrap%connectorGrouping == connectorGroupingPostModel) then
        ! Run: connectorComps after the model components
        do i=1, is%wrap%modelCount
          write (iString, *) i
          do j=1, is%wrap%modelCount
            if (j==i) cycle ! skip self connection
            write (jString, *) j
            if (NUOPC_CplCompAreServicesSet(is%wrap%connectorComp(i,j))) then
              call ESMF_CplCompRun(is%wrap%connectorComp(i,j), &
                importState=is%wrap%modelES(i), exportState=is%wrap%modelIS(j), &
                clock=internalClock, phase=1, userRc=localrc, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, &
                msg="Failed calling phase 1 Run for connectorComp "// &
                trim(adjustl(iString))//" -> "//trim(adjustl(jString)), &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              if (ESMF_LogFoundError(rcToCheck=localrc, &
                msg="Phase 1 Run for connectorComp "// &
                trim(adjustl(iString))//" -> "//trim(adjustl(jString))// &
                " did not return ESMF_SUCCESS", &
                line=__LINE__, &
                file=__FILE__, &
                rcToReturn=rc)) &
                return  ! bail out
            endif
          enddo
        enddo
      endif

      ! advance to next time step
      call ESMF_ClockAdvance(internalClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    
    enddo ! end of time stepping loop
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine Finalize(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    integer                 :: localrc, stat
    type(InternalState)     :: is
    type(ESMF_Clock)        :: internalClock
    integer                 :: i, j
    character(ESMF_MAXSTR)  :: iString, jString
    logical                 :: existflag

    rc = ESMF_SUCCESS
    
    ! SPECIALIZE by calling into optional attached method
    call ESMF_MethodExecute(gcomp, label="DriverExplicit_Finalize", &
      existflag=existflag, userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRPASS, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOG_ERRPASS, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out

    ! query Component for its Clock
    call ESMF_GridCompGet(gcomp, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! query Component for this internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, "NUOPC_DriverExplicit", is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
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
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase 1 "// &
            "Finalize for connectorComp "// &
            trim(adjustl(iString))//" -> "//trim(adjustl(jString))//" did not "// &
            "return ESMF_SUCCESS", &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc)) &
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
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase 1 "// &
          "Finalize for modelComp "//trim(adjustl(iString))//" did not "// &
          "return ESMF_SUCCESS", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)) &
          return  ! bail out
      endif
    enddo
    
    ! destroy modelComps and their import and export States + connectorComps
    do i=1, is%wrap%modelCount
      write (iString, *) i
      call ESMF_GridCompDestroy(is%wrap%modelComp(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_StateDestroy(is%wrap%modelIS(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_StateDestroy(is%wrap%modelES(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      do j=1, is%wrap%modelCount
        if (j==i) cycle ! skip self connection
        write (jString, *) j
        call ESMF_CplCompDestroy(is%wrap%connectorComp(i,j), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      enddo
    enddo

    ! deallocate lists inside the internal state
    deallocate(is%wrap%modelComp, is%wrap%modelIS, is%wrap%modelES, &
      is%wrap%connectorPlacing, is%wrap%connectorComp, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out

    ! deallocate internal state memory
    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out
      
  end subroutine

end module
