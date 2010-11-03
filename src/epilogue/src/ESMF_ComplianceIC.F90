! $Id: ESMF_ComplianceIC.F90,v 1.17 2010/11/03 22:48:47 theurich Exp $
!
! Compliance Interface Component
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!-------------------------------------------------------------------------
! !DESCRIPTION:
!  Interface Component
!-------------------------------------------------------------------------


!TODO: make this macros available through ESMF_Mod as parameter or find other way
#define ESMF_INIT_CREATED 82949521

module ESMF_ComplianceICMod

  ! ESMF module
  use ESMF_Mod

  implicit none
  
  private
  
  integer, save :: ccfDepth = 1 ! component control flow depth
  
  public setvmIC, registerIC
        
  contains

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   !  The setvm routine is used by the child component to set VM properties
!   !TODO:  currently the setvmIC() is _not_ hooked into the ESMF callback 

  recursive subroutine setvmIC(comp, rc)
    type(ESMF_GridComp)   :: comp
    integer, intent(out)  :: rc
    
    ! Initialize user return code
    rc = ESMF_SUCCESS
    
    print *, "start setvmIC"

    ! This code is being executed _after_ the actual Component SetVM call
    
    !TODO: currently the setvmIC() is _not_ hooked into the ESMF callback 

    print *, "stop setvmIC"

  end subroutine

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
  recursive subroutine registerIC(comp, rc)
    type(ESMF_GridComp)   :: comp
    integer, intent(out)  :: rc

    character(ESMF_MAXSTR)  :: prefix
    character(ESMF_MAXSTR)  :: output
    integer                 :: phaseCount
    
    ! Initialize user return code
    rc = ESMF_SUCCESS
    
    ! IMPORTANT: As an InterfaceComponent the code must ensure:
    ! 1) That the return code from the actual child method is returned to the 
    !    parent (note that this is not currently possible for the register)

    ! This code is being executed _after_ the actual Component Register call

    call prefixString(comp, prefix=prefix, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_LogWrite(trim(prefix)//"Start register compliance check.", &
      ESMF_LOG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    !---------------------------------------------------------------------------
    ! Start Compliance Checking
    
    ! check Initialize registration
    call ESMF_GridCompGetEPPhaseCount(comp, ESMF_SETINIT, phaseCount, rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (phaseCount == 0) then
      call ESMF_LogWrite(trim(prefix)//" ==> No Initialize method registered!", &
        ESMF_LOG_WARNING, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      write(output,*) " ",phaseCount," phase(s) of Initialize registered."
      call ESMF_LogWrite(trim(prefix)//trim(output), ESMF_LOG_INFO, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    
    ! check Run registration
    call ESMF_GridCompGetEPPhaseCount(comp, ESMF_SETRUN, phaseCount, rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (phaseCount == 0) then
      call ESMF_LogWrite(trim(prefix)//" ==> No Run method registered!", &
        ESMF_LOG_WARNING, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      write(output,*) " ",phaseCount," phase(s) of Run registered."
      call ESMF_LogWrite(trim(prefix)//trim(output), ESMF_LOG_INFO, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! check Finalize registration
    call ESMF_GridCompGetEPPhaseCount(comp, ESMF_SETFINAL, phaseCount, rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (phaseCount == 0) then
      call ESMF_LogWrite(trim(prefix)//" ==> No Finalize method registered!", &
        ESMF_LOG_WARNING, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      write(output,*) " ",phaseCount," phase(s) of Finalize registered."
      call ESMF_LogWrite(trim(prefix)//trim(output), ESMF_LOG_INFO, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! Stop Compliance Checking
    !---------------------------------------------------------------------------

    ! Register the IC callback routines.
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINITIC, userRoutine=ic_init, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUNIC, userRoutine=ic_run, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINALIC, userRoutine=ic_final, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_LogWrite(trim(prefix)//"Stop register compliance check.", &
      ESMF_LOG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   !   Initialization routine.
    
  recursive subroutine ic_init(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    integer                 :: userrc
    character(ESMF_MAXSTR)  :: prefix
    character(ESMF_MAXSTR)  :: output
    type(ESMF_Clock)        :: clockCopy
    
    ! Initialize user return code
    rc = ESMF_SUCCESS

    call prefixString(comp, prefix=prefix, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    !---------------------------------------------------------------------------
    ! Start Compliance Checking: InitializePrologue
    
    call ESMF_LogWrite(trim(prefix)//"Start InitializePrologue.", &
      ESMF_LOG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! compliance check importState
    call checkState(prefix, referenceName="importState", state=importState, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! compliance check exportState
    call checkState(prefix, referenceName="exportState", state=exportState, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! compliance check clock usage
    call clockUsageIncoming(prefix, clock=clock, clockCopy=clockCopy, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_LogWrite(trim(prefix)//"Stop InitializePrologue.", &
      ESMF_LOG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! Stop Compliance Checking: InitializePrologue
    !---------------------------------------------------------------------------
    ccfDepth = ccfDepth + 1

    call ESMF_GridCompInitializeAct(comp, importState, exportState, clock, &
      userRc=userrc, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (userrc /= ESMF_SUCCESS) then
      rc=userrc
      return ! bail out
    endif
   
    ccfDepth = ccfDepth - 1
    !---------------------------------------------------------------------------
    ! Start Compliance Checking: InitializeEpilogue
    
    call ESMF_LogWrite(trim(prefix)//"Start InitializeEpilogue.", &
      ESMF_LOG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! compliance check Component metadata
    call checkComponentMetadata(prefix, comp=comp, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! compliance check importState
    call checkState(prefix, referenceName="importState", state=importState, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! compliance check exportState
    call checkState(prefix, referenceName="exportState", state=exportState, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! compliance check clock usage
    call clockUsageOutgoing(prefix, clock=clock, clockCopy=clockCopy, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! compliance check internal Clock
    call checkInternalClock(prefix, comp=comp, clock=clock, &
      mustReachStop=.false., rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Component Attributes should be set up -> ready to output
!    call ESMF_AttributeWrite(comp, convention='CIM 1.0', &
!      purpose='Model Component Simulation Description', &
!      attwriteflag=ESMF_ATTWRITE_XML, rc=rc)
!    if (ESMF_LogFoundError(rc, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
    
    
    call ESMF_LogWrite(trim(prefix)//"Stop InitializeEpilogue.", &
      ESMF_LOG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! Stop Compliance Checking: InitializeEpilogue
    !---------------------------------------------------------------------------

  end subroutine ic_init


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   !  Run routine
 
  recursive subroutine ic_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    integer                 :: userrc
    character(ESMF_MAXSTR)  :: prefix
    character(ESMF_MAXSTR)  :: output
    type(ESMF_Clock)        :: clockCopy
    
    ! Initialize user return code
    rc = ESMF_SUCCESS

    call prefixString(comp, prefix=prefix, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    !---------------------------------------------------------------------------
    ! Start Compliance Checking: RunPrologue
    
    call ESMF_LogWrite(trim(prefix)//"Start RunPrologue.", &
      ESMF_LOG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! compliance check importState
    call checkState(prefix, referenceName="importState", state=importState, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! compliance check exportState
    call checkState(prefix, referenceName="exportState", state=exportState, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! compliance check clock usage
    call clockUsageIncoming(prefix, clock=clock, clockCopy=clockCopy, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_LogWrite(trim(prefix)//"Stop RunPrologue.", &
      ESMF_LOG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! Stop Compliance Checking: RunPrologue
    !---------------------------------------------------------------------------
    ccfDepth = ccfDepth + 1

    call ESMF_GridCompRunAct(comp, importState, exportState, clock, &
      userRc=userrc, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (userrc /= ESMF_SUCCESS) then
      rc=userrc
      return ! bail out
    endif

    ccfDepth = ccfDepth - 1
    !---------------------------------------------------------------------------
    ! Start Compliance Checking: RunEpilogue
    
    call ESMF_LogWrite(trim(prefix)//"Start RunEpilogue.", &
      ESMF_LOG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! compliance check importState
    call checkState(prefix, referenceName="importState", state=importState, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! compliance check exportState
    call checkState(prefix, referenceName="exportState", state=exportState, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! compliance check clock usage
    call clockUsageOutgoing(prefix, clock=clock, clockCopy=clockCopy, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! compliance check internal Clock
    call checkInternalClock(prefix, comp=comp, clock=clock, &
      mustReachStop=.true., rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_LogWrite(trim(prefix)//"Stop RunEpilogue.", &
      ESMF_LOG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! Stop Compliance Checking: RunEpilogue
    !---------------------------------------------------------------------------

  end subroutine ic_run


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   !  Finalize routine
 
  recursive subroutine ic_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    integer                 :: userrc
    character(ESMF_MAXSTR)  :: prefix
    character(ESMF_MAXSTR)  :: output
    type(ESMF_Clock)        :: clockCopy
    
    ! Initialize user return code
    rc = ESMF_SUCCESS

    call prefixString(comp, prefix=prefix, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    !---------------------------------------------------------------------------
    ! Start Compliance Checking: FinalizePrologue
    
    call ESMF_LogWrite(trim(prefix)//"Start FinalizePrologue.", &
      ESMF_LOG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! compliance check importState
    call checkState(prefix, referenceName="importState", state=importState, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! compliance check exportState
    call checkState(prefix, referenceName="exportState", state=exportState, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! compliance check clock usage
    call clockUsageIncoming(prefix, clock=clock, clockCopy=clockCopy, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_LogWrite(trim(prefix)//"Stop FinalizePrologue.", &
      ESMF_LOG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! Stop Compliance Checking: FinalizePrologue
    !---------------------------------------------------------------------------
    ccfDepth = ccfDepth + 1

    call ESMF_GridCompFinalizeAct(comp, importState, exportState, clock, &
      userRc=userrc, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (userrc /= ESMF_SUCCESS) then
      rc=userrc
      return ! bail out
    endif

    ccfDepth = ccfDepth - 1
    !---------------------------------------------------------------------------
    ! Start Compliance Checking: FinalizeEpilogue
    
    call ESMF_LogWrite(trim(prefix)//"Start FinalizeEpilogue.", &
      ESMF_LOG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! compliance check importState
    call checkState(prefix, referenceName="importState", state=importState, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! compliance check exportState
    call checkState(prefix, referenceName="exportState", state=exportState, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! compliance check clock usage
    call clockUsageOutgoing(prefix, clock=clock, clockCopy=clockCopy, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! compliance check internal Clock
    call checkInternalClock(prefix, comp=comp, clock=clock, &
      mustReachStop=.true., rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_LogWrite(trim(prefix)//"Stop FinalizeEpilogue.", &
      ESMF_LOG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! Stop Compliance Checking: FinalizeEpilogue
    !---------------------------------------------------------------------------

  end subroutine ic_final


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! IC HELPER ROUTINES:
!-------------------------------------------------------------------------

  recursive subroutine prefixString(comp, prefix, rc)
    type(ESMF_GridComp)                       :: comp
    character(*),       intent(inout)         :: prefix
    integer,            intent(out), optional :: rc

    character(ESMF_MAXSTR) :: compName
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_GridCompGet(comp, name=compName, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    prefix = "COMPLIANCECHECKER:"//repeat("|->", ccfDepth)//":"//trim(compName)//":"

  end subroutine    

!-------------------------------------------------------------------------

  recursive subroutine checkState(prefix, referenceName, state, rc)
    character(*), intent(in)              :: prefix
    character(*), intent(in)              :: referenceName
    type(ESMF_State)                      :: state
    integer,      intent(out), optional   :: rc
      
    logical                               :: stateValid
    integer                               :: itemCount, item
    integer                               :: fieldCount, fitem
    character(ESMF_MAXSTR)                :: name
    type(ESMF_StateType)                  :: statetype
    character(ESMF_MAXSTR)                :: tempString
    character(ESMF_MAXSTR), allocatable   :: itemNameList(:)
    type(ESMF_StateItemType), allocatable :: stateitemtypeList(:)
    type(ESMF_Field)                      :: field
    type(ESMF_FieldBundle)                :: fieldbundle

    if (present(rc)) rc = ESMF_SUCCESS

    stateValid = .true.
    ! Ensure that the State is a valid object
    if (ESMF_StateGetInit(state) /= ESMF_INIT_CREATED) then
      call ESMF_LogWrite(trim(prefix)//" ==> The "//trim(referenceName)// &
        " is invalid!", &
        ESMF_LOG_WARNING, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      stateValid = .false.
    endif

    if (stateValid) then
      ! Provide name and type of State
      call ESMF_StateGet(state, name=name, statetype=statetype, &
        itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_LogWrite(trim(prefix)//" "//trim(referenceName)//" name: "// &
        trim(name), ESMF_LOG_INFO, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      if (statetype==ESMF_STATE_IMPORT) then
        tempString = "ESMF_STATE_IMPORT"
      else if (statetype==ESMF_STATE_EXPORT) then
        tempString = "ESMF_STATE_EXPORT"
      else
        tempString = "ESMF_STATE_INVALID"
      endif
      call ESMF_LogWrite(trim(prefix)//" "//trim(referenceName)//" statetype: "// &
        trim(tempString), ESMF_LOG_INFO, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      write (tempString, *) itemCount
      call ESMF_LogWrite(trim(prefix)//" "//trim(referenceName)//" itemCount: "// &
        trim(tempString), ESMF_LOG_INFO, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      if (itemCount > 0) then
        allocate(itemNameList(itemCount))
        allocate(stateitemtypeList(itemCount))
        call ESMF_StateGet(state, itemNameList=itemNameList, &
          stateitemtypeList=stateitemtypeList, rc=rc)
        if (ESMF_LogFoundError(rc, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
          
        do item=1, itemCount
          if (stateitemtypeList(item) == ESMF_STATEITEM_FIELD) then
            write (tempString, *) item, " [FIELD] name: "
          else if (stateitemtypeList(item) == ESMF_STATEITEM_FIELDBUNDLE) then
            write (tempString, *) item, " [FIELDBUNDLE] name: "
          else if (stateitemtypeList(item) == ESMF_STATEITEM_ARRAY) then
            call ESMF_LogWrite(trim(prefix)//" ==> The "//trim(referenceName)// &
              " contains an ESMF_Array object!", ESMF_LOG_WARNING, rc=rc)
            if (ESMF_LogFoundError(rc, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            write (tempString, *) item, " [ARRAY] name: "
          else if (stateitemtypeList(item) == ESMF_STATEITEM_ARRAYBUNDLE) then
            call ESMF_LogWrite(trim(prefix)//" ==> The "//trim(referenceName)// &
              " contains an ESMF_ArrayBundle object!", ESMF_LOG_WARNING, rc=rc)
            if (ESMF_LogFoundError(rc, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            write (tempString, *) item, " [ARRAYBUNDLE] name: "
          else if (stateitemtypeList(item) == ESMF_STATEITEM_ROUTEHANDLE) then
            write (tempString, *) item, " [ROUTEHANDLE] name: "
          else if (stateitemtypeList(item) == ESMF_STATEITEM_STATE) then
            write (tempString, *) item, " [STATE] name: "
          else if (stateitemtypeList(item) == ESMF_STATEITEM_NAME) then
            write (tempString, *) item, " [NAME] name: "
          else if (stateitemtypeList(item) == ESMF_STATEITEM_INDIRECT) then
            write (tempString, *) item, " [INDIRECT] name: "
          else if (stateitemtypeList(item) == ESMF_STATEITEM_UNKNOWN) then
            write (tempString, *) item, " [UNKNOWN] name: "
          else if (stateitemtypeList(item) == ESMF_STATEITEM_NOTFOUND) then
            write (tempString, *) item, " [NOTFOUND] name: "
          endif
          
          call ESMF_LogWrite(trim(prefix)//" "//trim(referenceName)//" item #"// &
            trim(tempString)//trim(itemNameList(item)), &
            ESMF_LOG_INFO, rc=rc)
          if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          
          ! check metadata compliance            
          if (stateitemtypeList(item) == ESMF_STATEITEM_FIELD) then
            ! compliance check Field metadata
            call ESMF_StateGet(state, itemName=itemNameList(item), &
              field=field, rc=rc)
            if (ESMF_LogFoundError(rc, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            call checkFieldMetadata(prefix, field=field, rc=rc)
            if (ESMF_LogFoundError(rc, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
          else if (stateitemtypeList(item) == ESMF_STATEITEM_FIELDBUNDLE) then
            call ESMF_StateGet(state, itemName=itemNameList(item), &
              fieldbundle=fieldbundle, rc=rc)
            if (ESMF_LogFoundError(rc, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            call ESMF_FieldBundleGet(fieldbundle, fieldCount=fieldCount, rc=rc)
            if (ESMF_LogFoundError(rc, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            do fitem=1, fieldCount
              call ESMF_FieldBundleGet(fieldbundle, fieldIndex=fitem, &
                field=field, rc=rc)
              if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              call ESMF_FieldGet(field, name=name, rc=rc)
              if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              call ESMF_LogWrite(trim(prefix)//" in FieldBundle, Field name: "//&
                trim(name), ESMF_LOG_INFO, rc=rc)
              if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              call checkFieldMetadata(prefix, field=field, rc=rc)
              if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
            enddo
          endif
          
        enddo
        
        deallocate(stateitemtypeList)
        deallocate(itemNameList)
      endif
    endif      
  end subroutine

!-------------------------------------------------------------------------

  recursive subroutine checkComponentMetadata(prefix, comp, rc)
    character(*), intent(in)              :: prefix
    type(ESMF_GridComp)                   :: comp
    integer,      intent(out), optional   :: rc
    
    type(ESMF_CompType)                   :: comptype
    character(ESMF_MAXSTR)                :: attributeName
    character(ESMF_MAXSTR)                :: convention
    character(ESMF_MAXSTR)                :: purpose
      
    if (present(rc)) rc = ESMF_SUCCESS
    
    ! skip the metadata check if this is not a Gridded Component
    call ESMF_GridCompGet(comp, comptype=comptype, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (comptype /= ESMF_COMPTYPE_GRID) return
    
    ! set CIM convention and purpose specifiers
    convention = "CIM 1.0"
    purpose = "Model Component Simulation Description"
    
    call ESMF_LogWrite(trim(prefix)//" Component level attribute check: "// &
      "convention: '"//trim(convention)//"', purpose: '"//trim(purpose)//"'.", &
      ESMF_LOG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    attributeName = "ShortName"
    call checkComponentAttribute(prefix, comp=comp, &
      attributeName=attributeName, convention=convention, purpose=purpose, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    attributeName = "LongName"
    call checkComponentAttribute(prefix, comp=comp, &
      attributeName=attributeName, convention=convention, purpose=purpose, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    attributeName = "Description"
    call checkComponentAttribute(prefix, comp=comp, &
      attributeName=attributeName, convention=convention, purpose=purpose, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    attributeName = "ModelType"
    call checkComponentAttribute(prefix, comp=comp, &
      attributeName=attributeName, convention=convention, purpose=purpose, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    attributeName = "ReleaseDate"
    call checkComponentAttribute(prefix, comp=comp, &
      attributeName=attributeName, convention=convention, purpose=purpose, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    attributeName = "PreviousVersion"
    call checkComponentAttribute(prefix, comp=comp, &
      attributeName=attributeName, convention=convention, purpose=purpose, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

#if 0
! skip Citation* attributes as per Cecelia 10/05/10      
    attributeName = "ShortTitle"
    call checkComponentAttribute(prefix, comp=comp, &
      attributeName=attributeName, convention=convention, purpose=purpose, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    attributeName = "LongTitle"
    call checkComponentAttribute(prefix, comp=comp, &
      attributeName=attributeName, convention=convention, purpose=purpose, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    attributeName = "Date"
    call checkComponentAttribute(prefix, comp=comp, &
      attributeName=attributeName, convention=convention, purpose=purpose, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    attributeName = "PresentationForm"
    call checkComponentAttribute(prefix, comp=comp, &
      attributeName=attributeName, convention=convention, purpose=purpose, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    attributeName = "DOI"
    call checkComponentAttribute(prefix, comp=comp, &
      attributeName=attributeName, convention=convention, purpose=purpose, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

    attributeName = "ResponsiblePartyRole"
    call checkComponentAttribute(prefix, comp=comp, &
      attributeName=attributeName, convention=convention, purpose=purpose, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    attributeName = "Name"
    call checkComponentAttribute(prefix, comp=comp, &
      attributeName=attributeName, convention=convention, purpose=purpose, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    attributeName = "EmailAddress"
    call checkComponentAttribute(prefix, comp=comp, &
      attributeName=attributeName, convention=convention, purpose=purpose, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    attributeName = "PhysicalAddress"
    call checkComponentAttribute(prefix, comp=comp, &
      attributeName=attributeName, convention=convention, purpose=purpose, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    attributeName = "URL"
    call checkComponentAttribute(prefix, comp=comp, &
      attributeName=attributeName, convention=convention, purpose=purpose, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
  end subroutine
    
    
  recursive subroutine checkComponentAttribute(prefix, comp, attributeName, &
    convention, purpose, rc)
    character(*), intent(in)              :: prefix
    type(ESMF_GridComp)                   :: comp
    character(*), intent(in)              :: attributeName
    character(*), intent(in)              :: convention
    character(*), intent(in)              :: purpose
    integer,      intent(out), optional   :: rc
    
    character(10*ESMF_MAXSTR)             :: value
    character(ESMF_MAXSTR)                :: defaultvalue

    defaultvalue = "ComplianceICdefault"

    call ESMF_AttributeGet(comp, name=attributeName, value=value, &
      defaultvalue=defaultvalue, convention=convention, purpose=purpose, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (trim(value) == trim(defaultvalue)) then
      ! attribute not present
      call ESMF_LogWrite(trim(prefix)//" ==> Component level attribute: <"// &
        trim(attributeName)//"> is NOT present!", ESMF_LOG_WARNING, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else if (len_trim(value) == 0) then
      ! attribute present but not set
      call ESMF_LogWrite(trim(prefix)//" ==> Component level attribute: <"// &
        trim(attributeName)//"> is NOT set!", ESMF_LOG_WARNING, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      ! attribute present and set
      call ESMF_LogWrite(trim(prefix)//" Component level attribute: <"// &
        trim(attributeName)//"> is present and set.", ESMF_LOG_INFO, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    
  end subroutine

!-------------------------------------------------------------------------

  recursive subroutine checkFieldMetadata(prefix, field, rc)
    character(*), intent(in)              :: prefix
    type(ESMF_Field)                      :: field
    integer,      intent(out), optional   :: rc
    
    character(ESMF_MAXSTR)                :: attributeName
    character(ESMF_MAXSTR)                :: convention
    character(ESMF_MAXSTR)                :: purpose
      
    if (present(rc)) rc = ESMF_SUCCESS
    
    ! set CIM convention and purpose specifiers
    convention = "ESG"
    purpose = "General"
    
    call ESMF_LogWrite(trim(prefix)//" Field level attribute check: "// &
      "convention: '"//trim(convention)//"', purpose: '"//trim(purpose)//"'.", &
      ESMF_LOG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    attributeName = "ShortName"
    call checkFieldAttribute(prefix, field=field, &
      attributeName=attributeName, convention=convention, purpose=purpose, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    attributeName = "LongName"
    call checkFieldAttribute(prefix, field=field, &
      attributeName=attributeName, convention=convention, purpose=purpose, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    attributeName = "StandardName"
    call checkFieldAttribute(prefix, field=field, &
      attributeName=attributeName, convention=convention, purpose=purpose, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    attributeName = "Units"
    call checkFieldAttribute(prefix, field=field, &
      attributeName=attributeName, convention=convention, purpose=purpose, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    attributeName = "Intent"
    call checkFieldAttribute(prefix, field=field, &
      attributeName=attributeName, convention=convention, purpose=purpose, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
  end subroutine
    
    
  recursive subroutine checkFieldAttribute(prefix, field, attributeName, &
    convention, purpose, rc)
    character(*), intent(in)              :: prefix
    type(ESMF_Field)                      :: field
    character(*), intent(in)              :: attributeName
    character(*), intent(in)              :: convention
    character(*), intent(in)              :: purpose
    integer,      intent(out), optional   :: rc
    
    character(10*ESMF_MAXSTR)             :: value
    character(ESMF_MAXSTR)                :: defaultvalue

    defaultvalue = "ComplianceICdefault"

    call ESMF_AttributeGet(field, name=attributeName, value=value, &
      defaultvalue=defaultvalue, convention=convention, purpose=purpose, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (trim(value) == trim(defaultvalue)) then
      ! attribute not present
      call ESMF_LogWrite(trim(prefix)//" ==> Field level attribute: <"// &
        trim(attributeName)//"> is NOT present!", ESMF_LOG_WARNING, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else if (len_trim(value) == 0) then
      ! attribute present but not set
      call ESMF_LogWrite(trim(prefix)//" ==> Field level attribute: <"// &
        trim(attributeName)//"> is NOT set!", ESMF_LOG_WARNING, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      ! attribute present and set
      call ESMF_LogWrite(trim(prefix)//" Field level attribute: <"// &
        trim(attributeName)//"> is present and set.", ESMF_LOG_INFO, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    
  end subroutine

!-------------------------------------------------------------------------

  recursive subroutine clockUsageIncoming(prefix, clock, clockCopy, rc)
    character(*), intent(in)                :: prefix
    type(ESMF_Clock), intent(in)            :: clock
    type(ESMF_Clock), intent(inout)         :: clockCopy
    integer,          intent(out), optional :: rc
    
    logical                                 :: clockValid
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    clockValid = .true.
    ! Ensure that the Clock is a valid object
    if (ESMF_ClockGetInit(clock) /= ESMF_INIT_CREATED) then
      call ESMF_LogWrite(trim(prefix)//" ==> The incoming Clock is invalid!", &
        ESMF_LOG_WARNING, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      clockValid = .false.
    endif
    
    if (clockValid) then
      clockCopy = ESMF_ClockCreate(clock, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    
  end subroutine

!-------------------------------------------------------------------------

  recursive subroutine clockUsageOutgoing(prefix, clock, clockCopy, rc)
    character(*), intent(in)                :: prefix
    type(ESMF_Clock), intent(in)            :: clock
    type(ESMF_Clock), intent(inout)         :: clockCopy
    integer,          intent(out), optional :: rc
    
    logical                                 :: clockValid
    logical                                 :: clockModified
    
    character (ESMF_MAXSTR) :: name, nameCopy
    type(ESMF_TimeInterval) :: timeStep, timeStepCopy
    type(ESMF_Time)         :: startTime, startTimeCopy
    type(ESMF_Time)         :: stopTime, stopTimeCopy
    type(ESMF_TimeInterval) :: runDuration, runDurationCopy
    real(ESMF_KIND_R8)      :: runTimeStepCount, runTimeStepCountCopy
    type(ESMF_Time)         :: refTime, refTimeCopy
    type(ESMF_Time)         :: currTime, currTimeCopy
    integer(ESMF_KIND_I8)   :: advanceCount, advanceCountCopy
    type(ESMF_Direction)    :: direction, directionCopy

    if (present(rc)) rc = ESMF_SUCCESS
    
    clockValid = .true.
    ! Ensure that the Clock is a valid object
    if (ESMF_ClockGetInit(clock) /= ESMF_INIT_CREATED) clockValid = .false.
    
    if (clockValid) then
      clockModified = .false.
      
      call ESMF_ClockGet(clock, name=name, timeStep=timeStep, &
        startTime=startTime, stopTime=stopTime, runDuration=runDuration, &
        runTimeStepCount=runTimeStepCount, refTime=refTime, currTime=currTime, &
        advanceCount=advanceCount, direction=direction, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    
      call ESMF_ClockGet(clockCopy, name=nameCopy, timeStep=timeStepCopy, &
        startTime=startTimeCopy, stopTime=stopTimeCopy, runDuration=runDurationCopy, &
        runTimeStepCount=runTimeStepCountCopy, refTime=refTimeCopy, currTime=currTimeCopy, &
        advanceCount=advanceCountCopy, direction=directionCopy, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
        
      if (name /= nameCopy) clockModified = .true.
      if (timeStep /= timeStepCopy) clockModified = .true.
      if (startTime /= startTimeCopy) clockModified = .true.
      if (stopTime /= stopTimeCopy) clockModified = .true.
      if (runDuration /= runDurationCopy) clockModified = .true.
      if (runTimeStepCount /= runTimeStepCountCopy) clockModified = .true.
      if (refTime /= refTimeCopy) clockModified = .true.
      if (currTime /= currTimeCopy) clockModified = .true.
      if (advanceCount /= advanceCountCopy) clockModified = .true.
      if (direction /= directionCopy) clockModified = .true.
    
      if (clockModified) then
        call ESMF_LogWrite(trim(prefix)//" ==> The incoming Clock was modified!", &
          ESMF_LOG_WARNING, rc=rc)
        if (ESMF_LogFoundError(rc, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      else
        call ESMF_LogWrite(trim(prefix)//" The incoming Clock was not modified.", &
          ESMF_LOG_INFO, rc=rc)
        if (ESMF_LogFoundError(rc, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      
      call ESMF_ClockDestroy(clockCopy, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
        
    endif
    
  end subroutine
    
!-------------------------------------------------------------------------

  recursive subroutine checkInternalClock(prefix, comp, clock, mustReachStop, &
    rc)
    character(*), intent(in)                :: prefix
    type(ESMF_GridComp)                     :: comp
    type(ESMF_Clock), intent(in)            :: clock
    logical,          intent(in)            :: mustReachStop
    integer,          intent(out), optional :: rc
    
    logical                                 :: clockValid
    logical                                 :: clockInternalValid
    type(ESMF_Clock)                        :: clockInternal
    logical                                 :: clockMatch
    
    character (ESMF_MAXSTR) :: name, nameInt
    type(ESMF_TimeInterval) :: timeStep, timeStepInt
    type(ESMF_Time)         :: startTime, startTimeInt
    type(ESMF_Time)         :: stopTime, stopTimeInt
    type(ESMF_TimeInterval) :: runDuration, runDurationInt
    real(ESMF_KIND_R8)      :: runTimeStepCount, runTimeStepCountInt
    type(ESMF_Time)         :: refTime, refTimeInt
    type(ESMF_Time)         :: currTime, currTimeInt
    integer(ESMF_KIND_I8)   :: advanceCount, advanceCountInt
    type(ESMF_Direction)    :: direction, directionInt

    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_GridCompGet(comp, clock=clockInternal, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    clockInternalValid = .true.
    ! Ensure that the internalClock is a valid object
    if (ESMF_ClockGetInit(clockInternal) /= ESMF_INIT_CREATED) &
      clockInternalValid = .false.

    if (.not.clockInternalValid) then
      call ESMF_LogWrite(trim(prefix)//" ==> The internal Clock is invalid!", &
        ESMF_LOG_WARNING, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      return
    endif
    
    clockValid = .true.
    ! Ensure that the Clock is a valid object
    if (ESMF_ClockGetInit(clock) /= ESMF_INIT_CREATED) &
      clockValid = .false.
    
    if (.not.clockValid) then
      call ESMF_LogWrite(trim(prefix)//" ==> No Clock to compare internal Clock!", &
        ESMF_LOG_WARNING, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      return
    endif
      
    call ESMF_ClockGet(clock, name=name, timeStep=timeStep, &
      startTime=startTime, stopTime=stopTime, runDuration=runDuration, &
      runTimeStepCount=runTimeStepCount, refTime=refTime, currTime=currTime, &
      advanceCount=advanceCount, direction=direction, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_ClockGet(clockInternal, name=nameInt, timeStep=timeStepInt, &
      startTime=startTimeInt, stopTime=stopTimeInt, runDuration=runDurationInt, &
      runTimeStepCount=runTimeStepCountInt, refTime=refTimeInt, currTime=currTimeInt, &
      advanceCount=advanceCountInt, direction=directionInt, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    clockMatch = .true. ! initialize
      
    if (startTimeInt /= startTime) then
      call ESMF_LogWrite(trim(prefix)//" ==> startTime of internal Clock does not match Clock!", &
        ESMF_LOG_WARNING, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      clockMatch = .false.
    endif
      
    if (stopTimeInt /= stopTime) then
      call ESMF_LogWrite(trim(prefix)//" ==> stopTime of internal Clock does not match Clock!", &
        ESMF_LOG_WARNING, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      clockMatch = .false.
    endif
        
    if (clockMatch) then
      call ESMF_LogWrite(trim(prefix)//" The internal Clock matches incoming Clock.", &
        ESMF_LOG_INFO, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    if (mustReachStop) then
      if (currTimeInt /= stopTimeInt) then
        call ESMF_LogWrite(trim(prefix)//" ==> The internal Clock has not run to its stopTime!", &
          ESMF_LOG_WARNING, rc=rc)
        if (ESMF_LogFoundError(rc, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      else
        call ESMF_LogWrite(trim(prefix)//" The internal Clock has run to its stopTime.", &
          ESMF_LOG_INFO, rc=rc)
        if (ESMF_LogFoundError(rc, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
    endif
  
  end subroutine

!-------------------------------------------------------------------------


end module ESMF_ComplianceICMod
    

!-------------------------------------------------------------------------
! The register routine of internal ICs must be available as an external routine

recursive subroutine ESMF_ComplianceICRegister(comp, rc)
  use ESMF_Mod
  use ESMF_ComplianceICMod
  implicit none
  type(ESMF_GridComp)   :: comp
  integer               :: rc
  
  call registerIC(comp, rc)   ! simply call the internal IC module's register
  if (ESMF_LogFoundError(rc, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out
  
end subroutine
