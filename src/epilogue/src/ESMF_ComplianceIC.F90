! $Id: ESMF_ComplianceIC.F90,v 1.3 2010/10/01 16:12:13 theurich Exp $
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
  
  public setvmIC, registerIC
        
  contains

!-------------------------------------------------------------------------
!   !  The setvm routine is used by the child component to set VM properties
!   !TODO:  currently the setvmIC() is _not_ hooked into the ESMF callback 

  subroutine setvmIC(comp, rc)
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
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
  subroutine registerIC(comp, rc)
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
    
    call ESMF_LogWrite(trim(prefix)//"Start registerIC.", ESMF_LOG_INFO, rc=rc)
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
        ESMF_LOG_ERROR, rc=rc)
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
        ESMF_LOG_ERROR, rc=rc)
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
        ESMF_LOG_ERROR, rc=rc)
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

    call ESMF_LogWrite(trim(prefix)//"Stop registerIC.", ESMF_LOG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine

!-------------------------------------------------------------------------
!   !   Initialization routine.
    
  subroutine ic_init(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    integer                 :: userrc
    character(ESMF_MAXSTR)  :: prefix
    character(ESMF_MAXSTR)  :: output
    
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

    call ESMF_LogWrite(trim(prefix)//"Stop InitializePrologue.", &
      ESMF_LOG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! Stop Compliance Checking: InitializePrologue
    !---------------------------------------------------------------------------

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
   
    !---------------------------------------------------------------------------
    ! Start Compliance Checking: InitializeEpilogue
    
    call ESMF_LogWrite(trim(prefix)//"Start InitializeEpilogue.", &
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

    ! Component Attributes should be set up -> ready to output
    call ESMF_AttributeWrite(comp, convention='CIM 1.0', &
      purpose='Model Component Simulation Description', &
      attwriteflag=ESMF_ATTWRITE_XML, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    
    call ESMF_LogWrite(trim(prefix)//"Stop InitializeEpilogue.", &
      ESMF_LOG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! Stop Compliance Checking: InitializePrologue
    !---------------------------------------------------------------------------

  end subroutine ic_init


!-------------------------------------------------------------------------
!   !  Run routine
 
  subroutine ic_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    integer                 :: userrc
    character(ESMF_MAXSTR)  :: prefix
    character(ESMF_MAXSTR)  :: output
    
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
    
    call ESMF_LogWrite(trim(prefix)//"Stop RunPrologue.", &
      ESMF_LOG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! Stop Compliance Checking: RunPrologue
    !---------------------------------------------------------------------------

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

    call ESMF_LogWrite(trim(prefix)//"Stop RunEpilogue.", &
      ESMF_LOG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! Stop Compliance Checking: RunPrologue
    !---------------------------------------------------------------------------

  end subroutine ic_run


!-------------------------------------------------------------------------
!   !  Finalize routine
 
  subroutine ic_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    integer                 :: userrc
    character(ESMF_MAXSTR)  :: prefix
    character(ESMF_MAXSTR)  :: output
    
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

    call ESMF_LogWrite(trim(prefix)//"Stop FinalizePrologue.", &
      ESMF_LOG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! Stop Compliance Checking: FinalizePrologue
    !---------------------------------------------------------------------------

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

    call ESMF_LogWrite(trim(prefix)//"Stop FinalizeEpilogue.", &
      ESMF_LOG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! Stop Compliance Checking: FinalizePrologue
    !---------------------------------------------------------------------------

  end subroutine ic_final


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! IC HELPER ROUTINES:
!-------------------------------------------------------------------------

  subroutine prefixString(comp, prefix, rc)
    type(ESMF_GridComp)         :: comp
    character(*), intent(inout) :: prefix
    integer, intent(out)        :: rc

    character(ESMF_MAXSTR) :: compName
    
    call ESMF_GridCompGet(comp, name=compName, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    prefix = "COMPLIANCECHECKER:"//trim(compName)//":"

  end subroutine    

!-------------------------------------------------------------------------

  subroutine checkState(prefix, referenceName, state, rc)
    character(*), intent(in)    :: prefix
    character(*), intent(in)    :: referenceName
    type(ESMF_State)            :: state
    integer, intent(out)        :: rc
      
    logical                               :: stateValid
    integer                               :: itemCount, item
    character(ESMF_MAXSTR)                :: name
    type(ESMF_StateType)                  :: statetype
    character(ESMF_MAXSTR)                :: tempString
    character(ESMF_MAXSTR), allocatable   :: itemNameList(:)
    type(ESMF_StateItemType), allocatable :: stateitemtypeList(:)

    stateValid = .true.
    ! Ensure that the State is a valid object
    if (ESMF_StateGetInit(state) /= ESMF_INIT_CREATED) then
      call ESMF_LogWrite(trim(prefix)//" ==> The "//trim(referenceName)// &
        " is invalid!", &
        ESMF_LOG_ERROR, rc=rc)
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
              " contains an ESMF_Array object", ESMF_LOG_ERROR, rc=rc)
            if (ESMF_LogFoundError(rc, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            write (tempString, *) item, " [ARRAY] name: "
          else if (stateitemtypeList(item) == ESMF_STATEITEM_ARRAYBUNDLE) then
            call ESMF_LogWrite(trim(prefix)//" ==> The "//trim(referenceName)// &
              " contains an ESMF_ArrayBundle object", ESMF_LOG_ERROR, rc=rc)
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
          
        enddo
        
        deallocate(stateitemtypeList)
        deallocate(itemNameList)
      ! Ensure that neither Arrays nor ArrayBundles are passed through the State
!                                itemNameList, stateitemtypeList, rc)
      endif
    endif      
  end subroutine



end module ESMF_ComplianceICMod
    

!-------------------------------------------------------------------------
! The register routine of internal ICs must be available as an external routine

subroutine ESMF_ComplianceICRegister(comp, rc)
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
