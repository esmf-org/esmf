! $Id$
!
! Compliance Interface Component
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

#define FILENAME "ESMF_ComplianceIC.F90"

!-------------------------------------------------------------------------
! !DESCRIPTION:
!  Interface Component
!-------------------------------------------------------------------------


!TODO: make this macros available through ESMF as parameter or find other way
#define ESMF_INIT_CREATED 82949521

module ESMF_ComplianceICMod

  ! ESMF module
  use ESMF

  implicit none
  
  private
  
  integer, save :: ccfDepth = 1   ! component control flow depth
  integer, save :: maxDepth = -2  ! maximum depth of compliance checker
  
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
    
    print *, ">START setvmIC"

    ! This code is being executed _after_ the actual Component SetVM call
    
    !TODO: currently the setvmIC() is _not_ hooked into the ESMF callback 

    print *, ">STOP setvmIC"

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
    integer                 :: phaseCount, phase
    logical                 :: phaseZeroFlag
    
    ! Initialize user return code
    rc = ESMF_SUCCESS
    
    ! IMPORTANT: As an InterfaceComponent the code must ensure:
    ! 1) That the return code from the actual child method is returned to the 
    !    parent (note that this is not currently possible for the register)

    ! This code is being executed _after_ the actual Component Register call

    call prefixString(comp, prefix=prefix, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    ! Need to set the maxDepth variable unless it has already been set before
    if (maxDepth < -1) then
      call c_esmc_getComplianceCheckDepth(maxDepth, rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    endif

    !---------------------------------------------------------------------------
    ! Start Compliance Checking and IC method Registration
    
    if (ccfDepth <= maxDepth .or. maxDepth < 0) then

    call ESMF_LogWrite(trim(prefix)//">START register compliance check.", &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    ! check Initialize registration
    call ESMF_GridCompGetEPPhaseCount(comp, ESMF_METHOD_INITIALIZE, phaseCount, &
      phaseZeroFlag, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (phaseZeroFlag) then
      call ESMF_LogWrite(trim(prefix)//" phase ZERO for Initialize registered.",&
        ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZEIC, &
        userRoutine=ic_init, phase=0, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    else
      call ESMF_LogWrite(trim(prefix)//" ==> NUOPC requires Initialize phase ZERO!", &
        ESMF_LOGMSG_WARNING, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    endif
    if (phaseCount == 0) then
      call ESMF_LogWrite(trim(prefix)//" ==> No Initialize method registered!", &
        ESMF_LOGMSG_WARNING, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    else
      if (phaseZeroFlag) then
        write(output,*) " ",phaseCount," phase(s) of Initialize registered"// &
          " (not counting ZERO phase)."
      else
        write(output,*) " ",phaseCount," phase(s) of Initialize registered."
      endif
      call ESMF_LogWrite(trim(prefix)//trim(output), ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      do phase=1, phaseCount
        call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZEIC, &
          userRoutine=ic_init, phase=phase, rc=rc)
        if (ESMF_LogFoundError(rc, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
      enddo
    endif
    
    ! check Run registration
    call ESMF_GridCompGetEPPhaseCount(comp, ESMF_METHOD_RUN, phaseCount, &
      phaseZeroFlag, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (phaseZeroFlag) then
      call ESMF_LogWrite(trim(prefix)//" phase ZERO for Run registered.",&
        ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_RUNIC, &
        userRoutine=ic_run, phase=0, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    endif
    if (phaseCount == 0) then
      call ESMF_LogWrite(trim(prefix)//" ==> No Run method registered!", &
        ESMF_LOGMSG_WARNING, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    else
      if (phaseZeroFlag) then
        write(output,*) " ",phaseCount," phase(s) of Run registered"// &
          " (not counting ZERO phase)."
      else
        write(output,*) " ",phaseCount," phase(s) of Run registered."
      endif
      call ESMF_LogWrite(trim(prefix)//trim(output), ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      do phase=1, phaseCount
        call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_RUNIC, &
          userRoutine=ic_run, phase=phase, rc=rc)
        if (ESMF_LogFoundError(rc, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
      enddo
    endif

    ! check Finalize registration
    call ESMF_GridCompGetEPPhaseCount(comp, ESMF_METHOD_FINALIZE, phaseCount, &
      phaseZeroFlag, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (phaseZeroFlag) then
      call ESMF_LogWrite(trim(prefix)//" phase ZERO for Finalize registered.",&
        ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_FINALIZEIC, &
        userRoutine=ic_final, phase=0, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    endif
    if (phaseCount == 0) then
      call ESMF_LogWrite(trim(prefix)//" ==> No Finalize method registered!", &
        ESMF_LOGMSG_WARNING, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    else
      if (phaseZeroFlag) then
        write(output,*) " ",phaseCount," phase(s) of Finalize registered"// &
          " (not counting ZERO phase)."
      else
        write(output,*) " ",phaseCount," phase(s) of Finalize registered."
      endif
      call ESMF_LogWrite(trim(prefix)//trim(output), ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      do phase=1, phaseCount
        call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_FINALIZEIC, &
          userRoutine=ic_final, phase=phase, rc=rc)
        if (ESMF_LogFoundError(rc, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
      enddo
    endif

    call ESMF_LogWrite(trim(prefix)//">STOP register compliance check.", &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    endif
    ! Stop Compliance Checking
    !---------------------------------------------------------------------------

    
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
    integer                 :: phase
    
    ! Initialize user return code
    rc = ESMF_SUCCESS

    call prefixString(comp, prefix=prefix, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    call ESMF_GridCompGet(comp, currentPhase=phase, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    !---------------------------------------------------------------------------
    ! Start Compliance Checking: InitializePrologue
    if (ccfDepth <= maxDepth .or. maxDepth < 0) then
    
    write(output,*) ">START InitializePrologue for phase=", phase
    call ESMF_LogWrite(trim(prefix)//trim(output), &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    ! compliance check importState
    call checkState(prefix, referenceName="importState", state=importState, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    ! compliance check exportState
    call checkState(prefix, referenceName="exportState", state=exportState, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    ! compliance check clock usage
    call clockUsageIncoming(prefix, clock=clock, clockCopy=clockCopy, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    ! check Component statistics
    call checkComponentStatistics(prefix, comp=comp, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! compliance check Component metadata
    call checkComponentMetadata(prefix, comp=comp, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    write(output,*) ">STOP InitializePrologue for phase=", phase
    call ESMF_LogWrite(trim(prefix)//trim(output), &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    endif
    ! Stop Compliance Checking: InitializePrologue
    !---------------------------------------------------------------------------
    ccfDepth = ccfDepth + 1

    ! Call the actual Initialize routine
    call ESMF_GridCompInitializeAct(comp, importState, exportState, clock, &
      phase=phase, userRc=userrc, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
   
    ccfDepth = ccfDepth - 1
    !---------------------------------------------------------------------------
    ! Start Compliance Checking: InitializeEpilogue
    if (ccfDepth <= maxDepth .or. maxDepth < 0) then

    call prefixString(comp, prefix=prefix, forward=.false., rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    write(output,*) ">START InitializeEpilogue for phase=", phase
    call ESMF_LogWrite(trim(prefix)//trim(output), &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    ! check Component statistics
    call checkComponentStatistics(prefix, comp=comp, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! compliance check Component metadata
    call checkComponentMetadata(prefix, comp=comp, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    ! compliance check importState
    call checkState(prefix, referenceName="importState", state=importState, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    ! compliance check exportState
    call checkState(prefix, referenceName="exportState", state=exportState, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! compliance check clock usage
    call clockUsageOutgoing(prefix, clock=clock, clockCopy=clockCopy, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! compliance check internal Clock
    call checkInternalClock(prefix, comp=comp, clock=clock, &
      mustMatchCurr=.false., mustReachStop=.false., rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    ! Component Attributes should be set up -> ready to output
!    call ESMF_AttributeWrite(comp, convention='CIM 1.5', &
!      purpose='ModelComp', &
!      attwriteflag=ESMF_ATTWRITE_XML, rc=rc)
!    if (ESMF_LogFoundError(rc, &
!      line=__LINE__, &
!      file=FILENAME)) &
!      return  ! bail out
    
    
    write(output,*) ">STOP InitializeEpilogue for phase=", phase
    call ESMF_LogWrite(trim(prefix)//trim(output), &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    endif  
    ! Stop Compliance Checking: InitializeEpilogue
    !---------------------------------------------------------------------------
    
    ! if not bailed for other reasons then pass back the actual userrc
    rc = userrc

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
    integer                 :: phase
    
    ! Initialize user return code
    rc = ESMF_SUCCESS

    call prefixString(comp, prefix=prefix, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    call ESMF_GridCompGet(comp, currentPhase=phase, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    !---------------------------------------------------------------------------
    ! Start Compliance Checking: RunPrologue
    if (ccfDepth <= maxDepth .or. maxDepth < 0) then
    
    write(output,*) ">START RunPrologue for phase=", phase
    call ESMF_LogWrite(trim(prefix)//trim(output), &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    ! compliance check importState
    call checkState(prefix, referenceName="importState", state=importState, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    ! compliance check exportState
    call checkState(prefix, referenceName="exportState", state=exportState, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    ! compliance check clock usage
    call clockUsageIncoming(prefix, clock=clock, clockCopy=clockCopy, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! compliance check internal Clock
    call checkInternalClock(prefix, comp=comp, clock=clock, &
      mustMatchCurr=.true., mustReachStop=.false., rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    ! check Component statistics
    call checkComponentStatistics(prefix, comp=comp, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    write(output,*) ">STOP RunPrologue for phase=", phase
    call ESMF_LogWrite(trim(prefix)//trim(output), &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    endif
    ! Stop Compliance Checking: RunPrologue
    !---------------------------------------------------------------------------
    ccfDepth = ccfDepth + 1

    ! Call the actual Run routine
    call ESMF_GridCompRunAct(comp, importState, exportState, clock, &
      phase=phase, userRc=userrc, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ccfDepth = ccfDepth - 1
    !---------------------------------------------------------------------------
    ! Start Compliance Checking: RunEpilogue
    if (ccfDepth <= maxDepth .or. maxDepth < 0) then
    
    call prefixString(comp, prefix=prefix, forward=.false., rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    write(output,*) ">START RunEpilogue for phase=", phase
    call ESMF_LogWrite(trim(prefix)//trim(output), &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    ! check Component statistics
    call checkComponentStatistics(prefix, comp=comp, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! compliance check importState
    call checkState(prefix, referenceName="importState", state=importState, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    ! compliance check exportState
    call checkState(prefix, referenceName="exportState", state=exportState, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! compliance check clock usage
    call clockUsageOutgoing(prefix, clock=clock, clockCopy=clockCopy, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! compliance check internal Clock
    call checkInternalClock(prefix, comp=comp, clock=clock, &
      mustMatchCurr=.false., mustReachStop=.true., rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    write(output,*) ">STOP RunEpilogue for phase=", phase
    call ESMF_LogWrite(trim(prefix)//trim(output), &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    endif
    ! Stop Compliance Checking: RunEpilogue
    !---------------------------------------------------------------------------

    ! if not bailed for other reasons then pass back the actual userrc
    rc = userrc

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
    integer                 :: phase
    
    ! Initialize user return code
    rc = ESMF_SUCCESS

    call prefixString(comp, prefix=prefix, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    call ESMF_GridCompGet(comp, currentPhase=phase, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    !---------------------------------------------------------------------------
    ! Start Compliance Checking: FinalizePrologue
    if (ccfDepth <= maxDepth .or. maxDepth < 0) then
    
    write(output,*) ">START FinalizePrologue for phase=", phase
    call ESMF_LogWrite(trim(prefix)//trim(output), &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    ! compliance check importState
    call checkState(prefix, referenceName="importState", state=importState, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    ! compliance check exportState
    call checkState(prefix, referenceName="exportState", state=exportState, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! compliance check clock usage
    call clockUsageIncoming(prefix, clock=clock, clockCopy=clockCopy, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! check Component statistics
    call checkComponentStatistics(prefix, comp=comp, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    write(output,*) ">STOP FinalizePrologue for phase=", phase
    call ESMF_LogWrite(trim(prefix)//trim(output), &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    endif
    ! Stop Compliance Checking: FinalizePrologue
    !---------------------------------------------------------------------------
    ccfDepth = ccfDepth + 1

    ! Call the actual Finalize routine
    call ESMF_GridCompFinalizeAct(comp, importState, exportState, clock, &
      phase=phase, userRc=userrc, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ccfDepth = ccfDepth - 1
    !---------------------------------------------------------------------------
    ! Start Compliance Checking: FinalizeEpilogue
    if (ccfDepth <= maxDepth .or. maxDepth < 0) then
    
    call prefixString(comp, prefix=prefix, forward=.false., rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    write(output,*) ">START FinalizeEpilogue for phase=", phase
    call ESMF_LogWrite(trim(prefix)//trim(output), &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    ! check Component statistics
    call checkComponentStatistics(prefix, comp=comp, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! compliance check importState
    call checkState(prefix, referenceName="importState", state=importState, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    ! compliance check exportState
    call checkState(prefix, referenceName="exportState", state=exportState, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! compliance check clock usage
    call clockUsageOutgoing(prefix, clock=clock, clockCopy=clockCopy, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    write(output,*) ">STOP FinalizeEpilogue for phase=", phase
    call ESMF_LogWrite(trim(prefix)//trim(output), &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    endif
    ! Stop Compliance Checking: FinalizeEpilogue
    !---------------------------------------------------------------------------

    ! if not bailed for other reasons then pass back the actual userrc
    rc = userrc

  end subroutine ic_final


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! IC HELPER ROUTINES:
!-------------------------------------------------------------------------

  recursive subroutine prefixString(comp, prefix, forward, rc)
    type(ESMF_GridComp)                       :: comp
    character(*),       intent(inout)         :: prefix
    logical,            intent(in),  optional :: forward
    integer,            intent(out), optional :: rc

    character(ESMF_MAXSTR)  :: compName
    character(len=3)        :: arrow
    character(len=3)        :: forwardArrow
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    forwardArrow = "|->"
    arrow = "|->" ! default direction
    if (present(forward)) then
      if (.not.forward) arrow = "|<-"
    endif
    
    call ESMF_GridCompGet(comp, name=compName, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    prefix = "COMPLIANCECHECKER:"//repeat(forwardArrow, ccfDepth-1)//&
      arrow//":"//trim(compName)//":"

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
    type(ESMF_StateIntent_Flag)           :: stateintent
    character(ESMF_MAXSTR)                :: tempString
    character(ESMF_MAXSTR), allocatable   :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable :: stateitemtypeList(:)
    type(ESMF_Field), allocatable         :: fields(:)
    type(ESMF_Field)                      :: field
    type(ESMF_FieldBundle)                :: fieldbundle
    type(ESMF_State)                      :: nestedState
    character(ESMF_MAXSTR)                :: nestedPrefix
    character(ESMF_MAXSTR)                :: attributeName
    character(ESMF_MAXSTR)                :: convention
    character(ESMF_MAXSTR)                :: purpose

    if (present(rc)) rc = ESMF_SUCCESS

    stateValid = .true.
    ! Ensure that the State is a valid object
    if (ESMF_StateGetInit(state) /= ESMF_INIT_CREATED) then
      call ESMF_LogWrite(trim(prefix)//" ==> The "//trim(referenceName)// &
        " is invalid!", &
        ESMF_LOGMSG_WARNING, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      stateValid = .false.
    endif

    if (stateValid) then
      ! Provide name and type of State
      call ESMF_StateGet(state, name=name, stateintent=stateintent, &
        itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      call ESMF_LogWrite(trim(prefix)//" "//trim(referenceName)//" name: "// &
        trim(name), ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      if (stateintent==ESMF_STATEINTENT_IMPORT) then
        tempString = "ESMF_STATEINTENT_IMPORT"
      else if (stateintent==ESMF_STATEINTENT_EXPORT) then
        tempString = "ESMF_STATEINTENT_EXPORT"
      else if (stateintent==ESMF_STATEINTENT_UNSPECIFIED) then
        tempString = "ESMF_STATEINTENT_UNSPECIFIED"
      else
        tempString = "ESMF_STATEINTENT_INVALID"
      endif
      call ESMF_LogWrite(trim(prefix)//" "//trim(referenceName)//" stateintent: "// &
        trim(tempString), ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out

      ! set NUOPC convention and purpose specifiers
      convention = "NUOPC"
      purpose = "Instance"
      
      call ESMF_LogWrite(trim(prefix)//" State level attribute check: "// &
        "convention: '"//trim(convention)//"', purpose: '"//trim(purpose)//"'.", &
        ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out

      attributeName = "Namespace"
      call checkStateAttribute(prefix, state=state, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out

      write (tempString, *) itemCount
      call ESMF_LogWrite(trim(prefix)//" "//trim(referenceName)//" itemCount: "// &
        trim(tempString), ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out

      if (itemCount > 0) then
        allocate(itemNameList(itemCount))
        allocate(stateitemtypeList(itemCount))
        call ESMF_StateGet(state, itemNameList=itemNameList, &
          itemtypeList=stateitemtypeList, rc=rc)
        if (ESMF_LogFoundError(rc, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
          
        do item=1, itemCount
          if (stateitemtypeList(item) == ESMF_STATEITEM_FIELD) then
            write (tempString, *) item, " [FIELD] name: "
          else if (stateitemtypeList(item) == ESMF_STATEITEM_FIELDBUNDLE) then
            write (tempString, *) item, " [FIELDBUNDLE] name: "
          else if (stateitemtypeList(item) == ESMF_STATEITEM_ARRAY) then
            call ESMF_LogWrite(trim(prefix)//" ==> The "//trim(referenceName)// &
              " contains an ESMF_Array object!", ESMF_LOGMSG_WARNING, rc=rc)
            if (ESMF_LogFoundError(rc, &
              line=__LINE__, &
              file=FILENAME)) &
              return  ! bail out
            write (tempString, *) item, " [ARRAY] name: "
          else if (stateitemtypeList(item) == ESMF_STATEITEM_ARRAYBUNDLE) then
            call ESMF_LogWrite(trim(prefix)//" ==> The "//trim(referenceName)// &
              " contains an ESMF_ArrayBundle object!", ESMF_LOGMSG_WARNING, rc=rc)
            if (ESMF_LogFoundError(rc, &
              line=__LINE__, &
              file=FILENAME)) &
              return  ! bail out
            write (tempString, *) item, " [ARRAYBUNDLE] name: "
          else if (stateitemtypeList(item) == ESMF_STATEITEM_ROUTEHANDLE) then
            write (tempString, *) item, " [ROUTEHANDLE] name: "
          else if (stateitemtypeList(item) == ESMF_STATEITEM_STATE) then
            write (tempString, *) item, " [STATE] name: "
          else if (stateitemtypeList(item) == ESMF_STATEITEM_UNKNOWN) then
            write (tempString, *) item, " [UNKNOWN] name: "
          else if (stateitemtypeList(item) == ESMF_STATEITEM_NOTFOUND) then
            write (tempString, *) item, " [NOTFOUND] name: "
          endif
          
          call ESMF_LogWrite(trim(prefix)//" "//trim(referenceName)//" item #"// &
            trim(tempString)//trim(itemNameList(item)), &
            ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
          
          ! check metadata compliance            
          if (stateitemtypeList(item) == ESMF_STATEITEM_FIELD) then
            ! compliance check Field metadata
            call ESMF_StateGet(state, itemName=itemNameList(item), &
              field=field, rc=rc)
            if (ESMF_LogFoundError(rc, &
              line=__LINE__, &
              file=FILENAME)) &
              return  ! bail out
            call checkFieldMetadata(prefix, field=field, rc=rc)
            if (ESMF_LogFoundError(rc, &
              line=__LINE__, &
              file=FILENAME)) &
              return  ! bail out
          else if (stateitemtypeList(item) == ESMF_STATEITEM_FIELDBUNDLE) then
            call ESMF_StateGet(state, itemName=itemNameList(item), &
              fieldbundle=fieldbundle, rc=rc)
            if (ESMF_LogFoundError(rc, &
              line=__LINE__, &
              file=FILENAME)) &
              return  ! bail out
            call ESMF_FieldBundleGet(fieldbundle, fieldCount=fieldCount, rc=rc)
            if (ESMF_LogFoundError(rc, &
              line=__LINE__, &
              file=FILENAME)) &
              return  ! bail out
            allocate(fields(fieldCount))
            call ESMF_FieldBundleGet(fieldbundle, fieldList=fields, rc=rc)
            if (ESMF_LogFoundError(rc, &
              line=__LINE__, &
              file=FILENAME)) &
              return  ! bail out
            do fitem=1, fieldCount
              field = fields(fitem)
              call ESMF_FieldGet(field, name=name, rc=rc)
              if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
              call ESMF_LogWrite(trim(prefix)//" in FieldBundle, Field name: "//&
                trim(name), ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
              call checkFieldMetadata(prefix, field=field, rc=rc)
              if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
            enddo
            deallocate(fields)
          else if (stateitemtypeList(item) == ESMF_STATEITEM_STATE) then
            ! recursive call
            call ESMF_StateGet(state, itemName=itemNameList(item), &
              nestedState=nestedState, rc=rc)
            if (ESMF_LogFoundError(rc, &
              line=__LINE__, &
              file=FILENAME)) &
              return  ! bail out
            nestedPrefix = trim(prefix)//trim(itemNameList(item))//":"
            call checkState(nestedPrefix, referenceName, nestedState, rc=rc)
            if (ESMF_LogFoundError(rc, &
              line=__LINE__, &
              file=FILENAME)) &
              return  ! bail out
          endif
          
        enddo
        
        deallocate(stateitemtypeList)
        deallocate(itemNameList)
      endif
    endif      
  end subroutine

  recursive subroutine checkStateAttribute(prefix, state, attributeName, &
    convention, purpose, rc)
    character(*), intent(in)              :: prefix
    type(ESMF_State)                      :: state
    character(*), intent(in)              :: attributeName
    character(*), intent(in)              :: convention
    character(*), intent(in)              :: purpose
    integer,      intent(out), optional   :: rc
    
    type(ESMF_AttPack)                    :: attpack
    type(ESMF_TypeKind_Flag)              :: typekind
    integer                               :: itemCount, i
    logical                               :: isPresent
    character(10*ESMF_MAXSTR), pointer    :: valueStringList(:)
    character(ESMF_MAXSTR)                :: iStr, vStr
    integer(ESMF_KIND_I4), pointer        :: valueI4List(:)

    call ESMF_AttributeGetAttPack(state, attpack=attpack, &
      convention=convention, purpose=purpose, isPresent=isPresent, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (.not.isPresent) then      
      ! attpack not present
      call ESMF_LogWrite(trim(prefix)//" ==> State level attpack: <"// &
        "convention: '"//trim(convention)//"', "// &
        "purpose: '"//trim(purpose)//"'> is NOT present!", &
        ESMF_LOGMSG_WARNING, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    endif
    call ESMF_AttributeGet(state, name=attributeName, attpack=attpack, &
      typekind=typekind, itemCount=itemCount, isPresent=isPresent, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (.not.isPresent) then      
      ! attribute not present
      call ESMF_LogWrite(trim(prefix)//" ==> State level attribute: <"// &
        trim(attributeName)//"> is NOT present!", &
        ESMF_LOGMSG_WARNING, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    else if (itemCount == 0) then
      ! attribute present but not set
      call ESMF_LogWrite(trim(prefix)//" ==> State level attribute: <"// &
        trim(attributeName)//"> present but NOT set!", &
        ESMF_LOGMSG_WARNING, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    else
      ! attribute present and set
      if (typekind == ESMF_TYPEKIND_CHARACTER) then
        allocate(valueStringList(itemCount))
        call ESMF_AttributeGet(state, name=attributeName, &
          valueList=valueStringList, &
          convention=convention, purpose=purpose, rc=rc)
        if (itemCount == 1) then
          ! single valued
          call ESMF_LogWrite(trim(prefix)//" State level attribute: <"// &
            trim(attributeName)//"> "// &
            "present and set: "// trim(valueStringList(1)), &
            ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
        else
          ! multi valued -> requires loop
          do i=1, itemCount
            write(iStr,*) i
            call ESMF_LogWrite(trim(prefix)//" State level attribute: <"// &
              trim(attributeName)//">["//trim(adjustl(iStr))//"] "// &
              "present and set: "// trim(valueStringList(i)), &
              ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rc, &
              line=__LINE__, &
              file=FILENAME)) &
              return  ! bail out
          enddo
        endif
        deallocate(valueStringList)
      else if (typekind == ESMF_TYPEKIND_I4) then
        allocate(valueI4List(itemCount))
        call ESMF_AttributeGet(state, name=attributeName, &
          valueList=valueI4List, &
          convention=convention, purpose=purpose, rc=rc)
        if (itemCount == 1) then
          ! single valued
          write(vStr,*) valueI4List(1)
          call ESMF_LogWrite(trim(prefix)//" State level attribute: <"// &
            trim(attributeName)//"> "// &
            "present and set: "// vStr, &
            ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
        else
          ! multi valued -> requires loop
          do i=1, itemCount
            write(iStr,*) i
            write(vStr,*) valueI4List(i)
            call ESMF_LogWrite(trim(prefix)//" State level attribute: <"// &
              trim(attributeName)//">["//trim(adjustl(iStr))//"] "// &
              "present and set: "// vStr, &
              ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rc, &
              line=__LINE__, &
              file=FILENAME)) &
              return  ! bail out
          enddo
        endif
        deallocate(valueI4List)
      else
        call ESMF_LogWrite(trim(prefix)//" State level attribute: <"// &
          trim(attributeName)//"> "// &
          "present and set: <unsupported data type>", &
          ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rc, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
      endif
    endif

  end subroutine

!-------------------------------------------------------------------------

  recursive subroutine checkComponentMetadata(prefix, comp, rc)
    character(*), intent(in)              :: prefix
    type(ESMF_GridComp)                   :: comp
    integer,      intent(out), optional   :: rc
    
    type(ESMF_CompType_Flag)              :: comptype
    character(ESMF_MAXSTR)                :: attributeName
    character(ESMF_MAXSTR)                :: convention
    character(ESMF_MAXSTR)                :: purpose
      
    if (present(rc)) rc = ESMF_SUCCESS
    
    ! get Component type and branch on it
    call ESMF_GridCompGet(comp, comptype=comptype, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    if (comptype == ESMF_COMPTYPE_GRID) then
    
      ! set NUOPC convention and purpose specifiers
      convention = "NUOPC"
      purpose = "Instance"
    
      call ESMF_LogWrite(trim(prefix)//" GridComp level attribute check: "// &
        "convention: '"//trim(convention)//"', purpose: '"//trim(purpose)//"'.", &
        ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out

      attributeName = "ShortName"
      call checkComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
        
      attributeName = "LongName"
      call checkComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
        
      attributeName = "Description"
      call checkComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
      return  ! bail out
      
      attributeName = "ModelType"
      call checkComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
        
      attributeName = "ReleaseDate"
      call checkComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
        
      attributeName = "PreviousVersion"
      call checkComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      
#if 0
! skip Citation* attributes as per Cecelia 10/05/10      
      attributeName = "ShortTitle"
      call checkComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
        
      attributeName = "LongTitle"
      call checkComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      
      attributeName = "Date"
      call checkComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      
      attributeName = "PresentationForm"
      call checkComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      
      attributeName = "DOI"
      call checkComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
#endif

      attributeName = "ResponsiblePartyRole"
      call checkComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
        
      attributeName = "Name"
      call checkComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
        
      attributeName = "EmailAddress"
      call checkComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
        
      attributeName = "PhysicalAddress"
      call checkComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
        
      attributeName = "URL"
      call checkComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
        
      attributeName = "Verbosity"
      call checkComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
        
      attributeName = "InitializePhaseMap"
      call checkComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
        
      attributeName = "InternalInitializePhaseMap"
      call checkComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
        
      attributeName = "RunPhaseMap"
      call checkComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
        
      attributeName = "FinalizePhaseMap"
      call checkComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
        
      attributeName = "NestingGeneration"
      call checkComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
        
      attributeName = "Nestling"
      call checkComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
        
      attributeName = "InitializeDataComplete"
      call checkComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
        
      attributeName = "InitializeDataProgress"
      call checkComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
        
    elseif (comptype == ESMF_COMPTYPE_CPL) then

      ! set NUOPC convention and purpose specifiers
      convention = "NUOPC"
      purpose = "Instance"
    
      call ESMF_LogWrite(trim(prefix)//" CplComp level attribute check: "// &
        "convention: '"//trim(convention)//"', purpose: '"//trim(purpose)//"'.", &
        ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out

      attributeName = "ComponentLongName"
      call checkComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
        
      attributeName = "Verbosity"
      call checkComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
        
      attributeName = "InitializePhaseMap"
      call checkComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
        
      attributeName = "RunPhaseMap"
      call checkComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
        
      attributeName = "FinalizePhaseMap"
      call checkComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
        
      attributeName = "CplList"
      call checkComponentAttribute(prefix, comp=comp, &
        attributeName=attributeName, convention=convention, purpose=purpose, &
        rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
        
    else
      ! currently there is no other type by GridComp or CplComp
    endif
      
  end subroutine
    
  recursive subroutine checkComponentAttribute(prefix, comp, attributeName, &
    convention, purpose, rc)
    character(*), intent(in)              :: prefix
    type(ESMF_GridComp)                   :: comp
    character(*), intent(in)              :: attributeName
    character(*), intent(in)              :: convention
    character(*), intent(in)              :: purpose
    integer,      intent(out), optional   :: rc
    
    type(ESMF_AttPack)                    :: attpack
    type(ESMF_TypeKind_Flag)              :: typekind
    integer                               :: itemCount, i
    logical                               :: isPresent
    character(10*ESMF_MAXSTR), pointer    :: valueStringList(:)
    character(ESMF_MAXSTR)                :: iStr, vStr
    integer(ESMF_KIND_I4), pointer        :: valueI4List(:)

    call ESMF_AttributeGetAttPack(comp, attpack=attpack, &
      convention=convention, purpose=purpose, isPresent=isPresent, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (.not.isPresent) then      
      ! attpack not present
      call ESMF_LogWrite(trim(prefix)//" ==> Component level attpack: <"// &
        "convention: '"//trim(convention)//"', "// &
        "purpose: '"//trim(purpose)//"'> is NOT present!", &
        ESMF_LOGMSG_WARNING, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    endif
    call ESMF_AttributeGet(comp, name=attributeName, attpack=attpack, &
      typekind=typekind, itemCount=itemCount, isPresent=isPresent, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (.not.isPresent) then      
      ! attribute not present
      call ESMF_LogWrite(trim(prefix)//" ==> Component level attribute: <"// &
        trim(attributeName)//"> is NOT present!", &
        ESMF_LOGMSG_WARNING, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    else if (itemCount == 0) then
      ! attribute present but not set
      call ESMF_LogWrite(trim(prefix)//" ==> Component level attribute: <"// &
        trim(attributeName)//"> present but NOT set!", &
        ESMF_LOGMSG_WARNING, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    else
      ! attribute present and set
      if (typekind == ESMF_TYPEKIND_CHARACTER) then
        allocate(valueStringList(itemCount))
        call ESMF_AttributeGet(comp, name=attributeName, &
          valueList=valueStringList, &
          convention=convention, purpose=purpose, rc=rc)
        if (itemCount == 1) then
          ! single valued
          call ESMF_LogWrite(trim(prefix)//" Component level attribute: <"// &
            trim(attributeName)//"> "// &
            "present and set: "// trim(valueStringList(1)), &
            ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
        else
          ! multi valued -> requires loop
          do i=1, itemCount
            write(iStr,*) i
            call ESMF_LogWrite(trim(prefix)//" Component level attribute: <"// &
              trim(attributeName)//">["//trim(adjustl(iStr))//"] "// &
              "present and set: "// trim(valueStringList(i)), &
              ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rc, &
              line=__LINE__, &
              file=FILENAME)) &
              return  ! bail out
          enddo
        endif
        deallocate(valueStringList)
      else if (typekind == ESMF_TYPEKIND_I4) then
        allocate(valueI4List(itemCount))
        call ESMF_AttributeGet(comp, name=attributeName, &
          valueList=valueI4List, &
          convention=convention, purpose=purpose, rc=rc)
        if (itemCount == 1) then
          ! single valued
          write(vStr,*) valueI4List(1)
          call ESMF_LogWrite(trim(prefix)//" Component level attribute: <"// &
            trim(attributeName)//"> "// &
            "present and set: "// vStr, &
            ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
        else
          ! multi valued -> requires loop
          do i=1, itemCount
            write(iStr,*) i
            write(vStr,*) valueI4List(i)
            call ESMF_LogWrite(trim(prefix)//" Component level attribute: <"// &
              trim(attributeName)//">["//trim(adjustl(iStr))//"] "// &
              "present and set: "// vStr, &
              ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rc, &
              line=__LINE__, &
              file=FILENAME)) &
              return  ! bail out
          enddo
        endif
        deallocate(valueI4List)
      else
        call ESMF_LogWrite(trim(prefix)//" Component level attribute: <"// &
          trim(attributeName)//"> "// &
          "present and set: <unsupported data type>", &
          ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rc, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
      endif
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
    
    ! set NUOPC convention and purpose specifiers
    convention = "NUOPC"
    purpose = "Instance"
    
    call ESMF_LogWrite(trim(prefix)//" Field level attribute check: "// &
      "convention: '"//trim(convention)//"', purpose: '"//trim(purpose)//"'.", &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    attributeName = "StandardName"
    call checkFieldAttribute(prefix, field=field, &
      attributeName=attributeName, convention=convention, purpose=purpose, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    attributeName = "Units"
    call checkFieldAttribute(prefix, field=field, &
      attributeName=attributeName, convention=convention, purpose=purpose, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    attributeName = "LongName"
    call checkFieldAttribute(prefix, field=field, &
      attributeName=attributeName, convention=convention, purpose=purpose, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    attributeName = "ShortName"
    call checkFieldAttribute(prefix, field=field, &
      attributeName=attributeName, convention=convention, purpose=purpose, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    attributeName = "Intent"
    call checkFieldAttribute(prefix, field=field, &
      attributeName=attributeName, convention=convention, purpose=purpose, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    attributeName = "Connected"
    call checkFieldAttribute(prefix, field=field, &
      attributeName=attributeName, convention=convention, purpose=purpose, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    attributeName = "TimeStamp"
    call checkFieldAttribute(prefix, field=field, &
      attributeName=attributeName, convention=convention, purpose=purpose, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    attributeName = "ProducerConnection"
    call checkFieldAttribute(prefix, field=field, &
      attributeName=attributeName, convention=convention, purpose=purpose, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    attributeName = "ConsumerConnection"
    call checkFieldAttribute(prefix, field=field, &
      attributeName=attributeName, convention=convention, purpose=purpose, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    attributeName = "Updated"
    call checkFieldAttribute(prefix, field=field, &
      attributeName=attributeName, convention=convention, purpose=purpose, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    attributeName = "TransferOfferGeomObject"
    call checkFieldAttribute(prefix, field=field, &
      attributeName=attributeName, convention=convention, purpose=purpose, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    attributeName = "TransferActionGeomObject"
    call checkFieldAttribute(prefix, field=field, &
      attributeName=attributeName, convention=convention, purpose=purpose, &
      rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
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
    
    type(ESMF_AttPack)                    :: attpack
    type(ESMF_TypeKind_Flag)              :: typekind
    integer                               :: itemCount, i
    logical                               :: isPresent
    character(10*ESMF_MAXSTR), pointer    :: valueStringList(:)
    character(ESMF_MAXSTR)                :: iStr, vStr
    integer(ESMF_KIND_I4), pointer        :: valueI4List(:)

    call ESMF_AttributeGetAttPack(field, attpack=attpack, &
      convention=convention, purpose=purpose, isPresent=isPresent, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (.not.isPresent) then      
      ! attpack not present
      call ESMF_LogWrite(trim(prefix)//" ==> Field level attpack: <"// &
        "convention: '"//trim(convention)//"', "// &
        "purpose: '"//trim(purpose)//"'> is NOT present!", &
        ESMF_LOGMSG_WARNING, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    endif
    call ESMF_AttributeGet(field, name=attributeName, attpack=attpack, &
      typekind=typekind, itemCount=itemCount, isPresent=isPresent, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (.not.isPresent) then      
      ! attribute not present
      call ESMF_LogWrite(trim(prefix)//" ==> Field level attribute: <"// &
        trim(attributeName)//"> is NOT present!", &
        ESMF_LOGMSG_WARNING, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    else if (itemCount == 0) then
      ! attribute present but not set
      call ESMF_LogWrite(trim(prefix)//" ==> Field level attribute: <"// &
        trim(attributeName)//"> present but NOT set!", &
        ESMF_LOGMSG_WARNING, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    else
      ! attribute present and set
      if (typekind == ESMF_TYPEKIND_CHARACTER) then
        allocate(valueStringList(itemCount))
        call ESMF_AttributeGet(field, name=attributeName, &
          valueList=valueStringList, &
          convention=convention, purpose=purpose, rc=rc)
        if (itemCount == 1) then
          ! single valued
          call ESMF_LogWrite(trim(prefix)//" Field level attribute: <"// &
            trim(attributeName)//"> "// &
            "present and set: "// trim(valueStringList(1)), &
            ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
        else
          ! multi valued -> requires loop
          do i=1, itemCount
            write(iStr,*) i
            call ESMF_LogWrite(trim(prefix)//" Field level attribute: <"// &
              trim(attributeName)//">["//trim(adjustl(iStr))//"] "// &
              "present and set: "// trim(valueStringList(i)), &
              ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rc, &
              line=__LINE__, &
              file=FILENAME)) &
              return  ! bail out
          enddo
        endif
        deallocate(valueStringList)
      else if (typekind == ESMF_TYPEKIND_I4) then
        allocate(valueI4List(itemCount))
        call ESMF_AttributeGet(field, name=attributeName, &
          valueList=valueI4List, &
          convention=convention, purpose=purpose, rc=rc)
        if (itemCount == 1) then
          ! single valued
          write(vStr,*) valueI4List(1)
          call ESMF_LogWrite(trim(prefix)//" Field level attribute: <"// &
            trim(attributeName)//"> "// &
            "present and set: "// vStr, &
            ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
        else
          ! multi valued -> requires loop
          do i=1, itemCount
            write(iStr,*) i
            write(vStr,*) valueI4List(i)
            call ESMF_LogWrite(trim(prefix)//" Field level attribute: <"// &
              trim(attributeName)//">["//trim(adjustl(iStr))//"] "// &
              "present and set: "// vStr, &
              ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rc, &
              line=__LINE__, &
              file=FILENAME)) &
              return  ! bail out
          enddo
        endif
        deallocate(valueI4List)
      else
        call ESMF_LogWrite(trim(prefix)//" Field level attribute: <"// &
          trim(attributeName)//"> "// &
          "present and set: <unsupported data type>", &
          ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rc, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
      endif
    endif
    
  end subroutine

!-------------------------------------------------------------------------

  recursive subroutine clockUsageIncoming(prefix, clock, clockCopy, rc)
    character(*), intent(in)                :: prefix
    type(ESMF_Clock), intent(in)            :: clock
    type(ESMF_Clock), intent(inout)         :: clockCopy
    integer,          intent(out), optional :: rc
    
    type(ESMF_Pointer)                      :: clockThis
    logical                                 :: clockValid
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    clockValid = .true.
    ! Ensure that the Clock is a valid object
    ! Clock has deep C++ implementation, thus must also check this pointer here
    call ESMF_ClockGetThis(clock, clockThis, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if ((ESMF_ClockGetInit(clock) /= ESMF_INIT_CREATED) .or. &
      (clockThis == ESMF_NULL_POINTER)) then
      call ESMF_LogWrite(trim(prefix)//" ==> The incoming Clock is invalid!", &
        ESMF_LOGMSG_WARNING, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      clockValid = .false.
    endif
    
    if (clockValid) then
      clockCopy = ESMF_ClockCreate(clock, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    endif
    
  end subroutine

!-------------------------------------------------------------------------

  recursive subroutine clockUsageOutgoing(prefix, clock, clockCopy, rc)
    character(*), intent(in)                :: prefix
    type(ESMF_Clock), intent(in)            :: clock
    type(ESMF_Clock), intent(inout)         :: clockCopy
    integer,          intent(out), optional :: rc
    
    type(ESMF_Pointer)                      :: clockThis
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
    type(ESMF_Direction_Flag)    :: direction, directionCopy

    if (present(rc)) rc = ESMF_SUCCESS
    
    clockValid = .true.
    ! Ensure that the Clock is a valid object
    ! Clock has deep C++ implementation, thus must also check this pointer here
    call ESMF_ClockGetThis(clock, clockThis, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if ((ESMF_ClockGetInit(clock) /= ESMF_INIT_CREATED) .or. &
      (clockThis == ESMF_NULL_POINTER)) clockValid = .false.
    ! Further ensure that the clockCopy is a valid object
    ! Clock has deep C++ implementation, thus must also check this pointer here
    call ESMF_ClockGetThis(clockCopy, clockThis, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if ((ESMF_ClockGetInit(clockCopy) /= ESMF_INIT_CREATED) .or. &
      (clockThis == ESMF_NULL_POINTER)) clockValid = .false.
    
    if (clockValid) then
      clockModified = .false.
      
      call ESMF_ClockGet(clock, name=name, timeStep=timeStep, &
        startTime=startTime, stopTime=stopTime, runDuration=runDuration, &
        runTimeStepCount=runTimeStepCount, refTime=refTime, currTime=currTime, &
        advanceCount=advanceCount, direction=direction, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    
      call ESMF_ClockGet(clockCopy, name=nameCopy, timeStep=timeStepCopy, &
        startTime=startTimeCopy, stopTime=stopTimeCopy, runDuration=runDurationCopy, &
        runTimeStepCount=runTimeStepCountCopy, refTime=refTimeCopy, currTime=currTimeCopy, &
        advanceCount=advanceCountCopy, direction=directionCopy, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
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
          ESMF_LOGMSG_WARNING, rc=rc)
        if (ESMF_LogFoundError(rc, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
      else
        call ESMF_LogWrite(trim(prefix)//" The incoming Clock was not modified.", &
          ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rc, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
      endif
      
      call ESMF_ClockDestroy(clockCopy, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
        
    endif
    
  end subroutine
    
!-------------------------------------------------------------------------

  recursive subroutine checkInternalClock(prefix, comp, clock, mustMatchCurr, &
    mustReachStop, rc)
    character(*), intent(in)                :: prefix
    type(ESMF_GridComp)                     :: comp
    type(ESMF_Clock), intent(in)            :: clock
    logical,          intent(in)            :: mustMatchCurr
    logical,          intent(in)            :: mustReachStop
    integer,          intent(out), optional :: rc
    
    logical                                 :: clockValid
    logical                                 :: clockInternalValid
    type(ESMF_Clock)                        :: clockInternal
    logical                                 :: clockMatch
    logical                                 :: clockIsPresent
    
    character (ESMF_MAXSTR) :: name, nameInt
    type(ESMF_TimeInterval) :: timeStep, timeStepInt
    type(ESMF_Time)         :: startTime, startTimeInt
    type(ESMF_Time)         :: stopTime, stopTimeInt
    type(ESMF_TimeInterval) :: runDuration, runDurationInt
    real(ESMF_KIND_R8)      :: runTimeStepCount, runTimeStepCountInt
    type(ESMF_Time)         :: refTime, refTimeInt
    type(ESMF_Time)         :: currTime, currTimeInt
    integer(ESMF_KIND_I8)   :: advanceCount, advanceCountInt
    type(ESMF_Direction_Flag)    :: direction, directionInt

    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_GridCompGet(comp, clockIsPresent=clockIsPresent, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    if (.not.clockIsPresent) then

      call ESMF_LogWrite(trim(prefix)// &
        " ==> The internal Clock is not present!", &
        ESMF_LOGMSG_WARNING, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      return
      
    else
    
      call ESMF_GridCompGet(comp, clock=clockInternal, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    
      clockInternalValid = .true.
      ! Ensure that the internalClock is a valid object
      if (ESMF_ClockGetInit(clockInternal) /= ESMF_INIT_CREATED) &
        clockInternalValid = .false.

      if (.not.clockInternalValid) then
        call ESMF_LogWrite(trim(prefix)//" ==> The internal Clock is invalid!", &
          ESMF_LOGMSG_WARNING, rc=rc)
        if (ESMF_LogFoundError(rc, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
        return
      endif
      
    endif
    
    ! making it to hear means that the internal Clock is present and valid
    ! -> go and try to compare it to the passed in Clock object
    
    clockValid = .true.
    ! Ensure that the Clock is a valid object
    if (ESMF_ClockGetInit(clock) /= ESMF_INIT_CREATED) &
      clockValid = .false.
    
    if (.not.clockValid) then
      call ESMF_LogWrite(trim(prefix)//" ==> No Clock to compare internal Clock!", &
        ESMF_LOGMSG_WARNING, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      return
    endif
      
    call ESMF_ClockGet(clock, name=name, timeStep=timeStep, &
      startTime=startTime, stopTime=stopTime, runDuration=runDuration, &
      runTimeStepCount=runTimeStepCount, refTime=refTime, currTime=currTime, &
      advanceCount=advanceCount, direction=direction, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    call ESMF_ClockGet(clockInternal, name=nameInt, timeStep=timeStepInt, &
      startTime=startTimeInt, stopTime=stopTimeInt, runDuration=runDurationInt, &
      runTimeStepCount=runTimeStepCountInt, refTime=refTimeInt, currTime=currTimeInt, &
      advanceCount=advanceCountInt, direction=directionInt, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    clockMatch = .true. ! initialize
      
    if (startTimeInt /= startTime) then
      call ESMF_LogWrite(trim(prefix)//" ==> startTime of internal Clock does not match Clock!", &
        ESMF_LOGMSG_WARNING, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      clockMatch = .false.
    endif
    
    if (mustMatchCurr) then
      if (currTimeInt /= currTime) then
        call ESMF_LogWrite(trim(prefix)//" ==> currTime of internal Clock does not match Clock!", &
          ESMF_LOGMSG_WARNING, rc=rc)
        if (ESMF_LogFoundError(rc, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
        clockMatch = .false.
      endif
    endif
        
    if (clockMatch) then
      call ESMF_LogWrite(trim(prefix)//" The internal Clock matches incoming Clock.", &
        ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rc, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    endif

    if (mustReachStop) then
      if (currTimeInt /= stopTimeInt) then
        call ESMF_LogWrite(trim(prefix)//" ==> The internal Clock has not run to its stopTime!", &
          ESMF_LOGMSG_WARNING, rc=rc)
        if (ESMF_LogFoundError(rc, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
      else
        call ESMF_LogWrite(trim(prefix)//" The internal Clock has run to its stopTime.", &
          ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rc, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
      endif
    endif
  
  end subroutine

!-------------------------------------------------------------------------

  recursive subroutine checkComponentStatistics(prefix, comp, rc)
    character(*), intent(in)              :: prefix
    type(ESMF_GridComp)                   :: comp
    integer,      intent(out), optional   :: rc
  
    integer                 :: fobjCount, objCount
    integer                 :: virtMemPet, physMemPet
    character(ESMF_MAXSTR)  :: output
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    ! memory statistics for this PET
    call ESMF_VMGetMemInfo(virtMemPet, physMemPet, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    write (output, *) virtMemPet
    call ESMF_LogWrite(trim(prefix)//"ESMF Stats: "//&
      "the virtual memory used by this PET (in KB): "// &
      trim(adjustl(output)), ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
        
    write (output, *) physMemPet
    call ESMF_LogWrite(trim(prefix)//"ESMF Stats: "//&
    "the physical memory used by this PET (in KB): "// &
      trim(adjustl(output)), ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
        
    ! objects tracked by the ESMF garbage collection
    call ESMF_VMGetCurrentGarbageInfo(fobjCount, objCount, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    write (output, *) fobjCount
    call ESMF_LogWrite(trim(prefix)//"ESMF Stats: "//&
    "ESMF Fortran objects referenced by the ESMF garbage collection: "// &
      trim(adjustl(output)), ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
        
    write (output, *) objCount
    call ESMF_LogWrite(trim(prefix)//"ESMF Stats: "//&
    "ESMF Base objects (all C++, most Fortran) referenced by the ESMF garbage collection: "// &
      trim(adjustl(output)), ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

  end subroutine
    
!-------------------------------------------------------------------------

  recursive function zeroTerminatedString(string)
    logical :: zeroTerminatedString
    character(len=*), intent(in)        :: string
    integer(kind=selected_int_kind(2))  :: vbyte, mbyte
    
    zeroTerminatedString = .false. ! initialize
    vbyte = transfer(string, mbyte)
    
    print *, "In function zeroTerminatedString", vbyte
    
    if (vbyte==0) zeroTerminatedString = .true.
    
  end function
 
!-------------------------------------------------------------------------


end module ESMF_ComplianceICMod
    

!-------------------------------------------------------------------------
! The register routine of internal ICs must be available as an external routine

recursive subroutine ESMF_ComplianceICRegister(comp, rc)
  use ESMF
  use ESMF_ComplianceICMod
  implicit none
  type(ESMF_GridComp)   :: comp
  integer               :: rc
  
  call registerIC(comp, rc)   ! simply call the internal IC module's register
  if (ESMF_LogFoundError(rc, &
    line=__LINE__, &
    file=FILENAME)) &
    return  ! bail out
  
end subroutine
