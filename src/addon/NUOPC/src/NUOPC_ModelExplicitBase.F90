! $Id: NUOPC_ModelExplicitBase.F90,v 1.6 2011/06/30 06:00:03 theurich Exp $

#define FILENAME "src/addon/NUOPC/NUOPC_ModelExplicitBase.F90"

module NUOPC_ModelExplicitBase

  !-----------------------------------------------------------------------------
  ! Generic Model Component for explicit integration
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC

  implicit none
  
  private
  
  public routine_SetServices
  public label_CheckImport, label_Advance, label_TimestampExport
  
  character(*), parameter :: &
    label_CheckImport = "ModelExplicitBase_CheckImport"
  character(*), parameter :: &
    label_Advance = "ModelExplicitBase_Advance"
  character(*), parameter :: &
    label_TimestampExport = "ModelExplicitBase_TimestampExport"

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine routine_SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=Noop, phase=2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=Noop, phase=3, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=Run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_FINALIZE, &
      userRoutine=Noop, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Noop(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine Run(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables    
    integer                 :: localrc
    type(ESMF_Clock)        :: internalClock
    logical                 :: allCurrent
    logical                 :: existflag
    character(ESMF_MAXSTR)  :: modelName

    rc = ESMF_SUCCESS
    
    call ESMF_GridCompGet(gcomp, name=modelName, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    ! check and set the internal clock against the external clock
    call NUOPC_GridCompCheckSetClock(gcomp, clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    ! get the internal clock for the time stepping loop
    call ESMF_GridCompGet(gcomp, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call NUOPC_ClockPrintCurrTime(internalClock, ">>>"// &
      trim(modelName)//" entered Run with current time: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    ! SPECIALIZE by calling into optional attached method
    call ESMF_MethodExecute(gcomp, label=label_CheckImport, &
      existflag=existflag, userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME, &
      rcToReturn=rc)) &
      return  ! bail out

    ! model time stepping loop
    do while (.not. ESMF_ClockIsStopTime(internalClock, rc=rc))
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, file=FILENAME)) return  ! bail out
        
      ! by default update the timestamp on Fields in exportState to the 
      ! currTime. This timestamp can then be overridded in Advance() or 
      ! in TimestampExport() after the timestepping loop.
      call NUOPC_StateSetTimestamp(exportState, internalClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      
      ! SPECIALIZE by calling into attached method to advance the model t->t+dt
      call ESMF_MethodExecute(gcomp, label=label_Advance, userRc=localrc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME, &
        rcToReturn=rc)) &
        return  ! bail out
        
      ! advance the internalClock to the new current time
      call ESMF_ClockAdvance(internalClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    
      call NUOPC_ClockPrintCurrTime(internalClock, &
        trim(modelName)//" time stepping loop, current time: ", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
        
    enddo ! end of time stepping loop
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    ! SPECIALIZE by calling into optional attached method
    call ESMF_MethodExecute(gcomp, label=label_TimestampExport, &
      existflag=existflag, userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME, &
      rcToReturn=rc)) &
      return  ! bail out

  end subroutine
  
end module
