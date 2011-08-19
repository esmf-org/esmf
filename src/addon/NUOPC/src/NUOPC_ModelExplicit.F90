! $Id: NUOPC_ModelExplicit.F90,v 1.11 2011/08/19 17:35:56 theurich Exp $

#define FILENAME "src/addon/NUOPC/NUOPC_ModelExplicit.F90"

module NUOPC_ModelExplicit

  !-----------------------------------------------------------------------------
  ! Generic Model Component for explicit integration
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_ModelExplicitBase, only: &
    ModelExB_routine_SS             => routine_SetServices, &
    ModelExB_label_CheckImport      => label_CheckImport, &
    label_Advance                   => label_Advance, &
    ModelExB_label_TimestampExport  => label_TimestampExport

  implicit none
  
  private
  
  public routine_SetServices
  public label_DataInitialize, label_Advance, label_SetClock
  
  character(*), parameter :: &
    label_DataInitialize = "ModelExplicit_DataInitialize"
  character(*), parameter :: &
    label_SetClock = "ModelExplicit_SetClock"
    
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine routine_SetServices(gcomp, rc)
    type(ESMF_GridComp)   :: gcomp
    integer, intent(out)  :: rc
    
    rc = ESMF_SUCCESS
    
    ! SetServices of generic component deriving from
    call ModelExB_routine_SS(gcomp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! Override InitP2 -> compatibility checking
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP2, phase=2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    ! Override InitP3 -> data initialize callback + initial time stamping
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP3, phase=3, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    ! Specialize Run -> checking import Fields
    call ESMF_MethodAdd(gcomp, label=ModelExB_label_CheckImport, &
      userRoutine=CheckImport, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    ! Specialize Run -> timestamp export Fields
    call ESMF_MethodAdd(gcomp, label=ModelExB_label_TimestampExport, &
      userRoutine=TimestampExport, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP2(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    
    ! local variables    
    logical               :: allConnected
    integer               :: localrc
    logical               :: existflag
       
    rc = ESMF_SUCCESS
    
    ! by default set the internal clock to the parent clock, but only if
    ! internal clock wasn't already set
    call NUOPC_GridCompSetClock(gcomp, clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! SPECIALIZE by calling into optional attached method to set internal clock
    call ESMF_MethodExecute(gcomp, label=label_SetClock, &
      existflag=existflag, userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    ! query if all import Fields are connected
    allConnected = NUOPC_StateIsAllConnected(importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    ! compatibility check
    if (.not.allConnected) then
      !TODO: introduce and use INCOMPATIBILITY return codes!!!!
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="NUOPC INCOMPATIBILITY DETECTED: Import Fields not all connected", &
        line=__LINE__, &
        file=FILENAME, &
        rcToReturn=rc)
      return  ! bail out
    endif
    
    !TODO: remove Fields that aren't connected from import and export State
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP3(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    
    ! local variables    
    integer               :: localrc
    type(ESMF_Clock)      :: internalClock
    logical               :: allConnected
    logical               :: existflag
        
    rc = ESMF_SUCCESS
    
    ! fill all export Fields with valid initial data for current time
    ! note that only connected Fields reside in exportState at this time
    ! SPECIALIZE by calling into attached method to fill initial data
    call ESMF_MethodExecute(gcomp, label=label_DataInitialize, &
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
    
    ! update timestamp on export Fields
    call ESMF_GridCompGet(gcomp, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call NUOPC_StateSetTimestamp(exportState, internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------
  
  subroutine CheckImport(gcomp, rc)
    type(ESMF_GridComp)   :: gcomp
    integer, intent(out)  :: rc
    
    ! local variables
    type(ESMF_Clock)      :: clock
    type(ESMF_State)      :: importState
    logical               :: allCurrent

    rc = ESMF_SUCCESS
    
    ! query the Component for its clock and importState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! check that Fields in the importState show correct timestamp
    allCurrent = NUOPC_StateIsCurrentTimestamp(importState, clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    if (.not.allCurrent) then
      !TODO: introduce and use INCOMPATIBILITY return codes!!!!
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="NUOPC INCOMPATIBILITY DETECTED: Import Fields not at current time", &
        line=__LINE__, &
        file=FILENAME, &
        rcToReturn=rc)
      return  ! bail out
    endif
    
  end subroutine
    
  !-----------------------------------------------------------------------------
  
  subroutine TimestampExport(gcomp, rc)
    type(ESMF_GridComp)   :: gcomp
    integer, intent(out)  :: rc
    
    ! local variables
    type(ESMF_Clock)      :: clock
    type(ESMF_State)      :: exportState

    rc = ESMF_SUCCESS
    
    ! query the Component for its clock and importState
    call ESMF_GridCompGet(gcomp, clock=clock, exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! update timestamp on export Fields
    call NUOPC_StateSetTimestamp(exportState, clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
  end subroutine
    
end module
