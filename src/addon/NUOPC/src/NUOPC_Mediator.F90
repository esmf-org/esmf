! $Id: NUOPC_Mediator.F90,v 1.4 2012/10/30 20:51:02 theurich Exp $

#define FILENAME "src/addon/NUOPC/NUOPC_Mediator.F90"

module NUOPC_Mediator

  !-----------------------------------------------------------------------------
  ! Generic Mediator Component
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_ModelBase, only: &
    ModelBase_routine_SS            => routine_SetServices, &
    routine_Run                     => routine_Run, &
    type_InternalState              => type_InternalState, &
    type_InternalStateStruct        => type_InternalStateStruct, &
    label_InternalState             => label_InternalState, &
    label_CheckImport               => label_CheckImport, &
    label_Advance                   => label_Advance

  implicit none
  
  private
  
  public &
    routine_Run, &
    routine_SetServices
    
  public &
    type_InternalState, &
    type_InternalStateStruct
    
  public &
    label_InternalState, &
    label_Advance, &
    label_CheckImport, &
    label_DataInitialize

  character(*), parameter :: &
    label_DataInitialize = "Mediator_DataInitialize"

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine routine_SetServices(gcomp, rc)
    type(ESMF_GridComp)   :: gcomp
    integer, intent(out)  :: rc
    
    rc = ESMF_SUCCESS
    
    ! SetServices of generic component deriving from
    call ModelBase_routine_SS(gcomp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! Override InitP3 -> compatibility checking
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP3, phase=3, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    ! Override InitP4 -> data initialize callback + initial time stamping
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP4, phase=4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP3(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    
    ! local variables    
    logical               :: allConnected
        
    rc = ESMF_SUCCESS

    ! set the internal clock to the parent clock
    call NUOPC_GridCompSetClock(gcomp, clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

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

  subroutine InitializeP4(gcomp, importState, exportState, clock, rc)
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
  
end module
