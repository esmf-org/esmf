! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define FILENAME "src/addon/NUOPC/src/NUOPC_Mediator.F90"
!==============================================================================

module NUOPC_Mediator

  !-----------------------------------------------------------------------------
  ! Generic Mediator Component
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_ModelBase, only: &
    SetVM, &
    ModelBase_routine_SS            => SetServices, &
    routine_Run, &
    routine_Nop, &
    label_Advertise, &
    label_ModifyAdvertised, &
    label_RealizeProvided, &
    label_AcceptTransfer, &
    label_RealizeAccepted, &
    label_SetClock, &
    label_DataInitialize, &
    label_Advance, &
    label_AdvanceClock, &
    label_CheckImport, &
    label_SetRunClock, &
    label_TimestampExport, &
    label_Finalize, &
    type_InternalStateStruct, type_InternalState, label_InternalState, &
    NUOPC_ModelBaseGet

  implicit none
  
  private
  
  public &
    SetVM, &
    SetServices, &
    routine_Run
    
  public &
    label_Advertise, &
    label_ModifyAdvertised, &
    label_RealizeProvided, &
    label_AcceptTransfer, &
    label_RealizeAccepted, &
    label_SetClock, &
    label_DataInitialize, &
    label_Advance, &
    label_AdvanceClock, &
    label_CheckImport, &
    label_SetRunClock, &
    label_TimestampExport, &
    label_Finalize

  ! Generic methods
  public NUOPC_MediatorGet

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)   :: gcomp
    integer, intent(out)  :: rc

    ! local variables
    character(ESMF_MAXSTR):: name

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(gcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! Derive from ModelBase
    call NUOPC_CompDerive(gcomp, ModelBase_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    ! Identify this as a Mediator component kind
    call NUOPC_CompAttributeSet(gcomp, name="Kind", value="Mediator", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! Specialize Run -> timestamp export Fields
    call NUOPC_CompSpecialize(gcomp, specLabel=label_TimestampExport, &
      specRoutine=TimestampExport, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    ! Set IPDvX attribute
    call NUOPC_CompAttributeSet(gcomp, name="IPDvX", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine TimestampExport(gcomp, rc)
    type(ESMF_GridComp)   :: gcomp
    integer, intent(out)  :: rc

    ! local variables
    character(ESMF_MAXSTR)    :: name
    type(type_InternalState)  :: is

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(gcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! update timestamp on export Fields
    if (associated(is%wrap%cachedExportFieldList)) then
      call NUOPC_SetTimestamp(is%wrap%cachedExportFieldList, &
        is%wrap%preAdvanceCurrTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_MediatorGet - Get info from a Mediator
!
! !INTERFACE:
  subroutine NUOPC_MediatorGet(mediator, driverClock, mediatorClock, &
    importState, exportState, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                        :: mediator
    type(ESMF_Clock),    intent(out), optional :: driverClock
    type(ESMF_Clock),    intent(out), optional :: mediatorClock
    type(ESMF_State),    intent(out), optional :: importState
    type(ESMF_State),    intent(out), optional :: exportState
    integer,             intent(out), optional :: rc
!
! !DESCRIPTION:
!   Access Mediator information.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                         :: localrc
    character(ESMF_MAXSTR)          :: name

    if (present(rc)) rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(mediator, name=name, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out

    ! query ModeBase
    call NUOPC_ModelBaseGet(mediator, driverClock=driverClock, &
      clock=mediatorClock, importState=importState, exportState=exportState, &
      rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

end module
