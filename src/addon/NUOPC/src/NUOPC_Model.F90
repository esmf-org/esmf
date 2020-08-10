! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2020, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define FILENAME "src/addon/NUOPC/src/NUOPC_Model.F90"
!==============================================================================

module NUOPC_Model

  !-----------------------------------------------------------------------------
  ! Generic Model Component
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_ModelBase, only: &
    SetVM, &
    ModelBase_routine_SS            => SetServices, &
    routine_Run, &
    routine_Nop, &
    label_Advertise, &
    label_Realize, &
    label_Advance, &
    label_AdvanceClock, &
    label_CheckImport, &
    label_DataInitialize, &
    label_SetClock, &
    label_SetRunClock, &
    label_TimestampExport, &
    label_Finalize, &
    NUOPC_ModelBaseGet

  implicit none

  private

  public &
    SetVM, &
    SetServices, &
    routine_Run

  public &
    label_Advertise, &
    label_Realize, &
    label_Advance, &
    label_AdvanceClock, &
    label_CheckImport, &
    label_DataInitialize, &
    label_SetClock, &
    label_SetRunClock, &
    label_TimestampExport, &
    label_Finalize

  ! Generic methods
  public NUOPC_ModelGet

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
    type(ESMF_Clock)      :: clock
    type(ESMF_State)      :: exportState
    character(ESMF_MAXSTR):: name

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(gcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    ! get to the clock and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    ! update timestamp on export Fields
    call NUOPC_SetTimestamp(exportState, clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_ModelGet - Get info from a Model
!
! !INTERFACE:
  subroutine NUOPC_ModelGet(model, driverClock, modelClock, &
    importState, exportState, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                        :: model
    type(ESMF_Clock),    intent(out), optional :: driverClock
    type(ESMF_Clock),    intent(out), optional :: modelClock
    type(ESMF_State),    intent(out), optional :: importState
    type(ESMF_State),    intent(out), optional :: exportState
    integer,             intent(out), optional :: rc
!
! !DESCRIPTION:
!   Access Model information.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                         :: localrc
    character(ESMF_MAXSTR)          :: name

    if (present(rc)) rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(model, name=name, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out

    ! query ModeBase
    call NUOPC_ModelBaseGet(model, driverClock=driverClock, clock=modelClock, &
      importState=importState, exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

end module
