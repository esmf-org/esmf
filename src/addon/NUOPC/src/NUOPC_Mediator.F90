! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research, 
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
    ModelBase_routine_SS            => SetServices, &
    routine_Run                     => routine_Run, &
    routine_Nop                     => routine_Nop, &
    label_Advance                   => label_Advance, &
    label_AdvanceClock              => label_AdvanceClock, &
    label_CheckImport               => label_CheckImport, &
    label_SetRunClock               => label_SetRunClock, &
    label_TimestampExport           => label_TimestampExport, &
    label_Finalize                  => label_Finalize, &
    NUOPC_ModelBaseGet

  implicit none
  
  private
  
  public &
    SetServices, &
    routine_Run
    
  public &
    label_Advance, &
    label_AdvanceClock, &
    label_CheckImport, &
    label_DataInitialize, &
    label_SetRunClock, &
    label_TimestampExport, &
    label_Finalize

  character(*), parameter :: &
    label_DataInitialize = "Mediator_DataInitialize"

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

    ! query the Component for info
    call ESMF_GridCompGet(gcomp, name=name, rc=rc)
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

    ! Initialize phases

    ! For backward compatibility with v6 API the sequence of the following
    ! NUOPC_CompSetEntryPoint() calls is critical to produce the old default
    ! InitializePhaseMap.

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p1", "IPDv01p1", "IPDv02p1", "IPDv03p1"/), &
      userRoutine=routine_Nop, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p2", "IPDv01p3", "IPDv02p3", "IPDv03p3"/), &
      userRoutine=routine_Nop, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p3", "IPDv01p4", "IPDv02p4", "IPDv03p6"/), &
      userRoutine=InitializeP3, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p4", "IPDv01p5"/), &
      userRoutine=InitializeP4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv02p5", "IPDv03p7"/), &
      userRoutine=InitializeP5, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP3(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    
    ! local variables
    character(ESMF_MAXSTR):: name
    logical               :: allConnected
    integer               :: i
    character(ESMF_MAXSTR), pointer :: impStdNameList(:)
    character(ESMF_MAXSTR), pointer :: impItemNameList(:)
    character(ESMF_MAXSTR), pointer :: impConnectedList(:)
    
    rc = ESMF_SUCCESS

    nullify(impStdNameList)
    nullify(impItemNameList)
    nullify(impConnectedList)

    ! get the Component name
    call ESMF_GridCompGet(gcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! set the internal clock to the parent clock
    call NUOPC_CompSetClock(gcomp, clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! query if all import Fields are connected
    call NUOPC_GetStateMemberLists(importState, StandardNameList=impStdNameList, &
      ConnectedList=impConnectedList, itemNameList=impItemNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    allConnected = .true.  ! initialize
    if (associated(impConnectedList)) then
      do i=1, size(impConnectedList)
        if (impConnectedList(i) /= "true") then
          allConnected = .false.
          call ESMF_LogWrite(trim(name)//": Import Field not connected: "// &
            trim(impStdNameList(i)), ESMF_LOGMSG_WARNING, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        endif
      enddo
    endif
    
    ! compatibility check
    if (.not.allConnected) then
      !TODO: introduce and use INCOMPATIBILITY return codes!!!!
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="NUOPC INCOMPATIBILITY DETECTED: Import Fields not all connected", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
      return  ! bail out
    endif
    
    !TODO: remove Fields that aren't connected from import and export State
    
    if (associated(impStdNameList)) deallocate(impStdNameList)
    if (associated(impItemNameList)) deallocate(impItemNameList)
    if (associated(impConnectedList)) deallocate(impConnectedList)
    
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
    logical               :: existflag
    character(ESMF_MAXSTR):: name

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(gcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! fill all export Fields with valid initial data for current time
    ! note that only connected Fields reside in exportState at this time
    ! SPECIALIZE by calling into attached method to fill initial data
    call ESMF_MethodExecute(gcomp, label=label_DataInitialize, &
      existflag=existflag, userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, &
      rcToReturn=rc)) &
      return  ! bail out
    
    ! update timestamp on all the export Fields
    call ESMF_GridCompGet(gcomp, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call NUOPC_UpdateTimestamp(exportState, internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------
  
  subroutine InitializeP5(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    
    ! local variables
    integer               :: localrc
    type(ESMF_Clock)      :: internalClock
    logical               :: existflag
    character(ESMF_MAXSTR):: name, oldDataComplete, newDataComplete
    integer               :: oldUpdatedCount, newUpdatedCount
    logical               :: allUpdated

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(gcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! check how many Fields in the exportState have the "Updated" Attribute set
    ! to "true" BEFORE calling the DataInitialize
    allUpdated = NUOPC_IsUpdated(exportState, count=oldUpdatedCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! get the value of the "InitializeDataComplete" Attribute
    call NUOPC_CompAttributeGet(gcomp, name="InitializeDataComplete", &
      value=oldDataComplete, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    
    ! Initialize component data structures, including its export Fields,
    ! only connected Fields reside in exportState at this time.
    ! Expect the component to set "InitializeDataComplete" Attribute when done.
    ! SPECIALIZE by calling into attached method to fill initial data
    call ESMF_MethodExecute(gcomp, label=label_DataInitialize, &
      existflag=existflag, userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, &
      rcToReturn=rc)) &
      return  ! bail out
    
    ! re-set the "InitializeDataProgress" Attribute to "false"
    call NUOPC_CompAttributeSet(gcomp, &
      name="InitializeDataProgress", value="false", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! check how many Fields in the exportState have the "Updated" Attribute set
    ! to "true" AFTER calling the DataInitialize
    allUpdated = NUOPC_IsUpdated(exportState, count=newUpdatedCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      
    ! see if number of updated export fields went up
    if (newUpdatedCount > oldUpdatedCount) then
      ! there are more Fields now that have their "Updated" Attribute set "true"
      ! -> set "InitializeDataProgress" Attribute "true"
      call NUOPC_CompAttributeSet(gcomp, &
        name="InitializeDataProgress", value="true", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    endif
    
    ! get the value of the "InitializeDataComplete" Attribute
    call NUOPC_CompAttributeGet(gcomp, name="InitializeDataComplete", &
      value=newDataComplete, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    
    ! see if the "InitializeDataComplete" Attribute has changed
    if (trim(newDataComplete) /= trim(oldDataComplete)) then
      ! there was a change in the "InitializeDataComplete" Attribute setting
      ! -> set "InitializeDataProgress" Attribute "true"
      call NUOPC_CompAttributeSet(gcomp, &
        name="InitializeDataProgress", value="true", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    endif
    
    ! correct setting of timestamps
    call ESMF_GridCompGet(gcomp, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    if (allUpdated) then
      ! update timestamp on all the export Fields
      call NUOPC_UpdateTimestamp(exportState, internalClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
    else
      ! update timestamp on only those export Fields that have the 
      ! "Updated" Attribute set to "true"
      call NUOPC_UpdateTimestamp(exportState, internalClock, &
        selective=.true., rc=rc)
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
    character(ESMF_MAXSTR)          :: name

    if (present(rc)) rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(mediator, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! query ModeBase
    call NUOPC_ModelBaseGet(mediator, driverClock=driverClock, &
      clock=mediatorClock, importState=importState, exportState=exportState, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

end module
