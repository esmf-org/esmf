! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research, 
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
    ModelBase_routine_SS            => SetServices, &
    routine_Run                     => routine_Run, &
    routine_Nop                     => routine_Nop, &
    label_Advance                   => label_Advance, &
    label_AdvanceClock              => label_AdvanceClock, &
    label_CheckImport               => label_CheckImport, &
    label_DataInitialize            => label_DataInitialize, &
    label_SetRunClock               => label_SetRunClock, &
    label_Finalize                  => label_Finalize, &
    ModelBase_label_TimestampExport => label_TimestampExport, &
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
    label_SetClock, &
    label_SetRunClock, &
    label_Finalize
  
  character(*), parameter :: &
    label_SetClock = "Model_SetClock"
    
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
      
    ! Initialize phases

    ! For backward compatibility with v6 API the sequence of the following
    ! NUOPC_CompSetEntryPoint() calls is critical to produce the old default
    ! InitializePhaseMap.

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p1", "IPDv01p1", "IPDv02p1", "IPDv03p1", &
      "IPDv04p1", "IPDv05p1"/), userRoutine=routine_Nop, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p2", "IPDv01p3", "IPDv02p3", "IPDv03p3", &
      "IPDv04p3", "IPDv05p4"/), userRoutine=routine_Nop, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p3", "IPDv01p4", "IPDv02p4", "IPDv03p6", &
      "IPDv04p6", "IPDv05p7"/), userRoutine=InitializeP3, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p4", "IPDv01p5"/), &
      userRoutine=InitializeP4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    ! Specialize Run -> timestamp export Fields
    call ESMF_MethodAdd(gcomp, label=ModelBase_label_TimestampExport, &
      userRoutine=TimestampExport, rc=rc)
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
    character(*), parameter   :: rName="InitializeP3"
    integer                   :: localrc
    character(ESMF_MAXSTR)    :: name
    logical                   :: clockIsPresent
    logical                   :: existflag
    logical                   :: allConnected
    integer                   :: i
    character(ESMF_MAXSTR), pointer :: impStdNameList(:)
    character(ESMF_MAXSTR), pointer :: impItemNameList(:)
    character(ESMF_MAXSTR), pointer :: impConnectedList(:)
    integer                   :: verbosity, diagnostic
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString
    
    rc = ESMF_SUCCESS

    nullify(impStdNameList)
    nullify(impItemNameList)
    nullify(impConnectedList)
    
    ! query the component for info
    call NUOPC_CompGet(gcomp, name=name, verbosity=verbosity, &
      diagnostic=diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! intro
    call NUOPC_LogIntro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle diagnostic
    if (diagnostic>0) then
      call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_TimePrint(currTime, unit=currTimeString, options="underscore", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    if (btest(diagnostic,0)) then
      call NUOPC_Write(importState, fileNamePrefix="diagnostic_"//&
        trim(name)//"_"//trim(rName)//"_enter_import_"//trim(currTimeString)//&
        "_", timeslice=1, status=ESMF_FILESTATUS_REPLACE, relaxedFlag=.true., &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    if (btest(diagnostic,1)) then
      call NUOPC_Write(exportState, fileNamePrefix="diagnostic_"//&
        trim(name)//"_"//trim(rName)//"_enter_export_"//trim(currTimeString)//&
        "_", timeslice=1, status=ESMF_FILESTATUS_REPLACE, relaxedFlag=.true., &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    
    ! test whether internal Clock has already been set in the Component
    call ESMF_GridCompGet(gcomp, clockIsPresent=clockIsPresent, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      
    if (.not.clockIsPresent .and. ESMF_ClockIsCreated(clock)) then
      ! set the internal Clock as a copy of the incoming Clock by a default
      call NUOPC_CompSetClock(gcomp, clock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif

    ! SPECIALIZE by calling into optional attached method to set internal clock
    call ESMF_MethodExecute(gcomp, label=label_SetClock, &
      existflag=existflag, userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out

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
    
    ! handle diagnostic
    if (btest(diagnostic,2)) then
      call NUOPC_Write(importState, fileNamePrefix="diagnostic_"//&
        trim(name)//"_"//trim(rName)//"_exit_import_"//trim(currTimeString)//&
        "_", timeslice=1, status=ESMF_FILESTATUS_REPLACE, relaxedFlag=.true., &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    if (btest(diagnostic,3)) then
      call NUOPC_Write(exportState, fileNamePrefix="diagnostic_"//&
        trim(name)//"_"//trim(rName)//"_exit_export_"//trim(currTimeString)//&
        "_", timeslice=1, status=ESMF_FILESTATUS_REPLACE, relaxedFlag=.true., &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    
    ! extro
    call NUOPC_LogExtro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP4(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    
    ! local variables
    character(*), parameter   :: rName="InitializeP4"
    integer                   :: localrc
    type(ESMF_Clock)          :: internalClock
    logical                   :: existflag
    character(ESMF_MAXSTR)    :: name
    integer                   :: verbosity, diagnostic
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(gcomp, name=name, verbosity=verbosity, &
      diagnostic=diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! intro
    call NUOPC_LogIntro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle diagnostic
    if (diagnostic>0) then
      call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_TimePrint(currTime, unit=currTimeString, options="underscore", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    if (btest(diagnostic,0)) then
      call NUOPC_Write(importState, fileNamePrefix="diagnostic_"//&
        trim(name)//"_"//trim(rName)//"_enter_import_"//trim(currTimeString)//&
        "_", timeslice=1, status=ESMF_FILESTATUS_REPLACE, relaxedFlag=.true., &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    if (btest(diagnostic,1)) then
      call NUOPC_Write(exportState, fileNamePrefix="diagnostic_"//&
        trim(name)//"_"//trim(rName)//"_enter_export_"//trim(currTimeString)//&
        "_", timeslice=1, status=ESMF_FILESTATUS_REPLACE, relaxedFlag=.true., &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    
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
    call NUOPC_SetTimestamp(exportState, internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    
    ! handle diagnostic
    if (btest(diagnostic,2)) then
      call NUOPC_Write(importState, fileNamePrefix="diagnostic_"//&
        trim(name)//"_"//trim(rName)//"_exit_import_"//trim(currTimeString)//&
        "_", timeslice=1, status=ESMF_FILESTATUS_REPLACE, relaxedFlag=.true., &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    if (btest(diagnostic,3)) then
      call NUOPC_Write(exportState, fileNamePrefix="diagnostic_"//&
        trim(name)//"_"//trim(rName)//"_exit_export_"//trim(currTimeString)//&
        "_", timeslice=1, status=ESMF_FILESTATUS_REPLACE, relaxedFlag=.true., &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    
    ! extro
    call NUOPC_LogExtro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

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
    character(ESMF_MAXSTR)          :: name

    if (present(rc)) rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(model, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! query ModeBase
    call NUOPC_ModelBaseGet(model, driverClock=driverClock, clock=modelClock, &
      importState=importState, exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

end module
