! $Id: NUOPC_Model.F90,v 1.4 2012/10/31 00:04:42 theurich Exp $

#define FILENAME "src/addon/NUOPC/NUOPC_Model.F90"

module NUOPC_Model

  !-----------------------------------------------------------------------------
  ! Generic Model Component
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_ModelBase, only: &
    ModelBase_routine_SS            => routine_SetServices, &
    routine_Run                     => routine_Run, &
    type_InternalState              => type_InternalState, &
    type_InternalStateStruct        => type_InternalStateStruct, &
    label_InternalState             => label_InternalState, &
    label_Advance                   => label_Advance, &
    label_CheckImport               => label_CheckImport, &
    label_SetRunClock               => label_SetRunClock, &
    ModelBase_label_TimestampExport => label_TimestampExport

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
    label_DataInitialize, &
    label_SetClock, &
    label_SetRunClock
  
  character(*), parameter :: &
    label_DataInitialize = "Model_DataInitialize"
  character(*), parameter :: &
    label_SetClock = "Model_SetClock"
    
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine routine_SetServices(gcomp, rc)
    type(ESMF_GridComp)   :: gcomp
    integer, intent(out)  :: rc
    
    character(ESMF_MAXSTR):: name

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(gcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! SetServices of generic component deriving from
    call ModelBase_routine_SS(gcomp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    ! Override InitP3 -> compatibility checking
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP3, phase=3, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    
    ! Override InitP4 -> data initialize callback + initial time stamping
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP4, phase=4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    
    ! Override InitP5 -> data initialize callback + initial time stamping
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP5, phase=5, rc=rc)
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
    integer               :: localrc
    character(ESMF_MAXSTR):: name
    logical               :: clockIsPresent
    logical               :: existflag
    logical               :: allConnected
    integer               :: i
    character(ESMF_MAXSTR), pointer :: impStdNameList(:)
    character(ESMF_MAXSTR), pointer :: impItemNameList(:)
    character(ESMF_MAXSTR), pointer :: impConnectedList(:)
    
    rc = ESMF_SUCCESS

    nullify(impStdNameList)
    nullify(impItemNameList)
    nullify(impConnectedList)
    
    ! test whether internal Clock has already been set in the Component
    call ESMF_GridCompGet(gcomp, name=name, clockIsPresent=clockIsPresent, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      
    if (.not.clockIsPresent .and. NUOPC_IsCreated(clock)) then
      ! set the internal Clock as a copy of the incoming Clock by a default
      call NUOPC_GridCompSetClock(gcomp, clock, rc=rc)
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
    call NUOPC_StateBuildStdList(importState, stdAttrNameList=impStdNameList, &
      stdItemNameList=impItemNameList, stdConnectedList=impConnectedList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    allConnected = .true.  ! initialize
    if (associated(impConnectedList)) then
      do i=1, size(impConnectedList)
        if (impConnectedList(i) /= "true") then
          allConnected = .false.
          call ESMF_LogWrite(trim(name)//" - Import Field not connected: "// &
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
    call NUOPC_StateSetTimestamp(exportState, internalClock, rc=rc)
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
    character(ESMF_MAXSTR):: name
    integer               :: oldUpdatedCount, newUpdatedCount
    logical               :: allUpdated

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(gcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! check how many Fields in the exportState have the "Updated" Attribute set
    ! to "true" BEFORE calling the DataInitialize
    allUpdated = NUOPC_StateIsUpdated(exportState, count=oldUpdatedCount, rc=rc)
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
    
    ! check how many Fields in the exportState have the "Updated" Attribute set
    ! to "true" AFTER calling the DataInitialize
    allUpdated = NUOPC_StateIsUpdated(exportState, count=newUpdatedCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      
    ! deal with the "InitializeDataProgress" Attribute
    if (newUpdatedCount > oldUpdatedCount) then
      ! there are more Fields now that have their "Updated" Attriubute "true"
      call ESMF_AttributeSet(gcomp, &
        name="InitializeDataProgress", value="true", &
        convention="NUOPC", purpose="General", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    endif
    
    ! deal with "InitializeDataComplete" and setting timestamps
    if (allUpdated) then
      call ESMF_AttributeSet(gcomp, &
        name="InitializeDataComplete", value="true", &
        convention="NUOPC", purpose="General", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      ! update timestamp on all the export Fields
      call ESMF_GridCompGet(gcomp, clock=internalClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
      call NUOPC_StateSetTimestamp(exportState, internalClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
    else
      ! -> Still it may be that "InitializeDataComplete" should be set here,
      ! -> but only the Component code would know that, so it must be done
      ! -> inside of DataInitialize.
      ! update timestamp on only those export Fields that have Attribute "Updated"
      ! set to "true"
      call ESMF_GridCompGet(gcomp, clock=internalClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
      call NUOPC_StateSetTimestamp(exportState, internalClock, selective=.true., &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
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
    character(ESMF_MAXSTR):: name

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(gcomp, name=name, clock=clock, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    ! update timestamp on export Fields
    call NUOPC_StateSetTimestamp(exportState, clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    
  end subroutine
    
end module
