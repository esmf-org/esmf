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
#define FILENAME "src/addon/NUOPC/src/NUOPC_ModelBase.F90"
!==============================================================================

module NUOPC_ModelBase

  !-----------------------------------------------------------------------------
  ! Generic Model Component Base implementation
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC

  implicit none
  
  private
  
  public &
    SetServices, &
    routine_Run, &
    routine_Nop
 
  public &
    label_Advance, &
    label_AdvanceClock, &
    label_CheckImport, &
    label_SetRunClock, &
    label_TimestampExport, &
    label_Finalize
  
  character(*), parameter :: &
    label_InternalState = "ModelBase_InternalState"
  character(*), parameter :: &
    label_Advance = "ModelBase_Advance"
  character(*), parameter :: &
    label_AdvanceClock = "ModelBase_AdvanceClock"
  character(*), parameter :: &
    label_CheckImport = "ModelBase_CheckImport"
  character(*), parameter :: &
    label_SetRunClock = "ModelBase_SetRunClock"
  character(*), parameter :: &
    label_TimestampExport = "ModelBase_TimestampExport"
  character(*), parameter :: &
    label_Finalize = "ModelBase_Finalize"

  type type_InternalStateStruct
    type(ESMF_Clock)      :: driverClock
  end type

  type type_InternalState
    type(type_InternalStateStruct), pointer :: wrap
  end type

  ! Generic methods
  public NUOPC_ModelBaseGet

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    character(ESMF_MAXSTR)    :: name
    integer                   :: stat
    type(type_InternalState)  :: is

    rc = ESMF_SUCCESS
    
    ! query the Component for info
    call ESMF_GridCompGet(gcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! add standard NUOPC GridComp Attribute Package to the Model
    call NUOPC_CompAttributeInit(gcomp, kind="Model", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

#ifndef NO_COMP_SPECIFIC_COMPLIANCE_CHECK
    ! set the ESMF compliance checker register Attribute
    call ESMF_AttributeSet(gcomp, name="ESMF_RUNTIME_COMPLIANCEICREGISTER", &
      value="NUOPC_Model_ComplianceICR", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
#endif
    ! Initialize phases
    
    ! Phase 0 requires use of ESMF method.
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    ! Run phases
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"RunPhase1"/), userRoutine=routine_Run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    
    ! Specialize default Run -> setting the run Clock
    call ESMF_MethodAdd(gcomp, label=label_SetRunClock, &
      userRoutine=SetRunClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    ! Specialize default Run -> checking import Fields
    call ESMF_MethodAdd(gcomp, label=label_CheckImport, &
      userRoutine=CheckImport, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    ! Finalize phases
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_FINALIZE, &
      phaseLabelList=(/"FinalizePhase1"/), userRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
     
    ! Set up the internal state
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal state memory failed.", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call ESMF_UserCompSetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
        
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine routine_Nop(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    
    ! local variables
    character(ESMF_MAXSTR)    :: name
    type(type_InternalState)  :: is

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(gcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! filter all other entries but those of type IPDv00
    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv00p"/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! store the incoming clock as driverClock in internal state
    is%wrap%driverClock = clock
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine routine_Run(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    integer                   :: localrc
    type(type_InternalState)  :: is
    type(ESMF_Clock)          :: internalClock
    logical                   :: allCurrent
    logical                   :: existflag
    character(ESMF_MAXSTR)    :: modelName, msgString, valueString, pLabel
    integer                   :: phase
    integer                   :: verbosity
    character(ESMF_MAXSTR)    :: name

#define NUOPC_MODELBASE_TRACE__OFF
#ifdef NUOPC_MODELBASE_TRACE
    call ESMF_TraceRegionEnter("NUOPC_ModelBase:Run")
#endif
    
    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(gcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! determine verbosity
    call NUOPC_CompAttributeGet(gcomp, name="Verbosity", value=valueString, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    verbosity = ESMF_UtilString2Int(valueString, &
      specialStringList=(/"high", "max "/), specialValueList=(/255, 255/), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! get the modelName and currentPhase
    call ESMF_GridCompGet(gcomp, name=modelName, currentPhase=phase, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! store the incoming clock as driverClock in internal state
    is%wrap%driverClock = clock

#ifdef NUOPC_MODELBASE_TRACE
    call ESMF_TraceRegionEnter("NUOPC_ModelBase:SetRunClock")
#endif
    ! SPECIALIZE required: label_SetRunClock
    ! -> first check for the label with phase index
    call ESMF_MethodExecute(gcomp, label=label_SetRunClock, index=phase, &
      existflag=existflag, userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, &
      rcToReturn=rc)) &
      return  ! bail out
    if (.not.existflag) then
      ! -> next check for the label without phase index
      call ESMF_MethodExecute(gcomp, label=label_SetRunClock, &
        userRc=localrc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, &
        rcToReturn=rc)) &
        return  ! bail out
    endif
#ifdef NUOPC_MODELBASE_TRACE
    call ESMF_TraceRegionExit("NUOPC_ModelBase:SetRunClock")
#endif
    
    ! get the internal clock for the time stepping loop
    call ESMF_GridCompGet(gcomp, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    ! conditionally output diagnostic to Log file
    if (btest(verbosity,0)) then
      call NUOPC_CompSearchRevPhaseMap(gcomp, ESMF_METHOD_RUN, &
        phaseIndex=phase, phaseLabel=pLabel, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      call ESMF_ClockPrint(internalClock, options="currTime", &
        preString=">>>"//trim(modelName)//&
        " entered Run (phase="//trim(adjustl(pLabel))// &
        ") with current time: ", unit=msgString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

#ifdef NUOPC_MODELBASE_TRACE
    call ESMF_TraceRegionEnter("NUOPC_ModelBase:CheckImport")
#endif
    ! SPECIALIZE optionally: label_CheckImport
    ! -> first check for the label with phase index
    call ESMF_MethodExecute(gcomp, label=label_CheckImport, index=phase, &
      existflag=existflag, userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, &
      rcToReturn=rc)) &
      return  ! bail out
    if (.not.existflag) then
      ! -> next check for the label without phase index
      call ESMF_MethodExecute(gcomp, label=label_CheckImport, &
        existflag=existflag, userRc=localrc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, &
        rcToReturn=rc)) &
        return  ! bail out
   endif
#ifdef NUOPC_MODELBASE_TRACE
   call ESMF_TraceRegionExit("NUOPC_ModelBase:CheckImport")
#endif

    ! model time stepping loop
    do while (.not. ESMF_ClockIsStopTime(internalClock, rc=rc))
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        
      ! by default update the timestamp on Fields in exportState to the 
      ! currTime. This timestamp can then be overridded in Advance() or 
      ! in TimestampExport() after the timestepping loop.
      call NUOPC_UpdateTimestamp(exportState, internalClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

#ifdef NUOPC_MODELBASE_TRACE
      call ESMF_TraceRegionEnter("NUOPC_ModelBase:Advance")
#endif
      ! advance the model t->t+dt
      ! SPECIALIZE required: label_Advance
      ! -> first check for the label with phase index
      call ESMF_MethodExecute(gcomp, label=label_Advance, index=phase, &
        existflag=existflag, userRc=localrc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, &
        rcToReturn=rc)) &
        return  ! bail out
      if (.not.existflag) then
        ! -> next check for the label without phase index
        call ESMF_MethodExecute(gcomp, label=label_Advance, userRc=localrc, &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) &
          return  ! bail out
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, &
          rcToReturn=rc)) &
          return  ! bail out
      endif
#ifdef NUOPC_MODELBASE_TRACE
      call ESMF_TraceRegionExit("NUOPC_ModelBase:Advance")
#endif

#ifdef NUOPC_MODELBASE_TRACE
      call ESMF_TraceRegionEnter("NUOPC_ModelBase:AdvanceClock")
#endif
      ! advance the internalClock to the new current time (optionally specialz)
      call ESMF_MethodExecute(gcomp, label=label_AdvanceClock, index=phase, &
        existflag=existflag, userRc=localrc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, &
        rcToReturn=rc)) &
        return  ! bail out
      if (.not.existflag) then
        ! -> next check for the label without phase index
        call ESMF_MethodExecute(gcomp, label=label_AdvanceClock, &
          existflag=existflag, userRc=localrc, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) &
          return  ! bail out
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, &
          rcToReturn=rc)) &
          return  ! bail out
        if (.not.existflag) then
          ! at last use the DEFAULT implementation to advance the Clock
          call ESMF_ClockAdvance(internalClock, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) &
            return  ! bail out
        endif
     endif
#ifdef NUOPC_MODELBASE_TRACE
     call ESMF_TraceRegionExit("NUOPC_ModelBase:AdvanceClock")
#endif
    
      ! conditionally output diagnostic to Log file
      if (btest(verbosity,0)) then
        call ESMF_ClockPrint(internalClock, options="currTime", &
          preString=trim(modelName)//" time stepping loop, current time: ", &
          unit=msgString, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
        
    enddo ! end of time stepping loop
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

#ifdef NUOPC_MODELBASE_TRACE
    call ESMF_TraceRegionEnter("NUOPC_ModelBase:TimestampExport")
#endif
    ! SPECIALIZE optionally: label_TimestampExport
    ! -> first check for the label with phase index
    call ESMF_MethodExecute(gcomp, label=label_TimestampExport, index=phase, &
      existflag=existflag, userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, &
      rcToReturn=rc)) &
      return  ! bail out
    if (.not.existflag) then
      ! -> next check for the label without phase index
      call ESMF_MethodExecute(gcomp, label=label_TimestampExport, &
        existflag=existflag, userRc=localrc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, &
        rcToReturn=rc)) &
        return  ! bail out
   endif
#ifdef NUOPC_MODELBASE_TRACE
   call ESMF_TraceRegionExit("NUOPC_ModelBase:TimestampExport")
#endif

    ! conditionally output diagnostic to Log file
    if (btest(verbosity,0)) then
      call ESMF_ClockPrint(internalClock, options="currTime", &
        preString="<<<"//trim(modelName)//&
        " leaving Run (phase="//trim(adjustl(pLabel))// &
        ") with current time: ", unit=msgString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

#ifdef NUOPC_MODELBASE_TRACE
    call ESMF_TraceRegionExit("NUOPC_ModelBase:Run")
#endif
    
  end subroutine
  
  !-----------------------------------------------------------------------------
  
  subroutine SetRunClock(gcomp, rc)
    type(ESMF_GridComp)   :: gcomp
    integer, intent(out)  :: rc
    
    ! Set the model clock according to the incoming driver clock.
    ! Implement the default explicit timekeeping rules.
    
    ! local variables
    type(type_InternalState)  :: is
    character(ESMF_MAXSTR):: name

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(gcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! query component for its internal state
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    
    ! check and set the model clock against the driver clock
    call NUOPC_CompCheckSetClock(gcomp, is%wrap%driverClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
          
  end subroutine
    
  !-----------------------------------------------------------------------------
  
  subroutine CheckImport(gcomp, rc)
    type(ESMF_GridComp)   :: gcomp
    integer, intent(out)  :: rc
    
    ! This is the routine that enforces the explicit time dependence on the
    ! import fields. This simply means that the timestamps on the Fields in the
    ! importState are checked against the currentTime on the Component's 
    ! internalClock. Consequenty, this model starts out with forcing fields
    ! at the current time as it does its forward step from currentTime to 
    ! currentTime + timeStep.
    
    ! local variables
    type(ESMF_Clock)        :: clock
    type(ESMF_Time)         :: time
    type(ESMF_State)        :: importState
    logical                 :: allCurrent
    character(ESMF_MAXSTR):: name

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(gcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! query the Component for its clock and importState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    ! get the current time out of the clock
    call ESMF_ClockGet(clock, currTime=time, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
    
    ! check that Fields in the importState show correct timestamp
    allCurrent = NUOPC_IsAtTime(importState, time, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out
      
    if (.not.allCurrent) then
      !TODO: introduce and use INCOMPATIBILITY return codes!!!!
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="NUOPC INCOMPATIBILITY DETECTED: Import Fields not at current time", &
        line=__LINE__, file=trim(name)//":"//FILENAME, &
        rcToReturn=rc)
      return  ! bail out
    endif
    
  end subroutine
    
  !-----------------------------------------------------------------------------
  
  recursive subroutine Finalize(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    integer                   :: localrc, stat
    logical                   :: existflag
    character(ESMF_MAXSTR)    :: name
    type(type_InternalState)  :: is

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(gcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! SPECIALIZE by calling into optional attached method
    call ESMF_MethodExecute(gcomp, label=label_Finalize, existflag=existflag, &
      userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! deallocate internal state memory
    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_ModelBaseGet - Get info from a ModelBase
!
! !INTERFACE:
  subroutine NUOPC_ModelBaseGet(gcomp, driverClock, clock, importState, &
    exportState, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                        :: gcomp
    type(ESMF_Clock),    intent(out), optional :: driverClock
    type(ESMF_Clock),    intent(out), optional :: clock
    type(ESMF_State),    intent(out), optional :: importState
    type(ESMF_State),    intent(out), optional :: exportState
    integer,             intent(out), optional :: rc
!
! !DESCRIPTION:
!   Access ModelBase information.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is

    if (present(rc)) rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(gcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! driverClock
    if (present(driverClock)) then
      ! query Component for the internal State
      nullify(is%wrap)
      call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      driverClock = is%wrap%driverClock
    endif
    
    ! remaining arguments
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

end module
