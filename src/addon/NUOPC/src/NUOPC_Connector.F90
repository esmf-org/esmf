! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define FILENAME "src/addon/NUOPC/src/NUOPC_Connector.F90"
!==============================================================================

module NUOPC_Connector

  !-----------------------------------------------------------------------------
  ! Generic Coupler Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC

  implicit none
  
  private
  
  public SetVM, SetServices
  public label_ComputeRouteHandle, label_ExecuteRouteHandle, &
    label_ReleaseRouteHandle, label_Finalize
  
  character(*), parameter :: &
    label_InternalState = "Connector_InternalState"
  character(*), parameter :: &
    label_ComputeRouteHandle = "Connector_ComputeRH"
  character(*), parameter :: &
    label_ExecuteRouteHandle = "Connector_ExecuteRH"
  character(*), parameter :: &
    label_ReleaseRouteHandle = "Connector_ReleaseRH"
  character(*), parameter :: &
    label_Finalize = "Connector_Finalize"
    
  type type_UpdatePacket
    integer                             :: srcLocalPet
    integer                             :: dstLocalPet
    integer                             :: fieldCount
    integer, pointer                    :: fieldIndex(:)
    integer, pointer                    :: sendToPets(:)
    integer                             :: recvFromPet
    integer, pointer                    :: sendBuffer(:,:)
    integer, pointer                    :: recvBuffer(:,:)
    type(type_UpdatePacket), pointer    :: prev
  end type
  
  type type_CplSet
    integer                             :: count
    type(ESMF_FieldBundle)              :: srcFields
    type(ESMF_FieldBundle)              :: dstFields
    type(ESMF_Field), pointer           :: srcFieldList(:)
    type(ESMF_Field), pointer           :: dstFieldList(:)
    integer                             :: srcFieldCount
    integer                             :: dstFieldCount
    type(ESMF_RouteHandle)              :: rh
    type(ESMF_State)                    :: state
    type(ESMF_TermOrder_Flag), pointer  :: termOrders(:)
  end type

  type type_InternalStateStruct
    type(ESMF_FieldBundle)              :: srcFields
    type(ESMF_FieldBundle)              :: dstFields
    type(ESMF_Field), pointer           :: srcFieldList(:)
    type(ESMF_Field), pointer           :: dstFieldList(:)
    integer                             :: srcFieldCount
    integer                             :: dstFieldCount    
    type(ESMF_RouteHandle)              :: rh
    type(ESMF_State)                    :: state
    type(ESMF_TermOrder_Flag), pointer  :: termOrders(:)
    type(type_UpdatePacket), pointer    :: updatePackets
    integer                             :: cplSetCount
    character(ESMF_MAXSTR), pointer     :: cplSetList(:)
    type(type_CplSet), allocatable      :: cplSet(:)
    type(ESMF_VM)                       :: srcVM
    type(ESMF_VM)                       :: dstVM
    type(ESMF_Clock)                    :: driverClock
  end type

  type type_InternalState
    type(type_InternalStateStruct), pointer :: wrap
  end type

  ! Generic methods
  public NUOPC_ConnectorGet, NUOPC_ConnectorSet

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine SetVM(connector, rc)
    type(ESMF_CplComp)   :: connector
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS
  
  end subroutine

  !-----------------------------------------------------------------------------
  
  subroutine SetServices(connector, rc)
    type(ESMF_CplComp)   :: connector
    integer, intent(out) :: rc
    
    ! local variables
    character(ESMF_MAXSTR)    :: name
    integer                   :: stat
    type(type_InternalState)  :: is

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(connector, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! add standard NUOPC CplComp Attribute Package to the Connector
    call NUOPC_CompAttributeInit(connector, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

#ifndef NO_COMP_SPECIFIC_COMPLIANCE_CHECK
    ! set the ESMF compliance checker register Attribute
    call ESMF_AttributeSet(connector, name="ESMF_RUNTIME_COMPLIANCEICREGISTER", &
      value="NUOPC_Connector_ComplianceICR", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
#endif    

    ! allocate memory for the internal state and set it in the Component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal state memory failed.", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call ESMF_UserCompSetInternalState(connector, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    ! initialize internal state members that require sepcial treatment
    call ESMF_VMSetThis(is%wrap%srcVM, ESMF_NULL_POINTER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call ESMF_VMSetInitCreated(is%wrap%srcVM, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call ESMF_VMSetThis(is%wrap%dstVM, ESMF_NULL_POINTER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call ESMF_VMSetInitCreated(is%wrap%dstVM, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! Initialize phases
    
    ! Phase 0 requires use of ESMF method.
    call ESMF_CplCompSetEntryPoint(connector, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! Implement different IPD versions. By convention, the actual routines
    ! are named after the _highest_ IPD version they service. Make sure to
    ! rename routines when a new IPD version is introduced!
    call NUOPC_CompSetEntryPoint(connector, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv05p1"/), &
      userRoutine=InitializeIPDv05p1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call NUOPC_CompSetEntryPoint(connector, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p1", "IPDv01p1", "IPDv02p1", "IPDv03p1"/), &
      userRoutine=InitializeIPDv03p1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call NUOPC_CompSetEntryPoint(connector, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv01p2", "IPDv02p2", "IPDv03p2", "IPDv04p2", &
      "IPDv05p3"/), userRoutine=InitializeIPDv05p3, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call NUOPC_CompSetEntryPoint(connector, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p3", "IPDv04p3", "IPDv05p4"/), &
      userRoutine=InitializeIPDv05p4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call NUOPC_CompSetEntryPoint(connector, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p4", "IPDv04p4", "IPDv05p5"/), &
      userRoutine=InitializeIPDv05p5, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call NUOPC_CompSetEntryPoint(connector, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv01p3a", "IPDv02p3a", "IPDv03p5a", "IPDv04p5a", &
      "IPDv05p6a"/), userRoutine=InitializeIPDv05p6a, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call NUOPC_CompSetEntryPoint(connector, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv01p3b", "IPDv02p3b", "IPDv03p5b", "IPDv04p5b", &
      "IPDv05p6b"/), userRoutine=InitializeIPDv05p6b, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call NUOPC_CompSetEntryPoint(connector, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv04p1a", "IPDv05p2a"/), &
      userRoutine=InitializeIPDv05p2a, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call NUOPC_CompSetEntryPoint(connector, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv04p1b", "IPDv05p2b"/), &
      userRoutine=InitializeIPDv05p2b, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! setting IPDv00 phases here ensures backward compatibility with v6 API
    call NUOPC_CompSetEntryPoint(connector, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p2a"/), &
      userRoutine=InitializeIPDv00p2a, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call NUOPC_CompSetEntryPoint(connector, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p2b"/), &
      userRoutine=InitializeIPDv00p2b, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! Run phases
    call NUOPC_CompSetEntryPoint(connector, ESMF_METHOD_RUN, &
      phaseLabelList=(/"RunPhase1"/), userRoutine=Run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! Finalize phases
    call NUOPC_CompSetEntryPoint(connector, ESMF_METHOD_FINALIZE, &
      phaseLabelList=(/"FinalizePhase1"/), userRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP0(connector, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: connector
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    character(*), parameter   :: rName="InitializeP0"
    character(ESMF_MAXSTR)    :: name
    integer                   :: verbosity, diagnostic
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString
    type(type_InternalState)  :: is

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(connector, name=name, verbosity=verbosity, &
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
    
    ! filter all other entries but those of type IPDv05
    call NUOPC_CompFilterPhaseMap(connector, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv05p"/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(connector, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! store the incoming clock as driverClock in internal state
    is%wrap%driverClock = clock
    
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

  subroutine InitializeIPDv05p1(connector, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: connector
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    character(*), parameter   :: rName="InitializeIPDv05p1"
    character(ESMF_MAXSTR)    :: name
    character(ESMF_MAXSTR)    :: importXferPolicy, exportXferPolicy
    type(ESMF_VM)             :: vm
    integer                   :: verbosity, profiling, diagnostic
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(connector, name=name, verbosity=verbosity, &
      profiling=profiling, diagnostic=diagnostic, rc=rc)
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
    
    ! reconcile the States including Attributes
    if (btest(profiling,1)) then    ! PROFILE
      call ESMF_VMLogMemInfo("befP1 Reconcile")
    endif
    call NUOPC_Reconcile(importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call NUOPC_Reconcile(exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (btest(profiling,1)) then    ! PROFILE
      call ESMF_VMLogMemInfo("aftP1 Reconcile")
    endif

    ! get transfer policy for both states
    call NUOPC_GetAttribute(importState, name="FieldTransferPolicy", &
        value=importXferPolicy, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    call NUOPC_GetAttribute(exportState, name="FieldTransferPolicy", &
        value=exportXferPolicy, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    if (btest(verbosity,8)) then
      call ESMF_LogWrite(trim(name)//": importState FieldTransferPolicy = "// &
        trim(importXferPolicy), ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      call ESMF_LogWrite(trim(name)//": exportState FieldTransferPolicy = "// &
        trim(exportXferPolicy), ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif
    
    if (trim(exportXferPolicy)=="transferAll") then
      call NUOPC_ConnectorGet(connector, dstVM=vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      call doMirror(importState, exportState, acceptorVM=vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif
    if (trim(importXferPolicy)=="transferAll") then
      call NUOPC_ConnectorGet(connector, srcVM=vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      call doMirror(exportState, importState, acceptorVM=vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif

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

   contains

    recursive subroutine doMirror(providerState, acceptorState, acceptorVM, rc)
      type(ESMF_State)           :: providerState
      type(ESMF_State)           :: acceptorState
      type(ESMF_VM), intent(in)  :: acceptorVM       
      integer,       intent(out) :: rc

      integer                :: item, itemCount
      character(ESMF_MAXSTR) :: providerTransferOffer, acceptorTransferOffer
      character(ESMF_MAXSTR) :: acceptorStateName
      type(ESMF_State)       :: providerNestedState
      type(ESMF_State)       :: acceptorNestedState
      character(ESMF_MAXSTR) :: nestedStateName
      character(ESMF_MAXSTR) :: namespace
      character(ESMF_MAXSTR) :: cplSet
      integer                :: i, j
      character(ESMF_MAXSTR), allocatable     :: itemNameList(:)
      type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
      character(ESMF_MAXSTR), pointer       :: providerStandardNameList(:)
      character(ESMF_MAXSTR), pointer       :: acceptorStandardNameList(:)
      character(ESMF_MAXSTR), pointer       :: providerNamespaceList(:)
      character(ESMF_MAXSTR), pointer       :: acceptorNamespaceList(:)
      type(ESMF_Field),       pointer       :: providerFieldList(:)
      type(ESMF_Field),       pointer       :: acceptorFieldList(:)
      character(ESMF_MAXSTR), pointer       :: providerCplSetList(:)
      character(ESMF_MAXSTR), pointer       :: acceptorCplSetList(:)
      
      rc = ESMF_SUCCESS

      nullify(providerStandardNameList)
      nullify(providerNamespaceList)
      nullify(providerFieldList)
      nullify(providerCplSetList)
      nullify(acceptorStandardNameList)
      nullify(acceptorNamespaceList)
      nullify(acceptorFieldList)
      nullify(acceptorCplSetList)
      
      call ESMF_StateGet(acceptorState, name=acceptorStateName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out


      ! recursively duplicate nested states
      call ESMF_StateGet(providerState, nestedFlag=.false., &
        itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

      if (itemCount > 0) then
        allocate(itemNameList(itemCount))
        allocate(itemTypeList(itemCount))
        call ESMF_StateGet(providerState, nestedFlag=.false., &
          itemNameList=itemNameList, itemTypeList=itemTypeList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        do item=1, itemCount
          if (itemTypeList(item) == ESMF_STATEITEM_STATE) then
            call ESMF_StateGet(providerState, itemName=itemNameList(item), &
              nestedState=providerNestedState, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call ESMF_StateGet(providerNestedState, name=nestedStateName, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,8)) then
              call ESMF_LogWrite(trim(name)//": cloning nestedState: "// &
                trim(nestedStateName), ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) &
                return  ! bail out
            endif
            call NUOPC_GetAttribute(providerNestedState, name="Namespace", &
              value=namespace, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_GetAttribute(providerNestedState, name="CplSet", &
              value=cplSet, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_AddNestedState(acceptorState, Namespace=namespace, &
              CplSet=cplSet, nestedStateName=nestedStateName, &
              nestedState=acceptorNestedState, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call doMirror(providerState=providerNestedState, &
              acceptorState=acceptorNestedState, acceptorVM=acceptorVM, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          endif
        enddo

        deallocate(itemNameList)
        deallocate(itemTypeList)
      endif

      call NUOPC_GetStateMemberLists(providerState, providerStandardNameList, &
        fieldList=providerFieldList, namespaceList=providerNamespaceList, &
        cplSetList=providerCplSetList, nestedFlag=.false., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        
      call NUOPC_GetStateMemberLists(acceptorState, acceptorStandardNameList, &
        fieldList=acceptorFieldList, namespaceList=acceptorNamespaceList, &
        cplSetList=acceptorCplSetList, nestedFlag=.false., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      !TODO: does not currently deal with field bundles
      
      if (associated(providerStandardNameList)) then
        do i=1, size(providerStandardNameList)

          ! do not transfer if it already exists in the destination state
          if (associated(acceptorStandardNameList)) then
            do j=1, size(acceptorStandardNameList)
              if (trim(providerStandardNameList(i))&
                ==trim(acceptorStandardNameList(j))) exit
            enddo
            if (j<size(acceptorStandardNameList)+1) cycle
          endif

          ! reverse TransferOffer attribute, e.g., if a component
          ! providing a field wants to provide a grid, then the accepting
          ! component should not try to provide its own grid
          call NUOPC_GetAttribute(providerFieldList(i), &
            name="TransferOfferGeomObject", value=providerTransferOffer, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

          ! default
          acceptorTransferOffer = "cannot provide"
          if (trim(providerTransferOffer)=="will provide") then
            acceptorTransferOffer = "cannot provide"
          else if (trim(providerTransferOffer)=="can provide") then
            acceptorTransferOffer = "cannot provide"
          else if (trim(providerTransferOffer)=="cannot provide") then
            acceptorTransferOffer = "will provide"
          end if

          ! transfer to acceptorState
          if (btest(verbosity,8)) then
            call ESMF_LogWrite(trim(name)//": mirroring: "// &
              trim(providerStandardNameList(i))//" into acceptorState: "//&
              trim(acceptorStateName), ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) &
              return  ! bail out
          endif
          call NUOPC_Advertise(acceptorState, &
            StandardName=trim(providerStandardNameList(i)), &
            TransferOfferGeomObject=acceptorTransferOffer, vm=acceptorVM, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

        end do
      endif

      if (associated(providerStandardNameList)) &
        deallocate(providerStandardNameList)
      if (associated(providerNamespaceList)) deallocate(providerNamespaceList)
      if (associated(providerFieldList)) deallocate(providerFieldList)
      if (associated(providerCplSetList)) deallocate(providerCplSetList)
      if (associated(acceptorStandardNameList)) &
        deallocate(acceptorStandardNameList)
      if (associated(acceptorNamespaceList)) deallocate(acceptorNamespaceList)
      if (associated(acceptorFieldList)) deallocate(acceptorFieldList)
      if (associated(acceptorCplSetList)) deallocate(acceptorCplSetList)
    end subroutine

  end subroutine

  !-----------------------------------------------------------------------------
  
  subroutine InitializeIPDv05p2a(connector, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: connector
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    character(*), parameter               :: rName="InitializeIPDv05p2a"
    integer                               :: i, j
    integer                               :: bondLevel, bondLevelMax
    character(ESMF_MAXSTR)                :: name
    character(ESMF_MAXSTR), pointer       :: importStandardNameList(:)
    character(ESMF_MAXSTR), pointer       :: exportStandardNameList(:)
    type(ESMF_Field),       pointer       :: importFieldList(:)
    type(ESMF_Field),       pointer       :: exportFieldList(:)
    type(ESMF_Field)                      :: field
    character(ESMF_MAXSTR)                :: connectionString
    character(ESMF_MAXSTR), pointer       :: importNamespaceList(:)
    character(ESMF_MAXSTR), pointer       :: exportNamespaceList(:)
    character(ESMF_MAXSTR), pointer       :: importCplSetList(:)
    character(ESMF_MAXSTR), pointer       :: exportCplSetList(:)
    character(len=240)                    :: msgString
    logical                               :: match
    integer                   :: verbosity, profiling, diagnostic
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(connector, name=name, verbosity=verbosity, &
      profiling=profiling, diagnostic=diagnostic, rc=rc)
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

    ! reconcile the States including Attributes
    if (btest(profiling,1)) then    ! PROFILE
      call ESMF_VMLogMemInfo("befP1a Reconcile")
    endif
    call NUOPC_Reconcile(importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call NUOPC_Reconcile(exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (btest(profiling,1)) then    ! PROFILE
      call ESMF_VMLogMemInfo("aftP1a Reconcile")
    endif

    nullify(importStandardNameList)
    nullify(importFieldList)
    nullify(importNamespaceList)
    nullify(importCplSetList)
    nullify(exportStandardNameList)
    nullify(exportFieldList)
    nullify(exportNamespaceList)
    nullify(exportCplSetList)
    
    call NUOPC_GetStateMemberLists(importState, importStandardNameList, &
      fieldList=importFieldList, namespaceList=importNamespaceList, &
      cplSetList=importCplSetList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    call NUOPC_GetStateMemberLists(exportState, exportStandardNameList, &
      fieldList=exportFieldList, namespaceList=exportNamespaceList, &
      cplSetList=exportCplSetList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! associated pointers means that there are name lists
    if (associated(importStandardNameList) .and. &
      associated(exportStandardNameList)) then
      
      ! simple linear search of items that match between both lists
      do j=1, size(exportStandardNameList)  ! consumer side
        do i=1, size(importStandardNameList)  ! producer side
          match = NUOPC_FieldDictionaryMatchSyno( &
            importStandardNameList(i), exportStandardNameList(j), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (match) then
            ! found matching standard name pair
            ! -> determine bondLevel according to namespace matching
            bondLevel = &
              getBondLevel(importNamespaceList(i), exportNamespaceList(j), &
                importCplSetList(i), exportCplSetList(j))
            if (bondLevel == -1) cycle  ! break out and look for next match

            ! Getting to this place in the double loop means that the 
            ! standard name match has a connection that supports the match.
            
            ! -> get the current ProducerConnection bondLevel highmark
            field = exportFieldList(j)
            call NUOPC_GetAttribute(field, name="ProducerConnection", &
              value=connectionString, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) &
              return  ! bail out
            if (trim(connectionString)=="open") then
              ! first valid connection that was found
              write (connectionString, "(i10)") bondLevel
              call NUOPC_SetAttribute(field, name="ProducerConnection", &
                value=connectionString, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) &
                return  ! bail out
            else
              ! see if a new bondLevel highmark was found
              read (connectionString, "(i10)") bondLevelMax
              if (bondLevel > bondLevelMax) then
                write (connectionString, "(i10)") bondLevel
                call NUOPC_SetAttribute(field, name="ProducerConnection", &
                  value=connectionString, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//FILENAME)) &
                  return  ! bail out
              endif
            endif
            
            if (btest(verbosity,9)) then
              write (msgString,'(A, ": ", A30, I3, "): ", A60)') trim(name), &
                "importStandardNameList(i=", i, importStandardNameList(i)
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
              write (msgString,'(A, ": ", A30, I3, "): ", A60)') trim(name), &
                "importNamespaceList(i=", i, importNamespaceList(i)
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
              write (msgString,'(A, ": ", A30, I3, "): ", A60)') trim(name), &
                "importCplSetList(i=", i, importCplSetList(i)
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
              write (msgString,'(A, ": ", A30, I3, "): ", A60)') trim(name), &
                "exportStandardNameList(j=", j, exportStandardNameList(j)
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
              write (msgString,'(A, ": ", A30, I3, "): ", A60)') trim(name), &
                "exportNamespaceList(j=", j, exportNamespaceList(j)
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
              write (msgString,'(A, ": ", A30, I3, "): ", A60)') trim(name), &
                "exportCplSetList(j=", j, exportCplSetList(j)
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
              write (msgString,'(A, ": bondLevel=", I2)') trim(name), bondLevel
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
              write (msgString,&
                '(A, ": ProducerConnection (bondLevelMax):", A)') trim(name), &
                trim(connectionString)
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
            
          endif
        enddo
      enddo
      
    endif
    
    if (associated(importStandardNameList)) deallocate(importStandardNameList)
    if (associated(importFieldList)) deallocate(importFieldList)
    if (associated(importNamespaceList)) deallocate(importNamespaceList)
    if (associated(importCplSetList)) deallocate(importCplSetList)
    if (associated(exportStandardNameList)) deallocate(exportStandardNameList)
    if (associated(exportFieldList)) deallocate(exportFieldList)
    if (associated(exportNamespaceList)) deallocate(exportNamespaceList)
    if (associated(exportCplSetList)) deallocate(exportCplSetList)
    
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

  subroutine InitializeIPDv05p2b(connector, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: connector
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    character(*), parameter               :: rName="InitializeIPDv05p2b"
    integer                               :: i, j, count, maxCount
    integer                               :: bondLevel, bondLevelMax
    character(ESMF_MAXSTR)                :: name
    character(ESMF_MAXSTR), pointer       :: importStandardNameList(:)
    character(ESMF_MAXSTR), pointer       :: exportStandardNameList(:)
    type(ESMF_Field),       pointer       :: importFieldList(:)
    type(ESMF_Field),       pointer       :: exportFieldList(:)
    type(ESMF_Field)                      :: field
    character(ESMF_MAXSTR)                :: connectionString
    character(ESMF_MAXSTR), pointer       :: importNamespaceList(:)
    character(ESMF_MAXSTR), pointer       :: exportNamespaceList(:)
    character(ESMF_MAXSTR), pointer       :: importCplSetList(:)
    character(ESMF_MAXSTR), pointer       :: exportCplSetList(:)
    character(ESMF_MAXSTR), pointer       :: cplList(:)
    character(ESMF_MAXSTR), pointer       :: cplSetList(:)
    character(len=240)                    :: msgString
    logical                               :: match
    type(ESMF_StateIntent_Flag)           :: importStateIntent
    character(ESMF_MAXSTR)                :: fieldName
    integer                   :: verbosity, profiling, diagnostic
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString
    
    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(connector, name=name, verbosity=verbosity, &
      profiling=profiling, diagnostic=diagnostic, rc=rc)
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

    ! intro
    call NUOPC_LogIntro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      
    ! reconcile the States including Attributes
    if (btest(profiling,1)) then    ! PROFILE
      call ESMF_VMLogMemInfo("befP1b Reconcile")
    endif
    call NUOPC_Reconcile(importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call NUOPC_Reconcile(exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (btest(profiling,1)) then    ! PROFILE
      call ESMF_VMLogMemInfo("aftP1b Reconcile")
    endif

    ! set Attributes
    call NUOPC_CompAttributeSet(connector, &
      name="ComponentLongName", value="NUOPC Generic Connector Component", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! determine importStateIntent
    call ESMF_StateGet(importState, stateintent=importStateIntent, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! prepare to get lists out of States    
    nullify(importStandardNameList)
    nullify(importFieldList)
    nullify(importNamespaceList)
    nullify(importCplSetList)
    nullify(exportStandardNameList)
    nullify(exportFieldList)
    nullify(exportNamespaceList)
    nullify(exportCplSetList)
    
    call NUOPC_GetStateMemberLists(importState, importStandardNameList, &
      fieldList=importFieldList, namespaceList=importNamespaceList, &
      cplSetList=importCplSetList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      
    call NUOPC_GetStateMemberLists(exportState, exportStandardNameList, &
      fieldList=exportFieldList, namespaceList=exportNamespaceList, &
      cplSetList=exportCplSetList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! associated pointers means that there are name lists
    if (associated(importStandardNameList) .and. &
      associated(exportStandardNameList)) then
      
      ! the maximum number of matches is limited by the larger list, because
      ! the same producer can be matched to multiple consumers
      maxCount = max(size(importStandardNameList), size(exportStandardNameList))
      allocate(cplList(maxCount)) ! temporary list
      allocate(cplSetList(maxCount)) ! temporary list

      count = 0
      ! simple linear search of items that match between both lists
      do j=1, size(exportStandardNameList)  ! consumer side
        do i=1, size(importStandardNameList)  ! producer side
          match = NUOPC_FieldDictionaryMatchSyno( &
            importStandardNameList(i), exportStandardNameList(j), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (match) then
            ! found matching standard name pair
            ! -> determine bondLevel according to namespace matching
            bondLevel = &
              getBondLevel(importNamespaceList(i), exportNamespaceList(j), &
                importCplSetList(i), exportCplSetList(j))
              
            if (btest(verbosity,9)) then
              write (msgString,'(A, ": ", A30, I3, "): ", A60)') trim(name), &
                "importStandardNameList(i=", i, importStandardNameList(i)
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
              write (msgString,'(A, ": ", A30, I3, "): ", A60)') trim(name), &
                "importNamespaceList(i=", i, importNamespaceList(i)
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
              write (msgString,'(A, ": ", A30, I3, "): ", A60)') trim(name), &
                "importCplSetList(i=", i, importCplSetList(i)
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
              write (msgString,'(A, ": ", A30, I3, "): ", A60)') trim(name), &
                "exportStandardNameList(j=", j, exportStandardNameList(j)
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
              write (msgString,'(A, ": ", A30, I3, "): ", A60)') trim(name), &
                "exportNamespaceList(j=", j, exportNamespaceList(j)
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
              write (msgString,'(A, ": ", A30, I3, "): ", A60)') trim(name), &
                "exportCplSetList(j=", j, exportCplSetList(j)
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
              write (msgString,'(A, ": bondLevel=", I2)') trim(name), bondLevel
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif

            if (bondLevel == -1) cycle  ! break out and look for next match
                       
            ! Getting to this place in the double loop means that the 
            ! standard name match has a connection that supports the match.
            
            ! -> look at the current ProducerConnection entry to see what to do
            field = exportFieldList(j)
            call NUOPC_GetAttribute(field, name="ProducerConnection", &
              value=connectionString, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (index(trim(connectionString), "targeted:")==1) then
              ! this export field has already been targeted
              read (connectionString(10:len(connectionString)), "(i10)") &
                bondLevelMax  ! the bondLevel that was targeted
              if (bondLevel == bondLevelMax) then
                ! ambiguity detected -> check if this can be resolved
                if (importStateIntent==ESMF_STATEINTENT_IMPORT) then
                  ! importState is a component's importState, i.e. not a 
                  ! real producer, but a driver intermediary
                  ! -> obviously there are local producers that are available
                  ! -> not a problem, simply ignore here, i.e. nothing to be
                  !    added to this Connectors CplList
                  ! -> do not modify the importState though here, because other
                  !    Connectors may still need to interact with it. The driver
                  !    will take care of removing fields from its importState
                  !    when it is time to do so.
                else
                  ! importState is a model's exportState, i.e. real producer
                  ! cannot resolve that situation -> bail out
                  call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
                    msg="Ambiguous connection status, multiple connections "// &
                    "with identical bondLevel found for: "// &
                    trim(importStandardNameList(i)), &
                    line=__LINE__, file=trim(name)//":"//FILENAME, &
                    rcToReturn=rc)
                  return  ! bail out
                endif
              endif
            else
              ! obtain the bondLevel that needs to be targeted
              read (connectionString, "(i10)") bondLevelMax
              if (bondLevel == bondLevelMax) then
                ! the connection can be satisfied here
                count = count+1
                if (count > maxCount) then
                  call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
                    msg="Bad internal error - should never get here!",&
                    line=__LINE__, file=trim(name)//":"//FILENAME, &
                    rcToReturn=rc)
                  return  ! bail out
                endif
                cplList(count) = importStandardNameList(i)
                cplSetList(count) = importCplSetList(i)
                if (btest(verbosity,10)) then
                  write (msgString, '(A, ": added cplList(", I3, ")=", A, '//&
                    '", cplSet(", I3, ")=", A)') &
                    trim(name), count, trim(cplList(count)), &
                    count, trim(cplSetList(count))
                  call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
                  if (ESMF_LogFoundError(rcToCheck=rc, &
                    msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__, file=trim(name)//":"//FILENAME, &
                    rcToReturn=rc)) return  ! bail out
                endif
                ! make the targeted entry to the ProducerConnection attribute
                write (connectionString, "('targeted:', i10)") bondLevel
                call NUOPC_SetAttribute(field, name="ProducerConnection", &
                  value=connectionString, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//FILENAME)) &
                  return  ! bail out
              endif
            endif
            
          endif
        enddo
      enddo
      
      if (associated(cplList)) then
        if (count>0) then
          call NUOPC_CompAttributeSet(connector, &
            name="CplList", valueList=cplList(1:count), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          call NUOPC_CompAttributeSet(connector, &
            name="CplSetList", valueList=cplSetList(1:count), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        endif
        deallocate(cplList)
        deallocate(cplSetList)
      endif

    endif
    
    if (associated(importStandardNameList)) deallocate(importStandardNameList)
    if (associated(importFieldList)) deallocate(importFieldList)
    if (associated(importNamespaceList)) deallocate(importNamespaceList)
    if (associated(importCplSetList)) deallocate(importCplSetList)
    if (associated(exportStandardNameList)) deallocate(exportStandardNameList)
    if (associated(exportFieldList)) deallocate(exportFieldList)
    if (associated(exportNamespaceList)) deallocate(exportNamespaceList)
    if (associated(exportCplSetList)) deallocate(exportCplSetList)
    
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

  subroutine InitializeIPDv03p1(connector, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: connector
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    character(*), parameter   :: rName="InitializeIPDv03p1"
    type(ESMF_Clock)          :: internalClock
    character(ESMF_MAXSTR)    :: name
    integer                   :: verbosity, diagnostic
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(connector, name=name, verbosity=verbosity, &
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

#if 0
! There is currently no need to set the internal clock of a Connector. Also
! there is no code yet to keep updating it during Run(). For now keep this code
! inactive, but keep it here, maybe some day we will notice a need for it.

    ! set the internal clock to be a copy of the parent clock
    internalClock = ESMF_ClockCreate(clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call ESMF_CplCompSet(connector, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
#endif

    ! Simply the combination of IPDv05p2a + IPDv05p2b
    call InitializeIPDv05p2a(connector, importState, exportState, clock, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call InitializeIPDv05p2b(connector, importState, exportState, clock, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
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

  subroutine InitializeIPDv05p3(connector, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: connector
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    character(*), parameter         :: rName="InitializeIPDv05p3"
    character(ESMF_MAXSTR), pointer :: cplList(:), chopStringList(:)
    character(ESMF_MAXSTR), pointer :: cplSetList(:)
    character(ESMF_MAXSTR)          :: cplName
    integer                         :: cplListSize, i, localPet
    integer                         :: cplSetListSize
    integer                         :: bondLevel, bondLevelMax
    character(ESMF_MAXSTR), pointer :: importNamespaceList(:)
    character(ESMF_MAXSTR), pointer :: exportNamespaceList(:)
    character(ESMF_MAXSTR), pointer :: importCplSetList(:)
    character(ESMF_MAXSTR), pointer :: exportCplSetList(:)
    character(ESMF_MAXSTR), pointer :: importStandardNameList(:)
    character(ESMF_MAXSTR), pointer :: exportStandardNameList(:)
    type(ESMF_Field),       pointer :: importFieldList(:)
    type(ESMF_Field),       pointer :: exportFieldList(:)
    integer                         :: iMatch, eMatch
    type(ESMF_Field)                :: iField, eField
    integer                         :: stat
    type(type_InternalState)        :: is
    logical                         :: foundFlag
    character(ESMF_MAXSTR)          :: connectionString
    character(ESMF_MAXSTR)          :: name, iString
    character(len=160)              :: msgString
    character(ESMF_MAXSTR)          :: iTransferAction, eTransferAction
    character(ESMF_MAXSTR)          :: iTransferOffer, eTransferOffer
    character(ESMF_MAXSTR)          :: iSharePolicy, eSharePolicy
    logical                         :: matchE, matchI, acceptFlag
    type(ESMF_VM)                   :: acceptorVM, providerVM, currentVM
    logical                         :: sharable
    integer                         :: helperIn, helperOut
    integer                   :: verbosity, profiling, diagnostic
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(connector, name=name, verbosity=verbosity, &
      profiling=profiling, diagnostic=diagnostic, rc=rc)
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

    ! prepare local pointer variables
    nullify(cplList)
    nullify(cplSetList)
    nullify(importStandardNameList)
    nullify(importFieldList)
    nullify(importNamespaceList)
    nullify(importCplSetList)
    nullify(exportStandardNameList)
    nullify(exportFieldList)
    nullify(exportNamespaceList)
    nullify(exportCplSetList)
    
    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(connector, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! clean starting condition for pointer member inside internal state
    nullify(is%wrap%termOrders)

    ! re-reconcile the States because they may have changed
    ! (previous proxy objects are dropped before fresh reconcile)
    if (btest(profiling,1)) then    ! PROFILE
      call ESMF_VMLogMemInfo("befP2 Reconcile")
    endif
    call NUOPC_Reconcile(importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call NUOPC_Reconcile(exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (btest(profiling,1)) then    ! PROFILE
      call ESMF_VMLogMemInfo("aftP2 Reconcile")
    endif
    
    ! get the cplList Attribute
    call NUOPC_CompAttributeGet(connector, name="CplList", &
      itemCount=cplListSize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    ! get the cplSetList Attribute
    call NUOPC_CompAttributeGet(connector, name="CplSetList", &
      itemCount=cplSetListSize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    ! check the cplSetList size
    if (cplListSize /= cplSetListSize) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="Bad internal error - CplSetList size must match CplList size!",&
        line=__LINE__, file=trim(name)//":"//FILENAME, &
        rcToReturn=rc)
      return  ! bail out
    endif
    if (cplListSize>0) then
      allocate(cplList(cplListSize), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of internal cplList() failed.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call NUOPC_CompAttributeGet(connector, name="CplList", valueList=cplList, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      allocate(cplSetList(cplSetListSize), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of internal cplSetList() failed.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call NUOPC_CompAttributeGet(connector, name="CplSetList", &
        valueList=cplSetList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif
    ! get the importState and exportState std lists
    call NUOPC_GetStateMemberLists(importState, importStandardNameList, &
      fieldList=importFieldList, namespaceList=importNamespaceList, &
      cplSetList=importCplSetList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call NUOPC_GetStateMemberLists(exportState, exportStandardNameList, &
      fieldList=exportFieldList, namespaceList=exportNamespaceList, &
      cplSetList=exportCplSetList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! calculate coupling set for set specific connections
    nullify(is%wrap%cplSetList)
    call getUniqueList(list=cplSetList, uniqueList=is%wrap%cplSetList, &
      uniqueCount=is%wrap%cplSetCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! allocate memory for set specific connections
    allocate(is%wrap%cplSet(is%wrap%cplSetCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal state cplSet member failed.", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! clean starting condition for pointer member inside internal state   
    do i=1, is%wrap%cplSetCount
      nullify(is%wrap%cplSet(i)%termOrders) 
    enddo
    
    ! prepare chopStringList
    nullify(chopStringList)

    ! main loop over all entries in the cplList
    do i=1, cplListSize
      call NUOPC_ChopString(cplList(i), chopChar=":", &
        chopStringList=chopStringList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      cplName = chopStringList(1) ! first part is the standard name of cpl field
      deallocate(chopStringList)

      if (btest(verbosity,11).or.btest(verbosity,12)) then
        write (iString,'(I4)') i
        write (msgString, '(A)') trim(name)//": handle "// &
          "cplList("//trim(adjustl(iString))//"): "//trim(cplName)
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
      
      ! find import and export side match
      foundFlag = .false. ! reset
      do eMatch=1, size(exportStandardNameList)  ! consumer side
        if (.NOT.(cplSetList(i).EQ.exportCplSetList(eMatch))) cycle
        do iMatch=1, size(importStandardNameList)  ! producer side
          if (.NOT.(cplSetList(i).EQ.importCplSetList(iMatch))) cycle
          matchE = NUOPC_FieldDictionaryMatchSyno( &
            exportStandardNameList(eMatch), cplName, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          matchI = NUOPC_FieldDictionaryMatchSyno( &
            importStandardNameList(iMatch), cplName, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (matchE .and. matchI) then
            ! found a matching standard name pair
            ! -> determine bondLevel according to namespace matching
            bondLevel = &
              getBondLevel(importNamespaceList(iMatch), &
                exportNamespaceList(eMatch), &
                importCplSetList(iMatch), &
                exportCplSetList(eMatch))
              
            if (bondLevel == -1) cycle  ! break out and look for next match
            
            ! Getting to this place in the double loop means that the 
            ! standard name match has a connection that supports the match.
            
            ! -> look at the current ProducerConnection entry to see what to do
            eField = exportFieldList(eMatch)
            call NUOPC_GetAttribute(eField, name="ProducerConnection", &
              value=connectionString, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (index(trim(connectionString), "targeted:")==1) then
              ! this export field has been targeted -> obtain targeted bondLevel
              read (connectionString(10:len(connectionString)), "(i10)") &
                bondLevelMax  ! the bondLevel that was targeted
              if (bondLevel == bondLevelMax) then
                ! this is the targeted connection
                foundFlag = .true.
                exit
              endif
            endif
            
          endif
        enddo
        if (foundFlag) exit
      enddo
      
      if (.not.foundFlag) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="Bad internal error - should never get here!",&
          line=__LINE__, file=trim(name)//":"//FILENAME, &
          rcToReturn=rc)
        return  ! bail out
      endif
      
      if (btest(verbosity,12)) then
        write (msgString, '(A)') trim(name)//": - connected."
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
      
      if (iMatch>0 .and. eMatch>0) then
        ! there are matching Fields in the import and export States
        iField=importFieldList(iMatch)
        eField=exportFieldList(eMatch)
        
        ! set the connected Attribute on import Field
        call NUOPC_SetAttribute(iField, name="Connected", value="true", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        call NUOPC_SetAttribute(iField, name="ConsumerConnection", value="true", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        ! set the connected Attribute on export Field
        call NUOPC_SetAttribute(eField, name="Connected", value="true", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        
        ! coordinate the transfer and sharing between components
        call NUOPC_GetAttribute(iField, name="TransferOfferGeomObject", &
          value=iTransferOffer, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        call NUOPC_GetAttribute(eField, name="TransferOfferGeomObject", &
          value=eTransferOffer, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        acceptFlag=.false. ! reset
        if (trim(iTransferOffer)=="will provide") then
          if (trim(eTransferOffer)=="will provide") then
            ! -> both sides must provide
            call NUOPC_SetAttribute(iField, &
              name="TransferActionGeomObject", value="provide", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_SetAttribute(eField, &
              name="TransferActionGeomObject", value="provide", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,11)) then
              write (msgString, '(A)') trim(name)//": "//&
                "- both sides must provide the Field/GeomObject."
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
          elseif (trim(eTransferOffer)=="can provide") then
            ! -> import side must provide, export side may accept
            acceptFlag=.true.
            call NUOPC_SetAttribute(iField, &
              name="TransferActionGeomObject", value="provide", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_SetAttribute(eField, &
              name="TransferActionGeomObject", value="accept", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,11)) then
              write (msgString, '(A)') trim(name)//": "//&
                "- import side must provide, export side may accept the Field/GeomObject."
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
          else  ! eTransferOffer=="cannot provide"
            ! -> import side must provide, export side may accept
            acceptFlag=.true.
            call NUOPC_SetAttribute(iField, &
              name="TransferActionGeomObject", value="provide", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_SetAttribute(eField, &
              name="TransferActionGeomObject", value="accept", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,11)) then
              write (msgString, '(A)') trim(name)//": "//&
                "- import side must provide, export side may accept the Field/GeomObject."
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
          endif
        elseif (trim(iTransferOffer)=="can provide") then
          if (trim(eTransferOffer)=="will provide") then
            ! -> import side may accept, export side must provide
            acceptFlag=.true.
            call NUOPC_SetAttribute(iField, &
              name="TransferActionGeomObject", value="accept", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_SetAttribute(eField, &
              name="TransferActionGeomObject", value="provide", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,11)) then
              write (msgString, '(A)') trim(name)//": "//&
                "- import side may accept, export side must provide the Field/GeomObject."
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
          elseif (trim(eTransferOffer)=="can provide") then
            ! -> import side must provide, export side may accept
            acceptFlag=.true.
            call NUOPC_SetAttribute(iField, &
              name="TransferActionGeomObject", value="provide", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_SetAttribute(eField, &
              name="TransferActionGeomObject", value="accept", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,11)) then
              write (msgString, '(A)') trim(name)//": "//&
                "- import side must provide, export side may accept the Field/GeomObject."
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
          else  ! eTransferOffer=="cannot provide"
            ! -> import side must provide, export side may accept
            acceptFlag=.true.
            call NUOPC_SetAttribute(iField, &
              name="TransferActionGeomObject", value="provide", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_SetAttribute(eField, &
              name="TransferActionGeomObject", value="accept", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,11)) then
              write (msgString, '(A)') trim(name)//": "//&
                "- import side must provide, export side may accept the Field/GeomObject."
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
          endif
        else  ! iTransferOffer=="cannot provide"
          if (trim(eTransferOffer)=="will provide") then
            ! -> import side may accept, export side must provide
            acceptFlag=.true.
            call NUOPC_SetAttribute(iField, &
              name="TransferActionGeomObject", value="accept", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_SetAttribute(eField, &
              name="TransferActionGeomObject", value="provide", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,11)) then
              write (msgString, '(A)') trim(name)//": "//&
                "- import side may accept, export side must provide the Field/GeomObject."
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
          elseif (trim(eTransferOffer)=="can provide") then
            ! -> import side may accept, export side must provide
            acceptFlag=.true.
            call NUOPC_SetAttribute(iField, &
              name="TransferActionGeomObject", value="accept", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_SetAttribute(eField, &
              name="TransferActionGeomObject", value="provide", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,11)) then
              write (msgString, '(A)') trim(name)//": "//&
                "- import side may accept, export side must provide the Field/GeomObject."
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
          else  ! eTransferOffer=="cannot provide"
            ! -> neither side is able to provide -> error
            call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
              msg="Neither side (import/export) able to provide geom object.", &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
            return  ! bail out
          endif
        endif

        ! Provider and acceptor can share Field and/or GeomObjects if all of the
        ! provider PETs are also active on the acceptor side. The acceptor side
        ! might have additional PETs active, which is okay.
        sharable = .false.  ! initialize
        
        if (acceptFlag) then
          ! determine provider and acceptor VM
          call NUOPC_GetAttribute(iField, name="TransferActionGeomObject", &
            value=iTransferAction, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          call NUOPC_GetAttribute(eField, name="TransferActionGeomObject", &
            value=eTransferAction, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if ((trim(iTransferAction)=="provide") &
            .and.(trim(eTransferAction)=="accept")) then
            call ESMF_FieldGet(iField, vm=providerVM, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call ESMF_FieldGet(eField, vm=acceptorVM, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          elseif ((trim(eTransferAction)=="provide") &
            .and.(trim(iTransferAction)=="accept")) then
            call ESMF_FieldGet(eField, vm=providerVM, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call ESMF_FieldGet(iField, vm=acceptorVM, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          else  ! both sides "provide"
            !TODO: indicate error condition, should never get here
          endif
          ! determine whether the provider Field can be accepted by acceptor VM
          helperIn = 0  ! initialize
          call ESMF_VMGet(providerVM, localPet=localPet, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (localPet >= 0) then
            ! this PET is active under the provider VM -> check acceptor VM
            call ESMF_VMGet(acceptorVM, localPet=localPet, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (localPet >= 0) then
              ! acceptor does execute on this PET, therefore can share
            else
              ! acceptor does NOT execute on this PET, therefore cannot share
              helperIn = 1  ! indicated that a not sharable situation was found
            endif
          endif
          ! implement a logical OR operation based on REDUCE_SUM
          call ESMF_VMGetCurrent(currentVM, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          call ESMF_VMAllFullReduce(currentVM, sendData=(/helperIn/), &
            recvData=helperOut, count=1, reduceflag=ESMF_REDUCE_SUM, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          ! now know globally whether this is a sharable situation or not
          if (helperOut == 0) sharable = .true.
        endif
        
        if (btest(verbosity,12)) then
          if (sharable) then
            write (msgString, '(A)') trim(name)//": "//&
              "- combination of provider and acceptor VM supports "// &
              "Field and/or GeomObject sharing between them."
            call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
          else
            write (msgString, '(A)') trim(name)//": "//&
              "- combination of provider and acceptor VM does NOT support "// &
              "Field and/or GeomObject sharing between them."
            call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
          endif
        endif

        if (acceptFlag .and. sharable) then
          ! One side accepts the other and VMs allow sharing 
          ! Look at GeomObject sharing
          call NUOPC_GetAttribute(iField, name="SharePolicyGeomObject", &
            value=iSharePolicy, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          call NUOPC_GetAttribute(eField, name="SharePolicyGeomObject", &
            value=eSharePolicy, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (trim(iSharePolicy)=="share" .and. trim(eSharePolicy)=="share") &
            then
            ! both sides want to share -> shared
            call NUOPC_SetAttribute(iField, &
              name="ShareStatusGeomObject", value="shared", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_SetAttribute(eField, &
              name="ShareStatusGeomObject", value="shared", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,12)) then
              write (msgString, '(A)') trim(name)//": "//&
                "- both sides want to share the GeomObject -> shared."
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
          else
            ! at least one side does not want to share -> not shared
            ! but don't modify attribute here because if alread shared through
            ! another connection, it must stay shared. Rely on "not shared" 
            ! default.
            if (btest(verbosity,12)) then
              write (msgString, '(A)') trim(name)//": "//&
                "- at least one side does not want to share the GeomObject "// &
                "-> not shared (isharePolicy="//trim(iSharePolicy)//&
                " & esharePolicy="//trim(eSharePolicy)//")"
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
          endif
          ! Look at Field sharing
          call NUOPC_GetAttribute(iField, name="SharePolicyField", &
            value=iSharePolicy, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          call NUOPC_GetAttribute(eField, name="SharePolicyField", &
            value=eSharePolicy, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (trim(iSharePolicy)=="share" .and. trim(eSharePolicy)=="share") &
            then
            ! both sides want to share -> shared
            call NUOPC_SetAttribute(iField, &
              name="ShareStatusField", value="shared", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_SetAttribute(eField, &
              name="ShareStatusField", value="shared", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,12)) then
              write (msgString, '(A)') trim(name)//": "//&
                "- both sides want to share the Field -> shared."
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
          else
            ! at least one side does not want to share -> not shared
            ! but don't modify attribute here because if alread shared through
            ! another connection, it must stay shared. Rely on "not shared" 
            ! default.
            if (btest(verbosity,12)) then
              write (msgString, '(A)') trim(name)//": "//&
                "- at least one side does not want to share the Field "// &
                "-> not shared (isharePolicy="//trim(iSharePolicy)//&
                " & esharePolicy="//trim(eSharePolicy)//")"
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
          endif
        endif

      else
        !TODO: Fields mentioned via stdname in Cpl metadata not found -> error?
      endif
    enddo

    if (associated(cplList)) deallocate(cplList)
    if (associated(cplSetList)) deallocate(cplSetList)
    if (associated(importStandardNameList)) deallocate(importStandardNameList)
    if (associated(importFieldList)) deallocate(importFieldList)
    if (associated(importNamespaceList)) deallocate(importNamespaceList)
    if (associated(importCplSetList)) deallocate(importCplSetList)
    if (associated(exportStandardNameList)) deallocate(exportStandardNameList)
    if (associated(exportFieldList)) deallocate(exportFieldList)
    if (associated(exportNamespaceList)) deallocate(exportNamespaceList)
    if (associated(exportCplSetList)) deallocate(exportCplSetList)
    
    ! create the State member    
    is%wrap%state = ESMF_StateCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
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

  subroutine InitializeIPDv05p4(connector, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: connector
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    character(*), parameter         :: rName="InitializeIPDv05p4"
    character(ESMF_MAXSTR), pointer :: cplList(:), chopStringList(:)
    character(ESMF_MAXSTR), pointer :: cplSetList(:)
    character(ESMF_MAXSTR)          :: cplName
    integer                         :: cplListSize, i
    integer                         :: cplSetListSize
    integer                         :: bondLevel, bondLevelMax
    character(ESMF_MAXSTR), pointer :: importNamespaceList(:)
    character(ESMF_MAXSTR), pointer :: exportNamespaceList(:)
    character(ESMF_MAXSTR), pointer :: importCplSetList(:)
    character(ESMF_MAXSTR), pointer :: exportCplSetList(:)
    character(ESMF_MAXSTR), pointer :: importStandardNameList(:)
    character(ESMF_MAXSTR), pointer :: exportStandardNameList(:)
    type(ESMF_Field),       pointer :: importFieldList(:)
    type(ESMF_Field),       pointer :: exportFieldList(:)
    type(ESMF_State),       pointer :: importStateList(:)
    type(ESMF_State),       pointer :: exportStateList(:)
    integer                         :: iMatch, eMatch
    type(ESMF_Field)                :: iField, eField
    type(ESMF_Field)                :: providerField, acceptorField
    type(ESMF_State)                :: acceptorState
    type(ESMF_GeomType_Flag)        :: geomtype
    type(ESMF_Grid)                 :: grid
    type(ESMF_Mesh)                 :: mesh
    type(ESMF_LocStream)            :: locstream
    type(ESMF_StaggerLoc)           :: staggerloc
    type(ESMF_MeshLoc)              :: meshloc
    type(ESMF_DistGrid)             :: providerDG, acceptorDG
    type(ESMF_DistGrid)             :: providerDG_nodal, acceptorDG_nodal
    type(ESMF_VM)                   :: vm
    integer                         :: stat
    type(type_InternalState)        :: is
    logical                         :: foundFlag
    character(ESMF_MAXSTR)          :: connectionString
    character(ESMF_MAXSTR)          :: name, iString
    character(len=160)              :: msgString
    character(ESMF_MAXSTR)          :: geomobjname, fieldName
    character(ESMF_MAXSTR)          :: iTransferAction, eTransferAction
    character(ESMF_MAXSTR)          :: iShareStatusF, eShareStatusF
    character(ESMF_MAXSTR)          :: iShareStatusG, eShareStatusG
    integer(ESMF_KIND_I4), pointer  :: ungriddedLBound(:), ungriddedUBound(:)
    integer(ESMF_KIND_I4), pointer  :: gridToFieldMap(:)
    integer                         :: fieldDimCount, gridDimCount, arbDimCount
    logical                         :: matchE, matchI
    integer                         :: dimCount
    integer, allocatable            :: minIndex(:), maxIndex(:)
    logical                         :: sharedField, sharedGeom
    type(ESMF_Array)                :: array
    integer                         :: verbosity, profiling, diagnostic
    type(ESMF_Time)                 :: currTime
    character(len=40)               :: currTimeString
    character(len=40)               :: transferDirection
    type(ESMF_TypeKind_Flag)        :: tkf
    integer                         :: tk, gl
    
    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(connector, name=name, verbosity=verbosity, &
      profiling=profiling, diagnostic=diagnostic, rc=rc)
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

    ! prepare local pointer variables
    nullify(cplList)
    nullify(cplSetList)
    nullify(importStandardNameList)
    nullify(importFieldList)
    nullify(importStateList)
    nullify(importNamespaceList)
    nullify(importCplSetList)
    nullify(exportStandardNameList)
    nullify(exportFieldList)
    nullify(exportStateList)
    nullify(exportNamespaceList)
    nullify(exportCplSetList)
    
    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(connector, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! re-reconcile the States because they may have changed
    ! (previous proxy objects are dropped before fresh reconcile)
    if (btest(profiling,1)) then    ! PROFILE
      call ESMF_VMLogMemInfo("befP3 Reconcile")
    endif
    call NUOPC_Reconcile(importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call NUOPC_Reconcile(exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (btest(profiling,1)) then    ! PROFILE
      call ESMF_VMLogMemInfo("aftP3 Reconcile")
    endif
    
    ! get the cplList Attribute
    call NUOPC_CompAttributeGet(connector, name="CplList", &
      itemCount=cplListSize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    ! get the cplSetList Attribute
    call NUOPC_CompAttributeGet(connector, name="CplSetList", &
      itemCount=cplSetListSize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    ! check the cplSetList size
    if (cplListSize /= cplSetListSize) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="Bad internal error - CplSetList size must match CplList size!",&
        line=__LINE__, file=trim(name)//":"//FILENAME, &
        rcToReturn=rc)
      return  ! bail out
    endif
    if (cplListSize>0) then
      allocate(cplList(cplListSize), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of internal cplList() failed.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call NUOPC_CompAttributeGet(connector, name="CplList", valueList=cplList, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      allocate(cplSetList(cplSetListSize), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of internal cplSetList() failed.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call NUOPC_CompAttributeGet(connector, name="CplSetList", &
        valueList=cplSetList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif
    ! get the importState and exportState std lists
    call NUOPC_GetStateMemberLists(importState, importStandardNameList, &
      fieldList=importFieldList, stateList=importStateList, &
      namespaceList=importNamespaceList, cplSetList=importCplSetList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call NUOPC_GetStateMemberLists(exportState, exportStandardNameList, &
      fieldList=exportFieldList, stateList=exportStateList, &
      namespaceList=exportNamespaceList, cplSetList=exportCplSetList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! prepare chopStringList
    nullify(chopStringList)
    
    ! main loop over all entries in the cplList
    do i=1, cplListSize
      call NUOPC_ChopString(cplList(i), chopChar=":", &
        chopStringList=chopStringList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      cplName = chopStringList(1) ! first part is the standard name of cpl field
      deallocate(chopStringList)
      
      if (btest(verbosity,11).or.btest(verbosity,12)) then
        write (iString,'(I4)') i
        write (msgString, '(A)') trim(name)//": handle "// &
          "cplList("//trim(adjustl(iString))//"): "//trim(cplName)
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
      
      ! find import and export side match
      foundFlag = .false. ! reset
      do eMatch=1, size(exportStandardNameList)  ! consumer side
        if (.NOT.(cplSetList(i).EQ.exportCplSetList(eMatch))) cycle
        do iMatch=1, size(importStandardNameList)  ! producer side
          if (.NOT.(cplSetList(i).EQ.importCplSetList(iMatch))) cycle
          matchE = NUOPC_FieldDictionaryMatchSyno( &
            exportStandardNameList(eMatch), cplName, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          matchI = NUOPC_FieldDictionaryMatchSyno( &
            importStandardNameList(iMatch), cplName, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (matchE .and. matchI) then
            ! found a matching standard name pair
            ! -> determine bondLevel according to namespace matching
            bondLevel = &
              getBondLevel(importNamespaceList(iMatch), &
              exportNamespaceList(eMatch), &
              importCplSetList(iMatch), &
              exportCplSetList(eMatch))
              
            if (bondLevel == -1) cycle  ! break out and look for next match
            
            ! Getting to this place in the double loop means that the 
            ! standard name match has a connection that supports the match.
            
            ! -> look at the current ProducerConnection entry to see what to do
            eField = exportFieldList(eMatch)
            call NUOPC_GetAttribute(eField, name="ProducerConnection", &
              value=connectionString, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (index(trim(connectionString), "targeted:")==1) then
              ! this export field has been targeted -> obtain targeted bondLevel
              read (connectionString(10:len(connectionString)), "(i10)") &
                bondLevelMax  ! the bondLevel that was targeted
              if (bondLevel == bondLevelMax) then
                ! this is the targeted connection
                foundFlag = .true.
                exit
              endif
            endif
            
          endif
        enddo
        if (foundFlag) exit
      enddo
      
      if (.not.foundFlag) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="Bad internal error - should never get here!",&
          line=__LINE__, file=trim(name)//":"//FILENAME, &
          rcToReturn=rc)
        return  ! bail out
      endif
      
      if (iMatch>0 .and. eMatch>0) then
        ! there are matching Fields in the import and export States
        iField=importFieldList(iMatch)
        eField=exportFieldList(eMatch)
        
        ! check if TransferAction of one side is "accept"
        call NUOPC_GetAttribute(iField, name="TransferActionGeomObject", &
          value=iTransferAction, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        call NUOPC_GetAttribute(eField, name="TransferActionGeomObject", &
          value=eTransferAction, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          
        if ((trim(iTransferAction)=="provide") &
          .and.(trim(eTransferAction)=="accept")) then
          providerField = iField
          acceptorField = eField
          acceptorState = exportStateList(eMatch)
          transferDirection = "(import -> export)"
        elseif ((trim(eTransferAction)=="provide") &
          .and.(trim(iTransferAction)=="accept")) then
          providerField = eField
          acceptorField = iField
          acceptorState = importStateList(iMatch)
          transferDirection = "(import <- export)"
        else  ! both sides "provide"
          ! not a situation that needs handling here
          cycle ! continue with the next i
        endif
        
        ! there is transfer: provider -> acceptor
        ! determine if there is any sharing of GeomObject and/or Field
        
        call NUOPC_GetAttribute(iField, name="ShareStatusGeomObject", &
          value=iShareStatusG, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        call NUOPC_GetAttribute(eField, name="ShareStatusGeomObject", &
          value=eShareStatusG, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        call NUOPC_GetAttribute(iField, name="ShareStatusField", &
          value=iShareStatusF, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        call NUOPC_GetAttribute(eField, name="ShareStatusField", &
          value=eShareStatusF, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

        sharedGeom = &
          (trim(iShareStatusG)=="shared" .and. trim(eShareStatusG)=="shared")
        sharedField = &
          (trim(iShareStatusF)=="shared" .and. trim(eShareStatusF)=="shared")

        if (btest(verbosity,12)) then
          write (msgString, '(A)') trim(name)//": "//&
            "- import ShareStatusGeomObject="//trim(iShareStatusG)
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          write (msgString, '(A)') trim(name)//": "//&
            "- export ShareStatusGeomObject="//trim(eShareStatusG)
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          if (sharedGeom) then
            call ESMF_LogWrite(trim(name)//": "//"-> sharing GeomObject", &
              ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
          else
            call ESMF_LogWrite(trim(name)//": "//"-> NOT sharing GeomObject", &
              ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
          endif
          write (msgString, '(A)') trim(name)//": "//&
            "- import ShareStatusField="//trim(iShareStatusF)
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          write (msgString, '(A)') trim(name)//": "//&
            "- export ShareStatusField="//trim(eShareStatusF)
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          if (sharedField) then
            call ESMF_LogWrite(trim(name)//": "//"-> sharing Field.", &
              ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
          else
            call ESMF_LogWrite(trim(name)//": "//"-> NOT sharing Field.", &
              ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
          endif
        endif

        ! transfer the underlying DistGrid from provider to acceptor
        ! or share the providerField or providerGeomObject with the acceptor
        call ESMF_FieldGet(providerField, geomtype=geomtype, typekind=tkf, &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        
        ! ESMF_GEOMTYPE_GRID
        if (geomtype==ESMF_GEOMTYPE_GRID) then
          call ESMF_FieldGet(providerField, grid=grid, staggerloc=staggerloc, &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (sharedGeom) then
            ! set the shared Grid on the acceptor side
            call ESMF_FieldEmptySet(acceptorField, grid=grid, &
              staggerloc=staggerloc, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return
          else
            ! not shared GeomObject > must transfer
            call ESMF_GridGet(grid, distgrid=providerDG, name=geomobjname, &
              dimCount=dimCount, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call ESMF_FieldGet(acceptorField, vm=vm, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,11)) then
              call ESMF_LogWrite(trim(name)//": - transferring underlying "// &
                "DistGrid "//trim(transferDirection)//" for Grid: "&
                //trim(geomobjname), ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            endif
            acceptorDG = ESMF_DistGridCreate(providerDG, vm=vm, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,11)) then
              call ESMF_LogWrite(trim(name)//&
                ": - done creating acceptorDG from providerDG", &
                ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            endif
            grid = ESMF_GridEmptyCreate(vm=vm, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call ESMF_GridSet(grid, name=geomobjname, distgrid=acceptorDG, &
              vm=vm, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call ESMF_FieldEmptySet(acceptorField, grid=grid, &
              staggerloc=staggerloc, vm=vm, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,11)) then
              call ESMF_LogWrite(trim(name)//&
                ": - done transferring underlying DistGrid", &
                ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            endif
          endif
          ! query additional provider information
          call ESMF_FieldGet(providerField, grid=grid, dimCount=fieldDimCount, &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          call ESMF_GridGet(grid, dimCount=gridDimCount, &
            arbDimCount=arbDimCount, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          allocate(minIndex(gridDimCount), maxIndex(gridDimCount), stat=rc)
          if (ESMF_LogFoundAllocError(rc, msg="Allocating minIndex, maxIndex", &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          call ESMF_GridGetIndex(grid, tileNo=1, &  !TODO: support tileCount>1!!
            minIndex=minIndex, maxIndex=maxIndex, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          allocate(gridToFieldMap(gridDimCount),stat=stat)
          if (ESMF_LogFoundAllocError(statusToCheck=stat, &
            msg="Allocation of internal gridToFieldMap failed.", &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          call ESMF_FieldGet(providerField, gridToFieldMap=gridToFieldMap, &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (fieldDimCount - gridDimCount > 0) then
            ! query ungridded dim bounds
            allocate(ungriddedLBound(fieldDimCount-gridDimCount),stat=stat)
            if (ESMF_LogFoundAllocError(statusToCheck=stat, &
              msg="Allocation of internal ungriddedLBound failed.", &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
            allocate(ungriddedUBound(fieldDimCount-gridDimCount),stat=stat)
            if (ESMF_LogFoundAllocError(statusToCheck=stat, &
              msg="Allocation of internal ungriddedUBound failed.", &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
            call ESMF_FieldGet(providerField, ungriddedLBound=ungriddedLBound, &
              ungriddedUBound=ungriddedUBound, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          endif
          if (.not.sharedField) then
            ! transfer additional provider info in form of attributes
            tk = tkf  ! convert TypeKind_Flag to integer
            call ESMF_AttributeSet(acceptorField, &
              name="TypeKind", value=tk, &
              convention="NUOPC", purpose="Instance", &
              attnestflag=ESMF_ATTNEST_ON, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            gl = staggerloc  ! convert StaggerLoc to integer
            call ESMF_AttributeSet(acceptorField, &
              name="GeomLoc", value=gl, &
              convention="NUOPC", purpose="Instance", &
              attnestflag=ESMF_ATTNEST_ON, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call ESMF_AttributeSet(acceptorField, &
              name="MinIndex", valueList=minIndex, &
              convention="NUOPC", purpose="Instance", &
              attnestflag=ESMF_ATTNEST_ON, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call ESMF_AttributeSet(acceptorField, &
              name="MaxIndex", valueList=maxIndex, &
              convention="NUOPC", purpose="Instance", &
              attnestflag=ESMF_ATTNEST_ON, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            ! bring over arbDimCount as attribute
            call ESMF_AttributeSet(acceptorField, &
              name="ArbDimCount", value=arbDimCount, &
              convention="NUOPC", purpose="Instance", &
              attnestflag=ESMF_ATTNEST_ON, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            ! bring over gridToFieldMap as attributes
            call ESMF_AttributeSet(acceptorField, &
              name="GridToFieldMap", valueList=gridToFieldMap, &
              convention="NUOPC", purpose="Instance", &
              attnestflag=ESMF_ATTNEST_ON, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (fieldDimCount - gridDimCount > 0) then
              ! bring over ungridded dim bounds as attributes
              call ESMF_AttributeSet(acceptorField, &
                name="UngriddedLBound", valueList=ungriddedLBound, &
                convention="NUOPC", purpose="Instance", &
                attnestflag=ESMF_ATTNEST_ON, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
              call ESMF_AttributeSet(acceptorField, &
                name="UngriddedUBound", valueList=ungriddedUBound, &
                convention="NUOPC", purpose="Instance", &
                attnestflag=ESMF_ATTNEST_ON, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            endif
          else
            ! sharedField, can create acceptorField here if grid is also shared
            if (sharedGeom) then
              call ShareFieldWithGrid(acceptorField, providerField, name=name, &
                rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return
              ! replace the old acceptorField in acceptorState by realizing
              call NUOPC_Realize(acceptorState, acceptorField, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return
              ! reset the TransferActionGeomObject for this completed field
              ! to prevent handling of the same field in the next phase
              call NUOPC_SetAttribute(acceptorField, &
                name="TransferActionGeomObject", value="complete", rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return
              if (btest(verbosity,12)) then
                write (msgString, '(A)') trim(name)//": "//&
                  "- Grid-based acceptorField created and realized, "//&
                  "reference sharing with providerField"
                call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                  return  ! bail out
               endif
            endif
          endif
          
          ! clean-up
          deallocate(minIndex, maxIndex, stat=rc)
          if (ESMF_LogFoundDeallocError(rc, &
            msg="Deallocating minIndex, maxIndex", &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          deallocate(gridToFieldMap, stat=rc)
          if (ESMF_LogFoundDeallocError(rc, &
            msg="Deallocating gridToFieldMap", &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (fieldDimCount - gridDimCount > 0) then
            deallocate(ungriddedLBound, stat=rc)
            if (ESMF_LogFoundDeallocError(rc, &
              msg="Deallocating ungriddedLBound", &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            deallocate(ungriddedUBound, stat=rc)
            if (ESMF_LogFoundDeallocError(rc, &
              msg="Deallocating ungriddedUBound", &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          endif
        
        ! ESMF_GEOMTYPE_MESH
        elseif (geomtype==ESMF_GEOMTYPE_MESH) then
          call ESMF_FieldGet(providerField, mesh=mesh, meshloc=meshloc, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (sharedGeom) then
            ! set the shared Mesh on the acceptor side
            call ESMF_FieldEmptySet(acceptorField, mesh=mesh, meshloc=meshloc, &
              rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          else
            ! not sharedGeom -> must transfer
            call ESMF_MeshGet(mesh, elementDistgrid=providerDG, &
              nodalDistgrid=providerDG_nodal, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call ESMF_FieldGet(acceptorField, vm=vm, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,11)) then
              call ESMF_LogWrite(trim(name)//&
                ": - transferring underlying DistGrid "//&
                trim(transferDirection)//" for Mesh", ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            endif
            acceptorDG = ESMF_DistGridCreate(providerDG, vm=vm, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            acceptorDG_nodal = ESMF_DistGridCreate(providerDG_nodal, vm=vm, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            !TODO: When Mesh implements a name, make sure to transfer it here!
            mesh = ESMF_MeshCreate(acceptorDG, nodalDistgrid=acceptorDG_nodal, &
              rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call ESMF_FieldEmptySet(acceptorField, mesh=mesh, meshloc=meshloc, &
              rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,11)) then
              call ESMF_LogWrite(trim(name)//&
                ": - done transferring underlying DistGrid", &
                ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            endif
          endif
          ! query additional provider information
          call ESMF_FieldGet(providerField, dimCount=fieldDimCount, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          call ESMF_DistGridGet(providerDG, dimCount=gridDimCount, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          allocate(gridToFieldMap(gridDimCount),stat=stat)
          if (ESMF_LogFoundAllocError(statusToCheck=stat, &
            msg="Allocation of internal gridToFieldMap failed.", &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          call ESMF_FieldGet(providerField, gridToFieldMap=gridToFieldMap, &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (fieldDimCount - gridDimCount > 0) then
            ! query ungridded dim bounds
            allocate(ungriddedLBound(fieldDimCount-gridDimCount),stat=stat)
            if (ESMF_LogFoundAllocError(statusToCheck=stat, &
              msg="Allocation of internal ungriddedLBound failed.", &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
            allocate(ungriddedUBound(fieldDimCount-gridDimCount),stat=stat)
            if (ESMF_LogFoundAllocError(statusToCheck=stat, &
              msg="Allocation of internal ungriddedUBound failed.", &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
            call ESMF_FieldGet(providerField, ungriddedLBound=ungriddedLBound, &
              ungriddedUBound=ungriddedUBound, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          endif
          if (.not.sharedField) then
            ! transfer additional provider info in form of attributes
            tk = tkf  ! convert TypeKind_Flag to integer
            call ESMF_AttributeSet(acceptorField, &
              name="TypeKind", value=tk, &
              convention="NUOPC", purpose="Instance", &
              attnestflag=ESMF_ATTNEST_ON, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            gl = meshloc  ! convert StaggerLoc to integer
            call ESMF_AttributeSet(acceptorField, &
              name="GeomLoc", value=gl, &
              convention="NUOPC", purpose="Instance", &
              attnestflag=ESMF_ATTNEST_ON, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            ! bring over gridToFieldMap as attributes
            call ESMF_AttributeSet(acceptorField, &
              name="GridToFieldMap", valueList=gridToFieldMap, &
              convention="NUOPC", purpose="Instance", &
              attnestflag=ESMF_ATTNEST_ON, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (fieldDimCount - gridDimCount > 0) then
              ! bring over ungridded dim bounds as attributes
              call ESMF_AttributeSet(acceptorField, &
                name="UngriddedLBound", valueList=ungriddedLBound, &
                convention="NUOPC", purpose="Instance", &
                attnestflag=ESMF_ATTNEST_ON, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
              call ESMF_AttributeSet(acceptorField, &
                name="UngriddedUBound", valueList=ungriddedUBound, &
                convention="NUOPC", purpose="Instance", &
                attnestflag=ESMF_ATTNEST_ON, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            endif
          else
            ! sharedField, can create acceptorField here if mesh is also shared
            if (sharedGeom) then
              call ShareFieldWithMesh(acceptorField, providerField, name=name, &
                rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return
              ! replace the old acceptorField in acceptorState by realizing
              call NUOPC_Realize(acceptorState, acceptorField, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return
              ! reset the TransferActionGeomObject for this completed field
              ! to prevent handling of the same field in the next phase
              call NUOPC_SetAttribute(acceptorField, &
                name="TransferActionGeomObject", value="complete", rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return
              if (btest(verbosity,12)) then
                write (msgString, '(A)') trim(name)//": "//&
                  "- Mesh-based acceptorField created and realized, "//&
                  "reference sharing with providerField"
                call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                  return  ! bail out
               endif
            endif
          endif

          ! clean-up
          deallocate(gridToFieldMap, stat=rc)
          if (ESMF_LogFoundDeallocError(rc, &
            msg="Deallocating gridToFieldMap", &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (fieldDimCount - gridDimCount > 0) then
            deallocate(ungriddedLBound, stat=rc)
            if (ESMF_LogFoundDeallocError(rc, &
              msg="Deallocating ungriddedLBound", &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            deallocate(ungriddedUBound, stat=rc)
            if (ESMF_LogFoundDeallocError(rc, &
              msg="Deallocating ungriddedUBound", &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          endif

        ! ESMF_GEOMTYPE_LOCSTREAM
        elseif (geomtype==ESMF_GEOMTYPE_LOCSTREAM) then
          !TODO: consider sharedGeom and sharedField
          call ESMF_FieldGet(providerField, locstream=locstream, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          call ESMF_LocStreamGet(locstream, distgrid=providerDG, &
            name=geomobjname, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          call ESMF_FieldGet(acceptorField, vm=vm, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (btest(verbosity,11)) then
            call ESMF_LogWrite(trim(name)//&
              ": - transferring underlying DistGrid "//trim(transferDirection)&
              //" for LocStream", ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          endif
          acceptorDG = ESMF_DistGridCreate(providerDG, vm=vm, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          locstream = ESMF_LocStreamCreate(distgrid=acceptorDG, &
            name=geomobjname, vm=vm, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          call ESMF_FieldEmptySet(acceptorField, locstream=locstream, vm=vm, &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (btest(verbosity,11)) then
            call ESMF_LogWrite(trim(name)//&
              ": - done transferring underlying DistGrid", &
              ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          endif
          ! query additional provider information
          call ESMF_FieldGet(providerField, dimCount=fieldDimCount, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          call ESMF_DistGridGet(providerDG, dimCount=gridDimCount, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          allocate(gridToFieldMap(gridDimCount),stat=stat)
          if (ESMF_LogFoundAllocError(statusToCheck=stat, &
            msg="Allocation of internal gridToFieldMap failed.", &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          call ESMF_FieldGet(providerField, gridToFieldMap=gridToFieldMap, &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (fieldDimCount - gridDimCount > 0) then
            ! query ungridded dim bounds
            allocate(ungriddedLBound(fieldDimCount-gridDimCount),stat=stat)
            if (ESMF_LogFoundAllocError(statusToCheck=stat, &
              msg="Allocation of internal ungriddedLBound failed.", &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
            allocate(ungriddedUBound(fieldDimCount-gridDimCount),stat=stat)
            if (ESMF_LogFoundAllocError(statusToCheck=stat, &
              msg="Allocation of internal ungriddedUBound failed.", &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
            call ESMF_FieldGet(providerField, ungriddedLBound=ungriddedLBound, &
              ungriddedUBound=ungriddedUBound, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          endif
          ! transfer additional provider info in form of attributes
          tk = tkf  ! convert TypeKind_Flag to integer
          call ESMF_AttributeSet(acceptorField, &
            name="TypeKind", value=tk, &
            convention="NUOPC", purpose="Instance", &
            attnestflag=ESMF_ATTNEST_ON, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          ! bring over gridToFieldMap as attributes
          call ESMF_AttributeSet(acceptorField, &
            name="GridToFieldMap", valueList=gridToFieldMap, &
            convention="NUOPC", purpose="Instance", &
            attnestflag=ESMF_ATTNEST_ON, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (fieldDimCount - gridDimCount > 0) then
            ! bring over ungridded dim bounds as attributes
            call ESMF_AttributeSet(acceptorField, &
              name="UngriddedLBound", valueList=ungriddedLBound, &
              convention="NUOPC", purpose="Instance", &
              attnestflag=ESMF_ATTNEST_ON, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call ESMF_AttributeSet(acceptorField, &
              name="UngriddedUBound", valueList=ungriddedUBound, &
              convention="NUOPC", purpose="Instance", &
              attnestflag=ESMF_ATTNEST_ON, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          endif
          ! clean-up
          deallocate(gridToFieldMap, stat=rc)
          if (ESMF_LogFoundDeallocError(rc, &
            msg="Deallocating gridToFieldMap", &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (fieldDimCount - gridDimCount > 0) then
            deallocate(ungriddedLBound, stat=rc)
            if (ESMF_LogFoundDeallocError(rc, &
              msg="Deallocating ungriddedLBound", &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            deallocate(ungriddedUBound, stat=rc)
            if (ESMF_LogFoundDeallocError(rc, &
              msg="Deallocating ungriddedUBound", &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          endif

        ! unsupported ESMF_GEOMTYPE
        else
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="Provided GeomType must be Grid, Mesh, or LocStream.", &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
          return  ! bail out
        endif
      else
        !TODO: Fields mentioned via stdname in Cpl metadata not found -> error?
      endif

    enddo

    if (associated(cplList)) deallocate(cplList)
    if (associated(cplSetList)) deallocate(cplSetList)
    if (associated(importStandardNameList)) deallocate(importStandardNameList)
    if (associated(importFieldList)) deallocate(importFieldList)
    if (associated(importStateList)) deallocate(importStateList)
    if (associated(importNamespaceList)) deallocate(importNamespaceList)
    if (associated(importCplSetList)) deallocate(importCplSetList)
    if (associated(exportStandardNameList)) deallocate(exportStandardNameList)
    if (associated(exportFieldList)) deallocate(exportFieldList)
    if (associated(exportStateList)) deallocate(exportStateList)
    if (associated(exportNamespaceList)) deallocate(exportNamespaceList)
    if (associated(exportCplSetList)) deallocate(exportCplSetList)
    
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

  subroutine InitializeIPDv05p5(connector, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: connector
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    character(*), parameter         :: rName="InitializeIPDv05p5"
    character(ESMF_MAXSTR), pointer :: cplList(:), chopStringList(:)
    character(ESMF_MAXSTR), pointer :: cplSetList(:)
    character(ESMF_MAXSTR)          :: cplName
    integer                         :: cplListSize, i
    integer                         :: cplSetListSize
    integer                         :: bondLevel, bondLevelMax
    character(ESMF_MAXSTR), pointer :: importNamespaceList(:)
    character(ESMF_MAXSTR), pointer :: exportNamespaceList(:)
    character(ESMF_MAXSTR), pointer :: importCplSetList(:)
    character(ESMF_MAXSTR), pointer :: exportCplSetList(:)
    character(ESMF_MAXSTR), pointer :: importStandardNameList(:)
    character(ESMF_MAXSTR), pointer :: exportStandardNameList(:)
    type(ESMF_Field),       pointer :: importFieldList(:)
    type(ESMF_Field),       pointer :: exportFieldList(:)
    type(ESMF_State),       pointer :: importStateList(:)
    type(ESMF_State),       pointer :: exportStateList(:)
    integer                         :: iMatch, eMatch
    type(ESMF_Field)                :: iField, eField
    type(ESMF_Field)                :: providerField, acceptorField
    type(ESMF_State)                :: acceptorState
    type(ESMF_GeomType_Flag)        :: geomtype
    type(ESMF_Grid)                 :: providerGrid, acceptorGrid
    type(ESMF_Mesh)                 :: providerMesh, acceptorMesh
    type(ESMF_StaggerLoc)           :: staggerloc
    type(ESMF_MeshLoc)              :: meshloc
    type(ESMF_LocStream)            :: providerLocstream, acceptorLocstream
    logical                         :: meshNoConnections
    type(ESMF_DistGrid)             :: distgrid, eDistgrid, nDistgrid
    integer                         :: stat
    type(type_InternalState)        :: is
    logical                         :: foundFlag
    character(ESMF_MAXSTR)          :: connectionString
    character(ESMF_MAXSTR)          :: name, iString
    character(len=160)              :: msgString
    character(ESMF_MAXSTR)          :: geomobjname
    character(ESMF_MAXSTR)          :: iTransferAction, eTransferAction
    character(ESMF_MAXSTR)          :: iShareStatusF, eShareStatusF
    character(ESMF_MAXSTR)          :: iShareStatusG, eShareStatusG
    logical                         :: matchE, matchI
    logical                         :: sharedField, sharedGeom
    integer                         :: verbosity, profiling, diagnostic
    type(ESMF_Time)                 :: currTime
    character(len=40)               :: currTimeString
    character(len=40)               :: transferDirection
    logical                         :: isPresentNDG, isPresentEDG

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(connector, name=name, verbosity=verbosity, &
      profiling=profiling, diagnostic=diagnostic, rc=rc)
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

    ! prepare local pointer variables
    nullify(cplList)
    nullify(cplSetList)
    nullify(importStandardNameList)
    nullify(importFieldList)
    nullify(importStateList)
    nullify(importNamespaceList)
    nullify(importCplSetList)
    nullify(exportStandardNameList)
    nullify(exportFieldList)
    nullify(exportStateList)
    nullify(exportNamespaceList)
    nullify(exportCplSetList)

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(connector, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! re-reconcile the States because they may have changed
    ! (previous proxy objects are dropped before fresh reconcile)
    if (btest(profiling,1)) then    ! PROFILE
      call ESMF_VMLogMemInfo("befP4 Reconcile")
    endif
    call NUOPC_Reconcile(importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call NUOPC_Reconcile(exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (btest(profiling,1)) then    ! PROFILE
      call ESMF_VMLogMemInfo("aftP4 Reconcile")
    endif
    
    ! get the cplList Attribute
    call NUOPC_CompAttributeGet(connector, name="CplList", &
      itemCount=cplListSize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    ! get the cplSetList Attribute
    call NUOPC_CompAttributeGet(connector, name="CplSetList", &
      itemCount=cplSetListSize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    ! check the cplSetList size
    if (cplListSize /= cplSetListSize) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="Bad internal error - CplSetList size must match CplList size!",&
        line=__LINE__, file=trim(name)//":"//FILENAME, &
        rcToReturn=rc)
      return  ! bail out
    endif
    if (cplListSize>0) then
      allocate(cplList(cplListSize), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of internal cplList() failed.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call NUOPC_CompAttributeGet(connector, name="CplList", valueList=cplList, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      allocate(cplSetList(cplSetListSize), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of internal cplSetList() failed.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call NUOPC_CompAttributeGet(connector, name="CplSetList", &
        valueList=cplSetList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif
    ! get the importState and exportState std lists
    call NUOPC_GetStateMemberLists(importState, importStandardNameList, &
      fieldList=importFieldList, stateList=importStateList, &
      namespaceList=importNamespaceList, cplSetList=importCplSetList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call NUOPC_GetStateMemberLists(exportState, exportStandardNameList, &
      fieldList=exportFieldList, stateList=exportStateList, &
      namespaceList=exportNamespaceList, cplSetList=exportCplSetList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! prepare chopStringList
    nullify(chopStringList)

    ! main loop over all entries in the cplList
    do i=1, cplListSize
      call NUOPC_ChopString(cplList(i), chopChar=":", &
        chopStringList=chopStringList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      cplName = chopStringList(1) ! first part is the standard name of cpl field
      deallocate(chopStringList)
      
      if (btest(verbosity,11).or.btest(verbosity,12)) then
        write (iString,'(I4)') i
        write (msgString, '(A)') trim(name)//": handle "// &
          "cplList("//trim(adjustl(iString))//"): "//trim(cplName)
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
      
      ! find import and export side match
      foundFlag = .false. ! reset
      do eMatch=1, size(exportStandardNameList)  ! consumer side
        if (.NOT.(cplSetList(i).EQ.exportCplSetList(eMatch))) cycle
        do iMatch=1, size(importStandardNameList)  ! producer side
          if (.NOT.(cplSetList(i).EQ.importCplSetList(iMatch))) cycle
          matchE = NUOPC_FieldDictionaryMatchSyno( &
            exportStandardNameList(eMatch), cplName, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          matchI = NUOPC_FieldDictionaryMatchSyno( &
            importStandardNameList(iMatch), cplName, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (matchE .and. matchI) then
            ! found a matching standard name pair
            ! -> determine bondLevel according to namespace matching
            bondLevel = &
              getBondLevel(importNamespaceList(iMatch), &
              exportNamespaceList(eMatch), &
              importCplSetList(iMatch), &
              exportCplSetList(eMatch))
              
            if (bondLevel == -1) cycle  ! break out and look for next match
            
            ! Getting to this place in the double loop means that the 
            ! standard name match has a connection that supports the match.
            
            ! -> look at the current ProducerConnection entry to see what to do
            eField = exportFieldList(eMatch)
            call NUOPC_GetAttribute(eField, name="ProducerConnection", &
              value=connectionString, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (index(trim(connectionString), "targeted:")==1) then
              ! this export field has been targeted -> obtain targeted bondLevel
              read (connectionString(10:len(connectionString)), "(i10)") &
                bondLevelMax  ! the bondLevel that was targeted
              if (bondLevel == bondLevelMax) then
                ! this is the targeted connection
                foundFlag = .true.
                exit
              endif
            endif
            
          endif
        enddo
        if (foundFlag) exit
      enddo
      
      if (.not.foundFlag) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="Bad internal error - should never get here!",&
          line=__LINE__, file=trim(name)//":"//FILENAME, &
          rcToReturn=rc)
        return  ! bail out
      endif
      
      if (iMatch>0 .and. eMatch>0) then
        ! there are matching Fields in the import and export States
        iField=importFieldList(iMatch)
        eField=exportFieldList(eMatch)

        ! check if TransferAction of one side is "accept"
        call NUOPC_GetAttribute(iField, name="TransferActionGeomObject", &
          value=iTransferAction, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        call NUOPC_GetAttribute(eField, name="TransferActionGeomObject", &
          value=eTransferAction, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          
        if ((trim(iTransferAction)=="provide") &
          .and.(trim(eTransferAction)=="accept")) then
          providerField = iField
          acceptorField = eField
          acceptorState = exportStateList(eMatch)
          transferDirection = "(import -> export)"
        elseif ((trim(eTransferAction)=="provide") &
          .and.(trim(iTransferAction)=="accept")) then
          providerField = eField
          acceptorField = iField
          acceptorState = importStateList(iMatch)
          transferDirection = "(import <- export)"
        else  ! not a situation that needs handling here
          cycle ! continue with the next i
        endif

        ! there is transfer: provider -> acceptor
        ! determine if there is any sharing of GeomObject and/or Field
        
        call NUOPC_GetAttribute(iField, name="ShareStatusGeomObject", &
          value=iShareStatusG, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        call NUOPC_GetAttribute(eField, name="ShareStatusGeomObject", &
          value=eShareStatusG, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        call NUOPC_GetAttribute(iField, name="ShareStatusField", &
          value=iShareStatusF, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        call NUOPC_GetAttribute(eField, name="ShareStatusField", &
          value=eShareStatusF, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

        sharedGeom = &
          (trim(iShareStatusG)=="shared" .and. trim(eShareStatusG)=="shared")
        sharedField = &
          (trim(iShareStatusF)=="shared" .and. trim(eShareStatusF)=="shared")

        if (btest(verbosity,12)) then
          write (msgString, '(A)') trim(name)//": "//&
            "- import ShareStatusGeomObject="//trim(iShareStatusG)
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          write (msgString, '(A)') trim(name)//": "//&
            "- export ShareStatusGeomObject="//trim(eShareStatusG)
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          if (sharedGeom) then
            call ESMF_LogWrite(trim(name)//": "//"-> sharing GeomObject", &
              ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
          else
            call ESMF_LogWrite(trim(name)//": "//"-> NOT sharing GeomObject", &
              ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
          endif
          write (msgString, '(A)') trim(name)//": "//&
            "- import ShareStatusField="//trim(iShareStatusF)
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          write (msgString, '(A)') trim(name)//": "//&
            "- export ShareStatusField="//trim(eShareStatusF)
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          if (sharedField) then
            call ESMF_LogWrite(trim(name)//": "//"-> sharing Field", &
              ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
          else
            call ESMF_LogWrite(trim(name)//": "//"-> NOT sharing Field", &
              ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
          endif
        endif

        if (.not.sharedGeom) then
          ! transfer the underlying Grid/Mesh/LocStream from provider to acceptor
          call ESMF_FieldGet(providerField, geomtype=geomtype, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (geomtype==ESMF_GEOMTYPE_GRID) then
            if (btest(verbosity,11)) then
              call ESMF_LogWrite(trim(name)//&
                ": - transferring the full Grid with coordinates "//&
                trim(transferDirection), ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            endif
            call ESMF_FieldGet(providerField, grid=providerGrid, &
              staggerloc=staggerloc, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call ESMF_GridGet(providerGrid, name=geomobjname, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call ESMF_FieldGet(acceptorField, grid=acceptorGrid, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call ESMF_GridGet(acceptorGrid, distgrid=distgrid, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            acceptorGrid = ESMF_GridCreate(providerGrid, distgrid, &
              name=geomobjname, copyAttributes=.TRUE.,rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call ESMF_FieldEmptySet(acceptorField, grid=acceptorGrid, &
              staggerloc=staggerloc, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,11)) then
              call ESMF_LogWrite(trim(name)//&
                ": - done transferring the full Grid with coordinates", &
                ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            endif
            if (sharedField) then
              ! sharedField, can now be created because grid is complete
              call ShareFieldWithGrid(acceptorField, providerField, name=name, &
                rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return
              ! replace the old acceptorField in acceptorState by realizing
              call NUOPC_Realize(acceptorState, acceptorField, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return
              if (btest(verbosity,12)) then
                write (msgString, '(A)') trim(name)//": "//&
                  "- Grid-based acceptorField created and realized, "//&
                  "reference sharing with providerField"
                call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                  return  ! bail out
               endif
            endif
          elseif (geomtype==ESMF_GEOMTYPE_MESH) then
            if (btest(verbosity,11)) then
              call ESMF_LogWrite(trim(name)//&
                ": - transferring the full Mesh with coordinates "//&
                trim(transferDirection), ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            endif
            call ESMF_FieldGet(providerField, mesh=providerMesh, &
              meshloc=meshloc, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out            
            call ESMF_MeshGet(providerMesh, isMemFreed=meshNoConnections, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call ESMF_FieldGet(acceptorField, mesh=acceptorMesh, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call ESMF_MeshGet(acceptorMesh, nodalDistgridIsPresent=isPresentNDG, &
              elementDistgridIsPresent=isPresentEDG, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (isPresentNDG.and.isPresentEDG) then
              ! get and use both DistGrids
              call ESMF_MeshGet(acceptorMesh, nodalDistgrid=nDistgrid, &
                elementDistgrid=eDistgrid, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
              acceptorMesh = ESMF_MeshCreate(providerMesh, &
                nodalDistgrid=nDistgrid, elementDistgrid=eDistgrid, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            elseif (isPresentNDG.and. .not.isPresentEDG) then
              ! only use Node DistGrids
              call ESMF_MeshGet(acceptorMesh, nodalDistgrid=nDistgrid, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
              acceptorMesh = ESMF_MeshCreate(providerMesh, &
                nodalDistgrid=nDistgrid, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            elseif (isPresentEDG.and. .not.isPresentNDG) then
              ! only use Element DistGrids
              call ESMF_MeshGet(acceptorMesh, elementDistgrid=eDistgrid, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
              acceptorMesh = ESMF_MeshCreate(providerMesh, &
                elementDistgrid=eDistgrid, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            else
              ! cannot create Mesh without a DistGrid -> error out
              call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
                msg="Acceptor side must define nodal or element DistGrid.", &
                line=__LINE__, file=trim(name)//":"//FILENAME, &
                rcToReturn=rc)
              return  ! bail out
            endif
            call ESMF_FieldEmptySet(acceptorField, mesh=acceptorMesh, &
              meshloc=meshloc, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,11)) then
              call ESMF_LogWrite(trim(name)//&
                ": - done transferring the full Mesh with coordinates", &
                ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            endif
            if (sharedField) then
              ! sharedField, can now be created because mesh is complete
              call ShareFieldWithMesh(acceptorField, providerField, name=name, &
                rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return
              ! replace the old acceptorField in acceptorState by realizing
              call NUOPC_Realize(acceptorState, acceptorField, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return
              if (btest(verbosity,12)) then
                write (msgString, '(A)') trim(name)//": "//&
                  "- Mesh-based acceptorField created and realized, "//&
                  "reference sharing with providerField"
                call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                  return  ! bail out
               endif
            endif
          elseif (geomtype==ESMF_GEOMTYPE_LOCSTREAM) then
            if (btest(verbosity,11)) then
              call ESMF_LogWrite(trim(name)//&
                ": - transferring the full LocStream with coordinates "//&
                trim(transferDirection), ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            endif
            call ESMF_FieldGet(providerField, locstream=providerLocstream, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call ESMF_LocStreamGet(providerLocstream, name=geomobjname, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call ESMF_FieldGet(acceptorField, locstream=acceptorLocstream, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call ESMF_LocStreamGet(acceptorLocstream, distgrid=distgrid, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            acceptorLocstream = ESMF_LocStreamCreate(providerLocstream, &
              distgrid=distgrid, name=geomobjname, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call ESMF_FieldEmptySet(acceptorField, locstream=acceptorLocstream, &
              rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,11)) then
              call ESMF_LogWrite(trim(name)//&
                ": - done transferring the full LocStream with coordinates", &
                ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            endif
            if (sharedField) then
              !TODO: sharedField, can now be created because mesh is complete
            endif
          else
            call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
              msg="Provided GeomType must be Grid, Mesh, or LocStream.", &
              line=__LINE__, file=trim(name)//":"//FILENAME, &
              rcToReturn=rc)
            return  ! bail out
          endif
        endif
        
        ! Need to reset the TransferOffer and TransferAction
        ! attributes on the acceptorField, just in case this Field interacts on
        ! multiple levels of a component hierarchy.
        call NUOPC_SetAttribute(acceptorField, &
          name="TransferOfferGeomObject", value="will provide", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        call NUOPC_SetAttribute(acceptorField, &
          name="TransferActionGeomObject", value="provide", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

      else
        !TODO: Fields mentioned via stdname in Cpl metadata not found -> error?
      endif

    enddo

    if (associated(cplList)) deallocate(cplList)
    if (associated(cplSetList)) deallocate(cplSetList)
    if (associated(importStandardNameList)) deallocate(importStandardNameList)
    if (associated(importFieldList)) deallocate(importFieldList)
    if (associated(importStateList)) deallocate(importStateList)
    if (associated(importNamespaceList)) deallocate(importNamespaceList)
    if (associated(importCplSetList)) deallocate(importCplSetList)
    if (associated(exportStandardNameList)) deallocate(exportStandardNameList)
    if (associated(exportFieldList)) deallocate(exportFieldList)
    if (associated(exportStateList)) deallocate(exportStateList)
    if (associated(exportNamespaceList)) deallocate(exportNamespaceList)
    if (associated(exportCplSetList)) deallocate(exportCplSetList)

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

  subroutine ShareFieldWithGrid(acceptorField, providerField, name, rc)
    type(ESMF_Field), intent(inout) :: acceptorField
    type(ESMF_Field), intent(in)    :: providerField
    character(*),     intent(in)    :: name
    integer,          intent(out)   :: rc
    ! local variables
    type(ESMF_Grid)                 :: grid
    integer                         :: fieldDimCount, gridDimCount
    integer, allocatable            :: minIndex(:), maxIndex(:)
    integer(ESMF_KIND_I4), pointer  :: ungriddedLBound(:), ungriddedUBound(:)
    integer(ESMF_KIND_I4), pointer  :: gridToFieldMap(:)
    type(ESMF_StaggerLoc)           :: staggerloc
    type(ESMF_Array)                :: array
    type(ESMF_VM)                   :: vm
    character(ESMF_MAXSTR)          :: fieldName
    integer                         :: stat
    integer                         :: localPet
    type(ESMF_State)                :: state

    ! set RC
    rc = ESMF_SUCCESS
    
    ! queries
    call ESMF_FieldGet(acceptorField, grid=grid, name=fieldName, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return
    call ESMF_FieldGet(providerField, staggerloc=staggerloc, &
      dimCount=fieldDimCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call ESMF_GridGet(grid, dimCount=gridDimCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    allocate(minIndex(gridDimCount), maxIndex(gridDimCount), stat=rc)
    if (ESMF_LogFoundAllocError(rc, msg="Allocating minIndex, maxIndex", &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call ESMF_GridGetIndex(grid, tileNo=1, &  !TODO: support tileCount>1!!
      minIndex=minIndex, maxIndex=maxIndex, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    allocate(gridToFieldMap(gridDimCount),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal gridToFieldMap failed.", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call ESMF_FieldGet(providerField, gridToFieldMap=gridToFieldMap, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (fieldDimCount - gridDimCount > 0) then
      ! query ungridded dim bounds
      allocate(ungriddedLBound(fieldDimCount-gridDimCount),stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of internal ungriddedLBound failed.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      allocate(ungriddedUBound(fieldDimCount-gridDimCount),stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of internal ungriddedUBound failed.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_FieldGet(providerField, ungriddedLBound=ungriddedLBound, &
        ungriddedUBound=ungriddedUBound, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif
    ! obtain the array from provider to be shared with acceptor
    call ESMF_FieldGet(providerField, array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return
    ! obtain the vm from provider to create the new field on the provider vm
    ! This way shared fields will only be send/receive during Timestamp
    ! propagation on actvive PETs. This is what you expect for a shared field.
    call ESMF_FieldGet(providerField, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return

    ! create a helper state needed for reconciliation across Connector VM
    state = ESMF_StateCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return

    ! determine active PETs
    call ESMF_VMGet(vm, localpet=localPet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    if (localPet>-1) then
      ! this is an active PET -> create the acceptorField
      
      !TODO: make sure that this FieldCreate() sets total widths correctly
      !TODO: difficult to do with current FieldCreate() for multiple DEs/PET
      if (fieldDimCount - gridDimCount > 0) then
        acceptorField=ESMF_FieldCreate(grid=grid, array=array, &
          datacopyflag=ESMF_DATACOPY_REFERENCE, staggerloc=staggerloc, &
          gridToFieldMap=gridToFieldMap, name=trim(fieldName), vm=vm, &
          ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return
      else
        acceptorField=ESMF_FieldCreate(grid=grid, array=array, &
          datacopyflag=ESMF_DATACOPY_REFERENCE, staggerloc=staggerloc, &
          gridToFieldMap=gridToFieldMap, name=trim(fieldName), vm=vm, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return
      end if
      call ESMF_StateAdd(state, (/acceptorField/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return
    endif
  
    ! reconcile across the entire Connector VM
    call ESMF_StateReconcile(state, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return
    call ESMF_StateGet(state, itemName=fieldName, field=acceptorField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return
    
    ! done with the helper state
    call ESMF_StateDestroy(state, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return

    ! clean-up
    deallocate(minIndex, maxIndex, stat=rc)
    if (ESMF_LogFoundDeallocError(rc, &
      msg="Deallocating minIndex, maxIndex", &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    deallocate(gridToFieldMap, stat=rc)
    if (ESMF_LogFoundDeallocError(rc, &
      msg="Deallocating gridToFieldMap", &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (fieldDimCount - gridDimCount > 0) then
      deallocate(ungriddedLBound, stat=rc)
      if (ESMF_LogFoundDeallocError(rc, &
        msg="Deallocating ungriddedLBound", &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      deallocate(ungriddedUBound, stat=rc)
      if (ESMF_LogFoundDeallocError(rc, &
        msg="Deallocating ungriddedUBound", &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif
    
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ShareFieldWithMesh(acceptorField, providerField, name, rc)
    type(ESMF_Field), intent(inout) :: acceptorField
    type(ESMF_Field), intent(in)    :: providerField
    character(*),     intent(in)    :: name
    integer,          intent(out)   :: rc
    ! local variables
    type(ESMF_Mesh)                 :: mesh
    type(ESMF_DistGrid)             :: distgrid
    integer                         :: fieldDimCount, gridDimCount
    integer(ESMF_KIND_I4), pointer  :: ungriddedLBound(:), ungriddedUBound(:)
    integer(ESMF_KIND_I4), pointer  :: gridToFieldMap(:)
    type(ESMF_MeshLoc)              :: meshloc
    type(ESMF_Array)                :: array
    type(ESMF_VM)                   :: vm
    character(ESMF_MAXSTR)          :: fieldName
    integer                         :: stat
    integer                         :: localPet
    type(ESMF_State)                :: state

    ! set RC
    rc = ESMF_SUCCESS
    
    ! queries
    call ESMF_FieldGet(acceptorField, mesh=mesh, name=fieldName, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return
    call ESMF_FieldGet(providerField, meshloc=meshloc, &
      dimCount=fieldDimCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call ESMF_MeshGet(mesh, elementDistgrid=distgrid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call ESMF_DistGridGet(distgrid, dimCount=gridDimCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    allocate(gridToFieldMap(gridDimCount),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal gridToFieldMap failed.", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call ESMF_FieldGet(providerField, gridToFieldMap=gridToFieldMap, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (fieldDimCount - gridDimCount > 0) then
      ! query ungridded dim bounds
      allocate(ungriddedLBound(fieldDimCount-gridDimCount),stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of internal ungriddedLBound failed.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      allocate(ungriddedUBound(fieldDimCount-gridDimCount),stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of internal ungriddedUBound failed.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_FieldGet(providerField, ungriddedLBound=ungriddedLBound, &
        ungriddedUBound=ungriddedUBound, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif
    ! obtain the array from provider to be shared with acceptor
    call ESMF_FieldGet(providerField, array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return
    ! obtain the vm from provider to create the new field on the provider vm
    ! This way shared fields will only be send/receive during Timestamp
    ! propagation on actvive PETs. This is what you expect for a shared field.
    call ESMF_FieldGet(providerField, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return

    ! create a helper state needed for reconciliation across Connector VM
    state = ESMF_StateCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return

    ! determine active PETs
    call ESMF_VMGet(vm, localpet=localPet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    if (localPet>-1) then
      ! this is an active PET -> create the acceptorField

      !TODO: make sure that this FieldCreate() sets total widths correctly
      !TODO: difficult to do with current FieldCreate() for multiple DEs/PET
      if (fieldDimCount - gridDimCount > 0) then
        acceptorField=ESMF_FieldCreate(mesh=mesh, array=array, &
          datacopyflag=ESMF_DATACOPY_REFERENCE, meshloc=meshloc, &
          gridToFieldMap=gridToFieldMap, name=fieldName, vm=vm, &
          ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return
      else
        acceptorField=ESMF_FieldCreate(mesh=mesh, array=array, &
          datacopyflag=ESMF_DATACOPY_REFERENCE, meshloc=meshloc, &
          gridToFieldMap=gridToFieldMap, name=fieldName, vm=vm, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return
      end if
      call ESMF_StateAdd(state, (/acceptorField/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return
    endif
      
    ! reconcile across the entire Connector VM
    call ESMF_StateReconcile(state, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return
    call ESMF_StateGet(state, itemName=fieldName, field=acceptorField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return
    
    ! done with the helper state
    call ESMF_StateDestroy(state, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return
      
    ! clean-up
    deallocate(gridToFieldMap, stat=rc)
    if (ESMF_LogFoundDeallocError(rc, &
      msg="Deallocating gridToFieldMap", &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (fieldDimCount - gridDimCount > 0) then
      deallocate(ungriddedLBound, stat=rc)
      if (ESMF_LogFoundDeallocError(rc, &
        msg="Deallocating ungriddedLBound", &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      deallocate(ungriddedUBound, stat=rc)
      if (ESMF_LogFoundDeallocError(rc, &
        msg="Deallocating ungriddedUBound", &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif
    
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeIPDv05p6a(connector, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: connector
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    character(*), parameter   :: rName="InitializeIPDv05p6a"
    character(ESMF_MAXSTR)    :: name
    integer                   :: verbosity, profiling, diagnostic
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(connector, name=name, verbosity=verbosity, &
      profiling=profiling, diagnostic=diagnostic, rc=rc)
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

    ! re-reconcile the States because they may have changed
    ! (previous proxy objects are dropped before fresh reconcile)
    if (btest(profiling,1)) then    ! PROFILE
      call ESMF_VMLogMemInfo("befP5a Reconcile")
    endif
    call NUOPC_Reconcile(importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call NUOPC_Reconcile(exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (btest(profiling,1)) then    ! PROFILE
      call ESMF_VMLogMemInfo("aftP5a Reconcile")
    endif

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

  subroutine InitializeIPDv05p6b(connector, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: connector
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    type type_CplList
      character(ESMF_MAXSTR), pointer :: cplList(:)
      integer                         :: j
    end type
    
    ! local variables
    character(*), parameter         :: rName="InitializeIPDv05p6b"
    character(ESMF_MAXSTR), pointer :: cplList(:), chopStringList(:)
    character(ESMF_MAXSTR), pointer :: cplSetList(:)
    character(ESMF_MAXSTR), pointer :: cplListTemp(:)
    type(type_CplList),     pointer :: cplSetListTemp(:)
    character(ESMF_MAXSTR)          :: cplName
    integer                         :: cplListSize, i, j
    integer                         :: cplSetListSize
    integer                         :: bondLevel, bondLevelMax
    character(ESMF_MAXSTR), pointer :: importNamespaceList(:)
    character(ESMF_MAXSTR), pointer :: exportNamespaceList(:)
    character(ESMF_MAXSTR), pointer :: importCplSetList(:)
    character(ESMF_MAXSTR), pointer :: exportCplSetList(:)
    character(ESMF_MAXSTR), pointer :: importStandardNameList(:)
    character(ESMF_MAXSTR), pointer :: exportStandardNameList(:)
    type(ESMF_Field),       pointer :: importFieldList(:)
    type(ESMF_Field),       pointer :: exportFieldList(:)
    integer                         :: iMatch, eMatch
    type(ESMF_Field)                :: iField, eField
    integer                         :: stat
    type(type_InternalState)        :: is
    logical                         :: foundFlag
    integer                         :: localrc
    logical                         :: existflag
    character(ESMF_MAXSTR)          :: connectionString
    character(ESMF_MAXSTR)          :: name, iString
    character(len=160)              :: msgString
    logical                         :: matchE, matchI
    integer                         :: count
    integer                         :: sIndex
    character(ESMF_MAXSTR)          :: iShareStatus, eShareStatus
    logical                         :: sharedFlag
    integer                   :: verbosity, diagnostic
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(connector, name=name, verbosity=verbosity, &
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

    ! prepare local pointer variables
    nullify(cplList)
    nullify(cplSetList)
    nullify(importStandardNameList)
    nullify(importFieldList)
    nullify(importNamespaceList)
    nullify(importCplSetList)
    nullify(exportStandardNameList)
    nullify(exportFieldList)
    nullify(exportNamespaceList)
    nullify(exportCplSetList)
    
    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(connector, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! get the cplList Attribute
    call NUOPC_CompAttributeGet(connector, name="CplList", &
      itemCount=cplListSize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    ! get the cplSetList Attribute
    call NUOPC_CompAttributeGet(connector, name="CplSetList", &
      itemCount=cplSetListSize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    ! check the cplSetList size
    if (cplListSize /= cplSetListSize) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="Bad internal error - CplSetList size must match CplList size!",&
        line=__LINE__, file=trim(name)//":"//FILENAME, &
        rcToReturn=rc)
      return  ! bail out
    endif
    if (cplListSize>0) then
      allocate(cplList(cplListSize), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of internal cplList() failed.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call NUOPC_CompAttributeGet(connector, name="CplList", valueList=cplList, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      allocate(cplSetList(cplSetListSize), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of internal cplSetList() failed.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call NUOPC_CompAttributeGet(connector, name="CplSetList", &
        valueList=cplSetList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif
    ! get the importState and exportState std lists
    call NUOPC_GetStateMemberLists(importState, importStandardNameList, &
      fieldList=importFieldList, namespaceList=importNamespaceList, &
      cplSetList=importCplSetList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call NUOPC_GetStateMemberLists(exportState, exportStandardNameList, &
      fieldList=exportFieldList, namespaceList=exportNamespaceList, &
      cplSetList=exportCplSetList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! prepare FieldBundles to store src and dst Fields
    is%wrap%srcFields = ESMF_FieldBundleCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    is%wrap%dstFields = ESMF_FieldBundleCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      
    ! prepare cplListTemp
    allocate(cplListTemp(cplListSize))
    j=1 ! initialize

    ! prepare lists of fields
    allocate(is%wrap%srcFieldList(cplListSize))
    allocate(is%wrap%dstFieldList(cplListSize))

    ! prepare set specific connection cpl list and field list and FieldBundles
    allocate(cplSetListTemp(is%wrap%cplSetCount))
    do i=1, is%wrap%cplSetCount
      is%wrap%cplSet(i)%srcFields = ESMF_FieldBundleCreate(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      is%wrap%cplSet(i)%dstFields = ESMF_FieldBundleCreate(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      count = getCount(is%wrap%cplSetList(i), cplSetList)
      is%wrap%cplSet(i)%count = 0 ! reset count
      allocate(is%wrap%cplSet(i)%srcFieldList(count))
      allocate(is%wrap%cplSet(i)%dstFieldList(count))
      allocate(cplSetListTemp(i)%cplList(count))
      cplSetListTemp(i)%j = 1 ! initialize
    enddo

    ! prepare chopStringList
    nullify(chopStringList)
    
    ! main loop over all entries in the cplList
    do i=1, cplListSize
      call NUOPC_ChopString(cplList(i), chopChar=":", &
        chopStringList=chopStringList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      cplName = chopStringList(1) ! first part is the standard name of cpl field
      deallocate(chopStringList)

      if (btest(verbosity,11).or.btest(verbosity,12)) then
        write (iString,'(I4)') i
        write (msgString, '(A)') trim(name)//": handle "// &
          "cplList("//trim(adjustl(iString))//"): "//trim(cplList(i))
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
      
      ! find import and export side match
      foundFlag = .false. ! reset
      do eMatch=1, size(exportStandardNameList)  ! consumer side
        if (.NOT.(cplSetList(i).EQ.exportCplSetList(eMatch))) cycle
        do iMatch=1, size(importStandardNameList)  ! producer side
          if (.NOT.(cplSetList(i).EQ.importCplSetList(iMatch))) cycle
          matchE = NUOPC_FieldDictionaryMatchSyno( &
            exportStandardNameList(eMatch), cplName, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          matchI = NUOPC_FieldDictionaryMatchSyno( &
            importStandardNameList(iMatch), cplName, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (matchE .and. matchI) then
            ! found a matching standard name pair
            ! -> determine bondLevel according to namespace matching
            bondLevel = &
              getBondLevel(importNamespaceList(iMatch), &
              exportNamespaceList(eMatch), &
              importCplSetList(iMatch), &
              exportCplSetList(eMatch))
              
            if (bondLevel == -1) cycle  ! break out and look for next match
            
            ! Getting to this place in the double loop means that the 
            ! standard name match has a connection that supports the match.
            
            ! -> look at the current ProducerConnection entry to see what to do
            eField = exportFieldList(eMatch)
            call NUOPC_GetAttribute(eField, name="ProducerConnection", &
              value=connectionString, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (index(trim(connectionString), "targeted:")==1) then
              ! this export field has been targeted -> obtain targeted bondLevel
              read (connectionString(10:len(connectionString)), "(i10)") &
                bondLevelMax  ! the bondLevel that was targeted
              if (bondLevel == bondLevelMax) then
                ! this is the targeted connection
                foundFlag = .true.
                write (connectionString, "('connected:', i10)") bondLevel
                call NUOPC_SetAttribute(eField, &
                  name="ProducerConnection", value=connectionString, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//FILENAME)) &
                  return  ! bail out
                exit
              endif
            endif
            
          endif
        enddo
        if (foundFlag) exit
      enddo
      
      if (.not.foundFlag) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="Should never get here: "//trim(cplName),&
          line=__LINE__, file=trim(name)//":"//FILENAME, &
          rcToReturn=rc)
        return  ! bail out
      endif
      
      if (iMatch>0 .and. eMatch>0) then
        ! there are matching Fields in the import and export States
        iField=importFieldList(iMatch)
        eField=exportFieldList(eMatch)
        sIndex=getIndex(importCplSetList(iMatch),is%wrap%cplSetList)
        ! add the fields to the field lists
        is%wrap%srcFieldList(i)=iField
        is%wrap%dstFieldList(i)=eField
        count=is%wrap%cplSet(sIndex)%count + 1
        is%wrap%cplSet(sIndex)%count=count
        is%wrap%cplSet(sIndex)%srcFieldList(count) = iField
        is%wrap%cplSet(sIndex)%dstFieldList(count) = eField
        ! check if the field pair is doing reference sharing
        call NUOPC_GetAttribute(iField, name="ShareStatusField", &
          value=iShareStatus, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        call NUOPC_GetAttribute(eField, name="ShareStatusField", &
          value=eShareStatus, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        sharedFlag = .false. ! reset
        if (trim(iShareStatus)=="shared" .and. trim(eShareStatus)=="shared") &
          sharedFlag = .true.
        if (sharedFlag) then
          ! sharing -> do NOT add the import and export Fields to FieldBundles
          if (btest(verbosity,12)) then
            write (msgString, '(A)') trim(name)//": "//&
              "- Field sharing between import and export side: NO remap."
            call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
          endif
        else
          ! not sharing -> add the import and export Fields to FieldBundles
          if (btest(verbosity,12)) then
            write (msgString, '(A)') trim(name)//": "//&
              "- no Field sharing between import and export side: remap."
            call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
              return  ! bail out
          endif
          ! add the import and export Fields to FieldBundles
          call ESMF_FieldBundleAdd(is%wrap%srcFields, (/iField/), &
            multiflag=.true., rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          call ESMF_FieldBundleAdd(is%wrap%dstFields, (/eField/), &
            multiflag=.true., rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          ! also add to cplListTemp
          cplListTemp(j)=cplList(i)
          j=j+1
          call ESMF_FieldBundleAdd(is%wrap%cplSet(sIndex)%srcFields, &
            (/iField/), multiflag=.true., rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          call ESMF_FieldBundleAdd(is%wrap%cplSet(sIndex)%dstFields, &
            (/eField/), multiflag=.true., rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          cplSetListTemp(sIndex)%cplList(cplSetListTemp(sIndex)%j)=cplList(i)
          cplSetListTemp(sIndex)%j=cplSetListTemp(sIndex)%j+1
        endif
          
        ! set the connected Attribute on import Field
        call NUOPC_SetAttribute(iField, name="Connected", value="true", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        call NUOPC_SetAttribute(iField, name="ConsumerConnection", value="true", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        ! set the connected Attribute on export Field
        call NUOPC_SetAttribute(eField, name="Connected", value="true", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        call NUOPC_SetAttribute(eField, name="ProducerConnection", value="true", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      else
        !TODO: Fields mentioned via stdname in Cpl metadata not found -> error?
      endif

    enddo

    ! SPECIALIZE by calling into attached method to precompute routehandle
    call ESMF_MethodExecute(connector, label=label_ComputeRouteHandle, &
      existflag=existflag, userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    if (.not.existflag) then
      ! if not specialized -> use default method to:
      ! precompute the regrid for all src to dst Fields
      if (is%wrap%cplSetCount > 1) then
        do i=1, is%wrap%cplSetCount
          call FieldBundleCplStore(is%wrap%cplSet(i)%srcFields, &
            is%wrap%cplSet(i)%dstFields, &
            cplList=cplSetListTemp(i)%cplList(1:cplSetListTemp(i)%j-1), &
            rh=is%wrap%cplSet(i)%rh, &
            termOrders=is%wrap%cplSet(i)%termOrders, name=name, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        enddo
      else
        call FieldBundleCplStore(is%wrap%srcFields, is%wrap%dstFields, &
          cplList=cplListTemp(1:j-1), rh=is%wrap%rh, &
          termOrders=is%wrap%termOrders, name=name, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      endif
      if (btest(verbosity,12)) then
        call ESMF_LogWrite(trim(name)//&
          ": called default label_ComputeRouteHandle", &
          ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      endif
    else
      if (btest(verbosity,12)) then
        call ESMF_LogWrite(trim(name)//&
          ": called specialized label_ComputeRouteHandle", &
          ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      endif    
    endif
    
    ! build Timestamp update packets
    !TODO: consider whether CplSet needs extra treatment here or not
    call BuildUpdatePackets(is%wrap%srcFieldList, is%wrap%dstFieldList, &
      is%wrap%updatePackets, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    if (btest(verbosity,4)) then
      write (msgString, '(A, ": CplSet List: ")') trim(name)
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, &
        msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, &
        rcToReturn=rc)) return  ! bail out
      do i=1, is%wrap%cplSetCount
        write (msgString, &
          '(A, ": ->CplSet(", I3, "): ", A60)') &
          trim(name), i, is%wrap%cplSetList(i)
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, &
          msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, &
          rcToReturn=rc)) return  ! bail out
        do j=1, cplSetListTemp(i)%j-1
          write (msgString, &
          '(A, ": -->Field(", I3, "): ", A60)') &
          trim(name), j, cplSetListTemp(i)%cplList(j)
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, &
            msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, &
            rcToReturn=rc)) return  ! bail out
        enddo
      enddo
    endif
    
    ! clean-up
    do i=1, is%wrap%cplSetCount
      if (associated(cplSetListTemp(i)%cplList)) &
        deallocate(cplSetListTemp(i)%cplList)
    enddo
    if (associated(cplList)) deallocate(cplList)
    if (associated(cplSetList)) deallocate(cplSetList)
    if (associated(cplListTemp)) deallocate(cplListTemp)
    if (associated(cplSetListTemp)) deallocate(cplSetListTemp)
    if (associated(importStandardNameList)) deallocate(importStandardNameList)
    if (associated(importFieldList)) deallocate(importFieldList)
    if (associated(importNamespaceList)) deallocate(importNamespaceList)
    if (associated(importCplSetList)) deallocate(importCplSetList)
    if (associated(exportStandardNameList)) deallocate(exportStandardNameList)
    if (associated(exportFieldList)) deallocate(exportFieldList)
    if (associated(exportNamespaceList)) deallocate(exportNamespaceList)
    if (associated(exportCplSetList)) deallocate(exportCplSetList)
    
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

  subroutine BuildUpdatePackets(srcFieldList, dstFieldList, updatePackets, &
    verbosity, rc)
    type(ESMF_Field), pointer           :: srcFieldList(:)
    type(ESMF_Field), pointer           :: dstFieldList(:)
    type(type_UpdatePacket), pointer    :: updatePackets
    integer,                intent(in)  :: verbosity
    integer,                intent(out) :: rc
    ! local variables
    integer                           :: i, j, k, stat, petCount, localPet
    integer                           :: srcLocalPet, dstLocalPet, fieldCount
    integer, allocatable              :: srcLocalPetList(:), dstLocalPetList(:)
    integer, allocatable              :: auxList(:)
    integer                           :: srcPetCount, dstPetCount
    type(ESMF_VM)                     :: vm, srcVM, dstVM
    character(ESMF_MAXSTR)            :: fieldName
    character(len=240)                :: msgString
    logical                           :: createNewPacket
    logical                           :: mismatch
    integer                           :: helperIn, helperOut
    type(type_UpdatePacket), pointer  :: upE, upN, upT
    integer                           :: upCount
    type type_upFields
      type(type_UpdatePacket), pointer  :: up
    end type
    type(type_upFields), pointer      :: upFields(:)

    rc = ESMF_SUCCESS
    
    nullify(updatePackets)
    
    fieldCount = size(srcFieldList)
    
    allocate(upFields(fieldCount), stat=stat)
    if (ESMF_LogFoundAllocError(stat, msg="allocating upFields", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    
    call ESMF_VMGetCurrent(vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    
    allocate(srcLocalPetList(petCount), dstLocalPetList(petCount), stat=stat)
    if (ESMF_LogFoundAllocError(stat, msg="allocating LocalPetLists", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    
    allocate(auxList(petCount), stat=stat)
    if (ESMF_LogFoundAllocError(stat, msg="allocating auxList", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    upCount = 0 ! initialize
    createNewPacket = .true.  ! make sure first valid packet is being added
    do i=1, fieldCount
      call ESMF_FieldGet(srcFieldList(i), vm=srcVM, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      call ESMF_VMGet(srcVM, localPet=srcLocalPet, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      if (btest(verbosity,12)) then
        call ESMF_FieldGet(srcFieldList(i), name=fieldName, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        write(msgString,*) "BuildUpdatePackets: "//trim(fieldName)//&
          ", srcLocalPet=", srcLocalPet
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      endif
      call ESMF_FieldGet(dstFieldList(i), vm=dstVM, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      call ESMF_VMGet(dstVM, localPet=dstLocalPet, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      if (btest(verbosity,12)) then
        call ESMF_FieldGet(dstFieldList(i), name=fieldName, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        write(msgString,*) "BuildUpdatePackets: "//trim(fieldName)//&
          ", dstLocalPet=", dstLocalPet
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      endif       
      if (.not.createNewPacket) then
        ! some packets exist already -> must check against them for match
        createNewPacket = .true.  ! initialize, might be reset if packet found
        upE=>updatePackets  ! start with the last packet that was added
        do while (associated(upE))
          ! see if this packet has the same src/dstLocalPet pattern combination 
          mismatch = (upE%srcLocalPet/=srcLocalPet) .or. &
            (upE%dstLocalPet/=dstLocalPet)
          helperIn = 0  ! initialize
          if (mismatch) helperIn=1
          ! implement a logical OR operation based on REDUCE_SUM
          call ESMF_VMAllFullReduce(vm, sendData=(/helperIn/), &
            recvData=helperOut, count=1, reduceflag=ESMF_REDUCE_SUM, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
          ! determine the global match condition
          if (helperOut == 0) then
            ! pattern matches on all PETs -> matching UpdatePacket found
            createNewPacket = .false. ! no new packet needed
            exit  ! break out of the search loop
          endif
          upE=>upE%prev   ! move to previous element
        enddo
      endif
      
      if (createNewPacket) then
        ! create a new packet
        upCount = upCount+1
        ! AllGather srcLocalPet and dstLocalPet onto each PET
        call ESMF_VMAllGather(vm, (/srcLocalPet/), srcLocalPetList, count=1, &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        call ESMF_VMAllGather(vm, (/dstLocalPet/), dstLocalPetList, count=1, &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        allocate(upE, stat=stat)
        if (ESMF_LogFoundAllocError(stat, msg="allocating new update packet", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        upE%prev=>updatePackets  ! link new element to previous list head
        updatePackets=>upE       ! list head now pointing to new element
        ! fill the new packet VM information
        upE%srcLocalPet=srcLocalPet
        upE%dstLocalPet=dstLocalPet
        upE%fieldCount = 0 ! initialize
        ! Determine srcPetCount and dstPetCount
        srcPetCount = 0 ! initialize
        dstPetCount = 0 ! initialize
        do j=1, petCount
          if (srcLocalPetList(j)>-1) srcPetCount=srcPetCount+1
          if (dstLocalPetList(j)>-1) dstPetCount=dstPetCount+1
        enddo
        if (btest(verbosity,12)) then
          write(msgString,*) "creating new UpdatePacket for srcPetCount=", &
            srcPetCount, " dstPetCount=", dstPetCount
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        endif
        ! Construct local sendToPets
        k = 0 ! initialize
        do j=1, petCount
          if (dstLocalPetList(j)>-1) then
            ! this PET holds a dstField
            if (mod(dstLocalPetList(j),srcPetCount)==srcLocalPet) then
              ! localPet responsible for sending to this destination
              k = k+1
              auxList(k) = j-1  ! PETs are basis 0
            endif
          endif
        enddo
        allocate(upE%sendToPets(k), stat=stat)
        if (ESMF_LogFoundAllocError(stat, msg="allocating sendToPets", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        if (k>0) upE%sendToPets(1:k) = auxList(1:k)  ! transfer the elements
        if (btest(verbosity,12)) then
          write(msgString,*) "upE%sendToPets=", upE%sendToPets
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        endif   
        ! Determine recvFromPet
        upE%recvFromPet = -1  ! initialize, indicating not receiving any info
        if (dstLocalPet>-1) then
          ! localPet holds a dstField
          do j=1, petCount
            if (mod(dstLocalPet,srcPetCount)==srcLocalPetList(j)) then
              ! found PET that localPet is receiving from 
              upE%recvFromPet = j-1 ! PETs are basis 0
            endif
          enddo
        endif
        if (btest(verbosity,12)) then
          write(msgString,*) "upE%recvFromPet=", upE%recvFromPet
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        endif
        ! reset flag for next iteration
        createNewPacket = .false.
      endif
      
      ! either case, new packet or existing packet found, fill in field info
      if (srcLocalPet>-1 .or. dstLocalPet>-1) then
        ! upE must be valid
        upE%fieldCount = upE%fieldCount+1
        upFields(i)%up => upE  ! keep track of packet for this field index
      else
        nullify(upFields(i)%up)
      endif
      
      if (btest(verbosity,12)) then
        write(msgString,*) "after field add, now upE%fieldCount=", upE%fieldCount
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      endif

    enddo

    ! traverse all packets and either prepare internal allocations, or delete
    upCount = 0   ! re-initialize
    upN=>null()   ! initialize
    upE=>updatePackets
    do while (associated(upE))
      if (upE%fieldCount > 0) then
        ! this is an interacting packet -> keep
        upCount = upCount+1
        ! allocate for the known number of fields handled
        allocate(upE%fieldIndex(upE%fieldCount), stat=stat)
        if (ESMF_LogFoundAllocError(stat, msg="allocating upE%fieldIndex", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        allocate(upE%sendBuffer(10,upE%fieldCount), stat=stat)
        if (ESMF_LogFoundAllocError(stat, msg="allocating upE%sendBuffer", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        allocate(upE%recvBuffer(10,upE%fieldCount), stat=stat)
        if (ESMF_LogFoundAllocError(stat, msg="allocating upE%recvBuffer", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        ! reset the fieldCount for next loop usage
        upE%fieldCount = 0
        ! step to next element
        upN=>upE
        upE=>upE%prev
      else
        ! this packet has no interactions -> optimize execution by elimination
        upT=>upE%prev
        ! unhook upE
        if (associated(upN)) then
          upN%prev=>upT
        else
          updatePackets=>upT
        endif
        ! internal clean-up
        deallocate(upE%sendToPets, stat=stat)
        if (ESMF_LogFoundDeallocError(stat, msg="deallocating sendToPets", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        ! deallocate upE itself
        deallocate(upE, stat=stat)
        if (ESMF_LogFoundDeallocError(stat, msg="deallocating upE", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        ! step to next element
        upE=>upT
      endif
    enddo
    
    ! loop through fields one more time and set references inside packets
    do i=1, fieldCount
      upE => upFields(i)%up
      if (associated(upE)) then
        upE%fieldCount = upE%fieldCount+1
        upE%fieldIndex(upE%fieldCount) = i
      endif
    enddo
    
    if (btest(verbosity,12)) then
      write(msgString,*) "BuildUpdatePackets: final UpdatePacket count=", &
        upCount
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    endif

    ! clean-up
    deallocate(auxList, stat=stat)
    if (ESMF_LogFoundDeallocError(stat, msg="deallocating auxList", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    deallocate(srcLocalPetList, dstLocalPetList, stat=stat)
    if (ESMF_LogFoundDeallocError(stat, msg="deallocating LocalPetLists", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    deallocate(upFields, stat=stat)
    if (ESMF_LogFoundDeallocError(stat, msg="deallocating upFields", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    
  end subroutine
    
  !-----------------------------------------------------------------------------

  subroutine ExecuteUpdatePackets(srcFieldList, dstFieldList, updatePackets, rc)
    type(ESMF_Field), pointer           :: srcFieldList(:)
    type(ESMF_Field), pointer           :: dstFieldList(:)
    type(type_UpdatePacket), pointer    :: updatePackets
    integer,                intent(out) :: rc
    ! local variables
    type(ESMF_VM)                     :: vm
    type(type_UpdatePacket), pointer  :: upE
    integer                           :: i, j, jj
    integer, pointer                  :: bufferPtr(:)
    
    rc = ESMF_SUCCESS
    
    call ESMF_VMGetCurrent(vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    ! traverse all packets and post the receives
    upE=>updatePackets
    do while (associated(upE))
      if (upE%recvFromPet > -1) then
        ! post the receive
        bufferPtr => upE%recvBuffer(:,1)
        call ESMF_VMRecv(vm, bufferPtr, size(upE%recvBuffer), &
          upE%recvFromPet, syncflag=ESMF_SYNC_NONBLOCKING, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      endif
      ! step to next element
      upE=>upE%prev
    enddo
    
    ! traverse all packets, fill the sendBuffer and post the sends
    upE=>updatePackets
    do while (associated(upE))
      if (size(upE%sendToPets)>0) then
        ! there are messages to be sent -> fill the sendBuffer
        do j=1, upE%fieldCount
          jj = upE%fieldIndex(j)
          call ESMF_AttributeGet(srcFieldList(jj), &
            name="TimeStamp", valueList=upE%sendBuffer(:,j), &
            convention="NUOPC", purpose="Instance", &
            attnestflag=ESMF_ATTNEST_ON, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        enddo
        ! post all the sends
        bufferPtr => upE%sendBuffer(:,1)
        do i=1, size(upE%sendToPets)
          call ESMF_VMSend(vm, bufferPtr, size(upE%sendBuffer), &
            upE%sendToPets(i), syncflag=ESMF_SYNC_NONBLOCKING, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        enddo
      endif
      ! step to next element
      upE=>upE%prev
    enddo

    ! wait for all outstanding communications the localPet is engaged in
    call ESMF_VMCommWaitAll(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    
    ! traverse all packets and fill the received timestamps into fields
    upE=>updatePackets
    do while (associated(upE))
      if (upE%recvFromPet > -1) then
        ! fill in the reiceived timestamps
        do j=1, upE%fieldCount
          jj = upE%fieldIndex(j)
          call ESMF_AttributeSet(dstFieldList(jj), &
            name="TimeStamp", valueList=upE%recvBuffer(:,j), &
            convention="NUOPC", purpose="Instance", &
            attnestflag=ESMF_ATTNEST_ON, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        enddo
      endif
      ! step to next element
      upE=>upE%prev
    enddo

  end subroutine
    
  !-----------------------------------------------------------------------------

  subroutine DestroyUpdatePackets(updatePackets, rc)
    type(type_UpdatePacket), pointer    :: updatePackets
    integer,                intent(out) :: rc
    ! local variables
    type(type_UpdatePacket), pointer    :: upE
    integer                             :: stat

    rc = ESMF_SUCCESS
    
    ! take down updatePackets
    do while (associated(updatePackets))
      upE=>updatePackets
      updatePackets=>updatePackets%prev
      ! clean up all allocations inside of upE
      deallocate(upE%fieldIndex, stat=stat)
      if (ESMF_LogFoundDeallocError(stat, msg="deallocating fieldIndex", &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      deallocate(upE%sendToPets, stat=stat)
      if (ESMF_LogFoundDeallocError(stat, msg="deallocating sendToPets", &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      deallocate(upE%sendBuffer, stat=stat)
      if (ESMF_LogFoundDeallocError(stat, msg="deallocating sendBuffer", &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      deallocate(upE%recvBuffer, stat=stat)
      if (ESMF_LogFoundDeallocError(stat, msg="deallocating recvBuffer", &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      ! deallocate upE itself
      deallocate(upE, stat=stat)
      if (ESMF_LogFoundDeallocError(stat, msg="deallocating upE", &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    enddo

  end subroutine
    
  !-----------------------------------------------------------------------------

  subroutine InitializeIPDv00p2a(connector, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: connector
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    character(*), parameter               :: rName="InitializeIPDv00p2a"
    type(ESMF_Clock)                      :: internalClock
    character(ESMF_MAXSTR)                :: name
    integer                               :: verbosity

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(connector, name=name, verbosity=verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! intro
    call NUOPC_LogIntro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! Simply the combination of IPDv05p3 + IPDv05p6a
    call InitializeIPDv05p3(connector, importState, exportState, clock, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call InitializeIPDv05p6a(connector, importState, exportState, clock, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! extro
    call NUOPC_LogExtro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeIPDv00p2b(connector, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: connector
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    character(*), parameter               :: rName="InitializeIPDv00p2b"
    type(ESMF_Clock)                      :: internalClock
    character(ESMF_MAXSTR)                :: name
    integer                               :: verbosity

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(connector, name=name, verbosity=verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! intro
    call NUOPC_LogIntro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! Simply same as IPDv05p6b
    call InitializeIPDv05p6b(connector, importState, exportState, clock, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! extro
    call NUOPC_LogExtro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine Run(connector, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: connector
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    character(*), parameter   :: rName="Run"
    type(type_InternalState)  :: is
    type(ESMF_VM)             :: vm
    integer                   :: localrc
    logical                   :: existflag
    logical                   :: routeHandleIsCreated
    integer                   :: rootPet, rootVas, vas, petCount
    character(ESMF_MAXSTR)    :: compName, pLabel
    character(len=160)        :: msgString
    integer                   :: phase
    integer                   :: verbosity, profiling, diagnostic
    character(ESMF_MAXSTR)    :: name
    integer                   :: i
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString

    real(ESMF_KIND_R8)        :: timeBase, time0, time

    rc = ESMF_SUCCESS

    ! PROFILE base time
    call ESMF_VMWtime(timeBase)
    time0=timeBase

    ! query the component for info
    call NUOPC_CompGet(connector, name=name, verbosity=verbosity, &
      profiling=profiling, diagnostic=diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        
    ! intro
    call NUOPC_LogIntro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! get the compName and currentPhase
    call ESMF_CplCompGet(connector, name=compName, currentPhase=phase, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle verbosity
    if (btest(verbosity,13)) then
      call NUOPC_CompSearchRevPhaseMap(connector, ESMF_METHOD_RUN, &
        phaseIndex=phase, phaseLabel=pLabel, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      write (msgString,"(A)") ">>>"//trim(compName)//&
      " entered Run (phase="//trim(adjustl(pLabel))//")"
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    
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
    if (btest(diagnostic,4)) then
      call NUOPC_Write(importState, fileNamePrefix="diagnostic_"//&
        trim(name)//"_"//trim(rName)//"_enter_import_"//trim(currTimeString)//&
        "_", timeslice=1, status=ESMF_FILESTATUS_REPLACE, relaxedFlag=.true., &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    if (btest(diagnostic,5)) then
      call NUOPC_Write(exportState, fileNamePrefix="diagnostic_"//&
        trim(name)//"_"//trim(rName)//"_enter_export_"//trim(currTimeString)//&
        "_", timeslice=1, status=ESMF_FILESTATUS_REPLACE, relaxedFlag=.true., &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    
    ! handle profiling
    if (btest(profiling,0)) then
      call ESMF_VMWtime(time)
      write (msgString, *) trim(name)//": Profile 01 time=   ", &
        time-time0, time-timeBase
        time0=time
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
    endif

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(connector, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      
    ! store the incoming clock as driverClock in internal state
    is%wrap%driverClock = clock

    if (btest(profiling,0)) then
      call ESMF_VMWtime(time)
      write (msgString, *) trim(name)//": Profile 02 time=   ", &
        time-time0, time-timeBase
        time0=time
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
    endif

    !TODO: here may be the place to ensure incoming States are consistent
    !TODO: with the Fields held in the FieldBundle inside the internal State?
      
    if (btest(profiling,0)) then
      call ESMF_VMWtime(time)
      write (msgString, *) trim(name)//": Profile 03 time=   ", &
        time-time0, time-timeBase
        time0=time
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
    endif

    ! SPECIALIZE by calling into attached method to execute routehandle
    call ESMF_MethodExecute(connector, label=label_ExecuteRouteHandle, &
      existflag=existflag, userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! handle profiling
    if (btest(profiling,0)) then
      call ESMF_VMWtime(time)
      write (msgString, *) trim(name)//": Profile 04 time=   ", &
        time-time0, time-timeBase
        time0=time
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
    endif

    if (.not.existflag) then
      ! if not specialized -> use default method to:
      ! execute the regrid operation
      if (is%wrap%cplSetCount > 1) then
        do i=1, is%wrap%cplSetCount
          routeHandleIsCreated = ESMF_RouteHandleIsCreated( &
            is%wrap%cplSet(i)%rh, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (routeHandleIsCreated) then
            call ESMF_FieldBundleSMM(is%wrap%cplSet(i)%srcFields, &
              is%wrap%cplSet(i)%dstFields, &
              routehandle=is%wrap%cplSet(i)%rh, &
              termorderflag=is%wrap%cplSet(i)%termOrders, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          endif
        enddo
      else
        routeHandleIsCreated = ESMF_RouteHandleIsCreated(is%wrap%rh, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        if (routeHandleIsCreated) then
          call ESMF_FieldBundleSMM(is%wrap%srcFields, is%wrap%dstFields, &
            routehandle=is%wrap%rh, termorderflag=is%wrap%termOrders, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        endif
      endif
      if (btest(verbosity,14)) then
        call ESMF_LogWrite(trim(name)//&
          ": called default label_ExecuteRouteHandle", &
          ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      endif
    else
      if (btest(verbosity,14)) then
        call ESMF_LogWrite(trim(name)//&
          ": called specialized label_ExecuteRouteHandle", &
          ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      endif    
    endif
    
    ! handle profiling
    if (btest(profiling,0)) then
      call ESMF_VMWtime(time)
      write (msgString, *) trim(name)//": Profile 05 time=   ", &
        time-time0, time-timeBase
        time0=time
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
    endif

    ! Next update the TimeStamp metadata on the export Fields....
    call ExecuteUpdatePackets(is%wrap%srcFieldList, is%wrap%dstFieldList, &
      is%wrap%updatePackets, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle diagnostic
    if (btest(diagnostic,6)) then
      call NUOPC_Write(importState, fileNamePrefix="diagnostic_"//&
        trim(name)//"_"//trim(rName)//"_exit_import_"//trim(currTimeString)//&
        "_", timeslice=1, status=ESMF_FILESTATUS_REPLACE, relaxedFlag=.true., &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    if (btest(diagnostic,7)) then
      call NUOPC_Write(exportState, fileNamePrefix="diagnostic_"//&
        trim(name)//"_"//trim(rName)//"_exit_export_"//trim(currTimeString)//&
        "_", timeslice=1, status=ESMF_FILESTATUS_REPLACE, relaxedFlag=.true., &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    
    ! handle verbosity
    if (btest(verbosity,13)) then
      write (msgString,"(A)") "<<<"//trim(compName)//&
      " leaving Run (phase="//trim(adjustl(pLabel))//")"
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
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

  subroutine Finalize(connector, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: connector
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    character(*), parameter   :: rName="Finalize"
    integer                   :: stat
    type(type_InternalState)  :: is
    integer                   :: localrc
    logical                   :: existflag
    logical                   :: routeHandleIsCreated
    character(ESMF_MAXSTR)    :: name
    integer                   :: verbosity, diagnostic
    integer                   :: i
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(connector, name=name, verbosity=verbosity, &
      diagnostic=diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! intro
    call NUOPC_LogIntro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(connector, label_InternalState, is, rc)
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
    if (btest(diagnostic,8)) then
      call NUOPC_Write(importState, fileNamePrefix="diagnostic_"//&
        trim(name)//"_"//trim(rName)//"_enter_import_"//trim(currTimeString)//&
        "_", timeslice=1, status=ESMF_FILESTATUS_REPLACE, relaxedFlag=.true., &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    if (btest(diagnostic,9)) then
      call NUOPC_Write(exportState, fileNamePrefix="diagnostic_"//&
        trim(name)//"_"//trim(rName)//"_enter_export_"//trim(currTimeString)//&
        "_", timeslice=1, status=ESMF_FILESTATUS_REPLACE, relaxedFlag=.true., &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    
    ! SPECIALIZE by calling into attached method to release routehandle
    call ESMF_MethodExecute(connector, label=label_ReleaseRouteHandle, &
      existflag=existflag, userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    if (.not.existflag) then
      ! if not specialized -> use default method to:
      ! release the regrid operation
      if (is%wrap%cplSetCount > 1) then
        do i=1, is%wrap%cplSetCount
          routeHandleIsCreated = ESMF_RouteHandleIsCreated( &
            is%wrap%cplSet(i)%rh, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (routeHandleIsCreated) then
            call ESMF_FieldBundleRegridRelease(is%wrap%cplSet(i)%rh, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
             line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          endif
        enddo
      else
        routeHandleIsCreated = ESMF_RouteHandleIsCreated(is%wrap%rh, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        if (routeHandleIsCreated) then
          call ESMF_FieldBundleRegridRelease(is%wrap%rh, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        endif
      endif
      if (btest(verbosity,15)) then
        call ESMF_LogWrite(trim(name)//&
          ": called default label_ReleaseRouteHandle", &
          ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      endif
    else
      if (btest(verbosity,15)) then
        call ESMF_LogWrite(trim(name)//&
          ": called specialized label_ReleaseRouteHandle", &
          ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      endif    
    endif

    ! SPECIALIZE by calling into optional attached method
    call ESMF_MethodExecute(connector, label=label_Finalize, &
      existflag=existflag, userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! destroy Timestamp update packets
    !TODO: consider whether CplSet needs extra treatment here or not
    call DestroyUpdatePackets(is%wrap%updatePackets, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! deallocate and destroy remaining internal state members
    do i=1, is%wrap%cplSetCount
      deallocate(is%wrap%cplSet(i)%srcFieldList, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg="Deallocation of internal state srcFieldList member failed.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      deallocate(is%wrap%cplSet(i)%dstFieldList, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg="Deallocation of internal state dstFieldList member failed.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_FieldBundleDestroy(is%wrap%cplSet(i)%srcFields, &
        noGarbage=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      call ESMF_FieldBundleDestroy(is%wrap%cplSet(i)%dstFields, &
        noGarbage=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      if (associated(is%wrap%cplSet(i)%termOrders)) then
        deallocate(is%wrap%cplSet(i)%termOrders, stat=stat)
        if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
          msg="Deallocation of termOrders list.", &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
    enddo
    deallocate(is%wrap%cplSet, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state cplSet member failed.", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (associated(is%wrap%cplSetList)) then
      deallocate(is%wrap%cplSetList, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg="Deallocation of cplSetList.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    deallocate(is%wrap%srcFieldList, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state srcFieldList member failed.", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    deallocate(is%wrap%dstFieldList, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state dstFieldList member failed.", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call ESMF_FieldBundleDestroy(is%wrap%srcFields, noGarbage=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call ESMF_FieldBundleDestroy(is%wrap%dstFields, noGarbage=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call ESMF_StateDestroy(is%wrap%state, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (associated(is%wrap%termOrders)) then
      deallocate(is%wrap%termOrders, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg="Deallocation of termOrders list.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    
    ! deallocate internal state memory
    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
      
    ! handle diagnostic
    if (btest(diagnostic,10)) then
      call NUOPC_Write(importState, fileNamePrefix="diagnostic_"//&
        trim(name)//"_"//trim(rName)//"_exit_import_"//trim(currTimeString)//&
        "_", timeslice=1, status=ESMF_FILESTATUS_REPLACE, relaxedFlag=.true., &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    if (btest(diagnostic,11)) then
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
  !----- Helper routines below ...
  !-----------------------------------------------------------------------------

  function getBondLevel(imNamespace, exNamespace, imCplSet, exCplSet)
    integer           :: getBondLevel
    character(len=*)  :: imNamespace, exNamespace
    character(len=*)  :: imCplSet, exCplSet
    character(len=80) :: imKey1, imKey2, imKey
    character(len=80) :: exKey1, exKey2, exKey
    integer           :: imMark1, imMark2
    integer           :: exMark1, exMark2
    
    getBondLevel = 1 ! reset

    ! check for coupling set match
    if (imCplSet /= exCplSet) then
      getBondLevel = -1  ! mark abort
      return          ! break out
    endif

    imMark1 = 1 ! reset
    exMark1 = 1 ! reset
    ! key1 always exists
    imMark2 = index(imNamespace, ":")
    if (imMark2 == 0) then
      imMark2 = len(imNamespace)
    else
      imMark2 = imMark2 - 1
    endif
    imKey1 = trim(imNamespace(imMark1:imMark2))
    imMark1 = imMark2
    exMark2 = index(exNamespace, ":")
    if (exMark2 == 0) then
      exMark2 = len(exNamespace)
    else
      exMark2 = exMark2 - 1
    endif
    exKey1 = trim(exNamespace(exMark1:exMark2))
    exMark1 = exMark2
    ! key2 may or may not exist
    if (imMark1 < len(imNamespace)) then
      imMark1 = imMark1 + 2   ! skip over the previously found ":"
      imMark2 = index(imNamespace(imMark1:len(imNamespace)), ":")
      if (imMark2 == 0) then
        imMark2 = len(imNamespace)
      else
        imMark2 = imMark1 + imMark2 - 2
      endif
      imKey2 = trim(imNamespace(imMark1:imMark2))
    else
      imKey2 = "" ! empty string
    endif
    if (exMark1 < len(exNamespace)) then
      exMark1 = exMark1 + 2   ! skip over the previously found ":"
      exMark2 = index(exNamespace(exMark1:len(exNamespace)), ":")
      if (exMark2 == 0) then
        exMark2 = len(exNamespace)
      else
        exMark2 = exMark1 + exMark2 - 2
      endif
      exKey2 = trim(exNamespace(exMark1:exMark2))
    else
      exKey2 = "" ! empty string
    endif
    
#if 0
print *, "found match:"// &
  " imKey1=",trim(imKey1), " imKey2=",trim(imKey2), &
  " exKey1=",trim(exKey1), " exKey2=",trim(exKey2)
#endif
              
    ! check for key1 x key2 cross match
    if (imKey2 /= "") then
      if (imKey2 /= exKey1) then
        getBondLevel = -1  ! mark abort
        return          ! break out
      endif
      getBondLevel = getBondLevel + 1
    endif
    if (exKey2 /= "") then
      if (exKey2 /= imKey1) then
        getBondLevel = -1  ! mark abort
        return          ! break out
      endif
      getBondLevel = getBondLevel + 1
    endif
    
    !TODO: it may make sense to check for further nested namespace match
   
  end function

  !-----------------------------------------------------------------------------

  function getIndex(value, list)
    integer                    :: getIndex
    character(len=*)           :: value
    character(len=*), pointer  :: list(:)
    integer                    :: i

    if (associated(list)) then
      do i=0, size(list)-1
        if (value.EQ.list(i+lbound(list,1))) then 
          getIndex = i+lbound(list,1)
          return
        endif
      enddo
    endif

    getIndex = 0

  end function

  !-----------------------------------------------------------------------------

  function getCount(value, list)
    integer                   :: getCount
    character(len=*)          :: value
    character(len=*), pointer :: list(:)
    integer                   :: i

    getCount = 0

    if (associated(list)) then
      do i=0, size(list)-1
        if (value.EQ.list(i+lbound(list,1))) getCount = getCount + 1
      enddo
    endif

  end function

  !-----------------------------------------------------------------------------

  subroutine getUniqueList(list, uniqueList, uniqueCount, rc)
    character(len=*)      , pointer   :: list(:)
    character(len=*)      , pointer   :: uniqueList(:)
    integer               , optional  :: uniqueCount
    integer               , optional  :: rc
    integer                           :: l_count
    character(ESMF_MAXSTR), pointer   :: l_uniqueList(:)
    integer                           :: i,stat

    if (present(rc)) rc = ESMF_SUCCESS

    if (associated(uniqueList)) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="uniqueList must enter unassociated", &
        line=__LINE__, &
        file=FILENAME, &
        rcToReturn=rc)
      return  ! bail out
    endif

    if (.NOT.associated(list)) then
      if (present(uniqueCount)) then
        uniqueCount = 0
      endif

      allocate(uniqueList(0), stat=stat)
      if (ESMF_LogFoundAllocError(stat, msg="allocating uniqueList", &
        line=__LINE__, &
        file=FILENAME, &
        rcToReturn=rc)) &
        return  ! bail out
      return
    endif

    if (len(list) > len(uniqueList)) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="list string length greater than uniqueList string length!", &
        line=__LINE__, &
        file=FILENAME, &
        rcToReturn=rc)
      return  ! bail out
    endif

    allocate(l_uniqueList(size(list)), stat=stat) ! temporary list
    if (ESMF_LogFoundAllocError(stat, msg="allocating l_uniqueList", &
      line=__LINE__, &
      file=FILENAME, &
      rcToReturn=rc)) &
      return  ! bail out

    l_count = 0
    do i=0, size(list)-1
      if (l_count > 0) then
        if (ANY(list(i+lbound(list,1)).EQ.l_uniqueList(1:l_count))) cycle
      endif
      l_count = l_count + 1
      l_uniqueList(l_count) = list(i+lbound(list,1))
    enddo

    if (present(uniqueCount)) then
      uniqueCount = l_count
    endif

    allocate(uniqueList(l_count), stat=stat)
    if (ESMF_LogFoundAllocError(stat, msg="allocating uniqueList", &
      line=__LINE__, &
      file=FILENAME, &
      rcToReturn=rc)) &
      return  ! bail out

    if (l_count > 0) then 
      uniqueList(1:l_count) = l_uniqueList(1:l_count)
    endif

    deallocate(l_uniqueList)
    if (ESMF_LogFoundDeallocError(stat, msg="deallocating l_uniqueList", &
      line=__LINE__, &
      file=FILENAME, &
      rcToReturn=rc)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine printStringList(prefix, stringList)
    character(len=*)                      :: prefix
    character(ESMF_MAXSTR), pointer       :: stringList(:)
    integer                               :: i
    
    print *, trim(prefix), ":"
    if (associated(stringList)) then
      print *, "size: ", size(stringList)
      do i=1, size(stringList)
        print *, i,": ", trim(stringList(i))
      enddo
    else
      print *, "stringList is unassociated!!!"
    endif
    
  end subroutine
    
  !-----------------------------------------------------------------------------

  subroutine FieldBundleCplStore(srcFB, dstFB, cplList, rh, termOrders, name, &
    rc)
    ! this method will destroy srcFB/dstFB, and replace with newly created FBs
    ! order of fields in outgoing srcFB/dstFB may be different from incoming
    ! order of elements in termOrders matches those in outgoing srcFB/dstFB
    type(ESMF_FieldBundle),    intent(inout)         :: srcFB
    type(ESMF_FieldBundle),    intent(inout)         :: dstFB
    character(*)                                     :: cplList(:)
    type(ESMF_RouteHandle),    intent(inout)         :: rh
    type(ESMF_TermOrder_Flag), pointer               :: termOrders(:)
    character(*),              intent(in)            :: name
    integer,                   intent(out), optional :: rc
    
    ! local variables
    integer                         :: localrc
    integer                         :: i, j, k, count, stat, localDeCount
    integer                         :: iRegrid, iRedist
    type(ESMF_Field), pointer       :: srcFields(:), dstFields(:)
    type(ESMF_FieldBundle)          :: srcFBRedist, dstFBRedist
    integer                         :: rraShift, vectorLengthShift
    type(ESMF_RouteHandle)          :: rhh
    integer(ESMF_KIND_I4), pointer  :: factorIndexList(:,:)
    real(ESMF_KIND_R8), pointer     :: factorList(:)
    character(ESMF_MAXSTR), pointer :: chopStringList(:)
    character(ESMF_MAXSTR), pointer :: chopSubString(:), chopSubSubString(:)
    character(len=160)              :: msgString
    character(len=480)              :: tempString
    type(ESMF_TermOrder_Flag)       :: termOrder
    type(ESMF_TermOrder_Flag), pointer :: termOrdersRedist(:)
    logical                         :: redistflag
    type(ESMF_RegridMethod_Flag)    :: regridmethod
    type(ESMF_PoleMethod_Flag)      :: polemethod
    integer                         :: regridPoleNPnts
    type(ESMF_UnmappedAction_Flag)  :: unmappedaction
    integer(ESMF_KIND_I4), pointer  :: srcMaskValues(:)
    integer(ESMF_KIND_I4), pointer  :: dstMaskValues(:)
    integer                         :: srcTermProcessing, pipelineDepth
    logical                         :: dumpWeightsFlag
    type(ESMF_Grid)                 :: srcGrid, dstGrid
    type(ESMF_GeomType_Flag)        :: srcGeomtype, dstGeomtype
    type(ESMF_ArraySpec)            :: srcArraySpec, dstArraySpec
    type(ESMF_StaggerLoc)           :: srcStaggerLoc, dstStaggerLoc
    integer, pointer                :: srcGridToFieldMap(:)
    integer, pointer                :: dstGridToFieldMap(:)
    integer, pointer                :: srcUngriddedLBound(:)
    integer, pointer                :: srcUngriddedUBound(:)
    integer, pointer                :: dstUngriddedLBound(:)
    integer, pointer                :: dstUngriddedUBound(:)
    integer                         :: fieldDimCount, gridDimCount
    logical                         :: gridPair
    
    type RHL
      type(ESMF_Grid)                   :: srcGrid, dstGrid
      ! field specific items, TODO: push into a FieldMatch() method
      type(ESMF_ArraySpec)              :: srcArraySpec, dstArraySpec
      type(ESMF_StaggerLoc)             :: srcStaggerLoc, dstStaggerLoc
      integer, pointer                  :: srcGridToFieldMap(:)
      integer, pointer                  :: dstGridToFieldMap(:)
      integer, pointer                  :: srcUngriddedLBound(:)
      integer, pointer                  :: srcUngriddedUBound(:)
      integer, pointer                  :: dstUngriddedLBound(:)
      integer, pointer                  :: dstUngriddedUBound(:)
      ! remap specific items
      logical                           :: redistflag 
      type(ESMF_RegridMethod_Flag)      :: regridmethod
      type(ESMF_RouteHandle)            :: rh
      integer(ESMF_KIND_I4), pointer    :: factorIndexList(:,:)
      real(ESMF_KIND_R8), pointer       :: factorList(:)
      integer(ESMF_KIND_I4), pointer    :: srcMaskValues(:)
      integer(ESMF_KIND_I4), pointer    :: dstMaskValues(:)
      type(ESMF_PoleMethod_Flag)        :: polemethod
      integer                           :: regridPoleNPnts
      type(ESMF_UnmappedAction_Flag)    :: unmappedaction
      type(RHL), pointer                :: prev
    end type
    
    type(RHL), pointer              :: rhList, rhListE
    logical                         :: rhListMatch
    
#if 0
call ESMF_VMLogCurrentGarbageInfo(trim(name)//": FieldBundleCplStore enter: ")
#endif

    ! consistency check counts
    count = size(cplList)
    call ESMF_FieldBundleGet(srcFB, fieldCount=i, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
    if (i /= count) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="Counts must match!", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
      return  ! bail out
    endif
    call ESMF_FieldBundleGet(dstFB, fieldCount=i, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
    if (i /= count) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="Counts must match!", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
      return  ! bail out
    endif

    ! if no fields in bundles, bail out
    if (count < 1) return
    
    ! consistency check the incoming "termOrders" argument
    if (associated(termOrders)) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="The 'termOrders' argument must enter unassociated!", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
      return  ! bail out
    endif
    ! prepare "termOrders" list
    allocate(termOrders(count), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of termOrders.", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    ! prepare "termOrdersRedist" list
    allocate(termOrdersRedist(count), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of termOrdersRedist.", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! access the fields in the add order
    allocate(srcFields(count), dstFields(count), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of srcFields and dstFields.", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call ESMF_FieldBundleGet(srcFB, fieldList=srcFields, &
      itemorderflag=ESMF_ITEMORDER_ADDORDER, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
    call ESMF_FieldBundleGet(dstFB, fieldList=dstFields, &
      itemorderflag=ESMF_ITEMORDER_ADDORDER, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
    
    ! destroy the incoming FieldBundles and create replacement FieldBundles
    call ESMF_FieldBundleDestroy(srcFB, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
    srcFB = ESMF_FieldBundleCreate(rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
    call ESMF_FieldBundleDestroy(dstFB, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
    dstFB = ESMF_FieldBundleCreate(rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out

    ! prepare temporary FieldBundles for Redist operation
    srcFBRedist = ESMF_FieldBundleCreate(rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
    dstFBRedist = ESMF_FieldBundleCreate(rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out

    ! prepare Routehandle
    rh = ESMF_RouteHandleCreate(rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
    call ESMF_RouteHandlePrepXXE(rh, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out

    ! prepare auxiliary variables
    rraShift = 0              ! reset
    vectorLengthShift = 0     ! reset
    iRegrid = 0               ! reset
    iRedist = 0               ! reset

    ! prepare rhList linked list
    nullify(rhList)

    ! loop over all field pairs
    do i=1, count

      ! prepare pointer variables
      nullify(chopStringList)   ! reset
      nullify(chopSubString)    ! reset
      nullify(chopSubSubString) ! reset
      nullify(factorIndexList)  ! reset
      nullify(factorList)       ! reset
      nullify(srcMaskValues)    ! reset
      nullify(dstMaskValues)    ! reset

      ! use a temporary string and convert the cplList(i) to lower characters
      tempString = ESMF_UtilStringLowerCase(cplList(i), rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out

      ! chop the cplList entry
      call NUOPC_ChopString(tempString, chopChar=":", &
        chopStringList=chopStringList, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out

      ! determine "termOrder" which will be used by Run() method
      termOrder = ESMF_TERMORDER_FREE ! default
      do j=2, size(chopStringList)
        if (index(chopStringList(j),"termorder=")==1) then
          call NUOPC_ChopString(chopStringList(j), chopChar="=", &
            chopStringList=chopSubString, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
          if (size(chopSubString)>=2) then
            if (trim(chopSubString(2))=="srcseq") then
              termOrder = ESMF_TERMORDER_SRCSEQ
            else if (trim(chopSubString(2))=="srcpet") then
              termOrder = ESMF_TERMORDER_SRCPET
            else if (trim(chopSubString(2))=="free") then
              termOrder = ESMF_TERMORDER_FREE
            else
              write (msgString,*) "Specified option '", &
                trim(chopStringList(j)), &
                "' is not a vailid choice. Defaulting to FREE for: '", &
                trim(chopStringList(1)), "'"
              call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_WARNING)
            endif
          endif
          deallocate(chopSubString) ! local garbage collection
          exit ! skip the rest of the loop after first hit
        endif
      enddo
      
      ! determine "srcMaskValues"
      allocate(srcMaskValues(0))  ! default
      do j=2, size(chopStringList)
        if (index(chopStringList(j),"srcmaskvalues=")==1) then
          call NUOPC_ChopString(chopStringList(j), chopChar="=", &
            chopStringList=chopSubString, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
          if (size(chopSubString)>=2) then
            call NUOPC_ChopString(chopSubString(2), chopChar=",", &
              chopStringList=chopSubSubString, rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
            if (size(chopSubSubString)>0) then
              deallocate(srcMaskValues)
              allocate(srcMaskValues(size(chopSubSubString)))
              do k=1, size(chopSubSubString)
                read(chopSubSubString(k), "(i10)") srcMaskValues(k)
              enddo
            endif
            deallocate(chopSubSubString)
          endif
          deallocate(chopSubString) ! local garbage collection
          exit ! skip the rest of the loop after first hit
        endif
      enddo
      
      ! determine "dstMaskValues"
      allocate(dstMaskValues(0))  ! default
      do j=2, size(chopStringList)
        if (index(chopStringList(j),"dstmaskvalues=")==1) then
          call NUOPC_ChopString(chopStringList(j), chopChar="=", &
            chopStringList=chopSubString, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
          if (size(chopSubString)>=2) then
            call NUOPC_ChopString(chopSubString(2), chopChar=",", &
              chopStringList=chopSubSubString, rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
            if (size(chopSubSubString)>0) then
              deallocate(dstMaskValues)
              allocate(dstMaskValues(size(chopSubSubString)))
              do k=1, size(chopSubSubString)
                read(chopSubSubString(k), "(i10)") dstMaskValues(k)
              enddo
            endif
            deallocate(chopSubSubString)
          endif
          deallocate(chopSubString) ! local garbage collection
          exit ! skip the rest of the loop after first hit
        endif
      enddo
      
      ! determine "redistflag" and "regridmethod"
      redistflag = .false. ! default to regridding
      regridmethod = ESMF_REGRIDMETHOD_BILINEAR ! default
      do j=2, size(chopStringList)
        if (index(chopStringList(j),"remapmethod=")==1) then
          call NUOPC_ChopString(chopStringList(j), chopChar="=", &
            chopStringList=chopSubString, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
          if (size(chopSubString)>=2) then
            if (trim(chopSubString(2))=="redist") then
              redistflag = .true.
            else if (trim(chopSubString(2))=="bilinear") then
              regridmethod = ESMF_REGRIDMETHOD_BILINEAR
            else if (trim(chopSubString(2))=="patch") then
              regridmethod = ESMF_REGRIDMETHOD_PATCH
            else if (trim(chopSubString(2))=="nearest_stod") then
              regridmethod = ESMF_REGRIDMETHOD_NEAREST_STOD
            else if (trim(chopSubString(2))=="nearest_dtos") then
              regridmethod = ESMF_REGRIDMETHOD_NEAREST_DTOS
            else if (trim(chopSubString(2))=="conserve") then
              regridmethod = ESMF_REGRIDMETHOD_CONSERVE
            else
              write (msgString,*) "Specified option '", &
                trim(chopStringList(j)), &
                "' is not a vailid choice. Defaulting to BILINEAR for: '", &
                trim(chopStringList(1)), "'"
              call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_WARNING)
            endif
          endif
          deallocate(chopSubString) ! local garbage collection
          exit ! skip the rest of the loop after first hit
        endif
      enddo
      
      ! decide whether this is a Redist field pair, or to proceed with Regrid
      if (redistflag) then
        call ESMF_FieldBundleAdd(srcFBRedist, (/srcFields(i)/), &
          multiflag=.true., rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
        call ESMF_FieldBundleAdd(dstFBRedist, (/dstFields(i)/), &
          multiflag=.true., rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
        iRedist = iRedist+1
        termOrdersRedist(iRedist) = termOrder ! record in list to merge below
        cycle ! advance to the next field pair, handle Redist further down
      endif

      ! only Regid field pairs will proceed here...
      iRegrid = iRegrid+1
      termOrders(iRegrid) = termOrder ! record in the list used by Run

      ! add Regrid field pair to the beginning of replacement srcFB and dstFB
      call ESMF_FieldBundleAdd(srcFB, (/srcFields(i)/), multiflag=.true., rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
      call ESMF_FieldBundleAdd(dstFB, (/dstFields(i)/), multiflag=.true., rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out

      ! determine "polemethod" and "regridPoleNPnts"
      polemethod = ESMF_POLEMETHOD_NONE ! default
      regridPoleNPnts = 1 ! default
      do j=2, size(chopStringList)
        if (index(chopStringList(j),"polemethod=")==1) then
          call NUOPC_ChopString(chopStringList(j), chopChar="=", &
            chopStringList=chopSubString, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
          if (size(chopSubString)>=2) then
            if (trim(chopSubString(2))=="none") then
              polemethod = ESMF_POLEMETHOD_NONE
            else if (trim(chopSubString(2))=="allavg") then
              polemethod = ESMF_POLEMETHOD_ALLAVG
            else if (trim(chopSubString(2))=="npntavg") then
              polemethod = ESMF_POLEMETHOD_NPNTAVG
              if (size(chopSubString)>=3) then
                read(chopSubString(3), "(i10)") regridPoleNPnts
              endif
            else if (trim(chopSubString(2))=="teeth") then
              polemethod = ESMF_POLEMETHOD_TEETH
            else
              write (msgString,*) "Specified option '", &
                trim(chopStringList(j)), &
                "' is not a vailid choice. Defaulting to NONE for: '", &
                trim(chopStringList(1)), "'"
              call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_WARNING)
            endif
          endif
          deallocate(chopSubString) ! local garbage collection
          exit ! skip the rest of the loop after first hit
        endif
      enddo
      
      ! determine "unmappedaction"
      unmappedaction = ESMF_UNMAPPEDACTION_IGNORE ! default
      do j=2, size(chopStringList)
        if (index(chopStringList(j),"unmappedaction=")==1) then
          call NUOPC_ChopString(chopStringList(j), chopChar="=", &
            chopStringList=chopSubString, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
          if (size(chopSubString)>=2) then
            if (trim(chopSubString(2))=="error") then
              unmappedaction = ESMF_UNMAPPEDACTION_ERROR
            else if (trim(chopSubString(2))=="ignore") then
              unmappedaction = ESMF_UNMAPPEDACTION_IGNORE
            else
              write (msgString,*) "Specified option '", &
                trim(chopStringList(j)), &
                "' is not a vailid choice. Defaulting to IGNORE for: '", &
                trim(chopStringList(1)), "'"
              call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_WARNING)
            endif
          endif
          deallocate(chopSubString) ! local garbage collection
          exit ! skip the rest of the loop after first hit
        endif
      enddo
      
      ! determine "srcTermProcessing"
      srcTermProcessing = -1  ! default -> force auto-tuning
      do j=2, size(chopStringList)
        if (index(chopStringList(j),"srctermprocessing=")==1) then
          call NUOPC_ChopString(chopStringList(j), chopChar="=", &
            chopStringList=chopSubString, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
          if (size(chopSubString)>=2) then
            read(chopSubString(2), "(i10)") srcTermProcessing
          endif
          deallocate(chopSubString) ! local garbage collection
          exit ! skip the rest of the loop after first hit
        endif
      enddo
      
      ! determine "pipelineDepth"
      pipelineDepth = -1  ! default -> force auto-tuning
      do j=2, size(chopStringList)
        if (index(chopStringList(j),"pipelinedepth=")==1) then
          call NUOPC_ChopString(chopStringList(j), chopChar="=", &
            chopStringList=chopSubString, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
          if (size(chopSubString)>=2) then
            read(chopSubString(2), "(i10)") pipelineDepth
          endif
          deallocate(chopSubString) ! local garbage collection
          exit ! skip the rest of the loop after first hit
        endif
      enddo
      
      ! determine "dumpWeightsFlag"
      dumpWeightsFlag = .false. ! default
      do j=2, size(chopStringList)
        if (index(chopStringList(j),"dumpweights=")==1) then
          call NUOPC_ChopString(chopStringList(j), chopChar="=", &
            chopStringList=chopSubString, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
          if (size(chopSubString)>=2) then
            if (trim(chopSubString(2))=="on") then
              dumpWeightsFlag = .true.
            else if (trim(chopSubString(2))=="off") then
              dumpWeightsFlag = .false.
            else if (trim(chopSubString(2))=="yes") then
              dumpWeightsFlag = .true.
            else if (trim(chopSubString(2))=="no") then
              dumpWeightsFlag = .false.
            else if (trim(chopSubString(2))=="true") then
              dumpWeightsFlag = .true.
            else if (trim(chopSubString(2))=="false") then
              dumpWeightsFlag = .false.
            else
              write (msgString,*) "Specified option '", &
                trim(chopStringList(j)), &
                "' is not a vailid choice. Defaulting to OFF for: '", &
                trim(chopStringList(1)), "'"
              call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_WARNING)
            endif
          endif
          deallocate(chopSubString) ! local garbage collection
          exit ! skip the rest of the loop after first hit
        endif
      enddo

      ! for now optimized reuse of RouteHandle is only implemented for Grids
      
      call ESMF_FieldGet(srcFields(i), geomtype=srcGeomtype, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
      call ESMF_FieldGet(dstFields(i), geomtype=dstGeomtype, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out

      gridPair = (srcGeomtype==ESMF_GEOMTYPE_GRID)
      gridPair = gridPair .and. (dstGeomtype==ESMF_GEOMTYPE_GRID)

      rhListMatch = .false.

      if (gridPair) then
        ! access the src and dst grid objects
        call ESMF_FieldGet(srcFields(i), arrayspec=srcArraySpec, grid=srcGrid, &
          staggerLoc=srcStaggerLoc, dimCount=fieldDimCount, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
        call ESMF_GridGet(srcGrid, dimCount=gridDimCount, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
        allocate(srcGridToFieldMap(gridDimCount))
        allocate(srcUngriddedLBound(fieldDimCount-gridDimCount), &
          srcUngriddedUBound(fieldDimCount-gridDimCount))
        call ESMF_FieldGet(srcFields(i), gridToFieldMap=srcGridToFieldMap, &
          ungriddedLBound=srcUngriddedLBound, &
          ungriddedUBound=srcUngriddedUBound,rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
        
        call ESMF_FieldGet(dstFields(i), arrayspec=dstArraySpec, grid=dstGrid, &
          staggerLoc=dstStaggerLoc, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
        call ESMF_GridGet(dstGrid, dimCount=gridDimCount, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
        allocate(dstGridToFieldMap(gridDimCount))
        allocate(dstUngriddedLBound(fieldDimCount-gridDimCount), &
          dstUngriddedUBound(fieldDimCount-gridDimCount))
        call ESMF_FieldGet(dstFields(i), gridToFieldMap=dstGridToFieldMap, &
          ungriddedLBound=dstUngriddedLBound, &
          ungriddedUBound=dstUngriddedUBound,rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out

        ! search for a match
        rhListE=>rhList
        do while (associated(rhListE))
          ! test src grid match
          rhListMatch = &
            ESMF_GridMatch(rhListE%srcGrid, srcGrid, globalflag=.true., rc=localrc) &
            >= ESMF_GRIDMATCH_EXACT
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
#if 0
write (msgString,*) trim(name)//": srcGrid Match for i=", i, " is: ", &
  rhListMatch
call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
#endif
          if (.not.rhListMatch) goto 123
          ! test dst grid match
          rhListMatch = &
            ESMF_GridMatch(rhListE%dstGrid, dstGrid, globalflag=.true., rc=localrc) &
            >= ESMF_GRIDMATCH_EXACT
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
#if 0
write (msgString,*) trim(name)//": dstGrid Match for i=", i, " is: ", &
  rhListMatch
call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
#endif
          if (.not.rhListMatch) goto 123
          ! test src arrayspec match
          rhListMatch = (rhListE%srcArraySpec==srcArraySpec)
          if (.not.rhListMatch) goto 123
          ! test dst arrayspec match
          rhListMatch = (rhListE%dstArraySpec==dstArraySpec)
          if (.not.rhListMatch) goto 123
          ! test src staggerLoc match
          rhListMatch = (rhListE%srcStaggerLoc==srcStaggerLoc)
          if (.not.rhListMatch) goto 123
          ! test dst staggerLoc match
          rhListMatch = (rhListE%dstStaggerLoc==dstStaggerLoc)
          if (.not.rhListMatch) goto 123
          ! test srcGridToFieldMap
          rhListMatch = &
            (size(rhListE%srcGridToFieldMap)==size(srcGridToFieldMap))
          if (.not.rhListMatch) goto 123
          do j=1, size(srcGridToFieldMap)
            rhListMatch = (rhListE%srcGridToFieldMap(j)==srcGridToFieldMap(j))
            if (.not.rhListMatch) goto 123
          enddo
          ! test dstGridToFieldMap
          rhListMatch = &
            (size(rhListE%dstGridToFieldMap)==size(dstGridToFieldMap))
          if (.not.rhListMatch) goto 123
          do j=1, size(dstGridToFieldMap)
            rhListMatch = (rhListE%dstGridToFieldMap(j)==dstGridToFieldMap(j))
            if (.not.rhListMatch) goto 123
          enddo
          ! test srcUngriddedLBound
          rhListMatch = &
            (size(rhListE%srcUngriddedLBound)==size(srcUngriddedLBound))
          if (.not.rhListMatch) goto 123
          do j=1, size(srcUngriddedLBound)
            rhListMatch = (rhListE%srcUngriddedLBound(j)==srcUngriddedLBound(j))
            if (.not.rhListMatch) goto 123
          enddo
          ! test srcUngriddedUBound
          rhListMatch = &
            (size(rhListE%srcUngriddedUBound)==size(srcUngriddedUBound))
          if (.not.rhListMatch) goto 123
          do j=1, size(srcUngriddedUBound)
            rhListMatch = (rhListE%srcUngriddedUBound(j)==srcUngriddedUBound(j))
            if (.not.rhListMatch) goto 123
          enddo
          ! test dstUngriddedLBound
          rhListMatch = &
            (size(rhListE%dstUngriddedLBound)==size(dstUngriddedLBound))
          if (.not.rhListMatch) goto 123
          do j=1, size(dstUngriddedLBound)
            rhListMatch = (rhListE%dstUngriddedLBound(j)==dstUngriddedLBound(j))
            if (.not.rhListMatch) goto 123
          enddo
          ! test dstUngriddedUBound
          rhListMatch = &
            (size(rhListE%dstUngriddedUBound)==size(dstUngriddedUBound))
          if (.not.rhListMatch) goto 123
          do j=1, size(dstUngriddedUBound)
            rhListMatch = (rhListE%dstUngriddedUBound(j)==dstUngriddedUBound(j))
            if (.not.rhListMatch) goto 123
          enddo
          ! test redistflag
          rhListMatch = (rhListE%redistflag .eqv. redistflag)
          if (.not.rhListMatch) goto 123
          ! test regridmethod
          rhListMatch = (rhListE%regridmethod==regridmethod)
          if (.not.rhListMatch) goto 123
          ! test srcMaskValues
          rhListMatch = &
            (size(rhListE%srcMaskValues)==size(srcMaskValues))
          if (.not.rhListMatch) goto 123
          do j=1, size(srcMaskValues)
            rhListMatch = (rhListE%srcMaskValues(j)==srcMaskValues(j))
            if (.not.rhListMatch) goto 123
          enddo
          ! test dstMaskValues
          rhListMatch = &
            (size(rhListE%dstMaskValues)==size(dstMaskValues))
          if (.not.rhListMatch) goto 123
          do j=1, size(dstMaskValues)
            rhListMatch = (rhListE%dstMaskValues(j)==dstMaskValues(j))
            if (.not.rhListMatch) goto 123
          enddo
          ! test polemethod
          rhListMatch = (rhListE%polemethod==polemethod)
          if (.not.rhListMatch) goto 123
          ! test regridPoleNPnts
          rhListMatch = (rhListE%regridPoleNPnts==regridPoleNPnts)
          if (.not.rhListMatch) goto 123
          ! test unmappedaction
          rhListMatch = (rhListE%unmappedaction==unmappedaction)
          if (.not.rhListMatch) goto 123
          ! completed search 
          exit ! break out
123       continue
          rhListE=>rhListE%prev   ! previous element
        enddo
        
      endif

      if (.not.rhListMatch) then
#if 0
call ESMF_LogWrite(trim(name)//&
  ": no rhListMatch -> pre-compute new remapping: "// &
  trim(cplList(i)), ESMF_LOGMSG_INFO)
#endif
        if (gridPair) then
          ! add a new rhList element
          allocate(rhListE)
          rhListE%prev=>rhList  ! link new element to previous list head
          rhList=>rhListE       ! list head now pointing to new element
        endif
        ! precompute remapping
        if (redistflag) then
          ! redist handled via ESMF_FieldBundleRedistStore() outside pair loop
          ! finding it here indicates that something went wrong
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="Bad internal error - should never get here!",&
            line=__LINE__, file=trim(name)//":"//FILENAME, &
            rcToReturn=rc)
          return  ! bail out
        else      
          ! regrid store call
          !TODO: leverage ESMF_FieldBundleRegridStore(), like for the Redist
          !TODO: case, once ESMF_FieldBundleRegridStore() supports passing
          !TODO: field pair specific arguments e.g. for polemethod,
          !TODO: srcTermProcessing, etc. Until then must do each field
          !TODO: individually here. Notice that most of the RH reuse
          !TODO: optimization is already available on the ESMF side, too.
          call ESMF_FieldRegridStore(srcField=srcFields(i), &
            dstField=dstFields(i), &
            srcMaskValues=srcMaskValues, dstMaskValues=dstMaskValues, &
            regridmethod=regridmethod, &
            polemethod=polemethod, regridPoleNPnts=regridPoleNPnts, &
            unmappedaction=unmappedaction, &
            srcTermProcessing=srcTermProcessing, pipelineDepth=pipelineDepth, &
            routehandle=rhh, &
            factorIndexList=factorIndexList, factorList=factorList, &
            rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
        endif
        if (gridPair) then
          ! store info in the new rhList element
          rhListE%srcGrid=srcGrid
          rhListE%dstGrid=dstGrid
          rhListE%srcArraySpec=srcArraySpec
          rhListE%dstArraySpec=dstArraySpec
          rhListE%srcStaggerLoc=srcStaggerLoc
          rhListE%dstStaggerLoc=dstStaggerLoc
          rhListE%srcGridToFieldMap=>srcGridToFieldMap
          rhListE%dstGridToFieldMap=>dstGridToFieldMap
          rhListE%srcUngriddedLBound=>srcUngriddedLBound
          rhListE%srcUngriddedUBound=>srcUngriddedUBound
          rhListE%dstUngriddedLBound=>dstUngriddedLBound
          rhListE%dstUngriddedUBound=>dstUngriddedUBound
          rhListE%redistflag=redistflag
          rhListE%regridmethod=regridmethod
          rhListE%rh=rhh
          rhListE%factorIndexList=>factorIndexList
          rhListE%factorList=>factorList
          rhListE%srcMaskValues=>srcMaskValues
          rhListE%dstMaskValues=>dstMaskValues
          rhListE%polemethod=polemethod
          rhListE%regridPoleNPnts=regridPoleNPnts
          rhListE%unmappedaction=unmappedaction
        endif
      else
#if 0
call ESMF_LogWrite(trim(name)//&
  ": found rhListMatch -> reuse routehandle: "// &
  trim(cplList(i)), ESMF_LOGMSG_INFO)
#endif
        ! pull out the routehandle from the matching rhList element
        rhh = rhListE%rh
        factorIndexList => rhListE%factorIndexList
        factorList => rhListE%factorList
        ! deallocate temporary grid/field info
        deallocate(srcGridToFieldMap, dstGridToFieldMap)
        deallocate(srcUngriddedLBound, srcUngriddedUBound)
        deallocate(dstUngriddedLBound, dstUngriddedUBound)
        deallocate(srcMaskValues,      dstMaskValues)
      endif
      
      ! append rhh to rh and clear rhh
      call ESMF_RouteHandleAppend(rh, appendRoutehandle=rhh, &
        rraShift=rraShift, vectorLengthShift=vectorLengthShift, &
        transferflag=.not.rhListMatch, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
      
      ! adjust rraShift and vectorLengthShift
      call ESMF_FieldGet(srcFields(i), localDeCount=localDeCount, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
      rraShift = rraShift + localDeCount
      call ESMF_FieldGet(dstFields(i), localDeCount=localDeCount, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
      rraShift = rraShift + localDeCount
      vectorLengthShift = vectorLengthShift + 1
      
      ! weight dumping
      if (dumpWeightsFlag .and. .not.redistflag) then
        call NUOPC_Write(factorList=factorList, &
          factorIndexList=factorIndexList, &
          fileName="weightmatrix_"//trim(name)//"_"//trim(chopStringList(1))//".nc",&
          rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
      endif
      
      ! local garbage collection
      if (.not.gridPair) then
        ! grid pairs transfer ownership of lists into rhList struct
        if (associated(factorIndexList)) deallocate(factorIndexList)
        if (associated(factorList)) deallocate(factorList)
      endif
      if (associated(chopStringList)) deallocate(chopStringList)

    enddo ! loop over all field pairs

    ! take down rhList and destroy rh objects
    do while (associated(rhList))
      rhListE=>rhList
      rhList=>rhList%prev
      call ESMF_RouteHandleDestroy(rhListE%rh, noGarbage=.true., rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
      if (associated(rhListE%factorIndexList)) deallocate(rhListE%factorIndexList)
      if (associated(rhListE%factorList)) deallocate(rhListE%factorList)
      deallocate(rhListE%srcGridToFieldMap, rhListE%dstGridToFieldMap)
      deallocate(rhListE%srcUngriddedLBound, rhListE%srcUngriddedUBound)
      deallocate(rhListE%dstUngriddedLBound, rhListE%dstUngriddedUBound)
      deallocate(rhListE%srcMaskValues, rhListE%dstMaskValues)
      deallocate(rhListE)
    enddo

    ! garbage collection
    deallocate(srcFields, dstFields)

    ! now deal with the Redist field pairs
    call ESMF_FieldBundleGet(srcFBRedist, fieldCount=count, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
    if (count /= iRedist) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="Counts must match!", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
      return  ! bail out
    endif
    call ESMF_FieldBundleGet(dstFBRedist, fieldCount=i, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
    if (i /= count) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="Counts must match!", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
      return  ! bail out
    endif

    if (count > 0) then
#if 0
write(msgString,*) "Redist FBs have elements: ", count
call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
#endif
      ! call into ESMF for an optimized Redist pre-compute between FieldBundles
      call ESMF_FieldBundleRedistStore(srcFBRedist, dstFBRedist, &
        routehandle=rhh, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out

      ! append rhh to rh and clear rhh
      call ESMF_RouteHandleAppend(rh, appendRoutehandle=rhh, &
        rraShift=rraShift, vectorLengthShift=vectorLengthShift, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out

      ! obtain fields from srcFBRedist and dstFBRedist
      allocate(srcFields(count), dstFields(count), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of srcFields and dstFields.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call ESMF_FieldBundleGet(srcFBRedist, fieldList=srcFields, &
        itemorderflag=ESMF_ITEMORDER_ADDORDER, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
      call ESMF_FieldBundleGet(dstFBRedist, fieldList=dstFields, &
      itemorderflag=ESMF_ITEMORDER_ADDORDER, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out

      ! add fields to srcFB and dstFB
      call ESMF_FieldBundleAdd(srcFB, srcFields, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
      call ESMF_FieldBundleAdd(dstFB, dstFields, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out

      ! append termOrdersRedist at the end of termOrders list
      do i=1, count
        termOrders(iRegrid+i) = termOrdersRedist(i)
      enddo

      ! local garbage collection
      deallocate(srcFields, dstFields)
    endif

    ! garbage collection
    deallocate(termOrdersRedist)
    call ESMF_FieldBundleDestroy(srcFBRedist, noGarbage=.true., rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
    call ESMF_FieldBundleDestroy(dstFBRedist, noGarbage=.true., rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

#if 0
call ESMF_VMLogCurrentGarbageInfo(trim(name)//": FieldBundleCplStore leaving: ")
#endif

  end subroutine

  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_ConnectorGet - Get parameters from a Connector
!
! !INTERFACE:
  subroutine NUOPC_ConnectorGet(connector, srcFields, dstFields, rh, state, &
    CplSet, cplSetList, srcVM, dstVM, driverClock, rc)
! !ARGUMENTS:
    type(ESMF_CplComp)                            :: connector
    type(ESMF_FieldBundle), intent(out), optional :: srcFields
    type(ESMF_FieldBundle), intent(out), optional :: dstFields
    type(ESMF_RouteHandle), intent(out), optional :: rh
    type(ESMF_State),       intent(out), optional :: state
    character(*),           intent(in),  optional :: CplSet
    character(ESMF_MAXSTR), pointer,     optional :: cplSetList(:)
    type(ESMF_VM),          intent(out), optional :: srcVM
    type(ESMF_VM),          intent(out), optional :: dstVM
    type(ESMF_Clock),       intent(out), optional :: driverClock
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Get parameters from the {\tt connector} internal state.
!
!   The Connector keeps information about the connection that it implements 
!   in its internal state. When customizing a Connector, it is often necessary
!   to access and sometimes modify these data objects.
!
!   The arguments are:
!   \begin{description}
!   \item[connector]
!     The Connector component.
!   \item[{[srcFields]}]
!     The FieldBundle under which the Connector keeps track of all connected
!     source side fields. The order in which the fields are stored
!     in {\tt srcFields} is significant, as it corresponds to the order of
!     fields in {\tt dstFields}. Consequently, when accessing and modifying
!     the fields inside of {\tt srcFields}, it is imporant to use the
!     {\tt itemorderflag=ESMF\_ITEMORDER\_ADDORDER} option to
!     {\tt ESMF\_FieldBundleGet()}.
!   \item[{[dstFields]}]
!     The FieldBundle under which the Connector keeps track of all connected
!     destination side fields. The order in which the fields are stored
!     in {\tt dstFields} is significant, as it corresponds to the order of
!     fields in {\tt srcFields}. Consequently, when accessing and modifying
!     the fields inside of {\tt dstFields}, it is imporant to use the
!     {\tt itemorderflag=ESMF\_ITEMORDER\_ADDORDER} option to
!     {\tt ESMF\_FieldBundleGet()}.
!   \item[{[rh]}]
!     The RouteHandle that the Connector uses to move data from {\tt srcFields}
!     to {\tt dstFields}.
!   \item[{[state]}]
!     A State object that the Connector keeps to make customization of the 
!     Connector more convenient. The generic Connector code handles creation
!     and destruction of {\tt state}, but does {\em not} access it directly 
!     for information.
!   \item[{[CplSet]}]
!     If present, all of the returned information is specific to the specified
!     coupling set.
!   \item[{[cplSetList]}]
!     The list of coupling sets currently known to the Connector. This argument
!     must enter the call {\em unassociated} or an error is returned. This means
!     that the user code must explicitly call {\tt nullify()} or use the
!     {\tt => null()} syntax on the variable passed in as {\tt cplSetList}
!     argument. On return, the {\tt cplSetList} argument will be associated, 
!     potentially of size zero. The responsibility for deallocation transfers
!     to the caller.
!   \item[{[srcVM]}]
!     The VM of the source side component.
!   \item[{[dstVM]}]
!     The VM of the destination side component.
!   \item[{[driverClock]}]
!     The Clock object used by the current RunSequence level to drive this
!     component.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is
    integer                         :: sIndex
    integer                         :: stat
    integer                         :: localrc

    if (present(rc)) rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(connector, name=name, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
    
    ! early exit if nothing to be done -> this allows calling the method even
    ! if the internal state does not (yet) exist - done for testing
    if (.not.present(srcFields) .and. &
        .not.present(dstFields) .and. &
        .not.present(rh) .and. &
        .not.present(state) .and. &
        .not.present(cplSetList) .and. &
        .not.present(srcVM) .and. &
        .not.present(dstVM) .and. &
        .not.present(driverClock)) return
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(connector, label_InternalState, is, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! driverClock
    if (present(driverClock)) driverClock = is%wrap%driverClock

    ! Get the requested members
    if (present(CplSet)) then
      sIndex=getIndex(value=CplSet, list=is%wrap%cplSetList)
      if (sIndex < 1 .OR. sIndex > is%wrap%cplSetCount) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="CplSet does not exist!", &
          line=__LINE__, &
          file=FILENAME, &
          rcToReturn=rc)
        return  ! bail out
      endif
      if (present(srcFields)) srcFields = is%wrap%cplSet(sIndex)%srcFields
      if (present(dstFields)) dstFields = is%wrap%cplSet(sIndex)%dstFields
      if (present(rh))        rh = is%wrap%cplSet(sIndex)%rh
    else
      if (present(srcFields)) srcFields = is%wrap%srcFields
      if (present(dstFields)) dstFields = is%wrap%dstFields
      if (present(rh))        rh = is%wrap%rh
    endif
    if (present(state))     state = is%wrap%state

    if (present(cplSetList)) then
      if (associated(cplSetList)) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="cplSetList must enter unassociated", &
          line=__LINE__, &
          file=FILENAME, &
          rcToReturn=rc)
        return  ! bail out
      else
        allocate(cplSetList(is%wrap%cplSetCount), stat=stat)
        if (ESMF_LogFoundAllocError(stat, msg="allocating cplSetList", &
          line=__LINE__, &
          file=FILENAME, &
          rcToReturn=rc)) &
          return  ! bail out
        cplSetList=is%wrap%cplSetList ! copy the elements
      endif
    endif
    
    if (present(srcVM)) then
      srcVM = is%wrap%srcVM
    endif
    if (present(dstVM)) then
      dstVM = is%wrap%dstVM
    endif
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_ConnectorSet - Set parameters in a Connector
!
! !INTERFACE:
  subroutine NUOPC_ConnectorSet(connector, srcFields, dstFields, rh, state, &
    CplSet, srcVM, dstVM, rc)
! !ARGUMENTS:
    type(ESMF_CplComp)                            :: connector
    type(ESMF_FieldBundle), intent(in),  optional :: srcFields
    type(ESMF_FieldBundle), intent(in),  optional :: dstFields
    type(ESMF_RouteHandle), intent(in),  optional :: rh
    type(ESMF_State),       intent(in),  optional :: state
    character(*),           intent(in),  optional :: CplSet
    type(ESMF_VM),          intent(in),  optional :: srcVM
    type(ESMF_VM),          intent(in),  optional :: dstVM
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Set parameters in the {\tt connector} internal state.
!
!   The Connector keeps information about the connection that it implements 
!   in its internal state. When customizing a Connector, it is often necessary
!   to access and sometimes modify these data objects.
!
!   The arguments are:
!   \begin{description}
!   \item[connector]
!     The Connector component.
!   \item[{[srcFields]}]
!     The FieldBundle under which the Connector keeps track of all connected
!     source side fields. The order in which the fields are stored
!     in {\tt srcFields} is significant, as it corresponds to the order of
!     fields in {\tt dstFields}. Consequently, when setting {\tt srcFields}, it
!     is important to add them in the same order as for {\tt dstFields}.
!   \item[{[dstFields]}]
!     The FieldBundle under which the Connector keeps track of all connected
!     destination side fields. The order in which the fields are stored
!     in {\tt dstFields} is significant, as it corresponds to the order of
!     fields in {\tt srcFields}. Consequently, when setting {\tt dstFields}, it
!     is important to add them in the same order as for {\tt srcFields}.
!   \item[{[rh]}]
!     The RouteHandle that the Connector uses to move data from {\tt srcFields}
!     to {\tt dstFields}.
!   \item[{[state]}]
!     A State object that the Connector keeps to make customization of the 
!     Connector more convenient. Only in very rare cases would the user want
!     to replace the {\tt state} that is managed by the generic Connector
!     implementation. If {\tt state} is set by this call, the user essentially
!     claims ownership of the previous {\tt state} object, and becomes 
!     responsible for its destruction. Ownership of the new {\tt state} is 
!     transferred to the Connector and must not be explicitly destroyed by the
!     user code.
!   \item[{[CplSet]}]
!     If present, all of the passed in information is set under the specified
!     coupling set.
!   \item[{[srcVM]}]
!     The VM of the source side component.
!   \item[{[dstVM]}]
!     The VM of the destination side component.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is
    logical                         :: isPetLocal
    integer                         :: sIndex
    integer                         :: localrc

    if (present(rc)) rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(connector, name=name, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! on PETs that are not part of the connector, this is a noop
    isPetLocal = ESMF_CplCompIsPetLocal(connector, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (.not.isPetLocal) return ! early successful return
    
    ! early exit if nothing to be done -> this allows calling the method even
    ! if the internal state does not (yet) exist - done for testing
    if (.not.present(srcFields) .and. &
        .not.present(dstFields) .and. &
        .not.present(rh) .and. &
        .not.present(state) .and. &
        .not.present(srcVM) .and. &
        .not.present(dstVM)) return ! early successful return

    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(connector, label_InternalState, is, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! Set the requested members
    if (present(CplSet)) then
      sIndex=getIndex(value=CplSet, list=is%wrap%cplSetList)
      if (sIndex < 1 .OR. sIndex > is%wrap%cplSetCount) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="CplSet does not exist!", &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
        return  ! bail out
      endif 
      if (present(srcFields)) is%wrap%cplSet(sIndex)%srcFields = srcFields
      if (present(dstFields)) is%wrap%cplSet(sIndex)%dstFields = dstFields
      if (present(rh))        is%wrap%cplSet(sIndex)%rh = rh
    else
      if (present(srcFields)) is%wrap%srcFields = srcFields
      if (present(dstFields)) is%wrap%dstFields = dstFields
      if (present(rh))        is%wrap%rh = rh
    endif
    if (present(state))     is%wrap%state = state
    
    if (present(srcVM)) then
      is%wrap%srcVM = srcVM
    endif
    if (present(dstVM)) then
      is%wrap%dstVM = dstVM
    endif
    
  end subroutine
  !-----------------------------------------------------------------------------

end module
