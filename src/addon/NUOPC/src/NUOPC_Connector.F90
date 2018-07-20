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
  
  public SetServices
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
    integer                             :: cplSetCount
    character(ESMF_MAXSTR), pointer     :: cplSetList(:)
    type(type_CplSet), allocatable      :: cplSet(:)
  end type

  type type_InternalState
    type(type_InternalStateStruct), pointer :: wrap
  end type

  ! Generic methods
  public NUOPC_ConnectorGet, NUOPC_ConnectorSet

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine SetServices(connector, rc)
    type(ESMF_CplComp)   :: connector
    integer, intent(out) :: rc
    
    ! local variables
    character(ESMF_MAXSTR)    :: name

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

  subroutine InitializeP0(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    character(*), parameter   :: rName="InitializeP0"
    character(ESMF_MAXSTR)    :: name
    integer                   :: verbosity, diagnostic
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(cplcomp, name=name, verbosity=verbosity, &
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
    call NUOPC_CompFilterPhaseMap(cplcomp, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv05p"/), rc=rc)
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

  subroutine InitializeIPDv05p1(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    character(*), parameter   :: rName="InitializeIPDv05p1"
    character(ESMF_MAXSTR)    :: name
    character(ESMF_MAXSTR)    :: importXferPolicy, exportXferPolicy
    integer                   :: verbosity, profiling, diagnostic
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(cplcomp, name=name, verbosity=verbosity, &
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
      call doTransfer(importState, exportState, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif
    if (trim(importXferPolicy)=="transferAll") then
      call doTransfer(exportState, importState, rc=rc)
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

    subroutine doTransfer(srcState, dstState, rc)

      type(ESMF_State)     :: srcState
      type(ESMF_State)     :: dstState
      integer, intent(out) :: rc

      character(ESMF_MAXSTR) :: srcTransferGeom, dstTransferGeom
      integer                :: i, j
      character(ESMF_MAXSTR), pointer       :: srcStandardNameList(:)
      character(ESMF_MAXSTR), pointer       :: dstStandardNameList(:)
      character(ESMF_MAXSTR), pointer       :: srcNamespaceList(:)
      character(ESMF_MAXSTR), pointer       :: dstNamespaceList(:)
      type(ESMF_Field),       pointer       :: srcFieldList(:)
      type(ESMF_Field),       pointer       :: dstFieldList(:)
      character(ESMF_MAXSTR), pointer       :: srcCplSetList(:)
      character(ESMF_MAXSTR), pointer       :: dstCplSetList(:)
      
      rc = ESMF_SUCCESS

      nullify(srcStandardNameList)
      nullify(srcNamespaceList)
      nullify(srcFieldList)
      nullify(srcCplSetList)
      nullify(dstStandardNameList)
      nullify(dstNamespaceList)
      nullify(dstFieldList)
      nullify(dstCplSetList)
    
      call NUOPC_GetStateMemberLists(srcState, srcStandardNameList, &
        fieldList=srcFieldList, namespaceList=srcNamespaceList, &
        cplSetList=srcCplSetList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        
      call NUOPC_GetStateMemberLists(dstState, dstStandardNameList, &
        fieldList=dstFieldList, namespaceList=dstNamespaceList, &
        cplSetList=dstCplSetList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      ! WARNING: does not currently deal with nested states or field bundles
      
      if (associated(srcStandardNameList)) then
        do i=1, size(srcStandardNameList)

          ! do not transfer if it already exists in the destination state
          if (associated(dstStandardNameList)) then
            do j=1, size(dstStandardNameList)
              if (trim(srcStandardNameList(i))==trim(dstStandardNameList(j))) &
                exit
            enddo
            if (j<size(dstStandardNameList)+1) cycle
          endif

          ! reverse TransferOfferGeomObject attribute, e.g., if a component
          ! providing a field wants to provide a grid, then the accepting
          ! component should not try to provide its own grid
          call NUOPC_GetAttribute(srcFieldList(i), &
            name="TransferOfferGeomObject", value=srcTransferGeom, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

          ! default
          dstTransferGeom = "cannot provide"
          if (trim(srcTransferGeom)=="will provide") then
            dstTransferGeom = "cannot provide"
          else if (trim(srcTransferGeom)=="can provide") then
            dstTransferGeom = "cannot provide"
          else if (trim(srcTransferGeom)=="cannot provide") then
            dstTransferGeom = "will provide"
          end if

          ! transfer to toState
          call NUOPC_Advertise(dstState, &
            StandardName=trim(srcStandardNameList(i)), &
            TransferOfferGeomObject=dstTransferGeom, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

        end do
      endif

      if (associated(srcStandardNameList)) deallocate(srcStandardNameList)
      if (associated(srcNamespaceList)) deallocate(srcNamespaceList)
      if (associated(srcFieldList)) deallocate(srcFieldList)
      if (associated(srcCplSetList)) deallocate(srcCplSetList)
      if (associated(dstStandardNameList)) deallocate(dstStandardNameList)
      if (associated(dstNamespaceList)) deallocate(dstNamespaceList)
      if (associated(dstFieldList)) deallocate(dstFieldList)
      if (associated(dstCplSetList)) deallocate(dstCplSetList)
    end subroutine

  end subroutine

  !-----------------------------------------------------------------------------
  
  subroutine InitializeIPDv05p2a(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
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
    logical                               :: match
    integer                   :: verbosity, profiling, diagnostic
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(cplcomp, name=name, verbosity=verbosity, &
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

#if 0
call ESMF_VMLogCurrentGarbageInfo(trim(name)//": InitializeIPDv05p2a after reconcile: ")
#endif

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

#if 0
call printStringList("importStandardNameList", importStandardNameList)
call printStringList("importNamespaceList", importNamespaceList)
call printStringList("importCplSetList", importCplSetList)
#endif
      
    call NUOPC_GetStateMemberLists(exportState, exportStandardNameList, &
      fieldList=exportFieldList, namespaceList=exportNamespaceList, &
      cplSetList=exportCplSetList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

#if 0
call printStringList("exportStandardNameList", exportStandardNameList)
call printStringList("exportNamespaceList", exportNamespaceList)
call printStringList("exportCplSetList", exportCplSetList)
#endif
      
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

#if 0
print *, "current bondLevel=", bondLevel
#endif

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
#if 0
print *, "connectionString: ", connectionString
#endif
              ! see if a new bondLevel highmark was found
              read (connectionString, "(i10)") bondLevelMax
#if 0
print *, "bondLevelMax:", bondLevelMax, "bondLevel:", bondLevel
#endif
              if (bondLevel > bondLevelMax) then
                write (connectionString, "(i10)") bondLevel
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

  subroutine InitializeIPDv05p2b(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
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
    call NUOPC_CompGet(cplcomp, name=name, verbosity=verbosity, &
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
    call NUOPC_CompAttributeSet(cplcomp, &
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
      
#if 0
call printStringList("importStandardNameList", importStandardNameList)
call printStringList("importNamespaceList", importNamespaceList)
call printStringList("importCplSetList", importCplSetList)
#endif
      
    call NUOPC_GetStateMemberLists(exportState, exportStandardNameList, &
      fieldList=exportFieldList, namespaceList=exportNamespaceList, &
      cplSetList=exportCplSetList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
#if 0
call printStringList("exportStandardNameList", exportStandardNameList)
call printStringList("exportNamespaceList", exportNamespaceList)
call printStringList("exportCplSetList", exportCplSetList)
#endif
      
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
              
#if 0
print *, "current bondLevel=", bondLevel
#endif

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
          call NUOPC_CompAttributeSet(cplcomp, &
            name="CplList", valueList=cplList(1:count), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          call NUOPC_CompAttributeSet(cplcomp, &
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

  subroutine InitializeIPDv03p1(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
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
    call NUOPC_CompGet(cplcomp, name=name, verbosity=verbosity, &
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
    call ESMF_CplCompSet(cplcomp, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
#endif

    ! Simply the combination of IPDv05p2a + IPDv05p2b
    call InitializeIPDv05p2a(cplcomp, importState, exportState, clock, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call InitializeIPDv05p2b(cplcomp, importState, exportState, clock, rc)
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

  subroutine InitializeIPDv05p3(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    character(*), parameter         :: rName="InitializeIPDv05p3"
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
    integer                         :: iMatch, eMatch
    type(ESMF_Field)                :: iField, eField
    integer                         :: stat
    type(type_InternalState)        :: is
    logical                         :: foundFlag
    character(ESMF_MAXSTR)          :: connectionString
    character(ESMF_MAXSTR)          :: name, iString
    character(len=160)              :: msgString
    character(ESMF_MAXSTR)          :: iTransferOffer, eTransferOffer
    character(ESMF_MAXSTR)          :: iSharePolicy, eSharePolicy
    logical                         :: matchE, matchI, acceptFlag
    integer                   :: verbosity, profiling, diagnostic
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(cplcomp, name=name, verbosity=verbosity, &
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
    
    ! allocate memory for the internal state and set it in the Component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal state memory failed.", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call ESMF_UserCompSetInternalState(cplcomp, label_InternalState, is, rc)
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
    call NUOPC_CompAttributeGet(cplcomp, name="CplList", &
      itemCount=cplListSize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    ! get the cplSetList Attribute
    call NUOPC_CompAttributeGet(cplcomp, name="CplSetList", &
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
      call NUOPC_CompAttributeGet(cplcomp, name="CplList", valueList=cplList, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      allocate(cplSetList(cplSetListSize), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of internal cplSetList() failed.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call NUOPC_CompAttributeGet(cplcomp, name="CplSetList", &
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
      call chopString(cplList(i), chopChar=":", chopStringList=chopStringList, &
        rc=rc)
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
        
        ! coordinate the transfer and sharing of fields between components
        call NUOPC_GetAttribute(iField, name="TransferOfferField", &
          value=iTransferOffer, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        call NUOPC_GetAttribute(eField, name="TransferOfferField", &
          value=eTransferOffer, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        acceptFlag=.false. ! reset
        if (trim(iTransferOffer)=="will provide") then
          if (trim(eTransferOffer)=="will provide") then
            ! -> both sides must provide
            call NUOPC_SetAttribute(iField, &
              name="TransferActionField", value="provide", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_SetAttribute(eField, &
              name="TransferActionField", value="provide", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,12)) then
              write (msgString, '(A)') trim(name)//": "//&
                "- both sides must provide the Field."
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
          elseif (trim(eTransferOffer)=="can provide") then
            ! -> import side must provide, export side must accept
            acceptFlag=.true.
            call NUOPC_SetAttribute(iField, &
              name="TransferActionField", value="provide", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_SetAttribute(eField, &
              name="TransferActionField", value="accept", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,12)) then
              write (msgString, '(A)') trim(name)//": "//&
                "- import side must provide, export side must accept the Field."
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
          else  ! eTransferOffer=="cannot provide"
            ! -> import side must provide, export side must accept
            acceptFlag=.true.
            call NUOPC_SetAttribute(iField, &
              name="TransferActionField", value="provide", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_SetAttribute(eField, &
              name="TransferActionField", value="accept", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,12)) then
              write (msgString, '(A)') trim(name)//": "//&
                "- import side must provide, export side must accept the Field."
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
          endif
        elseif (trim(iTransferOffer)=="can provide") then
          if (trim(eTransferOffer)=="will provide") then
            ! -> import side must accept, export side must provide
            acceptFlag=.true.
            call NUOPC_SetAttribute(iField, &
              name="TransferActionField", value="accept", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_SetAttribute(eField, &
              name="TransferActionField", value="provide", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,12)) then
              write (msgString, '(A)') trim(name)//": "//&
                "- import side must accept, export side must provide the Field."
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
          elseif (trim(eTransferOffer)=="can provide") then
            ! -> import side must provide, export side must accept
            acceptFlag=.true.
            call NUOPC_SetAttribute(iField, &
              name="TransferActionField", value="provide", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_SetAttribute(eField, &
              name="TransferActionField", value="accept", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,12)) then
              write (msgString, '(A)') trim(name)//": "//&
                "- import side must accept, export side must provide the Field."
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
          else  ! eTransferOffer=="cannot provide"
            ! -> import side must provide, export side must accept
            acceptFlag=.true.
            call NUOPC_SetAttribute(iField, &
              name="TransferActionField", value="provide", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_SetAttribute(eField, &
              name="TransferActionField", value="accept", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,12)) then
              write (msgString, '(A)') trim(name)//": "//&
                "- import side must accept, export side must provide the Field."
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
          endif
        else  ! iTransferOffer=="cannot provide"
          if (trim(eTransferOffer)=="will provide") then
            ! -> import side must accept, export side must provide
            acceptFlag=.true.
            call NUOPC_SetAttribute(iField, &
              name="TransferActionField", value="accept", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_SetAttribute(eField, &
              name="TransferActionField", value="provide", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,12)) then
              write (msgString, '(A)') trim(name)//": "//&
                "- import side must accept, export side must provide the Field."
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
          elseif (trim(eTransferOffer)=="can provide") then
            ! -> import side must accept, export side must provide
            acceptFlag=.true.
            call NUOPC_SetAttribute(iField, &
              name="TransferActionField", value="accept", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_SetAttribute(eField, &
              name="TransferActionField", value="provide", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,12)) then
              write (msgString, '(A)') trim(name)//": "//&
                "- import side must accept, export side must provide the Field."
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
        if (acceptFlag) then
          ! One side accepts the other -> need to look at sharing
          call NUOPC_GetAttribute(iField, name="SharePolicyField", &
            value=iSharePolicy, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          call NUOPC_GetAttribute(eField, name="SharePolicyField", &
            value=eSharePolicy, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (trim(iSharePolicy)=="share" .and. trim(eSharePolicy)=="share") then
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
          endif
        endif

        ! coordinate the transfer and sharing of geomobjects between components
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
            if (btest(verbosity,12)) then
              write (msgString, '(A)') trim(name)//": "//&
                "- both sides must provide the GeomObject."
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
          elseif (trim(eTransferOffer)=="can provide") then
            ! -> import side must provide, export side must accept
            acceptFlag=.true.
            call NUOPC_SetAttribute(iField, &
              name="TransferActionGeomObject", value="provide", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_SetAttribute(eField, &
              name="TransferActionGeomObject", value="accept", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,12)) then
              write (msgString, '(A)') trim(name)//": "//&
                "- import side must provide, export side must accept the GeomObject."
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
          else  ! eTransferOffer=="cannot provide"
            ! -> import side must provide, export side must accept
            acceptFlag=.true.
            call NUOPC_SetAttribute(iField, &
              name="TransferActionGeomObject", value="provide", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_SetAttribute(eField, &
              name="TransferActionGeomObject", value="accept", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,12)) then
              write (msgString, '(A)') trim(name)//": "//&
                "- import side must provide, export side must accept the GeomObject."
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
          endif
        elseif (trim(iTransferOffer)=="can provide") then
          if (trim(eTransferOffer)=="will provide") then
            ! -> import side must accept, export side must provide
            acceptFlag=.true.
            call NUOPC_SetAttribute(iField, &
              name="TransferActionGeomObject", value="accept", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_SetAttribute(eField, &
              name="TransferActionGeomObject", value="provide", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,12)) then
              write (msgString, '(A)') trim(name)//": "//&
                "- import side must accept, export side must provide the GeomObject."
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
          elseif (trim(eTransferOffer)=="can provide") then
            ! -> import side must provide, export side must accept
            acceptFlag=.true.
            call NUOPC_SetAttribute(iField, &
              name="TransferActionGeomObject", value="provide", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_SetAttribute(eField, &
              name="TransferActionGeomObject", value="accept", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,12)) then
              write (msgString, '(A)') trim(name)//": "//&
                "- import side must provide, export side must accept the GeomObject."
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
          else  ! eTransferOffer=="cannot provide"
            ! -> import side must provide, export side must accept
            acceptFlag=.true.
            call NUOPC_SetAttribute(iField, &
              name="TransferActionGeomObject", value="provide", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_SetAttribute(eField, &
              name="TransferActionGeomObject", value="accept", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,12)) then
              write (msgString, '(A)') trim(name)//": "//&
                "- import side must provide, export side must accept the GeomObject."
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
          endif
        else  ! iTransferOffer=="cannot provide"
          if (trim(eTransferOffer)=="will provide") then
            ! -> import side must accept, export side must provide
            acceptFlag=.true.
            call NUOPC_SetAttribute(iField, &
              name="TransferActionGeomObject", value="accept", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_SetAttribute(eField, &
              name="TransferActionGeomObject", value="provide", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,12)) then
              write (msgString, '(A)') trim(name)//": "//&
                "- import side must accept, export side must provide the GeomObject."
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
            endif
          elseif (trim(eTransferOffer)=="can provide") then
            ! -> import side must accept, export side must provide
            acceptFlag=.true.
            call NUOPC_SetAttribute(iField, &
              name="TransferActionGeomObject", value="accept", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_SetAttribute(eField, &
              name="TransferActionGeomObject", value="provide", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,12)) then
              write (msgString, '(A)') trim(name)//": "//&
                "- import side must accept, export side must provide the GeomObject."
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
        if (acceptFlag) then
          ! One side accepts the other -> need to look at sharing
          call NUOPC_GetAttribute(iField, name="SharePolicyGeomObject", &
            value=iSharePolicy, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          call NUOPC_GetAttribute(eField, name="SharePolicyGeomObject", &
            value=eSharePolicy, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (trim(iSharePolicy)=="share" .and. trim(eSharePolicy)=="share") then
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
            ! at least one side does not want to share -> not shared
            ! but don't modify attribute here because if alread shared through
            ! another connection, it must stay shared. Rely on "not shared" 
            ! default.
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

  subroutine InitializeIPDv05p4(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
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
    integer                         :: iMatch, eMatch
    type(ESMF_Field)                :: iField, eField
    type(ESMF_Field)                :: providerField, acceptorField
    type(ESMF_State)                :: providerState, acceptorState
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
    character(ESMF_MAXSTR)          :: iShareStatus, eShareStatus
    integer(ESMF_KIND_I4), pointer  :: ungriddedLBound(:), ungriddedUBound(:)
    integer(ESMF_KIND_I4), pointer  :: gridToFieldMap(:)
    integer                         :: fieldDimCount, gridDimCount, arbDimCount
    logical                         :: matchE, matchI
    integer                         :: dimCount
    integer, allocatable            :: minIndex(:), maxIndex(:)
    logical                         :: sharedFlag
    type(ESMF_Array)                :: array
    integer                         :: verbosity, profiling, diagnostic
    type(ESMF_Time)                 :: currTime
    character(len=40)               :: currTimeString
    character(len=40)               :: transferDirection
    
    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(cplcomp, name=name, verbosity=verbosity, &
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
    call ESMF_UserCompGetInternalState(cplcomp, label_InternalState, is, rc)
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
    call NUOPC_CompAttributeGet(cplcomp, name="CplList", &
      itemCount=cplListSize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    ! get the cplSetList Attribute
    call NUOPC_CompAttributeGet(cplcomp, name="CplSetList", &
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
      call NUOPC_CompAttributeGet(cplcomp, name="CplList", valueList=cplList, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      allocate(cplSetList(cplSetListSize), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of internal cplSetList() failed.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call NUOPC_CompAttributeGet(cplcomp, name="CplSetList", &
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

    ! prepare chopStringList
    nullify(chopStringList)
    
    ! main loop over all entries in the cplList
    do i=1, cplListSize
      call chopString(cplList(i), chopChar=":", chopStringList=chopStringList, &
        rc=rc)
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
          providerState = importState
          acceptorState = exportState
          transferDirection = "(import -> export)"
        elseif ((trim(eTransferAction)=="provide") &
          .and.(trim(iTransferAction)=="accept")) then
          providerField = eField
          acceptorField = iField
          providerState = exportState
          acceptorState = importState
          transferDirection = "(import <- export)"
        else  ! not a situation that needs handling here
          cycle ! continue with the next i
        endif
        
        call NUOPC_GetAttribute(iField, name="ShareStatusField", &
          value=iShareStatus, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        call NUOPC_GetAttribute(eField, name="ShareStatusField", &
          value=eShareStatus, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

        if (btest(verbosity,12)) then
          write (msgString, '(A)') trim(name)//": "//&
            "- import Field ShareStatus="//trim(iShareStatus)
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          write (msgString, '(A)') trim(name)//": "//&
            "- export Field ShareStatus="//trim(eShareStatus)
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
        endif

        sharedFlag = .false. ! reset
        if (trim(iShareStatus)=="shared" .and. trim(eShareStatus)=="shared") &
          sharedFlag = .true.
          
        ! transfer the underlying DistGrid from provider to acceptor
        ! or share the providerField or providerGeomObject with the acceptor
        call ESMF_FieldGet(providerField, geomtype=geomtype, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        if (geomtype==ESMF_GEOMTYPE_GRID) then
          call ESMF_FieldGet(providerField, grid=grid, staggerloc=staggerloc, &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (.not.sharedFlag) then
            ! not shared -> must transfer
            call ESMF_GridGet(grid, distgrid=providerDG, name=geomobjname, &
              dimCount=dimCount, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call ESMF_FieldGet(acceptorField, vm=vm, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,11)) then
              call ESMF_LogWrite(trim(name)//": transferring underlying "// &
                "DistGrid "//trim(transferDirection)//" for Grid: "&
                //trim(geomobjname), ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            endif
            acceptorDG = ESMF_DistGridCreate(providerDG, vm=vm, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            grid = ESMF_GridEmptyCreate(vm=vm, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,11)) then
              call ESMF_LogWrite(trim(name)//&
                ": done transferring underlying DistGrid", &
                ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            endif
            call ESMF_GridSet(grid, name=geomobjname, distgrid=acceptorDG, &
              vm=vm, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call ESMF_FieldEmptySet(acceptorField, grid=grid, &
              staggerloc=staggerloc, vm=vm, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          endif
          ! query additional provider information
          call ESMF_FieldGet(providerField, grid=grid, &
            dimCount=fieldDimCount, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          call ESMF_GridGet(grid, dimCount=gridDimCount, &
            arbDimCount=arbDimCount, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          allocate(minIndex(gridDimCount), maxIndex(gridDimCount), stat=rc)
          if (ESMF_LogFoundAllocError(rc, msg="Allocating minIndex, maxIndex", &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          call ESMF_GridGetIndex(grid, tileNo=1, &
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
          if (.not.sharedFlag) then
            ! transfer additional provider info in form of attributes
            call ESMF_AttributeSet(acceptorField, &
              name="MinIndex", valueList=minIndex, &
              convention="NUOPC", purpose="Instance", &
              attnestflag=ESMF_ATTNEST_ON, rc=rc)
            call ESMF_AttributeSet(acceptorField, &
              name="MaxIndex", valueList=maxIndex, &
              convention="NUOPC", purpose="Instance", &
              attnestflag=ESMF_ATTNEST_ON, rc=rc)
            ! bring over arbDimCount as attribute
            call ESMF_AttributeSet(acceptorField, &
              name="ArbDimCount", value=arbDimCount, &
              convention="NUOPC", purpose="Instance", &
              attnestflag=ESMF_ATTNEST_ON, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=FILENAME)) &
              return  ! bail out
            ! bring over gridToFieldMap as attributes
            call ESMF_AttributeSet(acceptorField, &
              name="GridToFieldMap", valueList=gridToFieldMap, &
              convention="NUOPC", purpose="Instance", &
              attnestflag=ESMF_ATTNEST_ON, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=FILENAME)) &
              return  ! bail out
            if (fieldDimCount - gridDimCount > 0) then
              ! bring over ungridded dim bounds as attributes
!            print *, "fieldDimCount = ", fieldDimCount
!            print *, "gridDimCount = ", gridDimCount
              call ESMF_AttributeSet(acceptorField, &
                name="UngriddedLBound", valueList=ungriddedLBound, &
                convention="NUOPC", purpose="Instance", &
                attnestflag=ESMF_ATTNEST_ON, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
              call ESMF_AttributeSet(acceptorField, &
                name="UngriddedUBound", valueList=ungriddedUBound, &
                convention="NUOPC", purpose="Instance", &
                attnestflag=ESMF_ATTNEST_ON, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
            endif
          else
            ! shared:
            ! obtain the array from provider to be shared with acceptor
            call ESMF_FieldGet(providerField, array=array, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            ! accceptor fieldName to remain unchanged
            call ESMF_FieldGet(acceptorField, name=fieldName, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            !TODO: make sure that this FieldCreate() sets total widths correctly
            if (fieldDimCount - gridDimCount > 0) then
              acceptorField=ESMF_FieldCreate(grid=grid, array=array, &
                datacopyflag=ESMF_DATACOPY_REFERENCE, staggerloc=staggerloc, &
                gridToFieldMap=gridToFieldMap, name=fieldName, &
                ungriddedLBound=ungriddedLBound, &
                ungriddedUBound=ungriddedUBound, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            else
              acceptorField=ESMF_FieldCreate(grid=grid, array=array, &
                datacopyflag=ESMF_DATACOPY_REFERENCE, staggerloc=staggerloc, &
                gridToFieldMap=gridToFieldMap, name=fieldName, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            end if
            call NUOPC_Realize(acceptorState, acceptorField, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            ! reset the TransferAction for this completed field
            call NUOPC_SetAttribute(acceptorField, &
              name="TransferActionField", value="complete", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_SetAttribute(acceptorField, &
              name="TransferActionGeomObject", value="complete", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
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
          
        elseif (geomtype==ESMF_GEOMTYPE_MESH) then
          call ESMF_FieldGet(providerField, mesh=mesh, meshloc=meshloc, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (.not.sharedFlag) then
            ! not shared -> must transfer
            call ESMF_MeshGet(mesh, elementDistgrid=providerDG, &
              nodalDistgrid=providerDG_nodal, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call ESMF_FieldGet(acceptorField, vm=vm, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            if (btest(verbosity,11)) then
              call ESMF_LogWrite(trim(name)//&
                ": transferring underlying DistGrid "//trim(transferDirection)&
                //" for Mesh", ESMF_LOGMSG_INFO, rc=rc)
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
                ": done transferring underlying DistGrid", &
                ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            endif
          endif
          ! query additional provider information
          call ESMF_FieldGet(providerField, mesh=mesh, &
            dimCount=fieldDimCount, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          call ESMF_DistGridGet(providerDG, dimCount=gridDimCount, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          allocate(gridToFieldMap(gridDimCount),stat=stat)
          if (ESMF_LogFoundAllocError(statusToCheck=stat, &
            msg="Allocation of internal ungriddedLBound failed.", &
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
          if (.not.sharedFlag) then
            ! transfer additional provider info in form of attributes
            ! bring over gridToFieldMap as attributes
            call ESMF_AttributeSet(acceptorField, &
              name="GridToFieldMap", valueList=gridToFieldMap, &
              convention="NUOPC", purpose="Instance", &
              attnestflag=ESMF_ATTNEST_ON, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=FILENAME)) &
              return  ! bail out
            if (fieldDimCount - gridDimCount > 0) then
              ! bring over ungridded dim bounds as attributes
              call ESMF_AttributeSet(acceptorField, &
                name="UngriddedLBound", valueList=ungriddedLBound, &
                convention="NUOPC", purpose="Instance", &
                attnestflag=ESMF_ATTNEST_ON, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
              call ESMF_AttributeSet(acceptorField, &
                name="UngriddedUBound", valueList=ungriddedUBound, &
                convention="NUOPC", purpose="Instance", &
                attnestflag=ESMF_ATTNEST_ON, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
            endif
          else
            ! shared:
            ! obtain the array from provider to be shared with acceptor
            call ESMF_FieldGet(providerField, array=array, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            ! accceptor fieldName to remain unchanged
            call ESMF_FieldGet(acceptorField, name=fieldName, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            !TODO: make sure that this FieldCreate() sets total widths correctly
            if (fieldDimCount - gridDimCount > 0) then
              acceptorField=ESMF_FieldCreate(mesh=mesh, array=array, &
                datacopyflag=ESMF_DATACOPY_REFERENCE, meshloc=meshloc, &
                gridToFieldMap=gridToFieldMap, name=fieldName, &
                ungriddedLBound=ungriddedLBound, &
                ungriddedUBound=ungriddedUBound, &
                rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            else
              acceptorField=ESMF_FieldCreate(mesh=mesh, array=array, &
                datacopyflag=ESMF_DATACOPY_REFERENCE, meshloc=meshloc, &
                gridToFieldMap=gridToFieldMap, name=fieldName, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            end if
            call NUOPC_Realize(acceptorState, acceptorField, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            ! reset the TransferAction for this completed field
            call NUOPC_SetAttribute(acceptorField, &
              name="TransferActionField", value="complete", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_SetAttribute(acceptorField, &
              name="TransferActionGeomObject", value="complete", rc=rc)
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

        elseif (geomtype==ESMF_GEOMTYPE_LOCSTREAM) then
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
              ": transferring underlying DistGrid "//trim(transferDirection)&
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
              ": done transferring underlying DistGrid", &
              ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          endif
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

  subroutine InitializeIPDv05p5(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
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
    integer                         :: iMatch, eMatch
    type(ESMF_Field)                :: iField, eField
    type(ESMF_Field)                :: providerField, acceptorField
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
    logical                         :: matchE, matchI
    integer                         :: verbosity, profiling, diagnostic
    type(ESMF_Time)                 :: currTime
    character(len=40)               :: currTimeString
    character(len=40)               :: transferDirection

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(cplcomp, name=name, verbosity=verbosity, &
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
    call ESMF_UserCompGetInternalState(cplcomp, label_InternalState, is, rc)
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
    call NUOPC_CompAttributeGet(cplcomp, name="CplList", &
      itemCount=cplListSize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    ! get the cplSetList Attribute
    call NUOPC_CompAttributeGet(cplcomp, name="CplSetList", &
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
      call NUOPC_CompAttributeGet(cplcomp, name="CplList", valueList=cplList, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      allocate(cplSetList(cplSetListSize), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of internal cplSetList() failed.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call NUOPC_CompAttributeGet(cplcomp, name="CplSetList", &
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
    
    ! prepare chopStringList
    nullify(chopStringList)

    ! main loop over all entries in the cplList
    do i=1, cplListSize
      call chopString(cplList(i), chopChar=":", chopStringList=chopStringList, &
        rc=rc)
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
          transferDirection = "(import -> export)"
        elseif ((trim(eTransferAction)=="provide") &
          .and.(trim(iTransferAction)=="accept")) then
          providerField = eField
          acceptorField = iField
          transferDirection = "(import <- export)"
        else  ! not a situation that needs handling here
          cycle ! continue with the next i
        endif

        ! transfer the underlying Grid/Mesh/LocStream from provider to acceptor
        call ESMF_FieldGet(providerField, geomtype=geomtype, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        if (geomtype==ESMF_GEOMTYPE_GRID) then
          if (btest(verbosity,11)) then
            call ESMF_LogWrite(trim(name)//&
              ": transferring the full Grid with coordinates "//&
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
              ": done transferring the full Grid with coordinates", &
              ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          endif
        elseif (geomtype==ESMF_GEOMTYPE_MESH) then
          if (btest(verbosity,11)) then
            call ESMF_LogWrite(trim(name)//&
              ": transferring the full Mesh with coordinates "//&
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
          call ESMF_MeshGet(acceptorMesh, nodalDistgrid=nDistgrid, &
            elementDistgrid=eDistgrid, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (meshNoConnections) then
            ! provider Mesh does not have connections
            ! -> need both DistGrids on the acceptor side
            !TODO: When Mesh implements a name, make sure to transfer it here!
            acceptorMesh = ESMF_MeshCreate(providerMesh, &
              nodalDistgrid=nDistgrid, elementDistgrid=eDistgrid, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          else
            ! provider Mesh does have connections
            ! -> only need one DistGrid on the acceptor side -> use eDistgrid
            !TODO: When Mesh implements a name, make sure to transfer it here!
            acceptorMesh = ESMF_MeshCreate(providerMesh, &
              elementDistgrid=eDistgrid, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          endif
          call ESMF_FieldEmptySet(acceptorField, mesh=acceptorMesh, &
            meshloc=meshloc, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (btest(verbosity,11)) then
            call ESMF_LogWrite(trim(name)//&
              ": done transferring the full Mesh with coordinates", &
              ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          endif
        elseif (geomtype==ESMF_GEOMTYPE_LOCSTREAM) then
          if (btest(verbosity,11)) then
            call ESMF_LogWrite(trim(name)//&
              ": transferring the full LocStream with coordinates "//&
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
              ": done transferring the full LocStream with coordinates", &
              ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          endif
        else
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="Provided GeomType must be Grid, Mesh, or LocStream.", &
            line=__LINE__, file=trim(name)//":"//FILENAME, &
            rcToReturn=rc)
          return  ! bail out
        endif
          
        ! Need to reset the TransferOfferGeomObject and TransferActionGeomObject
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

  subroutine InitializeIPDv05p6a(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
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
    call NUOPC_CompGet(cplcomp, name=name, verbosity=verbosity, &
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

  subroutine InitializeIPDv05p6b(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
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
    call NUOPC_CompGet(cplcomp, name=name, verbosity=verbosity, &
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
    call ESMF_UserCompGetInternalState(cplcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! get the cplList Attribute
    call NUOPC_CompAttributeGet(cplcomp, name="CplList", &
      itemCount=cplListSize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    ! get the cplSetList Attribute
    call NUOPC_CompAttributeGet(cplcomp, name="CplSetList", &
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
      call NUOPC_CompAttributeGet(cplcomp, name="CplList", valueList=cplList, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      allocate(cplSetList(cplSetListSize), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of internal cplSetList() failed.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call NUOPC_CompAttributeGet(cplcomp, name="CplSetList", &
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
      call chopString(cplList(i), chopChar=":", chopStringList=chopStringList, &
        rc=rc)
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
#if 0
call ESMF_LogWrite("iShareStatus: "//trim(iShareStatus), ESMF_LOGMSG_INFO, rc=rc)
call ESMF_LogWrite("eShareStatus: "//trim(eShareStatus), ESMF_LOGMSG_INFO, rc=rc)
#endif
        sharedFlag = .false. ! reset
        if (trim(iShareStatus)=="shared" .and. trim(eShareStatus)=="shared") &
          sharedFlag = .true.
        if (.not.sharedFlag) then
          ! not sharing -> add the import and export Fields to FieldBundles
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
    call ESMF_MethodExecute(cplcomp, label=label_ComputeRouteHandle, &
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

  subroutine InitializeIPDv00p2a(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
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
    call NUOPC_CompGet(cplcomp, name=name, verbosity=verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! intro
    call NUOPC_LogIntro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! Simply the combination of IPDv05p3 + IPDv05p6a
    call InitializeIPDv05p3(cplcomp, importState, exportState, clock, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call InitializeIPDv05p6a(cplcomp, importState, exportState, clock, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! extro
    call NUOPC_LogExtro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeIPDv00p2b(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
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
    call NUOPC_CompGet(cplcomp, name=name, verbosity=verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! intro
    call NUOPC_LogIntro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! Simply same as IPDv05p6b
    call InitializeIPDv05p6b(cplcomp, importState, exportState, clock, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! extro
    call NUOPC_LogExtro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine Run(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    character(*), parameter   :: rName="Run"
    type(type_InternalState)  :: is
    type(ESMF_VM)             :: vm
    integer                   :: localrc
    logical                   :: existflag
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
    call NUOPC_CompGet(cplcomp, name=name, verbosity=verbosity, &
      profiling=profiling, diagnostic=diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        
    ! intro
    call NUOPC_LogIntro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! get the compName and currentPhase
    call ESMF_CplCompGet(cplcomp, name=compName, currentPhase=phase, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle verbosity
    if (btest(verbosity,13)) then
      call NUOPC_CompSearchRevPhaseMap(cplcomp, ESMF_METHOD_RUN, &
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
    call ESMF_UserCompGetInternalState(cplcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      
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
    call ESMF_MethodExecute(cplcomp, label=label_ExecuteRouteHandle, &
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
          call ESMF_FieldBundleRegrid(is%wrap%cplSet(i)%srcFields, &
            is%wrap%cplSet(i)%dstFields, &
            routehandle=is%wrap%cplSet(i)%rh, &
            termorderflag=is%wrap%cplSet(i)%termOrders, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        enddo
      else
        if (ESMF_RouteHandleIsCreated(is%wrap%rh)) then !--- TEST
          call ESMF_FieldBundleRegrid(is%wrap%srcFields, is%wrap%dstFields, &
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

    ! get the rootPet attribute out of the importState
    call ESMF_AttributeGet(importState, name="rootVas", value=rootVas, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! handle profiling
    if (btest(profiling,0)) then
      call ESMF_VMWtime(time)
      write (msgString, *) trim(name)//": Profile 06 time=   ", &
        time-time0, time-timeBase
        time0=time
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
    endif

    call ESMF_CplCompGet(cplcomp, vm=vm, petCount=petCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! handle profiling
    if (btest(profiling,0)) then
      call ESMF_VMWtime(time)
      write (msgString, *) trim(name)//": Profile 07 time=   ", &
        time-time0, time-timeBase
        time0=time
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
    endif
    
    do rootPet=0, petCount-1
      call ESMF_VMGet(vm, rootPet, vas=vas, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      if (vas == rootVas) exit
    enddo
    
    ! handle profiling
    if (btest(profiling,0)) then
      call ESMF_VMWtime(time)
      write (msgString, *) trim(name)//": Profile 08 time=   ", &
        time-time0, time-timeBase
        time0=time
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
    endif

    !TODO: bail out if rootPet not found

    ! hand coded, specific AttributeUpdate
    call NUOPC_UpdateTimestamp(is%wrap%srcFieldList, rootPet=rootPet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle profiling
    if (btest(profiling,0)) then
      call ESMF_VMWtime(time)
      write (msgString, *) trim(name)//": Profile 09 time=   ", &
        time-time0, time-timeBase
        time0=time
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
    endif

    ! update the timestamp on all of the dst fields to that on the src side
    call NUOPC_UpdateTimestamp(is%wrap%srcFieldList, is%wrap%dstFieldList, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! handle profiling
    if (btest(profiling,0)) then
      call ESMF_VMWtime(time)
      write (msgString, *) trim(name)//": Profile 10 time=   ", &
        time-time0, time-timeBase
        time0=time
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
    endif

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

  subroutine Finalize(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    character(*), parameter   :: rName="Finalize"
    integer                   :: stat
    type(type_InternalState)  :: is
    integer                   :: localrc
    logical                   :: existflag
    character(ESMF_MAXSTR)    :: name
    integer                   :: verbosity, diagnostic
    integer                   :: i
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(cplcomp, name=name, verbosity=verbosity, &
      diagnostic=diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! intro
    call NUOPC_LogIntro(name, rName, verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(cplcomp, label_InternalState, is, rc)
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
    call ESMF_MethodExecute(cplcomp, label=label_ReleaseRouteHandle, &
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
          call ESMF_FieldBundleRegridRelease(is%wrap%cplSet(i)%rh, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        enddo
      else
        if (ESMF_RouteHandleIsCreated(is%wrap%rh)) then !--- TEST
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
    call ESMF_MethodExecute(cplcomp, label=label_Finalize, &
      existflag=existflag, userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

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
        file=FILENAME)) &
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
      file=FILENAME)) &
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
      file=FILENAME)) &
      return  ! bail out

    if (l_count > 0) then 
      uniqueList(1:l_count) = l_uniqueList(1:l_count)
    endif

    deallocate(l_uniqueList)
    if (ESMF_LogFoundDeallocError(stat, msg="deallocating l_uniqueList", &
      line=__LINE__, &
      file=FILENAME)) &
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

  subroutine chopString(string, chopChar, chopStringList, rc)
    character(len=*)                              :: string
    character                                     :: chopChar
    character(ESMF_MAXSTR), pointer               :: chopStringList(:)
    integer,                intent(out), optional :: rc
    ! local variables
    integer               :: i, j, count
    integer, allocatable  :: chopPos(:)
    
    ! check the incoming pointer
    if (associated(chopStringList)) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="chopStringList must enter unassociated", &
        line=__LINE__, &
        file=FILENAME, &
        rcToReturn=rc)
      return  ! bail out
    endif
    
    ! determine how many times chopChar is found in string
    count=0 ! reset
    do i=1, len(trim(string))
      if (string(i:i)==chopChar) count=count+1
    enddo
    
    ! record positions where chopChar is found in string
    allocate(chopPos(count))
    j=1 ! reset
    do i=1, len(trim(string))
      if (string(i:i)==chopChar) then
        chopPos(j)=i
        j=j+1
      endif
    enddo
    
    ! chop up the string
    allocate(chopStringList(count+1))
    j=1 ! reset
    do i=1, count
      chopStringList(i) = string(j:chopPos(i)-1)
      j=chopPos(i)+1
    enddo
    chopStringList(count+1) = trim(string(j:len(string)))
    deallocate(chopPos)
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
    
  !-----------------------------------------------------------------------------

  subroutine FieldBundleCplStore(srcFB, dstFB, cplList, rh, termOrders, name, &
    rc)
    type(ESMF_FieldBundle),    intent(in)            :: srcFB
    type(ESMF_FieldBundle),    intent(inout)         :: dstFB
    character(*)                                     :: cplList(:)
    type(ESMF_RouteHandle),    intent(inout)         :: rh
    type(ESMF_TermOrder_Flag), pointer               :: termOrders(:)
    character(*),              intent(in)            :: name
    integer,                   intent(out), optional :: rc
    
    ! local variables
    integer                         :: i, j, k, count, stat, localDeCount
    type(ESMF_Field), pointer       :: srcFields(:), dstFields(:)
    integer                         :: rraShift, vectorLengthShift
    type(ESMF_RouteHandle)          :: rhh
    integer(ESMF_KIND_I4), pointer  :: factorIndexList(:,:)
    real(ESMF_KIND_R8), pointer     :: factorList(:)
    character(ESMF_MAXSTR), pointer :: chopStringList(:)
    character(ESMF_MAXSTR), pointer :: chopSubString(:), chopSubSubString(:)
    character(len=160)              :: msgString
    character(len=480)              :: tempString
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
    call ESMF_FieldBundleGet(srcFB, fieldCount=i, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (i /= count) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="Counts must match!", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
      return  ! bail out
    endif
    call ESMF_FieldBundleGet(dstFB, fieldCount=i, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
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
    
    ! access the fields in the add order
    allocate(srcFields(count), dstFields(count), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of srcFields and dstFields.", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call ESMF_FieldBundleGet(srcFB, fieldList=srcFields, &
      itemorderflag=ESMF_ITEMORDER_ADDORDER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call ESMF_FieldBundleGet(dstFB, fieldList=dstFields, &
      itemorderflag=ESMF_ITEMORDER_ADDORDER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! prepare Routehandle
    rh = ESMF_RouteHandleCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call ESMF_RouteHandlePrepXXE(rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! prepare auxiliary variables
    rraShift = 0              ! reset
    vectorLengthShift = 0     ! reset
    
    ! prepare rhList linked list
    nullify(rhList)
    
    ! loop over all fields
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
      tempString = ESMF_UtilStringLowerCase(cplList(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      
      ! chop the cplList entry
      call chopString(tempString, chopChar=":", chopStringList=chopStringList, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      
      ! determine "srcMaskValues"
      allocate(srcMaskValues(0))  ! default
      do j=2, size(chopStringList)
        if (index(chopStringList(j),"srcmaskvalues=")==1) then
          call chopString(chopStringList(j), chopChar="=", &
            chopStringList=chopSubString, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (size(chopSubString)>=2) then
            call chopString(chopSubString(2), chopChar=",", &
              chopStringList=chopSubSubString, rc=rc)
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
          call chopString(chopStringList(j), chopChar="=", &
            chopStringList=chopSubString, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (size(chopSubString)>=2) then
            call chopString(chopSubString(2), chopChar=",", &
              chopStringList=chopSubSubString, rc=rc)
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
          call chopString(chopStringList(j), chopChar="=", &
            chopStringList=chopSubString, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
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
      
      ! determine "polemethod" and "regridPoleNPnts"
      polemethod = ESMF_POLEMETHOD_NONE ! default
      regridPoleNPnts = 1 ! default
      do j=2, size(chopStringList)
        if (index(chopStringList(j),"polemethod=")==1) then
          call chopString(chopStringList(j), chopChar="=", &
            chopStringList=chopSubString, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
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
          call chopString(chopStringList(j), chopChar="=", &
            chopStringList=chopSubString, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
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
          call chopString(chopStringList(j), chopChar="=", &
            chopStringList=chopSubString, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
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
          call chopString(chopStringList(j), chopChar="=", &
            chopStringList=chopSubString, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
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
          call chopString(chopStringList(j), chopChar="=", &
            chopStringList=chopSubString, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
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
      
      call ESMF_FieldGet(srcFields(i), geomtype=srcGeomtype, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      call ESMF_FieldGet(dstFields(i), geomtype=dstGeomtype, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

      gridPair = (srcGeomtype==ESMF_GEOMTYPE_GRID)
      gridPair = gridPair .and. (dstGeomtype==ESMF_GEOMTYPE_GRID)

      rhListMatch = .false.

      if (gridPair) then
        ! access the src and dst grid objects
        call ESMF_FieldGet(srcFields(i), arrayspec=srcArraySpec, grid=srcGrid, &
          staggerLoc=srcStaggerLoc, dimCount=fieldDimCount, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        call ESMF_GridGet(srcGrid, dimCount=gridDimCount, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        allocate(srcGridToFieldMap(gridDimCount))
        allocate(srcUngriddedLBound(fieldDimCount-gridDimCount), &
          srcUngriddedUBound(fieldDimCount-gridDimCount))
        call ESMF_FieldGet(srcFields(i), gridToFieldMap=srcGridToFieldMap, &
          ungriddedLBound=srcUngriddedLBound, &
          ungriddedUBound=srcUngriddedUBound,rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        
        call ESMF_FieldGet(dstFields(i), arrayspec=dstArraySpec, grid=dstGrid, &
          staggerLoc=dstStaggerLoc, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        call ESMF_GridGet(dstGrid, dimCount=gridDimCount, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        allocate(dstGridToFieldMap(gridDimCount))
        allocate(dstUngriddedLBound(fieldDimCount-gridDimCount), &
          dstUngriddedUBound(fieldDimCount-gridDimCount))
        call ESMF_FieldGet(dstFields(i), gridToFieldMap=dstGridToFieldMap, &
          ungriddedLBound=dstUngriddedLBound, &
          ungriddedUBound=dstUngriddedUBound,rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

        ! search for a match
        rhListE=>rhList
        do while (associated(rhListE))
          ! test src grid match
          rhListMatch = &
            ESMF_GridMatch(rhListE%srcGrid, srcGrid, globalflag=.true.) &
            >= ESMF_GRIDMATCH_EXACT
#if 0
write (msgString,*) trim(name)//": srcGrid Match for i=", i, " is: ", &
  rhListMatch
call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
#endif
          if (.not.rhListMatch) goto 123
          ! test dst grid match
          rhListMatch = &
            ESMF_GridMatch(rhListE%dstGrid, dstGrid, globalflag=.true.) &
            >= ESMF_GRIDMATCH_EXACT
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
          ! redist store call
          call ESMF_FieldRedistStore(srcField=srcFields(i), &
            dstField=dstFields(i), &
!not yet implemented:          pipelineDepth=pipelineDepth, &
            routehandle=rhh, &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        else      
          ! regrid store call
          call ESMF_FieldRegridStore(srcField=srcFields(i), &
            dstField=dstFields(i), &
            srcMaskValues=srcMaskValues, dstMaskValues=dstMaskValues, &
            regridmethod=regridmethod, &
            polemethod=polemethod, regridPoleNPnts=regridPoleNPnts, &
            unmappedaction=unmappedaction, &
            srcTermProcessing=srcTermProcessing, pipelineDepth=pipelineDepth, &
            routehandle=rhh, &
            factorIndexList=factorIndexList, factorList=factorList, &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
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
        transferflag=.not.rhListMatch, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      
      ! adjust rraShift and vectorLengthShift
      call ESMF_FieldGet(srcFields(i), localDeCount=localDeCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      rraShift = rraShift + localDeCount
      call ESMF_FieldGet(dstFields(i), localDeCount=localDeCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      rraShift = rraShift + localDeCount
      vectorLengthShift = vectorLengthShift + 1
      
      ! weight dumping
      if (dumpWeightsFlag .and. .not.redistflag) then
        call NUOPC_Write(factorList=factorList, &
          factorIndexList=factorIndexList, &
          fileName="weightmatrix_"//trim(name)//"_"//trim(chopStringList(1))//".nc",&
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      endif
      
      ! determine "termOrders" list which will be used by Run() method
      termOrders(i) = ESMF_TERMORDER_FREE ! default
      do j=2, size(chopStringList)
        if (index(chopStringList(j),"termorder=")==1) then
          call chopString(chopStringList(j), chopChar="=", &
            chopStringList=chopSubString, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (size(chopSubString)>=2) then
            if (trim(chopSubString(2))=="srcseq") then
              termOrders(i) = ESMF_TERMORDER_SRCSEQ
            else if (trim(chopSubString(2))=="srcpet") then
              termOrders(i) = ESMF_TERMORDER_SRCPET
            else if (trim(chopSubString(2))=="free") then
              termOrders(i) = ESMF_TERMORDER_FREE
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
      
      ! local garbage collection
      if (.not.gridPair) then
        ! grid pairs transfer ownership of lists into rhList struct
        if (associated(factorIndexList)) deallocate(factorIndexList)
        if (associated(factorList)) deallocate(factorList)
      endif
      if (associated(chopStringList)) deallocate(chopStringList)

    enddo
    
    ! take down rhList and destroy rh objects
    do while (associated(rhList))
      rhListE=>rhList
      rhList=>rhList%prev
      call ESMF_RouteHandleDestroy(rhListE%rh, noGarbage=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
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
    CplSet, cplSetList, rc)
! !ARGUMENTS:
    type(ESMF_CplComp)                            :: connector
    type(ESMF_FieldBundle), intent(out), optional :: srcFields
    type(ESMF_FieldBundle), intent(out), optional :: dstFields
    type(ESMF_RouteHandle), intent(out), optional :: rh
    type(ESMF_State),       intent(out), optional :: state
    character(*),           intent(in),  optional :: CplSet
    character(ESMF_MAXSTR), pointer,     optional :: cplSetList(:)
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

    if (present(rc)) rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(connector, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! early exit if nothing to be done -> this allows calling the method even
    ! if the internal state does not (yet) exist - done for testing
    if (.not.present(cplSetList) .and. &
        .not.present(srcFields) .and. &
        .not.present(dstFields) .and. &
        .not.present(rh) .and. &
        .not.present(state)) return
    
    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(connector, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
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
          file=FILENAME)) &
          return  ! bail out
        cplSetList=is%wrap%cplSetList
      endif
    endif
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_ConnectorSet - Set parameters in a Connector
!
! !INTERFACE:
  subroutine NUOPC_ConnectorSet(connector, srcFields, dstFields, rh, state, &
    CplSet, rc)
! !ARGUMENTS:
    type(ESMF_CplComp)                            :: connector
    type(ESMF_FieldBundle), intent(in),  optional :: srcFields
    type(ESMF_FieldBundle), intent(in),  optional :: dstFields
    type(ESMF_RouteHandle), intent(in),  optional :: rh
    type(ESMF_State),       intent(in),  optional :: state
    character(*),           intent(in),  optional :: CplSet
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

    if (present(rc)) rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(connector, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! early exit if nothing to be done -> this allows calling the method even
    ! if the internal state does not (yet) exist - done for testing
    if (.not.present(srcFields) .and. &
        .not.present(dstFields) .and. &
        .not.present(rh) .and. &
        .not.present(state)) return

    ! query Component for the internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(connector, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! Set the requested members
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
      if (present(srcFields)) is%wrap%cplSet(sIndex)%srcFields = srcFields
      if (present(dstFields)) is%wrap%cplSet(sIndex)%dstFields = dstFields
      if (present(rh))        is%wrap%cplSet(sIndex)%rh = rh
    else
      if (present(srcFields)) is%wrap%srcFields = srcFields
      if (present(dstFields)) is%wrap%dstFields = dstFields
      if (present(rh))        is%wrap%rh = rh
    endif
    if (present(state))     is%wrap%state = state
    
  end subroutine
  !-----------------------------------------------------------------------------

end module
