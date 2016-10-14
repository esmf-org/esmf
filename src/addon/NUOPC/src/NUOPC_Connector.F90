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

    ! query the Component for info
    call ESMF_CplCompGet(connector, name=name, rc=rc)
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
    
    ! For backward compatibility with v6 API the sequence of the following
    ! NUOPC_CompSetEntryPoint() calls is critical to produce the old default
    ! InitializePhaseMap.

    call NUOPC_CompSetEntryPoint(connector, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv05p1"/), &
      userRoutine=Initialize05P1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call NUOPC_CompSetEntryPoint(connector, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p1", "IPDv01p1", "IPDv02p1", "IPDv03p1"/), &
      userRoutine=InitializeP1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call NUOPC_CompSetEntryPoint(connector, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv01p2", "IPDv02p2", "IPDv03p2", "IPDv04p2", "IPDv05p3"/), &
      userRoutine=InitializeP2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call NUOPC_CompSetEntryPoint(connector, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p3", "IPDv04p3", "IPDv05p4"/), &
      userRoutine=InitializeP3, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call NUOPC_CompSetEntryPoint(connector, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p4", "IPDv04p4", "IPDv05p5"/), &
      userRoutine=InitializeP4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call NUOPC_CompSetEntryPoint(connector, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv01p3a", "IPDv02p3a", "IPDv03p5a", "IPDv04p5a", "IPDv05p6a"/), &
      userRoutine=InitializeP5a, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call NUOPC_CompSetEntryPoint(connector, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv01p3b", "IPDv02p3b", "IPDv03p5b", "IPDv04p5b", "IPDv05p6b"/), &
      userRoutine=InitializeP5b, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call NUOPC_CompSetEntryPoint(connector, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv04p1a", "IPDv05p2a"/), &
      userRoutine=InitializeP1a, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call NUOPC_CompSetEntryPoint(connector, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv04p1b", "IPDv05p2b"/), &
      userRoutine=InitializeP1b, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    call NUOPC_CompSetEntryPoint(connector, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p2a"/), &
      userRoutine=Initialize00P2a, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call NUOPC_CompSetEntryPoint(connector, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p2b"/), &
      userRoutine=Initialize00P2b, rc=rc)
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
    character(ESMF_MAXSTR)                    :: name

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_CplCompGet(cplcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! filter all other entries but those of type IPDv05
    call NUOPC_CompFilterPhaseMap(cplcomp, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv05p"/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

  end subroutine
  
   !-----------------------------------------------------------------------------

  subroutine Initialize05P1(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    character(ESMF_MAXSTR) :: name
    character(ESMF_MAXSTR) :: importXferPolicy, exportXferPolicy

    rc = ESMF_SUCCESS

#if 0
call ESMF_VMLogCurrentGarbageInfo("Connector Initialize05P1 in: ")
#endif

    ! query the Component for info
    call ESMF_CplCompGet(cplcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! get transfer policy for both states
    call NUOPC_GetAttribute(importState, name="FieldTransferPolicy", &
        value=importXferPolicy, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    call NUOPC_GetAttribute(exportState, name="FieldTransferPolicy", &
        value=exportXferPolicy, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

!    print *, "importState xferPolicy = ", importXferPolicy
!    print *, "exportState xferPolicy = ", exportXferPolicy

    ! States on both sides must accept transfer
    if (trim(exportXferPolicy)=="transferAll" .and. &
        trim(importXferPolicy)=="transferAll") then

        call doTransfer(exportState, importState, rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

        call doTransfer(importState, exportState, rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    end if

#if 0
call ESMF_VMLogCurrentGarbageInfo("Connector Initialize05P1 out: ")
#endif

    contains

    subroutine doTransfer(fromState, toState, rc)

      type(ESMF_State), intent(inout) :: fromState
      type(ESMF_State), intent(inout) :: toState
      integer, intent(out) :: rc

      character(ESMF_MAXSTR) :: name
      character(ESMF_MAXSTR) :: oldTransferGeom, newTransferGeom
      integer                :: itemCount, i, stat
      character (ESMF_MAXSTR), allocatable :: itemNameList(:)
      type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
      type(ESMF_Field)       :: field

      rc = ESMF_SUCCESS

      call ESMF_StateGet(fromState, itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

      allocate(itemNameList(itemCount),stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out

      allocate(itemTypeList(itemCount),stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out

      call ESMF_StateGet(fromState, itemNameList=itemNameList, &
        itemTypeList=itemTypeList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

      ! WARNING: does not currently deal with nested states or field bundles
      do i=lbound(itemNameList,1), ubound(itemNameList,1)
        !print *, "conn export state item ", i, " = ", itemNameList(i), " type = ", itemTypeList(i)
        if (itemTypeList(i)==ESMF_STATEITEM_FIELD) then

          ! do not transfer if it already exists in the destination state
          call ESMF_StateGet(toState, &
            itemSearch=itemNameList(i), itemCount=itemCount, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          if (itemCount > 0) then
            cycle
          endif

          ! reverse TransferOfferGeomObject attribute, e.g., if a component
          ! providing a field wants to provide a grid, then the accepting
          ! component should not try to provide its own grid
          call ESMF_StateGet(fromState, &
            itemNameList(i), field, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

          call NUOPC_GetAttribute(field, name="TransferOfferGeomObject", &
             value=oldTransferGeom, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

          ! default
          newTransferGeom = "cannot provide"
          if (trim(oldTransferGeom)=="will provide") then
            newTransferGeom = "cannot provide"
          else if (trim(oldTransferGeom)=="can provide") then
            newTransferGeom = "cannot provide"
          else if (trim(oldTransferGeom)=="cannot provide") then
            newTransferGeom = "will provide"
          end if

          ! transfer to toState
          call NUOPC_Advertise(toState, StandardName=itemNameList(i), &
            TransferOfferGeomObject=newTransferGeom, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

        end if
     end do

     deallocate(itemNameList)
     deallocate(itemTypeList)

    end subroutine

  end subroutine


  !-----------------------------------------------------------------------------
  
  subroutine InitializeP1a(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    integer                               :: i, j
    integer                               :: bondLevel, bondLevelMax
    character(ESMF_MAXSTR)                :: name, valueString
    character(ESMF_MAXSTR), pointer       :: importStandardNameList(:)
    character(ESMF_MAXSTR), pointer       :: exportStandardNameList(:)
    type(ESMF_Field),       pointer       :: importFieldList(:)
    type(ESMF_Field),       pointer       :: exportFieldList(:)
    type(ESMF_Field)                      :: field
    character(ESMF_MAXSTR)                :: connectionString
    character(ESMF_MAXSTR), pointer       :: importNamespaceList(:)
    character(ESMF_MAXSTR), pointer       :: exportNamespaceList(:)
    integer                               :: profiling
    logical                               :: match

    rc = ESMF_SUCCESS

#if 0
call ESMF_VMLogCurrentGarbageInfo("Connector InitializeP1a in: ")
#endif

    ! query the Component for info
    call ESMF_CplCompGet(cplcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    ! determine profiling
    call NUOPC_CompAttributeGet(cplcomp, name="Profiling", value=valueString, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    profiling = ESMF_UtilString2Int(valueString, &
      specialStringList=(/"high", "max "/), specialValueList=(/65535, 65535/), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

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
call ESMF_VMLogCurrentGarbageInfo("Connector InitializeP1a after reconcile: ")
#endif

    nullify(importStandardNameList)
    nullify(importFieldList)
    nullify(importNamespaceList)
    nullify(exportStandardNameList)
    nullify(exportFieldList)
    nullify(exportNamespaceList)
    
    call NUOPC_GetStateMemberLists(importState, importStandardNameList, &
      fieldList=importFieldList, namespaceList=importNamespaceList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

#if 0
call printStringList("importStandardNameList", importStandardNameList)
call printStringList("importNamespaceList", importNamespaceList)
#endif
      
    call NUOPC_GetStateMemberLists(exportState, exportStandardNameList, &
      fieldList=exportFieldList, namespaceList=exportNamespaceList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

#if 0
call printStringList("exportStandardNameList", exportStandardNameList)
call printStringList("exportNamespaceList", exportNamespaceList)
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
              getBondLevel(importNamespaceList(i), exportNamespaceList(j))
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
    if (associated(exportStandardNameList)) deallocate(exportStandardNameList)
    if (associated(exportFieldList)) deallocate(exportFieldList)
    if (associated(exportNamespaceList)) deallocate(exportNamespaceList)
    
#if 0
call ESMF_VMLogCurrentGarbageInfo("Connector InitializeP1a out: ")
#endif

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP1b(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
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
    character(ESMF_MAXSTR), pointer       :: cplList(:)
    character(ESMF_MAXSTR)                :: msgString, valueString
    integer                               :: verbosity
    integer                               :: profiling
    logical                               :: match

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_CplCompGet(cplcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    ! determine profiling
    call NUOPC_CompAttributeGet(cplcomp, name="Profiling", value=valueString, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    profiling = ESMF_UtilString2Int(valueString, &
      specialStringList=(/"high", "max "/), specialValueList=(/65535, 65535/), &
      rc=rc)
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

    ! determine verbosity
    call NUOPC_CompAttributeGet(cplcomp, name="Verbosity", value=valueString, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    verbosity = ESMF_UtilString2Int(valueString, &
      specialStringList=(/"high", "max "/), specialValueList=(/65535, 65535/), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! set Attributes
    call NUOPC_CompAttributeSet(cplcomp, &
      name="ComponentLongName", value="NUOPC Generic Connector Component", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    nullify(importStandardNameList)
    nullify(importFieldList)
    nullify(importNamespaceList)
    nullify(exportStandardNameList)
    nullify(exportFieldList)
    nullify(exportNamespaceList)
    
    call NUOPC_GetStateMemberLists(importState, importStandardNameList, &
      fieldList=importFieldList, namespaceList=importNamespaceList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      
#if 0
call printStringList("importStandardNameList", importStandardNameList)
call printStringList("importNamespaceList", importNamespaceList)
#endif
      
    call NUOPC_GetStateMemberLists(exportState, exportStandardNameList, &
      fieldList=exportFieldList, namespaceList=exportNamespaceList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
#if 0
call printStringList("exportStandardNameList", exportStandardNameList)
call printStringList("exportNamespaceList", exportNamespaceList)
#endif
      
    ! associated pointers means that there are name lists
    if (associated(importStandardNameList) .and. &
      associated(exportStandardNameList)) then
      
      ! the maximum number of matches is limited by the larger list, because
      ! the same producer can be matched to multiple consumers
      maxCount = max(size(importStandardNameList), size(exportStandardNameList))
      allocate(cplList(maxCount)) ! temporary list

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
              getBondLevel(importNamespaceList(i), exportNamespaceList(j))
              
#if 0
print *, "current bondLevel=", bondLevel
#endif

            if (btest(verbosity,4)) then
              write (msgString,'(A, ": ", A30, I3, "): ", A30)') trim(name), &
                "exportStandardNameList(j=", j, exportStandardNameList(j)
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
              write (msgString,'(A, ": ", A30, I3, "): ", A30)') trim(name), &
                "exportNamespaceList(j=", j, exportNamespaceList(j)
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
              write (msgString,'(A, ": ", A30, I3, "): ", A30)') trim(name), &
                "importStandardNameList(i=", i, importStandardNameList(i)
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
                return  ! bail out
              write (msgString,'(A, ": ", A30, I3, "): ", A30)') trim(name), &
                "importNamespaceList(i=", i, importNamespaceList(i)
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
                ! ambiguity detected -> bail out
                call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
                  msg="Ambiguous connection status, multiple connections "// &
                  " with the same bondLevel were found for: "// &
                  trim(importStandardNameList(i)), &
                  line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
                return  ! bail out
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
                if (btest(verbosity,4)) then
                  write (msgString,'(A, ": added cplList(", I3, ")=", A30)') &
                    trim(name), count, cplList(count)
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
        endif
        deallocate(cplList)
      endif

    endif
    
    if (associated(importStandardNameList)) deallocate(importStandardNameList)
    if (associated(importFieldList)) deallocate(importFieldList)
    if (associated(importNamespaceList)) deallocate(importNamespaceList)
    if (associated(exportStandardNameList)) deallocate(exportStandardNameList)
    if (associated(exportFieldList)) deallocate(exportFieldList)
    if (associated(exportNamespaceList)) deallocate(exportNamespaceList)
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP1(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)                      :: internalClock
    character(ESMF_MAXSTR)                :: name

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_CplCompGet(cplcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

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

    ! Simply the combination of P1a + P1b
    call InitializeP1a(cplcomp, importState, exportState, clock, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call InitializeP1b(cplcomp, importState, exportState, clock, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP2(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    character(ESMF_MAXSTR), pointer :: cplList(:), chopStringList(:)
    character(ESMF_MAXSTR)          :: cplName
    integer                         :: cplListSize, i
    integer                         :: bondLevel, bondLevelMax
    character(ESMF_MAXSTR), pointer :: importNamespaceList(:)
    character(ESMF_MAXSTR), pointer :: exportNamespaceList(:)
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
    character(ESMF_MAXSTR)          :: name, valueString
    character(ESMF_MAXSTR)          :: iTransferOffer, eTransferOffer
    integer                         :: profiling
    logical                         :: matchE, matchI

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_CplCompGet(cplcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    ! determine profiling
    call NUOPC_CompAttributeGet(cplcomp, name="Profiling", value=valueString, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    profiling = ESMF_UtilString2Int(valueString, &
      specialStringList=(/"high", "max "/), specialValueList=(/65535, 65535/), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! prepare local pointer variables
    nullify(cplList)
    nullify(importStandardNameList)
    nullify(importFieldList)
    nullify(importNamespaceList)
    nullify(exportStandardNameList)
    nullify(exportFieldList)
    nullify(exportNamespaceList)
    
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
    endif
    ! get the importState and exportState std lists
    call NUOPC_GetStateMemberLists(importState, importStandardNameList, &
      fieldList=importFieldList, namespaceList=importNamespaceList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call NUOPC_GetStateMemberLists(exportState, exportStandardNameList, &
      fieldList=exportFieldList, namespaceList=exportNamespaceList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! prepare chopStringList
    nullify(chopStringList)

    ! main loop over all entries in the cplList
    do i=1, cplListSize
!print *, "cplList(",i,")=", trim(cplList(i))
      call chopString(cplList(i), chopChar=":", chopStringList=chopStringList, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      cplName = chopStringList(1) ! first part is the standard name of cpl field
      deallocate(chopStringList)

      ! find import and export side match
      foundFlag = .false. ! reset
      do eMatch=1, size(exportStandardNameList)  ! consumer side
        do iMatch=1, size(importStandardNameList)  ! producer side
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
              exportNamespaceList(eMatch))
              
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
        
        ! coordinate the transfer of geomobjects between components
        call NUOPC_GetAttribute(iField, name="TransferOfferGeomObject", &
          value=iTransferOffer, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        call NUOPC_GetAttribute(eField, name="TransferOfferGeomObject", &
          value=eTransferOffer, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
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
          elseif (trim(eTransferOffer)=="can provide") then
            ! -> import side must provide, export side must accept
            call NUOPC_SetAttribute(iField, &
              name="TransferActionGeomObject", value="provide", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_SetAttribute(eField, &
              name="TransferActionGeomObject", value="accept", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          else  ! eTransferOffer=="cannot provide"
            ! -> import side must provide, export side must accept
            call NUOPC_SetAttribute(iField, &
              name="TransferActionGeomObject", value="provide", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_SetAttribute(eField, &
              name="TransferActionGeomObject", value="accept", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          endif
        elseif (trim(iTransferOffer)=="can provide") then
          if (trim(eTransferOffer)=="will provide") then
            ! -> import side must accept, export side must provide
            call NUOPC_SetAttribute(iField, &
              name="TransferActionGeomObject", value="accept", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_SetAttribute(eField, &
              name="TransferActionGeomObject", value="provide", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          elseif (trim(eTransferOffer)=="can provide") then
            ! -> import side must provide, export side must accept
            call NUOPC_SetAttribute(iField, &
              name="TransferActionGeomObject", value="provide", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_SetAttribute(eField, &
              name="TransferActionGeomObject", value="accept", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          else  ! eTransferOffer=="cannot provide"
            ! -> import side must provide, export side must accept
            call NUOPC_SetAttribute(iField, &
              name="TransferActionGeomObject", value="provide", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_SetAttribute(eField, &
              name="TransferActionGeomObject", value="accept", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          endif
        else  ! iTransferOffer=="cannot provide"
          if (trim(eTransferOffer)=="will provide") then
            ! -> import side must accept, export side must provide
            call NUOPC_SetAttribute(iField, &
              name="TransferActionGeomObject", value="accept", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_SetAttribute(eField, &
              name="TransferActionGeomObject", value="provide", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          elseif (trim(eTransferOffer)=="can provide") then
            ! -> import side must accept, export side must provide
            call NUOPC_SetAttribute(iField, &
              name="TransferActionGeomObject", value="accept", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
            call NUOPC_SetAttribute(eField, &
              name="TransferActionGeomObject", value="provide", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          else  ! eTransferOffer=="cannot provide"
            ! -> neither side is able to provide -> error
            call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
              msg="Neither side (import/export) able to provide geom object.", &
              line=__LINE__, file=trim(name)//":"//FILENAME)
            return  ! bail out
          endif
        endif
      else
        !TODO: Fields mentioned via stdname in Cpl metadata not found -> error?
      endif

    enddo

    ! create the State member    
    is%wrap%state = ESMF_StateCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    if (associated(cplList)) deallocate(cplList)
    if (associated(importStandardNameList)) deallocate(importStandardNameList)
    if (associated(importFieldList)) deallocate(importFieldList)
    if (associated(importNamespaceList)) deallocate(importNamespaceList)
    if (associated(exportStandardNameList)) deallocate(exportStandardNameList)
    if (associated(exportFieldList)) deallocate(exportFieldList)
    if (associated(exportNamespaceList)) deallocate(exportNamespaceList)
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP3(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    character(ESMF_MAXSTR), pointer :: cplList(:), chopStringList(:)
    character(ESMF_MAXSTR)          :: cplName
    integer                         :: cplListSize, i
    integer                         :: bondLevel, bondLevelMax
    character(ESMF_MAXSTR), pointer :: importNamespaceList(:)
    character(ESMF_MAXSTR), pointer :: exportNamespaceList(:)
    character(ESMF_MAXSTR), pointer :: importStandardNameList(:)
    character(ESMF_MAXSTR), pointer :: exportStandardNameList(:)
    type(ESMF_Field),       pointer :: importFieldList(:)
    type(ESMF_Field),       pointer :: exportFieldList(:)
    integer                         :: iMatch, eMatch
    type(ESMF_Field)                :: iField, eField
    type(ESMF_Field)                :: providerField, acceptorField
    type(ESMF_GeomType_Flag)        :: geomtype
    type(ESMF_Grid)                 :: grid
    type(ESMF_Mesh)                 :: mesh
    type(ESMF_LocStream)            :: locstream
    type(ESMF_DistGrid)             :: providerDG, acceptorDG
    type(ESMF_DistGrid)             :: providerDG_nodal, acceptorDG_nodal
    type(ESMF_VM)                   :: vm
    integer                         :: stat
    type(type_InternalState)        :: is
    logical                         :: foundFlag
    character(ESMF_MAXSTR)          :: connectionString
    character(ESMF_MAXSTR)          :: name, valueString
    character(ESMF_MAXSTR)          :: geomobjname
    character(ESMF_MAXSTR)          :: iTransferAction, eTransferAction
    integer                         :: verbosity
    integer(ESMF_KIND_I4), pointer  :: ungriddedLBound(:), ungriddedUBound(:)
    integer(ESMF_KIND_I4), pointer  :: gridToFieldMap(:)
    integer                         :: fieldDimCount, gridDimCount
    integer                         :: profiling
    logical                         :: matchE, matchI

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_CplCompGet(cplcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    ! determine profiling
    call NUOPC_CompAttributeGet(cplcomp, name="Profiling", value=valueString, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    profiling = ESMF_UtilString2Int(valueString, &
      specialStringList=(/"high", "max "/), specialValueList=(/65535, 65535/), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! determine verbosity
    call NUOPC_CompAttributeGet(cplcomp, name="Verbosity", value=valueString, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    verbosity = ESMF_UtilString2Int(valueString, &
      specialStringList=(/"high", "max "/), specialValueList=(/65535, 65535/), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! prepare local pointer variables
    nullify(cplList)
    nullify(importStandardNameList)
    nullify(importFieldList)
    nullify(importNamespaceList)
    nullify(exportStandardNameList)
    nullify(exportFieldList)
    nullify(exportNamespaceList)
    
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
    endif
    ! get the importState and exportState std lists
    call NUOPC_GetStateMemberLists(importState, importStandardNameList, &
      fieldList=importFieldList, namespaceList=importNamespaceList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call NUOPC_GetStateMemberLists(exportState, exportStandardNameList, &
      fieldList=exportFieldList, namespaceList=exportNamespaceList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! prepare chopStringList
    nullify(chopStringList)
    
    ! main loop over all entries in the cplList
    do i=1, cplListSize
!print *, "cplList(",i,")=", trim(cplList(i))
      call chopString(cplList(i), chopChar=":", chopStringList=chopStringList, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      cplName = chopStringList(1) ! first part is the standard name of cpl field
      deallocate(chopStringList)
      
      ! find import and export side match
      foundFlag = .false. ! reset
      do eMatch=1, size(exportStandardNameList)  ! consumer side
        do iMatch=1, size(importStandardNameList)  ! producer side
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
              exportNamespaceList(eMatch))
              
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
        elseif ((trim(eTransferAction)=="provide") &
          .and.(trim(iTransferAction)=="accept")) then
          providerField = eField
          acceptorField = iField
        else  ! not a situation that needs handling here
          cycle ! continue with the next i
        endif
        
        if (btest(verbosity,1)) then
          call ESMF_LogWrite(trim(name)//": transferring underlying DistGrid", &
            ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        endif

        ! transfer the underlying DistGrid from provider to acceptor
        call ESMF_FieldGet(providerField, geomtype=geomtype, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        if (geomtype==ESMF_GEOMTYPE_GRID) then
          call ESMF_FieldGet(providerField, grid=grid, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          call ESMF_GridGet(grid, distgrid=providerDG, name=geomobjname, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          call ESMF_FieldGet(acceptorField, vm=vm, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          acceptorDG = ESMF_DistGridCreate(providerDG, vm=vm, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          grid = ESMF_GridCreate(acceptorDG, name=geomobjname, vm=vm, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          call ESMF_FieldEmptySet(acceptorField, grid=grid, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

          ! bring over more info as attributes
          call ESMF_FieldGet(providerField, grid=grid, &
            dimCount=fieldDimCount, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          call ESMF_GridGet(grid, dimCount=gridDimCount, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          ! bring over gridToFieldMap as attributes
          allocate(gridToFieldMap(gridDimCount),stat=stat)
          if (ESMF_LogFoundAllocError(statusToCheck=stat, &
            msg="Allocation of internal ungriddedLBound failed.", &
            line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
            return  ! bail out
          call ESMF_FieldGet(providerField, gridToFieldMap=gridToFieldMap, &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          call ESMF_AttributeSet(acceptorField, &
            name="GridToFieldMap", valueList=gridToFieldMap, &
            convention="NUOPC", purpose="Instance", &
            attnestflag=ESMF_ATTNEST_ON, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
          deallocate(gridToFieldMap)

!          print *, "fieldDimCount = ", fieldDimCount
!          print *, "gridDimCount = ", gridDimCount
          if (fieldDimCount - gridDimCount > 0) then
            ! bring over ungridded dim bounds as attributes
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
            deallocate(ungriddedLBound)
            deallocate(ungriddedUBound)
          endif
        elseif (geomtype==ESMF_GEOMTYPE_MESH) then
          call ESMF_FieldGet(providerField, mesh=mesh, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          call ESMF_MeshGet(mesh, elementDistgrid=providerDG, &
            nodalDistgrid=providerDG_nodal, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          call ESMF_FieldGet(acceptorField, vm=vm, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
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
          call ESMF_FieldEmptySet(acceptorField, mesh=mesh, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
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
          acceptorDG = ESMF_DistGridCreate(providerDG, vm=vm, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          locstream = ESMF_LocStreamCreate(distgrid=acceptorDG, &
            name=geomobjname, vm=vm, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          call ESMF_FieldEmptySet(acceptorField, locstream=locstream, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        else
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="Provided GeomType must be Grid or Mesh.", &
            line=__LINE__, file=trim(name)//":"//FILENAME)
          return  ! bail out
        endif

        if (btest(verbosity,1)) then
          call ESMF_LogWrite(trim(name)//&
            ": done transferring underlying DistGrid", &
            ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        endif

      else
        !TODO: Fields mentioned via stdname in Cpl metadata not found -> error?
      endif

    enddo

    if (associated(cplList)) deallocate(cplList)
    if (associated(importStandardNameList)) deallocate(importStandardNameList)
    if (associated(importFieldList)) deallocate(importFieldList)
    if (associated(importNamespaceList)) deallocate(importNamespaceList)
    if (associated(exportStandardNameList)) deallocate(exportStandardNameList)
    if (associated(exportFieldList)) deallocate(exportFieldList)
    if (associated(exportNamespaceList)) deallocate(exportNamespaceList)
    
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP4(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    character(ESMF_MAXSTR), pointer :: cplList(:), chopStringList(:)
    character(ESMF_MAXSTR)          :: cplName
    integer                         :: cplListSize, i
    integer                         :: bondLevel, bondLevelMax
    character(ESMF_MAXSTR), pointer :: importNamespaceList(:)
    character(ESMF_MAXSTR), pointer :: exportNamespaceList(:)
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
    type(ESMF_LocStream)            :: providerLocstream, acceptorLocstream
    logical                         :: meshNoConnections
    type(ESMF_DistGrid)             :: distgrid, eDistgrid, nDistgrid
    integer                         :: stat
    type(type_InternalState)        :: is
    logical                         :: foundFlag
    character(ESMF_MAXSTR)          :: connectionString
    character(ESMF_MAXSTR)          :: name, valueString
    character(ESMF_MAXSTR)          :: geomobjname
    character(ESMF_MAXSTR)          :: iTransferAction, eTransferAction
    integer                         :: verbosity
    integer                         :: profiling
    logical                         :: matchE, matchI

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_CplCompGet(cplcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    ! determine profiling
    call NUOPC_CompAttributeGet(cplcomp, name="Profiling", value=valueString, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    profiling = ESMF_UtilString2Int(valueString, &
      specialStringList=(/"high", "max "/), specialValueList=(/65535, 65535/), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! determine verbosity
    call NUOPC_CompAttributeGet(cplcomp, name="Verbosity", value=valueString, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    verbosity = ESMF_UtilString2Int(valueString, &
      specialStringList=(/"high", "max "/), specialValueList=(/65535, 65535/), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! prepare local pointer variables
    nullify(cplList)
    nullify(importStandardNameList)
    nullify(importFieldList)
    nullify(importNamespaceList)
    nullify(exportStandardNameList)
    nullify(exportFieldList)
    nullify(exportNamespaceList)
    
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
    endif
    ! get the importState and exportState std lists
    call NUOPC_GetStateMemberLists(importState, importStandardNameList, &
      fieldList=importFieldList, namespaceList=importNamespaceList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call NUOPC_GetStateMemberLists(exportState, exportStandardNameList, &
      fieldList=exportFieldList, namespaceList=exportNamespaceList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! prepare chopStringList
    nullify(chopStringList)

    ! main loop over all entries in the cplList
    do i=1, cplListSize
!print *, "cplList(",i,")=", trim(cplList(i))
      call chopString(cplList(i), chopChar=":", chopStringList=chopStringList, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      cplName = chopStringList(1) ! first part is the standard name of cpl field
      deallocate(chopStringList)
      
      ! find import and export side match
      foundFlag = .false. ! reset
      do eMatch=1, size(exportStandardNameList)  ! consumer side
        do iMatch=1, size(importStandardNameList)  ! producer side
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
              exportNamespaceList(eMatch))
              
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
        elseif ((trim(eTransferAction)=="provide") &
          .and.(trim(iTransferAction)=="accept")) then
          providerField = eField
          acceptorField = iField
        else  ! not a situation that needs handling here
          cycle ! continue with the next i
        endif

        if (btest(verbosity,1)) then
          call ESMF_LogWrite(trim(name)//": transferring the full Grid/Mesh", &
            ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        endif

        ! transfer the underlying Grid/Mesh from provider to acceptor
        call ESMF_FieldGet(providerField, geomtype=geomtype, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        if (geomtype==ESMF_GEOMTYPE_GRID) then
          call ESMF_FieldGet(providerField, grid=providerGrid, rc=rc)
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
            name=geomobjname, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          call ESMF_FieldEmptySet(acceptorField, grid=acceptorGrid, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        elseif (geomtype==ESMF_GEOMTYPE_MESH) then
          call ESMF_FieldGet(providerField, mesh=providerMesh, rc=rc)
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
          call ESMF_FieldEmptySet(acceptorField, mesh=acceptorMesh, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        elseif (geomtype==ESMF_GEOMTYPE_LOCSTREAM) then
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
        else
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="Provided GeomType must be Grid or Mesh.", &
            line=__LINE__, file=trim(name)//":"//FILENAME)
          return  ! bail out
        endif
          
        if (btest(verbosity,1)) then
          call ESMF_LogWrite(trim(name)//&
            ": done transferring the full Grid/Mesh", &
            ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        endif

      else
        !TODO: Fields mentioned via stdname in Cpl metadata not found -> error?
      endif

    enddo

    if (associated(cplList)) deallocate(cplList)
    if (associated(importStandardNameList)) deallocate(importStandardNameList)
    if (associated(importFieldList)) deallocate(importFieldList)
    if (associated(importNamespaceList)) deallocate(importNamespaceList)
    if (associated(exportStandardNameList)) deallocate(exportStandardNameList)
    if (associated(exportFieldList)) deallocate(exportFieldList)
    if (associated(exportNamespaceList)) deallocate(exportNamespaceList)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP5a(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    character(ESMF_MAXSTR)          :: name, valueString
    integer                         :: profiling

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_CplCompGet(cplcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    ! determine profiling
    call NUOPC_CompAttributeGet(cplcomp, name="Profiling", value=valueString, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    profiling = ESMF_UtilString2Int(valueString, &
      specialStringList=(/"high", "max "/), specialValueList=(/65535, 65535/), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! re-reconcile the States because they may have changed
    ! (previous proxy objects are dropped before fresh reconcile)
    if (btest(profiling,1)) then    ! PROFILE
      call ESMF_VMLogMemInfo("befP5 Reconcile")
    endif
    call NUOPC_Reconcile(importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call NUOPC_Reconcile(exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (btest(profiling,1)) then    ! PROFILE
      call ESMF_VMLogMemInfo("aftP5 Reconcile")
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP5b(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    character(ESMF_MAXSTR), pointer :: cplList(:), chopStringList(:)
    character(ESMF_MAXSTR)          :: cplName
    integer                         :: cplListSize, i
    integer                         :: bondLevel, bondLevelMax
    character(ESMF_MAXSTR), pointer :: importNamespaceList(:)
    character(ESMF_MAXSTR), pointer :: exportNamespaceList(:)
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
    character(ESMF_MAXSTR)          :: name, valueString, msgString, iString
    integer                         :: verbosity
    logical                         :: matchE, matchI

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_CplCompGet(cplcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! determine verbosity
    call NUOPC_CompAttributeGet(cplcomp, name="Verbosity", value=valueString, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    verbosity = ESMF_UtilString2Int(valueString, &
      specialStringList=(/"high", "max "/), specialValueList=(/65535, 65535/), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! prepare local pointer variables
    nullify(cplList)
    nullify(importStandardNameList)
    nullify(importFieldList)
    nullify(importNamespaceList)
    nullify(exportStandardNameList)
    nullify(exportFieldList)
    nullify(exportNamespaceList)
    
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
    endif
    ! get the importState and exportState std lists
    call NUOPC_GetStateMemberLists(importState, importStandardNameList, &
      fieldList=importFieldList, namespaceList=importNamespaceList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call NUOPC_GetStateMemberLists(exportState, exportStandardNameList, &
      fieldList=exportFieldList, namespaceList=exportNamespaceList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! prepare FieldBundles to store src and dst Fields
    is%wrap%srcFields = ESMF_FieldBundleCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    is%wrap%dstFields = ESMF_FieldBundleCreate(rc=rc)
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

      if (btest(verbosity,3)) then
        write (iString,'(I4)') i
        write (msgString,*) "loop over all entries in cplList: "// &
          trim(adjustl(iString))//": "//trim(cplName)
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
          return  ! bail out
      endif
      
      ! find import and export side match
      foundFlag = .false. ! reset
      do eMatch=1, size(exportStandardNameList)  ! consumer side
        do iMatch=1, size(importStandardNameList)  ! producer side
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
              exportNamespaceList(eMatch))
              
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
        
        ! add the import and export Fields to FieldBundles
        call ESMF_FieldBundleAdd(is%wrap%srcFields, (/iField/), &
          multiflag=.true., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        call ESMF_FieldBundleAdd(is%wrap%dstFields, (/eField/), &
          multiflag=.true., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
          
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
      call FieldBundleCplStore(is%wrap%srcFields, is%wrap%dstFields, &
        cplList=cplList, rh=is%wrap%rh, termOrders=is%wrap%termOrders, &
        name=name, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      if (btest(verbosity,2)) then
        call ESMF_LogWrite(trim(name)//&
          ": called default label_ComputeRouteHandle", &
          ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      endif
    else
      if (btest(verbosity,2)) then
        call ESMF_LogWrite(trim(name)//&
          ": called specialized label_ComputeRouteHandle", &
          ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      endif    
    endif
    
    ! populate remaining internal state members
    call ESMF_FieldBundleGet(is%wrap%srcFields, &
      fieldCount=is%wrap%srcFieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    allocate(is%wrap%srcFieldList(is%wrap%srcFieldCount))
    call ESMF_FieldBundleGet(is%wrap%srcFields, &
      fieldList=is%wrap%srcFieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call ESMF_FieldBundleGet(is%wrap%dstFields, &
      fieldCount=is%wrap%dstFieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    allocate(is%wrap%dstFieldList(is%wrap%dstFieldCount))
    call ESMF_FieldBundleGet(is%wrap%dstFields, &
      fieldList=is%wrap%dstFieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! clean-up
    if (associated(cplList)) deallocate(cplList)
    if (associated(importStandardNameList)) deallocate(importStandardNameList)
    if (associated(importFieldList)) deallocate(importFieldList)
    if (associated(importNamespaceList)) deallocate(importNamespaceList)
    if (associated(exportStandardNameList)) deallocate(exportStandardNameList)
    if (associated(exportFieldList)) deallocate(exportFieldList)
    if (associated(exportNamespaceList)) deallocate(exportNamespaceList)
    
  end subroutine
    
  !-----------------------------------------------------------------------------

  subroutine Initialize00P2a(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)                      :: internalClock
    character(ESMF_MAXSTR)                :: name

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_CplCompGet(cplcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! Simply the combination of P2 + P5a
    call InitializeP2(cplcomp, importState, exportState, clock, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call InitializeP5a(cplcomp, importState, exportState, clock, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine Initialize00P2b(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)                      :: internalClock
    character(ESMF_MAXSTR)                :: name

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_CplCompGet(cplcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! Simply same as P5b
    call InitializeP5b(cplcomp, importState, exportState, clock, rc)
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
    type(type_InternalState)  :: is
    type(ESMF_VM)             :: vm
    integer                   :: localrc
    logical                   :: existflag
    integer                   :: rootPet, rootVas, vas, petCount
    character(ESMF_MAXSTR)    :: compName, msgString, valueString, pLabel
    integer                   :: phase
    integer                   :: verbosity
    integer                   :: profiling
    character(ESMF_MAXSTR)    :: name

    real(ESMF_KIND_R8)        :: timeBase, time0, time

    rc = ESMF_SUCCESS

    ! PROFILE base time
    call ESMF_VMWtime(timeBase)
    time0=timeBase

#if 0
call ESMF_VMLogCurrentGarbageInfo("Connector RUN enter: ")
#endif

    ! query the Component for info
    call ESMF_CplCompGet(cplcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
        
    ! determine profiling
    call NUOPC_CompAttributeGet(cplcomp, name="Profiling", value=valueString, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    profiling = ESMF_UtilString2Int(valueString, &
      specialStringList=(/"high", "max "/), specialValueList=(/65535, 65535/), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! determine verbosity
    call NUOPC_CompAttributeGet(cplcomp, name="Verbosity", value=valueString, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    verbosity = ESMF_UtilString2Int(valueString, &
      specialStringList=(/"high", "max "/), specialValueList=(/65535, 65535/), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! get the compName and currentPhase
    call ESMF_CplCompGet(cplcomp, name=compName, currentPhase=phase, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! conditionally output diagnostic to Log file
    if (btest(verbosity,0)) then
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
    
    if (btest(profiling,0)) then    ! PROFILE
      call ESMF_VMWtime(time)
      write (msgString, *) "ConnectorProfile 01 time=   ", &
        time-time0, time-timeBase
        time0=time
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
    endif

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(cplcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      
    if (btest(profiling,0)) then    ! PROFILE
      ! PROFILE
      call ESMF_VMWtime(time)
      write (msgString, *) "ConnectorProfile 02 time=   ", &
        time-time0, time-timeBase
        time0=time
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
    endif

    !TODO: here may be the place to ensure incoming States are consistent
    !TODO: with the Fields held in the FieldBundle inside the internal State?
      
    if (btest(profiling,0)) then    ! PROFILE
      ! PROFILE
      call ESMF_VMWtime(time)
      write (msgString, *) "ConnectorProfile 03 time=   ", &
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

    if (btest(profiling,0)) then    ! PROFILE
      ! PROFILE
      call ESMF_VMWtime(time)
      write (msgString, *) "ConnectorProfile 04 time=   ", &
        time-time0, time-timeBase
        time0=time
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
    endif

    if (.not.existflag) then
      ! if not specialized -> use default method to:
      ! execute the regrid operation
      call ESMF_FieldBundleRegrid(is%wrap%srcFields, is%wrap%dstFields, &
        routehandle=is%wrap%rh, termorderflag=is%wrap%termOrders, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      if (btest(verbosity,2)) then
        call ESMF_LogWrite(trim(name)//&
          ": called default label_ExecuteRouteHandle", &
          ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      endif
    else
      if (btest(verbosity,2)) then
        call ESMF_LogWrite(trim(name)//&
          ": called specialized label_ExecuteRouteHandle", &
          ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      endif    
    endif
    
    if (btest(profiling,0)) then    ! PROFILE
      ! PROFILE
      call ESMF_VMWtime(time)
      write (msgString, *) "ConnectorProfile 05 time=   ", &
        time-time0, time-timeBase
        time0=time
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
    endif

    ! Next update the TimeStamp metadata on the export Fields....

    ! get the rootPet attribute out of the importState
    call ESMF_AttributeGet(importState, name="rootVas", value=rootVas, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    if (btest(profiling,0)) then    ! PROFILE
      ! PROFILE
      call ESMF_VMWtime(time)
      write (msgString, *) "ConnectorProfile 06 time=   ", &
        time-time0, time-timeBase
        time0=time
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
    endif

    call ESMF_CplCompGet(cplcomp, vm=vm, petCount=petCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    if (btest(profiling,0)) then    ! PROFILE
      ! PROFILE
      call ESMF_VMWtime(time)
      write (msgString, *) "ConnectorProfile 07 time=   ", &
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
    
    if (btest(profiling,0)) then    ! PROFILE
      ! PROFILE
      call ESMF_VMWtime(time)
      write (msgString, *) "ConnectorProfile 08 time=   ", &
        time-time0, time-timeBase
        time0=time
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
    endif

    !TODO: bail out if rootPet not found

    ! hand coded, specific AttributeUpdate
    call NUOPC_UpdateTimestamp(is%wrap%srcFieldList, rootPet=rootPet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    if (btest(profiling,0)) then    ! PROFILE
      ! PROFILE
      call ESMF_VMWtime(time)
      write (msgString, *) "ConnectorProfile 09 time=   ", &
        time-time0, time-timeBase
        time0=time
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
    endif

    ! update the timestamp on all of the dst fields to that on the src side
    call NUOPC_UpdateTimestamp(is%wrap%srcFieldList, is%wrap%dstFieldList, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    if (btest(profiling,0)) then    ! PROFILE
      ! PROFILE
      call ESMF_VMWtime(time)
      write (msgString, *) "ConnectorProfile 10 time=   ", &
        time-time0, time-timeBase
        time0=time
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
    endif

    ! conditionally output diagnostic to Log file
    if (btest(verbosity,0)) then
      write (msgString,"(A)") "<<<"//trim(compName)//&
      " leaving Run (phase="//trim(adjustl(pLabel))//")"
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    
#if 0
call ESMF_VMLogCurrentGarbageInfo("Connector RUN leaving: ")
#endif

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine Finalize(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    integer                   :: stat
    type(type_InternalState)  :: is
    integer                   :: localrc
    logical                   :: existflag
    character(ESMF_MAXSTR)    :: name, valueString
    integer                   :: verbosity

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_CplCompGet(cplcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
#if 0
call ESMF_VMLogCurrentGarbageInfo("Connector finalize enter: ")
#endif

    ! determine verbosity
    call NUOPC_CompAttributeGet(cplcomp, name="Verbosity", value=valueString, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    verbosity = ESMF_UtilString2Int(valueString, &
      specialStringList=(/"high", "max "/), specialValueList=(/65535, 65535/), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(cplcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      
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
      call ESMF_FieldBundleRegridRelease(is%wrap%rh, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      if (btest(verbosity,2)) then
        call ESMF_LogWrite(trim(name)//&
          ": called default label_ReleaseRouteHandle", &
          ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      endif
    else
      if (btest(verbosity,2)) then
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
    deallocate(is%wrap%srcFieldList, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Deallocation of internal state srcFieldList member failed.", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    deallocate(is%wrap%dstFieldList, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
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
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Deallocation of termOrders list.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif
    
    ! deallocate internal state memory
    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
      
#if 0
call ESMF_VMLogCurrentGarbageInfo("Connector finalize leaving: ")
#endif

  end subroutine
  
  !-----------------------------------------------------------------------------
  !----- Helper routines below ...
  !-----------------------------------------------------------------------------

  function getBondLevel(imNamespace, exNamespace)
    integer           :: getBondLevel
    character(len=*)  :: imNamespace, exNamespace
    character(len=80) :: imKey1, imKey2, imKey
    character(len=80) :: exKey1, exKey2, exKey
    integer           :: imMark1, imMark2
    integer           :: exMark1, exMark2
    
    getBondLevel = 1 ! reset
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
    character(*),              pointer               :: cplList(:)
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
    type(ESMF_UnmappedAction_Flag)  :: unmappedaction
    type(ESMF_PoleMethod_Flag)      :: polemethod
    integer                         :: regridPoleNPnts
    integer(ESMF_KIND_I4), pointer  :: srcMaskValues(:)
    integer(ESMF_KIND_I4), pointer  :: dstMaskValues(:)
    integer                         :: srcTermProcessing, pipelineDepth
    logical                         :: dumpWeightsFlag
    type(ESMF_Grid)                 :: srcGrid, dstGrid
    type(ESMF_GeomType_Flag)        :: srcGeomtype, dstGeomtype
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
      ! field specific items, TODO: offer FieldMatch
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
      type(RHL), pointer                :: prev
    end type
    
    type(RHL), pointer              :: rhList, rhListE
    logical                         :: rhListMatch
    
#if 0
call ESMF_VMLogCurrentGarbageInfo("Connector FieldBundleCplStore enter: ")
#endif

    ! consistency check counts
    if (associated(cplList)) then
      count = size(cplList)
    else
      count = 0
    endif
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
        call ESMF_FieldGet(srcFields(i), grid=srcGrid, &
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
        
        call ESMF_FieldGet(dstFields(i), grid=dstGrid, &
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
          if (rhListMatch) exit ! break out of search 
123       continue
          rhListE=>rhListE%prev   ! previous element
        enddo
        
      endif

      if (.not.rhListMatch) then
#if 1
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
          rhListE%rh = rhh
          rhListE%factorIndexList => factorIndexList
          rhListE%factorList => factorList
        endif
      else
#if 1
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
          fileName="weights_"//trim(name)//"_"//trim(chopStringList(1))//".nc",&
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
        ! grid pairs will have factorIndexList and factorList in rhList struct
        if (associated(factorIndexList)) deallocate(factorIndexList)
        if (associated(factorList)) deallocate(factorList)
      endif
      if (associated(chopStringList)) deallocate(chopStringList)
      if (associated(srcMaskValues)) deallocate(srcMaskValues)
      if (associated(dstMaskValues)) deallocate(dstMaskValues)
      
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
      deallocate(rhListE)
    enddo

    ! garbage collection
    deallocate(srcFields, dstFields)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

#if 0
call ESMF_VMLogCurrentGarbageInfo("Connector FieldBundleCplStore leaving: ")
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
  subroutine NUOPC_ConnectorGet(connector, srcFields, dstFields, rh, state, rc)
! !ARGUMENTS:
    type(ESMF_CplComp)                            :: connector
    type(ESMF_FieldBundle), intent(out), optional :: srcFields
    type(ESMF_FieldBundle), intent(out), optional :: dstFields
    type(ESMF_RouteHandle), intent(out), optional :: rh
    type(ESMF_State),       intent(out), optional :: state
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
! Get parameters from the {\tt connector} internal state.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is

    if (present(rc)) rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_CplCompGet(connector, name=name, rc=rc)
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
    
    ! Get the requested members
    if (present(srcFields)) srcFields = is%wrap%srcFields
    if (present(dstFields)) dstFields = is%wrap%dstFields
    if (present(rh))        rh = is%wrap%rh
    if (present(state))     state = is%wrap%state
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_ConnectorSet - Set parameters in a Connector
!
! !INTERFACE:
  subroutine NUOPC_ConnectorSet(connector, srcFields, dstFields, rh, state, rc)
! !ARGUMENTS:
    type(ESMF_CplComp)                            :: connector
    type(ESMF_FieldBundle), intent(in),  optional :: srcFields
    type(ESMF_FieldBundle), intent(in),  optional :: dstFields
    type(ESMF_RouteHandle), intent(in),  optional :: rh
    type(ESMF_State),       intent(in),  optional :: state
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
! Set parameters in the {\tt connector} internal state.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)          :: name
    type(type_InternalState)        :: is

    if (present(rc)) rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_CplCompGet(connector, name=name, rc=rc)
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
    if (present(srcFields)) is%wrap%srcFields = srcFields
    if (present(dstFields)) is%wrap%dstFields = dstFields
    if (present(rh))        is%wrap%rh = rh
    if (present(state))     is%wrap%state = state
    
  end subroutine
  !-----------------------------------------------------------------------------

end module
