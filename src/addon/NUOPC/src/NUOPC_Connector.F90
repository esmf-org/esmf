! $Id: NUOPC_Connector.F90,v 1.20 2012/10/26 21:14:57 theurich Exp $

#define FILENAME "src/addon/NUOPC/NUOPC_Connector.F90"

module NUOPC_Connector

  !-----------------------------------------------------------------------------
  ! Generic Coupler Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC

  implicit none
  
  private
  
  public routine_SetServices
  public type_InternalState, type_InternalStateStruct
  public label_InternalState
  public label_ComputeRouteHandle, label_ExecuteRouteHandle, &
    label_ReleaseRouteHandle
  
  character(*), parameter :: &
    label_InternalState = "Connector_InternalState"
  character(*), parameter :: &
    label_ComputeRouteHandle = "Connector_ComputeRH"
  character(*), parameter :: &
    label_ExecuteRouteHandle = "Connector_ExecuteRH"
  character(*), parameter :: &
    label_ReleaseRouteHandle = "Connector_ReleaseRH"

  type type_InternalStateStruct
    type(ESMF_FieldBundle)  :: srcFields
    type(ESMF_FieldBundle)  :: dstFields
    type(ESMF_RouteHandle)  :: rh
    type(ESMF_State)        :: state
  end type

  type type_InternalState
    type(type_InternalStateStruct), pointer :: wrap
  end type

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine routine_SetServices(cplcomp, rc)
    type(ESMF_CplComp)   :: cplcomp
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP1, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_RUN, &
      userRoutine=Run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_FINALIZE, &
      userRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP0(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_StateIntent_Flag)           :: isType, esType
    integer                               :: isItemCount, esItemCount
    
    rc = ESMF_SUCCESS
    
    ! reconcile the States
    call ESMF_StateReconcile(importState, attreconflag=ESMF_ATTRECONCILE_ON, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_StateReconcile(exportState, attreconflag=ESMF_ATTRECONCILE_ON, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    ! access the state types
    call ESMF_StateGet(importState, stateintent=isType, itemCount=isItemCount, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_StateGet(exportState, stateintent=esType, itemCount=esItemCount, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    if (.not.((isType==ESMF_STATEINTENT_EXPORT).and.(esType==ESMF_STATEINTENT_IMPORT))) then
      ! not ES -> IS ==> should indicate problem???
    endif
    
    ! look for matching Fields and set as "CplList" metadata
    call NUOPC_CplCompAttributeSet(cplcomp, importState, exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP1(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_StateIntent_Flag)           :: isType, esType
    integer                               :: isItemCount, esItemCount
    character(ESMF_MAXSTR), pointer       :: cplList(:)
    integer                               :: cplListSize, i, j
    character(ESMF_MAXSTR), pointer       :: importStdAttrNameList(:)
    character(ESMF_MAXSTR), pointer       :: importStdItemNameList(:)
    character(ESMF_MAXSTR), pointer       :: exportStdAttrNameList(:)
    character(ESMF_MAXSTR), pointer       :: exportStdItemNameList(:)
    integer                               :: iMatch, eMatch
    type(ESMF_Field)                      :: iField, eField
    integer                               :: stat
    type(type_InternalState)              :: is
    integer                               :: localrc
    logical                               :: existflag
    
    rc = ESMF_SUCCESS
    
    ! prepare local pointer variables
    nullify(importStdAttrNameList)
    nullify(importStdItemNameList)
    nullify(exportStdAttrNameList)
    nullify(exportStdItemNameList)
    
    ! allocate memory for the internal state and set it in the Component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal state memory failed.", &
      line=__LINE__, &
      file=FILENAME, &
      rcToReturn=rc)) &
      return  ! bail out
    call ESMF_UserCompSetInternalState(cplcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! re-reconcile the States because they may have changed
    ! (previous proxy objects are dropped before fresh reconcile)
    call ESMF_StateReconcile(importState, attreconflag=ESMF_ATTRECONCILE_ON, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_StateReconcile(exportState, attreconflag=ESMF_ATTRECONCILE_ON, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    ! access the state types
    call ESMF_StateGet(importState, stateintent=isType, itemCount=isItemCount, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_StateGet(exportState, stateintent=esType, itemCount=esItemCount, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    if (.not.((isType==ESMF_STATEINTENT_EXPORT).and.(esType==ESMF_STATEINTENT_IMPORT))) then
      ! not ES -> IS ==> should indicate problem???
    endif
    
    ! get the cplList Attribute
    call NUOPC_CplCompAttributeGet(cplcomp, cplListSize=cplListSize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    allocate(cplList(cplListSize), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal cplList() failed.", &
      line=__LINE__, &
      file=FILENAME, &
      rcToReturn=rc)) &
      return  ! bail out
    call NUOPC_CplCompAttributeGet(cplcomp, cplList=cplList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    ! get the importState std lists
    call NUOPC_StateBuildStdList(importState, importStdAttrNameList, &
      importStdItemNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    ! get the exportState std lists
    call NUOPC_StateBuildStdList(exportState, exportStdAttrNameList, &
      exportStdItemNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    ! prepare FieldBundles to store src and dst Fields
    is%wrap%srcFields = ESMF_FieldBundleCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    is%wrap%dstFields = ESMF_FieldBundleCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    do i=1, cplListSize
!print *, "cplList(",i,")=", trim(cplList(i))

      iMatch = 0  ! reset
      do j=1, size(importStdAttrNameList)
        if (importStdAttrNameList(j) == cplList(i)) then
          iMatch = j
          exit
        endif
      enddo
      
!if (iMatch > 0) &
!print *, "found match for importStdItemNameList()=", importStdItemNameList(iMatch)

      eMatch = 0  ! reset
      do j=1, size(exportStdAttrNameList)
        if (exportStdAttrNameList(j) == cplList(i)) then
          eMatch = j
          exit
        endif
      enddo
      
!if (eMatch > 0) &
!print *, "found match for exportStdItemNameList()=", exportStdItemNameList(eMatch)

      if (iMatch>0 .and. eMatch>0) then
        ! there are matching Fields in the import and export States
        call ESMF_StateGet(importState, field=iField, &
          itemName=importStdItemNameList(iMatch), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
        call ESMF_StateGet(exportState, field=eField, &
          itemName=exportStdItemNameList(eMatch), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
        
        ! add the import and export Fields to FieldBundles
        call ESMF_FieldBundleAdd(is%wrap%srcFields, (/iField/), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
        call ESMF_FieldBundleAdd(is%wrap%dstFields, (/eField/), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
          
        ! set the connected Attribute on import Field
        call ESMF_AttributeSet(iField, &
          name="Connected", value="true", &
          convention="NUOPC", purpose="General", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
        ! set the connected Attribute on export Field
        call ESMF_AttributeSet(eField, &
          name="Connected", value="true", &
          convention="NUOPC", purpose="General", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
      else
        !TODO: Fields mentioned via stdname in Cpl metadata not found -> error?
      endif

    enddo

    ! create the State member    
    is%wrap%state = ESMF_StateCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    ! SPECIALIZE by calling into attached method to precompute routehandle
    call ESMF_MethodExecute(cplcomp, label=label_ComputeRouteHandle, &
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

    if (.not.existflag) then
      ! if not specialized -> use default method to:
      ! precompute the regrid for all src to dst Fields
      call ESMF_FieldBundleRegridStore(is%wrap%srcFields, is%wrap%dstFields, &
        unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
        routehandle=is%wrap%rh, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    endif

    deallocate(cplList)

    if (associated(importStdAttrNameList)) deallocate(importStdAttrNameList)
    if (associated(importStdItemNameList)) deallocate(importStdItemNameList)
    if (associated(exportStdAttrNameList)) deallocate(exportStdAttrNameList)
    if (associated(exportStdItemNameList)) deallocate(exportStdItemNameList)
    
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
    character(ESMF_MAXSTR)    :: compName, msgString, valueString
    integer                   :: phase
    logical                   :: verbose
 
    rc = ESMF_SUCCESS
    
    ! determine verbosity
    verbose = .false. ! initialize
    call ESMF_AttributeGet(cplcomp, name="Verbosity", value=valueString, &
      convention="NUOPC", purpose="General", rc=rc)
    if (trim(valueString)=="high") &
      verbose = .true.
    
    ! get the compName and currentPhase
    call ESMF_CplCompGet(cplcomp, name=compName, currentPhase=phase, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(cplcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    !TODO: here may be the place to ensure incoming States are consistent
    !TODO: with the Fields held in the FieldBundle inside the internal State?
      
    ! conditionally output diagnostic to Log file
    if (verbose) then
      write(msgString,"(A)") ">>>"//trim(compName)//" entered Run"
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    endif
    
    ! SPECIALIZE by calling into attached method to execute routehandle
    call ESMF_MethodExecute(cplcomp, label=label_ExecuteRouteHandle, &
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

    if (.not.existflag) then
      ! if not specialized -> use default method to:
      ! execute the regrid operation
      call ESMF_FieldBundleRegrid(is%wrap%srcFields, is%wrap%dstFields, &
        routehandle=is%wrap%rh, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    endif
    
    ! Next update the TimeStamp metadata on the export Fields....

    ! get the rootPet attribute out of the importState
    call ESMF_AttributeGet(importState, name="rootVas", value=rootVas, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    call ESMF_CplCompGet(cplcomp, vm=vm, petCount=petCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    do rootPet=0, petCount-1
      call ESMF_VMGet(vm, rootPet, vas=vas, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      if (vas == rootVas) exit
    enddo
    
    !TODO: bail out if rootPet not found

    ! hand coded, specific AttributeUpdate
    call NUOPC_StateUpdateTimestamp(importState, rootPet=rootPet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! update the timestamp on all of the dst fields to that on the src side
    call NUOPC_FieldBundleUpdateTime(srcFields=is%wrap%srcFields, &
      dstFields=is%wrap%dstFields, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! conditionally output diagnostic to Log file
    if (verbose) then
      write(msgString,"(A)") "<<<"//trim(compName)//" leaving Run"
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    endif
    
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

    rc = ESMF_SUCCESS
    
    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(cplcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    ! SPECIALIZE by calling into attached method to release routehandle
    call ESMF_MethodExecute(cplcomp, label=label_ReleaseRouteHandle, &
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

    if (.not.existflag) then
      ! if not specialized -> use default method to:
      ! release the regrid operation
      call ESMF_FieldBundleRegridRelease(is%wrap%rh, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    endif

    ! deallocate remaining internal state memebers
    call ESMF_FieldBundleDestroy(is%wrap%srcFields, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_FieldBundleDestroy(is%wrap%dstFields, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_StateDestroy(is%wrap%state, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
        
    ! deallocate internal state memory
    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, &
      file=FILENAME, &
      rcToReturn=rc)) &
      return  ! bail out
      
  end subroutine
  
end module
