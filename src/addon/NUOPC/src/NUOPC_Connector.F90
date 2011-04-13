! $Id: NUOPC_Connector.F90,v 1.1 2011/04/13 23:12:16 theurich Exp $
module NUOPC_Connector

  !-----------------------------------------------------------------------------
  ! Generic Coupler Component.
  !-----------------------------------------------------------------------------

  use ESMF_Mod
  use NUOPC

  implicit none
  
  private
  
  public SetServices
  
  type internalState
    type(ESMF_FieldBundle)  :: srcFields
    type(ESMF_FieldBundle)  :: dstFields
    type(ESMF_RouteHandle)  :: rh
  end type

  type internalStateWrap
    type(internalState), pointer :: wrap
  end type

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine SetServices(cplcomp, rc)
    type(ESMF_CplComp)   :: cplcomp
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_SETINIT, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_SETINIT, &
      userRoutine=InitializeP1, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_SETRUN, &
      userRoutine=Run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_SETFINAL, &
      userRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP0(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_StateType)                  :: isType, esType
    integer                               :: isItemCount, esItemCount
    type(ESMF_VM)                         :: vm
    
    rc = ESMF_SUCCESS
    
    ! get current VM because StateReconcile needs it
    !TODO: StateReconcile should have VM optional and this is obsolete
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! reconcile the States
    call ESMF_StateReconcile(importState, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_StateReconcile(exportState, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! access the state types
    call ESMF_StateGet(importState, statetype=isType, itemCount=isItemCount, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_StateGet(exportState, statetype=esType, itemCount=esItemCount, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    if (.not.((isType==ESMF_STATE_EXPORT).and.(esType==ESMF_STATE_IMPORT))) then
      ! not ES -> IS ==> should indicate problem???
    endif
    
    ! look for matching Fields and add them to the CPL component metadata
    call NUOPC_CplCompAttributeAdd(cplcomp, importState, exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP1(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_StateType)                  :: isType, esType
    integer                               :: isItemCount, esItemCount
    character(ESMF_MAXSTR)                :: cplList(100) !TODO make dynamic
    integer                               :: cplListSize, i, j
    character(ESMF_MAXSTR), pointer       :: importStdAttrNameList(:)
    character(ESMF_MAXSTR), pointer       :: importStdItemNameList(:)
    character(ESMF_MAXSTR), pointer       :: exportStdAttrNameList(:)
    character(ESMF_MAXSTR), pointer       :: exportStdItemNameList(:)
    integer                               :: iMatch, eMatch
    type(ESMF_Field)                      :: iField, eField
    type(ESMF_VM)                         :: vm
    integer                               :: stat
    type(internalStateWrap)               :: isw
    
    rc = ESMF_SUCCESS
    
    ! allocate memory for the internal state and set it in the Component
    allocate(isw%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal state memory failed.", &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out
    call ESMF_CplCompSetInternalState(cplcomp, isw, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! get current VM because StateReconcile needs it
    !TODO: StateReconcile should have VM optional and this is obsolete
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! reconcile the States
    call ESMF_StateReconcile(importState, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_StateReconcile(exportState, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! access the state types
    call ESMF_StateGet(importState, statetype=isType, itemCount=isItemCount, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_StateGet(exportState, statetype=esType, itemCount=esItemCount, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    if (.not.((isType==ESMF_STATE_EXPORT).and.(esType==ESMF_STATE_IMPORT))) then
      ! not ES -> IS ==> should indicate problem???
    endif
    
    ! get the cplList Attribute
    cplListSize=100
    call NUOPC_CplCompAttributeGet(cplcomp, cplList, cplListSize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! get the importState std lists
    call NUOPC_StateBuildStdList(importState, importStdAttrNameList, &
      importStdItemNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! get the exportState std lists
    call NUOPC_StateBuildStdList(exportState, exportStdAttrNameList, &
      exportStdItemNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! prepare FieldBundles to store src and dst Fields
    isw%wrap%srcFields = ESMF_FieldBundleCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    isw%wrap%dstFields = ESMF_FieldBundleCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    do i=3, cplListSize ! todo: starting at 3 because of Attribute workaround
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
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_StateGet(exportState, field=eField, &
          itemName=exportStdItemNameList(iMatch), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        
        ! add the import and export Fields to FieldBundles
        call ESMF_FieldBundleAdd(isw%wrap%srcFields, field=iField, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_FieldBundleAdd(isw%wrap%dstFields, field=eField, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
          
        ! set the connected Attribute on import Field
        call ESMF_AttributeSet(iField, &
          name="Connected", value="true", &
          convention="NUOPC", purpose="General", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        ! set the connected Attribute on export Field
        call ESMF_AttributeSet(eField, &
          name="Connected", value="true", &
          convention="NUOPC", purpose="General", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      else
        !TODO: Fields mentioned via stdname in Cpl metadata not found -> error?
      endif

    enddo
    
    ! precompute the regrid for all src to dst Fields
    call ESMF_FieldBundleRegridStore(isw%wrap%srcFields, isw%wrap%dstFields, &
      unmappedDstAction=ESMF_UNMAPPEDACTION_IGNORE, &
      routehandle=isw%wrap%rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

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
    type(internalStateWrap) :: isw
    type(ESMF_VM)           :: vm

    rc = ESMF_SUCCESS
    
    ! query Component for its internal State
    nullify(isw%wrap)
    call ESMF_CplCompGetInternalState(cplcomp, isw, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    !TODO: here may be the place to ensure incoming States are consistent
    !TODO: with the Fields held in the FieldBundle inside the internal State?
      
    ! execute the regrid operation
    call ESMF_FieldBundleRegrid(isw%wrap%srcFields, isw%wrap%dstFields, &
      routehandle=isw%wrap%rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! update the timestamp on all of the dst fields to that on the src side
    call NUOPC_FieldBundleUpdateTime(isw%wrap%srcFields, isw%wrap%dstFields, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! get current VM because AttributeUpdate needs it
    !TODO: AttributeUpdate should have VM optional and this is obsolete
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! ensure that Attributes are correctly updated across the exportState    
    !TODO: rootList should be that of owner of importState, because that is
    !TODO: the petList on which attributes (timestamp) have been set ->
    !TODO: not sure how to find this out here... -> for now use PET 0 and hope!
    call ESMF_AttributeUpdate(exportState, vm=vm, rootList=(/0/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine Finalize(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    integer                 :: stat
    type(internalStateWrap) :: isw

    rc = ESMF_SUCCESS
    
    ! query Component for its internal State
    nullify(isw%wrap)
    call ESMF_CplCompGetInternalState(cplcomp, isw, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! destroy the objects in the internal state
    call ESMF_FieldBundleRegridRelease(isw%wrap%rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldBundleDestroy(isw%wrap%srcFields, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldBundleDestroy(isw%wrap%dstFields, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! deallocate internal state memory
    deallocate(isw%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out
      
  end subroutine

end module
