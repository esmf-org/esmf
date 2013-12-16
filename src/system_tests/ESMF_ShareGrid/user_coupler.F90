! $Id$
!
! Example/test code which shows User Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!
! !DESCRIPTION:
!  User-supplied Coupler
!
!
!\begin{verbatim}

module user_coupler

  ! ESMF Framework module
  use ESMF
    
  implicit none
   
  public usercpl_setvm, usercpl_register
        
  ! global data
  type(ESMF_RouteHandle), save :: routehandle

  contains

!-------------------------------------------------------------------------
 
  subroutine usercpl_setvm(comp, rc)
    type(ESMF_CplComp)    :: comp
    integer, intent(out)  :: rc
#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    logical :: pthreadsEnabled
#endif

    ! Initialize return code
    rc = ESMF_SUCCESS

#ifdef ESMF_TESTWITHTHREADS
    ! The following call will turn on ESMF-threading (single threaded)
    ! for this component. If you are using this file as a template for
    ! your own code development you probably don't want to include the
    ! following call unless you are interested in exploring ESMF's
    ! threading features.

    ! First test whether ESMF-threading is supported on this machine
    call ESMF_VMGetGlobal(vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMGet(vm, pthreadsEnabledFlag=pthreadsEnabled, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    if (pthreadsEnabled) then
      call ESMF_CplCompSetVMMinThreads(comp, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
    endif
#endif

  end subroutine

!--------------------------------------------------------------------------------

  subroutine usercpl_register(comp, rc)
    type(ESMF_CplComp)    :: comp
    integer, intent(out)  :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Coupler Register starting"
    
    ! Register the callback routines.
    call ESMF_CplCompSetEntryPoint(comp, methodflag=ESMF_METHOD_INITIALIZE, &
      userRoutine=user_initP1, phase=1, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_CplCompSetEntryPoint(comp, methodflag=ESMF_METHOD_INITIALIZE, &
      userRoutine=user_initP2, phase=2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_CplCompSetEntryPoint(comp, methodflag=ESMF_METHOD_INITIALIZE, &
      userRoutine=user_initP3, phase=3, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_CplCompSetEntryPoint(comp, methodflag=ESMF_METHOD_RUN, &
      userRoutine=user_run, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_CplCompSetEntryPoint(comp, methodflag=ESMF_METHOD_FINALIZE, &
      userRoutine=user_final, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print *, "Registered Initialize, Run, and Finalize routines"
    print *, "User Coupler Register returning"

  end subroutine

!-------------------------------------------------------------------------
  
  ! In phase 1 Initialize CplComp reconciles the objects that are contained
  ! in the importState (from Comp1) and the exportState (from Comp2). From the
  ! source side (Comp1) CplComp accesses the index space information of the
  ! Grid that is being shared, i.e. the DistGrid. On the destination side
  ! (Comp2) CplComp accesses the VM on which the destination Field is defined.
  !
  !   The collected information is used to create the destination DistGrid from
  ! the shared source DistGrid, considering the destination VM. A destination
  ! Grid is created on top of this DistGrid, again considering Comp2's VM. 
  ! Finally the destination Grid is set on the destination Field, making it
  ! available to Comp2.
  
  subroutine user_initP1(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp)    :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    type(ESMF_Field)    :: srcField, dstField
    type(ESMF_Grid)     :: srcGrid, dstGrid
    type(ESMF_DistGrid) :: srcDistGrid, dstDistGrid
    type(ESMF_VM)       :: dstVM

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Coupler Init phase=1 starting"

    ! Need to reconcile import and export states
    call ESMF_StateReconcile(importState, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateReconcile(exportState, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Get source Field out of import state
    call ESMF_StateGet(importState, "srcField", srcField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Get the source side Grid and DistGrid
    call ESMF_FieldGet(srcField, grid=srcGrid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridGet(srcGrid, distgrid=srcDistGrid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Get destination Field out of export state
    call ESMF_StateGet(exportState, "dstField", dstField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Get the destination side VM
    call ESMF_FieldGet(dstField, vm=dstVM, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Create the dstDistGrid from the srcDistGrid, but on dstVM
    dstDistGrid = ESMF_DistGridCreate(srcDistGrid, vm=dstVM, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Create the dstGrid on the dstDistGrid on dstVM that contains shared info
    dstGrid = ESMF_GridCreate(dstDistGrid, vm=dstVM, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Set the new dstGrid in the dstField
    call ESMF_FieldEmptySet(dstField, grid=dstGrid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print *, "User Coupler Init phase=1 returning"
   
  end subroutine user_initP1

!-------------------------------------------------------------------------
  
  ! In phase 2 Initialize CplComp the importState and exportState need to be
  ! re-reconciled because information may likely have changed (e.g. the 
  ! DistGrid information provided in the Field from Comp2).
  !
  !   The purpose of this phase is to create a new dstGrid which is a full
  ! representation of srcGrid, but on the dstVM, and using the dstDistGrid
  ! as supplied by Comp2. After this method Comp2 has a complete representation
  ! of the srcGrid that Comp1 shared.
  
  subroutine user_initP2(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp)    :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    type(ESMF_Field)    :: srcField, dstField
    type(ESMF_Grid)     :: srcGrid, dstGrid
    type(ESMF_VM)       :: dstVM

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Coupler Init phase=2 starting"

    ! Need to re-reconcile import and export states
    call ESMF_StateReconcile(importState, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateReconcile(exportState, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Access source Field and Grid
    call ESMF_StateGet(importState, "srcField", srcField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldGet(srcField, grid=srcGrid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Access destination Field, Grid, and VM
    call ESMF_StateGet(exportState, "dstField", dstField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldGet(dstField, grid=dstGrid, vm=dstVM, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! TODO: Here we would want to call something like a GridCreateFromGrid(),
    ! TODO: that considers the dstVM. All of the information, including 
    ! TODO: redistributed physical coordinate information that is available
    ! TODO: on srcGrid should be transferred to the dstGrid. This new dstGrid
    ! TODO: then would be set in the dstField, and thus all srcGrid info is
    ! TODO: made available to Comp2 on its distribution.
        
    print *, "User Coupler Init phase=2 returning"
   
  end subroutine user_initP2

!-------------------------------------------------------------------------
  
  ! In phase 3 Initialize CplComp simply stores a Redist operation between
  ! srcField and dstField. Redist will do, i.e. no Regrid required, because
  ! the Fields on both sides are on the same shared Grid, just that it is
  ! potentially decomposed, and likely distributed differently.
  
  subroutine user_initP3(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp)    :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    type(ESMF_Field)        :: srcField, dstField
    type(ESMF_RouteHandle)  :: rh

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Coupler Init phase=3 starting"

    ! Re-reconcile import and export states, just in case something changed
    call ESMF_StateReconcile(importState, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateReconcile(exportState, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Access source and destination Fields
    call ESMF_StateGet(importState, "srcField", srcField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateGet(exportState, "dstField", dstField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Precompute the Redist operation
    call ESMF_FieldRedistStore(srcField, dstField, routehandle=rh, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Keep the Redist RouteHandle in the importState
    call ESMF_RouteHandleSet(rh, name="RH", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateAdd(importState, (/rh/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
        
    print *, "User Coupler Init phase=3 returning"
   
  end subroutine user_initP3

!-------------------------------------------------------------------------
 
  ! Execute the precomputed Redist operation.
  
  subroutine user_run(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp)    :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    type(ESMF_Field)        :: srcField, dstField
    type(ESMF_RouteHandle)  :: rh

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Coupler Run starting"
  
    ! Access source and destination Fields
    call ESMF_StateGet(importState, "srcField", srcField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateGet(exportState, "dstField", dstField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Access the RouteHandle that was kept in the importState
    call ESMF_StateGet(importState, "RH", rh, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Execute the Redist operation
    call ESMF_FieldRedist(srcField, dstField, routehandle=rh, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    print *, "User Coupler Run returning"

  end subroutine user_run

!-------------------------------------------------------------------------
 
  subroutine user_final(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp)    :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Coupler Final starting"

    print *, "User Coupler Final returning"
  
  end subroutine user_final

!--------------------------------------------------------------------------------

end module user_coupler
    
!\end{verbatim}

