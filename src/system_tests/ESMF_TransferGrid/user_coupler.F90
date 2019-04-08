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

!-------------------------------------------------------------------------------

  subroutine usercpl_register(comp, rc)
    type(ESMF_CplComp)    :: comp
    integer, intent(out)  :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Coupler Register starting"
    call ESMF_LogWrite (msg='User Coupler starting', &
      logmsgFlag=ESMF_LOGMSG_TRACE)

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
    call ESMF_LogWrite (msg='User Coupler Register complete', &
      logmsgFlag=ESMF_LOGMSG_TRACE)
    print *, "User Coupler Register returning"

  end subroutine

!-------------------------------------------------------------------------
  
  ! In phase 1 Initialize, CplComp reconciles the objects that are contained
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
    call ESMF_LogWrite (msg='User Coupler Init phase=1 starting', &
      logmsgFlag=ESMF_LOGMSG_TRACE)

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
    ! The dstVM is important in order to specify which is the correct context
    ! of the dstDistGrid. The created dstDistGrid is only fully functional on 
    ! the PETs that are part of dstVM, other PETs must first generate proxies
    ! via a StateReconcile. Before reconciliation care must be given to which
    ! calls the dstDistGrid is passed (generally they must also take the dstVM).
    dstDistGrid = ESMF_DistGridCreate(srcDistGrid, vm=dstVM, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Create the dstGrid on the dstDistGrid limited to PETs in dstVM
    ! The dstGrid is used as a vehicle to hand the dstDistGrid to the Comp2.
    ! It must be created on the dstVM because only there the dstDistGrid
    ! currently exists.
    dstGrid = ESMF_GridCreate(dstDistGrid, vm=dstVM, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Set the new dstGrid in the dstField
    ! The dstGrid with dstDistGrid is handed to Comp2 inside the dstField.
    call ESMF_FieldEmptySet(dstField, grid=dstGrid, vm=dstVM, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_LogWrite (msg='User Coupler Init phase=1 complete', &
      logmsgFlag=ESMF_LOGMSG_TRACE)
    print *, "User Coupler Init phase=1 returning"
   
  end subroutine user_initP1

!-------------------------------------------------------------------------
  
  ! In phase 2 Initialize, CplComp the importState and exportState need to be
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
    type(ESMF_DistGrid) :: dstDistGrid

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Coupler Init phase=2 starting"
    call ESMF_LogWrite (msg='User Coupler Init phase=2 starting', &
      logmsgFlag=ESMF_LOGMSG_TRACE)

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
    
    ! Access destination Field, Grid, and DistGrid
    call ESMF_StateGet(exportState, "dstField", dstField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldGet(dstField, grid=dstGrid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridGet(dstGrid, distgrid=dstDistGrid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Create a new dstGrid from srcGrid, considering dstDistGrid
    ! Now that dstDistGrid is reconciled, it can be used without the need
    ! for passing in the dstVM argument. The resulting dstGrid will function
    ! correctly in the CplComp context, as well as the Comp2 context.
    dstGrid = ESMF_GridCreate(srcGrid, dstDistGrid,rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Set the new dstGrid in the dstField to make it available to Comp2
    call ESMF_FieldEmptySet(dstField, grid=dstGrid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
        
    call ESMF_LogWrite (msg='User Coupler Init phase=2 complete', &
      logmsgFlag=ESMF_LOGMSG_TRACE)
    print *, "User Coupler Init phase=2 returning"
   
  end subroutine user_initP2

!-------------------------------------------------------------------------
  
  ! In phase 3 Initialize, CplComp stores a Redist operation for
  ! srcField --to--> dstField remapping. A Redist works because the Fields
  ! on both sides are defined on the same Grid (just distributed differently).
  ! In order to test that also the Grid coordinates were shared (and 
  ! redistributed) correctly, a Regrid operation is precomputed for 
  ! dstField --to--> finalField remapping, i.e. from Comp2 back to Comp1.
  
  subroutine user_initP3(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp)    :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    type(ESMF_Field)        :: srcField, dstField, finalField
    type(ESMF_RouteHandle)  :: rh

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Coupler Init phase=3 starting"
    call ESMF_LogWrite (msg='User Coupler Init phase=3 starting', &
      logmsgFlag=ESMF_LOGMSG_TRACE)

    ! Re-reconcile import and export states, just in case something changed
    call ESMF_LogWrite (msg='User Coupler Init phase=3 Reconcile importState',  &
        logmsgFlag=ESMF_LOGMSG_TRACE)
    call ESMF_StateReconcile(importState, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_LogWrite (msg='User Coupler Init phase=3 Reconcile exportState',  &
        logmsgFlag=ESMF_LOGMSG_TRACE)
    call ESMF_StateReconcile(exportState, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Access source, final, and destination Fields
    call ESMF_LogWrite (msg='User Coupler Init phase=3 access Fields',  &
        logmsgFlag=ESMF_LOGMSG_TRACE)
    call ESMF_StateGet(importState, "srcField", srcField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateGet(importState, "finalField", finalField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateGet(exportState, "dstField", dstField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Precompute the Redist operation, give it a name and keep it in importState
    call ESMF_LogWrite (msg='User Coupler Init phase=3 FieldRegridStore srcField->dstField',  &
        logmsgFlag=ESMF_LOGMSG_TRACE)
    call ESMF_FieldRedistStore(srcField, dstField, routehandle=rh, rc=rc)
    call ESMF_LogWrite (msg='User Coupler Init phase=3 FieldRegridStore srcField->dstField complete',  &
        logmsgFlag=ESMF_LOGMSG_TRACE)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_RouteHandleSet(rh, name="Redist", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateAdd(importState, (/rh/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
        
    ! Precompute the Regrid operation, give it a name and keep it in importState
    call ESMF_LogWrite (msg='User Coupler Init phase=3 FieldRegridStore dstField->finalField',  &
        logmsgFlag=ESMF_LOGMSG_TRACE)
    call ESMF_FieldRegridStore(dstField, finalField, routehandle=rh, rc=rc)
    call ESMF_LogWrite (msg='User Coupler Init phase=3 FieldRegridStore dstField->finalField complete',  &
        logmsgFlag=ESMF_LOGMSG_TRACE)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_RouteHandleSet(rh, name="Regrid", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateAdd(importState, (/rh/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
        
    call ESMF_LogWrite (msg='User Coupler Init phase=3 complete', &
      logmsgFlag=ESMF_LOGMSG_TRACE)
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
    type(ESMF_Field)        :: srcField, dstField, finalField
    type(ESMF_RouteHandle)  :: rh

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Coupler Run starting"
    call ESMF_LogWrite (msg='User Coupler Run starting', &
      logmsgFlag=ESMF_LOGMSG_TRACE)

    ! Access source, final, and destination Fields
    call ESMF_StateGet(importState, "srcField", srcField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateGet(importState, "finalField", finalField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateGet(exportState, "dstField", dstField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Access the Redist RouteHandle that was kept in the importState
    call ESMF_StateGet(importState, "Redist", rh, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Execute the Redist operation: srcField -> dstField
    call ESMF_FieldRedist(srcField, dstField, routehandle=rh, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Access the Regrid RouteHandle that was kept in the importState
    call ESMF_StateGet(importState, "Regrid", rh, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Execute the Redist operation: dstField -> finalField
    call ESMF_FieldRedist(dstField, finalField, routehandle=rh, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    call ESMF_LogWrite (msg='User Coupler Run complete', &
      logmsgFlag=ESMF_LOGMSG_TRACE)
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
    call ESMF_LogWrite (msg='User Coupler Final start/complete', &
      logmsgFlag=ESMF_LOGMSG_TRACE)
    print *, "User Coupler Final returning"
  
  end subroutine user_final

!-------------------------------------------------------------------------------

end module user_coupler
    
!\end{verbatim}

