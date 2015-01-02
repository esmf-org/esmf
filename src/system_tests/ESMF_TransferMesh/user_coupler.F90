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
  
  ! In phase 1 Initialize, CplComp reconciles the objects that are contained
  ! in the importState (from Comp1) and the exportState (from Comp2). From the
  ! source side (Comp1) CplComp accesses the index space information of the
  ! Mesh that is being shared, i.e. the DistGrid. On the destination side
  ! (Comp2) CplComp accesses the VM on which the destination Field is defined.
  !
  !   The collected information is used to create the destination DistGrid from
  ! the shared source DistGrid, considering the destination VM. A destination
  ! Mesh is created on top of this DistGrid, again considering Comp2's VM. 
  ! Finally the destination Mesh is set on the destination Field, making it
  ! available to Comp2.
  
  subroutine user_initP1(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp)    :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    type(ESMF_Field)    :: srcField, dstField
    type(ESMF_Mesh)     :: srcMesh, dstMesh
    type(ESMF_DistGrid) :: srcElementDistGrid, dstElementDistGrid
    type(ESMF_DistGrid) :: srcNodeDistGrid, dstNodeDistGrid
    type(ESMF_VM)       :: dstVM

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Coupler Init phase=1 starting"

    ! Need to reconcile import and export states
    call ESMF_StateReconcile(importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out
    call ESMF_StateReconcile(exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out

    ! Get source Field out of import state
    call ESMF_StateGet(importState, "srcField", srcField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out

    ! Get the source side Mesh and DistGrid
    call ESMF_FieldGet(srcField, mesh=srcMesh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out
    call ESMF_MeshGet(srcMesh, elementDistgrid=srcElementDistGrid, &
      nodalDistgrid=srcNodeDistGrid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out
    
    ! Get destination Field out of export state
    call ESMF_StateGet(exportState, "dstField", dstField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out
    
    ! Get the destination side VM
    call ESMF_FieldGet(dstField, vm=dstVM, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out

    ! Create the dstDistGrid from the srcDistGrid, but on dstVM
    ! The dstVM is important in order to specify which is the correct context
    ! of the dstDistGrid. The created dstDistGrid is only fully functional on 
    ! the PETs that are part of dstVM, other PETs must first generate proxies
    ! via a StateReconcile. Before reconciliation care must be given to which
    ! calls the dstDistGrid is passed (generally they must also take the dstVM).
    dstElementDistGrid = ESMF_DistGridCreate(srcElementDistGrid, vm=dstVM, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out
    dstNodeDistGrid = ESMF_DistGridCreate(srcNodeDistGrid, vm=dstVM, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out

    ! Create the dstMesh that allows dstElementDistGrid and dstNodeDistGrid to
    ! be taken to Comp2. The method that is chosen to create dstMesh really 
    ! only creates a formal Mesh for the purpose of being a container for
    ! the two DistGrid objects, while also identifying itself as a geom object
    ! of type Mesh. It is not a fully usable Mehs!
    dstMesh = ESMF_MeshCreate(dstElementDistGrid, &
      nodalDistgrid=dstNodeDistGrid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out
    
    ! Set the new dstMesh in the dstField in order to hand it to Comp2.
    call ESMF_FieldEmptySet(dstField, mesh=dstMesh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out

    print *, "User Coupler Init phase=1 returning"
   
  end subroutine user_initP1

!-------------------------------------------------------------------------
  
  ! In phase 2 Initialize, CplComp the importState and exportState need to be
  ! re-reconciled because information may likely have changed (e.g. the 
  ! DistGrid information provided in the Field from Comp2).
  !
  !   The purpose of this phase is to create a new dstMesh which is a full
  ! representation of srcMesh, but on the dstVM, and using the dstDistGrid
  ! as supplied by Comp2. After this method Comp2 has a complete representation
  ! of the srcMesh that Comp1 shared.
  
  subroutine user_initP2(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp)    :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    type(ESMF_Field)    :: srcField, dstField
    type(ESMF_Mesh)     :: srcMesh, dstMesh
    type(ESMF_DistGrid) :: dstDistGrid

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Coupler Init phase=2 starting"

    ! Need to re-reconcile import and export states
    call ESMF_StateReconcile(importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out
    call ESMF_StateReconcile(exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out

    ! Access source Field and Mesh
    call ESMF_StateGet(importState, "srcField", srcField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out
    call ESMF_FieldGet(srcField, mesh=srcMesh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out
    
    ! Access destination Field, Mesh, and DistGrid
    call ESMF_StateGet(exportState, "dstField", dstField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out
    call ESMF_FieldGet(dstField, mesh=dstMesh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out
    call ESMF_MeshGet(dstMesh, elementDistgrid=dstDistGrid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out

    !call ESMF_DistGridPrint(dstDistGrid, rc=rc)
    !if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Create a new dstMesh from srcMesh, considering dstDistGrid
    ! Now that dstDistGrid is reconciled, it can be used without the need
    ! for passing in the dstVM argument. The resulting dstMesh will function
    ! correctly in the CplComp context, as well as the Comp2 context.
    dstMesh = ESMF_MeshCreate(srcMesh, elementDistgrid=dstDistGrid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out
    
    ! Set the new dstMesh in the dstField to make it available to Comp2
    call ESMF_FieldEmptySet(dstField, mesh=dstMesh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out
        
    print *, "User Coupler Init phase=2 returning"
   
  end subroutine user_initP2

!-------------------------------------------------------------------------
  
  ! In phase 3 Initialize, CplComp stores a Redist operation for
  ! srcField --to--> dstField remapping. A Redist works because the Fields
  ! on both sides are defined on the same Mesh (just distributed differently).
  ! In order to test that also the Mesh coordinates were shared (and 
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

    ! Re-reconcile import and export states, just in case something changed
    call ESMF_StateReconcile(importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out
    call ESMF_StateReconcile(exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out

    ! Access source, final, and destination Fields
    call ESMF_StateGet(importState, "srcField", srcField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out
    call ESMF_StateGet(importState, "finalField", finalField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out
    call ESMF_StateGet(exportState, "dstField", dstField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out
    
    ! Precompute the Redist operation, give it a name and keep it in importState
    call ESMF_FieldRedistStore(srcField, dstField, routehandle=rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out
    call ESMF_RouteHandleSet(rh, name="Redist", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out
    call ESMF_StateAdd(importState, (/rh/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out
        
    ! Precompute the Regrid operation, give it a name and keep it in importState
    call ESMF_FieldRegridStore(dstField, finalField, routehandle=rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out
    call ESMF_RouteHandleSet(rh, name="Regrid", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out
    call ESMF_StateAdd(importState, (/rh/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out
        
    print *, "User Coupler Init phase=3 returning"
   
  end subroutine user_initP3

!-------------------------------------------------------------------------
 
  ! Execute the precomputed Redist and Regrid operations.
  
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
  
    ! Access source, final, and destination Fields
    call ESMF_StateGet(importState, "srcField", srcField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out
    call ESMF_StateGet(importState, "finalField", finalField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out
    call ESMF_StateGet(exportState, "dstField", dstField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out
    
    ! Access the Redist RouteHandle that was kept in the importState
    call ESMF_StateGet(importState, "Redist", rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out
    
    ! Execute the Redist operation: srcField -> dstField
    call ESMF_FieldRedist(srcField, dstField, routehandle=rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out
    
    ! Access the Regrid RouteHandle that was kept in the importState
    call ESMF_StateGet(importState, "Regrid", rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out
    
    ! Execute the Redist operation: dstField -> finalField
    call ESMF_FieldRedist(dstField, finalField, routehandle=rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return ! bail out
    
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

