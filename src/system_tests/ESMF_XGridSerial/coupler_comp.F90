! $Id: coupler_comp.F90,v 1.3 2010/09/16 17:07:55 feiliu Exp $
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

module coupler_comp

  ! ESMF Framework module
  use ESMF_Mod
  use ESMF_XGridMod
  use ESMF_XGridCreateMod
    
  implicit none
   
  public usercpl_setvm, usercpl_register
        
  ! global data
  type(ESMF_RouteHandle), save :: routehandle

  private
  type(ESMF_XGrid), save  :: xgrid
  type(ESMF_Field), save  :: field
  contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
  subroutine usercpl_setvm(comp, rc)
    type(ESMF_CplComp) :: comp
    integer, intent(out) :: rc
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
    call ESMF_VMGet(vm, pthreadsEnabledFlag=pthreadsEnabled, rc=rc)
    if (pthreadsEnabled) then
      call ESMF_CplCompSetVMMinThreads(comp, rc=rc)
    endif
#endif

  end subroutine

  subroutine usercpl_register(comp, rc)
    type(ESMF_CplComp) :: comp
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Coupler Register starting"
    
    ! Register the callback routines.
    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETINIT, userRoutine=user_init, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETRUN, userRoutine=user_run, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETFINAL, userRoutine=user_final, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print *, "Registered Initialize, Run, and Finalize routines"
    print *, "User Coupler Register returning"

  end subroutine

!-------------------------------------------------------------------------
!   !User Comp Component created by higher level calls, here is the
!   ! Initialization routine.
    
  subroutine user_init(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    integer :: itemcount
    type(ESMF_Field) :: F_lnd, F_ocn, F_atm
    type(ESMF_VM) :: vm
    type(ESMF_Grid)  :: lnd_grid, ocn_grid, atm_grid
    type(ESMF_XGridSpec) :: sparseMatA2X(2), sparseMatX2B(1)
    real(ESMF_KIND_R8)   :: centroid(12,2), area(12)
    real(ESMF_KIND_R8), pointer         :: xfptr(:)

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Coupler Init starting"

    !call ESMF_StateGet(importState, itemcount=itemcount, rc=rc)
    !if (rc/=ESMF_SUCCESS) return ! bail out
    !print *, "Import State contains ", itemcount, " items."

    ! Need to reconcile import and export states
    !call ESMF_CplCompGet(comp, vm=vm, rc=rc)
    !if (rc/=ESMF_SUCCESS) return ! bail out
    !call ESMF_StateReconcile(importState, vm, rc=rc)
    !if (rc/=ESMF_SUCCESS) return ! bail out
    !call ESMF_StateReconcile(exportState, vm, rc=rc)
    !if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_StatePrint(importState, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Get source Fields out of import state
    call ESMF_StateGet(importState, itemName="F_ocn", field=F_ocn, &
        nestedStateName="ocean export", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateGet(importState, itemName="F_lnd", field=F_lnd, &
        nestedStateName="land export", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Get destination Field out of export state
    call ESMF_StateGet(exportState, "F_atm", F_atm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Create an XGrid
    ! - grab grids
    call ESMF_FieldGet(F_lnd, grid=lnd_grid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldGet(F_ocn, grid=ocn_grid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldGet(F_atm, grid=atm_grid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Set up the sparsematrix matmul information

    allocate(sparseMatA2X(1)%factorIndexList(2,9), sparseMatA2X(1)%factorList(9))
    allocate(sparseMatA2X(2)%factorIndexList(2,3), sparseMatA2X(2)%factorList(3))
    allocate(sparseMatX2B(1)%factorIndexList(2,12), sparseMatX2B(1)%factorList(12))

    ! factorIndexList
    ! setting up mapping between A1 -> X
    sparseMatA2X(1)%factorIndexList(1,1)=1
    sparseMatA2X(1)%factorIndexList(1,2)=2
    sparseMatA2X(1)%factorIndexList(1,3)=2
    sparseMatA2X(1)%factorIndexList(1,4)=3
    sparseMatA2X(1)%factorIndexList(1,5)=4
    sparseMatA2X(1)%factorIndexList(1,6)=4
    sparseMatA2X(1)%factorIndexList(1,7)=3
    sparseMatA2X(1)%factorIndexList(1,8)=4
    sparseMatA2X(1)%factorIndexList(1,9)=4
    sparseMatA2X(1)%factorIndexList(2,1)=1
    sparseMatA2X(1)%factorIndexList(2,2)=2
    sparseMatA2X(1)%factorIndexList(2,3)=3
    sparseMatA2X(1)%factorIndexList(2,4)=4
    sparseMatA2X(1)%factorIndexList(2,5)=5
    sparseMatA2X(1)%factorIndexList(2,6)=6
    sparseMatA2X(1)%factorIndexList(2,7)=7
    sparseMatA2X(1)%factorIndexList(2,8)=8
    sparseMatA2X(1)%factorIndexList(2,9)=9
    ! setting up mapping between A2 -> X
    sparseMatA2X(2)%factorIndexList(1,1)=1
    sparseMatA2X(2)%factorIndexList(1,2)=2
    sparseMatA2X(2)%factorIndexList(1,3)=2
    sparseMatA2X(2)%factorIndexList(2,1)=10
    sparseMatA2X(2)%factorIndexList(2,2)=11
    sparseMatA2X(2)%factorIndexList(2,3)=12

    ! Note that the weights are dest area weighted
    ! factorList
    ! setting up mapping between A1 -> X
    sparseMatA2X(1)%factorList(1)=1
    sparseMatA2X(1)%factorList(2)=1
    sparseMatA2X(1)%factorList(3)=1
    sparseMatA2X(1)%factorList(4)=1
    sparseMatA2X(1)%factorList(5)=1
    sparseMatA2X(1)%factorList(6)=1
    sparseMatA2X(1)%factorList(7)=1
    sparseMatA2X(1)%factorList(8)=1
    sparseMatA2X(1)%factorList(9)=1
    ! setting up mapping between A2 -> X
    sparseMatA2X(2)%factorList(1)=1
    sparseMatA2X(2)%factorList(2)=1
    sparseMatA2X(2)%factorList(3)=1

    ! factorIndexList
    ! setting up mapping between X -> B
    sparseMatX2B(1)%factorIndexList(1,1)=1
    sparseMatX2B(1)%factorIndexList(1,2)=2
    sparseMatX2B(1)%factorIndexList(1,3)=3
    sparseMatX2B(1)%factorIndexList(1,4)=4
    sparseMatX2B(1)%factorIndexList(1,5)=5
    sparseMatX2B(1)%factorIndexList(1,6)=6
    sparseMatX2B(1)%factorIndexList(1,7)=7
    sparseMatX2B(1)%factorIndexList(1,8)=8
    sparseMatX2B(1)%factorIndexList(1,9)=9
    sparseMatX2B(1)%factorIndexList(1,10)=10
    sparseMatX2B(1)%factorIndexList(1,11)=11
    sparseMatX2B(1)%factorIndexList(1,12)=12
    sparseMatX2B(1)%factorIndexList(2,1)=1
    sparseMatX2B(1)%factorIndexList(2,2)=1
    sparseMatX2B(1)%factorIndexList(2,3)=2
    sparseMatX2B(1)%factorIndexList(2,4)=1
    sparseMatX2B(1)%factorIndexList(2,5)=1
    sparseMatX2B(1)%factorIndexList(2,6)=2
    sparseMatX2B(1)%factorIndexList(2,7)=3
    sparseMatX2B(1)%factorIndexList(2,8)=3
    sparseMatX2B(1)%factorIndexList(2,9)=4
    sparseMatX2B(1)%factorIndexList(2,10)=3
    sparseMatX2B(1)%factorIndexList(2,11)=3
    sparseMatX2B(1)%factorIndexList(2,12)=4

    ! factorList
    ! setting up mapping between X -> B
    sparseMatX2B(1)%factorList(1)=4./9
    sparseMatX2B(1)%factorList(2)=2./9
    sparseMatX2B(1)%factorList(3)=2./3
    sparseMatX2B(1)%factorList(4)=2./9
    sparseMatX2B(1)%factorList(5)=1./9
    sparseMatX2B(1)%factorList(6)=1./3
    sparseMatX2B(1)%factorList(7)=2./9
    sparseMatX2B(1)%factorList(8)=1./9
    sparseMatX2B(1)%factorList(9)=1./3
    sparseMatX2B(1)%factorList(10)=4./9
    sparseMatX2B(1)%factorList(11)=2./9
    sparseMatX2B(1)%factorList(12)=2./3

    ! Finally ready to do an flux exchange from A side to B side
    xgrid = ESMF_XGridCreate((/ocn_grid, lnd_grid/), (/atm_grid/), &
        area=area, centroid=centroid, &
        sparseMatA2X=sparseMatA2X, sparseMatX2B=sparseMatX2B, &
        rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    field = ESMF_FieldCreate(xgrid, typekind=ESMF_TYPEKIND_R8, rank=1, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldGet(field, farrayPtr=xfptr, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    xfptr = 0.0

    ! Precompute and store an FieldRegrid operation
    !call ESMF_FieldRegridStore(F_lnd, flux, xgrid, &
    !  routehandle=routehandle, rc=rc)
    !if (rc/=ESMF_SUCCESS) return ! bail out
    !call ESMF_FieldRegridStore(F_ocn, flux, xgrid, &
    !  routehandle=routehandle, rc=rc)
    !if (rc/=ESMF_SUCCESS) return ! bail out
    !call ESMF_FieldRegridStore(flux, F_atm, xgrid, &
    !  routehandle=routehandle, rc=rc)
    !if (rc/=ESMF_SUCCESS) return ! bail out
    
    deallocate(sparseMatA2X(1)%factorIndexList, sparseMatA2X(1)%factorList)
    deallocate(sparseMatA2X(2)%factorIndexList, sparseMatA2X(2)%factorList)
    deallocate(sparseMatX2B(1)%factorIndexList, sparseMatX2B(1)%factorList)

    print *, "User Coupler Init returning"
   
  end subroutine user_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is coupled.
!   !
 
  subroutine user_run(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_Field) :: F_lnd, F_ocn, F_atm
    type(ESMF_Grid)  :: lnd_grid, ocn_grid, atm_grid

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Coupler Run starting"

    ! Get source Fields out of import state
    call ESMF_StateGet(importState, itemName="F_ocn", field=F_ocn, &
        nestedStateName="ocean export", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateGet(importState, itemName="F_lnd", field=F_lnd, &
        nestedStateName="land export", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Get destination Field out of export state
    call ESMF_StateGet(exportState, "F_atm", F_atm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! compute regridding
    !call ESMF_FieldRegrid(F_lnd, flux, xgrid, &
    !  routehandle=routehandle, rc=rc)
    !if (rc/=ESMF_SUCCESS) return ! bail out
    !call ESMF_FieldRegrid(F_ocn, flux, xgrid, &
    !  routehandle=routehandle, rc=rc)
    !if (rc/=ESMF_SUCCESS) return ! bail out
    !call ESMF_FieldRegrid(flux, F_atm, xgrid, &
    !  routehandle=routehandle, rc=rc)
    !if (rc/=ESMF_SUCCESS) return ! bail out
  
    print *, "User Coupler Run returning"

  end subroutine user_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
  subroutine user_final(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Coupler Final starting"

    call ESMF_XGridDestroy(xgrid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_FieldDestroy(field, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
  
    ! Release resources stored for the ArrayRedist.
    !call ESMF_FieldRegridRelease(routehandle=routehandle, rc=rc)
    !if (rc/=ESMF_SUCCESS) return ! bail out

    print *, "User Coupler Final returning"
  
  end subroutine user_final


end module coupler_comp
    
!\end{verbatim}

