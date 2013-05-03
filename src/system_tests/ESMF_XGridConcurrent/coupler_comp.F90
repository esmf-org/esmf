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

module coupler_comp

  ! ESMF Framework module
  use ESMF
    
  implicit none
  private
  public usercpl_setvm, usercpl_register

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
    call ESMF_CplCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, userRoutine=user_init, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_CplCompSetEntryPoint(comp, ESMF_METHOD_RUN, userRoutine=user_run, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_CplCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, userRoutine=user_final, &
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
    type(ESMF_Field) :: F_lnd, F_atm
    type(ESMF_VM) :: vm
    type(ESMF_Grid)  :: lnd_grid, atm_grid
    type(ESMF_XGrid) :: xgrid
    type(ESMF_Field) :: flux
    type(ESMF_RouteHandle) :: rh1, rh2, rh3
    real(ESMF_KIND_R8), pointer         :: xfptr(:)
    integer :: localPet
    integer              :: cnt

    ! Initialize return code
    rc = ESMF_SUCCESS

    !call ESMF_StateGet(importState, itemcount=itemcount, rc=rc)
    !if (rc/=ESMF_SUCCESS) return ! bail out
    !print *, "Import State contains ", itemcount, " items."

    call ESMF_CplCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMGet (vm, localPet=localPet)
    print *, "User Coupler Init starting, localPet =", localPet

    ! Need to reconcile import and export states
    call ESMF_StateReconcile(importState, vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateReconcile(exportState, vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_StatePrint(importState, nestedFlag=.true., rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Get destination Field out of export state
    call ESMF_StateGet(exportState, "F_atm", F_atm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Get source Fields out of import state
    call ESMF_StateGet(importState, itemName="F_lnd", field=F_lnd, &
        rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Create an XGrid
    ! - grab grids
    call ESMF_FieldGet(F_lnd, grid=lnd_grid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldGet(F_atm, grid=atm_grid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Finally ready to do an flux exchange from A side to B side
    xgrid = ESMF_XGridCreate(sideAGrid=(/lnd_grid/), sideBGrid=(/atm_grid/), &
        sideAMaskValues=(/2,3,4/), &
        rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    flux = ESMF_FieldCreate(xgrid, typekind=ESMF_TYPEKIND_R8, name='xgrid_flux', rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldGet(flux, farrayPtr=xfptr, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    xfptr = 0.0

    ! Precompute and store an FieldRegrid operation
    call ESMF_FieldRegridStore(xgrid, F_lnd, flux, &
      routehandle=rh1, rc=rc)
    call ESMF_RoutehandleSet(rh1, name='lnd2xgrid', rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_FieldRegridStore(xgrid, flux, F_atm, &
      routehandle=rh3, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_RoutehandleSet(rh3, name='xgrid2atm', rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_StateAdd(exportState, (/rh1,rh3/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateAdd(exportState, (/flux/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
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
    type(ESMF_Field) :: F_lnd, F_atm, flux
    type(ESMF_Grid)  :: lnd_grid, atm_grid
    type(ESMF_RouteHandle) :: rh1, rh2, rh3

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Coupler Run starting"

    ! Get source Fields out of import state
    call ESMF_StateGet(importState, itemName="F_lnd", field=F_lnd, &
        rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Get destination Field out of export state
    call ESMF_StateGet(exportState, "F_atm", F_atm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateGet(exportState, "xgrid_flux", flux, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_StateGet(exportState, "lnd2xgrid", routehandle=rh1, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateGet(exportState, "xgrid2atm", routehandle=rh3, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! compute regridding
    call ESMF_FieldRegrid(F_lnd, flux, routehandle=rh1, zeroregion=ESMF_REGION_EMPTY, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldRegrid(flux, F_atm, routehandle=rh3, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
  
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

    type(ESMF_Field) :: flux
    type(ESMF_XGrid) :: xgrid
    type(ESMF_RouteHandle) :: rh1, rh2, rh3

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Coupler Final starting"

    call ESMF_StateGet(exportState, "xgrid_flux", flux, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateGet(exportState, "lnd2xgrid", rh1, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateGet(exportState, "xgrid2atm", rh3, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_FieldGet(flux, xgrid=xgrid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_XGridDestroy(xgrid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_FieldDestroy(flux, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
  
    ! Release resources stored for the ArrayRedist.
    call ESMF_FieldRegridRelease(routehandle=rh1, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldRegridRelease(routehandle=rh3, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print *, "User Coupler Final returning"
  
  end subroutine user_final


end module coupler_comp
    
!\end{verbatim}

