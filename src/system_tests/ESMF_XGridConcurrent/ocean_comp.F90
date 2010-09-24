! $Id: ocean_comp.F90,v 1.2 2010/09/24 13:53:56 w6ws Exp $
!
! Example/test code which shows User Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!
! !DESCRIPTION:
!  Ocean component with a single Field exported
!
!
!\begin{verbatim}

module ocean_comp

  ! ESMF Framework module
  use ESMF_Mod

  implicit none
    
  public ocean_setvm, ocean_register
        
  contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
  subroutine ocean_setvm(comp, rc)
    type(ESMF_GridComp) :: comp
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
      call ESMF_GridCompSetVMMinThreads(comp, rc=rc)
    endif
#endif

  end subroutine

  subroutine ocean_register(comp, rc)
    type(ESMF_GridComp) :: comp
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "Ocean Register starting"

    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, userRoutine=ocean_init, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, userRoutine=ocean_run, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, userRoutine=ocean_final, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print *, "Registered Initialize, Run, and Finalize routines"
    print *, "Ocean Register returning"
    
  end subroutine

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
  subroutine ocean_init(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_ArraySpec)  :: arrayspec
    type(ESMF_DistGrid)   :: distgrid
    type(ESMF_Grid)       :: grid
    type(ESMF_Field)      :: field
    type(ESMF_VM)         :: vm
    integer               :: petCount, me
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    call ESMF_VMGetGlobal(vm, rc=rc)
    call ESMF_VMGet (vm, localPet=me)
    print *, "Ocean Init starting, myPet =", me

    ! Determine petCount
    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMGet(vm, petCount=petCount, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Create the source Field and add it to the export State
    call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/2,2/), &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    grid = ESMF_GridCreate(distgrid=distgrid, destroyDistGrid=.true., &
        indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    field = ESMF_FieldCreate(arrayspec=arrayspec, grid=grid, &
      indexflag=ESMF_INDEX_GLOBAL, name="F_ocn", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateAdd(exportState, field, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
   
    print *, "ocean Init returning"

  end subroutine ocean_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
  subroutine ocean_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    real(ESMF_KIND_R8)    :: pi
    type(ESMF_Field)      :: field
    real(ESMF_KIND_R8), pointer :: farrayPtr(:,:)   ! matching F90 array pointer
    integer               :: i, j
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "Ocean Run starting"

    ! Get the source Field from the export State
    call ESMF_StateGet(exportState, "F_ocn", field, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Gain access to actual data via F90 array pointer
    call ESMF_FieldGet(field, localDe=0, farrayPtr=farrayPtr, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Fill source Field with data
    farrayPtr = 0.0
 
    print *, "Ocean Run returning"

  end subroutine ocean_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
  subroutine ocean_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_Grid) :: grid
    type(ESMF_Field) :: field
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "Ocean Final starting"

    call ESMF_StateGet(exportState, "F_ocn", field, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldGet(field, grid=grid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldDestroy(field, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridDestroy(grid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print *, "Ocean Final returning"

  end subroutine ocean_final


end module ocean_comp
    
!\end{verbatim}
