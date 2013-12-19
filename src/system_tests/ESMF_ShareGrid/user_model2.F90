! $Id$
!
! Example/test code which shows User Component calls.

!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

!
! !DESCRIPTION:
!  User-supplied Component
!
!
!\begin{verbatim}

module user_model2

  ! ESMF Framework module
  use ESMF

  implicit none
    
  public userm2_setvm, userm2_register
        
  contains

!--------------------------------------------------------------------------------
 
  subroutine userm2_setvm(comp, rc)
    type(ESMF_GridComp)   :: comp
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
      call ESMF_GridCompSetVMMinThreads(comp, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
    endif
#endif

  end subroutine

!-------------------------------------------------------------------------

  subroutine userm2_register(comp, rc)
    type(ESMF_GridComp)   :: comp
    integer, intent(out)  :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp2 Register starting"

    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, methodflag=ESMF_METHOD_INITIALIZE, &
      userRoutine=user_initP1, phase=1, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, methodflag=ESMF_METHOD_INITIALIZE, &
      userRoutine=user_initP2, phase=2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, methodflag=ESMF_METHOD_INITIALIZE, &
      userRoutine=user_initP3, phase=3, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, methodflag=ESMF_METHOD_RUN, &
      userRoutine=user_run, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, methodflag=ESMF_METHOD_FINALIZE, &
      userRoutine=user_final, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print *, "Registered Initialize, Run, and Finalize routines"
    print *, "User Comp2 Register returning"
    
  end subroutine

!--------------------------------------------------------------------------------
 
  ! In phase 1 Initialize, Comp2 creates an empty Field without any Grid 
  ! information. The Field is added to the importState.
    
  subroutine user_initP1(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    type(ESMF_Field)      :: field
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp2 Init phase=1 starting"

    ! Create the destination Field and add it to the import State
    field = ESMF_FieldEmptyCreate(name="dstField", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateAdd(importState, (/field/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
   
    print *, "User Comp2 Init phase=1 returning"

  end subroutine user_initP1

!--------------------------------------------------------------------------------
 
  ! In phase 2 Initialize, Comp2 the incoming Field now contains Grid information
  ! that has been shared by Comp1 through the CplComp. The Grid contains
  ! index space information, i.e. DistGrid, which will be printed here.
  !
  !   When the CplComp shares Comp1 Grid information with Comp2, it does not
  ! change the Grid decomposition, i.e. the deCount of the Grid. However, since
  ! the number of PETs in Comp2 may be different from Comp1, it is not unlikely
  ! to find the Grid having multiple DEs per PET on Comp2, or PETs that do not
  ! have any local DEs.
  !
  !   In a real world application, this is the place where Comp2 could modify 
  ! the index space information that it received from Comp1. The most frequent
  ! modification would likely be the decomposition of the Grid as to guarantee
  ! a sensible distribution across the PETs available on Comp2.
  !
  !   The Grid that is attached to the Field in the importState when leaving
  ! this method will be used by the CplComp to redistribute the entire Grid
  ! that is shared by Comp1.
    
  subroutine user_initP2(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    type(ESMF_Field)      :: field
    type(ESMF_Grid)       :: grid
    type(ESMF_DistGrid)   :: distgrid
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp2 Init phase=2 starting"

    ! Access Field with shared Grid that is available in the importState
    call ESMF_StateGet(importState, "dstField", field, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Get the shared Grid
    call ESMF_FieldGet(field, grid=grid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
 
    ! Get the DistGrid
    call ESMF_GridGet(grid, distgrid=distgrid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Print the DistGrid of the shared Grid
    call ESMF_DistGridPrint(distgrid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! TODO: If there were changes made to the DistGrid, then the Grid will need
    ! TODO: to be re-created on the changed DistGrid, and swapped out in the 
    ! TODO: Field.
   
    print *, "User Comp2 Init phase=2 returning"

  end subroutine user_initP2

!--------------------------------------------------------------------------------
 
  ! In phase 3 Initialize, Comp2 finishes the creation of the Field on the 
  ! shared Grid. The incoming Field already holds the complete shared Grid,
  ! with correctly distributed coordinate arrays. All that is left to do here
  ! is to call FieldEmptyComplete.
    
  subroutine user_initP3(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    type(ESMF_Field)      :: field

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp2 Init phase=3 starting"

    ! Access Field with shared Grid that is available in the importState
    call ESMF_StateGet(importState, "dstField", field, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Allocate memory for data by finishing the creation of the Field
    call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
   
    print *, "User Comp2 Init phase=3 returning"

  end subroutine user_initP3

!--------------------------------------------------------------------------------
 
  ! Print the Field
  
  subroutine user_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    type(ESMF_Field)      :: field
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp2 Run starting"
    
    ! Access the Field
    call ESMF_StateGet(importState, "dstField", field, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Print the Field
    call ESMF_FieldPrint(field, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    print *, "User Comp2 Run returning"

  end subroutine user_run

!--------------------------------------------------------------------------------
 
  subroutine user_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp2 Final starting"

    print *, "User Comp2 Final returning"

  end subroutine user_final

!--------------------------------------------------------------------------------

end module user_model2
!\end{verbatim}
