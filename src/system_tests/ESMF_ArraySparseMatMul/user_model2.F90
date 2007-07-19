! $Id: user_model2.F90,v 1.15 2007/07/19 22:31:20 theurich Exp $
!
! Example/test code which shows User Component calls.

!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  User-supplied Component
!
!
!\begin{verbatim}

module user_model2

  ! ESMF Framework module
  use ESMF_Mod

  implicit none
    
  public userm2_register
        
  contains

!--------------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
  subroutine userm2_register(comp, rc)
    type(ESMF_GridComp) :: comp
    integer :: rc

    ! local variables

    print *, "In user register routine"

    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, user_init, &
      ESMF_SINGLEPHASE, rc)
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, user_run, &
      ESMF_SINGLEPHASE, rc)
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, user_final, &
      ESMF_SINGLEPHASE, rc)

    print *, "Registered Initialize, Run, and Finalize routines"

#ifdef ESMF_TESTWITHTHREADS
    ! The following call will turn on ESMF-threading (single threaded)
    ! for this component. If you are using this file as a template for 
    ! your own code development you probably don't want to include the 
    ! following call unless you are interested in exploring ESMF's 
    ! threading features.
    call ESMF_GridCompSetVMMinThreads(comp, rc=rc)
#endif

    rc = ESMF_SUCCESS
  end subroutine

!--------------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
  subroutine user_init(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp), intent(inout) :: comp
    type(ESMF_State), intent(inout) :: importState, exportState
    type(ESMF_Clock), intent(in) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_ArraySpec)  :: arrayspec
    type(ESMF_DistGrid)   :: distgrid
    type(ESMF_Array)      :: array
    type(ESMF_VM)         :: vm
    integer               :: petCount, status
    real(ESMF_KIND_R8), pointer :: farrayPtr(:,:)   ! matching F90 array pointer
    
    ! Determine petCount
    call ESMF_GridCompGet(comp, vm=vm, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 10
    call ESMF_VMGet(vm, petCount=petCount, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 10
    
    ! Create the destination Array and add it to the import State
    call ESMF_ArraySpecSet(arrayspec, &
                           typekind=ESMF_TYPEKIND_R8, rank=2, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 10
    distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/100,150/), &
      regDecomp=(/1,petCount/), rc=status)
    if (status .ne. ESMF_SUCCESS) goto 10
    array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
      indexflag=ESMF_INDEX_GLOBAL, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 10
    call ESMF_ArraySet(array, name="array data", rc=status )
    if (status .ne. ESMF_SUCCESS) goto 10
    call ESMF_StateAddArray(importState, array, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 10
   
    ! Reset the destination Array
    call ESMF_ArrayGet(array, farrayPtr=farrayPtr, rc=status)

    rc = ESMF_SUCCESS

    return
    
    ! get here only on error exit
10  continue
    rc = ESMF_FAILURE

  end subroutine user_init


!--------------------------------------------------------------------------------
!   !  The Run routine where data is validated.
!   !
 
  subroutine user_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp), intent(inout) :: comp
    type(ESMF_State), intent(inout) :: importState, exportState
    type(ESMF_Clock), intent(in) :: clock
    integer, intent(out) :: rc

    ! Local variables
    real(ESMF_KIND_R8)    :: pi
    type(ESMF_Array)      :: array
    real(ESMF_KIND_R8), pointer :: farrayPtr(:,:)   ! matching F90 array pointer
    integer               :: i, j, status
    
    print *, "User Comp Run starting"

    pi = 3.14159d0

    ! Get the destination Array from the import State
    call ESMF_StateGetArray(importState, "array data", array, rc=status)
   
    ! Gain access to actual data via F90 array pointer
    call ESMF_ArrayGet(array, farrayPtr=farrayPtr, rc=status)
      
    ! Test Array in import state against exact solution
    do j = lbound(farrayPtr, 2), ubound(farrayPtr, 2)
      do i = lbound(farrayPtr, 1), ubound(farrayPtr, 1)
        if (abs(farrayPtr(i,j) - (10.0d0 &
          + 5.0d0 * sin(real(i,ESMF_KIND_R8)/100.d0*pi) &
          + 2.0d0 * sin(real(j,ESMF_KIND_R8)/150.d0*pi))) > 1.d-8) goto 20
      enddo
    enddo
 
    print *, "User Comp Run returning"

    rc = status
    return
    
    ! get here only on error exit
20  continue
    rc = ESMF_FAILURE
    
  end subroutine user_run


!--------------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
  subroutine user_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp), intent(inout) :: comp
    type(ESMF_State), intent(inout) :: importState, exportState
    type(ESMF_Clock), intent(in) :: clock
    integer, intent(out) :: rc


    print *, "User Comp Final starting"

    print *, "User Comp Final returning"

    rc = ESMF_SUCCESS
   
  end subroutine user_final

end module user_model2
!\end{verbatim}
    
