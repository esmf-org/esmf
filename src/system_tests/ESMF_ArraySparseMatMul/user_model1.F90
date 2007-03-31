! $Id: user_model1.F90,v 1.5 2007/03/31 02:26:01 cdeluca Exp $
!
! Example/test code which shows User Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  User-supplied Component, most recent interface revision.
!
!
!\begin{verbatim}

module user_model1

  ! ESMF Framework module
  use ESMF_Mod

  implicit none
    
  public userm1_register
        
  contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
  subroutine userm1_register(comp, rc)
    type(ESMF_GridComp), intent(inout) :: comp
    integer, intent(out) :: rc

    print *, "in user register routine"

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

!-------------------------------------------------------------------------
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
    
    ! Determine petCount
    call ESMF_GridCompGet(comp, vm=vm, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 10
    call ESMF_VMGet(vm, petCount=petCount, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 10
    
    ! Create the source Array and add it to the export State
    call ESMF_ArraySpecSet(arrayspec, kind=ESMF_TYPEKIND_R8, &
      rank=2, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 10
    distgrid = ESMF_DistGridCreate(minCorner=(/1,1/), maxCorner=(/100,150/), &
      regDecomp=(/petCount,1/), rc=status)
    if (status .ne. ESMF_SUCCESS) goto 10
    array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
      indexflag=ESMF_INDEX_GLOBAL, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 10
    call ESMF_ArraySet(array, name="array data", rc=status )
    if (status .ne. ESMF_SUCCESS) goto 10
    call ESMF_StateAddArray(exportState, array, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 10
   
    rc = ESMF_SUCCESS
    return
    
    ! get here only on error exit
10  continue
    rc = ESMF_FAILURE

  end subroutine user_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
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
    integer               :: i, j, petCount, status
    
    print *, "User Comp Run starting"

    pi = 3.14159d0

    ! Get the source Array from the export State
    call ESMF_StateGetArray(exportState, "array data", array, rc=status)
   
    ! Gain access to actual data via F90 array pointer
    call ESMF_ArrayGet(array, farrayPtr=farrayPtr, rc=status)
      
    ! Fill source Array with data
    do j = lbound(farrayPtr, 2), ubound(farrayPtr, 2)
      do i = lbound(farrayPtr, 1), ubound(farrayPtr, 1)
        farrayPtr(i,j) = 10.0d0 &
          + 5.0d0 * sin(real(i,ESMF_KIND_R8)/100.d0*pi) &
          + 2.0d0 * sin(real(j,ESMF_KIND_R8)/150.d0*pi)
      enddo
    enddo
 
    print *, "User Comp Run returning"

    rc = status
  end subroutine user_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
  subroutine user_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp), intent(inout) :: comp
    type(ESMF_State), intent(inout) :: importState, exportState
    type(ESMF_Clock), intent(in) :: clock
    integer, intent(out) :: rc

    ! Local variables
    integer :: status

    print *, "User Comp Final starting"

    print *, "User Comp Final returning"

    rc = ESMF_SUCCESS
  end subroutine user_final


end module user_model1
    
!\end{verbatim}
    
