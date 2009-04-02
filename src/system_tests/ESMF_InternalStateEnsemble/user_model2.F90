! $Id: user_model2.F90,v 1.7 2009/04/02 20:15:40 svasquez Exp $
!
! Example/test code which shows User Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

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
    
  public userm2_setvm, userm2_register
  ! global data
  integer :: solution1, solution2, solution3

  ! Internal State Variables
  type testData
  sequence
      type(ESMF_ArraySpec), allocatable  :: arrayspec(:)
      type(ESMF_DistGrid), allocatable   :: distgrid(:)
      type(ESMF_Array), allocatable      :: array(:)
  end type

   type dataWrapper
   sequence
      type(testData), pointer :: p
   end type


   contains
!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.

  subroutine userm2_setvm(comp, rc)
    type(ESMF_GridComp) :: comp
    integer, intent(out) :: rc
#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm 
    logical :: supportPthreads
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
    call ESMF_VMGet(vm, supportPthreadsFlag=supportPthreads, rc=rc)
    if (supportPthreads) then
      call ESMF_GridCompSetVMMinThreads(comp, rc=rc)
    endif
#endif 
      
  end subroutine

 
  subroutine userm2_register(comp, rc)
    type(ESMF_GridComp) :: comp
    integer, intent(out) :: rc
#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    logical :: supportPthreads
#endif

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp2 Register starting"

    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, userRoutine=user_init, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, userRoutine=user_run, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, userRoutine=user_final, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print *, "Registered Initialize, Run, and Finalize routines"
    print *, "User Comp2 Register returning"
    
  end subroutine

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
  subroutine user_init(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_VM)         :: vm
    integer               :: petCount
    type(dataWrapper) :: wrap
    type(testData), pointer :: data
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp2 Init starting"

    ! Determine petCount
    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMGet(vm, petCount=petCount, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out



    ! Initialize  internal State 
    ! Allocate private data block
    allocate(data)
    allocate(data%arrayspec(3))
    allocate(data%distgrid(3))
    allocate(data%array(3))

    ! Create the destination Array 1 and add it to the import State
    call ESMF_ArraySpecSet(arrayspec=data%arrayspec(1), typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    data%distgrid(1) = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/100,150/), &
      regDecomp=(/1,petCount/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    data%array(1) = ESMF_ArrayCreate(arrayspec=data%arrayspec(1), distgrid=data%distgrid(1), &
      indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArraySet(array=data%array(1), name="array data1", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateAdd(importState, data%array(1), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out


    ! Create the destination Array 2 and add it to the import State
    call ESMF_ArraySpecSet(arrayspec=data%arrayspec(2), typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    data%distgrid(2) = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/100,150/), &
      regDecomp=(/1,petCount/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    data%array(2) = ESMF_ArrayCreate(arrayspec=data%arrayspec(2), distgrid=data%distgrid(2), &
      indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArraySet(array=data%array(2), name="array data2", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateAdd(importState, data%array(2), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out



    ! Create the destination Array 3 and add it to the import State
    call ESMF_ArraySpecSet(arrayspec=data%arrayspec(3), typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    data%distgrid(3) = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/100,150/), &
      regDecomp=(/1,petCount/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    data%array(3) = ESMF_ArrayCreate(arrayspec=data%arrayspec(3), distgrid=data%distgrid(3), &
      indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArraySet(array=data%array(3), name="array data3", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateAdd(importState, data%array(3), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out



    wrap%p => data
    ! Set  internal State 1
    call ESMF_GridCompSetInternalState(comp, wrap, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
 
    !Initialize solutions
    solution1 = 1
    solution2 = 2
    solution3 = 3

    print *, "User Comp2 Init returning"

  end subroutine user_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
  subroutine user_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_Array)      :: array1, array2, array3
    real(ESMF_KIND_R8), pointer :: farrayPtr(:,:)   ! matching F90 array pointer
    integer               :: i, j
    character(len=ESMF_MAXSTR) :: stateName
    type(ESMF_Time) :: startTime, currTime
    type(ESMF_Calendar) :: gregorianCalendar
    type(ESMF_Array)    :: array

    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp2 Run starting"

    ! Get the destination Array 1 from the import State
    call ESMF_StateGet(importState, "array data1", array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Gain access to actual data via F90 array pointer
    call ESMF_ArrayGet(array=array, localDe=0, farrayPtr=farrayPtr, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Set up the solution integer
    solution1 = solution1 * 10

    ! Test Array in import state against exact solution
    do j = lbound(farrayPtr, 2), ubound(farrayPtr, 2)
      do i = lbound(farrayPtr, 1), ubound(farrayPtr, 1)
        if (farrayPtr(i,j) /= solution1 ) then
          rc=ESMF_FAILURE
          return ! bail out
        endif
      enddo
    enddo



    ! Get the destination Array 2 from the import State
    call ESMF_StateGet(importState, "array data2", array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Gain access to actual data via F90 array pointer
    call ESMF_ArrayGet(array=array, localDe=0, farrayPtr=farrayPtr, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Set up the solution integer
    solution2 = solution2 * 10

    ! Test Array in import state against exact solution
    do j = lbound(farrayPtr, 2), ubound(farrayPtr, 2)
      do i = lbound(farrayPtr, 1), ubound(farrayPtr, 1)
        if (farrayPtr(i,j) /= solution2 ) then
          rc=ESMF_FAILURE
          return ! bail out
        endif
      enddo
    enddo

    ! Get the destination Array 3 from the import State
    call ESMF_StateGet(importState, "array data3", array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Gain access to actual data via F90 array pointer
    call ESMF_ArrayGet(array=array, localDe=0, farrayPtr=farrayPtr, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Set up the solution integer
    solution3 = solution3 * 10

    ! Test Array in import state against exact solution
    do j = lbound(farrayPtr, 2), ubound(farrayPtr, 2)
      do i = lbound(farrayPtr, 1), ubound(farrayPtr, 1)
        if (farrayPtr(i,j) /= solution3 ) then
          rc=ESMF_FAILURE
          return ! bail out
        endif
      enddo
    enddo

    print *, "User Comp2 Run returning"

  end subroutine user_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
  subroutine user_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(dataWrapper) :: wrap
    type(testData), pointer :: data

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp2 Final starting"

    ! Get internal State 1
    call ESMF_GridCompGetInternalState(comp, wrap, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    data => wrap%p
    deallocate(data)

    print *, "User Comp2 Final returning"

  end subroutine user_final


end module user_model2
    
!\end{verbatim}
