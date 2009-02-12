! $Id: user_model4.F90,v 1.1 2009/02/12 20:30:30 svasquez Exp $
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

module user_model4

  ! ESMF Framework module
  use ESMF_Mod

  implicit none
    
  public userm4_register
  ! global data
  integer :: solution1, solution2, solution3
        
  contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
  subroutine userm4_register(comp, rc)
    type(ESMF_GridComp) :: comp
    integer, intent(out) :: rc
#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    logical :: supportPthreads
#endif

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp4 Register starting"

    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, user_init, &
      ESMF_SINGLEPHASE, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, user_run, &
      ESMF_SINGLEPHASE, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, user_final, &
      ESMF_SINGLEPHASE, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print *, "Registered Initialize, Run, and Finalize routines"

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

    print *, "User Comp4 Register returning"
    
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
    type(ESMF_ArraySpec)  :: arrayspec
    type(ESMF_DistGrid)   :: distgrid
    type(ESMF_Array)      :: array
    type(ESMF_VM)         :: vm
    integer               :: petCount
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp4 Init starting"

    ! Determine petCount
    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMGet(vm, petCount=petCount, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Create the destination Array and add it to the import State
    call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/100,150/), &
      regDecomp=(/1,petCount/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
      indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArraySet(array, name="array data", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateAdd(importState, array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

   
    print *, "User Comp4 Init returning"

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
    type(ESMF_Array)      :: array
    real(ESMF_KIND_R8), pointer :: farrayPtr(:,:)   ! matching F90 array pointer
    integer               :: i, j, solution
    character(len=ESMF_MAXSTR) :: stateName
    type(ESMF_Time) :: startTime, currTime
    type(ESMF_Calendar) :: gregorianCalendar

    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp4 Run starting"


    ! Get the destination Array from the import State
    call ESMF_StateGet(importState, "array data", array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Gain access to actual data via F90 array pointer
    call ESMF_ArrayGet(array, localDe=0, farrayPtr=farrayPtr, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Get import state name
    call ESMF_StateGet(importState, name=stateName, rc=rc)

    ! Determine if this is the first time run is called
    ! initialize calendar to be Gregorian type
    gregorianCalendar = ESMF_CalendarCreate("Gregorian", &
                                              ESMF_CAL_GREGORIAN, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_TimeSet(startTime, yy=2009, mm=1, dd=1, h=9, &
                        calendar=gregorianCalendar, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
   
    ! Set up the solution integer
    if (startTime == currTime) then
    	select case (trim(stateName))
        	case ("comp4 import1")
			solution1 = 1
                	solution= solution1
        	case ("comp4 import2")
                	solution2 = 2
			solution = solution2
        	case ("comp4 import3")
                	solution3 = 3
			solution = solution3
    	end select
     else
        select case (trim(stateName))
                case ("comp4 import1")
                        solution1 = solution1 * 10
			solution = solution1
                case ("comp4 import2")
                        solution2 = solution2 * 10
                        solution = solution2 
                case ("comp4 import3")
                        solution3 = solution3 * 10
                        solution = solution3
    	end select
     endif

    ! Test Array in import state against exact solution
    if (rc/=ESMF_SUCCESS) return ! bail out

    do j = lbound(farrayPtr, 2), ubound(farrayPtr, 2)
      do i = lbound(farrayPtr, 1), ubound(farrayPtr, 1)
        if (farrayPtr(i,j) /= solution ) then
          rc=ESMF_FAILURE
          return ! bail out
        endif
      enddo
    enddo

 
    print *, "User Comp4 Run returning"

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
    type(ESMF_DistGrid) :: distgrid
    type(ESMF_Array) :: array
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp4 Final starting"

    call ESMF_StateGet(importState, "array data", array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArrayGet(array, distgrid=distgrid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArrayDestroy(array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_DistGridDestroy(distgrid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print *, "User Comp4 Final returning"

  end subroutine user_final


end module user_model4
    
!\end{verbatim}
