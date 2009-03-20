! $Id: user_coupler.F90,v 1.4 2009/03/20 19:49:18 svasquez Exp $
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
  use ESMF_Mod
    
  implicit none
   
  public usercpl_setvm, usercpl_register
        
  ! global data
  type(ESMF_RouteHandle), save :: routehandle1, routehandle2, routehandle3
    
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
      call ESMF_CplCompSetVMMinThreads(comp, rc=rc)
    endif
#endif

  end subroutine


  subroutine usercpl_register(comp, rc)
    type(ESMF_CplComp) :: comp
    integer, intent(out) :: rc
#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    logical :: supportPthreads
#endif

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Coupler Register starting"
    
    ! Register the callback routines.
    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETINIT, routine=user_init, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETRUN, routine=user_run, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETFINAL, routine=user_final, &
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
    character(len=ESMF_MAXSTR) :: stateName
    type(ESMF_Array) :: srcArray1, dstArray1
    type(ESMF_Array) :: srcArray2, dstArray2
    type(ESMF_Array) :: srcArray3, dstArray3
    type(ESMF_VM) :: vm

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Coupler Init starting"

    call ESMF_StateGet(importState, name=stateName, itemcount=itemcount, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    print *, "Import State, ", trim(stateName),", contains ", itemcount, " items."

    call ESMF_StateGet(exportState, name=stateName, itemcount=itemcount, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    print *, "Export State, ", trim(stateName),", contains ", itemcount, " items."

    ! Need to reconcile import and export states
    call ESMF_CplCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateReconcile(importState, vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateReconcile(exportState, vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Get source Array 1 out of import state
    call ESMF_StateGet(importState, "array data1", srcArray1, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Get destination Array 1 out of export state
    call ESMF_StateGet(exportState, "array data1", dstArray1, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Precompute and store an ArrayRedist operations 
    call ESMF_ArrayRedistStore(srcArray=srcArray1, dstArray=dstArray1, &
   	routehandle=routehandle1, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    
    ! Get source Array 2 out of import state
    call ESMF_StateGet(importState, "array data2", srcArray2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Get destination Array 2 out of export state
    call ESMF_StateGet(exportState, "array data2", dstArray2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Precompute and store an ArrayRedist operations 
    call ESMF_ArrayRedistStore(srcArray=srcArray2, dstArray=dstArray2, &
        routehandle=routehandle2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out


    ! Get source Array 3 out of import state
    call ESMF_StateGet(importState, "array data3", srcArray3, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Get destination Array 3 out of export state
    call ESMF_StateGet(exportState, "array data3", dstArray3, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Precompute and store an ArrayRedist operations 
    call ESMF_ArrayRedistStore(srcArray=srcArray3, dstArray=dstArray3, &
        routehandle=routehandle3, rc=rc)
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
    type(ESMF_Array) :: srcArray, dstArray
    character(len=ESMF_MAXSTR) :: stateName

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Coupler Run starting"

    ! Get import State name
    call ESMF_StateGet(importState, name=stateName, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    print *, "Import State ", trim(stateName)

    ! Get source Array 1 out of import state
    call ESMF_StateGet(importState, "array data1", srcArray, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Get destination Array 1 out of export state
    call ESMF_StateGet(exportState, "array data1", dstArray, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Use ArrayRedist() to take data from srcArray to dstArray 
    call ESMF_ArrayRedist(srcArray=srcArray, dstArray=dstArray, &
  	routehandle=routehandle1, rc=rc)

    ! Get source Array 2 out of import state
    call ESMF_StateGet(importState, "array data2", srcArray, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Get destination Array 2 out of export state
    call ESMF_StateGet(exportState, "array data2", dstArray, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Use ArrayRedist() to take data from srcArray to dstArray 
    call ESMF_ArrayRedist(srcArray=srcArray, dstArray=dstArray, &
        routehandle=routehandle2, rc=rc)

    ! Get source Array 3 out of import state
    call ESMF_StateGet(importState, "array data3", srcArray, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Get destination Array 3 out of export state
    call ESMF_StateGet(exportState, "array data3", dstArray, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Use ArrayRedist() to take data from srcArray to dstArray 
    call ESMF_ArrayRedist(srcArray=srcArray, dstArray=dstArray, &
        routehandle=routehandle3, rc=rc)

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
    character(len=ESMF_MAXSTR) :: stateName

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Coupler Final starting"
  
    ! Get import State name
    call ESMF_StateGet(importState, name=stateName, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Release resources stored for the ArrayRedist.
    call ESMF_ArrayRedistRelease(routehandle=routehandle1, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArrayRedistRelease(routehandle=routehandle2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArrayRedistRelease(routehandle=routehandle3, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print *, "User Coupler Final returning"
  
  end subroutine user_final


end module user_coupler
    
!\end{verbatim}

