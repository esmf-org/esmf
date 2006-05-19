! $Id: user_coupler.F90,v 1.2 2006/05/19 21:35:53 theurich Exp $
!
! Example/test code which shows User Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
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
   
  public usercpl_register
        
  ! global data
  type(ESMF_RouteHandle), save :: routehandle

  contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
  subroutine usercpl_register(comp, rc)
    type(ESMF_CplComp) :: comp
    integer :: rc

    print *, "in user setservices routine"

    ! Register the callback routines.
    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETINIT, user_init, &
      ESMF_SINGLEPHASE, rc)
    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETRUN, user_run, &
      ESMF_SINGLEPHASE, rc)
    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETFINAL, user_final, &
      ESMF_SINGLEPHASE, rc)

    print *, "Registered Initialize, Run, and Finalize routines"

#ifdef ESMF_TESTWITHTHREADS
    ! The following call will turn on ESMF-threading (single threaded)
    ! for this component. If you are using this file as a template for 
    ! your own code development you probably don't want to include the 
    ! following call unless you are interested in exploring ESMF's 
    ! threading features.
    call ESMF_CplCompSetVMMinThreads(comp, rc=rc)
#endif

    rc = ESMF_SUCCESS
  end subroutine

!-------------------------------------------------------------------------
!   !User Comp Component created by higher level calls, here is the
!   ! Initialization routine.
    
  subroutine user_init(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer :: rc

    ! Local variables
    integer :: itemcount
    type(ESMF_Array) :: srcArray, dstArray
    type(ESMF_VM) :: vm
    real(ESMF_KIND_R8):: factorList(15000)
    integer:: i, factorIndexList(2,15000)

    print *, "User Coupler Init starting"

    call ESMF_StateGet(importState, itemcount=itemcount, rc=rc)
    print *, "Import State contains ", itemcount, " items."

    ! Need to reconcile import and export states
    call ESMF_CplCompGet(comp, vm=vm, rc=rc)
    call ESMF_StateReconcile(importState, vm, rc=rc)
    call ESMF_StateReconcile(exportState, vm, rc=rc)

    ! Get source Array out of import state
    call ESMF_StateGetArray(importState, "array data", srcArray, rc=rc)
    ! call ESMF_ArrayPrint(srcArray, rc=rc)

    ! Get destination Array out of export state
    call ESMF_StateGetArray(exportState, "array data", dstArray, rc=rc)
    ! call ESMF_ArrayPrint(dstArray, rc=rc)

    ! Setup identity sparse matrix
    factorList = 1.d0
    factorIndexList(1,:) = (/(i,i=1,15000)/)
    factorIndexList(2,:) = factorIndexList(1,:)

    ! Precompute and store an ArraySparseMatMul operation
    call ESMF_ArraySparseMatMulStore(srcArray=srcArray, dstArray=dstArray, &
      factorList=factorList, factorIndexList=factorIndexList, &
      rootPet=0, routehandle=routehandle, rc=rc)

    print *, "User Coupler Init returning"
   
    rc = ESMF_SUCCESS

  end subroutine user_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is coupled.
!   !
 
  subroutine user_run(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer :: rc

    ! Local variables
    type(ESMF_Array) :: srcArray, dstArray

    print *, "User Coupler Run starting"

    ! Get source Array out of import state
    call ESMF_StateGetArray(importState, "array data", srcArray, rc=rc)
!    call ESMF_ArrayPrint(srcArray, rc=rc)

    ! Get destination Array out of export state
    call ESMF_StateGetArray(exportState, "array data", dstArray, rc=rc)
!    call ESMF_ArrayPrint(dstArray, rc=rc)

    ! Use ArraySparseMatMul() to take data from srcArray to dstArray
    call ESMF_ArraySparseMatMul(srcArray=srcArray, dstArray=dstArray, &
      routehandle=routehandle, rc=rc)
 
!    call ESMF_ArrayPrint(dstArray, rc=rc)
    
    print *, "User Coupler Run returning"

  end subroutine user_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
  subroutine user_final(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer :: rc

    ! Local variables

    print *, "User Coupler Final starting"
  
    ! Release resources stored for the Regridding.
    call ESMF_RouteHandleRelease(routehandle=routehandle, rc=rc)

    print *, "User Coupler Final returning"
  
    rc = ESMF_SUCCESS

  end subroutine user_final


end module user_coupler
    
!\end{verbatim}
    
