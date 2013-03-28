! $Id$
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
    use ESMF
    
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
      integer, intent(out) :: rc

      rc = ESMF_SUCCESS
      print *, "in user setservices routine"

      ! Register the callback routines.
      call ESMF_CplCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, user_init, rc=rc)
      if(rc/=ESMF_SUCCESS) return
      call ESMF_CplCompSetEntryPoint(comp, ESMF_METHOD_RUN, user_run, rc=rc)
      if(rc/=ESMF_SUCCESS) return
      call ESMF_CplCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, user_final, rc=rc)
      if(rc/=ESMF_SUCCESS) return

      print *, "Registered Initialize, Run, and Finalize routines"

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
      type(ESMF_Field) :: humidity1, humidity2
      type(ESMF_VM) :: vm

      rc = ESMF_SUCCESS
      print *, "User Coupler Init starting"

      ! Need to reconcile import and export states
      call ESMF_CplCompGet(comp, vm=vm, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
      call ESMF_StateReconcile(importState, vm=vm, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
      call ESMF_StateReconcile(exportState, vm=vm, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out

      call ESMF_StateGet(importState, itemcount=itemcount, rc=rc)
      if(rc/=ESMF_SUCCESS) return
      print *, "Import State contains ", itemcount, " items."
       
      ! Get input data
      call ESMF_StateGet(importState, "humidity", humidity1, rc=rc)
      if(rc/=ESMF_SUCCESS) return
      ! call ESMF_FieldPrint(humidity1, rc=rc)

      ! Get location of output data
      call ESMF_StateGet(exportState, "humidity", humidity2, rc=rc)
      if(rc/=ESMF_SUCCESS) return
      ! call ESMF_FieldPrint(humidity2, rc=rc)

      ! Get VM from coupler component to send down to Regrid Store routine
      call ESMF_CplCompGet(comp, vm=vm, rc=rc)
      if(rc/=ESMF_SUCCESS) return

      ! These are fields on different Grids - call RegridStore to set
      ! up the Regrid structure

      call ESMF_FieldRegridStore(srcField=humidity1, dstField=humidity2, &
                                 routeHandle=routehandle, &
                                 regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
                                 rc=rc)
      if(rc/=ESMF_SUCCESS) return

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
      type(ESMF_Field) :: humidity1, humidity2
      integer :: status

      rc = ESMF_SUCCESS
      print *, "User Coupler Run starting"

      ! Get input data
      call ESMF_StateGet(importState, "humidity", humidity1, rc=rc)
      if(rc/=ESMF_SUCCESS) return
      ! call ESMF_FieldPrint(humidity1, rc=rc)

      ! Get location of output data
      call ESMF_StateGet(exportState, "humidity", humidity2, rc=rc)
      if(rc/=ESMF_SUCCESS) return
      ! call ESMF_FieldPrint(humidity2, rc=rc)

      ! These are fields on different Grids - call Regrid to rearrange
      !  the data.   The communication pattern was computed at init,
      !  this simply has to execute the send and receive equivalents.

      call ESMF_FieldRegrid(humidity1, humidity2, routehandle, rc=status)
      if(rc/=ESMF_SUCCESS) return

      ! Data is moved directly to the field in the output state, so no
      ! "put" is needed here.
 
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

      ! Local variables

      rc = ESMF_SUCCESS
      print *, "User Coupler Final starting"
   
      ! Release resources stored for the Regridding.
      call ESMF_FieldRegridRelease(routehandle, rc=rc)
      if(rc/=ESMF_SUCCESS) return

      print *, "User Coupler Final returning"

    end subroutine user_final


    end module user_coupler
    
!\end{verbatim}
    
