! $Id: user_coupler.F90,v 1.1 2009/10/26 17:25:58 oehmke Exp $
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
      integer, intent(out) :: rc

      rc = ESMF_SUCCESS
      print *, "in user setservices routine"

      ! Register the callback routines.
      call ESMF_CplCompSetEntryPoint(comp, ESMF_SETINIT, user_init, rc=rc)
      if(rc/=ESMF_SUCCESS) return
      call ESMF_CplCompSetEntryPoint(comp, ESMF_SETRUN, user_run, rc=rc)
      if(rc/=ESMF_SUCCESS) return
      call ESMF_CplCompSetEntryPoint(comp, ESMF_SETFINAL, user_final, rc=rc)
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
      integer :: localPET, petCount
      type(ESMF_Field) :: srcField,dstField
      type(ESMF_VM) :: vm

      rc = ESMF_SUCCESS

      ! Need to reconcile import and export states
      call ESMF_CplCompGet(comp, vm=vm, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out

      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if(rc/=ESMF_SUCCESS) return

      write(*,*) localPET,"User Coupler Init Start"

      call ESMF_StateReconcile(importState, vm, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out

      write(*,*) localPET,"User Coupler After Import Reconcile"

      call ESMF_StateReconcile(exportState, vm, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out

      write(*,*) localPET,"User Coupler After Export Reconcile"

      ! Get input data
      call ESMF_StateGet(importState, "src", srcField, rc=rc)
      if(rc/=ESMF_SUCCESS) return

      ! Get location of output data
      call ESMF_StateGet(exportState, "dst", dstField, rc=rc)
      if(rc/=ESMF_SUCCESS) return

      ! These are fields on different Grids - call RegridStore to set
      ! up the Regrid structure
      call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, &
                                 routeHandle=routehandle, &
                                 regridMethod=ESMF_REGRID_METHOD_BILINEAR, &
                                 rc=rc)
      if(rc/=ESMF_SUCCESS) return


      write(*,*) localPET,"User Coupler Init End"
   
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
      type(ESMF_Field) :: srcField, dstField
      integer :: status

      rc = ESMF_SUCCESS
      print *, "User Coupler Run starting"

      ! Get input data
      call ESMF_StateGet(importState, "src", srcField, rc=rc)
      if(rc/=ESMF_SUCCESS) return

      ! Get location of output data
      call ESMF_StateGet(exportState, "dst", dstField, rc=rc)
      if(rc/=ESMF_SUCCESS) return

      ! These are fields on different Grids - call Regrid to rearrange
      !  the data.   The communication pattern was computed at init,
      !  this simply has to execute the send and receive equivalents.
      call ESMF_FieldRegrid(srcField, dstField, routehandle, rc=status)
      if(rc/=ESMF_SUCCESS) return

 
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
      call ESMF_FieldRegridRelease(routehandle, rc)
      if(rc/=ESMF_SUCCESS) return

      print *, "User Coupler Final returning"

    end subroutine user_final


    end module user_coupler
    
!\end{verbatim}
    
