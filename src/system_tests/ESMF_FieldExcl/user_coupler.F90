! $Id: user_coupler.F90,v 1.11 2004/12/07 23:36:04 jwolfe Exp $
!
! System test of Exclusive components, user-written Coupler component.

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

      integer :: localrc

      print *, "in user setservices routine"

      ! Register the callback routines.
      call ESMF_CplCompSetEntryPoint(comp, ESMF_SETINIT, user_init, &
                                     ESMF_SINGLEPHASE, localrc)
      call ESMF_CplCompSetEntryPoint(comp, ESMF_SETRUN, user_run, &
                                     ESMF_SINGLEPHASE, localrc)
      call ESMF_CplCompSetEntryPoint(comp, ESMF_SETFINAL, user_final, &
                                     ESMF_SINGLEPHASE, localrc)

      print *, "Registered Initialize, Run, and Finalize routines"

      ! set return code
      rc = localrc

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
      integer :: itemcount, status
      type(ESMF_Field) :: humidity1, humidity2
      type(ESMF_VM) :: vm
      integer :: pet_id

      print *, "User Coupler Init starting"
      status = ESMF_FAILURE

      ! Get VM from coupler component
      call ESMF_CplCompGet(comp, vm=vm, rc=status)
      call ESMF_VMGet(vm, localPET=pet_id, rc=status)

      ! Since we are planning to use a communication call, we must
      ! reconcile the object lists in the State objects.
 
      ! New routine:
      ! must be called on each state which is going to be accessed from
      ! this coupler.  when the call returns, all objects which were not
      ! in existance on all PETs now have an object which represents them.
      call ESMF_StateReconcile(importState, vm, rc=status)
      print *, "i am", pet_id, "this is my import state after reconcile"
      call ESMF_StatePrint(importState, rc=status)

      call ESMF_StateReconcile(exportState, vm, rc=status)
      print *, "i am", pet_id, "this is my export state after reconcile"
      call ESMF_StatePrint(exportState, rc=status)
      print *, "done with both reconcile calls"

      call ESMF_StateGet(importState, itemcount=itemcount, rc=status)
      print *, "Import State contains ", itemcount, " items."

      ! Get input data
      call ESMF_StateGetField(importState, "humidity1", humidity1, rc=status)
      ! call ESMF_FieldPrint(humidity1, rc=status)

      ! Get location of output data
      call ESMF_StateGetField(exportState, "humidity2", humidity2, rc=status)
      ! call ESMF_FieldPrint(humidity2, rc=status)

      ! These are fields on different Grids - call RegridStore to set
      ! up the Regrid structure

      call ESMF_FieldRegridStore(humidity1, humidity2, vm, routehandle, &
                                 regridmethod=ESMF_REGRID_METHOD_BILINEAR, &
                                 rc=status)

      ! for debugging, this prints who is planning to send data and where 
      call ESMF_RouteHandlePrint(routehandle, "", rc=status)

      print *, "User Coupler Init returning"
   
      rc = ESMF_SUCCESS
      return

      ! get here only on error exit
10  continue
      rc = status
  
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
      type(ESMF_Field) :: humidity1, humidity2
      integer :: status

      print *, "User Coupler Run starting"

      ! Get input data
      call ESMF_StateGetField(importState, "humidity1", humidity1, rc=status)
      ! call ESMF_FieldPrint(humidity1, rc=status)

      ! Get location of output data
      call ESMF_StateGetField(exportState, "humidity2", humidity2, rc=status)
      ! call ESMF_FieldPrint(humidity2, rc=status)

      ! These are fields on different Grids - call Regrid to rearrange
      !  the data.   The communication pattern was computed at init,
      !  this simply has to execute the send and receive equivalents.

      call ESMF_FieldRegrid(humidity1, humidity2, routehandle, rc=status)

      ! Data is moved directly to the field in the output state, so no
      ! "put" is needed here.
 
      print *, "User Coupler Run returning"

      rc = status

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
      integer :: status

      print *, "User Coupler Final starting"
      status = ESMF_FAILURE
   
      ! Release resources stored for the Regridding.
      call ESMF_FieldRegridRelease(routehandle, rc=status)

      print *, "User Coupler Final returning"
   
      rc = status

    end subroutine user_final


    end module user_coupler
    
!\end{verbatim}
    
