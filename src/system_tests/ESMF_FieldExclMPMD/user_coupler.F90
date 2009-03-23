! $Id: user_coupler.F90,v 1.7 2009/03/23 20:40:48 theurich Exp $
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
    type(ESMF_RouteHandle), save :: redistRH12, redistRH23, regridRH, haloRH3

    contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
    subroutine usercpl_register(comp, rc)
      type(ESMF_CplComp) :: comp
      integer :: rc

      integer :: localrc

      !print *, "in user setservices routine"

      ! Register the callback routines.
      call ESMF_CplCompSetEntryPoint(comp, ESMF_SETINIT, user_init, rc=localrc)
      call ESMF_CplCompSetEntryPoint(comp, ESMF_SETRUN, user_run, rc=localrc)
      call ESMF_CplCompSetEntryPoint(comp, ESMF_SETFINAL, user_final, rc=localrc)

      !print *, "Registered Initialize, Run, and Finalize routines"

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
      type(ESMF_Field) :: pressure1, pressure2, pressure3
      type(ESMF_VM) :: vm
      integer :: pet_id

      !print *, "User Coupler Init starting"
      status = ESMF_FAILURE

      ! Get VM from coupler component
      call ESMF_CplCompGet(comp, vm=vm, rc=status)
      call ESMF_VMGet(vm, localPET=pet_id, rc=status)

      ! Since the components we are coupling between are running concurrently,
      ! they have each separately created ESMF objects.   We are planning to
      ! use a communications call (Regrid) here, so first we must make a new
      ! call to reconcile the object lists in all the import and export states.
 
      ! New routine:
      ! Must be called on each state which is going to be accessed from
      ! this coupler.  When the call returns all objects which were not
      ! in existence on all PETs now have an object which represents them.
      call ESMF_StateReconcile(importState, vm, rc=status)
      ! call ESMF_StatePrint(importState, rc=status)

      call ESMF_StateReconcile(exportState, vm, rc=status)
      ! call ESMF_StatePrint(exportState, rc=status)

      call ESMF_StateGet(importState, itemcount=itemcount, rc=status)
      !print *, "Import State contains ", itemcount, " items."

      ! Get input data for Regrid test
      call ESMF_StateGetField(importState, "humidity1", humidity1, rc=status)

      ! Get location of output data for Regrid test
      call ESMF_StateGetField(exportState, "humidity2", humidity2, rc=status)

      ! These are fields on different IGrids - call RegridStore to set
      ! up the Regrid structure

      call ESMF_FieldRegridStore(humidity1, humidity2, vm, regridRH, &
                                 regridmethod=ESMF_REGRID_METHOD_BILINEAR, &
                                 rc=status)

      ! for debugging, this prints who is planning to send data and where 
      ! call ESMF_RouteHandlePrint(regridRH, "", rc=status)

      ! Get input data and final location for Redist test
      call ESMF_StateGetField(importState, "pressure1", pressure1, rc=status)
      call ESMF_StateGetField(importState, "pressure3", pressure3, rc=status)

      ! Get location of field on the different layout for Redist test
      call ESMF_StateGetField(exportState, "pressure2", pressure2, rc=status)

      ! These are fields on different layouts - call RedistStore to set
      ! up the Redist structures

      call ESMF_FieldRedistStore(pressure1, pressure2, vm, &
                                 routehandle=redistRH12, rc=status)
      call ESMF_FieldRedistStore(pressure2, pressure3, vm, &
                                 routehandle=redistRH23, rc=status)
      call ESMF_FieldHaloStore(pressure3, routehandle=haloRH3, rc=status)

      ! for debugging, this prints who is planning to send data and where 
      ! call ESMF_RouteHandlePrint(redistRH12, "", rc=status)
      ! call ESMF_RouteHandlePrint(redistRH23, "", rc=status)

      !print *, "User Coupler Init returning"
   
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
      type(ESMF_Field) :: pressure1, pressure2, pressure3
      integer :: status

      !print *, "User Coupler Run starting"

      ! Get input data for regrid test
      call ESMF_StateGetField(importState, "humidity1", humidity1, rc=status)
      ! call ESMF_FieldPrint(humidity1, rc=status)

      ! Get location of output data for regrid test
      call ESMF_StateGetField(exportState, "humidity2", humidity2, rc=status)
      ! call ESMF_FieldPrint(humidity2, rc=status)

      ! These are fields on different IGrids - call Regrid to rearrange
      !  the data.   The communication pattern was computed at init,
      !  this simply has to execute the send and receive equivalents.

      call ESMF_FieldRegrid(humidity1, humidity2, regridRH, rc=status)

      ! Data is moved directly to the field in the output state, so no
      ! "put" is needed here.
 
      ! Get input data and final array for redist test
      call ESMF_StateGetField(importState, "pressure1", pressure1, rc=status)
      call ESMF_StateGetField(importState, "pressure3", pressure3, rc=status)
      ! call ESMF_FieldPrint(pressure1, rc=status)
      ! call ESMF_FieldPrint(pressure3, rc=status)

      ! Get location of output data for redist test
      call ESMF_StateGetField(exportState, "pressure2", pressure2, rc=status)
      ! call ESMF_FieldPrint(pressure2, rc=status)

      ! These are fields on different layouts - call Redist to rearrange
      ! the data to the secodn layout and then back to the first.   The
      ! communication patterns was computed at init, this simply has to
      ! execute the send and receive equivalents.

      call ESMF_FieldRedist(pressure1, pressure2, redistRH12, rc=status)
      ! call ESMF_FieldPrint(pressure2)
      call ESMF_FieldRedist(pressure2, pressure3, redistRH23, rc=status)
      ! call ESMF_FieldPrint(pressure3)
      call ESMF_FieldHalo(pressure3, haloRH3, rc=status)
      ! call ESMF_FieldPrint(pressure3)

      ! Data is moved directly to the field in the output state, so no
      ! "put" is needed here.
 
      !print *, "User Coupler Run returning"

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

      !print *, "User Coupler Final starting"
      status = ESMF_FAILURE
   
      ! Release resources stored for the Regridding.
      call ESMF_FieldRegridRelease(regridRH,   rc=status)
      call ESMF_FieldRedistRelease(redistRH12, rc=status)
      call ESMF_FieldRedistRelease(redistRH23, rc=status)
      call ESMF_FieldHaloRelease(haloRH3, rc=status)

      !print *, "User Coupler Final returning"
   
      rc = status

    end subroutine user_final


    end module user_coupler
    
!\end{verbatim}
    
