! $Id: CouplerMod.F90,v 1.21 2009/03/23 20:40:48 theurich Exp $
!

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  Two-way Coupling between Injector and FlowSolver Models
!
!
!\begin{verbatim}

    module CouplerMod

    ! ESMF Framework module - defines ESMF data types and procedures
    use ESMF_Mod

    implicit none
    
    ! Public entry point 
    public Coupler_register
        
    ! module global, not public
    type(ESMF_RouteHandle), save :: rh_Flow_to_Inject, rh_Inject_to_Flow

    contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
  subroutine Coupler_register(comp, rc)
      type(ESMF_CplComp), intent(inout) :: comp
      integer, intent(out) :: rc

      print *, "in user setservices routine"

      ! Register the callback routines.

      call ESMF_CplCompSetEntryPoint(comp, ESMF_SETINIT, coupler_init, rc=rc)
      call ESMF_CplCompSetEntryPoint(comp, ESMF_SETRUN, coupler_run, rc=rc)
      call ESMF_CplCompSetEntryPoint(comp, ESMF_SETFINAL, coupler_final, rc=rc)

      print *, "Registered Initialize, Run, and Finalize routines"

      rc = ESMF_SUCCESS

  end subroutine

!-------------------------------------------------------------------------
!   !  Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
  subroutine coupler_init(comp, importState, exportState, clock, rc)
      type(ESMF_CplComp) :: comp
      type(ESMF_State) :: importState, exportState
      type(ESMF_Clock) :: clock
      integer :: rc

      ! Local variables
      character (len=ESMF_MAXSTR) :: statename
      type(ESMF_Field) :: srcfield, dstfield
      type(ESMF_VM) :: vm

      print *, "Coupler Init starting"

      ! Extract a field from each of the import and export states and use 
      ! them as template for precomputing the data transfer pattern needed
      ! for redistribution during the run phase.

      call ESMF_StateGetField(importState, "SIE", srcfield, rc=rc)
      call ESMF_StateGetField(exportState, "SIE", dstfield, rc=rc)
     
      ! Get VM info from coupler component
      call ESMF_CplCompGet(comp, vm=vm, rc=rc)

      ! Now see which way we're going so we set the correct fields needed
      ! and compute the right routehandle
      call ESMF_StateGet(importState, name=statename, rc=rc)
      if (trim(statename) .eq. "FlowSolver Feedback") then

        call ESMF_StateSetNeeded(importState, "SIE", ESMF_NEEDED, rc)
        call ESMF_StateSetNeeded(importState, "V", ESMF_NEEDED, rc)
        call ESMF_StateSetNeeded(importState, "RHO", ESMF_NEEDED, rc)
        call ESMF_StateSetNeeded(importState, "FLAG", ESMF_NEEDED, rc)

        ! Precompute and return a routehandle which identifies this operation
        call ESMF_FieldRedistStore(srcfield, dstfield, vm, &
                                   routehandle=rh_Flow_to_Inject, rc=rc)

      endif

      if (trim(statename) .eq. "Injection Feedback") then

        call ESMF_StateSetNeeded(importState, "SIE", ESMF_NEEDED, rc)
        call ESMF_StateSetNeeded(importState, "V", ESMF_NEEDED, rc)
        call ESMF_StateSetNeeded(importState, "RHO", ESMF_NEEDED, rc)
        call ESMF_StateSetNeeded(importState, "FLAG", ESMF_NEEDED, rc)

        ! Precompute and return a routehandle which identifies this operation
        call ESMF_FieldRedistStore(srcfield, dstfield, vm, &
                                   routehandle=rh_Inject_to_Flow, rc=rc)

      endif

      print *, "Coupler Init returning"
 
      rc = ESMF_SUCCESS

  end subroutine coupler_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is coupled.
!   !
 
  subroutine coupler_run(comp, importState, exportState, clock, rc)
      type(ESMF_CplComp) :: comp
      type(ESMF_State) :: importState, exportState
      type(ESMF_Clock) :: clock
      integer :: rc

    ! Local variables
      type(ESMF_Field) :: srcfield, dstfield
      character(len=ESMF_MAXSTR) :: statename
      integer :: status
      integer :: i, datacount
      character(len=ESMF_MAXSTR), dimension(7) :: datanames
      logical :: injecttoflow

      datacount = 7
      datanames(1) = "SIE"
      datanames(2) = "U"
      datanames(3) = "V"
      datanames(4) = "RHO"
      datanames(5) = "P"
      datanames(6) = "Q"
      datanames(7) = "FLAG"

      ! In this case, we don't actually care which way we're coupling,
      ! since the transformations are symmetric.  But if we cared, here is
      ! one way of telling.

      call ESMF_StateGet(importState, name=statename, rc=rc)
      if (trim(statename) .eq. "Injection Feedback") then
          ! Injector to FlowSolver
          injecttoflow = .TRUE.
      else if (trim(statename) .eq. "FlowSolver Feedback") then
          ! FlowSolver to Injector
          injecttoflow = .FALSE.
      else
         print *, "Unexpected State in Coupler Run routine, named ", trim(statename)
         rc = ESMF_FAILURE
         return
      endif

      do i=1, datacount

         ! check isneeded flag here
         if (.not. ESMF_StateIsNeeded(importState, datanames(i), rc)) then 
             !print *, "skipping field ", trim(datanames(i)), " not needed"
             cycle
         endif

         !print *, "processing field ", trim(datanames(i)), " as needed"
         call ESMF_StateGetField(importState, datanames(i), srcfield, rc=status)
         call ESMF_StateGetField(exportState, datanames(i), dstfield, rc=status)

         ! These are fields on different layouts - call Redist to rearrange
         !  the data using the Comm routines.  The handle identifies which
         !  precomputed pattern to use
         if (injecttoflow)  then
            call ESMF_FieldRedist(srcfield, dstfield, &
                                  rh_Inject_to_Flow, rc=status)
         else
            call ESMF_FieldRedist(srcfield, dstfield, &
                                  rh_Flow_to_Inject, rc=status)
         endif

         ! TODO: why is this commented out?
         !call ESMF_FieldHalo(dstfield, rc) 

      enddo
 
      rc = status

  end subroutine coupler_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
    subroutine coupler_final(comp, importState, exportState, clock, rc)
        type(ESMF_CplComp) :: comp
        type(ESMF_State) :: importState, exportState
        type(ESMF_Clock) :: clock
        integer, intent(out) :: rc

        ! Local variables

        print *, "Coupler Final starting"
  
        ! Release the Redist space in the framework
        call ESMF_FieldRedistRelease(rh_Flow_to_Inject, rc)
        call ESMF_FieldRedistRelease(rh_Inject_to_Flow, rc)
    
        print *, "Coupler Final returning"

        rc = ESMF_SUCCESS
   
    end subroutine coupler_final


    end module CouplerMod
    
!\end{verbatim}
    
