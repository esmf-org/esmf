! $Id: CouplerMod.F90,v 1.3 2004/02/13 18:08:36 jwolfe Exp $
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

      call ESMF_CplCompSetEntryPoint(comp, ESMF_SETINIT, coupler_init, &
                                                ESMF_SINGLEPHASE, rc)
      call ESMF_CplCompSetEntryPoint(comp, ESMF_SETRUN, coupler_run, &
                                                ESMF_SINGLEPHASE, rc)
      call ESMF_CplCompSetEntryPoint(comp, ESMF_SETFINAL, coupler_final, &
                                                ESMF_SINGLEPHASE, rc)

      print *, "Registered Initialize, Run, and Finalize routines"

      rc = ESMF_SUCCESS

  end subroutine

!-------------------------------------------------------------------------
!   !  Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
  subroutine coupler_init(comp, importstate, exportstate, clock, rc)
      type(ESMF_CplComp) :: comp
      type(ESMF_State) :: importstate, exportstate
      type(ESMF_Clock) :: clock
      integer :: rc

      ! Local variables
      character (len=ESMF_MAXSTR) :: statename

      print *, "Coupler Init starting"

      call ESMF_StateGetName(importstate, statename, rc)
      if (trim(statename) .eq. "FlowSolver Feedback") then

        call ESMF_StateSetNeeded(importstate, "SIE", ESMF_STATEDATAISNEEDED, rc)
        call ESMF_StateSetNeeded(importstate, "V", ESMF_STATEDATAISNEEDED, rc)
        call ESMF_StateSetNeeded(importstate, "RHO", ESMF_STATEDATAISNEEDED, rc)
        call ESMF_StateSetNeeded(importstate, "FLAG", ESMF_STATEDATAISNEEDED, rc)

      endif

      if (trim(statename) .eq. "Injection Feedback") then

        call ESMF_StateSetNeeded(importstate, "SIE", ESMF_STATEDATAISNEEDED, rc)
        call ESMF_StateSetNeeded(importstate, "V", ESMF_STATEDATAISNEEDED, rc)
        call ESMF_StateSetNeeded(importstate, "RHO", ESMF_STATEDATAISNEEDED, rc)
        call ESMF_StateSetNeeded(importstate, "FLAG", ESMF_STATEDATAISNEEDED, rc)

      endif

      print *, "Coupler Init returning"
 
      rc = ESMF_SUCCESS

  end subroutine coupler_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is coupled.
!   !
 
  subroutine coupler_run(comp, importstate, exportstate, clock, rc)
      type(ESMF_CplComp) :: comp
      type(ESMF_State) :: importstate, exportstate
      type(ESMF_Clock) :: clock
      integer :: rc

    ! Local variables
      type(ESMF_State) :: toflow, toinjector
      type(ESMF_State) :: mysource, mydest
      type(ESMF_Field) :: srcfield, dstfield
      type(ESMF_Array) :: srcarray, dstarray
      real(kind=ESMF_KIND_R4), dimension(:,:), pointer :: srcptr, dstptr
      type(ESMF_DELayout) :: cpllayout
    
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

      call ESMF_StateGetName(importstate, statename, rc)
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

      ! Get layout from coupler component
      call ESMF_CplCompGet(comp, layout=cpllayout, rc=status)

      do i=1, datacount

         ! check isneeded flag here
         if (.not. ESMF_StateIsNeeded(importstate, datanames(i), rc)) then 
             !print *, "skipping field ", trim(datanames(i)), " not needed"
             cycle
         endif

         !print *, "processing field ", trim(datanames(i)), " as needed"
         call ESMF_StateGetData(importstate, datanames(i), srcfield, rc=status)
         call ESMF_StateGetData(exportstate, datanames(i), dstfield, rc=status)

         ! These are fields on different layouts - call Redist to rearrange
         !  the data using the Comm routines.
         call ESMF_FieldRedist(srcfield, dstfield, cpllayout, rc=status)

         ! TODO: why is this commented out?
         !call ESMF_FieldHalo(dstfield, rc) 

      enddo
 
      rc = status

  end subroutine coupler_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
    subroutine coupler_final(comp, importstate, exportstate, clock, rc)
        type(ESMF_CplComp) :: comp
        type(ESMF_State) :: importstate, exportstate
        type(ESMF_Clock) :: clock
        integer, intent(out) :: rc

        ! Local variables

        print *, "Coupler Final starting"
  
        ! Nothing to do here.
    
        print *, "Coupler Final returning"

        rc = ESMF_SUCCESS
   
    end subroutine coupler_final


    end module CouplerMod
    
!\end{verbatim}
    
