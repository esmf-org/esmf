! $Id: CouplerMod.F90,v 1.2 2003/04/29 17:02:01 nscollins Exp $
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

    ! ESMF Framework module
    use ESMF_Mod

    implicit none
    
    public Coupler_register
        
    contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
    subroutine Coupler_register(comp, rc)
        type(ESMF_CplComp) :: comp
        integer :: rc

        print *, "in user setservices routine"

        ! Register the callback routines.

        call ESMF_CplCompSetEntryPoint(comp, ESMF_SETINIT, coupler_init, &
                                                  ESMF_SINGLEPHASE, rc)
        call ESMF_CplCompSetEntryPoint(comp, ESMF_SETRUN, coupler_run, &
                                                  ESMF_SINGLEPHASE, rc)
        call ESMF_CplCompSetEntryPoint(comp, ESMF_SETFINAL, coupler_final, &
                                                  ESMF_SINGLEPHASE, rc)

        print *, "Registered Initialize, Run, and Finalize routines"

    end subroutine

!-------------------------------------------------------------------------
!   !  Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
    subroutine coupler_init(comp, statelist, clock, rc)
        type(ESMF_CplComp) :: comp
        type(ESMF_State) :: statelist
        type(ESMF_Clock) :: clock
        integer :: rc

!     ! Local variables
        type(ESMF_State) :: flowstates, injectstates
        type(ESMF_State) :: toflow, fromflow
        type(ESMF_State) :: toinject, frominject

        print *, "Coupler Init starting"

        print *, "statelist before"
        call ESMF_StatePrint(statelist)
        call ESMF_StateGetData(statelist, &
                     "Coupler States FlowSolver to Injector", flowstates, rc)

        call ESMF_StateGetData(flowstates, "FlowSolver Feedback", fromflow, rc)
        call ESMF_StateSetNeeded(fromflow, "SIE", ESMF_STATEDATAISNEEDED, rc)
        call ESMF_StateSetNeeded(fromflow, "V", ESMF_STATEDATAISNEEDED, rc)
        call ESMF_StateSetNeeded(fromflow, "RHO", ESMF_STATEDATAISNEEDED, rc)
        call ESMF_StateSetNeeded(fromflow, "FLAG", ESMF_STATEDATAISNEEDED, rc)
        print *, "from flow"
        call ESMF_StatePrint(fromflow)

        call ESMF_StateGetData(statelist, &
                       "Coupler States Injector to FlowSolver", injectstates, rc)

        call ESMF_StateGetData(injectstates, "Injection Feedback", frominject, rc)
        call ESMF_StateSetNeeded(frominject, "SIE", ESMF_STATEDATAISNEEDED, rc)
        call ESMF_StateSetNeeded(frominject, "V", ESMF_STATEDATAISNEEDED, rc)
        call ESMF_StateSetNeeded(frominject, "RHO", ESMF_STATEDATAISNEEDED, rc)
        call ESMF_StateSetNeeded(frominject, "FLAG", ESMF_STATEDATAISNEEDED, rc)
        print *, "from inject"
        call ESMF_StatePrint(frominject)

        print *, "statelist after"
        call ESMF_StatePrint(statelist)

        print *, "Coupler Init returning"
   
    end subroutine coupler_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is coupled.
!   !
 
    subroutine coupler_run(comp, statelist, clock, rc)
        type(ESMF_CplComp) :: comp
        type(ESMF_State) :: statelist
        type(ESMF_Clock) :: clock
        integer :: rc

      ! Local variables
        type(ESMF_State) :: toflow, toinjector
        type(ESMF_State) :: mysource, mydest
        type(ESMF_Field) :: srcfield, dstfield
        type(ESMF_DELayout) :: cpllayout
      
        character(len=ESMF_MAXSTR) :: statename
       
        integer :: status
        integer :: i, datacount
        character(len=ESMF_MAXSTR), dimension(7) :: datanames

        datacount = 7
        datanames(1) = "SIE"
        datanames(2) = "U"
        datanames(3) = "V"
        datanames(4) = "RHO"
        datanames(5) = "P"
        datanames(6) = "Q"
        datanames(7) = "FLAG"

        ! Find which direction we are coupling based on the name of the state we have.
        call ESMF_StateGetName(statelist, statename, rc)
        if (trim(statename) .eq. "Coupler States Injector to FlowSolver") then

            ! Injector to FlowSolver
            call ESMF_StateGetData(statelist, "Injection Feedback", mysource, rc)
            call ESMF_StateGetData(statelist, "FlowSolver Input", mydest, rc)

        else if (trim(statename) .eq. "Coupler States FlowSolver to Injector") then

            ! Get import and export states
            call ESMF_StateGetData(statelist, "FlowSolver Feedback", mysource, rc)
            call ESMF_StateGetData(statelist, "Injection Input", mydest, rc)

        else

           print *, "Unexpected Statelist in Coupler Run routine, named ", trim(statename)
           rc = ESMF_FAILURE
           return
        endif

        ! Get layout from coupler component
        call ESMF_CplCompGet(comp, layout=cpllayout, rc=status)

        do i=1, datacount

           ! check isneeded flag here
           if (.not. ESMF_StateIsNeeded(mysource, datanames(i), rc)) then 
               cycle
           endif

           call ESMF_StateGetData(mysource, datanames(i), srcfield, rc=status)
           !call ESMF_FieldPrint(srcfield, "", rc=rc)

           call ESMF_StateGetData(mydest, datanames(i), dstfield, rc=status)
           !call ESMF_FieldPrint(dstfield, "", rc=rc)


          ! These are fields on different layouts - call Route to rearrange
          !  the data using the Comm routines.
          call ESMF_FieldRoute(srcfield, dstfield, cpllayout, status)


          ! Set export data in export state
          !call ESMF_StateAddData(mydest, datanames(i), rc=status)

        enddo

        !call ESMF_StatePrint(mydest, rc=status)
 
        rc = status

    end subroutine coupler_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
    subroutine coupler_final(comp, statelist, clock, rc)
        type(ESMF_CplComp) :: comp
        type(ESMF_State) :: statelist
        type(ESMF_Clock) :: clock
        integer :: rc

        ! Local variables
        type(ESMF_State) :: state1, state2

        print *, "Coupler Final starting"
  
        ! Nothing to do here.
    
        print *, "Coupler Final returning"
   
    end subroutine coupler_final


    end module CouplerMod
    
!\end{verbatim}
    
