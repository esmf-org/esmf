! $Id: user_coupler.F90,v 1.5 2003/04/28 19:08:43 nscollins Exp $
!
! Example/test code which shows User Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  User-supplied Coupler, Version A (minimalist)
!
!
!\begin{verbatim}

    module user_coupler

    ! ESMF Framework module
    use ESMF_Mod

    implicit none
    
    public usercpl_register
        
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

    end subroutine

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
    subroutine user_init(comp, statelist, clock, rc)
        type(ESMF_CplComp) :: comp
        type(ESMF_State) :: statelist
        type(ESMF_Clock) :: clock
        integer :: rc

!     ! Local variables
        type(ESMF_State) :: toflow, fromflow
        type(ESMF_Field) :: toflow_sie, toflow_u, toflow_v 
        type(ESMF_Field) :: fromflow_sie, fromflow_u, fromflow_v 

        print *, "User Coupler Init starting"
        call ESMF_StateGetData(statelist, "name?", toflow, rc)

        call ESMF_StateSetNeeded(toflow, "SIE", rc)
        call ESMF_StateSetNeeded(toflow, "V", rc)
        call ESMF_StateSetNeeded(toflow, "RHO", rc)
        call ESMF_StateSetNeeded(toflow, "FLAG", rc)

        call ESMF_StateGetData(statelist, "name?", fromflow, rc)

        call ESMF_StateSetNeeded(fromflow, "SIE", rc)
        call ESMF_StateSetNeeded(fromflow, "V", rc)
        call ESMF_StateSetNeeded(fromflow, "RHO", rc)
        call ESMF_StateSetNeeded(fromflow, "FLAG", rc)

        ! do we set up coupling now?  the flow solver reads in the
        ! initial conditions file, so the first coupling is solver
        ! feedback to heat injector, right?

        print *, "User Coupler Init returning"
   
    end subroutine user_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is coupled.
!   !
 
    subroutine user_run(comp, statelist, clock, rc)
        type(ESMF_CplComp) :: comp
        type(ESMF_State) :: statelist
        type(ESMF_Clock) :: clock
        integer :: rc

      ! Local variables
        type(ESMF_State) :: mysource, mydest
        type(ESMF_Field) :: temperature1, temperature2
        type(ESMF_DELayout) :: cpllayout

       
        integer :: status
        integer :: i, datacount
        character(len=ESMF_MAXSTR), dimension(datacount) :: datanames

        datanames(1) = "SIE"
        datanames(2) = "U"
        datanames(3) = "V"
        datanames(4) = "RHO"
        datanames(5) = "FLAG"

        print *, "User Coupler Run starting"

        ! Get import and export states
        call ESMF_StateGetData(statelist, "Heat Feedback", mysource, rc)
        call ESMF_StateGetData(statelist, "Heat Injection", mydest, rc)

        ! Get import and export states
        call ESMF_StateGetData(statelist, "Flow Source", mysource, rc)
        call ESMF_StateGetData(statelist, "Flow Feedback", mydest, rc)

        ! Get layout from coupler component
        call ESMF_CplCompGet(comp, layout=cpllayout, rc=status)

        do i=1, datacount

           ! check isneeded flag here
           if (.not. ESMF_StateIsNeeded(mysource, datanames(i), rc)) then 
               cycle
           endif

           call ESMF_StateGetData(mysource, datanames(i), srcfield, rc=status)
           call ESMF_FieldPrint(srcfield, "", rc=rc)

           call ESMF_StateGetData(mydest, datanames(i), dstfield, rc=status)
           call ESMF_FieldPrint(dstfield, "", rc=rc)


          ! These are fields on different layouts - call Route to rearrange
          !  the data using the Comm routines.
          call ESMF_FieldRoute(srcfield, dstfdield, cpllayout, status)


          ! Set export data in export state
          call ESMF_StateAddData(mydest, datanames(i), rc=status)

        enddo

        call ESMF_StatePrint(mydest, rc=status)

 
        print *, "User Coupler Run returning"

        rc = status

    end subroutine user_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
    subroutine user_final(comp, statelist, clock, rc)
        type(ESMF_CplComp) :: comp
        type(ESMF_State) :: statelist
        type(ESMF_Clock) :: clock
        integer :: rc

      ! Local variables
        type(ESMF_State) :: state1, state2

        print *, "User Coupler Final starting"
        call ESMF_StateGetData(statelist, "comp1 export", state1, rc)
        call ESMF_StatePrint(state1, rc=rc)

        call ESMF_StateGetData(statelist, "comp2 import", state2, rc)
        call ESMF_StatePrint(state2, rc=rc)
    
        print *, "User Coupler Final returning"
   
    end subroutine user_final


    end module user_coupler
    
!\end{verbatim}
    
