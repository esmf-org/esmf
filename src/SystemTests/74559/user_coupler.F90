! $Id: user_coupler.F90,v 1.3 2003/04/24 16:52:18 nscollins Exp $
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
        call ESMF_StateGetData(statelist, "Heat Energy", toflow_sie, rc)
        call ESMF_StatePrint(toflow_sie, rc=rc)

        call ESMF_StateGetData(statelist, "Heat U Velocity", toflow_u, rc)
        call ESMF_StatePrint(toflow_u, rc=rc)

        call ESMF_StateGetData(statelist, "Heat V Velocity", toflow_v, rc)
        call ESMF_StatePrint(toflow_v, rc=rc)

        call ESMF_StateGetData(statelist, "Flow Energy", fromflow_sie, rc)
        call ESMF_StatePrint(fromflow_sie, rc=rc)

        call ESMF_StateGetData(statelist, "Flow U Velocity", fromflow_u, rc)
        call ESMF_StatePrint(fromflow_u, rc=rc)

        call ESMF_StateGetData(statelist, "Flow V Velocity", fromflow_v, rc)
        call ESMF_StatePrint(fromflow_v, rc=rc)



        ! This is where the model specific setup code goes.  

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

        print *, "User Coupler Run starting"

        ! Get input data
        call ESMF_StateGetData(statelist, "Heat Source", mysource, rc)
        call ESMF_StatePrint(mysource, rc=rc)
        call ESMF_StateGetData(mysource, "temperature", temperature1, rc=status)
        call ESMF_FieldPrint(temperature1, "", rc=rc)

        ! Get location of output data
        call ESMF_StateGetData(statelist, "Heat Injection", mydest, rc)
        call ESMF_StatePrint(mydest, rc=rc)
        call ESMF_StateGetData(mydest, "temperature", temperature2, rc=status)
        call ESMF_FieldPrint(temperature2, "", rc=rc)

        ! Get layout from coupler component
        call ESMF_CplCompGet(comp, layout=cpllayout, rc=status)

        ! Need to get temp2 on temp1 layout to add?

        ! These are fields on different layouts - call Route to rearrange
        !  the data using the Comm routines.
        call ESMF_FieldRoute(temperature1, temperature2, cpllayout, status)


        ! Set output data
        call ESMF_StateAddData(mydest, temperature2, rc=status)
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
    
