! $Id: user_coupler.F90,v 1.8 2004/03/18 23:17:50 nscollins Exp $
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

        rc = ESMF_SUCCESS

    end subroutine

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
    subroutine user_init(comp, importState, exportState, clock, rc)
        type(ESMF_CplComp) :: comp
        type(ESMF_State) :: importState, exportState
        type(ESMF_Clock) :: clock
        integer :: rc

        ! Local variables
        type(ESMF_Field) :: humidity1, humidity2
        type(ESMF_DELayout) :: cpllayout

        print *, "User Coupler Init starting"
        call ESMF_StateGetField(importState, "humidity", humidity1, rc=rc)
        call ESMF_FieldPrint(humidity1, rc=rc)

        call ESMF_StateGetField(exportState, "humidity", humidity2, rc=rc)
        call ESMF_FieldPrint(humidity2, rc=rc)

        ! Get layout from coupler component
        call ESMF_CplCompGet(comp, delayout=cpllayout, rc=rc)

        ! Precompute communication patterns
        call ESMF_FieldRedistStore(humidity1, humidity2, cpllayout, &
                                   routehandle, rc=rc)

        ! This is where the model specific setup code goes.  

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
        type(ESMF_Field) :: humidity1, humidity2

       
        integer :: status

        print *, "User Coupler Run starting"

        ! Get input data
        call ESMF_StateGetField(importState, "humidity", humidity1, rc=rc)
        call ESMF_FieldPrint(humidity1, "", rc=rc)

        ! Get location of output data
        call ESMF_StateGetField(exportState, "humidity", humidity2, rc=rc)
        call ESMF_FieldPrint(humidity2, "", rc=rc)

        ! These are fields on different layouts - call Redist to rearrange
        !  the data using the Comm routines.
        call ESMF_FieldRedist(humidity1, humidity2, routehandle, rc=status)


        ! Output data operated on in place, export state now has new values.
        call ESMF_StatePrint(exportState, rc=status)

 
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


        print *, "User Coupler Final starting"

        call ESMF_StatePrint(importState, rc=rc)
        call ESMF_StatePrint(exportState, rc=rc)
    
        call ESMF_FieldRedistRelease(routehandle)

        print *, "User Coupler Final returning"
   
        rc = ESMF_SUCCESS

    end subroutine user_final


    end module user_coupler
    
!\end{verbatim}
    
