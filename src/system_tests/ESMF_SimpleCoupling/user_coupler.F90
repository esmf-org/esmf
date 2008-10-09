! $Id: user_coupler.F90,v 1.15 2008/10/09 19:25:45 feiliu Exp $
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

#ifdef ESMF_TESTWITHTHREADS
        ! The following call will turn on ESMF-threading (single threaded)
        ! for this component. If you are using this file as a template for 
        ! your own code development you probably don't want to include the 
        ! following call unless you are interested in exploring ESMF's 
        ! threading features.
        call ESMF_CplCompSetVMMinThreads(comp, rc=rc)
#endif

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
        type(ESMF_VM) :: vm
        integer :: status

        print *, "User Coupler Init starting"
        call ESMF_StateGet(importState, "humidity", humidity1, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10 
        call ESMF_FieldPrint(humidity1, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10 

        call ESMF_StateGet(exportState, "humidity", humidity2, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10 
        call ESMF_FieldPrint(humidity2, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10 

        ! Get VM from coupler component to use in computing the redist
        call ESMF_CplCompGet(comp, vm=vm, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10 

        ! Precompute communication patterns
        call ESMF_FieldRedistStore(humidity1, humidity2, &
                                   routehandle=routehandle, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10 

        ! This is where the model specific setup code goes.  

        print *, "User Coupler Init returning"
   
10 continue
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
        call ESMF_StateGet(importState, "humidity", humidity1, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10 
        call ESMF_FieldPrint(humidity1, "", rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10 

        ! Get location of output data
        call ESMF_StateGet(exportState, "humidity", humidity2, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10 
        call ESMF_FieldPrint(humidity2, "", rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10 

        ! These are fields on different layouts - call Redist to rearrange
        !  the data using the Comm routines.
        call ESMF_FieldRedist(humidity1, humidity2, routehandle, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10 


        ! Output data operated on in place, export state now has new values.
        call ESMF_StatePrint(exportState, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10 

 
        print *, "User Coupler Run returning"

10 continue
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
    
