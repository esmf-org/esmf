! $Id: user_coupler.F90,v 1.9 2009/03/23 20:40:48 theurich Exp $
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

        call ESMF_CplCompSetEntryPoint(comp, ESMF_SETINIT, user_init, rc=rc)
        call ESMF_CplCompSetEntryPoint(comp, ESMF_SETRUN, user_run, rc=rc)
        call ESMF_CplCompSetEntryPoint(comp, ESMF_SETFINAL, user_final, rc=rc)

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
        integer :: itemcount
        real, dimension(:,:), pointer :: mydata

        ! This is where the model specific setup code goes.  
        print *, "User Coupler Init starting"

        ! Examples of info routines
        call ESMF_StateGet(importState, itemcount=itemcount, rc=rc)
        print *, "Import State contains ", itemcount, " items."

        call ESMF_StatePrint(importState, rc=rc)

        call ESMF_StateGet(exportState, itemcount=itemcount, rc=rc)
        print *, "Export State contains ", itemcount, " items."

        call ESMF_StatePrint(exportState, rc=rc)

        ! direct access to field contents
        call ESMF_StateGetDataPointer(importState, "humidity", mydata, rc=rc)

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
        type(ESMF_DELayout) :: cpllayout
        integer :: status

        print *, "User Coupler Run starting"

        ! Since we are planning to use a communication call, we must
        ! reconcile the object lists in the State objects.

        ! New routine:
        !!call ESMF_StateReconcile(importState, rc=rc)
        !!call ESMF_StateReconcile(exportState, rc=rc)

        ! Get input data
        call ESMF_StateGetField(importState, "humidity", humidity1, rc=rc)
        call ESMF_FieldPrint(humidity1, rc=status)

        ! Get location of output data
        call ESMF_StateGetField(exportState, "humidity", humidity2, rc=rc)
        call ESMF_FieldPrint(humidity2, rc=status)

        ! Get layout from coupler component
        call ESMF_CplCompGet(comp, layout=cpllayout, rc=status)


        ! These are fields on different layouts - call Redist to rearrange
        !  the data using the Comm routines.
        call ESMF_FieldRedist(humidity1, humidity2, cpllayout, rc=status)


        ! Output data is accessed in place, so no "put" is needed.

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

        print *, "User Coupler Final starting"

        call ESMF_StatePrint(importState, rc=rc)
        call ESMF_StatePrint(exportState, rc=rc)
    
        print *, "User Coupler Final returning"
   
        rc = ESMF_SUCCESS

    end subroutine user_final


    end module user_coupler
    
!\end{verbatim}
    
