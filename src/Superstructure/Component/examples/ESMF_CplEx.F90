! $Id: ESMF_CplEx.F90,v 1.8 2004/01/08 21:07:11 nscollins Exp $
!
! Example/test code which shows Coupler Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  One of many possible examples of a Coupler component.
!  Also see the Programming Model section of this document.
!
!
!\begin{verbatim}

!   ! Example module showing Coupler calls to the Component routines.
    module ESMF_CouplerEx
    
!   ! ESMF Framework module
    use ESMF_Mod
    
    implicit none

    public CPL_SetServices
    
    contains

        
!-------------------------------------------------------------------------
!   !  The SetServices routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
    subroutine CPL_SetServices(comp, rc)
        type(ESMF_CplComp) :: comp
        integer :: rc

        ! SetServices the callback routines.
        call ESMF_CplCompSetEntryPoint(comp, ESMF_SETINIT, CPL_Init, 0, rc)
        call ESMF_CplCompSetEntryPoint(comp, ESMF_SETRUN, CPL_Run, 0, rc)
        call ESMF_CplCompSetEntryPoint(comp, ESMF_SETFINAL, CPL_Final, 0, rc)

        ! If desired, this routine can register a private data block
        ! to be passed in to the routines above:
        ! call ESMF_CplCompSetInternalState(comp, mydatablock, rc)

    end subroutine


!-------------------------------------------------------------------------
!   !  Coupler Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
    subroutine CPL_Init(comp, statelist, clock, rc)
        type(ESMF_CplComp) :: comp
        type(ESMF_State), dimension(:) :: statelist
        type(ESMF_Clock) :: clock
        integer :: rc

        print *, "Coupler Init starting"
    
        ! Add whatever code here needed
        ! Precompute any needed values, fill in any inital values
        !  needed in Import States

        call ESMF_StatePrint(statelist(1), rc=rc)
        call ESMF_StatePrint(statelist(2), rc=rc)
        call ESMF_StatePrint(statelist(3), rc=rc)

        print *, "Coupler Init returning"
   
    end subroutine CPL_Init


!-------------------------------------------------------------------------
!   !  The Run routine where data is exchanged.
!   !
 
    subroutine CPL_Run(comp, statelist, clock, rc)
        type(ESMF_CplComp) :: comp
        type(ESMF_State), dimension(:) :: statelist
        type(ESMF_Clock) :: clock
        integer :: rc

        print *, "Coupler Run starting"

        ! Add whatever code needed here to transform Export state data
        !  into Import states for the next timestep.  

        call ESMF_StatePrint(statelist(1), rc=rc)
        call ESMF_StatePrint(statelist(2), rc=rc)
        call ESMF_StatePrint(statelist(3), rc=rc)

        print *, "Coupler Run returning"

    end subroutine CPL_Run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
    subroutine CPL_Final(comp, statelist, clock, rc)
        type(ESMF_CplComp) :: comp
        type(ESMF_State), dimension(:) :: statelist
        type(ESMF_Clock) :: clock
        integer :: rc

        print *, "Coupler Final starting"
    
        ! Add whatever code needed here to compute final values and
        !  finish the computation.

        call ESMF_StatePrint(statelist(1), rc=rc)
        call ESMF_StatePrint(statelist(2), rc=rc)
        call ESMF_StatePrint(statelist(3), rc=rc)

        print *, "Coupler Final returning"
   
    end subroutine CPL_Final



    end module ESMF_CouplerEx
    
!\end{verbatim}
    
