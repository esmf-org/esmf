! $Id: ESMF_CplEx.F90,v 1.4 2003/02/20 17:31:25 nscollins Exp $
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
    
!   ! Some common definitions.  This requires the C preprocessor.
    #include "ESMF.h"

!   ! Other ESMF modules which are needed by Comps
    use ESMF_IOMod
    use ESMF_LayoutMod
    use ESMF_CompMod
    
    implicit none

    public CPL_Register
    
    contains

        
!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
    subroutine CPL_Register(comp, rc)
        type(ESMF_Comp) :: comp
        integer :: rc

        ! Register the callback routines.
        call ESMF_CompSetRoutine(comp, "init", 1, CPL_Init, rc)
        call ESMF_CompSetRoutine(comp, "run", 1, CPL_Run, rc)
        call ESMF_CompSetRoutine(comp, "final", 1, CPL_Final, rc)

        ! If desired, this routine can register a private data block
        ! to be passed in to the routines above:
        ! call ESMF_CompSetData(comp, mydatablock, rc)

    end subroutine


!-------------------------------------------------------------------------
!   !  Coupler Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
    subroutine CPL_Init(comp, clock, rc)
        type(ESMF_Comp) :: comp
        type(ESMF_Clock) :: clock
        integer :: rc

!     ! Local variables
        type(ESMF_State), pointer :: statelist(:)

        print *, "Coupler Init starting"
    
        !call ESMF_CompGetState(comp, statelist, rc)

        ! Add whatever code here needed
        ! Precompute any needed values, fill in any inital values
        !  needed in Import States

        print *, "Coupler Init returning"
   
    end subroutine CPL_Init


!-------------------------------------------------------------------------
!   !  The Run routine where data is exchanged.
!   !
 
    subroutine CPL_Run(comp, clock, timestep, rc)
        type(ESMF_Comp) :: comp(:)
        type(ESMF_Clock) :: clock
        integer :: timestep
        intger :: rc

!     ! Local variables
        type(ESMF_State), pointer :: statelist(:)

        print *, "Coupler Run starting"

        !call ESMF_CompGetState(comp, statelist, rc)

        ! Add whatever code needed here to transform Export state data
        !  into Import states for the next timestep.  

        print *, "Coupler Run returning"

    end subroutine CPL_Run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
    subroutine CPL_Final(comp, clock, rc)
        type(ESMF_Comp) :: comp
        type(ESMF_Clock) :: clock
        integer :: rc

!     ! Local variables
        type(ESMF_State), pointer :: statelist(:)

        print *, "Coupler Final starting"
    
        !call ESMF_CompGetState(comp, statelist, rc)

        ! Add whatever code needed here to compute final values and
        !  finish the computation.

        print *, "Coupler Final returning"
   
    end subroutine CPL_Final


    print *, "Coupler Example 1 finished"


    end module ESMF_CouplerEx
    
!\end{verbatim}
    
