! $Id: ESMF_CplEx.F90,v 1.3 2003/02/04 21:11:16 nscollins Exp $
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
    
    contains

        
!-------------------------------------------------------------------------
!   !  Coupler Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
    subroutine CPL_Init(comp, rc)
        type(ESMF_Comp) :: comp
        integer :: rc

!     ! Local variables
        type(ESMF_State), pointer :: statelist(:)

        print *, "Coupler Init starting"
    
        !call ESMF_CompGetState(comp, statelist, rc)
        ! TODO: add whatever code here needed

        print *, "Coupler Init returning"
   
    end subroutine CPL_Init


!-------------------------------------------------------------------------
!   !  The Run routine where data is exchanged.
!   !
 
    subroutine CPL_Run(comp, timestep, rc)
        type(ESMF_Comp) :: comp(:)
        integer :: timestep
        intger :: rc

        print *, "Comp Run starting"

        ! TODO: add code here

        print *, "Comp Run returning"

    end subroutine CPL_Run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
    subroutine CPL_Final(comp, rc)
        type(ESMF_Comp) :: comp
        integer :: rc

!     ! Local variables
        type(ESMF_State), pointer :: statelist(:)

        print *, "Coupler Final starting"
    
        !call ESMF_CompGetState(comp, statelist, rc)
        ! TODO: add whatever code here needed

        print *, "Coupler Final returning"
   
    end subroutine CPL_Final


    print *, "Coupler Example 1 finished"


    end module ESMF_CouplerEx
    
!\end{verbatim}
    
