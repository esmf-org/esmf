! $Id: ESMF_GCompEx.F90,v 1.3 2003/02/04 21:11:17 nscollins Exp $
!
! Example/test code which shows Gridded Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  One of many possible examples of a Gridded component.
!  Also see the Programming Model section of this document.
!
!
!\begin{verbatim}

!   ! Example module showing Gridded Comp calls to the Component routines.
    module ESMF_GriddedCompEx
    
!   ! Some common definitions.  This requires the C preprocessor.
    #include "ESMF.h"

!   ! Other ESMF modules which are needed by Comps
    use ESMF_IOMod
    use ESMF_LayoutMod
    use ESMF_CompMod
    
    implicit none
    
    contains

        
!-------------------------------------------------------------------------
!   !  Gridded Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
    subroutine GCOMP_Init(comp, rc)
        type(ESMF_Comp) :: comp
        integer :: rc

!     ! Local variables
        type(ESMF_State), pointer :: import, export

        print *, "Gridded Comp Init starting"
    
        !call ESMF_CompGetState(comp, ESMF_STATEIMPORT, import, rc)
        !call ESMF_CompGetState(comp, ESMF_STATEEXPORT, export, rc)
        ! TODO: add whatever code here needed

        print *, "Gridded Comp Init returning"
   
    end subroutine GCOMP_Init


!-------------------------------------------------------------------------
!   !  The Run routine where data is exchanged.
!   !
 
    subroutine GCOMP_Run(comp, timestep, rc)
        type(ESMF_Comp) :: comp(:)
        integer :: timestep
        intger :: rc

        print *, "Comp Run starting"

        ! TODO: add code here
        !  take data from import state, compute, fill in export state & return

        print *, "Comp Run returning"

    end subroutine GCOMP_Run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
    subroutine GCOMP_Final(comp, rc)
        type(ESMF_Comp) :: comp
        integer :: rc

!     ! Local variables

        print *, "Gridded Comp Final starting"
    
        ! TODO: add whatever code here needed

        print *, "Gridded Comp Final returning"
   
    end subroutine GCOMP_Final


    print *, "Gridded Comp Example 1 finished"


    end module ESMF_GriddedCompEx
    
!\end{verbatim}
    
