! $Id: ESMF_GCompEx.F90,v 1.4 2003/02/20 17:31:25 nscollins Exp $
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
    use ESMF_ClockMod
    use ESMF_CompMod
    
    implicit none
    
    public GCOMP_Register

    contains

        
!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
    subroutine GCOMP_Register(comp, rc)
        type(ESMF_Comp) :: comp
        integer :: rc

        ! Register the callback routines.
        call ESMF_CompSetRoutine(comp, "init", 1, GCOMP_Init, rc)
        call ESMF_CompSetRoutine(comp, "run", 1, GCOMP_Run, rc)
        call ESMF_CompSetRoutine(comp, "final", 1, GCOMP_Final, rc)

        ! If desired, this routine can register a private data block
        ! to be passed in to the routines above:
        ! call ESMF_CompSetData(comp, mydatablock, rc)

    end subroutine

!-------------------------------------------------------------------------
!   !  Gridded Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
    subroutine GCOMP_Init(comp, clock, rc)
        type(ESMF_Comp) :: comp
        type(ESMF_Clock) :: clock
        integer :: rc

!     ! Local variables
        type(ESMF_State), pointer :: import, export

        print *, "Gridded Comp Init starting"
    

        ! This is where the model specific setup code goes.  

        call ESMF_CompGetState(comp, ESMF_STATEEXPORT, export, rc)
 
        ! If the initial Export state needs to be filled, do it here.

        print *, "Gridded Comp Init returning"
   
    end subroutine GCOMP_Init


!-------------------------------------------------------------------------
!   !  The Run routine where data is exchanged.
!   !
 
    subroutine GCOMP_Run(comp, clock, timestep, rc)
        type(ESMF_Comp) :: comp(:)
        type(ESMF_Clock) :: clock
        integer :: timestep
        intger :: rc

!     ! Local variables
        type(ESMF_State), pointer :: import, export

        print *, "Gridded Comp Run starting"

        call ESMF_CompGetState(comp, ESMF_STATEIMPORT, import, rc)
        ! call ESMF_StateGetData() to get fields, bundles, arrays

        ! This is where the model specific computation goes.


        call ESMF_CompGetState(comp, ESMF_STATEEXPORT, export, rc)
 
        ! Fill export state here.

        print *, "Gridded Comp Run returning"

    end subroutine GCOMP_Run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
    subroutine GCOMP_Final(comp, clock, rc)
        type(ESMF_Comp) :: comp
        type(ESMF_Clock) :: clock
        integer :: rc

!     ! Local variables

        print *, "Gridded Comp Final starting"
    
        ! Add whatever code here needed

        print *, "Gridded Comp Final returning"
   
    end subroutine GCOMP_Final


    print *, "Gridded Comp Example 1 finished"


    end module ESMF_GriddedCompEx
    
!\end{verbatim}
    
