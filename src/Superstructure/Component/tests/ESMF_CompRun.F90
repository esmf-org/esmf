! $Id: ESMF_CompRun.F90,v 1.1 2003/04/01 23:51:19 nscollins Exp $
!
! Test code which creates a new Component.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! Tests, cursory and exahustive, for Component Create code.
!
!
!\begin{verbatim}

    program ESMF_CompCreateTest
    
!   ! Some common definitions.  This requires the C preprocessor.
#include "ESMF.h"

!   ! Other ESMF modules which are needed by Comps
    use ESMF_BaseMod
    use ESMF_CompMod
    use ESMF_GridMod
    use ESMF_DELayoutMod
    use ESMF_ClockMod

    use UserGridCompMod, only: User_SetServices
    
    implicit none
    
!   ! Local variables
    integer :: rc
    character(ESMF_MAXSTR) :: cname
    type(ESMF_GridComp) :: comp1
    type(ESMF_Clock) :: clock
        
!-------------------------------------------------------------------------
!   ! Test 1:
!   !
!   !  Quick Test - Create, Init, Run, Finalize, Destroy a Component.
 
    print *, "Component Test 1:"

    cname = "Atmosphere"
    comp1 = ESMF_GridCompCreate(name=cname, mtype=ESMF_ATM, &
                                               configfile="grid.rc", rc=rc)  

    print *, "Grid Comp Create returned, name = ", trim(cname)

    call ESMF_GridCompPrint(comp1, rc=rc)
    print *, "Grid Comp Print returned"

    ! register other entry points
    call ESMF_GridCompSetServices(comp1, User_SetServices, rc)

    call ESMF_GridCompInitialize(comp1, rc=rc)
    print *, "Grid Comp Initialize returned"

    call ESMF_GridCompRun(comp1, clock=clock, rc=rc)
    print *, "Grid Comp Run returned"

    call ESMF_GridCompRun(comp1, clock=clock, rc=rc)
    print *, "Grid Comp Run returned"

    call ESMF_GridCompFinalize(comp1, rc=rc)
    print *, "Grid Comp Finalize returned"

    call ESMF_GridCompDestroy(comp1, rc=rc)
    print *, "Grid Comp Destroy returned"

    print *, "Component Test 1 finished"


    end program ESMF_CompCreateTest
    
!\end{verbatim}
    
