! $Id: ESMF_CompCreate.F90,v 1.3 2003/03/04 15:02:18 nscollins Exp $
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
    use ESMF_IOMod
    use ESMF_LayoutMod
    use ESMF_CompMod
    
    implicit none
    
!   ! Local variables
    integer :: x, y, rc
    integer :: timestep
    integer, dimension(2) :: pelist
    character(ESMF_MAXSTR) :: cname
    type(ESMF_Layout) :: layout
    type(ESMF_Comp) :: comp1, comp2, comp3, comp4
        
!-------------------------------------------------------------------------
!   ! Test 1:
!   !
!   !  Quick Test - Create, Init, Run, Finalize, Destroy a Component.
 
    print *, "Component Test 1:"

    pelist = (/ 0, 1 /)
    layout = ESMF_LayoutCreate(2, 1, pelist, ESMF_XFAST, rc)

    cname = "Atmosphere"
    comp1 = ESMF_CompCreate(cname, layout=layout, ctype=ESMF_GRIDCOMP, &
                              mtype=ESMF_ATM, filepath="/usr/local", rc=rc)  

    print *, "Comp Create returned, name = ", trim(cname)

    call ESMF_CompInit(comp1, rc)
    print *, "Comp Init returned"


    call ESMF_CompPrint(comp1, rc)
    print *, "Comp Print returned"

    timestep = 1
    call ESMF_CompRun(comp1, timesteps=timestep, rc=rc)
    print *, "Comp Run returned"


    call ESMF_CompFinalize(comp1, rc)
    print *, "Comp Finalize returned"


    call ESMF_CompDestroy(comp1, rc)
    print *, "Comp Destroy returned"

    call ESMF_LayoutDestroy(layout, rc);
    print *, "Layout Destroy returned"

    print *, "Component Test 1 finished"


    end program ESMF_CompCreateTest
    
!\end{verbatim}
    
