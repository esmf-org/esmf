! $Id: ESMF_CompCreateEx.F90,v 1.1 2003/01/09 21:45:50 nscollins Exp $
!
! Example/test code which creates a new Component

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! See the following code fragments for examples of how to create new Components.
! Also see the Programming Model section of this document.
!
!
!\begin{verbatim}

!   ! Example program showing various ways to create a Comp object
    program ESMF_CompCreateEx
    
!   ! Some common definitions.  This requires the C preprocessor.
#include "ESMF.h"

!   ! Other ESMF modules which are needed by Comps
    use ESMF_IOMod
    use ESMF_LayoutMod
    use ESMF_ComponentMod
    
    implicit none
    
!   ! Local variables
    integer :: x, y, rc
    integer :: timestep
    integer, dimension(2) :: delist
    character(ESMF_MAXSTR) :: cname
    type(ESMF_Layout) :: layout
    type(ESMF_Component) :: comp1, comp2, comp3, comp4
        
!-------------------------------------------------------------------------
!   ! Example 1:
!   !
!   !  Create, Init, Run, Finalize, Destroy a Component.
 
    print *, "Component Example 1:"

    delist = (/ 0, 1 /)
    layout = ESMF_LayoutCreate(2, 1, delist, ESMF_XFAST, rc)

    cname = "Atmosphere"
    comp1 = ESMF_ComponentCreate(cname, layout, ESMF_GRIDCOMP, &
                                       ESMF_ATM, "/usr/local", rc=rc)  

    print *, "Comp Create returned, name = ", trim(cname)

    call ESMF_ComponentInit(comp1, rc)
    print *, "Comp Init returned"


    !call ESMF_ComponentPrint(comp1, rc)
    !print *, "Comp Print returned"

    timestep = 1
    call ESMF_ComponentRun(comp1, timestep, rc)
    print *, "Comp Run returned"


    call ESMF_ComponentFinalize(comp1, rc)
    print *, "Comp Finalize returned"


    call ESMF_ComponentDestroy(comp1, rc)
    print *, "Comp Destroy returned"

    print *, "Component Example 1 finished"


    end program ESMF_CompCreateEx
    
!\end{verbatim}
    
