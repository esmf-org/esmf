! $Id: ESMF_AppCompEx.F90,v 1.1 2003/02/03 17:09:50 nscollins Exp $
!
! Example code for an embeddable Application.  See ESMF_AppMainEx.F90
!   for an example of a main program Application.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! This is an Application which calls Components, but is also a Component
!   in a higher level Application.
!  Also see the Programming Model section of this document.
!
!
!\begin{verbatim}

!   ! Example program showing an Application which is also a Component
!   !  in a higher level Application.
    program ESMF_AppCompEx
    
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
    integer, dimension(2) :: delist
    character(ESMF_MAXSTR) :: cname
    type(ESMF_Layout) :: layout
    type(ESMF_Comp) :: comp1, comp2, comp3, comp4
        
! !TODO: UPDATE THIS 
!-------------------------------------------------------------------------
!   ! Example 1:
!   !
!   !  Create, Init, Run, Finalize, Destroy a Component.
 
    print *, "Component Example 1:"

    delist = (/ 0, 1 /)
    layout = ESMF_LayoutCreate(2, 1, delist, ESMF_XFAST, rc)

    cname = "Atmosphere"
    comp1 = ESMF_CompCreate(cname, layout, ESMF_GRIDCOMP, &
                                       ESMF_ATM, "/usr/local", rc=rc)  

    print *, "Comp Create returned, name = ", trim(cname)

    call ESMF_CompInit(comp1, rc)
    print *, "Comp Init returned"


    !call ESMF_CompPrint(comp1, rc=rc)
    !print *, "Comp Print returned"

    timestep = 1
    call ESMF_CompRun(comp1, timestep, rc)
    print *, "Comp Run returned"


    call ESMF_CompFinalize(comp1, rc)
    print *, "Comp Finalize returned"


    call ESMF_CompDestroy(comp1, rc)
    print *, "Comp Destroy returned"

    call ESMF_LayoutDestroy(layout, rc);
    print *, "Layout Destroy returned"

    print *, "Component Example 1 finished"


    end program ESMF_AppCompEx
    
!\end{verbatim}
    
