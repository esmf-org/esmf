! $Id: ESMF_CompEx.F90,v 1.3 2003/02/04 21:11:16 nscollins Exp $
!
! Example/test code which creates a new Component

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! Generic calls to Component routines.  See the other example files for
!  more specific examples of Application, Gridded, and Coupler Components.
!  Also see the Programming Model section of this document.
!
!
!\begin{verbatim}

!   ! Example program showing calls to the Component routines.
    program ESMF_CompCreateEx
    
!   ! Some common definitions.  This requires the C preprocessor.
    #include "ESMF.h"

!   ! Other ESMF modules which are needed by Comps
    use ESMF_IOMod
    use ESMF_LayoutMod
    use ESMF_CompMod
    
    implicit none
    
!   ! Local variables
    integer :: x, y, rc
    integer :: timestep, endtime
    logical :: finished
    integer, dimension(8) :: delist
    character(ESMF_MAXSTR) :: cname
    type(ESMF_Layout) :: layout
    type(ESMF_Comp) :: comp1, comp2, comp3, comp4
        
!-------------------------------------------------------------------------
!   ! Example 1:
!   !
!   !  Create, Init, Run, Finalize, Destroy a Component.
 
    print *, "Component Example 1:"

    delist = (/ (i, i=0,7) /)
    layout = ESMF_LayoutCreate(4, 2, delist, ESMF_XFAST, rc)

    cname = "Atmosphere"
    comp1 = ESMF_CompCreate(cname, layout, ESMF_GRIDCOMP, &
                                       ESMF_ATM, "/usr/local", rc=rc)  

    print *, "Comp Create returned, name = ", trim(cname)

    call ESMF_CompInit(comp1, rc)
    print *, "Comp Init returned"


    finished = .false.
    timestep = 1
    endtime = 10
    do while (.not. finished) 
        call ESMF_CompRun(comp1, timestep, rc)
        print *, "Comp Run returned"
   
        timestep = timestep + 1
        if (timestep .gt. endtime) finished = .true.
    enddo
    print *, "Comp Run finished"



    call ESMF_CompFinalize(comp1, rc)
    print *, "Comp Finalize returned"


    call ESMF_CompDestroy(comp1, rc)
    print *, "Comp Destroy returned"

    call ESMF_LayoutDestroy(layout, rc);
    print *, "Layout Destroy returned"

    print *, "Component Example 1 finished"


    end program ESMF_CompCreateEx
    
!\end{verbatim}
    
