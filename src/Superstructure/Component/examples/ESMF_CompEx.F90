! $Id: ESMF_CompEx.F90,v 1.5 2003/02/20 21:52:02 nscollins Exp $
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
    type(ESMF_Clock) :: clock
    type(ESMF_Comp) :: comp1
        
!-------------------------------------------------------------------------
!   ! Example 1:
!   !
!   !  Create, Init, Run, Finalize, Destroy a Component.
 
    print *, "Component Example 1:"

    delist = (/ (i, i=0,7) /)
    layout = ESMF_LayoutCreate(4, 2, delist, ESMF_XFAST, rc)

    call ESMF_ClockInit()

    cname = "Atmosphere"
    comp1 = ESMF_CompCreate(cname, layout, ESMF_GRIDCOMP, &
                                ESMF_ATM, clock, "/usr/local", rc=rc)  

    print *, "Comp Create returned, name = ", trim(cname)

    ! This sets which internal subroutines will be called
    !  for Init, Run, and Finalize.
    call ESMF_CompRegister(comp1, ATM_Register, rc)


    call ESMF_CompInit(comp1, clock, rc)
    print *, "Comp Init returned"


    finished = .false.
    timesteps = 1
    ! Query clock for end time
    call ESMF_ClockGet(clock, endtime=endtime)

    do while (.not. finished) 
        call ESMF_CompRun(comp1, clock, timesteps, rc)
        print *, "Comp Run returned"
   
        call ESMF_ClockAdvance(clock, timesteps)
        call ESMF_ClockGet(clock, time=currenttime)
        if (currenttime .gt. endtime) finished = .true.
    enddo
    print *, "Comp Run finished"



    call ESMF_CompFinalize(comp1, clock, rc)
    print *, "Comp Finalize returned"


    call ESMF_CompDestroy(comp1, rc)
    print *, "Comp Destroy returned"

    call ESMF_LayoutDestroy(layout, rc);
    print *, "Layout Destroy returned"

    print *, "Component Example 1 finished"


    end program ESMF_CompCreateEx
    
!\end{verbatim}
    
