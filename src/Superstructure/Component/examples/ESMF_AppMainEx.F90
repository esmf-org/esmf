! $Id: ESMF_AppMainEx.F90,v 1.3 2003/02/04 21:11:16 nscollins Exp $
!
! Example code for a main program Application.  See ESMF_AppCompEx.F90
!   for an example of an embeddable Application.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! Example of a main program Application.  See also the following example
!  for an Application which can be nested in a higher-level Application.
!  Also see the Programming Model section of this document.
!
!
!\begin{verbatim}

!   ! Example main program showing calls to the Component routines.
!   ! This version is not nestable inside a larger application.
    program ESMF_AppMainEx
    
!   ! Some common definitions.  This requires the C preprocessor.
    #include "ESMF.h"

!   ! Other ESMF modules which are needed by Comps
    use ESMF_IOMod
    use ESMF_LayoutMod
    use ESMF_CompMod
    
    implicit none
    
!   ! Local variables
    integer :: x, y, rc
    integer :: timestep, timeend
    logical :: finished
    integer :: delist1(8), delist2(8), delist3(2)
    character(ESMF_MAXSTR) :: cname
    type(ESMF_Layout) :: layout1, layout2, layout3
    type(ESMF_Comp) :: comp1, comp2, cpl
        
!-------------------------------------------------------------------------
!   !
!   !  Create, Init, Run, Finalize, Destroy Components.
 
    print *, "Application Example 1:"

    delist1 = (/ (i, i=0,7) /)
    layout1 = ESMF_LayoutCreate(2, 4, delist1, ESMF_XFAST, rc)

    cname = "Atmosphere Physics"
    comp1 = ESMF_CompCreate(cname, layout1, ESMF_GRIDCOMP, &
                                       ESMF_ATM, "/usr/local", rc=rc)  

    call ESMF_CompSetRoutine(comp1, "init", PHYS_Init)
    call ESMF_CompSetRoutine(comp1, "run", PHYS_Run)
    call ESMF_CompSetRoutine(comp1, "final", PHYS_Final)

    print *, "Comp Create returned, name = ", trim(cname)

    delist2 = (/ (i, i=8,15) /)
    layout2 = ESMF_LayoutCreate(2, 4, delist2, ESMF_XFAST, rc)

    cname = "Atmosphere Dynamics"
    comp2 = ESMF_CompCreate(cname, layout2, ESMF_GRIDCOMP, &
                                       ESMF_ATM, "/usr/local", rc=rc)  

    call ESMF_CompSetRoutine(comp2, "init", DYN_Init)
    call ESMF_CompSetRoutine(comp2, "run", DYN_Run)
    call ESMF_CompSetRoutine(comp2, "final", DYN_Final)

    print *, "Comp Create returned, name = ", trim(cname)

    delist3 = (/ (i, i=16,19) /)
    layout3 = ESMF_LayoutCreate(4, 1, delist3, ESMF_XFAST, rc)

    cname = "Atmosphere Coupler"
    cpl = ESMF_CompCreate(cname, layout3, ESMF_CPLCOMP, &
                                       ESMF_ATM, "/usr/local", rc=rc)  

    call ESMF_CompSetRoutine(cpl, "init", CPL_Init)
    call ESMF_CompSetRoutine(cpl, "run", CPL_Run)
    call ESMF_CompSetRoutine(cpl, "final", CPL_Final)

    print *, "Comp Create returned, name = ", trim(cname)


    call ESMF_CompInit(comp1, rc)
    call ESMF_CompInit(comp2, rc)
    call ESMF_CompInit(cpl, rc)
    print *, "Comp Init returned"


    finished = .false.
    timestep = 1
    endtime = 10
    do while (.not. finished)
        call ESMF_CompRun(comp1, timestep, rc)
        call ESMF_CompRun(comp2, timestep, rc)
        call ESMF_CompRun(cpl, timestep, rc)
        print *, "Comp Run returned"
   
        if (timestep .gt. endtime) finished = .true.
    enddo


    call ESMF_CompFinalize(comp1, rc)
    call ESMF_CompFinalize(comp2, rc)
    call ESMF_CompFinalize(cpl, rc)
    print *, "Comp Finalize returned"


    call ESMF_CompDestroy(comp1, rc)
    call ESMF_CompDestroy(comp2, rc)
    call ESMF_CompDestroy(cpl, rc)
    print *, "Comp Destroy returned"


    print *, "Application Example 1 finished"


    end program ESMF_AppMainEx
    
!\end{verbatim}
    
