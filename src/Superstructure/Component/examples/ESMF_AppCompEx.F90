! $Id: ESMF_AppCompEx.F90,v 1.4 2003/02/18 22:00:10 nscollins Exp $
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
!   !  in a higher level Application.   Note this is a module, and requires
!   !  the Driver program below to run standalone.

    module ESMF_AppCompEx
    
!   ! Some common definitions.  This requires the C preprocessor.
    #include "ESMF.h"

!   ! Other ESMF modules which are needed by Comps
    use ESMF_BaseMod
    use ESMF_IOMod
    use ESMF_LayoutMod
    use ESMF_CompMod
    
    implicit none
    

    contains

        
!-------------------------------------------------------------------------
!   !  The Init routine creates subcomponents and arranges for their 
!   !   init, run, and finalize routines to be registered.
 
    subroutine NATM_Init(comp, layout, rc)
        type(ESMF_Comp) :: comp
        type(ESMF_Layout) :: layout
        integer :: rc


        type(ESMF_Comp) :: comp1, comp2, cpl
        type(ESMF_Layout) :: layout1, layout2, layout3
        integer :: i, delist1(16), delist2(16), delist3(2)
        character(len=ESMF_MAXSTR) :: cname

        print *, "Nested Comp Init starting"
    
        delist1 = (/ (i, i=0,15) /)
        layout1 = ESMF_LayoutCreate(4, 4, delist1, ESMF_XFAST, rc)
    
        delist2 = (/ (i, i=16,31) /)
        layout2 = ESMF_LayoutCreate(4, 4, delist2, ESMF_XFAST, rc)
   
        delist3 = (/ 0, 16 /)
        layout3 = ESMF_LayoutCreate(2, 1, delist3, ESMF_XFAST, rc)
    

        cname = "Atmosphere Physics"
        comp1 = ESMF_CompCreate(cname, layout1, ESMF_GRIDCOMP, &
                                           ESMF_ATM, "/usr/local", rc=rc)  
    
        print *, "Internal Comp Create returned, name = ", trim(cname)
    
        ! this registers the Init, Run, and Final routines
        call PHYS_CompSetup(comp1)
     
        !! the setup routine above will make the following 3 calls,
        !!  allowing PHYS_Init, PHYS_Run, and PHYS_Final to remain
        !!  private entry points and not public. 
        !! call ESMF_CompSetRoutine(comp1, "init", PHYS_Init)
        !! call ESMF_CompSetRoutine(comp1, "run", PHYS_Run)
        !! call ESMF_CompSetRoutine(comp1, "final", PHYS_Final)

    
        cname = "Atmosphere Dynamics"
        comp2 = ESMF_CompCreate(cname, layout2, ESMF_GRIDCOMP, &
                                           ESMF_ATM, "/usr/local", rc=rc)  
    
        print *, "Internal Comp Create returned, name = ", trim(cname)
    
        ! this registers the Init, Run, and Final routines
        call DYNM_CompSetup(comp1)
     
        !! the setup routine above will make the following 3 calls,
        !!  allowing DYNM_Init, DYNM_Run, and DYNM_Final to remain
        !!  private entry points and not public. 
        !! call ESMF_CompSetRoutine(comp1, "init", DYNM_Init)
        !! call ESMF_CompSetRoutine(comp1, "run", DYNM_Run)
        !! call ESMF_CompSetRoutine(comp1, "final", DYNM_Final)


        cname = "PhysDyn Coupler"
        cpl = ESMF_CompCreate(cname, layout3, ESMF_CPLCOMP, &
                                           ESMF_ATM, "/usr/local", rc=rc)  
    
        print *, "Internal Comp Create returned, name = ", trim(cname)
    
        ! this registers the Init, Run, and Final routines
        call CPLR_CompSetup(comp1)
     
        !! the setup routine above will make the following 3 calls,
        !!  allowing CPLR_Init, CPLR_Run, and CPLR_Final to remain
        !!  private entry points and not public. 
        !! call ESMF_CompSetRoutine(comp1, "init", CPLR_Init)
        !! call ESMF_CompSetRoutine(comp1, "run", CPLR_Run)
        !! call ESMF_CompSetRoutine(comp1, "final", CPLR_Final)

     
        ! Do any other initialization needed here for the main component, 
        !  and then call nested init routines.
        call ESMF_CompInit(comp1, rc)
        call ESMF_CompInit(comp2, rc)
        call ESMF_CompInit(cpl, rc)

        print *, "Nested Comp Init returning"
    end subroutine NATM_Init

!-------------------------------------------------------------------------
!   !  The Run routine runs the internal time loop and returns at the
!   !   end of the timestep it was given.
 
    ! TODO: where does the internal context come from?
    subroutine NATM_Run(comp, timestep, rc)
        type(ESMF_Comp) :: comp
        integer :: timestep
        integer :: rc

        logical :: finished
        integer :: internaltimestep, internalendtime, starttime
        type(ESMF_Comp) :: comp1, comp2, cpl

        print *, "Nested Comp Run starting"

        finished = .false.
        starttime = 0
    
        internaltimestep = 1
        internalendtime = starttime + timestep
        do while (.not. finished) 
            call ESMF_CompRun(comp1, internaltimestep, rc)
            call ESMF_CompRun(comp2, internaltimestep, rc)
            call ESMF_CompRun(cpl, internaltimestep, rc)
            print *, "Nested Comp Run returned"
        
            internaltimestep = internaltimestep + 1
            if (internaltimestep .gt. internalendtime) finished = .true.
        enddo
    
        print *, "Nested Comp Run returning"
    end subroutine NATM_Run

!-------------------------------------------------------------------------
!   !  The Finalize routine gives each internal component a chance to clean
!   !   up and flush any output, and then destroys them before returning.
 
    subroutine NATM_Final(comp, rc)
        type(ESMF_Comp) :: comp
        integer :: rc

        type(ESMF_Comp) :: comp1, comp2, cpl

        print *, "Nested Comp Finalize starting"
        call ESMF_CompFinal(cpl, rc)
        call ESMF_CompFinal(comp1, rc)
        call ESMF_CompFinal(comp2, rc)
 
        call ESMF_CompDestroy(comp1, rc)
        call ESMF_CompDestroy(comp2, rc)
        call ESMF_CompDestroy(cpl, rc)

        print *, "Nested Comp Finalize returning"
    end subroutine NATM_Final


    end module ESMF_AppCompEx
    


!=========================================================================
!   ! Here is a sample driver program if you want to run the above 
!   !   code standalone.

    program ESMF_AppCompMain
    
!   ! Some common definitions.  This requires the C preprocessor.
    #include "ESMF.h"

!   ! Other ESMF modules which are needed by Comps
    use ESMF_IOMod
    use ESMF_LayoutMod
    use ESMF_CompMod
    
    implicit none
    
!   ! Local variables
    integer :: x, y, i, rc
    integer :: timestep, endtime
    integer, dimension(32) :: delist
    logical :: finished
    character(len=ESMF_MAXSTR) :: cname
    type(ESMF_Layout) :: layout
    type(ESMF_Comp) :: comp1, comp2, comp3, comp4
        
!-------------------------------------------------------------------------
!   ! Example 1:
!   !
!   !  Create the only layout, and create the component, and call the
!   !   various subroutines.
 
    print *, "Component Example Driver 1:"

    delist = (/ (i, i=0,31) /)
    layout = ESMF_LayoutCreate(8, 4, delist, ESMF_XFAST, rc)

    cname = "Nested Atmosphere"
    comp1 = ESMF_CompCreate(cname, layout, ESMF_APPCOMP+ESMF_GRIDCOMP, &
                                       ESMF_ATM, "/usr/local", rc=rc)  

    print *, "Driver Comp Create returned, name = ", trim(cname)

    ! this registers the Init, Run, and Final routines
    ! it must be a public method.
    call NATM_CompSetup(comp1)
     
    !! the setup routine above will make the following 3 calls,
    !!  allowing NATM_Init, NATM_Run, and NATM_Final to remain
    !!  private entry points and not public. 
    !! call ESMF_CompSetRoutine(comp1, "init", NATM_Init)
    !! call ESMF_CompSetRoutine(comp1, "run", NATM_Run)
    !! call ESMF_CompSetRoutine(comp1, "final", NATM_Final)


    call ESMF_CompInit(comp1, rc)
    print *, "Driver Comp Init returned"

    finished = .false.
    timestep = 1
    endtime = 10
    do while (.not. finished)
        call ESMF_CompRun(comp1, timestep, rc)
        print *, "Driver Comp Run returned"
    
        timestep = timestep + 1
        if (timestep .gt. endtime) finished = .true.
    enddo


    call ESMF_CompFinalize(comp1, rc)
    print *, "Driver Comp Finalize returned"


    call ESMF_CompDestroy(comp1, rc)
    print *, "Driver Comp Destroy returned"

    call ESMF_LayoutDestroy(layout, rc);
    print *, "Driver Layout Destroy returned"

    print *, "Component Example Driver 1 finished"


    end program ESMF_AppCompMain
    
!\end{verbatim}
    
