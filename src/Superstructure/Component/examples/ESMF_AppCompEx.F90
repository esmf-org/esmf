! $Id: ESMF_AppCompEx.F90,v 1.6 2003/02/20 21:52:00 nscollins Exp $
!
! Example code for a Component which can be either the Application 
!   Component, or can be embedded in a higher level Component.
!   See {\tt ESMF\_AppMainEx.F90} for an example of a main program 
!   Application Component.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! This is a Component which can be either the top level Application
!   Component, or be nested in a higher level Component.
!
!
!\begin{verbatim}

!   ! Example program showing a Component which can be either the top 
!   ! level Application Component, or be nested in a higher level Component.
!   ! It can use the generic Driver program which follows to run standalone.

    module ESMF_AppCompEx
    
!   ! Some common definitions.  This requires the C preprocessor.
    #include "ESMF.h"

    ! Other ESMF modules which are needed by Comps
    use ESMF_BaseMod
    use ESMF_IOMod
    use ESMF_LayoutMod
    use ESMF_StateMod
    use ESMF_CompMod
    
    ! User supplied modules
    use PHYS_Mod, only: PHYS_Register
    use DYNM_Mod, only: DYNM_Register
    use CPLR_Mod, only: CPLR_Register

    implicit none
    
    ! Public entry point which sets up Init, Run, and Finalize methods
    public NATM_Register

    contains

        
!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
    subroutine NATM_Register(comp, rc)
        type(ESMF_Comp) :: comp
        integer :: rc

        ! Register the callback routines.
        call ESMF_CompSetRoutine(comp, "init", 1, NATM_Init, rc)
        call ESMF_CompSetRoutine(comp, "run", 1, NATM_Run, rc)
        call ESMF_CompSetRoutine(comp, "final", 1, NATM_Final, rc)

        ! If desired, this routine can register a private data block
        ! to be passed in to the routines above:
        ! call ESMF_CompSetData(comp, mydatablock, rc)

    end subroutine

!-------------------------------------------------------------------------
!   !  The Init routine creates subcomponents and arranges for their 
!   !   init, run, and finalize routines to be registered.
 
    subroutine NATM_Init(comp, clock, rc)
        type(ESMF_Comp) :: comp
        type(ESMF_Clock) :: clock
        type(ESMF_State) :: localstates(2)
        integer :: rc


        type(ESMF_Comp) :: comp1, comp2, cpl, comps(2)
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
        comp1 = ESMF_CompCreate(cname, layout1, ESMF_GRIDCOMP, ESMF_ATM, &
                               clock, "/home/myuser/model1/configdir", rc)  
    
        print *, "Internal Comp Create returned, name = ", trim(cname)
    
        ! This registers the Physics Init, Run, and Final routines
        call PHYS_Register(comp1, rc)
     
    
        cname = "Atmosphere Dynamics"
        comp2 = ESMF_CompCreate(cname, layout2, ESMF_GRIDCOMP, ESMF_ATM, &
                               clock, "/home/myuser/model1/configdir", rc)  
    
        print *, "Internal Comp Create returned, name = ", trim(cname)
    
        ! This registers the Dynamics Init, Run, and Final routines
        call DYNM_Register(comp1)
     

        cname = "PhysDyn Coupler"
        comps(1) = comp1
        comps(2) = comp2
        cpl = ESMF_CompCreate(cname, layout3, ESMF_CPLCOMP, comps, &
                               clock, "/home/myuser/model1/configdir", rc=rc)  
    
        print *, "Internal Comp Create returned, name = ", trim(cname)
    
        ! This registers the Coupler Init, Run, and Final routines
        call CPLR_Register(comp1)
    
        ! Query the components for their import and export states, and
        ! set them in the coupler state list.
        call ESMF_StateGet(comp1, importstate=localstates(1), rc=rc)
        call ESMF_StateGet(comp2, exportstate=localstates(2), rc=rc)
        call ESMF_StateSet(cpl, statelist=localstates, rc=rc)
     
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
 
    subroutine NATM_Run(comp, clock, timesteps, rc)
        type(ESMF_Comp) :: comp
        type(ESMF_Clock) :: clock
        integer :: timesteps
        integer :: rc

        logical :: finished
        integer :: internaltimestep
        type(ESMF_TimeInstant) :: internalendtime
        type(ESMF_Comp) :: comp1, comp2, cpl

        print *, "Nested Comp Run starting"

        finished = .false.
        internaltimestep = 3

        ! query clock for end time
        call ESMF_ClockGet(clock, endtime=internalendtime)
        do while (.not. finished) 
            call ESMF_CompRun(comp1, clock, internaltimestep, rc)
            call ESMF_CompRun(comp2, clock, internaltimestep, rc)
            call ESMF_CompRun(cpl, clock, internaltimestep, rc)
            print *, "Nested Comp Run returned"
        
            call ESMF_ClockAdvance(clock, timesteps)
            ! Query clock for current time
            if (internaltime .gt. internalendtime) finished = .true.
        enddo
    
        print *, "Nested Comp Run returning"
    end subroutine NATM_Run

!-------------------------------------------------------------------------
!   !  The Finalize routine gives each internal component a chance to clean
!   !   up and flush any output, and then destroys them before returning.
 
    subroutine NATM_Final(comp, clock, rc)
        type(ESMF_Comp) :: comp
        type(ESMF_Clock) :: clock
        integer :: rc

        type(ESMF_Comp) :: comp1, comp2, cpl

        print *, "Nested Comp Finalize starting"
        call ESMF_CompFinal(cpl, clock, rc)
        call ESMF_CompFinal(comp1, clock, rc)
        call ESMF_CompFinal(comp2, clock, rc)
 
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
    
    use NATM_Mod, only: NATM_Register

    implicit none
    
!   ! Local variables
    integer :: x, y, i, rc
    integer :: timestep, endtime
    integer, dimension(32) :: delist
    logical :: finished
    character(len=ESMF_MAXSTR) :: cname
    type(ESMF_Layout) :: layout
    type(ESMF_Clock) :: clock
    type(ESMF_Comp) :: comp1
        
!-------------------------------------------------------------------------
!   ! Example 1:
!   !
!   !  Create the overall layout, create the application component, and 
!   !  call the various subroutines.
 
    print *, "Component Example Driver 1:"

    delist = (/ (i, i=0,31) /)
    layout = ESMF_LayoutCreate(8, 4, delist, ESMF_XFAST, rc)

    clock = ESMF_ClockInit()

    cname = "Nested Atmosphere"
    comp1 = ESMF_CompCreate(cname, layout, ESMF_APPCOMP, ESMF_ATM, &
                               clock, "/home/myuser/model1/configdir", rc=rc)  

    print *, "Driver Comp Create returned, name = ", trim(cname)

    ! This registers the Init, Run, and Final routines.
    ! It must be a public method.
    call ESMF_CompRegister(comp, NATM_Register, rc)
     

    ! Call the Nested Init routine, which will in turn call the other
    !  init routines for the components below it.
    call ESMF_CompInit(comp1, rc)
    print *, "Driver Comp Init returned"

    finished = .false.
    timesteps = 1
    ! query clock for end time
    call ESMF_ClockGet(clock, endtime=endtime)
    do while (.not. finished)
        call ESMF_CompRun(comp1, clock, timesteps, rc)
        print *, "Driver Comp Run returned"
    
        call ESMF_ClockAdvance(clock, timesteps)

        call ESMF_ClockGet(clock, currenttime)
        if (currenttime .gt. endtime) finished = .true.
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
    
