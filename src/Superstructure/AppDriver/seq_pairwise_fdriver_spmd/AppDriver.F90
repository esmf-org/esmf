!------------------------------------------------------------------------------
!BOP
!
! MODULE AppDriver.F90 - generic main program for an ESMF Application
!
! !DESCRIPTION:
! ESMF Application Driver for any Gridded Component.  Creates the top 
!  Gridded Component and calls the Initialize, Run, and Finalize routines
!  for it.  
!  This Gridded Component will most likely create and manage subcomponents
!  internally, so that this generic file becomes the "Cap" or root 
!  of a tree of components.
!
!  In the text below, {\tt USER\_APP} should be replaced by the specific
!   module and setservices names.
!
!\begin{verbatim}

! The file which defines the user application and config file.
! This must be edited before building this file.
#include "ChangeMe.F90"

    program ESMF_ApplicationDriver

    ! ESMF Framework module, defines all ESMF data types and procedures
    use ESMF_Mod
    
    ! Gridded Component registration routines.  Defined in "ChangeMe.F90"
    use USER_APP_Mod, only : SetServices => USER_APP_SetServices

    implicit none
    
    ! Local variables

    ! Components
    type(ESMF_GridComp) :: compGridded

    ! States and Layouts
    type(ESMF_DELayout) :: defaultlayout
    type(ESMF_State) :: defaultstate

    ! Configuration information
    type(ESMF_Config) :: config

    ! A common grid
    type(ESMF_Grid) :: grid

    ! A clock, a calendar, and timesteps
    type(ESMF_Clock) :: clock
    type(ESMF_Calendar) :: gregorianCalendar
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Time) :: startTime
    type(ESMF_Time) :: stopTime

    ! Variables related to grid and clock
    integer :: i_max, j_max
    real(ESMF_KIND_I8) :: x_min, x_max, y_min, y_max
    integer :: s_month, s_day, s_hour, s_min
    integer :: e_month, e_day, e_hour, e_min

    ! Return codes for error checks
    integer :: rc
        
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Initialize the ESMF Framework
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

    call ESMF_Initialize(rc=rc)
    if (rc .ne. ESMF_SUCCESS) stop 99 


    ! call ESMF_LogErrMsg("ESMF AppDriver start")
    print *, "ESMF AppDriver start"

    !
    ! Read in Configuration information from a default config file
    !
    
    config = ESMF_ConfigCreate(rc)
    call ESMF_ConfigLoadFile(config, USER_CONFIG_FILE, rc = rc)

    !  *** this section is incomplete. ***
    ! Get standard config parameters, for example:

    ! the default grid size and type
    ! the default start time, stop time, and running intervals
    !  for the main time loop.
    !
    ! e.g. to get an integer parameter from the config file:
    !  ndays = ESMF_ConfigGetInt( config, label ='Number_of_Days:', &
    !                             default=30, rc = rc )
    !
    i_max = ESMF_ConfigGetInt(config, label='I Counts:', default=20, rc=rc)
    j_max = ESMF_ConfigGetInt(config, label='J Counts:', default=80, rc=rc)
    x_min = ESMF_ConfigGetFloat(config, label='X Min:', default=0.0, rc=rc)
    y_min = ESMF_ConfigGetFloat(config, label='Y Min:', default=-180.0, rc=rc)
    x_max = ESMF_ConfigGetFloat(config, label='X Max:', default=90.0, rc=rc)
    y_max = ESMF_ConfigGetFloat(config, label='Y Max:', default=180.0, rc=rc)

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!    Create section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
 
    ! Create a default layout, which is 1xN in topology, where N is the
    !  number of DEs this program was started on.
    defaultlayout = ESMF_DELayoutCreate(rc=rc)


    ! Create the top Gridded component, passing in the default layout.
    compGridded = ESMF_GridCompCreate("ESMF Gridded Component", &
                                       layout=defaultlayout, rc=rc)


    ! call ESMF_LogErrMsg("Component Create finished")
    print *, "Component Create finished"


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Register section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
      call ESMF_GridCompSetServices(compGridded, SetServices, rc)
      ! call ESMF_LogErrMsg(rc, "Component SetServices finished")
      print *, "Comp SetServices finished, rc= ", rc


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Create and initialize a clock, and a grid.
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!    Based on values from the Config file, create a default Grid
!    and Clock.  We assume we have read in the variables below from
!    the config file.


      gregorianCalendar = ESMF_CalendarCreate("Gregorian", &
                                              ESMF_CAL_GREGORIAN, rc)

      call ESMF_TimeIntervalSet(timeStep, S=2, rc=rc)

      call ESMF_TimeSet(startTime, yy=2003, &
                         mm=s_month, dd=s_day, h=s_hour, m=s_min, s=0, &
                         calendar=gregorianCalendar, rc=rc)

      call ESMF_TimeSet(stopTime, yy=2003, &
                         mm=e_month, dd=e_day, h=e_hour, m=e_min, s=0, &
                         calendar=gregorianCalendar, rc=rc)

      clock = ESMF_ClockCreate("Application Clock", timeStep, startTime, &
                                stopTime, rc=rc)


      ! Same with the grid.

      grid = ESMF_GridCreate(numDims=2, counts=(/i_max, j_max/), &
                             min=(/x_min, y_min/), &
                             max=(/x_max, y_max/), &
                             layout=defaultlayout, &   
                             horz_gridtype=ESMF_GridType_XY, &
                             horz_stagger=ESMF_GridStagger_C, &
                             horz_coord_system=ESMF_CoordSystem_Cartesian, &
                             name="source grid", rc=rc)

     ! Attach the Grid to the Component
     call ESMF_GridCompSet(compGridded, grid=grid, rc=rc)


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Create and initialize a State to use for both import and export.
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

      defaultstate = ESMF_StateCreate("Default Gridded State", rc=rc)
     
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Init, Run, and Finalize section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
 
      call ESMF_GridCompInitialize(compGridded, defaultstate, defaultstate, &
                                                                  clock, rc=rc)
      ! call ESMF_LogErrMsg(rc, "Component Initialize finished")
      print *, "Component Initialize finished, rc =", rc
 


      call ESMF_GridCompRun(compGridded, defaultstate, defaultstate, &
                                                                  clock, rc=rc)
      ! call ESMF_LogErrMsg(rc, "Component Run finished")
      print *, "Component Run finished, rc =", rc
 


      call ESMF_GridCompFinalize(compGridded, defaultstate, defaultstate, &
                                                                  clock, rc=rc)
      ! call ESMF_LogErrMsg(rc, "Component Finalize finished")
      print *, "Component Finalize finished, rc =", rc
 
 
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!     Destroy section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!     Clean up

      call ESMF_ClockDestroy(clock, rc)

      call ESMF_CalendarDestroy(gregorianCalendar, rc)

      call ESMF_StateDestroy(defaultstate, rc)

      call ESMF_GridCompDestroy(compGridded, rc)

      call ESMF_DELayoutDestroy(defaultLayout, rc)

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

      call ESMF_Finalize(rc)

      end program ESMF_ApplicationDriver

!\end{verbatim}    
!EOP


