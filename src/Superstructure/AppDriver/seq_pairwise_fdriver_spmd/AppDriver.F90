!------------------------------------------------------------------------------
! MODULE AppDriver.F90 - generic main program for an ESMF Application
!
! !DESCRIPTION:
!  ESMF Application Driver for any Gridded Component.  Creates the top 
!  Gridded Component and calls the Initialize, Run, and Finalize routines
!  for it.  
!  This Gridded Component will most likely create and manage subcomponents
!  internally, so that this generic file becomes the "cap" or root 
!  of a tree of components.
!
!  In the text below, {\tt USER\_APP} should be replaced by the specific
!   module and setservices names.
!
!BOP
!\begin{verbatim}

!!------------------------------------------------------------------------------
!! The ChangeMe.F90 file contains a number of definitions 
!! that are used by the AppDriver, such as the name of the application's
!! main configuration file and the name of the application's SetServices 
!! routine.  
!!------------------------------------------------------------------------------

#include "ChangeMe.F90"

    program ESMF_AppDriver

    ! ESMF module, defines all ESMF data types and procedures
    use ESMF_Mod
    
    ! Gridded Component registration routines.  Defined in "ChangeMe.F90"
    use USER_APP_Mod, only : SetServices => USER_APP_SetServices

    implicit none
    
    ! Local variables

    ! Components
    type(ESMF_GridComp) :: compGridded

    ! States, Virtual Machines, and Layouts
    type(ESMF_VM) :: defaultvm
    type(ESMF_DELayout) :: defaultlayout
    type(ESMF_State) :: defaultstate

    ! Configuration information
    type(ESMF_Config) :: config

    ! A common grid
    type(ESMF_Grid) :: grid

    ! A clock, a calendar, and timesteps
    type(ESMF_Clock) :: clock
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Time) :: startTime
    type(ESMF_Time) :: stopTime

    ! Variables related to grid and clock
    integer :: i_max, j_max
    real(ESMF_KIND_R4) :: x_min4, x_max4, y_min4, y_max4
    real(ESMF_KIND_R8) :: x_min, x_max, y_min, y_max
    integer :: ix_min, ix_max, iy_min, iy_max

    ! Return codes for error checks
    integer :: rc
    logical :: dummy
        
!!------------------------------------------------------------------------------
!!  Initialize the ESMF Framework
!!------------------------------------------------------------------------------

    call ESMF_Initialize(defaultCalendar=ESMF_CAL_GREGORIAN, rc=rc)
    if (rc .ne. ESMF_SUCCESS) stop 99 


    call ESMF_LogWrite("ESMF AppDriver start", ESMF_LOG_INFO)

    !
    ! Read in Configuration information from a default config file
    !
    
    config = ESMF_ConfigCreate(rc)
    call ESMF_ConfigLoadFile(config, USER_CONFIG_FILE, rc = rc)

    ! Get standard config parameters, for example:

    ! the default grid size and type
    ! the default start time, stop time, and running intervals
    !  for the main time loop.
    !
    ! e.g. to get an integer parameter from the config file:
    ! call ESMF_ConfigGetAttribute( config, ndays, label ='Number_of_Days:', &
    !                               default=30, rc = rc )
    !
    call ESMF_ConfigGetAttribute(config, i_max, 'I Counts:', default=20, rc=rc)
    call ESMF_ConfigGetAttribute(config, j_max, 'J Counts:', default=80, rc=rc)
    call ESMF_ConfigGetAttribute(config, x_min4, 'X Min:', default=0.0, rc=rc)
    call ESMF_ConfigGetAttribute(config, y_min4, 'Y Min:', default=-180.0, rc=rc)
    call ESMF_ConfigGetAttribute(config, x_max4, 'X Max:', default=90.0, rc=rc)
    call ESMF_ConfigGetAttribute(config, y_max4, 'Y Max:', default=180.0, rc=rc)

!!------------------------------------------------------------------------------
!!------------------------------------------------------------------------------
!!    Create section
!!------------------------------------------------------------------------------
!!------------------------------------------------------------------------------
 
    ! Get the default VM which contains all PEs this job was started on.
    call ESMF_VMGetGlobal(defaultvm, rc)

    ! Create the top Gridded component, passing in the default layout.
    compGridded = ESMF_GridCompCreate(name="ESMF Gridded Component", rc=rc)

    call ESMF_LogWrite("Component Create finished", ESMF_LOG_INFO)


!!------------------------------------------------------------------------------
!!------------------------------------------------------------------------------
!!  Register section
!!------------------------------------------------------------------------------
!!------------------------------------------------------------------------------
      call ESMF_GridCompSetServices(compGridded, SetServices, rc)
      if (ESMF_LogMsgFoundError(rc, "Registration failed", rc)) goto 10


!!------------------------------------------------------------------------------
!!------------------------------------------------------------------------------
!!  Create and initialize a clock, and a grid.
!!------------------------------------------------------------------------------
!!------------------------------------------------------------------------------

     ! Based on values from the Config file, create a default grid
     ! and Clock.  We assume we have read in the variables below from
     ! the config file.


      call ESMF_TimeIntervalSet(timeStep, s=2, rc=rc)

      call ESMF_TimeSet(startTime, yy=2004, mm=9, dd=25, rc=rc)

      call ESMF_TimeSet(stopTime, yy=2004, mm=9, dd=26, rc=rc)

      clock = ESMF_ClockCreate("Application Clock", timeStep, startTime, &
                                stopTime, rc=rc)


      ! Same with the grid.  Get a default layout based on the VM.
      defaultlayout = ESMF_DELayoutCreate(defaultvm, rc=rc)

      ix_min = x_min4
      iy_min = y_min4
      ix_max = x_max4
      iy_max = y_max4
      grid = ESMF_GridCreateShapeTile(minIndex=(/ix_min, iy_min/), &
                             maxIndex=(/ix_max, iy_max/), &
                             name="source grid", rc=rc)
      !call ESMF_gridDistribute(grid, delayout=defaultlayout, rc=rc)

      ! Attach the grid to the Component
      call ESMF_GridCompSet(compGridded, grid=grid, rc=rc)


!!------------------------------------------------------------------------------
!!------------------------------------------------------------------------------
!!  Create and initialize a State to use for both import and export.
!!------------------------------------------------------------------------------
!!------------------------------------------------------------------------------

      defaultstate = ESMF_StateCreate("Default State", rc=rc)
     
!!------------------------------------------------------------------------------
!!------------------------------------------------------------------------------
!!  Init, Run, and Finalize section
!!------------------------------------------------------------------------------
!!------------------------------------------------------------------------------
 
      call ESMF_GridCompInitialize(compGridded, defaultstate, defaultstate, &
                                                                  clock, rc=rc)
      if (ESMF_LogMsgFoundError(rc, "Initialize failed", rc)) goto 10
 


      call ESMF_GridCompRun(compGridded, defaultstate, defaultstate, &
                                                                  clock, rc=rc)
      if (ESMF_LogMsgFoundError(rc, "Run failed", rc)) goto 10
 


      call ESMF_GridCompFinalize(compGridded, defaultstate, defaultstate, &
                                                                  clock, rc=rc)
      if (ESMF_LogMsgFoundError(rc, "Finalize failed", rc)) goto 10
 
 
!!------------------------------------------------------------------------------
!!------------------------------------------------------------------------------
!!     Destroy section
!!------------------------------------------------------------------------------
!!------------------------------------------------------------------------------

      ! Clean up

      call ESMF_ClockDestroy(clock, rc)

      call ESMF_StateDestroy(defaultstate, rc)

      call ESMF_GridCompDestroy(compGridded, rc)

      call ESMF_DELayoutDestroy(defaultLayout, rc)

!!------------------------------------------------------------------------------
!!------------------------------------------------------------------------------

10 continue

      call ESMF_Finalize(rc=rc)

      end program ESMF_AppDriver

!\end{verbatim}    
!EOP















