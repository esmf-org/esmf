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
!  module and setservices names.
!
!BOP
!\begin{verbatim}

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! EXAMPLE:  This is an AppDriver.F90 file for a sequential ESMF application.
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
!  The ChangeMe.F90 file that's included below contains a number of definitions 
!  that are used by the AppDriver, such as the name of the application's
!  main configuration file and the name of the application's SetServices 
!  routine.  This file is in the same directory as the AppDriver.F90 file.
!------------------------------------------------------------------------------

#include "ChangeMe.F90"

    program ESMF_AppDriver
#define ESMF_METHOD "program ESMF_AppDriver"

#include "ESMF.h"

    ! ESMF module, defines all ESMF data types and procedures
    use ESMF_Mod
    
    ! Gridded Component registration routines.  Defined in "ChangeMe.F90"
    use USER_APP_Mod, only : SetServices => USER_APP_SetServices

    implicit none
    
!------------------------------------------------------------------------------
!  Define local variables
!------------------------------------------------------------------------------

    ! Components and States
    type(ESMF_GridComp) :: compGridded
    type(ESMF_State) :: defaultstate

    ! Configuration information
    type(ESMF_Config) :: config

    ! A common Grid
    type(ESMF_Grid) :: grid

    ! A Clock, a Calendar, and timesteps
    type(ESMF_Clock) :: clock
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Time) :: startTime
    type(ESMF_Time) :: stopTime

    ! Variables related to the Grid
    integer :: i_max, j_max

    ! Return codes for error checks
    integer :: rc, localrc
        
!------------------------------------------------------------------------------
!  Initialize ESMF.  Note that an output Log is created by default.
!------------------------------------------------------------------------------

    call ESMF_Initialize(defaultCalendar=ESMF_CAL_GREGORIAN, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    call ESMF_LogWrite("ESMF AppDriver start", ESMF_LOG_INFO)

!------------------------------------------------------------------------------
!  Create and load a configuration file.
!  The USER_CONFIG_FILE is set to sample.rc in the ChangeMe.F90 file.
!  The sample.rc file is also included in the directory with the AppDriver.F90
!  file.
!------------------------------------------------------------------------------
    
    config = ESMF_ConfigCreate(rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    call ESMF_ConfigLoadFile(config, USER_CONFIG_FILE, rc = localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

!------------------------------------------------------------------------------
!  Get configuration information.
!
!  A configuration file like sample.rc might include:
!  - size and coordinate information needed to create the default Grid.
!  - the default start time, stop time, and running intervals
!    for the main time loop.
!------------------------------------------------------------------------------

    call ESMF_ConfigGetAttribute(config, i_max, 'I Counts:', default=10, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
    call ESMF_ConfigGetAttribute(config, j_max, 'J Counts:', default=40, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

!------------------------------------------------------------------------------
!  Create the top Gridded Component.
!------------------------------------------------------------------------------

    compGridded = ESMF_GridCompCreate(name="ESMF Gridded Component", rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    call ESMF_LogWrite("Component Create finished", ESMF_LOG_INFO)

!------------------------------------------------------------------------------
!  Register the set services method for the top Gridded Component.
!------------------------------------------------------------------------------

    call ESMF_GridCompSetServices(compGridded, SetServices, rc)
    if (ESMF_LogMsgFoundError(rc, "Registration failed", rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

!------------------------------------------------------------------------------
!  Create and initialize a Clock.
!------------------------------------------------------------------------------

      call ESMF_TimeIntervalSet(timeStep, s=2, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

      call ESMF_TimeSet(startTime, yy=2004, mm=9, dd=25, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

      call ESMF_TimeSet(stopTime, yy=2004, mm=9, dd=26, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

      clock = ESMF_ClockCreate("Application Clock", timeStep, startTime, &
                                stopTime, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

!------------------------------------------------------------------------------
!  Create and initialize a Grid.
!
!  The default lower indices for the Grid are (/1,1/).
!  The upper indices for the Grid are read in from the sample.rc file,
!  where they are set to (/10,40/).  This means a Grid will be
!  created with 10 grid cells in the x direction and 40 grid cells in the
!  y direction.  The Grid section in the Reference Manual shows how to set
!  coordinates.
!------------------------------------------------------------------------------

      grid = ESMF_GridCreateShapeTile(maxIndex=(/i_max, j_max/), &
                             name="source grid", rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

      ! Attach the grid to the Component
      call ESMF_GridCompSet(compGridded, grid=grid, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

!------------------------------------------------------------------------------
!  Create and initialize a State to use for both import and export.
!  In a real code, separate import and export States would normally be
!  created.
!------------------------------------------------------------------------------

      defaultstate = ESMF_StateCreate("Default State", rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
     
!------------------------------------------------------------------------------
!  Call the initialize, run, and finalize methods of the top component.
!  When the initialize method of the top component is called, it will in
!  turn call the initialize methods of all its child components, they
!  will initialize their children, and so on.  The same is true of the
!  run and finalize methods.
!------------------------------------------------------------------------------
 
      call ESMF_GridCompInitialize(compGridded, defaultstate, defaultstate, &
          clock, rc=localrc)
      if (ESMF_LogMsgFoundError(rc, "Initialize failed", rc)) &
          call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
 
      call ESMF_GridCompRun(compGridded, defaultstate, defaultstate, &
          clock, rc=localrc)
      if (ESMF_LogMsgFoundError(rc, "Run failed", rc)) &
          call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

      call ESMF_GridCompFinalize(compGridded, defaultstate, defaultstate, &
          clock, rc=localrc)
      if (ESMF_LogMsgFoundError(rc, "Finalize failed", rc)) &
          call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
 
 
!------------------------------------------------------------------------------
!  Destroy objects.
!------------------------------------------------------------------------------

      call ESMF_ClockDestroy(clock, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

      call ESMF_StateDestroy(defaultstate, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

      call ESMF_GridCompDestroy(compGridded, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

!------------------------------------------------------------------------------
!  Finalize and clean up.
!------------------------------------------------------------------------------

    call ESMF_Finalize()

    end program ESMF_AppDriver

!\end{verbatim}    
!EOP

