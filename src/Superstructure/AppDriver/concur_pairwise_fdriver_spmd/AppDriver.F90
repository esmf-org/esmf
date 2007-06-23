!------------------------------------------------------------------------------
! MODULE AppDriver.F90 - generic main program for an ESMF Application
!
! !DESCRIPTION:
! ESMF Application Driver for any IGridded Component.  Creates the top 
!  IGridded Component and calls the Initialize, Run, and Finalize routines
!  for it.  
!  This IGridded Component will most likely create and manage subcomponents
!  internally, so that this generic file becomes the "Cap" or root 
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
    
    ! IGridded Component registration routines.  Defined in "ChangeMe.F90"
    use USER_APP_Mod, only : SetServices => USER_APP_SetServices

    implicit none
    
    ! Local variables

    ! Components
    type(ESMF_IGridComp) :: compIGridded

    ! States, Virtual Machines, and Layouts
    type(ESMF_VM) :: defaultvm
    type(ESMF_DELayout) :: defaultlayout
    type(ESMF_State) :: defaultstate

    ! Configuration information
    type(ESMF_Config) :: config

    ! A common igrid
    type(ESMF_IGrid) :: igrid

    ! A clock, a calendar, and timesteps
    type(ESMF_Clock) :: clock
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Time) :: startTime
    type(ESMF_Time) :: stopTime

    ! Variables related to igrid and clock
    integer :: i_max, j_max
    real(ESMF_KIND_R4) :: x_min4, x_max4, y_min4, y_max4
    real(ESMF_KIND_R8) :: x_min, x_max, y_min, y_max

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

    ! the default igrid size and type
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

    ! Create the top IGridded component, passing in the default layout.
    compIGridded = ESMF_IGridCompCreate(name="ESMF IGridded Component", rc=rc)

    call ESMF_LogWrite("Component Create finished", ESMF_LOG_INFO)


!!------------------------------------------------------------------------------
!!------------------------------------------------------------------------------
!!  Register section
!!------------------------------------------------------------------------------
!!------------------------------------------------------------------------------
      call ESMF_IGridCompSetServices(compIGridded, SetServices, rc)
      if (ESMF_LogMsgFoundError(rc, "Registration failed", rc)) goto 10


!!------------------------------------------------------------------------------
!!------------------------------------------------------------------------------
!!  Create and initialize a clock, and a igrid.
!!------------------------------------------------------------------------------
!!------------------------------------------------------------------------------

     ! Based on values from the Config file, create a default IGrid
     ! and Clock.  We assume we have read in the variables below from
     ! the config file.


      call ESMF_TimeIntervalSet(timeStep, s=2, rc=rc)

      call ESMF_TimeSet(startTime, yy=2004, mm=9, dd=25, rc=rc)

      call ESMF_TimeSet(stopTime, yy=2004, mm=9, dd=26, rc=rc)

      clock = ESMF_ClockCreate("Application Clock", timeStep, startTime, &
                                stopTime, rc=rc)


      ! Same with the igrid.  Get a default layout based on the VM.
      defaultlayout = ESMF_DELayoutCreate(defaultvm, rc=rc)

      x_min = x_min4
      y_min = y_min4
      x_max = x_max4
      y_max = y_max4
      igrid = ESMF_IGridCreateHorzXYUni(counts=(/i_max, j_max/), &
                             minGlobalCoordPerDim=(/x_min, y_min/), &
                             maxGlobalCoordPerDim=(/x_max, y_max/), &
                             horzStagger=ESMF_IGRID_HORZ_STAGGER_C_SE, &
                             name="source igrid", rc=rc)
      call ESMF_IGridDistribute(igrid, delayout=defaultlayout, rc=rc)

      ! Attach the IGrid to the Component
      call ESMF_IGridCompSet(compIGridded, igrid=igrid, rc=rc)


!!------------------------------------------------------------------------------
!!------------------------------------------------------------------------------
!!  Create and initialize a State to use for both import and export.
!!------------------------------------------------------------------------------
!!------------------------------------------------------------------------------

      defaultstate = ESMF_StateCreate("Default IGridded State", rc=rc)
     
!!------------------------------------------------------------------------------
!!------------------------------------------------------------------------------
!!  Init, Run, and Finalize section
!!------------------------------------------------------------------------------
!!------------------------------------------------------------------------------
 
      call ESMF_IGridCompInitialize(compIGridded, defaultstate, defaultstate, &
                                                                  clock, rc=rc)
      if (ESMF_LogMsgFoundError(rc, "Initialize failed", rc)) goto 10
 


      call ESMF_IGridCompRun(compIGridded, defaultstate, defaultstate, &
                                                                  clock, rc=rc)
      if (ESMF_LogMsgFoundError(rc, "Run failed", rc)) goto 10
 


      call ESMF_IGridCompFinalize(compIGridded, defaultstate, defaultstate, &
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

      call ESMF_IGridCompDestroy(compIGridded, rc)

      call ESMF_DELayoutDestroy(defaultLayout, rc)

!!------------------------------------------------------------------------------
!!------------------------------------------------------------------------------

10 continue

      call ESMF_Finalize(rc=rc)

      end program ESMF_AppDriver

!\end{verbatim}    
!EOP















