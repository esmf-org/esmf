! $Id: CoupledFlowApp.F90,v 1.6 2007/07/19 22:15:33 cdeluca Exp $
!
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: CoupledFlowApp.F90 - Main program source file for demo
!
! !DESCRIPTION:
! ESMF Application Wrapper for Coupled Flow Demo.  This file contains the
!  main program, and creates a top level ESMF Gridded Component to contain
!  all other Components.
!
!
!EOP

    program ESMF_ApplicationWrapper

    ! ESMF module, defines all ESMF data types and procedures
    use ESMF_Mod
    
    ! Flow Component registration routines
    use CoupledFlowMod, only : CoupledFlow_register

    implicit none
    
    ! Local variables

    ! Components
    type(ESMF_GridComp) :: compGridded

    ! State, Virtual Machine, and DELayout
    type(ESMF_VM) :: vm
    type(ESMF_State) :: flowstate
    type(ESMF_DELayout) :: DELayoutTop
    integer :: pet_id

    ! A common igrid
    type(ESMF_IGrid) :: igrid

    ! A clock, a calendar, and timesteps
    type(ESMF_Clock) :: clock
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Time) :: startTime
    type(ESMF_Time) :: stopTime

    ! Variables related to igrid and clock
    integer :: counts(2)
    integer :: i_max, j_max
    real(ESMF_KIND_R8) :: x_min, x_max, y_min, y_max, g_min(2), g_max(2)
    integer :: s_month, s_day, s_hour, s_min
    integer :: e_month, e_day, e_hour, e_min

    ! Read in from config file
    namelist /input/ i_max, j_max, x_min, x_max, y_min, y_max, &
                     s_month, s_day, s_hour, s_min, &
                     e_month, e_day, e_hour, e_min
!BOP
!
! !DESCRIPTION:
! \subsubsection{Namelist Input Parameters for CoupledFlowApp:}
!     The following variables must be input to the CoupledFlow Application to
!     run.  They are located in a file called "coupled\_app\_input."
!
!     The variables are:
!     \begin{description}
!     \item [i\_max]
!           Global number of cells in the first igrid direction.
!     \item [j\_max]
!           Global number of cells in the second igrid direction.
!     \item [x\_min]
!           Minimum igrid coordinate in the first direction.
!     \item [x\_max]
!           Maximum igrid coordinate in the first direction.
!     \item [y\_min]
!           Minimum igrid coordinate in the second direction.
!     \item [y\_max]
!           Maximum igrid coordinate in the second direction.
!     \item [s\_month]
!           Simulation start time month (integer).
!     \item [s\_day]
!           Simulation start time day (integer).
!     \item [s\_hour]
!           Simulation start time hour (integer).
!     \item [s\_min]
!           Simulation start time minute (integer).
!     \item [e\_month]
!           Simulationendt time month (integer).
!     \item [e\_day]
!           Simulation end time day (integer).
!     \item [e\_hour]
!           Simulation end time hour (integer).
!     \item [e\_min]
!           Simulation end time minute (integer).
!     \end{description}
!
!EOP

    ! Return codes for error checks
    integer :: rc
        
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!    ESMF_Initialize
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!

!BOP
!
! !DESCRIPTION:
! \subsubsection{Example of Initializing the Framework:}
!
!     The first call to ESMF must be the initialize method.   As part of
!     initialization the default Calendar can be specified, some options
!     for logging can be set, and the default global VM can be returned.
!     Here we are setting the default Calendar to be Gregorian, and getting
!     back the global VM:
!\begin{verbatim}
    ! Initialize ESMF, get the default Global VM, and set
    ! the default calendar to be Gregorian.
    call ESMF_Initialize(vm=vm, defaultCalendar=ESMF_CAL_GREGORIAN, rc=rc)
!\end{verbatim}
!EOP 

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

    print *, "Coupled Flow Demo Application Start"

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!   Read in configuration data - will be replaced by Config routines
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
      !
      ! Read in input file
      !
      open(9, status="old", file="coupled_app_input",action="read",iostat=rc)
      if (rc .ne. 0) then
        print *, "Error!  Failed to open namelist file 'coupled_app_input' "
        stop
      endif
      read(9, input, end=20)
   20 continue
      close(9)

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!    Create section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!

!BOP
!\begin{verbatim}
    ! Create the top level Gridded Component.
    compGridded = ESMF_GridCompCreate(name="Coupled Flow Demo", rc=rc)
!\end{verbatim}
!EOP 

    print *, "Comp Creates finished"


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Register section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
      call ESMF_GridCompSetServices(compGridded, CoupledFlow_register, rc)
      print *, "Comp SetServices finished, rc= ", rc


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Create and initialize a clock, and a igrid.
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
!
! !DESCRIPTION:
! \subsubsection{Example of Calendar and Clock Creation and Usage:}
!
!     The following piece of code provides an example of Clock creation used in
!     the Demo.  Note that the Gregorian calendar was set as the default in
!     the ESMF\_Initialize() call above.  As shown in this example, we first
!     initialize a time interval (timestep) to 2 seconds:
!\begin{verbatim}
      call ESMF_TimeIntervalSet(timeStep, s=2, rc=rc)
!\end{verbatim}
!     And then we set the start time and stop time to input values for the month,
!     day, and hour (assuming the year to be 2003):
!\begin{verbatim}
      call ESMF_TimeSet(startTime, yy=2003, mm=s_month, dd=s_day, &
                        h=s_hour, m=s_min, s=0, rc=rc)

      call ESMF_TimeSet(stopTime, yy=2003, mm=e_month, dd=e_day, &
                        h=e_hour, m=e_min, s=0, rc=rc)
!\end{verbatim}
!     With the time interval, start time, and stop time set above, the Clock can
!     now be created:
!\begin{verbatim}
      clock = ESMF_ClockCreate(timeStep=timeStep, startTime=startTime, &
                               stopTime=stopTime, rc=rc)
!\end{verbatim}
!     Subsequent calls to ESMF\_ClockAdvance with this clock will increment the
!     current time from the start time by the timestep.
!EOP 
      !
      ! Create the IGrid and attach it to the Component
      !

      ! Create a default DELayout for the igrid based on the global VM
      DELayoutTop = ESMF_DELayoutCreate(vm, rc=rc)
!BOP
!
! !DESCRIPTION:
! \subsubsection{Example of IGrid Creation:}
!
!  The following piece of code provides an example of IGrid creation used in
!  the Demo.  The extents of the IGrid were previously read in from an input
!  file, but the rest of the IGrid parameters are set here by default.  The
!  IGrid spans the Application's PET list, while the type of the IGrid is 
!  assumed to be horizontal and cartesian x-y with an Arakawa C staggering.  
!  The IGrid name is set to "source igrid":
!\begin{verbatim}
      counts(1) = i_max
      counts(2) = j_max
      g_min(1) = x_min
      g_min(2) = y_min
      g_max(1) = x_max
      g_max(2) = y_max
      igrid = ESMF_IGridCreateHorzXYUni(counts=counts, &
                             minGlobalCoordPerDim=g_min, &
                             maxGlobalCoordPerDim=g_max, &
                             horzStagger=ESMF_IGRID_HORZ_STAGGER_C_NE, &
                             name="source igrid", rc=rc)
      call ESMF_IGridDistribute(igrid, delayout=DELayoutTop, rc=rc)

!\end{verbatim}
!     The IGrid can then be attached to the Gridded Component with a set call:
!\begin{verbatim}
     call ESMF_GridCompSet(compGridded, igrid=igrid, rc=rc)
!\end{verbatim}
!EOP

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Create and initialize a State to use for both import and export.
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

      flowstate = ESMF_StateCreate("Coupled Flow State", rc=rc)
     
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Init, Run, and Finalize section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
 
      call ESMF_GridCompInitialize(compGridded, flowstate, flowstate, &
                                                                 clock, rc=rc)
      print *, "Coupled Flow Component Initialize finished, rc =", rc
 


      call ESMF_GridCompRun(compGridded, flowstate, flowstate, clock, rc=rc)
      print *, "Coupled Flow Component Run finished, rc =", rc
 


      call ESMF_GridCompFinalize(compGridded, flowstate, flowstate, clock, rc=rc)
      print *, "Coupled Flow Component Finalize finished, rc =", rc
 
 
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!     Destroy section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!     Clean up

      call ESMF_StateDestroy(flowstate, rc)

      call ESMF_IGridDestroy(igrid, rc)

      call ESMF_ClockDestroy(clock, rc)

      call ESMF_GridCompDestroy(compGridded, rc)

      call ESMF_DELayoutDestroy(DELayoutTop, rc)


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
      ! This output goes into the log file (standard output, unit 6)
      print *, "**********************************************************"
      print *, "SUCCESS!  Your ESMF Coupled Flow Application Demo ", &
               "ran to completion!"
      print *, "See the output files in the Demo source directory for ", &
               "the generated data."
      print *, "**********************************************************"

      ! Get our PET number from the VM
      call ESMF_VMGet(vm, localPET=pet_id, rc=rc)

      ! This output goes to the console/screen (standard error) where
      ! hopefully the user will see it without needing to inspect the log file.
      if (pet_id .eq. 0) then
        write(0, *) ""
        write(0, *) "SUCCESS!  Your ESMF Coupled Flow Application Demo ", &
                 "ran to completion!"
        write(0, *) ""
      endif

      ! Finalize must be after the message, since this call shuts down MPI if
      ! it is being used and on some platforms that prevents messages from
      ! reaching their destination files.

      call ESMF_Finalize(rc=rc)

      end program ESMF_ApplicationWrapper
    
