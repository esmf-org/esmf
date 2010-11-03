! $Id: CoupledFlowApp.F90,v 1.22 2010/11/03 04:58:54 theurich Exp $
!
!------------------------------------------------------------------------------
!BOE
!
! !MODULE: CoupledFlowApp.F90 - Main program source file for demo
!
! !DESCRIPTION:
! ESMF Application Wrapper for Coupled Flow Demo.  This file contains the
!  main program, and creates a top level ESMF Gridded Component to contain
!  all other Components.
!
!
!EOE

    program ESMF_ApplicationWrapper

    ! ESMF module, defines all ESMF data types and procedures
    use ESMF
    
    ! Flow Component registration routines
    use CoupledFlowMod, only : CoupledFlow_register

    implicit none
    
    ! Local variables

    ! Components
    type(ESMF_GridComp) :: compGridded

    ! State, Virtual Machine, and DELayout
    type(ESMF_VM) :: vm
    type(ESMF_State) :: flowstate
    integer :: pet_id

    ! A common grid
    type(ESMF_Grid) :: grid

    ! A clock, a calendar, and timesteps
    type(ESMF_Clock) :: clock
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Time) :: startTime
    type(ESMF_Time) :: stopTime

    ! Variables related to grid and clock
    integer :: counts(2)
    integer :: i_max, j_max, i, j
    real(ESMF_KIND_R8) :: x_min, x_max, y_min, y_max, g_min(2), g_max(2)
    real(ESMF_KIND_R8), pointer :: CoordX(:), CoordY(:)
    real(ESMF_KIND_R8) :: dx, dy
    integer :: s_month, s_day, s_hour, s_min
    integer :: e_month, e_day, e_hour, e_min
    type(ESMF_Array)  :: Ax, Ay

    ! Read in from config file
    namelist /input/ i_max, j_max, x_min, x_max, y_min, y_max, &
                     s_month, s_day, s_hour, s_min, &
                     e_month, e_day, e_hour, e_min
!BOE
!
! !DESCRIPTION:
! \subsubsection{Namelist Input Parameters for CoupledFlowApp:}
!     The following variables must be input to the CoupledFlow Application to
!     run.  They are located in a file called "coupled\_app\_input."
!
!     The variables are:
!     \begin{description}
!     \item [i\_max]
!           Global number of cells in the first grid direction.
!     \item [j\_max]
!           Global number of cells in the second grid direction.
!     \item [x\_min]
!           Minimum grid coordinate in the first direction.
!     \item [x\_max]
!           Maximum grid coordinate in the first direction.
!     \item [y\_min]
!           Minimum grid coordinate in the second direction.
!     \item [y\_max]
!           Maximum grid coordinate in the second direction.
!     \item [s\_month]
!           Simulation start time month (integer).
!     \item [s\_day]
!           Simulation start time day (integer).
!     \item [s\_hour]
!           Simulation start time hour (integer).
!     \item [s\_min]
!           Simulation start time minute (integer).
!     \item [e\_month]
!           Simulation end time month (integer).
!     \item [e\_day]
!           Simulation end time day (integer).
!     \item [e\_hour]
!           Simulation end time hour (integer).
!     \item [e\_min]
!           Simulation end time minute (integer).
!     \end{description}
!
!EOE

    ! Return codes for error checks
    integer :: rc, urc
        
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!    ESMF_Initialize
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!

!BOE
!
! !DESCRIPTION:
! \subsubsection{Example of Initializing the Framework:}
!
!     The first call to ESMF must be the initialize method.   As part of
!     initialization the default Calendar can be specified, some options
!     for logging can be set, and the default global VM can be returned.
!     Here we are setting the default Calendar to be Gregorian, and getting
!     back the global VM:
!EOE

!BOC
    ! Initialize ESMF, get the default Global VM, and set
    ! the default calendar to be Gregorian.
    call ESMF_Initialize(vm=vm, defaultCalendar=ESMF_CAL_GREGORIAN, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
!EOC 

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

!BOC
    ! Create the top level Gridded Component.
    compGridded = ESMF_GridCompCreate(name="Coupled Flow Demo", rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
!EOC 

    print *, "Comp Creates finished"


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Register section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
      call ESMF_GridCompSetServices(compGridded, CoupledFlow_register, rc)
      print *, "Comp SetServices finished, rc= ", rc
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Create and initialize a clock, and a grid.
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOE
!
! !DESCRIPTION:
! \subsubsection{Example of Calendar and Clock Creation and Usage:}
!
!     The following piece of code provides an example of Clock creation used in
!     the Demo.  Note that the Gregorian calendar was set as the default in
!     the ESMF\_Initialize() call above.  As shown in this example, we first
!     initialize a time interval (timestep) to 2 seconds:
!EOE

!BOC
      call ESMF_TimeIntervalSet(timeStep, s=2, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      !And then we set the start time and stop time to input values for the month,
      !day, and hour (assuming the year to be 2003):
      call ESMF_TimeSet(startTime, yy=2003, mm=s_month, dd=s_day, &
                        h=s_hour, m=s_min, s=0, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

      call ESMF_TimeSet(stopTime, yy=2003, mm=e_month, dd=e_day, &
                        h=e_hour, m=e_min, s=0, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      !With the time interval, start time, and stop time set above, the Clock can
      !now be created:
      clock = ESMF_ClockCreate(timeStep=timeStep, startTime=startTime, &
                               stopTime=stopTime, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      !Subsequent calls to ESMF\_ClockAdvance with this clock will increment the
      !current time from the start time by the timestep.
!EOC 

!
! Create the Grid and attach it to the Component:
!

!BOE
!
! !DESCRIPTION:
! \subsubsection{Example of Grid Creation:}
!
!  The following piece of code provides an example of Grid creation used in
!  the Demo.  The extents of the Grid were previously read in from an input
!  file, but the rest of the Grid parameters are set here by default.  The
!  Grid spans the Application's PET list, while the type of the Grid is 
!  assumed to be horizontal and Cartesian x-y with an Arakawa C staggering.  
!  The Grid name is set to "source grid":
!EOE

!BOC
      counts(1) = i_max
      counts(2) = j_max
      g_min(1) = x_min
      g_min(2) = y_min
      g_max(1) = x_max
      g_max(2) = y_max
      grid = ESMF_GridCreateShapeTile(maxIndex=counts, &
                             coordDep1=(/1/), &
                             coordDep2=(/2/), &
                             gridEdgeLWidth=(/0,0/), &
                             name="source grid", rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      ! u
      call ESMF_GridAddCoord(grid, staggerLoc=ESMF_STAGGERLOC_EDGE1, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      ! v
      call ESMF_GridAddCoord(grid, staggerLoc=ESMF_STAGGERLOC_EDGE2, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      ! sie, p, q, rho, flag
      call ESMF_GridAddCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      ! Get pointer reference to internal coordinate array
      ! Compute center stagger coordinate values
      call ESMF_GridGetCoord(grid, localDE=0, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, &
        coordDim=1, fptr=CoordX, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      call ESMF_GridGetCoord(grid, localDE=0, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, &
        coordDim=2, fptr=CoordY, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

      dx = (x_max-x_min)/i_max
      dy = (y_max-y_min)/j_max
      coordX(1) = x_min + dx/2
      coordY(1) = y_min + dy/2
      do i = 2, i_max
        coordX(i) = coordX(i-1) + dx
      enddo
      do j = 2, j_max
        coordY(j) = coordY(j-1) + dy
      enddo

      ! Get pointer reference to internal coordinate for U
      ! Compute east stagger (U) coordinate values
      call ESMF_GridGetCoord(grid, localDE=0, &
        staggerLoc=ESMF_STAGGERLOC_EDGE1, &
        coordDim=1, fptr=CoordX, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      call ESMF_GridGetCoord(grid, localDE=0, &
        staggerLoc=ESMF_STAGGERLOC_EDGE1, &
        coordDim=2, fptr=CoordY, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

      dx = (x_max-x_min)/i_max
      dy = (y_max-y_min)/j_max
      coordX(1) = x_min + dx
      coordY(1) = y_min + dy/2
      do i = 2, i_max
        coordX(i) = coordX(i-1) + dx
      enddo
      do j = 2, j_max
        coordY(j) = coordY(j-1) + dy
      enddo

      ! Get pointer reference to internal coordinate for V
      ! Compute north stagger (V) coordinate values
      call ESMF_GridGetCoord(grid, localDE=0, &
        staggerLoc=ESMF_STAGGERLOC_EDGE2, &
        coordDim=1, fptr=CoordX, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      call ESMF_GridGetCoord(grid, localDE=0, &
        staggerLoc=ESMF_STAGGERLOC_EDGE2, &
        coordDim=2, fptr=CoordY, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

      dx = (x_max-x_min)/i_max
      dy = (y_max-y_min)/j_max
      coordX(1) = x_min + dx/2
      coordY(1) = y_min + dy
      do i = 2, i_max
        coordX(i) = coordX(i-1) + dx
      enddo
      do j = 2, j_max
        coordY(j) = coordY(j-1) + dy
      enddo

      !The Grid can then be attached to the Gridded Component with a set call:
      call ESMF_GridCompSet(compGridded, grid=grid, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
!EOC

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOE
!
! !DESCRIPTION:
! \subsubsection{Example of State Creation:}
!
!  Create and initialize a dummy State to use for both import and export.
!EOE
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!BOC
      flowstate = ESMF_StateCreate("Coupled Flow State", rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
!EOC
     
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOE
!
! !DESCRIPTION:
! \subsubsection{Example of Initialize, Run, and Finalize:}
!
! Init, Run, and Finalize sections of the Coupled Flow Component:
!EOE
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
 
!BOC
      call ESMF_GridCompInitialize(compGridded, flowstate, flowstate, &
                                                                 clock, rc=rc, userRc=urc)
      print *, "Coupled Flow Component Initialize finished, rc =", rc, urc
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      if(urc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=urc)

      call ESMF_GridCompRun(compGridded, flowstate, flowstate, clock, rc=rc, userRc=urc)
      print *, "Coupled Flow Component Run finished, rc =", rc, urc
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      if(urc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=urc)
 
      call ESMF_GridCompFinalize(compGridded, flowstate, flowstate, clock, rc=rc, userRc=urc)
      print *, "Coupled Flow Component Finalize finished, rc =", rc, urc
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      if(urc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=urc)
!EOC
 
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOE
!
! !DESCRIPTION:
! \subsubsection{Example of Object Destruction:}
!
!     Near the end of the application, call object destroy methods to 
!     clean up the objects previously created:
!EOE
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!BOC
      call ESMF_StateDestroy(flowstate, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

      call ESMF_GridDestroy(grid, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

      call ESMF_ClockDestroy(clock, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

      call ESMF_GridCompDestroy(compGridded, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
!EOC

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
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

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

!BOE
!
! !DESCRIPTION:
! \subsubsection{Example of ESMF Finalize:}
!
!EOE
!BOC
      !Call ESMF_Finalize at the end of an ESMF application:
      call ESMF_Finalize(rc=rc)
!EOC

      end program ESMF_ApplicationWrapper
    
