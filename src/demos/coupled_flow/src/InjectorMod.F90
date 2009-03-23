! $Id: InjectorMod.F90,v 1.8 2009/03/23 20:40:48 theurich Exp $
!
!-------------------------------------------------------------------------
!BOP
! !MODULE: InjectorMod - Fluid Injection Component
!
! !DESCRIPTION:
!   This is a user-supplied fluid injection component which interacts 
!   with a separate fluid flow model component by altering the inflow 
!   boundary conditions during a user-specifed time interval.
!   The energy, velocity, and density of the inflow fluid during
!   the injection time interval are user-specified.  
!   The location of the inflow is 
!   determined by the fluid flow model component through a set of
!   boundary condition flags which are supplied to this component
!   in the import state.  The energy, velocity, and density fields
!   of the calculation are updated by this component and returned
!   to the fluid flow solver for the next computational time step
!   in the export state.
!
!
!EOP

    module InjectorMod

    ! ESMF module
    use ESMF_Mod
    use InjectArraysMod
    
    implicit none
    
    ! Private data block
    type injectdata
       type(ESMF_Time) :: inject_start_time
       type(ESMF_Time) :: inject_stop_time
       real :: inject_energy
       real :: inject_velocity
       real :: inject_density
       ! padding to prevent issues on nag
       integer:: pad_1
       integer:: pad_2
       integer:: pad_3
       integer:: pad_4
    end type

    type wrapper
      type(injectdata), pointer :: ptr
    end type

   
    ! External entry point which will register the Init, Run, and Finalize
    !  routines for this Component.
    public Injector_register
        
    contains

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: Injector_register - Set the Init, Run, Final routines

! !INTERFACE:
      subroutine Injector_register(comp, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: comp
      integer, intent(out) :: rc
!
! !DESCRIPTION:
!     User-written registration routine.  This is the
!     only public entry point for this Component.  When this is called by
!     a higher level component it will register with the Framework the
!     subroutines to be called when the Framework needs to Initialize,
!     Run, or Finalize this Component.
!
!     The arguments are:
!     \begin{description}
!     \item[comp]
!          A Gridded Component.
!     \item[rc]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors,
!          {\tt ESMF\_FAILURE} othewise.
!     \end{description}
!
!EOPI

        ! local variables
        type(injectdata), pointer :: datablock
        type(wrapper) :: wrap

        print *, "in user register routine"

        ! Register the callback routines.
        !
        !  This Component has a 2 phase initialization, and a single
        !   phase run and finalize.
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, injector_init1, 1, rc=rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, injector_init2, 2, rc=rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, injector_run, rc=rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, injector_final, rc=rc)

        print *, "InjectorMod: Registered Initialize, Run, and Finalize routines"


        ! Allocate private persistent space
        allocate(datablock)
        wrap%ptr => datablock
        call ESMF_GridCompSetInternalState(comp, wrap, rc)

        print *, "InjectorMod: Registered Private Data block for Internal State"
    end subroutine

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: User Initialization routine, phase 1

! !INTERFACE:
      subroutine injector_init1(gcomp, importState, exportState, clock, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: gcomp
      type(ESMF_State), intent(inout) :: importState, exportState
      type(ESMF_Clock), intent(inout) :: clock
      integer, intent(out) :: rc
!
! !DESCRIPTION:
!     User-supplied Initialization routine.  Sets up data space,
!     and marks which Fields in the export state can be produced
!     by this Component.  The Coupler will mark which Fields are
!     needed by whatever other Component(s) are coupled with this
!     Component.  The second phase of the Init process will place
!     the actual Field data in the export state.
!
!     The arguments are:
!     \begin{description}
!     \item[gcomp]
!           A Gridded Component.
!     \item[importState]
!           State containing the import list.
!     \item[exportState]
!           State containing the export list.
!     \item[clock]
!           Clock describing the external time.
!     \item[rc]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      !
      ! Local variables
      !
      type(ESMF_DELayout) :: layout
      type(ESMF_IGrid) :: igrid
      integer :: on_month, on_day, on_hour, on_min
      integer :: off_month, off_day, off_hour, off_min
      real :: in_energy, in_velocity, in_rho
      type(injectdata), pointer :: datablock
      type(wrapper) :: wrap
      namelist /input/ on_month, on_day, on_hour, on_min, &
                       off_month, off_day, off_hour, off_min, &
                       in_energy, in_velocity, in_rho
!BOP
!
! !DESCRIPTION:
! \subsection{Namelist Input Parameters for Injector:}
!     The following variables must be input to the Injector Component to run.
!     They are located in a file called "coupled\_inject\_input."
!
!     The variables are:
!     \begin{description}
!     \item [on\_month]
!           Injector start time month (integer).
!     \item [on\_day]
!           Injector start time day (integer).
!     \item [on\_hour]
!           Injector start time hour (integer).
!     \item [on\_min]
!           Injector start time minute (integer).
!     \item [off\_month]
!           Injector stop time month (integer).
!     \item [off\_day]
!           Injector stop time day (integer).
!     \item [off\_hour]
!           Injector stop time hour (integer).
!     \item [off\_min]
!           Injector stop time minute (integer).
!     \item [in\_energy]
!           Standard internal energy of the injector flow.
!     \item [in\_velocity]
!           Vertical velocity of the injector flow.
!     \item [in\_rho]
!           Density of the injector flow.
!     \end{description}
!
!EOP
      !
      ! Set initial values
      !
      rc = ESMF_FAILURE
   
      !
      ! Read in input file
      !
      open(10, status="old", file="coupled_inject_input",action="read",iostat=rc)
      if (rc .ne. 0) then
        print *, "Error!  Failed to open namelist file 'coupled_inject_input' "
        stop
      endif
      read(10, input, end=20)
   20 continue

      ! Set peristent values in saved data block
      call ESMF_GridCompGetInternalState(gcomp, wrap, rc)
      datablock => wrap%ptr

      ! initialize start time to 12May2003, 3:00 pm
      ! for testing, initialize start time to 13May2003, 2:00 pm
      call ESMF_TimeSet(datablock%inject_start_time, &
                        yy=2003, mm=on_month, dd=on_day, &
                        h=on_hour, m=on_min, s=0, rc=rc)

      ! initialize stop time to 13May2003, 2:00 pm
      call ESMF_TimeSet(datablock%inject_stop_time, &
                        yy=2003, mm=off_month, dd=off_day, &
                        h=off_hour, m=off_min, s=0, rc=rc)


      datablock%inject_energy = in_energy
      datablock%inject_velocity = in_velocity
      datablock%inject_density = in_rho

      !
      ! Query component for information.
      !
      call ESMF_GridCompGet(gcomp, igrid=igrid, rc=rc)
      call ESMF_IGridGet(igrid, delayout=layout, rc=rc)
      if (rc .ne. ESMF_SUCCESS) then
         print *, "ERROR in injector_init: getting info from component"
         return
      endif

      !
      ! create space for data arrays
      !
      call InjectArraysAlloc(igrid, rc)
      if(rc .NE. ESMF_SUCCESS) then
        print *, "ERROR in injector_init:  injectarraysalloc"
        return
      endif

      ! For initialization, add all fields to the import state.  Only the ones
      !  needed will be copied over to the export state for coupling.
      !  These are empty and will be filled in by the first run of the 
      !  Coupler.
      call ESMF_StateAdd(importState, field_sie, rc)
      call ESMF_StateAdd(importState, field_u, rc)
      call ESMF_StateAdd(importState, field_v, rc)
      call ESMF_StateAdd(importState, field_rho, rc)
      call ESMF_StateAdd(importState, field_p, rc)
      call ESMF_StateAdd(importState, field_q, rc)
      call ESMF_StateAdd(importState, field_flag, rc)

      ! This is adding names only to the export list, marked by default
      !  as "not needed". The coupler will mark the ones needed based
      !  on the requirements of the component(s) this is coupled to.
      call ESMF_StateAdd(exportState, "SIE", rc)
      call ESMF_StateAdd(exportState, "U", rc)
      call ESMF_StateAdd(exportState, "V", rc)
      call ESMF_StateAdd(exportState, "RHO", rc)
      call ESMF_StateAdd(exportState, "P", rc)
      call ESMF_StateAdd(exportState, "Q", rc)
      call ESMF_StateAdd(exportState, "FLAG", rc)

! Give the export state an initial set of values for the SIE Field.
      call ESMF_StateAdd(exportState, field_sie, rc)

      rc = ESMF_SUCCESS

      end subroutine injector_init1

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE:  injector_init2 - second phase of injector init

! !INTERFACE:
      subroutine injector_init2(gcomp, importState, exportState, clock, rc)
!
! !ARGUMENTS:
     type(ESMF_GridComp), intent(inout) :: gcomp
     type(ESMF_State), intent(inout) :: importState, exportState
     type(ESMF_Clock), intent(inout) :: clock
     integer, intent(out) :: rc
!
! !DESCRIPTION:
!     User-supplied second phase of Initialization.  This updates the
!     export state with the actual Fields which are required to be
!     provided to other Components.
!
!     The arguments are:
!     \begin{description}
!     \item[comp] 
!           A Gridded Component.
!     \item[importState]
!           State containing the import list.
!     \item[exportState]
!           State containing the export list.
!     \item[clock] 
!           Clock describing the external time.
!     \item[rc] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors,
!          otherwise {\tt ESMF\_FAILURE}.
!     \end{description}
!
!EOPI
      integer :: i, datacount
      character(len=ESMF_MAXSTR), dimension(7) :: datanames
      type(ESMF_Field) :: thisfield

      !
      ! Set initial values
      !
      rc = ESMF_FAILURE
   
      ! All possible export data fields.
      datacount = 7
      datanames(1) = "SIE"
      datanames(2) = "U"
      datanames(3) = "V"
      datanames(4) = "RHO"
      datanames(5) = "P"
      datanames(6) = "Q"
      datanames(7) = "FLAG"

      ! Update any required fields in the export state
      do i=1, datacount

         ! check isneeded flag here
         if (.not. ESMF_StateIsNeeded(importState, datanames(i), rc)) then 
             cycle
         endif

         call ESMF_StateGet(importState, datanames(i), thisfield, rc=rc)
         call ESMF_StateAdd(exportState, thisfield, rc=rc)

      enddo

    end subroutine injector_init2

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE:  injector_run - injector run routine

! !INTERFACE:
      subroutine injector_run(comp, importState, exportState, clock, rc)
!
! !ARGUMENTS:
     type(ESMF_GridComp), intent(inout) :: comp
     type(ESMF_State), intent(inout) :: importState, exportState
     type(ESMF_Clock), intent(inout) :: clock
     integer, intent(out) :: rc
!
! !DESCRIPTION:
!     User-supplied Run routine.  Examines the Flag data for locations
!     where fluid is to be injected.  Examines the Clock for the current
!     time.  If it is during the injection period, the specified energy,
!     velocity, and density data values are overwritten by this Component.
!
!     The arguments are:
!     \begin{description}
!     \item[comp] 
!           A Gridded Component.
!     \item[importState]
!           State containing the import list.
!     \item[exportState]
!           State containing the export list.
!     \item[clock] 
!           Clock describing the external time.
!     \item[rc]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors,
!          otherwise {\tt ESMF\_FAILURE}.
!     \end{description}
!
!EOPI
      ! Local variables
        type(ESMF_Field) :: thisfield
        type(ESMF_Field) :: local_sie, local_v, local_rho, local_flag
        real(kind=ESMF_KIND_R4), dimension(:,:), pointer :: data_sie, data_v
        real(kind=ESMF_KIND_R4), dimension(:,:), pointer :: data_rho, data_flag
        type(ESMF_Time) :: currtime
        type(injectdata), pointer :: datablock
        type(wrapper) :: wrap

        integer :: i, j, datacount
        character(len=ESMF_MAXSTR), dimension(7) :: datanames


        ! All possible export data fields.
        datacount = 7
        datanames(1) = "SIE"
        datanames(2) = "U"
        datanames(3) = "V"
        datanames(4) = "RHO"
        datanames(5) = "P"
        datanames(6) = "Q"
        datanames(7) = "FLAG"

        ! Get our local info
        call ESMF_GridCompGetInternalState(comp, wrap, rc)
        datablock => wrap%ptr


        ! Get the Field and FieldBundle data from the State that we might update
        call ESMF_StateGet(importState, "SIE", local_sie, rc=rc)
        call ESMF_StateGet(importState, "V", local_v, rc=rc)
        call ESMF_StateGet(importState, "RHO", local_rho, rc=rc)
        call ESMF_StateGet(importState, "FLAG", local_flag, rc=rc)
      
        ! Get the Field and FieldBundle data from the State, and a pointer to
        !  the existing data (not a copy).
        call ESMF_FieldGetDataPointer(local_sie, data_sie, ESMF_DATA_REF, rc=rc)
            
        call ESMF_FieldGetDataPointer(local_v, data_v, ESMF_DATA_REF, rc=rc)
      
        call ESMF_FieldGetDataPointer(local_rho, data_rho, ESMF_DATA_REF, rc=rc)
      
        call ESMF_FieldGetDataPointer(local_flag, data_flag, ESMF_DATA_REF, rc=rc)
          
        ! Update values.  Flag = 10 means override values with our own.

        ! Check time to see if we are still injecting
        call ESMF_ClockGet(clock, currTime=currtime, rc=rc)
        if ((currtime .ge. datablock%inject_start_time) .and. &
            (currtime .le. datablock%inject_stop_time)) then

            ! Set injection values
            do j = jmin, jmax
              do i = imin, imax
                if (data_flag(i,j).eq.10) then
                  data_sie(i,j) = datablock%inject_energy
                  data_v(i,j) = datablock%inject_velocity
                  data_rho(i,j) = datablock%inject_density
                endif
              enddo
            enddo

        else

            ! Set default constant values.
            do j = jmin, jmax
              do i = imin, imax
                if (data_flag(i,j).eq.10) then
                  data_sie(i,j) = 200.0
                  data_v(i,j) = 0.0
                  data_rho(i,j) = 6.0
                endif
              enddo
            enddo

        endif 
 
        ! Update any required fields in the export state
        do i=1, datacount

           ! check isneeded flag here
           if (.not. ESMF_StateIsNeeded(importState, datanames(i), rc)) then 
               cycle
           endif

           call ESMF_StateGet(importState, datanames(i), thisfield, rc=rc)
           call ESMF_StateAdd(exportState, thisfield, rc=rc)

        enddo

    end subroutine injector_run

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE:  injector_final - finalize routine

! !INTERFACE:
      subroutine injector_final(comp, importState, exportState, clock, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: comp
      type(ESMF_State), intent(inout) :: importState, exportState
      type(ESMF_Clock), intent(inout) :: clock
      integer, intent(out) :: rc
!
! !DESCRIPTION:
!     User-supplied finalize routine.  Release space allocated
!      by this component.
!
!     The arguments are:
!     \begin{description}
!     \item[comp] 
!           A Gridded Component.
!     \item[importState]
!           State containing the import list.
!     \item[exportState]
!           State containing the export list.
!     \item[clock] 
!           Clock describing the external time.
!     \item[rc] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors,
!          otherwise {\tt ESMF\_FAILURE}.
!     \end{description}
!
!EOPI
        ! Local variables
        type(injectdata), pointer :: datablock
        type(wrapper) :: wrap
        integer :: allocrc

        print *, "Injector Finalize starting"
    
        ! Release our field and data space
        call InjectArraysDealloc(rc)


        ! Get our local info and release it
        nullify(wrap%ptr)
        datablock => wrap%ptr
        call ESMF_GridCompGetInternalState(comp, wrap, rc)

        datablock => wrap%ptr
        deallocate(datablock, stat=allocrc)
        nullify(wrap%ptr)

        print *, "Injector Finalize returning"

        rc = ESMF_SUCCESS
   
    end subroutine injector_final

    end module InjectorMod
    
