! $Id: InjectorMod.F90,v 1.4 2003/04/29 21:41:37 nscollins Exp $
!

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!   Inject energy and fluid flow into another model.
!
!
!\begin{verbatim}

    module InjectorMod

    ! ESMF Framework module
    use ESMF_Mod
    use ArraysGlobalMod
    
    implicit none
    
    type injectdata
       type(ESMF_Time) :: inject_start_time
       type(ESMF_Time) :: inject_stop_time
       real :: inject_energy
       real :: inject_velocity
       real :: inject_density
    end type

    type wrapper
      type(injectdata), pointer :: ptr
    end type

   
    public Injector_register
        
    contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
    subroutine Injector_register(comp, rc)
        type(ESMF_GridComp), intent(inout) :: comp
        integer, intent(out) :: rc

        ! local variables
        type(injectdata), pointer :: datablock
        type(wrapper) :: wrap
        type(ESMF_Calendar) :: gregorianCalendar

        print *, "in user register routine"

        ! Register the callback routines.

        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, &
                                               injector_init, ESMF_SINGLEPHASE, rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, &
                                                injector_run, ESMF_SINGLEPHASE, rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, &
                                              injector_final, ESMF_SINGLEPHASE, rc)

        print *, "Registered Initialize, Run, and Finalize routines"


        allocate(datablock)

        ! initialize calendar to be Gregorian type
        call ESMF_CalendarInit(gregorianCalendar, ESMF_CAL_GREGORIAN, rc)

        ! initialize start time to 13May2003, 3:00 pm
        ! for testing, initialize start time to 13May2003, 9:00:06 am
        call ESMF_TimeInit(datablock%inject_start_time, &
                           YR=int(2003,kind=ESMF_IKIND_I8), &
                           MM=5, DD=13, H=9, M=0, &
                           S=int(6,kind=ESMF_IKIND_I8), &
                           cal=gregorianCalendar, rc=rc)

        ! initialize stop time to 14May2003, 3:00 pm
        call ESMF_TimeInit(datablock%inject_stop_time, &
                           YR=int(2003,kind=ESMF_IKIND_I8), &
                           MM=5, DD=14, H=15, M=0, &
                           S=int(0,kind=ESMF_IKIND_I8), &
                           cal=gregorianCalendar, rc=rc)


        datablock%inject_energy = 600.0
        datablock%inject_velocity = 4.5
        datablock%inject_density = 2.0

        wrap%ptr => datablock
        call ESMF_GridCompSetInternalState(comp, wrap, rc)

        print *, "Registered Private Data block for Internal State"
    end subroutine

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
subroutine injector_init(gcomp, importstate, exportstate, clock, rc)
      type(ESMF_GridComp), intent(inout) :: gcomp
      type(ESMF_State), intent(inout) :: importstate, exportstate
      type(ESMF_Clock), intent(in) :: clock
      integer, intent(out) :: rc

      !
      ! Local variables
      !
      integer :: i, j
      type(ESMF_DELayout) :: layout
      type(ESMF_Grid) :: grid
      type(ESMF_AxisIndex), dimension(ESMF_MAXGRIDDIM) :: index
      real :: x_min, x_max, y_min, y_max
      integer :: i_max, j_max
      integer :: horz_gridtype, vert_gridtype
      integer :: horz_stagger, vert_stagger
      integer :: horz_coord_system, vert_coord_system
      integer :: myde, halo_width

      !
      ! Set initial values
      !
      rc = ESMF_FAILURE
   
      ! Grid parameters
      i_max = 156
      j_max = 72 
      x_min = 0.0
      x_max = 2.0e+05
      y_min = 0.0
      y_max = 5.0e+04

      !
      ! Query component for information.
      !
      call ESMF_GridCompGet(gcomp, layout=layout, rc=rc)

      !
      ! Create the Grid
      !
      horz_gridtype = ESMF_GridType_XY
      vert_gridtype = ESMF_GridType_Unknown
      horz_stagger = ESMF_GridStagger_A
      vert_stagger = ESMF_GridStagger_Unknown
      horz_coord_system = ESMF_CoordSystem_Cartesian
      vert_coord_system = ESMF_CoordSystem_Unknown
      halo_width = 1

      grid = ESMF_GridCreate(i_max=i_max, j_max=j_max, &
                             x_min=x_min, x_max=x_max, &
                             y_min=y_min, y_max=y_max, &
                             layout=layout, &
                             horz_gridtype=horz_gridtype, &
                             vert_gridtype=vert_gridtype, &
                             horz_stagger=horz_stagger, &
                             vert_stagger=vert_stagger, &
                             horz_coord_system=horz_coord_system, &
                             vert_coord_system=vert_coord_system, &
                             halo_width=halo_width, &
                             name="source grid", rc=rc)
      if(rc .NE. ESMF_SUCCESS) then
        print *, "ERROR in injector_init:  grid create"
        return
      endif

      call ESMF_GridCompSet(gcomp, grid=grid, rc=rc)
      if(rc .NE. ESMF_SUCCESS) then
        print *, "ERROR in injector_init:  grid comp set"
        return
      endif

      !
      ! create space for data arrays
      !
      call ArraysGlobalAlloc(grid, rc)
      if(rc .NE. ESMF_SUCCESS) then
        print *, "ERROR in injector_init:  arraysglobalalloc"
        return
      endif

      ! For initialization, add all fields to the import state.  Only the ones
      !  needed will be copied over to the export state for coupling.
      call ESMF_StateAddData(importstate, field_sie, rc)
      call ESMF_StateAddData(importstate, field_u, rc)
      call ESMF_StateAddData(importstate, field_v, rc)
      call ESMF_StateAddData(importstate, field_rho, rc)
      call ESMF_StateAddData(importstate, field_p, rc)
      call ESMF_StateAddData(importstate, field_q, rc)
      call ESMF_StateAddData(importstate, field_flag, rc)

      ! This is adding names only to the export list, marked by default
      !  as "not needed". The coupler will mark the ones needed based
      !  on the requirements of the component(s) this is coupled to.
      call ESMF_StateAddData(exportstate, "SIE", rc)
      call ESMF_StateAddData(exportstate, "U", rc)
      call ESMF_StateAddData(exportstate, "V", rc)
      call ESMF_StateAddData(exportstate, "RHO", rc)
      call ESMF_StateAddData(exportstate, "P", rc)
      call ESMF_StateAddData(exportstate, "Q", rc)
      call ESMF_StateAddData(exportstate, "FLAG", rc)

      call ESMF_StatePrint(exportstate, rc=rc)

    end subroutine injector_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
    subroutine injector_run(comp, importstate, exportstate, clock, rc)
        type(ESMF_GridComp), intent(inout) :: comp
        type(ESMF_State), intent(inout) :: importstate, exportstate
        type(ESMF_Clock), intent(in) :: clock
        integer, intent(out) :: rc

      ! Local variables
        type(ESMF_Field) :: thisfield, field_sie, field_v, field_rho, field_flag
        type(ESMF_Array) :: array_sie, array_v, array_rho, array_flag
        real, dimension(:,:), pointer :: data_sie, data_v, data_rho, data_flag
        type(ESMF_Time) :: currtime
        type(injectdata), pointer :: datablock
        type(wrapper) :: wrap

        integer :: i, datacount
        character(len=ESMF_MAXSTR), dimension(7) :: datanames

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


        ! Get the Field and Bundle data from the State that we might update
        call ESMF_StateGetData(importstate, "SIE", field_sie, rc=rc)
        call ESMF_StateGetData(importstate, "V", field_v, rc=rc)
        call ESMF_StateGetData(importstate, "RHO", field_rho, rc=rc)
        call ESMF_StateGetData(importstate, "FLAG", field_flag, rc=rc)
      
        call ESMF_FieldHalo(field_sie, rc)
        call ESMF_FieldHalo(field_u, rc)
        call ESMF_FieldHalo(field_v, rc)
        call ESMF_FieldHalo(field_rho, rc)
        
        ! Check time to see if we are still injecting
        call ESMF_ClockGetCurrTime(clock, currtime, rc)
        if ((currtime .ge. datablock%inject_start_time) .and. &
            (currtime .le. datablock%inject_stop_time)) then

            ! Get pointers to data contents
            print *, "injecting energy, flow, density:"
            print *,  datablock%inject_energy, datablock%inject_velocity, &
                        datablock%inject_density
    
            ! Get the Field and Bundle data from the State
            call ESMF_FieldGetData(field_sie, array_sie, rc=rc) 
            call ESMF_ArrayGetData(array_sie, data_sie, ESMF_DATA_REF, rc)
                
            call ESMF_FieldGetData(field_v, array_v, rc=rc) 
            call ESMF_ArrayGetData(array_v, data_v, ESMF_DATA_REF, rc)
          
            call ESMF_FieldGetData(field_rho, array_rho, rc=rc) 
            call ESMF_ArrayGetData(array_rho, data_rho, ESMF_DATA_REF, rc)
          
            call ESMF_FieldGetData(field_flag, array_flag, rc=rc) 
            call ESMF_ArrayGetData(array_flag, data_flag, ESMF_DATA_REF, rc)
          
    
            ! Update values.  Flag = 10 means override values with our own.
            where (data_flag .eq. 10.0) 
    
                data_sie = datablock%inject_energy
                data_v = datablock%inject_velocity
                data_rho = datablock%inject_density
    
            end where
        endif 
 
        ! Update any required fields in the export state
        do i=1, datacount

           ! check isneeded flag here
           if (.not. ESMF_StateIsNeeded(importstate, datanames(i), rc)) then 
               cycle
           endif

           call ESMF_StateGetData(importstate, datanames(i), thisfield, rc=rc)
           call ESMF_StateAddData(exportstate, thisfield, rc=rc)

        enddo

        !call ESMF_StatePrint(exportstate, rc=rc)
 
    end subroutine injector_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
    subroutine injector_final(comp, importstate, exportstate, clock, rc)
        type(ESMF_GridComp), intent(inout) :: comp
        type(ESMF_State), intent(inout) :: importstate, exportstate
        type(ESMF_Clock), intent(in) :: clock
        integer, intent(out) :: rc

        ! Local variables
        type(injectdata), pointer :: datablock
        type(wrapper) :: wrap

        print *, "Injector Finalize starting"
    
        ! Release our field and data space
        call ArraysGlobalDealloc(rc)


        ! Get our local info and release it
        nullify(wrap%ptr)
        datablock => wrap%ptr
        call ESMF_GridCompGetInternalState(comp, wrap, rc)

        datablock => wrap%ptr
        deallocate(datablock, stat=rc)
        nullify(wrap%ptr)

        print *, "Injector Finalize returning"
   
    end subroutine injector_final


    end module InjectorMod
    
!\end{verbatim}
    
