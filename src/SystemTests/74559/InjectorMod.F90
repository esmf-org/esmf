! $Id: InjectorMod.F90,v 1.8 2003/05/02 19:26:02 nscollins Exp $
!

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!   Inject energy and fluid flow into another model.
!   Example of a user-supplied Component.
!
!
!\begin{verbatim}

    module InjectorMod

    ! ESMF Framework module
    use ESMF_Mod
    use InjectArraysMod
    
    implicit none
    
    ! Private data block
    type injectdata
       type(ESMF_Calendar) :: gregorianCalendar
       type(ESMF_Time) :: inject_start_time
       type(ESMF_Time) :: inject_stop_time
       real :: inject_energy
       real :: inject_velocity
       real :: inject_density
    end type

    type wrapper
      type(injectdata), pointer :: ptr
    end type

   
    ! External entry point which will register the Init, Run, and Finalize
    !  routines for this Component.
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

        print *, "in user register routine"

        ! Register the callback routines.

        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, &
                                               injector_init, ESMF_SINGLEPHASE, rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, &
                                                injector_run, ESMF_SINGLEPHASE, rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, &
                                              injector_final, ESMF_SINGLEPHASE, rc)

        print *, "Registered Initialize, Run, and Finalize routines"


        ! Allocate private persistent space
        allocate(datablock)
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
      real :: in_energy, in_velocity, in_rho
      integer :: printout
      integer :: horz_gridtype, vert_gridtype
      integer :: horz_stagger, vert_stagger
      integer :: horz_coord_system, vert_coord_system
      integer :: myde, halo_width
      type(injectdata), pointer :: datablock
      type(wrapper) :: wrap
      namelist /input/ i_max, j_max, x_min, x_max, y_min, y_max, &
                       printout, in_energy, in_velocity, in_rho


      !
      ! Set initial values
      !
      rc = ESMF_FAILURE
   
      !
      ! Read in input file
      !
      open(10, status="old", file="coupled_inject_input")
      read(10, input, end=20)
   20 continue

      ! Set peristent values in saved data block
      call ESMF_GridCompGetInternalState(gcomp, wrap, rc)
      datablock => wrap%ptr

      ! initialize calendar to be Gregorian type
      call ESMF_CalendarInit(datablock%gregorianCalendar, &
                                          ESMF_CAL_GREGORIAN, rc)

      ! initialize start time to 12May2003, 3:00 pm
      ! for testing, initialize start time to 13May2003, 2:00 pm
      call ESMF_TimeInit(datablock%inject_start_time, &
                                 YR=int(2003,kind=ESMF_IKIND_I8), &
                                 MM=5, DD=12, H=14, M=0, &
                                 S=int(0,kind=ESMF_IKIND_I8), &
                                 cal=datablock%gregorianCalendar, rc=rc)

      ! initialize stop time to 13May2003, 2:00 pm
      call ESMF_TimeInit(datablock%inject_stop_time, &
                                 YR=int(2003,kind=ESMF_IKIND_I8), &
                                 MM=5, DD=13, H=14, M=0, &
                                 S=int(0,kind=ESMF_IKIND_I8), &
                                 cal=datablock%gregorianCalendar, rc=rc)


      datablock%inject_energy = in_energy
      datablock%inject_velocity = in_velocity
      datablock%inject_density = in_rho

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
      call InjectArraysAlloc(grid, rc)
      if(rc .NE. ESMF_SUCCESS) then
        print *, "ERROR in injector_init:  injectarraysalloc"
        return
      endif

      ! For initialization, add all fields to the import state.  Only the ones
      !  needed will be copied over to the export state for coupling.
      !  These are empty and will be filled in by the first run of the 
      !  Coupler.
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

      !! DEBUG: these are here so we can run w/o the coupler to debug
      !!  code.  remove these lines later.
      call ESMF_StateAddData(exportstate, field_sie, rc)
      call ESMF_StateAddData(exportstate, field_v, rc)
      call ESMF_StateAddData(exportstate, field_rho, rc)
      call ESMF_StateAddData(exportstate, field_flag, rc)

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
        type(ESMF_Field) :: thisfield
        type(ESMF_Field) :: local_sie, local_v, local_rho, local_flag
        type(ESMF_Array) :: array_sie, array_v, array_rho, array_flag
        real(kind=ESMF_IKIND_R4), dimension(:,:), pointer :: data_sie, data_v
        real(kind=ESMF_IKIND_R4), dimension(:,:), pointer :: data_rho, data_flag
        type(ESMF_Time) :: currtime
        type(injectdata), pointer :: datablock
        type(wrapper) :: wrap

        ! debug
        integer, save :: counter = 1
        integer :: ilb, jlb, iub, jub

        integer :: i, j, datacount
        character(len=ESMF_MAXSTR), dimension(7) :: datanames

      
        !print *, "Import States at start of injector run"
        !call ESMF_StatePrint(importstate, "", rc)

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


        ! Get the Field and Bundle data from the State that we might update
        call ESMF_StateGetData(importstate, "SIE", local_sie, rc=rc)
        call ESMF_StateGetData(importstate, "V", local_v, rc=rc)
        call ESMF_StateGetData(importstate, "RHO", local_rho, rc=rc)
        call ESMF_StateGetData(importstate, "FLAG", local_flag, rc=rc)
      
        ! Get the Field and Bundle data from the State, and a pointer to
        !  the existing data (not a copy).
        call ESMF_FieldGetData(local_sie, array_sie, rc=rc) 
        call ESMF_ArrayGetData(array_sie, data_sie, ESMF_DATA_REF, rc)
            
        call ESMF_FieldGetData(local_v, array_v, rc=rc) 
        call ESMF_ArrayGetData(array_v, data_v, ESMF_DATA_REF, rc)
      
        call ESMF_FieldGetData(local_rho, array_rho, rc=rc) 
        call ESMF_ArrayGetData(array_rho, data_rho, ESMF_DATA_REF, rc)
      
        call ESMF_FieldGetData(local_flag, array_flag, rc=rc) 
        call ESMF_ArrayGetData(array_flag, data_flag, ESMF_DATA_REF, rc)
          
        !! DEBUG.
        ! Debug checks for data pointers, exclusive region only
        ilb = lbound(data_flag, 1) + 1
        jlb = lbound(data_flag, 2) + 1
        iub = ubound(data_flag, 1) - 1
        jub = ubound(data_flag, 2) - 1
        if ((ilb .ne. imin) .or. (jlb .ne. jmin) .or. (iub .ne. imax) &
            .or. (jub .ne. jmax)) then
            print *, "!!!----Injector-----------!!!"
            print *, "run counter = ", counter
            call ESMF_ClockPrint(clock, "currtime string", rc)
            print *, "!!! i lbound = ", ilb, " imin = ", imin, " !!!"
            print *, "!!! j lbound = ", jlb, " jmin = ", jmin, " !!!"
            print *, "!!! i ubound = ", iub, " imax = ", imax, " !!!"
            print *, "!!! j ubound = ", jub, " jmax = ", jmax, " !!!"
       endif
       counter = counter + 1
       !! DEBUG.
    
        ! Update values.  Flag = 10 means override values with our own.

        ! Check time to see if we are still injecting
        call ESMF_ClockGetCurrTime(clock, currtime, rc)
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
           if (.not. ESMF_StateIsNeeded(importstate, datanames(i), rc)) then 
               cycle
           endif

           call ESMF_StateGetData(importstate, datanames(i), thisfield, rc=rc)
           call ESMF_StateAddData(exportstate, thisfield, rc=rc)

        enddo

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
        call InjectArraysDealloc(rc)


        ! Get our local info and release it
        nullify(wrap%ptr)
        datablock => wrap%ptr
        call ESMF_GridCompGetInternalState(comp, wrap, rc)

        datablock => wrap%ptr
        deallocate(datablock, stat=rc)
        nullify(wrap%ptr)

        print *, "Injector Finalize returning"
   
    end subroutine injector_final


!------------------------------------------------------------------------------

      subroutine FlowPrint(gcomp, clock, file_no, rc)

      type(ESMF_GridComp) :: gcomp
      type(ESMF_Clock) :: clock
      integer, intent(in) :: file_no
      integer, optional, intent(out) :: rc

      !
      ! Local variables
      !
      integer :: status
      logical :: rcpresent
      integer :: ni, nj, i, j, de_id
      type(ESMF_Array) :: outarray
      type(ESMF_Grid) :: grid
      type(ESMF_DELayout) :: layout
      character(len=ESMF_MAXSTR) :: filename
      !
      ! Set initial values
      !
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      !
      ! Initialize return code
      !
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif
      ! 
      ! Get our layout from the component, and our de number
      !
      call ESMF_GridCompGet(gcomp, layout=layout, rc=status)
      call ESMF_DELayoutGetDEID(layout, de_id, rc=status)
      !
      ! Collect results on DE 0 and output to a file
      !
      call ESMF_FieldAllGather(field_sie, outarray, status)
      if (de_id .eq. 0) then
        write(filename, 20)  "SIE", file_no
        call ESMF_ArrayWrite(outarray, filename=filename, rc=status)
      endif
      call ESMF_ArrayDestroy(outarray, status)

      call ESMF_FieldAllGather(field_u, outarray, status)
      if (de_id .eq. 0) then
        write(filename, 20)  "U_velocity", file_no
        call ESMF_ArrayWrite(outarray, filename=filename, rc=status)
      endif
      call ESMF_ArrayDestroy(outarray, status)

      call ESMF_FieldAllGather(field_v, outarray, status)
      if (de_id .eq. 0) then
        write(filename, 20)  "V_velocity", file_no
        call ESMF_ArrayWrite(outarray, filename=filename, rc=status)
      endif
      call ESMF_ArrayDestroy(outarray, status)

      if(file_no .eq. 1) then
        call ESMF_FieldAllGather(field_flag, outarray, status)
        if (de_id .eq. 0) then
          write(filename, 20)  "FLAG", file_no
          call ESMF_ArrayWrite(outarray, filename=filename, rc=status)
        endif
        call ESMF_ArrayDestroy(outarray, status)
      endif

      if(rcpresent) rc = ESMF_SUCCESS

 20   format(a,".",I3.3)

      end subroutine FlowPrint

    end module InjectorMod
    
!\end{verbatim}
    
