! $Id: InjectorMod.F90,v 1.15 2004/04/27 17:22:20 nscollins Exp $
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
    private
    
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

        rc = ESMF_SUCCESS

    end subroutine

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
subroutine injector_init(gcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp), intent(inout) :: gcomp
      type(ESMF_State), intent(inout) :: importState, exportState
      type(ESMF_Clock), intent(in) :: clock
      integer, intent(out) :: rc

      !
      ! Local variables
      !
      integer :: npets
      type(ESMF_VM) :: vm
      type(ESMF_newDELayout) :: layout
      type(ESMF_Grid) :: grid
      real(ESMF_KIND_R8) :: g_min(2), g_max(2)
      real(ESMF_KIND_R8) :: x_min, x_max, y_min, y_max
      integer :: counts(2)
      real :: in_energy, in_velocity, in_rho
      integer :: printout
      type(ESMF_GridType) :: horz_gridtype
      type(ESMF_GridStagger) :: horz_stagger
      type(ESMF_CoordSystem) :: horz_coord_system
      type(injectdata), pointer :: datablock
      type(wrapper) :: wrap
      namelist /input/ counts, x_min, x_max, y_min, y_max, &
                       printout, in_energy, in_velocity, in_rho


      !
      ! Set initial values
      !
      rc = ESMF_FAILURE
      ! TODO: reorder the input namelist so we can read directly into
      !  the g_min and g_max values.  
      g_min(1) = x_min
      g_max(1) = x_max
      g_min(2) = y_min
      g_max(2) = y_max
   
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
      datablock%gregorianCalendar = ESMF_CalendarCreate("Gregorian", &
                                                        ESMF_CAL_GREGORIAN, rc)

      ! initialize start time to 12May2003, 2:00 pm
      ! for testing, initialize start time to 12May2003, 2:00 pm
      call ESMF_TimeSet(datablock%inject_start_time, &
                                 yy=2003, mm=5, dd=12, h=14, &
                                 calendar=datablock%gregorianCalendar, rc=rc)

      ! initialize stop time to 13May2003, 2:00 pm
      call ESMF_TimeSet(datablock%inject_stop_time, &
                                 yy=2003, mm=5, dd=13, h=14, &
                                 calendar=datablock%gregorianCalendar, rc=rc)

      datablock%inject_energy = in_energy
      datablock%inject_velocity = in_velocity
      datablock%inject_density = in_rho

      !
      ! Query component for number of PETs, and create a layout.
      !

      call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
      call ESMF_VMGet(vm, petCount=npets, rc=rc)
      layout = ESMF_newDELayoutCreate(vm, (/ npets/4, 4 /), rc=rc)

      !
      ! Create the Grid
      !
      horz_gridtype = ESMF_GridType_XY
      horz_stagger = ESMF_GridStagger_C_NE
      horz_coord_system = ESMF_CoordSystem_Cartesian

      grid = ESMF_GridCreateLogRectUniform(2, counts=counts, &
                             minGlobalCoordPerDim=g_min, &
                             maxGlobalCoordPerDim=g_max, &
                             delayout=layout, &
                             horzGridType=horz_gridtype, &
                             horzStagger=horz_stagger, &
                             horzCoordSystem=horz_coord_system, &
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
      call ESMF_StateAddData(importState, field_sie, rc)
      call ESMF_StateAddData(importState, field_u, rc)
      call ESMF_StateAddData(importState, field_v, rc)
      call ESMF_StateAddData(importState, field_rho, rc)
      call ESMF_StateAddData(importState, field_p, rc)
      call ESMF_StateAddData(importState, field_q, rc)
      call ESMF_StateAddData(importState, field_flag, rc)

      ! This is adding names only to the export list, marked by default
      !  as "not needed". The coupler will mark the ones needed based
      !  on the requirements of the component(s) this is coupled to.
      call ESMF_StateAddData(exportState, "SIE", rc)
      call ESMF_StateAddData(exportState, "U", rc)
      call ESMF_StateAddData(exportState, "V", rc)
      call ESMF_StateAddData(exportState, "RHO", rc)
      call ESMF_StateAddData(exportState, "P", rc)
      call ESMF_StateAddData(exportState, "Q", rc)
      call ESMF_StateAddData(exportState, "FLAG", rc)

      !! DEBUG: these are here so we can run w/o the coupler to debug
      !!  code.  remove these lines later.
      call ESMF_StateAddData(exportState, field_sie, rc)
      call ESMF_StateAddData(exportState, field_v, rc)
      call ESMF_StateAddData(exportState, field_rho, rc)
      call ESMF_StateAddData(exportState, field_flag, rc)

      rc = ESMF_SUCCESS

    end subroutine injector_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
    subroutine injector_run(comp, importState, exportState, clock, rc)
        type(ESMF_GridComp), intent(inout) :: comp
        type(ESMF_State), intent(inout) :: importState, exportState
        type(ESMF_Clock), intent(in) :: clock
        integer, intent(out) :: rc

      ! Local variables
        type(ESMF_Field) :: thisfield
        type(ESMF_Field) :: local_sie, local_v, local_rho, local_flag
        type(ESMF_Array) :: array_sie, array_v, array_rho, array_flag
        real(kind=ESMF_KIND_R4), dimension(:,:), pointer :: data_sie, data_v
        real(kind=ESMF_KIND_R4), dimension(:,:), pointer :: data_rho, data_flag
        type(ESMF_Time) :: currtime
        type(injectdata), pointer :: datablock
        type(wrapper) :: wrap

        ! debug
        integer, save :: counter = 1
        integer :: ilb, jlb, iub, jub

        integer :: i, j, datacount
        character(len=ESMF_MAXSTR), dimension(7) :: datanames

      
        !print *, "Import States at start of injector run"
        !call ESMF_StatePrint(importState, "", rc)

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
        call ESMF_StateGetData(importState, "SIE", local_sie, rc=rc)
        call ESMF_StateGetData(importState, "V", local_v, rc=rc)
        call ESMF_StateGetData(importState, "RHO", local_rho, rc=rc)
        call ESMF_StateGetData(importState, "FLAG", local_flag, rc=rc)
      
        ! Get the Field and Bundle data from the State, and a pointer to
        !  the existing data (not a copy).
        call ESMF_FieldGetArray(local_sie, array_sie, rc=rc) 
        call ESMF_ArrayGetData(array_sie, data_sie, ESMF_DATA_REF, rc)
            
        call ESMF_FieldGetArray(local_v, array_v, rc=rc) 
        call ESMF_ArrayGetData(array_v, data_v, ESMF_DATA_REF, rc)
      
        call ESMF_FieldGetArray(local_rho, array_rho, rc=rc) 
        call ESMF_ArrayGetData(array_rho, data_rho, ESMF_DATA_REF, rc)
      
        call ESMF_FieldGetArray(local_flag, array_flag, rc=rc) 
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

           call ESMF_StateGetData(importState, datanames(i), thisfield, rc=rc)
           call ESMF_StateAddData(exportState, thisfield, rc=rc)

        enddo

        rc = ESMF_SUCCESS

    end subroutine injector_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
    subroutine injector_final(comp, importState, exportState, clock, rc)
        type(ESMF_GridComp), intent(inout) :: comp
        type(ESMF_State), intent(inout) :: importState, exportState
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
        call ESMF_CalendarDestroy(datablock%gregorianCalendar, rc)
        deallocate(datablock, stat=rc)
        nullify(wrap%ptr)

        print *, "Injector Finalize returning"

        rc = ESMF_SUCCESS
   
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
      integer :: de_id
      type(ESMF_Array) :: outarray
      type(ESMF_Grid) :: grid
      type(ESMF_newDELayout) :: layout
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
      call ESMF_GridCompGet(gcomp, grid=grid, rc=status)
      call ESMF_GridGet(grid, delayout=layout, rc=status)
      call ESMF_newDELayoutGet(layout, localDe=de_id, rc=status)
      !
      ! Collect results on DE 0 and output to a file
      !
      call ESMF_FieldGather(field_sie, 0, outarray, rc=status)
      if (de_id .eq. 0) then
        write(filename, 20)  "SIE", file_no
        call ESMF_ArrayWrite(outarray, filename=filename, rc=status)
        call ESMF_ArrayDestroy(outarray, status)
      endif

      call ESMF_FieldGather(field_u, 0, outarray, rc=status)
      if (de_id .eq. 0) then
        write(filename, 20)  "U_velocity", file_no
        call ESMF_ArrayWrite(outarray, filename=filename, rc=status)
        call ESMF_ArrayDestroy(outarray, status)
      endif

      call ESMF_FieldGather(field_v, 0, outarray, rc=status)
      if (de_id .eq. 0) then
        write(filename, 20)  "V_velocity", file_no
        call ESMF_ArrayWrite(outarray, filename=filename, rc=status)
        call ESMF_ArrayDestroy(outarray, status)
      endif

      if(file_no .eq. 1) then
        call ESMF_FieldGather(field_flag, 0, outarray, rc=status)
        if (de_id .eq. 0) then
          write(filename, 20)  "FLAG", file_no
          call ESMF_ArrayWrite(outarray, filename=filename, rc=status)
          call ESMF_ArrayDestroy(outarray, status)
        endif
      endif

      if(rcpresent) rc = ESMF_SUCCESS

 20   format(a,".",I3.3)

      end subroutine FlowPrint

    end module InjectorMod
    
!\end{verbatim}
    
