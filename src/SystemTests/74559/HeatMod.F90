! $Id: HeatMod.F90,v 1.3 2003/04/25 22:10:50 nscollins Exp $
!
! Example/test code which generates injections of heat.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!   Inject heat and fluid flow into another model.
!
!
!\begin{verbatim}

    module HeatFlowMod

    ! ESMF Framework module
    use ESMF_Mod
    use ArraysGlobalMod
    
    implicit none
    
    type heatdata
       integer :: last_inject_time
       integer :: min_temp
       integer :: max_temp
    end type

    type wrapper
      type(heatdata), pointer :: ptr
    end type

   
    public HeatMod_register
        
    contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
    subroutine HeatMod_register(comp, rc)
        type(ESMF_GridComp), intent(inout) :: comp
        integer, intent(out) :: rc

        ! local variables
        type(heatdata), pointer :: heatblock
        type(wrapper) :: wrap

        print *, "in user register routine"

        ! Register the callback routines.

        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, &
                                               heat_init, ESMF_SINGLEPHASE, rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, &
                                                heat_run, ESMF_SINGLEPHASE, rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, &
                                              heat_final, ESMF_SINGLEPHASE, rc)

        print *, "Registered Initialize, Run, and Finalize routines"


        allocate(heatblock)

        heatblock%last_inject_time = -1
        heatblock%min_temp = 50
        heatblock%max_temp = 100

        wrap%ptr => heatblock
        call ESMF_GridCompSetInternalState(comp, wrap, rc)

        print *, "Registered Private Data block for Internal State"
    end subroutine

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
subroutine heat_init(gcomp, importstate, exportstate, clock, rc)
      type(ESMF_GridComp), intent(inout) :: gcomp
      type(ESMF_State), intent(inout) :: importstate, exportstate
      type(ESMF_Clock), intent(in) :: clock
      integer, intent(out) :: rc

      !
      ! Local variables
      !
      integer :: status
      logical :: rcpresent
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
      ! Calculate some other quantities
      !
      dx = (x_max - x_min)/i_max      ! Should be calls to PhysGrid
      dy = (y_max - y_min)/j_max
      !
      ! Query component for information.
      !
      call ESMF_GridCompGet(gcomp, layout=layout, rc=status)
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
                             name="source grid", rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in Heat_init:  grid create"
        return
      endif

      call ESMF_GridCompSet(gcomp, grid=grid, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in Heat_init:  grid comp set"
        return
      endif

      !
      ! create space for Heat Flow arrays
      !
      call ArraysGlobalAlloc(grid, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in Heat_init:  arraysglobalalloc"
        return
      endif

      ! This is adding names only to the export list, marked by default
      !  as "not needed".  The coupler will consult with the other component
      !  and mark the ones which are needed.
      call ESMF_StateAddData(exportstate, "SIE", rc)
      call ESMF_StateAddData(exportstate, "U", rc)
      call ESMF_StateAddData(exportstate, "V", rc)
      call ESMF_StateAddData(exportstate, "RHO", rc)
      call ESMF_StateAddData(exportstate, "P", rc)
      call ESMF_StateAddData(exportstate, "Q", rc)
      call ESMF_StateAddData(exportstate, "FLAG", rc)

      call ESMF_StatePrint(exportstate, rc=rc)

      print *, "HeatMod Init returning"
   
    end subroutine heat_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
    subroutine heat_run(comp, importstate, exportstate, clock, rc)
        type(ESMF_GridComp), intent(inout) :: comp
        type(ESMF_State), intent(inout) :: importstate, exportstate
        type(ESMF_Clock), intent(in) :: clock
        integer, intent(out) :: rc

      ! Local variables
        type(ESMF_Field) :: field_sie, field_u, field_v
        type(ESMF_Array) :: array_sie, array_u, array_v
        real, dimension(:,:), pointer :: data_sie, data_u, data_v
        integer :: status
        type(mylocaldata), pointer :: heatblock
        type(wrapper) :: wrap

        print *, "HeatMod Run starting"

        ! Get our local info
        call ESMF_GridCompGetInternalState(comp, wrap, status)
        heatblock => wrap%ptr

        print *, "run, last_inject_time = ", heatblock%last_inject_time

        ! Get the Field and Bundle data from the State
        call ESMF_StateGetData(exportstate, "SIE", field_sie, rc=status)
        call ESMF_FieldGetData(field_sie, array_sie, rc=rc) 
        call ESMF_ArrayGetData(array_sie, data_sie, ESMF_DATA_REF, rc)
            
        call ESMF_StateGetData(exportstate, "U", field_u, rc=status)
        call ESMF_FieldGetData(field_u, array_u, rc=rc) 
        call ESMF_ArrayGetData(array_u, data_u, ESMF_DATA_REF, rc)

        call ESMF_StateGetData(exportstate, "V", field_v, rc=status)
        call ESMF_FieldGetData(field_v, array_v, rc=rc) 
        call ESMF_ArrayGetData(array_v, data_v, ESMF_DATA_REF, rc)
      
        ! Start with no injection anywhere
        data_sie = 0.0
     
        ! TODO:
        ! If enough time has passed, set values to positive heat.
        ! and update inject time

        call ESMF_StatePrint(exportstate, rc=status)
 
        print *, "HeatMod Run returning"

        rc = status

    end subroutine heat_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
    subroutine heat_final(comp, importstate, exportstate, clock, rc)
        type(ESMF_GridComp), intent(inout) :: comp
        type(ESMF_State), intent(inout) :: importstate, exportstate
        type(ESMF_Clock), intent(in) :: clock
        integer, intent(out) :: rc

        ! Local variables
        integer :: status
        type(ESMF_Field) :: field_sie, field_u, field_v
        type(mylocaldata), pointer :: heatblock
        type(wrapper) :: wrap

        print *, "HeatMod Final starting"
    
        call ESMF_StateGetData(exportstate, "SIE", field_sie, rc=status)
        call ESMF_FieldDestroy(field_sie, rc=status)

        call ESMF_StateGetData(exportstate, "U Velocity", field_u, rc=status)
        call ESMF_FieldDestroy(field_u, rc=status)

        call ESMF_StateGetData(exportstate, "V Velocity", field_v, rc=status)
        call ESMF_FieldDestroy(field_v, rc=status)


        ! Get our local info
        nullify(wrap%ptr)
        heatblock => wrap%ptr
        call ESMF_GridCompGetInternalState(comp, wrap, status)

        heatblock => wrap%ptr
        deallocate(heatblock, stat=status)
        nullify(wrap%ptr)

        print *, "HeatMod Final returning"
   
    end subroutine heat_final


    end module HeatFlowMod
    
!\end{verbatim}
    
