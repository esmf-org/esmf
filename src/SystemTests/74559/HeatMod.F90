! $Id: HeatMod.F90,v 1.1 2003/04/17 17:31:22 nscollins Exp $
!
! Example/test code which generates random injections of heat.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!   Randomly perturb the temperature of another model.
!
!
!\begin{verbatim}

    module HeatMod

    ! ESMF Framework module
    use ESMF_Mod
    
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
 
    
    subroutine heat_init(comp, importstate, exportstate, clock, rc)
        type(ESMF_GridComp), intent(inout) :: comp
        type(ESMF_State), intent(inout) :: importstate, exportstate
        type(ESMF_Clock), intent(in) :: clock
        integer, intent(out) :: rc

!     ! Local variables
        type(ESMF_Field) :: field_sie, field_u, field_v
        type(ESMF_DELayout) :: layout
        integer :: i, x, y
        type(ESMF_Grid) :: grid1
        type(ESMF_Array) :: array_sie, array_u, array_v
        type(ESMF_AxisIndex), dimension(ESMF_MAXGRIDDIM) :: index
        real, dimension(:,:), pointer :: data_sie, data_u, data_v
        type(ESMF_Array) :: array_sie, array_u, array_v
        integer :: nDE_i, nDE_j
        real :: x_min, x_max, y_min, y_max
        integer :: i_max, j_max
        integer :: ni, nj, de_id
        integer :: horz_gridtype, vert_gridtype
        integer :: horz_stagger, vert_stagger
        integer :: horz_coord_system, vert_coord_system
        integer :: status, myde

        print *, "HeatMod Init starting"

        ! query comp for layout
        call ESMF_GridCompGet(comp, layout=layout, rc=status)

        ! Add an energy, u velocity and v velocity field to the state.
        i_max = 40
        j_max = 20
        x_min = 0.0
        x_max = 20.0
        y_min = 0.0
        y_max = 5.0
        horz_gridtype = ESMF_GridType_XY
        vert_gridtype = ESMF_GridType_Unknown
        horz_stagger = ESMF_GridStagger_A
        vert_stagger = ESMF_GridStagger_Unknown
        horz_coord_system = ESMF_CoordSystem_Cartesian
        vert_coord_system = ESMF_CoordSystem_Unknown

        grid1 = ESMF_GridCreate(i_max=i_max, j_max=j_max, &
                                x_min=x_min, x_max=x_max, &
                                y_min=y_min, y_max=y_max, &
                                layout=layout, &
                                horz_gridtype=horz_gridtype, &
                                vert_gridtype=vert_gridtype, &
                                horz_stagger=horz_stagger, &
                                vert_stagger=vert_stagger, &
                                horz_coord_system=horz_coord_system, &
                                vert_coord_system=vert_coord_system, &
                                name="source grid", rc=status)

        ! Figure out our local processor id
        call ESMF_DELayoutGetDEID(layout, de_id, rc)

        ! Get size of our exclusive domain
        call ESMF_GridGetDE(grid1, lcellexc_index=index, rc=rc)
        ni = index(1)%r - index(1)%l + 1
        nj = index(2)%r - index(2)%l + 1

        print *, "allocating", ni, " by ",nj," cells on DE", de_id
        allocate(data_sie(ni,nj))
        allocate(data_u  (ni,nj))
        allocate(data_v  (ni,nj))

        ! Set initial data values over whole array to 0
        data_sie = 0.0
        data_u   = 0.0
        data_v   = 0.0

        ! Create Array based on an existing, allocated F90 pointer.
        ! Data is type Real, 1D.
        array_sie = ESMF_ArrayCreate(data_sie, ESMF_NO_COPY, rc)
        array_u   = ESMF_ArrayCreate(data_u, ESMF_NO_COPY, rc)
        array_v   = ESMF_ArrayCreate(data_v, ESMF_NO_COPY, rc)
        print *, "Array Create returned"

        field_sie = ESMF_FieldCreate(grid1, array1, relloc=ESMF_CELL_CENTER, &
                                                    name="SIE", rc=rc)
        field_u   = ESMF_FieldCreate(grid1, array1, relloc=ESMF_CELL_CENTER, &
                                                    name="U Velocity", rc=rc)
        field_v   = ESMF_FieldCreate(grid1, array1, relloc=ESMF_CELL_CENTER, &
                                                    name="V Velocity", rc=rc)


        call ESMF_StateAddData(exportstate, field_sie, rc)
        call ESMF_StateAddData(exportstate, field_u  , rc)
        call ESMF_StateAddData(exportstate, field_v  , rc)
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

!     ! Local variables
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
        call ESMF_ArrayGetData(array_sie, data_sie, ESMF_NO_COPY, rc)
            
        call ESMF_StateGetData(exportstate, "U Velocity", field_u, rc=status)
        call ESMF_FieldGetData(field_u, array_u, rc=rc) 
        call ESMF_ArrayGetData(array_u, data_u, ESMF_NO_COPY, rc)

        call ESMF_StateGetData(exportstate, "V Velocity", field_v, rc=status)
        call ESMF_FieldGetData(field_v, array_v, rc=rc) 
        call ESMF_ArrayGetData(array_v, data_v, ESMF_NO_COPY, rc)
      
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


    end module user_model1
    
!\end{verbatim}
    
