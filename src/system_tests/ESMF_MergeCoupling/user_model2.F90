! $Id: user_model2.F90,v 1.10 2004/06/15 13:34:44 nscollins Exp $
!
! Example/test code which shows User Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  User-supplied Component, most recent interface revision.
!
!
!\begin{verbatim}

    module user_model2

    ! ESMF Framework module
    use ESMF_Mod
    
    implicit none
    
    type mylocaldata
      ! instance specific data for ensembles
      integer :: runparam1
      integer :: runparam2
      real :: scale_factor
      ! plus whatever, including pointers to other stuff
    end type

    type wrapper
      type(mylocaldata), pointer :: ptr
    end type

   
    public userm2_register
        
    contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
    subroutine userm2_register(comp, rc)
        type(ESMF_GridComp), intent(inout) :: comp
        integer, intent(out) :: rc

        ! local variables
        type(mylocaldata), pointer :: mydatablock
        type(wrapper) :: wrap

        print *, "in user register routine"

        ! Register the callback routines.

        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, &
                                               user_init, ESMF_SINGLEPHASE, rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, &
                                                user_run, ESMF_SINGLEPHASE, rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, &
                                              user_final, ESMF_SINGLEPHASE, rc)

        print *, "Registered Initialize, Run, and Finalize routines"

        allocate(mydatablock)

        mydatablock%runparam1 = 100
        mydatablock%runparam2 = 5
        mydatablock%scale_factor = 0.66

        wrap%ptr => mydatablock
        call ESMF_GridCompSetInternalState(comp, wrap, rc)

        print *, "Registered Private Data block for Internal State"

        rc = ESMF_SUCCESS

    end subroutine

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
    subroutine user_init(comp, importState, exportState, clock, rc)
        type(ESMF_GridComp), intent(inout) :: comp
        type(ESMF_State), intent(inout) :: importState, exportState
        type(ESMF_Clock), intent(in) :: clock
        integer, intent(out) :: rc

!     ! Local variables
        type(ESMF_Field) :: humidity
        type(ESMF_DELayout) :: layout
        integer :: i, x, y
        type(ESMF_Grid) :: grid1
        type(ESMF_Array) :: array1
        type(ESMF_ArraySpec) :: arrayspec
        integer, dimension(:,:), pointer :: idata
        integer :: nDE_i, nDE_j
        real(ESMF_KIND_R8) :: g_min(2), g_max(2)
        integer :: counts(2)
        integer :: ni, nj, de_id
        type(ESMF_GridType) :: horz_gridtype
        type(ESMF_GridStagger) :: horz_stagger
        type(ESMF_CoordSystem) :: horz_coord_system
        integer :: status, myde

        print *, "User Comp Init starting"

        ! query comp for layout
        call ESMF_GridCompGet(comp, delayout=layout, rc=status)

        ! Add a "humidity" field to the export state.
        counts(1) = 40
        counts(2) = 20
        g_min(1) = 0.0
        g_max(1) = 20.0
        g_min(2) = 0.0
        g_max(2) = 5.0
        horz_gridtype = ESMF_GridType_XY
        horz_stagger = ESMF_GridStagger_A
        horz_coord_system = ESMF_CoordSystem_Cartesian

        grid1 = ESMF_GridCreateLogRectUniform(2, counts=counts, &
                                minGlobalCoordPerDim=g_min, &
                                maxGlobalCoordPerDim=g_max, &
                                layout=layout, &
                                horzGridType=horz_gridtype, &
                                horzStagger=horz_stagger, &
                                horzCoordSystem=horz_coord_system, &
                                name="source grid", rc=status)

        ! Figure out our local processor id
        call ESMF_DELayoutGet(layout, localDe=de_id, rc=rc)

        ! Set up a 2D integer array
        call ESMF_ArraySpecSet(arrayspec, rank=2, type=ESMF_DATA_INTEGER, &
                                kind=ESMF_I4)

        ! Create the field and have it create the array internally
        humidity = ESMF_FieldCreate(grid1, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                                         name="humidity", rc=rc)

        ! Get the allocated array back and get an F90 array pointer
        call ESMF_FieldGetArray(humidity, array1, rc)
        call ESMF_ArrayGetData(array1, idata, rc=rc)

        ! Set initial data values over exclusive domain to the de identifier
        idata = de_id

        call ESMF_StateAddField(exportState, humidity, rc)
        call ESMF_StatePrint(exportState, rc=rc)

        print *, "User Comp Init returning"
   
        rc = ESMF_SUCCESS

    end subroutine user_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
    subroutine user_run(comp, importState, exportState, clock, rc)
        type(ESMF_GridComp), intent(inout) :: comp
        type(ESMF_State), intent(inout) :: importState, exportState
        type(ESMF_Clock), intent(in) :: clock
        integer, intent(out) :: rc

!     ! Local variables
        type(ESMF_Field) :: humidity
        type(ESMF_Array) :: array1
        integer, dimension(:,:), pointer :: idata
        integer :: status
        type(mylocaldata), pointer :: mydatablock
        type(wrapper) :: wrap

        print *, "User Comp Run starting"

        ! Get our local info
        call ESMF_GridCompGetInternalState(comp, wrap, status)
        mydatablock => wrap%ptr

        print *, "run, scale_factor = ", mydatablock%scale_factor

        ! Get the Field and Bundle data from the State
        call ESMF_StateGetField(exportState, "humidity", humidity, rc=status)
      
        ! update field values here
        call ESMF_FieldGetArray(humidity, array1, rc=rc) 
        ! Get a pointer to the start of the data
        call ESMF_ArrayGetData(array1, idata, ESMF_DATA_REF, rc)

        ! increment data values in place
        idata = idata + 10
     

        call ESMF_StatePrint(exportState, rc=status)
        call ESMF_FieldPrint(humidity, rc=status)
        call ESMF_ArrayPrint(array1, "", rc=status)
 
        print *, "User Comp Run returning"

        rc = status

    end subroutine user_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
    subroutine user_final(comp, importState, exportState, clock, rc)
        type(ESMF_GridComp), intent(inout) :: comp
        type(ESMF_State), intent(inout) :: importState, exportState
        type(ESMF_Clock), intent(in) :: clock
        integer, intent(out) :: rc

        ! Local variables
        integer :: status
        type(mylocaldata), pointer :: mydatablock
        type(wrapper) :: wrap

        print *, "User Comp Final starting"
    
        ! Get our local info
        nullify(wrap%ptr)
        mydatablock => wrap%ptr
        call ESMF_GridCompGetInternalState(comp, wrap, status)

        mydatablock => wrap%ptr
        print *, "before dealloc, runparam1 = ", mydatablock%runparam1
        deallocate(mydatablock, stat=status)
        print *, "deallocate returned ", status
        nullify(wrap%ptr)

        print *, "User Comp Final returning"
   
        rc = ESMF_SUCCESS

    end subroutine user_final


    end module user_model2
    
!\end{verbatim}
    
