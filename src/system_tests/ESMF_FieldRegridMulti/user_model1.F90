! $Id: user_model1.F90,v 1.14 2004/04/19 20:34:51 jwolfe Exp $
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

    module user_model1

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
   
    public userm1_register
        
    contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
    subroutine userm1_register(comp, rc)
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

       ! Local variables
        type(ESMF_Field) :: humidity
        type(ESMF_newDELayout) :: delayout
        type(ESMF_DataMap) :: datamap
        integer :: i, x, y, n
        type(ESMF_Grid) :: grid1
        type(ESMF_Array) :: array1
        type(ESMF_ArraySpec) :: arrayspec
        type(ESMF_AxisIndex), dimension(ESMF_MAXGRIDDIM) :: index
        real(ESMF_KIND_R8), dimension(:,:,:), pointer :: idata
        integer :: nDE_i, nDE_j
        real(ESMF_KIND_R8) :: min(2), max(2)
        integer :: counts(3), order(3)
        integer :: ni, nj, nk, de_id
        type(ESMF_GridType) :: horz_gridtype, vert_gridtype
        type(ESMF_GridStagger) :: horz_stagger, vert_stagger
        type(ESMF_CoordSystem) :: horz_coord_system, vert_coord_system
        integer :: status, myde

        print *, "User Comp Init starting"

        ! query comp for layout
        call ESMF_GridCompGet(comp, delayout=delayout, rc=status)

        ! Add a "humidity" field to the export state.
        counts(1) = 4
        counts(2) = 60
        counts(3) = 40
        min(1) = 0.0
        max(1) = 60.0
        min(2) = 0.0
        max(2) = 50.0
        horz_gridtype = ESMF_GridType_XY
        horz_stagger = ESMF_GridStagger_A
        horz_coord_system = ESMF_CoordSystem_Cartesian

        grid1 = ESMF_GridCreateLogRectUniform(2, counts=counts(2:3), &
                                minGlobalCoordPerDim=min, &
                                maxGlobalCoordPerDim=max, &
                                delayout=delayout, &
                                horzGridType=horz_gridtype, &
                                horzStagger=horz_stagger, &
                                horzCoordSystem=horz_coord_system, &
                                name="source grid", rc=status)

        ! Figure out our local processor id
        call ESMF_newDELayoutGet(delayout, localDE=de_id, rc=rc)

        ! Set up a 3D real array
        call ESMF_ArraySpecSet(arrayspec, rank=3, type=ESMF_DATA_REAL, &
                                kind=ESMF_R8)

        ! Set up a datamap to tell the framework which of the 2 axes
        ! correspond to the grid, and which one is multiple scalar
        ! values for the same grid cell.
        order(1) = 0
        order(2) = 1
        order(3) = 2
        call ESMF_DataMapInit(datamap, 3, order, counts=counts(1:1), rc=rc)

        ! Create the field 
        humidity = ESMF_FieldCreate(grid1, arrayspec=arrayspec, datamap=datamap, &
                                    horzRelloc=ESMF_CELL_CENTER, &
                                    haloWidth=0, name="humidity", rc=rc)

        ! Get the allocated array back and get an F90 array pointer
        call ESMF_FieldGetArray(humidity, array1, rc)
        call ESMF_ArrayGetData(array1, idata, rc=rc)

        ! Set initial data values over whole array to our de id
        idata(:,:,:) = real(de_id)

        call ESMF_StateAddData(exportState, humidity, rc)
     !   call ESMF_StatePrint(exportState, rc=rc)

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

       ! Local variables
        type(ESMF_Field) :: humidity
        type(ESMF_RelLoc) :: relloc
        type(ESMF_Array) :: array1
        type(ESMF_Array), dimension(:), pointer :: coordArray
        type(ESMF_Grid) :: grid
        real(ESMF_KIND_R8) :: pi
        real(ESMF_KIND_R8), dimension(:,:,:), pointer :: idata
        real(ESMF_KIND_R8), dimension(:,:), pointer :: coordX, coordY
        integer :: status, i, j, k, counts(3)
        type(mylocaldata), pointer :: mydatablock
        type(wrapper) :: wrap

        print *, "User Comp Run starting"

        pi = 3.14159

        ! Get our local info
        call ESMF_GridCompGetInternalState(comp, wrap, status)
        mydatablock => wrap%ptr

        print *, "run, scale_factor = ", mydatablock%scale_factor

        ! Get the Field and Bundle data from the State
        call ESMF_StateGetData(exportState, "humidity", humidity, rc=status)
      
        ! get the grid and coordinates
        allocate(coordArray(2))
        call ESMF_FieldGetRelLoc(humidity, horzRelloc=relloc, rc=status)
        call ESMF_FieldGet(humidity, grid=grid, rc=status)
        call ESMF_GridGetCoord(grid, horzRelloc=relloc, &
                               centerCoord=coordArray, rc=status)
        call ESMF_ArrayGetData(coordArray(1), coordX, ESMF_DATA_REF, status)
        call ESMF_ArrayGetData(coordArray(2), coordY, ESMF_DATA_REF, status)

        ! update field values here
        ! call ESMF_StateGetDataPointer(exportState, "humidity", idata, rc=rc)
        call ESMF_FieldGetArray(humidity, array1, rc=rc) 
        ! get size of local array
        call ESMF_ArrayGet(array1, counts=counts, rc=rc)
        ! Get a pointer to the start of the data
        call ESMF_ArrayGetData(array1, idata, ESMF_DATA_REF, rc)

        ! increment data values in place - where do i get the third dim?
        do j = 1,counts(3)
          do i = 1,counts(2)
             do k = 1,counts(1)
                 idata(k,i,j) = 10.0 + 5.0*sin(coordX(i,j)/60.0*pi) &
                                     + 2.0*sin(coordY(i,j)/50.0*pi)
             enddo
          enddo
        enddo

     !   call ESMF_StatePrint(exportState, rc=status)
     !   call ESMF_FieldPrint(humidity, rc=status)
     !   call ESMF_ArrayPrint(array1, "", rc=status)
 
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


    end module user_model1
    
!\end{verbatim}
    
