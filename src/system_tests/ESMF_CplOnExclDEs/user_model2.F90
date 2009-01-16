! $Id: user_model2.F90,v 1.14 2009/01/16 05:28:25 theurich Exp $
!
! Example/test code which shows User Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  User-supplied Component
!
!
!\begin{verbatim}

    module user_model2

    ! ESMF Framework module
    use ESMF_Mod

    implicit none
    
    public userm2_register
        
    type mylocaldata
      integer :: dataoffset
    end type

    type wrapper
      type(mylocaldata), pointer :: ptr
    end type

    contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
    subroutine userm2_register(comp, rc)
        type(ESMF_GridComp) :: comp
        integer :: rc

        ! local variables
        type(mylocaldata), pointer :: mydatablock
        type(wrapper) :: wrap

        print *, "In user register routine"

        ! Register the callback routines.

        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, &
                                            user_init, ESMF_SINGLEPHASE, rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, &
                                            user_run, ESMF_SINGLEPHASE, rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, &
                                            user_final, ESMF_SINGLEPHASE, rc)

        print *, "Registered Initialize, Run, and Finalize routines"


        allocate(mydatablock)

        mydatablock%dataoffset = 52

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
        type(ESMF_IGrid) :: igrid1
        type(ESMF_Array) :: array1
        type(ESMF_ArraySpec) :: arrayspec
        integer, dimension(:,:), pointer :: idata
        integer :: nDE_i, nDE_j
        real(ESMF_KIND_R8) :: x_min, x_max, y_min, y_max
        integer :: counts(2)
        integer :: ni, nj, de_id
        integer :: horz_igridtype, vert_igridtype
        integer :: horz_stagger, vert_stagger
        integer :: horz_coord_system, vert_coord_system
        integer :: status, myde

        print *, "User Comp Init starting"

        ! Initially import state contains a field with a igrid but no data.
        call ESMF_GridCompGet(comp, layout=layout, rc=status)

        ! Add a "humidity" field to the import state.
        counts(1) = 40
        counts(2) = 20
        x_min = 0.0
        x_max = 20.0
        y_min = 0.0
        y_max = 5.0
        horz_igridtype = ESMF_IGridType_XY
        vert_igridtype = ESMF_IGridType_Unknown
        horz_stagger = ESMF_IGridStagger_A
        vert_stagger = ESMF_IGridStagger_Unknown
        horz_coord_system = ESMF_CoordSystem_Cartesian
        vert_coord_system = ESMF_CoordSystem_Unknown

        igrid1 = ESMF_IGridCreate(counts=counts, &
                                x_min=x_min, x_max=x_max, &
                                y_min=y_min, y_max=y_max, &
                                layout=layout, &
                                horz_igridtype=horz_igridtype, &
                                vert_igridtype=vert_igridtype, &
                                horz_stagger=horz_stagger, &
                                vert_stagger=vert_stagger, &
                                horz_coord_system=horz_coord_system, &
                                vert_coord_system=vert_coord_system, &
                                name="source igrid", rc=status)

        ! Figure out our local processor id
        call ESMF_DELayoutGetDEID(layout, de_id, rc)

        ! Set up a 2D integer array
        call ESMF_ArraySpecSet(arrayspec, rank=2, &
                               typekind=ESMF_TYPEKIND_I4)

        ! Create the field and have it create the array internally
        humidity = ESMF_FieldCreate(igrid1, arrayspec, relloc=ESMF_CELL_CENTER, &
                                         name="humidity", rc=rc)

        ! Get the allocated array back and get an F90 array pointer
        call ESMF_FieldGetArray(humidity, array1, rc)
        call ESMF_ArrayGetData(array1, idata, rc=rc)

        ! Set initial data values over exclusive domain to the de identifier
        idata = de_id


        call ESMF_StateAddField(importState, humidity, rc)
        call ESMF_StatePrint(importState, rc=rc)

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
        integer :: status

        print *, "User Comp Run starting"

        ! Get information from the component.
        call ESMF_StatePrint(importState, rc=status)
        call ESMF_StateGetField(importState, "humidity", humidity, rc=status)
        call ESMF_FieldPrint(humidity, "", rc=status)
    

        ! This is where the model specific computation goes.
        call ESMF_FieldGetArray(humidity, array1, rc=status)
        print *, "Imported Array in user model 2:"
        call ESMF_ArrayPrint(array1, "", rc)

 
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
        print *, "before deallocate, dataoffset = ", mydatablock%dataoffset
        deallocate(mydatablock, stat=status)
        print *, "deallocate returned ", status
        nullify(wrap%ptr)


        print *, "User Comp Final returning"
   
        rc = ESMF_SUCCESS

    end subroutine user_final


    end module user_model2
    
!\end{verbatim}
    
