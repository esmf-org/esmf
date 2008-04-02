! $Id: user_model1.F90,v 1.14 2008/04/02 20:43:03 cdeluca Exp $
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

#ifdef ESMF_TESTWITHTHREADS
        ! The following call will turn on ESMF-threading (single threaded)
        ! for this component. If you are using this file as a template for 
        ! your own code development you probably don't want to include the 
        ! following call unless you are interested in exploring ESMF's 
        ! threading features.
        call ESMF_GridCompSetVMMinThreads(comp, rc=rc)
#endif

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

        !!if (present(importState)) print *, "importState present"
        !!if (.not.present(importState)) print *, "importState *not* present"
        !!if (present(exportState)) print *, "exportState present"
        !!if (.not.present(exportState)) print *, "exportState *not* present"
        !!if (present(clock)) print *, "clock present"
        !!if (.not.present(clock)) print *, "clock *not* present"
        !!if (present(rc)) print *, "rc present"
        !!if (.not.present(rc)) print *, "rc *not* present"

        ! query comp for layout
        call ESMF_GridCompGet(comp, layout=layout, rc=status)

        ! Add a "humidity" field to the export state.
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

        !!if (present(importState)) print *, "importState present"
        !!if (.not.present(importState)) print *, "importState *not* present"
        !!if (present(exportState)) print *, "exportState present"
        !!if (.not.present(exportState)) print *, "exportState *not* present"
        !!if (present(clock)) print *, "clock present"
        !!if (.not.present(clock)) print *, "clock *not* present"
        !!if (present(rc)) print *, "rc present"
        !!if (.not.present(rc)) print *, "rc *not* present"

        ! Get our local info
        call ESMF_GridCompGetInternalState(comp, wrap, status)
        mydatablock => wrap%ptr

        print *, "run, scale_factor = ", mydatablock%scale_factor

        ! Get the Field and FieldBundle data from the State
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
    
        !!if (present(importState)) print *, "importState present"
        !!if (.not.present(importState)) print *, "importState *not* present"
        !!if (present(exportState)) print *, "exportState present"
        !!if (.not.present(exportState)) print *, "exportState *not* present"
        !!if (present(clock)) print *, "clock present"
        !!if (.not.present(clock)) print *, "clock *not* present"
        !!if (present(rc)) print *, "rc present"
        !!if (.not.present(rc)) print *, "rc *not* present"

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
    
