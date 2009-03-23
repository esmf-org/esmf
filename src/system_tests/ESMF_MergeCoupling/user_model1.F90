! $Id: user_model1.F90,v 1.22 2009/03/23 20:40:48 theurich Exp $
!
! Example/test code which shows User Component calls.


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  User-written component, one of two which supply data which is merged
!  in the coupler.
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

        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, user_init, rc=rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, user_run, rc=rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, user_final, rc=rc)

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
        type(ESMF_VM) :: vm
        type(ESMF_DELayout) :: layout
        type(ESMF_IGrid) :: igrid1
        type(ESMF_Array) :: array1
        type(ESMF_ArraySpec) :: arrayspec
        integer, dimension(:,:), pointer :: idata
        real(ESMF_KIND_R8) :: g_min(2), g_max(2)
        integer :: counts(2)
        integer :: npets, de_id
        type(ESMF_IGridHorzStagger) :: horz_stagger
        integer :: status

        print *, "User Comp Init starting"

        ! Query component for VM and create a layout with the right breakdown
        call ESMF_GridCompGet(comp, vm=vm, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10
        call ESMF_VMGet(vm, petCount=npets, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10
        layout = ESMF_DELayoutCreate(vm, (/ npets, 1 /), rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10

        ! Add a "humidity" field to the export state.
        counts(1) = 40
        counts(2) = 20
        g_min(1) = 0.0
        g_max(1) = 20.0
        g_min(2) = 0.0
        g_max(2) = 5.0
        horz_stagger = ESMF_IGRID_HORZ_STAGGER_A

        igrid1 = ESMF_IGridCreateHorzXYUni(counts=counts, &
                                minGlobalCoordPerDim=g_min, &
                                maxGlobalCoordPerDim=g_max, &
                                horzStagger=horz_stagger, &
                                name="source igrid", rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10
        call ESMF_IGridDistribute(igrid1, delayout=layout, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10

        ! Figure out our local processor id
        call ESMF_DELayoutGetDeprecated(layout, localDe=de_id, rc=rc)
        if (status .ne. ESMF_SUCCESS) goto 10

        ! Set up a 2D integer array
        call ESMF_ArraySpecSet(arrayspec, rank=2, &
                               typekind=ESMF_TYPEKIND_I4)

        ! Create the field and have it create the array internally
        humidity = ESMF_FieldCreate(igrid1, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                                    name="humidity", rc=rc)
        if (status .ne. ESMF_SUCCESS) goto 10

        ! Get the allocated array back and get an F90 array pointer
        call ESMF_FieldGetArray(humidity, array1, rc)
        if (status .ne. ESMF_SUCCESS) goto 10
        call ESMF_ArrayGetData(array1, idata, rc=rc)
        if (status .ne. ESMF_SUCCESS) goto 10

        ! Set initial data values over exclusive domain to the de identifier
        idata = de_id

        call ESMF_StateAddField(exportState, humidity, rc)
        if (status .ne. ESMF_SUCCESS) goto 10
        call ESMF_StatePrint(exportState, rc=rc)
        if (status .ne. ESMF_SUCCESS) goto 10

        print *, "User Comp Init returning"
   
10 continue
        rc = status

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
    
