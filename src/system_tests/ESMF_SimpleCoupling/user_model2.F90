! $Id: user_model2.F90,v 1.24 2007/04/03 16:36:27 cdeluca Exp $
!
! Example/test code which shows User Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  User-supplied Component, Version A (minimalist)
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
        type(ESMF_VM) :: vm
        type(ESMF_DELayout) :: layout
        type(ESMF_Grid) :: grid1
        type(ESMF_ArraySpec) :: arrayspec
        integer, dimension(:,:), pointer :: idata
        real(ESMF_KIND_R8) :: g_min(2), g_max(2)
        integer :: counts(2)
        integer :: npets, de_id
        type(ESMF_GridHorzStagger) :: horz_stagger
        integer :: status

        print *, "User Comp Init starting"

        ! Query component for VM and create a layout with the right breakdown
        call ESMF_GridCompGet(comp, vm=vm, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10
        call ESMF_VMGet(vm, petCount=npets, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10 
        layout = ESMF_DELayoutCreate(vm, (/ 2, npets/2 /), rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10
        
        ! and get our local de number
        call ESMF_DELayoutGetDeprecated(layout, localDE=de_id, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10
      

        ! Add a "humidity" field to the import state.
        counts(1) = 40
        counts(2) = 20
        g_min(1) = 0.0
        g_max(1) = 20.0
        g_min(2) = 0.0
        g_max(2) = 5.0
        horz_stagger = ESMF_GRID_HORZ_STAGGER_A

        grid1 = ESMF_GridCreateHorzXYUni(counts=counts, &
                                minGlobalCoordPerDim=g_min, &
                                maxGlobalCoordPerDim=g_max, &
                                horzStagger=horz_stagger, &
                                name="source grid", rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10
        call ESMF_GridDistribute(grid1, delayout=layout, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10

        ! Set up a 2D integer array
        call ESMF_ArraySpecSet(arrayspec, rank=2, &
                               typekind=ESMF_TYPEKIND_I4)

        ! Create the field and have it create the array internally
        humidity = ESMF_FieldCreate(grid1, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                                         name="humidity", rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10

        ! Get the allocated array back and get an F90 array pointer
        call ESMF_FieldGetDataPointer(humidity, idata, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10

        ! Set initial data values over exclusive domain to the de identifier
        idata = de_id

        call ESMF_StateAddField(importState, humidity, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10
        call ESMF_StatePrint(importState, rc=status)
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
        integer :: status

        print *, "User Comp Run starting"

        ! Get information from the component.
        call ESMF_StatePrint(importState, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10
        call ESMF_StateGetField(importState, "humidity", humidity, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10
        call ESMF_FieldPrint(humidity, "", rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10
    

        ! This is where the model specific computation goes.
!        call ESMF_FieldGetArray(humidity, array1, rc=status)
!        if (status .ne. ESMF_SUCCESS) goto 10
        print *, "Imported Array in user model 2:"
!        call ESMF_ArrayPrint(array1, "", rc=status)

 
        print *, "User Comp Run returning"

10 continue
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
    
