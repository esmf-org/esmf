! $Id$
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
    use ESMF

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

        call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, user_init, rc=rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_RUN, user_run, rc=rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, user_final, rc=rc)

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
        type(ESMF_VM) :: vm
        type(ESMF_DELayout) :: delayout
        type(ESMF_IGrid) :: igrid1
        type(ESMF_ArraySpec) :: arrayspec
        real(ESMF_KIND_R8), dimension(:,:), pointer :: idata
        real(ESMF_KIND_R8) :: min(2), max(2)
        integer :: counts(ESMF_MAXIGRIDDIM)
        integer :: npets, pet_id
        type(ESMF_IGridHorzStagger) :: horzStagger
        integer :: status

        ! Query component for VM and create a layout with the right breakdown
        call ESMF_GridCompGet(comp, vm=vm, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10
        call ESMF_VMGet(vm, petCount=npets, localPet=pet_id, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10
        delayout = ESMF_DELayoutCreate(vm, (/ 2, npets/2 /), rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10

        print *, pet_id, "User Comp 1 Init starting"

        ! Add a "humidity" field to the export state.
        counts(1) = 60
        counts(2) = 40
        min(1) = 0.0
        max(1) = 60.0
        min(2) = 0.0
        max(2) = 50.0
        horzStagger = ESMF_IGRID_HORZ_STAGGER_A

        igrid1 = ESMF_IGridCreateHorzXYUni(counts=counts, &
                                         minGlobalCoordPerDim=min, &
                                         maxGlobalCoordPerDim=max, &
                                         horzStagger=horzStagger, &
                                         name="source igrid", rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10
        call ESMF_IGridDistribute(igrid1, delayout=delayout, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10

        ! Set up a 2D real array
        call ESMF_ArraySpecSet(arrayspec, rank=2, &
                               typekind=ESMF_TYPEKIND_R8)

        ! Create the field and have it create the array internally
        humidity = ESMF_FieldCreate(igrid1, arrayspec, &
                                    horzRelloc=ESMF_CELL_CENTER, &
                                    haloWidth=0, name="humidity", rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10

        ! Get the allocated array back and get an F90 array pointer
        call ESMF_FieldGetDataPointer(humidity, idata, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10

        ! Set initial data values over whole array to our de id
        idata = real(pet_id,ESMF_KIND_R8)

        call ESMF_StateAddField(exportState, (/humidity/), rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10
     !   call ESMF_StatePrint(exportState, rc=rc)

        print *, pet_id, "User Comp 1 Init returning"
   
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

       ! Local variables
        type(ESMF_Field) :: humidity
        type(ESMF_RelLoc) :: relloc
        type(ESMF_IGrid) :: igrid
        real(ESMF_KIND_R8) :: pi
        real(ESMF_KIND_R8), dimension(:,:), pointer :: idata, coordX, coordY
        integer :: status, i, j, counts(2)
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

        pi = 3.14159

        ! Get our local info
        call ESMF_GridCompGetInternalState(comp, wrap, status)
        mydatablock => wrap%ptr

        print *, "run, scale_factor = ", mydatablock%scale_factor

        ! Get the Field and FieldBundle data from the State
        call ESMF_StateGetField(exportState, "humidity", humidity, rc=status)
      
        ! get the igrid and coordinates
        call ESMF_FieldGet(humidity, igrid=igrid, horzRelloc=relloc, rc=status)
        call ESMF_IGridGetDELocalInfo(igrid, localCellCountPerDim=counts, &
                                     horzRelloc=ESMF_CELL_CENTER, rc=status)
        call ESMF_IGridGetCoord(igrid, dim=1, horzRelloc=relloc, &
                               centerCoord=coordX, rc=status)
        call ESMF_IGridGetCoord(igrid, dim=2, horzRelloc=relloc, &
                               centerCoord=coordY, rc=status)

        ! update field values here
        ! call ESMF_StateGetDataPointer(exportState, "humidity", idata, rc=rc)
        ! Get a pointer to the start of the data
        call ESMF_FieldGetDataPointer(humidity, idata, ESMF_DATACOPY_REFERENCE, rc=rc)

        ! increment data values in place
    !    idata = idata + 10.0
        do j   = 1,counts(2)
          do i = 1,counts(1)
            idata(i,j) = 10.0 + 5.0*sin(coordX(i,j)/60.0*pi) &
                              + 2.0*sin(coordY(i,j)/50.0*pi)
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
    
