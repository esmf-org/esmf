! $Id: user_model1.F90,v 1.7 2004/12/07 23:36:32 jwolfe Exp $
!
! System test for Exclusive Components.  User-code, component 1.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  User-written code, supplies functions for component 1.
!  Creates a Field which is passed back thru the Export State.
!
!\begin{verbatim}

    module user_model1

    ! ESMF Framework module
    use ESMF_Mod

    implicit none
    
    public userm1_register
        
    contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
    subroutine userm1_register(comp, rc)
        type(ESMF_GridComp), intent(inout) :: comp
        integer, intent(out) :: rc

        integer :: localrc

        ! Register the callback routines.

        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, &
                                        user_init, ESMF_SINGLEPHASE, localrc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, &
                                        user_run, ESMF_SINGLEPHASE, localrc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, &
                                       user_final, ESMF_SINGLEPHASE, localrc)

        print *, "Registered Initialize, Run, and Finalize routines"

        ! return code
        rc = localrc

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
        type(ESMF_Field) :: humidity1
        type(ESMF_VM) :: vm
        type(ESMF_DELayout) :: delayout
        type(ESMF_Grid) :: grid1
        type(ESMF_Array) :: array1
        type(ESMF_ArraySpec) :: arrayspec
        real(ESMF_KIND_R8), dimension(:,:), pointer :: idata
        real(ESMF_KIND_R8) :: min(2), max(2)
        integer :: counts(ESMF_MAXGRIDDIM)
        integer :: npets, pet_id, countsPerDE1(2), countsPerDE2(2)
        type(ESMF_GridHorzStagger) :: horz_stagger
        type(ESMF_Field) :: temp1
        type(ESMF_Bundle) :: bundle1
        type(ESMF_State) :: state1
        integer :: status

        ! this should be overwritten by a more specific error code on error
        status = ESMF_FAILURE

        ! Query component for VM and create a layout with the right breakdown
        call ESMF_GridCompGet(comp, vm=vm, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10
        call ESMF_VMGet(vm, petCount=npets, localPET=pet_id, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10
      !  delayout = ESMF_DELayoutCreate(vm, (/ 4, 1 /), rc=status)
        delayout = ESMF_DELayoutCreate(vm, (/ 2, 2 /), rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10

        print *, pet_id, "User Comp 1 Init starting"

        ! Add a "humidity1" field to the export state.
        countsPerDE1 = (/ 30, 30 /)
        countsPerDE2 = (/ 20, 20 /)

        counts(1) = 60
        counts(2) = 40
        min(1) = 0.0
        max(1) = 60.0
        min(2) = 0.0
        max(2) = 50.0
        horz_stagger = ESMF_GRID_HORZ_STAGGER_A

        grid1 = ESMF_GridCreateHorzXYUni(counts=counts, &
                                minGlobalCoordPerDim=min, &
                                maxGlobalCoordPerDim=max, &
                                horzStagger=horz_stagger, &
                                name="source grid", rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10
        call ESMF_GridDistribute(grid1, delayout=delayout, &
                                 countsPerDEDim1=countsPerDE1, &
                                 countsPerDEDim2=countsPerDE2, &
                                 rc=status)

        if (status .ne. ESMF_SUCCESS) goto 10

        ! Set up a 2D real array
        call ESMF_ArraySpecSet(arrayspec, rank=2, type=ESMF_DATA_REAL, &
                                kind=ESMF_R8)
        if (status .ne. ESMF_SUCCESS) goto 10

        ! Create the field and have it create the array internally
        humidity1 = ESMF_FieldCreate(grid1, arrayspec, &
                                     horzRelloc=ESMF_CELL_CENTER, &
                                     haloWidth=0, name="humidity1", rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10

        ! Get the allocated array back and get an F90 array pointer
        call ESMF_FieldGetArray(humidity1, array1, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10
        call ESMF_ArrayGetData(array1, idata, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10

        ! Set initial data values over whole array to our de id
        idata = real(pet_id)

        call ESMF_StateAddField(exportState, humidity1, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10
     !   call ESMF_StatePrint(exportState, rc=status)

        ! for debugging Reconcile only, add some other objects to the state.
        ! they will not be used in the coupler, but will be grist to debug
        ! the reconcile code.
        state1 = ESMF_StateCreate(rc=status)
        call ESMF_StateAddState(exportState, state1, rc=status)
        temp1 = ESMF_FieldCreateNoData(rc=status)
        bundle1 = ESMF_BundleCreate(rc=status)
        call ESMF_BundleSetAttribute(bundle1, "Region", "Artic", rc=status)
        call ESMF_BundleSetAttribute(bundle1, "Cover", "World", rc=status)
        call ESMF_BundleSetAttribute(bundle1, "Scale Factor", 1.0, rc=status)
        call ESMF_BundleAddField(bundle1, temp1, rc=status)
        call ESMF_StateAddBundle(exportState, bundle1, rc=status)
        call ESMF_StateAddArray(exportState, array1, rc=status)

  
        call ESMF_StatePrint(exportState, rc=status)
     
        print *, pet_id, "User Comp 1 Init returning"
   
        rc = ESMF_SUCCESS
        return

        ! get here only on error exit
10  continue
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
        type(ESMF_Field) :: humidity1
        type(ESMF_RelLoc) :: relloc
        type(ESMF_Array) :: array1
        type(ESMF_Array), dimension(:), pointer :: coordArray
        type(ESMF_Grid) :: grid
        real(ESMF_KIND_R8) :: pi
        real(ESMF_KIND_R8), dimension(:,:), pointer :: idata, coordX, coordY
        integer :: status, i, j, counts(2)

        print *, "User Comp Run starting"

        pi = 3.14159

        ! Get the Field and Bundle data from the State
        call ESMF_StateGetField(exportState, "humidity1", humidity1, rc=status)
      
        ! get the grid and coordinates
        allocate(coordArray(2))
        call ESMF_FieldGet(humidity1, grid=grid, horzRelLoc=relloc, rc=status)
        call ESMF_GridGetDELocalInfo(grid, localCellCountPerDim=counts, &
                                     horzRelLoc=relloc, rc=status)
        if (counts(1)*counts(2).ne.0) then
          call ESMF_GridGetCoord(grid, horzRelLoc=relloc, &
                                 centerCoord=coordArray, rc=status)
          call ESMF_ArrayGetData(coordArray(1), coordX, ESMF_DATA_REF, status)
          call ESMF_ArrayGetData(coordArray(2), coordY, ESMF_DATA_REF, status)
        endif

        ! update field values here
        ! call ESMF_StateGetDataPointer(exportState, "humidity1", idata, rc=status)
        call ESMF_FieldGetArray(humidity1, array1, rc=status) 
        ! Get a pointer to the start of the data
        call ESMF_ArrayGetData(array1, idata, ESMF_DATA_REF, rc=status)

        ! increment data values in place
    !    idata = idata + 10.0
        if (counts(1)*counts(2).ne.0) then
          do j   = 1,counts(2)
            do i = 1,counts(1)
              idata(i,j) = 10.0 + 5.0*sin(coordX(i,j)/60.0*pi) &
                                + 2.0*sin(coordY(i,j)/50.0*pi)
            enddo
          enddo
        endif

     !   call ESMF_StatePrint(exportState, rc=status)
     !   call ESMF_FieldPrint(humidity1, rc=status)
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

        print *, "User Comp Final starting"
    
        ! currently nothing to do - replace this if we add code here.
        status = ESMF_SUCCESS

        print *, "User Comp Final returning"
   
        rc = status

    end subroutine user_final


    end module user_model1
    
!\end{verbatim}
    
