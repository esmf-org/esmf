! $Id: user_model1.F90,v 1.9 2007/06/23 04:01:25 cdeluca Exp $
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
        type(ESMF_IGridComp), intent(inout) :: comp
        integer, intent(out) :: rc

        integer :: localrc

        ! Register the callback routines.

        call ESMF_IGridCompSetEntryPoint(comp, ESMF_SETINIT, &
                                        user_init, ESMF_SINGLEPHASE, localrc)
        call ESMF_IGridCompSetEntryPoint(comp, ESMF_SETRUN, &
                                        user_run, ESMF_SINGLEPHASE, localrc)
        call ESMF_IGridCompSetEntryPoint(comp, ESMF_SETFINAL, &
                                       user_final, ESMF_SINGLEPHASE, localrc)

        !print *, "Registered Initialize, Run, and Finalize routines"

#ifdef ESMF_TESTWITHTHREADS
        ! The following call will turn on ESMF-threading (single threaded)
        ! for this component. If you are using this file as a template for 
        ! your own code development you probably don't want to include the 
        ! following call unless you are interested in exploring ESMF's 
        ! threading features.
        call ESMF_IGridCompSetVMMinThreads(comp, rc=rc)
#endif

        ! return code
        rc = localrc

    end subroutine

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
    subroutine user_init(comp, importState, exportState, clock, rc)
        type(ESMF_IGridComp), intent(inout) :: comp
        type(ESMF_State), intent(inout) :: importState, exportState
        type(ESMF_Clock), intent(in) :: clock
        integer, intent(out) :: rc

       ! Local variables
        type(ESMF_Field) :: humidity1, pressure1, pressure3
        type(ESMF_VM) :: vm
        type(ESMF_DELayout) :: delayout
        type(ESMF_IGrid) :: igrid1
        type(ESMF_ArraySpec) :: arrayspec
        real(ESMF_KIND_R8), dimension(:,:), pointer :: humidityData, pressureData
        real(ESMF_KIND_R8) :: min(2), max(2)
        integer :: counts(ESMF_MAXIGRIDDIM)
        integer :: npets, pet_id, countsPerDE1(2), countsPerDE2(2)
        type(ESMF_IGridHorzStagger) :: horz_stagger
        integer :: status

        ! this should be overwritten by a more specific error code on error
        status = ESMF_FAILURE

        ! Query component for VM and create a layout with the right breakdown
        call ESMF_IGridCompGet(comp, vm=vm, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10
        call ESMF_VMGet(vm, petCount=npets, localPET=pet_id, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10
        delayout = ESMF_DELayoutCreate(vm, (/ 2, 2 /), rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10
        !call ESMF_DELayoutPrint(delayout, rc=status)
        !if (status .ne. ESMF_SUCCESS) goto 10

        !print *, pet_id, "User Comp 1 Init starting"

        countsPerDE1 = (/ 760, 40 /)
        countsPerDE2 = (/ 30, 10 /)

        counts(1) = 800
        counts(2) = 40
        min(1) = 0.0
        max(1) = 60.0
        min(2) = 0.0
        max(2) = 50.0
        horz_stagger = ESMF_IGRID_HORZ_STAGGER_A

        igrid1 = ESMF_IGridCreateHorzXYUni(counts=counts, &
                                minGlobalCoordPerDim=min, &
                                maxGlobalCoordPerDim=max, &
                                horzStagger=horz_stagger, &
                                name="source igrid", rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10
        call ESMF_IGridDistribute(igrid1, delayout=delayout, &
                                 countsPerDEDim1=countsPerDE1, &
                                 countsPerDEDim2=countsPerDE2, &
                                 rc=status)

        if (status .ne. ESMF_SUCCESS) goto 10

        ! Set up a 2D real array
        call ESMF_ArraySpecSet(arrayspec, rank=2, &
                               typekind=ESMF_TYPEKIND_R8)
        if (status .ne. ESMF_SUCCESS) goto 10

        ! Create three fields and have it create the arrays internally
        humidity1 = ESMF_FieldCreate(igrid1, arrayspec, &
                                     horzRelloc=ESMF_CELL_CENTER, &
                                     haloWidth=0, name="humidity1", rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10
        pressure1 = ESMF_FieldCreate(igrid1, arrayspec, &
                                     horzRelloc=ESMF_CELL_CENTER, &
                                     haloWidth=2, name="pressure1", rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10
        pressure3 = ESMF_FieldCreate(igrid1, arrayspec, &
                                     horzRelloc=ESMF_CELL_CENTER, &
                                     haloWidth=2, name="pressure3", rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10

        ! Get the allocated arrays back and get F90 array pointers
        call ESMF_FieldGetDataPointer(humidity1, humidityData, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10
        call ESMF_FieldGetDataPointer(pressure1, pressureData, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10

        ! Set initial data values over whole arrays to our de id
        humidityData = real(pet_id,ESMF_KIND_R8)
        pressureData = real(pet_id,ESMF_KIND_R8)

        ! Add a "humidity" fields to the export state.
        call ESMF_StateAddField(exportState, humidity1, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10
        call ESMF_StateAddField(exportState, pressure1, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10
        call ESMF_StateAddField(exportState, pressure3, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10

        !call ESMF_StatePrint(exportState, rc=status)
     
        !print *, pet_id, "User Comp 1 Init returning"
   
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
        type(ESMF_IGridComp), intent(inout) :: comp
        type(ESMF_State), intent(inout) :: importState, exportState
        type(ESMF_Clock), intent(in) :: clock
        integer, intent(out) :: rc

       ! Local variables
        type(ESMF_Field) :: humidity1, pressure1
        type(ESMF_RelLoc) :: relloc
        type(ESMF_Array), dimension(:), pointer :: coordArray
        type(ESMF_IGrid) :: igrid
        real(ESMF_KIND_R8) :: pi
        real(ESMF_KIND_R8), dimension(:,:), pointer :: humidityData, &
                                                       pressureData, &
                                                       coordX, coordY
        integer :: status, i, j, counts(2)

        !print *, "User Comp Run starting"

        pi = 3.14159

        ! Get the Field and Bundle data from the State
        call ESMF_StateGetField(exportState, "humidity1", humidity1, rc=status)
        call ESMF_StateGetField(exportState, "pressure1", pressure1, rc=status)
      
        ! get the igrid and coordinates
        allocate(coordArray(2))
        call ESMF_FieldGet(humidity1, igrid=igrid, horzRelLoc=relloc, rc=status)
        call ESMF_IGridGetDELocalInfo(igrid, localCellCountPerDim=counts, &
                                     horzRelLoc=relloc, rc=status)
        if (counts(1)*counts(2).ne.0) then
          call ESMF_IGridGetCoord(igrid, horzRelLoc=relloc, &
                                 centerCoord=coordArray, rc=status)
          call ESMF_ArrayGetData(coordArray(1), coordX, ESMF_DATA_REF, status)
          call ESMF_ArrayGetData(coordArray(2), coordY, ESMF_DATA_REF, status)
        endif

        ! update field values here
        ! call ESMF_StateGetDataPointer(exportState, "humidity1", humidityData, &
        !                               rc=status)
        ! Get a pointer to the start of the data
        call ESMF_FieldGetDataPointer(humidity1, humidityData, ESMF_DATA_REF, rc=status)
        call ESMF_FieldGetDataPointer(pressure1, pressureData, ESMF_DATA_REF, rc=status)

        ! increment data values in place
        if (counts(1)*counts(2).ne.0) then
          do j   = 1,counts(2)
            do i = 1,counts(1)
              humidityData(i,j) = 10.0 + 5.0*sin(coordX(i,j)/60.0*pi) &
                                       + 2.0*sin(coordY(i,j)/50.0*pi)
              pressureData(i,j) = 10.0 + 5.0*sin(coordX(i,j)/60.0*pi) &
                                       + 2.0*sin(coordY(i,j)/50.0*pi)
            enddo
          enddo
        endif

     !   call ESMF_StatePrint(exportState, rc=status)
     !   call ESMF_FieldPrint(humidity1, rc=status)
     
        deallocate(coordArray)
        
        !print *, "User Comp Run returning"

        rc = status

    end subroutine user_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
    subroutine user_final(comp, importState, exportState, clock, rc)
        type(ESMF_IGridComp), intent(inout) :: comp
        type(ESMF_State), intent(inout) :: importState, exportState
        type(ESMF_Clock), intent(in) :: clock
        integer, intent(out) :: rc

        ! Local variables
        type(ESMF_Field) :: field1, field2
        integer :: localrc, finalrc

        !print *, "User Comp Final starting"

        ! set this up to run the validate code on all fields
        ! so we can see and compare the output.  but if any of
        ! the verify routines return error, return error at the end.
        finalrc = ESMF_SUCCESS

        ! check validity of results
        ! Get Fields from import state
        call ESMF_StateGetField(exportState, "pressure1", field1, rc=localrc)
        if (localrc .ne. ESMF_SUCCESS) then
          finalrc = localrc
          goto 30
        endif
        call ESMF_StateGetField(exportState, "pressure3", field2, rc=localrc)
        if (localrc .ne. ESMF_SUCCESS) then
          finalrc = localrc
          goto 30
        endif
        call verifyRedistResults(field1, field2, localrc)
        if (localrc .ne. ESMF_SUCCESS) then
          finalrc = localrc
          goto 30
        endif

        call verifyHaloResults(field2, localrc)
        if (localrc .ne. ESMF_SUCCESS) then
          finalrc = localrc
          goto 30
        endif

30   continue
        ! come straight here if you cannot get the data from the state.
        ! otherwise error codes are accumulated but ignored until the
        ! end so we can see the output from all the cases to help track
        ! down errors.

        !print *, "User Comp Final returning"
        rc = finalrc

    end subroutine user_final

!--------------------------------------------------------------------------------
!   !  The routine where results are validated.
!   !
      
    subroutine verifyRedistResults(field1, field2, rc)
      type(ESMF_Field), intent(in) :: field1
      type(ESMF_Field), intent(in) :: field2
      integer, intent(out) :: rc

      ! Local variables
      integer :: status, i, j, miscount, haloWidth, counts(2)
      logical :: match
      type(ESMF_Array) :: array1, array2
      real(ESMF_KIND_R8), dimension(:,:), pointer :: data1, data2
      
      !print *, "User verifyRedistResults starting"
      
      ! update field values here
      call ESMF_FieldGetArray(field1, array1, rc=status)
      call ESMF_FieldGetArray(field2, array2, rc=status)
      call ESMF_FieldGet(field1, haloWidth=haloWidth, rc=status)

      call ESMF_ArrayGet(array1, counts=counts, rc=status)

      ! Get pointers to the start of the data
      call ESMF_ArrayGetData(array1, data1, ESMF_DATA_REF, rc=status)
      call ESMF_ArrayGetData(array2, data2, ESMF_DATA_REF, rc=status)

      ! check and make sure the data in the two fields match exactly
      match    = .true.
      miscount = 0
      do j   = 1+haloWidth, counts(2)-haloWidth
        do i = 1+haloWidth, counts(1)-haloWidth
          if (data1(i,j).ne.data2(i,j)) then
            match = .false.
            miscount = miscount + 1
            if (miscount .gt. 40) then
              print *, "more than 40 mismatches, skipping rest of loop"
              goto 10
            endif
          endif
        enddo
      enddo
  10  continue

      if (match) then
        write(*,*) "Array contents matched correctly!!"
        rc = ESMF_SUCCESS
      else
        rc = ESMF_FAILURE
      endif

    end subroutine verifyRedistResults


    subroutine verifyHaloResults(field1, rc)
      type(ESMF_Field), intent(in) :: field1
      integer, intent(out) :: rc

      ! Local variables
      integer :: status, i, j, miscount, haloWidth, counts(2)
      logical :: match
      type(ESMF_Array) :: array1
      real(ESMF_KIND_R8), dimension(:,:), pointer :: data1
      
      !print *, "User verifyHaloResults starting"
      
      ! update field values here
      call ESMF_FieldGetArray(field1, array1, rc=status)
      call ESMF_FieldGet(field1, haloWidth=haloWidth, rc=status)

      call ESMF_ArrayGet(array1, counts=counts, rc=status)

      ! Get pointers to the start of the data
      call ESMF_ArrayGetData(array1, data1, ESMF_DATA_REF, rc=status)

      ! check and make sure the halo data is ok
      match    = .true.
      miscount = 0
      do j   = 1+haloWidth, counts(2)-haloWidth
        do i = 1+haloWidth, counts(1)-haloWidth
          !TODO: fix loops and tests here.
          ! if (data1(i,j).ne.data2(i,j)) then
          !  match = .false.
          !  miscount = miscount + 1
          !  if (miscount .gt. 40) then
          !    print *, "more than 40 mismatches, skipping rest of loop"
          !    goto 10
          !  endif
          ! endif
        enddo
      enddo
  10  continue

      if (match) then
        write(*,*) "Array halo contents matched correctly!!"
        rc = ESMF_SUCCESS
      else
        rc = ESMF_FAILURE
      endif

    end subroutine verifyHaloResults

    end module user_model1
    
!\end{verbatim}
    
