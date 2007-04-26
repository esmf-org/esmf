! $Id: user_model2.F90,v 1.23 2007/04/26 06:19:44 cdeluca Exp $
!
! System test for Exclusive Components, user-written component 2.

!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  User-supplied Component 2.
!
!
!\begin{verbatim}

    module user_model2

    ! ESMF Framework module
    use ESMF_Mod

    implicit none
    
    public userm2_register
        
    contains

!--------------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
    subroutine userm2_register(comp, rc)
        type(ESMF_GridComp) :: comp
        integer :: rc

        integer :: localrc

        !print *, "In user register routine"

        ! Register the callback routines.

        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, &
                                        user_init, ESMF_SINGLEPHASE, localrc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, &
                                        user_run, ESMF_SINGLEPHASE, localrc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, &
                                       user_final, ESMF_SINGLEPHASE, localrc)

        !print *, "Registered Initialize, Run, and Finalize routines"

#ifdef ESMF_TESTWITHTHREADS
        ! The following call will turn on ESMF-threading (single threaded)
        ! for this component. If you are using this file as a template for 
        ! your own code development you probably don't want to include the 
        ! following call unless you are interested in exploring ESMF's 
        ! threading features.
        call ESMF_GridCompSetVMMinThreads(comp, rc=rc)
#endif

        rc = localrc

    end subroutine

!--------------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    subroutine user_init(comp, importState, exportState, clock, rc)
      type(ESMF_GridComp), intent(inout) :: comp
      type(ESMF_State), intent(inout) :: importState, exportState
      type(ESMF_Clock), intent(in) :: clock
      integer, intent(out) :: rc

!   ! Local variables
      type(ESMF_Field) :: humidity2, pressure2
      type(ESMF_VM) :: vm
      type(ESMF_DELayout) :: delayout
      type(ESMF_Grid) :: grid1, grid2
      type(ESMF_ArraySpec) :: arrayspec
      real(ESMF_KIND_R8), dimension(:,:), pointer :: humidityData, &
                                                     pressureData
      real(ESMF_KIND_R8) :: min(2), max(2)
      real(ESMF_KIND_R8) :: delta1(40), delta2(50)
      integer :: countsPerDE1(3), countsPerDE2(1)
      integer :: npets, pet_id, counts(2)
      type(ESMF_GridHorzStagger) :: horz_stagger
      integer :: status

      ! Initially import state contains a field with a grid but no data.
      status = ESMF_FAILURE

      ! Query component for VM and create a layout with the right breakdown
      call ESMF_GridCompGet(comp, vm=vm, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10
      call ESMF_VMGet(vm, petCount=npets, localPET=pet_id, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10
      delayout = ESMF_DELayoutCreate(vm, (/ 3, 1 /), rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10
      !call ESMF_DELayoutPrint(delayout, rc=status)
      !if (status .ne. ESMF_SUCCESS) goto 10

      !print *, pet_id, "User Comp 2 Init starting"

      ! Add a "humidity2" field to the import state.
      countsPerDE1 = (/ 10, 6, 24 /)
      countsPerDE2 = (/ 50 /)
      min(1) = 0.0
      delta1 = (/ 1.0, 1.0, 1.0, 1.1, 1.1, 1.1, 1.2, 1.2, 1.3, 1.4, &
                  1.4, 1.5, 1.6, 1.6, 1.6, 1.8, 1.8, 1.7, 1.7, 1.6, &
                  1.6, 1.6, 1.8, 1.8, 2.0, 2.0, 2.2, 2.2, 2.2, 2.2, &
                  2.0, 1.7, 1.5, 1.3, 1.2, 1.1, 1.0, 1.0, 1.0, 0.9 /)
      min(2) = 0.0
      delta2 = (/ 0.8, 0.8, 0.8, 0.8, 0.8, 0.7, 0.7, 0.6, 0.7, 0.8, &
                  0.9, 0.9, 0.9, 0.9, 1.0, 1.0, 1.0, 1.0, 0.9, 1.0, &
                  1.0, 1.0, 1.0, 1.1, 1.2, 1.3, 1.3, 1.3, 1.4, 1.4, &
                  1.4, 1.4, 1.4, 1.4, 1.4, 1.3, 1.3, 1.3, 1.2, 1.2, &
                  1.1, 1.0, 1.0, 0.9, 0.8, 0.7, 0.6, 0.6, 0.5, 0.5 /)
      min(1) = 0.0
      min(2) = 0.0
      horz_stagger = ESMF_GRID_HORZ_STAGGER_D_NE

      grid1 = ESMF_GridCreateHorzXY(minGlobalCoordPerDim=min, &
                                    delta1=delta1, delta2=delta2, &
                                    horzStagger=horz_stagger, &
                                    name="source grid", rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10
      call ESMF_GridDistribute(grid1, delayout=delayout, &
                               countsPerDEDim1=countsPerDE1, &
                               countsPerDEDim2=countsPerDE2, &
                               rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10

      ! Set up a 2D real array
      call ESMF_ArraySpecSet(arrayspec, rank=2, &
                             typekind=ESMF_TYPEKIND_R8)
      if (status .ne. ESMF_SUCCESS) goto 10

      ! Create the field and have it create the array internally
      humidity2 = ESMF_FieldCreate(grid1, arrayspec, &
                                   horzRelloc=ESMF_CELL_NFACE, &
                                   haloWidth=0, name="humidity2", rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10
  
      ! Get the F90 array pointer to the data
      call ESMF_FieldGetDataPointer(humidity2, humidityData, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10
  
      call ESMF_StateAddField(importState, humidity2, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10
      !   call ESMF_StatePrint(importState, rc=status)


      ! create the second grid and field for use in the redist part of the test
      counts(1) = 800
      counts(2) = 40
      min(1) = 0.0
      max(1) = 60.0
      min(2) = 0.0
      max(2) = 50.0
      horz_stagger = ESMF_GRID_HORZ_STAGGER_A

      grid2 = ESMF_GridCreateHorzXYUni(counts=counts, &
                                       minGlobalCoordPerDim=min, &
                                       maxGlobalCoordPerDim=max, &
                                       horzStagger=horz_stagger, &
                                       name="redist grid", rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10
      call ESMF_GridDistribute(grid2, delayout=delayout, &
                               rc=status)

      ! Create the field and have it create the array internally
      pressure2 = ESMF_FieldCreate(grid2, arrayspec, &
                                   horzRelloc=ESMF_CELL_CENTER, &
                                   haloWidth=2, name="pressure2", rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10
  
      ! Get the F90 array pointer to the data
      call ESMF_FieldGetDataPointer(pressure2, pressureData, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10
  
      call ESMF_StateAddField(importState, pressure2, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10
      !   call ESMF_StatePrint(importState, rc=status)

  
      !print *, pet_id, "User Comp 2 Init returning"
   
      rc = ESMF_SUCCESS
      return

      ! get here only on error exit
10  continue
      rc = status
  
    end subroutine user_init


!--------------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
    subroutine user_run(comp, importState, exportState, clock, rc)
      type(ESMF_GridComp), intent(inout) :: comp
      type(ESMF_State), intent(inout) :: importState, exportState
      type(ESMF_Clock), intent(in) :: clock
      integer, intent(out) :: rc

!   ! Local variables
      type(ESMF_Field) :: humidity2, pressure2
!      type(ESMF_Array) :: array1, array2
      integer :: status

      status = ESMF_FAILURE
      !print *, "User Comp Run starting"

      ! Get information from the component.
      call ESMF_StateGetField(importState, "humidity2", humidity2, rc=status)
      call ESMF_StateGetField(importState, "pressure2", pressure2, rc=status)
    
      ! This is where the model specific computation goes.
!      call ESMF_FieldGetArray(humidity2, array1, rc=status)
!      call ESMF_FieldGetArray(pressure2, array2, rc=status)

      !print *, "User Comp Run returning"

      rc = status

    end subroutine user_run


!--------------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
    subroutine user_final(comp, importState, exportState, clock, rc)
      type(ESMF_GridComp), intent(inout) :: comp
      type(ESMF_State), intent(inout) :: importState, exportState
      type(ESMF_Clock), intent(in) :: clock
      integer, intent(out) :: rc

      ! Local variables
      type(ESMF_Field) :: field
      integer :: localrc, finalrc

      !print *, "User Comp Final starting"  

      ! set this up to run the validate code on all fields
      ! so we can see and compare the output.  but if any of
      ! the verify routines return error, return error at the end.
      finalrc = ESMF_SUCCESS

      ! check validity of results
      ! Get Fields from import state
      call ESMF_StateGetField(importState, "humidity2", field, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) then
        finalrc = localrc
        goto 30
      endif
      call verifyRegridResults(field, localrc)
      if (localrc .ne. ESMF_SUCCESS) finalrc = localrc

30 continue
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
 
    subroutine verifyRegridResults(humidity, rc)
      type(ESMF_Field), intent(inout) :: humidity
      integer, intent(out) :: rc

      ! Local variables
      integer :: status, i, j, myDE, counts(2)
      type(ESMF_RelLoc) :: relloc
      type(ESMF_Grid) :: grid
      real(ESMF_KIND_R8) :: pi, error, maxError, maxPerError
      real(ESMF_KIND_R8) :: minCValue, maxCValue, minDValue, maxDValue
      real(ESMF_KIND_R8), dimension(:,:), pointer :: calc, data, coordX, coordY

      !print *, "User verifyRegridResults starting"  

      pi = 3.14159

      ! get the grid and coordinates
      call ESMF_FieldGet(humidity, grid=grid, horzRelloc=relloc, rc=status)
      call ESMF_GridGetDELocalInfo(grid, myDE=myDE, &
                                   localCellCountPerDim=counts, &
                                   horzRelLoc=relloc, rc=status)
      if (counts(1)*counts(2).ne.0) then
        call ESMF_GridGetCoord(grid, dim=1, horzRelloc=relloc, &
                               centerCoord=coordX, rc=status)
        call ESMF_GridGetCoord(grid, dim=2, horzRelloc=relloc, &
                               centerCoord=coordY, rc=status)
      endif

      ! update field values here
      ! Get a pointer to the start of the data
      call ESMF_FieldGetDataPointer(humidity, data, ESMF_DATA_REF, rc=status)
      !print *, "rc from array get data = ", status

      ! allocate array for computed results and fill it
      if (counts(1)*counts(2).ne.0) then
        allocate(calc(counts(1), counts(2)))
        do j   = 1,counts(2)
          do i = 1,counts(1)
            calc(i,j) = 10.0 + 5.0*sin(coordX(i,j)/60.0*pi) &
                             + 2.0*sin(coordY(i,j)/50.0*pi)
          enddo
        enddo
      endif

     ! calculate data error from computed results
      maxError    = 0.0
      maxPerError = 0.0
      maxCValue   = 0.0
      minCValue   = 1000.0
      maxDValue   = 0.0
      minDValue   = 1000.0
      if (counts(1)*counts(2).ne.0) then
        do j   = 1,counts(2)
          do i = 1,counts(1)
            error       = abs(data(i,j)) - abs(calc(i,j))
            minCValue   = min(minCValue, abs(calc(i,j)))
            maxCValue   = max(maxCValue, abs(calc(i,j)))
            minDValue   = min(minDValue, abs(data(i,j)))
            maxDValue   = max(maxDValue, abs(data(i,j)))
            maxError    = max(maxError, abs(error))
            maxPerError = max(maxPerError, 100.*abs(error)/abs(calc(i,j)))
          enddo
        enddo
        deallocate(calc)
      endif

      write(*,*) "Results for DE #", myDE, ":"
      write(*,*) "   minimum regridded value = ", minDValue
      write(*,*) "   maximum regridded value = ", maxDValue
      write(*,*) "   minimum computed value  = ", minCValue
      write(*,*) "   maximum computed value  = ", maxCValue
      write(*,*) "   maximum error           = ", maxError
      write(*,*) "   maximum percent error   = ", maxPerError
      !print *, "User verifyRegridResults returning"
   
      ! only accept this test as successful if the max percent
      ! error is below 2%
      if (maxPerError .gt. 2.0) then
          write(*,*) "Test Failing because percentage error too large"
          rc = ESMF_FAILURE
      else
          rc = ESMF_SUCCESS
      endif

    end subroutine verifyRegridResults


    end module user_model2
    
!\end{verbatim}
    
