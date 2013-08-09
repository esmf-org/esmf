! $Id$
!
! Example/test code which shows User Component calls.

!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  User-supplied Component
!
!
!\begin{verbatim}

    module user_model2

    ! ESMF Framework module
    use ESMF

    implicit none
    
    public userm2_register
        
    type mylocaldata
      integer :: dataoffset
    end type

    type wrapper
      type(mylocaldata), pointer :: ptr
    end type

    contains

!--------------------------------------------------------------------------------
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

        call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, user_init, rc=rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_RUN, user_run, rc=rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, user_final, rc=rc)

        print *, "Registered Initialize, Run, and Finalize routines"

        allocate(mydatablock)

        mydatablock%dataoffset = 52

        wrap%ptr => mydatablock
        call ESMF_GridCompSetInternalState(comp, wrap, rc)

        print *, "Registered Private Data block for Internal State"

        rc = ESMF_SUCCESS

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
      type(ESMF_Field) :: humidity
      type(ESMF_VM) :: vm
      type(ESMF_DELayout) :: delayout
      type(ESMF_FieldDataMap) :: datamap
      type(ESMF_IGrid) :: igrid1
      type(ESMF_ArraySpec) :: arrayspec
      real(ESMF_KIND_R8), dimension(:,:), pointer :: idata
      real(ESMF_KIND_R8) :: min(2)
      real(ESMF_KIND_R8) :: delta1(40), delta2(50)
      integer :: countsPerDE1(3), countsPerDE2(2)
      integer :: npets, de_id
      type(ESMF_IGridHorzStagger) :: horzStagger
      type(ESMF_CoordOrder) :: coordOrder
      integer :: status


      ! Query component for VM and create a layout with the right breakdown
      call ESMF_GridCompGet(comp, vm=vm, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10
      call ESMF_VMGet(vm, petCount=npets, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10 
      delayout = ESMF_DELayoutCreate(vm, (/ npets/2, 2 /), rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10
      
      ! and get our local de number
      call ESMF_DELayoutGetDeprecated(delayout, localDE=de_id, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10
      
      print *, de_id, "User Comp 2 Init starting"

      ! Add a "humidity" field to the import state.
      countsPerDE1 = (/ 10, 18, 12 /)
      countsPerDE2 = (/ 22, 28 /)
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

      horzStagger     = ESMF_IGRID_HORZ_STAGGER_D_NE
      coordOrder      = ESMF_COORD_ORDER_YXZ

      call ESMF_FieldDataMapSetDefault(datamap, ESMF_INDEX_IJ, rc=rc)
      if (status .ne. ESMF_SUCCESS) goto 10

      igrid1 = ESMF_IGridCreateHorzXY( minGlobalCoordPerDim=min, &
                                    delta1=delta1, delta2=delta2, &
                                    horzStagger=horzStagger, &
                                    coordOrder=coordOrder, &
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

      ! Create the field and have it create the array internally
      humidity = ESMF_FieldCreate(igrid1, arrayspec, &
                                  horzRelloc=ESMF_CELL_NFACE, datamap=datamap, &
                                  haloWidth=0, name="humidity", rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10

      ! Get the allocated array back and get an F90 array pointer
      call ESMF_FieldGetDataPointer(humidity, idata, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10

      call ESMF_StateAddField(importState, (/humidity/), rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10

      print *, de_id, "User Comp 2 Init returning"
   
10 continue
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
      type(ESMF_Field) :: humidity
      integer :: status

      print *, "User Comp Run starting"

      ! Get information from the component.
      call ESMF_StateGetField(importState, "humidity", humidity, rc=status)
    
      ! This is where the model specific computation goes.
      print *, "Imported Array in user model 2:"

      print *, "User Comp Run returning"

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
      integer :: status
      type(ESMF_Field) :: field
      integer :: localrc, finalrc
      type(mylocaldata), pointer :: mydatablock
      type(wrapper) :: wrap

      print *, "User Comp Final starting"  

      ! set this up to run the validate code on all fields
      ! so we can see and compare the output.  but if any of
      ! the verify routines return error, return error at the end.
      finalrc = ESMF_SUCCESS

      ! Get our local info
      nullify(wrap%ptr)
      mydatablock => wrap%ptr
        
      call ESMF_GridCompGetInternalState(comp, wrap, status)

      mydatablock => wrap%ptr
      print *, "before deallocate, dataoffset = ", mydatablock%dataoffset

      ! check validity of results
      ! Get Fields from import state
      call ESMF_StateGetField(importState, "humidity", field, rc=rc)
      if (rc .ne. ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
        goto 30
      endif
      call verifyResults(field, localrc)
      if (localrc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

      deallocate(mydatablock, stat=status)
      print *, "deallocate returned ", status
      nullify(wrap%ptr)

30 continue
      ! come straight here if you cannot get the data from the state.
      ! otherwise error codes are accumulated but ignored until the
      ! end so we can see the output from all the cases to help track
      ! down errors.

      print *, "User Comp Final returning"
      rc = finalrc
   
    end subroutine user_final

!--------------------------------------------------------------------------------
!   !  The routine where results are validated.
!   !
 
    subroutine verifyResults(humidity, rc)
      type(ESMF_Field), intent(inout) :: humidity
      integer, intent(out) :: rc

      ! Local variables
      integer :: status, i, j, myDE, counts(2)
      type(ESMF_RelLoc) :: relloc
      type(ESMF_IGrid) :: igrid
      real(ESMF_KIND_R8) :: pi, error, maxError, maxPerError
      real(ESMF_KIND_R8) :: minCValue, maxCValue, minDValue, maxDValue
      real(ESMF_KIND_R8), dimension(:,:), pointer :: calc, data, coordX, coordY

      print *, "User verifyResults starting"  

      pi = 3.14159

      ! get the igrid and coordinates
      call ESMF_FieldGet(humidity, igrid=igrid, horzRelloc=relloc, rc=status)
      call ESMF_IGridGetDELocalInfo(igrid, myDE=myDE, &
                                   localCellCountPerDim=counts, &
                                   horzRelloc=ESMF_CELL_CENTER, rc=status)
      ! note: the IGrid is JI so the coord arrays have to be switched below
      call ESMF_IGridGetCoord(igrid, dim=1, horzRelloc=relloc, &
                             centerCoord=coordY, rc=status)
      call ESMF_IGridGetCoord(igrid, dim=2, horzRelloc=relloc, &
                             centerCoord=coordX, rc=status)

      ! update field values here
      ! Get a pointer to the start of the data
      call ESMF_FieldGetDataPointer(humidity, data, ESMF_DATACOPY_REFERENCE, rc=status)
      print *, "rc from array get data = ", status
      !if (associated(data)) print *, "pointer is associated"
      !if (.not.associated(data)) print *, "pointer is *NOT* associated"
      print *, "data in validate: ", data(1,1), data(1, 2), data(2, 1)

      ! allocate array for computed results and fill it
      allocate(calc(counts(1), counts(2)))
      do j   = 1,counts(2)
        do i = 1,counts(1)
          calc(i,j) = 10.0 + 5.0*sin(coordX(i,j)/60.0*pi) &
                           + 2.0*sin(coordY(i,j)/50.0*pi)
        enddo
      enddo

     ! calculate data error from computed results
      maxError    = 0.0
      maxPerError = 0.0
      maxCValue   = 0.0
      minCValue   = 1000.0
      maxDValue   = 0.0
      minDValue   = 1000.0
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

      write(*,*) "Results for DE #", myDE, ":"
      write(*,*) "   minimum regridded value = ", minDValue
      write(*,*) "   maximum regridded value = ", maxDValue
      write(*,*) "   minimum computed value  = ", minCValue
      write(*,*) "   maximum computed value  = ", maxCValue
      write(*,*) "   maximum error           = ", maxError
      write(*,*) "   maximum percent error   = ", maxPerError
      print *, "User verifyResults returning"
   
      ! only accept this test as successful if the max percent
      ! error is below 2%
      if (maxPerError .gt. 2.0) then
          write(*,*) "Test Failing because percentage error too large"
          rc = ESMF_FAILURE 
      else
          rc = ESMF_SUCCESS
      endif

    end subroutine verifyResults


    end module user_model2
    
!\end{verbatim}
    
