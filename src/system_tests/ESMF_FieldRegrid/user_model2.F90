! $Id: user_model2.F90,v 1.55 2009/09/29 16:53:07 feiliu Exp $
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
        integer, intent(out) :: rc

        rc = ESMF_SUCCESS
        print *, "In user register routine"

        ! Register the callback routines.

        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, user_init, rc=rc)
        if(rc/=ESMF_SUCCESS) return
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, user_run, rc=rc)
        if(rc/=ESMF_SUCCESS) return
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, user_final, rc=rc)
        if(rc/=ESMF_SUCCESS) return

        print *, "Registered Initialize, Run, and Finalize routines"

    end subroutine

!--------------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    subroutine user_init(comp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: comp
      type(ESMF_State) :: importState, exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

!   ! Local variables
      type(ESMF_Field) :: humidity
      type(ESMF_VM) :: vm
      type(ESMF_grid) :: grid1
      type(ESMF_ArraySpec) :: arrayspec
      real(ESMF_KIND_R8), dimension(:,:), pointer :: idata
      real(ESMF_KIND_R8), dimension(:,:), pointer :: coordX, coordY
      real(ESMF_KIND_R8) :: min(2), max(2), dx, dy
      integer :: counts(2)
      integer :: npets, de_id, i, j, tlb(2), tub(2)

      rc = ESMF_SUCCESS
      ! Initially import state contains a field with a grid but no data.

      ! Query component for VM and create a layout with the right breakdown
      call ESMF_GridCompGet(comp, vm=vm, rc=rc)
      if(rc/=ESMF_SUCCESS) return
      call ESMF_VMGet(vm, localPet=de_id, petCount=npets, rc=rc)
      if(rc/=ESMF_SUCCESS) return

      print *, de_id, "User Comp 2 Init starting"

      ! Add a "humidity" field to the import state.
      min(1) = 0.0
      min(2) = 0.0
      counts(1) = 200
      counts(2) = 100
      min(1) = 0.0
      max(1) = 60.0
      min(2) = 0.0
      max(2) = 50.0

      dx = (max(1)-min(1))/(counts(1)-1)
      dy = (max(2)-min(2))/(counts(2)-1)

      grid1 = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=counts, &
            gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
            indexflag=ESMF_INDEX_GLOBAL, &
            regDecomp=(/ npets/2, 2/), name="dest grid", rc=rc)
      if(rc/=ESMF_SUCCESS) return
      call ESMF_GridAddCoord(grid1, rc=rc)
      if(rc/=ESMF_SUCCESS) return
      call ESMF_GridGetCoord(grid1, localDE=0, coordDim=1, &
                         fptr=coordX, computationalLBound=tlb, computationalUBound=tub, rc=rc)
      if(rc/=ESMF_SUCCESS) return
      call ESMF_GridGetCoord(grid1, localDE=0, coordDim=2, &
                         fptr=coordY, rc=rc)
      if(rc/=ESMF_SUCCESS) return
      do j   = tlb(2), tub(2)
        do i = tlb(1), tub(1)
          coordX(i,j) = (i-1)*dx
          coordY(i,j) = (j-1)*dy
        enddo
      enddo

      ! Set up a 2D real array
      call ESMF_ArraySpecSet(arrayspec, rank=2, &
                             typekind=ESMF_TYPEKIND_R8, rc=rc)
      if(rc/=ESMF_SUCCESS) return

      ! Create the field and have it create the array internally
      humidity = ESMF_FieldCreate(grid1, arrayspec, &
                                  maxHaloLWidth=(/0,0/), maxHaloUWidth=(/0,0/), &
                                  name="humidity", rc=rc)
      if(rc/=ESMF_SUCCESS) return
  
      ! Get the allocated array back and get an F90 array pointer
      call ESMF_FieldGet(humidity, farrayPtr=idata, rc=rc)
      if(rc/=ESMF_SUCCESS) return
  
      call ESMF_StateAdd(importState, humidity, rc)
      if(rc/=ESMF_SUCCESS) return
      !   call ESMF_StatePrint(importState, rc=rc)
  
      print *, de_id, "User Comp 2 Init returning"
   
    end subroutine user_init


!--------------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
    subroutine user_run(comp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: comp
      type(ESMF_State) :: importState, exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

!   ! Local variables
      type(ESMF_Field) :: humidity

      rc = ESMF_SUCCESS
      print *, "User Comp Run starting"

      ! Get information from the component.
      call ESMF_StateGet(importState, "humidity", humidity, rc=rc)
      if(rc/=ESMF_SUCCESS) return
  !    call ESMF_FieldPrint(humidity, rc=rc)

      print *, "User Comp Run returning"

    end subroutine user_run


!--------------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
    subroutine user_final(comp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: comp
      type(ESMF_State) :: importState, exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      ! Local variables
      type(ESMF_Field) :: field
      type(ESMF_Grid) :: grid
      type(ESMF_VM) :: vm
      integer       :: de_id

      rc = ESMF_SUCCESS
      print *, "User Comp Final starting"  

      call ESMF_GridCompGet(comp, vm=vm, rc=rc)
      if(rc/=ESMF_SUCCESS) return
      call ESMF_VMGet(vm, localPet=de_id, rc=rc)
      if(rc/=ESMF_SUCCESS) return

      ! set this up to run the validate code on all fields
      ! so we can see and compare the output.  but if any of
      ! the verify routines return error, return error at the end.

      ! check validity of results
      ! Get Fields from import state
      call ESMF_StateGet(importState, "humidity", field, rc=rc)
      if(rc/=ESMF_SUCCESS) return
      call verifyResults(field, de_id, rc)
      if(rc/=ESMF_SUCCESS) return

      ! come straight here if you cannot get the data from the state.
      ! otherwise error codes are accumulated but ignored until the
      ! end so we can see the output from all the cases to help track
      ! down errors.

      ! garbage collection
      call ESMF_FieldGet(field, grid=grid, rc=rc)
      if (rc .ne. ESMF_SUCCESS) return
      call ESMF_FieldDestroy(field, rc=rc)
      if (rc .ne. ESMF_SUCCESS) return
      call ESMF_GridDestroy(grid, rc=rc)
      if (rc .ne. ESMF_SUCCESS) return

      print *, "User Comp Final returning"
   
    end subroutine user_final

!--------------------------------------------------------------------------------
!   !  The routine where results are validated.
!   !
 
    subroutine verifyResults(humidity, myDE, rc)
      type(ESMF_Field) :: humidity
      integer  :: myDE
      integer, intent(out) :: rc

      ! Local variables
      integer :: i, j, i1, j1, haloWidth, counts(2), haloUWidth(2), tlb(2), tub(2)
      type(ESMF_Grid) :: grid
      real(ESMF_KIND_R8) :: pi, error, maxError, maxPerError
      real(ESMF_KIND_R8) :: minCValue, maxCValue, minDValue, maxDValue
      real(ESMF_KIND_R8), dimension(:,:), pointer :: calc, data, coordX, coordY

      rc = ESMF_SUCCESS
      print *, "User verifyResults starting"  

      pi = 3.14159

      ! get the grid and coordinates
      call ESMF_FieldGet(humidity, grid=grid, &
                         maxHaloUWidth=haloUWidth, rc=rc)
      if(rc/=ESMF_SUCCESS) return
      haloWidth=haloUWidth(1)
      call ESMF_GridGetCoord(grid, localDE=0, coordDim=1, &
                            computationalLBound=tlb, computationalUBound=tub, &
                           fptr=coordX, rc=rc)
      if(rc/=ESMF_SUCCESS) return
      call ESMF_GridGetCoord(grid, localDE=0, coordDim=2, &
                           fptr=coordY, rc=rc)
      if(rc/=ESMF_SUCCESS) return

      ! update field values here
      ! Get a pointer to the start of the data
      call ESMF_FieldGet(humidity, farrayPtr=data, rc=rc)
      if(rc/=ESMF_SUCCESS) return
      print *, "rc from array get data = ", rc
      !if (associated(data)) print *, "pointer is associated"
      !if (.not.associated(data)) print *, "pointer is *NOT* associated"
      ! call ESMF_ArrayPrint(array)
      !print *, "data in validate: ", data(1,1), data(1, 2), data(2, 1)

      ! allocate array for computed results and fill it
      allocate(calc(tlb(1):tub(1), tlb(2):tub(2)))
      do j   = tlb(2), tub(2)
        do i = tlb(1), tub(1)
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
      do j   = tlb(2), tub(2)
        j1   = j + haloWidth
        do i = tlb(1), tub(1)
          i1 = i + haloWidth
          error       = abs(data(i1,j1)) - abs(calc(i,j))
          minCValue   = min(minCValue, abs(calc(i,j)))
          maxCValue   = max(maxCValue, abs(calc(i,j)))
          minDValue   = min(minDValue, abs(data(i1,j1)))
          maxDValue   = max(maxDValue, abs(data(i1,j1)))
          maxError    = max(maxError, abs(error))
          maxPerError = max(maxPerError, 100.*abs(error)/abs(calc(i,j)))
        enddo
      enddo
      deallocate(calc)

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
      endif

    end subroutine verifyResults


    end module user_model2
    
!\end{verbatim}
    
