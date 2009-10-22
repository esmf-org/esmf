! $Id: user_modelC.F90,v 1.1 2009/10/22 03:26:24 svasquez Exp $
!
! Example/test code which shows User Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!
! !DESCRIPTION:
!  User-supplied Component
!
!
!\begin{verbatim}

module user_modelC

  ! ESMF Framework module
  use ESMF_Mod

  implicit none
    
  public usermC_register
  ! global data
  real(ESMF_KIND_R8) :: solution
  integer :: petCount, myPet      
  contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.

  subroutine usermC_register(comp, rc)
    type(ESMF_GridComp) :: comp
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, userRoutine=user_init, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, userRoutine=user_run, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, userRoutine=user_final, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print *, "User CompC Register returning"
    
  end subroutine

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
  subroutine user_init(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_ArraySpec)  :: arrayspec
    type(ESMF_DistGrid)   :: distgrid
    type(ESMF_Array)      :: array
    type(ESMF_VM)         :: vm
    integer               :: xprocs, yprocs
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Determine petCount
    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMGet(vm, petCount=petCount, localPet = myPet, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Create the destination Array and add it to the import State
    call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    xprocs = 2
    yprocs = petCount/2
    distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/100,150/), &
      regDecomp=(/xprocs, yprocs/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
      indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArraySet(array, name="array data", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateAdd(exportState, array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! initial solution -- this is the average of the inital values of the
    ! four source arrays
    solution = 8.0;

    print *, "User CompC Init returning"

  end subroutine user_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
  subroutine user_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_Array)      :: srcArray, dstArray
    real(ESMF_KIND_R8), pointer :: ftrptr1(:,:), ftrptr2(:,:)   ! matching F90 array pointer
    integer               :: i, j, k, itemcount, count, l1, l2
    character(len=ESMF_MAXSTR) :: stateName, stateItemNames(4)
    integer :: ubnd1(2,1), ubnd2(2,1), lbnd1(2,1), lbnd2(2,1)
    
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    ! The import state contains four arrays with the same size and distribution as the 
    ! destination array.  Do an average of the four array and set the export array values
    ! accordingly.  Also check the values of the input arrays for correctness
    ! Get import State information
    call ESMF_StateGet(importState, name=stateName, itemNameList=stateItemNames, itemcount=itemcount, rc=rc)

    ! Get the destination Array from the import State
    call ESMF_StateGet(exportState, "array data", dstArray, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArrayGet(dstArray, farrayPtr=ftrptr1, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! find the computation bounds for the srcArray and dstArray and make sure they match with each other
    call ESMF_ArrayGet(dstArray, computationalUBound=ubnd1, computationalLBound=lbnd1, rc=rc)

    ! Reset the dest array values to 0
    ftrptr1 = 0.0
    
    ! Get the srcArray from the import state item and dstArray from the export state and do a regrid
    do k=1,itemcount
        call ESMF_StateGet(importState, stateItemNames(k), srcArray, rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail out

        call ESMF_ArrayGet(srcArray, farrayPtr=ftrptr2, rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail out

#if 0
	l1=lbound(ftrptr2,1)
   	l2=lbound(ftrptr2,2)
        print *, 'srcArray ', trim(stateItemNames(k)), ftrptr2(l1,l2) 
#endif
        ! find the computation bounds for the srcArray and dstArray and make sure they match with each other
        call ESMF_ArrayGet(srcArray, computationalUBound=ubnd2, computationalLBound=lbnd2, rc=rc)

	! print *, myPet, "The srcArray and dstArray dimension ", ubnd1, ubnd2

        if (ubnd1(1,1) /= ubnd1(1,1) .or. lbnd1(2,1) /= lbnd1(2,1)) then
	  print *, "The srcArray and dstArray dimension in the localDE does not match ", ubnd1, ubnd2
          rc=ESMF_FAILURE
          return
        end if

        ! add the import array value to the export array 
        do j = lbound(ftrptr1, 2), ubound(ftrptr1, 2)
           do i = lbound(ftrptr1, 1), ubound(ftrptr1, 1)
		ftrptr1(i,j) = ftrptr1(i,j)+ftrptr2(i,j)
           enddo
        enddo
     enddo

     ! now average the output array
     do j = lbound(ftrptr1, 2), ubound(ftrptr1, 2)
        do i = lbound(ftrptr1, 1), ubound(ftrptr1, 1)
   	    ftrptr1(i,j) = ftrptr1(i,j)/4
        enddo
     enddo

     ! check the values:  Model A-1 and B-1 (after regrid): 11+time*5, 
     !			 Model A-2 and B-2 (after regrid):  9+time*10
     ! so the average should be 10+7.5*time for each time step
     !   		
     solution = solution+6.0
     count = 0
     do j = lbound(ftrptr1, 2), ubound(ftrptr1, 2)
        do i = lbound(ftrptr1, 1), ubound(ftrptr1, 1)
   	    if (ftrptr1(i,j) /= solution) count = count+1
        enddo
     enddo

     if (count > 0) then
        rc=ESMF_FAILURE
	return
     endif

  print *, myPet, 'total number of wrong answers:', count
    
  end subroutine user_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
  subroutine user_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_DistGrid) :: distgrid
    type(ESMF_Array) :: array
    
    ! Initialize return code
    rc = ESMF_SUCCESS

     call ESMF_StateGet(exportState, "array data", array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArrayGet(array, distgrid=distgrid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArrayDestroy(array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_DistGridDestroy(distgrid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
 
  end subroutine user_final


end module user_modelC
    
!\end{verbatim}
