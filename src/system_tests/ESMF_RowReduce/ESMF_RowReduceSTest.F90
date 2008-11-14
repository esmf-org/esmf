! $Id: ESMF_RowReduceSTest.F90,v 1.38 2008/11/14 04:39:17 theurich Exp $
!
! System test DELayoutRowReduce
!  Description on Sourceforge under System Test #69725

!-------------------------------------------------------------------------
!ESMF_SYSTEM_____TEST__DISABLED        String used by test script to count system tests.
! Until the newArray becomes available this system tests has been disabled
!=========================================================================

!BOP
!
! !DESCRIPTION:
! System test DELayoutRowReduce.
!
!
!\begin{verbatim}

    program DELayoutRowReduce

#include "ESMF_Macros.inc"

    ! ESMF Framework module
    use ESMF_Mod
    use ESMF_TestMod
    
    implicit none
    
    ! Local variables
    integer :: i, j, ij, rc
    integer :: row_to_reduce
    integer :: rowlen
    integer :: result, pet_id, npets, rightvalue 
    integer :: counts(2), localCount(2)
    type(ESMF_IGridHorzStagger) :: horz_stagger
    real(ESMF_KIND_R8) :: min(2), max(2)
    integer(ESMF_KIND_I4), dimension(:), pointer :: ldata, rowdata, oneDdata
    integer(ESMF_KIND_I4), dimension(:,:), pointer :: idata, rdata
    character(len=ESMF_MAXSTR) :: cname, gname, fname
    type(ESMF_DELayout) :: delayout1 
    type(ESMF_IGrid) :: igrid1
    type(ESMF_Array) :: array1, array2
    type(ESMF_Field) :: field1
    type(ESMF_VM) :: vm
        
    ! cumulative result: count failures; no failures equals "all pass"
    integer :: testresult = 0

    ! individual test name
    character(ESMF_MAXSTR) :: testname

    ! individual test failure message and final status msg
    character(ESMF_MAXSTR) :: failMsg, finalMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    print *, "System Test DELayoutRowReduce:"


!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!

    ! Initialize the framework and query for the default VM
    call ESMF_Initialize(vm=vm, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    ! Find out how many PETs we were started with
    call ESMF_VMGet(vm, petCount=npets, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    ! Create a default 1 x N DELayout 
    delayout1 = ESMF_DELayoutCreate(vm, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    cname = "System Test DELayoutRowReduce"

    print *, "Create section finished"
!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!

!   !  The user creates a simple horizontal IGrid internally by passing all
!   !  necessary information through the CreateInternal argument list.

      counts(1) = 41
      counts(2) = 20
      min(1) = 0.0d0
      max(1) = 20.5d0
      min(2) = 0.0d0
      max(2) = 5.0d0
      horz_stagger = ESMF_IGRID_HORZ_STAGGER_A
      gname = "test igrid 1"

      print *, "right before igrid create"
      igrid1 = ESMF_IGridCreateHorzXYUni(counts=counts, &
                              minGlobalCoordPerDim=min, &
                              maxGlobalCoordPerDim=max, &
                              horzStagger=horz_stagger, &
                              name=gname, rc=rc)
      print *, "IGrid Create returned ", rc,  "(0=SUCCESS, -1=FAILURE)"

      call ESMF_IGridDistribute(igrid1, delayout=delayout1, rc=rc)
      print *, "IGrid Distribute returned ", rc,  "(0=SUCCESS, -1=FAILURE)"


    ! figure out our local processor id
    call ESMF_VMGet(vm, localPet=pet_id, rc=rc)

    ! Allocate and set initial data values.  These are different on each DE.
    call ESMF_IGridGetDELocalInfo(igrid1, localCellCountPerDim=localCount, &
                                 horzRelloc=ESMF_CELL_CENTER, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    print *, "allocating", localCount(1)*localCount(2), " cells on PET", pet_id
    allocate(idata(localCount(1), localCount(2)))
    allocate(ldata(localCount(1) * localCount(2)))
    allocate(oneDdata(localCount(1) * localCount(2)))

    ! set original values to 0; local to global call below will overwrite
    ! this with global index numbers
    idata = 0
    
    ! Generate global cell numbers.  First set to local number and
    ! then translate to global index.
    do j=1,localcount(2)
      do i=1,localcount(1)
        ij = ((j-1) * localcount(1)) + i
        ldata(ij) = ij
      enddo
    enddo
  
    print *, "size local1D", size(ldata)
    print *, "size global1D", size(idata)
    ! TODO: there should be a 2D version of this call.
    call ESMF_IGridDELocalToGlobalIndex(igrid1, local1D=ldata, global1D=oneDdata, &
                                       horzRelloc=ESMF_CELL_CENTER, rc=rc) 
    if (rc .ne. ESMF_SUCCESS) goto 10

    do j=1,localcount(2)
      do i=1,localcount(1)
        ij = ((j-1) * localcount(1)) + i
        idata(i,j) = oneDdata(ij)
      enddo
    enddo
  
    print *, "after local to global"
    print *, "ldata was = ", ldata
    print *, "idata now = ", idata
    ! Delete local cell number array, not needed anymore.
    deallocate(ldata, stat=rc)
    deallocate(oneDdata, stat=rc)

    !  Create Array based on an existing, allocated F90 pointer.
    !  Data is type Integer, 1D.
    array1 = ESMF_ArrayCreate(idata, ESMF_DATA_REF, rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    ! We did the allocate outside the framework, so at the end after
    ! the Array is destroyed we will need an explicit deallocate().


    ! Create a Field using the IGrid and Arrays created above
    fname = "relative humidity"
    field1 = ESMF_FieldCreate(igrid1, array1, horzRelloc=ESMF_CELL_CENTER, &
                                    name=fname, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    call ESMF_FieldPrint(field1, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    print *, "Field Create returned"


    print *, "Init section finished"

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!


!   Call Reduction operator here
    row_to_reduce = 5

    ! Get a pointer to the data Array in the Field
    call ESMF_FieldGetArray(field1, array2, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    call ESMF_ArrayValidate(array2, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    call ESMF_ArrayPrint(array2, "foo", rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    ! Get a pointer to the start of the result data
    call ESMF_ArrayGetData(array2, rdata, ESMF_DATA_REF, rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    ! Get the mapping between local and global indices for this DE
    !   and count of row size
    call ESMF_IGridGetDELocalInfo(igrid1, localCellCountPerDim=localCount, &
                                 horzRelloc=ESMF_CELL_CENTER, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
 
    ! Create a new Fortran array for just the part of this row on this DE
    rowlen = localCount(1)
    
    ! make space for row
    allocate(rowdata(rowlen))

    ! and copy over the data row
    rowdata = rdata(:,row_to_reduce)
    print *, "rowlen = ", rowlen
    print *, "row data = ", rowdata

    ! Call the Reduce code
    call ESMF_DELayoutAllFullReduce(delayout1, rowdata, result, rowlen, &
                                    ESMF_SUM, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    print *, "Row Reduction operation called"

    ! Clean up local array
    deallocate(rowdata)


    print *, "Run section finished"
!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   Print result

    print *, "-----------------------------------------------------------------"
    print *, "-----------------------------------------------------------------"
    print *, "Row Reduction operation returned ", result, " on PET ", pet_id
    print *, "-----------------------------------------------------------------"
    print *, "-----------------------------------------------------------------"

    print *, "Finalize section finished"
!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   Destroy section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   Clean up

    call ESMF_FieldDestroy(field1, rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    call ESMF_IGridDestroy(igrid1, rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    call ESMF_ArrayDestroy(array1, rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    call ESMF_DELayoutDestroy(delayout1, rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    print *, "All Destroy routines done"

    deallocate(idata, stat=rc)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
10     print *, "System Test DELayoutRowReduce complete."


    ! Only print on DE 0 for success, or any DE with an error
    if ((pet_id .eq. 0) .or. (rc .ne. ESMF_SUCCESS)) then
      write(failMsg, *)  "Row Reduction value incorrect"
      write(testname, *) "System Test DELayoutRowReduce: Row Reduction"

      rightvalue = 0
      if (npets .eq. 1) rightvalue = 7585
      if (npets .eq. 2) rightvalue = 12205
      
      call ESMF_Test((result .eq. rightvalue) .and. (rc.eq.ESMF_SUCCESS), &
                        testname, failMsg, testresult, ESMF_SRCLINE)

      ! Separate message to console for quick confirmation of success/failure
      if ((result .eq. rightvalue) .and. (rc .eq. ESMF_SUCCESS)) then
        write(finalMsg, *) "SUCCESS: Row reduction value (", rightvalue, &
                           ") is correct."
      else
        write(finalMsg, *) "System Test did not succeed. ", &
                               "Row reduction result", result, "not equal ", &
                               rightvalue, " or error code set ", rc
      endif
      write(0, *) ""
      write(0, *) trim(testname)
      write(0, *) trim(finalMsg)
      write(0, *) ""

    endif
    
    call ESMF_Finalize(rc=rc)

    end program DELayoutRowReduce
    
!\end{verbatim}
    
