! $Id: ESMF_FieldRedistBlk2BlkSTest.F90,v 1.3 2007/02/13 20:22:50 theurich Exp $
!
! System test FieldRedistBlk2Blk
!  Description on Sourceforge under System Test #XXXXX

!-------------------------------------------------------------------------
!SYSTEM_TEST        String used by test script to count system tests.
!=========================================================================

!BOP
!
! !DESCRIPTION:
! System test FieldRedistBlk2Blk.
!
! This system test checks the functionality of the grid distribution
! routines by redistributing data from one Field distributed in the normal
! block structure to another Field that has been distributed regularly
! and then back again.  The original data should exactly match the final
! data, which serves as the test for SUCCESS.  This program creates two
! identical Grids with different distributions, one with the normal block
! structure and the other with a different block distribution.  The first
! Grid has two Fields created from it, the first as the source for the test
! and the second for the final results.  The second Grid has a single Field
! that serves as an intermediate result between the two redistributions.
!
!\begin{verbatim}

     program Blk2BlkFldRedist

#include <ESMF_Macros.inc>

     ! ESMF Framework module
     use ESMF_Mod
     use ESMF_TestMod
    
     implicit none

     ! Local variables
     integer :: status
     integer :: i, j
     integer :: counts(2), localCounts(2), miscount
     integer :: npets, myDE, myPet
     logical :: match
     real(ESMF_KIND_R8) :: min(2), max(2), compval
     real(ESMF_KIND_R8) :: pi = 3.1416d0
     real(ESMF_KIND_R8), dimension(:,:), pointer :: coordX, coordY
     real(ESMF_KIND_R8), dimension(:,:), pointer :: srcdata, resdata
     type(ESMF_ArraySpec) :: arrayspec1, arrayspec2
     type(ESMF_DELayout) :: delayout1, delayout2
     type(ESMF_Field) :: humidity1, humidity2, humidity3
     type(ESMF_Grid) :: grid1, grid2
     type(ESMF_GridHorzStagger) :: horz_stagger
     type(ESMF_RouteHandle) :: rh12, rh23
     type(ESMF_VM) :: vm

     ! cumulative result: count failures; no failures equals "all pass"
     integer :: testresult = 0

     ! individual test name
     character(ESMF_MAXSTR) :: testname

     ! individual test failure message and final rc msg
     character(ESMF_MAXSTR) :: failMsg, finalMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

     print *, "System Test FieldRedistBlk2Blk."
!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
     ! Initialize the framework and get back the default global VM
     call ESMF_Initialize(vm=vm, rc=status)
     if (status .ne. ESMF_SUCCESS) goto 20

     ! Get the PET count and our PET number
     call ESMF_VMGet(vm, localPet=myPet, petCount=npets, rc=status)
     if (status .ne. ESMF_SUCCESS) goto 20

     miscount = 0

     if (2*(npets/2) .ne. npets) then
       print *, "This test must run on an even number of processes"
       goto 20
     endif

     print *, "Create section finished"
!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
     ! create two layouts, both are normal distribution but 
     ! different layout
     delayout1 = ESMF_DELayoutCreate(vm, (/ 2, npets/2 /), rc=status)
     if (status .ne. ESMF_SUCCESS) goto 20
     delayout2 = ESMF_DELayoutCreate(vm, (/ npets/2, 2 /), rc=status)
     if (status .ne. ESMF_SUCCESS) goto 20

     ! and get our local DE number on the first layout
     call ESMF_DELayoutGetDeprecated(delayout1, localDE=myDE, rc=status)
     if (status .ne. ESMF_SUCCESS) goto 20

     ! Create the grids and corresponding Fields
     counts(1) = 60
     counts(2) = 40
     min(1) = 0.0
     max(1) = 60.0
     min(2) = 0.0
     max(2) = 50.0
     horz_stagger = ESMF_GRID_HORZ_STAGGER_A

     ! make two identical grids, both are distributed in the normal
     ! block style but different layout
     grid1 = ESMF_GridCreateHorzXYUni(counts=counts, &
                             minGlobalCoordPerDim=min, &
                             maxGlobalCoordPerDim=max, &
                             horzStagger=horz_stagger, &
                             name="source grid", rc=status)
     if (status .ne. ESMF_SUCCESS) goto 20
     call ESMF_GridDistribute(grid1, delayout=delayout1, rc=status)
     if (status .ne. ESMF_SUCCESS) goto 20

     grid2 = ESMF_GridCreateHorzXYUni(counts=counts, &
                             minGlobalCoordPerDim=min, &
                             maxGlobalCoordPerDim=max, &
                             horzStagger=horz_stagger, &
                             name="source grid", rc=status)
     if (status .ne. ESMF_SUCCESS) goto 20
     call ESMF_GridDistribute(grid2, delayout=delayout2, rc=status)
     if (status .ne. ESMF_SUCCESS) goto 20

     ! Set up a 2D (distributed Field) and a 2D real array
     call ESMF_ArraySpecSet(arrayspec1, rank=2, type=ESMF_DATA_REAL, &
                            kind=ESMF_R8)
     if (status .ne. ESMF_SUCCESS) goto 20
     call ESMF_ArraySpecSet(arrayspec2, rank=2, type=ESMF_DATA_REAL, &
                            kind=ESMF_R8)
     if (status .ne. ESMF_SUCCESS) goto 20

     ! Create the field and have it create the array internally for each grid
     humidity1 = ESMF_FieldCreate(grid1, arrayspec1, &
                                  horzRelloc=ESMF_CELL_CENTER, &
                                  haloWidth=0, name="humidity1", rc=status)
     if (status .ne. ESMF_SUCCESS) goto 20
     humidity2 = ESMF_FieldCreate(grid2, arrayspec2, &
                                  horzRelloc=ESMF_CELL_CENTER, &
                                  haloWidth=0, name="humidity2", rc=status)
     if (status .ne. ESMF_SUCCESS) goto 20
     humidity3 = ESMF_FieldCreate(grid1, arrayspec1, &
                                  horzRelloc=ESMF_CELL_CENTER, &
                                  haloWidth=0, name="humidity3", rc=status)
     if (status .ne. ESMF_SUCCESS) goto 20

     ! precompute communication patterns, the first from the 1st regularly
     ! distributed Field to the another distributed Field and the second
     ! from the 2nd regularly distributed Field back to the 1st
     ! distributed Field
     call ESMF_FieldRedistStore(humidity1, humidity2, vm, &
                                routehandle=rh12, rc=status)
     call ESMF_FieldRedistStore(humidity2, humidity3, vm, &
                                routehandle=rh23, rc=status)

    ! get coordinate arrays available for setting the source data array
    call ESMF_GridGetCoord(grid1, dim=1, horzRelloc=ESMF_CELL_CENTER, &
      centerCoord=coordX, localCounts=localCounts, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 20
    call ESMF_GridGetCoord(grid1, dim=2, horzRelloc=ESMF_CELL_CENTER, &
      centerCoord=coordY, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 20

    ! Get pointers to the data and set it up
    call ESMF_FieldGetDataPointer(humidity1, srcdata, ESMF_DATA_REF, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 20
    call ESMF_FieldGetDataPointer(humidity3, resdata, ESMF_DATA_REF, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 20

    ! initialize data arrays
    srcdata = 0.0
    resdata = 0.0

    ! set data array to a function of coordinates (in the computational part
    ! of the array only, not the halo region)
    do j    = 1,localCounts(2)
      do i = 1,localCounts(1)
        srcdata(i,j) = 10.0 + 5.0*sin(coordX(i,j)/60.0*pi) &
                            + 2.0*sin(coordY(i,j)/50.0*pi) 
      enddo
    enddo

    print *, "Initial data, before Redistribution:"

    ! No deallocate() is needed for array data, it will be freed when the
    ! Array is destroyed. 

    print *, "Init section finished"
!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!

    ! Call redistribution method here, output ends up in humidity2
    call ESMF_FieldRedist(humidity1, humidity2, rh12, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 20

    print *, "Array contents after Transpose:"

    ! Redistribute back so we can compare contents
    ! output ends up in humidity3
    call ESMF_FieldRedist(humidity2, humidity3, rh23, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 20

    print *, "Array contents after second Redistribution, should match original:"

    print *, "Run section finished"

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   Print result

    call ESMF_DELayoutGetDeprecated(delayout1, localDE=myDE, rc=status)

    print *, "-----------------------------------------------------------------"
    print *, "-----------------------------------------------------------------"
    print *, "Result from DE number ", myDE
    print *, "-----------------------------------------------------------------"
    print *, "-----------------------------------------------------------------"

    ! check and make sure the original data and the data that has been 
    ! distributed to the Field and back again are the same
    match    = .true.
    miscount = 0
    do j   = 1,localCounts(2)
      do i = 1,localCounts(1)
        compval = 10.0 + 5.0*sin(coordX(i,j)/60.0*pi) &
                       + 2.0*sin(coordY(i,j)/50.0*pi)
        if ((srcdata(i,j) .ne. resdata(i,j)) .OR. &
            (abs(resdata(i,j)-compval).ge.1.0d-12)) then
          print *, "array contents do not match at: (", i,j, ") on DE ", &
                   myDE, ".  src=", srcdata(i,j), "dst=", &
                   resdata(i,j), "realval=", compval
          match = .false.
          miscount = miscount + 1
          if (miscount .gt. 40) then
            print *, "more than 40 mismatches, skipping rest of loop"
            goto 10
          endif
        endif
      enddo
    enddo
    if (match) print *, "Array contents matched correctly!! DE = ", myDE
10  continue

    print *, "Finalize section finished"

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   Destroy section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   Clean up

    call ESMF_FieldRedistRelease(rh12, status)
    if (status .ne. ESMF_SUCCESS) goto 20
    call ESMF_FieldRedistRelease(rh23, status)
    if (status .ne. ESMF_SUCCESS) goto 20
    call ESMF_FieldDestroy(humidity1, status)
    if (status .ne. ESMF_SUCCESS) goto 20
    call ESMF_FieldDestroy(humidity2, status)
    if (status .ne. ESMF_SUCCESS) goto 20
    call ESMF_FieldDestroy(humidity3, status)
    if (status .ne. ESMF_SUCCESS) goto 20
    call ESMF_GridDestroy(grid1, status)
    if (status .ne. ESMF_SUCCESS) goto 20
    call ESMF_GridDestroy(grid2, status)
    if (status .ne. ESMF_SUCCESS) goto 20
    call ESMF_DELayoutDestroy(delayout1, status)
    if (status .ne. ESMF_SUCCESS) goto 20
    call ESMF_DELayoutDestroy(delayout2, status)
    if (status .ne. ESMF_SUCCESS) goto 20
    print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
20  print *, "System Test FieldRedistBlk2Blk complete."

    write(failMsg, *)  "Redistribution back not same as original"
    write(testname, *) "System Test FieldRedistBlk2Blk: Field Redistribute"

    call ESMF_TestGlobal(((miscount.eq.0).and.(status.eq.ESMF_SUCCESS)), &
      testname, failMsg, testresult, ESMF_SRCLINE)

    if ((myPet .eq. 0) .or. (status .ne. ESMF_SUCCESS)) then
      ! Separate message to console, for quick confirmation of success/failure
      if ((miscount.eq.0) .and. (status .eq. ESMF_SUCCESS)) then
        write(finalMsg, *) "SUCCESS: Data redistributed twice same as original."
      else
        write(finalMsg, *) "System Test did not succeed. ", &
        "Data redistribution does not match expected values, or error code", &
        " set ",status 
      endif
      write(0, *) ""
      write(0, *) trim(testname)
      write(0, *) trim(finalMsg)
      write(0, *) ""

    endif
    
    call ESMF_Finalize(rc=status)

    end program Blk2BlkFldRedist
    
!\end{verbatim}
