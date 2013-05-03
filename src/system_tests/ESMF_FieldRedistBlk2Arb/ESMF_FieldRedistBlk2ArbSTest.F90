! $Id$
!
! System test FieldRedistBlk2Arb
!  Description on Sourceforge under System Test #XXXXX

!-------------------------------------------------------------------------
!ESMF_MULTI_PROC_SYSTEM_TEST        String used by test script to count system tests.
!=========================================================================

!BOP
!
! !DESCRIPTION:
! System test FieldRedistBlk2Arb.
!
! This system test checks the functionality of the arbitrary grid distribution
! routines by redistributing data from one Field distributed in the normal
! block structure to another Field that has been distributed arbitrarily
! and then back again.  The original data should exactly match the final
! data, which serves as the test for SUCCESS.  This program creates two
! identical Grids with different distributions, one with the normal block
! structure and the other with a semi-random arbitrary distribution.  The first
! Grid has two Fields created from it, the first as the source for the test
! and the second for the final results.  The second Grid has a single Field
! that serves as an intermediate result between the two redistributions.
!
!\begin{verbatim}

     program Blk2ArbFldRedist

     ! ESMF Framework module
     use ESMF
     use ESMF_TestMod
    
     implicit none

     ! Local variables
     integer :: rc
     integer :: i, j, j1, add, localCount
     integer :: counts(2), localCounts(2), miscount
     integer :: npets, localPet
     integer, dimension(:,:), allocatable :: myIndices
     logical :: match
     real(ESMF_KIND_R8) :: min(2), max(2), compval
     real(ESMF_KIND_R8) :: pi = 3.1416d0
     real(ESMF_KIND_R8), dimension(:,:), pointer :: coordX, coordY
     real(ESMF_KIND_R8), dimension(:,:), pointer :: srcdata, resdata
     type(ESMF_ArraySpec) :: arrayspec1, arrayspec2
     type(ESMF_Field) :: humidity1, humidity2, humidity3
     type(ESMF_Grid) :: grid1, grid2
     type(ESMF_RouteHandle) :: rh12, rh23
     type(ESMF_VM) :: vm

     ! cumulative result: count failures; no failures equals "all pass"
     integer :: result = 0

     ! individual test name
     character(ESMF_MAXSTR) :: testname

     ! individual test failure message and final rc msg
     character(ESMF_MAXSTR) :: failMsg, finalMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

     print *, "System Test FieldRedistBlk2Arb."
!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
     ! Initialize the framework and get back the default global VM
     call ESMF_Initialize(vm=vm, defaultlogfilename="FieldRedistBlk2ArbSTest.Log", &
                        logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
     if (rc .ne. ESMF_SUCCESS) goto 20

     ! Get the PET count and our PET number
     call ESMF_VMGet(vm, localPet=localPet, petCount=npets, rc=rc)
     if (rc .ne. ESMF_SUCCESS) goto 20

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

     ! Create the grids and corresponding Fields
     counts(1) = 60
     counts(2) = 40
     min(1) = 0.0
     max(1) = 60.0
     min(2) = 0.0
     max(2) = 50.0

     ! make two identical grids, except one is distributed in the normal
     ! block style and the second is distributed in arbitrary style
     grid1 = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=counts, &
                            gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &  
                             name="source grid", rc=rc)
     if (rc .ne. ESMF_SUCCESS) goto 20
     call ESMF_GridAddCoord(grid1, rc=rc)
     if (rc .ne. ESMF_SUCCESS) goto 20

     ! allocate myIndices to maximum number of points on any DE in the first
     ! dimension and 2 in the second dimension.
     localCount = int((counts(1)*counts(2) + npets -1)/npets)
     allocate (myIndices(localCount,2))

     ! calculate myIndices based on DE number
     ! for now, start at point (1,1+localPet) and go up in the j-direction first
     ! to create a semi-regular distribution of points
     j1  = 1 + localPet
     add = 0
     do i = 1,counts(1)
       do j = j1,counts(2),npets
         add = add + 1
         myIndices(add,1) = i
         myIndices(add,2) = j
       enddo
       j1 = j - counts(2)
     enddo

     grid2 = ESMF_GridCreateNoPeriDim(coordTypeKind=ESMF_TYPEKIND_R8, &
       minIndex=(/1,1/), maxIndex=counts, &
       arbIndexList=myIndices,arbIndexCount=localCount, &
       name="arbgrid", rc=rc)
     if (rc .ne. ESMF_SUCCESS) goto 20

     ! Set up a 1D (for the arbitrarily distributed Field) and a 2D real array
     call ESMF_ArraySpecSet(arrayspec1, rank=2, &
                            typekind=ESMF_TYPEKIND_R8)
     if (rc .ne. ESMF_SUCCESS) goto 20
     call ESMF_ArraySpecSet(arrayspec2, rank=1, &
                            typekind=ESMF_TYPEKIND_R8)
     if (rc .ne. ESMF_SUCCESS) goto 20

     ! Create the field and have it create the array internally for each grid
     humidity1 = ESMF_FieldCreate(grid1, arrayspec1, &
                                  name="humidity1", rc=rc)
     if (rc .ne. ESMF_SUCCESS) goto 20
     humidity2 = ESMF_FieldCreate(grid2, arrayspec2, &
                                  name="humidity2", rc=rc)
     if (rc .ne. ESMF_SUCCESS) goto 20
     humidity3 = ESMF_FieldCreate(grid1, arrayspec1, &
                                  name="humidity3", rc=rc)
     if (rc .ne. ESMF_SUCCESS) goto 20

     ! precompute communication patterns, the first from the regularly
     ! distributed Field to the arbitrarily distributed Field and the second
     ! from the arbitrarily distributed Field back to a different regularly
     ! distributed Field
     call ESMF_FieldRedistStore(humidity1, humidity2, &
                                routehandle=rh12, rc=rc)
     call ESMF_FieldRedistStore(humidity2, humidity3, &
                                routehandle=rh23, rc=rc)

    ! get coordinate arrays available for setting the source data array
    call ESMF_GridGetCoord(grid1, localDE=0, coordDim=1, &
      farrayPtr=coordX, totalCount=localCounts, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    call ESMF_GridGetCoord(grid1, localDE=0, coordDim=2, &
      farrayPtr=coordY, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20

    ! Get pointers to the data and set it up
    call ESMF_FieldGet(humidity1, farrayPtr=srcdata, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    call ESMF_FieldGet(humidity3, farrayPtr=resdata, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20

    ! initialize data arrays
    srcdata = 0.0
    resdata = 0.0

    ! set data array to a function of coordinates (in the computational part
    ! of the array only, not the halo region)
    do j   = 1,localCounts(2)
      do i = 1,localCounts(1)
        coordX(i, j) = ((max(1)-min(1))*i)/localCounts(1) 
        coordY(i, j) = ((max(2)-min(2))*j)/localCounts(2)
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
    call ESMF_FieldRedist(humidity1, humidity2, rh12, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20

    print *, "Array contents after Transpose:"

    ! Redistribute back so we can compare contents
    ! output ends up in humidity3
    call ESMF_FieldRedist(humidity2, humidity3, rh23, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20

    print *, "Array contents after second Redistribution, should match original:"

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
    print *, "Result from PET number ", localPet
    print *, "-----------------------------------------------------------------"
    print *, "-----------------------------------------------------------------"

    ! check and make sure the original data and the data that has been 
    ! distributed to the 1D Field and back again are the same
    match    = .true.
    miscount = 0
    do j   = 1,localCounts(2)
      do i = 1,localCounts(1)
        compval = 10.0 + 5.0*sin(coordX(i,j)/60.0*pi) &
                       + 2.0*sin(coordY(i,j)/50.0*pi)
        if ((srcdata(i,j) .ne. resdata(i,j)) .OR. &
            (abs(resdata(i,j)-compval).ge.1.0d-12)) then
          print *, "array contents do not match at: (", i,j, ") on DE ", &
                   localPet, ".  src=", srcdata(i,j), "dst=", &
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
    if (match) print *, "Array contents matched correctly!! PET = ", localPet
10  continue

    print *, "Finalize section finished"

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   Destroy section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   Clean up

    deallocate(myIndices)

    call ESMF_FieldRedistRelease(rh12, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    call ESMF_FieldRedistRelease(rh23, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    call ESMF_FieldDestroy(humidity1, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    call ESMF_FieldDestroy(humidity2, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    call ESMF_FieldDestroy(humidity3, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    call ESMF_GridDestroy(grid1, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    call ESMF_GridDestroy(grid2, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
20  print *, "System Test FieldRedistBlk2Arb complete."

    write(failMsg, *)  "Redistribution back not same as original"
    write(testname, *) "System Test FieldRedistBlk2Arb: Field Redistribute"

    if (rc .ne. ESMF_SUCCESS) then
      ! Separate message to console, for quick confirmation of success/failure
      if ((miscount.eq.0) .and. (rc .eq. ESMF_SUCCESS)) then
        write(finalMsg, *) "SUCCESS: Data redistributed twice same as original."
      else
        write(finalMsg, *) "System Test did not succeed. ", &
        "Data redistribution does not match expected values, or error code", &
        " set ",rc 
      endif
      write(0, *) ""
      write(0, *) trim(testname)
      write(0, *) trim(finalMsg)
      write(0, *) ""

    endif

      ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors
      ! into the Log file that the scripts grep for.
      call ESMF_STest((rc.eq.ESMF_SUCCESS), testname, failMsg, result, &
        __FILE__, &
        __LINE__)

    call ESMF_Finalize(rc=rc)

    end program Blk2ArbFldRedist
    
!\end{verbatim}
