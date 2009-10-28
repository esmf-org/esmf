! $Id: ESMF_FieldBundleRedistArb2ArbSTest.F90,v 1.8 2009/10/28 03:05:24 theurich Exp $
!
! System test FieldBundleRedistArb2Arb
!  Description on Sourceforge under System Test #XXXXX

!-------------------------------------------------------------------------
!ESMF_SYSTEM_TEST        String used by test script to count system tests.
!=========================================================================

!BOP
!
! !DESCRIPTION:
! System test FieldBundleRedistArb2Arb.
!
! This system test checks the functionality of the grid distribution
! routines by redistributing data from one FieldBundle arbitrarily distributed
! structure to another FieldBundle that has been distributed arbitrarily
! and then back again.  The original data should exactly match the final
! data, which serves as the test for SUCCESS.  This program creates two
! identical grids with different distributions, both with the semi-random
! arbitrary distribution.  The first grid has two FieldBundle created from it,
! the first as the source for the test and the second for the final results.
! The second grid has a single FieldBundle that serves as an intermediate
! result between the two redistributions.
!
!\begin{verbatim}

program Arb2ArbBunReDist

#include "ESMF_Macros.inc"

     ! ESMF Framework module
     use ESMF_Mod
     use ESMF_TestMod
    
     implicit none

     ! Local variables
     integer :: status
     integer :: i, j, j1, i1, add
     integer :: counts(2), localCounts(2), miscount, localCount
     integer :: npets, localPet
     integer, dimension(:,:), allocatable :: myIndices1, myIndices2
     logical :: match
     real(ESMF_KIND_R8) :: min(2), max(2), compval
     real(ESMF_KIND_R8) :: pi = 3.1416d0
     real(ESMF_KIND_R8), dimension(:), pointer :: coordX, coordY
     real(ESMF_KIND_R8), dimension(:), pointer :: srcdata, resdata
     type(ESMF_ArraySpec) :: arrayspec1, arrayspec2
     type(ESMF_Field) :: humidity1, humidity2, humidity3
     type(ESMF_FieldBundle) :: bundle1, bundle2, bundle3
     type(ESMF_grid) :: grid1, grid2
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

  write(testname, *) "System Test FieldBundleRedistArb2Arb"
  write(failMsg, *) "System Test failure"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  print *, "--------------------------------------- "
  print *, "Start of ", trim(testname)
  print *, "--------------------------------------- "

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
     ! Initialize the framework and get back the default global VM
     call ESMF_Initialize(vm=vm, defaultlogfilename="FieldBundleRedistArb2ArbSTest.Log", &
                        defaultlogtype=ESMF_LOG_MULTI, rc=status)
     if (status .ne. ESMF_SUCCESS) goto 20

     ! Get the PET count and our PET number
     call ESMF_VMGet(vm, localPet=localPet, petCount=npets, rc=status)
     if (status .ne. ESMF_SUCCESS) goto 20

     miscount = 0

     if (npets .eq. 1) then
       print *, "This test must run with > 1 processor"
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

     ! make two identical grids, both are distributed in arbitrary style
     ! with slightly different delayout.
     !
     ! Arbitrary case
     ! allocate myIndices to maximum number of points on any DE in the first
     ! dimension and 2 in the second dimension.
     localCount = int((counts(1)*counts(2) + npets -1)/npets)
     allocate (myIndices1(localCount,2))
     ! calculate myIndices based on DE number
     ! for now, start at point (1,1+localPet) and go up in the j-direction first
     ! to create a semi-regular distribution of points
     i1  = 1 + localPet
     add = 0
     do j = 1,counts(2)
       do i = i1,counts(1),npets
         add = add + 1
         myIndices1(add,1) = i
         myIndices1(add,2) = j
       enddo
       i1 = i - counts(1)
     enddo

     grid1 = ESMF_GridCreateShapeTile("source grid", coordTypeKind=ESMF_TYPEKIND_R8, &
       minIndex=(/1,1/), maxIndex=counts, &
       localArbIndex=myIndices1,localArbIndexCount=localCount,rc=status)
     if (status .ne. ESMF_SUCCESS) goto 20
     call ESMF_GridAddCoord(grid1, rc=status)
     if (status .ne. ESMF_SUCCESS) goto 20


     ! allocate myIndices to maximum number of points on any DE in the first
     ! dimension and 2 in the second dimension.
     localCount = int((counts(1)*counts(2) + npets -1)/npets)
     allocate (myIndices2(localCount,2))

     ! calculate myIndices based on DE number
     ! for now, start at point (1,1+localPet) and go up in the j-direction first
     ! to create a semi-regular distribution of points
     j1  = 1 + localPet
     add = 0
     do i = 1,counts(1)
       do j = j1,counts(2),npets
         add = add + 1
         myIndices2(add,1) = i
         myIndices2(add,2) = j
       enddo
       j1 = j - counts(2)
     enddo

     grid2 = ESMF_GridCreateShapeTile("dest grid", coordTypeKind=ESMF_TYPEKIND_R8, &
       minIndex=(/1,1/), maxIndex=counts, &
       localArbIndex=myIndices2,localArbIndexCount=localCount,rc=status)
     if (status .ne. ESMF_SUCCESS) goto 20

     ! Set up a 1D (for the arbitrarily distributed FieldBundle) and a 1D real array
     call ESMF_ArraySpecSet(arrayspec1, rank=1, &
                            typekind=ESMF_TYPEKIND_R8)
     if (status .ne. ESMF_SUCCESS) goto 20
     call ESMF_ArraySpecSet(arrayspec2, rank=1, &
                            typekind=ESMF_TYPEKIND_R8)
     if (status .ne. ESMF_SUCCESS) goto 20

     ! Create bundles
     bundle1 = ESMF_FieldBundleCreate(grid1, 'FieldBundle1', rc=status)
     if (status .ne. ESMF_SUCCESS) goto 20

     bundle2 = ESMF_FieldBundleCreate(grid2, 'FieldBundle2', rc=status)
     if (status .ne. ESMF_SUCCESS) goto 20

     bundle3 = ESMF_FieldBundleCreate(grid1, 'FieldBundle3', rc=status)
     if (status .ne. ESMF_SUCCESS) goto 20

     ! Create the field and have it create the array internally for each grid
     ! and add the Fields to the FieldBundle corresponding to the grid.
     humidity1 = ESMF_FieldCreate(grid1, arrayspec1, &
                                  name="humidity1", rc=status)
     call ESMF_FieldBundleAdd(bundle1, humidity1, rc=status)

     if (status .ne. ESMF_SUCCESS) goto 20
     humidity2 = ESMF_FieldCreate(grid2, arrayspec2, &
                                  name="humidity2", rc=status)
     call ESMF_FieldBundleAdd(bundle2, humidity2, rc=status)
     if (status .ne. ESMF_SUCCESS) goto 20

     humidity3 = ESMF_FieldCreate(grid1, arrayspec1, &
                                  name="humidity3", rc=status)
     call ESMF_FieldBundleAdd(bundle3, humidity3, rc=status)
     if (status .ne. ESMF_SUCCESS) goto 20

     ! precompute communication patterns, the first from the 1st arbitrarily
     ! distributed FieldBundle to the 2nd arbitrarily distributed FieldBundle; and
     ! the second from the 2nd arbitrarily distributed FieldBundle back to
     ! the 1st distributed FieldBundle
     call ESMF_FieldBundleRedistStore(bundle1, bundle2, rh12, rc=status)
     call ESMF_FieldBundleRedistStore(bundle2, bundle3, rh23, rc=status)

    ! get coordinate arrays available for setting the source data array
    call ESMF_GridGetCoord(grid1, localDE=0, coordDim=1, &
      fptr=coordX, totalCount=localCounts, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 20
    call ESMF_GridGetCoord(grid1, localDE=0, coordDim=2, &
      fptr=coordY, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 20

    ! Get pointers to the data and set it up
    call ESMF_FieldGet(humidity1, farrayPtr=srcdata, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 20
    call ESMF_FieldGet(humidity3, farrayPtr=resdata, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 20

    ! initialize data arrays
    srcdata = 0.0
    resdata = 0.0

    ! set data array to a function of coordinates (in the computational part
    ! of the array only, not the halo region)
    do i = 1,localCounts(1)
      coordX(i) = ((max(1)-min(1))*i)/localCounts(1) 
      coordY(i) = ((max(2)-min(2))*i)/localCounts(1)
      srcdata(i) = 10.0 + 5.0*sin(coordX(i)/60.0*pi) &
                        + 2.0*sin(coordY(i)/50.0*pi)
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
    call ESMF_FieldBundleRedist(bundle1, bundle2, rh12, rc=status)
    print *, "Run ESMF_FieldBundleRedist :",status,ESMF_SUCCESS
    call ESMF_FieldBundleGet(bundle2, "humidity2", humidity2, rc=status)
    print *, "Run ESMF_FieldBundleGetField :",status,ESMF_SUCCESS
    if (status .ne. ESMF_SUCCESS) goto 20

    print *, "Array contents after Transpose:"

    ! Redistribute back so we can compare contents
    ! output ends up in humidity3
    call ESMF_FieldBundleRedist(bundle2, bundle3, rh23, rc=status)
    call ESMF_FieldBundleGet(bundle3, "humidity3", humidity3, rc=status)
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

    print *, "-----------------------------------------------------------------"
    print *, "-----------------------------------------------------------------"
    print *, "Result from PET number ", localPet
    print *, "-----------------------------------------------------------------"
    print *, "-----------------------------------------------------------------"

    ! check and make sure the original data and the data that has been 
    ! distributed to the 1D Field and back again are the same
    match    = .true.
    miscount = 0
    do i = 1,localCounts(1)
        compval = 10.0 + 5.0*sin(coordX(i)/60.0*pi) &
                       + 2.0*sin(coordY(i)/50.0*pi)
        if ((srcdata(i) .ne. resdata(i)) .OR. &
            (abs(resdata(i)-compval).ge.1.0d-12)) then
          print *, "array contents do not match at: (", i , ") on DE ", &
                   localPet, ".  src=", srcdata(i), "dst=", &
                   resdata(i), "realval=", compval
          match = .false.
          miscount = miscount + 1
          if (miscount .gt. 40) then
            print *, "more than 40 mismatches, skipping rest of loop"
            goto 10
          endif
        endif
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

    deallocate(myIndices1, myIndices2)

    call ESMF_FieldBundleRedistRelease(rh12, status)
    if (status .ne. ESMF_SUCCESS) goto 20
    call ESMF_FieldBundleRedistRelease(rh23, status)
    if (status .ne. ESMF_SUCCESS) goto 20
    call ESMF_FieldBundleDestroy(bundle1, status)
    if (status .ne. ESMF_SUCCESS) goto 20
    call ESMF_FieldBundleDestroy(bundle2, status)
    if (status .ne. ESMF_SUCCESS) goto 20
    call ESMF_FieldBundleDestroy(bundle3, status)
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
    print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

20 continue

  ! Normal ESMF Test output
  print *, testname, " complete."

  if (status .eq. ESMF_SUCCESS) then
    ! Separate message to console, for quick confirmation of success/failure
    write(finalMsg, *) "SUCCESS: ",trim(testname)," finished correctly."
    write(0, *) ""
    write(0, *) trim(testname)
    write(0, *) trim(finalMsg)
    write(0, *) ""
  endif
  
  print *, "------------------------------------------------------------"
  print *, "------------------------------------------------------------"
  print *, "Test finished, localPet = ", localPet
  print *, "------------------------------------------------------------"
  print *, "------------------------------------------------------------"

  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((status.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

  call ESMF_Finalize()

end program Arb2ArbBunReDist
    
!\end{verbatim}
