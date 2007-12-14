! $Id: ESMF_BundleRedistBlk2ArbSTest.F90,v 1.10 2007/12/14 19:53:22 svasquez Exp $
!
! System test ESMF_BundleRedistBlk2Arb
!  Description on Sourceforge under System Test #XXXXX

!-------------------------------------------------------------------------
!ESMF_SYSTEM_removeTEST        String used by test script to count system tests.
!=========================================================================

!BOP
!
! !DESCRIPTION:
! System test BundleRedistBlk2Arb.
!
! This system test checks the functionality of the arbitrary igrid distribution
! routines by redistributing data from one Bundle distributed in the normal
! block structure to another Bundle that has been distributed arbitrarily
! and then back again.  The original data should exactly match the final
! data, which serves as the test for SUCCESS.  This program creates two
! identical IGrids with different distributions, one with the normal block
! structure and the other with a semi-random arbitrary distribution.  The first
! IGrid has two Bundle created from it, the first as the source for the test
! and the second for the final results.  The second IGrid has a single Bundle
! that serves as an intermediate result between the two redistributions.
!
!\begin{verbatim}

     program Blk2ArbBunRedist

#include <ESMF_Macros.inc>

     ! ESMF Framework module
     use ESMF_Mod
     use ESMF_TestMod
    
     implicit none

     ! Local variables
     integer :: status
     integer :: i, j, j1, add
     integer :: counts(2), localCounts(2), miscount
     integer :: npets, myDE, myPet
     integer, dimension(:,:), allocatable :: myIndices
     logical :: match
     real(ESMF_KIND_R8) :: min(2), max(2), compval
     real(ESMF_KIND_R8) :: pi = 3.1416d0
     real(ESMF_KIND_R8), dimension(:,:), pointer :: coordX, coordY
     real(ESMF_KIND_R8), dimension(:,:), pointer :: srcdata, resdata
     type(ESMF_ArraySpec) :: arrayspec1, arrayspec2
     type(ESMF_DELayout) :: delayout1, delayout2
     type(ESMF_Field) :: humidity1, humidity2, humidity3
     type(ESMF_Bundle) :: bundle1, bundle2, bundle3
     type(ESMF_IGrid) :: igrid1, igrid2
     type(ESMF_IGridHorzStagger) :: horz_stagger
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

     print *, "System Test BundleRedistBlk2Arb."
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
     ! create two layouts, one for a normal distribution and a 1D layout
     ! for arbitrary distribution
     delayout1 = ESMF_DELayoutCreate(vm, (/ 2, npets/2 /), rc=status)
     if (status .ne. ESMF_SUCCESS) goto 20
     delayout2 = ESMF_DELayoutCreate(vm, (/ npets, 1 /), rc=status)
     if (status .ne. ESMF_SUCCESS) goto 20

     ! and get our local DE number on the first layout
     call ESMF_DELayoutGetDeprecated(delayout1, localDE=myDE, rc=status)
     if (status .ne. ESMF_SUCCESS) goto 20

     ! Create the igrids and corresponding Fields
     counts(1) = 60
     counts(2) = 40
     min(1) = 0.0
     max(1) = 60.0
     min(2) = 0.0
     max(2) = 50.0
     horz_stagger = ESMF_IGRID_HORZ_STAGGER_A

     ! make two identical igrids, except one is distributed in the normal
     ! block style and the second is distributed in arbitrary style
     igrid1 = ESMF_IGridCreateHorzXYUni(counts=counts, &
                             minGlobalCoordPerDim=min, &
                             maxGlobalCoordPerDim=max, &
                             horzStagger=horz_stagger, &
                             name="source igrid", rc=status)
     if (status .ne. ESMF_SUCCESS) goto 20
     call ESMF_IGridDistribute(igrid1, delayout=delayout1, rc=status)
     if (status .ne. ESMF_SUCCESS) goto 20

     igrid2 = ESMF_IGridCreateHorzXYUni(counts=counts, &
                             minGlobalCoordPerDim=min, &
                             maxGlobalCoordPerDim=max, &
                             horzStagger=horz_stagger, &
                             name="source igrid", rc=status)
     if (status .ne. ESMF_SUCCESS) goto 20

     ! allocate myIndices to maximum number of points on any DE in the first
     ! dimension and 2 in the second dimension.
     i = int((counts(1)*counts(2) + npets -1)/npets)
     allocate (myIndices(i,2))

     ! calculate myIndices based on DE number
     ! for now, start at point (1,1+myDE) and go up in the j-direction first
     ! to create a semi-regular distribution of points
     j1  = 1 + myDE
     add = 0
     do i = 1,counts(1)
       do j = j1,counts(2),npets
         add = add + 1
         myIndices(add,1) = i
         myIndices(add,2) = j
       enddo
       j1 = j - counts(2)
     enddo

     ! the distribute call is similar to the block distribute but with
     ! a couple of different arguments
     call ESMF_IGridDistribute(igrid2, delayout=delayout2, myCount=add, &
                              myIndices=myIndices, rc=status)
     if (status .ne. ESMF_SUCCESS) goto 20

     ! Set up a 1D (for the arbitrarily distributed Field) and a 2D real array
     call ESMF_ArraySpecSet(arrayspec1, rank=2, &
                            typekind=ESMF_TYPEKIND_R8)
     if (status .ne. ESMF_SUCCESS) goto 20
     call ESMF_ArraySpecSet(arrayspec2, rank=1, &
                            typekind=ESMF_TYPEKIND_R8)
     if (status .ne. ESMF_SUCCESS) goto 20

     ! Create bundles
     bundle1 = ESMF_BundleCreate(igrid1, 'Bundle1', rc=status)
     if (status .ne. ESMF_SUCCESS) goto 20

     bundle2 = ESMF_BundleCreate(igrid2, 'Bundle2', rc=status)
     if (status .ne. ESMF_SUCCESS) goto 20

     bundle3 = ESMF_BundleCreate(igrid1, 'Bundle3', rc=status)
     if (status .ne. ESMF_SUCCESS) goto 20

     ! Create the field and have it create the array internally for each igrid
     humidity1 = ESMF_FieldCreate(igrid1, arrayspec1, &
                                  horzRelloc=ESMF_CELL_CENTER, &
                                  haloWidth=0, name="humidity1", rc=status)
     call ESMF_BundleAddField(bundle1, humidity1, rc=status)

     if (status .ne. ESMF_SUCCESS) goto 20
     humidity2 = ESMF_FieldCreate(igrid2, arrayspec2, &
                                  horzRelloc=ESMF_CELL_CENTER, &
                                  haloWidth=0, name="humidity2", rc=status)
     call ESMF_BundleAddField(bundle2, humidity2, rc=status)
     if (status .ne. ESMF_SUCCESS) goto 20

     humidity3 = ESMF_FieldCreate(igrid1, arrayspec1, &
                                  horzRelloc=ESMF_CELL_CENTER, &
                                  haloWidth=0, name="humidity3", rc=status)
     call ESMF_BundleAddField(bundle3, humidity3, rc=status)
     if (status .ne. ESMF_SUCCESS) goto 20

     ! precompute communication patterns, the first from the regularly
     ! distributed Bundle to the arbitrarily distributed Bundle and the second
     ! from the arbitrarily distributed Bundle back to a different regularly
     ! distributed Bundle
     call ESMF_BundleRedistStore(bundle1, bundle2, vm, rh12, rc=status)
     call ESMF_BundleRedistStore(bundle2, bundle3, vm, rh23, rc=status)

    ! get coordinate arrays available for setting the source data array
    call ESMF_IGridGetCoord(igrid1, dim=1, horzRelloc=ESMF_CELL_CENTER, &
      centerCoord=coordX, localCounts=localCounts, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 20
    call ESMF_IGridGetCoord(igrid1, dim=2, horzRelloc=ESMF_CELL_CENTER, &
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
    call ESMF_BundleRedist(bundle1, bundle2, rh12, rc=status)
    call ESMF_BundleGetField(bundle2, "humidity2", humidity2, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 20

    print *, "Array contents after Transpose:"

    ! Redistribute back so we can compare contents
    ! output ends up in humidity3
    call ESMF_BundleRedist(bundle2, bundle3, rh23, rc=status)
    call ESMF_BundleGetField(bundle3, "humidity3", humidity3, rc=status)
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

    deallocate(myIndices)

    call ESMF_BundleRedistRelease(rh12, status)
    if (status .ne. ESMF_SUCCESS) goto 20
    call ESMF_BundleRedistRelease(rh23, status)
    if (status .ne. ESMF_SUCCESS) goto 20
    call ESMF_FieldDestroy(humidity1, status)
    if (status .ne. ESMF_SUCCESS) goto 20
    call ESMF_FieldDestroy(humidity2, status)
    if (status .ne. ESMF_SUCCESS) goto 20
    call ESMF_FieldDestroy(humidity3, status)
    if (status .ne. ESMF_SUCCESS) goto 20
    call ESMF_IGridDestroy(igrid1, status)
    if (status .ne. ESMF_SUCCESS) goto 20
    call ESMF_IGridDestroy(igrid2, status)
    if (status .ne. ESMF_SUCCESS) goto 20
    call ESMF_DELayoutDestroy(delayout1, status)
    if (status .ne. ESMF_SUCCESS) goto 20
    call ESMF_DELayoutDestroy(delayout2, status)
    if (status .ne. ESMF_SUCCESS) goto 20
    print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
20  print *, "System Test BundleRedistBlk2Arb complete."

    write(failMsg, *)  "Redistribution back not same as original"
    write(testname, *) "System Test BundleRedistBlk2Arb: Bundle Redistribute"

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

    end program Blk2ArbBunRedist
    
!\end{verbatim}
