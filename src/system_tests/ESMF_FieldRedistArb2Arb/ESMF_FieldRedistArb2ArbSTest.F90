! $Id: ESMF_FieldRedistArb2ArbSTest.F90,v 1.11 2008/09/18 20:40:29 feiliu Exp $
!
! System test FieldRedistArb2Arb
!  Description on Sourceforge under System Test #XXXXX

!-------------------------------------------------------------------------
!ESMF_MULTI_PROC_SYSTEM_TEST        String used by test script to count system tests.
!=========================================================================

!BOP
!
! !DESCRIPTION:
! System test FieldRedistArb2Arb.
!
! This system test checks the functionality of the grid distribution
! routines by redistributing data from one Field arbitrarily distributed 
! structure to another Field that has been distributed arbitrarily
! and then back again.  The original data should exactly match the final
! data, which serves as the test for SUCCESS.  This program creates two
! identical Grids with different distributions, both with the semi-random
! arbitrary distribution.  The first Grid has two Fields created from it,
! the first as the source for the test and the second for the final results.
! The second Grid has a single Field that serves as an intermediate 
! result between the two redistributions.
!
!\begin{verbatim}

     program Arb2ArbFldReDist

#include <ESMF_Macros.inc>
#include <ESMF_Conf.inc>
#include <ESMF.h>
#define ESMF_METHOD "FieldRedistArb2ArbSTest"

     ! ESMF Framework module
     use ESMF_Mod
     use ESMF_TestMod
    
     implicit none
    
    ! Local variables
    integer :: i, j, rc, localrc
    integer :: npets, my_pet, is, ie, js, je
    integer :: miscount
    integer, dimension(2) :: counts
    logical :: match
    real(ESMF_KIND_R8) :: pi, compval
    integer, dimension(1) :: gclbx, gcubx
    integer, dimension(1) :: src_fclb, src_fcub, res_fclb, res_fcub
    real(ESMF_KIND_R8), dimension(:), pointer :: coordX
    real(ESMF_KIND_R8), dimension(:), pointer :: srcdata, resdata
    integer, dimension(:), allocatable :: arbIndexList_i, arbIndexList_j
    type(ESMF_ArraySpec) :: arrayspec, arrayspec2
    type(ESMF_DistGrid)  ::  distgrid1, distgrid2
    type(ESMF_Grid)  ::  grid1,  grid2
    type(ESMF_Field) :: field1, field2, field3
    type(ESMF_RouteHandle) :: rh12, rh23
    type(ESMF_VM):: vm

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: testresult = 0

    ! individual test name
    character(ESMF_MAXSTR) :: testname

    ! individual test failure message and final rc msg
    character(ESMF_MAXSTR) :: failMsg, finalMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    print *, "System Test FieldRedistArb2Arb:"
    rc = ESMF_SUCCESS
    localrc = ESMF_SUCCESS
!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
    ! Initialize the framework and get back the default global VM
    call ESMF_Initialize(vm=vm, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    ! Get the PET count and our PET number
    call ESMF_VMGet(vm, localPet=my_pet, petCount=npets, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    miscount = 0

    if (2*(npets/2) .ne. npets .or. npets*(60/npets) .ne. 60) then
        print *, "This test must run on an even number of processes divisible by 60"
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
    endif

    print *, "Create section finished"

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!

    !  Create the grids and corresponding Fields
    !  note that the Grids are the same but decomposed differently
    ! Perform a decomposition of arb-dist Fields equivalent in decomposing
    ! in the first dimension and 2nd dimension respectively.
    pi              = 3.14159
    counts(1)       = 60
    counts(2)       = 30
    is = counts(1)/npets*my_pet+1
    ie = is-1+counts(1)/npets
    js = counts(2)/npets*my_pet+1
    je = js-1+counts(2)/npets

    allocate(arbIndexList_i((ie-is+1)*counts(2)))
    allocate(arbIndexList_j((je-js+1)*counts(1)))
    do i = is, ie
        do j = 1, counts(2)
            arbIndexList_i((i-is)*counts(2)+j) = (i-1)*counts(2)+j
        enddo
    enddo
    do i = 1, counts(1)
        do j = js, je
            arbIndexList_j((i-1)*(je-js+1)+j-js+1) = (i-1)*counts(2)+j
        enddo
    enddo
    ! create arrayspec
    call ESMF_ArraySpecSet(arrayspec, rank=1, &
                           typekind=ESMF_TYPEKIND_R8, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
    call ESMF_ArraySpecSet(arrayspec2, rank=1, &
                           typekind=ESMF_TYPEKIND_R8, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
    
    ! create src and dst grids
    distgrid1 = ESMF_DistGridCreate(arbSeqIndexList=arbIndexList_i, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    grid1 = ESMF_GridCreate(distgrid=distgrid1, &
        gridEdgeLWidth=(/0/), gridEdgeUWidth=(/0/), &
        rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    distgrid2 = ESMF_DistGridCreate(arbSeqIndexList=arbIndexList_j, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    grid2 = ESMF_GridCreate(distgrid=distgrid2, &
        gridEdgeLWidth=(/0/), gridEdgeUWidth=(/0/), &
        rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    call ESMF_GridAddCoord(grid1, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    ! create src and dst fields
    field1 = ESMF_FieldCreate(grid1, arrayspec, rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    field3 = ESMF_FieldCreate(grid1, arrayspec, rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    field2 = ESMF_FieldCreate(grid2, arrayspec2, rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    ! precompute communication patterns
    call ESMF_FieldRedistStore(field1, field2, routehandle=rh12, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
    call ESMF_FieldRedistStore(field2, field3, routehandle=rh23, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    ! get coordinate arrays available for setting the source data array
    call ESMF_GridGetCoord(grid1, localDe=0, coordDim=1, &
        computationalLBound=gclbx, computationalUBound=gcubx, &
        fptr=coordX, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
    
    ! Get pointers to the data and set it up
    call ESMF_FieldGet(field1, localDe=0, farray=srcdata, &
        computationalLBound=src_fclb, computationalUBound=src_fcub, & 
        rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
    call ESMF_FieldGet(field3, localDe=0, farray=resdata, &
        computationalLBound=res_fclb, computationalUBound=res_fcub, & 
        rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    ! initialize data arrays
    srcdata = 0.0
    resdata = 0.0

    do i = gclbx(1), gcubx(1)
        coordX(i) = i*0.5
        srcdata(i) = 10.0 + 5.0*sin(coordX(i)/60.0*pi)
    enddo

    print *, "Initial data, before Transpose:"

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

    !! Call transpose method here, output ends up in field2
    call ESMF_FieldRedist(field1, field2, rh12, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    print *, "Array contents after Transpose:"

    !! Transpose back so we can compare contents
    !! Call transpose method again here, output ends up in field3
    call ESMF_FieldRedist(field2, field3, rh23, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    print *, "Array contents after second Transpose, should match original:"

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
    print *, "Result from PET number ", my_pet
    print *, "-----------------------------------------------------------------"
    print *, "-----------------------------------------------------------------"


    ! check and make sure the original data and the data that has been 
    ! distributed to the 1D Field and back again are the same
    match    = .true.
    miscount = 0
    do i = gclbx(1), gcubx(1)
        compval = 10.0 + 5.0*sin(coordX(i)/60.0*pi)
        if ((srcdata(i) .ne. resdata(i)) .OR. &
            (abs(resdata(i)-compval).ge.1.0d-12)) then
          print *, "array contents do not match at: (", i , ") on PET ", &
                   my_pet, ".  src=", srcdata(i), "dst=", &
                   resdata(i), "realval=", compval
          match = .false.
          miscount = miscount + 1
          if (miscount .gt. 40) then
            print *, "more than 40 mismatches, skipping rest of loop"
            goto 10
          endif
        endif
    enddo
    if (match) print *, "Array contents matched correctly!! PET = ", my_pet
10  continue

    print *, "Finalize section finished"
!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   Destroy section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   Clean up

    call ESMF_FieldRedistRelease(rh12, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    call ESMF_FieldRedistRelease(rh23, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    call ESMF_FieldDestroy(field1, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    call ESMF_FieldDestroy(field2, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    call ESMF_FieldDestroy(field3, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    call ESMF_GridDestroy(grid1, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    call ESMF_GridDestroy(grid2, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    call ESMF_DistgridDestroy(distgrid2, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    call ESMF_DistgridDestroy(distgrid1, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    deallocate(arbIndexList_i)
    deallocate(arbIndexList_j)

    print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
20  print *, "System Test FieldRedistArb2Arb complete."

    write(failMsg, *)  "Redistribution back not same as original"
    write(testname, *) "System Test FieldRedistArb2Arb: Field Redistribute"

    call ESMF_TestGlobal(((miscount.eq.0).and.(rc.eq.ESMF_SUCCESS)), &
      testname, failMsg, testresult, ESMF_SRCLINE)

    if ((my_pet .eq. 0) .or. (rc .ne. ESMF_SUCCESS)) then
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
    
    call ESMF_Finalize(rc=rc)

    end program Arb2ArbFldReDist
    
!\end{verbatim}
