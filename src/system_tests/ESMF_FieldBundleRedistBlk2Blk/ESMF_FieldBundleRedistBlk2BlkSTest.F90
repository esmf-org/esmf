! $Id: ESMF_FieldBundleRedistBlk2BlkSTest.F90,v 1.7 2009/10/28 03:05:27 theurich Exp $
!
! System test FieldBundleRedistBlk2Blk
!  Description on Sourceforge under System Test #XXXXX

!-------------------------------------------------------------------------
!ESMF_MULTI_PROC_SYSTEM_TEST        String used by test script to count system tests.
!=========================================================================

!BOP
!
! !DESCRIPTION:
! System test FieldBundleRedistBlk2Blk.
!
! This system test checks the functionality of the grid distribution
! routines by redistributing data from one FieldBundle distributed in the normal
! block structure to another FieldBundle that has been distributed regularly
! and then back again.  The original data should exactly match the final
! data, which serves as the test for SUCCESS.  This program creates two
! identical Grids with different distributions, one with the normal block
! structure and the other with a different block distribution.  The first
! Grid has two FieldBundle created from it, the first as the source for the test
! and the second for the final results.  The second Grid has a single FieldBundle
! that serves as an intermediate result between the two redistributions.
!
!\begin{verbatim}

program Blk2BlkBunRedist

#include "ESMF_Macros.inc"
#include "ESMF_Conf.inc"
#include "ESMF.h"
#define ESMF_METHOD "FieldBundleRedistBlk2BlkSTest"

    ! ESMF Framework module
    use ESMF_Mod
    use ESMF_TestMod
    
    implicit none
    
    ! Local variables
    integer :: i, j, rc, localrc
    integer :: npets, localPet
    integer :: miscount
    integer, dimension(2) :: counts
    logical :: match
    real(ESMF_KIND_R8) :: pi, compval
    integer, dimension(1) :: gclbx, gcubx, gclby, gcuby
    integer, dimension(2) :: src_fclb, src_fcub, res_fclb, res_fcub
    real(ESMF_KIND_R8), dimension(:), pointer :: coordX, coordY
    real(ESMF_KIND_R8), dimension(:,:), pointer :: srcdata, resdata
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_Grid)  ::  grid1,  grid2
    type(ESMF_Field) :: field1, field2, field3
    type(ESMF_FieldBundle) :: fieldbundle1, fieldbundle2, fieldbundle3
    type(ESMF_RouteHandle) :: rh12, rh23
    type(ESMF_VM):: vm

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0

    ! individual test name
    character(ESMF_MAXSTR) :: testname

    ! individual test failure message and final rc msg
    character(ESMF_MAXSTR) :: failMsg, finalMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(testname, *) "System Test FieldBundleRedistBlk2Blk"
  write(failMsg, *) "System Test failure"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  print *, "--------------------------------------- "
  print *, "Start of ", trim(testname)
  print *, "--------------------------------------- "

!-------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    localrc = ESMF_SUCCESS
    
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
    ! Initialize the framework and get back the default global VM
    call ESMF_Initialize(vm=vm, defaultlogfilename="FieldBundleRedistBlk2BlkSTest.Log", &
                        defaultlogtype=ESMF_LOG_MULTI, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    ! Get the PET count and our PET number
    call ESMF_VMGet(vm, localPet=localPet, petCount=npets, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    miscount = 0

    if (2*(npets/2) .ne. npets) then
        print *, "This test must run on an even number of processes"
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
    pi              = 3.14159
    counts(1)       = 60
    counts(2)       = 50
    ! create arrayspec
    call ESMF_ArraySpecSet(arrayspec, rank=2, &
                           typekind=ESMF_TYPEKIND_R8, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
    
    ! create src and dst grids
    grid1 = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=counts, &
        coordDep1=(/1/), coordDep2=(/2/), &
        regDecomp=(/2, npets/2/), rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    grid2 = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=counts, &
        coordDep1=(/1/), coordDep2=(/2/), &
        regDecomp=(/npets/2, 2/), rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    call ESMF_GridAddCoord(grid1, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    call ESMF_GridAddCoord(grid2, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    ! create src and dst fields
    field1 = ESMF_FieldCreate(grid1, arrayspec, maxHaloLWidth=(/2,2/), &
        maxHaloUWidth=(/0,0/), rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    field3 = ESMF_FieldCreate(grid1, arrayspec, maxHaloLWidth=(/2,2/), &
        maxHaloUWidth=(/0,0/), rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    field2 = ESMF_FieldCreate(grid2, arrayspec, maxHaloLWidth=(/2,2/), &
        maxHaloUWidth=(/0,0/), rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    ! create src and dst fieldbundles
    fieldbundle1 = ESMF_FieldBundleCreate(grid1, name="fieldbundle1", rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
    call ESMF_FieldBundleAdd(fieldbundle1, field1, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    fieldbundle3 = ESMF_FieldBundleCreate(grid1, name="fieldbundle3", rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
    call ESMF_FieldBundleAdd(fieldbundle3, field3, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    fieldbundle2 = ESMF_FieldBundleCreate(grid2, name="fieldbundle2", rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
    call ESMF_FieldBundleAdd(fieldbundle2, field2, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    ! precompute communication patterns
    call ESMF_FieldBundleRedistStore(fieldbundle1, fieldbundle2, routehandle=rh12, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
    call ESMF_FieldBundleRedistStore(fieldbundle2, fieldbundle3, routehandle=rh23, rc=localrc)
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
    call ESMF_GridGetCoord(grid1, localDe=0, coordDim=2, &
        computationalLBound=gclby, computationalUBound=gcuby, &
        fptr=coordY, rc=localrc)
        !fptr=coordY, totalCount=localCounts, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
    
    ! Get pointers to the data and set it up
    call ESMF_FieldGet(field1, localDe=0, farrayPtr=srcdata, &
        computationalLBound=src_fclb, computationalUBound=src_fcub, & 
        rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
    call ESMF_FieldGet(field3, localDe=0, farrayPtr=resdata, &
        computationalLBound=res_fclb, computationalUBound=res_fcub, & 
        rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    ! initialize data arrays
    srcdata = 0.0
    resdata = 0.0

    ! set data array to a function of coordinates (in the computational part
    ! of the array only, not the halo region)
    do i = gclbx(1), gcubx(1)
        coordX(i) = i*0.5
    enddo
    do i = gclby(1), gcuby(1)
        coordY(i) = i*0.5
    enddo

    do j    = gclby(1), gcuby(1)
       do i = gclbx(1), gcubx(1)
           srcdata(i,j) = 10.0 + 5.0*sin(coordX(i)/60.0*pi) &
                                             + 2.0*sin(coordY(j)/50.0*pi) 
      enddo
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
    call ESMF_FieldBundleRedist(fieldbundle1, fieldbundle2, rh12, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    print *, "Array contents after Transpose:"

    !! Transpose back so we can compare contents
    !! Call transpose method again here, output ends up in field3
    call ESMF_FieldBundleRedist(fieldbundle2, fieldbundle3, rh23, rc=localrc)
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
    print *, "Result from PET number ", localPet
    print *, "-----------------------------------------------------------------"
    print *, "-----------------------------------------------------------------"

    match    = .true.
    miscount = 0
    do j   = gclby(1), gcuby(1)
      do i = gclbx(1), gcubx(1)
        compval = 10.0 + 5.0*sin(coordX(i)/60.0*pi) &
                       + 2.0*sin(coordY(j)/50.0*pi)
        print *, srcdata(i,j), resdata(i,j), compval
        if ((srcdata(i,j) .ne. resdata(i,j)) .OR. &
            (abs(resdata(i,j)-compval).ge.1.0d-12)) then
          print *, "array contents do not match at: (", i,j, ") on PET ", &
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

    call ESMF_FieldBundleRedistRelease(rh12, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    call ESMF_FieldBundleRedistRelease(rh23, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    call ESMF_FieldBundleDestroy(fieldbundle1, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    call ESMF_FieldBundleDestroy(fieldbundle2, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    call ESMF_FieldBundleDestroy(fieldbundle3, rc=localrc)
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

    print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

20 continue

  ! Normal ESMF Test output
  print *, testname, " complete."

  if (rc .eq. ESMF_SUCCESS) then
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
  call ESMF_STest((rc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)
  
  call ESMF_Finalize(rc=rc)

end program Blk2BlkBunRedist
    
!\end{verbatim}
