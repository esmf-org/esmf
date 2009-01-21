! $Id: ESMF_FieldBundleCommOptionsUTest.F90,v 1.1.2.6 2009/01/21 21:25:21 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_FieldBundleCommOptionsUTest

!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!
!=============================================================================
!BOP
! !PROGRAM: ESMF_FieldBundleCommOptionsUTest - Test FieldBundle Route Run-time Options
!
! !DESCRIPTION:
!
! TODO: this test currently works at the Field level; once the FieldBundle
!  communications code is complete, this test needs to be revised to create
!  a loose FieldBundle of Fields with different data values, and redist them
!  into a second FieldBundle of Fields which are decomposed differently.
!
! The code in this file exercises various combinations of internal options
!  controlling how to do the actual redist communications - e.g. pack 
!  small discontig chunks into a buffer and do a single send, or make
!  multiple communication calls.  In general we expect that the framework
!  will have reasonable defaults and users will not need to specify the
!  internal methods, but the interfaces are here in order to allow the
!  defaults to be overridden in special cases.
!
!-----------------------------------------------------------------------------
! !USES:
    use ESMF_Mod
    use ESMF_TestMod    ! test methods
    implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
      '$Id: ESMF_FieldBundleCommOptionsUTest.F90,v 1.1.2.6 2009/01/21 21:25:21 cdeluca Exp $'
!------------------------------------------------------------------------------

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0

    ! individual test result code
    integer :: rc

    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name

    ! Local variables
    type(ESMF_VM):: vm
    type(ESMF_Field) :: humidity1, humidity2
    type(ESMF_RouteHandle) :: routehandle
    type(ESMF_RouteOptions) :: roption

    real(ESMF_KIND_R8) :: maxerror

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
    ! test program code starts here

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

!-------------------------------------------------------------------------
!   ! create the two fields which are going to be redistributed
!   ! and fill the first with known data values
!-------------------------------------------------------------------------

    call ESMF_VMGetGlobal(vm, rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    !------------------------------------------------------------------------
    !NEX_removeUTest
    call createField1(humidity1, vm, result, rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating Field 1 for testing"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10

    !------------------------------------------------------------------------
    !NEX_removeUTest
    call createField2(humidity2, vm, result, rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating Field 2 for testing"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10

    !------------------------------------------------------------------------
    !NEX_removeUTest
    ! set initial values
    call initializeData(humidity1, result, rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Initializing data for Field 1"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10

    !------------------------------------------------------------------------
    !NEX_removeUTest
    ! this is to test the verify routine - this is calling the verification
    ! on the original data; if this fails, there is something wrong with it
    ! before we even start the redist process.
    call verifyResults(humidity1, maxerror, result, rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Verifying data for Field 1"
    call ESMF_Test((maxerror .le. 0.01), name, failMsg, result, ESMF_SRCLINE)
    if (maxerror .ge. 0.01) goto 10

!-------------------------------------------------------------------------
!   ! precompute the communication pattern
!-------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_removeUTest
    call ESMF_FieldRedistStore(humidity1, humidity2, vm, &
                               routehandle=routehandle, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Precomputing the Redist Store"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10

    ! debug, see what route was set up.
    call ESMF_RouteHandlePrint(routehandle, rc=rc)

!-------------------------------------------------------------------------
!   ! execute the redist
!-------------------------------------------------------------------------

    ! These are fields on the same IGrid but different distributions.  
    !  call Redist to rearrange the data.   The communication pattern 
    !  was computed at init, this simply has to execute the send and 
    !  receive equivalents.

    !------------------------------------------------------------------------
    !NEX_removeUTest
    call ESMF_FieldRedist(humidity1, humidity2, routehandle, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Executing the Redist"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10

!-------------------------------------------------------------------------
!   ! validate the results
!-------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_removeUTest
    call verifyResults(humidity2, maxerror, result, rc)
    write(failMsg, *) "Percentage error in data result is too large"
    write(name, *) "Redist percentage error less than 0.1%"
    call ESMF_Test((maxerror .lt. 0.01), name, failMsg, result, ESMF_SRCLINE)
    if (maxerror .ge. 0.01) goto 10

    !------------------------------------------------------------------------
    !NEX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Redist returned ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10

#ifdef ESMF_TESTEXHAUSTIVE

!-------------------------------------------------------------------------
!   ! for exhaustive test, try various internal route strategies.
!   ! this section, alternate between executing a redist and validating
!   ! the results.
!-------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------
    !EX_removeUTest
    roption = ESMF_ROUTE_OPTION_SYNC + ESMF_ROUTE_OPTION_PACK_PET
    call ESMF_FieldRedist(humidity1, humidity2, routehandle, &
                          routeOptions=roption, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Executing the Redist with Sync/Pack_PET option"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10

    !------------------------------------------------------------------------
    !EX_removeUTest
    call verifyResults(humidity2, maxerror, result, rc)
    write(failMsg, *) "Percentage error in data result is too large"
    write(name, *) "Redist percentage error less than 2%"
    call ESMF_Test((maxerror .lt. 2.0), name, failMsg, result, ESMF_SRCLINE)
    if (maxerror .ge. 2.0) goto 10

    !------------------------------------------------------------------------
    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Redist returned ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10

    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------
    !EX_removeUTest
    roption = ESMF_ROUTE_OPTION_SYNC + ESMF_ROUTE_OPTION_PACK_XP
    call ESMF_FieldRedist(humidity1, humidity2, routehandle, &
                          routeOptions=roption, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Executing the Redist with Sync/Pack_XP option"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10

    !------------------------------------------------------------------------
    !EX_removeUTest
    call verifyResults(humidity2, maxerror, result, rc)
    write(failMsg, *) "Percentage error in data result is too large"
    write(name, *) "Redist percentage error less than 2%"
    call ESMF_Test((maxerror .lt. 2.0), name, failMsg, result, ESMF_SRCLINE)
    if (maxerror .ge. 2.0) goto 10

    !------------------------------------------------------------------------
    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Redist returned ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10

    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------
    !EX_removeUTest
    roption = ESMF_ROUTE_OPTION_SYNC + ESMF_ROUTE_OPTION_PACK_NOPACK
    call ESMF_FieldRedist(humidity1, humidity2, routehandle, &
                          routeOptions=roption, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Executing the Redist with Sync/Nopack option"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10

    !------------------------------------------------------------------------
    !EX_removeUTest
    call verifyResults(humidity2, maxerror, result, rc)
    write(failMsg, *) "Percentage error in data result is too large"
    write(name, *) "Redist percentage error less than 2%"
    call ESMF_Test((maxerror .lt. 2.0), name, failMsg, result, ESMF_SRCLINE)
    if (maxerror .ge. 2.0) goto 10

    !------------------------------------------------------------------------
    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Redist returned ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10

    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------
    !EX_removeUTest
    roption = ESMF_ROUTE_OPTION_ASYNC + ESMF_ROUTE_OPTION_PACK_PET
    call ESMF_FieldRedist(humidity1, humidity2, routehandle, &
                          routeOptions=roption, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Executing the Redist with Async/Pack_PET option"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10

    !------------------------------------------------------------------------
    !EX_removeUTest
    call verifyResults(humidity2, maxerror, result, rc)
    write(failMsg, *) "Percentage error in data result is too large"
    write(name, *) "Redist percentage error less than 2%"
    call ESMF_Test((maxerror .lt. 2.0), name, failMsg, result, ESMF_SRCLINE)
    if (maxerror .ge. 2.0) goto 10

    !------------------------------------------------------------------------
    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Redist returned ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10

    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------
    !EX_removeUTest
    roption = ESMF_ROUTE_OPTION_ASYNC + ESMF_ROUTE_OPTION_PACK_XP
    call ESMF_FieldRedist(humidity1, humidity2, routehandle, &
                          routeOptions=roption, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Executing the Redist with Async/Pack_XP option"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10

    !------------------------------------------------------------------------
    !EX_removeUTest
    call verifyResults(humidity2, maxerror, result, rc)
    write(failMsg, *) "Percentage error in data result is too large"
    write(name, *) "Redist percentage error less than 2%"
    call ESMF_Test((maxerror .lt. 2.0), name, failMsg, result, ESMF_SRCLINE)
    if (maxerror .ge. 2.0) goto 10

    !------------------------------------------------------------------------
    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Redist returned ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10

    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------
    !EX_removeUTest
    roption = ESMF_ROUTE_OPTION_ASYNC + ESMF_ROUTE_OPTION_PACK_NOPACK
    call ESMF_FieldRedist(humidity1, humidity2, routehandle, &
                          routeOptions=roption, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Executing the Redist with Async/Nopack option"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10

    !------------------------------------------------------------------------
    !EX_removeUTest
    call verifyResults(humidity2, maxerror, result, rc)
    write(failMsg, *) "Percentage error in data result is too large"
    write(name, *) "Redist percentage error less than 2%"
    call ESMF_Test((maxerror .lt. 2.0), name, failMsg, result, ESMF_SRCLINE)
    if (maxerror .ge. 2.0) goto 10

    !------------------------------------------------------------------------
    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Redist returned ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10

    !------------------------------------------------------------------------

#endif

!-------------------------------------------------------------------------
!   ! Release resources stored for the Redist.
!-------------------------------------------------------------------------
   
    !------------------------------------------------------------------------
    !NEX_removeUTest
    call ESMF_FieldRedistRelease(routehandle, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Releasing the routehandle"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10
   
10 continue

    call ESMF_TestEnd(result, ESMF_SRCLINE)


contains

!-------------------------------------------------------------------------
!--------------------------------------------------------------------------
!   ! Make a Field where we specify the decomposition; the number of cells
!   ! per processor is passed into the IGridDistribute call.

#undef  ESMF_METHOD
#define ESMF_METHOD "createField1"

    subroutine createField1(userfield, vm, result, rc)
      type(ESMF_Field), intent(out) :: userfield
      type(ESMF_VM), intent(in) :: vm
      integer, intent(inout) :: result
      integer, intent(out) :: rc

      ! Local variables
      integer :: status
      type(ESMF_DELayout) :: delayout
      type(ESMF_ArraySpec) :: arrayspec
      type(ESMF_IGrid) :: igrid
      integer :: npets, countsPerDE1(8), countsPerDE2(2)
      integer :: nDE1, nDE2, pet_id
      integer :: counts(3), order(3)
      real(ESMF_KIND_R8) :: mincoord(2), maxcoord(2)
      logical :: do2d
      real(ESMF_KIND_R8), pointer :: idata3(:,:,:)
      real(ESMF_KIND_R8), pointer :: idata2(:,:)
      type(ESMF_IGridHorzStagger) :: horz_stagger
      type(ESMF_FieldDataMap) :: datamap

      ! change this to make a 3d data field
      do2d = .TRUE.

      call ESMF_VMGet(vm, petCount=npets, localPET=pet_id, rc=status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10

      ! Create a field in which we determine the number of items 
      ! in each of the local decompositions.
      if (npets .eq. 1) then
        nDE1 = 1
        nDE2 = 1
        countsPerDE1(1:1) = (/ 60 /)
        countsPerDE2(1:1) = (/ 40 /)
      else
        nDE1 = npets/2
        nDE2 = 2
 
        if (nDE1 .eq. 2) then
          countsPerDE1(1:nDE1) = (/ 28, 32 /)
        else if (nDE1 .eq. 3) then
          countsPerDE1(1:nDE1) = (/ 10, 38, 12 /)
        else if (nDE1 .ge. 4) then
          nDE1 = 4
          countsPerDE1(1:nDE1) = (/ 10, 34, 10, 6 /)
        endif
        
        countsPerDE2 = (/ 12, 28 /)
      endif

      delayout = ESMF_DELayoutCreate(vm, (/ nDE1, nDE2 /), rc=status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10

      counts(1:3)  = (/  4, 60, 40 /)
      mincoord(1) = 0.0
      maxcoord(1) = 60.0
      mincoord(2) = 0.0
      maxcoord(2) = 50.0

      horz_stagger = ESMF_IGRID_HORZ_STAGGER_A

      igrid = ESMF_IGridCreateHorzXYUni(counts=counts(2:3), &
                                      minGlobalCoordPerDim=mincoord, &
                                      maxGlobalCoordPerDim=maxcoord, &
                                      horzStagger=horz_stagger, &
                                      name="source igrid", rc=status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10

      call ESMF_IGridDistribute(igrid, delayout=delayout, &
                               countsPerDEDim1=countsPerDE1(1:nDE1), &
                               countsPerDEDim2=countsPerDE2(1:nDE2), &
                               rc=status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10

      ! Set up either a 2D or 3D real array
      if (do2d) then
          call ESMF_ArraySpecSet(arrayspec, rank=2, typekind=ESMF_TYPEKIND_R8)
      else
          call ESMF_ArraySpecSet(arrayspec, rank=3, typekind=ESMF_TYPEKIND_R8)
      endif
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10

      ! Set up a datamap to tell the framework which of the 2 axes
      ! correspond to the igrid, and which one is multiple scalar
      ! values for the same igrid cell.
      order(1) = 0
      order(2) = 1
      order(3) = 2
      if (do2d) then
          call ESMF_FieldDataMapSetDefault(datamap, 2, order(2:3), rc=status)
      else
          call ESMF_FieldDataMapSetDefault(datamap, 3, order, &
                                           counts=counts(1:1), rc=status)
      endif
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10

      ! Create the field 
      userfield = ESMF_FieldCreate(igrid, arrayspec=arrayspec, datamap=datamap, &
                                  haloWidth=0, name="userfield", rc=status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10
  
      ! Get the allocated array back as an F90 array pointer
      if (do2d) then
          call ESMF_FieldGetDataPointer(userfield, idata2, ESMF_DATA_REF, rc=status)
      else
          call ESMF_FieldGetDataPointer(userfield, idata3, ESMF_DATA_REF, rc=status)
      endif
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10

      ! Set initial data values over whole array to bad value
      if (do2d) idata2 = -9999
      if (.not. do2d) idata3 = -9999

      status = ESMF_SUCCESS

   ! code comes directly here on error
10 continue

       ! set return code
       rc = status

    end subroutine createField1

!-------------------------------------------------------------------------
!--------------------------------------------------------------------------
!   ! Make a Field letting ESMF do the decomposition; it computes the number
!   ! of cells on each local processor based on the shape of the delayout.

#undef  ESMF_METHOD
#define ESMF_METHOD "createField2"

    subroutine createField2(userfield, vm, result, rc)
      type(ESMF_Field), intent(out) :: userfield
      type(ESMF_VM), intent(in) :: vm
      integer, intent(inout) :: result
      integer, intent(out) :: rc

      ! Local variables
      integer :: status
      type(ESMF_DELayout) :: delayout
      type(ESMF_ArraySpec) :: arrayspec
      type(ESMF_IGrid) :: igrid
      integer :: npets, decount(2)
      integer :: counts(ESMF_MAXIGRIDDIM), order(ESMF_MAXDIM)
      real(ESMF_KIND_R8) :: mincoord(2), maxcoord(2)
      logical :: do2d
      real(ESMF_KIND_R8), pointer :: idata2(:,:)
      real(ESMF_KIND_R8), pointer :: idata3(:,:,:)
      type(ESMF_IGridHorzStagger) :: horz_stagger
      type(ESMF_FieldDataMap) :: datamap

      ! change this to create 3d data
      do2d = .TRUE.

      ! compute which axes we want decomposed 
      call ESMF_VMGet(vm, petCount=npets, rc=status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10

      if (npets .eq. 1) then
        decount(:) = (/ 1, 1 /)
      else
        decount(:) = (/ 2, npets/2 /)
      endif
      delayout = ESMF_DELayoutCreate(vm, decount, rc=status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10


      ! Set up the igrid size:  60 by 40 cells, 4 data values per cell with
      ! halo width of 3.
      counts(1) = 4
      counts(2) = 60
      counts(3) = 40

      ! Set up the max and min coordinates
      mincoord(1) = 0.0
      maxcoord(1) = 60.0
      mincoord(2) = 0.0
      maxcoord(2) = 50.0

      ! Specify the places on each cell at which data might be located
      horz_stagger = ESMF_IGRID_HORZ_STAGGER_A

      igrid = ESMF_IGridCreateHorzXYUni(counts=counts(2:3), &
                                      minGlobalCoordPerDim=mincoord, &
                                      maxGlobalCoordPerDim=maxcoord, &
                                      horzStagger=horz_stagger, &
                                      name="source igrid", rc=status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10

      call ESMF_IGridDistribute(igrid, delayout=delayout, rc=status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10

      ! Set up a 2d or 3D real array
      if (do2d) then
          call ESMF_ArraySpecSet(arrayspec, rank=2, typekind=ESMF_TYPEKIND_R8)
      else
          call ESMF_ArraySpecSet(arrayspec, rank=3, typekind=ESMF_TYPEKIND_R8)
      endif
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10

      ! Set up a datamap to tell the framework which of the 2 axes
      ! correspond to the igrid, and which one is multiple scalar
      ! values for the same igrid cell.
      order(1) = 0
      order(2) = 1
      order(3) = 2
      if (do2d) then
          call ESMF_FieldDataMapSetDefault(datamap, 2, order(2:3), rc=status)
      else
          call ESMF_FieldDataMapSetDefault(datamap, 3, order, &
                                           counts=counts(1:1), rc=status)
      endif
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10

      ! Create the field 
      userfield = ESMF_FieldCreate(igrid, arrayspec=arrayspec, datamap=datamap, &
                                  horzRelloc=ESMF_CELL_CENTER, &
                                  haloWidth=0, name="userfield", rc=status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10

      ! Get the allocated array back as an F90 array pointer
      if (do2d) then
        call ESMF_FieldGetDataPointer(userfield, idata2, ESMF_DATA_REF, rc=status)
      else
        call ESMF_FieldGetDataPointer(userfield, idata3, ESMF_DATA_REF, rc=status)
      endif
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10

      status = ESMF_SUCCESS

      ! Set initial data values over whole array to -1
      if (do2d) idata2 = -2.0
      if (.not.do2d) idata3 = -2.0

   ! code comes directly here on error
10 continue
  
      ! set return code
      rc = status
      
    end subroutine createField2

!-------------------------------------------------------------------------
!--------------------------------------------------------------------------
!   ! Make a Field where we specify the decomposition; the number of cells
!   ! per processor is passed into the IGridDistribute call.

#undef  ESMF_METHOD
#define ESMF_METHOD "createField3"

    subroutine createField3(userfield, vm, result, rc)
      type(ESMF_Field), intent(out) :: userfield
      type(ESMF_VM), intent(in) :: vm
      integer, intent(inout) :: result
      integer, intent(out) :: rc

      ! Local variables
      integer :: status
      type(ESMF_DELayout) :: delayout
      type(ESMF_ArraySpec) :: arrayspec
      type(ESMF_IGrid) :: igrid
      integer :: npets, countsPerDE1(8), countsPerDE2(2)
      integer :: nDE1, nDE2, pet_id
      integer :: counts(3), order(3)
      real(ESMF_KIND_R8) :: mincoord(2), maxcoord(2)
      real(ESMF_KIND_R8), pointer :: idata(:,:,:)
      type(ESMF_IGridHorzStagger) :: horz_stagger
      type(ESMF_FieldDataMap) :: datamap


      call ESMF_VMGet(vm, petCount=npets, localPET=pet_id, rc=status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10

      ! Create a field in which we determine the number of items 
      ! in each of the local decompositions.
      if (npets .eq. 1) then
        nDE1 = 1
        nDE2 = 1
        countsPerDE1(1:1) = (/ 60 /)
        countsPerDE2(1:1) = (/ 40 /)
      else
        nDE1 = npets/2
        nDE2 = 2
 
        if (nDE1 .eq. 2) then
          countsPerDE1(1:nDE1) = (/ 28, 32 /)
        else if (nDE1 .eq. 3) then
          countsPerDE1(1:nDE1) = (/ 10, 38, 12 /)
        else if (nDE1 .ge. 4) then
          nDE1 = 4
          countsPerDE1(1:nDE1) = (/ 10, 34, 10, 6 /)
        endif
        
        countsPerDE2 = (/ 12, 28 /)
      endif

      delayout = ESMF_DELayoutCreate(vm, (/ nDE1, nDE2 /), rc=status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10

      counts(1:3)  = (/  4, 60, 40 /)
      mincoord(1) = 0.0
      maxcoord(1) = 60.0
      mincoord(2) = 0.0
      maxcoord(2) = 50.0

      horz_stagger = ESMF_IGRID_HORZ_STAGGER_A

      igrid = ESMF_IGridCreateHorzXYUni(counts=counts(2:3), &
                                      minGlobalCoordPerDim=mincoord, &
                                      maxGlobalCoordPerDim=maxcoord, &
                                      horzStagger=horz_stagger, &
                                      name="source igrid", rc=status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10

      call ESMF_IGridDistribute(igrid, delayout=delayout, &
                               countsPerDEDim1=countsPerDE1(1:nDE1), &
                               countsPerDEDim2=countsPerDE2(1:nDE2), &
                               rc=status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10

      ! Set up a 3D real array
      call ESMF_ArraySpecSet(arrayspec, rank=3, typekind=ESMF_TYPEKIND_R8)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10

      ! Set up a datamap to tell the framework which of the 2 axes
      ! correspond to the igrid, and which one is multiple scalar
      ! values for the same igrid cell.
      order(1) = 0
      order(2) = 1
      order(3) = 2
      call ESMF_FieldDataMapSetDefault(datamap, 3, order, &
                                       counts=counts(1:1), rc=status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10

      ! Create the field 
      userfield = ESMF_FieldCreate(igrid, arrayspec=arrayspec, datamap=datamap, &
                                  haloWidth=0, name="userfield", rc=status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10
  
      ! Get the allocated array back as an F90 array pointer
      call ESMF_FieldGetDataPointer(userfield, idata, ESMF_DATA_REF, rc=status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10

      ! Set initial data values over whole array to bad value
      idata(:,:,:) = -9999

      status = ESMF_SUCCESS

   ! code comes directly here on error
10 continue

       ! set return code
       rc = status

    end subroutine createField3

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   ! Fill the data arrays with known values so after the redist operation
!   ! we can verify the resulting data values are correct.

#undef  ESMF_METHOD
#define ESMF_METHOD "initializeData"

    subroutine initializeData(userfield, result, rc)
      type(ESMF_Field), intent(inout) :: userfield
      integer, intent(inout) :: result
      integer, intent(out) :: rc

      ! Local variables
      integer :: i, j, k, status
      type(ESMF_IGrid) :: igrid
      integer :: counts(ESMF_MAXIGRIDDIM), ranksize
      real(ESMF_KIND_R8), pointer :: coordX(:,:), coordY(:,:)
      real(ESMF_KIND_R8), pointer :: idata2(:,:), idata3(:,:,:)
      type(ESMF_RelLoc) :: relloc
      real(ESMF_KIND_R8) :: pi = 3.14159


      ! get the igrid and coordinates
      call ESMF_FieldGet(userfield, igrid=igrid, horzRelloc=relloc, &
                         rank=ranksize, rc=status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10

      call ESMF_IGridGetCoord(igrid, dim=1, horzRelloc=relloc, &
                             centerCoord=coordX, rc=status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10

      call ESMF_IGridGetCoord(igrid, dim=2, horzRelloc=relloc, &
                             centerCoord=coordy, rc=status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10

      ! get a pointer to the start of the local data block
      if (ranksize .eq. 2) then
          call ESMF_FieldGetDataPointer(userfield, idata2, ESMF_DATA_REF, &
                                        counts=counts, rc=status) 
      else if (ranksize .eq. 3) then
          call ESMF_FieldGetDataPointer(userfield, idata3, ESMF_DATA_REF, &
                                        counts=counts, rc=status) 
      else
         print *, "Unexpected rank", ranksize
         status = ESMF_FAILURE
      endif
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10

      print *, "counts =", counts
      ! set the initial data values
      if (ranksize .eq. 3) then
        do j = 1,counts(3)
          do i = 1,counts(2)
            do k = 1,counts(1)
               idata3(k,i,j) = 10.0 + 5.0*sin(coordX(i,j)/60.0*pi) &
                                   + 2.0*sin(coordY(i,j)/50.0*pi)
               !print *, "i=", i, "j=", j, "k=", k, "data=", idata3(k,i,j)
            enddo
          enddo
        enddo
      else
        do j = 1,counts(2)
          do i = 1,counts(1)
            idata2(i,j) = 10.0 + 5.0*sin(coordX(i,j)/60.0*pi) &
                                + 2.0*sin(coordY(i,j)/50.0*pi)
            !print *, "i=", i, "j=", j, "data=", idata2(i,j)
           enddo
        enddo
      endif

      status = ESMF_SUCCESS

   ! code comes directly here on error
10 continue

      rc = status

    end subroutine initializeData


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   ! Validate that the resulting data has the values we expect to be there.
 
#undef  ESMF_METHOD
#define ESMF_METHOD "verifyResults"

    subroutine verifyResults(userfield, returnedError, result, rc)
      type(ESMF_Field), intent(inout) :: userfield
      real(ESMF_KIND_R8), intent(out) :: returnedError
      integer, intent(inout) :: result
      integer, intent(out) :: rc

      ! Local variables
      integer :: status, i, j, k, myDE, counts(2), acounts(3), ranksize
      type(ESMF_RelLoc) :: relloc
      type(ESMF_IGrid) :: igrid
      real(ESMF_KIND_R8) :: pi = 3.14159
      real(ESMF_KIND_R8) :: error, maxError, maxPerError
      real(ESMF_KIND_R8) :: minCValue, maxCValue, minDValue, maxDValue
      real(ESMF_KIND_R8), dimension(:,:), pointer :: calc, coordX, coordY
      real(ESMF_KIND_R8), dimension(:,:), pointer :: data2
      real(ESMF_KIND_R8), dimension(:,:,:), pointer :: data3


      call ESMF_FieldGet(userfield, igrid=igrid, horzRelloc=relloc, &
                         rank=ranksize, rc=status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10

      call ESMF_IGridGetDELocalInfo(igrid, myDE=myDE, &
                                   localCellCountPerDim=counts, &
                                   horzRelloc=ESMF_CELL_CENTER, rc=status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10
      call ESMF_IGridGetCoord(igrid, dim=1, horzRelloc=relloc, &
                             centerCoord=coordX, rc=status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10
      call ESMF_IGridGetCoord(igrid, dim=2, horzRelloc=relloc, &
                             centerCoord=coordY, rc=status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10


      ! Get a pointer to the start of the data
      if (ranksize .eq. 2) then
        call ESMF_FieldGetDataPointer(userfield, data2, ESMF_DATA_REF, &
                                      counts=acounts, rc=status)
      else if (ranksize .eq. 3) then
        call ESMF_FieldGetDataPointer(userfield, data3, ESMF_DATA_REF, &
                                      counts=acounts, rc=status)
      else
        print *, "unexpected rank", ranksize
        status = ESMF_FAILURE
      endif
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10

      print *, "counts = ", counts
      print *, "acounts = ", acounts

      ! allocate array for computed results and fill it
      allocate(calc(counts(1), counts(2)))
      do j   = 1,counts(2)
        do i = 1,counts(1)
          calc(i,j) = 10.0 + 5.0*sin(coordX(i,j)/60.0*pi) &
                           + 2.0*sin(coordY(i,j)/50.0*pi)
          !print *, "i=", i, "j=", j, "calc=", calc(i,j)
        enddo
      enddo


      ! calculate data error from computed results
      maxError    = -99999.0
      maxPerError = 0.0
      maxCValue   = -99999.0
      minCValue   =  99999.0
      maxDValue   = -99999.0
      minDValue   =  99999.0
      if (ranksize .eq. 3) then
        do k     = 1,size(data3,1)
          do j   = 1,counts(2)
            do i = 1,counts(1)
              error       = abs(data3(k,i,j) - calc(i,j))
              minCValue   = min(minCValue, calc(i,j))
              maxCValue   = max(maxCValue, calc(i,j))
              minDValue   = min(minDValue, data3(k,i,j))
              maxDValue   = max(maxDValue, data3(k,i,j))
              maxError    = max(maxError, abs(error))
              maxPerError = max(maxPerError, 100.*abs(error)/abs(calc(i,j)))
            enddo
          enddo
        enddo
      else
        do j   = 1,counts(2)
          do i = 1,counts(1)
            error       = abs(data2(i,j) - calc(i,j))
            minCValue   = min(minCValue, calc(i,j))
            maxCValue   = max(maxCValue, calc(i,j))
            minDValue   = min(minDValue, data2(i,j))
            maxDValue   = max(maxDValue, data2(i,j))
            maxError    = max(maxError, abs(error))
            maxPerError = max(maxPerError, 100.*abs(error)/abs(calc(i,j)))
          enddo
        enddo
      endif

      write(*,*) " "
      write(*,*) "Detailed results for DE #", myDE, ":"
      write(*,*) "   minimum redisted value = ", minDValue
      write(*,*) "   maximum redisted value = ", maxDValue
      write(*,*) "   minimum computed value  = ", minCValue
      write(*,*) "   maximum computed value  = ", maxCValue
      write(*,*) "   maximum error           = ", maxError
      write(*,*) "   maximum percent error   = ", maxPerError
   
      ! set the return value
      returnedError = maxPerError

      if (maxPerError .ge. 2.0) then
          status = ESMF_FAILURE
      else
          status = ESMF_SUCCESS
      endif

      deallocate(calc)

   ! code comes directly here on error
10 continue

      ! set return code
      rc = status

    end subroutine verifyResults


!-------------------------------------------------------------------------

end program ESMF_FieldBundleCommOptionsUTest
