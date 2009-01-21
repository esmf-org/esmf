! $Id: ESMF_RegridOptionsUTest.F90,v 1.19.2.4 2009/01/21 21:25:23 cdeluca Exp $
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
      program ESMF_RegridOptionsUTest

!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!
!=============================================================================
!BOP
! !PROGRAM: ESMF_RegridOptionsUTest - Test Regrid Run-time Options
!
! !DESCRIPTION:
!
! The code in this file exercises various combinations of internal options
!  controlling how to do the actual regrid communications - e.g. pack 
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
      '$Id: ESMF_RegridOptionsUTest.F90,v 1.19.2.4 2009/01/21 21:25:23 cdeluca Exp $'
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
!   ! create the two fields which are going to be regridded
!   ! and fill the first with known data values
!-------------------------------------------------------------------------

    call ESMF_VMGetGlobal(vm, rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    call createField1(humidity1, vm, result, rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    call createField2(humidity2, vm, result, rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    call initializeData(humidity1, result, rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

!-------------------------------------------------------------------------
!   ! precompute the communication pattern
!-------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_removeUTest
    call ESMF_FieldRegridStore(humidity1, humidity2, vm, routehandle, &
                               regridmethod=ESMF_REGRID_METHOD_BILINEAR, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Precomputing the Regrid Store"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10

!-------------------------------------------------------------------------
!   ! execute the regrid
!-------------------------------------------------------------------------

    ! These are fields on different IGrids - call Regrid to rearrange
    !  the data.   The communication pattern was computed at init,
    !  this simply has to execute the send and receive equivalents.

    !------------------------------------------------------------------------
    !NEX_removeUTest
    call ESMF_FieldRegrid(humidity1, humidity2, routehandle, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Executing the Regrid"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10

!-------------------------------------------------------------------------
!   ! validate the results
!-------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_removeUTest
    call verifyResults(humidity2, maxerror, result, rc)
    write(failMsg, *) "Percentage error in data result is too large"
    write(name, *) "Regrid percentage error less than 2%"
    call ESMF_Test((maxerror .lt. 2.0), name, failMsg, result, ESMF_SRCLINE)
    if (maxerror .ge. 2.0) goto 10

    !------------------------------------------------------------------------
    !NEX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Regrid returned ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10

#ifdef ESMF_TESTEXHAUSTIVE

!-------------------------------------------------------------------------
!   ! for exhaustive test, try various internal route strategies.
!   ! this section, alternate between executing a regrid and validating
!   ! the results.
!-------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------
    !EX_removeUTest
    roption = ESMF_ROUTE_OPTION_SYNC + ESMF_ROUTE_OPTION_PACK_PET
    call ESMF_FieldRegrid(humidity1, humidity2, routehandle, &
                          routeOptions=roption, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Executing the Regrid with Sync/Pack_PET option"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10

    !------------------------------------------------------------------------
    !EX_removeUTest
    call verifyResults(humidity2, maxerror, result, rc)
    write(failMsg, *) "Percentage error in data result is too large"
    write(name, *) "Regrid percentage error less than 2%"
    call ESMF_Test((maxerror .lt. 2.0), name, failMsg, result, ESMF_SRCLINE)
    if (maxerror .ge. 2.0) goto 10

    !------------------------------------------------------------------------
    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Regrid returned ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10

    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------
    !EX_removeUTest
    roption = ESMF_ROUTE_OPTION_SYNC + ESMF_ROUTE_OPTION_PACK_XP
    call ESMF_FieldRegrid(humidity1, humidity2, routehandle, &
                          routeOptions=roption, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Executing the Regrid with Sync/Pack_XP option"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10

    !------------------------------------------------------------------------
    !EX_removeUTest
    call verifyResults(humidity2, maxerror, result, rc)
    write(failMsg, *) "Percentage error in data result is too large"
    write(name, *) "Regrid percentage error less than 2%"
    call ESMF_Test((maxerror .lt. 2.0), name, failMsg, result, ESMF_SRCLINE)
    if (maxerror .ge. 2.0) goto 10

    !------------------------------------------------------------------------
    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Regrid returned ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10

    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------
    !EX_removeUTest
    roption = ESMF_ROUTE_OPTION_SYNC + ESMF_ROUTE_OPTION_PACK_NOPACK
    call ESMF_FieldRegrid(humidity1, humidity2, routehandle, &
                          routeOptions=roption, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Executing the Regrid with Sync/Nopack option"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10

    !------------------------------------------------------------------------
    !EX_removeUTest
    call verifyResults(humidity2, maxerror, result, rc)
    write(failMsg, *) "Percentage error in data result is too large"
    write(name, *) "Regrid percentage error less than 2%"
    call ESMF_Test((maxerror .lt. 2.0), name, failMsg, result, ESMF_SRCLINE)
    if (maxerror .ge. 2.0) goto 10

    !------------------------------------------------------------------------
    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Regrid returned ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10

    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------
    !EX_removeUTest
    roption = ESMF_ROUTE_OPTION_ASYNC + ESMF_ROUTE_OPTION_PACK_PET
    call ESMF_FieldRegrid(humidity1, humidity2, routehandle, &
                          routeOptions=roption, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Executing the Regrid with Async/Pack_PET option"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10

    !------------------------------------------------------------------------
    !EX_removeUTest
    call verifyResults(humidity2, maxerror, result, rc)
    write(failMsg, *) "Percentage error in data result is too large"
    write(name, *) "Regrid percentage error less than 2%"
    call ESMF_Test((maxerror .lt. 2.0), name, failMsg, result, ESMF_SRCLINE)
    if (maxerror .ge. 2.0) goto 10

    !------------------------------------------------------------------------
    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Regrid returned ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10

    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------
    !EX_removeUTest
    roption = ESMF_ROUTE_OPTION_ASYNC + ESMF_ROUTE_OPTION_PACK_XP
    call ESMF_FieldRegrid(humidity1, humidity2, routehandle, &
                          routeOptions=roption, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Executing the Regrid with Async/Pack_XP option"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10

    !------------------------------------------------------------------------
    !EX_removeUTest
    call verifyResults(humidity2, maxerror, result, rc)
    write(failMsg, *) "Percentage error in data result is too large"
    write(name, *) "Regrid percentage error less than 2%"
    call ESMF_Test((maxerror .lt. 2.0), name, failMsg, result, ESMF_SRCLINE)
    if (maxerror .ge. 2.0) goto 10

    !------------------------------------------------------------------------
    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Regrid returned ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10

    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------
    !EX_removeUTest
    roption = ESMF_ROUTE_OPTION_ASYNC + ESMF_ROUTE_OPTION_PACK_NOPACK
    call ESMF_FieldRegrid(humidity1, humidity2, routehandle, &
                          routeOptions=roption, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Executing the Regrid with Async/Nopack option"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10

    !------------------------------------------------------------------------
    !EX_removeUTest
    call verifyResults(humidity2, maxerror, result, rc)
    write(failMsg, *) "Percentage error in data result is too large"
    write(name, *) "Regrid percentage error less than 2%"
    call ESMF_Test((maxerror .lt. 2.0), name, failMsg, result, ESMF_SRCLINE)
    if (maxerror .ge. 2.0) goto 10

    !------------------------------------------------------------------------
    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Regrid returned ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10

    !------------------------------------------------------------------------

#endif

!-------------------------------------------------------------------------
!   ! Release resources stored for the Regridding.
!-------------------------------------------------------------------------
   
    !------------------------------------------------------------------------
    !NEX_removeUTest
    call ESMF_FieldRegridRelease(routehandle, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Releasing the routehandle"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (rc .ne. ESMF_SUCCESS) goto 10
   
10 continue

    call ESMF_TestEnd(result, ESMF_SRCLINE)


contains

!-------------------------------------------------------------------------
!--------------------------------------------------------------------------
!   ! Make a Field letting ESMF do the decomposition; it computes the number
!   ! of cells on each local processor based on the shape of the delayout.

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
      integer :: npets, de_id, decount(2)
      integer :: counts(ESMF_MAXIGRIDDIM), order(ESMF_MAXDIM)
      real(ESMF_KIND_R8) :: mincoord(2), maxcoord(2)
      real(ESMF_KIND_R8), pointer :: idata(:,:,:)
      type(ESMF_IGridHorzStagger) :: horz_stagger
      type(ESMF_FieldDataMap) :: datamap


      ! compute which axes we want decomposed 
      call ESMF_VMGet(vm, petCount=npets, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10

      if (npets .eq. 1) then
        decount(:) = (/ 1, 1 /)
      else
        decount(:) = (/ 2, npets/2 /)
      endif
      delayout = ESMF_DELayoutCreate(vm, decount, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10

      ! and get our local de number
      call ESMF_DELayoutGetDeprecated(delayout, localDE=de_id, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10


      ! Set up the igrid size
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
      if (status .ne. ESMF_SUCCESS) goto 10

      call ESMF_IGridDistribute(igrid, delayout=delayout, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10

      ! Set up a 3D real array
      call ESMF_ArraySpecSet(arrayspec, rank=3, &
                             typekind=ESMF_TYPEKIND_R8)
      if (status .ne. ESMF_SUCCESS) goto 10

      ! Set up a datamap to tell the framework which of the 2 axes
      ! correspond to the igrid, and which one is multiple scalar
      ! values for the same igrid cell.
      order(1) = 0
      order(2) = 1
      order(3) = 2
      call ESMF_FieldDataMapSetDefault(datamap, 3, order, &
                                       counts=counts(1:1), rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10

      ! Create the field 
      userfield = ESMF_FieldCreate(igrid, arrayspec=arrayspec, datamap=datamap, &
                                  horzRelloc=ESMF_CELL_CENTER, &
                                  haloWidth=0, name="userfield", rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10

      ! Get the allocated array back as an F90 array pointer
      call ESMF_FieldGetDataPointer(userfield, idata, ESMF_DATA_REF, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10

      ! Set initial data values over whole array to our de id
      idata(:,:,:) = real(de_id,ESMF_KIND_R8)

      status = ESMF_SUCCESS

   ! code comes directly here on error
10 continue
  
      ! set return code
      rc = status
      
    end subroutine createField1

!-------------------------------------------------------------------------
!--------------------------------------------------------------------------
!   ! Make a Field where we specify the decomposition; the number of cells
!   ! per processor is passed into the IGridDistribute call.

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
      integer :: npets, countsPerDE1(8), countsPerDE2(2)
      integer :: nDE1, nDE2
      integer :: counts(3), order(3)
      real(ESMF_KIND_R8) :: mincoord(2)
      real(ESMF_KIND_R8) :: delta1(40), delta2(50)
      real(ESMF_KIND_R8), pointer :: idata(:,:,:)
      type(ESMF_IGridHorzStagger) :: horz_stagger
      type(ESMF_FieldDataMap) :: datamap


      call ESMF_VMGet(vm, petCount=npets, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10


      ! Create a field in which we determine the number of items 
      ! in each of the local decompositions.
      if (npets .eq. 1) then
        nDE1 = 1
        nDE2 = 1
        countsPerDE1(1:1) = (/ 40 /)
        countsPerDE2(1:1) = (/ 50 /)
      else
        nDE1 = npets/2
        nDE2 = 2
 
        if (nDE1 .eq. 2) then
          countsPerDE1(1:nDE1) = (/ 28, 12 /)
        else if (nDE1 .eq. 3) then
          countsPerDE1(1:nDE1) = (/ 10, 18, 12 /)
        else if (nDE1 .ge. 4) then
          nDE1 = 4
          countsPerDE1(1:nDE1) = (/ 10, 14, 10, 6 /)
        endif
        
        countsPerDE2 = (/ 22, 28 /)
      endif

      delayout = ESMF_DELayoutCreate(vm, (/ nDE1, nDE2 /), rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10

      counts(1:3)  = (/  4, 40, 50 /)
      mincoord(1) = 0.0
      delta1 = (/ 1.0, 1.0, 1.0, 1.1, 1.1, 1.1, 1.2, 1.2, 1.3, 1.4, &
                  1.4, 1.5, 1.6, 1.6, 1.6, 1.8, 1.8, 1.7, 1.7, 1.6, &
                  1.6, 1.6, 1.8, 1.8, 2.0, 2.0, 2.2, 2.2, 2.2, 2.2, &
                  2.0, 1.7, 1.5, 1.3, 1.2, 1.1, 1.0, 1.0, 1.0, 0.9 /)
      mincoord(2) = 0.0
      delta2 = (/ 0.8, 0.8, 0.8, 0.8, 0.8, 0.7, 0.7, 0.6, 0.7, 0.8, &
                  0.9, 0.9, 0.9, 0.9, 1.0, 1.0, 1.0, 1.0, 0.9, 1.0, &
                  1.0, 1.0, 1.0, 1.1, 1.2, 1.3, 1.3, 1.3, 1.4, 1.4, &
                  1.4, 1.4, 1.4, 1.4, 1.4, 1.3, 1.3, 1.3, 1.2, 1.2, &
                  1.1, 1.0, 1.0, 0.9, 0.8, 0.7, 0.6, 0.6, 0.5, 0.5 /)

      horz_stagger = ESMF_IGRID_HORZ_STAGGER_D_NE

      igrid = ESMF_IGridCreateHorzXY(minGlobalCoordPerDim=mincoord, &
                                    delta1=delta1, delta2=delta2, &
                                    horzStagger=horz_stagger, &
                                    name="source igrid", rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10
      call ESMF_IGridDistribute(igrid, delayout=delayout, &
                               countsPerDEDim1=countsPerDE1(1:nDE1), &
                               countsPerDEDim2=countsPerDE2(1:nDE2), &
                               rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10

      ! Set up a 3D real array
      call ESMF_ArraySpecSet(arrayspec, rank=3, &
                             typekind=ESMF_TYPEKIND_R8)

      ! Create a datamap
      order(1) = 0
      order(2) = 1
      order(3) = 2
      call ESMF_FieldDataMapSetDefault(datamap, 3, order, &
                                       horzRelloc=ESMF_CELL_NFACE, &
                                       counts=counts(1:1), rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10

      ! Create the field 
      userfield = ESMF_FieldCreate(igrid, arrayspec=arrayspec, datamap=datamap, &
                                  haloWidth=0, name="userfield", rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10
  
      ! Get the allocated array back as an F90 array pointer
      call ESMF_FieldGetDataPointer(userfield, idata, ESMF_DATA_REF, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10

      ! Set initial data values over whole array to -1
      idata(:,:,:) = -1.0

      status = ESMF_SUCCESS

   ! code comes directly here on error
10 continue

       ! set return code
       rc = status

    end subroutine createField2

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   ! Fill the data arrays with known values so after the regrid operation
!   ! we can verify the resulting data values are correct.

    subroutine initializeData(userfield, result, rc)
      type(ESMF_Field), intent(inout) :: userfield
      integer, intent(inout) :: result
      integer, intent(out) :: rc

      ! Local variables
      integer :: i, j, k, status
      type(ESMF_IGrid) :: igrid
      integer :: counts(ESMF_MAXIGRIDDIM)
      real(ESMF_KIND_R8), pointer :: coordX(:,:), coordY(:,:), idata(:,:,:)
      type(ESMF_RelLoc) :: relloc
      real(ESMF_KIND_R8) :: pi = 3.14159
      !type(ESMF_FieldDataMap) :: datamap


      ! get the igrid and coordinates
      call ESMF_FieldGet(userfield, igrid=igrid, horzRelloc=relloc, &
                         rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10

      call ESMF_IGridGetCoord(igrid, dim=1, horzRelloc=relloc, &
                             centerCoord=coordX, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10
      call ESMF_IGridGetCoord(igrid, dim=2, horzRelloc=relloc, &
                             centerCoord=coordY, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10


      ! get a pointer to the start of the loca data block
      call ESMF_FieldGetDataPointer(userfield, idata, ESMF_DATA_REF, &
                                    counts=counts,  rc=status) 
      if (status .ne. ESMF_SUCCESS) goto 10

      ! increment data values in place - where do i get the third dim?
      do j = 1,counts(3)
        do i = 1,counts(2)
           do k = 1,counts(1)
               idata(k,i,j) = 10.0 + 5.0*sin(coordX(i,j)/60.0*pi) &
                                   + 2.0*sin(coordY(i,j)/50.0*pi)
           enddo
        enddo
      enddo

      status = ESMF_SUCCESS

   ! code comes directly here on error
10 continue

      rc = status

    end subroutine initializeData


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   ! Validate that the resulting data has the values we expect to be there.
 
    subroutine verifyResults(userfield, returnedError, result, rc)
      type(ESMF_Field), intent(inout) :: userfield
      real(ESMF_KIND_R8), intent(out) :: returnedError
      integer, intent(inout) :: result
      integer, intent(out) :: rc

      ! Local variables
      integer :: status, i, j, k, myDE, counts(2)
      type(ESMF_RelLoc) :: relloc
      type(ESMF_IGrid) :: igrid
      real(ESMF_KIND_R8) :: pi, error, maxError, maxPerError
      real(ESMF_KIND_R8) :: minCValue, maxCValue, minDValue, maxDValue
      real(ESMF_KIND_R8), dimension(:,:), pointer :: calc, coordX, coordY
      real(ESMF_KIND_R8), dimension(:,:,:), pointer :: data


      pi = 3.14159

      ! get the igrid and coordinates

      call ESMF_FieldGet(userfield, igrid=igrid, horzRelloc=relloc, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10

      call ESMF_IGridGetDELocalInfo(igrid, myDE=myDE, &
                                   localCellCountPerDim=counts, &
                                   horzRelloc=ESMF_CELL_CENTER, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10

      call ESMF_IGridGetCoord(igrid, dim=1, horzRelloc=relloc, &
                             centerCoord=coordX, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10
      call ESMF_IGridGetCoord(igrid, dim=2, horzRelloc=relloc, &
                             centerCoord=coordY, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10

      ! Get a pointer to the start of the data
      call ESMF_FieldGetDataPointer(userfield, data, ESMF_DATA_REF, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 10

      ! allocate array for computed results and fill it
      allocate(calc(counts(1), counts(2)))
      do j   = 1,counts(2)
        do i = 1,counts(1)
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
      do k     = 1,size(data,1)
        do j   = 1,counts(2)
          do i = 1,counts(1)
            error       = abs(data(k,i,j)) - abs(calc(i,j))
            minCValue   = min(minCValue, abs(calc(i,j)))
            maxCValue   = max(maxCValue, abs(calc(i,j)))
            minDValue   = min(minDValue, abs(data(k,i,j)))
            maxDValue   = max(maxDValue, abs(data(k,i,j)))
            maxError    = max(maxError, abs(error))
            maxPerError = max(maxPerError, 100.*abs(error)/abs(calc(i,j)))
          enddo
        enddo
      enddo

      write(*,*) " "
      write(*,*) "Detailed results for DE #", myDE, ":"
      write(*,*) "   minimum regridded value = ", minDValue
      write(*,*) "   maximum regridded value = ", maxDValue
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

   ! code comes directly here on error
10 continue

      ! set return code
      rc = status

    end subroutine verifyResults


!-------------------------------------------------------------------------

end program ESMF_RegridOptionsUTest
