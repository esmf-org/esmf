! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_IOCompUTest

!------------------------------------------------------------------------------
! INCLUDES
!#define ESMF_FILENAME "ESMF_IOCompUTest.F90"
#include "ESMF.h"
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_IOCompUTest - I/O Unit Tests
!
! !DESCRIPTION:
!
! The code in this file drives F90 I/O unit tests.
! The companion file ESMF\_IO.F90 contains the definitions for the
! I/O methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF         ! the ESMF Framework

      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id$'
!------------------------------------------------------------------------------

!-------------------------------------------------------------------------
!=========================================================================

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

      ! local parameters
      integer,            parameter :: fieldCount = 6
      real(ESMF_KIND_R4), parameter :: epsR4 = epsilon(1._ESMF_KIND_R4)
      real(ESMF_KIND_R8), parameter :: epsR8 = epsilon(1._ESMF_KIND_R8)

      ! local variables
      integer :: item, rc
      integer(ESMF_KIND_I4), dimension(fieldCount) :: maxdiffI4
      real   (ESMF_KIND_R4), dimension(fieldCount) :: maxdiffR4
      real   (ESMF_KIND_R8), dimension(fieldCount) :: maxdiffR8
      type   (ESMF_Field),   dimension(fieldCount) :: fieldOut, fieldInp
      type(ESMF_Grid)            :: grid
      type(ESMF_GridComp)        :: ioComp

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

!-------------------------------------------------------------------------------
!  The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!  always run. When the environment variable, EXHAUSTIVE, is set to ON then
!  the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!  to OFF, then only the sanity unit tests.
!  Special strings (Non-exhaustive and exhaustive) have been
!  added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  ! check if ESMF was built with NetCDF support
  !-----------------------------------------------------------------------------
#ifdef ESMF_NETCDF
  write(6,'(2x,"INFO",2x,"ESMF was built with NetCDF support")')
#else
  write(6,'(2x,"INFO",2x,"ESMF was built WITHOUT NetCDF support")')
#endif
  !-----------------------------------------------------------------------------

  !------------------------------------------------------------------------
  ! Part 1 - Test I/O for Fields on cubed sphere grid
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  ! Preparations
  !------------------------------------------------------------------------

  call IOCompUTestSetup(grid, fieldInp, fieldOut, 1, rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! -- actual ESMFIO testing...

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Create ESMFIO Component Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  ioComp = ESMFIO_Create(grid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Write through ESMFIO Component Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMFIO_Write(IOComp, 'restart.nc', fieldOut, filePath='./', rc=rc)
#ifdef ESMF_NETCDF
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc.eq.ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Read through ESMFIO Component Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMFIO_Read(IOComp, 'restart.nc', fieldInp, filePath='./', rc=rc)
#ifdef ESMF_NETCDF
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc.eq.ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroy ESMFIO Component Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMFIO_Destroy(ioComp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------

#ifdef ESMF_NETCDF
  ! -- post test validation
  call IOCompUTestCompare(fieldInp, fieldOut, maxdiffI4, maxdiffR4, maxdiffR8, rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
#endif

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Compare read vs. written ESMF_TYPEKIND_I4 Field center values"
  write(failMsg, *) "Maximum difference found: ",maxdiffI4
#ifdef ESMF_NETCDF
  call ESMF_Test((maxdiffI4(1).eq.0), name, failMsg, result, ESMF_SRCLINE)
#else
  call ESMF_Test(.true., "SKIP: "//name, failMsg, result, ESMF_SRCLINE)
#endif

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Compare read vs. written ESMF_TYPEKIND_I4 Field edge1 values"
  write(failMsg, *) "Maximum difference found: ",maxdiffI4
#ifdef ESMF_NETCDF
  call ESMF_Test((maxdiffI4(2).eq.0), name, failMsg, result, ESMF_SRCLINE)
#else
  call ESMF_Test(.true., "SKIP: "//name, failMsg, result, ESMF_SRCLINE)
#endif

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Compare read vs. written ESMF_TYPEKIND_I4 Field edge2 values"
  write(failMsg, *) "Maximum difference found: ",maxdiffI4
#ifdef ESMF_NETCDF
  call ESMF_Test((maxdiffI4(3).eq.0), name, failMsg, result, ESMF_SRCLINE)
#else
  call ESMF_Test(.true., "SKIP: "//name, failMsg, result, ESMF_SRCLINE)
#endif

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Compare read vs. written ESMF_TYPEKIND_I4 Field corner values"
  write(failMsg, *) "Maximum difference found: ",maxdiffI4
#ifdef ESMF_NETCDF
  call ESMF_Test((maxdiffI4(4).eq.0), name, failMsg, result, ESMF_SRCLINE)
#else
  call ESMF_Test(.true., "SKIP: "//name, failMsg, result, ESMF_SRCLINE)
#endif

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Compare read vs. written ESMF_TYPEKIND_R4 Field values"
  write(failMsg, *) "Maximum difference found: ",maxdiffR4
#ifdef ESMF_NETCDF
  call ESMF_Test((maxdiffR4(5).lt.epsR4), name, failMsg, result, ESMF_SRCLINE)
#else
  call ESMF_Test(.true., "SKIP: "//name, failMsg, result, ESMF_SRCLINE)
#endif

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Compare read vs. written ESMF_TYPEKIND_R8 Field values"
  write(failMsg, *) "Maximum difference found: ",maxdiffR8
#ifdef ESMF_NETCDF
  call ESMF_Test((maxdiffR8(6).lt.epsR8), name, failMsg, result, ESMF_SRCLINE)
#else
  call ESMF_Test(.true., "SKIP: "//name, failMsg, result, ESMF_SRCLINE)
#endif

  !------------------------------------------------------------------------
  ! clean up
  !------------------------------------------------------------------------
  do item = 1, size(fieldInp)
    call ESMF_FieldDestroy(fieldInp(item), noGarbage=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  end do
  do item = 1, size(fieldOut)
    call ESMF_FieldDestroy(fieldOut(item), noGarbage=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  end do
  call ESMF_GridDestroy(grid, noGarbage=.true., rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  ! Part 2 - Test I/O for Fields on regular spherical grid
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  ! Preparations
  !------------------------------------------------------------------------

  call IOCompUTestSetup(grid, fieldInp, fieldOut, 2, rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! -- actual ESMFIO testing...
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Create ESMFIO Component Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  ioComp = ESMFIO_Create(grid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Write through ESMFIO Component Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMFIO_Write(IOComp, 'restart.nc', fieldOut, filePath='./', rc=rc)
#ifdef ESMF_NETCDF
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc.eq.ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Read through ESMFIO Component Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMFIO_Read(IOComp, 'restart.nc', fieldInp, filePath='./', rc=rc)
#ifdef ESMF_NETCDF
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc.eq.ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroy ESMFIO Component Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMFIO_Destroy(ioComp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------

#ifdef ESMF_NETCDF
  ! -- post test validation
  call IOCompUTestCompare(fieldInp, fieldOut, maxdiffI4, maxdiffR4, maxdiffR8, rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
#endif

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Compare read vs. written ESMF_TYPEKIND_I4 Field center values"
  write(failMsg, *) "Maximum difference found: ",maxdiffI4
#ifdef ESMF_NETCDF
  call ESMF_Test((maxdiffI4(1).eq.0), name, failMsg, result, ESMF_SRCLINE)
#else
  call ESMF_Test(.true., "SKIP: "//name, failMsg, result, ESMF_SRCLINE)
#endif

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Compare read vs. written ESMF_TYPEKIND_I4 Field edge1 values"
  write(failMsg, *) "Maximum difference found: ",maxdiffI4
#ifdef ESMF_NETCDF
  call ESMF_Test((maxdiffI4(2).eq.0), name, failMsg, result, ESMF_SRCLINE)
#else
  call ESMF_Test(.true., "SKIP: "//name, failMsg, result, ESMF_SRCLINE)
#endif

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Compare read vs. written ESMF_TYPEKIND_I4 Field edge2 values"
  write(failMsg, *) "Maximum difference found: ",maxdiffI4
#ifdef ESMF_NETCDF
  call ESMF_Test((maxdiffI4(3).eq.0), name, failMsg, result, ESMF_SRCLINE)
#else
  call ESMF_Test(.true., "SKIP: "//name, failMsg, result, ESMF_SRCLINE)
#endif

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Compare read vs. written ESMF_TYPEKIND_I4 Field corner values"
  write(failMsg, *) "Maximum difference found: ",maxdiffI4
#ifdef ESMF_NETCDF
  call ESMF_Test((maxdiffI4(4).eq.0), name, failMsg, result, ESMF_SRCLINE)
#else
  call ESMF_Test(.true., "SKIP: "//name, failMsg, result, ESMF_SRCLINE)
#endif

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Compare read vs. written ESMF_TYPEKIND_R4 Field values"
  write(failMsg, *) "Maximum difference found: ",maxdiffR4
#ifdef ESMF_NETCDF
  call ESMF_Test((maxdiffR4(5).lt.epsR4), name, failMsg, result, ESMF_SRCLINE)
#else
  call ESMF_Test(.true., "SKIP: "//name, failMsg, result, ESMF_SRCLINE)
#endif

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Compare read vs. written ESMF_TYPEKIND_R8 Field values"
  write(failMsg, *) "Maximum difference found: ",maxdiffR8
#ifdef ESMF_NETCDF
  call ESMF_Test((maxdiffR8(6).lt.epsR8), name, failMsg, result, ESMF_SRCLINE)
#else
  call ESMF_Test(.true., "SKIP: "//name, failMsg, result, ESMF_SRCLINE)
#endif

  !------------------------------------------------------------------------
  ! clean up      
  !------------------------------------------------------------------------
  do item = 1, size(fieldInp)
    call ESMF_FieldDestroy(fieldInp(item), noGarbage=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  end do
  do item = 1, size(fieldOut)
    call ESMF_FieldDestroy(fieldOut(item), noGarbage=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  end do
  call ESMF_GridDestroy(grid, noGarbage=.true., rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

contains

  subroutine IOCompUTestSetup(grid, fieldInp, fieldOut, igrid, rc)

    type(ESMF_Grid),                intent(inout) :: grid
    type(ESMF_Field), dimension(:), intent(inout) :: fieldInp, fieldOut
    integer,                          intent(in)  :: igrid
    integer,                          intent(out) :: rc

    ! local variables
    integer :: i, j, k, staggerlocCount
    integer :: de, deCount, dimCount, localDe, localDeCount, tile, tileCount
    integer, dimension(2) :: lbnd, ubnd, ccnt
    character(len=ESMF_MAXSTR) :: fieldName
    integer,               dimension(:),   allocatable :: localDeToDeMap
    integer,               dimension(:,:), allocatable :: minIndexPDe, maxIndexPDe
    integer(ESMF_KIND_I4), dimension(:,:), pointer :: fpOutI4
    real(ESMF_KIND_R4),    dimension(:,:), pointer :: fpOutR4
    real(ESMF_KIND_R8),    dimension(:,:), pointer :: fpOutR8
    real(ESMF_KIND_R8),    dimension(:,:), pointer :: plon, plat
    type(ESMF_DistGrid) :: distgrid
    type(ESMF_DELayout) :: delayout

    !------------------------------------------------------------------------
    ! Preparations
    !------------------------------------------------------------------------

    select case (igrid)
      case (1)
        write(6,'(2x,"INFO  Create Cubed Sphere Grid")')
        grid = ESMF_GridCreateCubedSphere(tilesize=96, &
          staggerlocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
          name='fcst_grid', rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
      case (2)
        write(6,'("2x,INFO  Create Regular Spherical Grid")')
        grid = ESMF_GridCreate1PeriDim(maxIndex=(/360, 180/), &
          coordSys=ESMF_COORDSYS_SPH_DEG, coordDep1=(/1,2/), coordDep2=(/1,2/), &
          indexflag=ESMF_INDEX_GLOBAL, name='fcst_grid', rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        do k = 1, 2
          call ESMF_GridGetCoord(grid, coordDim=k, localDE=0, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            computationalLBound=lbnd, computationalUBound=ubnd, &
            farrayPtr=plon, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return
          do j=lbnd(2), ubnd(2)
            do i=lbnd(1), ubnd(1)
              plon(i,j) = 1._ESMF_KIND_R8*(2-k)*(i-1) &
                          + (1._ESMF_KIND_R8*(j-1) - 90._ESMF_KIND_R8)*(k-1)
            end do
          end do
        end do
    end select

    ! -- Test supported ESMF typekinds
    ! -- ESMF_TYPEKIND_I4
    ! -- test all stagger locations
    call ESMF_GridGet(grid, staggerlocCount=staggerlocCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    do k = 1, staggerlocCount
      fieldName = ""
      write(fieldName,'("test_i4_sloc",i0)') k-1
      fieldInp(k) = ESMF_FieldCreate(grid, ESMF_TYPEKIND_I4, &
        staggerloc=ESMF_StaggerLoc(k-1), name=trim(fieldName), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      fieldOut(k) = ESMF_FieldCreate(grid, ESMF_TYPEKIND_I4, &
        staggerloc=ESMF_StaggerLoc(k-1), name=trim(fieldName), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
    end do

    k = staggerlocCount + 1
    ! -- ESMF_TYPEKIND_R4
    fieldInp(k) = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R4, name="test_r4", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    fieldOut(k) = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R4, name="test_r4", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    k = k + 1
    ! -- ESMF_TYPEKIND_R8
    fieldInp(k) = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R8, name="test_r8", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    fieldOut(k) = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R8, name="test_r8", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    call ESMF_GridGet(grid, localDECount=localDeCount, dimCount=dimCount, &
      tileCount=tileCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! -- fill in Fields
    do k = 1, staggerlocCount
     ! -- get domain decomposition
      call ESMF_GridGet(grid, ESMF_StaggerLoc(k-1), distgrid=distgrid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return

      call ESMF_DistGridGet(distgrid, deCount=deCount, dimCount=dimCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return

      allocate(minIndexPDe(dimCount, deCount), maxIndexPDe(dimCount, deCount), &
               localDeToDeMap(localDeCount))

      minIndexPDe = 0
      maxIndexPDe = 0
      localDeToDeMap = 0

      call ESMF_DistGridGet(distgrid, delayout=delayout, &
        minIndexPDe=minIndexPDe, maxIndexPDe=maxIndexPDe, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return

      call ESMF_DELayoutGet(delayout, localDeToDeMap=localDeToDeMap, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return

      do localDe = 0, localDeCount-1
        call ESMF_FieldGet(fieldOut(k), localDE=localDe, farrayPtr=fpOutI4, &
          computationalLBound=lbnd, computationalUBound=ubnd, &
          computationalCount=ccnt, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return

        call ESMF_GridGet(grid, localDE=localDe, tile=tile, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return

        de = localDeToDeMap(localDe+1) + 1

        do j = lbnd(2), ubnd(2)
          do i = lbnd(1), ubnd(1)
            fpOutI4(i,j) = i-lbnd(1)+minIndexPDe(1,de) &
                         + j-lbnd(2)+minIndexPDe(2,de)
          end do
        end do
      end do

      deallocate(minIndexPDe, maxIndexPDe, localDeToDeMap)

    end do

    do localDe = 0, localDeCount-1

      call ESMF_GridGetCoord(grid, coordDim=1, localDE=localDe, farrayPtr=plon, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      call ESMF_GridGetCoord(grid, coordDim=2, localDE=localDe, farrayPtr=plat, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return

      k = staggerlocCount + 1
      ! -- ESMF_TYPEKIND_R4
      call ESMF_FieldGet(fieldOut(k), localDE=localDe, farrayPtr=fpOutR4, &
        computationalLBound=lbnd, computationalUBound=ubnd, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return

      do j = lbnd(2), ubnd(2)
        do i = lbnd(1), ubnd(1)
          fpOutR4(i,j) = real(sin(plat(i,j)/ESMF_COORDSYS_RAD2DEG) &
                             *cos(plon(i,j)/ESMF_COORDSYS_RAD2DEG), kind=ESMF_KIND_R4)
        end do
      end do

      k = k + 1
      ! -- ESMF_TYPEKIND_R4
      call ESMF_FieldGet(fieldOut(k), localDE=localDe, farrayPtr=fpOutR8, &
        computationalLBound=lbnd, computationalUBound=ubnd, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return

      do j = lbnd(2), ubnd(2)
        do i = lbnd(1), ubnd(1)
          fpOutR8(i,j) = cos(plat(i,j)/ESMF_COORDSYS_RAD2DEG)*sin(plon(i,j)/ESMF_COORDSYS_RAD2DEG)
        end do
      end do

    end do


  end subroutine IOCompUTestSetup

  subroutine IOCompUTestCompare(inputFields, outputFields, maxdiffI4, maxdiffR4, maxdiffR8, rc)

    type(ESMF_Field),      dimension(:), intent(in)  :: inputFields, outputFields
    integer(ESMF_KIND_I4), dimension(:), intent(out) :: maxdiffI4
    real   (ESMF_KIND_R4), dimension(:), intent(out) :: maxdiffR4
    real   (ESMF_KIND_R8), dimension(:), intent(out) :: maxdiffR8
    integer,                             intent(out) :: rc

    ! -- local variables
    integer :: fieldCount, item, localDe, localDeCount
    integer(ESMF_KIND_I4) :: maxdI4
    real   (ESMF_KIND_R4) :: maxdR4
    real   (ESMF_KIND_R8) :: maxdR8

    integer(ESMF_KIND_I4), dimension(:,:), pointer :: fpInpI4, fpOutI4
    real   (ESMF_KIND_R4), dimension(:,:), pointer :: fpInpR4, fpOutR4
    real   (ESMF_KIND_R8), dimension(:,:), pointer :: fpInpR8, fpOutR8

    type(ESMF_TypeKind_Flag) :: typekind

    ! -- begin
    rc = ESMF_SUCCESS

    maxdiffI4 = 0_ESMF_KIND_I4
    maxdiffR4 = 0._ESMF_KIND_R4
    maxdiffR8 = 0._ESMF_KIND_R8

    fieldCount = size(inputFields)
    if (fieldCount /= size(outputFields)) then
      call ESMF_LogSetError(ESMF_RC_ARG_SIZE, &
      msg="Size of input and output Fields arrays must match.", &
      line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    end if

    if ((fieldCount /= size(maxdiffI4)) .or. &
        (fieldCount /= size(maxdiffR4)) .or. &
        (fieldCount /= size(maxdiffR8))) then
      call ESMF_LogSetError(ESMF_RC_ARG_SIZE, &
        msg="Size of maxdiff arguments must match size of input/output Field arrays", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    end if

    do item = 1, size(inputFields)
      call ESMF_FieldGet(inputFields(item), localDeCount=localDeCount, &
        typekind=typekind, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return

      if      (typekind == ESMF_TYPEKIND_I4) then
        print *,'Comparing ESMF_TYPEKIND_I4 Fields'
        do localDe = 0, localDeCount-1
          call ESMF_FieldGet(inputFields(item), localDE=localDe, &
            farrayPtr=fpInpI4, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return
          call ESMF_FieldGet(outputFields(item), localDE=localDe, &
            farrayPtr=fpOutI4, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return

          print *,'|write     |, min/max: ', minval(fpOutI4), maxval(fpOutI4)
          print *,'|read      |, min/max: ', minval(fpInpI4), maxval(fpInpI4)
          maxdI4 = maxval(abs(fpOutI4-fpInpI4))
          print *,'|read-write|, min/max: ', minval(abs(fpOutI4-fpInpI4)), maxdI4
          maxdiffI4(item) = max(maxdiffI4(item), maxdI4)
        end do
      else if (typekind == ESMF_TYPEKIND_R4) then
        print *,'Comparing ESMF_TYPEKIND_R4 Fields'
        do localDe = 0, localDeCount-1
          call ESMF_FieldGet(inputFields(item), localDE=localDe, &
            farrayPtr=fpInpR4, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return
          call ESMF_FieldGet(outputFields(item), localDE=localDe, &
            farrayPtr=fpOutR4, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return

          print *,'|write     |, min/max: ', minval(fpOutR4), maxval(fpOutR4)
          print *,'|read      |, min/max: ', minval(fpInpR4), maxval(fpInpR4)
          maxdR4 = maxval(abs(fpOutR4-fpInpR4))
          print *,'|read-write|, min/max: ', minval(abs(fpOutR4-fpInpR4)), maxdR4
          maxdiffR4(item) = max(maxdiffR4(item), maxdR4)
        end do
      else if (typekind == ESMF_TYPEKIND_R8) then
        print *,'Comparing ESMF_TYPEKIND_R8 Fields'
        do localDe = 0, localDeCount-1
          call ESMF_FieldGet(inputFields(item), localDE=localDe, &
            farrayPtr=fpInpR8, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return
          call ESMF_FieldGet(outputFields(item), localDE=localDe, &
            farrayPtr=fpOutR8, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return

          print *,'|write     |, min/max: ', minval(fpOutR8), maxval(fpOutR8)
          print *,'|read      |, min/max: ', minval(fpInpR8), maxval(fpInpR8)
          maxdR8 = maxval(abs(fpOutR8-fpInpR8))
          print *,'|read-write|, min/max: ', minval(abs(fpOutR8-fpInpR8)), maxdR8
          maxdiffR8(item) = max(maxdiffR8(item), maxdR8)
        end do
      end if
      flush(6)
    end do

  end subroutine IOCompUTestCompare

end program ESMF_IOCompUTest
