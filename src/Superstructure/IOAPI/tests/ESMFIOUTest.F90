! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMFIOUTest

!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_IOGridCompUTest - I/O GridComp Unit Tests
!
! !DESCRIPTION:
!
! The code in this file drives F90 I/O GridComp unit tests.
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

      ! local variables
      integer :: rc
      integer :: i, j, localDe, localDeCount
      real(ESMF_KIND_R8), dimension(:,:), pointer :: plon, plat, fp, fpIn
      type(ESMF_Grid)  :: grid
      type(ESMF_Field) :: field, fieldIn, wfield, rfield
      type(ESMF_GridComp) :: ioComp
      type(ESMF_VM) :: vm

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

#ifdef ESMF_TESTEXHAUSTIVE
      character(ESMF_MAXSTR) :: attrname, attrvalue, outChar
      character(ESMF_MAXSTR) :: conv, purp

#endif

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

      !------------------------------------------------------------------------
      ! Preparations
      !------------------------------------------------------------------------

  grid = ESMF_GridCreateCubedSphere(tilesize=96, &
    staggerlocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
    name='fcst_grid', rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize()

  field = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R8, name="test", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize()

  call ESMF_GridGet(grid, localDECount=localDeCount, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize()

  do localDe = 0, localDeCount-1
    call ESMF_GridGetCoord(grid, coordDim=1, localDE=localDe, farrayPtr=plon, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) call ESMF_Finalize()
    call ESMF_GridGetCoord(grid, coordDim=2, localDE=localDe, farrayPtr=plat, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) call ESMF_Finalize()

    call ESMF_FieldGet(field, localDE=localDe, farrayPtr=fp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) call ESMF_Finalize()

    do j = 1, size(fp,2)
      do i = 1, size(fp,1)
        fp(i,j) = cos(plat(i,j)/ESMF_COORDSYS_RAD2DEG)*sin(plon(i,j)/ESMF_COORDSYS_RAD2DEG)
      end do
    end do
  end do

  ! -- initialize ESMFIO
  ioComp = ESMFIO_Create(grid, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize()

  call ESMFIO_Write(IOComp, 'restart.nc', (/field/), filePath='./', rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize()

  fieldIn = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R8, name="test", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize()

  call ESMFIO_Read(IOComp, 'restart.nc', (/fieldIn/), filePath='./', rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize()


  call ESMFIO_Destroy(ioComp, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize()

  do localDe = 0, localDeCount-1
    call ESMF_FieldGet(field, localDE=localDe, farrayPtr=fp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) call ESMF_Finalize()
    call ESMF_FieldGet(fieldIn, localDE=localDe, farrayPtr=fpIn, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) call ESMF_Finalize()
    print *,'|write     |, min/max: ', minval(fp), maxval(fp)
    print *,'|read      |, min/max: ', minval(fpIn), maxval(fpIn)
    print *,'|read-write|, min/max: ', minval(abs(fp-fpIn)), maxval(abs(fp-fpIn))
  end do

      !------------------------------------------------------------------------
      ! clean up      
      !------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
end program ESMF_IOGridCompUTest
