! $Id$
!
! Earth System Modeling Framework
! Copyright (c) 2002-2024, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_IO_FileTypeCheckUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_IO_FileTypeCheckUTest - Unit tests of ESMF_FileTypeCheck
! !DESCRIPTION:
!
! The tests is this file exercise ESMF_FileTypeCheck and test the results for various files.
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF
  use ESMF_IOFileTypeCheckMod

  implicit none

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0
  integer :: rc

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  type(ESMF_FileFormat_Flag) :: fileType

  !------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

#ifdef ESMF_TESTEXHAUSTIVE

  !------------------------------------------------------------------------
  !EX_UTest
  call ESMF_FileTypeCheck("test_sph_3x3_scrip.nc", fileType, rc=rc)
  write(name, *) "Detection of SCRIP grid file succeeds"
#if (defined ESMF_NETCDF || defined ESMF_PNETCDF)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc == ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
  !EX_UTest
  write(name, *) "Detection of SCRIP grid file returns correct value"
#if (defined ESMF_NETCDF || defined ESMF_PNETCDF)
  write(failMsg, *) "Returned wrong file type"
  call ESMF_Test(fileType == ESMF_FILEFORMAT_SCRIP, name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Comparison did not fail as expected"
  call ESMF_Test(fileType /= ESMF_FILEFORMAT_SCRIP, name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest
  call ESMF_FileTypeCheck("test_sph_3x3_ugrid.nc", fileType, rc=rc)
  write(name, *) "Detection of UGRID grid file succeeds"
#if (defined ESMF_NETCDF || defined ESMF_PNETCDF)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc == ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
  !EX_UTest
  write(name, *) "Detection of UGRID grid file returns correct value"
#if (defined ESMF_NETCDF || defined ESMF_PNETCDF)
  write(failMsg, *) "Returned wrong file type"
  call ESMF_Test(fileType == ESMF_FILEFORMAT_UGRID, name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Comparison did not fail as expected"
  call ESMF_Test(fileType /= ESMF_FILEFORMAT_UGRID, name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest
  call ESMF_FileTypeCheck("test_sph_3x3_esmf.nc", fileType, rc=rc)
  write(name, *) "Detection of ESMFMESH grid file succeeds"
#if (defined ESMF_NETCDF || defined ESMF_PNETCDF)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc == ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
  !EX_UTest
  write(name, *) "Detection of ESMFMESH grid file returns correct value"
#if (defined ESMF_NETCDF || defined ESMF_PNETCDF)
  write(failMsg, *) "Returned wrong file type"
  call ESMF_Test(fileType == ESMF_FILEFORMAT_ESMFMESH, name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Comparison did not fail as expected"
  call ESMF_Test(fileType /= ESMF_FILEFORMAT_ESMFMESH, name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest
  call ESMF_FileTypeCheck("GRIDSPEC_1x1_subset.nc", fileType, rc=rc)
  write(name, *) "Detection of GRIDSPEC grid file succeeds"
#if (defined ESMF_NETCDF || defined ESMF_PNETCDF)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc == ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
  !EX_UTest
  write(name, *) "Detection of GRIDSPEC grid file returns correct value"
#if (defined ESMF_NETCDF || defined ESMF_PNETCDF)
  write(failMsg, *) "Returned wrong file type"
  call ESMF_Test(fileType == ESMF_FILEFORMAT_GRIDSPEC, name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Comparison did not fail as expected"
  call ESMF_Test(fileType /= ESMF_FILEFORMAT_GRIDSPEC, name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest
  call ESMF_FileTypeCheck("C48_mosaic.nc", fileType, rc=rc)
  write(name, *) "Detection of MOSAIC grid file succeeds"
#if (defined ESMF_NETCDF || defined ESMF_PNETCDF)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc == ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
  !EX_UTest
  write(name, *) "Detection of MOSAIC grid file returns correct value"
#if (defined ESMF_NETCDF || defined ESMF_PNETCDF)
  write(failMsg, *) "Returned wrong file type"
  call ESMF_Test(fileType == ESMF_FILEFORMAT_MOSAIC, name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Comparison did not fail as expected"
  call ESMF_Test(fileType /= ESMF_FILEFORMAT_MOSAIC, name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest
  call ESMF_FileTypeCheck("horizontal_grid.tile6_subset.nc", fileType, rc=rc)
  write(name, *) "Detection of TILE grid file succeeds"
#if (defined ESMF_NETCDF || defined ESMF_PNETCDF)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc == ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
  !EX_UTest
  write(name, *) "Detection of TILE grid file returns correct value"
#if (defined ESMF_NETCDF || defined ESMF_PNETCDF)
  write(failMsg, *) "Returned wrong file type"
  call ESMF_Test(fileType == ESMF_FILEFORMAT_TILE, name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Comparison did not fail as expected"
  call ESMF_Test(fileType /= ESMF_FILEFORMAT_TILE, name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest
  call ESMF_FileTypeCheck("test_sph_3x3_scrip_units_look_like_gridspec.nc", fileType, rc=rc)
  write(name, *) "Detection of SCRIP grid file (with units like GRIDSPEC) succeeds"
#if (defined ESMF_NETCDF || defined ESMF_PNETCDF)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc == ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
  !EX_UTest
  write(name, *) "Detection of SCRIP grid file (with units like GRIDSPEC) returns correct value"
#if (defined ESMF_NETCDF || defined ESMF_PNETCDF)
  write(failMsg, *) "Returned wrong file type"
  call ESMF_Test(fileType == ESMF_FILEFORMAT_SCRIP, name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Comparison did not fail as expected"
  call ESMF_Test(fileType /= ESMF_FILEFORMAT_SCRIP, name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

! ESMF_TESTEXHAUSTIVE
#endif

  !------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------

end program ESMF_IO_FileTypeCheckUTest
