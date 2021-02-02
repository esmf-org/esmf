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
program ESMF_RegridWeightGenUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_RegridWeightGenUTest - Test two ESMF_RegridWeightGen() APIs
!
! !DESCRIPTION:
!
! The code in this file tests the two ESMF_RegridWeightGen() APIs -- one generates a weight file and the other
! generate a route handle.
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF_RegridWeightGenMod

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id$'
!------------------------------------------------------------------------------

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: localrc, rc, petCount,localPet
  ! individual test failure message
  character(ESMF_MAXSTR) :: name, failMsg

  logical :: correct
  type(ESMF_VM) :: vm
  character(len=256) :: srcfile, dstfile, wgtfile, rhfile
  type(ESMF_PoleMethod_Flag) :: pole
  type(ESMF_FileFormat_Flag) :: srcFileType, dstFileType
  type(ESMF_RegridMethod_Flag) :: methodflag
  type(ESMF_UnmappedAction_Flag) :: unmappedaction
  type(ESMF_RouteHandle) :: routehandle

  rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !-----------------------------------------------------------------------------

  ! get global VM
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "RegridWeightGen bilinear routehandle Test"
  write(failmsg, *) "Did not return ESMF_SUCCESS"

  srcfile = 'data/T42_grid.nc'
  dstfile = 'data/ll2.5deg_grid.nc'
  wgtfile = 'data/T42_ll2.5_bilinear.nc'

  pole = ESMF_POLEMETHOD_ALLAVG
  srcFileType = ESMF_FILEFORMAT_SCRIP
  dstFileType = ESMF_FILEFORMAT_SCRIP
  methodflag = ESMF_REGRIDMETHOD_BILINEAR
  unmappedaction = ESMF_UNMAPPEDACTION_ERROR

  call ESMF_RegridWeightGen(srcfile, dstfile, &
       weightFile=wgtfile, regridmethod=methodflag, &
       polemethod = pole, unmappedaction = unmappedaction, &
       srcFileType = srcFileType, dstFileType = dstFileType, &
       verboseFlag = .true., rc=rc)

#ifdef ESMF_NETCDF
  call ESMF_Test(((rc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "RegridWeightGen weightfile and routehandle file Test"
  write(failmsg, *) "Did not return ESMF_SUCCESS"

  rhfile = 'data/routehandle.dat'

  call ESMF_RegridWeightGen(srcfile, dstfile, &
       weightFile=wgtfile, rhfile=rhfile, regridmethod=methodflag, &
       polemethod = pole, unmappedaction = unmappedaction, &
       srcFileType = srcFileType, dstFileType = dstFileType, &
       verboseFlag = .true., rc=rc)

#ifdef ESMF_NETCDF
  call ESMF_Test(((rc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "RegridWeightGen just routehandle file Test"
  write(failmsg, *) "Did not return ESMF_SUCCESS"

  call ESMF_RegridWeightGen(srcfile, dstfile, &
       rhfile=rhfile, regridmethod=methodflag, &
       polemethod = pole, unmappedaction = unmappedaction, &
       srcFileType = srcFileType, dstFileType = dstFileType, &
       verboseFlag = .true., rc=rc)

#ifdef ESMF_NETCDF
  call ESMF_Test(((rc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "RegridWeightGen no file fail Test"
  write(failmsg, *) "Did not return ESMF_RC_ARG_WRONG"

  call ESMF_RegridWeightGen(srcfile, dstfile, &
       regridmethod=methodflag, &
       polemethod = pole, unmappedaction = unmappedaction, &
       srcFileType = srcFileType, dstFileType = dstFileType, &
       verboseFlag = .true., rc=rc)

#ifdef ESMF_NETCDF
  call ESMF_Test(((rc.eq.ESMF_RC_ARG_WRONG)), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "RegridWeightGen patch Test"
  write(failmsg, *) "Did not return ESMF_SUCCESS"

  methodflag = ESMF_REGRIDMETHOD_PATCH
  wgtfile = 'data/T42_ll2.5_patch.nc'

  call ESMF_RegridWeightGen(srcfile, dstfile, &
       weightFile=wgtfile, regridmethod=methodflag, &
       polemethod = pole, unmappedaction = unmappedaction, &
       srcFileType = srcFileType, dstFileType = dstFileType, &
       verboseFlag = .true., rc=rc)

#if defined(ESMF_NETCDF) && defined(ESMF_LAPACK)
  call ESMF_Test(((rc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "RegridWeightGen conserve with no pole Test"
  write(failmsg, *) "Did not return ESMF_SUCCESS"

  pole = ESMF_POLEMETHOD_NONE
  methodflag = ESMF_REGRIDMETHOD_CONSERVE
  unmappedaction = ESMF_UNMAPPEDACTION_IGNORE
  wgtfile = 'data/T42_ll2.5_conserve.nc'

  call ESMF_RegridWeightGen(srcfile, dstfile, &
       weightFile=wgtfile, regridmethod=methodflag, &
       polemethod = pole, unmappedaction = unmappedaction, &
       srcFileType = srcFileType, dstFileType = dstFileType, &
       verboseFlag = .true., rc=rc)

#ifdef ESMF_NETCDF
  call ESMF_Test(((rc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "RegridWeightGen bilinear with route handle"
  write(failmsg, *) "Did not return ESMF_SUCCESS"

  srcfile = 'data/T42_grid.nc'
  dstfile = 'data/ll2.5deg_grid.nc'
  wgtfile = 'data/T42_ll2.5_bilinear.nc'

  pole = ESMF_POLEMETHOD_ALLAVG
  srcFileType = ESMF_FILEFORMAT_SCRIP
  dstFileType = ESMF_FILEFORMAT_SCRIP
  methodflag = ESMF_REGRIDMETHOD_BILINEAR
  unmappedaction = ESMF_UNMAPPEDACTION_IGNORE

  call ESMF_RegridWeightGen(srcfile, dstfile, routehandle, regridmethod=methodflag, &
       unmappedaction = unmappedaction, &
       verboseFlag = .true., rc=rc)

#ifdef ESMF_NETCDF
  call ESMF_Test(((rc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)
  call ESMF_FieldSMMRelease(routehandle, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "RegridWeightGen patch Test with route handle"
  write(failmsg, *) "Did not return ESMF_SUCCESS"

  methodflag = ESMF_REGRIDMETHOD_PATCH
  wgtfile = 'data/T42_ll2.5_patch.nc'

  call ESMF_RegridWeightGen(srcfile, dstfile, routehandle, regridmethod=methodflag, &
       unmappedaction = unmappedaction, &
       verboseFlag = .true., rc=rc)

#if defined(ESMF_NETCDF) && defined(ESMF_LAPACK)
  call ESMF_Test(((rc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)
  call ESMF_FieldSMMRelease(routehandle, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "RegridWeightGen conserve with route handle"
  write(failmsg, *) "Did not return ESMF_SUCCESS"

  methodflag = ESMF_REGRIDMETHOD_CONSERVE
  unmappedaction = ESMF_UNMAPPEDACTION_IGNORE
  wgtfile = 'data/T42_ll2.5_conserve.nc'

  call ESMF_RegridWeightGen(srcfile, dstfile, routehandle, regridmethod=methodflag, &
       unmappedaction = unmappedaction, &
       verboseFlag = .true., rc=rc)

#ifdef ESMF_NETCDF
  call ESMF_Test(((rc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)
  call ESMF_FieldSMMRelease(routehandle, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

end program ESMF_RegridWeightGenUTest
