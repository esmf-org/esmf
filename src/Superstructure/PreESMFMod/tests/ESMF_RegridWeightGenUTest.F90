! $Id: ESMF_RegridWeightGenUTest.F90,v 1.1 2012/11/19 22:32:38 peggyli Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2012, University Corporation for Atmospheric Research,
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

!==============================================================================
!BOP
! !PROGRAM: ESMF_RegridWeightGenUTest - Check Grid Coordinate manipulation routines
!
! !DESCRIPTION:
!
! The code in this file tests ESMF_RegridWeightGen() API.
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF_RegridWeightGenMod

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id: ESMF_RegridWeightGenUTest.F90,v 1.1 2012/11/19 22:32:38 peggyli Exp $'
!------------------------------------------------------------------------------
    
  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: localrc, rc, petCount,localPet
  ! individual test failure message
  character(ESMF_MAXSTR) :: name, failMsg

  logical :: correct
  type(ESMF_VM) :: vm
  character(len=256) :: srcfile, dstfile, wgtfile
  type(ESMF_PoleMethod_Flag) :: pole
  type(ESMF_FileFormat_Flag) :: srcFileType, dstFileType
  type(ESMF_RegridMethod_Flag) :: methodflag
  type(ESMF_UnmappedAction_Flag) :: unmappedaction
  
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
  write(name, *) "RegridWeightGen bilinear Test"
  write(failmsg, *) "Did not return ESMF_SUCCESS"

  srcfile = 'data/T42_grid.nc'
  dstfile = 'data/ll2.5deg_grid.nc'
  wgtfile = 'data/T42_ll2.5_bilinear.nc'

  pole = ESMF_POLEMETHOD_ALLAVG
  srcFileType = ESMF_FILEFORMAT_SCRIP
  dstFileType = ESMF_FILEFORMAT_SCRIP
  methodflag = ESMF_REGRIDMETHOD_BILINEAR
  unmappedaction = ESMF_UNMAPPEDACTION_ERROR

  call ESMF_RegridWeightGen(srcfile, dstfile, wgtfile, regridmethod=methodflag, &
       polemethod = pole, unmappedaction = unmappedaction, &
       srcFileType = srcFileType, dstFileType = dstFileType, &
       verboseFlag = .true., rc=rc)

  call ESMF_Test(((rc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "RegridWeightGen patch Test"
  write(failmsg, *) "Did not return ESMF_SUCCESS"

  methodflag = ESMF_REGRIDMETHOD_PATCH
  wgtfile = 'data/T42_ll2.5_patch.nc'

  call ESMF_RegridWeightGen(srcfile, dstfile, wgtfile, regridmethod=methodflag, &
       polemethod = pole, unmappedaction = unmappedaction, &
       srcFileType = srcFileType, dstFileType = dstFileType, &
       verboseFlag = .true., rc=rc)

  call ESMF_Test(((rc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "RegridWeightGen conserve with no pole Test"
  write(failmsg, *) "Did not return ESMF_SUCCESS"

  pole = ESMF_POLEMETHOD_NONE
  methodflag = ESMF_REGRIDMETHOD_CONSERVE
  unmappedaction = ESMF_UNMAPPEDACTION_IGNORE
  wgtfile = 'data/T42_ll2.5_conserve.nc'

  call ESMF_RegridWeightGen(srcfile, dstfile, wgtfile, regridmethod=methodflag, &
       polemethod = pole, unmappedaction = unmappedaction, &
       srcFileType = srcFileType, dstFileType = dstFileType, &
       verboseFlag = .true., rc=rc)

  call ESMF_Test(((rc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

end program ESMF_RegridWeightGenUTest
