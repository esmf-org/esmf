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

#define FILENAME "src/Infrastructure/Array/tests/ESMF_ArraySMMFromFileUTest.F90"

#include "ESMF_Macros.inc"
#include "ESMF.h"

module ESMF_ArraySMMFromFileUTestMod

  ! modules
  use ESMF_TestMod     ! test methods
  use ESMF_RegridWeightGenCheckMod ! test array SMM from file
  use ESMF_RegridWeightGenMod      ! test array SMM from file
  use ESMF
  
  implicit none

  public test_smm_from_file

  contains !--------------------------------------------------------------------

  subroutine test_smm_from_file(srcFile, dstFile, weightFile, checkMethod, &
                                rc)
    character(len=*), intent(in) :: srcFile, dstFile, weightFile
    integer, intent(out) :: rc
    type(ESMF_RWGCheckMethod_Flag), intent(in) :: checkMethod

    rc = ESMF_FAILURE

    ! Generate the netCDF weights file.
    call ESMF_RegridWeightGenFile(srcFile, dstFile, weightFile=weightFile, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return

    ! Validate generated weights produce acceptable errors. This subroutine
    ! calls ESMF_ArraySMMStoreFromFile or ESMF_FieldSMMStoreFromFile.
    call ESMF_RegridWeightGenCheck(weightFile, checkMethod=checkMethod, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return

    rc = ESMF_SUCCESS

  end subroutine test_smm_from_file

end module

!==============================================================================
!==============================================================================
!==============================================================================

program ESMF_ArraySMMFromFileUTest

!==============================================================================
!BOP
! !PROGRAM: ESMF_ArraySMMFromFileUTest - Tests ArraySMMFromFile()
!
! !DESCRIPTION:
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod ! test methods
  use ESMF

  use ESMF_ArraySMMFromFileUTestMod

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id$'
!------------------------------------------------------------------------------

!-------------------------------------------------------------------------
!=========================================================================

  character(ESMF_MAXSTR)      :: failMsg
  character(ESMF_MAXSTR)      :: name
  character(len=*), parameter :: srcFile = 'data/T42_grid.nc'
  character(len=*), parameter :: dstFile = 'data/T42_grid.nc'
  character(len=*), parameter :: weightFile = 'test_smm_from_file_weights.nc'
  integer                     :: rc
  integer                     :: result = 0

!-------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------

  !------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------
    
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_ArraySMMFromFile Unit Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

#ifdef ESMF_NETCDF
  call test_smm_from_file(srcFile, dstFile, weightFile, &
                          ESMF_RWGCHECKMETHOD_ARRAY, rc)
#else
  rc = ESMF_SUCCESS
#endif

  call ESMF_Test((rc .eq. ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  ! Must abort to prevent possible hanging due to communications.
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_FieldSMMFromFile Unit Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

#ifdef ESMF_NETCDF
  call test_smm_from_file(srcFile, dstFile, weightFile, &
                          ESMF_RWGCHECKMETHOD_ARRAY, rc)
#else
  rc = ESMF_SUCCESS
#endif

  call ESMF_Test((rc .eq. ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  ! Must abort to prevent possible hanging due to communications.
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally

end program ESMF_ArraySMMFromFileUTest
