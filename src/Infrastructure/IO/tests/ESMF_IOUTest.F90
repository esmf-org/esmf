! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_IOUTest

!------------------------------------------------------------------------------

#define ESMF_FILENAME "ESMF_IOUTest.F90"
#include "ESMF.h"

use ESMF_IOScripMod
!==============================================================================
!BOP
! !PROGRAM: ESMF_IOUTest -  Tests some basic ESMF IO configuration and usage
!
! !DESCRIPTION:
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF
#if defined ESMF_NETCDF
  use netcdf
#endif

  implicit none

!-------------------------------------------------------------------------
!=========================================================================

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name
  integer :: result = 0

  ! local variables
  type(ESMF_VM):: vm
  integer :: localPet, petCount, rc, wantRc, ii, ncRc, ncid, varid

  real(ESMF_KIND_R8) :: factorList(10), factorListEmpty(0), desiredFactorList(10), &
    actualFactorList(10), actualSrc(10), desiredSrc(10), actualDst(10), desiredDst(10)
  real(ESMF_KIND_R8), allocatable, dimension(:) :: factorListParallel(:)
  integer(ESMF_KIND_I4) :: factorIndexList(2,10), factorIndexListEmpty(2, 0)
  integer(ESMF_KIND_I4), allocatable, dimension(:,:) :: factorIndexListParallel(:,:)
  character(32) :: filename

  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !-----------------------------------------------------------------------------

  ! Set up
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  factorList = (/0,1,2,3,4,5,6,7,8,9/)
  factorIndexList = reshape((/1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2/), &
                            shape(factorIndexList))
  filename = "doodle.nc"

  !NEX_UTest
  rc = ESMF_FAILURE
  call ESMF_OutputWeightFile(filename, factorList, factorIndexList, rc=rc)
  write(failMsg, *) "did not return ESMF_SUCCESS"
  write(name, *) "call ESMF_OutputWeightFile"
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
  
  !=================================================================================
  ! Test empty factor lists on some PETs.
  
  ! Set up factor lists and factor index lists on four PETs. PET 0 and 2 will be
  ! empty with data on the other PETs.
  if (any(localPet == (/0,2/))) then
    allocate(factorListParallel(0), factorIndexListParallel(2,0))
  else
    allocate(factorListParallel(localPet**2), factorIndexListParallel(2,localPet**2))
    factorListParallel(:) = localPet * 10
    factorIndexListParallel(:,:) = localPet * 20
    do ii=1,localPet**2
      factorListParallel(ii) = factorListParallel(ii) + ii
      factorIndexListParallel(1,ii) = factorIndexListParallel(1,ii) + ii
      factorIndexListParallel(2,ii) = factorIndexListParallel(2,ii) + ii + 100
    enddo
  endif
  
  !NEX_UTest
  rc = ESMF_FAILURE
  call ESMF_OutputWeightFile("doodle2.nc", factorListParallel, factorIndexListParallel, rc=rc)
  write(name, *) "call ESMF_OutputWeightFile with empty factor list on four PETs"
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  if (petCount == 1) then
    write(failMsg, *) "Did not return ESMF_RC_NOT_IMPL"
    wantRc = ESMF_RC_NOT_IMPL
  else
    write(failMsg, *) "Did not return ESMF_SUCESS"
    wantRc = ESMF_SUCCESS

#if defined ESMF_NETCDF
  ! **^^**^^**^^**^^**^^**^^**^^**^^**^^**^^**^^**^^**^^**^^**^^**^^**^^**^^**^^**^^
  ! Open netCDf weights file.
  ncRc = nf90_open("doodle2.nc", NF90_NOWRITE, ncid)
  if (ESMF_LogFoundNetCDFError(ncRc, file=ESMF_FILENAME, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  ! Read netCDF data into actual variables.
  ncRc = nf90_inq_varid(ncid, "S", varid)
  if (ESMF_LogFoundNetCDFError(ncRc, file=ESMF_FILENAME, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ncRc = nf90_get_var(ncid, varid, actualFactorList)
  if (ESMF_LogFoundNetCDFError(ncRc, file=ESMF_FILENAME, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  ncRc = nf90_inq_varid(ncid, "col", varid)
  if (ESMF_LogFoundNetCDFError(ncRc, file=ESMF_FILENAME, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ncRc = nf90_get_var(ncid, varid, actualSrc)
  if (ESMF_LogFoundNetCDFError(ncRc, file=ESMF_FILENAME, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  ncRc = nf90_inq_varid(ncid, "row", varid)
  if (ESMF_LogFoundNetCDFError(ncRc, file=ESMF_FILENAME, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ncRc = nf90_get_var(ncid, varid, actualDst)
  if (ESMF_LogFoundNetCDFError(ncRc, file=ESMF_FILENAME, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  ! Close netCDF weights file.
  ncRc = nf90_close(ncid)
  if (ESMF_LogFoundNetCDFError(ncRc, file=ESMF_FILENAME, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  ! --++==--++==--++==--++==--++==--++==--++==--++==--++==--++==--++==--++==--++==--
  ! Test factors are as expected.
  rc = ESMF_SUCCESS
  
  desiredFactorList = (/11, 31, 32, 33, 34, 35, 36, 37, 38, 39/)
  do ii=1,size(desiredFactorList)
    if (actualFactorList(ii) .ne. desiredFactorList(ii)) then
      write(failMsg, *) "actual factor list not equal to desired factor list"
      rc = ESMF_FAILURE
      exit
    endif
  enddo
  
  if (rc .eq. ESMF_SUCCESS) then
    desiredSrc = (/21, 61, 62, 63, 64, 65, 66, 67, 68, 69/)
    do ii=1,size(desiredFactorList)
      if (actualSrc(ii) .ne. desiredSrc(ii)) then
        write(failMsg, *) "actual source (col) list not equal to desired factor index list"
        rc = ESMF_FAILURE
        exit
      endif
    enddo
  endif
  
  if (rc .eq. ESMF_SUCCESS) then
    desiredDst = (/121, 161, 162, 163, 164, 165, 166, 167, 168, 169/)
    do ii=1,size(desiredFactorList)
      if (actualDst(ii) .ne. desiredDst(ii)) then
        write(failMsg, *) "actual destination (row) list not equal to desired factor index list"
        rc = ESMF_FAILURE
        exit
      endif
    enddo
  endif
  ! --++==--++==--++==--++==--++==--++==--++==--++==--++==--++==--++==--++==--++==--
    
  ! **^^**^^**^^**^^**^^**^^**^^**^^**^^**^^**^^**^^**^^**^^**^^**^^**^^**^^**^^**^^
#endif

  endif
  call ESMF_Test((rc==wantRc), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

  deallocate(factorListParallel, factorIndexListParallel)
  !=================================================================================

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !-----------------------------------------------------------------------------

  end program ESMF_IOUTest
