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

#define FILENAME "ESMF_FieldSMMFromFileUTest.F90"

#include "ESMF_Macros.inc"
#include "ESMF.h"

module ESMF_FieldSMMFromFileUTestMod

  ! modules
  use ESMF_TestMod     ! test methods
  use ESMF_RegridWeightGenCheckMod ! test array SMM from file
  use ESMF_RegridWeightGenMod      ! test array SMM from file
  use ESMF
  
  implicit none

  public test_smm_from_file

  contains !--------------------------------------------------------------------

  subroutine test_smm_from_file(srcFile, dstFile, weightFile, checkMethod, rc)
    character(len=*), intent(in) :: srcFile, dstFile, weightFile
    integer, intent(out) :: rc
    type(ESMF_RWGCheckMethod_Flag), intent(in) :: checkMethod

    integer :: localrc

    rc = ESMF_FAILURE

    ! Generate the netCDF weights file.
    call ESMF_RegridWeightGenFile(srcFile, dstFile, weightFile, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return

    ! Validate generated weights produce acceptable errors. This subroutine
    ! calls ESMF_ArraySMMStoreFromFile or ESMF_FieldSMMStoreFromFile.
    call ESMF_RegridWeightGenCheck(weightFile, checkMethod=checkMethod, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return

    rc = ESMF_SUCCESS

  end subroutine test_smm_from_file

  subroutine test_regrid_store_from_file(rc)
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_VM) :: vm
  type(ESMF_Grid) :: srcGrid, dstGrid
  type(ESMF_Field) :: srcField, dstField
  type(ESMF_RouteHandle) :: routeHandle
  real(ESMF_KIND_R8), pointer :: src(:,:), dst(:,:)
  real(ESMF_KIND_R8) :: lon, lat, theta, phi
  real(ESMF_KIND_R8), pointer :: s_x(:,:), s_y(:,:), d_x(:,:), d_y(:,:)
  integer(ESMF_KIND_I4) :: lbnd(2), ubnd(2)
  integer :: localPet, petCount
  integer :: i, j, m, n
  real(ESMF_KIND_R8), pointer :: factorList(:) 
  integer(ESMF_KIND_I4), pointer :: factorIndexList(:,:) 

  ! init success flag
  correct=.true.
  rc=ESMF_FAILURE

  ! get pet info
  call ESMF_VMGetGlobal(vm, rc=localrc)
  if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
  if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME, rcToReturn=rc)) return

  m = 8
  n = 9

  srcGrid = ESMF_GridCreateNoPeriDim(maxIndex=(/n,n/), &
                                     coordSys=ESMF_COORDSYS_CART, &
                                     indexflag=ESMF_INDEX_GLOBAL, &
                                     rc=localrc)
  if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME, rcToReturn=rc)) return
  dstGrid = ESMF_GridCreateNoPeriDim(maxIndex=(/m,m/), &
                                     coordSys=ESMF_COORDSYS_CART, &
                                     indexflag=ESMF_INDEX_GLOBAL,  &
                                     rc=localrc)
  if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME, rcToReturn=rc)) return

  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME, rcToReturn=rc)) return
  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME, rcToReturn=rc)) return

  call ESMF_GridGetCoord(srcGrid, 1, staggerloc=ESMF_STAGGERLOC_CENTER, &
                         computationalLBound=lbnd, computationalUBound=ubnd, &
                         farrayPtr=s_x, rc=localrc)
  if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME, rcToReturn=rc)) return

  call ESMF_GridGetCoord(srcGrid, 2, staggerloc=ESMF_STAGGERLOC_CENTER, &
                         computationalLBound=lbnd, computationalUBound=ubnd, &
                         farrayPtr=s_y, rc=localrc)
  if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME, rcToReturn=rc)) return

  do i = lbnd(1), ubnd(1)
    do j = lbnd(2), ubnd(2)
        s_x(i, j) = i - 0.5
        s_y(i, j) = j - 0.5
    enddo
  enddo


  call ESMF_GridGetCoord(dstGrid, 1, staggerloc=ESMF_STAGGERLOC_CENTER, &
                         computationalLBound=lbnd, computationalUBound=ubnd, &
                         farrayPtr=d_x, rc=localrc)
  if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME, rcToReturn=rc)) return

  call ESMF_GridGetCoord(dstGrid, 2, staggerloc=ESMF_STAGGERLOC_CENTER, &
                         computationalLBound=lbnd, computationalUBound=ubnd, &
                         farrayPtr=d_y, rc=localrc)
  if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME, rcToReturn=rc)) return

    do i = lbnd(1), ubnd(1)
      do j = lbnd(2), ubnd(2)
          d_x(i, j) = i
          d_y(i, j) = j
      enddo
    enddo

  ! Create source/destination fields
   srcField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                               staggerloc=ESMF_STAGGERLOC_CENTER, &
                               name="source", rc=localrc)
  if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME, rcToReturn=rc)) return

   dstField = ESMF_FieldCreate(dstGrid, ESMF_TYPEKIND_R8, &
                               staggerloc=ESMF_STAGGERLOC_CENTER, &
                               name="dest", rc=localrc)
  if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME, rcToReturn=rc)) return


  ! Get arrays
  ! dstArray
  call ESMF_FieldGet(dstField, farrayPtr=dst, rc=localrc)
  if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME, rcToReturn=rc)) return
  dst = 0.

  ! srcArray
  call ESMF_FieldGet(srcField, farrayPtr=src, rc=localrc)
  if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME, rcToReturn=rc)) return
  src = 42.

  ! Do regrid
  call ESMF_FieldRegridStore(srcField, dstField, routehandle=routeHandle, &
    factorList=factorList, factorIndexList=factorIndexList, rc=localrc)
  if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME, rcToReturn=rc)) return

  call ESMF_SparseMatrixWrite(factorList, factorIndexList, &
                              "data/weights_esmf_smmsff.nc", rc=localrc)
  if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME, rcToReturn=rc)) return

  ! SMM store
  call ESMF_FieldSMMStore(srcField, dstField, "data/weights_esmf_smmsff.nc", &
                          routeHandle, rc=localrc)
  if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME, rcToReturn=rc)) return

  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME, rcToReturn=rc)) return

  do i = lbnd(1), ubnd(1)
    do j = lbnd(2), ubnd(2)
        if (dst(i, j) /= 42.) then 
          correct = .false.
        endif
    enddo
  enddo

  deallocate(factorList)
  deallocate(factorIndexList)

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME, rcToReturn=rc)) return

  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
  if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME, rcToReturn=rc)) return

   call ESMF_FieldDestroy(dstField, rc=localrc)
  if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME, rcToReturn=rc)) return

  ! Free the grids
  call ESMF_GridDestroy(srcGrid, rc=localrc)
  if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME, rcToReturn=rc)) return

  call ESMF_GridDestroy(dstGrid, rc=localrc)
  if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME, rcToReturn=rc)) return

  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

  end subroutine test_regrid_store_from_file

end module

!==============================================================================
!==============================================================================
!==============================================================================

program ESMF_FieldSMMFromFileUTest

!==============================================================================
!BOP
! !PROGRAM: ESMF_FieldSMMFromFileUTest - Tests FieldSMMFromFile()
!
! !DESCRIPTION:
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod ! test methods
  use ESMF

  use ESMF_FieldSMMFromFileUTestMod

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
  character(len=*), parameter :: weightFile = 'data/test_smm_from_file_weights.nc'
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
  write(name, *) "ESMF_FieldSMMFromFile Unit Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  call test_smm_from_file(srcFile, dstFile, weightFile, &
                          ESMF_RWGCHECKMETHOD_FIELD, rc)

#if defined ESMF_NETCDF
  call ESMF_Test((rc .eq. ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc .eq. ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_FieldRegridStoreFromFile Unit Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  call test_regrid_store_from_file(rc)

#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc .eq. ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc .eq. ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally

end program ESMF_FieldSMMFromFileUTest
