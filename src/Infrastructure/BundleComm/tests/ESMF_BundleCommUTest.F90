! $Id: ESMF_BundleCommUTest.F90,v 1.5 2005/06/27 22:11:44 nscollins Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2005, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
      program ESMF_BundleCommUTest

!------------------------------------------------------------------------------
#define ESMF_FILENAME "ESMF_BundleCommUTest.F90"
!
!
#include <ESMF.h>

!==============================================================================
!BOP
! !PROGRAM: ESMF_BundleCommUTest - Bundle Communication Unit Tests
!
! !DESCRIPTION:
!
! The code in this file drives F90 Bundle Communication unit tests.
! These include calls like Halo, Gather, Regrid, and Redistribution.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod

      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_BundleCommUTest.F90,v 1.5 2005/06/27 22:11:44 nscollins Exp $'
!------------------------------------------------------------------------------

!     ! Local variables
      integer :: rc
      type(ESMF_Grid) :: grid, grid2
      type(ESMF_VM) :: vm
      type(ESMF_Field) :: fields(4)
      type(ESMF_Field) :: nofield
      type(ESMF_Array) :: arrayList(2)
      type(ESMF_Bundle) :: bundle1, bundle2, bundle3, nobundle
      type(ESMF_RouteHandle) :: rh


      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name


!-------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------

      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

      !------------------------------------------------------------------------
      !NEX_UTest
      !  Create an empty bundle for use below.
      bundle2 = ESMF_BundleCreate(name="time step 1", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating Empty Bundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     
      !------------------------------------------------------------------------
      !NEX_UTest
      !  Create a data Field for use below.
      fields(1) = CreateDataField("humidity", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating Data Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !NEX_UTest
      !  Create another data Field for use below.
      !  Get grid from previous field and reuse it.
      call ESMF_FieldGet(fields(1), grid=grid, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting Grid from Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !NEX_UTest
      !  Create another data Field for use below.
      !  Get grid from previous field and reuse it.
      fields(2) = CreateDataField("pressure", grid=grid, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating Data Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !NEX_UTest
      !  Add fields to bundle
      call ESMF_BundleAddField(bundle2, 2, fields(1:2), rc=rc)
      write(failMsg, *) ""
      write(name, *) "Adding 2 Fields to Bundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !NEX_UTest
      !  BUG - if fields are empty this dies.
      call ESMF_BundleHaloStore(bundle2, rh, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Precompute Halo Communication"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !NEX_UTest
      !  Print out precomputed Route Table
      call ESMF_RouteHandlePrint(rh, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing out the contents of a Route Table"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !NEX_UTest
      !  Halo both fields in a single Bundle call.
      call ESMF_BundleHalo(bundle2, rh, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Haloing all Fields in a Bundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !NEX_UTest
      !  Release the routehandle
      call ESMF_BundleHaloRelease(rh, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Release the RouteHandle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !NEX_UTest
      ! Clean up by deleting the Fields.
      call ESMF_FieldDestroy(fields(1), rc=rc)
      write(failMsg, *) ""
      write(name, *) "Field Destroy Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Clean up by deleting the Fields.
      call ESMF_FieldDestroy(fields(2), rc=rc)
      write(failMsg, *) ""
      write(name, *) "Field Destroy Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Clean up by deleting the Bundle.
      call ESMF_BundleDestroy(bundle2, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Bundle Destroy Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

#ifdef ESMF_EXHAUSTIVE

      !------------------------------------------------------------------------

      ! set these up for use later
      nobundle = ESMF_BundleCreate()
      call ESMF_BundleDestroy(nobundle)
      nofield = ESMF_FieldCreateNoData()
      call ESMF_FieldDestroy(nofield)
      call ESMF_VMGetGlobal(vm, rc)

      !------------------------------------------------------------------------
      !EX_UTest
      !  Create an empty bundle for use below.
      bundle1 = ESMF_BundleCreate(name="time step 1", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating Empty Bundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     
      !------------------------------------------------------------------------
      !EX_UTest
      !  Create an empty bundle for use below.
      bundle2 = ESMF_BundleCreate(name="time step 2", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating Empty Bundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     
      !------------------------------------------------------------------------
      !EX_UTest
      !  Create a data Field for use below.
      fields(1) = CreateDataField("humidity", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating Data Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Create another data Field for use below.
      !  Get grid from previous field and reuse it.
      call ESMF_FieldGet(fields(1), grid=grid, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting Grid from Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Create another data Field for use below.
      !  Get grid from previous field and reuse it.
      fields(2) = CreateDataField("pressure", grid=grid, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating Data Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Add fields to bundle
      call ESMF_BundleAddField(bundle1, 2, fields(1:2), rc=rc)
      write(failMsg, *) ""
      write(name, *) "Adding 2 Fields to Bundle 1"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Add fields to bundle
      call ESMF_BundleAddField(bundle2, 2, fields(1:2), rc=rc)
      write(failMsg, *) ""
      write(name, *) "Adding 2 Fields to Bundle 2"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_BundleRedistStore(bundle1, bundle2, vm, rh, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Precompute Redist Communication"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Print out precomputed Route Table
      call ESMF_RouteHandlePrint(rh, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing out the contents of a Route Table"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Redist both fields in a single Bundle call.
      call ESMF_BundleRedist(bundle1, bundle2, rh, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Redisting all Fields in a Bundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Release the routehandle
      call ESMF_BundleRedistRelease(rh, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Release the RouteHandle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !EX_UTest
      !  Create another data Field for use below.
      !  Get grid from previous field and reuse it.
      fields(3) = CreateDataField2("pressure", rvalue=9.9_ESMF_KIND_R8, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating Data Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Create another data Field for use below.
      call ESMF_FieldGet(fields(3), grid=grid2, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting Grid from Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Create another data Field for use below.
      !  Get grid from previous field and reuse it.
      fields(4) = CreateDataField2("pressure", grid2, -1.0_ESMF_KIND_R8, rc)
      write(failMsg, *) ""
      write(name, *) "Creating Data Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Create an empty bundle for use below.
      bundle3 = ESMF_BundleCreate(2, fields(3:4), name="time step 3", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating Bundle w/ 2 Fields Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     
      !------------------------------------------------------------------------
      !E-X_UTest
      !call ESMF_BundleRegridStore(bundle1, bundle3, vm, rh, &
      !                            ESMF_REGRID_METHOD_BILINEAR, rc=rc)
      !write(failMsg, *) ""
      !write(name, *) "Precompute Regrid Communication"
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Print out precomputed Route Table
      !call ESMF_RouteHandlePrint(rh, rc=rc)
      !write(failMsg, *) ""
      !write(name, *) "Printing out the contents of a Route Table"
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !E-X_UTest
      ! TODO: Fails. File bug on this.
      !  Regrid both fields in a single Bundle call.
      !call ESMF_BundleRegrid(bundle1, bundle3, rh, rc=rc)
      !write(failMsg, *) ""
      !write(name, *) "Regriding all Fields in a Bundle"
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Release the routehandle
      !call ESMF_BundleRegridRelease(rh, rc=rc)
      !write(failMsg, *) ""
      !write(name, *) "Release the RouteHandle"
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_BundleGather(bundle1, 1, arrayList, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Gathering Bundle Array data to a single processor"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !EX_UTest
      ! Clean up by deleting the Fields.
      call ESMF_FieldDestroy(fields(1), rc=rc)
      write(failMsg, *) ""
      write(name, *) "Field 1 Destroy Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Clean up by deleting the Fields.
      call ESMF_FieldDestroy(fields(2), rc=rc)
      write(failMsg, *) ""
      write(name, *) "Field 2 Destroy Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Clean up by deleting the Fields.
      call ESMF_FieldDestroy(fields(3), rc=rc)
      write(failMsg, *) ""
      write(name, *) "Field 3 Destroy Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Clean up by deleting the Fields.
      call ESMF_FieldDestroy(fields(4), rc=rc)
      write(failMsg, *) ""
      write(name, *) "Field 4 Destroy Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Clean up by deleting the Bundle.
      call ESMF_BundleDestroy(bundle1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Bundle 1 Destroy Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Clean up by deleting the Bundle.
      call ESMF_BundleDestroy(bundle2, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Bundle 2 Destroy Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Clean up by deleting the Bundle.
      call ESMF_BundleDestroy(bundle3, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Bundle 3 Destroy Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#endif

      call ESMF_TestEnd(result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

contains

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "CreateDataField"
function CreateDataField(name, grid, rvalue, rc)
  type(ESMF_Field) :: CreateDataField

  character(len=*), intent(in) :: name                ! field name
  type(ESMF_Grid), intent(in), optional :: grid       ! if set, grid to use
  real(ESMF_KIND_R8), intent(in), optional :: rvalue  ! if set, initial value
  integer, intent(out), optional :: rc                ! return code

  type(ESMF_Grid) :: newgrid
  type(ESMF_ArraySpec) :: as
  type(ESMF_VM) :: vm
  type(ESMF_DELayout) :: lay
  integer :: status
  real(ESMF_KIND_R8), pointer :: rdata(:,:)

  rc = ESMF_FAILURE

  call ESMF_VMGetGlobal(vm, rc=status)
  if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rc)) goto 10

  lay = ESMF_DELayoutCreate(vm, rc=status)
  if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rc)) goto 10

  if (.not. present(grid)) then
      newgrid = ESMF_GridCreateHorzXYUni(counts=(/100, 200/), &
                               minGlobalCoordPerDim=(/0.0_ESMF_KIND_R8, &
                                                      0.0_ESMF_KIND_R8/), &
                               maxGlobalCoordPerDim=(/180.0_ESMF_KIND_R8, &
                                                      360.0_ESMF_KIND_R8/), &
                               rc=status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10
      call ESMF_GridDistribute(newgrid, delayout=lay, rc=status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10

  else
      newgrid = grid
  endif

  call ESMF_ArraySpecSet(as, rank=2, type=ESMF_DATA_REAL, &
                         kind=ESMF_R8, rc=status)
  if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rc)) goto 10

  CreateDataField = ESMF_FieldCreate(grid=newgrid, arrayspec=as, &
                    horzRelloc=ESMF_CELL_CENTER, haloWidth=2, &
                    name=name, rc=status)
  if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rc)) goto 10

  call ESMF_FieldGetDataPointer(CreateDataField, rdata, ESMF_DATA_REF, rc)
  if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rc)) goto 10

  if (present(rvalue)) then
      rdata = rvalue
  else
      rdata = 3.14159
  endif
  
10 continue
  ! rc will have been set by the call to logerr; 
  ! just return at this point

end function CreateDataField

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "CreateDataField2"
function CreateDataField2(name, grid, rvalue, rc)
  type(ESMF_Field) :: CreateDataField2

  character(len=*), intent(in) :: name                ! field name
  type(ESMF_Grid), intent(in), optional :: grid       ! optional precompute grid
  real(ESMF_KIND_R8), intent(in), optional :: rvalue  ! optional initial value
  integer, intent(out), optional :: rc                ! return code

  type(ESMF_Grid) :: newgrid
  type(ESMF_ArraySpec) :: as
  type(ESMF_VM) :: vm
  type(ESMF_DELayout) :: lay
  integer :: npets, status
  real(ESMF_KIND_R8), pointer :: rdata(:,:)

  rc = ESMF_FAILURE

  call ESMF_VMGetGlobal(vm, rc=status)
  if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rc)) goto 10

  call ESMF_VMGet(vm, petCount=npets, rc=status)
  if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rc)) goto 10

  lay = ESMF_DELayoutCreate(vm, (/ npets, 1 /), rc=status)
  if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rc)) goto 10

  if (.not. present(grid)) then
      newgrid = ESMF_GridCreateHorzXYUni(counts=(/500, 100/), &
                               minGlobalCoordPerDim=(/1.0_ESMF_KIND_R8, &
                                                      1.0_ESMF_KIND_R8/), &
                               maxGlobalCoordPerDim=(/179.0_ESMF_KIND_R8, &
                                                      359.0_ESMF_KIND_R8/), &
                               rc=status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10
      call ESMF_GridDistribute(newgrid, delayout=lay, rc=status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10

  else
      newgrid = grid
  endif

  call ESMF_ArraySpecSet(as, rank=2, type=ESMF_DATA_REAL, &
                         kind=ESMF_R8, rc=status)
  if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rc)) goto 10

  CreateDataField2 = ESMF_FieldCreate(grid=newgrid, arrayspec=as, &
                                    horzRelloc=ESMF_CELL_CENTER, haloWidth=2, &
                                    name=name, rc=status)
  if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rc)) goto 10

  call ESMF_FieldGetDataPointer(CreateDataField2, rdata, ESMF_DATA_REF, rc)
  if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rc)) goto 10

  if (present(rvalue)) then
      rdata = rvalue
  else
      rdata = 3.14159
  endif
  
10 continue
  ! rc will have been set by the call to logerr; 
  ! just return at this point

end function CreateDataField2

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "CreateEmptyDataField"
function CreateEmptyDataField(name, rc)
  type(ESMF_Field) :: CreateEmptyDataField

  character(len=*), intent(in) :: name
  integer, intent(out) :: rc

  integer :: status

  rc = ESMF_FAILURE
  CreateEmptyDataField = ESMF_FieldCreateNoData(name=name, rc=status)
  if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rc)) goto 10

10 continue
  ! rc will have been set by the call to logerr; 
  ! just return at this point

end function CreateEmptyDataField

!------------------------------------------------------------------------------
      end program ESMF_BundleCommUTest
