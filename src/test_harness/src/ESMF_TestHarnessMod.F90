! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!===============================================================================
#define ESMF_FILENAME "ESMF_TestHarnessMod"
!
!  ESMF Test Harness Module
   module ESMF_TestHarnessMod
!
!===============================================================================
!
! This file contains parameters, global data types, and parser 
! functions/subroutines for the Testing Harness.
! These methods are used by the test harness driver ESMF_TestHarnessUTest.F90.
!
!-------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!===============================================================================
!BOPI
! !MODULE: ESMF_TestHarnessMod
!
! !DESCRIPTION:
!
! The code in this file contains data types and basic functions for the
! {\tt ESMF\_TestHarness}.  
!  Expand on the type of routines included here
!
!-------------------------------------------------------------------------------
!
! !USES:

  use ESMF_TestHarnessReportMod

  use ESMF
  use ESMF_RegridMod
  use ESMF_FieldMod
  use ESMF_GridUtilMod

  use ESMF_FieldGetMod

  implicit none
  private

!-------------------------------------------------------------------------------
! PUBLIC METHODS:
!-------------------------------------------------------------------------------
  public array_redist_test, field_regrid_test, field_redist_test

!===============================================================================
! debug trace switch
logical                       :: checkpoint = .FALSE.

  contains 

!===============================================================================


!-------------------------------------------------------------------------------
!===============================================================================
! Public Methods
!===============================================================================
  !-----------------------------------------------------------------------------
  subroutine array_redist_test(PDS, test_failure, reportType, VM, rc)
  !-----------------------------------------------------------------------------
  !
  !-----------------------------------------------------------------------------
  ! arguments
  type(problem_descriptor_strings), intent(inout) :: PDS
  character(THARN_MAXSTR), intent(in   ) :: reportType 
  type(ESMF_VM), intent(in   ) :: VM
  integer, intent(inout) :: test_failure
  integer, intent(  out) :: rc

  ! local parameters
  real(ESMF_KIND_R8), parameter :: initvalue = 0.0

  ! local ESMF Types
  type(ESMF_Array) :: src_array, return_array, dst_array
  type(ESMF_DistGrid) :: src_distgrid, dst_distgrid
  type(ESMF_RouteHandle) :: redistHandle_forward, redistHandle_reverse

  ! local integers
  integer :: localrc ! local error status
  integer :: iDfile, iGfile, iD, iG
  integer :: test_status

  integer :: localPET

  ! local characters
  character(THARN_MAXSTR) :: liG, liD

  ! debug
  ! real(ESMF_KIND_R8), pointer :: farrayPtr2(:,:)
  ! integer ::de, localDeCount, dimCount 
  ! integer :: i1, i2
  ! integer, allocatable ::  localDeToDeMap(:)
  ! type(ESMF_LocalArray), allocatable :: larrayList(:)
  ! integer, allocatable :: LBnd(:,:), UBnd(:,:) 
  ! type(ESMF_Index_Flag) :: indexflag

  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL
  rc = ESMF_RC_NOT_IMPL

  ! initialize test counter
  test_failure = 0

  call ESMF_VMGet(VM, localPet=localPET, rc=localrc)
  if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"can not get local pet ",          &
                  rcToReturn=rc)) return

  !-----------------------------------------------------------------------------
  ! for a single problem descriptor string, loop through each specifier file
  ! combination
  !-----------------------------------------------------------------------------
  if (localPet == 0) then
    print*,'-----------------======array redist test==========-----------------------'
  end if

  do iDfile=1,PDS%nDfiles         ! distribution specifier files
    do iGfile=1,PDS%nGfiles       ! grid specifier files
      do iD=1, PDS%Dfiles(iDfile)%nDspecs   ! entries in distribution specifier
        do iG=1, PDS%Gfiles(iGfile)%nGspecs ! entries in grid specifier file
          write(liG,"(i5)") iG 
          write(liD,"(i5)") iD

          if (localPet == 0) then
            print '(A)',"array redist test - "  &
               // " with string "  // trim(adjustL(PDS%pds)) //                  &
               " with entry "  // trim(adjustL(liD)) // " of file " //           &
               trim(adjustL(PDS%Dfiles(iDfile)%filename))                        &
               // " and entry " // trim(adjustL(liG)) // " of file " //          &
               trim(adjustL(PDS%Gfiles(iGfile)%filename))
          end if

          !---------------------------------------------------------------------
          ! create source and destination distributions
          !---------------------------------------------------------------------
          call create_distribution(PDS%SrcMem, PDS%Dfiles(iDfile)%src_dist(iD),&
                    PDS%Gfiles(iGfile)%src_grid(iG), src_distgrid, VM, localrc)

          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error creating source distgrid "  &
             // " with string "  // trim(adjustL(PDS%pds)) //                  &
             " with entry "  // trim(adjustL(liD)) // " of file " //           &
             trim(adjustL(PDS%Dfiles(iDfile)%filename))                        &
             // " and entry " // trim(adjustL(liG)) // " of file " //          &
             trim(adjustL(PDS%Gfiles(iGfile)%filename)),                       &
             rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! create source array
          !---------------------------------------------------------------------
          call create_array(src_array, src_distgrid, PDS%SrcMem,               &
                      PDS%Gfiles(iGfile)%src_grid(iG), localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error creating source array "     &
             // " with string "  // trim(adjustL(PDS%pds)) //                  &
             " with entry "  // trim(adjustL(liD)) // " of file " //           &
             trim(adjustL(PDS%Dfiles(iDfile)%filename))                        &
             // " and entry " // trim(adjustL(liG)) // " of file " //          &
             trim(adjustL(PDS%Gfiles(iGfile)%filename)),                       &
             rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! populate source array for redistribution test
          !---------------------------------------------------------------------
          call populate_redist_array(src_array, src_distgrid, PDS%SrcMem,      &
                       PDS%Gfiles(iGfile)%src_grid(iG), localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error populating source array ",  &
                  rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! create return array
          !---------------------------------------------------------------------
          call create_array(return_array, src_distgrid, PDS%SrcMem,            &
                      PDS%Gfiles(iGfile)%src_grid(iG), localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error creating return array "     &
             // " with string "  // trim(adjustL(PDS%pds)) //                  &
             " with entry "  // trim(adjustL(liD)) // " of file " //           &
             trim(adjustL(PDS%Dfiles(iDfile)%filename))                        &
             // " and entry " // trim(adjustL(liG)) // " of file " //          &
             trim(adjustL(PDS%Gfiles(iGfile)%filename)),                       &
             rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! populate return array with zeros for redistribution test
          !---------------------------------------------------------------------
          call populate_array_value(return_array, initvalue, src_distgrid,     &
                  PDS%SrcMem, PDS%Gfiles(iGfile)%src_grid(iG), localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error initializing value in " //  &
             "return array ", rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! Create Destination distribution and array
          !---------------------------------------------------------------------
          call create_distribution(PDS%DstMem, PDS%Dfiles(iDfile)%dst_dist(iD),&
                    PDS%Gfiles(iGfile)%dst_grid(iG), dst_distgrid, VM, localrc)
          write(liG,"(i5)") iG 
          write(liD,"(i5)") iD 
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error creating source distgrid "  &
             // " with string "  // trim(adjustL(PDS%pds)) //                  &
             " with entry "  // trim(adjustL(liD)) // " of file " //           &
             trim(adjustL(PDS%Dfiles(iDfile)%filename))                        &
             // " and entry " // trim(adjustL(liG)) // " of file " //          &
             trim(adjustL(PDS%Gfiles(iGfile)%filename)),                       &
             rcToReturn=rc)) return

          call create_array(dst_array, dst_distgrid, PDS%DstMem,               &
                      PDS%Gfiles(iGfile)%dst_grid(iG), localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error creating destinationarray " &
             // " with string "  // trim(adjustL(PDS%pds)) //                  &
             " with entry "  // trim(adjustL(liD)) // " of file " //           &
             trim(adjustL(PDS%Dfiles(iDfile)%filename))                        &
             // " and entry " // trim(adjustL(liG)) // " of file " //          &
             trim(adjustL(PDS%Gfiles(iGfile)%filename)),                       &
             rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! populate destination array with zeros for redistribution test
          !---------------------------------------------------------------------
          call populate_array_value(dst_array, initvalue, dst_distgrid,        &
                  PDS%DstMem, PDS%Gfiles(iGfile)%dst_grid(iG), localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error initializing value in " //  &
             "destination array ", rcToReturn=rc)) return

  !-----------------------------------------------------------------------------
  ! Now conduct the redistribution test
  !
  ! the test consists of a forward redistribution from the source
  ! distribution to the destination distribution, and a second backward
  ! redistribution from the destination back to the source distribution.
  !-----------------------------------------------------------------------------
          !---------------------------------------------------------------------
          ! redistribution store for forward direction
          !---------------------------------------------------------------------
          call ESMF_ArrayRedistStore(srcArray=src_array, dstArray=dst_array,   &
                   routehandle=redistHandle_forward, rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"Array redist store failed for" // &
                  " forward direction", rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! forward redistribution run
          !---------------------------------------------------------------------
          call ESMF_ArrayRedist(srcArray=src_array, dstArray=dst_array,        &
                   routehandle=redistHandle_forward, rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"Array redist run failed for " //  &
                  " forward failed ", rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! release redistribution handles
          !---------------------------------------------------------------------
          call ESMF_ArrayRedistRelease(routehandle=redistHandle_forward,       &
                   rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"redistribution release for" //    &
                  " forward failed ", rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! redistribution store for reverse direction
          !---------------------------------------------------------------------
          call ESMF_ArrayRedistStore(srcArray=dst_array, dstArray=return_array,&
                   routehandle=redistHandle_reverse, rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"Array redist store failed for" // &
                  " reverse direction", rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! forward redistribution run
          !---------------------------------------------------------------------
          call ESMF_ArrayRedist(srcArray=dst_array, dstArray=return_array,     &
                   routehandle=redistHandle_reverse, rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"Array redist run failed for " //  &
                  " reverse failed ", rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! release redistribution handles
          !---------------------------------------------------------------------
          call ESMF_ArrayRedistRelease(routehandle=redistHandle_reverse,       &
                   rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"redistribution release for" //    &
                  " reverse failed ", rcToReturn=rc)) return

  !-----------------------------------------------------------------------------
  ! Check redistribution
  !-----------------------------------------------------------------------------
          !---------------------------------------------------------------------
          ! compare source array values with the return array values
          !---------------------------------------------------------------------
          call compare_redist_array(test_status,                               & 
                  src_array, return_array, src_distgrid, src_distgrid,         &
                  PDS%SrcMem, PDS%Gfiles(iGfile)%src_grid(iG), localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"redistribution array " //         &
                  " comparison failed ", rcToReturn=rc)) return

! gblock - do we want to return on the compare failure?  How about internal ESMF error?
!   suspect already HarnessTest_FAILURE reported on compare error, ESMF_xxx on ESMF error

          PDS%test_record(iDfile,iGfile)%test_status(iD,iG) = test_status

          if( test_status == HarnessTest_FAILURE ) then 
             test_failure = test_failure + 1
          endif

          call report_descriptor_string(PDS, iG, iD, iGfile, iDfile,           &
                                        reportType, localPET, localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"redistribution array " //         &
                  " test report failed ", rcToReturn=rc)) return

  !-----------------------------------------------------------------------------
  ! Clean up!!!!!!
  !-----------------------------------------------------------------------------
          !---------------------------------------------------------------------
          ! Destroy Array objects before moving to next test
          !---------------------------------------------------------------------
          call ESMF_ArrayDestroy(src_array, rc=localrc) ! original source
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"unable to destroy src_array",     &
             rcToReturn=rc)) return

          call ESMF_ArrayDestroy(return_array, rc=localrc) ! return to source
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"unable to destroy return_array",  &
             rcToReturn=rc)) return

          call ESMF_ArrayDestroy(dst_array, rc=localrc) ! redistribution 
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"unable to destroy dst_array   ",  &
             rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! Destroy DistGrid objects before running next test
          !---------------------------------------------------------------------
          call ESMF_DistGridDestroy(src_distgrid, rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"unable to destroy src_distgrid",  &
             rcToReturn=rc)) return

          call ESMF_DistGridDestroy(dst_distgrid, rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"unable to destroy src_distgrid",  &
             rcToReturn=rc)) return

          !---------------------------------------------------------------------

        enddo  ! iG
      enddo  ! iD
    enddo  ! iGfile
  enddo   ! iDfile
  !-----------------------------------------------------------------------------
  ! if I've gotten this far without an error, then the routine has succeeded.
  !-----------------------------------------------------------------------------
  rc = ESMF_SUCCESS
  
  if (localPet == 0) then
      print*,'Array Redist Completed'
  end if

  !-----------------------------------------------------------------------------
  end subroutine array_redist_test
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine field_regrid_test(PDS, test_failure, reportType, VM, rc)
  !-----------------------------------------------------------------------------
  ! routine conducts the field regrid test
  !-----------------------------------------------------------------------------
  ! arguments
  type(problem_descriptor_strings), intent(inout) :: PDS
  character(THARN_MAXSTR), intent(in   ) :: reportType 
  type(ESMF_VM), intent(in   ) :: VM
  integer, intent(inout) :: test_failure
  integer, intent(  out) :: rc

  ! local parameters
  real(ESMF_KIND_R8), parameter :: initvalue = 0.0

  ! local ESMF Types
  type(ESMF_Grid) :: gridSrc
  type(ESMF_Grid) :: gridDst
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_ArraySpec) :: SrcArraySpec
  type(ESMF_ArraySpec) :: DstArraySpec
  type(ESMF_DistGrid) :: src_distgrid, dst_distgrid
  type(ESMF_RouteHandle) :: routeHandle

  ! local integers
  integer :: localrc ! local error status
  integer :: iDfile, iGfile, iD, iG, ix
  integer :: test_status
  integer :: localPET
  integer :: libflag

  ! local characters
  character(THARN_MAXSTR) :: liG, liD

  ! debug
  ! real(ESMF_KIND_R8), pointer :: farrayPtr2(:,:)
  ! integer :: i1, i2, de, localDeCount, dimCount 
  ! integer, allocatable ::  localDeToDeMap(:)
  ! type(ESMF_LocalArray), allocatable :: larrayList(:)
  ! integer, allocatable :: LBnd(:,:), UBnd(:,:) 
  ! type(ESMF_Index_Flag) :: indexflag

  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL
  rc = ESMF_RC_NOT_IMPL

  ! initialize test counter
  test_failure = 0

  call ESMF_VMGet(VM, localPet=localPET, rc=localrc)
  if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"can not get local pet ", rcToReturn=rc)) return

  !-----------------------------------------------------------------------------
  ! for a single problem descriptor string, loop through each specifier file
  ! combination
  ! Create source and destination distributions, Fields and conduct regrid
  !-----------------------------------------------------------------------------
  if (localPet == 0) then
      print*,'-----------------======field regrid test==========-----------------------'
  end if

  do iDfile=1,PDS%nDfiles         ! distribution specifier files
    do iGfile=1,PDS%nGfiles       ! grid specifier files
      do iD=1, PDS%Dfiles(iDfile)%nDspecs   ! entries in distribution specifier
        do iG=1, PDS%Gfiles(iGfile)%nGspecs ! entries in grid specifier file
          write(liG,"(i5)") iG 
          write(liD,"(i5)") iD 

          if (localPet == 0) then
            print '(A)', "field regrid"  &
               // " with string " // trim(adjustL(PDS%pds)) //                   &
               " with entry "  // trim(adjustL(liD)) // " of file " //           &
               trim(adjustL(PDS%Dfiles(iDfile)%filename))                        &
               // " and entry " // trim(adjustL(liG)) // " of file " //          &
               trim(adjustL(PDS%Gfiles(iGfile)%filename))
          end if

          !---------------------------------------------------------------------
          ! create source distribution
          !---------------------------------------------------------------------
          call create_distribution(PDS%SrcMem, PDS%Dfiles(iDfile)%src_dist(iD),&
                    PDS%Gfiles(iGfile)%src_grid(iG), src_distgrid, VM, localrc)

          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error creating source distgrid "  &
             // " with string "  // trim(adjustL(PDS%pds)) //                  &
             " with entry "  // trim(adjustL(liD)) // " of file " //           &
             trim(adjustL(PDS%Dfiles(iDfile)%filename))                        &
             // " and entry " // trim(adjustL(liG)) // " of file " //          &
             trim(adjustL(PDS%Gfiles(iGfile)%filename)),                       &
             rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! create source grid from from distribution
          !---------------------------------------------------------------------
          call create_grid_from_distgrid(gridSrc, src_distgrid, PDS%SrcMem,    &
                      PDS%Gfiles(iGfile)%src_grid(iG),  &
                      PDS%Dfiles(iDfile)%src_dist(iD), localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error creating source array "     &
             // " with string "  // trim(adjustL(PDS%pds)) //                  &
             " with entry "  // trim(adjustL(liD)) // " of file " //           &
             trim(adjustL(PDS%Dfiles(iDfile)%filename))                        &
             // " and entry " // trim(adjustL(liG)) // " of file " //          &
             trim(adjustL(PDS%Gfiles(iGfile)%filename)),                       &
             rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! create array spec
          !---------------------------------------------------------------------
          !---------------------------------------------------------------------
          ! set the dimensionality of actual data storage to the memory size 
          ! specified by the problem descriptor string
          !---------------------------------------------------------------------
          call ESMF_ArraySpecSet(SrcArraySpec, typekind=ESMF_TYPEKIND_R8,      &
                         rank=PDS%SrcMem%memRank, rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error creating ArraySpecSet",     &
                         rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! create source field from grid and arrayspec
          !---------------------------------------------------------------------
          srcField = ESMF_FieldCreate(grid=gridSrc, arrayspec=SrcArraySpec,    &
                  staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error creating source field",     &
                  rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! populate src field with test function for regridding test
          !---------------------------------------------------------------------
          call populate_field(srcField, gridSrc, PDS%SrcMem,                   &
                   PDS%Gfiles(iGfile)%src_grid(iG),                            &
                   PDS%Gfiles(iGfile)%testfunction(iG), localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error initializing value in " //  &
             "source array ", rcToReturn=rc)) return

!-------------------------------------------------------------------------------
! Destination
!-------------------------------------------------------------------------------
          !print*,'field regrid create destination'
          !---------------------------------------------------------------------
          ! Create Destination distribution
          !---------------------------------------------------------------------
          call create_distribution(PDS%DstMem, PDS%Dfiles(iDfile)%dst_dist(iD),&
                    PDS%Gfiles(iGfile)%dst_grid(iG), dst_distgrid, VM, localrc)
          write(liG,"(i5)") iG 
          write(liD,"(i5)") iD 
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error creating source distgrid "  &
             // " with string "  // trim(adjustL(PDS%pds)) //                  &
             " with entry "  // trim(adjustL(liD)) // " of file " //           &
             trim(adjustL(PDS%Dfiles(iDfile)%filename))                        &
             // " and entry " // trim(adjustL(liG)) // " of file " //          &
             trim(adjustL(PDS%Gfiles(iGfile)%filename)),                       &
             rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! create destination grid from from distribution
          !---------------------------------------------------------------------
          call create_grid_from_distgrid(gridDst, dst_distgrid, PDS%DstMem,    &
                      PDS%Gfiles(iGfile)%dst_grid(iG),    &
                      PDS%Dfiles(iDfile)%dst_dist(iD), localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error creating source array "     &
             // " with string "  // trim(adjustL(PDS%pds)) //                  &
             " with entry "  // trim(adjustL(liD)) // " of file " //           &
             trim(adjustL(PDS%Dfiles(iDfile)%filename))                        &
             // " and entry " // trim(adjustL(liG)) // " of file " //          &
             trim(adjustL(PDS%Gfiles(iGfile)%filename)),                       &
             rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! create array spec
          !---------------------------------------------------------------------
          !---------------------------------------------------------------------
          ! set the dimensionality of actual data storage to the memory size
          ! specified by the problem descriptor string
          !---------------------------------------------------------------------
          call ESMF_ArraySpecSet(DstArraySpec, typekind=ESMF_TYPEKIND_R8,      &
                         rank=PDS%DstMem%memRank, rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error creating dst ArraySpecSet", &
                         rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! create source field from grid and arrayspec
          !---------------------------------------------------------------------
          dstField = ESMF_FieldCreate(gridDst, DstArraySpec,                   &
                  staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error creating dst field",        &
                  rcToReturn=rc)) return

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !!!!!! NEED TO FIX THIS WHEN GRID AND MESH RECOGNIZE UNITS !!!!!!!
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  ! check that the grid units agree. If they don't log an error. Eventually the 
  ! mesh class should be able to reconcile different units.
  !-----------------------------------------------------------------------------
         do ix=1, PDS%Gfiles(iGfile)%src_grid(iG)%grank
           if( trim(PDS%Gfiles(iGfile)%src_grid(iG)%gunits(ix)%string) .ne.    &
               trim(PDS%Gfiles(iGfile)%dst_grid(iG)%gunits(ix)%string) ) then

               print*,'ERROR: source and destination grid units do not agree' 
               print*,'Source units are: ',                                    &
                 trim(PDS%Gfiles(iGfile)%src_grid(iG)%gunits(ix)%string),      &
                 ' while destination units are: ',                             &
                 trim(PDS%Gfiles(iGfile)%dst_grid(iG)%gunits(ix)%string)

                call ESMF_LogSetError( ESMF_FAILURE, msg="Incompatible grid" //  &
                       " units.",rcToReturn=localrc)
                return
           endif
         enddo      ! ix

  !-----------------------------------------------------------------------------
  ! Now conduct the interpolation
  !
  ! the test consists of interpolating an analytical test function evaluated
  ! on a source grid onto a destination grid, and checking that the result 
  ! agrees with the analytical solution to within a set tolerance.
  !-----------------------------------------------------------------------------
          !print*,'field regrid store'
          !---------------------------------------------------------------------
          ! select the correct regridding method and do a Field Regrid store
          !---------------------------------------------------------------------
          select case( PDS%process%tag )
             case( Harness_BilinearRegrid )
                call ESMF_FieldRegridStore(srcField, dstField=dstField, routeHandle=routeHandle,    &
                        regridmethod=ESMF_REGRIDMETHOD_BILINEAR, rc=localrc)
                if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"Field Bilinear Regrid " //  &
                        "store failed", rcToReturn=rc)) return

             case( Harness_PatchRegrid )
                libflag = 0  ! flag to catch if framework built w/ LAPACK and BLAS libs

#ifdef ESMF_LAPACK
                libflag = 1  ! 
                call ESMF_FieldRegridStore(srcField, dstField=dstField, routeHandle=routeHandle,    &
                        regridmethod=ESMF_REGRIDMETHOD_PATCH, rc=localrc)
                if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"Field Patch Regrid " // &
                        "store failed", rcToReturn=rc)) return
#endif

                if(libflag==0) call ESMF_LogSetError( ESMF_FAILURE, msg="Patch " // &
                       "regridding requires LAPACK & BLAS libraries. These" // &
                       " libraries appear not to have been set. The "//        &
                       " environment variable ESMF_LAPACK must be set ON," //  &
                       " and ESMF_LAPACK_LIBS set to the LAPACK and BLAS "//   &
                       " library paths",rcToReturn=localrc)

             case( Harness_ConservRegrid )
                call ESMF_LogSetError( ESMF_FAILURE, msg="Conservative " //      &
                       "regridding not currently supported",rcToReturn=localrc)
                return
             case( Harness_2ndConservRegrid )
                call ESMF_LogSetError( ESMF_FAILURE, msg="Conservative " //      &
                       "regridding not currently supported",rcToReturn=localrc)
                return
             case( Harness_NearNeighRegrid )
                call ESMF_LogSetError( ESMF_FAILURE, msg="Nearest Neighbor " //  &
                       "regridding not currently supported",rcToReturn=localrc)
                return
             case( Harness_Error )
               ! error - invalid regrid method
                call ESMF_LogSetError( ESMF_FAILURE, msg="Invalid regrid " //    &
                       "method.",rcToReturn=localrc)
             case default
               ! error
                call ESMF_LogSetError( ESMF_FAILURE, msg="Invalid regrid " //    &
                       "method.",rcToReturn=localrc)
          end select

          !---------------------------------------------------------------------
          ! regrid run
          !---------------------------------------------------------------------
          !print*,'field regrid run'
          call ESMF_FieldRegrid(srcField, dstField, routeHandle=routeHandle, rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"Field Regrid run failed for " //  &
                  " forward failed ", rcToReturn=rc)) return

  !-----------------------------------------------------------------------------
  ! Check regrid     
  !-----------------------------------------------------------------------------
          !print*,'check field regrid'
          !---------------------------------------------------------------------
          ! compare interpolated array values with the exact solution
          !---------------------------------------------------------------------
          call check_field(test_status, dstField, gridDst,                     &
                           PDS%Gfiles(iGfile)%dst_grid(iG),                    &
                           PDS%Gfiles(iGfile)%testfunction(iG), localrc) 
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"redistribution array " //         &
                  " comparison failed ", rcToReturn=rc)) return

          PDS%test_record(iDfile,iGfile)%test_status(iD,iG) = test_status

          if( test_status == HarnessTest_FAILURE ) then 
             test_failure = test_failure + 1
          endif

          call report_descriptor_string(PDS, iG, iD, iGfile, iDfile,           &
                                        reportType, localPET, localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"redistribution array " //         &
                  " test report failed ", rcToReturn=rc)) return

  !-----------------------------------------------------------------------------
  ! Clean up!!!
  !-----------------------------------------------------------------------------
          !---------------------------------------------------------------------
          ! release handles
          !---------------------------------------------------------------------
          call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"Regrid routehandle Release failed",&
                 rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! release Fields
          !---------------------------------------------------------------------
          call ESMF_FieldDestroy(srcField, rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"SRC Field Regrid Release failed", &
                 rcToReturn=rc)) return

          call ESMF_FieldDestroy(dstField, rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"DST Field Regrid Release failed", &
                 rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! release Grids
          !---------------------------------------------------------------------
          call ESMF_GridDestroy(gridSrc, rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"SRC grid Release failed",         &
                 rcToReturn=rc)) return

          call ESMF_GridDestroy(gridDst, rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"DST Grid Release failed",         &
                 rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! Destroy DistGrid objects before running next test
          !---------------------------------------------------------------------
          call ESMF_DistGridDestroy(src_distgrid, rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"unable to destroy src_distgrid",  &
             rcToReturn=rc)) return

          call ESMF_DistGridDestroy(dst_distgrid, rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"unable to destroy src_distgrid",  &
             rcToReturn=rc)) return


          !---------------------------------------------------------------------

        enddo  ! iG
      enddo  ! iD
    enddo  ! iGfile
  enddo   ! iDfile
  !-----------------------------------------------------------------------------
  ! if I've gotten this far without an error, then the routine has succeeded.
  !-----------------------------------------------------------------------------
  rc = ESMF_SUCCESS
  
  if (localPet == 0) then
      print*,'Regrid Completed'
  end if

  !-----------------------------------------------------------------------------
  end subroutine field_regrid_test
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine field_redist_test(PDS, test_failure, reportType, VM, rc)
  !-----------------------------------------------------------------------------
  ! routine conducts the field redist test by redistributing from a source 
  ! field to a destination and back again to the return field
  !-----------------------------------------------------------------------------
  ! arguments
  type(problem_descriptor_strings), intent(inout) :: PDS
  character(THARN_MAXSTR), intent(in   ) :: reportType 
  type(ESMF_VM), intent(in   ) :: VM
  integer, intent(inout) :: test_failure
  integer, intent(  out) :: rc

  ! local parameters
  real(ESMF_KIND_R8), parameter :: initvalue = 0.0

  ! local ESMF Types
  type(ESMF_Grid) :: gridSrc
  type(ESMF_Grid) :: gridReturn
  type(ESMF_Grid) :: gridDst
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: returnField
  type(ESMF_ArraySpec) :: SrcArraySpec
  type(ESMF_ArraySpec) :: DstArraySpec
  type(ESMF_DistGrid) :: src_distgrid, dst_distgrid
  type(ESMF_RouteHandle) :: routeHandle_forward
  type(ESMF_RouteHandle) :: routeHandle_backward

  ! local integers
  integer :: localrc ! local error status
  integer :: iDfile, iGfile, iD, iG
  integer :: test_status
  integer :: localPET
  ! integer :: libflag

  ! local characters
  character(THARN_MAXSTR) :: liG, liD

  ! debug
  ! real(ESMF_KIND_R8), pointer :: farrayPtr2(:,:)
  ! integer :: i1, i2, de, localDeCount, dimCount 
  ! integer, allocatable ::  localDeToDeMap(:)
  ! type(ESMF_LocalArray), allocatable :: larrayList(:)
  ! integer, allocatable :: LBnd(:,:), UBnd(:,:) 
  ! type(ESMF_Index_Flag) :: indexflag

  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL
  rc = ESMF_RC_NOT_IMPL
  ! Setting to fail per the Don's email of 3/9/09.
  test_status = HarnessTest_FAILURE

  ! initialize test counter
  test_failure = 0

  !-----------------------------------------------------------------------------
  ! for a single problem descriptor string, loop through each specifier file
  ! combination
  ! Create source and destination distributions, Fields and conduct regrid
  !-----------------------------------------------------------------------------
  print*,'-----------------======field regrid test==========-----------------------'

  do iDfile=1,PDS%nDfiles         ! distribution specifier files
    do iGfile=1,PDS%nGfiles       ! grid specifier files
      do iD=1, PDS%Dfiles(iDfile)%nDspecs   ! entries in distribution specifier
        do iG=1, PDS%Gfiles(iGfile)%nGspecs ! entries in grid specifier file
          !---------------------------------------------------------------------
          ! create source distribution
          !---------------------------------------------------------------------
          call create_distribution(PDS%SrcMem, PDS%Dfiles(iDfile)%src_dist(iD),&
                    PDS%Gfiles(iGfile)%src_grid(iG), src_distgrid, VM, localrc)
          write(liG,"(i5)") iG 
          write(liD,"(i5)") iD 
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error creating source distgrid "  &
             // " with string "  // trim(adjustL(PDS%pds)) //                  &
             " with entry "  // trim(adjustL(liD)) // " of file " //           &
             trim(adjustL(PDS%Dfiles(iDfile)%filename))                        &
             // " and entry " // trim(adjustL(liG)) // " of file " //          &
             trim(adjustL(PDS%Gfiles(iGfile)%filename)),                       &
             rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! create source grid from from distribution
          !---------------------------------------------------------------------
          call create_grid_from_distgrid(gridSrc, src_distgrid, PDS%SrcMem,    &
                      PDS%Gfiles(iGfile)%src_grid(iG),  &
                      PDS%Dfiles(iDfile)%src_dist(iD), localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error creating source array "     &
             // " with string "  // trim(adjustL(PDS%pds)) //                  &
             " with entry "  // trim(adjustL(liD)) // " of file " //           &
             trim(adjustL(PDS%Dfiles(iDfile)%filename))                        &
             // " and entry " // trim(adjustL(liG)) // " of file " //          &
             trim(adjustL(PDS%Gfiles(iGfile)%filename)),                       &
             rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! create array spec
          !---------------------------------------------------------------------
          !---------------------------------------------------------------------
          ! set the dimensionality of actual data storage to the memory size 
          ! specified by the problem descriptor string
          !---------------------------------------------------------------------
          call ESMF_ArraySpecSet(SrcArraySpec, typekind=ESMF_TYPEKIND_R8,      &
                         rank=PDS%SrcMem%memRank, rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error creating ArraySpecSet",     &
                         rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! create source field from grid and arrayspec
          !---------------------------------------------------------------------
          srcField = ESMF_FieldCreate(grid=gridSrc, arrayspec=SrcArraySpec,    &
                  staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error creating source field",     &
                  rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! populate src field with test function for regridding test
          !---------------------------------------------------------------------
!!!! need to populate as in array redist test

          !---------------------------------------------------------------------
          ! create return field from grid and arrayspec
          !---------------------------------------------------------------------
          returnField = ESMF_FieldCreate(grid=gridReturn, arrayspec=SrcArraySpec, &
                  staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error creating source field",     &
                  rcToReturn=rc)) return

!-------------------------------------------------------------------------------
! Destination
!-------------------------------------------------------------------------------
          !---------------------------------------------------------------------
          ! Create Destination distribution
          !---------------------------------------------------------------------
          call create_distribution(PDS%DstMem, PDS%Dfiles(iDfile)%dst_dist(iD),&
                    PDS%Gfiles(iGfile)%dst_grid(iG), dst_distgrid, VM, localrc)
          write(liG,"(i5)") iG 
          write(liD,"(i5)") iD 
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error creating source distgrid "  &
             // " with string "  // trim(adjustL(PDS%pds)) //                  &
             " with entry "  // trim(adjustL(liD)) // " of file " //           &
             trim(adjustL(PDS%Dfiles(iDfile)%filename))                        &
             // " and entry " // trim(adjustL(liG)) // " of file " //          &
             trim(adjustL(PDS%Gfiles(iGfile)%filename)),                       &
             rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! create destination grid from from distribution
          !---------------------------------------------------------------------
          call create_grid_from_distgrid(gridDst, dst_distgrid, PDS%DstMem,    &
                      PDS%Gfiles(iGfile)%dst_grid(iG),    &
                      PDS%Dfiles(iDfile)%src_dist(iD), localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error creating source array "     &
             // " with string "  // trim(adjustL(PDS%pds)) //                  &
             " with entry "  // trim(adjustL(liD)) // " of file " //           &
             trim(adjustL(PDS%Dfiles(iDfile)%filename))                        &
             // " and entry " // trim(adjustL(liG)) // " of file " //          &
             trim(adjustL(PDS%Gfiles(iGfile)%filename)),                       &
             rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! create array spec
          !---------------------------------------------------------------------
          !---------------------------------------------------------------------
          ! set the dimensionality of actual data storage to the memory size
          ! specified by the problem descriptor string
          !---------------------------------------------------------------------
          call ESMF_ArraySpecSet(DstArraySpec, typekind=ESMF_TYPEKIND_R8,      &
                         rank=PDS%DstMem%memRank, rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error creating dst ArraySpecSet", &
                         rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! create source field from grid and arrayspec
          !---------------------------------------------------------------------
          dstField = ESMF_FieldCreate(gridDst, DstArraySpec,                   &
                  staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error creating dst field",        &
                  rcToReturn=rc)) return

  !-----------------------------------------------------------------------------
  ! Now conduct the forward redist test
  !-----------------------------------------------------------------------------
          ! forward redist
          call ESMF_FieldRedistStore(srcField, dstField, routeHandle_forward,  &
                        rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"Field Redist " //                 &
                        "store failed", rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! redist run
          !---------------------------------------------------------------------
          call ESMF_FieldRedist(srcField, dstField, routeHandle_forward,       &
                                rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"Field Redist run failed for " //  &
                  " forward failed ", rcToReturn=rc)) return

  !-----------------------------------------------------------------------------
  ! backward redist
  !-----------------------------------------------------------------------------
          call ESMF_FieldRedistStore(dstField, returnField,                    &
                                     routeHandle_backward, rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"Field Redist " //                 &
                        "store failed", rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! redist run
          !---------------------------------------------------------------------
          call ESMF_FieldRedist(dstField, returnField, routeHandle_backward,   &
                                rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"Field Redist run failed for " //  &
                  " backward failed ", rcToReturn=rc)) return

  !-----------------------------------------------------------------------------
  ! Check redistribution
  !-----------------------------------------------------------------------------
          !---------------------------------------------------------------------
          !---------------------------------------------------------------------
!!!!! to do: check the redist - should be similar to the array redist check 
!!!!! except that you need to extract the values from a field rather than an array 

          PDS%test_record(iDfile,iGfile)%test_status(iD,iG) = test_status

          if( test_status == HarnessTest_FAILURE ) then 
             test_failure = test_failure + 1
          endif

          call ESMF_VMGet(VM, localPet=localPET, rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"can not get local pet ",          &
                  rcToReturn=rc)) return

          call report_descriptor_string(PDS, iG, iD, iGfile, iDfile,           &
                                        reportType, localPET, localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"redistribution array " //         &
                  " test report failed ", rcToReturn=rc)) return

  !-----------------------------------------------------------------------------
  ! Clean up!!!
  !-----------------------------------------------------------------------------
          !---------------------------------------------------------------------
          ! release handles
          !---------------------------------------------------------------------
          call ESMF_FieldRegridRelease(routeHandle_forward, rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"Regrid routehandle Release failed",&
                 rcToReturn=rc)) return

          call ESMF_FieldRegridRelease(routeHandle_backward, rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"Regrid routehandle Release failed",&
                 rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! release Fields
          !---------------------------------------------------------------------
          call ESMF_FieldDestroy(srcField, rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"SRC Field Regrid Release failed", &
                 rcToReturn=rc)) return

          call ESMF_FieldDestroy(returnField, rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"Return Field Regrid Release failed", &
                 rcToReturn=rc)) return

          call ESMF_FieldDestroy(dstField, rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"DST Field Regrid Release failed", &
                 rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! release Grids
          !---------------------------------------------------------------------
          call ESMF_GridDestroy(gridSrc, rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"SRC grid Release failed",         &
                 rcToReturn=rc)) return

          call ESMF_GridDestroy(gridDst, rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"DST Grid Release failed",         &
                 rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! Destroy DistGrid objects before running next test
          !---------------------------------------------------------------------
          call ESMF_DistGridDestroy(src_distgrid, rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"unable to destroy src_distgrid",  &
             rcToReturn=rc)) return

          call ESMF_DistGridDestroy(dst_distgrid, rc=localrc)
          if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"unable to destroy src_distgrid",  &
             rcToReturn=rc)) return


          !---------------------------------------------------------------------

        enddo  ! iG
      enddo  ! iD
    enddo  ! iGfile
  enddo   ! iDfile
  !-----------------------------------------------------------------------------
  ! if I've gotten this far without an error, then the routine has succeeded.
  !-----------------------------------------------------------------------------
  rc = ESMF_SUCCESS
  
  print*,'Field Redist Completed'
  !-----------------------------------------------------------------------------
  end subroutine field_redist_test
  !-----------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!===============================================================================
! Internal Methods
!===============================================================================
!-------------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  subroutine create_distribution(Memory, DistRecord, GridRecord, DistGrid,VM,rc)
  !-----------------------------------------------------------------------------
  ! routine creates a single distribution from specifier files
  !
  !-----------------------------------------------------------------------------
  ! arguments
  type(memory_config), intent(in   ) :: Memory
  type(dist_specification_record), intent(in   ) :: DistRecord
  type(grid_specification_record), intent(in   ) :: GridRecord
  type(ESMF_DistGrid), intent(  out) :: DistGrid
  type(ESMF_VM), intent(in   ) :: VM
  integer, intent(inout) :: rc

  ! local parameters
  integer :: localrc ! local error status
  integer :: allocRcToTest

  ! local integer variables
  integer :: k, nconnect
  integer, allocatable :: BIndx(:), EIndx(:)
  integer, allocatable :: decompOrder(:)
  type(ESMF_Decomp_Flag), allocatable :: decompType(:)
  type(ESMF_DistGridConnection), allocatable :: connectionList(:)
  integer, allocatable :: positionVector(:),orientationVector(:)

  ! local logicals
  logical :: noconnections

  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL
  rc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! allocate input arrays with size of the grid rank - since dist rank is now
  ! equal to the size of Grid Rank, with any dist missing dimensions set to one.
  !-----------------------------------------------------------------------------
  allocate( BIndx(Memory%GridRank), stat=allocRcToTest )
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//    &
     " BIndx in create_distribution", rcToReturn=rc)) then
  endif
  allocate( EIndx(Memory%GridRank), stat=allocRcToTest )
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//    &
     " EIndx in create_distribution", rcToReturn=rc)) then
  endif
  allocate( decompOrder(Memory%GridRank), stat=allocRcToTest )
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//    &
     " decompOrder in create_distribution", rcToReturn=rc)) then
  endif
  allocate( decompType(Memory%GridRank), stat=allocRcToTest )
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="Type "//                &
     " decompType in create_distribution", rcToReturn=rc)) then
  endif
  allocate( connectionList(1), stat=allocRcToTest )
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//    &
     " connectionList in create_distribution", rcToReturn=rc)) then
  endif

  !-----------------------------------------------------------------------------
  ! fill input arrays:
  ! EIndx - filled with the grid sizes as specified by the grid specifier
  !         files, but in the order indicated by the problem descriptor strings
  ! decompOrder - filled with distribution sizes as specified by the dist
  !           specifier files, but in the order indicated by the PDStrings
  ! decompType - set to either ESMF_DECOMP_BALANCED or ESMF_DECOMP_CYCLIC
  !           depending on how its indicated in the problem descriptor string
  !-----------------------------------------------------------------------------
  nconnect = 0           ! assume number of connections is zero

! print*,' memory rank ',Memory%memRank
! print*,' grid rank',Memory%GridRank
  ! fill the array with gridRank number of elements 
  do k=1,Memory%GridRank
    BIndx(k)   = 1
    EIndx(k) = GridRecord%gsize( Memory%GridOrder(k) )
  enddo  ! k

  ! if there are additional memory elements fill them with what is left over
! if( Memory%memRank > Memory%GridRank ) then
!   do k=Memory%GridRank+1,Memory%memRank
!     EIndx(k) = GridRecord%gsize( Memory%GridOrder(k) )
!   enddo  ! k
! endif

  ! pad the distribution with ones until its the same rank as the grid
  do k=1,Memory%DistRank
    decompOrder(k) = DistRecord%dsize( Memory%DistOrder(k) )
    decompType(k)  = ESMF_DECOMP_BALANCED
  enddo   ! k

  do k=Memory%DistRank+1, Memory%GridRank
    decompOrder(k) = 1
    decompType(k)  = ESMF_DECOMP_BALANCED
  enddo   ! k

  do k=1, Memory%DistRank
    !  assume the decomposition type is block unless block-cyclic is specified
    if( trim(adjustL(Memory%DistType(k)%string)) == "C" )  then
      decompType(k) = ESMF_DECOMP_CYCLIC
    endif

    ! look for periodic boundary conditions specified in the grid specifier file
    if( pattern_query(GridRecord%gtype(Memory%GridOrder(k))%string,            &
      "_periodic") /= 0 .or. pattern_query(                                    &
      GridRecord%gtype(Memory%GridOrder(k))%string,"_PERIODIC") /= 0)  then
      nconnect = nconnect + 1
    endif
  enddo  ! k

! print*,' mem rank ',Memory%memRank
! do k=1,Memory%GridRank
!   print*,k,' order/size ',Memory%GridOrder(k),decompOrder(k),EIndx(k)
! enddo
! print*,'        '
! print*,'record size dist ',DistRecord%dsize(1),DistRecord%dsize(2)
  !-----------------------------------------------------------------------------
  ! check for a connected domain - set connection call arguments
  !-----------------------------------------------------------------------------
  if( nconnect == 1 ) then
    ! singlely periodic domain
    noconnections = .FALSE. 
    ! workspace
    allocate( positionVector(Memory%memRank), stat=allocRcToTest )
    if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//        &
       " positionVector in create_distribution", rcToReturn=rc)) then
    endif
    allocate( orientationVector(Memory%memRank), stat=allocRcToTest )
    if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//        &
       " orientationVector in create_distribution", rcToReturn=rc)) then
    endif

    do k=1, Memory%GridRank
      positionVector(k) = 0
      orientationVector(k) = k
      if( pattern_query(GridRecord%gtype(Memory%GridOrder(k))%string,          &
        "_periodic") /= 0 .or. pattern_query(                                  &
        GridRecord%gtype(Memory%GridOrder(k))%string,"_PERIODIC") /= 0)  then
        positionVector(k) = EIndx(k)
      endif
    enddo
  elseif( nconnect > 1 ) then
    ! multiply periodic domain
    noconnections = .FALSE. 
      ! multiply connected domains are not currently supported
  else
    ! no tile connections specified
    noconnections = .TRUE.  

  endif

  !-----------------------------------------------------------------------------
  ! create the distgrid
  !-----------------------------------------------------------------------------
  if( noconnections ) then
    ! no connection
    distgrid = ESMF_DistGridCreate(minIndex=BIndx, maxIndex=EIndx,             &
                   regDecomp=decompOrder, decompflag=decompType,               &
                   vm=VM, rc=localrc)
    if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error creating distgrid",               &
       rcToReturn=rc)) return

    !---------------------------------------------------------------------------
    ! debug
    !---------------------------------------------------------------------------
    if( debug_flag ) then
        print*,'==============Dist Grid Create info=============      '
        print*,' Min index ', BIndx
        print*,' Max index ', EIndx
        print*,' Decomp Order ', decompOrder
        print*,'      '
    endif
    !---------------------------------------------------------------------------
    ! debug
    !---------------------------------------------------------------------------

  else
    ! singlely periodic connection

    call ESMF_DistGridConnectionSet(connection=connectionList(1),              &
                                 tileIndexA=1, tileIndexB=1,                   &
                                 positionVector=positionVector,                &
                                 orientationVector=orientationVector,          &
                                 rc=localrc)

    distgrid = ESMF_DistGridCreate(minIndex=BIndx, maxIndex=EIndx,             &
                   regDecomp=decompOrder, decompflag=decompType,               &
                   connectionList=connectionList,rc=localrc)

  if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error creating distgrid",                 &
             rcToReturn=rc)) return
    deallocate( positionVector,orientationVector )
  endif


  !-----------------------------------------------------------------------------
  ! clean up
  !-----------------------------------------------------------------------------
  deallocate( BIndx, EIndx )
  deallocate( decompOrder, decompType )
  deallocate( connectionList )

  !-----------------------------------------------------------------------------
  rc = ESMF_SUCCESS     
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  end subroutine create_distribution
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine create_array(Array, DistGrid, Memory, Grid, rc)
  !-----------------------------------------------------------------------------
  ! routine creates a single distribution from specifier files
  !
  !-----------------------------------------------------------------------------
  ! arguments
  type(ESMF_Array), intent(  out) :: Array
  type(ESMF_DistGrid), intent(in   ) :: DistGrid
  type(memory_config), intent(in   ) :: Memory
  type(grid_specification_record), intent(in   ) :: Grid
  integer, intent(inout) :: rc
 
  ! local ESMF types
  type(ESMF_ArraySpec) :: ArraySpec

  ! local integer variables
  integer :: irank, k, tensorsize
  integer, allocatable :: haloL(:), haloR(:)
  integer, allocatable :: top(:), bottom(:)
  integer :: localrc ! local error status
  integer :: allocRcToTest

  ! local logicals
  logical :: nohaloflag


  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL
  rc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! set the dimensionality of actual data storage to the memory size specified
  ! by the problem descriptor string
  !-----------------------------------------------------------------------------
  call ESMF_ArraySpecSet(ArraySpec, typekind=ESMF_TYPEKIND_R8,                 &
                         rank=Memory%memRank, rc=localrc)

  if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error creating ArraySpecSet",             &
                            rcToReturn=rc)) return

  !-----------------------------------------------------------------------------
  ! sanity check, make certain there are nonzero values for all distgrid dimensions
  !-----------------------------------------------------------------------------
! do k=1, Memory%DistRank
!    if( map(k) == 0 .or. map(k) > Memory%MemRank ) then
!       print*,'error - the inversion of DistOrder has failed',map(k)
!    endif
! enddo
! ! drs debug
! print*,' distgridto arraymap ',map
  ! drs debug

  !-----------------------------------------------------------------------------
  ! determine if halo is present
  !-----------------------------------------------------------------------------
  nohaloflag = .true.  
  do irank=1, Memory%GridRank
     if( Memory%HaloL(irank) /= 0 ) nohaloflag = .false.
     if( Memory%HaloR(irank) /= 0 ) nohaloflag = .false.
  enddo   ! irank

  !-----------------------------------------------------------------------------
  ! if no halo specified, create array from ArraySpec object
  !-----------------------------------------------------------------------------
  if( nohaloflag ) then
!    print*,' no halos specified '
     !--------------------------------------------------------------------------
     ! assume that the GridRank=DistGridRank, by construction if the
     ! distGridRank < GridRank, we pad the distGrid with ones so that they are
     ! the same rank.
     !
     ! next if the MemoryRank = GridRank, there are no tensor dimensions (i.e.
     ! dimensions both not distributed nor associated with a grid), thus the 
     ! undistLBound and undistUBound arguments need not be specified.
     !--------------------------------------------------------------------------
     if( Memory%memRank ==  Memory%GridRank ) then
        !-----------------------------------------------------------------------
        ! Memory Rank = Grid Rank
        !-----------------------------------------------------------------------
!       print*,'Memory Rank = Grid Rank ',Memory%memRank, ' = ', Memory%GridRank
        Array = ESMF_ArrayCreate(arrayspec=ArraySpec, distgrid=DistGrid,       &
                  indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
        if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error creating non-haloed ESMF " // &
           "Array with no tensor dimensions", rcToReturn=rc)) return

     elseif( Memory%memRank > Memory%GridRank ) then
        !-----------------------------------------------------------------------
        ! Memory Rank > Grid Rank, so there are tensor dimensions
        !-----------------------------------------------------------------------
        tensorsize = Memory%memRank-Memory%GridRank
        allocate( top(tensorsize), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//    &
           " top in create_array", rcToReturn=rc)) then
        endif
        allocate( bottom(tensorsize), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//    &
           " bottom in create_array", rcToReturn=rc)) then
        endif

        print*,'Tensor dims ',tensorsize,' - Memory Rank > Grid Rank ',        &
               Memory%memRank, Memory%GridRank

        !-----------------------------------------------------------------------
        ! specify the bounds of the undistributed dimension(s).
        !-----------------------------------------------------------------------
        do k=Grid%grank,Grid%grank-tensorsize+1,-1
           bottom(k) = 1
           top(k) = Grid%gsize( Memory%GridOrder(k) )
        enddo  ! k

        Array = ESMF_ArrayCreate(arrayspec=ArraySpec, distgrid=DistGrid,       &
                  indexflag=ESMF_INDEX_GLOBAL,                                 &
                  undistLBound=bottom, undistUBound=top, rc=localrc)
        if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error creating non-haloed ESMF " // &
           "Array with tensor dimensions", rcToReturn=rc)) return

        deallocate( top, bottom )
     else
        print*,'error - Memory Rank < Grid Rank'
        call ESMF_LogSetError( ESMF_FAILURE, msg="memory rank < Grid rank not"// &
               "supported ",rcToReturn=localrc)
        return
     endif

  else
  !-----------------------------------------------------------------------------
  ! else if halo is specified, create an array with halo padding by setting
  ! totalLWith and totalRwidth
  !-----------------------------------------------------------------------------
     allocate( haloL(Memory%memRank), stat=allocRcToTest )
     if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//       &
        " haloL in create_array", rcToReturn=rc)) then
     endif
     allocate( haloR(Memory%memRank), stat=allocRcToTest )
     if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//       &
        " haloR in create_array", rcToReturn=rc)) then
     endif

     do k=1,Memory%GridRank
        haloL(k) = Memory%HaloL(k)
        haloR(k) = Memory%HaloR(k)
     enddo
     ! padd additional values so that array sizes matches memory rank
     do k=Memory%GridRank+1,Memory%memRank
        haloL(k) = 0
        haloR(k) = 0
     enddo

     !--------------------------------------------------------------------------
     ! assume that the GridRank=DistGridRank, by construction if the
     ! distGridRank < GridRank, we pad the distGrid with ones so that they are
     ! the same rank.
     !
     ! next if the MemoryRank = GridRank, there are no tensor dimensions (i.e.
     ! dimensions both not distributed nor associated with a grid), thus the 
     ! undistLBound and undistUBound arguments need not be specified.
     !--------------------------------------------------------------------------
     if( Memory%memRank ==  Memory%GridRank ) then
        !-----------------------------------------------------------------------
        ! Memory Rank = Grid Rank
        !-----------------------------------------------------------------------
!       print*,'Memory Rank = Grid Rank ',Memory%memRank, Memory%GridRank
        Array = ESMF_ArrayCreate(arrayspec=ArraySpec, distgrid=DistGrid,       &
                     totalLWidth=HaloL, totalUWidth=HaloR,                     &
                     indexflag=ESMF_INDEX_GLOBAL, rc=localrc)

        if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error creating non-haloed ESMF " // &
                 "Array with no tensor dimensions", rcToReturn=rc)) return

     elseif( Memory%memRank > Memory%GridRank ) then
        !-----------------------------------------------------------------------
        ! Memory Rank > Grid Rank, so there are tensor dimensions
        !-----------------------------------------------------------------------
        tensorsize = Memory%memRank-Memory%GridRank
        allocate( top(tensorsize), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//    &
           " top in create_array", rcToReturn=rc)) then
        endif
        allocate( bottom(tensorsize), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//    &
           " bottom in create_array", rcToReturn=rc)) then
        endif

!       print*,'Tensor dims ',tensorsize,' - Memory Rank > Grid Rank ',        &
!              Memory%memRank, Memory%GridRank

        !-----------------------------------------------------------------------
        ! specify the bounds of the undistributed dimension(s).
        !-----------------------------------------------------------------------
        do k=Grid%grank,Grid%grank-tensorsize+1,-1
           bottom(k) = 1
           top(k) = Grid%gsize( Memory%GridOrder(k) )
        enddo  ! k

        Array = ESMF_ArrayCreate(arrayspec=ArraySpec, distgrid=DistGrid,       &
                     totalLWidth=HaloL, totalUWidth=HaloR,                     &
                     indexflag=ESMF_INDEX_GLOBAL,                              &
                     undistLBound=bottom, undistUBound=top, rc=localrc)

        if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error creating haloed ESMF " //     &
                 "Array with tensor dimensions", rcToReturn=rc)) return

        deallocate( top, bottom )
     else
!       print*,'error - Memory Rank < Grid Rank'
        call ESMF_LogSetError( ESMF_FAILURE, msg="memory rank < Grid rank not"// &
               "supported ",rcToReturn=localrc)
        return
     endif

     !--------------------------------------------------------------------------
     ! clean up
     !--------------------------------------------------------------------------
     deallocate( haloL, haloR )

  endif

  !-----------------------------------------------------------------------------
  rc = ESMF_SUCCESS     
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  end subroutine create_array
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
   subroutine create_grid_from_distgrid(Grid, DistGrid, Memory, Grid_info,     &
                                        Dist_info, rc)
  !-----------------------------------------------------------------------------
  ! routine creates a grid from an existing distribution and specifier files
  !-----------------------------------------------------------------------------
  ! arguments
  type(ESMF_Grid), intent(inout) :: Grid
  type(ESMF_DistGrid), intent(in   ) :: DistGrid
  type(memory_config), intent(in   ) :: Memory
  type(grid_specification_record), intent(in   ) :: Grid_info
  type(dist_specification_record), intent(in   ) :: Dist_info
  integer, intent(inout) :: rc
 
  ! local ESMF types

  ! local integer variables
  integer :: i,j,k
  ! integer :: l, m
  integer, allocatable :: lbnd(:), ubnd(:), maxI(:)
  integer :: localDECount, lDE, GridRank
  integer, allocatable :: decompOrder(:)

  integer :: localrc ! local error status

  ! local real variables
  real(ESMF_KIND_R8), pointer :: coordX2D(:,:), coordY2D(:,:)
  real(ESMF_KIND_R8), pointer :: coordX3D(:,:,:), coordY3D(:,:,:)
  real(ESMF_KIND_R8), pointer :: coordZ3D(:,:,:)

  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL
  rc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! cludge the distribution info
  !-----------------------------------------------------------------------------
  allocate( decompOrder(Memory%GridRank) )
  do k=1,Memory%DistRank
     decompOrder(k) = Dist_info%dsize( Memory%DistOrder(k) )
  enddo   ! k
  ! pad the distribution with ones until its the same rank as the grid
  do k=Memory%DistRank+1, Memory%GridRank
     decompOrder(k) = 1
  enddo   ! k

  !-----------------------------------------------------------------------------
  ! Create the Grid object from an existing distribution 
  !-----------------------------------------------------------------------------
  select case(Grid_info%grank)
    case(2)
!     Grid = ESMF_GridCreate(distgrid=DistGrid, gridEdgeLWidth=(/ 0,0 /),      &
!                 gridEdgeUWidth =(/0,0/),  rc=localrc)
      allocate( maxI(Grid_info%grank) )
      maxI = Grid_info%gsize
      Grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=maxI,         &
                regDecomp=decompOrder, &
                coordSys=ESMF_COORDSYS_CART,                                      &
                indexflag=ESMF_INDEX_GLOBAL,                                   &
                gridEdgeLWidth=(/ 0,0 /),                                      &
                gridEdgeUWidth =(/0,0/),  rc=localrc)
      deallocate( maxI )

    case(3)
!     Grid = ESMF_GridCreate(distgrid=DistGrid, gridEdgeLWidth=(/0,0,0/),      &
!                 gridEdgeUWidth =(/0,0,0/),  rc=localrc)
      allocate( maxI(Grid_info%grank) )
      maxI = Grid_info%gsize
      Grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/), maxIndex=maxI,       &
                regDecomp=decompOrder,                                         &
                coordSys=ESMF_COORDSYS_CART,                                      &
                indexflag=ESMF_INDEX_GLOBAL,                                   &
                gridEdgeLWidth=(/0,0,0/),                                      &
                gridEdgeUWidth =(/0,0,0/),  rc=localrc)
      deallocate( maxI )

    case default
      call ESMF_LogSetError(ESMF_FAILURE, msg="error in creating grid from distribution - unsupported rank", &
                    rcToReturn=rc)
      return
  end select

  if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error creating grid from distribution",   &
                            rcToReturn=rc)) return

  !-----------------------------------------------------------------------------
  ! Get the number of local DEs
  !-----------------------------------------------------------------------------
  call ESMF_GridGet(grid=Grid, localDECount=localDECount, rc=localrc)
  if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting local DE count from grid",  &
                            rcToReturn=rc)) return

  !-----------------------------------------------------------------------------
  ! Get the number of local DEs
  !-----------------------------------------------------------------------------
  call ESMF_GridGet(grid=Grid, dimCount=GridRank, rc=localrc)
  if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting Grid rank from grid",       &
                            rcToReturn=rc)) return

  !-----------------------------------------------------------------------------
  ! allocate arrays to hold the local array bounds
  !-----------------------------------------------------------------------------
  allocate( lbnd(GridRank) )
  allocate( ubnd(GridRank) )

  !-----------------------------------------------------------------------------
  ! Allocate coordinates
  !-----------------------------------------------------------------------------
  call ESMF_GridAddCoord(Grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error adding coord to grid",              &
                            rcToReturn=rc)) return

  do lDE=0,localDECount-1
    select case(GridRank)
      case(1)
      !-------------------------------------------------------------------------
      ! grid rank = 1
      !-------------------------------------------------------------------------
      localrc = ESMF_FAILURE
      call ESMF_LogSetError(ESMF_FAILURE, msg="Grid rank=1 not supported ",      &
                    rcToReturn=localrc)
      return

      case(2)
      !-------------------------------------------------------------------------
      ! grid rank = 2
      !-------------------------------------------------------------------------
      call ESMF_GridGetCoord(Grid, localDE=lDE,                                &
                   staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1,              &
                   computationalLBound=lbnd, computationalUBound=ubnd,         &
                   farrayPtr=coordX2D, rc=localrc)
      if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting grid=1 coordinates",    &
                            rcToReturn=rc)) return

      call ESMF_GridGetCoord(Grid, localDE=lDE,                                &
                   staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2,              &
                   computationalLBound=lbnd, computationalUBound=ubnd,         &
                   farrayPtr=coordY2D, rc=localrc)
      if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting grid=2 coordinates",    &
                            rcToReturn=rc)) return


      !-------------------------------------------------------------------------
      !  set coordinates
      !-------------------------------------------------------------------------
      do j=lbnd(2),ubnd(2)
         do i=lbnd(1),ubnd(1)
           coordX2D(i,j) = create_coord(i, Grid_info, 1, localrc)
           if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting x coordinates",    &
                            rcToReturn=rc)) return

           coordY2D(i,j) = create_coord(j, Grid_info, 2, localrc)
           if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting y coordinates",    &
                            rcToReturn=rc)) return
           !--------------------------------------------------------------------
           ! debug
           !--------------------------------------------------------------------
           if( debug_flag ) then
              print*,'coord values ',i,j,coordX2D(i,j),coordY2D(i,j)
           endif
           !--------------------------------------------------------------------
           ! debug
           !--------------------------------------------------------------------
         enddo    ! i loop
      enddo    ! j loop

      !-------------------------------------------------------------------------
      ! debug
      !-------------------------------------------------------------------------
      if( debug_flag ) then
         print*,'Set Coordinates'
         print*,'x/y(1,1)',coordX2D(lbnd(1),lbnd(2)),coordY2D(lbnd(1),lbnd(2))
         print*,'x/y(1,n)',coordX2D(lbnd(1),ubnd(2)),coordY2D(lbnd(1),ubnd(2))
        print*,'x/y(n,n)',coordX2D(ubnd(1),ubnd(2)),coordY2D(ubnd(1),ubnd(2))
      endif
      !-------------------------------------------------------------------------
      ! debug
      !-------------------------------------------------------------------------


      case(3)
      !-------------------------------------------------------------------------
      ! grid rank = 3
      !-------------------------------------------------------------------------
      call ESMF_GridGetCoord(Grid, localDE=lDE,                                &
                   staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1,              &
                   computationalLBound=lbnd, computationalUBound=ubnd,         &
                   farrayPtr=coordX3D, rc=localrc)
      if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting grid=1 coordinates",    &
                            rcToReturn=rc)) return

      call ESMF_GridGetCoord(Grid, localDE=lDE,                                &
                   staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2,              &
                   computationalLBound=lbnd, computationalUBound=ubnd,         &
                   farrayPtr=coordY3D, rc=localrc)
      if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting grid=2 coordinates",    &
                            rcToReturn=rc)) return

      call ESMF_GridGetCoord(Grid, localDE=lDE,                                &
                   staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=3,              &
                   computationalLBound=lbnd, computationalUBound=ubnd,         &
                   farrayPtr=coordZ3D, rc=localrc)
      if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting grid=3 coordinates",    &
                            rcToReturn=rc)) return

      !-------------------------------------------------------------------------
      !  set coordinates
      !-------------------------------------------------------------------------
      do k=lbnd(3),ubnd(3)
         do j=lbnd(2),ubnd(2)
            do i=lbnd(1),ubnd(1)
              coordX3D(i,j,k) = create_coord(i, Grid_info, 1, localrc)
              if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting x coordinates", &
                            rcToReturn=rc)) return
              coordY3D(i,j,k) = create_coord(j, Grid_info, 2, localrc)
              if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting y coordinates", &
                            rcToReturn=rc)) return
              coordZ3D(i,j,k) = create_coord(k, Grid_info, 3, localrc)
              if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting z coordinates", &
                            rcToReturn=rc)) return
            enddo    ! i loop
         enddo    ! j loop
      enddo    ! k loop

      case(4)
      !-------------------------------------------------------------------------
      ! grid rank = 4
      !-------------------------------------------------------------------------
          localrc = ESMF_FAILURE
           call ESMF_LogSetError(ESMF_FAILURE, msg="Grid rank=4 not supported ", &
                    rcToReturn=localrc)
           return

      case(5)
      !-------------------------------------------------------------------------
      ! grid rank = 5
      !-------------------------------------------------------------------------
          localrc = ESMF_FAILURE
           call ESMF_LogSetError(ESMF_FAILURE, msg="Grid rank=5 not supported ", &
                    rcToReturn=localrc)
           return

      case(6)
      !-------------------------------------------------------------------------
      ! grid rank = 6
      !-------------------------------------------------------------------------
          localrc = ESMF_FAILURE
           call ESMF_LogSetError(ESMF_FAILURE, msg="Grid rank=6 not supported ", &
                    rcToReturn=localrc)
           return

      case(7)
      !-------------------------------------------------------------------------
      ! grid rank = 7
      !-------------------------------------------------------------------------
           localrc = ESMF_FAILURE
           call ESMF_LogSetError(ESMF_FAILURE, msg="Grid rank=7 not supported ", &
                    rcToReturn=localrc)
           return
      case default
      !-------------------------------------------------------------------------
      ! error
      !-------------------------------------------------------------------------
           localrc = ESMF_FAILURE
           call ESMF_LogSetError(ESMF_FAILURE, msg="Grid rank not between 1 & 7",&
                    rcToReturn=localrc)
           return

      end select

   enddo    ! lDE

  !-----------------------------------------------------------------------------
  ! clean up
  !-----------------------------------------------------------------------------
  deallocate( lbnd, ubnd)
  deallocate( decompOrder )

  !-----------------------------------------------------------------------------
  rc = ESMF_SUCCESS     
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  end subroutine create_grid_from_distgrid
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine populate_field(Field, Grid, Memory, Grid_info, TestFunction, rc)
  !-----------------------------------------------------------------------------
  ! routine populates the source field with the test function
  !-----------------------------------------------------------------------------
  ! arguments
  type(ESMF_Field), intent(inout) :: Field
  type(ESMF_Grid), intent(in   ) :: Grid
  type(memory_config), intent(in   ) :: Memory
  type(grid_specification_record), intent(in   ) :: Grid_info
  type(test_function_record), intent(in   ) :: TestFunction
  integer, intent(inout) :: rc
 
  ! local ESMF types

  ! local integer variables
  integer :: i,j,k
  ! integer :: l,m
  integer, allocatable :: lbnd(:), ubnd(:)
  integer :: localDECount, lDE, GridRank

  integer :: localrc ! local error status

  ! local real variables
  real(ESMF_KIND_R8) :: a, b, kx, ly, lenk, lenl
  ! real(ESMF_KIND_R8) :: exact
  real(ESMF_KIND_R8), pointer :: coordX2D(:,:), coordY2D(:,:)
  real(ESMF_KIND_R8), pointer :: exp2D(:,:)
  real(ESMF_KIND_R8), pointer :: coordX3D(:,:,:), coordY3D(:,:,:)
  real(ESMF_KIND_R8), pointer :: coordZ3D(:,:,:)
  real(ESMF_KIND_R8), pointer :: exp3D(:,:,:)

  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL
  rc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! Get the number of local DEs from the Grid
  !-----------------------------------------------------------------------------
  call ESMF_GridGet(grid=Grid, localDECount=localDECount, rc=localrc)
  if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting local DE count from grid",  &
                            rcToReturn=rc)) return

  !-----------------------------------------------------------------------------
  ! Get the grid rank
  !-----------------------------------------------------------------------------
  call ESMF_GridGet(grid=Grid, dimCount=GridRank, rc=localrc)
  if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting Grid rank from grid",       &
                            rcToReturn=rc)) return

  !-----------------------------------------------------------------------------
  ! allocate arrays to hold the local array bounds
  !-----------------------------------------------------------------------------
  allocate( lbnd(GridRank) )
  allocate( ubnd(GridRank) )

  
  !-----------------------------------------------------------------------------
  ! populate Field
  !-----------------------------------------------------------------------------

  do lDE=0,localDECount-1
    select case(GridRank)
      case(1)
      !-------------------------------------------------------------------------
      ! grid rank = 1
      !-------------------------------------------------------------------------
      localrc = ESMF_FAILURE
      call ESMF_LogSetError(ESMF_FAILURE, msg="Grid rank=1 not supported ",      &
                    rcToReturn=localrc)
      return

      case(2)
      !-------------------------------------------------------------------------
      ! grid rank = 2
      !-------------------------------------------------------------------------
      call ESMF_GridGetCoord(Grid, localDE=lDE,                                &
                   staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1,              &
                   computationalLBound=lbnd, computationalUBound=ubnd,         &
                   farrayPtr=coordX2D, rc=localrc)
      if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting grid=1 coordinates",    &
                            rcToReturn=rc)) return

      call ESMF_GridGetCoord(Grid, localDE=lDE,                                &
                   staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2,              &
                   computationalLBound=lbnd, computationalUBound=ubnd,         &
                   farrayPtr=coordY2D, rc=localrc)
      if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting grid=2 coordinates",    &
                            rcToReturn=rc)) return

      !-------------------------------------------------------------------------
      ! Get pointers to field array
      !-------------------------------------------------------------------------
      call ESMF_FieldGet(Field, lDE, exp2D, rc=localrc)
      if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error field get", rcToReturn=rc)) return

      !-------------------------------------------------------------------------
      !  set coordinates
      !-------------------------------------------------------------------------
      do j=lbnd(2),ubnd(2)
         do i=lbnd(1),ubnd(1)
           !-------------------------------------------------------------------
           select case( trim(TestFunction%string) )
             case("CONSTANT")
               exp2D(i,j) = TestFunction%param(1)

             case("COORDINATEX")
               exp2D(i,j) =  TestFunction%param(1)*coordX2D(i,j) 

             case("COORDINATEY")
               exp2D(i,j) =  TestFunction%param(1)*coordY2D(i,j) 

             case("SPHERICAL_HARMONIC")
              ! (a+b) + acos(k*2pi*x/Lx) + b*sin(l*2pi*y/Ly)
	      a = TestFunction%param(1) 
              kx = TestFunction%param(2)
              b = TestFunction%param(3)
              lY = TestFunction%param(4)
              lenk = Grid_info%grange(1,2) - Grid_info%grange(1,1)
              lenl = Grid_info%grange(2,2) - Grid_info%grange(2,1)
               exp2D(i,j) =  abs(a+b) +a*cos(pi2*kx*coordX2D(i,j)/lenk) +        &
                             b*sin(pi2*ly*coordY2D(i,j)/lenl) 

             !case("PEAK_VALLEY")
             ! (1.-x*y)*sin(k*pi*x/Lx)*cos(l*pi*y)+2.

             case default
               ! error
                call ESMF_LogSetError( ESMF_FAILURE, msg="undefined case in select statement",    &
                       rcToReturn=localrc)
                return
           end select
           !-------------------------------------------------------------------
         enddo    ! i loop
      enddo    ! j loop

      case(3)
      !-------------------------------------------------------------------------
      ! grid rank = 3
      !-------------------------------------------------------------------------
      call ESMF_GridGetCoord(Grid, localDE=lDE,                                &
                   staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1,              &
                   computationalLBound=lbnd, computationalUBound=ubnd,         &
                   farrayPtr=coordX3D, rc=localrc)
      if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting grid=1 coordinates",    &
                            rcToReturn=rc)) return

      call ESMF_GridGetCoord(Grid, localDE=lDE,                                &
                   staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2,              &
                   computationalLBound=lbnd, computationalUBound=ubnd,         &
                   farrayPtr=coordY3D, rc=localrc)
      if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting grid=2 coordinates",    &
                            rcToReturn=rc)) return

      call ESMF_GridGetCoord(Grid, localDE=lDE,                                &
                   staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=3,              &
                   computationalLBound=lbnd, computationalUBound=ubnd,         &
                   farrayPtr=coordZ3D, rc=localrc)
      if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting grid=3 coordinates",    &
                            rcToReturn=rc)) return

      !-------------------------------------------------------------------------
      ! Get pointers to field array
      !-------------------------------------------------------------------------
     call ESMF_FieldGet(Field, lDE, exp3D, rc=localrc)
      if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error field get", rcToReturn=rc)) return

      !-------------------------------------------------------------------------
      !  set coordinates
      !-------------------------------------------------------------------------
      do k=lbnd(3),ubnd(3)
         do j=lbnd(2),ubnd(2)
            do i=lbnd(1),ubnd(1)
           !-------------------------------------------------------------------
           select case( trim(TestFunction%string) )
             case("CONSTANT")
               exp3D(i,j,k) = TestFunction%param(1)

             case("COORDINATEX")
               exp3D(i,j,k) =  TestFunction%param(1)*coordX3D(i,j,k) 

             case("COORDINATEY")
               exp3D(i,j,k) =  TestFunction%param(1)*coordY3D(i,j,k) 

             case("COORDINATEZ")
               exp3D(i,j,k) =  TestFunction%param(1)*coordZ3D(i,j,k) 

             case default
               ! error
                call ESMF_LogSetError( ESMF_FAILURE, msg="undefined case in select statement",    &
                       rcToReturn=localrc)
                return
           end select
           !-------------------------------------------------------------------
            ! coordX3D(i,j,k) 
            ! coordY3D(i,j,k)
            ! coordZ3D(i,j,k)
            enddo    ! i loop
         enddo    ! j loop
      enddo    ! k loop

      case(4)
      !-------------------------------------------------------------------------
      ! grid rank = 4
      !-------------------------------------------------------------------------
          localrc = ESMF_FAILURE
           call ESMF_LogSetError(ESMF_FAILURE, msg="Grid rank=4 not supported ", &
                    rcToReturn=localrc)
           return

      case(5)
      !-------------------------------------------------------------------------
      ! grid rank = 5
      !-------------------------------------------------------------------------
          localrc = ESMF_FAILURE
           call ESMF_LogSetError(ESMF_FAILURE, msg="Grid rank=5 not supported ", &
                    rcToReturn=localrc)
           return

      case(6)
      !-------------------------------------------------------------------------
      ! grid rank = 6
      !-------------------------------------------------------------------------
          localrc = ESMF_FAILURE
           call ESMF_LogSetError(ESMF_FAILURE, msg="Grid rank=6 not supported ", &
                    rcToReturn=localrc)
           return

      case(7)
      !-------------------------------------------------------------------------
      ! grid rank = 7
      !-------------------------------------------------------------------------
           localrc = ESMF_FAILURE
           call ESMF_LogSetError(ESMF_FAILURE, msg="Grid rank=7 not supported ", &
                    rcToReturn=localrc)
           return
      case default
      !-------------------------------------------------------------------------
      ! error
      !-------------------------------------------------------------------------
           localrc = ESMF_FAILURE
           call ESMF_LogSetError(ESMF_FAILURE, msg="Grid rank not between 1 & 7",&
                    rcToReturn=localrc)
           return

      end select

   enddo    ! lDE
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  ! clean up
  !-----------------------------------------------------------------------------

  deallocate( lbnd, ubnd)

  !-----------------------------------------------------------------------------
  rc = ESMF_SUCCESS     
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  end subroutine populate_field
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine check_field(test_status, Field, Grid, Grid_info, TestFunction, rc)
  !-----------------------------------------------------------------------------
  ! routine checks destination field against analytical solution evaluated at 
  ! the destination grid points
  !-----------------------------------------------------------------------------
  ! arguments
  integer, intent(inout) :: test_status
  type(ESMF_Field), intent(inout) :: Field
  type(ESMF_Grid), intent(in   ) :: Grid
  type(grid_specification_record), intent(in   ) :: Grid_info
  type(test_function_record), intent(in   ) :: TestFunction
  integer, intent(inout) :: rc
 
  ! local ESMF types

  ! local integer variables
  integer :: i,j,k
  ! integer :: l,m
  integer, allocatable :: lbnd(:), ubnd(:)
  integer :: localDECount, lDE, GridRank

  integer :: localrc ! local error status

  ! local real variables
  real(ESMF_KIND_R8) :: a, b, kx, ly, lenk, lenl, exact

  real(ESMF_KIND_R8), pointer :: coordX2D(:,:), coordY2D(:,:)
  real(ESMF_KIND_R8), pointer :: interp2D(:,:)
  real(ESMF_KIND_R8), pointer :: coordX3D(:,:,:), coordY3D(:,:,:)
  real(ESMF_KIND_R8), pointer :: coordZ3D(:,:,:)
  real(ESMF_KIND_R8), pointer :: interp3D(:,:,:)

  ! initialize return flag
  test_status = HarnessTest_SUCCESS
  localrc = ESMF_RC_NOT_IMPL
  rc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! Get the number of local DEs from the Grid
  !-----------------------------------------------------------------------------
  call ESMF_GridGet(grid=Grid, localDECount=localDECount, rc=localrc)
  if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting local DE count from grid",  &
                            rcToReturn=rc)) return

  !-----------------------------------------------------------------------------
  ! Get the grid rank
  !-----------------------------------------------------------------------------
  call ESMF_GridGet(grid=Grid, dimCount=GridRank, rc=localrc)
  if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting Grid rank from grid",       &
                            rcToReturn=rc)) return

  !-----------------------------------------------------------------------------
  ! allocate arrays to hold the local array bounds
  !-----------------------------------------------------------------------------
  allocate( lbnd(GridRank) )
  allocate( ubnd(GridRank) )

  
  !-----------------------------------------------------------------------------
  ! check Field
  !-----------------------------------------------------------------------------

  do lDE=0,localDECount-1
    select case(GridRank)
      case(1)
      !-------------------------------------------------------------------------
      ! grid rank = 1
      !-------------------------------------------------------------------------
      localrc = ESMF_FAILURE
      call ESMF_LogSetError(ESMF_FAILURE, msg="Grid rank=1 not supported ",      &
                    rcToReturn=localrc)
      return

      case(2)
      !-------------------------------------------------------------------------
      ! grid rank = 2
      !-------------------------------------------------------------------------
      call ESMF_GridGetCoord(Grid, localDE=lDE,                                &
                   staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1,              &
                   computationalLBound=lbnd, computationalUBound=ubnd,         &
                   farrayPtr=coordX2D, rc=localrc)
      if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting grid=1 coordinates",    &
                            rcToReturn=rc)) return

      call ESMF_GridGetCoord(Grid, localDE=lDE,                                &
                   staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2,              &
                   computationalLBound=lbnd, computationalUBound=ubnd,         &
                   farrayPtr=coordY2D, rc=localrc)
      if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting grid=2 coordinates",    &
                            rcToReturn=rc)) return

      !-------------------------------------------------------------------------
      ! Get pointers to field array
      !-------------------------------------------------------------------------
      call ESMF_FieldGet(Field, lDE, interp2D, rc=localrc)
      if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error field get", rcToReturn=rc)) return

      !-------------------------------------------------------------------------
      !  compute relative error
      !-------------------------------------------------------------------------
      do j=lbnd(2),ubnd(2)
         do i=lbnd(1),ubnd(1)
           !-------------------------------------------------------------------
           select case( trim(TestFunction%string) )
             case("CONSTANT")
               if (.not. check_value (TestFunction%param(1), interp2D(i,j), TestFunction%param(2))) then
                 test_status = HarnessTest_FAILURE
                 print *,' fields disagree ',i,j,interp2D(i,j),                &
                          TestFunction%param(1)
               endif

             case("COORDINATEX")
               if (.not. check_value (TestFunction%param(1)*coordX2D(i,j), interp2D(i,j), &
                 TestFunction%param(2))) then
                   test_status = HarnessTest_FAILURE
                   print*,' fields disagree ',i,j,interp2D(i,j),                &
                          TestFunction%param(1)*coordX2D(i,j)
               endif

             case("COORDINATEY")
               if (.not. check_value (TestFunction%param(1)*coordY2D(i,j), interp2D(i,j), &
                 TestFunction%param(2))) then
                   test_status = HarnessTest_FAILURE
                   print*,' fields disagree ',i,j,interp2D(i,j),                &
                          TestFunction%param(1)*coordY2D(i,j)
               endif

             case("SPHERICAL_HARMONIC")
              ! (a+b) + acos(k*2pi*x/Lx) + b*sin(l*2pi*y/Ly)
              a = TestFunction%param(1)
              kx = TestFunction%param(2)
              b = TestFunction%param(3)
              ly = TestFunction%param(4)
              lenk = Grid_info%grange(1,2) - Grid_info%grange(1,1)
              lenl = Grid_info%grange(2,2) - Grid_info%grange(2,1)
              exact = abs(a+b) +a*cos(pi2*kx*coordX2D(i,j)/lenk) +        &
                             b*sin(pi2*ly*coordY2D(i,j)/lenl) 

              if (.not. check_value (exact, interp2D(i,j), TestFunction%param(5))) then
                 test_status = HarnessTest_FAILURE
                 print*,' fields disagree ',i,j,interp2D(i,j), exact
               endif

             case("PEAK_VALLEY")
                ! (1.-x*y)*sin(k*pi*x/Lx)*cos(l*pi*y)+2.
               test_status = HarnessTest_FAILURE
               print *, 'test function PEAK_VALLEY not implemented'

             case default
               test_status = HarnessTest_FAILURE
               print *, 'undefined test function ',trim(TestFunction%string)

           end select
           !-------------------------------------------------------------------
         enddo    ! i loop
      enddo    ! j loop

      case(3)
      !-------------------------------------------------------------------------
      ! grid rank = 3
      !-------------------------------------------------------------------------
      call ESMF_GridGetCoord(Grid, localDE=lDE,                                &
                   staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1,              &
                   computationalLBound=lbnd, computationalUBound=ubnd,         &
                   farrayPtr=coordX3D, rc=localrc)
      if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting grid=1 coordinates",    &
                            rcToReturn=rc)) return

      call ESMF_GridGetCoord(Grid, localDE=lDE,                                &
                   staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2,              &
                   computationalLBound=lbnd, computationalUBound=ubnd,         &
                   farrayPtr=coordY3D, rc=localrc)
      if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting grid=2 coordinates",    &
                            rcToReturn=rc)) return

      call ESMF_GridGetCoord(Grid, localDE=lDE,                                &
                   staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=3,              &
                   computationalLBound=lbnd, computationalUBound=ubnd,         &
                   farrayPtr=coordZ3D, rc=localrc)
      if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting grid=3 coordinates",    &
                            rcToReturn=rc)) return

      !-------------------------------------------------------------------------
      ! Get pointers to field array
      !-------------------------------------------------------------------------
      call ESMF_FieldGet(Field, lDE, interp3D, rc=localrc)
      if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error field get", rcToReturn=rc)) return

      !-------------------------------------------------------------------------
      !  check all values
      !-------------------------------------------------------------------------
      do k=lbnd(3),ubnd(3)
         do j=lbnd(2),ubnd(2)
            do i=lbnd(1),ubnd(1)
           !-------------------------------------------------------------------
           select case( trim(TestFunction%string) )
             case("CONSTANT")
               if (.not. check_value (TestFunction%param(1), interp3D(i,j,k), TestFunction%param(2))) then
                 test_status = HarnessTest_FAILURE
                 print*,' fields disagree ',i,j,k,interp3D(i,j,k),            &
                          TestFunction%param(1)
               endif

             case("COORDINATEX")
               if (.not. check_value (TestFunction%param(1)*coordX3D(i,j,k), interp3D(i,j,k), &
                 TestFunction%param(2))) then
                   test_status = HarnessTest_FAILURE
                   print*,' fields disagree ',i,j,k,interp3D(i,j,k),            &
                        TestFunction%param(1)*coordX3D(i,j,k)
               endif

             case("COORDINATEY")
               if (.not. check_value (TestFunction%param(1)*coordY3D(i,j,k), interp3D(i,j,k), &
                 TestFunction%param(2))) then
                   test_status = HarnessTest_FAILURE
                   print*,' fields disagree ',i,j,k,interp3D(i,j,k),            &
                        TestFunction%param(1)*coordY3D(i,j,k)
               endif

             case("COORDINATEZ")
               if (.not. check_value (TestFunction%param(1)*coordZ3D(i,j,k), interp3D(i,j,k), &
                 TestFunction%param(2))) then
                   test_status = HarnessTest_FAILURE
                   print*,' fields disagree ',i,j,k,interp3D(i,j,k),            &
                        TestFunction%param(1)*coordZ3D(i,j,k)
               endif

             case default
               test_status = HarnessTest_FAILURE
               print *, 'undefined test function ',trim(TestFunction%string)

           end select
           !-------------------------------------------------------------------
            enddo    ! i loop
         enddo    ! j loop
      enddo    ! k loop

      case(4)
      !-------------------------------------------------------------------------
      ! grid rank = 4
      !-------------------------------------------------------------------------
          localrc = ESMF_FAILURE
           call ESMF_LogSetError(ESMF_FAILURE, msg="Grid rank=4 not supported ", &
                    rcToReturn=localrc)
           return

      case(5)
      !-------------------------------------------------------------------------
      ! grid rank = 5
      !-------------------------------------------------------------------------
          localrc = ESMF_FAILURE
           call ESMF_LogSetError(ESMF_FAILURE, msg="Grid rank=5 not supported ", &
                    rcToReturn=localrc)
           return

      case(6)
      !-------------------------------------------------------------------------
      ! grid rank = 6
      !-------------------------------------------------------------------------
          localrc = ESMF_FAILURE
           call ESMF_LogSetError(ESMF_FAILURE, msg="Grid rank=6 not supported ", &
                    rcToReturn=localrc)
           return

      case(7)
      !-------------------------------------------------------------------------
      ! grid rank = 7
      !-------------------------------------------------------------------------
           localrc = ESMF_FAILURE
           call ESMF_LogSetError(ESMF_FAILURE, msg="Grid rank=7 not supported ", &
                    rcToReturn=localrc)
           return
      case default
      !-------------------------------------------------------------------------
      ! error
      !-------------------------------------------------------------------------
           localrc = ESMF_FAILURE
           call ESMF_LogSetError(ESMF_FAILURE, msg="Grid rank not between 1 & 7",&
                    rcToReturn=localrc)
           return

      end select

   enddo    ! lDE
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  ! clean up
  !-----------------------------------------------------------------------------

  deallocate( lbnd, ubnd)

  !-----------------------------------------------------------------------------
  rc = ESMF_SUCCESS     
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  end subroutine check_field
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine populate_redist_array(Array, DistGrid, Memory, Grid, rc)
  !-----------------------------------------------------------------------------
  ! routine populates an esmf array to the values used for a redist test. These 
  ! values are dependent on the global coordinates and the local DE number. For
  ! a rank 2 array the values are set to Value = localDE + 1000*i1 + 1/1000 *i2
  !
  !-----------------------------------------------------------------------------
  ! arguments
  type(ESMF_Array), intent(inout) :: Array
  type(ESMF_DistGrid), intent(in   ) :: DistGrid
  type(memory_config), intent(in   ) :: Memory
  type(grid_specification_record), intent(in   ) :: Grid
  integer, intent(inout) :: rc
 
  ! local ESMF types
  type(ESMF_LocalArray), allocatable :: larrayList(:)
  type(ESMF_Index_Flag) :: indexflag

  ! local parameters
  integer :: localrc ! local error status

  ! local integer variables
  integer :: de, localDeCount, dimCount 
  integer, allocatable ::  localDeToDeMap(:)
  integer, allocatable :: LBnd(:,:), UBnd(:,:) 
  integer :: i1, i2, i3, i4, i5, i6, i7
  ! integer :: irank, k, tensorsize, fsize(7)
  ! integer, allocatable :: haloL(:), haloR(:)
  ! integer, allocatable :: top(:), bottom(:)
  integer :: allocRcToTest

  ! local real variables
  real(ESMF_KIND_R8), pointer :: farrayPtr1(:), farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr3(:,:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr4(:,:,:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr5(:,:,:,:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr6(:,:,:,:,:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr7(:,:,:,:,:,:,:)

  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL
  rc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! get local array DE list
  !-----------------------------------------------------------------------------
  call ESMF_ArrayGet(array, localDeCount=localDeCount, rc=localrc)
  if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting local DE count from array", &
          rcToReturn=rc)) return

  allocate(localDeToDeMap(localDeCount), stat=allocRcToTest)
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//          &
     " localDeToDeMap in populate_redist_array", rcToReturn=rc)) then
  endif
  call ESMF_ArrayGet(array, localDeToDeMap=localDeToDeMap, rc=localrc)
  if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting local DE list from array",  &
          rcToReturn=rc)) return

  allocate(larrayList(localDeCount), stat=allocRcToTest)
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="type "//                      &
     " larrayList in populate_redist_array", rcToReturn=rc)) then
  endif
  call ESMF_ArrayGet(array, localarrayList=larrayList, rc=localrc)
  if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting local array list",          &
          rcToReturn=rc)) return

  !-----------------------------------------------------------------------------
  ! get dimcount to allocate bound arrays
  !-----------------------------------------------------------------------------
  call ESMF_DistGridGet(DistGrid, dimCount=dimCount, rc=localrc)
  if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting dimCount from distGrid",    &
          rcToReturn=rc)) return
  
  allocate(UBnd(dimCount, localDeCount), stat=allocRcToTest )
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//          &
     " UBnd in populate_redist_array", rcToReturn=rc)) then
  endif
  allocate(LBnd(dimCount, localDeCount), stat=allocRcToTest )  
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable "//          &
     " LBnd in populate_redist_array", rcToReturn=rc)) then
  endif

  call ESMF_ArrayGet(array, indexflag=indexflag,                               &
           exclusiveLBound=LBnd, exclusiveUBound=UBnd, rc=localrc)
  if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting exclusive bound range",     &
          rcToReturn=rc)) return

  !-----------------------------------------------------------------------------
  ! associate the fortran pointer with the array object and populate the array
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  ! Memory Rank = Grid Rank, then there are no tensor dimensions
  !-----------------------------------------------------------------------------
  if( Memory%memRank ==  Memory%GridRank ) then

     select case(dimCount)
     case(1)
     !--------------------------------------------------------------------------
     ! rank = 1
     !--------------------------------------------------------------------------
        do de=1, localDeCount
           call ESMF_LocalArrayGet(larrayList(de), farrayPtr=farrayPtr1, &
                                   datacopyflag=ESMF_DATACOPY_REFERENCE, rc=localrc) 
           if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error connecting pointer to " // &
                   "array list", rcToReturn=rc)) return

           do i1=LBnd(1,de), UBnd(1,de)
              farrayPtr1(i1) = localDeToDeMap(de) + 1000.0d0*i1
           enddo    !   i1
        enddo    ! de
     case(2)
     !--------------------------------------------------------------------------
     ! rank = 2
     !--------------------------------------------------------------------------
        do de=1, localDeCount
           call ESMF_LocalArrayGet(larrayList(de), farrayPtr=farrayPtr2, &
                                   datacopyflag=ESMF_DATACOPY_REFERENCE, rc=localrc) 
           if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error connecting pointer to " // &
                   "array list", rcToReturn=rc)) return

           do i1=LBnd(1,de), UBnd(1,de)
              do i2=LBnd(2,de), UBnd(2,de)
                 farrayPtr2(i1,i2) = localDeToDeMap(de) + 1000.0d0*i1 + 0.001d0*i2
              enddo   !   i2
           enddo    !   i1
        enddo    ! de
     case(3)
     !--------------------------------------------------------------------------
     ! rank = 3
     !--------------------------------------------------------------------------
        do de=1, localDeCount
           call ESMF_LocalArrayGet(larrayList(de), farrayPtr=farrayPtr3, &
                                   datacopyflag=ESMF_DATACOPY_REFERENCE, rc=localrc) 
           if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error connecting pointer to " // &
                   "array list", rcToReturn=rc)) return

           do i1=LBnd(1,de), UBnd(1,de)
              do i2=LBnd(2,de), UBnd(2,de)
                 do i3=LBnd(3,de), UBnd(3,de)
                    farrayPtr3(i1,i2,i3) = localDeToDeMap(de) + 1.0d4*i1 + 10.0d2*i2 &
                          + 1.0d-2*i3
                 enddo   !   i3
              enddo   !   i2
           enddo    !   i1
        enddo    ! de
     case(4)
     !--------------------------------------------------------------------------
     ! rank = 4
     !--------------------------------------------------------------------------
        do de=1, localDeCount
           call ESMF_LocalArrayGet(larrayList(de), farrayPtr=farrayPtr4, &
                                   datacopyflag=ESMF_DATACOPY_REFERENCE, rc=localrc) 
           if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error connecting pointer to " // &
                   "array list", rcToReturn=rc)) return

           do i1=LBnd(1,de), UBnd(1,de)
              do i2=LBnd(2,de), UBnd(2,de)
                 do i3=LBnd(3,de), UBnd(3,de)
                    do i4=LBnd(4,de), UBnd(4,de)
                       farrayPtr4(i1,i2,i3,i4) = localDeToDeMap(de) + 1.0d4*i1         &
                             + 1.0d2*i2 + 1.0d-2*i3 + 1.0d-4*i4 
                    enddo   !   i4
                 enddo   !   i3
              enddo   !   i2
           enddo    !   i1
        enddo    ! de
#ifndef ESMF_NO_GREATER_THAN_4D
     case(5)
     !--------------------------------------------------------------------------
     ! rank = 5
     !--------------------------------------------------------------------------
        do de=1, localDeCount
           call ESMF_LocalArrayGet(larrayList(de), farrayPtr=farrayPtr5, &
                                   datacopyflag=ESMF_DATACOPY_REFERENCE, rc=localrc) 
           if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error connecting pointer to " // &
                   "array list", rcToReturn=rc)) return

           do i1=LBnd(1,de), UBnd(1,de)
              do i2=LBnd(2,de), UBnd(2,de)
                 do i3=LBnd(3,de), UBnd(3,de)
                    do i4=LBnd(4,de), UBnd(4,de)
                       do i5=LBnd(5,de), UBnd(5,de)
                          farrayPtr5(i1,i2,i3,i4,i5) = localDeToDeMap(de) + 1.0d4*i1   &
                             + 1.0d2*i2 + 1.0d0*i3 + 1.0d-2*i4  + 1.0d-4*i5
                       enddo   !   i5
                    enddo   !   i4
                 enddo   !   i3
              enddo   !   i2
           enddo    !   i1
        enddo    ! de
     case(6)
     !--------------------------------------------------------------------------
     ! rank = 6
     !--------------------------------------------------------------------------
        do de=1, localDeCount
           call ESMF_LocalArrayGet(larrayList(de), farrayPtr=farrayPtr6, &
                                   datacopyflag=ESMF_DATACOPY_REFERENCE, rc=localrc) 
           if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error connecting pointer to " // &
                   "array list", rcToReturn=rc)) return

           do i1=LBnd(1,de), UBnd(1,de)
              do i2=LBnd(2,de), UBnd(2,de)
                 do i3=LBnd(3,de), UBnd(3,de)
                    do i4=LBnd(4,de), UBnd(4,de)
                       do i5=LBnd(5,de), UBnd(5,de)
                       do i6=LBnd(6,de), UBnd(6,de)
                       farrayPtr6(i1,i2,i3,i4,i5,i6) = localDeToDeMap(de) +            &
                             1.0d5*i1 + 1.0d3*i2 + 1.0d1*i3 + 1.0d-1*i4        &
                             + 1.0d-3*i5 + 1.0d-5*i6
                       enddo   !   i6
                       enddo   !   i5
                    enddo   !   i4
                 enddo   !   i3
              enddo   !   i2
           enddo    !   i1
        enddo    ! de
     case(7)
     !--------------------------------------------------------------------------
     ! rank = 7
     !--------------------------------------------------------------------------
        do de=1, localDeCount
           call ESMF_LocalArrayGet(larrayList(de), farrayPtr=farrayPtr7, &
                                   datacopyflag=ESMF_DATACOPY_REFERENCE, rc=localrc) 
           if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error connecting pointer to " // &
                   "array list", rcToReturn=rc)) return

           do i1=LBnd(1,de), UBnd(1,de)
              do i2=LBnd(2,de), UBnd(2,de)
                 do i3=LBnd(3,de), UBnd(3,de)
                    do i4=LBnd(4,de), UBnd(4,de)
                       do i5=LBnd(5,de), UBnd(5,de)
                       do i6=LBnd(6,de), UBnd(6,de)
                       do i7=LBnd(7,de), UBnd(7,de)
                          farrayPtr7(i1,i2,i3,i4,i5,i6,i7) = localDeToDeMap(de) +      &
                             1.0d5*i1 + 1.0d3*i2 + 1.0d1*i3 + 1.0d-1*i4        &
                             + 1.0d-3*i5 + 1.0d-5*i6 + 1.0d0*i7
                       enddo   !   i7
                       enddo   !   i6
                       enddo   !   i5
                    enddo   !   i4
                 enddo   !   i3
              enddo   !   i2
           enddo    !   i1
        enddo    ! de
#endif
     case default
     !--------------------------------------------------------------------------
     ! error
     !--------------------------------------------------------------------------
        localrc = ESMF_FAILURE
        call ESMF_LogSetError(ESMF_FAILURE, msg="DimCount out of range", &
                 rcToReturn=localrc)
        return
     end select

  !-----------------------------------------------------------------------------
  ! Memory Rank > Grid Rank, then there are MemRank-GridRank tensor dimensions
  !-----------------------------------------------------------------------------
  elseif( Memory%memRank >  Memory%GridRank ) then
! -----------
      localrc = ESMF_FAILURE
      call ESMF_LogSetError(ESMF_FAILURE, msg="tensor dimensions not supported",      &
                    rcToReturn=localrc)
      return

  endif

  !-----------------------------------------------------------------------------
  ! clean up allocated arrays
  !-----------------------------------------------------------------------------
  deallocate(localDeToDeMap)
  deallocate(LBnd, UBnd)
  deallocate(larrayList)

  !-----------------------------------------------------------------------------
  rc = ESMF_SUCCESS     
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  end subroutine populate_redist_array 
  !-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine populate_array_value(Array, value, DistGrid, Memory, Grid, rc)
  !-----------------------------------------------------------------------------
  ! routie populates an esmf array to a constant value. Typically used for 
  ! initialization.
  !
  !-----------------------------------------------------------------------------
  ! arguments
  type(ESMF_Array), intent(inout) :: Array
  real(ESMF_KIND_R8), intent(in   ) :: value
  type(ESMF_DistGrid), intent(in   ) :: DistGrid
  type(memory_config), intent(in   ) :: Memory
  type(grid_specification_record), intent(in   ) :: Grid
  integer, intent(inout) :: rc
 
  ! local ESMF types
  type(ESMF_LocalArray), allocatable :: larrayList(:)
  type(ESMF_Index_Flag) :: indexflag

  ! local integer variables
  integer :: de, localDeCount, dimCount 
  integer, allocatable ::  localDeToDeMap(:)
  integer, allocatable :: LBnd(:,:), UBnd(:,:) 
  integer :: i1, i2, i3, i4, i5, i6, i7
  ! integer :: irank, k, tensorsize, fsize(7)
  ! integer, allocatable :: haloL(:), haloR(:)
  ! integer, allocatable :: top(:), bottom(:)
  integer :: localrc ! local error status
  integer :: allocRcToTest

  ! local real variables
  real(ESMF_KIND_R8), pointer :: farrayPtr1(:), farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr3(:,:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr4(:,:,:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr5(:,:,:,:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr6(:,:,:,:,:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr7(:,:,:,:,:,:,:)

  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL
  rc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! get local array DE list
  !-----------------------------------------------------------------------------
  call ESMF_ArrayGet(array, localDeCount=localDeCount, rc=localrc)
  if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting local DE count from array", &
          rcToReturn=rc)) return

  allocate(localDeToDeMap(localDeCount), stat=allocRcToTest )
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable"//           &
     " localDeToDeMap in populate_array_value", rcToReturn=rc)) then
  endif
  call ESMF_ArrayGet(array, localDeToDeMap=localDeToDeMap, rc=localrc)
  if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting local DE list from array",  &
          rcToReturn=rc)) return

  allocate(larrayList(localDeCount), stat=allocRcToTest )
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="type "//                      &
     " larrayList in populate_array_value", rcToReturn=rc)) then
  endif
  call ESMF_ArrayGet(array, localarrayList=larrayList, rc=localrc)
  if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting local array list",          &
          rcToReturn=rc)) return

  !-----------------------------------------------------------------------------
  ! get dimcount to allocate bound arrays
  !-----------------------------------------------------------------------------
  call ESMF_DistGridGet(DistGrid, dimCount=dimCount, rc=localrc)
  if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting dimCount from distGrid",    &
          rcToReturn=rc)) return
  
  allocate(UBnd(dimCount, localDeCount), stat=allocRcToTest)
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable"//           &
     " UBnd in populate_array_value", rcToReturn=rc)) then
  endif
  allocate(LBnd(dimCount, localDeCount), stat=allocRcToTest)  
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable"//           &
     " LBnd in populate_array_value", rcToReturn=rc)) then
  endif

  call ESMF_ArrayGet(array, indexflag=indexflag,                               &
           exclusiveLBound=LBnd, exclusiveUBound=UBnd, rc=localrc)
  if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting exclusive bound range",     &
          rcToReturn=rc)) return

  !-----------------------------------------------------------------------------
  ! associate the fortran pointer with the array object and populate the array
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  ! Memory Rank = Grid Rank, then there are no tensor dimensions
  !-----------------------------------------------------------------------------
  if( Memory%memRank ==  Memory%GridRank ) then

     select case(dimCount)
     case(1)
     !--------------------------------------------------------------------------
     ! rank = 1
     !--------------------------------------------------------------------------
        do de=1, localDeCount
           call ESMF_LocalArrayGet(larrayList(de), farrayPtr=farrayPtr1, &
                                   datacopyflag=ESMF_DATACOPY_REFERENCE, rc=localrc) 
           if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error connecting pointer to " // &
                   "array list", rcToReturn=rc)) return

           do i1=LBnd(1,de), UBnd(1,de)
              farrayPtr1(i1) =  value
           enddo    !   i1
        enddo    ! de
     case(2)
     !--------------------------------------------------------------------------
     ! rank = 2
     !--------------------------------------------------------------------------
        do de=1, localDeCount
           call ESMF_LocalArrayGet(larrayList(de), farrayPtr=farrayPtr2, &
                                   datacopyflag=ESMF_DATACOPY_REFERENCE, rc=localrc) 
           if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error connecting pointer to " // &
                   "array list", rcToReturn=rc)) return

           do i1=LBnd(1,de), UBnd(1,de)
              do i2=LBnd(2,de), UBnd(2,de)
                 farrayPtr2(i1,i2) = value
              enddo   !   i2
           enddo    !   i1
        enddo    ! de
     case(3)
     !--------------------------------------------------------------------------
     ! rank = 3
     !--------------------------------------------------------------------------
        do de=1, localDeCount
           call ESMF_LocalArrayGet(larrayList(de), farrayPtr=farrayPtr3, &
                                   datacopyflag=ESMF_DATACOPY_REFERENCE, rc=localrc) 
           if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error connecting pointer to " // &
                   "array list", rcToReturn=rc)) return

           do i1=LBnd(1,de), UBnd(1,de)
              do i2=LBnd(2,de), UBnd(2,de)
                 do i3=LBnd(3,de), UBnd(3,de)
                    farrayPtr3(i1,i2,i3) = value
                 enddo   !   i3
              enddo   !   i2
           enddo    !   i1
        enddo    ! de
     case(4)
     !--------------------------------------------------------------------------
     ! rank = 4
     !--------------------------------------------------------------------------
        do de=1, localDeCount
           call ESMF_LocalArrayGet(larrayList(de), farrayPtr=farrayPtr4, &
                                   datacopyflag=ESMF_DATACOPY_REFERENCE, rc=localrc) 
           if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error connecting pointer to " // &
                   "array list", rcToReturn=rc)) return

           do i1=LBnd(1,de), UBnd(1,de)
              do i2=LBnd(2,de), UBnd(2,de)
                 do i3=LBnd(3,de), UBnd(3,de)
                    do i4=LBnd(4,de), UBnd(4,de)
                       farrayPtr4(i1,i2,i3,i4) = value
                    enddo   !   i4
                 enddo   !   i3
              enddo   !   i2
           enddo    !   i1
        enddo    ! de
#ifndef ESMF_NO_GREATER_THAN_4D
     case(5)
     !--------------------------------------------------------------------------
     ! rank = 5
     !--------------------------------------------------------------------------
        do de=1, localDeCount
           call ESMF_LocalArrayGet(larrayList(de), farrayPtr=farrayPtr5, &
                                   datacopyflag=ESMF_DATACOPY_REFERENCE, rc=localrc) 
           if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error connecting pointer to " // &
                   "array list", rcToReturn=rc)) return

           do i1=LBnd(1,de), UBnd(1,de)
              do i2=LBnd(2,de), UBnd(2,de)
                 do i3=LBnd(3,de), UBnd(3,de)
                    do i4=LBnd(4,de), UBnd(4,de)
                       do i5=LBnd(5,de), UBnd(5,de)
                          farrayPtr5(i1,i2,i3,i4,i5) =  value
                       enddo   !   i5
                    enddo   !   i4
                 enddo   !   i3
              enddo   !   i2
           enddo    !   i1
        enddo    ! de
     case(6)
     !--------------------------------------------------------------------------
     ! rank = 6
     !--------------------------------------------------------------------------
        do de=1, localDeCount
           call ESMF_LocalArrayGet(larrayList(de), farrayPtr=farrayPtr6, &
                                   datacopyflag=ESMF_DATACOPY_REFERENCE, rc=localrc) 
           if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error connecting pointer to " // &
                   "array list", rcToReturn=rc)) return

           do i1=LBnd(1,de), UBnd(1,de)
              do i2=LBnd(2,de), UBnd(2,de)
                 do i3=LBnd(3,de), UBnd(3,de)
                    do i4=LBnd(4,de), UBnd(4,de)
                       do i5=LBnd(5,de), UBnd(5,de)
                       do i6=LBnd(6,de), UBnd(6,de)
                       farrayPtr6(i1,i2,i3,i4,i5,i6) =  value
                       enddo   !   i6
                       enddo   !   i5
                    enddo   !   i4
                 enddo   !   i3
              enddo   !   i2
           enddo    !   i1
        enddo    ! de
     case(7)
     !--------------------------------------------------------------------------
     ! rank = 7
     !--------------------------------------------------------------------------
        do de=1, localDeCount
           call ESMF_LocalArrayGet(larrayList(de), farrayPtr=farrayPtr7, &
                                   datacopyflag=ESMF_DATACOPY_REFERENCE, rc=localrc) 
           if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error connecting pointer to " // &
                   "array list", rcToReturn=rc)) return

           do i1=LBnd(1,de), UBnd(1,de)
              do i2=LBnd(2,de), UBnd(2,de)
                 do i3=LBnd(3,de), UBnd(3,de)
                    do i4=LBnd(4,de), UBnd(4,de)
                       do i5=LBnd(5,de), UBnd(5,de)
                       do i6=LBnd(6,de), UBnd(6,de)
                       do i7=LBnd(7,de), UBnd(7,de)
                          farrayPtr7(i1,i2,i3,i4,i5,i6,i7) = value
                       enddo   !   i7
                       enddo   !   i6
                       enddo   !   i5
                    enddo   !   i4
                 enddo   !   i3
              enddo   !   i2
           enddo    !   i1
        enddo    ! de
#endif
     case default
     !--------------------------------------------------------------------------
     ! error
     !--------------------------------------------------------------------------
        localrc = ESMF_FAILURE
        call ESMF_LogSetError(ESMF_FAILURE, msg="DimCount out of range", &
                 rcToReturn=localrc)
        return
     end select

  !-----------------------------------------------------------------------------
  ! Memory Rank > Grid Rank, then there are MemRank-GridRank tensor dimensions
  !-----------------------------------------------------------------------------
  elseif( Memory%memRank >  Memory%GridRank ) then
! -----------
  endif

  !-----------------------------------------------------------------------------
  ! clean up allocated arrays
  !-----------------------------------------------------------------------------
  deallocate(localDeToDeMap)
  deallocate(LBnd, UBnd)
  deallocate(larrayList)

  !-----------------------------------------------------------------------------
  rc = ESMF_SUCCESS     
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  end subroutine populate_array_value 
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine compare_redist_array(test_status, Array1, Array2,                 &
                                  DistGrid1, DistGrid2,                        &
                                  Memory, Grid, rc)
  !-----------------------------------------------------------------------------
  ! routine compares the contents of two arrays and returns if they agree
  !-----------------------------------------------------------------------------
  ! arguments
  integer, intent(inout) :: test_status
  type(ESMF_Array), intent(in   ) :: Array1, Array2
  type(ESMF_DistGrid), intent(in   ) :: DistGrid1, DistGrid2
  type(memory_config), intent(in   ) :: Memory
  type(grid_specification_record), intent(in   ) :: Grid
  integer, intent(inout) :: rc
 
  ! local ESMF types
  type(ESMF_LocalArray), allocatable :: larrayList1(:)
  type(ESMF_LocalArray), allocatable :: larrayList2(:)
  type(ESMF_Index_Flag) :: indexflag

  ! local integer variables
  integer :: de, i1, i2, i3, i4, i5, i6, i7, k
  integer :: localDeCount1, dimCount1, localDeCount2, dimCount2
  integer, allocatable ::  localDeToDeMap1(:), localDeToDeMap2(:)
  integer, allocatable :: LBnd(:,:), UBnd(:,:) 
  integer, allocatable :: LBnd2(:,:), UBnd2(:,:) 
  ! integer :: irank, tensorsize, fsize(7)
  ! integer, allocatable :: haloL(:), haloR(:)
  ! integer, allocatable :: top(:), bottom(:)
  integer :: allocRcToTest
  integer :: localrc ! local error status

  ! local logicals
  ! logical :: nohaloflag

  ! local real variables
  real(ESMF_KIND_R8), pointer :: farray1D(:), farray2D(:,:)
  real(ESMF_KIND_R8), pointer :: farray3D(:,:,:)
  real(ESMF_KIND_R8), pointer :: farray4D(:,:,:,:)
  real(ESMF_KIND_R8), pointer :: farray5D(:,:,:,:,:)
  real(ESMF_KIND_R8), pointer :: farray6D(:,:,:,:,:,:)
  real(ESMF_KIND_R8), pointer :: farray7D(:,:,:,:,:,:,:)

  real(ESMF_KIND_R8), pointer :: rarray1D(:), rarray2D(:,:)
  real(ESMF_KIND_R8), pointer :: rarray3D(:,:,:)
  real(ESMF_KIND_R8), pointer :: rarray4D(:,:,:,:)
  real(ESMF_KIND_R8), pointer :: rarray5D(:,:,:,:,:)
  real(ESMF_KIND_R8), pointer :: rarray6D(:,:,:,:,:,:)
  real(ESMF_KIND_R8), pointer :: rarray7D(:,:,:,:,:,:,:)


  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL
  rc = ESMF_RC_NOT_IMPL
  test_status = HarnessTest_SUCCESS

  !-----------------------------------------------------------------------------
  ! Sanity check - confirm that the two arrays have the same ranks and sizes
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  ! get local array DE list from array1
  !-----------------------------------------------------------------------------
  call ESMF_ArrayGet(array1, localDeCount=localDeCount1, rc=localrc)
  if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting local DE count from array", &
          rcToReturn=rc)) return

  allocate(localDeToDeMap1(localDeCount1), stat=allocRcToTest )
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable"//           &
     " localDeToDeMap1 in compare redist array", rcToReturn=rc)) then
  endif
  call ESMF_ArrayGet(array1, localDeToDeMap=localDeToDeMap1, rc=localrc)
  if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting local DE list from array",  &
          rcToReturn=rc)) return

  allocate(larrayList1(localDeCount1), stat=allocRcToTest )
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="char variable"//              &
     " larrayList1 in compare redist array", rcToReturn=rc)) then
  endif
  call ESMF_ArrayGet(array1, localarrayList=larrayList1, rc=localrc)
  if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting local array list",          &
          rcToReturn=rc)) return

  call ESMF_DistGridGet(DistGrid1, dimCount=dimCount1, rc=localrc)
  if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting dimCount from distGrid",    &
          rcToReturn=rc)) return

  !-----------------------------------------------------------------------------
  ! get local array DE list from array2
  !-----------------------------------------------------------------------------
  call ESMF_ArrayGet(array2, localDeCount=localDeCount2, rc=localrc)
  if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting local DE count from array", &
          rcToReturn=rc)) return

  ! check localDeCount for agreement
  if( localDeCount1 /= localDeCount2 ) then
     localrc = ESMF_FAILURE
     call ESMF_LogSetError(ESMF_FAILURE, msg="local De Counts do not agree",     &
              rcToReturn=localrc)
     return
  endif

  allocate(localDeToDeMap2(localDeCount2), stat=allocRcToTest )
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable"//           &
     " localDeToDeMap2 in compare redist array", rcToReturn=rc)) then
  endif
  call ESMF_ArrayGet(array2, localDeToDeMap=localDeToDeMap2, rc=localrc)
  if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting local DE list from array",  &
          rcToReturn=rc)) return

  ! check localDeToDeMap for agreement
  do de=1, localDeCount2
     if( localDeToDeMap2(de) /= localDeToDeMap1(de) ) then
        localrc = ESMF_FAILURE
        call ESMF_LogSetError(ESMF_FAILURE, msg="local De lists do not agree",   &
                 rcToReturn=localrc)
        return
     endif
  enddo

  ! get local De List
  allocate(larrayList2(localDeCount2), stat=allocRcToTest )
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="char variable"//              &
     " larrayList2 in compare redist array", rcToReturn=rc)) then
  endif
  call ESMF_ArrayGet(array2, localarrayList=larrayList2, rc=localrc)
  if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting local array list",          &
          rcToReturn=rc)) return

  !-----------------------------------------------------------------------------
  ! compare dimcounts for both arrays 
  !-----------------------------------------------------------------------------
  call ESMF_DistGridGet(DistGrid2, dimCount=dimCount2, rc=localrc)
  if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting dimCount from distGrid",    &
          rcToReturn=rc)) return
  
  ! check localDeCount for agreement
  if( dimCount1 /= dimCount2 ) then
     localrc = ESMF_FAILURE
     call ESMF_LogSetError(ESMF_FAILURE, msg="array 1 and 2 dimCounts disagree", &
              rcToReturn=localrc)
     return
  endif

  !-----------------------------------------------------------------------------
  ! allocate bound arrays and extract exclusive bounds
  !-----------------------------------------------------------------------------
  allocate(UBnd(dimCount1, localDeCount1), stat=allocRcToTest )
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable"//           &
     " UBnd in compare redist array", rcToReturn=rc)) then
  endif
  allocate(LBnd(dimCount1, localDeCount1), stat=allocRcToTest )  
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable"//           &
     " LBnd in compare redist array", rcToReturn=rc)) then
  endif

  call ESMF_ArrayGet(array=array1, indexflag=indexflag,                        &
           exclusiveLBound=LBnd, exclusiveUBound=UBnd, rc=localrc)
  if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting exclusive bound range",     &
          rcToReturn=rc)) return


  allocate(UBnd2(dimCount2, localDeCount2), stat=allocRcToTest )
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable"//           &
     " UBnd2 in compare redist array", rcToReturn=rc)) then
  endif
  allocate(LBnd2(dimCount2, localDeCount2), stat=allocRcToTest )  
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer variable"//           &
     " LBnd2 in compare redist array", rcToReturn=rc)) then
  endif

  call ESMF_ArrayGet(array=array2, indexflag=indexflag,                        &
           exclusiveLBound=LBnd2, exclusiveUBound=UBnd2, rc=localrc)
  if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error getting exclusive bound range",     &
          rcToReturn=rc)) return

  !-----------------------------------------------------------------------------
  ! check for exclusive bound agreement 
  !-----------------------------------------------------------------------------
  do de=1, localDeCount1
     do k=1,dimCount1
        if( LBnd(k,de) /= LBnd2(k,de) ) then
           print*,'exclusive Lower bounds disagree',LBnd(k,de),LBnd2(k,de) 
           localrc = ESMF_FAILURE
           call ESMF_LogSetError(ESMF_FAILURE,msg="exclusive L bounds disagree",&
              rcToReturn=localrc)
           return
        endif
        if( UBnd(k,de) /= UBnd2(k,de) ) then
           print*,'exclusive Upper bounds disagree',UBnd(k,de),UBnd2(k,de) 
           localrc = ESMF_FAILURE
           call ESMF_LogSetError(ESMF_FAILURE,msg="exclusive U bounds disagree",&
              rcToReturn=localrc)
           return
        endif
     enddo
  enddo
  !-----------------------------------------------------------------------------
  ! associate fortran pointers with the two local array objects and compare
  ! entries
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  ! Memory Rank = Grid Rank, then there are no tensor dimensions
  !-----------------------------------------------------------------------------
  if( Memory%memRank ==  Memory%GridRank ) then

     select case(dimCount1)
     case(1)
     !--------------------------------------------------------------------------
     ! rank = 1
     !--------------------------------------------------------------------------
        do de=1, localDeCount1
           call ESMF_LocalArrayGet(larrayList1(de), farrayPtr=farray1D,             &
                    datacopyflag=ESMF_DATACOPY_REFERENCE, rc=localrc)

           if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error connecting pointer to " // &
                  "array1 list", rcToReturn=rc)) return

           call ESMF_LocalArrayGet(larrayList2(de), farrayPtr=rarray1D,             &
                    datacopyflag=ESMF_DATACOPY_REFERENCE, rc=localrc)

           if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error connecting pointer to " // &
                   "array2 list", rcToReturn=rc)) return

           do i1=LBnd(1,de), UBnd(1,de)
              if( farray1D(i1) /= rarray1D(i1) ) then
                 test_status = HarnessTest_FAILURE
                 print*,' arrays disagree ',i1,farray1D(i1),                   &
                        rarray1D(i1)
              endif
           enddo    !   i1
        enddo    ! de
     case(2)
     !--------------------------------------------------------------------------
     ! rank = 2
     !--------------------------------------------------------------------------
        do de=1, localDeCount1
           call ESMF_LocalArrayGet(larrayList1(de), farrayPtr=farray2D,             &
                    datacopyflag=ESMF_DATACOPY_REFERENCE, rc=localrc)

           if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error connecting pointer to " // &
                  "array1 list", rcToReturn=rc)) return

           call ESMF_LocalArrayGet(larrayList2(de), farrayPtr=rarray2D,             &
                    datacopyflag=ESMF_DATACOPY_REFERENCE, rc=localrc)

           if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error connecting pointer to " // &
                   "array2 list", rcToReturn=rc)) return

           do i1=LBnd(1,de), UBnd(1,de)
             do i2=LBnd(2,de), UBnd(2,de)
               if( farray2D(i1,i2) /= rarray2D(i1,i2) ) then
                 test_status = HarnessTest_FAILURE
                 print*,' arrays disagree ',i1,i2,farray2D(i1,i2),             &
                        rarray2D(i1,i2)
               endif
             enddo   !   i2
           enddo    !   i1
        enddo    ! de
     case(3)
     !--------------------------------------------------------------------------
     ! rank = 3
     !--------------------------------------------------------------------------
        do de=1, localDeCount1
           call ESMF_LocalArrayGet(larrayList1(de), farrayPtr=farray3D,             &
                    datacopyflag=ESMF_DATACOPY_REFERENCE, rc=localrc)

           if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error connecting pointer to " // &
                  "array1 list", rcToReturn=rc)) return

           call ESMF_LocalArrayGet(larrayList2(de), farrayPtr=rarray3D,             &
                    datacopyflag=ESMF_DATACOPY_REFERENCE, rc=localrc)

           if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error connecting pointer to " // &
                   "array2 list", rcToReturn=rc)) return

           do i1=LBnd(1,de), UBnd(1,de)
             do i2=LBnd(2,de), UBnd(2,de)
                do i3=LBnd(3,de), UBnd(3,de)
                   if( farray3D(i1,i2,i3) /= rarray3D(i1,i2,i3) ) then
                       test_status = HarnessTest_FAILURE
                       print*,' arrays disagree ',i1,i2,i3,farray3D(i1,i2,i3), &
                              rarray3D(i1,i2,i3)
                   endif
                enddo   !   i3
             enddo   !   i2
           enddo    !   i1
        enddo    ! de
     case(4)
     !--------------------------------------------------------------------------
     ! rank = 4
     !--------------------------------------------------------------------------
        do de=1, localDeCount1
           call ESMF_LocalArrayGet(larrayList1(de), farrayPtr=farray4D,             &
                    datacopyflag=ESMF_DATACOPY_REFERENCE, rc=localrc)

           if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error connecting pointer to " // &
                  "array1 list", rcToReturn=rc)) return

           call ESMF_LocalArrayGet(larrayList2(de), farrayPtr=rarray4D,             &
                    datacopyflag=ESMF_DATACOPY_REFERENCE, rc=localrc)

           if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error connecting pointer to " // &
                   "array2 list", rcToReturn=rc)) return

           do i1=LBnd(1,de), UBnd(1,de)
             do i2=LBnd(2,de), UBnd(2,de)
                do i3=LBnd(3,de), UBnd(3,de)
                   do i4=LBnd(4,de), UBnd(4,de)
                      if( farray4D(i1,i2,i3,i4) /= rarray4D(i1,i2,i3,i4) ) then
                       test_status = HarnessTest_FAILURE
                       print*,' arrays disagree ',i1,i2,i3,i4,                 &
                              farray4D(i1,i2,i3,i4), rarray4D(i1,i2,i3,i4)
                      endif
                   enddo   !   i4
                enddo   !   i3
             enddo   !   i2
           enddo    !   i1
        enddo    ! de
#ifndef ESMF_NO_GREATER_THAN_4D    
     case(5)
     !--------------------------------------------------------------------------
     ! rank = 5
     !--------------------------------------------------------------------------
        do de=1, localDeCount1
           call ESMF_LocalArrayGet(larrayList1(de), farrayPtr=farray5D,             &
                    datacopyflag=ESMF_DATACOPY_REFERENCE, rc=localrc)

           if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error connecting pointer to " // &
                  "array1 list", rcToReturn=rc)) return

           call ESMF_LocalArrayGet(larrayList2(de), farrayPtr=rarray5D,             &
                    datacopyflag=ESMF_DATACOPY_REFERENCE, rc=localrc)

           if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error connecting pointer to " // &
                   "array2 list", rcToReturn=rc)) return

           do i1=LBnd(1,de), UBnd(1,de)
             do i2=LBnd(2,de), UBnd(2,de)
                do i3=LBnd(3,de), UBnd(3,de)
                   do i4=LBnd(4,de), UBnd(4,de)
                      do i5=LBnd(5,de), UBnd(5,de)
                         if( farray5D(i1,i2,i3,i4,i5) /=                       &
                             rarray5D(i1,i2,i3,i4,i5) ) then
                               test_status = HarnessTest_FAILURE
                               print*,' arrays disagree ',i1,i2,i3,i4,i5,      &
                              farray5D(i1,i2,i3,i4,i5),                        &
                              rarray5D(i1,i2,i3,i4,i5)
                         endif
                      enddo   !   i5
                   enddo   !   i4
                enddo   !   i3
             enddo   !   i2
           enddo    !   i1
        enddo    ! de
     case(6)
     !--------------------------------------------------------------------------
     ! rank = 6
     !--------------------------------------------------------------------------
        do de=1, localDeCount1
           call ESMF_LocalArrayGet(larrayList1(de), farrayPtr=farray6D,             &
                    datacopyflag=ESMF_DATACOPY_REFERENCE, rc=localrc)

           if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error connecting pointer to " // &
                  "array1 list", rcToReturn=rc)) return

           call ESMF_LocalArrayGet(larrayList2(de), farrayPtr=rarray6D,             &
                    datacopyflag=ESMF_DATACOPY_REFERENCE, rc=localrc)

           if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error connecting pointer to " // &
                   "array2 list", rcToReturn=rc)) return

           do i1=LBnd(1,de), UBnd(1,de)
             do i2=LBnd(2,de), UBnd(2,de)
                do i3=LBnd(3,de), UBnd(3,de)
                   do i4=LBnd(4,de), UBnd(4,de)
                      do i5=LBnd(5,de), UBnd(5,de)
                      do i6=LBnd(6,de), UBnd(6,de)
                         if( farray6D(i1,i2,i3,i4,i5,i6) /=                    &
                             rarray6D(i1,i2,i3,i4,i5,i6) ) then
                               test_status = HarnessTest_FAILURE
                               print*,' arrays disagree ',i1,i2,i3,i4,i5,i6,   &
                              farray6D(i1,i2,i3,i4,i5,i6),                     &
                              rarray6D(i1,i2,i3,i4,i5,i6)
                         endif
                      enddo   !   i6
                      enddo   !   i5
                   enddo   !   i4
                enddo   !   i3
             enddo   !   i2
           enddo    !   i1
        enddo    ! de
     case(7)
     !--------------------------------------------------------------------------
     ! rank = 7
     !--------------------------------------------------------------------------
        do de=1, localDeCount1
           call ESMF_LocalArrayGet(larrayList1(de), farrayPtr=farray7D,             &
                    datacopyflag=ESMF_DATACOPY_REFERENCE, rc=localrc)

           if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error connecting pointer to " // &
                  "array1 list", rcToReturn=rc)) return

           call ESMF_LocalArrayGet(larrayList2(de), farrayPtr=rarray7D,             &
                    datacopyflag=ESMF_DATACOPY_REFERENCE, rc=localrc)

           if (CheckError(checkpoint, __LINE__, __FILE__, localrc,"error connecting pointer to " // &
                   "array2 list", rcToReturn=rc)) return

           do i1=LBnd(1,de), UBnd(1,de)
             do i2=LBnd(2,de), UBnd(2,de)
                do i3=LBnd(3,de), UBnd(3,de)
                   do i4=LBnd(4,de), UBnd(4,de)
                      do i5=LBnd(5,de), UBnd(5,de)
                      do i6=LBnd(6,de), UBnd(6,de)
                      do i7=LBnd(7,de), UBnd(7,de)
                         if( farray7D(i1,i2,i3,i4,i5,i6,i7) /=                 &
                             rarray7D(i1,i2,i3,i4,i5,i6,i7) ) then
                              test_status = HarnessTest_FAILURE
                              print*,' arrays disagree ',i1,i2,i3,i4,i5,i6,i7, &
                              farray7D(i1,i2,i3,i4,i5,i6,i7),                  &
                              rarray7D(i1,i2,i3,i4,i5,i6,i7)
                         endif
                      enddo   !   i7
                      enddo   !   i6
                      enddo   !   i5
                   enddo   !   i4
                enddo   !   i3
             enddo   !   i2
           enddo    !   i1
        enddo    ! de
#endif
     case default
        ! error
        localrc = ESMF_FAILURE
        call ESMF_LogSetError(ESMF_FAILURE, msg="DimCount out of range",  &
                 rcToReturn=localrc)
        return
     end select

  !-----------------------------------------------------------------------------
  ! Memory Rank > Grid Rank, then there are MemRank-GridRank tensor dimensions
  !-----------------------------------------------------------------------------
  elseif (Memory%memRank >  Memory%GridRank) then
    call ESMF_LogSetError (ESMF_FAILURE, msg="tensor dimensions not supported", &
         rcToReturn=rc)
    return

  !-----------------------------------------------------------------------------
  ! Memory Rank < Grid Rank, then parameters don't make sense
  !-----------------------------------------------------------------------------
  else
    call ESMF_LogSetError (ESMF_FAILURE, msg="memory rank < grid rank", &
         rcToReturn=rc)
    return
  endif

  !-----------------------------------------------------------------------------
  ! clean up allocated arrays
  !-----------------------------------------------------------------------------
  deallocate(larrayList1, larrayList2)
  deallocate(localDeToDeMap1, localDeToDeMap2)
  deallocate(LBnd, UBnd)
  deallocate( LBnd2, UBnd2 )

  !-----------------------------------------------------------------------------
  rc = ESMF_SUCCESS     
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  end subroutine compare_redist_array 
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  real(ESMF_KIND_R8) function create_coord(index, grid, axis, rc) 
  !-----------------------------------------------------------------------------
  ! selects coordinate value based upon specified grid type.
  !-----------------------------------------------------------------------------
  integer, intent(in   ) :: index, axis
  integer, intent(inout) :: rc
  type(grid_specification_record)::grid

  !-----------------------------------------------------------------------------
  !
  !-----------------------------------------------------------------------------
  select case( trim(grid%gtype(axis)%string) )
    case("UNI")
       create_coord = create_uniform_coord(index, grid%grange(axis,2),         &
                            grid%grange(axis,1), grid%gsize(axis))
    case("UNIFORM")
       create_coord = create_uniform_coord(index, grid%grange(axis,2),         &
                            grid%grange(axis,1), grid%gsize(axis))
    case("UNIFORM_POLE")
       create_coord = create_uniform_coord(index, grid%grange(axis,2),         &
                            grid%grange(axis,1), grid%gsize(axis))
    case("UNIFORM_PERIODIC")
       create_coord = create_uniform_coord(index, grid%grange(axis,2),         &
                            grid%grange(axis,1), grid%gsize(axis))
    case("GAUSS")
       create_coord = create_gaussian_coord(index, grid%grange(axis,2),        &
                            grid%grange(axis,1), grid%gsize(axis))
    case("GAUSSIAN")
       create_coord = create_gaussian_coord(index, grid%grange(axis,2),        &
                            grid%grange(axis,1), grid%gsize(axis))
    case("GAUSSIAN_POLE")
       create_coord = create_gaussian_coord(index, grid%grange(axis,2),        &
                            grid%grange(axis,1), grid%gsize(axis))
    case("GAUSSIAN_PERIODIC")
       create_coord = create_gaussian_coord(index, grid%grange(axis,2),        &
                            grid%grange(axis,1), grid%gsize(axis))
    case default
       print*,'Unsupported grid type',trim(grid%gtype(axis)%string)
       call ESMF_LogSetError( ESMF_FAILURE, msg="Unsupported grid type " //       &
               trim(grid%gtype(axis)%string), rcToReturn=rc)
       return

  end select

  !-----------------------------------------------------------------------------
  end function create_coord
  !-----------------------------------------------------------------------------

 !------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  real(ESMF_KIND_R8) function create_uniform_coord(k, finish, start, ncells) 
  !-----------------------------------------------------------------------------
  ! define the coordinate for a uniform grid in terms of the global index k,
  ! the top and bottom of the range (finish and start), and the total number
  ! of cells. 
  ! create_uniform_coord(1) = start
  ! create_uniform_coord(ncells) = finish
  !-----------------------------------------------------------------------------
  integer :: k, ncells
  real(ESMF_KIND_R8) :: finish, start
  !-----------------------------------------------------------------------------
  create_uniform_coord =  (k-1)*(finish-start)/(ncells-1) +start

  !-----------------------------------------------------------------------------
  end function create_uniform_coord
  !-----------------------------------------------------------------------------

 !------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  real(ESMF_KIND_R8) function create_gaussian_coord(k, finish, start, ncells) 
  !-----------------------------------------------------------------------------
  ! define the coordinates for a gaussian grid in terms of the global index k,
  ! the top and bottom of the range (finish and start), and the total number
  ! of cells. 
  ! create_uniform_coord(1) = start
  ! create_uniform_coord(ncells) = finish
  !-----------------------------------------------------------------------------
  integer :: k, ncells
  real(ESMF_KIND_R8) :: finish, start, coord
  real(ESMF_KIND_R8), allocatable :: root(:), w(:)

  ! allocate work space
  allocate( root(ncells), w(ncells) )

  !-----------------------------------------------------------------------------
  ! compute all the roots (we only are asking for one, but I can't figure out 
  ! how to only compute the one we want).
  !-----------------------------------------------------------------------------
  call legendre_roots(ncells,root,w)

  !-----------------------------------------------------------------------------
  ! pih - root(ncells+1-k) constructs gaussian grid on the interval -pi to pi
  ! shift to 0:2*(root(ncells)-pih)
  !-----------------------------------------------------------------------------
  coord =  root(ncells) - root(ncells+1-k) 

  ! adjust range from start to finish 
  create_gaussian_coord = coord * 0.5*(finish - start)/(root(ncells)-pih) + start

  !-----------------------------------------------------------------------------
  ! clean up
  !-----------------------------------------------------------------------------
  deallocate( root, w )

  !-----------------------------------------------------------------------------
  end function create_gaussian_coord
  !-----------------------------------------------------------------------------

 !------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
      subroutine legendre_roots(l,root,w)
  !-----------------------------------------------------------------------------
  ! Subroutine finds the l roots (in theta) and gaussian weights associated
  ! with the legendre polynomial of degree l > 1. (Source from SCRIP create 
  ! gaussian grid routine
  !-----------------------------------------------------------------------------
  implicit none

  !-----------------------------------------------------------------------------
  !   arguments
  !-----------------------------------------------------------------------------
  integer, intent(in) :: l

  real(ESMF_KIND_R8), dimension(l), intent(out) :: root, w

  !-----------------------------------------------------------------------------
  !   local variables
  !-----------------------------------------------------------------------------
  integer :: l1, l2, l22, l3, k, i, j
  real(ESMF_KIND_R8) :: del,co,p1,p2,p3,t1,t2,slope,s,c,pp1,pp2,p00

  !-----------------------------------------------------------------------------
  !   Define useful constants.
  !-----------------------------------------------------------------------------
  del= pi/float(4*l)
  l1 = l+1
  co = float(2*l+3)/float(l1**2)
  p2 = 1.0
  t2 = -del
  l2 = l/2
  k = 1
  p00 = one/sqrt(two)

  !-----------------------------------------------------------------------------
  !   Start search for each root by looking for crossing point.
  !-----------------------------------------------------------------------------
  do i=1,l2
 10  t1 = t2
     t2 = t1+del
     p1 = p2
     s = sin(t2)
     c = cos(t2)
     pp1 = 1.0
     p3 = p00
     do j=1,l1
        pp2 = pp1
        pp1 = p3
        p3 = 2.0*sqrt((float(j**2)-0.250)/float(j**2))*c*pp1-                  &
             sqrt(float((2*j+1)*(j-1)*(j-1))/float((2*j-3)*j*j))*pp2
     enddo
     p2 = pp1
     if ((k*p2).gt.0) goto 10

  !-----------------------------------------------------------------------------
  !      Now converge using Newton-Raphson.
  !-----------------------------------------------------------------------------
     k = -k
 20  continue
     slope = (t2-t1)/(p2-p1)
     t1 = t2
     t2 = t2-slope*p2
     p1 = p2
     s = sin(t2)
     c = cos(t2)
     pp1 = 1.0
     p3 = p00
     do j=1,l1
        pp2 = pp1
        pp1 = p3
        p3 = 2.0*sqrt((float(j**2)-0.250)/float(j**2))*c*pp1-                  &
             sqrt(float((2*j+1)*(j-1)*(j-1))/float((2*j-3)*j*j))*pp2
     enddo
     p2 = pp1
     if (abs(p2).gt.1.e-10) goto 20
     root(i) = t2
     w(i) = co*(sin(t2)/p3)**2
  enddo

  !-----------------------------------------------------------------------------
  !   If l is odd, take care of odd point.
  !-----------------------------------------------------------------------------
  l22 = 2*l2
  if (l22 .ne. l) then
     l2 = l2+1
     t2 = pi/2.0
     root(l2) = t2
     s = sin(t2)
     c = cos(t2)
     pp1 = 1.0
     p3 = p00
     do j=1,l1
        pp2 = pp1
        pp1 = p3
        p3 = 2.0*sqrt((float(j**2)-0.250)/float(j**2))*c*pp1-                  &
             sqrt(float((2*j+1)*(j-1)*(j-1))/float((2*j-3)*j*j))*pp2
     enddo
     p2 = pp1
     w(l2) = co/p3**2
  endif

  !-----------------------------------------------------------------------------
  !   Use symmetry to compute remaining roots and weights.
  !-----------------------------------------------------------------------------
  l3 = l2+1
  do i=l3,l
     root(i) = pi-root(l-i+1)
     w(i) = w(l-i+1)
  enddo

  return

  !-----------------------------------------------------------------------------
      end subroutine legendre_roots
  !-----------------------------------------------------------------------------

!===============================================================================
  end module ESMF_TestHarnessMod
!===============================================================================

