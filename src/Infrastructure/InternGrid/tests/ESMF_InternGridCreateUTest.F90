! $Id: ESMF_InternGridCreateUTest.F90,v 1.1 2007/06/22 23:21:38 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_InternGridCreateUTest

!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
#include <ESMF_Macros.inc>
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_InternGridUTest - These tests verifies InternGrid functionalities.
!
! !DESCRIPTION:
!
! The code in this file specializes on the various ways of creating 
! {\tt InternGrid}s, including 3D and others.
! The companion test file {\tt ESMF\_InternGridUTest.F90} contains general
!  interngrid tests.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod
    
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_InternGridCreateUTest.F90,v 1.1 2007/06/22 23:21:38 cdeluca Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc

      ! individual test name
      character(ESMF_MAXSTR) :: name, gName, Rgname

      ! individual test failure messages
      character(ESMF_MAXSTR*2) :: failMsg

      ! local variables needed to pass into function/subroutine calls
      !character(ESMF_MAXSTR) :: validate_options
      !character(ESMF_MAXSTR) :: print_options
      !type(ESMF_InternGridConfig) :: config_set
      !type(ESMF_InternGridConfig) :: config_get

      integer :: i, counts(2)
      integer :: nDE_i, nDE_j
      type(ESMF_InternGridHorzStagger) :: horz_stagger, Rhorz_stagger
      type(ESMF_InternGridVertStagger) :: vert_stagger
      integer :: status
      integer, dimension (4) :: DEDim1
      integer, dimension (1) :: DEDimX
      integer, dimension (10000) :: DEDim2
      real(ESMF_KIND_R8) :: delta(15), interngrid_min(3), interngrid_max(3)
      real(ESMF_KIND_R8) :: coord1(21), coord2(16)
      real(ESMF_KIND_R8) :: Rinterngrid_min(3), Rinterngrid_max(3)
      type(ESMF_InternGrid) :: interngrid, interngrid1, interngrid2, interngrid3
      type(ESMF_DELayout) :: layout
      type(ESMF_VM) :: vm


!------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------


      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)


      !------------------------------------------------------------------------
      !NEX_UTest
      call ESMF_VMGetGlobal(vm, status)
      write(name, *) "ESMF_VMGetGlobal Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! set up values to be used below
      counts(1) = 10
      counts(2) = 12
      DEDim1(1) = 1
      DEDim1(2) = 5
      DEDim1(3) = 2
      DEDim1(4) = 2
      DEDim2(1) = 12
      nDE_i = 2
      nDE_j = 2
      horz_stagger      = ESMF_IGRID_HORZ_STAGGER_A
      vert_stagger      = ESMF_IGRID_VERT_STAGGER_CENTER
      interngrid_min(1) = -90.0
      interngrid_max(1) =  90.0
      interngrid_min(2) =   0.0
      interngrid_max(2) = 180.0
      interngrid_min(3) =  90.0
      interngrid_max(3) = 100.0
      delta(1:15) = 6.6667
      gName = "test interngrid 1"


      !------------------------------------------------------------------------
      !NEX_UTest
      layout = ESMF_DELayoutCreate(vm, rc=rc)
      write(name, *) "Creating a DELayout Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Create a HorzXYUni InternGrid Test.
      !NEX_UTest
      interngrid = ESMF_InternGridCreateHorzXYUni(counts=counts, &
                              minGlobalCoordPerDim=interngrid_min, &
                              maxGlobalCoordPerDim=interngrid_max, &
                              horzstagger=horz_stagger, &
                              name=gName, rc=status)
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Creating a LogRectUniform InternGrid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Add Vert Height Test
      !NEX_UTest
      call ESMF_InternGridAddVertHeight(interngrid, delta, vertstagger=vert_stagger, &
                                  rc=status)
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Add Vert Height InternGrid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! InternGrid Distribute Test
      !NEX_UTest
      call ESMF_InternGridDistribute(interngrid, delayout=layout, rc=status)
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "InternGrid Distribute Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest 
      ! Destroy the InternGrid test
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Destroy the InternGrid Test"
      call ESMF_InternGridDestroy(interngrid, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
                              
      !------------------------------------------------------------------------
      ! Create a HorzLatLon InternGrid Test.
      !NEX_UTest
      coord1(1) = interngrid_min(1)
      do i = 2, size(coord1)
        coord1(i) = coord1(i-1) + 0.50d0
      enddo
      coord2(1) = interngrid_min(2)
      do i = 2, size(coord2)
        coord2(i) = coord2(i-1) + 0.40d0
      enddo
      interngrid1 = ESMF_InternGridCreateHorzLatLon(coord1=coord1, coord2=coord2, &
                                        horzstagger=horz_stagger, &
                                        name=gName, rc=status)

      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Creating a HorzLatLon InternGrid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! InternGrid Distribute Test
      !NEX_UTest
      call ESMF_InternGridDistribute(interngrid1, delayout=layout, rc=status)
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "InternGrid Distribute Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest 
      ! Destroy the InternGrid test
      call ESMF_InternGridDestroy(interngrid1, rc=rc)
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Destroy the InternGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
                              
      !------------------------------------------------------------------------
      ! Create a HorzLatLon InternGrid Uni Test.
      !NEX_UTest
      interngrid2 = ESMF_InternGridCreateHorzLatLonUni(counts=counts, &
			      minGlobalCoordPerDim=interngrid_min, &
                              deltaPerDim=(/0.60d0, 0.55d0/), &
                              horzstagger=horz_stagger, &
                              name=gName, rc=status)

      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Creating a HorzLatLon Uni InternGrid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! InternGrid Distribute Test
      !NEX_UTest
      call ESMF_InternGridDistribute(interngrid2, delayout=layout, rc=status)
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "InternGrid Distribute Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest 
      ! Destroy the InternGrid test
      call ESMF_InternGridDestroy(interngrid2, rc=rc)
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Destroy the InternGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
                              
#ifdef ESMF_EXHAUSTIVE


      !------------------------------------------------------------------------
      !EX_UTest
      ! Destroy a destroyed InternGrid
      call ESMF_InternGridDestroy(interngrid2, rc=rc)
      write(failMsg, *) "Did not returned ESMF_RC_OBJ_DELETED"
      write(name, *) "Destroy a destroyed InternGrid Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)
                              
      !------------------------------------------------------------------------
      !EX_UTest
      ! Destroy a Non-created InternGrid
      call ESMF_InternGridDestroy(interngrid3, rc=rc)
      write(failMsg, *) "Did not returned ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Destroy a non-created InternGrid Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get the InternGrid horzStagger from a non-created InternGrid
      call ESMF_InternGridGet(interngrid3, horzstagger=Rhorz_stagger, rc=rc)
      write(name, *) "Get the InternGrid horzStagger from non-created InternGrid Test"
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED "
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      ! InternGrid Add Vert Height to a deleted InternGrid Test.
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Add InternGrid Vert Height to a destroyed InternGrid Test"
      call ESMF_InternGridAddVertHeight(interngrid, delta, vertstagger=vert_stagger, &
                                  rc=status)
      call ESMF_Test((status.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! InternGrid Add Vert Height to a non-created InternGrid Test.
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Add InternGrid Vert Height to a non-created InternGrid Test"
      call ESMF_InternGridAddVertHeight(interngrid3, delta, vertstagger=vert_stagger, &
                                  rc=status)
      call ESMF_Test((status.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! InternGrid Distribute of destroyed InternGrid Test.
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "InternGrid Distribute of destroyed InternGrid Test"
      call ESMF_InternGridDistribute(interngrid, delayout=layout, countsPerDEDim1=DEDim1, &
                               countsPerDEDim2=DEDim2, rc=status)
      call ESMF_Test((status.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! InternGrid Distribute of non-created InternGrid Test.
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "InternGrid Distribute of non-created InternGrid Test"
      call ESMF_InternGridDistribute(interngrid3, delayout=layout, countsPerDEDim1=DEDim1, &
                               countsPerDEDim2=DEDim2, rc=status)
      call ESMF_Test((status.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      ! Printing a destroyed InternGrid
      !EX_UTest
      call ESMF_InternGridPrint(interngrid, "", rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Printing a destroyed InternGrid Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Printing a non-created InternGrid
      !EX_UTest
      call ESMF_InternGridPrint(interngrid3, "", rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Printing a non-created InternGrid Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)



      !------------------------------------------------------------------------
      ! Create a InternGrid Test.
      !EX_UTest
      interngrid = ESMF_InternGridCreateHorzXYUni(counts=counts, &
                              minGlobalCoordPerDim=interngrid_min, &
                              maxGlobalCoordPerDim=interngrid_max, &
                              horzstagger=horz_stagger, &
                              name=gName, rc=status)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a LogRectUniform InternGrid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! InternGrid Add Vert Height Test.
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Add InternGrid Vert Height Test"
      call ESMF_InternGridAddVertHeight(interngrid, delta, vertstagger=vert_stagger, &
                                  rc=status)
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! InternGrid Distribute Test.
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "InternGrid Distribute Test"
      call ESMF_InternGridDistribute(interngrid, delayout=layout, countsPerDEDim1=DEDim1, &
                               countsPerDEDim2=DEDim2, rc=status)
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Printing a InternGrid
      !EX_UTest
      call ESMF_InternGridPrint(interngrid, "", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Printing a InternGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get the InternGrid horzStagger
      call ESMF_InternGridGet(interngrid, horzstagger=Rhorz_stagger, rc=rc)
      write(name, *) "Get the InternGrid horzStagger Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS or the horzStagger is not correct"
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. Rhorz_Stagger.eq.horz_stagger), &
                      name, failMsg, result, ESMF_SRCLINE)
  
  
      !------------------------------------------------------------------------
      !EX_UTest
      ! Get the InternGrid minGlobalCoordPerDim
      call ESMF_InternGridGet(interngrid, minGlobalCoordPerDim=Rinterngrid_min, rc=rc)
      write(name, *) "Get the InternGrid minGlobalCoordPerDim Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS or the minGlobalCoordPerDim is not correct"
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. Rinterngrid_min(1).eq.-90.0 .and. Rinterngrid_min(2).eq.0.0), &
                        name, failMsg, result, ESMF_SRCLINE)
  
      !------------------------------------------------------------------------
      !EX_UTest
      ! Get the InternGrid maxGlobalCoordPerDim
      call ESMF_InternGridGet(interngrid, maxGlobalCoordPerDim=Rinterngrid_max, rc=rc)
      write(name, *) "Get the InternGrid maxGlobalCoordPerDim Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS or the maxGlobalCoordPerDim is not correct"
      write(name, *) "Verify the InternGrid maxGlobalCoordPerDim Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. Rinterngrid_max(1).eq.90.0 .and. Rinterngrid_max(2).eq.180.0), &
                        name, failMsg, result, ESMF_SRCLINE)
  
      !------------------------------------------------------------------------
      !EX_UTest
      ! Get the InternGrid name
      call ESMF_InternGridGet(interngrid, name=RgName, rc=rc)
      write(name, *) "Get the InternGrid name Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS or the name is not correct"
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. trim(RgName).eq."test interngrid 1"), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Validate the InternGrid test
      call ESMF_InternGridValidate(interngrid, rc=rc)
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Validate the InternGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Distribute a InternGrid test
      call ESMF_InternGridDistribute(interngrid, layout, rc=rc)
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Distribute the InternGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Destroy the InternGrid test
      call ESMF_InternGridDestroy(interngrid, rc=rc)
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Destroy the InternGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Create an Empty InternGrid Test.
      gName = "test interngrid 2"
      interngrid1 = ESMF_InternGridCreate(name=gName, rc=status)
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Creating an empty InternGrid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get the InternGrid name
      call ESMF_InternGridGet(interngrid1, name=RgName, rc=rc)
      write(name, *) "Get the InternGrid name Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS or the name is not correct"
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. trim(RgName).eq."test interngrid 2"), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "Rgname = ", trim(RgName)
      print *, " rc = ", rc


      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      ! Create a InternGrid Test.
      !EX_UTest
      interngrid = ESMF_InternGridCreateHorzXY(coord1=(/ 1d0,2d0,3d0,4d0,5d0,6d0,7d0,8d0,9d0,10d0,11d0 /), &
                                   coord2=(/ 1d0,2d0,3d0,4d0,5d0,6d0,7d0,8d0,9d0,10d0,11d0 /), &
                                   horzstagger=ESMF_IGRID_HORZ_STAGGER_C_SW, &
                                   name="coordinate interngrid", rc=status)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a interngrid with explicit coordinates Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! InternGrid Distribute Test - should fail.
      !EX_UTest
      DEDim1(:) = (/ 2, 3, 3, 3 /)
      DEDimX(1) = 11
      write(failMsg, *) "Returned ESMF_SUCCESS when expecting failure"
      write(name, *) "InternGrid Distribute Test"
      call ESMF_InternGridDistribute(interngrid, delayout=layout, &
                               countsPerDEDim1=DEDim1, &
                               countsPerDEDim2=DEDimX, rc=status)
      call ESMF_Test((status.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Create a InternGrid Test.
      !EX_UTest
      interngrid = ESMF_InternGridCreateHorzXY(coord1=(/ 1d0,2d0,3d0,4d0,5d0,6d0,7d0,8d0,9d0,10d0,11d0 /), &
                                   coord2=(/ 1d0,2d0,3d0,4d0,5d0,6d0,7d0,8d0,9d0,10d0,11d0 /), &
                                   horzstagger=ESMF_IGRID_HORZ_STAGGER_C_SW, &
                                   name="coordinate interngrid", rc=status)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a interngrid with explicit coordinates Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! InternGrid Distribute Test - should succeed.
      !EX_UTest
      DEDim1(:) = (/ 2, 3, 2, 3 /)
      DEDimX(1) = 10
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "InternGrid Distribute Test"
      call ESMF_InternGridDistribute(interngrid, delayout=layout, &
                               countsPerDEDim1=DEDim1, &
                               countsPerDEDim2=DEDimX, rc=status)
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Printing a InternGrid
      !EX_UTest
      call ESMF_InternGridPrint(interngrid, "", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Printing a InternGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
#endif

      call ESMF_TestEnd(result, ESMF_SRCLINE)

      end program ESMF_InternGridCreateUTest
