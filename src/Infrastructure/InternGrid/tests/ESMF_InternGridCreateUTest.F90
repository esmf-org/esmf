! $Id: ESMF_InternGridCreateUTest.F90,v 1.2 2007/06/23 04:37:05 cdeluca Exp $
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
      program ESMF_IGridCreateUTest

!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
#include <ESMF_Macros.inc>
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_IGridUTest - These tests verifies IGrid functionalities.
!
! !DESCRIPTION:
!
! The code in this file specializes on the various ways of creating 
! {\tt IGrid}s, including 3D and others.
! The companion test file {\tt ESMF\_IGridUTest.F90} contains general
!  igrid tests.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod
    
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_InternGridCreateUTest.F90,v 1.2 2007/06/23 04:37:05 cdeluca Exp $'
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
      !type(ESMF_IGridConfig) :: config_set
      !type(ESMF_IGridConfig) :: config_get

      integer :: i, counts(2)
      integer :: nDE_i, nDE_j
      type(ESMF_IGridHorzStagger) :: horz_stagger, Rhorz_stagger
      type(ESMF_IGridVertStagger) :: vert_stagger
      integer :: status
      integer, dimension (4) :: DEDim1
      integer, dimension (1) :: DEDimX
      integer, dimension (10000) :: DEDim2
      real(ESMF_KIND_R8) :: delta(15), igrid_min(3), igrid_max(3)
      real(ESMF_KIND_R8) :: coord1(21), coord2(16)
      real(ESMF_KIND_R8) :: Rigrid_min(3), Rigrid_max(3)
      type(ESMF_IGrid) :: igrid, igrid1, igrid2, igrid3
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
      igrid_min(1) = -90.0
      igrid_max(1) =  90.0
      igrid_min(2) =   0.0
      igrid_max(2) = 180.0
      igrid_min(3) =  90.0
      igrid_max(3) = 100.0
      delta(1:15) = 6.6667
      gName = "test igrid 1"


      !------------------------------------------------------------------------
      !NEX_UTest
      layout = ESMF_DELayoutCreate(vm, rc=rc)
      write(name, *) "Creating a DELayout Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Create a HorzXYUni IGrid Test.
      !NEX_UTest
      igrid = ESMF_IGridCreateHorzXYUni(counts=counts, &
                              minGlobalCoordPerDim=igrid_min, &
                              maxGlobalCoordPerDim=igrid_max, &
                              horzstagger=horz_stagger, &
                              name=gName, rc=status)
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Creating a LogRectUniform IGrid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Add Vert Height Test
      !NEX_UTest
      call ESMF_IGridAddVertHeight(igrid, delta, vertstagger=vert_stagger, &
                                  rc=status)
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Add Vert Height IGrid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! IGrid Distribute Test
      !NEX_UTest
      call ESMF_IGridDistribute(igrid, delayout=layout, rc=status)
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "IGrid Distribute Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest 
      ! Destroy the IGrid test
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Destroy the IGrid Test"
      call ESMF_IGridDestroy(igrid, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
                              
      !------------------------------------------------------------------------
      ! Create a HorzLatLon IGrid Test.
      !NEX_UTest
      coord1(1) = igrid_min(1)
      do i = 2, size(coord1)
        coord1(i) = coord1(i-1) + 0.50d0
      enddo
      coord2(1) = igrid_min(2)
      do i = 2, size(coord2)
        coord2(i) = coord2(i-1) + 0.40d0
      enddo
      igrid1 = ESMF_IGridCreateHorzLatLon(coord1=coord1, coord2=coord2, &
                                        horzstagger=horz_stagger, &
                                        name=gName, rc=status)

      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Creating a HorzLatLon IGrid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! IGrid Distribute Test
      !NEX_UTest
      call ESMF_IGridDistribute(igrid1, delayout=layout, rc=status)
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "IGrid Distribute Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest 
      ! Destroy the IGrid test
      call ESMF_IGridDestroy(igrid1, rc=rc)
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Destroy the IGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
                              
      !------------------------------------------------------------------------
      ! Create a HorzLatLon IGrid Uni Test.
      !NEX_UTest
      igrid2 = ESMF_IGridCreateHorzLatLonUni(counts=counts, &
			      minGlobalCoordPerDim=igrid_min, &
                              deltaPerDim=(/0.60d0, 0.55d0/), &
                              horzstagger=horz_stagger, &
                              name=gName, rc=status)

      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Creating a HorzLatLon Uni IGrid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! IGrid Distribute Test
      !NEX_UTest
      call ESMF_IGridDistribute(igrid2, delayout=layout, rc=status)
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "IGrid Distribute Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest 
      ! Destroy the IGrid test
      call ESMF_IGridDestroy(igrid2, rc=rc)
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Destroy the IGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
                              
#ifdef ESMF_EXHAUSTIVE


      !------------------------------------------------------------------------
      !EX_UTest
      ! Destroy a destroyed IGrid
      call ESMF_IGridDestroy(igrid2, rc=rc)
      write(failMsg, *) "Did not returned ESMF_RC_OBJ_DELETED"
      write(name, *) "Destroy a destroyed IGrid Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)
                              
      !------------------------------------------------------------------------
      !EX_UTest
      ! Destroy a Non-created IGrid
      call ESMF_IGridDestroy(igrid3, rc=rc)
      write(failMsg, *) "Did not returned ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Destroy a non-created IGrid Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get the IGrid horzStagger from a non-created IGrid
      call ESMF_IGridGet(igrid3, horzstagger=Rhorz_stagger, rc=rc)
      write(name, *) "Get the IGrid horzStagger from non-created IGrid Test"
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED "
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      ! IGrid Add Vert Height to a deleted IGrid Test.
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Add IGrid Vert Height to a destroyed IGrid Test"
      call ESMF_IGridAddVertHeight(igrid, delta, vertstagger=vert_stagger, &
                                  rc=status)
      call ESMF_Test((status.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! IGrid Add Vert Height to a non-created IGrid Test.
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Add IGrid Vert Height to a non-created IGrid Test"
      call ESMF_IGridAddVertHeight(igrid3, delta, vertstagger=vert_stagger, &
                                  rc=status)
      call ESMF_Test((status.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! IGrid Distribute of destroyed IGrid Test.
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "IGrid Distribute of destroyed IGrid Test"
      call ESMF_IGridDistribute(igrid, delayout=layout, countsPerDEDim1=DEDim1, &
                               countsPerDEDim2=DEDim2, rc=status)
      call ESMF_Test((status.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! IGrid Distribute of non-created IGrid Test.
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "IGrid Distribute of non-created IGrid Test"
      call ESMF_IGridDistribute(igrid3, delayout=layout, countsPerDEDim1=DEDim1, &
                               countsPerDEDim2=DEDim2, rc=status)
      call ESMF_Test((status.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      ! Printing a destroyed IGrid
      !EX_UTest
      call ESMF_IGridPrint(igrid, "", rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Printing a destroyed IGrid Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Printing a non-created IGrid
      !EX_UTest
      call ESMF_IGridPrint(igrid3, "", rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Printing a non-created IGrid Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)



      !------------------------------------------------------------------------
      ! Create a IGrid Test.
      !EX_UTest
      igrid = ESMF_IGridCreateHorzXYUni(counts=counts, &
                              minGlobalCoordPerDim=igrid_min, &
                              maxGlobalCoordPerDim=igrid_max, &
                              horzstagger=horz_stagger, &
                              name=gName, rc=status)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a LogRectUniform IGrid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! IGrid Add Vert Height Test.
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Add IGrid Vert Height Test"
      call ESMF_IGridAddVertHeight(igrid, delta, vertstagger=vert_stagger, &
                                  rc=status)
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! IGrid Distribute Test.
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "IGrid Distribute Test"
      call ESMF_IGridDistribute(igrid, delayout=layout, countsPerDEDim1=DEDim1, &
                               countsPerDEDim2=DEDim2, rc=status)
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Printing a IGrid
      !EX_UTest
      call ESMF_IGridPrint(igrid, "", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Printing a IGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get the IGrid horzStagger
      call ESMF_IGridGet(igrid, horzstagger=Rhorz_stagger, rc=rc)
      write(name, *) "Get the IGrid horzStagger Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS or the horzStagger is not correct"
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. Rhorz_Stagger.eq.horz_stagger), &
                      name, failMsg, result, ESMF_SRCLINE)
  
  
      !------------------------------------------------------------------------
      !EX_UTest
      ! Get the IGrid minGlobalCoordPerDim
      call ESMF_IGridGet(igrid, minGlobalCoordPerDim=Rigrid_min, rc=rc)
      write(name, *) "Get the IGrid minGlobalCoordPerDim Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS or the minGlobalCoordPerDim is not correct"
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. Rigrid_min(1).eq.-90.0 .and. Rigrid_min(2).eq.0.0), &
                        name, failMsg, result, ESMF_SRCLINE)
  
      !------------------------------------------------------------------------
      !EX_UTest
      ! Get the IGrid maxGlobalCoordPerDim
      call ESMF_IGridGet(igrid, maxGlobalCoordPerDim=Rigrid_max, rc=rc)
      write(name, *) "Get the IGrid maxGlobalCoordPerDim Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS or the maxGlobalCoordPerDim is not correct"
      write(name, *) "Verify the IGrid maxGlobalCoordPerDim Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. Rigrid_max(1).eq.90.0 .and. Rigrid_max(2).eq.180.0), &
                        name, failMsg, result, ESMF_SRCLINE)
  
      !------------------------------------------------------------------------
      !EX_UTest
      ! Get the IGrid name
      call ESMF_IGridGet(igrid, name=RgName, rc=rc)
      write(name, *) "Get the IGrid name Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS or the name is not correct"
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. trim(RgName).eq."test igrid 1"), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Validate the IGrid test
      call ESMF_IGridValidate(igrid, rc=rc)
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Validate the IGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Distribute a IGrid test
      call ESMF_IGridDistribute(igrid, layout, rc=rc)
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Distribute the IGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Destroy the IGrid test
      call ESMF_IGridDestroy(igrid, rc=rc)
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Destroy the IGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Create an Empty IGrid Test.
      gName = "test igrid 2"
      igrid1 = ESMF_IGridCreate(name=gName, rc=status)
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Creating an empty IGrid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get the IGrid name
      call ESMF_IGridGet(igrid1, name=RgName, rc=rc)
      write(name, *) "Get the IGrid name Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS or the name is not correct"
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. trim(RgName).eq."test igrid 2"), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "Rgname = ", trim(RgName)
      print *, " rc = ", rc


      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      ! Create a IGrid Test.
      !EX_UTest
      igrid = ESMF_IGridCreateHorzXY(coord1=(/ 1d0,2d0,3d0,4d0,5d0,6d0,7d0,8d0,9d0,10d0,11d0 /), &
                                   coord2=(/ 1d0,2d0,3d0,4d0,5d0,6d0,7d0,8d0,9d0,10d0,11d0 /), &
                                   horzstagger=ESMF_IGRID_HORZ_STAGGER_C_SW, &
                                   name="coordinate igrid", rc=status)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a igrid with explicit coordinates Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! IGrid Distribute Test - should fail.
      !EX_UTest
      DEDim1(:) = (/ 2, 3, 3, 3 /)
      DEDimX(1) = 11
      write(failMsg, *) "Returned ESMF_SUCCESS when expecting failure"
      write(name, *) "IGrid Distribute Test"
      call ESMF_IGridDistribute(igrid, delayout=layout, &
                               countsPerDEDim1=DEDim1, &
                               countsPerDEDim2=DEDimX, rc=status)
      call ESMF_Test((status.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Create a IGrid Test.
      !EX_UTest
      igrid = ESMF_IGridCreateHorzXY(coord1=(/ 1d0,2d0,3d0,4d0,5d0,6d0,7d0,8d0,9d0,10d0,11d0 /), &
                                   coord2=(/ 1d0,2d0,3d0,4d0,5d0,6d0,7d0,8d0,9d0,10d0,11d0 /), &
                                   horzstagger=ESMF_IGRID_HORZ_STAGGER_C_SW, &
                                   name="coordinate igrid", rc=status)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a igrid with explicit coordinates Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! IGrid Distribute Test - should succeed.
      !EX_UTest
      DEDim1(:) = (/ 2, 3, 2, 3 /)
      DEDimX(1) = 10
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "IGrid Distribute Test"
      call ESMF_IGridDistribute(igrid, delayout=layout, &
                               countsPerDEDim1=DEDim1, &
                               countsPerDEDim2=DEDimX, rc=status)
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Printing a IGrid
      !EX_UTest
      call ESMF_IGridPrint(igrid, "", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Printing a IGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
#endif

      call ESMF_TestEnd(result, ESMF_SRCLINE)

      end program ESMF_IGridCreateUTest
