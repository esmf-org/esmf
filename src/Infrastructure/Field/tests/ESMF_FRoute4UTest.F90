! $Id: ESMF_FRoute4UTest.F90,v 1.6 2004/10/05 15:39:43 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
      program ESMF_FRouteUTest

!------------------------------------------------------------------------------
 
#include <ESMF_Macros.inc>

!==============================================================================
!BOP
! !PROGRAM: ESMF_FRouteUTest - Unit test for Field Route function.
!
! !DESCRIPTION:
!
! The code in this file drives the F90 Field Route tests.  The Field
!   Route function is complex enough to require a separate test file.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod

      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_FRoute4UTest.F90,v 1.6 2004/10/05 15:39:43 svasquez Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc = 1

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

!     !LOCAL VARIABLES:
      integer :: i, x, y
      type(ESMF_Grid) :: grid1, grid2, grid3, grid4
      type(ESMF_Array) :: arr1, arr2
      type(ESMF_AxisIndex), dimension(ESMF_MAXDIM) :: g1_ai, g2_ai
      integer, dimension(ESMF_MAXDIM) :: g1_cells, g2_cells
      integer, dimension(:,:), pointer :: f90ptr1, f90ptr2
      type(ESMF_FieldDataMap) :: dm
      type(ESMF_RelLoc) :: rl
      type(ESMF_DELayout) :: layout0, layout1, layout2
      type(ESMF_VM) :: vm
      type(ESMF_RouteHandle) :: rh
      integer :: delist(64)
      character (len = 20) :: fname, fname1, fname2, gname
      type(ESMF_IOSpec) :: ios
      type(ESMF_Field) :: f1, f2, f3, f4, f5
      integer :: nDEs, nDE_i, nDE_j
      integer :: half, quart
      real (ESMF_KIND_R8):: min(2), max(2)
      integer :: counts(ESMF_MAXGRIDDIM)
      type(ESMF_GridHorzStagger) :: horz_stagger
      integer :: status, myde, npets

!------------------------------------------------------------------------------
!   The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!   always run. When the environment variable, EXHAUSTIVE, is set to ON then
!   the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!   Special strings (Non-exhaustive and exhaustive) have been
!   added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------


      print *, "*************FIELD ROUTE UNIT TESTS***************************"
      print *

      call ESMF_Initialize(vm=vm, rc=rc)
      call ESMF_VMGet(vm, petCount=npets, rc=rc)
      print '(/, a, i3)' , "NUMBER_OF_PROCESSORS", npets

      ! Make a default 1xN layout
      layout0 = ESMF_DELayoutCreate(vm, rc=rc)
      call ESMF_DELayoutGet(layout0, deCount=nDEs, rc=rc)

      ! exit early if we have less than 4 procs
      if (nDEs .lt. 4) then
        print *, "This test cannot run with less than 4 processors"
        goto 10
      endif

      half = nDEs / 2
      quart = nDEs / 4

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Make a Nx4 and Nx2 layout
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write(name, *) "Creating a DELayout Test"
      layout1 = ESMF_DELayoutCreate(vm, (/ quart, 4 /), rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      if (rc .eq. ESMF_FAILURE) then
        print *, "cannot create 1x4 layout"
        goto 10
      endif

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      print *, "Layout 1:"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write(name, *) "Printing a DELayout Test"
      call ESMF_DELayoutPrint(layout1, "", rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write(name, *) "Creating a DELayout Test"
      layout2 = ESMF_DELayoutCreate(vm, (/ half, 2 /), rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      if (rc .eq. ESMF_FAILURE) then
        print *, "cannot create 2x2 layout"
        goto 10
      endif

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      print *, "Layout 1:"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write(name, *) "Printing a DELayout Test"
      print *, "Layout 2:"
      call ESMF_DELayoutPrint(layout2, "", rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write(name, *) "DELayout Get Test"
      call ESMF_DELayoutGet(layout1, localDE=myde, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      counts(1) = 48
      counts(2) = 24
      min(1) = 0.0
      max(1) = 20.0
      min(2) = 0.0
      max(2) = 5.0
      horz_stagger = ESMF_GRID_HORZ_STAGGER_A
      gname = "test grid 1"

      grid1 = ESMF_GridCreateHorzXYUni(counts=counts, &
                              minGlobalCoordPerDim=min, &
                              maxGlobalCoordPerDim=max, &
                              horzStagger=horz_stagger, &
                              name=gname, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCESS"
      write(name, *) "Creating a source Test Grid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCESS"
      write(name, *) "Grid distribute Test "
      call ESMF_GridDistribute(grid1, delayout=layout1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Destroy Grid Test
      write(failMsg, *) ""
      write(name, *) "Destroy Grid Test"
      call ESMF_GridDestroy(grid1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

#ifdef ESMF_EXHAUSTIVE
     
      
      !EX_UTest_Multi_Proc_Only
      grid1 = ESMF_GridCreateHorzXYUni(counts=counts, &
                              minGlobalCoordPerDim=min, &
                              maxGlobalCoordPerDim=max, &
                              horzStagger=horz_stagger, &
                              name=gname, rc=status)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a source Test Grid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCESS"
      write(name, *) "Grid distribute Test "
      call ESMF_GridDistribute(grid1, delayout=layout1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verifing that an uninitialized Grid can be printed
      call ESMF_GridPrint(grid3, "", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing an uninitialized Grid Test"
!     call 
      ! Second grid
      gname = "test grid 2"
      horz_stagger = ESMF_GRID_HORZ_STAGGER_D_NE
      grid2 = ESMF_GridCreateHorzXYUni(counts=counts, &
                              minGlobalCoordPerDim=min, &
                              maxGlobalCoordPerDim=max, &
                              horzStagger=horz_stagger, &
                              name=gname, rc=status)
      call ESMF_GridDistribute(grid2, delayout=layout1, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a destination Test Grid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verifing that an Array can be created
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Get DE Local Info Test"
      call ESMF_GridGetDELocalInfo(grid1, localCellCountPerDim=g1_cells, &
                          horzRelloc=ESMF_CELL_CENTER, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      allocate(f90ptr1(g1_cells(1), g1_cells(2)))
      f90ptr1 = 10+myde
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a src Test Array"
      arr1 = ESMF_ArrayCreate(f90ptr1, ESMF_DATA_REF, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Print Array Test"
      call ESMF_ArrayPrint(arr1)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! second array
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Get DE Local Info Test"
      call ESMF_GridGetDELocalInfo(grid2, localCellCountPerDim=g2_cells, &
                          horzRelloc=ESMF_CELL_CENTER)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      allocate(f90ptr2(g2_cells(1), g2_cells(2)))
      f90ptr2 = -1
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a dst Test Array"
      arr2 = ESMF_ArrayCreate(f90ptr2, ESMF_DATA_REF, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Print Array Test"
      call ESMF_ArrayPrint(arr2)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verifing that a Field can be created with a Grid and Array
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Field DataMap Set Test"
      call ESMF_FieldDataMapSetDefault(dm, ESMF_INDEX_IJ, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a Field with a Grid and Array Test"
      f1 = ESMF_FieldCreate(grid1, arr1, ESMF_DATA_REF, ESMF_CELL_CENTER, &
                            ESMF_CELL_CELL, 1, dm, "Field 0", ios, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! second field
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a Field with a Grid and Array Test"
      f2 = ESMF_FieldCreate(grid2, arr2, ESMF_DATA_REF, ESMF_CELL_CENTER, &
                            ESMF_CELL_CELL, 1, dm, "Field 1", ios, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! route test
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Field Redist Store Test"
      call ESMF_FieldRedistStore(f1, f2, layout1, rh, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Field Redist Test"
      call ESMF_FieldRedist(f1, f2, rh, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Field Redist Release Test"
      call ESMF_FieldRedistRelease(rh, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! results
      !EX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Field Print Test"
      call ESMF_FieldPrint(f2, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Array Print Test"
      call ESMF_ArrayPrint(arr2, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Field Destroy Test"
      call ESMF_FieldDestroy(f1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Field Destroy Test"
      call ESMF_FieldDestroy(f2, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Grid Destroy Test"
      call ESMF_GridDestroy(grid1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Grid Destroy Test"
      call ESMF_GridDestroy(grid2, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Array Destroy Test"
      call ESMF_ArrayDestroy(arr1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Array Destroy Test"
      call ESMF_ArrayDestroy(arr2, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#endif

10    print *, "end of Field Route test"

      call ESMF_Finalize(rc)

      end program ESMF_FRouteUTest
