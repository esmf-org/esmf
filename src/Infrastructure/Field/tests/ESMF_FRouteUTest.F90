! $Id: ESMF_FRouteUTest.F90,v 1.31 2004/03/22 23:28:35 nscollins Exp $
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
      '$Id: ESMF_FRouteUTest.F90,v 1.31 2004/03/22 23:28:35 nscollins Exp $'
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
      type(ESMF_DataMap) :: dm
      type(ESMF_RelLoc) :: rl
      type(ESMF_DELayout) :: layout0, layout1, layout2
      type(ESMF_RouteHandle) :: rh
      integer :: delist(64)
      character (len = 20) :: fname, fname1, fname2, gname
      type(ESMF_IOSpec) :: ios
      type(ESMF_Field) :: f1, f2, f3, f4, f5
      integer :: nDEs, nDE_i, nDE_j
      integer :: half, quart
      real (ESMF_KIND_R8):: min(2), max(2)
      integer :: counts(ESMF_MAXGRIDDIM)
      type(ESMF_GridType) :: horz_gridtype, vert_gridtype
      type(ESMF_GridStagger) :: horz_stagger, vert_stagger
      type(ESMF_CoordSystem) :: horz_coord_system, vert_coord_system
      integer :: status, myde

!------------------------------------------------------------------------------
!   The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!   always run. When the environment variable, EXHAUSTIVE, is set to ON then
!   the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!   Special strings (Non-exhaustive and exhaustive) have been
!   added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------


      print *, "*************FIELD ROUTE UNIT TESTS***************************"
      print *

      call ESMF_Initialize(rc)

      ! Make a default 1xN layout
      layout0 = ESMF_DELayoutCreate(rc)
      call ESMF_DELayoutGetNumDEs(layout0, nDEs, rc)

      half = nDEs / 2
      quart = nDEs / 4

      ! Make a Nx4 and Nx2 layout
      delist = (/ (i, i=0, ndes-1) /)
      layout1 = ESMF_DELayoutCreate(delist, 2, (/ quart, 4 /), (/ 0, 0 /), rc)
      if (rc .eq. ESMF_FAILURE) then
        print *, "cannot create 1x4 layout"
        goto 10
      endif
      print *, "Layout 1:"
      call ESMF_DELayoutPrint(layout1, "", rc)
      layout2 = ESMF_DELayoutCreate(delist, 2, (/ half, 2 /), (/ 0, 0 /), rc)
      if (rc .eq. ESMF_FAILURE) then
        print *, "cannot create 2x2 layout"
        goto 10
      endif
      print *, "Layout 2:"
      call ESMF_DELayoutPrint(layout2, "", rc)

      call ESMF_DELayoutGetDEid(layout1, myde, rc)

      !------------------------------------------------------------------------
      counts(1) = 48
      counts(2) = 24
      min(1) = 0.0
      max(1) = 20.0
      min(2) = 0.0
      max(2) = 5.0
      horz_gridtype = ESMF_GridType_XY
      horz_stagger = ESMF_GridStagger_A
      horz_coord_system = ESMF_CoordSystem_Cartesian
      gname = "test grid 1"

      !NEX_UTest
      grid1 = ESMF_GridCreateLogRectUniform(2, counts=counts, &
                              minGlobalCoordPerDim=min, &
                              maxGlobalCoordPerDim=max, &
                              horzGridType=horz_gridtype, &
                              horzStagger=horz_stagger, &
                              horzCoordSystem=horz_coord_system, &
                              layout=layout1, &
                              name=gname, rc=status)

      write(failMsg, *) ""
      write(name, *) "Creating a source Test Grid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! Verifing that an uninitialized Grid can be printed
      call ESMF_GridPrint(grid3, "", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing an uninitialized Grid Test"
!     call 
      ! Second grid
      gname = "test grid 2"
      grid2 = ESMF_GridCreateLogRectUniform(2, counts=counts, &
                              minGlobalCoordPerDim=min, &
                              maxGlobalCoordPerDim=max, &
                              horzGridType=horz_gridtype, &
                              horzStagger=horz_stagger, &
                              horzCoordSystem=horz_coord_system, &
                              layout=layout1, &
                              name=gname, rc=status)
      write(failMsg, *) ""
      write(name, *) "Creating a destination Test Grid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! Verifing that an Array can be created
      call ESMF_GridGetDE(grid1, localCellCountPerDim=g1_cells, &
                          horzRelloc=ESMF_CELL_CENTER)
      allocate(f90ptr1(g1_cells(1), g1_cells(2)))
      f90ptr1 = 10+myde
      arr1 = ESMF_ArrayCreate(f90ptr1, ESMF_DATA_REF, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a src Test Array"
      call ESMF_ArrayPrint(arr1)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! second array
      call ESMF_GridGetDE(grid2, localCellCountPerDim=g2_cells, &
                          horzRelloc=ESMF_CELL_CENTER)
      allocate(f90ptr2(g2_cells(1), g2_cells(2)))
      f90ptr2 = -1
      arr2 = ESMF_ArrayCreate(f90ptr2, ESMF_DATA_REF, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a dst Test Array"
      call ESMF_ArrayPrint(arr2)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! Verifing that a Field can be created with a Grid and Array
      f1 = ESMF_FieldCreate(grid1, arr1, ESMF_DATA_REF, ESMF_CELL_CENTER, &
                            ESMF_CELL_CELL, 1, dm, "Field 0", ios, rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Field with a Grid and Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! second field
      f2 = ESMF_FieldCreate(grid2, arr2, ESMF_DATA_REF, ESMF_CELL_CENTER, &
                            ESMF_CELL_CELL, 1, dm, "Field 1", ios, rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Field with a Grid and Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! route test
      rh = ESMF_RouteHandleCreate(rc)
      call ESMF_FieldRedistStore(f1, f2, layout1, rh, rc=rc)
      call ESMF_FieldRedist(f1, f2, rh, rc=rc)
      call ESMF_FieldRedistRelease(rh, rc)
      call ESMF_RouteHandleDestroy(rh, rc)
      write(failMsg, *) ""
      write(name, *) "Calling Field Redist"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! results
      call ESMF_FieldPrint(f2)
      call ESMF_ArrayPrint(arr2)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------

      call ESMF_FieldDestroy(f1)
      call ESMF_FieldDestroy(f2)
      call ESMF_GridDestroy(grid1)
      call ESMF_GridDestroy(grid2)
      call ESMF_ArrayDestroy(arr1)
      call ESMF_ArrayDestroy(arr2)

10    print *, "end of Field Route test"

      call ESMF_Finalize(rc)

      end program ESMF_FRouteUTest
