! $Id: ESMF_FRouteUTest.F90,v 1.16 2003/07/17 20:19:37 nscollins Exp $
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
      '$Id: ESMF_FRouteUTest.F90,v 1.16 2003/07/17 20:19:37 nscollins Exp $'
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
      integer, dimension(:,:), pointer :: f90ptr1, f90ptr2
      type(ESMF_DataMap) :: dm
      type(ESMF_RelLoc) :: rl
      type(ESMF_DELayout) :: layout0, layout1, layout2
      integer :: delist(64)
      character (len = 20) :: fname, fname1, fname2, gname
      type(ESMF_IOSpec) :: ios
      type(ESMF_Field) :: f1, f2, f3, f4, f5
      integer :: nDEs, nDE_i, nDE_j
      integer :: half, quart
      real :: x_min, x_max, y_min, y_max
      integer :: i_max, j_max
      integer :: horz_gridtype, vert_gridtype
      integer :: horz_stagger, vert_stagger
      integer :: horz_coord_system, vert_coord_system
      integer :: status, myde

!--------------------------------------------------------------------------------
!     The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!     always run. When the environment variable, EXHAUSTIVE, is set to ON then
!     the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!     Special strings (Non-exhaustive and exhaustive) have been
!     added to allow a script to count the number and types of unit tests.
!--------------------------------------------------------------------------------



      print *, "*************FIELD ROUTE UNIT TESTS***************************"
      print *

      call ESMF_FrameworkInitialize(rc)

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
      i_max = 48
      j_max = 24
      x_min = 0.0
      x_max = 20.0
      y_min = 0.0
      y_max = 5.0
      horz_gridtype = ESMF_GridType_XY
      vert_gridtype = ESMF_GridType_Unknown
      horz_stagger = ESMF_GridStagger_A
      vert_stagger = ESMF_GridStagger_Unknown
      horz_coord_system = ESMF_CoordSystem_Cartesian
      vert_coord_system = ESMF_CoordSystem_Unknown
      gname = "test grid 1"

      !NEX_UTest
      grid1 = ESMF_GridCreate(i_max=i_max, j_max=j_max, &
                             x_min=x_min, x_max=x_max, &
                             y_min=y_min, y_max=y_max, &
                             layout=layout1, &
                             horz_gridtype=horz_gridtype, &
                             vert_gridtype=vert_gridtype, &
                             horz_stagger=horz_stagger, &
                             vert_stagger=vert_stagger, &
                             horz_coord_system=horz_coord_system, &
                             vert_coord_system=vert_coord_system, &
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
      grid2 = ESMF_GridCreate(i_max=i_max, j_max=j_max, &
                             x_min=x_min, x_max=x_max, &
                             y_min=y_min, y_max=y_max, &
                             layout=layout2, &
                             horz_gridtype=horz_gridtype, &
                             vert_gridtype=vert_gridtype, &
                             horz_stagger=horz_stagger, &
                             vert_stagger=vert_stagger, &
                             horz_coord_system=horz_coord_system, &
                             vert_coord_system=vert_coord_system, &
                             name=gname, rc=status)

      write(failMsg, *) ""
      write(name, *) "Creating a destination Test Grid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! Verifing that an Array can be created
      call ESMF_GridGetDE(grid1, lcelltot_index=g1_ai)
      allocate(f90ptr1(g1_ai(1)%max, g1_ai(2)%max))
      f90ptr1 = 10+myde
      arr1 = ESMF_ArrayCreate(f90ptr1, ESMF_DATA_REF, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a src Test Array"
      call ESMF_ArrayPrint(arr1)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! second array
      call ESMF_GridGetDE(grid2, lcelltot_index=g2_ai)
      allocate(f90ptr2(g2_ai(1)%max, g2_ai(2)%max))
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
                                   1, dm, "Field 0", ios, rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Field with a Grid and Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! second field
      f2 = ESMF_FieldCreate(grid2, arr2, ESMF_DATA_REF, ESMF_CELL_CENTER, &
                                   1, dm, "Field 1", ios, rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Field with a Grid and Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! route test
      call ESMF_FieldRoute(f1, f2, layout1, rc)
      write(failMsg, *) ""
      write(name, *) "Calling Field Route"
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

      call ESMF_FrameworkFinalize(rc)

      end program ESMF_FRouteUTest
