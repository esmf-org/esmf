! $Id: ESMF_FRouteUTest.F90,v 1.5 2003/03/24 22:56:56 nscollins Exp $
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
! INCLUDES
#include <ESMF.h>
!
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
      use ESMF_BaseMod
      use ESMF_IOMod
      use ESMF_ArrayMod
      use ESMF_DataMapMod
      use ESMF_GridMod
      use ESMF_DELayoutMod
      use ESMF_RouteMod
      use ESMF_FieldMod

      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_FRouteUTest.F90,v 1.5 2003/03/24 22:56:56 nscollins Exp $'
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
      integer, dimension(:,:), pointer :: f90ptr1, f90ptr2
      type(ESMF_DataMap) :: dm
      type(ESMF_RelLoc) :: rl
      type(ESMF_DELayout) :: layout
      character (len = 20) :: fname, fname1, fname2, gname
      type(ESMF_IOSpec) :: ios
      type(ESMF_Field) :: f1, f2, f3, f4, f5
      integer :: nDE_i, nDE_j
      real :: x_min, x_max, y_min, y_max
      integer :: i_max, j_max
      integer :: horz_gridtype, vert_gridtype
      integer :: horz_stagger, vert_stagger
      integer :: horz_coord_system, vert_coord_system
      integer :: status, myde


      print *, "*************FIELD ROUTE UNIT TESTS***************************"
      print *

      ! the default layout
      layout = ESMF_DELayoutCreate(rc=rc)
      call ESMF_DELayoutGetDEid(layout, myde, rc)

      !------------------------------------------------------------------------
      i_max = 40
      j_max = 20
      horz_gridtype = ESMF_GridType_XY
      vert_gridtype = ESMF_GridType_Unknown
      horz_stagger = ESMF_GridStagger_A
      vert_stagger = ESMF_GridStagger_Unknown
      horz_coord_system = ESMF_CoordSystem_Cartesian
      vert_coord_system = ESMF_CoordSystem_Unknown
      x_min = 0.0
      x_max = 20.0
      y_min = 0.0
      y_max = 5.0
      gname = "test grid 1"

      grid1 = ESMF_GridCreate(i_max=i_max, j_max=j_max, &
                             nDE_i=4, nDE_j=1, &
                             horz_gridtype=horz_gridtype, &
                             vert_gridtype=vert_gridtype, &
                             horz_stagger=horz_stagger, &
                             vert_stagger=vert_stagger, &
                             horz_coord_system=horz_coord_system, &
                             vert_coord_system=vert_coord_system, &
                             x_min=x_min, x_max=x_max, &
                             y_min=y_min, y_max=y_max, &
                             name=gname, rc=status)

      write(failMsg, *) ""
      write(name, *) "Creating a source Test Grid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
     
      ! Second grid
      gname = "test grid 2"
      grid2 = ESMF_GridCreate(i_max=i_max, j_max=j_max, &
                             nDE_i=2, nDE_j=2, &
                             horz_gridtype=horz_gridtype, &
                             vert_gridtype=vert_gridtype, &
                             horz_stagger=horz_stagger, &
                             vert_stagger=vert_stagger, &
                             horz_coord_system=horz_coord_system, &
                             vert_coord_system=vert_coord_system, &
                             x_min=x_min, x_max=x_max, &
                             y_min=y_min, y_max=y_max, &
                             name=gname, rc=status)

      write(failMsg, *) ""
      write(name, *) "Creating a destination Test Grid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! Verifing that an Array can be created
      allocate(f90ptr1(10,20))
      f90ptr1 = 10+myde
      arr1 = ESMF_ArrayCreate(f90ptr1, ESMF_NO_COPY, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a src Test Array"
      call ESMF_ArrayPrint(arr1)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! second array
      allocate(f90ptr2(20,10))
      f90ptr2 = 100+myde
      arr2 = ESMF_ArrayCreate(f90ptr2, ESMF_NO_COPY, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a dst Test Array"
      call ESMF_ArrayPrint(arr2)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! Verifing that a Field can be created with a Grid and Array
      f1 = ESMF_FieldCreate(grid1, arr1, ESMF_NO_COPY, ESMF_CELL_CENTER, &
                                   dm, "Field 0", ios, rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Field with a Grid and Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! second field
      f2 = ESMF_FieldCreate(grid2, arr2, ESMF_NO_COPY, ESMF_CELL_CENTER, &
                                   dm, "Field 1", ios, rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Field with a Grid and Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! route test
      call ESMF_FieldRoute(f1, f2, layout, rc)
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

      end program ESMF_FRouteUTest
