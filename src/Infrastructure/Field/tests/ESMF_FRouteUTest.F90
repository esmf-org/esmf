! $Id: ESMF_FRouteUTest.F90,v 1.1 2003/03/17 21:53:20 nscollins Exp $
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
      '$Id: ESMF_FRouteUTest.F90,v 1.1 2003/03/17 21:53:20 nscollins Exp $'
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
      type(ESMF_Grid) :: grid, grid2, grid3, grid4
      type(ESMF_Array) :: arr, arr2
      integer, dimension(:,:), pointer :: f90ptr1, f90ptr2
      type(ESMF_DataMap) :: dm
      type(ESMF_RelLoc) :: rl
      type(ESMF_DELayout) :: layout
      character (len = 20) :: fname, fname1, fname2
      type(ESMF_IOSpec) :: ios
      type(ESMF_Field) :: f1, f2, f3, f4, f5

      print *, "*************FIELD ROUTE UNIT TESTS***************************"
      print *

      !------------------------------------------------------------------------

      ! Verifing that a Grid can be created
      grid =  ESMF_GridCreate("atmgrid", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a source Test Grid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
     
      ! Second grid
      grid2 =  ESMF_GridCreate("landgrid", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a destination Test Grid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! Verifing that an Array can be created
      allocate(f90ptr1(10,20))
      f90ptr1 = reshape( (/ (i,i=1,200) /), (/ 10, 20 /) )
      arr = ESMF_ArrayCreate(f90ptr1, ESMF_NO_COPY, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a src Test Array"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! second array
      allocate(f90ptr2(10,20))
      f90ptr2 = reshape( (/ (i,i=601,800) /), (/ 10, 20 /) )
      arr2 = ESMF_ArrayCreate(f90ptr2, ESMF_NO_COPY, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a src Test Array"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! Verifing that a Field can be created with a Grid and Array
      f1 = ESMF_FieldCreate(grid, arr, ESMF_NO_COPY, ESMF_CELL_CENTER, &
                                   dm, "Field 0", ios, rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Field with a Grid and Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f1)
      !------------------------------------------------------------------------

      ! second field
      f2 = ESMF_FieldCreate(grid2, arr2, ESMF_NO_COPY, ESMF_CELL_CENTER, &
                                   dm, "Field 1", ios, rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Field with a Grid and Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f2)
      !------------------------------------------------------------------------

      ! the default layout is ok for now
      layout = ESMF_DELayoutCreate(rc=rc)

      ! route test
      call ESMF_FieldRoute(f1, f2, layout, rc)
      write(failMsg, *) ""
      write(name, *) "Calling Field Route"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f2)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------

      call ESMF_FieldDestroy(f1)
      call ESMF_FieldDestroy(f2)
      call ESMF_GridDestroy(grid)
      call ESMF_GridDestroy(grid2)
      call ESMF_ArrayDestroy(arr)
      call ESMF_ArrayDestroy(arr2)

      end program ESMF_FRouteUTest
