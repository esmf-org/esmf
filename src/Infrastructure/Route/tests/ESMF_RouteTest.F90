! $Id: ESMF_RouteTest.F90,v 1.2 2003/03/17 17:53:30 nscollins Exp $
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
      program ESMF_RouteTest

!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_RouteTest - One line general statement about this test
!
! !DESCRIPTION:
!
! The code in this file drives F90 Route unit tests.
! The companion file ESMF\_Route.F90 contains the definitions for the
! Route methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_RouteMod  ! the class to test
      use ESMF_DELayoutMod
      use ESMF_ArrayMod
      use ESMF_GridMod
      use ESMF_FieldMod
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_RouteTest.F90,v 1.2 2003/03/17 17:53:30 nscollins Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: validate_options = "full"
      character(ESMF_MAXSTR) :: print_options = "all"

      ! instantiate a Route 
      type(ESMF_Route) :: route
 
      ! local args needed to create/construct objects
      type(ESMF_Field) :: srcfield, dstfield
      type(ESMF_Grid) :: srcgrid, dstgrid
      type(ESMF_Array) :: srcarray, dstarray
      type(ESMF_DELayout) :: mylayout


      ! all variable declarations above here -
      ! beginning of executable code below here

      ! make a layout for what's needed below
      mylayout = ESMF_DELayoutCreate(rc=rc)

      ! test the actual user entry point
      ! call ESMF_FieldRoute(srcfield, dstfield, mylayout)

      ! test dynamic allocation of ESMF_Route
      route = ESMF_RouteCreate(mylayout, rc)
      write(name, *) "ESMF_RouteCreate"
      write(failMsg, *) "rc =", rc, ", args ="
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    
      ! test internal dynamic allocation within statically allocated ESMF_Route
      !call ESMF_RouteConstruct(route, mylayout, rc)
      !write(name, *) "ESMF_RouteConstruct"
      !write(failMsg, *) "rc =", rc, ", args ="
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! test validate method via option string
      call ESMF_RouteValidate(route, validate_options, rc)
      write(name, *) "ESMF_RouteValidate"
      write(failMsg, *) "rc =", rc, ", validate_options =", validate_options
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! test print method via option string
      call ESMF_RoutePrint(route, print_options, rc)
      write(name, *) "ESMF_RoutePrint"
      write(failMsg, *) "rc =", rc, ", print_options =", print_options
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! test internal dynamic deallocation within statically allocated 
      !   ESMF_Route
      !call ESMF_RouteDestruct(route, rc)
      !write(name, *) "ESMF_RouteDestruct"
      !write(failMsg, *) "rc =", rc
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! test dynamic deallocation of ESMF_Route also tests destructor
      call ESMF_RouteDestroy(route, rc)
      write(name, *) "ESMF_RouteDestroy"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?
  
      end program ESMF_RouteTest
