! $Id: ESMF_DELayoutCreateUTest.F90,v 1.1 2003/03/15 01:01:05 eschwab Exp $
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
      program ESMF_DELayoutCreateTest

!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_DELayoutCreateTest - tests parent-sublayout create methods
!
! !DESCRIPTION:
!
! The code in this file drives F90 DELayout unit tests.
! The companion file ESMF\_DELayout.F90 contains the definitions for the
! DELayout methods.
!
! run with 8 MPI processes
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_DELayoutMod   ! the class to test
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_DELayoutCreateUTest.F90,v 1.1 2003/03/15 01:01:05 eschwab Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg

      ! instantiate a DELayout
      type(ESMF_DELayout) :: layout1D, layout0, layout1, layout2, layout3

      ! number of DEs in default 1D layout
      integer :: numDEs

      ! test dynamic allocation of default 1D ESMF_DELayout
      layout1D = ESMF_DELayoutCreate(rc)
      write(name, *) "ESMF_DELayoutCreateDefault1D"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test getting of number of DEs discovered in creating 1D layout
      call ESMF_DELayoutGetNumDEs(layout1D, numDEs, rc)
      !print *, "layout1D numDEs = ", numDEs
      write(name, *) "ESMF_DELayoutGetNumDEs"
      write(failMsg, *) "rc =", rc, ", numDEs =", numDEs
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

#ifdef EXHAUSTIVE
      ! test print method via option string
      call ESMF_DELayoutPrint(layout1D, rc=rc)
      write(name, *) "ESMF_DELayoutPrint"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
#endif

      layout0 = ESMF_DELayoutCreate(2,4, layout1D, ESMF_XFAST, rc)
      write(name, *) "ESMF_DELayoutCreate2D non-exclusive"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test getting of number of DEs in creating 2D layout
      call ESMF_DELayoutGetNumDEs(layout0, numDEs, rc)
      !print *, "layout0 numDEs = ", numDEs
      write(name, *) "ESMF_DELayoutGetNumDEs"
      write(failMsg, *) "rc =", rc, ", numDEs =", numDEs
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      layout1 = ESMF_DELayoutCreate(2,2, layout1D, ESMF_XFAST, ESMF_EXCL, rc)
      write(name, *) "ESMF_DELayoutCreate2D exclusive -- 1st call"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test getting of number of DEs in creating 2D layout
      call ESMF_DELayoutGetNumDEs(layout1, numDEs, rc)
      !print *, "layout1 numDEs = ", numDEs
      write(name, *) "ESMF_DELayoutGetNumDEs"
      write(failMsg, *) "rc =", rc, ", numDEs =", numDEs
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      layout2 = ESMF_DELayoutCreate(1,3, layout1D, ESMF_YFAST, ESMF_EXCL, rc)
      write(name, *) "ESMF_DELayoutCreate2D exclusive -- 2nd call"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test getting of number of DEs in creating 2D layout
      call ESMF_DELayoutGetNumDEs(layout2, numDEs, rc)
      !print *, "layout2 numDEs = ", numDEs
      write(name, *) "ESMF_DELayoutGetNumDEs"
      write(failMsg, *) "rc =", rc, ", numDEs =", numDEs
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      layout3 = ESMF_DELayoutCreate(1,1, layout1D, ESMF_XFAST, ESMF_EXCL, rc)
      write(name, *) "ESMF_DELayoutCreate2D exclusive -- 3rd call"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test getting of number of DEs in creating 2D layout
      call ESMF_DELayoutGetNumDEs(layout3, numDEs, rc)
      !print *, "layout3 numDEs = ", numDEs
      write(name, *) "ESMF_DELayoutGetNumDEs"
      write(failMsg, *) "rc =", rc, ", numDEs =", numDEs
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

#ifdef EXHAUSTIVE
      ! test print method via option string
      call ESMF_DELayoutPrint(layout1D, rc=rc)
      write(name, *) "ESMF_DELayoutPrint"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
#endif

      ! test dynamic deallocation of ESMF_DELayout
      !   also tests destructor
      call ESMF_DELayoutDestroy(layout1D, rc)
      write(name, *) "ESMF_DELayoutDestroy layout1D"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_DELayoutDestroy(layout0, rc)
      write(name, *) "ESMF_DELayoutDestroy layout0"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_DELayoutDestroy(layout1, rc)
      write(name, *) "ESMF_DELayoutDestroy layout1"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_DELayoutDestroy(layout2, rc)
      write(name, *) "ESMF_DELayoutDestroy layout2"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_DELayoutDestroy(layout3, rc)
      write(name, *) "ESMF_DELayoutDestroy layout3"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?

      ! entire test pass/fail result
      write(name, *) "ESMF_DELayoutCreateTest"
      write(failMsg, *) "ESMF_DELayoutCreateTest failed one or more tests" 
      call ESMF_Test((result.eq.0), &
                      name, failMsg, result, ESMF_SRCLINE)
 
      end program ESMF_DELayoutCreateTest
