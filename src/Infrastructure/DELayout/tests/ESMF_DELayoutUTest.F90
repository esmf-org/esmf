! $Id: ESMF_DELayoutUTest.F90,v 1.4 2004/04/09 19:53:59 eschwab Exp $
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
      use ESMF_Mod         ! the ESMF framework
      use ESMF_TestMod     ! test methods
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_DELayoutUTest.F90,v 1.4 2004/04/09 19:53:59 eschwab Exp $'
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

!-------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------

      call ESMF_Initialize(rc=rc)

      !NEX_UTest
      ! test dynamic allocation of default 1D ESMF_DELayout
      layout1D = ESMF_DELayoutCreate(rc)
      write(name, *) "ESMF_DELayoutCreateDefault1D"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !NEX_UTest
      ! test getting of number of DEs discovered in creating 1D layout
      call ESMF_DELayoutGetNumDEs(layout1D, numDEs, rc)
      !print *, "layout1D numDEs = ", numDEs
      write(name, *) "ESMF_DELayoutGetNumDEs"
      write(failMsg, *) "rc =", rc, ", numDEs =", numDEs
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! if uni-processor test, bail-out
      if (numDEs.eq.1) then
        ! print *, "uni-processor test, bailing out ..."
        goto 10
      endif

#ifdef ESMF_EXHAUSTIVE
      !EX_UTest
      ! test print method via option string
      call ESMF_DELayoutPrint(layout1D, rc=rc)
      write(name, *) "ESMF_DELayoutPrint"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
#endif

      !NEX_UTest
      layout0 = ESMF_DELayoutCreate(layout1D, 2, (/2,4/), (/0,0/), &
                                    de_indices=(/0,1,2,3,4,5,6,7/), rc=rc)
      write(name, *) "ESMF_DELayoutCreateFromParent non-exclusive"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !NEX_UTest
      ! test getting of number of DEs in creating 2D layout
      call ESMF_DELayoutGetNumDEs(layout0, numDEs, rc)
      !print *, "layout0 numDEs = ", numDEs
      write(name, *) "ESMF_DELayoutGetNumDEs"
      write(failMsg, *) "rc =", rc, ", numDEs =", numDEs
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_EXHAUSTIVE
      !EX_UTest
      layout1 = ESMF_DELayoutCreate(layout1D, 2, (/2,2/), (/0,0/), &
                                    de_indices=(/0,1,2,3/), rc=rc)
      write(name, *) "ESMF_DELayoutCreate2D exclusive -- 1st call"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! test getting of number of DEs in creating 2D layout
      call ESMF_DELayoutGetNumDEs(layout1, numDEs, rc)
      !print *, "layout1 numDEs = ", numDEs
      write(name, *) "ESMF_DELayoutGetNumDEs"
      write(failMsg, *) "rc =", rc, ", numDEs =", numDEs
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      layout2 = ESMF_DELayoutCreate(layout1D, 2, (/1,3/), (/0,0/), &
                                    de_indices=(/5,6,7/), rc=rc)
      write(name, *) "ESMF_DELayoutCreate2D exclusive -- 2nd call"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! test getting of number of DEs in creating 2D layout
      call ESMF_DELayoutGetNumDEs(layout2, numDEs, rc)
      !print *, "layout2 numDEs = ", numDEs
      write(name, *) "ESMF_DELayoutGetNumDEs"
      write(failMsg, *) "rc =", rc, ", numDEs =", numDEs
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      layout3 = ESMF_DELayoutCreate(layout1D, 2, (/1,1/), (/0,0/), &
                                    de_indices=(/4/), rc=rc)
      write(name, *) "ESMF_DELayoutCreate2D exclusive -- 3rd call"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! test getting of number of DEs in creating 2D layout
      call ESMF_DELayoutGetNumDEs(layout3, numDEs, rc)
      !print *, "layout3 numDEs = ", numDEs
      write(name, *) "ESMF_DELayoutGetNumDEs"
      write(failMsg, *) "rc =", rc, ", numDEs =", numDEs
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! test print method via option string
      call ESMF_DELayoutPrint(layout1D, rc=rc)
      write(name, *) "ESMF_DELayoutPrint"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
#endif

      !NEX_UTest
      ! test dynamic deallocation of ESMF_DELayout
      !   also tests destructor
      call ESMF_DELayoutDestroy(layout1D, rc)
      write(name, *) "ESMF_DELayoutDestroy layout1D"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !NEX_UTest
      call ESMF_DELayoutDestroy(layout0, rc)
      write(name, *) "ESMF_DELayoutDestroy layout0"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_EXHAUSTIVE
      !EX_UTest
      call ESMF_DELayoutDestroy(layout1, rc)
      write(name, *) "ESMF_DELayoutDestroy layout1"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      call ESMF_DELayoutDestroy(layout2, rc)
      write(name, *) "ESMF_DELayoutDestroy layout2"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      call ESMF_DELayoutDestroy(layout3, rc)
      write(name, *) "ESMF_DELayoutDestroy layout3"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
#endif

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?

10    continue

      !NEX_UTest
      ! entire test pass/fail result
      write(name, *) "ESMF_DELayoutCreateTest"
      write(failMsg, *) "ESMF_DELayoutCreateTest failed one or more tests" 
      call ESMF_Test((result.eq.0), &
                      name, failMsg, result, ESMF_SRCLINE)
 

      call ESMF_Finalize(rc)

      end program ESMF_DELayoutCreateTest
