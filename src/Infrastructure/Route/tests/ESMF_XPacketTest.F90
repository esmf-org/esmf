! $Id: ESMF_XPacketTest.F90,v 1.1 2003/03/14 15:28:16 nscollins Exp $
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
      program ESMF_RTableTest

!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_RTableTest - One line general statement about this test
!
! !DESCRIPTION:
!
! The code in this file drives F90 RTable unit tests.
! The companion file ESMF\_RTable.F90 contains the definitions for the
! RTable methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_RTableMod  ! the class to test
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_XPacketTest.F90,v 1.1 2003/03/14 15:28:16 nscollins Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg

      ! instantiate a RTable 
      type(ESMF_RTable) :: rtable
 
      ! local args needed to create/construct objects
      integer :: mydeid
      integer :: decount

      mydeid = 1
      decount = 4

      ! test dynamic allocation of ESMF_RTable
      rtable = ESMF_RTableCreate(mydeid, decount, rc)
      write(name, *) "ESMF_RTableCreate"
      write(failMsg, *) "rc =", rc, ", args =", mydeid, decount
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
    
      ! test internal dynamic allocation within statically allocated
      !   ESMF_RTable
      call ESMF_RTableConstruct(rtable, mydeid, decount, rc)
      write(name, *) "ESMF_RTableConstruct"
      write(failMsg, *) "rc =", rc, ", args =", mydeid, decount
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test validate method via option string
      character(ESMF_MAXSTR) :: validate_options
      call ESMF_RTableValidate(rtable, validate_options, rc)
      write(name, *) "ESMF_RTableValidate"
      write(failMsg, *) "rc =", rc, ", validate_options =", validate_options
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test print method via option string
      character(ESMF_MAXSTR) :: print_options
      call ESMF_RTablePrint(rtable, print_options, rc)
      write(name, *) "ESMF_RTablePrint"
      write(failMsg, *) "rc =", rc, ", print_options =", print_options
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test internal dynamic deallocation within statically allocated 
      !   ESMF_RTable
      call ESMF_RTableDestruct(rtable, rc)
      write(name, *) "ESMF_RTableDestruct"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      failMsg, result, ESMF_SRCLINE)

      ! test dynamic deallocation of ESMF_RTable
      !   also tests destructor
      call ESMF_RTableDestroy(rtable, rc)
      write(name, *) "ESMF_RTableDestroy"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?
  
      end program ESMF_RTableTest
