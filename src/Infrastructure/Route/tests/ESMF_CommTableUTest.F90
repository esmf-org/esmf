! $Id: ESMF_CommTableUTest.F90,v 1.2 2003/04/25 20:49:41 nscollins Exp $
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
      program ESMF_CommTableTest

#include "ESMF_Macros.inc"

!------------------------------------------------------------------------------
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_CommTableTest - One line general statement about this test
!
! !DESCRIPTION:
!
! The code in this file drives F90 CommTable unit tests.
! The companion file ESMF\_CommTable.F90 contains the definitions for the
! CommTable methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod         ! all esmf
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_CommTableUTest.F90,v 1.2 2003/04/25 20:49:41 nscollins Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg

      ! instantiate a CommTable 
      type(ESMF_CommTable) :: rtable
 
      ! local args needed to create/construct objects
      integer :: mydeid
      integer :: decount

      mydeid = 1
      decount = 4

      ! test dynamic allocation of ESMF_CommTable
      rtable = ESMF_CommTableCreate(mydeid, decount, rc)
      write(name, *) "ESMF_CommTableCreate"
      write(failMsg, *) "rc =", rc, ", args =", mydeid, decount
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
    
      ! test internal dynamic allocation within statically allocated
      !   ESMF_CommTable
      call ESMF_CommTableConstruct(rtable, mydeid, decount, rc)
      write(name, *) "ESMF_CommTableConstruct"
      write(failMsg, *) "rc =", rc, ", args =", mydeid, decount
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test validate method via option string
      character(ESMF_MAXSTR) :: validate_options
      call ESMF_CommTableValidate(rtable, validate_options, rc)
      write(name, *) "ESMF_CommTableValidate"
      write(failMsg, *) "rc =", rc, ", validate_options =", validate_options
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test print method via option string
      character(ESMF_MAXSTR) :: print_options
      call ESMF_CommTablePrint(rtable, print_options, rc)
      write(name, *) "ESMF_CommTablePrint"
      write(failMsg, *) "rc =", rc, ", print_options =", print_options
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test internal dynamic deallocation within statically allocated 
      !   ESMF_CommTable
      call ESMF_CommTableDestruct(rtable, rc)
      write(name, *) "ESMF_CommTableDestruct"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      failMsg, result, ESMF_SRCLINE)

      ! test dynamic deallocation of ESMF_CommTable
      !   also tests destructor
      call ESMF_CommTableDestroy(rtable, rc)
      write(name, *) "ESMF_CommTableDestroy"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?
  
      end program ESMF_CommTableTest
