! $Id: ESMF_RTableUTest.F90,v 1.5.2.2 2009/01/21 21:25:23 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_RTableTest

#include "ESMF_Macros.inc"
!------------------------------------------------------------------------------
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
      use ESMF_Mod         ! rest of esmf
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_RTableUTest.F90,v 1.5.2.2 2009/01/21 21:25:23 cdeluca Exp $'
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
      character(ESMF_MAXSTR) :: print_options
      character(ESMF_MAXSTR) :: validate_options
      type(ESMF_VM) :: vm
      integer :: myvmid
      integer :: vmcount

      !------------------------------------------------------------------------

      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
      if (.not. ESMF_TestMinPETs(4, ESMF_SRCLINE)) goto 10

      call ESMF_VMGetGlobal(vm, rc=rc)
      call ESMF_VMGet(vm, localpet=myvmid, petcount=vmcount, rc=rc)


      !------------------------------------------------------------------------
      !
      ! test dynamic allocation of ESMF_RTable
      rtable = ESMF_RTableCreate(myvmid, vmcount, rc)
      write(name, *) "ESMF_RTableCreate"
      write(failMsg, *) "rc =", rc, ", args =", myvmid, vmcount
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
    

      !------------------------------------------------------------------------
      !
      ! test validate method via option string
      call ESMF_RTableValidate(rtable, validate_options, rc)
      write(name, *) "ESMF_RTableValidate"
      write(failMsg, *) "rc =", rc, ", validate_options =", validate_options
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !
      ! test print method via option string
      call ESMF_RTablePrint(rtable, print_options, rc)
      write(name, *) "ESMF_RTablePrint"
      write(failMsg, *) "rc =", rc, ", print_options =", print_options
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !
      ! test dynamic deallocation of ESMF_RTable
      !   also tests destructor
      call ESMF_RTableDestroy(rtable, rc)
      write(name, *) "ESMF_RTableDestroy"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      
10    continue

      call ESMF_TestEnd(result, ESMF_SRCLINE)
  
      end program ESMF_RTableTest
