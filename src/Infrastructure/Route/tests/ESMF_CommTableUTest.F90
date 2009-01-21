! $Id: ESMF_CommTableUTest.F90,v 1.7.2.3 2009/01/21 21:25:23 cdeluca Exp $
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
      program ESMF_CommTableTest

#include "ESMF.h"

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
    '$Id: ESMF_CommTableUTest.F90,v 1.7.2.3 2009/01/21 21:25:23 cdeluca Exp $'
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
      type(ESMF_VM) :: vm
      integer :: myvmid
      integer :: vmcount

      !character(32) :: validate_options
      character(32) :: print_options


      print_options = ""

      !------------------------------------------------------------------------

      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

      if (.not. ESMF_TestMinPETs(4, ESMF_SRCLINE)) goto 10

      ! get the VM PET count
      call ESMF_VMGetGlobal(vm, rc=rc)
      call ESMF_VMGet(vm, petcount=vmcount, localpet=myvmid, rc=rc)


      !------------------------------------------------------------------------
      !NEX_removeUTest_Multi_Proc_Only
      ! test dynamic allocation of ESMF_CommTable
      rtable = ESMF_CommTableCreate(myvmid, vmcount, rc)
      write(name, *) "ESMF_CommTableCreate"
      write(failMsg, *) "rc =", rc, ", args =", myvmid, vmcount
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
    
      !------------------------------------------------------------------------
      !
      ! test validate method via option string
      !call ESMF_CommTableValidate(rtable, validate_options, rc)
      !write(name, *) "ESMF_CommTableValidate"
      !write(failMsg, *) "rc =", rc, ", validate_options =", validate_options
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), &
      !                name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest_Multi_Proc_Only
      ! test print method via option string
      call ESMF_CommTablePrint(rtable, print_options, rc)
      write(name, *) "ESMF_CommTablePrint"
      write(failMsg, *) "rc =", rc, ", print_options =", trim(print_options)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !
      ! test dynamic deallocation of ESMF_CommTable
      !   also tests destructor
      !call ESMF_CommTableDestroy(rtable, rc)
      !write(name, *) "ESMF_CommTableDestroy"
      !write(failMsg, *) "rc =", rc
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), &
      !                name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
#ifdef ESMF_TESTEXHAUSTIVE

#endif
      !------------------------------------------------------------------------

10    continue

      call ESMF_TestEnd(result, ESMF_SRCLINE)

  
      end program ESMF_CommTableTest
