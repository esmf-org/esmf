! $Id: ESMF_RTableTest.F90,v 1.1 2003/03/10 23:21:14 nscollins Exp $
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
      '$Id: ESMF_RTableTest.F90,v 1.1 2003/03/10 23:21:14 nscollins Exp $'
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

      ! test dynamic allocation of ESMF_RTable
      rtable = ESMF_RTableCreate(args, rc)
      write(name, *) "ESMF_RTableCreate"
      write(failMsg, *) "rc =", rc, ", args =", args
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
    
      ! test internal dynamic allocation within statically allocated
      !   ESMF_RTable
      call ESMF_RTableConstruct(rtable, args, rc)
      write(name, *) "ESMF_RTableConstruct"
      write(failMsg, *) "rc =", rc, ", args =", args
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test initialization of members of statically allocated ESMF_RTable
      !   may want to read back values via Get methods for comparison
      call ESMF_RTableInit(rtable, args, rc)
      write(name, *) "ESMF_RTableInit"
      write(failMsg, *) "rc =", rc, ", args =", args
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test setting of configuration values
      type(ESMF_RTableConfig) config_set
      call ESMF_RTableSetConfig(rtable, config_set, rc)
      write(name, *) "ESMF_RTableSetConfig"
      write(failMsg, *) "rc =", rc, ", config_set =", config_set
      call ESMF_Test((rc.eq.ESMF_SUCCESS),  &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test getting of configuration values,
      !  compare to values set previously
      type(ESMF_RTableConfig) :: config_get
      call ESMF_RTableGetConfig(rtable, config_get, rc)
      write(name, *) "ESMF_RTableGetConfig"
      write(failMsg, *) "rc =", rc, ", config_get =", config_get
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. config_get .eq. config_set), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test setting of ESMF_RTable members values
      <value type> :: value_set
      call ESMF_RTableSet<Value>(rtable, value_set, rc)
      write(name, *) "ESMF_RTableSet<Value>"
      write(failMsg, *) "rc =", rc, ", value_set =", value_set
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test getting of ESMF_RTable members values,
      !   compare to values set previously
      <value type> :: value_get
      call ESMF_RTableGet<Value>(rtable, value_get, rc)
      write(name, *) "ESMF_RTableGet<Value>"
      write(failMsg, *) "rc =", rc, ", value_get =", value_get
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. value_get .eq. value_set), &
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
