! $Id: ESMF_CommTableTest.F90,v 1.1 2003/03/10 23:21:13 nscollins Exp $
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

!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
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
      use ESMF_CommTableMod  ! the class to test
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_CommTableTest.F90,v 1.1 2003/03/10 23:21:13 nscollins Exp $'
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
      type(ESMF_CommTable) :: commtable

      ! test dynamic allocation of ESMF_CommTable
      commtable = ESMF_CommTableCreate(args, rc)
      write(name, *) "ESMF_CommTableCreate"
      write(failMsg, *) "rc =", rc, ", args =", args
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
    
      ! test internal dynamic allocation within statically allocated
      !   ESMF_CommTable
      call ESMF_CommTableConstruct(commtable, args, rc)
      write(name, *) "ESMF_CommTableConstruct"
      write(failMsg, *) "rc =", rc, ", args =", args
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test initialization of members of statically allocated ESMF_CommTable
      !   may want to read back values via Get methods for comparison
      call ESMF_CommTableInit(commtable, args, rc)
      write(name, *) "ESMF_CommTableInit"
      write(failMsg, *) "rc =", rc, ", args =", args
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test setting of configuration values
      type(ESMF_CommTableConfig) config_set
      call ESMF_CommTableSetConfig(commtable, config_set, rc)
      write(name, *) "ESMF_CommTableSetConfig"
      write(failMsg, *) "rc =", rc, ", config_set =", config_set
      call ESMF_Test((rc.eq.ESMF_SUCCESS),  &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test getting of configuration values,
      !  compare to values set previously
      type(ESMF_CommTableConfig) :: config_get
      call ESMF_CommTableGetConfig(commtable, config_get, rc)
      write(name, *) "ESMF_CommTableGetConfig"
      write(failMsg, *) "rc =", rc, ", config_get =", config_get
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. config_get .eq. config_set), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test setting of ESMF_CommTable members values
      <value type> :: value_set
      call ESMF_CommTableSet<Value>(commtable, value_set, rc)
      write(name, *) "ESMF_CommTableSet<Value>"
      write(failMsg, *) "rc =", rc, ", value_set =", value_set
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test getting of ESMF_CommTable members values,
      !   compare to values set previously
      <value type> :: value_get
      call ESMF_CommTableGet<Value>(commtable, value_get, rc)
      write(name, *) "ESMF_CommTableGet<Value>"
      write(failMsg, *) "rc =", rc, ", value_get =", value_get
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. value_get .eq. value_set), &
                      name, failMsg, result, ESMF_SRCLINE)
    
      ! test validate method via option string
      character(ESMF_MAXSTR) :: validate_options
      call ESMF_CommTableValidate(commtable, validate_options, rc)
      write(name, *) "ESMF_CommTableValidate"
      write(failMsg, *) "rc =", rc, ", validate_options =", validate_options
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test print method via option string
      character(ESMF_MAXSTR) :: print_options
      call ESMF_CommTablePrint(commtable, print_options, rc)
      write(name, *) "ESMF_CommTablePrint"
      write(failMsg, *) "rc =", rc, ", print_options =", print_options
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test internal dynamic deallocation within statically allocated 
      !   ESMF_CommTable
      call ESMF_CommTableDestruct(commtable, rc)
      write(name, *) "ESMF_CommTableDestruct"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      failMsg, result, ESMF_SRCLINE)

      ! test dynamic deallocation of ESMF_CommTable
      !   also tests destructor
      call ESMF_CommTableDestroy(commtable, rc)
      write(name, *) "ESMF_CommTableDestroy"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?
  
      end program ESMF_CommTableTest
