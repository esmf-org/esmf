! $Id: ESMF_FieldTest.F90,v 1.1 2003/03/10 21:54:22 cdeluca Exp $
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
      program ESMF_FieldsTest

!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_FieldsTest - One line general statement about this test
!
! !DESCRIPTION:
!
! The code in this file drives F90 Fields unit tests.
! The companion file ESMF\_Fields.F90 contains the definitions for the
! Fields methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_BaseMod
      use ESMF_IOMod
      use ESMF_ArrayMod
      use ESMF_DataMapMod
      use ESMF_GridMod
      use ESMF_FieldMod

      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_FieldTest.F90,v 1.1 2003/03/10 21:54:22 cdeluca Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc = 1

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

!     !LOCAL VARIABLES:
      integer :: x, y
      type(ESMF_Grid) :: grid
      type(ESMF_Array) :: arr
      real, dimension(:,:), pointer :: f90ptr1
      type(ESMF_DataMap) :: dm
      type(ESMF_RelLoc) :: rl
      character (len = 20) :: fname
      type(ESMF_IOSpec) :: ios
      type(ESMF_Field) :: f1, f2, f3, f4, f5

      print *, "******************FIELDS UNIT TESTS****************************"
      print *
      call ESMF_FieldPrint(f1, rc=rc)
      write(failMsg, *) "Uninitialized Field f1 print failed"
      write(name, *) "Printing an uninitialized Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *
      f1 = ESMF_FieldCreateNoData()
      write(failMsg, *) "Field f1 create failed"
      write(name, *) "Creating a Field with no data Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *
      call ESMF_FieldPrint(f1, rc=rc)
      write(failMsg, *) "Initialized Field print failed"
      write(name, *) "Printing an initialized Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *
      call ESMF_FieldGetName(f1, fname, rc=rc)
      write(failMsg, *) "Get name of a Field with no data failed"
      write(name, *) "Getting name of Field with no data Test"
      call ESMF_Test((fname.eq."default_name"), name, failMsg, result, ESMF_SRCLINE)

      print *
      call ESMF_FieldDestroy(f1, rc=rc)
      write(failMsg, *) "Destroy Field failed"
      write(name, *) "Destroying initialized Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!      The following code is commented out because it crashes the program.
!      It will be uncommented when the bug is fixed.
!      print *
!      call ESMF_FieldGetName(f1, fname, rc=rc)
!      write(failMsg, *) "Get name of a destroyed Field failed"
!      write(name, *) "Getting name of a destroyed Field Test"
!      call ESMF_Test((rc.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)
!
      print *
      call ESMF_FieldPrint(f1, rc=rc)
      write(failMsg, *) "Printing destroyed Field failed"
      write(name, *) "Printing destroyed Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *
      f2 = ESMF_FieldCreateNoData("pressure", rc=rc)
      write(failMsg, *) "Creating a Field with a name failed"
      write(name, *) "Creating Field with name Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *
      call ESMF_FieldPrint(f2)

      print *
      Call ESMF_FieldGetName(f2, fname, rc=rc)
      write(failMsg, *) "Getting a Field name failed"
      write(name, *) "Getting a Field name Test"
      call ESMF_Test((fname.eq."pressure"), name, failMsg, result, ESMF_SRCLINE)
      print *
      call ESMF_FieldPrint(f2)

      print *
      f2 = ESMF_FieldCreateNoData("temperature", rc=rc)
      write(failMsg, *) "Field f2 (temperature) should not have been created"
      write(name, *) "Recreate a created Field Test"
      call ESMF_Test((rc.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f2)
      call ESMF_FieldDestroy(f2)

      print *
      f2 = ESMF_FieldCreateNoData("precipitation", rc=rc)
      write(failMsg, *) "Field f2 (precipitation) should have been created"
      write(name, *) "Recreate a destroyed Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f2)

      print *
      grid =  ESMF_GridCreate("atmgrid", rc=rc)
      write(failMsg, *) "Creating a Grid Failed "
      write(name, *) "Creating a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

     
      print *
      call ESMF_GridPrint(grid, "", rc=rc)
      write(failMsg, *) "Printing a Grid Failed "
      write(name, *) "Printing a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *
      grid =  ESMF_GridCreate("landgrid", rc=rc)
      write(failMsg, *) "Grid should not have been created, it had previously been created"
      write(name, *) "Recreating a created Grid Test"
      call ESMF_Test((rc.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)

      print *
      allocate(f90ptr1(10,20))
      arr = ESMF_ArrayCreate(f90ptr1, ESMF_NO_COPY, rc=rc)
      write(failMsg, *) "Creating an Array Failed "
      write(name, *) "Creating an Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *
      call ESMF_ArrayPrint(arr, rc=rc)
      print *
      write(failMsg, *) "Printing an Array Failed "
      write(name, *) "Printing an Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *
      allocate(f90ptr1(10,20))
      arr = ESMF_ArrayCreate(f90ptr1, ESMF_NO_COPY, rc=rc)
      write(failMsg, *) "The array should not have been created, it had been created previously "
      write(name, *) "Recreating a created Array Test"
      call ESMF_Test((rc.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)

      print *
      f3 = ESMF_FieldCreate(grid, arr, ESMF_NO_COPY, ESMF_CELL_CENTER, &
                                   dm, "Field 0", ios, rc)
      write(failMsg, *) "Creating an Field with a Grid and Array Failed "
      write(name, *) "Creating a Field with a Grid and Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *
      call ESMF_FieldPrint(f3)

      print *
      call ESMF_FieldDestroy(f3, rc=rc)
      write(failMsg, *) "Destroying an Field with an Grid and Array Failed "
      write(name, *) "Destroying a Field with a Grid and Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f3)

      print *
      call ESMF_FieldDestroy(f2, rc=rc)
      write(failMsg, *) "Destroying an Field with no data Failed "
      write(name, *) "Destroying a Field with no data Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f2)


      print *
      call ESMF_FieldDestroy(f2, rc=rc)
      write(failMsg, *) "Destroying Field that has already been destroyed "
      write(name, *) "Destroying a destroyed Field Test"
      call ESMF_Test((rc.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f2)

      ! test internal dynamic allocation within statically allocated
      !   ESMF_Fields
      !call ESMF_FieldsConstruct(fields, args, rc)
      !write(failMsg, *) "rc =", rc, ", args =", args
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      !failMsg, result, ESMF_SRCLINE)

      ! test initialization of members of statically allocated ESMF_Fields
      !   may want to read back values via Get methods for comparison
      !call ESMF_FieldsInit(fields, args, rc)
      !write(failMsg, *) "rc =", rc, ", args =", args
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      !failMsg, result, ESMF_SRCLINE)

      ! test setting of configuration values
      !type(ESMF_FieldsConfig) config_set
      !call ESMF_FieldsSetConfig(fields, config_set, rc)
      !write(failMsg, *) "rc =", rc, ", config_set =", config_set
      !call ESMF_Test((rc.eq.ESMF_SUCCESS),  &
                      !failMsg, result, ESMF_SRCLINE)

      ! test getting of configuration values,
      !  compare to values set previously
      !type(ESMF_FieldsConfig) :: config_get
      !call ESMF_FieldsGetConfig(fields, config_get, rc)
      !write(failMsg, *) "rc =", rc, ", config_get =", config_get
      !call ESMF_Test((rc.eq.ESMF_SUCCESS .and. config_get .eq. config_set), &
                      !failMsg, result, ESMF_SRCLINE)

      ! test setting of ESMF_Fields members values
      !<value type> :: value_set
      !call ESMF_FieldsSet<Value>(fields, value_set, rc)
      !write(failMsg, *) "rc =", rc, ", value_set =", value_set
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      !failMsg, result, ESMF_SRCLINE)

      ! test getting of ESMF_Fields members values,
      !   compare to values set previously
      !<value type> :: value_get
      !call ESMF_FieldsGet<Value>(fields, value_get, rc)
      !write(failMsg, *) "rc =", rc, ", value_get =", value_get
      !call ESMF_Test((rc.eq.ESMF_SUCCESS .and. value_get .eq. value_set), &
                      !failMsg, result, ESMF_SRCLINE)
    
      ! test validate method via option string
      !character(ESMF_MAXSTR) :: validate_options
      !call ESMF_FieldsValidate(fields, validate_options, rc)
      !write(failMsg, *) "rc =", rc, ", validate_options =", validate_options
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      !failMsg, result, ESMF_SRCLINE)

      ! test print method via option string
      !character(ESMF_MAXSTR) :: print_options
      !call ESMF_FieldsPrint(fields, print_options, rc)
      !write(failMsg, *) "rc =", rc, ", print_options =", print_options
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      !failMsg, result, ESMF_SRCLINE)

      ! test internal dynamic deallocation within statically allocated 
      !   ESMF_Fields
      !call ESMF_FieldsDestruct(fields, rc)
      !write(failMsg, *) "rc =", rc
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      !failMsg, result, ESMF_SRCLINE)

      ! test dynamic deallocation of ESMF_Fields
      !   also tests destructor
      !call ESMF_FieldsDestroy(fields, rc)
      !write(failMsg, *) "rc =", rc
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      !failMsg, result, ESMF_SRCLINE)

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?
  
      end program ESMF_FieldsTest
