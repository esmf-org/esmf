! $Id: ESMF_BundleTest.F90,v 1.2 2003/03/11 20:06:09 svasquez Exp $
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
      program ESMF_BundleTest

!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_BundleTest - One line general statement about this test
!
! !DESCRIPTION:
!
! The code in this file drives F90 Bundle unit tests.
! The companion file ESMF\_Bundle.F90 contains the definitions for the
! Bundle methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_BundleMod  ! the class to test
      use ESMF_IOMod
      use ESMF_ArrayMod
      use ESMF_DataMapMod
      use ESMF_GridMod
      use ESMF_FieldMod

      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_BundleTest.F90,v 1.2 2003/03/11 20:06:09 svasquez Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0
!     ! Local variables
      integer :: i, x, y, rc, mycell, fieldcount
      type(ESMF_Grid) :: grid
      type(ESMF_ArraySpec) :: arrayspec
      type(ESMF_Array) :: arraya, arrayb
      type(ESMF_DataMap) :: datamap
      type(ESMF_RelLoc) :: relativelocation
      character (len = ESMF_MAXSTR) :: bname1, bname2, fname1, fname2, fname3
      type(ESMF_IOspec) :: iospec
      type(ESMF_Field) :: field(10), returnedfield1, returnedfield2, returnedfield3, simplefield
      type(ESMF_Bundle) :: bundle1, bundle2, bundle3, bundle4
      real (selected_real_kind(6,45)), dimension(:,:), pointer :: f90ptr1, f90ptr2



      ! individual test result code

      ! individual test name
      character(ESMF_MAXSTR) :: named


      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name


      ! instantiate a Bundle 
      type(ESMF_Bundle) :: bundle
      print *, "******************Bundles UNIT TESTS****************************"
      print *

!    ! Verify that getting the name  of an uninitialized Bundle is handled properly.
      call ESMF_BundleGetName(bundle1, bname1, rc)
     write(failMsg, *) "Failed"
     write(name, *) "Getting name of uninitalized Bundle Test"
     call ESMF_Test((rc.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)

!    !  Create several empty Fields and add them to a new Bundle.
     field(1) = ESMF_FieldCreateNoData(name="pressure", rc=rc)
     field(2) = ESMF_FieldCreateNoData(name="temperature", rc=rc)
     field(3) = ESMF_FieldCreateNoData(name="heat flux", rc=rc)

!    ! Verify that a Bundle can created with 3 Fields
     bundle1 = ESMF_BundleCreate(3, field, name="atmosphere data", rc=rc)
     write(failMsg, *) "Bundle Create failed"
     write(name, *) "Creating Bundle with 3 NoData Fields Test"
     call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     print *, "Bundle example 1 returned"

!    ! Verify that adding a Field to an uninitialized Bundle is handled correctly
!    ! This code is commented out because it crashes the program.
!    !It will be uncommented when the bug is fixed
     !call ESMF_BundleAddFields(bundle2, simplefield, rc=rc);
     !write(failMsg, *) "Add Field to uncreated Bundle failed"
     !write(name, *) "Adding a Field to an uncreated Bundle Test"
     !call ESMF_Test((rc.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)

     !  Verify that a Field can be added to a Bundle
     simplefield = ESMF_FieldCreate(grid, arrayspec, ESMF_CELL_CENTER, &
                                    name="rh", rc=rc)

     !  Verify that an empty Bundle can be created
     bundle2 = ESMF_BundleCreate(name="time step 1", rc=rc);
     write(failMsg, *) "Empty Bundle Create failed"
     write(name, *) "Creating Empty Bundle Test"
     call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

     !  Verify that an creating a created Bundle is handled properly
     bundle2 = ESMF_BundleCreate(name="time step 1", rc=rc);
     write(failMsg, *) "The bundle should not have been created"
     write(name, *) "Creating a Bundle that has already been created Test"
     call ESMF_Test((rc.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)

     !  Verify that a Field can be added to an empty Bundle
     call ESMF_BundleAddFields(bundle2, simplefield, rc=rc);
     write(failMsg, *) "Add Field to Empty Bundle failed"
     write(name, *) "Adding a Field to an Empty Bundle Test"
     call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

     !  Verify that the Field count can be queried from a Bundle
     call ESMF_BundleGetFieldCount(bundle2, fieldcount, rc);
     write(failMsg, *) "Getting Field count failed"
     write(name, *) "Getting Field count from a Bundle Test"
     call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(fieldcount.eq.1), name, failMsg, result, ESMF_SRCLINE)

     print *, "Bundle example 2 returned, fieldcount =", fieldcount

!   !  Create an empty Bundle and then add multiple fields to it.
     bundle3 = ESMF_BundleCreate(name="southern hemisphere", rc=rc);
   
     !  Verify that that multiple Fields can be added to a Bundle 
     call ESMF_BundleAddFields(bundle3, 3, field, rc);
     write(failMsg, *) "Adding Multiple Fields failed"
     write(name, *) "Adding multiple Fields to a Bundle Test"
     call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

     !  Verify that that Fields can be queried from a Bundle 
     call ESMF_BundleAddFields(bundle3, 3, field, rc);
     call ESMF_BundleGetFieldCount(bundle3, fieldcount, rc);
     write(failMsg, *) "Getting Field count failed"
     write(name, *) "Getting Field count from a Bundle Test"
     call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(fieldcount.eq.3), name, failMsg, result, ESMF_SRCLINE)

     print *, "Bundle example 3 returned, fieldcount =", fieldcount


     ! Verify that the first Field names can be queried fron a Bundle
     call ESMF_BundleGetFields(bundle1, "pressure", returnedfield1, rc)
     write(failMsg, *) "Getting first Field from Bundle failed"
     write(name, *) "Getting first a Field from a Bundle Test"
     call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     call ESMF_FieldGetName(returnedfield1, fname1, rc)
     write(failMsg, *) "Getting  first Field name from Bundle failed"
     write(name, *) "Getting first Field from a Bundle Test continued"
     call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(fname1.eq."pressure"), name, failMsg, result, ESMF_SRCLINE)

     ! Verify that the second Field names can be queried fron a Bundle
     call ESMF_BundleGetFields(bundle1, 2, returnedfield2, rc)
     write(failMsg, *) "Getting second Field from Bundle failed"
     write(name, *) "Getting a second Field from a Bundle Test"
     call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     call ESMF_FieldGetName(returnedfield2, fname2, rc)
     write(failMsg, *) "Getting second Field name from Bundle failed"
     write(name, *) "Getting a second Field from a Bundle Test continued"
     call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(fname2.eq."temperature"), name, failMsg, result, ESMF_SRCLINE)


     ! Verify that the third Field names can be queried fron a Bundle
     call ESMF_BundleGetFields(bundle1, 3, returnedfield3, rc)
     write(failMsg, *) "Getting third Field from Bundle failed"
     write(name, *) "Getting a third Field from a Bundle Test"
     call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     call ESMF_FieldGetName(returnedfield3, fname3, rc)
     write(failMsg, *) "Getting third Field name from Bundle failed"
     write(name, *) "Getting a third Field from a Bundle Test continued"
     call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(fname3.eq."heat flux"), name, failMsg, result, ESMF_SRCLINE)

     ! Verify that the Bundle name can be queried 
     call ESMF_BundleGetName(bundle1, bname1, rc)
     print *, "Bundle example 4 returned, field names = ", &
                    trim(fname2), ", ", trim(fname2), ", ", trim(fname3)
     print *, "Bundle name = ", trim(bname1)


      !bundle = ESMF_BundleCreate(args, rc)
      !write(name, *) "ESMF_BundleCreate"
      !write(failMsg, *) "rc =", rc, ", args =", args
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      !name, failMsg, result, ESMF_SRCLINE)
    
      !! test internal dynamic allocation within statically allocated
      !!   ESMF_Bundle
      !call ESMF_BundleConstruct(bundle, args, rc)
      !write(name, *) "ESMF_BundleConstruct"
      !write(failMsg, *) "rc =", rc, ", args =", args
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      !name, failMsg, result, ESMF_SRCLINE)
!
      !! test initialization of members of statically allocated ESMF_Bundle
      !!   may want to read back values via Get methods for comparison
      !call ESMF_BundleInit(bundle, args, rc)
      !write(name, *) "ESMF_BundleInit"
      !write(failMsg, *) "rc =", rc, ", args =", args
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      !name, failMsg, result, ESMF_SRCLINE)

      !! test setting of configuration values
      !type(ESMF_BundleConfig) config_set
      !call ESMF_BundleSetConfig(bundle, config_set, rc)
      !write(name, *) "ESMF_BundleSetConfig"
      !write(failMsg, *) "rc =", rc, ", config_set =", config_set
      !call ESMF_Test((rc.eq.ESMF_SUCCESS),  &
                      !name, failMsg, result, ESMF_SRCLINE)

      ! test getting of configuration values,
      !  compare to values set previously
      !type(ESMF_BundleConfig) :: config_get
      !call ESMF_BundleGetConfig(bundle, config_get, rc)
      !write(name, *) "ESMF_BundleGetConfig"
      !write(failMsg, *) "rc =", rc, ", config_get =", config_get
      !call ESMF_Test((rc.eq.ESMF_SUCCESS .and. config_get .eq. config_set), &
                      !name, failMsg, result, ESMF_SRCLINE)

      !! test setting of ESMF_Bundle members values
      !<value type> :: value_set
      !call ESMF_BundleSet<Value>(bundle, value_set, rc)
      !write(name, *) "ESMF_BundleSet<Value>"
      !write(failMsg, *) "rc =", rc, ", value_set =", value_set
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      !name, failMsg, result, ESMF_SRCLINE)

      !! test getting of ESMF_Bundle members values,
      !!   compare to values set previously
      !<value type> :: value_get
      !call ESMF_BundleGet<Value>(bundle, value_get, rc)
      !write(name, *) "ESMF_BundleGet<Value>"
      !write(failMsg, *) "rc =", rc, ", value_get =", value_get
      !call ESMF_Test((rc.eq.ESMF_SUCCESS .and. value_get .eq. value_set), &
                      !name, failMsg, result, ESMF_SRCLINE)
    
      !! test validate method via option string
      !character(ESMF_MAXSTR) :: validate_options
      !call ESMF_BundleValidate(bundle, validate_options, rc)
      !write(name, *) "ESMF_BundleValidate"
      !write(failMsg, *) "rc =", rc, ", validate_options =", validate_options
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      !name, failMsg, result, ESMF_SRCLINE)

      !! test print method via option string
      !character(ESMF_MAXSTR) :: print_options
      !call ESMF_BundlePrint(bundle, print_options, rc)
      !write(name, *) "ESMF_BundlePrint"
      !write(failMsg, *) "rc =", rc, ", print_options =", print_options
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      !name, failMsg, result, ESMF_SRCLINE)

      !! test internal dynamic deallocation within statically allocated 
      !!   ESMF_Bundle
      !call ESMF_BundleDestruct(bundle, rc)
      !write(name, *) "ESMF_BundleDestruct"
      !write(failMsg, *) "rc =", rc
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      !failMsg, result, ESMF_SRCLINE)
!
      ! test dynamic deallocation of ESMF_Bundle
      !   also tests destructor
      !call ESMF_BundleDestroy(bundle, rc)
      !write(name, *) "ESMF_BundleDestroy"
      !write(failMsg, *) "rc =", rc
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      !name, failMsg, result, ESMF_SRCLINE)

      !! return number of failures to environment; 0 = success (all pass)
      !! return result  ! TODO: no way to do this in F90 ?
 !! 
      end program ESMF_BundleTest
