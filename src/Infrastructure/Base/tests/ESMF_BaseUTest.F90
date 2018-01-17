! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_BaseUTest

!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_BaseUTest - One line general statement about this test
!
! !DESCRIPTION:
!
! The code in this file drives F90 Base unit tests.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF         ! the ESMF Framework
      use ESMF_BaseMod, only: ESMF_BaseDeserializeIDVMId
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id$'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure messages
      character(ESMF_MAXSTR*2) :: failMsg

      ! local variables needed to pass into function/subroutine calls
      !type(ESMF_BaseConfig) :: config_set
      !type(ESMF_BaseConfig) :: config_get

      ! instantiate a Base 
      type(ESMF_Base) :: base

#ifdef ESMF_TESTEXHAUSTIVE
      character(ESMF_MAXSTR) :: print_options
      character(ESMF_MAXSTR) :: validate_options
      character(ESMF_MAXSTR) :: name_set, name_get
      ! instantiate a Base 
      type(ESMF_Base) :: base1, base2
      type(ESMF_AttReconcileFlag) :: attreconflag
      integer :: id, id_inq
      type(ESMF_VMId) :: vmid, vmid_inq, vmid_new, vmid_new2
      character, allocatable   :: buffer(:)
      integer :: buff_size
      integer :: offset1, offset2, offset3
      logical :: tf
#endif

!-------------------------------------------------------------------------------
!  The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!  always run. When the environment variable, EXHAUSTIVE, is set to ON then
!  the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!  to OFF, then only the sanity unit tests.
!  Special strings (Non-exhaustive and exhaustive) have been
!  added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------

      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


      !NEX_UTest
      ! test creation of base objects
      call ESMF_BaseCreate(base, "Base", "test object", 0, rc=rc)
      write(name, *) "ESMF_BaseCreate"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

                      
      !NEX_UTest
      ! destroy base object
      call ESMF_BaseDestroy(base, rc=rc)
      write(name, *) "ESMF_Destroy"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      

#ifdef ESMF_TESTEXHAUSTIVE

                      
      !EX_UTest
      ! destroy base object
      call ESMF_BaseDestroy(base, rc=rc)
      write(name, *) "Destroy a destroyed Base"
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)
      
                      
      !EX_UTest
      ! destroy base object
      call ESMF_BaseDestroy(base1, rc=rc)
      write(name, *) "Destroy a non-created Base"
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)


      !EX_UTest
      ! test print method of deleted base via option string
      print_options = "brief"
      call ESMF_BasePrint(base, print_options, rc=rc)
      write(name, *) "ESMF_BasePrint of deleted Base"
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)


      !EX_UTest
      ! test print method of non-created base via option string
      print_options = "brief"
      call ESMF_BasePrint(base1, print_options, rc=rc)
      write(name, *) "ESMF_BasePrint of non-created Base"
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! test setting of ESMF_Base members values of uncreated Base
      ! Note That this will recreate the base
      name_set = "fred"
      call ESMF_SetName(base1, name_set, "Base", rc=rc)
      write(name, *) "ESMF_SetName of non-created Base"
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      !EX_UTest
      ! test setting of ESMF_Base members values of deleted Base
      ! Note That this will recreate the base
      name_set = "fred"
      call ESMF_SetName(base, name_set, "Base", rc=rc)
      write(name, *) "ESMF_SetName of deleted Base"
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! destroy base objects created by ESMF_SetName
      call ESMF_BaseDestroy(base, rc=rc)
      call ESMF_BaseDestroy(base1, rc=rc)


      
      !EX_UTest
      ! test creation of base objects with slash in its name
      call ESMF_BaseCreate(base, "Base", "test/object", 0, rc=rc)
      write(name, *) "ESMF_BaseCreate with slash in name"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc /= ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      !EX_UTest
      ! test creation of base objects
      call ESMF_BaseCreate(base, "Base", "test object", 0, rc=rc)
      write(name, *) "ESMF_BaseCreate"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      !EX_UTest
      ! test setting of ESMF_Base members values with slash in its name
      name_set = "fred/jones"
      call ESMF_SetName(base, name_set, "Base", rc=rc)
      write(name, *) "ESMF_SetName with slash in its name"
      write(failMsg, *) "rc =", rc, ", name =", trim(name_set)
      call ESMF_Test((rc /= ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! test setting of ESMF_Base members values
      name_set = "fred"
      call ESMF_SetName(base, name_set, "Base", rc=rc)
      write(name, *) "ESMF_SetName"
      write(failMsg, *) "rc =", rc, ", name =", trim(name_set)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! test getting of ESMF_Base members values,
      !   compare to values set previously
      call ESMF_GetName(base, name_get, rc=rc)
      write(name, *) "ESMF_GetName"
      write(failMsg, *) "rc =", rc, ", name =", name_get
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. name_get .eq. name_set), &
                      name, failMsg, result, ESMF_SRCLINE)
    
      !EX_UTest
      ! test validate method via option string
      validate_options = ''
      call ESMF_BaseValidate(base, validate_options, rc=rc)
      write(name, *) "ESMF_BaseValidate"
      write(failMsg, *) "rc =",rc,", validate_options =", trim(validate_options)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! test getting of ESMF_Base members values,
      id = -1
      call ESMF_BaseGetId (base, id, rc=rc)
      write(name, *) "ESMF_BaseGetID"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, 'id =', id

      !EX_UTest
      ! test getting of ESMF_Base members values,
      call ESMF_BaseGetVMId (base, vmid, rc=rc)
      write(name, *) "ESMF_BaseGetVMId of original Base"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! test setting of ESMF_Base members values,
      call ESMF_VMIdCreate (vmid_new, rc=rc)
      call c_ESMCI_VMIdSet (vmid_new, 1234, achar (123), rc)
      call ESMF_BaseSetVMId (base, vmid_new, rc=rc)
      write(name, *) "ESMF_BaseSetVMId a new VMId into the Base"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_BasePrint (base)

      !EX_UTest
      ! test getting of ESMF_Base members values,
      call ESMF_BaseGetVMId (base, vmid_new2, rc=rc)
      write(name, *) "ESMF_BaseGetVMId new VMid"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! test resetting of ESMF_Base members values,
      call ESMF_BaseSetVMId (base, vmid, rc=rc)
      write(name, *) "ESMF_BaseSetVMId reset the original VMId into the Base"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_BasePrint (base)

      !EX_UTest
      ! test print method via option string
      print_options = "brief"
      call ESMF_BasePrint(base, print_options, rc=rc)
      write(name, *) "ESMF_BasePrint brief"
      write(failMsg, *) "rc =", rc, ", print_options =", trim(print_options)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! test print method via option string
      print_options = "debug"
      call ESMF_BasePrint(base, print_options, rc=rc)
      write(name, *) "ESMF_BasePrint debug"
      write(failMsg, *) "rc =", rc, ", print_options =", trim(print_options)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! BEGIN tests of certain INTERNAL methods.  They are subject
      ! to change and are NOT part of the ESMF user API.

      !EX_UTest
      ! test the serialize inquire-only option
      ! WARNING: This is testing an INTERNAL method.  It is NOT
      ! part of the supported ESMF user API!
      attreconflag = ESMF_ATTRECONCILE_OFF
      buff_size = 1
      allocate (buffer(buff_size))
      offset1 = 0
      call ESMF_BaseSerialize (base, buffer, offset1, &
          attreconflag, ESMF_INQUIREONLY, rc=rc)
      write(name, *) "ESMF_BaseSerialize - inquire only option"
      write(failMsg, *) "rc =", rc, ", offset =", offset1
      call ESMF_Test((rc == ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, '  offset returned =', offset1, ' bytes'
      deallocate (buffer)

      !EX_UTest
      ! test doing a serialize for real.
      ! WARNING: This is testing an INTERNAL method.  It is NOT
      ! part of the supported ESMF user API!
      buff_size = offset1 ! from previous inquiry
      allocate (buffer(buff_size))
      buffer = char (z'42')
      offset2 = 0
      call ESMF_BaseSerialize (base, buffer, offset2, &
          attreconflag, ESMF_NOINQUIRE, rc=rc)
      write(name, *) "ESMF_BaseSerialize - perform serialization"
      write(failMsg, *) "rc =", rc, ", offset =", offset2
      call ESMF_Test((rc == ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, '  offset returned =', offset2, ' bytes'

      !EX_UTest
      ! Compare inquired size with actual size.  Note that the two
      ! sizes will not be equal because the inquire option currently
      ! overestimates the space needed - which is ok.
      ! WARNING: This is testing an INTERNAL method.  It is NOT
      ! part of the supported ESMF user API!
      write(name, *) "Compare calculated buffer size with actual size"
      write(failMsg, *) 'actual offset', offset2, ' > inquire offset', offset1
      call ESMF_Test(offset1 >= offset2, &
                      name, failMsg, result, ESMF_SRCLINE)

      ! print '(25z3)', iachar (buffer)

      !EX_UTest
      ! test doing a deserialize for real.
      ! WARNING: This is testing an INTERNAL method.  It is NOT
      ! part of the supported ESMF user API!
      offset3 = 0
      base2 = ESMF_BaseDeserialize (buffer, offset3, &
          attreconflag, rc=rc)
      write(name, *) "ESMF_BaseDeserialize - perform deserialization"
      write(failMsg, *) "rc =", rc, ", offset =", offset3
      call ESMF_Test((rc == ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! Compare calculated serialed offset with actual deserialed offset.
      ! WARNING: This is testing an INTERNAL method.  It is NOT
      ! part of the supported ESMF user API!
      write(name, *) "Compare serialize/deserialize offsets"
      write(failMsg, *) 'offset', offset2, ' /=', offset3
      call ESMF_Test(offset2 == offset3, &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! Create VMId for inquiry
      ! WARNING: This is testing an INTERNAL method.  It is NOT
      ! part of the supported ESMF user API!
      call ESMF_VMIdCreate (vmid_inq, rc=rc)
      write(name, *) "Creating VMId for inquiry"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc == ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! Test ID/VMId inquiry in a serialized buffer
      ! WARNING: This is testing an INTERNAL method.  It is NOT
      ! part of the supported ESMF user API!
      offset3 = 0
      call ESMF_BaseDeserializeIDVMId (buffer, offset3, &
          id_inq, vmid_inq, rc=rc)
      write(name, *) "ESMF_BaseDeserializeID/VMId - perform deserialization inquiry"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc == ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! Compare original vs inquired ids.
      ! WARNING: This is testing an INTERNAL method.  It is NOT
      ! part of the supported ESMF user API!
      write(name, *) "Compare original vs inquired ids"
      write(failMsg, *) 'id', id, '/=', id_inq
      call ESMF_Test(id == id_inq, &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! Compare original vs inquired VMId inquiry.
      ! WARNING: This is testing an INTERNAL method.  It is NOT
      ! part of the supported ESMF user API!
      write(name, *) "Compare original vs inquired VMId inquiry"
      write(failMsg, *) 'VMIds do not compare'
#if 0
      print *, 'original vmid:'
      call ESMF_VMIdPrint (vmid)
      print *, 'deserialized/inquiry vmid:'
      call ESMF_VMIdPrint (vmid_inq)
#endif
      tf = ESMF_VMIdCompare (vmid, vmid_inq, rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! Compare original vs inquired VMIds.
      ! WARNING: This is testing an INTERNAL method.  It is NOT
      ! part of the supported ESMF user API!
      write(name, *) "Compare original vs inquired VMIds"
      write(failMsg, *) 'VMIds do not compare'
      rc = merge (ESMF_SUCCESS, ESMF_FAILURE, tf)
      call ESMF_Test((rc == ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_VMIdDestroy (vmid_new, rc=rc)
      call ESMF_VMIdDestroy (vmid_inq, rc=rc)

      ! END of tests of INTERNAL methods.

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?

      !EX_UTest
      ! destroy base object
      call ESMF_BaseDestroy(base, rc=rc)
      write(name, *) "ESMF_Destroy"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      deallocate (buffer)

#endif

      ! This calls finalize before returning, so it must be the last
      ! ESMF-related thing the test does.
      call ESMF_TestEnd(ESMF_SRCLINE)
  
      end program ESMF_BaseUTest
