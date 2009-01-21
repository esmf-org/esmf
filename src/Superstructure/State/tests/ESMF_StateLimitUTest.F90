! $Id: ESMF_StateLimitUTest.F90,v 1.9.2.4 2009/01/21 21:25:25 cdeluca Exp $
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
      program ESMF_StateLimitUTest

#include "ESMF_Macros.inc"

!------------------------------------------------------------------------------
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_StateLimitUTest - State Data max sizes tests
!
! !DESCRIPTION:
!
! The code in this file drives tests the size of objects able to
!  be reconciled between components.  It should have some successful
!  tests and some which fail - and right now the failure may not be
!  so graceful.
!
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod
      use ESMF_StateHelpers
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_StateLimitUTest.F90,v 1.9.2.4 2009/01/21 21:25:25 cdeluca Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure message
      character(ESMF_MAXSTR*2) :: failMsg
      character(ESMF_MAXSTR) :: validate_options = "full"
      character(ESMF_MAXSTR) :: print_options = "all"

      ! local args needed to create/construct objects
      type(ESMF_IGrid) :: igrid(2)
      type(ESMF_Field) :: sfield(20), dfield(30)
      type(ESMF_FieldBundle) :: bundle(2)
      type(ESMF_VM) :: vm
      character(ESMF_MAXSTR) :: placeholders(5)

      type(ESMF_State) :: state

      integer :: combined_rc
      integer :: nx, ny, nz, m, n, mprime, nprime



      ! -------- beginning of executable code below here -------

      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
 
      ! set up values to use below
      nx = 144
      ny = 90
      nz = 72

      ! these depend on npets - use these for testing
      m = 2
      n = 2
      mprime = 1
      nprime = 4
      
      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! create first igrid
      igrid(1) = CreateLatLonIGrid(nx, ny, nz, m, n, "MxN", rc=rc)
      write(name, *) "Creating igrid 1"
      write(failMsg, *) "Unable to create igrid 1"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! create fields
      call CreateFields(igrid(1), &
                        sfield(1), sfield(2), sfield(3), sfield(4), sfield(5), &
                        dim1=3, dim2=3, dim3=3, dim4=3, dim5=3, &
		        vrelloc1=ESMF_CELL_CELL, &
		        vrelloc2=ESMF_CELL_CELL, &
		        vrelloc3=ESMF_CELL_CELL, &
		        vrelloc4=ESMF_CELL_CELL, &
		        vrelloc5=ESMF_CELL_CELL, &
                        rc=rc)
      write(name, *) "Creating first 5 src fields"
      write(failMsg, *) "Unable to create first 5 src fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! create fields, cont.
      call CreateFields(igrid(1), &
                        sfield(6), sfield(7), sfield(8), sfield(9), sfield(10),&
                        dim1=3, dim2=3, dim3=3, dim4=3, dim5=3, &
		        vrelloc1=ESMF_CELL_CELL, &
		        vrelloc2=ESMF_CELL_CELL, &
		        vrelloc3=ESMF_CELL_CELL, &
		        vrelloc4=ESMF_CELL_CELL, &
		        vrelloc5=ESMF_CELL_CELL, &
                        rc=rc)
      write(name, *) "Creating next 5 src fields"
      write(failMsg, *) "Unable to create next 5 src fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! create fields, cont.
      call CreateFields(igrid(1), &
                        sfield(11), sfield(12), sfield(13), sfield(14), sfield(15),&
                        dim1=3, dim2=3, dim3=3, dim4=3, dim5=3, &
		        vrelloc1=ESMF_CELL_CELL, &
		        vrelloc2=ESMF_CELL_CELL, &
		        vrelloc3=ESMF_CELL_CELL, &
		        vrelloc4=ESMF_CELL_CELL, &
		        vrelloc5=ESMF_CELL_CELL, &
                        rc=rc)
      write(name, *) "Creating next 5 src fields"
      write(failMsg, *) "Unable to create next 5 src fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! create fields, cont.
      call CreateFields(igrid(1), &
                        sfield(16), sfield(17), sfield(18), sfield(19), sfield(20), &
                        dim1=3, dim2=3, dim3=3, dim4=3, dim5=3, &
		        vrelloc1=ESMF_CELL_CELL, &
		        vrelloc2=ESMF_CELL_CELL, &
		        vrelloc3=ESMF_CELL_CELL, &
		        vrelloc4=ESMF_CELL_CELL, &
		        vrelloc5=ESMF_CELL_CELL, &
                        rc=rc)
      write(name, *) "Creating last 4 src fields"
      write(failMsg, *) "Unable to create last 4 src fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! create src bundle
      call CreateFieldBundle(bundle(1), sfield(1), sfield(2), sfield(3), &
                                   sfield(4), sfield(5), rc=rc)
      write(name, *) "Creating src bundle"
      write(failMsg, *) "Unable to create src bundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! add more fields
      call AddFieldBundle(bundle(1), sfield(6), sfield(7), sfield(8), &
                                sfield(9), sfield(10), rc=rc)
      write(name, *) "Adding to src bundle"
      write(failMsg, *) "Unable to add to src bundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! add more fields
      call AddFieldBundle(bundle(1), sfield(11), sfield(12), sfield(13), &
                                sfield(14), sfield(15), rc=rc)
      write(name, *) "Adding to src bundle"
      write(failMsg, *) "Unable to add to src bundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! add more fields
      call AddFieldBundle(bundle(1), sfield(16), sfield(17), sfield(18), &
                                sfield(19), sfield(20), rc=rc)
      write(name, *) "Adding to src bundle"
      write(failMsg, *) "Unable to add to src bundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !NEX_removeUTest
      !igrid(2) = CreateLatLonIGrid(nx, ny, nz, mprime, nprime, "M'xN'", 1, 1, rc)
      igrid(2) = CreateLatLonIGrid(nx, ny, nz, mprime, nprime, "M'xN'", rc=rc)
      write(name, *) "Creating igrid 2"
      write(failMsg, *) "Unable to create igrid 2"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! create fields
      call CreateFields(igrid(2), &
                        dfield(1), dfield(2), dfield(3), dfield(4), dfield(5),&
                        dim1=3, dim2=3, dim3=3, dim4=3, dim5=3, &
		        vrelloc1=ESMF_CELL_CELL, &
		        vrelloc2=ESMF_CELL_CELL, &
		        vrelloc3=ESMF_CELL_CELL, &
		        vrelloc4=ESMF_CELL_CELL, &
		        vrelloc5=ESMF_CELL_CELL, &
                        rc=rc)
      write(name, *) "Creating 1st set of 5 dst fields"
      write(failMsg, *) "Unable to create 1st set 5 dst fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! create fields, cont.
      call CreateFields(igrid(2), &
                        dfield(6), dfield(7), dfield(8), dfield(9), dfield(10),&
                        dim1=3, dim2=3, dim3=3, dim4=3, dim5=3, &
		        vrelloc1=ESMF_CELL_CELL, &
		        vrelloc2=ESMF_CELL_CELL, &
		        vrelloc3=ESMF_CELL_CELL, &
		        vrelloc4=ESMF_CELL_CELL, &
		        vrelloc5=ESMF_CELL_CELL, &
                        rc=rc)
      write(name, *) "Creating 2nd set of 5 dst fields"
      write(failMsg, *) "Unable to create 2nd set of 5 dst fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! create fields, cont.
      call CreateFields(igrid(2), &
                        dfield(11), dfield(12), dfield(13), dfield(14), dfield(15),&
                        dim1=3, dim2=3, dim3=3, dim4=3, dim5=3, &
		        vrelloc1=ESMF_CELL_CELL, &
		        vrelloc2=ESMF_CELL_CELL, &
		        vrelloc3=ESMF_CELL_CELL, &
		        vrelloc4=ESMF_CELL_CELL, &
		        vrelloc5=ESMF_CELL_CELL, &
                        rc=rc)
      write(name, *) "Creating 3rd set of 5 dst fields"
      write(failMsg, *) "Unable to create 3rd set of 5 dst fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! create fields, cont.
      call CreateFields(igrid(2), &
                        dfield(16), dfield(17), dfield(18), dfield(19), dfield(20),&
                        dim1=3, dim2=3, dim3=3, dim4=3, dim5=3, &
		        vrelloc1=ESMF_CELL_CELL, &
		        vrelloc2=ESMF_CELL_CELL, &
		        vrelloc3=ESMF_CELL_CELL, &
		        vrelloc4=ESMF_CELL_CELL, &
		        vrelloc5=ESMF_CELL_CELL, &
                        rc=rc)
      write(name, *) "Creating 4th set of 5 dst fields"
      write(failMsg, *) "Unable to create 4th set of 5 dst fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! create fields, cont.
      call CreateFields(igrid(2), &
                        dfield(21), dfield(22), dfield(23), dfield(24), dfield(25),&
                        dim1=3, dim2=3, dim3=3, dim4=3, dim5=3, &
		        vrelloc1=ESMF_CELL_CELL, &
		        vrelloc2=ESMF_CELL_CELL, &
		        vrelloc3=ESMF_CELL_CELL, &
		        vrelloc4=ESMF_CELL_CELL, &
		        vrelloc5=ESMF_CELL_CELL, &
                        rc=rc)
      write(name, *) "Creating 5th set of 5 dst fields"
      write(failMsg, *) "Unable to create 5th set of 5 dst fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! create fields, cont.
      call CreateFields(igrid(2), &
                        dfield(26), dfield(27), dfield(28), dfield(29), dfield(30),&
                        dim1=3, dim2=3, dim3=3, dim4=3, dim5=3, &
		        vrelloc1=ESMF_CELL_CELL, &
		        vrelloc2=ESMF_CELL_CELL, &
		        vrelloc3=ESMF_CELL_CELL, &
		        vrelloc4=ESMF_CELL_CELL, &
		        vrelloc5=ESMF_CELL_CELL, &
                        rc=rc)
      write(name, *) "Creating 6th set of 5 dst fields"
      write(failMsg, *) "Unable to create 6th set of 5 dst fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! create dst bundle
      call CreateFieldBundle(bundle(2), dfield(1), dfield(2), dfield(3), &
                                   dfield(4), dfield(5), rc=rc)
      write(name, *) "Creating dst bundle"
      write(failMsg, *) "Unable to create dst bundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! add more fields
      call AddFieldBundle(bundle(2), dfield(6), dfield(7), dfield(8), &
                                dfield(9), dfield(10), rc=rc)
      write(name, *) "Adding to dst bundle"
      write(failMsg, *) "Unable to add to dst bundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! add more fields
      call AddFieldBundle(bundle(2), dfield(11), dfield(12), dfield(13), &
                                dfield(14), dfield(15), rc=rc)
      write(name, *) "Adding to dst bundle"
      write(failMsg, *) "Unable to add to dst bundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! add more fields
      call AddFieldBundle(bundle(2), dfield(16), dfield(17), dfield(18), &
                                dfield(19), dfield(20), rc=rc)
      write(name, *) "Adding to dst bundle"
      write(failMsg, *) "Unable to add to dst bundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! add more fields
      call AddFieldBundle(bundle(2), dfield(21), dfield(22), dfield(23), &
                                dfield(24), dfield(25), rc=rc)
      write(name, *) "Adding to dst bundle"
      write(failMsg, *) "Unable to add to dst bundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! add more fields
      call AddFieldBundle(bundle(2), dfield(26), dfield(27), dfield(28), &
                                dfield(29), dfield(30), rc=rc)
      write(name, *) "Adding to dst bundle"
      write(failMsg, *) "Unable to add to dst bundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! get VM for Reconcile call below
      call ESMF_VMGetGlobal(vm, rc=rc)
      write(name, *) "Getting global VM"
      write(failMsg, *) "Unable to get global VM"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! create an empty state and start there

      state = ESMF_StateCreate("reconcile size test", rc=rc)
      write(name, *) "Creating an empty State"
      write(failMsg, *) "Unable to create an empty State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! reconcile
      call ESMF_StateReconcile(state, vm=vm, rc=rc)
      write(name, *) "Calling StateReconcile"
      write(failMsg, *) "Error return from StateReconcile"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! now add stuff
      
      call ESMF_StateAdd(state, &
                                  1, bundlelist=bundle(1:1), &
                                  rc=rc)
      write(name, *) "Adding FieldBundle(s) to State"
      write(failMsg, *) "Unable to add FieldBundle(s) to State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! reconcile
      call ESMF_StateReconcile(state, vm=vm, rc=rc)
      write(name, *) "Calling StateReconcile"
      write(failMsg, *) "Error return from StateReconcile"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! and more stuff

      call ESMF_StateAdd(state, &
                              5, fieldlist=sfield(1:5), &
                              rc=rc)
      write(name, *) "Adding Field(s) to State"
      write(failMsg, *) "Unable to add Field(s) to State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! reconcile
      call ESMF_StateReconcile(state, vm=vm, rc=rc)
      write(name, *) "Calling StateReconcile"
      write(failMsg, *) "Error return from StateReconcile"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! and more stuff
      placeholders(1) = "Temperature Field"
      placeholders(2) = "Density Field"
      placeholders(3) = "U Field"
      placeholders(4) = "V Field"
      placeholders(5) = "Energy Field"

      call ESMF_StateAdd(state, &
                                 5, namelist=placeholders(1:5), &
                                 rc=rc)
      write(name, *) "Adding Placeholder Name(s) to State"
      write(failMsg, *) "Unable to add Placeholder Name(s) to State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! reconcile
      call ESMF_StateReconcile(state, vm=vm, rc=rc)
      write(name, *) "Calling StateReconcile"
      write(failMsg, *) "Error return from StateReconcile"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! and still more stuff

      call ESMF_StateAdd(state, &
                              15, fieldlist=sfield(1:15), &
                              rc=rc)
      write(name, *) "Adding Field(s) to State"
      write(failMsg, *) "Unable to add Field(s) to State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! reconcile
      call ESMF_StateReconcile(state, vm=vm, rc=rc)
      write(name, *) "Calling StateReconcile"
      write(failMsg, *) "Error return from StateReconcile"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! now add too much stuff - this should provoke an error
      ! at reconcile time.
      
      call ESMF_StateAdd(state, &
                                  1, bundlelist=bundle(2:2), &
                                  rc=rc)
      write(name, *) "Adding FieldBundle(s) to State"
      write(failMsg, *) "Unable to add FieldBundle(s) to State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! reconcile
      call ESMF_StateReconcile(state, vm=vm, rc=rc)
      write(name, *) "Calling StateReconcile expecting failure"
      write(failMsg, *) "No Error return from StateReconcile"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !NEX_removeUTest
      call ESMF_StateDestroy(state, rc=rc)
      write(name, *) "Calling StateDestroy"
      write(failMsg, *) "Error return from StateDestroy"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------


      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! cleanup
      call FieldBundleCleanup(bundle(1), bundle(2), rc=rc)
      write(name, *) "Deleting bundles at cleanup time"
      write(failMsg, *) "Deleting bundles at cleanup time"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! cleanup
      combined_rc = ESMF_SUCCESS
      call FieldCleanup(sfield(1), sfield(2), sfield(3), sfield(4), &
                        sfield(5), .FALSE., rc=rc)
      if (rc .ne. ESMF_SUCCESS) combined_rc = rc
      call FieldCleanup(sfield(6), sfield(7), sfield(8), sfield(9), &
                        sfield(10), rc=rc)
      if (rc .ne. ESMF_SUCCESS) combined_rc = rc
      call FieldCleanup(dfield(1), dfield(2), dfield(3), dfield(4), &
                        dfield(5), .FALSE., rc=rc)
      if (rc .ne. ESMF_SUCCESS) combined_rc = rc
      call FieldCleanup(dfield(6), dfield(7), dfield(8), dfield(9), &
                        dfield(10), rc=rc)
      if (rc .ne. ESMF_SUCCESS) combined_rc = rc
      write(name, *) "Deleting fields at cleanup time"
      write(failMsg, *) "Deleting fields at cleanup time"
      call ESMF_Test((combined_rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


10    continue

      call ESMF_TestEnd(result, ESMF_SRCLINE)
  
      ! -------- end of unit test code ------------------------

      end program ESMF_StateLimitUTest



