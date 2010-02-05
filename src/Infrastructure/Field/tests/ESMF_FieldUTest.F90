! $Id: ESMF_FieldUTest.F90,v 1.146.2.1 2010/02/05 19:56:15 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_FieldUTest

!------------------------------------------------------------------------------
 
#include "ESMF.h"

!==============================================================================
!BOPI
! !PROGRAM: ESMF_FieldTest - One line general statement about this test
!
! !DESCRIPTION:
!
! The code in this file drives F90 Field unit tests.
! The companion file ESMF\_Field.F90 contains the definitions for the
! Field methods.
!EOPI
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod
      use ESMF_FieldGetMod

      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_FieldUTest.F90,v 1.146.2.1 2010/02/05 19:56:15 svasquez Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc = 1

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

!     !LOCAL VARIABLES:
      type(ESMF_VM) :: vm
      real(ESMF_KIND_R8) :: minCoord(2)
      !type(ESMF_RelLoc) :: rl
      !type(ESMF_Mask) :: mask
      type(ESMF_Field) :: f1
      logical :: isCommitted


#ifdef ESMF_TESTEXHAUSTIVE
      integer :: cu(2), cl(2), cc(2), localrc
      integer :: ldecount
      type(ESMF_IOSpec) :: ios
      type(ESMF_Grid) :: grid3
      type(ESMF_ArraySpec)            :: arrayspec
      type(ESMF_LocStream)            :: locstream
      type(ESMF_StaggerLoc)                       :: staggerloc8
      type(ESMF_ArraySpec)                        :: arrayspec8
      real(ESMF_KIND_R8), dimension(:,:,:), allocatable :: fptr
      real(ESMF_KIND_R4), dimension(:,:), pointer :: lsfptrR4Out
      type(ESMF_Grid) :: grid, grid2
      real(ESMF_KIND_R8), dimension(:), pointer :: lsfptr,lsfptrOut
      type(ESMF_Field) :: f2, f3, f4, f5, f6, fls, fS
      integer :: ulb(1), uub(1)
      character (len = 20) :: gname, gname3
      character (len = 20) :: fname, fname1, fname2
      logical :: correct

#endif
!-------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then 
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------- 
      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)


      !------------------------------------------------------------------------
      ! several calls to field need a valid grid.  these will be used in
      ! the grid create calls.
      call ESMF_VMGetGlobal(vm, rc=rc)
      minCoord(:) = (/ 0.0, 0.0 /)


      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Test Requirement FLD1.1.3 Creation without data 
      ! Fields may be created as in FLD1.1.1 without allocating data or 
      ! specifying an associated data array. In this case specifying the 
      ! grid parameters and data array dimensions may be deferred until 
      ! data is attached.
      f1 = ESMF_FieldCreateEmpty(rc=rc) 
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a Field with no data"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !NEX_UTest_Multi_Proc_Only
      ! Test isCommitted in FieldGet api
      call ESMF_FieldGet(f1, isCommitted=isCommitted, rc=rc)
      write(failMsg, *) "isCommitted or rc wrong"
      write(name, *) "Query isCommitted flag from an empty Field"
      call ESMF_Test(((.not.isCommitted).and.(rc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Verifying that a Field with no data can be destroyed
      call ESMF_FieldDestroy(f1, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroying a Field with no data Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_TESTEXHAUSTIVE

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Testing creating a field on a locstream
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Creating a Field on a LocStream from an ArraySpec"
      ! initialize 
      rc=ESMF_SUCCESS
      correct=.true.
      
      ! Create locstream
      locstream=ESMF_LocStreamCreate(localCount=10, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE   

      ! Set Array Spec
      call ESMF_ArraySpecSet(arrayspec, rank=2, typekind=ESMF_TYPEKIND_R4, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE         

      ! Create Field
      fls=ESMF_FieldCreate(locstream, arrayspec, &
            ungriddedLBound=(/1/), ungriddedUBound=(/4/), rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE         

      call ESMF_FieldGet(fls, isCommitted=isCommitted, rc=rc)

      ! check bounds
      call ESMF_FieldGet(fls, localDE=0, computationalCount=cc, &
             computationalLBound=cl, computationalUBound=cu, &
             farrayPtr=lsfptrR4Out, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE         

      ! Check output
      if (cl(1) .ne. 1) correct=.false.
      if (cu(1) .ne. 10) correct=.false.
      if (cc(1) .ne. 10) correct=.false.
      if (cl(2) .ne. 1) correct=.false.
      if (cu(2) .ne. 4) correct=.false.
      if (cc(2) .ne. 4) correct=.false.

      ! Destroy Field
      call ESMF_FieldDestroy(fls, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE   

      ! Destroy LocStream 
      call ESMF_LocStreamDestroy(locstream, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE   

      call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Test isCommitted in FieldGet api
      write(failMsg, *) "isCommitted or rc wrong"
      write(name, *) "Query isCommitted flag from a commmitted Field on locstream"
      call ESMF_Test((isCommitted.and.(rc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Testing creating a field on a locstream
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Creating a Field on a LocStream from an farray"
      ! initialize 
      rc=ESMF_SUCCESS
      correct=.true.
      
      ! Create locstream
      locstream=ESMF_LocStreamCreate(localCount=10, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE   

      ! Allocate array
      allocate (lsfptr(10))

      ! Create Field
      fls=ESMF_FieldCreate(locstream, farray=lsfptr, indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE         

      ! check bounds
      call ESMF_FieldGet(fls, localDE=0, computationalCount=cc(1:1), &
             computationalLBound=cl(1:1), computationalUBound=cu(1:1), &
             farrayPtr=lsfptrOut, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE         

      ! Check output
      if (cl(1) .ne. 1) correct=.false.
      if (cu(1) .ne. 10) correct=.false.
      if (cc(1) .ne. 10) correct=.false.

      ! Destroy Field
      call ESMF_FieldDestroy(fls, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE   

      ! Destroy LocStream 
      call ESMF_LocStreamDestroy(locstream, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE   

      ! deallocate array
      deallocate (lsfptr)

      call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verifying that a Field that has not been created doesn't
      ! crash when it is destroyed 
      call ESMF_FieldDestroy(f2, rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Destroying a Field not created"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_remove_UTest_Multi_Proc_Only
      ! Get a name from a deleted created Field
!      call ESMF_FieldGet(f1, name=fname, rc=rc)
!      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED "
!      write(name, *) "Getting name of deleted Field"
!      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Get a name from a non-created Field
      call ESMF_FieldGet(f2, name=fname, rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Getting name of uncreated Field"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      f1 = ESMF_FieldCreateEmpty(rc=rc) 
      write(failMsg, *) ""
      write(name, *) "Creating a Field with no data Test Req. FLD1.1.3"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!      !------------------------------------------------------------------------
!      !E-X_UTest_Multi_Proc_Only
!      ! Verifying that an initialized Field can be printed
!      call ESMF_FieldPrint(f1, rc=rc)
!      write(failMsg, *) ""
!      write(name, *) "Printing an initialized Field with no data Test"
!      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verifying that the Field name can be queried from a no data Field
      call ESMF_FieldGet(f1, name=fname, rc=rc)
      write(failMsg, *) "default name not generated"
      write(name, *) "Getting name of Field with no data Test"
      call ESMF_Test((fname.ne.""), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! default names unique
      f2 = ESMF_FieldCreateEmpty(rc=rc)
      call ESMF_FieldGet(f1, name=fname1, rc=rc)
      call ESMF_FieldGet(f2, name=fname2, rc=rc)
      call ESMF_FieldPrint(f1)
      call ESMF_FieldPrint(f2)
      write(failMsg, *) "default name not unique"
      write(name, *) "Verifying uniqueness of fields created default name"
      call ESMF_Test((fname1.ne.fname2), name, failMsg, result, ESMF_SRCLINE)

      fS = f2 ! assignment will lead to shallow copy, use later down
      
      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verifying that a Field with no data can be destroyed
      call ESMF_FieldDestroy(f2, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying a Field with no data Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Test Requirement  
      ! Fields may be deleted.
      call ESMF_FieldDestroy(f1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying initialized Field Test Req. FLD1.4"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verifying that printing an uninitialized Field is handled properly.
      call ESMF_FieldPrint(f6, rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Printing an uninitialized Field Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! verify that querying the name of a destroyed Field is handled properly.
      call ESMF_FieldGet(f1, name=fname, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting name of a destroyed Field Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verifying that printing a destroyed Field is handled properly.
      call ESMF_FieldPrint(f1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing destroyed Field Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verifying that a Field can be created with a name
      f2 = ESMF_FieldCreateEmpty("pressure", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating Field with name Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verifying that the Field name can be queried.
      Call ESMF_FieldGet(f2, name=fname, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting a Field name Test"
      call ESMF_Test((fname.eq."pressure"), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f2)
      call ESMF_FieldDestroy(f2)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verifying that recreating a Field is allowed.
      f2 = ESMF_FieldCreateEmpty("temperature", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Recreate a created Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f2)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verifying that a Field can be created after it has been destroyed
      call ESMF_FieldDestroy(f2)
      f2 = ESMF_FieldCreateEmpty("precipitation", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Recreate a destroyed Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f2)
      call ESMF_FieldDestroy(f2)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verifying that a Grid can be created
      grid = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/10,20/), &
        regDecomp=(/2,2/), gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
        name="landgrid", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Grid to use in Field Tests"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     
      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Test requirement Default name attribute 
      ! The only default attribute of a field will be a name. A unique 
      ! name will be generated if not supplied by the user.
      ! Test Requirement Query name
      ! A field shall be able to easily return its name. 
      ! If the user does not provide a field name one will be created. 
      ! Field names must be unique within an address 
      ! space and it shall be possible to check this.
      ! Bug 705087 "Default Field names not unique"
      f1 = ESMF_FieldCreateEmpty(rc=rc)
      f2 = ESMF_FieldCreateEmpty(rc=rc)
      call ESMF_FieldGet(f1, name=fname1, rc=rc)
      call ESMF_FieldGet(f2, name=fname2, rc=rc)
      write(failMsg, *) "Field names not unique"
      write(name, *) "Unique default Field names Test, FLD1.5.1 & 1.7.1"
      call ESMF_Test((fname1.ne.fname2), name, failMsg, result, ESMF_SRCLINE)
      !print *, "Field (f1) name = ", trim(fname1)
      print *, "Field (f2) name = ", trim(fname2)
      call ESMF_FieldPrint(f1)
      call ESMF_FieldPrint(f2)
      call ESMF_FieldDestroy(f1)
      call ESMF_FieldDestroy(f2)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      f4 = ESMF_FieldCreateEmpty(rc=rc) 
      write(failMsg, *) ""
      write(name, *) "Creating a Field with no data"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!      !------------------------------------------------------------------------
!      !EremoveX_UTest_Multi_Proc_Only
!      ! Setting a Grid associated with Field
!      call ESMF_FieldSet(f4, grid, rc=rc) 
!      write(failMsg, *) "Did not return ESMF_SUCCESS"
!      write(name, *) "Setting a Grid associated with Field Test"
!      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      call ESMF_FieldValidate(f4, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Testing to see if Field is Valid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verifying that a Field with a Grid and Array can be destroyed
      call ESMF_FieldDestroy(f4, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying a Field with a Grid and Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Fields may be created by specifying attributes, a grid, data array 
      ! dimensions and descriptors, optional masks (e.g. for active cells), 
      ! and an optional I/O specification. In this case a field will 
      ! allocate its own data. The grid passed into the argument list 
      ! is referenced and not copied.
      call ESMF_ArraySpecSet(arrayspec, rank=2, typekind=ESMF_TYPEKIND_R4, rc=rc)
      write(name, *) "Creating an ArraySpec Test "
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      f2 = ESMF_FieldCreate(grid, arrayspec=arrayspec, indexflag=ESMF_INDEX_DELOCAL, &
                                          staggerloc=ESMF_STAGGERLOC_CENTER, &
                                          name="rh", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a Field with a Grid and ArraySpec Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Test isCommitted in FieldGet api
      call ESMF_FieldGet(f2, isCommitted=isCommitted, rc=rc)
      write(failMsg, *) "isCommitted or rc wrong"
      write(name, *) "Query isCommitted flag from a commmitted Field"
      call ESMF_Test((isCommitted.and.(rc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verifying that destroying a destroyed  Field is handled properly.
      call ESMF_FieldDestroy(f2, rc=rc)  ! should succeed, f2 exists
      call ESMF_FieldDestroy(f2, rc=rc)  ! should fail
      write(failMsg, *) ""
      write(name, *) "Destroying a destroyed Field Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verifying that destroying a shallow copy of destroyed Field is o.k.
      call ESMF_FieldDestroy(fS, rc=rc)  ! should be o.k.
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroying a shallow copy of destroyed Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! verify we can create a 3d data on a 2d grid
      call ESMF_ArraySpecSet(arrayspec, rank=3, typekind=ESMF_TYPEKIND_R4, rc=rc)
      f2 = ESMF_FieldCreate(grid, arrayspec=arrayspec, indexflag=ESMF_INDEX_DELOCAL, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            ungriddedLBound=(/1/), ungriddedUBound=(/10/), &
            name="rh", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a Field with a 2d Grid and 3d ArraySpec"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldDestroy(f2)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Creation with external data 
      ! Fields may be created as in FLD1.1.1 with a data array passed into 
      ! the argument list. The data array is referenced and not copied.
      ! Verifying that a Field can be created with a Grid
      call ESMF_ArraySpecSet(arrayspec, rank=2, typekind=ESMF_TYPEKIND_R4, rc=rc)
      f3 = ESMF_FieldCreate(grid, arrayspec=arrayspec, indexflag=ESMF_INDEX_DELOCAL, &
                        staggerloc=ESMF_STAGGERLOC_CENTER, &
                        name="Field 0", iospec=ios, rc=rc)

      write(failMsg, *) ""
      write(name, *) "Creating a Field with a Grid and Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      call ESMF_FieldValidate(f3, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Testing to see if Field is Valid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldDestroy(f3)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verifying that destroying the Field did not destroy the Grid
       call ESMF_GridValidate(grid, rc=rc)
       write(failMsg, *) ""
       write(name, *) "Using a Grid after the Field is destroyed"
       call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verifying that a Field cannot be created with an uninitialized Grid 
      ! and Array.  f6 is *not* created here and should be invalid.
      grid2 = ESMF_GridCreateEmpty(rc=rc)
      call ESMF_GridDestroy(grid2, rc=rc)
      f6 = ESMF_FieldCreate(grid2, arrayspec=arrayspec, indexflag=ESMF_INDEX_DELOCAL, &
                        staggerloc=ESMF_STAGGERLOC_CENTER, &
                        name="Field 0", iospec=ios, rc=rc)

      write(failMsg, *) ""
      write(name, *) "Creating a Field with an uninitialized Grid and Array"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verify that a Grid cannot be gotten from a Field created with no data
      f5 = ESMF_FieldCreateEmpty(rc=rc)
      call ESMF_FieldGet(f5, grid=grid3, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting a Grid from a Field created with no data Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldDestroy(f5, rc=rc)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Return grid 
      ! A field shall be able to return a reference to its grid.
      ! f3 gets created here and used thru the rest of the tests.
      gname="oceangrid"
      call ESMF_ArraySpecSet(arrayspec, rank=2, typekind=ESMF_TYPEKIND_R4, rc=rc)
      f3 = ESMF_FieldCreate(grid, arrayspec=arrayspec, indexflag=ESMF_INDEX_DELOCAL, &
                        staggerloc=ESMF_STAGGERLOC_CENTER, &
                        name="Field 0", iospec=ios, rc=rc)
      call ESMF_FieldGet(f3, grid=grid3, localDeCount=ldecount, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting a Grid from a Field created with no data Test"
      call ESMF_GridGet(grid, name=gname, rc=rc)
      call ESMF_GridGet(grid3, name=gname3, rc=rc)
      print *, "Grid (grid3) name = ", trim(gname3)
      call ESMF_Test((gname.eq.gname3), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only 
      ! ESMF_ArraySpecPrint test ArraySpecPrint public interface
      call ESMF_ArraySpecSet(arrayspec8, rank=2, typekind=ESMF_TYPEKIND_R8, rc=rc)
      call ESMF_ArraySpecPrint(arrayspec8, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Test ESMF_ArraySpecPrint public interface"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only 
      ! ESMF_StaggerLocPrint test StaggerLoc public interface
      call ESMF_StaggerLocPrint(staggerloc8, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Test ESMF_StaggerLocPrint public interface"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      call ESMF_FieldDestroy(f3)

      !------------------------------------------------------------------------
      ! Local memory layout 
      ! It shall be possible to specify whether the field data is row major 
      ! or column major at field creation and to rearrange it (assumes 
      ! local copy).
      !EX_remove_UTest_Multi_Proc_Only

      ! Requirement 1.3 Index Order
      ! It shall be possible to specify the index order of field data and 
      !  also to rearrange it.
      ! Cannot be tested until Bug 705308 "ESMF_FieldGetLocalDataInfo not 
      !  implemented" is fixed.

      ! Requirement 1.7.2 Query number of dimensions and index order
      ! A Field shall be able to return the number of dimensions and index 
      !  order it has.
      ! Cannot be tested until Bug 705308 "ESMF_FieldGetLocalDataInfo not 
      !  implemented" is fixed.

      ! Requirement 1.7.3 Query attributes
      ! A Field can return its list of attributes.
      ! Cannot be tested until Bug 705716 "Field Query attributes not 
      !  implemented" is fixed.

      allocate(fptr(5,10,15))
      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only 
      f3 = ESMF_FieldCreate(grid, farray=fptr, indexflag=ESMF_INDEX_DELOCAL, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Create a field from grid and fortran dummy array"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only 
      call ESMF_FieldGet(f3, ungriddedLBound=ulb, ungriddedUBound=uub, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Get ungridded bounds from Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only 
      if(ulb(1) /= 1 .or. uub(1) /= 15) rc = 1
      write(failMsg, *) ""
      write(name, *) "Verify ungridded bounds are valid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldDestroy(f3)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only 
      f3 = ESMF_FieldCreate(grid, farray=fptr, indexflag=ESMF_INDEX_DELOCAL, ungriddedLBound=(/3/), rc=rc)
      write(failMsg, *) ""
      write(name, *) "Create a field from grid and fortran dummy array"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only 
      call ESMF_FieldGet(f3, ungriddedLBound=ulb, ungriddedUBound=uub, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Get ungridded bounds from Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only 
      if(ulb(1) /= 3 .or. uub(1) /= 17) rc = 1
      write(failMsg, *) ""
      write(name, *) "Verify ungridded bounds are valid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldDestroy(f3)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only 
      f3 = ESMF_FieldCreate(grid, farray=fptr, indexflag=ESMF_INDEX_DELOCAL, ungriddedUBound=(/12/), rc=rc)
      write(failMsg, *) ""
      write(name, *) "Create a field from grid and fortran dummy array"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only 
      call ESMF_FieldGet(f3, ungriddedLBound=ulb, ungriddedUBound=uub, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Get ungridded bounds from Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only 
      if(ulb(1) /= -2 .or. uub(1) /= 12) rc = 1
      write(failMsg, *) ""
      write(name, *) "Verify ungridded bounds are valid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldDestroy(f3)
! grid destroy to clear previous grids.
      deallocate(fptr)
      call ESMF_GridDestroy(grid, rc=rc)

#endif

      call ESMF_TestEnd(result, ESMF_SRCLINE)

      end program ESMF_FieldUTest
