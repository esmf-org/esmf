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
      program ESMF_FieldBundleUTest

!------------------------------------------------------------------------------
!
#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_FieldBundleTest - FieldBundle Unit Tests
!
! !DESCRIPTION:
!
! The code in this file drives F90 FieldBundle unit tests.
! The companion file ESMF\_FieldBundle.F90 contains the definitions for the
! FieldBundle methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF

      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id$'
!------------------------------------------------------------------------------

!     ! Local variables
      integer :: rc, petCount,localPet
      type(ESMF_VM) :: vm
      type(ESMF_FieldBundle) :: bundle2, fieldbundleAlias
      logical:: fieldbundleBool
      logical:: isCreated


      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name
#ifdef ESMF_TESTEXHAUSTIVE
      type(ESMF_Grid) :: grid
      integer :: i, fieldcount,localrc, loop_rc, n_match
      integer :: number, count
      character (len = ESMF_MAXSTR) :: fname1, fname2,fname3
      character(len = ESMF_MAXSTR), dimension(10) :: fieldNameList
      type(ESMF_Field) :: fields(10),fieldTst(2), fields5(10), fields6(10)
      type(ESMF_Grid) :: grid2, gridTst1,gridTst2, grid5
      type(ESMF_LocStream) :: locstreamTst1, locStreamTst2
      type(ESMF_Mesh) :: meshTst1, meshTst2
      type (ESMF_ArraySpec) :: arrayspec
      type(ESMF_Field) :: simplefield, field5, ft, fp
      type(ESMF_Field) :: returnedfield1, returnedfield2, returnedfield3
      !real (ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr2
      type(ESMF_FieldBundle) :: bundle1, bundle3, bundle4, bundleTst, bundle5
      character (len = ESMF_MAXSTR) :: bname1
      integer, pointer :: nodeIds(:),nodeOwners(:)
      real(ESMF_KIND_R8), pointer :: nodeCoords(:)
      integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
      integer :: numNodes, numElems
      character(len=ESMF_MAXSTR) :: fnames(10), fnames5(10)

      character(1), pointer :: buffer(:)
      integer :: buffer_len
      integer :: offset, offset_inq
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
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

     ! get global VM
     call ESMF_VMGetGlobal(vm, rc=rc)
     if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
     call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
     if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing FieldBundle IsCreated for uncreated object"
  write(failMsg, *) "Did not return .false."
  isCreated = ESMF_FieldBundleIsCreated(bundle2)
  call ESMF_Test((isCreated .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing FieldBundle IsCreated for uncreated object"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_FieldBundleIsCreated(bundle2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Create test FieldBundle for IsCreated"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  bundle2 = ESMF_FieldBundleCreate(name="time step 1", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing FieldBundle IsCreated for created object"
  write(failMsg, *) "Did not return .true."
  isCreated = ESMF_FieldBundleIsCreated(bundle2)
  call ESMF_Test((isCreated .eqv. .true.), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing FieldBundle IsCreated for created object"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_FieldBundleIsCreated(bundle2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroy test FieldBundle for IsCreated"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldBundleDestroy(bundle2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing FieldBundle IsCreated for destroyed object"
  write(failMsg, *) "Did not return .false."
  isCreated = ESMF_FieldBundleIsCreated(bundle2)
  call ESMF_Test((isCreated .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing FieldBundle IsCreated for destroyed object"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_FieldBundleIsCreated(bundle2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------


      !NEX_UTest
      !  Verify that an empty FieldBundle can be created
      bundle2 = ESMF_FieldBundleCreate(name="time step 1", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating Empty FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "FieldBundle equality before assignment Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      fieldbundleBool = (fieldbundleAlias.eq.bundle2)
      call ESMF_Test(.not.fieldbundleBool, name, failMsg, result, ESMF_SRCLINE)
  
      !------------------------------------------------------------------------
      !NEX_UTest
      ! Testing ESMF_FieldBundleAssignment(=)()
      write(name, *) "FieldBundle assignment and equality Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      fieldbundleAlias = bundle2
      fieldbundleBool = (fieldbundleAlias.eq.bundle2)
      call ESMF_Test(fieldbundleBool, name, failMsg, result, ESMF_SRCLINE)
  
      !------------------------------------------------------------------------
      ! Test Requirement - Deletion
      ! FieldBundles may be deleted. Data allocated by and included in packed bundles
      ! is deleted along with the bundle. Pointers to field data in unpacked 
      ! bundles are returned at deletion. 
      !NEX_UTest
      write(name, *) "FieldBundleDestroy Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_FieldBundleDestroy(bundle2, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
      !------------------------------------------------------------------------
      !NEX_UTest
      ! Testing ESMF_FieldBundleOperator(==)()
      write(name, *) "FieldBundle equality after destroy Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      fieldbundleBool = (fieldbundleAlias==bundle2)
      call ESMF_Test(.not.fieldbundleBool, name, failMsg, result, ESMF_SRCLINE)
  
      !------------------------------------------------------------------------
      !NEX_UTest
      ! Testing ESMF_FieldBundleOperator(/=)()
      write(name, *) "FieldBundle non-equality after destroy Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      fieldbundleBool = (fieldbundleAlias/=bundle2)
      call ESMF_Test(fieldbundleBool, name, failMsg, result, ESMF_SRCLINE)
  
      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Double FieldBundleDestroy through alias Test"
      write(failMsg, *) "Returned ESMF_SUCCESS"
      call ESMF_FieldBundleDestroy(fieldbundleAlias, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_TESTEXHAUSTIVE

#if 0
      ! Remove test until better GridMatch functionality is in place

      !------------------------------------------------------------------------
      !EX_REMOVE_UTest

      ! init rc
      rc=ESMF_SUCCESS

      ! Create a couple of grids
      gridTst1=ESMF_GridCreateNoPeriDim(maxIndex=(/8,8/), regDecomp=(/2,2/), name="Grid", rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      gridTst2=ESMF_GridCreateNoPeriDim(maxIndex=(/16,16/), regDecomp=(/2,2/), name="Grid", rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      ! Set ArraySpec
      call ESMF_ArraySpecSet(arrayspec, rank=2, typekind=ESMF_TYPEKIND_R8, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      ! Create a couple of Fields
      fieldTst(1)=ESMF_FieldCreate(grid=gridTst1, arrayspec=arrayspec, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      fieldTst(2)=ESMF_FieldCreate(grid=gridTst2, arrayspec=arrayspec, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      ! Try creating a bundle of these
      ! SHOULD FAIL BECAUSE ON DIFFERENT GRIDS
      bundleTst=ESMF_FieldBundleCreate(2,fieldTst,rc=localrc)      
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE ! SHOULD FAIL


      ! Destroy Fields
      call ESMF_FieldDestroy(fieldTst(1), rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      call ESMF_FieldDestroy(fieldTst(2), rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      ! Destroy Grids
      call ESMF_GridDestroy(gridTst1, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      call ESMF_GridDestroy(gridTst2, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      write(failMsg, *) "Test not successful"
      write(name, *) "FieldBundleCreate works when fields created on different Grids"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#endif

      !------------------------------------------------------------------------
      !EX_UTest

      ! init rc
      rc=ESMF_SUCCESS

      ! Create a LocStream
      locstreamTst1=ESMF_LocStreamCreate(maxIndex=32, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      ! Set ArraySpec
      call ESMF_ArraySpecSet(arrayspec, rank=1, typekind=ESMF_TYPEKIND_R8, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      ! Create a couple of Fields
      fieldTst(1)=ESMF_FieldCreate(locstream=locstreamTst1, arrayspec=arrayspec, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      fieldTst(2)=ESMF_FieldCreate(locstream=locstreamTst1, arrayspec=arrayspec, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      ! Try creating a bundle of these
      ! SHOULD WORK BECAUSE ON SAME LOCSTREAM
      bundleTst=ESMF_FieldBundleCreate(fieldList=fieldTst,rc=localrc)      
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE 

      ! Destroy FieldBundle
      call ESMF_FieldBundleDestroy(bundleTst, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE 

      ! Destroy Fields
      call ESMF_FieldDestroy(fieldTst(1), rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      call ESMF_FieldDestroy(fieldTst(2), rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      ! Destroy LocStream
      call ESMF_LocStreamDestroy(locStreamTst1, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      write(failMsg, *) "Test not successful"
      write(name, *) "Test of creating a FieldBundle on fields built on LocStreams"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      !EX_UTest

      ! init rc
      rc=ESMF_SUCCESS

      ! Create a couple of LocStream
      locstreamTst1=ESMF_LocStreamCreate(maxIndex=32, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      locstreamTst2=ESMF_LocStreamCreate(maxIndex=64, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      ! Set ArraySpec
      call ESMF_ArraySpecSet(arrayspec, rank=1, typekind=ESMF_TYPEKIND_R8, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      ! Create a couple of Fields on different locstreams
      fieldTst(1)=ESMF_FieldCreate(locstream=locstreamTst1, arrayspec=arrayspec, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      fieldTst(2)=ESMF_FieldCreate(locstream=locstreamTst2, arrayspec=arrayspec, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      ! Try creating a bundle of these
      ! SHOULD FAIL BECAUSE ON DIFFERENT LocStreams
      bundleTst=ESMF_FieldBundleCreate(fieldList=fieldTst,rc=localrc)      
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE ! SHOULD FAIL

      ! Destroy Fields
      call ESMF_FieldDestroy(fieldTst(1), rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      call ESMF_FieldDestroy(fieldTst(2), rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      ! Destroy LocStreams
      call ESMF_LocStreamDestroy(locStreamTst1, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      call ESMF_LocStreamDestroy(locStreamTst2, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      write(failMsg, *) "Test not successful"
      write(name, *) "Make sure FieldBundleCreate works when fields created on different Location Streams"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest

      ! init rc
      rc=ESMF_SUCCESS

      ! Create a LocStream
      locstreamTst1=ESMF_LocStreamCreate(maxIndex=32, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      ! Set ArraySpec
      call ESMF_ArraySpecSet(arrayspec, rank=1, typekind=ESMF_TYPEKIND_R8, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      ! Create a couple of Fields
      fieldTst(1)=ESMF_FieldCreate(locstream=locstreamTst1, arrayspec=arrayspec, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      fieldTst(2)=ESMF_FieldCreate(locstream=locstreamTst1, arrayspec=arrayspec, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      ! Try creating a bundle
      bundleTst=ESMF_FieldBundleCreate(rc=localrc)      
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE 

      ! Add fields
      call ESMF_FieldBundleAdd(bundleTst, (/fieldTst/),rc=localrc)      
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE 

      ! Destroy FieldBundle
      call ESMF_FieldBundleDestroy(bundleTst, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE 

      ! Destroy Fields
      call ESMF_FieldDestroy(fieldTst(1), rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      call ESMF_FieldDestroy(fieldTst(2), rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      ! Destroy LocStream
      call ESMF_LocStreamDestroy(locStreamTst1, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      write(failMsg, *) "Test not successful"
      write(name, *) "Test of creating a LocStream FieldBundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest

      ! init rc
      rc=ESMF_SUCCESS

      ! Create a Mesh
      if (petCount .eq. 1) then
         ! Fill in node data
         numNodes=9

         !! node ids
         allocate(nodeIds(numNodes))
         nodeIds=(/1,2,3,4,5,6,7,8,9/) 

        !! node Coords
        allocate(nodeCoords(numNodes*2))
        nodeCoords=(/0.0,0.0, &
                     1.0,0.0, &
                     2.0,0.0, &
                     0.0,1.0, &
                     1.0,1.0, &
                     2.0,1.0, &
                     0.0,2.0, &
                     1.0,2.0, &
                     2.0,2.0 /)

        !! node owners
        allocate(nodeOwners(numNodes))
        nodeOwners=0 ! everything on proc 0

        ! Fill in elem data
        numElems=4

        !! elem ids
        allocate(elemIds(numElems))
        elemIds=(/1,2,3,4/) 

        !! elem types
        allocate(elemTypes(numElems))
        elemTypes=ESMF_MESHELEMTYPE_QUAD

        !! elem conn
        allocate(elemConn(numElems*4))
        elemConn=(/1,2,5,4, & 
                   2,3,6,5, & 
                   4,5,8,7, & 
                   5,6,9,8/)

       ! Create Mesh structure in 1 step
       meshTst1=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
                  nodeIds=nodeIds, nodeCoords=nodeCoords, &
                  nodeOwners=nodeOwners, elementIds=elemIds,&
                  elementTypes=elemTypes, elementConn=elemConn, &
                  rc=localrc)
       if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

       ! deallocate node data
       deallocate(nodeIds)
       deallocate(nodeCoords)
       deallocate(nodeOwners)

       ! deallocate elem data
       deallocate(elemIds)
       deallocate(elemTypes)
       deallocate(elemConn)

     else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
     if (localPet .eq. 0) then
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,5/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/0.0,0.0, &
                    1.0,0.0, &
                    0.0,1.0, &
                    1.0,1.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,0,0,0/) ! everything on proc 0

       ! Fill in elem data
       numElems=1

       !! elem ids
       allocate(elemIds(numElems))
       elemIds=(/1/) 

       !! elem type
       allocate(elemTypes(numElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numElems*4))
       elemConn=(/1,2,4,3/)
     else if (localPet .eq. 1) then
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/2,3,5,6/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/1.0,0.0, &
                    2.0,0.0, &
                    1.0,1.0, &
                    2.0,1.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,1,0,1/) 

       ! Fill in elem data
       numElems=1

       !! elem ids
       allocate(elemIds(numElems))
       elemIds=(/2/) 

       !! elem type
       allocate(elemTypes(numElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numElems*4))
       elemConn=(/1,2,4,3/)
     else if (localPet .eq. 2) then
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/4,5,7,8/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/0.0,1.0, &
                    1.0,1.0, &
                    0.0,2.0, &
                    1.0,2.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,0,2,2/) 

       ! Fill in elem data
       numElems=1

       !! elem ids
       allocate(elemIds(numElems))
       elemIds=(/3/) 

       !! elem type
       allocate(elemTypes(numElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numElems*4))
       elemConn=(/1,2,4,3/)  
     else 
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/5,6,8,9/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/1.0,1.0, &
                    2.0,1.0, &
                    1.0,2.0, &
                    2.0,2.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,1,2,3/) 

       ! Fill in elem data
       numElems=1

       !! elem ids
       allocate(elemIds(numElems))
       elemIds=(/4/) 

       !! elem type
       allocate(elemTypes(numElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numElems*4))
       elemConn=(/1,2,4,3/)  
     endif

    ! Create Mesh structure in 1 step
    meshTst1=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, &
         rc=localrc)
    if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


    ! deallocate node data
    deallocate(nodeIds)
    deallocate(nodeCoords)
    deallocate(nodeOwners)

    ! deallocate elem data
    deallocate(elemIds)
    deallocate(elemTypes)
    deallocate(elemConn)

   endif ! endif for skip for .ne. 1 or 4 proc

   if ((petCount .eq. 1) .or. (petCount .eq. 4)) then
      ! Set ArraySpec
      call ESMF_ArraySpecSet(arrayspec, rank=1, typekind=ESMF_TYPEKIND_R8, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      ! Create a couple of Fields
      fieldTst(1)=ESMF_FieldCreate(mesh=meshTst1, arrayspec=arrayspec, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      fieldTst(2)=ESMF_FieldCreate(mesh=meshTst1, arrayspec=arrayspec, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      ! Try creating a bundle of these
      ! SHOULD WORK BECAUSE ON SAME MESHES
      bundleTst=ESMF_FieldBundleCreate(fieldList=fieldTst,rc=localrc)      
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE 

      ! Destroy FieldBundle
      call ESMF_FieldBundleDestroy(bundleTst, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE 

      ! Destroy Fields
      call ESMF_FieldDestroy(fieldTst(1), rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      call ESMF_FieldDestroy(fieldTst(2), rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      ! Get rid of Meshes
      call ESMF_MeshDestroy(meshTst1, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     endif ! if 1 or 4 PETS

      write(failMsg, *) "Test not successful"
      write(name, *) "Test of creating a FieldBundle on fields built on the same Mesh"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      !EX_UTest

      ! init rc
      rc=ESMF_SUCCESS

      ! Create a Mesh
      if (petCount .eq. 1) then
         ! Fill in node data
         numNodes=9

         !! node ids
         allocate(nodeIds(numNodes))
         nodeIds=(/1,2,3,4,5,6,7,8,9/) 

        !! node Coords
        allocate(nodeCoords(numNodes*2))
        nodeCoords=(/0.0,0.0, &
                     1.0,0.0, &
                     2.0,0.0, &
                     0.0,1.0, &
                     1.0,1.0, &
                     2.0,1.0, &
                     0.0,2.0, &
                     1.0,2.0, &
                     2.0,2.0 /)

        !! node owners
        allocate(nodeOwners(numNodes))
        nodeOwners=0 ! everything on proc 0

        ! Fill in elem data
        numElems=4

        !! elem ids
        allocate(elemIds(numElems))
        elemIds=(/1,2,3,4/) 

        !! elem types
        allocate(elemTypes(numElems))
        elemTypes=ESMF_MESHELEMTYPE_QUAD

        !! elem conn
        allocate(elemConn(numElems*4))
        elemConn=(/1,2,5,4, & 
                   2,3,6,5, & 
                   4,5,8,7, & 
                   5,6,9,8/)

       ! Create Mesh structure in 1 step
       meshTst1=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
                  nodeIds=nodeIds, nodeCoords=nodeCoords, &
                  nodeOwners=nodeOwners, elementIds=elemIds,&
                  elementTypes=elemTypes, elementConn=elemConn, &
                  rc=localrc)
       if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

       ! deallocate node data
       deallocate(nodeIds)
       deallocate(nodeCoords)
       deallocate(nodeOwners)

       ! deallocate elem data
       deallocate(elemIds)
       deallocate(elemTypes)
       deallocate(elemConn)


         ! Fill in node data
         numNodes=8

         !! node ids
         allocate(nodeIds(numNodes))
         nodeIds=(/1,2,3,4,5,6,7,8/) 

        !! node Coords
        allocate(nodeCoords(numNodes*2))
        nodeCoords=(/0.0,0.0, &
                     1.0,0.0, &
                     2.0,0.0, &
                     0.0,1.0, &
                     1.0,1.0, &
                     2.0,1.0, &
                     0.0,2.0, &
                     1.0,2.0 /)

        !! node owners
        allocate(nodeOwners(numNodes))
        nodeOwners=0 ! everything on proc 0

        ! Fill in elem data
        numElems=4

        !! elem ids
        allocate(elemIds(numElems))
        elemIds=(/1,2,3,4/) 

        !! elem types
        allocate(elemTypes(numElems))
        elemTypes=ESMF_MESHELEMTYPE_QUAD
        elemTypes(4)=ESMF_MESHELEMTYPE_TRI

        !! elem conn
        allocate(elemConn(15))
        elemConn=(/1,2,5,4, & 
                   2,3,6,5, & 
                   4,5,8,7, & 
                   5,6,8/)


       ! Create Mesh structure in 1 step
       meshTst2=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
                  nodeIds=nodeIds, nodeCoords=nodeCoords, &
                  nodeOwners=nodeOwners, elementIds=elemIds,&
                  elementTypes=elemTypes, elementConn=elemConn, &
                  rc=localrc)
       if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

       ! deallocate node data
       deallocate(nodeIds)
       deallocate(nodeCoords)
       deallocate(nodeOwners)

       ! deallocate elem data
       deallocate(elemIds)
       deallocate(elemTypes)
       deallocate(elemConn)

     else if (petCount .eq. 4) then

     ! Setup mesh data depending on PET
     if (localPet .eq. 0) then
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,5/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/0.0,0.0, &
                    1.0,0.0, &
                    0.0,1.0, &
                    1.0,1.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,0,0,0/) ! everything on proc 0

       ! Fill in elem data
       numElems=1

       !! elem ids
       allocate(elemIds(numElems))
       elemIds=(/1/) 

       !! elem type
       allocate(elemTypes(numElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numElems*4))
       elemConn=(/1,2,4,3/)
     else if (localPet .eq. 1) then
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/2,3,5,6/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/1.0,0.0, &
                    2.0,0.0, &
                    1.0,1.0, &
                    2.0,1.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,1,0,1/) 

       ! Fill in elem data
       numElems=1

       !! elem ids
       allocate(elemIds(numElems))
       elemIds=(/2/) 

       !! elem type
       allocate(elemTypes(numElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numElems*4))
       elemConn=(/1,2,4,3/)
     else if (localPet .eq. 2) then
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/4,5,7,8/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/0.0,1.0, &
                    1.0,1.0, &
                    0.0,2.0, &
                    1.0,2.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,0,2,2/) 

       ! Fill in elem data
       numElems=1

       !! elem ids
       allocate(elemIds(numElems))
       elemIds=(/3/) 

       !! elem type
       allocate(elemTypes(numElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numElems*4))
       elemConn=(/1,2,4,3/)  
     else 
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/5,6,8,9/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/1.0,1.0, &
                    2.0,1.0, &
                    1.0,2.0, &
                    2.0,2.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,1,2,3/) 

       ! Fill in elem data
       numElems=1

       !! elem ids
       allocate(elemIds(numElems))
       elemIds=(/4/) 

       !! elem type
       allocate(elemTypes(numElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numElems*4))
       elemConn=(/1,2,4,3/)  
     endif


    ! Create Mesh structure in 1 step
    meshTst1=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, &
         rc=localrc)
    if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


    ! deallocate node data
    deallocate(nodeIds)
    deallocate(nodeCoords)
    deallocate(nodeOwners)

    ! deallocate elem data
    deallocate(elemIds)
    deallocate(elemTypes)
    deallocate(elemConn)

     ! Setup mesh data depending on PET
     if (localPet .eq. 0) then
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,5/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/0.0,0.0, &
                    1.0,0.0, &
                    0.0,1.0, &
                    1.0,1.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,0,0,0/) ! everything on proc 0

       ! Fill in elem data
       numElems=1

       !! elem ids
       allocate(elemIds(numElems))
       elemIds=(/1/) 

       !! elem type
       allocate(elemTypes(numElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numElems*4))
       elemConn=(/1,2,4,3/)
     else if (localPet .eq. 1) then
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/2,3,5,6/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/1.0,0.0, &
                    2.0,0.0, &
                    1.0,1.0, &
                    2.0,1.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,1,0,1/) 

       ! Fill in elem data
       numElems=1

       !! elem ids
       allocate(elemIds(numElems))
       elemIds=(/2/) 

       !! elem type
       allocate(elemTypes(numElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numElems*4))
       elemConn=(/1,2,4,3/)
     else if (localPet .eq. 2) then
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/4,5,7,8/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/0.0,1.0, &
                    1.0,1.0, &
                    0.0,2.0, &
                    1.0,2.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,0,2,2/) 

       ! Fill in elem data
       numElems=1

       !! elem ids
       allocate(elemIds(numElems))
       elemIds=(/3/) 

       !! elem type
       allocate(elemTypes(numElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numElems*4))
       elemConn=(/1,2,4,3/)  
     else 
        ! Fill in node data
        numNodes=3

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/5,6,8/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/1.0,1.0, &
                    2.0,1.0, &
                    1.0,2.0 /)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,1,2/) 

       ! Fill in elem data
       numElems=1

       !! elem ids
       allocate(elemIds(numElems))
       elemIds=(/4/) 

       !! elem type
       allocate(elemTypes(numElems))
       elemTypes=ESMF_MESHELEMTYPE_TRI

       !! elem conn
       allocate(elemConn(numElems*3))
       elemConn=(/1,2,3/)  
     endif


    ! Create Mesh structure in 1 step
    meshTst2=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, &
         rc=localrc)
    if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


    ! deallocate node data
    deallocate(nodeIds)
    deallocate(nodeCoords)
    deallocate(nodeOwners)

    ! deallocate elem data
    deallocate(elemIds)
    deallocate(elemTypes)
    deallocate(elemConn)

   endif ! endif for skip for >4 proc


   if ((petCount .eq. 1) .or. (petCount .eq. 4)) then
      ! Set ArraySpec
      call ESMF_ArraySpecSet(arrayspec, rank=1, typekind=ESMF_TYPEKIND_R8, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      ! Create a couple of Fields
      fieldTst(1)=ESMF_FieldCreate(mesh=meshTst1, arrayspec=arrayspec, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      fieldTst(2)=ESMF_FieldCreate(mesh=meshTst2, arrayspec=arrayspec, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      ! Create FieldBundle
      ! SHOULD FAIL BECAUSE ON DIFFERENT Meshes
      bundleTst=ESMF_FieldBundleCreate(fieldList=fieldTst,rc=localrc)      
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE ! SHOULD FAIL


      ! Destroy Fields
      call ESMF_FieldDestroy(fieldTst(1), rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      call ESMF_FieldDestroy(fieldTst(2), rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      ! Get rid of Mesh
      call ESMF_MeshDestroy(meshTst1, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      call ESMF_MeshDestroy(meshTst2, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     endif ! if 1 or 4 PETS

      write(failMsg, *) "Test not successful"
      write(name, *) "Make sure FieldBundleCreate works when fields created on different Meshes"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest

      ! init rc
      rc=ESMF_SUCCESS

      ! Create a Mesh
      if (petCount .eq. 1) then
         ! Fill in node data
         numNodes=9

         !! node ids
         allocate(nodeIds(numNodes))
         nodeIds=(/1,2,3,4,5,6,7,8,9/) 

        !! node Coords
        allocate(nodeCoords(numNodes*2))
        nodeCoords=(/0.0,0.0, &
                     1.0,0.0, &
                     2.0,0.0, &
                     0.0,1.0, &
                     1.0,1.0, &
                     2.0,1.0, &
                     0.0,2.0, &
                     1.0,2.0, &
                     2.0,2.0 /)

        !! node owners
        allocate(nodeOwners(numNodes))
        nodeOwners=0 ! everything on proc 0

        ! Fill in elem data
        numElems=4

        !! elem ids
        allocate(elemIds(numElems))
        elemIds=(/1,2,3,4/) 

        !! elem types
        allocate(elemTypes(numElems))
        elemTypes=ESMF_MESHELEMTYPE_QUAD

        !! elem conn
        allocate(elemConn(numElems*4))
        elemConn=(/1,2,5,4, & 
                   2,3,6,5, & 
                   4,5,8,7, & 
                   5,6,9,8/)

       ! Create Mesh structure in 1 step
       meshTst1=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
                  nodeIds=nodeIds, nodeCoords=nodeCoords, &
                  nodeOwners=nodeOwners, elementIds=elemIds,&
                  elementTypes=elemTypes, elementConn=elemConn, &
                  rc=localrc)
       if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

       ! deallocate node data
       deallocate(nodeIds)
       deallocate(nodeCoords)
       deallocate(nodeOwners)

       ! deallocate elem data
       deallocate(elemIds)
       deallocate(elemTypes)
       deallocate(elemConn)

     else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
     if (localPet .eq. 0) then
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,5/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/0.0,0.0, &
                    1.0,0.0, &
                    0.0,1.0, &
                    1.0,1.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,0,0,0/) ! everything on proc 0

       ! Fill in elem data
       numElems=1

       !! elem ids
       allocate(elemIds(numElems))
       elemIds=(/1/) 

       !! elem type
       allocate(elemTypes(numElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numElems*4))
       elemConn=(/1,2,4,3/)
     else if (localPet .eq. 1) then
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/2,3,5,6/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/1.0,0.0, &
                    2.0,0.0, &
                    1.0,1.0, &
                    2.0,1.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,1,0,1/) 

       ! Fill in elem data
       numElems=1

       !! elem ids
       allocate(elemIds(numElems))
       elemIds=(/2/) 

       !! elem type
       allocate(elemTypes(numElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numElems*4))
       elemConn=(/1,2,4,3/)
     else if (localPet .eq. 2) then
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/4,5,7,8/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/0.0,1.0, &
                    1.0,1.0, &
                    0.0,2.0, &
                    1.0,2.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,0,2,2/) 

       ! Fill in elem data
       numElems=1

       !! elem ids
       allocate(elemIds(numElems))
       elemIds=(/3/) 

       !! elem type
       allocate(elemTypes(numElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numElems*4))
       elemConn=(/1,2,4,3/)  
     else 
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/5,6,8,9/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/1.0,1.0, &
                    2.0,1.0, &
                    1.0,2.0, &
                    2.0,2.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,1,2,3/) 

       ! Fill in elem data
       numElems=1

       !! elem ids
       allocate(elemIds(numElems))
       elemIds=(/4/) 

       !! elem type
       allocate(elemTypes(numElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numElems*4))
       elemConn=(/1,2,4,3/)  
     endif

    ! Create Mesh structure in 1 step
    meshTst1=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, &
         rc=localrc)
    if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


    ! deallocate node data
    deallocate(nodeIds)
    deallocate(nodeCoords)
    deallocate(nodeOwners)

    ! deallocate elem data
    deallocate(elemIds)
    deallocate(elemTypes)
    deallocate(elemConn)

   endif ! endif for skip for .ne. 1 or 4 proc

   if ((petCount .eq. 1) .or. (petCount .eq. 4)) then
      ! Set ArraySpec
      call ESMF_ArraySpecSet(arrayspec, rank=1, typekind=ESMF_TYPEKIND_R8, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      ! Create a couple of Fields
      fieldTst(1)=ESMF_FieldCreate(mesh=meshTst1, arrayspec=arrayspec, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      fieldTst(2)=ESMF_FieldCreate(mesh=meshTst1, arrayspec=arrayspec, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      ! Try creating a bundle
      bundleTst=ESMF_FieldBundleCreate(rc=localrc)      
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE 

      ! Add fields
      call ESMF_FieldBundleAdd(bundleTst, (/fieldTst/),rc=localrc)      
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE 

      ! Destroy FieldBundle
      call ESMF_FieldBundleDestroy(bundleTst, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE 

      ! Destroy Fields
      call ESMF_FieldDestroy(fieldTst(1), rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      call ESMF_FieldDestroy(fieldTst(2), rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      ! Get rid of Mesh
      call ESMF_MeshDestroy(meshTst1, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     endif ! if 1 or 4 PETS

      write(failMsg, *) "Test not successful"
      write(name, *) "Test of creating a Mesh FieldBundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)



      !------------------------------------------------------------------------
      !EX_UTest
      ! FieldBundle destroy of destroyed FieldBundle
      call ESMF_FieldBundleDestroy(bundle2, rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Destroy of destroyed FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! FieldBundle destroy of non-created FieldBundle
      call ESMF_FieldBundleDestroy(bundle1, rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Destroy of non-created FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Verify getting the name of a non-created  FieldBundle
      call ESMF_FieldBundleGet(bundle2, name=bname1, rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Getting name of deleted FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Verify getting the name of a non-created FieldBundle
      call ESMF_FieldBundleGet(bundle1, name=bname1, rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Getting name of non-create FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Getting Attribute count from a deleted FieldBundle
      call ESMF_AttributeGet(bundle2, count, rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Getting Attribute Count from a deleted FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Getting Attribute count from a non-created FieldBundle
      call ESMF_AttributeGet(bundle1, count, rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Getting Attribute Count from a non-created FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Getting a third Field name from a deleted FieldBundle
      call ESMF_FieldBundleGet(bundle2, 3, returnedfield3, rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Getting a third Field by index from a deleted FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Getting a third Field name from a non-created FieldBundle
      call ESMF_FieldBundleGet(bundle1, 3, returnedfield3, rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Getting a third Field by index from a non-create FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Validate a deleted FieldBundle Test
      call ESMF_FieldBundleValidate(bundle2, rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Validating a deleted FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      !EX_UTest
      ! Validate a non-created FieldBundle Test
      call ESMF_FieldBundleValidate(bundle1, rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Validating a non-created FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Requirement Deletion
      ! Verify getting the name of an uninitialized FieldBundle is handled properly.
      call ESMF_FieldBundleGet(bundle1, name=bname1, rc=rc)
      write(failMsg, *) "Subroutine should have returned ESMF_FAILURE"
      write(name, *) "Getting name of uninitalized FieldBundle Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
     
      !EX_UTest
      !  Verify that Field count query from an uninitialized FieldBundle is handled
      !  properly
      call ESMF_FieldBundleGet(bundle1, fieldCount=fieldcount, rc=rc)
      write(failMsg, *) "Returned ESMF_SUCCESS"
      write(name, *) "Getting Field count from an uninitialized FieldBundle Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
     
      !EX_UTest
      !This test crashes, bug 1169299 created, commented out
      !  Verify the getting Field names query from an uninitialized FieldBundle is handled
      ! (I think its fixed - Bob 2/12/2007)
      call ESMF_FieldBundleGet(bundle1, fieldnameList=fieldNameList, fieldCount=fieldcount, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting Field names from an uninitialized FieldBundle Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !print *, "Field count of uninitialized FieldBundle = ", fieldcount
      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Requirement Creation using Field list
      ! It shall be possible to create a bundle with a field list, an optional 
      ! I/O specification, and an identifier that specifies whether the bundle 
      ! is to be packed (contiguous data) or loose (noncontiguous data). 
      !  Create several empty Fields and add them to a new FieldBundle.
      fields(1) = ESMF_FieldEmptyCreate(name="pressure", rc=rc)
      fields(2) = ESMF_FieldEmptyCreate(name="temperature", rc=rc)
      fields(3) = ESMF_FieldEmptyCreate(name="heat flux", rc=rc)
      bundle1 = ESMF_FieldBundleCreate(fieldList=fields(1:3), name="atmosphere data", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating FieldBundle with 3 No Data Fields Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !E-X_UTest
      ! Test Requirement Creating a FieldBundle with ESMF_PACKED_DATA option
      ! The ESMF_PACKED_DATA option is not implemented and until it is, it
      ! is correct for the method to return ESMF_RC_NOT_IMPL when it is used.
      ! Fei: disabled until packing is implemented
      !write(failMsg, *) "Did not return ESMF_RC_NOT_IMPL"
      !bundle4 = ESMF_FieldBundleCreate(fieldList=fields(1:3), &
      !      name="atmosphere data", rc=rc)
      !write(name, *) "Creating FieldBundle with ESMF_PACKED_DATA"
      !call ESMF_Test((rc.eq.ESMF_RC_NOT_IMPL), name, failMsg, result, ESMF_SRCLINE)
      !print *, "rc = ", rc
      !------------------------------------------------------------------------

      !EX_UTest
      !  Verify the getting Field names query from FieldBundle returns ESMF_SUCCESS
      call ESMF_FieldBundleGet(bundle1, fieldnameList=fieldNameList, fieldCount=fieldcount, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Field names from a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      !  Verify the Field names query count is correct
      print *, "The Field names are:"
      do i = 1 , fieldcount
      print *, fieldNameList(i)
      end do
      write(failMsg, *) "Field count not 3"
      write(name, *) "Verifying Field count from a FieldBundle Test"
      call ESMF_Test((fieldcount.eq.3), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_FieldBundleAdd(bundle2, (/simplefield/), rc=rc)
      write(failMsg, *) "Add uninitialized Field to uncreated FieldBundle failed"
      write(name, *) "Adding an uninitialized Field to an uncreated FieldBundle Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !Verify that an empty FieldBundle can be created
      !EX_UTest
      bundle2 = ESMF_FieldBundleCreate(name="time step 1", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating Empty FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Creating a Grid
      grid=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/180,90/), &
                                regDecomp=(/2,2/), name="Grid", rc=rc)

      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Create a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Creating a Field Test
      simplefield = ESMF_FieldEmptyCreate(name="rh", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Create a Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      !  Verify the getting Field names query from FieldBundle returns ESMF_SUCCESS
      call ESMF_FieldBundleGet(bundle2, fieldnameList=fieldNameList, fieldCount=fieldcount, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Field names from a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      
      !EX_UTest
      !  Verify the Field names query count is correct
      write(failMsg, *) "Field count not 0"
      write(name, *) "Verifying Field count from a FieldBundle Test"
      call ESMF_Test((fieldcount.eq.0), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Add a field to an empty FieldBundle
      call ESMF_FieldBundleAdd(bundle2, (/simplefield/), rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding a field to an Empty FieldBundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      !  Verify the getting Field names query from FieldBundle returns ESMF_SUCCESS
      call ESMF_FieldBundleGet(bundle2, fieldnameList=fieldNameList, fieldCount=fieldcount, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Field names from a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      
      !EX_UTest
      !  Verify the Field names query count is correct
      write(failMsg, *) "Field count not 1"
      write(name, *) "Verifying Field count from a FieldBundle Test"
      call ESMF_Test((fieldcount.eq.1), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------


      !EX_UTest
      !  Verify the Field names are correct
      write(failMsg, *) "Field name is wrong"
      write(name, *) "Verifying Field name from a FieldBundle Test"
      call ESMF_Test((fieldNameList(1).eq."rh"), name, failMsg, result, ESMF_SRCLINE)

      print *, "The Field names is:"
      do i = 1 , fieldcount
      print *, fieldNameList(i)
      end do

      !------------------------------------------------------------------------
      !EX_UTest
      !  Verify that recreating a created FieldBundle is handled properly
      bundle3 = bundle2 ! get copy to prevent memory leak
      bundle2 = ESMF_FieldBundleCreate(name="time step 1", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a FieldBundle that has already been created Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      !  Verify that the Field count query from an empty FieldBundle is handled properly
      print *, 'fieldcount = ', fieldcount
      call ESMF_FieldBundleGet(bundle2, fieldCount=fieldcount, rc=rc)
      print *, 'fieldcount = ', fieldcount
      write(failMsg, *) "Returned ESMF_FAILURE or field count not equal to zero"
      write(name, *) "Getting Field count from an empty FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(fieldcount.eq.0), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "Field count of empty FieldBundle = ", fieldcount
      !------------------------------------------------------------------------

      !EX_UTest
      !  Verify that a Field can be added to an empty FieldBundle
      call ESMF_FieldBundleAdd(bundle2, (/simplefield/), rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding a Field to an Empty FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      !  Verify that the Field count can be queried from a FieldBundle
      call ESMF_FieldBundleGet(bundle2, fieldCount=fieldcount, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Field count from a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(fieldcount.eq.1), &
                       name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      call ESMF_FieldBundleDestroy(bundle3)

      ! Create an empty FieldBundle and then add multiple fields to it.
      bundle3 = ESMF_FieldBundleCreate(name="southern hemisphere", rc=rc)
      !bundle3 = ESMF_FieldBundleCreate(name="northern hemisphere", rc=rc)
   
      !EX_UTest
      !  Verify that multiple Fields can be added to a FieldBundle 
      call ESMF_FieldBundleAdd(bundle3, fields(1:3), rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding multiple Fields to a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      !  Verify that Fields count can be queried from a FieldBundle 
      call ESMF_FieldBundleGet(bundle3, fieldCount=fieldcount, rc=rc)
      write(failMsg, *) "Returned ESMF_FAILURE or field count not equal to three"
      write(name, *) "Getting Field count from a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(fieldcount.eq.3), &
                     name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldBundleDestroy(bundle3)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Verify that the first Field name can be queried fron a FieldBundle
      call ESMF_FieldBundleGet(bundle1, "pressure", field=returnedfield1, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting first Field by name from a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldGet(returnedfield1, name=fname1, rc=rc)
      !------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS or incorrect name returned"
      write(name, *) "Getting first Field from a FieldBundle Test continued"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(fname1.eq."pressure"), name, &
                        failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Verify that the second Field name can be queried from a FieldBundle
      call ESMF_FieldBundleGet(bundle1, 2, returnedfield2, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting a second Field by index from a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldGet(returnedfield2, name=fname2, rc=rc)

      !EX_UTest
      write(failMsg, *) "Subroutine returned ESMF_FAILURE or incorrect name returned"
      write(name, *) "Getting a second Field from a FieldBundle Test continued"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, &
                        failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Verify that the third Field name can be queried fron a FieldBundle
      call ESMF_FieldBundleGet(bundle1, 3, returnedfield3, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting a third Field by index from a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldGet(returnedfield3, name=fname3, rc=rc)

      !------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Subroutine returned ESMF_FAILURE or incorrect name returned"
      write(name, *) "Getting a third Field from a FieldBundle Test continued"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, &
                        failMsg, result, ESMF_SRCLINE)
      print *, "FieldBundle returned, field names = ", &
                    trim(fname1), ", ", trim(fname2), ", ", trim(fname3)
      !------------------------------------------------------------------------


      !EX_UTest
      ! Verify that the fourth Field name cannot be queried from a FieldBundle
      ! because there are only three Fields in the FieldBundle
      call ESMF_FieldBundleGet(bundle1, 4, returnedfield3, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting a non-existent Field from a FieldBundle Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------


      !EX_UTest
      ! Add an integer attribute to a FieldBundle Test
      call ESMF_AttributeSet(bundle1, name="Sides", value=65, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an integer attribute to a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------


      !EX_UTest
      ! Get an integer attribute from a FieldBundle Test
      call ESMF_AttributeGet(bundle1, name="Sides", value=number, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an integer attribute from a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(number.eq.65), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an integer attribute from a FieldBundle Test
      call ESMF_AttributeGet(bundle1, name="Sides", itemcount=number, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an attribute info from a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(number.eq.1), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Getting Attribute count from a FieldBundle
      call ESMF_AttributeGet(bundle1, count, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Attribute Count from a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      
      !EX_UTest
      ! Verify Attribute count Test
      write(failMsg, *) "Incorrect count"
      write(name, *) "Verify Attribute count from a FieldBundle "
      call ESMF_Test((count.eq.1), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Verify that the third Field names can be queried fron a FieldBundle
      call ESMF_FieldBundleGet(bundle1, 3, returnedfield3, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting a third Field by index from a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldGet(returnedfield3, name=fname3, rc=rc)

! TODO:FIELDINTEGRATE Restore once FieldBundleGetDataPointer is implemented
      !------------------------------------------------------------------------
      ! Get a FieldBundle Data Pointer from a field with no data - should fail
      !EX_removeUTest
      ! write(failMsg, *) "Returned ESMF_SUCCESS incorrectly"
      ! write(name, *) "Get a FieldBundle Data Pointer Test from empty Field"
      ! call ESMF_FieldBundleGetDataPointer(bundle1, fieldName="heat flux", dataPointer=f90ptr2, rc=rc)
      ! call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      ! print *, "rc =",  rc
      !------------------------------------------------------------------------

      !EX_UTest
      ! Print a FieldBundle Test
      call ESMF_FieldBundlePrint(bundle1, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Printing a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Validate a FieldBundle Test
      call ESMF_FieldBundleValidate(bundle1, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Validating a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Verify that the zeroth Field names cannot be queried fron a FieldBundle
      call ESMF_FieldBundleGet(bundle1, 0, returnedfield3, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting a zeroth non-existent Field from a FieldBundle Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------


      !EX_UTest
      ! Verify that the querying Field with wrong name from a FieldBundle returns FAILURE
      call ESMF_FieldBundleGet(bundle1, "nressure", field=returnedfield1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting wrong Field name from a FieldBundle Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------


      !EX_UTest
      ! Verify that the FieldBundle name can be queried 
      call ESMF_FieldBundleGet(bundle1, name=bname1, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Query FieldBundle Name Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bname1.eq."atmosphere data"), &
                    name, failMsg, result, ESMF_SRCLINE)
      print *, "FieldBundle name = ", trim(bname1)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test Requirement Deletion
      ! FieldBundles may be deleted. Data allocated by and included in packed bundles 
      ! is deleted along with the bundle. Pointers to field data in unpacked 
      ! bundles are returned at deletion. 
      call ESMF_FieldBundleDestroy(bundle1, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "FieldBundle Destroy Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, "rc = ", rc
      !------------------------------------------------------------------------


      !EX_UTest
      ! Verify that destroying a destroyed FieldBundle is handled correctly
      call ESMF_FieldBundleDestroy(bundle1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying a Destroyed FieldBundle Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, "rc = ", rc
      !------------------------------------------------------------------------

      !EX_UTest
      ! Destroying Fields
      call ESMF_FieldDestroy(fields(1), rc=rc)
      write(failMsg, *) "Destroying a Field"
      write(name, *) "Destroying a Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Destroying Fields
      call ESMF_FieldDestroy(fields(2), rc=rc)
      write(failMsg, *) "Destroying a Field"
      write(name, *) "Destroying a Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Destroying Fields
      call ESMF_FieldDestroy(fields(3), rc=rc)
      write(failMsg, *) "Destroying a Field"
      write(name, *) "Destroying a Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Destroying fields
      call ESMF_FieldDestroy(simplefield, rc=rc)
      write(failMsg, *) "Destroying a Field"
      write(name, *) "Destroying a Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Destroying a grid
      call ESMF_GridDestroy(grid, rc=rc)
      write(failMsg, *) "Destroying a Grid"
      write(name, *) "Destroying a Grid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Requirement Insert and remove Field
      ! A Field can be inserted into or removed from a FieldBundle
      ! The remove portion of this requirement cannot be tested until Bug 705849
      ! ESMF_FieldBundleDeleteField not implemented" is fixed.
      !------------------------------------------------------------------------
      !EX_UTest
      grid5 = ESMF_GridCreateNoPeriDim(maxIndex=(/10,20/), rc=rc)
      write(failMsg, *) "Creating a Grid"
      write(name, *) "Creating a Grid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      do i = 1, 10
        fields5(i) = ESMF_FieldCreate(grid=grid5, typekind=ESMF_TYPEKIND_I4, rc=rc)
      enddo
      write(failMsg, *) "Creating a list of Fields"
      write(name, *) "Creating a list of Fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      do i = 1, 10
        call ESMF_FieldGet(fields5(i), name=fnames5(i), rc=rc)
      enddo
      write(failMsg, *) "Get Field names"
      write(name, *) "Get Field names"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      do i = 1, 10
        call ESMF_FieldGet(fields5(i), name=fnames5(i), rc=rc)
      enddo
      write(failMsg, *) "Get Field names"
      write(name, *) "Get Field names"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      do i = 1, 10
        fields6(i) = ESMF_FieldCreate(grid=grid5, typekind=ESMF_TYPEKIND_I4, name=fnames5(i), rc=rc)
      enddo
      write(failMsg, *) "Creating another list of Fields"
      write(name, *) "Creating another list of Fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      bundle5 = ESMF_FieldBundleCreate(name = "fb", rc=rc)
      write(failMsg, *) "Creating a FieldBundle"
      write(name, *) "Creating a FieldBundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_FieldBundleAdd(bundle5, fields5(1:4), rc=rc)
      write(failMsg, *) "Add a list of Fields to FieldBundle"
      write(name, *) "Add a list of Fields to FieldBundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_FieldBundleAdd(bundle5, (/fields5(9)/), rc=rc)
      write(failMsg, *) "Add a field to FieldBundle"
      write(name, *) "Add a field to FieldBundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_FieldBundleRemove(bundle5, (/fnames5(2)/), rc=rc)
      write(failMsg, *) "Remove a Field from FieldBundle"
      write(name, *) "Remove a Field from FieldBundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_FieldBundleRemove(bundle5, (/fnames5(7)/), rc=rc)
      write(failMsg, *) "Remove a non-existant Field from FieldBundle"
      write(name, *) "Remove a non-existant Field from FieldBundle"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_FieldBundleRemove(bundle5, (/fnames5(7)/), relaxedflag=.true., rc=rc)
      write(failMsg, *) "Remove a non-existant Field from FieldBundle"
      write(name, *) "Remove a non-existant Field from FieldBundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_FieldBundleReplace(bundle5, (/fields6(7)/), relaxedflag=.true., rc=rc)
      write(failMsg, *) "Replace a non-existant Field from FieldBundle"
      write(name, *) "Replace a non-existant Field from FieldBundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_FieldBundleReplace(bundle5, fields6(2:3), rc=rc)
      write(failMsg, *) "Replace Field from FieldBundle"
      write(name, *) "Replace Field from FieldBundle"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_FieldBundleReplace(bundle5, fields6(6:7), rc=rc)
      write(failMsg, *) "Replace non-existant Field from FieldBundle"
      write(name, *) "Replace non-existant Field from FieldBundle"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Test this twice to make sure no auto-vivification underneath...
      !EX_UTest
      call ESMF_FieldBundleReplace(bundle5, fields6(6:7), rc=rc)
      write(failMsg, *) "Replace non-existant Field from FieldBundle"
      write(name, *) "Replace non-existant Field from FieldBundle"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_FieldBundleReplace(bundle5, fields6(6:7), relaxedflag=.true., rc=rc)
      write(failMsg, *) "Replace non-existant Field from FieldBundle"
      write(name, *) "Replace non-existant Field from FieldBundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_FieldBundleAddReplace(bundle5, fields6(4:7), rc=rc)
      write(failMsg, *) "Replace non-existant Field from FieldBundle"
      write(name, *) "Replace non-existant Field from FieldBundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_FieldBundleAdd(bundle5, fields5(5:10), multiflag=.true., relaxedflag=.true., rc=rc)
      write(failMsg, *) "Add multi Field to FieldBundle"
      write(name, *) "Add multi Field to FieldBundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_FieldBundleReplace(bundle5, (/fields6(7)/), multiflag=.true., rc=rc)
      write(failMsg, *) "Replace a non-unique Field from FieldBundle"
      write(name, *) "Replace a non-unique Field from FieldBundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_FieldBundleRemove(bundle5, fnames5(9:9), multiflag=.true., rc=rc)
      write(failMsg, *) "Remove a non-unique Field from FieldBundle with multiflag turned on"
      write(name, *) "Remove a non-unique Field from FieldBundle with multiflag turned on"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_FieldBundleGet(bundle5, fieldName=fnames5(9), field=field5, rc=rc)
      write(failMsg, *) "Get a non-existant Field from FieldBundle"
      write(name, *) "Get a non-existant Field from FieldBundle"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_FieldBundleDestroy(bundle5, rc=rc)
      write(failMsg, *) "Destroy FieldBundle"
      write(name, *) "Destroy FieldBundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      do i = 1, 10
        Call ESMF_FieldDestroy(fields5(i), rc=rc)
        Call ESMF_FieldDestroy(fields6(i), rc=rc)
      enddo
      call ESMF_GridDestroy(grid5, rc=rc)
      write(failMsg, *) "Destroy Fields and Grid"
      write(name, *) "Destroy Fields and Grid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      bundle5 = ESMF_FieldBundleCreate(name = "fb", rc=rc)
      write(failMsg, *) "Creating a FieldBundle"
      write(name, *) "Creating a FieldBundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ft = ESMF_FieldEmptyCreate(name = "Temperature", rc=rc)
      write(failMsg, *) "Creating a Field"
      write(name, *) "Creating a Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      fp = ESMF_FieldEmptyCreate(name = "Pressure", rc=rc)
      write(failMsg, *) "Creating a Field"
      write(name, *) "Creating a Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_FieldBundleAdd(bundle5, (/ft/), rc=rc)
      write(failMsg, *) "Adding a Field to a FieldBundle"
      write(name, *) "Adding a Field to a FieldBundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_FieldBundleAdd(bundle5, (/fp/), rc=rc)
      write(failMsg, *) "Adding a Field to a FieldBundle"
      write(name, *) "Adding a Field to a FieldBundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_FieldBundleGet(bundle5, fieldList=fields(1:2), &
        itemorderflag=ESMF_ITEMORDER_ADDORDER, rc=rc)
      write(failMsg, *) "Getting Fields from a FieldBundle"
      write(name, *) "Getting Fields from a FieldBundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_FieldBundleGet(bundle5, fieldList=fields(3:4), &
        itemorderflag=ESMF_ITEMORDER_ABC, rc=rc)
      write(failMsg, *) "Getting Fields from a FieldBundle"
      write(name, *) "Getting Fields from a FieldBundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      loop_rc = ESMF_SUCCESS
      do i = 1,2 
        call ESMF_FieldGet(fields(i), name=fnames(i), rc=rc)
        if(rc /= ESMF_SUCCESS) loop_rc=ESMF_FAILURE
        call ESMF_FieldGet(fields(i+2), name=fnames(i+2), rc=rc)
        if(rc /= ESMF_SUCCESS) loop_rc=ESMF_FAILURE
      enddo
      write(failMsg, *) "Getting Names from Fields"
      write(name, *) "Getting Names from Fields"
      call ESMF_Test((loop_rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! The two retrieved name lists are expected to be different.
      loop_rc = ESMF_SUCCESS
      do i = 1,2 
        if(trim(fnames(i)) == trim(fnames(i+2))) loop_rc=ESMF_FAILURE
      enddo
      write(failMsg, *) "Comparing two modes of BundleGet"
      write(name, *) "Comparing two modes of BundleGet"
      call ESMF_Test((loop_rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_FieldBundleAdd(bundle5, (/ft/), multiflag=.true., rc=rc)
      write(failMsg, *) "Adding a Field to a FieldBundle"
      write(name, *) "Adding a Field to a FieldBundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_FieldBundleGet(bundle5, fieldList=fields(1:3), &
        itemorderflag=ESMF_ITEMORDER_ADDORDER, rc=rc)
      write(failMsg, *) "Getting Fields from a FieldBundle"
      write(name, *) "Getting Fields from a FieldBundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_FieldBundleGet(bundle5, fieldList=fields(4:6), &
        itemorderflag=ESMF_ITEMORDER_ABC, rc=rc)
      write(failMsg, *) "Getting Fields from a FieldBundle"
      write(name, *) "Getting Fields from a FieldBundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      loop_rc = ESMF_SUCCESS
      do i = 1,3
        call ESMF_FieldGet(fields(i), name=fnames(i), rc=rc)
        if(rc /= ESMF_SUCCESS) loop_rc=ESMF_FAILURE
        call ESMF_FieldGet(fields(i+3), name=fnames(i+3), rc=rc)
        if(rc /= ESMF_SUCCESS) loop_rc=ESMF_FAILURE
        !print *, 'n_match = ', fnames(i), fnames(i+3)
      enddo
      write(failMsg, *) "Getting Names from Fields"
      write(name, *) "Getting Names from Fields"
      call ESMF_Test((loop_rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! The two retrieved name lists are expected to be different.
      ! TPT PTT
      n_match = 0
      do i = 1,3 
        if(trim(fnames(i)) == trim(fnames(i+3))) n_match = n_match + 1
        !print *, 'n_match = ', trim(fnames(i)), trim(fnames(i+3)), n_match
      enddo
      write(failMsg, *) "Comparing two modes of BundleGet"
      write(name, *) "Comparing two modes of BundleGet"
      call ESMF_Test((n_match .eq. 1), name, failMsg, result, ESMF_SRCLINE)


      ! Serialization and deserialization - these are internal methods that are
      ! NOT part of the ESMF API.

      !------------------------------------------------------------------------
      !EX_UTest
      ! Serialization test, inquire only
      allocate (buffer(1))
      buffer_len = 1
      offset_inq = 0
      call ESMF_FieldBundleSerialize (bundle5,  &
          buffer, buffer_len, offset_inq,  &
          ESMF_ATTRECONCILE_ON, ESMF_INQUIREONLY,  &
          rc=rc)
      write(name, *) "Test serialization, inquire only"
      write(failMsg, *) "Serialization inquiry failed"
      call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Serialization test
      deallocate (buffer)
      allocate (buffer(offset_inq))
      buffer_len = offset_inq
      offset = 0
      call ESMF_FieldBundleSerialize (bundle5,  &
          buffer, buffer_len, offset,  &
          ESMF_ATTRECONCILE_ON, ESMF_NOINQUIRE,  &
          rc=rc)
      write(name, *) "Test serialization for real"
      write(failMsg, *) "Serialization failed"
      call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

      print *, 'offset_inq, offset =', offset_inq, offset

      !------------------------------------------------------------------------
      !EX_UTest
      Call ESMF_FieldDestroy(ft, rc=rc)
      Call ESMF_FieldDestroy(fp, rc=rc)
      call ESMF_FieldBundleDestroy(bundle5, rc=rc)
      write(failMsg, *) "Destroy FieldBundle"
      write(name, *) "Destroy FieldBundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#endif

      call ESMF_TestEnd(ESMF_SRCLINE)

      end program ESMF_FieldBundleUTest
