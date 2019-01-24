! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

     program FieldRedistEx

!-------------------------------------------------------------------------
!ESMF_MULTI_PROC_EXAMPLE        String used by test script to count examples.
!==============================================================================
!
! !PROGRAM: ESMF_FieldRedistEx - Field Redistribution
!     
! !DESCRIPTION:
!     
! This program shows examples of Field interfaces for redistribution of data.
!-----------------------------------------------------------------------------
#include "ESMF.h"
#include "ESMF_Macros.inc"
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldRedistEx"
     ! ESMF Framework module
     use ESMF
     use ESMF_TestMod
     implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
    '$Id$'
!------------------------------------------------------------------------------

    ! Local variables
    integer :: rc, finalrc, result
    type(ESMF_Field)                            :: srcField, dstField
    type(ESMF_Field)                            :: srcFieldA, dstFieldA
    type(ESMF_Grid)                             :: grid
    type(ESMF_DistGrid)                         :: distgrid
    type(ESMF_VM)                               :: vm
    type(ESMF_RouteHandle)                      :: routehandle
    type(ESMF_Array)                            :: srcArray, dstArray
    type(ESMF_ArraySpec)                        :: arrayspec
    integer                                     :: localrc, localPet, i

    integer, pointer                            :: fptr(:)

    integer, pointer                            :: srcfptr(:), dstfptr(:)
    type(ESMF_Mesh)                             :: mesh

    integer, pointer :: nodeIds(:),nodeOwners(:)
    real(ESMF_KIND_R8), pointer :: nodeCoords(:)
    integer :: numNodes
    integer :: numElems
    integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)

    character(ESMF_MAXSTR) :: testname
    character(ESMF_MAXSTR) :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    write(failMsg, *) "Example failure"
    write(testname, *) "Example ESMF_FieldRedistEx"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------


    rc = ESMF_SUCCESS
    localrc = ESMF_SUCCESS
    finalrc = ESMF_SUCCESS
!------------------------------------------------------------------------------
    call ESMF_Initialize(defaultlogfilename="FieldRedistEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    if (.not. ESMF_TestMinPETs(4, ESMF_SRCLINE)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
!------------------------------------------------------------------------------
!BOE
! \subsubsection{Redistribute data from source Field to destination Field}
! \label{sec:field:usage:redist_1dptr}
!
! User can use {\tt ESMF\_FieldRedist} interface to redistribute data from 
! source Field to destination Field. This interface is overloaded by type and kind;
! In the version of {\tt ESMF\_FieldRedist} without factor argument, a default value
! of 1 is used.
! 
! \begin{sloppypar}
! In this example, we first create two 1D Fields, a source Field and a destination
! Field. Then we use {\tt ESMF\_FieldRedist} to
! redistribute data from source Field to destination Field.
! \end{sloppypar}
!EOE
!BOC 

    ! Get current VM and pet number
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_VMGet(vm, localPet=localPet, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! create grid
    distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/16/), &
            regDecomp=(/4/), &
            rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    grid = ESMF_GridCreate(distgrid=distgrid, &
        name="grid", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! create srcField
    ! +--------+--------+--------+--------+
    !      0        1        2        3            ! value
    ! 1        4        8        12       16       ! bounds
    srcField = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_I4, &
      indexflag=ESMF_INDEX_DELOCAL, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_FieldGet(srcField, farrayPtr=srcfptr, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    srcfptr(:) = localPet

    ! create dstField
    ! +--------+--------+--------+--------+
    !      0        0        0        0            ! value
    ! 1        4        8        12       16       ! bounds
    dstField = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_I4, &
      indexflag=ESMF_INDEX_DELOCAL, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_FieldGet(dstField, farrayPtr=dstfptr, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
    dstfptr(:) = 0

    ! perform redist
    ! 1. setup routehandle from source Field to destination Field
    call ESMF_FieldRedistStore(srcField, dstField, routehandle, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! 2. use precomputed routehandle to redistribute data
    call ESMF_FieldRedist(srcfield, dstField, routehandle, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! verify redist
    call ESMF_FieldGet(dstField, localDe=0, farrayPtr=fptr, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Verify that the redistributed data in dstField is correct.
    ! Before the redist op, the dst Field contains all 0. 
    ! The redist op reset the values to the PE value, verify this is the case.
    do i = lbound(fptr, 1), ubound(fptr, 1)
        if(fptr(i) .ne. localPet) localrc = ESMF_FAILURE
    enddo
    if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!EOC

    ! destroy all objects created in this example to prevent memory leak
    call ESMF_FieldDestroy(srcField, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_FieldDestroy(dstField, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Field redistribution can also be performed between different Field pairs that
! match the original Fields in {\em type}, {\em kind}, and memory layout of the
! {\em gridded} dimensions. However, the size, number, and index order of 
! {\em ungridded} dimensions may be different. See section \ref{RH:Reusability}
! for a more detailed discussion of RouteHandle reusability.
!EOE
!BOC
    call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_I4, rank=2, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Create two fields with ungridded dimensions using the Grid created previously.
! The new Field pair has matching number of elements. The ungridded dimension
! is mapped to the first dimension of either Field.
!EOE
!BOC
    srcFieldA = ESMF_FieldCreate(grid, arrayspec, gridToFieldMap=(/2/), &
        ungriddedLBound=(/1/), ungriddedUBound=(/10/), rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    dstFieldA = ESMF_FieldCreate(grid, arrayspec, gridToFieldMap=(/2/), &
        ungriddedLBound=(/1/), ungriddedUBound=(/10/), rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Using the previously computed routehandle, the Fields can be redistributed.
!EOE
!BOC
    call ESMF_FieldRedist(srcfieldA, dstFieldA, routehandle, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
    call ESMF_FieldRedistRelease(routehandle, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_FieldDestroy(srcFieldA, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_FieldDestroy(dstFieldA, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridDestroy(grid, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!------------------------------------------------------------------------------
!BOE
! \subsubsection{FieldRedist as a form of scatter involving arbitrary distribution}
! \label{sec:field:usage:redist_scattering}
!
! User can use {\tt ESMF\_FieldRedist} interface to redistribute data from 
! source Field to destination Field, where the destination Field is built on
! an arbitrarily distributed structure, e.g. {\tt ESMF\_Mesh}. The underlying mechanism is explained
! in section \ref{Array:ScatterGatherRevisited}.
!
! In this example, we will create 2 one dimensional Fields, the src Field has a regular decomposition
! and holds all its data on a single PET, in this case PET 0. The destination Field is built on a Mesh
! which is itself built on an arbitrarily distributed distgrid. Then we use {\tt ESMF\_FieldRedist} to
! redistribute data from source Field to destination Field, similar to a traditional scatter operation.
!
! The src Field only has data on PET 0 where it is sequentially initialized, i.e. 1,2,3...This data
! will be redistributed (or scattered) from PET 0 to the destination Field arbitrarily distributed on 
! all the PETs.
!EOE
!BOC
    ! a one dimensional grid whose elements are all located on PET 0
    distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/9/), &
        regDecomp=(/1/), &
        rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    grid = ESMF_GridCreate(distgrid=distgrid, &
        indexflag=ESMF_INDEX_DELOCAL, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    srcField = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_I4, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! initialize the source data
    if (localPet == 0) then
        call ESMF_FieldGet(srcField, farrayPtr=srcfptr, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        do i = 1, 9
            srcfptr(i) = i
        enddo
    endif
!EOC
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

!BOE
! For more information on Mesh creation, user can refer to Mesh examples section or Field creation
! on Mesh example for more details.
!EOE
!BOC 
      ! Create Mesh structure
      mesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
             nodeIds=nodeIds, nodeCoords=nodeCoords, &
             nodeOwners=nodeOwners, elementIds=elemIds,&
             elementTypes=elemTypes, elementConn=elemConn, &
             rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!EOC
      ! deallocate node data
      deallocate(nodeIds)
      deallocate(nodeCoords)
      deallocate(nodeOwners)

      ! deallocate elem data
      deallocate(elemIds)
      deallocate(elemTypes)
      deallocate(elemConn)
!BOE
! Create the destination Field on the Mesh that is arbitrarily distributed on 
! all the PETs.
!EOE
!BOC
      dstField = ESMF_FieldCreate(mesh, typekind=ESMF_TYPEKIND_I4, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!EOC

!BOE
! Perform the redistribution from source Field to destination Field.
!EOE
!BOC
     call ESMF_FieldRedistStore(srcField, dstField, &
             routehandle=routehandle, rc=rc)
     if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
     call ESMF_FieldRedist(srcField, dstField, routehandle=routehandle, rc=rc)
     if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!EOC
!BOE
! We can now verify that the sequentially initialized source data is scattered
! on to the destination Field. The data has been scattered onto the destination
! Field with the following distribution.
!\begin{verbatim}
!
! 4 elements on PET 0:  1 2 4 5
! 2 elements on PET 1:  3 6
! 2 elements on PET 2:  7 8
! 1 element  on PET 3:  9
!
!\end{verbatim}
! Because the redistribution is index based, the elements also corresponds to the
! index space of Mesh in the destination Field.
!EOE
!BOC
    call ESMF_FieldGet(dstField, farrayPtr=dstfptr, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!EOC
!BOE
! The scatter operation is successful. Since the routehandle computed with
! {\tt ESMF\_FieldRedistStore} can be reused, user can use the same routehandle
! to scatter multiple source Fields from a single PET to multiple destination
! Fields distributed on all PETs. The {\tt gathering} operation is just the 
! opposite of the demonstrated {\tt scattering} operation, where a user would
! redist from a source Field distributed on multiple PETs to a destination Field
! that only has data storage on a single PET.
!
! Now it's time to release all the resources.
!EOE
!BOC
    call ESMF_FieldRedistRelease(routehandle=routehandle, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!------------------------------------------------------------------------------
!BOE
! \subsubsection{FieldRedist as a form of gather involving arbitrary distribution}
! \label{sec:field:usage:redist_gathering}
!
! Similarly, one can use the same approach to gather the data from an arbitrary distribution
! to a non-arbitrary distribution. This concept is demonstrated by using the previous Fields but 
! the data operation is reversed. This time data is gathered from the Field built on the mesh to the Field
! that has only data allocation on rootPet.
!
!EOE

!BOE
! First a FieldRedist routehandle is created from the Field built on Mesh to the Field
! that has only data allocation on rootPet.
!EOE
!BOC
    call ESMF_FieldRedistStore(dstField, srcField, routehandle=routehandle, &
         rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Perform FieldRedist, this will gather the data points from the Field built on mesh to
! the data pointer on the rootPet (default to 0) stored in the srcField.
!EOE
!BOC
    call ESMF_FieldRedist(dstField, srcField, routehandle=routehandle, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Release the routehandle used for the gather operation.
!EOE
!BOC
    call ESMF_FieldRedistRelease(routehandle=routehandle, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_FieldDestroy(srcField, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_FieldDestroy(dstField, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_MeshDestroy(mesh, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_DistGridDestroy(distgrid, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
    ! file that the scripts grep for.
    call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)



    call ESMF_Finalize(rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE


     if (finalrc.EQ.ESMF_SUCCESS) then
       print *, "PASS: ESMF_FieldRedistEx.F90"
     else
       print *, "FAIL: ESMF_FieldRedistEx.F90"
     end if

    end program FieldRedistEx
