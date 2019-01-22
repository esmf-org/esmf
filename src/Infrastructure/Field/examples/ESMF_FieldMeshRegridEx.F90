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
!
program ESMF_MeshEx

!==============================================================================
!ESMF_MULTI_PROC_EXAMPLE        String used by test script to count examples.
!==============================================================================
#include "ESMF.h"
#include "ESMF_Macros.inc"

! !USES:
  use ESMF
  use ESMF_TestMod     ! test methods
  use ESMF_MeshMod
  use ESMF_RegridMod
  use ESMF_FieldMod
  use ESMF_GridUtilMod

  use ESMF_FieldGetMod

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id$'
!------------------------------------------------------------------------------
    
  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test failure message
  character(ESMF_MAXSTR) :: name, failMsg

  !-----------------------------------------------------------------------------
  !NEX_Ex
  
  type(ESMF_Mesh) :: dstMesh
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: fptrXC(:,:), fptr1D(:)
  real(ESMF_KIND_R8), pointer :: fptrYC(:,:)
  real(ESMF_KIND_R8), pointer :: fptr(:,:),fptr2(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2), i
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  real(ESMF_KIND_R8) :: dx,dy
  
  real(ESMF_KIND_R8) :: x,y
  
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  integer, pointer :: srcNodeIds(:),srcNodeOwners(:)
  real(ESMF_KIND_R8), pointer :: srcNodeCoords(:)
  integer, pointer :: srcElemIds(:),srcElemTypes(:),srcElemConn(:)
  integer :: srcNumNodes, srcNumElems
  integer :: srcNumQuadElems,srcNumTriElems, srcNumTotElems

  integer, pointer :: dstNodeIds(:),dstNodeOwners(:)
  real(ESMF_KIND_R8), pointer :: dstNodeCoords(:)
  integer, pointer :: dstElemIds(:),dstElemTypes(:),dstElemConn(:)
  integer :: dstNumNodes, dstNumElems
  integer :: dstNumQuadElems,dstNumTriElems, dstNumTotElems
  integer :: spatialDim, numOwnedNodes
  real(ESMF_KIND_R8), pointer :: ownedNodeCoords(:)


  ! result code
  integer :: finalrc, rc, localrc

  character(ESMF_MAXSTR) :: testname

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_FieldMeshRegridEx"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------


  finalrc = ESMF_SUCCESS
  call ESMF_Initialize(vm=vm, defaultlogfilename="FieldMeshRegridEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


  ! Only do this in serial to make Mesh creation easier
  if ((petCount .eq. 1) .or. (petCount .eq. 4)) then

  !!!!!!!!!!!!!!!!!! SETUP SRC MESH !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  if (petCount .eq. 1) then
  ! Set number of nodes
  srcNumNodes=9

  ! Allocate and fill the node id array.
  allocate(srcNodeIds(srcNumNodes))
  srcNodeIds=(/1,2,3,4,5,6,7,8,9/) 

  ! Allocate and fill node coordinate array.
  ! Since this is a 2D Mesh the size is 2x the
  ! number of nodes.
  allocate(srcNodeCoords(2*srcNumNodes))
  srcNodeCoords=(/-0.1,-0.1, & ! node id 1
                1.0,-0.1, & ! node id 2
                2.1,-0.1, & ! node id 3
               -0.1, 1.0, & ! node id 4
                1.0, 1.0, & ! node id 5
                2.1, 1.0, & ! node id 6
               -0.1, 2.1, & ! node id 7
                1.0, 2.1, & ! node id 8
                2.1, 2.1 /) ! node id 9

  ! Allocate and fill the node owner array.
  ! Since this Mesh is all on PET 0, it's just set to all 0.
  allocate(srcNodeOwners(srcNumNodes))
  srcNodeOwners=0 ! everything on PET 0

  ! Set the number of each type of element, plus the total number.
  srcNumQuadElems=3
  srcNumTriElems=2
  srcNumTotElems=srcNumQuadElems+srcNumTriElems

  ! Allocate and fill the element id array.
  allocate(srcElemIds(srcNumTotElems))
  srcElemIds=(/1,2,3,4,5/) 


  ! Allocate and fill the element topology type array.
  allocate(srcElemTypes(srcNumTotElems))
  srcElemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! elem id 1
              ESMF_MESHELEMTYPE_TRI,  & ! elem id 2
              ESMF_MESHELEMTYPE_TRI,  & ! elem id 3
              ESMF_MESHELEMTYPE_QUAD, & ! elem id 4
              ESMF_MESHELEMTYPE_QUAD/)  ! elem id 5


  ! Allocate and fill the element connection type array.
  ! Note that entries in this array refer to the 
  ! positions in the srcNodeIds, etc. arrays and that
  ! the order and number of entries for each element
  ! reflects that given in the Mesh options 
  ! section for the corresponding entry
  ! in the srcElemTypes array.
  allocate(srcElemConn(4*srcNumQuadElems+3*srcNumTriElems))
  srcElemConn=(/1,2,5,4, &  ! elem id 1
             2,3,5,   &  ! elem id 2
             3,6,5,   &  ! elem id 3
             4,5,8,7, &  ! elem id 4
             5,6,9,8/)   ! elem id 5

 else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
    if (localPET .eq. 0) then !!! This part only for PET 0
       ! Set number of nodes
       srcnumNodes=4

       ! Allocate and fill the node id array.
       allocate(srcnodeIds(srcnumNodes))
       srcnodeIds=(/1,2,4,5/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(srcnodeCoords(2*srcnumNodes))
       srcnodeCoords=(/-0.1, -0.1, & ! node id 1
                     1.0, -0.1, & ! node id 2
                    -0.1,  1.0, & ! node id 4
                     1.0,  1.0 /) ! node id 5

       ! Allocate and fill the node owner array.
       allocate(srcnodeOwners(srcnumNodes))
       srcnodeOwners=(/0, & ! node id 1
                    0, & ! node id 2
                    0, & ! node id 4
                    0/)  ! node id 5

       ! Set the number of each type of element, plus the total number.
       srcnumQuadElems=1
       srcnumTriElems=0
       srcnumTotElems=srcnumQuadElems+srcnumTriElems

       ! Allocate and fill the element id array.
       allocate(srcelemIds(srcnumTotElems))
       srcelemIds=(/1/) 

       ! Allocate and fill the element topology type array.
       allocate(srcelemTypes(srcnumTotElems))
       srcelemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 1

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(srcelemConn(4*srcnumQuadElems+3*srcnumTriElems))
       srcelemConn=(/1,2,4,3/) ! elem id 1

     else if (localPET .eq. 1) then !!! This part only for PET 1
       ! Set number of nodes
       srcnumNodes=4

       ! Allocate and fill the node id array.
       allocate(srcnodeIds(srcnumNodes))
       srcnodeIds=(/2,3,5,6/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(srcnodeCoords(2*srcnumNodes))
       srcnodeCoords=(/1.0,-0.1, & ! node id 2
                    2.1,-0.1, & ! node id 3
                    1.0, 1.0, & ! node id 5
                    2.1, 1.0 /) ! node id 6

       ! Allocate and fill the node owner array.
       allocate(srcnodeOwners(srcnumNodes))
       srcnodeOwners=(/0, & ! node id 2
                    1, & ! node id 3
                    0, & ! node id 5
                    1/)  ! node id 6

       ! Set the number of each type of element, plus the total number.
       srcnumQuadElems=0
       srcnumTriElems=2
       srcnumTotElems=srcnumQuadElems+srcnumTriElems

       ! Allocate and fill the element id array.
       allocate(srcelemIds(srcnumTotElems))
       srcelemIds=(/2,3/) 

       ! Allocate and fill the element topology type array.
       allocate(srcelemTypes(srcnumTotElems))
       srcelemTypes=(/ESMF_MESHELEMTYPE_TRI, & ! elem id 2
                   ESMF_MESHELEMTYPE_TRI/)  ! elem id 3

       ! Allocate and fill the element connection type array.
       allocate(srcelemConn(4*srcnumQuadElems+3*srcnumTriElems))
       srcelemConn=(/1,2,3, & ! elem id 2
                  2,4,3/)  ! elem id 3

    else if (localPET .eq. 2) then !!! This part only for PET 2
        ! Set number of nodes
        srcnumNodes=4

        ! Allocate and fill the node id array.
        allocate(srcnodeIds(srcnumNodes))
        srcnodeIds=(/4,5,7,8/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(srcnodeCoords(2*srcnumNodes))
        srcnodeCoords=(/-0.1,1.0, & ! node id 4
                      1.0,1.0, & ! node id 5
                     -0.1,2.1, & ! node id 7
                      1.0,2.1 /) ! node id 8

        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
        allocate(srcnodeOwners(srcnumNodes))
        srcnodeOwners=(/0, & ! node id 4
                     0, & ! node id 5
                     2, & ! node id 7
                     2/)  ! node id 8

        ! Set the number of each type of element, plus the total number.
        srcnumQuadElems=1
        srcnumTriElems=0
        srcnumTotElems=srcnumQuadElems+srcnumTriElems

        ! Allocate and fill the element id array.
        allocate(srcelemIds(srcnumTotElems))
        srcelemIds=(/4/) 

        ! Allocate and fill the element topology type array.
        allocate(srcelemTypes(srcnumTotElems))
        srcelemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 4

        ! Allocate and fill the element connection type array.
        allocate(srcelemConn(4*srcnumQuadElems+3*srcnumTriElems))
        srcelemConn=(/1,2,4,3/) ! elem id 4

     else if (localPET .eq. 3) then !!! This part only for PET 3
        ! Set number of nodes
        srcnumNodes=4

        ! Allocate and fill the node id array.
        allocate(srcnodeIds(srcnumNodes))
        srcnodeIds=(/5,6,8,9/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(srcnodeCoords(2*srcnumNodes))
        srcnodeCoords=(/1.0,1.0, &  ! node id 5
                     2.1,1.0, &  ! node id 6
                     1.0,2.1, &  ! node id 8
                     2.1,2.1 /)  ! node id 9

        ! Allocate and fill the node owner array.
        allocate(srcnodeOwners(srcnumNodes))
        srcnodeOwners=(/0, & ! node id 5
                     1, & ! node id 6
                     2, & ! node id 8
                     3/)  ! node id 9
 
        ! Set the number of each type of element, plus the total number.
        srcnumQuadElems=1
        srcnumTriElems=0
        srcnumTotElems=srcnumQuadElems+srcnumTriElems

        ! Allocate and fill the element id array.
        allocate(srcelemIds(srcnumTotElems))
        srcelemIds=(/5/)  

        ! Allocate and fill the element topology type array.
        allocate(srcelemTypes(srcnumTotElems))
        srcelemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 5

        ! Allocate and fill the element connection type array.
        allocate(srcelemConn(4*srcnumQuadElems+3*srcnumTriElems))
        srcelemConn=(/1,2,4,3/) ! elem id 5
       endif
    endif


  !!!!!!!!!!!!!!!!!! SETUP DST MESH !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (petCount .eq. 1) then
  ! Set number of nodes
  dstNumNodes=9

  ! Allocate and fill the node id array.
  allocate(dstNodeIds(dstNumNodes))
  dstNodeIds=(/1,2,3,4,5,6,7,8,9/) 

  ! Allocate and fill node coordinate array.
  ! Since this is a 2D Mesh the size is 2x the
  ! number of nodes.
  allocate(dstNodeCoords(2*dstNumNodes))
  dstNodeCoords=(/0.0,0.0, & ! node id 1
               1.0,0.0, & ! node id 2
               2.0,0.0, & ! node id 3
               0.0,1.0, & ! node id 4
               1.0,1.0, & ! node id 5
               2.0,1.0, & ! node id 6
               0.0,2.0, & ! node id 7
               1.0,2.0, & ! node id 8
               2.0,2.0 /) ! node id 9


  ! Allocate and fill the node owner array.
  ! Since this Mesh is all on PET 0, it's just set to all 0.
  allocate(dstNodeOwners(dstNumNodes))
  dstNodeOwners=0 ! everything on PET 0

  ! Set the number of each type of element, plus the total number.
  dstNumQuadElems=3
  dstNumTriElems=2
  dstNumTotElems=dstNumQuadElems+dstNumTriElems

  ! Allocate and fill the element id array.
  allocate(dstElemIds(dstNumTotElems))
  dstElemIds=(/1,2,3,4,5/) 


  ! Allocate and fill the element topology type array.
  allocate(dstElemTypes(dstNumTotElems))
  dstElemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! elem id 1
              ESMF_MESHELEMTYPE_TRI,  & ! elem id 2
              ESMF_MESHELEMTYPE_TRI,  & ! elem id 3
              ESMF_MESHELEMTYPE_QUAD, & ! elem id 4
              ESMF_MESHELEMTYPE_QUAD/)  ! elem id 5


  ! Allocate and fill the element connection type array.
  ! Note that entries in this array refer to the 
  ! positions in the dstNodeIds, etc. arrays and that
  ! the order and number of entries for each element
  ! reflects that given in the Mesh options 
  ! section for the corresponding entry
  ! in the dstElemTypes array.
  allocate(dstElemConn(4*dstNumQuadElems+3*dstNumTriElems))
  dstElemConn=(/1,2,5,4, &  ! elem id 1
             2,3,5,   &  ! elem id 2
             3,6,5,   &  ! elem id 3
             4,5,8,7, &  ! elem id 4
             5,6,9,8/)   ! elem id 5
 else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
    if (localPET .eq. 0) then !!! This part only for PET 0
       ! Set number of nodes
       dstnumNodes=4

       ! Allocate and fill the node id array.
       allocate(dstnodeIds(dstnumNodes))
       dstnodeIds=(/1,2,4,5/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(dstnodeCoords(2*dstnumNodes))
       dstnodeCoords=(/0.0, 0.0, & ! node id 1
                     1.0, 0.0, & ! node id 2
                    0.0,  1.0, & ! node id 4
                     1.0,  1.0 /) ! node id 5

       ! Allocate and fill the node owner array.
       allocate(dstnodeOwners(dstnumNodes))
       dstnodeOwners=(/0, & ! node id 1
                    0, & ! node id 2
                    0, & ! node id 4
                    0/)  ! node id 5

       ! Set the number of each type of element, plus the total number.
       dstnumQuadElems=1
       dstnumTriElems=0
       dstnumTotElems=dstnumQuadElems+dstnumTriElems

       ! Allocate and fill the element id array.
       allocate(dstelemIds(dstnumTotElems))
       dstelemIds=(/1/) 

       ! Allocate and fill the element topology type array.
       allocate(dstelemTypes(dstnumTotElems))
       dstelemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 1

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(dstelemConn(4*dstnumQuadElems+3*dstnumTriElems))
       dstelemConn=(/1,2,4,3/) ! elem id 1

     else if (localPET .eq. 1) then !!! This part only for PET 1
       ! Set number of nodes
       dstnumNodes=4

       ! Allocate and fill the node id array.
       allocate(dstnodeIds(dstnumNodes))
       dstnodeIds=(/2,3,5,6/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(dstnodeCoords(2*dstnumNodes))
       dstnodeCoords=(/1.0,0.0, & ! node id 2
                    2.0,0.0, & ! node id 3
                    1.0, 1.0, & ! node id 5
                    2.0, 1.0 /) ! node id 6

       ! Allocate and fill the node owner array.
       allocate(dstnodeOwners(dstnumNodes))
       dstnodeOwners=(/0, & ! node id 2
                    1, & ! node id 3
                    0, & ! node id 5
                    1/)  ! node id 6

       ! Set the number of each type of element, plus the total number.
       dstnumQuadElems=0
       dstnumTriElems=2
       dstnumTotElems=dstnumQuadElems+dstnumTriElems

       ! Allocate and fill the element id array.
       allocate(dstelemIds(dstnumTotElems))
       dstelemIds=(/2,3/) 

       ! Allocate and fill the element topology type array.
       allocate(dstelemTypes(dstnumTotElems))
       dstelemTypes=(/ESMF_MESHELEMTYPE_TRI, & ! elem id 2
                   ESMF_MESHELEMTYPE_TRI/)  ! elem id 3

       ! Allocate and fill the element connection type array.
       allocate(dstelemConn(4*dstnumQuadElems+3*dstnumTriElems))
       dstelemConn=(/1,2,3, & ! elem id 2
                  2,4,3/)  ! elem id 3

    else if (localPET .eq. 2) then !!! This part only for PET 2
        ! Set number of nodes
        dstnumNodes=4

        ! Allocate and fill the node id array.
        allocate(dstnodeIds(dstnumNodes))
        dstnodeIds=(/4,5,7,8/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(dstnodeCoords(2*dstnumNodes))
        dstnodeCoords=(/0.0,1.0, & ! node id 4
                      1.0,1.0, & ! node id 5
                      0.0,2.0, & ! node id 7
                      1.0,2.0 /) ! node id 8

        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
        allocate(dstnodeOwners(dstnumNodes))
        dstnodeOwners=(/0, & ! node id 4
                     0, & ! node id 5
                     2, & ! node id 7
                     2/)  ! node id 8

        ! Set the number of each type of element, plus the total number.
        dstnumQuadElems=1
        dstnumTriElems=0
        dstnumTotElems=dstnumQuadElems+dstnumTriElems

        ! Allocate and fill the element id array.
        allocate(dstelemIds(dstnumTotElems))
        dstelemIds=(/4/) 

        ! Allocate and fill the element topology type array.
        allocate(dstelemTypes(dstnumTotElems))
        dstelemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 4

        ! Allocate and fill the element connection type array.
        allocate(dstelemConn(4*dstnumQuadElems+3*dstnumTriElems))
        dstelemConn=(/1,2,4,3/) ! elem id 4

     else if (localPET .eq. 3) then !!! This part only for PET 3
        ! Set number of nodes
        dstnumNodes=4

        ! Allocate and fill the node id array.
        allocate(dstnodeIds(dstnumNodes))
        dstnodeIds=(/5,6,8,9/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(dstnodeCoords(2*dstnumNodes))
        dstnodeCoords=(/1.0,1.0, &  ! node id 5
                     2.0,1.0, &  ! node id 6
                     1.0,2.0, &  ! node id 8
                     2.0,2.0 /)  ! node id 9

        ! Allocate and fill the node owner array.
        allocate(dstnodeOwners(dstnumNodes))
        dstnodeOwners=(/0, & ! node id 5
                     1, & ! node id 6
                     2, & ! node id 8
                     3/)  ! node id 9
 
        ! Set the number of each type of element, plus the total number.
        dstnumQuadElems=1
        dstnumTriElems=0
        dstnumTotElems=dstnumQuadElems+dstnumTriElems

        ! Allocate and fill the element id array.
        allocate(dstelemIds(dstnumTotElems))
        dstelemIds=(/5/)  

        ! Allocate and fill the element topology type array.
        allocate(dstelemTypes(dstnumTotElems))
        dstelemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 5

        ! Allocate and fill the element connection type array.
        allocate(dstelemConn(4*dstnumQuadElems+3*dstnumTriElems))
        dstelemConn=(/1,2,4,3/) ! elem id 5
       endif
    endif


!BOE
!\subsubsection{Field regrid example: Mesh to Mesh}
! This example demonstrates the regridding process between Fields created on Meshes. First
! the Meshes are created. This example omits the setup of the arrays describing the Mesh, but please see
! Section~\ref{sec:mesh:usage:meshCreation} for examples of this. After creation Fields are constructed on the Meshes, 
! and then ESMF\_FieldRegridStore() is called to construct a RouteHandle implementing the regrid operation. Finally, ESMF\_FieldRegrid() is
! called with the Fields and the RouteHandle to do the interpolation between the source Field and 
! destination Field.  Note the coordinates of the source and destination Mesh should be in degrees.
! 
!EOE

!BOC

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Create Source Mesh
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Create the Mesh structure.
  ! For brevity's sake, the code to fill the Mesh creation 
  ! arrays is omitted from this example. However, here
  ! is a brief description of the arrays:
  ! srcNodeIds    - the global ids for the src nodes
  ! srcNodeCoords - the coordinates for the src nodes
  ! srcNodeOwners - which PET owns each src node
  ! srcElemIds    - the global ids of the src elements
  ! srcElemTypes  - the topological shape of each src element
  ! srcElemConn   - how to connect the nodes to form the elements
  !                 in the source mesh
  ! Several examples of setting up these arrays can be seen in
  ! the Mesh Section "Mesh Creation". 
  srcMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
         nodeIds=srcNodeIds, nodeCoords=srcNodeCoords, &
         nodeOwners=srcNodeOwners, elementIds=srcElemIds,&
         elementTypes=srcElemTypes, elementConn=srcElemConn, rc=rc)

  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Create and Fill Source Field
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Set description of source Field
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Create source Field
  srcField = ESMF_FieldCreate(srcMesh, arrayspec, &
                        name="source", rc=rc)

  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Get source Field data pointer to put data into
  call ESMF_FieldGet(srcField, 0, fptr1D,  rc=rc)

  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Get number of local nodes to allocate space
  ! to hold local node coordinates
  call ESMF_MeshGet(srcMesh, &
         numOwnedNodes=numOwnedNodes, rc=rc)

  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Allocate space to hold local node coordinates
  ! (spatial dimension of Mesh*number of local nodes)
  allocate(ownedNodeCoords(2*numOwnedNodes))

  ! Get local node coordinates
  call ESMF_MeshGet(srcMesh, &
         ownedNodeCoords=ownedNodeCoords, rc=rc)

  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Set the source Field to the function 20.0+x+y
  do i=1,numOwnedNodes
    ! Get coordinates
    x=ownedNodeCoords(2*i-1)
    y=ownedNodeCoords(2*i)

   ! Set source function
   fptr1D(i) = 20.0+x+y
  enddo

  ! Deallocate local node coordinates
  deallocate(ownedNodeCoords)


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Create Destination Mesh
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Create the Mesh structure.
  ! For brevity's sake, the code to fill the Mesh creation 
  ! arrays is omitted from this example. However, here
  ! is a brief description of the arrays:
  ! dstNodeIds    - the global ids for the dst nodes
  ! dstNodeCoords - the coordinates for the dst nodes
  ! dstNodeOwners - which PET owns each dst node
  ! dstElemIds    - the global ids of the dst elements
  ! dstElemTypes  - the topological shape of each dst element
  ! dstElemConn   - how to connect the nodes to form the elements
  !                 in the destination mesh
  ! Several examples of setting up these arrays can be seen in
  ! the Mesh Section "Mesh Creation". 
  dstMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
         nodeIds=dstNodeIds, nodeCoords=dstNodeCoords, &
         nodeOwners=dstNodeOwners, elementIds=dstElemIds,&
         elementTypes=dstElemTypes, elementConn=dstElemConn, rc=rc)

  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Create Destination Field
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Set description of source Field
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Create destination Field
  dstField = ESMF_FieldCreate(dstMesh, arrayspec, &
                        name="destination", rc=rc)

  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Do Regrid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Compute RouteHandle which contains the regrid operation
  call ESMF_FieldRegridStore( &
          srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          rc=rc)

  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Perform Regrid operation moving data from srcField to dstField
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=rc)


  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! dstField now contains the interpolated data.
  ! If the Meshes don't change, then routeHandle
  ! may be used repeatedly to interpolate from 
  ! srcField to dstField.  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   
  ! User code to use the routeHandle, Fields, and
  ! Meshes goes here before they are freed below.


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Free the objects created in the example.
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Free the RouteHandle
  call ESMF_FieldRegridRelease(routeHandle, rc=rc)

  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Free the Fields
  call ESMF_FieldDestroy(srcField, rc=rc)

  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldDestroy(dstField, rc=rc)

  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Free the Meshes
  call ESMF_MeshDestroy(dstMesh, rc=rc)

  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_MeshDestroy(srcMesh, rc=rc)
 
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !EOC

   ! Cleanup after example

   ! deallocate node data
   deallocate(srcNodeIds)
   deallocate(srcNodeCoords)
   deallocate(srcNodeOwners)

   ! deallocate elem data
   deallocate(srcElemIds)
   deallocate(srcElemTypes)
   deallocate(srcElemConn)


   ! deallocate node data
   deallocate(dstNodeIds)
   deallocate(dstNodeCoords)
   deallocate(dstNodeOwners)

   ! deallocate elem data
   deallocate(dstElemIds)
   deallocate(dstElemTypes)
   deallocate(dstElemConn)


  endif ! Only for petCount .eq. 1 


10   continue
  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((rc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

  call ESMF_Finalize(rc=rc)

  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_FieldMeshRegridEx.F90"
  else
    print *, "FAIL: ESMF_FieldMeshRegridEx.F90"
  endif



end program ESMF_MeshEx
