! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research,
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


  ! individual test result code
  integer :: finalrc, rc, petCount,localPet, localrc, result

  ! individual test failure message
  character(ESMF_MAXSTR) :: name

  logical :: correct
  type(ESMF_VM) :: vm
  type(ESMF_Mesh) :: mesh, mesh2
  type(ESMF_DistGrid) :: nodeDistgrid, elemDistgrid  

! The following arrays are used to declare the mesh to the ESMF framework.
  integer :: numNodes, numTotElems, numTriElems, numQuadElems
  integer, allocatable :: nodeIds(:)
  real(ESMF_KIND_R8), allocatable :: nodeCoords(:)
  integer, allocatable :: nodeOwners(:)

  integer, allocatable :: elemIds(:)
  integer, allocatable :: elemTypes(:)
  integer, allocatable :: elemConn(:)

  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_Field)  ::  field

  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_MeshEx"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------



  finalrc = ESMF_SUCCESS
  call  ESMF_Initialize(vm=vm, defaultlogfilename="ESMF_MeshEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)

  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  write(name, *) "Test GridToMesh"

  ! init success flag
  correct=.true.
  rc=ESMF_SUCCESS

!BOE
!
! This section describes the use of the ESMF Mesh class. It starts with an explanation and examples of 
! creating a Mesh and then goes through other Mesh methods. This set of sections covers the use of the 
! Mesh class interfaces, for further detail which applies to creating a Field on a Mesh, please see 
! Section~\ref{sec:field:usage:create_mesh_arrayspec}.
!
!\subsubsection{Mesh creation}
!\label{sec:mesh:usage:meshCreation}
!
! To create a Mesh we need to set some properties of the Mesh as a whole, some properties of each node in the mesh and 
! then some properties of each element which connects the nodes (for a definition of node and element please see 
! Section~\ref{sec:meshrep}).
!
! For the Mesh as a whole we set its parametric dimension ({\tt parametricDim}) and spatial dimension ({\tt spatialDim}). 
! A Meshes' parametric dimension can  be thought of as the dimension of the elements which make up the Mesh. 
! A Mesh's spatial dimension, on the other hand, is the is the number of coordinate dimensions needed to describe the location of 
! the nodes making up the Mesh. (For a fuller definition of these terms please see Section~\ref{sec:meshrep}.)
!
! The structure of the per node and element information used to create a Mesh is influenced by the Mesh distribution strategy. 
! The Mesh class is distributed by elements. This means that a node must be present on any PET that contains an element 
! associated with that node, but not on any other PET (a node can't be on a PET without an element ""home"). Since a node may be used
! by two or more elements located on different PETs, a node may be duplicated on multiple PETs. When a node is duplicated in this manner, 
! one and only one of the PETs that contain the node must "own" the node. The user sets this ownership when they define the nodes during Mesh creation.
! When a Field is created on a Mesh (i.e. on  the Mesh nodes), on each PET the Field is only created on the nodes which are owned by that PET.
! This means that the size of the Field memory on the PET can be smaller than the number of nodes used to create the Mesh on 
! that PET. Please see Section~\ref{sec:field:usage:create_mesh_arrayspec} in Field for further explanation and examples of this
! issue and others in working with Fields on Meshes. 
!
! \begin{sloppypar}
! For each node in the Mesh we set three properties: the global id of the node ({\tt nodeIds}), node coordinates 
! ({\tt nodeCoords}), and which PET owns the node ({\tt nodeOwners}). The node id is a unique (across all PETs) integer 
! attached to the particular node. It is used to indicate which nodes are the same when connecting together pieces of the 
! Mesh on different processors. The node coordinates indicate the location of a node in space and are used in the
! {\tt ESMF\_FieldRegrid()} functionality when interpolating. The node owner indicates which PET is in charge of the node. This
! is used when creating a Field on the Mesh to indicate which PET should contain a Field location for the data.  
! \end{sloppypar}
! 
! For each element in the Mesh we set three properties: the global id of the element ({\tt elementIds}), the topology type of
! the element ({\tt elementTypes}), and which nodes are connected together to form the element ({\tt elementConn}). The element id is
! a unique (across all PETs) integer attached to the particular element. The element type describes the topology of the element 
! (e.g. a triangle vs. a quadrilateral). The range of choices for the topology of the elements in a Mesh are restricted by the 
! Mesh's parametric dimension (e.g. a Mesh can't contain a 2D element like a triangle, when its parametric dimension is 3D), but it can contain
! any combination of elements appropriate to its dimension. In particular, in 2D ESMF supports two native element types triangle and quadrilateral, but
! also provides support for polygons with any number of sides. These polygons are represented internally as sets of triangles, but to the user
! should behave like other elements. To specify a polygon with more than four sides, the element type should be set to the number of corners of 
! the polygon (e.g. element type=6 for a hexagon). 
! The element connectivity indicates which nodes are to be connected together to
! form the element. The number of nodes connected together for each element is implied by the elements topology type ({\tt elementTypes}). 
! It is IMPORTANT to note, that the entries in this list are NOT the global ids of the nodes, but are indices into the PET local lists of
! node info used in the Mesh Create. In other words, the element connectivity isn't specified in terms of the global list of nodes, but instead
! is specified in terms of the locally described node info. One other important point about connectivities is that the order of the nodes in the 
! connectivity list of an element is important. Please see Section~\ref{const:meshelemtype} for diagrams illustrating the correct order of
! nodes in an element. In general, when specifying an element with parametric dimension 2, the nodes should be given in counter-clockwise order 
! around the element. 
!
! \begin{sloppypar}
! Mesh creation may either be performed as a one step process using the full {\tt ESMF\_MeshCreate()} call, or may be done in three steps. The
! three step process starts with a more minimal {\tt ESMF\_MeshCreate()} call. It is then followed by the {\tt ESMF\_MeshAddNodes()} to 
! specify nodes, and then the {\tt ESMF\_MeshAddElements()} call to specify elements. This three step sequence is useful to conserve memory
! because the node arrays being used for the {\tt ESMF\_MeshAddNodes()} call can be deallocated before creating the arrays to be used in the {\tt ESMF\_MeshAddElements()} call.
! \end{sloppypar}
!
!EOE

!BOE
!\subsubsection{Create a small single PET Mesh in one step}\label{sec:mesh:1pet1step}
!
!\begin{verbatim}
!
! 
!  2.0   7 ------- 8 ------- 9
!        |         |         |
!        |    4    |    5    |
!        |         |         |
!  1.0   4 ------- 5 ------- 6
!        |         |  \   3  |
!        |    1    |    \    |
!        |         |  2   \  |
!  0.0   1 ------- 2 ------- 3
!
!       0.0       1.0        2.0 
! 
!        Node Id labels at corners
!       Element Id labels in centers
!       (Everything owned by PET 0) 
!
!\end{verbatim}
!
! This example is intended to illustrate the creation of a small Mesh on one PET. The reason for starting with a single PET
! case is so that the user can start to familiarize themselves with the concepts of Mesh creation without the added complication of 
! multiple processors. Later examples illustrate the multiple processor case. This example creates the small 2D Mesh which can be 
! seen in the figure above. Note that this Mesh consists of 9 nodes and 5 elements, where the elements are a mixture of 
! quadrilaterals and triangles.  The coordinates of the nodes in the Mesh range from 0.0 to 2.0 in both dimensions. The node ids are 
! in the corners of the elements whereas the element ids are in the centers. The following section of code illustrates the creation of
! this Mesh. 
!
!EOE

  ! Only do this if we have 1 processor
  if (petCount .eq. 1) then

!BOC

  ! Set number of nodes
  numNodes=9

  ! Allocate and fill the node id array.
  allocate(nodeIds(numNodes))
  nodeIds=(/1,2,3,4,5,6,7,8,9/) 

  ! Allocate and fill node coordinate array.
  ! Since this is a 2D Mesh the size is 2x the
  ! number of nodes.
  allocate(nodeCoords(2*numNodes))
  nodeCoords=(/0.0,0.0, & ! node id 1
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
  allocate(nodeOwners(numNodes))
  nodeOwners=0 ! everything on PET 0


  ! Set the number of each type of element, plus the total number.
  numQuadElems=3
  numTriElems=2
  numTotElems=numQuadElems+numTriElems


  ! Allocate and fill the element id array.
  allocate(elemIds(numTotElems))
  elemIds=(/1,2,3,4,5/) 


  ! Allocate and fill the element topology type array.
  allocate(elemTypes(numTotElems))
  elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! elem id 1
              ESMF_MESHELEMTYPE_TRI,  & ! elem id 2
              ESMF_MESHELEMTYPE_TRI,  & ! elem id 3
              ESMF_MESHELEMTYPE_QUAD, & ! elem id 4
              ESMF_MESHELEMTYPE_QUAD/)  ! elem id 5


  ! Allocate and fill the element connection type array.
  ! Note that entries in this array refer to the 
  ! positions in the nodeIds, etc. arrays and that
  ! the order and number of entries for each element
  ! reflects that given in the Mesh options 
  ! section for the corresponding entry
  ! in the elemTypes array. The number of 
  ! entries in this elemConn array is the
  ! number of nodes in a quad. (4) times the 
  ! number of quad. elements plus the number
  ! of nodes in a triangle (3) times the number
  ! of triangle elements. 
  allocate(elemConn(4*numQuadElems+3*numTriElems))
  elemConn=(/1,2,5,4, &  ! elem id 1
             2,3,5,   &  ! elem id 2
             3,6,5,   &  ! elem id 3
             4,5,8,7, &  ! elem id 4
             5,6,9,8/)   ! elem id 5


  ! Create Mesh structure in 1 step
  mesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
         coordSys=ESMF_COORDSYS_CART, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, &
         rc=localrc)


  ! After the creation we are through with the arrays, so they may be
  ! deallocated.
  deallocate(nodeIds)
  deallocate(nodeCoords)
  deallocate(nodeOwners)
  deallocate(elemIds)
  deallocate(elemTypes)
  deallocate(elemConn)


  ! At this point the mesh is ready to use. For example, as is 
  ! illustrated here, to have a field created on it. Note that 
  ! the Field only contains data for nodes owned by the current PET.
  ! Please see Section "Create a Field from a Mesh" under Field
  ! for more information on creating a Field on a Mesh. 
  field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_R8,  rc=localrc)

!EOC

  ! Get rid of Field
  call ESMF_FieldDestroy(field, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get rid of Mesh
  call ESMF_MeshDestroy(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! endif for skip for >1 proc
  endif 


!BOE
!\subsubsection{Create a small single PET Mesh in three steps}\label{sec:mesh:1pet3step}
!
! This example is intended to illustrate the creation of a small Mesh in three steps on one PET. The Mesh being created is exactly
! the same one as in the last example (Section~\ref{sec:mesh:1pet1step}), but the three step process allows the creation to occur in 
! a more memory efficient manner. 
! 
!EOE

  ! Only do this if we have 1 processor
  if (petCount .eq. 1) then

!BOC

  ! Create the mesh structure setting the dimensions
  ! and coordinate system
  mesh = ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
                         coordSys=ESMF_COORDSYS_CART, &
                         rc=localrc)

  ! Set number of nodes
  numNodes=9

  ! Allocate and fill the node id array.
  allocate(nodeIds(numNodes))
  nodeIds=(/1,2,3,4,5,6,7,8,9/) 

  ! Allocate and fill node coordinate array.
  ! Since this is a 2D Mesh the size is 2x the
  ! number of nodes.
  allocate(nodeCoords(2*numNodes))
  nodeCoords=(/0.0,0.0, & ! node id 1
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
  allocate(nodeOwners(numNodes))
  nodeOwners=0 ! everything on PET 0

  ! Add the nodes to the Mesh
  call ESMF_MeshAddNodes(mesh, nodeIds=nodeIds, &
         nodeCoords=nodeCoords, nodeOwners=nodeOwners, rc=localrc)

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! HERE IS THE POINT OF THE THREE STEP METHOD
  ! WE CAN DELETE THESE NODE ARRAYS BEFORE 
  ! ALLOCATING THE ELEMENT ARRAYS, THEREBY
  ! REDUCING THE AMOUNT OF MEMORY NEEDED 
  ! AT ONE TIME. 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  deallocate(nodeIds)
  deallocate(nodeCoords)
  deallocate(nodeOwners)


  ! Set the number of each type of element, plus the total number.
  numQuadElems=3
  numTriElems=2
  numTotElems=numQuadElems+numTriElems

  ! Allocate and fill the element id array.
  allocate(elemIds(numTotElems))
  elemIds=(/1,2,3,4,5/) 

  ! Allocate and fill the element topology type array.
  allocate(elemTypes(numTotElems))
  elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! elem id 1
              ESMF_MESHELEMTYPE_TRI,  & ! elem id 2
              ESMF_MESHELEMTYPE_TRI,  & ! elem id 3
              ESMF_MESHELEMTYPE_QUAD, & ! elem id 4
              ESMF_MESHELEMTYPE_QUAD/)  ! elem id 5


  ! Allocate and fill the element connection type array.
  ! Note that entries in this array refer to the 
  ! positions in the nodeIds, etc. arrays and that
  ! the order and number of entries for each element
  ! reflects that given in the Mesh options 
  ! section for the corresponding entry
  ! in the elemTypes array. The number of 
  ! entries in this elemConn array is the
  ! number of nodes in a quad. (4) times the 
  ! number of quad. elements plus the number
  ! of nodes in a triangle (3) times the number
  ! of triangle elements. 
  allocate(elemConn(4*numQuadElems+3*numTriElems))
  elemConn=(/1,2,5,4, &  ! elem id 1
             2,3,5,   &  ! elem id 2
             3,6,5,   &  ! elem id 3
             4,5,8,7, &  ! elem id 4
             5,6,9,8/)   ! elem id 5


  ! Finish the creation of the Mesh by adding the elements
  call ESMF_MeshAddElements(mesh, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, &
         rc=localrc)

  ! After the creation we are through with the arrays, so they may be
  ! deallocated.
  deallocate(elemIds)
  deallocate(elemTypes)
  deallocate(elemConn)


  ! At this point the mesh is ready to use. For example, as is 
  ! illustrated here, to have a field created on it. Note that 
  ! the Field only contains data for nodes owned by the current PET.
  ! Please see Section "Create a Field from a Mesh" under Field
  ! for more information on creating a Field on a Mesh. 
  field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_R8,  rc=localrc)

!EOC

  ! Get rid of Field
  call ESMF_FieldDestroy(field, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get rid of Mesh
  call ESMF_MeshDestroy(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! endif for skip for >1 proc
  endif 

!BOE
!\subsubsection{Create a small Mesh on 4 PETs in one step}
!\label{sec:mesh:4pet1step}
!
!\begin{verbatim}
!
!  2.0   7 ------- 8        [8] ------ 9          
!        |         |         |         |
!        |    4    |         |    5    |
!        |         |         |         |
!  1.0  [4] ----- [5]       [5] ----- [6]
!        
!       0.0       1.0       1.0       2.0
!
!           PET 2               PET 3
!
!
!  1.0   4 ------- 5        [5] ------ 6
!        |         |         |  \   3  |
!        |    1    |         |    \    |
!        |         |         | 2    \  |
!  0.0   1 ------- 2        [2] ------ 3
!
!       0.0       1.0       1.0      2.0 
! 
!           PET 0               PET 1
!
!               Node Id labels at corners
!              Element Id labels in centers
!
!\end{verbatim}
! 
! This example is intended to illustrate the creation of a small Mesh on multiple PETs. This example creates the same small 2D Mesh as the 
! previous two examples (See Section~\ref{sec:mesh:1pet1step} for a diagram), however, in this case the Mesh is broken up across 4 PETs. 
! The figure above illustrates the distribution of the Mesh across the PETs. As in the previous diagram, the node ids are in
! the corners of the elements and the element ids are in the centers. In this figure '[' and ']' around a character indicate a node which
! is owned by another PET. The nodeOwner parameter indicates which PET owns the node.  Note that the three step creation 
! illustrated in Section~\ref{sec:mesh:1pet3step} could also be used in a parallel Mesh creation such as this by simply interleaving 
! the three calls in the appropriate places between the node and element array definitions. 
!
!EOE

  ! Only do this if we have 1 processor
  if (petCount .eq. 4) then

!BOC


 ! Break up what's being set by PET
 if (localPET .eq. 0) then !!! This part only for PET 0
    ! Set number of nodes
     numNodes=4

    ! Allocate and fill the node id array.
    allocate(nodeIds(numNodes))
    nodeIds=(/1,2,4,5/) 

    ! Allocate and fill node coordinate array.
    ! Since this is a 2D Mesh the size is 2x the
    ! number of nodes.
    allocate(nodeCoords(2*numNodes))
    nodeCoords=(/0.0,0.0, & ! node id 1
                 1.0,0.0, & ! node id 2
                 0.0,1.0, & ! node id 4
                 1.0,1.0 /) ! node id 5

    ! Allocate and fill the node owner array.
    allocate(nodeOwners(numNodes))
    nodeOwners=(/0, & ! node id 1
                 0, & ! node id 2
                 0, & ! node id 4
                 0/)  ! node id 5

    ! Set the number of each type of element, plus the total number.
    numQuadElems=1
    numTriElems=0
    numTotElems=numQuadElems+numTriElems

    ! Allocate and fill the element id array.
    allocate(elemIds(numTotElems))
    elemIds=(/1/) 

    ! Allocate and fill the element topology type array.
    allocate(elemTypes(numTotElems))
    elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 1

    ! Allocate and fill the element connection type array.
    ! Note that entry are local indices
    allocate(elemConn(4*numQuadElems+3*numTriElems))
    elemConn=(/1,2,4,3/) ! elem id 1

  else if (localPET .eq. 1) then !!! This part only for PET 1
    ! Set number of nodes
     numNodes=4

    ! Allocate and fill the node id array.
    allocate(nodeIds(numNodes))
    nodeIds=(/2,3,5,6/) 

    ! Allocate and fill node coordinate array.
    ! Since this is a 2D Mesh the size is 2x the
    ! number of nodes.
    allocate(nodeCoords(2*numNodes))
    nodeCoords=(/1.0,0.0, & ! node id 2
                 2.0,0.0, & ! node id 3
                 1.0,1.0, & ! node id 5
                 2.0,1.0 /) ! node id 6

    ! Allocate and fill the node owner array.
    allocate(nodeOwners(numNodes))
    nodeOwners=(/0, & ! node id 2
                 1, & ! node id 3
                 0, & ! node id 5
                 1/)  ! node id 6

    ! Set the number of each type of element, plus the total number.
    numQuadElems=0
    numTriElems=2
    numTotElems=numQuadElems+numTriElems

    ! Allocate and fill the element id array.
    allocate(elemIds(numTotElems))
    elemIds=(/2,3/) 

    ! Allocate and fill the element topology type array.
    allocate(elemTypes(numTotElems))
    elemTypes=(/ESMF_MESHELEMTYPE_TRI, & ! elem id 2
                ESMF_MESHELEMTYPE_TRI/)  ! elem id 3

    ! Allocate and fill the element connection type array.
    allocate(elemConn(4*numQuadElems+3*numTriElems))
    elemConn=(/1,2,3, & ! elem id 2
               2,4,3/)  ! elem id 3

  else if (localPET .eq. 2) then !!! This part only for PET 2
    ! Set number of nodes
     numNodes=4

    ! Allocate and fill the node id array.
    allocate(nodeIds(numNodes))
    nodeIds=(/4,5,7,8/) 

    ! Allocate and fill node coordinate array.
    ! Since this is a 2D Mesh the size is 2x the
    ! number of nodes.
    allocate(nodeCoords(2*numNodes))
    nodeCoords=(/0.0,1.0, & ! node id 4
                 1.0,1.0, & ! node id 5
                 0.0,2.0, & ! node id 7
                 1.0,2.0 /) ! node id 8

    ! Allocate and fill the node owner array.
    ! Since this Mesh is all on PET 0, it's just set to all 0.
    allocate(nodeOwners(numNodes))
    nodeOwners=(/0, & ! node id 4
                 0, & ! node id 5
                 2, & ! node id 7
                 2/)  ! node id 8

    ! Set the number of each type of element, plus the total number.
    numQuadElems=1
    numTriElems=0
    numTotElems=numQuadElems+numTriElems

    ! Allocate and fill the element id array.
    allocate(elemIds(numTotElems))
    elemIds=(/4/) 

    ! Allocate and fill the element topology type array.
    allocate(elemTypes(numTotElems))
    elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 4

    ! Allocate and fill the element connection type array.
    allocate(elemConn(4*numQuadElems+3*numTriElems))
    elemConn=(/1,2,4,3/) ! elem id 4

  else if (localPET .eq. 3) then !!! This part only for PET 3
    ! Set number of nodes
     numNodes=4

    ! Allocate and fill the node id array.
    allocate(nodeIds(numNodes))
    nodeIds=(/5,6,8,9/) 

    ! Allocate and fill node coordinate array.
    ! Since this is a 2D Mesh the size is 2x the
    ! number of nodes.
    allocate(nodeCoords(2*numNodes))
    nodeCoords=(/1.0,1.0, &  ! node id 5
                 2.0,1.0, &  ! node id 6
                 1.0,2.0, &  ! node id 8
                 2.0,2.0 /)  ! node id 9

    ! Allocate and fill the node owner array.
    allocate(nodeOwners(numNodes))
    nodeOwners=(/0, & ! node id 5
                 1, & ! node id 6
                 2, & ! node id 8
                 3/)  ! node id 9

    ! Set the number of each type of element, plus the total number.
    numQuadElems=1
    numTriElems=0
    numTotElems=numQuadElems+numTriElems

    ! Allocate and fill the element id array.
    allocate(elemIds(numTotElems))
    elemIds=(/5/)  

    ! Allocate and fill the element topology type array.
    allocate(elemTypes(numTotElems))
    elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 5

    ! Allocate and fill the element connection type array.
    allocate(elemConn(4*numQuadElems+3*numTriElems))
    elemConn=(/1,2,4,3/) ! elem id 5
  endif

  
  ! Create Mesh structure in 1 step
  mesh=ESMF_MeshCreate(parametricDim=2, spatialDim=2, &
         coordSys=ESMF_COORDSYS_CART, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, &
         rc=localrc)


  ! After the creation we are through with the arrays, so they may be
  ! deallocated.
  deallocate(nodeIds)
  deallocate(nodeCoords)
  deallocate(nodeOwners)
  deallocate(elemIds)
  deallocate(elemTypes)
  deallocate(elemConn)


  ! At this point the mesh is ready to use. For example, as is 
  ! illustrated here, to have a field created on it. Note that 
  ! the Field only contains data for nodes owned by the current PET.
  ! Please see Section "Create a Field from a Mesh" under Field
  ! for more information on creating a Field on a Mesh. 
  field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_R8,  rc=localrc)

!EOC

 ! Get rid of Field
  call ESMF_FieldDestroy(field, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! DON'T GET RID OF MESH BECAUSE USING IT BELOW
  !! Get rid of Mesh
  !call ESMF_MeshDestroy(mesh, rc=localrc)
  !if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE




!BOE
!\subsubsection{Create a copy of a Mesh with a new distribution}
!
!\begin{verbatim}
!
!  2.0   7 -------[8]               8 ------- 9          
!        |         |                |         |
!        |    4    |                |    5    |
!        |         |                |         |
!  1.0   4 ------ [5]               5 ------- 6
!        
!       0.0       1.0              1.0       2.0
!
!           PET 1                      PET 0
!
!
!  1.0  [4] ----- [5]              [5] ----- [6]
!        |         |  \                \      |
!        |    1    | 2  \                \  3 |
!        |         |      \                \  |
!  0.0   1 ------- 2 -----[3]                 3
!
!       0.0       1.0               1.0      2.0 
! 
!           PET 2                      PET 3
!
!               Node Id labels at corners
!              Element Id labels in centers
!
!\end{verbatim}
!
! This example demonstrates the creation of a new Mesh which is a copy of an existing Mesh
! with a new distribution of the original Mesh's nodes and elements. To create the new Mesh 
! in this manner the user needs two DistGrids. One to describe the new distribution of the nodes. 
! The other to describe the new distribution of the elements. In this example we create new 
! DistGrids from a list of indices. The DistGrids are then used in the redistribution 
! Mesh create interface which is overloaded to {\tt ESMF\_MeshCreate()}. In this example
! we redistribute the Mesh created in the previous example (Section~\ref{sec:mesh:4pet1step}) 
! to the distribution pictured above. Note that for simplicity's sake, the position of the
! Mesh in the diagram is basically the same, but the PET positions and node owners 
! have been changed. 
!
!EOE

!BOC

  ! Setup the new location of nodes and elements depending on the processor
  if (localPet .eq. 0) then !!! This part only for PET 0
     allocate(elemIds(1))
     elemIds=(/5/)
     
     allocate(nodeIds(4))
     nodeIds=(/5,6,8,9/)
     
  else if (localPet .eq. 1) then !!! This part only for PET 1
     allocate(elemIds(1))
     elemIds=(/4/)
     
     allocate(nodeIds(2))
     nodeIds=(/7,4/)        
     
  else if (localPet .eq. 2) then !!! This part only for PET 2
     allocate(elemIds(2))
     elemIds=(/1,2/)
     
     allocate(nodeIds(2))
     nodeIds=(/1,2/)
     
  else if (localPet .eq. 3) then !!! This part only for PET 3
     allocate(elemIds(1))
     elemIds=(/3/)
     
     allocate(nodeIds(1))
     nodeIds=(/3/)
     
  endif


  ! Create new node DistGrid
  nodedistgrid=ESMF_DistGridCreate(nodeIds, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

 
  ! Create new element DistGrid
  elemdistgrid=ESMF_DistGridCreate(elemIds, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Can now deallocate distribution lists
  deallocate(elemIds)
  deallocate(nodeIds)


  ! Create new redisted Mesh based on DistGrids
  mesh2=ESMF_MeshCreate(mesh,                         &
                        nodalDistgrid=nodedistgrid,   & 
                        elementDistgrid=elemdistgrid, &
                        rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! At this point the mesh is ready to use. For example, as is 
  ! illustrated here, to have a field created on it. Note that 
  ! the Field only contains data for nodes owned by the current PET.
  ! Please see Section "Create a Field from a Mesh" under Field
  ! for more information on creating a Field on a Mesh. 
  field = ESMF_FieldCreate(mesh2, ESMF_TYPEKIND_R8,  rc=localrc)

!EOC
  
 ! Destroy Field
  call ESMF_FieldDestroy(field, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Destroy Meshes
  call ESMF_MeshDestroy(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_MeshDestroy(mesh2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Destroy Distgrids
  call ESMF_DistgridDestroy(nodedistgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_DistgridDestroy(elemdistgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

   ! endif for skip for != 4 proc (extends all the way to previous subsection)
  endif 

!BOE
!\subsubsection{Create a Mesh from an unstructured grid file}
!\label{sec:example:UnstructFromFile}
!
! ESMF supports the creation of a Mesh from three grid file formats: the SCRIP format~\ref{sec:fileformat:scrip}, the 
! ESMF format~\ref{sec:fileformat:esmf} or the
! proposed CF unstructured grid UGRID format~\ref{sec:fileformat:ugrid}.  All three of these grid file formats
! are NetCDF files. 
! 
! When creating a Mesh from a SCRIP format file, there are a number of options to control the output Mesh.
! The data is located at the center of the grid cell in a SCRIP grid; whereas
! the data is located at the corner of a cell in an ESMF Mesh object.  Therefore,
! we create a Mesh object by default by constructing a "dual" mesh using the coordinates in the file. 
! If the user wishes to not construct the dual mesh, the optional argument {\tt convertToDual} may be 
! used to control this behavior. When {\tt comvertToDual} is 
! set to .false. the Mesh constructed from the file will not be the dual. This is necessary when using the 
! Mesh as part of a conservative regridding operation in the {\tt ESMF\_FieldRegridStore()} call, so the
! weights are properly generated for the cell centers in the file. 
!
! The following example code depicts how to create a Mesh using a SCRIP file. Note that
! you have to set the {\tt fileformat} to ESMF\_FILEFORMAT\_SCRIP.  
!EOE

#ifdef ESMF_NETCDF
!BOC
   mesh = ESMF_MeshCreate(filename="data/ne4np4-pentagons.nc", &
	   fileformat=ESMF_FILEFORMAT_SCRIP, rc=localrc)
!EOC
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get rid of Mesh
  call ESMF_MeshDestroy(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
#endif

!BOE
! As mentioned above ESMF also supports creating Meshes from the ESMF format.
! The ESMF format works better with the methods used to create an ESMF Mesh object, so less conversion needs 
! to be done to create a Mesh, and thus this format is more efficient than SCRIP to use within ESMF. 
! The ESMF format is also more general than the SCRIP format because it supports higher dimension coordinates and more general
! topologies.  Currently, ESMF\_MeshCreate() does not support conversion to a dual mesh for this format. All regrid methods
! are supported on Meshes in this format. 
!
! Here is an example of creating a Mesh from an ESMF unstructured grid file. Note that you have to set the {\tt fileformat} to
! ESMF\_FILEFORMAT\_ESMFMESH.  
!EOE

#ifdef ESMF_NETCDF
!BOC
   mesh = ESMF_MeshCreate(filename="data/ne4np4-esmf.nc", &
            fileformat=ESMF_FILEFORMAT_ESMFMESH, &
            rc=localrc)
!EOC
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get rid of Mesh
  call ESMF_MeshDestroy(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
#endif


!BOE
!\subsubsection{Remove Mesh memory}
!
! There are two different levels that the memory in a Mesh can be removed. The first of these is the standard destroy call, 
! {\tt ESMF\_MeshDestroy()}. As with other classes, this call removes all memory associated with the object, and afterwards  
! the object can not be used further (i.e. should not be used in any methods). The second, which is unique to Mesh, is the 
! {\tt ESMF\_MeshFreeMemory()} call. This call removes the connection and coordinate information associated with the Mesh, but
! leaves the distgrid information. The coordinate and connection information held in the Mesh can consume a large amount of memory
! for a big Mesh, so using this call can very significantly reduce the amount of memory used. However, once this method
! has been used on a Mesh there are some restriction on what may be done with it. Once a Mesh has had its memory freed using this method, 
! any Field built on the Mesh can no longer be used as part of an {\tt ESMF\_FieldRegridStore()} call. However, because the distgrid 
! information is still part of the Mesh, Fields built on such a Mesh can still be part of an {\tt ESMF\_FieldRegrid()}
! call (where the routehandle was generated previous to the {\tt ESMF\_MeshFreeMemory()} operation). Fields may also 
! still be created on these Meshes. The following short piece of code illustrates the use of this call.
!
!EOE

! Just do this for 1 PET mesh
if (petCount .eq. 1) then 
  ! Set number of nodes
  numNodes=9

  ! Allocate and fill the node id array.
  allocate(nodeIds(numNodes))
  nodeIds=(/1,2,3,4,5,6,7,8,9/) 

  ! Allocate and fill node coordinate array.
  ! Since this is a 2D Mesh the size is 2x the
  ! number of nodes.
  allocate(nodeCoords(2*numNodes))
  nodeCoords=(/0.0,0.0, & ! node id 1
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
  allocate(nodeOwners(numNodes))
  nodeOwners=0 ! everything on PET 0


  ! Set the number of each type of element, plus the total number.
  numQuadElems=3
  numTriElems=2
  numTotElems=numQuadElems+numTriElems


  ! Allocate and fill the element id array.
  allocate(elemIds(numTotElems))
  elemIds=(/1,2,3,4,5/) 


  ! Allocate and fill the element topology type array.
  allocate(elemTypes(numTotElems))
  elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! elem id 1
              ESMF_MESHELEMTYPE_TRI,  & ! elem id 2
              ESMF_MESHELEMTYPE_TRI,  & ! elem id 3
              ESMF_MESHELEMTYPE_QUAD, & ! elem id 4
              ESMF_MESHELEMTYPE_QUAD/)  ! elem id 5


  ! Allocate and fill the element connection type array.
  ! Note that entries in this array refer to the 
  ! positions in the nodeIds, etc. arrays and that
  ! the order and number of entries for each element
  ! reflects that given in the Mesh options 
  ! section for the corresponding entry
  ! in the elemTypes array. The number of 
  ! entries in this elemConn array is the
  ! number of nodes in a quad. (4) times the 
  ! number of quad. elements plus the number
  ! of nodes in a triangle (3) times the number
  ! of triangle elements. 
  allocate(elemConn(4*numQuadElems+3*numTriElems))
  elemConn=(/1,2,5,4, &  ! elem id 1
             2,3,5,   &  ! elem id 2
             3,6,5,   &  ! elem id 3
             4,5,8,7, &  ! elem id 4
             5,6,9,8/)   ! elem id 5


  ! Create Mesh structure in 1 step
  mesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
         coordSys=ESMF_COORDSYS_CART, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, &
         rc=localrc)


  ! After the creation we are through with the arrays, so they may be
  ! deallocated.
  deallocate(nodeIds)
  deallocate(nodeCoords)
  deallocate(nodeOwners)
  deallocate(elemIds)
  deallocate(elemTypes)
  deallocate(elemConn)

!BOC

   ! Here a Field built on a mesh may be used
   ! as part of a ESMF_FieldRegridStore() call

   ! This call removes connection and coordinate 
   ! information, significantly reducing the memory used by
   ! mesh, but limiting what can be done with it.
   call ESMF_MeshFreeMemory(mesh, rc=localrc)

   ! Here a new Field may be built on mesh, or
   ! a field built on a mesh may be used as part
   ! of an ESMF_FieldRegrid() call

   ! Destroy the mesh
   call ESMF_MeshDestroy(mesh, rc=localrc)

   ! Here mesh can't be used for anything

!EOC
endif ! 1 proc

!BOE
!\subsubsection{Mesh Masking}\label{sec:mesh:mask}
!
! There are two types of masking available in Mesh: node masking and element masking. These both work
! in a similar manner, but vary slightly in the details of setting the mask information during mesh creation. 
!
! For node masking, the mask information is set using the {\tt nodeMask} argument to either {\tt ESMF\_MeshCreate()} or 
! {\tt ESMF\_MeshAddNodes()}. When a regrid store method is called (e.g. {\tt ESMF\_FieldRegridStore()}) the mask values arguments 
! ({\tt srcMaskValues} and {\tt dstMaskValues}) can 
! then be used to indicate which particular values set in the {\tt nodeMask} array indicate that the node should be masked. For example, when 
! calling {\tt ESMF\_FieldRegridStore()} if {\tt dstMaskValues} has been set to 1, then any node in the destination Mesh whose 
! corresponding {\tt nodeMask} value is 1 will be masked out (a node with any other value than 1 will not be masked). 
!
! For element masking, the mask information is set using the {\tt elementMask} argument to either {\tt ESMF\_MeshCreate()} or 
! {\tt ESMF\_MeshAddElements()}. In a similar manner to node masking, when a regrid store method is called (e.g. {\tt ESMF\_FieldRegridStore()}) 
! the mask values arguments 
! ({\tt srcMaskValues} and {\tt dstMaskValues}) can 
! then be used to indicate which particular values set in the {\tt elementMask} array indicate that the element should be masked. For example, when 
! calling {\tt ESMF\_FieldRegridStore()} if {\tt dstMaskValues} has been set to 1, then any element in the destination Mesh whose 
! corresponding {\tt elementMask} value is 1 will be masked out (an element with any other value than 1 will not be masked). 
!
!EOE



10   continue
  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

  call ESMF_Finalize(rc=rc)

  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_MeshEx.F90"
  else
    print *, "FAIL: ESMF_MeshEx.F90"
  endif



end program ESMF_MeshEx
