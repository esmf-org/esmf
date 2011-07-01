! $Id: ESMF_MeshEx.F90,v 1.44 2011/07/01 16:07:29 rokuingh Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2011, University Corporation for Atmospheric Research,
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
  integer :: finalrc, rc, petCount,localPet, localrc

  ! individual test failure message
  character(ESMF_MAXSTR) :: name
  character(ESMF_MAXSTR) :: filename

  logical :: correct
  type(ESMF_VM) :: vm
  type(ESMF_Mesh) :: mesh
  

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
! Mesh class interfaces, for further detail which applies to using a Field specifically on created on a Mesh, please see 
! Section~\ref{sec:field:usage:create_mesh_arrayspec}.
!
!\subsubsection{Mesh creation}
!\label{sec:mesh:usage:meshCreation}
!
! To create a Mesh we need to set some properties of the Mesh as a whole,  some properties of each node in the mesh and 
! then some properties of each element which connects the nodes. 
!
! For the Mesh as a whole we set its parametric dimension ({\tt parametricDim}) and spatial dimension ({\tt spatialDim}). 
! The parametric dimension of a Mesh is the dimension of the topology of the Mesh, this can be thought of as the dimension of 
! the elements which make up the Mesh. For example, a Mesh composed of triangles would have a parametric dimension of 2, whereas
! a Mesh composed of tetrahedra would have a parametric dimension of 3. A Mesh's spatial dimension, on the other hand, is the 
! dimension of the space the Mesh is embedded in, in other words the number of coordinate dimensions needed to describe the 
! location of the nodes making up the Mesh. For example, a Mesh constructed of squares on a plane would have a parametric 
! dimension of 2 and a spatial dimension of 2, whereas if that same Mesh were used to represent the 2D surface of a sphere 
! then the Mesh would still have a parametric dimension of 2, but now its spatial dimension would be 3. 
!
! The structure of the per node and element information used to create a Mesh is influenced by the Mesh distribution strategy. 
! The Mesh class is distributed by elements. This means that a node must be present on any PET that contains an element 
! associated with that node, but not on any other PET (a node can't be on a PET without an element ""home"). Since a node may be used
! by two or more elements located on different PETs, a node may be duplicated on muliple PETs. When a node is duplicated in this manner, 
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
! (e.g. a triangle vs. a quadralateral). The range of choices for the topology of the elements in a Mesh are restricted by the 
! Mesh's parametric dimension (e.g. a Mesh can't contain a 2D element like a triangle, when its parametric dimension is 3D), but it can contain
! any combination of elements appropriate to its dimension. The element connectivity indicates which nodes are to be connected together to
! form the element. The number of nodes connected together for each element is implied by the elements topology type ({\tt elementTypes}). 
! It is IMPORTANT to note, that the entries in this list are NOT the global ids of the nodes, but are indices into the PET local lists of
! node info used in the Mesh Create. In other words, the element connectivity isn't specified in terms of the global list of nodes, but instead
! is specified in terms of the locally described node info. One other important point about connectivities is that the order of the nodes in the 
! connectivity list of an element is important. Please see Section~\ref{const:meshelemtype} for diagrams illustrating the correct order of
! nodes in an element. 
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
! quadralaterals and triangles.  The coordinates of the nodes in the Mesh range from 0.0 to 2.0 in both dimensions. The node ids are 
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


  ! Set arrayspec for example field create
  ! Use a dimension of 1, because Mesh data is linearized 
  ! into a one dimensional array. 
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=localrc)

  ! At this point the mesh is ready to use. For example, as is 
  ! illustrated here, to have a field created on it. Note that 
  ! the Field only contains data for nodes owned by the current PET.
  ! Please see Section "Create a Field from a Mesh" under Field
  ! for more information on creating a Field on a Mesh. 
  field = ESMF_FieldCreate(mesh, arrayspec,  rc=localrc)

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
  mesh = ESMF_MeshCreate(parametricDim=2,spatialDim=2, rc=localrc)

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


  ! Set arrayspec for example field create
  ! Use a dimension of 1, because Mesh data is linearized 
  ! into a one dimensional array. 
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=localrc)

  ! At this point the mesh is ready to use. For example, as is 
  ! illustrated here, to have a field created on it. Note that 
  ! the Field only contains data for nodes owned by the current PET.
  ! Please see Section "Create a Field from a Mesh" under Field
  ! for more information on creating a Field on a Mesh. 
  field = ESMF_FieldCreate(mesh, arrayspec,  rc=localrc)

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
!
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


  ! Set arrayspec for example field create
  ! Use a dimension of 1, because Mesh data is linearized 
  ! into a one dimensional array. 
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=localrc)

  ! At this point the mesh is ready to use. For example, as is 
  ! illustrated here, to have a field created on it. Note that 
  ! the Field only contains data for nodes owned by the current PET.
  ! Please see Section "Create a Field from a Mesh" under Field
  ! for more information on creating a Field on a Mesh. 
  field = ESMF_FieldCreate(mesh, arrayspec,  rc=localrc)

!EOC

 ! Get rid of Field
  call ESMF_FieldDestroy(field, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get rid of Mesh
  call ESMF_MeshDestroy(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

   ! endif for skip for != 4 proc
  endif 

!BOE
!\subsubsection{Create a Mesh from a SCRIP Grid file or an ESMF unstructured Grid file}
!\label{sec:example:UnstructFromFile}
!
! ESMF supports the creation of a Mesh from a 2D unstructured grid defined in a SCRIP format
! grid file~\cite{ref:SCRIP} or an ESMF format grid file.  Both the SCRIP grid file and the
! ESMF grid file are in NetCDF format. Here is a sample header from a SCRIP unstructured
! grid file:
!\begin{verbatim}
!netcdf ne4np4-pentagons {
!dimensions:
!	grid_size = 866 ;
!	grid_corners = 5 ;
!	grid_rank = 1 ;
!variables:
!	double grid_area(grid_size) ;
!		grid_area:units = "radians^2" ;
!		grid_area:long_name = "area weights" ;
!	double grid_center_lat(grid_size) ;
!		grid_center_lat:units = "degrees" ;
!	double grid_center_lon(grid_size) ;
!		grid_center_lon:units = "degrees" ;
!	double grid_corner_lon(grid_size, grid_corners) ;
!		grid_corner_lon:units = "degrees" ;
!		grid_corner_lon:_FillValue = -9999. ;
!	double grid_corner_lat(grid_size, grid_corners) ;
!		grid_corner_lat:units = "degrees" ;
!		grid_corner_lat:_FillValue = -9999. ;
!	double grid_imask(grid_size) ;
!		grid_imask:_FillValue = -9999. ;
!	int grid_dims(grid_rank) ;
!}
!\end{verbatim}
!
! The grid cells are organized as a one dimensional array ({\tt grid\_rank = 1}). The
! cell connection is defined using {\tt grid\_corner\_lat} and {\tt grid\_corner\_lon} with
! the maximum number of corners defined in {\tt grid\_corners}. {\tt grid\_imask} is not used 
! in the Mesh object in the current implementation.  
! The data is located at the center of the grid cell in a SCRIP grid; whereas
! the data is located at the corner of a cell in an ESMF Mesh object.  Therefore,
! we create a Mesh object by default by constructing a "dual" mesh using {\tt grid\_center\_lat} and
! {\tt grid\_center\_lon}.  
! If the user wishes to not construct the dual mesh, the optional argument {\tt convertToDual} may be 
! used to control this behavior. When {\tt comvertToDual} is 
! set to .false. the Mesh constructed from the file will not be the dual. This is necessary when using the 
! Mesh as part of a conservative regridding operation in the {\tt ESMF\_FieldRegridStore()} call, so the
! weights are properly generated for the cell centers in the file. 
!
! The following example code depicts how to create a Mesh using a SCRIP file. Note that
! you have to set the filetypeflag to ESMF\_FILEFORMAT\_SCRIP.  If the optional argument {\tt convert3D}
! is set to .true., the coordinates will be converted into 3D Cartesian first.  If the grid
! is a global grid and will be used in a regrid operation, this flag should be set to .true.
!EOE

#ifdef ESMF_NETCDF
!BOC
   mesh = ESMF_MeshCreate(filename="data/ne4np4-pentagons.nc", &
	   filetypeflag=ESMF_FILEFORMAT_SCRIP, convert3D=.true., rc=localrc)
!EOC
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get rid of Mesh
  call ESMF_MeshDestroy(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
#endif

!BOE
! In addition to the SCRIP format, ESMF also supports a more general unstructured grid file format for describing meshes.
! In the ESMF file format, the node coordinates are defined in a separate array
! {\tt nodeCoords} and indices to the {\tt nodeCoords} array are used in the element
! connectivity array {\tt elementConn}.  While in the SCRIP format, the two are combined into 
! {\tt grid\_corner\_lat} and {\tt grid\_corner\_lon} arrays.  The ESMF file format works
! better with the methods used to create an ESMF Mesh object, so less conversion needs to be done to create a Mesh. 
! The ESMF format is also more general than the SCRIP format because it supports higher dimension coordinates and more general
! topologies.  Currently, ESMF\_MeshCreate() does not support conversion to a dual mesh for this format. All regrid methods
! are supported on Meshes in this format.  The following is a sample header of a mesh described in the ESMF format.
!  
!\begin{verbatim}
! netcdf ne4np4-esmf {
! dimensions:	
!	nodeCount = 866 ;
!	elementCount = 936 ;
!	maxNodePElement = 4 ;
!	coordDim = 2 ;
!variables:	
!	double 	nodeCoords(numNode, coordDim);
!		nodeCoords:units = "degrees,degrees" ;
!	int elementConn(numElement, maxNodePElement) ;
!		elementConn:long_name = "Node Indices that define the element connectivity";
!		elementConn:_FillValue = -1 ;	
!	byte numElementConn(numElement) ;
!		numElementConn:long_name = "Number of nodes per element" ;
!	double centerCoords(numElement, coordDim) ;
!		centerCoords:units = "degrees" ;
!	double elementArea(numElement) ;
!		elementArea:units = "radians^2" ;
!		elementArea:long_name = "area weights" ;
!	int  elementMask(numElement) ;
!		elementMask:_FillValue = -9999. ;
!// global attributes:
!		:gridType="unstructured";
!		:version = "0.9" ;
!		:inputFile = "ne4np4-pentagons.nc" ;
!		:timeGenerated = "Fri Apr 16 16:05:24 2010" ;
!}
!\end{verbatim}
!
! Here is an example of creating a Mesh from an ESMF unstructured grid file. Note that you have to set the filetypeflag to
! ESMF\_FILEFORMAT\_ESMFMESH.  As with the previous example, we set {\tt convert3D} to true because this is a
! global grid.
!EOE

#ifdef ESMF_NETCDF
!BOC
   mesh = ESMF_MeshCreate(filename="data/ne4np4-esmf.nc", &
            filetypeflag=ESMF_FILEFORMAT_ESMFMESH, convert3D=.true., rc=localrc)
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

10   continue
  call ESMF_Finalize(rc=rc)

  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_MeshEx.F90"
  else
    print *, "FAIL: ESMF_MeshEx.F90"
  endif



end program ESMF_MeshEx
