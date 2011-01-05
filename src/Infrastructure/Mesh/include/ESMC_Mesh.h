// $Id: ESMC_Mesh.h,v 1.37 2011/01/05 20:05:44 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2011, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// This file is part of the pure C public ESMC API
//-----------------------------------------------------------------------------

//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_Mesh_h
#define ESMC_Mesh_h

//-----------------------------------------------------------------------------
//
//  !CLASS ESMC_Mesh - Public C interface to the ESMF Mesh class
//
// !DESCRIPTION:
//
// The code in this file defines the public C Mesh class and declares method
// signatures (prototypes).  The companion file {\tt ESMC\_Mesh.C} contains
// the definitions (full code bodies) for the Mesh methods.
//
//
//-----------------------------------------------------------------------------

#include "ESMC_Util.h"

#if defined (__cplusplus)
extern "C" {
#endif

// Class declaration type
typedef struct{
  void *ptr;
}ESMC_Mesh;


// MESH ELEMENT TYPES
#define ESMC_MESHELEMTYPE_TRI  5
#define ESMC_MESHELEMTYPE_QUAD 9
#define ESMC_MESHELEMTYPE_TETRA 10
#define ESMC_MESHELEMTYPE_HEX  12


// Class API

//------------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_MeshAddElements - Add elements to a Mesh \label{sec:mesh:capi:meshaddelements}
//
// !INTERFACE:
int ESMC_MeshAddElements(
  ESMC_Mesh mesh,           // inout 
  int  elementCount,        // in
  int *elementIds,          // in
  int *elementTypes,        // in
  int *elementConn          // in
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//   This call is the third and last part of the three part mesh create
//   sequence and should be called after the mesh is created with {\tt ESMF\_MeshCreate()}
//   (\ref{sec:mesh:capi:meshcreate})
//   and after the nodes are added with {\tt ESMF\_MeshAddNodes()} (\ref{sec:mesh:capi:meshaddnodes}).
//   This call adds the elements to the
//   mesh and finalizes the create. After this call the Mesh is usable, for
//   example a Field may be built on the created Mesh object and
//   this Field may be used in a {\tt ESMF\_FieldRegridStore()} call.
//
//   The parameters to this call {\tt elementIds}, {\tt elementTypes}, and
//   {\tt elementConn} describe the elements to be created. The description
//   for a particular element lies at the same index location in {\tt elementIds}
//   and {\tt elementTypes}. Each entry in {\tt elementConn} consists of the list of
//   nodes used to create that element, so the connections for element $e$ in the
//   {\tt elementIds} array will start at $number\_of\_nodes\_in\_element(1) + number\_of\_nodes\_in\_element(2) +
//   \cdots + number\_of\_nodes\_in\_element(e-1) + 1$ in {\tt elementConn}.
//
//   \begin{description}
//   \item[mesh]
//          Mesh object.        
//   \item[elementCount]
//          The number of elements on this PET. 
//   \item[elementIds]
//          An array containing the global ids of the elements to be created on this PET.
//          This input consists of a 1D array of size {\tt elementCount}.
//   \item[elementTypes]
//          An array containing the types of the elements to be created on this PET. The types used
//          must be appropriate for the parametric dimension of the Mesh. Please see
//          Section~\ref{sec:mesh:opt:elemtype} for the list of options. This 
//          input consists of a 1D array of size {\tt elementCount}.
//   \item[elementConn]
//         An array containing the indexes of the sets of nodes to be connected together to form the
//         elements to be created on this PET. The entries in this list are NOT node global ids,
//         but rather each entry is a local index (1 based) into the list of nodes which were
//         created on this PET by the previous {\tt ESMC\_MeshAddNodes()} call.
//         In other words, an entry of 1 indicates that this element contains the node
//         described by {\tt nodeIds(1)}, {\tt nodeCoords(1)}, etc. passed into the
//         {\tt ESMC\_MeshAddNodes()} call on this PET. It is also
//         important to note that the order of the nodes in an element connectivity list
//         matters. Please see Section~\ref{sec:mesh:opt:elemtype} for diagrams illustrating
//         the correct order of nodes in a element. This input consists of a 1D array with
//         a total size equal to the sum of the number of nodes in each element on
//         this PET. The number of nodes in each element is implied by its element type in
//         {\tt elementTypes}. The nodes for each element
//         are in sequence in this array (e.g. the nodes for element 1 are elementConn(1),
//         elementConn(2), etc.).
//   \end{description}
//
//EOP
//------------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_MeshAddNodes - Add nodes to a Mesh \label{sec:mesh:capi:meshaddnodes}
//
// !INTERFACE:
int ESMC_MeshAddNodes(
  ESMC_Mesh mesh,          // inout
  int nodeCount,           // in
  int *nodeIds,            // in
  double *nodeCoords,      // in
  int *nodeOwners          // in
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//   This call is the second part of the three part mesh create
//   sequence and should be called after the mesh's dimensions are set
//   using {\tt ESMC\_MeshCreate()}.
//   This call adds the nodes to the
//   mesh. The next step is to call {\tt ESMC\_MeshAddElements()} (\ref{sec:mesh:capi:meshcreate}).
//
//   The parameters to this call {\tt nodeIds}, {\tt nodeCoords}, and
//   {\tt nodeOwners} describe the nodes to be created on this PET.
//   The description for a particular node lies at the same index location in
//   {\tt nodeIds} and {\tt nodeOwners}. Each entry
//   in {\tt nodeCoords} consists of spatial dimension coordinates, so the coordinates
//   for node $n$ in the {\tt nodeIds} array will start at $(n-1)*spatialDim+1$.
//
//   \begin{description}
//   \item[mesh]
//     Mesh object.
//   \item[nodeCount]
//     The number of nodes on this PET. 
//   \item [nodeIds]
//         An array containing the global ids of the nodes to be created on this PET. 
//        This input consists of a 1D array the size of the number of nodes on this PET (i.e. {\tt nodeCount}).
//   \item[nodeCoords]
//          An array containing the physical coordinates of the nodes to be created on this
//          PET. The coordinates in this array are ordered
//          so that the coordinates for a node lie in sequence in memory. (e.g. for a
//          Mesh with spatial dimension 2, the coordinates for node 1 are in nodeCoords(0) and
//          nodeCoords(1), the coordinates for node 2 are in nodeCoords(2) and nodeCoords(3),
//          etc.). This input consists of a 1D array the size of {\tt nodeCount} times the Mesh's
//          spatial dimension ({\tt spatialDim}).
//   \item[nodeOwners]
//         An array containing the PETs that own the nodes to be created on this PET.
//         If the node is shared with another PET, the value
//         may be a PET other than the current one. Only nodes owned by this PET
//         will have PET local entries in a Field created on the Mesh. This 
//         input consists of a 1D array the size of the number of nodes on this PET (i.e. {\tt nodeCount}).
//   \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//------------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_MeshCreate - Create a Mesh as a 3 step process \label{sec:mesh:capi:meshcreate}
//
// !INTERFACE:
ESMC_Mesh ESMC_MeshCreate(
  int parametricDim,         // in
  int spatialDim,            // in
  int *rc                    // out
);
// !RETURN VALUE:
//  type(ESMC_Mesh)         :: ESMC_MeshCreate
//
// !DESCRIPTION:
//
//  This call is the first part of the three part mesh create sequence. This call sets the dimension of the elements 
//  in the mesh ({\tt parametricDim}) and the number of coordinate dimensions in the mesh ({\tt spatialDim}). 
//  The next step is to call {\tt ESMC\_MeshAddNodes()} (\ref{sec:mesh:capi:meshaddnodes}) to add the nodes and then 
//  {\tt ESMC\_MeshAddElements(})  (\ref{sec:mesh:capi:meshaddelements}) 
//  to add the elements and finalize the mesh. 
//
//  The arguments are:
//  \begin{description}
//  \item[parametricDim]
//    Dimension of the topology of the Mesh. (E.g. a mesh constructed of squares would have a parametric dimension 
//    of 2, whereas a Mesh constructed of cubes would have one of 3.)
//  \item[spatialDim]
//  The number of coordinate dimensions needed to describe the locations of the nodes making up the Mesh. For a 
//  manifold, the spatial dimesion can be larger than the parametric dim (e.g. the 2D 
//  surface of a sphere in 3D space), 
//   but it can't be smaller. 
//  \item[rc]
//  Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//------------------------------------------------------------------------------
//BOPI
// !IROUTINE: ESMC_MeshCreateDistGrids - Create Dist Grids in a Mesh
//
// !INTERFACE:
int ESMC_MeshCreateDistGrids(
  ESMC_Mesh mesh,             // in
  int *nodeDistGrid,          // in
  int *elemDistGrid,          // in
  int *nodeCount,             // in
  int *elementCount           // in
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Description to be added.
//
//  The arguments are:
//  \begin{description}
//  \item[mesh]
//    Mesh object.
//  \item[nodeDistGrid]
//  Description to be added.
//  \item[elemDistGrid]
//  Description to be added.
//  \item[nodeCount]
//  Description to be added.
//  \item[elementCount]
//  Description to be added.
//  \end{description}
//
//EOPI
//-----------------------------------------------------------------------------

//------------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_MeshDestroy - Destroy a Mesh
//
// !INTERFACE:
int ESMC_MeshDestroy(
  ESMC_Mesh *mesh             // in
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//  Destroy the Mesh. This call removes all internal memory associated with {\tt mesh}. After this call mesh will no longer be usable. 
//
//  The arguments are:
//  \begin{description}
//  \item[mesh]
//    Mesh object whose memory is to be freed. 
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//------------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_MeshFreeMemory - Remove a Mesh and its memory
//
// !INTERFACE:
int ESMC_MeshFreeMemory(
  ESMC_Mesh mesh            // in
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
//
// !DESCRIPTION:
//    This call removes the portions of {\tt mesh} which contain connection and coordinate
//    information. After this call, Fields build on {\tt mesh} will no longer be usable
//    as part of an {\tt ESMF\_FieldRegridStore()} operation. However, after this call
//    Fields built on {\tt mesh} can still be used in an {\tt ESMF\_FieldRegrid()}
//    operation if the routehandle was generated beforehand. New Fields may also
//    be built on {\tt mesh} after this call.
//
// The arguments are:
// \begin{description}
// \item [mesh]
// Mesh object whose memory is to be freed.
// \end{description}
//
//EOP
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_MeshGetLocalElementCount - Get the number of elements in a Mesh owned by the current PET
//
// !INTERFACE:
int ESMC_MeshGetLocalElementCount(
  ESMC_Mesh mesh,           // in
  int *elementCount         // out
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
//
// !DESCRIPTION:
// Query the number of elements in a mesh owned by the local PET.
// The arguments are:
// \begin{description}
// \item[mesh]
//     The mesh
// \item[elementCount]
//     The number of elements on this PET. 
// \end{description}
//
//EOP
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_MeshGetLocalNodeCount - Get the number of nodes in a Mesh owned by the current PET
//
// !INTERFACE:
int ESMC_MeshGetLocalNodeCount(
  ESMC_Mesh mesh,          // in
  int *nodeCount           // out
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
//
// !DESCRIPTION:
// Query the number of nodes in a mesh owned by the local PET.
// The arguments are:
// \begin{description}
// \item[mesh]
//     The mesh
// \item[nodeCount]
//     The number of nodes on this PET. 
// \end{description}
//
//EOP
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//BOPI
// !IROUTINE: ESMC_MeshWrite - Write a Mesh to a VTK file
//
// !INTERFACE:
int ESMC_MeshWrite(
  ESMC_Mesh mesh,            // in
  const char* fname          // in
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
//
// !DESCRIPTION:
//   Write a mesh to VTK file.
//
// The arguments are:
//   \begin{description}
//   \item [mesh]
//         The mesh.
//   \item[filename]
//         The name of the output file.
//   \end{description}
//
//EOPI
//------------------------------------------------------------------------------

// Associated methods

//BOPI
// !IROUTINE: ESMC_MeshVTKHeader
//
// !INTERFACE:
int ESMC_MeshVTKHeader(
  const char *fname, 
  int *num_elem, 
  int *num_node,
  int *conn_size
);
//EOPI

//BOPI
// !IROUTINE: ESMC_MeshVTKBody
//
// !INTERFACE:
int ESMC_MeshVTKBody(
  const char *fname, 
  int *nodeId, 
  double *nodeCoord,
  int *nodeOwner, 
  int *elemId, 
  int *elemType, 
  int *elemConn
);
//EOPI


#if defined (__cplusplus)
} // extern "C"
#endif

#endif  // ESMC_Mesh_h
