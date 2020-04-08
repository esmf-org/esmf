// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2020, University Corporation for Atmospheric Research,
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
#define ESMC_MESHELEMTYPE_TRI  3
#define ESMC_MESHELEMTYPE_QUAD 4
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
  int *elementConn,         // in
  int *elementMask,         // in
  double *elementArea,      // in
  double *elementCoords     // in
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
//          Section~\ref{const:cmeshelemtype} for the list of options. This
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
//         matters. Please see Section~\ref{const:cmeshelemtype} for diagrams illustrating
//         the correct order of nodes in a element. This input consists of a 1D array with
//         a total size equal to the sum of the number of nodes in each element on
//         this PET. The number of nodes in each element is implied by its element type in
//         {\tt elementTypes}. The nodes for each element
//         are in sequence in this array (e.g. the nodes for element 1 are elementConn(1),
//         elementConn(2), etc.).
//   \item[{[elementMask]}]
//         An array containing values which can be used for element masking.
//         Which values indicate masking are chosen via the srcMaskValues or
//         dstMaskValues arguments to ESMF\_FieldRegridStore() call. This input
//         consists of a 1D array the size of the number of elements on this
//         PET. If not specified (i.e. NULL is passed in), then no masking will occur.
//   \item [{[elementArea]}]
//          An array containing element areas.  This input consists of a 1D array
//          the size of the number of elements on this PET. If not specified (i.e. NULL is passed in), the
//          element areas are internally calculated.
//   \item[{[elementCoords]}]
//          An array containing the physical coordinates of the elements to be created on this
//          PET. This input consists of a 1D array the size of the number of elements on this PET times the Mesh's
//          spatial dimension ({\tt spatialDim}). The coordinates in this array are ordered
//          so that the coordinates for an element lie in sequence in memory. (e.g. for a
//          Mesh with spatial dimension 2, the coordinates for element 1 are in elementCoords(1) and
//          elementCoords(2), the coordinates for element 2 are in elementCoords(3) and elementCoords(4),
//          etc.).
//
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
  enum ESMC_CoordSys_Flag *coordSys, // in
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
//  manifold, the spatial dimension can be larger than the parametric dim (e.g. the 2D
//  surface of a sphere in 3D space),
//   but it can't be smaller.
//  \item[{[coordSys]}]
//  Set the coordinate system of the mesh. If not specified, then
//  defaults to ESMC\_COORDSYS\_SPH\_DEG.
//  \item[rc]
//  Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//------------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_MeshCreateFromFile - Create a Mesh from a NetCDF grid file \label{sec:mesh:capi:meshcreatefromfile}
//
// !INTERFACE:
ESMC_Mesh ESMC_MeshCreateFromFile(
                                  const char *filename, // in (required)
                                  int fileTypeFlag,     // in (required)
                                  int *convertToDual,   // in (optional)
                                  int *addUserArea,     // in (optional)
                                  const char *meshname, // in (optional)
                                  int *maskFlag,        // in (optional)
                                  const char *varname,  // in (optional)
                                  int *rc               // out
);
// !RETURN VALUE:
//  type(ESMC_Mesh)         :: ESMC_MeshCreateFromFile
//
// !DESCRIPTION:
//
// Method to create a Mesh object from a NetCDF file in either SCRIP, UGRID,
// or ESMF file formats.
//
//  The required arguments are:
//  \begin{description}
//   \item [filename]
//         The name of the grid file
//   \item[filetypeflag]
//         The file type of the grid file to be read, please see Section~\ref{const:mesh:cfileformat}
//         for a list of valid options.
//   \item[{[convertToDual]}]
//         if 1, the mesh will be converted to its dual. If not specified,
//         defaults to 0. Converting to dual is only supported with
//         file type {\tt ESMF\_FILEFORMAT\_SCRIP}.
//   \item[{[addUserArea]}]
//         if 1, the cell area will be read in from the GRID file.  This feature is
//         only supported when the grid file is in the SCRIP or ESMF format. If not specified,
//         defaults to 0.
//   \item[{[meshname]}]
//         The dummy variable for the mesh metadata in the UGRID file if the {\tt filetypeflag}
//         is {\tt ESMF\_FILEFORMAT\_UGRID}.  If not specified, defaults to empty string.
//   \item[{[maskFlag]}]
//         An enumerated integer that, if specified, tells whether a mask in
//         a UGRID file should be defined on the nodes (MeshLoc.NODE) or
//         elements (MeshLoc.ELEMENT) of the mesh.  If specified, generate
//         the mask using the missing\_value attribute defined in 'varname'.
//         This flag is only supported when the grid file is in the UGRID
//         format.  If not specified, defaults to no mask.
//   \item[{[varname]}]
//         If maskFlag is specified, provide a variable name stored in the UGRID file and
//         the mask will be generated using the missing value of the data value of
//         this variable.  The first two dimensions of the variable has to be the
//         the longitude and the latitude dimension and the mask is derived from the
//         first 2D values of this variable even if this data is 3D, or 4D array. If not
//         specified, defaults to empty string.
//   \item [{[rc]}]
//         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------



//------------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_MeshGetCoord - Get lat/lon coordinates from a Mesh \label{sec:mesh:capi:meshgetcoord}
//
// !INTERFACE:
void ESMC_MeshGetCoord(
                         ESMC_Mesh mesh_in, // in (required)
                         double *nodeCoord, // out
                         int *num_nodes,    // out
                         int *num_dims,     // out
                         int *rc            // out
                         );
// !RETURN VALUE:
//  None
//
// !DESCRIPTION:
//
// This call returns the node coordinates of the given {\tt ESMC\_Mesh}
// in the provided {\tt nodeCoord} buffer of doubles.  At completion, this
// buffer is a 1-D array with the coordinates for a given node
// in adjacent indices.  For example, for $d$-dimensional coordinates, the first
// $d$ values in the returned array are the coordinates for the first node,
// the second $d$ values are the coordinates for the second node, etc.
//
// The arguments are:
// \begin{description}
// \item[mesh\_in] Mesh object.
// \item[nodeCoord] Pointer to doubles.  The node coordinates are returned here.
// \item[num\_nodes] Pointer to an integer.  The number of nodes found in
// the input Mesh is returned here.
// \item[num\_dims] Pointer to an integer.  The number of coordinate dimensions
// is returned here.
// \item[rc] Return code; equals {\tt ESMF\_SUCCESS} if there are no
// errors.
// \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//------------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_MeshGetElemCoord - Get lat/lon element center coordinates from a Mesh \label{sec:mesh:capi:meshgetelemcoord}
//
// !INTERFACE:
void ESMC_MeshGetElemCoord(
                         ESMC_Mesh mesh_in, // in (required)
                         double *elemCoord, // out
                         int *num_elems,    // out
                         int *num_dims,     // out
                         int *rc            // out
                         );
// !RETURN VALUE:
//  None
//
// !DESCRIPTION:
//
// This call returns the element coordinates of the given {\tt ESMC\_Mesh}
// in the provided {\tt elemCoord} buffer of doubles.  At completion, this
// buffer is a 1-D array with the coordinates for a given element
// in adjacent indices.  For example, for $d$-dimensional coordinates, the first
// $d$ values in the returned array are the coordinates for the first element,
// the second $d$ values are the coordinates for the second element, etc.
//
// The arguments are:
// \begin{description}
// \item[mesh\_in] Mesh object.
// \item[elemCoord] Pointer to doubles.  The element coordinates are returned here.
// \item[num\_elems] Pointer to an integer.  The number of elements found in
// the input Mesh is returned here.
// \item[num\_dims] Pointer to an integer.  The number of coordinate dimensions
// is returned here.
// \item[rc] Return code; equals {\tt ESMF\_SUCCESS} if there are no
// errors.
// \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//------------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_MeshGetConnectivity - Get Mesh connectivity
//
// !INTERFACE:
void ESMC_MeshGetConnectivity(
                         ESMC_Mesh mesh_in,     // in (required)
                         double *connCoord,     // out
                         int *nodesPerElem,  // out
                         int *rc                // out
                         );
// !RETURN VALUE:
//  None
//
// !DESCRIPTION:
//
//  NOTE: At this time the connectivity that is returned from this call is
//        not necessarily in the same format as how it was passed into the
//        creation routine.
//
// This call returns the connectivity of the given {\tt ESMC\_Mesh}
// in the provided {\tt connCoord} buffer of doubles.  At completion, this
// buffer is a 1-D array with the coordinates for the nodes of a given element
// in counterclockwise order.  The {/tt nodesPerElem} buffer of integers
// contains the number of nodes to expect per element.
//
// The arguments are:
// \begin{description}
// \item[mesh\_in] Mesh object.
// \item[connCoord] Pointer to doubles.  The connectivity is returned here.
// \item[nodesPerElem] Pointer to integers.  The number of nodes in each
//    element.
// \item[rc] Return code; equals {\tt ESMF\_SUCCESS} if there are no
// errors.
// \end{description}
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
// !IROUTINE: ESMC_MeshGetLocalElementCount - Get the number of elements in a Mesh on the current PET
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
// Query the number of elements in a mesh on the local PET.
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
// !IROUTINE: ESMC_MeshGetLocalNodeCount - Get the number of nodes in a Mesh on the current PET
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
// Query the number of nodes in a mesh on the local PET.
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
//BOP
// !IROUTINE: ESMC_MeshGetOwnedElementCount - Get the number of elements in a Mesh owned by the current PET
//
// !INTERFACE:
int ESMC_MeshGetOwnedElementCount(
  ESMC_Mesh mesh,           // in
  int *elementCount         // out
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
//
// !DESCRIPTION:
// Query the number of elements in a mesh owned by the local PET. This number will be equal or less than the
// local element count.
// The arguments are:
// \begin{description}
// \item[mesh]
//     The mesh
// \item[elementCount]
//     The number of elements owned by this PET.
// \end{description}
//
//EOP
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_MeshGetOwnedNodeCount - Get the number of nodes in a Mesh owned by the current PET
//
// !INTERFACE:
int ESMC_MeshGetOwnedNodeCount(
  ESMC_Mesh mesh,          // in
  int *nodeCount           // out
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
//
// !DESCRIPTION:
// Query the number of nodes in a mesh owned by the local PET.  This number will be equal or less than the
// local node count.
// The arguments are:
// \begin{description}
// \item[mesh]
//     The mesh
// \item[nodeCount]
//     The number of nodes owned by this PET.
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


//------------------------------------------------------------------------------
//BOPI
// !IROUTINE: ESMC_MeshCreateFromPtr - Create a Mesh from information and a pointer
//
// !INTERFACE:
  ESMC_Mesh ESMC_MeshCreateFromPtr(void *mesh_ptr,
                                   int parametricDim,
                                   int spatialDim, 
                                   enum ESMC_CoordSys_Flag coordSys,
                                   int *rc               // out
);
// !RETURN VALUE:
//  type(ESMC_Mesh)         :: ESMC_MeshCreateFromPtr
//
// !DESCRIPTION:
//
// Method to create a Mesh object from a NetCDF file in either SCRIP, UGRID,
// or ESMF file formats.
//
//  The required arguments are:
//  \begin{description}
//   \item [mesh_ptr]
//         Internal Mesh pointer
//   \item[parametricDim]
//         The parametric dimension of the Mesh.
//   \item[spatialDim]
//         The spatial dimension of the Mesh.
//   \item[coordSys]
//         The coordinate system the Mesh is expressed in. 
//   \item [{[rc]}]
//         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOPI
//-----------------------------------------------------------------------------



#if defined (__cplusplus)
} // extern "C"
#endif

#endif  // ESMC_Mesh_h
