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

#ifndef ESMC_XGrid_h
#define ESMC_XGrid_h

//-----------------------------------------------------------------------------
// ESMC_XGrid - Public C interface to the ESMF XGrid class
//
// The code in this file defines the public C XGrid class and declares method
// signatures (prototypes).  The companion file {\tt ESMC\_XGrid.C} contains
// the definitions (full code bodies) for the Field methods.
//-----------------------------------------------------------------------------

#include "ESMC_Mesh.h"
#include "ESMC_Interface.h"
#include "ESMC_Grid.h"
#include "ESMC_Util.h"

#if defined (__cplusplus)
extern "C" {
#endif

// Class declaration type
typedef struct{
  void *ptr;
} ESMC_XGrid;

// Class API

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_XGridCreate - Create an XGrid
//
// !INTERFACE:
  ESMC_XGrid ESMC_XGridCreate(
                              int sideAGridCount,  ESMC_Grid *sideAGrid, // in
                              int sideAMeshCount,  ESMC_Mesh *sideAMesh, // in
                              int sideBGridCount,  ESMC_Grid *sideBGrid, // in
                              int sideBMeshCount,  ESMC_Mesh *sideBMesh, // in
                              ESMC_InterArrayInt *sideAGridPriority,     // in
                              ESMC_InterArrayInt *sideAMeshPriority,     // in
                              ESMC_InterArrayInt *sideBGridPriority,     // in
                              ESMC_InterArrayInt *sideBMeshPriority,     // in
                              ESMC_InterArrayInt *sideAMaskValues,       // in
                              ESMC_InterArrayInt *sideBMaskValues,       // in
                              int storeOverlay,                          // in
                              int *rc                                    // out
);

// !RETURN VALUE:
//  Newly created ESMC_XGrid object.
//
// !DESCRIPTION:
//
//      Create an {\tt ESMC\_XGrid} object from user supplied input: the list of Grids or Meshes on side A and side B, 
//  and other optional arguments. A user can supply both Grids and Meshes on one side to create
//  the XGrid. By default, the Grids have a higher priority over Meshes but the order of priority 
//  can be adjusted by the optional GridPriority and MeshPriority arguments. The priority order
//  of Grids and Meshes can also be interleaved by rearranging the optional 
//  GridPriority and MeshPriority arguments accordingly.
//  
//  Sparse matrix multiplication coefficients are internally computed and
//  uniquely determined by the Grids or Meshes provided in {\tt sideA} and {\tt sideB}. User can supply
//  a single {\tt ESMC\_Grid} or an array of {\tt ESMC\_Grid} on either side of the 
//  {\tt ESMC\_XGrid}. For an array of {\tt ESMC\_Grid} or {\tt ESMC\_Mesh} in {\tt sideA} or {\tt sideB},
//  a merging process concatenates all the {\tt ESMC\_Grid}s and {\tt ESMC\_Mesh}es 
//  into a super mesh represented
//  by {\tt ESMC\_Mesh}. The super mesh is then used to compute the XGrid. 
//  Grid or Mesh objects in {\tt sideA} and {\tt sideB} arguments must have coordinates defined for
//  the corners of a Grid or Mesh cell. XGrid creation can be potentially memory expensive given the
//  size of the input Grid and Mesh objects. By default, the super mesh is not stored
//  to reduce memory usage. 
// 
//  If {\tt sideA} and {\tt sideB} have a single 
//  Grid or Mesh object, it's erroneous
//  if the two Grids or Meshes are spatially disjoint. 
//  It is also erroneous to specify a Grid or Mesh object in {\tt sideA} or {\tt sideB}
//  that is spatially disjoint from the {\tt ESMC\_XGrid}. 
//
//  This call is {\em collective} across the current VM. For more details please refer to the description 
//  \ref{sec:xgrid:desc} of the XGrid class. 
//
//     The arguments are:
//     \begin{description}
//     \item [sideAGridCount]
//           The number of Grids in the {\tt sideAGrid} array.
//     \item [{[sideAGrid]}]
//           Parametric 2D Grids on side A, for example, 
//           these Grids can be either Cartesian 2D or Spherical.
//     \item [sideAMeshCount]
//           The number of Meshes in the {\tt sideAMesh} array.
//     \item [{[sideAMesh]}]
//           Parametric 2D Meshes on side A, for example, 
//           these Meshes can be either Cartesian 2D or Spherical.
//     \item [sideBGridCount]
//           The number of Grids in the {\tt sideBGrid} array.
//     \item [{[sideBGrid]}]
//           Parametric 2D Grids on side B, for example, 
//           these Grids can be either Cartesian 2D or Spherical.
//     \item [sideBMeshCount]
//           The number of Meshes in the {\tt sideBMesh} array.
//     \item [{[sideBMesh]}]
//           Parametric 2D Meshes on side B, for example, 
//           these Meshes can be either Cartesian 2D or Spherical.
//     \item [{[sideAGridPriority]}]
//           Priority array of Grids on {\tt sideA} during overlay generation.
//           The priority arrays describe the priorities of Grids at the overlapping region.
//           Flux contributions at the overlapping region are computed in the order from the Grid of the
//           highest priority to the lowest priority.
//     \item [{[sideAMeshPriority]}]
//           Priority array of Meshes on {\tt sideA} during overlay generation.
//           The priority arrays describe the priorities of Meshes at the overlapping region.
//           Flux contributions at the overlapping region are computed in the order from the Mesh of the
//           highest priority to the lowest priority.
//     \item [{[sideBGridPriority]}]
//           Priority of Grids on {\tt sideB} during overlay generation
//           The priority arrays describe the priorities of Grids at the overlapping region.
//           Flux contributions at the overlapping region are computed in the order from the Grid of the
//           highest priority to the lowest priority.
//     \item [{[sideBMeshPriority]}]
//           Priority array of Meshes on {\tt sideB} during overlay generation.
//           The priority arrays describe the priorities of Meshes at the overlapping region.
//           Flux contributions at the overlapping region are computed in the order from the Mesh of the
//           highest priority to the lowest priority.
//     \item [{[sideAMaskValues]}]
//           Mask information can be set in the Grid (see~\ref{sec:usage:items}) or Mesh
//           upon which the Field is built. The {\tt sideAMaskValues} argument specifies the values in that 
//           mask information which indicate a point should be masked out. In other words, a location is masked if and only if the
//           value for that location in the mask information matches one of the values listed in {\tt sideAMaskValues}.  
//           If {\tt sideAMaskValues} is not specified, no masking on side A will occur. 
//     \item [{[sideBMaskValues]}]
//           Mask information can be set in the Grid (see~\ref{sec:usage:items}) or Mesh
//           upon which the Field is built. The {\tt sideBMaskValues} argument specifies the values in that 
//           mask information which indicate a point should be masked out. In other words, a location is masked if and only if the
//           value for that location in the mask information matches one of the values listed in {\tt sideBMaskValues}.  
//           If {\tt sideBMaskValues} is not specified, no masking on side B will occur. 
//     \item [storeOverlay]
//           Setting the {\tt storeOverlay} optional argument to 0. 
//           allows a user to bypass storage of the Mesh used to represent the XGrid.
//           Only a DistGrid is stored to allow Field to be built on the XGrid.
//           If the temporary mesh object is of interest, {\tt storeOverlay} can be set to a value not equal to 0.
//           so a user can retrieve it for future use.
//     \item [{[rc]}]
//           Return code; equals {\tt ESMF\_SUCCESS} only if the {\tt ESMF\_XGrid} 
//           is created.
//     \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_XGridDestroy - Destroy a XGrid
//
// !INTERFACE:
int ESMC_XGridDestroy(
  ESMC_XGrid *xgrid     // inout
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Releases all resources associated with this {\tt ESMC\_XGrid}.
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//
//  The arguments are:
//  \begin{description}
//  \item[xgrid]
//    Destroy contents of this {\tt ESMC\_XGrid}.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_XGridGetSideAGridCount - Get the number of Grids on side A.
//
// !INTERFACE:
int ESMC_XGridGetSideAGridCount(
  ESMC_XGrid xgrid,     // in
  int *rc               // out
);

// !RETURN VALUE:
//  The number of Grids on side A. 
//
// !DESCRIPTION:
//
//  Get the number of Grids on side A. 
//
//  The arguments are:
//  \begin{description}
//  \item[xgrid]
//    The XGrid from which to get the information.
//  \item[{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_XGridGetSideAMeshCount - Get the number of Meshes on side A.
//
// !INTERFACE:
int ESMC_XGridGetSideAMeshCount(
  ESMC_XGrid xgrid,     // in
  int *rc               // out
);

// !RETURN VALUE:
//  The number of Meshes on side A. 
//
// !DESCRIPTION:
//
//  Get the number of Meshes on side A. 
//
//  The arguments are:
//  \begin{description}
//  \item[xgrid]
//    The XGrid from which to get the information.
//  \item[{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_XGridGetSideBGridCount - Get the number of Grids on side B.
//
// !INTERFACE:
int ESMC_XGridGetSideBGridCount(
  ESMC_XGrid xgrid,     // in
  int *rc               // out
);

// !RETURN VALUE:
//  The number of Grids on side B. 
//
// !DESCRIPTION:
//
//  Get the number of Grids on side B. 
//
//  The arguments are:
//  \begin{description}
//  \item[xgrid]
//    The XGrid from which to get the information.
//  \item[{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_XGridGetSideBMeshCount - Get the number of Meshes on side B.
//
// !INTERFACE:
int ESMC_XGridGetSideBMeshCount(
  ESMC_XGrid xgrid,     // in
  int *rc               // out
);

// !RETURN VALUE:
//  The number of Meshes on side B. 
//
// !DESCRIPTION:
//
//  Get the number of Meshes on side B. 
//
//  The arguments are:
//  \begin{description}
//  \item[xgrid]
//    The XGrid from which to get the information.
//  \item[{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_XGridGetDimCount - Get the dimension of the XGrid.
//
// !INTERFACE:
int ESMC_XGridGetDimCount(
  ESMC_XGrid xgrid,     // in
  int *rc               // out
);

// !RETURN VALUE:
//  The dimension of the XGrid.
//
// !DESCRIPTION:
//
//   Get the dimension of the XGrid.
//
//  The arguments are:
//  \begin{description}
//  \item[xgrid]
//    The XGrid from which to get the information.
//  \item[{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_XGridGetElementCount - Get the number of elements in the XGrid.
//
// !INTERFACE:
int ESMC_XGridGetElementCount(
  ESMC_XGrid xgrid,     // in
  int *rc               // out
);

// !RETURN VALUE:
//     The number of elements in the XGrid.
//
// !DESCRIPTION:
//
//    Get the number of elements in the XGrid.
//
//  The arguments are:
//  \begin{description}
//  \item[xgrid]
//    The XGrid from which to get the information.
//  \item[{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_XGridGetMesh - Get the Mesh representation of the XGrid. 
//
// !INTERFACE:
ESMC_Mesh ESMC_XGridGetMesh(
  ESMC_XGrid xgrid,     // in
  int *rc               // out
);

// !RETURN VALUE:
//  The ESMC_Mesh object representing the XGrid. 
//
// !DESCRIPTION:
//
//  Get the ESMC\_Mesh object representing the XGrid. 
//
//  The arguments are:
//  \begin{description}
//  \item[xgrid]
//    The xgrid from which to get the information. 
//  \item[{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_XGridGetElementArea - Get the area of elements in the XGrid.
//
// !INTERFACE:
void ESMC_XGridGetElementArea(
  ESMC_XGrid xgrid,     // in
  ESMC_R8 *area,        // out
  int *rc               // out
);

// !RETURN VALUE:
//     The number of elements in the XGrid.
//
// !DESCRIPTION:
//
//    Get the number of elements in the XGrid.
//
//  The arguments are:
//  \begin{description}
//  \item[xgrid]
//    The XGrid from which to get the information.
//  \item[area]
//    An array to fill with element areas. The array must be allocated
//    to size elementCount.
//  \item[{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_XGridGetElementCentroid - Get the centroid of elements in the XGrid.
//
// !INTERFACE:
void ESMC_XGridGetElementCentroid(
  ESMC_XGrid xgrid,     // in
  ESMC_R8 *centroid,    // out
  int *rc               // out
);

// !RETURN VALUE:
//     The number of elements in the XGrid.
//
// !DESCRIPTION:
//
//    Get the centroid for each element in the exchange grid. 
//
//  The arguments are:
//  \begin{description}
//  \item[xgrid]
//    The XGrid from which to get the information.
//  \item[centroid]
//    An array to fill with element centroids. The array must be allocated
//    to size elementCount*dimCount.
//  \item[{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_XGridGetSparseMatA2X - Get the sparse matrix that goes from a side A grid to the exchange grid.

//
// !INTERFACE:
void ESMC_XGridGetSparseMatA2X(
                               ESMC_XGrid xgrid,      // in
                               int sideAIndex,        // in
                               int *factorListCount,  // out
                               double **factorList,   // out
                               int **factorIndexList, // out
                               int *rc);

// !RETURN VALUE:
//     N/A
//
// !DESCRIPTION:
// 
//    Get the sparse matrix that goes from a side A grid to the exchange grid.
//
//  The arguments are:
//  \begin{description}
//  \item[xgrid]
//    The XGrid from which to get the information.
//  \item[sideAIndex]
//    The 0 based index of the Grid/Mesh on side A to get the sparse matrix for.
//    If a priority has been specified for Grids and Meshes, then this index is 
//    in priority order. If no priority was specified, then the all the Grids are
//    first followed by all the Meshes in the order they were passed into the XGrid 
//    create call. 
//  \item[factorListCount]
//    The size of the sparse matrix.
//  \item[factorList]
//    A pointer to the list of factors for the requested sparse matrix. 
//    The list is of size {\tt factorListCount}. To save space
//    this is a pointer to the internal xgrid memory for this list. 
//    Don't deallocate it. 
//  \item[factorIndexList]
//    A pointer to the list of indices for the requested sparse matrix. 
//    The list is of size 2*{\tt factorListCount}. For each pair of entries
//    in this array: entry 0 is the sequence index in the source grid, and entry 1 is
//    the sequence index in the destination grid. To save space
//    this is a pointer to the internal xgrid memory for this list. 
//    Don't deallocate it. 
//  \item[{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_XGridGetSparseMatA2X - Get the sparse matrix that goes from the exchange grid to a side A grid.

//
// !INTERFACE:
void ESMC_XGridGetSparseMatX2A(
                               ESMC_XGrid xgrid,      // in
                               int sideAIndex,        // in
                               int *factorListCount,  // out
                               double **factorList,   // out
                               int **factorIndexList, // out
                               int *rc);

// !RETURN VALUE:
//     N/A
//
// !DESCRIPTION:
// 
//    Get the sparse matrix that goes from the exchange grid to a side A grid. 
//
//  The arguments are:
//  \begin{description}
//  \item[xgrid]
//    The XGrid from which to get the information.
//  \item[sideAIndex]
//    The 0 based index of the Grid/Mesh on side A to get the sparse matrix for.
//    If a priority has been specified for Grids and Meshes, then this index is 
//    in priority order. If no priority was specified, then the all the Grids are
//    first followed by all the Meshes in the order they were passed into the XGrid 
//    create call. 
//  \item[factorListCount]
//    The size of the sparse matrix.
//  \item[factorList]
//    A pointer to the list of factors for the requested sparse matrix. 
//    The list is of size {\tt factorListCount}. To save space
//    this is a pointer to the internal xgrid memory for this list. 
//    Don't deallocate it. 
//  \item[factorIndexList]
//    A pointer to the list of indices for the requested sparse matrix. 
//    The list is of size 2*{\tt factorListCount}. For each pair of entries
//    in this array: entry 0 is the sequence index in the source grid, and entry 1 is
//    the sequence index in the destination grid. To save space 
//    this is a pointer to the internal xgrid memory for this list. 
//    Don't deallocate it. 
//  \item[{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_XGridGetSparseMatB2X - Get the sparse matrix that goes from a side B grid to the exchange grid.

//
// !INTERFACE:
void ESMC_XGridGetSparseMatB2X(
                               ESMC_XGrid xgrid,      // in
                               int sideBIndex,        // in
                               int *factorListCount,  // out
                               double **factorList,   // out
                               int **factorIndexList, // out
                               int *rc);

// !RETURN VALUE:
//     N/A
//
// !DESCRIPTION:
// 
//    Get the sparse matrix that goes from a side B grid to the exchange grid.
//
//  The arguments are:
//  \begin{description}
//  \item[xgrid]
//    The XGrid from which to get the information.
//  \item[sideBIndex]
//    The 0 based index of the Grid/Mesh on side B to get the sparse matrix for.
//    If a priority has been specified for Grids and Meshes, then this index is 
//    in priority order. If no priority was specified, then the all the Grids are
//    first followed by all the Meshes in the order they were passed into the XGrid 
//    create call. 
//  \item[factorListCount]
//    The size of the sparse matrix.
//  \item[factorList]
//    A pointer to the list of factors for the requested sparse matrix. 
//    The list is of size {\tt factorListCount}. To save space
//    this is a pointer to the internal xgrid memory for this list. 
//    Don't deallocate it. 
//  \item[factorIndexList]
//    A pointer to the list of indices for the requested sparse matrix. 
//    The list is of size 2*{\tt factorListCount}. For each pair of entries
//    in this array: entry 0 is the sequence index in the source grid, and entry 1 is
//    the sequence index in the destination grid. To save space
//    this is a pointer to the internal xgrid memory for this list. 
//    Don't deallocate it. 
//  \item[{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_XGridGetSparseMatB2X - Get the sparse matrix that goes from the exchange grid to a side B grid.

//
// !INTERFACE:
void ESMC_XGridGetSparseMatX2B(
                               ESMC_XGrid xgrid,      // in
                               int sideBIndex,        // in
                               int *factorListCount,  // out
                               double **factorList,   // out
                               int **factorIndexList, // out
                               int *rc);

// !RETURN VALUE:
//     N/A
//
// !DESCRIPTION:
// 
//    Get the sparse matrix that goes from the exchange grid to a side B grid. 
//
//  The arguments are:
//  \begin{description}
//  \item[xgrid]
//    The XGrid from which to get the information.
//  \item[sideBIndex]
//    The 0 based index of the Grid/Mesh on side B to get the sparse matrix for.
//    If a priority has been specified for Grids and Meshes, then this index is 
//    in priority order. If no priority was specified, then the all the Grids are
//    first followed by all the Meshes in the order they were passed into the XGrid 
//    create call. 
//  \item[factorListCount]
//    The size of the sparse matrix.
//  \item[factorList]
//    A pointer to the list of factors for the requested sparse matrix. 
//    The list is of size {\tt factorListCount}. To save space
//    this is a pointer to the internal xgrid memory for this list. 
//    Don't deallocate it. 
//  \item[factorIndexList]
//    A pointer to the list of indices for the requested sparse matrix. 
//    The list is of size 2*{\tt factorListCount}. For each pair of entries
//    in this array: entry 0 is the sequence index in the source grid, and entry 1 is
//    the sequence index in the destination grid. To save space 
//    this is a pointer to the internal xgrid memory for this list. 
//    Don't deallocate it. 
//  \item[{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------




#if defined (__cplusplus)
} // extern "C"
#endif

#endif
