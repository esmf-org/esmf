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

#ifndef ESMC_Grid_h
#define ESMC_Grid_h

//-----------------------------------------------------------------------------
//
//  !CLASS ESMC_Grid - Public C interface to the ESMF Grid class
//
// !DESCRIPTION:
//
// The code in this file defines the public C Grid class and declares method
// signatures (prototypes).  The companion file {\tt ESMC\_Grid.C} contains
// the definitions (full code bodies) for the Grid methods.
//
//
//-----------------------------------------------------------------------------

#include "ESMC_Interface.h"
#include "ESMC_Util.h"

#if defined (__cplusplus)
extern "C" {
#endif

// Class declaration type
typedef struct{
  void *ptr;
}ESMC_Grid;

// Class API


//------------------------------------------------------------------------------
//TODO: InterArray should be passed by value when ticket 3613642 is resolved
//BOP
// !IROUTINE: ESMC_GridCreateNoPeriDim - Create a Grid with no periodic dimensions
//
// !INTERFACE:
ESMC_Grid ESMC_GridCreateNoPeriDim(
  ESMC_InterArrayInt *maxIndex,           // in
  enum ESMC_CoordSys_Flag *coordSys,      // in
  enum ESMC_TypeKind_Flag *coordTypeKind, // in
  enum ESMC_IndexFlag *indexflag,         // in
  int *rc                                 // out
);
// !RETURN VALUE:
//  type(ESMC_Grid)
//
// !DESCRIPTION:
//
//  This call creates an ESMC\_Grid with no periodic dimensions.
//
//  The arguments are:
//  \begin{description}
//  \item[maxIndex]
//      The upper extent of the grid array.
//  \item[coordSys]
//      The coordinated system of the grid coordinate data. If not specified then
//      defaults to ESMF\_COORDSYS\_SPH\_DEG.
//  \item[coordTypeKind]
//      The type/kind of the grid coordinate data.  If not specified then the
//      type/kind will be 8 byte reals.
//  \item[indexflag]
//      Indicates the indexing scheme to be used in the new Grid. If not present,
//      defaults to ESMC\_INDEX\_DELOCAL.
//  \item[rc]
//      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TODO: InterArray should be passed by value when ticket 3613642 is resolved
//BOP
// !IROUTINE: ESMC_GridCreate1PeriDim - Create a Grid with 1 periodic dimension
//
// !INTERFACE:
ESMC_Grid ESMC_GridCreate1PeriDim(
  ESMC_InterArrayInt *maxIndex,           // in
  ESMC_InterArrayInt *polekindflag,       // in
  int *periodicDim,                       // in
  int *poleDim,                           // in
  enum ESMC_CoordSys_Flag *coordSys,      // in
  enum ESMC_TypeKind_Flag *coordTypeKind, // in
  enum ESMC_IndexFlag *indexflag,         // in
  int *rc                                 // out
);
// !RETURN VALUE:
//  type(ESMC_Grid)
//
// !DESCRIPTION:
//
//  This call creates an ESMC\_Grid with 1 periodic dimension.
//
//  The arguments are:
//  \begin{description}
//  \item[maxIndex]
//      The upper extent of the grid array.
//  \item[polekindflag]
//      Two item array which specifies the type of connection which occurs at the
//      pole. polekindflag(1) the connection that occurs at the minimum end of the
//      index dimension. polekindflag(2) the connection that occurs at the maximum
//      end of the index dimension. If not specified, the default is
//      ESMF\_POLETYPE\_MONOPOLE for both.
//  \item[periodicDim]
//      The periodic dimension.  If not specified, defaults to 1.
//  \item[poleDim]
//      The dimension at which the poles are located at the ends.  If not
//      specified, defaults to 2.
//  \item[coordSys]
//      The coordinated system of the grid coordinate data. If not specified then
//      defaults to ESMF\_COORDSYS\_SPH\_DEG.
//  \item[coordTypeKind]
//      The type/kind of the grid coordinate data.  If not specified then the
//      type/kind will be 8 byte reals.
//  \item[indexflag]
//      Indicates the indexing scheme to be used in the new Grid. If not present,
//      defaults to ESMC\_INDEX\_DELOCAL.
//  \item[rc]
//      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TODO: InterArray should be passed by value when ticket 3613642 is resolved
//BOP
// !IROUTINE: ESMC_GridCreateCubedSphere - Create a cubed sphere Grid
//
// !INTERFACE:
ESMC_Grid ESMC_GridCreateCubedSphere(
  int *tilesize,                      // in
  ESMC_InterArrayInt *regDecompPTile, // in
  //ESMC_InterArrayInt *decompFlagPTile,  // in
  //ESMC_InterArrayInt *deLabelList,    // in
  //ESMC_DELayout *delayout,            // in
  ESMC_InterArrayInt *staggerLocList,   // in
  const char *name,                   // in
  int *rc);                           // out
// !RETURN VALUE:
//  type(ESMC_Grid)
//
// !DESCRIPTION:
//
//  Create a six-tile {\tt ESMC\_Grid} for a cubed sphere grid using regular
//  decomposition.  Each tile can have different decomposition. The grid
//  coordinates are generated based on the algorithm used by GEOS-5. The tile
//  resolution is defined by tileSize.
//
//  The arguments are:
//  \begin{description}
//  \item[tilesize]
//      The number of elements on each side of the tile of the cubed sphere grid.
//  \item[regDecompPTile]
//      List of DE counts for each dimension. The second index steps through
//      the tiles. The total {\tt deCount} is determined as the sum over
//      the products of {\tt regDecompPTile} elements for each tile.
//      By default every tile is decomposed in the same way.  If the total
//      PET count is less than 6, one tile will be assigned to one DE and the DEs
//      will be assigned to PETs sequentially, therefore, some PETs may have
//      more than one DE. If the total PET count is greater than 6, the total
//      number of DEs will be a multiple of 6 and less than or equal to the total
//      PET count. For instance, if the total PET count is 16, the total DE count
//      will be 12 with each tile decomposed into 1x2 blocks. The 12 DEs are mapped
//      to the first 12 PETs and the remaining 4 PETs have no DEs locally, unless
//      an optional {\tt delayout} is provided.
//  \item[staggerLocList]
//      The list of stagger locations to fill with coordinates. Only {\tt ESMF\_STAGGERLOC\_CENTER} and
//      {\tt ESMF\_STAGGERLOC\_CORNER} are supported. If not present, no coordinates will be added or filled.
//  \item[name]
//      The name of the {\tt ESMC\_Grid}.
//  \item[rc]
//      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

// these ones are not yet operations

//  \item[decompFlagPTile]
//      List of decomposition flags indicating how each dimension of each
//      tile is to be divided between the DEs. The default setting
//      is {\tt ESMC\_DECOMP\_BALANCED} in all dimensions for all tiles.
//      See section \ref{const:decompflag} for a list of valid decomposition
//      flag options. The second index indicates the tile number.
//  \item[deLabelList]
//      List assigning DE labels to the default sequence of DEs. The default
//      sequence is given by the column major order of the {\tt regDecompPTile}
//      elements in the sequence as they appear following the tile index.
//  \item[delayout]
//      Optional {\tt ESMC\_DELayout} object to be used. By default a new
//      DELayout object will be created with as many DEs as there are PETs,
//      or tiles, which ever is greater. If a DELayout object is specified,
//      the number of DEs must match {\tt regDecompPTile}, if present. In the
//      case that {\tt regDecompPTile} was not specified, the {\tt deCount}
//      must be at least that of the default DELayout. The
//      {\tt regDecompPTile} will be constructed accordingly.

//------------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_GridCreateFromFile - Create a Grid from a NetCDF file specification.
//
// !INTERFACE:
ESMC_Grid ESMC_GridCreateFromFile(const char *filename, int fileTypeFlag, 
                  int *regDecomp, int *decompflag,
                  int *isSphere, ESMC_InterArrayInt *polekindflag,
                  int *addCornerStagger,
                  int *addUserArea, enum ESMC_IndexFlag *indexflag,
                  int *addMask, const char *varname,
                  const char **coordNames, int *rc);
// !RETURN VALUE:
//  type(ESMC_Grid)
//
// !DESCRIPTION:
// This function creates a {\tt ESMC\_Grid} object from the specification in
// a NetCDF file.
//
//  The arguments are:
//  \begin{description}
// \item[filename]
//     The NetCDF Grid filename.
// \item[fileTypeFlag]
//     The Grid file format, please see Section~\ref{const:cfileformat}
//         for a list of valid options. 
// \item[regDecomp] 
//      A 2 element array specifying how the grid is decomposed.
//      Each entry is the number of decounts for that dimension.
//      The total decounts cannot exceed the total number of PETs.  In other
//      word, at most one DE is allowed per processor.
//      If not specified, the default decomposition will be petCountx1.
// \item[{[decompflag]}]
//      List of decomposition flags indicating how each dimension of the
//      tile is to be divided between the DEs. The default setting
//      is {\tt ESMF\_DECOMP\_BALANCED} in all dimensions. Please see
//      Section~\ref{const:cdecompflag} for a full description of the 
//      possible options. 
// \item[{[isSphere]}]
//      Set to 1 for a spherical grid, or 0 for regional. Defaults to 1.
//  \item[polekindflag]
//      Two item array which specifies the type of connection which occurs at the
//      pole. polekindflag(1) the connection that occurs at the minimum end of the
//      index dimension. polekindflag(2) the connection that occurs at the maximum
//      end of the index dimension. If not specified, the default is
//      ESMF\_POLETYPE\_MONOPOLE for both.
// \item[{[addCornerStagger]}]
//      Set to 1 to use the information in the grid file to add the Corner stagger to 
//      the Grid. The coordinates for the corner stagger are required for conservative
//      regridding. If not specified, defaults to 0. 
// \item[{[addUserArea]}]
//      Set to 1 to read in the cell area from the Grid file; otherwise, ESMF will 
//      calculate it.  This feature is only supported when the grid file is in the SCRIP
//      format.  
//  \item[indexflag]
//      Indicates the indexing scheme to be used in the new Grid. If not present,
//      defaults to ESMC\_INDEX\_DELOCAL.
// \item[{[addMask]}]
//      Set to 1 to generate the mask using the missing\_value attribute defined in 'varname'.
//      This flag is only needed when the grid file is in the GRIDSPEC format.
// \item[{[varname]}]
//      If addMask is non-zero, provide a variable name stored in the grid file and
//      the mask will be generated using the missing value of the data value of
//      this variable.  The first two dimensions of the variable has to be the
//      longitude and the latitude dimension and the mask is derived from the
//      first 2D values of this variable even if this data is 3D, or 4D array.
//\item[{[coordNames]}]
//      A two-element array containing the longitude and latitude variable names in a
//      GRIDSPEC file if there are multiple coordinates defined in the file.
// \item[{[rc]}]
//      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//------------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_GridDestroy - Destroy a Grid
//
// !INTERFACE:
int ESMC_GridDestroy(
  ESMC_Grid *grid             // in
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//  Destroy the Grid.
//
//  The arguments are:
//  \begin{description}
//  \item[grid]
//    Grid object whose memory is to be freed. 
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//------------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_GridAddItem - Add items to a Grid
//
// !INTERFACE:
int ESMC_GridAddItem(
  ESMC_Grid grid,                   // in
  enum ESMC_GridItem_Flag itemflag, // in
  enum ESMC_StaggerLoc staggerloc   // in
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//  Add an item (e.g. a mask) to the Grid.
//
//  The arguments are:
//  \begin{description}
//  \item[grid]
//    Grid object to which the coordinates will be added
//  \item[itemflag]
//    The grid item to add.
//  \item[staggerloc]
//    The stagger location to add.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//------------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_GridGetItem - Get item from a Grid
//
// !INTERFACE:
void * ESMC_GridGetItem(
  ESMC_Grid grid,                         // in
  enum ESMC_GridItem_Flag itemflag,       // in
  enum ESMC_StaggerLoc staggerloc,        // in
  int *localDE,                           // in
  int *rc                                 // out
);

// !RETURN VALUE:
//  A pointer to the item data. 
//
// !DESCRIPTION:
//  Get a pointer to item data (e.g. mask data) in the Grid.
//
//  The arguments are:
//  \begin{description}
//  \item[grid]
//    Grid object from which to obtain the coordinates.
//  \item[itemflag]
//    The grid item to add.
//  \item[staggerloc]
//    The stagger location to add.
//  \item[localDE]
//    The local decompositional element. If not present, defaults to 0.
//  \item[rc]
//  Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//------------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_GridAddCoord - Add coordinates to a Grid
//
// !INTERFACE:
int ESMC_GridAddCoord(
  ESMC_Grid grid,                   // in
  enum ESMC_StaggerLoc staggerloc   // in
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//  Add coordinates to the Grid.
//
//  The arguments are:
//  \begin{description}
//  \item[grid]
//    Grid object to which the coordinates will be added
//  \item[staggerloc]
//    The stagger location to add.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//------------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_GridGetCoord - Get coordinates from a Grid
//
// !INTERFACE:
void * ESMC_GridGetCoord(
  ESMC_Grid grid,                         // in
  int coordDim,                           // in
  enum ESMC_StaggerLoc staggerloc,        // in
  int *localDE,
  int *exclusiveLBound,                   // out
  int *exclusiveUBound,                   // out
  int *rc                                 // out
);

// !RETURN VALUE:
//  A pointer to coordinate data in the Grid. 
//
// !DESCRIPTION:
//  Get a pointer to coordinate data in the Grid.
//
//  The arguments are:
//  \begin{description}
//  \item[grid]
//    Grid object from which to obtain the coordinates.
//  \item[coordDim]
//    The coordinate dimension from which to get the data.
//  \item[staggerloc]
//    The stagger location to add.
//  \item[localDE]
//    The local decompositional element. If not present, defaults to 0.
//  \item[exclusiveLBound]
//    Upon return this holds the lower bounds of the exclusive region. This bound
//    must be allocated to be of size equal to the coord dimCount.  
//  \item[exclusiveUBound]
//    Upon return this holds the upper bounds of the exclusive region. This bound
//    must be allocated to be of size equal to the coord dimCount.  
//  \item[rc]
//  Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//------------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_GridGetCoordBounds - Get coordinate bounds from a Grid
//
// !INTERFACE:
int ESMC_GridGetCoordBounds(
  ESMC_Grid grid,                         // in
  enum ESMC_StaggerLoc staggerloc,        // in
  int *localDE,                           // in
  int *exclusiveLBound,                   // out
  int *exclusiveUBound,                   // out
  int *rc                                 // out
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//  Get coordinates bounds from the Grid.
//
//  The arguments are:
//  \begin{description}
//  \item[grid]
//    Grid object from which to obtain the coordinates.
//  \item[staggerloc]
//    The stagger location to add.
//  \item[localDE]
//    The local decompositional element. If not present, defaults to 0.
//  \item[exclusiveLBound]
//    Upon return this holds the lower bounds of the exclusive region. This bound
//    must be allocated to be of size equal to the coord dimCount.  
//  \item[exclusiveUBound]
//    Upon return this holds the upper bounds of the exclusive region. This bound
//    must be allocated to be of size equal to the coord dimCount.  
//  \item[rc]
//  Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//------------------------------------------------------------------------------
//BOPI
// !IROUTINE: ESMC_GridWrite - Write a Grid to a VTK file
//
// !INTERFACE:
int ESMC_GridWrite(
  ESMC_Grid grid,                  // in
  enum ESMC_StaggerLoc staggerloc, // in
  const char* fname                // in
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
//
// !DESCRIPTION:
//   Write a grid to VTK file.
//
// The arguments are:
//   \begin{description}
//   \item [grid]
//     The grid.
//   \item[staggerloc]
//     The stagger location to add.
//   \item[filename]
//     The name of the output file.
//   \end{description}
//
//EOPI
//-----------------------------------------------------------------------------

#if defined (__cplusplus)
} // extern "C"
#endif

#endif  // ESMC_Grid_h
