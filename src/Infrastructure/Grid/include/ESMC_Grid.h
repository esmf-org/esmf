// $Id: ESMC_Grid.h,v 1.19 2012/03/15 19:24:17 rokuingh Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2012, University Corporation for Atmospheric Research, 
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
//BOP
// !IROUTINE: ESMC_GridCreateNoPeriDim - Create a Grid with no periodic dimensions
//
// !INTERFACE:
ESMC_Grid ESMC_GridCreateNoPeriDim(
  ESMC_InterfaceInt maxIndex,  // in
  enum ESMC_CoordSys coordSys,      // in
  enum ESMC_TypeKind coordTypeKind, // in
  int *rc                      // out
);
// !RETURN VALUE:
//  type(ESMC_Grid)
//
// !DESCRIPTION:
//
//  This call creates an ESMC_Grid with no periodic dimensions.
//
//  The arguments are:
//  \begin{description}
//  \item[maxIndex]
//  The upper extent of the grid array.
//  \item[coordSys]
//  The coordinated system of the grid coordinate data. If not specified then
//  defaults to ESMF\_COORDSYS\_SPH\_DEG.
//  \item[coordTypeKind]
//  The type/kind of the grid coordinate data.  If not specified then the
//  type/kind will be 8 byte reals.
//  \item[rc]
//  Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//------------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_GridCreate1PeriDim - Create a Grid with 1 periodic dimension
//
// !INTERFACE:
ESMC_Grid ESMC_GridCreate1PeriDim(
  ESMC_InterfaceInt maxIndex,  // in
  enum ESMC_CoordSys coordSys,      // in
  enum ESMC_TypeKind coordTypeKind, // in
  int *rc                      // out
);
// !RETURN VALUE:
//  type(ESMC_Grid)
//
// !DESCRIPTION:
//
//  This call creates an ESMC_Grid with 1 periodic dimension.
//
//  The arguments are:
//  \begin{description}
//  \item[maxIndex]
//  The upper extent of the grid array.
//  \item[coordSys]
//  The coordinated system of the grid coordinate data. If not specified then
//  defaults to ESMF\_COORDSYS\_SPH\_DEG.
//  \item[coordTypeKind]
//  The type/kind of the grid coordinate data.  If not specified then the
//  type/kind will be 8 byte reals.
//  \item[rc]
//  Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
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
//    The stagger location to add.  If not present, defaults to ESMC_STAGGERLOC_CENTER.
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
  int *exclusiveLBound,                   // out
  int *exclusiveUBound,                   // out
  int *rc                                 // out
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//  Get coordinatess from the Grid.
//
//  The arguments are:
//  \begin{description}
//  \item[grid]
//    Grid object from which to obtain the coordinates.
//  \item[coordDim]
//    The coordinate dimension from which to get the data.
//  \item[staggerloc]
//    The stagger location to add.  If not present, defaults to ESMC_STAGGERLOC_CENTER.
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

#if defined (__cplusplus)
} // extern "C"
#endif

#endif  // ESMC_Mesh_h
