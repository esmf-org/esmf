// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2022, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------

#include <string>
#include <ostream>
#include <iterator>
#include <algorithm>

#include "ESMCI_Macros.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"
#include "ESMCI_CoordSys.h"
#include "ESMCI_Array.h"
#include "ESMC_Util.h"

#include "ESMCI_TraceMacros.h"  // for profiling

#include "Mesh/include/ESMCI_Pgon.h"
#include "Mesh/include/ESMCI_MathUtil.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id$";
//-----------------------------------------------------------------------------
using namespace ESMCI;


// Fill a Pgon from an elem
// TO THINK ABOUT: Could also have another method that creates a Pgon from an elem, but this seems more mem efficient because you can reuse Pgons, so
//                 don't make the other unless it becomes useful in some circumstances.

template<class GEOM>
void  Pgon_fill_from_elem(Pgon<GEOM> &pgon, const MeshObj *elem, const MEField<>  *cfield) {

  // Get dimension of pgon pnts
  int pgon_pnt_dim=pgon.get_pnt_size();
  
  // Get topo from element
  const ESMCI::MeshObjTopo *topo = ESMCI::GetMeshObjTopo(*elem);
  
  // Error check elem dim
  if (topo->spatial_dim != pgon_pnt_dim) Throw() << "This mesh has the wrong spatial dimension ("<<topo->spatial_dim<<") to create a "<<pgon_pnt_dim<<"D polygon.";

  // Temp space for polygon
#define MAX_PNTS_IN_POLY 6
#define MAX_PNT_DIM 3
  double tmp_coords[MAX_PNT_DIM*MAX_PNTS_IN_POLY];
  int tmp_num_pnts;

  // Error check pnt dim
  if (pgon_pnt_dim > MAX_PNT_DIM) Throw() << "Pgon point dimension ("<<pgon_pnt_dim<<") is larger than max supported dim.";

  // Get coords
  get_elem_coords(elem, cfield, pgon_pnt_dim, MAX_PNTS_IN_POLY, &tmp_num_pnts, tmp_coords);

#undef MAX_PNTS_IN_POLY
#undef MAX_PNT_DIM
  
  // Clear Pgon
  pgon.clear();
  
  // Reserve space in Pgon
  pgon.reserve(tmp_num_pnts);
  
  // Iterate and fill Pgon
  double *curr_coord_pos=tmp_coords;
  for (int i=0; i<tmp_num_pnts; i++) {
    pgon.push_back_pnt(curr_coord_pos);
    curr_coord_pos += pgon_pnt_dim;
  }
}



