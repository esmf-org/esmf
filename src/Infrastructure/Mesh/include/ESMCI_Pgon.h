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

#ifndef ESMCI_PGON_H
#define ESMCI_PGON_H

#include <vector>

#include "ESMCI_Macros.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"
#include "ESMCI_CoordSys.h"
#include "ESMCI_Array.h"
#include "ESMCI_DistGrid.h"

#include "Mesh/include/ESMCI_MathUtil.h"
#include <Mesh/include/Legacy/ESMCI_Exception.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
// static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


using namespace ESMCI;

template <class GEOM>
class Pgon {

  // Coords
  std::vector<double> pnt_coords;

  // is this a hole? 
  bool is_hole;

public:

  
  // Full constructor
  Pgon(bool _is_hole): is_hole(_is_hole) {

    // Error check number of coords
    if ((GEOM::pnt_size != 2) && (GEOM::pnt_size != 3)) Throw() << "Pgon only supports 2D or 3D points.";
  }

  // Default constructor
  Pgon():Pgon(false) { }

  // Clear points
  void clear() {
    pnt_coords.clear();
  }
  
  // Reserve to add future points
  void reserve(int num_pnts) {
    pnt_coords.reserve(num_pnts*GEOM::pnt_size);
  }

 
  // Add a point
  void push_back_pnt(double *pnt) {

    // Add point 0 & 1
    pnt_coords.push_back(pnt[0]);
    pnt_coords.push_back(pnt[1]);

    // Maybe add 3
    if (GEOM::pnt_size > 2) pnt_coords.push_back(pnt[2]);

    // Only support 3 coords right now, this is error checked in creation
  }

  // Add a point 2D case
  void push_back_pnt(double x, double y) {

    // Add point 0 & 1
    pnt_coords.push_back(x);
    pnt_coords.push_back(y);

  }

  // Add a point 3D case
  void push_back_pnt(double x, double y, double z) {

    // Add point 0 & 1 & 2
    pnt_coords.push_back(x);
    pnt_coords.push_back(y);
    pnt_coords.push_back(z);
  }

  
  
  // Calc area of the polygon
  double area() {

    // if not empty, return area
    if (!pnt_coords.empty()) {
      GEOM::calc_area_polygon(pnt_coords.size(), pnt_coords.data()); 
    } else { // else return 0
      return 0.0;
    }
  }


  // Get methods
  bool get_is_hole() {return is_hole;}
  int get_num_pnts() {return pnt_coords.size()/GEOM::pnt_size;}  
  int get_pnt_size() {return GEOM::pnt_size;}

  // Returns a direct pointer to the coordinate memory
  // (coordinates for a given point are stored next to each other in memory) 
  double *get_coord_mem() {return pnt_coords.data();}

  // Write to vtk file for debuggin
  void write_to_vtk(const char *filename);

  // Compute difference of Pgons
  static void difference(Pgon<GEOM> subject, Pgon<GEOM> clipper,
                         std::vector< Pgon<GEOM> > reults);

};



#endif // ESMCI_PGON_H
