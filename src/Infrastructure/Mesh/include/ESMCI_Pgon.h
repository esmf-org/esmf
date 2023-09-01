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
class Vert {

  double pnt[GEOM::pnt_size];

  Vert *next, *prev;

  Vert(double x, double y): next(NULL), prev(NULL) {
    pnt[0]=x;
    pnt[1]=y;
  }

  Vert(double x, double y, double z): next(NULL), prev(NULL) {
    if (GEOM::pnt_size == 2) Throw() << "This instance of Pgon only supports 2D points.";
    pnt[0]=x;
    pnt[1]=y;
    pnt[2]=z;
  }

  Vert(double *_pnt): next(NULL), prev(NULL) {

    // Add point 0 & 1
    pnt[0]=_pnt[0];
    pnt[1]=_pnt[1];

    // Maybe add 3
    if (GEOM::pnt_size > 2) pnt[2]=_pnt[2];

    // Only support 3 coords right now, this is error checked in Pgon creation
  }
  
  void add_next(Vert *v) {
    
    // Point new vert appropriately 
    v->next=next;
    v->prev=this;
    
    // If there's something after, point it's prev to this
    if (next != NULL) {
      next->prev=v;
    }

    // Make the next node the new one
    next=v;
  }

 void make_first() {
    
   // Point new node appropriately 
   next=this;
   prev=this;
  }
 
  
};


template <class GEOM2>
class Pgon {

  // Buffer for holding Coords when they need to be passed someplace
  std::vector<double> pnt_coords;

  Vert<GEOM2> *beg,*end; // List of vertices

  int num_pnts; // Size of polygon
  
public:

  
  // Full constructor
  Pgon(): beg(NULL), end(NULL), num_pnts(0) {

    // Error check number of coords
    if ((GEOM2::pnt_size != 2) && (GEOM2::pnt_size != 3)) Throw() << "Pgon only supports 2D or 3D points.";
  }

  // Clear points
  void clear() {
    pnt_coords.clear();
  }
  
  // Reserve to add future points
  void reserve(int num_pnts) {
    pnt_coords.reserve(num_pnts*GEOM2::pnt_size);
  }

  void push_back_Vert(Vert<GEOM2> *vert) {
    // If empty, then just make the only one
    if (beg == NULL) {
      vert->make_first();
      beg=end=vert;      
    } else { // Add at end
      end->add_next(vert);
      end=vert;
    }

    // Increase the number of points
    num_pnts++;
  }
  
  // Add a point
  void push_back_pnt(double *pnt) {

    // create vert
    Vert<GEOM2> *vert=new Vert<GEOM2>(pnt);

    // Add
    push_back_Vert(vert);
  }

  // Add a point 2D case
  void push_back_pnt(double x, double y) {

    // create vert
    Vert<GEOM2> *vert=new Vert<GEOM2>(x,y);

    // Add
    push_back_Vert(vert);
  }

  // Add a point 3D case
  void push_back_pnt(double x, double y, double z) {

    // create vert
    Vert<GEOM2> *vert=new Vert<GEOM2>(x,y,z);

    // Add
    push_back_Vert(vert);
  }

  
  
  // Calc area of the polygon
  double area() {

    // if not empty, return area
    if (!pnt_coords.empty()) {
      GEOM2::calc_area_polygon(pnt_coords.size(), pnt_coords.data()); 
    } else { // else return 0
      return 0.0;
    }
  }


  // Get methods
  int get_num_pnts() {return pnt_coords.size()/GEOM2::pnt_size;}  
  int get_pnt_size() {return GEOM2::pnt_size;}

  double *get_pnt_coord(int i) {return GEOM2::pnt_size*i+pnt_coords.data();}

  // Returns a direct pointer to the coordinate memory
  // (coordinates for a given point are stored next to each other in memory) 
  double *get_coord_mem() {return pnt_coords.data();}

  // Write to vtk file for debuggin
  void write_to_vtk(const char *filename);

  // Compute intersection of Pgons
  static void intersection(Pgon<GEOM2> &subject, Pgon<GEOM2> &clipper, Pgon<GEOM2> &result);

  // Compute difference of Pgons
  static void difference(Pgon<GEOM2> &subject, Pgon<GEOM2> &clipper, Pgon<GEOM2> &result);


  // Eventually add these, where the result ends up in the Pgon object.
  // This would be more efficient because in repeated use you wouldn't
  // have to keep recreating the subject polygon
#if 0

  // Compute intersection of Pgons
  void intersection(Pgon<GEOM2> &other);

  // Compute difference of Pgons
  void difference(Pgon<GEOM2> &clipper);

 
#endif

};



#endif // ESMCI_PGON_H
