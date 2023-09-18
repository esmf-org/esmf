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
  
public: 
  
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


  // Debug output
template <class GEOMVO>
  std::ostream &operator<<(std::ostream &os, const Vert<GEOMVO> &v) {

  os<<v.pnt[0];
  os<<" "<<v.pnt[1];
  if (GEOMVO::pnt_size >2) os<<" "<<v.pnt[2];

  return os;
}


template <class GEOMVI>
class VertIter {

private:
  class iterator {
  public: 
    iterator(Vert<GEOMVI> *_beg): beg(_beg), curr(_beg) {}

    const iterator& operator++() {
      next_Vert();
      return *this;
    }

    Vert<GEOMVI> *operator*() {
      return curr;
    }

    bool operator !=(const iterator& rhs) const {
      return (beg != rhs.beg || curr != rhs.beg);
    } 

  private:
    Vert<GEOMVI> *beg;
    Vert<GEOMVI> *curr;
    
    // Go to next Vert in the list
   Vert<GEOMVI> *next_Vert() {

     // If we're at end, just leave
     if (curr == NULL) return NULL;

     // Go to next
     curr=curr->next;

     // If we're back at the beginning, then mark as end
     if (curr == beg) {
       curr=NULL;
       beg=NULL;
     }

     // return current vertex
     return curr;
   }
  };

public:
  VertIter() : beg(NULL) {};

  iterator begin() { return iterator(beg); }
  iterator end() { return iterator(NULL); }

  Vert<GEOMVI> *beg;  
};




template <class GEOM2>
class Pgon {

  public:
  // Buffer for holding Coords when they need to be passed someplace
  std::vector<double> coord_buff;

  Vert<GEOM2> *beg,*end; // List of vertices

  int num_pnts; // Size of polygon

protected:
  VertIter<GEOM2> vertIter;
  

  // private methods 
private: 

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

  ////////////////////////
  /// TODO: Look into having specific template overriding for particular GEOMS for some methods to avoid ifs
  ////////////////////////

  void pack_coords_into_buff() {

    // Leave if no points
    if (num_pnts == 0) return;
    
    // Clear vector
    coord_buff.clear();

    // Reserve the correct amount of mem
    coord_buff.reserve(num_pnts*GEOM2::pnt_size);

    // Loop adding points
    Vert<GEOM2> *v=beg;
    ThrowRequire(v != NULL);
    while (v != end) {
      
      // Pack coords
      coord_buff.push_back(v->pnt[0]);
      coord_buff.push_back(v->pnt[1]);
      if (GEOM2::pnt_size >2) coord_buff.push_back(v->pnt[2]);

      // Go to next
      v=v->next;
    }

    // Do End
    coord_buff.push_back(v->pnt[0]);
    coord_buff.push_back(v->pnt[1]);
    if (GEOM2::pnt_size >2) coord_buff.push_back(v->pnt[2]);
  }


  
public:

  
  // Full constructor
  Pgon(): beg(NULL), end(NULL), num_pnts(0) {

    // Error check number of coords
    if ((GEOM2::pnt_size != 2) && (GEOM2::pnt_size != 3)) Throw() << "Pgon only supports 2D or 3D points.";
  }


  // Loop over vertices
  // TODO: Make this so it makes an independant iterator object? So you can have more than one per polygon
  VertIter<GEOM2> &get_VertIter(Vert<GEOM2> *first=NULL) {
    
    // Set beginning
    if (first == NULL) {
      vertIter.beg=beg;
    } else {
      vertIter.beg=first;
    }

    return vertIter;
  }

  
  
  // Clear points
  // TODO: This should keep Verts, but take out of beg, end and drop num_pnts to 0
  void clear() {
    coord_buff.clear();
  }
  
  // Reserve to add future points
  // TODO: This should preallocate Verts
  void reserve(int num_pnts) {
    coord_buff.reserve(num_pnts*GEOM2::pnt_size);
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

    // Put coords into buffer
    pack_coords_into_buff();
    
    // Return area
    return GEOM2::calc_area_polygon(num_pnts, coord_buff.data()); 
  }
  

  // Get methods
  int get_num_pnts() {return num_pnts;}
  int get_pnt_size() {return GEOM2::pnt_size;}

  double *get_pnt_coord(int i) {return GEOM2::pnt_size*i+coord_buff.data();}

  // Returns a direct pointer to the coordinate memory
  // (coordinates for a given point are stored next to each other in memory) 
  double *get_coord_buff() {return coord_buff.data();}

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

  // Debug output
template <class GEOM3>
  std::ostream &operator<<(std::ostream &os, Pgon<GEOM3> &pg) {

  // Output Pgon Type
  os<<"Type: ";
  if (GEOM3::pnt_size==2) os<<"CART_2D";
  else os<<"SPH_2D3D";
  os<<" ";

  // Output Pgon size
  os<<"num_pnts= "<<pg.num_pnts;

  // Next line
  os<<"\n";

  // Vertices
  os << "Vertices:\n";
  if (pg.beg == NULL) return (os); // Leave if empty

  // Loop outputting vertices
  for (Vert<GEOM3> *v : pg.get_VertIter()) {

    // Output Vert
    os<<"  ["<<*v<<"] ";
    
    // Next line
    os<<"\n";
  }
    
  return(os);
}


// OLD WAY
#if 0 
  // Debug output
template <class GEOM3>
  std::ostream &operator<<(std::ostream &os, const Pgon<GEOM3> &pg) {

  // Output Pgon Type
  os<<"Type: ";
  if (GEOM3::pnt_size==2) os<<"CART_2D";
  else os<<"SPH_2D3D";
  os<<" ";

  // Output Pgon size
  os<<"num_pnts= "<<pg.num_pnts;

  // Next line
  os<<"\n";
  
    // Loop outputting vertices
  os << "Vertices:\n";
  Vert<GEOM3> *v=pg.beg;
  ThrowRequire(v != NULL);
  while (v != pg.end) {
    
    // Output Vert
    os<<"  ["<<*v<<"] ";
    
    // Next line
    os<<"\n";
    
    // Go to next
    v=v->next;
  }
  
  // Do End
  os<<"  ["<<*v<<"] ";
  
  // Next line
  os<<"\n";
  
  return(os);
}

#endif



#endif // ESMCI_PGON_H
