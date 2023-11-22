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
  double t;  // Parameter distance of this vertex between enclosing original vertices
  bool orig; // Part of the original Pgon
  bool inter; // An intersection vertex (not necessarily disjoint from orig) basically it means that it's connected with another polygon
  Vert *next, *prev;

  // Base constructor
  // Inits everything except points which are handled below
  Vert( ): t(-1.0), orig(false), inter(false), next(NULL), prev(NULL) {}

  Vert(double x, double y);

  Vert(double x, double y, double z);

  Vert(double *_pnt ): Vert() {
    GEOM::copy(pnt, _pnt); 
  }
    
  // Add a new vertex after this one
  void add_next(Vert *v) {
    
    // Point new vert appropriately 
    v->next=next;
    v->prev=this;

    // If there's something after, point it's prev to this
    if (next != NULL) {
      next->prev=v;
    }

    // Make the next node the new one
    // (Needs to be after the above)
    next=v;    
  }

 // Point new node appropriately 
 void add_first() {    
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

enum VertIterType {
  PGON_VERTITERTYPE_ALL,
  PGON_VERTITERTYPE_ORIG,
  PGON_VERTITERTYPE_INTER,
  PGON_VERTITERTYPE_CROSS_INTER
};


template <class GEOMVI>
class VertIter {

private:
  class iterator {
  public: 
    iterator(Vert<GEOMVI> *_beg, VertIterType _type): beg(_beg), curr(_beg), type(_type) {

      // If beg not NULL (i.e. this isn't the end), start with curr at first vertex of type
      if (beg != NULL)  find_first_Vert_of_type();
    }

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
    VertIterType type; 
    
    // Move curr to the next vertex of the correct type
    // If at end then marks at end by setting both beg and end to NULL
   void find_first_Vert_of_type() {

     // If we're at end, just leave
     if (curr == NULL) return;

     // Find the first based on type
     switch(type) {
     case PGON_VERTITERTYPE_ALL:
       // Don't do anything because for _ALL curr will always be correct 
       break;

     case PGON_VERTITERTYPE_ORIG:
       if (!curr->orig) {
         // Since curr wasn't orig, shift to next (also moves away from beg)
         curr=curr->next; 

         // Loop until we find orig (this'll be a noop if new curr is orig)
         while (!curr->orig && curr != beg) {
           curr=curr->next;
         }

         // If we've come back to beg, then we're at end
         if (curr == beg) {
           curr=NULL;
           beg=NULL;
         }
       }
       break;
       
     case PGON_VERTITERTYPE_INTER:
       if (!curr->inter) {
         // Since curr wasn't inter, shift to next (also moves away from beg)
         curr=curr->next; 

         // Loop until we find inter (this'll be a noop if new curr is inter)
         while (!curr->inter && curr != beg) {
           curr=curr->next;
         }

         // If we've come back to beg, then we're at end
         if (curr == beg) {
           curr=NULL;
           beg=NULL;
         }
       }
       break;
       
     case PGON_VERTITERTYPE_CROSS_INTER:
       if (!curr->inter) {
         // Since curr wasn't inter, shift to next (also moves away from beg)
         curr=curr->next; 

         // Loop until we find inter (this'll be a noop if new curr is inter)
         while (!curr->inter && curr != beg) {
           curr=curr->next;
         }

         // If we've come back to beg, then we're at end
         if (curr == beg) {
           curr=NULL;
           beg=NULL;
         }
       }

       break;
       
     }
   }
    

    // Go to next Vert in the list
   Vert<GEOMVI> *next_Vert() {

     // If we're at end, just leave
     if (curr == NULL) return NULL;

     // Go to next
     curr=curr->next;

     // Move along until we're at the next of the correct type
     switch(type) {
     case PGON_VERTITERTYPE_ALL:
       // Don't do anything because for _ALL curr will always be correct 
       break;

     case PGON_VERTITERTYPE_ORIG:
       while (!curr->orig && curr != beg) {
         curr=curr->next;
       }
       break;
       
     case PGON_VERTITERTYPE_INTER:
       while (!curr->inter && curr != beg) {
         curr=curr->next;
       }
       break;
       
     case PGON_VERTITERTYPE_CROSS_INTER:
       // TODO: Still need to add the cross part
       while (!curr->inter && curr != beg) {
         curr=curr->next;
       }
       break;
     }


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
  VertIter() : beg(NULL), type(PGON_VERTITERTYPE_ALL) {};

  iterator begin() { return iterator(beg,type); }
  iterator end() { return iterator(NULL,type); }

  Vert<GEOMVI> *beg;
  VertIterType type;
};




template <class GEOM2>
class Pgon {

  public:

  // Buffer for holding Coords when they need to be passed someplace    
  std::vector<double> coord_buff;
  // TODO: add a flag that tracks whether new points have been added since last pack, so
  //       we only pack when necessary       

  
  int num_pnts; // Size of polygon  
  Vert<GEOM2> *beg,*end; // List of vertices
  
protected:
  VertIter<GEOM2> vertIter;
  

  // private methods 
private: 

  // All new vertex addition should eventually go through the following two methods, because
  // they will do the complete set of processing necessary to do that.
  
  // Add the first vertex to the polygon 
  void add_new_first(Vert<GEOM2> *v_new) {
    
    // Make this the first vertex
    v_new->add_first();
    
    // Increase the number of points
    num_pnts++;
  }


  // Add a new vertex to the polygon after v
  void add_new_next(Vert<GEOM2> *v, Vert<GEOM2> *v_new) {
    
    // Add if after this vert
    v->add_next(v_new);
    
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
    for (Vert<GEOM2> *v : get_VertIter(PGON_VERTITERTYPE_ALL)) {

      // Pack coords
      coord_buff.push_back(v->pnt[0]);
      coord_buff.push_back(v->pnt[1]);
      if (GEOM2::pnt_size >2) coord_buff.push_back(v->pnt[2]);
    }
  }

  
public:

  
  // Full constructor
  Pgon(): beg(NULL), end(NULL), num_pnts(0) {

    // Error check number of coords
    if ((GEOM2::pnt_size != 2) && (GEOM2::pnt_size != 3)) Throw() << "Pgon only supports 2D or 3D points.";
  }


  // Loop over vertices
  // TODO: Make this so it makes an independant iterator object? So you can have more than one per polygon
  VertIter<GEOM2> &get_VertIter(VertIterType type, Vert<GEOM2> *first=NULL) {
    
    // Set beginning
    if (first == NULL) {
      vertIter.beg=beg;
    } else {
      vertIter.beg=first;
    }

    // Set type
    vertIter.type=type;

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


  // TODO: THINK ABOUT THE WHOLE ORIG/INTER VERT THING AND
  //       SEE IF YOU WANT TO KEEP SEPERATE METHODS FOR CREATING OR
  //       HAVE A FLAG OR SOMETHING. ALSO MAYE INSTEAD OF LINK()
  //       CALL IT MAKE INTER() AND AT THAT POINT SET TO INTER=TRUE
  //       MAYBE HAVE A FLAG FOR THE OTHER METHODS TO JUST BE ORIG TRUE/FALSE??
  
  void push_back(Vert<GEOM2> *v) {

    // If nothing added yet, make the first
    if (end == NULL) {
      add_new_first(v);
      beg=end=v;
    } else { // Otherwise, add it after the end and make it the new end
      add_new_next(end, v);
      end=v;
    }
  }

  
  // Add a vert to the end of the polygon
  // Assumes is original, unless specified otherwise
  Vert<GEOM2> *push_back(double *pnt, bool orig=true) {

    // create vert
    Vert<GEOM2> *vert=new Vert<GEOM2>(pnt);

    // Set orig status
    vert->orig=orig;
    
    // Add to end
    push_back(vert);

    // Return new vert
    return vert;
  }


  // Add a vert to the end of the polygon
  Vert<GEOM2> *push_back(double x, double y, bool orig=true) {

    // create vert
    Vert<GEOM2> *vert=new Vert<GEOM2>(x,y);

    // Mark as origin
    vert->orig=orig;
    
    // Add to end
    push_back(vert);

    // Return new vert
    return vert;
  }

  // Add an original vert to the end of the polygon
  Vert<GEOM2> *push_back(double x, double y, double z, bool orig=true) {

    // create vert
    Vert<GEOM2> *vert=new Vert<GEOM2>(x,y,z);

    // Mark as original
    vert->orig=orig;
    
    // Add to end
    push_back(vert);

    // Return new vert
    return vert;
  }


  // Insert vert between two vert, order by t if more than 1
  // Doesn't look at t's of v0 or v1
  Vert<GEOM2> *add_between(Vert<GEOM2> *v0, Vert<GEOM2> *v1, double *pnt, double t, bool orig=true) {

    // Create vert
    Vert<GEOM2> *vert=new Vert<GEOM2>(pnt);

    // Set original
    vert->orig=orig;

    // Set t
    vert->t=t;

    // Loop finding vert that's right before place to add
    Vert<GEOM2> *just_before=v0;
    while ((just_before->next != v1) && (just_before->next->t < t)) {
      just_before=just_before->next;
    } 

    // Add new vert next after just_before
    add_new_next(just_before, vert);
    
    // Return new vert
    return vert;
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
  for (Vert<GEOM3> *v : pg.get_VertIter(PGON_VERTITERTYPE_ALL)) {

    // Output Vert
    os<<"  ["<<*v<<"] ";
    
    // Next line
    os<<"\n";
  }
    
  return(os);
}



#endif // ESMCI_PGON_H
