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

// Intersecction types
#define PGON_INTERTYPE_UNDEF -1
#define PGON_INTERTYPE_NONE 0
#define PGON_INTERTYPE_X 1
#define PGON_INTERTYPE_T_P1 2
#define PGON_INTERTYPE_T_P2 3
#define PGON_INTERTYPE_V 4

// Generate and classify intersection
// TODO: Eventually add general method here that does whatever for intersecting and adding new verts, so
// that it can be called from both loops and OTree code. Maybe take in verts defining points, so they can
// be added to.
template<class GEOM>
void _intersect_and_classify_edge(Pgon<GEOM> &pg1, Pgon<GEOM> &pg2,
                                  Vert<GEOM> *v1_pg1, Vert<GEOM> *v2_pg1,
                                  Vert<GEOM> *v1_pg2, Vert<GEOM> *v2_pg2) {

      std::cout << "["<<*v1_pg1<<"]->["<<*v2_pg1<<"] with ["<<*v1_pg2<<"]->["<<*v2_pg2<<"] \n";
  
     // Intersect edges
      double pg1_t,pg2_t;
      GEOM::intersect_segs(v1_pg1->pnt,v2_pg1->pnt,v1_pg2->pnt,v2_pg2->pnt,&pg1_t,&pg2_t); 

      // Classify intersection
      // TODO: think about changing to table driven
      int intertype=PGON_INTERTYPE_UNDEF;        
      if (pg1_t < 0.0) {
        intertype=PGON_INTERTYPE_NONE;        
      } else if (pg1_t == 0.0) {
        intertype=PGON_INTERTYPE_T_P1;
      } else if (pg1_t < 1.0) {
        if (pg2_t < 0.0) {
 
        } else if (pg2_t == 0.0) {
          intertype=PGON_INTERTYPE_T_P2;         
        } else if (pg2_t < 1.0) {
          intertype=PGON_INTERTYPE_X; 
        } else if (pg1_t == 1.0) {
          
        } else { // pg1_t > 1.0
          intertype=PGON_INTERTYPE_NONE;        
        }
      } else if (pg1_t == 1.0) {
        
      } else { // pg1_t > 1.0
        intertype=PGON_INTERTYPE_NONE;        
      }

      //      printf("t1=%f t2=%f intertype=%d\n",pg1_t,pg2_t,intertype);

      std::cout << "    intertype="<<intertype<<"\n";
      
      // React to intesection classifications
      switch(intertype) {
      
      case PGON_INTERTYPE_X:

        // Calc new point
        double new_pnt[GEOM::pnt_size];

        
        // Create new vertex and add to polygon 1
        GEOM::calc_pnt_between(v1_pg1->pnt, v2_pg1->pnt, pg1_t, new_pnt);
        Vert<GEOM> *vnew_pg1=pg1.add_inter_vert_between(v1_pg1, v2_pg1, new_pnt, pg1_t);

        // Create new vertex and add to polygon 2
        GEOM::calc_pnt_between(v1_pg2->pnt, v2_pg2->pnt, pg2_t, new_pnt);
        Vert<GEOM> *vnew_pg2=pg2.add_inter_vert_between(v1_pg2, v2_pg2, new_pnt, pg2_t);
        
        // Connect vertices together

        std::cout << "    adding:"<<*vnew_pg1<<" to pg1\n";
        std::cout << "    adding:"<<*vnew_pg2<<" to pg2\n";
        
        break;
#if 0

        
      case PGON_INTERTYPE_T_P1:
        
        // Create new vertex and add to polygon 2
        Vert<GEOM> *vnew_pg2=new Vert<GEOM>(v1_pg1->pnt);
        v1_pg2->add_next(vnew_pg2);
        
      case PGON_INTERTYPE_T_P2:
        
        // Create new vertex and add to polygon 1
        Vert<GEOM> *vnew_pg1=new Vert<GEOM>(v1_pg2->pnt);
        v1_pg1->add_next(vnew_pg1);
        

        // Connect vertices together
        
        break;
#endif
        
        //      case PGON_INTERTYPE_UNDEF:
        //Throw() << " Trying to use undefined polygon intersection type.";        
        //break;
        
      } 

      
}


// Compute the intersecting points of two polygons (pg1 and pg2). Insert the intersection points into
// both polygons
template<class GEOM>
void _intersect_pgons(Pgon<GEOM> &pg1, Pgon<GEOM> &pg2) {

  // Loop through vertices in both polygons adding intersections
  for (Vert<GEOM> *v1_pg1 : pg1.get_VertIter(PGON_VERTITERTYPE_ORIG)) {

    // Get next vert
    // TODO: Make method to get next
    Vert<GEOM> *v2_pg1 = v1_pg1->next;

    // Debug output
    //std::cout << "["<<*v1_pg1<<"]->["<<*v2_pg1<<"] \n";

    for (Vert<GEOM> *v1_pg2 : pg2.get_VertIter(PGON_VERTITERTYPE_ORIG)) {
      
      // Get next vert
      // TODO: Make method to get next
      Vert<GEOM> *v2_pg2 = v1_pg2->next;
      
      // Debug output
      _intersect_and_classify_edge(pg1, pg2,
                                   v1_pg1, v2_pg1,
                                   v1_pg2, v2_pg2);
                
    }
    
  }
  
}


// Intersect polygon pg1 with polygon pg2 to yield the result polygon pg_result
/// Algorithm based on the modified Greiner-Hormann found in "Clipping simple polygons with degenerate intersections"
/// by Erich L. Foster, Kai Hormann, and Romeo Traian Popa
template<class GEOM>
static void Pgon<GEOM>::intersection(Pgon<GEOM> &pg1, Pgon<GEOM> &pg2, Pgon<GEOM> &pg_result) {

  std::cout << "Pgon::intersection(): Beg\n";
  
  // Compute intersections and add to polygons
  _intersect_pgons(pg1, pg2); 

  std::cout << "Pgon::intersection(): End\n";
  
}

// Instaniate specific versions
template void Pgon<GEOM_CART2D>::intersection(Pgon<GEOM_CART2D> &pg1, Pgon<GEOM_CART2D> &pg2,
                                                     Pgon<GEOM_CART2D> &pg_result);


// Method for subtracting polygon clipper from polygon subject to yield the results
// list of new polygons
template<class GEOM>
static void Pgon<GEOM>::difference(Pgon<GEOM> &subject, Pgon<GEOM> &clipper, Pgon<GEOM> &result) {

  
}



