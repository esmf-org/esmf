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
#define EQ_TOL 1.0E-15
  
      std::cout << "["<<*v1_pg1<<"]->["<<*v2_pg1<<"] with ["<<*v1_pg2<<"]->["<<*v2_pg2<<"] \n";
  
     // Intersect edges
      bool parallel, colinear;
      double pg1_t,pg2_t;
      GEOM::intersect_segs(v1_pg1->pnt, v2_pg1->pnt, v1_pg2->pnt, v2_pg2->pnt,
                           parallel, colinear, 
                           pg1_t,pg2_t); 

      // Classify intersection
      // TODO: think about changing to table driven
      int intertype=PGON_INTERTYPE_NONE;
      if (!parallel) { // Not parallel t's valid

        // Set pg1_t bools
        bool pg1_t_in_0_1=false;
        bool pg1_t_on_0_end=false;
        if ((pg1_t > -EQ_TOL) && (pg1_t < 1.0-EQ_TOL)) {
          if (pg1_t < EQ_TOL) {
            pg1_t_on_0_end=true; // on 0.0 end
          } else {
            pg1_t_in_0_1=true; // in middle
          }
        }

        // Set pg2_t bools
        bool pg2_t_in_0_1=false;
        bool pg2_t_on_0_end=false;
        if ((pg2_t > -EQ_TOL) && (pg2_t < 1.0-EQ_TOL)) {
          if (pg2_t < EQ_TOL) {
            pg2_t_on_0_end=true; // on 0.0 end
          } else {
            pg2_t_in_0_1=true; // in middle
          }
        }

        // Classify
        if (pg1_t_in_0_1) {
          if (pg2_t_in_0_1) {
            intertype=PGON_INTERTYPE_X;          
          } else if (pg2_t_on_0_end) {
            intertype=PGON_INTERTYPE_T_P2;
          }           
        } else if (pg1_t_on_0_end) {
          if (pg2_t_in_0_1) {
            intertype=PGON_INTERTYPE_T_P1;
          } else if (pg2_t_on_0_end) {
            intertype=PGON_INTERTYPE_V;
          }           
        }
        
      } else { // Is parallel

        // Only need to do something if colinear
        if (colinear) {
          // TODO: Fill in the last overlapping cases here
        }
      }

      
      //      printf("t1=%f t2=%f intertype=%d\n",pg1_t,pg2_t,intertype);

      std::cout << "    intertype="<<intertype<<"\n";
      
      // React to intesection classifications
      if (intertype==PGON_INTERTYPE_NONE) {
        // Don't do anything
      } else if (intertype==PGON_INTERTYPE_X) {
        // Use point from polygon 1, eventually compute average
        double new_pnt[GEOM::pnt_size];
        GEOM::calc_pnt_between(v1_pg1->pnt, v2_pg1->pnt, pg1_t, new_pnt);

        // Create new vertex and add to polygon 1
        Vert<GEOM> *vnew_pg1=pg1.add_between(v1_pg1, v2_pg1, new_pnt, pg1_t, false);
        
        // Create new vertex and add to polygon 2
        Vert<GEOM> *vnew_pg2=pg2.add_between(v1_pg2, v2_pg2, new_pnt, pg2_t, false);
        
        // Connect vertices together as an intersection
        make_intersection_Vert(vnew_pg1, vnew_pg2);
        
        std::cout << "    adding:"<<*vnew_pg1<<" to pg1\n";
        std::cout << "    adding:"<<*vnew_pg2<<" to pg2\n";
      } else if (intertype==PGON_INTERTYPE_T_P1) {        
        Vert<GEOM> *vnew_pg2=pg2.add_between(v1_pg2, v2_pg2, v1_pg1->pnt, pg2_t, false);
        make_intersection_Vert(v1_pg1, vnew_pg2);
      } else if (intertype==PGON_INTERTYPE_T_P2) {
        Vert<GEOM> *vnew_pg1=pg1.add_between(v1_pg1, v2_pg1, v1_pg1->pnt, pg1_t, false);
        make_intersection_Vert(v1_pg2, vnew_pg1);
      } else if (intertype==PGON_INTERTYPE_V) {
        make_intersection_Vert(v1_pg1, v1_pg2);
      }

#undef EQ_TOL      
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
      
      // Intersect and add new verts, etc. 
      _intersect_and_classify_edge(pg1, pg2,
                                   v1_pg1, v2_pg1,
                                   v1_pg2, v2_pg2);
                
    }
    
  }
  
}

enum PGON_RELPOSTYPE {
  PGON_RELPOSTYPE_RIGHT=0,
  PGON_RELPOSTYPE_LEFT,
  PGON_RELPOSTYPE_NBR_OF_V_NEXT,
  PGON_RELPOSTYPE_NBR_OF_V_PREV,
  PGON_RELPOSTYPE_NUM
};

// Array to map from relative pos types to labels first dim is prev, 2nd is next
PGON_INTERLABEL relpostype_to_label[PGON_RELPOSTYPE_NUM][PGON_RELPOSTYPE_NUM]= {
  {PGON_INTERLABEL_BOUNCING, PGON_INTERLABEL_CROSSING, PGON_INTERLABEL_LEFT_ON, PGON_INTERLABEL_ON_LEFT}, // prev (1st) ind = PGON_RELPOSTYPE_RIGHT
  {PGON_INTERLABEL_CROSSING, PGON_INTERLABEL_BOUNCING, PGON_INTERLABEL_RIGHT_ON, PGON_INTERLABEL_ON_RIGHT}, // prev (1st) ind = PGON_RELPOSTYPE_LEFT
  {PGON_INTERLABEL_LEFT_ON, PGON_INTERLABEL_RIGHT_ON, PGON_INTERLABEL_ERROR, PGON_INTERLABEL_ON_ON}, // prev (1st) ind = PGON_RELPOSTYPE_NBR_OF_V_NEXT
  {PGON_INTERLABEL_ON_LEFT, PGON_INTERLABEL_ON_RIGHT, PGON_INTERLABEL_ON_ON, PGON_INTERLABEL_ERROR} // prev (1st) ind = PGON_RELPOSTYPE_NBR_OF_V_PREV
};

// Get the position of w relative to the chain of vertices v_prev, v, v_next
template<class GEOM>
PGON_RELPOSTYPE _calc_relative_postion(Vert<GEOM> *w, Vert<GEOM> *v_prev, Vert<GEOM> *v, Vert<GEOM> *v_next) {

  // Is w a neighbor of v_prev
  if (v_prev->inter && (v_prev->nbr == w)) return PGON_RELPOSTYPE_NBR_OF_V_PREV;

  // Is w a neighbor of v_next
  if (v_next->inter && (v_next->nbr == w)) return PGON_RELPOSTYPE_NBR_OF_V_NEXT;      

  // Calc vectors
  double p_vec[GEOM::pnt_size]; // v to v_prev
  GEOM::sub(p_vec,v_prev->pnt,v->pnt);

  double n_vec[GEOM::pnt_size]; // v to v_next
  GEOM::sub(n_vec,v_next->pnt,v->pnt);

  double w_vec[GEOM::pnt_size]; // v to w_vec
  GEOM::sub(w_vec,w->pnt,v->pnt);

 
  // Calculate where w is relative to different segments
  bool p_to_left_of_n = (GEOM::turn(p_vec, n_vec, v->pnt) > 0.0);
  bool w_to_left_of_p = (GEOM::turn(w_vec, p_vec, v->pnt) > 0.0);
  bool w_to_left_of_n = (GEOM::turn(w_vec, n_vec, v->pnt) > 0.0);

  // Calculate where w is relative to whole chain
  if (p_to_left_of_n) { // There's a left turn in the chain
    // If w is between n and p then it's too the left
    if (w_to_left_of_n && !w_to_left_of_p) {
      return PGON_RELPOSTYPE_LEFT;
    } else {
      return PGON_RELPOSTYPE_RIGHT;
    }
  } else { // There's a right turn in the chain or it's straight
    // If w is between n and p then it's too the right
    if (!w_to_left_of_n && w_to_left_of_p) {
      return PGON_RELPOSTYPE_RIGHT;
    } else {
      return PGON_RELPOSTYPE_LEFT;
    }
  }
  
}


// Compute the intersecting points of two polygons (pg1 and pg2). Insert the intersection points into
// both polygons
template<class GEOM>
void _label_intersections(Pgon<GEOM> &pg) {

  // QUESTION: MAYBE WE NEED TO DO EACH (OR SOME) OF THESE SECTIONS TO ALL PGons BEFORE MOVING ON TO THE NEXT, so MAYBE WE
  //           NEED TO PASS BOTH IN???

  
  // TODO: MAYBE MAKE EACH OF THESE SEPERATE SUBROUTINES???
  
  // Loop through intersections labeling them according to the positions of their neighbors
  for (Vert<GEOM> *v : pg.get_VertIter(PGON_VERTITERTYPE_INTER)) {

    // Get next vert
    // TODO: Make method to get next
    Vert<GEOM> *v_next = v->next;
    Vert<GEOM> *v_prev = v->prev;
    Vert<GEOM> *v_nbr_next = v->nbr->next;
    Vert<GEOM> *v_nbr_prev = v->nbr->prev;

    // Get positions of neighboring points
    PGON_RELPOSTYPE v_nbr_prev_pos=_calc_relative_postion(v_nbr_prev, v_prev, v, v_next);
    PGON_RELPOSTYPE v_nbr_next_pos=_calc_relative_postion(v_nbr_next, v_prev, v, v_next);

    // Map relative positions to intesection label
    v->interlabel=relpostype_to_label[v_nbr_prev_pos][v_nbr_next_pos];
    
  }
  
  // Spread labels down chains
  for (Vert<GEOM> *v : pg.get_VertIter(PGON_VERTITERTYPE_INTER)) {

    // If we're starting a chain
    if ((v->interlabel == PGON_INTERLABEL_LEFT_ON) || (v->interlabel == PGON_INTERLABEL_RIGHT_ON)) {

      // Save start vertex
      Vert<GEOM> *chain_start = v;

      // Move to end of chain, setting to NONE along chain
      do {
        v->interlabel=PGON_INTERLABEL_NONE; // TODO: SHOULD WE HAVE A SPECIAL LABEL FOR THIS???
        v=v->next;
      } while (v->interlabel == PGON_INTERLABEL_ON_ON);

      // Decide label of whole chain by comparing start and end
      PGON_INTERLABEL chain_label;
      if ((chain_start->interlabel == PGON_INTERLABEL_LEFT_ON) && (v->interlabel == PGON_INTERLABEL_ON_LEFT)) {
        chain_label=PGON_INTERLABEL_DELAYED_BOUNCING;
      } else if ((chain_start->interlabel == PGON_INTERLABEL_RIGHT_ON) && (v->interlabel == PGON_INTERLABEL_ON_RIGHT)) {
          chain_label=PGON_INTERLABEL_DELAYED_BOUNCING;
        } else if ((chain_start->interlabel == PGON_INTERLABEL_LEFT_ON) && (v->interlabel == PGON_INTERLABEL_ON_RIGHT)) {
        chain_label=PGON_INTERLABEL_DELAYED_CROSSING;
      } else if ((chain_start->interlabel == PGON_INTERLABEL_RIGHT_ON) && (v->interlabel == PGON_INTERLABEL_ON_LEFT)) {
        chain_label=PGON_INTERLABEL_DELAYED_CROSSING;
      } else {
        Throw() << "Unexpected crossing situation.";
      }

      // Mark both ends with chain label
      chain_start->interlabel = chain_label;  // chain start
      v->interlabel = chain_label;            // chain end
    }      
  }

  
  // Copy labels from pg to other polygons it's intersected with
  for (Vert<GEOM> *v : pg.get_VertIter(PGON_VERTITERTYPE_INTER)) {
    v->nbr->interlabel = v->interlabel;
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

  // Label intersections of both polygons
  _label_intersections(pg1);
  _label_intersections(pg2);

  // Create result
  

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



