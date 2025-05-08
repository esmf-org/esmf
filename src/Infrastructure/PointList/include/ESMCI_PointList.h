// $Id$
//
// Earth System Modeling Framework
// Copyright (c) 2002-2025, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times


#ifndef ESMCI_PointList_H
#define ESMCI_PointList_H

#include <stdio.h>


//-----------------------------------------------------------------------------
//BOPI
// !CLASS:  ESMCI::PointList 
//
// !DESCRIPTION:
//
// Defines a structure to hold a list of points for regridding, etc....
//
// The code in this file defines the C++ {\tt PointList} class and declares
// method signatures (prototypes).  The companion file {\tt ESMCI_PointList.C}
// contains the full code (bodies) for the {\tt PointList} methods.
//
//EOPI
//-------------------------------------------------------------------------

#include "ESMCI_CoordSys.h"


namespace ESMCI {

  struct point {
    int id;
    double coords[3];

    point() {
      id=0;
      coords[0] = coords[1] = coords[2] = 0.0;
    }

    bool operator< (const point &rhs) const {
      return id < rhs.id;
    }
    bool operator!= (const point &rhs) const {
      return id != rhs.id;
    }
  };

// class definition
  class PointList {

  private:

    int max_num_pts;
    int curr_num_pts;
    int coord_dim;                  // Number of dimensions for point coordinates

    point *points;

    ESMC_CoordSys_Flag orig_coord_sys;
    int orig_coord_dim;
    point *orig_points;  // Contains original coord points. Id is repeated from above to allow sorting. In separate list to
                         // keep typical implementation as standard as possible. i


  public:

    // Construct 
    PointList(int max_size, int coord_dim, int orig_coord_dim=0,ESMC_CoordSys_Flag orig_coord_sys=ESMC_COORDSYS_UNINIT);

    // Destruct
    ~PointList();

    // Detect original coords
    bool hasOrigCoords() const {return (orig_points != NULL);}

    // Get coordinate system of original points
    ESMC_CoordSys_Flag get_orig_coord_sys() const {return orig_coord_sys;}
    
    // Add Point to List
    int add(int _id, const double *_coord);

    // Add Point to List including original coords
    int add(int _id, const double *_coord, const double *_orig_coord);

    // diagnostic print to file
    int WriteVTK(const char *filename);

    // diagnostic print
    int diagprint();

    // Get a pointer to the coordinates for a particular location
    const double *get_coord_ptr(int loc) const;

    // Get a pointer to the original coordinates for a particular location
    const double *get_orig_coord_ptr(int loc) const;

    // Get a pointer to the id for a particular location
    const int *get_id_ptr(int loc) const;

    void get_coord(int loc, double *_coord) const;

    void get_orig_coord(int loc, double *_orig_coord) const;

    // Get id for a particular location
    int get_id(int loc) const;

    point *get_point(int loc) const;

    // Get current number of points
    int get_curr_num_pts() const {
      return curr_num_pts;
    }

    // Get number of dimensions
    int get_coord_dim() const {
      return coord_dim;
    }

    // Get number of origina dimensions
    int get_orig_coord_dim() const {
      return orig_coord_dim;
    }

    // Get maximum number of points
    int get_max_num_pts() const {
      return max_num_pts;
    }

    // Sort by ID (needed for pointlists created from grids)
    int sort_by_id();

    // currently not in use - would need some testing
    PointList &operator=(PointList &rhs);

    // impelement iterators????
    // + Maybe have seperate iterators for coords and ids, but have a
    //   way to get the other from the one??
    // + OR, have one for just one (e.g. coords) and a way to get the other
    //   (e.g. id).

  };   // class PointList

} // namespace ESMCI

#endif  // ESMCI_PointList_H
