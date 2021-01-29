// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research, 
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

  public:
    // Construct
    PointList(int max_size, int coord_dim);

    // Destruct
    ~PointList();

    // Add Point to List
    int add(int _id, const double *_coord);

    // diagnostic print to file
    int WriteVTK(const char *filename);

    // diagnostic print
    int diagprint();

    // Get a pointer to the coordinates for a particular location
    const double *get_coord_ptr(int loc) const;

    // Get a pointer to the id for a particular location
    const int *get_id_ptr(int loc) const;

    void get_coord(int loc, double *_coord) const;

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
