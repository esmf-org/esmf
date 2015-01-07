// $Id: ESMCI_PntList.h,v 1.9 2012/01/06 20:17:47 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2012, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMCI Grid include file for C++

// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMCI_PntList_H
#define ESMCI_PntList_H

//-------------------------------------------------------------------------
//BOP
// !CLASS: PntList
//
// !DESCRIPTION:
//
// Defines a structure to hold a list of points for regridding, etc....
//
///EOP
//-------------------------------------------------------------------------


// Start name space
namespace ESMCI {  

// class definition
class PntList {  

 private: 
  int max_num_pnts;
  int curr_num_pnts;
  int coord_dim;  // Number of dimensions for point coordinates
  
  // TODO: think about using vectors for the below???

  double *coords; // of size coord_dim*num_pnts
  
  // Make the below UInt??
  int *ids; // of size num_pnts

  // Map from id to location, put it in if you need it
  // std::map<int,int> id_to_loc;
     
 public:

  // Construct 
  PntList(int max_size, int coord_dim);

  // OTree Destruct
  ~PntList();

  // Add Point to List
  void add(int _id, double *_coord);

  // Get the pointer to the coordinates
  // for a particular location
  const double *get_coord_ptr(int loc) {
    if (loc >= curr_num_pnts) {
      //      Throw() << "Accessing point off end of PntList";
    }
    return coords+coord_dim*loc;
  }

  // Put coordinates into an array
  // for a particular location
  // The input variable: _coord needs
  //  to be of size coord_dim
  // TODO: ADD ERROR CHECKING FOR loc???
  void get_coord(int loc, double *_coord) {
    if (loc >= curr_num_pnts) {
      //  Throw() << "Accessing point off end of PntList";
    }
    double *pnt_coord_base=coords+coord_dim*loc;
    for (int i=0; i<coord_dim; i++) {
      _coord[i]=pnt_coord_base[i];
    }
  }

  // Get id for a particular location
  // TODO: ADD ERROR CHECKING FOR loc???
  int get_id(int loc) {
    if (loc >= curr_num_pnts) {
      // Throw() << "Accessing point off end of PntList";
    }
    return ids[loc];
  }

  // Get current number of points
  int get_curr_num_pnts() {
    return curr_num_pnts;
  }

  // Get current number of points
  int get_coord_dim() {
    return coord_dim;
  }

  // Get current number of points
  int get_max_num_pnts() {
    return max_num_pnts;
  }

 
  // impelement iterators????
  // + Maybe have seperate iterators for coords and ids, but have a 
  //   way to get the other from the one??
  // + OR, have one for just one (e.g. coords) and a way to get the other 
  //   (e.g. id). 
   
   
};  // end class PntList

 
} // END ESMCI namespace

#endif  // ESMCI_PNTLIST_H


