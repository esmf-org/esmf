// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research, 
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

#ifndef ESMCI_OTree_H
#define ESMCI_OTree_H

// FOR ESMF
#include <Mesh/include/Legacy/ESMCI_Exception.h>

// OUTSIDE ESMF
//#include "ESMCI_Exception.h"

//-------------------------------------------------------------------------
//BOP
// !CLASS: ESMCI_OTree - OTree
//
// !DESCRIPTION:
//
// The code in this file defines the C++ {\tt OTree} members and method
// signatures (prototypes).  The companion file {\tt ESMC\_OTree.C}
// contains the full code (bodies) for the {\tt OTree} methods.
// This code/algorithm developed by Bob Oehmke. Note to self:
// put full ref here when paper is done making its way through 
// publication process. 
//
///EOP
//-------------------------------------------------------------------------


// Start name space
namespace ESMCI {  

class OTree;

class ONode;

  // Nodes which make up tree
  class ONode {
  public:

    double min[3],max[3];

    unsigned int itype;

    ONode *children;

    ONode *next;

    void *data;
  };


// class definition
class OTree {    // inherits from ESMC_Base class

 private:
 
  // top of tree
  ONode *root;

  // Holder for Nodes which make up tree
  // Not using vector, because DON'T want it to reallocate
  ONode *mem;
  int max_size_mem;
  int curr_size_mem;

  // committed
  bool is_committed;

 public:

  // OTree Construct 
  OTree(int max_size);

  // OTree Destruct
  ~OTree();

 // Add item to tree
 void add(double min[3], double max[3], void *data);

 // Add item to tree and commit it at the same time
 void add_commit(double min[3], double max[3], void *data);

 // Build tree
 void commit();

 int runon(double [], double [], int (*func)(void *,void *),void *);

 int runon_mm_chng(double [], double [],
        int (*func)(void *, void *, double *, double *),void *);
   


};  // end class ESMC_OTree

 
} // END ESMCI namespace

#endif  // ESMCI_OTREE_H


