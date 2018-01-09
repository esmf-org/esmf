// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research, 
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

#ifndef ESMCI_SpaceDir_H
#define ESMCI_SpaceDir_H

#include <Mesh/include/ESMCI_Exception.h>
#include <Mesh/include/ESMCI_OTree.h>
#include <vector>
#include <set>
//-------------------------------------------------------------------------
//BOP
// !CLASS: ESMCI_SpaceDir - SpaceDir
//
// !DESCRIPTION:
//
// The code in this file defines the C++ {\tt SpaceDir} members and method
// signatures (prototypes).  The companion file {\tt ESMC\_SpaceDir.C}
// contains the full code (bodies) for the {\tt SpaceDir} methods.
// This code/algorithm developed by Bob Oehmke. This classes guesses
// which procs hold elements in a particular piece of space. If this 
// class says a proc is in a piece of space, then it may or may not be, if it
// says one isn't, then it definitely isn't. I.e. if it says that it is, you 
// have to check further to be sure, if it doesn't then you know that it doesn't.
//
///EOP
//-------------------------------------------------------------------------


// Start name space
namespace ESMCI {  


class SpaceDir;


// class definition
class SpaceDir {    // inherits from ESMC_Base class

 private:
 
  // Serial OTree holding fine scale min max boxes for objects on local proc
  OTree *otree;

  // min max box of local proc
  double proc_min[3];
  double proc_max[3];

  // List holding proc numbers
  int *proc_nums;

  // otree holding proc min max boxes
  OTree *proc_otree;

 public:

  // SpaceDir Construct 
  SpaceDir(double proc_min[3], double proc_max[3], OTree *otree, bool searchThisProc=true);

  // SpaceDir Destruct (Does not delete OTree)
  ~SpaceDir();

  // Get list of procs that might hold min-max box
  //  void get_procs(double min[3], double max[3], int *num_procs, int **procs);
  void get_procs(double min[3], double max[3], std::vector<int> *procs);

};  // end class ESMC_SpaceDir

 
} // END ESMCI namespace

#endif  // ESMCI_SpaceDir_H


