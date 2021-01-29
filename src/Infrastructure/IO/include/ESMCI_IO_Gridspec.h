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
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMCI_IO_GRIDSPEC_H
#define ESMCI_IO_GRIDSPEC_H

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::IO_Gridspec - IO_Gridspec
//
// !DESCRIPTION:
//
// The code in this file defines the C++ {\tt IO_Gridspec} members and method
// signatures (prototypes).  The companion file {\tt ESMCI\_IO_Gridspec.C}
// contains the full code (bodies) for the {\tt IO_Gridspec} methods.
//
// !USES:
#include "ESMCI_Base.h"  // inherited IO_NetCDF class

namespace ESMCI
{

// !PUBLIC TYPES:
 class IO_Gridspec;

// !PRIVATE TYPES:

// class definition type
 class IO_Gridspec : public ESMC_Base { // inherit ESMC_Base class

// !PRIVATE MEMBER FUNCTIONS:
//
  private:   // corresponds to F90 module 'type ESMF_IO_Gridspec' members
   std::string fileName;

// !PUBLIC MEMBER FUNCTIONS:

  public:
   // accessor methods

   // Retrieve rank and dimensions
   static void inq(const char *filename, int *ndims, int *grid_dims, int *rc);

   // native C++ constructors/destructors
   IO_Gridspec(void);
   ~IO_Gridspec(){destruct();}
   private:
    void destruct();

//EOPI
//-------------------------------------------------------------------------
 };  // end class IO_Gridspec
}  // namespace ESMCI
#endif  // ESMCI_IO_GRIDSPEC_H
