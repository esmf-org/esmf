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

#ifndef ESMCI_IO_SCRIP_H
#define ESMCI_IO_SCRIP_H

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::IO_Scrip - IO_Scrip
//
// !DESCRIPTION:
//
// The code in this file defines the C++ {\tt IO_Scrip} members and method
// signatures (prototypes).  The companion file {\tt ESMCI\_IO_Scrip.C}
// contains the full code (bodies) for the {\tt IO_Scrip} methods.
//
// !USES:
#include "ESMCI_Base.h"  // inherited IO_NetCDF class

namespace ESMCI
{

// !PUBLIC TYPES:
 class IO_Scrip;

// !PRIVATE TYPES:

// class definition type
 class IO_Scrip : public ESMC_Base { // inherit ESMC_Base class

// !PRIVATE MEMBER FUNCTIONS:
//
  private:   // corresponds to F90 module 'type ESMF_IO_Scrip' members
   std::string fileName;

// !PUBLIC MEMBER FUNCTIONS:

  public:
   // accessor methods

   // Retrieve rank and dimensions
   static void inq(const char *filename, int *grid_dims, int *grid_rank, 
                   int *rc);

   // native C++ constructors/destructors
   IO_Scrip(void);
   ~IO_Scrip(){destruct();}
   private:
    void destruct();

//EOPI
//-------------------------------------------------------------------------
 };  // end class IO_Scrip
}  // namespace ESMCI
#endif  // ESMCI_IO_SCRIP_H
