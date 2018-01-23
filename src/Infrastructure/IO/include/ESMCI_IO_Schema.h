// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research, 
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

#ifndef __ESMCI_IO_SCHEMA_H
#define __ESMCI_IO_SCHEMA_H

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::IO_Schema - IO
//
// !DESCRIPTION:
//
// The code in this file defines the C++ {\tt IO_Schema} members and method
// signatures (prototypes) along with support classes.  The companion file
// {\tt ESMCI\_IO\_Schema.C} contains the full code (bodies) for the
// {\tt IO_Schema} methods.
//
//EOPI
//-------------------------------------------------------------------------

#include "ESMCI_VM.h"
#include "ESMC_Util.h"

#include <cstdio>
#include <vector>

//-------------------------------------------------------------------------

namespace ESMCI {

  // Helper class to hold Schema information
  class IOSchemaBase;

  class IO_Schema {
  private:

    std::vector<IOSchemaBase *> schemaObjects; // The list of schema objects
    char *filename;                            // File source for this schema

  public:

    IO_Schema() {                              // Create a bare Schema
      filename = (char *)NULL;
    }

    IO_Schema(const char *const file,
              int *rc);                        // Create a Schema from a file

    ~IO_Schema();

  private:

    int parseFile(const char *const filename); // Parse file into schema

  };
  //===========================================================================

} // namespace ESMCI

#endif // __ESMCI_IO_SCHEMA_H
