// $Id: ESMCI_RHandle.h,v 1.3.2.1 2010/02/05 19:59:57 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
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

#ifndef ESMCI_RHandle_H
#define ESMCI_RHandle_H

//-----------------------------------------------------------------------------
//BOPI
// !CLASS:  ESMCI::RouteHandle - Handle which points to precomputed information
//              
//
// !DESCRIPTION:
//
// The code in this file defines the C++ {\tt RouteHandle} class and declares
// method signatures (prototypes).  The companion file {\tt ESMCI_RHandle.C}
// contains the full code (bodies) for the {\tt RouteHandle} methods.
//
//EOPI
//-------------------------------------------------------------------------

#include "ESMC_Base.h"      // Base is superclass to RouteHandle

//-------------------------------------------------------------------------

namespace ESMCI {

  // classes and structs

  typedef enum { 
    ESMC_UNINITIALIZEDHANDLE=1,
    ESMC_ARRAYXXE,
    ESMC_ARRAYBUNDLEXXE
  }RouteHandleType;

  // class definition
  class RouteHandle : public ESMC_Base {    // inherits from ESMC_Base class

   private:
    RouteHandleType htype;          // type info
    void *storage;                  // storage used by specific communication
 
   public:
    RouteHandle():ESMC_Base(-1){}   // use Base constructor w/o BaseID increment
    static RouteHandle *create(int *rc);
    static int destroy(RouteHandle *routehandle);
    int construct(void);
    int destruct(void);    
    RouteHandleType getType(void) const { return htype; }
    int setType(RouteHandleType h){ htype = h; return ESMF_SUCCESS; }
    void *getStorage(void) const{ return storage; }
    int setStorage(void *ptr){ storage = ptr; return ESMF_SUCCESS; }
        
    // required methods inherited and overridden from the ESMC_Base class
    int validate(const char *options) const;
    int print(const char *options) const;

  };   // class RouteHandle

} // namespace ESMCI

#endif  // ESMCI_RHandle_H
