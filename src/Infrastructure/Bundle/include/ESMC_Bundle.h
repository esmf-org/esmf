// $Id: ESMC_Bundle.h,v 1.4.6.3 2007/10/18 02:42:21 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMF Bundle C++ declaration include file
//
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

#ifndef ESMC_Bundle_H
#define ESMC_Bundle_H

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_Bundle - C++ interface to the F90 Bundle object
//
// !DESCRIPTION:
//
// The code in this file defines the C++ Bundle members and declares method 
// signatures (prototypes).  The companion file ESMC\_Bundle.C contains
// the definitions (full code bodies) for the Bundle methods.
//
//-----------------------------------------------------------------------------
// 
// !USES:
#include "ESMC_Start.h"
#include "ESMC_Base.h"  // all classes inherit from the ESMC Base class.

// !PUBLIC TYPES:
 class ESMC_Bundle;

// !PRIVATE TYPES:

 // fortran interface declarations
extern "C" {
   void FTN(f_esmf_bundlecreate)(ESMC_Bundle *, int *rc);
   void FTN(f_esmf_bundledestroy)(ESMC_Bundle *, int *rc);

   void FTN(c_esmc_bundleserialize)(ESMC_Status *bundlestatus,
              ESMC_Status *gridstatus, int *field_count, int *pack_flag,
              void *mapping, ESMC_Status *iostatus,
              void *buffer, int *length, int *offset, int *localrc);
   void FTN(c_esmc_bundledeserialize)(ESMC_Status *bundlestatus,
              ESMC_Status *gridstatus, int *field_count, int *pack_flag,
              void *mapping, ESMC_Status *iostatus,
              void *buffer, int *offset, int *localrc);

};

 // class declaration type
 // TODO: decide if this does or does not inherit from the ESMC_Base class.
 //  NO: not needed because this is simply an interface to the F90 code      
 //  YES: makes inheritance on the C++ side possible before crossing into F90
 class ESMC_Bundle {                     
 //class ESMC_Bundle : public ESMC_Base {

   private:
     void *handle;    // pointer to Fortran ESMF_BundleType

// !PUBLIC MEMBER FUNCTIONS:
//

   public:
     int ESMC_BundleConstruct(void);        
     int ESMC_BundleDestruct(void);        

    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_BundleValidate(const char *options) const;
    int ESMC_BundlePrint(const char *options) const;

 // native C++ constructors/destructors
	ESMC_Bundle(void) {  handle = 0; }
	~ESMC_Bundle(void) { }
  
 // < declare the rest of the public interface methods here >
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_Bundle


ESMC_Bundle *ESMC_BundleCreate(int *rc);
int ESMC_BundleDestroy(ESMC_Bundle *f);

#endif  // ESMC_Bundle_H








