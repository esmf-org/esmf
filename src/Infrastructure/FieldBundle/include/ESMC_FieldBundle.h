// $Id: ESMC_FieldBundle.h,v 1.1.2.4 2009/01/21 21:25:20 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMF FieldBundle C++ declaration include file
//
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

#ifndef ESMC_FieldBundle_H
#define ESMC_FieldBundle_H

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_FieldBundle - C++ interface to the F90 FieldBundle object
//
// !DESCRIPTION:
//
// The code in this file defines the C++ FieldBundle members and declares method 
// signatures (prototypes).  The companion file ESMC\_FieldBundle.C contains
// the definitions (full code bodies) for the FieldBundle methods.
//
//-----------------------------------------------------------------------------
// 
// !USES:
#include "ESMC_Start.h"
#include "ESMC_Base.h"  // all classes inherit from the ESMC Base class.

// !PUBLIC TYPES:
 class ESMC_FieldBundle;

// !PRIVATE TYPES:

 // fortran interface declarations
extern "C" {
   void FTN(f_esmf_bundlecreate)(ESMC_FieldBundle *, int *rc);
   void FTN(f_esmf_bundledestroy)(ESMC_FieldBundle *, int *rc);

   void FTN(c_esmc_bundleserialize)(ESMC_Status *bundlestatus,
              ESMC_Status *igridstatus, int *field_count, int *pack_flag,
              void *mapping, ESMC_Status *iostatus,
              void *buffer, int *length, int *offset, int *localrc);
   void FTN(c_esmc_bundledeserialize)(ESMC_Status *bundlestatus,
              ESMC_Status *igridstatus, int *field_count, int *pack_flag,
              void *mapping, ESMC_Status *iostatus,
              void *buffer, int *offset, int *localrc);

};

 // class declaration type
 // TODO: decide if this does or does not inherit from the ESMC_Base class.
 //  NO: not needed because this is simply an interface to the F90 code      
 //  YES: makes inheritance on the C++ side possible before crossing into F90
 class ESMC_FieldBundle {                     
 //class ESMC_FieldBundle : public ESMC_Base {

   private:
     void *handle;    // pointer to Fortran ESMF_FieldBundleType

// !PUBLIC MEMBER FUNCTIONS:
//

   public:
     int ESMC_FieldBundleConstruct(void);        
     int ESMC_FieldBundleDestruct(void);        

    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_FieldBundleValidate(const char *options) const;
    int ESMC_FieldBundlePrint(const char *options) const;

 // native C++ constructors/destructors
	ESMC_FieldBundle(void) {  handle = 0; }
	~ESMC_FieldBundle(void) { }
  
 // < declare the rest of the public interface methods here >
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_FieldBundle


ESMC_FieldBundle *ESMC_FieldBundleCreate(int *rc);
int ESMC_FieldBundleDestroy(ESMC_FieldBundle *f);

#endif  // ESMC_FieldBundle_H








