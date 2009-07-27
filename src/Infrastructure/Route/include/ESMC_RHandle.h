// $Id: ESMC_RHandle.h,v 1.17 2009/07/27 23:23:34 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMF RHandle C++ declaration include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

 #ifndef ESMC_RHandle_H
 #define ESMC_RHandle_H

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_RouteHandle - Handle which points to precomputed information
//              
//
// !DESCRIPTION:
//
// The code in this file defines the C++ RouteHandle class and declares method 
// signatures (prototypes).  The companion file ESMC_RHandle.C contains
// the definitions (full code bodies) for the RouteHandle methods.
//
// 
//
//-----------------------------------------------------------------------------
// 
// !USES:
 #include <ESMC_Base.h>  // all classes inherit from the ESMC Base class.
 #include <ESMCI_LocalArray.h>

// !PUBLIC TYPES:
 class ESMC_RouteHandle;

// does this handle describe the memory movements needed to execute
// a halo, redistribution, or regrid operation?  set by the corresponding
// store operation, and available to be error checked at run time.
typedef enum { 
    ESMC_UNINITIALIZEDHANDLE=1,
    ESMC_ARRAYXXE,
    ESMC_ARRAYBUNDLEXXE
} ESMC_HandleType;

// !PRIVATE TYPES:

 // class declaration type
 class ESMC_RouteHandle : public ESMC_Base {    // inherits from ESMC_Base class

   private:
     ESMC_HandleType htype;         // halo, redist, or regrid
     void *storage;                 // storage used by specific communication
 
// !PUBLIC MEMBER FUNCTIONS:
//

  public:
    int ESMC_RouteHandleConstruct(void);
    int ESMC_RouteHandleDestruct(void);    


 // accessor methods for individual class members
    ESMC_HandleType ESMC_RouteHandleGetType(void) const { return htype; }
    int ESMC_RouteHandleSetType(ESMC_HandleType h) { 
         htype = h; return ESMF_SUCCESS; }
    void *ESMC_RouteHandleGetStorage(void) const{ return storage; }
    int ESMC_RouteHandleSetStorage(void *ptr){
      storage = ptr;
      return ESMF_SUCCESS;
    }
        
        
        
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_RouteHandleValidate(const char *options) const;
    int ESMC_RouteHandlePrint(const char *options) const;

 // native C++ constructors/destructors
	ESMC_RouteHandle(void);
	~ESMC_RouteHandle(void);
  
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 

//
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_RouteHandle

// Create and Destroy are declared as class helper functions (not methods)
// since they create and destroy an ESMC_RHandle object itself. E.g. if Create
// were a method, the ESMC_RHandle object on whose behalf it was being invoked
// would need to already exist!  These functions are supersets of C++ new
// and delete; they perform allocation/deallocation specialized to
// an ESMC_RouteHandle object.

 ESMC_RouteHandle *ESMC_RouteHandleCreate(int *rc);
 int ESMC_RouteHandleDestroy(ESMC_RouteHandle *rtable);


 #endif  // ESMC_RHandle_H
