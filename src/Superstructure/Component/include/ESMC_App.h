// $Id: ESMC_App.h,v 1.3 2003/03/11 03:01:07 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMF App C++ declaration include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

#ifndef ESMC_App_H
#define ESMC_App_H

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.
#include <ESMC_Comp.h> 

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_App - one line general statement about this class
//
// !DESCRIPTION:
//
// The code in this file defines the C++ App members and declares method 
// signatures (prototypes).  The companion file ESMC_App.C contains
// the definitions (full code bodies) for the App methods.
//
// 
//
//-----------------------------------------------------------------------------
// 
// !USES:
 #include <ESMC_Base.h>  // all classes inherit from the ESMC Base class.
 //#include <ESMC_XXX.h>   // other dependent classes (subclasses, aggregates,
                        // composites, associates, friends)

// !PUBLIC TYPES:
 class ESMC_AppConfig;
 class ESMC_App;

// !PRIVATE TYPES:

 // class configuration type
 class ESMC_AppConfig {
   private:
 //   < insert resource items here >
 };

 // class declaration type
 class ESMC_App : public ESMC_Base {    // inherits from ESMC_Base class

   private:
 //  < insert class members here >  // real class definition

// !PUBLIC MEMBER FUNCTIONS:
//
  public:
    int ESMC_AppInit(void);
    int ESMC_AppRun(void);
    int ESMC_AppFinalize(void);

 // optional configuration methods
    int ESMC_AppGetConfig(ESMC_AppConfig *config) const;
    int ESMC_AppSetConfig(const ESMC_AppConfig *config);

 // accessor methods for class members
    //int ESMC_AppGet<Value>(<value type> *value) const;
    //int ESMC_AppSet<Value>(<value type>  value);
    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_AppValidate(const char *options) const;
    int ESMC_AppPrint(const char *options) const;

 // construct/destructors
    int ESMC_AppConstruct(void);
    int ESMC_AppDestruct(void);

 // native C++ constructors/destructors
	ESMC_App(void);
	~ESMC_App(void);
  
 // < declare the rest of the public interface methods here >
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_App

// Create and Destroy are declared as class helper functions (not methods)
// since they create and destroy an ESMC_App object itself. E.g. if Create
// were a method, the ESMC_App object on whose behalf it was being invoked
// would need to already exist!  These functions are supersets of C++ new
// and delete; they perform allocation/deallocation specialized to
// an ESMC_App object.

 ESMC_App *ESMC_AppCreate(int *rc);
 int ESMC_AppDestroy(ESMC_App *app);

 #endif  // ESMC_App_H
