// $Id: ESMC_Route.h,v 1.3 2003/03/05 20:58:56 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMF Route C++ declaration include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

 #ifndef ESMC_Route_H
 #define ESMC_Route_H

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_Route - one line general statement about this class
//
// !DESCRIPTION:
//
// The code in this file defines the C++ Route members and declares method 
// signatures (prototypes).  The companion file ESMC_Route.C contains
// the definitions (full code bodies) for the Route methods.
//
// < insert a paragraph or two explaining what you'll find in this file >
//
//-----------------------------------------------------------------------------
// 
// !USES:
 #include <ESMC_Base.h>  // all classes inherit from the ESMC Base class.
 //#include <ESMC_XXX.h>   // other dependent classes (subclasses, aggregates,
                        // composites, associates, friends)

// !PUBLIC TYPES:
 class ESMC_RouteConfig;
 class ESMC_Route;

// !PRIVATE TYPES:

 // class configuration type
 class ESMC_RouteConfig {
   private:
 //   < insert resource items here >
 };

 // class declaration type
 class ESMC_Route : public ESMC_Base {    // inherits from ESMC_Base class

   private:
 //  < insert class members here >  corresponds to type ESMF_Route members
 //                                 in F90 modules

// !PUBLIC MEMBER FUNCTIONS:
//

  public:
 // ESMC_RouteCreate and ESMC_RouteDestroy are declared below,
 // outside the ESMC_Route declaration
    int ESMC_RouteConstruct(int arg1);      // internal only, deep class
    int ESMC_RouteDestruct(void);           // internal only, deep class

 // optional configuration methods
    int ESMC_RouteGetConfig(ESMC_RouteConfig *config) const;
    int ESMC_RouteSetConfig(const ESMC_RouteConfig *config);

 // accessor methods for class members
    //int ESMC_RouteGet(<value type> *value) const;
    //int ESMC_RouteSet(<value type>  value);
    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_RouteValidate(const char *options) const;
    int ESMC_RoutePrint(const char *options) const;

 // native C++ constructors/destructors
	ESMC_Route(void);
	~ESMC_Route(void);
  
 // < declare the rest of the public interface methods here >
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_Route

// Create and Destroy are declared as class helper functions (not methods)
// since they create and destroy an ESMC_Route object itself. E.g. if Create
// were a method, the ESMC_Route object on whose behalf it was being invoked
// would need to already exist!  These functions are supersets of C++ new
// and delete; they perform allocation/deallocation specialized to
// an ESMC_Route object.

 ESMC_Route *ESMC_RouteCreate(int arg1, int *rc);// interface only, deep class
 int ESMC_RouteDestroy(ESMC_Route *route); // interface only, deep class

 #endif  // ESMC_Route_H
