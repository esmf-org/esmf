// $Id: ESMC_State.h,v 1.1 2003/01/07 21:38:41 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMF State C++ declaration include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

 #ifndef ESMC_State_H
 #define ESMC_State_H

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.
 #include <ESMC_Component.h> 

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_State - one line general Statement about this class
//
// !DESCRIPTION:
//
// The code in this file defines the C++ State members and declares method 
// signatures (prototypes).  The companion file ESMC_State.C contains
// the definitions (full code bodies) for the State methods.
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
 class ESMC_StateConfig;
 class ESMC_State;

// !PRIVATE TYPES:

 // class configuration type
 class ESMC_StateConfig {
   private:
 //   < insert resource items here >
 };

 // class declaration type
 class ESMC_State : public ESMC_Base {    // inherits from ESMC_Base class

   private:
 //  < insert class members here >  corresponds to type ESMF_State members
 //                                 in F90 modules

// !PUBLIC MEMBER FUNCTIONS:
//

  public:
    int ESMC_StateInit(void);
    int ESMC_StateReady(void);
    int ESMC_StateValidate(const char *options) const;
    int ESMC_StateFinalize(void);

    int ESMC_StateReceive(void);
    int ESMC_StateSend(void);

 // optional configuration methods
    int ESMC_StateGetConfig(ESMC_StateConfig *config) const;
    int ESMC_StateSetConfig(const ESMC_StateConfig *config);

 // accessor methods for class members
    //int ESMC_StateGet<Value>(<value type> *value) const;
    //int ESMC_StateSet<Value>(<value type>  value);
    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_StatePrint(const char *options) const;

 // initialize and free secondary resources
    int ESMC_StateConstruct(void);
    int ESMC_StateDestruct(void);

 // native C++ constructors/destructors
	ESMC_State(void);
	~ESMC_State(void);
  
 // < declare the rest of the public interface methods here >
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_State

// Create and Destroy are declared as class helper functions (not methods)
// since they create and destroy an ESMC_State object itself. E.g. if Create
// were a method, the ESMC_State object on whose behalf it was being invoked
// would need to already exist!  These functions are supersets of C++ new
// and delete; they perform allocation/deallocation specialized to
// an ESMC_State object.

 ESMC_State *ESMC_StateCreate(int *rc);
 int ESMC_StateDestroy(ESMC_State *state);

 #endif  // ESMC_State_H
