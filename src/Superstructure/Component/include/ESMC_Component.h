// $Id: ESMC_Component.h,v 1.3 2003/01/09 19:51:13 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMF Component C++ declaration include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

#ifndef ESMC_Component_H
#define ESMC_Component_H

//-----------------------------------------------------------------------------

#include "ESMC_Layout.h"

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.

enum ESMC_CompType { ESMF_APPCOMP=1, ESMF_GRIDCOMP, ESMF_CPLCOMP, 
                     ESMF_COMPTYPE_UNKNOWN };
enum ESMC_ModelType { ESMF_ATM=1, ESMF_LAND, ESMF_OCEAN, ESMF_SEAICE, 
                      ESMF_RIVER, ESMF_MODEL_UNKNOWN };

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_Component - one line general statement about this class
//
// !DESCRIPTION:
//
// The code in this file defines the C++ Component members and declares method 
// signatures (prototypes).  The companion file ESMC_Component.C contains
// the definitions (full code bodies) for the Component methods.
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
 class ESMC_ComponentConfig;
 class ESMC_Component;

// !PRIVATE TYPES:

 // class configuration type
 class ESMC_ComponentConfig {
   private:
 //   < insert resource items here >
 };

 // class declaration type
 class ESMC_Component : public ESMC_Base {    // inherits from ESMC_Base class

   private:
    char compname[ESMF_MAXSTR];
    ESMC_Layout layout;
    enum ESMC_CompType ctype;
    enum ESMC_ModelType mtype;
    char filepath[ESMF_MAXSTR];
    
 //  < insert class members here >  // real class definition

// !PUBLIC MEMBER FUNCTIONS:
//
  public:
    int ESMC_ComponentInit(void);
    int ESMC_ComponentRun(int timesteps);
    int ESMC_ComponentFinalize(void);

 // optional configuration methods
    int ESMC_ComponentGetConfig(ESMC_ComponentConfig *config) const;
    int ESMC_ComponentSetConfig(const ESMC_ComponentConfig *config);

 // accessor methods for class members
    //int ESMC_ComponentGet<Value>(<value type> *value) const;
    //int ESMC_ComponentSet<Value>(<value type>  value);
    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_ComponentValidate(const char *options) const;
    int ESMC_ComponentPrint(const char *options) const;

 // secondary construct/destruct routines
    int ESMC_ComponentConstruct(char *name, ESMC_Layout *layout,
                                      enum ESMC_CompType ctype,
                                      enum ESMC_ModelType mtype,
                                      char *filepath);
    int ESMC_ComponentDestruct(void);

 // native C++ constructors/destructors
	ESMC_Component(void);
	~ESMC_Component(void);
  
 // < declare the rest of the public interface methods here >
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_Component

// Create and Destroy are declared as class helper functions (not methods)
// since they create and destroy an ESMC_Component object itself. E.g. if Create
// were a method, the ESMC_Component object on whose behalf it was being invoked
// would need to already exist!  These functions are supersets of C++ new
// and delete; they perform allocation/deallocation specialized to
// an ESMC_Component object.

 ESMC_Component *ESMC_ComponentCreate(char *name, ESMC_Layout *layout,
                                      enum ESMC_CompType ctype,
                                      enum ESMC_ModelType mtype,
                                      char *filepath, int *rc);
 int ESMC_ComponentDestroy(ESMC_Component *comp);

 #endif  // ESMC_Component_H
