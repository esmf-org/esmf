// $Id: ESMC_GComp.h,v 1.6 2004/04/20 19:03:26 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMF GComp C++ declaration include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

#ifndef ESMC_GComp_H
#define ESMC_GComp_H

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_GComp - one line general statement about this class
//
// !DESCRIPTION:
//
// The code in this file defines the C++ GComp members and declares method 
// signatures (prototypes).  The companion file ESMC\_GComp.C contains
// the definitions (full code bodies) for the GComp methods.
//
// 
//
//-----------------------------------------------------------------------------
// 
// !USES:
 #include <ESMC_Base.h>  // all classes inherit from the ESMC Base class.
 #include <ESMC_Comp.h> 

// !PUBLIC TYPES:
 class ESMC_GComp;

// !PRIVATE TYPES:


 // class declaration type
 class ESMC_GComp : public ESMC_Base {    // inherits from ESMC_Base class

   private:
 //  < insert class members here >  // real class definition

// !PUBLIC MEMBER FUNCTIONS:
//
  public:
    int ESMC_GCompInit(void);
    int ESMC_GCompRun(void);
    int ESMC_GCompFinalize(void);

 // accessor methods for class members
    //int ESMC_GCompGet<Value>(<value type> *value) const;
    //int ESMC_GCompSet<Value>(<value type>  value);
    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_GCompValidate(const char *options) const;
    int ESMC_GCompPrint(const char *options) const;

 // secondary construct/destructor routines
    int ESMC_GCompConstruct(void);
    int ESMC_GCompDestruct(void);

 // native C++ constructors/destructors
	ESMC_GComp(void);
	~ESMC_GComp(void);
  
 // < declare the rest of the public interface methods here >
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_GComp

// Create and Destroy are declared as class helper functions (not methods)
// since they create and destroy an ESMC_GComp object itself. E.g. if Create
// were a method, the ESMC_GComp object on whose behalf it was being invoked
// would need to already exist!  These functions are supersets of C++ new
// and delete; they perform allocation/deallocation specialized to
// an ESMC_GComp object.

 ESMC_GComp *ESMC_GCompCreate(int *rc);
 int ESMC_GCompDestroy(ESMC_GComp *gcomp);

 #endif  // ESMC_GComp_H
