// $Id: ESMC_Cpl.h,v 1.5 2004/04/20 19:03:26 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMF Cpl C++ declaration include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

#ifndef ESMC_Cpl_H
#define ESMC_Cpl_H

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.
#include <ESMC_Comp.h> 

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_Cpl - one line general statement about this class
//
// !DESCRIPTION:
//
// The code in this file defines the C++ Cpl members and declares method 
// signatures (prototypes).  The companion file ESMC\_Cpl.C contains
// the definitions (full code bodies) for the Cpl methods.
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
 class ESMC_Cpl;

// !PRIVATE TYPES:

 // class declaration type
 class ESMC_Cpl : public ESMC_Base {    // inherits from ESMC_Base class

   private:
 //  < insert class members here >  // real class definition

// !PUBLIC MEMBER FUNCTIONS:
//
  public:
    int ESMC_CplInit(void);
    int ESMC_CplRun(void);
    int ESMC_CplFinalize(void);

 // accessor methods for class members
    //int ESMC_CplGet<Value>(<value type> *value) const;
    //int ESMC_CplSet<Value>(<value type>  value);
    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_CplValidate(const char *options) const;
    int ESMC_CplPrint(const char *options) const;

 // secondary construct/destruct routines
    int ESMC_CplConstruct(void);
    int ESMC_CplDestruct(void);

 // native C++ constructors/destructors
	ESMC_Cpl(void);
	~ESMC_Cpl(void);
  
 // < declare the rest of the public interface methods here >
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_Cpl

// Create and Destroy are declared as class helper functions (not methods)
// since they create and destroy an ESMC_Cpl object itself. E.g. if Create
// were a method, the ESMC_Cpl object on whose behalf it was being invoked
// would need to already exist!  These functions are supersets of C++ new
// and delete; they perform allocation/deallocation specialized to
// an ESMC_Cpl object.

 ESMC_Cpl *ESMC_CplCreate(int *rc);
 int ESMC_CplDestroy(ESMC_Cpl *cpl);

 #endif  // ESMC_Cpl_H
