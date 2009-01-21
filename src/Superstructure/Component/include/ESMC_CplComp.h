// $Id: ESMC_CplComp.h,v 1.15.2.3 2009/01/21 21:25:24 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMF Component C++ declaration include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

#ifndef ESMC_CplComp_H
#define ESMC_CplComp_H

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.


//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_CplComp - one line general statement about this class
//
// !DESCRIPTION:
//
// The code in this file defines the C++ Component interfaces to 
// Cplded Components.  The implementation language is Fortran 90.
// 
//
//-----------------------------------------------------------------------------
// 
// !USES:
#include "ESMC_Start.h"
#include "ESMC_Base.h"  // all classes inherit from the ESMC Base class.
#include "ESMC_Time.h"
#include "ESMC_Clock.h"
#include "ESMC_Grid.h"
#include "ESMC_State.h"
#include "ESMC_FTable.h"  // function & data pointer table 
#include "ESMC_Comp.h"

// !PUBLIC TYPES:
 class ESMC_CplComp;

// !PRIVATE TYPES:

 // class declaration type
 class ESMC_CplComp {    // just a cover for the F90 class, no need for Base

  private:
    ESMC_F90ClassHolder fortranclass;
    
// !PUBLIC MEMBER FUNCTIONS:
//
  public:
  int ESMC_CplCompSetServices(void (*)(ESMC_CplComp *, int *));

  int ESMC_CplCompInitialize(ESMC_State *importState, ESMC_State *exportState, 
                             ESMC_Clock *clock, int phase, 
                             ESMC_BlockingFlag blockingFlag);
  int ESMC_CplCompRun(ESMC_State *importState, ESMC_State *exportState, 
                      ESMC_Clock *clock, int phase, 
                      ESMC_BlockingFlag blockingFlag);
  int ESMC_CplCompFinalize(ESMC_State *importState, ESMC_State *exportState, 
                           ESMC_Clock *clock, int phase, 
                           ESMC_BlockingFlag blockingFlag);


 // accessor methods for class members.  these need more options.
  int ESMC_CplCompGet(char *name) const;
  int ESMC_CplCompSet(const char *name);
    
  // standard routines
  int ESMC_CplCompValidate(const char *options) const;
  int ESMC_CplCompPrint(const char *options) const;

 // native C++ constructors/destructors
	ESMC_CplComp(void);
	~ESMC_CplComp(void);
  
 // < declare the rest of the public interface methods here >
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_CplComp

// Create and Destroy are declared as class helper functions (not methods)
// since they create and destroy an ESMC_CplComp object itself. E.g. if Create
// were a method, the ESMC_CplComp object on whose behalf it was being invoked
// an ESMC_CplComp object.

 ESMC_CplComp *ESMC_CplCompCreate(char *name, char *configFile, 
                                  ESMC_Clock *clock, int *rc);
 int ESMC_CplCompDestroy(ESMC_CplComp *comp);


// prototypes for fortran interface routines
extern "C" {
 void FTN(f_esmf_cplcompcreate)(ESMC_CplComp *comp, char *name, 
                         ESMC_Config *config, char *configFile, 
                         ESMC_Clock *clock, int *rc,
			 ESMCI_FortranStrLenArg nlen, ESMCI_FortranStrLenArg clen);
 void FTN(f_esmf_cplcompdestroy)(ESMC_CplComp *comp, int *rc);
 void FTN(f_esmf_cplcompinitialize)(ESMC_CplComp *ccomp, 
                         ESMC_State *importState, ESMC_State *exportState, 
                         ESMC_Clock *clock, int *phase, 
                         ESMC_BlockingFlag *blockingFlag, int *rc);
 void FTN(f_esmf_cplcomprun)(ESMC_CplComp *ccomp, 
                         ESMC_State *importState, ESMC_State *exportState, 
                         ESMC_Clock *clock, int *phase,
                         ESMC_BlockingFlag *blockingFlag, int *rc);
 void FTN(f_esmf_cplcompfinalize)(ESMC_CplComp *ccomp, 
                         ESMC_State *importState, ESMC_State *exportState, 
                         ESMC_Clock *clock, int *phase, 
                         ESMC_BlockingFlag *blockingFlag, int *rc);
 void FTN(f_esmf_cplcompget)(const ESMC_CplComp *ccomp, int *rc);
 void FTN(f_esmf_cplcompset)(ESMC_CplComp *ccomp, int *rc);
 void FTN(f_esmf_cplcompvalidate)(const ESMC_CplComp *ccomp, const char *options,
				int *rc, ESMCI_FortranStrLenArg olen);
 void FTN(f_esmf_cplcompprint)(const ESMC_CplComp *ccomp, const char *options,
				int *rc, ESMCI_FortranStrLenArg olen);
};

#endif   // ESMC_CplComp_H
