// $Id: ESMC_GridComp.h,v 1.19.2.4 2009/01/21 21:25:24 cdeluca Exp $
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

#ifndef ESMC_GridComp_H
#define ESMC_GridComp_H

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.


//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_GridComp - one line general statement about this class
//
// !DESCRIPTION: 
//
// The code in this file defines the C++ Component interfaces to 
// Gridded Components.  The implementation language is Fortran 90.
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
#include "ESMC_FTable.h"  // function & data pointer table 
#include "ESMC_State.h"
#include "ESMC_Comp.h"


// !PUBLIC TYPES:
 class ESMC_GridComp;

// !PRIVATE TYPES:

 // class declaration type
 class ESMC_GridComp {    // do *not* inherit from Base

  private:
    ESMC_F90ClassHolder fortranclass;
    
// !PUBLIC MEMBER FUNCTIONS:
//
  public:
  int ESMC_GridCompSetServices(void (*)(ESMC_GridComp *, int *));

  int ESMC_GridCompSetEntryPoint(const char *functionType,
                                 void (*)(ESMC_GridComp *, ESMC_State *,
                                          ESMC_State *, ESMC_Clock *),
                                 int phase=ESMF_SINGLEPHASE);

  int ESMC_GridCompInitialize(ESMC_State *importState, ESMC_State *exportState, 
                              ESMC_Clock *clock, int phase=ESMF_SINGLEPHASE,
                              ESMC_BlockingFlag blockingFlag=ESMF_BLOCKING);
  int ESMC_GridCompRun(ESMC_State *importState, ESMC_State *exportState, 
                       ESMC_Clock *clock, int phase=ESMF_SINGLEPHASE,
                       ESMC_BlockingFlag blockingFlag=ESMF_BLOCKING);
  int ESMC_GridCompFinalize(ESMC_State *importState, ESMC_State *exportState, 
                            ESMC_Clock *clock, int phase=ESMF_SINGLEPHASE,
                            ESMC_BlockingFlag blockingFlag=ESMF_BLOCKING);

 // accessor methods for class members.  these need more options.
  int ESMC_GridCompGet(char *name) const;
  int ESMC_GridCompSet(const char *name);
    
 // required methods inherited and overridden from the ESMC_Base class
  int ESMC_GridCompValidate(const char *options) const;
  int ESMC_GridCompPrint(const char *options) const;

 // native C++ constructors/destructors
	ESMC_GridComp(void);
	~ESMC_GridComp(void);
  
 // < declare the rest of the public interface methods here >
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_GridComp

// Create and Destroy are declared as class helper functions (not methods)
// since they create and destroy an ESMC_GridComp object itself. E.g. if Create
// were a method, the ESMC_GridComp object on whose behalf it was being invoked
// an ESMC_GridComp object.

 ESMC_GridComp *ESMC_GridCompCreate(char *name, enum ESMC_GridCompType mtype,
                                    ESMC_Grid *grid, char *configFile, 
                                    ESMC_Clock *clock, int *rc);
 int ESMC_GridCompDestroy(ESMC_GridComp *comp);


// prototypes for fortran interface routines
extern "C" {
 void FTN(f_esmf_gridcompcreate)(ESMC_GridComp *comp, char *name, 
                         ESMC_GridCompType *mtype, 
                         ESMC_Grid *grid, ESMC_Config *config, 
                         char *configFile, ESMC_Clock *clock, 
                         int *rc, ESMCI_FortranStrLenArg nlen,
			 ESMCI_FortranStrLenArg clen);
 void FTN(f_esmf_gridcompdestroy)(ESMC_GridComp *comp, int *rc);
 void FTN(esmf_gridcompsetentrypoint)(void *ptr, char *tname, void *func,
		         int *phase, int *status, ESMCI_FortranStrLenArg slen);
 void FTN(f_esmf_gridcompinitialize)(ESMC_GridComp *gcomp, 
                         ESMC_State *importState, ESMC_State *exportState, 
                         ESMC_Clock *clock, int *phase, 
                         ESMC_BlockingFlag *blockingFlag, int *rc);
 void FTN(f_esmf_gridcomprun)(ESMC_GridComp *gcomp, 
                         ESMC_State *importState, ESMC_State *exportState, 
                         ESMC_Clock *clock, int *phase,
                         ESMC_BlockingFlag *blockingFlag, int *rc);
 void FTN(f_esmf_gridcompfinalize)(ESMC_GridComp *gcomp, 
                         ESMC_State *importState, ESMC_State *exportState, 
                         ESMC_Clock *clock, int *phase, 
                         ESMC_BlockingFlag *blockingFlag, int *rc);
 void FTN(f_esmf_gridcompget)(const ESMC_GridComp *gcomp, int *rc);
 void FTN(f_esmf_gridcompset)(ESMC_GridComp *gcomp, int *rc);
 void FTN(f_esmf_gridcompvalidate)(const ESMC_GridComp *gcomp, const char *options,
			 int *rc, ESMCI_FortranStrLenArg olen);
 void FTN(f_esmf_gridcompprint)(const ESMC_GridComp *gcomp, const char *options,
			 int *rc, ESMCI_FortranStrLenArg olen);
};
#endif   // ESMC_GridComp_H
