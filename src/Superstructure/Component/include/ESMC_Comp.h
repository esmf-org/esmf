// $Id: ESMC_Comp.h,v 1.14 2004/04/09 20:19:53 eschwab Exp $
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

#ifndef ESMC_Comp_H
#define ESMC_Comp_H

//-----------------------------------------------------------------------------


 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.

#define ESMF_INIT 1
#define ESMF_RUN 2
#define ESMF_FINAL 3

enum ESMC_CompType { ESMF_COMPTYPE_GRID=1, ESMF_COMPTYPE_CPL, 
                     ESMF_COMPTYPE_UNKNOWN };
enum ESMC_GridCompType { ESMF_ATM=1, ESMF_LAND, ESMF_OCEAN, ESMF_SEAICE, 
                      ESMF_RIVER, ESMF_GRIDCOMPTYPE_UNKNOWN };

extern const char *ESMC_SetInit;
extern const char *ESMC_SetRun;
extern const char *ESMC_SetFinal;
extern const char *ESMC_SetWriteRestart;
extern const char *ESMC_SetReadRestart;


//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_Comp - one line general statement about this class
//
// !DESCRIPTION:
//
// The code in this file defines the C++ Component members and declares method 
// signatures (prototypes).  The companion file ESMC\_Comp.C contains
// the definitions (full code bodies) for the Component methods.
//
// 
//
//-----------------------------------------------------------------------------
// 
// !USES:
#include "ESMC.h"
#include "ESMC_Base.h"  // all classes inherit from the ESMC Base class.
#include "ESMC_DELayout.h"
#include "ESMC_Calendar.h"
#include "ESMC_State.h"
#include "ESMC_FTable.h"  // function & data pointer table 

// !PUBLIC TYPES:
 class ESMC_CompConfig;
 class ESMC_Comp;

// TODO: need C++ interface to config objects.  REMOVE THIS WHEN READY.
#define ESMC_Config char

// !PRIVATE TYPES:

 // class configuration type
 class ESMC_CompConfig {
   private:
 //   < insert resource items here >
 };

 // class declaration type
 class ESMC_Comp : public ESMC_Base {    // inherits from ESMC_Base class

   private:
    void *fortranclass;
    
// !PUBLIC MEMBER FUNCTIONS:
//
  public:
    int ESMC_CompRegister(void *);

    int ESMC_CompInit(void);
    int ESMC_CompRun(int timesteps);
    int ESMC_CompFinal(void);

 // optional configuration methods
    int ESMC_CompGetConfig(ESMC_CompConfig *config) const;
    int ESMC_CompSetConfig(const ESMC_CompConfig *config);

 // accessor methods for class members.  these need more options.
    int ESMC_CompGet(char *name) const;
    int ESMC_CompSet(const char *name);
    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_CompValidate(const char *options) const;
    int ESMC_CompPrint(const char *options) const;

 // native C++ constructors/destructors
	ESMC_Comp(void);
	~ESMC_Comp(void);
  
 // < declare the rest of the public interface methods here >
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_Comp

// Create and Destroy are declared as class helper functions (not methods)
// since they create and destroy an ESMC_Comp object itself. E.g. if Create
// were a method, the ESMC_Comp object on whose behalf it was being invoked
// would need to already exist!  These functions are supersets of C++ new
// and delete; they perform allocation/deallocation specialized to
// an ESMC_Comp object.

 ESMC_Comp *ESMC_CompCreate(char *name, ESMC_DELayout *layout,
                                      enum ESMC_CompType ctype,
                                      enum ESMC_GridCompType mtype,
                                      char *filepath, int *rc);
 int ESMC_CompDestroy(ESMC_Comp *comp);

 int ESMC_Initialize(ESMC_CalendarType defaultCalendar=ESMC_CAL_NOCALENDAR);
 int ESMC_Initialize(int argc, char **argv,
                     ESMC_CalendarType defaultCalendar=ESMC_CAL_NOCALENDAR);
 int ESMC_Finalize(void);

// prototypes for fortran interface routines
extern "C" {
   void FTN(f_esmf_compcreate)(ESMC_Comp *compp, char *name, int *rc, int nlen);
   void FTN(f_esmf_compdestroy)(ESMC_Comp *compp, char *name, int *rc, int nlen);
   void FTN(f_esmf_compinit)(ESMC_Comp *compp, char *name, void *func, int *rc, int nlen);
   void FTN(f_esmf_comprun)(ESMC_Comp *compp, char *name, int *rc, int nlen);
   void FTN(f_esmf_compfinalize)(ESMC_Comp *compp, char *name, int *rc, int nlen);

   void FTN(f_esmf_frameworkinitialize)(int *language, 
                                        ESMC_CalendarType *defaultCalendar,
                                        int *rc);
   void FTN(f_esmf_frameworkfinalize)(int *rc);
};

 #endif  // ESMC_Comp_H
