// $Id: ESMC_Comp.h,v 1.31.2.3 2009/01/21 21:25:24 cdeluca Exp $
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

#ifndef ESMC_Comp_H
#define ESMC_Comp_H

//-----------------------------------------------------------------------------


 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.

#define ESMF_INIT 1
#define ESMF_RUN 2
#define ESMF_FINAL 3
#define ESMF_WRITERESTART 4
#define ESMF_READRESTART 5
#define ESMF_SINGLEPHASE 0

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
#include "ESMC_Start.h"
#include "ESMC_Base.h"  // all classes inherit from the ESMC Base class.
#include "ESMC_Calendar.h"
#include "ESMC_State.h"
#include "ESMC_Config.h"
#include "ESMC_FTable.h"  // function & data pointer table 

// !PUBLIC TYPES:
 class ESMC_Comp;

// !PRIVATE TYPES:

 // class declaration type
 class ESMC_Comp {       // a cover class for F90 - does NOT inherit from Base

   private:
    void *fortranclass;
    
// !PUBLIC MEMBER FUNCTIONS:
//
  public:
    int ESMC_CompRegister(void *);

    // not at all the final interfaces - just something to start
    // early testing with the C++ interfaces.
    int ESMC_CompInit(void);
    int ESMC_CompRun(int timesteps);
    int ESMC_CompFinal(void);
    //int ESMC_CompWriteRestart(void);
    //int ESMC_CompReadRestart(void);

    // accessor methods for class members.  these need more options.
    int ESMC_CompGet(char *name) const;
    int ESMC_CompSet(const char *name);
    
    // standard methods
    int ESMC_CompValidate(const char *options) const;
    int ESMC_CompPrint(const char *options) const;

    // native C++ constructors/destructors
    ESMC_Comp(void);
    ~ESMC_Comp(void);
  
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
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

 // this is missing the config object, and a clock object.
 ESMC_Comp *ESMC_CompCreate(char *name, enum ESMC_CompType ctype,
                            enum ESMC_GridCompType mtype, char *filepath, 
                            int *rc);
 int ESMC_CompDestroy(ESMC_Comp *comp);

// prototypes for fortran interface routines
extern "C" {
   void FTN(f_esmf_compsetvminfo)(ESMC_Comp *compp, void *vm_info, int *rc);
   void FTN(f_esmf_compgetvmparent)(ESMC_Comp *compp, ESMCI::VM **vmparent, 
     int *rc);
   void FTN(f_esmf_compgetvmplan)(ESMC_Comp *compp, ESMCI::VMPlan **vmplan, 
     int *rc);
   void FTN(f_esmf_compinsertvm)(ESMC_Comp *compp, void *vm, int *rc);
   void FTN(f_esmf_compget)(ESMC_Comp *compp, ESMC_CompType *ctype, int *rc);
   void FTN(f_esmf_compreplicate)(ESMC_Comp *compp, ESMC_Comp *compp_src,
     void *vm, int *rc);
   void FTN(f_esmf_compcopy)(ESMC_Comp *compp, ESMC_Comp *compp_src, int *rc);
   void FTN(f_esmf_compdelete)(ESMC_Comp *compp, int *rc);
   void FTN(f_esmf_compcreate)(ESMC_Comp *compp, char *name, int *rc,
				ESMCI_FortranStrLenArg nlen);
   void FTN(f_esmf_compdestroy)(ESMC_Comp *compp, char *name, int *rc,
				ESMCI_FortranStrLenArg nlen);
   void FTN(f_esmf_compinit)(ESMC_Comp *compp, char *name, void *func,
                             int *rc, ESMCI_FortranStrLenArg nlen);
   void FTN(f_esmf_comprun)(ESMC_Comp *compp, char *name, int *rc,
				ESMCI_FortranStrLenArg nlen);
   void FTN(f_esmf_compfinalize)(ESMC_Comp *compp, char *name, int *rc,
                                ESMCI_FortranStrLenArg nlen);

};

 #endif  // ESMC_Comp_H
