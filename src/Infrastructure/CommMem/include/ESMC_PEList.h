// $Id: ESMC_PEList.h,v 1.5 2003/03/13 22:56:12 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMF PEList C++ definition include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

 #ifndef ESMC_PEList_H
 #define ESMC_PEList_H

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.
// #include <ESMC_CommMem.h> 

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_PEList - contains a list of processing elements
//
// !DESCRIPTION:
//
// The code in this file defines the C++ PEList members and declares method 
// signatures (prototypes).  The companion file ESMC\_PEList.C contains
// the definitions (full code bodies) for the PEList methods.
//
// 
//
//-----------------------------------------------------------------------------
// 
// !USES:
 #include <ESMC_Base.h>  // all classes inherit from the ESMC Base class.
 #include <ESMC_PE.h>
 //#include <ESMC_XXX.h>   // other dependent classes (subclasses, aggregates,
                        // composites, associates, friends)

// !PUBLIC TYPES:
// class ESMC_PEListConfig;
 class ESMC_PEList;

// !PRIVATE TYPES:

 // class configuration type
// class ESMC_PEListConfig {
//   private:
 //   < insert resource items here >
// };

 // class definition type
 class ESMC_PEList : public ESMC_Base {    // inherits from ESMC_Base class

   private:
 //  < insert class members here >  corresponds to type ESMF_PE members
 //                                 in F90 modules
     ESMC_PE *peList;         // dynamically allocated list
     int numPEs;              // number of PEs in list

// !PUBLIC MEMBER FUNCTIONS:
//
  public:
 // the following methods apply to deep classes only
    int ESMC_PEListConstruct(int numpes);    // internal only, deep class
    int ESMC_PEListDestruct(void);           // internal only, deep class
    int ESMC_PEListInit(int i, int esmfid, int cpuid, int nodeid);
                                             // initialize ith PE

 // optional configuration methods
//    int ESMC_PEListGetConfig(ESMC_PEListConfig *config) const;
//    int ESMC_PEListSetConfig(const ESMC_PEListConfig *config);

 // accessor methods for class members
//    int ESMC_PEListGet<Value>(<value type> *value) const;
//    int ESMC_PEListSet<Value>(<value type>  value);
    int ESMC_PEListGetPE(int i, ESMC_PE **pe) const;
    int ESMC_PEListSetPE(int i, ESMC_PE *pe);
    int ESMC_PEListGetNumPEs(int *npes) const;
    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_PEListValidate(void) const;
    int ESMC_PEListPrint(void) const;

 // native C++ constructors/destructors
	ESMC_PEList(void);
	~ESMC_PEList(void);
  
 // < declare the rest of the public interface methods here >
    int ESMC_PEListSort(void);

    friend ESMC_PEList *ESMC_PEListCreate(int firstpe, int lastpe, int *rc);
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_PEList

ESMC_PEList *ESMC_PEListCreate(int numpes, int *rc);
ESMC_PEList *ESMC_PEListCreate(int firstpe, int lastpe, int *rc);
                                              // interface only, deep class
int ESMC_PEListDestroy(ESMC_PEList *pelist);  // interface only, deep class

#ifdef __cplusplus
extern "C" {
#endif
 // class helper function for qsort() in ESMC_PEListSort()
 int ESMC_PEListPECompare(const void *pe1, const void *pe2);
#ifdef __cplusplus
}
#endif

 #endif  // ESMC_PEList_H
