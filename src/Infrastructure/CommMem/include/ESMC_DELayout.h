// $Id: ESMC_DELayout.h,v 1.4 2003/03/11 03:00:43 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMF DELayout C++ declaration include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

 #ifndef ESMC_DELayout_H
 #define ESMC_DELayout_H

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.
// #include <ESMC_CommMem.h> 

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_DELayout - Topology of DE's mapped onto a PEList
//
// !DESCRIPTION:
//
// The code in this file defines the C++ DELayout members and declares method 
// signatures (prototypes).  The companion file ESMC\_DELayout.C contains
// the definitions (full code bodies) for the DELayout methods.
//
// 
//
//-----------------------------------------------------------------------------
// 
// !USES:
 #include <ESMC_Base.h>  // all classes inherit from the ESMC Base class.
 #include <ESMC_PE.h>  
 #include <ESMC_PEList.h>  
 #include <ESMC_DE.h>  
 #include <ESMC_Comm.h>  
 //#include <ESMC_XXX.h>   // other dependent classes (subclasses, aggregates,
                        // composites, associates, friends)

// !PUBLIC TYPES:
// class ESMC_DELayoutConfig;
 class ESMC_DELayout;

// !PRIVATE TYPES:

 // class configuration type
 //class ESMC_DELayoutConfig {
 //  private:
 //   < insert resource items here >
 //};

// hint type about the most performance critical communication direction
enum ESMC_CommHint_e {ESMC_NOHINT, ESMC_XFAST, ESMC_YFAST, ESMC_ZFAST};

 // class definition type
 class ESMC_DELayout : public ESMC_Base {    // inherits from ESMC_Base class

   private:
 //  < insert class members here >  corresponds to type ESMF_DELayout members
 //                                 in F90 modules
    ESMC_DE ***layout;     // 3D topology (3D array) of DE's -- aligns
                           //   with (maps to) a Distributed Grid
    int nxDELayout;          // number of DE's laid out in the x-direction
    int nyDELayout;          // number of DE's laid out in the y-direction
    int nzDELayout;          // number of DE's laid out in the z-direction
    ESMC_PEList *peList;   // PE list on which the layout maps. 
    ESMC_CommHint_e commHint;  // hint about direction of the most performance
                               //   critical (frequent and/or voluminous)
                               //   communication direction: x, y or z

                      // TODO: should these really be part of DELayout, or 
                      //       standalone ?
    ESMC_DE myDE;     // the DE on which this DELayout copy (instance) resides
    ESMC_PE myPE;     // the PE on which this DELayout copy (instance) resides
    ESMC_Comm comm;   // comm object for this DE (TODO: make property of DE ? )
    ESMC_Machine Mach;// machine model for this layout
                      //  (TODO: make property of DE or PE ? heterogenous PEs)

// !PUBLIC MEMBER FUNCTIONS:
//
// pick one or the other of the init/create sections depending on
//  whether this is a deep class (the class/derived type has pointers to
//  other memory which must be allocated/deallocated) or a shallow class
//  (the class/derived type is self-contained) and needs no destroy methods
//  other than deleting the memory for the object/derived type itself.

  public:
 // the following methods apply to deep classes only
    int ESMC_DELayoutConstruct(int nx, int ny, int *delist,
                             ESMC_CommHint_e commhint); 
    int ESMC_DELayoutConstruct(int nx, int ny, int nz, ESMC_PEList *pelist,
                             ESMC_CommHint_e commhint); 
                                             // internal only, deep class
    int ESMC_DELayoutDestruct(void);           // internal only, deep class
    int ESMC_DELayoutInit(void);

 // optional configuration methods
//    int ESMC_DELayoutGetConfig(ESMC_DELayoutConfig *config) const;
//    int ESMC_DELayoutSetConfig(const ESMC_DELayoutConfig *config);

 // accessor methods for class members
//    int ESMC_DELayoutGet<Value>(<value type> *value) const;
//    int ESMC_DELayoutSet<Value>(<value type>  value);
    int ESMC_DELayoutGetSize(int *nx, int *ny) const;
    int ESMC_DELayoutGetSize(int *nx, int *ny, int *nz) const;
    int ESMC_DELayoutGetDEPosition(ESMC_DE *de, int *x, int *y, int *z) const;
    int ESMC_DELayoutGetDEPosition(int *x, int *y) const;
    int ESMC_DELayoutGetDEid(int *deid) const;
    int ESMC_DELayoutSetAxisIndex(int global_counts[], int size_gcount,
                                int decompids[], int size_decomp,
                                ESMC_AxisIndex *AIPtr);
    int ESMC_DELayoutGatherArrayI(int *DistArray, int decompids[], int size_decomp,
                                ESMC_AxisIndex *AIPtr, ESMC_AxisIndex *AIPtr2, 
                                int *GlobalArray);
    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_DELayoutValidate(void) const;
    int ESMC_DELayoutPrint(void) const;

 // native C++ constructors/destructors
	ESMC_DELayout(void);
	~ESMC_DELayout(void);
  
 // < declare the rest of the public interface methods here >
  // TODO: make comm public and invoke directly on comm rather than at
  //       DELayout level ? (does not depend on any DELayout knowledge)
  //       Or remove comm from DELayout (standalone) and remove these
  //       entry points from DELayout?

    int ESMC_DELayoutAllGatherVI(int *sndArray, int  sndLen, 
                               int *rcvArray, int *rcvLen, int *rcvDispls);
    int ESMC_DELayoutAllReduce(int *dataArray, int *result, int arrayLen,
                             ESMC_Op_e op);
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_DELayout

    ESMC_DELayout *ESMC_DELayoutCreate(int nx, int ny, int *delist,
                                   ESMC_CommHint_e commhint, int *rc);
                                                 // interface only, deep class
    ESMC_DELayout *ESMC_DELayoutCreate(int nx, int ny, int nz, ESMC_PEList *pelist,
                                   ESMC_CommHint_e commhint, int *rc);
                                                 // interface only, deep class
    int ESMC_DELayoutDestroy(ESMC_DELayout *layout); // interface only, deep class

 #endif  // ESMC_DELayout_H
