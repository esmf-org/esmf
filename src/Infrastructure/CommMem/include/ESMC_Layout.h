// $Id: ESMC_Layout.h,v 1.4 2003/01/09 02:15:54 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMF Layout C++ declaration include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

 #ifndef ESMC_Layout_H
 #define ESMC_Layout_H

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.
// #include <ESMC_CommMem.h> 

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_Layout - Topology of DE's mapped onto a PElist
//
// !DESCRIPTION:
//
// The code in this file defines the C++ Layout members and declares method 
// signatures (prototypes).  The companion file ESMC\_Layout.C contains
// the definitions (full code bodies) for the Layout methods.
//
// < insert a paragraph or two explaining what you'll find in this file >
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
// class ESMC_LayoutConfig;
 class ESMC_Layout;

// !PRIVATE TYPES:

 // class configuration type
 //class ESMC_LayoutConfig {
 //  private:
 //   < insert resource items here >
 //};

// hint type about the most performance critical communication direction
enum ESMC_CommHint_e {ESMC_NOHINT, ESMC_XFAST, ESMC_YFAST, ESMC_ZFAST};

 // class definition type
 class ESMC_Layout : public ESMC_Base {    // inherits from ESMC_Base class

   private:
 //  < insert class members here >  corresponds to type ESMF_Layout members
 //                                 in F90 modules
    ESMC_DE ***layout;     // 3D topology (3D array) of DE's -- aligns
                           //   with (maps to) a Distributed Grid
    int nxLayout;          // number of DE's laid out in the x-direction
    int nyLayout;          // number of DE's laid out in the y-direction
    int nzLayout;          // number of DE's laid out in the z-direction
    ESMC_PEList *peList;   // PE list on which the layout maps. 
    ESMC_CommHint_e commHint;  // hint about direction of the most performance
                               //   critical (frequent and/or voluminous)
                               //   communication direction: x, y or z
    ESMC_DE myDE;     // the DE on which this Layout copy (instance) resides
    ESMC_PE myPE;     // the PE on which this Layout copy (instance) resides
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
    int ESMC_LayoutConstruct(int nx, int ny, int *delist,
                             ESMC_CommHint_e commhint); 
    int ESMC_LayoutConstruct(int nx, int ny, int nz, ESMC_PEList *pelist,
                             ESMC_CommHint_e commhint); 
                                             // internal only, deep class
    int ESMC_LayoutDestruct(void);           // internal only, deep class
//    int ESMC_LayoutInit(args);

 // optional configuration methods
//    int ESMC_LayoutGetConfig(ESMC_LayoutConfig *config) const;
//    int ESMC_LayoutSetConfig(const ESMC_LayoutConfig *config);

 // accessor methods for class members
//    int ESMC_LayoutGet<Value>(<value type> *value) const;
//    int ESMC_LayoutSet<Value>(<value type>  value);
    int ESMC_LayoutGetSize(int *nx, int *ny) const;
    int ESMC_LayoutGetSize(int *nx, int *ny, int *nz) const;
    int ESMC_LayoutGetDEPosition(ESMC_DE *de, int *x, int *y, int *z) const;
    int ESMC_LayoutGetDEPosition(int *x, int *y) const;
    int ESMC_LayoutGetDEid(int *deid) const;
    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_LayoutValidate(void) const;
    int ESMC_LayoutPrint(void) const;

 // native C++ constructors/destructors
	ESMC_Layout(void);
	~ESMC_Layout(void);
  
 // < declare the rest of the public interface methods here >
    int ESMC_LayoutAllReduce(int *dataArray, int *result, int arrayLen,
                             ESMC_Op_e op);
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_Layout

    ESMC_Layout *ESMC_LayoutCreate(int nx, int ny, int *delist,
                                   ESMC_CommHint_e commhint, int *rc);
                                                 // interface only, deep class
    ESMC_Layout *ESMC_LayoutCreate(int nx, int ny, int nz, ESMC_PEList *pelist,
                                   ESMC_CommHint_e commhint, int *rc);
                                                 // interface only, deep class
    int ESMC_LayoutDestroy(ESMC_Layout *layout); // interface only, deep class

 #endif  // ESMC_Layout_H
