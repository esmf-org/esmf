// $Id: ESMC_Regrid.h,v 1.7.2.2 2009/01/21 21:25:23 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMF Regrid C++ declaration include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

 #ifndef ESMC_Regrid_H
 #define ESMC_Regrid_H

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.
 #include <ESMC_Regrid.h> 

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_Regrid - one line general statement about this class
//
// !DESCRIPTION:
//
// The code in this file defines the C++ Regrid members and declares method 
// signatures (prototypes).  The companion file ESMC\_Regrid.C contains
// the definitions (full code bodies) for the Regrid methods.
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
 class ESMC_RegridConfig;
 class ESMC_Regrid;

// !PRIVATE TYPES:

 // class configuration type
 class ESMC_RegridConfig {
   private:
 //   < insert resource items here >
 };

 // class declaration type
 class ESMC_Regrid : public ESMC_Base {    // inherits from ESMC_Base class

   private:
 // select 1 from the 2 options below:
 //  if deep class, the class only contains the address of the fortran 90
 //  derived type:
     void *f90this;

 //  if shallow class, the members of this class must correspond exactly to
 //  the fortran derived type - in size and order.
 //  < insert class members here >  corresponds to type ESMF_Regrid members
 //                                 in F90 modules
  

// !PUBLIC MEMBER FUNCTIONS:
//
// pick one or the other of the init/create sections depending on
//  whether this is a deep class (the class/derived type has pointers to
//  other memory which must be allocated/deallocated) or a shallow class
//  (the class/derived type is self-contained) and needs no destroy methods
//  other than deleting the memory for the object/derived type itself.

  public:
 // the following methods apply to deep classes only
 // ESMC\_RegridCreate and ESMC\_RegridDestroy are declared below,
 // outside the ESMC_Regrid declaration

 // or
 // the following method applies to a shallow class
    int ESMC_RegridInit(int *arg1, int *arg2, const char *arg3,
                        int *rc);         // shallow class only

 // optional configuration methods
    int ESMC_RegridGetConfig(ESMC_RegridConfig *config) const;
    int ESMC_RegridSetConfig(const ESMC_RegridConfig *config);

 // accessor methods for class members
  //  int ESMC_RegridGet(<value type> *value) const;
  //  int ESMC_RegridSet(<value type>  value);
    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_RegridValidate(const char *options) const;
    int ESMC_RegridPrint(const char *options) const;

 // native C++ constructors/destructors
	ESMC_Regrid(args);
	~ESMC_Regrid(args);
  
 // < declare the rest of the public interface methods here >
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_Regrid

// Create and Destroy are declared as class helper functions (not methods)
// since they create and destroy an ESMC_Regrid object itself. E.g. if Create
// were a method, the ESMC_Regrid object on whose behalf it was being invoked
// would need to already exist!  These functions are supersets of C++ new
// and delete; they perform allocation/deallocation specialized to
// an ESMC_Regrid object.

 ESMC_Regrid *ESMC_RegridCreate(int *arg1, int *arg2, const char *arg3,
                                int *rc);// interface only, deep class
 int ESMC_RegridDestroy(ESMC_Regrid *regrid); // interface only, deep class

 #endif  // ESMC_Regrid_H
