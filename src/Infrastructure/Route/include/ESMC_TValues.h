// $Id: ESMC_TValues.h,v 1.6.2.2 2009/01/21 21:25:23 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMF Transform Values C++ declaration include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

 #ifndef ESMC_TValues_H
 #define ESMC_TValues_H

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_TransformValues - Regridding transformation information.
//              
//
// !DESCRIPTION:
//
// The code in this file defines the C++ TransformValues class and 
// declares method signatures (prototypes).  The companion file 
// ESMC_TValues.C contains
// the definitions (full code bodies) for the TransformValues methods.
//
// 
//
//-----------------------------------------------------------------------------
// 
// !USES:
 #include <ESMC_Base.h>  // all classes inherit from the ESMC Base class.
 #include <ESMC_LocalArray.h>

// !PUBLIC TYPES:
 class ESMC_TransformValues;

// !PRIVATE TYPES:

 // class declaration type
 class ESMC_TransformValues {   // does NOT inherit from Base class
     //  Could be a src x dst sparse matrix, but instead
     //   this stores explicitly the source index, destination
     //   index, and weighting factor.  

   private:
        // TODO:  needs a transform type enum
        // TODO:  needs option for single large sparse matrix, etc.

        int numlist;
        ESMC_LocalArray *srcindex;
        ESMC_LocalArray *dstindex;
        ESMC_LocalArray *weights;

   public:
    int ESMC_TransformValuesConstruct(int count);
    int ESMC_TransformValuesDestruct(void);

 // accessor methods for class members
    int ESMC_TransformValuesGet(int *numlist, ESMC_LocalArray **si,
                              ESMC_LocalArray **di, ESMC_LocalArray **w) const;
    int ESMC_TransformValuesGet(int *numlist, struct c_F90ptr *si,
                              struct c_F90ptr *di, struct c_F90ptr *w) const;
    int ESMC_TransformValuesSet(int numlist, ESMC_LocalArray *si,
                               ESMC_LocalArray *di, ESMC_LocalArray *w);

 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_TransformValuesValidate(const char *options) const;
    int ESMC_TransformValuesPrint(const char *options) const;

 // native C++ constructors/destructors
        ESMC_TransformValues(void);
        ~ESMC_TransformValues(void);

 };   // end class ESMC_TransformValues


 ESMC_TransformValues *ESMC_TransformValuesCreate(int *rc);
 ESMC_TransformValues *ESMC_TransformValuesCreate(int count, int *rc);
 int ESMC_TransformValuesDestroy(ESMC_TransformValues *tv);

 #endif  // ESMC_TValues_H
