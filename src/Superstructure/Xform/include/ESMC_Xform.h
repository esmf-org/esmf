// $Id: ESMC_Xform.h,v 1.2.8.3 2007/10/18 02:44:07 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMF Xform C++ declaration include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

 #ifndef ESMC_Xform_H
 #define ESMC_Xform_H

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_Xform - one line general statement about this class
//
// !DESCRIPTION:
//
// The code in this file defines the C++ Xform members and declares method 
// signatures (prototypes).  The companion file ESMC\_Xform.C contains
// the definitions (full code bodies) for the Xform methods.
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
 class ESMC_XformConfig;
 class ESMC_Xform;

// !PRIVATE TYPES:

 // class configuration type
 class ESMC_XformConfig {
   private:
 //   < insert resource items here >
 };

 // class declaration type
 class ESMC_Xform : public ESMC_Base {    // inherits from ESMC_Base class

   private:
   // class data here

// !PUBLIC MEMBER FUNCTIONS:
//

  public:
    int ESMC_XformInit(char *name, void (*funcp)(void));

 // optional configuration methods
    int ESMC_XformGetConfig(ESMC_XformConfig *config) const;
    int ESMC_XformSetConfig(const ESMC_XformConfig *config);

 // accessor methods for class members
    //int ESMC_XformGet<Value>(<value type> *value) const;
    //int ESMC_XformSet<Value>(<value type>  value);
    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_XformValidate(const char *options) const;
    int ESMC_XformPrint(const char *options) const;


 // native C++ constructors/destructors
 	ESMC_Xform(void);
	~ESMC_Xform(void);
  
 // < declare the rest of the public interface methods here >
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_Xform


 #endif  // ESMC_Xform_H
