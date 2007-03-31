// $Id: ESMC_Field.h,v 1.6 2007/03/31 05:51:02 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMF Field C++ declaration include file
//
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

#ifndef ESMC_Field_H
#define ESMC_Field_H

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_Field - C++ interface to the F90 Field object
//
// !DESCRIPTION:
//
// The code in this file defines the C++ Field members and declares method 
// signatures (prototypes).  The companion file ESMC\_Field.C contains
// the definitions (full code bodies) for the Field methods.
//
//-----------------------------------------------------------------------------
// 
// !USES:
#include "ESMC_Start.h"
#include "ESMC_Base.h"  // all classes inherit from the ESMC Base class.

// !PUBLIC TYPES:
 class ESMC_Field;

// !PRIVATE TYPES:

 // fortran interface declarations
extern "C" {
   void FTN(f_esmf_fieldcreate)(ESMC_Field *, int *rc);
   void FTN(f_esmf_fielddestroy)(ESMC_Field *, int *rc);
  
   void FTN(c_esmc_fieldserialize)(ESMC_Status *fieldstatus, 
              ESMC_Status *gridstatus, ESMC_Status *datastatus, 
              ESMC_Status *datamapstatus, ESMC_Status *iostatus,
              void *buffer, int *length, int *offset, int *localrc);
   
   void FTN(c_esmc_fielddeserialize)(ESMC_Status *fieldstatus,
              ESMC_Status *gridstatus, ESMC_Status *datastatus,
              ESMC_Status *datamapstatus, ESMC_Status *iostatus,
              void *buffer, int *offset, int *localrc);

};


 // class declaration type
 // TODO: decide if this does or does not inherit from the ESMC_Base class.
 //  NO: not needed because this is simply an interface to the F90 code
 //  YES: makes inheritance on the C++ side possible before crossing into F90
 class ESMC_Field { 
 //class ESMC_Field : public ESMC_Base { 

   private:
     void *handle;    // pointer to Fortran ESMF_FieldType

// !PUBLIC MEMBER FUNCTIONS:
//

   public:
     int ESMC_FieldConstruct(void);        
     int ESMC_FieldDestruct(void);        

    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_FieldValidate(const char *options) const;
    int ESMC_FieldPrint(const char *options) const;

 // native C++ constructors/destructors
	ESMC_Field(void) {  handle = 0; }
	~ESMC_Field(void) { }
  
 // < declare the rest of the public interface methods here >
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_Field

ESMC_Field *ESMC_FieldCreate(int *rc);
int ESMC_FieldDestroy(ESMC_Field *f);

#endif  // ESMC_Field_H







