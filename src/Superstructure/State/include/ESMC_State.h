// $Id: ESMC_State.h,v 1.20 2008/04/02 20:42:59 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMF State C++ declaration include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

 #ifndef ESMC_State_H
 #define ESMC_State_H

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_State - C interface to the F90 State object
//
// !DESCRIPTION:
//
// The code in this file defines the C State members and declares method 
// signatures (prototypes).  The companion file ESMC\_State.C contains
// the definitions (full code bodies) for the State methods.
//
// 
//
//-----------------------------------------------------------------------------
// 
// !USES:
#include "ESMC_Array.h"


extern "C" {

// class declaration type
typedef struct {

    // pointer 
    void* ptr;


//
//EOP
//-----------------------------------------------------------------------------

} ESMC_State ; // end class ESMC_State


 // accessor methods for class members
    //int ESMC_StateGet<Value>(<value type> *value) const;
    //int ESMC_StateSet<Value>(<value type>  value);
    
 // required methods inherited and overridden from the ESMC_Base class
 //   int ESMC_StatePrint(const char *options) const;

 // initialize and free secondary resources
 //   int ESMC_StateConstruct(void);
 //   int ESMC_StateDestruct(void);

  
//
//EOP
//-----------------------------------------------------------------------------



 ESMC_State ESMC_StateCreate(char *name, int *rc);

 int ESMC_StateDestroy(ESMC_State state);

 int ESMC_StateAddArray(ESMC_State state, ESMC_Array array);

 int ESMC_StateGetArray(ESMC_State state, char *name, ESMC_Array *array);
 }; // end extern "C"

// !PRIVATE TYPES:

typedef enum ESMC_StateType {
      ESMC_StateImport=1, ESMC_StateExport,
      ESMC_StateImpExp, ESMC_StateUnknown } ESMC_StateType;

typedef enum ESMC_Objtype { FieldBundle=1, Field=2, Array=3 } ESMC_Objtype;
typedef enum ESMC_Needed { Needed=1, NotNeeded=2 } ESMC_Needed;
typedef enum ESMC_Ready { ReadyToRead=1, ReadyToWrite=2 } ESMC_Ready;


 #endif  // ESMC_State_H
