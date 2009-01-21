// $Id: ESMC_State.h,v 1.11.2.6 2009/01/21 21:25:25 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
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

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_State - one line general Statement about this class
//
// !DESCRIPTION:
//
// The code in this file defines the C++ State members and declares method 
// signatures (prototypes).  The companion file ESMC\_State.C contains
// the definitions (full code bodies) for the State methods.
//
// 
//
//-----------------------------------------------------------------------------
// 
// !USES:
#include "ESMC_Base.h"  // all classes inherit from the ESMC Base class.
#include "ESMCI_Array.h"


// !PUBLIC TYPES:
 class ESMC_State;

// !PRIVATE TYPES:

typedef enum ESMC_StateType { 
      ESMC_StateImport=1, ESMC_StateExport, 
      ESMC_StateImpExp, ESMC_StateUnknown } ESMC_StateType;

typedef enum ESMC_Objtype { FieldBundle=1, Field=2, Array=3 } ESMC_Objtype;
typedef enum ESMC_Needed { Needed=1, NotNeeded=2 } ESMC_Needed;
typedef enum ESMC_Ready { ReadyToRead=1, ReadyToWrite=2 } ESMC_Ready;

 // class declaration type
 class ESMC_State : public ESMC_Base {    // inherits from ESMC_Base class

   private:
      void *statep;    // object implemented in F90

// !PUBLIC MEMBER FUNCTIONS:
//

  public:
    int ESMC_StateReady(void);
    int ESMC_StateValidate(const char *options) const;
    int ESMC_StateFinalize(void);

    int ESMC_StateReceive(void);
    int ESMC_StateSend(void);

 // accessor methods for class members
    //int ESMC_StateGet<Value>(<value type> *value) const;
    //int ESMC_StateSet<Value>(<value type>  value);
    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_StatePrint(const char *options) const;

 // initialize and free secondary resources
    int ESMC_StateConstruct(void);
    int ESMC_StateDestruct(void);

 // native C++ constructors/destructors
	ESMC_State(void);
	~ESMC_State(void);
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_State

// Create and Destroy are declared as class helper functions (not methods)
// since they create and destroy an ESMC_State object itself. E.g. if Create
// were a method, the ESMC_State object on whose behalf it was being invoked
// would need to already exist!  These functions are supersets of C++ new
// and delete; they perform allocation/deallocation specialized to
// an ESMC_State object.

 ESMC_State *ESMC_StateCreate(char *name, int *rc);
 int ESMC_StateDestroy(ESMC_State *state);

 extern "C" {

  void FTN(c_esmc_stateserialize)(int *statestatus, int *st, 
              int *needed_default, int *ready_default, int *stvalid_default, 
              int *reqrestart_default, int *alloccount, int *datacount, 
              void *buffer, int *length, int *offset, int *localrc);

  void FTN(c_esmc_statedeserialize)(int *statestatus, int *st, 
              int *needed_default, int *ready_default, int *stvalid_default, 
              int *reqrestart_default, int *alloccount, int *datacount, 
              void *buffer, int *offset, int *localrc);

  void FTN(c_esmc_stateitemserialize)(int *otype, char *namep, 
               int *indirect_index, int *needed, int *ready, 
               int *valid, int *reqrestart, 
               void *buffer, int *length, int *offset, int *localrc, int clen);

  void FTN(c_esmc_stateitemdeserialize)(int *otype, char *namep, 
              int *indirect_index, int *needed, int *ready, int *valid, 
              int *reqrestart, 
              void *buffer, int *offset, int *localrc, int clen);

  void FTN(f_esmf_statecreate)(ESMC_State *state, char *statename, int *rc);
  void FTN(f_esmf_statedestroy)(ESMC_State *state, int *rc);

#if 0
      TODO: finish these prototypes
      void FTN(f_esmf_statecreate)(char *statename, statetype, compname,
            bundles, fields, arrays, nestedstates, names, itemcount, int *rc);
      void FTN(f_esmf_statedestroy)(ESMC_State *state, int *rc);
      void FTN(f_esmf_stateaddbundle)(ESMC_State *state, ESMC_FieldBundle *bundle, int *rc);
      void FTN(f_esmf_stateaddfield)(ESMC_State *state, ESMC_Field *field, int *rc);
      void FTN(f_esmf_stateaddarray)(ESMC_State *state, ESMC_Array *array, int *rc);
      void FTN(f_esmf_stateaddstate)(ESMC_State *state, nestedstate, int *rc);
      void FTN(f_esmf_stateadddataname)(ESMC_State *state, char *name, int *rc);
      void FTN(f_esmf_stategetinfo)(ESMC_State *state, char *statename, 
                          statetype, char *compname,
                          int *itemcount, char *itemnames, objtypes, int *rc);
      void FTN(f_esmf_stategetname)(ESMC_State *state, char *statename, int *rc);
      void FTN(f_esmf_stateisneeded)(ESMC_State *state, char *dataname, int *rc);
      void FTN(f_esmf_stategetneeded)(ESMC_State *state, char *dataname, 
                                 needed, int *rc);
      void FTN(f_esmf_statesetneeded)(ESMC_State *state, char *dataname, needed, int *rc);
      void FTN(f_esmf_stategetbundle)(ESMC_State *state, char *name, ESMC_FieldBundle *bundle, int *rc);
      void FTN(f_esmf_stategetfield)(ESMC_State *state, char *name, ESMC_Field *field, int *rc);
      void FTN(f_esmf_stategetarray)(ESMC_State *state, char *name, ESMC_Array *array, int *rc);
      void FTN(f_esmf_stategetstate)(ESMC_State *state, char *name, nestedstate, int *rc);
      void FTN(f_esmf_statewriterestart)(ESMC_State *state, iospec, int *rc);
      void FTN(f_esmf_statereadrestart)(char *name, iospec, int *rc);
      void FTN(f_esmf_statevalidate)(ESMC_State *state, char *options, int *rc);
      void FTN(f_esmf_stateprint)(ESMC_State *state, char *options, int *rc);
#endif
 }

 #endif  // ESMC_State_H
