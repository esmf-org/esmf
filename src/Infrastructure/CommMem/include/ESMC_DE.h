// $Id: ESMC_DE.h,v 1.4 2003/03/10 03:22:59 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMF DE C++ declaration include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

 #ifndef ESMC_DE_H
 #define ESMC_DE_H

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.
// #include <ESMC_CommMem.h> 

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_DE - Decomposition element
//
// !DESCRIPTION:
//
// The code in this file defines the C++ DE members and declares method 
// signatures (prototypes).  The companion file ESMC\_DE.C contains
// the definitions (full code bodies) for the DE methods.
//
// ESMF abstraction of a process or thread
//
//-----------------------------------------------------------------------------
// 
// !USES:
 #include <ESMC_Base.h>  // all classes inherit from the ESMC Base class.
 #include <ESMC_PE.h> 
 //#include <ESMC_XXX.h>   // other dependent classes (subclasses, aggregates,
                        // composites, associates, friends)

enum ESMC_DEType_e {ESMC_PROCESS, ESMC_THREAD};

// !PUBLIC TYPES:
// class ESMC_DEConfig;
 class ESMC_DE;

// !PRIVATE TYPES:

 // class configuration type
// class ESMC_DEConfig {
//   private:
 //   < insert resource items here >
// };

 // class declaration type
 class ESMC_DE : public ESMC_Base {    // inherits from ESMC_Base class

   private:
 //  < insert class members here >  corresponds to type ESMF_DE members
 //                                 in F90 modules
     int esmfID;     // ESMF assigned ID
     int pID;        // processID (index)
     int tID;        // threadID (index)
     ESMC_DEType_e deType;  // process or thread
     bool process;   // true if DE is a process
     bool thread;    // true if DE is a thread
     ESMC_PE *PE;    // assigned PE from peList

// !PUBLIC MEMBER FUNCTIONS:
//
  public:
    int ESMC_DEInit(int esmfid, int pid, int tid, bool proc, bool thrd,
                    ESMC_PE *pe);

 // optional configuration methods
//    int ESMC_DEGetConfig(ESMC_DEConfig *config) const;
//    int ESMC_DESetConfig(const ESMC_DEConfig *config);

 // accessor methods for class members
//    int ESMC_DEGet<Value>(<value type> *value) const;
//    int ESMC_DESet<Value>(<value type>  value);
    int ESMC_DESetPE(ESMC_PE *pe);
    int ESMC_DESetESMFID(int esmfid);
    int ESMC_DEGetESMFID(int *esmfid) const;
    int ESMC_DEGetpID(int *pid) const;
    int ESMC_DESetType(ESMC_DEType_e detype);
    int ESMC_DEGetType(ESMC_DEType_e *detype) const;
    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_DEValidate(void) const;
    int ESMC_DEPrint(void) const;

 // native C++ constructors/destructors
	ESMC_DE(void);
	ESMC_DE(ESMC_DEType_e detype);
	~ESMC_DE(void);
  
 // < declare the rest of the public interface methods here >

    friend class ESMC_Comm;
    friend class ESMC_DELayout;
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_DE

 #endif  // ESMC_DE_H
