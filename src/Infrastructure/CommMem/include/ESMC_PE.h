// $Id: ESMC_PE.h,v 1.2 2002/12/10 03:49:15 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMF PE C++ declaration include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

 #ifndef ESMC_PE_H
 #define ESMC_PE_H

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.
// #include <ESMC_CommMem.h> 

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_PE - one line general statement about this class
//
// !DESCRIPTION:
//
// The code in this file defines the C++ PE members and declares method 
// signatures (prototypes).  The companion file ESMC\_PE.C contains
// the definitions (full code bodies) for the PE methods.
//
// < insert a paragraph or two explaining what you'll find in this file >
//
//-----------------------------------------------------------------------------
// 
// !USES:
 #include <ESMC_Base.h>  // all classes inherit from the ESMC Base class.
 #include <ESMC_Machine.h>  
 //#include <ESMC_XXX.h>   // other dependent classes (subclasses, aggregates,
                        // composites, associates, friends)

// !PUBLIC TYPES:
// class ESMC_PEConfig;
 class ESMC_PE;

// !PRIVATE TYPES:

 // class configuration type
// class ESMC_PEConfig {
//   private:
 //   < insert resource items here >
// };

 // class declaration type
 class ESMC_PE : public ESMC_Base {    // inherits from ESMC_Base class

   private:
 //  < insert class members here >  corresponds to type ESMF_PE members
 //                                 in F90 modules
    int esmfID;    // ESMF assigned ID
    int cpuID;     // Hardware assigned processor ID
    int nodeID;    // hardware assigned associated node ID
    static int peCount;  // number of PEs instantiated

    ESMC_Machine *machine;  // interface to specific platform
    // neighbor connections ?  see ESMC_Layout.h

// !PUBLIC MEMBER FUNCTIONS:
//
  public:
 // the following method applies to a shallow class
    int ESMC_PEInit(void);
    int ESMC_PEInit(int esmfid, int cpuid, int nodeid);

 // optional configuration methods
//    int ESMC_PEGetConfig(ESMC_PEConfig *config) const;
//    int ESMC_PESetConfig(const ESMC_PEConfig *config);

 // accessor methods for class members
    int ESMC_PEGetEsmfID(int *id) const;
    int ESMC_PESetEsmfID(int  id);

    int ESMC_PEGetCpuID(int *id) const;
    int ESMC_PESetCpuID(int  id);

    int ESMC_PEGetNodeID(int *id) const;
    int ESMC_PESetNodeID(int  id);
    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_PEValidate(void) const;
    int ESMC_PEPrint(void) const;

 // native C++ constructors/destructors
	ESMC_PE(void);
	~ESMC_PE(void);
  
 // < declare the rest of the public interface methods here >

    // friend int ESMC_PEListPECompare(const void *pe1, const void *pe2);
    // friend class ESMC_PEList;  // TODO: ?? implement in ESMC_PEList
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_PE

 #endif  // ESMC_PE_H
