// $Id: ESMC_RTable.h,v 1.3 2003/03/11 14:19:48 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMF RTable C++ declaration include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

 #ifndef ESMC_RTable_H
 #define ESMC_RTable_H

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.
 #include <ESMC_XPacket.h>

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_RTable - Route table contains lists of data exchanges to make
//
// !DESCRIPTION:
//
// The code in this file defines the C++ RTable members and declares method 
// signatures (prototypes).  The companion file ESMC_RTable.C contains
// the definitions (full code bodies) for the RTable methods.
//
// 
//
//-----------------------------------------------------------------------------
// 
// !USES:
 #include <ESMC_Base.h>  // all classes inherit from the ESMC Base class.
 #include <ESMC_XPacket.h> 

// !PUBLIC TYPES:
 class ESMC_RTableConfig;
 class ESMC_RTable;

// !PRIVATE TYPES:

 // class configuration type
 class ESMC_RTableConfig {
   private:
 //   < insert resource items here >
 };

 // class declaration type
 class ESMC_RTable : public ESMC_Base {    // inherits from ESMC_Base class

   private:
     int entrycount;
     int my_peid;
     struct rtableentry {
        int dest_peid;
        int xpcount;
        ESMC_XPacket *xp;
     } *entry;
 
// !PUBLIC MEMBER FUNCTIONS:
//

  public:
 // the following methods apply to deep classes only
 // ESMC_RTableCreate and ESMC_RTableDestroy are declared below,
 // outside the ESMC_RTable declaration
    int ESMC_RTableConstruct(int pecount);   // internal only, deep class
    int ESMC_RTableDestruct(void);           // internal only, deep class

 // optional configuration methods
    int ESMC_RTableGetConfig(ESMC_RTableConfig *config) const;
    int ESMC_RTableSetConfig(const ESMC_RTableConfig *config);

 // accessor methods for class members
    //int ESMC_RTableGet<Value>(<value type> *value) const;
    //int ESMC_RTableSet<Value>(<value type>  value);
    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_RTableValidate(const char *options) const;
    int ESMC_RTablePrint(const char *options) const;

 // native C++ constructors/destructors
	ESMC_RTable(int pecount);
	~ESMC_RTable(void);
  
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 

//
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_RTable

// Create and Destroy are declared as class helper functions (not methods)
// since they create and destroy an ESMC_RTable object itself. E.g. if Create
// were a method, the ESMC_RTable object on whose behalf it was being invoked
// would need to already exist!  These functions are supersets of C++ new
// and delete; they perform allocation/deallocation specialized to
// an ESMC_RTable object.

 ESMC_RTable *ESMC_RTableCreate(int pecount, int *rc);// interface only, deep class
 int ESMC_RTableDestroy(ESMC_RTable *rtable); // interface only, deep class

 #endif  // ESMC_RTable_H
