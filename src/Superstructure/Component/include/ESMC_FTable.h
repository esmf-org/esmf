// $Id: ESMC_FTable.h,v 1.1 2003/02/25 18:26:57 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMF FTable C++ declaration include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

#ifndef ESMC_FTable_H
#define ESMC_FTable_H

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_FTable - function and data pointer table.
//
// !DESCRIPTION:
//  List of descriptive strings and function/data pointers which can
//   be get and set by name.  Used to register and call functions by
//   string instead of making public symbols.  
// 
//
//-----------------------------------------------------------------------------
// 
// !USES:
//  This function does NOT inherit from the base class; it is embedded
//  in other classes which inherit from base.

// !PUBLIC TYPES:
 class ESMC_FTable;

// !PRIVATE TYPES:

 enum ftype { FVOID=1, FINT=2, F2INT=3 };
 struct funcinfo {
    char *funcname;
    void *funcptr;
    enum ftype ftype;
 };

 enum dtype { FVOIDPTR=1 };
 struct datainfo {
    char *dataname;
    void *dataptr;
    enum dtype dtype;
 };

 // class declaration type
 class ESMC_FTable {

   private:
    int funccount;
    int funcalloc;
    struct funcinfo funcs[];
    int datacount;
    int dataalloc;
    struct datainfo data[];

  public:

    int ESMC_FTableSetFuncPtr(void *, char *name, enum ftype ftype);
    int ESMC_FTableSetDataPtr(void *, char *name, enum dtype dtype);

    void *ESMC_FTableGetFuncPtr(char *name);
    void *ESMC_FTableGetDataPtr(char *name);

 // native C++ constructors/destructors
    ESMC_FTable(void);
    ~ESMC_FTable(void);
  
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
   int ESMC_FTableExtend(int nfuncp, int ndatap);
   int ESMC_FTableQuery(int *nfuncp, int *ndatap);

//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_FTable

// Create and Destroy are declared as class helper functions (not methods)
// since they create and destroy an ESMC_FTable object itself. E.g. if Create
// were a method, the ESMC_FTable object on whose behalf it was being invoked
// would need to already exist!  These functions are supersets of C++ new
// and delete; they perform allocation/deallocation specialized to
// an ESMC_FTable object.

 ESMC_FTable *ESMC_FTableCreate(int nfunc, int ndata, int *rc);
 int ESMC_FTableDestroy(ESMC_FTable *comp);

 #endif  // ESMC_FTable_H
