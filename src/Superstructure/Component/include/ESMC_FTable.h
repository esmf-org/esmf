// $Id: ESMC_FTable.h,v 1.4 2003/02/27 21:28:25 nscollins Exp $
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

 enum ftype { FT_VOID=1, FT_INT, FT_2INT, FT_INTP, FT_VOIDP, FT_VOIDPINTP };
 typedef int (*VoidFunc)(void);
 typedef int (*IntFunc)(int);
 typedef int (*Int2Func)(int, int);
 typedef int (*IntPtrFunc)(int *);
 typedef int (*VoidPtrFunc)(void *);
 typedef int (*VoidPtrIntPtrFunc)(void *, int *);
 struct funcinfo {
    char *funcname;
    void *funcptr;
    void *funcarg1;
    void *funcarg2;
    char funcblock[16];    // TODO: make this extensible
    enum ftype ftype;
 };

 enum dtype { DT_VOIDP=1 };
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
    struct funcinfo *funcs;
    int datacount;
    int dataalloc;
    struct datainfo *data;

  public:

    int ESMC_FTableSetFuncPtr(char *name, void *func, enum ftype ftype);
    int ESMC_FTableSetFuncPtr(char *name, void *func, enum ftype ftype, 
                                                      void *arg1, void *arg2);
    int ESMC_FTableSetDataPtr(char *name, void *data, enum dtype dtype);

    int ESMC_FTableGetFuncPtr(char *name, void **func, enum ftype *ftype);
    int ESMC_FTableGetDataPtr(char *name, void **data, enum dtype *dtype);

    int ESMC_FTableExtend(int nfuncp, int ndatap);
    int ESMC_FTableCallVFuncPtr(char *name, int *funcrc);

    int ESMC_FTableValidate(const char*) const;
    int ESMC_FTablePrint(const char*) const;

 // native C++ constructors/destructors
    ESMC_FTable(void);
    ~ESMC_FTable(void);
  
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
   //int ESMC_FTableExtend(int nfuncp, int ndatap);
   int ESMC_FTableQuery(int *nfuncp, int *ndatap);

//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_FTable


 #endif  // ESMC_FTable_H
