// $Id: ESMC_FTable.h,v 1.21.2.3 2009/01/21 21:25:24 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

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

#include "ESMC_VM.h"


// !PUBLIC TYPES:
 class ESMC_FTable;

// !PRIVATE TYPES:

// These are the types of functions which can be entered in the function
// table.  The types are the arguments that will be stored in the table
// and that the function will be called with.  There are a few generic 
// types (called with an int, 2 ints, etc) and then some very specific
// types used by the framework (e.g. a Gridded Component called with a list 
// of void's and a final int * for the return code.)
// The typedefs are to ease the declarations of the function entry point
// itself.
enum ftype { FT_VOID=1, FT_INT, FT_2INT, FT_INTP, FT_VOIDP, FT_VOIDPINTP,
              FT_INITFINAL, FT_RUN, FT_COMP1STAT, FT_COMP2STAT, FT_COMPSLIST };
typedef void (*VoidFunc)(void);
typedef void (*IntFunc)(int);
typedef void (*Int2Func)(int, int);
typedef void (*IntPtrFunc)(int *);
typedef void (*VoidPtrFunc)(void *);
typedef void (*VoidPtrIntPtrFunc)(void *, int *);
typedef void (*C1SFunc)(void *, void *, void *, int *);
typedef void (*C2SFunc)(void *, void *, void *, void *, int *);
typedef void (*CSLFunc)(void *, void *, void *, int *);

typedef void (*VoidFuncVM)(void *);
typedef void (*IntFuncVM)(int, void *);
typedef void (*Int2FuncVM)(int, int, void *);
typedef void (*IntPtrFuncVM)(int *, void *);
typedef void (*VoidPtrFuncVM)(void *, void *);
typedef void (*VoidPtrIntPtrFuncVM)(void *, int *, void *);
typedef void (*C1SFuncVM)(void *, void *, void *, int *, void *);
typedef void (*C2SFuncVM)(void *, void *, void *, void *, int *, void *);
typedef void (*CSLFuncVM)(void *, void *, void *, int *, void *);

class funcinfo {
 private:
   static const int numargs = 16;
 protected:
   char *funcname;
   void *funcptr;
   void *funcarg[numargs];
   enum ftype ftype;
 public:
   funcinfo() { funcname = NULL;
                funcptr = NULL;
                for (int i=0; i<numargs; i++) funcarg[i] = NULL; 
                ftype = FT_VOID; }
  ~funcinfo() { if (funcname != NULL) delete[] funcname; }
               
   // assignment and copy
   //funcinfo(const funcinfo& rhs) { }  // copy strings and args, not ptr
   //funcinfo& operator=(const funcinfo& rhs) { }
 friend class ESMC_FTable;
};

enum dtype { DT_VOIDP=1, DT_FORTRAN_UDT_POINTER };
class datainfo {
 private:
 protected:
   char *dataname;
   void *dataptr;
   enum dtype dtype;
 public:
   datainfo() { dataname = NULL; dataptr = NULL; dtype=DT_VOIDP; }
  ~datainfo() { if (dataname != NULL) delete[] dataname; 
                if (dtype == DT_FORTRAN_UDT_POINTER)
                  delete [] (char *)dataptr;} 
   //datainfo(const datainfo& rhs) { }  // copy strings and type, not ptr
   //datainfo& operator=(const datainfo& rhs) { }
 friend class ESMC_FTable;
};

 // class declaration type
class ESMC_FTable {

   private:
    int funccount;
    int funcalloc;
    funcinfo *funcs;
    int datacount;
    int dataalloc;
    datainfo *data;

   public:

    int ESMC_FTableSetFuncPtr(char *name, void *func, enum ftype ftype);
    int ESMC_FTableSetFuncPtr(char *name, void *func);
    int ESMC_FTableSetFuncPtr(char *name, void *func, void *arg1, int *arg2);
    int ESMC_FTableSetFuncPtr(char *name, void *func, enum ftype ftype, 
                                                  int acount, void **arglist);
    int ESMC_FTableSetFuncArgs(char *name, int acount, void **arglist);
    int ESMC_FTableSetDataPtr(char *name, void **data, enum dtype dtype);

    int ESMC_FTableGetFuncPtr(char *name, void **func, enum ftype *ftype);
    int ESMC_FTableGetDataPtr(char *name, void **data, enum dtype *dtype);

    int ESMC_FTableExtend(int nfuncp, int ndatap);
    int ESMC_FTableCallVFuncPtr(char *name, int *funcrc);
    int ESMC_FTableCallVFuncPtr(char *name, ESMCI::VM *vm, int *funcrc);
    
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

typedef struct{
  char name[160];         // trimmed type string
  ESMC_FTable *ftable;    // pointer to function table
  int esmfrc;             // return code of esmf call back method
  int userrc;             // return code of registered user method
} cargotype;

 #endif  // ESMC_FTable_H

