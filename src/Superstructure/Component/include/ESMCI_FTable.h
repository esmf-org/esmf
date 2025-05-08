// $Id$
//
// Earth System Modeling Framework
// Copyright (c) 2002-2025, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMCI_FTable_H
#define ESMCI_FTable_H

//-----------------------------------------------------------------------------
//BOPI
// !CLASS:  ESMCI::FTable - function and data pointer table.
//
// !DESCRIPTION:
//  List of descriptive strings and function/data pointers which can
//   be get and set by name.  Used to register and call functions by
//   string instead of making public symbols.  
// 
//EOPI
//-----------------------------------------------------------------------------

#include <string>

#include "ESMCI_VM.h"
#include "ESMCI_Comp.h"

namespace ESMCI {

// constants and enums

// These are the types of functions which can be entered in the function
// table.  The types are the arguments that will be stored in the table
// and that the function will be called with.  There are a few generic 
// types (called with an int, 2 ints, etc) and then some very specific
// types used by the framework (e.g. a Gridded Component called with a list 
// of void's and a final int * for the return code.)
// The typedefs are to ease the declarations of the function entry point
// itself.

enum dtype { DT_VOIDP=1, DT_FORTRAN_UDT_POINTER };
enum ftype { FT_NULL=1, FT_VOIDP1INTP, FT_VOIDP4INTP };

typedef void (*VoidFunc)(void);
typedef void (*VoidP1IntPFunc)(void *, int *);
typedef void (*VoidP4IntPFunc)(void *, void *, void *, void *, int *);


// classes
class FTable;

// class definition
class funcinfo {
  private:
    static const int numargs = 16;
  protected:
    char *funcname;
    void *funcptr;
    void *funcarg[numargs];
    enum ftype ftype;
  public:
    funcinfo(){
      funcname = NULL;
      funcptr = NULL;
      for (int i=0; i<numargs; i++)
        funcarg[i] = NULL; 
      ftype = FT_NULL;
    }
    ~funcinfo(){
      if (funcname != NULL) 
        delete[] funcname;
    }
               
  friend class FTable;
};

class datainfo {
  private:
  protected:
    char *dataname;
    void *dataptr;
    enum dtype dtype;
  public:
    datainfo(){
      dataname = NULL; 
      dataptr = NULL;
      dtype=DT_VOIDP;
    }
    ~datainfo(){
      if (dataname != NULL) 
        delete[] dataname; 
      if (dtype == DT_FORTRAN_UDT_POINTER)
        delete [] (char *)dataptr;
    } 
  friend class FTable;
};

class FTable {
  private:
    int funccount;
    int funcalloc;
    funcinfo *funcs;
    int datacount;
    int dataalloc;
    datainfo *data;
  public:
    int componentcount;
    Comp *component;
  public:
    // data
    static void getDP(FTable ***ptr, void **datap, int *rc);
    static void setDP(FTable ***ptr, void **datap, int *rc);
    int getDataPtr(char const *name, void **data, enum dtype *dtype);
    int setDataPtr(char const *name, void **data, enum dtype dtype);
    int getDataPtrCount(int *count, int *maxLen);
    int getDataPtrList(char *labelList);
    // func
    static void setServices(void *ptr, void (*func)(), int *userRc, int *rc);
    static void setVM(void *ptr, void (*func)(), int *userRc, int *rc);
    int getEntry(char const *name, int *rc);
    int setFuncPtr(char const *name, void *func, enum ftype ftype);
    int setFuncPtr(char const *name, void *func);
    int setFuncPtr(char const *name, void *func, void *arg);
    int setFuncArgs(char const *name, int acount, void **arglist);
    int callVFuncPtr(char const *name, ESMCI::VM *vm, int *funcrc);
    int extend(int nfuncp, int ndatap);
    int validate(const char*) const;
    int print(const char*) const;
    // native C++ constructors/destructors
    FTable(void);
    ~FTable(void);
    static void newtrim(char const *oldc, int clen, int *phase, int *nstate,
      char **newc);
    static char const *methodString(enum ESMCI::method method);
    static enum method methodFromString(char const *methodString);
    enum method methodFromIndex(int i);
  private: 
    int query(int *nfuncp, int *ndatap);
};

typedef struct{
  char name[160];       // trimmed type string
  Comp *f90comp;        // pointer to Fortran component object
  FTable *ftable;       // pointer to function table
  int rcCount;          // number of return codes in esmfrc and userrc
  int *esmfrc;          // return codes of esmf call back method (all threads)
  int *userrc;          // return codes of registered user method (all threads)
  void *previousCargo;  // support for recursive entering of methods
  int previousParentFlag; // support for recursive entering of methods
  enum method currentMethod;
  int currentPhase;
  int timeout;          // timeout in seconds
}cargotype;

  
} // namespace ESMCI

#endif  // ESMCI_FTable_H
