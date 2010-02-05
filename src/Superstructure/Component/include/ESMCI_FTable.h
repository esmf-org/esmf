// $Id: ESMCI_FTable.h,v 1.16.2.1 2010/02/05 20:03:54 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
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
    int  funcintarg;
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
  private: 
    int query(int *nfuncp, int *ndatap);
};

typedef struct{
  char name[160];     // trimmed type string
  FTable *ftable;     // pointer to function table
  int rcCount;        // number of return codes in esmfrc and userrc
  int *esmfrc;        // return codes of esmf call back method (all threads)
  int *userrc;        // return codes of registered user method (all threads)
}cargotype;



//==============================================================================
//==============================================================================
// MethodTable
//==============================================================================
//==============================================================================

class MethodTable;

class MethodElement{
  private:
    const std::string label;
    void *pointer;
    std::string name;
    std::string shobj;
    MethodElement *nextElement;
  public:
    // native C++ constructors/destructors
    MethodElement(void):label(""){
      pointer = NULL;
      name = std::string("");
      shobj = std::string("");
      nextElement = NULL;
    }
    MethodElement(std::string labelArg):label(labelArg){
      pointer = NULL;
      name = std::string("");
      shobj = std::string("");
      nextElement = NULL;
    }
    MethodElement(std::string labelArg, void *pointerArg):label(labelArg){
      pointer = pointerArg;
      name = std::string("");
      shobj = std::string("");
      nextElement = NULL;
    }
    MethodElement(std::string labelArg, std::string nameArg,
      std::string shobjArg):label(labelArg){
      pointer = NULL;
      name = nameArg;
      shobj = shobjArg;
      nextElement = NULL;
    }
    ~MethodElement(void){
      nextElement = NULL;
    }
    // other methods
    int print(void)const;
    int execute(void *object, int *userRc);
    int resolve(void);
  friend class MethodTable;
};


class MethodTable{
  private:
    MethodElement *table;
  public:
    // native C++ constructors/destructors
    MethodTable(void){
      table = NULL;
    }
    ~MethodTable(void){
      while (table){
        MethodElement *next = table->nextElement;   
        delete table;
        table = next;
      }
      table = NULL;
    }
    // other methods
    int print(void)const;
    int add(std::string labelArg, void *pointer);
    int add(std::string labelArg, std::string name, std::string sharedObj);
    int remove(std::string labelArg);
    int execute(std::string labelArg, void *object, int *userRc);
};

} // namespace ESMCI

#endif  // ESMCI_FTable_H
