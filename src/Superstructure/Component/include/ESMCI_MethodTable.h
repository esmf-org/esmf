// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research, 
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

#ifndef ESMCI_MethodTable_H
#define ESMCI_MethodTable_H

//-----------------------------------------------------------------------------
//BOPI
// !CLASS:  ESMCI::MethodTable - method table.
//
// !DESCRIPTION:
// 
//EOPI
//-----------------------------------------------------------------------------

#include <string>

namespace ESMCI {

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
    int execute(std::string labelArg, void *object, int *userRc,
      bool* existflag=NULL);
};

} // namespace ESMCI

#endif  // ESMCI_MethodTable_H
