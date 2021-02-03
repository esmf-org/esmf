// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research, 
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
#include <map>

namespace ESMCI {

//==============================================================================
//==============================================================================
// MethodTable
//==============================================================================
//==============================================================================

class MethodTable;

class MethodElement{
  private:
    void *pointer;
    std::string name;
    std::string shobj;
  public:
    // native C++ constructors/destructors
    MethodElement(void){
      pointer = NULL;
      name = std::string("");
      shobj = std::string("");
    }
    MethodElement(void *pointerArg){
      pointer = pointerArg;
      name = std::string("");
      shobj = std::string("");
    }
    MethodElement(std::string nameArg, std::string shobjArg){
      pointer = NULL;
      name = nameArg;
      shobj = shobjArg;
    }
    // other methods
    int execute(void *object, int *userRc)const;
    int resolve(void);
  friend class MethodTable;
};

class MethodTable : std::map<std::string, MethodElement>{
  public:
    // other methods
    int add(std::string labelArg, void *pointer);
    int add(std::string labelArg, std::string name, std::string sharedObj);
    int addreplace(std::string labelArg, void *pointer);
    int addreplace(std::string labelArg, std::string name, std::string sharedObj);
    int remove(std::string labelArg);
    bool isPresent(std::string labelArg);
    int execute(std::string labelArg, void *object, int *userRc,
      bool* existflag=NULL);
};

} // namespace ESMCI

#endif  // ESMCI_MethodTable_H
