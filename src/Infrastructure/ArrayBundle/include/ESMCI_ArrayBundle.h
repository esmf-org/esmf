// $Id: ESMCI_ArrayBundle.h,v 1.1.2.1 2008/04/24 00:15:52 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMCI_ArrayBundle_H
#define ESMCI_ArrayBundle_H

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::ArrayBundle - ArrayBundle
//
// !DESCRIPTION:
//
// The code in this file defines the C++ {\tt ArrayBundle} members and method
// signatures (prototypes).  The companion file {\tt ESMCI\_ArrayBundle.C}
// contains the full code (bodies) for the {\tt ArrayBundle} methods.
//
//EOPI
//-------------------------------------------------------------------------

#include "ESMC_Base.h"      // Base is superclass to Array
#include "ESMC_VM.h"
#include "ESMC_RHandle.h"
#include "ESMCI_Array.h"

//-------------------------------------------------------------------------

namespace ESMCI {

// classes and structs

class ArrayBundle;

// class definition
class ArrayBundle : public ESMC_Base {    // inherits from ESMC_Base class
  
  private:
    Array **arrayList;
    int arrayCount;
  
  public:
    // constructor and destructor
    ArrayBundle(){
      arrayList = NULL;
      arrayCount = 0;
    }
  private:
    ArrayBundle(Array **arrayList, int arrayCount, int *rc);
  public:
    ~ArrayBundle();
    // create() and destroy()
    static ArrayBundle *create(Array **arrayList, int arrayCount, int *rc);
    static int destroy(ArrayBundle **arraybundle);
    // get() and set()
    Array **getArrayList()      const {return arrayList;}
    int getArrayCount()         const {return arrayCount;}
    const char *getName()       const {return ESMC_BaseGetName();}
    int setName(char *name){return ESMC_BaseSetName(name, "ArrayBundle");}
    // misc.
    int print() const;
          
};  // class ArrayBundle

} // namespace ESMCI

#endif  // ESMCI_ArrayBundle_H
