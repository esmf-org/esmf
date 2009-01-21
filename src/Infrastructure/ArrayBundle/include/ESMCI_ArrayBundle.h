// $Id: ESMCI_ArrayBundle.h,v 1.1.2.7 2009/01/21 21:25:19 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
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
    bool arrayCreator;
  
  public:
    // constructor and destructor
    ArrayBundle(){
      arrayList = NULL;
      arrayCount = 0;
      arrayCreator = false;
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
    // serialize() and deserialize()
    int serialize(char *buffer, int *length, int *offset) const;
    int deserialize(char *buffer, int *offset);
    // comms
    static int redistStore(ArrayBundle *srcArraybundle,
      ArrayBundle *dstArraybundle, ESMC_RouteHandle **routehandle,
      InterfaceInt *srcToDstTransposeMap,
      ESMC_TypeKind typekindFactor = ESMF_NOKIND, void *factor = NULL);
    static int redist(ArrayBundle *srcArraybundle,
      ArrayBundle *dstArraybundle, ESMC_RouteHandle **routehandle,
      ESMC_Logical checkflag=ESMF_FALSE);
    static int redistRelease(ESMC_RouteHandle *routehandle);
    static int sparseMatMulStore(ArrayBundle *srcArraybundle,
      ArrayBundle *dstArraybundle, ESMC_RouteHandle **routehandle,
      ESMC_TypeKind typekindFactors = ESMF_NOKIND, void *factorList = NULL,
      int factorListCount = 0, InterfaceInt *factorIndexList = NULL);
    static int sparseMatMul(ArrayBundle *srcArraybundle,
      ArrayBundle *dstArraybundle, ESMC_RouteHandle **routehandle,
      ESMC_RegionFlag zeroflag=ESMF_REGION_TOTAL,
      ESMC_Logical checkflag=ESMF_FALSE);
    static int sparseMatMulRelease(ESMC_RouteHandle *routehandle);
          
};  // class ArrayBundle

} // namespace ESMCI

#endif  // ESMCI_ArrayBundle_H
