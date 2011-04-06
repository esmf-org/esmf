// $Id: ESMCI_ArrayBundle.h,v 1.23 2011/04/06 01:08:17 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2011, University Corporation for Atmospheric Research, 
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

#include "ESMCI_Base.h"       // Base is superclass to ArrayBundle
#include "ESMCI_VM.h"
#include "ESMCI_RHandle.h"
#include "ESMCI_Array.h"
#include "ESMCI_Container.h"

#include <string>
#include <vector>

//-------------------------------------------------------------------------

namespace ESMCI {

// classes and structs

class ArrayBundle;

// class definition
class ArrayBundle : public ESMC_Base {    // inherits from ESMC_Base class
  
  private:
    Container<std::string, Array *> arrayContainer;
    bool arrayCreator;
  
  public:
    // constructor and destructor
    ArrayBundle(){
      arrayContainer;
      arrayCreator = false;
    }
    ArrayBundle(int baseID):ESMC_Base(baseID){// prevent baseID counter incr.
      arrayContainer;
      arrayCreator = false;
    }
  private:
    ArrayBundle(Array **arrayList, int arrayCount, int *rc);
  public:
    ~ArrayBundle(){destruct(false);}
  private:
    int destruct(bool followCreator=true);
  public:
    // create() and destroy()
    static ArrayBundle *create(Array **arrayList, int arrayCount, int *rc);
    static int destroy(ArrayBundle **arraybundle);
    // add(), get(), remove(), set()
    void addArray(Array *array){
      arrayContainer.add(array->getName(), array);
    }
    Array *getArray(std::string arrayName){
      return arrayContainer.get(arrayName);
    }
    void getArrayVector(vector<Array *> &arrayVector)const{ 
      arrayContainer.getVector(arrayVector);
    }
    void getArrayNameVector(vector<string> &arrayNameVector)const{ 
      arrayContainer.getKeyVector(arrayNameVector);
    }
    int getArrayCount()         const {return arrayContainer.size();}
    char const *getName()       const {return ESMC_BaseGetName();}
    void removeArray(std::string arrayName){
      arrayContainer.remove(arrayName);
    }
    int setName(char *name){return ESMC_BaseSetName(name, "ArrayBundle");}
    // misc.
    int print() const;
    // serialize() and deserialize()
    int serialize(char *buffer,int *length,int *offset,
                  const ESMC_AttReconcileFlag &attreconflag,
                  const ESMC_InquireFlag &inquireflag) const;
    int deserialize(char *buffer,int *offset,
                    const ESMC_AttReconcileFlag &attreconflag);
    // comms
    static int haloStore(ArrayBundle *arraybundle, RouteHandle **routehandle,
      ESMC_HaloStartRegionFlag halostartregionflag=ESMF_REGION_EXCLUSIVE,
      InterfaceInt *haloLDepth=NULL, InterfaceInt *haloUDepth=NULL);
    static int halo(ArrayBundle *arraybundle,
      RouteHandle **routehandle, bool checkflag=false);
    static int haloRelease(RouteHandle *routehandle);
    static int redistStore(ArrayBundle *srcArraybundle,
      ArrayBundle *dstArraybundle, RouteHandle **routehandle,
      InterfaceInt *srcToDstTransposeMap,
      ESMC_TypeKind typekindFactor = ESMF_NOKIND, void *factor = NULL);
    static int redist(ArrayBundle *srcArraybundle,
      ArrayBundle *dstArraybundle, RouteHandle **routehandle,
      bool checkflag=false);
    static int redistRelease(RouteHandle *routehandle);
    static int sparseMatMulStore(ArrayBundle *srcArraybundle,
      ArrayBundle *dstArraybundle, RouteHandle **routehandle,
        vector<SparseMatrix> &sparseMatrix);
    static int sparseMatMul(ArrayBundle *srcArraybundle,
      ArrayBundle *dstArraybundle, RouteHandle **routehandle,
      ESMC_RegionFlag zeroflag=ESMF_REGION_TOTAL,
      bool checkflag=false, bool haloFlag=false);  
    static int sparseMatMulRelease(RouteHandle *routehandle);
          
};  // class ArrayBundle

} // namespace ESMCI

#endif  // ESMCI_ArrayBundle_H
