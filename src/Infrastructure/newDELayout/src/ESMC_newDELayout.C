// $Id: ESMC_newDELayout.C,v 1.20 2004/04/26 14:35:10 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC DELayout method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ newDELayout methods declared
// in the companion file ESMC_newDELayout.h
//
//-----------------------------------------------------------------------------

// insert any higher level, 3rd party or system includes here
#include <iostream.h>  // cout
#include <stdio.h>
#include <string.h>
#include <ESMC_Start.h>
#include <ESMC_Base.h>  
#include <ESMC_VM.h>  

// associated class definition file
#include <ESMC_newDELayout.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_newDELayout.C,v 1.20 2004/04/26 14:35:10 theurich Exp $";
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// This section includes all the newDELayout routines
//
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_newDELayoutCreate
//
// !INTERFACE:
ESMC_newDELayout *ESMC_newDELayoutCreate(
//
// !RETURN VALUE:
//    ESMC_newDELayout * to newly allocated ESMC_newDELayout
//
// !ARGUMENTS:
//
  ESMC_VM &vm,              // reference to ESMC_VM object
  int *nDEs,                // number of DEs
  int ndim,                 // number of dimensions
  int *DEtoPET,             // DEtoPET list
  int len,                  // number of DEs in DEtoPET list
  ESMC_Logical *cyclic_opt, // cyclic boundary option
  int *rc){                 // return code
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------
  ESMC_newDELayout *layout;
  // deal with optional variables
  ESMC_Logical cyclic = ESMF_FALSE;
  if (cyclic_opt != ESMC_NULL_POINTER)
    cyclic = *cyclic_opt;

  // decide whether this is a 1D or an ND layout
  if (ndim==1){
    try {
      layout = new ESMC_newDELayout;
      *rc = layout->ESMC_newDELayoutConstruct1D(vm, *nDEs, DEtoPET, len,
        cyclic);
      return(layout);
    }
    catch (...) {
      // TODO:  call ESMF log/err handler
      cerr << "ESMC_newDELayoutCreate() memory allocation failed\n";
      *rc = ESMF_FAILURE;
      return(ESMC_NULL_POINTER);
    }
  }else{
    try {
      layout = new ESMC_newDELayout;
      *rc = layout->ESMC_newDELayoutConstructND(vm, nDEs, ndim, DEtoPET, len,
        cyclic);
      return(layout);
    }
    catch (...) {
      // TODO:  call ESMF log/err handler
      cerr << "ESMC_newDELayoutCreate1D() memory allocation failed\n";
      *rc = ESMF_FAILURE;
      return(ESMC_NULL_POINTER);
    }
  }
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_newDELayoutDestroy
//
// !INTERFACE:
int ESMC_newDELayoutDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  ESMC_newDELayout *layout){     // in - ESMC_newDELayout to destroy
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------
  if (layout != ESMC_NULL_POINTER) {
    layout->ESMC_newDELayoutDestruct();
    delete layout;
    layout = ESMC_NULL_POINTER;
    return(ESMF_SUCCESS);
  }else{
    return(ESMF_FAILURE);
  }
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_newDELayoutConstruct1D
//
// !INTERFACE:
int ESMC_newDELayout::ESMC_newDELayoutConstruct1D(ESMC_VM &vm, int nDEs,
  int *DEtoPET, int len, ESMC_Logical cyclic){
//
// !RETURN VALUE:
//    int error return code
//
//
// !DESCRIPTION:
//    Construct the internal information structure in a new ESMC\_newDELayout
//
//EOP
//-----------------------------------------------------------------------------
  myvm = &vm;                       // set the pointer onto this VM instance
  int npets =  vm.vmachine_npets(); // get number of PETs
  if (nDEs==0){
    // this will be a 1:1 Layout
    ndes = npets;                   // number of DEs to be the same as PETs
  }else{
    // number of DEs has been supplied
    ndes = nDEs;
  }
  des = new de_type[ndes];          // allocate as many DEs as there are PETs
  // uniquely label the DEs in the layout 
  for (int i=0; i<ndes; i++){
    des[i].deid = i;                // default is to use basis zero
  }
  // now define connectivity between the DEs
  if (ndes>1){
    for (int i=0; i<ndes; i++){
      if (i==0){
        if (cyclic==ESMF_TRUE){
          des[i].nconnect = 2;
          des[i].connect_de = new int[2];
          des[i].connect_w  = new int[2];
          des[i].connect_de[0] = ndes-1;
          des[i].connect_w[0] = ESMF_CWGHT_NORMAL;
          des[i].connect_de[1] = 1;
          des[i].connect_w[1] = ESMF_CWGHT_NORMAL;
        }else{
          des[i].nconnect = 1;
          des[i].connect_de = new int[1];
          des[i].connect_w  = new int[1];
          des[i].connect_de[0] = 1;
          des[i].connect_w[0] = ESMF_CWGHT_NORMAL;
        }
      }else if (i==ndes-1){
        if (cyclic==ESMF_TRUE){
          des[i].nconnect = 2;
          des[i].connect_de = new int[2];
          des[i].connect_w  = new int[2];
          des[i].connect_de[0] = i-1;
          des[i].connect_w[0] = ESMF_CWGHT_NORMAL;
          des[i].connect_de[1] = 0;
          des[i].connect_w[1] = ESMF_CWGHT_NORMAL;
        }else{
          des[i].nconnect = 1;
          des[i].connect_de = new int[1];
          des[i].connect_w  = new int[1];
          des[i].connect_de[0] = 0;
          des[i].connect_w[0] = ESMF_CWGHT_NORMAL;
        }
      }else{
        des[i].nconnect = 2;
        des[i].connect_de = new int[2];
        des[i].connect_w  = new int[2];
        des[i].connect_de[0] = i-1;
        des[i].connect_w[0] = ESMF_CWGHT_NORMAL;
        des[i].connect_de[1] = i+1;
        des[i].connect_w[1] = ESMF_CWGHT_NORMAL;
      }        
    }
  }
  // Setup the dimensionality and coordinates of this layout. This information
  // is only kept for external use!
  ndim = 1; // this is a 1D logical rectangular routine
  logRectFlag = ESMF_TRUE;
  dims = new int[ndim];
  dims[0] = ndes;
  for (int i=0; i<ndes; i++){
    des[i].coord = new int[ndim];
    des[i].coord[0] = i;
  }
  // DE-to-PET mapping
  if (len==ndes){
    // DEtoPET mapping has been provided externally
    for (int i=0; i<ndes; i++)
      des[i].petid = DEtoPET[i];   // copy the mapping
  }else{
    // Use the mapper algorithm to find good DE-to-PET mapping
    ESMC_newDELayoutFindDEtoPET(npets);
  }
  // Fill local part of layout object
  int mypet = vm.vmachine_mypet();    // get my PET id
  ESMC_newDELayoutFillLocal(mypet);
  // Now that the layout is pretty much set up it is time to go through once
  // more to set the correct pids to provide a means to identify the virtual
  // memory space in which the DEs operate.
  for (int i=0; i<ndes; i++)
    des[i].pid = vm.vmachine_pid(des[i].petid);
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_newDELayoutConstructND
//
// !INTERFACE:
int ESMC_newDELayout::ESMC_newDELayoutConstructND(ESMC_VM &vm, int *nDEs, 
  int nndim, int *DEtoPET, int len, ESMC_Logical cyclic){
//
// !RETURN VALUE:
//    int error return code
//
//
// !DESCRIPTION:
//    Construct the internal information structure in a new ESMC\_newDELayout
//
//EOP
//-----------------------------------------------------------------------------
  myvm = &vm;                       // set the pointer onto this VM instance
  int npets =  vm.vmachine_npets(); // get number of PETs
  ndim = nndim; // set the number of dimensions
  // this is a N-dim logical rectangular routine
  logRectFlag = ESMF_TRUE;
  dims = new int[ndim];
  for (int i=0; i<ndim; i++)
    dims[i] = nDEs[i];
  // determine how many DEs there are in this layout
  ndes = 1;
  for (int i=0; i<nndim; i++)
    ndes *= nDEs[i];
  des = new de_type[ndes];          // allocate as many DEs as there are PETs
  // uniquely label the DEs in the layout 
  for (int i=0; i<ndes; i++){
    des[i].deid = i;                // default is to use basis zero
  }
  // Setup the dimensionality and coordinates of this layout. This information
  // is only kept for external use!
  for (int i=0; i<ndes; i++){
    des[i].coord = new int[ndim];
  }
  for (int j=0; j<ndim; j++)
    des[0].coord[j] = 0;
  for (int i=1; i<ndes; i++){
    int carryover = 1;
    for (int j=0; j<ndim; j++){
      des[i].coord[j] = des[i-1].coord[j] + carryover;
      if (des[i].coord[j]==nDEs[j]){
        des[i].coord[j] = 0;
        carryover = 1;
      }else{
        carryover=0;
      }
    }
  }
  // TODO: define connectivity between the DEs
  // For now don't connect any of the DEs
  for (int i=0; i<ndes; i++){
    des[i].nconnect = 0;
  }
  // DE-to-PET mapping
  if (len==ndes){
    // DEtoPET mapping has been provided externally
    for (int i=0; i<ndes; i++)
      des[i].petid = DEtoPET[i];   // copy the mapping
  }else{
    // Use the mapper algorithm to find good DE-to-PET mapping
    ESMC_newDELayoutFindDEtoPET(npets);
  }
  // Fill local part of layout object
  int mypet = vm.vmachine_mypet();    // get my PET id
  ESMC_newDELayoutFillLocal(mypet);
  // Now that the layout is pretty much set up it is time to go through once
  // more to set the correct pids to provide a means to identify the virtual
  // memory space in which the DEs operate.
  for (int i=0; i<ndes; i++)
    des[i].pid = vm.vmachine_pid(des[i].petid);
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_newDELayoutDestruct
//
// !INTERFACE:
int ESMC_newDELayout::ESMC_newDELayoutDestruct(void){
//
// !RETURN VALUE:
//    int error return code
//
//
// !DESCRIPTION:
//    Destruct the internal information structure in a new ESMC\_newDELayout
//
//EOP
//-----------------------------------------------------------------------------
  for (int i=0; i<ndes; i++){
    delete [] des[i].connect_de;
    delete [] des[i].connect_w;
    delete [] des[i].coord;
  }
  delete [] des;
  delete [] mydes;
  if (logRectFlag == ESMF_TRUE)
    delete [] dims;
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_newDELayoutGet
//
// !INTERFACE:
int ESMC_newDELayout::ESMC_newDELayoutGet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  int  *nDEs,                 // out - Total number of DEs
  int  *ndim,                 // out - Number of dimensions in coordinate tuple
  int  *nmyDEs,               // out - number of DEs for my PET instance
  int  *myDEs,                // out - list DEs for my PET instance
  int  len_myDEs,             // in  - number of elements in myDEs list
  int *localDe,               // out - local DE id for 1-to-1 layouts
  ESMC_Logical *oneToOneFlag, // out - 1-to-1 layout flag
  ESMC_Logical *logRectFlag,  // out - logical rectangular layout flag
  int  *deCountPerDim,        // out - list of dimension sizes
  int  len_deCountPerDim){    // in  - number of elements in deCountPerDim list
//
// !DESCRIPTION:
//    Get information about a DELayout object
//
//EOP
//-----------------------------------------------------------------------------
  int i;
  if (nDEs != ESMC_NULL_POINTER)
    *nDEs = ndes;
  if (ndim != ESMC_NULL_POINTER)
    *ndim = this->ndim;
  if (nmyDEs != ESMC_NULL_POINTER)
    *nmyDEs = nmydes;
  if (len_myDEs >= nmydes)
    for (i=0; i<nmydes; i++)
      myDEs[i] = mydes[i];
  if (oneToOneFlag != ESMC_NULL_POINTER)
    *oneToOneFlag = this->oneToOneFlag;
  if (localDe != ESMC_NULL_POINTER)
    *localDe = mydes[0];
  if (logRectFlag != ESMC_NULL_POINTER)
    *logRectFlag = this->logRectFlag;
  if ((len_deCountPerDim >= this->ndim) && (this->logRectFlag == ESMF_TRUE)) {
    for (i=0; i<this->ndim; i++)
      deCountPerDim[i] = dims[i];
    for (i=this->ndim; i<len_deCountPerDim; i++)
      deCountPerDim[i] = 1;
  }
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_newDELayoutGetDE
//
// !INTERFACE:
int ESMC_newDELayout::ESMC_newDELayoutGetDE(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  int  DEid,              // in  - DE id of DE to be queried
  int  *DEcoord,          // out - DE's coordinate tuple
  int  len_coord,         // in  - dimensions in DEcoord
  int  *DEcde,            // out - DE's connection table
  int  len_cde,           // in  - dimensions in DEcde
  int  *DEcw,             // out - DE's connection weight table
  int  len_cw,            // in  - dimensions in DEcw
  int  *nDEc              // out - DE's number of connections
  ){              
//
// !DESCRIPTION:
//    Get information about a DELayout object
//
//EOP
//-----------------------------------------------------------------------------
  int i;
  if (len_coord >= ndim) {
    for (i=0; i<ndim; i++)
      DEcoord[i] = des[DEid].coord[i];
    for (i=ndim; i<len_coord; i++)
      DEcoord[i] = 0;
  }
  if (len_cde >= des[DEid].nconnect) {
    for (i=0; i<des[DEid].nconnect; i++)
      DEcde[i] = des[DEid].connect_de[i];
    for (i=des[DEid].nconnect; i<len_cde; i++)
      DEcde[i] = 0;
  }
  if (len_cw >= des[DEid].nconnect) {
    for (i=0; i<des[DEid].nconnect; i++)
      DEcw[i] = des[DEid].connect_w[i];
    for (i=des[DEid].nconnect; i<len_cw; i++)
      DEcw[i] = 0;
  }
  if (nDEc != ESMC_NULL_POINTER)
    *nDEc = des[DEid].nconnect;
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_newDELayoutGetDEMatch
//
// !INTERFACE:
int ESMC_newDELayout::ESMC_newDELayoutGetDEMatch(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  int DEid,                     // in  - DE id of DE to be queried
  ESMC_newDELayout &layoutMatch,// in  - layout to match against
  int *deMatchCount,            // out - number of matching DEs in layoutMatch
  int *deMatchList,             // out - list of matching DEs in layoutMatch
  int len_deMatchList           // in  - size of deMatchList
  ){              
//
// !DESCRIPTION:
//    Get information about a DELayout object
//
//EOP
//-----------------------------------------------------------------------------
  int *tempMatchList = new int[layoutMatch.ndes]; // maximum number of DEs
  int tempMatchCount = 0;
  int comparePID = des[DEid].pid;
  int j=0;
  for (int i=0; i<layoutMatch.ndes; i++)
    if (layoutMatch.des[i].pid == comparePID){
      tempMatchList[j] = i;
      ++j;
    }
  // now j is equal to the number of DEs in layoutMatch which operate in the 
  // same virtual memory space as "DEid" does in this layout.
  if (deMatchCount != ESMC_NULL_POINTER)
    *deMatchCount = j;
  if (len_deMatchList >= j)
    for (int i=0; i<j; i++)
      deMatchList[i] = tempMatchList[i];
  delete [] tempMatchList;
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_newDELayoutPrint
//
// !INTERFACE:
int ESMC_newDELayout::ESMC_newDELayoutPrint(){
//
// !RETURN VALUE:
//    int error return code
//
//
// !DESCRIPTION:
//    Print details of DELayout object 
//
//EOP
//-----------------------------------------------------------------------------
  // print info about the vmachine object
  printf("--- ESMC_newDELayoutPrint start ---\n");
  printf("myvm = %p\n", myvm);
  printf("ndes = %d\n", ndes);
  for (int i=0; i<ndes; i++){
    printf("  des[%d]: deid=%d, petid=%d, pid=%d, nconnect=%d\n", i, 
      des[i].deid, des[i].petid, des[i].pid, des[i].nconnect);
    for (int j=0; j<des[i].nconnect; j++)
      printf("      connect_de[%d]=%d, weight=%d\n", j, des[i].connect_de[j],
        des[i].connect_w[j]);
  }
  printf("nmydes=%d\n", nmydes);
  for (int i=0; i<nmydes; i++)
    printf("  mydes[%d]=%d\n", i, mydes[i]);
  printf("ndim = %d\n", ndim);
  for (int i=0; i<ndes; i++){
    printf("[%d]: ", i);
    int j;
    for (j=0; j<ndim-1; j++)
      printf("%d, ", des[i].coord[j]);
    printf("%d\n", des[i].coord[j]);
  }
  printf("--- ESMC_newDELayoutPrint end ---\n");
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_newDELayoutValidate
//
// !INTERFACE:
int ESMC_newDELayout::ESMC_newDELayoutValidate(){
//
// !RETURN VALUE:
//    int error return code
//
//
// !DESCRIPTION:
//    Validate details of DELayout object 
//
//EOP
//-----------------------------------------------------------------------------
  int rc;

  // validate info about the vmachine object
  //printf("--- ESMC_newDELayoutValidate start ---\n");
  //printf("myvm = %p\n", myvm);
  //printf("ndes = %d\n", ndes);
  //for (int i=0; i<ndes; i++){
  //  printf("  des[%d]: deid=%d, petid=%d, pid=%d, nconnect=%d\n", i, 
  //    des[i].deid, des[i].petid, des[i].pid, des[i].nconnect);
  //  for (int j=0; j<des[i].nconnect; j++)
  //    printf("      connect_de[%d]=%d, weight=%d\n", j, des[i].connect_de[j],
  //      des[i].connect_w[j]);
  //}
  //printf("nmydes=%d\n", nmydes);
  //for (int i=0; i<nmydes; i++)
  //  printf("  mydes[%d]=%d\n", i, mydes[i]);
  //printf("ndim = %d\n", ndim);
  //for (int i=0; i<ndes; i++){
  //  printf("[%d]: ", i);
  //  int j;
  //  for (j=0; j<ndim-1; j++)
  //    printf("%d, ", des[i].coord[j]);
  //  printf("%d\n", des[i].coord[j]);
  // }
  // printf("--- ESMC_newDELayoutValidate end ---\n");
 
  // for now, validate at least the base object
  rc = this->ESMC_Validate();

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_newDELayoutCopy
//
// !INTERFACE:
int ESMC_newDELayout::ESMC_newDELayoutCopy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  void **srcdata, // input array
  void **destdata,// output array
  int blen,       // size in bytes that need to be copied from src to dest
  int srcDE,      // input DE
  int destDE,     // output DE
  ESMC_Logical oneToOneFlag){   // indicator whether this Layout is 1-to-1
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------
  // local reference to VM
  ESMC_VM &vm = *myvm;
  int mypet = vm.vmachine_mypet();
  int srcpet = des[srcDE].petid;      // PETid where srcDE lives
  int destpet = des[destDE].petid;    // PETid where destDE lives
  if (srcpet==mypet && destpet==mypet){
    // srcDE and destDE are on my PET
    if (oneToOneFlag==ESMF_TRUE){
      memcpy(destdata, srcdata, blen);
    }else{
      int i, j;
      for (i=0; i<nmydes; i++)
        if (mydes[i]==srcDE) break;
      for (j=0; j<nmydes; j++)
        if (mydes[j]==destDE) break;
      // now i is this PETs srcDE index and j is this PETs destDE index
      memcpy(destdata[j], srcdata[i], blen);
    }
  }else if (srcpet==mypet){
    // srcDE is on my PET, but destDE is on another PET
    if (oneToOneFlag==ESMF_TRUE){
      vm.vmachine_send(srcdata, blen, destpet);
    }else{
      int i;
      for (i=0; i<nmydes; i++)
        if (mydes[i]==srcDE) break;
      vm.vmachine_send(srcdata[i], blen, destpet);
    }
  }else if (destpet==mypet){
    // srcDE is on my PET, but destDE is on another PET
    if (oneToOneFlag==ESMF_TRUE){
      vm.vmachine_recv(destdata, blen, srcpet);
    }else{
      int i;
      for (i=0; i<nmydes; i++)
        if (mydes[i]==destDE) break;
      vm.vmachine_recv(destdata[i], blen, srcpet);
    }
  }
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_newDELayoutCopy
//
// !INTERFACE:
int ESMC_newDELayout::ESMC_newDELayoutCopy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  void **srcdata,   // input array
  void **destdata,  // output array
  int len,          // size in elements that need to be copied from src to dest
  ESMC_DataKind dtk,// data type kind
  int srcDE,        // input DE
  int destDE,       // output DE
  ESMC_Logical oneToOneFlag){   // indicator whether this Layout is 1-to-1
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------
  int blen = len * ESMC_DataKindSize(dtk);
  return ESMC_newDELayoutCopy(srcdata, destdata, blen, srcDE, destDE, 
    oneToOneFlag);
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_newDELayoutCopyCopy
//
// !INTERFACE:
int ESMC_newDELayout::ESMC_newDELayoutCopyCopy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  void **srcData1,    // input array
  void **srcData2,    // input array
  void **dstData1,    // output array
  void **dstData2,    // output array
  int blen1,          // size in bytes to copy from srcData1 to dstData2
  int blen2,          // size in bytes to copy from srcData2 to dstData1
  int de1,            // de for data1
  int de2,            // de for data2
  ESMC_Logical oneToOneFlag){   // indicator whether this Layout is 1-to-1
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------
  if (de1<=de2){
    ESMC_newDELayoutCopy(srcData1, dstData2, blen1, de1, de2, oneToOneFlag);
    ESMC_newDELayoutCopy(srcData2, dstData1, blen2, de2, de1, oneToOneFlag);
  }else{
    ESMC_newDELayoutCopy(srcData2, dstData1, blen2, de2, de1, oneToOneFlag);
    ESMC_newDELayoutCopy(srcData1, dstData2, blen1, de1, de2, oneToOneFlag);
  }
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_newDELayoutCopyCopy
//
// !INTERFACE:
int ESMC_newDELayout::ESMC_newDELayoutCopyCopy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  void **srcData1,    // input array
  void **srcData2,    // input array
  void **dstData1,    // output array
  void **dstData2,    // output array
  int len1,           // size in elements to copy from srcData1 to dstData2
  int len2,           // size in elements to copy from srcData2 to dstData1
  ESMC_DataKind dtk1, // data type kind
  ESMC_DataKind dtk2, // data type kind
  int de1,            // de for data1
  int de2,            // de for data2
  ESMC_Logical oneToOneFlag){   // indicator whether this Layout is 1-to-1
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------
  int blen1 = len1 * ESMC_DataKindSize(dtk1);
  int blen2 = len2 * ESMC_DataKindSize(dtk2);
  return ESMC_newDELayoutCopyCopy(srcData1, srcData2, dstData1, dstData2, 
    blen1, blen2, de1, de2, oneToOneFlag);
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_newDELayoutBcast
//
// !INTERFACE:
int ESMC_newDELayout::ESMC_newDELayoutBcast(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  void **data,    // data 
  int blen,       // message size in bytes
  int rootDE,     // root DE
  ESMC_Logical oneToOneFlag){   // indicator whether this Layout is 1-to-1
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------
  // very crude implementation of a layout wide bcast
  for (int i=0; i<ndes; i++)
    ESMC_newDELayoutCopy(data, data, blen, rootDE, i, ESMF_TRUE);
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_newDELayoutBcast
//
// !INTERFACE:
int ESMC_newDELayout::ESMC_newDELayoutBcast(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  void **data,    // data 
  int len,       // message size in bytes
  ESMC_DataKind dtk,// data type kind
  int rootDE,     // root DE
  ESMC_Logical oneToOneFlag){   // indicator whether this Layout is 1-to-1
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------
  int blen = len * ESMC_DataKindSize(dtk);
  // very crude implementation of a layout wide bcast
  for (int i=0; i<ndes; i++)
    ESMC_newDELayoutCopy(data, data, blen, rootDE, i, oneToOneFlag);
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_newDELayoutScatter
//
// !INTERFACE:
int ESMC_newDELayout::ESMC_newDELayoutScatter(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  void **srcdata, // input array
  void **destdata,// output array
  int blen,       // message size in bytes
  int rootDE,     // root DE
  ESMC_Logical oneToOneFlag){   // indicator whether this Layout is 1-to-1
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------
  // local reference to VM
  ESMC_VM &vm = *myvm;
  // very crude implementation of a layout wide scatter
  int mypet = vm.vmachine_mypet();
  if (des[rootDE].petid==mypet){
    // my PET holds the rootDE
    if (oneToOneFlag==ESMF_TRUE){
      char *tempdata = (char *)srcdata;
      for (int i=0; i<ndes; i++){
        ESMC_newDELayoutCopy((void **)tempdata, destdata, blen, rootDE, i,
          ESMF_TRUE);
        tempdata += blen;
      }
    }else{
      int j;
      for (j=0; j<nmydes; j++)
        if (mydes[j]==rootDE) break;
      void *rootdata = srcdata[j]; // backup the correct start of rootDE's data
      char *tempdata = (char *)srcdata[j];
      for (int i=0; i<ndes; i++){
        ESMC_newDELayoutCopy(srcdata, destdata, blen, rootDE, i, ESMF_FALSE);
        tempdata += blen;
        srcdata[j] = tempdata;
      }
      srcdata[j] = rootdata;  // restore correct start of root's src data
    }
  }else{
    if (oneToOneFlag==ESMF_TRUE){
      for (int i=0; i<ndes; i++)
        ESMC_newDELayoutCopy(srcdata, destdata, blen, rootDE, i, ESMF_TRUE);
    }else{
      for (int i=0; i<ndes; i++)
        ESMC_newDELayoutCopy(srcdata, destdata, blen, rootDE, i, ESMF_FALSE);
    }
  }
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_newDELayoutScatter
//
// !INTERFACE:
int ESMC_newDELayout::ESMC_newDELayoutScatter(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  void **srcdata,   // input array
  void **destdata,  // output array
  int len,          // message size in elements
  ESMC_DataKind dtk,// data type kind
  int rootDE,       // root DE
  ESMC_Logical oneToOneFlag){   // indicator whether this Layout is 1-to-1
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------
  int blen = len * ESMC_DataKindSize(dtk);
  return ESMC_newDELayoutScatter(srcdata, destdata, blen, rootDE, oneToOneFlag);
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_newDELayoutGather
//
// !INTERFACE:
int ESMC_newDELayout::ESMC_newDELayoutGather(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  void **srcdata, // input array
  void **destdata,// output array
  int blen,       // message size in bytes
  int rootDE,     // root DE
  ESMC_Logical oneToOneFlag){   // indicator whether this Layout is 1-to-1
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------
  // local reference to VM
  ESMC_VM &vm = *myvm;
  // very crude implementation of a layout wide gather
  int mypet = vm.vmachine_mypet();
  if (des[rootDE].petid==mypet){
    // my PET holds the rootDE -> receive chunks from all other DEs
    if (oneToOneFlag==ESMF_TRUE){
      char *tempdata = (char *)destdata;
      for (int i=0; i<ndes; i++){
        ESMC_newDELayoutCopy(srcdata, (void **)tempdata, blen, i, rootDE,
          ESMF_TRUE);
        tempdata += blen;
      }
    }else{
      int j;
      for (j=0; j<nmydes; j++)
        if (mydes[j]==rootDE) break;
      void *rootdata = destdata[j]; // backup the correct start of rootDE's data
      char *tempdata = (char *)destdata[j];
      for (int i=0; i<ndes; i++){
        ESMC_newDELayoutCopy(srcdata, destdata, blen, i, rootDE, ESMF_FALSE);
        tempdata += blen;
        destdata[j] = tempdata;
      }
      destdata[j] = rootdata;  // restore correct start of root's destdata
    }
  }else{
    if (oneToOneFlag==ESMF_TRUE){
      for (int i=0; i<ndes; i++)
        ESMC_newDELayoutCopy(srcdata, destdata, blen, i, rootDE, ESMF_TRUE);
    }else{
      for (int i=0; i<ndes; i++)
        ESMC_newDELayoutCopy(srcdata, destdata, blen, i, rootDE, ESMF_FALSE);
    }
  }
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_newDELayoutGather
//
// !INTERFACE:
int ESMC_newDELayout::ESMC_newDELayoutGather(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  void **srcdata,   // input array
  void **destdata,  // output array
  int len,          // message size in bytes
  ESMC_DataKind dtk,// data type kind
  int rootDE,       // root DE
  ESMC_Logical oneToOneFlag){   // indicator whether this Layout is 1-to-1
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------
  int blen = len * ESMC_DataKindSize(dtk);
  return ESMC_newDELayoutGather(srcdata, destdata, blen, rootDE, oneToOneFlag);
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_newDELayoutAllGlobalReduce
//
// !INTERFACE:
int ESMC_newDELayout::ESMC_newDELayoutAllGlobalReduce(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  void **srcdata,     // input array
  void *result,       // result
  int len,            // number of elements in each DE
  ESMC_DataKind dtk,  // data type kind
  ESMC_newOp op,      // reduction operation
  ESMC_Logical oneToOneFlag){   // indicator whether this Layout is 1-to-1
//
// !DESCRIPTION:
//    Reduce data into a single value
//
//EOP
//-----------------------------------------------------------------------------
  // local reference to VM
  ESMC_VM &vm = *myvm;
  // very crude implementation of a layout wide GlobalReduce
  int mypet = vm.vmachine_mypet();
  void *localresult;
  int local_i4;
  float local_r4;
  double local_r8;
  // first reduce across all of this PET's DEs
  switch (op){
  case ESMF_newSUM:
    switch (dtk){
    case ESMF_I4:
      {
        localresult = (void *)&local_i4;
        local_i4 = 0;
        if (oneToOneFlag==ESMF_TRUE){
          int *tempdata = (int *)srcdata;
          for (int j=0; j<len; j++){
            // looping over all the elements in this DE
            local_i4 += tempdata[j];
          }
        }else{
          for (int i=0; i<nmydes; i++){
            // looping over all of my DEs
            int *tempdata = (int *)srcdata[i];
            for (int j=0; j<len; j++){
              // looping over all the elements in this DE
              local_i4 += tempdata[j];
            }
          }
        }
      }
      break;
    case ESMF_R4:
      {
        localresult = (void *)&local_r4;
        local_r4 = 0.;
        if (oneToOneFlag==ESMF_TRUE){
          float *tempdata = (float *)srcdata;
          for (int j=0; j<len; j++){
            // looping over all the elements in this DE
            local_r4 += tempdata[j];
          }
        }else{
          for (int i=0; i<nmydes; i++){
            // looping over all of my DEs
            float *tempdata = (float *)srcdata[i];
            for (int j=0; j<len; j++){
              // looping over all the elements in this DE
              local_r4 += tempdata[j];
            }
          }
        }
      }
      break;
    case ESMF_R8:
      {
        localresult = (void *)&local_r8;
        local_r8 = 0.;
        if (oneToOneFlag==ESMF_TRUE){
          double *tempdata = (double *)srcdata;
          for (int j=0; j<len; j++){
            // looping over all the elements in this DE
            local_r8 += tempdata[j];
          }
        }else{
          for (int i=0; i<nmydes; i++){
            // looping over all of my DEs
            double *tempdata = (double *)srcdata[i];
            for (int j=0; j<len; j++){
              // looping over all the elements in this DE
              local_r8 += tempdata[j];
            }
          }
        }
      }
      break;
    }
    break;
  case ESMF_newMIN:
    printf("Reduce operation ESMF_newMIN is not yet implemented\n");
    break;
  case ESMF_newMAX:
    printf("Reduce operation ESMF_newMIN is not yet implemented\n");
    break;
  }
  // now each PET holds the reduced result for all its DEs
  // next is to allreduce this result across the entire VM
  vmType vmt = vmI4;
  switch (dtk){
  case ESMF_I4:
    vmt = vmI4;
    break;
  case ESMF_R4:
    vmt = vmR4;
    break;
  case ESMF_R8:
    vmt = vmR8;
    break;
  }
  vm.vmachine_allreduce(localresult, result, 1, vmt, (vmOp)op);
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_newDELayoutFindDEtoPET
//
// !INTERFACE:
int ESMC_newDELayout::ESMC_newDELayoutFindDEtoPET(int npets){
//
// !RETURN VALUE:
//    int error return code
//
//
// !DESCRIPTION:
//    Find best DE-to-PET mapping for the layout
//
//EOP
//-----------------------------------------------------------------------------
// TODO: Use the connectivity info to find best DE-to-PET mapping!
// For now I simply connect in sequence...
  if (ndes==npets){
    // 1:1 layout
    oneToOneFlag = ESMF_TRUE;
    for (int i=0; i<ndes; i++)
      des[i].petid = i;   // default 1:1 DE-to-PET mapping
  }else{
    // not a 1:1 layout
    oneToOneFlag = ESMF_FALSE;
    // first find how many DEs will be placed onto a certain PET
    int *ndepet = new int[npets];
    for (int i=0; i<npets; i++)
      ndepet[i] = 0;
    int i = 0;
    for (int j=0; j<ndes; j++){
      ++ndepet[i];
      ++i;
      if (i>=npets) i=0;
    }
    // now associate the DEs with their PETs
    i=0;
    int k=0;
    for (int j=0; j<ndes; j++){
      des[j].petid = i;
      ++k;
      if (k>=ndepet[i]){
        k=0;
        ++i;
      }
    }
    delete [] ndepet;
  }
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_newDELayoutFillLocal
//
// !INTERFACE:
int ESMC_newDELayout::ESMC_newDELayoutFillLocal(int mypet){
//
// !RETURN VALUE:
//    int error return code
//
//
// !DESCRIPTION:
//    Fill local part of layout object
//
//EOP
//-----------------------------------------------------------------------------
  nmydes = 0;               // reset local de count
  for (int i=0; i<ndes; i++)
    if (des[i].petid == mypet) ++nmydes;
  mydes = new int[nmydes];  // allocate space to hold ids of all of my DEs
  int j=0;
  for (int i=0; i<ndes; i++)
    if (des[i].petid == mypet){
      mydes[j]=i;
      ++j;
    }
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_newDELayoutDataCreate
//
// !INTERFACE:
void **ESMC_newDELayoutDataCreate(
//
// !RETURN VALUE:
//    void* to newly allocated memory
//
// !ARGUMENTS:
//
  int n,                    // number of DE data on this PET
  int *rc){                 // return code
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------
  void **array;
  try {
    array = new void*[n];
    *rc = ESMF_SUCCESS;
    return(array);
  }
  catch (...) {
    // TODO:  call ESMF log/err handler
    cerr << "ESMC_newDELayoutDataCreate() memory allocation failed\n";
    *rc = ESMF_FAILURE;
    return(ESMC_NULL_POINTER);
  }
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_newDELayoutDataAdd
//
// !INTERFACE:
int ESMC_newDELayoutDataAdd(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  void **ptr,               // in - DELayout data reference array
  void *a,                  // in - DELayout data reference to be added
  int index){               // in - DELayout data reference index
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------
  ptr[index] = a;
  return(ESMF_SUCCESS);
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_newDELayoutDataDestroy
//
// !INTERFACE:
int ESMC_newDELayoutDataDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  void **ptr){              // in - DELayout data references to destroy
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------
  delete [] ptr;  
  return(ESMF_SUCCESS);
}
//-----------------------------------------------------------------------------
