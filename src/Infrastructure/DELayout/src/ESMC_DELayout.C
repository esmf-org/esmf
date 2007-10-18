// $Id: ESMC_DELayout.C,v 1.42.2.3 2007/10/18 02:42:34 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMC_DELayout.C"
//==============================================================================
//
// ESMC DELayout method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ DELayout methods declared
// in the companion file ESMC_DELayout.h
//
//-----------------------------------------------------------------------------

// insert any higher level, 3rd party or system includes here
#include <stdio.h>
#include <string.h>
#include <ESMC_Start.h>
#include <ESMC_Base.h>  
#include <ESMC_VM.h>  

// associated class definition file
#include <ESMC_DELayout.h>

// LogErr headers
#include "ESMC_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"             // for LogErr
//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_DELayout.C,v 1.42.2.3 2007/10/18 02:42:34 cdeluca Exp $";
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// This section includes all the DELayout routines
//
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutCreate()"
//BOP
// !IROUTINE:  ESMC_DELayoutCreate
//
// !INTERFACE:
ESMC_DELayout *ESMC_DELayoutCreate(
//
// !RETURN VALUE:
//    ESMC_DELayout * to newly allocated ESMC_DELayout
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
  ESMC_DELayout *layout;
  // deal with optional variables
  ESMC_Logical cyclic = ESMF_FALSE;
  if (cyclic_opt != ESMC_NULL_POINTER)
    cyclic = *cyclic_opt;
  
  
  // CAUTION: todo: THIS IS A _NASTY_ HACK to make things happy on higher levels
  // that rely on DELayout to be _always 2D! Here I promote a 1D layout request
  // to 2D: N x 1. I write a message to LogErr to make people aware of this!!!
  if (ndim==0){
    // ESMC_LogDefault.ESMC_LogWrite("Promoting 1D DELayout to 2D",
    //   ESMC_LOG_WARN);
    ndim = 2;
    nDEs = new int[2];  // this will leave a memor leak, but hey, its a hack!
    nDEs[0] = vm.vmk_npets();
    nDEs[1] = 1;
  }
  if (ndim==1){
    // ESMC_LogDefault.ESMC_LogWrite("Promoting 1D DELayout to 2D",
    //  ESMC_LOG_WARN);
    ndim = 2;
    int firstDEdim = nDEs[0];
    nDEs = new int[2];  // this will leave a memor leak, but hey, its a hack!
    nDEs[0] = firstDEdim;
    nDEs[1] = 1;
  }
  

  // decide whether this is a 1D or an ND layout
  if (ndim==0){
    // special case of a 1D layout where deCount will equal petCount
    try {
      layout = new ESMC_DELayout;
      *rc = layout->ESMC_DELayoutConstruct1D(vm, 0, DEtoPET, len, cyclic);
      return(layout);
    }
    catch (...) {
      // LogErr catches the allocation error
      ESMC_LogDefault.ESMC_LogMsgAllocError("for new ESMC_DELayout.", rc);  
      return(ESMC_NULL_POINTER);
    }
  }else if(ndim==1){
    try {
      layout = new ESMC_DELayout;
      *rc = layout->ESMC_DELayoutConstruct1D(vm, *nDEs, DEtoPET, len, cyclic);
      return(layout);
    }
    catch (...) {
      // LogErr catches the allocation error
      ESMC_LogDefault.ESMC_LogMsgAllocError("for new ESMC_DELayout.", rc);  
      return(ESMC_NULL_POINTER);
    }
  }else{
    try {
      layout = new ESMC_DELayout;
      *rc = layout->ESMC_DELayoutConstructND(vm, nDEs, ndim, DEtoPET, len,
        cyclic);
      return(layout);
    }
    catch (...) {
      // LogErr catches the allocation error
      ESMC_LogDefault.ESMC_LogMsgAllocError("for new ESMC_DELayout.", rc);  
      return(ESMC_NULL_POINTER);
    }
  }
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutDestroy()"
//BOP
// !IROUTINE:  ESMC_DELayoutDestroy
//
// !INTERFACE:
int ESMC_DELayoutDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  ESMC_DELayout **layout){      // in - ESMC_DELayout to destroy
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------
  if (*layout != ESMC_NULL_POINTER) {
    (*layout)->ESMC_DELayoutDestruct();
    delete (*layout);
    *layout = ESMC_NULL_POINTER;
    return(ESMF_SUCCESS);
  }else{
    ESMC_LogDefault.ESMC_LogWrite("Cannot delete bad DELayout object.", 
      ESMC_LOG_ERROR);
    return(ESMF_FAILURE);
  }
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutConstruct1D()"
//BOP
// !IROUTINE:  ESMC_DELayoutConstruct1D
//
// !INTERFACE:
int ESMC_DELayout::ESMC_DELayoutConstruct1D(ESMC_VM &vm, int nDEs,
  int *DEtoPET, int len, ESMC_Logical cyclic){
//
// !RETURN VALUE:
//    int error return code
//
//
// !DESCRIPTION:
//    Construct the internal information structure in a new ESMC\_DELayout
//
//EOP
//-----------------------------------------------------------------------------
  myvm = &vm;                       // set the pointer onto this VM instance
  int npets =  vm.vmk_npets(); // get number of PETs
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
          des[i].connect_w[0] = ESMC_CWGHT_NORMAL;
          des[i].connect_de[1] = 1;
          des[i].connect_w[1] = ESMC_CWGHT_NORMAL;
        }else{
          des[i].nconnect = 1;
          des[i].connect_de = new int[1];
          des[i].connect_w  = new int[1];
          des[i].connect_de[0] = 1;
          des[i].connect_w[0] = ESMC_CWGHT_NORMAL;
        }
      }else if (i==ndes-1){
        if (cyclic==ESMF_TRUE){
          des[i].nconnect = 2;
          des[i].connect_de = new int[2];
          des[i].connect_w  = new int[2];
          des[i].connect_de[0] = i-1;
          des[i].connect_w[0] = ESMC_CWGHT_NORMAL;
          des[i].connect_de[1] = 0;
          des[i].connect_w[1] = ESMC_CWGHT_NORMAL;
        }else{
          des[i].nconnect = 1;
          des[i].connect_de = new int[1];
          des[i].connect_w  = new int[1];
          des[i].connect_de[0] = 0;
          des[i].connect_w[0] = ESMC_CWGHT_NORMAL;
        }
      }else{
        des[i].nconnect = 2;
        des[i].connect_de = new int[2];
        des[i].connect_w  = new int[2];
        des[i].connect_de[0] = i-1;
        des[i].connect_w[0] = ESMC_CWGHT_NORMAL;
        des[i].connect_de[1] = i+1;
        des[i].connect_w[1] = ESMC_CWGHT_NORMAL;
      }        
    }
  } else  {
     des[0].nconnect = 1;
     des[0].connect_de = new int[1];
     des[0].connect_w  = new int[1];
     des[0].connect_de[0] = 0;
     des[0].connect_w[0] = ESMC_CWGHT_NORMAL;
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
    ESMC_DELayoutFindDEtoPET(npets);
  }
  // Issue warning if this is not a 1:1 layout. Do this because higher levels
  // of ESMF are written with 1:1 in mind.
  // TODO: remove this warning once all of ESMF accepts the more general case
  // of multiple DEs per PET.
  if (oneToOneFlag == ESMF_FALSE){
    ESMC_LogDefault.ESMC_LogWrite("A layout without 1:1 DE:PET mapping was"
      " created! This may cause problems in higher layers of ESMF!", 
      ESMC_LOG_WARN);
  }
  // Issue warning if this is not logically rectangular
  // TODO: remove this warning when non logRect layouts o.k.
  if (logRectFlag == ESMF_FALSE){
    ESMC_LogDefault.ESMC_LogWrite("A non logRect layout was"
      " created! This may cause problems in higher layers of ESMF!", 
      ESMC_LOG_WARN);
  }
  // Fill local part of layout object
  int mypet = vm.vmk_mypet();    // get my PET id
  ESMC_DELayoutFillLocal(mypet);
  // Now that the layout is pretty much set up it is time to go through once
  // more to set the correct pids to provide a means to identify the virtual
  // memory space in which the DEs operate.
  for (int i=0; i<ndes; i++)
    des[i].pid = vm.vmk_pid(des[i].petid);
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutConstructND()"
//BOP
// !IROUTINE:  ESMC_DELayoutConstructND
//
// !INTERFACE:
int ESMC_DELayout::ESMC_DELayoutConstructND(ESMC_VM &vm, int *nDEs, 
  int nndim, int *DEtoPET, int len, ESMC_Logical cyclic){
//
// !RETURN VALUE:
//    int error return code
//
//
// !DESCRIPTION:
//    Construct the internal information structure in a new ESMC\_DELayout
//
//EOP
//-----------------------------------------------------------------------------
  myvm = &vm;                       // set the pointer onto this VM instance
  int npets =  vm.vmk_npets(); // get number of PETs
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
  // however even without real connections the connect_de and connect_w arrays
  // must be valid in order for the delete method to function correctly!
  for (int i=0; i<ndes; i++){
    des[i].nconnect = 0;
    des[i].connect_de = new int[1];
    des[i].connect_w  = new int[1];
  }
  // DE-to-PET mapping
  if (len==ndes){
    // DEtoPET mapping has been provided externally
    for (int i=0; i<ndes; i++)
      des[i].petid = DEtoPET[i];   // copy the mapping
    // nsc - if ndes is = npets, go ahead and set the 1:1 flag
    // even if the user supplied the mapping.   6dec04
    // TODO: gjt - this needs a bit more consideration than this, just 
    // ndes == npets alone does not indicate 1:1!
      if (ndes==npets) // 1:1 layout
        oneToOneFlag = ESMF_TRUE;
      else
        oneToOneFlag = ESMF_FALSE;  // if there are more or less DEs than PETs
  }else{
    // Use the mapper algorithm to find good DE-to-PET mapping
    ESMC_DELayoutFindDEtoPET(npets);
  }
  // Issue warning if this is not a 1:1 layout. Do this because higher levels
  // of ESMF are written with 1:1 in mind.
  // TODO: remove this warning once all of ESMF accepts the more general case
  // of multiple DEs per PET.
  if (oneToOneFlag == ESMF_FALSE){
    ESMC_LogDefault.ESMC_LogWrite("A layout without 1:1 DE:PET mapping was"
      " created! This may cause problems in higher layers of ESMF!", 
      ESMC_LOG_WARN);
  }
  // Issue warning if this is not logically rectangular
  // TODO: remove this warning when non logRect layouts o.k.
  if (logRectFlag == ESMF_FALSE){
    ESMC_LogDefault.ESMC_LogWrite("A non logRect layout was"
      " created! This may cause problems in higher layers of ESMF!", 
      ESMC_LOG_WARN);
  }
  // Fill local part of layout object
  int mypet = vm.vmk_mypet();    // get my PET id
  ESMC_DELayoutFillLocal(mypet);
  // Now that the layout is pretty much set up it is time to go through once
  // more to set the correct pids to provide a means to identify the virtual
  // memory space in which the DEs operate.
  for (int i=0; i<ndes; i++)
    des[i].pid = vm.vmk_pid(des[i].petid);
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutDestruct()"
//BOP
// !IROUTINE:  ESMC_DELayoutDestruct
//
// !INTERFACE:
int ESMC_DELayout::ESMC_DELayoutDestruct(void){
//
// !RETURN VALUE:
//    int error return code
//
//
// !DESCRIPTION:
//    Destruct the internal information structure in a new ESMC\_DELayout
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
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutGetVM()"
//BOP
// !IROUTINE:  ESMC_DELayoutGetVM
//
// !INTERFACE:
int ESMC_DELayout::ESMC_DELayoutGetVM(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  ESMC_VM **vm){              // out - VM this layout is defined on
//
// !DESCRIPTION:
//    Get VM of this DELayout object
//
//EOP
//-----------------------------------------------------------------------------
  *vm = myvm;
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutGet()"
//BOP
// !IROUTINE:  ESMC_DELayoutGet
//
// !INTERFACE:
int ESMC_DELayout::ESMC_DELayoutGet(
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
  if (localDe != ESMC_NULL_POINTER){
    if (this->nmydes >= 1)  // if there are at least 1 DE on this PET return 1st
      *localDe = mydes[0];
    else{
      *localDe = -1;    // mark invalid
      return ESMC_RC_CANNOT_GET;
    }
  }
  if (logRectFlag != ESMC_NULL_POINTER)
    *logRectFlag = this->logRectFlag;
  if (len_deCountPerDim >= this->ndim){
    if (this->logRectFlag == ESMF_TRUE){
      for (i=0; i<this->ndim; i++)
        deCountPerDim[i] = dims[i];
      for (i=this->ndim; i<len_deCountPerDim; i++)
        deCountPerDim[i] = 1;
    }else{
      for (i=0; i<len_deCountPerDim; i++)
        deCountPerDim[i] = -1;    // mark invalid
      return ESMC_RC_CANNOT_GET; 
    }
  }
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutGetDELocalInfo()"
//BOP
// !IROUTINE:  ESMC_DELayoutGetDELocalInfo
//
// !INTERFACE:
int ESMC_DELayout::ESMC_DELayoutGetDELocalInfo(
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
  int  *nDEc,             // out - DE's number of connections
  int  *pid               // out - pid for this DE
  ){              
//
// !DESCRIPTION:
//    Get information about a DELayout object
//
//EOP
//-----------------------------------------------------------------------------
  int i;
  if (DEid < 0 || DEid >= ndes){
    // DEid out of range
    return ESMC_RC_ARG_OUTOFRANGE;
  }
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
  if (pid != ESMC_NULL_POINTER)
    *pid = des[DEid].pid;
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutGetDEMatchDE()"
//BOP
// !IROUTINE:  ESMC_DELayoutGetDEMatchDE
//
// !INTERFACE:
int ESMC_DELayout::ESMC_DELayoutGetDEMatchDE(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  int DEid,                     // in  - DE id of DE to be queried
  ESMC_DELayout &layoutMatch,// in  - layout to match against
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
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutGetDEMatchPET()"
//BOP
// !IROUTINE:  ESMC_DELayoutGetDEMatchPET
//
// !INTERFACE:
int ESMC_DELayout::ESMC_DELayoutGetDEMatchPET(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  int DEid,                     // in  - DE id of DE to be matched
  ESMC_VM &vmMatch,             // in  - vm to match against
  int *petMatchCount,           // out - number of matching PETs in vmMatch
  int *petMatchList,            // out - list of matching PETs in vmMatch
  int len_petMatchList          // in  - size of petMatchList
  ){              
//
// !DESCRIPTION:
//    Match DEid in the current DELayout object against the PETs in the 
//    provided vmMatch VM. Return number of matched PETs and a list of the
//    matching pet id's that operate in the same virtual address space in which
//    DEid lies.
//
//EOP
//-----------------------------------------------------------------------------
  int npets = vmMatch.vmk_npets();  // maximum number of PETs in vmMatch
  int *tempMatchList = new int[npets];
  int tempMatchCount = 0;
  int comparePID = des[DEid].pid; // this is the virtual address space id
  int j=0;
  for (int i=0; i<npets; i++)
    if (vmMatch.vmk_pid(i) == comparePID){
      tempMatchList[j] = i;
      ++j;
    }
  // now j is equal to the number of PETs in vmMatch which operate in the 
  // same virtual address space as "DEid" does in this layout.
  if (petMatchCount != ESMC_NULL_POINTER)
    *petMatchCount = j;
  if (len_petMatchList >= j)
    for (int i=0; i<j; i++)
      petMatchList[i] = tempMatchList[i];
  delete [] tempMatchList;
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutPrint()"
//BOP
// !IROUTINE:  ESMC_DELayoutPrint
//
// !INTERFACE:
int ESMC_DELayout::ESMC_DELayoutPrint(){
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
  // print info about the ESMC_DELayout object
  printf("--- ESMC_DELayoutPrint start ---\n");
  printf("myvm = %p\n", myvm);
  printf("ndes = %d\n", ndes);
  for (int i=0; i<ndes; i++){
    printf("  des[%d]: de=%d, pet=%d, vas=%d, nconnect=%d\n", i, 
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
  printf("--- ESMC_DELayoutPrint end ---\n");
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutSerialize"
//BOPI
// !IROUTINE:  ESMC_DELayoutSerialize - Turn delayout information into a byte stream
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutSerialize(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      char *buffer,          // inout - byte stream to fill
      int *length,           // inout - buf length; realloc'd here if needed
      int *offset) {         // inout - original offset, updated to point 
                             //  to first free byte after current obj info
//
// !DESCRIPTION:
//    Turn info in delayout class into a stream of bytes.
//
//EOPI
//-----------------------------------------------------------------------------
    int fixedpart, nbytes, rc;
    int i, j;
    char *cp;
    int *ip;
    ESMC_Logical *lp;
    ESMC_VM **vp;
    de_type *dep;

    // TODO: we cannot reallocate from C++ if the original buffer is
    //  allocated on the f90 side.  change the code to make the allocate
    //  happen in C++; then this will be fine.  (for now make sure buffer
    //  is always big enough so realloc is not needed.)
    fixedpart = sizeof(ESMC_DELayout);
    if ((*length - *offset) < fixedpart) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, 
                             "Buffer too short to add a DELayout object", &rc);
        return ESMF_FAILURE; 
    }

    // fixedpart = sizeof(ESMC_DELayout);
    // if ((*length - *offset) < fixedpart) {
    //     buffer = (char *)realloc((void *)buffer, *length + 2*fixedpart);
    //     *length += 2 * fixedpart;
    //  }

    // first set the base part of the object
    rc = this->ESMC_Base::ESMC_Serialize(buffer, length, offset);

#if 0
/ DE type used internally in the ESMC_DELayout class
typedef struct{
  int deid;         // DE's external id number (in case not base zero)
  int petid;        // Id of the PET associated with this DE
  int pid;          // absolute process ID, specifying virtual memory space
  int nconnect;     // number of connections from this DE
  int *connect_de;  // connected DEs
  int *connect_w;   // connection weight
  int *coord;       // coordinates of this DE in the layout
}de_type;

// class definition
class ESMC_DELayout : public ESMC_Base {    // inherits from ESMC_Base class
    ESMC_VM *myvm;  // ptr to this PET's VM instance this layout is running on
    int ndes;       // number of DEs
    de_type *des;   // list that holds all of this layout's DE info
    int nmydes;     // number of DEs associated with instantiating PET
    int *mydes;     // list that holds all of the des indices for this instance
    int ndim;       // dimensionality of this layout
    ESMC_Logical oneToOneFlag;  // indicate whether this is a 1-to-1 layout
    ESMC_Logical logRectFlag;   // indicate whether this is logical rectangular
    int *dims;      // sizes of dimensions in a logical rectangular layout
#endif

    cp = (char *)(buffer + *offset);
    
    // TODO: for now, send NULL as the vm, because i do not know how to
    // serialize a VM.   probably sending an integer VM ID number would be
    // what we want in the long run.
    vp = (ESMC_VM **)cp;   
    *vp++ = NULL;     

    ip = (int *)vp;
    *ip++ = ndes;
    // ndim must be available before decoding the next loop, so it has
    // to be sent now.
    *ip++ = ndim;

    for (i=0, dep=des; i<ndes; i++, dep++) {
        *ip++ = dep->deid;
        *ip++ = dep->petid;
        *ip++ = dep->pid;
        *ip++ = dep->nconnect;
        for (j=0; j<dep->nconnect; j++) {
            *ip++ = dep->connect_de[j];
            *ip++ = dep->connect_w[j];
        }
        for (j=0; j<ndim; j++) 
            *ip++ = dep->coord[j];
    }
  
    *ip++ = nmydes;
    for (i=0; i<nmydes; i++) 
        *ip++ = mydes[i];

    // this has to come before dims, since they are not allocated unless
    // logRectFlag is true.
    lp = (ESMC_Logical *)ip;
    *lp++ = oneToOneFlag;
    *lp++ = logRectFlag;
    
    ip = (int *)lp;
    if (logRectFlag == ESMF_TRUE)
        for (i=0; i<ndim; i++) 
            *ip++ = dims[i];

    cp = (char *)ip;

    *offset = (cp - buffer);
   
    return ESMF_SUCCESS;

 } // end ESMC_Serialize
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutDeserialize"
//BOPI
// !IROUTINE:  ESMC_DELayoutDeserialize - Turn a byte stream into an object
//
// !INTERFACE:
      ESMC_DELayout *ESMC_DELayoutDeserialize(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      char *buffer,          // in - byte stream to read
      int *offset) {         // inout - original offset, updated to point 
                             //  to first free byte after current obj info
//
// !DESCRIPTION:
//    Turn a stream of bytes into an object.
//
//EOPI
//-----------------------------------------------------------------------------
    ESMC_DELayout *a = new ESMC_DELayout;
    int fixedpart, nbytes, rc;
    int i, j;
    char *cp;
    int *ip;
    ESMC_Logical *lp;
    ESMC_VM **vp;
    de_type *dep;

    // first get the base part of the object
    rc = a->ESMC_Base::ESMC_Deserialize(buffer, offset);

    // now the rest
    cp = (char *)(buffer + *offset);
    
    vp = (ESMC_VM **)cp;
    a->myvm = *vp++; 

    ip = (int *)vp;
    a->ndes = *ip++;

    // ndim must be known before this loop.
    a->ndim = *ip++;
    a->des = new de_type[a->ndes];
    for (i=0, dep=a->des; i<a->ndes; i++, dep++) {
        dep->deid = *ip++;
        dep->petid = *ip++;
        dep->pid = *ip++;
        dep->nconnect = *ip++;

        dep->connect_de = new int[dep->nconnect];
        dep->connect_w = new int[dep->nconnect];
        dep->coord = new int[a->ndim];
        for (j=0; j<dep->nconnect; j++) {
            dep->connect_de[j] = *ip++;
            dep->connect_w[j] = *ip++;
        }
        for (j=0; j<a->ndim; j++) 
            dep->coord[j] = *ip++;
    }
  
    a->nmydes = *ip++;
    a->mydes = new int[a->nmydes];
    for (i=0; i<a->nmydes; i++) 
        a->mydes[i] = *ip++;
  
    // decode flags first, because dims is not sent unless logRectFlag is true.
    lp = (ESMC_Logical *)ip;
    a->oneToOneFlag = *lp++;
    a->logRectFlag = *lp++;

    ip = (int *)lp;
    if (a->logRectFlag == ESMF_TRUE) {
        a->dims = new int[a->ndim];
        for (i=0; i<a->ndim; i++) 
            a->dims[i] = *ip++;
    } else
        a->dims = NULL;

    cp = (char *)ip;

    *offset = (cp - buffer);
   
    return a;

 } // end ESMC_DELayoutDeserialize
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutValidate()"
//BOP
// !IROUTINE:  ESMC_DELayoutValidate
//
// !INTERFACE:
int ESMC_DELayout::ESMC_DELayoutValidate(){
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
  int rc = ESMF_SUCCESS;

  // validate info about the ESMC_DELayout object
  //printf("--- ESMC_DELayoutValidate start ---\n");
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
  // printf("--- ESMC_DELayoutValidate end ---\n");
 
  // for now, validate at least the base object
    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }
  // rc = this->ESMC_Validate();

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutCopy()"
//BOP
// !IROUTINE:  ESMC_DELayoutCopy
//
// !INTERFACE:
int ESMC_DELayout::ESMC_DELayoutCopy(
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
  int mypet = vm.vmk_mypet();
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
      vm.vmk_send(srcdata, blen, destpet);
    }else{
      int i;
      for (i=0; i<nmydes; i++)
        if (mydes[i]==srcDE) break;
      vm.vmk_send(srcdata[i], blen, destpet);
    }
  }else if (destpet==mypet){
    // srcDE is on my PET, but destDE is on another PET
    if (oneToOneFlag==ESMF_TRUE){
      vm.vmk_recv(destdata, blen, srcpet);
    }else{
      int i;
      for (i=0; i<nmydes; i++)
        if (mydes[i]==destDE) break;
      vm.vmk_recv(destdata[i], blen, srcpet);
    }
  }
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutCopy()"
//BOP
// !IROUTINE:  ESMC_DELayoutCopy
//
// !INTERFACE:
int ESMC_DELayout::ESMC_DELayoutCopy(
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
  return ESMC_DELayoutCopy(srcdata, destdata, blen, srcDE, destDE, 
    oneToOneFlag);
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutExchange()"
//BOP
// !IROUTINE:  ESMC_DELayoutExchange
//
// !INTERFACE:
int ESMC_DELayout::ESMC_DELayoutExchange(
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
    ESMC_DELayoutCopy(srcData1, dstData2, blen1, de1, de2, oneToOneFlag);
    ESMC_DELayoutCopy(srcData2, dstData1, blen2, de2, de1, oneToOneFlag);
  }else{
    ESMC_DELayoutCopy(srcData2, dstData1, blen2, de2, de1, oneToOneFlag);
    ESMC_DELayoutCopy(srcData1, dstData2, blen1, de1, de2, oneToOneFlag);
  }
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutExchange()"
//BOP
// !IROUTINE:  ESMC_DELayoutExchange
//
// !INTERFACE:
int ESMC_DELayout::ESMC_DELayoutExchange(
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
  return ESMC_DELayoutExchange(srcData1, srcData2, dstData1, dstData2, 
    blen1, blen2, de1, de2, oneToOneFlag);
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutBcast()"
//BOP
// !IROUTINE:  ESMC_DELayoutBcast
//
// !INTERFACE:
int ESMC_DELayout::ESMC_DELayoutBcast(
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
    ESMC_DELayoutCopy(data, data, blen, rootDE, i, ESMF_TRUE);
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutBcast()"
//BOP
// !IROUTINE:  ESMC_DELayoutBcast
//
// !INTERFACE:
int ESMC_DELayout::ESMC_DELayoutBcast(
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
    ESMC_DELayoutCopy(data, data, blen, rootDE, i, oneToOneFlag);
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutScatter()"
//BOP
// !IROUTINE:  ESMC_DELayoutScatter
//
// !INTERFACE:
int ESMC_DELayout::ESMC_DELayoutScatter(
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
  int mypet = vm.vmk_mypet();
  if (des[rootDE].petid==mypet){
    // my PET holds the rootDE
    if (oneToOneFlag==ESMF_TRUE){
      char *tempdata = (char *)srcdata;
      for (int i=0; i<ndes; i++){
        ESMC_DELayoutCopy((void **)tempdata, destdata, blen, rootDE, i,
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
        ESMC_DELayoutCopy(srcdata, destdata, blen, rootDE, i, ESMF_FALSE);
        tempdata += blen;
        srcdata[j] = tempdata;
      }
      srcdata[j] = rootdata;  // restore correct start of root's src data
    }
  }else{
    if (oneToOneFlag==ESMF_TRUE){
      for (int i=0; i<ndes; i++)
        ESMC_DELayoutCopy(srcdata, destdata, blen, rootDE, i, ESMF_TRUE);
    }else{
      for (int i=0; i<ndes; i++)
        ESMC_DELayoutCopy(srcdata, destdata, blen, rootDE, i, ESMF_FALSE);
    }
  }
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutScatter()"
//BOP
// !IROUTINE:  ESMC_DELayoutScatter
//
// !INTERFACE:
int ESMC_DELayout::ESMC_DELayoutScatter(
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
  return ESMC_DELayoutScatter(srcdata, destdata, blen, rootDE, oneToOneFlag);
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutGather()"
//BOP
// !IROUTINE:  ESMC_DELayoutGather
//
// !INTERFACE:
int ESMC_DELayout::ESMC_DELayoutGather(
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
  int mypet = vm.vmk_mypet();
  if (des[rootDE].petid==mypet){
    // my PET holds the rootDE -> receive chunks from all other DEs
    if (oneToOneFlag==ESMF_TRUE){
      char *tempdata = (char *)destdata;
      for (int i=0; i<ndes; i++){
        ESMC_DELayoutCopy(srcdata, (void **)tempdata, blen, i, rootDE,
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
        ESMC_DELayoutCopy(srcdata, destdata, blen, i, rootDE, ESMF_FALSE);
        tempdata += blen;
        destdata[j] = tempdata;
      }
      destdata[j] = rootdata;  // restore correct start of root's destdata
    }
  }else{
    if (oneToOneFlag==ESMF_TRUE){
      for (int i=0; i<ndes; i++)
        ESMC_DELayoutCopy(srcdata, destdata, blen, i, rootDE, ESMF_TRUE);
    }else{
      for (int i=0; i<ndes; i++)
        ESMC_DELayoutCopy(srcdata, destdata, blen, i, rootDE, ESMF_FALSE);
    }
  }
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutGather()"
//BOP
// !IROUTINE:  ESMC_DELayoutGather
//
// !INTERFACE:
int ESMC_DELayout::ESMC_DELayoutGather(
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
  return ESMC_DELayoutGather(srcdata, destdata, blen, rootDE, oneToOneFlag);
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutGatherV()"
//BOP
// !IROUTINE:  ESMC_DELayoutGatherV
//
// !INTERFACE:
int ESMC_DELayout::ESMC_DELayoutGatherV(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  void **srcdata, // input array
  void **destdata,// output array
  int *blen,      // array of message sizes in bytes for each DE
                  // - the PET that holds rootDE must provide all blen elementes
                  // - all other PETs only need to fill elements for their DEs
  int *bdestdispl,// displacement vector for destdata for each DE mes. in bytes
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
  int mypet = vm.vmk_mypet();
  if (des[rootDE].petid==mypet){
    // my PET holds the rootDE -> receive chunks from all other DEs
    if (oneToOneFlag==ESMF_TRUE){
      char *tempdata;
      for (int i=0; i<ndes; i++){
        tempdata = (char *)destdata + bdestdispl[i];
        ESMC_DELayoutCopy(srcdata, (void **)tempdata, blen[i], i, rootDE,
          ESMF_TRUE);
      }
    }else{
      int j;
      for (j=0; j<nmydes; j++)
        if (mydes[j]==rootDE) break;
      void *rootdata = destdata[j]; // backup the correct start of rootDE's data
      for (int i=0; i<ndes; i++){
        destdata[j] = (char *)rootdata + bdestdispl[i];;
        ESMC_DELayoutCopy(srcdata, destdata, blen[i], i, rootDE, ESMF_FALSE);
      }
      destdata[j] = rootdata;  // restore correct start of root's destdata
    }
  }else{
    if (oneToOneFlag==ESMF_TRUE){
      for (int i=0; i<ndes; i++)
        ESMC_DELayoutCopy(srcdata, destdata, blen[i], i, rootDE, ESMF_TRUE);
    }else{
      for (int i=0; i<ndes; i++)
        ESMC_DELayoutCopy(srcdata, destdata, blen[i], i, rootDE, ESMF_FALSE);
    }
  }
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutGatherV()"
//BOP
// !IROUTINE:  ESMC_DELayoutGatherV
//
// !INTERFACE:
int ESMC_DELayout::ESMC_DELayoutGatherV(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  void **srcdata, // input array
  void **destdata,// output array
  int *len,       // array of message sizes in elements for each DE
                  // - the PET that holds rootDE must provide all blen elementes
                  // - all other PETs only need to fill elements for their DEs
  int *destdispl, // displacement vector for destdata for each DE mes. in elem.
  ESMC_DataKind dtk,// data type kind
  int rootDE,     // root DE
  ESMC_Logical oneToOneFlag){   // indicator whether this Layout is 1-to-1
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------
  int rc;
  int *blen = new int[ndes];
  int *bdestdispl = new int[ndes];
  int dtk_size = ESMC_DataKindSize(dtk);
  for (int i=0; i<ndes; i++){
    blen[i] = len[i] * dtk_size;
    bdestdispl[i] = destdispl[i] * dtk_size;
  }
  rc =  ESMC_DELayoutGatherV(srcdata, destdata, blen, bdestdispl, rootDE,
    oneToOneFlag);
  delete [] blen;
  delete [] bdestdispl;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutAllFullReduce()"
//BOP
// !IROUTINE:  ESMC_DELayoutAllFullReduce
//
// !INTERFACE:
int ESMC_DELayout::ESMC_DELayoutAllFullReduce(
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
  ESMC_Operation op,  // reduction operation
  ESMC_Logical oneToOneFlag){   // indicator whether this Layout is 1-to-1
//
// !DESCRIPTION:
//    Reduce data into a single value
//
//EOP
//-----------------------------------------------------------------------------
  // local reference to VM
  ESMC_VM &vm = *myvm;
  // very crude implementation of a layout wide FullReduce
  int mypet = vm.vmk_mypet();
  void *localresult;
  int local_i4;
  float local_r4;
  double local_r8;
  // first reduce across all of this PET's DEs
  switch (op){
  case ESMF_SUM:
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
  case ESMF_MIN:
    ESMC_LogDefault.ESMC_LogWrite("ESMF_MIN is not yet implemented.", 
      ESMC_LOG_ERROR);
    break;
  case ESMF_MAX:
    ESMC_LogDefault.ESMC_LogWrite("ESMF_MAX is not yet implemented.", 
      ESMC_LOG_ERROR);
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
  vm.vmk_allreduce(localresult, result, 1, vmt, (vmOp)op);
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutFindDEtoPET()"
//BOP
// !IROUTINE:  ESMC_DELayoutFindDEtoPET
//
// !INTERFACE:
int ESMC_DELayout::ESMC_DELayoutFindDEtoPET(int npets){
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
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutFillLocal()"
//BOP
// !IROUTINE:  ESMC_DELayoutFillLocal
//
// !INTERFACE:
int ESMC_DELayout::ESMC_DELayoutFillLocal(int mypet){
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
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutDataCreate()"
//BOP
// !IROUTINE:  ESMC_DELayoutDataCreate
//
// !INTERFACE:
void **ESMC_DELayoutDataCreate(
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
    // LogErr catches the allocation error
    ESMC_LogDefault.ESMC_LogMsgAllocError("for new DELayoutData object.", rc);  
    return(ESMC_NULL_POINTER);
  }
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutDataAdd()"
//BOP
// !IROUTINE:  ESMC_DELayoutDataAdd
//
// !INTERFACE:
int ESMC_DELayoutDataAdd(
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
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutDataDestroy()"
//BOP
// !IROUTINE:  ESMC_DELayoutDataDestroy
//
// !INTERFACE:
int ESMC_DELayoutDataDestroy(
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
