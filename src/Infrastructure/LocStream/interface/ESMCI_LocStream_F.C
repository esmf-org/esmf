// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC interface routines

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
//
//-----------------------------------------------------------------------------
//
// insert any higher level, 3rd party or system includes here

#include <cstring>
using namespace std;

#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_DistGrid.h"
#include "ESMCI_Array.h"


//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version =
             "$Id$";
//-----------------------------------------------------------------------------

extern "C" {
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the LocStream routines
//
//


// non-method functions
void FTN_X(c_esmc_locstreamgetkeybnds)(ESMCI::Array **_array,
                                     int *_localDE,
                                     int *exclusiveLBound,
                                     int *exclusiveUBound,
                                     int *exclusiveCount,
                                     int *computationalLBound,
                                     int *computationalUBound,
                                     int *computationalCount,
                                     int *totalLBound,
                                     int *totalUBound,
                                     int *totalCount,
                                     int *rc){

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_locstreamgetkeybnds()"

  ESMCI::Array *array;
  int localDE;
  int localrc;

  // Initialize return code; assume routine not implemented
  if (rc != NULL) *rc = ESMC_RC_NOT_IMPL;

  // Dereference variables
  array=*_array;

  // localDE
  if (ESMC_NOT_PRESENT_FILTER(_localDE) == ESMC_NULL_POINTER) {
    if (array->getDistGrid()->getDELayout()->getLocalDeCount()>1) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
                                    "- Must provide localDE if localDeCount >1",
                                    ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));
      return;
    } else {
      localDE=0;
    }
  } else {
    localDE=*_localDE; // already 0 based

    // Input Error Checking
    if ((localDE < 0) || (localDE >=array->getDistGrid()->getDELayout()->getLocalDeCount())) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
        "- localDE outside range on this processor", ESMC_CONTEXT,
        ESMC_NOT_PRESENT_FILTER(rc));
      return;
    }
  }

  // ExclusiveLBound
  if (ESMC_NOT_PRESENT_FILTER(exclusiveLBound) != ESMC_NULL_POINTER) {
    *exclusiveLBound=*(array->getExclusiveLBound()+localDE);
  }

  // ExclusiveUBound
  if (ESMC_NOT_PRESENT_FILTER(exclusiveUBound) != ESMC_NULL_POINTER) {
    *exclusiveUBound=*(array->getExclusiveUBound()+localDE);
  }

  // ExclusiveCount
  if (ESMC_NOT_PRESENT_FILTER(exclusiveCount) != ESMC_NULL_POINTER) {
    *exclusiveCount=*(array->getExclusiveUBound()+localDE) -
                    *(array->getExclusiveLBound()+localDE) + 1;
  }

  // ComputationalLBound
  if (ESMC_NOT_PRESENT_FILTER(computationalLBound) != ESMC_NULL_POINTER) {
    *computationalLBound=*(array->getComputationalLBound()+localDE);
  }

  // ComputationalUBound
  if (ESMC_NOT_PRESENT_FILTER(computationalUBound) != ESMC_NULL_POINTER) {
    *computationalUBound=*(array->getComputationalUBound()+localDE);
  }

  // ComputationalCount
  if (ESMC_NOT_PRESENT_FILTER(computationalCount) != ESMC_NULL_POINTER) {
    *computationalCount=*(array->getComputationalUBound()+localDE) -
                        *(array->getComputationalLBound()+localDE) + 1;
  }


  // TotalLBound
  if (ESMC_NOT_PRESENT_FILTER(totalLBound) != ESMC_NULL_POINTER) {
    *totalLBound=*(array->getTotalLBound()+localDE);
  }

  // TotalUBound
  if (ESMC_NOT_PRESENT_FILTER(totalUBound) != ESMC_NULL_POINTER) {
    *totalUBound=*(array->getTotalUBound()+localDE);
  }

  // TotalCount
  if (ESMC_NOT_PRESENT_FILTER(totalCount) != ESMC_NULL_POINTER) {
    *totalCount=*(array->getTotalUBound()+localDE) -
                *(array->getTotalLBound()+localDE) + 1;
  }



  // Return ESMF_SUCCESS
  if (rc != NULL) *rc = ESMF_SUCCESS;

  return;
}

// non-method functions
void FTN_X(c_esmc_locstreamgetelbnd)(ESMCI::DistGrid **_distgrid,
                                 int *_localDE,
                                 ESMC_IndexFlag *_indexflag,
                                 int *exclusiveLBound,
                                 int *rc){

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_locstreamgetelbnd()"

  ESMCI::DistGrid *distgrid;
  int localDE;
  int localrc;
  ESMC_IndexFlag indexflag;

  // Initialize return code; assume routine not implemented
  if (rc != NULL) *rc = ESMC_RC_NOT_IMPL;

  // Dereference variables
  distgrid=*_distgrid;
  indexflag=*_indexflag;

  // localDE
  if (ESMC_NOT_PRESENT_FILTER(_localDE) == ESMC_NULL_POINTER) {
    if (distgrid->getDELayout()->getLocalDeCount()>1) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
                                    "- Must provide localDE if localDeCount >1",
                                    ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));
      return;
    } else {
      localDE=0;
    }
  } else {
    localDE=*_localDE; // already 0 based

    // Input Error Checking
    if ((localDE < 0) || (localDE >=distgrid->getDELayout()->getLocalDeCount())) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
        "- localDE outside range on this processor", ESMC_CONTEXT,
        ESMC_NOT_PRESENT_FILTER(rc));
      return;
    }
  }

  if (indexflag==ESMC_INDEX_DELOCAL) {
    *exclusiveLBound = 1; // excl. region starts at (1,1,1...)
  } else {

    // Get some useful information
    const int *localDeToDeMap = distgrid->getDELayout()->getLocalDeToDeMap();

    // Get the Global DE from the local DE
    int de = localDeToDeMap[localDE];

    // obtain min index for this DE
    int const *index_min=distgrid->getMinIndexPDimPDe(de,&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      ESMC_NOT_PRESENT_FILTER(rc))) return;

    // Set lower bound of exclusive region
    *exclusiveLBound = *index_min;
  }

  // Return ESMF_SUCCESS
  if (rc != NULL) *rc = ESMF_SUCCESS;
  return;
}


void FTN_X(c_esmc_locstreamgeteubnd)(ESMCI::DistGrid **_distgrid,
                                 int *_localDE,
                                 ESMC_IndexFlag *_indexflag,
                                 int *exclusiveUBound,
                                 int *rc){

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_locstreamgeteubnd()"

  ESMCI::DistGrid *distgrid;
  int localDE;
  int localrc;
  ESMC_IndexFlag indexflag;

  // Initialize return code; assume routine not implemented
  if (rc != NULL) *rc = ESMC_RC_NOT_IMPL;

  // Dereference variables
  distgrid=*_distgrid;
  indexflag=*_indexflag;

  // localDE
  if (ESMC_NOT_PRESENT_FILTER(_localDE) == ESMC_NULL_POINTER) {
    if (distgrid->getDELayout()->getLocalDeCount()>1) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
                                    "- Must provide localDE if localDeCount >1",
                                    ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));
      return;
    } else {
      localDE=0;
    }
  } else {
    localDE=*_localDE; // already 0 based

    // Input Error Checking
    if ((localDE < 0) || (localDE >=distgrid->getDELayout()->getLocalDeCount())) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
        "- localDE outside range on this processor", ESMC_CONTEXT,
        ESMC_NOT_PRESENT_FILTER(rc));
      return;
    }
  }

  // Get some useful information
  const int *localDeToDeMap = distgrid->getDELayout()->getLocalDeToDeMap();
  const int *indexCountPDimPDe = distgrid->getIndexCountPDimPDe();

  // Get the Global DE from the local DE
  int de = localDeToDeMap[localDE];

  // Set upper bound based on indexflag
  if (indexflag==ESMC_INDEX_DELOCAL) {
    *exclusiveUBound = indexCountPDimPDe[de];
  } else {

    // obtain max index for this DE
    int const *index_max=distgrid->getMaxIndexPDimPDe(de,&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      ESMC_NOT_PRESENT_FILTER(rc))) return;

    // Set upper bound of exclusive region
    *exclusiveUBound = *index_max;
  }
  // Return ESMF_SUCCESS
  if (rc != NULL) *rc = ESMF_SUCCESS;

  return;
}



#if 1
// non-method functions
void FTN_X(c_esmc_locstreamserialize)(ESMC_IndexFlag *indexflag,
                int *keyCount,
                char *buffer, int *length, int *offset,
                ESMC_InquireFlag *inquireflag,
                int *rc,
                ESMCI_FortranStrLenArg buffer_l){

    ESMC_IndexFlag *ifp;
    int *ip;


    // Initialize return code; assume routine not implemented
    if (rc) *rc = ESMC_RC_NOT_IMPL;

    // TODO: verify length > vars.
    int size = sizeof(ESMC_IndexFlag) + sizeof(int);
    if (*inquireflag != ESMF_INQUIREONLY) {
      if ((*length - *offset) < size) {
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "Buffer too short to add a LocStream object", ESMC_CONTEXT, rc);
         return;
      }
    }


    // Save indexflag
    ifp = (ESMC_IndexFlag *)(buffer + *offset);
    if (*inquireflag != ESMF_INQUIREONLY)
      *ifp++ = *indexflag;

    // Save keyCount
    ip= (int *)ifp;
    if (*inquireflag != ESMF_INQUIREONLY)
      *ip++ = *keyCount;

    // Adjust offset
    *offset += sizeof(ESMC_IndexFlag) + sizeof(int);

    // return success
    if (rc) *rc = ESMF_SUCCESS;

    return;
}


void FTN_X(c_esmc_locstreamdeserialize)(ESMC_IndexFlag *indexflag,
                int *keyCount, char *buffer,
                int *offset,
                int *rc,
                ESMCI_FortranStrLenArg buffer_l){

    ESMC_IndexFlag *ifp;
    int *ip;

    // Initialize return code; assume routine not implemented
    if (rc) *rc = ESMC_RC_NOT_IMPL;

    // Get indexflag
    ifp = (ESMC_IndexFlag *)(buffer + *offset);
    *indexflag=*ifp++;

    // Get keyCount
    ip= (int *)ifp;
    *keyCount=*ip++;

    // Adjust offset
    *offset += sizeof(ESMC_IndexFlag) + sizeof(int);

    // return success
    if (rc) *rc = ESMF_SUCCESS;

    return;
}

// non-method functions
void FTN_X(c_esmc_locstreamkeyserialize)(
                                       int *keyNameLen, char *keyName,
                                       int *unitsLen, char *units,
                                       int *longNameLen, char *longName,
                char *buffer, int *length, int *offset,
                ESMC_InquireFlag *inquireflag,
                int *rc,
                ESMCI_FortranStrLenArg keyName_l,
                ESMCI_FortranStrLenArg units_l,
                ESMCI_FortranStrLenArg longName_l,
                ESMCI_FortranStrLenArg buffer_l) {

  ESMC_InquireFlag linquireflag = *inquireflag;
  int *ip;
  char *cp;
  int r;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  // TODO: verify length > vars.
  int size = *keyNameLen + *unitsLen + *longNameLen;
  if (*inquireflag != ESMF_INQUIREONLY) {
    if ((*length - *offset) < size) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
         "Buffer too short to add a LocStream object", ESMC_CONTEXT, rc);
      return;
    }
  }

  // Get pointer to memory
  ip = (int *)(buffer + *offset);

  // Save string lengths
  if (linquireflag != ESMF_INQUIREONLY) {
    *ip++ = *keyNameLen;
    *ip++ = *unitsLen;
    *ip++ = *longNameLen;
  }

  // Switch to char pointer
  cp = (char *)ip;

  // Save keyNames
  if (linquireflag != ESMF_INQUIREONLY)
    memcpy((void *)cp, (const void *)keyName, *keyNameLen*sizeof(char));
  cp += *keyNameLen*sizeof(char);

  // Save units
  if (linquireflag != ESMF_INQUIREONLY)
    memcpy((void *)cp, (const void *)units, *unitsLen*sizeof(char));
  cp += *unitsLen*sizeof(char);

  // Save longName
  if (linquireflag != ESMF_INQUIREONLY)
    memcpy((void *)cp, (const void *)longName, *longNameLen*sizeof(char));
  cp += *longNameLen*sizeof(char);

  // Adjust offset
  *offset += 3*sizeof(int)+*keyNameLen + *unitsLen + *longNameLen;

  // Adjust alignment
  r=*offset%8;
  if (r!=0) *offset += 8-r;

  // return success
  if (rc) *rc = ESMF_SUCCESS;

  return;
}


void FTN_X(c_esmc_locstreamkeydeserialize)(
                                         char *keyName,
                                         char *units,
                                         char *longName,
                                         char *buffer,
                                         int *offset,
                                         int *rc,
                                         ESMCI_FortranStrLenArg keyName_l,
                                         ESMCI_FortranStrLenArg units_l,
                                         ESMCI_FortranStrLenArg longName_l,
                                         ESMCI_FortranStrLenArg buffer_l) {

  int *ip;
  char *cp;
  int r, keyNameLen, unitsLen, longNameLen;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  // Get pointer to memory
  ip = (int *)(buffer + *offset);

  // Save string lengths
  keyNameLen = *ip++;
  unitsLen = *ip++;
  longNameLen = *ip++;

  // Switch to char pointer
  cp = (char *)ip;

  // Save keyNames
  // First fill with spaces (NOTE THAT THIS ASSUMES THAT keyName is of size ESMF_MAXSTR)
  memset((void *)keyName,' ', ESMF_MAXSTR*sizeof(char));
  memcpy((void *)keyName, (const void *)cp, keyNameLen*sizeof(char));
  cp += keyNameLen*sizeof(char);

  // Save units
  // First fill with spaces (NOTE THAT THIS ASSUMES THAT units is of size ESMF_MAXSTR)
  memset((void *)units,' ', ESMF_MAXSTR*sizeof(char));
  memcpy((void *)units, (const void *)cp, unitsLen*sizeof(char));
  cp += unitsLen*sizeof(char);

  // Save longName
  // First fill with spaces (NOTE THAT THIS ASSUMES THAT longName is of size ESMF_MAXSTR)
  memset((void *)longName,' ', ESMF_MAXSTR*sizeof(char));
  memcpy((void *)longName, (const void *)cp, longNameLen*sizeof(char));
  cp += longNameLen*sizeof(char);

  // Adjust offset
  *offset += 3*sizeof(int)+keyNameLen + unitsLen + longNameLen;

  // Adjust alignment
  r=*offset%8;
  if (r!=0) *offset += 8-r;

  // return success
  if (rc) *rc = ESMF_SUCCESS;

  return;
}


#endif

}
