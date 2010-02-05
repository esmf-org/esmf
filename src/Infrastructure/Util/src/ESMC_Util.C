// $Id: ESMC_Util.C,v 1.41.2.1 2010/02/05 20:01:08 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

#define ESMF_FILENAME "ESMC_Util.C"

// ESMC Util method implementation (body) file

// single blank line to make protex happy.
//BOP

//EOP
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ Util methods declared
// in the companion file ESMC_Util.h
//
//-----------------------------------------------------------------------------
//
 // associated class definition file and others
#include <string.h>
#include <stdlib.h>
#include "ESMCI_Util.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"
#include "ESMF_LogMacros.inc"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Util.C,v 1.41.2.1 2010/02/05 20:01:08 svasquez Exp $";
//-----------------------------------------------------------------------------

// define constants once to avoid duplicate instantiations
ESMC_ObjectID ESMC_ID_BASE = {1, "ESMF_Base"};
ESMC_ObjectID ESMC_ID_IOSPEC = {2, "ESMF_IOSpec"};
ESMC_ObjectID ESMC_ID_LOGERR = {3, "ESMF_LogErr"};
ESMC_ObjectID ESMC_ID_TIME = {4, "ESMF_Time"};
ESMC_ObjectID ESMC_ID_CALENDAR = {5, "ESMF_Calendar"};
ESMC_ObjectID ESMC_ID_TIMEINTERVAL = {6, "ESMF_TimeInterval"};
ESMC_ObjectID ESMC_ID_ALARM = {7, "ESMF_Alarm"};
ESMC_ObjectID ESMC_ID_CLOCK = {8, "ESMF_Clock"};
ESMC_ObjectID ESMC_ID_ARRAYSPEC = {9, "ESMF_ArraySpec"};
ESMC_ObjectID ESMC_ID_LOCALARRAY = {10, "ESMF_LocalArray"};
ESMC_ObjectID ESMC_ID_ARRAYBUNDLE = {11, "ESMF_ArrayBundle"};
ESMC_ObjectID ESMC_ID_VM = {12, "ESMF_VM"};
ESMC_ObjectID ESMC_ID_DELAYOUT = {13, "ESMF_DELayout"};
ESMC_ObjectID ESMC_ID_CONFIG = {14, "ESMF_Config"};
ESMC_ObjectID ESMC_ID_ARRAY = {16, "ESMF_Array"};
ESMC_ObjectID ESMC_ID_PHYSGRID = {18, "ESMF_PhysGrid"};
ESMC_ObjectID ESMC_ID_IGRID = {19, "ESMF_IGrid"};
ESMC_ObjectID ESMC_ID_EXCHANGEPACKET = {20, "ESMF_ExchangePacket"};
ESMC_ObjectID ESMC_ID_COMMTABLE = {21, "ESMF_CommTable"};
ESMC_ObjectID ESMC_ID_ROUTETABLE = {22, "ESMF_RouteTable"};
ESMC_ObjectID ESMC_ID_ROUTE = {23, "ESMF_Route"};
ESMC_ObjectID ESMC_ID_ROUTEHANDLE = {24, "ESMF_RouteHandle"};
ESMC_ObjectID ESMC_ID_FIELDDATAMAP = {25, "ESMF_FieldDataMap"};
ESMC_ObjectID ESMC_ID_FIELD = {26, "ESMF_Field"};
ESMC_ObjectID ESMC_ID_BUNDLEDATAMAP = {27, "ESMF_FieldBundleDataMap"};
ESMC_ObjectID ESMC_ID_FIELDBUNDLE = {28, "ESMF_FieldBundle"};
ESMC_ObjectID ESMC_ID_GEOMBASE = {29, "ESMF_GeomBase"};
ESMC_ObjectID ESMC_ID_REGRID = {30, "ESMF_Regrid"};
ESMC_ObjectID ESMC_ID_LOCSTREAM = {31, "ESMF_Locstream"};
ESMC_ObjectID ESMC_ID_STATE = {32, "ESMF_State"};
ESMC_ObjectID ESMC_ID_GRIDCOMPONENT = {33, "ESMF_GridComponent"};
ESMC_ObjectID ESMC_ID_CPLCOMPONENT = {34, "ESMF_CplComponent"};
ESMC_ObjectID ESMC_ID_COMPONENT = {35, "ESMF_Component"};
ESMC_ObjectID ESMC_ID_NONE = {99, "ESMF_None"};

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the ESMC_Util routines
//
//

//-----------------------------------------------------------------------------
// General utility methods
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_TypeKindSize"
//BOPI
// !IROUTINE:  ESMC_TypeKindSize - Return number of bytes in a TypeKind
//
// !INTERFACE:
    int ESMC_TypeKindSize(
//
// !RETURN VALUE:
//  int number of bytes (negative for error)
// 
// !ARGUMENTS:
    ESMC_TypeKind dk) {       // in - a data kind 
//EOPI

    switch (dk) {
      case ESMC_TYPEKIND_I1:  return  1;
      case ESMC_TYPEKIND_I2:  return  2;
      case ESMC_TYPEKIND_I4:  return  4;
      case ESMC_TYPEKIND_I8:  return  8;
      case ESMC_TYPEKIND_R4:  return  4;
      case ESMC_TYPEKIND_R8:  return  8;
      case ESMF_C8:  return  8;
      case ESMF_C16: return 16;
      default:
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
                                "Unknown TypeKind", NULL);
       return -1;
    }

    /* not reached */
}

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_StatusString"
//BOPI
// !IROUTINE:  ESMC_StatusString - Return fixed char string for printing
//
// !INTERFACE:
    const char *ESMC_StatusString(
//
// !RETURN VALUE:
//  const char * to string name of value
// 
// !ARGUMENTS:
    ESMC_Status stat) {       // in - a status value
//EOPI

    switch (stat) {
      case ESMF_STATUS_UNINIT:       return  "Uninitialized";
      case ESMF_STATUS_READY:        return  "Ready";
      case ESMF_STATUS_UNALLOCATED:  return  "Unallocated";
      case ESMF_STATUS_ALLOCATED:    return  "Allocated";
      case ESMF_STATUS_BUSY:         return  "Busy";
      case ESMF_STATUS_INVALID:      return  "Invalid";
      default:
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
                                      "Unknown Status", NULL);
       return NULL;
    }

    /* not reached */
}

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_TypeKindString"
//BOPI
// !IROUTINE:  ESMC_TypeKindString - Return fixed char string for printing
//
// !INTERFACE:
    const char *ESMC_TypeKindString(
//
// !RETURN VALUE:
//  const char * to string name of value
// 
// !ARGUMENTS:
    ESMC_TypeKind dk) {       // in - a datakind value
//EOPI

    switch (dk) {
      case ESMC_TYPEKIND_I1:      return  "Integer*1";
      case ESMC_TYPEKIND_I2:      return  "Integer*2";
      case ESMC_TYPEKIND_I4:      return  "Integer*4";
      case ESMC_TYPEKIND_I8:      return  "Integer*8";
      case ESMC_TYPEKIND_R4:      return  "Real*4";
      case ESMC_TYPEKIND_R8:      return  "Real*8";
      case ESMF_C8:      return  "Complex*8";
      case ESMF_C16:     return  "Complex*16";
      case ESMC_TYPEKIND_LOGICAL: return  "Logical";
      case ESMC_TYPEKIND_CHARACTER: return "Character";
      default:
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
                                     "Unknown TypeKind", NULL);
         return "";
    }

    /* not reached */
}

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_LogicalString"
//BOPI
// !IROUTINE:  ESMC_LogicalString - Return fixed char string for printing
//
// !INTERFACE:
    const char *ESMC_LogicalString(
//
// !RETURN VALUE:
//  const char * to string name of value
// 
// !ARGUMENTS:
    ESMC_Logical tf) {       // in - a logical value
//EOPI

    switch (tf) {
      case ESMF_TRUE:      return  "True";
      case ESMF_FALSE:     return  "False";
      default:
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
                                     "Unknown TypeKind", NULL);
         return NULL;
    }

    /* not reached */
}

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_F90toCstring"
//BOPI
// !IROUTINE:  ESMC_F90toCstring - Convert an F90 string into a C string
//
// !INTERFACE:
    char *ESMC_F90toCstring(
//
// !RETURN VALUE:
//  returns pointer to a newly allocated C string buffer.  this space
//  must be deleted by the caller when finished!
// 
// !ARGUMENTS:
    const char *src,                // in - F90 character source buffer
    ESMCI_FortranStrLenArg slen) {   // in - length of the F90 source buffer
//EOPI

    char *cp, *ctmp;
    ESMCI_FortranStrLenArg clen;

    if (slen == 0) return NULL; // nothing to do, but not an error
    
    // minor idiotproofing
    if ((src == NULL) || (src[0] == '\0') || (slen < 0)) {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                            "bad count or NULL pointer", NULL);
       return NULL;
    }

    // count back from end of string to last non-blank character.
    for (clen=slen; clen > 0; clen--)
      if (src[clen-1] != ' ') break;

    // make new space and leave room for a null terminator
    ctmp = new char[clen+1];
    strncpy(ctmp, src, clen);
    ctmp[clen] = '\0';

    // return pointer.  caller MUST free this when finished with it.
    return ctmp;
}

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_F90toCstring"
//BOPI
// !IROUTINE:  ESMC_F90toCstring - Convert an F90 string into a C string
//
// !INTERFACE:
    int ESMC_F90toCstring(
//
// !RETURN VALUE:
//  converts an F90, space padded string into a C++ null terminated string
//  returns ESMF_SUCCESS or ESMF_FAILURE.
// 
// !ARGUMENTS:
    const char *src,               // in - F90 character source buffer
    ESMCI_FortranStrLenArg slen,   // in - length of the F90 source buffer
    char *dst,                     // inout - pointer to a buffer to hold C string
    ESMCI_FortranStrLenArg dlen) { // in - max len of C dst buffer, inc term NULL
//EOPI

    char *cp;
    ESMCI_FortranStrLenArg clen;
    int rc;
    char msgbuf[ESMF_MAXSTR];

    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    // minor idiotproofing
    if ((src == NULL) || (src[0] == '\0') || (slen <= 0) ||
        (dst == NULL) || (dlen <= 0)) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad count or NULL pointer", &rc);
            return rc;
    }

    // count back from end of string to last non-blank character.
    for (clen=slen; clen > 0; clen--)
      if (src[clen-1] != ' ') break;

    // make sure dst space is long enough 
    if (clen >= dlen) {
       sprintf(msgbuf, 
             "dest buffer size of %ld bytes too small, must be >= %ld bytes\n", 
             (long) dlen, (long) clen+1);
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &rc);
       return rc;
    }
    
    strncpy(dst, src, clen);
    dst[clen] = '\0';

    // return ok.  caller has passed us in dst buffer so it is up to them
    // to manage that space.
    rc = ESMF_SUCCESS;
    return rc;
}

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_CtoF90string"
//BOPI
// !IROUTINE:  ESMC_CtoF90string - Convert a C string into an F90 string
//
// !INTERFACE:
    int ESMC_CtoF90string(
//
// !RETURN VALUE:
//  constructs a space padded F90, non-null terminated string in dst buffer.
//  returns ESMF_SUCCESS or ESMF_FAILURE.
// 
// !ARGUMENTS:
    const char *src,               // in - C++ null term string source buffer
    char *dst,                     // inout - pointer to a buffer holding F90 string
    
    ESMCI_FortranStrLenArg dlen) {  // in - length of dst buffer, space padded
//EOPI

    char *cp;
    int rc;
    ESMCI_FortranStrLenArg clen;
    char msgbuf[ESMF_MAXSTR];

    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    // minor idiotproofing
    if ((src == NULL) || (dst == NULL) || (dlen <= 0)) {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad count or NULL pointer", &rc);
       return rc;
    }

    // fortran doesn't need trailing null, so len can be up to == maxlen
    clen = strlen(src);
    if (clen > dlen) {
       sprintf(msgbuf, 
             "dest buffer size of %ld bytes too small, must be >= %ld bytes\n", 
             (long) dlen, (long) clen);
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &rc);
       return rc;
    }

    // move bytes, then pad rest of string to spaces
    strncpy(dst, src, clen);
    
    for (cp=&dst[clen]; cp < &dst[dlen]; cp++)
        *cp = ' ';

    // return ok. 
    rc = ESMF_SUCCESS;
    return rc;
}

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "esmf_f90tostring"
//BOPI
// !IROUTINE:  ESMF_F90toCstring - Fortran-callable conversion routine from F90 character to C++ string
//
// !INTERFACE:
extern "C" {
    void  FTN(esmf_f90tocstring)(
//
// !RETURN VALUE:
//  converts an F90, space padded string into a C++ null terminated string
//  sets *rc to ESMF_SUCCESS or ESMF_FAILURE, returns nothing.
//  the arguments below labeled *hidden* are added by the fortran compiler
//  and should not appear in the fortran argument list
// 
// !ARGUMENTS:
    const char *src,               // in - F90 character source buffer
    char *dst,                     // inout - pointer to a buffer to hold C string
    int *rc,                       // out - return code
    ESMCI_FortranStrLenArg slen,   // *hidden* in - length of the F90 source buffer
    ESMCI_FortranStrLenArg dlen) { // *hidden* in - max len of C dst buffer, inc term NULL
//EOPI

    char *cp;
    ESMCI_FortranStrLenArg clen;
    char msgbuf[ESMF_MAXSTR];

    // Initialize return code; assume routine not implemented
    if (rc) *rc = ESMC_RC_NOT_IMPL;

    // minor idiotproofing
    if ((src == NULL) || (src[0] == '\0') || (slen < 0) ||
        (dst == NULL) || (dlen <= 0)) {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad count or NULL pointer", rc);
       return;
    }

    // count back from end of string to last non-blank character.
    for (clen=slen; clen > 0; clen--)
      if (src[clen-1] != ' ') break;

    // make sure dst space is long enough 
    if (clen >= dlen) {
       sprintf(msgbuf, 
             "dest buffer size of %ld bytes too small, must be >= %ld bytes\n", 
             (long) dlen, (long) clen+1);
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, rc);
       return;
    }
    
    strncpy(dst, src, clen);
    dst[clen] = '\0';

    // return ok.  caller has passed us in dst buffer so it is up to them
    // to manage that space.
    if (rc) *rc = ESMF_SUCCESS;
    return;
 }
}

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "esmf_ctof90string"
//BOPI
// !IROUTINE:  ESMF_CtoF90string - Fortran-callable conversion routine from C++ string to F90 character 
//
// !INTERFACE:
extern "C" {
    void  FTN(esmf_ctof90string)(
//
// !RETURN VALUE:
//  converts a C++ null terminated string info an F90, space padded string
//  sets *rc to ESMF_SUCCESS or ESMF_FAILURE, returns nothing.
//  the arguments below labeled *hidden* are added by the fortran compiler
//  and should not appear in the fortran argument list
// 
// !ARGUMENTS:
    const char *src,               // in - F90 character source buffer
    char *dst,                     // inout - pointer to a buffer to hold C string
    int *rc,                       // out - return code
    ESMCI_FortranStrLenArg slen,   // *hidden* in - length of the F90 source buffer
    ESMCI_FortranStrLenArg dlen) { // *hidden* in - max len of C dst buffer, inc term NULL
//EOPI

    char *cp;
    ESMCI_FortranStrLenArg clen;
    char msgbuf[ESMF_MAXSTR];

    // Initialize return code; assume routine not implemented
    if (rc) *rc = ESMC_RC_NOT_IMPL;

    // minor idiotproofing
    if ((src == NULL) || (src[0] == '\0') || (slen < 0) ||
        (dst == NULL) || (dlen <= 0)) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad count or NULL pointer", rc);
            return;
    }

    // fortran doesn't need trailing null, so len can be up to == maxlen
    clen = strlen(src);
    if (clen > dlen) {
       sprintf(msgbuf, 
             "dest buffer size of %ld bytes too small, must be >= %ld bytes\n", 
             (long) dlen, (long) clen);
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, rc);
       return;
    }

    // move bytes, then pad rest of string to spaces
    strncpy(dst, src, clen);
    
    for (cp=&dst[clen]; cp < &dst[dlen-1]; cp++)
        *cp = ' ';

    // return ok.  caller has passed us in dst buffer so it is up to them
    // to manage that space.
    if (rc) *rc = ESMF_SUCCESS;
    return;
 }
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "esmf_pointertoint"
//BOPI
// !IROUTINE:  ESMF_PointerToInt - Fortran-callable routine which returns
//   the value of a fortran address as a simple integer.
//
// !INTERFACE:
extern "C" {
    void  FTN(esmf_pointertoint)(
//
// !RETURN VALUE:
//  converts a F90 pointer into a normal integer.
// 
// !ARGUMENTS:
    int *n,                    // in - number of expected bytes in a pointer
    short *s,                  // in - F90 pointer of some kind
    ESMC_POINTER *len) {  // out - that same value cast to an int/long
//EOPI

    // if n does not match the actual pointer size, return an error.
    // we cannot return good values if n is not correct.
    if (sizeof(n) != *n) {
        printf("error: fortran pointer size does not match C pointer size\n");
        printf("  fortran is sending %d bytes, C expects %ld bytes\n", 
                  *n, (long) sizeof(n));
        *len = 0;
        return;
    }

    if (sizeof(n) != sizeof(ESMC_POINTER)) {
        printf("error: C pointer size does not match include file value\n");
        printf("  C pointer is %ld bytes, ESMC_POINTER is %ld bytes\n",  
                   (long) sizeof(n), (long) sizeof(ESMC_POINTER));
        *len = 0;
        return;
    }

    // if we passed those tests, this should give valid results.

    *len = (ESMC_POINTER)s;
    return;
 }
}

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "esmf_pointerdifference"
//BOPI
// !IROUTINE:  ESMF_PointerDifference - Fortran-callable routine which returns
//   the difference between 2 addresses as a simple integer.
//
// !INTERFACE:
extern "C" {
    void  FTN(esmf_pointerdifference)(
//
// !RETURN VALUE:
//  converts the difference between 2 F90 pointers into a normal integer.
//  the pointers should be close enough to each other to be less than
//  2 ^ 32 apart so the result fits in to a standard integer return val.
// 
// !ARGUMENTS:
    int *n,            // in - number of expected bytes in a pointer
    short *s1,         // in - F90 pointer of some kind
    short *s2,         // in - F90 pointer of some kind
    int *len) {        // out - that same value cast to an int
//EOPI
    ESMC_POINTER t1, t2; 

    // if n does not match the actual pointer size, return an error.
    // we cannot return good values if n is not correct.
    if (sizeof(n) != *n) {
        printf("error: fortran pointer size does not match C pointer size\n");
        printf("  fortran is sending %d bytes, C expects %ld bytes\n", 
                  *n, (long) sizeof(n));
        *len = 0;
        return;
    }

    if (sizeof(n) != sizeof(ESMC_POINTER)) {
        printf("error: C pointer size does not match include file value\n");
        printf("  C pointer is %ld bytes, ESMC_POINTER is %ld bytes\n",  
                   (long) sizeof(n), (long) sizeof(ESMC_POINTER));
        *len = 0;
        return;
    }

    // if we passed those tests, this should give valid results.

    t1 = (ESMC_POINTER)s1;
    t2 = (ESMC_POINTER)s2;
    if (t1 > t2)
        *len = (int)(t1 - t2);
    else
        *len = (int)(t2 - t1);
  
    return;
 }
}


