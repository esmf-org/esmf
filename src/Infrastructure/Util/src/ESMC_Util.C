// $Id: ESMC_Util.C,v 1.28.2.3 2009/01/21 21:25:24 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
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
#include "ESMC_LogErr.h"
#include "ESMC_VM.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Util.C,v 1.28.2.3 2009/01/21 21:25:24 cdeluca Exp $";
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
ESMC_ObjectID ESMC_ID_ARRAYDATAMAP = {11, "ESMF_ArrayDataMap"};
ESMC_ObjectID ESMC_ID_VM = {12, "ESMF_VM"};
ESMC_ObjectID ESMC_ID_DELAYOUT = {13, "ESMF_DELayout"};
ESMC_ObjectID ESMC_ID_CONFIG = {14, "ESMF_Config"};
ESMC_ObjectID ESMC_ID_ARRAY = {16, "ESMF_Array"};
ESMC_ObjectID ESMC_ID_INTERNDG = {17, "ESMF_InternDG"};
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
ESMC_ObjectID ESMC_ID_BUNDLE = {28, "ESMF_FieldBundle"};
ESMC_ObjectID ESMC_ID_TRANSFORMVALUES = {29, "ESMF_TransformValues"};
ESMC_ObjectID ESMC_ID_REGRID = {30, "ESMF_Regrid"};
ESMC_ObjectID ESMC_ID_TRANSFORM = {31, "ESMF_Transform"};
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
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AxisIndexSet"
//BOPI
// !IROUTINE:  ESMC_AxisIndexSet - Initialize an AxisIndex object
//
// !INTERFACE:
    int ESMC_AxisIndexSet(
//
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
     ESMC_AxisIndex *ai,
     int min,
     int max,
     int stride) {
// 
// !DESCRIPTION:
//     Initialize/set an AxisIndex object.
//
//EOPI

    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

     if (ai == NULL) 
         return ESMF_FAILURE;

     ai->min = min;
     ai->max = max;
     ai->stride = stride;

     rc = ESMF_SUCCESS;
    return rc;
};

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AxisIndexSet"
//BOPI
// !IROUTINE:  ESMC_AxisIndexSet - Initialize an AxisIndex object
//
// !INTERFACE:
    int ESMC_AxisIndexSet(
//
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
     ESMC_AxisIndex *ai,
     int min,
     int max) {
// 
// !DESCRIPTION:
//     Initialize/set an AxisIndex object.  This one does not take a stride
//     and sets it to (max-min)+1.
//
//EOPI

    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

     if (ai == NULL) 
         return ESMF_FAILURE;

     ai->min = min;
     ai->max = max;
     ai->stride = (max-min) + 1;

     rc = ESMF_SUCCESS;
    return rc;
};

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AxisIndexGet"
//BOPI
// !IROUTINE:  ESMC_AxisIndexGet - Retrieve values from an AxisIndex object
//
// !INTERFACE:
    int ESMC_AxisIndexGet(
//
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
     ESMC_AxisIndex *ai,
     int *min,
     int *max,
     int *stride) {
// 
// !DESCRIPTION:
//     Get values from an AxisIndex object.
//
//EOPI

    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

     if (ai == NULL) 
        return ESMF_FAILURE;

     if (min) *min = ai->min;
     if (max) *max = ai->max;
     if (stride) *stride = ai->stride;

     rc = ESMF_SUCCESS;
    return rc;
};

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AxisIndexPrint"
//BOPI
// !IROUTINE:  ESMC_AxisIndexPrint - Print an AxisIndex object
//
// !INTERFACE:
    int ESMC_AxisIndexPrint(
//
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
     ESMC_AxisIndex *ai) {
// 
// !DESCRIPTION:
//     Print values from an AxisIndex object.
//
//EOPI
     char msgbuf[ESMF_MAXSTR];

    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

     if (ai == NULL) 
        ESMC_LogDefault.ESMC_LogWrite("Empty (NULL) AxisIndex pointer", 
                                       ESMC_LOG_INFO);

     sprintf(msgbuf, "min=%d, max=%d, stride=%d\n", ai->min, ai->max, ai->stride);
     ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);

     rc = ESMF_SUCCESS;
    return rc;
}

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AxisIndexCopy"
//BOP
// !IROUTINE:  ESMC_AxisIndexCopy - assignment operator for axis indices
//
// !INTERFACE:
      int ESMC_AxisIndexCopy(
//
// !RETURN VALUE:
//    error code 
//
// !ARGUMENTS:
        ESMC_AxisIndex *src,           // in - ESMC_AxisIndex
        ESMC_AxisIndex *dst) {         // out - ESMC_AxisIndex
//
// !DESCRIPTION:
//   copy an AxisIndex
//
//EOP

    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

  dst->min = src->min;
  dst->max = src->max;
  dst->stride = src->stride;

  rc = ESMF_SUCCESS;
    return rc;
}

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AxisIndexIntersect"
//BOP
// !IROUTINE:  ESMC_AxisIndexIntersect - return the intersection of 2 AIs
//
// !INTERFACE:
      bool ESMC_AxisIndexIntersect(
//
// !RETURN VALUE:
//    boolean: true means there is an intersect; false for none.
//
// !ARGUMENTS:
        int ndims,                     // in - number of dimensions
        ESMC_AxisIndex *src1,          // in - ESMC_AxisIndex
        ESMC_AxisIndex *src2,          // in - ESMC_AxisIndex
        ESMC_AxisIndex *dst) {         // out - ESMC_AxisIndex
//
// !DESCRIPTION:
//   compute the intersection of 2 AIs. return false if there is no 
//   intersection; return true and describe the intersection area with
//   a third AI.
//
//EOP
  
  int i;
  ESMC_AxisIndex *lo, *hi;

  // the inputs to this routine must be lists of AIs, ndims long.
  // the output is a list of AIs, ndims long.
  // for there to be an actual overlap of area, the overlap for each rank
  // must be true.  if any are false, the overall overlap is false and we
  // can return early.
  

  for (i=0; i<ndims; i++) {

    // get local labels sorted out so we have fewer cases to deal with.
    // lo has the smallest (or equal) minimum.
    if (src1[i].min < src2[i].min) {
      lo = src1+i;
      hi = src2+i;
    } else {
      lo = src2+i;
      hi = src1+i;
    }
  
    // disjoint completely?  (if edges touch, we need to capture that as a
    // yes and return the line as an overlap somehow.)  if disjoint, we cannot
    // have an overlap - exit early with the bad news.
    if (lo->max < hi->min) {
      dst[i].min = 0;
      dst[i].max = 0;
      dst[i].stride = 0;
      return false;
    }
  
    // there is an overlap in this dim - figure out what it is.
    // 2 cases - one is fully contained in the other, or partial overlap.
  
    // look for complete containment.
    if (lo->max > hi->max) {
      dst[i].min = hi->min;
      dst[i].max = hi->max;
      dst[i].stride = 0;   // we do not know the global strides at this point.

    } else {

      // only partial overlap, but good enough to continue.
      dst[i].min = hi->min;
      dst[i].max = lo->max;
      dst[i].stride = 0;   // we do not know the global strides at this point.
    }

    // and continue looping thru the ranks.
  }


  // if we have not returned yet, they overlap.
  return true;
}

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AxisIndexLocalToGlobal"
//BOP
// !IROUTINE:  ESMC_AxisIndexLocalToGlobal - translate local AI to global AIs
//
// !INTERFACE:
      int ESMC_AxisIndexLocalToGlobal(
//
// !RETURN VALUE:
//    error code 
//
// !ARGUMENTS:
        ESMC_AxisIndex *srclocal,      // in - local AI extents
        ESMC_AxisIndex *global,        // in - AIs for the global extents
        int *globalStarts,             // in - array of global offsets
        int ndims,                     // in - number of dimensions
        ESMC_AxisIndex *dstglobal) {   // out - AIs translated to global space
//
// !DESCRIPTION:
//   take a list of ndims AIs that are relative to the local decomposition,
//   plus a list of offsets to the local chunks, and translate this into
//   a list of AIs which are relative to the global igrid, with (0,0) being
//   the true origin, and not accounting for halo regions.
//
//   TODO: the global starts should be only for the igrid axes, not the others.
//   do we fix that when we make global starts? (we think yes.)
//   there are global igrid starts which are the same dim as the igrid.
//   then based on the field datamap we need to add 0 for non-igrid dims
//   and reorder if needed and then use the global starts which are the
//   same dim as the data array from there onwards.  (these routines are
//   assuming ndims == data array dims).
//
//EOP
  int i;

    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

  for (i=0; i<ndims; i++) {

    dstglobal[i].min = srclocal[i].min + globalStarts[i];
    dstglobal[i].max = srclocal[i].max + globalStarts[i];
    dstglobal[i].stride = global[i].stride;

  }
  
  rc = ESMF_SUCCESS;
    return rc;
}

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AxisIndexGlobalToLocal"
//BOP
// !IROUTINE:  ESMC_AxisIndexGlobalToLocal - translate global AI to local AI
//
// !INTERFACE:
      int ESMC_AxisIndexGlobalToLocal(
//
// !RETURN VALUE:
//    error code 
//
// !ARGUMENTS:
        int ndims,                     // in - number of dimensions
        ESMC_AxisIndex *srcglobal,     // in - global AI extents to transform
        ESMC_AxisIndex *globalref,     // in - global comp counts
        ESMC_AxisIndex *dstlocal) {    // out - AIs translated to local space
//
// !DESCRIPTION:
//   take a list of ndims AIs that are relative to the global decomposition,
//   plus a list of offsets to the local chunks, and translate this into
//   a list of AIs which are relative to the local chunk, with (0,0) being
//   the true origin, and strides must account for halo regions. (TODO: is this
//   last part true?)
//
//EOP
  int i;

    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

  for (i=0; i<ndims; i++) {

    dstlocal[i].min = srcglobal[i].min - globalref[i].min;
    dstlocal[i].max = srcglobal[i].max - globalref[i].min;
    dstlocal[i].stride = srcglobal[i].stride;

  }
  
  rc = ESMF_SUCCESS;
    return rc;
}

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AxisIndexEqual"
//BOPI
// !IROUTINE:  ESMC_AxisIndexEqual - Compare two AxisIndex structs for equality
//
// !INTERFACE:
    ESMC_Logical ESMC_AxisIndexEqual(
//
// !RETURN VALUE:
//    ESMC_Logical
// 
// !ARGUMENTS:
     ESMC_AxisIndex *ai1,
     ESMC_AxisIndex *ai2) {
// 
// !DESCRIPTION:
//     Compare two AxisIndex objects for equality.
//
//EOPI

     // if both null, say ok.
     if ((ai1 == NULL) && (ai2 == NULL))
        return ESMF_TRUE;   // in some sense...

     // if only 1 null, can't be equal.
     if ((ai1 == NULL) || (ai2 == NULL))
        return ESMF_FALSE;

     if ((ai1->min != ai2->min) ||
         (ai1->max != ai2->max) ||
         (ai1->stride != ai2->stride))
         return ESMF_FALSE;

     return ESMF_TRUE;
};

int  ESMC_DomainList::ESMC_DomainListGetDE(int domainnum) {
    int rc, de;
    //ESMC_DomainList *dp = this;
    FTN(f_esmf_domainlistgetde)(this, &domainnum, &de, &rc);
    return de;
}

ESMC_AxisIndex ESMC_DomainList::ESMC_DomainListGetAI(int domainnum, int ainum) {
    int rc;
    //ESMC_DomainList *dp = this;
    ESMC_AxisIndex AI;
    FTN(f_esmf_domainlistgetai)(this, &domainnum, &ainum, &AI, &rc);
    return AI;
}
   

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
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
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
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
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
         ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
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
         ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
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
// !IROUTINE:  ESMC_F90toCstring - Convert an F90 string into a C++ string
//
// !INTERFACE:
    char *ESMC_F90toCstring(
//
// !RETURN VALUE:
//  returns pointer to a newly allocated C string buffer.  this space
//  must be deleted by the caller when finished!
// 
// !ARGUMENTS:
    char *src,                // in - F90 character source buffer
    int slen) {               // in - length of the F90 source buffer
//EOPI

    char *cp, *ctmp;
    int clen;

    if (slen == 0) return NULL; // nothing to do, but not an error
    
    // minor idiotproofing
    if ((src == NULL) || (src[0] == '\0') || (slen < 0)) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                            "bad count or NULL pointer", NULL);
       return NULL;
    }

    // count back from end of string to last non-blank character.
    for (clen=slen, cp = &src[slen-1];(*cp == ' ') && (clen > 0); cp--, clen--);

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
// !IROUTINE:  ESMC_F90toCstring - Convert an F90 string into a C++ string
//
// !INTERFACE:
    int ESMC_F90toCstring(
//
// !RETURN VALUE:
//  converts an F90, space padded string into a C++ null terminated string
//  returns ESMF_SUCCESS or ESMF_FAILURE.
// 
// !ARGUMENTS:
    char *src,                // in - F90 character source buffer
    int slen,                 // in - length of the F90 source buffer
    char *dst,                // inout - pointer to a buffer to hold C string
    int dlen) {               // in - max len of C dst buffer, inc term NULL
//EOPI

    char *cp;
    int clen, rc;
    char msgbuf[ESMF_MAXSTR];

    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    // minor idiotproofing
    if ((src == NULL) || (src[0] == '\0') || (slen <= 0) ||
        (dst == NULL) || (dlen <= 0)) {
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad count or NULL pointer", &rc);
            return rc;
    }

    // count back from end of string to last non-blank character.
    for (clen=slen, cp = &src[slen-1];(*cp == ' ') && (clen > 0); cp--, clen--);

    // make sure dst space is long enough 
    if (clen >= dlen) {
       sprintf(msgbuf, 
             "dest buffer size of %d bytes too small, must be >= %d bytes\n", 
             dlen, clen+1);
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &rc);
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
// !IROUTINE:  ESMC_CtoF90string - Convert a C++ string into an F90 string
//
// !INTERFACE:
    int ESMC_CtoF90string(
//
// !RETURN VALUE:
//  constructs a space padded F90, non-null terminated string in dst buffer.
//  returns ESMF_SUCCESS or ESMF_FAILURE.
// 
// !ARGUMENTS:
    char *src,                // in - C++ null term string source buffer
    char *dst,                // inout - pointer to a buffer holding F90 string
    int dlen) {               // in - length of dst buffer, space padded
//EOPI

    char *cp;
    int clen, rc;
    char msgbuf[ESMF_MAXSTR];

    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    // minor idiotproofing
    if ((src == NULL) || (dst == NULL) || (dlen <= 0)) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad count or NULL pointer", &rc);
       return rc;
    }

    // fortran doesn't need trailing null, so len can be up to == maxlen
    clen = strlen(src);
    if (clen > dlen) {
       sprintf(msgbuf, 
             "dest buffer size of %d bytes too small, must be >= %d bytes\n", 
             dlen, clen);
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &rc);
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
    char *src,          // in - F90 character source buffer
    char *dst,          // inout - pointer to a buffer to hold C string
    int *rc,            // out - return code
    int *slen,          // *hidden* in - length of the F90 source buffer
    int *dlen) {        // *hidden* in - max len of C dst buffer, inc term NULL
//EOPI

    char *cp;
    int clen;
    char msgbuf[ESMF_MAXSTR];

    // Initialize return code; assume routine not implemented
    if (rc) *rc = ESMC_RC_NOT_IMPL;

    // minor idiotproofing
    if ((src == NULL) || (src[0] == '\0') || (*slen <= 0) ||
        (dst == NULL) || (*dlen <= 0)) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad count or NULL pointer", rc);
       return;
    }

    // count back from end of string to last non-blank character.
    for (clen= *slen, cp = &src[*slen-1]; (*cp == ' ') && (clen > 0); cp--, clen--)
        ;

    // make sure dst space is long enough 
    if (clen >= *dlen) {
       sprintf(msgbuf, 
             "dest buffer size of %d bytes too small, must be >= %d bytes\n", 
             *dlen, clen+1);
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, rc);
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
    char *src,         // in - F90 character source buffer
    char *dst,         // inout - pointer to a buffer to hold C string
    int *rc,           // out - return code
    int *slen,         // *hidden* in - length of the F90 source buffer
    int *dlen) {       // *hidden* in - max len of C dst buffer, inc term NULL
//EOPI

    char *cp;
    int clen;
    char msgbuf[ESMF_MAXSTR];

    // Initialize return code; assume routine not implemented
    if (rc) *rc = ESMC_RC_NOT_IMPL;

    // minor idiotproofing
    if ((src == NULL) || (src[0] == '\0') || (*slen <= 0) ||
        (dst == NULL) || (*dlen <= 0)) {
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad count or NULL pointer", rc);
            return;
    }

    // fortran doesn't need trailing null, so len can be up to == maxlen
    clen = strlen(src);
    if (clen > *dlen) {
       sprintf(msgbuf, 
             "dest buffer size of %d bytes too small, must be >= %d bytes\n", 
             *dlen, clen);
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, rc);
       return;
    }

    // move bytes, then pad rest of string to spaces
    strncpy(dst, src, clen);
    
    for (cp=&dst[clen]; cp < &dst[*dlen-1]; cp++)
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
                  *n, sizeof(n));
        *len = 0;
        return;
    }

    if (sizeof(n) != sizeof(ESMC_POINTER)) {
        printf("error: C pointer size does not match include file value\n");
        printf("  C pointer is %ld bytes, ESMC_POINTER is %ld bytes\n",  
                   sizeof(n), sizeof(ESMC_POINTER));
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
                  *n, sizeof(n));
        *len = 0;
        return;
    }

    if (sizeof(n) != sizeof(ESMC_POINTER)) {
        printf("error: C pointer size does not match include file value\n");
        printf("  C pointer is %ld bytes, ESMC_POINTER is %ld bytes\n",  
                   sizeof(n), sizeof(ESMC_POINTER));
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


