// $Id: ESMC_Base.C,v 1.19 2004/01/28 00:34:44 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC Base method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ Base methods declared
// in the companion file ESMC_Base.h
//
//-----------------------------------------------------------------------------
//
 // associated class definition file and others
#include <string.h>
#include <stdlib.h>
#include "ESMC.h"
#include "ESMC_Base.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Base.C,v 1.19 2004/01/28 00:34:44 nscollins Exp $";
//-----------------------------------------------------------------------------

// initialize class-wide instance counter
static int globalCount = 0;

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the ESMC_Base routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Read - virtual read-in contents of a Base class
//
// !INTERFACE:
      int ESMC_Base::ESMC_Read(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      base class provides stubs for optional Read/Write
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  return ESMF_SUCCESS;

 } // end ESMC_Read

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Write - virtual write-out contents of a Base class
//
// !INTERFACE:
      int ESMC_Base::ESMC_Write(void) const {
// 
// !RETURN VALUE:
//    int error return code
// 
// !ARGUMENTS:
//    const char *options) const {     //  in - print options
// 
// !DESCRIPTION:
//      base class provides stubs for optional Read/Write
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  return ESMF_SUCCESS;

} // end ESMC_Write

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Validate - virtual internal consistency check for a Base
//
// !INTERFACE:
      int ESMC_Base::ESMC_Validate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      base provides stub for required Validate method in derived classes
//      (must define for sub-classes on either C++ or F90 side)
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

  return ESMF_SUCCESS;

 } // end ESMC_Validate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Print - virutal print contents of a Base
//
// !INTERFACE:
      int ESMC_Base::ESMC_Print(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      base provides stub for required Validate method in derived classes
//      (must define for sub-classes on either C++ or F90 side)
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  int i;

  printf("Base object ID: %d, Ref count: %d, Status=%s, Name=%s\n", 
           ID, refCount, ESMC_StatusString(baseStatus), baseName);

  if (attrCount > 0) printf("  %d Attributes:\n", attrCount);
  for (i=0; i<attrCount; i++) {
      printf(" Attr %d: \n");
      // TODO:  add attr value print here
  }
                         
  return ESMF_SUCCESS;

 } // end ESMC_Print

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseGetInstCount - get number of base class instances
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseGetInstCount(void) const {
//
// !RETURN VALUE:
//    int instance count
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//     accessor to number of base class instances
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  return globalCount;

} // end ESMC_BaseGetInstCount

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseSetID - set Base class unique ID
//  
// !INTERFACE:
      void ESMC_Base::ESMC_BaseSetID(
//  
// !RETURN VALUE:
//    none
//  
// !ARGUMENTS:
      int id) {   // in - ID to set
//  
// !DESCRIPTION: 
//     override default ID (see constructor)
//  
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  ID = id;

}  // end ESMC_BaseSetID

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseGetID - get Base class unique ID
//  
// !INTERFACE:
      int ESMC_Base::ESMC_BaseGetID(void) const {
//  
// !RETURN VALUE:
//    int ID
//  
// !ARGUMENTS:
//    none
//  
// !DESCRIPTION:
//     accessor to unique ID
//  
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  return ID;
} // end ESMC_BaseGetID

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseSetRefCount - set Base class reference count
//
// !INTERFACE:
      void ESMC_Base::ESMC_BaseSetRefCount(
// 
// !RETURN VALUE:
//    none
// 
// !ARGUMENTS:
      int count) {
// 
// !DESCRIPTION:
//     accessor to reference count
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  refCount = count;

} // end ESMC_BaseSetRefCount

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseGetRefCount - get Base class reference count
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseGetRefCount(void) const {
//
// !RETURN VALUE:
//    int reference count
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//     accessor to reference count
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  return refCount;
} // end ESMC_BaseGetRefCount

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseSetStatus - set Base class status
//
// !INTERFACE:
      void ESMC_Base::ESMC_BaseSetStatus(
// 
// !RETURN VALUE:
//    none
// 
// !ARGUMENTS:
      ESMC_Status status) {   // in - base status to set
// 
// !DESCRIPTION:
//     accessor to base class status
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  baseStatus = status;

}  // end ESMC_BaseSetStatus
 
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseGetStatus - get Base class status
//
// !INTERFACE:
      ESMC_Status ESMC_Base::ESMC_BaseGetStatus(void) const {
// 
// !RETURN VALUE:
//    ESMC_Status
// 
// !ARGUMENTS:
//    none
// 
// !DESCRIPTION:
//     accessor to base class status
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  return baseStatus;

}  // end ESMC_BaseGetStatus

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseSetName - set Base class name
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseSetName(
// 
// !RETURN VALUE:
//    return code
// 
// !ARGUMENTS:
      char *name,           // in - base name to set
      char *classname) {    // in - context in which name should be unique
// 
// !DESCRIPTION:
//     accessor to base class name
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  int len;
 
  // no name, no context:  generate a name "globalXXX" where xxx is a seq num
  // no name, but a context: name is contextXXX with the seq num again
  // name given: use it as is

  // simple error checks first
  if (name) { 
     len = strlen(name);
     if (len >= ESMF_MAXSTR) {
       fprintf(stderr, "Error: object name %d bytes longer than limit of %d\n", 
                          len, ESMF_MAXSTR-1);
       return ESMF_FAILURE;
     }
  }

  if (classname) {
     len = strlen(classname);
     if (len >= ESMF_MAXSTR) {
       fprintf(stderr, "Error: object type %d bytes longer than limit of %d\n",
                          len, ESMF_MAXSTR-1);
       return ESMF_FAILURE;
     }
  }

  strcpy(className, classname ? classname : "global");
  if (!name) 
      sprintf(baseName, "%s%03d", className, ID); 
  else
      strcpy(baseName, name);

  ESMC_CtoF90string(baseName, baseNameF90, ESMF_MAXSTR);

  return ESMF_SUCCESS;

}  // end ESMC_BaseSetName
 
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseGetName - get Base class name
//
// !INTERFACE:
      char *ESMC_Base::ESMC_BaseGetName(void) const {
// 
// !RETURN VALUE:
//    char pointer to name
// 
// !ARGUMENTS:
//    none
// 
// !DESCRIPTION:
//     accessor to base class name
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  return (char * const)baseName;

}  // end ESMC_BaseGetName

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseGetF90Name - get Base class name
//
// !INTERFACE:
      char *ESMC_Base::ESMC_BaseGetF90Name(void) const {
// 
// !RETURN VALUE:
//    char pointer to name.  not null terminated, space filled.
// 
// !ARGUMENTS:
//    none
// 
// !DESCRIPTION:
//     accessor to base class name
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  return (char * const)baseNameF90;

}  // end ESMC_BaseGetF90Name

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseSetF90Name - set Base class name
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseSetF90Name(char *name, int nlen) {
// 
// !RETURN VALUE:
//    ESMF_SUCCESS or ESMF_FAILURE
// 
// !ARGUMENTS:
//    char pointer to name.  not null terminated, space filled.
// 
// !DESCRIPTION:
//     accessor to base class name
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  if (nlen > ESMF_MAXSTR) {
      fprintf(stderr, "string name %d bytes longer than limit of %d bytes\n",
                       nlen, ESMF_MAXSTR);
      return ESMF_FAILURE;
  }

  strncpy(baseNameF90, name, nlen);
  if (nlen < ESMF_MAXSTR) 
      memset(baseNameF90 + nlen, (int)' ', ESMF_MAXSTR-nlen);

  ESMC_F90toCstring(baseNameF90, ESMF_MAXSTR-1, baseName, ESMF_MAXSTR);
  return ESMF_SUCCESS;

}  // end ESMC_BaseSetF90Name


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseSetClassName - set Base class name
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseSetClassName(
// 
// !RETURN VALUE:
//    return code
// 
// !ARGUMENTS:
      char *classname) {    // in - context in which name should be unique
// 
// !DESCRIPTION:
//     accessor to base class name
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  int len;
 
  if (classname) {
     len = strlen(classname);
     if (len >= ESMF_MAXSTR) {
       fprintf(stderr, "Error: object type %d bytes longer than limit of %d\n",
                          len, ESMF_MAXSTR-1);
       return ESMF_FAILURE;
     }
  }

  strcpy(className, classname ? classname : "global");

  return ESMF_SUCCESS;

}  // end ESMC_BaseSetClassName
 
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseGetClassName - get Base class name
//
// !INTERFACE:
      char *ESMC_Base::ESMC_BaseGetClassName(void) const {
// 
// !RETURN VALUE:
//    char pointer to name
// 
// !ARGUMENTS:
//    none
// 
// !DESCRIPTION:
//     accessor to base class name
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  return (char * const)className;

}  // end ESMC_BaseGetClassName

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseGetF90ClassName - get Base class name
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseGetF90ClassName(char *name, int nlen) const {
// 
// !RETURN VALUE:
//    char pointer to name.  not null terminated, space filled.
// 
// !ARGUMENTS:
//    none
// 
// !DESCRIPTION:
//     accessor to base class name
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  // make the compiler happy:
  return ESMC_CtoF90string((char *)className, name, nlen);

}  // end ESMC_BaseGetF90ClassName

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseSetF90ClassName - set Base class name
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseSetF90ClassName(char *name, int nlen) {
// 
// !RETURN VALUE:
//    ESMF_SUCCESS or ESMF_FAILURE
// 
// !ARGUMENTS:
//    char pointer to name.  not null terminated, space filled.
// 
// !DESCRIPTION:
//     accessor to base class name
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  if (nlen > ESMF_MAXSTR) {
      fprintf(stderr, "string name %d bytes longer than limit of %d bytes\n",
                       nlen, ESMF_MAXSTR);
      return ESMF_FAILURE;
  }

  return ESMC_F90toCstring(name, nlen, className, ESMF_MAXSTR);

}  // end ESMC_BaseSetF90ClassName


//-----------------------------------------------------------------------------
// Misc Utility methods
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
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
//EOP

     if (ai == NULL) 
         return ESMF_FAILURE;

     ai->min = min;
     ai->max = max;
     ai->stride = stride;

     return ESMF_SUCCESS;
};

//-----------------------------------------------------------------------------
//BOP
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
//EOP

     if (ai == NULL) 
        return ESMF_FAILURE;

     if (min) *min = ai->min;
     if (max) *max = ai->max;
     if (stride) *stride = ai->stride;

     return ESMF_SUCCESS;
};

//-----------------------------------------------------------------------------
//BOP
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
//EOP

     if (ai == NULL) 
        printf("Empty (NULL) AxisIndex pointer\n");

     printf("min=%d, max=%d, stride=%d\n", ai->min, ai->max, ai->stride);

     return ESMF_SUCCESS;
};

//-----------------------------------------------------------------------------
//BOP
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
//EOP

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
    ESMC_DomainList *dp = this;
    FTN(f_esmf_domainlistgetde)(this, &domainnum, &de, &rc);
    return de;
}

struct ESMC_AxisIndex ESMC_DomainList::ESMC_DomainListGetAI(int domainnum, int ainum) {
    int rc;
    ESMC_DomainList *dp = this;
    struct ESMC_AxisIndex AI;
    FTN(f_esmf_domainlistgetai)(this, &domainnum, &ainum, &AI, &rc);
    return AI;
}
   

//-----------------------------------------------------------------------------
// General utility methods
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DataKindSize - Return number of bytes in a DataKind
//
// !INTERFACE:
    int ESMC_DataKindSize(
//
// !RETURN VALUE:
//  int number of bytes (negative for error)
// 
// !ARGUMENTS:
    ESMC_DataKind dk) {       // in - a data kind 

    switch (dk) {
      case ESMF_I1:  return  1;
      case ESMF_I2:  return  2;
      case ESMF_I4:  return  4;
      case ESMF_I8:  return  8;
      case ESMF_R4:  return  4;
      case ESMF_R8:  return  8;
      case ESMF_C8:  return  8;
      case ESMF_C16: return 16;
      default:
         fprintf(stderr, "Unknown DataKind in ESMC_DataKindSize()\n");
         return -1;
    }

    /* not reached */
}

//-----------------------------------------------------------------------------
//BOP
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

    switch (stat) {
      case ESMF_STATE_UNINIT:       return  "Uninitialized";
      case ESMF_STATE_READY:        return  "Ready";
      case ESMF_STATE_UNALLOCATED:  return  "Unallocated";
      case ESMF_STATE_ALLOCATED:    return  "Allocated";
      case ESMF_STATE_BUSY:         return  "Busy";
      case ESMF_STATE_INVALID:      return  "Invalid";
      default:
         fprintf(stderr, "Unknown Status in ESMC_StatusString()\n");
         return NULL;
    }

    /* not reached */
}

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//BOP
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

    char *cp, *ctmp;
    int clen;

    // minor idiotproofing
    if ((src == NULL) || (slen < 0) || (slen > ESMF_MAXSTR)) {
        printf("Bad input parameters: either bad count or NULL pointer\n");
        return NULL;
    }

    // count back from end of string to last non-blank character.
    for (clen=slen, cp = &src[slen-1]; *cp == ' '; cp--, clen--)
        ;

    // make new space and leave room for a null terminator
    ctmp = new char[clen+1];
    strncpy(ctmp, src, clen);
    ctmp[clen] = '\0';

    // return pointer.  caller MUST free this when finished with it.
    return ctmp;
}

//-----------------------------------------------------------------------------
//BOP
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

    char *cp, *ctmp;
    int clen;

    // minor idiotproofing
    if (  (slen < 0)    || (slen > ESMF_MAXSTR) 
       || (dlen < 0)    || (dlen > ESMF_MAXSTR)
       || (src == NULL) || (dst == NULL) ) {
        printf("Bad input parameters: either bad count or NULL pointer\n");
        return ESMF_FAILURE;
    }

    // count back from end of string to last non-blank character.
    for (clen=slen, cp = &src[slen-1]; *cp == ' '; cp--, clen--)
        ;

    // make sure dst space is long enough 
    if (clen >= dlen) {
        printf("dest buffer size of %d bytes too small, must be >= %d bytes\n", 
                dlen, clen+1);
        return ESMF_FAILURE;
    }
    
    strncpy(dst, src, clen);
    dst[clen] = '\0';

    // return ok.  caller has passed us in dst buffer so it is up to them
    // to manage that space.
    return ESMF_SUCCESS;
}

//-----------------------------------------------------------------------------
//BOP
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

    char *cp, *ctmp;
    int clen;

    // minor idiotproofing
    if (  (dlen < 0)    || (dlen > ESMF_MAXSTR)
       || (src == NULL) || (dst == NULL) ) {
        printf("Bad input parameters: either bad count or NULL pointer\n");
        return ESMF_FAILURE;
    }

    // fortran doesn't need trailing null, so len can be up to == maxlen
    clen = strlen(src);
    if (clen > dlen) {
        printf("dest buffer size of %d bytes too small, must be >= %d bytes\n", 
                dlen, clen);
        return ESMF_FAILURE;
    }

    // move bytes, then pad rest of string to spaces
    strncpy(dst, src, clen);
    
    for (cp=&dst[clen]; cp < &dst[dlen-1]; cp++)
        *cp = ' ';

    // return ok. 
    return ESMF_SUCCESS;
}

//-----------------------------------------------------------------------------
//BOP
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

    char *cp, *ctmp;
    int clen;

    // minor idiotproofing
    if (  (*slen < 0)   || (*slen > ESMF_MAXSTR) 
       || (*dlen < 0)   || (*dlen > ESMF_MAXSTR)
       || (src == NULL) || (dst == NULL) ) {
        printf("Bad input parameters: either bad count or NULL pointer\n");
        if (rc) *rc = ESMF_FAILURE;
        return;
    }

    // count back from end of string to last non-blank character.
    for (clen= *slen, cp = &src[*slen-1]; *cp == ' '; cp--, clen--)
        ;

    // make sure dst space is long enough 
    if (clen >= *dlen) {
        printf("dest buffer size of %d bytes too small, must be >= %d bytes\n", 
                *dlen, clen+1);
        if (rc) *rc = ESMF_FAILURE;
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
//BOP
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

    char *cp, *ctmp;
    int clen;

    // minor idiotproofing
    if (  (*slen < 0)   || (*slen > ESMF_MAXSTR) 
       || (*dlen < 0)   || (*dlen > ESMF_MAXSTR)
       || (src == NULL) || (dst == NULL) ) {
        printf("Bad input parameters: either bad count or NULL pointer\n");
        if (rc) *rc = ESMF_FAILURE;
        return;
    }

    // fortran doesn't need trailing null, so len can be up to == maxlen
    clen = strlen(src);
    if (clen > *dlen) {
        printf("dest buffer size of %d bytes too small, must be >= %d bytes\n", 
                *dlen, clen);
        if (rc) *rc = ESMF_FAILURE;
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
//-----------------------------------------------------------------------------
// Attribute methods
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  ESMC_AttributeAlloc - ensure the attribute list is long enough
//
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeAlloc(
//
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      int adding) {             // in - number of attributes being added
// 
// !DESCRIPTION:
//     Ensure there is enough space to add nattr more attributes.
//
//EOPI

#define ATTR_CHUNK  4           // allocate and realloc in units of this

  void *saveme;   // in case of error

  if ((attrCount + adding) <= attrAlloc) 
      return ESMF_SUCCESS;

  saveme = (void *)attr;
  attr = (ESMC_Attribute *)realloc((void *)attr, 
                           (attrAlloc + ATTR_CHUNK) * sizeof(ESMC_Attribute));
  if (attr == NULL) {
      free(saveme);   // although at this point, the heap is probably boffed
      return ESMF_FAILURE;
  }
  attrAlloc += ATTR_CHUNK;

  return ESMF_SUCCESS;

}  // end ESMC_AttributeAlloc

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeSet - set attribute on an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeSet(
//
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      ESMC_Attribute *attr) {   // in - attribute name, type, value
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any type in the system.
//
//EOP

  int i, rc;

  // simple sanity checks
  if ((!attr) || (!attr->attrName) || (attr->attrName[0] == '\0')) {
      printf("ESMF_AttributeSet: bad attribute object\n");
      return ESMF_FAILURE;
  }

  // first, see if you are replacing an existing attribute
  for (i=0; i<attrCount; i++) {
      if (strcmp(attr->attrName, attr[i].attrName))
          continue;

      // FIXME: we might want an explicit flag saying that this is what
      // is wanted, instead of an error if a previous value not expected.

      // if you get here, you found a match.  replace previous copy.
      // FIXME - if length > 1, you have to delete the pointer contents first
      // otherwise, memory leaks here.
      attr[i].attrValue = attr->attrValue;        // struct contents copy
      return ESMF_SUCCESS;
  }   

  // new attribute name, make sure there is space for it.
  rc = ESMC_AttributeAlloc(1);
  if (rc != ESMF_SUCCESS)
      return rc;
  
  strcpy(attr[attrCount].attrName, attr->attrName);
  attr[attrCount].attrValue = attr->attrValue;      // struct contents copy
  attrCount++;
  return ESMF_SUCCESS;

}  // end ESMC_AttributeSet

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeGet - get attribute from an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeGet(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      ESMC_Attribute *attr) const {    // in/out - name in, type & value out
// 
// !DESCRIPTION:
//
//EOP

  int rc, i;

  // simple sanity checks
  if ((!attr) || (!attr->attrName) || (attr->attrName[0] == '\0')) {
      printf("ESMF_AttributeGet: bad attribute object\n");
      return ESMF_FAILURE;
  }

  for (i=0; i<attrCount; i++) {
      if (strcmp(attr->attrName, attr[i].attrName))
          continue;

      // if you get here, you found a match.  struct contents copy.
      attr->attrValue = attr[i].attrValue;
      return ESMF_SUCCESS;
  }   

  // bad news - you get here if no matches found
  return ESMF_FAILURE;

}  // end ESMC_AttributeGet

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeGetCount - get an ESMF object's number of attributes
// 
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeGetCount(
// 
// !RETURN VALUE:
//    int attribute count
// 
// !ARGUMENTS:
      void) const {  
// 
// !DESCRIPTION:
//      Returns number of attributes present
//
//EOP
// !REQUIREMENTS:   FLD1.7.5

  
  return attrCount;

} // end ESMC_AttributeGetCount

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeGetbyNumber - get an ESMF object's attribute by number
//
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeGetbyNumber(
// 
// !RETURN VALUE:
//    int return code.  name must point to existing space long enough to
//    hold up to ESMF_MAXSTR bytes.
// 
// !ARGUMENTS:
      int number,                      // in - attribute number
      // FIXME: should this be passing ESMC_Attribute * here?
      char *name,                      // out - attribute name
      ESMC_DataType *type,             // out - attribute type
      ESMC_DataValue *value) const {   // out - attribute value
// 
// !DESCRIPTION:
//     Allows the caller to get attributes by number instead of by name.
//     This can be useful in iterating through all attributes in a loop.

//
//EOP
// !REQUIREMENTS:   


  int rc, i;

  // simple sanity check
  if ((number < 0) || (number >= attrCount)) {
      printf("ESMC_AttributeGetByNumber: attribute number must be  0 < N <= %d\n",
                                         attrCount-1);
      return ESMF_FAILURE;
  }

  if (name) strcpy(name, attr[number].attrName);
  if (type) *type = attr[number].attrValue.dt;
  if (value) *value = attr[number].attrValue;      // struct contents copy

  return ESMF_SUCCESS;

}  // end ESMC_AttributeGetbyNumber

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeGetNameList - get an ESMF object's attribute name list
//
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeGetNameList(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      int *count,               // out - number of attributes
      char **namelist) const {  // out - namelist
// 
// !DESCRIPTION:
//     Return a list of all attribute names without returning the values.
//
//EOP
// !REQUIREMENTS:   FLD1.7.3

  return ESMF_SUCCESS;

}  // end ESMC_AttributeGetNameList

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeSetList - set an ESMF object's attributes
// 
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeSetList(
// 
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
      char **namelist,          // in - list of attributes to set
      ESMC_DataValue *values) { // in - list of attribute values
// 
// !DESCRIPTION:
//    Set multiple attributes on an object in one call.  Depending on what is
//    allowed by the interface, all attributes may have to have the same type.
//
//EOP
// !REQUIREMENTS:   (none.  added for completeness)

  return ESMF_SUCCESS;

}  // end ESMC_AttributeSetList

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeGetList - get an ESMF object's attributes 
// 
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeGetList(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char **namelist,                   // out - list of attribute names
      ESMC_DataType *typelist,           // out - list of attribute types
      ESMC_DataValue *valuelist) const { // out - list of attribute values
// 
// !DESCRIPTION:
//     Get multiple attributes from an object in a single call
//
//EOP
// !REQUIREMENTS:   FLD1.7.4

  return ESMF_SUCCESS;

}  // end ESMC_AttributeGetList

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeCopy - copy an attribute between two objects
//
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeCopy(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,                 // in - attribute to copy
      ESMC_Base *destination) {   // in - the destination object
// 
// !DESCRIPTION:
//     The specified attribute associated with the source object (this) is
//     copied to the destination object.  << does this assume overwriting the
//     attribute if it already exists in the output or does this require yet
//     another arg to say what to do with collisions? >>

//EOP
// !REQUIREMENTS:   FLD1.5.4

  return ESMF_SUCCESS;

}  // end ESMC_AttributeCopy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeCopyAll - copy attributes between two objects 
//
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeCopyAll(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      ESMC_Base *destination) {  // in - the destination object
// 
// !DESCRIPTION:
//     All attributes associated with the source object (this) are copied to the
//     destination object.  Some attributes will have to be considered
//     {\tt read only} and won't be updated by this call.  (e.g. an attribute
//     like {\tt name} must be unique and therefore can't be duplicated.)

//EOP
// !REQUIREMENTS:   FLD1.5.4

  return ESMF_SUCCESS;

}  // end ESMC_AttributeCopyAll


//-----------------------------------------------------------------------------
// ESMC_Base class utility functions, not methods, since they operate on
//   multiple objects at once
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeSetObjectList - set an attribute on multiple ESMF objects
//
// !INTERFACE:
      int ESMC_AttributeSetObjectList(
//
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      ESMC_Base *anytypelist,    // in - list of ESMC objects
      char *name,                // in - attribute name
      ESMC_DataValue *value) {   // in - attribute value
// 
// !DESCRIPTION:
//     Set the same attribute on multiple objects in one call
//
//EOP
// !REQUIREMENTS:   FLD1.5.5 (pri 2)

  return ESMF_SUCCESS;

}  // end ESMC_AttributeSetObjectList

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeGetObjectList - get an attribute from multiple ESMF objects 
//
// !INTERFACE:
      int ESMC_AttributeGetObjectList(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      ESMC_Base *anytypelist,            // in - list of ESMC objects
      char *name,                        // in - attribute name
      ESMC_DataType *typelist,           // out - list of attribute types
      ESMC_DataValue *valuelist) {       // out - list of attribute values
// 
// !DESCRIPTION:
//     Get the same attribute name from multiple objects in one call
//
//EOP
// !REQUIREMENTS:   FLD1.5.5 (pri 2)

  return ESMF_SUCCESS;

}  // end ESMC_AttributeGetObjectList

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Base - native C++ constructor for ESMC_Base class
//
// !INTERFACE:
      ESMC_Base::ESMC_Base(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//     increment total number of instances; use for this instance's ID
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  attrCount = 0;
  attrAlloc = 0;
  attr = ESMC_NULL_POINTER;

  ID = ++globalCount;
  refCount = 1;
  strcpy(baseName, "unnamed");
  strcpy(className, "global");
  baseStatus = ESMF_STATE_READY;

 } // end ESMC_Base

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Base - native C++ constructor for ESMC_Base class
//
// !INTERFACE:
      ESMC_Base::ESMC_Base(char *superclass, char *name, int nattrs) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//   initialization with known class name, object name, initial number
//   of attributes to make space for.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  ID = ++globalCount;
  refCount = 1;
  strcpy(className, superclass ? superclass : "global");
  if (name)
      // TODO: make sure this name is unique in this namespace.  This means
      // some sort of registry utility.
      strcpy(baseName, name);
  else
      sprintf(baseName, "%s%3d", className, ID);

  attrCount = 0;
  attrAlloc = 0;
  attr = ESMC_NULL_POINTER;
  if (nattrs > 0) {
      if (ESMC_AttributeAlloc(nattrs) != ESMF_SUCCESS) {
          baseStatus = ESMF_STATE_INVALID;   // can't return err, but can
          return;                            // try to indicate unhappiness
      }
  }

  baseStatus = ESMF_STATE_READY;

 } // end ESMC_Base

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_Base - native C++ destructor for ESMC_Base class
//
// !INTERFACE:
      ESMC_Base::~ESMC_Base(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  baseStatus = ESMF_STATE_INVALID;
  if (attr) delete (attr);

  // if we have to support reference counts someday,
  // if (refCount > 0) do something;

 } // end ~ESMC_Base
