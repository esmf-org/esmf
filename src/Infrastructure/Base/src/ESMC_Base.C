// $Id: ESMC_Base.C,v 1.36 2004/04/23 21:53:13 nscollins Exp $
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
#include "ESMC_Start.h"
#include "ESMC_Base.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Base.C,v 1.36 2004/04/23 21:53:13 nscollins Exp $";
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

  if (baseStatus != ESMF_STATE_READY) 
    return ESMF_FAILURE;

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

  printf("Base object ID: %d, Ref count: %d, Status=%s, Name=%s, Class=%s\n", 
           ID, refCount, ESMC_StatusString(baseStatus), baseName, className);

  printf("  %d Attributes:\n", attrCount);
  for (i=0; i<attrCount; i++) {
      printf(" Attr %d: ", i);
      attrList[i]->ESMC_Print();
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
  int defname, defclass;
 
  // no name, no context:  generate a name "globalXXX" where xxx is a seq num
  // no name, but a context: name is contextXXX with the seq num again
  // name given: use it as is
  defname = 1;
  defclass = 1;

  // simple error checks first
  if (name && (name[0]!='\0')) { 
     len = strlen(name);
     if (len >= ESMF_MAXSTR) {
       fprintf(stderr, "Error: object name %d bytes longer than limit of %d\n", 
                          len, ESMF_MAXSTR-1);
       return ESMF_FAILURE;
     }
     defname = 0;
  } 

  if (classname && (classname[0]!='\0')) {
     len = strlen(classname);
     if (len >= ESMF_MAXSTR) {
       fprintf(stderr, "Error: object type %d bytes longer than limit of %d\n",
                          len, ESMF_MAXSTR-1);
       return ESMF_FAILURE;
     }
     defclass = 0;
  }

  strcpy(className, defclass ? "global" : classname);
  if (defname) 
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

  memcpy(baseNameF90, name, nlen);
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
//BOP
// !IROUTINE:  ESMC_DataTypeString - Return fixed char string for printing
//
// !INTERFACE:
    const char *ESMC_DataTypeString(
//
// !RETURN VALUE:
//  const char * to string name of value
// 
// !ARGUMENTS:
    ESMC_DataType dt) {       // in - a datatype value

    switch (dt) {
      case ESMF_DATA_INTEGER:      return  "Integer";
      case ESMF_DATA_REAL:         return  "Real";
      case ESMF_DATA_LOGICAL:      return  "Logical";
      case ESMF_DATA_CHARACTER:    return  "Character";
      default:
         fprintf(stderr, "Unknown DataType in ESMC_DataTypeString()\n");
         return NULL;
    }

    /* not reached */
}

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DataKindString - Return fixed char string for printing
//
// !INTERFACE:
    const char *ESMC_DataKindString(
//
// !RETURN VALUE:
//  const char * to string name of value
// 
// !ARGUMENTS:
    ESMC_DataKind dk) {       // in - a datakind value

    switch (dk) {
      case ESMF_I1:      return  "Integer*1";
      case ESMF_I2:      return  "Integer*2";
      case ESMF_I4:      return  "Integer*4";
      case ESMF_I8:      return  "Integer*8";
      case ESMF_R4:      return  "Real*4";
      case ESMF_R8:      return  "Real*8";
      case ESMF_C8:      return  "Complex*8";
      case ESMF_C16:     return  "Complex*16";
      default:
         fprintf(stderr, "Unknown DataKind in ESMC_DataKindString()\n");
         return NULL;
    }

    /* not reached */
}

//-----------------------------------------------------------------------------
//BOP
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

    switch (tf) {
      case ESMF_TRUE:      return  "True";
      case ESMF_FALSE:     return  "False";
      default:
         fprintf(stderr, "Unknown Logical in ESMC_LogicalString()\n");
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
    if ((src == NULL) || (src[0] == '\0') || (slen <= 0)) {
        printf("Bad input parameters: 1 either bad count or NULL pointer\n");
        return NULL;
    }

    // count back from end of string to last non-blank character.
    for (clen=slen, cp = &src[slen-1]; (*cp == ' ') && (clen > 0); cp--, clen--)
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
    if ((src == NULL) || (src[0] == '\0') || (slen <= 0) ||
        (dst == NULL) || (dlen <= 0)) {
        printf("Bad input parameters: 2 either bad count or NULL pointer\n");
        return ESMF_FAILURE;
    }

    // count back from end of string to last non-blank character.
    for (clen=slen, cp = &src[slen-1]; (*cp == ' ') && (clen > 0); cp--, clen--)
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
    if ((src == NULL) || (src[0] == '\0') || (dst == NULL) || (dlen <= 0)) {
        printf("Bad input parameters: 3 either bad count or NULL pointer\n");
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
    
    for (cp=&dst[clen]; cp < &dst[dlen]; cp++)
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
    if ((src == NULL) || (src[0] == '\0') || (*slen <= 0) ||
        (dst == NULL) || (*dlen <= 0)) {
        printf("Bad input parameters: 4 either bad count or NULL pointer\n");
        if (rc) *rc = ESMF_FAILURE;
        return;
    }

    // count back from end of string to last non-blank character.
    for (clen= *slen, cp = &src[*slen-1]; (*cp == ' ') && (clen > 0); cp--, clen--)
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
    if ((src == NULL) || (src[0] == '\0') || (*slen <= 0) ||
        (dst == NULL) || (*dlen <= 0)) {
        printf("Bad input parameters: 5 either bad count or NULL pointer\n");
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

  // FIXME: this should be arrays of *attrs, not whole size, right?
  saveme = (void *)attrList;
  attrList = (ESMC_Attribute **)realloc((void *)attrList, 
                           (attrAlloc + ATTR_CHUNK) * sizeof(ESMC_Attribute *));
  if (attrList == NULL) {
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
//     This version of set is used when the caller has already allocated
//     an attribute object and filled it, and the attribute is simply
//     added to the list belonging to this object.  The caller must not
//     delete the attribute.  Generally used internally - see below for
//     individual attribute set routines for each supported type.
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
      if (strcmp(attr->attrName, attrList[i]->attrName))
          continue;

      // FIXME: we might want an explicit flag saying that this is what
      // is wanted, instead of an error if a previous value not expected.

      // if you get here, you found a match.  replace previous copy.

      // delete old attribute, including possibly freeing a list
      attrList[i]->~ESMC_Attribute();

      attrList[i] = attr;
      return ESMF_SUCCESS;
  }   

  // new attribute name, make sure there is space for it.
  rc = ESMC_AttributeAlloc(1);
  if (rc != ESMF_SUCCESS)
      return rc;
  
  attrList[attrCount] = attr;   
  attrCount++;
  return ESMF_SUCCESS;

}  // end ESMC_AttributeSet

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeSet(int) - set attribute on an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeSet(
//
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,     // in - attribute name
      int value) {    // in - attribute value
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any type in the system.
//
//EOP

  int rc;
  ESMC_Attribute *attr;

  attr = new ESMC_Attribute(name, ESMF_DATA_INTEGER, 1, &value);  
  if (!attr)
    return ESMF_FAILURE;
 
  rc = ESMC_AttributeSet(attr);

  return rc;

}  // end ESMC_AttributeSet(int)

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeSet(int *) - set attribute on an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeSet(
//
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,     // in - attribute name
      int count,      // in - number of ints in list
      int *value) {   // in - attribute values
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any type in the system.
//
//EOP

  int rc;
  ESMC_Attribute *attr;

  attr = new ESMC_Attribute(name, ESMF_DATA_INTEGER, count, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  rc = ESMC_AttributeSet(attr);

  return rc;

}  // end ESMC_AttributeSet(int *)

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeSet(double) - set attribute on an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeSet(
//
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,        // in - attribute name
      double value) {    // in - attribute value
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any type in the system.
//
//EOP

  int rc;
  ESMC_Attribute *attr;

  attr = new ESMC_Attribute(name, ESMF_DATA_REAL, 1, &value);  
  if (!attr)
    return ESMF_FAILURE;
 
  rc = ESMC_AttributeSet(attr);

  return rc;

}  // end ESMC_AttributeSet(double)

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeSet(double *) - set attribute on an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeSet(
//
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,        // in - attribute name
      int count,         // in - number of doubles in list
      double *value) {   // in - attribute values
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any type in the system.
//
//EOP

  int rc;
  ESMC_Attribute *attr;

  attr = new ESMC_Attribute(name, ESMF_DATA_REAL, count, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  rc = ESMC_AttributeSet(attr);

  return rc;

}  // end ESMC_AttributeSet(double *)

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeSet(bool) - set attribute on an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeSet(
//
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,              // in - attribute name
      ESMC_Logical value) {    // in - attribute value
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any type in the system.
//
//EOP

  int rc;
  ESMC_Attribute *attr;

  attr = new ESMC_Attribute(name, ESMF_DATA_LOGICAL, 1, &value);  
  if (!attr)
    return ESMF_FAILURE;
 
  rc = ESMC_AttributeSet(attr);

  return rc;

}  // end ESMC_AttributeSet(bool)

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeSet(bool *) - set attribute on an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeSet(
//
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,              // in - attribute name
      int count,               // in - number of logicals in list
      ESMC_Logical *value) {   // in - attribute values
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any type in the system.
//
//EOP

  int rc;
  ESMC_Attribute *attr;

  attr = new ESMC_Attribute(name, ESMF_DATA_LOGICAL, count, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  rc = ESMC_AttributeSet(attr);

  return rc;

}  // end ESMC_AttributeSet(bool *)

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeSet(char) - set attribute on an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeSet(
//
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,       // in - attribute name
      char *value) {    // in - attribute value
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any type in the system.
//
//EOP

  int rc;
  ESMC_Attribute *attr;

  attr = new ESMC_Attribute(name, ESMF_DATA_CHARACTER, 1, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  rc = ESMC_AttributeSet(attr);

  return rc;

}  // end ESMC_AttributeSet(char)


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeSet(dt,count,value) - set attribute on an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeSet(
//
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,       // in - attribute name
      ESMC_DataType dt, // in - data type
      int count,        // in - number of values
      void *value) {    // in - attribute value
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any type in the system.
//
//EOP

  int rc;
  ESMC_Attribute *attr;

  attr = new ESMC_Attribute(name, dt, count, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  rc = ESMC_AttributeSet(attr);

  return rc;

}  // end ESMC_AttributeSet(dt,count,value)


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeGet - get attribute from an ESMF type
//
// !INTERFACE:
      ESMC_Attribute *ESMC_Base::ESMC_AttributeGet(
// 
// !RETURN VALUE:
//    pointer to requested attribute
// 
// !ARGUMENTS:
      char *name) const {        // in - attr name to retrieve
// 
// !DESCRIPTION:
//
//EOP

  int rc, i;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
      printf("ESMF_AttributeGet: 5 bad attribute name\n");
      return NULL;
  }

  for (i=0; i<attrCount; i++) {
      if (strcmp(name, attrList[i]->attrName))
          continue;

      // if you get here, you found a match. 
      return attrList[i]; 
  }   

  // bad news - you get here if no matches found
  return NULL;

}  // end ESMC_AttributeGet

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeGet(int) - get attribute from an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeGet(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,           // in - name of attribute to retrieve
      int *value) const {   // out - attribute value
// 
// !DESCRIPTION:
//
//EOP

  int rc, i;
  ESMC_Attribute *attr;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
      printf("ESMF_AttributeGet: 6 bad attribute name\n");
      return ESMF_FAILURE;
  }

  attr = ESMC_AttributeGet(name);
  if (!attr) {
      printf("ESMF_AttributeGet: attribute not found\n");
      return ESMF_FAILURE;
  }

  if (attr->dt != ESMF_DATA_INTEGER) {
      printf("ESMF_AttributeGet: attribute not type integer\n");
      return ESMF_FAILURE;
  }
  if (attr->items != 1) {
      printf("ESMF_AttributeGet: attribute not single value\n");
      return ESMF_FAILURE;
  }

  *value = attr->vi;
  return ESMF_SUCCESS;

}  // end ESMC_AttributeGet(int)

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeGet(int *) - get attribute from an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeGet(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,           // in - name of attribute to retrieve
      int *count,           // out - number of values in list
      int *value) const {   // out - attribute value
// 
// !DESCRIPTION:
//
//EOP

  int rc, i;
  ESMC_Attribute *attr;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
      printf("ESMF_AttributeGet: 7 bad attribute name\n");
      return ESMF_FAILURE;
  }

  attr = ESMC_AttributeGet(name);
  if (!attr) {
      printf("ESMF_AttributeGet: attribute not found\n");
      return ESMF_FAILURE;
  }

  if (attr->dt != ESMF_DATA_INTEGER) {
      printf("ESMF_AttributeGet: attribute not type integer\n");
      return ESMF_FAILURE;
  }

  if (count) 
      *count = attr->items;

  if (value) {
      if (attr->items == 1)
          value[0] = attr->vi;
      else for (i=0; i<attr->items; i++)
          value[i] = attr->vip[i];
  }

  return ESMF_SUCCESS;

}  // end ESMC_AttributeGet(int *)

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeGet(double) - get attribute from an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeGet(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,              // in - name of attribute to retrieve
      double *value) const {   // out - attribute value
// 
// !DESCRIPTION:
//
//EOP

  int rc, i;
  ESMC_Attribute *attr;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
      printf("ESMF_AttributeGet: 8 bad attribute name\n");
      return ESMF_FAILURE;
  }

  attr = ESMC_AttributeGet(name);
  if (!attr) {
      printf("ESMF_AttributeGet: attribute not found\n");
      return ESMF_FAILURE;
  }

  if (attr->dt != ESMF_DATA_REAL) {
      printf("ESMF_AttributeGet: attribute not type real\n");
      return ESMF_FAILURE;
  }
  if (attr->items != 1) {
      printf("ESMF_AttributeGet: attribute not single value\n");
      return ESMF_FAILURE;
  }

  *value = attr->vr;
  return ESMF_SUCCESS;

}  // end ESMC_AttributeGet(double)

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeGet(double *) - get attribute from an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeGet(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,              // in - name of attribute to retrieve
      int *count,              // out - number of values in list
      double *value) const {   // out - attribute value
// 
// !DESCRIPTION:
//
//EOP

  int rc, i;
  ESMC_Attribute *attr;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
      printf("ESMF_AttributeGet: 9 bad attribute name\n");
      return ESMF_FAILURE;
  }

  attr = ESMC_AttributeGet(name);
  if (!attr) {
      printf("ESMF_AttributeGet: attribute not found\n");
      return ESMF_FAILURE;
  }

  if (attr->dt != ESMF_DATA_REAL) {
      printf("ESMF_AttributeGet: attribute not type real\n");
      return ESMF_FAILURE;
  }

  if (count) 
      *count = attr->items;

  if (value) {
      if (attr->items == 1)
          value[0] = attr->vr;
      else for (i=0; i<attr->items; i++)
          value[i] = attr->vrp[i];
  }

  return ESMF_SUCCESS;

}  // end ESMC_AttributeGet(double *)

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeGet(bool) - get attribute from an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeGet(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,                    // in - name of attribute to retrieve
      ESMC_Logical *value) const {   // out - attribute value
// 
// !DESCRIPTION:
//
//EOP

  int rc, i;
  ESMC_Attribute *attr;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
      printf("ESMF_AttributeGet: 10 bad attribute name\n");
      return ESMF_FAILURE;
  }

  attr = ESMC_AttributeGet(name);
  if (!attr) {
      printf("ESMF_AttributeGet: attribute not found\n");
      return ESMF_FAILURE;
  }

  if (attr->dt != ESMF_DATA_LOGICAL) {
      printf("ESMF_AttributeGet: attribute not type logical\n");
      return ESMF_FAILURE;
  }
  if (attr->items != 1) {
      printf("ESMF_AttributeGet: attribute not single value\n");
      return ESMF_FAILURE;
  }

  *value = attr->vl;
  return ESMF_SUCCESS;

}  // end ESMC_AttributeGet(bool)


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeGet(bool *) - get attribute from an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeGet(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,                    // in - name of attribute to retrieve
      int *count,                    // out - number of values in list
      ESMC_Logical *value) const {   // out - attribute value
// 
// !DESCRIPTION:
//
//EOP

  int rc, i;
  ESMC_Attribute *attr;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
      printf("ESMF_AttributeGet: 1 bad attribute name\n");
      return ESMF_FAILURE;
  }

  attr = ESMC_AttributeGet(name);
  if (!attr) {
      printf("ESMF_AttributeGet: attribute not found\n");
      return ESMF_FAILURE;
  }

  if (attr->dt != ESMF_DATA_LOGICAL) {
      printf("ESMF_AttributeGet: attribute not type logical\n");
      return ESMF_FAILURE;
  }

  if (count) 
      *count = attr->items;

  if (value) {
      if (attr->items == 1)
          value[0] = attr->vl;
      else for (i=0; i<attr->items; i++)
          value[i] = attr->vlp[i];
  }

  return ESMF_SUCCESS;

}  // end ESMC_AttributeGet(bool *)

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeGet(char) - get attribute from an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeGet(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,            // in - name of attribute to retrieve
      char *value) const {   // out - attribute value
// 
// !DESCRIPTION:
//
//EOP

  int rc, i;
  ESMC_Attribute *attr;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
      printf("ESMF_AttributeGet: 2 bad attribute name\n");
      return ESMF_FAILURE;
  }
  if (!value) {
      printf("ESMF_AttributeGet: bad return location\n");
      return ESMF_FAILURE;
  }

  attr = ESMC_AttributeGet(name);
  if (!attr) {
      printf("ESMF_AttributeGet: attribute not found\n");
      return ESMF_FAILURE;
  }

  if (attr->dt != ESMF_DATA_CHARACTER) {
      printf("ESMF_AttributeGet: attribute not type character\n");
      return ESMF_FAILURE;
  }
  if (attr->items != 1) {
      printf("ESMF_AttributeGet: attribute not single value\n");
      return ESMF_FAILURE;
  }

  strcpy(value, attr->vcp);
  return ESMF_SUCCESS;

}  // end ESMC_AttributeGet(char)

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeGet(dt,count,value) - get attribute from an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeGet(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,                    // in - name of attribute to retrieve
      ESMC_DataType *dt,             // out - data type
      int *count,                    // out - number of values in list
      void *value) const {           // out - attribute value(s)
// 
// !DESCRIPTION:
//
//EOP

  int rc, i;
  ESMC_Attribute *attr;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
      printf("ESMF_AttributeGet: 3 bad attribute name\n");
      return ESMF_FAILURE;
  }

  attr = ESMC_AttributeGet(name);
  if (!attr) {
      printf("ESMF_AttributeGet: attribute not found\n");
      return ESMF_FAILURE;
  }

  if (dt) 
      *dt = attr->dt;

  if (count) {
      if (attr->dt == ESMF_DATA_CHARACTER)
          *count = attr->slen;
      else
          *count = attr->items;
  }

  if (value) {
      if (attr->items == 1) {
          switch(attr->dt) {
            case ESMF_DATA_INTEGER: 
              *(int *)value = attr->vi; 
              break;
            case ESMF_DATA_REAL: 
              *(double *)value = attr->vr; 
              break;
            case ESMF_DATA_LOGICAL: 
              *(ESMC_Logical *)value = attr->vl; 
              break;
            case ESMF_DATA_CHARACTER:
              printf("ESMF_AttributeGet: cannot return character string here\n");
              break;
            default:  
              printf("ESMF_AttributeGet: 1 unknown data type\n");
              return ESMF_FAILURE;
          }
 
      } else {
          switch(attr->dt) {
            case ESMF_DATA_INTEGER:
              for (i=0; i<attr->items; i++)
                  ((int *)value)[i] = attr->vip[i];
              break;
            case ESMF_DATA_REAL:
              for (i=0; i<attr->items; i++)
                  ((double *)value)[i] = attr->vrp[i];
              break;
            case ESMF_DATA_LOGICAL:
              for (i=0; i<attr->items; i++)
                  ((ESMC_Logical *)value)[i] = attr->vlp[i];
              break;
            default:  
              printf("ESMF_AttributeGet: 2 unknown data type\n");
              return ESMF_FAILURE;
          }
      }
  }

  return ESMF_SUCCESS;

}  // end ESMC_AttributeGet(dt,count,value)

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeGet(dt,count,value) - get attribute from an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeGet(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      int num,                       // in - number of attribute to retrieve
      char *name,                    // out - attribute name
      ESMC_DataType *dt,             // out - data type
      int *count,                    // out - number of values in list
      void *value) const {           // out - attribute value(s)
// 
// !DESCRIPTION:
//
//EOP

  int rc, i;
  ESMC_Attribute *attr;

  attr = ESMC_AttributeGet(num);
  if (!attr) {
      printf("ESMF_AttributeGet: attribute not found\n");
      return ESMF_FAILURE;
  }

  if (name)
       strcpy(name, attr->attrName);

  if (dt) 
      *dt = attr->dt;

  if (count) {
      if (attr->dt == ESMF_DATA_CHARACTER)
          *count = attr->slen;
      else
          *count = attr->items;
  }

  if (value) {
      if (attr->items == 1) {
          switch(attr->dt) {
            case ESMF_DATA_INTEGER: 
              *(int *)value = attr->vi; 
              break;
            case ESMF_DATA_REAL: 
              *(double *)value = attr->vr; 
              break;
            case ESMF_DATA_LOGICAL: 
              *(ESMC_Logical *)value = attr->vl; 
              break;
            case ESMF_DATA_CHARACTER:
              printf("ESMF_AttributeGet: cannot return character string here\n");
              break;
            default:  
              printf("ESMF_AttributeGet: 3 unknown data type\n");
              return ESMF_FAILURE;
          }
 
      } else {
          switch(attr->dt) {
            case ESMF_DATA_INTEGER:
              for (i=0; i<attr->items; i++)
                  ((int *)value)[i] = attr->vip[i];
              break;
            case ESMF_DATA_REAL:
              for (i=0; i<attr->items; i++)
                  ((double *)value)[i] = attr->vrp[i];
              break;
            case ESMF_DATA_LOGICAL:
              for (i=0; i<attr->items; i++)
                  ((ESMC_Logical *)value)[i] = attr->vlp[i];
              break;
            default:  
              printf("ESMF_AttributeGet: 4 unknown data type\n");
              return ESMF_FAILURE;
          }
      }
  }

  return ESMF_SUCCESS;

}  // end ESMC_AttributeGet(dt,count,value)


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
// !IROUTINE:  ESMC_AttributeGet - get an ESMF object's attribute by number
//
// !INTERFACE:
      ESMC_Attribute *ESMC_Base::ESMC_AttributeGet(
// 
// !RETURN VALUE:
//    int return code.
// 
// !ARGUMENTS:
      int number) const {             // in - attribute number
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
      printf("ESMC_AttributeGet: attribute number must be  0 < N <= %d\n",
                                         attrCount-1);
      return NULL;
  }

  return attrList[number];

}  // end ESMC_AttributeGet

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeGetNameList - get the list of attribute names
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
// !IROUTINE:  ESMC_AttributeSetList - set multiple attributes at once
// 
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeSetList(
// 
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
      int count,                   // in - number of attributes to set
      ESMC_Attribute *valuelist) { // in - list of attribute values
// 
// !DESCRIPTION:
//    Set multiple attributes on an object in one call. 
//
//EOP
// !REQUIREMENTS:   (none.  added for completeness)

  return ESMF_SUCCESS;

}  // end ESMC_AttributeSetList

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeGetList - get multiple attributes at once
// 
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeGetList(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char **namelist,                   // in - null term list of names
      ESMC_Attribute *valuelist) const { // out - list of attribute values
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
//     copied to the destination object. 

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
      ESMC_Base *source) {  // in - the source object
// 
// !DESCRIPTION:
//     All attributes associated with the source object are copied to the
//     destination object (this).  Some attributes might have to be considered
//     {\tt read only} and won't be updated by this call. 

//EOP
// !REQUIREMENTS:   FLD1.5.4

  return ESMF_SUCCESS;

}  // end ESMC_AttributeCopyAll

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Attribute::ESMC_Print - print the Attribute contents
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_Print(
//
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      void) {                    // could add options at some point
// 
// !DESCRIPTION:
//     Print the contents of a Attribute object
//
//EOP

  printf("name '%s', type %s", attrName, ESMC_DataTypeString(dt));

  if (items <= 0) 
      printf("\n");

  if (items == 1) {
      printf(", value: ");
      switch (dt) {
        case ESMF_DATA_INTEGER:   printf("%d\n", vi); break;
        case ESMF_DATA_REAL:      printf("%g\n", vr); break; 
        case ESMF_DATA_LOGICAL:   printf("%s\n", ESMC_LogicalString(vl)); break;
        case ESMF_DATA_CHARACTER: printf("%s\n", vcp); break;
        default:  printf(" unknown\n"); return ESMF_FAILURE;
      }
  }

  if (items > 1) { 
      printf(", %d items, values:\n", items);
      for (int i=0; i<items; i++) {
          switch (dt) {
            case ESMF_DATA_INTEGER: printf(" item %d: %d\n", i, vip[i]); break;
            case ESMF_DATA_REAL:    printf(" item %d: %g\n", i, vrp[i]); break; 
            case ESMF_DATA_LOGICAL: printf(" item %d: %s\n", 
                                          i, ESMC_LogicalString(vlp[i])); break;
            default:  printf(" unknown\n"); return ESMF_FAILURE;
          }
      }
  }

  return ESMF_SUCCESS;

}  // end ESMC_Print


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
      ESMC_Attribute *value) {   // in - attribute value
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
      ESMC_Attribute *valuelist) {       // out - list of attribute values
// 
// !DESCRIPTION:
//     Get the same attribute name from multiple objects in one call
//
//EOP
// !REQUIREMENTS:   FLD1.5.5 (pri 2)

  return ESMF_SUCCESS;

}  // end ESMC_AttributeGetObjectList

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeCopy(=) - assignment operator for attributes
//
// !INTERFACE:
      ESMC_Attribute& ESMC_Attribute::operator=(
//
// !RETURN VALUE:
//    new attribute object with allocations done if lists or char strings
//    must be stored.
//
// !ARGUMENTS:
        const ESMC_Attribute &source) {   // in - ESMC_Attribute
//
// !DESCRIPTION:
//   copy an attribute, including contents, to current object (this)
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n
  int i, len;

  memcpy(attrName, source.attrName, ESMF_MAXSTR);

  dt = source.dt;
  items = source.items;
  slen = source.slen;
  
  if (items == 0)
    voidp = NULL;
 
  else if (items == 1) {
    switch (dt) {
      case ESMF_DATA_INTEGER:   
        vi = source.vi;  
        break;
      case ESMF_DATA_REAL:      
        vr = source.vr;  
        break;
      case ESMF_DATA_LOGICAL:   
        vl = source.vl;  
        break;
      case ESMF_DATA_CHARACTER: 
        vcp = new char[slen];   // includes trailing null
        memcpy(vcp, (char *)source.vcp, slen);
        break;

      default:
        voidp = NULL;
        break;
    }

  } else {
    // items > 1, alloc space for a list and do the copy
      switch (dt) {
        case ESMF_DATA_INTEGER:   
          vip = new int[items];      
          for (i=0; i<items; i++)
              vip[i] = source.vip[i];  
          break;
        case ESMF_DATA_REAL:      
          vrp = new double[items];      
          for (i=0; i<items; i++)
              vrp[i] = source.vrp[i];  
          break;
        case ESMF_DATA_LOGICAL:   
          vlp = new ESMC_Logical[items];      
          for (i=0; i<items; i++)
              vlp[i] = source.vlp[i];  
          break;
        case ESMF_DATA_CHARACTER: 
        default:
          // error - arrays of char strings not allowed
          voidp = NULL;
          break;
      }
  }

  return (*this);

 } // end ESMC_Attribute::operator=

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Attribute - native C++ constructor for ESMC_Attribute class
//
// !INTERFACE:
      ESMC_Attribute::ESMC_Attribute(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//     create an empty attribute structure
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  attrName[0] = '\0';
  items = 0;
  slen = 0;
  voidp = NULL;

 } // end ESMC_Attribute

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Attribute - native C++ constructor for ESMC_Attribute class
//
// !INTERFACE:
      ESMC_Attribute::ESMC_Attribute(
//
// !RETURN VALUE:
//    new attribute object
//
// !ARGUMENTS:
        char *name,                // attribute name
        ESMC_DataType datatype,    // data type
        int numitems,              // single or list
        void *datap) {             // generic pointer to values
//
// !DESCRIPTION:
//   initialize an attribute, and make a copy of the data if items > 1
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n
  int i, len;

  if (!name)
      attrName[0] = '\0';
  else {
      len = strlen(name)+1;   // strlen doesn't count trailing null
      if (len > ESMF_MAXSTR) {
          printf("ERROR in ESMF_Attribute: attr name longer than %d\n",
                                                ESMF_MAXSTR);
          return; 
      }
      memcpy(attrName, name, len);
  }

  dt = datatype;
  items = numitems;
  slen = 0;          // only used for string values
  
  if (items == 0)
      voidp = NULL;
 
  else if (items == 1) {
      if (!datap) 
          voidp = NULL;
      else  {
        switch (dt) {
          case ESMF_DATA_INTEGER:   
            vi = *(int *)datap;  
            break;
          case ESMF_DATA_REAL:      
            vr = *(double *)datap;  
            break;
          case ESMF_DATA_LOGICAL:   
            vl = *(ESMC_Logical *)datap;  
            break;
          case ESMF_DATA_CHARACTER: 
            slen = strlen((char *)datap) + 1;
            vcp = new char[slen];
            strncpy(vcp, (char *)datap, slen);
            break;
    
          default:
            voidp = NULL;
            break;
        }
    }

  } else {
    // items > 1, alloc space for a list and do the copy
    switch (dt) {
      case ESMF_DATA_INTEGER:   
        vip = new int[items];      
        if (!datap) 
            break;
        for (i=0; i<items; i++)
            vip[i] = ((int *)datap)[i];  
        break;
      case ESMF_DATA_REAL:      
        vrp = new double[items];      
        if (!datap) 
            break;
        for (i=0; i<items; i++)
            vrp[i] = ((double *)datap)[i];  
        break;
      case ESMF_DATA_LOGICAL:   
        vlp = new ESMC_Logical[items];      
        if (!datap) 
            break;
        for (i=0; i<items; i++)
            vlp[i] = ((ESMC_Logical *)datap)[i];  
        break;
      case ESMF_DATA_CHARACTER: 
      default:
        // error - arrays of char strings not allowed
        voidp = NULL;
        break;
    }
  }

 } // end ESMC_Attribute

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_Attribute - native C++ destructor for ESMC_Attribute class
//
// !INTERFACE:
      ESMC_Attribute::~ESMC_Attribute(void) {
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

  if (dt == ESMF_DATA_CHARACTER) delete [] vcp;

  if (items > 1) {
    switch (dt) {
      case ESMF_DATA_INTEGER:   delete [] vip;  break;
      case ESMF_DATA_REAL:      delete [] vrp;  break;
      case ESMF_DATA_LOGICAL:   delete [] vlp;  break;
    }
  }

 } // end ~ESMC_Attribute

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
//   default initialization 
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  ID = ++globalCount;
  refCount = 1;
  strcpy(className, "global");
  sprintf(baseName, "%s%3d", "unnamed", ID);
  ESMC_CtoF90string(baseName, baseNameF90, ESMF_MAXSTR);

  attrCount = 0;
  attrAlloc = 0;
  attrList = ESMC_NULL_POINTER;

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
  if (name && (name[0]!='\0')) 
      // TODO: make sure this name is unique in this namespace.  This means
      // some sort of registry utility.
      strcpy(baseName, name);
  else
      sprintf(baseName, "%s%3d", className, ID);
  ESMC_CtoF90string(baseName, baseNameF90, ESMF_MAXSTR);

  attrCount = 0;
  attrAlloc = 0;
  attrList = ESMC_NULL_POINTER;
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
  int i;

  baseStatus = ESMF_STATE_INVALID;

  // if attribute lists, delete them.
  for (i=0; i<attrCount; i++) 
      attrList[i]->~ESMC_Attribute();
                         
  if (attrList) delete [] attrList;

  // if we have to support reference counts someday,
  // test if (refCount > 0) and do something if true;

 } // end ~ESMC_Base

