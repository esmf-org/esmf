// $Id: ESMC_Base.C,v 1.39 2004/06/10 22:45:00 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

#define ESMF_FILENAME "ESMC_Base.C"

// ESMC Base method implementation (body) file

// single blank line to make protex happy.
//BOP

//EOP
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
#include "ESMC_LogErr.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Base.C,v 1.39 2004/06/10 22:45:00 eschwab Exp $";
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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_BaseGetClassName"
//BOPI
// !IROUTINE:  ESMC_BaseGetClassName - Get Base class name
//
// !INTERFACE:
      char *ESMC_Base::ESMC_BaseGetClassName(
//
// !ARGUMENTS:
      void) const {
// 
// !RETURN VALUE:
//    Character pointer to class name.
// 
// !DESCRIPTION:
//    Accessor method for the class name of the object.
//
//EOPI

  return (char * const)className;

}  // end ESMC_BaseGetClassName

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_BaseGetF90ClassName"
//BOPI
// !IROUTINE:  ESMC_BaseGetF90ClassName - Get Base class name in Fortran format
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseGetF90ClassName(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
     char *name,         // in - Location to copy name into
     int nlen) const {   // in - Maximum length of string buffer
// 
// !DESCRIPTION:
//     Return a separate copy of the base class name, in Fortran friendly
//     format, which means not null terminated, and space filled.  
//     Will not copy more than {\tt nlen} bytes into {\tt name} string.
//
//EOPI

  // the (char *) cast is to try to make the compiler happy:
  return ESMC_CtoF90string((char *)className, name, nlen);

}  // end ESMC_BaseGetF90ClassName

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_BaseGetID"
//BOPI
// !IROUTINE:  ESMC_BaseGetID - Get Base class unique ID
//  
// !INTERFACE:
      int ESMC_Base::ESMC_BaseGetID(
// 
// !ARGUMENTS:
      void) const {
//  
// !RETURN VALUE:
//    Unique object ID.
//  
// !DESCRIPTION:
//    Returns the unique object ID.
//  
//EOPI

  return ID;

} // end ESMC_BaseGetID

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_BaseGetInstCount"
//BOPI
// !IROUTINE:  ESMC_BaseGetInstCount - Get number of Base class instances
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseGetInstCount(
// 
// !ARGUMENTS:
      void) const {
//
// !RETURN VALUE:
//    Integer instance count.
//
// !DESCRIPTION:
//    Return a count of how many instances of the {\tt ESMC_Base} class
//    have been instantiated.
//
//EOPI

  return globalCount;

} // end ESMC_BaseGetInstCount

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_BaseGetName"
//BOPI
// !IROUTINE:  ESMC_BaseGetName - Get Base object name
//
// !INTERFACE:
      char *ESMC_Base::ESMC_BaseGetName(
// 
// !ARGUMENTS:
      void) const {
// 
// !RETURN VALUE:
//    Character pointer to {\tt ESMC\_Base} name.
// 
// !DESCRIPTION:
//    Accessor method for the {\tt ESMC\_Base} name.
//
//EOPI

  return (char * const)baseName;

}  // end ESMC_BaseGetName

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_BaseGetF90Name"
//BOPI
// !IROUTINE:  ESMC_BaseGetF90Name - Get Base object name in Fortran format
//
// !INTERFACE:
      char *ESMC_Base::ESMC_BaseGetF90Name(
// 
// !ARGUMENTS:
      void) const {
// 
// !RETURN VALUE:
//     Pointer to object name, not null terminated and space filled.
// 
// !DESCRIPTION:
//     Accessor to base class name returned in Fortran friendly format, which
//     means not null terminated, and space filled.
//
//EOPI

  return (char * const)baseNameF90;

}  // end ESMC_BaseGetF90Name

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_BaseGetRefCount"
//BOPI
// !IROUTINE:  ESMC_BaseGetRefCount - Get Base class reference count
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseGetRefCount(
// 
// !ARGUMENTS:
      void) const {
//
// !RETURN VALUE:
//    Integer reference count.
//
// !DESCRIPTION:
//    Accessor method for base class reference count.
//
//EOPI

  return refCount;
} // end ESMC_BaseGetRefCount

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_BaseGetStatus"
//BOPI
// !IROUTINE:  ESMC_BaseGetStatus - Get Base class status
//
// !INTERFACE:
      ESMC_Status ESMC_Base::ESMC_BaseGetStatus(
// 
// !ARGUMENTS:
      void) const {
// 
// !RETURN VALUE:
//    {\tt ESMC\_Status} object containing the {\tt ESMC\_Base} status.
// 
// !DESCRIPTION:
//    Accessor method for base class status.
//
//EOPI

  return baseStatus;

}  // end ESMC_BaseGetStatus

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_BaseSetClassName"
//BOPI
// !IROUTINE:  ESMC_BaseSetClassName - Set Base class name
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseSetClassName(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *classname) {    // in - context in which name should be unique
// 
// !DESCRIPTION:
//    Accessor method to set base class name.
//
//EOPI

  int rc, len;
  char msgbuf[ESMF_MAXSTR];
 
  if (classname) {
     len = strlen(classname);
     if (len >= ESMF_MAXSTR) {
       sprintf(msgbuf, "Error: object type %d bytes longer than limit of %d\n",
                          len, ESMF_MAXSTR-1);
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &rc);
       return rc;
     }
  }

  strcpy(className, classname ? classname : "global");

  return ESMF_SUCCESS;

}  // end ESMC_BaseSetClassName
 
//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_BaseSetF90ClassName"
//BOPI
// !IROUTINE:  ESMC_BaseSetF90ClassName - Set Base class name
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseSetF90ClassName(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *name,      // in - contains name to set in fortran format
      int nlen) {      // in - length of the input name buffer
// 
// !DESCRIPTION:
//    Accessor method to set base class name.
//
//EOPI
  int rc;
  char msgbuf[ESMF_MAXSTR];

  if (nlen > ESMF_MAXSTR) {
       sprintf(msgbuf, "string name %d bytes longer than limit of %d bytes\n",
                       nlen, ESMF_MAXSTR);
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &rc);
       return rc;
  }

  return ESMC_F90toCstring(name, nlen, className, ESMF_MAXSTR);

}  // end ESMC_BaseSetF90ClassName

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_BaseSetID"
//BOPI
// !IROUTINE:  ESMC_BaseSetID - Set Base class unique ID
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
//EOPI

  ID = id;

}  // end ESMC_BaseSetID

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_BaseSetName"
//BOPI
// !IROUTINE:  ESMC_BaseSetName - Set Base class name
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseSetName(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *name,           // in - base name to set
      char *classname) {    // in - context in which name should be unique
// 
// !DESCRIPTION:
//     Accessor method for base class name.
//
//EOPI

  int len, rc;
  int defname, defclass;
  char msgbuf[ESMF_MAXSTR];
 
  // no name, no context:  generate a name "globalXXX" where xxx is a seq num
  // no name, but a context: name is contextXXX with the seq num again
  // name given: use it as is
  defname = 1;
  defclass = 1;

  // simple error checks first
  if (name && (name[0]!='\0')) { 
     len = strlen(name);
     if (len >= ESMF_MAXSTR) {
       sprintf(msgbuf, "object name %d bytes longer than limit of %d bytes\n",
                       len, ESMF_MAXSTR-1);
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &rc);
       return rc;
     }
     defname = 0;
  } 

  if (classname && (classname[0]!='\0')) {
     len = strlen(classname);
     if (len >= ESMF_MAXSTR) {
       sprintf(msgbuf, "object type %d bytes longer than limit of %d bytes\n",
                       len, ESMF_MAXSTR-1);
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &rc);
       return rc;
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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_BaseSetF90Name"
//BOPI
// !IROUTINE:  ESMC_BaseSetF90Name - Set Base class name
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseSetF90Name(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *name,     // in - class name to set, in fortran format
      int nlen) {     // in - length of class name buffer
// 
// !DESCRIPTION:
//     Accessor method to set base class name.
//
//EOPI
  int rc;
  char msgbuf[ESMF_MAXSTR];

  if (nlen > ESMF_MAXSTR) {
       sprintf(msgbuf, "string name %d bytes longer than limit of %d bytes\n",
                       nlen, ESMF_MAXSTR);
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &rc);
       return rc;
  }

  memcpy(baseNameF90, name, nlen);
  if (nlen < ESMF_MAXSTR) 
      memset(baseNameF90 + nlen, (int)' ', ESMF_MAXSTR-nlen);

  ESMC_F90toCstring(baseNameF90, ESMF_MAXSTR-1, baseName, ESMF_MAXSTR);
  return ESMF_SUCCESS;

}  // end ESMC_BaseSetF90Name


//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_BaseSetRefCount"
//BOPI
// !IROUTINE:  ESMC_BaseSetRefCount - Set Base class reference count
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
//     Accessor method for reference count.
//
//EOPI

  refCount = count;

} // end ESMC_BaseSetRefCount

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_BaseSetStatus"
//BOPI
// !IROUTINE:  ESMC_BaseSetStatus - Set Base class status
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
//     Accessor method for base class status.
//
//EOPI

  baseStatus = status;

}  // end ESMC_BaseSetStatus
 

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_Print"
//BOPI
// !IROUTINE:  ESMC_Print - Print contents of a Base object
//
// !INTERFACE:
      int ESMC_Base::ESMC_Print(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//    Print the contents of an {\tt ESMC\_Base} object.  Expected to be
//    called internally from the object-specific print routines.
//
//EOPI

  int i;
  char msgbuf[ESMF_MAXSTR];

  sprintf(msgbuf,
       "Base object ID: %d, Ref count: %d, Status=%s, Name=%s, Class=%s\n", 
           ID, refCount, ESMC_StatusString(baseStatus), baseName, className);
  printf(msgbuf);
    // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
  

  sprintf(msgbuf, "  %d Attributes:\n", attrCount);
  printf(msgbuf);
    // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
  for (i=0; i<attrCount; i++) {
      sprintf(msgbuf, " Attr %d: ", i);
      printf(msgbuf);
        // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
      attrList[i]->ESMC_Print();
  }
                         
  return ESMF_SUCCESS;

 } // end ESMC_Print

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_Read"
//BOPI
// !IROUTINE:  ESMC_Read - Read in contents of a Base object
//
// !INTERFACE:
      int ESMC_Base::ESMC_Read(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      void) {
//
// !DESCRIPTION:
//    Base class provides stubs for optional read/write methods.
//
//EOPI

  return ESMF_SUCCESS;

 } // end ESMC_Read

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_Validate"
//BOPI
// !IROUTINE:  ESMC_Validate - Internal consistency check for Base object
//
// !INTERFACE:
      int ESMC_Base::ESMC_Validate(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//     Validation of the {\tt ESMC\_Base} object.  Expected to be called
//     internally from the object-specific validation methods.
//
//EOPI

  if (baseStatus != ESMF_STATE_READY) 
    return ESMF_FAILURE;

  return ESMF_SUCCESS;

 } // end ESMC_Validate


//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_Write"
//BOP
// !IROUTINE:  ESMC_Write - Write out contents of a Base object
//
// !INTERFACE:
      int ESMC_Base::ESMC_Write(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      void) const {
// 
// !DESCRIPTION:
//    Base class provides stubs for optional read/write methods.
//
//EOP

  return ESMF_SUCCESS;

} // end ESMC_Write

//-----------------------------------------------------------------------------
// Misc Utility methods
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_AxisIndexSet"
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

     if (ai == NULL) 
         return ESMF_FAILURE;

     ai->min = min;
     ai->max = max;
     ai->stride = stride;

     return ESMF_SUCCESS;
};

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_AxisIndexGet"
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

     if (ai == NULL) 
        return ESMF_FAILURE;

     if (min) *min = ai->min;
     if (max) *max = ai->max;
     if (stride) *stride = ai->stride;

     return ESMF_SUCCESS;
};

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_AxisIndexPrint"
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

     if (ai == NULL) 
        ESMC_LogDefault.ESMC_LogWrite("Empty (NULL) AxisIndex pointer", 
                                       ESMC_LOG_INFO);

     sprintf(msgbuf, "min=%d, max=%d, stride=%d\n", ai->min, ai->max, ai->stride);
     ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);

     return ESMF_SUCCESS;
};

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_AxisIndexEqual"
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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_DataKindSize"
//BOPI
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
//EOPI

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
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                                "Unknown DataKind", NULL);
       return -1;
    }

    /* not reached */
}

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_StatusString"
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
      case ESMF_STATE_UNINIT:       return  "Uninitialized";
      case ESMF_STATE_READY:        return  "Ready";
      case ESMF_STATE_UNALLOCATED:  return  "Unallocated";
      case ESMF_STATE_ALLOCATED:    return  "Allocated";
      case ESMF_STATE_BUSY:         return  "Busy";
      case ESMF_STATE_INVALID:      return  "Invalid";
      default:
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                                      "Unknown Status", NULL);
       return NULL;
    }

    /* not reached */
}

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_DataTypeString"
//BOPI
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
//EOPI

    switch (dt) {
      case ESMF_DATA_INTEGER:      return  "Integer";
      case ESMF_DATA_REAL:         return  "Real";
      case ESMF_DATA_LOGICAL:      return  "Logical";
      case ESMF_DATA_CHARACTER:    return  "Character";
      default:
         ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                                     "Unknown DataType", NULL);
         return NULL;
    }

    /* not reached */
}

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_DataKindString"
//BOPI
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
//EOPI

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
         ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                                     "Unknown DataKind", NULL);
         return NULL;
    }

    /* not reached */
}

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_LogicalString"
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
                                     "Unknown DataKind", NULL);
         return NULL;
    }

    /* not reached */
}

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_F90toCstring"
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

    // minor idiotproofing
    if ((src == NULL) || (src[0] == '\0') || (slen <= 0)) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                            "bad count or NULL pointer", NULL);
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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_F90toCstring"
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

    char *cp, *ctmp;
    int clen, rc;
    char msgbuf[ESMF_MAXSTR];

    // minor idiotproofing
    if ((src == NULL) || (src[0] == '\0') || (slen <= 0) ||
        (dst == NULL) || (dlen <= 0)) {
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad count or NULL pointer", &rc);
            return rc;
    }

    // count back from end of string to last non-blank character.
    for (clen=slen, cp = &src[slen-1]; (*cp == ' ') && (clen > 0); cp--, clen--)
        ;

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
    return ESMF_SUCCESS;
}

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_CtoF90string"
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

    char *cp, *ctmp;
    int clen, rc;
    char msgbuf[ESMF_MAXSTR];

    // minor idiotproofing
    if ((src == NULL) || (src[0] == '\0') || (dst == NULL) || (dlen <= 0)) {
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
    return ESMF_SUCCESS;
}

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "esmf_f90tostring"
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

    char *cp, *ctmp;
    int clen;
    char msgbuf[ESMF_MAXSTR];

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
#undef  ESMF_METHOD
#define ESMF_METHOD "esmf_ctof90string"
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

    char *cp, *ctmp;
    int clen;
    char msgbuf[ESMF_MAXSTR];

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
//-----------------------------------------------------------------------------
// Attribute methods
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_AttributeAlloc"
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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_AttributeSet"
//BOPI
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
//EOPI

  int i, rc;

  // simple sanity checks
  if ((!attr) || (!attr->attrName) || (attr->attrName[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad attribute object", &rc);
       return rc;
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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_AttributeSet"
//BOPI
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
//EOPI

  int rc;
  ESMC_Attribute *attr;

  attr = new ESMC_Attribute(name, ESMF_DATA_INTEGER, 1, &value);  
  if (!attr)
    return ESMF_FAILURE;
 
  rc = ESMC_AttributeSet(attr);

  return rc;

}  // end ESMC_AttributeSet(int)

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_AttributeSet"
//BOPI
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
//EOPI

  int rc;
  ESMC_Attribute *attr;

  attr = new ESMC_Attribute(name, ESMF_DATA_INTEGER, count, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  rc = ESMC_AttributeSet(attr);

  return rc;

}  // end ESMC_AttributeSet(int *)

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_AttributeSet"
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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_AttributeSet"
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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_AttributeSet"
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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_AttributeSet"
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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_AttributeSet"
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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_AttributeSet"
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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_AttributeGet"
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

  int i;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                               "bad attribute name", NULL);
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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_AttributeGet"
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
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad attribute name", &rc);
       return rc;
  }

  attr = ESMC_AttributeGet(name);
  if (!attr) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not found", &rc);
       return rc;
  }

  if (attr->dt != ESMF_DATA_INTEGER) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not type integer", &rc);
       return rc;
  }
  if (attr->items != 1) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not single value", &rc);
       return rc;
  }

  *value = attr->vi;
  return ESMF_SUCCESS;

}  // end ESMC_AttributeGet(int)

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_AttributeGet"
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
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad attribute name", &rc);
       return rc;
  }

  attr = ESMC_AttributeGet(name);
  if (!attr) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not found", &rc);
       return rc;
  }

  if (attr->dt != ESMF_DATA_INTEGER) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not type integer", &rc);
       return rc;
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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_AttributeGet"
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
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad attribute name", &rc);
       return rc;
  }

  attr = ESMC_AttributeGet(name);
  if (!attr) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not found", &rc);
       return rc;
  }

  if (attr->dt != ESMF_DATA_REAL) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not type integer", &rc);
       return rc;
  }
  if (attr->items != 1) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not single value", &rc);
       return rc;
  }

  *value = attr->vr;
  return ESMF_SUCCESS;

}  // end ESMC_AttributeGet(double)

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_AttributeGet"
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
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad attribute name", &rc);
       return rc;
  }

  attr = ESMC_AttributeGet(name);
  if (!attr) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not found", &rc);
       return rc;
  }

  if (attr->dt != ESMF_DATA_REAL) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not type integer", &rc);
       return rc;
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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_AttributeGet"
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
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad attribute name", &rc);
       return rc;
  }

  attr = ESMC_AttributeGet(name);
  if (!attr) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not found", &rc);
       return rc;
  }

  if (attr->dt != ESMF_DATA_LOGICAL) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not type integer", &rc);
       return rc;
  }
  if (attr->items != 1) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not single value", &rc);
       return rc;
  }

  *value = attr->vl;
  return ESMF_SUCCESS;

}  // end ESMC_AttributeGet(bool)


//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_AttributeGet"
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
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad attribute name", &rc);
       return rc;
  }

  attr = ESMC_AttributeGet(name);
  if (!attr) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not found", &rc);
       return rc;
  }

  if (attr->dt != ESMF_DATA_LOGICAL) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not type integer", &rc);
       return rc;
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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_AttributeGet"
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
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad attribute name", &rc);
       return rc;
  }
  if (!value) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad value return argument", &rc);
      return ESMF_FAILURE;
  }

  attr = ESMC_AttributeGet(name);
  if (!attr) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not found", &rc);
       return rc;
  }

  if (attr->dt != ESMF_DATA_CHARACTER) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not type integer", &rc);
       return rc;
  }
  if (attr->items != 1) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not single value", &rc);
       return rc;
  }

  strcpy(value, attr->vcp);
  return ESMF_SUCCESS;

}  // end ESMC_AttributeGet(char)

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_AttributeGet"
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
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                             "bad attribute name", &rc);
       return rc;
  }

  attr = ESMC_AttributeGet(name);
  if (!attr) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                             "attribute not found", &rc);
       return rc;
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
              ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                                       "cannot return character string here", 
                                       &rc);
              return rc;
            default:  
              ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                                       "unknown data type", 
                                       &rc);
              return rc;
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
              ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                                       "unknown data type", 
                                       &rc);
              return rc;
          }
      }
  }

  return ESMF_SUCCESS;

}  // end ESMC_AttributeGet(dt,count,value)

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_AttributeGet"
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
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                             "attribute not found", &rc);
       return rc;
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
              ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                                       "cannot return character string here", 
                                       &rc);
              return rc;
            default:  
              ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                                       "unknown data type", 
                                       &rc);
              return rc;
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
              ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                                       "unknown data type", 
                                       &rc);
              return rc;
          }
      }
  }

  return ESMF_SUCCESS;

}  // end ESMC_AttributeGet(dt,count,value)


//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_AttributeGetCount"
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

  
  return attrCount;

} // end ESMC_AttributeGetCount

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_AttributeGet"
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

  int i;
  char msgbuf[ESMF_MAXSTR];

  // simple sanity check
  if ((number < 0) || (number >= attrCount)) {
      sprintf(msgbuf, "attribute number must be  0 < N <= %d\n", attrCount-1);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, NULL);
      return NULL;
  }

  return attrList[number];

}  // end ESMC_AttributeGet

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_AttributeGetNameList"
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

  return ESMF_SUCCESS;

}  // end ESMC_AttributeGetNameList

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_AttributeSetList"
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

  return ESMF_SUCCESS;

}  // end ESMC_AttributeSetList

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_AttributeGetList"
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

  return ESMF_SUCCESS;

}  // end ESMC_AttributeGetList

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_AttributeCopy"
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

  return ESMF_SUCCESS;

}  // end ESMC_AttributeCopy

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_AttributeCopyAll"
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

  return ESMF_SUCCESS;

}  // end ESMC_AttributeCopyAll

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_Print"
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
  int rc;
  char msgbuf[ESMF_MAXSTR];

  sprintf(msgbuf, "name '%s', type %s", attrName, ESMC_DataTypeString(dt));
  ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);

  if (items <= 0) 
      ESMC_LogDefault.ESMC_LogWrite("\n", ESMC_LOG_INFO);

  if (items == 1) {
      ESMC_LogDefault.ESMC_LogWrite(", value: ", ESMC_LOG_INFO);
      switch (dt) {
        case ESMF_DATA_INTEGER:   sprintf(msgbuf, "%d\n", vi); break;
        case ESMF_DATA_REAL:      sprintf(msgbuf, "%g\n", vr); break; 
        case ESMF_DATA_LOGICAL:   sprintf(msgbuf, "%s\n", ESMC_LogicalString(vl)); break;
        case ESMF_DATA_CHARACTER: sprintf(msgbuf, "%s\n", vcp); break;
        default:  
             ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                             "unknown value", &rc);
             return rc;
      }
      ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
  }

  if (items > 1) { 
      sprintf(msgbuf, ", %d items, values:\n", items);
      ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
      for (int i=0; i<items; i++) {
          switch (dt) {
            case ESMF_DATA_INTEGER: sprintf(msgbuf, " item %d: %d\n", i, vip[i]); break;
            case ESMF_DATA_REAL:    sprintf(msgbuf, " item %d: %g\n", i, vrp[i]); break; 
            case ESMF_DATA_LOGICAL: sprintf(msgbuf, " item %d: %s\n", 
                                          i, ESMC_LogicalString(vlp[i])); break;
            default: 
             ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                             "unknown value", &rc);
             return rc;
          }
      }
      ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
  }

  return ESMF_SUCCESS;

}  // end ESMC_Print


//-----------------------------------------------------------------------------
// ESMC_Base class utility functions, not methods, since they operate on
//   multiple objects at once
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_AttributeSetObjectList"
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

  return ESMF_SUCCESS;

}  // end ESMC_AttributeSetObjectList

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_AttributeGetObjectList"
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

  return ESMF_SUCCESS;

}  // end ESMC_AttributeGetObjectList

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_AttributeCopy(=)"
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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_Attribute()"
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

  attrName[0] = '\0';
  items = 0;
  slen = 0;
  voidp = NULL;

 } // end ESMC_Attribute

//-----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_Attribute()"
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
  int i, len, rc;
  char msgbuf[ESMF_MAXSTR];

  if (!name)
      attrName[0] = '\0';
  else {
      len = strlen(name)+1;   // strlen doesn't count trailing null
      if (len > ESMF_MAXSTR) {
        sprintf(msgbuf, "attr name %d bytes longer than limit of %d bytes\n",
                       len, ESMF_MAXSTR);
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &rc);
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
#undef  ESMF_METHOD
#define ESMF_METHOD "~ESMC_Attribute()"
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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_Base()"
//BOPI
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
//EOPI

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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMC_Base()"
//BOPI
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
//EOPI

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
#undef  ESMF_METHOD
#define ESMF_METHOD "~ESMC_Base()"
//BOPI
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
//EOPI
  int i;

  baseStatus = ESMF_STATE_INVALID;

  // if attribute lists, delete them.
  for (i=0; i<attrCount; i++) 
      attrList[i]->~ESMC_Attribute();
                         
  if (attrList) delete [] attrList;

  // if we have to support reference counts someday,
  // test if (refCount > 0) and do something if true;

 } // end ~ESMC_Base

