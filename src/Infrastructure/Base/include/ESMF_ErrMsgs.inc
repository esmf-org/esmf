#if 0
$Id: ESMF_ErrMsgs.inc,v 1.2 2004/05/14 09:23:33 nscollins Exp $

Earth System Modeling Framework
Copyright 2002-2003, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the GPL.

Do not use either C++ or F90 style comments here because the contents of
this file must be understood by both C++ and F90 compilers.
#endif

#if 0
The following 2 lines prevent this file from being read more than once 
if it is included multiple times.
#endif

#ifndef ESMF_ERRMSGS_INC
#define ESMF_ERRMSGS_INC

#if 0
General error message strings, as generic as possible.  Additional strings
can be concatentated in fortran with the // operator; works for any character
variables. The trim() function may be needed to suppress trailing blanks.

In C++ concatentation can be done by simply putting literal strings 
"a" "b" next to each other, e.g.  ESMF_ERR_BAD_OBJ ": FieldDataMap" 
turns into the single string "Invalid object: FieldDataMap"
(In C++ this cannot be used with variables; use sprintf() into a temp buffer
to do more elaborate formatting.)
#endif

#define ESMF_ERR_OBJ_OBJ        "Invalid object"
#define ESMF_ERR_OBJ_INIT       "Object SetDefault method not called"
#define ESMF_ERR_OBJ_CREATE     "Object Create method not called"
#define ESMF_ERR_OBJ_COR        "Corrupted ESMF object detected"
#define ESMF_ERR_OBJ_WRONGSTATE "Object is in wrong state"

#define ESMF_ERR_ARG_BAD        "Invalid argument"
#define ESMF_ERR_ARG_RANK       "Argument rank is not required size"
#define ESMF_ERR_ARG_SIZE       "Argument sizes do not match"
#define ESMF_ERR_ARG_VALUE      "Value unrecognized or out of range"
#define ESMF_ERR_ARG_DUP        "Two arguments not allowed to be the same"
#define ESMF_ERR_ARG_SAMETYPE   "Two arguments must be same object type"
#define ESMF_ERR_ARG_SAMECOMM   "Two arguments must have the same communicators"
#define ESMF_ERR_ARG_INCOMP     "Arguments are incompatible"
#define ESMF_ERR_ARG_CORRUPT    "Argument contains invalid ESMF object"
#define ESMF_ERR_ARG_WRONG      "Wrong argument specified"
#define ESMF_ERR_ARG_OUTOFRANGE "Input argument out of range"
#define ESMF_ERR_ARG_OPT        "Unrecognized option string"

#define ESMF_ERR_NOT_IMPL       "Operation not yet supported"

#define ESMF_ERR_FILE_OPEN       "Unable to open file"
#define ESMF_ERR_FILE_CREATE     "Unable to open file"
#define ESMF_ERR_FILE_READ       "Unable to read from file"
#define ESMF_ERR_FILE_WRITE      "Unable to write to file"
#define ESMF_ERR_FILE_UNEXPECTED "Unexpected data in file"
#define ESMF_ERR_FILE_CLOSE      "Unable to close file"
#define ESMF_ERR_FILE_ACTIVE     "Instrumented region is still active"

#define ESMF_ERR_PTR_NULL       "Value cannot be a NULL pointer"
#define ESMF_ERR_PTR_BAD        "Invalid pointer"
#define ESMF_ERR_PTR_NOTALLOC   "Pointer must already be allocated"
#define ESMF_ERR_PTR_ISALLOC    "Pointer must not already be allocated"
#define ESMF_ERR_MEM            "Unable to allocate requested memory"
#define ESMF_ERR_MEMC           "Memory corrupted"

#define ESMF_ERR_DUP_NAME       "Name already exists"
#define ESMF_ERR_LONG_NAME      "Name too long, must be less than ESMF_MAXSTR"
#define ESMF_ERR_LONG_STR       "String too long, must be less than ESMF_MAXSTR"
#define ESMF_ERR_COPY_FAIL      "Cannot copy non-existent object"
#define ESMF_ERR_DIV_ZERO       "Cannot divide by zero"
#define ESMF_ERR_CANNOT_GET     "Cannot get value"
#define ESMF_ERR_CANNOT_SET     "Cannot set value"
#define ESMF_ERR_NOT_FOUND      "Not found"
#define ESMF_ERR_NOT_VALID      "Not valid" 

#define ESMF_ERR_INTNRL_LIST    "Internal error: List overflow"
#define ESMF_ERR_INTNRL_INCONS  "Internal error: Inconsistent data"
#define ESMF_ERR_INTNRL_BAD     "Internal error: Unknown error"

#define ESMF_ERR_SYS            "System call error"
#define ESMF_ERR_BUSY           "Resource is busy"
#define ESMF_ERR_LIB            "Error in library called by ESMF"

#if 0
The following error is intended for passing an error from a subroutine
back up to the caller when deep inside the framework and there is no
particular context information to add.  The error handling subroutines
will add the calling method so a calling tree can be defined.
#endif

#define ESMF_ERR_PASSTHRU   "Internal subroutine call returned Error" 


#if 0
i do not believe we can return these without installing a specific
signal handler; by default these errors cause the program to dump core.
(and we do not want to handle signals ourselves - too complex.)
 ESMF_ERR_SIG : Signal received
 ESMF_ERR_FP : Floating point exception
#endif



#if 0
final endif below matches the initial ifdef ESMF_ERRMSGS_INC
#endif

#endif 


