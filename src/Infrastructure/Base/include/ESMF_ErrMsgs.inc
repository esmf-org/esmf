#if 0
$Id: ESMF_ErrMsgs.inc,v 1.1 2004/05/14 08:12:55 nscollins Exp $

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

#define ESMF_ERR_BAD_OBJ     "Invalid object"
#define ESMF_ERR_BAD_ARG     "Invalid argument"
#define ESMF_ERR_BAD_RANK    "Argument rank is not required size"
#define ESMF_ERR_BAD_MATCH   "Argument sizes do not match"
#define ESMF_ERR_BAD_VALUE   "Value unrecognized or out of range"
#define ESMF_ERR_BAD_OPT     "Unrecognized option string"
#define ESMF_ERR_NOT_IMPL    "Operation not yet supported"
#define ESMF_ERR_NOT_ALLOC   "Pointer must already be allocated"
#define ESMF_ERR_IS_ALLOC    "Pointer must not already be allocated"
#define ESMF_ERR_CANNOT_GET  "Cannot get value"
#define ESMF_ERR_CANNOT_SET  "Cannot set value"
#define ESMF_ERR_FILE_OPEN   "Cannot open file"
#define ESMF_ERR_FILE_CREATE "Cannot create file"
#define ESMF_ERR_DUP_NAME    "Name already exists"
#define ESMF_ERR_NOT_FOUND   "Not found"
#define ESMF_ERR_NOT_VALID   "Not valid" 
#define ESMF_ERR_LONG_NAME   "Name too long, must be less than ESMF_MAXSTR"
#define ESMF_ERR_LONG_STR    "String too long, must be less than ESMF_MAXSTR"
#define ESMF_ERR_COPY_FAIL   "Cannot copy non-existent object"
#define ESMF_ERR_LIST_FAIL   "Internal error: List overflow"
#define ESMF_ERR_NULL_PTR    "Value cannot be a NULL pointer"
#define ESMF_ERR_DIV_ZERO    "Cannot divide by zero"

#if 0
The following error is intended for passing an error from a subroutine
back up to the caller when deep inside the framework and there is no
particular context information to add.  The error handling subroutines
will add the calling method so a calling tree can be defined.
#endif

#define ESMF_ERR_PASS_THRU   "Internal subroutine call returned Error" 


#if 0
final endif below matches the initial ifdef ESMF_ERRMSGS_INC
#endif

#endif 


