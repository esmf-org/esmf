// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_Util_F.C"
//==============================================================================

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the Fortran callable
// interfaces to the C++ Util methods.
//
//-----------------------------------------------------------------------------
//

//insert any higher level, 3rd party or system includes here
#if defined (MAPDEBUG)
#include <iostream>
#endif
#include <map>
#include <string>
#include <cstdlib>
#include <cstring>
#include <cstdio>
using namespace std;

#if !defined (ESMF_OS_MinGW)
#include <errno.h>
#include <sys/stat.h>
#include <unistd.h>
#else
#include <Windows.h>
#endif

// associated class definition file and others
#include "ESMCI_Macros.h"
#include "ESMCI_Util.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_LogErr.h"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


extern "C" {

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// String routines
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_StringSerialize - Serialize String object
//
// !INTERFACE:
      void FTN_X(c_esmc_stringserialize)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
//
// !ARGUMENTS:
      char *string,             // in/out - string object
      char *buf,                // in/out - really a byte stream
      int *length,              // in/out - number of allocated bytes
      int *offset,              // in/out - current offset in the stream
      ESMC_InquireFlag *inquireflag, // in - inquire flag
      int *rc,                  // out - return code
      ESMCI_FortranStrLenArg clen) { // in, hidden - string length
//
// !DESCRIPTION:
//     Serialize the contents of a string object.
//
//EOPI
#undef  ESMC_METHOD
#define ESMC_METHOD "c_ESMC_StringSerialize"

  char *cp;

  if (!string) {
    //printf("uninitialized String object\n");
    ESMC_LogDefault.Write("String object uninitialized", ESMC_LOGMSG_INFO,
      ESMC_CONTEXT);
    if (rc) *rc = ESMF_SUCCESS;
    return;
  }

  int fixedpart = clen + 1;
  if (*inquireflag != ESMF_INQUIREONLY) {
    if ((*length - *offset) < fixedpart) {

       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
         "Buffer too short to add a String object", ESMC_CONTEXT, rc);
       return;

      //buffer = (char *)realloc((void *)buffer,
      //                         *length + 2*fixedpart + byte_count);
      //*length += 2 * fixedpart;
    }
  }

  cp = buf + *offset;
  if (*inquireflag != ESMF_INQUIREONLY)
    memcpy(cp, string, clen);
  cp += clen;

  *offset = cp - buf;

  if (rc) *rc = ESMF_SUCCESS;

  return;

}  // end c_ESMC_StringSerialize


//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_StringDeserialize - Deserialize String object
//
// !INTERFACE:
      void FTN_X(c_esmc_stringdeserialize)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
//
// !ARGUMENTS:
      char *string,             // in/out - string object
      char *buf,                // in/out - really a byte stream
      int *offset,              // in/out - current offset in the stream
      int *rc,                  // out - return code
      ESMCI_FortranStrLenArg clen) { // in, hidden - string length
//
// !DESCRIPTION:
//     Deserialize the contents of a base object.
//
//EOPI
#undef  ESMC_METHOD
#define ESMC_METHOD "c_ESMC_StringDeserialize"

  char *cp;

  cp = buf + *offset;
  memcpy(string, cp, clen);
  cp += clen;

  *offset = cp - buf;

  if (rc) *rc = ESMF_SUCCESS;

  return;

}  // end c_ESMC_StringDeserialize



//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// System Call routines
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_MakeDirectory - Make a directory in the file system
//
// !INTERFACE:
      void FTN_X(c_esmc_makedirectory)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
//
// !ARGUMENTS:
      const char *pathname,     // in - path name
      int *mode,                // in - protection mode
      ESMC_Logical *relaxedFlag,// in - relaxed mode
      int *rc,                  // out - return code
      ESMCI_FortranStrLenArg pathname_l) { // in, hidden - pathname length
//
// !DESCRIPTION:
//     Creates a new directory in the file system.  If the directory already
//     exists, and the relaxedFlag argument is set to {tt ESMF\_TRUE},
//     return an rc of ESMF\_SUCCESS.
//
//     On native Windows, the protection mode argument is ignored.  Default
//     security attributes are used.
//
//EOPI
#undef  ESMC_METHOD
#define ESMC_METHOD "c_ESMC_MakeDirectory"

  string path = string (pathname, ESMC_F90lentrim (pathname, pathname_l));
  bool relaxedflag = *relaxedFlag == ESMF_TRUE;

#if !defined (ESMF_OS_MinGW)
  if (mkdir (path.c_str(), *mode) == 0)
    *rc = ESMF_SUCCESS;
  else
    switch (errno) {
      case EEXIST:
        if (relaxedflag)
          *rc = ESMF_SUCCESS;
        else
          *rc = ESMF_FAILURE;
        break;
      default:
        *rc = ESMF_FAILURE;
    }
#else
  if (CreateDirectory (path.c_str(), NULL))
    *rc = ESMF_SUCCESS;
  else
    switch (GetLastError()) {
      case ERROR_ALREADY_EXISTS:
        if (relaxedflag)
          *rc = ESMF_SUCCESS;
        else
          *rc = ESMF_FAILURE;
        break;
      default:
        *rc = ESMF_FAILURE;
    }
#endif

}

//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_RemoveDirectory - Remove a directory from the file system
//
// !INTERFACE:
      void FTN_X(c_esmc_removedirectory)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
//
// !ARGUMENTS:
      const char *pathname,     // in - path name
      ESMC_Logical *relaxedFlag,// in - relaxed mode
      int *rc,                  // out - return code
      ESMCI_FortranStrLenArg pathname_l) { // in, hidden - pathname length
//
// !DESCRIPTION:
//     Removes an existing directory in the file system.
//
//EOPI
#undef  ESMC_METHOD
#define ESMC_METHOD "c_ESMC_UtilRemoveDirectory"

  string path = string (pathname, ESMC_F90lentrim (pathname, pathname_l));
  bool relaxedflag = *relaxedFlag == ESMF_TRUE;

#if !defined (ESMF_OS_MinGW)
  if (rmdir (path.c_str()) == 0) {
    *rc = ESMF_SUCCESS;
  } else
    switch (errno) {
      case ENOENT:
        if (relaxedflag)
          *rc = ESMF_SUCCESS;
        else
          *rc = ESMF_RC_NOT_FOUND;
  break;
      default:
  *rc = ESMF_FAILURE;
    }
#else
  if (RemoveDirectory (path.c_str())) {
    *rc = ESMF_SUCCESS;
  } else {
    switch (GetLastError ()) {
      case ERROR_FILE_NOT_FOUND:
        if (relaxedflag)
          *rc = ESMF_SUCCESS;
        else
          *rc = ESMF_RC_NOT_FOUND;
        break;
      default:
        *rc = ESMF_FAILURE;
    }
  }
#endif

}

//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_UtilSystem - Execute a command line
//
// !INTERFACE:
      void FTN_X(c_esmc_utilsystem)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
//
// !ARGUMENTS:
      char *command,            // out - command line
      int *rc,                  // out - return code
      ESMCI_FortranStrLenArg command_l) { // in, hidden - command length
//
// !DESCRIPTION:
//     Execute a command line.  Return when complete.
//
//EOPI
#undef  ESMC_METHOD
#define ESMC_METHOD "c_ESMC_UtilSystem"

  string comnd = string (command, ESMC_F90lentrim (command, command_l));
  int err = system (comnd.c_str());
  if (rc)
    *rc = (err == 0) ? ESMF_SUCCESS : ESMF_FAILURE;
}

//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_GetCWD - Get the current directory from the file system
//
// !INTERFACE:
      void FTN_X(c_esmc_getcwd)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
//
// !ARGUMENTS:
      char *pathname,           // out - path name
      int *rc,                  // out - return code
      ESMCI_FortranStrLenArg pathname_l) { // in, hidden - pathname length
//
// !DESCRIPTION:
//     Gets the current directory in the file system.
//
//EOPI
#undef  ESMC_METHOD
#define ESMC_METHOD "c_ESMC_UtilGetCWD"

#if !defined (ESMF_OS_MinGW)
  if (getcwd (pathname, pathname_l) != NULL)
    *rc = ESMF_SUCCESS;
  else {
    switch (errno) {
      case ERANGE:
        *rc = ESMF_RC_BUFFER_SHORT;
        break;
      default:
        *rc = ESMF_FAILURE;
    }
    return;
  }
#else
  int winrt = GetCurrentDirectory (pathname_l, pathname);
  if (winrt == 0 || winrt > pathname_l) {
    *rc = ESMF_FAILURE;
    return;
  } else
    *rc = ESMF_SUCCESS;
#endif
  int len = strlen (pathname);
  if (len < pathname_l)
    memset (pathname+len, ' ', pathname_l-len);
}

//-----------------------------------------------------------------------------

void FTN_X(c_pointerprint)(void **ptr){
  printf("ESMF_PointerPrint: %p\n", *ptr);
}

} // extern "C"
