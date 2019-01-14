// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the Fortran callable
// interfaces to the C++ methods.
//
//-----------------------------------------------------------------------------
//
 // associated class definition file and others
#include <cstring>
#include <ctime>
#include <string>

#if !defined (ESMF_OS_MinGW)
#include <sys/time.h>
#endif

#include "ESMC_Util.h"
#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
//
//

extern "C" {

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//  LogErr Methods
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#define ESMC_METHOD "c_esmc_logfinalize"
//BOP
// !IROUTINE:  c_ESMC_LogFinalize - Finalize global Error Log
//
// !INTERFACE:
      void FTN_X(c_esmc_logfinalize)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
//
// !ARGUMENTS:
      int *rc){                 // out - return code
//
// !DESCRIPTION:
//     Finalize C++ version of LogErr.
//
//EOP
// !REQUIREMENTS:

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (ESMC_LogDefault.errorMaskCount > 0)
    delete [] ESMC_LogDefault.errorMask;
  if (rc) *rc = ESMF_SUCCESS;
  return;

}  // end c_ESMC_Logfinalize

//-----------------------------------------------------------------------------
#undef ESMC_METHOD
#define ESMC_METHOD "c_ESMC_LogGetErrMessage"
//BOP
// !IROUTINE:  c_ESMC_LogGetErrMessage - convert rc to message string
//
// !INTERFACE:
      void FTN_X(c_esmc_loggeterrormsg)(
//
// !RETURN VALUE:
//    none.
//
// !ARGUMENTS:

      int *rc,                  // in - return code
      char *msg,                // out - message associated with code
      int *msglen,              // out - length of msg excluding trailing blanks
      ESMCI_FortranStrLenArg msg_l) { // hidden/in - msg length
//
// !DESCRIPTION:
//     Given an ESMF rc, return a Fortran string containing the error message.
//
//EOP
// !REQUIREMENTS:

  const char *msg_local = ESMC_LogGetErrMsg(*rc);

  // copy and convert C string to Fortran with trailing blanks
  int msg_local_len = strlen (msg_local);
  strncpy(msg, msg_local, msg_l);
  if (msg_l > msg_local_len) {
    memset (msg+msg_local_len, ' ', msg_l-msg_local_len);
    *msglen = msg_local_len;
  } else
    *msglen = msg_l;

  return;

}  // end c_ESMC_LogGetErrMessage

//-----------------------------------------------------------------------------
#undef ESMC_METHOD
#define ESMC_METHOD "c_ESMC_LogInitialize"
//BOP
// !IROUTINE:  c_ESMC_LogInitialize - initialize global Error Log
//
// !INTERFACE:
      void FTN_X(c_esmc_loginitialize)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
//
// !ARGUMENTS:
      const char *filename,           // in - F90 filename, non-null terminated string
      int *petnum,
      ESMC_LogKind_Flag *logtype,
      int *rc,                        // out - return code
      ESMCI_FortranStrLenArg nlen){   // hidden/in - strlen count for filename
//
// !DESCRIPTION:
//     Initialize C++ version of LogErr.
//
//EOP
// !REQUIREMENTS:

  *rc = ESMC_RC_NOT_IMPL;

  ESMC_LogDefault.nameLogErrFile = std::string (filename, ESMC_F90lentrim (filename, nlen));
  ESMC_LogDefault.SetTrace(false);
  ESMC_LogDefault.pet_num=petnum;
  ESMC_LogDefault.logtype=*logtype;
  ESMC_LogDefault.errorMaskCount = 0;
  ESMC_LogDefault.errorMask = NULL;
  *rc = ESMF_SUCCESS;
  return;

}  // end c_ESMC_Loginitialize


//-----------------------------------------------------------------------------
#undef ESMC_METHOD
#define ESMC_METHOD "c_ESMC_LogSet"
//BOP
// !IROUTINE:  c_ESMC_LogSet - set values in global default Error Log
//
// !INTERFACE:
      void FTN_X(c_esmc_logset)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
//
// !ARGUMENTS:
      int *errorMask,
      int *errorMaskCount,
      int *rc){                 // out - return code
//
// !DESCRIPTION:
//     Set values in C++ version of global default LogErr.
//
//EOP
// !REQUIREMENTS:

  *rc = ESMC_RC_NOT_IMPL;
  if (ESMC_LogDefault.errorMaskCount > 0)
    delete [] ESMC_LogDefault.errorMask;
  ESMC_LogDefault.errorMaskCount = *errorMaskCount;
  ESMC_LogDefault.errorMask = new int[*errorMaskCount];
  for (int i=0; i<*errorMaskCount; i++)
    ESMC_LogDefault.errorMask[i] = errorMask[i];
  *rc = ESMF_SUCCESS;
  return;

}  // end c_ESMC_LogSet

//-----------------------------------------------------------------------------
#undef ESMC_METHOD
#define ESMC_METHOD "c_ESMC_LogSetTrace"
//BOP
// !IROUTINE:  c_ESMC_LogSetTrace - set trace flag in global default Error Log
//
// !INTERFACE:
      void FTN_X(c_esmc_logsettrace)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
//
// !ARGUMENTS:
      ESMC_Logical *traceflag,
      int *rc){                 // out - return code
//
// !DESCRIPTION:
//     Set values in C++ version of global default LogErr.
//
//EOP
// !REQUIREMENTS:

  *rc = ESMC_RC_NOT_IMPL;
  bool trace = *traceflag == ESMF_TRUE;
  *rc = ESMC_LogDefault.SetTrace (trace);
  return;

}  // end c_ESMC_LogSetTrace


//-----------------------------------------------------------------------------
#undef ESMC_METHOD
#define ESMC_METHOD "c_ESMC_LogTimeStamp"
//BOP
// !IROUTINE:  c_ESMC_LogTimeStamp - Get Time Stamp
//
// !INTERFACE:
      void FTN_X(c_esmc_timestamp)(
//
// !RETURN VALUE:
//    none.  timestamp is passed thru the parameter list
//
// !RETURN VALUE:
//  none
//
// !ARGUMENTS:
    int *y,                             // out - year
    int *mn,                    // out - month
    int *d,                     // out - day
    int *h,                     // out - hour
    int *m,                     // out - minute
    int *s,                     // out - second
    int *ms                     // out - microsecond

    )
// !DESCRIPTION:
// TimeStamp
//EOP
{
    time_t tm;
    struct tm ti;
#if !defined (ESMF_OS_MinGW)
    struct timeval tv;  
    gettimeofday(&tv,NULL);
    ti=*localtime((const time_t*)&tv.tv_sec);
#else
    time_t tv;
    time (&tv);
    ti = *localtime (&tv);
#endif
    *y=ti.tm_year+1900;
    *mn=ti.tm_mon;
    *d=ti.tm_mday;
    *h=ti.tm_hour;
    *m=ti.tm_min;
    *s=ti.tm_sec;
#if !defined (ESMF_OS_MinGW)
    *ms=tv.tv_usec;
#else
    *ms=0;
#endif
}  // end c_ESMC_Timestamp


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------


} // extern "C"
