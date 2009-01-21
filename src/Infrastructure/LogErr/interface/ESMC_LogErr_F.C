// $Id: ESMC_LogErr_F.C,v 1.24.2.2 2009/01/21 21:25:22 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC Base method interface (from F90 to C++) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the Fortran callable 
// interfaces to the C++ Base methods.
//
//-----------------------------------------------------------------------------
//
 // associated class definition file and others
#include <string.h>
#include <stdlib.h>
#include <sys/time.h>
#include "ESMC_LogErr.h"
#include "ESMC_Base.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_LogErr_F.C,v 1.24.2.2 2009/01/21 21:25:22 cdeluca Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes ESMC_Base routine interfaces
//
//

extern "C" {

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//  LogErr Methods
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_LogFinalize - Finalize global Error Log
//
// !INTERFACE:
      void FTN(c_esmc_logfinalize)(
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
//BOP
// !IROUTINE:  c_ESMC_LogGetErrMessage - initialize global Error Log
//
// !INTERFACE:
      void FTN(c_esmc_loggeterrormsg)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
                
      int *rc,        		// in - return code          
      char *msg,		// out - message associted with code
      int *msglen){ 		// out - strlen(msg)
// 
// !DESCRIPTION:
//     Initialize C++ version of LogErr.
//
//EOP
// !REQUIREMENTS: 

  // copy and convert F90 strings to null terminated ones
  strcpy(msg,ESMC_LogGetErrMsg(*rc));
  *msglen=strlen(msg);
  return;

}  // end c_ESMC_LogGetErrMessage

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_LogInitialize - initialize global Error Log
//
// !INTERFACE:
      void FTN(c_esmc_loginitialize)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      char *filename,           // in - F90 filename, non-null terminated string
      int *petnum,
      ESMC_LogType *logtype,
      int *rc,                  // out - return code
      int nlen){                // hidden/in - strlen count for filename
// 
// !DESCRIPTION:
//     Initialize C++ version of LogErr.
//
//EOP
// !REQUIREMENTS: 

  char *fname = NULL;

  *rc = ESMC_RC_NOT_IMPL;
  // copy and convert F90 strings to null terminated ones
  if (filename && (nlen > 0) && (filename[0] != '\0')) {
      fname = ESMC_F90toCstring(filename, nlen);
      if (!fname) {
          delete [] fname;
          *rc = ESMF_FAILURE;
          return;
      }
  } 
  strcpy(ESMC_LogDefault.nameLogErrFile,fname);
  ESMC_LogDefault.pet_num=petnum;
  ESMC_LogDefault.logtype=*logtype;
  if (fname)  delete [] fname;
  ESMC_LogDefault.errorMaskCount = 0;
  ESMC_LogDefault.errorMask = NULL;
  *rc = ESMF_SUCCESS;
  return;

}  // end c_ESMC_Loginitialize


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_LogSet - set values in global default Error Log
//
// !INTERFACE:
      void FTN(c_esmc_logset)(
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


//----------------------------------------------------------------------------- //BOP
// !IROUTINE:  c_ESMC_LogTimeStamp - Get Time Stamp
//
// !INTERFACE:
      void FTN(c_esmc_timestamp)(
//
// !RETURN VALUE:
//    none.  timestamp is passed thru the parameter list
// 
// !RETURN VALUE:
//  none
//
// !ARGUMENTS:
    int *y,    			// out - year
    int *mn,			// out - month
    int *d,			// out - day
    int *h,			// out - hour
    int *m,			// out - minute
    int *s,			// out - second
    int *ms			// out - microsecond
      
    )
// !DESCRIPTION:
// TimeStamp
//EOP
{
    time_t tm;
    struct tm ti;
    struct timeval tv;	
    gettimeofday(&tv,NULL);
    ti=*localtime((const time_t*)&tv.tv_sec);
    *y=ti.tm_year+1900;
    *mn=ti.tm_mon;
    *d=ti.tm_mday;
    *h=ti.tm_hour;
    *m=ti.tm_min;
    *s=ti.tm_sec;
    *ms=tv.tv_usec;
    return;
}  // end c_ESMC_Timestamp


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------


} // extern "C"
