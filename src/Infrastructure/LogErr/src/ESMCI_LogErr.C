// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC Log method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The LogErr class (defined in ESMC\_Log.C and declared in
// the companion file ESMC\_LogErr.h) provides the user a way to write 
// {\tt ESMC\_Log} data.
//

// associated class definition file
#include "ESMCI_LogErr.h"

// higher level, 3rd party or system headers
#include <stdio.h>        
#include <stdlib.h>
#include <stdarg.h>
#include <string>
#include <cstring>
#include <time.h>

#if !defined (ESMF_OS_MinGW)
#include <sys/time.h>
#endif

// other ESMF headers
#include "ESMCI_Macros.h"

// include array of error messages
#include "ESMCI_ErrMsgs.C"

using namespace std;

#define ESMC_SUCCESSDEFAULT_ON

//Global Variables
ESMCI::LogErr ESMC_LogDefault;
FILE* logErrCFilePtr[10];
int numCFiles=0;
int logErrFortFile[10];
int numFortFiles=0;
char listOfCFileNames[20][32];
char listOfFortFileNames[20][32];

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
 static const char *const version = "$Id$";
//----------------------------------------------------------------------------
//
// This section includes all the Log routines
//
//----------------------------------------------------------------------------
//BOP

extern "C" {
  void FTN_X(esmf_breakpoint)(void);
}

namespace ESMCI{

//----------------------------------------------------------------------------
//BOP
// !IROUTINE: AllocError
//
// !INTERFACE:


bool LogErr::AllocError(

// !RETURN VALUE:
//  none
//
// !ARGUMENTS:
    int LINE,
    const char FILE[],
    const char method[],
    int *rcToReturn      
    )
// !DESCRIPTION:
//    Allocation error without message.
//EOP
{
    FTN_X(esmf_breakpoint)();  // no-op to assist debugging
    bool result=false;
    if (rcToReturn != ESMC_NULL_POINTER) *rcToReturn=ESMC_RC_MEM_ALLOCATE;
    Write(ESMC_LogGetErrMsg(ESMC_RC_MEM_ALLOCATE),ESMC_LOGMSG_ERROR,LINE,FILE,
    method);
    result=true;
    return result;
}

//----------------------------------------------------------------------------
//BOP
// !IROUTINE: MsgAllocError
//
// !INTERFACE:


bool LogErr::MsgAllocError(

// !RETURN VALUE:
//  none
//
// !ARGUMENTS:
    const char msg[],
    int LINE,
    const char FILE[],
    const char method[],
    int *rcToReturn      
    )
// !DESCRIPTION:
//    Allocation error with message.
//EOP
{
    FTN_X(esmf_breakpoint)();  // no-op to assist debugging
    bool result=false;
    if (rcToReturn != ESMC_NULL_POINTER) *rcToReturn=ESMC_RC_MEM_ALLOCATE;

    string logMsg = string(ESMC_LogGetErrMsg(ESMC_RC_MEM_ALLOCATE)) + " - " + msg;
    Write(logMsg,ESMC_LOGMSG_ERROR,LINE,FILE,method);
    result=true;
    return result;
}

bool LogErr::MsgAllocError(

// !RETURN VALUE:
//  none
//
// !ARGUMENTS:
    const string& msg,
    int LINE,
    const char FILE[],
    const char method[],
    int *rcToReturn      
    )
// !DESCRIPTION:
//    Allocation error with message.
//EOP
{
    FTN_X(esmf_breakpoint)();  // no-op to assist debugging
    bool result=false;
    if (rcToReturn != ESMC_NULL_POINTER) *rcToReturn=ESMC_RC_MEM_ALLOCATE;

    string logMsg = string(ESMC_LogGetErrMsg(ESMC_RC_MEM_ALLOCATE)) + " - " + msg;
    Write(logMsg,ESMC_LOGMSG_ERROR,LINE,FILE,method);
    result=true;
    return result;
}


//----------------------------------------------------------------------------
//BOP
// !IROUTINE: DeallocError
//
// !INTERFACE:


bool LogErr::DeallocError(

// !RETURN VALUE:
//  none
//
// !ARGUMENTS:
    int LINE,
    const char FILE[],
    const char method[],
    int *rcToReturn      
    )
// !DESCRIPTION:
//    Deallocation error without message.
//EOP
{
    FTN_X(esmf_breakpoint)();  // no-op to assist debugging
    bool result=false;
    if (rcToReturn != ESMC_NULL_POINTER) *rcToReturn=ESMC_RC_MEM_DEALLOCATE;
    Write(ESMC_LogGetErrMsg(ESMC_RC_MEM_DEALLOCATE),ESMC_LOGMSG_ERROR,LINE,FILE,
    method);
    result=true;
    return result;
}

//----------------------------------------------------------------------------
//BOP
// !IROUTINE: MsgDeallocError
//
// !INTERFACE:


bool LogErr::MsgDeallocError(

// !RETURN VALUE:
//  none
//
// !ARGUMENTS:
    const char msg[],
    int LINE,
    const char FILE[],
    const char method[],
    int *rcToReturn      
    )
// !DESCRIPTION:
//    Deallocation error with message.
//EOP
{
    FTN_X(esmf_breakpoint)();  // no-op to assist debugging
    bool result=false;
    if (rcToReturn != ESMC_NULL_POINTER) *rcToReturn=ESMC_RC_MEM_DEALLOCATE;

    string logMsg = string(ESMC_LogGetErrMsg(ESMC_RC_MEM_DEALLOCATE)) + " - " + msg;
    Write(logMsg,ESMC_LOGMSG_ERROR,LINE,FILE,method);
    result=true;
    return result;
}

bool LogErr::MsgDeallocError(

// !RETURN VALUE:
//  none
//
// !ARGUMENTS:
    const string& msg,
    int LINE,
    const char FILE[],
    const char method[],
    int *rcToReturn      
    )
// !DESCRIPTION:
//    Deallocation error with message.
//EOP
{
    FTN_X(esmf_breakpoint)();  // no-op to assist debugging
    bool result=false;
    if (rcToReturn != ESMC_NULL_POINTER) *rcToReturn=ESMC_RC_MEM_DEALLOCATE;

    string logMsg = string(ESMC_LogGetErrMsg(ESMC_RC_MEM_DEALLOCATE)) + " - " + msg;
    Write(logMsg,ESMC_LOGMSG_ERROR,LINE,FILE,method);
    result=true;
    return result;
}


//----------------------------------------------------------------------------
//
//
//BOP
// !IROUTINE:  Open -  opens a Log object
//
// !INTERFACE:

void LogErr::Open(
//
// !RETURN VALUE:
//   none
//
// !ARGUMENTS:

     const char filename[]     //string to form name of log file (input)

   )
//
// !DESCRIPTION:
// {\tt ESMC\_LogOpen} opens a new log file and sets the deafult filename
//
//EOP
// 
{
    strcpy(nameLogErrFile,filename);
}   

} // namespace ESMCI

//----------------------------------------------------------------------------
//
//
//BOP
// !IROUTINE:  ESMC_LogSetFilename -  sets filename of a log that is open
//
// !INTERFACE:
int ESMC_LogSetFilename(
//
// !RETURN VALUE:
//  int
//
// !ARGUMENTS:

     const char filename[]     //string to form name of log file (input)

   )
//
// !DESCRIPTION:
// {\tt ESMC\_LogSetFilename} sets the filename to the opened log.
//
//EOP
// 
{
    int rc;

    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    strcpy(ESMC_LogDefault.nameLogErrFile,filename);
    rc = ESMF_SUCCESS;
    return rc;
}   

//----------------------------------------------------------------------------
//
//
//BOP
// !IROUTINE:  ESMC_LogFinalize -  Finalizes an open log
//
// !INTERFACE:
int ESMC_LogFinalize(
//
// !RETURN VALUE:
//   none
//
// !ARGUMENTS:

   )
//
// !DESCRIPTION:
// {\tt ESMC\_LogFinalize} finalizes an open log.
//EOP
// 
{
    return ESMF_SUCCESS;
} 

namespace ESMCI{

//----------------------------------------------------------------------------
//BOP
// !IROUTINE: Close - closes log file. 
//
// !INTERFACE:

void LogErr::Close(
//
// ! RETURN VALUE:
//    none
//
// !ARGUMENTS:
//   none

   )
//
// ! DESCRIPTION:
// This routine simply closes the log file(s).  
//
//EOP

{
  
}

//----------------------------------------------------------------------------
//BOP
// !IROUTINE: Write - write to log file
//
// !INTERFACE:

int LogErr::Write(

// !RETURN VALUE:
//  bool
//
// !ARGUMENTS:
	const char msg[],   // Log Entry
    	int msgtype   // Msg Type   
      )
// !DESCRIPTION:
// Prints log message and returns true if successful.  It takes two arguments -
// msg which is a user message and log type.  This method does not use cpp
// macros
//EOP
{
    int rc;

    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    if (ESMC_LogDefault.logtype == ESMC_LOGKIND_NONE) return true;
    FTN_X(f_esmf_logwrite0)(msg, &msgtype, &rc, strlen(msg));

    return rc;
}

int LogErr::Write(

// !RETURN VALUE:
//  bool
//
// !ARGUMENTS:
	const string& msg,   // Log Entry
    	int msgtype   // Msg Type   
      )
// !DESCRIPTION:
// Prints log message and returns true if successful.  It takes two arguments -
// msg which is a user message and log type.  This method does not use cpp
// macros
//EOP
{
    int rc;

    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    if (ESMC_LogDefault.logtype == ESMC_LOGKIND_NONE) return true;
    FTN_X(f_esmf_logwrite0)(msg.c_str(), &msgtype, &rc, msg.size());

    return rc;
}

//----------------------------------------------------------------------------
//BOP
// !IROUTINE: Write - write to log file
//
// !INTERFACE:

int LogErr::Write(

// !RETURN VALUE:
//  bool
//
// !ARGUMENTS:
    const char msg[],	// Log Entry
    int msgtype,        // Msg Type   
    int LINE,
    const char FILE[],
    const char method[]
    )
// !DESCRIPTION:
// Prints log message and returns true if successful.  It takes two arguments -
// msg which is a user message and log type.  This method uses cpp macros
//EOP
{
    int rc;
    
    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    if (ESMC_LogDefault.logtype == ESMC_LOGKIND_NONE) return true;
    FTN_X(f_esmf_logwrite1)(msg, &msgtype, &LINE, FILE, method, &rc,
                          strlen(msg), strlen(FILE), strlen(method));

    return rc;
}

int LogErr::Write(

// !RETURN VALUE:
//  bool
//
// !ARGUMENTS:
    const string& msg,	// Log Entry
    int msgtype,        // Msg Type   
    int LINE,
    const char FILE[],
    const char method[]
    )
// !DESCRIPTION:
// Prints log message and returns true if successful.  It takes two arguments -
// msg which is a user message and log type.  This method uses cpp macros
//EOP
{
    int rc;
    
    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    if (ESMC_LogDefault.logtype == ESMC_LOGKIND_NONE) return true;
    FTN_X(f_esmf_logwrite1)(msg.c_str(), &msgtype, &LINE, FILE, method, &rc,
                          msg.size(), strlen(FILE), strlen(method));

    return rc;
}

//----------------------------------------------------------------------------
//BOP
// !IROUTINE: Set - set Log parameters
//
// !INTERFACE:

int LogErr::Set(

// !RETURN VALUE:
//  integer return code
//
// !ARGUMENTS:
    int flush
    )
// !DESCRIPTION:
// Sets log parameters
//EOP
{
    int rc;
    
    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;
    
    // cast flush to bool
    bool lflush = false;
    
    lflush = static_cast<bool> (flush);

    if (ESMC_LogDefault.logtype == ESMC_LOGKIND_NONE) return ESMF_SUCCESS;
    FTN_X(f_esmf_logset)(&lflush, &rc);

    return rc;
}

//----------------------------------------------------------------------------
//BOP
// !IROUTINE: FoundError - LogFoundError
//
// !INTERFACE:


bool LogErr::FoundError(

// !RETURN VALUE:
//  bool
//
// !ARGUMENTS:
    int rcToCheck,
    int LINE,
    const char FILE[],
    const char method[],
    int *rcToReturn
    )
// !DESCRIPTION:
// Returns true if rcToCheck does not equal ESMF\_SUCCESS and writes the error
// to the log.  This method uses cpp macros.
//EOP
{
    bool result=false;
#ifdef ESMC_SUCCESSDEFAULT_ON
    if (rcToReturn != ESMC_NULL_POINTER) *rcToReturn=ESMF_SUCCESS;
#endif
    if (rcToCheck!=ESMF_SUCCESS){
      FTN_X(esmf_breakpoint)();  // no-op to assist debugging
      int i;
      for (i=0; i<errorMaskCount; i++)
        if (errorMask[i] == rcToCheck) break;
      if (i==errorMaskCount){
        // this means that rcToCheck was _not_ in the errorMask -> flag error
        result=true;
        if (rcToReturn != ESMC_NULL_POINTER) *rcToReturn=rcToCheck;
        Write(ESMC_LogGetErrMsg(rcToCheck),ESMC_LOGMSG_ERROR,LINE,FILE,method);
      }
    }
    return result;
}

//----------------------------------------------------------------------------
//BOP
// !IROUTINE: MsgFoundError - LogMsgFoundError
//
// !INTERFACE:

bool LogErr::MsgFoundError(

// !RETURN VALUE:
//  bool
//
// !ARGUMENTS:
    int rcToCheck,
    const char msg[],
    int LINE,
    const char FILE[],
    const char method[],
    int *rcToReturn
    )
// !DESCRIPTION:
// Returns true if rcToCheck does not equal ESMF\_SUCCESS and writes the error
// to the log with a user supplied message.  This method uses cpp macros.
//EOP
{
    bool result=false;
#ifdef ESMC_SUCCESSDEFAULT_ON
    if (rcToReturn != ESMC_NULL_POINTER) *rcToReturn=ESMF_SUCCESS;
#endif
    if (rcToCheck!=ESMF_SUCCESS){
      FTN_X(esmf_breakpoint)();  // no-op to assist debugging
      int i;
      for (i=0; i<errorMaskCount; i++)
        if (errorMask[i] == rcToCheck) break;
      if (i==errorMaskCount){
        // this means that rcToCheck was _not_ in the errorMask -> flag error
        result=true;   // TODO: if this line moved to after Write()
                       // below, will crash ESMF_TimeIntervalUTest.F90 on 
                       // Linux longs 2.4.20-31.9, Lahey lf95 6.0 optimized
        if (rcToReturn != ESMC_NULL_POINTER) *rcToReturn=rcToCheck;

        string logMsg = string(ESMC_LogGetErrMsg(rcToCheck)) + " - " + msg;
        Write(logMsg,ESMC_LOGMSG_ERROR,LINE,FILE,method);
      }
    }
    return result;
}

bool LogErr::MsgFoundError(

// !RETURN VALUE:
//  bool
//
// !ARGUMENTS:
    int rcToCheck,
    const string &msg,
    int LINE,
    const char FILE[],
    const char method[],
    int *rcToReturn
    )
// !DESCRIPTION:
// Returns true if rcToCheck does not equal ESMF\_SUCCESS and writes the error
// to the log with a user supplied message.  This method uses cpp macros.
//EOP
{
    bool result=false;
#ifdef ESMC_SUCCESSDEFAULT_ON
    if (rcToReturn != ESMC_NULL_POINTER) *rcToReturn=ESMF_SUCCESS;
#endif
    if (rcToCheck!=ESMF_SUCCESS){
      FTN_X(esmf_breakpoint)();  // no-op to assist debugging
      int i;
      for (i=0; i<errorMaskCount; i++)
        if (errorMask[i] == rcToCheck) break;
      if (i==errorMaskCount){
        // this means that rcToCheck was _not_ in the errorMask -> flag error
        result=true;   // TODO: if this line moved to after Write()
                       // below, will crash ESMF_TimeIntervalUTest.F90 on 
                       // Linux longs 2.4.20-31.9, Lahey lf95 6.0 optimized
        if (rcToReturn != ESMC_NULL_POINTER) *rcToReturn=rcToCheck;

        string logMsg = string(ESMC_LogGetErrMsg(rcToCheck)) + " - " + msg;
        Write(logMsg,ESMC_LOGMSG_ERROR,LINE,FILE,method);
      }
    }
    return result;
}

} // namespace ESMCI

//----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_TimeStamp - ESMC_TimeStamp
//
// !INTERFACE:

void ESMC_TimeStamp(

// !RETURN VALUE:
//  none
//
// !ARGUMENTS:
    int *y,
    int *mn,
    int *d,
    int *h,
    int *m,
    int *s,
    int *ms
      
    )
// !DESCRIPTION:
// Returns time stamp values so that microsecond precision can be used.
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
    ti=*localtime(&tv);
#endif
    *y=ti.tm_year+1900;
    *mn=ti.tm_mon+1;
    *d=ti.tm_mday;
    *h=ti.tm_hour;
    *m=ti.tm_min;
    *s=ti.tm_sec;
#if !defined (ESMF_OS_MinGW)
    *ms=tv.tv_usec;
#else
    *ms=0;
#endif
}


//----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_LogGetErrMsg - LogGetErrMsg
//
// !INTERFACE:

const char *ESMC_LogGetErrMsg(

// !RETURN VALUE:
//  char
//
// !ARGUMENTS:
    int rc
    )
// !DESCRIPTION:
// Returns error message corresponding to rc
//EOP
{
    if (rc == ESMF_SUCCESS) return("Success ");
    if (rc == ESMF_FAILURE) return("Failure ");
    if (rc < 1 || rc > ESMC_MAX_ERRORS) return("Unknown error ");
    return(errMsg[rc-1]);
}

