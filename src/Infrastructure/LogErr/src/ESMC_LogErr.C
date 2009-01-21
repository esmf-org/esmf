// $Id: ESMC_LogErr.C,v 1.78.2.2 2009/01/21 21:25:22 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
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
// insert any higher level, 3rd party or system includes here

#include <stdio.h>        
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>
#include <sys/time.h>

// associated class definition file
#include "ESMC_Start.h"
#include "ESMC_Base.h"
#include "ESMC_LogErr.h"
#include "ESMF_ErrReturnCodes.inc"

// include array of error messages
#include "ESMC_ErrMsgs.C"

#define ESMC_SUCCESSDEFAULT_ON

//Global Variables
ESMC_Log ESMC_LogDefault;
FILE* logErrCFilePtr[10];
int numCFiles=0;
int logErrFortFile[10];
int numFortFiles=0;
char listOfCFileNames[20][32];
char listOfFortFileNames[20][32];

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_LogErr.C,v 1.78.2.2 2009/01/21 21:25:22 cdeluca Exp $";
//----------------------------------------------------------------------------
//
// This section includes all the Log routines
//
//----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_LogAllocErr - LogAllocErr
//
// !INTERFACE:

bool ESMC_Log::ESMC_LogAllocError(

// !RETURN VALUE:
//  none
//
// !ARGUMENTS:
    int *rcToReturn
      
    )
// !DESCRIPTION:
// Allocation error with no message and no cpp macros
//EOP
{
    bool result=false;
    if (rcToReturn != ESMC_NULL_POINTER) *rcToReturn=ESMC_RC_MEM;
    ESMC_LogWrite(ESMC_LogGetErrMsg(ESMC_RC_MEM),ESMC_LOG_ERROR);
    result=true;
    return result;
}

//----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_LogAllocErr - LogAllocErr
//
// !INTERFACE:


bool ESMC_Log::ESMC_LogAllocError(

// !RETURN VALUE:
//  none
//
// !ARGUMENTS:
    int LINE,
    char FILE[],
    char method[],
    int *rcToReturn      
    )
// !DESCRIPTION:
// Allocation error with no message but with cpp macros
//EOP
{
	bool result=false;
	if (rcToReturn != ESMC_NULL_POINTER) *rcToReturn=ESMC_RC_MEM;
	ESMC_LogWrite(ESMC_LogGetErrMsg(ESMC_RC_MEM),ESMC_LOG_ERROR,LINE,FILE,
	method);
	result=true;
	return result;
}

//----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_LogMsgAllocErr - LogAllocErr
//
// !INTERFACE:

bool ESMC_Log::ESMC_LogMsgAllocError(

// !RETURN VALUE:
//  none
//
// !ARGUMENTS:
    char msg[],
    int *rcToReturn
      
    )
// !DESCRIPTION:
// Allocation error with message and no cpp macros
//EOP
{
    bool result=false;
    char logMsg[ESMF_MAXSTR];
    if (rcToReturn != ESMC_NULL_POINTER) *rcToReturn=ESMC_RC_MEM;
    strcpy(logMsg, ESMC_LogGetErrMsg(ESMC_RC_MEM));
    ESMC_LogWrite(strcat(logMsg,msg),ESMC_LOG_ERROR);
    result=true;
    return result;
}

//----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_LogMsgAllocErr - LogAllocErr
//
// !INTERFACE:


bool ESMC_Log::ESMC_LogMsgAllocError(

// !RETURN VALUE:
//  none
//
// !ARGUMENTS:
    char msg[],
    int LINE,
    char FILE[],
    char method[],
    int *rcToReturn      
    )
// !DESCRIPTION:
// Allocation error with message and cpp macros
//EOP
{
    bool result=false;
    char logMsg[ESMF_MAXSTR];
    if (rcToReturn != ESMC_NULL_POINTER) *rcToReturn=ESMC_RC_MEM;
    strcpy(logMsg, ESMC_LogGetErrMsg(ESMC_RC_MEM));
    ESMC_LogWrite(strcat(logMsg,msg),ESMC_LOG_ERROR,LINE,FILE,method);
    result=true;
    return result;
}
//----------------------------------------------------------------------------
//
//
//BOP
// !IROUTINE:  ESMC_LogOpen -  opens a Log object
//
// !INTERFACE:

void ESMC_Log::ESMC_LogOpen(
//
// !RETURN VALUE:
//   none
//
// !ARGUMENTS:

     char filename[]     //string to form name of log file (input)

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

     char filename[]     //string to form name of log file (input)

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

//----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_LogClose - closes log file. 
//
// !INTERFACE:

void ESMC_Log::ESMC_LogClose(
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
// !IROUTINE: ESMC_LogWrite - write to log file
//
// !INTERFACE:

bool ESMC_Log::ESMC_LogWrite(

// !RETURN VALUE:
//  bool
//
// !ARGUMENTS:
	char msg[],   // Log Entry
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

    if (ESMC_LogDefault.logtype == ESMC_LOG_NONE) return true;
    FTN(f_esmf_logwrite0)(msg, &msgtype, &rc, strlen(msg));

    return (rc == ESMF_SUCCESS) ? true : false;
}

//----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_LogWrite - write to log file
//
// !INTERFACE:

bool ESMC_Log::ESMC_LogWrite(

// !RETURN VALUE:
//  bool
//
// !ARGUMENTS:
    char msg[],	// Log Entry
    int msgtype,// Msg Type   
    int LINE,
    char FILE[],
    char method[]
    )
// !DESCRIPTION:
// Prints log message and returns true if successful.  It takes two arguments -
// msg which is a user message and log type.  This method uses cpp macros
//EOP
{
    int rc;
    
    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    if (ESMC_LogDefault.logtype == ESMC_LOG_NONE) return true;
    FTN(f_esmf_logwrite1)(msg, &msgtype, &LINE, FILE, method, &rc,
                          strlen(msg), strlen(FILE), strlen(method));

    return (rc == ESMF_SUCCESS) ? true : false;
}

//----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_LogFoundError - LogFoundError
//
// !INTERFACE:

bool ESMC_Log::ESMC_LogFoundError(

// !RETURN VALUE:
//  bool
//
// !ARGUMENTS:
    int rcToCheck,
    int *rcToReturn
    )
// !DESCRIPTION:
// Returns true if rcToCheck does not equal ESMF\_SUCCESS and writes the error
// to the log.  This method does not use cpp macros.
//EOP
{
    bool result=false;
#ifdef ESMC_SUCCESSDEFAULT_ON
    if (rcToReturn != ESMC_NULL_POINTER) *rcToReturn=ESMF_SUCCESS;
#endif
    if (rcToCheck!=ESMF_SUCCESS){
      int i;
      for (i=0; i<errorMaskCount; i++)
        if (errorMask[i] == rcToCheck) break;
      if (i==errorMaskCount){
        // this means that rcToCheck was _not_ in the errorMask -> flag error
        result=true;
        if (rcToReturn != ESMC_NULL_POINTER) *rcToReturn=rcToCheck;
        ESMC_LogWrite(ESMC_LogGetErrMsg(rcToCheck),ESMC_LOG_ERROR);
      }
    }
    return result;
}

//----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_LogFoundError - LogFoundError
//
// !INTERFACE:


bool ESMC_Log::ESMC_LogFoundError(

// !RETURN VALUE:
//  bool
//
// !ARGUMENTS:
    int rcToCheck,
    int LINE,
    char FILE[],
    char method[],
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
      int i;
      for (i=0; i<errorMaskCount; i++)
        if (errorMask[i] == rcToCheck) break;
      if (i==errorMaskCount){
        // this means that rcToCheck was _not_ in the errorMask -> flag error
        result=true;
        if (rcToReturn != ESMC_NULL_POINTER) *rcToReturn=rcToCheck;
        ESMC_LogWrite(ESMC_LogGetErrMsg(rcToCheck),ESMC_LOG_ERROR,LINE,FILE,method);
      }
    }
    return result;
}

//----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_LogMsgFoundError - LogMsgFoundError
//
// !INTERFACE:

bool ESMC_Log::ESMC_LogMsgFoundError(

// !RETURN VALUE:
//  bool
//
// !ARGUMENTS:
    int rcToCheck,
    char msg[],
    int *rcToReturn
    )
// !DESCRIPTION:
// Returns true if rcToCheck does not equal ESMF\_SUCCESS and writes the error
// to the log with a user supplied mesage.  This method does not use cpp 
// macros.
//EOP
{
    bool result=false;
#ifdef ESMC_SUCCESSDEFAULT_ON
    if (rcToReturn != ESMC_NULL_POINTER) *rcToReturn=ESMF_SUCCESS;
#endif
    if (rcToCheck!=ESMF_SUCCESS){
      int i;
      for (i=0; i<errorMaskCount; i++)
        if (errorMask[i] == rcToCheck) break;
      if (i==errorMaskCount){
        // this means that rcToCheck was _not_ in the errorMask -> flag error
        result=true;
        if (rcToReturn != ESMC_NULL_POINTER) *rcToReturn=rcToCheck;
        char logMsg[ESMF_MAXSTR];
        strcpy(logMsg, ESMC_LogGetErrMsg(rcToCheck));
        ESMC_LogWrite(strcat(logMsg,msg),ESMC_LOG_ERROR);
      }
    }
    return result;
}

//----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_LogMsgFoundError - LogMsgFoundError
//
// !INTERFACE:

bool ESMC_Log::ESMC_LogMsgFoundError(

// !RETURN VALUE:
//  bool
//
// !ARGUMENTS:
    int rcToCheck,
    char msg[],
    int LINE,
    char FILE[],
    char method[],
    int *rcToReturn
    )
// !DESCRIPTION:
// Returns true if rcToCheck does not equal ESMF\_SUCCESS and writes the error
// to the log with a user supplied mesage.  This method uses cpp macros.
//EOP
{
    bool result=false;
#ifdef ESMC_SUCCESSDEFAULT_ON
    if (rcToReturn != ESMC_NULL_POINTER) *rcToReturn=ESMF_SUCCESS;
#endif
    if (rcToCheck!=ESMF_SUCCESS){
      int i;
      for (i=0; i<errorMaskCount; i++)
        if (errorMask[i] == rcToCheck) break;
      if (i==errorMaskCount){
        // this means that rcToCheck was _not_ in the errorMask -> flag error
        result=true;   // TODO: if this line moved to after ESMC_LogWrite()
                       // below, will crash ESMF_TimeIntervalUTest.F90 on 
                       // Linux longs 2.4.20-31.9, Lahey lf95 6.0 optimized
        if (rcToReturn != ESMC_NULL_POINTER) *rcToReturn=rcToCheck;
        char logMsg[ESMF_MAXSTR];
        strcpy(logMsg, ESMC_LogGetErrMsg(rcToCheck));
        ESMC_LogWrite(strcat(logMsg,msg),ESMC_LOG_ERROR,LINE,FILE,method);
      }
    }
    return result;
}

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
    struct timeval tv;	
    gettimeofday(&tv,NULL);
    ti=*localtime((const time_t*)&tv.tv_sec);
    *y=ti.tm_year+1900;
    *mn=ti.tm_mon+1;
    *d=ti.tm_mday;
    *h=ti.tm_hour;
    *m=ti.tm_min;
    *s=ti.tm_sec;
    *ms=tv.tv_usec;
}

//----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_LogGetErrMsg - LogGetErrMsg
//
// !INTERFACE:

char *ESMC_LogGetErrMsg(

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
    return((char *)errMsg[rc-1]);
}

