// $Id: ESMC_LogErr.C,v 1.68 2005/01/13 04:57:29 cpboulder Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

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
 static const char *const version = "$Id: ESMC_LogErr.C,v 1.68 2005/01/13 04:57:29 cpboulder Exp $";
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
    ESMC_LogOpen(filename,ESMC_LOG_SINGLE);
}   

//----------------------------------------------------------------------------
//
//
//BOP
// !IROUTINE:  ESMC_LogOpen -  opens a Log object specifying single or multi log
//
// !INTERFACE:

void ESMC_Log::ESMC_LogOpen(
//
// !RETURN VALUE:
//   none
//
// !ARGUMENTS:

     char filename[],     //string to form name of log file (input)
     int logtype           //Single or Multi file
   )
//
// !DESCRIPTION:
// {\tt ESMC\_LogOpen} opens a new log file and sets the deafult filename
//
//EOP
// 
{
    if (logtype==ESMC_LOG_SINGLE)
    {
        strcpy(nameLogErrFile,filename);
    }
    else
    {
        //sprintf(nameLogErrFile,"PET%d.%s",*pet_number,filename);
		strcpy(nameLogErrFile,filename);
    }
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
    strcpy(ESMC_LogDefault.nameLogErrFile,filename);
    return ESMF_SUCCESS;
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
// !IROUTINE: ESMC_logWrite - write to log file
//
// !INTERFACE:

bool ESMC_Log::ESMC_LogWrite(

// !RETURN VALUE:
//  bool
//
// !ARGUMENTS:
	    char msg[],	  // Log Entry
    	int msgtype   // Msg Type   
      )
// !DESCRIPTION:
// Prints log messsge and returns true if successful.  It takes two arguments -
// msg which is a user message and log type.  This method does not use cpp
// macros
//EOP
{
    int y,mn,d,h,m,s,ms;
    int ok=0;
    int i=0;
	char petnum[8]="PET0";
    ESMC_TimeStamp(&y,&mn,&d,&h,&m,&s,&ms);
	  do
	  {
        ESMC_LogFile = fopen(nameLogErrFile, "a+");
		    if (ESMC_LogFile==NULL)
		        ok=0;
		    else
		        ok=2;
		    i++;
		    if (i>100000)
		        ok=1;
	  } while (ok<1);
	  if (ok<2)
	      return false;
	  switch(msgtype)
	  {
		  case ESMC_LOG_INFO:
			    fprintf(ESMC_LogFile, "%.2d%.2d%.2d %.2d%.2d%.2d.%.6d %s %s %s\n",
	  		  y,mn,d,h,m,s,ms,"INFO",petnum,msg);
			    break;
		  case ESMC_LOG_WARN:
			    fprintf(ESMC_LogFile, "%.2d%.2d%.2d %.2d%.2d%.2d.%.6d %s %s %s\n",
	  		  y,mn,d,h,m,s,ms,"WARNING",petnum,msg);
			    break;
		  case ESMC_LOG_ERROR:
			    fprintf(ESMC_LogFile, "%.2d%.2d%.2d %.2d%.2d%.2d.%.6d %s %s %s\n",
	  		  y,mn,d,h,m,s,ms,"ERROR",petnum,msg);
			break;
	  }		
    fclose(ESMC_LogFile);
    return true;
}

//----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_logWrite - write to log file
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
// Prints log messsge and returns true if successful.  It takes two arguments -
// msg which is a user message and log type.  This method uses cpp macros
//EOP
{
    int y,mn,d,h,m,s,ms;
    int ok=0;
    int i=0;
	char petnum[8]="PET0";
    ESMC_TimeStamp(&y,&mn,&d,&h,&m,&s,&ms);
	do
	{
        ESMC_LogFile = fopen(nameLogErrFile, "a+");
		if (ESMC_LogFile==NULL)
		    ok=0;
		else
		    ok=2;
		i++;
		if (i>100000)
		    ok=1;
	} while (ok<1);
	if (ok<2)
	    return false;
	switch(msgtype)
	{
		case ESMC_LOG_INFO:
			fprintf(ESMC_LogFile, "%.2d%.2d%.2d %.2d%.2d%.2d.%.6d %s %s %s %d %s %s\n",
	  		y,mn,d,h,m,s,ms,"INFO",petnum,FILE,LINE,method,msg);
			break;
		case ESMC_LOG_WARN:
			fprintf(ESMC_LogFile, "%.2d%.2d%.2d %.2d%.2d%.2d.%.6d %s %s %s %d %s %s\n",
	  		y,mn,d,h,m,s,ms,"WARNING",petnum,FILE,LINE,method,msg);
			break;
		case ESMC_LOG_ERROR:
			fprintf(ESMC_LogFile, "%.2d%.2d%.2d %.2d%.2d%.2d.%.6d %s % s %s %d %s %s\n",
	  		y,mn,d,h,m,s,ms,"ERROR",petnum,FILE,LINE,method,msg);
			break;
	}		
    fclose(ESMC_LogFile);
    return true;
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
    if (rcToReturn != ESMC_NULL_POINTER) *rcToReturn=rcToCheck;
    if (rcToCheck!=ESMF_SUCCESS)
    {
         result=true;
         ESMC_LogWrite(ESMC_LogGetErrMsg(rcToCheck),ESMC_LOG_ERROR);
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
    if (rcToReturn != ESMC_NULL_POINTER) *rcToReturn=rcToCheck;
    if (rcToCheck!=ESMF_SUCCESS)
    {
         result=true;
         ESMC_LogWrite(ESMC_LogGetErrMsg(rcToCheck),ESMC_LOG_ERROR,LINE,FILE,method);
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
    if (rcToReturn != ESMC_NULL_POINTER) *rcToReturn=rcToCheck;
    if (rcToCheck!=ESMF_SUCCESS)
    {
        result=true;
        char logMsg[ESMF_MAXSTR];
        strcpy(logMsg, ESMC_LogGetErrMsg(rcToCheck));
        ESMC_LogWrite(strcat(logMsg,msg),ESMC_LOG_ERROR);
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
    if (rcToReturn != ESMC_NULL_POINTER) *rcToReturn=rcToCheck;
    if (rcToCheck!=ESMF_SUCCESS)
    {
        result=true;   // TODO: if this line moved to after ESMC_LogWrite()
                       // below, will crash ESMF_TimeIntervalUTest.F90 on 
                       // Linux longs 2.4.20-31.9, Lahey lf95 6.0 optimized
        char logMsg[ESMF_MAXSTR];
        strcpy(logMsg, ESMC_LogGetErrMsg(rcToCheck));
        ESMC_LogWrite(strcat(logMsg,msg),ESMC_LOG_ERROR,LINE,FILE,method);
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

