// $Id: ESMC_LogErr.C,v 1.59 2004/05/19 00:52:05 eschwab Exp $
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
// the companion file ESMC\_LogErr.h) provides the user a way to write {\tt ESMC\_Log} data.
//
// insert any higher level, 3rd party or system includes here

#include <stdio.h>        
#include <stdlib.h>
#include <stdarg.h>
//#include <ctype.h>
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
 static const char *const version = "$Id: ESMC_LogErr.C,v 1.59 2004/05/19 00:52:05 eschwab Exp $";
//----------------------------------------------------------------------------
//
// This section includes all the Log routines
//
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
// {\tt ESMC\_LogOpen} takes two
// arguments.  The first should be set to ESMF\_SINGLE\_LOG\_FILE or
// ESMF\_MULT\_LOG\_FILE. These are symbolic constants, defined in
// ESMF\_LogConstants.h, set whether one file should be written for all 
// processes (ESMF\_SINGLE\_LOG\_FILE), or whether one file per process should
// be written (ESMF\_MULT\_LOG\_FILE).
//
// The second argument is a string and is used to form the name of the
// logfile.
//
// This routine is called from native C or C++ code. C I/O libraries are used.
//
//EOP
// 
//

{
    strcpy(nameLogErrFile,filename);
   /*if (!ESMC_LogNameValid(name,ESMF_FALSE) ) {
      printf("File name is already being used.\n");
      ESMC_LogExit();
   } 
   switch(numLogFile) {
    
   case ESMF_SINGLE_LOG_FILE:
       oneLogErrFile=ESMF_TRUE;
       
       break;
      
   case ESMF_MULT_LOG_FILE:
       oneLogErrFile=ESMF_FALSE;
       strcpy(nameLogErrFile,name);
       break;

   default:
     ESMC_LogExit();
  }
  if (oneLogErrFile == ESMF_FALSE) ESMC_LogFormName();
  logErrCFilePtr[numCFiles]=fopen(nameLogErrFile,"a+");
  numFilePtr=numCFiles;
  numCFiles++;
  if (logErrCFilePtr[numCFiles] == NULL) {
     printf("Could not open file.");
     ESMC_LogExit();
  }*/
     
}   //end ESMC_LogOpen

int ESMC_LogSetFilename(
//
// !RETURN VALUE:
//   none
//
// !ARGUMENTS:

     char filename[]     //string to form name of log file (input)

   )
//
// !DESCRIPTION:
// {\tt ESMC\_LogOpen} takes two
// arguments.  The first should be set to ESMF\_SINGLE\_LOG\_FILE or
// ESMF\_MULT\_LOG\_FILE. These are symbolic constants, defined in
// ESMF\_LogConstants.h, set whether one file should be written for all 
// processes (ESMF\_SINGLE\_LOG\_FILE), or whether one file per process should
// be written (ESMF\_MULT\_LOG\_FILE).
//
// The second argument is a string and is used to form the name of the
// logfile.
//
// This routine is called from native C or C++ code. C I/O libraries are used.
//
//EOP
// 
//

{
    strcpy(ESMC_LogDefault.nameLogErrFile,filename);
    return ESMF_SUCCESS;
}   //end ESMC_LogInitialize

int ESMC_LogFinalize(
//
// !RETURN VALUE:
//   none
//
// !ARGUMENTS:

   )
//
// !DESCRIPTION:
// {\tt ESMC\_LogOpen} takes two
// arguments.  The first should be set to ESMF\_SINGLE\_LOG\_FILE or
// ESMF\_MULT\_LOG\_FILE. These are symbolic constants, defined in
// ESMF\_LogConstants.h, set whether one file should be written for all 
// processes (ESMF\_SINGLE\_LOG\_FILE), or whether one file per process should
// be written (ESMF\_MULT\_LOG\_FILE).
//
// The second argument is a string and is used to form the name of the
// logfile.
//
// This routine is called from native C or C++ code. C I/O libraries are used.
//
//EOP
// 
//

{
    return ESMF_SUCCESS;
}   //end ESMC_LogInitialize

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
// This routine simply closes the log file(s).  It also removes
// file from the global file array. The routine is called from native
// C/C++ code (File is closed with C I/O libraries.)
//
//EOP

{
  /*int i,j;

   if (standardOut == ESMF_FALSE) {
     if (logErrCFilePtr[numFilePtr] != NULL) {
       fclose(logErrCFilePtr[numFilePtr]);
     }
    for( i=0; i< numCFiles; i++)
      if (strcmp(nameLogErrFile,listOfCFileNames[i])  == 0) {
       for(j=i+1;j<numCFiles; j++)
          strcpy(listOfCFileNames[j-1],listOfCFileNames[j]);
       for(j=i+1;j<numCFiles; j++)
	  logErrCFilePtr[j-1]=logErrCFilePtr[j];
       numCFiles--;
       break;
      } 
   }*/
}



 
//----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_logWrite - write to log file
//
// !INTERFACE:

bool ESMC_Log::ESMC_LogWrite(

// !RETURN VALUE:
//  none
//
// !ARGUMENTS:
	    char msg[],	  // Log Entry
    	int logtype   // Log Type   
      )
// !DESCRIPTION:
// Prints log messsge
//EOP
{
    int y,mn,d,h,m,s,ms;
    int ok=0;
    int i=0;
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
	  switch(logtype)
	  {
		  case ESMC_LOG_INFO:
			    fprintf(ESMC_LogFile, "%.2d%.2d%.2d %.2d%.2d%.2d.%.6d %s %s\n",
	  		  y,mn,d,h,m,s,ms,"INFO",msg);
			    break;
		  case ESMC_LOG_WARN:
			    fprintf(ESMC_LogFile, "%.2d%.2d%.2d %.2d%.2d%.2d.%.6d %s %s\n",
	  		  y,mn,d,h,m,s,ms,"WARNING",msg);
			    break;
		  case ESMC_LOG_ERROR:
			    fprintf(ESMC_LogFile, "%.2d%.2d%.2d %.2d%.2d%.2d.%.6d %s %s\n",
	  		  y,mn,d,h,m,s,ms,"ERROR",msg);
			break;
	  }		
    fclose(ESMC_LogFile);
    return true;
}


bool ESMC_Log::ESMC_LogWrite(

// !RETURN VALUE:
//  none
//
// !ARGUMENTS:
    char msg[],	// Log Entry
    int logtype,// Log Type   
    int LINE,
    char FILE[],
    char method[]
    )
// !DESCRIPTION:
// Prints log messsge, line number, file, directory
//EOP
{
    int y,mn,d,h,m,s,ms;
    int ok=0;
    int i=0;
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
	switch(logtype)
	{
		case ESMC_LOG_INFO:
			fprintf(ESMC_LogFile, "%.2d%.2d%.2d %.2d%.2d%.2d.%.6d %s %s %d %s %s\n",
	  		y,mn,d,h,m,s,ms,"INFO",FILE,LINE,method,msg);
			break;
		case ESMC_LOG_WARN:
			fprintf(ESMC_LogFile, "%.2d%.2d%.2d %.2d%.2d%.2d.%.6d %s %s %d %s %s\n",
	  		y,mn,d,h,m,s,ms,"WARNING",FILE,LINE,method,msg);
			break;
		case ESMC_LOG_ERROR:
			fprintf(ESMC_LogFile, "%.2d%.2d%.2d %.2d%.2d%.2d.%.6d %s %s %d %s %s\n",
	  		y,mn,d,h,m,s,ms,"ERROR",FILE,LINE,method,msg);
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
//  none
//
// !ARGUMENTS:
    int rcToCheck,
    int *rcToReturn
    )
// !DESCRIPTION:
// Prints log messsge, line number, file, directory
//EOP
{
    int result=false;
    if (rcToReturn != ESMC_NULL_POINTER) *rcToReturn=rcToCheck;
    if (rcToCheck!=ESMF_SUCCESS)
    {
         ESMC_LogWrite(ESMC_LogGetErrMsg(rcToCheck),ESMC_LOG_ERROR);
         result=true;
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
//  none
//
// !ARGUMENTS:
    int rcToCheck,
    int LINE,
    char FILE[],
    char method[],
    int *rcToReturn
    )
// !DESCRIPTION:
// Prints log messsge, line number, file, directory
//EOP
{
    int result=false;
    if (rcToReturn != ESMC_NULL_POINTER) *rcToReturn=rcToCheck;
    if (rcToCheck!=ESMF_SUCCESS)
    {
         ESMC_LogWrite(ESMC_LogGetErrMsg(rcToCheck),ESMC_LOG_ERROR,LINE,FILE,method);
         result=true;
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
//  none
//
// !ARGUMENTS:
    int rcToCheck,
    char msg[],
    int *rcToReturn
    )
// !DESCRIPTION:
// Prints log messsge, line number, file, directory
//EOP
{
    int result=false;
    if (rcToReturn != ESMC_NULL_POINTER) *rcToReturn=rcToCheck;
    if (rcToCheck!=ESMF_SUCCESS)
    {
        char logMsg[ESMF_MAXSTR];
        strcpy(logMsg, ESMC_LogGetErrMsg(rcToCheck));
        ESMC_LogWrite(strcat(logMsg,msg),ESMC_LOG_ERROR);
        result=true;
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
//  none
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
// Prints log messsge, line number, file, directory
//EOP
{
    int result=false;
    if (rcToReturn != ESMC_NULL_POINTER) *rcToReturn=rcToCheck;
    if (rcToCheck!=ESMF_SUCCESS)
    {
        char logMsg[ESMF_MAXSTR];
        strcpy(logMsg, ESMC_LogGetErrMsg(rcToCheck));
        ESMC_LogWrite(strcat(logMsg,msg),ESMC_LOG_ERROR,LINE,FILE,method);
        result=true;
    }
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
	int *rcToReturn
      
    )
// !DESCRIPTION:
// Prints log messsge, line number, file, directory
//EOP
{
	int result=false;
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
// Prints log messsge, line number, file, directory
//EOP
{
	int result=false;
	if (rcToReturn != ESMC_NULL_POINTER) *rcToReturn=ESMC_RC_MEM;
	ESMC_LogWrite(ESMC_LogGetErrMsg(ESMC_RC_MEM),ESMC_LOG_ERROR,LINE,FILE,method);
	result=ESMF_TRUE;
	return true;
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
// Prints log messsge, line number, file, directory
//EOP
{
    int result=false;
    char logMsg[ESMF_MAXSTR];
    if (rcToReturn != ESMC_NULL_POINTER) *rcToReturn=ESMC_RC_MEM;
    strcpy(logMsg, ESMC_LogGetErrMsg(ESMC_RC_MEM));
    ESMC_LogWrite(strcat(logMsg,msg),ESMC_LOG_ERROR);
    result=true;
    return result;

}

//----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_LogAllocErr - LogAllocErr
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
// Prints log messsge, line number, file, directory
//EOP
{
    int result=false;
    char logMsg[ESMF_MAXSTR];
    if (rcToReturn != ESMC_NULL_POINTER) *rcToReturn=ESMC_RC_MEM;
    strcpy(logMsg, ESMC_LogGetErrMsg(ESMC_RC_MEM));
    ESMC_LogWrite(strcat(logMsg,msg),ESMC_LOG_ERROR,LINE,FILE,method);
    result=ESMF_TRUE;
    return true;
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
// Prints log messsge, line number, file, directory
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
//  none
//
// !ARGUMENTS:
    int rc
    )
// !DESCRIPTION:
// Gets error message corresponding to rc
//EOP
{
    if (rc == ESMF_SUCCESS) return("Success ");
    if (rc == ESMF_FAILURE) return("Failure ");
    if (rc < 1 || rc > ESMC_MAX_ERRORS) return("Unknown error ");
    return((char *)errMsg[rc-1]);
}

