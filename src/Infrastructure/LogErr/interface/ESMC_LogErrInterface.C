// $Id: ESMC_LogErrInterface.C,v 1.8 2003/04/15 20:21:37 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

//---------------------------------------------------------------------------
// This Fortran interface to the ESMC_Log class is written in both C and
// Fortran.  This file contains the interface code written in C.
//----------------------------------------------------------------------------
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>

#include <ESMC.h>
#include "ESMC_LogErr.h"

//Global Variables
extern FILE* logErrCFilePtr[10];
extern int numCFiles;
extern int logErrFortFile[10];
extern int numFortFiles;
extern char listOfCFileNames[20][32];
extern char listOfFortFileNames[20][32];

extern "C" {
//------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMF_LogCloseFile - closes a file from Fortran code
//
// !INTERFACE 
// subroutine ESMF_LogCloseFile(aLog)
//
// !ARGUMENTS:
//   typdef(ESMF_Log) :: aLog
// 
// !DESCRIPTION:
// Calls the method ESMC\_LogCloseFileForWrite to close aLog's 
// log file.
//
//EOP
void FTN(esmf_logclosefile)(ESMC_Log* aLog)
{
       aLog->ESMC_LogCloseFortFile();
}

void FTN(esmf_loginit_c)(ESMC_Log* aLog,int* verbose, int* flush,
     int* haltOnErr, int* haltOnWarn)
{
   aLog->ESMC_LogInit(*verbose,*flush,*haltOnErr,*haltOnWarn);
}

//--------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMF_LogOpenFile - opens a log file
// !INTERFACE: 
// subroutine ESMF_LogOpenFile(aLog, numFile, name)
//
// !ARGUMENTS:
//
//  typdef(ESMF_Log) :: aLog
//
//  integer :: numFile           !! set to either ESMF_SINGLE_FILE
//                               !! or ESMF_MULTIPLE_FILE 
//
//  character(len=*) :: name     !! name of file
//
// !DESCRIPTION:
// This routine finds the first space in the array name and inserts a
// a null character. It then calls ESMC\_LogOpenFileForWrite 
// an ESMC\_Log method for opening files.
//
//EOP
//----------------------------------------------------------------------------

void FTN(esmf_logopenfile)(ESMC_Log *aLog, int *numFiles, char name[], 
                                                                 int namelen)
{
    char *nameCopy = NULL;

    if ((name != NULL) && (namelen > 0)) {
        nameCopy = new char[namelen+1];
        strncpy(nameCopy, name, namelen);
        nameCopy[namelen] = '\0';
    }

    aLog->ESMC_LogOpenFortFile(*numFiles, nameCopy);

    if (nameCopy != NULL)
        delete[] nameCopy;
  
}

//-----------------------------------------------------------------------
//BOP
// !IROUTINE: ESMF_LogInfo -  writes miscellaneous information to a log file
//
// !INTERFACE:
//   
//  ESMF_LogInfo(aLog, fmt, ...)
//
// !ARGUMENTS:
//
//  typedef(ESMF_LogInfo) :: aLog   ! log object
//  character(len=*) :: fmt         !c-style character descriptior
//
// !DESCRIPTION:
//  This routine allows the user to write miscellaneous information the
//  Log file. It uses a printf style character descriptor, e.g. 
//  ESMF\_LogInfo(aLog,"Hi there, %s ", shep), where shep here would be
//  a character string. The routine takes a variable number of arguments,
//  so that any number of data items can be written to the Log file.
//  Currently, only character, strings, integers, and reals are supported.
//  However, field widths, precisions, and flags are ignored.
//
//EOP
//-----------------------------------------------------------------------
void FTN(esmf_loginfo)(ESMC_Log* aLog, char* fmt,...){
    int intData[32];
    double floatData[32];
    char charData[32];
    char strData[32][32];
    int numChar=0;
    int  numInt=0;
    int  numFloat=0;
    int  numStr=0;
    va_list argp;
    va_start(argp, fmt);
    char* chPtr;

    va_start(argp,fmt);
    for (chPtr=fmt; *chPtr; chPtr++) {
     if (*chPtr == '%') {
       chPtr++;
       while (*chPtr == '-' || isdigit(*chPtr) || *chPtr == '.' ||
              *chPtr == 'h' || *chPtr=='l') chPtr++;
       switch (*chPtr) {
        case 'c':
         if (numChar < 32 ) {
           charData[numChar++]=*(va_arg(argp, char*));
         } else {
           #ifdef HAS_MPI
             if ( MPI::Is_Initialized() ) {
                 MPI::COMM_WORLD.Abort(-1);
             } else {
                 exit(EXIT_FAILURE);
             }
           #else
             exit(EXIT_FAILURE);
           #endif
         }
         break;
        case 's':
         if (numStr < 32 ) {
           strcpy(strData[numStr],va_arg(argp, char*));
           strData[numStr][31]='\0'; //just in case there is no null
           numStr++;
         } else {
           #ifdef HAS_MPI
             if ( MPI::Is_Initialized() ) {
                 MPI::COMM_WORLD.Abort(-1);
             } else {
                 exit(EXIT_FAILURE);
             }
           #else
             exit(EXIT_FAILURE);
           #endif
         }
         break;
        case 'd':
         if (numInt < 32) {
           intData[numInt++]=*(va_arg(argp, int*));
         } else {
           #ifdef HAS_MPI
             if ( MPI::Is_Initialized() ) {
                 MPI::COMM_WORLD.Abort(-1);
             } else {
                 exit(EXIT_FAILURE);
             }
           #else
             exit(EXIT_FAILURE);
           #endif
         }
         break;
        case 'f':
          if (numFloat< 32) {
	     floatData[numFloat++]=*(va_arg(argp, double*));
          } else {
           #ifdef HAS_MPI
             if ( MPI::Is_Initialized() ) {
                 MPI::COMM_WORLD.Abort(-1);
             } else {
                 exit(EXIT_FAILURE);
             }
           #else
             exit(EXIT_FAILURE);
           #endif
         }
          break;
         default:
           #ifdef HAS_MPI
             if ( MPI::Is_Initialized() ) {
                 MPI::COMM_WORLD.Abort(-1);
             } else {
                 exit(EXIT_FAILURE);
             }
           #else
             exit(EXIT_FAILURE);
           #endif
         }
        }
      }

	  
	  
    aLog->ESMC_LogInfoFortran(fmt, charData,strData, intData, floatData);
  }

          
//-----------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMF_LogWarnMsg  - writes a warning message to the log file
//
// !INTERFACE:
//
//  subroutine ESMF_LogWarnMsg(aLog, errCode, line,file,dir,msg)
//
// !ARGUMENTS:
//    typdef(ESMF_Log)::aLog
//
//    integer :: errCode         !integer value for error code         
//
//    character(len=*) :: msg    !msg written to log file
//
//    integer :: line            !line number of warning; argument
//                               !supplied by macro
//
//    character(len=*) :: file   !file where warning occurred in;
//                               !argument supplied by macro
//
//    character(len=*) :: dir    !directory where warning occurred in;
//                               !argument supplied by macro
//
// !DESCRIPTION:
//    This routine writes a warning message to the log file.  This warning
//    message consists of the erroCode, a description of the warning, the 
//    line number, file, and directory of the error, and a message. A 
//    preprocessor macro adds the predefined preprocessor symbolic
//    constants \_\_LINE\_\_, \_\_FILE\_\_, and \_\_DIR\_\_.
//    The macro operates on
//    the file from which these routines are called.  Note,
//    the value of \_\_DIR\_\_ 
//    must be suppliled by the user (usually done in
//    the makefile.).  By default, execution continues after encountering
//    a warning, but by calling the routine ESMF\_LogWarnHalt(), the user
//    can halt on warnings.
//
//EOP
//--------------------------------------------------------------------------
void FTN(esmf_logwarnmsg)(ESMC_Log *aLog,int *errCode, char msg[], int msglen)

{
    int line = 0;
    char *file = NULL;
    char *dir = NULL;
    char *msgCopy = NULL;

    if ((msg != NULL) && (msglen > 0)) {
        msgCopy = new char[msglen+1];
        strncpy(msgCopy, msg, msglen);
        msgCopy[msglen] = '\0';
    }

    aLog->ESMC_LogWarnFortran(*errCode, line, file, dir, msgCopy);

    if (msgCopy != NULL)
        delete[] msgCopy;
  
}


void FTN(esmf_logwarnmsg_ln)(ESMC_Log *aLog,int *errCode, int *line,
                             char file[], char dir[], char msg[],
                             int filelen, int dirlen, int msglen)

{
    char *fileCopy = NULL;
    char *dirCopy = NULL;
    char *msgCopy = NULL;

    if ((file != NULL) && (filelen > 0)) {
        fileCopy = new char[filelen+1];
        strncpy(fileCopy, file, filelen);
        fileCopy[filelen] = '\0';
    }

    if ((dir != NULL) && (dirlen > 0)) {
        dirCopy = new char[dirlen+1];
        strncpy(dirCopy, dir, dirlen);
        dirCopy[dirlen] = '\0';
    }

    if ((msg != NULL) && (msglen > 0)) {
        msgCopy = new char[msglen+1];
        strncpy(msgCopy, msg, msglen);
        msgCopy[msglen] = '\0';
    }

    aLog->ESMC_LogWarnFortran(*errCode, *line, fileCopy, dirCopy, msgCopy);

    if (fileCopy != NULL)
        delete[] fileCopy;
  
    if (dirCopy != NULL)
        delete[] dirCopy;
  
    if (msgCopy != NULL)
        delete[] msgCopy;
  
}


//-----------------------------------------------------------------------
//BOP
// !IROUTINE: ESMF_LogWarn - writes a warning message to log file
//
// !INTERFACE:
//    ESMF_LogWarn(aLog, errCode)
//
// !ARGUMENTS:
//   typdef(ESMF_Log) :: aLog
//   integer :: errCode
//
// !DESCRIPTION:
//   This routine is identical to ESMF\_LogWarnMsg, except a msg is
//   not written to the log file.
//
//EOP
//-----------------------------------------------------------------------
void FTN(esmf_logwarn)(ESMC_Log *aLog, int *errCode)

{
   int line = 0;
   char *file = NULL;
   char *dir = NULL;
   char *msg = NULL; 

   aLog->ESMC_LogWarnFortran(*errCode, line, file, dir, msg);

}

void FTN(esmf_logwarn_ln)(ESMC_Log *aLog, int *errCode, int *line,
                          char file[], char dir[], int filelen, int dirlen)

{
    char *fileCopy = NULL;
    char *dirCopy = NULL;
    char *msg = NULL;
 
    if ((file != NULL) && (filelen > 0)) {
        fileCopy = new char[filelen+1];
        strncpy(fileCopy, file, filelen);
        fileCopy[filelen] = '\0';
    }

    if ((dir != NULL) && (dirlen > 0)) {
        dirCopy = new char[dirlen+1];
        strncpy(dirCopy, dir, dirlen);
        dirCopy[dirlen] = '\0';
    }

    aLog->ESMC_LogWarnFortran(*errCode, *line, fileCopy, dirCopy, msg);

    if (fileCopy != NULL)
        delete[] fileCopy;
  
    if (dirCopy != NULL)
        delete[] dirCopy;
  
}

//------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMF_LogFlush - flushes output
//
// !INTERFACE:
//    ESMF_logflush(aLog)
//
// !ARGUMENTS:
//    typdef(ESMF_Log) :: aLog
//
// !DESCRIPTION:
//    This routine calls the Log method ESMC\_LogFlush() which sets a flag
//    that causes all output from the buffers.
//
//EOP
//---------------------------------------------------------------------------
void FTN(esmf_logflush)(ESMC_Log* aLog)
{
   aLog->ESMC_LogFlush();
}

//---------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMF_LogNotFlush - prevents output from being flushed
//
// !INTERFACE:
//    ESMF_LogNotFlush(aLog)
//
// !ARGUMENTS:
//    typdef(ESMF_Log) :: aLog
// !DESCRIPTION:
//    This routine calls the Log method ESMC\_LogNotFlush() which sets a flag
//    that turns off flushing. By default, this flag is set.
//
//EOP
//---------------------------------------------------------------------------

void FTN(esmf_lognotflush)(ESMC_Log* aLog)
{
   aLog->ESMC_LogNotFlush();
}

//---------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMF_LogVerbose -  causes output to be written to the Log
//
// !INTERFACE:
//   ESMF_LogVerbose(aLog)
// !ARGUMENTS:
//    typdef(ESMF_Log) :: aLog
//
// !DESCRIPTION:
//    This routine sets a flag that causes all output associated with
//    the aLog ESMC\_Log handle to be written.
//EOP
//------------------------------------------------------------------------

void FTN(esmf_logverbose)(ESMC_Log* aLog)
{
  aLog->ESMC_LogVerbose();
}

//---------------------------------------------------------------------------
//BOP                    
// !IROUTINE: ESMF_LogNotVerbose -  causes output not to be written to the Log
//                       
// !INTERFACE:            
//   ESMF_LogVerbose(aLog)
// !ARGUMENTS:            
//    typdef(ESMF_Log) :: aLog
//
// !DESCRIPTION:
//    This routine sets a flag that forces all output associated with
//    the aLog ESMC\_Log handle from being written.
//EOP
//------------------------------------------------------------------------
			 
void FTN(esmf_lognotverbose)(ESMC_Log* aLog)
{
   aLog->ESMC_LogNotVerbose();
}



//------------------------------------------------------------------------
//BOP
//
// !IROUTINE: LogWrite - Fortran style method to write to log file.
//
// !INTERFACE:
//    LogWrite(aLog)
// !ARGUMENTS:
//   typdef(ESMF_Log) :: aLog
//
// !DESCRIPTION:
//    This function called from with a Fortran write statement, e.g.
//    write(LogWrite(aLog),*)"Hi".  The LogWrite function appends some
//    header information (time,date etc.) to what ever is printed out
//    from the write, e.g. Hi.
//EOP
//------------------------------------------------------------------------------
int  FTN(logwrite)(ESMC_Log *aLog)
{
    return aLog->ESMC_LogWrite();
}


//-----------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMF_LogErrMsg - writes a err message to the log file
//
// !INTERFACE:
//
//  subroutine ESMF_LogErrMsg(aLog, errCode, line,file,dir,msg)
//
// !ARGUMENTS:
//    typdef(ESMF_Log)::aLog
//
//    integer :: errCode         !integer value for error code
//
//    character(len=*) :: msg    !msg written to log file
//
//    integer :: line            !line number of warning; argument
//                               !supplied by macro
//
//    character(len=*) :: file   !file where warning occurred in;
//                               !argument supplied by macro
//
//    character(len=*) :: dir    !directory where warning occurred in;
//                               !argument supplied by macro
//
// !DESCRIPTION:
//    This routine writes a warning message to the log file.  This warning
//    message consists of the erroCode, a description of the warning, the
//    line number, file, and directory of the error, and a message. A
//    preprocessor macro adds the predefined preprocessor symbolic
//    constants \_\_LINE\_\_, \_\_FILE\_\_, and \_\_DIR\_\_.
//    The macro operates on
//    the file from which these routines are called.  Note,
//    the value of \_\_DIR\_\_
//    must be suppliled by the user (usually done in
//    the makefile).  By default, execution continues after encountering
//    a warning, but by calling the routine ESMF\_LogWarnHalt(), the user
//    can halt on warnings.
//
//EOP
//--------------------------------------------------------------------------

void FTN(esmf_logerrmsg)(ESMC_Log *aLog, int *errCode, char msg[], int msglen)
{
    int line = 0;
    char *file = NULL;
    char *dir = NULL;
    char *msgCopy = NULL;

    if ((msg != NULL) && (msglen > 0)) {
        msgCopy = new char[msglen+1];
        strncpy(msgCopy, msg, msglen);
        msgCopy[msglen] = '\0';
    }

    aLog->ESMC_LogErrFortran(*errCode, line, file, dir, msgCopy);
  
    if (msgCopy != NULL)
        delete[] msgCopy;
} 


void FTN(esmf_logerrmsg_ln)(ESMC_Log *aLog, int *errCode, int *line, 
                            char file[], char dir[], char msg[], 
                            int filelen, int dirlen, int msglen)
{
    char *fileCopy = NULL;
    char *dirCopy = NULL;
    char *msgCopy = NULL;

    if ((file != NULL) && (filelen > 0)) {
        fileCopy = new char[filelen+1];
        strncpy(fileCopy, file, filelen);
        fileCopy[filelen] = '\0';
    }

    if ((dir != NULL) && (dirlen > 0)) {
        dirCopy = new char[dirlen+1];
        strncpy(dirCopy, dir, dirlen);
        dirCopy[dirlen] = '\0';
    }

    if ((msg != NULL) && (msglen > 0)) {
        msgCopy = new char[msglen+1];
        strncpy(msgCopy, msg, msglen);
        msgCopy[msglen] = '\0';
    }

    aLog->ESMC_LogErrFortran(*errCode, *line, fileCopy, dirCopy, msgCopy);
  
    if (fileCopy != NULL)
        delete[] fileCopy;
  
    if (dirCopy != NULL)
        delete[] dirCopy;
  
    if (msgCopy != NULL)
        delete[] msgCopy;
} 



//-----------------------------------------------------------------------
//BOP
// !IROUTINE: ESMF_LogErr - writes a warning message to log file
//
// !INTERFACE:
//    ESMF_LogErr(aLog, errCode)
//
// !ARGUMENTS:
//   typdef(ESMF_Log) :: aLog
//   integer :: errCode
//
// !DESCRIPTION:
//   This routine is identical to ESMF\_LogErrMsg, except a msg is
//   not written to the log file.
//
//EOP
//-----------------------------------------------------------------------

void FTN(esmf_logerr)(ESMC_Log* aLog, int* errCode)
{
    int line = 0;
    char *file = NULL;
    char *dir = NULL;
    char *msg = NULL;
   
    aLog->ESMC_LogErrFortran(*errCode, line, file, dir, msg);
} 

void FTN(esmf_logerr_ln)(ESMC_Log *aLog, int *errCode, int *line, 
                         char file[], char dir[], int filelen, int dirlen)
{
    char *fileCopy = NULL;
    char *dirCopy = NULL;
    char *msg = NULL;

    if ((file != NULL) && (filelen > 0)) {
        fileCopy = new char[filelen+1];
        strncpy(fileCopy, file, filelen);
        fileCopy[filelen] = '\0';
    }
   
    if ((dir != NULL) && (dirlen > 0)) {
        dirCopy = new char[dirlen+1];
        strncpy(dirCopy, dir, dirlen);
        dirCopy[dirlen] = '\0';
    }

    aLog->ESMC_LogErrFortran(*errCode, *line, file, dir, msg);


    if (fileCopy != NULL)
        delete[] fileCopy;
  
    if (dirCopy != NULL)
        delete[] dirCopy;
  
} 

//---------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMF_LogHaltOnErr - program halts on encountering an error
//
// !INTERFACE:
//    subroutine ESMF_LogHaltOnErr(aLog)
// !ARGUMENTS;
//    typdef(ESMF_Log) :: aLog
//
// !DESCRIPTION:
//    This routine calls a Log method that sets a flag to stop execution on
//    reaching an error. This is the default behavior of the Log class.
//EOP
//--------------------------------------------------------------------------
void FTN(esmf_loghaltonerr)(ESMC_Log* aLog) 
{
    aLog->ESMC_LogHaltOnErr();
}


//---------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMF_LogNotHaltOnErr - program does not halt on encountering an error
//
// !INTERFACE:
//    subroutine ESMF_LogNotHaltOnErr(aLog)
// !ARGUMENTS:
//    typdef(ESMF_Log) :: aLog
//
// !DESCRIPTION:
//    This routine calls a Log method that sets a flag to prevent the program
//    from stopping reaching an error. 
//EOP
//--------------------------------------------------------------------------

void FTN(esmf_lognothaltonerr)(ESMC_Log* aLog)
{
    aLog->ESMC_LogNotHaltOnErr();
}

//---------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMF_LogHaltOnWarn - program halts on encountering a warning
//
// !INTERFACE:
//    subroutine ESMF_LogHaltOnWarn(aLog)
// !ARGUMENTS:
//    typdef(ESMF_Log) :: aLog
//
// !DESCRIPTION:
//    This routine calls a Log method that sets a flag to stop execution on
//    reaching a warning.
//EOP
//--------------------------------------------------------------------------
void FTN(esmf_loghaltonwarn)(ESMC_Log* aLog)
{
   aLog->ESMC_LogHaltOnWarn();
}


//---------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMF_LogNotHaltOnWarn - program does not halt on encountering a warning
//
// !INTERFACE;
//    subroutine ESMF_LogNotHaltOnWarn(aLog)             
// !ARGUMENTS:                                        
//    typdef(ESMF_Log) :: aLog                                        
//                              
// !DESCRIPTION:                              
//    This routine calls a Log method that sets a flag to prevent the program
//    from stopping reaching an error.                            
//EOP                                                                           
//--------------------------------------------------------------------------

void FTN(esmf_lognothaltonwarn)(ESMC_Log* aLog)
{
  aLog->ESMC_LogNotHaltOnWarn();
}


    
}
