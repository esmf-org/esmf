// $Id: ESMC_LogErrInterface.C,v 1.1 2003/03/19 16:04:30 shep_smith Exp $
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
// The LogErr class (defined in ESMC_Log.C and declared in
// the companion file ESMC_LogErr.h) provides the user a way to write Log data.
//
// The following public methods are defined: ESMC_LogInfo (native C/C++ 
// method for writing to information to a log file),
// ESMC_LogInfoFortran (the fortran version
// of ESMC_LogPrint), ESMC_LogWrite (another way to write to the log file using
// the fortran write statement) ESMC_LogErrClose (closes any log file which are
// still open), and ESMC_LogErrOpen (initializes and opens the log file).
// See below for a more detailed definition of these methods.
//
//-----------------------------------------------------------------------------
//
// insert any higher level, 3rd party or system includes here
// #include <ESMC.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>
extern FILE* logErrFilePtr[10];
extern int numFileGlobal;
extern FILE* logErrFileFortran[10];
extern int numFileFortGlobal;
extern char listOfFileNames[20][32];
#include "/home/sjs/ESMF/esmf/src/Infrastructure/LogErr/include/ESMC_LogErr.h"
//------------------------------------------------------------------------
// Interface layer for F90
// These routines are used by the wrapper routines to call the corresponding
// Log methods.
//---------------------------------------------------------------------
//

extern "C" {


void FTN(esmf_logclosefile)(ESMC_Log* aLog)
{
//     aLog->ESMC_LogCloseFile();
       aLog->ESMC_LogCloseFileForWrite();
}

void FTN(esmf_loginit_c)(ESMC_Log* aLog,int* verbose, int* flush,
     int* haltOnErr, int* haltOnWarn)
{
   aLog->ESMC_LogInit(*verbose,*flush,*haltOnErr,*haltOnWarn);
}

void FTN(esmf_logopenfile)(ESMC_Log* aLog,int* numFiles,char name[])
{
  char c_name[32];
  bool foundSpace=false;
  int i=0;

  while (i<32 && !foundSpace) {
    if (isspace(name[i])) { 
       c_name[i]='\0';
       foundSpace=true;
    } else {
       c_name[i]=name[i];
    }
    i++;
  }
  if (!foundSpace) c_name[32]='\0';
//  aLog->ESMC_LogOpenFile(*numFiles, c_name);
    aLog->ESMC_LogOpenFileForWrite(*numFiles, c_name);
}

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

	  
	  
//    #include "ESMF_LogPrintf.inc"
    aLog->ESMC_LogInfoFortran(fmt, charData,strData, intData, floatData);
  }

          
void FTN(esmf_logwarnmsg_)(ESMC_Log* aLog,int* errCode, int* line,
  char file[],char dir[],char msg[])

{
   aLog->ESMC_LogWarningFortran(*errCode, *line,file,dir,msg);

}


void FTN(esmf_logwarn_)(ESMC_Log* aLog,int* errCode, int* line,
  char file[],char dir[])

{
   char msg[32];      
   msg[0]=NULL;
   aLog->ESMC_LogWarningFortran(*errCode, *line,file,dir,msg);

}
void FTN(esmf_logflush)(ESMC_Log* aLog)
{
   aLog->ESMC_LogFlush();
}

void FTN(esmf_lognotflush)(ESMC_Log* aLog)
{
   aLog->ESMC_LogNotFlush();
}

void FTN(esmf_logverbose)(ESMC_Log* aLog)
{
  aLog->ESMC_LogVerbose();
}

void FTN(esmf_lognoflush)(ESMC_Log* aLog)
{
   aLog->ESMC_LogNotFlush();
}


int  FTN(logwrite)(ESMC_Log *aLog)
{
    return aLog->ESMC_LogWrite();
}


void FTN(esmf_logerrmsg_)(ESMC_Log* aLog, int* errCode,int* line, char file[],
                     char dir[],char msg[])
{
    aLog->ESMC_LogErrFortran(*errCode,*line,file,dir,msg);
} 


void FTN(esmf_logerr_)(ESMC_Log* aLog, int* errCode, int* line, char file[],
                     char dir[])
{
    char msg[32];
    msg[0]=NULL;
    aLog->ESMC_LogErrFortran(*errCode,*line,file,dir,msg);
} 



void FTN(esmf_loghaltonerr)(ESMC_Log* aLog) 
{
    aLog->ESMC_LogHaltOnErr();
}


void FTN(esmf_lognothaltonerr)(ESMC_Log* aLog)
{
    aLog->ESMC_LogNotHaltOnErr();
}

void FTN(esmf_loghaltonwarn)(ESMC_Log* aLog)
{
   aLog->ESMC_LogHaltOnWarn();
}


void FTN(esmf_lognothaltonwarn)(ESMC_Log* aLog)
{
  aLog->ESMC_LogNotHaltOnWarn();
}


    
}
