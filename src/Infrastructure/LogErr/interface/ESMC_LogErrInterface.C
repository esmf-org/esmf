// $Id: ESMC_LogErrInterface.C,v 1.17 2004/03/19 07:16:52 cpboulder Exp $
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
#include <ESMC_Base.h>
#include "../include/ESMC_LogErr.h"

//Global Variables
extern FILE* logErrCFilePtr[10];
extern int numCFiles;
extern int logErrFortFile[10];
extern int numFortFiles;
extern char listOfCFileNames[20][32];
extern char listOfFortFileNames[20][32];

//-----------------------------------------------------------------------------------
//BOP
// These wrapper function are called by the FORTRAN routines defined in the file 
// ESMF\_LogErr.F90.  The structure is:
//  
//    ESMF\_LogFoo(aLog) --->  C\_ESMF\_LogFoo(aLog)  ----> aLog.ESMC\_LogFoo()
// In other words, these routines actually call the C++ methods.  You can't call
// the C++ methods directly from FORTRAN.  You must go thru this intermediate step.
//
//EOP
//-------------------------------------------------------------------------------------------

extern "C" {
//------------------------------------------------------------------------
//BOP
// !IROUTINE: C_ESMF_LogCloseFile - closes a file from Fortran code
//
// !INTERFACE: 

void FTN(c_esmf_logclosefile)(
//
// !RETURN VALUE:
// none
//
// !ARGUMENTS:
        ESMC_Log* aLog)
//
// !DESCRIPTION:
// Calls the method {\tt ESMC\_LogCloseFileForWrite} to close aLog's 
// log file.
//
//EOP
//
{
       aLog->ESMC_LogCloseFortFile();
}

//--------------------------------------------------------------------------
//BOP
//
// !IROUTINE: C_ESMF_LogOpenFile - opens a log file
// !INTERFACE: 
//
   void FTN(c_esmf_logopenfile)(
//
// !RETURN VALUE:
//   none
//
// !ARGUMENTS:
        ESMC_Log *aLog, 
        int *numFiles, 
        char name[], 
        int namelen)
//
// !DESCRIPTION:
// This routine finds the first space in the array name and inserts a
// a null character. It then calls {\tt ESMC\_LogOpenFileForWrite} 
// an {\tt ESMC\_Log} method for opening files.
//EOP
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

//---------------------------------------------------------------------------------
//BOP
// !IROUTINE: C_ESMF_LogWrite- writes messages
//
// !INTERFACE:
     void FTN(c_esmf_logwrite)(
//
// !RETURN VALUE:
// none
//
// !ARGUMENTS:
      ESMC_Log *aLog,
      char msg[],
      int msglen)
//
// !DESCRIPTION:
//    This routine is called by {\tt ESMF\_LogWarnMsg} (defined in ESMF\_LogErr.F90).  
//    {\tt C\_ESMF\_LogWarnMsg} calls the C++ method that actually writes the warning.
//
//EOP
//-------------------------------------------------------------------------
{
    char *msgCopy = NULL;

    if ((msg != NULL) && (msglen > 0)) {
        msgCopy = new char[msglen+1];
        strncpy(msgCopy, msg, msglen);
        msgCopy[msglen] = '\0';
    }

    //aLog->ESMC_LogWarnFortran(*errCode, *line, fileCopy, dirCopy, msgCopy);

    if (msgCopy != NULL)
        delete[] msgCopy;
  
}
//-----------------------------------------------------------------------
//BOP
// !IROUTINE: ESMF_LogInfo -  writes miscellaneous information to a log file
//
// !INTERFACE:
//
   void FTN(esmf_loginfo)(
//
// !RETURN VALUE:
//   none
//
// !ARGUMENTS:
      ESMC_Log* aLog,
      char* fmt,...)
//
// !DESCRIPTION:
//  This routine allows the user to write miscellaneous information the
//  {\tt ESMC\_Log} file. It uses a printf style character descriptor, e.g. 
//  {\tt ESMC\_LogInfo}(aLog,"Hi there, %s ", shep), where shep here would be
//  a character string. The routine takes a variable number of arguments,
//  so that any number of data items can be written to the {\tt ESMC\_Log} file.
//  Currently, only character, strings, integers, and reals are supported.
//  However, field widths, precisions, and flags are ignored.
//
//EOP
{
    int intData[32];
    double floatData[32];
    char charData[32];
    char strData[32][32];
    int numChar=0;
    int  numInt=0;
    int  numFloat=0;
    int  numStr=0;
    va_list argp;
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

          
//---------------------------------------------------------------------------------
//BOP
// !IROUTINE: C_ESMF_LogWarnMsg- writes warning messages
//
// !INTERFACE:
     void FTN(c_esmf_logwarnmsg)(
//
// !RETURN VALUE:
// none
//
// !ARGUMENTS:
      ESMC_Log *aLog,
      int *errCode,
      int *line,
      char file[],
      char dir[],
      char msg[],
      int filelen,
      int dirlen,
      int msglen)
//
// !DESCRIPTION:
//    This routine is called by {\tt ESMF\_LogWarnMsg} (defined in ESMF\_LogErr.F90).  
//    {\tt C\_ESMF\_LogWarnMsg} calls the C++ method that actually writes the warning.
//
//EOP
//-------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
//BOP
// !IROUTINE: C_ESMF_LogWarn - writes warning messages
//
// !INTERFACE:
//
      void FTN(c_esmf_logwarn)(
// !RETURN VALUES:
// none
//
// !ARGUMENTS:
//
       ESMC_Log *aLog,
       int *errCode,
       int *line,
       char file[],
       char dir[],
       int filelen,
       int dirlen)
//
// !DESCRIPTION:
//    This routine is called by {\tt ESMC\_LogWarn} (defined in ESMF\_LogErr.F90).  
//    {\tt C\_ESMC\_LogWarn} calls the C++ method that actually writes the warning.
//
//EOP
//-------------------------------------------------------------------------
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
// !IROUTINE: C_ESMF_LogSetFlush - flushes output
//
// !INTERFACE:
       void FTN(c_esmf_logsetflush)(
// !RETURN VALUE:
//     none
// !ARGUMENTS:
//  
      ESMC_Log* aLog)
// !DESCRIPTION:
//  This routine calls the {\tt ESMC\_Log} method that flushes output.
//
//EOP
//--------------------------------------------------------------------------
{
   aLog->ESMC_LogSetFlush();
}

//---------------------------------------------------------------------------
//BOP
// !IROUTINE: C_ESMF_LogSetNotFlush - prevents output from being flushed
//
// !INTERFACE:
     void FTN(c_esmf_logsetnotflush)(
// !RETURN VALUE:
//   none
// !ARGUMENTS:
      ESMC_Log* aLog)

//
// !DESCRIPTION:
//    This routine calls the Log method {\tt ESMC\_LogNotFlush()} which sets a flag
//    that turns off flushing. By default, this flag is set.
//
//EOP
//---------------------------------------------------------------------------
{
   aLog->ESMC_LogSetNotFlush();
}

//---------------------------------------------------------------------------
//BOP
// !IROUTINE: C_ESMF_LogSetVerbose -  causes output to be written to the Log
//
// !INTERFACE:
    void FTN(c_esmf_logsetverbose)(
// !RETURN VALUE:
//   none
// !ARGUMENTS:
     ESMC_Log* aLog)
//
// !DESCRIPTION:
//    This routine sets a flag that causes all output associated with
//    the aLog {\tt ESMC\_Log} handle to be written.
//EOP
//------------------------------------------------------------------------
{
  aLog->ESMC_LogSetVerbose();
}

//---------------------------------------------------------------------------
//BOP                    
// !IROUTINE: C_ESMF_LogNotVerbose -  causes output not to be written to the Log
//                       
// !INTERFACE:
     void FTN(c_esmf_logsetnotverbose)(
// !RETURN VALUE:
//    NONE
//!ARGUMENTS:
//
    ESMC_Log* aLog) 
//
// !DESCRIPTION:
//    This routine sets a flag that forces all output associated with
//    the aLog {\tt ESMC\_Log} handle from being written.
//EOP
//------------------------------------------------------------------------
{
   aLog->ESMC_LogSetNotVerbose();
}


//------------------------------------------------------------------------
//BOP
//
// !IROUTINE: C_ESMF_LogGetUnit - Fortran style method to write to log file.
//
// !INTERFACE:
     void  FTN(c_esmf_loggetunit)(
//  !RETURN VALUE:
//    A Fortran Unit Number
//
// !ARGUMENTS:
//
     ESMC_Log *aLog)
//
// !DESCRIPTION:
//    This function called from {tt\ ESMF\_GetUnit.}  See this function
//    for more details.
//EOP
//------------------------------------------------------------------------------
{
    aLog->ESMC_LogGetUnit();
}


//----------------------------------------------------------------------------------
//BOP
// !IROUTINE: C_ESMF_LogErrMsg - writes warning messages
//
// !INTERFACE:
    void FTN(c_esmf_logerrmsg)(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//
//
    ESMC_Log *aLog,
    int *errCode,
    int *line, 
    char file[],
    char dir[],
    char msg[], 
    int filelen,
    int dirlen,
    int msglen)
//
// !DESCRIPTION:
//    This routine is called by {\tt ESMF\_LogErrMsg} (defined in ESMF\_LogErr.F90).  
//    {\tt C\_ESMF\_LogErrMsg} calls the C++ method that actually writes
//    the warning.
//
//EOP
//----------------------------------------------------------------------------------------------
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


//-------------------------------------------------------------------------------------------
//BOP
// !IROUTINE: C_ESMF_LogErr - writes warning messages
//
// !INTERFACE:
     void FTN(c_esmf_logerr)(
// !RETURN VALUE:
//  none
// !ARGUMENTS:
//
     ESMC_Log *aLog,
     int *errCode,
     int *line, 
     char file[],
     char dir[],
     int filelen,
     int dirlen)
//
// !DESCRIPTION:
//    This routine is called by {\tt ESMF\_LogErr} (defined in ESMF\_LogErr.F90). 
//    {\tt C\_ESMC\_LogErr} calls the C++ method that actually writes the warning.
//
//EOP
//----------------------------------------------------------------------------------------------
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
// !IROUTINE: C_ESMF_LogSetHaltOnErr - program halts on encountering an error
//
// !INTERFACE:
    void FTN(c_esmf_logsethaltonerr)(
// !RETURN VALUE:
//  none
// !ARGUMENTS:
    ESMC_Log* aLog)
// !DESCRIPTION:
//    This routine calls a {\tt ESMC\_Log} method that sets
//    a flag to stop execution on
//    reaching an error. This is the default behavior of the {\tt ESMC\_Log} class.
//EOP
//--------------------------------------------------------------------------
{
    aLog->ESMC_LogSetHaltOnErr();
}


//---------------------------------------------------------------------------
//BOP
//
// !IROUTINE: C_ESMF_LogSetNotHaltOnErr - program does not halt on an error
//
// !INTERFACE:
   void FTN(c_esmf_logsetnothaltonerr)(
// !RETURN VALUE:
//   none
// !ARGUMENTS:
    ESMC_Log* aLog)
//
// !DESCRIPTION:
//    This routine calls a {\tt ESMC\_Log} method that sets a flag to 
//    prevent the program
//    from stopping reaching an error. 
//EOP
//--------------------------------------------------------------------------
{
    aLog->ESMC_LogSetNotHaltOnErr();
}

//---------------------------------------------------------------------------
//BOP
//
// !IROUTINE: C_ESMF_LogSetHaltOnWarn - program halts on encountering a warning
//
// !INTERFACE:
void FTN(c_esmf_logsethaltonwarn)(
// !RETURN VALUE:
//  none
// !ARGUMENTS:
    ESMC_Log* aLog)
//
// !DESCRIPTION:
//    This routine calls a {\tt ESMC\_Log} method that sets a flag to stop execution on
//    reaching a warning.
//EOP
//--------------------------------------------------------------------------
{
   aLog->ESMC_LogSetHaltOnWarn();
}


//---------------------------------------------------------------------------
//BOP
//
// !IROUTINE: C_ESMF_LogSetNotHaltOnWarn - program does not halt on warning
//
// !INTERFACE:
    void FTN(c_esmf_logsetnothaltonwarn)( 
// !RETURN VALUE:
//   none
// !ARGUMENTS:
      ESMC_Log* aLog)
//                      
// !DESCRIPTION:                              
//  This routine calls a {\tt ESMC\_Log} method that sets a flag to
//  prevent the program
//  from stopping reaching an error.                            
//EOP                                                                           
//--------------------------------------------------------------------------
{
  aLog->ESMC_LogSetNotHaltOnWarn();
}

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: C_ESMF_GetVerbose - gets verbose flag
//
// !INTERFACE:
void FTN(c_esmf_loggetverbose)(
// !RETURN VALUE:
//  none
// !ARGUMENTS:
    ESMC_Log* aLog, ESMC_Logical* verbose)
// !DESCRIPTION:
//  This routine calls a {\tt ESMC\_Log} method that gets the verbose flag
//
//EOP
//-----------------------------------------------------------------------------
{
  ESMC_Logical temp;
  temp=aLog->ESMC_LogGetVerbose();
  verbose =&(temp);
}

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: C_ESMF_LogGetFlush - gets flush flag
//
// !INTERFACE:
void FTN(c_esmf_loggetflush)(
//
// !RETURN VALUE:
//  none
// !ARGUMENTS:
    ESMC_Log* aLog, ESMC_Logical* flush)
// !DESCRIPTION:
// This routine calls a {\tt ESMC\_Log} method that gets the flush flag
//
//EOP
//--------------------------------------------------------------------------
{
  ESMC_Logical temp;
  temp=aLog->ESMC_LogGetFlush();
  flush=&(temp);

}

//----------------------------------------------------------------------
//BOP
// !IROUTINE: C_ESMF_LogGetHaltOnErr - gets HaltOnErr flag 
//
// !INTERFACE:
void FTN(c_esmf_loggethaltonerr)(
// !RETURN VALUE:
//  none
// !ARGUMENTS:
    ESMC_Log* aLog,ESMC_Logical* haltOnErr)
// !DESCRIPTION:
// This routine gets the HaltOnErr flag
//
//EOP
// ---------------------------------------------------------------------
{
  ESMC_Logical temp;
  temp=aLog->ESMC_LogGetHaltOnErr();
  haltOnErr=&(temp);
}

//----------------------------------------------------------------------
//BOP
// !IROUTINE: C_ESMF_LogGetHaltOnWarn - gets HaltOnErr flag
//
// !INTERFACE:
void FTN(c_esmf_loggethaltonwarn)(
// !RETURN VALUE:
//  none
// !ARGUMENTS:
    ESMC_Log* aLog,ESMC_Logical* haltOnWarn)
// !DESCRIPTION:
//  This routine calls a {\tt ESMC\_Log} method that gets the HaltOnWarn flag.
//
//EOP
//-----------------------------------------------------------------------------------
{
  ESMC_Logical temp;
  temp=aLog->ESMC_LogGetHaltOnWarn();
  haltOnWarn=&(temp);
}

}



