// $Id: ESMC_LogErr.h,v 1.4 2003/04/02 17:34:06 shep_smith Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC Log include file for C++

// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_LOG
#define ESMC_LOG

//-----------------------------------------------------------------------------
//BOP
//!CLASS: ESMC\_Log - C++ interface to Log
//
// !DESCRIPTION:
//
// The code in this file defines the C++ Log members and declares all class
// data and methods.  All methods, except for the SetVerbosity method, which is
// inlined are defined in the companion file ESMC\_LogErr.C
//
// !USES:
//#include "/home/sjs/ESMF/esmf/esmf/src/include/ESMC.h"
// commenting out the above and including the one below makes the code compile
#include "ftn.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include <mpi++.h>
#include <time.h>
#include <ctype.h>
#include "ESMF_LogConstants.inc"
#include "ESMF_ErrConstants.inc"
#include "ESMC_UtilityFunctions.h"


class ESMC_Log {
  private:
// !PRIVATE MEMBER FUNCIONS
    void ESMC_LogFormName();
    void ESMC_LogPrintHeader(int fortIO);
    void ESMC_LogPrint(int fortIO, int errCode, int line, char file[],
                       char dir[], char msg[]=NULL);
    void ESMC_LogGetErrMsg(int errCode, char msg[]) const;
    bool ESMC_LogNameValid(char name[]);
// !PRIVATE TYPES:

    int oneLogErrFile;      // if log data written to one log file,
                            // this is set to
                            // true.  Otherwise set to false.
			    // ESMC_OpenFile can override
			    // this value

    int standardOut;        //if log data written to standard out, this variable
                            // is set to true. Otherwise set to false.
			    // ESMC_OpenFile
			    // can over-ride this value.

    int fortIsOpen;         // used to to a file with Fortran I/O libraries 
    
    int unitNumber;         // fortran unit number for log/err file when
                            // ESMC\_LogWrite
                            // is used  Can be overwritten by
			    // ESMC_OpenFileFortran


    int numFilePtr;         // index into global array of File pointers
			    // for C++ I/O.


    int numFileFort;        // index into global array of unit numbers for 
                            // Fortran I/O

    int verbose;            // integer used to control which log
                            // messages written out.
                            // value can be over written by ESMC_Init

    int flush;              // if true, all output is flushed
			    // value can be overwritten by ESMC_INIT

    int haltOnWarn;          // Code will stop executing on
                             // encountering a warning
			    
    int haltOnErr;           // Code will stop executing on
                             // encountering an error


    char nameLogErrFile[32]; // name of logfile.
                             // Specified by user when LogInit called.  If
			     // multiple files are written out,
			     // PE rank is appended to name.
  public:
// !PUBLIC MEMBER FUNCTIONS (see ESMC\_LogErr.C for a description of these methods)
    void ESMC_LogInfo(char* fmt,...);   
    void ESMC_LogInfoFortran(char fmt[],
    char charData[],char strData[][32],int intData[], double floatData[]);
    void ESMC_LogOpenFile(int numLogFile,char name[]);
    void ESMC_LogOpenFileForWrite(int numLogFile, char name[]);
    void ESMC_LogInit(int verbosity=ESMF_LOG_TRUE, int flush=ESMF_LOG_FALSE,
	 int haltOnError=ESMF_LOG_TRUE, int haltOnWarning=ESMF_LOG_FALSE);
    int ESMC_LogWrite();
    void ESMC_LogCloseFile();
    void ESMC_LogCloseFileForWrite();
    void ESMC_LogFlush();
    void ESMC_LogNotFlush();
    void ESMC_LogVerbose();
    void ESMC_LogNotVerbose();
    void ESMC_LogHaltOnErr();
    void ESMC_LogNotHaltOnErr();
    void ESMC_LogHaltOnWarn();
    void ESMC_LogNotHaltOnWarn();
    void ESMC_LogWarnMsg_(int errCode, int line, char file[],
                     char dir[], char msg[]);
    void ESMC_LogWarn_(int errCode, int line, char file[],
                     char dir[]);
    void ESMC_LogWarnFortran(int errCode, int line, char file[],
         char dir[], char msg[]);
    void ESMC_LogErr_(int errCode, int line, char file[], char dir[]);
    void ESMC_LogErrMsg_(int errCode, int line, char file[],
                     char dir[], char msg[]);
    void ESMC_LogErrFortran(int errCode,int line,char file[],char dir[],char msg[]);
    void ESMC_LogExit();
    ESMC_Log();
};
 
//---------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogFlush() - set the flushSet variable.
// !INTERFACE:

inline void ESMC_Log::ESMC_LogFlush(

// !ARGUMENTS
//   none

   ) 

// !DESCRIPTION: 
// Causes output to be flushed.

{
      flush=ESMF_LOG_TRUE;
}

//EOP

//---------------------------------------------------------------------------
//BOP                
//                   
// !IROUTINE: ESMC_LogNotFlush() - output not flushed
// !INTERFACE:       
	     
inline void ESMC_Log::ESMC_LogNotFlush(
				  
// !ARGUMENTS        
//   none   

 )    
	     
// !DESCRIPTION:
// Causes output not to be flushed.
//EOP
{                    
   flush=ESMF_LOG_FALSE;
}               
									


//----------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogVerbose - output verbose 
//
// !INTERFACE:

inline void ESMC_Log::ESMC_LogVerbose(

// !ARGUMENTS
//   none
  )

// !DESCRIPTION:
// If theVerbosity is set to ESMF_LOG_TRUE, messages are printed out. 
// 
//EOP
{
     verbose=ESMF_LOG_TRUE;
}


//----------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogNotVerbose - output not verbose 
//
// !INTERFACE:

inline void ESMC_Log::ESMC_LogNotVerbose(

// !ARGUMENTS
//   none
  )

// !DESCRIPTION:
// If theVerbosity is set to ESMC_LOG_FALSE, no messages are printed out. 
// 
//EOP
{
     verbose=ESMF_LOG_FALSE;
}



//----------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogHaltOnErr - code will stop on encountering an error  
//
// !INTERFACE:

inline void ESMC_Log::ESMC_LogHaltOnErr(

// !ARGUMENTS
//   none
  )

// !DESCRIPTION:
// If haltOnErr is set to ESMC_LOG_TRUE, code will stop executing when
// encountering an error.
// 
//EOP
{
     haltOnErr=ESMF_LOG_TRUE;
}

//----------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogNotHaltOnErr - code will not stop on encountering
// an error  
//
// !INTERFACE:

inline void ESMC_Log::ESMC_LogNotHaltOnErr(

// !ARGUMENTS
//   none
  )

// !DESCRIPTION:
// If haltOnErr is set to ESMC_LOG_FALSE, code will not stop executing when
// encountering an error.
// 
//EOP
{
     haltOnErr=ESMF_LOG_FALSE;
}

//----------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogHaltOnWarn - code will stop on encountering
// a warning
//
// !INTERFACE:

inline void ESMC_Log::ESMC_LogHaltOnWarn(

// !ARGUMENTS
//   none
  )

// !DESCRIPTION:
// If haltOnWarn is set to ESMC_LOG_TRUE, code will stop executing when
// encountering an error.
// 
//EOP
{
     haltOnWarn=ESMF_LOG_TRUE;
}

//----------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogNotHaltOnWarn - code will not stop on encountering
// a warning
//
// !INTERFACE:

inline void ESMC_Log::ESMC_LogNotHaltOnWarn(

// !ARGUMENTS
//   none
  )

// !DESCRIPTION:
// If haltOnWarn is set to ESMC_LOG_FALSE, code will not stop executing when
// encountering an error.
// 
//EOP
{
     haltOnWarn=ESMF_LOG_FALSE;
}

#endif  //ESMC_LOG
