// $Id: ESMC_LogErr.h,v 1.14 2003/08/14 14:31:13 theurich Exp $
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
// data and methods.  All methods, except for the Set and Get methods, which
// are inlined, are defined in the companion file ESMC\_LogErr.C
//
// !USES:


#include <ESMC_Base.h>
//#include <ESMC_LogErr.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
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
    bool ESMC_LogNameValid(char name[], int FortIO);
// !PRIVATE TYPES:

    ESMC_Logical oneLogErrFile; // if log data written to one log file,
                                // this is set to
                                // true.  Otherwise set to false.
			        // ESMC_OpenFile can override
			        // this value

    ESMC_Logical  standardOut;  // if log data written to standard out,
                                // this variable
                                // is set to true. Otherwise set to false.
			        // ESMC_OpenFile
			        // can over-ride this value.

    ESMC_Logical fortIsOpen;    // used to to a file with Fortran
                                // I/O libraries 
    
    int unitNumber;             // fortran unit number for log/err file when
                                // ESMC\_LogGetUnit
                                // is used  Can be overwritten by
			        // ESMC_OpenFileFortran


    int numFilePtr;             // index into global array of File pointers
			        // for C++ I/O.


    int numFileFort;            // index into global array of unit numbers for 
                                // Fortran I/O

    ESMC_Logical verbose;       // If set to ESMC_TF_TRUE, log
                                // messages written out.

    ESMC_Logical flush;         // If true, all output is flushed

    ESMC_Logical haltOnWarn;    // Code will stop executing on
                                // encountering a warning
			    
    ESMC_Logical haltOnErr;     // Code will stop executing on
                                // encountering an error


    char nameLogErrFile[32];    // name of logfile.
			        // If multiple files are written out,
			        // PE rank is automatically
				// appended to name.
  public:
// !PUBLIC MEMBER FUNCTIONS (see ESMC\_LogErr.C for a description of these methods)
    void ESMC_LogInfo(char* fmt,...);   
    void ESMC_LogInfoFortran(char fmt[],
    char charData[],char strData[][32],int intData[], double floatData[]);
    void ESMC_LogOpenCFile(int numLogFile,char name[]);
    void ESMC_LogOpenFortFile(int numLogFile, char name[]);
    int ESMC_LogGetUnit();
    void ESMC_LogCloseCFile();
    void ESMC_LogCloseFortFile();
    void ESMC_LogSetFlush();
    ESMC_Logical ESMC_LogGetFlush() const;
    void ESMC_LogSetNotFlush();
    void ESMC_LogSetVerbose();
    ESMC_Logical ESMC_LogGetVerbose() const;
    void ESMC_LogSetNotVerbose();
    void ESMC_LogSetHaltOnErr();
    ESMC_Logical ESMC_LogGetHaltOnErr() const;
    void ESMC_LogSetNotHaltOnErr();
    void ESMC_LogSetHaltOnWarn();
    ESMC_Logical ESMC_LogGetHaltOnWarn() const;
    void ESMC_LogSetNotHaltOnWarn();
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

// private static data - address of fortran callback funcs

extern "C" {
 void FTN(f_esmf_logopenfortran)(ESMC_Logical*, int *, char*, int );
 void FTN(f_esmf_logclosefortran)(int *);
 void FTN(f_esmf_logprintstring)(int *, char*, ESMC_Logical*, int );
 void FTN(f_esmf_logprintnewline)(int *, ESMC_Logical*);
}


//---------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogSetFlush() - set the flushSet variable.
// !INTERFACE:

inline void ESMC_Log::ESMC_LogSetFlush(

// !ARGUMENTS
//   none

   ) 

// !DESCRIPTION: 
// Causes output to be flushed.
// 
//EOP
{
      flush=ESMF_TF_TRUE;
}


//---------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogGetFlush() - returns the flush variable 
// !INTERFACE:

inline ESMC_Logical ESMC_Log::ESMC_LogGetFlush (

// !ARGUMENTS
//   none

   ) const 

// !DESCRIPTION: 
// Returns the flush variable 
// 
//EOP
{
      return flush;
}




//---------------------------------------------------------------------------
//BOP                
//                   
// !IROUTINE: ESMC_LogSetNotFlush() - output not flushed
// !INTERFACE:       
	     
inline void ESMC_Log::ESMC_LogSetNotFlush(
				  
// !ARGUMENTS        
//   none   

 )    
	     
// !DESCRIPTION:
// Causes output not to be flushed.
//EOP
{                    
   flush=ESMF_TF_FALSE;
}               
									


//----------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogSetVerbose - make output verbose 
//
// !INTERFACE:

inline void ESMC_Log::ESMC_LogSetVerbose(

// !ARGUMENTS
//   none
  )

// !DESCRIPTION:
// If theVerbosity is set to ESMF\_TF\_TRUE, messages are printed out. 
// 
//EOP
{
     verbose=ESMF_TF_TRUE;
}

//----------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogGetVerbose - return verbose 
//
// !INTERFACE:

inline ESMC_Logical ESMC_Log::ESMC_LogGetVerbose(

// !ARGUMENTS
//   none
  ) const

// !DESCRIPTION:
// Returns  verbose value 
// 
//EOP
{
     return verbose;
}


//----------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogSetNotVerbose - output not verbose 
//
// !INTERFACE:

inline void ESMC_Log::ESMC_LogSetNotVerbose(

// !ARGUMENTS
//   none
  )

// !DESCRIPTION:
// If theVerbosity is set to ESMC\_TF\_FALSE, no messages are printed out. 
// 
//EOP
{
     verbose=ESMF_TF_FALSE;
}



//----------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogSetHaltOnErr - code will stop on encountering an error  
//
// !INTERFACE:

inline void ESMC_Log::ESMC_LogSetHaltOnErr(

// !ARGUMENTS
//   none
  )

// !DESCRIPTION:
// If haltOnErr is set to ESMC\_TF\_TRUE, code will stop executing when
// encountering an error.
// 
//EOP
{
     haltOnErr=ESMF_TF_TRUE;
}

//----------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogGetHaltOnErr - returns haltOnErr
//
// !INTERFACE:

inline ESMC_Logical ESMC_Log::ESMC_LogGetHaltOnErr(

// !ARGUMENTS
//   none
  ) const

// !DESCRIPTION:
// Returns haltOnErr
// 
//EOP
{
     return haltOnErr;
}

//----------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogSetNotHaltOnErr - code will not stop on encountering
// an error  
//
// !INTERFACE:

inline void ESMC_Log::ESMC_LogSetNotHaltOnErr(

// !ARGUMENTS
//   none
  )

// !DESCRIPTION:
// If haltOnErr is set to ESMC\_TF\_FALSE, code will not stop executing when
// encountering an error.
// 
//EOP
{
     haltOnErr=ESMF_TF_FALSE;
}

//----------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogSetHaltOnWarn - code will stop on encountering
// a warning
//
// !INTERFACE:

inline void ESMC_Log::ESMC_LogSetHaltOnWarn(

// !ARGUMENTS
//   none
  )

// !DESCRIPTION:
// If haltOnWarn is set to ESMC\_TF\_TRUE, code will stop executing when
// encountering an error.
// 
//EOP
{
     haltOnWarn=ESMF_TF_TRUE;
}

//----------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogGetHaltOnWarn - returns haltOnwarn value
// a warning
//
// !INTERFACE:

inline ESMC_Logical ESMC_Log::ESMC_LogGetHaltOnWarn(

// !ARGUMENTS
//   none
  ) const

// !DESCRIPTION:
// Returns haltOnWarn
// 
//EOP
{
     return haltOnWarn;
}

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogSetNotHaltOnWarn - code will not stop on encountering
// a warning
//
// !INTERFACE:

inline void ESMC_Log::ESMC_LogSetNotHaltOnWarn(

// !ARGUMENTS
//   none
  )

// !DESCRIPTION:
// If haltOnWarn is set to ESMC\_Tf\_FALSE, code will not stop executing when
// encountering an error.
// 
//EOP
{
     haltOnWarn=ESMF_TF_FALSE;
}


#endif  //ESMC_LOG
