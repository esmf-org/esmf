// $Id: ESMC_LogErr.h,v 1.19 2004/03/19 07:16:52 cpboulder Exp $
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
// !CLASS: ESMC_Log - C++ interface to Log
//
// !DESCRIPTION:
//
// The code in this file defines the C++ Log members and declares all class
// data and methods.  All methods, except for the Set and Get methods, which
// are inlined, are defined in the companion file ESMC\_LogErr.C
//
// !USES:


#include <ESMC_Base.h>
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
// !PRIVATE MEMBER FUNCIONS:
    void ESMC_LogFormName();
    void ESMC_LogPrintHeader(int fortIO);
    void ESMC_LogPrint(int fortIO, int errCode, int line, char file[],
                       char dir[], char msg[]=NULL);
    void ESMC_LogGetErrMsg(int errCode, char msg[]) const;
    bool ESMC_LogNameValid(char name[], int FortIO);
// !PRIVATE TYPES:

    ESMC_Logical oneLogErrFile;
                                // If log data written to one log file,
                                // this is set to true.  Otherwise set to false.
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
		
    ESMC_Logical fileI0;        // If true, output written to files

    int stdOutUnitNumber;       // Unit number corresponding to standard 
                                // out
  public:
// !PUBLIC MEMBER FUNCTIONS:
// (see ESMC\_LogErr.C for a description of these methods)
    void ESMC_LogInfo(char* fmt,...);   
    void ESMC_LogInfoFortran(char fmt[],
    char charData[],char strData[][32],int intData[], double floatData[]);
    void ESMC_LogOpenFile(int numLogFile,char name[]);
    void ESMC_LogOpenFortFile(int numLogFile, char name[]);
    int ESMC_LogGetUnit();
    void ESMC_LogCloseFile();
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
    void ESMC_LogWrite(int logtype, char msg[]);
    void ESMC_LogWarnFortran(int errCode, int line, char file[],
         char dir[], char msg[]);
    void ESMC_LogErr_(int errCode, int line, char file[], char dir[]);
    void ESMC_LogErrMsg_(int errCode, int line, char file[],
                     char dir[], char msg[]);
    void ESMC_LogErrFortran(int errCode,int line,char file[],char dir[],char msg[]);
    void ESMC_LogExit();
    void ESMC_LogSet(char* option, ESMC_Logical value, ...);
    void ESMC_LogGet(char* option, ESMC_Logical & value, ...);
    FILE* ESMC_GetFileHandle();
    ESMC_Log();
};
//EOP

// private static data - address of fortran callback funcs

 extern "C" {
  void FTN(f_esmf_logopenfortran)(ESMC_Logical*, int *, char*, int );
  void FTN(f_esmf_logclosefortran)(int *);
  void FTN(f_esmf_logprintstring)(int *, char*, ESMC_Logical*, int );
  void FTN(f_esmf_logprintnewline)(int *, ESMC_Logical*);
}

#endif  //ESMC_LOG
