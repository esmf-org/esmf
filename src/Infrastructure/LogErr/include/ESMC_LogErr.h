// $Id: ESMC_LogErr.h,v 1.23 2004/04/21 19:30:46 cpboulder Exp $
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

class ESMC_Log {
  private:
// !PRIVATE MEMBER FUNCIONS:
   
// !PRIVATE TYPES:

    int numFilePtr;             // index into global array of File pointers
                                // for C++ I/O
    ESMC_Logical FileIsOpen;

    ESMC_Logical verbose;       // If set to ESMC_TF_TRUE, log messages written out.

    ESMC_Logical flush;         // If true, all output is flushed

    ESMC_Logical root_only;

    int halt;

    int filetype;

    int stream;

    int max_elements;

    int MaxTryOpen=100000;

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
    void ESMC_LogOpen(char filename[]);
    void ESMC_LogClose();
    int ESMC_LogWrite(int logtype, char msg[], char module[], char method[]);
    //void ESMC_LogInfo(char* fmt,...);   
    //char charData[],char strData[][32],int intData[], double floatData[]);
    //int ESMC_LogGetUnit()
    //void ESMC_LogSetFlush();
    //ESMC_Logical ESMC_LogGetFlush() const;
    //void ESMC_LogSetNotFlush();
    //void ESMC_LogSetVerbose();
    //ESMC_Logical ESMC_LogGetVerbose() const;
    //void ESMC_LogSetNotVerbose();
    //void ESMC_LogSetHaltOnErr();
    //ESMC_Logical ESMC_LogGetHaltOnErr() const;
    //void ESMC_LogSetNotHaltOnErr();
    //void ESMC_LogSetHaltOnWarn();
    //ESMC_Logical ESMC_LogGetHaltOnWarn() const;
    //void ESMC_LogSetNotHaltOnWarn();
    //void ESMC_LogWarnMsg_(int errCode, int line, char file[],char dir[], char msg[]);
    //void ESMC_LogWarn_(int errCode, int line, char file[],char dir[]);
    
    //void ESMC_LogWarnFortran(int errCode, int line, char file[],char dir[], char msg[]);
    //void ESMC_LogErr_(int errCode, int line, char file[], char dir[]);
    //void ESMC_LogErrMsg_(int errCode, int line, char file[],char dir[], char msg[]);
    //void ESMC_LogExit();
    //void ESMC_LogSet(char* option, ESMC_Logical value, ...);
    //void ESMC_LogGet(char* option, ESMC_Logical & value, ...);
    FILE *ESMC_LogFile;
    //ESMC_Log();
};
//EOP

#endif  //ESMC_LOG
