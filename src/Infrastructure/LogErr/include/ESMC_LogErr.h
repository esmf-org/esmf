// $Id: ESMC_LogErr.h,v 1.47 2004/05/18 22:36:53 cpboulder Exp $
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

#ifndef ESMC_LOGERR_H
#define ESMC_LOGERR_H

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
#include "ESMF_ErrReturnCodes.inc"

enum ESMC_LogFileType{ESMC_LOG_INFO=1,ESMC_LOG_WARN=2,ESMC_LOG_ERROR=3};                   
int ESMC_LogSetFilename(char filename[]);
int ESMC_LogFinalize();
void ESMC_TimeStamp(int *y,int* mn,int *d,int *h,int *m,int *s,int *ms);
char *ESMC_LogGetErrMsg(int rc);

class ESMC_Log {
private:
// !PRIVATE TYPES:

    int numFilePtr;             // index into global array of File pointers
                                // for C++ I/O
    ESMC_Logical FileIsOpen;

    ESMC_Logical verbose;       // If set to ESMC_TF_TRUE, log messages written out.

    ESMC_Logical flush;         // If true, all output is flushed

    ESMC_Logical rootOnly;

    int halt;

    int filetype;

    int stream;

    int max_elements;
		
    ESMC_Logical fileI0;        // If true, output written to files

    int stdOutUnitNumber;       // Unit number corresponding to standard 
                                // out
  public:
// !PUBLIC MEMBER FUNCTIONS:
// (see ESMC\_LogErr.C for a description of these methods)
    void ESMC_LogOpen(char filename[]);
    void ESMC_LogClose();
    bool ESMC_LogWrite(char msg[],int logtype);
    bool ESMC_LogWrite(char msg[],int logtype,int LINE,char FILE[],char method[]);    
    bool ESMC_LogFoundError(int rcToCheck,int *rcToReturn);
    bool ESMC_LogFoundError(int rcToCheck,int LINE,char FILE[],char method[],int *rcToReturn);
    bool ESMC_LogMsgFoundError(int rcToCheck,char msg[],int *rcToReturn);
    bool ESMC_LogMsgFoundError(int rcToCheck,char msg[],int LINE,char FILE[],char method[],int *rcToReturn);
    bool ESMC_LogAllocError(int *rcToReturn);
    bool ESMC_LogAllocError(int LINE,char FILE[],char method[],int *rcToReturn);
    bool ESMC_LogMsgAllocError(char msg[],int *rcToReturn);
    bool ESMC_LogMsgAllocError(char msg[],int LINE,char FILE[],char method[],int *rcToReturn);
    char nameLogErrFile[32];
    FILE *ESMC_LogFile;
    //ESMC_Log();

  private:
// !PRIVATE MEMBER FUNCIONS:
    
};

// the default global log object
extern ESMC_Log ESMC_LogDefault;

//EOP

#endif  //ESMC_LOGERR_H
