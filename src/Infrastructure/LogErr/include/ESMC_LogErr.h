// $Id: ESMC_LogErr.h,v 1.61.2.3 2009/01/21 21:25:22 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

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

enum ESMC_MsgType{ESMC_LOG_INFO=1,ESMC_LOG_WARN=2,ESMC_LOG_ERROR=3};
enum ESMC_LogType{ESMC_LOG_SINGLE=1,ESMC_LOG_MULTI=2,ESMC_LOG_NONE=3};
int ESMC_LogFinalize();
char *ESMC_LogGetErrMsg(int rc);
int ESMC_LogSetFilename(char filename[]);
void ESMC_TimeStamp(int *y,int* mn,int *d,int *h,int *m,int *s,int *ms);

class ESMC_Log {
private:
// !PRIVATE TYPES:

    int numFilePtr;             // index into global array of File pointers
                                // for C++ I/O
    ESMC_Logical FileIsOpen;

    ESMC_Logical verbose;       // If set to ESMC_TF_TRUE, log messages written 
                                // out.

    ESMC_Logical flush;         // If true, all output is flushed

    ESMC_Logical rootOnly;

    int halt;

    int filetype;

    int stream;

    int max_elements;
    
    int *pet_number;
		
    ESMC_Logical fileI0;        // If true, output written to files

    int stdOutUnitNumber;       // Unit number corresponding to standard 
                                // out
    
  public:
// !PUBLIC MEMBER FUNCTIONS:
// (see ESMC\_LogErr.C for a description of these methods)
    
    bool ESMC_LogAllocError(int *rcToReturn);
    bool ESMC_LogAllocError(int LINE,char FILE[],char method[],int *rcToReturn);
    void ESMC_LogClose();
    bool ESMC_LogFoundError(int rcToCheck,int *rcToReturn);
    bool ESMC_LogFoundError(int rcToCheck,int LINE,char FILE[],char method[],
         int *rcToReturn);
    bool ESMC_LogMsgAllocError(char msg[],int *rcToReturn);
    bool ESMC_LogMsgAllocError(char msg[],int LINE,char FILE[],char method[],
    int *rcToReturn);
        bool ESMC_LogMsgFoundError(int rcToCheck,char msg[],int *rcToReturn);
    bool ESMC_LogMsgFoundError(int rcToCheck,char msg[],int LINE,char FILE[],
         char method[],int *rcToReturn);
    void ESMC_LogOpen(char filename[]);
    bool ESMC_LogWrite(char msg[],int msgtype);
    bool ESMC_LogWrite(char msg[],int msgtype,int LINE,char FILE[],
         char method[]);       
// !PUBLIC Variables:          
    FILE *ESMC_LogFile;
    char nameLogErrFile[32];
    int *pet_num;
    ESMC_LogType logtype;
    int *errorMask;
    int errorMaskCount;


  private:
// !PRIVATE MEMBER FUNCIONS:
    
};

// the default global log object
extern ESMC_Log ESMC_LogDefault;
extern "C" {
 void FTN(f_esmf_logwrite0)(char *msg, int *msgtype, int *rc, ESMCI_FortranStrLenArg mlen);
 void FTN(f_esmf_logwrite1)(char *msg, int *msgtype,
                            int *line, char *file, char *method, int *rc,
                            ESMCI_FortranStrLenArg mlen, ESMCI_FortranStrLenArg flen,
			    ESMCI_FortranStrLenArg mdlen);
}

//EOP

#endif  //ESMC_LOGERR_H
