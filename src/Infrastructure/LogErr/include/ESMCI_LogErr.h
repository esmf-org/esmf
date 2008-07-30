// $Id: ESMCI_LogErr.h,v 1.3 2008/07/30 22:17:22 rosalind Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC Log include file for C++

// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMCI_LOGERR_H
#define ESMCI_LOGERR_H

//-----------------------------------------------------------------------------
//BOP
// !CLASS: ESMCI_Log - C++ interface to Log
//
// !DESCRIPTION:
//
// The code in this file defines the C++ Log members and declares all class
// data and methods.  All methods, except for the Set and Get methods, which
// are inlined, are defined in the companion file ESMCI\_LogErr.C
//
// !USES:


#include "ESMC_LogErr.h"

namespace ESMCI{

class LogErr {
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
    
    bool AllocError(int *rcToReturn);
    bool AllocError(int LINE,char FILE[],char method[],int *rcToReturn);
    void Close();
    bool FoundError(int rcToCheck,int *rcToReturn);
    bool FoundError(int rcToCheck,int LINE,char FILE[],char method[],
         int *rcToReturn);
    bool MsgAllocError(char msg[],int *rcToReturn);
    bool MsgAllocError(char msg[],int LINE,char FILE[],char method[],
    int *rcToReturn);
        bool MsgFoundError(int rcToCheck,char msg[],int *rcToReturn);
    bool MsgFoundError(int rcToCheck,char msg[],int LINE,char FILE[],
         char method[],int *rcToReturn);
    void Open(char filename[]);
    bool WriteLog(char msg[],int msgtype);
    bool WriteLog(char msg[],int msgtype,int LINE,char FILE[],
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

} //namespace ESMCI

// the default global log object
extern ESMCI::LogErr ESMC_LogDefault;
extern "C" {
 void FTN(f_esmf_logwrite0)(char *msg, int *msgtype, int *rc, ESMCI_FortranStrLenArg mlen);
 void FTN(f_esmf_logwrite1)(char *msg, int *msgtype,
                            int *line, char *file, char *method, int *rc,
                            ESMCI_FortranStrLenArg mlen, ESMCI_FortranStrLenArg flen,
			    ESMCI_FortranStrLenArg mdlen);
}

//EOP


#endif  //ESMCI_LOGERR_H
