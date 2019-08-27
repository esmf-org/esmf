// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research,
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

#include "ESMF_LogMacros.inc"

#include "ESMC_LogErr.h"
#include "ESMC_Util.h"
#include "ESMCI_Util.h"

#include <cstdio>
#include <string>
#include <sstream>

// use this macro to test for NULL pointer in the interface layer
// -> here assume rcvar is not a pointer, and must be returned directly
#define ESMCI_NULL_CHECK_RC(ptr, rcvar) \
  if (ptr == NULL){ \
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL, \
    "Not a valid object pointer", ESMC_CONTEXT, &rcvar); \
    return rcvar;  \
  }

// use this macro to test for NULL pointer in the interface layer
// -> here assume rcvar is a pointer, and will be returned through argument list
#define ESMCI_NULL_CHECK_PRC(ptr, rcvar) \
  if (ptr == NULL){ \
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL, \
    "Not a valid object pointer", ESMC_CONTEXT, rcvar); \
    return;  \
  }

// use this macro to test for NULL pointer in the interface layer
// -> here assume rcvar is a pointer, and will be returned through argument list
#define ESMCI_NULL_CHECK_PRC_RETURN_NULL(ptr, rcvar) \
  if (ptr == NULL){ \
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL, \
    "Not a valid object pointer", ESMC_CONTEXT, rcvar); \
    return NULL;  \
  }

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

    bool trace;                 // If true, some methods issue trace messages upon entry

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
    void AllocError(
        int LINE, const std::string &FILE, const std::string &method,
        int *rcToReturn);
    void DeallocError(
        int LINE, const std::string &FILE, const std::string &method,
        int *rcToReturn);
    void Close();
    bool FoundError(int rcToCheck,
        int LINE, const char FILE[], const char method[],
        int *rcToReturn);
    bool FoundError(int rcToCheck,
        int LINE, const std::string &FILE, const std::string &method,
        int *rcToReturn);
    void MsgAllocError(const std::string& msg,
        int LINE, const std::string &FILE, const std::string &method,
        int *rcToReturn);
    void MsgDeallocError(const std::string& msg,
        int LINE, const std::string &FILE, const std::string &method,
        int *rcToReturn);
    bool MsgFoundError(int rcToCheck, const char msg[],
        int LINE, const char FILE[], const char method[],
        int *rcToReturn);
    bool MsgFoundError(int rcToCheck, const std::string &msg,
        int LINE, const std::string &FILE, const std::string &method,
        int *rcToReturn);
    bool MsgFoundError(int rcToCheck, const std::stringstream& msg,
        int LINE, const std::string &FILE, const std::string &method,
        int *rcToReturn) {
      return MsgFoundError(rcToCheck, msg.str(), LINE, FILE, method, rcToReturn);
    }
    void Open(const std::string &filename);
    int Set(int flush);
    int SetTrace(bool traceflag);
    int Write(const std::string& msg, int msgtype);
    int Write(const std::stringstream& msg, int msgtype) {
      return Write(msg.str(), msgtype);
    }
    int Write(const std::string& msg, int msgtype,
        int LINE, const std::string &FILE, const std::string &method);
    int Write(const std::stringstream& msg, int msgtype,
        int LINE, const std::string &FILE, const std::string &method) {
      return Write(msg.str(), msgtype, LINE, FILE, method);
    }

// !PUBLIC Variables:
    std::FILE *ESMC_LogFile;
    std::string nameLogErrFile;
    int *pet_num;
    ESMC_LogKind_Flag logtype;
    int *errorMask;
    int errorMaskCount;


  private:
// !PRIVATE MEMBER FUNCIONS:

};

} //namespace ESMCI

// the default global log object
extern ESMCI::LogErr ESMC_LogDefault;
extern "C" {
 void FTN_X(f_esmf_logset)(ESMC_Logical *flush, int *rc);
 void FTN_X(f_esmf_logwrite0)(const char *msg, int *msgtype, int *rc, ESMCI_FortranStrLenArg mlen);
 void FTN_X(f_esmf_logwrite1)(const char *msg, int *msgtype,
                            int *line, const char *file, const char *method, int *rc,
                            ESMCI_FortranStrLenArg mlen, ESMCI_FortranStrLenArg flen,
                            ESMCI_FortranStrLenArg mdlen);
}

//EOP


#endif  //ESMCI_LOGERR_H
