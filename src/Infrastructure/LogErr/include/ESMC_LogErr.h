// $Id: ESMC_LogErr.h,v 1.70.2.1 2010/02/05 19:58:55 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
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
// !CLASS: ESMC_LogErr - Public C interface to the ESMF LogErr class
//
// !DESCRIPTION:
//
// The code in this file defines the public C LogErr interface and declares 
// all class data and methods.  All methods are defined in the companion file 
// ESMC\_LogErr.C
//
// !USES:


#include "ESMF_LogConstants.inc"
#include "ESMF_ErrReturnCodes.inc"

#ifdef __cplusplus
extern "C"{
#endif

// Class declaration type
typedef struct{
void *ptr;
}ESMC_LogErr;

// Class API
int ESMC_LogWrite(const char msg[], int msgtype);

#ifdef __cplusplus
} // extern "C"
#endif

enum ESMC_MsgType{ESMC_LOG_INFO=1,ESMC_LOG_WARN=2,ESMC_LOG_ERROR=3};
enum ESMC_LogType{ESMC_LOG_SINGLE=1,ESMC_LOG_MULTI=2,ESMC_LOG_NONE=3};
int ESMC_LogFinalize();
char *ESMC_LogGetErrMsg(int rc);
int ESMC_LogSetFilename(char filename[]);
void ESMC_TimeStamp(int *y,int* mn,int *d,int *h,int *m,int *s,int *ms);

//EOP


#endif  //ESMC_LOGERR_H
