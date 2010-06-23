// $Id: ESMC_LogErr.h,v 1.74 2010/06/23 22:07:32 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// This file is part of the pure C public ESMC API
//-----------------------------------------------------------------------------

//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_LOGERR_H
#define ESMC_LOGERR_H

//-----------------------------------------------------------------------------
//BOPI
// !CLASS: ESMC_LogErr - Public C interface to the ESMF LogErr class
//
// !DESCRIPTION:
//
// The code in this file defines the public C LogErr interface and declares 
// all class data and methods.  All methods are defined in the companion file 
// ESMC\_LogErr.C
//
//EOPI
//-----------------------------------------------------------------------------


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
const char *ESMC_LogGetErrMsg(int rc);
int ESMC_LogSetFilename(char filename[]);
void ESMC_TimeStamp(int *y,int* mn,int *d,int *h,int *m,int *s,int *ms);

#endif  //ESMC_LOGERR_H
