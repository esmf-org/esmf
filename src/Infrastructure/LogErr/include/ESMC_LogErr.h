// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2020, University Corporation for Atmospheric Research, 
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
//
// ESMC_LogErr - Public C interface to the ESMF LogErr class
//
// The code in this file defines the public C LogErr interface and declares 
// all class data and methods.  All methods are defined in the companion file 
// ESMC\_LogErr.C
//
//
//-----------------------------------------------------------------------------


#include "ESMC_ReturnCodes.h"

#ifdef __cplusplus
extern "C"{
#endif


// Class declaration type
typedef struct{
void *ptr;
}ESMC_LogErr;

// Class API

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_LogWrite - Write an entry into the Log file
//
// !INTERFACE:
int ESMC_LogWrite(
  const char msg[], // in
  int msgtype       // in
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Write an entry into the Log file.
//
//  The arguments are:
//  \begin{description}
//  \item[msg]
//    The message to be written.
//  \item[msgtype]
//    The message type.  This flag is documented in section \ref{const:clogmsgflag}
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_LogSet - Set Log properties
//
// !INTERFACE:
int ESMC_LogSet(
  int flush       // in
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Set Log properties.
//
//  The arguments are:
//  \begin{description}
//  \item[flush]
//    If set to ESMF\_TRUE, flush log messages immediately, rather than buffering 
//    them. Default is to flush after 10 messages.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

#ifdef __cplusplus
} // extern "C"
#endif

int ESMC_LogFinalize();
const char *ESMC_LogGetErrMsg(
  int rc
);
int ESMC_LogSetFilename(
  const char filename[]
);
void ESMC_TimeStamp(
  int *y,
  int* mn,
  int *d,
  int *h,
  int *m,
  int *s,
  int *ms
);

#endif  //ESMC_LOGERR_H
