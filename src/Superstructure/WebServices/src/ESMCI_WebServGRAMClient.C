// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_WebServGRAMClient.C"
//==============================================================================
//
// ESMC WebServGRAMClient method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ GRAMClient methods declared
// in the companion file ESMCI_WebServGRAMClient.h.  This code
// provides access methods to the CAM output file.
//
// (KDS: This code is very specific to the CCSM/CAM prototype that only
//       had to fetch a few specific values.  I don't think this is a
//       class we're going to want to keep in the ESMF baseline.)
//
//-----------------------------------------------------------------------------

#include "ESMCI_WebServGRAMClient.h"

#include <stdlib.h>
#include <math.h>
#include <iostream>

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#define VERBOSITY             (1)       // 0: off, 10: max
//-----------------------------------------------------------------------------


namespace ESMCI
{

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServGRAMClient::ESMCI_WebServGRAMClient()"
//BOPI
// !ROUTINE:  ESMCI_WebServGRAMClient::ESMCI_WebServGRAMClient()
//
// !INTERFACE:
ESMCI_WebServGRAMClient::ESMCI_WebServGRAMClient(
//
//
// !ARGUMENTS:
//
  string  hostName,
  string  scriptDir,
  string  scriptName
  )
//
// !DESCRIPTION:
//    Cleans up memory allocated for CAM output file.
//
//EOPI
//-----------------------------------------------------------------------------
        : ESMCI_WebServCompSvrMgr(hostName, scriptDir, scriptName)
{
}

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServGRAMClient::~ESMCI_WebServGRAMClient()"
//BOPI
// !ROUTINE:  ESMCI_WebServGRAMClient::~ESMCI_WebServGRAMClient()
//
// !INTERFACE:
ESMCI_WebServGRAMClient::~ESMCI_WebServGRAMClient(
//
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Cleans up memory allocated for CAM output file.
//
//EOPI
//-----------------------------------------------------------------------------
{
        // need to free up memory here
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServGRAMClient::loginToServer()"
//BOPI
// !ROUTINE:  ESMCI_WebServGRAMClient::loginToServer()
//
// !INTERFACE:
int  ESMCI_WebServGRAMClient::loginToServer(
//
//
// !ARGUMENTS:
//
  string    userName,
  string    passwd
  )
//
// !DESCRIPTION:
//    Logs into the server.
//
//EOPI
//-----------------------------------------------------------------------------
{
        return 0; // do something
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServGRAMClient::submitJob()"
//BOPI
// !ROUTINE:  ESMCI_WebServGRAMClient::submitJob()
//
// !INTERFACE:
string  ESMCI_WebServGRAMClient::submitJob(
//
//
// !ARGUMENTS:
//
  int     portNum,
  string  registrarHost,
  int     clientId
  )
//
// !DESCRIPTION:
//    Submits the job specified by the argument to the job scheduler.
//
//EOPI
//-----------------------------------------------------------------------------
{
        return string(""); // do something
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServGRAMClient::cancelJob()"
//BOPI
// !ROUTINE:  ESMCI_WebServGRAMClient::cancelJob()
//
// !INTERFACE:
int  ESMCI_WebServGRAMClient::cancelJob(
//
//
// !ARGUMENTS:
//
  string  jobId
  )
//
// !DESCRIPTION:
//    Cancels the job specified by the job id.
//
//EOPI
//-----------------------------------------------------------------------------
{
        return 0; // do something
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServGRAMClient::jobStatus()"
//BOPI
// !ROUTINE:  ESMCI_WebServGRAMClient::jobStatus()
//
// !INTERFACE:
int  ESMCI_WebServGRAMClient::jobStatus(
//
//
// !ARGUMENTS:
//
  string  jobId
  )
//
// !DESCRIPTION:
//    Returns the status of the job specified by the job id.
//
//EOPI
//-----------------------------------------------------------------------------
{
        return 0; // do something
}


} // end namespace
