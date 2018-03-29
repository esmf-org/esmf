// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_WebServForkClient.C"
//==============================================================================
//
// ESMC WebServForkClient method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ ForkClient methods declared
// in the companion file ESMCI_WebServForkClient.h.  This code
// provides access methods to the CAM output file.
//
// (KDS: This code is very specific to the CCSM/CAM prototype that only
//       had to fetch a few specific values.  I don't think this is a
//       class we're going to want to keep in the ESMF baseline.)
//
//-----------------------------------------------------------------------------

#include "ESMCI_WebServForkClient.h"

#include <stdlib.h>
#include <math.h>
#include <signal.h>
#include <stdio.h>
#include <iostream>
#include <string>

#if !defined (ESMF_OS_MinGW)
#include <unistd.h>
#else
#include <Winsock.h>
#endif

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
#define ESMC_METHOD "ESMCI_WebServForkClient::ESMCI_WebServForkClient()"
//BOPI
// !ROUTINE:  ESMCI_WebServForkClient::ESMCI_WebServForkClient()
//
// !INTERFACE:
ESMCI_WebServForkClient::ESMCI_WebServForkClient(
//
//
// !ARGUMENTS:
//
  string                hostName,
  string                scriptDir,
  string                scriptName
  )
//
// !DESCRIPTION:
//    Instantiates a Component Service Manager that uses the system fork
//    method for starting a component service.
//
//EOPI
//-----------------------------------------------------------------------------
        : ESMCI_WebServCompSvrMgr(hostName, scriptDir, scriptName)
{
        char    host[512] = {""};
        gethostname(host, sizeof(host) - 1);
        theHostName = host;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServForkClient::~ESMCI_WebServForkClient()"
//BOPI
// !ROUTINE:  ESMCI_WebServForkClient::~ESMCI_WebServForkClient()
//
// !INTERFACE:
ESMCI_WebServForkClient::~ESMCI_WebServForkClient(
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
#define ESMC_METHOD "ESMCI_WebServForkClient::submitJob()"
//BOPI
// !ROUTINE:  ESMCI_WebServForkClient::submitJob()
//
// !INTERFACE:
string  ESMCI_WebServForkClient::submitJob(
//
//
// !ARGUMENTS:
//
  int     clientId,
  string  registrarHost,
  int     portNum
  )
//
// !DESCRIPTION:
//    Submits the job specified by the argument to the job scheduler.
//
//EOPI
//-----------------------------------------------------------------------------
{
#if !defined (ESMF_OS_MinGW)
printf("ESMCI_WebServForkClient::submitJob()\n");
printf("Client ID: %d\n", clientId);
printf("RegistrarHost: %s\n", registrarHost.c_str());
printf("Port Num: %d\n", portNum);
        int     pid = fork();

printf("PID: %d\n", pid);
        if (pid < 0) // error occurred
        {
                fprintf(stderr, "Fork failed!\n");
                return "";
        }
        else if (pid == 0) // child process
        {
                char            portNumStr[64];
                char            clientIdStr[64];
                string  scriptPath = theScriptDir + "/" + theScriptName;

                sprintf(portNumStr, "%d", portNum);
                sprintf(clientIdStr, "%d", clientId);

printf("Script Path: %s\n", scriptPath.c_str());
printf("Script Name: %s\n", theScriptName.c_str());
printf("Port Num: %s\n", portNumStr);
printf("Client ID: %s\n", clientIdStr);

                execlp(scriptPath.c_str(),
             theScriptName.c_str(),
             portNumStr,
             clientIdStr,
             registrarHost.c_str(),
             NULL);
        }

        //***
        // Build the job string
        //***
        char            pidStr[64];
        sprintf(pidStr, "%d", pid);
        string  jobId = theHostName + "_" + pidStr;

        return jobId;
#else
// TODO: Use the Windows CreateProcess system call.
        return "";
#endif
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServForkClient::cancelJob()"
//BOPI
// !ROUTINE:  ESMCI_WebServForkClient::cancelJob()
//
// !INTERFACE:
int  ESMCI_WebServForkClient::cancelJob(
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
#if !defined (ESMF_OS_MinGW)
        int     pid = extractPid(jobId);
        int     rc = 0;

        if (pid >= 0)
        {
                rc = kill(pid, SIGKILL);
        }

        return rc;
#else
// TODO: Use the Windows TerminateProcess system call.
        return -1;
#endif
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServForkClient::jobStatus()"
//BOPI
// !ROUTINE:  ESMCI_WebServForkClient::jobStatus()
//
// !INTERFACE:
int  ESMCI_WebServForkClient::jobStatus(
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


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServForkClient::extractPid()"
//BOPI
// !ROUTINE:  ESMCI_WebServForkClient::extractPid()
//
// !INTERFACE:
int  ESMCI_WebServForkClient::extractPid(
//
//
// !ARGUMENTS:
//
  string  jobId
  )
//
// !DESCRIPTION:
//    Returns the process id for the specified job id.
//
//EOPI
//-----------------------------------------------------------------------------
{
        int     pid = -1;

        //***
        // Basically, the jobid is made up of the hostname plus underscore ('_')
        // plus the process id.  So, search for the last underscore, and everything
        // after that should be the process id.
        //***
        size_t  lastUnderscore = jobId.find_last_of('_');
        if (lastUnderscore != string::npos)
        {
                string  pidStr = jobId.substr(lastUnderscore + 1);

                pid = atoi(pidStr.c_str());
        }

        return pid;
}



} // end namespace
