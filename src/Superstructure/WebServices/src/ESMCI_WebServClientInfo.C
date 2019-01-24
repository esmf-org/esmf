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
#define ESMC_FILENAME "ESMCI_WebServClientInfo.C"
//==============================================================================
//
// ESMC WebServClientInfo method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ ClientInfo methods declared
// in the companion file ESMCI_WebServClientInfo.h.  This code
// provides access methods for the client session information.
//
//-----------------------------------------------------------------------------
#include "ESMCI_WebServClientInfo.h"

#include "ESMCI_WebServNetEsmf.h"
#include <string>
#include <iostream>

using namespace std;

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
//
// constructor and destructor
//
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServClientInfo::ESMCI_WebServClientInfo()"
//BOPI
// !ROUTINE:  ESMCI_WebServClientInfo::ESMCI_WebServClientInfo()
//
// !INTERFACE:
ESMCI_WebServClientInfo::ESMCI_WebServClientInfo(
//
//
// !ARGUMENTS:
//
  int  clientId         // the client session id
  )
//
// !DESCRIPTION:
//    Setup the initial default values for the client session information.
//
//EOPI
//-----------------------------------------------------------------------------
{
        theClientId = clientId;
        theCurrentStatus = NET_ESMF_STAT_IDLE;
        theServerHost = "";
        theServerPort = 0;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServClientInfo::~ESMCI_WebServClientInfo()"
//BOPI
// !ROUTINE:  ESMCI_WebServClientInfo::~ESMCI_WebServClientInfo()
//
// !INTERFACE:
ESMCI_WebServClientInfo::~ESMCI_WebServClientInfo(
//
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Destroy any memory allocated for this class... essentially do nothing,
//    for now.
//
//EOPI
//-----------------------------------------------------------------------------
{
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServClientInfo::setUserName()"
//BOPI
// !ROUTINE:  ESMCI_WebServClientInfo::setUserName()
//
// !INTERFACE:
void  ESMCI_WebServClientInfo::setUserName(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  string  userName              // the login name of the client on the server
  )
//
// !DESCRIPTION:
//    Sets the client user name on the server.
//
//EOPI
//-----------------------------------------------------------------------------
{
        theUserName = userName;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServClientInfo::setPassword()"
//BOPI
// !ROUTINE:  ESMCI_WebServClientInfo::setPassword()
//
// !INTERFACE:
void  ESMCI_WebServClientInfo::setPassword(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  string  password              // the password of the client on the server
  )
//
// !DESCRIPTION:
//    Sets the client password on the server.
//
//EOPI
//-----------------------------------------------------------------------------
{
        thePassword = password;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServClientInfo::setStatus()"
//BOPI
// !ROUTINE:  ESMCI_WebServClientInfo::setStatus()
//
// !INTERFACE:
void  ESMCI_WebServClientInfo::setStatus(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  int  status           // the status value
  )
//
// !DESCRIPTION:
//    Sets the status for the client session.
//
//EOPI
//-----------------------------------------------------------------------------
{
        theCurrentStatus = status;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServClientInfo::setServerHost()"
//BOPI
// !ROUTINE:  ESMCI_WebServClientInfo::setServerHost()
//
// !INTERFACE:
void  ESMCI_WebServClientInfo::setServerHost(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  string  serverHost            // the name of the host machine to which the client
                                                                // is connected
  )
//
// !DESCRIPTION:
//    Sets the server host name for the client session.
//
//EOPI
//-----------------------------------------------------------------------------
{
        theServerHost = serverHost;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServClientInfo::setServerPort()"
//BOPI
// !ROUTINE:  ESMCI_WebServClientInfo::setServerPort()
//
// !INTERFACE:
void  ESMCI_WebServClientInfo::setServerPort(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  int  serverPort               // the port number to which the client session is
                                                        // connected
  )
//
// !DESCRIPTION:
//    Sets the server port number for the client session.
//
//EOPI
//-----------------------------------------------------------------------------
{
        theServerPort = serverPort;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServClientInfo::setJobId()"
//BOPI
// !ROUTINE:  ESMCI_WebServClientInfo::setJobId()
//
// !INTERFACE:
void  ESMCI_WebServClientInfo::setJobId(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  string  jobId                 // the unique id of the component server for this client
  )
//
// !DESCRIPTION:
//    Sets the component server id for the client session.
//
//EOPI
//-----------------------------------------------------------------------------
{
        theJobId = jobId;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServClientInfo::addOutputFile()"
//BOPI
// !ROUTINE:  ESMCI_WebServClientInfo::addOutputFile()
//
// !INTERFACE:
void  ESMCI_WebServClientInfo::addOutputFile(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  string  outputFile            // the name of the output file to add to the list
                                                                // of output files
  )
//
// !DESCRIPTION:
//    Adds the output filename to the list of output files for the client
//    session.
//
//EOPI
//-----------------------------------------------------------------------------
{
        theOutputFiles.push_back(outputFile);
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServClientInfo::print()"
//BOPI
// !ROUTINE:  ESMCI_WebServClientInfo::print()
//
// !INTERFACE:
void  ESMCI_WebServClientInfo::print(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Prints the data values for this class.  For debug purposes.
//
//EOPI
//-----------------------------------------------------------------------------
{
        cout << "***** BEGIN ESMCI_WebServClientInfo *****" << endl;
        cout << "Client ID       : " << theClientId << endl;
        cout << "User Name       : " << theUserName << endl;
        cout << "Password        : " << thePassword << endl;
        cout << "Status          : " << theCurrentStatus << endl;
        cout << "Server Host     : " << theServerHost << endl;
        cout << "Server Port     : " << theServerPort << endl;
        cout << "*****  END ESMCI_WebServClientInfo  *****" << endl;
}


} // end namespace
