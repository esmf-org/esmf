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
#define ESMC_FILENAME "ESMCI_WebServCompSvrInfo.C"
//==============================================================================
//
// ESMC WebServCompSvrInfo method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ CompSvrInfo methods declared
// in the companion file ESMCI_WebServCompSvrInfo.h.  This code
// provides access methods for the client session information.
//
//-----------------------------------------------------------------------------

#include "ESMCI_WebServCompSvrInfo.h"
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
#define ESMC_METHOD "ESMCI_WebServCompSvrInfo::ESMCI_WebServCompSvrInfo()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrInfo::ESMCI_WebServCompSvrInfo()
//
// !INTERFACE:
ESMCI_WebServCompSvrInfo::ESMCI_WebServCompSvrInfo(
//
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Setup the initial default values for the client session information.
//
//EOPI
//-----------------------------------------------------------------------------
{
        theClientId = 0;
        theJobId = "";
        theHostName = "";
        thePortNum = 0;
        theName = "";
        theDesc = "";
        thePhysHostName = "";
        theCurrentStatus = NET_ESMF_STAT_IDLE;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServCompSvrInfo::~ESMCI_WebServCompSvrInfo()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrInfo::~ESMCI_WebServCompSvrInfo()
//
// !INTERFACE:
ESMCI_WebServCompSvrInfo::~ESMCI_WebServCompSvrInfo(
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
#define ESMC_METHOD "ESMCI_WebServCompSvrInfo::setClientId()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrInfo::setClientId()
//
// !INTERFACE:
void  ESMCI_WebServCompSvrInfo::setClientId(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  int  clientId                 // the identifier of the client who started the server
  )
//
// !DESCRIPTION:
//    Sets the server port number for the client session.
//
//EOPI
//-----------------------------------------------------------------------------
{
        theClientId = clientId;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServCompSvrInfo::setJobId()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrInfo::setJobId()
//
// !INTERFACE:
void  ESMCI_WebServCompSvrInfo::setJobId(
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
#define ESMC_METHOD "ESMCI_WebServCompSvrInfo::setHostName()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrInfo::setHostName()
//
// !INTERFACE:
void  ESMCI_WebServCompSvrInfo::setHostName(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  string  hostName              // the name of the host machine on which the server
                                                                // was initiated (may not be running on this machine
                                                                // if this is a cluster)
  )
//
// !DESCRIPTION:
//    Sets the server host name for the client session.
//
//EOPI
//-----------------------------------------------------------------------------
{
        theHostName = hostName;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServCompSvrInfo::setPortNum()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrInfo::setPortNum()
//
// !INTERFACE:
void  ESMCI_WebServCompSvrInfo::setPortNum(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  int  portNum          // the port number to which the client session is connected
  )
//
// !DESCRIPTION:
//    Sets the server port number for the client session.
//
//EOPI
//-----------------------------------------------------------------------------
{
        thePortNum = portNum;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServCompSvrInfo::setName()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrInfo::setName()
//
// !INTERFACE:
void  ESMCI_WebServCompSvrInfo::setName(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  string  name          // the name of the component
  )
//
// !DESCRIPTION:
//    Sets the server host name for the client session.
//
//EOPI
//-----------------------------------------------------------------------------
{
        theName = name;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServCompSvrInfo::setDesc()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrInfo::setDesc()
//
// !INTERFACE:
void  ESMCI_WebServCompSvrInfo::setDesc(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  string  desc          // the description of the component
  )
//
// !DESCRIPTION:
//    Sets the server host name for the client session.
//
//EOPI
//-----------------------------------------------------------------------------
{
        theDesc = desc;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServCompSvrInfo::setPhysHostName()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrInfo::setPhysHostName()
//
// !INTERFACE:
void  ESMCI_WebServCompSvrInfo::setPhysHostName(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  string  physHostName  // the name of the host machine on which the server
                                                                // is listening (may be different from host name if
                                                                // this is a cluster)
  )
//
// !DESCRIPTION:
//    Sets the server host name for the client session.
//
//EOPI
//-----------------------------------------------------------------------------
{
        thePhysHostName = physHostName;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServCompSvrInfo::setStatus()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrInfo::setStatus()
//
// !INTERFACE:
void  ESMCI_WebServCompSvrInfo::setStatus(
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
#define ESMC_METHOD "ESMCI_WebServCompSvrInfo::print()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrInfo::print()
//
// !INTERFACE:
void  ESMCI_WebServCompSvrInfo::print(
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
        cout << "***** BEGIN ESMCI_WebServCompSvrInfo *****" << endl;
        cout << "Client ID      : " << theClientId << endl;
        cout << "Job ID         : " << theJobId << endl;
        cout << "Host Name      : " << theHostName << endl;
        cout << "Port Num       : " << thePortNum << endl;
        cout << "Name           : " << theName << endl;
        cout << "Desc           : " << theDesc << endl;
        cout << "Phys Host Name : " << thePhysHostName << endl;
        cout << "Status         : " << theCurrentStatus << endl;
        cout << "*****  END ESMCI_WebServCompSvrInfo  *****" << endl;
}


} // end namespace
