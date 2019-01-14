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
//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times


#ifndef ESMCI_WebServClientInfo_H
#define ESMCI_WebServClientInfo_H

#include <vector>
#include <string>

using namespace std;

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::ESMCI_WebServClientInfo
//
// !DESCRIPTION:
//
// The code in this file defines the C++ ClientInfo members and method
// signatures (prototypes).  The companion file ESMCI\_WebServClientInfo.C
// contains the full code (bodies) for the ClientInfo methods.
//
// This class contains the information about a client session and provides
// the methods for setting and retrieving that information.
//
//EOPI
//-------------------------------------------------------------------------

namespace ESMCI
{

  class ESMCI_WebServClientInfo
  {
  public:

     // constructor and destructor
          ESMCI_WebServClientInfo(int  clientId);
          ~ESMCI_WebServClientInfo();

     // get methods
          int                                   clientId()              { return theClientId; }
          string                        userName()              { return theUserName; }
          string                        password()              { return thePassword; }
          int                                   status()                        { return theCurrentStatus; }
          string                        serverHost()    { return theServerHost; }
          int                                   serverPort()    { return theServerPort; }
          string                        jobId()                 { return theJobId; }
          vector<string>        outputFiles()   { return theOutputFiles; }

     // set methods
          void  setUserName(string  userName);
          void  setPassword(string  password);
     void  setStatus(int  status);
          void  setServerHost(string  serverHost);
          void  setServerPort(int  serverPort);
          void  setJobId(string  jobId);
          void  addOutputFile(string  outputFile);

     void  print();

  private:

          int                                   theClientId;                    // the unique id for the client
     string          theUserName;         // the client's login name on the svr
                                          //   server
     string          thePassword;         // the client's password on the svr
          int                                   theCurrentStatus;               // the current status of the client
          string                                theServerHost;                  // the name of the host to which the
                                          //   client is connected
          int                                   theServerPort;       // the port number of the socket to
                                          //   which the client is connected
     string                             theJobId;                          // the unique id for the component
                                          //   service for this client
          vector<string>        theOutputFiles;                 // the list of output files
  };

} // end namespace

#endif          // ESMCI_WebServClientInfo_H
