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

#ifndef ESMCI_WebServCompSvrClient_H
#define ESMCI_WebServCompSvrClient_H

#include <stdlib.h>
#include <string>
#include <vector>

#include "ESMCI_WebServServerSocket.h"
#include "ESMCI_WebServClientSocket.h"
#include "ESMCI_WebServNetEsmf.h"
#include "ESMCI_WebServNetEsmfClient.h"
#include "ESMCI_WebServDataDesc.h"
#include "ESMCI_WebServDataContent.h"

using namespace std;

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::ESMCI_WebServCompSvrClient
//
// !DESCRIPTION:
//
// The code in this file defines the C++ CompSvrClient members and method
// signatures (prototypes).  The companion file ESMCI\_WebServCompSvrClient.C
// contains the full code (bodies) for the CompSvrClient methods.
//
// This class provides the capability to connect and communicate with a
// ESMF Component service implemented with the ESMCI_WebServComponentSvr class.
// This class is intended to be used only by a PassThruSvr service which
// is the intermediary between the client (web service) and the component
// service.
//
//EOPI
//-------------------------------------------------------------------------

namespace ESMCI
{

  class ESMCI_WebServCompSvrClient : public ESMCI_WebServNetEsmfClient
  {
  public:

     // constructor and destructor
          ESMCI_WebServCompSvrClient(const char*  host,
                                int          port,
                                int          clientId);
          ~ESMCI_WebServCompSvrClient();

     // methods to setup the connection parameters
          void setClientId(int  clientId);

     // methods to send client requests to the server
     int                                init();
     int                                run();
     int                                timestep(int  numTimesteps);
     int                                final();
     int                                state();
     ESMCI_WebServDataDesc*             dataDesc();
     ESMCI_WebServDataContent*  outputData(double      timestamp,
                                            int*        retNumVars,
                                            string**    retVarNames,
                                            int*        retNumLats,
                                            int*        retNumLons);
     vector<string>                     files();
     int                                end();
     int                                killServer();


  private:

          int                                                           theClientId;                    // id of the client
     ESMCI_WebServDataDesc*     theOutputDataDesc;      // description of the data
  };

} // end namespace

#endif          // ESMCI_WebServCompSvrClient_H
