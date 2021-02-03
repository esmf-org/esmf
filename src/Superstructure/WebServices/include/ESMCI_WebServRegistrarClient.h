// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
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

#ifndef ESMCI_WebServRegistrarClient_H
#define ESMCI_WebServRegistrarClient_H

#include <stdlib.h>

#include "ESMCI_WebServNetEsmfClient.h"
#include "ESMCI_WebServCompSvrInfo.h"

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::ESMCI_WebServRegistrarClient
//
// !DESCRIPTION:
//
// The code in this file defines the C++ RegistrarClient members and method
// signatures (prototypes).  The companion file ESMCI\_WebServRegistrarClient.C
// contains the full code (bodies) for the RegistrarClient methods.
//
// This class provides the capability to connect and communicate with a
// Registrar service.
//
//EOPI
//-------------------------------------------------------------------------

// The default port number for the Registrar
#define REGISTRAR_PORT  45002

namespace ESMCI
{

  class ESMCI_WebServRegistrarClient : public ESMCI_WebServNetEsmfClient
  {
  public:

     // constructor and destructor
          ESMCI_WebServRegistrarClient(const char*  host,
                                  int          port);
          ~ESMCI_WebServRegistrarClient();

     // methods to send client requests to the server
          int  registerComp(const char*  clientId,
                       const char*  hostName,
                       const char*  portNum);

          int  compSubmitted(const char*  clientId,
                        const char*  jobId);

          int  compStarted(const char*  clientId,
                      const char*  compName,
                      const char*  compDesc,
                      const char*  physHostName);

          int  getComponent(const char*                clientId,
                       ESMCI_WebServCompSvrInfo*  compSvrInfo);

          int  getStatus(const char*  clientId);

          int  setStatus(const char*  clientId,
                    const char*  status);

          int  unregisterComp(const char*  clientId);

  private:

  };

} // end namespace

#endif          // ESMCI_WebServRegistrarClient_H
