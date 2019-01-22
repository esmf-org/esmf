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

#ifndef ESMCI_WebServNetEsmfServer_H
#define ESMCI_WebServNetEsmfServer_H

#include "ESMCI_WebServServerSocket.h"
#include "ESMCI_WebServClientSocket.h"
#include "ESMCI_Comp.h"
#include "ESMCI_Util.h"
#include "ESMCI_WebServNetEsmf.h"
#include "ESMCI_WebServClientInfo.h"
#include <map>

using namespace std;

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::ESMCI_WebServNetEsmfServer
//
// !DESCRIPTION:
//
// The code in this file defines the C++ NetEsmfServer members and method
// signatures (prototypes).  The companion file ESMCI\_WebServNetEsmfServer.C
// contains the full code (bodies) for the NetEsmfServer methods.
//
// This class provides the basic functionality setting up an ESMF Component
// (Grid or Coupler) as a network-accessible service.  It provides the
// methods to setup a socket service and process client requests.
//
//EOPI
//-------------------------------------------------------------------------

namespace ESMCI
{

  class ESMCI_WebServNetEsmfServer
  {
  public:

     // Component type - Grid or Coupler
     typedef enum ESMC_CompType
          {
                  ESMC_COMPTYPE_GRID = 0,
                  ESMC_COMPTYPE_COUPLER = 1
          } ESMC_CompType;

     // constructor and destructor
          ESMCI_WebServNetEsmfServer(int  port);
          ~ESMCI_WebServNetEsmfServer();

     // port number access methods
          int  getPort()                { return thePort; }
          void setPort(int  port);

     // methods to setup socket service loop... one for grid components and
     // one for coupler components
          int  requestLoop(ESMCI::GridComp*       comp,
                      ESMCI::State*      importState,
                      ESMCI::State*      exportState,
                      ESMCI::Clock*      clock,
                      int                phase,
                      ESMC_BlockingFlag  blockingFlag);

          int  requestLoop(ESMCI::CplComp*        comp,
                      ESMCI::State*      importState,
                      ESMCI::State*      exportState,
                      ESMCI::Clock*      clock,
                      int                phase,
                      ESMC_BlockingFlag  blockingFlag);

  private:

     // methods to handle incoming requests
          int  getNextRequest();
          int  serviceRequest(int  request);

          int   getRequestId(const char  request[]);
          char* getRequestFromId(int  id);

     // process request methods
          int   processNew();
          int   processInit();
          int   processRun();
          int   processFinal();
          int   processState();
          int   processFiles();
          int   processEnd();
          int   processPing();

     // internal data access methods
          int  getNextClientId();
          void  copyFile(const char*  srcFilename,
                    const char*  destFilename);

          int                                   thePort;                        // the port number of the socket service
          char                          theMsg[8192];   // buffer to store client requests
          char*                                 theStatus;              // the current status of the service

          ESMCI_WebServServerSocket             theSocket;              // the server socket

          ESMC_CompType                         theCompType;            // the component type
          ESMCI::GridComp*              theGridComp;            // pointer to grid component
          ESMCI::CplComp*                       theCouplerComp;         // pointer to coupler component
     ESMCI::State*                      theImportState;         // component import state object
     ESMCI::State*                      theExportState;         // component export state object
     ESMCI::Clock*                      theClock;                       // component clock
          int                                                   thePhase;                       // component phase
          ESMC_BlockingFlag             theBlockingFlag;        // component blocking flag

     // list of client sessions and counter to keep track of the next
     // client session id
          map<int, ESMCI_WebServClientInfo*>    theClients;
          int                                                                                           theNextClientId;
  };

} // end namespace

#endif          // ESMCI_WebServNetEsmfServer_H
