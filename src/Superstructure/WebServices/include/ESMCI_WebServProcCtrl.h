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

#ifndef ESMCI_WebServProcCtrl_H
#define ESMCI_WebServProcCtrl_H

#include "ESMCI_WebServServerSocket.h"
#include "ESMCI_WebServClientSocket.h"
#include "ESMCI_WebServNetEsmf.h"
#include "ESMCI_WebServClientInfo.h"
#include "ESMCI_WebServCompSvrMgr.h"
#include <map>

using namespace std;

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::ESMCI_WebServProcCtrl
//
// !DESCRIPTION:
//
// The code in this file defines the C++ ProcCtrl members and method
// signatures (prototypes).  The companion file ESMCI\_WebServProcCtrl.C
// contains the full code (bodies) for the ProcCtrl methods.
//
// This class provides the basic functionality setting up Process Controller
// Service, which essentially just passes requests from its client on to an
// ESMF Component Service (implemented with the ESMCI_ComponentSvr class).
//
//EOPI
//-------------------------------------------------------------------------

namespace ESMCI
{

  class ESMCI_WebServProcCtrl
  {
  public:

     typedef enum ESMC_JobMgrType
     {
        ESMC_JOBMGRTYPE_FORK = 0,
        ESMC_JOBMGRTYPE_GLOBUS = 1
     } ESMC_JobMgrType;

     // constructor and destructor
          ESMCI_WebServProcCtrl(int              procCtrlPort,
                           string           registrarHost,
                           int              registrarPort,
                           string           compSvrHost,
                           int              compSvrStartPort,
                           int              portPoolSize,
                           string           compSvrScriptDir,
                           string           compSvrScriptName,
                           ESMC_JobMgrType  jobMgrType);
          ~ESMCI_WebServProcCtrl();

     // data member access methods
          int              getProcCtrlPort()       { return theProcCtrlPort; }
          string           getRegistrarHost()      { return theRegistrarHost; }
          int              getRegistrarPort()           { return theRegistrarPort; }
          string           getCompSvrHost()                { return theCompSvrHost; }
          int              getCompSvrStartPort()        { return theCompSvrStartPort; }
          int              getPortPoolSize()       { return thePortPoolSize; }
     string           getCompSvrScriptDir()  { return theCompSvrScriptDir; }
     string           getCompSvrScriptName() { return theCompSvrScriptName; }
     ESMC_JobMgrType  getJobMgrType()        { return theJobMgrType; }

     // method to setup socket service loop
          int  requestLoop();


  private:

     // methods to handle incoming requests
          int  getNextRequest();
          int  serviceRequest(int  request);

          int   getRequestId(const char  request[]);
          char* getRequestFromId(int  id);

     // process request methods
          int   processNew();
          int   processState();
          int   processInit();
          int   processRun();
          int   processTimestep();
          int   processFinal();
          int   processGetDataDesc();
          int   processGetData();
          int   processEnd();
          int   processExit();
          int   processPing();

     // internal data access methods
          int  getNextClientId();

          int                               theProcCtrlPort;         // listening port number
     string           theRegistrarHost;     // host name of registrar
          int                               theRegistrarPort;     // port num of registrar
     string           theCompSvrHost;       // host name of component svc
          int                               theCompSvrStartPort;  // starting port num of comp svc
          int                               thePortPoolSize;      // num of ports in pool of ports
     string           theCompSvrScriptDir;  // dir for comp svc startup script
     string           theCompSvrScriptName; // name of comp svc startup script
     ESMC_JobMgrType  theJobMgrType;        // the type of job manager

     ESMCI_WebServCompSvrMgr*           theCompSvrMgr;  // the component svr manager
          ESMCI_WebServServerSocket     theSocket;     // the server socket

     // list of client sessions and counter to keep track of the next
     // available client session id
          map<int, ESMCI_WebServClientInfo*>    theClients;
          int                                                                                           theNextClientId;
  };

} // end namespace


#endif          // ESMCI_WebServProcCtrl_H
