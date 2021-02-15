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

#ifndef ESMCI_WebServComponentSvr_H
#define ESMCI_WebServComponentSvr_H

#include "ESMCI_WebServServerSocket.h"
#include "ESMCI_WebServClientSocket.h"
#include "ESMCI_Comp.h"
#include "ESMCI_Util.h"
#include "ESMCI_WebServNetEsmf.h"
#include "ESMCI_WebServClientInfo.h"
#include "ESMCI_WebServDataMgr.h"
#include "ESMCI_WebServDataDesc.h"
#include <map>
#include <vector>

using namespace std;

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::ESMCI_WebServComponentSvr
//
// !DESCRIPTION:
//
// The code in this file defines the C++ ComponentSvr members and method
// signatures (prototypes).  The companion file ESMCI\_WebServComponentSvr.C
// contains the full code (bodies) for the ComponentSvr methods.
//
// This class provides the basic functionality setting up an ESMF Component
// (Grid only) as a network-accessible service.  It provides the methods
// to setup a socket service and process client requests.
//
// (Note: This class is essentially a subset of the ESMCI_WebServNetEsmfServer
//        class.  It was created when setting up CCSM/CAM as a Component
//        and is used in conjunction with a "Process Controller" that is
//        implemented using the ESMCI_WebServPassThruSvr.)
//
//EOPI
//-------------------------------------------------------------------------

namespace ESMCI
{

  class ESMCI_WebServComponentSvr
  {
  public:

     // constructor and destructor
          ESMCI_WebServComponentSvr(int     port,
                               int     clientId,
                               string  registrarHost);

          ~ESMCI_WebServComponentSvr();

     // port number access methods
          int  getPort()                { return thePort; }
          void setPort(int  port);

     // method to specify the output data format (variables, grid info)
     void  setOutputDesc(ESMCI_WebServDataDesc*  desc);

     // method to setup socket service loop...
          int  requestLoop(ESMCI::GridComp*       comp,
                      ESMCI::State*      importState,
                      ESMCI::State*      exportState,
                      ESMCI::Clock*      clock,
                      int                phase,
                      ESMC_BlockingFlag  blockingFlag);

     // method to setup socket service loop...
          int  cplCompRequestLoop(ESMCI::CplComp*          comp,
                             ESMCI::State*      importState,
                             ESMCI::State*      exportState,
                             ESMCI::Clock*      clock,
                             int                phase,
                             ESMC_BlockingFlag  blockingFlag);

     // methods to execute component functions... these need to be public so
     // that they can be called from a separate thread
          void  runInit(void);
          void  runRun(void);
          void  runTimeStep(void);
          void  runFinal(void);

     // methods to access other data members
     void  addOutputFilename(string  filename);
// TODO: Change input structure to support gridded data
     void  addOutputData(string  filename);

  private:

     typedef enum ESMC_WebServCompType
     {
        ESMC_WEBSERVCOMPTYPE_GRID = 0,
        ESMC_WEBSERVCOMPTYPE_COUPLER = 1
     } ESMC_WebServCompType;

     // methods to handle incoming requests
          int  getNextRequest();
          int  serviceRequest(int  request);

          int   getRequestId(const char  request[]);
          char* getRequestFromId(int  id);

     // internal data access methods
          void  setStatus(int  status);

     // process request methods
          int  processInit();
          int  processRun();
          int  processTimestep();
          int  processFinal();
          int  processState();
          int  processFiles();
          int  processGetDataDesc();
          int  processGetData();
          int  processEnd();

          int                   thePort;                        // the port number of the socket service
          char          theMsg[8192];   // buffer to store client requests

          ESMCI_WebServServerSocket             theSocket;              // the server socket

          ESMCI::CplComp*                       theCplComp;                     // pointer to coupler component
          ESMCI::GridComp*              theGridComp;            // pointer to grid component
     ESMCI::State*                      theImportState;         // component import state object
     ESMCI::State*                      theExportState;         // component export state object
     ESMCI::Clock*                      theClock;                       // component clock
          int                                                   thePhase;                       // component phase
          ESMC_BlockingFlag             theBlockingFlag;        // component blocking flag
     ESMC_WebServCompType       theCompType;

     string theRegistrarHost;    // the host on which the registrar is running
     int                theCurrentClientId;     // the id of the client currently accessing
                                 // the component service
          int           theCurrentStatus;               // the current status of the service

     vector<string>     theOutputFiles;         // the list of output files
     ESMCI_WebServDataMgr*      theOutputData;

#ifndef ESMF_NO_PTHREADS
          pthread_mutex_t       theStatusMutex;         // mutex lock for the current status
          pthread_mutex_t       theDataMutex;           // mutex lock for the output data
#endif
  };

} // end namespace

#endif          // ESMCI_WebServComponentSvr_H
