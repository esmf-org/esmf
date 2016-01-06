// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMCI_CompTunnel_H
#define ESMCI_CompTunnel_H

//-----------------------------------------------------------------------------
//BOPI
// !CLASS:  ESMCI::CompTunnel - component tunnel.
//
// !DESCRIPTION:
// 
//EOPI
//-----------------------------------------------------------------------------

#include <string>
#include <vector>

#include "ESMCI_Comp.h"
#include "ESMCI_FTable.h"

namespace ESMCI {

//==============================================================================
//==============================================================================
// CompTunnel
//==============================================================================
//==============================================================================

class CompTunnel{
  private:
    bool connected;
    bool vmBased;
    bool socketBased;
    //--------------
    Comp *dual;         // set if this is the dual side
    Comp *actual;       // set if this is the actual side
    //--------------
    enum method method;
    int phase;
    State *importState;
    State *exportState;
    Clock **clock;
    //--------------
    //--------------
    Comp *localActualComp; // in case the actual component happens to be local
    int localActualCompRootPet;
    cargotype localActualCompCargo;
    //--------------
    //--------------
    VM *bridgeVM;
    //--------------
    //-- vars for the current negotiation prototype - may move into inter-VM --
    int tag;            // safe tag for bridgeVM communications
    int interLocalPet;  // local PET in terms of bridgeVM
    int interRootPet;   // this side's rootPET in terms of bridgeVM
    int localPet;       // local PET in terms of this side's VM
    int rootPet;        // this side's rootPet in terms of this side's VM
    std::vector<int> localSendToPetList;  // other side's PETs terms of bridgeVM
    int localRecvFromPet; // other side's PET that sends in terms of bridgeVM 
    //--------------
    bool outstandingWaitFlag; // indicate whether a local wait call is required
    //--------------
    //--------------
    int sock;
    int port;
    std::string server;
    double timeout;     // timeout of current dual operation in seconds
    bool masterFlag;
    //--------------
  private:
    void zeroOut(void){
      connected = false;
      vmBased = false;
      socketBased = false;
      dual = NULL;
      actual = NULL;
      method = METHOD_NONE;
      phase = 0;
      importState = NULL;
      exportState = NULL;
      clock = NULL;
      localActualComp = NULL;
      bridgeVM = NULL;
      localSendToPetList.resize(0);
      outstandingWaitFlag = false;
      sock = -1;
      port = -1;
      masterFlag = false;
    }
    void chat(char *buffer);
  public:
    // native C++ constructors/destructors
    CompTunnel(void){
      zeroOut();
    }
    CompTunnel(Comp *localActualComp_, int localActualCompRootPet_){
      zeroOut();
      localActualComp = localActualComp_;
      localActualCompRootPet = localActualCompRootPet_;
      vmBased = true;
    }
    CompTunnel(VM *bridgeVM_){
      zeroOut();
      bridgeVM = bridgeVM_;
      vmBased = true;
    }
    CompTunnel(int port_){
      zeroOut();
      port = port_;
      socketBased = true;
    }
    CompTunnel(int port_, std::string server_){
      zeroOut();
      port = port_;
      server = server_;
      socketBased = true;
    }
    ~CompTunnel(void){
      //TODO: It may be good to move this into CompTunnel.C and to call
      //TODO: socketFinal() on the masterPet for a socketBased tunnel.
      connected = false;
      dual = NULL;
      actual = NULL;
      localActualComp = NULL;
    }
    // other methods
    void setConnected(bool connected_){ connected=connected_;}
    bool isConnected(){ return connected;}
    //--------------
    void setDual(Comp *dual_){ dual=dual_;}
    void setActual(Comp *actual_){ actual=actual_;}
    Comp *getDual(){ return dual;}
    Comp *getActual(){ return actual;}
    //--------------
    void setMethod(enum method method_){ method=method_;}
    void setPhase(int phase_){ phase=phase_;}
    void setImportState(State *state){ importState=state;}
    void setExportState(State *state){ exportState=state;}
    void setClock(Clock **clock_){ clock=clock_;}
    void setTimeout(double timeout_){ timeout=timeout_;}
    State *getImportState()const{ return importState; }
    State *getExportState()const{ return exportState; }
    Clock **getClock()const{ return clock; }
    double getTimeout()const{ return timeout; }
    //--------------
    int print(void)const;
    static void setServicesWrap(Comp *dualComp, int *rc);
    int setServices(Comp *dualComp);
    int execute(cargotype *cargo);
    static void waitWrap(Comp *dualComp, int *rc);
    int wait(cargotype *cargo);
    // --- comm methods --------------
    int negotiate(double timeout);
    int dual2actual(void *msg, int len, double timeout);
    int actual2dual(void *msg, int len, double timeout);
};

//==============================================================================
//==============================================================================
// ServiceLoop
//==============================================================================
//==============================================================================

void ServiceLoop(Comp *, State *, State *, Clock **, int *);

} // namespace ESMCI

#endif  // ESMCI_CompTunnel_H
