// $Id: ESMCI_CompTunnel.h,v 1.2 2011/10/27 21:38:27 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2011, University Corporation for Atmospheric Research, 
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
    Comp *localActualComp; // in case the actual component happens to be local
    int localActualCompRootPet;
    cargotype localActualCompCargo;
    //--------------
    VM *bridgeVM;
    //--------------
    //-- vars for the current negotiation prototype - may move into inter-VM --
    int interLocalPet;  // local PET in terms of bridgeVM
    int interRootPet;   // this side's rootPET in terms of bridgeVM
    int localPet;       // local PET in terms of this side's VM
    int rootPet;        // this side's rootPet in terms of this side's VM
    std::vector<int> localSendToPetList;  // other side's PETs terms of bridgeVM
    int localRecvFromPet; // other side's PET that sends in terms of bridgeVM 
    //--------------
  public:
    // native C++ constructors/destructors
    CompTunnel(void){
      connected = false;
      dual = NULL;
      actual = NULL;
      localActualComp = NULL;
      localSendToPetList.resize(0);
    }
    CompTunnel(Comp *localActualComp_, int localActualCompRootPet_){
      connected = false;
      dual = NULL;
      actual = NULL;
      localActualComp = localActualComp_;
      localActualCompRootPet = localActualCompRootPet_;
      localSendToPetList.resize(0);
    }
    CompTunnel(VM *bridgeVM_){
      connected = false;
      dual = NULL;
      actual = NULL;
      localActualComp = NULL;
      bridgeVM = bridgeVM_;
      localSendToPetList.resize(0);
    }
    ~CompTunnel(void){
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
    //--------------
    int print(void)const;
    static void setServicesWrap(Comp *dualComp, int *rc);
    int setServices(Comp *dualComp);
    int execute(cargotype *cargo);
    int wait(cargotype *cargo);
    //--------------
    int negotiate();
};

//==============================================================================
//==============================================================================
// ServiceLoop
//==============================================================================
//==============================================================================

void ServiceLoop(Comp *, State *, State *, Clock **, int *);

} // namespace ESMCI

#endif  // ESMCI_CompTunnel_H
