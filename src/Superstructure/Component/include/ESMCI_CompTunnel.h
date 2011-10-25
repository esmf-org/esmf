// $Id: ESMCI_CompTunnel.h,v 1.1 2011/10/25 23:05:39 theurich Exp $
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
    Comp *localComp;    // in case the actual component happens to be local
    cargotype localCargo;
    //--------------
    VM *interVM;
    //--------------
    //-- vars for the current negotiation prototype - may move into inter-VM --
    int interLocalPet;  // local PET on the interVM
    int dualRootPet;    // designated rootPET on the dual side within interVM
    int actualRootPet;  // designated rootPET on the actual side within interVM
    //--------------
  public:
    // native C++ constructors/destructors
    CompTunnel(void){
      connected = false;
      dual = NULL;
      actual = NULL;
      localComp = NULL;
    }
    CompTunnel(Comp *localActualComp){
      connected = false;
      dual = NULL;
      actual = NULL;
      localComp = localActualComp;
    }
    CompTunnel(VM *interVM_){
      connected = false;
      dual = NULL;
      actual = NULL;
      localComp = NULL;
      interVM = interVM_;
    }
    ~CompTunnel(void){
      connected = false;
      dual = NULL;
      actual = NULL;
      localComp = NULL;
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
