// $Id: ESMCI_CompTunnel.C,v 1.1 2011/10/25 23:05:35 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2011, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_CompTunnel.C"
//==============================================================================
//
// ESMCI CompTunnel implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt CompTunnel} methods 
// declared in the companion file {\tt ESMCI\_CompTunnel.h}.  
//
//-----------------------------------------------------------------------------

// include associated header file
#include "ESMCI_CompTunnel.h"

// insert higher level, 3rd party or system includes
#include <string>
#ifndef ESMF_NO_DLFCN
#include <dlfcn.h>
#endif

// include ESMF headers
#include "ESMCI_Macros.h"

// LogErr
#include "ESMCI_LogErr.h"
#include "ESMF_LogMacros.inc"

//==============================================================================
//==============================================================================
// CompTunnel class implementation
//==============================================================================
//==============================================================================

namespace ESMCI {
  
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::CompTunnel::print()"
  int CompTunnel::print(void)const{
    int rc = ESMC_RC_NOT_IMPL;
    printf("localComp: %p\n", localComp);
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
  
  
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::CompTunnel::setServicesWrap()"
  void CompTunnel::setServicesWrap(Comp *dualComp, int *rc){
    // This nethod is executed on the dualComp VM
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // get a pointer to the CompTunnel object that is now inside of the dualComp
    ESMCI::CompTunnel *compTunnel;
    localrc = dualComp->getTunnel(&compTunnel);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, rc))
      return; // bail out
    // call into setServices for this compTunnel object
    localrc = compTunnel->setServices(dualComp);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, rc)) 
      return; // bail out
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

  
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::CompTunnel::setServices()"
  int CompTunnel::setServices(Comp *dualComp){
    // This nethod is executed on the dualComp VM
    int rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    
    //TODO: need to create a joint-VM (that works even for the threaded case)
    //TODO: that combines two VMs
    
    //TODO: For now simply set the parentVM, hoping that there aren't any PETs
    //TODO: that are neither part of dual or actual component (in which case 
    //TODO: this approach will break down.
    
    localrc = dualComp->getVmParent(&interVM);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
      return rc;
    
    // negotiation phase
    setDual(dualComp);  // set this side as the dual side
    localrc = negotiate();
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
      return rc;
    
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::CompTunnel::execute()"
  int CompTunnel::execute(cargotype *cargo){
    int rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    //
    // This method is executing on the dual component VM.
    // Do pre-execution data transfer and kick-off actual component execution.
    //
printf("calling CompTunnel::execute() for name: %s\n", cargo->name);
    if (interVM){
      
      
      
      interVM->barrier();  //TODO: very simple control for now

    }else if (localComp){
      // the actual component is local
      localCargo = *cargo;   // copy dual component's cargo structure
      localCargo.f90comp = localComp;  // overwrite f90comp member
      
      ESMCI::FTable *ftable;
      localrc = localComp->getFTable(&ftable);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
        return rc;
      localCargo.ftable = ftable;
      
      ESMCI::VM *vm_parent;
      localrc = localComp->getVmParent(&vm_parent);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
        return rc;
      ESMCI::VMPlan *vmplan;
      localrc = localComp->getVmPlan(&vmplan);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
        return rc;
      void *vm_info;
      localrc = localComp->getVmInfo(&vm_info);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
        return rc;

printf("now calling into vm_parent->enter() from CompTunnel::execute()\n");

      //TODO: before calling into the actual component here make sure that
      //TODO: the arguments have been set up correctly for the callback!!!
      
      // enter the child VM -> resurface in ESMCI_FTableCallEntryPointVMHop()
      localrc = vm_parent->enter(vmplan, vm_info, (void*)&localCargo);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc)) 
        return rc; // bail out
            
      // ... if the child VM uses threads (multi-threading or single-threading) 
      // then this parent PET continues running concurrently to the child PET
      // in the same VAS! In that case the return codes in cargo are not valid
      // here! The status returned by VM::enter() indicates that success of
      // entering the child VM, not failure or success of the callback.
      // The return code of the callback code will be valid in all cases
      // (threading or no threading) _after_ VMK::exit() returns.
    }
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::CompTunnel::wait()"
  int CompTunnel::wait(cargotype *cargo){
    int rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    //
    // This method is executing on the dual component VM. (not sure if true???)
    // Do post-execution data transfer and wait for actual component to finish.
    //
printf("calling CompTunnel::wait() for name: %s\n", cargo->name);
    if (localComp){
      // the actual component is local
      ESMCI::VM *vm_parent;
      localrc = localComp->getVmParent(&vm_parent);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
        return rc;
      ESMCI::VMPlan *vmplan;
      localrc = localComp->getVmPlan(&vmplan);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
        return rc;
      void *vm_info;
      localrc = localComp->getVmInfo(&vm_info);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
        return rc;

printf("now calling into vm_parent->exit() from CompTunnel::wait()\n");
      
      // Now call the vmk_exit function which will block respective PETs
      vm_parent->exit(static_cast<ESMCI::VMKPlan *>(vmplan), vm_info);

    }
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
  
  
  int CompTunnel::negotiate(){
    int rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // check that dual/actual are uniquely defined
    Comp *comp = NULL;
    bool isDual = false;    // initialize
    bool isActual = false;  // initialize
    if (getDual()){ isDual = true; comp = getDual();}
    if (getActual()){ isActual = true; comp = getActual();}
    if ((isDual && isActual) || (!isDual && !isActual)){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, "- CompTunnel is bad!",
        &rc);
      return rc;
    }
    // get the Component's VM
    VM *vm;
    localrc = comp->getVm(&vm);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
      return rc; // bail out
    // every PET to determine which unique id it holds in the inter-VM
    //TODO: Of course this does not work if a child VM has multiple PETs spawned
    //TODO: from a single parent VM PET. Things get more involved for that case.
    interLocalPet = interVM->getLocalPet();
    
    
    
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
    

} // namespace ESMCI
  
//==============================================================================
//==============================================================================
// ServiceLoop implementation
//==============================================================================
//==============================================================================

namespace ESMCI {
  void ServiceLoop(Comp *comp, State *importState, State *exportState, 
    Clock **clock, int *rc){
    // This method is executed on the actual component VM
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    int userRc;

ESMC_LogDefault.Write("ServiceLoop: entering", ESMC_LOG_INFO);

    //TODO: for now simply use the parent VM, this will change!!!!
    VM *parentVM;
    localrc = comp->getVmParent(&parentVM);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, rc))
      return; // bail out
   
    CompTunnel compTunnel(parentVM);  // local compTunnel object

    // negotiation phase
    compTunnel.setActual(comp); // set as the actual side
    localrc = compTunnel.negotiate();
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, rc))
      return; // bail out
    
    // execution loop phase
    for (int i=0; i<3; i++){
      
      parentVM->barrier();  //TODO: for now very simple test for control
      
      localrc = comp->execute(METHOD_INITIALIZE, importState, exportState,
        *clock, ESMF_VASBLOCKING, 1, &userRc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, rc))
        return; // bail out
      localrc = comp->execute(METHOD_RUN, importState, exportState,
        *clock, ESMF_VASBLOCKING, 1, &userRc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, rc))
        return; // bail out
      
    }
    
ESMC_LogDefault.Write("ServiceLoop: exiting", ESMC_LOG_INFO);
    
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }
} // namespace ESMCI
  

