// $Id: ESMCI_CompTunnel.C,v 1.5 2011/11/03 20:17:43 theurich Exp $
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
#include "ESMCI_VMKernel.h"

// LogErr
#include "ESMCI_LogErr.h"
#include "ESMF_LogMacros.inc"

//==============================================================================
//==============================================================================
// CompTunnel class implementation
//==============================================================================
//==============================================================================

extern "C" {

  // Fortran access point to native class destructor
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_comptunneldestroy"
  void FTN(c_esmc_comptunneldestroy)(ESMCI::CompTunnel **ptr, int *rc){
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    if (*ptr == NULL){
      ESMC_LogDefault.MsgAllocError("- CompTunnel deallocation", rc);  
      return;
    }
    delete (*ptr);
    *ptr = NULL;
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

} // extern "C"



namespace ESMCI {
  
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::CompTunnel::print()"
  int CompTunnel::print(void)const{
    int rc = ESMC_RC_NOT_IMPL;
    printf("localActualComp: %p\n", localActualComp);
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
  
  
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::CompTunnel::setServicesWrap()"
  void CompTunnel::setServicesWrap(Comp *dualComp, int *rc){
    // This method is executed on the dualComp VM
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
    // This method is executed on the dualComp VM
    int rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    // use the parent VM as bridge during the negotiation    
    localrc = dualComp->getVmParent(&bridgeVM);
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
    
    if (method==METHOD_WAIT){
      
printf("in CompTunnel::execute() ... call CompTunnel::wait, cargo=%p\n", cargo);

      localrc = wait(cargo);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
        return rc;  // bail out
      

    }else if (bridgeVM){
      
printf("in CompTunnel::execute() ... call CompTunnel::dual2actual() with method %d\n", method);

      // send method info to actual component
      localrc = dual2actual(&method, sizeof(enum method));
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
        return rc;  // bail out

      // send phase info to actual component
      localrc = dual2actual(&phase, sizeof(int));
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
        return rc;  // bail out

    }else if (localActualComp){
      // the actual component is local
      //TODO: NOT SURE THIS WORKS ANYMORE!!!
      localActualCompCargo = *cargo;   // copy dual component's cargo structure
      localActualCompCargo.f90comp = localActualComp;  // overwrite f90comp
      
      ESMCI::FTable *ftable;
      localrc = localActualComp->getFTable(&ftable);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
        return rc;  // bail out
      localActualCompCargo.ftable = ftable;
      
      ESMCI::VM *vm_parent;
      localrc = localActualComp->getVmParent(&vm_parent);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
        return rc;  // bail out
      ESMCI::VMPlan *vmplan;
      localrc = localActualComp->getVmPlan(&vmplan);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
        return rc;  // bail out
      void *vm_info;
      localrc = localActualComp->getVmInfo(&vm_info);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
        return rc;  // bail out

printf("now calling into vm_parent->enter() from CompTunnel::execute()\n");

      //TODO: before calling into the actual component here make sure that
      //TODO: the arguments have been set up correctly for the callback!!!
      
      // enter the child VM -> resurface in ESMCI_FTableCallEntryPointVMHop()
      localrc = vm_parent->enter(vmplan, vm_info, (void*)&localActualCompCargo);
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
    // This method is executing on the dual component VM.
    // Do post-execution data transfer and wait for actual component to finish.
    //
printf("calling CompTunnel::wait() for name: %s\n", cargo->name);
    if (bridgeVM){
      
printf("in CompTunnel::wait() ... \n");

      int rcs[2];
      localrc = actual2dual(rcs, 2*sizeof(int));
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
        return rc;
      
printf("in CompTunnel::wait() ... received RC=%d, userRC=%d\n", rcs[0], rcs[1]);
      
      // pass the actual return codes back through dual cargo
      cargo->esmfrc[0] = rcs[0];
      cargo->userrc[0] = rcs[1];
      
    }else if (localActualComp){
      // the actual component is local
      //TODO: NOT SURE THIS WORKS ANYMORE!!!
      ESMCI::VM *vm_parent;
      localrc = localActualComp->getVmParent(&vm_parent);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
        return rc;
      ESMCI::VMPlan *vmplan;
      localrc = localActualComp->getVmPlan(&vmplan);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
        return rc;
      void *vm_info;
      localrc = localActualComp->getVmInfo(&vm_info);
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
  
  
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::CompTunnel::negotiate()"
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
    interLocalPet = bridgeVM->getLocalPet();  // localPet in terms of bridgeVM
    localPet = vm->getLocalPet();             // localPet in terms of VM
    int *localPets = new int[2];
    localPets[0] = interLocalPet;
    localPets[1] = localPet;
    int petCount = vm->getPetCount();
    int *petList = new int[2*petCount];
    vm->allgather(localPets, petList, 2*sizeof(int));
    interRootPet = petList[0];  // prime the search variable
    rootPet = petList[1];       // prime the associated variable
    for (int i=1; i<petCount; i++){
      if (petList[i*2] < interRootPet){
        interRootPet = petList[i*2];
        rootPet = petList[i*2];
      }
    }
    delete [] localPets;

#if 0
if (localActualComp){
printf("local rootPet was determined as %d and remote is %d\n", interRootPet, localActualCompRootPet);
}else{
printf("local rootPet was determined as %d\n", interRootPet);
}
#endif 

    // standard rootPet variables
    int interDualRootPet;
    int interActualRootPet;
    
    // dual and actual comps to rendezvous on the known actual component rootPet
    //TODO: this is the part that depends on the fact that there is a
    //TODO: localActualComp, because if not then the rendezvous is different 
    
    if (localActualComp){
      // this is a dual component side with locally associated actual component
      // already know rootPet of both sides
      interActualRootPet = localActualCompRootPet;
    }else{
      // this is the actual component side
      interActualRootPet = interRootPet;
    }    
    
    // construct a safe tag for communicating across the bridgeVM
    tag = vm->getMaxTag() - interActualRootPet;

    // now do the initial rendezvous
    if (localActualComp){
      // this is a dual component side with locally associated actual component
      // already know rootPet of both sides
      interDualRootPet = interRootPet;
      if (interLocalPet == interRootPet){
        // rootPet to send information over to the rendezvous PET
//printf("dualComp is sending %d -> %d\n", interRootPet, interActualRootPet);
        bridgeVM->send(&interDualRootPet, sizeof(int), interActualRootPet, tag);
      }
    }else{
      // this is the actual component side
      if (interLocalPet == interRootPet){
        // rootPet on the actual component side receives the dual side rootPET, 
        // but for this transfer the dual side rootPet is yet unknown on the
        // actual component side
//printf("actualComp is receiving ANY -> %d\n", interRootPet);
        bridgeVM->recv(&interDualRootPet, sizeof(int), VM_ANY_SRC, tag);
      }
      // tell all PETs on the actual component side about the dual rootPet
      vm->broadcast(&interDualRootPet, sizeof(int), rootPet);
//printf("all actualComp PETs know that interDualRootPet is: %d\n", interDualRootPet);
    }
    
    //TODO: now that each side knows about the both side rootPet an 
    //TODO: interCommunicator could be created -> need to wrap that into VM
    
    // standard petCount variables
    int dualPetCount;
    int actualPetCount;

    // dual and actual comps to exchange petCount info
    if (isDual){
      // this is a dual component side
      dualPetCount = petCount;
      if (interLocalPet == interRootPet){
        bridgeVM->send(&dualPetCount, sizeof(int), interActualRootPet, tag);
        bridgeVM->recv(&actualPetCount, sizeof(int), interActualRootPet, tag);
      }
      // tell all PETs on the dual component side about the actual petCount
      vm->broadcast(&actualPetCount, sizeof(int), rootPet);
    }else{
      // this is the actual component side
      actualPetCount = petCount;
      if (interLocalPet == interRootPet){
        bridgeVM->recv(&dualPetCount, sizeof(int), interDualRootPet, tag);
        bridgeVM->send(&actualPetCount, sizeof(int), interDualRootPet, tag);
      }
      // tell all PETs on the actual component side about the dual petCount
      vm->broadcast(&dualPetCount, sizeof(int), rootPet);
    }
    
//printf("now every PET knows dualPetCount=%d, actualPetCount=%d\n", dualPetCount, actualPetCount);

    // standard petList variables
    int *dualPetList = new int[dualPetCount];
    int *actualPetList = new int[actualPetCount];

    // dual and actual comps to exchange petList info
    if (isDual){
      // this is a dual component side
      for (int i=0; i<dualPetCount; i++)
        dualPetList[i] = petList[i*2];  // pull out the interPet values
      if (interLocalPet == interRootPet){
        bridgeVM->send(dualPetList, dualPetCount*sizeof(int),
          interActualRootPet, tag);
        bridgeVM->recv(actualPetList, actualPetCount*sizeof(int),
          interActualRootPet, tag);
      }
      // tell all PETs on the dual component side about the actual petList
      vm->broadcast(actualPetList, actualPetCount*sizeof(int), rootPet);
    }else{
      // this is the actual component side
      for (int i=0; i<actualPetCount; i++)
        actualPetList[i] = petList[i*2];  // pull out the interPet values
      if (interLocalPet == interRootPet){
        bridgeVM->recv(dualPetList, dualPetCount*sizeof(int),
          interDualRootPet, tag);
        bridgeVM->send(actualPetList, actualPetCount*sizeof(int),
          interDualRootPet, tag);
      }
      // tell all PETs on the actual component side about the dual petList
      vm->broadcast(dualPetList, dualPetCount*sizeof(int), rootPet);
    }
    
    // now construct the tunnel communication pattern: dual <-> actual PET map
    
    // determine send pattern for the localPet to be used in tunnel comms
    if (isDual){
      // this is a dual component side
      int startIndex = localPet * (actualPetCount / dualPetCount);
      if (localPet < actualPetCount % dualPetCount) startIndex += localPet;
      else startIndex += actualPetCount % dualPetCount;
      int startIndexNext = (localPet+1) * (actualPetCount / dualPetCount);
      if ((localPet+1) < actualPetCount % dualPetCount)
        startIndexNext += (localPet+1);
      else startIndexNext += actualPetCount % dualPetCount;
      for (int i=startIndex; i<startIndexNext; i++)
        localSendToPetList.push_back(actualPetList[i]);
    }else{
      // this is the actual component side
      int startIndex = localPet * (dualPetCount / actualPetCount);
      if (localPet < dualPetCount % actualPetCount) startIndex += localPet;
      else startIndex += dualPetCount % actualPetCount;
      int startIndexNext = (localPet+1) * (dualPetCount / actualPetCount);
      if ((localPet+1) < dualPetCount % actualPetCount)
        startIndexNext += (localPet+1);
      else startIndexNext += dualPetCount % actualPetCount;
      for (int i=startIndex; i<startIndexNext; i++)
        localSendToPetList.push_back(dualPetList[i]);
    }

    // determine the receive pattern for the localPet to be used in tunnel comms
    if (isDual){
      // this is a dual component side
      int ratio = actualPetCount / dualPetCount;
      if (ratio >= 1)
        localRecvFromPet = actualPetList[localPet];
      else{
        int revratio = dualPetCount / actualPetCount;
        int extraLimit = dualPetCount % actualPetCount;
        int limitSum = (revratio+1)*extraLimit;
        if (localPet >= limitSum){
          int index = extraLimit + (localPet - limitSum);
          localRecvFromPet = actualPetList[index];
        }else{
          int index = localPet / (revratio+1);
          localRecvFromPet = actualPetList[index];          
        }
      }
    }else{
      // this is the actual component side
      int ratio = dualPetCount / actualPetCount;
      if (ratio >= 1)
        localRecvFromPet = dualPetList[localPet];
      else{
        int revratio = actualPetCount / dualPetCount;
        int extraLimit = actualPetCount % dualPetCount;
        int limitSum = (revratio+1)*extraLimit;
        if (localPet >= limitSum){
          int index = extraLimit + (localPet - limitSum);
          localRecvFromPet = dualPetList[index];
        }else{
          int index = localPet / (revratio+1);
          localRecvFromPet = dualPetList[index];          
        }
      }
    }
        
//printf("On interLocalPet %d the localSendToPetCount = %d, and the localRecvFromPet = %d\n", interLocalPet, localSendToPetList.size(), localRecvFromPet);
    
    // garbage collection
    delete [] petList;
    delete [] dualPetList;
    delete [] actualPetList;
    
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
    
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::CompTunnel::dual2actual()"
  int CompTunnel::dual2actual(void *msg, int len){
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

    // carry out message transfer according to the tunnel comm pattern
    if (isDual){
      // this is a dual component side -> sender
      for (int i=0; i<localSendToPetList.size(); i++)
        bridgeVM->send(msg, len, localSendToPetList[i], tag);
    }else{
      // this is the actual component side -> receiver
      bridgeVM->recv(msg, len, localRecvFromPet, tag);
    }
    
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::CompTunnel::actual2dual()"
  int CompTunnel::actual2dual(void *msg, int len){
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

    // carry out message transfer according to the tunnel comm pattern
    if (isActual){
      // this is a actual component side -> sender
      for (int i=0; i<localSendToPetList.size(); i++)
        bridgeVM->send(msg, len, localSendToPetList[i], tag);
    }else{
      // this is the dual component side -> receiver
      bridgeVM->recv(msg, len, localRecvFromPet, tag);
    }
    
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
    VM *bridgeVM;
    localrc = comp->getVmParent(&bridgeVM);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, rc))
      return; // bail out
   
    CompTunnel compTunnel(bridgeVM);  // local compTunnel object

    // negotiation phase
    compTunnel.setActual(comp);       // set this component as the actual side
    localrc = compTunnel.negotiate();
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, rc))
      return; // bail out

    // temporary return code buffer    
    int rcs[2];
    
    // execution loop phase
    for (;;){
    
      // receive method info from dual component
      enum method method;
      localrc = compTunnel.dual2actual(&method, sizeof(enum method));
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, rc))
        return; // bail out

      // receive phase info from dual component
      int phase;      
      localrc = compTunnel.dual2actual(&phase, sizeof(int));
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, rc))
        return; // bail out
      
//printf("ServiceLoop has received a method through dual2actual: %d\n", method);

      // break condition
      if (method == METHOD_NONE) break;
      
      // execute the method indicated by what was received from the dual comp
      localrc = comp->execute(method, importState, exportState,
        *clock, ESMF_VASBLOCKING, phase, &userRc);
      
      // log the local error code, but do not bail out since dual component
      // remains in control
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, NULL);
      
      // send the return codes to the controlling dual component
      rcs[0] = localrc;
      rcs[1] = userRc;
      localrc = compTunnel.actual2dual(rcs, 2*sizeof(int));
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, NULL);
      
    }

    // dual component to know actual component successfully exited service loop
    rcs[0] = ESMF_SUCCESS;
    rcs[1] = ESMF_SUCCESS;
    localrc = compTunnel.actual2dual(rcs, 2*sizeof(int));    
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, NULL);
        
ESMC_LogDefault.Write("ServiceLoop: exiting", ESMC_LOG_INFO);
    
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }
} // namespace ESMCI
  

