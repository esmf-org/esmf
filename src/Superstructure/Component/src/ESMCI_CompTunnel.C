// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research, 
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

#include <time.h>

// socket specific headers
#ifdef ESMF_NO_SOCKETS
#define IPPORT_RESERVED 1024
#define IPPORT_USERRESERVED 5000
#elif ESMF_OS_MinGW
#include <Winsock.h>
#else
#include <netinet/in.h>
#endif

// include ESMF headers
#include "ESMCI_Macros.h"
#include "ESMCI_VMKernel.h"
#include "ESMCI_LogErr.h"

//==============================================================================
//==============================================================================
// CompTunnel class implementation
//==============================================================================
//==============================================================================

extern "C" {

  // Fortran access point to native class destructor
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_comptunneldestroy"
  void FTN_X(c_esmc_comptunneldestroy)(ESMCI::CompTunnel **ptr, int *rc){
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    if (*ptr == NULL){
      ESMC_LogDefault.MsgAllocError("- CompTunnel deallocation", ESMC_CONTEXT,
        rc);  
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
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return; // bail out
    // call into setServices for this compTunnel object
    localrc = compTunnel->setServices(dualComp);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return; // bail out
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

  
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::CompTunnel::setServices()"
  int CompTunnel::setServices(Comp *dualComp){
    // This method is executed on the dualComp VM
    int rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    if (vmBased){
      // use the parent VM as bridge during the negotiation    
      localrc = dualComp->getVmParent(&bridgeVM);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc)) return rc;
    }
    
    // negotiation phase
    setDual(dualComp);  // set this side as the dual side
    localrc = negotiate(timeout);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    
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

      if (outstandingWaitFlag){
        // only call into wait() if there is an outstanding interaction
printf("in CompTunnel::execute() ... call CompTunnel::wait really calling\n");
        localrc = wait(cargo);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc;  // bail out
        outstandingWaitFlag = false;  // reset the flag
      }

    }else if ((vmBased&&bridgeVM)||socketBased){
      
printf("in CompTunnel::execute() ... call CompTunnel::dual2actual() with method %d\n", method);

      // first test whether this is an outstanding connection that needs wait
      if (outstandingWaitFlag){
        if (method==METHOD_NONE){
          // for wrapup call it is o.k. to just swallow the return codes
          localrc = wait(cargo);
          if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            ESMC_CONTEXT, &rc)) return rc;  // bail out
          outstandingWaitFlag = false;  // reset the flag
        }else{
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
            "- An outstanding connection first requires a CompTunnel::wait()!",
            ESMC_CONTEXT, &rc);
          return rc;  // bail out
        }
      }

      // send method info to actual component
      localrc = dual2actual(&method, sizeof(enum method), cargo->timeout);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc))
        return rc;  // bail out

      // send phase info to actual component
      localrc = dual2actual(&phase, sizeof(int), cargo->timeout);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc)) return rc;  // bail out
      
      // indicate that there is an outstanding connection that needs a wait
      outstandingWaitFlag = true;   // set the flag
      
    }else if (localActualComp){
      // the actual component is local
      //TODO: NOT SURE THIS WORKS ANYMORE!!!
      localActualCompCargo = *cargo;   // copy dual component's cargo structure
      localActualCompCargo.f90comp = localActualComp;  // overwrite f90comp
      
      ESMCI::FTable *ftable;
      localrc = localActualComp->getFTable(&ftable);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc)) return rc;  // bail out
      localActualCompCargo.ftable = ftable;
      
      ESMCI::VM *vm_parent;
      localrc = localActualComp->getVmParent(&vm_parent);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc)) return rc;  // bail out
      ESMCI::VMPlan *vmplan;
      localrc = localActualComp->getVmPlan(&vmplan);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc)) return rc;  // bail out
      void *vm_info;
      localrc = localActualComp->getVmInfo(&vm_info);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc)) return rc;  // bail out

printf("now calling into vm_parent->enter() from CompTunnel::execute()\n");

      //TODO: before calling into the actual component here make sure that
      //TODO: the arguments have been set up correctly for the callback!!!
      
      // enter the child VM -> resurface in ESMCI_FTableCallEntryPointVMHop()
      localrc = vm_parent->enter(vmplan, vm_info, (void*)&localActualCompCargo);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc)) return rc; // bail out
            
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
    if ((vmBased&&bridgeVM)||socketBased){
      
printf("in CompTunnel::wait() ... \n");

      int rcs[2];
      localrc = actual2dual(rcs, 2*sizeof(int), cargo->timeout);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc)) return rc;
      
printf("in CompTunnel::wait() ... received RC=%d, userRC=%d\n", rcs[0], rcs[1]);
      
      // pass the actual return codes back through dual cargo
      cargo->esmfrc[0] = rcs[0];
      cargo->userrc[0] = rcs[1];
      
    }else if (localActualComp){
      // the actual component is local
      //TODO: NOT SURE THIS WORKS ANYMORE!!!
      ESMCI::VM *vm_parent;
      localrc = localActualComp->getVmParent(&vm_parent);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc)) return rc;
      ESMCI::VMPlan *vmplan;
      localrc = localActualComp->getVmPlan(&vmplan);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc)) return rc;
      void *vm_info;
      localrc = localActualComp->getVmInfo(&vm_info);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc)) return rc;

printf("now calling into vm_parent->exit() from CompTunnel::wait()\n");
      
      // Now call the vmk_exit function which will block respective PETs
      vm_parent->exit(static_cast<ESMCI::VMKPlan *>(vmplan), vm_info);

    }
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
  
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::CompTunnel::chat()"
  void CompTunnel::chat(char *buffer){
    if (strstr(buffer,"version")==buffer){
      sprintf(buffer, "ESMF_VERSION_STRING: %s\n", ESMF_VERSION_STRING);
    }else if (strstr(buffer,"date")==buffer){
      time_t tp;
      time(&tp);
      sprintf(buffer, "%s", ctime(&tp));
    }else if (strstr(buffer,"component")==buffer){
      sprintf(buffer, "TODO: this should print name of the actual component\n");
    }else
      sprintf(buffer, "I have a hard time understanding you :-(\n");
  }
  
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::CompTunnel::negotiate()"
  int CompTunnel::negotiate(double timeout){
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
        ESMC_CONTEXT, &rc);
      return rc;
    }

    char logmsg[80];
    
    // get the Component's VM
    VM *vm;
    localrc = comp->getVm(&vm);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, &rc)) return rc; // bail out
    localPet = vm->getLocalPet();             // localPet in terms of VM

    if (socketBased){
      // socket based component tunnel
      if (isActual){
        // actual: set up server side of the socket based component tunnel
        if (localPet == 0){
          // master PET on actual side -> initialize server side
          sprintf(logmsg, "negotiate socketBased actual: port=%d, timeout=%g",
            port, timeout);
          ESMC_LogDefault.Write(logmsg, ESMC_LOGMSG_INFO, ESMC_CONTEXT);
          char buffer[160];
          int len;
          bool continueFlag = false;
          while (!continueFlag){
            sock = socketServerInit(port, timeout);
            if (sock <= SOCKERR_UNSPEC){
              if (sock == SOCKERR_TIMEOUT){
                ESMC_LogDefault.MsgFoundError(ESMC_RC_TIMEOUT,
                  "- No client connected, time out!", ESMC_CONTEXT, &rc);
                vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
                return rc;
              }else{
                ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, 
                  "- Unspecified error setting up socket server side!",
                  ESMC_CONTEXT, &rc);
                vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
                return rc;
              }
            }
            // send greeting
            sprintf(buffer, "Hello from ESMF Actual Component server!\n");
            len = strlen(buffer);
            int sLen = socketSend(sock, buffer, len, timeout);
            if (sLen <= SOCKERR_UNSPEC){
              if (sLen == SOCKERR_TIMEOUT){
                ESMC_LogDefault.MsgFoundError(ESMC_RC_TIMEOUT,
                  "- Client not there to receive, time out!", ESMC_CONTEXT,
                  &rc);
                vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
                return rc;
              }else{
                ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, 
                  "- Unspecified error sending to client!", ESMC_CONTEXT,
                  &rc);
                vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
                return rc;
              }
            }
            for(;;){
              len = socketRecv(sock, buffer, 160, timeout);
              if (len <= SOCKERR_UNSPEC){
                if (len == SOCKERR_TIMEOUT){
                  ESMC_LogDefault.MsgFoundError(ESMC_RC_TIMEOUT,
                    "- Client not sending, time out!", ESMC_CONTEXT, &rc);
                  vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
                  return rc;
                }else{
                  ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, 
                    "- Unspecified error receiving from client!", ESMC_CONTEXT,
                    &rc);
                  vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
                  return rc;
                }
              }
              if (strstr(buffer,"continue")==buffer){
                continueFlag = true;
                sprintf(buffer, "Actual Component server will continue now!\n");
                break;
              }
              if (strstr(buffer,"reconnect")==buffer){
                sprintf(buffer, "Actual Component server will reconnect "
                "now!\n");
                break;
              }
              chat(buffer);   // chat by filling the buffer with an answer
              len = strlen(buffer);
              sLen = socketSend(sock, buffer, len, timeout);
              if (sLen <= SOCKERR_UNSPEC){
                if (sLen == SOCKERR_TIMEOUT){
                  ESMC_LogDefault.MsgFoundError(ESMC_RC_TIMEOUT,
                    "- Client not there to receive, time out!", ESMC_CONTEXT,
                    &rc);
                  vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
                  return rc;
                }else{
                  ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, 
                    "- Unspecified error sending to client!", ESMC_CONTEXT,
                    &rc);
                  vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
                  return rc;
                }
              }
            }
            // send handshake (needed to wrap up phase of flex size messages)
            len = strlen(buffer);
            sLen = socketSend(sock, buffer, len, timeout);
            if (sLen <= SOCKERR_UNSPEC){
              if (sLen == SOCKERR_TIMEOUT){
                ESMC_LogDefault.MsgFoundError(ESMC_RC_TIMEOUT,
                  "- Client not there to receive, time out!", ESMC_CONTEXT,
                  &rc);
                vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
                return rc;
              }else{
                ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, 
                  "- Unspecified error sending to client!", ESMC_CONTEXT, &rc);
                vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
                return rc;
              }
            }
            if (!continueFlag){
              sLen = socketFinal(sock, timeout);
              if (sLen <= SOCKERR_UNSPEC){
                if (sLen == SOCKERR_TIMEOUT){
                  ESMC_LogDefault.MsgFoundError(ESMC_RC_TIMEOUT,
                    "- Client not there to wrap up connection, time out!",
                    ESMC_CONTEXT, &rc);
                  vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
                  return rc;
                }else{
                  ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, 
                    "- Unspecified error wrapping up connection!", ESMC_CONTEXT,
                    &rc);
                  vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
                  return rc;
                }
              }
            }
          }
          masterFlag = true;  // this PET is the master on the dual side
          rc = ESMF_SUCCESS;
          vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
        }else{
          // the other PETs on actual side
          masterFlag = false;   // this PET is not the master on the dual side
          vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
          if (rc != ESMF_SUCCESS)
            return rc;
        }
        ESMC_LogDefault.Write("finished socketBased negotiate() on actual "
          "component side", ESMC_LOGMSG_INFO, ESMC_CONTEXT);
      }else{
        // dual: set up client side of the socket based component tunnel
        if (localPet == 0){
          // master PET on dual side -> initialize client side
          sprintf(logmsg, "negotiate socketBased dual: port=%d, timeout=%g",
            port, timeout);
          ESMC_LogDefault.Write(logmsg, ESMC_LOGMSG_INFO, ESMC_CONTEXT);
          sock = socketClientInit(server.c_str(), port, timeout);
          if (sock <= SOCKERR_UNSPEC){
            if (sock == SOCKERR_TIMEOUT){
              ESMC_LogDefault.MsgFoundError(ESMC_RC_TIMEOUT, 
                "- No server to connect to, time out!", ESMC_CONTEXT, &rc);
              vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
              return rc;
            }else{
              ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, 
                "- Unspecified error setting up socket client side!",
                ESMC_CONTEXT, &rc);
              vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
              return rc;
            }
          }
          // wait for greeting from actual side server
          char buffer[160];
          int len;
          bool helloFlag = false;
          while(!helloFlag){
            len = socketRecv(sock, buffer, 160, timeout);
            if (len <= SOCKERR_UNSPEC){
              if (len == SOCKERR_TIMEOUT){
                ESMC_LogDefault.MsgFoundError(ESMC_RC_TIMEOUT,
                  "- Server not sending, time out!", ESMC_CONTEXT, &rc);
                vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
                return rc;
              }else{
                ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, 
                  "- Unspecified error receiving from server!", ESMC_CONTEXT,
                  &rc);
                vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
                return rc;
              }
            }
            if (strstr(buffer,
              "Hello from ESMF Actual Component server!")==buffer){
              helloFlag = true;
              break;
            }
          }
          // send continue command to the actual side server
          sprintf(buffer, "continue\n");
          len = strlen(buffer);
          int sLen = socketSend(sock, buffer, len, timeout);
          if (sLen <= SOCKERR_UNSPEC){
            if (sLen == SOCKERR_TIMEOUT){
              ESMC_LogDefault.MsgFoundError(ESMC_RC_TIMEOUT,
                "- Server not there to receive, time out!", ESMC_CONTEXT, &rc);
              vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
              return rc;
            }else{
              ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, 
                "- Unspecified error sending to server!", ESMC_CONTEXT, &rc);
              vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
              return rc;
            }
          }
          // ensure the continue handshake is received
          len = socketRecv(sock, buffer, 160, timeout);
          if (len <= SOCKERR_UNSPEC){
            if (len == SOCKERR_TIMEOUT){
              ESMC_LogDefault.MsgFoundError(ESMC_RC_TIMEOUT,
                "- Server not sending, time out!", ESMC_CONTEXT, &rc);
              vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
              return rc;
            }else{
              ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, 
                "- Unspecified error receiving from server!", ESMC_CONTEXT,
                &rc);
              vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
              return rc;
            }
          }
          if (strstr(buffer,
            "Actual Component server will continue now!")!=buffer){
            ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, 
              "- Did not received expected continue handshake!", ESMC_CONTEXT,
              &rc);
            vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
            return rc;
          }
          masterFlag = true;  // this PET is the master on the dual side
          rc = ESMF_SUCCESS;
          vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
        }else{
          // the other PETs on dual side
          masterFlag = false;   // this PET is not the master on the dual side
          vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
          if (rc != ESMF_SUCCESS)
            return rc;
        }
        ESMC_LogDefault.Write("finished socketBased negotiate() on dual "
          "component side", ESMC_LOGMSG_INFO, ESMC_CONTEXT);
      }
      // return successfully
      rc = ESMF_SUCCESS;
      return rc;
    }
    
    // continue with not socket based tunnels, i.e. vmBased...
    
    // every PET to determine which unique id it holds in the inter-VM
    //TODO: Of course this does not work if a child VM has multiple PETs
    //TODO: spawned from a single parent VM PET. Things get more involved
    //TODO: in that case.
    interLocalPet = bridgeVM->getLocalPet();  // localPet in terms of bridgeVM
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
          int index = extraLimit + (localPet - limitSum) / revratio;
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
          int index = extraLimit + (localPet - limitSum) / revratio;
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
  int CompTunnel::dual2actual(void *msg, int len, double timeout){
    // For now it is assumed that msg has the same content on all PETs!!!!
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
        ESMC_CONTEXT, &rc);
      return rc;
    }

    char logmsg[80];

    if (vmBased){
      // message transfer according to the vmBased tunnel comm pattern
      if (isDual){
        // this is a dual component side -> sender
        ESMC_LogDefault.Write("dual2actual() vmBased (dual side)...",
          ESMC_LOGMSG_INFO, ESMC_CONTEXT);
        for (int i=0; i<localSendToPetList.size(); i++)
          bridgeVM->send(msg, len, localSendToPetList[i], tag);
      }else{
        // this is the actual component side -> receiver
        ESMC_LogDefault.Write("dual2actual() vmBased (actual side)...",
          ESMC_LOGMSG_INFO, ESMC_CONTEXT);
        bridgeVM->recv(msg, len, localRecvFromPet, tag);
      }
    }else if (socketBased){
      // message transfer according to the socketBased tunnel comm pattern
      VM *vm;
      localrc = comp->getVm(&vm);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc)) return rc; // bail out
      if (isDual){
        // this is a dual component side -> sender
        ESMC_LogDefault.Write("dual2actual() socketBased (dual side)...",
          ESMC_LOGMSG_INFO, ESMC_CONTEXT);
        // only master sends through socket
        if (masterFlag){
          int sLen = socketSend(sock, msg, len, timeout);
          if (sLen <= SOCKERR_UNSPEC){
            if (sLen == SOCKERR_TIMEOUT){
              sprintf(logmsg, "- socketBased dual side timed out on "
                " socketSend() with timeout=%g", timeout);
              ESMC_LogDefault.MsgFoundError(ESMC_RC_TIMEOUT, logmsg,
                ESMC_CONTEXT, &rc);
              vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
              return rc;
            }else{
              ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, 
                "- socketBased dual side failed unspecified in socketSend()",
                ESMC_CONTEXT, &rc);
              vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
              return rc;
            }
          }
          rc = ESMF_SUCCESS;
          vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
        }else{
          // the other PETs on dual side
          vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
          if (rc != ESMF_SUCCESS)
            return rc;
        }
      }else{
        // this is the actual component side -> receiver
        ESMC_LogDefault.Write("dual2actual() socketBased (actual side)...",
          ESMC_LOGMSG_INFO, ESMC_CONTEXT);
        // only master receives through socket and then broadcasts
        if (masterFlag){
          int rLen = socketRecv(sock, msg, len, timeout);
          if (rLen <= SOCKERR_UNSPEC){
            if (rLen == SOCKERR_TIMEOUT){
              sprintf(logmsg, "- socketBased actual side timed out on"
                " socketRecv() with timeout=%g", timeout);
              ESMC_LogDefault.MsgFoundError(ESMC_RC_TIMEOUT, logmsg,
                ESMC_CONTEXT, &rc);
              vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
              return rc;
            }else{
              ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, 
                "- socketBased actual side failed unspecified in socketRecv()",
                ESMC_CONTEXT, &rc);
              vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
              return rc;
            }
          }
          rc = ESMF_SUCCESS;
          vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
        }else{
          // the other PETs on actual side
          vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
          if (rc != ESMF_SUCCESS)
            return rc;
        }
        localrc = vm->broadcast(msg, len, 0); // localPet 0 is master
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc; // bail out
      }
    }else{
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
        "Reached an invalid branch in the code.", ESMC_CONTEXT, &rc);
      return rc; // bail out
    }

    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::CompTunnel::actual2dual()"
  int CompTunnel::actual2dual(void *msg, int len, double timeout){
    // For now it is assumed that msg has the same content on all PETs!!!!
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
        ESMC_CONTEXT, &rc);
      return rc;
    }
    
    if (vmBased){
      // carry out message transfer according to the tunnel comm pattern
      if (isActual){
        // this is a actual component side -> sender
        ESMC_LogDefault.Write("actual2dual() vmBased (actual side)...",
          ESMC_LOGMSG_INFO, ESMC_CONTEXT);
        for (int i=0; i<localSendToPetList.size(); i++)
          bridgeVM->send(msg, len, localSendToPetList[i], tag);
      }else{
        // this is the dual component side -> receiver
        ESMC_LogDefault.Write("actual2dual() vmBased (dual side)...",
          ESMC_LOGMSG_INFO, ESMC_CONTEXT);
        bridgeVM->recv(msg, len, localRecvFromPet, tag);
      }
    }else if (socketBased){
      char logmsg[80];
      // message transfer according to the socketBased tunnel comm pattern
      VM *vm;
      localrc = comp->getVm(&vm);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc)) return rc; // bail out
      if (isActual){
        // this is a actual component side -> sender
        ESMC_LogDefault.Write("actual2dual() socketBased (actual side)...",
          ESMC_LOGMSG_INFO, ESMC_CONTEXT);
        // only master sends through socket
        if (masterFlag){
          int sLen = socketSend(sock, msg, len, timeout);
          if (sLen <= SOCKERR_UNSPEC){
            if (sLen == SOCKERR_TIMEOUT){
              sprintf(logmsg, "- socketBased actual side timed out on"
                " socketSend() with timeout=%g", timeout);
              ESMC_LogDefault.MsgFoundError(ESMC_RC_TIMEOUT, logmsg,
                ESMC_CONTEXT, &rc);
              vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
              return rc;
            }else{
              ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, 
                "- socketBased actual side failed unspecified in socketSend()",
                ESMC_CONTEXT, &rc);
              vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
              return rc;
            }
          }
          rc = ESMF_SUCCESS;
          vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
        }else{
          // the other PETs on actual side
          vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
          if (rc != ESMF_SUCCESS)
            return rc;
        }
      }else{
        // this is the dual component side -> receiver
        ESMC_LogDefault.Write("actual2dual() socketBased (dual side)...", 
          ESMC_LOGMSG_INFO, ESMC_CONTEXT);
        // only master receives through socket and then broadcasts
        if (masterFlag){
          int rLen = socketRecv(sock, msg, len, timeout);
          if (rLen <= SOCKERR_UNSPEC){
            if (rLen == SOCKERR_TIMEOUT){
              sprintf(logmsg, "- socketBased dual side timed out on"
                " socketRecv() with timeout=%g", timeout);
              ESMC_LogDefault.MsgFoundError(ESMC_RC_TIMEOUT, logmsg,
                ESMC_CONTEXT, &rc);
              vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
              return rc;
            }else{
              ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, 
                "- socketBased dual side failed unspecified in socketRecv()",
                ESMC_CONTEXT, &rc);
              vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
              return rc;
            }
          }
          rc = ESMF_SUCCESS;
          vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
        }else{
          // the other PETs on dual side
          vm->broadcast(&rc, sizeof(int), 0); // localPet 0 is master
          if (rc != ESMF_SUCCESS)
            return rc;
        }
        localrc = vm->broadcast(msg, len, 0); // localPet 0 is master
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc))
          return rc; // bail out
      }
    }else{
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
        "Reached an invalid branch in the code.", ESMC_CONTEXT, &rc);
      return rc; // bail out
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

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ServiceLoop()"
namespace ESMCI {
  void ServiceLoop(Comp *comp, State *importState, State *exportState, 
    Clock **clock, int *rc){
    // This method is executed on the actual component VM
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    int userRc;
    char logmsg[80];

    int port;
    localrc = comp->getCurrentPhase(&port);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return; // bail out
    
    int timeout;
    localrc = comp->getTimeout(&timeout);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return; // bail out

    sprintf(logmsg, "ServiceLoop: entering with port argument: %d and timeout: "
      "%d", port, timeout);
    ESMC_LogDefault.Write(logmsg, ESMC_LOGMSG_INFO, ESMC_CONTEXT);
    
    // determine whether to use socket or VM based tunnel; if socket set port
    bool socketBasedTunnel = true;
    if (port == -1)
      socketBasedTunnel = false; // use VM based tunnel inside single executable
    else{
      if (port < IPPORT_RESERVED){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
          "port must be >= IPPORT_RESERVED", ESMC_CONTEXT, rc);
      }
    }
    bool vmBasedTunnel = !socketBasedTunnel;  // for now simply the opposite
    //TODO: consider also webServices and MPI-2 based tunnels

    CompTunnel compTunnel;  // local terminal point of the component tunnel
    
    if (vmBasedTunnel){
      //TODO: for now simply use the parent VM, may not be safe, should change!!
      VM *bridgeVM;
      localrc = comp->getVmParent(&bridgeVM);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc))
        return; // bail out
   
      compTunnel = CompTunnel(bridgeVM);
      
    }else if (socketBasedTunnel){
      
      compTunnel = CompTunnel(port);
      
    }

    // negotiation phase
    compTunnel.setActual(comp);       // set this component as the actual side
    localrc = compTunnel.negotiate(timeout);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return; // bail out

    // temporary return code buffer    
    int rcs[2];
    
    // execution loop phase
    for (;;){
    
      // receive method info from dual component
      enum method method;
      localrc = compTunnel.dual2actual(&method, sizeof(enum method), timeout);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc)) return; // bail out

      // receive phase info from dual component
      int phase;      
      localrc = compTunnel.dual2actual(&phase, sizeof(int), timeout);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc)) return; // bail out
      
      sprintf(logmsg, "ServiceLoop: received method: %s and phase: %d", 
        FTable::methodString(method), phase);
      ESMC_LogDefault.Write(logmsg, ESMC_LOGMSG_INFO, ESMC_CONTEXT);

      // break condition
      if (method == METHOD_NONE) break;
      
      // execute the method indicated by what was received from the dual comp
      localrc = comp->execute(method, importState, exportState,
        *clock, ESMF_VASBLOCKING, phase, 0, &userRc);
      
      // log the local error code, but do not bail out since dual component
      // remains in control
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        NULL);
      
      // send the return codes to the controlling dual component
      rcs[0] = localrc;
      rcs[1] = userRc;
      localrc = compTunnel.actual2dual(rcs, 2*sizeof(int), timeout);
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        NULL);
      
    }

    // dual component to know actual component successfully exited service loop
    rcs[0] = ESMF_SUCCESS;
    rcs[1] = ESMF_SUCCESS;
    localrc = compTunnel.actual2dual(rcs, 2*sizeof(int), timeout);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      NULL);
        
    ESMC_LogDefault.Write("ServiceLoop: exiting", ESMC_LOGMSG_INFO,
      ESMC_CONTEXT);
    
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }
} // namespace ESMCI
  

