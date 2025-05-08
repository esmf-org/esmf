// $Id$
//
// Earth System Modeling Framework
// Copyright (c) 2002-2025, University Corporation for Atmospheric Research,
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

#ifndef ESMCI_VM_H
#define ESMCI_VM_H

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::VM - VM (virtual machine)
//
// !DESCRIPTION:
//
// The code in this file defines two C++ classes {\tt ESMCI::VM} and 
// {\tt ESMCI::VMPlan}, their members and method signatures (prototypes).
// The companion file {\tt ESMCI\_VM.C} contains the full code (bodies) 
// for the methods.
//
// Currently class {\tt ESMCI::VM} is derived from base class {\tt ESMCI::VMK}
// and {\tt ESMCI::VMPlan} is derived from base class {\tt ESMCI::VMKPlan}.
// There are only very few new features that the derived classes add to their
// base classes, thus most of the implementing code is located in 
// {\tt ESMCI_VMKernel.C}.
//
//EOPI
//-------------------------------------------------------------------------

#include "ESMCI_VMKernel.h"    // inherit from ESMCI::VMK class
#include "ESMCI_Util.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_F90Interface.h"

#include <string>
#include <map>

//-------------------------------------------------------------------------

namespace ESMCI {

class VMId {
  public:
  unsigned char *vmKey;   // bit-pattern that identifies VM VAS context
  int localID;            // local ID of the VM within VAS context

  public:
  VMId() { vmKey=NULL; localID=0; }

  int create ();      // allocates memory for vmKey member
  int destroy ();     // frees memory for vmKey member
  int get(int *localID, char *key, int key_len);
  int getLeftmostOnBit(int *leftmostOnBit);
  int set(int  localID, const char *key, int key_len);
  int serialize(const char *buffer, int *length, int *offset,
                const ESMC_InquireFlag &inquireflag);
  int deserialize(const char *buffer, int *offset, bool offsetonly);
  void log(std::string prefix,
    ESMC_LogMsgType_Flag msgType=ESMC_LOGMSG_INFO) const;
  int print () const;
};

} // namespace ESMCI

//-------------------------------------------------------------------------

#include "ESMCI_Base.h"        // cannot move Base.h before def. of ESMCI::VMId 

//-------------------------------------------------------------------------

namespace ESMCI {

// ESMCI::VMId methods:
bool VMIdCompare(const VMId *vmID1, const VMId *vmID2, bool keyOnly=false,
  bool keySuper=false);
bool VMIdIsLocalPetActive(const VMId *vmID);
bool VMIdLessThan(const VMId *vmID1, const VMId *vmID2);
int VMIdCopy(VMId *vmIDdst, VMId *vmIDsrc);
} // namespace ESMCI


//-------------------------------------------------------------------------


namespace ESMCI {

// classes

class VM;
class VMPlan;

class VMTimer {
  double t0;
  double taccu;
  unsigned long iters;
  friend class VM;
};

// class definition
class VM : public VMK {   // inherits from ESMCI::VMK class
  // This is the ESMF derived virtual machine class.
    // performance timers
    std::map<std::string, VMTimer> timers;
  public:
    // initialize(), finalize() and abort() of global VM
    static VM *initialize(MPI_Comm mpiCommunicator, bool globalResourceControl,
      int *rc);
    static void set(bool globalResourceControl, int *rc);
    static void finalize(ESMC_Logical *keepMpiFlag, int *rc);
    static void abort(int *rc);
    static bool isInitialized(int *rc);
    static bool isFinalized(int *rc);
    // life cycle methods      
    void *startup(class VMPlan *vmp, void *(fctp)(void *, void *), void *cargo,
      int *rc);
    int enter(class VMPlan *vmp, void *info, void *cargo);
    void shutdown(class VMPlan *vmp, void *info, int *rc);
    // get()
    int getPETMatchPET(int pet, VM &vmMatch, int *petMatchCount,
      int *petMatchList, int len_petMatchList);
    VMId *getVMId(int *rc) const;   // Return VMId of the VM context.
    static void getArgs(int *argc, char ***argv, int *rc);  // command line args
    static VM *getGlobal(int *rc=NULL);       // global VM
    static VM *getCurrent(int *rc=NULL);      // current VM
    static bool isThreadKnown(int *rc=NULL);  // is thread known under any VM
    static VMId *getCurrentID(int *rc=NULL);  // VMId of current VM
    static void getCurrentGarbageInfo(int *, int *); // garbage info current VM
    static void logGarbageInfo(std::string prefix, bool current=false,
      ESMC_LogMsgType_Flag msgType=ESMC_LOGMSG_INFO); // garbage log current VM
    static void getMemInfo(int *virtMemPet, int *physMemPet);   // memory info
    static void logMemInfo(std::string prefix,
      ESMC_LogMsgType_Flag msgType=ESMC_LOGMSG_INFO,
      ESMCI::LogErr *log=&ESMC_LogDefault);   // memory log
    static void logBacktrace(std::string prefix,
      ESMC_LogMsgType_Flag msgType=ESMC_LOGMSG_INFO); // backtrace log
    static int getBaseIDAndInc(VMId *vmID);
    static void addObject(ESMC_Base *, VMId *vmID);
    static void rmObject(ESMC_Base *);
    static void addFObject(void **fobject, int objectID, VMId *vmID);
    static void rmFObject(void **fobject);
    static bool validObject(ESMC_Base *);
    static char const *getenv(char const *name);
    static void setenv(char const *name, char const *value);
    // misc.
    int print() const;
    int validate() const;
    int sendVMId(VMId *vmid, int dest);
    int recvVMId(VMId *vmid, int source);
    int bcastVMId(VMId **vmid, int count, int root);
    int translateVMId(VMId **vmids, ESMCI::InterArray<int> *ids,
      ESMCI::InterArray<int> *rootVmIds, int *rootVmIdCount);
    int allgathervVMId(VMId **sendvmid, int  sendcount,
      VMId **recvvmid, int *recvcounts, int *recvoffsets);
    int alltoallvVMId(VMId **sendvmid, int *sendcounts, int *sendoffsets,
      VMId **recvvmid, int *recvcounts, int *recvoffsets);
    // MPI error handler
    static bool MPIError(int mpiErrorToCheck,
      int LINE, const std::string &FILE, const std::string &method,
      int *rcToReturn=NULL);
    // performance timers API
    void timerReset(std::string timer){
      std::map<std::string, VMTimer>::iterator t = timers.find(timer);
      if (t==timers.end()){
        // create the new timer
        timers[timer].taccu = 0.;   // set to zero
        timers[timer].iters = 0;    // set to zero
      }else{
        // reset the timer
        t->second.taccu = 0.; // reset to zero
        t->second.iters = 0;  // reset to zero
      }
    }
    void timerStart(std::string timer){
      std::map<std::string, VMTimer>::iterator t = timers.find(timer);
      wtime(&(t->second.t0));   // start time
    }
    void timerStop(std::string timer){
      double t1;
      wtime(&t1);   // stop time
      std::map<std::string, VMTimer>::iterator t = timers.find(timer);
      t->second.taccu += t1 - t->second.t0;
      ++(t->second.iters);
    }
    void timerLog(std::string timer,
      ESMC_LogMsgType_Flag msgType=ESMC_LOGMSG_INFO);
};  // class VM


// class definition
class VMPlan : public VMKPlan {   // inherits from ESMCI::VMKPlan

  public:
    VMPlan(int _ndevlist=0, int *_devlist=NULL) : VMKPlan(_ndevlist,_devlist){}
    int nspawn;     // number of PETs this PET will spawn
    VM **myvms;     // pointer array of VM instances for this PET
    VMK **myvmachs; // pointer array of VMK instances for this PET
};  // class VMPlan

} // namespace ESMCI

#endif  // ESMCI_VM_H
