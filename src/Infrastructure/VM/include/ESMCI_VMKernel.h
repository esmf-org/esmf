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
//-----------------------------------------------------------------------------

#ifndef ESMCI_VMKERNEL_H
#define ESMCI_VMKERNEL_H

#ifndef MPICH_IGNORE_CXX_SEEK
#define MPICH_IGNORE_CXX_SEEK
#endif

#define USE_STRSTREAM

#include <mpi.h>
#include <vector>
#include <string>
#include <sstream>
#ifdef USE_STRSTREAM
#include <strstream>
#endif
#include <map>

#ifdef VMK_STANDALONE
#include <pthread.h>
typedef pthread_mutex_t esmf_pthread_mutex_t;
typedef pthread_cond_t  esmf_pthread_cond_t;
typedef pthread_t       esmf_pthread_t;
#else
#include "ESMF_Pthread.h"
#endif

// define NULL
#include <cstddef> 

#include "ESMCI_LogErr.h"

// reduction operations
enum vmOp   { vmSUM=1, vmMIN, vmMAX};
// typekind indicators
//   Note: vmL4 indicates Fortran logicals, not c++ bools
enum vmType { vmBYTE=1, vmI4, vmI8, vmR4, vmR8, vmL4};
// epochs
enum vmEpoch  { epochNone=0, epochBuffer};

// Pthread stack sizes
#define VM_PTHREAD_STACKSIZE_SERVICE  (4194304) //  4MiB for service threads
#define VM_PTHREAD_STACKSIZE_USER    (20971520) // 20MiB for user threads

// VM_ANY_SOURCE and VM_ANY_TAG
#define VM_ANY_SRC                    (-2)
#define VM_ANY_TAG                    (-2)

// define the communication preferences
#define PREF_INTRA_PROCESS_SHMHACK    (0)       // default
#define PREF_INTRA_PROCESS_PTHREAD    (1)

#define PREF_INTRA_SSI_POSIXIPC       (0)
#define PREF_INTRA_SSI_MPI1           (1)       // default

#define PREF_INTER_SSI_MPI1           (0)       // default

// - communication identifiers
#define VM_COMM_TYPE_MPIUNI   (-1)
#define VM_COMM_TYPE_MPI1     (0)
#define VM_COMM_TYPE_PTHREAD  (1)
#define VM_COMM_TYPE_SHMHACK  (2)
#define VM_COMM_TYPE_POSIXIPC (3)

// - VMKernel error code
#define VMK_ERROR             (-1)

// - buffer lenghts in bytes
#define PIPC_BUFFER                   (4096)
#define SHARED_BUFFER                 (64)

// - number of shared memory non-blocking channels
#define SHARED_NONBLOCK_CHANNELS      (16)

// begin sync stuff -----
#define SYNC_NBUFFERS                 (2)
typedef struct{
  // hack sync variables
  // The sole purpose of the last index on all of these variables is to provide
  // a tuning parameter! Changing the extend of this dimension will modify 
  // whether a variable takes the space of a whole cache line or not.
  volatile int a_done[1];
  volatile int b_done[1];
  volatile int buffer_done[SYNC_NBUFFERS][1];
}shmsync;  

void sync_a_flip(shmsync *shms);
void sync_a_flop(shmsync *shms);
void sync_b_flip(shmsync *shms);
void sync_b_flop(shmsync *shms);
void sync_buffer_flag_fill(shmsync *shms, int select);
void sync_buffer_flag_empty(shmsync *shms, int select);
void sync_buffer_wait_fill(shmsync *shms, int select);
void sync_buffer_wait_empty(shmsync *shms, int select);
void sync_reset(shmsync *shms);
// end sync stuff -----


namespace ESMCI {


//-----------------------------------------------------------------------------
// utility function
template<typename T> void append(std::stringstream &streami, T value){
  streami.write((char*)&value, sizeof(T));
}
#ifdef USE_STRSTREAM
template<typename T> void append(std::strstream &streami, T value){
  streami.write((char*)&value, sizeof(T));
}
#endif
//-----------------------------------------------------------------------------


class VMK{
  
  // structs
  public:
  
  struct commhandle{
    commhandle *prev_handle;// previous handle in the queue
    commhandle *next_handle;// next handle in the queue
    int nelements;          // number of elements
    int type;       // 0: commhandle container, 1: MPI_Requests, 2: ... reserved
    bool sendFlag;          // true if this is a send request
    commhandle **handles;   // sub handles
    MPI_Request *mpireq;    // request array
  };

  struct memhandle{
    int localPet;           // index of the localPet in the memhandle context
    int localPetCount;      // number of PETs that share same SSI with localPET
    std::vector<int> counts;// allocs requested by a specific PET on same SSI
#ifndef ESMF_MPIUNI
    std::vector<MPI_Win> wins;  // MPI shared memory windows
#else
    std::vector<void*> mems;
    std::vector<unsigned long> sizes;
#endif
  };

  struct ipmutex{
    // mutex variable for intraProcess sync
    esmf_pthread_mutex_t pth_mutex;
    int lastFlag;
  };

  struct status{
    int srcPet;
    int tag;
    int error;
    // comm_type specific parts
    int comm_type;      // comm_type for this communication status
    MPI_Status mpi_s;

    status() : mpi_s() {}
  };
  
  struct pipc_mp{
    // hack sync variables
    shmsync shms;
    // name of SHM resource
    char shm_name[80];
    // buffer
    char buffer[2][PIPC_BUFFER];
  };

  struct shared_mp{
    // source and destination pointers
    volatile const void *ptr_src;
    volatile void *ptr_dst;
    volatile int tcounter;
    // non-blocking channels
    volatile const void *ptr_src_nb[SHARED_NONBLOCK_CHANNELS];
    volatile void *ptr_dst_nb[SHARED_NONBLOCK_CHANNELS];
    int recvCount;
    int sendCount;
    // hack sync variables
    shmsync shms;
    // buffer for small messages
    char buffer[SHARED_BUFFER];
    // Pthread sync variables
    esmf_pthread_mutex_t mutex1;
    esmf_pthread_cond_t cond1;
    esmf_pthread_mutex_t mutex2;
    esmf_pthread_cond_t cond2;
  };

  struct comminfo{
    int comm_type;    // communication type
    shared_mp *shmp;  // shared memory message passing structure
    pipc_mp *pipcmp;  // posix ipc message passing structure
  };

  struct ipshmAlloc{
    void *allocation;   // shared memory allocation
    int auxCounter;     // counter to coordinate alloc/dealloc between threads
    ipshmAlloc *prev;   // pointer to prev. ipshmAlloc element in list
    ipshmAlloc *next;   // pointer to next ipshmAlloc element in list
  };
  
  struct sendBuffer{
#ifdef USE_STRSTREAM
    std::strstream stream;
#else
    std::stringstream stream;
    std::string streamBuffer;
#endif
    MPI_Request mpireq;
    bool firstFlag;
   public:
    sendBuffer(){  // native constructor
      mpireq = MPI_REQUEST_NULL;
      firstFlag = true;
    }
    void clear();
  };
    
  struct recvBuffer{
    std::string streamBuffer;
    void *buffer;
    bool firstFlag;
   public:
    recvBuffer(){  // native constructor
      firstFlag = true;
    }
  };

  // members
  protected:
    int mypet;          // PET id of this instance
    esmf_pthread_t mypthid;  // my pthread id
    // pet -> core mapping
    int npets;      // number of PETs in this VMK
    int *lpid;      // local pid (equal to rank in local MPI context)
    int *pid;       // pid (equal to rank in MPI_COMM_WORLD)
    int *tid;       // thread index
    int *ncpet;     // number of cores this pet references
    int *nadevs;    // number of accelerator devices accessible from this pet
    int **cid;      // core id of the cores this pet references
    int ssiCount;   // number of single system images in this VMK
    int ssiMinPetCount;   // minimum PETs on a single system image
    int ssiMaxPetCount;   // maximum PETs on a single system image
    int ssiLocalPetCount; // number of PETs on the same SSI as localPet (incl.)
    int *ssiLocalPetList; // PETs that are on the same SSI as localPet (incl.)
    // general information about this VMK
    int mpionly;    // 0: there is multi-threading, 1: MPI-only
    int nothreadsflag; // 0-threaded VM, 1-non-threaded VM
    // MPI Communicator handles
    MPI_Comm mpi_c;     // communicator across the entire VM
#if !(defined ESMF_NO_MPI3 || defined ESMF_MPIUNI)
    MPI_Comm mpi_c_ssi; // communicator holding PETs on the same SSI
#endif
    // Shared mutex and thread_finish variables. These are pointers that will be
    // pointing to shared memory variables between different thread-instances of
    // the VMK object.
    esmf_pthread_mutex_t *pth_mutex2;
    esmf_pthread_mutex_t *pth_mutex;
    int *pth_finish_count;
    // Mutex flag used to indicate that this PET must use muteces for MPI comms
    int mpi_mutex_flag;
    // Communication channels
    comminfo *sendChannel;
    comminfo *recvChannel;
    // IntraProcessSharedMemoryAllocation List
    ipshmAlloc **ipshmTop;      // top of shared alloc list (shared)
    ipshmAlloc *ipshmLocalTop;  // local top of shared alloc list
    esmf_pthread_mutex_t *ipshmMutex;  // mutex to operate (shared)
    // IntraProcessMutex setup mutex
    esmf_pthread_mutex_t *ipSetupMutex;// mutex used to init and destroy(shared)
    // Communication requests queue
    int nhandles;
    commhandle *firsthandle;
    // Epoch support
    vmEpoch epoch;
    bool pastFirst; // true if the first epoch enabled call has been made
    std::map<int, sendBuffer> sendMap;
    std::map<int, recvBuffer> recvMap;
    // static info of physical machine
    static int nssiid;  // total number of single system image ids
    static int ncores;  // total number of cores in the physical machine
    static int *cpuid;  // cpuid associated with certain core (multi-core cpus)
    static int *ssiid;  // single system image id to which this core belongs
    static int *ssipe;  // PE id on the SSI on which this PE resides
    static double wtime0; // the MPI WTime at the very beginning of execution
  public:
    // Declaration of static data members - Definitions are in the header of
    // source file ESMF_VMKernel.C
    // static MPI Comm of the default VMK
    // and the thread level that the MPI implementation supports.
    static MPI_Comm default_mpi_c;
    static int mpi_thread_level;
    static int mpi_init_outside_esmf;
    static int pre_mpi_init;
    // Static data members that hold command line arguments
    // There are two sets of these variables. The first set of variables is
    // used to obtain the command line arguments in the obtain_args() method
    // and can be used throughout execution to access the command line args.
    // The second set with the "_mpich" part in the name is necessary to support
    // MPICH1.2. MPICH1.2 requires command line args to be passed into its
    // MPI_Init() routine, however, MPICH1.2 modifies the content of the passed
    // in variables and hence a second, dedicated set of argc and argv variables
    // is needed. MPICH1.2 is furthermore the reason why the statically
    // allocated _store variables and the extra indirection via 
    // argv = &(argv_store[0])
    // is needed. For some reason with MPICH1.2 a memory leak will be reported
    // if argv (and argv_mpich) were allocated dynamially via "new char*[100]"!
    static int argc;
    static char *argv_store[100];
    static char **argv;
    // Second set of variables to support MPICH1.2
    static int argc_mpich;
    static char *argv_mpich_store[100];
    static char **argv_mpich;

  // methods
  private:
    void obtain_args();
    void commqueueitem_link(commhandle *commh);
    int  commqueueitem_unlink(commhandle *commh);
  public:
    static void InitPreMPI();
      // initialization step before MPI is initialized
    void init(MPI_Comm mpiCommunicator=MPI_COMM_WORLD);
      // initialize the physical machine and a default (all MPI) virtual machine
    void finalize(int finalizeMpi=1);
      // finalize default (all MPI) virtual machine
    void abort();
      // abort default (all MPI) virtual machine

    void construct(void *sarg);
      // fill an already existing VMK object with info
    void destruct();
      // free allocations within an existing VMK object

    void *startup(class VMKPlan *vmp, void *(fctp)(void *, void *),
      void *cargo, int *rc);
      // enter a vm derived from current vm according to the VMKPlan
    void enter(class VMKPlan *vmp, void *arg, void *argvmkt);
    void exit(class VMKPlan *vmp, void *arg);
    void shutdown(class VMKPlan *vmp, void *arg);
      // exit a vm derived from current vm according to the VMKPlan
  
    void print() const;
    void log(std::string prefix,
      ESMC_LogMsgType_Flag msgType=ESMC_LOGMSG_INFO)const;
    static void logSystem(std::string prefix,
      ESMC_LogMsgType_Flag msgType=ESMC_LOGMSG_INFO);
    
    // get() calls    <-- to be replaced by following new inlined section
    int getNpets();                // return npets
    int getMypet();                // return mypet
    esmf_pthread_t getMypthid();   // return mypthid
    int getNcpet(int i);           // return ncpet
    int getNadevs(int i);          // return nadevs
    int getSsi(int i);             // return ssiid
    MPI_Comm getMpi_c();           // return mpi_c
    int getNthreads(int i);        // return number of threads in group PET
    int getTid(int i);             // return tid for PET
    int getVas(int i);             // return vas for PET
    int getLpid(int i);            // return lpid for PET
    
    int getDefaultTag(int src, int dst);   // return default tag
    int getMaxTag();               // return maximum value of tag
    
    const int *getSsipe() const {return ssipe;}
    int **getCid() const {return cid;}
        
    // get() calls
    int getLocalPet() const {return mypet;}
    int getCurrentSsiPe() const;
    int getPetCount() const {return npets;}
    int getSsiCount() const {return ssiCount;}
    int getSsiMinPetCount() const {return ssiMinPetCount;}
    int getSsiMaxPetCount() const {return ssiMaxPetCount;}
    int getSsiLocalPetCount() const {return ssiLocalPetCount;}
    const int *getSsiLocalPetList() const {return ssiLocalPetList;}
    esmf_pthread_t getLocalPthreadId() const {return mypthid;}
    static bool isPthreadsEnabled(){
#ifdef ESMF_NO_PTHREADS
      // did not compile with threads
      return false;
#else
      // check whether MPI impl. supports MPI_THREAD_SERIALIZED or higher
      if (mpi_thread_level < MPI_THREAD_SERIALIZED)
        return false;
      return true;
#endif
    }
    static bool isOpenMPEnabled(){
#ifdef ESMF_NO_OPENMP
      return false;
#else
#ifndef _OPENMP
#error - Compiler flags do not support OpenMP.
#else
      return true;
#endif
#endif
    }
    static bool isOpenACCEnabled(){
#ifdef ESMF_NO_OPENACC
      return false;
#else
      return true;
#endif
    }
    static bool isSsiSharedMemoryEnabled();
      //TODO: For now had to implement this method in the source file, because
      //TODO: of the way the ESMF_NO_MPI3 macro is being determined.
      //TODO: Move it into the VMKernel header once includes are fixed.

    // p2p communication calls
    int send(const void *message, int size, int dest, int tag=-1);
    int send(const void *message, int size, int dest, commhandle **commh,
      int tag=-1);
    int recv(void *message, int size, int source, int tag=-1,
      status *status=NULL);
    int recv(void *message, int size, int source, commhandle **commh,
      int tag=-1);
    
    int sendrecv(void *sendData, int sendSize, int dst, void *recvData,
      int recvSize, int src, int dstTag=-1, int srcTag=-1);
    int sendrecv(void *sendData, int sendSize, int dst, void *recvData,
      int recvSize, int src, commhandle **commh);

    int vassend(void *message, int size, int destVAS, commhandle **commh,
      int tag=-1);
    int vasrecv(void *message, int size, int srcVAS, commhandle **commh,
      int tag=-1);

    // collective communication calls
    int barrier();

    int threadbarrier();

    int reduce(void *in, void *out, int len, vmType type, vmOp op, 
      int root);
    int allreduce(void *in, void *out, int len, vmType type, vmOp op);
    int allfullreduce(void *in, void *out, int len, vmType type,
      vmOp op);
    
    int reduce_scatter(void *in, void *out, int *outCounts, vmType type,
      vmOp op);

    int scatter(void *in, void *out, int len, int root);
    int scatter(void *in, void *out, int len, int root, commhandle **commh);
    int scatterv(void *in, int *inCounts, int *inOffsets, void *out,
      int outCount, vmType type, int root);
    
    int gather(void *in, void *out, int len, int root);
    int gather(void *in, void *out, int len, int root, commhandle **commh);
    int gatherv(void *in, int inCount, void *out, int *outCounts, 
      int *outOffsets, vmType type, int root);
    int allgather(void *in, void *out, int len);
    int allgather(void *in, void *out, int len, commhandle **commh);
    int allgatherv(void *in, int inCount, void *out, int *outCounts, 
      int *outOffsets, vmType type);

    int alltoall(void *in, int inCount, void *out, int outCount,
      vmType type);
    int alltoallv(void *in, int *inCounts, int *inOffsets, void *out, 
      int *outCounts, int *outOffsets, vmType type);

    int broadcast(void *data, int len, int root);
    int broadcast(void *data, int len, int root, commhandle **commh);
    
    // non-blocking service calls
    int commtest(commhandle **commh, int *completeFlag, status *status=NULL);
    int commwait(commhandle **commh, status *status=NULL, int nanopause=0);
    void commqueuewait();
    void commcancel(commhandle **commh);
    bool cancelled(status *status);
    
    // SSI shared memory methods
    int ssishmAllocate(std::vector<unsigned long>&bytes, memhandle *memh,
      bool contigFlag=false);
    int ssishmFree(memhandle *memh);
    int ssishmGetMems(memhandle memh, int pet, std::vector<void *> *mems=NULL,
      std::vector<unsigned long> *bytes=NULL);
    int ssishmGetLocalPet(memhandle memh){return memh.localPet;}
    int ssishmGetLocalPetCount(memhandle memh){return memh.localPetCount;}
    int ssishmSync(memhandle memh);
        
    // IntraProcessSharedMemoryAllocation Table Methods
    void *ipshmallocate(int bytes, int *firstFlag=NULL);
    void ipshmdeallocate(void *);

    // IntraProcessMutex Methods
    ipmutex *ipmutexallocate();
    void ipmutexdeallocate(ipmutex *ipmutex);
    int ipmutexlock(ipmutex *ipmutex);
    int ipmutexunlock(ipmutex *ipmutex);
    
    // Simple thread-safety lock/unlock using internal pth_mutex
    int lock();
    int unlock();

    // Epoch support
    void epochSetFirst();
    void epochInit();
    void epochFinal();
    void epochEnter(vmEpoch epoch);
    void epochExit(bool keepAlloc=true);
    vmEpoch getEpoch() const {return epoch;}
        
    // Timer methods
    static void wtime(double *time);
    static void wtimeprec(double *prec);
    static void wtimedelay(double delay);

  // friend classes
  friend class VMKPlan;
};


class VMKPlan{
  public:
    int npets;
    int nplist;       // number of PETs in petlist that participate
    int *petlist;     // keeping sequence of parent pets
    int nothreadflag; // 0-default threaded VM, 1-non-threaded VM
    int parentVMflag; // 0-create child VM, 1-run on parent VM
    int *spawnflag;   // for each pet: 0-don't spawn, >=1-spawn threads
    int *contribute;  // pet id to which non-spawning pet contributes its cores
    int *cspawnid;    // idication to which one of spawned pets to contribute to
    // Pthread specifications
    size_t minStackSize;  // minimum stack size for any user thread created
    // VMK references for this PET (as many entries as this PET spawns)
    int nspawn;       // number of PETs this PET will spwan
    VMK **myvms; // this array holds pointers to heap VMK instances
    // Communication preferences
    // These preferences will be satisfied if the architecture supports it, 
    // otherwise the default communication setting is chosen instead.
    int pref_intra_process;     // default: PREF_INTRA_PROCESS_SHMHACK
    int pref_intra_ssi;         // default: PREF_INTRA_SSI_POSIXIPC
    int pref_inter_ssi;         // defualt: PREF_INTER_SSI_MPI1
    // MPI communicator for the participating PET group of parent VM
    int *lpid_mpi_g_part_map;
    MPI_Comm mpi_c_part;
    int commfreeflag;   // flag to indicate which PETs must free MPIcommunicator
    // OpenMP handling
    int openmphandling; // 0-none, 1-setnumthreads, 2-initialize, 3-pinaffinity
    int openmpnumthreads; // -1 default: local peCount

  public:
    VMKPlan();
      // native constructor (sets communication preferences to defaults)
    ~VMKPlan();
      // native destructor
    void vmkplan_garbage();
      // perform garbage collection within a VMKPlan object
    int vmkplan_nspawn();
      // return number of PETs that are being spawned out of current PET
    void vmkplan_myvms(ESMCI::VMK **myvms);
      // set the internal myvms pointer array
    void vmkplan_mpi_c_part(VMK &vm);
      // set the mpi communicator for participating PETs
    void vmkplan_useparentvm(VMK &vm);
      // use the parent VM, don't create new context
    void vmkplan_maxthreads(VMK &vm);  
      // set up a VMKPlan that will maximize the number of thread-pets
    void vmkplan_maxthreads(VMK &vm, int max);  
      // set up a VMKPlan that will max. number of thread-pets up to max
    void vmkplan_maxthreads(VMK &vm, int max, 
      int pref_intra_process, int pref_intra_ssi, int pref_inter_ssi);
      // set up a VMKPlan that will max. number of thread-pets up to max
    void vmkplan_maxthreads(VMK &vm, int max, int *plist, int nplist);  
      // set up a VMKPlan that will max. number of thread-pets up to max
      // but only allow PETs listed in plist to participate
    int vmkplan_maxthreads(VMK &vm, int max, int *plist, int nplist,
      int pref_intra_process, int pref_intra_ssi, int pref_inter_ssi); 
      // set up a VMKPlan that will max. number of thread-pets up to max
      // but only allow PETs listed in plist to participate
    void vmkplan_minthreads(VMK &vm);
      // set up a VMKPlan that will only have single threaded pet
      // instantiations and claim all cores of pets that don't make it through
    void vmkplan_minthreads(VMK &vm, int max);
      // set up a VMKPlan that will only have single threaded pet
      // instantiations and claim all cores of pets that don't make it through,
      // up to max cores per pet
    void vmkplan_minthreads(VMK &vm, int max, int *plist, int nplist);
      // set up a VMKPlan that will only have single threaded pet
      // instantiations and claim all cores of pets that don't make it through,
      // up to max cores per pet but only allow PETs listed in plist to
      // participate
    int vmkplan_minthreads(VMK &vm, int max, int *plist, int nplist,
      int pref_intra_process, int pref_intra_ssi, int pref_inter_ssi); 
      // set up a VMKPlan that will only have single threaded pet
      // instantiations and claim all cores of pets that don't make it through,
      // up to max cores per pet but only allow PETs listed in plist to
      // participate
    void vmkplan_maxcores(VMK &vm);
      // set up a VMKPlan that will have pets with the maximum number of
      // cores available
    void vmkplan_maxcores(VMK &vm, int max);
      // set up a VMKPlan that will have pets with the maximum number of
      // cores available, but not more than max
    void vmkplan_maxcores(VMK &vm, int max, int *plist, int nplist);
      // set up a VMKPlan that will have pets with the maximum number of
      // cores available, but not more than max and only use PETs listed in
      // plist
    int vmkplan_maxcores(VMK &vm, int max, int *plist, int nplist,
      int pref_intra_process, int pref_intra_ssi, int pref_inter_ssi); 
      // set up a VMKPlan that will have pets with the maximum number of
      // cores available, but not more than max and only use PETs listed in
      // plist
    void vmkplan_print();  

  friend class VMK;
  
};


class ComPat{
 private:
  // pure virtual methods to be implemented by user
     
  virtual int messageSize(int srcPet, int dstPet)                  const =0;
    // will be called on both sides, i.e. localPet==srcPet and localPet==dstPet
 
  virtual void messagePrepare(int srcPet, int dstPet, char *buffer)const =0;
    // will be called only for localPet==srcPet
  
  virtual void messageProcess(int srcPet, int dstPet, char *buffer)      =0;
    // will be called only for localPet==dstPet
  
  virtual void localPrepareAndProcess(int localPet)                      =0;
    // will be called for every localPet once
  
 public:
  // communication patterns
  void totalExchange(VMK *vmk);
}; // ComPat


class ComPat2{
 private:
  // pure virtual methods to be implemented by user
     
  virtual void handleLocal() =0;
    // called on every localPet exactly once, before any other method

  virtual void generateRequest(int responsePet,
    char* &requestBuffer, int &requestSize) =0;
    // called on every localPet for every responsePet != localPet
 
  virtual void handleRequest(int requestPet,
    char *requestBuffer, int requestSize,
    char* &responseBuffer, int &responseSize)const =0;
    // called on every localPet for every requestPet != localPet
 
  virtual void handleResponse(int responsePet,
    char const *responseBuffer, int responseSize)const =0;
    // called on every localPet for every responsePet != localPet

 public:
     
  // communication patterns
  void totalExchange(VMK *vmk);
  void selectiveExchange(VMK *vmk, std::vector<int>&responderPet, 
    std::vector<int>&requesterPet);
}; // ComPat2



//==============================================================================
//==============================================================================
//==============================================================================
// Socket based VMKernel prototyping
//==============================================================================
//==============================================================================
//==============================================================================

int const SOCKERR_UNSPEC        = -1;
int const SOCKERR_TIMEOUT       = -2;
int const SOCKERR_DISCONNECT    = -3;

int socketServerInit(
  int port,               // port number
  double timeout          // timeout in seconds
  //--------------------------------------------------------------------------
  // Attempt to open an INET socket as server and wait for a client to connect
  // The return value are:
  // >SOCKERR_UNSPEC  -- successfully connected socket
  // SOCKERR_UNSPEC   -- unspecified error, may be fatal, prints perror()
  // SOCKERR_TIMEOUT  -- timeout condition was reached
  //--------------------------------------------------------------------------
);

int socketClientInit(
  char const *serverName, // server by name
  int port,               // port number
  double timeout          // timeout in seconds
  //--------------------------------------------------------------------------
  // Attempt to open an INET socket and connect to the specified server.
  // The return value are:
  // >SOCKERR_UNSPEC  -- successfully connected socket
  // SOCKERR_UNSPEC   -- unspecified error, may be fatal, prints perror()
  // SOCKERR_TIMEOUT  -- timeout condition was reached
  //--------------------------------------------------------------------------
);
    
int socketFinal(
  int sock,         // connected socket to be finalized
  double timeout    // timeout in seconds
  //--------------------------------------------------------------------------
  // Attempt to cleanly take down a socket connection.
  // The return value are:
  // 0                -- successfully disconnect hand-shake
  // SOCKERR_UNSPEC   -- unspecified error, may be fatal, prints perror()
  // SOCKERR_TIMEOUT  -- timeout condition was reached
  //--------------------------------------------------------------------------
);

int socketSend(
  int sock,             // socket holding the connection
  void const *buffer,   // data buffer
  size_t size,          // number of bytes to send out of buffer
  double timeout        // timeout in seconds
  //--------------------------------------------------------------------------
  // Attempt to send data through a socket connection.
  // The return value are:
  // >SOCKERR_UNSPEC  -- number of bytes sent
  // SOCKERR_UNSPEC   -- unspecified error, may be fatal, prints perror()
  // SOCKERR_TIMEOUT  -- timeout condition was reached
  //--------------------------------------------------------------------------
);

int socketRecv(
  int sock,             // socket holding the connection
  void *buffer,         // data buffer
  size_t size,          // size of buffer in bytes
  double timeout        // timeout in seconds
  //--------------------------------------------------------------------------
  // Attempt to receive data through a socket connection.
  // The return value are:
  // >0                 -- number of bytes received
  // SOCKERR_UNSPEC     -- unspecified error, may be fatal, prints perror()
  // SOCKERR_TIMEOUT    -- timeout condition was reached
  // SOCKERR_DISCONNECT -- the other side has disconnected
  //--------------------------------------------------------------------------
);



// testing methods, TODO: will be removed when done
int socketServer(void);
int socketClient(void);


} // namespace ESMCI

#endif  // ESMCI_VMKERNEL_H
