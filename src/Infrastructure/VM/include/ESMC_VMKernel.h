// $Id: ESMC_VMKernel.h,v 1.44 2007/04/23 18:53:44 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
//
//-----------------------------------------------------------------------------

#ifndef ESMC_VMKERNEL_H
#define ESMC_VMKERNEL_H

#ifdef VMK_STANDALONE
#include <pthread.h>
#else
#include "ESMF_Pthread.h"
#endif

#ifndef MPICH_IGNORE_CXX_SEEK
#define MPICH_IGNORE_CXX_SEEK
#endif
#include <mpi.h>

// reduction operations
enum vmOp   { vmSUM=1, vmMIN, vmMAX};
// typekind indicators
enum vmType { vmI4=1, vmR4, vmR8};

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

// - buffer lenghts in bytes
#define PIPC_BUFFER                   (4096)
#define SHARED_BUFFER                 (64)

// IntraProcessSharedMemoryAllocation Table size
#define IPSHM_TABLE_SIZE              (1000)

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


typedef struct{
  // mutex variable for intraProcess sync
  pthread_mutex_t pth_mutex;
  int lastFlag;
}vmk_ipmutex;


typedef struct vmk_ch{
  struct vmk_ch *prev_handle;   // previous handle in the queue
  struct vmk_ch *next_handle;   // next handle in the queue
  int nelements;    // number of elements
  int type;         // 0: commhandle container, 1: MPI_Requests, 2: ... reserved
  struct vmk_ch **handles;   // sub handles
  MPI_Request *mpireq; // request array
}vmk_commhandle;


typedef struct{
  int srcPet;
  int tag;
  int error;
  // comm_type specific parts
  int comm_type;      // comm_type for this communication status
  MPI_Status mpi_s;
}vmk_status;


typedef struct{
  // hack sync variables
  shmsync shms;
  // name of SHM resource
  char shm_name[80];
  // buffer
  char buffer[2][PIPC_BUFFER];
}pipc_mp;


typedef struct{
  // source and destination pointers
  volatile void *ptr_src;
  volatile void *ptr_dest;
  // hack sync variables
  shmsync shms;
  // buffer for small messages
  char buffer[SHARED_BUFFER];
  // Pthread sync variables
  pthread_mutex_t mutex1;
  pthread_cond_t cond1;
  pthread_mutex_t mutex2;
  pthread_cond_t cond2;
  volatile int tcounter;
}shared_mp;


typedef struct{
  int comm_type;    // communication type
  shared_mp *shmp;  // shared memory message passing structure
  pipc_mp *pipcmp;  // posix ipc message passing structure
}comminfo;


class ESMC_VMK{
  // members
  protected:
    int mypet;          // PET id of this instance
    pthread_t mypthid;  // my pthread id
    // pet -> core mapping
    int npets;      // number of PETs in this ESMC_VMK
    int *lpid;      // local pid (equal to rank in local MPI context)
    int *pid;       // pid (equal to rank in MPI_COMM_WORLD)
    int *tid;       // thread index
    int *ncpet;     // number of cores this pet references
    int **cid;      // core id of the cores this pet references
    // general information about this ESMC_VMK
    int mpionly;    // 0: there is multi-threading, 1: MPI-only
    int nothreadsflag; // 0-threaded VM, 1-non-threaded VM
    // MPI communication handles for MPI processes associated with this ESMC_VMK
    MPI_Group mpi_g;
    MPI_Comm mpi_c;
    // Shared mutex and thread_finish variables. These are pointers that will be
    // pointing to shared memory variables between different thread-instances of
    // the ESMC_VMK object.
    pthread_mutex_t *pth_mutex2;
    pthread_mutex_t *pth_mutex;
    int *pth_finish_count;
    // Mutex flag used to indicate that this PET must use muteces for MPI comms
    int mpi_mutex_flag;
    // Communications array
    comminfo **commarray;   // this array is shared between pets of same pid
    // IntraProcessSharedMemoryAllocation Table
    void **ipshmTable;  // table of memory pointers (shared)
    int *ipshmDeallocTable;  // keep track of how many threads dealloc (shared)
    int *ipshmCount;    // current number of allocations (shared)
    int ipshmAllocCount;// local count of how many times Alloc was called not-s
    pthread_mutex_t *ipshmMutex;  // mutex to operate (shared)
    // IntraProcessMutex setup mutex
    pthread_mutex_t *ipSetupMutex;  // mutex used to init and destroy (shared)
    // Communication requests queue
    int nhandles;
    vmk_commhandle *firsthandle;
    // static info of physical machine
    static int ncores; // total number of cores in the physical machine
    static int *cpuid; // cpuid associated with certain core (multi-core cpus)
    static int *ssiid; // single system inmage id to which this core belongs
  public:
    // Declaration of static data members - Definitions are in the header of
    // source file ESMF_VMKernel.C
    // static MPI info, Group and Comm of the default ESMC_VMK
    // and the thread level that the MPI implementation supports.
    static MPI_Group default_mpi_g;
    static MPI_Comm default_mpi_c;
    static int mpi_thread_level;
    // Static data members that hold command line arguments
    // There are two sets of these variables. The first set of variables is
    // used to obtain the command line arguments in the vmk_obtain_args() method
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
    void vmk_obtain_args(void);
    void vmk_commqueueitem_link(vmk_commhandle *commhandle);
    int vmk_commqueueitem_unlink(vmk_commhandle *commhandle);
  public:
    void vmk_init(MPI_Comm mpiCommunicator=MPI_COMM_WORLD);
      // initialize the physical machine and a default (all MPI) virtual machine
    void vmk_finalize(int finalizeMpi=1);
      // finalize default (all MPI) virtual machine
    void vmk_abort(void);
      // abort default (all MPI) virtual machine

    void vmk_construct(void *sarg);
      // fill an already existing ESMC_VMK object with info
    void vmk_destruct(void);
      // free allocations within an existing ESMC_VMK object

    void *vmk_startup(class ESMC_VMKPlan *vmp, void *(fctp)(void *, void *),
      void *cargo, int *rc);
      // enter a vm derived from current vm according to the ESMC_VMKPlan
    void vmk_enter(class ESMC_VMKPlan *vmp, void *arg, void *argvmkt);
    void vmk_exit(class ESMC_VMKPlan *vmp, void *arg);
    void vmk_shutdown(class ESMC_VMKPlan *vmp, void *arg);
      // exit a vm derived from current vm according to the ESMC_VMKPlan
  
    void vmk_print(void);
    
    // info calls
    int vmk_npets(void);           // return npets
    int vmk_mypet(void);           // return mypet
    pthread_t vmk_mypthid(void);   // return mypthid
    int vmk_ncpet(int i);          // return ncpet
    int vmk_ssiid(int i);          // return ssiid
    MPI_Comm vmk_mpi_comm(void);   // return mpi_c
    int vmk_nthreads(int i);       // return number of threads in group PET
    int vmk_tid(int i);            // return tid for PET
    int vmk_vas(int i);            // return vas for PET
    int vmk_lpid(int i);           // return lpid for PET
    
    
    // p2p communication calls
    void vmk_send(void *message, int size, int dest, int tag=-1);
    void vmk_send(void *message, int size, int dest,
      vmk_commhandle **commhandle, int tag=-1);
    void vmk_recv(void *message, int size, int source, int tag=-1, 
      vmk_status *status=NULL);
    void vmk_recv(void *message, int size, int source,
      vmk_commhandle **commhandle, int tag=-1);
    
    void vmk_sendrecv(void *sendData, int sendSize, int dst,
      void *recvData, int recvSize, int src);
    void vmk_sendrecv(void *sendData, int sendSize, int dst,
      void *recvData, int recvSize, int src, vmk_commhandle **commhandle);

    void vmk_vassend(void *message, int size, int destVAS,
      vmk_commhandle **commhandle, int tag=-1);
    void vmk_vasrecv(void *message, int size, int srcVAS,
      vmk_commhandle **commhandle, int tag=-1);

    // collective communication calls
    void vmk_barrier(void);

    void vmk_threadbarrier(void);

    void vmk_reduce(void *in, void *out, int len, vmType type, vmOp op, 
      int root);
    void vmk_allreduce(void *in, void *out, int len, vmType type, vmOp op);
    void vmk_allfullreduce(void *in, void *out, int len, vmType type,
      vmOp op);
    
    void vmk_scatter(void *in, void *out, int len, int root);
    void vmk_scatter(void *in, void *out, int len, int root,
      vmk_commhandle **commhandle);
    
    void vmk_gather(void *in, void *out, int len, int root);
    void vmk_gather(void *in, void *out, int len, int root,
      vmk_commhandle **commhandle);
    void vmk_allgather(void *in, void *out, int len);
    void vmk_allgather(void *in, void *out, int len,
      vmk_commhandle **commhandle);
    void vmk_allgatherv(void *in, int inCount, void *out, int *outCounts, 
      int *outOffsets, vmType type);

    void vmk_alltoallv(void *in, int *inCounts, int *inOffsets, void *out, 
      int *outCounts, int *outOffsets, vmType type);

    void vmk_broadcast(void *data, int len, int root);
    void vmk_broadcast(void *data, int len, int root,
      vmk_commhandle **commhandle);
    
    // non-blocking service calls
    void vmk_commwait(vmk_commhandle **commhandle, vmk_status *status=NULL,
      int nanopause=0);
    void vmk_commqueuewait(void);
    void vmk_commcancel(vmk_commhandle **commhandle);
    
    // IntraProcessSharedMemoryAllocation Table Methods
    void *vmk_ipshmallocate(int bytes, int *firstFlag=NULL);
    void vmk_ipshmdeallocate(void *);

    // IntraProcessMutex Methods
    vmk_ipmutex *vmk_ipmutexallocate(void);
    void vmk_ipmutexdeallocate(vmk_ipmutex *ipmutex);
    int vmk_ipmutexlock(vmk_ipmutex *ipmutex);
    int vmk_ipmutexunlock(vmk_ipmutex *ipmutex);

  // friend classes
  friend class ESMC_VMKPlan;
};

void vmk_wtime(double *time);
void vmk_wtimeprec(double *prec);
void vmk_wtimedelay(double delay);

class ESMC_VMKPlan{
  public:
    int npets;
    int nplist;       // number of PETs in petlist that participate
    int *petlist;     // keeping sequence of parent pets
    int nothreadflag; // 0-default threaded VM, 1-non-threaded VM
    int parentVMflag; // 0-create child VM, 1-run on parent VM
    int *spawnflag;   // for each pet: 0-don't spawn, >=1-spawn threads
    int *contribute;  // pet id to which non-spawning pet contributes its cores
    int *cspawnid;    // idication to which one of spawned pets to contribute to
    // ESMC_VMK references for this PET (as many entries as this PET spawns)
    int nspawn;       // number of PETs this PET will spwan
    ESMC_VMK **myvms; // this array holds pointers to heap ESMC_VMK instances
    // Communication preferences
    // These preferences will be satisfied if the architecture supports it, 
    // otherwise the default communication setting is chosen instead.
    int pref_intra_process;     // default: PREF_INTRA_PROCESS_SHMHACK
    int pref_intra_ssi;         // default: PREF_INTRA_SSI_POSIXIPC
    int pref_inter_ssi;         // defualt: PREF_INTER_SSI_MPI1
    // MPI communicator for the participating PET group of parent VM
    int *lpid_mpi_g_part_map;
    MPI_Group mpi_g_part;
    MPI_Comm mpi_c_part;
    int commfreeflag;   // flag to indicate which PETs must free MPIcommunicator
    int groupfreeflag;  // flag to indicate which PETs must free MPIgroup
        
  public:
    ESMC_VMKPlan(void);
      // native constructor (sets communication preferences to defaults)
    ~ESMC_VMKPlan(void);
      // native destructor
    void vmkplan_garbage(void);
      // perform garbage collection within a ESMC_VMKPlan object
    int vmkplan_nspawn(void);
      // return number of PETs that are being spawned out of current PET
    void vmkplan_myvms(ESMC_VMK **myvms);
      // set the internal myvms pointer array
    void vmkplan_mpi_c_part(ESMC_VMK &vm);
      // set the mpi communicator for participating PETs
    void vmkplan_useparentvm(ESMC_VMK &vm);
      // use the parent VM, don't create new context
    void vmkplan_maxthreads(ESMC_VMK &vm);  
      // set up a ESMC_VMKPlan that will maximize the number of thread-pets
    void vmkplan_maxthreads(ESMC_VMK &vm, int max);  
      // set up a ESMC_VMKPlan that will max. number of thread-pets up to max
    void vmkplan_maxthreads(ESMC_VMK &vm, int max, 
      int pref_intra_process, int pref_intra_ssi, int pref_inter_ssi);
      // set up a ESMC_VMKPlan that will max. number of thread-pets up to max
    void vmkplan_maxthreads(ESMC_VMK &vm, int max, int *plist, int nplist);  
      // set up a ESMC_VMKPlan that will max. number of thread-pets up to max
      // but only allow PETs listed in plist to participate
    int vmkplan_maxthreads(ESMC_VMK &vm, int max, int *plist, int nplist,
      int pref_intra_process, int pref_intra_ssi, int pref_inter_ssi); 
      // set up a ESMC_VMKPlan that will max. number of thread-pets up to max
      // but only allow PETs listed in plist to participate
    void vmkplan_minthreads(ESMC_VMK &vm);
      // set up a ESMC_VMKPlan that will only have single threaded pet
      // instantiations and claim all cores of pets that don't make it through
    void vmkplan_minthreads(ESMC_VMK &vm, int max);
      // set up a ESMC_VMKPlan that will only have single threaded pet
      // instantiations and claim all cores of pets that don't make it through,
      // up to max cores per pet
    void vmkplan_minthreads(ESMC_VMK &vm, int max, int *plist, int nplist);
      // set up a ESMC_VMKPlan that will only have single threaded pet
      // instantiations and claim all cores of pets that don't make it through,
      // up to max cores per pet but only allow PETs listed in plist to
      // participate
    int vmkplan_minthreads(ESMC_VMK &vm, int max, int *plist, int nplist,
      int pref_intra_process, int pref_intra_ssi, int pref_inter_ssi); 
      // set up a ESMC_VMKPlan that will only have single threaded pet
      // instantiations and claim all cores of pets that don't make it through,
      // up to max cores per pet but only allow PETs listed in plist to
      // participate
    void vmkplan_maxcores(ESMC_VMK &vm);
      // set up a ESMC_VMKPlan that will have pets with the maximum number of
      // cores available
    void vmkplan_maxcores(ESMC_VMK &vm, int max);
      // set up a ESMC_VMKPlan that will have pets with the maximum number of
      // cores available, but not more than max
    void vmkplan_maxcores(ESMC_VMK &vm, int max, int *plist, int nplist);
      // set up a ESMC_VMKPlan that will have pets with the maximum number of
      // cores available, but not more than max and only use PETs listed in
      // plist
    int vmkplan_maxcores(ESMC_VMK &vm, int max, int *plist, int nplist,
      int pref_intra_process, int pref_intra_ssi, int pref_inter_ssi); 
      // set up a ESMC_VMKPlan that will have pets with the maximum number of
      // cores available, but not more than max and only use PETs listed in
      // plist
    void vmkplan_print(void);  

  friend class ESMC_VMK;
  
};

#endif  // ESMC_VMKERNEL_H
