// vmachine and vmplan class declarations

// 2003 Gerhard Theurich, NASA NCCS and SGI Professional Services
//  < theurich@nccs.gsfc.nasa.gov > or < gtheurich@sgi.com >

#ifndef ESMF_VMACHINE_H
#define ESMF_VMACHINE_H

#include <pthread.h>
#include <mpi.h>

// reduction operations
enum vmOp   { vmSUM=1, vmMIN, vmMAX};
enum vmType { vmI4=1, vmR4, vmR8};

// define the communication preferences
#define PREF_INTRA_PROCESS_SHMHACK    (0)       // default
#define PREF_INTRA_PROCESS_PTHREAD    (1)

#define PREF_INTRA_SSI_POSIXIPC       (0)
#define PREF_INTRA_SSI_MPI1           (1)       // default

#define PREF_INTER_SSI_MPI1           (0)       // default

// - buffer lenghts in bytes
#define PIPC_BUFFER                   (4096)
#define SHARED_BUFFER                 (64)

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

class vmachine{
  // members
  private:
    int mypet;      // PET id of this instance
    // pet -> core mapping
    int npets;      // number of PETs in this vmachine
    int *lpid;      // local pid (equal to rank in local MPI context)
    int *pid;       // pid (equal to rank in MPI_COMM_WORLD)
    int *tid;       // thread index 
    int *ncpet;     // number of cores this pet references
    int **cid;      // core id of the cores this pet references
    // general information about this vmachine
    int mpionly;    // 0: there is multi-threading, 1: MPI-only
    // MPI communication handles for MPI processes associated with this vmachine
    MPI_Group mpi_g;
    MPI_Comm mpi_c;
    // Shared mutex and thread_finish variables. These are pointers that will be
    // pointing to shared memory variables between different thread-instances of
    // the vmachine object.
    pthread_mutex_t *pth_mutex2;
    pthread_mutex_t *pth_mutex;
    int *pth_finish_count;
    // Communications array
    comminfo **commarray;   // this array is shared between pets of same pid
    // static info of physical machine
    static int ncores; // total number of cores in the physical machine
    static int *cpuid; // cpuid associated with certain core (multi-core cpus)
    static int *ssiid; // single system inmage id to which this core belongs
  public:    
    // static MPI info, MPI_COMM_WORLD Group and Comm of the default vmachine
    static MPI_Group default_mpi_g;
    static MPI_Comm default_mpi_c;

  // methods
  public:
    void vmachine_init(void);
      // initialize the physical machine and a default (all MPI) virtual machine
    void vmachine_finalize(void);
      // finalize default (all MPI) virtual machine

    void vmachine_construct(int mypet, int npets, int *lpid, int *pid, 
      int *tid, int *ncpet, int **cid, MPI_Group mpi_g, MPI_Comm mpi_c,
      pthread_mutex_t *pth_mutex2, 
      pthread_mutex_t *pth_mutex, int *pth_finish_count, 
      comminfo **commarray, int pref_intra_ssi);
      // fill an already existing vmachine object with info
    void vmachine_destruct(void);
      // free allocations within an existing vmachine object

    void *vmachine_enter(class vmplan &vmp, void *(fctp)(void *, void *),
      void *cargo);
      // enter a vm derived from current vm according to the vmplan
    void vmachine_exit(class vmplan &vmp, void *arg);
      // exit a vm derived from current vm according to the vmplan
  
    void vmachine_print(void);
    
    // info calls
    int vmachine_npets(void);           // return npets
    int vmachine_mypet(void);           // return mypet
    int vmachine_ncpet(int i);          // return ncpet
    int vmachine_ssiid(int i);          // return ssiid
    MPI_Comm vmachine_mpi_comm(void);   // return mpi_c
    int vmachine_nthreads(int i);       // return number of threads in group PET
    int vmachine_tid(int i);            // return tid for PET
    int vmachine_pid(int i);            // return pid for PET
    
    
    // communication calls
    void vmachine_send(void *message, int size, int dest);
    void vmachine_recv(void *message, int size, int source);
    void vmachine_barrier(void);
    
    // newly written communication calls
    void vmachine_sendrecv(void *sendData, int sendSize, int dst,
      void *recvData, int recvSize, int src);
    void vmachine_threadbarrier(void);
    void vmachine_allreduce(void *in, void *out, int len, vmType type, vmOp op);
    void vmachine_allglobalreduce(void *in, void *out, int len, vmType type,
      vmOp op);
    void vmachine_scatter(void *in, void *out, int len, int root);
    void vmachine_gather(void *in, void *out, int len, int root);
    

  // friends    
  friend class vmplan;
  friend class ESMC_VM;
};


class vmplan{
  private:
    int npets;
    int *spawnflag;   // for each pet: 0-don't spawn, >=1-spawn threads
    int *contribute;  // pet id to which non-spawning pet contributes its cores
    int *cspawnid;    // idication to which one of spawned pets to contribute to
    // vmachine references for this PET (as many entries as this PET spawns)
    int nspawn;       // number of PETs this PET will spwan
    vmachine **myvms; // this array holds pointers to heap vmachine instances
    // Communication preferences
    // These preferences will be satisfied if the architecture supports it, 
    // otherwise the default communication setting is chosen instead.
    int pref_intra_process;     // default: PREF_INTRA_PROCESS_SHMHACK
    int pref_intra_ssi;         // default: PREF_INTRA_SSI_POSIXIPC
    int pref_inter_ssi;         // defualt: PREF_INTER_SSI_MPI1
        
  public:
    vmplan(void);
      // native constructor (sets communication preferences to defaults)
    ~vmplan(void);
      // native destructor
    void vmplan_garbage(void);
      // perform garbage collection within a vmplan object
    int vmplan_nspawn(void);
      // return number of PETs that are being spawned out of current PET
    void vmplan_myvms(vmachine **myvms);
      // set the internal myvms pointer array
    void vmplan_maxthreads(vmachine &vm);  
      // set up a vmplan that will maximize the number of thread-pets
    void vmplan_maxthreads(vmachine &vm, int max);  
      // set up a vmplan that will maximize the number of thread-pets up to max
    void vmplan_maxthreads(vmachine &vm, int max, 
      int pref_intra_process, int pref_intra_ssi, int pref_inter_ssi);
      // set up a vmplan that will maximize the number of thread-pets up to max
    void vmplan_maxthreads(vmachine &vm, int max, int *plist, int nplist);  
      // set up a vmplan that will maximize the number of thread-pets up to max
      // but only allow PETs listed in plist to participate
    void vmplan_maxthreads(vmachine &vm, int max, int *plist, int nplist,
      int pref_intra_process, int pref_intra_ssi, int pref_inter_ssi); 
      // set up a vmplan that will maximize the number of thread-pets up to max
      // but only allow PETs listed in plist to participate
    void vmplan_minthreads(vmachine &vm);
      // set up a vmplan that will only have single threaded pet instantiations
      // and claim all cores of pets that don't make it through
    void vmplan_minthreads(vmachine &vm, int max);
      // set up a vmplan that will only have single threaded pet instantiations
      // and claim all cores of pets that don't make it through, up to max cores
      // per pet
    void vmplan_minthreads(vmachine &vm, int max, int *plist, int nplist);
      // set up a vmplan that will only have single threaded pet instantiations
      // and claim all cores of pets that don't make it through, up to max cores
      // per pet but only allow PETs listed in plist to participate
    void vmplan_minthreads(vmachine &vm, int max, int *plist, int nplist,
      int pref_intra_process, int pref_intra_ssi, int pref_inter_ssi); 
      // set up a vmplan that will only have single threaded pet instantiations
      // and claim all cores of pets that don't make it through, up to max cores
      // per pet but only allow PETs listed in plist to participate
    void vmplan_maxcores(vmachine &vm);
      // set up a vmplan that will have pets with the maximum number of cores
      // available
    void vmplan_maxcores(vmachine &vm, int max);
      // set up a vmplan that will have pets with the maximum number of cores
      // available, but not more than max
    void vmplan_maxcores(vmachine &vm, int max, int *plist, int nplist);
      // set up a vmplan that will have pets with the maximum number of cores
      // available, but not more than max and only use PETs listed in plist
    void vmplan_maxcores(vmachine &vm, int max, int *plist, int nplist,
      int pref_intra_process, int pref_intra_ssi, int pref_inter_ssi); 
      // set up a vmplan that will have pets with the maximum number of cores
      // available, but not more than max and only use PETs listed in plist
    void vmplan_print(void);  

  friend class vmachine;
  
};

#endif  // ESMF_VMACHINE_H
