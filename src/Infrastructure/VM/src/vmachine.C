// vmachine and vmplan class implementations

// 2003 Gerhard Theurich, NASA NCCS and SGI Professional Services
//  < theurich@nccs.gsfc.nasa.gov > or < gtheurich@sgi.com >

#include <sys/types.h>

// On OSF1 (i.e. Tru64) systems there is a problem with picking up the 
// prototype of gethostid() from unistd.h from within C++....
#ifdef __osf__
#define _XOPEN_SOURCE_EXTENDED
#endif

#include <unistd.h>

// On OSF1 (i.e. Tru64) systems there is a problem with picking up the 
// prototype of gethostid() from unistd.h from within C++....
#ifdef __osf__
#undef _XOPEN_SOURCE_EXTENDED
#endif

#include <pthread.h>

#include <stdio.h>
#include <string.h>
#include <signal.h>

#include <sys/mman.h>
#include <fcntl.h>

#include <mpi.h>

#include "vmachine.h"

// macros used within this source file
#define VERBOSITY             (0)       // 0: off
#define VMACHINE_TID_MPI_TAG  (10)      // mpi tag used to send/recv TID
// - communication identifiers
#define VM_COMM_TYPE_MPI1     (0)
#define VM_COMM_TYPE_PTHREAD  (1)
#define VM_COMM_TYPE_SHMHACK  (2)
#define VM_COMM_TYPE_POSIXIPC (3)


// initialization of class static variables
MPI_Group vmachine::default_mpi_g;
MPI_Comm vmachine::default_mpi_c;
int vmachine::ncores;
int *vmachine::cpuid;
int *vmachine::ssiid;


void vmachine::vmachine_init(void){
  // initialize the physical machine and a default (all MPI) virtual machine
  // initialize signal handling -> this MUST happen before MPI_Init is called!!
  struct sigaction action;
  action.sa_handler = SIG_DFL;
  sigemptyset (&(action.sa_mask));
  sigaction(SIGUSR1, &action, NULL);  // restore default handle for USR1
  sigaction(SIGUSR2, &action, NULL);  // restore default handle for USR2
  sigset_t sigs_to_block;
  sigemptyset(&sigs_to_block);
  sigaddset(&sigs_to_block, SIGUSR1);
  sigaddset(&sigs_to_block, SIGUSR2);
  sigprocmask(SIG_BLOCK, &sigs_to_block, NULL); // block USR1 and USR2
  // next check is whether MPI has been initialized yet
  // actually we need to indicate an error if MPI has been initialized before
  // because signal blocking might not reach all of the threads again...
  int initialized;
  MPI_Initialized(&initialized);
  if (!initialized){
    // MPI_Init(NULL, NULL);
    // MPICH fix for broken MPI_Init():
    int fix_argc=0;
    char *fix_v;
    char **fix_argv=&fix_v;
    MPI_Init(&fix_argc, &fix_argv);
  } 
  // so now MPI is for sure initialized...
  int rank, size;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  // since this method is only supposed to be called my the main_vmachine 
  // and the main_vmachine is all MPI pets we can do the following:
  npets=size;           // user is required to start with #processes=#cores!!!!
  mypet=rank;
  mpionly = 1;          // the default VM can only be MPI-only
  // set up private MPI_COMM_WORLD Group and Comm
  MPI_Comm_group(MPI_COMM_WORLD, &mpi_g);
  MPI_Comm_create(MPI_COMM_WORLD, mpi_g, &mpi_c);
  // ... and copy them into the class static default variables...
  default_mpi_g = mpi_g;
  default_mpi_c = mpi_c;
  // initialize the shared memory variables
  pth_finish_count = NULL;
  pth_mutex = new pthread_mutex_t;
  pthread_mutex_init(pth_mutex, NULL);
  pth_mutex2 = new pthread_mutex_t;
  pthread_mutex_init(pth_mutex2, NULL);
  // set up the communication array
  commarray = new comminfo*[npets];
  for (int i=0; i<npets; i++){
    commarray[i] = new comminfo[npets];
    for (int j=0; j<npets; j++){
      // for the default vmachine all communication is via MPI-1
      commarray[i][j].comm_type = VM_COMM_TYPE_MPI1;
    }
  }
  // set up physical machine info
  ncores=size;          // user is required to start with #processes=#cores!!!!
  cpuid = new int[ncores];
  ssiid = new int[ncores];
  long int *temp_ssiid = new long int[ncores];
  for (int i=0; i<ncores; i++){
    cpuid[i]=i;                 // hardcoded assumption of single-core CPUs
    if (i==rank){
      temp_ssiid[i]=gethostid();
    }
    MPI_Bcast(&temp_ssiid[i], sizeof(long int), MPI_BYTE, i, mpi_c);
  }
  // now re-number the ssiid[] to go like 0, 1, 2, ...
  int ssi_counter=0;
  for (int i=0; i<ncores; i++){
    int j;
    for (j=0; j<i; j++)
      if (temp_ssiid[j] == temp_ssiid[i]) break;
    if (j==i){
      // new ssiid
      ssiid[i]=ssi_counter;
      ++ssi_counter;
    }else{
      // found previous ssiid
      ssiid[i]=ssiid[j];
    }
  }
  delete [] temp_ssiid;
  // vmachine pet -> core mapping
  lpid = new int[npets];
  pid = new int[npets];
  tid = new int[npets];
  ncpet = new int[npets];
  cid = new int*[npets];
  for (int i=0; i<npets; i++){
    lpid[i]=i;
    pid[i]=i;
    tid[i]=0;
    ncpet[i]=1;
    cid[i] = new int[ncpet[i]];
    cid[i][0]=i;
  }
}


void vmachine::vmachine_finalize(void){
  // finalize default (all MPI) virtual machine
  // todo: delete all allocations of default vmachine
  int finalized;
  MPI_Finalized(&finalized);
  if (!finalized)
    MPI_Finalize();
}


void vmachine::vmachine_construct(int mypet, int npets, int *lpid, int *pid,
  int *tid, int *ncpet, int **cid, MPI_Group mpi_g, MPI_Comm mpi_c,
  pthread_mutex_t *pth_mutex2,
  pthread_mutex_t *pth_mutex, int *pth_finish_count, comminfo **commarray,
  int pref_intra_ssi){
  // fill an already existing vmachine object with info
  this->mypet=mypet;
  this->npets=npets;
  this->lpid = new int[npets];
  this->pid = new int[npets];
  this->tid = new int[npets];
  this->ncpet = new int[npets];
  this->cid = new int*[npets];
  for (int i=0; i<npets; i++){
    this->lpid[i]=lpid[i];
    this->pid[i]=pid[i];
    this->tid[i]=tid[i];
    this->ncpet[i]=ncpet[i];
    this->cid[i] = new int[ncpet[i]];
    for (int k=0; k<ncpet[i]; k++)
      this->cid[i][k] = cid[i][k];
  }
  this->mpi_g = mpi_g;
  this->mpi_c = mpi_c;
  this->pth_mutex2 = pth_mutex2;
  this->pth_mutex = pth_mutex;
  this->pth_finish_count = pth_finish_count;
  this->commarray = commarray;
  if (pref_intra_ssi == PREF_INTRA_SSI_POSIXIPC){
    // now set up the POSIX IPC shared memory resources between pets
    // that run within the same SSI but different PID
    for (int i=0; i<npets; i++){
      // looping over all pets
      if (vmachine_ssiid(i) == vmachine_ssiid(mypet)){
        // found a pet under same SSI ...
        if (pid[i] != pid[mypet]){
          // ... and with different PID (which also excludes mypet!)
          // ready to set up shared memory segment using POSIX IPC
          char shm_file[80];
          int shm_fd;
          int size = sizeof(pipc_mp);
          void *shm_segment;
          // first: [mypet][i]
          sprintf(shm_file, "/tmp/shm_channel_%d_%d", mypet, i);
          // get a descriptor for this shared memory resource
          // which ever PET comes first will create this resource, the other
          // will just open it...
          shm_fd = shm_open(shm_file, O_RDWR | O_CREAT | O_EXCL , 0600);
          if (shm_fd == -1){
            // resource existed
            shm_fd = shm_open(shm_file, O_RDWR, 0600);
            shm_segment = mmap(NULL, size, PROT_WRITE, MAP_SHARED, shm_fd, 
            (off_t)0);
          }else{
            // resource needs to be created and sized
            ftruncate(shm_fd, size);
            shm_segment = mmap(NULL, size, PROT_WRITE, MAP_SHARED, shm_fd, 
            (off_t)0);
            strcpy(((pipc_mp *)shm_segment)->shm_name, shm_file);
            sync_reset(&((pipc_mp *)shm_segment)->shms);
          }
          // enter the address into the commarray
          commarray[mypet][i].pipcmp = (pipc_mp *)shm_segment;
          commarray[mypet][i].comm_type = VM_COMM_TYPE_POSIXIPC;
          // then: [i][sarg->mypet]
          sprintf(shm_file, "/tmp/shm_channel_%d_%d", i, mypet);
          // get a descriptor for this shared memory resource
          // which ever PET comes first will create this resource, the other
          // will just open it...
          shm_fd = shm_open(shm_file, O_RDWR | O_CREAT | O_EXCL , 0600);
          if (shm_fd == -1){
            // resource existed
            shm_fd = shm_open(shm_file, O_RDWR, 0600);
            shm_segment = mmap(NULL, size, PROT_WRITE, MAP_SHARED, shm_fd, 
            (off_t)0);
          }else{
            // resource needs to be created and sized
            ftruncate(shm_fd, size);
            shm_segment = mmap(NULL, size, PROT_WRITE, MAP_SHARED, shm_fd, 
            (off_t)0);
            strcpy(((pipc_mp *)shm_segment)->shm_name, shm_file);
            sync_reset(&((pipc_mp *)shm_segment)->shms);
          }
          // enter the address into the commarray
          commarray[i][mypet].pipcmp = (pipc_mp *)shm_segment;
          commarray[i][mypet].comm_type = VM_COMM_TYPE_POSIXIPC;
        }
      }
    }
  }
  // determine whether we are dealing with an MPI-only vmachine
  this->mpionly=1;  // assume this is MPI-only vmachine until found otherwise
  for (int i=0; i<npets; i++)
    if (this->tid[i]>0) this->mpionly=0;    // found multi-threading PET
  // need a barrier here before any of the PETs get into user code...
  vmachine_barrier();
}


void vmachine::vmachine_destruct(void){
  // determine how many pets are of the same pid as mypet is
  int num_same_pid=0;
  for (int i=0; i<npets; i++)
    if (pid[i]==pid[mypet])
      ++num_same_pid;
  // check with the other pets under this pid where we are in wrap-up
  int last_flag=0;
  pthread_mutex_lock(pth_mutex);
  ++(*pth_finish_count);          // increment counter
#if (VERBOSITY > 0)
  printf("wrap-up counts: %d %d\n", *pth_finish_count, num_same_pid);
#endif
  if (*pth_finish_count == num_same_pid)
    last_flag=1; // indicate that I am the last pet for this pid to wrap up...
  pthread_mutex_unlock(pth_mutex);
  // now we know if we are the last pet for this pid
  if (last_flag){
    // mypet is the last pet of this pid to wrap up:
#if (VERBOSITY > 0)
    printf("mypet is the last one to wrap up for this pid..MPI & shared mem\n");
#endif
    //  - free MPI Group and Comm
    MPI_Comm_free(&mpi_c);
    MPI_Group_free(&mpi_g);  // this might have to be called from higher up...
    //  - free the shared memory variables
    pthread_mutex_destroy(pth_mutex2);
    delete pth_mutex2;
    pthread_mutex_destroy(pth_mutex);
    delete pth_mutex;
    delete pth_finish_count;
    // - free commarray
    for (int pet1=0; pet1<npets; pet1++){
      for (int pet2=0; pet2<npets; pet2++){
        if(commarray[pet1][pet2].comm_type==VM_COMM_TYPE_SHMHACK
          ||commarray[pet1][pet2].comm_type==VM_COMM_TYPE_PTHREAD){
          // intra-process shared memory structure to be deleted
          shared_mp *shmp=commarray[pet1][pet2].shmp;
          if(commarray[pet1][pet2].comm_type==VM_COMM_TYPE_PTHREAD){                         pthread_mutex_destroy(&(shmp->mutex1));
            pthread_cond_destroy(&(shmp->cond1));
            pthread_mutex_destroy(&(shmp->mutex2));
            pthread_cond_destroy(&(shmp->cond2));
          }
#if (VERBOSITY > 0)
          printf("deleting shmp=%p for pet1=%d, pet2=%d, mypet=%d\n", 
            shmp, pet1, pet2, mypet);
#endif          
          delete shmp;
        }else if (commarray[pet1][pet2].comm_type==VM_COMM_TYPE_POSIXIPC){
          // this means that mypet is either pet1 or pet2
          pipc_mp *pipcmp=commarray[pet1][pet2].pipcmp;
          char shm_name[80];
          strcpy(shm_name, pipcmp->shm_name);
          munmap((void *)pipcmp, sizeof(pipc_mp));
          shm_unlink(shm_name);
#if (VERBOSITY > 0)
          printf("deleting pipcmp=%p (%s) for pet1=%d, pet2=%d, mypet=%d\n", 
            pipcmp, shm_name, pet1, pet2, mypet);
#endif          
        }
      }
      delete [] commarray[pet1];
    }
    delete [] commarray;
  }
  delete [] lpid;
  delete [] pid;
  delete [] tid;
  delete [] ncpet;
  for (int i=0; i<npets; i++)
    delete [] cid[i];
  delete [] cid;
}  


struct contrib_id{
  pthread_t blocker_tid;    // POSIX thread id of blocker thread
  int mpi_pid;              // MPI rank in the context of the default vmachine
  pid_t pid;                // POSIX process id
  pthread_t tid;            // POSIX thread id
};


struct vmachine_spawn_arg{
  // members which are different for each new pet
  vmachine *myvm;             // pointer to vm instance on heap
  pthread_t pthid;            // pthread id of the spawned thread
  int mypet;                  // new mypet 
  int *ncontributors;         // number of pets that contributed cores 
  contrib_id **contributors;  // array of contributors
  // members which are identical for all new pets
  void *(*fctp)(vmachine &, void *);  // pointer to the user function
  int npets;                  // new number of pets
  int *lpid;
  int *pid;
  int *tid;
  int *ncpet;
  int **cid;
  MPI_Group mpi_g;
  MPI_Comm mpi_c;
  // shared memory variables
  pthread_mutex_t *pth_mutex2;
  pthread_mutex_t *pth_mutex;
  int *pth_finish_count;
  comminfo **commarray;
  int pref_intra_ssi;
  // cargo
  void *cargo;
};

    
static void *vmachine_spawn(void *arg){
  // procedure that gets called from vmachine_enter via pthread_create()
  // typecast the argument into the type it really is:
  vmachine_spawn_arg *sarg = (vmachine_spawn_arg *)arg;
#if (VERBOSITY > 0)
  printf("hello from within vmachine_spawn, mypet=%d\n", sarg->mypet);
#endif
  // obtain reference to the vm instance on heap
  vmachine &vm = *(sarg->myvm);
  // setup the pet section in this vm instance
  vm.vmachine_construct(sarg->mypet, sarg->npets, sarg->lpid, sarg->pid,
    sarg->tid, sarg->ncpet, sarg->cid, sarg->mpi_g, sarg->mpi_c,
    sarg->pth_mutex2, sarg->pth_mutex, sarg->pth_finish_count,
    sarg->commarray, sarg->pref_intra_ssi);
  // call the function pointer with the new vmachine as its argument
  // this is where we finally enter the user code again...  
  sarg->fctp(vm, sarg->cargo);
  // wrap-up...
  vm.vmachine_destruct();
  // before pet terminates it must send a signal indicating that core is free
  for (int i=0; i<sarg->ncontributors[sarg->mypet]; i++){
#if (VERBOSITY > 0)
    printf(" send wake-up signal to : %d %d\n",
      sarg->contributors[sarg->mypet][i].pid, 
      sarg->contributors[sarg->mypet][i].blocker_tid);
#endif
    // send signal to the _other_ process
    kill(sarg->contributors[sarg->mypet][i].pid, SIGUSR1);
    // which ever thread of the other process woke up will try to receive tid
    MPI_Send(&(sarg->contributors[sarg->mypet][i].blocker_tid),
      sizeof(pthread_t), MPI_BYTE, sarg->contributors[sarg->mypet][i].mpi_pid,
      VMACHINE_TID_MPI_TAG, vm.default_mpi_c);
  }
  // when returning from this procedure this pet will terminate
  return NULL;
}


static void *vmachine_sigcatcher(void *arg){
  // This is a signal catcher. Its job is to catch a signal from other processes
  // indicating that cores have become available again. When such a signal has
  // been received the sigcatcher will communicate with the signaling process
  // via MPI and receive the actual pthread_id that need to be awoken on this
  // process, which is actually a blocker thread which then will wrap up and 
  // by that indicate that the resource has been made available again.
  sigset_t sigs_to_catch;
  sigemptyset(&sigs_to_catch);
  sigaddset(&sigs_to_catch, SIGUSR1);
  int caught;
  pthread_t thread_wake;
  MPI_Status mpi_s;
  // suspend thread until a signal arrives
#if (VERBOSITY > 0)
  printf("I am a sigcatcher for pid %d and am going to sleep...\n", getpid());
#endif
  sigwait(&sigs_to_catch, &caught);
#if (VERBOSITY > 0)
  printf("I am a sigcatcher for pid %d and signal: %d woke me up...\n",
    getpid(), caught);
#endif
  // this signal was received from a thread running under another process
  // receive the thread id of the blocker thread that needs to be woken up
  vmachine vm;  // need a handle to access the MPI_Comm of default vmachine
  MPI_Recv(&thread_wake, sizeof(pthread_t), MPI_BYTE, MPI_ANY_SOURCE, 
    VMACHINE_TID_MPI_TAG, vm.default_mpi_c, &mpi_s);
  // now wake up the correct blocker thread within this pid
#if (VERBOSITY > 0)
  printf("It's the sigcatcher for pid %d again. I received the blocker tid\n"
    " and I'll wake up blocker thread with tid: %d\n", getpid(), thread_wake);
#endif
  pthread_kill(thread_wake, SIGUSR2);
  // this sigcatcher has done its job and is allowed to terminate...
  return NULL;
}


static void *vmachine_block(void *arg){
  // This blocker thread is responsible for staying alive until resources,
  // i.e. cores, become available to the contributing pet. The contributing
  // pet is blocked (asynchonously) via pthread_join(). This means it is o.k.
  // for the blocker thread to exit before the contributing pet ever makes it
  // to pthread_join()... 
  pthread_t pthid;
  // spawn of an independent sigcatcher that listens to other processes
  pthread_create(&pthid, NULL, vmachine_sigcatcher, (void *)NULL);
  pthread_detach(pthid);  // the sigcatcher is independet of the blocker thread
  // now set up and install USR2 signal to be catched by this blocker thread
  int caught;
  sigset_t sigs_to_catch;
  sigemptyset(&sigs_to_catch);
  sigaddset(&sigs_to_catch, SIGUSR2);
  // suspend this thread until awoken by one of the sigcatcher threads
#if (VERBOSITY > 0)
  printf("I am a blocker for pid %d, my tid is %d and going to sleep...\n",
    getpid(), pthread_self());
#endif
  sigwait(&sigs_to_catch, &caught);
#if (VERBOSITY > 0)
  printf("I am a blocker for pid %d, my tid is %d and\n"
    "woke up with signal: %d. I'll exit and therefore free block on my pet\n",
    getpid(), pthread_self(), caught);
#endif
  // once the signal has been received from a sigcatcher the blocker can 
  // terminate and with that free the actual pet which might be waiting in a
  // pthread_join() or get there later...
  return NULL;
}


void *vmachine::vmachine_enter(class vmplan &vmp, 
  void *(fctp)(vmachine &, void *), void *cargo){
  // enter a vm derived from current vm according to the vmplan
  // need as many spawn_args as there are threads to be spawned from this pet
  // this is so that each spawned thread does not have to be worried about this
  // info to disappear while still accessing it
  int at_least_1 = vmp.spawnflag[mypet];
  if (at_least_1 < 1)
    at_least_1 = 1;
  vmachine_spawn_arg *sarg = new vmachine_spawn_arg[at_least_1];
  // now:
  //    sarg[] has as many elements as mypet spawns threads, but at least one
  // next, allocate as many vm objects off the heap as there will be spawned
  // next, set pointers in sarg to the vmachine instances on the heap
  for (int i=0; i<vmp.spawnflag[mypet]; i++)
    sarg[i].myvm = vmp.myvms[i];
  // next, determine new_npets and new_mypet_base ...
  int new_mypet_base=0;
  int new_npets=0;
  for (int i=0; i<npets; i++){
    new_npets += vmp.spawnflag[i];
    if (i<mypet)
      new_mypet_base += vmp.spawnflag[i];
  }
  // now:
  //    new_npets is equal to the total number of pets in the new vmachine
  //    new_mypet_base is the index of the first new pet that mypet will spawn
  // next, allocate temporary arrays ...
  int *new_lpid = new int[new_npets];
  int *new_pid = new int[new_npets];
  int *new_tid = new int[new_npets];
  int *new_ncpet = new int[new_npets];
  int *new_ncontributors = new int[new_npets];
  int **new_cid = new int*[new_npets];
  contrib_id **new_contributors = new contrib_id*[new_npets];
  // important note for the new_commarray:
  // every PET that enters vmachine_enter(which are all of the currentvmachine)
  // will get a new_commarray allocated. However, only those PETs that actually
  // spawn will really use it and free it during vmachine_destruct. The 
  // superficial allocations need to be dealt with in vmachine_enter still to
  // prevent memory leaks.
  comminfo **new_commarray = new comminfo*[new_npets];
  for (int i=0; i<new_npets; i++){
    new_commarray[i] = new comminfo[new_npets];
    for (int j=0; j<new_npets; j++){
      // by default all communication is via MPI-1
      new_commarray[i][j].comm_type = VM_COMM_TYPE_MPI1;
    }
  }
  // local variables, unallocated yet...
  pthread_mutex_t *new_pth_mutex2;
  pthread_mutex_t *new_pth_mutex;
  int *new_pth_finish_count;
  // utility variables that will be used beyond the next i-loop
  int num_diff_pids=0;  // total number of different pids/lpids in new vmachine
  // utility arrays and variables used only during the next i-loop
  int *keep_max_tid = new int[npets]; // sum of threads that will be spawned
  int new_petid=0;      // used for keeping track of new_petid in loop
  // next, run through all current pets and check the vmplan ...
  // inside the following loop pet "i" will be refered to as "this pet"
  for (int i=0; i<npets; i++){
    // get the last max_tid count of a pet with same pid
    int local_tid = 0;
    for (int j=0; j<i; j++)
      if (pid[j]==pid[i])
        local_tid = keep_max_tid[j];
    // now:
    //    local_tid is the tid index for the first pet this pet might spawn
    // next, if this pet spawns determine whether a previous spaner had same pid
    int temp_lpid;
    if (vmp.spawnflag[i]){
      int j;
      // check all previous spawners
      for (j=0; j<new_petid; j++)
        if (new_pid[j]==pid[i]) break;
      if (j<new_petid){
        // found same pid in previous spawner
        temp_lpid=new_lpid[j];   // carry over the new_lpid determined before
      }else{
        // no previous spawner had that same pid
        temp_lpid=num_diff_pids; // new lpid for spawned pets from this pet
        ++num_diff_pids;  // increment the total number of different pids in new
      }
    }
    // now:
    //    temp_lpid is the lpid for all new pets spawned by this pet
    // next, handle this pet amd fill in info for _all_ the threads it spawns
    for (int j=0; j<vmp.spawnflag[i]; j++){
      // here j is the counter over threads this pet spawns
      new_lpid[new_petid]=temp_lpid;  // new lpid is that previously determined
      new_pid[new_petid]=pid[i];      // new pid is equal to that of this pet
      new_tid[new_petid]=local_tid;   // new tid is continuous count per pid
      // next, determine how many cores the new pet will have & its contributors
      new_ncpet[new_petid]=0;         // reset the counter
      new_ncontributors[new_petid]=0; // reset the counter
      for (int k=0; k<npets; k++)
        if (vmp.contribute[k]==i && vmp.cspawnid[k]==j){
          // pet k contributes to this pet's spawned thread number j
          new_ncpet[new_petid]+=ncpet[k]; // add in all the cores from pet k
          if (k!=i){
            // this contribution came from _another_ pet
            ++new_ncontributors[new_petid]; // increase count of contributors
          }
        }
      // now:
      //    new_lpid[new_petid] is valid lpid of new pet
      //    new_pid[new_petid] is valid pid of new pet
      //    new_tid[new_petid] is valid tid of new pet
      //    new_ncpet[new_petid] is valid number of cores held by new pet
      //    new_ncontributors[new_petid] is valid number of contributor pets
      // next, allocate and fill 2nd dimension of new_cid and new_contributors
      new_cid[new_petid] = new int[new_ncpet[new_petid]];
      new_contributors[new_petid] = 
        new contrib_id[new_ncontributors[new_petid]];
      int ncpet_counter=0;      // reset core counter
      int ncontrib_counter=0;   // reset contributor counter
      // loop over all current pets and see how they contribute tho this pet
      for (int k=0; k<npets; k++){
        if (vmp.contribute[k]==i && vmp.cspawnid[k]==j){
          // found a contributor pet (k) which contributes cores
          // to this pet (i) for its spawned thread (j)
          // next, determine the contrib_id of pet (k) and share info with pet i
          if (k!=i){
            // only if contributor pet (k) is different from this receiver pet i
            if (mypet==k){
              // mypet (k) contributes to this pet (i)
              // mypet does not spawn but contributes -> spawn blocker thread
              pthread_create(&(sarg[0].pthid), NULL, vmachine_block, 
                (void *)&sarg[0]);
              // fill in the info about mypet contibuting...
              new_contributors[new_petid][ncontrib_counter].blocker_tid =
                sarg[0].pthid;
              new_contributors[new_petid][ncontrib_counter].mpi_pid =
                pid[mypet];
              new_contributors[new_petid][ncontrib_counter].pid = getpid();
              new_contributors[new_petid][ncontrib_counter].tid =
                 pthread_self();
              // send contributor info over to this pet (i) that receives cores
#if (VERBOSITY > 0)
              printf("sending...\n");
#endif
              vmachine_send(&new_contributors[new_petid][ncontrib_counter],
                sizeof(contrib_id), i);
#if (VERBOSITY > 0)
              printf("send off contrib_id for later wake-up signal: \n"
                " blocker_tid: %d\n mpi_pid: %d\n pid: %d\n tid: %d\n",
                new_contributors[new_petid][ncontrib_counter].blocker_tid,
                new_contributors[new_petid][ncontrib_counter].mpi_pid,
                new_contributors[new_petid][ncontrib_counter].pid,
                new_contributors[new_petid][ncontrib_counter].tid);
#endif
            }else if (mypet==i){
              // mypet is this pet (i)-> receiver of cores from _another_ pet k
#if (VERBOSITY > 0)
              printf("receiving...\n");
#endif
              vmachine_recv(&new_contributors[new_petid][ncontrib_counter],
                sizeof(contrib_id), k);
#if (VERBOSITY > 0)
              printf("received contrib_id for later wake-up signal: \n"
                " blocker_tid: %d\n mpi_pid: %d\n pid: %d\n tid: %d\n",
                new_contributors[new_petid][ncontrib_counter].blocker_tid,
                new_contributors[new_petid][ncontrib_counter].mpi_pid,
                new_contributors[new_petid][ncontrib_counter].pid,
                new_contributors[new_petid][ncontrib_counter].tid);
#endif
            }else{
              // mypet has nothing to do with this contribution of cores
              new_contributors[new_petid][ncontrib_counter].mpi_pid = NULL;
              new_contributors[new_petid][ncontrib_counter].pid = NULL;
              new_contributors[new_petid][ncontrib_counter].tid = NULL;
            }
            ++ncontrib_counter;   // increment the counter of contributing pets
          }
          // fill in the cores that will be contributed
          for (int l=0; l<ncpet[k]; l++){
            new_cid[new_petid][ncpet_counter]=cid[k][l];
            ++ncpet_counter;
          }
        }
      }
      // advance to the next new thread (j) spawned by current pet (i)
      // which will become pet (new_petid) in new vmachine
      ++new_petid;
      ++local_tid;
    }
    // keep record of how high local_tid counted for the pid of this pet
    keep_max_tid[i] = local_tid;
  }
  // collect garbage of temporary arrays from previous i-loop
  delete [] keep_max_tid;
  // now:
  //    new_lpid[new_petid] is valid lpid of new pet
  //    new_pid[new_petid] is valid pid of new pet
  //    new_tid[new_petid] is valid tid of new pet
  //    new_ncpet[new_petid] is valid number of cores held by new pet
  //    new_ncontributors[new_petid] is valid number of contributor pets
  //    new_cid[new_petid][] holds valid core indices
  //    new_contributors[new_petid][] holds valid contrib_id's for pairs
  //    num_diff_pids is the total number of different pids in new vmachine
#if (VERBOSITY > 0)
  printf(">>>>>>>>> num_diff_pids for new vmachine = %d\n", num_diff_pids);
#endif
  //
  // next, set up temporary arrays lpid_list and pet_list to facititate 
  // MPI_Comm creation and shared memory allocation for new vmachine
  int **lpid_list = new int*[2];
  lpid_list[0] = new int[num_diff_pids]; // this dimension holds lpids
  lpid_list[1] = new int[num_diff_pids]; // this dimension holds number of pets
  int **pet_list = new int*[num_diff_pids];
  for (int i=0; i<num_diff_pids; i++){
    lpid_list[0][i] = -1;  // invalidate the lpid entry
    lpid_list[1][i] = 0;   // no pets associated yet
    pet_list[i] = new int[npets];  // npets is maximum possible number here!
  }
  for (int i=0; i<npets; i++){
    if (vmp.spawnflag[i]){
      // this pet will spawn, so look if its lpid has already been recorded
      int j;
      for (j=0; j<num_diff_pids; j++)
        if (lpid_list[0][j]==lpid[i] || lpid_list[0][j]==-1) break;
      lpid_list[0][j] = lpid[i];  // store lpid (does not matter to overwrite)
      pet_list[j][lpid_list[1][j]] = i;  //enter the current pet id
      ++lpid_list[1][j];         // increment pet count for this pid
    }
  }
  // now:
  //    lpid_list[0][] list of current lpids with at least one pet spawning
  //    lpid_list[1][] associated list indicating how many pets will spawn
  //    pet_list[][] list of current pets that spawn
  // next, create MPI group
  // since this is a local MPI operation each pet can call this here...
  MPI_Group new_mpi_g;
  MPI_Group_incl(mpi_g, num_diff_pids, lpid_list[0], &new_mpi_g);
  // setting up MPI communicators is a collective MPI communication call
  // thus it requires that exactly one pet of each process running in the 
  // current vmachine makes that call, even if this process will not participate
  // in the new vmachine...
  MPI_Comm new_mpi_c;
  for (int first_lpid=0; first_lpid<=lpid[npets-1]; first_lpid++){
    // find the first pet whose lpid is qual to first_lpid
    int i;
    for (i=0; i<npets; i++)
      if (lpid[i]==first_lpid) break;
#if (VERBOSITY > 0)
    printf("pet %d of current vmachine is first one with lpid=%d\n", i,lpid[i]);
#endif
    if (mypet==i){
      // mypet is the first one with this lpid, thus call MPI_Comm_create
#if (VERBOSITY > 0)
      printf("... and mypet is %d, thus calling MPI_Comm_create\n", mypet);
#endif
      MPI_Comm_create(mpi_c, new_mpi_g, &new_mpi_c);
      // check and see whether pets with this lpid will spawn threads
      int j;
      for (j=0; j<num_diff_pids; j++)
        if (lpid_list[0][j]==first_lpid) break;
      if (j<num_diff_pids){
        // found first_lpid in lpid_list, the list of lpids with at least one
        // pet of the current vmachine that spawns threads for the new vmachine
        // next loop over all pet that spawn threads with this lpid
        for(int k=0; k<lpid_list[1][j]; k++){
          if (pet_list[j][k]!=mypet){
            // send MPI_Comm to all the other pets of the current vmachine which
            // spawn threads under the same lpid
#if (VERBOSITY > 0)
            printf("mypet %d sends new_mpi_c to pet %d\n",mypet,pet_list[j][k]);
#endif
            vmachine_send(&new_mpi_c, sizeof(MPI_Comm), pet_list[j][k]);
          }
        }
      }
    }else if (lpid[mypet]==first_lpid){
      // mypet is not the first one for lpid, but has the same lpid association
#if (VERBOSITY > 0)
      printf("mypet %d is not first thread to run under this pid\n", mypet);
#endif
      // check and see whether pets with this lpid will spawn threads
      int j;
      for (j=0; j<num_diff_pids; j++)
        if (lpid_list[0][j]==first_lpid) break;
      if (j<num_diff_pids){
        // found first_lpid in lpid_list, the list of lpids with at least one
        // pet of the current vmachine that spawns threads for the new vmachine
        // next loop over all pet that spawn threads with this lpid
        for(int k=0; k<lpid_list[1][j]; k++){
          if (pet_list[j][k]==mypet){
            // receive the MPI_comm from the pet which created it
#if (VERBOSITY > 0)
            printf("mypet %d recvs new_mpi_c frm %d\n", mypet, i);
#endif
            vmachine_recv(&new_mpi_c, sizeof(MPI_Comm), i);
            // debug output to see what we got...
            int rank, size;
            MPI_Comm_rank(new_mpi_c, &rank);
            MPI_Comm_size(new_mpi_c, &size);
#if (VERBOSITY > 0)
            printf("mypet = %d, new_mpi_c: rank: %d, size: %d\n", mypet, 
                rank, size);
#endif
          }
        }
      }
    }
  }
  // now:
  //    new_mpi_g is the valid MPI_Group for the new vmachine
  //    new_mpi_c is the valid MPI_Comm for the new vmachine
  //
  // Next, setting up intra-process shared memory connection between
  // qualifying pets of the new vmachine. Only one of the current pets that
  // spawn for a certain lpid must allocate memory for the shared variables 
  // and then send this info to all the associated intra- or inter-process pets
  // of the current vmachine which also spawn threads.
  for (int i=0; i<num_diff_pids; i++){
    // consider all of the different lpids of current vmachine
    if (lpid_list[0][i]>-1){
      // at least one pet of the current vmachine with this lpid will spawn
#if (VERBOSITY > 0)
      printf("setting up shared memory variables for lpid=%d\n", 
        lpid_list[0][i]);
#endif
      // allocate and initialize memory 
      if (mypet==pet_list[i][0]){
        // mypet is the first in the list of those that spawn from this lpid
        // -> will allocate shared memory variables
#if (VERBOSITY > 0)
        printf("mypet is first one for lpid -> allocating shared memory\n");
#endif
        new_pth_mutex2 = new pthread_mutex_t;
        new_pth_mutex = new pthread_mutex_t;
        new_pth_finish_count = new int;
        // initialize shared variables
        pthread_mutex_init(new_pth_mutex2, NULL);
        pthread_mutex_init(new_pth_mutex, NULL);
        *new_pth_finish_count = 0;
        // set up the shared_mp structure within the commarray
        for (int pet1=0; pet1<new_npets; pet1++){
          for (int pet2=0; pet2<new_npets; pet2++){
            if (pet1!=pet2 && new_pid[pet1]==new_pid[pet2] 
                && new_pid[pet1]==pid[mypet]){
              // pet1 and pet2 will be threads under the same process as mypet
              // for now hardcode VM_COMM_TYPE_SHMHACK for intra-process comm.
              // -> allocate shared_mp structure 
              new_commarray[pet1][pet2].shmp = new shared_mp;
              if (vmp.pref_intra_process == PREF_INTRA_PROCESS_SHMHACK){
                new_commarray[pet1][pet2].comm_type = VM_COMM_TYPE_SHMHACK;
                // initialize the shms structure in shared_mp
                sync_reset(&(new_commarray[pet1][pet2].shmp->shms));
              }else if(vmp.pref_intra_process == PREF_INTRA_PROCESS_PTHREAD){
                new_commarray[pet1][pet2].comm_type = VM_COMM_TYPE_PTHREAD;
                // initialize pthread variables in shared_mp
                pthread_mutex_init(&(new_commarray[pet1][pet2].shmp->mutex1),
                  NULL);
                pthread_cond_init(&(new_commarray[pet1][pet2].shmp->cond1),
                  NULL);
                pthread_mutex_init(&(new_commarray[pet1][pet2].shmp->mutex2),
                  NULL);
                pthread_cond_init(&(new_commarray[pet1][pet2].shmp->cond2),
                  NULL);
                new_commarray[pet1][pet2].shmp->tcounter = 0;
                // also initialize the shms structure in shared_mp for barrier()
                sync_reset(&(new_commarray[pet1][pet2].shmp->shms));
              }
            }
          }
        }
      }
      // share pointers with all current pets that also spawn for same pid/lpid
      for (int j=1; j<lpid_list[1][i]; j++){
        int pet_dest = pet_list[i][j];
        int pet_src = pet_list[i][0];
        if (mypet==pet_src){
          // mypet is the first pet in the list -> mypet allocated -> must send
          vmachine_send(&new_pth_mutex2, sizeof(pthread_mutex_t*), pet_dest);
          vmachine_send(&new_pth_mutex, sizeof(pthread_mutex_t*), pet_dest);
          vmachine_send(&new_pth_finish_count, sizeof(int*), pet_dest);
          vmachine_send(&new_commarray, sizeof(comminfo**), pet_dest);
        }else if(mypet==pet_dest){
          // mypet is one of the pets that also spawn for this lpid -> receive
          // before this PETs new_commarray is overwritten it must be deleted
          for (int ii=0; ii<new_npets; ii++)
            delete [] new_commarray[ii];
          delete [] new_commarray;
          // now this PET is ready to receive the pointers for shared variables
          vmachine_recv(&new_pth_mutex2, sizeof(pthread_mutex_t*), pet_src);
          vmachine_recv(&new_pth_mutex, sizeof(pthread_mutex_t*), pet_src);
          vmachine_recv(&new_pth_finish_count, sizeof(int*), pet_src);
          vmachine_recv(&new_commarray, sizeof(comminfo**), pet_src);
        }
      }
    }
  }
  // now:
  //    new_pth_mutex2 is valid pthread_mutex
  //    new_pth_mutex is valid pthread_mutex
  //    new_pth_finish_count is valid shared memory counter
  //    new_commarray now holds valid shared memory shared_mp objects
  //
  // next, enter the spawn-loop for mypet 
  for (int i=0; i<vmp.spawnflag[mypet]; i++){
    // copy this threads information into the sarg structure
    sarg[i].fctp = fctp;
    sarg[i].mypet = new_mypet_base + i;   // different for each thread spawned
    sarg[i].npets = new_npets;
    sarg[i].lpid = new int[new_npets];
    sarg[i].pid = new int[new_npets];
    sarg[i].tid = new int[new_npets];
    sarg[i].ncpet = new int[new_npets];
    sarg[i].cid = new int*[new_npets];
    sarg[i].ncontributors = new int[new_npets];
    sarg[i].contributors = new contrib_id*[new_npets];
    for (int j=0; j<new_npets; j++){
      sarg[i].lpid[j] = new_lpid[j];
      sarg[i].pid[j] = new_pid[j];
      sarg[i].tid[j] = new_tid[j];
      sarg[i].ncpet[j] = new_ncpet[j];
      sarg[i].cid[j] = new int[new_ncpet[j]];
      for (int k=0; k<new_ncpet[j]; k++)
        sarg[i].cid[j][k] = new_cid[j][k];
      sarg[i].ncontributors[j]=new_ncontributors[j];
      sarg[i].contributors[j] = new contrib_id[new_ncontributors[j]];
      for (int k=0; k<new_ncontributors[j]; k++)
        sarg[i].contributors[j][k] = new_contributors[j][k];
    }
    sarg[i].mpi_g = new_mpi_g;
    sarg[i].mpi_c = new_mpi_c;
    sarg[i].pth_mutex2 = new_pth_mutex2;
    sarg[i].pth_mutex = new_pth_mutex;
    sarg[i].pth_finish_count = new_pth_finish_count;
    sarg[i].commarray = new_commarray;
    sarg[i].pref_intra_ssi = vmp.pref_intra_ssi;
    // cargo
    sarg[i].cargo = cargo;
    // finally spawn of threads from this pet...
    pthread_create(&(sarg[i].pthid), NULL, vmachine_spawn, (void *)&sarg[i]);
  }
  // free all the temporary arrays.... (not sarg array!!!)
  delete [] new_lpid;
  delete [] new_pid;
  delete [] new_tid;
  delete [] new_ncpet;
  delete [] new_ncontributors;
  for (int i=0; i<new_npets; i++){
    delete [] new_cid[i];
    delete [] new_contributors[i];
  }
  delete [] new_cid;
  delete [] new_contributors;
  delete [] lpid_list[0];
  delete [] lpid_list[1];
  delete [] lpid_list;
  for (int i=0; i<num_diff_pids; i++)
    delete [] pet_list[i];
  delete [] pet_list;
  if (vmp.spawnflag[mypet]==0){
    // mypet does not spawn an thus new_commarray needs to be deleted here
    for (int ii=0; ii<new_npets; ii++)
      delete [] new_commarray[ii];
    delete [] new_commarray;
  }
  // return info that is associated with the new vmachine...
  return sarg;
}


void vmachine::vmachine_exit(class vmplan &vmp, void *arg){
  // Block all pets of the current vmachine until their individual resources,
  // i.e. cores, become available. This means:
  //  1) pets which did not spawn nor contribute in the vmplan are not blocked
  //  2) pets which did not spawn but contributed in the vmplan block until
  //     their cores have been returned by spwawned vmachine
  //  3) pets that spwan will be blocked until _all_ of _their_ spawned threads
  //     exit in order to delete allocated info structure
  // First need to cast arg into its correct type
  vmachine_spawn_arg *sarg = (vmachine_spawn_arg *)arg;
  // pets that spawned will be blocked by pthread_join() call...
  for (int i=0; i<vmp.spawnflag[mypet]; i++){
    pthread_join(sarg[i].pthid, NULL);
    // free arrays in sarg[i[ 
    delete [] sarg[i].lpid;
    delete [] sarg[i].pid;
    delete [] sarg[i].tid;
    delete [] sarg[i].ncpet;
    delete [] sarg[i].ncontributors;
    for (int j=0; j<sarg[i].npets; j++){
      delete [] sarg[i].cid[j];
      delete [] sarg[i].contributors[j];
    }
    delete [] sarg[i].cid;
    delete [] sarg[i].contributors;
  }
  // need to block pets that did not spawn but contributed in the vmplan
  if (vmp.spawnflag[mypet]==0 && vmp.contribute[mypet]>-1){
    // wait for the blocker thread to return, maybe already is done...
#if (VERBOSITY > 0)
    printf("I am pet, pid %d, tid %d. I'll block on a pthread_join until "
      "blocker returns.\n", getpid(), pthread_self());
#endif
    pthread_join(sarg[0].pthid, NULL);
#if (VERBOSITY > 0)
    printf("I am pet, pid %d, tid %d. My blocker returned\n", 
      getpid(), pthread_self());
#endif
  }
  // done holding info in vmachine_spawn_arg array -> delete now
  delete [] sarg;
  // done blocking...
}


void vmachine::vmachine_print(void){
  // print info about the vmachine object
  printf("--- vmachine_print start ---\n");
  printf("npets = %d, mypet=%d\n", npets, mypet);
  printf("  pth_mutex =\t\t %p\n"
         "  pth_finish_count =\t %p\n"
         "  commarray =\t\t %p\n", 
    pth_mutex, pth_finish_count, commarray);
  for (int i=0; i<npets; i++){
    printf("  lpid[%d]=%d, pid[%d]=%d, tid[%d]=%d, ncpet[%d]=%d",
      i, lpid[i], i, pid[i], i, tid[i], i, ncpet[i]);
    for (int j=0; j<ncpet[i]; j++)
      printf(", cid[%d][%d]=%d", i, j, cid[i][j]);
    printf("\n");
  }
  printf("ncores = %d\n", ncores);
  for (int i=0; i<ncores; i++)
    printf("  cpuid[%d]=%d, ssiid[%d]=%d\n", i, cpuid[i], i, ssiid[i]);
  printf("--- vmachine_print end ---\n");
}


int vmachine::vmachine_npets(void){
  return npets;
}


int vmachine::vmachine_mypet(void){
  return mypet;
}


int vmachine::vmachine_ncpet(int i){
  return ncpet[i];
}


int vmachine::vmachine_ssiid(int i){
  return ssiid[cid[i][0]];
}


MPI_Comm vmachine::vmachine_mpi_comm(void){
  return mpi_c;
}


int vmachine::vmachine_nthreads(int i){
  int n=0;
  for (int j=0; j<npets; j++)
    if (pid[j]==pid[i]) ++n;
  return n;
}


int vmachine::vmachine_tid(int i){
  return tid[i];
}

// --- vmplan methods ---


vmplan::vmplan(void){
  // native constructor
  // invalidate the arrays
  spawnflag = NULL;
  contribute = NULL;
  cspawnid = NULL;
  // invalidate the vmachine pointer array
  myvms = NULL;
  // set the default communication preferences
  pref_intra_process = PREF_INTRA_PROCESS_SHMHACK;
  pref_intra_ssi = PREF_INTRA_SSI_MPI1;
  pref_inter_ssi = PREF_INTER_SSI_MPI1;
}


vmplan::~vmplan(void){
  // native destructor
  vmplan_garbage();
}

  
void vmplan::vmplan_garbage(void){
  // perform garbage collection within a vmplan object
  if (spawnflag != NULL){
    delete [] spawnflag;
    delete [] contribute;
    delete [] cspawnid;
    spawnflag = NULL;
    contribute = NULL;
    cspawnid = NULL;
  }
  if (myvms != NULL){
    for (int i=0; i<nspawn; i++)
      delete myvms[i];
    delete [] myvms;
  }
}


void vmplan::vmplan_maxthreads(vmachine &vm){
  // set up a vmplan that will maximize the number of thread-pets
  vmplan_maxthreads(vm, 0);
}


void vmplan::vmplan_maxthreads(vmachine &vm, int max){
  // set up a vmplan that will maximize the number of thread-pets up to max
  vmplan_maxthreads(vm, max, NULL, 0);
}


void vmplan::vmplan_maxthreads(vmachine &vm, int max,
  int pref_intra_process, int pref_intra_ssi, int pref_inter_ssi){
  // set up a vmplan that will maximize the number of thread-pets up to max
  vmplan_maxthreads(vm, max, NULL, 0, 
    pref_intra_process, pref_intra_ssi, pref_inter_ssi);
}


void vmplan::vmplan_maxthreads(vmachine &vm, int max, int *plist, int nplist){
  // set up a vmplan that will maximize the number of thread-pets up to max
  // but only allow PETs listed in plist to participate
  // first do garbage collection on current object
  vmplan_garbage();
  // now set stuff up...
  npets = vm.npets;
  spawnflag = new int[npets];
  contribute = new int[npets];
  cspawnid = new int[npets];
  // set up a table for ssiid-to-petid mapping and invalidate all entries
  int *issiid = new int[npets]; // can only have as many different ssi as pets
  int *nssiid = new int[npets]; // can only have as many different ssi as pets
  for (int i=0; i<npets; i++)
    issiid[i]=-1;
  // loop over pets and set spawnflag
  for(int i=0; i<npets; i++){
    if (nplist != 0){
      int j;
      for (j=0; j<nplist; j++)
        if (plist[j]==i) break;
      if (j==nplist){
        contribute[i]=-1;   // don't contribute any cores
        cspawnid[i]=-1;     // invalidate
        continue;
      }
    }
    if (issiid[vm.ssiid[vm.cid[i][0]]]==-1){
      // new ssiid
      spawnflag[i]=1;     // spawn at least one thread in new virtual machine   
      contribute[i]=i;    // contribute cores to itself
      cspawnid[i]=0;      // contribute cores to itself
      issiid[vm.ssiid[vm.cid[i][0]]]=i;   // need this pets id for later
      nssiid[vm.ssiid[vm.cid[i][0]]]=1;   // count how many threads for this ssi
    }else{
      // same ssiid
      if (max <= 0 || nssiid[vm.ssiid[vm.cid[i][0]]] < max){
        // maximum degree of process multi-threading not yet reached
        ++nssiid[vm.ssiid[vm.cid[i][0]]]; // increment counter
        if (vm.pid[i]==vm.pid[issiid[vm.ssiid[vm.cid[i][0]]]]){
          // a previous pet with the same pid has been found before, but in 
          // order to minimize communications required on exit we'll have each
          // thread spawn...
          spawnflag[i]=1;
          contribute[i]=i;   // contribute cores to itself
          cspawnid[i]=0;     // contribute cores to itself
        }else{
          // so this was a pet that runs under a different process than a 
          // pet we found before on the same SSI, thus it shall not spawn
          // but contribute its cores to the previously found pet
          spawnflag[i]=0;
          contribute[i]=issiid[vm.ssiid[vm.cid[i][0]]];  // contribute cores
          cspawnid[i]=spawnflag[issiid[vm.ssiid[vm.cid[i][0]]]]; // to  pet
          ++spawnflag[issiid[vm.ssiid[vm.cid[i][0]]]];   // increment spawnflag
        }
      }else{
        // maximum number of threads per process reached -> start over
        nssiid[vm.ssiid[vm.cid[i][0]]]=0; // reset
        if (vm.pid[i]==vm.pid[issiid[vm.ssiid[vm.cid[i][0]]]]){
          // a previous pet with the same pid has been found before, 
          // therefore I cannot do anything with this pet...
          spawnflag[i]=0;     // don't spawn from this pet
          contribute[i]=-1;   // don't contribute any cores
          cspawnid[i]=-1;     // invalidate
        }else{
          // pet on same SSI but under new process -> make this process master
          spawnflag[i]=1;     // spawn at least one thread in new vmachine
          contribute[i]=i;    // contribute cores to itself
          cspawnid[i]=0;      // contribute cores to itself
          issiid[vm.ssiid[vm.cid[i][0]]]=i;   // need this pets id for later
          nssiid[vm.ssiid[vm.cid[i][0]]]=1;   // count threads for this ssi
        }
      }
    }
  }
  delete [] issiid;
  delete [] nssiid;
  // now deal with mypet specific members
  nspawn = spawnflag[vm.mypet];
  myvms = new vmachine*[nspawn];
  for (int i=0; i<nspawn; i++)
    myvms[i] = new vmachine;
}


void vmplan::vmplan_maxthreads(vmachine &vm, int max, int *plist, int nplist,
  int pref_intra_process, int pref_intra_ssi, int pref_inter_ssi){
  // set the communication preferences
  if (pref_intra_process >= 0)
    this->pref_intra_process = pref_intra_process;
  if (pref_intra_ssi >= 0)
    this->pref_intra_ssi = pref_intra_ssi;
  if (pref_inter_ssi >= 0)
    this->pref_inter_ssi = pref_inter_ssi;
  vmplan_maxthreads(vm, max, plist, nplist);
}


void vmplan::vmplan_minthreads(vmachine &vm){
  // set up a vmplan that will only have single threaded pet instantiations
  // and claim all cores of pets that don't make it through
  vmplan_minthreads(vm, 0);
}


void vmplan::vmplan_minthreads(vmachine &vm, int max){
  // set up a vmplan that will only have single threaded pet instantiations
  // and claim all cores of pets that don't make it through, up to max cores/pet
  vmplan_minthreads(vm, max, NULL, 0);
}


void vmplan::vmplan_minthreads(vmachine &vm, int max, int *plist, int nplist){
  // set up a vmplan that will only have single threaded pet instantiations
  // and claim all cores of pets that don't make it through, up to max cores/pet
  // but only allow PETs listed in plist to participate
  // first do garbage collection on current object
  vmplan_garbage();
  // now set stuff up...
  npets = vm.npets;
  spawnflag = new int[npets];
  contribute = new int[npets];
  cspawnid = new int[npets];
  // need temporary array to hold core count for each lpid
  int *core_count = new int[vm.npets];        // maximum lpid is <= npets
  int *first_pet_index = new int[vm.npets];   // maximum lpid is <= npets
  // loop over pets and set spawnflag
  for(int i=0; i<npets; i++){
    if (nplist != 0){
      int j;
      for (j=0; j<nplist; j++)
        if (plist[j]==i) break;
      if (j==nplist){
        contribute[i]=-1;   // don't contribute any cores
        cspawnid[i]=-1;     // invalidate
        continue;
      }
    }
    if (vm.tid[i]==0){
      // only pass pets with tid 0
      spawnflag[i]=1;     // spawn one thread in new virtual machine from pet
      contribute[i]=i;    // contribute cores to itself
      cspawnid[i]=0;      // contribute cores to itself
      // record the number of cores that this pet brings in for this lpid
      core_count[vm.lpid[i]]=vm.ncpet[i];
      first_pet_index[vm.lpid[i]]=i;
    }else{
      // all other pets don't spawn, ...
      spawnflag[i]=0;     // don't spawn from this pet
      // ... but might contribute
      if (core_count[vm.lpid[i]] < max){
        // maximum number of cores not yet reached -> contribute all cores
        contribute[i]=first_pet_index[vm.lpid[i]];
        cspawnid[i]=0;      // contribute to the single-threaded pet spawned
        core_count[vm.lpid[i]]+=vm.ncpet[i]; // add this pet's cores to count
      }else{
        // maximum number of cores per pet has been reached -> don't contribute
        contribute[i]=-1;   // don't contribute any cores
        cspawnid[i]=-1;     // invalidate
      }
    }
  }
  delete [] core_count;
  delete [] first_pet_index;
  // now deal with mypet specific members
  nspawn = spawnflag[vm.mypet];
  myvms= new vmachine*[nspawn];
  for (int i=0; i<nspawn; i++)
    myvms[i] = new vmachine;
}


void vmplan::vmplan_minthreads(vmachine &vm, int max, int *plist, int nplist,
  int pref_intra_process, int pref_intra_ssi, int pref_inter_ssi){
  // set the communication preferences
  if (pref_intra_process >= 0)
    this->pref_intra_process = pref_intra_process;
  if (pref_intra_ssi >= 0)
    this->pref_intra_ssi = pref_intra_ssi;
  if (pref_inter_ssi >= 0)
    this->pref_inter_ssi = pref_inter_ssi;
  vmplan_minthreads(vm, max, plist, nplist);
}


void vmplan::vmplan_maxcores(vmachine &vm){
  // set up a vmplan that will have pets with the maximum number of cores
  // available
  vmplan_maxcores(vm, 0);
}


void vmplan::vmplan_maxcores(vmachine &vm, int max){
  // set up a vmplan that will have pets with the maximum number of cores
  // available, but not more than max
  vmplan_maxcores(vm, max, NULL, 0);
}


void vmplan::vmplan_maxcores(vmachine &vm, int max, int *plist, int nplist){
  // set up a vmplan that will have pets with the maximum number of cores
  // available, but not more than max and only use PETs listed in plist
  // first do garbage collection on current object
  vmplan_garbage();
  // now set stuff up...
  npets = vm.npets;
  spawnflag = new int[npets];
  contribute = new int[npets];
  cspawnid = new int[npets];
  // set up a table for ssiid-to-petid mapping and invalidate all entries
  int *issiid = new int[npets]; // can only have as many different ssi as pets
  int *nssiid = new int[npets]; // can only have as many different ssi as pets
  for (int i=0; i<npets; i++)
    issiid[i]=-1;
  // loop over pets and set spawnflag
  for(int i=0; i<npets; i++){
    if (nplist != 0){
      int j;
      for (j=0; j<nplist; j++)
        if (plist[j]==i) break;
      if (j==nplist){
        contribute[i]=-1;   // don't contribute any cores
        cspawnid[i]=-1;     // invalidate
        continue;
      }
    }
    if (issiid[vm.ssiid[vm.cid[i][0]]]==-1){
      // new ssiid
      spawnflag[i]=1;     // spawn at least one thread in new virtual machine   
      contribute[i]=i;    // contribute cores to itself
      cspawnid[i]=0;      // contribute cores to itself
      issiid[vm.ssiid[vm.cid[i][0]]]=i;   // need this pets id for later
      nssiid[vm.ssiid[vm.cid[i][0]]]=vm.ncpet[i]; // count how many cores
    }else{
      // same ssiid
      if (max <= 0 || nssiid[vm.ssiid[vm.cid[i][0]]] < max){
        // maximum degree of cores per pet not yet reached
        nssiid[vm.ssiid[vm.cid[i][0]]]+=vm.ncpet[i]; // increment counter
        // contribute cores to the previously found pet
        spawnflag[i]=0;
        contribute[i]=issiid[vm.ssiid[vm.cid[i][0]]];  // contribute cores
        cspawnid[i]=0;                                 // to  pet
      }else{
        // maximum number of cores per pet reached -> start over
        nssiid[vm.ssiid[vm.cid[i][0]]]=0; // reset
        // start spawning from this pet
        spawnflag[i]=1;     // spawn
        contribute[i]=i;    // contribute core to itself
        cspawnid[i]=0 ;     // contribute core to itself
        issiid[vm.ssiid[vm.cid[i][0]]]=i;   // need this pets id for later
        nssiid[vm.ssiid[vm.cid[i][0]]]=vm.ncpet[i]; // count how many cores
      }
    }
  }
  delete [] issiid;
  delete [] nssiid;
  // now deal with mypet specific members
  nspawn = spawnflag[vm.mypet];
  myvms = new vmachine*[nspawn];
  for (int i=0; i<nspawn; i++)
    myvms[i] = new vmachine;
}


void vmplan::vmplan_maxcores(vmachine &vm, int max, int *plist, int nplist,
  int pref_intra_process, int pref_intra_ssi, int pref_inter_ssi){
  // set the communication preferences
  if (pref_intra_process >= 0)
    this->pref_intra_process = pref_intra_process;
  if (pref_intra_ssi >= 0)
    this->pref_intra_ssi = pref_intra_ssi;
  if (pref_inter_ssi >= 0)
    this->pref_inter_ssi = pref_inter_ssi;
  vmplan_maxcores(vm, max, plist, nplist);
}


void vmplan::vmplan_print(void){
  // print info about the vmplan object
  printf("--- vmplan_print start ---\n");
  printf("npets = %d\n", npets);
  for (int i=0; i<npets; i++)
    printf("  spawnflag[%d]=%d, contribute[%d]=%d, cspawnid[%d]=%d\n", 
      i, spawnflag[i], i, contribute[i], i, cspawnid[i]);
  printf("pref_intra_process:\t%d\n", pref_intra_process);
  printf("pref_intra_ssi:\t%d\n", pref_intra_ssi);
  printf("pref_inter_ssi:\t%d\n", pref_inter_ssi);
  printf("--- vmplan_print end ---\n");
}


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~ Communication Calls
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


void vmachine::vmachine_send(void *message, int size, int dest){
  // p2p send
#if (VERBOSITY > 0)
  printf("sending to: %d, %d\n", dest, lpid[dest]);
#endif
  shared_mp *shmp;
  pipc_mp *pipcmp;
  int scpsize;
  char *pdest;
  char *psrc;
  int i;
  char *mess;
  // switch into the appropriate implementation
  switch(commarray[mypet][dest].comm_type){
  case VM_COMM_TYPE_MPI1:
    // MPI-1 implementation
    // not sure if I need to make this atomic if called multi-threaded?
    //pthread_mutex_lock(pth_mutex);
    MPI_Send(message, size, MPI_BYTE, lpid[dest], 1000*mypet+dest, mpi_c);
    //pthread_mutex_unlock(pth_mutex);
    break;
  case VM_COMM_TYPE_PTHREAD:
    // Pthread implementation
    shmp = commarray[mypet][dest].shmp;  // shared memory mp channel
    shmp->ptr_src = message;                        // set the source pointer
    // synchronize with vmachine_recv()
    pthread_mutex_lock(&(shmp->mutex1));
    shmp->tcounter++;
    if (shmp->tcounter < 2){
      // wait for vmachine_recv()
      pthread_cond_wait(&(shmp->cond1), &(shmp->mutex1));
    }else{
      // reset counter and wake up vmachine_recv()
      shmp->tcounter = 0;
      pthread_cond_broadcast(&(shmp->cond1));
    }
    pthread_mutex_unlock(&(shmp->mutex1));
    // now ptr_src and ptr_dest are valid for this message
    scpsize = size/2;   // send takes the lower half
    pdest = (char *)shmp->ptr_dest;
    psrc = (char *)shmp->ptr_src;
    // do the actual memcpy
    memcpy(pdest, psrc, scpsize);
    // synchronize with vmachine_recv()
    pthread_mutex_lock(&(shmp->mutex2));
    shmp->tcounter++;
    if (shmp->tcounter < 2){
      // wait for vmachine_recv()
      pthread_cond_wait(&(shmp->cond2), &(shmp->mutex2));
    }else{
      // reset counter and wake up vmachine_recv()
      shmp->tcounter = 0;
      pthread_cond_broadcast(&(shmp->cond2));
    }
    pthread_mutex_unlock(&(shmp->mutex2));
    break;
  case VM_COMM_TYPE_SHMHACK:
    // Shared memory hack sync with spin-lock
    shmp = commarray[mypet][dest].shmp;  // shared memory mp channel
    if (size<=SHARED_BUFFER){
      // use buffer
      pdest = shmp->buffer;
      // wait until buffer is ready to be used
      sync_buffer_wait_empty(&shmp->shms, 0);
      // do the actual memcpy
      memcpy(pdest, message, size);
      // set flag indicating that send's memcpy() is done and buffer is valid
      sync_buffer_flag_fill(&shmp->shms, 0);
    }else{
      // don't use buffer
      shmp->ptr_src = message;                        // set the source pointer
      // synchronize with vmachine_recv()
      sync_a_flip(&shmp->shms);
      // now ptr_src and ptr_dest are valid for this message
      scpsize = size/2;   // send takes the lower half
      pdest = (char *)shmp->ptr_dest;
      psrc = (char *)shmp->ptr_src;
      // do the actual memcpy
      memcpy(pdest, psrc, scpsize);
      // synchronize with vmachine_recv()
      sync_a_flop(&shmp->shms);
    }
    break;
  case VM_COMM_TYPE_POSIXIPC:
    // Shared memory hack sync with spin-lock
    pipcmp = commarray[mypet][dest].pipcmp;  // shared memory mp channel
    i=0;
    mess = (char *)message;
    while (size>PIPC_BUFFER){
      pdest = pipcmp->buffer[i];
      // wait until buffer is ready to be used
      sync_buffer_wait_empty(&pipcmp->shms, i);
      // do the actual memcpy
      memcpy(pdest, mess, PIPC_BUFFER);
      // set flag indicating that send's memcpy() is done and buffer is valid
      sync_buffer_flag_fill(&pipcmp->shms, i);
      size -= PIPC_BUFFER;
      mess += PIPC_BUFFER;
      i = i^1; // 0->1  or  1->0
    }
    // do the remaining parts of the message
    pdest = pipcmp->buffer[i];
    // wait until buffer is ready to be used
    sync_buffer_wait_empty(&pipcmp->shms, i);
    // do the actual memcpy
    memcpy(pdest, mess, size);
    // set flag indicating that send's memcpy() is done and buffer is valid
    sync_buffer_flag_fill(&pipcmp->shms, i);
    break;
  default:
    break;
  }
}


void vmachine::vmachine_recv(void *message, int size, int source){
  // p2p recv
#if (VERBOSITY > 0)
  printf("receiving from: %d, %d\n", source, lpid[source]);
#endif
  pipc_mp *pipcmp;
  shared_mp *shmp;
  int scpsize, rcpsize;
  char *pdest;
  char *psrc;
  int i;
  char *mess;
  // switch into the appropriate implementation
  switch(commarray[source][mypet].comm_type){
  case VM_COMM_TYPE_MPI1:
    // MPI-1 implementation
    // not sure if I need to make this atomic if called multi-threaded
    //pthread_mutex_lock(pth_mutex2);
    MPI_Status mpi_s;
    MPI_Recv(message, size, MPI_BYTE, lpid[source], 1000*source+mypet, 
      mpi_c, &mpi_s);
    //pthread_mutex_unlock(pth_mutex2);
    break;
  case VM_COMM_TYPE_PTHREAD:
    // Pthread implementation
    shmp = commarray[source][mypet].shmp;   // shared memory mp channel
    shmp->ptr_dest = message;               // set the destination pointer
    // synchronize with vmachine_send()
    pthread_mutex_lock(&(shmp->mutex1));
    shmp->tcounter++;
    if (shmp->tcounter < 2){
      // wait for vmachine_send()
      pthread_cond_wait(&(shmp->cond1), &(shmp->mutex1));
    }else{
      // reset counter and wake up vmachine_send()
      shmp->tcounter = 0;
      pthread_cond_broadcast(&(shmp->cond1));
    }
    pthread_mutex_unlock(&(shmp->mutex1));
    // now ptr_src and ptr_dest are valid for this message
    scpsize = size/2;           // send takes the lower half
    rcpsize = size - scpsize;   // recv takes the upper half
    pdest = (char *)shmp->ptr_dest;
    psrc = (char *)shmp->ptr_src;
    // do actual memcpy
    memcpy(pdest + scpsize, psrc + scpsize, rcpsize);
    // synchronize with vmachine_send()
    pthread_mutex_lock(&(shmp->mutex2));
    shmp->tcounter++;
    if (shmp->tcounter < 2){
      // wait for vmachine_send()
      pthread_cond_wait(&(shmp->cond2), &(shmp->mutex2));
    }else{
      // reset counter and wake up vmachine_send()
      shmp->tcounter = 0;
      pthread_cond_broadcast(&(shmp->cond2));
    }
    pthread_mutex_unlock(&(shmp->mutex2));
    break;
  case VM_COMM_TYPE_SHMHACK:
    // Shared memory hack sync with spin-lock
    shmp = commarray[source][mypet].shmp;   // shared memory mp channel
    if (size<=SHARED_BUFFER){
      // use buffer
      psrc = shmp->buffer;
      // wait until buffer is ready to be used
      sync_buffer_wait_fill(&shmp->shms, 0);
      // do actual memcpy
      memcpy(message, psrc, size);
      // set flag indicating that recv's memcpy() is done and buffer is empty
      sync_buffer_flag_empty(&shmp->shms, 0);
    }else{
      // don't use buffer
      shmp->ptr_dest = message;               // set the destination pointer
      // synchronize with vmachine_send()
      sync_b_flip(&shmp->shms);
      // now ptr_src and ptr_dest are valid for this message
      scpsize = size/2;           // send takes the lower half
      rcpsize = size - scpsize;   // recv takes the upper half
      pdest = (char *)shmp->ptr_dest;
      psrc = (char *)shmp->ptr_src;
      // do actual memcpy
      memcpy(pdest + scpsize, psrc + scpsize, rcpsize);
      // synchronize with vmachine_send()
      sync_b_flop(&shmp->shms);
    }
    break;
  case VM_COMM_TYPE_POSIXIPC:
    // Shared memory hack sync with spin-lock
    pipcmp = commarray[source][mypet].pipcmp;   // shared memory mp channel
    i=0;
    mess = (char *)message;
    while (size>PIPC_BUFFER){
      psrc = pipcmp->buffer[i];
      // wait until buffer is ready to be used
      sync_buffer_wait_fill(&pipcmp->shms, i);
      // do the actual memcpy
      memcpy(mess, psrc, PIPC_BUFFER);
      // set flag indicating that send's memcpy() is done and buffer is valid
      sync_buffer_flag_empty(&pipcmp->shms, i);
      size -= PIPC_BUFFER;
      mess += PIPC_BUFFER;
      i = i^1; // 0->1  or  1->0
    }
    // do the remaining parts of the message
    psrc = pipcmp->buffer[i];
    // wait until buffer is ready to be used
    sync_buffer_wait_fill(&pipcmp->shms, i);
    // do the actual memcpy
    memcpy(mess, psrc, size);
    // set flag indicating that send's memcpy() is done and buffer is valid
    sync_buffer_flag_empty(&pipcmp->shms, i);
    break;
  default:
    break;
  }
}


void vmachine::vmachine_barrier(void){
  // collective barrier over all PETs
  int myp = pid[mypet];
  int myt = tid[mypet];
  if (myt==0){
    // mypet is the zero thread for this PID
    for (int i=0; i<npets; i++)
      if (i!=mypet && pid[i]==myp){
        // pet "i" is another thread under same PID
        shared_mp *shmp = commarray[mypet][i].shmp;
        sync_a_flip(&shmp->shms);
      }
    // now all threads are "flip"-synced under their master thread
    // master thread will use MPI-1 to sync with all the other masters
    MPI_Barrier(mpi_c);
    // now master thread is synced against all other masters
    for (int i=0; i<npets; i++)
      if (i!=mypet && pid[i]==myp){
        // pet "i" is another thread under same PID
        shared_mp *shmp = commarray[mypet][i].shmp;
        sync_a_flop(&shmp->shms);
      }
  }else{
    // mypet is not the master thread for this PID -> find master
    int i;
    for (i=0; i<npets; i++)
      if (pid[i]==myp && tid[i]==0) break;
    // now PET "i" is the master thread for this PID
    shared_mp *shmp = commarray[i][mypet].shmp;
    sync_b_flip(&shmp->shms);
    // now all threads are "flip"-synced under their master thread
    // master will sync against all other masters using MPI-1 and then do flop
    sync_b_flop(&shmp->shms);
  }
}


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// newly written communication calls

void vmachine::vmachine_threadbarrier(void){
  // collective barrier over all PETs in thread group with mypet 
  int myp = pid[mypet];
  int myt = tid[mypet];
  if (myt==0){
    // mypet is the zero thread for this PID
    for (int i=0; i<npets; i++)
      if (i!=mypet && pid[i]==myp){
        // pet "i" is another thread under same PID
        shared_mp *shmp = commarray[mypet][i].shmp;
        sync_a_flip(&shmp->shms);
      }
    for (int i=0; i<npets; i++)
      if (i!=mypet && pid[i]==myp){
        // pet "i" is another thread under same PID
        shared_mp *shmp = commarray[mypet][i].shmp;
        sync_a_flop(&shmp->shms);
      }
  }else{
    // mypet is not the master thread for this PID -> find master
    int i;
    for (i=0; i<npets; i++)
      if (pid[i]==myp && tid[i]==0) break;
    // now PET "i" is the master thread for this PID
    shared_mp *shmp = commarray[i][mypet].shmp;
    sync_b_flip(&shmp->shms);
    // now all threads are "flip"-synced under their master thread
    sync_b_flop(&shmp->shms);
  }
}


void vmachine::vmachine_allreduce(void *in, void *out, int len, vmType type,
  vmOp op){
  if (mpionly){
    // Find corresponding MPI operation
    MPI_Op mpiop;
    switch (op){
    case vmSUM:
      mpiop = MPI_SUM;
      break;
    case vmMIN:
      mpiop = MPI_MIN;
      break;
    case vmMAX:
      mpiop = MPI_MAX;
      break;
    }
    // Find corresponding MPI data type
    MPI_Datatype mpitype;
    switch (type){
    case vmI4:
      mpitype = MPI_INT;
      break;
    case vmR4:
      mpitype = MPI_FLOAT;
      break;
    case vmR8:
      mpitype = MPI_DOUBLE;
      break;
    }
    MPI_Allreduce(in, out, len, mpitype, mpiop, mpi_c);
  }else{
    // This is a very simplistic, probably very bad peformance implementation.
    int templen = len;
    switch (type){
    case vmI4:
      templen *= 4;   // 4 bytes
      break;
    case vmR4:
      templen *= 4;   // 4 bytes
      break;
    case vmR8:
      templen *= 8;   // 8 bytes
      break;
    }
    char *temparray = new char[templen*npets]; // allocate temp data array
    // gather all data onto each PET
    for (int i=0; i<npets; i++)
      vmachine_gather(in, temparray, templen, i);
    // each PET does its own reduction on its local temparray data
    switch (op){
    case vmSUM:
      switch (type){
      case vmI4:
        {
          int *tempdata = (int *)temparray;
          int *outdata = (int *)out;
          for (int i=0; i<len; i++){
            *outdata = 0;
            for (int j=0; j<npets; j++){
              *outdata += tempdata[j*len];
            }
            ++tempdata;
            ++outdata;
          }
        }
        break;
      case vmR4:
        {
          float *tempdata = (float *)temparray;
          float *outdata = (float *)out;
          for (int i=0; i<len; i++){
            *outdata = 0;
            for (int j=0; j<npets; j++){
              *outdata += tempdata[j*len];
            }
            ++tempdata;
            ++outdata;
          }
        }
        break;
      case vmR8:
        {
          double *tempdata = (double *)temparray;
          double *outdata = (double *)out;
          for (int i=0; i<len; i++){
            *outdata = 0;
            for (int j=0; j<npets; j++){
              *outdata += tempdata[j*len];
            }
            ++tempdata;
            ++outdata;
          }
        }
        break;
      }
      break;
    case vmMIN:
      printf("Reduce operation vmMIN is not yet implemented\n");
      break;
    case vmMAX:
      printf("Reduce operation vmMAX is not yet implemented\n");
      break;
    }
    delete [] temparray;
  }  
}


void vmachine::vmachine_scatter(void *in, void *out, int len, int root){
  if (mpionly){
    MPI_Scatter(in, len, MPI_BYTE, out, len, MPI_BYTE, root, mpi_c);
  }else{
    // This is a very simplistic, probably very bad peformance implementation.
    if (mypet==root){
      // I am root -> send chunks to all other PETs
      char *rootin = (char *)in;
      for (int i=0; i<root; i++){
        vmachine_send(rootin, len, i);
        rootin += len;
      }
      // memcpy root's chunk
      memcpy(out, rootin, len);
      rootin += len;
      // keep sending chunks
      for (int i=root+1; i<npets; i++){
        vmachine_send(rootin, len, i);
        rootin += len;
      }
    }else{
      // all other PETs receive their chunk
      vmachine_recv(out, len, root);
    }
  }
}


void vmachine::vmachine_gather(void *in, void *out, int len, int root){
  if (mpionly){
    MPI_Gather(in, len, MPI_BYTE, out, len, MPI_BYTE, root, mpi_c);
  }else{
    // This is a very simplistic, probably very bad peformance implementation.
    if (mypet==root){
      // I am root -> receive chunks from all other PETs
      char *rootout = (char *)out;
      for (int i=0; i<root; i++){
        vmachine_recv(rootout, len, i);
        rootout += len;
      }
      // memcpy root's chunk
      memcpy(rootout, in, len);
      rootout += len;
      // keep sending chunks
      for (int i=root+1; i<npets; i++){
        vmachine_recv(rootout, len, i);
        rootout += len;
      }
    }else{
      // all other PETs send their chunk
      vmachine_send(in, len, root);
    }
  }
}



// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~ Sync Calls
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// I noticed that you get slightly better performance if the sync functions are
// compiled without optimization! For this feature they need to be placed into
// a separate file again...


void sync_a_flip(shmsync *shms){
  volatile int *b_done = shms->b_done;
  *(shms->a_done) = 1;      // signal that send has arrived
  while (*b_done == 0);     // check whether recv has arrived
}

void sync_a_flop(shmsync *shms){
  volatile int *a_done = shms->a_done;
  *(shms->b_done) = 0;      // reset for next time
  while (*a_done == 1);     // check whether recv has arrived
}


void sync_b_flip(shmsync *shms){
  volatile int *a_done = shms->a_done;
  *(shms->b_done) = 1;      // signal that recv has arrived
  while (*a_done == 0);     // check whether send has arrived
}

void sync_b_flop(shmsync *shms){
  volatile int *b_done = shms->b_done;
  *(shms->a_done) = 0;      // reset for next time
  while (*b_done == 1);     // check whether send has arrived
}


void sync_buffer_flag_fill(shmsync *shms, int select){
  *(shms->buffer_done[select]) = 1;      // flag that buffer has been filled
}

void sync_buffer_flag_empty(shmsync *shms, int select){
  *(shms->buffer_done[select]) = 0;      // flag that buffer has been cleared
}

void sync_buffer_wait_fill(shmsync *shms, int select){
  volatile int *buffer_done = shms->buffer_done[select];
  while (*buffer_done == 0);     // check whether buffer has been filled
}

void sync_buffer_wait_empty(shmsync *shms, int select){
  volatile int *buffer_done = shms->buffer_done[select];
  while (*buffer_done == 1);     // check whether buffer has been cleared
}


void sync_reset(shmsync *shms){
  *(shms->a_done) = 0;
  *(shms->b_done) = 0;
  for (int i=0; i<SYNC_NBUFFERS; i++)
    *(shms->buffer_done[i]) = 0;
}


