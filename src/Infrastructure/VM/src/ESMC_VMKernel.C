// $Id: ESMC_VMKernel.C,v 1.99.2.13 2009/01/21 21:25:24 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//-----------------------------------------------------------------------------

#include "ESMC_VMKernel.h"

// On SunOS systems there are a couple of macros that need to be set
// in order to get POSIX compliant functions IPC, pthreads, gethostid
#ifdef __sun
#define _POSIX_SOURCE
#define _POSIX_C_SOURCE 199309L
#define __EXTENSIONS__
#define _POSIX_PTHREAD_SEMANTICS
#endif

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

// Standard headers
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <time.h>
#include <float.h>
#include <math.h>

// Memory mapped files may not be available on all systems
#ifndef ESMF_NO_POSIXIPC
#include <sys/mman.h>
#endif

#include <fcntl.h>

#ifndef MPICH_IGNORE_CXX_SEEK
#define MPICH_IGNORE_CXX_SEEK
#endif
#include <mpi.h>

// VMKernel can be compiled stand-alone outside of ESMF 
#ifdef VMK_STANDALONE
#include <pthread.h>
#else
#include "ESMF_Pthread.h"
#endif

// macros used within this source file
#define VERBOSITY             (0)       // 0: off, 10: max
#define VM_TID_MPI_TAG        (10)      // mpi tag used to send/recv TID
#ifdef SIGRTMIN
#define VM_SIG1               (SIGRTMIN)  // avoid sigusr1 and sigusr2 if avail.
#else
#ifdef ESMF_NO_SIGUSR2
#define VM_SIG1               (SIGUSR1)
// Note that SIGUSR1 interferes with MPICH's CH_P4 device!
#else
#define VM_SIG1               (SIGUSR2)
// Note that SIGUSR2 interferes with LAM!
#endif
#endif

namespace ESMCI {

// Definition of class static data members
MPI_Group VMK::default_mpi_g;
MPI_Comm VMK::default_mpi_c;
int VMK::mpi_thread_level;
int VMK::ncores;
int *VMK::cpuid;
int *VMK::ssiid;
// Static data members to support command line arguments
int VMK::argc;
char *VMK::argv_store[100];
char **VMK::argv = &(argv_store[0]);
// Second set of command line argument variables to support MPICH1.2
int VMK::argc_mpich;
char *VMK::argv_mpich_store[100];
char **VMK::argv_mpich = &(argv_mpich_store[0]);

} // namespace ESMCI

// -----------------------------------------------------------------------------
// vmkt encapsulation: begin
typedef struct{
  volatile int flag;
  pthread_t tid;
  pthread_mutex_t mut0;
  pthread_cond_t cond0;
  pthread_mutex_t mut1;
  pthread_cond_t cond1;
  pthread_mutex_t mut_extra1;
  pthread_cond_t cond_extra1;
  pthread_mutex_t mut_extra2;
  pthread_cond_t cond_extra2;
  void *arg;
}vmkt_t;

int vmkt_create(vmkt_t *vmkt, void *(*vmkt_spawn)(void *), void *arg){
  vmkt->flag = 0;
  pthread_mutex_init(&(vmkt->mut0), NULL);
  pthread_mutex_lock(&(vmkt->mut0));
  pthread_cond_init(&(vmkt->cond0), NULL);
  pthread_mutex_init(&(vmkt->mut1), NULL);
  //pthread_mutex_lock(&(vmkt->mut1));
  pthread_cond_init(&(vmkt->cond1), NULL);
  pthread_mutex_init(&(vmkt->mut_extra1), NULL);
  //pthread_mutex_lock(&(vmkt->mut_extra1));
  pthread_cond_init(&(vmkt->cond_extra1), NULL);
  pthread_mutex_init(&(vmkt->mut_extra2), NULL);
  pthread_mutex_lock(&(vmkt->mut_extra2));
  pthread_cond_init(&(vmkt->cond_extra2), NULL);
  int error = pthread_create(&(vmkt->tid), NULL, vmkt_spawn, arg);
  if (!error){ // only wait if the thread was successfully created
    pthread_cond_wait(&(vmkt->cond0), &(vmkt->mut0));   // back-sync #1
    pthread_cond_wait(&(vmkt->cond_extra2), &(vmkt->mut_extra2)); // back-s. #2
  }
  return error;
}

int vmkt_release(vmkt_t *vmkt, void *arg){
  vmkt->arg = arg;
  pthread_mutex_lock(&(vmkt->mut1));  
  pthread_cond_signal(&(vmkt->cond1));
  pthread_mutex_unlock(&(vmkt->mut1));
  return 0;
}

int vmkt_catch(vmkt_t *vmkt){
  pthread_cond_wait(&(vmkt->cond0), &(vmkt->mut0)); //wait for the child
  return 0;
}

int vmkt_join(vmkt_t *vmkt){
  vmkt->flag = 1;
  pthread_mutex_lock(&(vmkt->mut1));  
  pthread_cond_signal(&(vmkt->cond1));
  pthread_mutex_unlock(&(vmkt->mut1));
  pthread_join(vmkt->tid, NULL); //wait for the child
  return 0;
}
// vmkt encapsulation: end
// -----------------------------------------------------------------------------


namespace ESMCI {

void VMK::obtain_args(){
  // obtain command line args for this process
#ifndef ESMF_NO_SYSTEMCALL
  int mypid = getpid();
  char command[160], fname[80], args[8000];
  FILE *fp;
#ifdef ESMF_OS_Linux
  // this is a SUS3=sysV derived OS
  // fgets() changes dir on Linux/MPICH -> use fscanf() instead
  sprintf(command, "env COLUMNS=8000 ps -p %d -o args= > .args.%d", mypid,
    mypid);
  system(command);
  sprintf(fname, ".args.%d", mypid);
  fp=fopen(fname, "r");
  if (fp){
    fscanf(fp, "%[^\n]", args);
    fclose(fp);
  }else{
    args[0]='\0'; // empty args string
  }
  sprintf(command, "rm -f .args.%d", mypid);
  system(command);
#elif defined ESMF_OS_Darwin
  // this is a BSD derived OS
  sprintf(command, "env COLUMNS=8000 ps -w -w -p %d -o command > .args.%d",
          mypid, mypid);
  system(command);
  sprintf(fname, ".args.%d", mypid);
  fp=fopen(fname, "r");
  if (fp){
    fgets(args, 8000, fp);  // scan off header line of ps output
    fgets(args, 8000, fp);
    fclose(fp);
  }else{
    args[0]='\0'; // empty args string
  }
  sprintf(command, "rm -f .args.%d", mypid);
  system(command);
#else
  args[0]='\0'; // empty args string
#endif
  // now the string 'args' holds the complete command line with arguments
  argc=0;
  int i=0;
  int j=0;
  // chop up args and thus set up argc and argv
  while (args[i] != '\0'){
    if (args[i] != ' '){
      argv[argc][j] = args[i];
      ++j;
    }else{
      argv[argc][j] = '\0';
      ++argc;
      j=0;
    }
    ++i;
  }
  if (i){
    // only if this isn't for a complete NULL case
    argv[argc][j] = '\0';
    ++argc;
  }
  // now argc and argv are valid
  //printf("argc=%d\n", argc);
  //for (i=0; i<argc; i++)
  //  printf("%s\n", argv[i]);
#endif
}


void VMK::init(MPI_Comm mpiCommunicator){
  // initialize the physical machine and a default (all MPI) virtual machine
  // initialize signal handling -> this MUST happen before MPI_Init is called!!
  struct sigaction action;
  action.sa_handler = SIG_DFL;
  sigemptyset (&(action.sa_mask));
  sigaction(VM_SIG1, &action, NULL);  // restore default handle for VM_SIG1
  sigset_t sigs_to_block;
  sigemptyset(&sigs_to_block);
  sigaddset(&sigs_to_block, VM_SIG1);
  sigprocmask(SIG_BLOCK, &sigs_to_block, NULL); // block VM_SIG1
  // obtain command line arguments and store in the VM class
  argc = 0; // reset
  for (int k=0; k<100; k++)
    argv[k] = new char[1600];
#ifdef ESMF_MPICH
  // currently only obtain arguments for MPICH because it needs it!!!
  obtain_args();
#endif
  // next check is whether MPI has been initialized yet
  // actually we need to indicate an error if MPI has been initialized before
  // because signal blocking might not reach all of the threads again...
  int initialized;
  MPI_Initialized(&initialized);
  if (!initialized){
#ifdef ESMF_MPICH   
    // MPICH1.2 is not standard compliant and needs valid args
    // make copy of argc and argv for MPICH because it modifies them and
    // the original values are needed to delete the memory during finalize()
    argc_mpich = argc;
    for (int k=0; k<100; k++)
      argv_mpich[k] = argv[k];
    MPI_Init_thread(&argc_mpich, (char ***)&argv_mpich, MPI_THREAD_MULTIPLE,
      &mpi_thread_level);
#else
    MPI_Init_thread(NULL, NULL, MPI_THREAD_MULTIPLE, &mpi_thread_level);
#endif
  }
  // so now MPI is for sure initialized...
  // TODO: now it should be safe to call obtain_args() for all MPI impl.
  // Obtain MPI variables
  int rank, size;
  MPI_Comm_rank(mpiCommunicator, &rank);
  MPI_Comm_size(mpiCommunicator, &size);
  // since this method is only supposed to be called my the main_vmachine 
  // and the main_vmachine is all MPI pets we can do the following:
  npets=size;           // user is required to start with #processes=#cores!!!!
  mypet=rank;
  mypthid=pthread_self();
#ifdef ESMF_MPIUNI
  mpionly = 0;          // this way the commtype will be checked in comm calls
#else
  mpionly = 1;          // normally the default VM can only be MPI-only
#endif
  // no threading in default global VM
  nothreadsflag = 1;
  // set up private Group and Comm objects across "mpiCommunicator"
  MPI_Comm_group(mpiCommunicator, &mpi_g);
  MPI_Comm_create(mpiCommunicator, mpi_g, &mpi_c);
  // ... and copy them into the class static default variables...
  default_mpi_g = mpi_g;
  default_mpi_c = mpi_c;
  // initialize the shared memory variables
  pth_finish_count = NULL;
  pth_mutex = new pthread_mutex_t;
  pthread_mutex_init(pth_mutex, NULL);
  pth_mutex2 = new pthread_mutex_t;
  pthread_mutex_init(pth_mutex2, NULL);
  // the mutex flag must be reset
  if (mpi_thread_level<MPI_THREAD_MULTIPLE)
    mpi_mutex_flag = 1; // must use muteces around mpi comms
  else
    mpi_mutex_flag = 0; // don't need to use muteces around mpi comms
  // setup the communication channels
  sendChannel = new comminfo[npets];
  recvChannel = new comminfo[npets];
#ifdef ESMF_MPIUNI
  // for mpiuni the default communication is via MPIUNI branch
  sendChannel[0].comm_type = VM_COMM_TYPE_MPIUNI;
  sendChannel[0].shmp = new shared_mp;
  sync_reset(&(sendChannel[0].shmp->shms));
  recvChannel[0] = sendChannel[0];
#else
  for (int i=0; i<npets; i++){
    // normally by default all communication is via MPI-1
    sendChannel[i].comm_type = VM_COMM_TYPE_MPI1;
    recvChannel[i].comm_type = VM_COMM_TYPE_MPI1;
  }
#endif
  // setup the IntraProcessSharedMemoryAllocation List
  ipshmTop = new ipshmAlloc*;
  *ipshmTop = NULL;      // reset
  ipshmLocalTop = NULL;   // reset
  ipshmMutex = new pthread_mutex_t;
  pthread_mutex_init(ipshmMutex, NULL);
  ipSetupMutex = new pthread_mutex_t;
  pthread_mutex_init(ipSetupMutex, NULL);
  // set up the request queue
  nhandles=0;
  firsthandle=NULL;
  // set up physical machine info
  ncores=size;          // user is required to start with #processes=#cores!!!!
  // determine CPU ids
  cpuid = new int[ncores];
  for (int i=0; i<ncores; i++){
    cpuid[i]=i;                 // hardcoded assumption of single-core CPUs
  }
  // determine SSI ids
  ssiid = new int[ncores];
#ifdef ESMF_NO_GETHOSTID
  for (int i=0; i<ncores; i++){
    ssiid[i]=i;                 // hardcoded assumption of single-CPU SSIs
  }
#else
  long int *temp_ssiid = new long int[ncores];
  int hostid = gethostid();
  MPI_Allgather(&hostid, sizeof(long int), MPI_BYTE, temp_ssiid, 
    sizeof(long int), MPI_BYTE, mpi_c);
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
#endif
  // ESMC_VMK pet -> core mapping
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


void VMK::finalize(int finalizeMpi){
  // finalize default (all MPI) virtual machine, deleting all its allocations
  for (int k=0; k<100; k++)
    delete [] argv[k];
  pthread_mutex_destroy(pth_mutex);
  delete pth_mutex;
  pthread_mutex_destroy(pth_mutex2);
  delete pth_mutex2;
#ifdef ESMF_MPIUNI
  delete sendChannel[0].shmp;
#endif
  delete [] sendChannel;
  delete [] recvChannel;  
  while (*ipshmTop != NULL){
    if ((*ipshmTop)->auxCounter > 0)
      free((*ipshmTop)->allocation);
    ipshmAlloc *ipshmPrev = *ipshmTop;
    *ipshmTop = (*ipshmTop)->next;
    delete ipshmPrev;
  }
  delete ipshmTop;
  pthread_mutex_destroy(ipshmMutex);
  delete ipshmMutex;
  pthread_mutex_destroy(ipSetupMutex);
  delete ipSetupMutex;
  delete [] cpuid;
  delete [] ssiid;
  delete [] lpid;
  delete [] pid;
  delete [] tid;
  delete [] ncpet;
  for (int i=0; i<npets; i++)
    delete [] cid[i];
  delete [] cid;
  // conditionally finalize MPI
  int finalized;
  MPI_Finalized(&finalized);
  if (!finalized){
    MPI_Comm_free(&mpi_c);
    MPI_Group_free(&mpi_g);
    if (finalizeMpi)
      MPI_Finalize();
  }
}


struct contrib_id{
  pthread_t blocker_tid;    // POSIX thread id of blocker thread
  vmkt_t *blocker_vmkt;     // pointer to blocker's vmkt structure
  int mpi_pid;              // MPI rank in the context of the default VMK
  pid_t pid;                // POSIX process id
  pthread_t tid;            // POSIX thread id
};


struct SpawnArg{
  // members which are different for each new pet
  VMK *myvm;                  // pointer to vm instance on heap
  pthread_t pthid;            // pthread id of the spawned thread
  int mypet;                  // new mypet 
  int *ncontributors;         // number of pets that contributed cores 
  contrib_id **contributors;  // array of contributors
  vmkt_t vmkt;                // this pet's vmkt
  vmkt_t vmkt_extra;          // extra vmkt for this pet (sigcatcher)
  // members which are identical for all new pets
  void *(*fctp)(void *, void *);  // pointer to the user function
  // 1st (void *) points to the provided object (child of VMK class)
  // 2nd (void *) points to data that shall be passed to the user function
  int npets;                  // new number of pets
  int *lpid;
  int *pid;
  int *tid;
  int *ncpet;
  int **cid;
  MPI_Group mpi_g;
  MPI_Comm mpi_c;
  int nothreadsflag;
  // shared memory variables
  pthread_mutex_t *pth_mutex2;
  pthread_mutex_t *pth_mutex;
  int *pth_finish_count;
  VMK::comminfo *sendChannel;
  VMK::comminfo *recvChannel;
  VMK::ipshmAlloc **ipshmTop;
  pthread_mutex_t *ipshmMutex;
  pthread_mutex_t *ipSetupMutex;
  int pref_intra_ssi;
  // cargo
  void *cargo;
};

    
void VMK::abort(){
  // abort default (all MPI) virtual machine
  int finalized;
  MPI_Finalized(&finalized);
  if (!finalized)
    MPI_Abort(default_mpi_c, 0);
}


void VMK::construct(void *ssarg){
  SpawnArg *sarg = (SpawnArg *)ssarg;
  // fill an already existing VMK object with info
  mypet=sarg->mypet;
  mypthid=sarg->pthid;
  npets = sarg->npets;
  lpid = new int[npets];
  pid = new int[npets];
  tid = new int[npets];
  ncpet = new int[npets];
  cid = new int*[npets];
  for (int i=0; i<npets; i++){
    lpid[i]=sarg->lpid[i];
    pid[i]=sarg->pid[i];
    tid[i]=sarg->tid[i];
    ncpet[i]=sarg->ncpet[i];
    cid[i] = new int[ncpet[i]];
    for (int k=0; k<ncpet[i]; k++)
      cid[i][k] = sarg->cid[i][k];
  }
  mpi_g = sarg->mpi_g;
  mpi_c = sarg->mpi_c;
  pth_mutex2 = sarg->pth_mutex2;
  pth_mutex = sarg->pth_mutex;
  pth_finish_count = sarg->pth_finish_count;
  if (mpi_thread_level<MPI_THREAD_MULTIPLE)
    mpi_mutex_flag = 1; // must use muteces around mpi comms
  else
    mpi_mutex_flag = 0; // don't need to use muteces around mpi comms
  sendChannel = sarg->sendChannel;
  recvChannel = sarg->recvChannel;
  // setup the IntraProcessSharedMemoryAllocation List
  ipshmTop = sarg->ipshmTop;
  ipshmLocalTop = *ipshmTop;
  ipshmMutex = sarg->ipshmMutex;
  ipSetupMutex = sarg->ipSetupMutex;
  // initialize the request queue
  nhandles=0;
  firsthandle=NULL;
  // preference dependent settings
  if (sarg->pref_intra_ssi == PREF_INTRA_SSI_POSIXIPC){
#ifdef ESMF_NO_POSIXIPC
    fprintf(stderr, "PREF_INTRA_SSI_POSIXIPC not supported on this platform!\n"
      "-> default into PREF_INTRA_SSI_MPI1.\n");
    sarg->pref_intra_ssi = PREF_INTRA_SSI_MPI1;
#else
    // now set up the POSIX IPC shared memory resources between pets
    // that run within the same SSI but different PID
    for (int i=0; i<npets; i++){
      // looping over all pets
      if (getSsiid(i) == getSsiid(mypet)){
        // found a pet under same SSI ...
        if (pid[i] != pid[mypet]){
          // ... and with different PID (which also excludes mypet!)
          // ready to set up shared memory segment using POSIX IPC
          char shm_file[80];
          int shm_fd;
          int size = sizeof(pipc_mp);
          void *shm_segment;
          // first: sendChannel
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
          // enter the address into the sendChannel
          sendChannel[i].pipcmp = (pipc_mp *)shm_segment;
          sendChannel[i].comm_type = VM_COMM_TYPE_POSIXIPC;
          // then: recvChannel
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
          // enter the address into the recvChannel
          recvChannel[i].pipcmp = (pipc_mp *)shm_segment;
          recvChannel[i].comm_type = VM_COMM_TYPE_POSIXIPC;
        }
      }
    }
#endif
  }
#ifdef ESMF_MPIUNI
  // don't set mpionly flag so that comm call check for commtype
  mpionly=0;
#else
  // determine whether we are dealing with an MPI-only VMK
  mpionly=1;  // assume this is MPI-only VMK until found otherwise
  for (int i=0; i<npets; i++)
    if (tid[i]>0) mpionly=0;    // found multi-threading PET
#endif
  nothreadsflag = sarg->nothreadsflag;
  // need a barrier here before any of the PETs get into user code...
  //barrier();
}


void VMK::destruct(){
  // determine how many pets are of the same pid as mypet is
  int num_same_pid=0;
  for (int i=0; i<npets; i++)
    if (pid[i]==pid[mypet])
      ++num_same_pid;
  // check with the other pets under this pid where we are in wrap-up
  int last_flag=0;
  pthread_mutex_lock(pth_mutex);
  ++(*pth_finish_count);          // increment counter
#if (VERBOSITY > 9)
  printf("wrap-up counts: %d %d\n", *pth_finish_count, num_same_pid);
#endif
  if (*pth_finish_count == num_same_pid)
    last_flag=1; // indicate that I am the last pet for this pid to wrap up...
  pthread_mutex_unlock(pth_mutex);
  // now we know if we are the last pet for this pid
  if (last_flag){
    // mypet is the last pet of this pid to wrap up:
#if (VERBOSITY > 9)
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
    // free - the IntraProcessSharedMemoryAllocation List
    while (*ipshmTop != NULL){
      if ((*ipshmTop)->auxCounter > 0)
        free((*ipshmTop)->allocation);
      ipshmAlloc *ipshmPrev = *ipshmTop;
      *ipshmTop = (*ipshmTop)->next;
      delete ipshmPrev;
    }
    delete ipshmTop;
    pthread_mutex_destroy(ipshmMutex);
    delete ipshmMutex;
    pthread_mutex_destroy(ipSetupMutex);
    delete ipSetupMutex;
    for (int i=0; i<npets; i++){
      if(sendChannel[i].comm_type==VM_COMM_TYPE_SHMHACK
        ||sendChannel[i].comm_type==VM_COMM_TYPE_PTHREAD
        ||sendChannel[i].comm_type==VM_COMM_TYPE_MPIUNI){
        // intra-process shared memory structure to be deleted
        shared_mp *shmp=sendChannel[i].shmp;
        if(sendChannel[i].comm_type==VM_COMM_TYPE_PTHREAD){
          pthread_mutex_destroy(&(shmp->mutex1));
          pthread_cond_destroy(&(shmp->cond1));
          pthread_mutex_destroy(&(shmp->mutex2));
          pthread_cond_destroy(&(shmp->cond2));
        }
#if (VERBOSITY > 9)
        printf("deleting shmp=%p for sendChannel[%d], mypet=%d\n", 
          shmp, i, mypet);
#endif          
        delete shmp;
      }else if (sendChannel[i].comm_type==VM_COMM_TYPE_POSIXIPC){
#ifdef ESMF_NO_POSIXIPC
#else
        pipc_mp *pipcmp=sendChannel[i].pipcmp;
        char shm_name[80];
        strcpy(shm_name, pipcmp->shm_name);
        munmap((void *)pipcmp, sizeof(pipc_mp));
        shm_unlink(shm_name);
#if (VERBOSITY > 9)
        printf("deleting pipcmp=%p (%s) for sendChannel[%d], mypet=%d\n", 
          pipcmp, shm_name, i, mypet);
#endif
#endif
      }
      if (i != mypet){
        // diagonal element is handled by sendChannel
        if(recvChannel[i].comm_type==VM_COMM_TYPE_SHMHACK
          ||recvChannel[i].comm_type==VM_COMM_TYPE_PTHREAD
          ||recvChannel[i].comm_type==VM_COMM_TYPE_MPIUNI){
          // intra-process shared memory structure to be deleted
          shared_mp *shmp=recvChannel[i].shmp;
          if(recvChannel[i].comm_type==VM_COMM_TYPE_PTHREAD){
            pthread_mutex_destroy(&(shmp->mutex1));
            pthread_cond_destroy(&(shmp->cond1));
            pthread_mutex_destroy(&(shmp->mutex2));
            pthread_cond_destroy(&(shmp->cond2));
          }
#if (VERBOSITY > 9)
          printf("deleting shmp=%p for recvChannel[%d], mypet=%d\n", 
            shmp, i, mypet);
#endif          
          delete shmp;
        }else if (recvChannel[i].comm_type==VM_COMM_TYPE_POSIXIPC){
#ifdef ESMF_NO_POSIXIPC
#else
          pipc_mp *pipcmp=recvChannel[i].pipcmp;
          char shm_name[80];
          strcpy(shm_name, pipcmp->shm_name);
          munmap((void *)pipcmp, sizeof(pipc_mp));
          shm_unlink(shm_name);
#if (VERBOSITY > 9)
          printf("deleting pipcmp=%p (%s) for recvChannel[%d], mypet=%d\n", 
            pipcmp, shm_name, i, mypet);
#endif
#endif
        }
      }
    }    
    delete [] sendChannel;
    delete [] recvChannel;
  }
  delete [] lpid;
  delete [] pid;
  delete [] tid;
  delete [] ncpet;
  for (int i=0; i<npets; i++)
    delete [] cid[i];
  delete [] cid;
}  


static void *vmk_spawn(void *arg){
  // vmkt's first level spawn function, includes the catch/release loop
  // typecast the argument into the type it really is:
  SpawnArg *sarg = (SpawnArg *)arg;
#if (VERBOSITY > 5)
  fprintf(stderr, "hello from within vmk_spawn, mypet=%d\n", sarg->mypet);
#endif
  // now use vmkt features to prepare for catch/release loop (back-sync)
  // - part 1
  vmkt_t *vmkt = &(sarg->vmkt);
  pthread_mutex_lock(&(vmkt->mut0));        // back-sync #1 ...
  pthread_cond_signal(&(vmkt->cond0));      // . back-sync #1 .
  pthread_mutex_unlock(&(vmkt->mut0));      // ... back-sync #1
  // now we know that vmkt_create is past pthread_create()
  // ... and we are between back-sync #1 and #2
  pthread_mutex_lock(&(vmkt->mut1));        // prepare this thread's mutex
  pthread_mutex_lock(&(vmkt->mut_extra1));  // prepare this thread's mutex
  // fill in the tid for this thread
  sarg->pthid = sarg->vmkt.tid;
  // obtain reference to the vm instance on heap
  VMK *vm = sarg->myvm;
  // setup the pet section in this vm instance
  vm->construct((void *)sarg);
  // note: The VM above must be constructed _before_ back-sync'ing #2 to
  //       vmkt_create in order to assure that the entries in the VM are valid!
  // now use vmkt features to prepare for catch/release loop (back-sync)
  // - part 2
  pthread_mutex_lock(&(vmkt->mut_extra2));    // back-sync #2 ...
  pthread_cond_signal(&(vmkt->cond_extra2));  // . back-sync #2 .
  pthread_mutex_unlock(&(vmkt->mut_extra2));  // ... back-sync #2
  volatile int *f = &(vmkt->flag);
  // now enter the catch/release loop
  for(;;){
    //sleep(2); // put this in the code to verify that earlier received signals
    // will be pending on a per thread basis...
#if (VERBOSITY > 5)
    fprintf(stderr,"thread %d: %d going to wait for release, pid: %d\n",
      vmkt->tid, pthread_self(), getpid());
#endif
    pthread_cond_wait(&(vmkt->cond1), &(vmkt->mut1));
#if (VERBOSITY > 1)
    fprintf(stderr,"thread %d: %d was released, pid: %d, vm: %p\n", 
      vmkt->tid, pthread_self(), getpid(), vm);
#endif
    if (*f==1) break; // check whether this was a wrap up call

    //vm.barrier();
    
    // call the function pointer with the new VMK as its argument
    // this is where we finally enter the user code again...
    if (vmkt->arg==NULL)
      sarg->fctp((void *)vm, sarg->cargo);
    else
      sarg->fctp((void *)vm, vmkt->arg);
    //vmkt->routine(vmkt->arg);
    
  // before pet terminates it must send a signal indicating that core is free
  for (int i=0; i<sarg->ncontributors[sarg->mypet]; i++){
#if (VERBOSITY > 5)
    fprintf(stderr, " send wake-up signal to : %d %d\n",
      sarg->contributors[sarg->mypet][i].pid, 
      sarg->contributors[sarg->mypet][i].blocker_tid);
#endif
    // send signal to the _other_ process
    kill(sarg->contributors[sarg->mypet][i].pid, VM_SIG1);
    // which ever thread of the other process woke up will try to receive tid
//    MPI_Send(&(sarg->contributors[sarg->mypet][i].blocker_tid),
//      sizeof(pthread_t), MPI_BYTE, sarg->contributors[sarg->mypet][i].mpi_pid,
//      VM_TID_MPI_TAG, vm.default_mpi_c);
    MPI_Send(&(sarg->contributors[sarg->mypet][i].blocker_vmkt),
      sizeof(vmkt_t *), MPI_BYTE, sarg->contributors[sarg->mypet][i].mpi_pid,
      VM_TID_MPI_TAG, vm->default_mpi_c);
  }
    // now signal to parent thread that child is done with its work
    pthread_mutex_lock(&(vmkt->mut0)); // wait until parent has reached "catch"
    pthread_cond_signal(&(vmkt->cond0)); // then signal that child is done
    pthread_mutex_unlock(&(vmkt->mut0)); // release the mutex lock for parent
  }
  // wrap-up...
  vm->destruct();
  // when returning from this procedure this pet will terminate
  return NULL;
}


static void *vmk_sigcatcher(void *arg){
  // vmkt's first level spawn function, includes the catch/release loop
  // typecast the argument into the type it really is:
  SpawnArg *sarg = (SpawnArg *)arg;
#if (VERBOSITY > 5)
  fprintf(stderr, "hello from within vmk_sigcatcher\n");
#endif
  // need this for waking up blocker
  vmkt_t *blocker_vmkt;
  // now use vmkt features to prepare for catch/release loop (back-sync)
  // - part 1
  vmkt_t *vmkt = &(sarg->vmkt_extra);
  pthread_mutex_lock(&(vmkt->mut0));        // back-sync #1 ...
  pthread_cond_signal(&(vmkt->cond0));      // . back-sync #1 .
  pthread_mutex_unlock(&(vmkt->mut0));      // ... back-sync #1
  // now we know that vmkt_create is past pthread_create()
  // ... and we are between back-sync #1 and #2
  pthread_mutex_lock(&(vmkt->mut1));        // prepare this thread's mutex
  pthread_mutex_lock(&(vmkt->mut_extra1));  // prepare this thread's mutex
  volatile int *f = &(vmkt->flag);
  // since LinuxThreads (pre NPTL) have the problem that each thread reports
  // its own PID instead the same for each thread, which would be the posix
  // behavior, we need to get the sigcatcher's pid to send it over to the
  // other process.
  pid_t pid = getpid();
  vmkt->arg = (void *)&pid;
  // more preparation
  sigset_t sigs_to_catch;
  sigemptyset(&sigs_to_catch);
  sigaddset(&sigs_to_catch, VM_SIG1);
  int caught;
  MPI_Status mpi_s;
  VMK vm;  // need a handle to access the MPI_Comm of default VMK
  // now use vmkt features to prepare for catch/release loop (back-sync)
  // - part 2
  pthread_mutex_lock(&(vmkt->mut_extra2));    // back-sync #2 ...
  pthread_cond_signal(&(vmkt->cond_extra2));  // . back-sync #2 .
  pthread_mutex_unlock(&(vmkt->mut_extra2));  // ... back-sync #2
  // now enter the catch/release loop
  for(;;){
    //sleep(2); // put this in the code to verify that earlier received signals
    // will be pending on a per thread basis...
#if (VERBOSITY > 5)
    fprintf(stderr,"vmk_sigcatcher: thread %d: %d going to wait for release, pid: %d\n",
      vmkt->tid, pthread_self(), getpid());
#endif
    pthread_cond_wait(&(vmkt->cond1), &(vmkt->mut1));
#if (VERBOSITY > 5)
    fprintf(stderr,"vmk_sigcatcher: thread %d: %d was released, pid:%d\n", vmkt->tid,
      pthread_self(), getpid());
#endif

  if (*f==1) break; // check whether this was a wrap up call

  // This is a signal catcher. Its job is to catch a signal from other processes
  // indicating that cores have become available again. When such a signal has
  // been received the sigcatcher will communicate with the signaling process
  // via MPI and receive the actual pthread_id that need to be awoken on this
  // process, which is actually a blocker thread which then will wrap up and 
  // by that indicate that the resource has been made available again.
  // suspend thread until a signal arrives
#if (VERBOSITY > 5)
  fprintf(stderr,"I am a sigcatcher for pid %d and am going to sleep...\n",
    getpid());
#endif

#ifdef ESMF_NO_SIGNALS
#ifndef ESMF_NO_PTHREADS
#error Need signals for Pthreads in VMKernel
#endif
#else
  sigwait(&sigs_to_catch, &caught);
#endif
  
#if (VERBOSITY > 5)
  fprintf(stderr, "I am a sigcatcher for pid %d and signal: %d woke me up...\n",
    getpid(), caught);
#endif
  // this signal was received from a thread running under another process
  // receive the thread id of the blocker thread that needs to be woken up
//  MPI_Recv(&thread_wake, sizeof(pthread_t), MPI_BYTE, MPI_ANY_SOURCE, 
//    VM_TID_MPI_TAG, vm.default_mpi_c, &mpi_s);
  MPI_Recv(&blocker_vmkt, sizeof(vmkt_t *), MPI_BYTE, MPI_ANY_SOURCE, 
    VM_TID_MPI_TAG, vm.default_mpi_c, &mpi_s);
  // now wake up the correct blocker thread within this pid
#if (VERBOSITY > 5)
//  printf("It's the sigcatcher for pid %d again. I received the blocker tid\n"
//    " and I'll wake up blocker thread with tid: %d\n", getpid(), thread_wake);
  fprintf(stderr, "It's the sigcatcher for pid %d again. I received the blocker"
    " &vmkt\n"
    " and I'll wake up blocker thread with &vmkt: %p\n", getpid(),
    blocker_vmkt);
#endif
  
  pthread_mutex_lock(&(blocker_vmkt->mut_extra1));
  pthread_cond_signal(&(blocker_vmkt->cond_extra1));
  pthread_mutex_unlock(&(blocker_vmkt->mut_extra1));
  
  // this sigcatcher has done its job and is allowed to recycle to be caught...
  
    // now signal to parent thread that child is done with its work
    pthread_mutex_lock(&(vmkt->mut0)); // wait until parent has reached "catch"
    pthread_cond_signal(&(vmkt->cond0)); // then signal that child is done
    pthread_mutex_unlock(&(vmkt->mut0)); // release the mutex lock for parent
  }
  return NULL;
}


static void *vmk_block(void *arg){
  // vmkt's first level spawn function, includes the catch/release loop
  // typecast the argument into the type it really is:
  SpawnArg *sarg = (SpawnArg *)arg;
#if (VERBOSITY > 5)
  fprintf(stderr, "hello from within vmk_block\n");
#endif
  // now use vmkt features to prepare for catch/release loop (back-sync)
  // - part 1
  vmkt_t *vmkt = &(sarg->vmkt);
  pthread_mutex_lock(&(vmkt->mut0));        // back-sync #1 ...
  pthread_cond_signal(&(vmkt->cond0));      // . back-sync #1 .
  pthread_mutex_unlock(&(vmkt->mut0));      // ... back-sync #1
  // now we know that vmkt_create is past pthread_create()
  // ... and we are between back-sync #1 and #2
  pthread_mutex_lock(&(vmkt->mut1));        // prepare this thread's mutex
  pthread_mutex_lock(&(vmkt->mut_extra1));  // prepare this thread's mutex
  // fill in the tid for this thread
  sarg->pthid = sarg->vmkt.tid;
  // now use vmkt features to prepare for catch/release loop (back-sync)
  // - part 2
  pthread_mutex_lock(&(vmkt->mut_extra2));    // back-sync #2 ...
  pthread_cond_signal(&(vmkt->cond_extra2));  // . back-sync #2 .
  pthread_mutex_unlock(&(vmkt->mut_extra2));  // ... back-sync #2
#if (VERBOSITY > 5)
  fprintf(stderr, "blocker is past back-sync #2\n");  
#endif  
  volatile int *f = &(vmkt->flag);
  // now enter the catch/release loop
  for(;;){
    //sleep(2); // put this in the code to verify that earlier received signals
    // will be pending on a per thread basis...
#if (VERBOSITY > 5)
    fprintf(stderr,"vmk_block: thread %d: %d going to wait for release, pid: %d\n",
      vmkt->tid, pthread_self(), getpid());
#endif
    pthread_cond_wait(&(vmkt->cond1), &(vmkt->mut1));
#if (VERBOSITY > 5)
    fprintf(stderr,"vmk_block: thread %d: %d was released, pid:%d\n", vmkt->tid,
      pthread_self(), getpid());
#endif
    
    if (*f==1) break; // check whether this was a wrap up call


  // This blocker thread is responsible for staying alive until resources,
  // i.e. cores, become available to the contributing pet. The contributing
  // pet is blocked (asynchonously) via pthread_cond_wait().
#if (VERBOSITY > 5)
  fprintf(stderr, "I am a blocker for pid %d, my tid is %d and going to sleep...\n",
    getpid(), pthread_self());
#endif

  // suspend this thread until awoken by one of the sigcatcher threads  
  pthread_cond_wait(&(vmkt->cond_extra1), &(vmkt->mut_extra1));

#if (VERBOSITY > 5)
//  fprintf(stderr, "I am a blocker for pid %d, my tid is %d and\n"
//    "woke up with signal: %d. I'll exit and therefore free block on my pet\n",
//    getpid(), pthread_self(), caught);
  fprintf(stderr, "I am a blocker for pid %d, my tid is %d and\n"
    "woke up because of condition. I'll exit and therefore free block on my"
    "pet\n", getpid(), pthread_self());
#endif
  // once the signal has been received from a sigcatcher the blocker can 
  // go into the catch section before returning to wait for release

    // now signal to parent thread that child is done with its work
    pthread_mutex_lock(&(vmkt->mut0)); // wait until parent has reached "catch"
    pthread_cond_signal(&(vmkt->cond0)); // then signal that child is done
    pthread_mutex_unlock(&(vmkt->mut0)); // release the mutex lock for parent
  }
  return NULL;
}


void *VMK::startup(class VMKPlan *vmp, 
  void *(fctp)(void *, void *), void *cargo, int *rc){
#if (VERBOSITY > 9)
  vmp->vmkplan_print();
#endif
  // enter a vm derived from current vm according to the VMKPlan
  // need as many spawn_args as there are threads to be spawned from this pet
  // this is so that each spawned thread does not have to be worried about this
  // info to disappear while still accessing it
  int at_least_1 = vmp->spawnflag[mypet];
  if (at_least_1 < 1)
    at_least_1 = 1;
  SpawnArg *sarg = new SpawnArg[at_least_1];
  // set rc to indicate "no error". pthread_create() (if used) will set rc below
  *rc = 0;
  // first handle the simple case of using the parent VM
  if (vmp->parentVMflag){
    sarg[0].myvm = this;
    sarg[0].fctp = fctp;
    sarg[0].cargo = cargo;
    return sarg;
  }
  // now:
  //    sarg[] has as many elements as mypet spawns threads, but at least one
  // next, allocate as many vm objects off the heap as there will be spawned
  // next, set pointers in sarg to the VMK instances on the heap
  for (int i=0; i<vmp->spawnflag[mypet]; i++){
    if (vmp->myvms == NULL){
      fprintf(stderr, "VM_ERROR: No vm objects provided.\n");
      MPI_Abort(default_mpi_c, 0);
    } 
    sarg[i].myvm = vmp->myvms[i];
  }
  // next, determine new_npets and new_mypet_base ...
  int new_mypet_base=0;
  int new_npets=0;
  int found_my_pet_flag = 0;
  for (int ii=0; ii<npets; ii++){
    int i = vmp->petlist[ii];   // indirection to preserve petlist order
    new_npets += vmp->spawnflag[i];
    if (mypet == i) found_my_pet_flag = 1;
    if (!found_my_pet_flag){
      new_mypet_base += vmp->spawnflag[i];
    }
  }
  // now:
  //    new_npets is equal to the total number of pets in the new VMK
  //    new_mypet_base is the index of the first new pet that mypet will spawn
  // next, allocate temporary arrays ...
  int *new_lpid = new int[new_npets];
  int *new_pid = new int[new_npets];
  int *new_tid = new int[new_npets];
  int *new_ncpet = new int[new_npets];
  int *new_ncontributors = new int[new_npets];
  int **new_cid = new int*[new_npets];
  contrib_id **new_contributors = new contrib_id*[new_npets];
  // local variables, unallocated yet...
  pthread_mutex_t *new_pth_mutex2;
  pthread_mutex_t *new_pth_mutex;
  int *new_pth_finish_count;
  ipshmAlloc **new_ipshmTop;
  pthread_mutex_t *new_ipshmMutex;
  pthread_mutex_t *new_ipSetupMutex;
  // utility variables that will be used beyond the next i-loop
  int num_diff_pids=0;  // total number of different pids/lpids in new VMK
  // utility arrays and variables used only during the next i-loop
  int *keep_max_tid = new int[npets]; // sum of threads that will be spawned
  int new_petid=0;      // used for keeping track of new_petid in loop
  // next, run through all current pets and check the VMKPlan ...
  // inside the following loop pet "i" will be refered to as "this pet"
  for (int ii=0; ii<npets; ii++){
    int i = vmp->petlist[ii];   // indirection to preserve petlist order
    // get the last max_tid count of a pet with same pid
    int local_tid = 0;
    for (int j=0; j<ii; j++)
      if (pid[j]==pid[ii])
        local_tid = keep_max_tid[j];
    // now:
    //    local_tid is the tid index for the first pet this pet might spawn
    // next, if this pet spawns determine whether a previous spaner had same pid
    int temp_lpid;
    if (vmp->spawnflag[i]){
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
    for (int j=0; j<vmp->spawnflag[i]; j++){
      // here j is the counter over threads this pet spawns
      new_lpid[new_petid]=temp_lpid;  // new lpid is that previously determined
      new_pid[new_petid]=pid[i];      // new pid is equal to that of this pet
      new_tid[new_petid]=local_tid;   // new tid is continuous count per pid
      // next, determine how many cores the new pet will have & its contributors
      new_ncpet[new_petid]=0;         // reset the counter
      new_ncontributors[new_petid]=0; // reset the counter
      for (int kk=0; kk<npets; kk++){
        int k = vmp->petlist[kk];   // indirection to preserve petlist order
        if (vmp->contribute[k]==i && vmp->cspawnid[k]==j){
          // pet k contributes to this pet's spawned thread number j
          new_ncpet[new_petid]+=ncpet[k]; // add in all the cores from pet k
          if (k!=i){
            // this contribution came from _another_ pet
            ++new_ncontributors[new_petid]; // increase count of contributors
          }
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
      for (int kk=0; kk<npets; kk++){
        int k = vmp->petlist[kk];  // indirection to preserve petlist order
        if (vmp->contribute[k]==i && vmp->cspawnid[k]==j){
          // found a contributor pet (k) which contributes cores
          // to this pet (i) for its spawned thread (j)
          // next, determine the contrib_id of pet (k) and share info with pet i
          if (k!=i){
            // only if contributor pet (k) is different from this receiver pet i
            if (mypet==k){
              // mypet (k) contributes to this pet (i)
              // mypet does not spawn but contributes -> spawn blocker thread
              *rc = vmkt_create(&(sarg[0].vmkt), vmk_block, (void *)&sarg[0]);
              if (*rc) return NULL;  // could not create pthread -> bail out
              // also spawn sigcatcher thread
              *rc = vmkt_create(&(sarg[0].vmkt_extra), vmk_sigcatcher, 
                (void *)&sarg[0]);
#if (VERBOSITY > 5)
              fprintf(stderr, "parent thread is back from vmkt_create()s "
                "for vmk_block and vmk_sigcatcher\n");
#endif
              if (*rc) return NULL;  // could not create pthread -> bail out
              // fill in the info about mypet contibuting...
              new_contributors[new_petid][ncontrib_counter].blocker_tid =
                sarg[0].pthid;
              new_contributors[new_petid][ncontrib_counter].blocker_vmkt =
                &(sarg[0].vmkt);
              new_contributors[new_petid][ncontrib_counter].mpi_pid =
                pid[mypet];
              new_contributors[new_petid][ncontrib_counter].pid = getpid();
              new_contributors[new_petid][ncontrib_counter].tid =
                 pthread_self();
              // LinuxThreads (pre-NPTL) have the problem that each thread
              // comes with a different PID. Thus the stored PID must be that
              // of the sigcatcher in order to work. For correct posix behavior
              // that pid will be identical to the one out of the parent thread
              // obtained with getpid() above, and thus won't break posix!
              // The sigcatcher will have put the correct pid into the
              // vmkt_extra.arg during its vmkt_create() call, so we can
              // simply pull it out of there and replace the pid member.
              new_contributors[new_petid][ncontrib_counter].pid = 
                *(pid_t *)sarg[0].vmkt_extra.arg;
              // send contributor info over to this pet (i) that receives cores
#if (VERBOSITY > 5)
              fprintf(stderr, "sending...\n");
#endif
              send(&new_contributors[new_petid][ncontrib_counter],
                sizeof(contrib_id), i);
#if (VERBOSITY > 5)
              fprintf(stderr, "send off contrib_id for later wake-up signal: \n"
                " blocker_tid: %d\n mpi_pid: %d\n pid: %d\n tid: %d\n",
                new_contributors[new_petid][ncontrib_counter].blocker_tid,
                new_contributors[new_petid][ncontrib_counter].mpi_pid,
                new_contributors[new_petid][ncontrib_counter].pid,
                new_contributors[new_petid][ncontrib_counter].tid);
#endif
            }else if (mypet==i){
              // mypet is this pet (i)-> receiver of cores from _another_ pet k
#if (VERBOSITY > 5)
              fprintf(stderr, "receiving...\n");
#endif
              recv(&new_contributors[new_petid][ncontrib_counter],
                sizeof(contrib_id), k);
#if (VERBOSITY > 5)
              fprintf(stderr, "received contrib_id for later wake-up signal: \n"
                " blocker_tid: %d\n mpi_pid: %d\n pid: %d\n tid: %d\n",
                new_contributors[new_petid][ncontrib_counter].blocker_tid,
                new_contributors[new_petid][ncontrib_counter].mpi_pid,
                new_contributors[new_petid][ncontrib_counter].pid,
                new_contributors[new_petid][ncontrib_counter].tid);
#endif
            }else{
              // mypet has nothing to do with this contribution of cores
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
      // which will become pet (new_petid) in new VMK
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
  //    num_diff_pids is the total number of different pids in new VMK
#if (VERBOSITY > 9)
  printf(">>>>>>>>> num_diff_pids for new VMK = %d\n", num_diff_pids);
#endif
  //
  // next, set up temporary arrays lpid_list and pet_list to facititate 
  // MPI_Comm creation and shared memory allocation for new VMK
  int **lpid_list = new int*[2];
  lpid_list[0] = new int[num_diff_pids]; // this dimension holds lpids
  lpid_list[1] = new int[num_diff_pids]; // this dimension holds number of pets
  int **pet_list = new int*[num_diff_pids];
  for (int i=0; i<num_diff_pids; i++){
    lpid_list[0][i] = -1;  // invalidate the lpid entry
    lpid_list[1][i] = 0;   // no pets associated yet
    pet_list[i] = new int[npets];  // npets is maximum possible number here!
  }
  for (int ii=0; ii<npets; ii++){
    int i = vmp->petlist[ii];     // indirection to preserve petlist order
    if (vmp->spawnflag[i]){
      // this pet will spawn, so look if its lpid has already been recorded
      int j;
      for (j=0; j<num_diff_pids; j++)
        if (lpid_list[0][j]==lpid[i] || lpid_list[0][j]==-1) break;
      lpid_list[0][j] = lpid[i];  // store lpid (does not matter to overwrite)
      pet_list[j][lpid_list[1][j]] = i;  //enter the current pet id
      ++lpid_list[1][j];         // increment pet count for this pid
    }
  }

#if (VERBOSITY > 9)
  printf("finished setting up lpid_list and pet_list\n");
#endif
  
  // now:
  //    lpid_list[0][] list of current lpids with at least one pet spawning
  //    lpid_list[1][] associated list indicating how many pets will spawn
  //    pet_list[][] list of current pets that spawn
  
  // next, determine how many new pets are going to be in the thread group
  // that's running in the same VAS as mypet
  int mypetNewThreadGroupSize = 0;  // reset
  for (int i=0; i<num_diff_pids; i++){
    for (int j=0; j<lpid_list[1][i]; j++){
      if (mypet == pet_list[i][j]){
        for (int pet=0; pet<new_npets; pet++)
          if (pid[mypet] == new_pid[pet])
            ++mypetNewThreadGroupSize;
      }
    }
  }
  
  // A new_commarray will be allocated for every PET that runs in a VAS
  // that is going to have threads in the new VMK.
  // The new_commarray is a temporary data structure that will be deleted
  // for every PET at the end of this routine.
  comminfo **new_commarray;
  bool new_commarray_delete_flag = false; // reset
  if (mypetNewThreadGroupSize){
    new_commarray_delete_flag = true; // set
    new_commarray = new comminfo*[mypetNewThreadGroupSize];
    for (int i=0; i<mypetNewThreadGroupSize; i++){
      new_commarray[i] = new comminfo[mypetNewThreadGroupSize];
    }
  }
  
  // next, create MPI group
  // since this is a local MPI operation each pet can call this here...
  MPI_Group new_mpi_g;
  
  // the new group will be derived from the mpi_g_part group so there is an
  // additional level of indirection here
  int *grouplist = new int[num_diff_pids];
  for (int i=0; i<num_diff_pids; i++){
    grouplist[i] = vmp->lpid_mpi_g_part_map[lpid_list[0][i]];
  }
  
#if (VERBOSITY > 9)
  printf("successfully derived new_mpi_g\n");
#endif

  // setting up MPI communicators is a collective MPI communication call
  // thus it requires that exactly one pet of each process running in the 
  // current VMK makes that call, even if this process will not participate
  // in the new VMK...
  MPI_Comm new_mpi_c;
  
  int foundfirstflag=0;
  int foundfirstpet;
  int mylpid = lpid[mypet];
  for (int ii=0; ii<vmp->nplist; ii++){
    int i = vmp->petlist[ii];     // indirection to preserve petlist order
    if (mylpid == lpid[i]){
      // found lpid match
      if (!foundfirstflag){
        // found first pet with that spawns under this lpid
        foundfirstflag = 1;
        foundfirstpet = i;
      }
      if (mypet == i){
        // I am this pet
        if (foundfirstpet == i){
          // I am the first under this lpid and must create communicator
          MPI_Group_incl(vmp->mpi_g_part, num_diff_pids, grouplist, &new_mpi_g);
          MPI_Comm_create(vmp->mpi_c_part, new_mpi_g, &new_mpi_c);
        }else{
          // I am not the first under this lpid and must receive 
#if (VERBOSITY > 9)
          printf("mypet %d recvs new_mpi_c from %d\n", mypet, foundfirstpet);
#endif
          recv(&new_mpi_c, sizeof(MPI_Comm), foundfirstpet);
        }
      }else if (mypet == foundfirstpet){
        // I am the master and must send the communicator
#if (VERBOSITY > 9)
        printf("mypet %d sends new_mpi_c to pet %d\n", mypet, i);
#endif
        send(&new_mpi_c, sizeof(MPI_Comm), i);
      }
    }
  }
  delete [] grouplist;
  
#if (VERBOSITY > 9)
  printf("now valid new_mpi_g and new_mpi_c exist\n");
#endif

  // now:
  //    new_mpi_g is the valid MPI_Group for the new VMK
  //    new_mpi_c is the valid MPI_Comm for the new VMK
  //
  // Next, setting up intra-process shared memory connection between
  // qualifying pets of the new VMK. Only one of the current pets that
  // spawn for a certain lpid must allocate memory for the shared variables 
  // and then send this info to all the associated intra- or inter-process pets
  // of the current VMK which also spawn threads.
  for (int i=0; i<num_diff_pids; i++){
    // consider all of the different lpids of current VMK
    if (lpid_list[0][i]>-1){
      // at least one pet of the current VMK with this lpid will spawn
#if (VERBOSITY > 9)
      printf("setting up shared memory variables for lpid=%d\n", 
        lpid_list[0][i]);
#endif
      // allocate and initialize memory 
      if (mypet==pet_list[i][0]){
        // mypet is the first in the list of those that spawn from this lpid
        // -> will allocate shared memory variables
#if (VERBOSITY > 9)
        printf("mypet is first one for lpid -> allocating shared memory\n");
#endif
        // initialize shared variables
        new_pth_mutex2 = new pthread_mutex_t;
        pthread_mutex_init(new_pth_mutex2, NULL);
        new_pth_mutex = new pthread_mutex_t;
        pthread_mutex_init(new_pth_mutex, NULL);
        new_pth_finish_count = new int;
        *new_pth_finish_count = 0;
        // initialize the IntraProcessSharedMemoryAllocation Table
        new_ipshmTop = new ipshmAlloc*;
        *new_ipshmTop = NULL;  // reset
        new_ipshmMutex = new pthread_mutex_t;
        pthread_mutex_init(new_ipshmMutex, NULL);
        new_ipSetupMutex = new pthread_mutex_t;
        pthread_mutex_init(new_ipSetupMutex, NULL);
        // set up the shared_mp structure within the new_commarray
        int pet1Index = 0;  // reset
        for (int pet1=0; pet1<new_npets; pet1++){
          if (new_pid[pet1]==pid[mypet]){
            int pet2Index = 0;  // reset
            for (int pet2=0; pet2<new_npets; pet2++){
              if (new_pid[pet2]==pid[mypet]){
#ifdef ESMF_MPIUNI
                // pet1==pet2==mypet==0
                // -> allocate shared_mp structure for such PETs
                new_commarray[pet1Index][pet2Index].shmp = new shared_mp;
                // reset the shms structure in shared_mp preparing for use
                sync_reset(&(new_commarray[pet1Index][pet2Index].shmp->shms));
                new_commarray[pet1Index][pet2Index].comm_type =
                  VM_COMM_TYPE_MPIUNI;
#else
                if (pet1 != pet2){
                  // pet1 and pet2 are different PETs that run in mypet's VAS
                  // -> allocate shared_mp structure for such PETs
                  new_commarray[pet1Index][pet2Index].shmp = new shared_mp;
                  // reset the shms structure in shared_mp preparing for use
                  sync_reset(&(new_commarray[pet1Index][pet2Index].shmp->shms));
                  // don't modify intra-PET comm_type
                  if (vmp->pref_intra_process == PREF_INTRA_PROCESS_SHMHACK){
                    new_commarray[pet1Index][pet2Index].comm_type =
                      VM_COMM_TYPE_SHMHACK;
                  }else if(vmp->pref_intra_process==PREF_INTRA_PROCESS_PTHREAD){
                    new_commarray[pet1Index][pet2Index].comm_type =
                      VM_COMM_TYPE_PTHREAD;
                    // initialize pthread variables in shared_mp
                    pthread_mutex_init(
                      &(new_commarray[pet1Index][pet2Index].shmp->mutex1),
                      NULL);
                    pthread_cond_init(
                      &(new_commarray[pet1Index][pet2Index].shmp->cond1),
                      NULL);
                    pthread_mutex_init(
                      &(new_commarray[pet1Index][pet2Index].shmp->mutex2),
                      NULL);
                    pthread_cond_init(
                      &(new_commarray[pet1Index][pet2Index].shmp->cond2),
                      NULL);
                    new_commarray[pet1Index][pet2Index].shmp->tcounter = 0;
                  }
                }else{
                  new_commarray[pet1Index][pet2Index].comm_type =
                      VM_COMM_TYPE_MPI1;  // default for selfcommunication
                }
#endif               
                ++pet2Index;
              }
            }
            ++pet1Index;
          }
        }
      }
      // share pointers with all current pets that also spawn for same pid/lpid
      for (int j=1; j<lpid_list[1][i]; j++){
        int pet_dest = pet_list[i][j];
        int pet_src = pet_list[i][0];
        if (mypet==pet_src){
          // mypet is the first pet in the list -> mypet allocated -> must send
          send(&new_pth_mutex2, sizeof(pthread_mutex_t*), pet_dest);
          send(&new_pth_mutex, sizeof(pthread_mutex_t*), pet_dest);
          send(&new_pth_finish_count, sizeof(int*), pet_dest);
          send(&new_ipshmTop, sizeof(ipshmAlloc*), pet_dest);
          send(&new_ipshmMutex, sizeof(pthread_mutex_t*), pet_dest);
          send(&new_ipSetupMutex, sizeof(pthread_mutex_t*), pet_dest);
          send(&new_commarray, sizeof(comminfo**), pet_dest);
        }else if(mypet==pet_dest){
          // mypet is one of the pets that also spawn for this lpid -> receive
          // before this PETs new_commarray is overridden it must be deleted
          for (int ii=0; ii<mypetNewThreadGroupSize; ii++)
            delete [] new_commarray[ii];
          delete [] new_commarray;
          new_commarray_delete_flag = false;  // mypet doesn't delete again
          // now this PET is ready to receive the pointers for shared variables
          recv(&new_pth_mutex2, sizeof(pthread_mutex_t*), pet_src);
          recv(&new_pth_mutex, sizeof(pthread_mutex_t*), pet_src);
          recv(&new_pth_finish_count, sizeof(int*), pet_src);
          recv(&new_ipshmTop, sizeof(ipshmAlloc*), pet_src);
          recv(&new_ipshmMutex, sizeof(pthread_mutex_t*), pet_src);
          recv(&new_ipSetupMutex, sizeof(pthread_mutex_t*), pet_src);
          recv(&new_commarray, sizeof(comminfo**), pet_src);
        }
      }
    } // at least one PET of the current VMK will spawn from this lpid
  } // i
  // now:
  //    new_pth_mutex2 is valid pthread_mutex
  //    new_pth_mutex is valid pthread_mutex
  //    new_pth_finish_count is valid shared memory counter
  //    new_commarray now holds valid shared memory shared_mp objects
  //
  // next, enter the spawn-loop for mypet 
  for (int i=0; i<vmp->spawnflag[mypet]; i++){
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
    sarg[i].sendChannel = new comminfo[new_npets];
    sarg[i].recvChannel = new comminfo[new_npets];
    int new_mypet = sarg[i].mypet;
    int new_mypetIndex = 0; // reset
    for (int j=0; j<new_mypet; j++)
      if (new_pid[j] == new_pid[new_mypet])
        ++new_mypetIndex;
    int petIndex = 0; // reset
    for (int j=0; j<new_npets; j++){
      if (new_pid[j] == new_pid[new_mypet]){
        // new pet j and new_mypet will run in the same VAS
        // -> copy new_commarray entry into sendChannel and recvChannel
        sarg[i].sendChannel[j] = new_commarray[new_mypetIndex][petIndex];
        sarg[i].recvChannel[j] = new_commarray[petIndex][new_mypetIndex];
        ++petIndex;
      }else{
        // default inter-process communication via MPI1
        sarg[i].recvChannel[j].comm_type = VM_COMM_TYPE_MPI1;
        sarg[i].sendChannel[j].comm_type = VM_COMM_TYPE_MPI1;
      }
    }
    sarg[i].ipshmTop = new_ipshmTop;
    sarg[i].ipshmMutex = new_ipshmMutex;
    sarg[i].ipSetupMutex = new_ipSetupMutex;
    sarg[i].pref_intra_ssi = vmp->pref_intra_ssi;
    // cargo
    sarg[i].cargo = cargo;
    // threading stuff
    sarg[i].nothreadsflag = vmp->nothreadflag;
    if (vmp->nothreadflag){
      // for a VM that is not thread-based the VM can already be constructed
      // obtain reference to the vm instance on heap
      VMK &vm = *(sarg[0].myvm);
      // setup the pet section in this vm instance
      sarg[0].pthid = pthread_self();
      vm.construct((void *)&sarg[0]);
    }else{
      // if this is a thread-based VM then...
      // ...finally spawn threads from this pet...
      // in the thread-based case the VM cannot be constructured until the
      // pthreadID is known!
      *rc = vmkt_create(&(sarg[i].vmkt), vmk_spawn, (void *)&sarg[i]);
      if (*rc) return NULL;  // could not create pthread -> bail out
    }
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
  if (new_commarray_delete_flag){
    // mypet must deallocate its new_commarray
    for (int ii=0; ii<mypetNewThreadGroupSize; ii++)
      delete [] new_commarray[ii];
    delete [] new_commarray;
  }
  // return info that is associated with the new VMK...
  return sarg;
}


void VMK::enter(class VMKPlan *vmp, void *arg, void *argvmkt){
  // Enter into VMK by its registered function, i.e. release vmkt
  // First need to cast arg into its correct type
  SpawnArg *sarg = (SpawnArg *)arg;
  // simple case is that where the child runs in the parent VM, then all this
  // degenerates into a simple blocking callback 
  if (vmp->parentVMflag){
    if (argvmkt==NULL)
      sarg[0].fctp((void *)sarg[0].myvm, sarg[0].cargo);
    else
      sarg[0].fctp((void *)sarg[0].myvm, argvmkt);
    return;
  }
  // the non-thread based VMs simply do a blocking callback for all the 
  // spawning PETs.
  if (vmp->nothreadflag && vmp->spawnflag[mypet]==1){
    if (argvmkt==NULL)
      sarg[0].fctp((void *)sarg[0].myvm, sarg[0].cargo);
    else
      sarg[0].fctp((void *)sarg[0].myvm, argvmkt);
    return;
  }
  // pets that do not spawn but contribute need to release their blocker and
  // sigcatcher _before_ the actual spawner threads get released 
  // (this is so that no signals get missed!)
  if (vmp->spawnflag[mypet]==0){
    if (vmp->contribute[mypet]>-1){
      vmkt_release(&(sarg[0].vmkt), NULL);          // release blocker
      vmkt_release(&(sarg[0].vmkt_extra), NULL);    // release sigcatcher
      send(NULL, 0, vmp->contribute[mypet]);     // tell spawner about me
    }
  }else{
    // wait on all the contributors to this spawner
    for (int i=0; i<npets; i++)
      if (vmp->contribute[i]==mypet && i!=mypet)
        recv(NULL, 0, i); // listen if contributor has released its threads
    // now all contributors are in, pets that spawn need to release their vmkts
    for (int i=0; i<vmp->spawnflag[mypet]; i++){
#if (VERBOSITY > 9)
      fprintf(stderr, "gjt in VMK::enter(): release &(sarg[%d].vmkt)=%d\n", i,
        &(sarg[i].vmkt));
#endif
      vmkt_release(&(sarg[i].vmkt), argvmkt);
    }
  }
}


void VMK::exit(class VMKPlan *vmp, void *arg){
  // Exit from VMK's registered function, i.e. catch the vmkts
  // First need to cast arg into its correct type
  SpawnArg *sarg = (SpawnArg *)arg;
  // simple case is that where the child runs in the parent VM, then there is
  // nothing to catch on exit.
  if (vmp->parentVMflag) return;
  // check if this is a thread-based VM
  if (!vmp->nothreadflag){
    // pets that spawn in a thread-based VM need to catch their vmkts
    for (int i=0; i<vmp->spawnflag[mypet]; i++)
      vmkt_catch(&(sarg[i].vmkt));
    // pets that did not spawn but contributed need to catch their blocker and
    // sigcatcher
    if (vmp->spawnflag[mypet]==0 && vmp->contribute[mypet]>-1){
      vmkt_catch(&(sarg[0].vmkt));
      vmkt_catch(&(sarg[0].vmkt_extra));
    }
  }
  // The following threadbarrier ensures that each parent PET blocks until 
  // all threads that work in the PET-local VAS have completed.
  threadbarrier();
}


void VMK::shutdown(class VMKPlan *vmp, void *arg){
  // Block all pets of the current VMK until their individual resources,
  // i.e. cores, become available. This means:
  //  1) pets which did not spawn nor contribute in the VMKPlan are not
  //     blocked
  //  2) pets which did not spawn but contributed in the VMKPlan block
  //     until their cores have been returned by spwawned VMK
  //  3) pets that spwan will be blocked until _all_ of _their_ spawned threads
  //     exit in order to delete allocated info structure
  // First need to cast arg into its correct type
  SpawnArg *sarg = (SpawnArg *)arg;
  // simple case is that where the child runs in the parent VM, then there is
  // nothing to clean up here except to free sarg;
  if (vmp->parentVMflag){
    delete [] sarg;
    return;
  }
  for (int i=0; i<vmp->spawnflag[mypet]; i++){
    if (vmp->nothreadflag){
      // obtain reference to the vm instance on heap
      VMK &vm = *(sarg[0].myvm);
      // destroy this vm instance
      vm.destruct();
    }else{
      // thread-based VM pets must be joined
      vmkt_join(&(sarg[i].vmkt));
    }
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
  // need to block pets that did not spawn but contributed in the VMKPlan
  if (vmp->spawnflag[mypet]==0 && vmp->contribute[mypet]>-1){
    // wait for the blocker thread to return, maybe already is done...
#if (VERBOSITY > 9)
    printf("I am pet, pid %d, tid %d. I'll block on a vmkt_join until "
      "blocker returns.\n", getpid(), pthread_self());
#endif
    vmkt_join(&(sarg[0].vmkt));         // vmk_block
    vmkt_join(&(sarg[0].vmkt_extra));   // vmk_sigcatcher
#if (VERBOSITY > 9)
    printf("I am pet, pid %d, tid %d. My blocker returned\n", 
      getpid(), pthread_self());
#endif
  }
  // done holding info in SpawnArg array -> delete now
  delete [] sarg;
  // done blocking...
}


void VMK::print()const{
  // print info about the VMK object
  printf("--- VMK::print() start ---\n");
  printf("vm located at: %p\n", this);
  printf("npets = %d, mypet=%d\n", npets, mypet);
  printf("  pth_mutex =\t\t %p\n"
         "  pth_finish_count =\t %p\n",
    pth_mutex, pth_finish_count);
  int size, rank;
  MPI_Group_size(mpi_g, &size);
  printf("MPI_Group_size: %d\n", size);
  MPI_Comm_size(mpi_c, &size);
  printf("MPI_Comm_size: %d\n", size);
  MPI_Comm_rank(mpi_c, &rank);
  printf("MPI_Comm_rank in local MPI communicator: %d\n", rank);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  printf("MPI_Comm_rank in MPI_COMM_WORLD: %d\n", rank);
  printf("MPI thread level support: %d\n", mpi_thread_level);
  printf("mpi_mutex_flag: %d\n", mpi_mutex_flag);
  printf("mpionly: %d\n", mpionly);
  printf("nothreadsflag: %d\n", nothreadsflag);
  for (int i=0; i<npets; i++){
    printf("  lpid[%d]=%d, tid[%d]=%d, vas[%d]=%d, ncpet[%d]=%d",
      i, lpid[i], i, tid[i], i, pid[i], i, ncpet[i]);
    for (int j=0; j<ncpet[i]; j++)
      printf(", cid[%d][%d]=%d", i, j, cid[i][j]);
    printf("\n");
  }
  printf("ncores = %d\n", ncores);
  for (int i=0; i<ncores; i++)
    printf("  cpuid[%d]=%d, ssiid[%d]=%d\n", i, cpuid[i], i, ssiid[i]);
  printf("--- VMK::print() end ---\n");
}


int VMK::getNpets(){
  return npets;
}


int VMK::getMypet(){
  return mypet;
}


pthread_t VMK::getMypthid(){
  return mypthid;
}


int VMK::getNcpet(int i){
  return ncpet[i];
}


int VMK::getSsiid(int i){
  return ssiid[cid[i][0]];
}


MPI_Comm VMK::getMpi_c(){
  return mpi_c;
}


int VMK::getNthreads(int i){
  int n=0;
  for (int j=0; j<npets; j++)
    if (pid[j]==pid[i]) ++n;
  return n;
}


int VMK::getTid(int i){
  return tid[i];
}

int VMK::getVas(int i){
  return pid[i];
}

int VMK::getLpid(int i){
  return lpid[i];
}

// --- VMKPlan methods ---


VMKPlan::VMKPlan(){
  // native constructor
  nothreadflag = 1; // by default use non-threaded VMs
  parentVMflag = 0; // default is to create a new VM for every child
  // invalidate the arrays
  spawnflag = NULL;
  contribute = NULL;
  cspawnid = NULL;
  // invalidate the VMK pointer array
  myvms = NULL;
  // set the default communication preferences
  pref_intra_process = PREF_INTRA_PROCESS_SHMHACK;
  pref_intra_ssi = PREF_INTRA_SSI_MPI1;
  pref_inter_ssi = PREF_INTER_SSI_MPI1;
  // invalidate members that deal with communicator of participating PETs
  lpid_mpi_g_part_map = NULL;
  commfreeflag = 0;
  groupfreeflag = 0;
}


VMKPlan::~VMKPlan(){
  // native destructor
  vmkplan_garbage();
  if (lpid_mpi_g_part_map != NULL){
    delete [] lpid_mpi_g_part_map;
    lpid_mpi_g_part_map = NULL;
  }
  if (commfreeflag){
    MPI_Comm_free(&mpi_c_part);
    commfreeflag = 0;
  }
  if (groupfreeflag){
    MPI_Group_free(&mpi_g_part);
    groupfreeflag = 0;
  }
}

  
void VMKPlan::vmkplan_garbage(){
  // perform garbage collection within a VMKPlan object
  if (spawnflag != NULL){
    delete [] spawnflag;
    delete [] contribute;
    delete [] cspawnid;
    spawnflag = NULL;
    contribute = NULL;
    cspawnid = NULL;
    myvms = NULL;     // this does NOT deallocate VM objects!
    if (!parentVMflag)
      delete [] petlist;
  }
}


int VMKPlan::vmkplan_nspawn(){
  // return number of PETs that are being spawned out of current PET
  return nspawn;
}


void VMKPlan::vmkplan_myvms(VMK **myvms){
  // set the internal myvms pointer array
  this->myvms = myvms;
}


void VMKPlan::vmkplan_mpi_c_part(VMK &vm){
  // set up the communicator of participating PETs
  int *grouplist = new int[nplist];     // that's big enough
  int *grouppetlist = new int[nplist];  // associated list of pets
  int n=0;  // counter
  for (int i=0; i<nplist; i++){
    int pet = petlist[i];
    int lpid = vm.getLpid(pet);
    int j;
    for (j=0; j<n; j++)
      if (grouplist[j] == lpid) break;
    if (j==n){
      grouplist[n] = lpid;
      grouppetlist[n] = pet;
      ++n;
    }
  }
  
  // all parent PETs create the new group of participating PETs
  groupfreeflag = 1;
  MPI_Group_incl(vm.mpi_g, n, grouplist, &mpi_g_part);
 
  // all master PETs of the current vm must create the communicator
  int mypet = vm.getMypet();
  if (vm.getTid(mypet) == 0){
    // master PET in this VAS
    MPI_Comm_create(vm.mpi_c, mpi_g_part, &mpi_c_part);
    commfreeflag = 1;   // this PET is responsible for freeing the communicator
  }else{
    commfreeflag = 0;   // this PET is _not_ responsible for freeing the commu.
  }
  
  // reset those commfreeflags for PETs outside the group of participants
  int j;
  for (j=0; j<n; j++)
    if (mypet == grouppetlist[j]) break;
  if (j == n)
    commfreeflag = 0;

  lpid_mpi_g_part_map = new int[vm.getNpets()];
  for (int i=0; i<n; i++){
    lpid_mpi_g_part_map[grouplist[i]] = i;
  }
  
  delete [] grouppetlist;
  delete [] grouplist;
}


void VMKPlan::vmkplan_useparentvm(VMK &vm){
  // set up a VMKPlan that will run inside of parent VM
  parentVMflag = 1;
  npets = vm.npets;
  spawnflag = new int[npets];
  contribute = new int[npets];
  cspawnid = new int[npets];
  for (int i=0; i<npets; i++){
    spawnflag[i]=1;
    contribute[i]=0;
    cspawnid[i]=0;
  }
  // now deal with mypet specific members
  nspawn = spawnflag[vm.mypet];
}


void VMKPlan::vmkplan_maxthreads(VMK &vm){
  // set up a VMKPlan that will maximize the number of thread-pets
  vmkplan_maxthreads(vm, 0);
}


void VMKPlan::vmkplan_maxthreads(VMK &vm, int max){
  // set up a VMKPlan that will max. the number of thread-pets up to max
  vmkplan_maxthreads(vm, max, NULL, 0);
}


void VMKPlan::vmkplan_maxthreads(VMK &vm, int max,
  int pref_intra_process, int pref_intra_ssi, int pref_inter_ssi){
  // set up a VMKPlan that will max. the number of thread-pets up to max
  vmkplan_maxthreads(vm, max, NULL, 0, 
    pref_intra_process, pref_intra_ssi, pref_inter_ssi);
}


void VMKPlan::vmkplan_maxthreads(VMK &vm, int max, int *plist, 
  int nplist){
  // set up a VMKPlan that will max. the number of thread-pets up to max
  // but only allow PETs listed in plist to participate
  // first do garbage collection on current object
  vmkplan_garbage();
  // now set stuff up...
  nothreadflag = 0; // this plan will allow ESMF-threading
  npets = vm.npets;
  if (nplist != 0)
    this->nplist = nplist;
  else
    this->nplist = npets;
  petlist = new int[npets];
  spawnflag = new int[npets];
  contribute = new int[npets];
  cspawnid = new int[npets];
  // setup petlist
  for (int i=0; i<npets; i++)
    petlist[i] = i;   //default sequence
  if (nplist != 0){
    // an explicit petlist was provided
    for (int i=0; i<nplist; i++){
      int pet = plist[i];
      int j;
      for (j=0; j<npets; j++){
        // search for 'pet'
        if (petlist[j] == pet) break;
      }
      // j is position that holds 'pet' -> swap elements
      petlist[j] = petlist[i];
      petlist[i] = pet;
    }
  }
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
        spawnflag[i]=0;     // this PET is not spawn into new VMK
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
          spawnflag[i]=1;     // spawn at least one thread in new VMK
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
}


int VMKPlan::vmkplan_maxthreads(VMK &vm, int max, int *plist, 
  int nplist, int pref_intra_process, int pref_intra_ssi, int pref_inter_ssi){
  // set the communication preferences
  if (pref_intra_process >= 0)
    this->pref_intra_process = pref_intra_process;
  if (pref_intra_ssi >= 0)
    this->pref_intra_ssi = pref_intra_ssi;
  if (pref_inter_ssi >= 0)
    this->pref_inter_ssi = pref_inter_ssi;
  vmkplan_maxthreads(vm, max, plist, nplist);
#ifdef ESMF_NO_PTHREADS
  if (!nothreadflag) return 1; // indicate error
#endif
  return 0;
}


void VMKPlan::vmkplan_minthreads(VMK &vm){
  // set up a VMKPlan that will only have single threaded pet
  // instantiations and claim all cores of pets that don't make it through
  vmkplan_minthreads(vm, 0);
}


void VMKPlan::vmkplan_minthreads(VMK &vm, int max){
  // set up a VMKPlan that will only have single threaded pet
  // instantiations and claim all cores of pets that don't make it through, up
  // to max cores/pet
  vmkplan_minthreads(vm, max, NULL, 0);
}


void VMKPlan::vmkplan_minthreads(VMK &vm, int max, int *plist, 
  int nplist){
  // set up a VMKPlan that will only have single threaded pet
  // instantiations and claim all cores of pets that don't make it through, up
  // to max cores/pet but only allow PETs listed in plist to participate
  // first do garbage collection on current object
  vmkplan_garbage();
  // now set stuff up...
  nothreadflag = 0; // this plan will allow ESMF-threading
  npets = vm.npets;
  if (nplist != 0)
    this->nplist = nplist;
  else
    this->nplist = npets;
  petlist = new int[npets];
  spawnflag = new int[npets];
  contribute = new int[npets];
  cspawnid = new int[npets];
  // setup petlist
  for (int i=0; i<npets; i++)
    petlist[i] = i;   //default sequence
  if (nplist != 0){
    // an explicit petlist was provided
    for (int i=0; i<nplist; i++){
      int pet = plist[i];
      int j;
      for (j=0; j<npets; j++){
        // search for 'pet'
        if (petlist[j] == pet) break;
      }
      // j is position that holds 'pet' -> swap elements
      petlist[j] = petlist[i];
      petlist[i] = pet;
    }
  }
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
        spawnflag[i]=0;     // this PET is not spawn into new VMK
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
}


int VMKPlan::vmkplan_minthreads(VMK &vm, int max, int *plist,
  int nplist, int pref_intra_process, int pref_intra_ssi, int pref_inter_ssi){
  // set the communication preferences
  if (pref_intra_process >= 0)
    this->pref_intra_process = pref_intra_process;
  if (pref_intra_ssi >= 0)
    this->pref_intra_ssi = pref_intra_ssi;
  if (pref_inter_ssi >= 0)
    this->pref_inter_ssi = pref_inter_ssi;
  vmkplan_minthreads(vm, max, plist, nplist);
#ifdef ESMF_NO_PTHREADS
  if (!nothreadflag) return 1; // indicate error
#endif
  return 0;
}


void VMKPlan::vmkplan_maxcores(VMK &vm){
  // set up a VMKPlan that will have pets with the maximum number of cores
  // available
  vmkplan_maxcores(vm, 0);
}


void VMKPlan::vmkplan_maxcores(VMK &vm, int max){
  // set up a VMKPlan that will have pets with the maximum number of cores
  // available, but not more than max
  vmkplan_maxcores(vm, max, NULL, 0);
}


void VMKPlan::vmkplan_maxcores(VMK &vm, int max, int *plist,
  int nplist){
  // set up a VMKPlan that will have pets with the maximum number of cores
  // available, but not more than max and only use PETs listed in plist
  // first do garbage collection on current object
  vmkplan_garbage();
  // now set stuff up...
  nothreadflag = 0; // this plan will allow ESMF-threading
  npets = vm.npets;
  if (nplist != 0)
    this->nplist = nplist;
  else
    this->nplist = npets;
  petlist = new int[npets];
  spawnflag = new int[npets];
  contribute = new int[npets];
  cspawnid = new int[npets];
  // setup petlist
  for (int i=0; i<npets; i++)
    petlist[i] = i;   //default sequence
  if (nplist != 0){
    // an explicit petlist was provided
    for (int i=0; i<nplist; i++){
      int pet = plist[i];
      int j;
      for (j=0; j<npets; j++){
        // search for 'pet'
        if (petlist[j] == pet) break;
      }
      // j is position that holds 'pet' -> swap elements
      petlist[j] = petlist[i];
      petlist[i] = pet;
    }
  }
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
        spawnflag[i]=0;     // this PET is not spawn into new VMK
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
}


int VMKPlan::vmkplan_maxcores(VMK &vm, int max, int *plist, 
  int nplist, int pref_intra_process, int pref_intra_ssi, int pref_inter_ssi){
  // set the communication preferences
  if (pref_intra_process >= 0)
    this->pref_intra_process = pref_intra_process;
  if (pref_intra_ssi >= 0)
    this->pref_intra_ssi = pref_intra_ssi;
  if (pref_inter_ssi >= 0)
    this->pref_inter_ssi = pref_inter_ssi;
  vmkplan_maxcores(vm, max, plist, nplist);
#ifdef ESMF_NO_PTHREADS
  if (!nothreadflag) return 1; // indicate error
#endif
  return 0;
}


void VMKPlan::vmkplan_print(){
  // print info about the VMKPlan object
  printf("--- vmkplan_print start ---\n");
  printf("nothreadflag = %d\n", nothreadflag);
  printf("parentVMflag = %d\n", parentVMflag);
  printf("npets = %d\n", npets);
  for (int i=0; i<npets; i++)
    printf("  spawnflag[%d]=%d, contribute[%d]=%d, cspawnid[%d]=%d\n", 
      i, spawnflag[i], i, contribute[i], i, cspawnid[i]);
  printf("pref_intra_process:\t%d\n", pref_intra_process);
  printf("pref_intra_ssi:\t%d\n", pref_intra_ssi);
  printf("pref_inter_ssi:\t%d\n", pref_inter_ssi);
  printf("--- vmkplan_print end ---\n");
}


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~ Communication Handle and Communication Handle Queue
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


void VMK::commqueueitem_link(commhandle *ch){
  commhandle *handle;
  pthread_mutex_lock(pth_mutex2);
  if (nhandles==0){
    firsthandle=ch;
    ch->prev_handle=NULL;
    ch->next_handle=NULL;
  }else{
    handle=firsthandle;
    while (handle->next_handle!=NULL)
      handle=handle->next_handle;
    handle->next_handle=ch;
    ch->prev_handle=handle;
    ch->next_handle=NULL;
  }
  ++nhandles;
  pthread_mutex_unlock(pth_mutex2);
}

int VMK::commqueueitem_unlink(commhandle *ch){
  commhandle *handle;
  int found = 0; // reset
  pthread_mutex_lock(pth_mutex2);
  if(nhandles >= 1){
    handle=firsthandle;
    while (handle->next_handle!=NULL){
      if (handle==ch) break;
      handle=handle->next_handle;
    }
    if (handle==ch){
      // found commhandle in queue
      found = 1;
      --nhandles;
      if (handle->prev_handle==NULL){
        // commhandle was firsthandle in queue -> special treatment
        firsthandle=handle->next_handle;
        if (firsthandle != NULL)
          firsthandle->prev_handle=NULL;
      }else
        // regular unlink if commhandle was any other element in queue
        handle->prev_handle->next_handle=handle->next_handle;
      if (handle->next_handle!=NULL)
        // finish unlink by back linking next element to previous element
        handle->next_handle->prev_handle=handle->prev_handle;
    }
  }
  pthread_mutex_unlock(pth_mutex2);
  return found;
}


int VMK::commtest(commhandle **ch, int *completeFlag, status *status){
  // test all of the communications pointed to by *ch. For completed
  // ones delete all of the inside contents of *ch (even if it is a
  // tree)
  // finally unlink the *ch container from the commqueue and delete the
  // container (only) if the *ch was part of the commqueue!
//fprintf(stderr, "VMK::commwait: nhandles=%d\n", nhandles);
//fprintf(stderr, "VMK::commwait: *ch=%p\n", *ch);
  int localrc=0;
  if ((ch!=NULL) && ((*ch)!=NULL)){
    // wait for all non-blocking requests in commhandle to complete
    int localCompleteFlag = 0;
    if ((*ch)->type==0){
      // this is a commhandle container
      for (int i=0; i<(*ch)->nelements; i++){
        localrc = commwait(&((*ch)->handles[i]));  // recursive call
        delete (*ch)->handles[i];
      }
      delete [] (*ch)->handles;
    }else if ((*ch)->type==1){
      // this commhandle contains MPI_Requests
      if (status)
        status->comm_type = VM_COMM_TYPE_MPI1;
      MPI_Status *mpi_s;
      if (status)
        mpi_s = &(status->mpi_s);
      else
        mpi_s = MPI_STATUS_IGNORE;
      // TODO: status will only reflect the last communiction in the i-loop!
      for (int i=0; i<(*ch)->nelements; i++){
//fprintf(stderr, "MPI_Wait: ch=%p\n", &((*ch)->mpireq[i]));
        if (mpi_mutex_flag)
          pthread_mutex_lock(pth_mutex);
        localrc = MPI_Test(&((*ch)->mpireq[i]), &localCompleteFlag,
          mpi_s);
        if (mpi_mutex_flag)
          pthread_mutex_unlock(pth_mutex);
        if (status){
          if (lpid[mpi_s->MPI_SOURCE] == mpi_s->MPI_SOURCE)
            status->srcPet = mpi_s->MPI_SOURCE;
          else{
            for (int k=0; k<npets; k++)
              if (lpid[k] == mpi_s->MPI_SOURCE)
                status->srcPet = mpi_s->MPI_SOURCE;
          }
          status->tag     = mpi_s->MPI_TAG;
          status->error   = mpi_s->MPI_ERROR;
        }
      }
      if (localCompleteFlag)
        delete [] (*ch)->mpireq;
    }else if ((*ch)->type==-1){
      // this is a dummy commhandle and there is nothing to wait for...
      // ... but set localCompleteFlag
      localCompleteFlag = 1;      
    }else{
      printf("VMK: only MPI non-blocking implemented\n");
      localrc = VMK_ERROR;
    }
    // if this *ch is in the request queue x-> unlink and delete
    if (localCompleteFlag){
      if (commqueueitem_unlink(*ch)){ 
        delete *ch; // delete the container commhandle that was linked
        *ch = NULL; // ensure this container will not point to anything
      }
    }
    if (completeFlag != NULL)
      *completeFlag = localCompleteFlag;
  }
  return localrc;
}


int VMK::commwait(commhandle **ch, status *status, int nanopause){
  // wait for all of the communications pointed to by *ch to complete
  // and delete all of the inside contents of *ch (even if it is a tree)
  // finally unlink the *ch container from the commqueue and delete the
  // container (only) if the *ch was part of the commqueue!
//fprintf(stderr, "VMK::commwait: nhandles=%d\n", nhandles);
//fprintf(stderr, "VMK::commwait: *ch=%p\n", *ch);
  int localrc=0;
  if ((ch!=NULL) && ((*ch)!=NULL)){
    // wait for all non-blocking requests in commhandle to complete
    if ((*ch)->type==0){
      // this is a commhandle container
      for (int i=0; i<(*ch)->nelements; i++){
        localrc = commwait(&((*ch)->handles[i]));  // recursive call
        delete (*ch)->handles[i];
      }
      delete [] (*ch)->handles;
    }else if ((*ch)->type==1){
      // this commhandle contains MPI_Requests
      if (status)
        status->comm_type = VM_COMM_TYPE_MPI1;
      MPI_Status *mpi_s;
      if (status)
        mpi_s = &(status->mpi_s);
      else
        mpi_s = MPI_STATUS_IGNORE;
      // TODO: status will only reflect the last communiction in the i-loop!
      for (int i=0; i<(*ch)->nelements; i++){
//fprintf(stderr, "MPI_Wait: ch=%p\n", &((*ch)->mpireq[i]));
        if (nanopause){
          // use nanosleep to pause between tests to lower impact on CPU load
#ifdef ESMF_NO_NANOSLEEP
#else
          struct timespec dt = {0, nanopause};
#endif
          int completeFlag = 0;
          for(;;){
            if (mpi_mutex_flag)
              pthread_mutex_lock(pth_mutex);
            localrc = MPI_Test(&((*ch)->mpireq[i]), &completeFlag,
              mpi_s);
            if (mpi_mutex_flag)
              pthread_mutex_unlock(pth_mutex);
            if (completeFlag) break;
#ifdef ESMF_NO_NANOSLEEP
#else
            nanosleep(&dt, NULL);
#endif
          }
          if (status){
            if (lpid[mpi_s->MPI_SOURCE] == mpi_s->MPI_SOURCE)
              status->srcPet = mpi_s->MPI_SOURCE;
            else{
              for (int k=0; k<npets; k++)
                if (lpid[k] == mpi_s->MPI_SOURCE)
                  status->srcPet = mpi_s->MPI_SOURCE;
            }
            status->tag     = mpi_s->MPI_TAG;
            status->error   = mpi_s->MPI_ERROR;
          }
        }else{
          if (mpi_mutex_flag)
            pthread_mutex_lock(pth_mutex);
          localrc = MPI_Wait(&((*ch)->mpireq[i]), mpi_s);
          if (mpi_mutex_flag)
            pthread_mutex_unlock(pth_mutex);
          if (status){
            if (lpid[mpi_s->MPI_SOURCE] == mpi_s->MPI_SOURCE)
              status->srcPet = mpi_s->MPI_SOURCE;
            else{
              for (int k=0; k<npets; k++)
                if (lpid[k] == mpi_s->MPI_SOURCE)
                  status->srcPet = mpi_s->MPI_SOURCE;
            }
            status->tag     = mpi_s->MPI_TAG;
            status->error   = mpi_s->MPI_ERROR;
          }
        }
      }
      delete [] (*ch)->mpireq;
    }else if ((*ch)->type==-1){
      // this is a dummy commhandle and there is nothing to wait for...
    }else{
      printf("VMK: only MPI non-blocking implemented\n");
      localrc = VMK_ERROR;
    }
    // if this *ch is in the request queue x-> unlink and delete
    if (commqueueitem_unlink(*ch)){ 
      delete *ch; // delete the container commhandle that was linked
      *ch = NULL; // ensure this container will not point to anything
    }
  }
  return localrc;
}


void VMK::commqueuewait(){
  int n=nhandles;
  commhandle *fh;
  for (int i=0; i<n; i++){
//    printf("VMK::commqueuewait: %d\n", nhandles);
    fh = firsthandle;
    commwait(&fh);
  }
//  printf("VMK::commqueuewait: %d\n", nhandles);
}


void VMK::commcancel(commhandle **commh){
//fprintf(stderr, "VMK::commcancel: nhandles=%d\n", nhandles);
//fprintf(stderr, "VMK::commcancel: commh=%p\n", (*commh));
  if ((*commh)!=NULL){
    // cancel all non-blocking requests in commhandle to complete
    if ((*commh)->type==0){
      // this is a commhandle container
      for (int i=0; i<(*commh)->nelements; i++){
        commcancel(&((*commh)->handles[i]));  // recursive call
      }
    }else if ((*commh)->type==1){
      // this commhandle contains MPI_Requests
      for (int i=0; i<(*commh)->nelements; i++){
//fprintf(stderr, "MPI_Cancel: commh=%p\n", &((*commh)->mpireq[i]));
        if (mpi_mutex_flag)
          pthread_mutex_lock(pth_mutex);
        MPI_Cancel(&((*commh)->mpireq[i]));
        if (mpi_mutex_flag)
          pthread_mutex_unlock(pth_mutex);
      }
    }else{
      printf("VMK: only MPI non-blocking implemented\n");
    }
  }
}


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~ Communication Calls
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


int VMK::send(const void *message, int size, int dest, int tag){
  // p2p send
#if (VERBOSITY > 9)
  printf("sending to: %d, %d\n", dest, lpid[dest]);
#endif
  int localrc=0;
  shared_mp *shmp;
  pipc_mp *pipcmp;
  int scpsize;
  char *pdest;
  char *psrc;
  int i;
  char *mess;
  // switch into the appropriate implementation
  switch(sendChannel[dest].comm_type){
  case VM_COMM_TYPE_MPI1:
    // MPI-1 implementation
    void *messageC; // for MPI C interface convert (const void *) -> (void *)
    memcpy(&messageC, &message, sizeof(void *));
    // use mutex to serialize mpi comm calls if mpi thread support requires it
    if (mpi_mutex_flag)
      pthread_mutex_lock(pth_mutex);
    if (tag == -1) tag = 1000*mypet+dest;   // default tag to simplify debugging
    localrc = MPI_Send(messageC, size, MPI_BYTE, lpid[dest], tag, mpi_c);
    if (mpi_mutex_flag)
      pthread_mutex_unlock(pth_mutex);
    break;
  case VM_COMM_TYPE_PTHREAD:
    // Pthread implementation
    shmp = sendChannel[dest].shmp;  // shared memory mp channel
    shmp->ptr_src = message;                        // set the source pointer
    // synchronize with recv()
    pthread_mutex_lock(&(shmp->mutex1));
    shmp->tcounter++;
    if (shmp->tcounter < 2){
      // wait for recv()
      pthread_cond_wait(&(shmp->cond1), &(shmp->mutex1));
    }else{
      // reset counter and wake up recv()
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
    // synchronize with recv()
    pthread_mutex_lock(&(shmp->mutex2));
    shmp->tcounter++;
    if (shmp->tcounter < 2){
      // wait for recv()
      pthread_cond_wait(&(shmp->cond2), &(shmp->mutex2));
    }else{
      // reset counter and wake up recv()
      shmp->tcounter = 0;
      pthread_cond_broadcast(&(shmp->cond2));
    }
    pthread_mutex_unlock(&(shmp->mutex2));
    break;
  case VM_COMM_TYPE_SHMHACK:
    // Shared memory hack sync with spin-lock
    shmp = sendChannel[dest].shmp;  // shared memory mp channel
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
      // synchronize with recv()
      sync_a_flip(&shmp->shms);
      // now ptr_src and ptr_dest are valid for this message
      scpsize = size/2;   // send takes the lower half
      pdest = (char *)shmp->ptr_dest;
      psrc = (char *)shmp->ptr_src;
      // do the actual memcpy
      memcpy(pdest, psrc, scpsize);
      // synchronize with recv()
      sync_a_flop(&shmp->shms);
    }
    break;
  case VM_COMM_TYPE_POSIXIPC:
    // Shared memory hack sync with spin-lock
    pipcmp = sendChannel[dest].pipcmp;  // shared memory mp channel
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
  case VM_COMM_TYPE_MPIUNI:
    // Shared memory hack for mpiuni
    // TODO: this assumes that send will arrive first, otherwise this will hang
    shmp = sendChannel[dest].shmp;  // shared memory mp channel
    if (size<=SHARED_BUFFER){
      // buffer is sufficient
      pdest = shmp->buffer;
      // wait until buffer is ready to be used
      sync_buffer_wait_empty(&shmp->shms, 0);
      // do the actual memcpy
      memcpy(pdest, message, size);
      // set flag indicating that send's memcpy() is done and buffer is valid
      sync_buffer_flag_fill(&shmp->shms, 0);
    }else{
      // buffer is insufficient
      // todo: need to throw error
    }
    break;
  default:
    printf("unknown comm_type.\n");
    break;
  }
  return localrc;
}


int VMK::send(const void *message, int size, int dest, commhandle **ch,
  int tag){
  // p2p send
//fprintf(stderr, "VMK::send: ch=%p\n", *ch);
#if (VERBOSITY > 9)
  printf("sending to: %d, %d\n", dest, lpid[dest]);
#endif
  int localrc=0;
  shared_mp *shmp;
  pipc_mp *pipcmp;
  int scpsize;
  char *pdest;
  char *psrc;
  int i;
  char *mess;
  // check if this needs a new entry in the request queue
  if (*ch==NULL){
    *ch = new commhandle;
    commqueueitem_link(*ch);
  }
  // switch into the appropriate implementation
  switch(sendChannel[dest].comm_type){
  case VM_COMM_TYPE_MPI1:
    (*ch)->nelements=1;
    (*ch)->type=1;
    (*ch)->mpireq = new MPI_Request[1];
    // MPI-1 implementation
    void *messageC; // for MPI C interface convert (const void *) -> (void *)
    memcpy(&messageC, &message, sizeof(void *));
    // use mutex to serialize mpi comm calls if mpi thread support requires it
    if (mpi_mutex_flag)
      pthread_mutex_lock(pth_mutex);
//fprintf(stderr, "MPI_Isend: ch=%p\n", (*ch)->mpireq);
    if (tag == -1) tag = 1000*mypet+dest;   // default tag to simplify debugging
    localrc = MPI_Isend(messageC, size, MPI_BYTE, lpid[dest], tag, mpi_c, 
      (*ch)->mpireq);
    if (mpi_mutex_flag)
      pthread_mutex_unlock(pth_mutex);
    break;
  case VM_COMM_TYPE_PTHREAD:
    // Pthread implementation
    printf("non-blocking send not implemented for VM_COMM_TYPE_PTHREAD.\n");
    break;
  case VM_COMM_TYPE_SHMHACK:
    // Shared memory hack sync with spin-lock
    printf("non-blocking send not implemented for VM_COMM_TYPE_SHMHACK.\n");
    break;
  case VM_COMM_TYPE_POSIXIPC:
    // Shared memory hack sync with spin-lock
    printf("non-blocking send not implemented for VM_COMM_TYPE_POSIXIPC.\n");
    break;
  case VM_COMM_TYPE_MPIUNI:
    // Shared memory hack for mpiuni
    // This shared memory implementation is naturally non-blocking.
    // Of course this allows only one message per sender - receiver channel.
    // TODO: To remove the single message per channel limitation there will need
    // TODO: to be one shared_mp element per request.
    (*ch)->type=-1; // indicate that this is a dummy commhandle
    shmp = sendChannel[dest].shmp;  // shared memory mp channel
    if (shmp->tcounter == 1){
      // recv() already set the ptr_dest -> send() came second -> copy data
      pdest = (char *)shmp->ptr_dest;
      memcpy(pdest, message, size);      
      // reset counter
      shmp->tcounter = 0;
    }else{
      // send() came first, set variables for recv() side
      shmp->ptr_src = message;        // set the source pointer
      shmp->tcounter++; // no need to sync for this in mpiuni
    }
    break;
  default:
    printf("unknown comm_type.\n");
    break;
  }
  return localrc;
}


int VMK::recv(void *message, int size, int source, int tag, status *status){
  // p2p recv
#if (VERBOSITY > 9)
  printf("receiving from: %d, %d\n", source, lpid[source]);
#endif
  int localrc=0;
  pipc_mp *pipcmp;
  shared_mp *shmp;
  int scpsize, rcpsize;
  char *pdest;
  char *psrc;
  int i;
  char *mess;
  int comm_type;
  if (source == VM_ANY_SRC){
    if (!mpionly) return VMK_ERROR; // bail out
    comm_type = VM_COMM_TYPE_MPI1;
  }else{
    // use the predefined comm_type between source and destination (mypet)
    comm_type = recvChannel[source].comm_type;
  }
  // set comm_type in status
  if (status)
    status->comm_type = comm_type;
  // switch into the appropriate implementation
  switch(comm_type){
  case VM_COMM_TYPE_MPI1:
    // MPI-1 implementation
    // use mutex to serialize mpi comm calls if mpi thread support requires it
    if (mpi_mutex_flag)
      pthread_mutex_lock(pth_mutex);
    if (tag == -1) tag = 1000*source+mypet; // default tag to simplify debugging
    else if (tag == VM_ANY_TAG) tag = MPI_ANY_TAG;
    int mpiSource;
    if (source == VM_ANY_SRC) mpiSource = MPI_ANY_SOURCE;
    else mpiSource = lpid[source];
    MPI_Status *mpi_s;
    if (status)
      mpi_s = &(status->mpi_s);
    else
      mpi_s = MPI_STATUS_IGNORE;
    localrc = MPI_Recv(message, size, MPI_BYTE, mpiSource, tag, mpi_c, mpi_s);
    if (mpi_mutex_flag)
      pthread_mutex_unlock(pth_mutex);
    if (status){
      if (lpid[mpi_s->MPI_SOURCE] == mpi_s->MPI_SOURCE)
        status->srcPet = mpi_s->MPI_SOURCE;
      else{
        for (int k=0; k<npets; k++)
          if (lpid[k] == mpi_s->MPI_SOURCE)
            status->srcPet = mpi_s->MPI_SOURCE;
      }
      status->tag     = mpi_s->MPI_TAG;
      status->error   = mpi_s->MPI_ERROR;
    }
    break;
  case VM_COMM_TYPE_PTHREAD:
    // Pthread implementation
    shmp = recvChannel[source].shmp;   // shared memory mp channel
    shmp->ptr_dest = message;               // set the destination pointer
    // synchronize with send()
    pthread_mutex_lock(&(shmp->mutex1));
    shmp->tcounter++;
    if (shmp->tcounter < 2){
      // wait for send()
      pthread_cond_wait(&(shmp->cond1), &(shmp->mutex1));
    }else{
      // reset counter and wake up send()
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
    // synchronize with send()
    pthread_mutex_lock(&(shmp->mutex2));
    shmp->tcounter++;
    if (shmp->tcounter < 2){
      // wait for send()
      pthread_cond_wait(&(shmp->cond2), &(shmp->mutex2));
    }else{
      // reset counter and wake up send()
      shmp->tcounter = 0;
      pthread_cond_broadcast(&(shmp->cond2));
    }
    pthread_mutex_unlock(&(shmp->mutex2));
    break;
  case VM_COMM_TYPE_SHMHACK:
    // Shared memory hack sync with spin-lock
    shmp = recvChannel[source].shmp;   // shared memory mp channel
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
      // synchronize with send()
      sync_b_flip(&shmp->shms);
      // now ptr_src and ptr_dest are valid for this message
      scpsize = size/2;           // send takes the lower half
      rcpsize = size - scpsize;   // recv takes the upper half
      pdest = (char *)shmp->ptr_dest;
      psrc = (char *)shmp->ptr_src;
      // do actual memcpy
      memcpy(pdest + scpsize, psrc + scpsize, rcpsize);
      // synchronize with send()
      sync_b_flop(&shmp->shms);
    }
    break;
  case VM_COMM_TYPE_POSIXIPC:
    // Shared memory hack sync with spin-lock
    pipcmp = recvChannel[source].pipcmp;   // shared memory mp channel
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
  case VM_COMM_TYPE_MPIUNI:
    // Shared memory hack for mpiuni
    // TODO: this assumes that send will arrive first, otherwise this will hang
    shmp = recvChannel[source].shmp;   // shared memory mp channel
    if (size<=SHARED_BUFFER){
      // buffer is sufficient
      psrc = shmp->buffer;
      // wait until buffer is ready to be used
      sync_buffer_wait_fill(&shmp->shms, 0);
      // do actual memcpy
      memcpy(message, psrc, size);
      // set flag indicating that recv's memcpy() is done and buffer is empty
      sync_buffer_flag_empty(&shmp->shms, 0);
    }else{
      // buffer is insufficient
      // todo: need to throw error
    }
    break;
  default:
    printf("unknown comm_type.\n");
    break;
  }
  return localrc;
}


int VMK::recv(void *message, int size, int source, commhandle **ch, int tag){
  // p2p recv
//fprintf(stderr, "VMK::recv: ch=%p\n", *ch);
#if (VERBOSITY > 9)
  printf("receiving from: %d, %d\n", source, lpid[source]);
#endif
  int localrc=0;
  pipc_mp *pipcmp;
  shared_mp *shmp;
  int scpsize, rcpsize;
  char *pdest;
  char *psrc;
  int i;
  char *mess;
  // check if this needs a new entry in the request queue
  if (*ch==NULL){
    *ch = new commhandle;
    commqueueitem_link(*ch);
  }
  int comm_type;
  if (source == VM_ANY_SRC){
    if (!mpionly) return VMK_ERROR; // bail out
    comm_type = VM_COMM_TYPE_MPI1;
  }else{
    // use the predefined comm_type between source and destination (mypet)
    comm_type = recvChannel[source].comm_type;
  }
  // switch into the appropriate implementation
  switch(comm_type){
  case VM_COMM_TYPE_MPI1:
    (*ch)->nelements=1;
    (*ch)->type=1;
    (*ch)->mpireq = new MPI_Request[1];
    // MPI-1 implementation
    // use mutex to serialize mpi comm calls if mpi thread support requires it
    if (mpi_mutex_flag)
      pthread_mutex_lock(pth_mutex);
//fprintf(stderr, "MPI_Irecv: ch=%p\n", (*ch)->mpireq);
    if (tag == -1) tag = 1000*source+mypet; // default tag to simplify debugging
    else if (tag == VM_ANY_TAG) tag = MPI_ANY_TAG;
    int mpiSource;
    if (source == VM_ANY_SRC) mpiSource = MPI_ANY_SOURCE;
    else mpiSource = lpid[source];
    localrc = MPI_Irecv(message, size, MPI_BYTE, mpiSource, tag, mpi_c,
      (*ch)->mpireq);
    if (mpi_mutex_flag)
      pthread_mutex_unlock(pth_mutex);
    break;
  case VM_COMM_TYPE_PTHREAD:
    // Pthread implementation
    printf("non-blocking recv not implemented for VM_COMM_TYPE_PTHREAD.\n");
    break;
  case VM_COMM_TYPE_SHMHACK:
    // Shared memory hack sync with spin-lock
    printf("non-blocking recv not implemented for VM_COMM_TYPE_SHMHACK.\n");
    break;
  case VM_COMM_TYPE_POSIXIPC:
    // Shared memory hack sync with spin-lock
    printf("non-blocking recv not implemented for VM_COMM_TYPE_POSIXIPC.\n");
    break;
  case VM_COMM_TYPE_MPIUNI:
    // Shared memory hack for mpiuni
    // This shared memory implementation is naturally non-blocking.
    // Of course this allows only one message per sender - receiver channel.
    // TODO: To remove the single message per channel limitation there will need
    // TODO: to be one shared_mp element per request.
    (*ch)->type=-1; // indicate that this is a dummy commhandle
    shmp = recvChannel[source].shmp;  // shared memory mp channel
    if (shmp->tcounter == 1){
      // send() already set the ptr_src -> recv() came second -> copy data
      psrc = (char *)shmp->ptr_src;
      memcpy(message, psrc, size);
      // reset counter
      shmp->tcounter = 0;
    }else{
      // recv() came first, set variables for send() side
      shmp->ptr_dest = message;        // set the destination pointer
      shmp->tcounter++; // no need to sync for this in mpiuni
    }
    break;
  default:
    printf("unknown comm_type.\n");
    break;
  }
  return localrc;
}


int VMK::vassend(void *message, int size, int destVAS, commhandle **ch,
  int tag){
  // non-blocking send where the destination is a VAS, _not_ a PET
  // todo: currently this is just a stub that uses the PET-based 
  //       non-blocking send. Hence this will not work for the ESMF-threading
  //       case for which it is actually thought for!
  // for this stub implementation figure out first PET to run in the destVAS
  int localrc=0;
  int dest;
  for (dest=0; dest<npets; dest++)
    if (pid[dest] == destVAS) break;
  localrc = send(message, size, dest, ch, tag);
  return localrc;
}


int VMK::vasrecv(void *message, int size, int srcVAS, commhandle **ch,
  int tag){
  // non-blocking recv where the source is a VAS, _not_ a PET
  // todo: currently this is just a stub that uses the PET-based 
  //       non-blocking recv. Hence this will not work for the ESMF-threading
  //       case for which it is actually thought for!
  // for this stub implementation figure out first PET to run in the sourceVAS
  int localrc=0;
  int src;
  for (src=0; src<npets; src++)
    if (pid[src] == srcVAS) break;
  localrc = recv(message, size, src, ch, tag);
  return localrc;
}

  
int VMK::barrier(){
  // collective barrier over all PETs
  int localrc=0;
  if (mpionly){
    localrc = MPI_Barrier(mpi_c);
    return localrc;
  }
  int myp = pid[mypet];
  int myt = tid[mypet];
  if (myt==0){
    // mypet is the zero thread for this PID
    for (int i=0; i<npets; i++)
      if (i!=mypet && pid[i]==myp){
        // pet "i" is another thread under same PID
        shared_mp *shmp = sendChannel[i].shmp;
        sync_a_flip(&shmp->shms);
      }
    // now all threads are "flip"-synced under their master thread
    // master thread will use MPI-1 to sync with all the other masters
    MPI_Barrier(mpi_c);
    // now master thread is synced against all other masters
    for (int i=0; i<npets; i++)
      if (i!=mypet && pid[i]==myp){
        // pet "i" is another thread under same PID
        shared_mp *shmp = sendChannel[i].shmp;
        sync_a_flop(&shmp->shms);
      }
  }else{
    // mypet is not the master thread for this PID -> find master
    int i;
    for (i=0; i<npets; i++)
      if (pid[i]==myp && tid[i]==0) break;
    // now PET "i" is the master thread for this PID
    shared_mp *shmp = recvChannel[i].shmp;
    sync_b_flip(&shmp->shms);
    // now all threads are "flip"-synced under their master thread
    // master will sync against all other masters using MPI-1 and then do flop
    sync_b_flop(&shmp->shms);
  }
  return localrc;
}


int VMK::sendrecv(void *sendData, int sendSize, int dst, void *recvData,
  int recvSize, int src){
  // p2p sendrecv
  int localrc=0;
  if (mpionly){
    MPI_Status mpi_s;
    localrc = MPI_Sendrecv(sendData, sendSize, MPI_BYTE, dst, 1000*mypet+dst, 
      recvData, recvSize, MPI_BYTE, src, 1000*src+mypet, mpi_c, &mpi_s);
  }else{
    // A unique order of the send and receive is given by the PET index.
    // This very simplistic implementation establishes a unique order by
    // first transferring data to the smallest receiver PET and then to the
    // other one. A sendrecv has two receiver PETs, one is the local PET and
    // the other is rcv.
    if (mypet<dst){
      // mypet is the first receiver
      localrc = recv(recvData, recvSize, src);
      if (localrc) return localrc;
      localrc = send(sendData, sendSize, dst);
      if (localrc) return localrc;
    }else{
      // dst is first receiver
      localrc = send(sendData, sendSize, dst);
      if (localrc) return localrc;
      localrc = recv(recvData, recvSize, src);
      if (localrc) return localrc;
    }
  }
  return localrc;
}
  
int VMK::sendrecv(void *sendData, int sendSize, int dst, void *recvData,
  int recvSize, int src, commhandle **ch){
  // check if this needs a new entry in the request queue
//fprintf(stderr, "VMK::sendrecv: ch=%p\n", *ch);
  int localrc=0;
  if (*ch==NULL){
    *ch = new commhandle;
    commqueueitem_link(*ch);
  }
  // p2p sendrecv non-blocking
  (*ch)->nelements = 2; // 2 requests for send/recv
  (*ch)->type=0; // subhandles
  (*ch)->handles = new commhandle*[(*ch)->nelements];
  for (int i=0; i<(*ch)->nelements; i++)
    (*ch)->handles[i] = new commhandle;
  // MPI does not offer a non-blocking sendrecv operation, hence there is no
  // point in checking if the mpionly flag is set in this VM, in either case
  // an explicit implementation based on send and recv must be used:
  // A unique order of the send and receive is given by the PET index.
  // This very simplistic implementation establishes a unique order by
  // first transferring data to the smallest receiver PET and then to the
  // other one. A sendrecv has two receiver PETs, one is the local PET and
  // the other is rcv.
  if (mypet<dst){
    // mypet is the first receiver
    localrc = recv(recvData, recvSize, src, &((*ch)->handles[0]));
    if (localrc) return localrc;
    localrc = send(sendData, sendSize, dst, &((*ch)->handles[1]));
    if (localrc) return localrc;
  }else{
    // dst is first receiver
    localrc = send(sendData, sendSize, dst, &((*ch)->handles[0]));
    if (localrc) return localrc;
    localrc = recv(recvData, recvSize, src, &((*ch)->handles[1]));
    if (localrc) return localrc;
  }
  return localrc;
}
  
int VMK::threadbarrier(){
  int localrc=0;
  if (!mpionly && !nothreadsflag){
    // collective barrier over all PETs in thread group with mypet
    int myp = pid[mypet];
    int myt = tid[mypet];
    if (myt==0){
      // mypet is the zero thread for this PID
      // todo: optimize by storing PETs of thread group within VM as list
      for (int i=0; i<npets; i++)
        if (i!=mypet && pid[i]==myp){
          // pet "i" is another thread under same PID
          shared_mp *shmp = sendChannel[i].shmp;
          sync_a_flip(&shmp->shms);
        }
      for (int i=0; i<npets; i++)
        if (i!=mypet && pid[i]==myp){
          // pet "i" is another thread under same PID
          shared_mp *shmp = sendChannel[i].shmp;
          sync_a_flop(&shmp->shms);
        }
    }else{
      // mypet is not the master thread for this PID -> find master
      int i;
      // todo: optimize by storing root PET of thread group within VM
      for (i=0; i<npets; i++)
        if (pid[i]==myp && tid[i]==0) break;
      // now PET "i" is the master thread for this PID
      shared_mp *shmp = recvChannel[i].shmp;
      sync_b_flip(&shmp->shms);
      // now all threads are "flip"-synced under their master thread
      sync_b_flop(&shmp->shms);
    }
  }
  return localrc;
}


int VMK::reduce(void *in, void *out, int len, vmType type, vmOp op, int root){
  int localrc=0;
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
    localrc = MPI_Reduce(in, out, len, mpitype, mpiop, root, mpi_c);
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
    char *temparray;
    if (mypet==root)
      temparray = new char[templen*npets]; // allocate temp data array
    // gather all data onto root PET
    localrc = gather(in, temparray, templen, root);
    // root does the entire reduction on its local temparray data
    if (mypet==root){
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
        switch (type){
        case vmI4:
          {
            int *tempdata = (int *)temparray;
            int *outdata = (int *)out;
            for (int i=0; i<len; i++){
              *outdata = tempdata[0];
              for (int j=1; j<npets; j++){
                if (tempdata[j*len] < *outdata)
                  *outdata = tempdata[j*len];
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
              *outdata = tempdata[0];
              for (int j=1; j<npets; j++){
                if (tempdata[j*len] < *outdata)
                  *outdata = tempdata[j*len];
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
              *outdata = tempdata[0];
              for (int j=1; j<npets; j++){
                if (tempdata[j*len] < *outdata)
                  *outdata = tempdata[j*len];
              }
              ++tempdata;
              ++outdata;
            }
          }
          break;
        }
        break;
      case vmMAX:
        switch (type){
        case vmI4:
          {
            int *tempdata = (int *)temparray;
            int *outdata = (int *)out;
            for (int i=0; i<len; i++){
              *outdata = tempdata[0];
              for (int j=1; j<npets; j++){
                if (tempdata[j*len] > *outdata)
                  *outdata = tempdata[j*len];
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
              *outdata = tempdata[0];
              for (int j=1; j<npets; j++){
                if (tempdata[j*len] > *outdata)
                  *outdata = tempdata[j*len];
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
              *outdata = tempdata[0];
              for (int j=1; j<npets; j++){
                if (tempdata[j*len] > *outdata)
                  *outdata = tempdata[j*len];
              }
              ++tempdata;
              ++outdata;
            }
          }
          break;
        }
        break;
      }
      delete [] temparray;
    }
  }
  return localrc;
}


int VMK::allreduce(void *in, void *out, int len, vmType type, vmOp op){
  int localrc=0;
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
    localrc = MPI_Allreduce(in, out, len, mpitype, mpiop, mpi_c);
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
    for (int i=0; i<npets; i++){
      localrc = gather(in, temparray, templen, i);
      if (localrc) return localrc;
    }
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
      switch (type){
      case vmI4:
        {
          int *tempdata = (int *)temparray;
          int *outdata = (int *)out;
          for (int i=0; i<len; i++){
            *outdata = tempdata[0];
            for (int j=1; j<npets; j++){
              if (tempdata[j*len] < *outdata)
                *outdata = tempdata[j*len];
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
            *outdata = tempdata[0];
            for (int j=1; j<npets; j++){
              if (tempdata[j*len] < *outdata)
                *outdata = tempdata[j*len];
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
            *outdata = tempdata[0];
            for (int j=1; j<npets; j++){
              if (tempdata[j*len] < *outdata)
                *outdata = tempdata[j*len];
            }
            ++tempdata;
            ++outdata;
          }
        }
        break;
      }
      break;
    case vmMAX:
      switch (type){
      case vmI4:
        {
          int *tempdata = (int *)temparray;
          int *outdata = (int *)out;
          for (int i=0; i<len; i++){
            *outdata = tempdata[0];
            for (int j=1; j<npets; j++){
              if (tempdata[j*len] > *outdata)
                *outdata = tempdata[j*len];
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
            *outdata = tempdata[0];
            for (int j=1; j<npets; j++){
              if (tempdata[j*len] > *outdata)
                *outdata = tempdata[j*len];
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
            *outdata = tempdata[0];
            for (int j=1; j<npets; j++){
              if (tempdata[j*len] > *outdata)
                *outdata = tempdata[j*len];
            }
            ++tempdata;
            ++outdata;
          }
        }
        break;
      }
      break;
    }
    delete [] temparray;
  }
  return localrc;
}


int VMK::allfullreduce(void *in, void *out, int len, vmType type, vmOp op){
  int localrc=0;
  void *localresult;
  int local_i4;
  float local_r4;
  double local_r8;
  // first reduce the vector on each PET
  switch (op){
  case vmSUM:
    switch (type){
    case vmI4:
      {
        localresult = (void *)&local_i4;
        local_i4 = 0;
        int *tempdata = (int *)in;        // type cast for pointer arithmetic
        for (int j=0; j<len; j++)
          local_i4 += tempdata[j];
      }
      break;
    case vmR4:
      {
        localresult = (void *)&local_r4;  // type cast for pointer arithmetic
        local_r4 = 0.;
        float *tempdata = (float *)in;
        for (int j=0; j<len; j++)
          local_r4 += tempdata[j];
      }
      break;
    case vmR8:
      {
        localresult = (void *)&local_r8;  // type cast for pointer arithmetic
        local_r8 = 0.;
        double *tempdata = (double *)in;
        for (int j=0; j<len; j++)
          local_r8 += tempdata[j];
      }
      break;
    }
    break;
  case vmMIN:
    switch (type){
    case vmI4:
      {
        localresult = (void *)&local_i4;
        int *tempdata = (int *)in;        // type cast for pointer arithmetic
        local_i4 = tempdata[0];
        for (int j=1; j<len; j++)
          if (tempdata[j] < local_i4) local_i4 = tempdata[j];
      }
      break;
    case vmR4:
      {
        localresult = (void *)&local_r4;  // type cast for pointer arithmetic
        float *tempdata = (float *)in;
        local_r4 = tempdata[0];
        for (int j=1; j<len; j++)
          if (tempdata[j] < local_r4) local_r4 = tempdata[j];
      }
      break;
    case vmR8:
      {
        localresult = (void *)&local_r8;  // type cast for pointer arithmetic
        double *tempdata = (double *)in;
        local_r8 = tempdata[0];
        for (int j=1; j<len; j++)
          if (tempdata[j] < local_r8) local_r8 = tempdata[j];
      }
      break;
    }
    break;
  case vmMAX:
    switch (type){
    case vmI4:
      {
        localresult = (void *)&local_i4;
        int *tempdata = (int *)in;        // type cast for pointer arithmetic
        local_i4 = tempdata[0];
        for (int j=1; j<len; j++)
          if (tempdata[j] > local_i4) local_i4 = tempdata[j];
      }
      break;
    case vmR4:
      {
        localresult = (void *)&local_r4;  // type cast for pointer arithmetic
        float *tempdata = (float *)in;
        local_r4 = tempdata[0];
        for (int j=1; j<len; j++)
          if (tempdata[j] > local_r4) local_r4 = tempdata[j];
      }
      break;
    case vmR8:
      {
        localresult = (void *)&local_r8;  // type cast for pointer arithmetic
        double *tempdata = (double *)in;
        local_r8 = tempdata[0];
        for (int j=1; j<len; j++)
          if (tempdata[j] > local_r8) local_r8 = tempdata[j];
      }
      break;
    }
    break;
  }
  localrc = allreduce(localresult, out, 1, type, op);
  return localrc;
}


int VMK::scatter(void *in, void *out, int len, int root){
  int localrc=0;
  if (mpionly){
    localrc = MPI_Scatter(in, len, MPI_BYTE, out, len, MPI_BYTE, root, mpi_c);
  }else{
    // This is a very simplistic, probably very bad peformance implementation.
    if (mypet==root){
      // I am root -> send chunks to all other PETs
      char *rootin = (char *)in;
      for (int i=0; i<root; i++){
        localrc = send(rootin, len, i);
        if (localrc) return localrc;
        rootin += len;
      }
      // memcpy root's chunk
      memcpy(out, rootin, len);
      rootin += len;
      // keep sending chunks
      for (int i=root+1; i<npets; i++){
        localrc = send(rootin, len, i);
        if (localrc) return localrc;
        rootin += len;
      }
    }else{
      // all other PETs receive their chunk
      localrc = recv(out, len, root);
    }
  }
  return localrc;
}


int VMK::reduce_scatter(void *in, void *out, int *outCounts,
  vmType type, vmOp op){
  int localrc=0;
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
    localrc = MPI_Reduce_scatter(in, out, outCounts, mpitype, mpiop, mpi_c);
  }else{
    // TODO: not yet implemented
    localrc = VMK_ERROR;
  }
  return localrc;
}

    
int VMK::scatter(void *in, void *out, int len, int root,
  commhandle **ch){
  int localrc=0;
  // check if this needs a new entry in the request queue
  if (*ch==NULL){
    *ch = new commhandle;
    commqueueitem_link(*ch);
  }
  // MPI does not offer a non-blocking scatter operation, hence there is no
  // point in checking if the mpionly flag is set in this VM, in either case
  // an explicit implementation based on send and recv must be used.
  // The number of elements in the commhandle depends on whether the localPET
  // is root or not.
  // This is a very simplistic, probably very bad peformance implementation.
  if (mypet==root){
    // I am root -> send chunks to all other PETs
    // rootPET will need to issue (npets-1) sends.
    (*ch)->nelements = npets-1;   // number of non-blocking sends
    (*ch)->type=0;                // these are subhandles
    (*ch)->handles = new commhandle*[(*ch)->nelements];
    for (int i=0; i<(*ch)->nelements; i++)
      (*ch)->handles[i] = new commhandle; // allocate handles
    // get ready to send chunks
    char *rootin = (char *)in;
    for (int i=0; i<root; i++){
      localrc = send(rootin, len, i, &((*ch)->handles[i]));
      if (localrc) return localrc;
      rootin += len;
    }
    // memcpy root's chunk
    memcpy(out, rootin, len);
    rootin += len;
    // keep sending chunks
    for (int i=root+1; i<npets; i++){
      localrc = send(rootin, len, i, &((*ch)->handles[i-1]));
      if (localrc) return localrc;
      rootin += len;
    }
  }else{
    // all other PETs receive their chunk
    // there will be a single receive that needs to be issued
    (*ch)->nelements = 1;
    (*ch)->type=0;                // these are subhandles
    (*ch)->handles = new commhandle*[1];
    (*ch)->handles[0] = new commhandle; // allocate handle
    localrc = recv(out, len, root, &((*ch)->handles[0]));
  }
  return localrc;
}


int VMK::scatterv(void *in, int *inCounts, int *inOffsets, void *out,
  int outCount, vmType type, int root){
  int localrc=0;
  if (mpionly){
    // Find corresponding MPI data type
    MPI_Datatype mpitype;
    switch (type){
    case vmBYTE:
      mpitype = MPI_BYTE;
      break;
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
    localrc = MPI_Scatterv(in, inCounts, inOffsets, mpitype, out, outCount,
      mpitype, root, mpi_c);
  }else{
    // This is a very simplistic, probably very bad peformance implementation.
    int size=0;
    switch (type){
    case vmI4:
      size=4;
      break;
    case vmR4:
      size=4;
      break;
    case vmR8:
      size=8;
      break;
    }
    int root = 0; // arbitrary root, 0 always exists!
    if (mypet==root){
      // I am root -> send chunks to all other PETs
      int len;
      char *rootin;
      for (int i=0; i<root; i++){
        len = inCounts[i] * size;
        rootin = (char *)in + inOffsets[i] * size;
        localrc = send(rootin, len, i);
        if (localrc) return localrc;
      }
      // memcpy root's chunk
      len = inCounts[root] * size;
      rootin = (char *)in + inOffsets[root] * size;
      memcpy(out, rootin, len);
      // keep sending chunks
      for (int i=root+1; i<npets; i++){
        len = inCounts[i] * size;
        rootin = (char *)in + inOffsets[i] * size;
        localrc = send(rootin, len, i);
        if (localrc) return localrc;
      }
    }else{
      // all other PETs receive their chunk
      int len = outCount * size;
      localrc = recv(out, len, root);
    }
  }
  return localrc;
}


int VMK::gather(void *in, void *out, int len, int root){
  int localrc=0;
  if (mpionly){
    localrc = MPI_Gather(in, len, MPI_BYTE, out, len, MPI_BYTE, root, mpi_c);
  }else{
    // This is a very simplistic, probably very bad peformance implementation.
    if (mypet==root){
      // I am root -> receive chunks from all other PETs
      char *rootout = (char *)out;
      for (int i=0; i<root; i++){
        localrc = recv(rootout, len, i);
        if (localrc) return localrc;
        rootout += len;
      }
      // memcpy root's chunk
      memcpy(rootout, in, len);
      rootout += len;
      // keep receiving chunks
      for (int i=root+1; i<npets; i++){
        localrc = recv(rootout, len, i);
        if (localrc) return localrc;
        rootout += len;
      }
    }else{
      // all other PETs send their chunk
      localrc = send(in, len, root);
    }
  }
  return localrc;
}


int VMK::gather(void *in, void *out, int len, int root, commhandle **ch){
  int localrc = 0;
  // check if this needs a new entry in the request queue
  if (*ch==NULL){
    *ch = new commhandle;
    commqueueitem_link(*ch);
  }
  // MPI does not offer a non-blocking gather operation, hence there is no
  // point in checking if the mpionly flag is set in this VM, in either case
  // an explicit implementation based on send and recv must be used.
  // The number of elements in the commhandle depends on whether the localPET
  // is root or not.
  // This is a very simplistic, probably very bad peformance implementation.
  if (mypet==root){
    // I am root -> receive chunks from all other PETs
    // rootPET will need to issue (npets-1) recvs.
    (*ch)->nelements = npets-1;   // number of non-blocking recvs
    (*ch)->type=0;                // these are subhandles
    (*ch)->handles = new commhandle*[(*ch)->nelements];
    for (int i=0; i<(*ch)->nelements; i++)
      (*ch)->handles[i] = new commhandle; // allocate handles
    // get ready to receive chunks
    char *rootout = (char *)out;
    for (int i=0; i<root; i++){
      localrc = recv(rootout, len, i, &((*ch)->handles[i]));
      if (localrc) return localrc;
      rootout += len;
    }
    // memcpy root's chunk
    memcpy(rootout, in, len);
    rootout += len;
    // keep receiving chunks
    for (int i=root+1; i<npets; i++){
      localrc = recv(rootout, len, i, &((*ch)->handles[i-1]));
      if (localrc) return localrc;
      rootout += len;
    }
  }else{
    // all other PETs send their chunk
    // there will be a single send that needs to be issued
    (*ch)->nelements = 1;
    (*ch)->type=0;                // these are subhandles
    (*ch)->handles = new commhandle*[1];
    (*ch)->handles[0] = new commhandle; // allocate handle
    localrc = send(in, len, root, &((*ch)->handles[0]));
  }
  return localrc;
}


int VMK::gatherv(void *in, int inCount, void *out, int *outCounts, 
  int *outOffsets, vmType type, int root){
  int localrc=0;
  if (mpionly){
    // Find corresponding MPI data type
    MPI_Datatype mpitype;
    switch (type){
    case vmBYTE:
      mpitype = MPI_BYTE;
      break;
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
    localrc = MPI_Gatherv(in, inCount, mpitype, out, outCounts, outOffsets,
      mpitype, root, mpi_c);
  }else{
    // This is a very simplistic, probably very bad peformance implementation.
    int size=0;
    switch (type){
    case vmI4:
      size=4;
      break;
    case vmR4:
      size=4;
      break;
    case vmR8:
      size=8;
      break;
    }
    int root = 0; // arbitrary root, 0 always exists!
    if (mypet==root){
      // I am root -> receive chunks from all other PETs
      int len;
      char *rootout;
      for (int i=0; i<root; i++){
        len = outCounts[i] * size;
        rootout = (char *)out + outOffsets[i] * size;
        localrc = recv(rootout, len, i);
        if (localrc) return localrc;
      }
      // memcpy root's chunk
      len = outCounts[root] * size;
      rootout = (char *)out + outOffsets[root] * size;
      memcpy(rootout, in, len);
      // keep receiving chunks
      for (int i=root+1; i<npets; i++){
        len = outCounts[i] * size;
        rootout = (char *)out + outOffsets[i] * size;
        localrc = recv(rootout, len, i);
        if (localrc) return localrc;
      }
    }else{
      // all other PETs send their chunk
      int len = inCount * size;
      localrc = send(in, len, root);
    }
  }
  return localrc;
}


int VMK::allgather(void *in, void *out, int len){
  int localrc=0;
  if (mpionly){
    localrc = MPI_Allgather(in, len, MPI_BYTE, out, len, MPI_BYTE, mpi_c);
  }else{
    // This is a very simplistic, probably very bad peformance implementation.
    int root = 0; // arbitrary root, 0 always exists!
    if (mypet==root){
      // I am root -> receive chunks from all other PETs
      char *rootout = (char *)out;
      for (int i=0; i<root; i++){
        localrc = recv(rootout, len, i);
        if (localrc) return localrc;
        rootout += len;
      }
      // memcpy root's chunk
      memcpy(rootout, in, len);
      rootout += len;
      // keep receiving chunks
      for (int i=root+1; i<npets; i++){
        localrc = recv(rootout, len, i);
        if (localrc) return localrc;
        rootout += len;
      }
    }else{
      // all other PETs send their chunk
      localrc = send(in, len, root);
      if (localrc) return localrc;
    }
    // now broadcast root's out to all other PETs
    localrc = broadcast(out, len, root);
  }
  return localrc;
}


int VMK::allgather(void *in, void *out, int len, commhandle **ch){
  int localrc=0;
  // check if this needs a new entry in the request queue
  if (*ch==NULL){
    *ch = new commhandle;
    commqueueitem_link(*ch);
  }
  // MPI does not offer a non-blocking allgather operation, hence there is no
  // point in checking if the mpionly flag is set in this VM, in either case
  // an explicit implementation must be used.
  // There will be as many commhandles as there are PETs
  (*ch)->nelements = npets;     // number of non-blocking gathers
  (*ch)->type=0;                // these are subhandles
  (*ch)->handles = new commhandle*[(*ch)->nelements];
  for (int i=0; i<(*ch)->nelements; i++)
    (*ch)->handles[i] = new commhandle; // allocate handles
  // This is a very simplistic, probably very bad peformance implementation.
  for (int root=0; root<npets; root++){
    // Each PET is considered the root PET once for a non-blocking gather
    localrc = gather(in, out, len, root, &((*ch)->handles[root]));
    if (localrc) return localrc;
  }
  return localrc;
}


int VMK::allgatherv(void *in, int inCount, void *out, int *outCounts,
  int *outOffsets, vmType type){
  int localrc=0;
  if (mpionly){
    // Find corresponding MPI data type
    MPI_Datatype mpitype;
    switch (type){
    case vmBYTE:
      mpitype = MPI_BYTE;
      break;
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
    localrc = MPI_Allgatherv(in, inCount, mpitype, out, outCounts, outOffsets,
      mpitype, mpi_c);
  }else{
    // This is a very simplistic, probably very bad peformance implementation.
    int size=0;
    switch (type){
    case vmI4:
      size=4;
      break;
    case vmR4:
      size=4;
      break;
    case vmR8:
      size=8;
      break;
    }
    int root = 0; // arbitrary root, 0 always exists!
    if (mypet==root){
      // I am root -> receive chunks from all other PETs
      int len;
      char *rootout;
      for (int i=0; i<root; i++){
        len = outCounts[i] * size;
        rootout = (char *)out + outOffsets[i] * size;
        localrc = recv(rootout, len, i);
        if (localrc) return localrc;
      }
      // memcpy root's chunk
      len = outCounts[root] * size;
      rootout = (char *)out + outOffsets[root] * size;
      memcpy(rootout, in, len);
      // keep receiving chunks
      for (int i=root+1; i<npets; i++){
        len = outCounts[i] * size;
        rootout = (char *)out + outOffsets[i] * size;
        localrc = recv(rootout, len, i);
        if (localrc) return localrc;
      }
    }else{
      // all other PETs send their chunk
      int len = inCount * size;
      localrc = send(in, len, root);
      if (localrc) return localrc;
    }
    // now broadcast root's out to all other PETs
    int len=0;
    for (int i=0; i<npets; i++)
      len += outCounts[i] * size;
    localrc = broadcast(out, len, root);
  }
  return localrc;
}


int VMK::alltoall(void *in, int inCount, void *out, int outCount,
  vmType type){
  int localrc=0;
  if (mpionly){
    // Find corresponding MPI data type
    MPI_Datatype mpitype;
    switch (type){
    case vmBYTE:
      mpitype = MPI_BYTE;
      break;
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
    localrc = MPI_Alltoall(in, inCount, mpitype, out, outCount, mpitype, mpi_c);
  }else{
    // This is a very simplistic, probably very bad peformance implementation.
    int size=0;
    switch (type){
    case vmI4:
      size=4;
      break;
    case vmR4:
      size=4;
      break;
    case vmR8:
      size=8;
      break;
    }
    char *inC = (char *)in;
    char *outC = (char *)out;
    // send to all PETs with id smaller than mypet
    for (int i=0; i<mypet; i++){
      localrc = send(inC+inCount*i*size, inCount*size, i);
      if (localrc) return localrc;
    }
    // memcpy the local chunk
    memcpy(outC+outCount*mypet*size, inC+inCount*mypet*size, inCount*size);
    // receive the data from all Pets with id larger than mypet
    for (int i=mypet+1; i<npets; i++){
      localrc = recv(outC+outCount*i*size, outCount*size, i);
      if (localrc) return localrc;
    }
    // send to all PETs with larger than mypet
    for (int i=mypet+1; i<npets; i++){
      localrc = send(inC+inCount*i*size, inCount*size, i);
      if (localrc) return localrc;
    }
    // receive the data from all Pets with id smaller than mypet
    for (int i=0; i<mypet; i++){
      localrc = recv(outC+outCount*i*size, outCount*size, i);
      if (localrc) return localrc;
    }
  }
  return localrc;
}


int VMK::alltoallv(void *in, int *inCounts, int *inOffsets, void *out,
  int *outCounts, int *outOffsets, vmType type){
  int localrc=0;
  if (mpionly){
    // Find corresponding MPI data type
    MPI_Datatype mpitype;
    switch (type){
    case vmBYTE:
      mpitype = MPI_BYTE;
      break;
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
    localrc = MPI_Alltoallv(in, inCounts, inOffsets, mpitype, out, outCounts,
      outOffsets, mpitype, mpi_c);
  }else{
    // This is a very simplistic, probably very bad peformance implementation.
    int size=0;
    switch (type){
    case vmI4:
      size=4;
      break;
    case vmR4:
      size=4;
      break;
    case vmR8:
      size=8;
      break;
    }
    char *inC = (char *)in;
    char *outC = (char *)out;
    // send to all PETs with id smaller than mypet
    for (int i=0; i<mypet; i++){
      localrc = send(inC+inOffsets[i]*size, inCounts[i]*size, i);
      if (localrc) return localrc;
    }
    // memcpy the local chunk
    memcpy(outC+outOffsets[mypet]*size, inC+inOffsets[mypet]*size,
      inCounts[mypet]*size);
    // receive the data from all Pets with id larger than mypet
    for (int i=mypet+1; i<npets; i++){
      localrc = recv(outC+outOffsets[i]*size, outCounts[i]*size, i);
      if (localrc) return localrc;
    }
    // send to all PETs with larger than mypet
    for (int i=mypet+1; i<npets; i++){
      localrc = send(inC+inOffsets[i]*size, inCounts[i]*size, i);
      if (localrc) return localrc;
    }
    // receive the data from all Pets with id smaller than mypet
    for (int i=0; i<mypet; i++){
      localrc = recv(outC+outOffsets[i]*size, outCounts[i]*size, i);
      if (localrc) return localrc;
    }
  }
  return localrc;
}


int VMK::broadcast(void *data, int len, int root){
  int localrc=0;
  if (mpionly){
    localrc = MPI_Bcast(data, len, MPI_BYTE, root, mpi_c);
  }else{
    // This is a very simplistic, probably very bad peformance implementation.
    if (mypet==root){
      // I am root -> send my data to all other PETs
      for (int i=0; i<npets; i++) {
        if (i==mypet) continue; // skip root PET
        localrc = send(data, len, i);
        if (localrc) return localrc;
      }
    }else{
      // all other PETs receive the broadcasted data
      localrc = recv(data, len, root);
    }
  }
  return localrc;
}


int VMK::broadcast(void *data, int len, int root, commhandle **ch){
  int localrc=0;
  // check if this needs a new entry in the request queue
  if (*ch==NULL){
    *ch = new commhandle;
    commqueueitem_link(*ch);
  }
  // MPI does not offer a non-blocking broadcast operation, hence there is no
  // point in checking if the mpionly flag is set in this VM, in either case
  // an explicit implementation based on send and recv must be used.
  // The number of elements in the commhandle depends on whether the localPET
  // is root or not.
  // This is a very simplistic, probably very bad peformance implementation.
  if (mypet==root){
    // I am root -> send my data to all other PETs
    // rootPET will need to issue (npets-1) sends
    (*ch)->nelements = npets-1;   // number of non-blocking recvs
    (*ch)->type=0;                // these are subhandles
    (*ch)->handles = new commhandle*[(*ch)->nelements];
    for (int i=0; i<(*ch)->nelements; i++)
      (*ch)->handles[i] = new commhandle; // allocate handles
    // get ready to send chunks
    for (int i=0; i<root; i++){
      localrc = send(data, len, i, &((*ch)->handles[i]));
      if (localrc) return localrc;
    }
    // skip root
    for (int i=root+1; i<npets; i++){
      localrc = send(data, len, i, &((*ch)->handles[i-1]));
      if (localrc) return localrc;
    }
  }else{
    // all other PETs receive the broadcasted data
    // there will be a single receive that needs to be issued
    (*ch)->nelements = 1;
    (*ch)->type=0;                // these are subhandles
    (*ch)->handles = new commhandle*[1];
    (*ch)->handles[0] = new commhandle; // allocate handle
    localrc = recv(data, len, root, &((*ch)->handles[0]));
  }
  return localrc;
}


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~ Timing Calls
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


void VMK::wtime(double *time){
  *time = MPI_Wtime();
}


void VMK::wtimeprec(double *prec){
  double temp_prec = 0.;
  double t1, t2, dt;
  for(int i=0; i<10; i++){
    wtime(&t1);
    t2 = t1;
    while(fabs(t2-t1)<DBL_MIN)
      wtime(&t2);
    dt = t2 - t1;
    if (dt > temp_prec) temp_prec = dt;
  }  
  *prec = temp_prec;
}


void VMK::wtimedelay(double delay){
  double t1, t2;
  wtime(&t1);
  t2 = t1;
  while(t2-t1<delay)
    wtime(&t2);
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~ IntraProcessSharedMemoryAllocation List Methods
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


void *VMK::ipshmallocate(int bytes, int *firstFlag){
  if (firstFlag != NULL) *firstFlag = 0; // reset
  pthread_mutex_lock(ipshmMutex);
  if (ipshmLocalTop == *ipshmTop){
    // this is the first thread for this new request: add element + allocate
    *ipshmTop = new ipshmAlloc;  // new top element in shared allocation list
    (*ipshmTop)->allocation = (void *)malloc(bytes); // actual memory alloc
    (*ipshmTop)->auxCounter = getNthreads(getMypet()); // reset
    (*ipshmTop)->prev = NULL;          // indicate end of list
    (*ipshmTop)->next = ipshmLocalTop; // link to previous top element in list
    if (ipshmLocalTop != NULL)
      ipshmLocalTop->prev = *ipshmTop;  // back link
    ipshmLocalTop = *ipshmTop;          // update local top pointer
    if (firstFlag != NULL) *firstFlag = 1; // set caller provided flag
  }else{
    // this is a secondary thread for this request: find allocation element
    if (ipshmLocalTop != NULL)
      ipshmLocalTop = ipshmLocalTop->prev;
    else{
      // need to search for the previous element
      ipshmAlloc *ipshmTemp = *ipshmTop; // start at the top
      while (ipshmTemp != NULL){
        if (ipshmTemp->next == ipshmLocalTop) break;
        ipshmTemp = ipshmTemp->next;
      }
      if (ipshmTemp != NULL)
        ipshmLocalTop = ipshmTemp;  // new local top element
    }
  }
  pthread_mutex_unlock(ipshmMutex);
  return ipshmLocalTop->allocation;  // return allocation of local top
}


void VMK::ipshmdeallocate(void *pointer){
  // this call has undefined behavior if called multiple times from the same
  // thread with identical pointer argument
  pthread_mutex_lock(ipshmMutex);
  ipshmAlloc *ipshmTemp = *ipshmTop; // start at the current top of the list
  while (ipshmTemp != NULL){
    if (ipshmTemp->allocation == pointer) break;
    ipshmTemp = ipshmTemp->next;
  }
  if (ipshmTemp!=NULL){
    // found the allocation
    --(ipshmTemp->auxCounter); // count this thread's deallocate call
    if (ipshmTemp->auxCounter == 0){
      // this was the last thread to call deallocate for this allocation
      //printf("freeing %p\n", pointer);
      free(pointer);
      // Cannot deallocate the allocation element in list here without
      // disturbing list structure which is shared between threads.
      // It is anyway safer to do a centralized deallocation of this structure
      // during VM shutdown as it gives a chance to free any remaining
      // pointers that are still allocated in order to prevent memory leaks
      // because of improper user code.
    }
  }
  pthread_mutex_unlock(ipshmMutex);
}


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~ IntraProcessMutex Methods
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

VMK::ipmutex *VMK::ipmutexallocate(){
  int firstFlag;
  pthread_mutex_lock(ipSetupMutex);
  ipmutex *ipm = (ipmutex *)
    ipshmallocate(sizeof(ipmutex), &firstFlag);
  if (firstFlag) pthread_mutex_init(&(ipm->pth_mutex), NULL);
  ipm->lastFlag = getNthreads(getMypet()); //reset
  pthread_mutex_unlock(ipSetupMutex);
  return ipm;
}

void VMK::ipmutexdeallocate(ipmutex *ipm){
  pthread_mutex_lock(ipSetupMutex);
  --(ipm->lastFlag);  // register this thread
  if (ipm->lastFlag == 0) pthread_mutex_destroy(&(ipm->pth_mutex));
  ipshmdeallocate(ipm);
  pthread_mutex_unlock(ipSetupMutex);
}

int VMK::ipmutexlock(ipmutex *ipm){
  return pthread_mutex_lock(&(ipm->pth_mutex));
}

int VMK::ipmutexunlock(ipmutex *ipm){
  return pthread_mutex_unlock(&(ipm->pth_mutex));
}


} // namespace ESMCI


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
