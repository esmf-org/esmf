// $Id: ESMC_Comm.C,v 1.3 2002/12/10 03:48:51 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC Comm method code (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ Comm methods declared
// in the companion file ESMC_Comm.h
//
// < insert a paragraph or two explaining what you'll find in this file >
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
#include <iostream>
#include <ESMC.h>
#include <string.h>  // memset TODO:  ?? remove -test only
#include <mpi.h>

 // associated class definition file
 #include <ESMC_Comm.h>

// shared memory buffers TODO:  bring into class as pointers ??
extern int *lbuf;
extern int *gbuf;
extern int nodeRank;
extern pthread_t ESMC_Comm_tid[];

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Comm.C,v 1.3 2002/12/10 03:48:51 eschwab Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//

// Initialize class statics

 pthread_mutex_t ESMC_Comm::initMutex = PTHREAD_MUTEX_INITIALIZER;
 pthread_cond_t ESMC_Comm::initCV = PTHREAD_COND_INITIALIZER;
 pthread_mutex_t ESMC_Comm::barrierMutex = PTHREAD_MUTEX_INITIALIZER;
 pthread_cond_t ESMC_Comm::barrierCV = PTHREAD_COND_INITIALIZER;
 pthread_cond_t ESMC_Comm::mainProcBarrierCV = PTHREAD_COND_INITIALIZER;
 int ESMC_Comm::threadCountA = 0;
 int ESMC_Comm::threadCountB = 0;

// This section includes all the Comm routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommInit - initializes a Comm object
//
// !INTERFACE:
      int ESMC_Comm::ESMC_CommInit(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *argc,            // in - from main invocation
      char **argv[],        // in - from main invocation
      ESMC_DE *de) {        // in - DE we're communicating on behalf of
//
// !DESCRIPTION:
//      ESMF routine which only initializes Comm values; it does not
//      allocate any resources.  Define for shallow classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

  // finalization flag
  commFinal = false;

  // save DE pointer
  DE = de;

  numDEs = ESMC_COMM_NTHREADS * ESMC_COMM_NNODES;

  if (DE->deType == ESMC_PROCESS) {
    int initialized;

    MPI_Initialized(&initialized);
    if (!initialized) {
      MPI_Init(argc, argv);
    } else {
      // log error
    }

    // get size of DE process group
    // MPI_Comm_size(MPI_COMM_WORLD, &numDEs);

    // get my unique DE process group ID
    MPI_Comm_rank(MPI_COMM_WORLD, &(DE->pID));
    //std::cout << "pID = " << DE->pID << "\n";

    // share it with all sub-threads to calculate unique DE ids
    pthread_mutex_lock(&initMutex);
      nodeRank = DE->pID;
      pthread_cond_broadcast(&initCV);
    pthread_mutex_unlock(&initMutex);

    // initialize ESMC data type map to MPI types
    ESMC_TypeToMPI[ESMC_INT] = MPI_INT;
    ESMC_TypeToMPI[ESMC_LONG] = MPI_LONG;
    ESMC_TypeToMPI[ESMC_FLOAT] = MPI_FLOAT;
    ESMC_TypeToMPI[ESMC_DOUBLE] = MPI_DOUBLE;

    // initialize ESMC operation type map to MPI types
    ESMC_OpToMPI[ESMC_SUM] = MPI_SUM;
    ESMC_OpToMPI[ESMC_MIN] = MPI_MIN;
    ESMC_OpToMPI[ESMC_MAX] = MPI_MAX;
  }

  // determine thread index
  pthread_t mytid = pthread_self();
  for(int i=0; i<ESMC_COMM_NTHREADS; i++) {
    if (mytid == ESMC_Comm_tid[i]) {
      DE->tID = i;
//std::cout << "tid, i = " << ESMC_Comm_tid[i] << ", " << i << std::endl;
      break;
    }
  }

  // calculate unique DE id across all nodes and threads/processes
  pthread_mutex_lock(&initMutex);
    while(nodeRank < 0) {
        pthread_cond_wait(&initCV, &initMutex);
    }
    DE->esmfID = nodeRank * ESMC_COMM_NTHREADS + DE->tID;
  pthread_mutex_unlock(&initMutex);

  return(ESMF_SUCCESS);

 } // end ESMC_CommInit

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommFinal - finalizes a Comm object
//
// !INTERFACE:
      int ESMC_Comm::ESMC_CommFinal() {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    int error return code
//
// !DESCRIPTION:
//      ESMF routine which finalizes a Comm object; performs any 
//      necessary clean-up.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

  if (!commFinal) {
    if (DE->deType == ESMC_PROCESS) MPI_Finalize();
    commFinal = true;
  }

  return(ESMF_SUCCESS);

 } // end ESMC_CommFinal

#if 0
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommGetConfig - get configuration info from a Comm
//
// !INTERFACE:
      int ESMC_Comm::ESMC_CommGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_CommConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the Comm object was configured with.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_CommGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommSetConfig - set configuration info for a Comm
//
// !INTERFACE:
      int ESMC_Comm::ESMC_CommSetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_CommConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the Comm object with set of resources given.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_CommSetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommGet<Value> - get <Value> for a Comm
//
// !INTERFACE:
      int ESMC_Comm::ESMC_CommGet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of Comm member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_CommGet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommSet<Value> - set <Value> for a Comm
//
// !INTERFACE:
      int ESMC_Comm::ESMC_CommSet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> value) {     // in - value
//
// !DESCRIPTION:
//     Sets the Comm member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_CommSet<Value>
#endif

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommGetNumDEs - get number of DEs in a Comm
//
// !INTERFACE:
      int ESMC_Comm::ESMC_CommGetNumDEs(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *ndes) const {     // out - number of DEs
//
// !DESCRIPTION:
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

  *ndes = numDEs;

  return(ESMF_SUCCESS);

 } // end ESMC_CommGetNumProcesses

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommValidate - internal consistency check for a Comm
//
// !INTERFACE:
      int ESMC_Comm::ESMC_CommValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a Comm is internally consistent.
//      Returns error code if problems are found.  ESMC\_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

  return(ESMF_SUCCESS);

 } // end ESMC_CommValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommPrint - print contents of a Comm
//
// !INTERFACE:
      int ESMC_Comm::ESMC_CommPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a Comm.  The options control the
//      type of information and level of detail.  ESMC\_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  //std::cout << "ESMC_Comm numDEs = " << numDEs << std::endl;

  return(ESMF_SUCCESS);

 } // end ESMC_CommPrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Comm - native C++ constructor
//
// !INTERFACE:
      ESMC_Comm::ESMC_Comm(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Calls standard ESMF deep or shallow methods for initialization
//      with default or passed-in values
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  threadCount = &threadCountA;

 } // end ESMC_Comm

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Comm - native C++ constructor
//
// !INTERFACE:
      ESMC_Comm::ESMC_Comm(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      int *argc,            // in
      char **argv[],        // in
      ESMC_DE *de) {        // in
//
// !DESCRIPTION:
//      Calls standard ESMF deep or shallow methods for initialization
//      with default or passed-in values
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  ESMC_Comm();
  ESMC_CommInit(argc, argv,de);

 } // end ESMC_Comm

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_Comm - native C++ destructor
//
// !INTERFACE:
      ESMC_Comm::~ESMC_Comm(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Calls standard ESMF deep or shallow methods for destruction
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  if (!commFinal) ESMC_CommFinal();

 } // end ~ESMC_Comm

//
// Point-to-Point methods
//

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommIsend - Non-blocking send from one DE to another DE
//
// !INTERFACE:
      int ESMC_Comm::ESMC_CommIsend(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void *buf,
      int num,
      ESMC_Type_e type,
      ESMC_DE *dest,
      int tag,
      int *request) {
//
// !DESCRIPTION:
//      
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  int destpID;

  dest->ESMC_DEGetpID(&destpID);

#ifdef MPI
  MPI_Request req;
  MPI_Isend(buf, num, ESMC_TypeToMPI[type], destpID, tag, MPI_COMM_WORLD, &req);
  // add req to linked list TODO:
#endif

  return(ESMF_SUCCESS);

 } // end ESMC_CommBcast

//
// Collective methods
//

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommBarrier - Synchronize a set of DEs
//
// !INTERFACE:
      int ESMC_Comm::ESMC_CommBarrier(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

#ifdef MPI_ONLY
  MPI_Barrier(MPI_COMM_WORLD);
#endif

  //  TODO:  ??  need to reset thread_count to zero after or before

static int count = 0;

  pthread_mutex_lock(&barrierMutex);

count++;
//std::cout << "count = " << count << std::endl;

    // if this counter still in use by previous barrier, switch to partner
    if (*threadCount >= ESMC_COMM_NTHREADS) {
       threadCount = (threadCount == &threadCountA) ?
                      &threadCountB : &threadCountA;
    }
    // count how many threads (DEs) have entered the barrier
    (*threadCount)++;

    // inform main process/thread that all sub-threads are done
    if (*threadCount == ESMC_COMM_NTHREADS-1) {
      pthread_cond_broadcast(&mainProcBarrierCV);
    }

    // when last thread (DE) has entered, reset for next barrier, and inform all
    if (*threadCount == ESMC_COMM_NTHREADS) {
#if 0
if (count == 4 || count == 8) {
for(int i=0; i<12; i++) std::cout << rbuf[i] << " ";
  memset(rbuf, 0, 12*sizeof(int));
for(int i=0; i<12; i++) std::cout << rbuf[i] << " ";
}
//for(int i=0; i<12; i++) rbuf[i] = 0;
#endif

      // can clear previous barrier counter since we know all threads
      //   are in this barrier now
      (threadCount == &threadCountA) ? threadCountB = 0 : threadCountA = 0;

      // now tell the others we can exit the barrier
      pthread_cond_broadcast(&barrierCV);

    } else {

      // wait until all DEs have entered the barrier

      // use while loop to guard against spurious/erroneous wake-ups
      while (*threadCount < ESMC_COMM_NTHREADS) {
        pthread_cond_wait(&barrierCV, &barrierMutex);
//std::cout << "threadCount = " << *threadCount << std::endl;
      }
    }

  pthread_mutex_unlock(&barrierMutex);

  return(ESMF_SUCCESS);

 } // end ESMC_CommBarrier

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommBcast - Broadcast from a DE to all DEs
//
// !INTERFACE:
      int ESMC_Comm::ESMC_CommBcast(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void *buf,
      int num,
      ESMC_Type_e type,
      ESMC_DE *root) {
//
// !DESCRIPTION:
//      
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  int rootpID;

  root->ESMC_DEGetpID(&rootpID);

#ifdef MPI
  MPI_Bcast(buf, num, ESMC_TypeToMPI[type], rootpID, MPI_COMM_WORLD);
#endif

  return(ESMF_SUCCESS);

 } // end ESMC_CommBcast

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommScatter - Scatter from a DE to all DEs
//
// !INTERFACE:
      int ESMC_Comm::ESMC_CommScatter(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void *sbuf,
      void *rbuf,
      int num,
      ESMC_Type_e type,
      ESMC_DE *root) {
//
// !DESCRIPTION:
//      
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  int rootpID;

  root->ESMC_DEGetpID(&rootpID);

#ifdef MPI
  MPI_Scatter(sbuf, num, ESMC_TypeToMPI[type],
              rbuf, num, ESMC_TypeToMPI[type],
              rootpID, MPI_COMM_WORLD);
#endif

  return(ESMF_SUCCESS);

 } // end ESMC_CommScatter

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommAllGather - All DEs to All DEs Gather
//
// !INTERFACE:
      int ESMC_Comm::ESMC_CommAllGather(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void *sbuf,
      void *rbuf,
      int num,
      ESMC_Type_e type) {
//
// !DESCRIPTION:
//      
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

std::cout << "entered ESMC_CommAllGather(), tidx = " << DE->tID << std::endl;

  // copy our data into common buffer
  switch (type)
  {
    case ESMC_INT:
      // copy sbuf to our rbuf slot
      //  TODO:  use memcpy for speed ??
      for (int i=0; i<num; i++) {
        ((int *)rbuf)[(DE->tID)*num + i] = ((int *)sbuf)[i];
      }
      break;

    default:
      break;
  }

  if (DE->deType == ESMC_PROCESS) {
    // gather local node's thread data first by simply waiting
    //   for them to finish copying their data to the rbuf

    pthread_mutex_lock(&barrierMutex);

std::cout << "entered process barrier, threadCount = " << *threadCount << "\n";

      // if this counter still in use by previous barrier, switch to partner
      if (*threadCount >= ESMC_COMM_NTHREADS) {
         threadCount = (threadCount == &threadCountA) ?
                        &threadCountB : &threadCountA;
      }

      // use while loop to guard against spurious/erroneous wake-ups
      while (*threadCount < ESMC_COMM_NTHREADS-1) {
std::cout << "main waiting for sub-threads with threadCount = " << *threadCount << std::endl;
        pthread_cond_wait(&mainProcBarrierCV, &barrierMutex);
std::cout << "main wokeup with threadCount = " << *threadCount << std::endl;
      }

    pthread_mutex_unlock(&barrierMutex);

    // then exchange data with other nodes
    MPI_Allgather(lbuf, num*ESMC_COMM_NTHREADS, ESMC_TypeToMPI[type],
                  gbuf, num*ESMC_COMM_NTHREADS, ESMC_TypeToMPI[type],
                  MPI_COMM_WORLD);
  }

std::cout << "ESMC_CommAllGather(), final barrier, tidx = " << DE->tID << std::endl;

  // wait for all threads to finish copying their data
  ESMC_CommBarrier();

std::cout << "leaving ESMC_CommAllGather(), tidx = " << DE->tID << std::endl;
  return(ESMF_SUCCESS);

 } // end ESMC_CommAllGather

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommAlltoAll - All DEs to All DEs Scatter/Gather
//
// !INTERFACE:
      int ESMC_Comm::ESMC_CommAlltoAll(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void *sbuf,
      void *rbuf,
      int num,
      ESMC_Type_e type) {
//
// !DESCRIPTION:
//      
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

#ifdef MPI
  MPI_Alltoall(sbuf, num, ESMC_TypeToMPI[type],
               rbuf, num, ESMC_TypeToMPI[type],
               MPI_COMM_WORLD);
#endif

  return(ESMF_SUCCESS);

 } // end ESMC_CommAlltoAll

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommReduce - Reduce All DEs to a single DE
//
// !INTERFACE:
      int ESMC_Comm::ESMC_CommReduce(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void *sbuf,
      void *rbuf,
      int num,
      ESMC_Type_e type,
      ESMC_Op_e op,
      ESMC_DE *root) {
//
// !DESCRIPTION:
//      
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  int rootpID;

  root->ESMC_DEGetpID(&rootpID);

#ifdef MPI
  MPI_Reduce(sbuf, rbuf, num, ESMC_TypeToMPI[type], ESMC_OpToMPI[op], rootpID, 
             MPI_COMM_WORLD);
#endif

  return(ESMF_SUCCESS);

 } // end ESMC_CommReduce
