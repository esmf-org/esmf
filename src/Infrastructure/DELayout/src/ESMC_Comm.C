// $Id: ESMC_Comm.C,v 1.1 2003/09/19 17:00:03 cdeluca Exp $
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
// 
//
//-----------------------------------------------------------------------------
//
// insert any higher level, 3rd party or system includes here
#include <iostream.h>
//#include <iostream>  // TODO: use when namespaces consistently implemented
//using std::cout;
//using std::endl;
#include <ESMC.h>
#include <string.h>  // memset TODO:  ?? remove -test only
#include <stdio.h>
#include <mpi.h>

 // associated class definition file
#include <ESMC_Comm.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Comm.C,v 1.1 2003/09/19 17:00:03 cdeluca Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
#define ESMF_MPI_TAG 1

pthread_t *ESMC_Comm_tid = 0; // array of tid's shared with 
                              //  application main(), which needs it before
                              //  a ESMC_Comm is instantiated
                             // TODO: defined here so F90 program main doesn't
                             //   need to (yet) (avoids F90 unresolved link err)
                             // main() allocates to nThreadsPerProc size,
                             //   which must be same as that passed into Init()
                             // TODO: validate size match ?
                             // TODO: make class static? but would then need to
                             // instantiate ESMC_Comm first thing in main()
                             // how to do on F90 side ?

// Initialize class statics

 int ESMC_Comm::numDEs = 0;

 // default minimal DE type configuration
 int ESMC_Comm::nThreadsPerProc = 1;
 int ESMC_Comm::nProcs = 2;

 // initialize local inter-thread buffer TODO: beginnings of memory mgmt ?
 void *ESMC_Comm::lbuf = 0;
 int  ESMC_Comm::lbufSize = 4096;              // TODO:  from config file ?
 ESMC_DataKind ESMC_Comm::lbufType = ESMF_I4;   // TODO: from config file ?

 // initialize inter-thread comm variables
 pthread_mutex_t ESMC_Comm::bufMutex = PTHREAD_MUTEX_INITIALIZER;
 pthread_mutex_t ESMC_Comm::finalMutex = PTHREAD_MUTEX_INITIALIZER;
 pthread_mutex_t ESMC_Comm::initMutex = PTHREAD_MUTEX_INITIALIZER;
 pthread_cond_t ESMC_Comm::initCV = PTHREAD_COND_INITIALIZER;
 pthread_mutex_t ESMC_Comm::barrierMutex = PTHREAD_MUTEX_INITIALIZER;
 pthread_cond_t ESMC_Comm::barrierCV = PTHREAD_COND_INITIALIZER;
 pthread_cond_t ESMC_Comm::mainProcBarrierCV = PTHREAD_COND_INITIALIZER;
 int *ESMC_Comm::threadCount = &threadCountA;
 int ESMC_Comm::threadCountA = 0;
 int ESMC_Comm::threadCountB = 0;
 bool ESMC_Comm::lbufCleared = false;

 // initialize node rank to unknown
 int ESMC_Comm::nodeRank = -1;

 // finalization flag
 bool ESMC_Comm::commFinal = false;

// type conversion maps from ESMF to MPI
 MPI_Datatype ESMC_Comm::ESMC_DataKindToMPI[] =
   // ESMF_I1 ESMF_I2 ESMF_I4 ESMF_I8 ESMF_R4
  {0, MPI_CHAR,    MPI_SHORT,   MPI_INT,     MPI_LONG,    MPI_FLOAT,

   // ESMF_R8 ESMF_C8 ESMF_C16
      MPI_DOUBLE,  MPI_DOUBLE, MPI_LONG_DOUBLE };
   // MPI_DOUBLE,  MPI_COMPLEX, MPI_DOUBLE_COMPLEX }; // for MPI implementations
                                                      // that support COMPLEX

 MPI_Op ESMC_Comm::ESMC_OpToMPI[] = {0, MPI_SUM, MPI_MIN, MPI_MAX };
                                   //   ESMC_SUM ESMC_MIN ESMC_MAX
//
// This section includes all the Comm routines
//

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommInit - initializes a Comm object with default values
//
// !INTERFACE:
      int ESMC_Comm::ESMC_CommInit(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *argc,              // in - from main invocation
      char **argv[],          // in - from main invocation
      ESMC_DE *de) {          // in - DE we're communicating on behalf of
//
// !DESCRIPTION:
//      ESMF routine which only initializes Comm values; it does not
//      allocate any resources.  Define for shallow classes only.
//
//EOP
// !REQUIREMENTS:  

  //  initialize with default configuration values TODO: ??
  ESMC_CommInit(argc, argv, de, nThreadsPerProc, nProcs, lbufSize, lbufType);

  return(ESMF_SUCCESS);

 } // end ESMC_CommInit

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
      int *argc,              // in - from main invocation
      char **argv[],          // in - from main invocation
      ESMC_DE *de,            // in - DE we're communicating on behalf of
      int nthreadsperproc,    // in - number of threads per process
      int nprocs,             // in - number of processes
      int lbufsize,           // in - number of local message buffer elements
      ESMC_DataKind lbuftype) { // in - type of local message buffer elements
//
// !DESCRIPTION:
//      ESMF routine which only initializes Comm values; it does not
//      allocate any resources.  Define for shallow classes only.
//
//EOP
// !REQUIREMENTS:  

  // save DE pointer
  DE = de;

  // initialize class statics TODO: use pthread_once() ??

  pthread_mutex_lock(&initMutex);

    nThreadsPerProc = nthreadsperproc;
    nProcs = nprocs;
    lbufSize = lbufsize;
    lbufType = lbuftype;

    // TODO:  coordinate with other DEs to determine group size ??
    // default to what's given, override later if needed

    // TODO: this should be querying the 1 Machine instance to find out how
    //  many procs there are.  threads are another matter since they are
    //  more dynamic.  but this is hardcoded so we can't give a reasonable
    //  error if the framework user calls a create asking for more DEs than
    //  there are PEs.  eventually we are intending to support virtual DEs
    //  but for the current code they are *not* supported, but we can't give
    //  an error to tell them that.   this needs fixing.
    // for now try this, and comment out old code which does threads * procs
    Machine.ESMC_MachineGetNumCPUs(&numDEs);
    //numDEs = nThreadsPerProc * nProcs;

    // allocate local message buffer
    if (lbuf == 0) {
      switch (lbufType)
      {
        case ESMF_I4:
          try {
            lbuf = new int[lbufSize];
          }
          //  catch (bad_alloc) {
          // TODO: use when IBM supports it (blackforest doesn't)
          catch (...) {
          // TODO:  call ESMF log/err handler
            cerr << "ESMC_Comm() lbuf int memory allocation failed\n";
            return(ESMF_FAILURE);
          }
          break;
        case ESMF_I8:
          try {
            lbuf = new long[lbufSize];
          }
          //  catch (bad_alloc) {
          // TODO: use when IBM supports it (blackforest doesn't)
          catch (...) {
          // TODO:  call ESMF log/err handler
            cerr << "ESMC_Comm() lbuf long memory allocation failed\n";
            return(ESMF_FAILURE);
          }
          break;
        case ESMF_R4:
          try {
            lbuf = new float[lbufSize];
          }
          //  catch (bad_alloc) {
          // TODO: use when IBM supports it (blackforest doesn't)
          catch (...) {
          // TODO:  call ESMF log/err handler
            cerr << "ESMC_Comm() lbuf float memory allocation failed\n";
            return(ESMF_FAILURE);
          }
          break;
        case ESMF_R8:
          try {
            lbuf = new double[lbufSize];
          }
          //  catch (bad_alloc) {
          // TODO: use when IBM supports it (blackforest doesn't)
          catch (...) {
          // TODO:  call ESMF log/err handler
            cerr << "ESMC_Comm() lbuf double memory allocation failed\n";
            return(ESMF_FAILURE);
          }
          break;
        default:
          break;
      }
    }

  pthread_mutex_unlock(&initMutex);

  if (DE->deType == ESMC_PROCESS) {
    int initialized;

    //   TODO: MPI_Init needs to be called at Component Create level without
    //   exposing Comm ?
    MPI_Initialized(&initialized);
    if (!initialized) {
      MPI_Init(argc, argv);
    } else {
      // log error
    }

    // TODO: getting of DE and PE info below should move to machine model ??
    //  But then encapsulation of MPI splits across two classes:
    //   Comm and Machine, which could be ok

    // get size of DE process group
    // TODO: MPI overrides given nProcs ?
    MPI_Comm_size(MPI_COMM_WORLD, &nProcs);
    numDEs = nThreadsPerProc * nProcs;

    // get my unique DE process group ID
    MPI_Comm_rank(MPI_COMM_WORLD, &(DE->pID));  // TODO same as MPI rank for now
    //cout << "pID = " << DE->pID << "\n";

    // share it with all sub-threads to calculate unique DE ids
    pthread_mutex_lock(&initMutex);
      nodeRank = DE->pID;
      pthread_cond_broadcast(&initCV);
    pthread_mutex_unlock(&initMutex);

  } // end ESMC_PROCESS

  // determine thread index
  DE->tID = 0; // for nThreadsPerProc = 1, default to main thread index
  if (nThreadsPerProc > 1) {
    pthread_mutex_lock(&initMutex);
      pthread_t mytid = pthread_self();
      for(int i=1; i<nThreadsPerProc; i++) { // start at 1st real thread
        if (ESMC_Comm_tid == 0) {
          // TODO: LogErr
          cout << "ESMC_CommInit: ESMC_Comm_tid not allocated by main()"
               << endl;
          return(ESMF_FAILURE);
        }
        if (pthread_equal(mytid, ESMC_Comm_tid[i])) {
          DE->tID = i;
    //cout << "tid, i = " << ESMC_Comm_tid[i] << ", " << i << endl;
          break;
        }
      }
    pthread_mutex_unlock(&initMutex);
  } // end if nThreadsPerProc > 1

  // calculate unique DE id across all nodes and threads/processes
  pthread_mutex_lock(&initMutex);
    while(nodeRank < 0) {
        pthread_cond_wait(&initCV, &initMutex);
    }
    DE->esmfID = nodeRank * nThreadsPerProc + DE->tID;
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
//    none
//
// !DESCRIPTION:
//      ESMF routine which finalizes a Comm object; performs any 
//      necessary clean-up.
//
//EOP
// !REQUIREMENTS:  
 
  int finalized;

#if 0
  // TODO: if this isn't COMM_WORLD, it might be ok to finalize MPI,
  // but in general we don't want this to happen until the very end
  // of the program.  just deleting a layout is too early to do this.
  pthread_mutex_lock(&finalMutex);
    if (!commFinal) {
  //cout << "ESMC_CommFinal DE = " << DE << endl;
      if (DE->deType == ESMC_PROCESS) {
          MPI_Finalized(&finalized);
          if (!finalized)
              MPI_Finalize();
      }
      commFinal = true;
    }
  pthread_mutex_unlock(&finalMutex);
#endif

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
// !REQUIREMENTS:  

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
// !REQUIREMENTS:  

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
// !REQUIREMENTS:  

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
// !REQUIREMENTS:  

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
// !REQUIREMENTS:  

  // TODO: mutex protect if numDEs changes during run time?
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

  //cout << "ESMC_Comm numDEs = " << numDEs << endl;

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

//cout << "ESMC_Comm(void) constructor invoked\n";

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
      int *argc,              // in - from main invocation
      char **argv[],          // in - from main invocation
      ESMC_DE *de,            // in - DE we're communicating on behalf of
      int nthreadsperproc,    // in - number of threads per process
      int nprocs,             // in - number of processes
      int lbufsize,           // in - number of local message buffer elements
      ESMC_DataKind lbuftype) { // in - type of local message buffer elements
//
// !DESCRIPTION:
//      Calls standard ESMF deep or shallow methods for initialization
//      with default or passed-in values
//
//EOP

//cout << "ESMC_Comm(argc, argv, de, nthreadsperproc, nprocs, lbufsize, lbuftype) constructor invoked\n";
  ESMC_CommInit(argc, argv, de, nthreadsperproc, nprocs, lbufsize, lbuftype);

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

//cout << "~ESMC_Comm() invoked\n";
    if (!commFinal) {  // don't mutex lock -- will be done in ESMC_CommFinal()
                       //  besides, single integer type reference is atomic
      ESMC_CommFinal();
    }

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
      ESMC_DataKind type,
      ESMC_DE *dest,
      int tag,
      int *request) {
//
// !DESCRIPTION:
//      
//
//EOP

  int destpID;

  dest->ESMC_DEGetpID(&destpID);

#ifdef MPI
  MPI_Request req;
  MPI_Isend(buf, num, ESMC_DataKindToMPI[type], destpID, tag, MPI_COMM_WORLD, &req);
  // add req to linked list TODO:
#endif

  return(ESMF_SUCCESS);

 } // end ESMC_CommIsend

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

//static int count = 0;

  pthread_mutex_lock(&barrierMutex);

//count++;
//cout << "count = " << count << endl;

//cout << "entered Barrier, threadCount = " << *threadCount << endl;

    // if this counter still in use by previous barrier, switch to partner
    if (*threadCount >= nThreadsPerProc) {
//cout << "Barrier switching threadCount" << endl;
       threadCount = (threadCount == &threadCountA) ?
                      &threadCountB : &threadCountA;
    }
    // count how many threads (DEs) have entered the barrier
    (*threadCount)++;
//cout << "threadCount = " << *threadCount << endl;

    // inform main process/thread that all sub-threads are done
    if (*threadCount == nThreadsPerProc-1) {
//cout << "HERE1" << endl;
      pthread_cond_broadcast(&mainProcBarrierCV);
    }

    // when last thread (DE) has entered, reset for next barrier, and inform all
    if (*threadCount == nThreadsPerProc) {
#if 0
if (count == 4 || count == 8) {
for(int i=0; i<12; i++) cout << rbuf[i] << " ";
  memset(rbuf, 0, 12*sizeof(int));
for(int i=0; i<12; i++) cout << rbuf[i] << " ";
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
      while (*threadCount < nThreadsPerProc) {
//cout << "threadCount ptr = " << threadCount << endl;
//cout << "threadCountA ptr = " << &threadCountA << endl;
//cout << "threadCountB ptr = " << &threadCountB << endl;
        pthread_cond_wait(&barrierCV, &barrierMutex);
//cout << "threadCount = " << *threadCount << endl;
      }
    }

  pthread_mutex_unlock(&barrierMutex);

  return(ESMF_SUCCESS);

 } // end ESMC_CommBarrier

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
      ESMC_DataKind type,
      ESMC_DE *srcDE) {
//
// !DESCRIPTION:
//      
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  int srcpID;

  if (sbuf == ESMC_NULL_POINTER || rbuf == ESMC_NULL_POINTER ||
      srcDE == ESMC_NULL_POINTER) {
    return(ESMF_FAILURE);
  }

  srcDE->PE->ESMC_PEGetEsmfID(&srcpID);
  //srcDE->ESMC_DEGetpID(&srcpID);  TODO: really should be (MPI) process ID

  //printf("ESMC_CommScatter(): srcDE's srcPID = %d\n", srcpID);

  MPI_Scatter(sbuf, num, ESMC_DataKindToMPI[type],
              rbuf, num, ESMC_DataKindToMPI[type],
              srcpID, MPI_COMM_WORLD);

  return(ESMF_SUCCESS);

 } // end ESMC_CommScatter

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommAllGatherV - All DEs to All DEs Gather vectors
//
// !INTERFACE:
      int ESMC_Comm::ESMC_CommAllGatherV(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void *sbuf,
      int slen,
      void *rbuf,
      int *rlen,
      int *rdispls,
      ESMC_DataKind type) {
//
// !DESCRIPTION:
//      
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//cout << "entered ESMC_CommAllGatherV(), tidx = " << DE->tID << endl;

    MPI_Allgatherv(sbuf, slen, ESMC_DataKindToMPI[type],
                   rbuf, rlen, rdispls, ESMC_DataKindToMPI[type],
                   MPI_COMM_WORLD);

#if 0
  // copy our data into common buffer
  switch (type)
  {
    case ESMF_I4:
      // copy sbuf to our lbuf slot (don't need to mutex protect since
      //   we're writing to our own unique thread-specific slot)
      //  TODO:  use memcpy for speed ??
      int *myslot = &(lbuf[displs[DE->tID]]);  // start of our slot
      int *data = (int *)sbuf;                 // start of send data
      for (int i=0; i<num; i++) {
        *myslot++ = *data++;
        //((int *)lbuf)[displs[DE->tID] + i] = ((int *)sbuf)[i];
      }
      break;

    default:
      break;
  }

  if (DE->deType == ESMC_PROCESS) {
    // gather local node's thread data first by simply waiting
    //   for them to finish copying their data to the rbuf

    pthread_mutex_lock(&barrierMutex);

//cout << "entered process barrier, threadCount = " << *threadCount << "\n";

      // if this counter still in use by previous barrier, switch to partner
      if (*threadCount >= nThreadsPerProc) {
//cout << "switching threadCount" << endl;
         threadCount = (threadCount == &threadCountA) ?
                        &threadCountB : &threadCountA;
      }

      // use while loop to guard against spurious/erroneous wake-ups
      while (*threadCount < nThreadsPerProc-1) {
//cout << "main waiting for sub-threads with threadCount = " << *threadCount << endl;
        pthread_cond_wait(&mainProcBarrierCV, &barrierMutex);
//cout << "main wokeup with threadCount = " << *threadCount << endl;
      }

    pthread_mutex_unlock(&barrierMutex);

    // then exchange data with other nodes
    MPI_Allgatherv(lbuf, num*nThreadsPerProc, ESMC_DataKindToMPI[type],
                   rbuf, num*nThreadsPerProc, ESMC_DataKindToMPI[type],
                   MPI_COMM_WORLD);

  } // end if ESMC_PROCESS

//cout << "ESMC_CommAllGatherV(), final barrier, tidx = " << DE->tID << endl;

  // wait for all threads to finish copying their data
  ESMC_CommBarrier();
#endif

//cout << "leaving ESMC_CommAllGatherV(), tidx = " << DE->tID << endl;
  return(ESMF_SUCCESS);

 } // end ESMC_CommAllGatherV

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
      ESMC_DataKind type) {
//
// !DESCRIPTION:
//      
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//cout << "entered ESMC_CommAllGather(), tidx = " << DE->tID << endl;

  // copy our data into common buffer
  switch (type)
  {
    case ESMF_I4:
      // copy sbuf to our lbuf slot (don't need to mutex protect since
      //   we're writing to our own unique thread-specific slot)
      //  TODO:  use memcpy for speed ??
      for (int i=0; i<num; i++) {
        ((int *)lbuf)[(DE->tID)*num + i] = ((int *)sbuf)[i];
      }
      break;

    default:
      break;
  }

  if (DE->deType == ESMC_PROCESS) {
    // gather local node's thread data first by simply waiting
    //   for them to finish copying their data to the lbuf

    pthread_mutex_lock(&barrierMutex);

//cout << "entered process barrier, threadCount = " << *threadCount << "\n";

      // if this counter still in use by previous barrier, switch to partner
      if (*threadCount >= nThreadsPerProc) {
//cout << "AllGather switching threadCount" << endl;
         threadCount = (threadCount == &threadCountA) ?
                        &threadCountB : &threadCountA;
      }

      // use while loop to guard against spurious/erroneous wake-ups
      while (*threadCount < nThreadsPerProc-1) {
//cout << "main waiting for sub-threads with threadCount = " << *threadCount << endl;
        pthread_cond_wait(&mainProcBarrierCV, &barrierMutex);
//cout << "main wokeup with threadCount = " << *threadCount << endl;
      }

    pthread_mutex_unlock(&barrierMutex);

    // then exchange data with other nodes
int nMPIprocs;
MPI_Comm_size(MPI_COMM_WORLD, &nMPIprocs);
//cout << "number of MPI procs = " << nMPIprocs << endl;
//sleep(60);
    MPI_Allgather(lbuf, num*nThreadsPerProc, ESMC_DataKindToMPI[type],
                  rbuf, num*nThreadsPerProc, ESMC_DataKindToMPI[type],
                  MPI_COMM_WORLD);
//cout << "MPI_Allgather() complete" << endl;
  }  // end if ESMC_PROCESS

//cout << "ESMC_CommAllGather(), final barrier, tidx = " << DE->tID << endl;
//cout << "ESMC_CommAllGather(), threadCount = " << *threadCount << endl;

  // wait for all threads to finish copying their data
  ESMC_CommBarrier();

//cout << "leaving ESMC_CommAllGather(), tidx = " << DE->tID << endl;
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
      ESMC_DataKind type) {
//
// !DESCRIPTION:
//      
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

#ifdef MPI
  MPI_Alltoall(sbuf, num, ESMC_DataKindToMPI[type],
               rbuf, num, ESMC_DataKindToMPI[type],
               MPI_COMM_WORLD);
#endif

  return(ESMF_SUCCESS);

 } // end ESMC_CommAlltoAll

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommAllReduce - Data Reduction across All DEs
//
// !INTERFACE:
      int ESMC_Comm::ESMC_CommAllReduce(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void *sbuf,
      void *rbuf,
      int num,
      ESMC_DataKind type,
      ESMC_Op op) {
//
// !DESCRIPTION:
//      
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//cout << "entered ESMC_CommAllReduce(), tidx = " << DE->tID << endl;

  switch (op)
  {
    case ESMC_SUM:
    // add our data into common buffer
    switch (type)
    {
      case ESMF_I4:
        pthread_mutex_lock(&bufMutex);
          if(!lbufCleared) {
            memset(lbuf, 0, num*sizeof(int)); // 1st DE in clears lbuf
            lbufCleared = true;
          }
          // add our sbuf to first lbuf slot
          for (int i=0; i<num; i++) {
            ((int *)lbuf)[0] += ((int *)sbuf)[i];
          }
        pthread_mutex_unlock(&bufMutex);
        break;
  
      default:
        break;
    }
    default:
      break;
  }

  if (DE->deType == ESMC_PROCESS) {
    // gather local node's thread data first by simply waiting
    //   for them to finish reducing their data to the rbuf

    pthread_mutex_lock(&barrierMutex);

//cout << "entered process barrier, threadCount = " << *threadCount << "\n";

      // if this counter still in use by previous barrier, switch to partner
      if (*threadCount >= nThreadsPerProc) {
//cout << "AllReduce switching threadCount" << endl;
         threadCount = (threadCount == &threadCountA) ?
                        &threadCountB : &threadCountA;
      }

      // use while loop to guard against spurious/erroneous wake-ups
      while (*threadCount < nThreadsPerProc-1) {
//cout << "main waiting for sub-threads with threadCount = " << *threadCount << endl;
        pthread_cond_wait(&mainProcBarrierCV, &barrierMutex);
//cout << "main wokeup with threadCount = " << *threadCount << endl;
      }

    pthread_mutex_unlock(&barrierMutex);

    // then reduce data with other nodes
//cout << "calling MPI_Allreduce" << endl;
    memset(rbuf, 0, num*sizeof(int)); // clear gbuf
    MPI_Allreduce(lbuf, rbuf, num, ESMC_DataKindToMPI[type],
                  ESMC_OpToMPI[op], MPI_COMM_WORLD);
  }

//cout << "ESMC_CommAllReduce(), final barrier, tidx = " << DE->tID << endl;

  // wait for all threads to finish copying their data
  ESMC_CommBarrier();

  // reset result buffer cleared flag
  pthread_mutex_lock(&bufMutex);
    if (lbufCleared) lbufCleared = false;  // 1st thread in clears it
                                           // TODO: potential race problem
                                           //  if threads get way out of sync ?
  pthread_mutex_unlock(&bufMutex);

//cout << "leaving ESMC_CommAllReduce(), tidx = " << DE->tID << endl;

  return(ESMF_SUCCESS);

 } // end ESMC_CommAllReduce
