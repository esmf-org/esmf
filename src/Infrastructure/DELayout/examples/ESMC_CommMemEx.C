// $Id: ESMC_CommMemEx.C,v 1.5 2004/01/09 21:58:00 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
//
// ESMC CommMem prototype application example program
//
// NOTE: this is prototype test code; don't write user-level code based on this!
//
//_____________________________________________________________________________
//!EXAMPLE        String used by test script to count examples.
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// Excercises CommMem classes ESMC_PE, ESMC_DE, ESMC_PEList, ESMC_DELayout,
// ESMC_Comm and ESMC_Machine to build a PE List and corresponding DELayout.
// Uses mixed mode communication of MPI and pthreads to perform an "All Gather"
// collective of PE information to create a PE list.
//
// on halem, run with
// bsub -P "hp606" -q general -n 8 prun -m cyclic -n 2 -N 2 ./ESMC_CommMemEx
//-----------------------------------------------------------------------------

#include <ESMC_Base.h>
#include <ESMC_DE.h>
#include <ESMC_PE.h>
#include <ESMC_PEList.h>
#include <ESMC_Comm.h>
#include <ESMC_DELayout.h>
#include <ESMC_Comp.h>
#include <iostream.h>
//#include <iostream> // TODO: use when namespaces consistently implemented
//using std::cout; 
//using std::endl;
#include <pthread.h>
#include <math.h>    // pow()
#include <unistd.h>  // sleep()

#define ESMC_COMM_NTHREADSPERPROC 4
#define ESMC_COMM_NPROCS 2

static const success = ESMF_SUCCESS;
static const failure = ESMF_FAILURE;


// pthread start function requires C-linkage
extern "C" {
void *do_DE(void *);
}

// pack arg type
struct arg_s {
  int argc;
  char **argv;
  ESMC_DEType_e detype;
};

// shared memory thread id array, shared with ESMC_Comm.C
extern pthread_t *ESMC_Comm_tid;

// shared memory global receive buffer for inter-node MPI communications
int gbuf[ESMC_COMM_NPROCS * ESMC_COMM_NTHREADSPERPROC * 3];

///////////////////////////////
//    main process/thread    //
///////////////////////////////

int main(int argc, char **argv)
{
  // Define return codes
  int  *threadrc, rc, finalrc=ESMF_SUCCESS;

  //rc = ESMC_Initialize();

  // allocate ESMC_Comm_tid array to share with ESMC_Comm
  ESMC_Comm_tid = new pthread_t[ESMC_COMM_NTHREADSPERPROC];

  // pack args for separate DE threads
  arg_s t_arg = { argc, argv, ESMC_THREAD };

  // record main thread id
  ESMC_Comm_tid[0] = pthread_self();

  // start other DE (worker) threads
  //   note: loop starts at 1 since 0 is main thread
  for (int i=1; i<ESMC_COMM_NTHREADSPERPROC; i++) {
    rc = pthread_create(&ESMC_Comm_tid[i], NULL, do_DE, (void *)&t_arg);
    if (rc != ESMF_SUCCESS) {
	finalrc = ESMF_FAILURE;
    }
    //cout << "thread " << i << " started, tid=" << ESMC_Comm_tid[i] << endl;
  }

  // allow other threads to start before main thread continues
  sleep(1);  // TODO:  ?? avoids MPI_Init() vs. thread creation race condition
             //           condition variable initCV in CommInit not enough ? 
             //           - no, doesn't prevent main from racing thru first
             //           need "all threads created" condition variable ?

  // main() participates as DE worker process/thread itself; calls
  //  do_DE() directly (not as new thread via pthread_create() )
  t_arg.detype = ESMC_PROCESS;
  do_DE((void *)&t_arg);

  // wait for other (DE) threads to exit
  //   note: loop starts at 1 since 0 is main thread
  for (int i=1; i<ESMC_COMM_NTHREADSPERPROC; i++) {
    rc = pthread_join(ESMC_Comm_tid[i], (void**) &threadrc);
    if ((rc != ESMF_SUCCESS ) & (*threadrc != ESMF_SUCCESS)) {
        finalrc = ESMF_FAILURE;
    }

   if (finalrc == ESMF_SUCCESS) {
        cout << "PASS: ESMC_CommMemEx.C" << endl;
        return(ESMF_SUCCESS);
   }
   else {
        cout << "FAIL: ESMC_CommMemEx.C" << endl;
        return(ESMF_FAILURE);
   }


  }

  //rc = ESMC_Finalize();

}

///////////////////////
//    sub-threads    //
///////////////////////

// DE work function, called direct from main() process/thread, or as
//  separate pthread_create thread start function
void *do_DE(void *t_arg)
{
  // unpack args
  int argc, rc;
 int finalrc = ESMF_SUCCESS;
  char **argv;
  ESMC_DEType_e detype;
  argc   = ((arg_s *) t_arg)->argc;
  argv   = ((arg_s *) t_arg)->argv;
  detype = ((arg_s *) t_arg)->detype;

cout << "I am tid, detype " << pthread_self() << ", " << detype << endl;

  // initialize this DE's type with passed-in value (THREAD or PROCESS)
  ESMC_DE de(detype);

  // instantiate and initialize an ESMC comm object
  int nDEs=0, myDEid=0;
  ESMC_Comm comm;
  comm.ESMC_CommInit(&argc, &argv, &de, ESMC_COMM_NTHREADSPERPROC, 
                     ESMC_COMM_NPROCS, ESMC_COMM_NTHREADSPERPROC*3, ESMF_I4);
  comm.ESMC_CommGetNumDEs(&nDEs);
cout << "comm group size = " << nDEs << "\n";
  de.ESMC_DEGetESMFID(&myDEid);
cout << "myDEid = " << myDEid << "\n";

  // instantiate and initialize an ESMC machine object
  ESMC_Machine mach;
  mach.ESMC_MachineInit(64, 256, 4, true, true, true, 1, 100, 2, 200);

  // get my pe, cpu and node ids
  int mypeid=0, mycpuid=0, mynodeid=0;
  ESMC_PE pe;
  pe.ESMC_PEInit(&mach);
  pe.ESMC_PESetEsmfID(myDEid);  // assume 1-to-1 DE-to-PE for now:
  pe.ESMC_PEGetEsmfID(&mypeid); //   (mypeid = myDEid)
  pe.ESMC_PEGetCpuID(&mycpuid);
  pe.ESMC_PEGetNodeID(&mynodeid);
cout << "mycpuid, mynodeid = " << mycpuid << ", " << mynodeid << "\n";

  // prepare send buffer
  //  TODO: ?? use MPI derived type to send whole PE object rather
  //           than 3 ints. Then use resulting receive buffer as PE list array,
  //           thereby avoiding a copy operation.
  int sendbuf[3];
  sendbuf[0] = mypeid;
  sendbuf[1] = mycpuid;
  sendbuf[2] = mynodeid;

  // gather all PE IDs from all DEs
  rc = comm.ESMC_CommAllGather(sendbuf, gbuf, 3, ESMF_I4);

  if (rc != ESMF_SUCCESS) {
        finalrc = ESMF_FAILURE;
  }

//cout << "main: exited ESMC_CommAllGather" << endl;

  // create PE List from gathered IDs

  // create a PE list object entirely on the heap
  ESMC_PEList *peList = ESMC_PEListCreate(nDEs, &rc); // assume #PEs = nDEs
                                                      //   for now

  for(int i=0; i<nDEs; i++) {
    // retrieve ids from receive buffer
    mypeid   = gbuf[i*3];
    mycpuid  = gbuf[i*3+1];
    mynodeid = gbuf[i*3+2];

    // populate PE list with retrieved ids
    peList->ESMC_PEListInit(i, mypeid, mycpuid, mynodeid);

    // show what main thread DE got
    if (detype == ESMC_PROCESS) {
      cout << "(peid,cpuid,nodeid) = "  << mypeid   << " "
                                             << mycpuid  << " "
                                             << mynodeid << " \n";
    }
  }

  // sort PE list by node to prep assignment to layout
  //peList->ESMC_PEListPrint();
  peList->ESMC_PEListSort();

  // now let's create a layout, using our sorted PE list
  // TODO:  can't currently do, since DELayout now contains a Comm, which can't
  //        be initialized with NPROC/NTHREAD info yet (eventually from
  //        a config file, or expose at DELayoutCreate, which doesn't seem right)
//cout << "main: deid " << myDEid << " calling ESMC_DELayoutCreate()" << endl;
  //ESMC_DELayout *layout = ESMC_DELayoutCreate(2,4,1, peList, ESMC_YFAST, &rc);
  //ESMC_DELayout layout;
  //layout.ESMC_DELayoutConstruct(2,3,1, peList, ESMC_YFAST);

  // let's see what we have (show main thread DE's copy only)
  if (detype == ESMC_PROCESS) {
    peList->ESMC_PEListPrint();
    //layout->ESMC_DELayoutPrint();
  }

  // now get rid of 'em
  rc = ESMC_PEListDestroy(peList);    // deallocates entire object

  if (rc != ESMF_SUCCESS) {
      finalrc = ESMF_FAILURE;
  }

  //rc = ESMC_DELayoutDestroy(layout);
  //layout.ESMC_DELayoutDestruct();

//cout << "DE " << myDEid << " doing CommFinal" << endl;
  if (detype == ESMC_PROCESS) sleep(1);
                       // TODO: avoids MPI-thead shutdown race condition ??
                       //   allows other threads to exit gracefully before
                       //   MPI_Finalize()
                       //   need "all threads pthread_exit()ed" condition var ?
  comm.ESMC_CommFinal();

#if 0
  // dummy computation to keep this DE busy while we peek
  //   at DE-to-PE scheduling
  double a;
  for(int r=0; r < 10000000; r++) {
    a = 3.14159 * pow((double)r, 2.0);
    if(r%1000000 == 0) {
      // let's see what PE (cpu & node) we're currently running on
      pe.ESMC_PEInit(&mach);
      pe.ESMC_PEGetCpuID(&mycpuid);
      pe.ESMC_PEGetNodeID(&mynodeid);
     //cout << "tid, mycpuid, mynodeid = " << pthread_self() << ", " << mycpuid << ", " << mynodeid << "\n";
     }
  }
#endif
//cout << "DE " << myDEid << " outa here!" << endl;
  if (detype == ESMC_THREAD) { // don't exit main thread !
	if (finalrc == ESMF_SUCCESS) {
		pthread_exit((void*) &success); 
	}
	else {
		pthread_exit((void*) &failure); 
        }
   }

   if (finalrc == ESMF_SUCCESS) {
		return((void*) &success); 
	}
	else {
		return((void*) &failure); 
        }
}
