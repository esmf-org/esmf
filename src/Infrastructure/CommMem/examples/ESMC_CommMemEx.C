// $Id: ESMC_CommMemEx.C,v 1.1 2002/12/13 21:09:41 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC CommMem application example program

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// Excercises CommMem classes ESMC_PE, ESMC_DE, ESMC_PEList, ESMC_Layout,
// ESMC_Comm and ESMC_Machine to build a PE List and corresponding Layout.
// Uses mixed mode communication of MPI and pthreads to perform an "All Gather"
// collective of PE information to create a PE list.
//
// on halem, run with
// bsub -P "hp606" -q general -n 8 prun -m cyclic -n 2 -N 2 ./ESMC_CommMemEx
//-----------------------------------------------------------------------------

#include <ESMC_DE.h>
#include <ESMC_PE.h>
#include <ESMC_PEList.h>
#include <ESMC_Comm.h>
#include <ESMC_Layout.h>
#include <iostream>
//using std::cout;  // TODO: use when namespaces consistently implemented
//using std::endl;
#include <pthread.h>
#include <math.h>    // pow()
#include <unistd.h>  // sleep()

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

// shared memory thread id array
pthread_t ESMC_Comm_tid[ESMC_COMM_NTHREADS];

// shared memory local buffer for intra-node pthreads communications
int *lbuf = 0;

// shared memory global receive buffer for inter-node MPI communications
int *gbuf = 0;

// shared memory MPI rank of this node used by threads to calculate unique DE id
int nodeRank=-1;

///////////////////////////////
//    main process/thread    //
///////////////////////////////

int main(int argc, char **argv)
{
  // allocate local and receive buffers 
  if (lbuf == 0) lbuf = new int[ESMC_COMM_NTHREADS * 3];
  if (gbuf == 0) gbuf = new int[ESMC_COMM_NNODES * ESMC_COMM_NTHREADS * 3];

  // pack args for separate DE threads
  arg_s t_arg = { argc, argv, ESMC_THREAD };

  // record main thread id
  ESMC_Comm_tid[0] = pthread_self();

  // start other DE (worker) threads
  for (int i=1; i<ESMC_COMM_NTHREADS; i++) {
    pthread_create(&ESMC_Comm_tid[i], NULL, do_DE, (void *)&t_arg);
    //cout << "thread " << i << " started, tid=" << ESMC_Comm_tid[i] << endl;
  }

  sleep(1);  // TODO:  ?? avoids MPI_Init() vs. thread creation race condition

  // main() participates as DE worker process/thread itself; calls
  //  do_DE() directly (not as new thread via pthread_create() )
  t_arg.detype = ESMC_PROCESS;
  do_DE((void *)&t_arg);

  // wait for other (DE) threads to exit
  for (int i=1; i<ESMC_COMM_NTHREADS; i++) {
    pthread_join(ESMC_Comm_tid[i], NULL);
  }

  return(0);
}

///////////////////////
//    sub-threads    //
///////////////////////

// DE work function, called direct from main() process/thread, or as
//  separate thread start function
void *do_DE(void *t_arg)
{
  // unpack args
  int argc;
  char **argv;
  ESMC_DEType_e detype;
  argc   = ((arg_s *) t_arg)->argc;
  argv   = ((arg_s *) t_arg)->argv;
  detype = ((arg_s *) t_arg)->detype;

//cout << "I am tid, detype " << pthread_self() <<
             //", " << detype << endl;

  // initialize this DE's type
  ESMC_DE de(detype);

  // instantiate and initialize an ESMC comm object
  int nDEs=0, myDEid=0;
  ESMC_Comm comm;
  comm.ESMC_CommInit(&argc, &argv, &de);
  comm.ESMC_CommGetNumDEs(&nDEs);
//cout << "comm group size = " << nDEs << "\n";
  de.ESMC_DEGetESMFID(&myDEid);
//cout << "myDEid = " << myDEid << "\n";

  // get my pe, cpu and node ids
  int mypeid=0, mycpuid=0, mynodeid=0;
  ESMC_PE pe;
  pe.ESMC_PEInit();
  pe.ESMC_PESetEsmfID(myDEid);  // assume 1-to-1 DE-to-PE for now:
  pe.ESMC_PEGetEsmfID(&mypeid); //   (mypeid = myDEid)
  pe.ESMC_PEGetCpuID(&mycpuid);
  pe.ESMC_PEGetNodeID(&mynodeid);
//cout << "mycpuid, mynodeid = " << mycpuid << ", " << mynodeid << "\n";

  // prepare send buffer
  //  TODO: ?? use MPI derived type to send whole PE object rather
  //           than 3 ints. Then use resulting receive buffer as PE list array,
  //           thereby avoiding a copy operation.
  int sendbuf[3];
  sendbuf[0] = mypeid;
  sendbuf[1] = mycpuid;
  sendbuf[2] = mynodeid;

  // gather all PE IDs from all DEs
  comm.ESMC_CommAllGather(sendbuf, lbuf, 3, ESMC_INT);

  // create PE List from gathered IDs

  // create a PE list object entirely on the heap
  int rc;
  ESMC_PEList *peList = ESMC_PEListCreate(nDEs, &rc); // assume #PEs = nDEs
                                                      //   for now

  for(int i=0; i<nDEs; i++) {
    // retrieve ids from receive buffer
    mypeid   = gbuf[i*3];
    mycpuid  = gbuf[i*3+1];
    mynodeid = gbuf[i*3+2];

    // populate PE list with retrieved ids
    peList->ESMC_PEListInit(i, mypeid, mycpuid, mynodeid);

    if (myDEid == 0) {
      cout << "(peid,cpuid,nodeid) = "  << mypeid   << " "
                                             << mycpuid  << " "
                                             << mynodeid << " \n";
    }
  }

  // sort PE list by node to prep assignment to layout
  //peList->ESMC_PEListPrint();
  peList->ESMC_PEListSort();

  // now let's create a layout, using our sorted PE list
  ESMC_Layout *layout = ESMC_LayoutCreate(2,4,1, peList, ESMC_YFAST, &rc);
  //ESMC_Layout layout;
  //layout.ESMC_LayoutConstruct(2,3,1, peList, ESMC_YFAST);

  // let's see what we have (show one DE's copy only)
  if (myDEid == 0) {
    peList->ESMC_PEListPrint();
    layout->ESMC_LayoutPrint();
  }

  // now get rid of 'em
  rc = ESMC_PEListDestroy(peList);    // deallocates entire object
  rc = ESMC_LayoutDestroy(layout);
  //layout.ESMC_LayoutDestruct();

  comm.ESMC_CommFinal();

#if 0
  // dummy computation to keep this DE busy while we peek
  //   at DE-to-PE scheduling
  double a;
  for(int r=0; r < 10000000; r++) {
    a = 3.14159 * pow((double)r, 2.0);
    if(r%1000000 == 0) {
      // let's see what PE (cpu & node) we're currently running on
      pe.ESMC_PEInit();
      pe.ESMC_PEGetCpuID(&mycpuid);
      pe.ESMC_PEGetNodeID(&mynodeid);
     //cout << "tid, mycpuid, mynodeid = " << pthread_self() << ", " << mycpuid << ", " << mynodeid << "\n";
     }
  }
#endif

  if (detype == ESMC_THREAD) pthread_exit(NULL); // don't exit main thread !
  return(NULL);
}
