// $Id: ESMC_Comm.h,v 1.7 2003/02/21 05:19:05 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMF Comm C++ definition include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

 #ifndef ESMC_Comm_H
 #define ESMC_Comm_H

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.
// #include <ESMC_CommMem.h> 
 #include <pthread.h> 

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_Comm - Interface to various communications protocols
//
// !DESCRIPTION:
//
// The code in this file defines the C++ Comm members and declares method 
// signatures (prototypes).  The companion file ESMC\_Comm.C contains
// the definitions (full code bodies) for the Comm methods.
//
// < insert a paragraph or two explaining what you'll find in this file >
//
//-----------------------------------------------------------------------------
// 
// !USES:
 #include <ESMC_Base.h>  // all classes inherit from the ESMC Base class.
 #include <ESMC_DE.h>    // Comm class communicates between DEs
 //#include <ESMC_XXX.h>   // other dependent classes (subclasses, aggregates,
                        // composites, associates, friends)

// !PUBLIC TYPES:
// class ESMC_CommConfig;
 class ESMC_Comm;

 enum ESMC_Type_e {ESMC_INT=0, ESMC_LONG, ESMC_FLOAT, ESMC_DOUBLE};
 #define ESMC_COMM_TYPES 4

 enum ESMC_Op_e {ESMC_SUM=0, ESMC_MIN, ESMC_MAX};
 #define ESMC_OP_TYPES 3

 #define ESMC_MAX_QUEUE 1000

// !PRIVATE TYPES:

 // class configuration type
// class ESMC_CommConfig {
//   private:
 //   < insert resource items here >
// };

 // class definition type
 class ESMC_Comm : public ESMC_Base {    // inherits from ESMC_Base class

   private:
 //  < insert class members here >  corresponds to type ESMF_Comm members
 //                                 in F90 modules
     ESMC_DE *DE;         // this DE we're communicating on behalf of

     // ESMC-to-MPI type maps
     int ESMC_TypeToMPI[ESMC_COMM_TYPES];
     int ESMC_OpToMPI[ESMC_OP_TYPES];

     void *ESMC_Request; // linked list of requests
     void *ESMC_Status;  // linked list of statuses

     // shared memory variables: single instance visible to all threads in proc

     static int numDEs;          // number of DEs in communication group

     // DE type configuration TODO: get from config file ?
     static int nThreadsPerProc;
     static int nProcs;

     // local inter-thread message buffer TODO: beginnings of memory mgmt ?
     //                                         get from config file ?
     static void *lbuf;
     static int lbufSize;
     static ESMC_Type_e lbufType;

     // local inter-thread communication variables
     static pthread_mutex_t bufMutex;
     static pthread_mutex_t finalMutex;
     static pthread_mutex_t initMutex;
     static pthread_cond_t initCV;
     static pthread_mutex_t barrierMutex;
     static pthread_cond_t barrierCV;
     static pthread_cond_t mainProcBarrierCV;
     static int *threadCount;    // count threads in a barrier
     static int threadCountA;
     static int threadCountB;
     static bool lbufCleared;

     // shared memory MPI rank of this node used by threads to calculate
     // unique DE id
     static int nodeRank;

     // flag to prevent double finalization via destructor
     static bool commFinal;


// !PUBLIC MEMBER FUNCTIONS:
//
  public:
    int ESMC_CommInit(int *argc, char **argv[], ESMC_DE *de); // TODO: 
                                        // supports Layout.comm.CommInit, F90
    int ESMC_CommInit(int *argc, char **argv[], ESMC_DE *de, 
                      int nthreadsperproc, int nprocs,
                      int lbufsize, ESMC_Type_e lbuftype); // TODO: config file
    int ESMC_CommFinal(void);

 // optional configuration methods
//    int ESMC_CommGetConfig(ESMC_CommConfig *config) const;
//    int ESMC_CommSetConfig(const ESMC_CommConfig *config);

 // accessor methods for class members
//    int ESMC_CommGet<Value>(<value type> *value) const;
//    int ESMC_CommSet<Value>(<value type>  value);
    int ESMC_CommGetDEIDs(ESMC_DE *de) const;
    int ESMC_CommGetNumDEs(int *ndes) const;
    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_CommValidate(void) const;
    int ESMC_CommPrint(void) const;

 // native C++ constructors/destructors
	ESMC_Comm(void);
    ESMC_Comm(int *argc, char **argv[], ESMC_DE *de, 
              int nthreadsperproc, int nprocs,
              int lbufsize, ESMC_Type_e lbuftype);
	~ESMC_Comm(void);
  
 // < declare the rest of the public interface methods here >
    // point-to-point
    int ESMC_CommIsend(void *buf, int num, ESMC_Type_e type, ESMC_DE *dest,
                       int tag, int *request);
    int ESMC_CommIrecv(void *buf, int num, ESMC_Type_e type, ESMC_DE *source,
                       int tag, int *request);
    int ESMC_CommWait(int *request, int *status);

    // collectives
    int ESMC_CommBarrier(void);
    int ESMC_CommBcast(void *buf, int num, ESMC_Type_e type, ESMC_DE *root);
    int ESMC_CommScatter(void *sbuf, void *rbuf, int num, ESMC_Type_e type,
                         ESMC_DE *root);
    int ESMC_CommAllGather(void *sbuf, void *rbuf, int num, ESMC_Type_e type);
    int ESMC_CommAllGatherV(void *sbuf, int slen, void *rbuf, int *rlen,
                            int *rdispls, ESMC_Type_e type);
    int ESMC_CommAlltoAll(void *sbuf, void *rbuf, int num, ESMC_Type_e type);
    int ESMC_CommAllReduce(void *sbuf, void *rbuf, int num, ESMC_Type_e type,
                           ESMC_Op_e op);
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_Comm

 #endif  // ESMC_Comm_H
