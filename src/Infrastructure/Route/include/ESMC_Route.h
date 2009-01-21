// $Id: ESMC_Route.h,v 1.66.2.3 2009/01/21 21:25:23 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMF Route C++ declaration include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

 #ifndef ESMC_Route_H
 #define ESMC_Route_H

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_Route - Top level Route object
//
// !DESCRIPTION:
//
// The code in this file defines the C++ Route members and declares method 
// signatures (prototypes).  The companion file ESMC_Route.C contains
// the definitions (full code bodies) for the Route methods.
//
// 
//
//-----------------------------------------------------------------------------
// 
// !USES:
 #include <ESMC_Base.h>        // all classes inherit from the ESMC Base class.
 #include <ESMC_VM.h>
 #include <ESMCI_DELayout.h>
 #include <ESMC_XPacket.h>
 #include <ESMC_RTable.h>
 #include <ESMC_CommTable.h>

// !PUBLIC TYPES:
 class ESMC_Route;

// if you change these, you *must* change the corresponding values
// in the ESMF_Route.F90 file.
 enum ESMC_RouteOptions {
         ESMC_ROUTE_OPTION_NONE        = 0x000,
         ESMC_ROUTE_OPTION_ASYNC       = 0x001,
         ESMC_ROUTE_OPTION_SYNC        = 0x002,
         ESMC_ROUTE_OPTION_PACK_PET    = 0x004,
         ESMC_ROUTE_OPTION_PACK_XP     = 0x008,
         ESMC_ROUTE_OPTION_PACK_NOPACK = 0x010,
         ESMC_ROUTE_OPTION_PACK_VECTOR = 0x020,
         ESMC_ROUTE_OPTION_PACK_BUFFER = 0x040,
         ESMC_ROUTE_OPTION_DEFAULT     = 0x045,
 };

// !PRIVATE TYPES:


 // class declaration type
 class ESMC_Route : public ESMC_Base {    // inherits from ESMC_Base class

   private:
     // name in base class
     int routeid;           // unique id, used later for cacheing
     ESMC_RouteOptions options; // sync/async, how to pack when communicating
     ESMCI::VM *vm;         // vm on which this route runs
                            // must include VASs of all src + dst de's
     ESMC_RTable *sendRT;   // send route table
     ESMC_RTable *recvRT;   // receive route table
     int recvitems;         // if >0, numitems needed in the destination array
     ESMC_CommTable *ct;    // communication scheduling table
     ESMC_R8 timer1, timer2, timer3;

// !PUBLIC MEMBER FUNCTIONS:

  public:
 // ESMC_RouteCreate and ESMC_RouteDestroy are declared below,
 // outside the ESMC_Route declaration
    int ESMC_RouteConstruct(ESMCI::VM *vm);
    int ESMC_RouteDestruct(void);

 // accessor methods for class members
    //int ESMC_RouteGet(<value type> *value) const;

    int ESMC_RouteSetSend(int dst_pet, ESMC_XPacket *xp);
    int ESMC_RouteSetRecv(int src_pet, ESMC_XPacket *xp);
    
    int ESMC_RouteSetOptions(ESMC_RouteOptions opt);
    int ESMC_RouteGetOptions(void) { return options; };

    int ESMC_RouteSetRecvItems(int nitems);
    int ESMC_RouteGetRecvItems(void);

 // a couple very specialized count routines - see the actual code for details.
    int ESMC_RouteGetSumMaxXPsPerPET(int *count);
    int ESMC_RouteGetSumMaxRegionsPerXP(int *count);

    // initialize the communication routines in this route object
    int ESMC_RoutePrecomputeHalo(int rank, int my_DE, ESMC_AxisIndex *AI_exc,
                       ESMC_AxisIndex *AI_tot, int AI_count, 
                       int *global_start, int *global_count,
                       ESMCI::DELayout *delayout,
                       ESMC_Logical *periodic = NULL);

    int ESMC_RoutePrecomputeRedist(int rank, ESMC_Logical hasSrcData, 
                                   ESMCI::DELayout *srcDELayout,
                                   int mySrcDE, int srcDECount,
                                   ESMC_AxisIndex *srcGlobalCompAIperDEperRank,
                                   ESMC_AxisIndex *mySrcGlobalTotalAIperRank,
                                   ESMC_Logical  hasDstData,
                                   ESMCI::DELayout *dstDELayout,
                                   int myDstDE, int dstDECount,
                                   ESMC_AxisIndex *dstGlobalCompAIperDEperRank,
                                   ESMC_AxisIndex *myDstGlobalTotalAIperRank);

    int ESMC_RoutePrecomputeRedistV(int rank, ESMC_Logical hasDstData, 
                       int dstMyDE, ESMC_Logical dstVector,
                       ESMC_AxisIndex *dstCompAI, ESMC_AxisIndex *dstTotalAI,
                       int dstAICount, int *dstAICountPerDE,
                       int *dstGlobalStart, int dstGSCount, int *dstGlobalCount,
                       ESMCI::DELayout *dstdeLayout, ESMC_Logical hasSrcData,
                       int srcMyDE, ESMC_Logical srcVector,
                       ESMC_AxisIndex *srcCompAI, ESMC_AxisIndex *srcTotalAI,
                       int srcAICount, int *srcAICountPerDE,
                       int *srcGlobalStart, int srcGSCount, int *srcGlobalCount,
                       ESMCI::DELayout *srcdeLayout);
    int ESMC_RoutePrecomputeRedistA2A(int rank, ESMC_Logical hasDstData,
                       int dstMyDE,
                       ESMC_AxisIndex *dstCompAI,
                       int dstAICount, int *dstAICountPerDE,
                       int *dstGlobalStart, int dstGSCount, int *dstGlobalCount,
                       ESMCI::DELayout *dstdeLayout, ESMC_Logical hasSrcData,
                       int srcMyDE,
                       ESMC_AxisIndex *srcCompAI,
                       int srcAICount, int *srcAICountPerDE,
                       int *srcGlobalStart, int srcGSCount, int *srcGlobalCount,
                       ESMCI::DELayout *srcdeLayout);
    int ESMC_RoutePrecomputeRegrid(int rank, int my_DE_rcv, 
                       ESMC_AxisIndex *AI_rcv_exc, ESMC_AxisIndex *AI_rcv_tot,
                       int AI_rcv_count, int *global_start_rcv,
                       int *global_count_rcv, ESMCI::DELayout *delayout_rcv,
                       int my_DE_snd, 
                       ESMC_AxisIndex *AI_snd_exc, ESMC_AxisIndex *AI_snd_tot,
                       int AI_snd_count, int *global_start_snd,
                       int *global_count_snd, ESMCI::DELayout *delayout_snd);
    int ESMC_RoutePrecomputeDomList(int rank, ESMCI::DELayout *srcDELayout, 
                       ESMCI::DELayout *dstDELayout,
                       ESMC_DomainList *srcDomainList,
                       ESMC_DomainList *dstDomainList,
                       ESMC_Logical *hasSrcData, ESMC_Logical *hasDstData);

    // execute the communication routines set up in this route object
    // most frequently used interface is the first; the second one allows
    // a list of src and dst addrs to be passed in.

    int ESMC_RouteRun(void *srcaddr, void *dstaddr, ESMC_TypeKind dk);
    int ESMC_RouteRun(void **srcaddr, void **dstaddr, ESMC_TypeKind dk, 
                           int numAddrs=1);

 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_RouteValidate(int srcbufcount, int *srcbufsizes, 
                           int dstbufcount, int *dstbufsizes,
                           const char *options=NULL) const;
    int ESMC_RoutePrint(const char *options=NULL) const;

 // native C++ constructors/destructors
	ESMC_Route(void);
	~ESMC_Route(void);
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 

//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_Route

// Create and Destroy are declared as class helper functions (not methods)
// since they create and destroy an ESMC_Route object itself. E.g. if Create
// were a method, the ESMC_Route object on whose behalf it was being invoked
// would need to already exist!  These functions are supersets of C++ new
// and delete; they perform allocation/deallocation specialized to
// an ESMC_Route object.

 ESMC_Route *ESMC_RouteCreate(ESMCI::VM *vm, int *rc);
 int ESMC_RouteDestroy(ESMC_Route *route);

 #endif  // ESMC_Route_H
