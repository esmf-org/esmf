// $Id: ESMC_Route.h,v 1.19 2003/04/29 21:37:45 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

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
 #include <ESMC_DELayout.h>
 #include <ESMC_XPacket.h>
 #include <ESMC_RTable.h>
 #include <ESMC_CommTable.h>

// !PUBLIC TYPES:
 class ESMC_Route;

// !PRIVATE TYPES:

 // class declaration type
 class ESMC_Route : public ESMC_Base {    // inherits from ESMC_Base class

   private:
     // name in base class
     int routeid;           // unique id, used later for cacheing
     ESMC_DELayout *layout;   // layout which includes all src + dst de's
     ESMC_RTable *sendRT;   // send route table
     ESMC_RTable *recvRT;   // receive route table
     ESMC_CommTable *ct;    // communication scheduling table

// !PUBLIC MEMBER FUNCTIONS:

  public:
 // ESMC_RouteCreate and ESMC_RouteDestroy are declared below,
 // outside the ESMC_Route declaration
    int ESMC_RouteConstruct(ESMC_DELayout *layout);
    int ESMC_RouteDestruct(void);

 // accessor methods for class members
    //int ESMC_RouteGet(<value type> *value) const;

    int ESMC_RouteSetSend(int dst_de, ESMC_XPacket *xp);
    int ESMC_RouteSetRecv(int src_de, ESMC_XPacket *xp);
    
 // initialize the communication routines in this route object
    int ESMC_RoutePrecompute(int rank, int my_DE_rcv, 
                       ESMC_AxisIndex *AI_rcv_exc, ESMC_AxisIndex *AI_rcv_tot,
                       int AI_rcv_count, ESMC_DELayout *layout_rcv,
                       int my_DE_snd, 
                       ESMC_AxisIndex *AI_snd_exc, ESMC_AxisIndex *AI_snd_tot,
                       int AI_snd_count, ESMC_DELayout *layout_snd);
    int ESMC_RoutePrecomputeHalo(int rank, int my_DE, ESMC_AxisIndex *AI_exc,
                                 ESMC_AxisIndex *AI_tot, int AI_count, 
                                 ESMC_DELayout *layout);

 // execute the communication routines set up in this route object
    int ESMC_RouteRun(void *srcaddr, void *dstaddr);

 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_RouteValidate(const char *options) const;
    int ESMC_RoutePrint(const char *options) const;

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

 ESMC_Route *ESMC_RouteCreate(ESMC_DELayout *layout, int *rc);
 int ESMC_RouteDestroy(ESMC_Route *route);
 int ESMC_RouteGetCached(int rank, int my_DE_rcv, ESMC_AxisIndex *AI_rcv,
                             int AI_rcv_count, ESMC_DELayout *layout_rcv,
                             int my_DE_snd, ESMC_AxisIndex *AI_snd,
                             int AI_snd_count, ESMC_DELayout *layout_snd,
			     ESMC_Logical *hascachedroute, ESMC_Route **route);

 #endif  // ESMC_Route_H
