// $Id: ESMC_Machine.h,v 1.4 2003/03/11 03:00:43 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMF Machine C++ definition include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

 #ifndef ESMC_Machine_H
 #define ESMC_Machine_H

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.
// #include <ESMC_CommMem.h> 

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_Machine - encapsulates platform capabilites and dependencies
//
// !DESCRIPTION:
//
// The code in this file defines the C++ Machine members and declares method 
// signatures (prototypes).  The companion file ESMC\_Machine.C contains
// the definitions (full code bodies) for the Machine methods.
//
// 
//
//-----------------------------------------------------------------------------
// 
// !USES:
 #include <ESMC_Base.h>  // all classes inherit from the ESMC Base class.
 //#include <ESMC_XXX.h>   // other dependent classes (subclasses, aggregates,
                        // composites, associates, friends)

// !PUBLIC TYPES:
// class ESMC_MachineConfig;
 class ESMC_Machine;

// !PRIVATE TYPES:

 // class configuration type
// class ESMC_MachineConfig {
//   private:
 //   < insert resource items here >
// };

 // class definition type
 class ESMC_Machine : public ESMC_Base {    // inherits from ESMC_Base class

   private:
 //  < insert class members here >  corresponds to type ESMF_Machine members
 //                                 in F90 modules
     int numNodes;         // number of nodes in machine or domain
     int numCPUs;          // number of CPUs in machine or domain
     int numCPUsperNode;   // number of CPUs per Node
     bool hasMPI;          // machine has MPI 1.2
     bool hasOpenMP;       // machine has OpenMP 1.0
     bool hasShmem;        // machine has Shmem
     bool hasPthreads;     // machine has pthreads
     int shMemLatency;     // latency of shared memory (microsec)
     int shMemBandwidth;   // bandwidth of shared memory (MB/s)
     int distMemLatency;   // latency of distributed memory (microsec)
     int distMemBandwidth; // bandwidth of distributed memory (MB/s)

// !PUBLIC MEMBER FUNCTIONS:
//
   public:

 // the following method applies to a shallow class
    int ESMC_MachineInit(int nodes, int cpus, int cpuspernode,
                         bool hasmpi, bool hasopenmp, bool hasshmem,
                         int shmemlat, int shmemband,
                         int distmemlat, int distmemband);

 // optional configuration methods
//    int ESMC_MachineGetConfig(ESMC_MachineConfig *config) const;
//    int ESMC_MachineSetConfig(const ESMC_MachineConfig *config);

 // accessor methods for class members
      int ESMC_MachineGetNumNodes(int *nodes) const;
      int ESMC_MachineSetNumNodes(int nodes);
      int ESMC_MachineGetNumCPUs(int *cpu) const;
      int ESMC_MachineSetNumCPUs(int cpu);
      int ESMC_MachineGetNumCPUsperNode(int *cpuspernode) const;
      int ESMC_MachineSetNumCPUsperNode(int cpuspernode);

      bool ESMC_MachineHasMPI(void);
      bool ESMC_MachineHasOpenMP(void);
      bool ESMC_MachineHasShmem(void);
      bool ESMC_MachineHasPthreads(void);
      int ESMC_MachineSetHasMPI(bool hasit);
      int ESMC_MachineSetOpenMP(bool hasit);
      int ESMC_MachineSetShmem(bool hasit);
      int ESMC_MachineSetPthreads(bool hasit);

      int ESMC_MachineGetshMemLatency(int *lat) const;
      int ESMC_MachineSetshMemLatency(int lat);
      int ESMC_MachineGetshMemBandwidth(int *bandw) const;
      int ESMC_MachineSetshMemBandwidth(int bandw);
      int ESMC_MachineGetdistMemLatency(int *lat) const;
      int ESMC_MachineSetdistMemLatency(int lat);
      int ESMC_MachineGetdistMemBandwidth(int *bandw) const;
      int ESMC_MachineSetdistMemBandwidth(int bandw);
    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_MachineValidate(void) const;
    int ESMC_MachinePrint(void) const;

 // native C++ constructors/destructors
	ESMC_Machine(void);
	~ESMC_Machine(void);
  
 // < declare the rest of the public interface methods here >

  // get cpu id
  int ESMC_MachineGetCpuID(int *cpuid) const;

  // get node id
  int ESMC_MachineGetNodeID(int *nodeid) const;

  // get max cpus on a node
  int ESMC_MachineGetNodeCpuMax(int *maxcpus) const;
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_Machine

 #endif  // ESMC_Machine_H
