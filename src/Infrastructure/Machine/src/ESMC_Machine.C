// $Id: ESMC_Machine.C,v 1.5 2003/10/16 22:35:26 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC Machine method code (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ Machine methods declared
// in the companion file ESMC_Machine.h
//
// The ESMF {\tt ESMC\_Machine} class stores information about the architecture and
// operating environment of a platform in a generic way.  Only one 
// {\tt ESMC\_Machine} is created per application.
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here

#ifdef alpha
#include <unistd.h>               // gethostname
#include <sys/sysinfo.h>          // getsysinfo
#include <machine/hal_sysinfo.h>  // getsysinfo
#include <stdlib.h>               // atoi
#include <string.h>               // strpbrk
#endif

 #include <iostream.h>
//#include <iostream> // TODO: use when namespaces consistently implemented
//using std::cout;
 #include <ESMC.h>
 #include <mpi.h>     // TODO: temp - use MPI rank to id procs, NOT RIGHT

 // associated class definition file
 #include <ESMC_Machine.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
                 "$Id: ESMC_Machine.C,v 1.5 2003/10/16 22:35:26 nscollins Exp $";
//-----------------------------------------------------------------------------

//
//  Single public machine object, available to be queried by anyone.
ESMC_Machine Machine;

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Machine routines
//
//
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_MachineInitialize
//
// !INTERFACE:
      int ESMC_MachineInitialize(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
      ESMC_MainLanguage language) {    // was main in F90 or C++?
//
// !DESCRIPTION:
//     Should query {\tt ESMC\_Machine} for configuration, characteristics.
//     TODO: this is hardcoded now and needs to be fixed.
//
//EOP
// !REQUIREMENTS:  

    // initialize machine to defaults
    //  TODO: query actual hardware - don't send in hardcoded values
    return Machine.ESMC_MachineInit(256, 1024, 4, true, true, true, 
                                     1, 200, 2, 100);


 } // end ESMC_MachineInitialize


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_MachineFinalize
//
// !INTERFACE:
      int ESMC_MachineFinalize(void) {
//
// !RETURN VALUE:
//    int error return code
//
//
// !DESCRIPTION:
//     Place to shut down any needed resources.
//
//EOP
// !REQUIREMENTS:  

    //  TODO: shut down any necessary resources

    int finalized;

    MPI_Finalized(&finalized);
    if (!finalized) 
      MPI_Finalize();


    return ESMF_SUCCESS;

 } // end ESMC_MachineFinalize


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_MachineInit - initializes a Machine object
//
// !INTERFACE:
      int ESMC_Machine::ESMC_MachineInit(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int nodes,          // in - number of nodes in machine or domain
      int cpus,           // in - number of cpus in machine or domain
      int cpuspernode,    // in - number of cpus per node in machine
      bool hasmpi,        // in - machine supports MPI 1.2
      bool hasopenmp,     // in - machine supports OpenMP 1.0
      bool hasshmem,      // in - machine supports Shmem
      int shmemlat,       // in - shared memory latency
      int shmemband,      // in - shared memory bandwidth
      int distmemlat,     // in - distributed memory latency
      int distmemband) {  // in - distributed memory bandwidth
      
//
// !DESCRIPTION:
//      ESMF routine which only initializes {\tt ESMC\_Machine} values; it does not
//      allocate any resources. 
//
//EOP
// !REQUIREMENTS:  

  numNodes = nodes;
  numCPUs = cpus;
  numCPUsperNode = cpuspernode;
  hasMPI = hasmpi;
  hasOpenMP = hasopenmp;
  hasShmem = hasshmem;
  shMemLatency = shmemlat;
  shMemBandwidth = shmemband;
  distMemLatency = distmemlat;
  distMemBandwidth = distmemband;

  // TODO: currently the MPI rank is used as the cpu number, and the
  //  max rank overrides the numCPUs.  
  ESMC_MachineSetCpuID();
  ESMC_MachineSetNodeID();

  return(ESMF_SUCCESS);

 } // end ESMC_MachineInit

#if 0
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_MachineGetConfig - get configuration info from a Machine
//
// !INTERFACE:
      int ESMC_Machine::ESMC_MachineGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_MachineConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the {\tt ESMC\_Machine} object was configured with.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_MachineGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_MachineSetConfig - set configuration info for a Machine
//
// !INTERFACE:
      int ESMC_Machine::ESMC_MachineSetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_MachineConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the {\tt ESMC\_Machine} object with set of resources given.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_MachineSetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_MachineGet<Value> - get <Value> for a Machine
//
// !INTERFACE:
      int ESMC_Machine::ESMC_MachineGet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of {\tt ESMC\_Machine} member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_MachineGet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_MachineSet<Value> - set <Value> for a Machine
//
// !INTERFACE:
      int ESMC_Machine::ESMC_MachineSet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> value) {     // in - value
//
// !DESCRIPTION:
//     Sets the {\tt ESMC\_Machine} member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_MachineSet<Value>
#endif

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_MachineValidate - internal consistency check for a Machine
//
// !INTERFACE:
      int ESMC_Machine::ESMC_MachineValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a {\tt ESMC\_Machine} is internally consistent.
//      Returns error code if problems are found.  {\tt ESMC\_Base} class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

  return(ESMF_SUCCESS);

 } // end ESMC_MachineValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_MachinePrint - print contents of a Machine
//
// !INTERFACE:
      int ESMC_Machine::ESMC_MachinePrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a {\tt ESMC\_Machine}.  The options control the
//      type of information and level of detail.  {\tt ESMC\_Base} class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  return(ESMF_SUCCESS);

 } // end ESMC_MachinePrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Machine - native C++ constructor
//
// !INTERFACE:
      ESMC_Machine::ESMC_Machine(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      void) {  // in
//
// !DESCRIPTION:
//      Calls standard ESMF deep or shallow methods for initialization
//      with default or passed-in values
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_Machine

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_Machine - native C++ destructor
//
// !INTERFACE:
      ESMC_Machine::~ESMC_Machine(void) {
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

//
//  code goes here
//

 } // end ~ESMC_Machine

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_MachineSetCpuID - get the machine-specific CPU ID
//
// !INTERFACE:
      int ESMC_Machine::ESMC_MachineSetCpuID(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !DESCRIPTION:
//     Discover the CPU ID of the calling process/thread.  For non-alpha
//     platforms, this is using the MPI rank which is not the same thing,
//     but will have to do for now.  TODO: fix this.
//
//EOP
// !REQUIREMENTS:  

    int initialized, numProcs, numArgs;
    // TODO: mpich requires these be passed into init...
    //extern int argc;
    //extern char **argv;

#ifdef alpha
    long curr_cpu;
    int start = 0;

    // get cpu id we're on now (transient & relative, e.g. 0-3 on halem)
    getsysinfo(GSI_CURRENT_CPU, (char *)&curr_cpu, sizeof(long), &start);
    //cout << "curr_cpu = " << curr_cpu << "\n";
    CpuID = (int) curr_cpu;
#endif
 

    MPI_Initialized(&initialized);
    if (!initialized) {
      numArgs = 0;
      MPI_Init(&numArgs, NULL);
      //MPI_Init(&argc, &argv);
    } else {
      // log error?
    }

    // TODO: MPI overrides given nProcs ?

    // TODO: this used to call with &numProcs.  for right now since we
    //  cannot support virtual DEs per PE, set the max to numCPUs.
    MPI_Comm_size(MPI_COMM_WORLD, &numCPUs);

    // get my unique DE process group ID
    MPI_Comm_rank(MPI_COMM_WORLD, &CpuID);  

    return(ESMF_SUCCESS);

 } // end ESMC_MachineSetCpuID

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_MachineGetCpuID - get the machine-specific CPU ID
//
// !INTERFACE:
      int ESMC_Machine::ESMC_MachineGetCpuID(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *cpuid) const {     // out - CPU ID
//
// !DESCRIPTION:
//     Returns the CPU ID of the calling process/thread
//
//EOP
// !REQUIREMENTS:  

    *cpuid = CpuID;

    return(ESMF_SUCCESS);

 } // end ESMC_MachineGetCpuID

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_MachineSetNodeID - Set the machine-specific Node ID
//
// !INTERFACE:
      int ESMC_Machine::ESMC_MachineSetNodeID(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !DESCRIPTION:
//     Sets the Node ID of the calling process/thread
//
//EOP
// !REQUIREMENTS:  

#ifdef alpha
  char hname[32];

  // get node id we're on (absolute, e.g. 0-347 on halem)
  gethostname(hname, 128);
//cout << "hostname = " << hname << "\n";
  NodeID = atol(strpbrk(hname, "0123456789"));
#endif
 
  // TODO:  HACK, HACK, HACK.  This should really query the node
  //  but for now use MPI rank, which is how the cpu id is currently
  //  being hacked around.
  NodeID = CpuID;

  return(ESMF_SUCCESS);

 } // end ESMC_MachineGetNodeID

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_MachineGetNodeID - get the machine-specific Node ID
//
// !INTERFACE:
      int ESMC_Machine::ESMC_MachineGetNodeID(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *nodeid) const {     // out - Node ID
//
// !DESCRIPTION:
//     Returns the Node ID of the calling process/thread
//
//EOP
// !REQUIREMENTS:  

    *nodeid = NodeID;
 
    return(ESMF_SUCCESS);

 } // end ESMC_MachineGetNodeID

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_MachineGetNodeCpuMax - get the max CPUs per Node
//
// !INTERFACE:
      int ESMC_Machine::ESMC_MachineGetNodeCpuMax(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *maxcpus) const {     // out - max cpus per node
//
// !DESCRIPTION:
//     Returns the maximum number of cpus per node
//
//EOP
// !REQUIREMENTS:  

#ifdef alpha
  int max_cpu;
  int start = 0;

  // get max cpus on a node
  getsysinfo(GSI_MAX_CPU, (char *)&max_cpu, sizeof(int), &start);
//cout << "max_cpu = " << max_cpu << "\n";
  *maxcpus = max_cpu;
#endif
 
  return(ESMF_SUCCESS);

 } // end ESMC_MachineGetNodeCpuMax
