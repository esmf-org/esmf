// $Id: ESMC_Machine.C,v 1.7 2003/03/13 22:56:13 cdeluca Exp $
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
// The ESMF Machine class stores information about the architecture and
// operating environment of a platform in a generic way.  Only one 
// Machine is created per application.
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

 // associated class definition file
 #include <ESMC_Machine.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Machine.C,v 1.7 2003/03/13 22:56:13 cdeluca Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Machine routines
//
//

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
//      ESMF routine which only initializes Machine values; it does not
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
//    Returns the set of resources the Machine object was configured with.
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
//    Configures the Machine object with set of resources given.
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
//     Returns the value of Machine member <Value>.
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
//     Sets the Machine member <Value> with the given value.
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
//      Validates that a Machine is internally consistent.
//      Returns error code if problems are found.  ESMC\_Base class method.
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
//      Print information about a Machine.  The options control the
//      type of information and level of detail.  ESMC\_Base class method.
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
// !IROUTINE:  ESMC_MachineGetCpuID - get the machine-specific Cpu ID
//
// !INTERFACE:
      int ESMC_Machine::ESMC_MachineGetCpuID(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *cpuid) const {     // out - Cpu ID
//
// !DESCRIPTION:
//     Returns the Cpu ID of the calling process/thread
//
//EOP
// !REQUIREMENTS:  

#ifdef alpha
  long curr_cpu;
  int start = 0;

  // get cpu id we're on now (transient & relative, e.g. 0-3 on halem)
  getsysinfo(GSI_CURRENT_CPU, (char *)&curr_cpu, sizeof(long), &start);
//cout << "curr_cpu = " << curr_cpu << "\n";
  *cpuid = (int) curr_cpu;
#endif
 
  return(ESMF_SUCCESS);

 } // end ESMC_MachineGet<Value>

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

#ifdef alpha
  char hname[32];

  // get node id we're on (absolute, e.g. 0-347 on halem)
  gethostname(hname, 128);
//cout << "hostname = " << hname << "\n";
  *nodeid = atol(strpbrk(hname, "0123456789"));
#endif
 
  return(ESMF_SUCCESS);

 } // end ESMC_MachineGetNodeID

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_MachineGetNodeCpuMax - get the max Cpus per Node
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
