// $Id: ESMC_PE.C,v 1.8 2003/04/04 21:08:06 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC PE method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ PE methods declared
// in the companion file ESMC_PE.h
//
// 
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
#include <iostream.h>
//#include <iostream> // TODO: use when namespaces consistently implemented
//using std::cout;
#include <ESMC.h>

 // associated class definition file
 #include <ESMC_PE.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_PE.C,v 1.8 2003/04/04 21:08:06 nscollins Exp $";
//-----------------------------------------------------------------------------

// initialize peCount (class static)
int ESMC_PE::peCount = 0;
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the PE routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PEInit - initializes a PE object
//
// !INTERFACE:
      int ESMC_PE::ESMC_PEInit(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Machine *mach) {         // in - handle to machine model
//
// !DESCRIPTION:
//      ESMF routine which only initializes PE values; it does not
//      allocate any resources.
//
//EOP
// !REQUIREMENTS:  

  int esmfid, cpuid, nodeid;

  // save machine model handle
  machine = mach;

  // assign unique ESMF id - number of PE's instantiated
  esmfid = ++peCount;

  // get machine-specific IDs
  machine->ESMC_MachineGetCpuID(&cpuid);
  machine->ESMC_MachineGetNodeID(&nodeid);
  
  ESMC_PEInit(esmfid, cpuid, nodeid);

  return(ESMF_SUCCESS);

 } // end ESMC_PEInit

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PEInit - initializes a PE object
//
// !INTERFACE:
      int ESMC_PE::ESMC_PEInit(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int esmfid,           // in
      int cpuid,            // in
      int nodeid) {         // in
//
// !DESCRIPTION:
//      ESMF routine which only initializes PE values; it does not
//      allocate any resources.
//
//EOP
// !REQUIREMENTS:  

   esmfID = esmfid;
   cpuID  = cpuid;
   nodeID = nodeid;

   return(ESMF_SUCCESS);

 } // end ESMC_PEInit

#if 0
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PEGetConfig - get configuration info from a PE
//
// !INTERFACE:
      int ESMC_PE::ESMC_PEGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_PEConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the PE object was configured with.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_PEGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PESetConfig - set configuration info for a PE
//
// !INTERFACE:
      int ESMC_PE::ESMC_PESetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_PEConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the PE object with set of resources given.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_PESetConfig
#endif

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PEGetEsmfID - get esmfID for a PE
//
// !INTERFACE:
      int ESMC_PE::ESMC_PEGetEsmfID(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *esmfid) const {     // out - esmfid
//
// !DESCRIPTION:
//     Returns the value of PE member esmfID
//
//EOP
// !REQUIREMENTS: 

   *esmfid = esmfID;

   return(ESMF_SUCCESS);

 } // end ESMC_PEGetEsmfID

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PESetEsmfID - set esmfID for a PE
//
// !INTERFACE:
      int ESMC_PE::ESMC_PESetEsmfID(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int esmfid) {     // in - value
//
// !DESCRIPTION:
//     Sets the PE member esmfID with the given value.
//
//EOP
// !REQUIREMENTS:  

  esmfID = esmfid;

  return(ESMF_SUCCESS);

 } // end ESMC_PESetEsmfID

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PEGetCpuID - get cpuID for a PE
//
// !INTERFACE:
      int ESMC_PE::ESMC_PEGetCpuID(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *cpuid) const {     // out - cpuid
//
// !DESCRIPTION:
//     Returns the value of PE member cpuID
//
//EOP
// !REQUIREMENTS: 

   *cpuid = cpuID;

   return(ESMF_SUCCESS);

 } // end ESMC_PEGetCpuID

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PESetCpuID - set cpuID for a PE
//
// !INTERFACE:
      int ESMC_PE::ESMC_PESetCpuID(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int cpuid) {     // in - value
//
// !DESCRIPTION:
//     Sets the PE member cpuID with the given value.
//
//EOP
// !REQUIREMENTS:  

  cpuID = cpuid;

  return(ESMF_SUCCESS);

 } // end ESMC_PESetCpuID

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PEGetNodeID - get nodeID for a PE
//
// !INTERFACE:
      int ESMC_PE::ESMC_PEGetNodeID(
//
// !RETURN VALUE:
//    long error return code
//
// !ARGUMENTS:
      int *nodeid) const {     // out - nodeid
//
// !DESCRIPTION:
//     Returns the value of PE member nodeID
//
//EOP
// !REQUIREMENTS: 

   *nodeid = nodeID;

   return(ESMF_SUCCESS);

 } // end ESMC_PEGetNodeID

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PESetNodeID - set nodeID for a PE
//
// !INTERFACE:
      int ESMC_PE::ESMC_PESetNodeID(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int nodeid) {     // in - value
//
// !DESCRIPTION:
//     Sets the PE member nodeID with the given value.
//
//EOP
// !REQUIREMENTS:  

  nodeID = nodeid;

  return(ESMF_SUCCESS);

 } // end ESMC_PESetNodeID

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PEValidate - internal consistency check for a PE
//
// !INTERFACE:
      int ESMC_PE::ESMC_PEValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a PE is internally consistent.
//      Returns error code if problems are found.  ESMC\_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

  return(ESMF_SUCCESS);

 } // end ESMC_PEValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PEPrint - print contents of a PE
//
// !INTERFACE:
      int ESMC_PE::ESMC_PEPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a PE.  The options control the
//      type of information and level of detail.  ESMC\_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

       cout << "esmfID=" << esmfID 
            << ", cpuID="  << cpuID
            << ", nodeID=" << nodeID << "\n";

  return(ESMF_SUCCESS);

 } // end ESMC_PEPrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PE - native C++ constructor
//
// !INTERFACE:
      ESMC_PE::ESMC_PE(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      void) {
//
// !DESCRIPTION:
//      Calls standard ESMF deep or shallow methods for initialization
//      with default or passed-in values
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  ESMC_PEInit(0,0,0);

 } // end ESMC_PE

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_PE - native C++ destructor
//
// !INTERFACE:
      ESMC_PE::~ESMC_PE(void) {
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

 } // end ~ESMC_PE
