// $Id: ESMC_DE.C,v 1.1 2002/12/06 19:27:24 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC DE method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ DE methods declared
// in the companion file ESMC_DE.h
//
// < insert a paragraph or two explaining what you'll find in this file >
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <iostream>
 #include <ESMC.h>

 // associated class definition file
 #include <ESMC_DE.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_DE.C,v 1.1 2002/12/06 19:27:24 eschwab Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the DE routines
//
//
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DEInit - initializes a DE object
//
// !INTERFACE:
      int ESMC_DE::ESMC_DEInit(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int esmfid,                  // in - ESMF ID
      int pid,                     // in - platform process ID
      int tid,                     // in - platform thread ID
      bool proc,                   // in - true if DE is a process
      bool thrd,                   // in - true if DE is a thread
      ESMC_PE *pe) {               // in - assigned PE from peList
//
// !DESCRIPTION:
//      ESMF routine which only initializes DE values; it does not
//      allocate any resources.  Define for shallow classes only,
//      for deep classes define and use routines Create/Destroy and
//      Construct/Destruct.  Can be overloaded like ESMC_DECreate.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

  esmfID = esmfid;
  pID = pid;
  tID = tid;
  process = proc;
  thread = thrd;
  PE = pe;

  return(ESMF_SUCCESS);
  
 } // end ESMC_DEInit

#if 0
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DEGetConfig - get configuration info from a DE
//
// !INTERFACE:
      int ESMC_DE::ESMC_DEGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_DEConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the DE object was configured with.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_DEGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DESetConfig - set configuration info for a DE
//
// !INTERFACE:
      int ESMC_DE::ESMC_DESetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_DEConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the DE object with set of resources given.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_DESetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DEGet<Value> - get <Value> for a DE
//
// !INTERFACE:
      int ESMC_DE::ESMC_DEGet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of DE member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_DEGet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DESet<Value> - set <Value> for a DE
//
// !INTERFACE:
      int ESMC_DE::ESMC_DESet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> value) {     // in - value
//
// !DESCRIPTION:
//     Sets the DE member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_DESet<Value>
#endif

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DESetESMFID - set ESMF ID for this DE
//
// !INTERFACE:
      int ESMC_DE::ESMC_DESetESMFID(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int esmfid) {     // in - ESMF ID
//
// !DESCRIPTION:
//     Sets the ESMF ID for this DE to given value.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

  esmfID = esmfid;

  return(ESMF_SUCCESS);

 } // end ESMC_DESet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DESetPE - assign PE to a DE
//
// !INTERFACE:
      int ESMC_DE::ESMC_DESetPE(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_PE *pe) {     // in - PE
//
// !DESCRIPTION:
//     Sets the DE member PE with the given pe value.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

  PE = pe;

  return(ESMF_SUCCESS);

 } // end ESMC_DESet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DEGetESMFID - get current ESMF DE ID
//
// !INTERFACE:
      int ESMC_DE::ESMC_DEGetESMFID(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *esmfid) const {     // out - ESMF DE ID
//
// !DESCRIPTION:
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

  *esmfid = esmfID;

  return(ESMF_SUCCESS);

 } // end ESMC_DEGetESMFID

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DEGetpID - get DE's platform process ID

//
// !INTERFACE:
      int ESMC_DE::ESMC_DEGetpID(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *pid) const {     // out - platform process id
//
// !DESCRIPTION:
//     Get's the DE's platform-specific process ID (e.g. MPI rank)
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

  *pid = pID;

  return(ESMF_SUCCESS);

 } // end ESMC_DESetpID

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DEGetDEType - get DE's type (process or thread)

//
// !INTERFACE:
      int ESMC_DE::ESMC_DEGetType(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_DEType_e *detype) const {     // out - DE type
//
// !DESCRIPTION:
//     Get's the DE's type: process or thread
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

  *detype = deType;

  return(ESMF_SUCCESS);

 } // end ESMC_DEGetType

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DEValidate - internal consistency check for a DE
//
// !INTERFACE:
      int ESMC_DE::ESMC_DEValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a DE is internally consistent.
//      Returns error code if problems are found.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

  return(ESMF_SUCCESS);

 } // end ESMC_DEValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DEPrint - print contents of a DE
//
// !INTERFACE:
      int ESMC_DE::ESMC_DEPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a DE.  The options control the
//      type of information and level of detail.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  std::cout << "esmfID = " << esmfID << std::endl;
  std::cout << "pID = " << pID << std::endl;
  std::cout << "tID = " << tID << std::endl;
  std::cout << "process = " << process << std::endl;
  std::cout << "thread = " << thread << std::endl;

  std::cout << "PE = ";
  if (PE != 0) {
    PE->ESMC_PEPrint();
  } else {
    std::cout << "unassigned" << std::endl;
  }

  return(ESMF_SUCCESS);

 } // end ESMC_DEPrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DE - native C++ constructor
//
// !INTERFACE:
      ESMC_DE::ESMC_DE(
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

  ESMC_DEInit(0,0,0, false,false, 0);

 } // end ESMC_DE

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DE - native C++ constructor
//
// !INTERFACE:
      ESMC_DE::ESMC_DE(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      ESMC_DEType_e detype) {  // in
//
// !DESCRIPTION:
//      Calls standard ESMF deep or shallow methods for initialization
//      with default or passed-in values
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  deType = detype;
  ESMC_DEInit(0,0,0, false,false, 0);

 } // end ESMC_DE

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_DE - native C++ destructor
//
// !INTERFACE:
      ESMC_DE::~ESMC_DE(void) {
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

 } // end ~ESMC_DE
