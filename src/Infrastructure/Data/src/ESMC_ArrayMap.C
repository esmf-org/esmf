// $Id: ESMC_ArrayMap.C,v 1.2 2002/11/05 17:46:15 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC ArrayMap method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ ArrayMap methods declared
// in the companion file ESMC_ArrayMap.h
//
// < insert a paragraph or two explaining what you'll find in this file >
//
//-----------------------------------------------------------------------------
//

// associated class definition file
#include <ESMC_ArrayMap.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
       "$Id: ESMC_ArrayMap.C,v 1.2 2002/11/05 17:46:15 nscollins Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the ArrayMap routines
//
//


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArrayMapInit - initializes a ArrayMap object
//
// !INTERFACE:
      int ESMC_ArrayMap::ESMC_ArrayMapInit(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *rc) {  // in
//
// !DESCRIPTION:
//      ESMF routine which only initializes ArrayMap values; it does not
//      allocate any resources.  Define for shallow classes only,
//      for deep classes define and use routines Create/Destroy and
//      Construct/Destruct.  Can be overloaded like ESMC_ArrayMapCreate.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_ArrayMapInit

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArrayMapGetConfig - get configuration info from a ArrayMap
//
// !INTERFACE:
      int ESMC_ArrayMap::ESMC_ArrayMapGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_ArrayMapConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the ArrayMap object was configured with.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_ArrayMapGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArrayMapSetConfig - set configuration info for a ArrayMap
//
// !INTERFACE:
      int ESMC_ArrayMap::ESMC_ArrayMapSetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_ArrayMapConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the ArrayMap object with set of resources given.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_ArrayMapSetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArrayMapGet<Value> - get <Value> for a ArrayMap
//
// !INTERFACE:
      //int ESMC_ArrayMap::ESMC_ArrayMapGet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      //<value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of ArrayMap member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 //} // end ESMC_ArrayMapGet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArrayMapSet<Value> - set <Value> for a ArrayMap
//
// !INTERFACE:
      //int ESMC_ArrayMap::ESMC_ArrayMapSet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      //<value type> value) {     // in - value
//
// !DESCRIPTION:
//     Sets the ArrayMap member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 //} // end ESMC_ArrayMapSet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArrayMapValidate - internal consistency check for a ArrayMap
//
// !INTERFACE:
      int ESMC_ArrayMap::ESMC_ArrayMapValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a ArrayMap is internally consistent.
//      Returns error code if problems are found.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//

 } // end ESMC_ArrayMapValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArrayMapPrint - print contents of a ArrayMap
//
// !INTERFACE:
      int ESMC_ArrayMap::ESMC_ArrayMapPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a ArrayMap.  The options control the
//      type of information and level of detail.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_ArrayMapPrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArrayMap - native C++ constructor
//
// !INTERFACE:
      ESMC_ArrayMap::ESMC_ArrayMap(
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

 } // end ESMC_ArrayMap

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_ArrayMap - native C++ destructor
//
// !INTERFACE:
      ESMC_ArrayMap::~ESMC_ArrayMap(void) {
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

 } // end ~ESMC_ArrayMap
