// $Id: ESMC_Bundle.C,v 1.2 2003/03/11 03:00:50 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC Bundle method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ Bundle methods declared
// in the companion file ESMC_Bundle.h
//
// 
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
#include "ESMC.h"

 // associated class definition file
#include "ESMC_Bundle.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
             "$Id: ESMC_Bundle.C,v 1.2 2003/03/11 03:00:50 cdeluca Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Bundle routines
//
//

// non-method functions
ESMC_Bundle *ESMC_BundleCreate(int *rc) {
    ESMC_Bundle *f = new ESMC_Bundle;
    FTN(f_esmf_bundlecreate)(f, rc);
    return f;
} 

int ESMC_BundleDestroy(ESMC_Bundle *f) {
    int rc;
    FTN(f_esmf_bundledestroy)(f, &rc);
    delete f;
    f = 0;
    return rc;
}


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BundleConstruct - fill in an already allocated Bundle
//
// !INTERFACE:
      int ESMC_Bundle::ESMC_BundleConstruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !DESCRIPTION:
//      ESMF routine which fills in the contents of an already
//      allocated Bundle object.  May need to do additional allocations
//      as needed.  Must call the corresponding ESMC_BundleDestruct
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use ESMC_BundleCreate, which calls
//      ESMC_BundleConstruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//
    return ESMF_FAILURE;

 } // end ESMC_BundleConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BundleDestruct - release resources associated w/a Bundle
//
// !INTERFACE:
      int ESMC_Bundle::ESMC_BundleDestruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMF_BundleConstruct, does any additional cleanup before the
//      original Bundle object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC_BundleDestroy, which calls
//      ESMC_BundleDestruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//
    return ESMF_FAILURE;

 } // end ESMC_BundleDestruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BundleGetConfig - get configuration info from a Bundle
//
// !INTERFACE:
      //int ESMC_Bundle::ESMC_BundleGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      //ESMC_BundleConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the Bundle object was configured with.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

// } // end ESMC_BundleGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BundleSetConfig - set configuration info for a Bundle
//
// !INTERFACE:
  //    int ESMC_Bundle::ESMC_BundleSetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
   //   const ESMC_BundleConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the Bundle object with set of resources given.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

// } // end ESMC_BundleSetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BundleGet<Value> - get <Value> for a Bundle
//
// !INTERFACE:
 //      int ESMC_Bundle::ESMC_BundleGet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
   //    <value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of Bundle member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

  //} // end ESMC_BundleGet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BundleSet<Value> - set <Value> for a Bundle
//
// !INTERFACE:
    //   int ESMC_Bundle::ESMC_BundleSet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      // <value type> value) {     // in - value
//
// !DESCRIPTION:
//     Sets the Bundle member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 // } // end ESMC_BundleSet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BundleValidate - internal consistency check for a Bundle
//
// !INTERFACE:
      int ESMC_Bundle::ESMC_BundleValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a Bundle is internally consistent.
//      Returns error code if problems are found.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//
    return ESMF_FAILURE;

 } // end ESMC_BundleValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BundlePrint - print contents of a Bundle
//
// !INTERFACE:
      int ESMC_Bundle::ESMC_BundlePrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a Bundle.  The options control the
//      type of information and level of detail.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//
    return ESMF_FAILURE;

 } // end ESMC_BundlePrint

