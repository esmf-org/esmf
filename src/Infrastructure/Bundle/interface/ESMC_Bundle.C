// $Id: ESMC_Bundle.C,v 1.3.8.3 2007/10/18 02:42:22 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC Bundle method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt ESMC\_Bundle} methods declared
// in the companion file ESMC_Bundle.h
//
// 
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
#include "ESMC_Start.h"

 // associated class definition file
#include "ESMC_Bundle.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
             "$Id: ESMC_Bundle.C,v 1.3.8.3 2007/10/18 02:42:22 cdeluca Exp $";
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
//      allocated {\tt ESMC\_Bundle} object.  May need to do additional allocations
//      as needed.  Must call the corresponding {\tt ESMC\_ESMC_BundleDestruct}
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use {\tt ESMC\_ESMC_BundleCreate}, which calls
//      {\tt ESMC\_BundleConstruct}.  Define for deep classes only.
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
//      {\tt ESMF\_BundleConstruct}, does any additional cleanup before the
//      original {\tt ESMC\_Bundle} object is freed.  Intended for internal ESMF
//      use only; end-users use {\tt ESMC\_BundleDestroy}, which calls
//      {\tt ESMC\_ESMC_BundleDestruct}.  Define for deep classes only.
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
//     Returns the value of {\tt ESMC\_Bundle} member <Value>.
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
//     Sets the {\tt ESMC\_Bundle} member <Value> with the given value.
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
//      Validates that a {\tt ESMC\_Bundle} is internally consistent.
//      Returns error code if problems are found.  {\tt ESMC\_Base} class method.
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
//      Print information about a {\tt ESMC\_Bundle}.  The options control the
//      type of information and level of detail.  {\tt ESMC\_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//
    return ESMF_FAILURE;

 } // end ESMC_BundlePrint

