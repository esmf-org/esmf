// $Id: ESMC_CplComp.C,v 1.13.2.2 2009/01/21 21:25:24 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC Component method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt Component} methods declared
// in the companion file {\tt ESMC\_CplComp.h}.  They are wrappers for the
// actual code which is implemented in F90.
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
#include <string.h>
#include <stdio.h>
#include "ESMC_Start.h"

//-----------------------------------------------------------------------------
//BOP
// !CLASS: Component object
//
// !DESCRIPTION:
//  Component class which provides interfaces to the Fortran implementation
//    of Components.
//EOP
//-----------------------------------------------------------------------------

 // associated class definition file
#include "ESMC_Comp.h"
#include "ESMC_CplComp.h"

 // return min value 
#define min(a,b)  (((a)<(b))?(a):(b))

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
           "$Id: ESMC_CplComp.C,v 1.13.2.2 2009/01/21 21:25:24 cdeluca Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Component routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CplCompCreate - Create a new Component
//
// !INTERFACE:
      ESMC_CplComp *ESMC_CplCompCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_CplComp
//
// !ARGUMENTS:
      char *name,
      enum ESMC_GridCompType mtype,
      char *configFile,
      ESMC_Clock *clock,
      int *rc) {           // out - return code
//
// !DESCRIPTION:
//      C++ interface for creating a new {\tt Component}.  Calls into F90 
//      code to allocate the actual object.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in {\tt ESMC\_CplComp.h})
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

    ESMC_CplComp *comp;

    // the null is because we have no C++ interfaces to the config object yet
    // the null must be given in form of a variable in order to satisfy
    // fortran's pass by reference! *gjt*
    void *null = NULL;

    // Initialize return code; assume routine not implemented
    if (rc) *rc = ESMC_RC_NOT_IMPL;

    comp = new ESMC_CplComp;
    FTN(f_esmf_cplcompcreate)(comp, name, (ESMC_Config*)null, configFile, clock,
      rc, strlen(name), strlen(configFile));

    return comp;

 } // end ESMC_CplCompCreate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CplCompDestroy - free a Component created with Create
//
// !INTERFACE:
      int ESMC_CplCompDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_CplComp *component) {    // in - component object to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys a {\tt Component} object previously 
//      allocated via an {\tt ESMC\_CplCompCreate} routine. 
//
//      Note: this is a class helper function, not a class method
//      (see declaration in {\tt ESMC\_CplComp.h})
//
//EOP
// !REQUIREMENTS:  

    int rc;


    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    FTN(f_esmf_cplcompdestroy)(component, &rc);

    return rc;

 } // end ESMC_CplCompDestroy


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CplCompInitialize - call the Component init Routine
//
// !INTERFACE:
      int ESMC_CplComp::ESMC_CplCompInitialize(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_State *importState,   // in/out: required data
      ESMC_State *exportState,   // in/out: produced data
      ESMC_Clock *clock,         // in: model clock
      int phase,                 // in: if > 0, which phase of init to invoke
      ESMC_BlockingFlag blockingFlag) {  // in: run sync or async
//
// !DESCRIPTION:
//    Invokes the Initialize routine for an {\tt ESMC\_Component}.
//
//EOP

    int rc;

    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    FTN(f_esmf_cplcompinitialize)(this, importState, exportState, clock, 
                                   &phase, &blockingFlag, &rc);

    return rc;

 } // end ESMC_CplCompInitialize

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CplCompRun - call the Component run Routine
//
// !INTERFACE:
      int ESMC_CplComp::ESMC_CplCompRun(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_State *importState,   // in/out: required data
      ESMC_State *exportState,   // in/out: produced data
      ESMC_Clock *clock,         // in: model clock
      int phase,                 // in: if > 0, which phase of init to invoke
      ESMC_BlockingFlag blockingFlag) {  // in: run sync or async
//
// !DESCRIPTION:
//    Invokes the Run routine for an {\tt ESMC\_Component}.
//
//EOP

    int rc;

    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    FTN(f_esmf_cplcomprun)(this, importState, exportState, clock, &phase, 
                           &blockingFlag, &rc);

    return rc;

 } // end ESMC_CplCompRun

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CplCompFinalize - call the Component finalize Routine
//
// !INTERFACE:
      int ESMC_CplComp::ESMC_CplCompFinalize(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_State *importState,   // in/out: required data
      ESMC_State *exportState,   // in/out: produced data
      ESMC_Clock *clock,         // in: model clock
      int phase,                 // in: if > 0, which phase of init to invoke
      ESMC_BlockingFlag blockingFlag) {  // in: run sync or async
//
// !DESCRIPTION:
//    Invokes the Finalize routine for an {\tt ESMC\_Component}.
//
//EOP

    int rc;

    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    FTN(f_esmf_cplcompfinalize)(this, importState, exportState, clock, 
                                 &phase, &blockingFlag, &rc);

    return rc;

 } // end ESMC_CplCompFinalize

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CplCompGet - get info from a Component
//
// !INTERFACE:
      int ESMC_CplComp::ESMC_CplCompGet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      char *name) const {     // out -  etc - all add queries
//
// !DESCRIPTION:
//    Returns information about the Component object.
//
//EOP

    int rc;

    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    FTN(f_esmf_cplcompget)(this, &rc);

    return rc;

 } // end ESMC_CplCompGet

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CplCompSet - set info for a Component
//
// !INTERFACE:
      int ESMC_CplComp::ESMC_CplCompSet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *name) {     // in - what exactly is reasonable here?
//
// !DESCRIPTION:
//    Sets information associated with the Component object.
//
//EOP

    int rc;

    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    FTN(f_esmf_cplcompset)(this, &rc);

    return rc;

 } // end ESMC_CplCompSet


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CplCompValidate - internal consistency check for a Component
//
// !INTERFACE:
      int ESMC_CplComp::ESMC_CplCompValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a Component is internally consistent.
//      Returns error code if problems are found.  ESMC\_Base class method.
//
//EOP

    int rc;
    const ESMC_CplComp *comp;

    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;


    comp = this;
    FTN(f_esmf_cplcompvalidate)(comp, options, &rc, strlen(options));

    return rc;

 } // end ESMC_CplCompValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CplCompPrint - print contents of a Component
//
// !INTERFACE:
      int ESMC_CplComp::ESMC_CplCompPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a Component.  The options control the
//      type of information and level of detail.  ESMC\_Base class method.
//
//EOP

    int rc;
    const ESMC_CplComp *comp;

    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    comp = this;
    FTN(f_esmf_cplcompprint)(comp, options, &rc, strlen(options));

    return rc;

 } // end ESMC_CplCompPrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CplComp - native C++ constructor
//
// !INTERFACE:
      ESMC_CplComp::ESMC_CplComp(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      void) {
//
// !DESCRIPTION:
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  TODO: what code goes here?
//

 } // end ESMC_CplComp

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_CplComp - native C++ destructor
//
// !INTERFACE:
      ESMC_CplComp::~ESMC_CplComp(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//
//EOP

//
//  TODO: what code goes here?
//

 } // end ~ESMC_CplComp
