// $Id: ESMC_GridComp.C,v 1.16.2.2 2009/01/21 21:25:24 cdeluca Exp $
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
// in the companion file {\tt ESMC\_GridComp.h}.  They are wrappers for the
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
#include "ESMC_GridComp.h"

 // return min value 
#define min(a,b)  (((a)<(b))?(a):(b))

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
           "$Id: ESMC_GridComp.C,v 1.16.2.2 2009/01/21 21:25:24 cdeluca Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// This section includes all the Component routines
//
//
extern "C" { void ESMC_SetServ(ESMC_GridComp * const, void (*)(ESMC_GridComp *, int *), int *); }

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_GridCompSetServices - call the Component set services Routine
//
// !INTERFACE:
      int ESMC_GridComp::ESMC_GridCompSetServices(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
     void (*func)(ESMC_GridComp *, int *) ) {  // in: function which registers init/run/final
//
// !DESCRIPTION:
//    Invokes the Initialize routine for an {\tt ESMC\_Component}.
//
//EOP

    int rc;
 
    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    // this should go directly to C++ entry point
    ESMC_SetServ(this, func, &rc);

    return rc;

 } // end ESMC_GridCompSetServices

//----------------------------------------------------------------------------- 
//BOP
// !IROUTINE:  ESMC_GridCompSetEntryPoint - set pointers for GridComp Functions
//
// !INTERFACE:
      int ESMC_GridComp::ESMC_GridCompSetEntryPoint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *functionType,                          // in: function type
      void (*functionPtr)(ESMC_GridComp *, ESMC_State *,
                          ESMC_State *, ESMC_Clock *),   // in: function pointer
      int phase) {                                       // in: phase
//
// !DESCRIPTION:
//  Intended to be invoked by an {\tt ESMC\_GridComp} during the registration process.
//  An {\tt ESMC\_GridComp} invokes {\tt ESMC\_GridCompSetEntryPoint} for each of 
//  the predefined init, run, and finalize functions, to assocate the internal function
//  to be called for each function.  If multiple phases for init, run, or finalize
//  are needed, this can be called with phase numbers.
//
//  After this function returns, the framework now knows how to call
//  the initialize, run, and finalize functions for this child {\tt ESMC\_GridComp}.
//  The return code equals {\tt ESMC\_SUCCESS} if there are no errors.
//    
//  The arguments are:
//  \begin{description}
//   \item[functionType]
//    One of a set of predefined function types - e.g. {\tt ESMC\_SetInit}, 
//    {\tt ESMC\_SetRun}, {\tt ESMC\_SetFinal}.
//   \item[functionPtr]
//    The pointer to the {\tt gridcomp} function to be associated with the
//    {\tt functionType}.
//   \item[{[phase]}] 
//    For {\tt ESMC\_GridComp}s which need to initialize or run or finalize 
//    with mutiple phases, the phase number which corresponds to this function name.
//    For single phase function use the parameter {\tt ESMF\_SINGLEPHASE}.
//    The {\tt ESMC\_GridComp} writer must document the requirements of the
//    {\tt ESMC\_GridComp} for how and when the multiple phases are expected to be
//    called.
//  \end{description}
//
//EOP
// !REQUIREMENTS:  

    int rc;
 
    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    FTN(esmf_gridcompsetentrypoint)(this, (char *)functionType, (void *)functionPtr,
                                    &phase, &rc, strlen(functionType));

    return rc;

} // end ESMC_GridCompSetEntryPoint

//----------------------------------------------------------------------------- 
//BOP
// !IROUTINE:  ESMC_GridCompCreate - Create a new Component
//
// !INTERFACE:
      ESMC_GridComp *ESMC_GridCompCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_GridComp
//
// !ARGUMENTS:
      char *name,
      enum ESMC_GridCompType mtype,
      ESMC_Grid *grid,
      char *configFile,
      ESMC_Clock *clock,
      int *rc) {           // out - return code
//
// !DESCRIPTION:
//      C++ interface for creating a new {\tt Component}.  Calls into F90 
//      code to allocate the actual object.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in {\tt ESMC\_GridComp.h})
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

    ESMC_GridComp *comp;

    // the null is there because we have no C++ interface to config yet
    // the null must be given in form of a variable in order to satisfy
    // fortran's pass by reference! *gjt*
    void *null = NULL;
    comp = new ESMC_GridComp;

    // Initialize return code; assume routine not implemented
    if (rc) *rc = ESMC_RC_NOT_IMPL;

    FTN(f_esmf_gridcompcreate)(comp, name, &mtype, grid, (ESMC_Config*)null,
      configFile, clock, rc, strlen(name), strlen(configFile));

    return comp;

 } // end ESMC_GridCompCreate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_GridCompDestroy - free a Component created with Create
//
// !INTERFACE:
      int ESMC_GridCompDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_GridComp *component) {    // in - component object to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys a {\tt Component} object previously 
//      allocated via an {\tt ESMC\_GridCompCreate} routine. 
//
//      Note: this is a class helper function, not a class method
//      (see declaration in {\tt ESMC\_GridComp.h})
//
//EOP
// !REQUIREMENTS:  

    int rc;
 
    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    FTN(f_esmf_gridcompdestroy)(component, &rc);

    return rc;

 } // end ESMC_GridCompDestroy


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_GridCompInitialize - call the Component init Routine
//
// !INTERFACE:
      int ESMC_GridComp::ESMC_GridCompInitialize(
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

    FTN(f_esmf_gridcompinitialize)(this, importState, exportState, clock, 
                                   &phase, &blockingFlag, &rc);

    return rc;

 } // end ESMC_GridCompInitialize

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_GridCompRun - call the Component run Routine
//
// !INTERFACE:
      int ESMC_GridComp::ESMC_GridCompRun(
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

    FTN(f_esmf_gridcomprun)(this, importState, exportState, clock, 
                            &phase, &blockingFlag, &rc);

    return rc;

 } // end ESMC_GridCompRun

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_GridCompFinalize - call the Component finalize Routine
//
// !INTERFACE:
      int ESMC_GridComp::ESMC_GridCompFinalize(
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

    FTN(f_esmf_gridcompfinalize)(this, importState, exportState, clock, 
                                 &phase, &blockingFlag, &rc);

    return rc;

 } // end ESMC_GridCompFinalize

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_GridCompGet - get info from a Component
//
// !INTERFACE:
      int ESMC_GridComp::ESMC_GridCompGet(
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

    FTN(f_esmf_gridcompget)(this, &rc);

    return rc;

 } // end ESMC_GridCompGet

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_GridCompSet - set info for a Component
//
// !INTERFACE:
      int ESMC_GridComp::ESMC_GridCompSet(
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

    FTN(f_esmf_gridcompset)(this, &rc);

    return rc;

 } // end ESMC_GridCompSet


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_GridCompValidate - internal consistency check for a Component
//
// !INTERFACE:
      int ESMC_GridComp::ESMC_GridCompValidate(
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
 
    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    FTN(f_esmf_gridcompvalidate)(this, options, &rc, strlen(options));

    return rc;

 } // end ESMC_GridCompValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_GridCompPrint - print contents of a Component
//
// !INTERFACE:
      int ESMC_GridComp::ESMC_GridCompPrint(
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
 
    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    FTN(f_esmf_gridcompprint)(this, options, &rc, strlen(options));

    return rc;

 } // end ESMC_GridCompPrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_GridComp - native C++ constructor
//
// !INTERFACE:
      ESMC_GridComp::ESMC_GridComp(
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

 } // end ESMC_GridComp

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_GridComp - native C++ destructor
//
// !INTERFACE:
      ESMC_GridComp::~ESMC_GridComp(void) {
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

 } // end ~ESMC_GridComp
