// $Id: ESMC_GridComp.C,v 1.6 2004/04/23 17:25:17 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

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
#include "ESMC.h"

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
           "$Id: ESMC_GridComp.C,v 1.6 2004/04/23 17:25:17 theurich Exp $";
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

    // this should go directly to C++ entry point
    ESMC_SetServ(this, func, &rc);

    return rc;

 } // end ESMC_GridCompSetServices

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

    comp = new ESMC_GridComp;
    FTN(f_esmf_gridcompcreate)(comp, name, &mtype, grid, NULL, 
                         configFile, rc, strlen(name), strlen(configFile));

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
      int phase)  {              // in: if > 0, which phase of init to invoke
//
// !DESCRIPTION:
//    Invokes the Initialize routine for an {\tt ESMC\_Component}.
//
//EOP

    int rc;

    FTN(f_esmf_gridcompinitialize)(this, importState, exportState, clock, 
                                   &phase, &rc);

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
      int phase)  {              // in: if > 0, which phase of run to invoke
//
// !DESCRIPTION:
//    Invokes the Run routine for an {\tt ESMC\_Component}.
//
//EOP

    int rc;

    FTN(f_esmf_gridcomprun)(this, importState, exportState, clock, &phase, &rc);

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
      int phase)  {              // in: if > 0, which phase of finalize to invoke
//
// !DESCRIPTION:
//    Invokes the Finalize routine for an {\tt ESMC\_Component}.
//
//EOP

    int rc;

    FTN(f_esmf_gridcompfinalize)(this, importState, exportState, clock, 
                                 &phase, &rc);

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
