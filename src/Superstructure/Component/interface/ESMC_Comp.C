// $Id: ESMC_Comp.C,v 1.8 2003/03/10 03:23:12 cdeluca Exp $
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
// in the companion file {\tt ESMC\_Comp.h}.  They are wrappers for the
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

 // return min value 
#define min(a,b)  (((a)<(b))?(a):(b))

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
           "$Id: ESMC_Comp.C,v 1.8 2003/03/10 03:23:12 cdeluca Exp $";
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
// !IROUTINE:  ESMC_CompCreate - Create a new Component
//
// !INTERFACE:
      ESMC_Comp *ESMC_CompCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_Comp
//
// !ARGUMENTS:
      char *name,
      ESMC_DELayout *layout,
      enum ESMC_CompType ctype,
      enum ESMC_ModelType mtype,
      char *filepath,
      int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Create a new Component.  Allocates memory for a new Component
//      object and uses the internal routine ESMC\_CompContruct to
//      initialize it. 
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC\_Comp.h)
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

    ESMC_Comp *comp;

    // TODO: make this match the correct calling convention

    //f_esmf_compcreate(&comp, name, layout, ctype, mtype, filepath, rc)

    return comp;

 } // end ESMC_CompCreate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CompDestroy - free a Component created with Create
//
// !INTERFACE:
      int ESMC_CompDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Comp *component) {    // in - component object to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys a Component object previously allocated
//      via an ESMC\_CompCreate routine. 
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC\_Comp.h)
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

    int rc;

    // TODO: fix the calling conventions

    //f_esmf_compdestroy(comp, &rc)

    return rc;

 } // end ESMC_CompDestroy


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CompInit - call the Component init Routine
//
// !INTERFACE:
      int ESMC_Comp::ESMC_CompInit(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) {
//
// !DESCRIPTION:
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  TODO: add call to f_esmf_compinit() here
//
    printf("ComponentInit method called \n");
    return ESMF_FAILURE;

 } // end ESMC_CompInit

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CompRun - call the Component run routine
//
// !INTERFACE:
      int ESMC_Comp::ESMC_CompRun(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int timesteps) {
//
// !DESCRIPTION:
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  TODO: add call to f_esmf_comprun() here
//
    printf("ComponentRun method called \n");
    return ESMF_FAILURE;

 } // end ESMC_CompRun

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CompFinal - call the Component finalize routine
//
// !INTERFACE:
      int ESMC_Comp::ESMC_CompFinal(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) {
//
// !DESCRIPTION:
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  TODO: add call to f_esmf_compfinal() here
//
    printf("ComponentFinal method called \n");
    return ESMF_FAILURE;

 } // end ESMC_CompFinal

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CompGetConfig - get configuration info from a Component
//
// !INTERFACE:
      int ESMC_Comp::ESMC_CompGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_CompConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the Component object was configured with.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//
    return ESMF_FAILURE;

 } // end ESMC_CompGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CompSetConfig - set configuration info for a Component
//
// !INTERFACE:
      int ESMC_Comp::ESMC_CompSetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_CompConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the Component object with set of resources given.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//
    return ESMF_FAILURE;

 } // end ESMC_CompSetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CompGet<Value> - get <Value> for a Component
//
// !INTERFACE:
      //int ESMC_Comp::ESMC_CompGet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      //<value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of Component member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 //} // end ESMC_CompGet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CompSet<Value> - set <Value> for a Component
//
// !INTERFACE:
      //int ESMC_Comp::ESMC_CompSet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      //<value type> value) {     // in - value
//
// !DESCRIPTION:
//     Sets the Component member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 //} // end ESMC_CompSet<Value>


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CompValidate - internal consistency check for a Component
//
// !INTERFACE:
      int ESMC_Comp::ESMC_CompValidate(
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
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//
    return ESMF_FAILURE;

 } // end ESMC_CompValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CompPrint - print contents of a Component
//
// !INTERFACE:
      int ESMC_Comp::ESMC_CompPrint(
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
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//
    return ESMF_FAILURE;

 } // end ESMC_CompPrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Comp - native C++ constructor
//
// !INTERFACE:
      ESMC_Comp::ESMC_Comp(
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

//
//  code goes here
//

 } // end ESMC_Comp

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_Comp - native C++ destructor
//
// !INTERFACE:
      ESMC_Comp::~ESMC_Comp(void) {
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

 } // end ~ESMC_Comp
