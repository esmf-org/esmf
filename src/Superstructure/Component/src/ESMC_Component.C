// $Id: ESMC_Component.C,v 1.4 2003/01/09 22:02:05 nscollins Exp $
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
// The code in this file implements the C++ Component methods declared
// in the companion file ESMC_Component.h
//
// < insert a paragraph or two explaining what you'll find in this file >
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
#include <string.h>
#include <stdio.h>
#include "ESMC.h"

 // associated class definition file
#include <ESMC_Component.h>

 // return min value 
#define min(a,b)  (((a)<(b))?(a):(b))

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
           "$Id: ESMC_Component.C,v 1.4 2003/01/09 22:02:05 nscollins Exp $";
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
// !ROUTINE:  ESMC_ComponentCreate - Create a new Component
//
// !INTERFACE:
      ESMC_Component *ESMC_ComponentCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_Component
//
// !ARGUMENTS:
      char *name,
      ESMC_Layout *layout,
      enum ESMC_CompType ctype,
      enum ESMC_ModelType mtype,
      char *filepath,
      int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Create a new Component.  Allocates memory for a new Component
//      object and uses the internal routine ESMC_ComponentContruct to
//      initialize it. 
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_Component.h)
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

    ESMC_Component *comp = new ESMC_Component;

    *rc = comp->ESMC_ComponentConstruct(name, layout, ctype, mtype, filepath);

    return comp;

 } // end ESMC_ComponentCreate

//-----------------------------------------------------------------------------
//BOP
// !ROUTINE:  ESMC_ComponentDestroy - free a Component created with Create
//
// !INTERFACE:
      int ESMC_ComponentDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Component *component) {    // in - component object to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys a Component object previously allocated
//      via an ESMC_ComponentCreate routine. 
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_Component.h)
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

    component->ESMC_ComponentDestruct();
    delete component;

    return ESMF_SUCCESS;
//
//  code goes here
//

 } // end ESMC_ComponentDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ComponentConstruct - fill in an already allocated Component
//
// !INTERFACE:
      int ESMC_Component::ESMC_ComponentConstruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      char *name,
      ESMC_Layout *layout,
      enum ESMC_CompType ctype,
      enum ESMC_ModelType mtype,
      char *filepath) {
//
// !DESCRIPTION:
//      ESMF routine which fills in the contents of an already
//      allocated Component object.  May need to do additional allocations
//      as needed.  Must call the corresponding ESMC_ComponentDestruct
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use ESMC_ComponentCreate, which calls
//      ESMC_ComponentConstruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//
    int len;

    len = min(strlen(name), ESMF_MAXSTR-1); 
    strncpy(this->compname, name, len);
   
    this->layout = *layout;
    this->ctype = ctype;
    this->mtype = mtype;

    len = min(strlen(filepath), ESMF_MAXSTR-1);
    strncpy(this->filepath, filepath, len);

    return ESMF_SUCCESS ;

 } // end ESMC_ComponentConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ComponentDestruct - release resources associated w/a Component
//
// !INTERFACE:
      int ESMC_Component::ESMC_ComponentDestruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMF_ComponentConstruct, does any additional cleanup before the
//      original Component object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC_ComponentDestroy, which calls
//      ESMC_ComponentDestruct. 
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

    // TODO: add any needed code here to free/release resources

    return ESMF_SUCCESS;

 } // end ESMC_ComponentDestruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ComponentInit - call the Component init Routine
//
// !INTERFACE:
      int ESMC_Component::ESMC_ComponentInit(
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
//  code goes here
//
    printf("ComponentInit method called \n");
    return ESMF_SUCCESS;

 } // end ESMC_ComponentInit

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ComponentRun - call the Component run routine
//
// !INTERFACE:
      int ESMC_Component::ESMC_ComponentRun(
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
//  code goes here
//
    printf("ComponentRun method called \n");
    return ESMF_SUCCESS;

 } // end ESMC_ComponentRun

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ComponentFinalize - call the Component finalize routine
//
// !INTERFACE:
      int ESMC_Component::ESMC_ComponentFinalize(
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
//  code goes here
//
    printf("ComponentFinalize method called \n");
    return ESMF_SUCCESS;

 } // end ESMC_ComponentFinalize

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ComponentGetConfig - get configuration info from a Component
//
// !INTERFACE:
      int ESMC_Component::ESMC_ComponentGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_ComponentConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the Component object was configured with.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_ComponentGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ComponentSetConfig - set configuration info for a Component
//
// !INTERFACE:
      int ESMC_Component::ESMC_ComponentSetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_ComponentConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the Component object with set of resources given.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_ComponentSetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ComponentGet<Value> - get <Value> for a Component
//
// !INTERFACE:
      //int ESMC_Component::ESMC_ComponentGet<Value>(
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

 //} // end ESMC_ComponentGet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ComponentSet<Value> - set <Value> for a Component
//
// !INTERFACE:
      //int ESMC_Component::ESMC_ComponentSet<Value>(
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

 //} // end ESMC_ComponentSet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ComponentValidate - internal consistency check for a Component
//
// !INTERFACE:
      int ESMC_Component::ESMC_ComponentValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a Component is internally consistent.
//      Returns error code if problems are found.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//

 } // end ESMC_ComponentValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ComponentPrint - print contents of a Component
//
// !INTERFACE:
      int ESMC_Component::ESMC_ComponentPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a Component.  The options control the
//      type of information and level of detail.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_ComponentPrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Component - native C++ constructor
//
// !INTERFACE:
      ESMC_Component::ESMC_Component(
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

 } // end ESMC_Component

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_Component - native C++ destructor
//
// !INTERFACE:
      ESMC_Component::~ESMC_Component(void) {
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

 } // end ~ESMC_Component
