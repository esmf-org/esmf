// $Id: ESMC_XPacket.C,v 1.4 2003/03/10 05:14:22 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC XPacket method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ XPacket methods declared
// in the companion file ESMC_XPacket.h
//
// < insert a paragraph or two explaining what you'll find in this file >
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <ESMC.h>

 // associated class definition file
 #include <ESMC_XPacket.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
              "$Id: ESMC_XPacket.C,v 1.4 2003/03/10 05:14:22 cdeluca Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the XPacket routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_XPacketCreate - Create a new XPacket
//
// !INTERFACE:
      ESMC_XPacket *ESMC_XPacketCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_XPacket
//
// !ARGUMENTS:
      int arg1,            // in
      int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Create a new XPacket from ... Allocates memory for a new XPacket
//      object and uses the internal routine ESMC_XPacketConstruct to
//      initialize it. 
//      There can be multiple overloaded methods with the same name, but
//      different argument lists.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_XPacket.h)
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

//
//  code goes here
//

    return new ESMC_XPacket;

 } // end ESMC_XPacketCreate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_XPacketDestroy - free a XPacket created with Create
//
// !INTERFACE:
      int ESMC_XPacketDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_XPacket *xpacket) {    // in - xpacket object to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys a XPacket object previously allocated
//      via an ESMC_XPacketCreate routine. 
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_XPacket.h)
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

    return ESMF_FAILURE;

 } // end ESMC_XPacketDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_XPacketConstruct - fill in an already allocated XPacket
//
// !INTERFACE:
      int ESMC_XPacket::ESMC_XPacketConstruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int arg1) {          // in
//
// !DESCRIPTION:
//      ESMF routine which fills in the contents of an already
//      allocated XPacket object.  May need to do additional allocations
//      as needed.  Must call the corresponding ESMC_XPacketDestruct
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use ESMC_XPacketCreate, which calls
//      ESMC_XPacketConstruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

    return ESMF_FAILURE;

 } // end ESMC_XPacketConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_XPacketDestruct - release resources associated w/a XPacket
//
// !INTERFACE:
      int ESMC_XPacket::ESMC_XPacketDestruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMF_XPacketConstruct, does any additional cleanup before the
//      original XPacket object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC_XPacketDestroy, which calls
//      ESMC_XPacketDestruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

    return ESMF_FAILURE;

 } // end ESMC_XPacketDestruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_XPacketGetConfig - get configuration info from a XPacket
//
// !INTERFACE:
      int ESMC_XPacket::ESMC_XPacketGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_XPacketConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the XPacket object was configured with.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

    return ESMF_FAILURE;

 } // end ESMC_XPacketGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_XPacketSetConfig - set configuration info for a XPacket
//
// !INTERFACE:
      int ESMC_XPacket::ESMC_XPacketSetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_XPacketConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the XPacket object with set of resources given.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

    return ESMF_FAILURE;

 } // end ESMC_XPacketSetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_XPacketGet<Value> - get <Value> for a XPacket
//
// !INTERFACE:
      //int ESMC_XPacket::ESMC_XPacketGet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      //<value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of XPacket member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

    //return ESMF_FAILURE;

 //} // end ESMC_XPacketGet

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_XPacketSet - set <Value> for a XPacket
//
// !INTERFACE:
      //int ESMC_XPacket::ESMC_XPacketSet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      //<value type> value) {     // in - value
//
// !DESCRIPTION:
//     Sets the XPacket member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

    //return ESMF_FAILURE;

 //} // end ESMC_XPacketSet

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_XPacketValidate - internal consistency check for a XPacket
//
// !INTERFACE:
      int ESMC_XPacket::ESMC_XPacketValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a XPacket is internally consistent.
//      Returns error code if problems are found.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//

    return ESMF_FAILURE;

 } // end ESMC_XPacketValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_XPacketPrint - print contents of a XPacket
//
// !INTERFACE:
      int ESMC_XPacket::ESMC_XPacketPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a XPacket.  The options control the
//      type of information and level of detail.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

    return ESMF_FAILURE;

 } // end ESMC_XPacketPrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_XPacket - native C++ constructor
//
// !INTERFACE:
      ESMC_XPacket::ESMC_XPacket(
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

 } // end ESMC_XPacket

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_XPacket - native C++ destructor
//
// !INTERFACE:
      ESMC_XPacket::~ESMC_XPacket(void) {
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

 } // end ~ESMC_XPacket
