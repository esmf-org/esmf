// $Id: ESMC_CommTable.C,v 1.8 2003/03/14 15:26:05 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC CommTable method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ CommTable methods declared
// in the companion file ESMC_CommTable.h
//
// 
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <ESMC.h>
 #include <stdio.h>
 #include <stdlib.h>

 // associated class definition file
 #include <ESMC_CommTable.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
            "$Id: ESMC_CommTable.C,v 1.8 2003/03/14 15:26:05 nscollins Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the CommTable routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommTableCreate - Create a new CommTable
//
// !INTERFACE:
      ESMC_CommTable *ESMC_CommTableCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_CommTable
//
// !ARGUMENTS:
      int mydeid,               // in
      int partnercount,         // in
      int *rc) {                // out - return code
//
// !DESCRIPTION:
//      Create a new CommTable.  Allocates memory for a new CommTable
//      object and uses the internal routine ESMC_CommTableConstruct to
//      initialize it. 
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_CommTable.h)
//
//EOP
// !REQUIREMENTS: 

    ESMC_CommTable *newc = new ESMC_CommTable;

    *rc = newc->ESMC_CommTableConstruct(mydeid, partnercount);

    return newc;

 } // end ESMC_CommTableCreate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommTableDestroy - free a CommTable created with Create
//
// !INTERFACE:
      int ESMC_CommTableDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_CommTable *commtable) {    // in - commtable object to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys a CommTable object previously allocated
//      via an ESMC_CommTableCreate routine.  Define for deep classes only.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_CommTable.h)
//
//EOP
// !REQUIREMENTS:  

    return ESMF_FAILURE;

 } // end ESMC_CommTableDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommTableConstruct - fill in an already allocated CommTable
//
// !INTERFACE:
      int ESMC_CommTable::ESMC_CommTableConstruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int mydeid,
      int partnercount) {
//
// !DESCRIPTION:
//      ESMF routine which fills in the contents of an already
//      allocated CommTable object.  May need to do additional allocations
//      as needed.  Must call the corresponding ESMC_CommTableDestruct
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use ESMC_CommTableCreate, which calls
//      ESMC_CommTableConstruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  
    int i;

    myid = mydeid;
    commcount = partnercount;
    commpartner = new int[commcount];
    commneeded = new int[commcount];

    for(i=0; i<commcount; i++) 
        commneeded[i] = 0;

    switch(partnercount) {
      case 4:
        switch(mydeid) {
          case 0: commpartner[0] = 1; commpartner[1] = 2; commpartner[2] = 3; 
                  commpartner[3] = mydeid;  break;
          case 1: commpartner[0] = 0; commpartner[1] = 3; commpartner[2] = 2; 
                  commpartner[3] = mydeid;  break;
          case 2: commpartner[0] = 3; commpartner[1] = 0; commpartner[2] = 1; 
                  commpartner[3] = mydeid;  break;
          case 3: commpartner[0] = 2; commpartner[1] = 1; commpartner[2] = 0; 
                  commpartner[3] = mydeid;  break;
        }
        break;
      default:
        printf("no code for PE count of %d\n", partnercount);
        break;
    }

    return ESMF_SUCCESS;

 } // end ESMC_CommTableConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommTableDestruct - release resources associated w/a CommTable
//
// !INTERFACE:
      int ESMC_CommTable::ESMC_CommTableDestruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMF_CommTableConstruct, does any additional cleanup before the
//      original CommTable object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC_CommTableDestroy, which calls
//      ESMC_CommTableDestruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  


    return ESMF_FAILURE;

 } // end ESMC_CommTableDestruct


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommTableGetCount - get partner list count
//
// !INTERFACE:
     int ESMC_CommTable::ESMC_CommTableGetCount(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
     int *count) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of CommTable member count.
//
//EOP
// !REQUIREMENTS:  

    *count = commcount;
    
    return ESMF_SUCCESS;

 } // end ESMC_CommTableGetCount

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommTableSetPartner - set processor id to communicate with
//
// !INTERFACE:
    int ESMC_CommTable::ESMC_CommTableSetPartner(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
    int partner) {     // in - value
//
// !DESCRIPTION:
//     Sets the CommTable member <Value> with the given value.
//
//EOP
// !REQUIREMENTS:  

      if (partner < 0 || partner >= commcount) {
          fprintf(stderr, "value out of range, %d not >= 0 and < %d\n",
                        partner, commcount);
          return ESMF_FAILURE;
      }

      commneeded[partner]++;
      return ESMF_SUCCESS;

 } // end ESMC_CommTableSetPartner

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommTableValidate - internal consistency check for a CommTable
//
// !INTERFACE:
      int ESMC_CommTable::ESMC_CommTableValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a CommTable is internally consistent.
//      Returns error code if problems are found.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n
//

    return ESMF_FAILURE;

 } // end ESMC_CommTableValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommTablePrint - print contents of a CommTable
//
// !INTERFACE:
      int ESMC_CommTable::ESMC_CommTablePrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a CommTable.  The options control the
//      type of information and level of detail.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

    return ESMF_FAILURE;

 } // end ESMC_CommTablePrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CommTable - native C++ constructor
//
// !INTERFACE:
      ESMC_CommTable::ESMC_CommTable(void) {
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
// !REQUIREMENTS:

    commcount = 0;
    commpartner = NULL;
    commneeded = NULL;

 } // end ESMC_CommTable

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_CommTable - native C++ destructor
//
// !INTERFACE:
      ESMC_CommTable::~ESMC_CommTable(void) {
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

 } // end ~ESMC_CommTable
