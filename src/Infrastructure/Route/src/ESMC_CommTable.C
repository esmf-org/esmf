// $Id: ESMC_CommTable.C,v 1.15 2003/04/25 22:26:05 nscollins Exp $
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
            "$Id: ESMC_CommTable.C,v 1.15 2003/04/25 22:26:05 nscollins Exp $";
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
    int i, *ip;

    myid = mydeid;
    commcount = partnercount;
    commpartner = new int[commcount];
    commneeded = new int[commcount];

    for(i=0; i<commcount; i++) 
        commneeded[i] = 0;

    switch(partnercount) {
      case 4:
        { int ids[4][4] = { { 1,2,3,0 },
                            { 0,3,2,1 }, 
                            { 3,0,1,2 },
                            { 2,1,0,3 } };

          for (i=0; i<partnercount; i++)
              commpartner[i] = ids[mydeid][i];
        }
        break;
      case 6:
        { int ids[6][6] = { { 1,2,3,4,5,0 },
                            { 0,4,5,3,2,1 }, 
                            { 3,0,4,5,1,2 },
                            { 2,5,0,1,4,3 },
                            { 5,1,2,0,3,4 },
                            { 4,3,1,2,0,5 } };

          for (i=0; i<partnercount; i++)
              commpartner[i] = ids[mydeid][i];
        }
        break;
      case 8:
        { int ids[8][8] = { { 1,2,3,4,5,6,7,0 },
                            { 0,7,6,5,4,3,2,1 }, 
                            { 7,0,5,6,3,4,1,2 },
                            { 6,5,0,7,2,1,4,3 },
                            { 5,6,7,0,1,2,3,4 },
                            { 4,3,2,1,0,7,6,5 },
                            { 3,4,1,2,7,0,5,6 },
                            { 2,1,4,3,6,5,0,7 } };

          for (i=0; i<partnercount; i++)
              commpartner[i] = ids[mydeid][i];
        }
        break;
      case 12:
        { int ids[12][12] = { {  1, 2, 3, 4, 5, 6, 7, 8, 9,10,11, 0 },
                              {  0,10,11, 9, 8, 7, 6, 5, 4, 3, 2, 1 }, 
                              {  8, 0, 6, 7,11, 5,10, 9, 3, 4, 1, 2 }, 
                              { 11, 7, 0, 8,10, 9, 5, 6, 2, 1, 4, 3 },
                              {  7, 8, 5, 0, 6,11, 9,10, 1, 2, 3, 4 },
                              {  9,11, 4,10, 0, 2, 3, 1, 8, 7, 6, 5 }, 
                              { 10, 9, 2,11, 4, 0, 1, 3, 7, 8, 5, 6 },
                              {  4, 3,10, 2, 9, 1, 0,11, 6, 5, 6, 7 },
                              {  2, 4, 9, 3, 1,10,11, 0, 5, 6, 7, 8 },
                              {  5, 6, 8, 1, 7, 3, 4, 2, 0,11,10, 9 },
                              {  6, 1, 7, 5, 3, 8, 2, 4,11, 0, 9,10 },
                              {  3, 5, 1, 6, 2, 4, 8, 7,10, 9, 0,11 } };

          for (i=0; i<partnercount; i++)
              commpartner[i] = ids[mydeid][i];
        }
        break;
      case 16:
        {
        int ids[16][16] = {{  1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15, 0},
                           {  0,15,14,13,12,11,10, 9, 8, 7, 6, 5, 4, 3, 2, 1},
                           { 15, 0,13,14,11,12, 9,10, 7, 8, 5, 6, 3, 4, 1, 2},
                           { 14,13, 0,15,10, 9,12,11, 6, 5, 8, 7, 2, 1, 4, 3},
                           { 13,14,15, 0, 9,10,11,12, 5, 6, 7, 8, 1, 2, 3, 4},
                           { 12,11,10, 9, 0,15,14,13, 4, 3, 2, 1, 8, 7, 6, 5},
                           { 11,12, 9,10,15, 0,13,14, 3, 4, 1, 2, 7, 8, 5, 6},
                           { 10, 9,12,11,14,13, 0,15, 2, 1, 4, 3, 6, 5, 8, 7},
                           {  9,10,11,12,13,14,15, 0, 1, 2, 3, 4, 5, 6, 7, 8},
                           {  8, 7, 6, 5, 4, 3, 2, 1, 0,15,14,13,12,11,10, 9},
                           {  7, 8, 5, 6, 3, 4, 1, 2,15, 0,13,14,11,12, 9,10},
                           {  6, 5, 8, 7, 2, 1, 4, 3,14,13, 0,15, 1, 9,12,11},
                           {  5, 6, 7, 8, 1, 2, 3, 4,13,14,15, 0, 9,10,11,12},
                           {  4, 3, 2, 1, 8, 7, 6, 5,12,11,10, 9, 0,15,14,13},
                           {  3, 4, 1, 2, 7, 8, 5, 6,11,12, 9,10,15, 0,13,14},
                           {  2, 1, 4, 3, 6, 5, 8, 7,10, 9,12,11,14,13, 0,15} };

          for (i=0; i<partnercount; i++)
              commpartner[i] = ids[mydeid][i];
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
// !IROUTINE:  ESMC_CommTableGetPartner - get partner list count
//
// !INTERFACE:
     int ESMC_CommTable::ESMC_CommTableGetPartner(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
     int entry,               // in - integer entry count
     int *partner,            // out - integer partner number
     int *needed) const {     // out - integer flag 0=no, 1=yes
//
// !DESCRIPTION:
//     Returns the nth communication partner, and flag saying if
//     communication is needed.
//
//EOP
// !REQUIREMENTS:  

    if (entry < 0 || entry >= commcount) {
        *partner = -1;
        *needed = 0;
        return ESMF_FAILURE;
    }
    *partner = commpartner[entry];
    *needed = commneeded[entry];
    
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
 
      int i;

      if (partner < 0 || partner >= commcount) {
          fprintf(stderr, "CommTable partner value out of range, %d not >= 0 and < %d\n",
                        partner, commcount);
          fprintf(stderr, "CommTable current values are:\n");
          this->ESMC_CommTablePrint("");
          return ESMF_FAILURE;
      }

      for (i=0; i<commcount; i++) {
          if (commpartner[i] == partner) {
              commneeded[i]++;
              return ESMF_SUCCESS;
          }
      }

      return ESMF_FAILURE;

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

    int i;

    printf(" myid=%d, commcount=%d\n", myid, commcount);
    for (i=0; i<commcount; i++) {
        printf(" %2d: partner=%2d, needed=%1d\n", 
                   i, commpartner[i], commneeded[i]);
    }

    return ESMF_SUCCESS;

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
