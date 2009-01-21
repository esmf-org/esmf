// $Id: ESMC_CommTable.C,v 1.31.2.2 2009/01/21 21:25:23 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC CommTable method implementation (body) file
#define ESMF_FILENAME "ESMC_CommTable.C"

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
 #include <ESMC_Start.h>
 #include <stdio.h>
 #include <stdlib.h>
 #include <string.h>

 // associated class definition file
 #include <ESMC_CommTable.h>
 #include <ESMC_LogErr.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
            "$Id: ESMC_CommTable.C,v 1.31.2.2 2009/01/21 21:25:23 cdeluca Exp $";
//-----------------------------------------------------------------------------

//

//-----------------------------------------------------------------------------
//
// This section includes all the CommTable routines
//
//

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_CommTableCreate"
//BOPI
// !IROUTINE:  ESMC_CommTableCreate - Create a new CommTable
//
// !INTERFACE:
      ESMC_CommTable *ESMC_CommTableCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_CommTable
//
// !ARGUMENTS:
      int myvmid,               // in
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
//EOPI
// !REQUIREMENTS: 

    ESMC_CommTable *newc = new ESMC_CommTable;

    *rc = newc->ESMC_CommTableConstruct(myvmid, partnercount);

    return newc;

 } // end ESMC_CommTableCreate

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_CommTableDestroy"
//BOPI
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
//EOPI
// !REQUIREMENTS:  

     // Initialize return code
     int status=ESMC_RC_NOT_IMPL;

    return status;

 } // end ESMC_CommTableDestroy

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_CommTableConstruct"
//BOPI
// !IROUTINE:  ESMC_CommTableConstruct - fill in an already allocated CommTable
//
// !INTERFACE:
      int ESMC_CommTable::ESMC_CommTableConstruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int myvmid,
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
//EOPI
// !REQUIREMENTS:  
    int i, *ip, rc;

    // Initialize return code
    rc = ESMC_RC_NOT_IMPL;


    myid = myvmid;
    decount = partnercount;
    commcount = partnercount;
    commpartner = new int[commcount];
    commneeded = new int[commcount];

    switch(partnercount) {
      case 1:
        commpartner[0] = 0;
        break;
      case 2:
        { int ids[2][2] = { { 1,0 },
                            { 0,1 } };

          for (i=0; i<partnercount; i++)
              commpartner[i] = ids[myvmid][i];
        }
        break;
      case 4:
        { int ids[4][4] = { { 1,2,3,0 },
                            { 0,3,2,1 }, 
                            { 3,0,1,2 },
                            { 2,1,0,3 } };

          for (i=0; i<partnercount; i++)
              commpartner[i] = ids[myvmid][i];
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
              commpartner[i] = ids[myvmid][i];
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
              commpartner[i] = ids[myvmid][i];
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
                              {  4, 3,10, 2, 9, 1, 0,11, 6, 5, 8, 7 },
                              {  2, 4, 9, 3, 1,10,11, 0, 5, 6, 7, 8 },
                              {  5, 6, 8, 1, 7, 3, 4, 2, 0,11,10, 9 },
                              {  6, 1, 7, 5, 3, 8, 2, 4,11, 0, 9,10 },
                              {  3, 5, 1, 6, 2, 4, 8, 7,10, 9, 0,11 } };

          for (i=0; i<partnercount; i++)
              commpartner[i] = ids[myvmid][i];
        }
        break;
      default:
        rc = ESMC_CommTableFill();
        if (rc != ESMF_SUCCESS)
            return rc;
        break;
    }

    for(i=0; i<commcount; i++) 
        commneeded[i] = 0;

    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_CommTableConstruct

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_CommTableDestruct"
//BOPI
// !IROUTINE:  ESMC_CommTableDestruct - release resources associated w/a CommTable
//
// !INTERFACE:
      int ESMC_CommTable::ESMC_CommTableDestruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) {
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMF_CommTableConstruct, does any additional cleanup before the
//      original CommTable object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC_CommTableDestroy, which calls
//      ESMC_CommTableDestruct.  Define for deep classes only.
//
//EOPI
// !REQUIREMENTS:  

    delete [] commpartner;
    delete [] commneeded;

    return ESMF_SUCCESS;

 } // end ESMC_CommTableDestruct


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_CommTableGetCount"
//BOPI
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
//EOPI
// !REQUIREMENTS:  

    // Initialize return code
    int rc = ESMC_RC_NOT_IMPL;

    *count = commcount;
    
    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_CommTableGetCount

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_CommTableGetPartner"
//BOPI
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
//EOPI
// !REQUIREMENTS:  

    // Initialize return code
    int rc = ESMC_RC_NOT_IMPL;

    if (entry < 0 || entry >= commcount) {
        *partner = -1;
        *needed = 0;
        rc = ESMF_SUCCESS;
        return rc;
    } 

    *partner = commpartner[entry];
    *needed = commneeded[entry];
    
    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_CommTableGetCount

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_CommTableSetPartner"
//BOPI
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
//EOPI
// !REQUIREMENTS:  
 
      int i, rc;
      char msgbuf[ESMF_MAXSTR];

      // Initialize return code
      rc = ESMC_RC_NOT_IMPL;

      if (partner < 0 || partner >= commcount) {
          sprintf(msgbuf, "CommTable partner value out of range, %d not >= 0 and < %d\n",
                        partner, commcount);
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &rc);

          //fprintf(stderr, "CommTable current values are:\n");
          //this->ESMC_CommTablePrint("");
          return(rc);  
      }

      for (i=0; i<commcount; i++) {
          if (commpartner[i] == partner) {
              commneeded[i]++;
              rc = ESMF_SUCCESS;
              return rc;
          }
      }

      rc = ESMF_FAILURE;
      return rc;

 } // end ESMC_CommTableSetPartner

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_CommTableValidate"
//BOPI
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
//EOPI
// !REQUIREMENTS:  XXXn.n, YYYn.n
//
    // Initialize return code
    int rc = ESMC_RC_NOT_IMPL;

    return rc;
 } // end ESMC_CommTableValidate


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_CommTablePrint"
//BOPI
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
//EOPI
// !REQUIREMENTS:  SSSn.n, GGGn.n

    int i;
    char msgbuf[ESMF_MAXSTR];
    bool brief;
    int rc = ESMC_RC_NOT_IMPL;

    brief = strcmp(options, "brief") ? false : true;

    sprintf(msgbuf, " myid=%d, decount=%d, commcount=%d\n", myid, decount, commcount);
    printf(msgbuf);
    //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
    for (i=0; i<commcount; i++) {
        if ((commneeded[i] > 0) && !brief) {
            sprintf(msgbuf, " %2d: partner=%2d, needed=%1d\n", 
                             i, commpartner[i], commneeded[i]);
            //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
            printf(msgbuf);
        }
    }

    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_CommTablePrint

//-----------------------------------------------------------------------------

static void 
fill(int max, int size, int xpos, int ypos, int base, int *results)
{
    int nsize;

    if (size > 2) {
        nsize = size / 2;
        fill(max, nsize, xpos, ypos, base, results);
        fill(max, nsize, xpos+nsize, ypos, base+nsize, results);
        fill(max, nsize, xpos, ypos+nsize, base+nsize, results);
        fill(max, nsize, xpos+nsize, ypos+nsize, base, results);
    } else {
        results[xpos * max + ypos] = base;
        results[(xpos+1) * max + ypos] = base+1;
        results[xpos * max + (ypos+1)] = base+1;
        results[(xpos+1) * max + (ypos+1)] = base;
    }

    return;
}

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_CommTableFill"
//BOPI
// !IROUTINE:  ESMC_CommTableFill - private routine for computing comm patterns
//
// !INTERFACE:
    int ESMC_CommTable::ESMC_CommTableFill(
//
// !RETURN VALUE:
//    integer return code
//
// !ARGUMENTS:
         void) {
//
// !DESCRIPTION:
//   Generate the order for the communication patterns for each DE in
//    a group.
//
//EOPI
    // initialize return code
    int rc = ESMC_RC_NOT_IMPL;

    int i, npow2;
    int *results;

    npow2 = 1;
    while (npow2 < decount)
        npow2 *= 2;

    // update base object now to this pow of 2
    commcount = npow2;
    delete [] commpartner;
    commpartner = new int[npow2];
    delete [] commneeded;
    commneeded = new int[npow2];

    // this is the table for all procs
    results = new int[npow2*npow2];

    // call the recursive routine to fill the table
    fill(npow2, npow2, 0, 0, 0, results);

    // copy appropriate line back for this vmid
    for (i=0; i<npow2; i++) {
        if (results[myid * npow2 + i] < decount)
            commpartner[i] = results[myid * npow2 + i];
        else
            commpartner[i] = -1;
    }

    delete [] results;

    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_CommTableFill

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_CommTable()"
//BOPI
// !IROUTINE:  ESMC_CommTable - native C++ constructor
//
// !INTERFACE:
      ESMC_CommTable::ESMC_CommTable(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      void) {
//
// !DESCRIPTION:
//
//EOPI
// !REQUIREMENTS:

    decount = 0;
    commcount = 0;
    commpartner = NULL;
    commneeded = NULL;

 } // end ESMC_CommTable

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "~ESMC_CommTable()"
//BOPI
// !IROUTINE:  ~ESMC_CommTable - native C++ destructor
//
// !INTERFACE:
      ESMC_CommTable::~ESMC_CommTable(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      void) {
//
// !DESCRIPTION:
//      Calls standard ESMF deep or shallow methods for destruction
//
//EOPI
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ~ESMC_CommTable
