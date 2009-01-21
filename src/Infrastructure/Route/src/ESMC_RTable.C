// $Id: ESMC_RTable.C,v 1.32.2.2 2009/01/21 21:25:23 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC RTable method implementation (body) file
#define ESMF_FILENAME "ESMC_RTable.C"

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ RTable methods declared
// in the companion file ESMC_RTable.h
//
// 
//
//-----------------------------------------------------------------------------
//
// associated class definition file
#include "ESMC_RTable.h"

// higher level, 3rd party or system headers
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// ESMF headers
#include "ESMC_Start.h"
#include "ESMC_LogErr.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
            "$Id: ESMC_RTable.C,v 1.32.2.2 2009/01/21 21:25:23 cdeluca Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
int compare(const void* item1, const void* item2) {
  return (((ESMC_XPacket*)item1)->ESMC_XPacketGetIndex()
	  - ((ESMC_XPacket*)item2)->ESMC_XPacketGetIndex());
}
//-----------------------------------------------------------------------------
//
// This section includes all the RTable routines
//
//

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RTableCreate"
//BOP
// !IROUTINE:  ESMC_RTableCreate - Create a new RTable
//
// !INTERFACE:
      ESMC_RTable *ESMC_RTableCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_RTable
//
// !ARGUMENTS:
      int myde,
      int decount, 
      int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Create a new RTable from ... Allocates memory for a new RTable
//      object and uses the internal routine ESMC_RTableConstruct to
//      initialize it.  Define for deep classes only, for shallow classes only
//      define and use ESMC_RTableInit.
//      There can be multiple overloaded methods with the same name, but
//      different argument lists.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_RTable.h)
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

    ESMC_RTable *newrt = new ESMC_RTable(decount);

    *rc = newrt->ESMC_RTableConstruct(myde, decount);

    return newrt;

 } // end ESMC_RTableCreate

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RTableDestroy"
//BOP
// !IROUTINE:  ESMC_RTableDestroy - free a RTable created with Create
//
// !INTERFACE:
      int ESMC_RTableDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_RTable *rtable) {    // in - rtable object to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys a RTable object previously allocated
//      via an ESMC_RTableCreate routine.  Define for deep classes only.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC_RTable.h)
//
//EOP
// !REQUIREMENTS:  

    return ESMC_RC_NOT_IMPL;

 } // end ESMC_RTableDestroy

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RTableConstruct"
//BOP
// !IROUTINE:  ESMC_RTableConstruct - fill in an already allocated RTable
//
// !INTERFACE:
      int ESMC_RTable::ESMC_RTableConstruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int myde,
      int decount) { 
// 
// !DESCRIPTION: 
//      ESMF routine which fills in the contents of an already
//      allocated RTable object.  May need to do additional allocations
//      as needed.  Must call the corresponding ESMC_RTableDestruct
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use ESMC_RTableCreate, which calls
//      ESMC_RTableConstruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  

    int i;

    entrycount = decount;
    my_vmid = myde;
    entry = new struct rtableentry[decount];

    // TODO: this is a fully instantiated table, one slot per possible
    // destination processor.  if this table size gets too large, it can
    // be reimplmented as a linked list of entries only for those procs
    // which are going to be doing communication.  this is simplest to get
    // implemented to get feedback about how well it works.

    for (i=0; i<entrycount; i++) {
        entry[i].vmid = i;
        entry[i].xpcount = 0;
        entry[i].xp = ESMC_NULL_POINTER;
        entry[i].alloccount = 0;
    }

    return ESMF_SUCCESS;

 } // end ESMC_RTableConstruct

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RTableDestruct"
//BOP
// !IROUTINE:  ESMC_RTableDestruct - release resources associated w/a RTable
//
// !INTERFACE:
      int ESMC_RTable::ESMC_RTableDestruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) {
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMF_RTableConstruct, does any additional cleanup before the
//      original RTable object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC_RTableDestroy, which calls
//      ESMC_RTableDestruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  
    int i;

    for (i=0; i<entrycount; i++) 
        free(entry[i].xp);
    
    delete [] entry;

    return ESMF_SUCCESS;

 } // end ESMC_RTableDestruct


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RTableGetValue"
//BOP
// !IROUTINE:  ESMC_RTableGet<Value> - get <Value> for a RTable
//
// !INTERFACE:
    //  int ESMC_RTable::ESMC_RTableGet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
    //  <value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of RTable member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

// } // end ESMC_RTableGet<Value>

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RTableSetEntry"
//BOP
// !IROUTINE:  ESMC_RTableSetEntry - set entry for a RTable
//
// !INTERFACE:
      int ESMC_RTable::ESMC_RTableSetEntry(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
       int nvmid,          // in  -
       ESMC_XPacket *xp) { // in -
//
// !DESCRIPTION:
//     Sets an RTable entry with the given value.
//     Can be multiple routines, one per value
//
//EOP
    int rc;

// TODO  SetEntry will append an xp to the current table.
//       Do we want to add the ability to replace an entry?

    entry[nvmid].vmid = nvmid;
    entry[nvmid].xpcount++;

//  If not enough space allocated, allocate more.
//  If entry[nvmid].xp is NULL, then this will simply allocate
    if(entry[nvmid].alloccount < entry[nvmid].xpcount) {
      entry[nvmid].alloccount += ALLOCCHUNK;
      entry[nvmid].xp = (ESMC_XPacket *)realloc(entry[nvmid].xp, 
                                          entry[nvmid].alloccount * sizeof(ESMC_XPacket));
      if (entry[nvmid].xp == NULL) {
	!printf("Not enough memory to add more XPackets!?\n");
        ESMC_LogDefault.ESMC_LogAllocError(&rc);
        return(rc);
      }
    }

    // This is a contents copy; when this routine returns
    // the caller is free to reuse the packet.
    entry[nvmid].xp[ entry[nvmid].xpcount - 1 ] = *xp;

    return ESMF_SUCCESS;

 } // end ESMC_RTableSetEntry

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RTableGetEntry"
//BOP
// !IROUTINE:  ESMC_RTableGetEntry - Get entry from an RTable
//
// !INTERFACE:
      int ESMC_RTable::ESMC_RTableGetEntry(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
       int nvmid,             // in  -
       int xpcount,           // in
       ESMC_XPacket **xp) {   // out
//
// !DESCRIPTION:
//     Gets an RTable entry with the given value.
//
//EOP

    int rc;
    char msgbuf[ESMF_MAXSTR];

    if (nvmid < 0 || nvmid > entrycount) {
        sprintf(msgbuf, "nvmid out of range, %d must be between 0 and %d\n", 
		                                	nvmid, entrycount);
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &rc);

        return(rc);
    }

    *xp = &(entry[nvmid].xp[ xpcount ]);

    return ESMF_SUCCESS;

 } // end ESMC_RTableGetEntry

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RTableGetCount"
//BOP
// !IROUTINE:  ESMC_RTableGetCount - Get number of XPackets from RTable
//
// !INTERFACE:
      int ESMC_RTable::ESMC_RTableGetCount(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
       int nvmid,             // in  -
       int *xpcount) {        // out
//
// !DESCRIPTION:
//     Gets the RTable xpcount
//
//EOP

    int rc;
    char msgbuf[ESMF_MAXSTR];

    if (nvmid < 0 || nvmid > entrycount) {
        sprintf(msgbuf, "nvmid out of range, %d must be between 0 and %d\n", 
		                                	nvmid, entrycount);
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &rc);

        return(rc);
    }

    *xpcount = entry[nvmid].xpcount;

    return ESMF_SUCCESS;

 } // end ESMC_RTableGetCount

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RTableGetCount"
//BOP
// !IROUTINE:  ESMC_RTableGetTotalCount - Get number of all XPackets from RTable
//
// !INTERFACE:
      int ESMC_RTable::ESMC_RTableGetTotalCount(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
       int *xpcount) {        // out
//
// !DESCRIPTION:
//     Gets the count of all xpackets in the entire RTable
//
//EOP

    int i, rc;
    int count;
    char msgbuf[ESMF_MAXSTR];

    if (xpcount == NULL) {
        sprintf(msgbuf, "bad xpcount argument\n");
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &rc);

        return(rc);
    }

    count = 0;
    for (i=0; i<entrycount; i++)
       count += entry[i].xpcount;

    *xpcount = count;
    return ESMF_SUCCESS;

 } // end ESMC_RTableGetTotalCount

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RTableSetValue"
//BOP
// !IROUTINE:  ESMC_RTableSet<Value> - set <Value> for a RTable
//
// !INTERFACE:
 //     int ESMC_RTable::ESMC_RTableSet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
  //    <value type> value) {     // in - value
//
// !DESCRIPTION:
//     Sets the RTable member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

// } // end ESMC_RTableSet<Value>

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RTableValidate"
//BOP
// !IROUTINE:  ESMC_RTableValidate - internal consistency check for a RTable
//
// !INTERFACE:
      int ESMC_RTable::ESMC_RTableValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a RTable is internally consistent.
//      Returns error code if problems are found.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    return ESMC_RC_NOT_IMPL;

 } // end ESMC_RTableValidate

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RTableSort"
//BOP
// !IROUTINE:  ESMC_RTableSort - sort the XPackets in each table according to
//               its block_index value
//
// !INTERFACE:
      int ESMC_RTable::ESMC_RTableSort(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ) {    // in - none
//
// !DESCRIPTION:
//      Sort the XPackets in each table according to its block_index value
//      Returns error code if problems are found.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  

    int i, j;
    ESMC_XPacket *xptbl;

    for (i = 0; i < entrycount; i++) {
      xptbl = entry[i].xp;
      qsort(xptbl, entry[i].xpcount, sizeof(ESMC_XPacket), compare);
    }
    return ESMF_SUCCESS;

 } // end ESMC_RTableSort


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RTablePrint"
//BOP
// !IROUTINE:  ESMC_RTablePrint - print contents of a RTable
//
// !INTERFACE:
      int ESMC_RTable::ESMC_RTablePrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a RTable.  The options control the
//      type of information and level of detail.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

    int i, j,ixp;
    ESMC_XPacket *xptr;
    char msgbuf[ESMF_MAXSTR];
    bool brief;
    
    brief = strcmp(options,"brief") ? false : true;


    if ((entrycount == 0) && brief) return ESMF_SUCCESS;

    sprintf(msgbuf, " entrycount=%d, my_pet=%d\n", entrycount, my_vmid);
    //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
    printf(msgbuf);
      
    for(i=0; i<entrycount; i++) {
        // do not print out 0 entries
        if (entry[i].xpcount == 0) continue;
        sprintf(msgbuf, "%2d: pet=%2d, xpcount=%2d, xpaddr=0x%08lx\n",
                 i, entry[i].vmid, entry[i].xpcount, (ESMC_I8)(entry[i].xp)); 
        //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
        printf(msgbuf);
        for (j=0, xptr=&(entry[i].xp[0]); j<entry[i].xpcount; j++, xptr++)
            xptr->ESMC_XPacketPrint(3, options);
    }
  
    return ESMF_SUCCESS;

 } // end ESMC_RTablePrint

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RTable()"
//BOP
// !IROUTINE:  ESMC_RTable - native C++ constructor
//
// !INTERFACE:
      ESMC_RTable::ESMC_RTable(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      int decount) {
//
// !DESCRIPTION:
//
//EOP
// !REQUIREMENTS:

   // allocate space for full table here.

 } // end ESMC_RTable

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "~ESMC_RTable()"
//BOP
// !IROUTINE:  ~ESMC_RTable - native C++ destructor
//
// !INTERFACE:
      ESMC_RTable::~ESMC_RTable(
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
// !REQUIREMENTS: 

   // default destructor ok

 } // end ~ESMC_RTable
