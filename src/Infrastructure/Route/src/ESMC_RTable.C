// $Id: ESMC_RTable.C,v 1.18 2004/04/23 21:59:32 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC RTable method implementation (body) file

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
 // insert any higher level, 3rd party or system includes here
 #include "ESMC_Start.h"
 #include <stdio.h>
 #include <stdlib.h>

 // associated class definition file
 #include "ESMC_RTable.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_RTable.C,v 1.18 2004/04/23 21:59:32 nscollins Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the RTable routines
//
//

//-----------------------------------------------------------------------------
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

    return ESMF_FAILURE;

 } // end ESMC_RTableDestroy

//-----------------------------------------------------------------------------
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
    my_deid = myde;
    entry = new struct rtableentry[decount];

    // TODO: this is a fully instantiated table, one slot per possible
    // destination processor.  if this table size gets too large, it can
    // be reimplmented as a linked list of entries only for those procs
    // which are going to be doing communication.  this is simplest to get
    // implemented to get feedback about how well it works.

    for (i=0; i<entrycount; i++) {
        entry[i].deid = i;
        entry[i].xpcount = 0;
        entry[i].xp = ESMC_NULL_POINTER;
        entry[i].alloccount = 0;
    }

    return ESMF_SUCCESS;

 } // end ESMC_RTableConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_RTableDestruct - release resources associated w/a RTable
//
// !INTERFACE:
      int ESMC_RTable::ESMC_RTableDestruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
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
       int ndeid,          // in  -
       ESMC_XPacket *xp) { // in -
//
// !DESCRIPTION:
//     Sets an RTable entry with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  

//    if (entry[ndeid].xpcount > 0) {
//        printf("already an entry for deid %d\n", ndeid);
//        printf("need to implement multiple xp's per src/dst pair\n"); 
//        return ESMF_FAILURE;
//    }

// TODO  SetEntry will append an xp to the current table.
//       Do we want to add the ability to replace an entry?

    entry[ndeid].deid = ndeid;
    entry[ndeid].xpcount++;

//  If not enough space allocated, allocate more.
//  If entry[ndeid].xp is NULL, then this will simply allocate
    if(entry[ndeid].alloccount < entry[ndeid].xpcount) {
      entry[ndeid].alloccount += ALLOCCHUNK;
      entry[ndeid].xp = (ESMC_XPacket *)realloc(entry[ndeid].xp, 
                                          entry[ndeid].alloccount * sizeof(ESMC_XPacket));
      if (entry[ndeid].xp == NULL) {
	 printf("Not enough memory to add more XPackets!?\n");
	 return ESMF_FAILURE;
	 }
    }

    // This is a contents copy; when this routine returns
    // the caller is free to reuse the packet.
    entry[ndeid].xp[ entry[ndeid].xpcount - 1 ] = *xp;

    return ESMF_SUCCESS;

 } // end ESMC_RTableSetEntry

//-----------------------------------------------------------------------------
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
       int ndeid,             // in  -
       int xpcount,           // in
       ESMC_XPacket **xp) {   // out
//
// !DESCRIPTION:
//     Gets an RTable entry with the given value.
//
//EOP
// !REQUIREMENTS:  

    if (ndeid < 0 || ndeid > entrycount) {
        printf("ndeid out of range, %d must be between 0 and %d\n", 
		                                	ndeid, entrycount);
        return ESMF_FAILURE;
    }

    *xp = &(entry[ndeid].xp[ xpcount ]);

    return ESMF_SUCCESS;

 } // end ESMC_RTableGetEntry

//-----------------------------------------------------------------------------
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
       int ndeid,             // in  -
       int *xpcount) {        // out
//
// !DESCRIPTION:
//     Gets the RTable xpcount
//
//EOP
// !REQUIREMENTS:  

    if (ndeid < 0 || ndeid > entrycount) {
        printf("ndeid out of range, %d must be between 0 and %d\n", 
		                                	ndeid, entrycount);
        return ESMF_FAILURE;
    }

    *xpcount = entry[ndeid].xpcount;

    return ESMF_SUCCESS;

 } // end ESMC_RTableGetCount

//-----------------------------------------------------------------------------
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

    return ESMF_FAILURE;

 } // end ESMC_RTableValidate


//-----------------------------------------------------------------------------
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

    printf(" entrycount=%d, my_deid=%d\n", entrycount, my_deid);
   
    for(i=0; i<entrycount; i++) {
        printf("%2d: deid=%2d, xpcount=%2d, xpaddr=0x%08lx\n",
                 i, entry[i].deid, entry[i].xpcount, (long int)(entry[i].xp)); 
       for (j=0, xptr=&(entry[i].xp[0]); j<entry[i].xpcount; j++, xptr++)
          xptr->ESMC_XPacketPrint();
    }
  
    return ESMF_SUCCESS;

 } // end ESMC_RTablePrint

//-----------------------------------------------------------------------------
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
//BOP
// !IROUTINE:  ~ESMC_RTable - native C++ destructor
//
// !INTERFACE:
      ESMC_RTable::~ESMC_RTable(void) {
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

   // default destructor ok

 } // end ~ESMC_RTable
