// $Id: ESMC_FTable.C,v 1.1 2003/02/26 01:17:31 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC Function table implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt Function Table} methods 
// declared in the companion file {\tt ESMC\_FTable.h}.  
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
#include <string.h>
#include <stdio.h>
#include "ESMC.h"

//-----------------------------------------------------------------------------
//BOP
// !CLASS: Function table object
//
// !DESCRIPTION:
//   Implementation of a Function table and Data block table.
//
//EOP
//-----------------------------------------------------------------------------

 // associated class definition file
#include "ESMC_FTable.h"

 // return min value 
#define min(a,b)  (((a)<(b))?(a):(b))

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
           "$Id: ESMC_FTable.C,v 1.1 2003/02/26 01:17:31 nscollins Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the FTable routines
//
//


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FTableExtend - make space for additional functions/data
//
// !INTERFACE:
      int ESMC_FTable::ESMC_FTableExtend(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int nfuncp,     // in, number of functions which will be added
      int ndatap) {   // in, number of data pointers which will be added
//
// !DESCRIPTION:
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

    // TODO: allocate space for N items, rounded up?

    printf("TableExtend method called \n");
    return ESMF_FAILURE;

 } // end ESMC_FTableExtend

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FTableQuery - return count of functions/data
//
// !INTERFACE:
      int ESMC_FTable::ESMC_FTableQuery(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *nfuncp,     // out, number of functions which will be added
      int *ndatap) {   // out, number of data pointers which will be added
//
// !DESCRIPTION:
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

    // fill in values
    *nfuncp = this->funccount;
    *ndatap = this->datacount;

    //printf("TableQuery method called \n");
    return ESMF_SUCCESS;

 } // end ESMC_FTableQuery


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FTableSetFuncPtr - set function pointer
//
// !INTERFACE:
      int ESMC_FTable::ESMC_FTableSetFuncPtr(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void *func,            // in, function address
      char *name,            // in, function name
      enum ftype ftype) {    // in, function type
//
// !DESCRIPTION:
//    Sets the named function pointer
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

    int i;

 // TODO: add code here

    return ESMF_FAILURE;

 } // end ESMC_FTableSetFuncPtr

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FTableSetDataPtr - set data pointer
//
// !INTERFACE:
      int ESMC_FTable::ESMC_FTableSetDataPtr(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void *data,            // in, data address
      char *name,            // in, data name
      enum dtype dtype) {    // in, data type
//
// !DESCRIPTION:
//    Sets the named data pointer
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

    int i;

 // TODO: add code here

    return ESMF_FAILURE;

 } // end ESMC_FTableSetDataPtr


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FTableGetFuncPtr - get function pointer from name
//
// !INTERFACE:
      void * ESMC_FTable::ESMC_FTableGetFuncPtr(
//
// !RETURN VALUE:
//    function pointer
//
// !ARGUMENTS:
      char *name) {    // in, function name
//
// !DESCRIPTION:
//    Returns the named function pointer
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  TODO: add type information also to be returned
//
    int i;

    for (i=0; i<this->funccount; i++) {
        if (strcmp(name, this->funcs[i].funcname))
	   continue;

        return this->funcs[i].funcptr;
    }

    return 0;

 } // end ESMC_FTableGetFuncPtr

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FTableGetDataPtr - get data pointer from name
//
// !INTERFACE:
      void * ESMC_FTable::ESMC_FTableGetDataPtr(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      char *name) {    // in, data name
//
// !DESCRIPTION:
//    Returns the named data pointer
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  TODO: add type information also to be returned
//
    int i;

    for (i=0; i<this->datacount; i++) {
        if (strcmp(name, this->data[i].dataname))
	   continue;

        return this->data[i].dataptr;
    }

    return 0;

 } // end ESMC_FTableGetDataPtr


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FTableValidate - internal consistency check for a Component
//
// !INTERFACE:
      int ESMC_FTable::ESMC_FTableValidate(
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

 } // end ESMC_FTableValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FTablePrint - print contents of a Component
//
// !INTERFACE:
      int ESMC_FTable::ESMC_FTablePrint(
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

 } // end ESMC_FTablePrint

#if 0
 enum ftype { FVOID=1, FINT=2, F2INT=3 };
 struct funcinfo {
    char *funcname;
    void *funcptr;
    enum ftype ftype;
 };

 enum dtype { FVOIDPTR=1 };
 struct datainfo {
    char *dataname;
    void *dataptr;
    enum dtype dtype;
 };

 // class declaration type
 class ESMC_FTable {

   private:
    int funccount;       
    int funcalloc;
    struct funcinfo *funcs;
    int datacount;
    int dataalloc;
    struct datainfo *data;
#endif

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FTable - native C++ constructor
//
// !INTERFACE:
      ESMC_FTable::ESMC_FTable(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      void) {
//
// !DESCRIPTION:
//   Native constructor.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//
    this->funccount = 0;
    this->funcalloc = 0;
    this->funcs = 0;
    this->datacount = 0;
    this->dataalloc = 0; 
    this->data = 0;

 } // end ESMC_FTable

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_FTable - native C++ destructor
//
// !INTERFACE:
      ESMC_FTable::~ESMC_FTable(void) {
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
    if (this->funcs)
        delete this->funcs;

    if (this->data)
	delete this->data;

    this->funccount = 0;
    this->funcalloc = 0;
    this->funcs = 0;
    this->datacount = 0;
    this->dataalloc = 0; 
    this->data = 0;

 } // end ~ESMC_FTable
