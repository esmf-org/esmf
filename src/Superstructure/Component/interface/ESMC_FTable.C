// $Id: ESMC_FTable.C,v 1.3 2003/02/27 21:28:26 nscollins Exp $
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
#include <stdlib.h>
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
           "$Id: ESMC_FTable.C,v 1.3 2003/02/27 21:28:26 nscollins Exp $";
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
    if (nfuncp > funcalloc) {
        funcs = (struct funcinfo *)realloc((void *)funcs, 
                                                  nfuncp * sizeof(funcinfo));
        funcalloc = nfuncp; 
    }
    if (ndatap > dataalloc) {
        data = (struct datainfo *)realloc((void *)data, 
                                                 ndatap * sizeof(datainfo));
        dataalloc = ndatap;
    }

    printf("TableExtend method called \n");
    return ESMF_SUCCESS;

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
    *nfuncp = funccount;
    *ndatap = datacount;

    //printf("TableQuery method called \n");
    return ESMF_SUCCESS;

 } // end ESMC_FTableQuery


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FTableSetFuncPtr - set function pointer, no extra args
//
// !INTERFACE:
      int ESMC_FTable::ESMC_FTableSetFuncPtr(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      char *name,            // in, function name
      void *func,            // in, function address
      enum ftype ftype) {    // in, function type
//
// !DESCRIPTION:
//    Sets the named function pointer
//
//EOP
// !REQUIREMENTS:  developer's guide for classes


 // TODO: test this code
    if (funccount >= funcalloc) {
 	funcs = (struct funcinfo *)realloc((void *)funcs, (funccount+1) * sizeof(struct funcinfo));
        funcalloc = funccount+1;
    }
    funcs[funccount].funcptr = func;
    funcs[funccount].funcname = new char[sizeof(name)+1];
    strcpy(funcs[funccount].funcname, name);
    funcs[funccount].ftype = ftype;
   
    funccount++;

    return ESMF_SUCCESS;

 } // end ESMC_FTableSetFuncPtr

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FTableSetFuncPtr - set function pointer, two extra args
//
// !INTERFACE:
      int ESMC_FTable::ESMC_FTableSetFuncPtr(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      char *name,            // in, function name
      void *func,            // in, function address
      enum ftype ftype,      // in, function type
      void *arg1,            // in, address of arg 1
      void *arg2) {          // in, address of arg 2
//
// !DESCRIPTION:
//    Sets the named function pointer
//
//EOP
// !REQUIREMENTS:  developer's guide for classes


 // TODO: test this code
    if (funccount >= funcalloc) {
 	funcs = (struct funcinfo *)realloc((void *)funcs, (funccount+1) * sizeof(struct funcinfo));
        funcalloc = funccount+1;
    }
    funcs[funccount].funcptr = func;
    funcs[funccount].funcname = new char[sizeof(name)+1];
    strcpy(funcs[funccount].funcname, name);
    funcs[funccount].ftype = ftype;
    funcs[funccount].funcarg1 = arg1;
    funcs[funccount].funcarg2 = arg2;
   
    funccount++;

    return ESMF_SUCCESS;

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
      char *namep,           // in, data name
      void *datap,           // in, data address
      enum dtype dtype) {    // in, data type
//
// !DESCRIPTION:
//    Sets the named data pointer
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

 // TODO: test this code
    if (datacount >= dataalloc) {
 	data = (struct datainfo *)realloc((void *)data, (datacount+1) * sizeof(struct datainfo));
        dataalloc = datacount+1;
    }
    data[datacount].dataptr = datap;
    data[datacount].dataname = new char[sizeof(namep)+1];
    strcpy(data[datacount].dataname, namep);
    data[datacount].dtype = dtype;
   
    datacount++;

    return ESMF_SUCCESS;

 } // end ESMC_FTableSetDataPtr


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FTableGetFuncPtr - get function pointer from name
//
// !INTERFACE:
      int ESMC_FTable::ESMC_FTableGetFuncPtr(
//
// !RETURN VALUE:
//    integer return code
//
// !ARGUMENTS:
      char *name,            // in, function name
      void **func,           // out, function address
      enum ftype *ftype) {   // out, function type
//
// !DESCRIPTION:
//    Returns the named function pointer
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

    int i;

    for (i=0; i<funccount; i++) {
        if (strcmp(name, funcs[i].funcname))
	   continue;
   
        *func = funcs[i].funcptr;
        *ftype = funcs[i].ftype;

        return ESMF_SUCCESS;
    }

    return ESMF_FAILURE;

 } // end ESMC_FTableGetFuncPtr

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FTableGetDataPtr - get data pointer from name
//
// !INTERFACE:
      int ESMC_FTable::ESMC_FTableGetDataPtr(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      char *namep,           // in, data name
      void **datap,          // out, data address
      enum dtype *dtype) {   // out, data type
//
// !DESCRIPTION:
//    Returns the named data pointer
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

    int i;

    for (i=0; i<datacount; i++) {
        if (strcmp(namep, data[i].dataname))
	   continue;

        *datap =  data[i].dataptr;
        *dtype = data[i].dtype;

        return ESMF_SUCCESS;
    }

    return ESMF_FAILURE;

 } // end ESMC_FTableGetDataPtr



//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FTableCallVFuncPtr - call a function w/ proper args
//
// !INTERFACE:
      int ESMC_FTable::ESMC_FTableCallVFuncPtr(
//
// !RETURN VALUE:
//    integer return code
//
// !ARGUMENTS:
      char *name,         // in, function name 
      int *rc) {          // out, function return
//
// !DESCRIPTION:
//    Calls the named function pointer
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

    int i;

    for (i=0; i<funccount; i++) {
        if (strcmp(name, funcs[i].funcname))
	   continue;
   
        switch (funcs[i].ftype) {
          case FT_VOID: {
            VoidFunc vf;
            vf = (VoidFunc)funcs[i].funcptr;
            *rc = (*vf)();
            break;
          }
          case FT_INT: {
            IntFunc vf;
            vf = (IntFunc)funcs[i].funcptr;
            *rc = (*vf)(*(int *)funcs[i].funcarg1);
            break;
          }
          case FT_2INT: {
            Int2Func vf;
            vf = (Int2Func)funcs[i].funcptr;
            *rc = (*vf)(*(int *)funcs[i].funcarg1, *(int *)funcs[i].funcarg2);
            break;
          }
          case FT_INTP: {
            IntPtrFunc vf;
            vf = (IntPtrFunc)funcs[i].funcptr;
            *rc = (*vf)((int *)funcs[i].funcarg1);
            break;
          }
          case FT_VOIDP: {
            VoidPtrFunc vf;
            vf = (VoidPtrFunc)funcs[i].funcptr;
            *rc = (*vf)(funcs[i].funcarg1);
            break;
          }
          case FT_VOIDPINTP: {
            VoidPtrIntPtrFunc vf;
            vf = (VoidPtrIntPtrFunc)funcs[i].funcptr;
            *rc = (*vf)((void *)funcs[i].funcarg1, (int *)funcs[i].funcarg2);
            break;
          }
          default:
            fprintf(stderr, "unknown function type\n");
            return ESMF_FAILURE;
        }

        return ESMF_SUCCESS;
    }

    return ESMF_FAILURE;

 } // end ESMC_FTableCallVFuncPtr

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
    printf("in ftable constructor\n");
    funccount = 0;
    funcalloc = 0;
    funcs = 0;
    datacount = 0;
    dataalloc = 0; 
    data = 0;

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
    printf("in ftable destructor\n");
    if (funcs)
        delete funcs;

    if (data)
	delete data;

    funccount = 0;
    funcalloc = 0;
    funcs = 0;
    datacount = 0;
    dataalloc = 0; 
    data = 0;

 } // end ~ESMC_FTable
