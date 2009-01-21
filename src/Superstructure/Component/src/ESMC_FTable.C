// $Id: ESMC_FTable.C,v 1.31.2.6 2009/01/21 21:25:24 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC Function table implementation (body) file
#define ESMF_FILENAME "ESMC_FTable.C"

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
#include "ESMC_Start.h"
#include "ESMC_LogErr.h"
#include "ESMC_Comp.h"
#include "ESMC_GridComp.h"
#include "ESMC_CplComp.h"

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
           "$Id: ESMC_FTable.C,v 1.31.2.6 2009/01/21 21:25:24 cdeluca Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the FTable routines
//
//


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FTableExtend"
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
// !REQUIREMENTS:  

    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    // TODO: allocate space for N items, rounded up?
    if (nfuncp > funcalloc) {
        funcs = (funcinfo *)realloc((void *)funcs, nfuncp * sizeof(funcinfo));
        funcalloc = nfuncp; 
    }
    if (ndatap > dataalloc) {
        data = (datainfo *)realloc((void *)data, ndatap * sizeof(datainfo));
        dataalloc = ndatap;
    }

    //printf("TableExtend called, sizeof(funcinfo)=%d, sizeof(datainfo)=%d\n",
    //                            sizeof(funcinfo), sizeof(datainfo));
    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_FTableExtend

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FTableQuery"
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
// !REQUIREMENTS:  

    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    // fill in values
    *nfuncp = funccount;
    *ndatap = datacount;

    //printf("TableQuery method called \n");
    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_FTableQuery


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FTableSetFuncPtr"
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
      void *func) {          // in, function address
//
// !DESCRIPTION:
//    Sets the named function pointer
//
//EOP
// !REQUIREMENTS:  

    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    int i, thisfunc;
    //char msgbuf[ESMF_MAXSTR];

    // look for the name already existing in the table.  if found
    // replace it.  otherwise add it to the end.
    for (i=0; i<funccount; i++) {
        if (!strcmp(name, funcs[i].funcname))
           break;
    }

    // we found the function, or we got to the end of the table.
    // either way we are ready to add it.

    thisfunc = i;

    // extend the table if needed
    if (thisfunc >= funcalloc) {
        funcs = (funcinfo *)realloc((void *)funcs, (thisfunc+4) * sizeof(funcinfo));
        funcalloc = thisfunc+4;
    }
    funcs[thisfunc].funcptr = func;
    funcs[thisfunc].ftype = FT_VOID;
    // do these only if not replacing an existing entry.
    if (thisfunc == funccount) {
        funcs[thisfunc].funcname = new char[strlen(name)+1];
        strcpy(funcs[thisfunc].funcname, name);
        funccount++;
    }
   

    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_FTableSetFuncPtr

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FTableSetFuncPtr"
//BOP
// !IROUTINE:  ESMC_FTableSetFuncPtr - set function pointer, type; no args yet.
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
//    Sets the named function pointer and type, but specifies no argument 
//    values.  Before this can be called successfully, the user must call
//    ESMC\_FTableSetFuncArgs to fill in the argument list.
//
//EOP
// !REQUIREMENTS:  

    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    int i, thisfunc;
    //char msgbuf[ESMF_MAXSTR];

    // look for the name already existing in the table.  if found
    // replace it.  otherwise add it to the end.
    for (i=0; i<funccount; i++) {
        if (!strcmp(name, funcs[i].funcname))
           break;
    }

    // we found the function, or we got to the end of the table.
    // either way we are ready to add it.

    thisfunc = i;

    // extend the table if needed
    if (thisfunc >= funcalloc) {
        funcs = (funcinfo *)realloc((void *)funcs, (thisfunc+4) * sizeof(funcinfo));
        funcalloc = thisfunc+4;
    }
    funcs[thisfunc].funcptr = func;
    funcs[thisfunc].ftype = ftype;
    // do these only if not replacing an existing entry.
    if (thisfunc == funccount) {
        funcs[thisfunc].funcname = new char[strlen(name)+1];
        strcpy(funcs[thisfunc].funcname, name);
        funccount++;
    }
   

    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_FTableSetFuncPtr

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FTableSetFuncPtr"
//BOP
// !IROUTINE:  ESMC_FTableSetFuncPtr - set voidp, intp specifically
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
      void *arg1,            // in, void *
      int *arg2) {           // in, int *
//
// !DESCRIPTION:
//    Sets the named function pointer and args.  This is a common case
//    so it has it's own interface.
//
//EOP
// !REQUIREMENTS:  

    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    int i, thisfunc;
    //char msgbuf[ESMF_MAXSTR];

    // look for the name already existing in the table.  if found
    // replace it.  otherwise add it to the end.
    for (i=0; i<funccount; i++) {
        if (!strcmp(name, funcs[i].funcname))
           break;
    }

    // we found the function, or we got to the end of the table.
    // either way we are ready to add it.

    thisfunc = i;

    // The 2nd time I come in, delete the old one
    if (thisfunc < funccount) 
       delete ((int *) funcs[thisfunc].funcarg[1]);
    
    // extend the table if needed
    if (thisfunc >= funcalloc) {
        funcs = (funcinfo *)realloc((void *)funcs, (thisfunc+4) * sizeof(funcinfo));
        funcalloc = thisfunc+4;
    }
    funcs[thisfunc].funcptr = func;
    funcs[thisfunc].ftype = FT_VOIDPINTP;
    funcs[thisfunc].funcarg[0] = arg1;
    funcs[thisfunc].funcarg[1] = (void *)arg2;
    // do these only if not replacing an existing entry.
    if (thisfunc == funccount) {
        funcs[thisfunc].funcname = new char[strlen(name)+1];
        strcpy(funcs[thisfunc].funcname, name);
        funccount++;
    }
   

    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_FTableSetFuncPtr

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FTableSetFuncPtr"
//BOP
// !IROUTINE:  ESMC_FTableSetFuncPtr - set function pointer, arg list
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
      int acount,            // in, count of args
      void **arglist) {      // in, address of arg list
//
// !DESCRIPTION:
//    Sets the named function pointer and args
//
//EOP
// !REQUIREMENTS:  

    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    int i, thisfunc;
    //char msgbuf[ESMF_MAXSTR];

    // look for the name already existing in the table.  if found
    // replace it.  otherwise add it to the end.
    for (i=0; i<funccount; i++) {
        if (!strcmp(name, funcs[i].funcname))
           break;
    }

    // we found the function, or we got to the end of the table.
    // either way we are ready to add it.

    thisfunc = i;

    // extend the table if needed
    if (thisfunc >= funcalloc) {
        funcs = (funcinfo *)realloc((void *)funcs, (thisfunc+4) * sizeof(funcinfo));
        funcalloc = thisfunc+4;
    }
    funcs[thisfunc].funcptr = func;
    funcs[thisfunc].ftype = ftype;
    for(i=0; i<acount; i++)
        funcs[thisfunc].funcarg[i] = arglist[i];
    // do these only if not replacing an existing entry.
    if (thisfunc == funccount) {
        funcs[thisfunc].funcname = new char[strlen(name)+1];
        strcpy(funcs[thisfunc].funcname, name);
        funccount++;
    }
   

    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_FTableSetFuncPtr

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FTableSetFuncArgs"
//BOP
// !IROUTINE:  ESMC_FTableSetFuncArgs - set arglist for existing function
//
// !INTERFACE:
      int ESMC_FTable::ESMC_FTableSetFuncArgs(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      char *name,            // in, function name
      int acount,            // in, count of args
      void **arglist) {      // in, address of arg list
//
// !DESCRIPTION:
//    Sets the named function args.  The function must already exist.
//
//EOP
// !REQUIREMENTS:  

    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;
    int status = ESMC_RC_NOT_IMPL;
    int i, j;
    char msgbuf[ESMF_MAXSTR];

    for (i=0; i<funccount; i++) {
        if (strcmp(name, funcs[i].funcname))
           continue;
   
        for(j=0; j<acount; j++)
            funcs[i].funcarg[j] = arglist[j];
   
        return ESMF_SUCCESS;
    }

   
    sprintf(msgbuf, "Error: function '%s' not found\n", name);
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, msgbuf, &status);

    return status;

 } // end ESMC_FTableSetFuncArgs

//-----------------------------------------------------------------------------
 
extern "C"{ 
void FTN(f_esmf_fortranudtpointersize)(int *size);  // prototype used below
void FTN(f_esmf_fortranudtpointercopy)(void *dst, void *src);  // prototype
}
 
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FTableSetDataPtr"
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
      void **datap,          // in, data address
      enum dtype dtype) {    // in, data type
//
// !DESCRIPTION:
//    Sets the named data pointer
//
//EOP
// !REQUIREMENTS:  

    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    // TODO: test this code
    if (datacount >= dataalloc){
      data =
        (datainfo *)realloc((void *)data, (datacount+4) * sizeof(datainfo));
      dataalloc = datacount+4;
    }
    data[datacount].dataname = new char[strlen(namep)+1];
    strcpy(data[datacount].dataname, namep);
    data[datacount].dtype = dtype;
    
    if (dtype == DT_VOIDP){
      data[datacount].dataptr = *datap;
    }else if (dtype == DT_FORTRAN_UDT_POINTER){
      int datumSize;  // upper limit of (UDT, pointer) size
      FTN(f_esmf_fortranudtpointersize)(&datumSize);
      data[datacount].dataptr = (void *)new char[datumSize];
      FTN(f_esmf_fortranudtpointercopy)(data[datacount].dataptr, (void *)datap);
    }
   
    datacount++;

    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_FTableSetDataPtr


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FTableGetFuncPtr"
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
// !REQUIREMENTS:  

    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

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
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FTableGetDataPtr"
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
// !REQUIREMENTS:  

    int i;

    for (i=0; i<datacount; i++) {
      if (strcmp(namep, data[i].dataname)) continue;

      *dtype = data[i].dtype;
      
      if (*dtype == DT_VOIDP){
        *datap = data[i].dataptr;
      }else if (*dtype == DT_FORTRAN_UDT_POINTER){
        FTN(f_esmf_fortranudtpointercopy)((void *)datap, data[i].dataptr);
      }

      return ESMF_SUCCESS;
    }

    return ESMF_FAILURE;

 } // end ESMC_FTableGetDataPtr



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FTableCallVFuncPtr"
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
// !REQUIREMENTS:  

    int i, status;
    status = ESMC_RC_NOT_IMPL;

    *rc = ESMC_RC_NOT_IMPL;
    for (i=0; i<funccount; i++) {
        if (strcmp(name, funcs[i].funcname))
           continue;
   
        switch (funcs[i].ftype) {
          case FT_VOID: {
            VoidFunc vf;
//printf("calling out of case FT_VOID\n");
            vf = (VoidFunc)funcs[i].funcptr;
            (*vf)();
            break;
          }
          case FT_INT: {
            IntFunc vf;
//printf("calling out of case FT_INT\n");
            vf = (IntFunc)funcs[i].funcptr;
            (*vf)(*(int *)funcs[i].funcarg[0]);
            break;
          }
          case FT_2INT: {
            Int2Func vf;
//printf("calling out of case FT_2INT\n");
            vf = (Int2Func)funcs[i].funcptr;
            (*vf)(*(int *)funcs[i].funcarg[0], *(int *)funcs[i].funcarg[1]);
            break;
          }
          case FT_INTP: {
            IntPtrFunc vf;
//printf("calling out of case FT_INTP\n");
            vf = (IntPtrFunc)funcs[i].funcptr;
            (*vf)((int *)funcs[i].funcarg[0]);
            break;
          }
          case FT_VOIDP: {
            VoidPtrFunc vf;
//printf("calling out of case FT_VOIDP\n");
            vf = (VoidPtrFunc)funcs[i].funcptr;
            (*vf)(funcs[i].funcarg[0]);
            break;
          }
          case FT_VOIDPINTP: {
            VoidPtrIntPtrFunc vf;
//printf("calling out of case FT_VOIDPINTP\n");
            vf = (VoidPtrIntPtrFunc)funcs[i].funcptr;
            (*vf)((void *)funcs[i].funcarg[0], (int *)funcs[i].funcarg[1]);
            *rc = *(int *)(funcs[i].funcarg[1]);
            break;
          }
          case FT_COMP1STAT: {
            C1SFunc vf;
//printf("calling out of case FT_COMP1STAT\n");
            vf = (C1SFunc)funcs[i].funcptr;
            (*vf)(funcs[i].funcarg[0], funcs[i].funcarg[1],
                  funcs[i].funcarg[2], (int *)funcs[i].funcarg[3]);
            *rc = *(int *)(funcs[i].funcarg[3]);
            break;
          }
          case FT_COMP2STAT: {
            C2SFunc vf;
//printf("calling out of case FT_COMP2STAT\n");
            vf = (C2SFunc)funcs[i].funcptr;
            (*vf)(funcs[i].funcarg[0], funcs[i].funcarg[1],
                  funcs[i].funcarg[2], funcs[i].funcarg[3],
                 (int *)funcs[i].funcarg[4]);
            *rc = *(int *)(funcs[i].funcarg[4]);
            break;
          }
          case FT_COMPSLIST: {
            CSLFunc vf;
//printf("calling out of case FT_COMPSLIST\n");
            vf = (CSLFunc)funcs[i].funcptr;
            (*vf)(funcs[i].funcarg[0], funcs[i].funcarg[1],
                  funcs[i].funcarg[2], (int *)funcs[i].funcarg[3]);
            *rc = *(int *)(funcs[i].funcarg[3]);
            break;
          }
          default:
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, 
                                         "unknown function type", &status);
            return status;
        }

        return ESMF_SUCCESS;
    }

    return ESMF_FAILURE;

 } // end ESMC_FTableCallVFuncPtr

 
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FTableCallVFuncPtr"
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
      char *name,           // in, function name
      ESMCI::VM *vm_pointer,  // in, pointer to this PET's VM instance
      int *rc) {            // out, function return
//
// !DESCRIPTION:
//    Calls the named function pointer
//
//EOP
// !REQUIREMENTS:  

    int i, status;
    ESMCI::VM *vmm = vm_pointer;
    ESMCI::VM **vm = &vmm;
    
    *rc = ESMC_RC_NOT_IMPL;
    status = ESMC_RC_NOT_IMPL;

    for (i=0; i<funccount; i++) {
        if (strcmp(name, funcs[i].funcname))
           continue;
   
        switch (funcs[i].ftype) {
          case FT_VOID: {
            VoidFuncVM vf;
//printf("calling out of case FT_VOID VM\n");
            vf = (VoidFuncVM)funcs[i].funcptr;
            (*vf)(vm);
            break;
          }
          case FT_INT: {
            IntFuncVM vf;
//printf("calling out of case FT_INT VM\n");
            vf = (IntFuncVM)funcs[i].funcptr;
            (*vf)(*(int *)funcs[i].funcarg[0], vm);
            break;
          }
          case FT_2INT: {
            Int2FuncVM vf;
//printf("calling out of case FT_2INT VM\n");
            vf = (Int2FuncVM)funcs[i].funcptr;
            (*vf)(*(int *)funcs[i].funcarg[0], *(int *)funcs[i].funcarg[1], vm);
            break;
          }
          case FT_INTP: {
            IntPtrFuncVM vf;
//printf("calling out of case FT_INTP VM\n");
            vf = (IntPtrFuncVM)funcs[i].funcptr;
            (*vf)((int *)funcs[i].funcarg[0], vm);
            break;
          }
          case FT_VOIDP: {
            VoidPtrFuncVM vf;
//printf("calling out of case FT_VOIDP VM\n");
            vf = (VoidPtrFuncVM)funcs[i].funcptr;
            (*vf)(funcs[i].funcarg[0], vm);
            break;
          }
          case FT_VOIDPINTP: {
            VoidPtrIntPtrFuncVM vf;
//printf("calling out of case FT_VOIDPINTP VM\n");
            vf = (VoidPtrIntPtrFuncVM)funcs[i].funcptr;
            (*vf)((void *)funcs[i].funcarg[0], (int *)funcs[i].funcarg[1], vm);
            *rc = *(int *)(funcs[i].funcarg[1]);
            break;
          }
          case FT_COMP1STAT: {
            C1SFunc vf;
//printf("calling out of case FT_COMP1STAT VM\n");
            int rrc;
            void *comp;
            ESMC_CompType ctype;
            // Replicate the component object on the heap for this thread
            FTN(f_esmf_compget)((ESMC_Comp *)funcs[i].funcarg[0], &ctype, &rrc);
            if (ctype == ESMF_COMPTYPE_GRID)
              comp = (void *) new ESMC_GridComp;
            else if (ctype == ESMF_COMPTYPE_CPL)
              comp = (void *) new ESMC_CplComp;
            else
              comp = NULL;
            FTN(f_esmf_compreplicate)((ESMC_Comp *)comp, 
              (ESMC_Comp *)funcs[i].funcarg[0], vm, &rrc);
            // Callback: prepare prototype, call, store return code
            vf = (C1SFunc)funcs[i].funcptr;
            (*vf)(comp, funcs[i].funcarg[1],
                  funcs[i].funcarg[2], (int *)funcs[i].funcarg[3]);
            *rc = *(int *)(funcs[i].funcarg[3]);
            // Update the original with any changes made by the child
            FTN(f_esmf_compcopy)((ESMC_Comp *)funcs[i].funcarg[0], 
                                 (ESMC_Comp *)comp, &rrc);
            // Delete the heap copy of the component object for this thread
            FTN(f_esmf_compdelete)((ESMC_Comp *)comp, &rrc);
            delete (ESMC_Comp *)comp;
            break;
          }
          case FT_COMP2STAT: {
//printf("calling out of case FT_COMP2STAT VM\n");
            C2SFunc vf = (C2SFunc)funcs[i].funcptr;
            int rrc;
            void *comp;
            int mypet = vm_pointer->getMypet();
            if (vm_pointer->getNthreads(mypet) > 1){
              // mypet is part of a thread group
              ESMC_CompType ctype;
              // Replicate the component object on the heap for this thread
              FTN(f_esmf_compget)((ESMC_Comp *)funcs[i].funcarg[0], &ctype,
                &rrc);
              if (ctype == ESMF_COMPTYPE_GRID)
                comp = (void *) new ESMC_GridComp;
              else if (ctype == ESMF_COMPTYPE_CPL)
                comp = (void *) new ESMC_CplComp;
              else
                comp = NULL;
              // replicate the component object for each thread
              FTN(f_esmf_compreplicate)((ESMC_Comp *)comp,
                (ESMC_Comp *)funcs[i].funcarg[0], vm, &rrc);
              // call-back into user code with reference to comp object copy
              (*vf)(comp, funcs[i].funcarg[1],
                funcs[i].funcarg[2], funcs[i].funcarg[3],
                (int *)funcs[i].funcarg[4]);
              // Update the original with any changes made by the child
              // todo: how can this be done correctly merging all the copies
              // todo: into a single object?
              //FTN(f_esmf_compcopy)((ESMC_Comp *)funcs[i].funcarg[0],
              //  (ESMC_Comp *)comp, &rrc);
              // Delete the heap copy of the component object for this thread
              FTN(f_esmf_compdelete)((ESMC_Comp *)comp, &rrc);
              delete (ESMC_Comp *)comp;
            }else{
              // mypet is a single-threaded process within this VM
              // insert the current vm object into component object
              FTN(f_esmf_compinsertvm)((ESMC_Comp *)funcs[i].funcarg[0], vm,
                &rrc);
              // call-back into user code with reference to actual comp object
              (*vf)((void *)funcs[i].funcarg[0], funcs[i].funcarg[1],
                funcs[i].funcarg[2], funcs[i].funcarg[3],
                (int *)funcs[i].funcarg[4]);
            }
            // ensure that the return value is set correctly
            *rc = *(int *)(funcs[i].funcarg[4]);
            break;
          }
          case FT_COMPSLIST: {
            CSLFunc vf;
//printf("calling out of case FT_COMPSLIST VM\n");
            int rrc;
            void *comp;
            ESMC_CompType ctype;
            // Replicate the component object on the heap for this thread
            FTN(f_esmf_compget)((ESMC_Comp *)funcs[i].funcarg[0], &ctype, &rrc);
            if (ctype == ESMF_COMPTYPE_GRID)
              comp = (void *) new ESMC_GridComp;
            else if (ctype == ESMF_COMPTYPE_CPL)
              comp = (void *) new ESMC_CplComp;
            else
              comp = NULL;
            FTN(f_esmf_compreplicate)((ESMC_Comp *)comp, 
              (ESMC_Comp *)funcs[i].funcarg[0], vm, &rrc);
            // Callback: prepare prototype, call, store return code
            vf = (CSLFunc)funcs[i].funcptr;
            (*vf)(comp, funcs[i].funcarg[1],
                  funcs[i].funcarg[2], (int *)funcs[i].funcarg[3]);
            *rc = *(int *)(funcs[i].funcarg[3]);
            // Update the original with any changes made by the child
            FTN(f_esmf_compcopy)((ESMC_Comp *)funcs[i].funcarg[0], 
                                 (ESMC_Comp *)comp, &rrc);
            // Delete the heap copy of the component object for this thread
            FTN(f_esmf_compdelete)((ESMC_Comp *)comp, &rrc);
            delete (ESMC_Comp *)comp;
            break;
          }
          default:
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, 
                                         "unknown function type", &status);
            return status;
        }

        return ESMF_SUCCESS;
    }

    return ESMF_FAILURE;

 } // end ESMC_FTableCallVFuncPtr
 
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FTableValidate"
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
    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    return ESMC_RC_NOT_IMPL;

 } // end ESMC_FTableValidate


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FTablePrint"
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
    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    return rc;

 } // end ESMC_FTablePrint


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FTable()"
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

    //printf("in ftable constructor\n");
    funccount = 0;
    funcalloc = 0;
    funcs = NULL;
    datacount = 0;
    dataalloc = 0; 
    data = NULL;

 } // end ESMC_FTable

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "~ESMC_FTable()"
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
  
    int i;

    //printf("in ftable destructor\n");
    //if (funcs) delete[] funcs;
    //if (data) delete[] data;

    for (i=0; i<funccount; i++)
        funcs[i].~funcinfo();
        
    for (i=0; i<datacount; i++)
        data[i].~datainfo();

    if (funcs) free(funcs);
    if (data) free(data);

    funccount = 0;
    funcalloc = 0;
    funcs = 0;
    datacount = 0;
    dataalloc = 0; 
    data = 0;

 } // end ~ESMC_FTable

