// $Id: ESMC_FTable.C,v 1.10 2004/04/23 14:12:56 theurich Exp $
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
           "$Id: ESMC_FTable.C,v 1.10 2004/04/23 14:12:56 theurich Exp $";
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
// !REQUIREMENTS:  

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

    //printf("TableExtend called, sizeof(funcinfo)=%d, sizeof(datainfo)=%d\n",
    //                            sizeof(funcinfo), sizeof(datainfo));
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
// !REQUIREMENTS:  

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
      void *func) {          // in, function address
//
// !DESCRIPTION:
//    Sets the named function pointer
//
//EOP
// !REQUIREMENTS:  


 // TODO: test this code
    if (funccount >= funcalloc) {
 	funcs = (struct funcinfo *)realloc((void *)funcs, (funccount+4) * sizeof(struct funcinfo));
        funcalloc = funccount+4;
    }
    funcs[funccount].funcptr = func;
    funcs[funccount].funcname = new char[strlen(name)+1];
    strcpy(funcs[funccount].funcname, name);
    funcs[funccount].ftype = FT_VOID;
   
    funccount++;

    return ESMF_SUCCESS;

 } // end ESMC_FTableSetFuncPtr

//-----------------------------------------------------------------------------
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


 // TODO: search table for existing function with same name and 
 //        overwrite it, instead of just adding again to end of table.

 // TODO: test this code
    if (funccount >= funcalloc) {
 	funcs = (struct funcinfo *)realloc((void *)funcs, (funccount+4) * sizeof(struct funcinfo));
        funcalloc = funccount+4;
    }
    funcs[funccount].funcptr = func;
    funcs[funccount].funcname = new char[strlen(name)+1];
    strcpy(funcs[funccount].funcname, name);
    funcs[funccount].ftype = ftype;
   
    funccount++;

    return ESMF_SUCCESS;

 } // end ESMC_FTableSetFuncPtr

//-----------------------------------------------------------------------------
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

    int i;

 // TODO: test this code
    if (funccount >= funcalloc) {
 	funcs = (struct funcinfo *)realloc((void *)funcs, (funccount+4) * sizeof(struct funcinfo));
        funcalloc = funccount+4;
    }
    funcs[funccount].funcptr = func;
    funcs[funccount].funcname = new char[strlen(name)+1];
    strcpy(funcs[funccount].funcname, name);
    funcs[funccount].ftype = FT_VOIDPINTP;
    funcs[funccount].funcarg[0] = arg1;
    funcs[funccount].funcarg[1] = (void *)arg2;
   
    funccount++;

    return ESMF_SUCCESS;

 } // end ESMC_FTableSetFuncPtr

//-----------------------------------------------------------------------------
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

    int i;

 // TODO: test this code
    if (funccount >= funcalloc) {
 	funcs = (struct funcinfo *)realloc((void *)funcs, (funccount+4) * sizeof(struct funcinfo));
        funcalloc = funccount+4;
    }
    funcs[funccount].funcptr = func;
    funcs[funccount].funcname = new char[strlen(name)+1];
    strcpy(funcs[funccount].funcname, name);
    funcs[funccount].ftype = ftype;
    for(i=0; i<acount; i++)
        funcs[funccount].funcarg[i] = arglist[i];
   
    funccount++;

    return ESMF_SUCCESS;

 } // end ESMC_FTableSetFuncPtr

//-----------------------------------------------------------------------------
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

    int i, j;

    for (i=0; i<funccount; i++) {
        if (strcmp(name, funcs[i].funcname))
	   continue;
   
        for(j=0; j<acount; j++)
            funcs[i].funcarg[j] = arglist[j];
   
        return ESMF_SUCCESS;
    }

    printf("Error: function '%s' not found\n", name);
    return ESMF_FAILURE;

 } // end ESMC_FTableSetFuncArgs

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
// !REQUIREMENTS:  

 // TODO: test this code
    if (datacount >= dataalloc) {
 	data = (struct datainfo *)realloc((void *)data, (datacount+4) * sizeof(struct datainfo));
        dataalloc = datacount+4;
    }
    data[datacount].dataptr = datap;
    data[datacount].dataname = new char[strlen(namep)+1];
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
// !REQUIREMENTS:  

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
// !REQUIREMENTS:  

    int i;

    for (i=0; i<datacount; i++) {
        if (strcmp(namep, data[i].dataname))
	   continue;

        *datap = data[i].dataptr;
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
// !REQUIREMENTS:  

    int i;

    *rc = ESMF_FAILURE;
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
            fprintf(stderr, "unknown function type\n");
            return ESMF_FAILURE;
        }

        return ESMF_SUCCESS;
    }

    return ESMF_FAILURE;

 } // end ESMC_FTableCallVFuncPtr

 
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
      char *name,           // in, function name
      ESMC_VM *vm_pointer,  // in, pointer to this PET's VM instance
      int *rc) {            // out, function return
//
// !DESCRIPTION:
//    Calls the named function pointer
//
//EOP
// !REQUIREMENTS:  

    int i;
    ESMC_VM *vmm = vm_pointer;
    ESMC_VM **vm = &vmm;
    
    *rc = ESMF_FAILURE;
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
            // Delete the heap copy of the component object for this thread
            FTN(f_esmf_compdelete)((ESMC_Comp *)comp, &rrc);
            delete (ESMC_Comp *)comp;
            break;
          }
          case FT_COMP2STAT: {
            C2SFunc vf;
//printf("calling out of case FT_COMP2STAT VM\n");
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
            vf = (C2SFunc)funcs[i].funcptr;
            (*vf)(comp, funcs[i].funcarg[1],
                  funcs[i].funcarg[2], funcs[i].funcarg[3],
                 (int *)funcs[i].funcarg[4]);
            *rc = *(int *)(funcs[i].funcarg[4]);
            // Delete the heap copy of the component object for this thread
            FTN(f_esmf_compdelete)((ESMC_Comp *)comp, &rrc);
            delete (ESMC_Comp *)comp;
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
            // Delete the heap copy of the component object for this thread
            FTN(f_esmf_compdelete)((ESMC_Comp *)comp, &rrc);
            delete (ESMC_Comp *)comp;
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
    //printf("in ftable constructor\n");
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
    //printf("in ftable destructor\n");
    if (funcs)
        delete[] funcs;

    if (data)
	delete[] data;

    funccount = 0;
    funcalloc = 0;
    funcs = 0;
    datacount = 0;
    dataalloc = 0; 
    data = 0;

 } // end ~ESMC_FTable

