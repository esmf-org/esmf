
//$1.10 2007/04/26 16:13:59 rosalind Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_Field.C"
//==============================================================================
//
// ESMC Field method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt Field} methods declared
// in the companion file {\tt ESMCI\_Field.h}.  These are wrappers for the
// actual code which is implemented in F90.
//
//-----------------------------------------------------------------------------
//

// associated header file
#include "ESMCI_Field.h"
#include "ESMC_Field.h"

//insert any higher level, 3rd party or system includes here
#include <string.h>         // strlen()

// LogErr headers
#include "ESMCI_LogErr.h"
#include "ESMF_LogMacros.inc"

//-----------------------------------------------------------------------------
//BOP
// !CLASS: Field object
//
// !DESCRIPTION:
//  Field class which provides interfaces to the Fortran implementation
//    of Fields.
//EOP
//-----------------------------------------------------------------------------

// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_Field.C,v 1.2.2.1 2010/02/05 19:55:44 svasquez Exp $";
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// prototypes for the fortran interface routines.
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
extern "C" {
// Prototypes of the Fortran interface functions.
void FTN(f_esmf_fieldcreate)(ESMCI::Field *fieldp, void *mesh_pointer, ESMC_ArraySpec *arrayspec, 
    int *gridToFieldMap, int *len1, 
    int *ungriddedLBound, int *len2,
    int *ungriddedUBound, int *len3,
    char *name, 
    int *rc,
    ESMCI_FortranStrLenArg nlen);

void FTN(f_esmf_fielddestroy)(ESMCI::Field *fieldp, int *rc);

void FTN(f_esmf_fieldprint)(ESMCI::Field *fieldp, int *rc);

void FTN(f_esmf_fieldget)(ESMCI::Field *fieldp, void *mesh_pointer, int *rc);

}

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Field routines
//
//

namespace ESMCI {
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Field::create()"
//BOP
// !IROUTINE:  ESMCI::Field::create - Create a new Field
//
// !INTERFACE:
      Field *Field::create(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMCI::Field object
//
// !ARGUMENTS:
    ESMC_Mesh mesh, 
    ESMC_ArraySpec arrayspec, 
    ESMC_InterfaceInt gridToFieldMap, 
    ESMC_InterfaceInt ungriddedLBound, 
    ESMC_InterfaceInt ungriddedUBound, 
    const char *name,  
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Create a new Field.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC\_Field.h)
//
//EOP
    // Initialize return code. Assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;
    if(rc!=NULL) *rc=ESMC_RC_NOT_IMPL;
  
    ESMCI::InterfaceInt *gtfm = (ESMCI::InterfaceInt *)(gridToFieldMap.ptr);
    ESMCI::InterfaceInt *uglb = (ESMCI::InterfaceInt *)(ungriddedLBound.ptr);
    ESMCI::InterfaceInt *ugub = (ESMCI::InterfaceInt *)(ungriddedUBound.ptr);
  
    ESMCI::Field * field = NULL;
  
    if(gtfm->dimCount != 1){
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
         "- gridToFieldMap array must be of rank 1", rc);
       return field;
    }
    if(uglb->dimCount != 1){
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
         "- ungriddedLBound array must be of rank 1", rc);
       return field;
    }
    if(ugub->dimCount != 1){
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
         "- ungriddedUBound array must be of rank 1", rc);
       return field;
    }
  
    int slen = strlen(name);
    char * fName = new char[slen];
    localrc = ESMC_CtoF90string(name, fName, slen);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) {
        delete[] fName;
        return field;
    }

    try{
      field = new Field;
    }catch(...){
      // allocation error
      ESMC_LogDefault.MsgAllocError("for new ESMCI::Field.", rc);
      return ESMC_NULL_POINTER;
    }
  
    FTN(f_esmf_fieldcreate)(field, mesh.ptr, &arrayspec, 
        gtfm->array, &gtfm->extent[0], 
        uglb->array, &uglb->extent[0], 
        ugub->array, &ugub->extent[0], 
        fName, &localrc, slen);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) {
        return field;
    }
  
    delete[] fName;
  
    if (rc) *rc = localrc;
  
    return field;

 }

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Field::destroy()"
//BOP
// !IROUTINE:  ESMCI::Field::destroy - free a Field created with Create
//
// !INTERFACE:
      int Field::destroy(
//
// !RETURN VALUE:
//    int error return code
  
// !ARGUMENTS:
  Field *field){
  
// !DESCRIPTION:
//      ESMF routine which destroys a Field object previously allocated
//      via an ESMC\_FieldCreate routine.  Define for deep classes only.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC\_Field.h)
//
//EOP
// !REQUIREMENTS:  

    // Initialize return code. Assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;
    
    FTN(f_esmf_fielddestroy)(field, &localrc);

    delete field;
    localrc = ESMF_SUCCESS;

    return localrc;

 } 

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Field::getMesh()"
//BOP
// !IROUTINE:  ESMCI::Field::getMesh - Get the number of items contained
//             in this Field
//
// !INTERFACE:
  ESMC_Mesh Field::getMesh(
//
// !RETURN VALUE:
//     ESMC_Mesh object
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Get the number of items contained in an existing Field
//
//EOP
    // Initialize return code. Assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;

    ESMC_Mesh mesh;
    mesh.ptr = NULL; // initialize
    FTN(f_esmf_fieldget)(this, &(mesh.ptr), &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
      return mesh;
    
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
    return mesh;
  }
//-----------------------------------------------------------------------------
  
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Field::print()"
//BOP
// !IROUTINE:  ESMCI::Field::print - print the internal data for a field

// !INTERFACE:
  int Field::print(){

// !RETURN VALUE:
//    int error return code
  
// !ARGUMENTS:
//   none

//  !DESCRIPTION
//    Prints information about the {\tt field} to {\tt stdout}.

    // Initialize return code. Assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;
    int rc=ESMC_RC_NOT_IMPL;

    // Invoque the fortran interface through the F90-C++ "glue" code
    FTN(f_esmf_fieldprint)(this, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
      return rc;

    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
//-----------------------------------------------------------------------------
  
  
} // namespace ESMCI
