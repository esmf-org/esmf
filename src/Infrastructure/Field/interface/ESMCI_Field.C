//$1.10 2007/04/26 16:13:59 rosalind Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2013, University Corporation for Atmospheric Research, 
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
// associated header file
#include "ESMCI_Field.h"

//insert any higher level, 3rd party or system includes here
#include <string.h>         // strlen()

// ESMF headers
#include "ESMCI_LogErr.h"
#include "ESMCI_Array.h"
#include "ESMCI_Grid.h"

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
static const char *const version = "$Id$";
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
void FTN_X(f_esmf_fieldcreategridas)(ESMCI::Field *fieldp, ESMCI::Grid **grid, 
    ESMC_ArraySpec *arrayspec, ESMC_StaggerLoc *staggerloc,
    int *gridToFieldMap, int *len1, int *gtfm_present, 
    int *ungriddedLBound, int *len2, int *uglb_present,
    int *ungriddedUBound, int *len3, int *ugub_present,
    char *name, int *rc,
    ESMCI_FortranStrLenArg nlen);

void FTN_X(f_esmf_fieldcreategridtk)(ESMCI::Field *fieldp, ESMCI::Grid **grid, 
    ESMC_TypeKind_Flag *typekind, ESMC_StaggerLoc *staggerloc,
    int *gridToFieldMap, int *len1, int *gtfm_present, 
    int *ungriddedLBound, int *len2, int *uglb_present,
    int *ungriddedUBound, int *len3, int *ugub_present,
    char *name, int *rc,
    ESMCI_FortranStrLenArg nlen);

void FTN_X(f_esmf_fieldcreatemeshas)(ESMCI::Field *fieldp, void *mesh_pointer, 
    ESMC_ArraySpec *arrayspec, 
    int *gridToFieldMap, int *len1, int *gtfm_present, 
    int *ungriddedLBound, int *len2, int *uglb_present,
    int *ungriddedUBound, int *len3, int *ugub_present,
    char *name, int *rc,
    ESMCI_FortranStrLenArg nlen);

void FTN_X(f_esmf_fieldcreatemeshtk)(ESMCI::Field *fieldp, void *mesh_pointer, 
    ESMC_TypeKind_Flag *typekind, ESMC_MeshLoc_Flag *meshloc,
    int *gridToFieldMap, int *len1, int *gtfm_present, 
    int *ungriddedLBound, int *len2, int *uglb_present,
    int *ungriddedUBound, int *len3, int *ugub_present,
    char *name, int *rc,
    ESMCI_FortranStrLenArg nlen);

void FTN_X(f_esmf_fielddestroy)(ESMCI::Field *fieldp, int *rc);

void FTN_X(f_esmf_fieldgetmesh)(ESMCI::Field *fieldp, void *mesh_pointer,
  int *rc);

void FTN_X(f_esmf_fieldgetmesh)(ESMCI::Field *fieldp, void *mesh_pointer,
  int *rc);

void FTN_X(f_esmf_fieldgetarray)(ESMCI::Field *fieldp, void *array_pointer,
  int *rc);

void FTN_X(f_esmf_fieldprint)(ESMCI::Field *fieldp, int *rc);

void FTN_X(f_esmf_fieldcast)(ESMCI::F90ClassHolder *fieldOut,
  ESMCI::Field *fieldIn, int *rc);

void FTN_X(f_esmf_regridgetarea)(ESMCI::Field *fieldp, int *rc);

void FTN_X(f_esmf_regridstore)(ESMCI::Field *fieldpsrc, ESMCI::Field *fieldpdst,
  int *srcMaskValues, int *len1, int *smv_present,
  int *dstMaskValues, int *len2, int *dmv_present,
  ESMCI::RouteHandle **routehandlep, 
  ESMC_RegridMethod_Flag *regridmethod, int *rm_present,
  ESMC_PoleMethod_Flag *polemethod, int *pm_present,
  int *regridPoleNPnts, int *rpnp_present,
  ESMC_UnmappedAction_Flag *unmappedaction, int *ua_present,
  ESMCI::Field *srcfracfieldp, int *sff_present, 
  ESMCI::Field *dstfracfieldp, int *dff_present, int *rc);

void FTN_X(f_esmf_regrid)(ESMCI::Field *fieldpsrc, ESMCI::Field *fieldpdst,
  ESMCI::RouteHandle **routehandlep, ESMC_Region_Flag *zeroregion, int *zr_present,
  int *rc);

void FTN_X(f_esmf_regridrelease)(ESMCI::RouteHandle **routehandlep, int *rc);

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
// !IROUTINE:  ESMCI::Field::create - Create a new Field from Grid and ArraySpec
//
// !INTERFACE:
      Field *Field::create(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMCI::Field object
//
// !ARGUMENTS:
    ESMC_Grid *grid, 
    ESMC_ArraySpec arrayspec,
    ESMC_StaggerLoc staggerloc,
    ESMC_InterfaceInt *gridToFieldMap, 
    ESMC_InterfaceInt *ungriddedLBound, 
    ESMC_InterfaceInt *ungriddedUBound, 
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

    int gtfm_present, uglb_present, ugub_present;
    bool gtfm_created, uglb_created, ugub_created;
    gtfm_present = 0;
    uglb_present = 0;
    ugub_present = 0;
    gtfm_created = false;
    uglb_created = false;
    ugub_created = false;
    ESMCI::InterfaceInt *gtfm, *uglb, *ugub;

    if (gridToFieldMap != NULL) {
      gtfm = (ESMCI::InterfaceInt *)(gridToFieldMap->ptr);
      if(gtfm->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- gridToFieldMap array must be of rank 1", ESMC_CONTEXT, rc);
         return ESMC_NULL_POINTER;
      }
      gtfm_present = 1;
    } else
      gtfm = new ESMCI::InterfaceInt();

    if (ungriddedLBound != NULL) {
      uglb = (ESMCI::InterfaceInt *)(ungriddedLBound->ptr);
      if(uglb->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- ungriddedLBound array must be of rank 1", ESMC_CONTEXT, rc);
         return ESMC_NULL_POINTER;
      }
      uglb_present = 1;
    } else
      uglb = new ESMCI::InterfaceInt();

    if (ungriddedUBound != NULL) {
      ugub = (ESMCI::InterfaceInt *)(ungriddedUBound->ptr);
      if(ugub->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- ungriddedUBound array must be of rank 1", ESMC_CONTEXT, rc);
         return ESMC_NULL_POINTER;
      }
      ugub_present = 1;
    } else
      ugub = new ESMCI::InterfaceInt();

    char * fName = NULL;
    int slen = 0;
    if(name != NULL){
      slen = strlen(name);
      fName = new char[slen];
      localrc = ESMC_CtoF90string(name, fName, slen);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc)) {
        if (gtfm_created) if (gtfm_created) delete gtfm;
        if (uglb_created) if (uglb_created) delete uglb;
        if (ugub_created) if (ugub_created) delete ugub;
        delete[] fName;
        return ESMC_NULL_POINTER;
      }
    }

    // prepare the field pointer
    ESMCI::Field * field = NULL;
    try{
      field = new Field;
    }catch(...){
      // allocation error
      ESMC_LogDefault.MsgAllocError("for new ESMCI::Field.", ESMC_CONTEXT, rc);
      if (gtfm_created) delete gtfm;
      if (uglb_created) delete uglb;
      if (ugub_created) delete ugub;
      delete[] fName;
      return ESMC_NULL_POINTER;
    }

    // prepare the grid pointer
    ESMCI::Grid *gridp = reinterpret_cast<ESMCI::Grid *>(grid->ptr); 
 
    FTN_X(f_esmf_fieldcreategridas)(field, &gridp,
        &arrayspec, &staggerloc,
        gtfm->array, &gtfm->extent[0], &gtfm_present,
        uglb->array, &uglb->extent[0], &uglb_present,
        ugub->array, &ugub->extent[0], &ugub_present,
        fName, &localrc, slen);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) {
      if (gtfm_created) delete gtfm;
      if (uglb_created) delete uglb;
      if (ugub_created) delete ugub;
      if(fName) delete[] fName;
      return ESMC_NULL_POINTER;
    }
  
    if (gtfm_created) delete gtfm;
    if (uglb_created) delete uglb;
    if (ugub_created) delete ugub;
    if(fName) delete[] fName;
  
    if (rc) *rc = localrc;
  
    return field;

 }

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Field::create()"
//BOP
// !IROUTINE:  ESMCI::Field::create - Create a new Field from Grid and typekind
//
// !INTERFACE:
      Field *Field::create(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMCI::Field object
//
// !ARGUMENTS:
    ESMC_Grid *grid, 
    ESMC_TypeKind_Flag typekind, 
    ESMC_StaggerLoc staggerloc,
    ESMC_InterfaceInt *gridToFieldMap, 
    ESMC_InterfaceInt *ungriddedLBound, 
    ESMC_InterfaceInt *ungriddedUBound, 
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
  
    int gtfm_present, uglb_present, ugub_present;
    bool gtfm_created, uglb_created, ugub_created;
    gtfm_present = 0;
    uglb_present = 0;
    ugub_present = 0;
    gtfm_created = false;
    uglb_created = false;
    ugub_created = false;
    ESMCI::InterfaceInt *gtfm, *uglb, *ugub;

    if (gridToFieldMap != NULL) {
      gtfm = (ESMCI::InterfaceInt *)(gridToFieldMap->ptr);
      if(gtfm->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- gridToFieldMap array must be of rank 1", ESMC_CONTEXT, rc);
         return ESMC_NULL_POINTER;
      }
      gtfm_present = 1;
    } else
      gtfm = new ESMCI::InterfaceInt();

    if (ungriddedLBound != NULL) {
      uglb = (ESMCI::InterfaceInt *)(ungriddedLBound->ptr);
      if(uglb->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- ungriddedLBound array must be of rank 1", ESMC_CONTEXT, rc);
         return ESMC_NULL_POINTER;
      }
      uglb_present = 1;
    } else
      uglb = new ESMCI::InterfaceInt();

    if (ungriddedUBound != NULL) {
      ugub = (ESMCI::InterfaceInt *)(ungriddedUBound->ptr);
      if(ugub->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- ungriddedUBound array must be of rank 1", ESMC_CONTEXT, rc);
         return ESMC_NULL_POINTER;
      }
      ugub_present = 1;
    } else
      ugub = new ESMCI::InterfaceInt();

    char * fName = NULL;
    int slen = 0;
    if(name != NULL){
      slen = strlen(name);
      fName = new char[slen];
      localrc = ESMC_CtoF90string(name, fName, slen);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc)) {
        if (gtfm_created) delete gtfm;
        if (uglb_created) delete uglb;
        if (ugub_created) delete ugub;
        delete[] fName;
        return ESMC_NULL_POINTER;
      }
    }

    // prepare the Field pointer
    ESMCI::Field * field = NULL;
    try{
      field = new Field;
    }catch(...){
      // allocation error
      ESMC_LogDefault.MsgAllocError("for new ESMCI::Field.", ESMC_CONTEXT, rc);
      if (gtfm_created) delete gtfm;
      if (uglb_created) delete uglb;
      if (ugub_created) delete ugub;
      if(fName) delete[] fName;
      return ESMC_NULL_POINTER;
    }
  
    // prepare the Grid pointer
    ESMCI::Grid *gridp = reinterpret_cast<ESMCI::Grid *>(grid->ptr); 

    FTN_X(f_esmf_fieldcreategridtk)(field, &gridp, 
        &typekind, &staggerloc,
        gtfm->array, &gtfm->extent[0], &gtfm_present,
        uglb->array, &uglb->extent[0], &uglb_present,
        ugub->array, &ugub->extent[0], &ugub_present,
        fName, &localrc, slen);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) {
      if (gtfm_created) delete gtfm;
      if (uglb_created) delete uglb;
      if (ugub_created) delete ugub;
      if(fName) delete[] fName;
      return ESMC_NULL_POINTER;
    }
  
    if(fName) delete[] fName;
    if (gtfm_created) delete gtfm;
    if (uglb_created) delete uglb;
    if (ugub_created) delete ugub;
  
    if (rc) *rc = localrc;
  
    return field;

 }

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Field::create()"
//BOP
// !IROUTINE:  ESMCI::Field::create - Create a new Field from Mesh and ArraySpec
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
    ESMC_InterfaceInt *gridToFieldMap, 
    ESMC_InterfaceInt *ungriddedLBound, 
    ESMC_InterfaceInt *ungriddedUBound, 
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
  
    int gtfm_present, uglb_present, ugub_present;
    bool gtfm_created, uglb_created, ugub_created;
    gtfm_present = 0;
    uglb_present = 0;
    ugub_present = 0;
    gtfm_created = false;
    uglb_created = false;
    ugub_created = false;
    ESMCI::InterfaceInt *gtfm, *uglb, *ugub;

    if (gridToFieldMap != NULL) {
      gtfm = (ESMCI::InterfaceInt *)(gridToFieldMap->ptr);
      if(gtfm->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- gridToFieldMap array must be of rank 1", ESMC_CONTEXT, rc);
         return ESMC_NULL_POINTER;
      }
      gtfm_present = 1;
    } else
      gtfm = new ESMCI::InterfaceInt();

    if (ungriddedLBound != NULL) {
      uglb = (ESMCI::InterfaceInt *)(ungriddedLBound->ptr);
      if(uglb->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- ungriddedLBound array must be of rank 1", ESMC_CONTEXT, rc);
         return ESMC_NULL_POINTER;
      }
      uglb_present = 1;
    } else
      uglb = new ESMCI::InterfaceInt();

    if (ungriddedUBound != NULL) {
      ugub = (ESMCI::InterfaceInt *)(ungriddedUBound->ptr);
      if(ugub->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- ungriddedUBound array must be of rank 1", ESMC_CONTEXT, rc);
         return ESMC_NULL_POINTER;
      }
      ugub_present = 1;
    } else
      ugub = new ESMCI::InterfaceInt();

    char * fName = NULL;
    int slen = 0;
    if(name != NULL){
      slen = strlen(name);
      fName = new char[slen];
      localrc = ESMC_CtoF90string(name, fName, slen);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc)) {
        if (gtfm_created) delete gtfm;
        if (uglb_created) delete uglb;
        if (ugub_created) delete ugub;
        delete[] fName;
        return ESMC_NULL_POINTER;
      }
    }

    ESMCI::Field * field = NULL;
    try{
      field = new Field;
    }catch(...){
      // allocation error
      ESMC_LogDefault.MsgAllocError("for new ESMCI::Field.", ESMC_CONTEXT, rc);
      if(fName) delete[] fName;
      if (gtfm_created) delete gtfm;
      if (uglb_created) delete uglb;
      if (ugub_created) delete ugub;
      return ESMC_NULL_POINTER;
    }
  
    FTN_X(f_esmf_fieldcreatemeshas)(field, mesh.ptr, &arrayspec, 
      gtfm->array, &gtfm->extent[0], &gtfm_present,
      uglb->array, &uglb->extent[0], &uglb_present,
      ugub->array, &ugub->extent[0], &ugub_present,
      fName, &localrc, slen);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) {
      if(fName) delete[] fName;
      if (gtfm_created) delete gtfm;
      if (uglb_created) delete uglb;
      if (ugub_created) delete ugub;
      return ESMC_NULL_POINTER;
    }
  
    if(fName) delete[] fName;
    if (gtfm_created) delete gtfm;
    if (uglb_created) delete uglb;
    if (ugub_created) delete ugub;
  
    if (rc) *rc = localrc;
  
    return field;

 }

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Field::create()"
//BOP
// !IROUTINE:  ESMCI::Field::create - Create a new Field from Mesh and typekind
//
// !INTERFACE:
      Field *Field::create(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMCI::Field object
//
// !ARGUMENTS:
    ESMC_Mesh mesh, 
    ESMC_TypeKind_Flag typekind, 
    ESMC_MeshLoc_Flag meshloc,
    ESMC_InterfaceInt *gridToFieldMap, 
    ESMC_InterfaceInt *ungriddedLBound, 
    ESMC_InterfaceInt *ungriddedUBound, 
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
  
    int gtfm_present, uglb_present, ugub_present;
    bool gtfm_created, uglb_created, ugub_created;
    gtfm_present = 0;
    uglb_present = 0;
    ugub_present = 0;
    gtfm_created = false;
    uglb_created = false;
    ugub_created = false;
    ESMCI::InterfaceInt *gtfm, *uglb, *ugub;

    if (gridToFieldMap != NULL) {
      gtfm = (ESMCI::InterfaceInt *)(gridToFieldMap->ptr);
      if(gtfm->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- gridToFieldMap array must be of rank 1", ESMC_CONTEXT, rc);
         return ESMC_NULL_POINTER;
      }
      gtfm_present = 1;
    } else
      gtfm = new ESMCI::InterfaceInt();

    if (ungriddedLBound != NULL) {
      uglb = (ESMCI::InterfaceInt *)(ungriddedLBound->ptr);
      if(uglb->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- ungriddedLBound array must be of rank 1", ESMC_CONTEXT, rc);
         return ESMC_NULL_POINTER;
      }
      uglb_present = 1;
    } else
      uglb = new ESMCI::InterfaceInt();

    if (ungriddedUBound != NULL) {
      ugub = (ESMCI::InterfaceInt *)(ungriddedUBound->ptr);
      if(ugub->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- ungriddedUBound array must be of rank 1", ESMC_CONTEXT, rc);
         return ESMC_NULL_POINTER;
      }
      ugub_present = 1;
    } else
      ugub = new ESMCI::InterfaceInt();

    char * fName = NULL;
    int slen = 0;
    if(name != NULL){
      slen = strlen(name);
      fName = new char[slen];
      localrc = ESMC_CtoF90string(name, fName, slen);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc)) {
        if (gtfm_created) delete gtfm;
        if (uglb_created) delete uglb;
        if (ugub_created) delete ugub;
        delete[] fName;
        return ESMC_NULL_POINTER;
      }
    }

    ESMCI::Field * field = NULL;
    try{
      field = new Field;
    }catch(...){
      // allocation error
      ESMC_LogDefault.MsgAllocError("for new ESMCI::Field.", ESMC_CONTEXT, rc);
      if(fName) delete[] fName;
      if (gtfm_created) delete gtfm;
      if (uglb_created) delete uglb;
      if (ugub_created) delete ugub;
      return ESMC_NULL_POINTER;
    }

    FTN_X(f_esmf_fieldcreatemeshtk)(field, mesh.ptr, &typekind, &meshloc,
      gtfm->array, &gtfm->extent[0], &gtfm_present,
      uglb->array, &uglb->extent[0], &uglb_present,
      ugub->array, &ugub->extent[0], &ugub_present,
      fName, &localrc, slen);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) {
      if(fName) delete[] fName;
      if (gtfm_created) delete gtfm;
      if (uglb_created) delete uglb;
      if (ugub_created) delete ugub;
      return ESMC_NULL_POINTER;
    }
  
    if(fName) delete[] fName;
    if (gtfm_created) delete gtfm;
    if (uglb_created) delete uglb;
    if (ugub_created) delete ugub;
  
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
    
    FTN_X(f_esmf_fielddestroy)(field, &localrc);

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
    FTN_X(f_esmf_fieldgetmesh)(this, &(mesh.ptr), &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return mesh;
    
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
    return mesh;
  }
//-----------------------------------------------------------------------------
  
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Field::getArray()"
//BOP
// !IROUTINE:  ESMCI::Field::getArray - Get the number of items contained
//             in this Field
//
// !INTERFACE:
  ESMC_Array Field::getArray(
//
// !RETURN VALUE:
//     ESMC_Array object
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

    ESMC_Array array;
    array.ptr = NULL; // initialize
    FTN_X(f_esmf_fieldgetarray)(this, (ESMCI::Array **)&(array.ptr), &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return array;
    
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
    return array;
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
    FTN_X(f_esmf_fieldprint)(this, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;

    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
//-----------------------------------------------------------------------------
  
  
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Field::castToFortran()"
//BOP
// !IROUTINE:  ESMCI::Field::castToFortran - cast Field object to Fortran
// !INTERFACE:
  int Field::castToFortran(F90ClassHolder *fc){

// !RETURN VALUE:
//    int error return code
  
// !ARGUMENTS:
//   returned Fortran cast

//  !DESCRIPTION
//    Cast Field object to Fortran.

    // Initialize return code. Assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;
    int rc=ESMC_RC_NOT_IMPL;

    // Invoque the fortran interface through the F90-C++ "glue" code
    FTN_X(f_esmf_fieldcast)(fc, this, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;

    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Field::regridgetarea()"
//BOP
// !IROUTINE:  ESMCI::Field::regridgetarea - get the area of cells used for
//                                           conservative interpolation
//
// !INTERFACE:
  int Field::regridgetarea(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
    Field *fieldp) {
//
// !DESCRIPTION:
//
//
//EOP
    // Initialize return code. Assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    FTN_X(f_esmf_regridgetarea)(fieldp, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;

    rc = ESMF_SUCCESS;
    return rc;
  }
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Field::regridstore()"
//BOP
// !IROUTINE:  ESMCI::Field::regridstore - precompute a regriddding operation
//
// !INTERFACE:
  int Field::regridstore(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
    Field *fieldpsrc, 
    Field *fieldpdst, 
    ESMC_InterfaceInt *srcMaskValues, 
    ESMC_InterfaceInt *dstMaskValues, 
    RouteHandle **routehandlep, 
    ESMC_RegridMethod_Flag *regridMethod, 
    ESMC_PoleMethod_Flag *polemethod,
    int *regridPoleNPnts,
    ESMC_UnmappedAction_Flag *unmappedAction,
    Field *srcFracField, 
    Field *dstFracField) {
//
// !DESCRIPTION:
//
//
//EOP
    // Initialize return code. Assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
  
    int smv_present, dmv_present;
    int sff_present, dff_present;
    int rm_present, ua_present;
    int pm_present, rpnp_present;
    bool smv_created, dmv_created;
    bool sff_created, dff_created;
    smv_present = 0;
    dmv_present = 0;
    sff_present = 0;
    dff_present = 0;
    rm_present = 0;
    ua_present = 0;
    pm_present = 0;
    rpnp_present = 0;
    smv_created = false;
    dmv_created = false;
    sff_created = false;
    dff_created = false;
    ESMCI::InterfaceInt *smv, *dmv;
    ESMCI::Field *sff, *dff;

    if (srcMaskValues != NULL) {
      smv = (ESMCI::InterfaceInt *)(srcMaskValues->ptr);
      if(smv->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- srcMaskValues array must be of rank 1", ESMC_CONTEXT, &rc);
         return ESMC_NULL_POINTER;
      }
      smv_present = 1;
    } else {
      smv = new ESMCI::InterfaceInt();
      smv_created = true;
    }
 
    if (dstMaskValues != NULL) {
      dmv = (ESMCI::InterfaceInt *)(dstMaskValues->ptr);
      if(dmv->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- dstMaskValues array must be of rank 1", ESMC_CONTEXT, &rc);
         if (smv_created) delete smv;
         return ESMC_NULL_POINTER;
      }
      dmv_present = 1;
    } else {
      dmv = new ESMCI::InterfaceInt();
      dmv_created = true;
    }

    if (srcFracField != NULL) {
      sff_present = 1;
    } else {
      sff = new ESMCI::Field();
      sff_created = true;
    }

    if (dstFracField != NULL) {
      dff_present = 1;
    } else {
      dff = new ESMCI::Field();
      dff_created = true;
    }

    if (regridMethod != NULL)
      rm_present = 1;

    if (unmappedAction != NULL)
      ua_present = 1;

    if (polemethod != NULL)
      pm_present = 1;

    if (regridPoleNPnts != NULL)
      rpnp_present = 1;

    FTN_X(f_esmf_regridstore)(fieldpsrc, fieldpdst, 
                              smv->array, &smv->extent[0], &smv_present,
                              dmv->array, &dmv->extent[0], &dmv_present,
                              routehandlep,
                              regridMethod, &rm_present,
			      polemethod, &pm_present,
			      regridPoleNPnts, &rpnp_present,
                              unmappedAction, &ua_present,
                              srcFracField, &sff_present,
                              dstFracField, &dff_present, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) {
      if (smv_created) delete smv;
      if (dmv_created) delete dmv;
      if (sff_created) delete sff;
      if (dff_created) delete dff;
      return rc;
    }

    if (smv_created) delete smv;
    if (dmv_created) delete dmv;
    if (sff_created) delete sff;
    if (dff_created) delete dff;
    rc = ESMF_SUCCESS;
    return rc;
  }
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Field::regrid()"
//BOP
// !IROUTINE:  ESMCI::Field::regrid - compute a regridding operation
//
// !INTERFACE:
  int Field::regrid(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
    Field *fieldpsrc, 
    Field *fieldpdst, 
    RouteHandle *routehandlep,
    ESMC_Region_Flag *zeroRegion) {
//
// !DESCRIPTION:
//
//
//EOP
    // Initialize return code. Assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    int zr_present;
    
    zr_present = 0;
    if (zeroRegion != NULL)
      zr_present = 1;

    // TODO: why are fields.ptr and routehandle by reference??  from create.. 
    FTN_X(f_esmf_regrid)(fieldpsrc, fieldpdst, &routehandlep, 
                         zeroRegion, &zr_present, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;

    rc = ESMF_SUCCESS;
    return rc;
  }
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Field::regridrelease()"
//BOP
// !IROUTINE:  ESMCI::Field::regridrelase - release resources associated with a regridding operation
//
// !INTERFACE:
  int Field::regridrelease(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
    RouteHandle *routehandlep) {
//
// !DESCRIPTION:
//
//
//EOP
    // Initialize return code. Assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
  
    // TODO: why are fields.ptr and routehandle by reference??  from create.. 
    FTN_X(f_esmf_regridrelease)(&routehandlep, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;

    rc = ESMF_SUCCESS;
    return rc;
  }
//-----------------------------------------------------------------------------


} // namespace ESMCI
