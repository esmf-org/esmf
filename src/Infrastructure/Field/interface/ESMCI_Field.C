//$1.10 2007/04/26 16:13:59 rosalind Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
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
#include <string>         // strlen()

// ESMF headers
#include "ESMCI_LogErr.h"
#include "ESMCI_Array.h"
#include "ESMCI_Grid.h"
#include "ESMCI_LocStream.h"

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

void FTN_X(f_esmf_fieldcreatelocstreamas)(ESMCI::Field *fieldp, ESMCI::LocStream **locstream, 
    ESMC_ArraySpec *arrayspec, 
    int *gridToFieldMap, int *len1, int *gtfm_present, 
    int *ungriddedLBound, int *len2, int *uglb_present,
    int *ungriddedUBound, int *len3, int *ugub_present,
    char *name, int *rc,
    ESMCI_FortranStrLenArg nlen);

void FTN_X(f_esmf_fieldcreatelocstreamtk)(ESMCI::Field *fieldp, ESMCI::LocStream *locstream,
    ESMC_TypeKind_Flag *typekind,
    int *gridToFieldMap, int *len1, int *gtfm_present, 
    int *ungriddedLBound, int *len2, int *uglb_present,
    int *ungriddedUBound, int *len3, int *ugub_present,
    char *name, int *rc,
    ESMCI_FortranStrLenArg nlen);

void FTN_X(f_esmf_fielddestroy)(ESMCI::Field *fieldp, int *rc);

void FTN_X(f_esmf_fieldgetmesh)(ESMCI::Field *fieldp, void *mesh_pointer,
  int *rc);

void FTN_X(f_esmf_fieldgetarray)(ESMCI::Field *fieldp, void *array_pointer,
  int *rc);

void FTN_X(f_esmf_fieldgetbounds)(ESMCI::Field *fieldp, int *localDe,
  int *exclusiveLBound, int *len1,
  int *exclusiveUBound, int *len2,
  int *rc);

void FTN_X(f_esmf_fieldprint)(ESMCI::Field *fieldp, int *rc);

void FTN_X(f_esmf_fieldread)(ESMCI::Field *fieldp, const char *file,
  const char *variablename, int *timeslice, ESMC_IOFmt_Flag *iofmt,
  int *rc,
  ESMCI_FortranStrLenArg file_len,
  ESMCI_FortranStrLenArg variablename_len);

void FTN_X(f_esmf_fieldcast)(ESMCI::F90ClassHolder *fieldOut,
  ESMCI::Field *fieldIn, int *rc);

void FTN_X(f_esmf_regridgetarea)(ESMCI::Field *fieldp, int *rc);

void FTN_X(f_esmf_regridstore)(ESMCI::Field *fieldpsrc, ESMCI::Field *fieldpdst,
  int *srcMaskValues, int *len1,
  int *dstMaskValues, int *len2,
  ESMCI::RouteHandle **routehandlep,
  ESMC_RegridMethod_Flag *regridmethod,
  ESMC_PoleMethod_Flag *polemethod,
  int *regridPoleNPnts,
  ESMC_LineType_Flag *linetype,
  ESMC_NormType_Flag *normtype,
  ESMC_UnmappedAction_Flag *unmappedaction,
  ESMC_Logical *ignoreDegenerate,
  ESMCI::Field *srcfracfieldp,
  ESMCI::Field *dstfracfieldp,
  int *rc);

void FTN_X(f_esmf_regrid)(ESMCI::Field *fieldpsrc, ESMCI::Field *fieldpdst,
  ESMCI::RouteHandle **routehandlep, ESMC_Region_Flag *zeroregion, int *zr_present,
  int *rc);

void FTN_X(f_esmf_regridrelease)(ESMCI::RouteHandle **routehandlep, int *rc);

void FTN_X(f_esmf_fieldwrite)(ESMCI::Field *fieldp, const char *file,
  const char *variablename,
  ESMC_Logical *overwrite, ESMC_FileStatus_Flag *status,
  int *timeslice, ESMC_IOFmt_Flag *iofmt,
  int *rc,
  ESMCI_FortranStrLenArg file_len,
  ESMCI_FortranStrLenArg variablename_len);

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

    gtfm = (ESMCI::InterfaceInt *)gridToFieldMap;
    if (present(gtfm)) {
      if(gtfm->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- gridToFieldMap array must be of rank 1", ESMC_CONTEXT, rc);
         return ESMC_NULL_POINTER;
      }
      gtfm_present = 1;
    } else
      gtfm = new ESMCI::InterfaceInt();

    uglb = (ESMCI::InterfaceInt *)ungriddedLBound;
    if (present(uglb)) {
      if(uglb->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- ungriddedLBound array must be of rank 1", ESMC_CONTEXT, rc);
         return ESMC_NULL_POINTER;
      }
      uglb_present = 1;
    } else
      uglb = new ESMCI::InterfaceInt();

    ugub = (ESMCI::InterfaceInt *)ungriddedUBound;
    if (present(ugub)) {
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

    gtfm = (ESMCI::InterfaceInt *)gridToFieldMap;
    if (present(gtfm)) {
      if(gtfm->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- gridToFieldMap array must be of rank 1", ESMC_CONTEXT, rc);
         return ESMC_NULL_POINTER;
      }
      gtfm_present = 1;
    } else
      gtfm = new ESMCI::InterfaceInt();

    uglb = (ESMCI::InterfaceInt *)ungriddedLBound;
    if (present(uglb)) {
      if(uglb->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- ungriddedLBound array must be of rank 1", ESMC_CONTEXT, rc);
         return ESMC_NULL_POINTER;
      }
      uglb_present = 1;
    } else
      uglb = new ESMCI::InterfaceInt();

    ugub = (ESMCI::InterfaceInt *)ungriddedUBound;
    if (present(ugub)) {
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

    gtfm = (ESMCI::InterfaceInt *)gridToFieldMap;
    if (present(gtfm)) {
      if(gtfm->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- gridToFieldMap array must be of rank 1", ESMC_CONTEXT, rc);
         return ESMC_NULL_POINTER;
      }
      gtfm_present = 1;
    } else
      gtfm = new ESMCI::InterfaceInt();

    uglb = (ESMCI::InterfaceInt *)ungriddedLBound;
    if (present(uglb)) {
      if(uglb->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- ungriddedLBound array must be of rank 1", ESMC_CONTEXT, rc);
         return ESMC_NULL_POINTER;
      }
      uglb_present = 1;
    } else
      uglb = new ESMCI::InterfaceInt();

    ugub = (ESMCI::InterfaceInt *)ungriddedUBound;
    if (present(ugub)) {
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

    gtfm = (ESMCI::InterfaceInt *)gridToFieldMap;
    if (present(gtfm)) {
      if(gtfm->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- gridToFieldMap array must be of rank 1", ESMC_CONTEXT, rc);
         return ESMC_NULL_POINTER;
      }
      gtfm_present = 1;
    } else
      gtfm = new ESMCI::InterfaceInt();

    uglb = (ESMCI::InterfaceInt *)ungriddedLBound;
    if (present(uglb)) {
      if(uglb->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- ungriddedLBound array must be of rank 1", ESMC_CONTEXT, rc);
         return ESMC_NULL_POINTER;
      }
      uglb_present = 1;
    } else
      uglb = new ESMCI::InterfaceInt();

    ugub = (ESMCI::InterfaceInt *)ungriddedUBound;
    if (present(ugub)) {
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
#define ESMC_METHOD "ESMCI::Field::create()"
//BOP
// !IROUTINE:  ESMCI::Field::create - Create a new Field from LocStream and ArraySpec
//
// !INTERFACE:
      Field *Field::create(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMCI::Field object
//
// !ARGUMENTS:
    ESMC_LocStream *locstream, 
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

    gtfm = (ESMCI::InterfaceInt *)gridToFieldMap;
    if (present(gtfm)) {
      if(gtfm->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- gridToFieldMap array must be of rank 1", ESMC_CONTEXT, rc);
         return ESMC_NULL_POINTER;
      }
      gtfm_present = 1;
    } else
      gtfm = new ESMCI::InterfaceInt();

    uglb = (ESMCI::InterfaceInt *)ungriddedLBound;
    if (present(uglb)) {
      if(uglb->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- ungriddedLBound array must be of rank 1", ESMC_CONTEXT, rc);
         return ESMC_NULL_POINTER;
      }
      uglb_present = 1;
    } else
      uglb = new ESMCI::InterfaceInt();

    ugub = (ESMCI::InterfaceInt *)ungriddedUBound;
    if (present(ugub)) {
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

    // prepare the locstream pointer
    ESMCI::LocStream *locstreamp = reinterpret_cast<ESMCI::LocStream *>(locstream->ptr); 
 
    FTN_X(f_esmf_fieldcreatelocstreamas)(field, &locstreamp,
        &arrayspec,
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
// !IROUTINE:  ESMCI::Field::create - Create a new Field from LocStream and typekind
//
// !INTERFACE:
      Field *Field::create(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMCI::Field object
//
// !ARGUMENTS:
    ESMC_LocStream *locstream, 
    ESMC_TypeKind_Flag typekind, 
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

    gtfm = (ESMCI::InterfaceInt *)gridToFieldMap;
    if (present(gtfm)) {
      if(gtfm->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- gridToFieldMap array must be of rank 1", ESMC_CONTEXT, rc);
         return ESMC_NULL_POINTER;
      }
      gtfm_present = 1;
    } else
      gtfm = new ESMCI::InterfaceInt();

    uglb = (ESMCI::InterfaceInt *)ungriddedLBound;
    if (present(uglb)) {
      if(uglb->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- ungriddedLBound array must be of rank 1", ESMC_CONTEXT, rc);
         return ESMC_NULL_POINTER;
      }
      uglb_present = 1;
    } else
      uglb = new ESMCI::InterfaceInt();

    ugub = (ESMCI::InterfaceInt *)ungriddedUBound;
    if (present(ugub)) {
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
  
    // prepare the LocStream pointer
    ESMCI::LocStream *locstreamp = reinterpret_cast<ESMCI::LocStream *>(locstream->ptr); 

    FTN_X(f_esmf_fieldcreatelocstreamtk)(field, locstreamp, 
        &typekind,
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
#define ESMC_METHOD "ESMCI::Field::getbounds()"
//BOP
// !IROUTINE:  ESMCI::Field::getbounds - get Field bounds
//
// !INTERFACE:
      int Field::getbounds(
//
// !RETURN VALUE:
//    int error return code

// !ARGUMENTS:
  Field *field,
  int *localDe,
  ESMCI::InterfaceInt *exLB,
  ESMCI::InterfaceInt *exUB){

// !DESCRIPTION:
//      ESMF routine to return bounds from the Field.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC\_Field.h)
//
//EOP
// !REQUIREMENTS:

    // Initialize return code. Assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;

    FTN_X(f_esmf_fieldgetbounds)(field, localDe, exLB->array, &(exLB->extent[0]),
      exUB->array, &exUB->extent[0], &localrc);

    localrc = ESMF_SUCCESS;

    return localrc;

 }

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
//EOP

    // Initialize return code. Assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;
    int rc=ESMC_RC_NOT_IMPL;

    // Invoke the fortran interface through the F90-C++ "glue" code
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
#define ESMC_METHOD "ESMCI::Field::read()"
//BOPI
// !IROUTINE:  ESMCI::Field::read - read external data into a field

// !INTERFACE:
  int Field::read(const char *file,
      const char* variableName,
      int timeslice, ESMC_IOFmt_Flag iofmt){

// !RETURN VALUE:
//    int error return code
  
// !ARGUMENTS:
//   none

//  !DESCRIPTION
//    Reads external data into a {\tt field}.
//EOPI

    // Initialize return code. Assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;
    int rc=ESMC_RC_NOT_IMPL;

    // Invoke the fortran interface through the F90-C++ "glue" code
    std::string file_local = file;
    std::string variableName_local = variableName;
    FTN_X(f_esmf_fieldread)(this, file, variableName,
        &timeslice, &iofmt, &localrc,
        file_local.size(), variableName_local.size());
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

    // Invoke the fortran interface through the F90-C++ "glue" code
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
    ESMC_LineType_Flag *lineType,
    ESMC_NormType_Flag *normType,
    ESMC_UnmappedAction_Flag *unmappedAction,
    ESMC_Logical *ignoreDegenerate,
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

    bool sff_created, dff_created;
    sff_created = false;
    dff_created = false;
    ESMCI::InterfaceInt *smv, *dmv;
    ESMCI::Field *sff, *dff;
    int *srcMaskArray,*dstMaskArray;
    int srcMaskLen,dstMaskLen;


    smv = (ESMCI::InterfaceInt *)srcMaskValues;
    if (present(smv)) {
      if(smv->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- srcMaskValues array must be of rank 1", ESMC_CONTEXT, &rc);
         return ESMC_NULL_POINTER;
      }
      srcMaskArray=smv->array;
      srcMaskLen=smv->extent[0];
    } else {
      srcMaskArray=NULL;
      srcMaskLen=0;
    }
 
    dmv = (ESMCI::InterfaceInt *)dstMaskValues;
    if (present(dmv)) {
      if(dmv->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- dstMaskValues array must be of rank 1", ESMC_CONTEXT, &rc);
         return ESMC_NULL_POINTER;
      }
      dstMaskArray=dmv->array;
      dstMaskLen=dmv->extent[0];
    } else {
      dstMaskArray=NULL;
      dstMaskLen=0;
    }

    if (srcFracField == NULL) {
      sff = new ESMCI::Field();
      sff_created = true;
    }

    if (dstFracField == NULL) {
      dff = new ESMCI::Field();
      dff_created = true;
    }

    FTN_X(f_esmf_regridstore)(fieldpsrc, fieldpdst, 
                              srcMaskArray, &srcMaskLen,
                              dstMaskArray, &dstMaskLen,
                              routehandlep,
                              regridMethod,
                              polemethod,
                              regridPoleNPnts,
                              lineType,
                              normType,
                              unmappedAction,
                              ignoreDegenerate,
                              srcFracField,
                              dstFracField,
                              &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) {
      if (sff_created) delete sff;
      if (dff_created) delete dff;
      return rc;
    }
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

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Field::write()"
//BOPI
// !IROUTINE:  ESMCI::Field::write - write Field data to an external file

// !INTERFACE:
  int Field::write(const char *file,
      const char* variableName,
      int overwrite,
      ESMC_FileStatus_Flag status,
      int timeslice, ESMC_IOFmt_Flag iofmt){

// !RETURN VALUE:
//    int error return code
  
// !ARGUMENTS:
//   none

//  !DESCRIPTION
//    Writes {\tt field} data into an external file.
//EOPI

    // Initialize return code. Assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;
    int rc=ESMC_RC_NOT_IMPL;

    // Invoke the fortran interface through the F90-C++ "glue" code
    std::string file_local = file;
    ESMC_Logical overwrite_local = (overwrite != 0) ? ESMF_TRUE:ESMF_FALSE;
    if (variableName) {
      std::string variableName_local = variableName;
      FTN_X(f_esmf_fieldwrite)(this, file, variableName,
          &overwrite_local, &status,
          &timeslice, &iofmt, &localrc,
          file_local.size(), variableName_local.size());
    } else {
      FTN_X(f_esmf_fieldwrite)(this, file, NULL,
          &overwrite_local, &status,
          &timeslice, &iofmt, &localrc,
          file_local.size(), 0);
    }
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;

    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
//-----------------------------------------------------------------------------


} // namespace ESMCI
