// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_Grid.C"
//==============================================================================
//
// ESMC Grid method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ Array methods declared
// in the companion file ESMCI_Grid.h
//
//-----------------------------------------------------------------------------
// include associated header file
#include "ESMCI_Grid.h"


// include higher level, 3rd party or system headers
#include <stdio.h>
#include <string.h>
#include <cmath>

// include ESMF headers
#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_CoordSys.h"
#include "ESMCI_GridToMesh.h"
#include "Mesh/include/ESMCI_MeshCap.h"
#include "Mesh/include/ESMCI_Mesh.h"
#include "Mesh/include/Legacy/ESMCI_MeshRead.h"


// Some xlf compilers don't define this
#ifndef M_PI
#define M_PI 3.14159265358979323846
 #endif

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

#define VERBOSITY             (1)       // 0: off, 10: max

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// Prototypes of the C->Fortran interface functions.
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
extern "C" {
void FTN_X(f_esmf_gridcreatenoperidim)(ESMCI::Grid **grid,
    int *maxIndex, int *len1, 
    ESMC_CoordSys_Flag *coordSys, int *cs_present,
    ESMC_TypeKind_Flag *coordTypeKind, int *ctk_present,
    ESMC_IndexFlag *indexflag, int *rc);

void FTN_X(f_esmf_gridcreate1peridim)(ESMCI::Grid **grid,
    int *maxIndex, int *len1, 
    int *poleKind, int *len2,
    int *periodicDim,
    int *poleDim,
    ESMC_CoordSys_Flag *coordSys,
    ESMC_TypeKind_Flag *coordTypeKind,
    ESMC_IndexFlag *indexflag, int *rc);

void FTN_X(f_esmf_gridcreatecubedsphere)(ESMCI::Grid **grid,
    int *tilesize,
    int *regDecompPTile, int *len11, int *len12, int *rdpresent,
    int *decompFlagPTile, int *len21, int *len22, int *dfpresent,
    int *deLabelList, int *len3, int *llpresent,
    //ESMC_DELayout *delayout,
    int *staggerLocList, int *len4,
    const char *name,
    int *rc,
    ESMCI_FortranStrLenArg len_name);

void FTN_X(f_esmf_gridcreatefromfile)(ESMCI::Grid **grid,
    const char *filename, int *fileTypeFlag,
    int *regDecomp,
    int *decompflag,
    int *isSphere, 
    int *polekind, int *len1,
    int *addCornerStagger,
    int *addUserArea,
    ESMC_IndexFlag *indexflag,
    int *addMask,
    const char *varname,
    const char *coordNames, int *rc,
    ESMCI_FortranStrLenArg len_filename,
    ESMCI_FortranStrLenArg len_varname,
    ESMCI_FortranStrLenArg len_coordNames);

}

//


//-----------------------------------------------------------------------------


// Set up ESMCI name space for these methods
namespace ESMCI{  


  int grid_debug=false;

  void _create_nopole_distgrid(DistGrid *distgrid, DistGrid **distgrid_nopole, int *rc);
  void _translate_distgrid_conn(DistGrid *distgrid, 
                                ESMC_GridConn *connL, ESMC_GridConn *connU, int *rc);
  void _add_poles_to_conn(DistGrid *distgrid, int *lwidth, int *uwidth, 
                        ESMC_GridConn *connL, ESMC_GridConn *connU,
                        InterArray<int> **connListOut, int *rc);

  
//  File Local Prototypes (actual implementation at end of file)
static int _NumStaggerLocsFromDimCount(int dimCount);

template <class Type>
static  Type **_allocate2D(int sizeDim1, int sizeDim2);

template <class Type>
static  void _free2D(Type ***array);

template <class Type>
static  Type ***_allocate3D(int sizeDim1, int sizeDim2, int sizeDim3);

template <class Type>
static  void _free3D(Type ****array);

static InterArray<int> *_copyInterArray(InterArray<int> *in);
 
static void _freeInterArray(InterArray<int> **in);

static int _createIsDEBnd(char **_isDELBnd, char **_isDEUBnd, 
                          DistGrid *distgrid,int *distgridToGridMap);

int construct(Grid *_grid, int _nameLen, char *_name, ESMC_TypeKind_Flag *_typekind,
              DistGrid *_distgrid, InterArray<int> *_gridEdgeLWidth, 
              InterArray<int> *_gridEdgeUWidth, InterArray<int> *_gridAlign,
              InterArray<int> *_distgridToGridMap,
              InterArray<int> *_undistLBound, InterArray<int> *_undistUBound, 
              ESMC_CoordSys_Flag *coordSys, 
              InterArray<int> *_coordDimCount, InterArray<int> *_coordDimMap,
          InterArray<int> *_gridMemLBound,
              ESMC_IndexFlag *_indexflag,
              bool destroyDistgrid,
              bool destroyDELayout);

int construct(Grid *_grid, int _nameLen, char *_name, ESMC_TypeKind_Flag *_typekind,
              DistGrid *_distgrid, 
              InterArray<int> *_minIndex, InterArray<int> *_maxIndex,
          InterArray<int> *_localArbIndex, int localArbIndexCount,
              InterArray<int> *_distDim, int arbDim, 
              InterArray<int> *_undistLBound, InterArray<int> *_undistUBound, 
              ESMC_CoordSys_Flag *coordSys, 
              InterArray<int> *_coordDimCount, InterArray<int> *_coordDimMap,
          InterArray<int> *_gridMemLBound,
              ESMC_IndexFlag *_indexflag,
              bool destroyDistgrid, bool destroyDELayout);

int setDefaultsLUA(int dimCount,
                   InterArray<int> *lWidthIn, InterArray<int> *uWidthIn,
                   InterArray<int> *alignIn,
                   int *lWidthDefault, int *uWidthDefault, int *alignDefault, 
                   int *lWidthOut, int *uWidthOut, int *alignOut);


//-----------------------------------------------------------------------------
//
// Public Interfaces
//
//-----------------------------------------------------------------------------

// the following two routine are for the ESMC->Fortran interface

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::createnoperidim()"
//BOP
// !IROUTINE:  ESMCI::Grid::create - Create a new Grid
//
// !INTERFACE:
       Grid* Grid::createnoperidim(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMCI::Grid object
//
// !ARGUMENTS:
    ESMC_InterArrayInt *maxIndex, 
    ESMC_CoordSys_Flag *coordSys,
    ESMC_TypeKind_Flag *coordTypeKind,
    ESMC_IndexFlag *indexflag,
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Create a new Grid.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC\_Grid.h)
//
//EOP
    // Initialize return code. Assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;
    if(rc!=NULL) *rc=ESMC_RC_NOT_IMPL;
  
    int cs_present, ctk_present;
    cs_present = 0;
    ctk_present = 0;

    ESMCI::InterArray<int> *mi = (ESMCI::InterArray<int> *)maxIndex;
  

    // this is a test to see if the data is passed in correctly
    /*printf("Grid::createnoperidim - array = [");
    for (int i=0; i<mi->extent[0]; ++i)
      printf("%d,", mi->array[i]);
    printf("]\nextent = [");
    for (int i=0; i<7; ++i)
        printf("%d,", mi->extent[i]);
    printf("]\nlength = %d\n", mi->extent[0]);
    printf("dimCount = %d\n", mi->dimCount);*/


    if(mi->dimCount != 1){
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
         "- maxIndex array must be of rank 1", ESMC_CONTEXT, rc);
       return ESMC_NULL_POINTER;
    }

    // handle the optional arguments
    if (coordSys != NULL)
      cs_present = 1;
    if (coordTypeKind != NULL)
      ctk_present = 1; 

    // allocate the grid object
    Grid *grid;

    FTN_X(f_esmf_gridcreatenoperidim)(&grid,
                                      mi->array, &mi->extent[0],
                                      coordSys, &cs_present,
                                      coordTypeKind, &ctk_present,
                                      indexflag, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return ESMC_NULL_POINTER;
  
    if (rc) *rc = localrc;
  
    return grid;

 }

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::create1peridim()"
//BOP
// !IROUTINE:  ESMCI::Grid::create - Create a new Grid
//
// !INTERFACE:
      Grid* Grid::create1peridim(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMCI::Grid object
//
// !ARGUMENTS:
    ESMC_InterArrayInt *maxIndex, 
    ESMC_InterArrayInt *polekindflag,
    int *periodicDim,
    int *poleDim,
    ESMC_CoordSys_Flag *coordSys,
    ESMC_TypeKind_Flag *coordTypeKind,
    ESMC_IndexFlag *indexflag,
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Create a new Grid.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC\_Grid.h)
//
//EOP
    // Initialize return code. Assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;
    if(rc!=NULL) *rc=ESMC_RC_NOT_IMPL;

    int *pkfArray;
    int pkfLen;

    ESMCI::InterArray<int> *mi = (ESMCI::InterArray<int> *)maxIndex;
  
    if(mi->dimCount != 1){
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
         "- maxIndex array must be of rank 1", ESMC_CONTEXT, rc);
       return ESMC_NULL_POINTER;
    }

    ESMCI::InterArray<int> *pkf = (ESMCI::InterArray<int> *)polekindflag;
    if (pkf != NULL) {
      if(pkf->dimCount != 1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "- polekindflag array must be of rank 1", ESMC_CONTEXT, rc);
          return ESMC_NULL_POINTER;
      }

      if(pkf->extent[0] != 2){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "- polekindflag array must hold 2 values", ESMC_CONTEXT, rc);
          return ESMC_NULL_POINTER;
      }
      pkfArray = pkf->array;
      pkfLen = pkf->extent[0];
    } else {
      pkfArray = NULL;
      pkfLen = 0;
    }


    // handle the optional arguments
    int periodicDimLoc = 1;
    if (periodicDim != NULL) {
      periodicDimLoc = *periodicDim;
    }
    int poleDimLoc = 2;
    if (poleDim != NULL) {
      poleDimLoc = *poleDim;
    }
    // allocate the grid object
    Grid *grid;
  
    FTN_X(f_esmf_gridcreate1peridim)(&grid,
                                     mi->array, &mi->extent[0],
                                     pkfArray, &pkfLen, 
                                     &periodicDimLoc, 
                                     &poleDimLoc, 
                                     coordSys, 
                                     coordTypeKind, 
                                     indexflag, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return grid;
  
    if (rc) *rc = localrc;
  
    return grid;

 }

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::createcubedsphere()"
//BOP
// !IROUTINE:  ESMCI::Grid::createcubedsphere - Create a new cubed sphere Grid
//
// !INTERFACE:
      Grid* Grid::createcubedsphere(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMCI::Grid object
//
// !ARGUMENTS:
    int *tilesize,
    ESMC_InterArrayInt *regDecompPTile,
    ESMC_InterArrayInt *decompFlagPTile,
    ESMC_InterArrayInt *deLabelList,
    //ESMC_DELayout *delayout,
    ESMC_InterArrayInt *staggerLocList,
    const char *name,
    int *rc) {
//
// !DESCRIPTION:
//      Create a new Grid.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC\_Grid.h)
//
//EOP
    // Initialize return code. Assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;
    if(rc!=NULL) *rc=ESMC_RC_NOT_IMPL;
    int rdpresent = 0, dfpresent = 0, llpresent = 0;
    int *rdarray=NULL, *dfarray=NULL, *llarray=NULL, *ssarray=NULL;
    int rdlen1, rdlen2, dflen1, dflen2, lllen, nlen, sslen;

    ESMCI::InterArray<int> *rd = (ESMCI::InterArray<int> *)regDecompPTile;
    if (present(rd)) {
      if(rd->dimCount != 2){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- regDecompPTile array must be of rank 2", ESMC_CONTEXT, rc);
         return ESMC_NULL_POINTER;
      }
      rdpresent = 1;
      rdarray=rd->array;
      rdlen1=rd->extent[0];
      rdlen2=rd->extent[1];

      /*// this is a test to see if the data is passed in correctly
      printf("\nregDecompPTile:\n  array = [");
      for (int i=0; i<rd->extent[0]; ++i)
        for (int j=0; j<rd->extent[1]; ++j)
          printf("%d,", rd->array[i*2+j]);
      printf("]\n  extent = [");
      for (int i=0; i<7; ++i)
          printf("%d,", rd->extent[i]);
      printf("]\n  dimCount = %d\n", rd->dimCount);*/

    } else {
      rdarray=NULL;
      rdlen1=0;
      rdlen2=0;
    }

    ESMCI::InterArray<int> *df = (ESMCI::InterArray<int> *)decompFlagPTile;
    if (present(df)) {
      if(df->dimCount != 2){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- decompFlagPTile array must be of rank 2", ESMC_CONTEXT, rc);
         return ESMC_NULL_POINTER;
      }
      dfpresent = 1;
      dfarray=df->array;
      dflen1=df->extent[0];
      dflen2=df->extent[1];

      /*// this is a test to see if the data is passed in correctly
      printf("\ndecompFlagPTile:\n  array = [");
      for (int i=0; i<df->extent[0]; ++i)
        for (int j=0; j<df->extent[1]; ++j)
          printf("%d,", df->array[i*2+j]);
      printf("]\n  extent = [");
      for (int i=0; i<7; ++i)
          printf("%d,", df->extent[i]);
      printf("]\n  dimCount = %d\n", df->dimCount);*/

    } else {
      dfarray=NULL;
      dflen1=0;
      dflen2=0;
    }

    ESMCI::InterArray<int> *ll = (ESMCI::InterArray<int> *)deLabelList;
    if (present(ll)) {
      if(ll->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- deLabelList array must be of rank 1", ESMC_CONTEXT, rc);
         return ESMC_NULL_POINTER;
      }
      llpresent = 1;
      llarray=ll->array;
      lllen=ll->extent[0];

      /*// this is a test to see if the data is passed in correctly
      printf("\ndeLabelList:\n  array = [");
      for (int i=0; i<ll->extent[0]; ++i)
        printf("%d,", ll->array[i]);
      printf("]\n  extent = [");
      for (int i=0; i<7; ++i)
          printf("%d,", ll->extent[i]);
      printf("]\n  dimCount = %d\n", ll->dimCount);*/

    } else {
      llarray=NULL;
      lllen=0;
    }

    ESMCI::InterArray<int> *ss = (ESMCI::InterArray<int> *)staggerLocList;
    if (present(ss)) {
      if(ss->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- staggerLocList array must be of rank 1", ESMC_CONTEXT, rc);
         return ESMC_NULL_POINTER;
      }
      ssarray=ss->array;
      sslen=ss->extent[0];

      /*// this is a test to see if the data is passed in correctly
      printf("\nstaggerLocList:\n  array = [");
      for (int i=0; i<ss->extent[0]; ++i)
        printf("%d,", ss->array[i]);
      printf("]\n  extent = [");
      for (int i=0; i<7; ++i)
          printf("%d,", ss->extent[i]);
      printf("]\n  dimCount = %d\n", ss->dimCount);*/

    } else {
      ssarray=NULL;
      sslen=0;
    }

    if (name) nlen = strlen(name);
    else nlen = 0;

    Grid *grid;
    FTN_X(f_esmf_gridcreatecubedsphere)(&grid,
        tilesize,
        rdarray, &rdlen1, &rdlen2, &rdpresent,
        dfarray, &dflen1, &dflen2, &dfpresent,
        llarray, &lllen, &llpresent,
        ssarray, &sslen,
        name,
        &localrc, nlen);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return grid;

    if (rc) *rc = localrc;

    return grid;

 }

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::createfromfile()"
//BOP
// !IROUTINE:  ESMCI::Grid::createfromfile - Create a new Grid from file
//
// !INTERFACE:
      Grid* Grid::createfromfile(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMCI::Grid object
//
// !ARGUMENTS:
    const char *filename,
    int fileTypeFlag,
    int *regDecomp,
    int *decompflag,
    int *isSphere,
    ESMC_InterArrayInt *polekindflag,
    int *addCornerStagger,
    int *addUserArea,
    ESMC_IndexFlag *indexflag,
    int *addMask,
    const char *varname,
    const char **coordNames,
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Create a new Grid from NetCDF file.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC\_Grid.h)
//
//EOP

    //printf ("Start ESMCI_Grid.C : createfromfile(%s,%d)\n", filename, fileTypeFlag);

    // Initialize return code. Assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;
    if(rc!=NULL) *rc=ESMC_RC_NOT_IMPL;


    // handle the optional arguments
    int *df_loc = NULL;
    int *rd_loc = NULL;
    char *vn_loc = (char *)"";
    int vn_len = 0;

    if (regDecomp != NULL) {
      rd_loc = regDecomp;
    }
    if (decompflag != NULL) {
      df_loc = decompflag;
    }
    int is_loc = 1;
    if (isSphere != NULL) {
      is_loc = *isSphere;
    }
    int acs_loc = 0;
    if (addCornerStagger != NULL) {
      acs_loc = *addCornerStagger;
    }
    int aua_loc = 0;
    if (addUserArea != NULL) {
      aua_loc = *addUserArea;
    }
    int am_loc = 0;
    if (addMask != NULL) {
      am_loc = *addMask;
    }
    if (varname != NULL) {
      vn_len = strlen(varname);
      if (vn_len > 0) {
        vn_loc = (char *)malloc(vn_len);
        strncpy(vn_loc, varname, vn_len);
      }
    }

    int *pkfArray;
    int pkfLen;
    
    ESMCI::InterArray<int> *pkf = (ESMCI::InterArray<int> *)polekindflag;
    if (pkf != NULL) {
      if(pkf->dimCount != 1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "- polekindflag array must be of rank 1", ESMC_CONTEXT, rc);
          return ESMC_NULL_POINTER;
      }

      if(pkf->extent[0] != 2){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "- polekindflag array must hold 2 values", ESMC_CONTEXT, rc);
          return ESMC_NULL_POINTER;
      }
      pkfArray = pkf->array;
      pkfLen = pkf->extent[0];
    } else {
      pkfArray = NULL;
      pkfLen = 0;
    }


    // Create Fortran-style buffer to pass coordNames to Fortran if coordNames are present.
    int cn_len = 0;
    char *cn_buf=NULL;
    if (coordNames) {
      int cn_len0=0, cn_len1=0;
      if (coordNames[0]) {
    cn_len0 = strlen(coordNames[0]);
      }
      if (coordNames[1]) {
    cn_len1 = strlen(coordNames[1]);
      }
      cn_len = (cn_len0 >= cn_len1) ? cn_len0 : cn_len1;
      int fortran_buf_size = 2 * cn_len;
      cn_buf = (char *)malloc(fortran_buf_size);
      memset(cn_buf, ' ', fortran_buf_size);
      strncpy(&cn_buf[0], coordNames[0], cn_len);
      strncpy(&cn_buf[cn_len], coordNames[1], cn_len);
    }

    // allocate the grid object
    Grid *grid;
    FTN_X(f_esmf_gridcreatefromfile)(&grid, filename, &fileTypeFlag,
                     rd_loc, df_loc, &is_loc, pkfArray, &pkfLen, 
                     &acs_loc, &aua_loc, 
                     indexflag, &am_loc, vn_loc, cn_buf, &localrc,
                     strlen(filename), vn_len, cn_len);
    if (vn_loc && (vn_len > 0)) {
      free(vn_loc);
    }
    if (cn_buf) {
      free(cn_buf);
    }
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return grid;

    if (rc) *rc = localrc;

    //printf ("End ESMCI_Grid.C : createfromfile()\n");

    return grid;
 }

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::write()"
//BOP
// !IROUTINE:  ESMCI::Grid::write - Write a grid in vtk format
//
// !INTERFACE:
      int Grid::write(
//
// !RETURN VALUE:
//     return value
//
// !ARGUMENTS:
    ESMC_StaggerLoc staggerloc,
    const char *fname) {
//
// !DESCRIPTION:
//      Write a Grid
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC\_Grid.h)
//
//      Note2:  this code is duplicated in ESMCI_GridUtil_F.C in gridio
//
//EOP

   // Initialize return code. Assume routine not implemented
   int localrc = ESMC_RC_NOT_IMPL;
    int rc = ESMC_RC_NOT_IMPL;

  ESMCI::Grid &gridp = *this;

  // Temp. empty array of Arrays
  std::vector<ESMCI::Array*> arrays;


  // Convert Grid to Mesh
  int regridConserve = 0; // ESMC_REGRID_CONSERVE_OFF;
  MeshCap *meshp=MeshCap::GridToMesh(gridp, staggerloc, 
                                     arrays,
                                     NULL,
                                     &regridConserve, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  
  
  char *non_const_fname = const_cast<char *>(fname);
  int nlen=strlen(non_const_fname);
  meshp->meshwrite(non_const_fname, &localrc, nlen);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // Get rid of Mesh, also considering garbage collection
  localrc = MeshCap::destroy(&meshp, true);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return success
  return ESMF_SUCCESS;
 }

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::addCoordArray()"
//BOP
// !IROUTINE:  addCoordArray
//
// !INTERFACE:
int Grid::addCoordArray(

//
// !RETURN VALUE:
//   Return code
//
// !ARGUMENTS:
//
                          int *staggerlocArg,             // (in) optional
                          InterArray<int> *staggerEdgeLWidthArg, // (in) optional
                          InterArray<int> *staggerEdgeUWidthArg, // (in) optional
                          InterArray<int> *staggerAlignArg,   // (in) optional 
                          InterArray<int> *staggerMemLBoundArg   // (in) optional 
  ) {
//
// !DESCRIPTION:
//   Create the storage (ESMF Array objects) to hold the coordinates for a
// stagger location. Note that this subroutine creates an Array for each 
// coordinate component in the stagger location. The Arrays are, of course,
// created with the correct size and shape to hold the coordinates. 
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int rc, localrc;
  int staggerloc;
  int coord;
  DataCopyFlag docopy;
  const int *distgridToArrayMap;
  Array *array;
  DistGrid *staggerDistgrid;
  int extent[1];

  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  rc = ESMC_RC_NOT_IMPL;
  
  // Make sure the grid has the correct status for this action
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
      "- grid not of correct status to perform this operation", ESMC_CONTEXT,
      &rc);
    return rc;
  }  

  // If staggerloc hasn't been passed in use a default, otherwise copy it. 
  if (staggerlocArg==NULL) {
    staggerloc=0;  // default
  } else {
    staggerloc=*staggerlocArg;
  }

  // Check staggerloc
  if ((staggerloc < 0) || (staggerloc >= staggerLocCount)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- stagger location out of range", ESMC_CONTEXT, &rc);
    return rc;
  }


  // Error check and then set information about this stagger's bounds in the staggerLists
  localrc=this->setStaggerInfo(staggerloc, staggerEdgeLWidthArg, staggerEdgeUWidthArg,
                   staggerAlignArg, staggerMemLBoundArg);
  if (ESMC_LogDefault.MsgFoundError(localrc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;        


  // Get distgrid for this staggerloc 
  localrc=this->getStaggerDistgrid(staggerloc, &staggerDistgrid);
  if (ESMC_LogDefault.MsgFoundError(localrc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;        

  // construct ArraySpec for using to call Array::create
  ArraySpec *arrayspec= new ArraySpec;     
    
  int *distgridToArrayMapIntIntArray=new int[dimCount];
  extent[0]=dimCount;
  InterArray<int> *distgridToArrayMapIntInt =
    new InterArray<int>(distgridToArrayMapIntIntArray,1,extent); 

  InterArray<int> *staggerMemLBoundIntInt =
    (InterArray<int> *)ESMC_NULL_POINTER;
  int *staggerMemLBoundIntIntArray=(int *)ESMC_NULL_POINTER;

  // Only setup membounds if index flag is user
  if (indexflag==ESMC_INDEX_USER) {
    staggerMemLBoundIntIntArray=new int[dimCount];
    extent[0]=dimCount;
    staggerMemLBoundIntInt = 
      new InterArray<int>(staggerMemLBoundIntIntArray,1,extent); 
  }


  ////////////
  ///// Loop Constructing all the coordinate arrays
  ///////////
  for (coord=0; coord<dimCount; coord++) {
    
    // fill in ArraySpec with information describing coordinate
    arrayspec->set(coordDimCount[coord], typekind);
   
    //// Initialize distgridToArrayMap array to 0 to make all unspecified dimensions
    //// replicated
    for (int i=0; i<dimCount; i++) {
      distgridToArrayMapIntIntArray[i]=0;
    }

    //// fill in distgridToArrayMap, undistLBound, and undistUBound for use in Array::create
    //// distgridToArrayMap - computed by inverting how coords dims map to distgrid
    //// bounds - computed by matching up an undistributed coord dimension
    ////          with the bound values at the corresponding grid dimension 
    int coordDistDimCount=0;
    for (int i=0; i<coordDimCount[coord]; i++) {
      if (coordIsDist[coord][i]) {
        distgridToArrayMapIntIntArray[coordMapDim[coord][i]]=i+1; // convert to 1-based
        coordDistDimCount++;
      } 
    }

    // (needs to be total dimCount of distGrid even if coord dimCount < distgrid dimCount)
    // (0's indicate unused dimensions)
    distgridToArrayMapIntInt->extent[0]=dimCount;
    

    /* DEBUG 
    printf("compUWidth=");
    for (int i=0; i<coordDistDimCount; i++) {
      printf("%d ", compUWidthIntIntArray[i]);
    }
    printf("\n");

    printf("compLWidth=");
    for (int i=0; i<coordDistDimCount; i++) {
      printf("%d ", compLWidthIntIntArray[i]);
    }
    printf("\n");
      DEBUG */

    //// Optionally fix the lower memory bounds of each DE's memory chunk
    if (indexflag==ESMC_INDEX_USER) {
      // Set size of array based on coord dim
      staggerMemLBoundIntInt->extent[0]=coordDimCount[coord];

      // Fill Array
      int j=0;
      for (int i=0; i<coordDimCount[coord]; i++) {
    int gi=coordDimMap[coord][i];
    if (coordIsDist[coord][i]) {
      staggerMemLBoundIntIntArray[j] = staggerMemLBoundList[staggerloc][gi];
      j++;
    } 
      }
    }


    // Create an Array to hold the coords 
    array=Array::create(arrayspec, staggerDistgrid,
                          distgridToArrayMapIntInt,
                          (InterArray<int> *)ESMC_NULL_POINTER,
                          (InterArray<int> *)ESMC_NULL_POINTER,
                          (InterArray<int> *)ESMC_NULL_POINTER,
                          (InterArray<int> *)ESMC_NULL_POINTER,
                          (InterArray<int> *)ESMC_NULL_POINTER,
                          (InterArray<int> *)ESMC_NULL_POINTER,
                          &indexflag, NULL, staggerMemLBoundIntInt, 
                          (InterArray<int> *)ESMC_NULL_POINTER,
                          (InterArray<int> *)ESMC_NULL_POINTER, 
                          &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;        

    // Set newly created Array into Grid
    localrc=this->setCoordArrayInternal(staggerloc, coord, array, true);
    if (ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;        
    
  } // end of coord loop



  // Dellocate temporay arrays
  delete arrayspec;     
  delete [] distgridToArrayMapIntIntArray;
  delete distgridToArrayMapIntInt;
  if (indexflag==ESMC_INDEX_USER) {
    delete [] staggerMemLBoundIntIntArray;
    delete staggerMemLBoundIntInt;
  }


  // return ESMF_SUCCESS
  return ESMF_SUCCESS;
  }
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::addCoordArrayArb()"
//BOP
// !IROUTINE:  addCoordArrayArb
//
// !INTERFACE:
int Grid::addCoordArrayArb(

//
// !RETURN VALUE:
//   Return code
//
// !ARGUMENTS:
//
                          int *staggerlocArg             // (in) optional
  ) {
//
// !DESCRIPTION:
//   Create the storage (ESMF Array objects) to hold the coordinates for a
// stagger location for an arbitrarily distributed grid. Only STAGGER_CENTER is
// supported. Note that this subroutine creates an Array for each 
// coordinate component in the stagger location. The Coordinate Arrays are
// always 1D arrays regardless the dimension of the grid.
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int rc, localrc;
  int staggerloc;
  int coord;
  const int *distgridToArrayMap;
  Array *array;
  int extent[1];

  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  rc = ESMC_RC_NOT_IMPL;
  
  // Make sure the grid has the correct status for this action
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
      "- grid not of correct status to perform this operation", ESMC_CONTEXT,
      &rc);
    return rc;
  }  


  // If staggerloc hasn't been passed in use a default, otherwise copy it. 
  if (staggerlocArg==NULL) {
    staggerloc=0;  // default
  } else {
    staggerloc=*staggerlocArg;
  }

  // Check staggerloc
  if ((staggerloc < 0) || (staggerloc >= staggerLocCount)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- stagger location out of range", ESMC_CONTEXT, &rc);
    return rc;
  }

  // construct ArraySpec for using to call Array::create
  ArraySpec *arrayspec= new ArraySpec;     
 
  int *distgridToArrayMapIntIntArray=new int[dimCount];
  extent[0]=dimCount;
  InterArray<int> *distgridToArrayMapIntInt =
    new InterArray<int>(distgridToArrayMapIntIntArray,1,extent); 
   
  ////////////
  ///// Loop Constructing all the coordinate arrays
  ///////////
  for (coord=0; coord<dimCount; coord++) {
    
    // fill in ArraySpec with information describing coordinate
    arrayspec->set(coordDimCount[coord], typekind);

    //// Initialize distgridToArrayMap array to 0 to make all unspecified dimensions
    //// replicated
    for (int i=0; i<dimCount; i++) {
      distgridToArrayMapIntIntArray[i]=0;
    }

    //// fill in distgridToArrayMap for use in Array::create
    //// distgridToArrayMap - computed by inverting how coords dims map to distgrid
    for (int i=0; i<coordDimCount[coord]; i++) {
      if (distgrid->getDELayout()->getLocalDeCount()){
        // only do this for real when there are localDEs
        if (coordDimMap[coord][i] == ESMC_GRID_ARBDIM) 
          distgridToArrayMapIntIntArray[coordMapDim[coord][i]]=arbDim; // convert to 1-based
        else {
          distgridToArrayMapIntIntArray[coordMapDim[coord][i]]=i+1; // convert to 1-based
        }
      }else{
        // otherwise provide default sequence 1,2,3,...coordDimCount[coord] to satisfy Array
        distgridToArrayMapIntIntArray[i]=i+1; // convert to 1-based
      }
    }

    distgridToArrayMapIntInt->extent[0]=distgrid->getDimCount();

    array=Array::create(arrayspec, distgrid,
                       distgridToArrayMapIntInt,
                       (InterArray<int> *)ESMC_NULL_POINTER,
                       (InterArray<int> *)ESMC_NULL_POINTER,
                       (InterArray<int> *)ESMC_NULL_POINTER,
                       (InterArray<int> *)ESMC_NULL_POINTER,
                       (InterArray<int> *)ESMC_NULL_POINTER,
                       (InterArray<int> *)ESMC_NULL_POINTER,
                        ESMC_NULL_POINTER, NULL, NULL,
                       (InterArray<int> *)ESMC_NULL_POINTER,
                       (InterArray<int> *)ESMC_NULL_POINTER, 
            &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;        

    // Set newly created Array into Grid
    localrc=this->setCoordArrayInternal(staggerloc, coord, array, true);
    if (ESMC_LogDefault.MsgFoundError(localrc,
               ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;        
    
  } // end of coord loop

  // Dellocate temporay arrays
  delete arrayspec;     
  delete [] distgridToArrayMapIntIntArray;
  delete distgridToArrayMapIntInt;

  // return ESMF_SUCCESS
  return ESMF_SUCCESS;
  }
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::addCoordFromArrayList()"
//BOP
// !IROUTINE:  addCoordFromArrayList
//
// !INTERFACE:
int Grid::addCoordFromArrayList(

//
// !RETURN VALUE:
//   Return code
//
// !ARGUMENTS:
//
                        int *staggerlocArg,        // (in) optional
                        int arrayCount,             // (in) 
                        Array **arrayList,           // (in)
                        DataCopyFlag *docopyArg,   // (in) optional
                        InterArray<int> *staggerEdgeLWidthArg, // (in) optional
                        InterArray<int> *staggerEdgeUWidthArg, // (in) optional
                        InterArray<int> *staggerAlignArg   // (in) optional 
  ) {
//
// !DESCRIPTION:
//    Set {\tt arrayArg} as the coordinate Array for stagger location 
// {\tt staggerlocArg} and coordinate component {\tt coordArg}. Use either
// a copy or a direct reference depending on the value of {\tt docopyArg}.
//EOP
//-----------------------------------------------------------------------------
  // ATTENTION ATTENTION ATTENTION ATTENTION ATTENTION ATTENTION ATTENTION
  //
  // TODO: This hasn't been a public interface and doesn't have an assocaited test.
  //       before its made public it should be gone over with a fine tooth comb, 
  //       or rewritten from scratch from setCoordArray. 
  //
  // ATTENTION ATTENTION ATTENTION ATTENTION ATTENTION ATTENTION ATTENTION

  // local vars
  int localrc;
  int rc;
  int staggerloc;
  int coord;
  DataCopyFlag docopy;
  int *staggerAlign;
  int *staggerEdgeLWidth;
  int *staggerEdgeUWidth;
  const int *arrayDimMap, *arrayLBounds, *arrayUBounds;
  const int *gridLBounds, *gridUBounds;
  bool ok;  


   // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL; 
  rc = ESMC_RC_NOT_IMPL;

  // This needs to be completely rewritten, so just make it an error to 
  // call it.
  ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
             "- This method not yet implemented ", ESMC_CONTEXT, &rc);
  return rc;


  // return Success
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::addItemArray()"
//BOP
// !IROUTINE:  addItemArray
//
// !INTERFACE:
int Grid::addItemArray(

//
// !RETURN VALUE:
//   Return code
//
// !ARGUMENTS:
//
                          int *staggerlocArg,             // (in) optional
                          int *itemArg,
              ESMC_TypeKind_Flag *typekindArg,          
                          InterArray<int> *staggerEdgeLWidthArg, // (in) optional
                          InterArray<int> *staggerEdgeUWidthArg, // (in) optional
                          InterArray<int> *staggerAlignArg,   // (in) optional 
                          InterArray<int> *staggerMemLBoundArg   // (in) optional 
  ) {
//
// !DESCRIPTION:
//   Create the storage (ESMF Array objects) to hold the item for a
// stagger location. 
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int rc, localrc;
  int staggerloc, item;
  const int *distgridToArrayMap;
  Array *array;
  DistGrid *staggerDistgrid;
  int extent[1];
  ESMC_TypeKind_Flag typekind;


  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  rc = ESMC_RC_NOT_IMPL;
  
  // Make sure the grid has the correct status for this action
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
      "- grid not of correct status to perform this operation", ESMC_CONTEXT,
      &rc);
    return rc;
  }  

  // If staggerloc hasn't been passed in use a default, otherwise copy it. 
  if (staggerlocArg==NULL) {
    staggerloc=0;  // default
  } else {
    staggerloc=*staggerlocArg;
  }

  // Check staggerloc
  if ((staggerloc < 0) || (staggerloc >= staggerLocCount)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- stagger location out of range", ESMC_CONTEXT, &rc);
    return rc;
  }

    // Translate itemArg to item
  if (itemArg==NULL) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
      "- must provide item", ESMC_CONTEXT, &rc);
    return rc;
  } else {
    item=*itemArg;
  }

  // Error check item
   if ((item < 0) || (item >= ESMC_GRIDITEM_COUNT)) {
     ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
                  "- Invalid item type", ESMC_CONTEXT, &rc);
    return rc;
  } 

  // Translate typekindArg to typekind to be used. 
  if (typekindArg==NULL) {
    if (item==ESMC_GRIDITEM_MASK) {
      typekind=ESMC_TYPEKIND_I4;
    } else {
      typekind=ESMC_TYPEKIND_R8;
    }
  } else {
    typekind=*typekindArg;

    // Error check typekind vs. item type
    if ((typekind != ESMC_TYPEKIND_I4) && (item==ESMC_GRIDITEM_MASK)) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
               "- Mask item must be integer", ESMC_CONTEXT, &rc);
      return rc;
    } 
  }

  // Error check and then set information about this stagger's bounds in the staggerLists
  localrc=this->setStaggerInfo(staggerloc, staggerEdgeLWidthArg, staggerEdgeUWidthArg,
                   staggerAlignArg, staggerMemLBoundArg);
  if (ESMC_LogDefault.MsgFoundError(localrc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;        


  // Get distgrid for this staggerloc 
  localrc=this->getStaggerDistgrid(staggerloc, &staggerDistgrid);
  if (ESMC_LogDefault.MsgFoundError(localrc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;        

  // construct ArraySpec for using to call Array::create
  ArraySpec *arrayspec= new ArraySpec;     
    
  int *distgridToArrayMapIntIntArray=new int[dimCount];
  extent[0]=dimCount;
  InterArray<int> *distgridToArrayMapIntInt =
    new InterArray<int>(distgridToArrayMapIntIntArray,1,extent); 

  InterArray<int> *staggerMemLBoundIntInt
    = (InterArray<int> *)ESMC_NULL_POINTER;
  int *staggerMemLBoundIntIntArray=(int *)ESMC_NULL_POINTER;

  // Only setup membounds if index flag is user
  if (indexflag==ESMC_INDEX_USER) {
    staggerMemLBoundIntIntArray=new int[dimCount];
    extent[0]=dimCount;
    staggerMemLBoundIntInt =
      new InterArray<int>(staggerMemLBoundIntIntArray,1,extent); 
  }


  ////////////
  ///// Construct the item array
  ///////////
  // fill in ArraySpec with information describing coordinate
  arrayspec->set(dimCount, typekind);

    
  // Initialize distgridToArrayMap array to 0 to make all unspecified dimensions
  //// replicated
  for (int i=0; i<dimCount; i++) {
    distgridToArrayMapIntIntArray[i]=0;
  }

  //// fill in distgridToArrayMap and computationalEdgeWidths for Array::create
  //// distgridToArrayMap - the same as distgridToGridMap
  //// computationalEdgeWidth - computed from staggerEdgeWidths and GridEdgeWidths
  for (int i=0; i<dimCount; i++) {
    distgridToArrayMapIntIntArray[i]=distgridToGridMap[i]+1; // convert to 1-based
  }
      
  // set size of distgridToArrayMap 
  // (needs to be total dimCount of distGrid even if coord dimCount < distgrid dimCount)
  // (0's indicate unused dimensions)
  distgridToArrayMapIntInt->extent[0]=dimCount;
    

    /* DEBUG 
    printf("compUWidth=");
    for (int i=0; i<coordDistDimCount; i++) {
      printf("%d ", compUWidthIntIntArray[i]);
    }
    printf("\n");

    printf("compLWidth=");
    for (int i=0; i<coordDistDimCount; i++) {
      printf("%d ", compLWidthIntIntArray[i]);
    }
    printf("\n");
      DEBUG */

    //// Optionally fix the lower memory bounds of each DE's memory chunk
    if (indexflag==ESMC_INDEX_USER) {
      // Set size of array based on dimCount
      staggerMemLBoundIntInt->extent[0]=dimCount;

      // Fill Array
      for (int i=0; i<dimCount; i++) {
      staggerMemLBoundIntIntArray[i] = staggerMemLBoundList[staggerloc][i];
      }
    }

    // Create an Array to hold the coords 
    array=Array::create(arrayspec, staggerDistgrid,
                          distgridToArrayMapIntInt,
                          (InterArray<int> *)ESMC_NULL_POINTER,
                          (InterArray<int> *)ESMC_NULL_POINTER,
                          (InterArray<int> *)ESMC_NULL_POINTER,
                          (InterArray<int> *)ESMC_NULL_POINTER,
                          (InterArray<int> *)ESMC_NULL_POINTER,
                          (InterArray<int> *)ESMC_NULL_POINTER,
                          &indexflag, NULL, staggerMemLBoundIntInt, 
                          (InterArray<int> *)ESMC_NULL_POINTER,
                          (InterArray<int> *)ESMC_NULL_POINTER, 
                          &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;        


   // Set newly created Array into Grid
   localrc=this->setItemArrayInternal(staggerloc, item, array, true);
   if (ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
    

  // Dellocate temporay arrays
  delete arrayspec;     
  delete [] distgridToArrayMapIntIntArray;
  delete distgridToArrayMapIntInt;
  if (indexflag==ESMC_INDEX_USER) {
    delete [] staggerMemLBoundIntIntArray;
    delete staggerMemLBoundIntInt;
  }

  // return ESMF_SUCCESS
  return ESMF_SUCCESS;
  }
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::addItemArrayArb()"
//BOP
// !IROUTINE:  addItemArrayArb
//
// !INTERFACE:
int Grid::addItemArrayArb(

//
// !RETURN VALUE:
//   Return code
//
// !ARGUMENTS:
//
                          int *staggerlocArg,             // (in) optional
              int *itemArg,                   // (in) required
              ESMC_TypeKind_Flag *typekindArg     // (in) optional          
  ) {
//
// !DESCRIPTION:
//   Create the storage (ESMF Array objects) to hold the item for a
// stagger location. 
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int rc, localrc;
  int staggerloc,item;
  DataCopyFlag docopy;
  const int *distgridToArrayMap;
  Array *array;
  int extent[1];
  ESMC_TypeKind_Flag typekind;

  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  rc = ESMC_RC_NOT_IMPL;
  
  // Make sure the grid has the correct status for this action
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
      "- grid not of correct status to perform this operation", ESMC_CONTEXT,
      &rc);
    return rc;
  }  

  // If staggerloc hasn't been passed in use a default, otherwise copy it. 
  if (staggerlocArg==NULL) {
    staggerloc=0;  // default
  } else {
    staggerloc=*staggerlocArg;
  }


  // Check staggerloc
  if ((staggerloc < 0) || (staggerloc >= staggerLocCount)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- stagger location out of range", ESMC_CONTEXT, &rc);
    return rc;
  }

  // Translate itemArg to item
  if (itemArg==NULL) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
      "- must provide item", ESMC_CONTEXT, &rc);
    return rc;
  } else {
    item=*itemArg;
  }

  // Error check item
  if ((item < 0) || (item >= ESMC_GRIDITEM_COUNT)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
                  "- Invalid item type", ESMC_CONTEXT, &rc);
    return rc;
  } 

  // Translate typekindArg to typekind to be used. 
  if (typekindArg==NULL) {
    if (item==ESMC_GRIDITEM_MASK) {
      typekind=ESMC_TYPEKIND_I4;
    } else {
      typekind=ESMC_TYPEKIND_R8;
    }
  } else {
    typekind=*typekindArg;

    // Error check typekind vs. item type
    if ((typekind != ESMC_TYPEKIND_I4) && (item==ESMC_GRIDITEM_MASK)) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
               "- Mask item must be integer", ESMC_CONTEXT, &rc);
      return rc;
    } 
  }


  int distgridDimCount = dimCount - distDimCount + 1;

  // construct ArraySpec for using to call Array::create
  ArraySpec *arrayspec= new ArraySpec;     
    
  int *distgridToArrayMapIntIntArray=new int[distgridDimCount];
  extent[0]=distgridDimCount;
  InterArray<int> *distgridToArrayMapIntInt =
    new InterArray<int>(distgridToArrayMapIntIntArray,1,extent); 

  ////////////
  ///// Construct the item array
  ///////////
  // fill in ArraySpec with information describing coordinate
  arrayspec->set(distgridDimCount, typekind);

    
  // make distgridToArray mapping a 1-1 mapping between array and distgrid
  for (int i=0; i<distgridDimCount; i++) {
    distgridToArrayMapIntIntArray[i]=i+1; // convert to 1-based
  }
      
  // set size of distgridToArrayMap 
  // (needs to be distgrid dimCount)
  // (0's indicate unused dimensions)
  distgridToArrayMapIntInt->extent[0]=distgridDimCount;
        
  
    // Create an Array to hold the coords 
   array=Array::create(arrayspec, distgrid,
              distgridToArrayMapIntInt,
               (InterArray<int> *)ESMC_NULL_POINTER,
               (InterArray<int> *)ESMC_NULL_POINTER,
               (InterArray<int> *)ESMC_NULL_POINTER,
               (InterArray<int> *)ESMC_NULL_POINTER,
               (InterArray<int> *)ESMC_NULL_POINTER,
               (InterArray<int> *)ESMC_NULL_POINTER,
               &indexflag, NULL, NULL,
               (InterArray<int> *)ESMC_NULL_POINTER,
               (InterArray<int> *)ESMC_NULL_POINTER, 
               &localrc);

   if (ESMC_LogDefault.MsgFoundError(localrc,
       ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;        

   // Set newly created Array into Grid
   localrc=this->setItemArrayInternal(staggerloc, item, array, true);
   if (ESMC_LogDefault.MsgFoundError(localrc,
                          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;        
    

  // Dellocate temporay arrays
  delete arrayspec;     
  delete [] distgridToArrayMapIntIntArray;
  delete distgridToArrayMapIntInt;

  // return ESMF_SUCCESS
  return ESMF_SUCCESS;
  }
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::commit()"
//BOP
// !IROUTINE:  commit
//
// !INTERFACE:
int Grid::commit(
//
// !RETURN VALUE:
//   Error code
//
// !ARGUMENTS:
//
 ){
//
// !DESCRIPTION:
//   This call is the final step of the create empty/set/commit incremental
// method for creating a Grid. The \gridArg parameter passed in here should
// have been created with create(rc), then filled with information using set 
// set().
// After this call the grid object will be usable in other methods, but may
// no longer be the subject of set().
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int rc;
  int localrc;                 // local error status

  // initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  // Make sure that we're the correct status
  if (status != ESMC_GRIDSTATUS_NOT_READY) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_BAD,
      "- Can't use commit on an already created object", ESMC_CONTEXT, &rc);
    return rc;
  }

 // Get the protoGrid which holds the information from set()
  if (proto == ESMC_NULL_POINTER) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_BAD,
      "- Null protoGrid ", ESMC_CONTEXT, &rc);
    return rc;
  }

  // setup the grid's internal structures 
  // if localArbIndexCount >= 0, it is an arbitrary grid
  if (proto->localArbIndexCount >= 0) {
    localrc = construct(this, proto->nameLen, proto->name, proto->typekind, 
            proto->distgrid, proto->minIndex, proto->maxIndex,
            proto->localArbIndex, proto->localArbIndexCount,
            proto->distDim, 
            proto->arbDim,
            proto->undistLBound,
            proto->undistUBound, proto->coordSys, 
            proto->coordDimCount, proto->coordDimMap,
                        proto->gridMemLBound,
                        proto->indexflag,
                        proto->destroyDistgrid, proto->destroyDELayout);
  } else {
    localrc=construct(this, proto->nameLen, proto->name, proto->typekind, 
                       proto->distgrid, 
                       proto->gridEdgeLWidth, proto->gridEdgeUWidth,
                       proto->gridAlign,
                       proto->distgridToGridMap, 
                       proto->undistLBound, proto->undistUBound,
                       proto->coordSys,  
                       proto->coordDimCount, proto->coordDimMap,
                       proto->gridMemLBound,
               proto->indexflag,
                       proto->destroyDistgrid, proto->destroyDELayout);
  }  
  if (ESMC_LogDefault.MsgFoundError(localrc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;        

   // Now that we don't need it anymore, remove the protogrid from the grid
   localrc=this->delProtoGrid();
   if (ESMC_LogDefault.MsgFoundError(localrc,
                       ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;        

  // return successfully
  return ESMF_SUCCESS;
  }
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::create()"
//BOP
// !IROUTINE:  create
//
// !INTERFACE:
Grid *Grid::create(
//
// !RETURN VALUE:
//    Grid * to newly allocated ESMC_Grid
//
// !ARGUMENTS:
//
  int nameLenArg,                           // (in) 
  char *nameArg,                            // (in) optional
  ESMC_TypeKind_Flag *typekindArg,               // (in) optional
  DistGrid *distgridArg,                    // (in) optional
  InterArray<int> *gridEdgeLWidthArg,           // (in) optional
  InterArray<int> *gridEdgeUWidthArg,           // (in) optional
  InterArray<int> *gridAlignArg,                // (in) optional
  InterArray<int> *distgridToGridMapArg,                  // (in) optional
  ESMC_CoordSys_Flag *coordSys, 
  InterArray<int> *coordDimCountArg,               // (in) optional
  InterArray<int> *coordDimMapArg,             // (in) optional
  InterArray<int> *gridMemLBoundArg,          // (in) optional
  ESMC_IndexFlag *indexflagArg,             // (in) optional
  bool *destroyDistgridArg,
  bool *destroyDELayoutArg,
  int *rcArg,                                // (out) return code optional
  VM *vm                                     // (in)
  ){
//
// !DESCRIPTION:
//    Create an {\tt ESMC\gridArg} object from a DistGrid. This method sets up
//  the internal structures of the Grid. After calling this create the 
//  returned Grid object may be used in most other Grid methods. 
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                 // local error status

  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  if (rcArg!=NULL)
    *rcArg = ESMC_RC_NOT_IMPL;

  // allocate the grid object
  Grid *grid=ESMC_NULL_POINTER;
  try{
    grid = new Grid(vm);  // specific VM, or default if vm==NULL
  }catch(...){
     // allocation error
     ESMC_LogDefault.MsgAllocError("for new ESMC_Grid.", ESMC_CONTEXT, rcArg);
     return ESMC_NULL_POINTER;
  }


  // setup the grids internal structure using the passed in parameters. 
  localrc=construct(grid, nameLenArg, nameArg, typekindArg, distgridArg, 
                    gridEdgeLWidthArg,gridEdgeUWidthArg, gridAlignArg,
                    distgridToGridMapArg, 
                    (InterArray<int> *)ESMC_NULL_POINTER,
                    (InterArray<int> *)ESMC_NULL_POINTER,
                    coordSys, coordDimCountArg, coordDimMapArg, gridMemLBoundArg,
                    indexflagArg,
                    destroyDistgridArg, destroyDELayoutArg);
   if (ESMC_LogDefault.MsgFoundError(localrc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rcArg)) return ESMC_NULL_POINTER;        

  // return successfully
  *rcArg = ESMF_SUCCESS;
  return grid;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::create()"
//BOP
// !IROUTINE:  create
//
// !INTERFACE:
Grid *Grid::create(
//
// !RETURN VALUE:
//    Grid * to newly allocated ESMC_Grid 
//
// !ARGUMENTS:
//
  int nameLenArg,                           // (in) 
  char *nameArg,                            // (in) optional
  ESMC_TypeKind_Flag *typekindArg,               // (in) optional
  DistGrid *distgridArg,                    // (in) optional
  InterArray<int> *minIndexArg,                // (in) optional
  InterArray<int> *maxIndexArg,                // (in)
  InterArray<int> *localArbIndexArg,            // (in)
  int  localArbIndexCount,                          // (in)
  InterArray<int> *distDimArg,                 // (in) 
  int  arbDim,                              // (in)
  ESMC_CoordSys_Flag *coordSys, 
  InterArray<int> *coordDimCountArg,               // (in) optional
  InterArray<int> *coordDimMapArg,             // (in) optional
  bool *destroyDistgridArg,
  bool *destroyDELayoutArg,
  int *rcArg                                // (out) return code optional
  ){
//
// !DESCRIPTION:
//    Create an {\tt ESMC\gridArg} object from a DistGrid. This method sets up
//  the internal structures of the Grid. After calling this create the 
//  returned Grid object may be used in most other Grid methods.  This is an
//  overloaded function for an arbitrarily distributed grid. 
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                 // local error status

  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  if (rcArg!=NULL)
    *rcArg = ESMC_RC_NOT_IMPL;

  // allocate the grid object
  Grid *grid=ESMC_NULL_POINTER;
  try{
    grid = new Grid();
  }catch(...){
     // allocation error
     ESMC_LogDefault.MsgAllocError("for new ESMC_Grid.", ESMC_CONTEXT, rcArg);
     return ESMC_NULL_POINTER;
  }


  // setup the grids internal structure using the passed in parameters. 
  localrc=construct(grid, nameLenArg, nameArg, typekindArg, distgridArg, 
                    minIndexArg, maxIndexArg, localArbIndexArg, localArbIndexCount,
            distDimArg, arbDim, 
                    (InterArray<int> *)ESMC_NULL_POINTER,
                    (InterArray<int> *)ESMC_NULL_POINTER,
            coordSys, 
                    coordDimCountArg,
            coordDimMapArg,
                    (InterArray<int> *)ESMC_NULL_POINTER,
                    (ESMC_IndexFlag *)NULL,  
                    destroyDistgridArg, destroyDELayoutArg);
   if (ESMC_LogDefault.MsgFoundError(localrc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rcArg)) return ESMC_NULL_POINTER;        

  // return successfully
  *rcArg = ESMF_SUCCESS;
  return grid;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::create()"
//BOP
// !IROUTINE:  create empty
//
// !INTERFACE:
Grid *Grid::create(
//
// !RETURN VALUE:
//    Grid * to newly allocated ESMC_Grid
//
// !ARGUMENTS:
//
                   int *rcArg  // (out) optional return code 
  ){
//
// !DESCRIPTION:
//    Create an empty {\tt ESMC\_Grid} object. The returned Grid object may
// only be used with set and commit. To make this object usable employ
// {\tt set} to fill the object with parameters and then {\tt commit} to
// construct a usable Grid based on those parameters. 
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                 // local error status

  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  if (rcArg!=NULL)
    *rcArg = ESMC_RC_NOT_IMPL;
  
  // allocate the new Grid object
  Grid *grid=ESMC_NULL_POINTER;
  try{
    grid = new Grid();
  }catch(...){
     // allocation error
     ESMC_LogDefault.MsgAllocError("for new ESMC_Grid.", ESMC_CONTEXT, rcArg);
     return ESMC_NULL_POINTER;
  }

  // Add a protogrid to hold the information that will eventually be used
  // by commit to construct the internal structures of the Grid
  localrc=grid->addProtoGrid();
   if (ESMC_LogDefault.MsgFoundError(localrc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rcArg)) return ESMC_NULL_POINTER;        

  // return successfully
  *rcArg = ESMF_SUCCESS;

  return grid;
  }
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::destroy()"
//BOP
// !IROUTINE:  destroy
//
// !INTERFACE:
int Grid::destroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  Grid **gridArg,               // in - Grid to destroy
  bool noGarbage){              // in - remove from garbage collection
//
// !DESCRIPTION:
// Deallocate a Grid's internal memory and then deallocate the Grid object 
// itself. The grid parameter is set to ESMC_NULL_POINTER. 
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int rc;                // automatic variable for local return code

  // initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  // return with error for NULL pointer
  if (gridArg == ESMC_NULL_POINTER || *gridArg == ESMC_NULL_POINTER){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to Grid", ESMC_CONTEXT, &rc);
    return rc;
  }

  // check if this Grid object has the persist flag set
  if ((*gridArg)->ESMC_BaseGetPersist())
    return ESMF_SUCCESS;  // nothing to be done here, return successfully

  try{
    // check if this Grid object still has a valid entry in the garbage collection
    if (!VM::validObject(*gridArg))
      return ESMF_SUCCESS;  // nothing to be done here, return successfully
    // destruct Grid object
    (*gridArg)->destruct(true, noGarbage);
    // mark as invalid object
    (*gridArg)->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc);
    return rc;
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught exception", ESMC_CONTEXT, &rc);
    return rc;
  }

  // optionally delete the complete object and remove from garbage collection
  if (noGarbage){
    VM::rmObject(*gridArg); // remove object from garbage collection
    delete (*gridArg);      // completely delete the object, free heap
  }

  // return successfully
  return ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::getCoordArray()"
//BOP
// !IROUTINE:  gridCoordArray
//
// !INTERFACE:
Array *Grid::getCoordArray(

//
// !RETURN VALUE:
//   The requested array
//
// !ARGUMENTS:
//
                        int *staggerlocArg,        // (in) optional
                        int coordArg,              // (in) base-1
                        DataCopyFlag *docopyArg,  // (in) optional
                        int *rcArg                 // (out) optional return code 
  ) {
//
// !DESCRIPTION:
//    Get the Array object holding the coordinates values for stagger location
//    {\tt staggerloc} and coordinate component {\tt coord} (coord is 1 based).
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                 // local error status
  int staggerloc;
  int coord;
  DataCopyFlag docopy;
  int dimCount;
  Array *array;

  // initialize return code; assume routine not implemented
  if (rcArg != ESMC_NULL_POINTER) *rcArg = ESMC_RC_NOT_IMPL;

  // make sure grid is the correct status for this action
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
      "- grid not of correct status to perform this operation", ESMC_CONTEXT,
      rcArg);

    return ESMC_NULL_POINTER;
  }  
  
  // If staggerlocArg wasn't passed in, use default, else copy the value.
  if (staggerlocArg==NULL) {
    staggerloc=0;  // default center
  } else {
    staggerloc=*staggerlocArg;
  }

  // translate coord to C++ base-0.
  coord=coordArg-1; // translate from F90 1 based to 0 based

  // TODO: make sure coord is within 0 to dimCount-1


  // If docopyArg wasn't passed in, use default, else copy the value
  if (docopyArg==NULL) {
    docopy=DATACOPY_REFERENCE;  // default
  } else {
    docopy=*docopyArg;
  }

  // Copy option isn't working for now
  if (docopy==DATACOPY_VALUE) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
        "- Data Copy Flag not implemented yet", ESMC_CONTEXT, rcArg);
      return ESMC_NULL_POINTER;
  }

  // Get Coord Array
  localrc=this->getCoordArrayInternal(staggerloc, coord, &array);
  if (ESMC_LogDefault.MsgFoundError(localrc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rcArg)) return ESMC_NULL_POINTER;        

  // return SUCCESS
  if (rcArg != ESMC_NULL_POINTER) *rcArg = ESMF_SUCCESS;  

   // return what getCoordArray returned
  return array;
  }
//-----------------------------------------------------------------------------


  //-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::getItemArray()"
//BOP
// !IROUTINE:  gridItemArray
//
// !INTERFACE:
Array *Grid::getItemArray(

//
// !RETURN VALUE:
//   The requested array
//
// !ARGUMENTS:
//
                        int *staggerlocArg,        // (in) optional
                        int *itemArg,              // (in) required
                        DataCopyFlag *docopyArg,  // (in) optional
                        int *rcArg                 // (out) optional return code 
  ) {
//
// !DESCRIPTION:
//    Get the Array object holding the coordinates values for stagger location
//    {\tt staggerloc}.
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                 // local error status
  int staggerloc, item;
  DataCopyFlag docopy;
  int dimCount;
  Array *array;

  // initialize return code; assume routine not implemented
  if (rcArg != ESMC_NULL_POINTER) *rcArg = ESMC_RC_NOT_IMPL;

  // make sure grid is the correct status for this action
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
      "- grid not of correct status to perform this operation", ESMC_CONTEXT,
      rcArg);

    return ESMC_NULL_POINTER;
  }  
  
  // If staggerlocArg wasn't passed in, use default, else copy the value.
  if (staggerlocArg==NULL) {
    staggerloc=0;  // default center
  } else {
    staggerloc=*staggerlocArg;
  }

  // Translate itemArg to item
  if (itemArg==NULL) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
      "- must provide item", ESMC_CONTEXT, rcArg);
    return ESMC_NULL_POINTER;
  } else {
    item=*itemArg;
  }

  // Error check item
  if ((item < 0) || (item >= ESMC_GRIDITEM_COUNT)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
                  "- Invalid item type", ESMC_CONTEXT, rcArg);
    return ESMC_NULL_POINTER;
  } 


  // If docopyArg wasn't passed in, use default, else copy the value
  if (docopyArg==NULL) {
    docopy=DATACOPY_REFERENCE;  // default
  } else {
    docopy=*docopyArg;
  }

  // Copy option isn't working for now
  if (docopy==DATACOPY_VALUE) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
        "- Data Copy Flag not implemented yet", ESMC_CONTEXT, rcArg);
      return ESMC_NULL_POINTER;
  }

  // Get Item Array
  localrc=this->getItemArrayInternal(staggerloc, item, &array);
  if (ESMC_LogDefault.MsgFoundError(localrc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rcArg)) return ESMC_NULL_POINTER;        

  // return SUCCESS
  if (rcArg != ESMC_NULL_POINTER) *rcArg = ESMF_SUCCESS;  

   // return what getCoordArray returned
  return array;
  }
//-----------------------------------------------------------------------------


#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::getExclusiveLBound()"
//BOPI
// !IROUTINE:  Grid::getExclusiveLBound()"
//
// !INTERFACE:
int Grid::getExclusiveLBound(
//
// !RETURN VALUE:
//   return code
//
// !ARGUMENTS:
//
                                 int staggerlocArg, //(in)
                                 int localDEArg,     // (in)
                                 int *lBndArg      // (out) needs to be of size > distDimCount
                                 ){
//
// !DESCRIPTION:
//  The exclusive lower bound for this localde.
//
//EOPI
//-----------------------------------------------------------------------------
  int rc,localrc;
  int distExLBnd[ESMF_MAXDIM];
  int offsetL[ESMF_MAXDIM];

  // initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  
  // Check status
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Grid not fully created", ESMC_CONTEXT, &rc);
    return rc;
  }

  // Check staggerloc
  if ((staggerlocArg < 0) || (staggerlocArg >= staggerLocCount)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- stagger location out of range", ESMC_CONTEXT, &rc);
    return rc;
  }

  // Ensure localDEArg isn't out of range for this PET
  if ((localDEArg < 0) || (localDEArg >=distgrid->getDELayout()->getLocalDeCount())) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
          "- localDE outside range on this processor", ESMC_CONTEXT, &rc);
        return rc;
  }

  if (decompType == ESMC_GRID_NONARBITRARY) { 

    // get grid distributed exclusive bounds
    localrc=this->getDistExclusiveLBound(localDEArg, distExLBnd);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;

    // Map from distgrid to grid
    for (int i=0; i<dimCount; i++) {
      if (gridIsDist[i]) {
    lBndArg[i]=distExLBnd[gridMapDim[i]];
      } else {
    lBndArg[i]=undistLBound[gridMapDim[i]];
      }
    }
 
    // get computational offset
    localrc=this->getLDEStaggerLOffset(staggerlocArg, localDEArg, offsetL);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
       &rc)) return rc;
    
    // subtract offset
    for (int i=0; i<dimCount; i++) {
      lBndArg[i] -= offsetL[i];  
    }
  } else {
   // always start with 1 for arbitrary grid
    for (int i=0; i<dimCount-distDimCount+1; i++) {
      lBndArg[i]=1;
    }
  }
  // tell the calling subroutine that we've had a successful outcome
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::getExclusiveUBound()"
//BOPI
// !IROUTINE:  Grid::getExclusiveUBound()"
//
// !INTERFACE:
int Grid::getExclusiveUBound(
//
// !RETURN VALUE:
//   return code
//
// !ARGUMENTS:
//
                                 int staggerlocArg, //(in)
                                 int localDEArg,     // (in)
                                 int *uBndArg      // (out) needs to be of size > distDimCount
                                 ){
//
// !DESCRIPTION:
//  The computational lower bound for this localde.
//
//EOPI
//-----------------------------------------------------------------------------
  int rc,localrc;
  int offsetU[ESMF_MAXDIM];
  int distExUBnd[ESMF_MAXDIM];

  // initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  
  // Check status
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Grid not fully created", ESMC_CONTEXT, &rc);
    return rc;
  }

  // Check staggerloc
  if ((staggerlocArg < 0) || (staggerlocArg >= staggerLocCount)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- stagger location out of range", ESMC_CONTEXT, &rc);
    return rc;
  }

  // Ensure localDEArg isn't out of range for this PET
  if ((localDEArg < 0) || (localDEArg >=distgrid->getDELayout()->getLocalDeCount())) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
          "- localDE outside range on this processor", ESMC_CONTEXT, &rc);
        return rc;
  }

  if (decompType == ESMC_GRID_NONARBITRARY) { 
   // get grid distributed exclusive bounds
   localrc=this->getDistExclusiveUBound(localDEArg, distExUBnd);
   if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &rc)) return rc;

   // Map between distgrid and grid
   for (int i=0; i<dimCount; i++) {
     if (gridIsDist[i]) {
       uBndArg[i]=distExUBnd[gridMapDim[i]];
     } else {
       uBndArg[i]=undistUBound[gridMapDim[i]];
     }
   }

    // get computational offset
    localrc=this->getLDEStaggerUOffset(staggerlocArg, localDEArg, offsetU);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
         &rc)) return rc;

    // Add offset
    for (int i=0; i<dimCount; i++) {
      uBndArg[i] += offsetU[i];  
    }
  } else {
    // Get some useful information
    int distgridDimCount = dimCount-distDimCount+1;
    const int *localDeToDeMap = distgrid->getDELayout()->getLocalDeToDeMap();
    const int *indexCountPDimPDe = distgrid->getIndexCountPDimPDe();

    // Get the Global DE from the local DE
    int de = localDeToDeMap[localDEArg];

    // exlc. region for each DE ends at indexCountPDimPDe of the associated
    // DistGrid
    for (int i=0; i<distgridDimCount; i++) {
      uBndArg[i]=indexCountPDimPDe[de*distgridDimCount+i];
    }
  }
  // tell the calling subroutine that we've had a successful outcome
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::getDistExclusiveLBound()"
//BOPI
// !IROUTINE:  Grid::getDistExclusiveLBound()"
//
// !INTERFACE:
int Grid::getDistExclusiveLBound(
//
// !RETURN VALUE:
//   return code
//
// !ARGUMENTS:
//
  int localDEArg,     // (in)
  int *lBndArg      // (out) needs to be of size > distDimCount
  ){
//
// !DESCRIPTION:
//  The exclusive lower bound for this localde.
// TODO: eventually this should return all the grid bounds, not just
//       the distributed ones.
//
//EOPI
//-----------------------------------------------------------------------------
  int rc,localrc;

  // initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  
  // Check status
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Grid not fully created", ESMC_CONTEXT, &rc);
    return rc;
  }


  // Ensure localDEArg isn't out of range for this PET
  if ((localDEArg < 0) || (localDEArg >=distgrid->getDELayout()->getLocalDeCount())) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
          "- localDE outside range on this processor", ESMC_CONTEXT, &rc);
        return rc;
  }

  // Set lower bound based on indexflag
  if ((indexflag==ESMC_INDEX_DELOCAL) || (indexflag==ESMC_INDEX_USER)) {
    for (int i=0; i<distDimCount; i++)
      lBndArg[i] = 1; // excl. region starts at (1,1,1...) 
  } else {
    // Get some useful information
    const int *localDeToDeMap = distgrid->getDELayout()->getLocalDeToDeMap();

    // Get the Global DE from the local DE
    int de = localDeToDeMap[localDEArg];

    // Set Bound based on distgrid info
    for (int i=0; i<distDimCount; i++){
        
      // obtain indexList for this DE and dim
      const int *indexList =
        distgrid->getIndexListPDimPLocalDe(localDEArg, i+1, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
         &rc)) return rc;
      
      // make sure this dimension is contiguous         
      const int contig=distgrid->getContigFlagPDimPDe(de, i+1, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
         &rc)) return rc;
      if (!contig) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
               "- doesn't handle non-contiguous DEs yet ", ESMC_CONTEXT, &rc);
        return rc;
      }
      
      // Set lower bounds of exclusive region to match indexList[0]
      lBndArg[i] = indexList[0];
    } // i
  }
  
  // tell the calling subroutine that we've had a successful outcome
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::getDistExclusiveUBound()"
//BOPI
// !IROUTINE:  Grid::getDistExclusiveUBound()"
//
// !INTERFACE:
int Grid::getDistExclusiveUBound(
//
// !RETURN VALUE:
//   return code
//
// !ARGUMENTS:
//
  int localDEArg,     // (in)
  int *uBndArg      // (out) needs to be of size > distDimCount
  ){
//
// !DESCRIPTION:
//  The exclusive upper bound for this localde
// TODO: eventually this should return all the grid bounds, not just
//       the distributed ones.
//
//EOPI
//-----------------------------------------------------------------------------
  int rc,localrc;

  // initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  
  // Check status
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Grid not fully created", ESMC_CONTEXT, &rc);
    return rc;
  }


  // Ensure localDEArg isn't out of range for this PET
  if ((localDEArg < 0) || (localDEArg >=distgrid->getDELayout()->getLocalDeCount())) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
          "- localDE outside range on this processor", ESMC_CONTEXT, &rc);
        return rc;
  }

  // Get some useful information
  const int *localDeToDeMap = distgrid->getDELayout()->getLocalDeToDeMap();
  const int *indexCountPDimPDe = distgrid->getIndexCountPDimPDe();

  // Get the Global DE from the local DE
  int de = localDeToDeMap[localDEArg];

  // exlc. region for each DE ends at indexCountPDimPDe of the associated
  // DistGrid
    for (int i=0; i<distDimCount; i++)
      uBndArg[i]=indexCountPDimPDe[de*distDimCount+i];

  // Set upper bound based on indexflag
  if (indexflag==ESMC_INDEX_GLOBAL) {

      for (int i=0; i<distDimCount; i++){

        // obtain indexList for this DE and dim
        const int *indexList =
          distgrid->getIndexListPDimPLocalDe(localDEArg, i+1, &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,
            ESMC_CONTEXT, &rc)) return rc;

        // make sure is contiguous         
        const int contig=distgrid->getContigFlagPDimPDe(de, i+1, &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc,
                       ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
        if (!contig) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
                "- doesn't handle non-contiguous DEs yet ", ESMC_CONTEXT, &rc);
          return rc;
        }

        // shift bounds of exclusive region to match indexList[0]
        uBndArg[i] += indexList[0] - 1;
      } // i
  }

  // tell the calling subroutine that we've had a successful outcome
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


// Translate type to typekind for debugging
  template<class TYPE> 
  ESMC_TypeKind_Flag get_TypeKind_from_Type() {
    return ESMF_NOKIND;
  }

  template<> 
  ESMC_TypeKind_Flag get_TypeKind_from_Type<ESMC_R8>() {
    return ESMC_TYPEKIND_R8;
  }

  template<> 
  ESMC_TypeKind_Flag get_TypeKind_from_Type<ESMC_R4>() {
    return ESMC_TYPEKIND_R4;
  }

  template<> 
  ESMC_TypeKind_Flag get_TypeKind_from_Type<ESMC_I4>() {
    return ESMC_TYPEKIND_I4;
  }

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::getCoordInternal()"
//BOPI
// !IROUTINE:  Grid::getCoordInternal()"
//
// !INTERFACE:
 template <class TYPE>
void Grid::getCoordInternal(
//
// !RETURN VALUE:
//   void
//
// !ARGUMENTS:
//
                                 int staggerloc, // (in)
                                 int localDE,    // (in)
                                 int *index,     // (in)  needs to be of size Grid dimCount
                                 TYPE *coord     // (out) needs to be of size Grid dimCount
                                 ){
//
// !DESCRIPTION:
//
//   WARNING: NO ERROR CHECKING OR TYPE CONVERSION MAKE SURE THIS IS BEING DONE IN CALLING ROUTINE
// 
//  Get coordinates from an index tuple. For efficiency reasons this version doesn't do error checking or
//  type conversion for a public version with error checking see  Grid::getCoord().  
//
//
//EOPI
//-----------------------------------------------------------------------------
  int coordIndex[ESMF_MAXDIM];
  LocalArray *localArray;
  int index1D;
  int localrc;

  // TODO: need to make this function more efficient. Use templates? 

  // For arbitrary grid, need to find the index of the 1D distgrid from the original index
  if (decompType == ESMC_GRID_NONARBITRARY) {
    // Loop Getting coordinates
    for (int c=0; c<dimCount; c++) {
      
      //// Map Grid indices to coord indices
      for (int i=0; i<coordDimCount[c]; i++) {
    coordIndex[i]=index[coordDimMap[c][i]];
      }
      //// Get LocalArray cooresponding to staggerloc, coord and localDE
      localArray=(coordArrayList[staggerloc][c]->getLocalarrayList())[localDE];
       
      //// Get pointer to LocalArray data
      localArray->getDataInternal(coordIndex, coord+c);
    }
      
  } else {
      index1D = convertIndex(index);
     //if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
     //                           &rc)) return rc;
     for (int c=0; c<dimCount; c++) {
       
       //// Get LocalArray cooresponding to staggerloc, coord and localDE
       localArray=(coordArrayList[staggerloc][c]->getLocalarrayList())[localDE];

       for (int i=1; i<coordDimCount[c]; i++) {
     if (coordDimMap[c][i] == ESMC_GRID_ARBDIM) {
       coordIndex[i] = index1D;
      } else {
       coordIndex[i] = index[coordDimMap[c][i]];
     }
       }
       //// Get pointer to LocalArray data
       localArray->getDataInternal(coordIndex, coord+c);
     }
  }
}

// Add more types here if necessary
template void Grid::getCoordInternal(int staggerloc, int localDE, int *index, ESMC_R8 *data);
template void Grid::getCoordInternal(int staggerloc, int localDE, int *index, ESMC_R4 *data);
template void Grid::getCoordInternal(int staggerloc, int localDE, int *index, ESMC_I4 *data);

//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::getCoordInternalConvert()"
//BOPI
 // !IROUTINE:  Grid::getCoordInternalConvert()"
//
// !INTERFACE:
template <class TYPE>
int Grid::getCoordInternalConvert(
 //
// !RETURN VALUE:
//    return code
//
// !ARGUMENTS:
//
                                 int staggerloc, // (in)
                                 int localDE,    // (in)
                                 int *index,     // (in)  needs to be of size Grid dimCount
                                 TYPE *coord     // (out) needs to be of size Grid dimCount
                                 ){
//
// !DESCRIPTION:
//
//  WARNING: NO ERROR CHECKING EXCEPT FOR TYPEKIND, MAKE SURE OTHER CHECKING IS BEING DONE IN CALLING ROUTINE
//
//  Get coordinates from an index tuple. For efficiency reasons this version doesn't do error checking, except
//  on the typekind. For a public version with full error checking see  Grid::getCoord(). 
//  However, this version does convert types of the coordinates if necessary
//
//
//EOPI
//-----------------------------------------------------------------------------

  // Get coordinates depending on type of the grid
  if (typekind == ESMC_TYPEKIND_R8) {
    ESMC_R8 tmp_coord[ESMF_MAXDIM];

    // get coordinates
    getCoordInternal(staggerloc, localDE, index, tmp_coord);
    
    // Copy/convert to output
     for (int i=0; i<dimCount; i++) {   
      coord[i]=static_cast<TYPE>(tmp_coord[i]);
    }
  } else if (typekind == ESMC_TYPEKIND_R4) {
    ESMC_R4 tmp_coord[ESMF_MAXDIM];

    // get coordinates
    getCoordInternal(staggerloc, localDE, index, tmp_coord);
    
    // Copy/convert to output
    for (int i=0; i<dimCount; i++) {   
      coord[i]=static_cast<TYPE>(tmp_coord[i]);
    }
  } else if (typekind == ESMC_TYPEKIND_I4) {
    ESMC_I4 tmp_coord[ESMF_MAXDIM];
    
    // get coordinates
    getCoordInternal(staggerloc, localDE, index, tmp_coord);
    
    // Copy/convert to output
    for (int i=0; i<dimCount; i++) {   
      coord[i]=static_cast<TYPE>(tmp_coord[i]);
    }
  } else {
    int localrc;
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
                                  "- Unsupported typekind in conversion", ESMC_CONTEXT, &localrc);
    return localrc;
  }

  // return success
  return ESMF_SUCCESS;
}

// Add more types here if necessary
template int Grid::getCoordInternalConvert(int staggerloc, int localDE, int *index, ESMC_R8 *data);
template int Grid::getCoordInternalConvert(int staggerloc, int localDE, int *index, ESMC_R4 *data);
 template int Grid::getCoordInternalConvert(int staggerloc, int localDE, int *index, ESMC_I4 *data);


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::getCoord()"
//BOPI
// !IROUTINE:  Grid::getCoord()"
//
// !INTERFACE:
template <class TYPE>
int Grid::getCoord(
//
// !RETURN VALUE:
//   return code
//   
// !ARGUMENTS:
//
                                 int staggerloc, // (in)
                                 int localDE,    // (in)
                                 int *index,     // (in)  needs to be of size Grid dimCount
                                 TYPE *coord     // (out) needs to be of size Grid dimCount
                                 ){
//
// !DESCRIPTION:
//  Get coordinates from an index tuple. For efficiency reasons this version doesn't do error checking
//  for a public version with error checking see  Grid::getCoord().  
//
//
//EOPI
//-----------------------------------------------------------------------------
  int coordIndex[ESMF_MAXDIM];
  LocalArray *localArray;
  int localrc = ESMC_RC_NOT_IMPL;
  int rc = ESMC_RC_NOT_IMPL;
  int index1D;

  // Check status
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
      "- Grid not fully created", ESMC_CONTEXT, &rc);
    return rc;
  }

  // Check typekind
  if (typekind != get_TypeKind_from_Type<TYPE>()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
               "- requested type does not match type of coordinates in grid", ESMC_CONTEXT, &rc);
    return rc;
  }

  // Check staggerloc
  if ((staggerloc < 0) || (staggerloc >= staggerLocCount)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
      "- stagger location out of range", ESMC_CONTEXT, &rc);
    return rc;
  }

  // Ensure localDE isn't out of range for this PET
  if ((localDE < 0) || (localDE >=distgrid->getDELayout()->getLocalDeCount())) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
          "- localDE outside range on this processor", ESMC_CONTEXT, &rc);
         return rc;
  }
 
  // Check here for coordinate Array existance
  if (!hasCoordStaggerLoc(staggerloc)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
               "- staggerloc is empty on this Grid", ESMC_CONTEXT, &rc);
    return rc;
  }

 // For arbitrary grid, need to find the index of the 1D distgrid from the original index
  if (decompType == ESMC_GRID_NONARBITRARY) {
    // Loop Getting coordinates
    for (int c=0; c<dimCount; c++) {
      
      //// Map Grid indices to coord indices
      for (int i=0; i<coordDimCount[c]; i++) {
    coordIndex[i]=index[coordDimMap[c][i]];
      }
      //// Get LocalArray cooresponding to staggerloc, coord and localDE
      localArray=(coordArrayList[staggerloc][c]->getLocalarrayList())[localDE];
      
      //// Get pointer to LocalArray data
      localrc=localArray->getData(coordIndex, coord+c);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc; 
    }
      
  } else {
     index1D = convertIndex(index);
     for (int c=0; c<dimCount; c++) {
       
       //// Get LocalArray cooresponding to staggerloc, coord and localDE
        localArray=(coordArrayList[staggerloc][c]->getLocalarrayList())[localDE];

       for (int i=0; i<coordDimCount[c]; i++) {
     if (coordDimMap[c][i] == ESMC_GRID_ARBDIM) {
        coordIndex[i] = index1D;
     } else {
        coordIndex[i] = index[coordDimMap[c][i]];
     }
       }
       
       //// Get pointer to LocalArray data
       localrc=localArray->getData(coordIndex, coord+c);
       if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                        &rc)) return rc; 
     }
  }
  
  // return success
  return ESMF_SUCCESS;
}

// Add more types here if necessary
template int Grid::getCoord(int staggerloc, int localDE, int *index, ESMC_R8 *data);
template int Grid::getCoord(int staggerloc, int localDE, int *index, ESMC_R4 *data);
template int Grid::getCoord(int staggerloc, int localDE, int *index, ESMC_I4 *data);

//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::getItemInternal()"
//BOPI
// !IROUTINE:  Grid::getItemInternal()"
//
// !INTERFACE:
template <class TYPE>
void Grid::getItemInternal(
//
// !RETURN VALUE:
 //   void
//
// !ARGUMENTS:
//
                                 int staggerloc, // (in)
                                 int item,       // (in)
                                 int localDE,    // (in)
                                 int *index,     // (in)  needs to be of size Grid dimCount
                                 TYPE *value     // (out) needs to just be a single value
                                 ){
//
// !DESCRIPTION:
// 
//  WARNING: NO ERROR CHECKING OR TYPE CONVERSION MAKE SURE THAT THIS IS BEING DONE IN CALLING ROUTINE 
//
//  Get item value from an index tuple. For efficiency reasons this version doesn't do error checking
//  for a public version with error checking see  Grid::getItem().  
//
//
//EOPI
//-----------------------------------------------------------------------------
  int itemIndex[ESMF_MAXDIM];
  LocalArray *localArray;
   int index1D;
  int localrc;

  // TODO: need to make this function more efficient. Use templates? 

  // For arbitrary grid, need to find the index of the 1D distgrid from the original index
  if (decompType == ESMC_GRID_NONARBITRARY) {

      //// Get LocalArray cooresponding to staggerloc, coord and localDE
    //      localArray=(itemArrayList[staggerloc][item]->getLocalarrayList())[localDE];

      localArray=(itemArrayList[staggerloc][item]->getLocalarrayList())[localDE];
      
      //// Get pointer to LocalArray data
       localArray->getDataInternal(index, value);
      
  } else {
#if 0 // Talk to PLi and then fix this
     index1D = convertIndex(index);
     //if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
     //                           &rc)) return rc;
       
       //// Get LocalArray cooresponding to staggerloc, coord and localDE
       localArray=(itemArrayList[staggerloc][item]->getLocalarrayList())[localDE];

    // WHAT TODO HERE???
       for (int i=1; i<coordDimCount[c]; i++) {
     if (coordDimMap[c][i] == ESMC_GRID_ARBDIM) {
       itemIndex[i] = index1D;
     } else {
       itemIndex[i] = index[coordDimMap[c][i]];
     }
       }
       //// Get pointer to LocalArray data
       localArray->getDataInternal(itemIndex, value);
#endif
  }
}

// Add more types here if necessary
template void Grid::getItemInternal(int staggerloc, int item, int localDE, int *index, ESMC_R8 *data);
template void Grid::getItemInternal(int staggerloc, int item, int localDE, int *index, ESMC_R4 *data);
template void Grid::getItemInternal(int staggerloc, int item, int localDE, int *index, ESMC_I4 *data);

//-----------------------------------------------------------------------------



 //-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::getItemInternalConvert()"
 //BOPI
 // !IROUTINE:  Grid::getItemInternalConvert()"
//
// !INTERFACE:
template <class TYPE>
int Grid::getItemInternalConvert(
 //
// !RETURN VALUE:
//   void
//
// !ARGUMENTS:
//
                                 int staggerloc, // (in)
                                 int item,       // (in)
                                 int localDE,    // (in)
                                 int *index,     // (in)  needs to be of size Grid dimCount
                                 TYPE *value     // (out) needs to be just a single value
                                 ){
//
// !DESCRIPTION:
//
//  WARNING: NO ERROR CHECKING MAKE SURE THIS IS BEING DONE IN CALLING ROUTINE
//
//  Get coordinates from an index tuple. For efficiency reasons this version doesn't do error checking
//  for a public version with error checking see  Grid::getCoord(). However, this version does convert
//  types of the coordinates if necessary
//
//
 //EOPI
//-----------------------------------------------------------------------------

  // Get item typekind
  ESMC_TypeKind_Flag itemtk=itemArrayList[staggerloc][item]->getTypekind();

  // Get coordinates depending on type of the grid
  if (itemtk == ESMC_TYPEKIND_R8) {
    ESMC_R8 tmp_value;
 
    // get coordinates
    getItemInternal(staggerloc, item, localDE, index, &tmp_value);
    
    // Copy/convert to output
    *value=static_cast<TYPE>(tmp_value);
  } else if (itemtk == ESMC_TYPEKIND_R4) {
    ESMC_R4 tmp_value;

    // get coordinates
    getItemInternal(staggerloc, item, localDE, index, &tmp_value);
    
    // Copy/convert to output
    *value=static_cast<TYPE>(tmp_value);
   } else if (itemtk == ESMC_TYPEKIND_I4) {
    ESMC_I4 tmp_value;

    // get coordinates
    getItemInternal(staggerloc, item, localDE, index, &tmp_value);
    
    // Copy/convert to output
    *value=static_cast<TYPE>(tmp_value);
  } else {
    int localrc;
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
                                  "- Unsupported typekind in conversion", ESMC_CONTEXT, &localrc);
    return localrc;
  }

  // return success
  return ESMF_SUCCESS;
}

// Add more types here if necessary
template int Grid::getItemInternalConvert(int staggerloc, int item, int localDE, int *index, ESMC_R8 *data);
template int Grid::getItemInternalConvert(int staggerloc, int item, int localDE, int *index, ESMC_R4 *data);
template int Grid::getItemInternalConvert(int staggerloc, int item, int localDE, int *index, ESMC_I4 *data);
 

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::getItem()"
//BOPI
// !IROUTINE:  Grid::getItem()"
//
// !INTERFACE:
template <class TYPE>
int Grid::getItem(
//
// !RETURN VALUE:
//   return code
 //   
// !ARGUMENTS:
//
                                 int staggerloc, // (in)
                                 int item,       // (in)
                                 int localDE,    // (in)
                                 int *index,     // (in)  needs to be of size Grid dimCount
                                 TYPE *value     // (out) needs to be only 1 value
                                 ){
//
// !DESCRIPTION:
//  Get item value from an index tuple. For efficiency reasons this version doesn't do error checking
//  for a public version with error checking see  Grid::getItem().  
//
//
//EOPI
//-----------------------------------------------------------------------------
  int itemIndex[ESMF_MAXDIM];
  LocalArray *localArray;
  int localrc = ESMC_RC_NOT_IMPL;
  int rc = ESMC_RC_NOT_IMPL;
  int index1D;

  // Check status
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
      "- Grid not fully created", ESMC_CONTEXT, &rc);
    return rc;
  }

  // Check typekind
  if (itemArrayList[staggerloc][item]->getTypekind() != get_TypeKind_from_Type<TYPE>()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
               "- requested type does not match type of data in item ", ESMC_CONTEXT, &rc);
    return rc;
  }

  // Check item
  if ((item < 0) || (item >= ESMC_GRIDITEM_COUNT)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
      "- item out of range", ESMC_CONTEXT, &rc);
    return rc;
  }

  // Check staggerloc
  if ((staggerloc < 0) || (staggerloc >= staggerLocCount)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
      "- stagger location out of range", ESMC_CONTEXT, &rc);
    return rc;
  }

  // Ensure localDE isn't out of range for this PET
  if ((localDE < 0) || (localDE >=distgrid->getDELayout()->getLocalDeCount())) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
          "- localDE outside range on this processor", ESMC_CONTEXT, &rc);
        return rc;
  }

  // Check here for coordinate Array existance
  if (!hasItemStaggerLoc(staggerloc,item)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
               "- staggerloc is empty on this Grid", ESMC_CONTEXT, &rc);
    return rc;
  }

  // Check typekind
  if (itemArrayList[staggerloc][item]->getTypekind() != get_TypeKind_from_Type<TYPE>()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
               "- requested type does not match type of data in item ", ESMC_CONTEXT, &rc);
    return rc;
  }

  // For arbitrary grid, need to find the index of the 1D distgrid from the original index
  if (decompType == ESMC_GRID_NONARBITRARY) {

      //// Get LocalArray cooresponding to staggerloc, coord and localDE
      localArray=(itemArrayList[staggerloc][item]->getLocalarrayList())[localDE];
      
      //// Get pointer to LocalArray data
      localArray->getDataInternal(index, value);
      
  } else {
#if 0 // Talk to PLi and then fix this
     index1D = convertIndex(index);
     //if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
     //                           &rc)) return rc;
       
       //// Get LocalArray cooresponding to staggerloc, coord and localDE
       localArray=(itemArrayList[staggerloc][item]->getLocalarrayList())[localDE];

    // WHAT TODO HERE???
       for (int i=1; i<coordDimCount[c]; i++) {
     if (coordDimMap[c][i] == ESMC_GRID_ARBDIM) {
       itemIndex[i] = index1D;
     } else {
       itemIndex[i] = index[coordDimMap[c][i]];
     }
        }
       //// Get pointer to LocalArray data
       localArray->getDataInternal(itemIndex, value);
     
#endif
  }

  // return success
  return ESMF_SUCCESS;
}

// Add more types here if necessary
template int Grid::getItem(int staggerloc, int item, int localDE, int *index, ESMC_R8 *data);
template int Grid::getItem(int staggerloc, int item, int localDE, int *index, ESMC_R4 *data);
template int Grid::getItem(int staggerloc, int item, int localDE, int *index, ESMC_I4 *data);

//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::convertIndex()"
//BOPI
// !IROUTINE:  Grid::convertIndex()
//
// !INTERFACE:
int Grid::convertIndex(
//
// !RETURN VALUE:
//   return the 1D index of the distgrid for a nD arbitrary grid index
//   
// !ARGUMENTS:
//
                                 int *index   // (in)
                                 ){
//
// !DESCRIPTION:
//  convert the grid index into the 1D index of the associated grid, for arbitrary 
//  grid only.  If the index is not found in the local DE, return ERROR
//
//
//EOPI
//-----------------------------------------------------------------------------
  int distIndex[ESMF_MAXDIM];
  int rc = ESMC_RC_NOT_IMPL;
  int index1D=-1;
  bool found;
  int i, j;

  // Check status
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
      "- Grid not fully created", ESMC_CONTEXT, &rc);
    return index1D;
  }
  for (int i=0; i<dimCount; i++) {
    if (gridIsDist[i]) {
      distIndex[gridMapDim[i]] =index[i];
    }
  }
  // Search LocalIndices array for a match with distDim
  // The local index array is not sorted, so can't search it fast.  
  // If we sort the index array, the search will be faster -- TODO
  found = false;
  for (i=0; i<localArbIndexCount; i++) {
    for (j=0; j<distDimCount; j++) {
      if (localArbIndex[i][j] != distIndex[j]) break;
    }
    if (j==distDimCount) {
      found = true;
      break;
    }
  }
  if (found) {
    index1D = i+1;
  } else {
    index1D = -1;
  }
  return index1D;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::hasCoordStaggerLoc()"
//BOPI
// !IROUTINE:  Grid::hasCoordStaggerLoc()
//
// !INTERFACE:
bool Grid::hasCoordStaggerLoc(
//
// !RETURN VALUE:
//   true if staggerloc is allocated in the Grid
//   
// !ARGUMENTS:
//
                                 int staggerloc // (in)
                                 ){
//
// !DESCRIPTION:
//  Used to detect if staggerloc has been allocated in the Grid.
//
//
//EOPI
//-----------------------------------------------------------------------------

  // Check status
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
    return false;
  }

  // Check staggerloc
  if ((staggerloc < 0) || (staggerloc >= staggerLocCount)) {
    return false;
  }

  // Loop Getting coordinates
  for (int c=0; c<dimCount; c++) {
    if (coordArrayList[staggerloc][c] == ESMC_NULL_POINTER) return false;
  }
  
  // return success
  return true;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::hasItemStaggerLoc()"
//BOPI
// !IROUTINE:  Grid::hasItemStaggerLoc()
//
// !INTERFACE:
bool Grid::hasItemStaggerLoc(
//
// !RETURN VALUE:
//   true if staggerloc has a item allocated in the Grid
//   
// !ARGUMENTS:
//
                                 int staggerloc,  // (in)
                                 int item         // (in)
                                 ){
//
// !DESCRIPTION:
//  Used to detect if staggerloc has been allocated in the Grid.
//
//
//EOPI
//-----------------------------------------------------------------------------

  // Check status
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
    return false;
  }

  // Check staggerloc
  if ((staggerloc < 0) || (staggerloc >= staggerLocCount)) {
    return false;
  }

  // Check item
  if ((item < 0) || (item >= ESMC_GRIDITEM_COUNT)) {
    return false;
  }

  // Check for valid Item pointer
  if (itemArrayList[staggerloc][item] == ESMC_NULL_POINTER) return false;
  
  // return success
  return true;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::getLDEStaggerLOffset()"
//BOPI
// !IROUTINE:  Grid::getLDEStaggerLOffset()"
//
// !INTERFACE:
int Grid::getLDEStaggerLOffset(
//
// !RETURN VALUE:
//   return code
//
// !ARGUMENTS:
//
  int staggerlocArg,  // (in)
  int localDEArg,     // (in)
  int *LWidthArg      // (out) needs to be of the same size as the grid dimCount
  ){
//
// !DESCRIPTION:
//   Returns the amount the lower end of the exclusive region of
//   this local DE should be shifted upward
//   to match the lower side of the computational region for this stagger location.
//
//EOPI
//-----------------------------------------------------------------------------
  int rc;

  // initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  
  // Check status
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Grid not fully created", ESMC_CONTEXT, &rc);
    return rc;
  }

  // Check staggerloc
  if ((staggerlocArg < 0) || (staggerlocArg >= staggerLocCount)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- stagger location out of range", ESMC_CONTEXT, &rc);
    return rc;
  }

  // Ensure localDEArg isn't out of range for this PET
  if ((localDEArg < 0) || (localDEArg >=distgrid->getDELayout()->getLocalDeCount())) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
          "- localDE outside range on this processor", ESMC_CONTEXT, &rc);
        return rc;
  }

  // Loop through grid dimCount dimensions adding the staggerLwidth
  // if we're on a lower boundary 
  for (int i=0; i<dimCount; i++) {
    if (isDELBnd[localDEArg] & (0x1 << i)) {
      LWidthArg[i]=staggerEdgeLWidthList[staggerlocArg][i];
    } else {
      LWidthArg[i]=0;
    }
  }

  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::getLDEStaggerUOffset()"
//BOPI
// !IROUTINE:  Grid::getLDEStaggerUOffset()"
//
// !INTERFACE:
int Grid::getLDEStaggerUOffset(
//
// !RETURN VALUE:
//   return code
//
// !ARGUMENTS:
//
  int staggerlocArg, // (in)
  int localDEArg,    // (in)
  int *UWidthArg     // (out) needs to be of the same size as the grid dimCount
  ){
//
// !DESCRIPTION:
//   Returns the amount the Upper end of the exclusive region of
//   this local DE should be shifted downward
//   to get to the upper side of the computational region for this stagger location.
//
//EOPI
//-----------------------------------------------------------------------------
  int rc;

  // initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  
  // Check status
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Grid not fully created", ESMC_CONTEXT, &rc);
    return rc;
  }

  // Check staggerloc
  if ((staggerlocArg < 0) || (staggerlocArg >= staggerLocCount)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- stagger location out of range", ESMC_CONTEXT, &rc);
    return rc;
  }

  // Input Error Checking
  if ((localDEArg < 0) || (localDEArg >=distgrid->getDELayout()->getLocalDeCount())) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
          "- localDE outside range on this processor", ESMC_CONTEXT, &rc);
        return rc;
  }

  // Loop through grid dimCount dimensions setting the staggerEdgeUWidth
  // for dimensions for which this local de is on the upper boundary
  for (int i=0; i<dimCount; i++) {
    if (isDEUBnd[localDEArg] & (0x1 << i)) {
      UWidthArg[i]=staggerEdgeUWidthList[staggerlocArg][i];
    } else {
      UWidthArg[i]=0;
    }
  }

  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::set()"
//BOP
// !IROUTINE:  set
//
// !INTERFACE:
int Grid::set(
//
// !RETURN VALUE:
//   Error code
//
// !ARGUMENTS:
//
  int nameLenArg,                // (in) optional
  char *nameArg,                 // (in) optional
  ESMC_TypeKind_Flag *typekindArg,    // (in) optional
  DistGrid *distgridArg,         // (in) optional
  InterArray<int> *gridEdgeLWidthArg,  // (in) optional
  InterArray<int> *gridEdgeUWidthArg,  // (in) optional
  InterArray<int> *gridAlignArg,       // (in) optional
  InterArray<int> *distgridToGridMapArg,       // (in) optional
  InterArray<int> *distDimArg,          // (in) optional
  InterArray<int> *minIndexArg,           // (int) optional
  InterArray<int> *maxIndexArg,           // (int) optional
  InterArray<int> *localArbIndexArg,           // (int) optional
  int  *localArbIndexCountArg,                    // (int) optional
  ESMC_CoordSys_Flag *coordSysArg, 
  InterArray<int> *coordDimCountArg,    // (in) optional
  InterArray<int> *coordDimMapArg,  // (in) optional
  InterArray<int> *gridMemLBoundArg,          // (in)
  ESMC_IndexFlag *indexflagArg,   // (in) optional
  bool *destroyDistgridArg,
  bool *destroyDELayoutArg
  ){
//
// !DESCRIPTION:
//   As the second part of the create empty/set/commit incremental grid 
//  creation paradigm, this subroutine is used to set values in a Grid in
//  preparation for  a later commit. This method may be called multiple times 
//  to set different sets of parameters. If the same parameter is set twice,
//  the second value overwrites the first. 
//   
//   TODO: eventually separate this into a bunch of separate sets to allow 
//         easier access from C.
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int rc;
  int localrc;                 // local error status

  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  rc = ESMC_RC_NOT_IMPL;

  // Make sure that we haven't been created
  if (status != ESMC_GRIDSTATUS_NOT_READY) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_WRONG,
      "- Can't use set on an already created object", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  // Make sure the protoGrid exists
  if (proto == ESMC_NULL_POINTER) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_BAD,
      "- Null protoGrid ", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  // if passed in, set name 
  if (nameArg != ESMC_NULL_POINTER && nameLenArg > 0) {
    // if present get rid of the old data
    if (proto->name!=ESMC_NULL_POINTER) delete [] proto->name;

    // record name
    proto->nameLen=nameLenArg;
    proto->name= new char[nameLenArg];
    memcpy(proto->name, nameArg, nameLenArg * sizeof(char));

    //// Convert F90 name string to C++ string 
    char *name = ESMC_F90toCstring(nameArg, nameLenArg);
    if (!name && nameLenArg){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
                                    "- Not a valid string", ESMC_CONTEXT, &rc);
      return rc;
    }

    // Set in base (so it can be pulled out when grid is still empty)
    localrc = ESMC_BaseSetName(name, "Grid");
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc))
      return rc;
  }

  //  if passed in, set typekind
  if (typekindArg != ESMC_NULL_POINTER) {
    if (proto->typekind == ESMC_NULL_POINTER) proto->typekind= new ESMC_TypeKind_Flag;
    *(proto->typekind)=*typekindArg;
  }

  //  if passed in, set typekind
  if (coordSysArg != ESMC_NULL_POINTER) {
    if (proto->coordSys == ESMC_NULL_POINTER) proto->coordSys= new ESMC_CoordSys_Flag;
    *(proto->coordSys)=*coordSysArg;
  }

  // if passed in, set distgrid
  if (distgridArg != ESMC_NULL_POINTER) {
    proto->distgrid=distgridArg;
  }

  // if passed in, set gridEdgeLWidth
  if (present(gridEdgeLWidthArg)) { 
    // if present get rid of the old data
    if (present(proto->gridEdgeLWidth)) _freeInterArray(&proto->gridEdgeLWidth);

    // record the new data
    proto->gridEdgeLWidth=_copyInterArray(gridEdgeLWidthArg);
  }

  // if passed in, set gridEdgeUWidth
  if (present(gridEdgeUWidthArg)) { 
    // if present get rid of the old data
    if (present(proto->gridEdgeUWidth)) _freeInterArray(&proto->gridEdgeUWidth);

    // record the new data
    proto->gridEdgeUWidth=_copyInterArray(gridEdgeUWidthArg);
  }

  // if passed in, set gridAlign
  if (present(gridAlignArg)) { 
    // if present get rid of the old data
    if (present(proto->gridAlign)) _freeInterArray(&proto->gridAlign);

    // record the new data
    proto->gridAlign=_copyInterArray(gridAlignArg);
  }

  // if passed in, set distgridToGridMap
  if (present(distgridToGridMapArg)) { 
    // if present get rid of the old data
    if (present(proto->distgridToGridMap)) _freeInterArray(&proto->distgridToGridMap);

    // record the new data
    proto->distgridToGridMap=_copyInterArray(distgridToGridMapArg);
  }

  // if passed in, set distDim
  if (present(distDimArg)) { 
    // if present get rid of the old data
    if (present(proto->distDim)) _freeInterArray(&proto->distDim);

    // record the new data
    proto->distDim=_copyInterArray(distDimArg);
  }

  // if passed in, set minIndex
  if (present(minIndexArg)) {
    // if present get rid of the old data
    if (present(proto->minIndex)) _freeInterArray(&proto->minIndex);

    // record the new data
    proto->minIndex = _copyInterArray(minIndexArg);
  }

  // if passed in, set maxIndex
  if (present(maxIndexArg)) {
    // if present get rid of the old data
    if (present(proto->maxIndex)) _freeInterArray(&proto->maxIndex);

    // record the new data
    proto->maxIndex = _copyInterArray(maxIndexArg);
  }

  // if passed in, set localArbIndex
  if (present(localArbIndexArg)) {
    // if present get rid of the old data
    if (present(proto->localArbIndex)) _freeInterArray(&proto->localArbIndex);

    // record the new data
    proto->localArbIndex = _copyInterArray(localArbIndexArg);
  }

  // if passed in, set localArbIndexCount
  if (localArbIndexCountArg != ESMC_NULL_POINTER) {
    proto->localArbIndexCount = *localArbIndexCountArg;
  }

  // if passed in, set coordDimCount
  if (present(coordDimCountArg)) { 
    // if present get rid of the old data
    if (present(proto->coordDimCount)) _freeInterArray(&proto->coordDimCount);

    // record the new data
    proto->coordDimCount=_copyInterArray(coordDimCountArg);
  }

  // if passed in, set coordDimMap
  if (present(coordDimMapArg)) { 
    // if present get rid of the old data
    if (present(proto->coordDimMap)) _freeInterArray(&proto->coordDimMap);

    // record the new data
    proto->coordDimMap=_copyInterArray(coordDimMapArg);
  }

  // if passed in, set gridMemLBoundArg
  if (present(gridMemLBoundArg)) { 
    // if present get rid of the old data
    if (present(proto->gridMemLBound)) _freeInterArray(&proto->gridMemLBound);

    // record the new data
    proto->gridMemLBound=_copyInterArray(gridMemLBoundArg);
  }

  // if passed in, set indexflag
  if (indexflagArg!=NULL) {
    if (proto->indexflag == ESMC_NULL_POINTER) proto->indexflag= new ESMC_IndexFlag;
    *(proto->indexflag)=*indexflagArg;
  }
 
  // if passed in, set destroyDistgrid
  if (destroyDistgridArg!=NULL) {
    if (proto->destroyDistgrid == ESMC_NULL_POINTER) proto->destroyDistgrid= new bool;
    *(proto->destroyDistgrid)=*destroyDistgridArg;
  }

  // if passed in, set destroyDistgrid
  if (destroyDELayoutArg!=NULL) {
    if (proto->destroyDELayout == ESMC_NULL_POINTER) proto->destroyDELayout= new bool;
    *(proto->destroyDELayout)=*destroyDELayoutArg;
  }



  // return successfully
  return ESMF_SUCCESS;
  }
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::setCoordArray()"
//BOP
// !IROUTINE:  setCoordArray
//
// !INTERFACE:
int Grid::setCoordArray(

//
// !RETURN VALUE:
//   Return code
//
// !ARGUMENTS:
//
                        int *staggerlocArg,        // (in) optional
                        int *coordArg,             // (in) 
                        Array *arrayArg,           // (in)
                        DataCopyFlag *docopyArg   // (in) optional
  ) {
//
// !DESCRIPTION:
//    Set {\tt arrayArg} as the coordinate Array for stagger location 
// {\tt staggerlocArg} and coordinate component {\tt coordArg}. Use either
// a copy or a direct reference depending on the value of {\tt docopyArg}.
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;
  int rc;
  int staggerloc;
  int coord;
  DataCopyFlag docopy;
  const int *distgridToArrayMap, *arrayUndistLBound, *arrayUndistUBound;
  const int *gridUndistLBound, *gridUndistUBound;
  bool ok;  

   // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL; 
  rc = ESMC_RC_NOT_IMPL;
  
  // make sure grid is active
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
      "- grid not of correct status to perform this operation", ESMC_CONTEXT,
      &rc);
    return rc;
  }  


  // If staggerlocArg not passed in, then set a default otherwise copy
  if (staggerlocArg==NULL) {
    staggerloc=0;  // default to center
  } else {
    staggerloc=*staggerlocArg;
  }


  // Check staggerloc
  if ((staggerloc < 0) || (staggerloc >= staggerLocCount)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- stagger location out of range", ESMC_CONTEXT, &rc);
    return rc;
  }

  // Make sure a valid coordinate component has been passed in
  // and then translate to 0-based. 
  if (coordArg==NULL) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Must pass in coord argument", ESMC_CONTEXT, &rc);
    return rc;

  } else {
    coord=(*coordArg)-1; // translate from 1 based to 0 based
  }


    // Check coord
  if ((coord < 0) || (coord >= dimCount)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- coord out of range", ESMC_CONTEXT, &rc);
    return rc;
  }


  // Make sure arrayArg is a valid pointer
  if (arrayArg == NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to array argument", ESMC_CONTEXT, &rc);
    return rc;
  }

  // If docopyArg hasn't been passed in use a default otherwise, copy it. 
  if (docopyArg==NULL) {
    docopy=DATACOPY_REFERENCE;  // default
  } else {
    docopy=*docopyArg;
  }

  // Don't support copy right now
  if (docopy==DATACOPY_VALUE) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
        "- Data Copy Flag not implemented yet", ESMC_CONTEXT, &rc);
      return rc;
  }

 
  // Ensure the passed in array has the correct dimCount
  if (coordDimCount[coord] != arrayArg->getRank()){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "- Array and Grid coord dimCount mismatch ", ESMC_CONTEXT, &rc);
      return rc;
    }

  // Ensure the passed in array has the correct typekind
  if (typekind != arrayArg->getTypekind()){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "- Array and Grid TypeKind mismatch ", ESMC_CONTEXT, &rc);
      return rc;
    }

  // Ensure the passed in array has the correct distgrid
  DistGrid *staggerDistgrid;

  // Get distgrid for this staggerloc 
  localrc=this->getStaggerDistgrid(staggerloc, &staggerDistgrid);
  if (ESMC_LogDefault.MsgFoundError(localrc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;        

  // Make sure that they match
  if (!DistGrid::match(staggerDistgrid, arrayArg->getDistGrid())) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "- Array and Grid DistGrid mismatch ", ESMC_CONTEXT, &rc);
      return rc;
    }

  // Ensure the passed in array has the correct indexflag
  if (indexflag != arrayArg->getIndexflag()){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "- Array and Grid Indexflag mismatch ", ESMC_CONTEXT, &rc);
      return rc;
    }

  // Skip all the checkings for the arbitrary grid

  if (decompType == ESMC_GRID_NONARBITRARY) {

    // Check that the passed in Array's dimmap is consistant with this coord's
    int distgridToCoordMap[ESMF_MAXDIM];

    //// Init coordDimMap (loop over entire distGrid dimCount)
    for (int i=0; i<distDimCount; i++) {
      distgridToCoordMap[i]=0;
    }

    //// Construct coordinate analog to array dimmap (will be 1-based)
    for (int i=0; i<coordDimCount[coord]; i++) {
      if (coordIsDist[coord][i]) {
    distgridToCoordMap[coordMapDim[coord][i]]=i+1; // convert to 1-based
      }
    }
    
    //// get the Array's dimmap 
    distgridToArrayMap=arrayArg->getDistGridToArrayMap();

    //// Check if the Array's dimmap matches what we expect for the coord
    ok=true;
    for (int i=0; i<distDimCount; i++) {
      if (distgridToCoordMap[i] != distgridToArrayMap[i]) ok=false;
    }
    if (!ok) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "- Array and Grid Coord dimension mapping mismatch ", ESMC_CONTEXT, &rc);
      return rc;
    }
  }

    // If distgrids match then exclusive bounds match, so don't need to check anything. 
    // TODO: However, may eventually want to check totalBounds and computationalBounds

    
    // If we've reached this point then arrayArg is of the right size and shape
    // to hold the coordinates in coord, so put it in. 
    rc=this->setCoordArrayInternal(staggerloc, coord, arrayArg, false);
    
    // return what setCoordArrayInternal returned
    return rc;
  }
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::setItemArray()"
//BOP
// !IROUTINE:  setItemArray
//
// !INTERFACE:
int Grid::setItemArray(

//
// !RETURN VALUE:
//   Return code
//
// !ARGUMENTS:
//
                        int *staggerlocArg,        // (in) optional
                        int *itemArg,              // (in)
                        Array *arrayArg,           // (in)
                        DataCopyFlag *docopyArg   // (in) optional
  ) {
//
// !DESCRIPTION:
//    Set {\tt arrayArg} as the item Array for stagger location 
// {\tt staggerlocArg} and coordinate component {\tt coordArg}. Use either
// a copy or a direct reference depending on the value of {\tt docopyArg}.
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;
  int rc;
  int staggerloc, item;
  DataCopyFlag docopy;
  const int *distgridToArrayMap;
  bool ok;  
  DistGrid *staggerDistgrid;

   // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL; 
  rc = ESMC_RC_NOT_IMPL;
  
  // make sure grid is active
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
      "- grid not of correct status to perform this operation", ESMC_CONTEXT, &rc);
    return rc;
  }  


  // If staggerlocArg not passed in, then set a default otherwise copy
  if (staggerlocArg==NULL) {
    staggerloc=0;  // default to center
  } else {
    staggerloc=*staggerlocArg;
  }
  
  // Translate itemArg to item
  if (itemArg==NULL) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
      "- must provide item", ESMC_CONTEXT, &rc);
    return rc;
  } else {
    item=*itemArg;
  }

  // Error check item
  if ((item < 0) || (item >= ESMC_GRIDITEM_COUNT)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
       "- Invalid item type", ESMC_CONTEXT, &rc);
    return rc;
  } 

  // Make sure arrayArg is a valid pointer
  if (arrayArg == NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to array argument", ESMC_CONTEXT, &rc);
    return rc;
  }

  // If docopyArg hasn't been passed in use a default otherwise, copy it. 
  if (docopyArg==NULL) {
    docopy=DATACOPY_REFERENCE;  // default
  } else {
    docopy=*docopyArg;
  }

  // Don't support copy right now
  if (docopy==DATACOPY_VALUE) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
        "- Data Copy Flag not implemented yet", ESMC_CONTEXT, &rc);
      return rc;
  }


  // Get distgrid for this staggerloc 
  localrc=this->getStaggerDistgrid(staggerloc, &staggerDistgrid);
  if (ESMC_LogDefault.MsgFoundError(localrc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;        

  // Ensure the passed in array has the correct dimCount
  if (decompType == ESMC_GRID_NONARBITRARY) {
    // for non-arbitrary grid, the item array has the same dim count as the grid
    if (dimCount != arrayArg->getRank()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "- Array and Grid coord dimCount mismatch ", ESMC_CONTEXT, &rc);
        return rc;
    } 
  } else {
    // for arbitrary grid, the item array has the same dim count as the distgrid
    int distgridDimCount = dimCount - distDimCount + 1;
    if (distgridDimCount != arrayArg->getRank()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "- Array and DistGrid dimCount mismatch ", ESMC_CONTEXT, &rc);
        return rc;
    } 
  }
  // Ensure the passed in array has the correct typekind
  if ((item == ESMC_GRIDITEM_MASK) && 
      (ESMC_TYPEKIND_I4 != arrayArg->getTypekind())){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "- Grid Mask should be of type ESMC_TYPEKIND_I4 ", ESMC_CONTEXT, &rc);
      return rc;
    }

  // Ensure the passed in array has the correct distgrid
  if (!DistGrid::match(staggerDistgrid, arrayArg->getDistGrid())) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "- Array and Grid DistGrid mismatch ", ESMC_CONTEXT, &rc);
      return rc;
    }


  // Ensure the passed in array has the correct indexflag
  if (indexflag != arrayArg->getIndexflag()){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "- Array and Grid Indexflag mismatch ", ESMC_CONTEXT, &rc);
      return rc;
    }

  // Skip the distgridToArrayMap and bound checkings for the arbitrary grid
  if (decompType == ESMC_GRID_NONARBITRARY) {

     // get the Array's dimmap 
     distgridToArrayMap=arrayArg->getDistGridToArrayMap();

     //// Check if the Array's dimmap matches what we expect for the coord
     ok=true;
     for (int i=0; i<dimCount; i++) {
        if (distgridToGridMap[i]+1 != distgridToArrayMap[i]) ok=false;
     }
     if (!ok) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
           "- Array and Grid to distgrid mapping mismatch ", ESMC_CONTEXT, &rc);
        return rc;
     }     
  }

    // If distgrids match then exclusive bounds match, so don't need to check anything. 
    // TODO: However, may eventually want to check totalBounds and computationalBounds

  // If we've reached this point then arrayArg is of the right size and shape
  // to hold the coordinates in coord, so put it in. 
  rc=this->setItemArrayInternal(staggerloc, item, arrayArg, false);

  // return what setItemArrayInternal returned
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
//  Grid Class Internal Routines:
//
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::addProtoGrid()"
//BOPI
// !IROUTINE:  addProtoGrid
//
// !INTERFACE:
int Grid::addProtoGrid(
//
// !RETURN VALUE:
//    error code
//
// !ARGUMENTS:
//
  ){
//
// !DESCRIPTION:
//   Adds a protogrid to a grid. The protogrid is to hold data for the 
// set/commit paradigm
//
//EOPI
//-----------------------------------------------------------------------------
  int rc;

  if (status != ESMC_GRIDSTATUS_NOT_READY) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_WRONG,
      "- Can't add a protogrid to an already created Grid", ESMC_CONTEXT, &rc);
    return rc;
  }

  if (proto != ESMC_NULL_POINTER) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_WRONG,
      "- this Grid already has a protogrid", ESMC_CONTEXT, &rc);
    return rc;
  }

  proto=new ProtoGrid();

  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::constructInternal()"
//BOPI
// !IROUTINE:  constructInternal
//
// !INTERFACE:
int Grid::constructInternal(
//
// !RETURN VALUE:
//    error code
//
// !ARGUMENTS:
//
  char *nameArg,                          // (in)
  ESMC_TypeKind_Flag typekindArg,              // (in)
  DistGrid *distgridArg,                  // (in)
  int distDimCountArg,                    // (in)
  int *distgridToGridMapArg,              // (in)
  int undistDimCountArg,                  // (in)
  int *undistLBoundArg,                   // (in)
  int *undistUBoundArg,                   // (in)
  int dimCountArg,                        // (in)
  int *gridEdgeLWidthArg,                 // (in)
  int *gridEdgeUWidthArg,                 // (in)
  int *gridAlignArg,                      // (in)
  ESMC_CoordSys_Flag coordSysArg, 
  int *coordDimCountArg,                  // (in)
  int **coordDimMapArg,                   // (in)
  int *gridMemLBoundArg,                  // (in)
  ESMC_IndexFlag indexflagArg,            // (in)
  int *minIndexArg,                       // (in)
  int *maxIndexArg,                       // (in)
  int **localArbIndexArg,                  // (in)
  int localArbIndexCountArg,                      // (in)
  int arbDimArg,                          // (in)
  bool destroyDistgridArg, 
  bool destroyDELayoutArg 
  ){
//
// !DESCRIPTION:
//   Construct the internal information structure of an ESMC\_Grid object.
//  No error checking wrt consistency of input arguments is needed because
//  constructInternal() is only to be called by construct() interfaces which
//  are responsible for providing consistent arguments to this layer.
//
//EOPI
//-----------------------------------------------------------------------------
   int rc,localrc;


  // Init lat lon flag
   // Eventually this'll come through the interface
  coordSys=coordSysArg;

  // Connections aren't being forced at the start
  forceConn=false;

  // Copy values into the grid object
  typekind = typekindArg;

  distgrid = distgridArg;

  // Construct distgrid_wo_poles
  _create_nopole_distgrid(distgrid, &distgrid_wo_poles, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc))
    return rc;


  distDimCount=distDimCountArg;

  undistDimCount=undistDimCountArg;

  dimCount = dimCountArg;

  indexflag=indexflagArg;

  destroyDistgrid=destroyDistgridArg; 
  destroyDELayout=destroyDELayoutArg;

  arbDim = arbDimArg;

  // Set the number of stagger locations from the grid dimCount
  staggerLocCount=_NumStaggerLocsFromDimCount(dimCount); 

  // if there are distributed dimensions - allocate and copy distgridToGridMap
  if (distDimCount) {
     distgridToGridMap = new int[distDimCount];
     memcpy(distgridToGridMap, distgridToGridMapArg, distDimCount * sizeof(int));
  }

  // if there are undistributed dimensions - allocate and copy bounds
  // only for regular grid
  if (decompType == ESMC_GRID_NONARBITRARY && undistDimCount) {
    undistLBound = new int[undistDimCount];
     memcpy(undistLBound, undistLBoundArg, undistDimCount * sizeof(int));
    
    undistUBound = new int[undistDimCount];
    memcpy(undistUBound, undistUBoundArg, undistDimCount * sizeof(int));
   }

  // if there are any dimensions 
  if (dimCount) {

    //// record connL
    connL = new ESMC_GridConn[dimCount];
   
    //// record connU
    connU = new ESMC_GridConn[dimCount];

    //// Default these to no connection
    for(int i=0; i<dimCount; i++) {
      connL[i]=ESMC_GRIDCONN_NONE;
      connU[i]=ESMC_GRIDCONN_NONE;
    }
    
    // translate distgrid connections to fill connection info for poles
    _translate_distgrid_conn(distgrid, connL, connU, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc))
      return rc;

    //// record gridEdgeLWidth
    gridEdgeLWidth = new int[dimCount];
    memcpy(gridEdgeLWidth, gridEdgeLWidthArg, dimCount * sizeof(int));

    //// record gridEdgeUWidth
    gridEdgeUWidth = new int[dimCount];
    memcpy(gridEdgeUWidth, gridEdgeUWidthArg, dimCount * sizeof(int));

    //// record gridAlign
    gridAlign = new int[dimCount];
    memcpy(gridAlign, gridAlignArg, dimCount * sizeof(int));

    //// record coordDimCount
    coordDimCount = new int[dimCount];
    memcpy(coordDimCount, coordDimCountArg, dimCount * sizeof(int));
    
    //// record coordDimMap
    coordDimMap=_allocate2D<int>(dimCount,dimCount);
    for(int i=0; i<dimCount; i++)
      for(int j=0; j<dimCount; j++)
        coordDimMap[i][j]=coordDimMapArg[i][j];
    
     //// allocate coordinate Lower Width storage
    staggerEdgeLWidthList=_allocate2D<int>(staggerLocCount,dimCount);

    //// allocate coordinate Upper Width storage
     staggerEdgeUWidthList=_allocate2D<int>(staggerLocCount,dimCount);

    //// allocate coordinate Alignment storage
    staggerAlignList=_allocate2D<int>(staggerLocCount,dimCount);

    //// allocate coordinate memLBound storage
    staggerMemLBoundList=_allocate2D<int>(staggerLocCount,dimCount);

    //// set defaults for stagger alignment and stagger width
    for(int i=0; i<staggerLocCount; i++) {
      for(int j=0; j<dimCount; j++) {
        if (i & (0x1<<j)) {   // Set defaults based on the stagger location
          staggerEdgeLWidthList[i][j]=gridEdgeLWidth[j];
          staggerEdgeUWidthList[i][j]=gridEdgeUWidth[j];
          staggerAlignList[i][j]=gridAlign[j];
        } else {
          staggerEdgeLWidthList[i][j]=0;
          staggerEdgeUWidthList[i][j]=0;
          staggerAlignList[i][j]=0;
        }
    staggerMemLBoundList[i][j]=gridMemLBoundArg[j];
      }
    }

    // allocate bound arrays
    isStaggerDELBnd = new char *[staggerLocCount];
    isStaggerDEUBnd = new char *[staggerLocCount];

    //// Default to NULL
    for(int i=0; i<staggerLocCount; i++) {
      isStaggerDELBnd[i] = ESMC_NULL_POINTER;
      isStaggerDEUBnd[i] = ESMC_NULL_POINTER;
    }    
 
    //// allocate coordinate array storage
    coordArrayList=_allocate2D<Array *>(staggerLocCount,dimCount);
    for(int i=0; i<staggerLocCount; i++)
      for(int j=0; j<dimCount; j++)
        coordArrayList[i][j]=ESMC_NULL_POINTER;


    //// allocate storage for array allocation flag
    coordDidIAllocList=_allocate2D<bool>(staggerLocCount,dimCount);
    //// set default
    for(int i=0; i<staggerLocCount; i++) {
      for(int j=0; j<dimCount; j++) {
          coordDidIAllocList[i][j]=false;
      }
     }


    //// allocate coordinate array storage
    itemArrayList=_allocate2D<Array *>(staggerLocCount,ESMC_GRIDITEM_COUNT);
    for(int i=0; i<staggerLocCount; i++) {
      for(int j=0; j<ESMC_GRIDITEM_COUNT; j++) {
        itemArrayList[i][j]=ESMC_NULL_POINTER;
      }
    }

    //// allocate storage for array allocation flag
    itemDidIAllocList=_allocate2D<bool>(staggerLocCount,ESMC_GRIDITEM_COUNT);
    //// set default
    for(int i=0; i<staggerLocCount; i++) {
      for(int j=0; j<ESMC_GRIDITEM_COUNT; j++) {
          itemDidIAllocList[i][j]=false;
      }
    }

    //// Create staggerlocs for each distgrid
    staggerDistgridList=new DistGrid *[staggerLocCount];
    for (int i=0; i<staggerLocCount; i++) {
      staggerDistgridList[i]=ESMC_NULL_POINTER;
    }   


    //// setup map from Grid dimensions to distgrid or undistUBound/undistLBound 
    //// dimensions 
    ////// allocate storage for mapping
    gridIsDist=new bool[dimCount];
    gridMapDim=new int[dimCount];

    ////// init to undistributed
    for (int i=0; i<dimCount; i++) {
      gridIsDist[i]=false;
    } 

    ////// fill in map to distributed dimensions
    for(int i=0; i<distDimCount; i++) {
      gridMapDim[distgridToGridMap[i]]=i;
      gridIsDist[distgridToGridMap[i]]=true;
    }

    ////// fill in map to undistributed dimensions
    int j=0;
    for(int i=0; i<dimCount; i++) {
      if (!gridIsDist[i]) {
         gridMapDim[i]=j;
        j++;
      }
    }

     //// setup map from coord dimensions to either distgrid
     //// dimensions or the coordinate Array's undistUBound/undistLBound dimensions
     ////// Allocate storage
     coordIsDist=_allocate2D<bool>(dimCount,dimCount);
     coordMapDim=_allocate2D<int>(dimCount,dimCount);
     
     ////// Fill in per coord
     if (decompType == ESMC_GRID_NONARBITRARY) {
       for(int c=0; c<dimCount; c++) {
     int k=0;
     for (int i=0; i<coordDimCount[c]; i++) {
       int gi=coordDimMap[c][i]; // get grid dim corresponding to coord dim
       if (gridIsDist[gi]) {
         coordMapDim[c][i]=gridMapDim[gi];  // coord dim maps to the one its grid dim maps to
         coordIsDist[c][i]=true;
       } else {
         coordMapDim[c][i]=k; // else the undist dim occur in order
         coordIsDist[c][i]=false;
         k++;
       }
     }
       }
     } else {
       int* undistDimMap = new int[dimCount];
       for (int i=0; i < dimCount; i++)
     undistDimMap[i]=-1;
       for (int i=0, k=0; i<dimCount; i++) {
     if (!gridIsDist[i]) {
       if (k == arbDimArg-1) k++;
       undistDimMap[i]=k;
       k++;
     }
       }

       for(int c=0; c<dimCount; c++) {
     int k=0;
     for (int i=0; i<coordDimCount[c]; i++) {
       int gi=coordDimMap[c][i]; // get grid dim corresponding to coord dim
       // coordMapDim is the distgrid dimension mapped from the grid dimension
           // in coordDimMap
       if (gi == ESMC_GRID_ARBDIM) {
         coordMapDim[c][i] = arbDimArg-1;
         coordIsDist[c][i] = true;
        } else {
         coordMapDim[c][i] = undistDimMap[gi];  
         coordIsDist[c][i] = false;
       }
     }
       }
       delete [] undistDimMap;
     }
  }
 
  // allocate and fill isDELBnd and isDEUbnd
  // These record if the local de is on the top or bottom
  // boundary in each dimension, use distgrid without poles
  // because we don't want the poles to count
  if (decompType != ESMC_GRID_ARBITRARY){
    localrc=_createIsDEBnd(&isDELBnd,&isDEUBnd, distgrid_wo_poles, distgridToGridMap);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc))
      return rc;
  }

  // Set the name for this Grid object in the Base class
  localrc = ESMC_BaseSetName(nameArg, "Grid");
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc))
    return rc;

  // allocate and fill minIndex and maxIndex 
  minIndex = new int[dimCount];
  maxIndex = new int[dimCount];
  memcpy(minIndex, minIndexArg, sizeof(int)*dimCount);
  memcpy(maxIndex, maxIndexArg, sizeof(int)*dimCount);

  // allocate and fill local index array
  localArbIndexCount = localArbIndexCountArg;
  if (localArbIndexCount > 0) {
    localArbIndex = _allocate2D<int>(localArbIndexCount, distDimCount);
    for (int i=0; i < localArbIndexCount; i++) {
      for (int j=0; j < distDimCount; j++) {
    localArbIndex[i][j]=localArbIndexArg[i][j];
      }
    }
  }

  // Grid is now ready to be used in grid methods, so set status appropriately
  status=ESMC_GRIDSTATUS_SHAPE_READY;

  
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::delProtoGrid()"
//BOPI
// !IROUTINE:  delProtoGrid
//
// !INTERFACE:
int Grid::delProtoGrid(
//
// !RETURN VALUE:
//    error code
//
// !ARGUMENTS:
//
  ){
//
// !DESCRIPTION:
//   Removes a protogrid from a grid. The protogrid is used to record
//   values for the incremental grid creation paradigm 
//   (create empty/set/commit).
//
//EOPI
//-----------------------------------------------------------------------------
  int rc;

  // this shouldn't cause problems, but its weird enough to 
  // ring alarm bells, so if necessary remove this check 
  if (status == ESMC_GRIDSTATUS_NOT_READY) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_WRONG,
            "- removing a protogrid from an uncreated Grid", ESMC_CONTEXT, &rc); 
    return rc;
  }

  // If present delete ProtoGrid
  if (proto != ESMC_NULL_POINTER) delete proto;

  // Set to NULL so we can tell that proto has been deleted 
  proto = ESMC_NULL_POINTER;

  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::getCoordArrayInternal()"
//BOPI
// !IROUTINE:  Grid::getCoordArrayInternal
//
// !INTERFACE:
int Grid::getCoordArrayInternal(
//
// !RETURN VALUE:
//   return code
//
// !ARGUMENTS:
//
  int staggerlocArg, // (in)
  int coordArg,      // (in)
  Array **arrayArg   // (out)
  ){
//
// !DESCRIPTION:
//   Get a coordinate array from the grid structure
//
//EOPI
//-----------------------------------------------------------------------------
  int rc;
  Array *array;

  // initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  
  // Check status
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Grid not fully created", ESMC_CONTEXT, &rc);
    return rc;
  }

  // Check staggerloc
  if ((staggerlocArg < 0) || (staggerlocArg >= staggerLocCount)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- stagger location out of range", ESMC_CONTEXT, &rc);
    return rc;
  }

  // Check coord
  if ((coordArg < 0) || (coordArg >= dimCount)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- coord out of range", ESMC_CONTEXT, &rc);
    return rc;
  }

  // get Array pointer from List
  array=coordArrayList[staggerlocArg][coordArg];

  // Check if array has been set
  if (array==ESMC_NULL_POINTER) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- accessing unset coord array", ESMC_CONTEXT, &rc);
    return rc;
  }

  // output array
  *arrayArg=array;

  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::getItemArrayInternal()"
//BOPI
// !IROUTINE:  Grid::getItemArrayInternal
//
// !INTERFACE:
int Grid::getItemArrayInternal(
//
// !RETURN VALUE:
//   return code
//
// !ARGUMENTS:
//
  int staggerlocArg, // (in)
  int itemArg,       // (in)
  Array **arrayArg   // (out)
  ){
//
// !DESCRIPTION:
//   Get a item array from the grid structure
//
//EOPI
//-----------------------------------------------------------------------------
  int rc;
  Array *array;

  // initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  
  // Check status
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Grid not fully created", ESMC_CONTEXT, &rc);
    return rc;
  }

  // Check staggerloc
  if ((staggerlocArg < 0) || (staggerlocArg >= staggerLocCount)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- stagger location out of range", ESMC_CONTEXT, &rc);
    return rc;
  }

  // Check item
  if ((itemArg < 0) || (itemArg >= ESMC_GRIDITEM_COUNT)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- stagger location out of range", ESMC_CONTEXT, &rc);
    return rc;
  }

  // get Array pointer from List
  array=itemArrayList[staggerlocArg][itemArg];

  // Check if array has been set
  if (array==ESMC_NULL_POINTER) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- accessing unset coord array", ESMC_CONTEXT, &rc);
    return rc;
  }

  // output array
  *arrayArg=array;

  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid()"
//BOPI
// !IROUTINE:  GridConstruct
//
// !INTERFACE:
Grid::Grid(
//
// !RETURN VALUE:
//    Pointer to a new grid
//
// !ARGUMENTS:
//  none
  VM *vm):ESMC_Base(vm){  // allow specific VM instead default
//
// !DESCRIPTION:
//    Because of the possible use of incremental create this just
//    sets default values, the real construction of the internal
//    grid structures is done in constructInternal.
//
//EOPI
//-----------------------------------------------------------------------------

  // Init lat lon flag
  coordSys=ESMC_COORDSYS_SPH_DEG;
  
  // Start out with connections unforced
  forceConn=false;

  // Set default values for grid members
  proto = ESMC_NULL_POINTER; 
  
  status=ESMC_GRIDSTATUS_NOT_READY; // default status not ready
  decompType = ESMC_GRID_INVALID;   // grid deompose type unknonw
  
  typekind = ESMC_TYPEKIND_R8;
  distDimCount = 0;
  distgridToGridMap = ESMC_NULL_POINTER; 
  
  undistDimCount = 0;
  undistLBound = ESMC_NULL_POINTER; 
  undistUBound = ESMC_NULL_POINTER; 
  
  dimCount=0;

  connL = ESMC_NULL_POINTER; 
  connU = ESMC_NULL_POINTER; 

  gridEdgeLWidth = ESMC_NULL_POINTER; 
  gridEdgeUWidth = ESMC_NULL_POINTER; 
  gridAlign = ESMC_NULL_POINTER; 
  coordDimCount = ESMC_NULL_POINTER; 
  coordDimMap = ESMC_NULL_POINTER; 
  
  staggerLocCount=0;
  coordArrayList = ESMC_NULL_POINTER;
  staggerEdgeLWidthList = ESMC_NULL_POINTER;
  staggerEdgeUWidthList = ESMC_NULL_POINTER;
  staggerAlignList = ESMC_NULL_POINTER;
  staggerMemLBoundList = ESMC_NULL_POINTER;
  coordDidIAllocList = ESMC_NULL_POINTER;
  
  staggerDistgridList =ESMC_NULL_POINTER;

  itemArrayList = ESMC_NULL_POINTER;
  itemDidIAllocList = ESMC_NULL_POINTER;

  isStaggerDELBnd = ESMC_NULL_POINTER;
  isStaggerDEUBnd = ESMC_NULL_POINTER;

  gridIsDist = ESMC_NULL_POINTER;
  gridMapDim = ESMC_NULL_POINTER;
  
  coordIsDist = ESMC_NULL_POINTER;
  coordMapDim = ESMC_NULL_POINTER;
  
  isDELBnd = ESMC_NULL_POINTER;
  isDEUBnd = ESMC_NULL_POINTER;
  
  indexflag=ESMC_INDEX_DELOCAL;
  distgrid= ESMC_NULL_POINTER; 
  distgrid_wo_poles= ESMC_NULL_POINTER; 

  minIndex = ESMC_NULL_POINTER;
  maxIndex = ESMC_NULL_POINTER;
  localArbIndex = ESMC_NULL_POINTER;
  localArbIndexCount = -1;  

  destroyDistgrid=false; 
  destroyDELayout=false;

  // FIXME:  this is a temporary fix for AttributeUpdate() to be able to
  //         recognize that Grids are multiply created after StateReconcile()
  classID = 42;

}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid()"
//BOPI
// !IROUTINE:  GridConstruct
//
// !INTERFACE:
Grid::Grid(
//
// !RETURN VALUE:
//    Pointer to a new grid
//
// !ARGUMENTS:
  int baseID
  ):ESMC_Base(baseID){  // prevent baseID counter increment
//
// !DESCRIPTION:
//    Because of the possible use of incremental create this just
//    sets default values, the real construction of the internal
//    grid structures is done in constructInternal.
//
//EOPI
//-----------------------------------------------------------------------------

  // Set default values for grid members
  proto = ESMC_NULL_POINTER; 
  
  // Init lat lon flag
  coordSys=ESMC_COORDSYS_SPH_DEG;

  forceConn=false;

  status=ESMC_GRIDSTATUS_NOT_READY; // default status not ready
  decompType = ESMC_GRID_INVALID;   // grid deompose type unknonw
  
  typekind = ESMC_TYPEKIND_R8;
  distDimCount = 0;
  distgridToGridMap = ESMC_NULL_POINTER; 
  
  undistDimCount = 0;
  undistLBound = ESMC_NULL_POINTER; 
  undistUBound = ESMC_NULL_POINTER; 
  
  dimCount=0;

  connL = ESMC_NULL_POINTER; 
  connU = ESMC_NULL_POINTER; 

  gridEdgeLWidth = ESMC_NULL_POINTER; 
  gridEdgeUWidth = ESMC_NULL_POINTER; 
  gridAlign = ESMC_NULL_POINTER; 
  coordDimCount = ESMC_NULL_POINTER; 
  coordDimMap = ESMC_NULL_POINTER; 
  
  staggerLocCount=0;
  coordArrayList = ESMC_NULL_POINTER;
  staggerEdgeLWidthList = ESMC_NULL_POINTER;
  staggerEdgeUWidthList = ESMC_NULL_POINTER;
  staggerAlignList = ESMC_NULL_POINTER;
  staggerMemLBoundList = ESMC_NULL_POINTER;
  coordDidIAllocList = ESMC_NULL_POINTER;
  
  staggerDistgridList = ESMC_NULL_POINTER;

  itemArrayList = ESMC_NULL_POINTER;
  itemDidIAllocList = ESMC_NULL_POINTER;

  gridIsDist = ESMC_NULL_POINTER;
  gridMapDim = ESMC_NULL_POINTER;
  
  coordIsDist = ESMC_NULL_POINTER;
  coordMapDim = ESMC_NULL_POINTER;
  
  isDELBnd = ESMC_NULL_POINTER;
  isDEUBnd = ESMC_NULL_POINTER;
  
  indexflag=ESMC_INDEX_DELOCAL;
  distgrid= ESMC_NULL_POINTER; 
  distgrid_wo_poles= ESMC_NULL_POINTER; 

  minIndex = ESMC_NULL_POINTER;
  maxIndex = ESMC_NULL_POINTER;
  localArbIndex = ESMC_NULL_POINTER;
  localArbIndexCount = -1;  

  destroyDistgrid=false; 
  destroyDELayout=false;

  // FIXME:  this is a temporary fix for AttributeUpdate() to be able to
  //         recognize that Grids are multiply created after StateReconcile()
  classID = 42;

}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::destruct()"
//BOPI
// !IROUTINE:  ESMCI::Grid::destruct
//
// !INTERFACE:
void Grid::destruct(bool followCreator, bool noGarbage){
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
// none
//
// !DESCRIPTION:
//    Destruct Grid, deallocates all internal memory, etc. 
//
//EOPI
//-----------------------------------------------------------------------------
 if (ESMC_BaseGetStatus()==ESMF_STATUS_READY){
  if (getStatus() < ESMC_GRIDSTATUS_SHAPE_READY){
   // If present delete ProtoGrid
   if (proto != ESMC_NULL_POINTER) delete proto;    
  }else{
   if (followCreator){

   // Delete external class contents of Grid before deleting Grid
   //// Delete Arrays
   for(int i=0; i<staggerLocCount; i++) {
     for(int j=0; j<dimCount; j++) {
       if (coordDidIAllocList[i][j] && (coordArrayList[i][j]!=ESMC_NULL_POINTER)) {
         Array::destroy(&coordArrayList[i][j], noGarbage);
       }
     }
   }


   //// Delete Item Arrays
   for(int i=0; i<staggerLocCount; i++) {
     for(int j=0; j<ESMC_GRIDITEM_COUNT; j++) {
       if (itemDidIAllocList[i][j] && (itemArrayList[i][j]!=ESMC_NULL_POINTER)){
         Array::destroy(&itemArrayList[i][j], noGarbage);
       }
     }
   }


   // Get tmpDELayout if we need it later
   // (also if prevents us from getting layout from empty grid)
   DELayout *tmpDELayout;
   if (destroyDELayout) {
     tmpDELayout=distgrid->getDELayout();
   }

   // delete distgrid
   if (destroyDistgrid) {
     DistGrid::destroy(&distgrid, noGarbage);
   }

   // Grid created this one
   if (distgrid_wo_poles!=ESMC_NULL_POINTER)
     DistGrid::destroy(&distgrid_wo_poles, noGarbage);

   // delete delayout
   if (destroyDELayout) {
     DELayout::destroy(&tmpDELayout, noGarbage);
   }

   // Get rid of staggerDistgrids
   for (int i=0; i<staggerLocCount; i++) {
     if (staggerDistgridList[i]!=ESMC_NULL_POINTER) {
       DistGrid::destroy(&staggerDistgridList[i], noGarbage);
     }
   }
   
   }

   if (staggerDistgridList != ESMC_NULL_POINTER) delete[] staggerDistgridList;

   // If present delete ProtoGrid
   if (proto != ESMC_NULL_POINTER) delete proto;

   // delete distributed dimension stuff
   if (distDimCount) {
     if (distgridToGridMap !=ESMC_NULL_POINTER) delete [] distgridToGridMap;
   }

   // delete undistributed dimension stuff
   if (undistDimCount && decompType != ESMC_GRID_ARBITRARY) {
     if (undistLBound !=ESMC_NULL_POINTER) delete [] undistLBound;
     if (undistUBound !=ESMC_NULL_POINTER) delete [] undistUBound;
   }

   // delete all dimension stuff
   if (dimCount) {
     if (connL !=ESMC_NULL_POINTER) delete [] connL;
     if (connU !=ESMC_NULL_POINTER) delete [] connU;
     if (gridEdgeLWidth !=ESMC_NULL_POINTER) delete [] gridEdgeLWidth;
     if (gridEdgeUWidth !=ESMC_NULL_POINTER) delete [] gridEdgeUWidth;
     if (gridAlign !=ESMC_NULL_POINTER) delete [] gridAlign;
     if (coordDimCount !=ESMC_NULL_POINTER) delete [] coordDimCount;
     _free2D<int>(&coordDimMap);
      _free2D<int>(&staggerEdgeLWidthList);
     _free2D<int>(&staggerEdgeUWidthList);
     _free2D<int>(&staggerAlignList);
     _free2D<Array *>(&coordArrayList);
     _free2D<bool>(&coordDidIAllocList);

     _free2D<Array *>(&itemArrayList);
     _free2D<bool>(&itemDidIAllocList);

     _free2D<int>(&staggerMemLBoundList);

     if (gridIsDist !=ESMC_NULL_POINTER) delete [] gridIsDist;
     if (gridMapDim !=ESMC_NULL_POINTER) delete [] gridMapDim;
     _free2D<bool>(&coordIsDist); 
     _free2D<int>(&coordMapDim); 
  }

   // Get rid of stagger bounds stuff
   if (isStaggerDELBnd != ESMC_NULL_POINTER) {
     for (int i=0; i<staggerLocCount; i++) {
       if (isStaggerDELBnd[i] != ESMC_NULL_POINTER) delete [] isStaggerDELBnd[i];
     }    
     delete [] isStaggerDELBnd;
   }

   if (isStaggerDEUBnd != ESMC_NULL_POINTER) {
     for (int i=0; i<staggerLocCount; i++) {
       if (isStaggerDEUBnd[i] != ESMC_NULL_POINTER) delete [] isStaggerDEUBnd[i];
     }    
     delete [] isStaggerDEUBnd;
   }


   // delete local de bounds indicators
  if (isDELBnd != ESMC_NULL_POINTER) delete [] isDELBnd;
  if (isDEUBnd != ESMC_NULL_POINTER) delete [] isDEUBnd;


  // delete minIndex and maxIndex
  delete [] minIndex;
  delete [] maxIndex;


  // delete local indices for arbitrary grid
  if (localArbIndexCount) {
    _free2D<int>(&localArbIndex);
  }
 }
 }
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ProtoGrid()"
//BOPI
// !IROUTINE:  ProtoGridConstruct
//
// !INTERFACE:
ProtoGrid::ProtoGrid(
//
// !RETURN VALUE:
//    Pointer to a new proto grid
//
// !ARGUMENTS:
//  none
  ){
//
// !DESCRIPTION:
//    Construct the internal information structure of an ESMC\_ProtoGrid object.
//
//EOPI
//-----------------------------------------------------------------------------

  nameLen=0; 
  name=ESMC_NULL_POINTER;  
  typekind=ESMC_NULL_POINTER;
  distgrid=ESMC_NULL_POINTER;    
  gridEdgeLWidth=ESMC_NULL_POINTER;   
  gridEdgeUWidth=ESMC_NULL_POINTER;   
  gridAlign=ESMC_NULL_POINTER;   
  gridMemLBound=ESMC_NULL_POINTER;   
  distgridToGridMap=ESMC_NULL_POINTER;   
  distDim=ESMC_NULL_POINTER;   
  undistLBound=ESMC_NULL_POINTER;  
  undistUBound=ESMC_NULL_POINTER;  
  coordSys=ESMC_NULL_POINTER;  
  coordDimCount=ESMC_NULL_POINTER;  
  coordDimMap=ESMC_NULL_POINTER; 
  indexflag=ESMC_NULL_POINTER; 
  minIndex=ESMC_NULL_POINTER;
  maxIndex=ESMC_NULL_POINTER;
  localArbIndexCount=-1;
  arbDim = 1;
  localArbIndex=ESMC_NULL_POINTER;
  destroyDistgrid=ESMC_NULL_POINTER; 
  destroyDELayout=ESMC_NULL_POINTER; 
}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::~ProtoGrid()"
//BOPI
// !IROUTINE:  ~ProtoGrid
//
// !INTERFACE:
 ProtoGrid::~ProtoGrid(void){
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
// none
//
// !DESCRIPTION:
//  Destructor for ProtoGrid, deallocates all internal memory, etc. 
//
//EOPI
//-----------------------------------------------------------------------------

  if (name != ESMC_NULL_POINTER) delete [] name;  
  if (typekind != ESMC_NULL_POINTER) delete typekind;
  // don't delete distgrid
  if (gridEdgeLWidth != ESMC_NULL_POINTER) _freeInterArray(&gridEdgeLWidth);
  if (gridEdgeUWidth != ESMC_NULL_POINTER) _freeInterArray(&gridEdgeUWidth);
  if (gridAlign != ESMC_NULL_POINTER) _freeInterArray(&gridAlign);
  if (gridMemLBound != ESMC_NULL_POINTER) _freeInterArray(&gridMemLBound);
  if (distgridToGridMap != ESMC_NULL_POINTER) _freeInterArray(&distgridToGridMap);
  if (distDim != ESMC_NULL_POINTER) _freeInterArray(&distDim);
  if (undistLBound != ESMC_NULL_POINTER) _freeInterArray(&undistLBound);
  if (undistUBound != ESMC_NULL_POINTER) _freeInterArray(&undistUBound);
  if (coordSys != ESMC_NULL_POINTER) delete coordSys; 
  if (coordDimCount != ESMC_NULL_POINTER) _freeInterArray(&coordDimCount);
  if (coordDimMap != ESMC_NULL_POINTER) _freeInterArray(&coordDimMap);
  if (indexflag != ESMC_NULL_POINTER) delete indexflag; 
  if (minIndex != ESMC_NULL_POINTER) _freeInterArray(&minIndex); 
  if (maxIndex != ESMC_NULL_POINTER) _freeInterArray(&maxIndex); 
  if (localArbIndex != ESMC_NULL_POINTER) _freeInterArray(&localArbIndex);
  if (destroyDistgrid != ESMC_NULL_POINTER) delete destroyDistgrid; 
  if (destroyDELayout != ESMC_NULL_POINTER) delete destroyDELayout; 
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::setCoordArrayInternal()"
//BOPI
// !IROUTINE:  Grid::setCoordArrayInternal
//
// !INTERFACE:
int Grid::setCoordArrayInternal(
//
// !RETURN VALUE:
//   return code
//
// !ARGUMENTS:
//
  int staggerlocArg, // (in)
  int coordArg,      // (in)
  Array *arrayArg,   // (in)
  bool didIAlloc   // (in)
  ){
//
// !DESCRIPTION:
//   Set a coordinate array in the grid structure
//
//EOPI
//-----------------------------------------------------------------------------
  int rc;

  // initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  
  // Check status
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Grid not fully created", ESMC_CONTEXT, &rc);
    return rc;
  }

  // Check staggerloc
  if ((staggerlocArg < 0) || (staggerlocArg >= staggerLocCount)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- stagger location out of range", ESMC_CONTEXT, &rc);
    return rc;
  }

  // Check coord
  if ((coordArg < 0) || (coordArg >= dimCount)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- coord out of range", ESMC_CONTEXT, &rc);
    return rc;
  }

  // Set array in list
  coordArrayList[staggerlocArg][coordArg] = arrayArg;

  // Set alloc
  coordDidIAllocList[staggerlocArg][coordArg]=didIAlloc;


  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::setItemArrayInternal()"
//BOPI
// !IROUTINE:  Grid::setItemArrayInternal
//
// !INTERFACE:
int Grid::setItemArrayInternal(
//
// !RETURN VALUE:
//   return code
//
// !ARGUMENTS:
//
  int staggerlocArg, // (in)
  int itemArg, // (in)
  Array *arrayArg,   // (in)
  bool didIAlloc   // (in)
  ){
//
// !DESCRIPTION:
//   Set a item array in the grid structure
//
//EOPI
//-----------------------------------------------------------------------------
  int rc;

  // initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  
  // Check status
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Grid not fully created", ESMC_CONTEXT, &rc);
    return rc;
  }

  // Check staggerloc
  if ((staggerlocArg < 0) || (staggerlocArg >= staggerLocCount)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- stagger location out of range", ESMC_CONTEXT, &rc);
    return rc;
  }

  // Check item
  if ((itemArg < 0) || (itemArg >= ESMC_GRIDITEM_COUNT)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- grid item out of range", ESMC_CONTEXT, &rc);
    return rc;
  }

  // Set array in list
  itemArrayList[staggerlocArg][itemArg] = arrayArg;

  // Set alloc
  itemDidIAllocList[staggerlocArg][itemArg]=didIAlloc;

  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::setStaggerInfo()"
//BOP
// !IROUTINE:  setStaggerInfo
//
// !INTERFACE:
  int Grid::setStaggerInfo(

//
// !RETURN VALUE:
//   Return code
//
// !ARGUMENTS:
//
                          int staggerloc,             // (in) optional
                          InterArray<int> *staggerEdgeLWidthArg, // (in) optional
                          InterArray<int> *staggerEdgeUWidthArg, // (in) optional
                          InterArray<int> *staggerAlignArg,   // (in) optional 
                          InterArray<int> *staggerMemLBoundArg   // (in) optional 
  ) {
//
// !DESCRIPTION:
//  This call sets the stagger location info based on user input.
//  It is an error to change these once the internal distgrid has been set, 
//  but this call detects this and  just passes through transparently if the user
//  isn't attempting to change anything. This call also error checks the user input.
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int rc, localrc;
  int *staggerAlign=(int *)ESMC_NULL_POINTER;
  int *staggerMemLBound=(int *)ESMC_NULL_POINTER;
  int *staggerEdgeLWidth=(int *)ESMC_NULL_POINTER;
  int *staggerEdgeUWidth=(int *)ESMC_NULL_POINTER;
  int extent[1];

  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  rc = ESMC_RC_NOT_IMPL;
  
  // Error check LWidth
  if (present(staggerEdgeLWidthArg)) {
    //// Ensure staggerEdgeLWidth is of the correct dimCount 
    if (staggerEdgeLWidthArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- staggerEdgeLWidth array must be of dimCount 1", ESMC_CONTEXT, &rc);
      return rc;
    }
    //// Ensure staggerEdgeLWidth is of the correct size
    if (staggerEdgeLWidthArg->extent[0] != dimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
      "- staggerEdgeLWidth size and Grid dimCount mismatch ", ESMC_CONTEXT, &rc);
      return rc;
    }
    //// Ensure staggerEdgeLWidthArg values fit within gridEdgeLWidth
    for (int i=0; i<dimCount; i++){
      if ((staggerEdgeLWidthArg->array[i] < 0) || (staggerEdgeLWidthArg->array[i] > gridEdgeLWidth[i])) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
           "- staggerEdgeLWidth doesn't fit within gridEdgeLWidth", ESMC_CONTEXT, &rc);
        return rc;
      }
    }
  }

  // Error check UWidth
  if (present(staggerEdgeUWidthArg)) {
    //// Ensure staggerEdgeUWidth is of the correct dimCount 
    if (staggerEdgeUWidthArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
       "- staggerEdgeUWidth array must be of dimCount 1", ESMC_CONTEXT, &rc);
      return rc;
    }
    //// Ensure staggerEdgeUWidth is of the correct size
    if (staggerEdgeUWidthArg->extent[0] != dimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
      "- staggerEdgeUWidth size and Grid dimCount mismatch ", ESMC_CONTEXT, &rc);
      return rc;
    }
    //// Ensure staggerEdgeUWidthArg values fit within gridEdgeUWidth    
    for (int i=0; i<dimCount; i++){
      if ((staggerEdgeUWidthArg->array[i] < 0) || (staggerEdgeUWidthArg->array[i] > gridEdgeUWidth[i])) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          "- staggerEdgeUWidth doesn't fit within gridEdgeUWidth", ESMC_CONTEXT, &rc);
        return rc;
      }
    }
  }

  // Error check Align
  if (present(staggerAlignArg)) {
    //// Ensure staggerAlign has the correct dimCount
    if (staggerAlignArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- staggerAlign array must be of dimCount 1", ESMC_CONTEXT, &rc);
      return rc;
    }
    //// Ensure staggerAlign has the correct size
    if (staggerAlignArg->extent[0] != dimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "- staggerAlign size and Grid dimCount mismatch ", ESMC_CONTEXT, &rc);
      return rc;
    }
    for (int i=0; i<dimCount; i++){
      //// Ensure staggerAlign values are -1,0,1
      if ((staggerAlignArg->array[i] < -1) || (staggerAlignArg->array[i] > 1)) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                 "- staggerAlign must be either -1, 0, or 1", ESMC_CONTEXT, &rc);
        return rc;
      }
    }
  }

  // Error check Align
  if (present(staggerMemLBoundArg)) {
    //// Ensure staggerMemLBoundArg has the correct dimCount
    if (staggerMemLBoundArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- staggerMemLBound array must be of dimCount 1", ESMC_CONTEXT, &rc);
      return rc;
    }
    //// Ensure staggerAlign has the correct size
    if (staggerMemLBoundArg->extent[0] != dimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "- staggerMemLBound size and Grid dimCount mismatch ", ESMC_CONTEXT, &rc);
      return rc;
    }
  }



  // Allocate lWidth, uWidth, and Align based on inputs and defaults
  staggerEdgeLWidth = new int[dimCount];
  staggerEdgeUWidth = new int[dimCount];
  staggerAlign = new int[dimCount];
  staggerMemLBound = new int[dimCount];

  // Set lWidth, uWidth, and Align based on inputs and defaults
  localrc=setDefaultsLUA(dimCount,
          staggerEdgeLWidthArg, staggerEdgeUWidthArg, staggerAlignArg,
          staggerEdgeLWidthList[staggerloc], staggerEdgeUWidthList[staggerloc], staggerAlignList[staggerloc], 
          staggerEdgeLWidth, staggerEdgeUWidth, staggerAlign);
  if (ESMC_LogDefault.MsgFoundError(localrc,
                           ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

   
  // Set staggerMemLBound 
  if (present(staggerMemLBoundArg)) {
    for (int i=0; i<dimCount; i++) {
      staggerMemLBound[i]=staggerMemLBoundArg->array[i];
    }
  } else {
    for (int i=0; i<dimCount; i++) {
      staggerMemLBound[i]=staggerMemLBoundList[staggerloc][i];
    }
  }


  // If this stagger has already been set then error out if it's being changed
  if (staggerDistgridList[staggerloc] != ESMC_NULL_POINTER) {
    //// Error check LWidth
    for (int i=0; i<dimCount; i++) {
      if (staggerEdgeLWidth[i] != staggerEdgeLWidthList[staggerloc][i]) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
         "- staggerEdgeLWidth must not be changed once set", ESMC_CONTEXT, &rc);
        return rc;
      }
    }

    //// Error check UWidth
    for (int i=0; i<dimCount; i++) {
      if (staggerEdgeUWidth[i] != staggerEdgeUWidthList[staggerloc][i]) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
         "- staggerEdgeUWidth must not be changed once set", ESMC_CONTEXT, &rc);
        return rc;
      }
    }

    //// Error check Align
    for (int i=0; i<dimCount; i++) {
      if (staggerAlign[i] != staggerAlignList[staggerloc][i]) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
        "- staggerAlign must not be changed once set", ESMC_CONTEXT, &rc);
        return rc;
      }
    }

    //// Error check staggerMemLBound
    for (int i=0; i<dimCount; i++) {
      if (staggerMemLBound[i] != staggerMemLBoundList[staggerloc][i]) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                 "- staggerMemLBound must not be changed once set", ESMC_CONTEXT, &rc);
        return rc;
      }
    }

    // Deallocate lists
    if (staggerEdgeUWidth != ESMC_NULL_POINTER) delete [] staggerEdgeUWidth;
    if (staggerEdgeLWidth != ESMC_NULL_POINTER) delete [] staggerEdgeLWidth;
    if (staggerAlign != ESMC_NULL_POINTER) delete [] staggerAlign;
    if (staggerMemLBound != ESMC_NULL_POINTER) delete [] staggerMemLBound;
    
    // If its already set then don't need to do anything, so leave successfully
    return ESMF_SUCCESS;
  }

  // Set the stagger info in the grid arrays
  // Set staggerEdgeLWidth
  for (int i=0; i<dimCount; i++) {
    staggerEdgeLWidthList[staggerloc][i]=staggerEdgeLWidth[i];
   }

  // Set staggerEdgeUWidth
  for (int i=0; i<dimCount; i++) {
    staggerEdgeUWidthList[staggerloc][i]=staggerEdgeUWidth[i];
   }

  // Set staggerAlign
  for (int i=0; i<dimCount; i++) {
    staggerAlignList[staggerloc][i]=staggerAlign[i];
   }


  // Set staggerMemLBound
  for (int i=0; i<dimCount; i++) {
    staggerMemLBoundList[staggerloc][i]=staggerMemLBound[i];
   }

  // Deallocate lists
  if (staggerEdgeUWidth != ESMC_NULL_POINTER) delete [] staggerEdgeUWidth;
  if (staggerEdgeLWidth != ESMC_NULL_POINTER) delete [] staggerEdgeLWidth;
  if (staggerAlign != ESMC_NULL_POINTER) delete [] staggerAlign;
  if (staggerMemLBound != ESMC_NULL_POINTER) delete [] staggerMemLBound;

  //leave successfully
  return ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::getStaggerDistgrid"
//BOP
// !IROUTINE:  getStaggerDistgrid
//
// !INTERFACE:
int Grid::getStaggerDistgrid(

//
// !RETURN VALUE:
//   Return code
//
// !ARGUMENTS:
//
                          int staggerloc,      // (in) optional
                           DistGrid **distgridArg   // (in) optional 
  ) {
//
// !DESCRIPTION:
// This call gets the distgrid for a particular staggerloc. If the distgrid hasn't 
// been created yet, this call creates and sets it. This call error checks its input.
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int rc, localrc;

  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  rc = ESMC_RC_NOT_IMPL;
 
  // Check staggerloc
  if ((staggerloc < 0) || (staggerloc >= staggerLocCount)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
      "- stagger location out of range", ESMC_CONTEXT, &rc);
    return rc;
  }

  // Get TileCount
  int tileCount=distgrid_wo_poles->getTileCount();

  // If non-arbitrary get a stagger distgrid
  if (decompType == ESMC_GRID_NONARBITRARY) {
    
    //  DistGrid **staggerDistgridList; // [staggerloc]
    // If a stagger distgrid doesn't exist then create one
    if (staggerDistgridList[staggerloc] == ESMC_NULL_POINTER) {
      if (tileCount <= 1) {
        int extent[1];
      
        // Create InterArrays holding stagger padding
        extent[0]=dimCount;
        int *staggerEdgeLWidthIntIntArray=new int[dimCount];
        InterArray<int> *staggerEdgeLWidthIntInt
          = new InterArray<int>(staggerEdgeLWidthIntIntArray,1,extent);

        int *staggerEdgeUWidthIntIntArray=new int[dimCount];
        InterArray<int> *staggerEdgeUWidthIntInt =
          new InterArray<int>(staggerEdgeUWidthIntIntArray,1,extent);
      
        // Map offsets into distgrid space
        for (int i=0; i<dimCount; i++) {
          staggerEdgeLWidthIntIntArray[i]=staggerEdgeLWidthList[staggerloc][distgridToGridMap[i]];
          staggerEdgeUWidthIntIntArray[i]=staggerEdgeUWidthList[staggerloc][distgridToGridMap[i]];
        }


        // Get connection List with pole added back in
        InterArray<int> *connListWPoles=NULL;
        if (staggerloc==0){ // center stagger
//TODO: gjt thinks that poles should only be added back in for center stagger.
//TODO: For all other staggers the DistGrid should not contain pole connections.
//TODO: This way the DistGrid::create() will add the edge padding correctly.
          _add_poles_to_conn(distgrid_wo_poles,
                           staggerEdgeLWidthIntIntArray,
                           staggerEdgeUWidthIntIntArray,
                           connL,connU, &connListWPoles, &localrc);
          if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                          &rc)) return rc;
        }

        // Create stagger distgrid w no poles with this padding
        staggerDistgridList[staggerloc]=DistGrid::create(distgrid_wo_poles,
                                                         ///   DistGrid *staggerdistgrid_wo_poles=DistGrid::create(distgrid_wo_poles,
                                                         staggerEdgeLWidthIntInt, 
                                                         staggerEdgeUWidthIntInt, 
                                                         NULL, connListWPoles, false,
                                                         NULL, NULL, true,
                                                         &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                          &rc)) return rc;
        
        // Get rid of Interface ints
        delete staggerEdgeLWidthIntInt;
        delete [] staggerEdgeLWidthIntIntArray;
        
        delete staggerEdgeUWidthIntInt;
        delete [] staggerEdgeUWidthIntIntArray;
        
        if (connListWPoles!=NULL) {
          if (connListWPoles->array !=NULL) delete[] connListWPoles->array;
          delete connListWPoles;
        }

        // Fill in DEBounds for 1Tile, if new distgrid 
        // (Unlike in the multi-tile case, here we don't change the connections per stagger, so
        //  it's ok to just use the main nopole distgrid, instead of making a stagger specfic one.
        //  The bounds need to be without poles, because we don't want cells across the pole in regridding.) 
        localrc=_createIsDEBnd(&(isStaggerDELBnd[staggerloc]),&(isStaggerDEUBnd[staggerloc]), 
                               distgrid_wo_poles, distgridToGridMap);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc))
          return rc;

       } else { // Multi-tile grids
        int extent[2];

        // Create InterArrays holding stagger padding
        extent[0]=dimCount;
        extent[1]=tileCount;
        int *staggerEdgeLWidthIntIntArray=new int[dimCount*tileCount];
        InterArray<int> *staggerEdgeLWidthIntInt =
          new InterArray<int>(staggerEdgeLWidthIntIntArray,2,extent);
         
        int *staggerEdgeUWidthIntIntArray=new int[dimCount*tileCount];
        InterArray<int> *staggerEdgeUWidthIntInt =
          new InterArray<int>(staggerEdgeUWidthIntIntArray,2,extent);


        // Map offsets into distgrid space by tile and dimension
        int k=0;
        for (int t=0; t<tileCount; t++) {
          for (int d=0; d<dimCount; d++) {
            staggerEdgeLWidthIntIntArray[k]=staggerEdgeLWidthList[staggerloc][distgridToGridMap[d]];
            staggerEdgeUWidthIntIntArray[k]=staggerEdgeUWidthList[staggerloc][distgridToGridMap[d]];

            //printf("%d sELW=%d SEUW=%d\n",k,staggerEdgeLWidthIntIntArray[k],staggerEdgeUWidthIntIntArray[k]);
            k++;
          }
        }
         
        // NOTE: Right now with multi-tile grids we aren't taking out the poles, because we need to rewrite 
        //       the pole taking out methods to work with more tiles (see _create_nopole_distgrid() and _translate_distgrid_conn()). This 
        //       probably isn't an issue because having a pole with multi-tiles seems uncommon. If you have a multi-tile
        //       grid with a pole and you are getting bowtie shaped elements, this is probably why. Ask Bob how to fix. 
         //       (If you take out the poles, then you need to add them back in here. See _add_poles_to_conn(), etc. in the if part above)

        // Remove connections for everything but centers and corners for multi-tile grids, since those are the only ones that seem to work with connections
        // TODO: talk to Gerhard about fixing for EDGE1 and EDGE2
        if ((staggerloc != ESMCI_STAGGERLOC_CENTER) && (staggerloc != ESMCI_STAGGERLOC_CORNER))  {

          // Create 0 sized connection list to indicate that there should be no connections
           // Allocate list without poles
          int dimCount=distgrid->getDimCount();
          int connSize=2+2*dimCount;
          int tmpConnList[5]; // Small tmp array, just to provide usable address, 
                              // not actually used, because 0 connections
          int extent[2];
          extent[0]=connSize;
          extent[1]=0; // No connections, to indicate there should be no connections
          InterArray<int> *emptyConnListII =
            new InterArray<int>(tmpConnList,2,extent);

          // Create stagger distgrid with no connections, so corners work 
          staggerDistgridList[staggerloc]=DistGrid::create(distgrid,
                                                           staggerEdgeLWidthIntInt, 
                                                           staggerEdgeUWidthIntInt, 
                                                           NULL, emptyConnListII, false,
                                                           NULL, NULL, true,
                                                           &localrc);
          if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                            &rc)) return rc;
          // Get rid of connections
          delete emptyConnListII;

        } else {
          // Leave connections, so bilinear, etc. works
          staggerDistgridList[staggerloc]=DistGrid::create(distgrid,
                                                           staggerEdgeLWidthIntInt, 
                                                           staggerEdgeUWidthIntInt, 
                                                           NULL, NULL, false,
                                                           NULL, NULL, true,
                                                           &localrc);
          if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                            &rc)) return rc;
 
        }

        // Get rid of Interface ints
        delete staggerEdgeLWidthIntInt;
        delete [] staggerEdgeLWidthIntIntArray;
        
        delete staggerEdgeUWidthIntInt;
        delete [] staggerEdgeUWidthIntIntArray;

        // Fill in DEBounds for NTile, if new distgrid 
        // (Because right now we strip connections off of non-center staggers for multi-tile grids, for that 
        //  type of grid we want the bounds to be for the specific stagger).  
       localrc=_createIsDEBnd(&(isStaggerDELBnd[staggerloc]),&(isStaggerDEUBnd[staggerloc]), 
                               staggerDistgridList[staggerloc], distgridToGridMap);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc))
          return rc;
      }
    }

    // Return distgrid
    *distgridArg=staggerDistgridList[staggerloc];
    
  } else {
    *distgridArg=distgrid;
  }
  
  //leave successfully
  return ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
//
// serialize() and deserialize()
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::serialize()"
//BOPI
// !IROUTINE:  ESMCI::Grid::serialize - Turn Grid into a byte stream
//
// !INTERFACE:
int Grid::serialize(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
  char *buffer,          // inout - byte stream to fill
  int *length,           // inout - buf length
  int *offset,           // inout - original offset
  const ESMC_AttReconcileFlag &attreconflag, // attreconcile flag
  const ESMC_InquireFlag &inquireflag)       // inquiry flag
{
                         
//
// !DESCRIPTION:
//    Turn info in grid class into a stream of bytes.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  bool cp, done;
  int loffset,r;
  bool **coordExists;
  bool **itemExists;
  bool *staggerDistgridExists;


  // Define serialization macros
#define SERIALIZE_VAR(cp,bufptr,loff,var,t) \
  if (cp) *((t *)(bufptr+loff))=var;    \
  loff += (sizeof(t));  

#define SERIALIZE_VAR1D(cp,bufptr,loff,varptr,s1,t)    \
  if (cp) memcpy(bufptr+loff,varptr,(s1*sizeof(t)));       \
  loff += (s1*sizeof(t));  

#define SERIALIZE_VAR2D(cp,bufptr,loff,varptr,s1,s2,t) \
  if (cp) memcpy(bufptr+loff,((t **)varptr)+s1,(s1*s2*sizeof(t))); \
  loff += (s1*s2*sizeof(t));  

  // Allocate depending on status
  if (status == ESMC_GRIDSTATUS_SHAPE_READY) {
    
    // Create list of which Arrays exist
    coordExists=_allocate2D<bool>(staggerLocCount,dimCount);
    for (int s=0; s<staggerLocCount; s++) {
      for (int c=0; c<dimCount; c++) {
        if (coordArrayList[s][c] == ESMC_NULL_POINTER) {
          coordExists[s][c]=false;
        } else {
            coordExists[s][c]=true;
        }
      }
    }
    

    // Create list of which item Arrays exist
    itemExists=_allocate2D<bool>(staggerLocCount,ESMC_GRIDITEM_COUNT);
    for (int s=0; s<staggerLocCount; s++) {
      for (int i=0; i<ESMC_GRIDITEM_COUNT; i++) {
        if (itemArrayList[s][i] == ESMC_NULL_POINTER) {
          itemExists[s][i]=false;
        } else {
          itemExists[s][i]=true;
        }
      }
    }
    
    // Create list of which staggerdistgrids exist
    staggerDistgridExists= new bool[staggerLocCount];
    for (int s=0; s<staggerLocCount; s++) {
      if (staggerDistgridList[s] == ESMC_NULL_POINTER) {
        staggerDistgridExists[s]=false;
      } else {
        staggerDistgridExists[s]=true;
      }
    }
  }


  // Run twice:
  //    1. check the sizes
  //    2. do the actual copies
  cp=false;
  done=false;
  while (!done) {
    // get localoffset
    loffset=*offset;

    // First, serialize the base class,
    localrc = ESMC_Base::ESMC_Serialize(buffer, length, &loffset, attreconflag, inquireflag);

    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    
    // Serialize status
    SERIALIZE_VAR(cp, buffer,loffset,status,ESMC_GridStatus_Flag);    

    // Serialize depending on status
    if (status == ESMC_GRIDSTATUS_SHAPE_READY) {
      
      SERIALIZE_VAR(cp, buffer,loffset,coordSys,ESMC_CoordSys_Flag);
      
      SERIALIZE_VAR(cp, buffer,loffset,forceConn,bool);
      
      SERIALIZE_VAR(cp, buffer,loffset, decompType, ESMC_GridDecompType);
      
      SERIALIZE_VAR(cp, buffer,loffset,typekind,ESMC_TypeKind_Flag);
      
      SERIALIZE_VAR(cp, buffer,loffset,indexflag,ESMC_IndexFlag);
      
      // Don't serialize these because after deserailizing distgrid and delayout are local
      //  destroyDistgrid 
      //  destroyDELayout
      
      SERIALIZE_VAR(cp, buffer,loffset,distDimCount,int);    
      
      SERIALIZE_VAR1D(cp, buffer,loffset,distgridToGridMap,distDimCount,int);
      
      SERIALIZE_VAR(cp, buffer,loffset,undistDimCount,int);
      
      SERIALIZE_VAR1D(cp, buffer,loffset,undistLBound,undistDimCount,int);
      SERIALIZE_VAR1D(cp, buffer,loffset,undistUBound,undistDimCount,int);
      
      SERIALIZE_VAR(cp, buffer,loffset,dimCount,int);    
      
      SERIALIZE_VAR1D(cp, buffer,loffset,minIndex, dimCount,int);
      SERIALIZE_VAR1D(cp, buffer,loffset,maxIndex, dimCount,int);
      
      SERIALIZE_VAR1D(cp, buffer,loffset,connL, dimCount,ESMC_GridConn);
      SERIALIZE_VAR1D(cp, buffer,loffset,connU, dimCount,ESMC_GridConn);
      
      SERIALIZE_VAR1D(cp, buffer,loffset,gridEdgeLWidth,dimCount,int);
      SERIALIZE_VAR1D(cp, buffer,loffset,gridEdgeUWidth,dimCount,int);
      SERIALIZE_VAR1D(cp, buffer,loffset,gridAlign,dimCount,int);
      
      SERIALIZE_VAR1D(cp, buffer,loffset,coordDimCount,dimCount,int);
      
      SERIALIZE_VAR2D(cp, buffer,loffset,coordDimMap,dimCount,dimCount,int);
      
      SERIALIZE_VAR(cp, buffer,loffset,localArbIndexCount,int);
      SERIALIZE_VAR2D(cp, buffer,loffset,localArbIndex,localArbIndexCount,distDimCount,int);
      
      SERIALIZE_VAR(cp, buffer,loffset,staggerLocCount,int);
      
      SERIALIZE_VAR2D(cp, buffer,loffset,staggerAlignList,staggerLocCount,dimCount,int);
      SERIALIZE_VAR2D(cp, buffer,loffset,staggerEdgeLWidthList,staggerLocCount,dimCount,int);
      SERIALIZE_VAR2D(cp, buffer,loffset,staggerEdgeUWidthList,staggerLocCount,dimCount,int);
      SERIALIZE_VAR2D(cp, buffer,loffset,staggerMemLBoundList,staggerLocCount,dimCount,int);
      
      // Don't serialize didIAllocList since this proxy grid won't
      // have Array's allocated 
      
      SERIALIZE_VAR1D(cp, buffer,loffset,gridIsDist,dimCount,bool);
      SERIALIZE_VAR1D(cp, buffer,loffset,gridMapDim,dimCount,int);
      
      SERIALIZE_VAR2D(cp, buffer,loffset,coordIsDist,dimCount,dimCount,bool);
      SERIALIZE_VAR2D(cp, buffer,loffset,coordMapDim,dimCount,dimCount,int);
      
      // Don't do isDEBnds because a proxy object isn't on a valid DE
      
      // make sure loffset is aligned correctly
      r=loffset%8;
      if (r!=0) loffset += 8-r;
      
      // Serialize the Array exists array
      SERIALIZE_VAR2D(cp, buffer,loffset,coordExists,staggerLocCount,dimCount,bool);
      
      // Serialize the Coord Arrays 
      for (int s=0; s<staggerLocCount; s++) {
        for (int c=0; c<dimCount; c++) {
          if (coordExists[s][c]) {
            //// Serialize the Array
            localrc = coordArrayList[s][c]->serialize(buffer, length, &loffset, attreconflag, inquireflag);
            if (ESMC_LogDefault.MsgFoundError(localrc, 
                                              ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;  
          }
        }
      }
      
      // make sure loffset is aligned correctly
      r=loffset%8;
      if (r!=0) loffset += 8-r;
      
      // Serialize the item Array exists array
      SERIALIZE_VAR2D(cp, buffer,loffset,itemExists,staggerLocCount,ESMC_GRIDITEM_COUNT,bool);
      
      // Serialize the Item Arrays 
      for (int s=0; s<staggerLocCount; s++) {
        for (int i=0; i<ESMC_GRIDITEM_COUNT; i++) {
          if (itemExists[s][i]) {
            //// Serialize the Array
            localrc = itemArrayList[s][i]->serialize(buffer, length, &loffset, attreconflag, inquireflag);
            if (ESMC_LogDefault.MsgFoundError(localrc, 
                                              ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;  
          }
        }
      }
      

      // make sure loffset is aligned correctly
      r=loffset%8;
      if (r!=0) loffset += 8-r;
      
      // Serialize the staggerDistgridExists array
      SERIALIZE_VAR1D(cp, buffer,loffset,staggerDistgridExists,staggerLocCount,bool);
      
      // Serialize the Item Arrays 
      for (int s=0; s<staggerLocCount; s++) {
        if (staggerDistgridExists[s]) {
          //// Serialize the Array
          localrc = staggerDistgridList[s]->serialize(buffer, length, &loffset, inquireflag);
          if (ESMC_LogDefault.MsgFoundError(localrc, 
                                            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;  
        }
      }
      
      
      
      // make sure loffset is aligned correctly
      r=loffset%8;
      if (r!=0) loffset += 8-r;

      // Serialize the DistGrid
      localrc = distgrid->serialize(buffer, length, &loffset, inquireflag);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                        &rc)) return rc;  
      
      // make sure loffset is aligned correctly
      r=loffset%8;
      if (r!=0) loffset += 8-r;
      // Serialize the DistGrid_wo_poles
      localrc = distgrid_wo_poles->serialize(buffer, length, &loffset, inquireflag);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &rc)) return rc;  

    } else if (status == ESMC_GRIDSTATUS_NOT_READY) {

      // If the distgrid is there, then serialize it
      int isDistgridPresent=0;
      if (proto->distgrid != ESMC_NULL_POINTER) {
        isDistgridPresent=1;
      }
      SERIALIZE_VAR(cp, buffer,loffset,isDistgridPresent,int);
    
      if (isDistgridPresent) {
        // make sure loffset is aligned correctly
        r=loffset%8;
        if (r!=0) loffset += 8-r;
        
        // Serialize the DistGrid
        localrc = proto->distgrid->serialize(buffer, length, &loffset, inquireflag);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                          &rc)) return rc;  
      }
    }

    // make sure loffset is aligned correctly
    r=loffset%8;
    if (r!=0) loffset += 8-r;

    // Check if buffer has enough free memory to hold object
    if ((inquireflag != ESMF_INQUIREONLY) && (*length < loffset)){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "Buffer too short to add a Grid object", ESMC_CONTEXT, &rc);
      return rc;
    }

    // If we've done the copy then we're done
    if (cp || (inquireflag == ESMF_INQUIREONLY)) {
      done=true;
    } else { 
      // if we haven't done the the copy,
      // then loop through again and do the copy
      cp=true;
    }
  }

  // Deallocate depending on status
  if (status == ESMC_GRIDSTATUS_SHAPE_READY) {
    
    // free coordExists
    _free2D<bool>(&coordExists);
    
    // free itemExists
    _free2D<bool>(&itemExists);
    
    // free staggerDistgridExists
    delete [] staggerDistgridExists;
  }

  // output localoffset
  *offset=loffset;

  // Undefine serialization macros, so they don't cause troubles elsewhere
#undef SERIALIZE_VAR
#undef SERIALIZE_VAR1D
#undef SERIALIZE_VAR2D
 
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::deserialize()"
//BOPI
// !IROUTINE:  ESMCI::Grid::deserialize - Turn a byte stream into an Grid
//
// !INTERFACE:
int Grid::deserialize(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
  char *buffer,          // in - byte stream to read
  int *offset,          // inout - original offset 
  const ESMC_AttReconcileFlag &attreconflag) // attreconcile flag
{
//
// !DESCRIPTION:
//    Turn a stream of bytes into an object.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  int loffset,r;
  bool **coordExists;
  bool **itemExists;
  bool *staggerDistgridExists;

  // Define serialization macros
#define DESERIALIZE_VAR(bufptr,loff,var,t) \
  var=*((t *)(bufptr+loff));    \
  loff += (sizeof(t));  

#define DESERIALIZE_VAR1D(bufptr,loff,varptr,s1,t)  \
  if (s1) { \
     varptr = new t[s1];           \
     memcpy(varptr,bufptr+loff,(s1*sizeof(t)));      \
    loff += (s1*sizeof(t));  \
  }

#define DESERIALIZE_VAR2D(bufptr,loff,varptr,s1,s2,t) \
  if (s1 && s2) { \
  varptr=_allocate2D<t>(s1,s2);         \
  memcpy(((t **)varptr)+s1,bufptr+loff,(s1*s2*sizeof(t))); \
  loff += (s1*s2*sizeof(t));  \
  }

  // get localoffset
  loffset=*offset;

  // First, deserialize the base class
  localrc = ESMC_Base::ESMC_Deserialize(buffer, &loffset, attreconflag);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
     &rc)) return rc;
  
  // Deserialize status
  DESERIALIZE_VAR( buffer,loffset, status, ESMC_GridStatus_Flag);

  // Deserialize based on status
  if (status == ESMC_GRIDSTATUS_SHAPE_READY) {
    // Make sure proto grid is null, since we're not adding one
    proto=ESMC_NULL_POINTER;

    DESERIALIZE_VAR( buffer,loffset, coordSys, ESMC_CoordSys_Flag);
    
    DESERIALIZE_VAR( buffer,loffset, forceConn, bool);
    
    DESERIALIZE_VAR( buffer,loffset, decompType, ESMC_GridDecompType);
    
    DESERIALIZE_VAR( buffer,loffset,typekind,ESMC_TypeKind_Flag);
    
    DESERIALIZE_VAR( buffer,loffset,indexflag,ESMC_IndexFlag);
    
    // Don't deserialize, but set 
    destroyDistgrid=true;   // proxy DistGrid belongs to this proxy Grid object
    destroyDELayout=false;  // DELayout belongs to DistGrid
    
    DESERIALIZE_VAR( buffer,loffset,distDimCount,int);    
    
    DESERIALIZE_VAR1D( buffer,loffset,distgridToGridMap,distDimCount,int);
    
    DESERIALIZE_VAR( buffer,loffset,undistDimCount,int);
    
    DESERIALIZE_VAR1D( buffer,loffset,undistLBound,undistDimCount,int);
    DESERIALIZE_VAR1D( buffer,loffset,undistUBound,undistDimCount,int);
    
    DESERIALIZE_VAR( buffer,loffset,dimCount,int);    

    DESERIALIZE_VAR1D( buffer,loffset,minIndex,dimCount,int);
    DESERIALIZE_VAR1D( buffer,loffset,maxIndex,dimCount,int);
    
    DESERIALIZE_VAR1D( buffer,loffset,connL,dimCount,ESMC_GridConn);
    DESERIALIZE_VAR1D( buffer,loffset,connU,dimCount,ESMC_GridConn);
    
    DESERIALIZE_VAR1D( buffer,loffset,gridEdgeLWidth,dimCount,int);
    DESERIALIZE_VAR1D( buffer,loffset,gridEdgeUWidth,dimCount,int);
    DESERIALIZE_VAR1D( buffer,loffset,gridAlign,dimCount,int);
    
    DESERIALIZE_VAR1D( buffer,loffset,coordDimCount,dimCount,int);
    DESERIALIZE_VAR2D( buffer,loffset,coordDimMap,dimCount,dimCount,int);
    
    DESERIALIZE_VAR( buffer,loffset,localArbIndexCount,int);
    DESERIALIZE_VAR2D( buffer,loffset,localArbIndex,localArbIndexCount, distDimCount,int);
    
    DESERIALIZE_VAR( buffer,loffset,staggerLocCount,int);
    
    DESERIALIZE_VAR2D( buffer,loffset,staggerAlignList,staggerLocCount,dimCount,int);
    DESERIALIZE_VAR2D( buffer,loffset,staggerEdgeLWidthList,staggerLocCount,dimCount,int);
    DESERIALIZE_VAR2D( buffer,loffset,staggerEdgeUWidthList,staggerLocCount,dimCount,int);
    DESERIALIZE_VAR2D( buffer,loffset,staggerMemLBoundList,staggerLocCount,dimCount,int);
    
    DESERIALIZE_VAR1D( buffer,loffset,gridIsDist,dimCount,bool);
    DESERIALIZE_VAR1D( buffer,loffset,gridMapDim,dimCount,int);

    DESERIALIZE_VAR2D( buffer,loffset,coordIsDist,dimCount,dimCount,bool);
    DESERIALIZE_VAR2D( buffer,loffset,coordMapDim,dimCount,dimCount,int);
    
    // Don't do isDEBnds because a proxy object isn't on a valid DE
    // So make sure that they're NULL
    isDELBnd=ESMC_NULL_POINTER;
    isDEUBnd=ESMC_NULL_POINTER;
    
    // Allocate stagger  bound arrays, 
    // but since there are no localDEs these should always be NULL
    isStaggerDELBnd = ESMC_NULL_POINTER;
    isStaggerDEUBnd = ESMC_NULL_POINTER;
    isStaggerDELBnd = new char *[staggerLocCount];
    isStaggerDEUBnd = new char *[staggerLocCount];
    
    // Set to NULL
    for(int i=0; i<staggerLocCount; i++) {
      isStaggerDELBnd[i] = ESMC_NULL_POINTER;
      isStaggerDEUBnd[i] = ESMC_NULL_POINTER;
    }    
    
    // make sure loffset is aligned correctly
    r=loffset%8;
    if (r!=0) loffset += 8-r;
    
    // Deserialize the Array exists array
    DESERIALIZE_VAR2D( buffer,loffset,coordExists,staggerLocCount,dimCount,bool);
    
    // Deserialize the Coord Arrays 
    coordArrayList=_allocate2D<Array *>(staggerLocCount,dimCount);
    for (int s=0; s<staggerLocCount; s++) {
      for (int c=0; c<dimCount; c++) {
        if (coordExists[s][c]) {
          coordArrayList[s][c]=new Array(-1); // prevent baseID counter increment
          coordArrayList[s][c]->deserialize(buffer, &loffset, attreconflag);
        } else {
          coordArrayList[s][c]=ESMC_NULL_POINTER;
        }
      }
    }
    
    // Setup coordDidIAllocList if the coordExists then deallocate it
    //// allocate storage for array allocation flag
    coordDidIAllocList=_allocate2D<bool>(staggerLocCount,dimCount);
    //// set to all false since we're a proxy Grid
    for(int i=0; i<staggerLocCount; i++) {
      for(int j=0; j<dimCount; j++) {
        coordDidIAllocList[i][j]=coordExists[i][j];
      }
    }
    
    
    // make sure loffset is aligned correctly
    r=loffset%8;
    if (r!=0) loffset += 8-r;

    // Deserialize the item Array exists array
    DESERIALIZE_VAR2D( buffer,loffset,itemExists,staggerLocCount,ESMC_GRIDITEM_COUNT,bool);
    
    // Deserialize the Coord Arrays 
    itemArrayList=_allocate2D<Array *>(staggerLocCount,ESMC_GRIDITEM_COUNT);
    for (int s=0; s<staggerLocCount; s++) {
      for (int i=0; i<ESMC_GRIDITEM_COUNT; i++) {
        if (itemExists[s][i]) {
          itemArrayList[s][i]=new Array(-1);  // prevent baseID counter increment
          itemArrayList[s][i]->deserialize(buffer, &loffset, attreconflag);
        } else {
          itemArrayList[s][i]=ESMC_NULL_POINTER;
        }
      }
    }
    
    // Setup itemDidIAllocList if the itemExists then deallocate it
    //// allocate storage for array allocation flag
    itemDidIAllocList=_allocate2D<bool>(staggerLocCount,ESMC_GRIDITEM_COUNT);
    //// set to all false since we're a proxy Grid
    for(int i=0; i<staggerLocCount; i++) {
      for(int j=0; j<ESMC_GRIDITEM_COUNT; j++) {
        itemDidIAllocList[i][j]=itemExists[i][j];
      }
    }
    
    // make sure loffset is aligned correctly
    r=loffset%8;
    if (r!=0) loffset += 8-r;
    
    // Deserialize the staggerDistgridExists array
    DESERIALIZE_VAR1D( buffer,loffset,staggerDistgridExists,staggerLocCount,bool);
    
    // Deserialize the Coord Arrays 
    staggerDistgridList=new DistGrid *[staggerLocCount];
    for (int s=0; s<staggerLocCount; s++) {
      if (staggerDistgridExists[s]) {
        staggerDistgridList[s]=DistGrid::deserialize(buffer, &loffset);
      } else {
        staggerDistgridList[s]=ESMC_NULL_POINTER;
      }
    }
    
    // Deserialize the DistGrid
    distgrid = DistGrid::deserialize(buffer, &loffset);
    
    // make sure loffset is aligned correctly
    r=loffset%8;
    if (r!=0) loffset += 8-r;
    
    // Deserialize the DistGrid
    distgrid_wo_poles = DistGrid::deserialize(buffer, &loffset);  

    // free coordExists
    _free2D<bool>(&coordExists);
    
    // free itemExists
    _free2D<bool>(&itemExists);
    
    // free staggerDistgridExists
    delete [] staggerDistgridExists;

  } else if (status == ESMC_GRIDSTATUS_NOT_READY) {
    // Add protogrid
    localrc=this->addProtoGrid();
    if (ESMC_LogDefault.MsgFoundError(localrc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;        

    // If the distgrid is there, then deserialize it
    int isDistgridPresent=0;
    DESERIALIZE_VAR( buffer,loffset, isDistgridPresent, int);
    if (isDistgridPresent) {
        // make sure loffset is aligned correctly
        r=loffset%8;
        if (r!=0) loffset += 8-r;
        
        // Deserialize the DistGrid
        proto->distgrid = DistGrid::deserialize(buffer, &loffset);
    }
  }
  
  
  // make sure loffset is aligned correctly
  r=loffset%8;
  if (r!=0) loffset += 8-r;
  
  // output localoffset
  *offset=loffset;

  // Undefine serialization macros, so they don't cause troubles elsewhere
#undef DESERIALIZE_VAR
#undef DESERIALIZE_VAR1D
#undef DESERIALIZE_VAR2D
 
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}


//-----------------------------------------------------------------------------
//
//  File Local Routines
//
//-----------------------------------------------------------------------------
// Compute the total number of stagger locations from a Grid's dimCount
static int _NumStaggerLocsFromDimCount(int dimCount)
{
  return 0x1<<dimCount;
}

// At some point consider replacing the following templated subroutines
// with a whole templated multidimensional class.

// Allocate a 2D array in one chunk of memory
template <class Type>
static  Type **_allocate2D(int sizeDim1, int sizeDim2)
  {
    Type **array,*p;

    // allocate enough space for pointers to rows and rows
    array=(Type **)malloc(sizeDim1*sizeof(Type *)+
                          sizeDim1*sizeDim2*sizeof(Type));

    // fill in row pointers
    p=((Type *)(array+sizeDim1));
    for (int i=0; i<sizeDim1; i++) {
      array[i]=p;
      p=p+sizeDim2;  // advance to beginning of next row
    }

    return array;
  }

// Deallocate a 2D array of ints in one chunk of memory
template <class Type>
static  void _free2D(Type ***array)
  {
    if (*array==ESMC_NULL_POINTER) return;
    free(*array);  
    *array=ESMC_NULL_POINTER;
  }


// Allocate a 3D array in one chunk of memory
template <class Type>
static  Type ***_allocate3D(int sizeDim1, int sizeDim2, int sizeDim3)
  {
    Type ***array,**p1, *p2;

    // allocate enough space for pointers to pointers and pointers 
    // to data and data
    array=(Type ***)malloc(sizeDim1*sizeof(Type **)+
                           sizeDim1*sizeDim2*sizeof(Type *)+
                           sizeDim1*sizeDim2*sizeDim3*sizeof(Type));

    // fill in pointers to arrays of pointers to actual data
    p1=((Type **)(array+sizeDim1));
    for (int i=0; i<sizeDim1; i++) {
      array[i]=p1;
      p1=p1+sizeDim2;  // advance to next row in pointers to pointers
    }

    // starting with pointer to beginning of data storage fill in row pointers
    p2=(Type *)p1;
    for (int i=0; i<sizeDim1; i++) {
      for (int j=0; j<sizeDim2; j++) {
        array[i][j]=p2;
        p2=p2+sizeDim3;  // advance to beginning of next row
      }
    }

    return array;
  }


// Deallocate a 3D array of ints in one chunk of memory
template <class Type>
static  void _free3D(Type ****array)
  {
    if (*array==ESMC_NULL_POINTER) return;
    free(*array);  
    *array=ESMC_NULL_POINTER;
  }


  // Make a copy of an interface int, allocating a new chunk of memory for its
  // internal array
  static InterArray<int> *_copyInterArray(InterArray<int> *in) {

    // calc size of array
    int size=1;
    for (int i=0; i<in->dimCount; i++) {
      size=size*(in->extent[i]);
    }

    // allocate new storage
    int *array;
    if (size>0) {
      array=new int[size];
      memcpy(array,in->array,size*sizeof(int));
    } else {
      array=ESMC_NULL_POINTER;
    }

    return new InterArray<int>(array,in->dimCount,in->extent);
  }

  // Deallocate an InterArray which was created with _copyInterArray 
  static void _freeInterArray(InterArray<int> **in) {

    // make sure its not a null pointer
    if (in==ESMC_NULL_POINTER) return;
    if (*in==ESMC_NULL_POINTER) return;

    // delete internal memory
    if ((*in)->array !=ESMC_NULL_POINTER) {
      delete [] ((*in)->array);  
    }

    // delete object
    delete *in;

    // set pointer
    *in=ESMC_NULL_POINTER;
  }


#if 0
// OLD WAY OF CALCULATING EDGE LOCAL DEs

  // Create arrays (isDEUBnd and isDELBnd) which tell if a particular DE is on
  // the edge of a tile.
  // If bit r of isDEUBnd is 1 then the DE is on the upper boundary in dim. r
  // If bit r of isDELBnd is 1 then the DE is on the lower boundary in dim. r
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::_createIsBnd()"
  static int _createIsDEBnd(char **_isDELBnd, char **_isDEUBnd, 
                            DistGrid *distgrid, int *distgridToGridMap) {
    char *isDELBnd,*isDEUBnd;
    int rc,localrc;

    // get dimCount;
    int dimCount=distgrid->getDimCount();

    // Get the DELayout
    DELayout *delayout=distgrid->getDELayout();

    // Get the number of local DEs
    const int localDECount=delayout->getLocalDeCount();

    // Get map between local and global DEs
    const int *localDEList=delayout->getLocalDeToDeMap();
   
    // Get map between DEs and tiles
    const int *DETileList = distgrid->getTileListPDe();

    // Get list of tile min and maxs
    const int *tileMinIndexList = distgrid->getMinIndexPDimPTile();
    const int *tileMaxIndexList = distgrid->getMaxIndexPDimPTile();

    // Get Extents of index lists
    const int *deIndexListExtentList=distgrid->getIndexCountPDimPDe();

    // allocate Bnds
    if (localDECount > 0) {
      isDELBnd=new char[localDECount];
      isDEUBnd=new char[localDECount];
    } else {
      isDELBnd=ESMC_NULL_POINTER;
      isDEUBnd=ESMC_NULL_POINTER;
    }

    // loop through local DE's setting flags
    for (int lDE=0; lDE<localDECount; lDE++) {

      //// get global de
      int gDE=localDEList[lDE];

      //// get tile
      int tile=DETileList[gDE];

      //// Avoid tile 0 because they're 0 sized
      if (tile != 0) {
        //// get the extents for this de
        const int *deExtent=deIndexListExtentList+gDE*dimCount;
        
        //// get tile min/max
        const int *tileMin=distgrid->getMinIndexPDimPTile(tile, &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc,
                           ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
        const int *tileMax=distgrid->getMaxIndexPDimPTile(tile, &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc,
                           ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
        
        //// Init flags
        isDELBnd[lDE]=0xff;
        isDEUBnd[lDE]=0xff;
        
        //// loop setting flags
        for (int d=0; d<dimCount; d++) {
          
          ////// make sure is contiguous         
          const int contig=distgrid->getContigFlagPDimPDe(gDE, d+1, &localrc);
          if (ESMC_LogDefault.MsgFoundError(localrc,
                        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
          if (!contig) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
              "- doesn't handle non-contiguous DEs yet ", ESMC_CONTEXT, &rc);
            return rc;
          }
          
          // get indices of DE
          const int *indexList=distgrid->getIndexListPDimPLocalDe(lDE, d+1,
                                                                  &localrc);
          if (ESMC_LogDefault.MsgFoundError(localrc,
              ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
          
          // if we're not at the min then we're not a lower bound 
          // so turn off the bit
          if (indexList[0] != tileMin[d]) {
            isDELBnd[lDE] &= ~(0x1<<distgridToGridMap[d]);
          } 
          
          // if we're at the min then we're a lower bound
          if (indexList[deExtent[d]-1]!=tileMax[d]) {
            isDEUBnd[lDE] &= ~(0x1<<distgridToGridMap[d]);
          }
        }
      } else { // If we're empty then we're not on a boundary
        isDELBnd[lDE]=0x0;
        isDEUBnd[lDE]=0x0;
      }
    }
    
    // set output variables
    *_isDELBnd=isDELBnd;
    *_isDEUBnd=isDEUBnd;

    // return success
    return ESMF_SUCCESS;
  }

#endif



  // Create arrays (isDEUBnd and isDELBnd) which tell if a particular DE is on
  // the edge of a tile.
  // If bit r of isDEUBnd is 1 then the DE is on the upper boundary in dim. r
  // If bit r of isDELBnd is 1 then the DE is on the lower boundary in dim. r
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::_createIsBnd()"
  static int _createIsDEBnd(char **_isDELBnd, char **_isDEUBnd, 
                            DistGrid *distgrid, int *distgridToGridMap) {
    char *isDELBnd,*isDEUBnd;
    int rc,localrc;

    // get dimCount;
    int dimCount=distgrid->getDimCount();

    // Get the DELayout
    DELayout *delayout=distgrid->getDELayout();

    // Get the number of local DEs
    const int localDECount=delayout->getLocalDeCount();

    // allocate Bnds
    if (localDECount > 0) {
      isDELBnd=new char[localDECount];
      isDEUBnd=new char[localDECount];
    } else {
      isDELBnd=ESMC_NULL_POINTER;
      isDEUBnd=ESMC_NULL_POINTER;
    }

    // loop through local DE's setting flags
    for (int lDE=0; lDE<localDECount; lDE++) {
        
        //// Init flags
        isDELBnd[lDE]=0xff;
        isDEUBnd[lDE]=0xff;
        
        //// loop setting flags
        for (int d=0; d<dimCount; d++) {
          
          // if we're not a lower bound turn off the bit
          bool isLBnd=distgrid->isLocalDeOnEdgeL(lDE,d+1,&localrc);
          if (ESMC_LogDefault.MsgFoundError(localrc,
                              ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
          if (!isLBnd) {
            isDELBnd[lDE] &= ~(0x1<<distgridToGridMap[d]);
          } 
          

          // if we're not an upper bound turn off the bit
          bool isUBnd=distgrid->isLocalDeOnEdgeU(lDE,d+1,&localrc);
          if (ESMC_LogDefault.MsgFoundError(localrc,
                              ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
          if (!isUBnd) {
            isDEUBnd[lDE] &= ~(0x1<<distgridToGridMap[d]);
          }
        }
    }
    
    // set output variables
    *_isDELBnd=isDELBnd;
    *_isDEUBnd=isDEUBnd;

    // return success
    return ESMF_SUCCESS;
  }

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::construct()"
//BOP
// !IROUTINE:  construct
//
// !INTERFACE:
int construct(
//
// !RETURN VALUE:
//   error code
//
// !ARGUMENTS:
//
  Grid *gridArg, 
  int nameLenArg,                           // (in)
  char *nameArg,                            // (in) optional
  ESMC_TypeKind_Flag *typekindArg,               // (in) optional
  DistGrid *distgridArg,                    // (in) 
  InterArray<int> *gridEdgeLWidthArg,             // (in) optional
  InterArray<int> *gridEdgeUWidthArg,             // (in) optional
  InterArray<int> *gridAlignArg,             // (in) optional
  InterArray<int> *distgridToGridMapArg,                  // (in) optional
  InterArray<int> *undistLBoundArg,                 // (in) optional
  InterArray<int> *undistUBoundArg,                 // (in) optional
   ESMC_CoordSys_Flag *coordSysArg, 
  InterArray<int> *coordDimCountArg,               // (in) optional
  InterArray<int> *coordDimMapArg,             // (in) optional
  InterArray<int> *gridMemLBoundArg,             // (in) optional
  ESMC_IndexFlag *indexflagArg,              // (in) optional
  bool *destroyDistgridArg,
  bool *destroyDELayoutArg
  ){
//
// !DESCRIPTION:
//   Take an existing {\tt ESMCI_Grid} object and setup its internal structure
//   so that its usable in other Grid methods. Note that this routine
//   does error checking of input parameters and sets a default if an optional
//   parameter isn't passed in. (A non-present optional parameters is passed with 
//   the value NULL).The consturction routine is for a non-arbitrarily distributed
//   grid.
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                 // local error status
  int rc;                 // error status
  int dimCount;
  int distDimCount;
  int undistDimCount;
  int *distgridToGridMap;
  ESMC_TypeKind_Flag typekind;
  int *undistUBound;
  int *undistLBound;
  int *coordDimCount;
  int **coordDimMap;
  int *gridEdgeLWidth;
  int *gridEdgeUWidth;
  int *gridAlign;
  int *gridMemLBound;
  ESMC_IndexFlag indexflag;
  int ind;
  char *name;  
  int *minIndex;
  int *maxIndex;
  const int *distGridMinIndex;
  const int *distGridMaxIndex; 
  bool destroyDistgrid;
  bool destroyDELayout;
  ESMC_CoordSys_Flag coordSys;

  // initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

   // To prevent erasing an existing grid, make sure grid is inactive
  if (gridArg->getStatus() != ESMC_GRIDSTATUS_NOT_READY) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
      "- grid must be status 'not ready' to be activated ", ESMC_CONTEXT, &rc);
    return rc;
  }  

  // Set GridDecompType to be non-arbitrary
  gridArg->decompType = ESMC_GRID_NONARBITRARY;
  
  // Need a DistGrid to create a Grid, so error if not passed in
  if (distgridArg == NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to distgrid argument", ESMC_CONTEXT, &rc);
    return rc;
  }

  // Convert F90 name string to C++ string 
  name = ESMC_F90toCstring(nameArg, nameLenArg);
  if (!name && nameLenArg){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid string", ESMC_CONTEXT, &rc);
    return rc;
  }

  // If typekind wasn't passed in then use default, otherwise 
  // copy passed in value
  if (typekindArg==NULL) {
    typekind=ESMC_TYPEKIND_R8;  // Default
  } else {
    typekind=*typekindArg;
  }

  // If coordSys wasn't passed in then use default, otherwise 
  // copy passed in value
  if (coordSysArg==NULL) {
    coordSys=ESMC_COORDSYS_SPH_DEG;
  } else {
    coordSys=*coordSysArg;
  }


  // If indexflag wasn't passed in then use default, otherwise 
  // copy passed in value
  if (indexflagArg==NULL) {
    indexflag=ESMC_INDEX_DELOCAL;  // default
  } else {
    indexflag=*indexflagArg;
   }


  // Get DimCount of Distributed Dimensions
  distDimCount = distgridArg->getDimCount();

  // Process undistLBoundArg and undistUBoundArg
  // process these first to be able to calculate dimCount before distgridToGridMap processing

  // If undistUBoundArg paramter hasn't been passed in then the grid doesn't have
  // undistributed dimensions 
  // (undistDimCount=0), if it has been then error check and copy it
  undistDimCount=0; // default to 0
  undistUBound = NULL; // default to NULL
  if (present(undistUBoundArg)){
    if (undistUBoundArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- undistUBound array must be of dimCount 1", ESMC_CONTEXT, &rc);
      return rc;
    }
    if (undistUBoundArg->extent[0] < 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- undistUBound array must at least be of size 1", ESMC_CONTEXT, &rc);
      return rc;
    }
    undistDimCount=undistUBoundArg->extent[0]; 
    undistUBound = undistUBoundArg->array;
  }

  // If undistLBoundArg have been passed in, then copy it, unless undistUBound isn't
  // present in which
  // case there's an error (no undistUBound -> no undist. dims in grid). 
  // If undistLBound isn't present and undistUBound is then set a default, 
  // otherwise error check and copy undistLBoundArg. 
  undistLBound = NULL; // reset
  if (present(undistLBoundArg)){
    if (!present(undistUBoundArg)){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- can't have undistUBound without undistLBound", ESMC_CONTEXT, &rc);
      return rc;
    }
    if (undistLBoundArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- undistLBound array must be of dimCount 1", ESMC_CONTEXT, &rc);
      return rc;
    }
    if (undistLBoundArg->extent[0] != undistDimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
         "- undistLBound, undistUBound size mismatch", ESMC_CONTEXT, &rc);
      return rc;
    }
    // set undistLBound from argument
    undistLBound=new int[undistDimCount];
    for (int i=0; i<undistDimCount; i++)
         undistLBound[i]=undistLBoundArg->array[i];
  } else if (present(undistUBoundArg)) {
    // default undistLBound to (1,1,1,...)
    undistLBound=new int[undistDimCount];
    for (int i=0; i<undistDimCount; i++)
         undistLBound[i]=1;  // default to a bottom of 1
  }


  // Compute grid dimCount (the sum of the distributed and undistributed dimCounts)
  dimCount=distDimCount+undistDimCount;

  // Grid must have positve dimCount
  if (dimCount<1) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- Grid must have dimCount >=1 ", ESMC_CONTEXT, &rc);
      return rc;
  }

  // Error check gridEdgeLWidthArg
  if (present(gridEdgeLWidthArg)) {
    if (gridEdgeLWidthArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- gridEdgeLWidth array must be of dimCount 1", ESMC_CONTEXT, &rc);
      return rc;
    }
    if (gridEdgeLWidthArg->extent[0] != dimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "- gridEdgeLWidth must be the same size as the dimCount of the Grid",
        ESMC_CONTEXT, &rc);
      return rc;
    }
    for (int i=0; i<dimCount; i++){
      if (gridEdgeLWidthArg->array[i] < 0){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          "- gridEdgeLWidth entries must be 0 or greater", ESMC_CONTEXT, &rc);
        return rc;
      }
    }
  } 

  // Error check gridEdgeUWidthArg
   if (present(gridEdgeUWidthArg)) {
    if (gridEdgeUWidthArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- gridEdgeUWidth array must be of dimCount 1", ESMC_CONTEXT, &rc);
      return rc;
    }
    if (gridEdgeUWidthArg->extent[0] != dimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "- gridEdgeUWidth must be the same size as the dimCount of the Grid",
        ESMC_CONTEXT, &rc);
      return rc;
    }
    for (int i=0; i<dimCount; i++) {
      if (gridEdgeUWidthArg->array[i] < 0){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          "- gridEdgeUWidth entries must be 0 or greater", ESMC_CONTEXT, &rc);
        return rc;
      }
    }
  }

  // Error check gridAlignArg
  if (present(gridAlignArg)) {
    if (gridAlignArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- gridAlign array must be of dimCount 1", ESMC_CONTEXT, &rc);
      return rc;
    }
    if (gridAlignArg->extent[0] != dimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "- gridAlign must be the same size as the dimCount of the Grid",
        ESMC_CONTEXT, &rc);
      return rc;
    }
    for (int i=0; i<dimCount; i++) {
      if ((gridAlignArg->array[i] != 1) && (gridAlignArg->array[i] != -1)){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          "- gridAlign entries must be +1, or -1", ESMC_CONTEXT, &rc);
        return rc;
      }
    }
  } 

  // Allocate lWidth, uWidth, and Align based on inputs and defaults
  gridEdgeLWidth = new int[dimCount];
  gridEdgeUWidth = new int[dimCount];
  gridAlign = new int[dimCount];

   localrc=setGridDefaultsLUA(dimCount,
          gridEdgeLWidthArg, gridEdgeUWidthArg, gridAlignArg,
          gridEdgeLWidth, gridEdgeUWidth, gridAlign);
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;


  // Error check gridMemLBound and fill in value
  gridMemLBound=new int[dimCount];
  if (present(gridMemLBoundArg)) {
    if (indexflag != ESMC_INDEX_USER){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
        "- if gridMemLBound is set then indexflag must be ESMC_INDEX_USER",
        ESMC_CONTEXT, &rc);
      return rc;
    }
    if (gridMemLBoundArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- gridMemLBound array must be of dimCount 1", ESMC_CONTEXT, &rc);
      return rc;
    }
    if (gridMemLBoundArg->extent[0] != dimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "- gridMemLBound must be the same size as the dimCount of the Grid",
        ESMC_CONTEXT, &rc);
      return rc;
    }
    for (int i=0; i<dimCount; i++) {
      gridMemLBound[i]=gridMemLBoundArg->array[i];
    }
  } else {
    if (indexflag == ESMC_INDEX_USER){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
        "- if indexflag=ESMC_INDEX_USER then gridMemLBound must be set",
        ESMC_CONTEXT, &rc);
      return rc;
    }
    for (int i=0; i<dimCount; i++) {
      gridMemLBound[i]=1;
    }
  }


  // If the distgridToGridMapArg parameter has been passed in then error check 
  // and copy it, otherwise set a default.
  distgridToGridMap = new int[distDimCount];
  if (!present(distgridToGridMapArg)) {
    for (int i=0; i<distDimCount; i++)
       distgridToGridMap[i] = i; // set distgridToGridMap to default (0,1,2..)
  } else {
    if (distgridToGridMapArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- distgridToGridMap array must be of dimCount 1", ESMC_CONTEXT, &rc);
      return rc;
    }
    if (distgridToGridMapArg->extent[0] != distDimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "- distgridToGridMap and distgrid mismatch", ESMC_CONTEXT, &rc);
      return rc;
    }
    for (int i=0; i<distDimCount; i++){
      if (distgridToGridMapArg->array[i] < 1 || distgridToGridMapArg->array[i] > dimCount){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          "- distgridToGridMap / dimCount mismatch", ESMC_CONTEXT, &rc);
        return rc;
      }
      distgridToGridMap[i] = distgridToGridMapArg->array[i]-1;  // copy distgridToGridMap and make zero based
    }
  } 

  // If the coordDimCountArg parameter has been passed in then error check and 
  // copy it, otherwise set a default.
  coordDimCount=new int[dimCount];
  if (!present(coordDimCountArg)) {
    for (int i=0; i<dimCount; i++)
      coordDimCount[i] = dimCount; // set coordDimCount to default all curvilinear
  } else {
    if (coordDimCountArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- coordDimCount array must be of dimCount 1", ESMC_CONTEXT, &rc);
      return rc;
    }
    if (coordDimCountArg->extent[0] != dimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "- coordDimCount and distgrid (and perhaps undistUBound) mismatch",
        ESMC_CONTEXT, &rc);
      return rc;
    }
    for (int i=0; i<dimCount; i++){
      if (coordDimCountArg->array[i] < 1 || coordDimCountArg->array[i] > dimCount){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          "- coordDimCount / dimCount mismatch", ESMC_CONTEXT, &rc);
        return rc;
      }
      // // TODO: take this out when Array Factorization works
      // if (coordDimCountArg->array[i] != dimCount){
       //  ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
      //    "- Array and thus Grid don't currently support factorization",
      //    ESMC_CONTEXT, &rc);
      //  return rc;
      // }
      coordDimCount[i] = coordDimCountArg->array[i];  // copy coordDimCount array element
    }
  } 
  
  // If the coordDimMapArg parameter has been passed in then error check and
  // copy it, otherwise set a default.
  coordDimMap=_allocate2D<int>(dimCount,dimCount);
  // initialize array to 0
  for(int i=0; i<dimCount; i++) {
    for (int j=0; j<dimCount; j++) {
      coordDimMap[i][j]=0;  
    }
  }

  if (!present(coordDimMapArg)) {
    for(int i=0; i<dimCount; i++) {
      for (int j=0; j<coordDimCount[i]; j++) {
        coordDimMap[i][j]=j;  // initialize to a default
      }
    }
  } else {
    if (!present(coordDimCountArg)){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "- if coordDimMap is specified then a corresponding coordDimCount must also be specified",
        ESMC_CONTEXT, &rc);
      return rc;
    }
    if (coordDimMapArg->dimCount != 2){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- coordDimMap array must be of dimCount 2", ESMC_CONTEXT, &rc);
      return rc;
    }
    if ((coordDimMapArg->extent[0] != dimCount) || 
        (coordDimMapArg->extent[1] != dimCount)) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "- coordDimMap and distgrid (and perhaps undistUBound) mismatch",
        ESMC_CONTEXT, &rc);
      return rc;
    }
    for (int i=0; i<dimCount; i++){
      for (int j=0; j<coordDimCount[i]; j++) {
        // Note: order of i,j is because of F vs. C array ordering
        ind=j*dimCount+i;
 
        // Check to make sure data is correct
       if (coordDimMapArg->array[ind] < 1 || coordDimMapArg->array[ind] > dimCount){
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
            " - invalid coordDimMap value", ESMC_CONTEXT, &rc);
          return rc;
        }

        // copy coordDimMap array element
        coordDimMap[i][j] = coordDimMapArg->array[ind]-1; // switch to 0-based  
      }
    }

    /* DEBUG
    // test coordDimMap Order
    for (int i=0; i<dimCount; i++){
      for (int j=0; j<dimCount; j++) {
        printf("[%d][%d]=%d\n",i,j,coordDimMap[i][j]);
      }
    }
    */

  }  

  // reconstruct minIndex and maxIndex from distGrid's min and maxIndexPDimPTile and 
  // undistGridLand UBounds and distgridToGridMap
  distGridMinIndex = distgridArg->getMinIndexPDimPTile();
  distGridMaxIndex = distgridArg->getMaxIndexPDimPTile();

  // allocate minIndex and maxIndex and fill them
  minIndex = new int[dimCount];
  maxIndex = new int[dimCount];
  // initialize them to 0
  for (int i = 0; i < dimCount; i++) {
    minIndex[i]=maxIndex[i]=0;
  }
  
  // assign the min/max to the distributed dimensions
  for (int i = 0; i < distDimCount; i++) {
    minIndex[distgridToGridMap[i]] = distGridMinIndex[i];
    maxIndex[distgridToGridMap[i]] = distGridMaxIndex[i];
  }

  // assign the min/max to the undistributed dimensions
  if (undistDimCount > 0) {
    for (int i=0, j=0; i < dimCount; i++) {
      if (maxIndex[i] == 0) {
    minIndex[i] = undistLBound[j];
         maxIndex[i] = undistUBound[j];
    j++;
      }
    }
  }

  // If destroyDistgrid wasn't passed in then use default, otherwise 
  // copy passed in value
  if (destroyDistgridArg==NULL) {
    destroyDistgrid=false;  // default
  } else {
    destroyDistgrid=*destroyDistgridArg;
  }

  // If destroyDELayout wasn't passed in then use default, otherwise 
  // copy passed in value
  if (destroyDELayoutArg==NULL) {
    destroyDELayout=false;  // default
  } else {
    destroyDELayout=*destroyDELayoutArg;
  }

  // construct the Grid object using the massaged parameter values
  localrc=gridArg->constructInternal(name, typekind, distgridArg, 
                     distDimCount, distgridToGridMap, 
                     undistDimCount, undistLBound, undistUBound,
                     dimCount, gridEdgeLWidth, gridEdgeUWidth,
                     gridAlign, coordSys, coordDimCount, coordDimMap, 
                     gridMemLBound, indexflag,
                     minIndex, maxIndex, NULL, 0, 0, 
                     destroyDistgrid, destroyDELayout);
   if (ESMC_LogDefault.MsgFoundError(localrc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
        
  // Dellocate temporay arrays
  if (present(undistUBoundArg))  delete [] undistLBound;

  if (name) delete [] name;

  delete [] gridEdgeLWidth;

  delete [] gridEdgeUWidth;

  delete [] gridAlign;

  delete [] gridMemLBound;

  delete [] distgridToGridMap;

  delete [] coordDimCount;

  delete [] minIndex;

  delete [] maxIndex;
  
  _free2D<int>(&coordDimMap);

  return ESMF_SUCCESS;
  }

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::construct()"
//BOP
// !IROUTINE:  construct
//
// !INTERFACE:
int construct(
//
// !RETURN VALUE:
//   error code
//
// !ARGUMENTS:
//
  Grid *gridArg, 
  int nameLenArg,                           // (in)
  char *nameArg,                            // (in) optional
  ESMC_TypeKind_Flag *typekindArg,               // (in) optional
  DistGrid *distgridArg,                    // (in) 
  InterArray<int> *minIndexArg,               // (in) optional
  InterArray<int> *maxIndexArg,                // (in)  
  InterArray<int> *localArbIndexArg,            // (in)  
  int localArbIndexCountArg,                           // (in)  
  InterArray<int> *distDimArg,                // (in) 
  int arbDimArg,                           // (in)
  InterArray<int> *undistLBoundArg,            // (in) optional
  InterArray<int> *undistUBoundArg,            // (in) optional
  ESMC_CoordSys_Flag *coordSysArg, 
  InterArray<int> *coordDimCountArg,               // (in) optional
  InterArray<int> *coordDimMapArg,             // (in) optional
  InterArray<int> *gridMemLBoundArg,             // (in) optional
  ESMC_IndexFlag *indexflagArg,             // (in) optional
  bool *destroyDistgridArg,
  bool *destroyDELayoutArg
  ){
//
// !DESCRIPTION:
//   Take an existing {\tt ESMCI_Grid} object and setup its internal structure
//   so that its usable in other Grid methods. Note that this routine
//   is the construction routine for a arbitrarily distributed grid
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                 // local error status
  int rc;                 // error status
  int dimCount;
  int distDimCount;
  int undistDimCount;
  int *distgridToGridMap;
  ESMC_TypeKind_Flag typekind;
  int *gridEdgeLWidth;
  int *gridEdgeUWidth;
  int *gridAlign;
  int *gridMemLBound;
  int *coordDimCount;
  int **coordDimMap;
  int *undistUBound;
  int *undistLBound;
  int *minIndex;
  int *maxIndex;
  int **localArbIndex;
  int *distDim;
  int localArbIndexCount;
  ESMC_IndexFlag indexflag;
  int ind;
  char *name;  
  bool destroyDistgrid;
  bool destroyDELayout;
  ESMC_CoordSys_Flag coordSys;

  // initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  // To prevent erasing an existing grid, make sure grid is inactive
  if (gridArg->getStatus() != ESMC_GRIDSTATUS_NOT_READY) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
      "- grid must be status 'not ready' to be activated ", ESMC_CONTEXT, &rc);
    return rc;
  }  
  
  // Set GridDecompType to be non-arbitrary
  gridArg->setDecompType(ESMC_GRID_ARBITRARY);
  
  // Need a DistGrid to create a Grid, so error if not passed in
  if (distgridArg == NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to distgrid argument", ESMC_CONTEXT, &rc);
    return rc;
  }

  // Convert F90 name string to C++ string 
  name = ESMC_F90toCstring(nameArg, nameLenArg);
  if (!name && nameLenArg){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid string", ESMC_CONTEXT, &rc);
    return rc;
  }

  // If typekind wasn't passed in then use default, otherwise 
  // copy passed in value
  if (typekindArg==NULL) {
    typekind=ESMC_TYPEKIND_R8;  // Default
  } else {
    typekind=*typekindArg;
  }

  // If coordSys wasn't passed in then use default, otherwise 
  // copy passed in value
  if (coordSysArg==NULL) {
    coordSys=ESMC_COORDSYS_SPH_DEG;
  } else {
    coordSys=*coordSysArg;
  }


  // If indexflag wasn't passed in then use default, otherwise 
  // copy passed in value
  if (indexflagArg==NULL) {
    indexflag=ESMC_INDEX_DELOCAL;  // default
  } else {
    indexflag=*indexflagArg;
  }


  // find out the dimCount of the grid from maxindex
  dimCount = maxIndexArg->extent[0];
  if (dimCount < 1) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- The dimCount of the grid must be 2 or above", ESMC_CONTEXT, &rc);
      return rc;
  }
  
  // Process undistLBoundArg and undistUBoundArg
  // process these first to be able to calculate dimCount before distgridToGridMap processing

  // If undistUBoundArg paramter hasn't been passed in then the grid doesn't have
  // undistributed dimensions 
  // (undistDimCount=0), if it has been then error check and copy it
    undistUBound = NULL; // default to NULL
  if (present(undistUBoundArg)){
    if (undistUBoundArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- undistUBound array must be of dimCount 1", ESMC_CONTEXT, &rc);
      return rc;
    }
    if (undistUBoundArg->extent[0] < 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- undistUBound array must at least be of size 1", ESMC_CONTEXT, &rc);
      return rc;
    }
    undistDimCount=undistUBoundArg->extent[0]; 
    undistUBound = undistUBoundArg->array;
  }

  // If undistLBoundArg have been passed in, then copy it, unless undistUBound isn't
  // present in which
  // case there's an error (no undistUBound -> no undist. dims in grid). 
  // If undistLBound isn't present and undistUBound is then set a default, 
  // otherwise error check and copy undistLBoundArg. 
  undistLBound = NULL; // reset
  if (present(undistLBoundArg)){
    if (!present(undistUBoundArg)){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- can't have undistUBound without undistLBound", ESMC_CONTEXT, &rc);
      return rc;
    }
    if (undistLBoundArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- undistLBound array must be of dimCount 1", ESMC_CONTEXT, &rc);
      return rc;
    }
    if (undistLBoundArg->extent[0] != undistDimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "- undistLBound, undistUBound size mismatch", ESMC_CONTEXT, &rc);
      return rc;
    }
    // set undistLBound from argument
    undistLBound=new int[undistDimCount];
    for (int i=0; i<undistDimCount; i++)
      undistLBound[i]=undistLBoundArg->array[i];
  } 

  // All the default values should be zero
  // Allocate lWidth, uWidth, and Align based on inputs and defaults
  gridEdgeLWidth = new int[dimCount];
  gridEdgeUWidth = new int[dimCount];
  gridAlign = new int[dimCount];

  for (int i = 0; i < dimCount; i++) {
    gridEdgeLWidth[i] = 0;
    gridEdgeUWidth[i] = 0;
    gridAlign[i] = 0;
  }


  // Error check gridMemLBound and fill in value
  gridMemLBound=new int[dimCount];
  if (present(gridMemLBoundArg)) {
    if (indexflag != ESMC_INDEX_USER){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
        "- if gridMemLBound is set then indexflag must be ESMC_INDEX_USER",
        ESMC_CONTEXT, &rc);
      return rc;
    }
    if (gridMemLBoundArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- gridMemLBound array must be of dimCount 1", ESMC_CONTEXT, &rc);
      return rc;
    }
    if (gridMemLBoundArg->extent[0] != dimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "- gridMemLBound must be the same size as the dimCount of the Grid",
        ESMC_CONTEXT, &rc);
      return rc;
    }
    for (int i=0; i<dimCount; i++) {
      gridMemLBound[i]=gridMemLBoundArg->array[i];
    }
  } else {
    if (indexflag == ESMC_INDEX_USER){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
        "- if indexflag=ESMC_INDEX_USER then gridMemLBound must be set",
        ESMC_CONTEXT, &rc);
      return rc;
    }
    for (int i=0; i<dimCount; i++) {
      gridMemLBound[i]=1;
    }
  }


  // If the distDimArg parameter has been passed in then error check 
  // and copy it, otherwise set a default.
  if (distDimArg->dimCount != 1){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
      "- distgridToGridMap array must be of dimCount 1", ESMC_CONTEXT, &rc);
    return rc;
  }
  if (distDimArg->extent[0] > dimCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
      "- distDim array size must not be greater than dimCount", ESMC_CONTEXT,
      &rc);
    return rc;
  }
  distDimCount = distDimArg->extent[0];
  undistDimCount = dimCount - distDimCount;

  distgridToGridMap = new int[distDimCount];
  for (int i=0; i<distDimCount; i++){
    if (distDimArg->array[i] < 1 || distDimArg->array[i] > dimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
        "- distgridToGridMap / dimCount mismatch", ESMC_CONTEXT, &rc);
      return rc;
    }
    distgridToGridMap[i] = distDimArg->array[i]-1;  // copy distgridToGridMap and make zero based
   } 

  coordDimCount=new int[dimCount];
  if (!present(coordDimCountArg)) {
    // default should be 1 for both arbitrary dimension or undistributed dimension
    for (int i=0; i<dimCount; i++) coordDimCount[i]=1;
  } else { 
    if (coordDimCountArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- coordDimCount must be of rank 1", ESMC_CONTEXT, &rc);
      return rc;
    }
    if (coordDimCountArg->extent[0] != dimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- coordDimCount array size must be of dimCount", ESMC_CONTEXT, &rc);
      return rc;
    }
    // copy coordDimCount array element
    for (int i=0; i<dimCount; i++) 
     coordDimCount[i] = coordDimCountArg->array[i];  
  }

  // Not sure if we need coordDimMap for arbitrary grid, probably not
  // Create coordDimMap
  coordDimMap=_allocate2D<int>(dimCount,dimCount);
  // initialize array to 0
  if (!present(coordDimMapArg)) {
    // ESMC_GRID_ARBDIM (-2) if arbitrary dim, otherwise, i
    for (int i=0; i<dimCount; i++) coordDimMap[i][0]=i;
    for (int i=0; i<distDimCount; i++) {
      coordDimMap[distgridToGridMap[i]][0]=ESMC_GRID_ARBDIM;
    }
  } else { 
    if (coordDimMapArg->dimCount != 2){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- coordDimMap array must be of dimCount 2", ESMC_CONTEXT, &rc);
      return rc;
    }
    if ((coordDimMapArg->extent[0] != dimCount) || 
        (coordDimMapArg->extent[1] != dimCount)) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "- coordDimMap and distgrid (and perhaps undistUBound) mismatch",
        ESMC_CONTEXT, &rc);
      return rc;
    }
    for(int i=0, k=0; i<dimCount; i++) {
      for (int j=0; j<dimCount; j++) {
    coordDimMap[j][i]=coordDimMapArg->array[k]-1;
        k=k+1;  
      }
    }
  }

  minIndex = new int[dimCount];
  maxIndex = new int[dimCount];

  if (!present(minIndexArg)) {
    for (int i=0; i<dimCount; i++) {
      minIndex[i]=1;
    }
  } else {
    if (minIndexArg->dimCount != 1) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- minIndex array must be of rank 1", ESMC_CONTEXT, &rc);
      return rc;
    }
    if (minIndexArg->extent[0] != dimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "- minIndex and grid dimension mismatch", ESMC_CONTEXT, &rc);
      return rc;
    }
    for (int i=0; i<dimCount; i++){
      minIndex[i] = minIndexArg->array[i];  // copy minIndex
    }
  }    

  if (maxIndexArg->dimCount != 1) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- maxIndex array must be of rank 1", ESMC_CONTEXT, &rc);
    return rc;
  }
  if (maxIndexArg->extent[0] != dimCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "- maxIndex and grid dimension mismatch", ESMC_CONTEXT, &rc);
    return rc;
  }
  for (int i=0; i<dimCount; i++){
    maxIndex[i] = maxIndexArg->array[i];  // copy maxIndex
  }
  
  localArbIndexCount = localArbIndexCountArg;
  if (localArbIndexCount > 0) {
    localArbIndex = _allocate2D<int>(localArbIndexCount, distDimCount);
    for (int i=0, k=0; i < localArbIndexCount; i++)
      for (int j=0; j < distDimCount; j++) {
        k=j*localArbIndexCount+i;
    localArbIndex[i][j]=localArbIndexArg->array[k];
      }
  } else {
    localArbIndex = ESMC_NULL_POINTER;
  }

  distDim=ESMC_NULL_POINTER;
  if (distDimCount > 0) {
    distDim = new int[distDimCount];
    memcpy(distDim, distDimArg->array, distDimCount*sizeof(int));
  }



  // If destroyDistgrid wasn't passed in then use default, otherwise 
  // copy passed in value
  if (destroyDistgridArg==NULL) {
    destroyDistgrid=false;  // default
  } else {
    destroyDistgrid=*destroyDistgridArg;
  }

  // If destroyDELayout wasn't passed in then use default, otherwise 
  // copy passed in value
  if (destroyDELayoutArg==NULL) {
    destroyDELayout=false;  // default
  } else {
    destroyDELayout=*destroyDELayoutArg;
  }


  // construct the Grid object using the massaged parameter values
  localrc=gridArg->constructInternal(name, typekind, distgridArg, 
             distDimCount, distgridToGridMap, 
             undistDimCount, undistLBound, undistUBound,
             dimCount, gridEdgeLWidth, gridEdgeUWidth, gridAlign, 
             coordSys, coordDimCount, coordDimMap, gridMemLBound, 
             indexflag, minIndex, maxIndex, localArbIndex, 
         localArbIndexCount, arbDimArg, destroyDistgrid, destroyDELayout);
   if (ESMC_LogDefault.MsgFoundError(localrc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // Dellocate temporay arrays
  if (present(undistUBoundArg))  delete [] undistLBound;

  if (name) delete [] name;
   
  if (distDim != ESMC_NULL_POINTER) delete [] distDim;

  delete [] distgridToGridMap;

  delete [] gridEdgeLWidth;

  delete [] gridEdgeUWidth;

  delete [] gridAlign;

  delete [] gridMemLBound;

  delete [] coordDimCount;

  delete [] minIndex;
 
  delete [] maxIndex;

  _free2D<int> (&coordDimMap);

  if (localArbIndexCount > 0) {
     _free2D<int> (&localArbIndex);
  }
  return ESMF_SUCCESS;
}

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::setGridDefaultsLUA()"
//BOPI
// !IROUTINE:  Grid::setGridDefaultsLUA
//
// !INTERFACE:
int setGridDefaultsLUA(
//
// !RETURN VALUE:
//   return code
//
// !ARGUMENTS:
//
                       int dimCount,                // Size of the input arrays
                       InterArray<int> *gridEdgeLWidthIn,  // (in) optional
                       InterArray<int> *gridEdgeUWidthIn,  // (in) optional
                       InterArray<int> *gridAlignIn,   // (in) optional
                       int *gridEdgeLWidthOut,          // (out)
                       int *gridEdgeUWidthOut,          // (out)
                       int *gridAlignOut            // (out)
                       ){
//
// !DESCRIPTION:
//   This routine takes in gridEdgeLWidth, gridEdgeUWidth and gridAlignment 
// information provided by the user and sets the output variables appropriately.
// If the user provides a value its used
// (or an error may be generated), otherwise a sensible default is used.
// This logic is encapsulated in one place so it'll be consistent everywhere. 
//
// NOTE: all the input arrays must at least be of size dimCount. (not present, optional,
//       InterArrays need not obey this)
//  
//EOPI
//-----------------------------------------------------------------------------
  static int gridEdgeLWidthDefault[ESMF_MAXDIM]={0,0,0,0,0,0,0};
  static int gridEdgeUWidthDefault[ESMF_MAXDIM]={1,1,1,1,1,1,1};
  static int gridAlignDefault[ESMF_MAXDIM]={-1,-1,-1,-1,-1,-1,-1};
  int localrc, rc;

  // initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  
  localrc= setDefaultsLUA(dimCount,
                   gridEdgeLWidthIn, gridEdgeUWidthIn, gridAlignIn,
                   gridEdgeLWidthDefault, gridEdgeUWidthDefault, gridAlignDefault,
                   gridEdgeLWidthOut, gridEdgeUWidthOut, gridAlignOut);
   if (ESMC_LogDefault.MsgFoundError(localrc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return success
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::setDefaultsLUA()"
//BOPI
// !IROUTINE:  Grid::process
//
// !INTERFACE:
int setDefaultsLUA(
//
// !RETURN VALUE:
//   return code
//
// !ARGUMENTS:
//
                   int dimCount,                // all of the input arrays must be of at least this size 
                   InterArray<int> *lWidthIn,  // (in) optional
                   InterArray<int> *uWidthIn,  // (in) optional
                   InterArray<int> *alignIn,   // (in) optional
                   int *lWidthDefault,      // (in)
                   int *uWidthDefault,      // (in)
                   int *alignDefault,       // (in)
                   int *lWidthOut,          // (out)
                   int *uWidthOut,          // (out)
                   int *alignOut            // (out)
 ){
//
// !DESCRIPTION:
//   This routine takes in LWidth, UWidth and Alignment information provided by the user
// and sets the output variables appropriately. If the user provides a value its used
// (or an error may be generated), otherwise a (hopefully) sensible default is used.
// This logic is encapsulated in one place so it'll be consistent everywhere. 
//
// NOTE: all the input arrays must at least be of size dimCount. (not present, optional,
//       InterArrays need not obey this)
//  
// NOTE: This routine only does minimal error checking of the inputs.
//
//EOPI
//-----------------------------------------------------------------------------
  int rc;

  // initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  
  // At least make sure the input arrays are the right size
  if (present(lWidthIn)) {
    if (lWidthIn->extent[0] < dimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
                        "- LWidth size too small ", ESMC_CONTEXT, &rc);
      return rc;
    }
  }

  if (present(uWidthIn)) {
    if (uWidthIn->extent[0] < dimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
                        "- UWidth size too small ", ESMC_CONTEXT, &rc);
      return rc;
    }
  }

  if (present(alignIn)) {
    if (alignIn->extent[0] < dimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "- align size too small ", ESMC_CONTEXT, &rc);
      return rc;
    }
  }

  // set outputs based on presence of inputs
  if (!present(lWidthIn)) { // lWidthIn NOT present
    if (!present(uWidthIn)) { // uWidthIn NOT present
      if (!present(alignIn)) { // alignIn NOT present
        // present:       ,       ,
        memcpy(lWidthOut,lWidthDefault,dimCount*sizeof(int));        
        memcpy(uWidthOut,uWidthDefault,dimCount*sizeof(int));        
        memcpy(alignOut,alignDefault,dimCount*sizeof(int));        
    
      } else { // alignIn present
        // present:       ,       , alignIn        
        for (int i=0; i<dimCount; i++) {
          if (alignIn->array[i] > 0) {
            lWidthOut[i]= 1;
            uWidthOut[i]= 0;
          } else if (alignIn->array[i] < 0) {
            lWidthOut[i]= 0;
            uWidthOut[i]= 1;
          } else {
            lWidthOut[i]= 0;
            uWidthOut[i]= 0;
          }
        }        
        memcpy(alignOut,alignIn->array,dimCount*sizeof(int));        
        
      }
    } else { // uWidthIn present
      if (!present(alignIn)) { // alignIn NOT present
        // present:       , UWidth,
        for (int i=0; i<dimCount; i++)
          lWidthOut[i]=0;
        memcpy(uWidthOut,uWidthIn->array,dimCount*sizeof(int));        
        for (int i=0; i<dimCount; i++)
          alignOut[i]=-1;
      } else { // alignIn present
        // present:       , UWidth, alignIn
        for (int i=0; i<dimCount; i++)
          lWidthOut[i]=0;
        memcpy(uWidthOut,uWidthIn->array,dimCount*sizeof(int));        
        memcpy(alignOut,alignIn->array,dimCount*sizeof(int));                
      }
    }
  } else { // lWidthIn present
    if (!present(uWidthIn)) { // uWidthIn NOT present
      if (!present(alignIn)) { // alignIn NOT present
        // present: lWidth,       ,
        memcpy(lWidthOut,lWidthIn->array,dimCount*sizeof(int));        
        for (int i=0; i<dimCount; i++)
          uWidthOut[i]=0;
        for (int i=0; i<dimCount; i++)
          alignOut[i]=1;
   
      } else { // alignIn present
        // present: lWidth,       , alignIn
        memcpy(lWidthOut,lWidthIn->array,dimCount*sizeof(int));        
        for (int i=0; i<dimCount; i++)
          uWidthOut[i]=0;
        memcpy(alignOut,alignIn->array,dimCount*sizeof(int));                
     
      }      
    } else { // uWidthIn present
      if (!present(alignIn)) { // alignIn NOT present
        // present: lWidth, UWidth, 
        memcpy(lWidthOut,lWidthIn->array,dimCount*sizeof(int));        
        memcpy(uWidthOut,uWidthIn->array,dimCount*sizeof(int));        
        for (int i=0; i<dimCount; i++) {
          if (lWidthIn->array[i] <= uWidthIn->array[i]) {
            alignOut[i]=-1;
          } else {
            alignOut[i]=1;
          }
        }
      } else { // alignIn present
        // present: lWidth, UWidth, alignIn
        memcpy(lWidthOut,lWidthIn->array,dimCount*sizeof(int));        
        memcpy(uWidthOut,uWidthIn->array,dimCount*sizeof(int));        
        memcpy(alignOut,alignIn->array,dimCount*sizeof(int));                

      }      
    }
  }

  // return success
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
//  Grid Iter Routines
//
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridIter::getDEBnds()"
//BOPI
// !IROUTINE:  getDEBnds
//
// !INTERFACE:
void GridIter::getDEBnds(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//  
                         int localDE,
                         int *uBnd,
                         int *lBnd
 ){
//
// !DESCRIPTION:
// Get the bounds for this iterator corresponding to the given localDE. 
//
//EOPI
//-----------------------------------------------------------------------------

  // Set Bounds of iteration on this proc
  grid->getDistExclusiveUBound(staggerDistgrid, localDE, uBnd);  
  grid->getDistExclusiveLBound(staggerDistgrid, localDE, lBnd);  

   // if cell iterator then expand bounds
  if (cellNodes) {
      int localrc,rc;
      for (int i=0; i<rank; i++) {
        bool isLBnd=grid->isStaggerLBnd(staggerloc, localDE, i);
        bool isUBnd=grid->isStaggerUBnd(staggerloc, localDE, i);

        //// Expand to include all nodes touched by cells on this proc
        if (!isLBnd) lBnd[i]--;
        if (!isUBnd) uBnd[i]++; 
      } 


        // Expand for bipole, if center stagger
      if (staggerloc == 0) {
        for (int i=0; i<rank; i++) {
          //// Expand to include all nodes touched by cells on this proc
          if (grid->isLBnd(localDE,i) && (connL[i]==ESMC_GRIDCONN_BIPOLE)) lBnd[i]--;
          if (grid->isUBnd(localDE,i) && (connU[i]==ESMC_GRIDCONN_BIPOLE)) uBnd[i]++; 
        } 
      }
  }
 
}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridIter::setDEBnds()"
//BOPI
// !IROUTINE:  setDEBnds
//
// !INTERFACE:
void GridIter::setDEBnds(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//  
   int localDE
 ){
//
// !DESCRIPTION:
// Set the bounds in this iterator to the values corresponding to
// this DE. 
//
//EOPI
//-----------------------------------------------------------------------------


  // Set Bounds of iteration on this proc
  this->getDEBnds(localDE,uBndInd,lBndInd);

    // Setup info for calculating the DE index tuple location quickly
  // Needs to be done after bounds are set
  int currOff=1;
  lOff=0;
  for (int i=0; i<rank; i++) {
    dimOff[i]=currOff;
    lOff +=currOff*lBndInd[i];

    currOff *=(uBndInd[i]-lBndInd[i]+1);
  }  
 
  // Exclusive Bounds
  grid->getDistExclusiveLBound(staggerDistgrid, localDE, exLBndInd);  

   // Get Original bounds for detecting localness
  grid->getDistExclusiveUBound(staggerDistgrid, localDE, uBndOrig);  
  grid->getDistExclusiveLBound(staggerDistgrid, localDE, lBndOrig);  

  // Set to first index on DE
  for (int i=0; i<rank; i++) {
    curInd[i]=lBndInd[i];
  }

  // Temporarily set min/max
  int localrc;
  const int *localDEList= staggerDistgrid->getDELayout()->getLocalDeToDeMap();
  const int *DETileList = staggerDistgrid->getTileListPDe();
  int tile=DETileList[localDEList[localDE]];
 
  const int *tileMin=staggerDistgrid->getMinIndexPDimPTile(tile, &localrc);
  const int *tileMax=staggerDistgrid->getMaxIndexPDimPTile(tile, &localrc);

   for (int i=0; i<rank; i++) {
    minInd[i]=tileMin[i];
    maxInd[i]=tileMax[i];
  }

#if 0
   printf("new DE ------- \n");
   printf(" rank=%d \n",rank);
   printf(" lbnd=[%d,%d] \n",lBndInd[0],lBndInd[1]);
   printf(" ubnd=[%d,%d] \n",uBndInd[0],uBndInd[1]);
   printf(" cur=[%d,%d] \n",curInd[0],curInd[1]);
   printf("new DE ------- \n");
#endif

}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridIter::advToNonZeroDE()"
//BOPI
// !IROUTINE:  advToNonZeroDE()
 //
// !INTERFACE:
bool GridIter::advToNonEmptyDE(
//
// !RETURN VALUE:
//    true - advanced to next DEs
//    false - there were no more non-empty DEs
// !ARGUMENTS:
//  
   int *_localDE
 ){
//
// !DESCRIPTION:
// Starting with the current value of localDE advance to the next non-empty one. 
//
//EOPI
//-----------------------------------------------------------------------------

  int tmp_uBnd[ESMF_MAXDIM];
  int tmp_lBnd[ESMF_MAXDIM];

 /* XMRKX */
 
  // temp localDE to advance
  int localDE=*_localDE;

  // Loop until we've found a non-empty DE
  while (localDE <= uBndDE) {

    // Set Bounds of iteration on this proc
    this->getDEBnds(localDE,tmp_uBnd,tmp_lBnd);

    // See if the DE has 0-width in some dimension
    bool empty=false;
    for (int i=0; i<rank; i++) {
      if (tmp_uBnd[i] < tmp_lBnd[i]) {
        empty=true;
        break;
      }
    }  

    // If it's not empty then set output and return
    if (!empty) {
      *_localDE=localDE;
      return true;
    }

    // Advance to next DE 
     localDE++;
  }

  // return that there were no more empty DEs
  *_localDE=localDE;
  return false;

}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
 #undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridIter()"
//BOPI
 // !IROUTINE:  GridIter Construct
//
 // !INTERFACE:
GridIter::GridIter(
//
// !RETURN VALUE:
//    Pointer to a new Grid Iterator
//
 // !ARGUMENTS:
//  
 Grid *gridArg,
 int  staggerlocArg,
 bool cellNodesArg
  ){
//
// !DESCRIPTION:

//
//EOPI
 //-----------------------------------------------------------------------------

  // Set parameters
  grid=gridArg;
  staggerloc=staggerlocArg;
  rank=grid->getDimCount();
  cellNodes=cellNodesArg;
  connL=grid->getConnL();
  connU=grid->getConnU();

  // Get distgrid for this staggerloc 
  grid->getStaggerDistgrid(staggerloc, &staggerDistgrid);

  // Set isLBnd
   /* XMRKX */
 
   // initialize 
  for (int i=0; i<ESMF_MAXDIM; i++) {
     curInd[i]=0;
    lBndInd[i]=0;
    uBndInd[i]=0;
  }
  curDE=0;
  uBndDE=0;
  numDE=0;

  // set number of local DEs
  numDE=staggerDistgrid->getDELayout()->getLocalDeCount();

  // set end of local DEs
  uBndDE=numDE-1;

  // Get number of tiles
  tileCount=grid->getTileCount();

  // set to beginning (just in case)
  this->toBeg();

}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridIter::toBeg()"
 //BOPI
 // !IROUTINE:  toBeg
//
// !INTERFACE:
GridIter *GridIter::toBeg(
//
// !RETURN VALUE:
//    GridIter object
//
// !ARGUMENTS:
  //  
 ){
//
// !DESCRIPTION:
// Move to beginning of iteration list
//
//EOPI
//-----------------------------------------------------------------------------

  // If no DEs then just set iterator to done 
  if (numDE==0) {
    done=true;
     return this;
  } 

  // Set to first DE 
  curDE=0; 

   // Advance to first non-empty position
   // If there are none, then set as done and leave
  if (!this->advToNonEmptyDE(&curDE)) {
    done=true;
    return this;
  }

  // Set to beginning 
  this->setDEBnds(curDE);
  
  // Set to first index
  for (int i=0; i<rank; i++) {
    curInd[i]=lBndInd[i];
  }

  // set done status
  if (curDE > uBndDE) { 
    done=true;
  } else {
    done=false;
  }
  
  // return pointer to GridIter
  return this;
}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridIter::adv()"
//BOPI
// !IROUTINE:  GridIter advance
//
// !INTERFACE:
GridIter *GridIter::adv(
 //
 // !RETURN VALUE:
//    none
//
// !ARGUMENTS:
  //  

 ){
//
// !DESCRIPTION:
// Move to next item in grid index list
//
//EOPI
//-----------------------------------------------------------------------------

    // if done then leave
   if (done) return this;

    //printf("A cur=[%d,%d] uBnd=[%d,%d]\n",curInd[0],curInd[1],uBndInd[0],uBndInd[1]);

  // advance first index
  curInd[0]++;

  // if greater than upper bound advance rest of indices
  if (curInd[0] > uBndInd[0]) {

    //// advance the rest of the indices
    int i=1;
    while (i<rank) {
      curInd[i-1]=lBndInd[i-1]; 

      curInd[i]++;

      if (curInd[i] <= uBndInd[i]) break;               
  
      i++;
    }

    //// advance the DE if necessary 
    if (i==rank) {
      curDE++;

      ////// If we're past the top of the DEs then we're done
      if (curDE > uBndDE) { 
        done=true;
        return this;
      }

      // Advance to first non-empty position
      // If there are none, then set as done and leave
      if (!this->advToNonEmptyDE(&curDE)) {
        done=true;
        return this;
       }

       ////// Set the boundaries based on this DE
      this->setDEBnds(curDE);
    }
  }

  // return pointer to object
  return this;
}
//-----------------------------------------------------------------------------



  //-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridIter::getGlobalID()"
//BOPI
// !IROUTINE:  GridIter 
//
// !INTERFACE:
int GridIter::getGlobalID(
//
// !RETURN VALUE:
//    global id
//
// !ARGUMENTS:
//   none  
 ){
//
// !DESCRIPTION:
// return the global identifier of this item
//
 //EOPI
//-----------------------------------------------------------------------------
  int localrc;
  int gid;
  int deBasedInd[ESMF_MAXDIM];
 
  // if done then leave
  if (done) return -1;

  // Convert to DE based
  for (int i=0; i<rank; i++) {
    deBasedInd[i]=curInd[i]-exLBndInd[i];
  }
  
  //printf("GI curDE=%d curInd=%d %d deBasedInd=%d %d \n",
   //  curDE,curInd[0],curInd[1],deBasedInd[0],deBasedInd[1]);  
  
  // determine sequence index
  std::vector<int> seqIndex;
  localrc=staggerDistgrid->getSequenceIndexLocalDe(curDE,deBasedInd,seqIndex);
  
  //#define REPORT_DEGENERACY
#ifdef REPORT_DEGENERACY
  {
    std::stringstream debugmsg;
    if (seqIndex.size()>1){
      debugmsg << "degeneracy detected in GridIter:";
      for (unsigned int i=0; i<seqIndex.size(); i++)
        debugmsg << " ["<<i<<"]=" << seqIndex[i];
      ESMC_LogDefault.Write(debugmsg.str(), ESMC_LOGMSG_INFO);
    }
  }
#endif
  
  if (seqIndex.size() > 0)
    gid = seqIndex[0];
  else
    gid = -1; // invalidate
  
  //printf("GI Gid=%d curDE=%d curInd=%d %d deBasedInd=%d %d localrc=%d ESMC_SUCCESS=%d \n",
  //  gid,curDE,curInd[0],curInd[1],deBasedInd[0],deBasedInd[1],localrc,ESMF_SUCCESS);

  return gid;
 
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridIter::getPoleID()"
//BOPI
 // !IROUTINE:  GridIter 
//
// !INTERFACE:
int GridIter::getPoleID(
//
// !RETURN VALUE:
//    if this node is next to a pole then return the pole id, otherwise 
//  return 0.
//
// !ARGUMENTS:
//   none  
 ){
//
// !DESCRIPTION:
//    if this node is next to a pole then return the pole id, otherwise 
//  return 0.
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc, rc;

  // if done then leave
  if (done) return 0;

  // Regridding currently doesn't generate poles for 3 or greater dimension grids, 
  // so leave if that's the case
  if (rank > 2) return 0;
 
  // check to see if we're on this proc
  for (int i=0; i<rank; i++) {
    bool isLBnd=grid->isStaggerLBnd(staggerloc, curDE, i);
    bool isUBnd=grid->isStaggerUBnd(staggerloc, curDE, i);
    
    if ((curInd[i]==lBndInd[i]) && isLBnd && (connL[i]==ESMC_GRIDCONN_POLE)) return 2*(i+1);
    if ((curInd[i]==uBndInd[i]) && isUBnd && (connU[i]==ESMC_GRIDCONN_POLE)) return 2*(i+1)+1;
  }

  // if we pass the above test then we're not next to a pole node
  return 0;
}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridIter::getCount()"
//BOPI
// !IROUTINE:  GridIter::getCount
//
// !INTERFACE:
int GridIter::getCount(
//
// !RETURN VALUE:
//    the number of nodes in this iterator
//
 // !ARGUMENTS:
//   none  
 ){
//
// !DESCRIPTION:
// Return the number of nodes in this iterator. 
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc;
  int cnt, cntDE;
  int lBnd[ESMF_MAXDIM];
  int uBnd[ESMF_MAXDIM];

  // Loop through DEs count nodes
  cnt=0;
   for (int d=0; d<numDE; d++) {
    // Set Bounds of iteration on this proc
    this->getDEBnds(d,uBnd,lBnd);

    // For this DE get the number of nodes
    cntDE=1;
    for (int i=0; i<rank; i++) {
       cntDE *= (uBnd[i]-lBnd[i]+1);
    }

    // Add the size of this DE to the rest
    cnt +=cntDE;
  }

  // Output count
  return cnt;

}
//-----------------------------------------------------------------------------
 


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridIter::getDE()"
//BOPI
// !IROUTINE:  GridIter::getDE
//
// !INTERFACE:
int GridIter::getDE(
//
// !RETURN VALUE:
//    the DE of the current position
//
// !ARGUMENTS:
//   none  
 ){
//
// !DESCRIPTION:
// Return the DE of the current position
//
//EOPI
//-----------------------------------------------------------------------------

  // if we're done return -1
  if (done) return -1;
  
  // Get some useful information
   const int *localDeToDeMap = staggerDistgrid->getDELayout()->getLocalDeToDeMap();
  
  // Output DE
  return localDeToDeMap[curDE];
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridIter::isLocal()"
 //BOPI
// !IROUTINE:  isLocal 
//
// !INTERFACE:
bool GridIter::isLocal(
//
// !RETURN VALUE:
//    returns true if current index location is on this processor
//
// !ARGUMENTS:
//   none  
 ){
//
// !DESCRIPTION:
// return true if this item is on this processor
//
 //EOPI
//-----------------------------------------------------------------------------

  // if done then leave
  if (done) return false;

  // if not cell then they're all on this proc
   if (!cellNodes) return true;
 
  // TODO: FIX THIS FOR DISTGRID CONNECTIONS!!!!!!!!!!!!!!

  // check to see if we're on this proc
  for (int i=0; i<rank; i++) {
    if ((curInd[i]<lBndOrig[i]) || (curInd[i]>uBndOrig[i])) return false;
  }

  // if we pass the above test then we're on the proc
  return true;
}
//-----------------------------------------------------------------------------

 

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridIter::isShared()"
//BOPI
// !IROUTINE:  isShared 
//
// !INTERFACE:
bool GridIter::isShared(
//
// !RETURN VALUE:
//    returns true if the current index MAY have a copy on another processor.  
//    return false otherwise
//
// !ARGUMENTS:
//   none  
 ){
//
// !DESCRIPTION:
//    returns true if current index location MAY  have a ghost copy on another processor 
// due to the overlap of cell nodes. Note, if the iterator is not through cell nodes then
// there isn't an overlap.
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc;

  // if done then leave
  if (done) return false;

  // if not cell then they're no shared nodes
  if (!cellNodes) return false;

  // For single tile grids, if we're more than 1 inside the 
  //  exclusive region then we shouldn't be shared
  bool interior=true;
  if (tileCount <= 1) {
    for (int i=0; i<rank; i++) {
      if ((curInd[i]<lBndOrig[i]+1) || (curInd[i]>uBndOrig[i]-1)) {
        interior=false;
        break;
      }
    }
  } else {
    // multi-tile grids need 1 deeper sharing because of inter-tile connections
    for (int i=0; i<rank; i++) {
      if ((curInd[i]<lBndOrig[i]+2) || (curInd[i]>uBndOrig[i]-2)) {
        interior=false;
        break;
      }
    }
  }

  // If we're inside then we're not shared
  if (interior) return false;

  // TODO: Could also check if we're next to a non-shared edge

  // If none of the above are true then return true, because we might be shared
   return true;
 }
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridIter::getLocalID()"
//BOPI
// !IROUTINE:  getLocalID
//
// !INTERFACE:
int GridIter::getLocalID(
//
// !RETURN VALUE:
//  local id
//
// !ARGUMENTS:
//   none  
 ){
//
// !DESCRIPTION:
//   returns a local ID (an id unique on this processor) for an iteration location. 
//   Note that the range of local IDs is not necessarily continuous or contiguous
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc;
  int dePos;  

  // if done then leave
  if (done) return -1;

  // compute position in DE
  dePos=-lOff;
  for (int i=0; i<rank; i++) {
    dePos +=dimOff[i]*curInd[i];
  }

  // Add in DE number and output
   return dePos*numDE+curDE;

 }
//-----------------------------------------------------------------------------



 //-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridIter::getCoord()"
//BOPI
// !IROUTINE:  getCoord
//
// !INTERFACE:
template <class TYPE>
void GridIter::getCoord(
//
// !RETURN VALUE:
//  void
//
// !ARGUMENTS:
//   Coordinate output 
// 
                         TYPE *coord // (out) input array needs to be at
                                       // least of size grid dimCount    
 ){
//
// !DESCRIPTION:
//  Returns the coordinates for an iteration location. Array should be at least
// be of size Grid dimCount. Converts coords to input typekind.
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc, rc;

  // if done then leave
  if (done) return;

  // get coordinates
  localrc=grid->getCoordInternalConvert(staggerloc, curDE, curInd, coord);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                    &rc)) throw rc;

}
// Add more types here if necessary
template void GridIter::getCoord(ESMC_R8 *data);
template void GridIter::getCoord(ESMC_R4 *data);
template void GridIter::getCoord(ESMC_I4 *data);
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::GridIter::getCartCoord()"
  //BOPI
// !IROUTINE:  getCartCoord
//
// !INTERFACE:
template <class TYPE>
void GridIter::getCartCoord(
//
// !RETURN VALUE:
//  void
//
// !ARGUMENTS:
//   Coordinate output 
// 
                        TYPE *coord // (out) input array needs to be at
                                       // least of size grid CartCoordDimCount    
 ){
//
// !DESCRIPTION:
//  Returns the cartesian coordinates for an iteration location. Array should be at least
// be of size Grid CartCoordDimCount.
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc,rc;
  TYPE orig_coord[ESMF_MAXDIM];

  // if done then leave
  if (done) return;

  // get coordinates
  localrc=grid->getCoordInternalConvert(staggerloc, curDE, curInd, orig_coord);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                    &rc)) throw rc;
  
  // Call into coordsys method to convert to Cart
  localrc=ESMCI_CoordSys_ConvertToCart(grid->getCoordSys(),
                                       grid->getDimCount(),
                                       orig_coord,  // Input coordinates 
                                       coord);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                    &rc)) throw rc;
  

}
// Add more types here if necessary
template void GridIter::getCartCoord(ESMC_R8 *data);
//template void GridIter::getCoord(ESMC_R4 *data);
//template void GridIter::getCoord(ESMC_I4 *data);
 //-----------------------------------------------------------------------------


 //-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridIter::getItem()"
//BOPI
 // !IROUTINE:  getItem
//
// !INTERFACE:
template <class TYPE>
void GridIter::getItem(
//
// !RETURN VALUE:
//  void
//
// !ARGUMENTS:
//   Value output 
// 
                int item,     // item type
               TYPE *value // (out) input array needs to be at
                                       // least of the size of 1 item    
 ){
//
// !DESCRIPTION:
//  Returns the item value for an iteration location.
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc, rc;

  // if done then leave
  if (done) return;

  // get item
  localrc=grid->getItemInternalConvert(staggerloc, item, curDE, curInd, value);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                    &rc)) throw rc;
}
// Add more types here if necessary
template void GridIter::getItem(int item, ESMC_R8 *data);
template void GridIter::getItem(int item, ESMC_R4 *data);
template void GridIter::getItem(int item, ESMC_I4 *data);
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::GridIter::getArrayData()"
//BOPI
// !IROUTINE:  getArrayData
//
// !INTERFACE:
template <class TYPE>
void GridIter::getArrayData(
 //
// !RETURN VALUE:
//  void
//
// !ARGUMENTS:
//   Data output
// 
                             Array *array,
                            TYPE *data // (out) input array needs to be at
                                       // least of size grid dimCount    
 ){
//
// !DESCRIPTION:
// Get data from a passed in Array
// TODO: Need to come up with a way to handle Arrays with more dimensions than the Grid
// TODO: Need error checking!!!!!
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc;
  LocalArray *localArray;
  
  // if done then leave
  if (done) return;

  // Check typekind
  if (array->getTypekind() != get_TypeKind_from_Type<TYPE>()) {
    int rc;
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
               "- requested type does not match type of data in Array ", ESMC_CONTEXT, &rc);
    throw rc;
  }
  
  //// Get LocalArray cooresponding to staggerloc, coord and localDE
  localArray=array->getLocalarrayList()[curDE];
  
  //// Get pointer to LocalArray data
  localArray->getDataInternal(curInd, data);
 }

// Add more types here if necessary
template void GridIter::getArrayData(Array *array, ESMC_R8 *data);
template void GridIter::getArrayData(Array *array, ESMC_R4 *data);
template void GridIter::getArrayData(Array *array, ESMC_I4 *data);
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::GridIter::getArrayData()"
//BOPI
// !IROUTINE:  getArrayData
//
// !INTERFACE:
template <class TYPE>
void GridIter::setArrayData(
//
// !RETURN VALUE:
//  void
 //
// !ARGUMENTS:
//   Data output
// 
                            Array *array,
                            TYPE data 
 ){
//
// !DESCRIPTION:
// Set data to a passed in Array
// TODO: Need to come up with a way to handle Arrays with more dimensions than the Grid
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc;
  LocalArray *localArray;
  
  // if done then leave
  if (done) return;
  
  // Check typekind
  if (array->getTypekind() != get_TypeKind_from_Type<TYPE>()) {
    int rc;
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
               "- requested type does not match type of data in Array ", ESMC_CONTEXT, &rc);
    throw rc;
  }
 
  //// Get LocalArray cooresponding to staggerloc, coord and localDE
  localArray=array->getLocalarrayList()[curDE];
  
  //// Get pointer to LocalArray data
  localArray->setData(curInd, data);
  
}

// Add more types here if necessary
template void GridIter::setArrayData(Array *array, ESMC_R8 data);
template void GridIter::setArrayData(Array *array, ESMC_R4 data);
template void GridIter::setArrayData(Array *array, ESMC_I4 data);
//-----------------------------------------------------------------------------

 

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridIter::moveToLocalID()"
//BOPI
// !IROUTINE:  moveToLocalID
//
// !INTERFACE:
GridIter *GridIter::moveToLocalID(
//
// !RETURN VALUE:
//    returns the grid iterator
 //
// !ARGUMENTS:
//   
 int localID){
//
// !DESCRIPTION:
// Move to the position in the iteration list represented by local id
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc;
  int de,dePos,cnt;  

  // compute DE and the dePos
  de=localID%numDE;
  dePos=localID/numDE;

  // check for bad DE
  if (de > uBndDE) return this;

   // set DE
  curDE=de;

  // load DE bounds and other info
  this->setDEBnds(curDE);  

  // reset current index location using dePos 
  for (int i=0; i<rank-1; i++) {
    cnt=uBndInd[i]-lBndInd[i]+1;
    curInd[i] = dePos%cnt+lBndInd[i];
    dePos /=cnt;
  }
  curInd[rank-1]=dePos+lBndInd[rank-1];

  //  printf("lid=%d  DE=%d Ind=%d %d\n",localID,de,curInd[0],curInd[1]);

  // since we're now not done set done to false
  done=false;

   // Add in DE number and output
  return this;

}
//-----------------------------------------------------------------------------




//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::~GridIter()"
//BOPI
// !IROUTINE:  GridIter Destruct
//
// !INTERFACE:
 GridIter::~GridIter(
//
// !RETURN VALUE:
//    void
//
// !ARGUMENTS:
//
 void  
) {
//
// !DESCRIPTION:

//
 //EOPI
//-----------------------------------------------------------------------------

}
//-----------------------------------------------------------------------------




//-----------------------------------------------------------------------------
//
//  GridCellIter Routines
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridCellIter::getDEBnds()"
//BOPI
// !IROUTINE:  getDEBnds
//
// !INTERFACE:
void GridCellIter::getDEBnds(
 //
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//  
                          int localDE,
                         int *uBnd,
                         int *lBnd
 ){
//
// !DESCRIPTION:
// Get the bounds for this iterator corresponding to the given localDE. 
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc, rc;

  // Set Bounds of iteration on this proc
  grid->getDistExclusiveUBound(staggerDistgrid, localDE, uBnd);  
  grid->getDistExclusiveLBound(staggerDistgrid, localDE, lBnd);  

  // Adjust to be cell bounds
  if (staggerloc==0) {

      for (int i=0; i<rank; i++) {
        bool isLBnd=grid->isStaggerLBnd(staggerloc, localDE, i);
        bool isUBnd=grid->isStaggerUBnd(staggerloc, localDE, i);
        
        // Adjust based on alignment
        if (align[i] < 0) {
          if (isUBnd) uBnd[i]--;
        } else {
          if (isLBnd) lBnd[i]++;
        }    

        // Reexpand for bipole
        if (isLBnd && (connL[i]==ESMC_GRIDCONN_BIPOLE)) lBnd[i]--;
        if (isUBnd && (connU[i]==ESMC_GRIDCONN_BIPOLE)) uBnd[i]++; 
      }

    } else {
      // Calculations are different for single and multi-tile cases
      if (tileCount <= 1) {
        for (int i=0; i<rank; i++) {
          bool isLBnd=grid->isStaggerLBnd(staggerloc, localDE, i);
          bool isUBnd=grid->isStaggerUBnd(staggerloc, localDE, i);
          
          if (align[i] <0) {
            if (isUBnd) uBnd[i]--;
          } else {
            if (isLBnd) lBnd[i]++;
          }    
        }
      } else {
        // Get tile min/max
        int localrc,rc;
        const int *localDEList= staggerDistgrid->getDELayout()->getLocalDeToDeMap();
        const int *DETileList = staggerDistgrid->getTileListPDe();
        int tile=DETileList[localDEList[localDE]];
        
        const int *tileMin=staggerDistgrid->getMinIndexPDimPTile(tile, &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                          &rc)) throw rc;
        const int *tileMax=staggerDistgrid->getMaxIndexPDimPTile(tile, &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                          &rc)) throw rc;        

        // Loop setting bounds based on tile edges
        for (int i=0; i<rank; i++) {
          
          // See if we're on the tile edge
          bool isLBnd=false;
          if (lBnd[i] == tileMin[i]) isLBnd=true;
          
          bool isUBnd=false;
          if (uBnd[i] == tileMax[i]) isUBnd=true;
          
          // Adjust bounds
          if (align[i] <0) {
            if (isUBnd) uBnd[i]--;
          } else {
            if (isLBnd) lBnd[i]++;
          }    
        }
      }
    }
}

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridCellIter::setDEBnds()"
//BOPI
// !IROUTINE:  setDEBnds
//
// !INTERFACE:
void GridCellIter::setDEBnds(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//  
   int localDE
 ){
//
// !DESCRIPTION:
// Set the bounds in this iterator to the values corresponding to
// this DE. 
//
 //EOPI
//-----------------------------------------------------------------------------

  // Set Bounds of iteration on this proc
  this->getDEBnds(localDE, uBndInd, lBndInd);
 
  // Get exclusive bounds for center stagger
  grid->getExclusiveLBound(0, localDE, exLBndInd);

  // Setup info for calculating the DE index tuple location quickly
  // Needs to be done after bounds are set
  int currOff=1;
  lOff=0;
  for (int i=0; i<rank; i++) {
    dimOff[i]=currOff;
    lOff +=currOff*lBndInd[i];

    currOff *=(uBndInd[i]-lBndInd[i]+1);
  }  

  // Set to first index on DE
  for (int i=0; i<rank; i++) {
    curInd[i]=lBndInd[i];
  }

  // Temporarily set min/max
  int localrc;
  const int *localDEList= staggerDistgrid->getDELayout()->getLocalDeToDeMap();
  const int *DETileList = staggerDistgrid->getTileListPDe();
  int tile=DETileList[localDEList[localDE]];

  const int *tileMin=staggerDistgrid->getMinIndexPDimPTile(tile, &localrc);
  const int *tileMax=staggerDistgrid->getMaxIndexPDimPTile(tile, &localrc);
    
   for (int i=0; i<rank; i++) {
    minInd[i]=tileMin[i];
    maxInd[i]=tileMax[i];
  }


}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridIter::advToNonZeroDE()"
//BOPI
 // !IROUTINE:  advToNonZeroDE()
//
// !INTERFACE:
bool GridCellIter::advToNonEmptyDE(
//
// !RETURN VALUE:
//    true - advanced to next DEs
//    false - there were no more non-empty DEs
// !ARGUMENTS:
 //  
   int *_localDE
 ){
//
 // !DESCRIPTION:
// Starting with the current value of localDE advance to the next non-empty one. 
//
//EOPI
//-----------------------------------------------------------------------------

  int tmp_uBnd[ESMF_MAXDIM];
  int tmp_lBnd[ESMF_MAXDIM];

 /* XMRKX */

  // temp localDE to advance
  int localDE=*_localDE;

  // Loop until we've found a non-empty DE
  while (localDE <= uBndDE) {

    // Set Bounds of iteration on this proc
    this->getDEBnds(localDE,tmp_uBnd,tmp_lBnd);

    // See if the DE has 0-width in some dimension
    bool empty=false;
    for (int i=0; i<rank; i++) {
      if (tmp_uBnd[i] < tmp_lBnd[i]) {
        empty=true;
        break;
      }
    }  

    // If it's not empty then set output and return
    if (!empty) {
      *_localDE=localDE;
      return true;
    }

     // Advance to next DE 
    localDE++;
  }

  // return that there were no more empty DEs
  *_localDE=localDE;
  return false;

}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::GridCellIter()"
//BOPI
// !IROUTINE:  GridCellIter Construct
//
// !INTERFACE:
GridCellIter::GridCellIter(
//
 // !RETURN VALUE:
//    Pointer to a new Grid Iterator
//
// !ARGUMENTS:
//  
 Grid *gridArg,
 int  staggerlocArg

 ){
//
// !DESCRIPTION:

//
//EOPI
//-----------------------------------------------------------------------------

  // Set parameters
  grid=gridArg;
  staggerloc=staggerlocArg;

  rank=grid->getDimCount();
  connL=grid->getConnL();
  connU=grid->getConnU();

    
  // Get Alignment for staggerloc
  const int *staggerAlign= grid->getStaggerAlign(staggerloc);

  // Convert to -1,+1 alignment used in GridCellIter
   // (make 0 the same as -1)
  for (int i=0; i<rank; i++) {
    if (staggerAlign[i] < 1) align[i]=-1;
    else align[i]=1;
  }

  // Get distgrid for this staggerloc 
  grid->getStaggerDistgrid(staggerloc, &staggerDistgrid);

  // If we should use the center distgrid for calculating seqIndices
  // (e.g. if we're doing conservative regridding)
  grid->getStaggerDistgrid(0,&centerDistgrid);
  
  // initialize 
  for (int i=0; i<ESMF_MAXDIM; i++) {
    curInd[i]=0;
    lBndInd[i]=0;
     uBndInd[i]=0;
  }
  curDE=0;
  uBndDE=0;
  numDE=0;

  // set number of local DEs
  numDE=staggerDistgrid->getDELayout()->getLocalDeCount();

   // set end of local DEs
  uBndDE=numDE-1;

  // Get number of tiles
  tileCount=grid->getTileCount();


  // set to beginning (just in case)
  this->toBeg();


}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridCellIter::toBeg()"
//BOPI
// !IROUTINE:  toBeg
//
// !INTERFACE:
GridCellIter *GridCellIter::toBeg(
//
 // !RETURN VALUE:
//    GridCellIter object
//
// !ARGUMENTS:
//  
 ){
//
// !DESCRIPTION:
// Move to beginning of iteration list
//
//EOPI
//-----------------------------------------------------------------------------

  // If no DEs then just set iterator to done 
  if (numDE==0) {
    done=true;
    return this;
  } 

  // Set to first DE 
  curDE=0; 

  // Advance to first non-empty position
  // If there are none, then set as done and leave
  if (!this->advToNonEmptyDE(&curDE)) {
    done=true;
    return this;
  }

  // Set to beginning (localDE=0)
  this->setDEBnds(curDE);

  // Set to first index
  for (int i=0; i<rank; i++) {
    curInd[i]=lBndInd[i];
  }

  // set done status
  if (curDE > uBndDE) { 
    done=true;
  } else {
    done=false;
   }

  // return pointer to GridCellIter
  return this;
}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridCellIter::adv()"
//BOPI
// !IROUTINE:  GridCellIter advance
//
// !INTERFACE:
GridCellIter *GridCellIter::adv(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//  

 ){
//
// !DESCRIPTION:
// Move to next item in grid index list
//
//EOPI
//-----------------------------------------------------------------------------

   // if done then leave
  if (done) return this;

  // advance first index
  curInd[0]++;

  // if greater than upper bound advance rest of indices
   if (curInd[0] > uBndInd[0]) {

    //// advance the rest of the indices
    int i=1;
    while (i<rank) {
      curInd[i-1]=lBndInd[i-1]; 
      curInd[i]++;

      if (curInd[i] <= uBndInd[i]) break;               
 
      i++;
    }

    //// advance the DE if necessary 
    if (i==rank) {
      curDE++;

       ////// If we're past the top of the DEs then we're done
      if (curDE > uBndDE) { 
        done=true;
        return this;
      }

      // Advance to first non-empty position
      // If there are none, then set as done and leave
      if (!this->advToNonEmptyDE(&curDE)) {
        done=true;
        return this;
      }

      ////// Set the boundaries based on this DE
      this->setDEBnds(curDE);
    }
  }

  // return pointer to object
  return this;
}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridCellIter::getGlobalID()"
//BOPI
// !IROUTINE:  GridCellIter 
//
// !INTERFACE:
int GridCellIter::getGlobalID(
//
  // !RETURN VALUE:
//    global id
//
// !ARGUMENTS:
//   none  
 ){
//
// !DESCRIPTION:
// return the global identifier of this item
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc;
  int gid;
  int deBasedInd[ESMF_MAXDIM];

  // if done then leave
  if (done) return -1;


  // Convert to DE based
  for (int i=0; i<rank; i++) {
    deBasedInd[i]=curInd[i]-lBndInd[i];
  }
      
  // determine sequence index
  std::vector<int> seqIndex;
  localrc=centerDistgrid->getSequenceIndexLocalDe(curDE,deBasedInd,seqIndex);



#define REPORT_DEGENERACY
#ifdef REPORT_DEGENERACY
  {
    std::stringstream debugmsg;
    if (seqIndex.size()>1){
      debugmsg << "degeneracy detected in GridCellIter:";
      for (unsigned int i=0; i<seqIndex.size(); i++)
        debugmsg << " ["<<i<<"]=" << seqIndex[i];
      ESMC_LogDefault.Write(debugmsg.str(), ESMC_LOGMSG_INFO);
    }
  }
#endif

  if (seqIndex.size() > 0)
    gid = seqIndex[0];
  else
    gid = -1; // invalidate

  // return sequence index
  return gid;

}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridCellIter::getDE()"
//BOPI
// !IROUTINE:  GridCellIter::getDE
//
// !INTERFACE:
int GridCellIter::getDE(
//
// !RETURN VALUE:
 //    the DE of the current position
//
// !ARGUMENTS:
//   none  
 ){
//
// !DESCRIPTION:
 // Return the DE of the current position
//
//EOPI
//-----------------------------------------------------------------------------

  // if we're done return -1
  if (done) return -1;
  
  // Get some useful information
  const int *localDeToDeMap = staggerDistgrid->getDELayout()->getLocalDeToDeMap();
  
  // Output DE
  return localDeToDeMap[curDE];
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridCellIter::getCount()"
//BOPI
// !IROUTINE:  GridCellIter::getCount
//
// !INTERFACE:
int GridCellIter::getCount(
//
// !RETURN VALUE:
//    the number of nodes in this iterator
//
// !ARGUMENTS:
//   none  
 ){
//
// !DESCRIPTION:
// Return the number of nodes in this iterator. 
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc;
  int cnt, cntDE;
  int lBnd[ESMF_MAXDIM];
   int uBnd[ESMF_MAXDIM];

  // Loop through DEs count nodes
  cnt=0;
  for (int d=0; d<numDE; d++) {

    // Set Bounds of iteration on this proc
    this->getDEBnds(d,uBnd,lBnd);

    // For this DE get the number of nodes
    cntDE=1;
    for (int i=0; i<rank; i++) {
      cntDE *= (uBnd[i]-lBnd[i]+1);
    }

     // Add the size of this DE to the rest
    cnt +=cntDE;
  }

  // Output count
  return cnt;

}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridCellIter::getLocalID()"
//BOPI
// !IROUTINE:  getLocalID
//
// !INTERFACE:
int GridCellIter::getLocalID(
//
// !RETURN VALUE:
//  local id
//
// !ARGUMENTS:
//   none  
 ){
//
// !DESCRIPTION:
//   returns a local ID (an id unique on this processor) for an iteration location. 
//   Note that the range of local IDs is not necessarily continuous or contiguous
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc;
  int dePos;  
 
  // if done then leave
  if (done) return -1;

  // compute position in DE
  dePos=-lOff;
  for (int i=0; i<rank; i++) {
    dePos +=dimOff[i]*curInd[i];
  }

  // Add in DE number and output
  return dePos*numDE+curDE;
}
//-----------------------------------------------------------------------------


// This method must correspond to GridIter::setDEBnds(), so adjust accordingly
void precomputeCellNodeLIDInfo(Grid *grid, DistGrid *staggerDistgrid, int dimCount, int staggerloc, int localDE, const ESMC_GridConn *connL, const ESMC_GridConn *connU,  
                               int *dimOffCN, int *lOffCN) {
  int uBnd[ESMF_MAXDIM];
  int lBnd[ESMF_MAXDIM];
  int tmplOff;

  // Set Bounds of iteration on this proc
  grid->getDistExclusiveUBound(staggerDistgrid, localDE, uBnd);  
  grid->getDistExclusiveLBound(staggerDistgrid, localDE, lBnd);  
  // OLD grid->getExclusiveUBound(staggerloc, localDE, uBnd);  
  // OLD grid->getExclusiveLBound(staggerloc, localDE, lBnd);  

  // if cell iterator then expand bounds
  int localrc, rc;
  for (int i=0; i<dimCount; i++) {
    bool isLBnd=grid->isStaggerLBnd(staggerloc, localDE, i);
    bool isUBnd=grid->isStaggerUBnd(staggerloc, localDE, i);

    //// Expand to include all nodes touched by cells on this proc
    if (!isLBnd) lBnd[i]--;
    if (!isUBnd) uBnd[i]++;
  } 

  // Also expand for bipole, if center stagger
  if (staggerloc == 0) {
    for (int i=0; i<dimCount; i++) {
      bool isLBnd=grid->isStaggerLBnd(staggerloc, localDE, i);
      bool isUBnd=grid->isStaggerUBnd(staggerloc, localDE, i);

      //// Expand to include all nodes touched by cells on this proc
      if (isLBnd && (connL[i]==ESMC_GRIDCONN_BIPOLE)) lBnd[i]--;
      if (isUBnd && (connU[i]==ESMC_GRIDCONN_BIPOLE)) uBnd[i]++; 
    } 
  } 


  // Setup info for calculating the DE index tuple location quickly
  // Needs to be done after bounds are set
  int currOff=1;
  tmplOff=0;
  for (int i=0; i<dimCount; i++) {
    dimOffCN[i]=currOff;
    tmplOff +=currOff*lBnd[i];

    currOff *=(uBnd[i]-lBnd[i]+1);
  }  
  *lOffCN=tmplOff;
}

int getCellNodeLID(int *ind, int dimCount, int curDE, int numDE, int *dimOffCN, int lOffCN) {

 // compute position in DE
  int dePos=-lOffCN;
  for (int i=0; i<dimCount; i++) {
    dePos +=dimOffCN[i]*ind[i];
  }

  // Add in DE number and output
  return dePos*numDE+curDE;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::GridCellIter::getCornersCellNodeLocalID()"
//BOPI
// !IROUTINE: getCornersCellNodeLocalID()"
//
// !INTERFACE:
void GridCellIter::getCornersCellNodeLocalID(
//
// !RETURN VALUE:
//  none
//
// !ARGUMENTS:
                                              int *cnrCount,
                                             int *cnrList
 ){
//
// !DESCRIPTION:
//   Returns the corners of the current Grid Cell. The corners
// are identified by the localID that would be returned by
// a grid cell node iterator on that corner. 
//
// NOTE: cnrList must at least be allocated to size 2^grid rank
//       currently this only returns cubes, but may eventually
//       return other shapes as more complex grids can be created
//
//EOPI
//-----------------------------------------------------------------------------
  int cnrNum;
  int dimOffCN[ESMF_MAXDIM];
  int lOffCN;
  int cnrMap[4][8]={{0,0,0,0,0,0,0,0},
                    {0,1,0,0,0,0,0,0},
                    {0,1,3,2,0,0,0,0},
                    {0,1,3,2,4,5,7,6}};

  // if rank is bigger than supported exit
  // ADD THROW HERE
  if (rank >3) return;
  
  // Set number of corners
  cnrNum=0x1<<rank;
  
  // Set number of corners output 
  *cnrCount=cnrNum;

  // Precompute info for calculating local IDs
  precomputeCellNodeLIDInfo(grid, staggerDistgrid, rank, staggerloc, curDE, connL, connU, dimOffCN, &lOffCN);

  // Loop through setting corners
   for (int i=0; i<cnrNum; i++) {
    int ind[ESMF_MAXDIM];

    // generate index value for corner
    for (int j=0; j<rank; j++) {
      ind[j]=curInd[j];
      if (align[j] <0) { // center aligned with bottom of cell, so move upward
    if (i & (0x1<<j)) {
      ind[j]++;
    }        
      } else {  // center aligned with top of cell, so move downward
    if (!(i & (0x1<<j))) {
      ind[j]--;
    }        
      }
     }

    // compute Local IDs
    cnrList[cnrMap[rank][i]]=getCellNodeLID(ind, rank, curDE, numDE, dimOffCN, lOffCN);
  }

}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridIter::getArrayData()"
//BOPI
// !IROUTINE:  getArrayData
//
// !INTERFACE:
template <class TYPE>
void GridCellIter::setArrayData(
//
// !RETURN VALUE:
//  void
//
// !ARGUMENTS:
//   Data output
// 
                            Array *array,
                            TYPE data 
 ){
//
// !DESCRIPTION:
// Set data to a passed in Array
// TODO: Need to come up with a way to handle Arrays with more dimensions than the Grid
 //
//EOPI
//-----------------------------------------------------------------------------
  int localrc;
  LocalArray *localArray;
  
  // if done then leave
  if (done) return;

  // Check typekind
  if (array->getTypekind() != get_TypeKind_from_Type<TYPE>()) {
    int rc;
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
               "- requested type does not match type of data in Array ", ESMC_CONTEXT, &rc);
    throw rc;
  }
  
  //// Get LocalArray cooresponding to staggerloc, coord and localDE
  localArray=array->getLocalarrayList()[curDE];

  
  // Convert curInd to index used in localArray
  int arrayInd[ESMF_MAXDIM];
  for (int i=0; i<rank; i++) {
    arrayInd[i]=(curInd[i]-lBndInd[i])+exLBndInd[i];
  }

  //// Get pointer to LocalArray data
  localArray->setData(arrayInd, data);
   
}

// Add more types here if necessary
template void GridCellIter::setArrayData(Array *array, ESMC_R8 data);
template void GridCellIter::setArrayData(Array *array, ESMC_R4 data);
template void GridCellIter::setArrayData(Array *array, ESMC_I4 data);
//-----------------------------------------------------------------------------

 /* XMRKX */

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridCellIter::getArrayData()"
//BOPI
// !IROUTINE:  getArrayData
//
// !INTERFACE:
template <class TYPE>
void GridCellIter::getArrayData(
//
// !RETURN VALUE:
//  void
//
// !ARGUMENTS:
//   Data output
 // 
                            Array *array,
                            TYPE *data // (out) input array needs to be at
                                       // least of size grid dimCount    
 ){
//
// !DESCRIPTION:
// Get data from a passed in Array
// TODO: Need to come up with a way to handle Arrays with more dimensions than the Grid
// TODO: Need error checking!!!!!
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc;
  LocalArray *localArray;
  
  // if done then leave
  if (done) return;
  
  // Check typekind
  if (array->getTypekind() != get_TypeKind_from_Type<TYPE>()) {
    int rc;
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
                "- requested type does not match type of data in Array ", ESMC_CONTEXT, &rc);
    throw rc;
  }

  //// Get LocalArray cooresponding to staggerloc, coord and localDE
  localArray=array->getLocalarrayList()[curDE];
  
  // Convert curInd to index used in localArray
  int arrayInd[ESMF_MAXDIM];
  for (int i=0; i<rank; i++) {
    arrayInd[i]=(curInd[i]-lBndInd[i])+exLBndInd[i];
  }

  //// Get pointer to LocalArray data
  localArray->getDataInternal(arrayInd, data);
}

// Add more types here if necessary
template void GridCellIter::getArrayData(Array *array, ESMC_R8 *data);
template void GridCellIter::getArrayData(Array *array, ESMC_R4 *data);
template void GridCellIter::getArrayData(Array *array, ESMC_I4 *data);
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridCellIter::getItem()"
//BOPI
// !IROUTINE:  getItem
 //
// !INTERFACE:
template <class TYPE>
void GridCellIter::getItem(
//
// !RETURN VALUE:
//  void
//
// !ARGUMENTS:
//   Value output 
// 
                int item,     // item type
               TYPE *value // (out) input array needs to be at
                                       // least of the size of 1 item    
 ){
//
// !DESCRIPTION:
//  Returns the item value for an iteration location.
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc,rc;

  // if done then leave
  if (done) return;

  // get item data from center stagger (where the data for a cell lives)
   localrc=grid->getItemInternalConvert(0, item, curDE, curInd, value);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                    &rc)) throw rc;

}
// Add more types here if necessary
template void GridCellIter::getItem(int item, ESMC_R8 *data);
template void GridCellIter::getItem(int item, ESMC_R4 *data);
template void GridCellIter::getItem(int item, ESMC_I4 *data);
//-----------------------------------------------------------------------------




//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridCellIter::moveToLocalID()"
//BOPI
// !IROUTINE:  moveToLocalID
//
// !INTERFACE:
 GridCellIter *GridCellIter::moveToLocalID(
//
// !RETURN VALUE:
//    returns the grid iterator
//
// !ARGUMENTS:
//   
 int localID){
//
// !DESCRIPTION:
// Move to the position in the iteration list represented by local id
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc;
  int de,dePos,cnt;  

  // compute DE and the dePos
  de=localID%numDE;
  dePos=localID/numDE;

  // check for bad DE
  if (de > uBndDE) return this;

  // set DE
  curDE=de;

  // load DE bounds and other info
  this->setDEBnds(curDE);  

  // reset current index location using dePos 
   for (int i=0; i<rank-1; i++) {
    cnt=uBndInd[i]-lBndInd[i]+1;
    curInd[i] = dePos%cnt+lBndInd[i];
    dePos /=cnt;
  }
  curInd[rank-1]=dePos+lBndInd[rank-1];

  // since we're now not done set done to false
  done=false;

  // Add in DE number and output
  return this;

}
//-----------------------------------------------------------------------------


 

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::~GridCellIter()"
//BOPI
// !IROUTINE:  GridCellIter Destruct
//
// !INTERFACE:
GridCellIter::~GridCellIter(
//
// !RETURN VALUE:
//    void
//
// !ARGUMENTS:
//
 void  
) {
//
// !DESCRIPTION:

//
//EOPI
//-----------------------------------------------------------------------------

}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::matchCoordInternal()"
//BOPI
// !IROUTINE:  Grid::matchCoordInternal()"
 //
// !INTERFACE:
template <class TYPE>
bool Grid:: matchCoordInternal(
//
// !RETURN VALUE:
//   void
//
// !ARGUMENTS:
//
                 Grid *grid1,
                 Grid *grid2
                                 ){
 //
 // !DESCRIPTION:
//  This internal function returns true if all of the coords in grid1 and grid2 match. 
// NOTE: This function assumes that other items in the grids (e.g. dimCount) match. 
//
//EOPI
//-----------------------------------------------------------------------------

  // Check that coordinates match
  for (int s=0; s<grid1->staggerLocCount; s++) {

    // Make sure that both grids have coordinates
    // allocated for this staggerloc
    bool grid1HasCoords=grid1->hasCoordStaggerLoc(s);
    bool grid2HasCoords=grid2->hasCoordStaggerLoc(s);

    if (grid1HasCoords != grid2HasCoords) {
      return false;
    }

    // If no coords, then skip to next staggerloc
    if (!grid1HasCoords) continue;

    // Get number of DEs on the PET
    int localDECount=grid1->staggerDistgridList[s]->getDELayout()->getLocalDeCount();
    
    // Loop over DEs
    for (int lDE=0; lDE<localDECount; lDE++) {  
      int i[ESMF_MAXDIM];
      int lBnd[ESMF_MAXDIM]={0,0,0,0,0,0,0}; // Initialize loops ranges so that 
      int uBnd[ESMF_MAXDIM]={0,0,0,0,0,0,0}; // loops outside of dimCount only go once
      TYPE coord1[ESMF_MAXDIM];
       TYPE coord2[ESMF_MAXDIM];
      const int dimCount=grid1->dimCount;

      // Loop through coordinates
      // Set Bounds of iteration on this proc
      grid1->getExclusiveUBound(s, lDE, uBnd);  
      grid1->getExclusiveLBound(s, lDE, lBnd);        
 
      // Loop through all dimensions although
      // only valid ones will not be initialized 
      // to 0,0 (and those won't be used in getCoordInternal call)
      for (i[6]=lBnd[6]; i[6]<=uBnd[6]; i[6]++) {
      for (i[5]=lBnd[5]; i[5]<=uBnd[5]; i[5]++) {
      for (i[4]=lBnd[4]; i[4]<=uBnd[4]; i[4]++) {
      for (i[3]=lBnd[3]; i[3]<=uBnd[3]; i[3]++) {
      for (i[2]=lBnd[2]; i[2]<=uBnd[2]; i[2]++) {
        for (i[1]=lBnd[1]; i[1]<=uBnd[1]; i[1]++) {
      for (i[0]=lBnd[0]; i[0]<=uBnd[0]; i[0]++) {

    // Get Coordinates for Grid 1
    grid1->getCoordInternal(s,lDE, i, coord1);

    // Get Coordinates for Grid 2
    grid2->getCoordInternal(s,lDE, i, coord2);

    // Check if coordinates match
    for (int d=0; d< dimCount; d++) {
      if (coord1[d] != coord2[d]) {
        return false;
      }
    }

      } // 0
      } // 1
      } // 2
      } // 3
      } // 4
      } // 5
      } // 6
    } //lDE
  } // s

  return true;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::matchItemInternal()"
//BOPI
// !IROUTINE:  Grid::matchItemInternal()"
//
// !INTERFACE:
template <class TYPE>
bool Grid:: matchItemInternal(
//
// !RETURN VALUE:
//   void
//
// !ARGUMENTS:
//   
                              int staggerloc,
                  int item,
                   Grid *grid1,
                   Grid *grid2
                  ){
  //
// !DESCRIPTION:
//  This internal function returns true if all of the elements in item in grid1 and grid2 match. 
// NOTE: This function assumes that other items in the grids (e.g. dimCount) match. 
//
//EOPI
//-----------------------------------------------------------------------------


    // Get number of DEs on the PET
    int localDECount=grid1->staggerDistgridList[staggerloc]->getDELayout()->getLocalDeCount();
    
    // Loop over DEs
    for (int lDE=0; lDE<localDECount; lDE++) {  
      int i[ESMF_MAXDIM];
      int lBnd[ESMF_MAXDIM]={0,0,0,0,0,0,0}; // Initialize loops ranges so that 
      int uBnd[ESMF_MAXDIM]={0,0,0,0,0,0,0}; // loops outside of dimCount only go once
      TYPE iv1;
      TYPE iv2;
      const int dimCount=grid1->dimCount;

      // Loop through coordinates
      // Set Bounds of iteration on this proc
      grid1->getExclusiveUBound(staggerloc, lDE, uBnd);  
      grid1->getExclusiveLBound(staggerloc, lDE, lBnd);        

      // Loop through all dimensions although
      // only valid ones will not be initialized 
      // to 0,0 (and those won't be used in getCoordInternal call)
      for (i[6]=lBnd[6]; i[6]<=uBnd[6]; i[6]++) {
      for (i[5]=lBnd[5]; i[5]<=uBnd[5]; i[5]++) {
      for (i[4]=lBnd[4]; i[4]<=uBnd[4]; i[4]++) {
      for (i[3]=lBnd[3]; i[3]<=uBnd[3]; i[3]++) {
      for (i[2]=lBnd[2]; i[2]<=uBnd[2]; i[2]++) {
      for (i[1]=lBnd[1]; i[1]<=uBnd[1]; i[1]++) {
      for (i[0]=lBnd[0]; i[0]<=uBnd[0]; i[0]++) {

    // Get Item value for Grid 1
    grid1->getItemInternal(staggerloc,item,lDE, i, &iv1);

    // Get Item value for Grid 2
    grid2->getItemInternal(staggerloc,item,lDE, i, &iv2);

    // Check if item values match
    if (iv1 != iv2) {
      return false;
     }

      } // 0
      } // 1
      } // 2
      } // 3
      } // 4
      } // 5
      } // 6
    } //lDE

  return true;
}

 
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::match()"
//BOPI
// !IROUTINE:  ESMCI::Grid::match
//
// !INTERFACE:
bool Grid::match(
//
// !RETURN VALUE:
//    bool according to match
//
// !ARGUMENTS:
//
  Grid *grid1,                          // in
  Grid *grid2,                          // in
  int *rc                               // (out) return code
  ){
//
//
// !DESCRIPTION:
//    Determine if grid1 and grid2 match.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  // return with errors for NULL pointer
  if (grid1 == NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to Grid", ESMC_CONTEXT, rc);
     return false;
  }
  if (grid2 == NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to Grid", ESMC_CONTEXT, rc);
    return false;
  }
  
  // check if Grid pointers are identical
  if (grid1 == grid2){
    // pointers are identical -> nothing more to check
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return true;
  }
  

  // Don't look into Grid proto, if the grids are both empty then consider them to match
  // Reason: no public interface for partially setting a grid right now
  
  // Check status
  if (grid1->status != grid2-> status) {
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return false;
  }

  // if we're less then ready then leave, because this is all the valid items in Grid
  // (Note at this point grids status are equal)
  if (grid1->status < ESMC_GRIDSTATUS_SHAPE_READY) {
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
     return false;
  }


  // Check decomp type
  if (grid1->decompType != grid2->decompType) {
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return false;;
  }

  // Check typekind
  if (grid1->typekind != grid2->typekind) {
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return false;;
  }

  // Is the following still necessary???

  // Check distDimCount
   if (grid1->distDimCount != grid2->distDimCount) {
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return false;;
  }

  // Check distgridToGridMap
  for (int i=0; i<grid1->distDimCount; i++) {
    if (grid1->distgridToGridMap[i] != grid2->distgridToGridMap[i]) {
      if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
      return false;
    }
  }


  // NOTE: Don't check undistributed bounds because they aren't supported anymore

  // Check dimCount
  if (grid1->dimCount != grid2->dimCount) {
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return false;
  }

  // Check Grid min/max
  for (int i=0; i<grid1->dimCount; i++) {
    if (grid1->minIndex[i] != grid2->minIndex[i]) {
      if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
      return false;
    }

    if (grid1->maxIndex[i] != grid2->maxIndex[i]) {
      if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
      return false;
    }
  }


  // DON'T CHECK TOPOLOGY INFO, BECAUSE IT'S NOT SET YET. ALSO THE setSphere() 
  // WILL MESS THINGS UP
  //  ESMC_GridConn *connL;  // size of Grid rank
  //  ESMC_GridConn *connU;  // size of Grid rank


  // DON'T CHECK GRIDALIGN, GRIDEDGEWIDTH INFO, INSTEAD CHECK STAGGERLOC PARTICUALR VERSIONS
  // REASON: THESE ARE ONLY USED TO SET DEFAULTS, THE PARTICULAR STAGGER INFO IS WHAT IS ACTUALLY
   //         USED IN THE GRID
  //  int *gridEdgeLWidth; // size of grid dimCount
  //  int *gridEdgeUWidth; // size of grid dimCount
  //  int *gridAlign; // size of grid dimCount
 
  
  // Check Grid coordDimCount
  for (int i=0; i<grid1->dimCount; i++) {
    if (grid1->coordDimCount[i] != grid2->coordDimCount[i]) {
      if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
      return false;
    }
  }

  // Check Grid coordDimCount
  for (int i=0; i<grid1->dimCount; i++) {
    for (int j=0; j<grid1->dimCount; j++) {
      if (grid1->coordDimMap[i][j] != grid2->coordDimMap[i][j]) {
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return false;
      }
    }
  }

  // NOTE: SHOULD WE TRY TO MATCH THNGS THAT HAVE A DIFFERENT distgrid, but still match?????
  //       NO! Distgrids need to match, otherwise ASMM won't be the same

  // Index array for arbitrarily distributed grid
  // Note equality of decompTypes is checked above, so can assume both are same
  if (grid1->decompType == ESMC_GRID_ARBITRARY) {
    if (grid1->localArbIndexCount != grid2->localArbIndexCount) {
      if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
      return false;
    }


    if (grid1->arbDim != grid2->arbDim) {
      if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
      return false;
    }

    // Check localArbIndex
    for (int i=0; i<grid1->localArbIndexCount; i++) {
      for (int j=0; j<grid1->distDimCount; j++) {
    if (grid1->localArbIndex[i][j] != grid2->localArbIndex[i][j]) {
      if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
      return false;
    }
      }
    }
  }

   
  // Check staggerLocCount
  if (grid1->staggerLocCount != grid2->staggerLocCount) {
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return false;
  }

  // Don't check arrays right now because we might want to support different factorizations
  // Array ***coordArrayList; // size of coordArrayList = staggerLocCountxdimCount [staggerLoc][coord]


   // Check coordArrayList
  for (int i=0; i<grid1->staggerLocCount; i++) {
    for (int j=0; j<grid1->dimCount; j++) {
      if (grid1->coordArrayList[i][j] != ESMC_NULL_POINTER &&
      grid2->coordArrayList[i][j] != ESMC_NULL_POINTER) {
    bool arraymatch=Array::matchBool(grid1->coordArrayList[i][j],
      grid2->coordArrayList[i][j],&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) return false;
    if (!arraymatch) {
      if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
      return false;
    }
      }
    }
  }

  // Don't check this
  //  bool  **coordDidIAllocList;        // if true, I allocated this Array [staggerloc][coord]


  // Check staggerMemLBoundList
  for (int i=0; i<grid1->staggerLocCount; i++) {
    for (int j=0; j<grid1->dimCount; j++) {
      if (grid1->staggerMemLBoundList[i][j] != grid2->staggerMemLBoundList[i][j]) {
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return false;
      }
    }
  }


  // Check staggerMemLBoundList
  for (int i=0; i<grid1->staggerLocCount; i++) {
    for (int j=0; j<grid1->dimCount; j++) {
      if (grid1->staggerMemLBoundList[i][j] != grid2->staggerMemLBoundList[i][j]) {
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return false;
       }
    }
  }


  // Check staggerAlignList
  for (int i=0; i<grid1->staggerLocCount; i++) {
    for (int j=0; j<grid1->dimCount; j++) {
      if (grid1->staggerAlignList[i][j] != grid2->staggerAlignList[i][j]) {
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return false;
      }
    }
  }

  // Check staggerEdgeLWidthList
  for (int i=0; i<grid1->staggerLocCount; i++) {
    for (int j=0; j<grid1->dimCount; j++) {
      if (grid1->staggerEdgeLWidthList[i][j] != grid2->staggerEdgeLWidthList[i][j]) {
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return false;
      }
    }
  }

  // Check staggerEdgeUWidthList
   for (int i=0; i<grid1->staggerLocCount; i++) {
    for (int j=0; j<grid1->dimCount; j++) {
      if (grid1->staggerEdgeUWidthList[i][j] != grid2->staggerEdgeUWidthList[i][j]) {
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return false;
      }
    }
  }


  // Check itemArrayList
  for (int i=0; i<grid1->staggerLocCount; i++) {
    for (int j=0; j<ESMC_GRIDITEM_COUNT; j++) {
      if (grid1->itemArrayList[i][j] != ESMC_NULL_POINTER &&
      grid2->itemArrayList[i][j] != ESMC_NULL_POINTER) {
    bool arraymatch=Array::matchBool(grid1->itemArrayList[i][j],
      grid2->itemArrayList[i][j],&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc,
              ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) return false;
    if (!arraymatch) {
      if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
      return false;
    }
       }
    }
  }


  // Check if item data matches
  for (int i=0; i<grid1->staggerLocCount; i++) {
    for (int j=0; j<ESMC_GRIDITEM_COUNT; j++) {
      if (grid1->itemArrayList[i][j] != ESMC_NULL_POINTER &&
      grid2->itemArrayList[i][j] != ESMC_NULL_POINTER) {
    if (grid1->itemArrayList[i][j]->getTypekind() != 
            grid2->itemArrayList[i][j]->getTypekind()) {
      if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
      return false;
    }

    // Check coordinates
    bool itemMatch=false;
    switch(grid1->itemArrayList[i][j]->getTypekind()) {
    case ESMC_TYPEKIND_R4:
      itemMatch=matchItemInternal<ESMC_R4>(i,j,grid1,grid2);
      break;
    case ESMC_TYPEKIND_R8:
      itemMatch=matchItemInternal<ESMC_R8>(i,j,grid1,grid2);
      break;
    case ESMC_TYPEKIND_I4:
      itemMatch=matchItemInternal<ESMC_I4>(i,j,grid1,grid2);
      break;
    default:
      ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
        "- Grid doesn't currently support this data type", ESMC_CONTEXT, rc);
      return false;
    }
    
    // return coord match result
    if (!itemMatch) {
      if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
      return false;
    }
    
      }
     }
  }

  // Don't check this:
  // bool   **itemDidIAllocList; // holds item Arrays [staggerloc][GRIDITEM_COUNT]


  // These are implied by the coordDimMap and coordDimCount
#if 0 
  // map grid dim to distgrid dim and grid bounds dim
  bool *gridIsDist;  // size=dimCount [grid-dim]
  int *gridMapDim;   // size=dimCount [grid-dim]

  // map coord dim to distgrid dim and coord array bounds dim
  bool **coordIsDist; // size=dimCountxdimCount [coord][coord-dim]
  int **coordMapDim; // size=dimCountxdimCount [coord][coord-dim]
#endif


  // These should be implied by the distgrid
#if 0
  char *isDELBnd;
  char *isDEUBnd;
#endif
  

  // These shouldn't matter in match
#if 0
  bool destroyDistgrid;
  bool destroyDELayout;
#endif

  // Check indexflag matching
  if (grid1->indexflag != grid2->indexflag) {
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return false;
  }


  // Check main distgrid
  if (!DistGrid::match(grid1->distgrid, grid2->distgrid)) {
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return false;
  }


  // Check stagger distgrids
  for (int i=0; i<grid1->staggerLocCount; i++) {
    if (grid1->staggerDistgridList[i] != ESMC_NULL_POINTER &&
    grid2->staggerDistgridList[i] != ESMC_NULL_POINTER) {
      if (!DistGrid::match(grid1->staggerDistgridList[i], grid2->staggerDistgridList[i])) {
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return false;
      }
    }
  }


  // Check coordinates
  bool coordMatch=false;
  switch(grid1->typekind) {
  case ESMC_TYPEKIND_R4:
    coordMatch=matchCoordInternal<ESMC_R4>(grid1,grid2);
     break;
  case ESMC_TYPEKIND_R8:
    coordMatch=matchCoordInternal<ESMC_R8>(grid1,grid2);
    break;
  case ESMC_TYPEKIND_I4:
    coordMatch=matchCoordInternal<ESMC_I4>(grid1,grid2);
    break;
  default:
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
     "- Grid doesn't currently support this data type", ESMC_CONTEXT, rc);
    return false;
  }
  

  // return coord match result
  if (!coordMatch) {
      if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
      return false;
  }

  // return successfully indicating match
  if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
  return true;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::getCartCoordDimCount()"
int Grid::getCartCoordDimCount() {
  int localrc, rc;
  int cartDimCount;

  // call into coord sys method to calculate this
  localrc=ESMCI_CoordSys_CalcCartDim(coordSys, dimCount, &cartDimCount);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
       &rc)) throw rc;

  // return cart dim
  return cartDimCount;
}


//-----------------------------------------------------------------------------
// Check if the passed in connection is a monopole, if so return some info 
// If isLower then is at the minimum end of the dimension, else is at the upper end.
// PoleDimOut, periodicDimOut are 0-based
 bool check_monopole(int *conn, int dimCount, int *widthIndex, bool *isLowerOut, int *poleDimOut, 
                     int *periodicDimOut) {
   int poleDim;

  // Count negative orientations, and perhaps find pole dim
  int neg=0;
  for (int i=0; i<dimCount; i++) {
    if (conn[2+dimCount+i] == -(i+1)) {
      neg++;
      poleDim=i;
    }
  }

  // If don't have just one neg orientation, then not a monopole
  if (neg != 1) return false;

   // Check lower vs. upper
  bool isLower;
  if (conn[2+poleDim]==1) isLower=true;
  else if (conn[2+poleDim]==2*widthIndex[poleDim]+1) isLower=false;
  else return false; // positionVector not correct so not a pole
  
  // Make sure has periodic Dim positionVect
  int periodicDim=-1;
  for (int i=0; i<dimCount; i++) {
    if (conn[2+i] == widthIndex[i]/2) {
      periodicDim=i;
    }
  }
  if (periodicDim == -1) return false; // no periodic offset

  // Output
  *isLowerOut=isLower;
  *poleDimOut=poleDim;
  *periodicDimOut=periodicDim;
  return true;
 }


// Check if the passed in connection is a bipole, if so return some info 
// If isLower then is at the minimum end of the dimension, else is at the upper end.
// PoleDimOut, periodicDimOut are 0-based
 bool check_bipole(int *conn, int dimCount, int *widthIndex, bool *isLowerOut, int *poleDimOut, 
                   int *periodicDimOut) {
   int negDim[ESMF_MAXDIM];

  // Count negative orientations, and perhaps find pole dim
  int neg=0;
  for (int i=0; i<dimCount; i++) {
    if (conn[2+dimCount+i] == -(i+1)) {
      negDim[neg]=i;
      neg++;
    }
  }

  // If don't have two neg orientation, then not a monopole
  if (neg != 2) return false;

  // Find periodic dim 
  int poleDim=-1;
  int periodicDim=-1;
  if (conn[2+negDim[0]]==widthIndex[negDim[0]]+1) {
    periodicDim=negDim[0];
    poleDim=negDim[1];
  } else if (conn[2+negDim[1]]==widthIndex[negDim[1]]+1) {
    periodicDim=negDim[1];
    poleDim=negDim[0];
  } else return false;

  // Check lower vs. upper
  bool isLower;
  if (conn[2+poleDim]==1) isLower=true;
  else if (conn[2+poleDim]==2*widthIndex[poleDim]+1) isLower=false;
  else return false; // positionVector not correct so not a pole
  
  // Output
  *isLowerOut=isLower;
  *poleDimOut=poleDim;
  *periodicDimOut=periodicDim;
   return true;
 }




bool isPoleConn(int *conn, int dimCount, int const *minIndexPTile, int const *maxIndexPTile) {
  bool isLower;
  int poleDim, periodicDim;
  int widthIndex[ESMF_MAXDIM];

   // if same tile isn't involved then skip
   int tile=conn[0];
   if (tile != conn[1]) return false;

  // Get MinIndex
   int const *minIndex=minIndexPTile+(tile-1)*dimCount;

  // Get MaxIndex
   int const *maxIndex=maxIndexPTile+(tile-1)*dimCount;

   // Compute width
   for (int j=0; j<dimCount; j++) {
     widthIndex[j]=maxIndex[j]-minIndex[j]+1;
   }
   // check if its a monpole 
  if (check_monopole(conn, dimCount, widthIndex, 
                     &isLower, &poleDim, &periodicDim)) {
    return true;
  } else if (check_bipole(conn, dimCount, widthIndex, 
                          &isLower, &poleDim, &periodicDim)) {
    return true;
  } else return false;

  return false;
}

void _create_nopole_distgrid(DistGrid *distgrid, DistGrid **distgrid_nopole, int *rc) {
  int localrc;

  // Obviously no pole, so just copy
  if (distgrid->getConnectionCount() <1) {
    *distgrid_nopole=DistGrid::create(distgrid,
                                      (InterArray<int> *)NULL, (InterArray<int> *)NULL,
                                      (ESMC_IndexFlag *)NULL, (InterArray<int> *)NULL, false,
                                      NULL, NULL, true, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
       rc)) return; 
    return;
  }


  // NOTE: Right now with multi-tile grids we aren't taking out the poles, because we need to rewrite 
  //       the pole taking out methods to work with more tiles. This 
  //       probably isn't an issue because having a pole with multi-tiles seems uncommon. If you have a multi-tile
  //       grid with a pole and you are getting bowtie shaped elements, this is probably why. Ask Bob how to fix. 
  //       (If you take out the poles, then you need to add them back in later. See _add_poles_to_conn(), etc. 
  //       in getStaggerDistgrid())
  if (distgrid->getTileCount() > 1) {

    // Copy distgrid
    *distgrid_nopole=DistGrid::create(distgrid,
                                   (InterArray<int> *)NULL, (InterArray<int> *)NULL,
                                   (ESMC_IndexFlag *)NULL, (InterArray<int> *)NULL, false,
                                   NULL, NULL, true, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      rc)) return; 

    return;
  }



// Get some info
 int dimCount=distgrid->getDimCount();
 int const *minIndexPTile=distgrid->getMinIndexPDimPTile();
 int const *maxIndexPTile=distgrid->getMaxIndexPDimPTile();

 
 // Get connection info from distgrid
 int connCount=distgrid->getConnectionCount();
 int * const* connList=distgrid->getConnectionList();
 
  // Loop through counting non-pole connections
 int newConnCount=0;  
  for (int i=0; i<connCount; i++) {
   if (!isPoleConn(connList[i],dimCount,minIndexPTile,maxIndexPTile)) newConnCount++;
 }

 // Allocate list without poles
 int connSize=2+2*dimCount;
 int *newConnList=new int[newConnCount*connSize];

#if 0
 printf("connCount=%d\n",connCount);

 for (int i=0; i<connCount; i++) {
   for (int j=0; j<connSize; j++) {
     printf("%d ",connList[i][j]);
   }
   printf("\n");
 }

   printf("\n");
   printf("\n");
#endif


 // Fill in
 int k=0;
 for (int i=0; i<connCount; i++) {
   if (!isPoleConn(connList[i],dimCount,minIndexPTile,maxIndexPTile)) {
     for (int j=0; j<connSize; j++) {
       newConnList[k]=connList[i][j];
       k++;
     }
   }
 }

 // Construct interface int
 int extent[2];
 
  extent[0]=connSize;
  extent[1]=newConnCount;
  InterArray<int> *newConnListII=new InterArray<int>(newConnList,2,extent);

 *distgrid_nopole=DistGrid::create(distgrid,
                                   (InterArray<int> *)NULL, (InterArray<int> *)NULL,
                                   (ESMC_IndexFlag *)NULL, newConnListII, false,
                                   NULL, NULL, true, &localrc);
 if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
     rc)) return; 

#if 0
 int tstconnCount=(*distgrid_nopole)->getConnectionCount();
 int * const* tstconnList=(*distgrid_nopole)->getConnectionList();

   printf("TST:\n");
 for (int i=0; i<tstconnCount; i++) {
   for (int j=0; j<connSize; j++) {
     printf("%d ",tstconnList[i][j]);
   }
   printf("\n");
 }
#endif

 // Free memory
 delete newConnListII;
  delete [] newConnList;
}

void _translate_distgrid_conn(DistGrid *distgrid, 
                              ESMC_GridConn *connL, ESMC_GridConn *connU, int *rc) {
  int localrc;
  int widthIndex[ESMF_MAXDIM];
  bool isLower;
  int  poleDim, periodicDim;

  // NOTE: Right now with multi-tile grids we aren't taking out the poles, because we need to rewrite 
  //       the pole taking out methods to work with more tiles (e.g.  _create_nopole_distgrid()). This 
  //       probably isn't an issue because having a pole with multi-tiles seems uncommon. If you have a multi-tile
  //       grid with a pole and you are getting bowtie shaped elements, this is probably why. Ask Bob how to fix. 
  //       (If you take out the poles, then you need to add them back in later. See _add_poles_to_conn(), etc. in getStaggerDistgrid())
  if (distgrid->getTileCount() > 1) {
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully 
    return;
  }

  // Get some info
 int dimCount=distgrid->getDimCount();
 int const *minIndexPTile=distgrid->getMinIndexPDimPTile();
 int const *maxIndexPTile=distgrid->getMaxIndexPDimPTile();
 
 // Get connection info from distgrid
 int connCount=distgrid->getConnectionCount();
 int * const* connList=distgrid->getConnectionList();
 int connSize=2+2*dimCount;
 

  // Loop through  translating connections
 for (int i=0; i<connCount; i++) {

   // if same tile isn't involved then skip
   int tile=connList[i][0];
   if (tile != connList[i][1]) continue;

  // Get MinIndex
   int const *minIndex=minIndexPTile+(tile-1)*dimCount;

  // Get MaxIndex
   int const *maxIndex=maxIndexPTile+(tile-1)*dimCount;

   // Compute width
   for (int j=0; j<dimCount; j++) {
     widthIndex[j]=maxIndex[j]-minIndex[j]+1;
   }

   // check if its a monpole 
   if (check_monopole(connList[i], dimCount, widthIndex, 
                      &isLower, &poleDim, &periodicDim)) {
     if (isLower) connL[poleDim]=ESMC_GRIDCONN_POLE;
     else connU[poleDim]=ESMC_GRIDCONN_POLE;
     //     printf("MONOPOLE poleDim=%d isLower=%d periodicDim=%d \n",poleDim, isLower, periodicDim);

    } else if (check_bipole(connList[i], dimCount, widthIndex, 
                            &isLower, &poleDim, &periodicDim)) {
     if (isLower) connL[poleDim]=ESMC_GRIDCONN_BIPOLE;
     else connU[poleDim]=ESMC_GRIDCONN_BIPOLE;
     //printf("BIPOLE poleDim=%d isLower=%d periodicDim=%d \n",poleDim, isLower, periodicDim);
   }
 }

   if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully 
}





// Output is in InterArray, calling subroutine needs to deallacate list
void _add_poles_to_conn(DistGrid *distgrid, 
                        int *lwidth, int *uwidth, ESMC_GridConn *connL, ESMC_GridConn *connU, InterArray<int> **connListOut, int *rc) {
  int localrc;
  int widthIndex[ESMF_MAXDIM];
  bool isLower;

// Get some info
 int dimCount=distgrid->getDimCount();
 int const *minIndexPTile=distgrid->getMinIndexPDimPTile();
 int const *maxIndexPTile=distgrid->getMaxIndexPDimPTile();
 
 // Get connection info from distgrid
 int connCount=distgrid->getConnectionCount();
 int * const* connList=distgrid->getConnectionList();
 int connSize=2+2*dimCount;
 
 // Get min,max index 
 // TODO: this is assuming 1 TILE FIX THIS
 int tile=1;
 int const *minIndex=minIndexPTile+(tile-1)*dimCount;
 int const *maxIndex=maxIndexPTile+(tile-1)*dimCount;

 // Compute width
 for (int i=0; i<dimCount; i++) {
   widthIndex[i]=(maxIndex[i]+uwidth[i])-(minIndex[i]-lwidth[i])+1;
 }

 // Count number of poles
 int num_poles=0;
 for (int i=0; i<dimCount; i++) {
   if (connL[i]==ESMC_GRIDCONN_POLE) num_poles++;
   if (connU[i]==ESMC_GRIDCONN_POLE) num_poles++;
   if (connL[i]==ESMC_GRIDCONN_BIPOLE) num_poles++;
   if (connU[i]==ESMC_GRIDCONN_BIPOLE) num_poles++;
 }

 // Allocate list with poles back in
 int newConnCount=connCount+num_poles;
 int *newConnList=NULL;
 if ((newConnCount*connSize)>0) newConnList=new int[newConnCount*connSize];

  // Copy old connections into list
 int newPos=0;
 for (int i=0; i<connCount; i++) {
   for(int j=0; j<connSize; j++) {
     newConnList[i*connSize+j]=connList[i][j];
   }
   newPos++;
 }

 // PeriodicDim
 int periodicDim=0;

 // Put in Lower poles
 for (int i=0; i<dimCount; i++) {
    if (connL[i]==ESMC_GRIDCONN_POLE) {
     newConnList[newPos*connSize+0]=1;
     newConnList[newPos*connSize+1]=1;
     for(int j=0; j<dimCount; j++) {
       newConnList[newPos*connSize+2+j]=0;
     }
     newConnList[newPos*connSize+2+periodicDim]=widthIndex[periodicDim]/2;
     newConnList[newPos*connSize+2+i]=1;
     for(int j=0; j<dimCount; j++) {
       newConnList[newPos*connSize+2+dimCount+j]=j+1;
     }
     newConnList[newPos*connSize+2+dimCount+i]=-newConnList[newPos*connSize+2+dimCount+i];
     newPos++;
   } else if (connL[i]==ESMC_GRIDCONN_BIPOLE) {
     newConnList[newPos*connSize+0]=1;
     newConnList[newPos*connSize+1]=1;
     for(int j=0; j<dimCount; j++) {
       newConnList[newPos*connSize+2+j]=0;
     }
     newConnList[newPos*connSize+2+periodicDim]=widthIndex[periodicDim]+1;
     newConnList[newPos*connSize+2+i]=1;
     for(int j=0; j<dimCount; j++) {
       newConnList[newPos*connSize+2+dimCount+j]=j+1;
     }
     newConnList[newPos*connSize+2+dimCount+i]=-newConnList[newPos*connSize+2+dimCount+i];
     newConnList[newPos*connSize+2+dimCount+periodicDim]=-newConnList[newPos*connSize+2+dimCount+periodicDim];
     newPos++;
   }
 }

 // Put in upper poles
 for (int i=0; i<dimCount; i++) {
   if (connU[i]==ESMC_GRIDCONN_POLE) {
     newConnList[newPos*connSize+0]=1;
     newConnList[newPos*connSize+1]=1;
     for(int j=0; j<dimCount; j++) {
       newConnList[newPos*connSize+2+j]=0;
     }
     newConnList[newPos*connSize+2+periodicDim]=widthIndex[periodicDim]/2;
     newConnList[newPos*connSize+2+i]=2*widthIndex[i]+1;
     for(int j=0; j<dimCount; j++) {
       newConnList[newPos*connSize+2+dimCount+j]=j+1;
     }
     newConnList[newPos*connSize+2+dimCount+i]=-newConnList[newPos*connSize+2+dimCount+i];
     newPos++;
   } else if (connU[i]==ESMC_GRIDCONN_BIPOLE) {
     newConnList[newPos*connSize+0]=1;
     newConnList[newPos*connSize+1]=1;
     for(int j=0; j<dimCount; j++) {
       newConnList[newPos*connSize+2+j]=0;
     }
     newConnList[newPos*connSize+2+periodicDim]=widthIndex[periodicDim]+1;
     newConnList[newPos*connSize+2+i]=2*widthIndex[i]+1;
     for(int j=0; j<dimCount; j++) {
       newConnList[newPos*connSize+2+dimCount+j]=j+1;
     }
     newConnList[newPos*connSize+2+dimCount+i]=-newConnList[newPos*connSize+2+dimCount+i];
     newConnList[newPos*connSize+2+dimCount+periodicDim]=-newConnList[newPos*connSize+2+dimCount+periodicDim];
     newPos++;
   }
 }


 #if 0
 printf("CONNECTIONS\n");
 int k=0;
 for (int i=0; i<newConnCount; i++) {
   for (int j=0; j<connSize; j++) {
     printf("%d ",newConnList[k]);
     k++;
   }
   printf("\n");
 }
#endif

 // Output connection info
 if (newConnCount >0) {
   int extent[2];
   extent[0]=connSize;
   extent[1]=newConnCount;
   *connListOut=new InterArray<int>(newConnList, 2, extent);
 } else {
   *connListOut=NULL;
 }
  if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully 
}




//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::getDistExclusiveUBound()"
//BOPI
// !IROUTINE:  Grid::getDistExclusiveUBound()"
//
// !INTERFACE:
int Grid::getDistExclusiveUBound(
//
// !RETURN VALUE:
//   return code
//
// !ARGUMENTS:
//
                                 DistGrid *distgridArg, 
                                 int localDEArg,     // (in)
                                 int *uBndArg      // (out) needs to be of size > distDimCount
  ){
//
// !DESCRIPTION:
//  The exclusive upper bound for this localde
// TODO: eventually this should return all the grid bounds, not just
//       the distributed ones.
//
//EOPI
//-----------------------------------------------------------------------------
  int rc,localrc;

  // initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  
  // Check status
   if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Grid not fully created", ESMC_CONTEXT, &rc);
    return rc;
  }


  // Ensure localDEArg isn't out of range for this PET
  if ((localDEArg < 0) || (localDEArg >=distgridArg->getDELayout()->getLocalDeCount())) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
          "- localDE outside range on this processor", ESMC_CONTEXT, &rc);
        return rc;
  }

  // Get some useful information
  const int *localDeToDeMap = distgridArg->getDELayout()->getLocalDeToDeMap();
  const int *indexCountPDimPDe = distgridArg->getIndexCountPDimPDe();

  // Get the Global DE from the local DE
  int de = localDeToDeMap[localDEArg];

  // exlc. region for each DE ends at indexCountPDimPDe of the associated
  // DistgridArg
    for (int i=0; i<distDimCount; i++)
      uBndArg[i]=indexCountPDimPDe[de*distDimCount+i];

  // Set upper bound based on indexflag
  if (indexflag==ESMC_INDEX_GLOBAL) {

      for (int i=0; i<distDimCount; i++){

        // obtain indexList for this DE and dim
        const int *indexList =
          distgridArg->getIndexListPDimPLocalDe(localDEArg, i+1, &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
           &rc)) return rc;

        // make sure is contiguous         
        const int contig=distgridArg->getContigFlagPDimPDe(de, i+1, &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc,
                              ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
        if (!contig) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
                     "- doesn't handle non-contiguous DEs yet ", ESMC_CONTEXT, &rc);
          return rc;
        }

        // shift bounds of exclusive region to match indexList[0]
        uBndArg[i] += indexList[0] - 1;
      } // i
  }

  // tell the calling subroutine that we've had a successful outcome
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::getDistExclusiveLBound()"
//BOPI
 // !IROUTINE:  Grid::getDistExclusiveLBound()"
//
// !INTERFACE:
int Grid::getDistExclusiveLBound(
//
// !RETURN VALUE:
//   return code
//
// !ARGUMENTS:
//
                                 DistGrid *distgridArg, 
                                 int localDEArg,     // (in)
                                 int *lBndArg      // (out) needs to be of size > distDimCount
  ){
//
// !DESCRIPTION:
//  The exclusive lower bound for this localde.
// TODO: eventually this should return all the grid bounds, not just
//       the distributed ones.
//
//EOPI
//-----------------------------------------------------------------------------
  int rc,localrc;

  // initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  
  // Check status
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Grid not fully created", ESMC_CONTEXT, &rc);
    return rc;
  }


  // Ensure localDEArg isn't out of range for this PET
  if ((localDEArg < 0) || (localDEArg >=distgridArg->getDELayout()->getLocalDeCount())) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
          "- localDE outside range on this processor", ESMC_CONTEXT, &rc);
        return rc;
  }

  // Set lower bound based on indexflag
  if ((indexflag==ESMC_INDEX_DELOCAL) || (indexflag==ESMC_INDEX_USER)) {
    for (int i=0; i<distDimCount; i++)
      lBndArg[i] = 1; // excl. region starts at (1,1,1...) 
  } else {
    // Get some useful information
    const int *localDeToDeMap = distgridArg->getDELayout()->getLocalDeToDeMap();

    // Get the Global DE from the local DE
    int de = localDeToDeMap[localDEArg];

    // Set Bound based on distgridArg info
    for (int i=0; i<distDimCount; i++){
        
      // obtain indexList for this DE and dim
      const int *indexList =
        distgridArg->getIndexListPDimPLocalDe(localDEArg, i+1, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &rc)) return rc;
      
      // make sure this dimension is contiguous         
       const int contig=distgridArg->getContigFlagPDimPDe(de, i+1, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc,
                             ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
      if (!contig) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
          "- doesn't handle non-contiguous DEs yet ",ESMC_CONTEXT, &rc);
        return rc;
      }
      
      // Set lower bounds of exclusive region to match indexList[0]
      lBndArg[i] = indexList[0];
    } // i
  }
  
  // tell the calling subroutine that we've had a successful outcome
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------



} // END ESMCI name space
//-----------------------------------------------------------------------------

