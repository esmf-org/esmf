// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2014, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_PointList_F.C"
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "ESMCI_Macros.h"
#include "ESMCI_PointList.h"
#include "ESMCI_Grid.h"
#include "ESMCI_GridToMesh.h"
#include "Mesh/include/ESMCI_Mesh.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_DELayout.h"
#include "ESMCI_LogErr.h"
#include "Mesh/include/ESMCI_MeshCap.h"
//------------------------------------------------------------------------------
//BOPI
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt PointList} class functions.
//
//EOPI
//------------------------------------------------------------------------------


// the interface subroutine names MUST be in lower case
extern "C" {

  void FTN_X(c_esmc_pointlistcreatefrmgrid)(ESMCI::Grid **gptr, ESMC_StaggerLoc *staggerLoc, ESMCI::InterArray<int> *maskValuesArg, ESMCI::PointList **plptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_pointlistcreatefrmgrid()"


    try {

      // Initialize return code; assume routine not implemented
      if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
      int localrc = ESMC_RC_NOT_IMPL;

      // call into C++
      ESMCI::GridToPointList(**gptr, *staggerLoc, maskValuesArg, plptr, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                        ESMC_NOT_PRESENT_FILTER(rc))) throw localrc;
    } catch (std::exception &x) {
      // catch Grid exception return code
      if (x.what()) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                      x.what(), ESMC_CONTEXT,rc);
      } else {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                      "UNKNOWN", ESMC_CONTEXT,rc);
      }
      return;
    } catch(int localrc) {
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,rc);
      return;
    } catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                    "- Caught unknown exception", ESMC_CONTEXT, rc);
      return;
    }

    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_pointlistcreatefrmmesh)(ESMCI::MeshCap **mptr, ESMC_MeshLoc_Flag *meshLoc, ESMCI::InterArray<int> *maskValuesArg, ESMCI::PointList **plptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_pointlistcreatefrmmesh()"

    try {
      // Initialize return code; assume routine not implemented
      if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
      int localrc = ESMC_RC_NOT_IMPL;

      // call into C++
      (*mptr)->MeshCap_to_PointList(*meshLoc, maskValuesArg, plptr, &localrc);
      //      *plptr = (*mptr)->MeshToPointList(*meshLoc, maskValuesArg, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                        ESMC_NOT_PRESENT_FILTER(rc))) throw localrc;

    } catch (std::exception &x) {
      // catch Mesh exception return code
      if (x.what()) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                      x.what(), ESMC_CONTEXT,rc);
      } else {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                      "UNKNOWN", ESMC_CONTEXT,rc);
      }
      return;
    } catch(int localrc) {
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,rc);
      return;
    } catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                    "- Caught unknown exception", ESMC_CONTEXT, rc);
      return;
    }

    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }


  void FTN_X(c_esmc_pointlistcreatefrminput)(int *maxpts, int *numdims,
                                             ESMCI::PointList **plptr,
                                             int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_pointlistcreatefrminput()"

    try {
      if (*maxpts < 0) {
        int localrc;
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          "- size of Point List object must be greater than zero",
          ESMC_CONTEXT, &localrc)) throw localrc;
      }
      if (*numdims < 1) {
        int localrc;
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          "- number of dimensions for points in Point List must be greater than zero",
          ESMC_CONTEXT, &localrc)) throw localrc;
      }

      int localrc = ESMC_RC_NOT_IMPL;

      *plptr = new ESMCI::PointList(*maxpts,*numdims);
    } catch(std::exception &x) {
      // catch Mesh exception return code
      if (x.what()) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                      x.what(), ESMC_CONTEXT,rc);
      } else {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                      "UNKNOWN", ESMC_CONTEXT,rc);
      }

      return;
    } catch(int localrc) {

      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,rc);
      return;

    } catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                    "- Caught unknown exception", ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_pointlistdestroy)(ESMCI::PointList **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_pointlistdestroy()"

    try {
      delete *ptr;
    } catch (std::exception &x) {
      if (x.what()) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                      x.what(), ESMC_CONTEXT,rc);
      } else {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                      "UNKNOWN", ESMC_CONTEXT,rc);
      }
      return;
    } catch(int localrc) {
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,rc);
      return;
    } catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                    "- Caught unknown exception", ESMC_CONTEXT, rc);
      return;
    }

    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_pointlistgetnumpts)(ESMCI::PointList **ptr, int *numpts,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_pointlistgetnumpts()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // call into C++
    *numpts = (*ptr)->get_curr_num_pts();
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_pointlistgetmaxpts)(ESMCI::PointList **ptr, int *maxpts,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_pointlistgetmaxpts()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // call into C++
    *maxpts = (*ptr)->get_max_num_pts();
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_pointlistgetdims)(ESMCI::PointList **ptr, int *dims,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_pointlistgetdims()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // call into C++
    *dims = (*ptr)->get_coord_dim();
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_pointlistgetid)(ESMCI::PointList **ptr, int *loc, int *id,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_pointlistgetid()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // call into C++
    *id = (*ptr)->get_id(*loc);
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_pointlistgetcoords)(ESMCI::PointList **ptr, int *loc, double *_coords,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_pointlistgetcoords()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // call into C++
    (*ptr)->get_coord(*loc, _coords);

    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_pointlistadd)(ESMCI::PointList **ptr, int *id, double *_coords,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_pointlistadd()"

    try {

      // Initialize return code; assume routine not implemented
      if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
      int localrc = ESMC_RC_NOT_IMPL;

      // call into C++
      localrc = (*ptr)->add(*id,_coords);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                        ESMC_NOT_PRESENT_FILTER(rc))) throw localrc;
    } catch (std::exception &x) {
      // catch Grid exception return code
      if (x.what()) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                      x.what(), ESMC_CONTEXT,rc);
      } else {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                      "UNKNOWN", ESMC_CONTEXT,rc);
      }
      return;
    } catch(int localrc) {
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,rc);
      return;
    } catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                    "- Caught unknown exception", ESMC_CONTEXT, rc);
      return;
    }

    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_pointlistprint)(ESMCI::PointList **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_pointlistprint()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // call into C++
    localrc = (*ptr)->diagprint();
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;

    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_pointlistwritevtk)(ESMCI::PointList **ptr, char *filename, int *rc, ESMCI_FortranStrLenArg nlen){

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_pointlistwritevtk()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    // Convert fortran filename to c
    char *c_filename=ESMC_F90toCstring(filename,nlen);

    // call into C++
    localrc = (*ptr)->WriteVTK(c_filename);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;

    // Delete c filename
    delete [] c_filename;

    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_pointlistsort)(ESMCI::PointList **ptr, int *rc){

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_pointlistsort()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    // call into C++
    localrc = (*ptr)->sort_by_id();
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;

    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_pointlistsph2cartcoord)(ESMC_CoordSys_Flag *coordSys, int *in_dim,
                                           double *in_coord, double *cart_coord, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_pointlistsph2cartcoord()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    // call into C++
    //    localrc = (*ptr)->sort_by_id();
    //if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    //  ESMC_NOT_PRESENT_FILTER(rc))) return;

    ESMCI_CoordSys_ConvertToCart(*coordSys,*in_dim,in_coord,cart_coord);

    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_pointlistcalccartdim)(ESMC_CoordSys_Flag *coordSys, int *in_dim,
                                          int *cart_dim, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_pointlistcalccartdim()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    // call into C++
    //    localrc = (*ptr)->sort_by_id();
    //if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    //  ESMC_NOT_PRESENT_FILTER(rc))) return;

    ESMCI_CoordSys_CalcCartDim(*coordSys,*in_dim,cart_dim);

    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }


};


