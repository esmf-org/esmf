// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2022, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC interface routines

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
//
//-----------------------------------------------------------------------------
//
// insert any higher level, 3rd party or system includes here

#include <cstring>

using namespace std;

#include "ESMC_Macros.h"
#include "ESMCI_LogErr.h"
#include "ESMC_Util.h"
#include "ESMCI_Array.h"
#include "Mesh/include/ESMCI_Mesh.h"
#include "Mesh/include/Legacy/ESMCI_Exception.h"
#include "Mesh/include/ESMCI_XGridUtil.h"
#include "Mesh/include/Regridding/ESMCI_MeshRegrid.h"
#include "Mesh/include/Legacy/ESMCI_MeshMerge.h"
#include "Mesh/include/Regridding/ESMCI_Regrid_Helper.h"
#include "Mesh/include/ESMCI_Mesh_XGrid_Glue.h"
#include "ESMCI_Grid.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version =
             "$Id$";
//-----------------------------------------------------------------------------

using namespace ESMCI;

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the XGrid routines
//
//

// xgrid regrid create method tailored for XGrid
void ESMCI_xgridregrid_create(Mesh **meshsrcpp, Mesh **meshdstpp,
                   Mesh **mesh,
                   int *compute_midmesh,
                   int *regridMethod,
                   int *unmappedaction,
                   ESMC_CoordSys_Flag *coordSys,
                   int *nentries, ESMCI::TempWeights **tweights,
                   int*rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_xgridregrid_create()"
  Trace __trace(" FTN_X(c_esmc_xgridregrid_create)");
  
  Mesh &srcmesh = **meshsrcpp;
  Mesh &dstmesh = **meshdstpp;
  
  // Old Regrid conserve turned off for now
  int regridConserve=ESMC_REGRID_CONSERVE_OFF;

  try {

    // Get source mesh dimensions
    int pdim=srcmesh.parametric_dim();
   int sdim=srcmesh.spatial_dim();
    int orig_sdim=srcmesh.orig_spatial_dim;

    // Check that meshes dimensions match
    if ((pdim != dstmesh.parametric_dim()) ||
        (sdim != dstmesh.spatial_dim())    ||
        (orig_sdim != dstmesh.orig_spatial_dim)) {
      // printf("spdim=%d ssdim=%d dpdim=%d dsdim=%d\n",pdim,sdim,dstmesh.parametric_dim(),dstmesh.spatial_dim());
      Throw() <<"Source and destination dimensions must match when creating an XGrid";
    }

    // Create Mesh and set info
    *mesh=0;
    if(*compute_midmesh) {
      // Create mesh
      *mesh = new Mesh();

      // Set dimensions and coord_sys
      (*mesh)->set_parametric_dimension(pdim);
      (*mesh)->set_spatial_dimension(sdim);
      (*mesh)->orig_spatial_dim=orig_sdim;
      (*mesh)->coordsys=*coordSys;
    }
    
    // Weights matrix
    IWeights wts;

    if(!online_regrid_xgrid(srcmesh, dstmesh, *mesh, wts, &regridConserve, regridMethod,
                      unmappedaction))
      Throw() << "Online regridding error" << std::endl;
    
    // Firstly, the index list
    std::pair<UInt,UInt> iisize = wts.count_matrix_entries();
    int num_entries = iisize.first;
    int *iientries = new int[2*iisize.first];
    int larg[2] = {2, static_cast<int>(iisize.first)};
    // Gather the list
    ESMCI::InterArray<int> ii(iientries, 2, larg);
    ESMCI::InterArray<int> *iiptr = &ii;
    
    double *factors = new double[iisize.first];

    // Translate weights to sparse matrix representatio
    UInt i = 0;
    WMat::WeightMap::iterator wi = wts.begin_row(), we = wts.end_row();
    for (; wi != we; ++wi) {
      const WMat::Entry &w = wi->first;

      std::vector<WMat::Entry> &wcol = wi->second;

      // Construct factor index list
      for (UInt j = 0; j < wcol.size(); ++j) {
        UInt twoi = 2*i;
        const WMat::Entry &wc = wcol[j];

        // Construct factor list entry
        iientries[twoi+1] = w.id;  iientries[twoi] = wc.id;
        factors[i] = wc.value;

        i++;
      } // for j
    } // for wi

    *nentries = num_entries;
    // Clean up.  If has_iw, then we will use the arrays to
    // fill out the users pointers.  These will be deleted following a copy.
    // Save off the weights so the F90 caller can allocate arrays and
    // copy the values.
    *tweights = new ESMCI::TempWeights;
    (*tweights)->nentries = num_entries;
    (*tweights)->factors = factors;
    (*tweights)->iientries = iientries;
  } catch(std::exception &x) {
    // catch Mesh exception return code
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                          x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                          "UNKNOWN", ESMC_CONTEXT, rc);
    }

    return;
  } catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }
  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}

// mesh merge
void ESMCI_meshmerge(Mesh **srcmeshpp, Mesh **dstmeshpp,
                Mesh **meshpp,
                int*rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_meshmerge()"
  Trace __trace(" FTN_X(meshmerge) ");

  Mesh &srcmesh = **srcmeshpp;
  Mesh &dstmesh = **dstmeshpp;

  try {
    MeshMerge(srcmesh, dstmesh, meshpp);

  } catch(std::exception &x) {
    // catch Mesh exception return code
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                          x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                          "UNKNOWN", ESMC_CONTEXT, rc);
    }

    return;
  } catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }
  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}

// This call is specifically for calculating 1st order conservative weights from 
// a side mesh to it's XGrid. It should only be called inside ESMF_XGridCreate() where
// the mesh side information has been correctly set. 
void ESMCI_Mesh_XGrid_calc_wgts_from_side_mesh(Mesh *src_side_mesh, Mesh *dst_xgrid_mesh,
                                          int *nentries, ESMCI::TempWeights **tweights,
                                          int*rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_Mesh_XGrid_calc_wgts_from_side_mesh()"


  try {

    // Weights matrix
    IWeights wts;

    // Calc the weights
    calc_wgts_from_side_mesh_to_xgrid(src_side_mesh, dst_xgrid_mesh, wts);

    // Translate weights to sparse matrix representation used by code above this
    std::pair<UInt,UInt> iisize = wts.count_matrix_entries();
    int num_entries = iisize.first;
    int *iientries = new int[2*iisize.first];
    int larg[2] = {2, static_cast<int>(iisize.first)};
    // Gather the list
    ESMCI::InterArray<int> ii(iientries, 2, larg);
    ESMCI::InterArray<int> *iiptr = &ii;

    double *factors = new double[iisize.first];

    UInt i = 0;
    WMat::WeightMap::iterator wi = wts.begin_row(), we = wts.end_row();
    for (; wi != we; ++wi) {
      const WMat::Entry &w = wi->first;

      std::vector<WMat::Entry> &wcol = wi->second;

      // Construct factor index list
      for (UInt j = 0; j < wcol.size(); ++j) {
        UInt twoi = 2*i;
        const WMat::Entry &wc = wcol[j];

        // Construct factor list entry
        iientries[twoi+1] = w.id;  iientries[twoi] = wc.id;
        factors[i] = wc.value;

        i++;
      } // for j
    } // for wi

    // If meshes are split, then translate indices and factorss
    if (src_side_mesh->is_split) translate_split_src_elems_in_wts(src_side_mesh, num_entries, iientries);
    if (dst_xgrid_mesh->is_split) translate_split_dst_elems_in_wts(dst_xgrid_mesh, num_entries, iientries, factors);
    
    *nentries = num_entries;
    // Clean up.  If has_iw, then we will use the arrays to
    // fill out the users pointers.  These will be deleted following a copy.
    // Save off the weights so the F90 caller can allocate arrays and
    // copy the values.
    *tweights = new ESMCI::TempWeights;
    (*tweights)->nentries = num_entries;
    (*tweights)->factors = factors;
    (*tweights)->iientries = iientries;


  } catch(std::exception &x) {
    // catch Mesh exception return code
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                          x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                          "UNKNOWN", ESMC_CONTEXT, rc);
    }

    return;
  } catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }
  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}

// This call is specifically for calculating 1st order conservative weights from 
// an XGrid to one of it's side meshes. It should only be called inside ESMF_XGridCreate() where
// the mesh side information has been correctly set. 
void ESMCI_Mesh_XGrid_calc_wgts_to_side_mesh(Mesh *src_xgrid_mesh, Mesh *dst_side_mesh,
                                             int *nentries, ESMCI::TempWeights **tweights,
                                             int*rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_Mesh_XGrid_calc_wgts_to_side_mesh()"


  try {

    // Weights matrix
    IWeights wts;

    // Calc the weights
    calc_wgts_from_xgrid_to_side_mesh(src_xgrid_mesh, dst_side_mesh, wts);

    // Translate weights to sparse matrix representation used by code above this
    std::pair<UInt,UInt> iisize = wts.count_matrix_entries();
    int num_entries = iisize.first;
    int *iientries = new int[2*iisize.first];
    int larg[2] = {2, static_cast<int>(iisize.first)};
    // Gather the list
    ESMCI::InterArray<int> ii(iientries, 2, larg);
    ESMCI::InterArray<int> *iiptr = &ii;

    double *factors = new double[iisize.first];

    UInt i = 0;
    WMat::WeightMap::iterator wi = wts.begin_row(), we = wts.end_row();
    for (; wi != we; ++wi) {
      const WMat::Entry &w = wi->first;

      std::vector<WMat::Entry> &wcol = wi->second;

      // Construct factor index list
      for (UInt j = 0; j < wcol.size(); ++j) {
        UInt twoi = 2*i;
        const WMat::Entry &wc = wcol[j];

        // Construct factor list entry
        iientries[twoi+1] = w.id;  iientries[twoi] = wc.id;
        factors[i] = wc.value;

        i++;
      } // for j
    } // for wi

    // If meshes are split, then translate indices and factorss
    if (src_xgrid_mesh->is_split) translate_split_src_elems_in_wts(src_xgrid_mesh, num_entries, iientries);
    if (dst_side_mesh->is_split) translate_split_dst_elems_in_wts(dst_side_mesh, num_entries, iientries, factors);
    
    *nentries = num_entries;
    // Clean up.  If has_iw, then we will use the arrays to
    // fill out the users pointers.  These will be deleted following a copy.
    // Save off the weights so the F90 caller can allocate arrays and
    // copy the values.
    *tweights = new ESMCI::TempWeights;
    (*tweights)->nentries = num_entries;
    (*tweights)->factors = factors;
    (*tweights)->iientries = iientries;


  } catch(std::exception &x) {
    // catch Mesh exception return code
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                          x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                          "UNKNOWN", ESMC_CONTEXT, rc);
    }

    return;
  } catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }
  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}
