// $Id$
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_Conserve2ndInterp_h
#define  ESMCI_Conserve2ndInterp_h


#include <Mesh/include/ESMCI_MeshDB.h>
#include <Mesh/include/ESMCI_MeshObj.h>
#include <Mesh/include/ESMCI_MEField.h>
#include <Mesh/include/ESMCI_MasterElement.h>
#include <Mesh/include/ESMCI_Exception.h>
#include <Mesh/include/ESMCI_MCoord.h>
#include <Mesh/include/ESMCI_Sintdnode.h>
#include <Mesh/include/ESMCI_SM.h>

#include <vector>

namespace ESMCI {

  struct HC_WGHT{
  HC_WGHT() : src_id(-1), dst_id(-1), dst_index(-1), wgt(0.0) {}

    int src_id;
    int dst_id;
    int dst_index;
    double wgt;
  };

  typedef struct {
    MeshObj *elem;
    double cntr[3];
    double angle;

    double grad[3];

  } NBR_ELEM;

#if 0
  typedef struct {
    double cntr[3];
    double area;
    int dst_index;
  } SM_CELL;
#endif

  void calc_2nd_order_weights_2D_3D_sph(const MeshObj *src_elem, MEField<> *src_cfield, MEField<> *src_mask_field, 
                                           std::vector<const MeshObj *> dst_elems, MEField<> *dst_cfield, MEField<> * dst_mask_field, MEField<> * dst_frac2_field,
                                           double *src_elem_area,
                                           std::vector<int> *valid, 
                                           std::vector<HC_WGHT> *wgts, 
                                           std::vector<double> *sintd_areas_out, std::vector<double> *dst_areas_out,
                                           std::vector<int> *tmp_valid, std::vector<double> *tmp_sintd_areas_out, std::vector<double> *tmp_dst_areas_out,
                                           std::vector<SM_CELL> *sm_cells, 
                                           std::vector<NBR_ELEM> *nbrs
                                           );

  void calc_2nd_order_weights_2D_2D_cart(const MeshObj *src_elem, MEField<> *src_cfield, MEField<> *src_mask_field, 
                                           std::vector<const MeshObj *> dst_elems, MEField<> *dst_cfield, MEField<> * dst_mask_field, MEField<> * dst_frac2_field,
                                           double *src_elem_area,
                                           std::vector<int> *valid, 
                                           std::vector<HC_WGHT> *wgts, 
                                           std::vector<double> *sintd_areas_out, std::vector<double> *dst_areas_out,
                                           std::vector<int> *tmp_valid, std::vector<double> *tmp_sintd_areas_out, std::vector<double> *tmp_dst_areas_out,
                                           std::vector<SM_CELL> *sm_cells, 
                                           std::vector<NBR_ELEM> *nbrs
                                           );

} // namespace

#endif
