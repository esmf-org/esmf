// $Id$
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_SM_h
#define  ESMCI_SM_h

namespace ESMCI {

  typedef struct {
    double cntr[3];
    double area;
    int dst_index;
  } SM_CELL;

  void create_SM_cells_2D_3D_sph(const MeshObj *src_elem, MEField<> *src_cfield, 
                                 std::vector<const MeshObj *> dst_elems, MEField<> *dst_cfield, MEField<> * dst_mask_field, MEField<> * dst_frac2_field,
                                 double *src_elem_area,
                                 std::vector<int> *valid, 
                                 std::vector<double> *sintd_areas_out, std::vector<double> *dst_areas_out,
                                 std::vector<int> *tmp_valid, std::vector<double> *tmp_sintd_areas_out, std::vector<double> *tmp_dst_areas_out,
                                 std::vector<SM_CELL> *sm_cells);


  void create_SM_cells_2D_2D_cart(const MeshObj *src_elem, MEField<> *src_cfield, 
                                  std::vector<const MeshObj *> dst_elems, MEField<> *dst_cfield, MEField<> * dst_mask_field, MEField<> * dst_frac2_field,
                                  double *src_elem_area,
                                  std::vector<int> *valid, 
                                  std::vector<double> *sintd_areas_out, std::vector<double> *dst_areas_out,
                                  std::vector<int> *tmp_valid, std::vector<double> *tmp_sintd_areas_out, std::vector<double> *tmp_dst_areas_out, 
                                  std::vector<SM_CELL> *sm_cells);

} // namespace

#endif
