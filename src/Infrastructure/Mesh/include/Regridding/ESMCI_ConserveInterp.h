// $Id$
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_ConserveInterp_h
#define  ESMCI_ConserveInterp_h


#include <Mesh/include/Legacy/ESMCI_MeshDB.h>
#include <Mesh/include/Legacy/ESMCI_MeshObj.h>
#include <Mesh/include/Legacy/ESMCI_MEField.h>
#include <Mesh/include/Legacy/ESMCI_MasterElement.h>
#include <Mesh/include/Legacy/ESMCI_Exception.h>
#include <Mesh/include/Legacy/ESMCI_MCoord.h>
#include <Mesh/include/Legacy/ESMCI_Sintdnode.h>

#include <vector>

namespace ESMCI {

  struct interp_res{
    const MeshObj * clip_elem;
    int num_sintd_nodes;
    int num_subject_nodes;
    int num_clip_nodes;
    int sdim;
    double * subject_coords;
    double * clip_coords;
    double subject_elem_area;
    double clip_elem_area;
    double fraction;

    interp_res(const MeshObj * _clip_elem, int _num_sintd_nodes, int _num_subject_nodes, int _num_clip_nodes,
      int _sdim, double * _subject_coords, double * _clip_coords,
      double _subject_elem_area, double _clip_elem_area, double _fraction) : clip_elem(_clip_elem),
        num_sintd_nodes(_num_sintd_nodes), num_subject_nodes(_num_subject_nodes), num_clip_nodes(_num_clip_nodes),
        sdim(_sdim), subject_elem_area(_subject_elem_area), clip_elem_area(_clip_elem_area),
        fraction(_fraction) {

      subject_coords = new double[num_subject_nodes*sdim];
      for(int i = 0; i < num_subject_nodes*sdim; i ++) subject_coords[i] = _subject_coords[i];

      clip_coords = new double[num_clip_nodes*sdim];
      for(int i = 0; i < num_clip_nodes*sdim; i ++) clip_coords[i] = _clip_coords[i];

    }

    ~interp_res(){
      delete[] subject_coords;
      delete[] clip_coords;
    }

  };

  typedef std::multimap<const MeshObj *, const interp_res *> interp_map;
  typedef std::multimap<const MeshObj *, const interp_res *> * interp_mapp;
  typedef std::multimap<const MeshObj *, const interp_res *>::const_iterator interp_map_citer;
  typedef std::multimap<const MeshObj *, const interp_res *>::iterator interp_map_iter;
  typedef std::pair<std::multimap<const MeshObj *, const interp_res *>::iterator,
                    std::multimap<const MeshObj *, const interp_res *>::iterator > interp_map_range;

  void calc_1st_order_weights_2D_2D_cart(const MeshObj *src_elem, MEField<> *src_cfield,
                                           std::vector<const MeshObj *> dst_elems, MEField<> *dst_cfield, MEField<> *dst_mask_field, MEField<> * dst_frac2_field,
                                           double *src_elem_area,
                                           std::vector<int> *valid, std::vector<double> *wgts, std::vector<double> *areas, std::vector<double> *dst_areas,
                                           std::vector<int> *tmp_valid, std::vector<double> *tmp_sintd_areas_out, std::vector<double> *tmp_dst_areas_out,
                                           Mesh * midmesh, std::vector<sintd_node *> * sintd_nodes, std::vector<sintd_cell *> * sintd_cells,
                                           interp_mapp res_map, struct Zoltan_Struct * zz);


  void calc_1st_order_weights_2D_3D_sph(const MeshObj *src_elem, MEField<> *src_cfield, 
                                        std::vector<const MeshObj *> dst_elems, MEField<> *dst_cfield, MEField<> *dst_mask_field, MEField<> * dst_frac2_field,
                                        double *src_elem_area,
                                        std::vector<int> *valid, std::vector<double> *wgts, std::vector<double> *areas, std::vector<double> *dst_areas,
                                        std::vector<int> *tmp_valid, std::vector<double> *tmp_sintd_areas_out, std::vector<double> *tmp_dst_areas_out,
                                        Mesh * midmesh, std::vector<sintd_node *> * sintd_nodes, std::vector<sintd_cell *> * sintd_cells, 
					interp_mapp res_map, struct Zoltan_Struct * zz, MEField<> *src_side_field=NULL, MEField<> *dst_side_field=NULL);

  void calc_1st_order_weights_3D_3D_cart(const MeshObj *src_elem, MEField<> *src_cfield,
                                           std::vector<const MeshObj *> dst_elems, MEField<> *dst_cfield, MEField<> *dst_mask_field, MEField<> * dst_frac2_field,
                                           double *src_elem_area,
                                           std::vector<int> *valid, std::vector<double> *wgts, std::vector<double> *areas, std::vector<double> *dst_areas,
                                           Mesh *midmesh, std::vector<sintd_node *> * sintd_nodes,
                                           std::vector<sintd_cell *> * sintd_cells, interp_mapp res_map, struct Zoltan_Struct *zz);

  void intersect_convex_poly2D(int num_p, double *p,
             int num_q, double *q,
             double *tmp,
             int *num_out, double *out);

  void intersect_convex_2D_3D_sph_gc_poly(int num_p, double *p,
             int num_q, double *q,
             double *tmp,
             int *num_out, double *out);

  bool line_with_seg2D(double *a1, double *a2, double *sin, double *sout,
                       double *p);
  bool line_with_gc_seg3D(double *a1, double *a2, double *sin, double *sout,
                       double *p);



} // namespace

#endif
