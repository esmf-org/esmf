// $Id: ESMCI_ConserveInterp.h,v 1.13 2012/03/27 22:37:28 oehmke Exp $
// Earth System Modeling Framework
// Copyright 2002-2012, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_ConserveInterp_h
#define  ESMCI_ConserveInterp_h


#include <Mesh/include/ESMCI_MeshDB.h>
#include <Mesh/include/ESMCI_MeshObj.h>
#include <Mesh/include/ESMCI_MEField.h>
#include <Mesh/include/ESMCI_MasterElement.h>
#include <Mesh/include/ESMCI_Exception.h>
#include <Mesh/include/ESMCI_MCoord.h>
#include <Mesh/include/ESMCI_Sintdnode.h>

#include <vector>

namespace ESMCI {

  void calc_1st_order_weights_2D_2D_cart(const MeshObj *src_elem, MEField<> *src_cfield, 
                                           std::vector<const MeshObj *> dst_elems, MEField<> *dst_cfield, 
                                           double *src_elem_area,
                                        std::vector<int> *valid, std::vector<double> *wgts, std::vector<double> *areas,
                                        Mesh * midmesh, std::vector<sintd_node *> * sintd_nodes, std::vector<sintd_cell *> * sintd_cells, 
                                        struct Zoltan_Struct * zz);

  void calc_1st_order_weights_2D_3D_sph(const MeshObj *src_elem, MEField<> *src_cfield, 
                                           std::vector<const MeshObj *> dst_elems, MEField<> *dst_cfield, 
                                           double *src_elem_area,
                                           std::vector<int> *valid, std::vector<double> *wgts, std::vector<double> *areas, std::vector<double> *dst_areas,
                                           Mesh * midmesh, std::vector<sintd_node *> * sintd_nodes, std::vector<sintd_cell *> * sintd_cells, 
                                           struct Zoltan_Struct * zz);

  void calc_1st_order_weights_3D_3D_cart(const MeshObj *src_elem, MEField<> *src_cfield, 
                                           std::vector<const MeshObj *> dst_elems, MEField<> *dst_cfield, 
                                           double *src_elem_area,
                                           std::vector<int> *valid, std::vector<double> *wgts, std::vector<double> *areas, 
                                           Mesh *midmesh, std::vector<sintd_node *> * sintd_nodes, 
                                         std::vector<sintd_cell *> * sintd_cells, struct Zoltan_Struct *zz);

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
