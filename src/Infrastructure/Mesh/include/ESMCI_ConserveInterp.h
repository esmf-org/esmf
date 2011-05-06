// $Id: ESMCI_ConserveInterp.h,v 1.7 2011/05/06 18:59:17 feiliu Exp $
// Earth System Modeling Framework
// Copyright 2002-2011, University Corporation for Atmospheric Research, 
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
                                           std::vector<int> *valid, std::vector<double> *wgts, std::vector<double> *areas,
                                           Mesh * midmesh, std::vector<sintd_node *> * sintd_nodes, std::vector<sintd_cell *> * sintd_cells, 
                                           struct Zoltan_Struct * zz);


} // namespace

#endif
