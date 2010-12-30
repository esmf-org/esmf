// $Id: ESMCI_ConserveInterp.h,v 1.3 2010/12/30 22:30:20 oehmke Exp $
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
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

#include <vector>

namespace ESMCI {


  void calc_1st_order_weights_2D_2D_cart(const MeshObj *src_elem, MEField<> *src_cfield, 
                             std::vector<const MeshObj *> dst_elems, MEField<> *dst_cfield, 
			      std::vector<int> *valid, std::vector<double> *wgts);

  void calc_1st_order_weights_2D_3D_sph(const MeshObj *src_elem, MEField<> *src_cfield, 
                                           std::vector<const MeshObj *> dst_elems, MEField<> *dst_cfield, 
                                           std:: vector<char> dst_node_on_bndry, double *src_elem_area,
                                           std::vector<int> *valid, std::vector<double> *wgts, std::vector<double> *areas);

  void fill_node_is_on_bndry_list(Mesh &mesh, std::vector<char> *is_on_bndry);

} // namespace

#endif
