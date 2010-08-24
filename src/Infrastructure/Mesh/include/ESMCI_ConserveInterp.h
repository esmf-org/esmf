// $Id: ESMCI_ConserveInterp.h,v 1.1 2010/08/24 16:10:51 oehmke Exp $
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


  void calc_1st_order_weights(const MeshObj *src_elem, MEField<> *src_cfield, 
                             std::vector<const MeshObj *> dst_elems, MEField<> *dst_cfield, 
			      std::vector<int> *valid, std::vector<double> *wgts);

} // namespace

#endif
