// $Id: ESMCI_MathUtil.h,v 1.1 2010/09/09 20:27:18 oehmke Exp $
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_MathUtil_h
#define  ESMCI_MathUtil_h


#include <Mesh/include/ESMCI_MeshDB.h>
#include <Mesh/include/ESMCI_MeshObj.h>
#include <Mesh/include/ESMCI_MEField.h>
#include <Mesh/include/ESMCI_MasterElement.h>
#include <Mesh/include/ESMCI_Exception.h>
#include <Mesh/include/ESMCI_MCoord.h>

#include <vector>

namespace ESMCI {


  bool invert_matrix_3x3(double m[], double m_inv[]);

  bool intersect_quad_with_line(const double *q, const double *l1, const double *l2, double *p,
				double *t);

  bool intersect_tri_with_line(const double *tri, const double *l1, const double *l2, double *p,
			       double *t);

} // namespace

#endif
