// $Id: ESMCI_Search.h,v 1.13 2012/11/13 22:22:41 oehmke Exp $
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_MBMeshDual_h
#define ESMCI_MBMeshDual_h

#if defined ESMF_MOAB

#include <Mesh/include/Legacy/ESMCI_MBMesh.h>


namespace ESMCI {

class MBMesh;

 void MBMeshDual(MBMesh *src_mesh, MBMesh **_dual_mesh);

#endif

} //namespace

#endif
