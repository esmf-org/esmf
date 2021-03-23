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
#ifndef ESMCI_CreepFill_h
#define ESMCI_CreepFill_h

#include <Mesh/include/Legacy/ESMCI_MeshDB.h>

#include <vector>
using std::vector;

namespace ESMCI {

  void CreepFill(Mesh &mesh, vector<int> &valid_gids, int num_creep_levels, int num_donor_levels, WMat &wts, bool set_dst_status, WMat &dst_status);

} // namespace

#endif
