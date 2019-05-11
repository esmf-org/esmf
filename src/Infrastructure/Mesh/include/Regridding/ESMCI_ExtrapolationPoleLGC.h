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
#ifndef ESMCI_EXTRAPOLATION_H_
#define ESMCI_EXTRAPOLATION_H_

#include <Mesh/include/Legacy/ESMCI_MeshTypes.h>

namespace ESMCI {

class Mesh; class Context; class IWeights;

/*
 * Triangulate the pole that is encircled by nodes with nodeset=node_id.
 * ASSUMPTION1: The pole is well formed.
 *
 * AFTER EFFECT: This routine will ship all elements attached to these nodes to
 * processors 0.  For the moment it will NOT ship them back.
 *
 * We then create the pole, fill out the weights, and mark the pole with the
 * constraint context.
 *
 * TODO: Make work for quadratic meshes;
 */
void MeshAddPole(Mesh &mesh, UInt node_id,
                      UInt constraint_id,
                      IWeights &cweights);

void MeshAddPoleNPnts(Mesh &mesh, int num_avg_pnts, UInt node_id,
                      UInt constraint_id,
                      IWeights &cweights);

  void MeshAddPoleTeeth(Mesh &mesh, UInt node_id,
                        UInt constraint_id, IWeights &cweights);
} // namespace

#endif /*ESMC_EXTRAPOLATION_H_*/
