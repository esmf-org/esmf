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
#ifndef ESMCI_Integrate_h
#define ESMCI_Integrate_h

#include <Mesh/include/Legacy/ESMCI_ParEnv.h>
#include <Mesh/include/Legacy/ESMCI_MEValues.h>
#include <Mesh/include/Legacy/ESMCI_MEField.h>
#include <Mesh/include/Legacy/ESMCI_MeshObjConn.h>

namespace ESMCI {

/*
 * Provides integration for serial and parallel meshes.
 */

class Mesh;

class Integrate {

public:

  Integrate(Mesh &);
  ~Integrate();

  // Generate the integration weights on a specified mesh
  void intWeights(MEField<> *);

  // Add weights to meshes before poles so they are on user data points
  void AddPoleWeights(Mesh &, UInt, MEField<> *);

  void clearWeights(MEField<> *iwts);

private:

  // integration weights parallel?
  void int_weights_serial(MEField<> *);
  void int_weights_parallel(MEField<> *);

  // mesh to which these integration weights apply
  Mesh &mesh;
  bool is_parallel;

};



} // namespace

#endif /*ESMC_INTEGRATE_H_*/
