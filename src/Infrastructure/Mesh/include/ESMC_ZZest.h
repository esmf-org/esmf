//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMC_ZZest_h
#define ESMC_ZZest_h

#include <ESMC_MEField.h>

namespace ESMC {

class Mesh;

/**
 * Implements the zz estimation algorithm.  Uses patch recovery to
 * find a better representation of the gradient of a function.  Since
 * elliptic problems typically have superconvergence at gauss points, 
 * a least squares fit around nodes gives a good estimation to the 
 * gradient.
 * The error estimate is the L2 integral of the difference in patch
 * and numerical gradients. 
 */
class ZZest {
public:
ZZest(Mesh &);
~ZZest();

/**
 * Perform the error estimation.
 * @param field: the field to estimate on.  Sums error in each comp.
 * @param err: element field to place the error estimate.
 */
void Estimate(MEField<> &field, MEField<> &err);

private:
Mesh &mesh;

};

} // namespace

#endif
