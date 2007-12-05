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
#ifndef ESMC_AdaptMarker_h
#define ESMC_AdaptMarker_h

#include <ESMC_MEField.h>

/**
 * @defgroup markers
 * A group of marking strategies.  Marking takes an element error field as an input
 * and performs various statistical analysis to decide what elements to refine and
 * unrefine.  It then calls the underlying hadapt to mark the elements.
 */

namespace ESMC {

class Mesh;
class HAdapt;

/**
 * Some classes to mark elements for refinement under
 * various strategies.
 * @ingroup markers
 */
class QuantileMarker {
public:

/**
 * Mark the elements based on quantiles.
 * @param top_frac refine elements above this quantile.
 * @param bot_frac unrefine elements below this quantile.
 */
QuantileMarker(HAdapt &hadapt, MEField<> &err, double top_frac, double bot_frac);
};

} // namespace

#endif
