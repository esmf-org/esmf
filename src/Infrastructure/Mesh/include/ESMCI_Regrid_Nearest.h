// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

#ifndef ESMCI_Regrid_Nearest_h
#define ESMCI_Regrid_Nearest_h

#include "Mesh/include/Regridding/ESMCI_WMat.h"


//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
// static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


using namespace ESMCI;

void calc_nearest_regrid_wgts(PointList *srcpl, PointList *dstpl, 
                              WMat &wts, bool set_dst_status, 
                              WMat &dst_status, int *regridMethod,
                              int *extrapNumSrcPnts, 
                              ESMC_R8 *extrapDistExponent);

#endif
