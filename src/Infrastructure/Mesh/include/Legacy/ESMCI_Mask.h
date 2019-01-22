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
#ifndef ESMCI_Mask_h
#define ESMCI_Mask_h


// Definitions for mask macros
namespace ESMCI {


#define MASKED 1.0
#define NOT_MASKED 0.0
#define IS_MASKED(val)  ((val)>0.5)


} // namespace ESMCI

#endif
