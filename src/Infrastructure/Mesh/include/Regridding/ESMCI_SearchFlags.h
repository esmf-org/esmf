// $Id$
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_SearchFlags_h
#define ESMCI_SearchFlags_h

namespace ESMCI {

// Regrid Status return
#define ESMCI_REGRIDSTATUS_OUTSIDE_SRC
#define ESMCI_REGRIDSTATUS_SRC_MASKED
#define ESMCI_REGRIDSTATUS_DST_MASKED


// What to do if dest points aren't found
#define ESMCI_UNMAPPEDACTION_ERROR  0
#define ESMCI_UNMAPPEDACTION_IGNORE 1

} //namespace

#endif
