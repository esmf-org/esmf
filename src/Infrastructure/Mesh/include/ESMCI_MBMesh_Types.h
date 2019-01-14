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
#ifndef ESMCI_MBMesh_Types_h
#define ESMCI_MBMesh_Types_h

// Take out if MOAB isn't being used
#if defined ESMF_MOAB

namespace ESMCI {

typedef unsigned int UInt;
typedef unsigned char UChar;
typedef unsigned short UShort;
typedef unsigned long ULong;

// Some promotion traits
template<typename T1, typename T2>
struct mbmesh_richest_type {
  typedef T1 value;
};

} // namespace

#endif
#endif
