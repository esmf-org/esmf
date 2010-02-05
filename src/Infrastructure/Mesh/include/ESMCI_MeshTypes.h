//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_MeshTypes_h
#define ESMCI_MeshTypes_h

#include <Mesh/include/sacado/Sacado.hpp>

namespace ESMCI {

typedef unsigned int UInt;
typedef unsigned char UChar;
typedef unsigned short UShort;
typedef unsigned long ULong;

typedef Sacado::Fad::DFad<double> fad_type;


// Some promotion traits
template<typename T1, typename T2>
struct richest_type {
  typedef T1 value;
};

template<>
struct richest_type<double,fad_type> {
  typedef fad_type value;
};

template<>
struct richest_type<fad_type,double> {
  typedef fad_type value;
};

// The good old fortran macro
#define ESMCI_FTN(a) a##_

} // namespce

#endif
