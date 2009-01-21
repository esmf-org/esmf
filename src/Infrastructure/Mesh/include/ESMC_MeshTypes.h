// $Id: ESMC_MeshTypes.h,v 1.3.2.2 2009/01/21 21:25:22 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.


// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_MeshTypes_h
#define ESMC_MeshTypes_h

#include <sacado/Sacado.hpp>

namespace ESMCI {
namespace MESH {

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
#define ESMC_FTN(a) a##_

} // namespce
} // namespce

#endif
