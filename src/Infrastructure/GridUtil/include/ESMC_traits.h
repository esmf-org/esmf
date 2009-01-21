// $Id: ESMC_traits.h,v 1.3.2.2 2009/01/21 21:25:21 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMF Ptypes C++ convenient type definitions for programmers.
//
//-----------------------------------------------------------------------------
//

#ifndef ESMC_traits_h
#define ESMC_traits_h

// Type traits for use within template code.  The tr1 libraries contain
// these capabilities, but, unfortunately, tr1 is not part of the C++ standard,
// and is not supported on all of our platforms.  It is, however, under review
// and is expected to be approved to the C++ standard.  This file provides
// a subset of tr1 functionality in the meantime.
namespace ESMC {
typedef unsigned char UChar;
typedef unsigned int  UInt; 

// Default is_unsigned
template <typename T>
struct is_unsigned { enum {value = 0}; };

// is_unsigend::value = true specializations
template <> struct is_unsigned<unsigned char> { enum {value = 1}; };
template <> struct is_unsigned<unsigned short> { enum {value = 1}; };
template <> struct is_unsigned<unsigned int> { enum {value = 1}; };
template <> struct is_unsigned<unsigned long> { enum {value = 1}; };
template <> struct is_unsigned<unsigned long long> { enum {value = 1}; };

// Add unsigned to a type
template <typename T>
struct to_unsigned { typedef T type; };
template <> struct to_unsigned<char> { typedef unsigned char type; };
template <> struct to_unsigned<short> { typedef unsigned short type; };
template <> struct to_unsigned<int> { typedef unsigned int type; };
template <> struct to_unsigned<long> { typedef unsigned long type; };
template <> struct to_unsigned<long long> { typedef unsigned long long type; };

} // namespace

#endif  // ESMC_Ptypes_h
