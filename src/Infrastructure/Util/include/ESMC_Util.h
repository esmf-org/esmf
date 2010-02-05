// $Id: ESMC_Util.h,v 1.32.2.1 2010/02/05 20:01:04 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// This file is part of the pure C public ESMC API
//-----------------------------------------------------------------------------

//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_UTIL_H
#define ESMC_UTIL_H

#include "ESMC_Conf.h"

// general logical value - MUST MATCH F90
enum ESMC_Logical { ESMF_TRUE=1,
                    ESMF_FALSE };

enum ESMC_TypeKind { ESMC_TYPEKIND_I1=1,
                     ESMC_TYPEKIND_I2,
                     ESMC_TYPEKIND_I4,
                     ESMC_TYPEKIND_I8,
                     ESMC_TYPEKIND_R4,
                     ESMC_TYPEKIND_R8,
                     ESMF_C8,
                     ESMF_C16,
                     ESMC_TYPEKIND_LOGICAL,
                     ESMC_TYPEKIND_CHARACTER,
                     ESMF_NOKIND=99 };

// ESMF platform-dependent data types
#if (ESMC_POINTER_SIZE == 4)
  // 32-bit machine
  typedef long long ESMC_I8;
  typedef int       ESMC_I4;
  typedef short     ESMC_I2;
  typedef char      ESMC_I1;
  typedef double    ESMC_R8;
  typedef float     ESMC_R4;
  typedef unsigned long      ESMC_POINTER;
#else
  // 64-bit machine
#if defined (PARCH_mingw)
  typedef long long ESMC_I8;
#else
  typedef long      ESMC_I8;
#endif
  typedef int       ESMC_I4;
  typedef short     ESMC_I2;
  typedef char      ESMC_I1;
  typedef double    ESMC_R8;
  typedef float     ESMC_R4;
  typedef unsigned long long ESMC_POINTER;
#endif

#endif  // ESMC_UTIL_H
