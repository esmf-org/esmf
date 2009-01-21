// $Id: ESMC_Util.h,v 1.26.2.2 2009/01/21 21:25:24 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMF Util C++ declaration include file
//
//-----------------------------------------------------------------------------
//

#ifndef ESMC_UTIL_H
#define ESMC_UTIL_H

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


#endif  // ESMC_UTIL_H
