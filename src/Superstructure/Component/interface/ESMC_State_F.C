// $Id: ESMC_State_F.C,v 1.2 2003/02/19 18:50:49 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
//
//==============================================================================
//
//==============================================================================
//
// This file contains interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include <stdio.h>
#include <string.h>
#include "ESMC.h"
#include "ESMC_Base.h"
#include "ESMC_State.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The {\tt State} implementation language is Fortran 90, but if any
// callback routines are needed this is where they should go.
//
// For the general C++ interfaces to the public entry points, see 
// the file {\tt ESMF_State_C.F90}.
//
//EOP


// the interface subroutine names MUST be in lower case
extern "C" {

     // currently there are no places in the State code where
     // the F90 routines need to call into C++.

};



