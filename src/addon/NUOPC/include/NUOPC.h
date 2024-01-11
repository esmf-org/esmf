// $Id$
//
// Earth System Modeling Framework
// Copyright (c) 2002-2024, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//-----------------------------------------------------------------------------

#include "ESMC.h"

//-----------------------------------------------------------------------------
// This file is part of the pure C public NUOPC API
//-----------------------------------------------------------------------------

#ifndef NUOPC_H
#define NUOPC_H

int NUOPC_CompDerive(
  ESMC_GridComp comp,                           // in
  void (*userRoutine)(ESMC_GridComp, int *)     // in
);

void NUOPC_ModelSetServices(ESMC_GridComp, int *);

#endif  // NUOPC_H
