// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements C++ methods used to verify that PIO
// derived types can be passed correctly between F90 and the ESMF C++
// interface.
//
//-------------------------------------------------------------------------
//
#define ESMC_FILENAME "ESMCI_WordsizeSubr.C"

#include <cstddef>
#include <string>
using namespace std;

// ESMF header
#include "ESMC.h"

// ESMF Test header
#include "ESMCI_Test.h"

using namespace ESMCI;

#include <pio.h>

