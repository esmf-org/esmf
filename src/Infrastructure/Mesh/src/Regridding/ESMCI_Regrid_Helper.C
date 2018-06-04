// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_Regrid_F.C"
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "ESMCI_Macros.h"
#include "ESMCI_VM.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Grid.h"
#include "ESMCI_GridToMesh.h"
#include "ESMC_Util.h"
#include "ESMCI_Array.h"

#include "Mesh/include/Regridding/ESMCI_Regrid_Helper.h"


//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
//
//EOP
//-------------------------------------------------------------------------


using namespace ESMCI;


// Copy the weights stored in the temporary tw into the fortran arrays.  Also,
// delete the temp weights.
extern "C" void FTN_X(c_esmc_copy_tempweights)(ESMCI::TempWeights **_tw, int *ii, double *w) {

  // See if the TempWeights structure is allocated, if not then just leave
  if (*_tw==NULL) return;

  // Copy Weights
  ESMCI::TempWeights &tw = (**_tw);

  for (int i = 0; i < tw.nentries; ++i) {
    int two_i = i << 1;

    // Reverse order of indices
    //ii[i] = tw.iientries[two_i+0];
    //ii[tw.nentries+i] = tw.iientries[two_i+1];
    ii[two_i+0] = tw.iientries[two_i+0];
    ii[two_i+1] = tw.iientries[two_i+1];

    w[i] = tw.factors[i];
  }

  if (tw.factors != NULL) delete [] tw.factors;
  if (tw.iientries != NULL) delete [] tw.iientries;

  delete *_tw;

 }


// Copy the weights stored in the temporary tw into the fortran arrays.  Also,
// delete the temp weights.
extern "C" void FTN_X(c_esmc_copy_tempudl)(int *_num_udl, ESMCI::TempUDL **_tudl, int *unmappedDstList) {

  // See if the TempUDL structure is allocated, if not then just leave
  if (*_tudl==NULL) return;


  // Get number of points
  int num_udl = *_num_udl;

  // Copy Weights
  ESMCI::TempUDL &tudl = (**_tudl);

  for (int i = 0; i<num_udl; ++i) {
    unmappedDstList[i] = tudl.udl[i];
  }

  if (tudl.udl != NULL) delete [] tudl.udl;

  delete *_tudl;
}

#undef  ESMC_METHOD
