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
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMCI_LocStream_H
#define ESMCI_LocStream_H

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_LocStream - represention for the locations of a set of data points
//
// !DESCRIPTION:
//
// The code in this file defines the ESMCI LocStream class prototypes for the
// Fortran interface routines. The companion file ESMCI\_LocStream_C.F90  contains
// the definitions (full code bodies) for the interface routines.
//
//EOP
//

//-----------------------------------------------------------------------------
//
// !USES:
#include "ESMC_LocStream.h"
#include "ESMC_Mesh.h"
#include "ESMC_Array.h"
#include "ESMC_ArraySpec.h"
#include "ESMC_RHandle.h"
#include "ESMCI_RHandle.h"
#include "ESMCI_F90Interface.h"
#include "ESMC_Interface.h"
#include "ESMCI_Util.h"
#include "ESMC_Grid.h"


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// C++ LocStream class declaration
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
namespace ESMCI{

  class LocStream{
    // pointer to fortran derived type
    F90ClassHolder fortranclass;
    // methods
   public:
    LocStream(){}
    LocStream(F90ClassHolder fc){
      fortranclass = fc;
    }

    // TODO: ESMC objects should be cast to ESMCI objects in the ESMC layer
    static LocStream *create(int ls_size, ESMC_IndexFlag *indexflag,
                             ESMC_CoordSys_Flag *coordSys, int *rc);
    static int getbounds(LocStream *locstream, int localDe,
                         int *cLBound, int *cUBound);
    static int addKeyAlloc(LocStream *locstream, const char *keyName,
                           ESMC_TypeKind_Flag *typekind);
    ESMC_Array getKeyArray(const char *keyName, int *rc);
    static int destroy(LocStream *locstream);
  };
}

#endif  // ESMCI_LocStream_H
