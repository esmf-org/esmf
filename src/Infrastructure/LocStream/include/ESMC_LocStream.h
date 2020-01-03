// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2020, University Corporation for Atmospheric Research,
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
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_LocStream_h
#define ESMC_LocStream_h

//-----------------------------------------------------------------------------
// ESMC_LocStream - Public C interface to the ESMF LocStream class
//
// The code in this file defines the public C LocStream class and declares method
// signatures (prototypes).  The companion file {\tt ESMC\_LocStream.C} contains
// the definitions (full code bodies) for the LocStream methods.
//-----------------------------------------------------------------------------

#include "ESMC_Mesh.h"
#include "ESMC_Array.h"
#include "ESMC_ArraySpec.h"
#include "ESMC_RHandle.h"
#include "ESMC_Interface.h"
#include "ESMC_Grid.h"
#include "ESMC_Util.h"

#if defined (__cplusplus)
extern "C" {
#endif

// Class declaration type
typedef struct{
  void *ptr;
}ESMC_LocStream;

// Class API

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_LocStreamCreateLocal - Create a LocStream
//
// !INTERFACE:
ESMC_LocStream ESMC_LocStreamCreateLocal(
                                    int ls_size,
                                    enum ESMC_IndexFlag *indexflag,
                                    enum ESMC_CoordSys_Flag *coordSys,
                                    int *rc
);

// !RETURN VALUE:
//  Newly created ESMC_LocStream object.
//
// !DESCRIPTION:
//
//  Creates a {\tt ESMC\_LocStream} object.
//
//  The arguments are:
//  \begin{description}
//  \item[{[ls_size]}]
//    number of points in the location stream.
//  \item[indexflag]
//    Indicates the indexing scheme to be used in the new LocStream. If not present,
//    defaults to ESMC\_INDEX\_DELOCAL.
//  \item[coordSys]
//    The coordinated system of the LocStream coordinate data. If not specified then
//    defaults to ESMF\_COORDSYS\_SPH\_DEG.
//  \item[{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_LocStreamGetBounds - Get the LocStream bounds
//
// !INTERFACE:
int ESMC_LocStreamGetBounds(
  ESMC_LocStream locstream,      // in
  int localDe,
  int *cLBound,
  int *cUBound
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Get the LocStream bounds from the {\tt ESMC\_LocStream}.
//
//  The arguments are:
//  \begin{description}
//  \item[LocStream]
//    {\tt ESMC\_LocStream} whose bounds will be returned
//  \item[localDe]
//    The local DE of the {\tt ESMC\_LocStream} (not implemented)
//  \item[exclusiveLBound]
//    The exclusive lower bounds of the {\tt ESMC\_LocStream}
//  \item[exclusiveUBound]
//    The exclusive upper bounds of the {\tt ESMC\_LocStream}
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_LocStreamAddKeyAlloc - allocate memory for adding a key
//
// !INTERFACE:
int ESMC_LocStreamAddKeyAlloc(
  ESMC_LocStream locstream,      // in
  const char *keyName,
  enum ESMC_TypeKind_Flag *keyTypeKind
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  allocate memory for a new key in {\tt ESMC\_LocStream}.
//
//  The arguments are:
//  \begin{description}
//  \item[LocStream]
//    {\tt ESMC\_LocStream} who will have the new key
//  \item[keyName]
//    name of the new key in {\tt ESMC\_LocStream}
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_LocStreamGetKeyPtr - Get a pointer to a LocStream key
//
// !INTERFACE:
void *ESMC_LocStreamGetKeyPtr(
  ESMC_LocStream locstream,      // in
  const char *keyName,
  int localDe,
  int *rc
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Get a pointer to a specified key in {\tt ESMC\_LocStream}.
//
//  The arguments are:
//  \begin{description}
//  \item[LocStream]
//    {\tt ESMC\_LocStream} containing the key.
//  \item[keyName]
//    name of the new key in {\tt ESMC\_LocStream}
//  \item[localDe]
//    The local DE of the {\tt ESMC\_LocStream} (not implemented)
//  \item[{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_LocStreamGetKeyArray - Get the internal Array stored in the LocStream
//
// !INTERFACE:
  ESMC_Array ESMC_LocStreamGetKeyArray(
                                ESMC_LocStream locstream,     // in
                                const char *keyName,          // in
                                int *rc                       // out
                                );

// !RETURN VALUE:
//  The ESMC_Array object stored in the ESMC_LocStream.
//
// !DESCRIPTION:
//
//  Get the internal Array stored in the {\tt ESMC\_LocStream}.
//
//  The arguments are:
//  \begin{description}
//  \item[LocStream]
//    {\tt ESMC\_LocStream} containing the array.
//  \item[keyName]
//    name of the new key in {\tt ESMC\_LocStream}
//  \item[{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_LocStreamDestroy - Destroy a LocStream
//
// !INTERFACE:
int ESMC_LocStreamDestroy(
  ESMC_LocStream *locstream     // inout
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Releases all resources associated with this {\tt ESMC\_LocStream}.
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//
//  The arguments are:
//  \begin{description}
//  \item[LocStream]
//    Destroy contents of this {\tt ESMC\_LocStream}.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------


#if defined (__cplusplus)
} // extern "C"
#endif

#endif
