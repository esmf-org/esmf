// $Id: ESMC_Field.h,v 1.20 2010/06/24 02:47:06 feiliu Exp $
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
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_Field_h
#define ESMC_Field_h

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_Field - Public C interface to the ESMF Field class
//
// !DESCRIPTION:
//
// The code in this file defines the public C Field class and declares method
// signatures (prototypes).  The companion file {\tt ESMC\_Field.C} contains
// the definitions (full code bodies) for the Field methods.
//
//EOP
//-----------------------------------------------------------------------------

#if defined (__cplusplus)
extern "C" {
#endif

#include "ESMC_Mesh.h"
#include "ESMC_Array.h"
#include "ESMC_ArraySpec.h"
#include "ESMC_Interface.h"

// Class declaration type
typedef void * ESMC_Field;

// Class API

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_FieldCreate - Create a Field
//
// !INTERFACE:
ESMC_Field ESMC_FieldCreate(ESMC_Mesh mesh, ESMC_ArraySpec arrayspec,
  ESMC_InterfaceInt gridToFieldMap, ESMC_InterfaceInt ungriddedLBound,
  ESMC_InterfaceInt ungriddedUBound, const char *name, int *rc);
// !DESCRIPTION:
//
//  Creates a {\tt ESMC\_Field} object
//
//  The arguments are:
//  \begin{description}
//  \item[mesh]
//    A {\tt ESMC\_Mesh} object.
//  \item[arrayspec]
//    A {\tt ESMC\_ArraySpec} object describing data type and kind specification.
//  \item[gridToFieldMap]
//    List with number of elements equal to the grid's dimCount. The list
//    elements map each dimension of the grid to a dimension in the field by
//    specifying the appropriate field dimension index. The default is to map all of
//    the grid's dimensions against the lowest dimensions of the field in sequence,
//    i.e. gridToFieldMap = (/1,2,3,.../). The values of all gridToFieldMap entries
//    must be greater than or equal to one and smaller than or equal to the field
//    rank. It is erroneous to specify the same gridToFieldMap entry multiple times.
//    The total ungridded dimensions in the field  are the total field dimensions
//    less the dimensions in the grid. Ungridded dimensions must be in the same order
//    they are stored in the field. If the Field dimCount is less than the Mesh
//    dimCount then the default gridToFieldMap will contain zeros for the rightmost
//    entries. A zero entry in the gridToFieldMap indicates that the particular Mesh
//    dimension will be replicating the Field across the DEs along this direction.
//  \item[ungriddedLBound]
//    Lower bounds of the ungridded dimensions of the field. The number of elements
//    in the ungriddedLBound is equal to the number of ungridded dimensions in the
//    field. All ungridded dimensions of the field are also undistributed. When field
//    dimension count is greater than grid dimension count, both ungriddedLBound and
//    ungriddedUBound must be specified. When both are specified the values are
//    checked for consistency. Note that the the ordering of these ungridded
//    dimensions is the same as their order in the field.  
//  \item[ungriddedUBound]
//    Upper bounds of the ungridded dimensions of the field. The number of elements
//    in the ungriddedUBound is equal to the number of ungridded dimensions in the
//    field. All ungridded dimensions of the field are also undistributed. When field
//    dimension count is greater than grid dimension count, both ungriddedLBound and
//    ungriddedUBound must be specified. When both are specified the values are
//    checked for consistency. Note that the the ordering of these ungridded
//    dimensions is the same as their order in the field.  
//  \item[{[name]}]
//    The name for the newly created field.  If not specified, i.e. NULL,
//    a default unique name will be generated: "FieldNNN" where NNN
//    is a unique sequence number from 001 to 999.
//  \item[{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_FieldDestroy - Destroy a Field
//
// !INTERFACE:
int ESMC_FieldDestroy(ESMC_Field *field);
// !DESCRIPTION:
//
//  Releases all resources associated with this {\tt ESMC\_Field}.
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//
//  The arguments are:
//  \begin{description}
//  \item[field]
//    Destroy contents of this {\tt ESMC\_Field}.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_FieldPrint - Print the internal information of a Field
//
// !INTERFACE:
int ESMC_FieldPrint(ESMC_Field field);
// !DESCRIPTION:
//
//  Print the internal information within this {\tt ESMC\_Field}.
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//
//  The arguments are:
//  \begin{description}
//  \item[field]
//    Print contents of this {\tt ESMC\_Field}.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_FieldGetMesh - Get the internal Mesh stored in the Field
//
// !INTERFACE:
ESMC_Mesh ESMC_FieldGetMesh(ESMC_Field field, int *rc);
// !DESCRIPTION:
//
//  Get the internal Mesh stored in the {\tt ESMC\_Field}.
//
//  The arguments are:
//  \begin{description}
//  \item[field]
//    Get the internal Mesh stored in this {\tt ESMC\_Field}.
//  \item[{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_FieldGetArray - Get the internal Array stored in the Field
//
// !INTERFACE:
ESMC_Array ESMC_FieldGetArray(ESMC_Field field, int *rc);
// !DESCRIPTION:
//
//  Get the internal Array stored in the {\tt ESMC\_Field}.
//
//  The arguments are:
//  \begin{description}
//  \item[field]
//    Get the internal Array stored in this {\tt ESMC\_Field}.
//  \item[{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_FieldGetPtr - Get the internal Fortran data pointer stored in the Field
//
// !INTERFACE:
void *ESMC_FieldGetPtr(ESMC_Field field, int localDe, int *rc);
// !DESCRIPTION:
//
//  Get the internal Fortran data pointer stored in the {\tt ESMC\_Field}.
//
//  The arguments are:
//  \begin{description}
//  \item[field]
//    Get the internal Fortran data pointer stored in this {\tt ESMC\_Field}.
//  \item[{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------


#if defined (__cplusplus)
} // extern "C"
#endif

#endif
