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

#ifndef ESMC_Field_h
#define ESMC_Field_h

//-----------------------------------------------------------------------------
// ESMC_Field - Public C interface to the ESMF Field class
//
// The code in this file defines the public C Field class and declares method
// signatures (prototypes).  The companion file {\tt ESMC\_Field.C} contains
// the definitions (full code bodies) for the Field methods.
//-----------------------------------------------------------------------------

#include "ESMC_Mesh.h"
#include "ESMC_Array.h"
#include "ESMC_ArraySpec.h"
#include "ESMC_RHandle.h"
#include "ESMC_Interface.h"
#include "ESMC_Grid.h"
#include "ESMC_Util.h"
#include "ESMC_LocStream.h"

#if defined (__cplusplus)
extern "C" {
#endif

// Class declaration type
typedef struct{
  void *ptr;
}ESMC_Field;

// Class API

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_FieldCreateGridArraySpec - Create a Field from Grid and ArraySpec
//
// !INTERFACE:
ESMC_Field ESMC_FieldCreateGridArraySpec(
  ESMC_Grid grid,                           // in
  ESMC_ArraySpec arrayspec,                 // in
  enum ESMC_StaggerLoc staggerloc,          // in
  ESMC_InterArrayInt *gridToFieldMap,       // in
  ESMC_InterArrayInt *ungriddedLBound,      // in
  ESMC_InterArrayInt *ungriddedUBound,      // in
  const char *name,                         // in
  int *rc                                   // out
);

// !RETURN VALUE:
//  Newly created ESMC_Field object.
//
// !DESCRIPTION:
//
//  Creates a {\tt ESMC\_Field} object.
//
//  The arguments are:
//  \begin{description}
//  \item[grid]
//    A {\tt ESMC\_Grid} object.
//  \item[arrayspec]
//    A {\tt ESMC\_ArraySpec} object describing data type and kind specification.
//  \item[staggerloc]
//    Stagger location of data in grid cells. The default value is 
//    ESMF\_STAGGERLOC\_CENTER.
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
// !IROUTINE: ESMC_FieldCreateGridTypeKind - Create a Field from Grid and typekind
//
// !INTERFACE:
ESMC_Field ESMC_FieldCreateGridTypeKind(
  ESMC_Grid grid,                           // in
  enum ESMC_TypeKind_Flag typekind,         // in
  enum ESMC_StaggerLoc staggerloc,          // in
  ESMC_InterArrayInt *gridToFieldMap,       // in
  ESMC_InterArrayInt *ungriddedLBound,      // in
  ESMC_InterArrayInt *ungriddedUBound,      // in
  const char *name,                         // in
  int *rc                                   // out
);

// !RETURN VALUE:
//  Newly created ESMC_Field object.
//
// !DESCRIPTION:
//
//  Creates a {\tt ESMC\_Field} object.
//
//  The arguments are:
//  \begin{description}
//  \item[grid]
//    A {\tt ESMC\_Grid} object.
//  \item[typekind]
//    The ESMC\_TypeKind\_Flag that describes this Field data.
//  \item[staggerloc]
//    Stagger location of data in grid cells. The default value is 
//    ESMF\_STAGGERLOC\_CENTER.
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

  // TODO: This interface fails when using the NULL (optional) for 
  //       gridToFieldMap and the ungriddedXBounds 
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_FieldCreateMeshArraySpec - Create a Field from Mesh and ArraySpec
//
// !INTERFACE:
ESMC_Field ESMC_FieldCreateMeshArraySpec(
  ESMC_Mesh mesh,                           // in
  ESMC_ArraySpec arrayspec,                 // in
  ESMC_InterArrayInt *gridToFieldMap,       // in
  ESMC_InterArrayInt *ungriddedLBound,      // in
  ESMC_InterArrayInt *ungriddedUBound,      // in
  const char *name,                         // in
  int *rc                                   // out
);

// !RETURN VALUE:
//  Newly created ESMC_Field object.
//
// !DESCRIPTION:
//
//  Creates a {\tt ESMC\_Field} object.
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
// !IROUTINE: ESMC_FieldCreateMeshTypeKind - Create a Field from Mesh and typekind
//
// !INTERFACE:
ESMC_Field ESMC_FieldCreateMeshTypeKind(
  ESMC_Mesh mesh,                           // in
  enum ESMC_TypeKind_Flag typekind,         // in
  enum ESMC_MeshLoc_Flag meshloc,           // in
  ESMC_InterArrayInt *gridToFieldMap,       // in
  ESMC_InterArrayInt *ungriddedLBound,      // in
  ESMC_InterArrayInt *ungriddedUBound,      // in
  const char *name,                         // in
  int *rc                                   // out
);

// !RETURN VALUE:
//  Newly created ESMC_Field object.
//
// !DESCRIPTION:
//
//  Creates a {\tt ESMC\_Field} object.
//
//  The arguments are:
//  \begin{description}
//  \item[mesh]
//    A {\tt ESMC\_Mesh} object.
//  \item[typekind]
//    The ESMC\_TypeKind\_Flag that describes this Field data.
//  \item[meshloc]
//    The ESMC\_MeshLoc\_Flag that describes this Field data.
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
// !IROUTINE: ESMC_FieldCreateLocStreamArraySpec - Create a Field from LocStream and ArraySpec
//
// !INTERFACE:
ESMC_Field ESMC_FieldCreateLocStreamArraySpec(
  ESMC_LocStream locstream,                 // in
  ESMC_ArraySpec arrayspec,                 // in
  ESMC_InterArrayInt *gridToFieldMap,       // in
  ESMC_InterArrayInt *ungriddedLBound,      // in
  ESMC_InterArrayInt *ungriddedUBound,      // in
  const char *name,                         // in
  int *rc                                   // out
);

// !RETURN VALUE:
//  Newly created ESMC_Field object.
//
// !DESCRIPTION:
//
//  Creates a {\tt ESMC\_Field} object.
//
//  The arguments are:
//  \begin{description}
//  \item[locstream]
//    A {\tt ESMC\_LocStream} object.
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
// !IROUTINE: ESMC_FieldCreateLocStreamTypeKind - Create a Field from LocStream and typekind
//
// !INTERFACE:
ESMC_Field ESMC_FieldCreateLocStreamTypeKind(
  ESMC_LocStream locstream,                 // in
  enum ESMC_TypeKind_Flag typekind,         // in
  ESMC_InterArrayInt *gridToFieldMap,       // in
  ESMC_InterArrayInt *ungriddedLBound,      // in
  ESMC_InterArrayInt *ungriddedUBound,      // in
  const char *name,                         // in
  int *rc                                   // out
);

// !RETURN VALUE:
//  Newly created ESMC_Field object.
//
// !DESCRIPTION:
//
//  Creates a {\tt ESMC\_Field} object.
//
//  The arguments are:
//  \begin{description}
//  \item[locstream]
//    A {\tt ESMC\_LocStream} object.
//  \item[typekind]
//    The ESMC\_TypeKind\_Flag that describes this Field data.
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
int ESMC_FieldDestroy(
  ESMC_Field *field     // inout
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
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
// !IROUTINE: ESMC_FieldGetArray - Get the internal Array stored in the Field
//
// !INTERFACE:
ESMC_Array ESMC_FieldGetArray(
  ESMC_Field field,     // in
  int *rc               // out
);

// !RETURN VALUE:
//  The ESMC_Array object stored in the ESMC_Field.
//
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
// !IROUTINE: ESMC_FieldGetMesh - Get the internal Mesh stored in the Field
//
// !INTERFACE:
ESMC_Mesh ESMC_FieldGetMesh(
  ESMC_Field field,     // in
  int *rc               // out
);

// !RETURN VALUE:
//  The ESMC_Mesh object stored in the ESMC_Field.
//
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
// !IROUTINE: ESMC_FieldGetPtr - Get the internal Fortran data pointer stored in the Field
//
// !INTERFACE:
void *ESMC_FieldGetPtr(
  ESMC_Field field,     // in
  int localDe,          // in
  int *rc               // out
);

// !RETURN VALUE:
//  The Fortran data pointer stored in the ESMC_Field.
//
// !DESCRIPTION:
//
//  Get the internal Fortran data pointer stored in the {\tt ESMC\_Field}.
//
//  The arguments are:
//  \begin{description}
//  \item[field]
//    Get the internal Fortran data pointer stored in this {\tt ESMC\_Field}.
//  \item[localDe]
//    Local DE for which information is requested. {\tt [0,..,localDeCount-1]}. 
//  \item[{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_FieldGetBounds - Get the Field bounds
//
// !INTERFACE:
int ESMC_FieldGetBounds(
  ESMC_Field field,      // in
  int *localDe,
  int *exclusiveLBound,
  int *exclusiveUBound,
  int rank
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Get the Field bounds from the {\tt ESMC\_Field}.
//
//  The arguments are:
//  \begin{description}
//  \item[field]
//    {\tt ESMC\_Field} whose bounds will be returned
//  \item[localDe]
//    The local DE of the {\tt ESMC\_Field} (not implemented)
//  \item[exclusiveLBound]
//    The exclusive lower bounds of the {\tt ESMC\_Field}
//  \item[exclusiveUBound]
//    The exclusive upper bounds of the {\tt ESMC\_Field}
//  \item[rank]
//    The rank of the {\tt ESMC\_Field}, to size the bounds arrays
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_FieldPrint - Print the internal information of a Field
//
// !INTERFACE:
int ESMC_FieldPrint(
  ESMC_Field field      // in
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Print the internal information within this {\tt ESMC\_Field}.
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
//BOPI
// !IROUTINE: ESMC_FieldRead - Read Array data into a Field
//
// !INTERFACE:
  int ESMC_FieldRead(ESMC_Field field, // inout
      const char *file,                // in
      const char *variableName,        // in
      int timeslice,                   // in
      ESMC_IOFmt_Flag iofmt            // in
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Read Field data from a file and put it into an {ESMF\_Field} object.
//  For this API to be functional, the environment variable {\tt ESMF\_PIO}
//  should be set to "internal" when the ESMF library is built.
//  Please see the section on Data I/O,~\ref{io:dataio}.
//
//  Limitations:
//  \begin{itemize}
//    \item Only 1 DE per PET supported.
//    \item Not supported in {\tt ESMF\_COMM=mpiuni} mode.
//  \end{itemize}
//
//  The arguments are:
//  \begin{description}
//  \item [field]
//    The {\tt ESMF\_Field} object in which the read data is returned.
//  \item[file]
//    The name of the file from which Field data is read.
//  \item[{[variableName]}]
//   Variable name in the file; default (NULL) is the "name" of Field.
//   Use this argument only in the I/O format (such as NetCDF) that
//   supports variable name. If the I/O format does not support this
//   (such as binary format), ESMF will return an error code.
//  \item[timeslice]
//    Number of slices to be read from file, starting from the 1st slice
//  \item[{[iofmt]}]
//    \begin{sloppypar}
//    The I/O format.  Please see Section~\ref{opt:iofmtflag} for the list
//    of options.  If set to NULL, defaults to {\tt ESMF\_IOFMT\_NETCDF}.
//    \end{sloppypar}
//  \end{description}
//
//EOPI
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_FieldRegridGetArea - Get the area of the cells used for 
//                                      conservative interpolation
//
// !INTERFACE:
int ESMC_FieldRegridGetArea(
  ESMC_Field field      // in
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//     This subroutine gets the area of the cells used for conservative interpolation for the grid object 
//     associated with {\tt areaField} and puts them into {\tt areaField}. If created on a 2D Grid, it must 
//     be built on the {\tt ESMF\_STAGGERLOC\_CENTER} stagger location. 
//     If created on a 3D Grid, it must be built on the {\tt ESMF\_STAGGERLOC\_CENTER\_VCENTER} stagger 
//     location. If created on a Mesh, it must be built on the {\tt ESMF\_MESHLOC\_ELEMENT} mesh location. 
//
//     The arguments are:
//     \begin{description}
//     \item [areaField]
//           The Field to put the area values in. 
//     \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_FieldRegridStore - Precompute a Field regridding operation and return a RouteHandle
//
// !INTERFACE:
int ESMC_FieldRegridStore( 
    ESMC_Field srcField,                           // in
    ESMC_Field dstField,                           // in
    ESMC_InterArrayInt *srcMaskValues,             // in
    ESMC_InterArrayInt *dstMaskValues,             // in
    ESMC_RouteHandle *routehandle,                 // inout
    enum ESMC_RegridMethod_Flag *regridmethod,     // in
    enum ESMC_PoleMethod_Flag *polemethod,         // in
    int *regridPoleNPnts,                          // in
    enum ESMC_LineType_Flag *lineType,             // in
    enum ESMC_NormType_Flag *normType,             // in
    enum ESMC_ExtrapMethod_Flag *extrapMethod,     // in
    int *extrapNumSrcPnts,                         // in
    float *extrapDistExponent,                     // in
    enum ESMC_UnmappedAction_Flag *unmappedaction, // in
    enum ESMC_Logical *ignoreDegenerate,           // in
    double **factorList,                           // inout
    int **factorIndexList,                         // inout
    int *numFactors,                               // inout
    ESMC_Field *srcFracField,                      // inout
    ESMC_Field *dstFracField);                     // inout

// !RETURN VALUE:
//   Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//   Creates a sparse matrix operation (stored in routehandle) that contains 
//   the calculations and communications necessary to interpolate from srcField 
//   to dstField. The routehandle can then be used in the call ESMC\_FieldRegrid() 
//   to interpolate between the Fields. 
//
//  The arguments are:
//  \begin{description}
//  \item[srcField]
//    ESMC\_Field with source data.
//  \item[dstField]
//    ESMC\_Field with destination data.
//  \item[srcMaskValues]
//    List of values that indicate a source point should be masked out. 
//    If not specified, no masking will occur.
//  \item[dstMaskValues]
//    List of values that indicate a destination point should be masked out. 
//    If not specified, no masking will occur.
//  \item[routehandle]
//    The handle that implements the regrid, to be used in {\tt ESMC\_FieldRegrid()}.
//  \item[regridmethod]
//    The type of interpolation. If not specified, defaults to {\tt ESMF\_REGRIDMETHOD\_BILINEAR}.
//  \item [polemethod]
//    Which type of artificial pole
//    to construct on the source Grid for regridding. 
//    If not specified, defaults to {\tt ESMF\_POLEMETHOD\_ALLAVG} for non-conservative regrid methods, 
//    and {\tt ESMF\_POLEMETHOD\_NONE} for conservative methods. 
//    If not specified, defaults to {\tt ESMC\_POLEMETHOD\_ALLAVG}. 
//  \item [regridPoleNPnts]
//    If {\tt polemethod} is {\tt ESMC\_POLEMETHOD\_NPNTAVG}.
//    This parameter indicates how many points should be averaged
//    over. Must be specified if {\tt polemethod} is 
//    {\tt ESMC\_POLEMETHOD\_NPNTAVG}.
//  \item [{[lineType]}]
//    This argument controls the path of the line which connects two points on a sphere surface. This in
//    turn controls the path along which distances are calculated and the shape of the edges that make
//    up a cell. Both of these quantities can influence how interpolation weights are calculated.
//    As would be expected, this argument is only applicable when {\tt srcField} and {\tt dstField} are
//    built on grids which lie on the surface of a sphere. Section~\ref{opt:lineType} shows a
//    list of valid options for this argument. If not specified, the default depends on the
//    regrid method. Section~\ref{opt:lineType} has the defaults by line type.
//  \item[normType]
//    This argument controls the type of normalization used when generating conservative weights.
//    This option only applies to weights generated with {\tt regridmethod=ESMF\_REGRIDMETHOD\_CONSERVE}.
//    If not specified normType defaults to {\tt ESMF\_NORMTYPE\_DSTAREA}.
//  \item [{[extrapMethod]}]
//    The type of extrapolation. Please see Section~\ref{opt:cextrapmethod} 
//    for a list of valid options. If not specified, defaults to 
//    {\tt ESMC\_EXTRAPMETHOD\_NONE}.
//  \item [{[extrapNumSrcPnts]}] 
//    The number of source points to use for the extrapolation methods that use more than one source point 
//    (e.g. {\tt ESMC\_EXTRAPMETHOD\_NEAREST\_IDAVG}). If not specified, defaults to 8.
//  \item [{[extrapDistExponent]}] 
//    The exponent to raise the distance to when calculating weights for 
//    the {\tt ESMC\_EXTRAPMETHOD\_NEAREST\_IDAVG} extrapolation method. A higher value reduces the influence 
//    of more distant points. If not specified, defaults to 2.0.
//  \item[unmappedaction]
//    Specifies what should happen if there are destination points that can't 
//    be mapped to a source cell. Options are {\tt ESMF\_UNMAPPEDACTION\_ERROR} or
//    {\tt ESMF\_UNMAPPEDACTION\_IGNORE}. If not specified, defaults to {\tt ESMF\_UNMAPPEDACTION\_ERROR}.
//  \item [{[factorList]}] 
//    The list of coefficients for a sparse matrix which interpolates from {\tt srcField} to 
//    {\tt dstField}. The array coming out of this variable is in the appropriate format to be used
//    in other ESMF sparse matrix multiply calls, for example {\tt ESMC\_FieldSMMStore()}. 
//    The {\tt factorList} array is allocated by the method and the user is responsible for 
//    deallocating it. 
//  \item [{[factorIndexList]}] 
//    The indices for a sparse matrix which interpolates from {\tt srcField} to 
//    {\tt dstField}. This argument is a 2D array containing pairs of source and destination
//    sequence indices corresponding to the coefficients in the {\tt factorList} argument. 
//    The first dimension of {\tt factorIndexList} is of size 2. {\tt factorIndexList(1,:)} specifies 
//    the sequence index of the source element in the {\tt srcField}. {\tt factorIndexList(2,:)} specifies 
//    the sequence index of the destination element in the {\tt dstField}. The second dimension of 
//    {\tt factorIndexList} steps through the list of pairs, i.e. {\tt size(factorIndexList,2)==size(factorList)}.
//    The array coming out of this variable is in the appropriate format to be used
//    in other ESMF sparse matrix multiply calls, for example {\tt ESMC\_FieldSMMStore()}. 
//    The {\tt factorIndexList} array is allocated by the method and the user is responsible for deallocating it. 
//  \item [{[numFactors]}] 
//    The number of factors returned in {\tt factorList}.
//  \item [{[srcFracField]}] 
//    The fraction of each source cell participating in the regridding. Only 
//    valid when regridmethod is {\tt ESMC\_REGRIDMETHOD\_CONSERVE}.
//    This Field needs to be created on the same location (e.g staggerloc) 
//    as the srcField.
//  \item [{[dstFracField]}] 
//    The fraction of each destination cell participating in the regridding. Only 
//    valid when regridmethod is {\tt ESMF\_REGRIDMETHOD\_CONSERVE}.
//    This Field needs to be created on the same location (e.g staggerloc) 
//    as the dstField.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_FieldRegridStoreFile - Precompute a Field regridding operation and return a RouteHandle
//
// !INTERFACE:
int ESMC_FieldRegridStoreFile(
    ESMC_Field srcField,                           // in
    ESMC_Field dstField,                           // in
    const char *filename,                          // in
    ESMC_InterArrayInt *srcMaskValues,              // in
    ESMC_InterArrayInt *dstMaskValues,              // in
    ESMC_RouteHandle *routehandle,                 // inout
    enum ESMC_RegridMethod_Flag *regridmethod,     // in
    enum ESMC_PoleMethod_Flag *polemethod,         // in
    int *regridPoleNPnts,                          // in
    enum ESMC_LineType_Flag *lineType,             // in
    enum ESMC_NormType_Flag *normType,             // in
    enum ESMC_UnmappedAction_Flag *unmappedaction, // in
    enum ESMC_Logical *ignoreDegenerate,           // in
    enum ESMC_Logical *create_rh,                  // in
    ESMC_Field *srcFracField,                      // out
    ESMC_Field *dstFracField);                     // out

// !RETURN VALUE:
//   Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//   Creates a sparse matrix operation (stored in routehandle) that contains
//   the calculations and communications necessary to interpolate from srcField
//   to dstField. The routehandle can then be used in the call ESMC\_FieldRegrid()
//   to interpolate between the Fields. The weights will be output to the file
//   with name {\tt filename}.
//
//  The arguments are:
//  \begin{description}
//  \item[srcField]
//    ESMC\_Field with source data.
//  \item[dstField]
//    ESMC\_Field with destination data.
//  \item[{[filename]}]
//    The output filename for the factorList and factorIndexList.
//  \item[{[srcMaskValues]}]
//    List of values that indicate a source point should be masked out.
//    If not specified, no masking will occur.
//  \item[{[dstMaskValues]}]
//    List of values that indicate a destination point should be masked out.
//    If not specified, no masking will occur.
//  \item[{[routehandle]}]
//    The handle that implements the regrid, to be used in {\tt ESMC\_FieldRegrid()}.
//  \item[{[regridmethod]}]
//    The type of interpolation. If not specified, defaults to {\tt ESMC\_REGRIDMETHOD\_BILINEAR}.
//  \item [{[polemethod]}]
//    Which type of artificial pole
//    to construct on the source Grid for regridding.
//    If not specified, defaults to {\tt ESMC\_POLEMETHOD\_ALLAVG} for non-conservative regrid methods,
//    and {\tt ESMC\_POLEMETHOD\_NONE} for conservative methods.
//    If not specified, defaults to {\tt ESMC\_POLEMETHOD\_ALLAVG}.
//  \item [{[regridPoleNPnts]}]
//    If {\tt polemethod} is {\tt ESMC\_POLEMETHOD\_NPNTAVG}.
//    This parameter indicates how many points should be averaged
//    over. Must be specified if {\tt polemethod} is
//    {\tt ESMC\_POLEMETHOD\_NPNTAVG}.
//  \item [{[lineType]}]
//    This argument controls the path of the line which connects two points on a sphere surface. This in
//    turn controls the path along which distances are calculated and the shape of the edges that make
//    up a cell. Both of these quantities can influence how interpolation weights are calculated.
//    As would be expected, this argument is only applicable when {\tt srcField} and {\tt dstField} are
//    built on grids which lie on the surface of a sphere. Section~\ref{opt:lineType} shows a
//    list of valid options for this argument. If not specified, the default depends on the
//    regrid method. Section~\ref{opt:lineType} has the defaults by line type.
//  \item[{[normType]}]
//    This argument controls the type of normalization used when generating conservative weights.
//    This option only applies to weights generated with {\tt regridmethod=ESMC\_REGRIDMETHOD\_CONSERVE}.
//    If not specified normType defaults to {\tt ESMC\_NORMTYPE\_DSTAREA}.
//  \item[{[unmappedaction]}]
//    Specifies what should happen if there are destination points that can't
//    be mapped to a source cell. Options are {\tt ESMC\_UNMAPPEDACTION\_ERROR} or
//    {\tt ESMC\_UNMAPPEDACTION\_IGNORE}. If not specified, defaults to {\tt ESMC\_UNMAPPEDACTION\_ERROR}.
//  \item[{create\_rh}]
//    Specifies whether or not to create a routehandle, or just write weights to file.
//    If not specified, defaults to {\tt ESMF\_TRUE}.
//  \item [{[srcFracField]}]
//    The fraction of each source cell participating in the regridding. Only
//    valid when regridmethod is {\tt ESMC\_REGRIDMETHOD\_CONSERVE}.
//    This Field needs to be created on the same location (e.g staggerloc)
//    as the srcField.
//  \item [{[dstFracField]}]
//    The fraction of each destination cell participating in the regridding. Only
//    valid when regridmethod is {\tt ESMC\_REGRIDMETHOD\_CONSERVE}.
//    This Field needs to be created on the same location (e.g staggerloc)
//    as the dstField.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_FieldRegrid - Compute a regridding operation
//
// !INTERFACE:
  int ESMC_FieldRegrid( 
    ESMC_Field srcField,                  // in
    ESMC_Field dstField,                  // inout
    ESMC_RouteHandle routehandle,         // in
    enum ESMC_Region_Flag *zeroregion);   // in

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Execute the precomputed regrid operation stored in routehandle to interpolate 
//  from srcField to dstField. See ESMF\_FieldRegridStore() on how to precompute
//  the routehandle.  It is erroneous to specify the identical Field object for
//  srcField and dstField arguments.  This call is collective across the 
//  current VM.
//
//  The arguments are:
//  \begin{description}
//  \item[srcField]
//    ESMC\_Field with source data.
//  \item[dstField]
//    ESMC\_Field with destination data.
//  \item[routehandle]
//    Handle to the precomputed Route.
//  \item [{[zeroregion]}]
//    \begin{sloppypar}
//    If set to {\tt ESMC\_REGION\_TOTAL} {\em (default)} the total regions of
//    all DEs in {\tt dstField} will be initialized to zero before updating the 
//    elements with the results of the sparse matrix multiplication. If set to
//    {\tt ESMC\_REGION\_EMPTY} the elements in {\tt dstField} will not be
//    modified prior to the sparse matrix multiplication and results will be
//    added to the incoming element values. Setting {\tt zeroregion} to 
//    {\tt ESMC\_REGION\_SELECT} will only zero out those elements in the 
//    destination Array that will be updated by the sparse matrix
//    multiplication.
//    \end{sloppypar}
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_FieldRegridRelease - Free resources used by a regridding operation
//
// !INTERFACE:
  int ESMC_FieldRegridRelease(ESMC_RouteHandle *routehandle);  // inout

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Free resources used by regrid object
//
//  The arguments are:
//  \begin{description}
//  \item[routehandle]
//    Handle carrying the sparse matrix
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE: ESMC_FieldRegridReleaseFactors - Free resources allocated for weights
//
// !INTERFACE:
  int ESMC_FieldRegridReleaseFactors(double **factorList, int **factorIndexList, int* numFactors);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Free resources allocated for regridding weights.
//
//  The arguments are:
//  \begin{description}
//  \item[factorList]
//    Pointer to the factorList to be deallocated
//  \item[factorIndexList]
//    Pointer to the factorIndexList to be deallocated
//  \item[numFactors]
//    Size of factorList
//  \end{description}
//
//EOPI
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_FieldSMMStore - Precompute a Field regridding operation and return a RouteHandle
//
// !INTERFACE:
int ESMC_FieldSMMStore(
    ESMC_Field srcField,                           // in
    ESMC_Field dstField,                           // in
    const char *filename,                          // in
    ESMC_RouteHandle *routehandle,                 // out
    enum ESMC_Logical *ignoreUnmatchedIndices,     // in
    int *srcTermProcessing,                        // in
    int *pipeLineDepth);                           // in

// !RETURN VALUE:
//   Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//   Creates a sparse matrix operation (stored in routehandle) that contains
//   the calculations and communications necessary to interpolate from srcField
//   to dstField. The routehandle can then be used in the call ESMC\_FieldRegrid()
//   to interpolate between the Fields.
//
//  The arguments are:
//  \begin{description}
//  \item[srcField]
//    ESMC\_Field with source data.
//  \item[dstField]
//    ESMC\_Field with destination data.
//  \item [filename]
//    Path to the file containing weights for creating an {\tt ESMC\_RouteHandle}.
//    Only "row", "col", and "S" variables are required. They
//    must be one-dimensionsal with dimension "n\_s".
//  \item[routehandle]
//    The handle that implements the regrid, to be used in {\tt ESMC\_FieldRegrid()}.
//  \item [{[ignoreUnmatchedIndices]}]
//    A logical flag that affects the behavior for when sequence indices
//    in the sparse matrix are encountered that do not have a match on the
//    {\tt srcField} or {\tt dstField} side. The default setting is
//    {\tt .false.}, indicating that it is an error when such a situation is
//    encountered. Setting {\tt ignoreUnmatchedIndices} to {\tt .true.} ignores
//    entries with unmatched indices.
//  \item [{[srcTermProcessing]}]
//    The {\tt srcTermProcessing} parameter controls how many source terms,
//    located on the same PET and summing into the same destination element,
//    are summed into partial sums on the source PET before being transferred
//    to the destination PET. A value of 0 indicates that the entire arithmetic
//    is done on the destination PET; source elements are neither multiplied
//    by their factors nor added into partial sums before being sent off by the
//    source PET. A value of 1 indicates that source elements are multiplied
//    by their factors on the source side before being sent to the destination
//    PET. Larger values of {\tt srcTermProcessing} indicate the maximum number
//    of terms in the partial sums on the source side.
//    Note that partial sums may lead to bit-for-bit differences in the results.
//    See section \ref{RH:bfb} for an in-depth discussion of {\em all}
//    bit-for-bit reproducibility aspects related to route-based communication
//    methods.
//    The {\tt ESMC\_FieldSMMStore()} method implements an auto-tuning scheme
//    for the {\tt srcTermProcessing} parameter. The intent on the
//    {\tt srcTermProcessing} argument is "{\tt inout}" in order to
//    support both overriding and accessing the auto-tuning parameter.
//    If an argument $>= 0$ is specified, it is used for the
//    {\tt srcTermProcessing} parameter, and the auto-tuning phase is skipped.
//    In this case the {\tt srcTermProcessing} argument is not modified on
//    return. If the provided argument is $< 0$, the {\tt srcTermProcessing}
//    parameter is determined internally using the auto-tuning scheme. In this
//    case the {\tt srcTermProcessing} argument is re-set to the internally
//    determined value on return. Auto-tuning is also used if the optional
//    {\tt srcTermProcessing} argument is omitted.
//  \item [{[pipelineDepth]}]
//    The {\tt pipelineDepth} parameter controls how many messages a PET
//    may have outstanding during a sparse matrix exchange. Larger values
//    of {\tt pipelineDepth} typically lead to better performance. However,
//    on some systems too large a value may lead to performance degradation,
//    or runtime errors.
//    Note that the pipeline depth has no effect on the bit-for-bit
//    reproducibility of the results. However, it may affect the performance
//    reproducibility of the exchange.
//    The {\tt ESMC\_FieldSMMStore()} method implements an auto-tuning scheme
//    for the {\tt pipelineDepth} parameter. The intent on the
//    {\tt pipelineDepth} argument is "{\tt inout}" in order to
//    support both overriding and accessing the auto-tuning parameter.
//    If an argument $>= 0$ is specified, it is used for the
//    {\tt pipelineDepth} parameter, and the auto-tuning phase is skipped.
//    In this case the {\tt pipelineDepth} argument is not modified on
//    return. If the provided argument is $< 0$, the {\tt pipelineDepth}
//    parameter is determined internally using the auto-tuning scheme. In this
//    case the {\tt pipelineDepth} argument is re-set to the internally
//    determined value on return. Auto-tuning is also used if the optional
//    {\tt pipelineDepth} argument is omitted.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE: ESMC_FieldWrite - Write Field
//
// !INTERFACE:
  int ESMC_FieldWrite(ESMC_Field field,  // inout
      const char *file,                  // in
      const char *variableName,          // in
      int overwrite,                     // in
      ESMC_FileStatus_Flag status,       // in
      int timeslice,                     // in
      ESMC_IOFmt_Flag iofmt              // in
  );
//
// !DESCRIPTION:
//  Write Field data into a file.  For this API to be functional, the 
//  environment variable {\tt ESMF\_PIO} should be set to "internal" when 
//  the ESMF library is built.  Please see the section on 
//  Data I/O,~\ref{io:dataio}.
//
//  Limitations:
//  \begin{itemize}
//    \item Only 1 DE per PET supported.
//    \item Not supported in {\tt ESMF\_COMM=mpiuni} mode.
//  \end{itemize}
//
//  The arguments are:
//  \begin{description}
//  \item [field]
//    The {\tt ESMF\_Field} object that contains data to be written.
//  \item[file]
//    The name of the output file to which Field data is written.
//  \item[{[variableName]}]
//   Variable name in the output file; default is the "name" of field.
//   Use this argument only in the I/O format (such as NetCDF) that
//   supports variable name. If the I/O format does not support this
//   (such as binary format), ESMF will return an error code.
//  \item[{[overwrite]}]
//   \begin{sloppypar}
//     A logical flag, the default is .false., i.e., existing field data may
//     {\em not} be overwritten. If .true., the overwrite behavior depends
//     on the value of {\tt iofmt} as shown below:
//   \begin{description}
//   \item[{\tt iofmt} = {\tt ESMF\_IOFMT\_BIN}:]\ All data in the file will
//     be overwritten with each field's data.
//   \item[{\tt iofmt} = {\tt ESMF\_IOFMT\_NETCDF}:]\ Only the
//     data corresponding to each field's name will be
//     be overwritten. If the {\tt timeslice} option is given, only data for
//     the given timeslice may be overwritten.
//     Note that it is always an error to attempt to overwrite a NetCDF
//     variable with data which has a different shape.
//   \end{description}
//   \end{sloppypar}
//  \item[{[status]}]
//   \begin{sloppypar}
//   The file status. Please see Section~\ref{const:filestatusflag} for
//   the list of options. If not present, defaults to
//   {\tt ESMF\_FILESTATUS\_UNKNOWN}.
//   \end{sloppypar}
//  \item[{[timeslice]}]
//   \begin{sloppypar}
//   Some I/O formats (e.g. NetCDF) support the output of data in form of
//   time slices. The {\tt timeslice} argument provides access to this
//   capability. {\tt timeslice} must be positive. The behavior of this
//   option may depend on the setting of the {\tt overwrite} flag:
//   \begin{description}
//   \item[{\tt overwrite = .false.}:]\ If the timeslice value is
//   less than the maximum time already in the file, the write will fail.
//   \item[{\tt overwrite = .true.}:]\ Any positive timeslice value is valid.
//   \end{description}
//   By default, i.e. by omitting the {\tt timeslice} argument, no
//   provisions for time slicing are made in the output file,
//   however, if the file already contains a time axis for the variable,
//   a timeslice one greater than the maximum will be written.
//   \end{sloppypar}
//  \item[{[iofmt]}]
//    \begin{sloppypar}
//    The I/O format. Please see Section~\ref{opt:iofmtflag} for the list
//    of options. If not present, defaults to {\tt ESMF\_IOFMT\_NETCDF}.
//    \end{sloppypar}
//  \item [{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOPI

#if defined (__cplusplus)
} // extern "C"
#endif

#endif
