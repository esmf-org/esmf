// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2013, University Corporation for Atmospheric Research, 
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
  ESMC_InterfaceInt *gridToFieldMap,         // in
  ESMC_InterfaceInt *ungriddedLBound,        // in
  ESMC_InterfaceInt *ungriddedUBound,        // in
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
  enum ESMC_TypeKind_Flag typekind,              // in
  enum ESMC_StaggerLoc staggerloc,          // in
  ESMC_InterfaceInt *gridToFieldMap,         // in
  ESMC_InterfaceInt *ungriddedLBound,        // in
  ESMC_InterfaceInt *ungriddedUBound,        // in
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
  ESMC_InterfaceInt *gridToFieldMap,         // in
  ESMC_InterfaceInt *ungriddedLBound,        // in
  ESMC_InterfaceInt *ungriddedUBound,        // in
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
  enum ESMC_TypeKind_Flag typekind,              // in
  enum ESMC_MeshLoc_Flag meshloc,           // in
  ESMC_InterfaceInt *gridToFieldMap,         // in
  ESMC_InterfaceInt *ungriddedLBound,        // in
  ESMC_InterfaceInt *ungriddedUBound,        // in
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
    ESMC_Field srcField,                       // in
    ESMC_Field dstField,                       // in
    ESMC_InterfaceInt *srcMaskValues,          // in
    ESMC_InterfaceInt *dstMaskValues,          // in
    ESMC_RouteHandle *routehandle,             // inout
    enum ESMC_RegridMethod_Flag *regridmethod,       // in
    ESMC_PoleMethod_Flag *polemethod,          // in
    int *regridPoleNPnts,                      // in
    enum ESMC_UnmappedAction_Flag *unmappedaction,   // in
    ESMC_Field *srcFracField,                  // out
    ESMC_Field *dstFracField);                 // out

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
//    The handle that implements the regrid, to be used in ESMC\_FieldRegrid().
//  \item[regridmethod]
//    The type of interpolation. If not specified, defaults to ESMF\_REGRIDMETHOD\_BILINEAR.
//  \item [polemethod]
//    Which type of artificial pole
//    to construct on the source Grid for regridding. 
//    If not specified, defaults to {\tt ESMC\_POLEMETHOD\_ALLAVG}. 
//  \item [regridPoleNPnts]
//    If {\tt polemethod} is {\tt ESMC\_POLEMETHOD\_NPNTAVG}.
//    This parameter indicates how many points should be averaged
//    over. Must be specified if {\tt polemethod} is 
//    {\tt ESMC\_POLEMETHOD\_NPNTAVG}.
//  \item[unmappedaction]
//    Specifies what should happen if there are destination points that can't 
//    be mapped to a source cell. Options are ESMF\_UNMAPPEDACTION\_ERROR or 
//    ESMF\_UNMAPPEDACTION\_IGNORE. If not specified, defaults to ESMF\_UNMAPPEDACTION\_ERROR.
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
// !IROUTINE: ESMC_FieldRegrid - Compute a regridding operation
//
// !INTERFACE:
  int ESMC_FieldRegrid( 
    ESMC_Field srcField,                // in
    ESMC_Field dstField,                // inout
    ESMC_RouteHandle routehandle,       // in
    enum ESMC_Region_Flag *zeroregion);  // in

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


#if defined (__cplusplus)
} // extern "C"
#endif

#endif
