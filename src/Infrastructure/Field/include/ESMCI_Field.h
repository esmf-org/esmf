// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research, 
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

#ifndef ESMCI_Field_H
#define ESMCI_Field_H

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_Field - one line general Fieldment about this class
//
// !DESCRIPTION:
//
// The code in this file defines the ESMC Field class prototypes for the
// Fortran interface routines. The companion file ESMC\_Field_C.F90  contains
// the definitions (full code bodies) for the interface routines.
//
//EOP 
//

//-----------------------------------------------------------------------------
// 
// !USES:
#include "ESMC_Field.h"
#include "ESMC_Mesh.h"
#include "ESMC_Array.h"
#include "ESMC_ArraySpec.h"
#include "ESMC_RHandle.h"
#include "ESMCI_RHandle.h"
#include "ESMCI_F90Interface.h"
#include "ESMC_Interface.h"
#include "ESMCI_Util.h"
#include "ESMC_Grid.h"
#include "ESMC_LocStream.h"

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// C++ Field class declaration
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
namespace ESMCI{

  class Field{
    // pointer to fortran derived type
    F90ClassHolder fortranclass;
    // methods
   public:
    Field(){}
    Field(F90ClassHolder fc){
      fortranclass = fc;
    }
    // TODO: ESMC objects should be cast to ESMCI objects in the ESMC layer
    static Field* create(ESMC_Grid *grid, ESMC_ArraySpec arrayspec,
      ESMC_StaggerLoc staggerloc, ESMC_InterArrayInt *gridToFieldMap, 
      ESMC_InterArrayInt *ungriddedLBound, ESMC_InterArrayInt *ungriddedUBound, 
      const char *name, int *rc); 
    static Field* create(ESMC_Grid *grid, ESMC_TypeKind_Flag typekind,
      ESMC_StaggerLoc staggerloc, ESMC_InterArrayInt *gridToFieldMap, 
      ESMC_InterArrayInt *ungriddedLBound, ESMC_InterArrayInt *ungriddedUBound, 
      const char *name, int *rc); 
    static Field* create(ESMC_Mesh mesh, ESMC_ArraySpec arrayspec,
      ESMC_InterArrayInt *gridToFieldMap, ESMC_InterArrayInt *ungriddedLBound,
      ESMC_InterArrayInt *ungriddedUBound, const char *name, int *rc); 
    static Field* create(ESMC_Mesh mesh, ESMC_TypeKind_Flag typekind,
      ESMC_MeshLoc_Flag meshloc, ESMC_InterArrayInt *gridToFieldMap, 
      ESMC_InterArrayInt *ungriddedLBound, ESMC_InterArrayInt *ungriddedUBound, 
      const char *name, int *rc); 
    static Field* create(ESMC_LocStream *locstream, ESMC_ArraySpec arrayspec,
      ESMC_InterArrayInt *gridToFieldMap, ESMC_InterArrayInt *ungriddedLBound,
      ESMC_InterArrayInt *ungriddedUBound, const char *name, int *rc); 
    static Field* create(ESMC_LocStream *locstream, ESMC_TypeKind_Flag typekind,
      ESMC_InterArrayInt *gridToFieldMap, 
      ESMC_InterArrayInt *ungriddedLBound, ESMC_InterArrayInt *ungriddedUBound, 
      const char *name, int *rc); 
    static int destroy(Field *field);
    ESMC_Mesh getMesh(int *rc);
    ESMC_Array getArray(int *rc);
    static int getbounds(Field *field, int *localDe,
      InterArray<int> *exclusiveLBound, InterArray<int> *exclusiveUBound);
    int print();
    int read(const char *file,
      const char* variableName,
      int timeslice, ESMC_IOFmt_Flag iofmt);
    int castToFortran(F90ClassHolder *fc);
    static int regridgetarea(Field *field);
    static int regridstore(Field *fieldsrc, Field *fielddst,
                           ESMC_InterArrayInt *srcMaskValues, 
                           ESMC_InterArrayInt *dstMaskValues,
                           RouteHandle **routehandle,
                           ESMC_RegridMethod_Flag *regridMethod,
                           ESMC_PoleMethod_Flag *polemethod,
                           int *regridPoleNPnts,
                           ESMC_LineType_Flag *lineType,
                           ESMC_NormType_Flag *normType,
                           ESMC_ExtrapMethod_Flag *extrapMethod,
                           int *extrapNumSrcPnts,
                           float *extrapDistExponent,
                           ESMC_UnmappedAction_Flag *unmappedAction,
                           ESMC_Logical *ignoreDegenerate,
                           double **factorList,
                           int **factorIndexList,
                           int *numFactors,
                           Field *srcFracField, Field *dstFracField);
    static int regridstorefile(Field *fieldsrc, Field *fielddst,
                           const char *filename,
                           ESMC_InterArrayInt *srcMaskValues,
                           ESMC_InterArrayInt *dstMaskValues,
                           RouteHandle **routehandle,
                           ESMC_RegridMethod_Flag *regridMethod,
                           ESMC_PoleMethod_Flag *polemethod,
                           int *regridPoleNPnts,
                           ESMC_LineType_Flag *lineType,
                           ESMC_NormType_Flag *normType,
                           ESMC_UnmappedAction_Flag *unmappedAction,
                           ESMC_Logical *ignoreDegenerate,
                           ESMC_Logical *create_rh,
                           Field *srcFracField, Field *dstFracField);
    static int regrid(Field *fieldsrc, Field *fielddst,
                      RouteHandle *routehandle, ESMC_Region_Flag *zeroRegion);
    static int regridrelease(RouteHandle *routehandle);
    static int regridreleasefactors(double **factorList, int **factorIndexList, int* numFactors);
    static int smmstore(Field *fieldsrc, Field *fielddst,
                        const char *filename, RouteHandle **routehandle,
                        ESMC_Logical *ignoreUnmatchedIndices,
                        int *srcTermProcessing, int *pipeLineDepth);
    int write(const char *file,
      const char* variableName,
      int overwrite,
      ESMC_FileStatus_Flag status,
      int timeslice, ESMC_IOFmt_Flag iofmt);
  }; 
}

#endif  // ESMCI_Field_H
