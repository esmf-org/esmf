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

#ifndef ESMCI_XGrid_H
#define ESMCI_XGrid_H

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_XGrid - one line general XGridment about this class
//
// !DESCRIPTION:
//
// The code in this file defines the ESMC XGrid class prototypes for the
// Fortran interface routines. The companion file ESMC\_XGrid_C.F90  contains
// the definitions (full code bodies) for the interface routines.
//
//EOP 
//

//-----------------------------------------------------------------------------
// 
// !USES:
#include "ESMC_XGrid.h"
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
// C++ XGrid class declaration
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
namespace ESMCI{

  class XGrid{
    // pointer to fortran derived type
    F90ClassHolder fortranclass;
    // methods
   public:
    XGrid(){}
    XGrid(F90ClassHolder fc){
      fortranclass = fc;
    }

    // TODO: ESMC objects should be cast to ESMCI objects in the ESMC layer    
    static XGrid *create(                         
                         int sideAGridCount,  ESMC_Grid *sideAGrid, 
                         int sideAMeshCount,  ESMC_Mesh *sideAMesh, 
                         int sideBGridCount,  ESMC_Grid *sideBGrid, 
                         int sideBMeshCount,  ESMC_Mesh *sideBMesh, 
                         ESMC_InterArrayInt *sideAGridPriority, 
                         ESMC_InterArrayInt *sideAMeshPriority, 
                         ESMC_InterArrayInt *sideBGridPriority, 
                         ESMC_InterArrayInt *sideBMeshPriority, 
                         ESMC_InterArrayInt *sideAMaskValues, 
                         ESMC_InterArrayInt *sideBMaskValues, 
                         int storeOverlay, 
                         int *rc);

    static int destroy(XGrid *xgrid);

    int getSideAGridCount(int *rc);
    int getSideAMeshCount(int *rc);
    int getSideBGridCount(int *rc);
    int getSideBMeshCount(int *rc);
    int getDimCount(int *rc);
    int getElementCount(int *rc);
    ESMC_Mesh getMesh(int *rc);
    void getArea(ESMC_R8 *area, int *rc);
    void getCentroid(ESMC_R8 *centroid, int *rc);
    void getSparseMatA2X(
                         int sideAIndex, 
                         int *factorListCount,
                         double **factorList, 
                         int **factorIndexList,
                         int *rc);

    void getSparseMatX2A(
                         int sideAIndex, 
                         int *factorListCount,
                         double **factorList, 
                         int **factorIndexList,
                         int *rc);

    void getSparseMatB2X(
                         int sideBIndex, 
                         int *factorListCount,
                         double **factorList, 
                         int **factorIndexList,
                         int *rc);

    void getSparseMatX2B(
                         int sideBIndex, 
                         int *factorListCount,
                         double **factorList, 
                         int **factorIndexList,
                         int *rc);




    // BEGIN OLD STUFF
#if 0
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
#endif
// END OLD STUFF

  }; 
}

#endif  // ESMCI_Field_H
