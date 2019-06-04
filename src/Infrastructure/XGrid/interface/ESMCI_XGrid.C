//$1.10 2007/04/26 16:13:59 rosalind Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_XGrid.C"
//==============================================================================
//
// ESMC XGrid method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt Field} methods declared
// in the companion file {\tt ESMCI\_Field.h}.  These are wrappers for the
// actual code which is implemented in F90.
//
//-----------------------------------------------------------------------------
// associated header file
#include "ESMCI_XGrid.h"

//insert any higher level, 3rd party or system includes here
#include <string>         // strlen()

// ESMF headers
#include "ESMCI_LogErr.h"
#include "ESMCI_Array.h"
#include "ESMCI_Grid.h"

//-----------------------------------------------------------------------------
//BOP
// !CLASS: XGrid object
//
// !DESCRIPTION:
//  XGrid class which provides interfaces to the Fortran implementation
//    of Fields.
//EOP
//-----------------------------------------------------------------------------

// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// prototypes for the fortran interface routines.
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
extern "C" {
// Prototypes of the Fortran interface functions.

  void FTN_X(f_esmf_xgridcreate)(ESMCI::XGrid *xgrid,  
                                 int *num_sideAGrid, ESMCI::Grid **sideAGrid,  
                                 int *num_sideAMesh, void **sideAMesh,  
                                 int *num_sideBGrid, ESMCI::Grid **sideBGrid,  
                                 int *num_sideBMesh, void **sideBMesh,  
                                 int *num_sideAGridPriority, int *sideAGridPriority,  
                                 int *num_sideAMeshPriority, int *sideAMeshPriority,  
                                 int *num_sideBGridPriority, int *sideBGridPriority,  
                                 int *num_sideBMeshPriority, int *sideBMeshPriority,  
                                 int *num_sideAMaskValues, int *sideAMaskValues,  
                                 int *num_sideBMaskValues, int *sideBMaskValues,  
                                 int *storeOverlay,  
                                 int *rc);


  void FTN_X(f_esmf_xgriddestroy)(ESMCI::XGrid *xgridp, int *rc);

  void FTN_X(f_esmf_xgridgetsideagridcount)(ESMCI::XGrid *xgridp, int *count, int *rc);

  void FTN_X(f_esmf_xgridgetsideameshcount)(ESMCI::XGrid *xgridp, int *count, int *rc);

  void FTN_X(f_esmf_xgridgetsidebgridcount)(ESMCI::XGrid *xgridp, int *count, int *rc);

  void FTN_X(f_esmf_xgridgetsidebmeshcount)(ESMCI::XGrid *xgridp, int *count, int *rc);

  void FTN_X(f_esmf_xgridgetdimcount)(ESMCI::XGrid *xgridp, int *count, int *rc);

  void FTN_X(f_esmf_xgridgetelementcount)(ESMCI::XGrid *xgridp, int *count, int *rc);

  void FTN_X(f_esmf_xgridgetmesh)(ESMCI::XGrid *xgridp, void **mesh_pointer, int *parametricDim, 
                                  int *spatialDim, ESMC_CoordSys_Flag *coordSys, int *rc);

  void FTN_X(f_esmf_xgridgetarea)(ESMCI::XGrid *xgridp, int *num_area, ESMC_R8 *area, int *rc);

  void FTN_X(f_esmf_xgridgetcentroid)(ESMCI::XGrid *xgridp, int *elemCount, int*dimCount, ESMC_R8 *centroid, int *rc);


  void FTN_X(f_esmf_xgridgetsparsemata2x)(ESMCI::XGrid *xgridp, int *sideAIndex, 
                                          int *factorListCount, 
                                          double **factorList, 
                                          int **factorIndexList, 
                                          int *rc);

  void FTN_X(f_esmf_xgridgetsparsematx2a)(ESMCI::XGrid *xgridp, int *sideAIndex, 
                                          int *factorListCount, 
                                          double **factorList, 
                                          int **factorIndexList, 
                                          int *rc);

  void FTN_X(f_esmf_xgridgetsparsematb2x)(ESMCI::XGrid *xgridp, int *sideBIndex, 
                                          int *factorListCount, 
                                          double **factorList, 
                                          int **factorIndexList, 
                                          int *rc);

  void FTN_X(f_esmf_xgridgetsparsematx2b)(ESMCI::XGrid *xgridp, int *sideBIndex, 
                                          int *factorListCount, 
                                          double **factorList, 
                                          int **factorIndexList, 
                                          int *rc);


}

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Field routines
//
//

namespace ESMCI {
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XGrid::create()"
//BOP
// !IROUTINE:  ESMCI::XGrid::create - Create a new XGrid
//
// !INTERFACE:
      XGrid *XGrid::create(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMCI::XGrid object
//
// !ARGUMENTS:
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
                           int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Create a new XGrid
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC\_Field.h)
//
//EOP
    // Initialize return code. Assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;
    if(rc!=NULL) *rc=ESMC_RC_NOT_IMPL;

    // Temp memory, so Fortran doesn't get messed up when 
    // there is nothing in array
    int tmp_int_mem;

    // prepare the side A Grid info
    int cpp_sideAGridCount=0;
    ESMCI::Grid **cpp_sideAGrid;
    if ((sideAGridCount>0) && (sideAGrid != NULL)) {
      cpp_sideAGridCount=sideAGridCount;
      cpp_sideAGrid = reinterpret_cast<ESMCI::Grid **>(sideAGrid); 
    } else {
      cpp_sideAGridCount=0;
      cpp_sideAGrid = NULL;
    }

    // prepare the side A Mesh info
    int cpp_sideAMeshCount=0;
    void **cpp_sideAMesh;
    if ((sideAMeshCount>0) && (sideAMesh != NULL)) {
      cpp_sideAMeshCount=sideAMeshCount;
      cpp_sideAMesh = reinterpret_cast<void **>(sideAMesh); 
    } else {
      cpp_sideAMeshCount=0;
      cpp_sideAMesh = NULL;
    }

    // prepare the side B Grid info
    int cpp_sideBGridCount=0;
    ESMCI::Grid **cpp_sideBGrid;
    if ((sideBGridCount>0) && (sideBGrid != NULL)) {
      cpp_sideBGridCount=sideBGridCount;
      cpp_sideBGrid = reinterpret_cast<ESMCI::Grid **>(sideBGrid); 
    } else {
      cpp_sideBGridCount=0;
      cpp_sideBGrid = NULL;
    }

    // prepare the side B Mesh info
    int cpp_sideBMeshCount=0;
    void **cpp_sideBMesh;
    if ((sideBMeshCount>0) && (sideBMesh != NULL)) {
      cpp_sideBMeshCount=sideBMeshCount;
      cpp_sideBMesh = reinterpret_cast<void **>(sideBMesh); 
    } else {
      cpp_sideBMeshCount=0;
      cpp_sideBMesh = NULL;
    }

    // prepare the sideAGridPriority info
    int cpp_sideAGridPriorityCount;      
    int *cpp_sideAGridPriority;     
    if (present((ESMCI::InterArray<int> *)sideAGridPriority)) {
      ESMCI::InterArray<int> *tmp_cppInterArray=(ESMCI::InterArray<int> *)sideAGridPriority;
      if(tmp_cppInterArray->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- sideAGridPriority array must be of rank 1", ESMC_CONTEXT, rc);
         return ESMC_NULL_POINTER;
      }

      cpp_sideAGridPriorityCount=tmp_cppInterArray->extent[0];      
      cpp_sideAGridPriority=tmp_cppInterArray->array;      
    } else {
      cpp_sideAGridPriorityCount=0;
      cpp_sideAGridPriority=&tmp_int_mem;
    }


    // prepare the sideAMeshPriority info
    int cpp_sideAMeshPriorityCount;      
    int *cpp_sideAMeshPriority;     
    if (present((ESMCI::InterArray<int> *)sideAMeshPriority)) {
      ESMCI::InterArray<int> *tmp_cppInterArray=(ESMCI::InterArray<int> *)sideAMeshPriority;
      if(tmp_cppInterArray->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- sideAMeshPriority array must be of rank 1", ESMC_CONTEXT, rc);
         return ESMC_NULL_POINTER;
      }

      cpp_sideAMeshPriorityCount=tmp_cppInterArray->extent[0];      
      cpp_sideAMeshPriority=tmp_cppInterArray->array;      
    } else {
      cpp_sideAMeshPriorityCount=0;
      cpp_sideAMeshPriority=&tmp_int_mem;
    }


    // prepare the sideBGridPriority info
    int cpp_sideBGridPriorityCount;      
    int *cpp_sideBGridPriority;     
    if (present((ESMCI::InterArray<int> *)sideBGridPriority)) {
      ESMCI::InterArray<int> *tmp_cppInterArray=(ESMCI::InterArray<int> *)sideBGridPriority;
      if(tmp_cppInterArray->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- sideBGridPriority array must be of rank 1", ESMC_CONTEXT, rc);
         return ESMC_NULL_POINTER;
      }

      cpp_sideBGridPriorityCount=tmp_cppInterArray->extent[0];      
      cpp_sideBGridPriority=tmp_cppInterArray->array;      
    } else {
      cpp_sideBGridPriorityCount=0;
      cpp_sideBGridPriority=&tmp_int_mem;
    }


    // prepare the sideBMeshPriority info
    int cpp_sideBMeshPriorityCount;      
    int *cpp_sideBMeshPriority;     
    if (present((ESMCI::InterArray<int> *)sideBMeshPriority)) {
      ESMCI::InterArray<int> *tmp_cppInterArray=(ESMCI::InterArray<int> *)sideBMeshPriority;
      if(tmp_cppInterArray->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- sideBMeshPriority array must be of rank 1", ESMC_CONTEXT, rc);
         return ESMC_NULL_POINTER;
      }

      cpp_sideBMeshPriorityCount=tmp_cppInterArray->extent[0];      
      cpp_sideBMeshPriority=tmp_cppInterArray->array;      
    } else {
      cpp_sideBMeshPriorityCount=0;
      cpp_sideBMeshPriority=&tmp_int_mem;
    }


    // prepare the sideAMaskValues info
    int cpp_sideAMaskValuesCount;      
    int *cpp_sideAMaskValues;     
    if (present((ESMCI::InterArray<int> *)sideAMaskValues)) {
      ESMCI::InterArray<int> *tmp_cppInterArray=(ESMCI::InterArray<int> *)sideAMaskValues;
      if(tmp_cppInterArray->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- sideAMaskValues array must be of rank 1", ESMC_CONTEXT, rc);
         return ESMC_NULL_POINTER;
      }

      cpp_sideAMaskValuesCount=tmp_cppInterArray->extent[0];      
      cpp_sideAMaskValues=tmp_cppInterArray->array;      
    } else {
      cpp_sideAMaskValuesCount=0;
      cpp_sideAMaskValues=&tmp_int_mem;
    }


    // prepare the sideBMaskValues info
    int cpp_sideBMaskValuesCount;      
    int *cpp_sideBMaskValues;     
    if (present((ESMCI::InterArray<int> *)sideBMaskValues)) {
      ESMCI::InterArray<int> *tmp_cppInterArray=(ESMCI::InterArray<int> *)sideBMaskValues;
      if(tmp_cppInterArray->dimCount != 1){
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
           "- sideBMaskValues array must be of rank 1", ESMC_CONTEXT, rc);
         return ESMC_NULL_POINTER;
      }

      cpp_sideBMaskValuesCount=tmp_cppInterArray->extent[0];      
      cpp_sideBMaskValues=tmp_cppInterArray->array;      
    } else {
      cpp_sideBMaskValuesCount=0;
      cpp_sideBMaskValues=&tmp_int_mem;
    }


    // Allocate C++ XGrid struct
    ESMCI::XGrid *xgrid = NULL;
    try{
      xgrid = new XGrid;
    }catch(...){
      // allocation error
      ESMC_LogDefault.MsgAllocError("for new ESMCI::XGrid.", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }

    // Call into Fortran 
    FTN_X(f_esmf_xgridcreate)(xgrid,  
                              &cpp_sideAGridCount, cpp_sideAGrid,
                              &cpp_sideAMeshCount, cpp_sideAMesh,
                              &cpp_sideBGridCount, cpp_sideBGrid,
                              &cpp_sideBMeshCount, cpp_sideBMesh,
                              &cpp_sideAGridPriorityCount, cpp_sideAGridPriority, 
                              &cpp_sideAMeshPriorityCount, cpp_sideAMeshPriority, 
                              &cpp_sideBGridPriorityCount, cpp_sideBGridPriority, 
                              &cpp_sideBMeshPriorityCount, cpp_sideBMeshPriority, 
                              &cpp_sideAMaskValuesCount, cpp_sideAMaskValues,     
                              &cpp_sideBMaskValuesCount, cpp_sideBMaskValues,     
                              &storeOverlay,  
                              &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      rc)) return ESMC_NULL_POINTER;
   
  
    // output return code  
    if (rc) *rc = localrc;
  
    // output xgrid
    return xgrid;
  }



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XGrid::destroy()"
//BOP
// !IROUTINE:  ESMCI::XGrid::destroy - free a XGrid created with Create
//
// !INTERFACE:
      int XGrid::destroy(
//
// !RETURN VALUE:
//    int error return code
  
// !ARGUMENTS:
  XGrid *xgrid){
  
// !DESCRIPTION:
//      ESMF routine which destroys a XGrid object previously allocated
//      via an ESMC\_XGridCreate routine.  Define for deep classes only.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMC\_XGrid.h)
//
//EOP
// !REQUIREMENTS:  

    // Initialize return code. Assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;
    
    FTN_X(f_esmf_xgriddestroy)(xgrid, &localrc);

    delete xgrid;
    localrc = ESMF_SUCCESS;

    return localrc;

 } 

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XGrid::getSideAGridCount()"
//BOP
// !IROUTINE:  ESMCI::XGrid::getSideAGridCount - Get the number of side A Grids
//
// !INTERFACE:
  int XGrid::getSideAGridCount(
//
// !RETURN VALUE:
//     number of side A Grids
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Get the number of side A Grids. 
//
//EOP
    // Initialize return code. Assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;

    // get info from XGrid
    int count=0;
    FTN_X(f_esmf_xgridgetsideagridcount)(this, &count, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return count;
    
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
    return count;
  }
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XGrid::getSideAMeshCount()"
//BOP
// !IROUTINE:  ESMCI::XGrid::getSideAMeshCount - Get the number of side A Meshes
//
// !INTERFACE:
  int XGrid::getSideAMeshCount(
//
// !RETURN VALUE:
//     number of side A Meshes
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Get the number of side A Meshes. 
//
//EOP
    // Initialize return code. Assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;

    // get info from XGrid
    int count=0;
    FTN_X(f_esmf_xgridgetsideameshcount)(this, &count, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return count;
    
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
    return count;
  }
//-----------------------------------------------------------------------------




//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XGrid::getSideBGridCount()"
//BOP
// !IROUTINE:  ESMCI::XGrid::getSideBGridCount - Get the number of side B Grids
//
// !INTERFACE:
  int XGrid::getSideBGridCount(
//
// !RETURN VALUE:
//     number of side B Grids
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Get the number of side B Grids. 
//
//EOP
    // Initialize return code. Assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;

    // get info from XGrid
    int count=0;
    FTN_X(f_esmf_xgridgetsidebgridcount)(this, &count, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return count;
    
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
    return count;
  }
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XGrid::getSideBMeshCount()"
//BOP
// !IROUTINE:  ESMCI::XGrid::getSideBMeshCount - Get the number of side B Meshes
//
// !INTERFACE:
  int XGrid::getSideBMeshCount(
//
// !RETURN VALUE:
//     number of side B Meshes
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Get the number of side B Meshes. 
//
//EOP
    // Initialize return code. Assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;

    // get info from XGrid
    int count=0;
    FTN_X(f_esmf_xgridgetsidebmeshcount)(this, &count, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return count;
    
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
    return count;
  }
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XGrid::getDimCount()"
//BOP
// !IROUTINE:  ESMCI::XGrid::getDimCount - Get the dimension of the XGrid.
//
// !INTERFACE:
  int XGrid::getDimCount(
//
// !RETURN VALUE:
//     The dimension of the XGrid.
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//     Get the dimension of the XGrid.
//
//EOP
    // Initialize return code. Assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;

    // get info from XGrid
    int count=0;
    FTN_X(f_esmf_xgridgetdimcount)(this, &count, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return count;
    
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
    return count;
  }
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XGrid::getElementCount()"
//BOP
// !IROUTINE:  ESMCI::XGrid::getElementCount - Get the number of elements in the XGrid.
//
// !INTERFACE:
  int XGrid::getElementCount(
//
// !RETURN VALUE:
//     The number of elements in the XGrid.
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//     Get the number of elements in the XGrid.
//
//EOP
    // Initialize return code. Assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;

    // get info from XGrid
    int count=0;
    FTN_X(f_esmf_xgridgetelementcount)(this, &count, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return count;
    
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
    return count;
  }
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XGrid::getMesh()"
//BOP
// !IROUTINE:  ESMCI::XGrid::getMesh - Get the Mesh that represents the XGrid.
//
// !INTERFACE:
  ESMC_Mesh XGrid::getMesh(
//
// !RETURN VALUE:
//     The XGrid Mesh
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//     Get the XGrid Mesh structure. 
//
//EOP
    // Initialize return code. Assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;

    int parametricDim, spatialDim;
    ESMC_CoordSys_Flag coordSys;
    void *mesh_ptr;

    // Init Mesh struct
    ESMC_Mesh mesh;
    mesh.ptr = NULL; 

    // get info from XGrid
    FTN_X(f_esmf_xgridgetmesh)(this, &mesh_ptr, &parametricDim, 
                               &spatialDim, &coordSys, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return mesh;


    // Create the Mesh from info coming from F90
    mesh=ESMC_MeshCreateFromPtr(mesh_ptr, parametricDim, spatialDim,
                                coordSys, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return mesh;

    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
    return mesh;
  }
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XGrid::getArea()"
//BOP
// !IROUTINE:  ESMCI::XGrid::getArea - Fill an array of areas.
//
// !INTERFACE:
  void XGrid::getArea(
//
// !RETURN VALUE:
//     N/A
//
// !ARGUMENTS:
                      ESMC_R8 *area, // out - the array of areas to fill
                      int *rc) {     // out - return code
//
// !DESCRIPTION:
//     Fill and array with element areas
//
//EOP
    // Initialize return code. Assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;

    // get info from XGrid
    int count=0;
    FTN_X(f_esmf_xgridgetelementcount)(this, &count, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;


    // get info from XGrid
    FTN_X(f_esmf_xgridgetarea)(this, &count, area, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
    return;
  }
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XGrid::getCentroid()"
//BOP
// !IROUTINE:  ESMCI::XGrid::getCentroid - Fill an array of areas.
//
// !INTERFACE:
  void XGrid::getCentroid(
//
// !RETURN VALUE:
//     N/A
//
// !ARGUMENTS:
                      ESMC_R8 *centroid, // out - the array of areas to fill
                      int *rc) {     // out - return code
//
// !DESCRIPTION:
//     Fill and array with element areas
//
//EOP
    // Initialize return code. Assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;

    // get info from XGrid
    int elemCount=0;
    FTN_X(f_esmf_xgridgetelementcount)(this, &elemCount, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;

    int dimCount=0;
    FTN_X(f_esmf_xgridgetdimcount)(this, &dimCount, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;


    // get info from XGrid
    FTN_X(f_esmf_xgridgetcentroid)(this, &elemCount, &dimCount, centroid, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
    return;
  }
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XGrid::getSparseMatA2X()"
//BOP
// !IROUTINE:  ESMCI::XGrid::getSparseMatA2X() 
//
// !INTERFACE:
  void XGrid::getSparseMatA2X(
//
// !RETURN VALUE:
//     N/A
//
// !ARGUMENTS:
                              int sideAIndex, 
                              int *factorListCount,
                              double **factorList, 
                              int **factorIndexList,
                              int *rc) {     // out - return code
//
// !DESCRIPTION:
//     Fill and array with element areas
//
//EOP
    // Initialize return code. Assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;

    // Make sideAIndex 0 based
    int sideAIndex_base0=sideAIndex+1;

    // get info from XGrid
    FTN_X(f_esmf_xgridgetsparsemata2x)(this,
                                       &sideAIndex, 
                                       factorListCount, 
                                       factorList, 
                                       factorIndexList, 
                                       &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
    return;
  }
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XGrid::getSparseMatX2A()"
//BOP
// !IROUTINE:  ESMCI::XGrid::getSparseMatX2A() 
//
// !INTERFACE:
  void XGrid::getSparseMatX2A(
//
// !RETURN VALUE:
//     N/A
//
// !ARGUMENTS:
                              int sideAIndex, 
                              int *factorListCount,
                              double **factorList, 
                              int **factorIndexList,
                              int *rc) {     // out - return code
//
// !DESCRIPTION:
//     Fill and array with element areas
//
//EOP
    // Initialize return code. Assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;

    // Make sideAIndex 0 based
    int sideAIndex_base0=sideAIndex+1;

    // get info from XGrid
    FTN_X(f_esmf_xgridgetsparsematx2a)(this,
                                       &sideAIndex, 
                                       factorListCount, 
                                       factorList, 
                                       factorIndexList, 
                                       &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
    return;
  }
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XGrid::getSparseMatB2X()"
//BOP
// !IROUTINE:  ESMCI::XGrid::getSparseMatB2X() 
//
// !INTERFACE:
  void XGrid::getSparseMatB2X(
//
// !RETURN VALUE:
//     N/A
//
// !ARGUMENTS:
                              int sideBIndex, 
                              int *factorListCount,
                              double **factorList, 
                              int **factorIndexList,
                              int *rc) {     // out - return code
//
// !DESCRIPTION:
//     Fill and array with element areas
//
//EOP
    // Initialize return code. Assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;

    // Make sideBIndex 0 based
    int sideBIndex_base0=sideBIndex+1;

    // get info from XGrid
    FTN_X(f_esmf_xgridgetsparsematb2x)(this,
                                       &sideBIndex, 
                                       factorListCount, 
                                       factorList, 
                                       factorIndexList, 
                                       &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
    return;
  }
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XGrid::getSparseMatX2B()"
//BOP
// !IROUTINE:  ESMCI::XGrid::getSparseMatX2B() 
//
// !INTERFACE:
  void XGrid::getSparseMatX2B(
//
// !RETURN VALUE:
//     N/A
//
// !ARGUMENTS:
                              int sideBIndex, 
                              int *factorListCount,
                              double **factorList, 
                              int **factorIndexList,
                              int *rc) {     // out - return code
//
// !DESCRIPTION:
//     Fill and array with element areas
//
//EOP
    // Initialize return code. Assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;

    // Make sideBIndex 0 based
    int sideBIndex_base0=sideBIndex+1;

    // get info from XGrid
    FTN_X(f_esmf_xgridgetsparsematx2b)(this,
                                       &sideBIndex, 
                                       factorListCount, 
                                       factorList, 
                                       factorIndexList, 
                                       &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
    return;
  }
//-----------------------------------------------------------------------------



} // namespace ESMCI
