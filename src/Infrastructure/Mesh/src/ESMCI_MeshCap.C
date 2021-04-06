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
//==============================================================================
#define ESMC_FILENAME "ESMCI_MeshCap.C"
//==============================================================================

//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------

#include <string>
#include <ostream>
#include <iterator>
#include "ESMCI_Macros.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Mesh.h"
#include "ESMCI_VM.h"
#include "ESMCI_CoordSys.h"

#include "ESMCI_TraceMacros.h"  // for profiling

#include "Mesh/include/ESMCI_Mesh_Glue.h"
#include "Mesh/include/ESMCI_Mesh_GToM_Glue.h"
#include "Mesh/include/ESMCI_Mesh_Regrid_Glue.h"
#include "Mesh/include/ESMCI_Mesh_XGrid_Glue.h"
#include "Mesh/include/ESMCI_MBMesh.h"
#include "Mesh/include/ESMCI_MBMesh_Dual.h"
#include "Mesh/include/ESMCI_MBMesh_Glue.h"
#include "Mesh/include/ESMCI_MBMesh_GToM_Glue.h"
#include "Mesh/include/ESMCI_MBMesh_Regrid_Glue.h"
#include "Mesh/include/ESMCI_MBMesh_Util.h"
#include "Mesh/include/ESMCI_MeshCap.h"
//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
// static const char *const version = "$Id$";
 //-----------------------------------------------------------------------------


 using namespace ESMCI;

// Private constructor
 MeshCap::MeshCap() : is_esmf_mesh(false), mesh(NULL), mbmesh(NULL) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::MeshCap()"
    ESMC_BaseSetName(NULL, "Mesh");
  }


void MeshCap::fit_on_vm(VM **vm, int *rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::fit_on_vm()"

  // Call into func. depending on mesh type
  int localrc;
  if (is_esmf_mesh) {
    ESMCI_MeshFitOnVM(&mesh, vm, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;
#if defined ESMF_MOAB
  } else {
    MBMesh_FitOnVM(&mbmesh, vm, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;
#endif
  }
}



// This method converts a Mesh to a PointList
void MeshCap::MeshCap_to_PointList(ESMC_MeshLoc_Flag meshLoc,
                                   ESMCI::InterArray<int> *maskValuesArg, PointList **out_pl,
                                   int *rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::MeshCap_to_PointList()"



  // Call into func. depending on mesh type
  int localrc;
  if (is_esmf_mesh) {
    *out_pl=mesh->MeshToPointList(meshLoc,
                                  maskValuesArg, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;

#if defined ESMF_MOAB
  } else {
    *out_pl = MBMesh_to_PointList(static_cast<MBMesh *>(mbmesh), meshLoc,
                                  maskValuesArg, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;
#endif
  }
}


// returns NULL if unsuccessful
MeshCap *MeshCap::create_from_ptr(void **_mesh,
                              bool _is_esmf_mesh, int *rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::create_from_ptr()"


  // Create MeshCap
  MeshCap *mc=new MeshCap();

  // Set member variables
  mc->is_esmf_mesh=_is_esmf_mesh;
  if (_is_esmf_mesh) {
    // ESMC_LogDefault.Write("create_from_ptr: creating with NATIVE", ESMC_LOGMSG_DEBUG);
    mc->mesh=(Mesh *)(*_mesh);
  } else {
#if defined ESMF_MOAB
    // ESMC_LogDefault.Write("create_from_ptr: creating with MOAB", ESMC_LOGMSG_DEBUG);
   mc->mbmesh=static_cast<MBMesh*>(*_mesh);
#else
   if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
      "This functionality requires ESMF to be built with the MOAB library enabled" , ESMC_CONTEXT, rc)) return NULL;
#endif
  }

  // Set error code to success
  if (rc) *rc=ESMF_SUCCESS;

  // Output new MeshCap
   return mc;
}


void MeshCap::xgrid_getfrac(Grid **gridpp,
                             ESMCI::Array **arraypp, int *staggerLoc,
                             int *rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::xgrid_getfrac()"

  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_xgrid_getfrac(gridpp,
                         &mesh, arraypp, staggerLoc,
                         rc);
  } else {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
        "- this functionality is not currently supported using MOAB",
                                  ESMC_CONTEXT, rc);
    return;
  }
}

void MeshCap::xgrid_getfrac2(Grid **gridpp,
                             ESMCI::Array **arraypp, int *staggerLoc,
                             int *rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::xgrid_getfrac2()"

  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_xgrid_getfrac2(gridpp,
                           &mesh, arraypp, staggerLoc,
                         rc);
  } else {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
        "- this functionality is not currently supported using MOAB",
                                  ESMC_CONTEXT, rc);
    return;
  }
}


void MeshCap::meshsetfrac(double * fraction,
                            int*rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshsetfrac()"

  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_meshsetfraction(&mesh, fraction,
                          rc);
  } else {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
        "- this functionality is not currently supported using MOAB",
                                  ESMC_CONTEXT, rc);
    return;
  }
}


MeshCap *MeshCap::merge(MeshCap **srcmeshpp, MeshCap **dstmeshpp,
                        int*rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::merge()"

  // Can only do if the same kind
  if ((*srcmeshpp)->is_esmf_mesh != (*dstmeshpp)->is_esmf_mesh) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
       "- can't do this operation with different mesh types",
                                  ESMC_CONTEXT, rc);
    return NULL;
  }

  // Get mesh type
  bool is_esmf_mesh=(*srcmeshpp)->is_esmf_mesh;

  // Call into func. depending on mesh type
  Mesh *mesh;
  if (is_esmf_mesh) {
    // ESMC_LogDefault.Write("merge:creating with NATIVE", ESMC_LOGMSG_DEBUG);
    int localrc;
    ESMCI_meshmerge(&((*srcmeshpp)->mesh),
                    &((*dstmeshpp)->mesh),
                    &mesh,
                    &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return NULL;
  } else {
    // ESMC_LogDefault.Write("merge:creating with MOAB", ESMC_LOGMSG_DEBUG);
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
       "- this functionality is not currently supported using MOAB",
                                  ESMC_CONTEXT, rc);
    return NULL;
  }

  // Create MeshCap
  MeshCap *mc=new MeshCap();

  // Set member variables
  mc->is_esmf_mesh=is_esmf_mesh;
  mc->mesh=mesh;

  // Output new MeshCap
  return mc;
 }



void MeshCap::xgridregrid_create(MeshCap **meshsrcpp, MeshCap **meshdstpp,
                                 MeshCap **out_mesh,
                                 int *compute_midmesh,
                                 int *regridMethod,
                                 int *unmappedaction,
                                 int *nentries, ESMCI::TempWeights **tweights,
                                 int*rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::xgridregrid_create()"


  // Can only do if the same kind
  if ((*meshsrcpp)->is_esmf_mesh != (*meshdstpp)->is_esmf_mesh) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
       "- can't do this operation with different mesh types",
                                  ESMC_CONTEXT, rc);
    return;
  }

  // Get mesh type
  bool is_esmf_mesh=(*meshsrcpp)->is_esmf_mesh;

  // Call into func. depending on mesh type
  Mesh *mesh;
  if (is_esmf_mesh) {
    // ESMC_LogDefault.Write("xgridregrid_create:creating with NATIVE", ESMC_LOGMSG_DEBUG);
    int localrc;
    ESMCI_xgridregrid_create(&((*meshsrcpp)->mesh),
                             &((*meshdstpp)->mesh),
                             &mesh,
                             compute_midmesh,
                             regridMethod,
                             unmappedaction,
                             nentries, tweights,
                             &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;
  } else {
    // ESMC_LogDefault.Write("xgridregrid_create:creating with MOAB", ESMC_LOGMSG_DEBUG);
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
       "- this functionality is not currently supported using MOAB",
                                  ESMC_CONTEXT, rc);
    return;
  }

  // Create MeshCap
  MeshCap *mc=new MeshCap();

  // Set member variables
  mc->is_esmf_mesh=is_esmf_mesh;
  mc->mesh=mesh;

  // Output new MeshCap
  *out_mesh=mc;

  return;
 }


MeshCap *MeshCap::GridToMesh(const Grid &grid_, int staggerLoc,
                             const std::vector<ESMCI::Array*> &arrays,
                             ESMCI::InterArray<int> *maskValuesArg,
                             int *regridConserve, int *rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::GridToMesh()"


  // Eventually should be argument
  bool _is_esmf_mesh=true;

  // Local error code
  int localrc;

  // Create mesh depending on the type
  Mesh *mesh;
  MBMesh *mbmesh = nullptr;
  if (_is_esmf_mesh) {
    // ESMC_LogDefault.Write("GridToMesh:creating with NATIVE", ESMC_LOGMSG_DEBUG);
    ESMCI_GridToMesh(grid_, staggerLoc,
                     arrays,
                     maskValuesArg,
                     regridConserve, &mesh, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return NULL;
  } else {
    // ESMC_LogDefault.Write("GridToMesh:creating with MOAB", ESMC_LOGMSG_DEBUG);
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
      "- this functionality is not currently supported using MOAB",
                                  ESMC_CONTEXT, rc);
    return NULL;
  }

  // Create MeshCap
  MeshCap *mc=new MeshCap();

  // Set member variables
  mc->is_esmf_mesh=_is_esmf_mesh;
  if (_is_esmf_mesh) {
    mc->mesh=mesh;
  } else {
    mc->mbmesh=mbmesh;
  }

  // Output new MeshCap
   return mc;
}

// Global in ESMCI_Mesh_F.C
// (This is kind of ugly, and the variable should probably moved to this file
//  after the release, but I don't want to make big changes this close.)
extern bool Moab_on;
MeshCap *MeshCap::GridToMeshCell(const Grid &grid_,
                             const std::vector<ESMCI::Array*> &arrays,
                             int *rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::GridToMeshCell()"


  // Set from global var. set in ESMCI_Mesh_F.C
  bool _is_esmf_mesh=true;
  if (Moab_on) _is_esmf_mesh=false;

  // Local error code
  int localrc;

  // Create mesh depending on the type
  Mesh *mesh;
  MBMesh *mbmesh = nullptr;
  if (_is_esmf_mesh) {
    // ESMC_LogDefault.Write("GridToMeshCell:creating with NATIVE", ESMC_LOGMSG_DEBUG);
    ESMCI_GridToMeshCell(grid_,
                         arrays,
                         &mesh, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return NULL;
  } else {
#if defined ESMF_MOAB
    // ESMC_LogDefault.Write("GridToMeshCell:creating with MOAB", ESMC_LOGMSG_DEBUG);

    MBMesh_GridToMeshCell(grid_,
                          &mbmesh, 
                          &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return NULL;
#else
   if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
      "This functionality requires ESMF to be built with the MOAB library enabled" , ESMC_CONTEXT, rc)) return NULL;
#endif
  }

  // Create MeshCap
  MeshCap *mc=new MeshCap();

  // Set member variables
  mc->is_esmf_mesh=_is_esmf_mesh;
  if (_is_esmf_mesh) {
    mc->mesh=mesh;
  } else {
    mc->mbmesh=mbmesh;
  }

  // Output new MeshCap
   return mc;
}


#if 0
  // Only works for scalar data right now, but would be pretty easy to add more dimensions
  void ESMCI_CpMeshDataToArray(Grid &grid, int staggerLoc, ESMCI::Mesh &mesh, ESMCI::Array &array, MEField<> *dataToArray);

  // Assumes array is on center staggerloc of grid
  void ESMCI_CpMeshElemDataToArray(Grid &grid, int staggerloc, ESMCI::Mesh &mesh, ESMCI::Array &array, MEField<> *dataToArray);

  void ESMCI_PutElemAreaIntoArray(Grid &grid, int staggerLoc, ESMCI::Mesh &mesh, ESMCI::Array &array);

#endif

void MeshCap::regrid_getiwts(Grid **gridpp,
                             MeshCap **meshpp, ESMCI::Array **arraypp, int *staggerLoc,
                             int *regridScheme, int*rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::regrid_getiwts()"

  // Get mesh type
  bool is_esmf_mesh=(*meshpp)->is_esmf_mesh;

  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    int localrc;
    ESMCI_regrid_getiwts(gridpp,
                         &((*meshpp)->mesh), arraypp, staggerLoc,
                         regridScheme, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;
  } else {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
       "- this functionality is not currently supported using MOAB",
                                  ESMC_CONTEXT, rc);
    return;
  }
}

void MeshCap::regrid_getarea(Grid **gridpp,
                             MeshCap **meshpp, ESMCI::Array **arraypp, int *staggerLoc,
                             int *regridScheme, int*rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::regrid_getarea()"

  // Get mesh type
  bool is_esmf_mesh=(*meshpp)->is_esmf_mesh;

  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    int localrc;
    ESMCI_regrid_getarea(gridpp,
                         &((*meshpp)->mesh), arraypp, staggerLoc,
                         regridScheme, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;
  } else {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
       "- this functionality is not currently supported using MOAB",
                                  ESMC_CONTEXT, rc);
    return;
  }
}

void MeshCap::regrid_getfrac(Grid **gridpp,
                             MeshCap **meshpp, ESMCI::Array **arraypp, int *staggerLoc,
                             int*rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::regrid_getfrac()"

  // Get mesh type
  bool is_esmf_mesh=(*meshpp)->is_esmf_mesh;

  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    int localrc;
    ESMCI_regrid_getfrac(gridpp,
                         &((*meshpp)->mesh), arraypp, staggerLoc,
                         &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;
  } else {
#if defined ESMF_MOAB
    int localrc;
    MBMesh_get_elem_frac_into_Array((*meshpp)->mbmesh, *arraypp, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;
#else

   if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
      "This functionality requires ESMF to be built with the MOAB library enabled" , ESMC_CONTEXT, rc)) return;
#endif
  }
}


void MeshCap::regrid_create(
    MeshCap **mcapsrcpp, ESMCI::Array **arraysrcpp, ESMCI::PointList **plsrcpp,
    MeshCap **mcapdstpp, ESMCI::Array **arraydstpp, ESMCI::PointList **pldstpp,
    int *regridMethod,
    int *map_type,
    int *norm_type,
    int *regridPoleType, int *regridPoleNPnts,
    int *regridScheme,
    int *extrapMethod,
    int *extrapNumSrcPnts,
    ESMC_R8 *extrapDistExponent,
    int *extrapNumLevels,
    int *extrapNumInputLevels,
    int *unmappedaction, int *_ignoreDegenerate,
    int *srcTermProcessing, int *pipelineDepth,
    ESMCI::RouteHandle **rh, int *has_rh, int *has_iw,
    int *nentries, ESMCI::TempWeights **tweights,
    int *has_udl, int *_num_udl, ESMCI::TempUDL **_tudl,
    int *has_statusArray, ESMCI::Array **statusArray,
    int *checkFlag, 
    int*rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::regrid_create()"


  // Variables needed below
  bool is_esmf_mesh;
  void *mesh_src_p;
  void *mesh_dst_p;


   // Handle case where incoming meshes are null
  if (*mcapsrcpp != NULL) {
    if (*mcapdstpp != NULL) {
      // Can only do if the same kind
      if ((*mcapsrcpp)->is_esmf_mesh != (*mcapdstpp)->is_esmf_mesh) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
            " - can't do this operation with different mesh types",
                                      ESMC_CONTEXT, rc);
        return;
      }

      // Get mesh type
      is_esmf_mesh=(*mcapsrcpp)->is_esmf_mesh;

      // Get Mesh **
      if (is_esmf_mesh) {
        mesh_src_p=(void *)(*mcapsrcpp)->mesh;
        mesh_dst_p=(void *)(*mcapdstpp)->mesh;
      } else {
        mesh_src_p=static_cast<void*>((*mcapsrcpp)->mbmesh);
        mesh_dst_p=static_cast<void*>((*mcapdstpp)->mbmesh);
      }
    } else {
      // Get mesh type
      is_esmf_mesh=(*mcapsrcpp)->is_esmf_mesh;

      // Get Mesh **
      if (is_esmf_mesh) {
        mesh_src_p=(void *)(*mcapsrcpp)->mesh;
      } else {
        mesh_src_p=static_cast<void*>((*mcapsrcpp)->mbmesh);
      }
      //mesh_dst_pp=NULL;
      mesh_dst_p=NULL;
    }
  } else {
    if (*mcapdstpp != NULL) {
      // Get mesh type
      is_esmf_mesh=(*mcapdstpp)->is_esmf_mesh;

      // Get Mesh **
      //mesh_src_pp=NULL;
      mesh_src_p=NULL;
       if (is_esmf_mesh) {
         mesh_dst_p=(void *)(*mcapdstpp)->mesh;
      } else {
        mesh_dst_p=static_cast<void*>((*mcapdstpp)->mbmesh);
      }
    } else {
      // Get mesh type
      is_esmf_mesh=true; // ESMF MESH CAN HANDLE, SO USE THAT

     // Get Mesh **
      mesh_src_p=NULL;
      mesh_dst_p=NULL;
     }
  }

  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    int localrc;
    ESMCI_REGRID_TRACE_ENTER("NativeMesh regrid");
    ESMCI_regrid_create((Mesh **)&mesh_src_p, arraysrcpp, plsrcpp,
                        (Mesh **)&mesh_dst_p, arraydstpp, pldstpp,
                        regridMethod,
                        map_type,
                        norm_type,
                        regridPoleType, regridPoleNPnts,
                        regridScheme,
                        extrapMethod,
                        extrapNumSrcPnts,
                        extrapDistExponent,
                        extrapNumLevels,
                        extrapNumInputLevels,
                        unmappedaction, _ignoreDegenerate,
                        srcTermProcessing, pipelineDepth,
                        rh, has_rh, has_iw,
                        nentries, tweights,
                        has_udl, _num_udl, _tudl,
                        has_statusArray, statusArray,
                        checkFlag, 
                        &localrc);
    ESMCI_REGRID_TRACE_EXIT("NativeMesh regrid");
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;
  } else {
#if defined ESMF_MOAB
    int localrc;
    ESMCI_REGRID_TRACE_ENTER("MBMesh regrid");
    MBMesh_regrid_create((MBMesh**)(&mesh_src_p), arraysrcpp, plsrcpp,
                         (MBMesh**)(&mesh_dst_p), arraydstpp, pldstpp,
                         regridMethod,
                         map_type,
                         norm_type,
                         regridPoleType, regridPoleNPnts,
                         regridScheme,
                         extrapMethod,
                         extrapNumSrcPnts,
                         extrapDistExponent,
                         extrapNumLevels,
                         extrapNumInputLevels,
                         unmappedaction, _ignoreDegenerate,
                         srcTermProcessing, pipelineDepth,
                         rh, has_rh, has_iw,
                         nentries, tweights,
                         has_udl, _num_udl, _tudl,
                         has_statusArray, statusArray,
                         &localrc);
    ESMCI_REGRID_TRACE_EXIT("MBMesh regrid");
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;
#else
   if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
      "This functionality requires ESMF to be built with the MOAB library enabled" , ESMC_CONTEXT, rc)) return;
#endif
  }
}


// returns NULL if unsuccessful
MeshCap *MeshCap::meshcreate(int *pdim, int *sdim,
                              ESMC_CoordSys_Flag *coordSys,
                              bool _is_esmf_mesh, int *rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshcreate()"

  int localrc;

  // Create mesh depending on the type
  Mesh *mesh;
  MBMesh *mbmesh = nullptr;
  if (_is_esmf_mesh) {
    // ESMC_LogDefault.Write("meshcreate:creating with NATIVE", ESMC_LOGMSG_DEBUG);
    ESMCI_MESHCREATE_TRACE_ENTER("NativeMesh create");
    ESMCI_meshcreate(&mesh,
                     pdim, sdim,
                     coordSys, &localrc);
    ESMCI_MESHCREATE_TRACE_EXIT("NativeMesh create");
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                       ESMC_CONTEXT, rc)) return NULL;
  } else {
#if defined ESMF_MOAB
    // ESMC_LogDefault.Write("meahcreate:creating with MOAB", ESMC_LOGMSG_DEBUG);
    ESMCI_MESHCREATE_TRACE_ENTER("MBMesh create");
    MBMesh_create(&mbmesh,
                  pdim, sdim,
                  coordSys, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return NULL;
    ESMCI_MESHCREATE_TRACE_EXIT("MBMesh create");
#else
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
      "This functionality requires ESMF to be built with the MOAB library enabled" , ESMC_CONTEXT, rc)) return NULL;
#endif
  }

  // Create MeshCap
  MeshCap *mc=new MeshCap();

  // Set member variables
  mc->is_esmf_mesh=_is_esmf_mesh;
  if (_is_esmf_mesh) {
    mc->mesh=mesh;
  } else {
    mc->mbmesh=mbmesh;
  }

  // Output new MeshCap
  return mc;
}

void MeshCap::meshaddnodes(int *num_nodes, int *nodeId,
                           double *nodeCoord, int *nodeOwner, InterArray<int> *nodeMaskII,
                           ESMC_CoordSys_Flag *_coordSys, int *_orig_sdim,
                           int *rc)
{
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshaddnodes()"

  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_MESHCREATE_TRACE_ENTER("NativeMesh addnodes");
    ESMCI_meshaddnodes(&mesh, num_nodes, nodeId,
                       nodeCoord, nodeOwner, nodeMaskII,
                       _coordSys, _orig_sdim,
                       rc);
    ESMCI_MESHCREATE_TRACE_EXIT("NativeMesh addnodes");
  } else {
#if defined ESMF_MOAB
    ESMCI_MESHCREATE_TRACE_ENTER("MBMesh addnodes");
    MBMesh_addnodes(&mbmesh, num_nodes, nodeId,
                     nodeCoord, nodeOwner, nodeMaskII,
                     _coordSys, _orig_sdim,
                     rc);
    ESMCI_MESHCREATE_TRACE_EXIT("MBMesh addnodes");
#else
   if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
      "This functionality requires ESMF to be built with the MOAB library enabled" , ESMC_CONTEXT, rc)) return;
#endif
   }
}


void MeshCap::meshwrite(char *fname, int *rc,
    ESMCI_FortranStrLenArg nlen) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshwrite()"

  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_meshwrite(&mesh, fname, rc, nlen);

  } else {
#if defined ESMF_MOAB
    MBMesh_write(&mbmesh, fname, rc,
                 nlen);
#else
   if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
      "This functionality requires ESMF to be built with the MOAB library enabled" , ESMC_CONTEXT, rc)) return;
#endif
  }
}

void MeshCap::meshwritewarrays(char *fname, ESMCI_FortranStrLenArg nlen,
                               int num_nodeArrays, ESMCI::Array **nodeArrays,
                               int num_elemArrays, ESMCI::Array **elemArrays,
                               int *rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshwritewarrays()"

  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_meshwritewarrays(&mesh,
                           fname, nlen,
                           num_nodeArrays, nodeArrays,
                           num_elemArrays, elemArrays,
                           rc);
  } else {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
          "- this functionality is not currently supported using MOAB",
                                  ESMC_CONTEXT, rc);
    return;
  }
}


void MeshCap::meshaddelements(int *_num_elems, int *elemId, int *elemType, InterArray<int> *_elemMaskII ,
                              int *_areaPresent, double *elemArea,
                              int *_coordsPresent, double *elemCoords,
                              int *_num_elemConn, int *elemConn, int *regridConserve,
                              ESMC_CoordSys_Flag *_coordSys, int *_orig_sdim,
                              int *rc)
{
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshaddelements()"

  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_MESHCREATE_TRACE_ENTER("NativeMesh addelements");
    ESMCI_meshaddelements(&mesh,
                          _num_elems, elemId, elemType, _elemMaskII ,
                          _areaPresent, elemArea,
                          _coordsPresent, elemCoords,
                          _num_elemConn, elemConn, regridConserve,
                          _coordSys, _orig_sdim,
                          rc);
    ESMCI_MESHCREATE_TRACE_EXIT("NativeMesh addelements");
  } else {
 #if defined ESMF_MOAB
    ESMCI_MESHCREATE_TRACE_ENTER("MBMesh addelements");
    MBMesh_addelements(&mbmesh,
                       _num_elems, elemId, elemType, _elemMaskII,
                       _areaPresent, elemArea,
                       _coordsPresent, elemCoords,
                       _num_elemConn, elemConn, regridConserve,
                       _coordSys, _orig_sdim,
                       rc);
    ESMCI_MESHCREATE_TRACE_EXIT("MBMesh addelements");
#else
   if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
      "This functionality requires ESMF to be built with the MOAB library enabled" , ESMC_CONTEXT, rc)) return;
#endif
  }
}


/**
 * Routines for reading in a test VTK mesh to fortran arrays (for testing the array interface)
 */
void MeshCap::meshvtkheader(char *filename, int *num_elem, int *num_node, int *conn_size, int *rc,
     ESMCI_FortranStrLenArg nlen) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshvtkheader()"

  // Static, so can't use is_esmf_mesh
  // Check if need, and if so
  // eventually put into something separate from both Mesh and MOAB
  ESMCI_meshvtkheader(filename, num_elem, num_node, conn_size, rc,
                         nlen);
}

void MeshCap::meshvtkbody(char *filename, int *nodeId, double *nodeCoord,
                    int *nodeOwner, int *elemId, int *elemType, int *elemConn, int *rc,
    ESMCI_FortranStrLenArg nlen) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshvtkbody()"

  // Static, so can't use is_esmf_mesh
  // Check if need, and if so
  // eventually put into something separate from both Mesh and MOAB
  ESMCI_meshvtkbody(filename, nodeId, nodeCoord,
                    nodeOwner, elemId, elemType, elemConn, rc,
                    nlen);
}


// Just destroy structure, internal meshes are destroyed in destroy()
MeshCap::~MeshCap() {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::~MeshCap()"

}

void MeshCap::meshfreememory(int *rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshfreememory()"

  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_meshfreememory(&mesh, rc);

    // Make this NULL to indicate that mesh is gone
    mesh=NULL;
  } else {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
       "- this functionality is not currently supported using MOAB",
                                  ESMC_CONTEXT, rc);
    return;
  }
}

void MeshCap::meshget(int *num_nodes, int *num_elements, int *rc){
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshget()"

  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_meshget(&mesh, num_nodes, num_elements, rc);
  } else {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
       "- this functionality is not currently supported using MOAB",
                                  ESMC_CONTEXT, rc);
    return;
  }
}



void MeshCap::getNodeCount(int *nodeCount, int *rc){
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::getNodeCount()"

  int localrc;
  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_MeshGetNodeCount(mesh, nodeCount, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;
  } else {
#if defined ESMF_MOAB
    MBMesh_GetNodeCount(mbmesh, nodeCount, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;
#else
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
       "This functionality requires ESMF to be built with the MOAB library enabled" , ESMC_CONTEXT, rc)) return;
#endif
  }
}


void MeshCap::getElemCount(int *elemCount, int *rc){
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::getElemCount()"

  int localrc;
  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_MeshGetElemCount(mesh, elemCount, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;
  } else {
#if defined ESMF_MOAB
    MBMesh_GetElemCount(mbmesh, elemCount, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;
#else
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
       "This functionality requires ESMF to be built with the MOAB library enabled" , ESMC_CONTEXT, rc)) return;
#endif
  }
}


void MeshCap::getElemConnCount(int *elemConnCount, int *rc){
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::getElemConnCount()"

  int localrc;
  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_MeshGetElemConnCount(mesh, elemConnCount, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;
  } else {
#if defined ESMF_MOAB
    MBMesh_GetElemConnCount(mbmesh, elemConnCount, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;
#else
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
       "This functionality requires ESMF to be built with the MOAB library enabled" , ESMC_CONTEXT, rc)) return;
#endif
  }
}


void MeshCap::getElemInfoPresence(int *elemMaskIsPresent,
                                  int *elemAreaIsPresent,
                                  int *elemCoordsIsPresent,
                                  int *rc){
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::getElemInfoPresence()"

  int localrc;
  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_MeshGetElemInfoPresence(mesh,
                                  elemMaskIsPresent, 
                                  elemAreaIsPresent, 
                                  elemCoordsIsPresent, 
                                  &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;
  } else {
#if defined ESMF_MOAB
    MBMesh_GetElemInfoPresence(mbmesh,
                               elemMaskIsPresent, 
                               elemAreaIsPresent, 
                               elemCoordsIsPresent, 
                               &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;
#else
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
       "This functionality requires ESMF to be built with the MOAB library enabled" , ESMC_CONTEXT, rc)) return;
#endif
  }
}



void MeshCap::getElemCreateInfo(ESMCI::InterArray<int> *elemIds,
                                ESMCI::InterArray<int> *elemTypes,
                                ESMCI::InterArray<int> *elemConn,
                                ESMCI::InterArray<int> *elemMask,
                                ESMCI::InterArray<ESMC_R8> *elemArea,
                                ESMCI::InterArray<ESMC_R8> *elemCoords, int *rc){
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::getElemCreateInfo()"

  int localrc;
  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_MeshGetElemCreateInfo(mesh, elemIds,
                                elemTypes, elemConn,
                                elemMask, elemArea, 
                                elemCoords, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;
  } else {
#if defined ESMF_MOAB
    MBMesh_GetElemCreateInfo(mbmesh, elemIds,
                             elemTypes, elemConn,
                             elemMask, elemArea, 
                             elemCoords, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;
#else
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
       "This functionality requires ESMF to be built with the MOAB library enabled" , ESMC_CONTEXT, rc)) return;
#endif
  }
}

void MeshCap::setElemInfo(ESMCI::InterArray<int> *elemMask,
                          ESMCI::InterArray<ESMC_R8> *elemArea,
                          int *rc){
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::setElemInfo()"

  int localrc;
  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_MeshSetElemInfo(mesh, elemMask, elemArea, &localrc);
  } else {
#if defined ESMF_MOAB
    MBMesh_SetElemCreateInfo(mbmesh, elemMask, elemArea, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;
#else
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
       "This functionality requires ESMF to be built with the MOAB library enabled" , ESMC_CONTEXT, rc)) return;
#endif
  }
}

void MeshCap::getNodeInfoPresence(int *nodeMaskIsPresent, int *rc){
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::getNodeInfoPresence()"

  int localrc;
  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_MeshGetNodeInfoPresence(mesh, 
                                  nodeMaskIsPresent, 
                                  &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;
  } else {
#if defined ESMF_MOAB
    MBMesh_GetNodeInfoPresence(mbmesh, 
                               nodeMaskIsPresent, 
                               &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;
#else
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
       "This functionality requires ESMF to be built with the MOAB library enabled" , ESMC_CONTEXT, rc)) return;
#endif
  }
}


void MeshCap::getNodeCreateInfo(ESMCI::InterArray<int> *nodeIds,
                                ESMCI::InterArray<ESMC_R8> *nodeCoords,
                                ESMCI::InterArray<int> *nodeOwners,
                                ESMCI::InterArray<int> *nodeMask,
                                int *rc){
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::getNodeCreateInfo()"

  int localrc;
  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_MeshGetNodeCreateInfo(mesh, nodeIds,
                                nodeCoords, nodeOwners,
                                nodeMask, &localrc);
  } else {
#if defined ESMF_MOAB
    MBMesh_GetNodeCreateInfo(mbmesh,  nodeIds, 
                             nodeCoords, nodeOwners,
                             nodeMask,  &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;
#else
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
       "This functionality requires ESMF to be built with the MOAB library enabled" , ESMC_CONTEXT, rc)) return;
#endif
  }
}


void MeshCap::meshcreatenodedistgrid(int *ngrid, int *num_lnodes, int *rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshcreatenodedistgrid()"

  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_meshcreatenodedistgrid(&mesh, ngrid, num_lnodes, rc);
  } else {
#if defined ESMF_MOAB
    MBMesh_createnodedistgrid(&mbmesh, ngrid, num_lnodes, rc);
#else
   if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
      "This functionality requires ESMF to be built with the MOAB library enabled" , ESMC_CONTEXT, rc)) return;
#endif
  }
}


void MeshCap::meshcreateelemdistgrid(int *egrid, int *num_lelems, int *rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshcreateelemdistgrid()"

  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_meshcreateelemdistgrid(&mesh, egrid, num_lelems, rc);
  } else {
#if defined ESMF_MOAB
    MBMesh_createelemdistgrid(&mbmesh, egrid, num_lelems, rc);
#else
   if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
      "This functionality requires ESMF to be built with the MOAB library enabled" , ESMC_CONTEXT, rc)) return;
#endif
  }
}

void MeshCap::meshinfoserialize(int *intMeshFreed,
                                int *spatialDim, int *parametricDim,
                                int *intIsPresentNDG, int *intIsPresentEDG,
                                int *coordSys, 
                                char *buffer, int *length, int *offset,
                                ESMC_InquireFlag *inquireflag, int *rc,
                                ESMCI_FortranStrLenArg buffer_l){
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshinfoserialize()"

  // Static, so can't use is_esmf_mesh
  // Check if need, and if so
  // eventually put into something separate from both Mesh and MOAB
  ESMCI_meshinfoserialize(intMeshFreed,
                          spatialDim, parametricDim,
                          intIsPresentNDG, intIsPresentEDG,
                          coordSys, 
                          buffer, length, offset,
                          inquireflag, rc, buffer_l);
}


void MeshCap::meshinfodeserialize(int *intMeshFreed,
                                  int *spatialDim, int *parametricDim,
                                  int *intIsPresentNDG, int *intIsPresentEDG,
                                  int *coordSys, 
                                  char *buffer, int *offset, int *rc,
                                  ESMCI_FortranStrLenArg buffer_l){
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshinfodeserialize()"

  // Static, so can't use is_esmf_mesh
  // Check if need, and if so
  // eventually put into something separate from both Mesh and MOAB
  ESMCI_meshinfodeserialize(intMeshFreed,
                            spatialDim, parametricDim,
                            intIsPresentNDG, intIsPresentEDG,
                            coordSys, 
                            buffer, offset, rc,
                            buffer_l);
}

void MeshCap::meshserialize(char *buffer, int *length, int *offset,
                            const ESMC_AttReconcileFlag &attreconflag,
                            ESMC_InquireFlag *inquireflag, bool baseOnly,
                            int *rc, ESMCI_FortranStrLenArg buffer_l){
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshserialize()"

  int localrc;
  int r;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  // Calc Size
  int size = 1*sizeof(int);

  // TODO: verify length > vars.
  if (*inquireflag != ESMF_INQUIREONLY) {
    if ((*length - *offset) < size) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "Buffer too short to add Mesh object", ESMC_CONTEXT, rc);
       return;
    }
  }

  // Save integers
  int *ip= (int *)(buffer + *offset);
  if (*inquireflag != ESMF_INQUIREONLY) {
    *ip++ = static_cast<int> (is_esmf_mesh);
  }

  // Adjust offset
  *offset += size;

  // Serialize the Base class,
  localrc = ESMC_Base::ESMC_Serialize(buffer, length, offset, attreconflag,
    *inquireflag);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return;

  if (!baseOnly){
    // Call into func. depending on mesh type
    if (is_esmf_mesh) {
      ESMCI_meshserialize(&mesh,
                        buffer, length, offset,
                        inquireflag, &localrc,
                        buffer_l);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;
    } else {
#if defined ESMF_MOAB
      MBMesh_serialize(&mbmesh, buffer, length, offset, inquireflag, &localrc,
                     buffer_l);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;
#else
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
        "This functionality requires ESMF to be built with the MOAB library enabled" , ESMC_CONTEXT, rc)) return;
#endif
    }
  }

  if (rc!=NULL) *rc=ESMF_SUCCESS;
}

void MeshCap::meshdeserialize(char *buffer, int *offset,
                              const ESMC_AttReconcileFlag &attreconflag,
                              bool baseOnly,
                              int *rc, ESMCI_FortranStrLenArg buffer_l){
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshdeserialize()"

  Mesh *mesh;
  MBMesh *mbmesh = nullptr;
  int localrc;
  int local_is_esmf_mesh = 1;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  // Get pointer
  int *ip= (int *)(buffer + *offset);

  // Get values
  local_is_esmf_mesh=*ip++;

  // Adjust offset
  *offset += sizeof(int);

  // Deserialize the Base class
  localrc = ESMC_Base::ESMC_Deserialize(buffer, offset, attreconflag);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return;

  if (!baseOnly){
    if (local_is_esmf_mesh) {
    // ESMC_LogDefault.Write("meshdeserialize:creating with NATIVE", ESMC_LOGMSG_DEBUG);

      ESMCI_meshdeserialize(&mesh,
                          buffer, offset, &localrc,
                          buffer_l);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;
    } else {
#if defined ESMF_MOAB
    // ESMC_LogDefault.Write("meshdeserialize:creating with MOAB", ESMC_LOGMSG_DEBUG);
      MBMesh_deserialize(&mbmesh, buffer, offset, &localrc, buffer_l);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;
#else
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
        "This functionality requires ESMF to be built with the MOAB library enabled" , ESMC_CONTEXT, rc)) return;
#endif
    }
    // Set member variables
    this->is_esmf_mesh=static_cast<bool> (local_is_esmf_mesh);
    this->mesh=mesh;
    this->mbmesh=mbmesh;
  }

  if (rc!=NULL) *rc=ESMF_SUCCESS;

  // Output new MeshCap
  return;
}

void MeshCap::meshfindpnt(int *unmappedaction, int *dimPnts, int *numPnts,
                          double *pnts, int *pets, int *rc){
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshfindpnt()"

  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_meshfindpnt(&mesh, unmappedaction, dimPnts, numPnts,
                       pnts, pets, rc);
  } else {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
       "- this functionality is not currently supported using MOAB",
                                  ESMC_CONTEXT, rc);
    return;
  }

}

void MeshCap::geteleminfointoarray(DistGrid *elemDistgrid,
                                   int numElemArrays,
                                   int *infoTypeElemArrays,
                                   Array **elemArrays,
                                   int *rc)
{
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::geteleminfointoarray()"
  int localrc;

  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_geteleminfointoarray(mesh,
                               elemDistgrid,
                               numElemArrays,
                               infoTypeElemArrays,
                               elemArrays,
                               &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;
  } else {
#if defined ESMF_MOAB
    MBMesh_geteleminfointoarray(mbmesh,
                                elemDistgrid,
                                numElemArrays,
                                infoTypeElemArrays,
                                elemArrays,
                                &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;

#else
   if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
      "This functionality requires ESMF to be built with the MOAB library enabled" , ESMC_CONTEXT, rc)) return;
#endif
  }
}

void MeshCap::getlocalelemcoords(double *elemCoord, int *_orig_sdim, int *rc)
{
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::getlocalelemcoords()"

  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_getlocalelemcoords(&mesh, elemCoord, _orig_sdim, rc);
  } else {
#if defined ESMF_MOAB
    MBMesh_getlocalelemcoords(&mbmesh, elemCoord, _orig_sdim, rc);
#else
   if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
      "This functionality requires ESMF to be built with the MOAB library enabled" , ESMC_CONTEXT, rc)) return;
#endif
  }
}

void MeshCap::getlocalcoords(double *nodeCoord, int *_orig_sdim, int *rc)
{
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::getlocalcoords()"

  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_getlocalcoords(&mesh, nodeCoord, _orig_sdim, rc);
  } else {
#if defined ESMF_MOAB
    MBMesh_getlocalcoords(&mbmesh, nodeCoord, _orig_sdim, rc);
#else
   if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
      "This functionality requires ESMF to be built with the MOAB library enabled" , ESMC_CONTEXT, rc)) return;
#endif
  }
}



void MeshCap::meshgetarea(int *num_elem, double *elem_areas, int *rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshgetarea()"

  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_meshgetarea(&mesh, num_elem, elem_areas, rc);
  } else {
#if defined ESMF_MOAB
    MBMesh_getarea(&mbmesh, num_elem, elem_areas, rc);
#else
   if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
      "This functionality requires ESMF to be built with the MOAB library enabled" , ESMC_CONTEXT, rc)) return;
#endif
  }
}


void MeshCap::meshgetdimensions(int *sdim, int *pdim, int *rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshgetdimensions()"

  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_meshgetdimensions(&mesh, sdim, pdim, rc);
  } else {
#if defined ESMF_MOAB
    MBMesh_GetDimensions(mbmesh, sdim, pdim, rc);
#else
   if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
      "This functionality requires ESMF to be built with the MOAB library enabled" , ESMC_CONTEXT, rc)) return;
#endif
  }
}

void MeshCap::meshgetcentroid(int *num_elem, double *elem_centroid, int *rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshgetcentroid()"

  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_meshgetcentroid(&mesh, num_elem, elem_centroid, rc);
  } else {
#if defined ESMF_MOAB
    MBMesh_GetCentroid(mbmesh, num_elem, elem_centroid, rc);
#else
   if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
      "This functionality requires ESMF to be built with the MOAB library enabled" , ESMC_CONTEXT, rc)) return;
#endif
  }
}

void MeshCap::meshgetfrac(int *_num_elem, double *elem_fracs, int *rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshgetfrac()"

  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_meshgetfrac(&mesh, _num_elem, elem_fracs, rc);
  } else {
#if defined ESMF_MOAB
    MBMesh_getelemfrac(mbmesh, _num_elem, elem_fracs, rc);
#else
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
      "This functionality requires ESMF to be built with the MOAB library enabled" , ESMC_CONTEXT, rc)) return;
#endif
    return;
  }
}

void MeshCap::meshgetfrac2(int *num_elem, double *elem_fracs, int *rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshgetfrac2()"

  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_meshgetfrac2(&mesh, num_elem, elem_fracs, rc);
  } else {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
       "- this functionality is not currently supported using MOAB",
                                  ESMC_CONTEXT, rc);
    return;
  }

}

void MeshCap::triangulate(int *pdim, int *sdim, int *numPnts,
                          double *pnts, double *td, int *ti, int *triInd, int *rc){
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::triangulate()"

  // Static, so can't use is_esmf_mesh
  // Check if need, and if so
  // eventually put into something separate from both Mesh and MOAB
   ESMCI_triangulate(pdim, sdim, numPnts,
                     pnts, td, ti, triInd, rc);
}

void MeshCap::meshturnoncellmask(ESMCI::InterArray<int> *maskValuesArg,  int *rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshturnoncellmask()"

  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_meshturnoncellmask(&mesh, maskValuesArg, rc);
  } else {
#if defined ESMF_MOAB
    MBMesh_turnonelemmask(&mbmesh, maskValuesArg, rc);
#else
   if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
      "This functionality requires ESMF to be built with the MOAB library enabled" , ESMC_CONTEXT, rc)) return;
#endif
  }

}

// Turn OFF masking
void MeshCap::meshturnoffcellmask(int *rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshturnoffcellmask()"

  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_meshturnoffcellmask(&mesh, rc);
  } else {
#if defined ESMF_MOAB
    MBMesh_turnoffelemmask(&mbmesh, rc);
#else
   if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
      "This functionality requires ESMF to be built with the MOAB library enabled" , ESMC_CONTEXT, rc)) return;
#endif
  }

 }


void MeshCap::meshturnonnodemask(ESMCI::InterArray<int> *maskValuesArg, int *rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshturnonnodemask()"

  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_meshturnonnodemask(&mesh, maskValuesArg, rc);
  } else {
#if defined ESMF_MOAB
    MBMesh_turnonnodemask(&mbmesh, maskValuesArg, rc);
#else
   if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
      "This functionality requires ESMF to be built with the MOAB library enabled" , ESMC_CONTEXT, rc)) return;
#endif
  }

}

// Turn OFF masking
void MeshCap::meshturnoffnodemask(int *rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshturnoffnodemask()"

  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_meshturnoffnodemask(&mesh, rc);
  } else {
#if defined ESMF_MOAB
    MBMesh_turnoffnodemask(&mbmesh, rc);
#else
   if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
      "This functionality requires ESMF to be built with the MOAB library enabled" , ESMC_CONTEXT, rc)) return;
#endif
  }
}



////////////
void MeshCap::get_polygon_area(int *spatialdim, int *nedges,
                               double *points, double *area, int *rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::get_polygon_area()"

  // Static, so can't use is_esmf_mesh
  // Check if need, and if so
  // eventually put into something separate from both Mesh and MOAB
  ESMCI_get_polygon_area(spatialdim, nedges,
                         points, area, rc);
}

MeshCap *MeshCap::meshcreatefrommeshes(MeshCap **meshapp, MeshCap **meshbpp,
ESMC_MeshOp_Flag * meshop, double * threshold, int *rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshcreatefrommeshes()"


  // Can only do if the same kind
  if ((*meshapp)->is_esmf_mesh != (*meshbpp)->is_esmf_mesh) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
       "- can't do this operation with different mesh types",
                                  ESMC_CONTEXT, rc);
    return NULL;
  }

  // Get mesh type
  bool is_esmf_mesh=(*meshapp)->is_esmf_mesh;

  // Call into func. depending on mesh type
  Mesh *mesh;
  if (is_esmf_mesh) {
    // ESMC_LogDefault.Write("meshcreatefrommeshes:creating with NATIVE", ESMC_LOGMSG_DEBUG);

    int localrc;
    ESMCI_meshcreatefrommeshes(&((*meshapp)->mesh),
                               &((*meshbpp)->mesh),
                               &mesh,
                               meshop, threshold, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return NULL;
  } else {
    // ESMC_LogDefault.Write("meshcreatefrommeshes:creating with MOAB", ESMC_LOGMSG_DEBUG);

    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
       "- this functionality is not currently supported using MOAB",
                                  ESMC_CONTEXT, rc);
    return NULL;
  }

  // Create MeshCap
  MeshCap *mc=new MeshCap();

  // Set member variables
  mc->is_esmf_mesh=is_esmf_mesh;
  mc->mesh=mesh;

  // Output new MeshCap
  return mc;
 }


MeshCap *MeshCap::meshcreateredistelems(MeshCap **src_meshpp, int *num_elem_gids, int *elem_gids,
                                        int *rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshcreateredistelems()"

  int localrc;

  // Get mesh type
  bool is_esmf_mesh=(*src_meshpp)->is_esmf_mesh;

  // Call into func. depending on mesh type
  Mesh *mesh = NULL;
  MBMesh *mbmesh = nullptr;

  if (is_esmf_mesh) {
    // ESMC_LogDefault.Write("meshcreateredistelems:creating with NATIVE", ESMC_LOGMSG_DEBUG);

    ESMCI_MESHREDIST_TRACE_ENTER("NativeMesh redist (elements)");
    ESMCI_meshcreateredistelems(&((*src_meshpp)->mesh), num_elem_gids, elem_gids,
                                &mesh, &localrc);
    ESMCI_MESHREDIST_TRACE_EXIT("NativeMesh redist (elements)");
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return NULL;
  } else {
#if defined ESMF_MOAB
    // ESMC_LogDefault.Write("meshcreateredistelems:creating with MOAB", ESMC_LOGMSG_DEBUG);
    ESMCI_MESHREDIST_TRACE_ENTER("MBMesh redist (elements)");
    MBMesh_createredistelems(&((*src_meshpp)->mbmesh), num_elem_gids, elem_gids,
                                &mbmesh, &localrc);
    ESMCI_MESHREDIST_TRACE_EXIT("MBMesh redist (elements)");
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return NULL;
#else
   if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
      "This functionality requires ESMF to be built with the MOAB library enabled" ,
      ESMC_CONTEXT, rc)) return NULL;
#endif
  }

  // Create MeshCap
  MeshCap *mc=new MeshCap();

  // Set member variables
  mc->is_esmf_mesh=is_esmf_mesh;
  mc->mesh=mesh;
  mc->mbmesh=mbmesh;

  // Output new MeshCap
  return mc;
}


MeshCap *MeshCap::meshcreateredistnodes(MeshCap **src_meshpp,int *num_node_gids, int *node_gids,
                                    int *rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshcreateredistnodes()"

  int localrc;

  // Get mesh type
  bool is_esmf_mesh=(*src_meshpp)->is_esmf_mesh;

  // Call into func. depending on mesh type
  Mesh *mesh = NULL;
  MBMesh *mbmesh = nullptr;

  if (is_esmf_mesh) {
    // ESMC_LogDefault.Write("meshcreateredistnodes:creating with NATIVE", ESMC_LOGMSG_DEBUG);
    ESMCI_MESHREDIST_TRACE_ENTER("NativeMesh redist (nodes)");
    ESMCI_meshcreateredistnodes(&((*src_meshpp)->mesh), num_node_gids, node_gids,
                                &mesh, &localrc);
    ESMCI_MESHREDIST_TRACE_EXIT("NativeMesh redist (nodes)");
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return NULL;
 } else {
#if defined ESMF_MOAB
    // ESMC_LogDefault.Write("meshcreateredistnodes:creating with MOAB", ESMC_LOGMSG_DEBUG);
    ESMCI_MESHREDIST_TRACE_ENTER("MBMesh redist (nodes)");
    MBMesh_createredistnodes(&((*src_meshpp)->mbmesh), num_node_gids, node_gids,
                                &mbmesh, &localrc);
    ESMCI_MESHREDIST_TRACE_EXIT("MBMesh redist (nodes)");
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return NULL;
#else
   if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
      "This functionality requires ESMF to be built with the MOAB library enabled" ,
      ESMC_CONTEXT, rc)) return NULL;
#endif
  }

  // Create MeshCap
  MeshCap *mc=new MeshCap();

  // Set member variables
  mc->is_esmf_mesh=is_esmf_mesh;
  mc->mesh=mesh;
  mc->mbmesh=mbmesh;

  // Output new MeshCap
  return mc;
}


MeshCap *MeshCap::meshcreateredist(MeshCap **src_meshpp, int *num_node_gids, int *node_gids,
                               int *num_elem_gids, int *elem_gids, int *rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshcreateredist()"

  int localrc;

  // Get mesh type
  bool is_esmf_mesh=(*src_meshpp)->is_esmf_mesh;

  // Call into func. depending on mesh type
  Mesh *mesh = NULL;
  MBMesh *mbmesh = nullptr;

  if (is_esmf_mesh) {
    // ESMC_LogDefault.Write("meshcreateredist:creating with NATIVE", ESMC_LOGMSG_DEBUG);
    ESMCI_MESHREDIST_TRACE_ENTER("NativeMesh redist");
    ESMCI_meshcreateredist(&((*src_meshpp)->mesh), num_node_gids, node_gids,
                           num_elem_gids, elem_gids, &mesh, &localrc);
    ESMCI_MESHREDIST_TRACE_EXIT("NativeMesh redist");
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return NULL;
  } else {
#if defined ESMF_MOAB
    // ESMC_LogDefault.Write("meshcreateredist:creating with MOAB", ESMC_LOGMSG_DEBUG);
    ESMCI_MESHREDIST_TRACE_ENTER("MBMesh redist");
    MBMesh_createredist(&((*src_meshpp)->mbmesh), num_node_gids, node_gids,
                        num_elem_gids, elem_gids, &mbmesh, &localrc);
    ESMCI_MESHREDIST_TRACE_EXIT("MBMesh redist");
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return NULL;
#else
   if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
      "This functionality requires ESMF to be built with the MOAB library enabled" ,
      ESMC_CONTEXT, rc)) return NULL;
#endif
  }

  // Create MeshCap
  MeshCap *mc=new MeshCap();

  // Set member variables
  mc->is_esmf_mesh=is_esmf_mesh;
  mc->mesh=mesh;
  mc->mbmesh=mbmesh;

  // Output new MeshCap
  return mc;
}




// This method verifies that nodes in node_gids array are the same as the local nodes in meshpp, otherwise
// it returns an error (used to test MeshRedist()).
// To do this check make sure the number of nodes in both cases are the same and that every
// entry in node_gids is contained in meshpp
void MeshCap::meshchecknodelist(int *_num_node_gids, int *node_gids, int *rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshchecknodelist()"

  int localrc;

  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_meshchecknodelist(&mesh, _num_node_gids, node_gids, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;
  } else {
#if defined ESMF_MOAB
    MBMesh_checknodelist(&mbmesh, _num_node_gids, node_gids, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;
#else
   if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
      "This functionality requires ESMF to be built with the MOAB library enabled" ,
      ESMC_CONTEXT, rc)) return;
#endif
  }

}


// This method verifies that elems in elem_gids array are the same as the local elems in meshpp, otherwise
// it returns an error (used to test MeshRedist()).
// To do this check make sure the number of elems in both cases are the same and that every
// entry in elem_gids is contained in meshpp
void MeshCap::meshcheckelemlist(int *_num_elem_gids, int *elem_gids, int *rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshcheckelemlist()"

  int localrc;

  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_meshcheckelemlist(&mesh, _num_elem_gids, elem_gids, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;
  } else {
#if defined ESMF_MOAB
    MBMesh_checkelemlist(&mbmesh, _num_elem_gids, elem_gids, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return;
#else
   if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
      "This functionality requires ESMF to be built with the MOAB library enabled" ,
      ESMC_CONTEXT, rc)) return;
#endif
  }
}

// Interface to internal code to convert coords from spherical in degrees to Cartesian
// Input is: lon, lat - spherical coordinates in degrees
// Output is: x,y,z - Cartesian coordinates
 //
void MeshCap::sphdeg_to_cart(double *lon, double *lat,
                             double *x, double *y, double *z, int *rc){
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::sphdeg_to_cart()"

  // Static, so can't use is_esmf_mesh
  // Check if need, and if so
  // eventually put into something separate from both Mesh and MOAB
  ESMCI_sphdeg_to_cart(lon, lat, x, y, z, rc);
}


// This method sets the pole values so a 2D Mesh from a SCRIP grid can still be used in regrid with poles
void MeshCap::meshsetpoles(int *_pole_obj_type, int *_pole_val, 
                           int *_min_pole_gid, int *_max_pole_gid, int *rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshsetpoles()"

  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    ESMCI_meshsetpoles(&mesh, _pole_obj_type, _pole_val, _min_pole_gid, _max_pole_gid, rc);
  } else {
    // Not using poles right now, so comment this out, so we can test
    // with logically rectangular meshes
#if 0
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
       "- this functionality is not currently supported using MOAB",
                                  ESMC_CONTEXT, rc);
    return;
#endif
    *rc=ESMF_SUCCESS;
  }
}
MeshCap *MeshCap::meshcreatedual(MeshCap **src_meshpp, int *rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshcreatedual()"

  int localrc;

 // Get mesh type
  bool is_esmf_mesh=(*src_meshpp)->is_esmf_mesh;

  // Call into func. depending on mesh type
  Mesh *mesh;
  MBMesh *mbmesh = nullptr;
  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    // ESMC_LogDefault.Write("meshcreatedual:creating with NATIVE", ESMC_LOGMSG_DEBUG);
    ESMCI_DUALMESH_TRACE_ENTER("NativeMesh Dual Mesh Generation");
    ESMCI_meshcreatedual(&((*src_meshpp)->mesh), &mesh, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return NULL;
    ESMCI_DUALMESH_TRACE_EXIT("NativeMesh Dual Mesh Generation")
  } else {
#if defined ESMF_MOAB
    // ESMC_LogDefault.Write("meshcreatedual:creating with MOAB", ESMC_LOGMSG_DEBUG);
    MBMesh *meshin = (MBMesh *)((*src_meshpp)->mbmesh);
    ESMCI_DUALMESH_TRACE_ENTER("MBMesh Dual Mesh Generation");
    MBMeshDual(meshin, &mbmesh, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return NULL;
    ESMCI_DUALMESH_TRACE_EXIT("MBMesh Dual Mesh Generation")
#else
   if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
      "This functionality requires ESMF to be built with the MOAB library enabled" ,
      ESMC_CONTEXT, rc)) return NULL;
#endif
  }


  // Create MeshCap
  MeshCap *mc=new MeshCap();

  // Set member variables
  mc->is_esmf_mesh=is_esmf_mesh;
  if (mc->is_esmf_mesh) {
    mc->mesh=mesh;
    mc->mbmesh=NULL;
  } else {
#if defined ESMF_MOAB
    mc->mesh=NULL;
    mc->mbmesh=mbmesh;
#else
   if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
      "This functionality requires ESMF to be built with the MOAB library enabled" ,
      ESMC_CONTEXT, rc)) return NULL;
#endif
  }

  // Output new MeshCap
  return mc;
}

int MeshCap::destroy(MeshCap **mcpp, bool noGarbage) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::destroy()"

  int rc = ESMC_RC_NOT_IMPL;              // final return code

    // Dereference meshcap
  MeshCap *mcp=*mcpp;

  // check if this Mesh object has the persist flag set
  if (mcp->ESMC_BaseGetPersist())
    return ESMF_SUCCESS;  // nothing to be done here, return successfully

  // check if this Mesh object still has a valid entry in the garbage collection
  if (!VM::validObject(mcp))
    return ESMF_SUCCESS;  // nothing to be done here, return successfully

  if (mcp->ESMC_BaseGetStatus()==ESMF_STATUS_READY){

    // Get mesh type
    bool is_esmf_mesh=mcp->is_esmf_mesh;

    // Call into func. depending on mesh type
    if (is_esmf_mesh) {
      // Only do if mesh is present
      if (mcp->mesh != NULL) {
        int localrc;
        ESMCI_meshdestroy(&(mcp->mesh), &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                        ESMC_CONTEXT, &rc)) return rc;
      }
    } else {
#if defined ESMF_MOAB
      // Only do if mbmesh is present
      if (mcp->mbmesh != NULL) {
        int localrc;
        MBMesh_destroy(&(mcp->mbmesh), &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                        ESMC_CONTEXT, &rc)) return rc;
      }
#else
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
        "This functionality requires ESMF to be built with the MOAB library enabled" ,
        ESMC_CONTEXT, &rc)) return rc;
#endif
    }
  }

  // mark as invalid object
  mcp->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);

  // optionally delete the complete object and remove from garbage collection
  if (noGarbage){
    VM::rmObject(mcp);  // remove object from garbage collection
    delete mcp;         // completely delete the object, free heap
  }

  // Set to NULL
  *mcpp=NULL;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}

// returns NULL if unsuccessful
MeshCap *MeshCap::meshcreate_easy_elems(int *pdim,
                                        int *sdim,
                                        int *num_elems,
                                        InterArray<int> *elemIdsII,
                                        int *elemTypes,
                                        InterArray<int> *elemMaskII,
                                        int *num_elemCorners,
                                        double *elemCornerCoords,
                                        int *has_elemArea,
                                        double *elemArea,
                                        int *has_elemCoords,
                                        double *elemCoords,
                                        ESMC_CoordSys_Flag *coordSys,
                                        bool _is_esmf_mesh, int *rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshcreate_easy_elems()"

  int localrc;


  // Create mesh depending on the type
  Mesh *mesh;
  MBMesh *mbmesh = nullptr;
  if (_is_esmf_mesh) {
    // ESMC_LogDefault.Write(":meshcreate_easy_elems:creating with NATIVE", ESMC_LOGMSG_DEBUG);
    ESMCI_meshcreate_easy_elems(&mesh,
                                pdim, sdim,
                                num_elems, elemIdsII, elemTypes, elemMaskII,
                                num_elemCorners, elemCornerCoords,
                                has_elemArea, elemArea,
                                has_elemCoords, elemCoords,
                                coordSys, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                       ESMC_CONTEXT, rc)) return NULL;
  } else {
    // ESMC_LogDefault.Write(":meshcreate_easy_elems:creating with MOAB", ESMC_LOGMSG_DEBUG);
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
       "- this functionality is not currently supported using MOAB",
                                  ESMC_CONTEXT, rc);
    return NULL;
  }

  // Create MeshCap
  MeshCap *mc=new MeshCap();

  // Set member variables
  mc->is_esmf_mesh=_is_esmf_mesh;
  if (_is_esmf_mesh) {
    mc->mesh=mesh;
  } else {
    mc->mbmesh=mbmesh;
  }

  // Output new MeshCap
  return mc;
}



// returns NULL if unsuccessful
MeshCap *MeshCap::meshcreate_from_grid(Grid **gridpp,
                                       bool _is_esmf_mesh,
                                       int *rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "MeshCap::meshcreate_from_grid()"
  int localrc;

  // Dereference grid pointer
  if (gridpp == NULL) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                     " Grid pointer NULL",
                                     ESMC_CONTEXT, rc)) return NULL;
  }
  Grid *gridp=*gridpp;

  if (gridp == NULL) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                     " Grid pointer NULL",
                                     ESMC_CONTEXT, rc)) return NULL;
  }
  Grid &grid=*gridp;

  // Empty vector of Arrays
  std::vector<ESMCI::Array*> empty_arrays;

  // Create mesh depending on the type
  Mesh *mesh;
  MBMesh *mbmesh = nullptr;
  if (_is_esmf_mesh) {
    // ESMC_LogDefault.Write("meshcreate_from_grid:creating with NATIVE", ESMC_LOGMSG_DEBUG);

    ESMCI_GridToMeshCell(grid,
                         empty_arrays,
                         &mesh, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                       ESMC_CONTEXT, rc)) return NULL;
  } else {
#if defined ESMF_MOAB
    // ESMC_LogDefault.Write("meshcreate_from_grid:creating with MOAB", ESMC_LOGMSG_DEBUG);

    MBMesh_GridToMeshCell(grid,
                          &mbmesh, 
                          &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc)) return NULL;
#else
   if(ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
      "This functionality requires ESMF to be built with the MOAB library enabled" , ESMC_CONTEXT, rc)) return NULL;
#endif
  }

  // Create MeshCap
  MeshCap *mc=new MeshCap();

  // Set member variables
  mc->is_esmf_mesh=_is_esmf_mesh;
  if (_is_esmf_mesh) {
    mc->mesh=mesh;
  } else {
    mc->mbmesh=mbmesh;
  }

  // Output new MeshCap
  return mc;
}

void MeshCap::set_xgrid_info(int *side, int *ind, int *rc) {
#undef ESMC_METHOD
#define ESMC_METHOD "set_xgrid_info()"

  // Call into func. depending on mesh type
  if (is_esmf_mesh) {
    mesh->side=*side;
    mesh->ind=*ind;
  } else {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
        "- this functionality is not currently supported using MOAB",
                                  ESMC_CONTEXT, rc);
    return;
  }
}
