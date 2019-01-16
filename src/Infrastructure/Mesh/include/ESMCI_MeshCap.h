// $Id$
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_MeshCap_h
#define ESMCI_MeshCap_h

#include "ESMCI_Mesh.h"

#include "Mesh/include/Regridding/ESMCI_Regrid_Helper.h"

#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Grid.h"
#include "ESMCI_GridToMesh.h"
#include "ESMC_Util.h"
#include "ESMCI_Array.h"



 namespace ESMCI {

  class MeshCap {

    // Eventually merge this with MeshCXX

  private:
    MeshCap();

  public:
    bool is_esmf_mesh;
    Mesh *mesh;     // Make 1 void pointer here for both
    void *mbmesh;

    // NOT NEEDED RIGHT NOW
    //    bool is_internal_mesh_esmf() {return is_esmf_mesh;}

    void *get_internal_mesh_ptr() {
      if (is_esmf_mesh) {
        return (void *)mesh;
      } else {
        return mbmesh;
      }
    }

    void fit_on_vm(VM **vm, int *rc);

    void MeshCap_to_PointList(ESMC_MeshLoc_Flag meshLoc,
                                   ESMCI::InterArray<int> *maskValuesArg, PointList **out_pl,
                                   int *rc);

    static MeshCap *create_from_ptr(void **_mesh,
                             bool _is_esmf_mesh, int *rc);


    static MeshCap *meshcreate(int *pdim, int *sdim,
                                 ESMC_CoordSys_Flag *coordSys,
                                 bool _is_esmf_mesh, int *rc);

    static MeshCap *meshcreate_easy_elems(int *pdim,
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
                                          bool _is_esmf_mesh, int *rc);

    static MeshCap *meshcreate_from_grid(Grid **gridpp,
                                                  bool _is_esmf_mesh, 
                                                  int *rc);

    void meshaddnodes(int *num_nodes, int *nodeId,
                      double *nodeCoord, int *nodeOwner, InterArray<int> *nodeMaskII,
                      ESMC_CoordSys_Flag *_coordSys, int *_orig_sdim,
                      int *rc);

    void meshwrite(char *fname, int *rc,
                   ESMCI_FortranStrLenArg nlen);

    void meshwritewarrays(char *fname, ESMCI_FortranStrLenArg nlen,
                                   int num_nodeArrays, ESMCI::Array **nodeArrays, 
                                   int num_elemArrays, ESMCI::Array **elemArrays, 
                                   int *rc);

    void meshaddelements(int *_num_elems, int *elemId, int *elemType, InterArray<int> *_elemMaskII ,
                         int *_areaPresent, double *elemArea,
                         int *_coordsPresent, double *elemCoords,
                         int *_num_elemConn, int *elemConn, int *regridConserve,
                         ESMC_CoordSys_Flag *_coordSys, int *_orig_sdim,
                         int *rc);

    static void meshvtkheader(char *filename, int *num_elem, int *num_node, int *conn_size, int *rc,
                       ESMCI_FortranStrLenArg nlen);

    static void meshvtkbody(char *filename, int *nodeId, double *nodeCoord,
                     int *nodeOwner, int *elemId, int *elemType, int *elemConn, int *rc,
                     ESMCI_FortranStrLenArg nlen);

    ~MeshCap();

    void meshfreememory(int *rc);

    void meshget(int *num_nodes, int *num_elements, int *rc);


    void meshcreatenodedistgrid(int *ngrid, int *num_lnodes, int *rc);


    void meshcreateelemdistgrid(int *egrid, int *num_lelems, int *rc);


    static void meshinfoserialize(int *intMeshFreed,
                                  int *spatialDim, int *parametricDim,
                                  int *intIsPresentNDG, int *intIsPresentEDG,
                                  char *buffer, int *length, int *offset,
                                  ESMC_InquireFlag *inquireflag, int *rc,
                                  ESMCI_FortranStrLenArg buffer_l);


    static void meshinfodeserialize(int *intMeshFreed,
                                    int *spatialDim, int *parametricDim,
                                    int *intIsPresentNDG, int *intIsPresentEDG,
                                    char *buffer, int *offset, int *rc,
                                    ESMCI_FortranStrLenArg buffer_l);

    void meshserialize(char *buffer, int *length, int *offset,
                       ESMC_InquireFlag *inquireflag, int *rc,
                       ESMCI_FortranStrLenArg buffer_l);


    static MeshCap *meshdeserialize(char *buffer, int *offset, int *rc,
                         ESMCI_FortranStrLenArg buffer_l);

    void meshfindpnt(int *unmappedaction, int *dimPnts, int *numPnts,
                     double *pnts, int *pets, int *rc);

    void geteleminfointoarray(DistGrid *elemDistgrid, 
                                   int numElemArrays,
                                   int *infoTypeElemArrays, 
                                   Array **elemArrays, 
                                   int *rc);

    void getlocalcoords(double *nodeCoord, int *_orig_sdim, int *rc);

    void getlocalelemcoords(double *elemCoord, int *_orig_sdim, int *rc);

    void meshgetarea(int *num_elem, double *elem_areas, int *rc);

    void meshgetdimensions(int *sdim, int *pdim, int *rc);

    void meshgetcentroid(int *num_elem, double *elem_centroid, int *rc);

    void meshgetfrac(int *_num_elem, double *elem_fracs, int *rc);


    void meshgetfrac2(int *num_elem, double *elem_fracs, int *rc);

    static void triangulate(int *pdim, int *sdim, int *numPnts,
                     double *pnts, double *td, int *ti, int *triInd, int *rc);


    void meshturnoncellmask(ESMCI::InterArray<int> *maskValuesArg,  int *rc);

    void meshturnoffcellmask(int *rc);


    void meshturnonnodemask(ESMCI::InterArray<int> *maskValuesArg,  int *rc);

    void meshturnoffnodemask(int *rc);

    static void get_polygon_area(int *spatialdim, int *nedges,
                          double *points, double *area, int *rc);

    static MeshCap *meshcreatefrommeshes(MeshCap **meshapp, MeshCap **meshbpp,
                                         ESMC_MeshOp_Flag * meshop, double * threshold, int *rc);


    static MeshCap *meshcreateredistelems(MeshCap **src_meshpp, int *num_elem_gids, int *elem_gids,
                                          int *rc);


    static MeshCap *meshcreateredistnodes(MeshCap **src_meshpp,int *num_node_gids, int *node_gids,
                                          int *rc);

    static MeshCap *meshcreateredist(MeshCap **src_meshpp, int *num_node_gids, int *node_gids,
                                     int *num_elem_gids, int *elem_gids, int *rc);

    void meshchecknodelist(int *_num_node_gids, int *node_gids,
                           int *rc);

    void meshcheckelemlist(int *_num_elem_gids, int *elem_gids,
                           int *rc);

    static void sphdeg_to_cart(double *lon, double *lat,
                        double *x, double *y, double *z, int *rc);

    void meshsetpoles(int *_pole_obj_type, int *_pole_val, int *_min_pole_gid, int *_max_pole_gid,
                           int *rc);

    static MeshCap *meshcreatedual(MeshCap **src_meshpp, int *rc);

    static void regrid_create(
              MeshCap **meshsrcpp, ESMCI::Array **arraysrcpp, ESMCI::PointList **plsrcpp,
              MeshCap **meshdstpp, ESMCI::Array **arraydstpp, ESMCI::PointList **pldstpp,
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
              int*rc);

    static void regrid_getiwts(Grid **gridpp,
                               MeshCap **meshpp, ESMCI::Array **arraypp, int *staggerLoc,
                               int *regridScheme, int*rc);

    static void regrid_getarea(Grid **gridpp,
                               MeshCap **meshpp, ESMCI::Array **arraypp, int *staggerLoc,
                               int *regridScheme, int*rc);


    static void regrid_getfrac(Grid **gridpp,
                               MeshCap **meshpp, ESMCI::Array **arraypp, int *staggerLoc,
                               int*rc);

    static MeshCap *GridToMesh(const Grid &grid_, int staggerLoc,
                        const std::vector<ESMCI::Array*> &arrays,
                        ESMCI::InterArray<int> *maskValuesArg,
                        int *regridConserve, int *rc);

    static MeshCap *GridToMeshCell(const Grid &grid_,
                                   const std::vector<ESMCI::Array*> &arrays,
                                   int *rc);

     static void xgridregrid_create(MeshCap **meshsrcpp, MeshCap **meshdstpp,
                                    MeshCap **out_mesh,
                                    int *compute_midmesh,
                                    int *regridMethod,
                                    int *unmappedaction,
                                    int *nentries, ESMCI::TempWeights **tweights,
                                    int*rc);

     static MeshCap *merge(MeshCap **srcmeshpp, MeshCap **dstmeshpp,
                           int*rc);

     void meshsetfrac(double * fraction,
                      int*rc);

     void xgrid_getfrac2(Grid **gridpp,
                         ESMCI::Array **arraypp, int *staggerLoc,
                         int *rc);

     void xgrid_getfrac(Grid **gridpp,
                        ESMCI::Array **arraypp, int *staggerLoc,
                        int *rc);

     static void destroy(MeshCap **mcpp,int *rc);
  };

} // namespace

#endif
