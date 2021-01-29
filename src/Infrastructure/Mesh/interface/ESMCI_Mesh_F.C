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
#define ESMC_FILENAME "ESMCI_Mesh_F.C"
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
#include "ESMCI_VM.h"
#include "ESMCI_CoordSys.h"
#include "Mesh/include/ESMCI_MeshCap.h"
#include "Mesh/include/ESMCI_ClumpPnts.h"
#include "Mesh/include/Legacy/ESMCI_ParEnv.h"
#include "ESMCI_DistGrid.h"
#include "ESMCI_Array.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


using namespace ESMCI;



//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt Mesh} class functions.
//
//EOP
//-------------------------------------------------------------------------


/*----------------------------------------------------------------------------
 *  Low level helper functions: translate from F90 to C++.
 *----------------------------------------------------------------------------*/

// Moab variable
bool Moab_on=false;

// This method turns on MOAB
extern "C" void FTN_X(c_esmc_meshsetmoab)(int *_moabOn, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_meshsetmoab()"

  if (*_moabOn==1) Moab_on=true;
  else Moab_on=false;
  
  if (rc!=NULL) *rc=ESMF_SUCCESS;
}

// This method turns on MOAB
extern "C" void FTN_X(c_esmc_meshgetmoab)(int *_moabOn, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_meshsetmoab()"

  *_moabOn=0;
  if (Moab_on) *_moabOn=1; 

  if (rc!=NULL) *rc=ESMF_SUCCESS;
}



#if 0
extern "C" void FTN_X(c_esmc_meshinit)(char *logfile, int *use_log,
   ESMCI_FortranStrLenArg nlen) {

  char *lname = ESMC_F90toCstring(logfile, nlen);

  Par::Init(lname, (*use_log == 1 ? true : false));

  delete [] lname;

}
#endif

extern "C" void FTN_X(c_esmc_clumppntsll)(int *num_pnt, double *pnt_lon, double *pnt_lat,
                                        double *tol, int *pnt_cl_ind,int *num_cl,
                                        int *max_size_cl,
                                        double *start_lat, double *end_lat, int *rc)
{
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_clumppntsll()"

  double *cl_lon, *cl_lat;

  ESMCI::ClumpPntsLL(*num_pnt, pnt_lon, pnt_lat,
                     *tol, pnt_cl_ind, num_cl,
                     &cl_lon, &cl_lat, max_size_cl,
                     *start_lat, *end_lat, rc) ;

  delete [] cl_lon;
  delete [] cl_lat;

}

extern "C" void FTN_X(c_esmc_meshcreate)(MeshCap **meshpp,
                                         int *pdim, int *sdim,
                                         ESMC_CoordSys_Flag *coordSys, 
                                         char *name,
                                         int *rc, ESMCI_FortranStrLenArg name_l)
{
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_meshcreate()"

  int localrc = ESMC_RC_NOT_IMPL;

  // Create Mesh depending on whether MOAB or not
  if (Moab_on) {
    *meshpp=MeshCap::meshcreate(pdim,sdim,coordSys,false,rc);
  } else {
    *meshpp=MeshCap::meshcreate(pdim,sdim,coordSys,true,rc);
  }

  // copy and convert F90 string to null terminated one
  std::string cname(name, name_l);
  cname.resize(cname.find_last_not_of(" ")+1);

  if (cname.length() > 0) {
    localrc = (*meshpp)->ESMC_BaseSetName(cname.c_str(), "Mesh");
    ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));
    return;
  }

} // meshcreate


extern "C" void FTN_X(c_esmc_meshaddnodes)(MeshCap **meshpp, int *num_nodes, int *nodeId,
                                           double *nodeCoord, int *nodeOwner, InterArray<int> *nodeMaskII,
                                           ESMC_CoordSys_Flag *_coordSys, int *_orig_sdim,
                                           int *rc)
{
  // Call into implementation
  (*meshpp)->meshaddnodes(num_nodes, nodeId,
                          nodeCoord, nodeOwner, nodeMaskII,
                          _coordSys, _orig_sdim,
                          rc);

}

extern "C" void FTN_X(c_esmc_meshwrite)(MeshCap **meshpp, char *fname, int *rc,
    ESMCI_FortranStrLenArg nlen) {

  // Call into implementation
  (*meshpp)->meshwrite(fname, rc, nlen);
}


extern "C" void FTN_X(c_esmc_meshwritevtk)(MeshCap **meshpp, char *fname,
                                           ESMCI::Array **nodeArray1,
                                           ESMCI::Array **nodeArray2,
                                           ESMCI::Array **nodeArray3,
                                           ESMCI::Array **elemArray1,
                                           ESMCI::Array **elemArray2,
                                           ESMCI::Array **elemArray3,
                                           int *rc,
                                           ESMCI_FortranStrLenArg nlen) {

#define MAX_NUM_NODE_ARRAYS 3
  int num_nodeArrays;
  ESMCI::Array *nodeArrays[MAX_NUM_NODE_ARRAYS];

  int n=0;
  if (ESMC_NOT_PRESENT_FILTER(nodeArray1) != ESMC_NULL_POINTER) {
    nodeArrays[n]=*nodeArray1;
    n++;
  }
  if (ESMC_NOT_PRESENT_FILTER(nodeArray2) != ESMC_NULL_POINTER) {
    nodeArrays[n]=*nodeArray2;
    n++;
  }
  if (ESMC_NOT_PRESENT_FILTER(nodeArray3) != ESMC_NULL_POINTER) {
    nodeArrays[n]=*nodeArray3;
    n++;
  }
  num_nodeArrays=n;


#define MAX_NUM_ELEM_ARRAYS 3
  int num_elemArrays;
  ESMCI::Array *elemArrays[MAX_NUM_ELEM_ARRAYS];

  // Use optional arguments to fill arrays
  int e=0;
  if (ESMC_NOT_PRESENT_FILTER(elemArray1) != ESMC_NULL_POINTER) {
    elemArrays[e]=*elemArray1;
    e++;
  }
  if (ESMC_NOT_PRESENT_FILTER(elemArray2) != ESMC_NULL_POINTER) {
    elemArrays[e]=*elemArray2;
    e++;
  }
  if (ESMC_NOT_PRESENT_FILTER(elemArray3) != ESMC_NULL_POINTER) {
    elemArrays[e]=*elemArray3;
    e++;
  }
  num_elemArrays=e;

  // Call into implementation
  (*meshpp)->meshwritewarrays(fname, nlen,
                              num_nodeArrays, nodeArrays,
                              num_elemArrays, elemArrays, rc);

#undef MAX_NUM_NODE_ARRAYS
#undef MAX_NUM_ELEM_ARRAYS
}

extern "C" void FTN_X(c_esmc_meshaddelements)(MeshCap **meshpp,
                                              int *_num_elems, int *elemId, int *elemType, InterArray<int> *_elemMaskII ,
                                              int *_areaPresent, double *elemArea,
                                              int *_coordsPresent, double *elemCoords,
                                              int *_num_elemConn, int *elemConn, int *regridConserve,
                                              ESMC_CoordSys_Flag *_coordSys, int *_orig_sdim,
                                              int *rc)
{
  // Call into implementation
  (*meshpp)->meshaddelements(_num_elems, elemId, elemType, _elemMaskII ,
                         _areaPresent, elemArea,
                         _coordsPresent, elemCoords,
                         _num_elemConn, elemConn, regridConserve,
                         _coordSys, _orig_sdim,
                         rc);
}


/**
 * Routines for reading in a test VTK mesh to fortran arrays (for testing the array interface)
 */
extern "C" void FTN_X(c_esmc_meshvtkheader)(char *filename, int *num_elem, int *num_node, int *conn_size, int *rc,
    ESMCI_FortranStrLenArg nlen) {

  // Call into implementation
  MeshCap::meshvtkheader(filename, num_elem, num_node, conn_size, rc, nlen);
}

extern "C" void FTN_X(c_esmc_meshvtkbody)(char *filename, int *nodeId, double *nodeCoord,
                    int *nodeOwner, int *elemId, int *elemType, int *elemConn, int *rc,
    ESMCI_FortranStrLenArg nlen) {

  MeshCap::meshvtkbody(filename, nodeId, nodeCoord,
                         nodeOwner, elemId, elemType, elemConn, rc,
                         nlen);
}

extern "C" void FTN_X(c_esmc_meshdestroy)(MeshCap **meshpp,
  ESMC_Logical *noGarbage, int *rc){
  // Initialize return code; assume routine not implemented
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
  // convert to bool
  bool noGarbageOpt = false;  // default
  if (ESMC_NOT_PRESENT_FILTER(noGarbage) != ESMC_NULL_POINTER)
    if (*noGarbage == ESMF_TRUE) noGarbageOpt = true;
  // test for NULL pointer via macro before calling any class methods
  ESMCI_NULL_CHECK_PRC(meshpp, rc)
  ESMCI_NULL_CHECK_PRC(*meshpp, rc)
  ESMC_LogDefault.MsgFoundError(ESMCI::MeshCap::destroy(meshpp,noGarbageOpt),
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    ESMC_NOT_PRESENT_FILTER(rc));
}

extern "C" void FTN_X(c_esmc_meshfreememory)(MeshCap **meshpp, int *rc) {

  (*meshpp)->meshfreememory(rc);

}


extern "C" void FTN_X(c_esmc_meshget)(MeshCap **meshpp, int *num_nodes, int *num_elements, int *rc){

  (*meshpp)->meshget(num_nodes, num_elements, rc);
}


extern "C" void FTN_X(c_esmc_meshgetnodecount)(MeshCap **meshpp, int *nodeCount, int *rc){

  (*meshpp)->getNodeCount(nodeCount, rc);
}


extern "C" void FTN_X(c_esmc_meshgetelemcount)(MeshCap **meshpp, int *elemCount, int *rc){

  (*meshpp)->getElemCount(elemCount, rc);
}

extern "C" void FTN_X(c_esmc_meshgetelemconncount)(MeshCap **meshpp, int *elemConnCount, int *rc){

  (*meshpp)->getElemConnCount(elemConnCount, rc);
}

extern "C" void FTN_X(c_esmc_meshgeteleminfopresence)(MeshCap **meshpp, 
                                                      int *elemMaskIsPresent,
                                                      int *elemAreaIsPresent,
                                                      int *elemCoordsIsPresent,
                                                      int *rc){

  (*meshpp)->getElemInfoPresence(elemMaskIsPresent, elemAreaIsPresent, elemCoordsIsPresent, rc);
}

extern "C" void FTN_X(c_esmc_meshgetelemcreateinfo)(MeshCap **meshpp, 
                                                    ESMCI::InterArray<int> *elemIds,
                                                    ESMCI::InterArray<int> *elemTypes,
                                                    ESMCI::InterArray<int> *elemConn,
                                                    ESMCI::InterArray<int> *elemMask,
                                                    ESMCI::InterArray<ESMC_R8> *elemArea,
                                                    ESMCI::InterArray<ESMC_R8> *elemCoords,
                                                    int *rc){

  (*meshpp)->getElemCreateInfo(elemIds, elemTypes, elemConn, elemMask, elemArea, elemCoords, rc);
}

extern "C" void FTN_X(c_esmc_meshseteleminfo)(MeshCap **meshpp, 
                                                    ESMCI::InterArray<int> *elemMask,
                                                    ESMCI::InterArray<ESMC_R8> *elemArea,
                                                    int *rc){

  (*meshpp)->setElemInfo(elemMask, elemArea, rc);
}



extern "C" void FTN_X(c_esmc_meshgetnodeinfopresence)(MeshCap **meshpp, 
                                                      int *nodeMaskIsPresent,
                                                      int *rc){

  (*meshpp)->getNodeInfoPresence(nodeMaskIsPresent, rc);
}


extern "C" void FTN_X(c_esmc_meshgetnodecreateinfo)(MeshCap **meshpp, 
                                                    ESMCI::InterArray<int> *nodeIds,
                                                    ESMCI::InterArray<ESMC_R8> *nodeCoords,
                                                    ESMCI::InterArray<int> *nodeOwners,
                                                    ESMCI::InterArray<int> *nodeMask,
                                                    int *rc){

  (*meshpp)->getNodeCreateInfo(nodeIds, nodeCoords, nodeOwners, nodeMask, rc);
}



extern "C" void FTN_X(c_esmc_meshcreatenodedistgrid)(MeshCap **meshpp, int *ngrid, int *num_lnodes, int *rc) {

  (*meshpp)->meshcreatenodedistgrid(ngrid, num_lnodes, rc);

}


extern "C" void FTN_X(c_esmc_meshcreateelemdistgrid)(MeshCap **meshpp, int *egrid, int *num_lelems, int *rc) {

  (*meshpp)->meshcreateelemdistgrid(egrid, num_lelems, rc);
}


extern "C" void FTN_X(c_esmc_meshinfoserialize)(int *intMeshFreed,
                                                int *spatialDim, int *parametricDim,
                                                int *intIsPresentNDG, int *intIsPresentEDG,
                                                int *coordSys, 
                                                char *buffer, int *length, int *offset,
                                                ESMC_InquireFlag *inquireflag, int *rc,
                                                ESMCI_FortranStrLenArg buffer_l){

  MeshCap::meshinfoserialize(intMeshFreed,
                             spatialDim, parametricDim,
                             intIsPresentNDG, intIsPresentEDG,
                             coordSys, 
                             buffer, length, offset,
                             inquireflag, rc,
                             buffer_l);

}


extern "C" void FTN_X(c_esmc_meshinfodeserialize)(int *intMeshFreed,
                                                  int *spatialDim, int *parametricDim,
                                                  int *intIsPresentNDG, int *intIsPresentEDG,
                                                  int *coordSys, 
                                                  char *buffer, int *offset,
                                                  int *rc,
                                                  ESMCI_FortranStrLenArg buffer_l){

  MeshCap::meshinfodeserialize(intMeshFreed,
                               spatialDim, parametricDim,
                               intIsPresentNDG, intIsPresentEDG,
                               coordSys, 
                               buffer, offset, rc,
                               buffer_l);
}


extern "C" void FTN_X(c_esmc_meshserialize)(MeshCap **meshpp,
                char *buffer, int *length, int *offset,
                ESMC_AttReconcileFlag *attreconflag,
                ESMC_InquireFlag *inquireflag, int *rc,
                ESMCI_FortranStrLenArg buffer_l){

  (*meshpp)->meshserialize(buffer, length, offset,
                           *attreconflag, inquireflag, false, rc, buffer_l);

}


extern "C" void FTN_X(c_esmc_meshdeserialize)(MeshCap **meshpp,
                             char *buffer, int *offset,
                             ESMC_AttReconcileFlag *attreconflag, int *rc,
                             ESMCI_FortranStrLenArg buffer_l){

  // Create MeshCap
  *meshpp=new MeshCap(-1);   // prevent baseID counter increment

  (*meshpp)->meshdeserialize(buffer, offset, *attreconflag, false, rc, buffer_l);
}


extern "C" void FTN_X(c_esmc_meshserializebase)(MeshCap **meshpp,
                char *buffer, int *length, int *offset,
                ESMC_AttReconcileFlag *attreconflag,
                ESMC_InquireFlag *inquireflag, int *rc,
                ESMCI_FortranStrLenArg buffer_l){

  (*meshpp)->meshserialize(buffer, length, offset,
                           *attreconflag, inquireflag, true, rc, buffer_l);

}


extern "C" void FTN_X(c_esmc_meshdeserializebase)(MeshCap **meshpp,
                             char *buffer, int *offset,
                             ESMC_AttReconcileFlag *attreconflag, int *rc,
                             ESMCI_FortranStrLenArg buffer_l){

  // Create MeshCap
  *meshpp=new MeshCap(-1);   // prevent baseID counter increment

  (*meshpp)->meshdeserialize(buffer, offset, *attreconflag, true, rc, buffer_l);
}


extern "C" void FTN_X(c_esmc_meshfindpnt)(MeshCap **meshpp, int *unmappedaction, int *dimPnts, int *numPnts,
                                        double *pnts, int *pets, int *rc){
  (*meshpp)->meshfindpnt(unmappedaction, dimPnts, numPnts,
                                  pnts, pets, rc);
}

extern "C" void FTN_X(c_esmc_getlocalcoords)(MeshCap **meshpp, double *nodeCoord, int *_orig_sdim, int *rc)
{

  (*meshpp)->getlocalcoords(nodeCoord, _orig_sdim, rc);
}

extern "C" void FTN_X(c_esmc_getlocalelemcoords)(MeshCap **meshpp, double *elemCoord, int *_orig_sdim, int *rc)
{

  (*meshpp)->getlocalelemcoords(elemCoord, _orig_sdim, rc);
}


extern "C" void FTN_X(c_esmc_meshgetarea)(MeshCap **meshpp, int *num_elem, double *elem_areas, int *rc) {

  (*meshpp)->meshgetarea(num_elem, elem_areas, rc);
}

extern "C" void FTN_X(c_esmc_meshgetdimensions)(MeshCap **meshpp, int *sdim, int *pdim, int *rc) {

  (*meshpp)->meshgetdimensions(sdim, pdim, rc);
}

extern "C" void FTN_X(c_esmc_meshgetcentroid)(MeshCap **meshpp, int *num_elem, double *elem_centroid, int *rc) {

  (*meshpp)->meshgetcentroid(num_elem, elem_centroid, rc);
}

extern "C" void FTN_X(c_esmc_meshgetfrac)(MeshCap **meshpp, int *_num_elem, double *elem_fracs, int *rc) {

    (*meshpp)->meshgetfrac(_num_elem, elem_fracs, rc);
}

extern "C" void FTN_X(c_esmc_meshgetfrac2)(MeshCap **meshpp, int *num_elem, double *elem_fracs, int *rc) {

     (*meshpp)->meshgetfrac2(num_elem, elem_fracs, rc);
}

// Interface to internal code to triangulate a polygon
// Input is: pnts (the polygon of size numPnts*sdim
//           td a temporary buffer of the same size as pnts
//           ti a temporary buffer of size numPnts
// Output is: tri_ind, which are the 0-based indices of the triangles
//             making up the triangulization. tri_ind should be of size 3*(numPnts-2).
//
extern "C" void FTN_X(c_esmc_triangulate)(int *pdim, int *sdim, int *numPnts,
                                        double *pnts, double *td, int *ti, int *triInd, int *rc){
  MeshCap::triangulate(pdim, sdim, numPnts,
                                    pnts, td, ti, triInd, rc);
}


extern "C" void FTN_X(c_esmc_meshturnoncellmask)(MeshCap **meshpp, ESMCI::InterArray<int> *maskValuesArg,  int *rc) {

    (*meshpp)->meshturnoncellmask(maskValuesArg, rc);
}

// Turn OFF masking
extern "C" void FTN_X(c_esmc_meshturnoffcellmask)(MeshCap **meshpp, int *rc) {

    (*meshpp)->meshturnoffcellmask(rc);
}

////////////
extern "C" void FTN_X(c_esmc_meshturnonnodemask)(MeshCap **meshpp, ESMCI::InterArray<int> *maskValuesArg,  int *rc) {

    (*meshpp)->meshturnonnodemask(maskValuesArg, rc);
}

// Turn OFF masking
extern "C" void FTN_X(c_esmc_meshturnoffnodemask)(MeshCap **meshpp, int *rc) {

    (*meshpp)->meshturnoffnodemask(rc);
}

////////////

extern "C" void FTN_X(c_esmc_get_polygon_area)(int *spatialdim, int *nedges,
                                                 double *points, double *area, int *rc) {
  MeshCap::get_polygon_area(spatialdim, nedges,
                                points, area, rc);
}

////////////////

extern "C" void FTN_X(c_esmc_meshcreatefrommeshes)(MeshCap **meshapp, MeshCap **meshbpp, MeshCap **meshpp,
ESMC_MeshOp_Flag * meshop, double * threshold, int *rc) {

  *meshpp=MeshCap::meshcreatefrommeshes(meshapp, meshbpp,
                               meshop, threshold, rc);
}


extern "C" void FTN_X(c_esmc_meshcreateredistelems)(MeshCap **src_meshpp, int *num_elem_gids, int *elem_gids,
                                                    MeshCap **output_meshpp, int *rc) {

  *output_meshpp=MeshCap::meshcreateredistelems(src_meshpp, num_elem_gids, elem_gids,
                                       rc);
}



extern "C" void FTN_X(c_esmc_meshcreateredistnodes)(MeshCap **src_meshpp, int *num_node_gids, int *node_gids,
                                                    MeshCap **output_meshpp, int *rc) {

  *output_meshpp=MeshCap::meshcreateredistnodes(src_meshpp, num_node_gids, node_gids,
                                          rc);
}



extern "C" void FTN_X(c_esmc_meshcreateredist)(MeshCap **src_meshpp, int *num_node_gids, int *node_gids,
                                               int *num_elem_gids, int *elem_gids,  MeshCap **output_meshpp, int *rc) {

  *output_meshpp=MeshCap::meshcreateredist(src_meshpp, num_node_gids, node_gids,
                                  num_elem_gids, elem_gids, rc);
}


extern "C" void FTN_X(c_esmc_meshcreatebase)(MeshCap **mesh, int *rc) {

  *mesh=new MeshCap();
  if (rc) *rc=ESMF_SUCCESS;
}

// This method verifies that nodes in node_gids array are the same as the local nodes in meshpp, otherwise
// it returns an error (used to test MeshRedist()).
// To do this check make sure the number of nodes in both cases are the same and that every
// entry in node_gids is contained in meshpp
extern "C" void FTN_X(c_esmc_meshchecknodelist)(MeshCap **meshpp, int *_num_node_gids, int *node_gids,
                                             int *rc) {
  (*meshpp)->meshchecknodelist(_num_node_gids, node_gids,
                               rc);

}


// This method verifies that elems in elem_gids array are the same as the local elems in meshpp, otherwise
// it returns an error (used to test MeshRedist()).
// To do this check make sure the number of elems in both cases are the same and that every
// entry in elem_gids is contained in meshpp
extern "C" void FTN_X(c_esmc_meshcheckelemlist)(MeshCap **meshpp, int *_num_elem_gids, int *elem_gids,
                                             int *rc) {
  (*meshpp)->meshcheckelemlist(_num_elem_gids, elem_gids,
                                        rc);
}

// Interface to internal code to convert coords from spherical in degrees to Cartesian
// Input is: lon, lat - spherical coordinates in degrees
// Output is: x,y,z - Cartesian coordinates
//
extern "C" void FTN_X(c_esmc_sphdeg_to_cart)(double *lon, double *lat,
                                             double *x, double *y, double *z, int *rc){
  MeshCap::sphdeg_to_cart(lon, lat, x, y, z, rc);
}


// This method sets the pole values so a 2D Mesh from a SCRIP grid can still be used in regrid with poles
extern "C" void FTN_X(c_esmc_meshsetpoles)(MeshCap **meshpp, int *_pole_obj_type, int *_pole_val,
                                           int *_min_pole_gid, int *_max_pole_gid,
                                           int *rc) {

  (*meshpp)->meshsetpoles(_pole_obj_type, _pole_val, _min_pole_gid, _max_pole_gid,
                          rc);
}


extern "C" void FTN_X(c_esmc_meshcreatedual)(MeshCap **src_meshpp, MeshCap **output_meshpp, int *rc) {

  *output_meshpp=MeshCap::meshcreatedual(src_meshpp, rc);
}


extern "C" void FTN_X(c_esmc_meshgetinternalptr)(MeshCap **meshpp,
                                                 void **internal_ptr, int *rc)
{
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_meshgetinternalptr"

  *internal_ptr=(*meshpp)->get_internal_mesh_ptr();

  // return success
  if (rc) *rc=ESMF_SUCCESS;

} // meshcreate


extern "C" void FTN_X(c_esmc_meshcreatefromintptr)(MeshCap **meshpp,
                                                void **ptr, int *rc)
{
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_meshcreatefromintptr()"

  // Create MeshCap for now assuming pointer is Mesh, need other way
  // to indicate it's MOAB, or wait until passing whole MeshCap struct
    *meshpp=MeshCap::create_from_ptr(ptr, true, rc);
}

extern "C" void FTN_X(c_esmc_meshfitonvm)(MeshCap **meshpp, VM **vm, int *rc) {

  (*meshpp)->fit_on_vm(vm,rc);
}

extern "C" void FTN_X(c_esmc_meshcreateeasyelems)(MeshCap **meshpp,
                                                  int *pdim,
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
                                                  int *rc)
{
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_meshcreateeasyelems()"

  // Create Mesh depending on whether MOAB or not
  if (Moab_on) {
    *meshpp=MeshCap::meshcreate_easy_elems(pdim, sdim,
                                num_elems, elemIdsII, elemTypes, elemMaskII,
                                num_elemCorners, elemCornerCoords,
                                has_elemArea, elemArea,
                                has_elemCoords, elemCoords,
                                coordSys,false,rc);
  } else {
    *meshpp=MeshCap::meshcreate_easy_elems(pdim, sdim,
                                num_elems, elemIdsII, elemTypes, elemMaskII,
                                num_elemCorners, elemCornerCoords,
                                has_elemArea, elemArea,
                                has_elemCoords, elemCoords,
                                coordSys,true,rc);
  }

} // meshcreate



extern "C" void FTN_X(c_esmc_meshcreatefromgrid)(MeshCap **meshpp,
                                                 Grid **gridpp,
                                                 char *name,
                                                 int *rc, 
                                                 ESMCI_FortranStrLenArg name_l)
{
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_meshcreatefromgrid()"

  int localrc = ESMC_RC_NOT_IMPL;

  // Create Mesh depending on whether MOAB or not
  if (Moab_on) {
    *meshpp=MeshCap::meshcreate_from_grid(gridpp,false,rc);
  } else {
    *meshpp=MeshCap::meshcreate_from_grid(gridpp,true,rc);
  }

  // copy and convert F90 string to null terminated one
  std::string cname(name, name_l);
  cname.resize(cname.find_last_not_of(" ")+1);

  if (cname.length() > 0) {
    localrc = (*meshpp)->ESMC_BaseSetName(cname.c_str(), "Mesh");
    ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));
    return;
  }

} // meshcreate


extern "C" void FTN_X(c_esmc_geteleminfointoarray)(MeshCap **meshpp,
                                                   DistGrid **elemDistgrid,
                                                   int *numElemArrays,
                                                   int *infoTypeElemArrays,
                                                   Array **elemArrays,
                                                   int *rc)
{

  (*meshpp)->geteleminfointoarray(*elemDistgrid,
                                  *numElemArrays,
                                  infoTypeElemArrays,
                                  elemArrays,
                                  rc);

}




#if 0
extern "C" void FTN_X(c_esmc_meshaddelements)(MeshCap **meshpp,
                                              int *_num_elems, int *elemId, int *elemType, InterArray<int> *_elemMaskII ,
                                              int *_areaPresent, double *elemArea,
                                              int *_coordsPresent, double *elemCoords,
                                              int *_num_elemConn, int *elemConn, int *regridConserve,
                                              ESMC_CoordSys_Flag *_coordSys, int *_orig_sdim,
                                              int *rc)
{
  // Call into implementation
  (*meshpp)->meshaddelements(_num_elems, elemId, elemType, _elemMaskII ,
                         _areaPresent, elemArea,
                         _coordsPresent, elemCoords,
                         _num_elemConn, elemConn, regridConserve,
                         _coordSys, _orig_sdim,
                         rc);
}
#endif
