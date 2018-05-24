// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

// Take out if MOAB isn't being used
#if defined ESMF_MOAB

#include <Mesh/include/Legacy/ESMCI_Exception.h>
#include <Mesh/include/Legacy/ESMCI_MeshObjConn.h>
#include <Mesh/include/Legacy/ESMCI_MeshUtils.h>
#include <Mesh/include/Legacy/ESMCI_MEValues.h>
#include <Mesh/include/Legacy/ESMCI_Polynomial.h>
#include <Mesh/include/Legacy/ESMCI_MeshField.h>
#include <Mesh/include/Legacy/ESMCI_MeshTypes.h>
#include <Mesh/include/ESMCI_MathUtil.h>
#include <Mesh/include/Legacy/ESMCI_Ftn.h>
#include <Mesh/include/Legacy/ESMCI_ParEnv.h>
#include <Mesh/include/Legacy/ESMCI_Sintdnode.h>
#include <Mesh/include/ESMCI_XGridUtil.h>
#include <Mesh/include/Legacy/ESMCI_Phedra.h>
#include <Mesh/include/ESMCI_MBMesh.h>
#include <Mesh/include/Regridding/ESMCI_WMat.h>
#include <Mesh/include/ESMCI_MBMesh_BBox.h>
#include <Mesh/include/ESMCI_MBMesh_Search_EToE.h>
#include <Mesh/include/ESMCI_MBMesh_Util.h>
#include <Mesh/include/Regridding/ESMCI_Interp.h>

#include <iostream>
#include <iterator>
#include <iomanip>
#include <cmath>
#include <vector>
#include <map>

#include <ESMCI_VM.h>
#include <ESMCI_LogErr.h>


//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif


using namespace ESMCI;

// NOTE::This finds the list of meshB elements which intersect with each meshA element and returns
//       it in sres

static int num_intersecting_elems(MBMesh *mbmp, const MBMesh_BBox &meshBBBox, double btol, double nexp) {

  int ret = 0;

  //Get MOAB Mesh
  Interface *moab_mesh=mbmp->mesh;

  // MOAB error
  int merr;

  // Get a range containing all elements
  Range range_elem;
  merr=moab_mesh->get_entities_by_dimension(0,mbmp->pdim,range_elem);
  if (merr != MB_SUCCESS) {
    Throw() << "MOAB ERROR:: "<<moab::ErrorCodeStr[merr];
  }

  // Loop over elements
  for(Range::iterator it=range_elem.begin(); it !=range_elem.end(); it++) {
    const EntityHandle elem=*it;

     const MBMesh_BBox bounding_box(mbmp, elem, nexp);

     // First check to see if the box even intersects the meshB mesh bounding
     // box.
     if (MBMesh_BBoxIntersect(meshBBBox, bounding_box, btol)) ++ret;
  }

  return ret;
}

  static void populate_box_elems(OTree *box, MBMesh_Search_EToE_Result_List &result, MBMesh *mbmp, const MBMesh_BBox &meshBBBox, double btol, double nexp) {

  // Get spatial dim of mesh
  int sdim = mbmp->sdim;

  //Get MOAB Mesh
  Interface *moab_mesh=mbmp->mesh;

  // MOAB error
  int merr;

  // Get a range containing all elements
  Range range_elem;
  merr=moab_mesh->get_entities_by_dimension(0,mbmp->pdim,range_elem);
  if (merr != MB_SUCCESS) {
    Throw() << "MOAB ERROR:: "<<moab::ErrorCodeStr[merr];
  }

  // Loop over elements
  for(Range::iterator it=range_elem.begin(); it !=range_elem.end(); it++) {
    const EntityHandle elem=*it;

     MBMesh_BBox bounding_box(mbmp, elem, nexp);

     // First check to see if the box even intersects the meshB mesh bounding
     // box.
     if (MBMesh_BBoxIntersect(meshBBBox, bounding_box, btol)) {

       // Create Search result
       MBMesh_Search_EToE_Result *sr=new MBMesh_Search_EToE_Result();
       sr->src_elem=elem;
       sr->dst_elems.clear();

       // Add it to results list
       result.push_back(sr);

       // Add it to tree
       double min[3], max[3];

       min[0] = bounding_box.getMin()[0] - btol;
       min[1] = bounding_box.getMin()[1] - btol;
       if (sdim >2) min[2] = bounding_box.getMin()[2] - btol;
       else min[2] = - btol;

       max[0] = bounding_box.getMax()[0] + btol;
       max[1] = bounding_box.getMax()[1] + btol;
       if (sdim >2) max[2] = bounding_box.getMax()[2] + btol;
       else  max[2] = btol;

       /*
       if (elem.get_id() == 2426) {
         std::cout << "elem 2426, bbox=" << bounding_box << std::endl;
       }*/

       // Add element to search tree
       box->add(min, max, (void*)sr);
     }

    }

  }


struct OctSearchElemsData {
  EntityHandle meshB_elem;
  bool found;
};

static int found_func_elems(void *c, void *y) {
  MBMesh_Search_EToE_Result *sr = static_cast<MBMesh_Search_EToE_Result*>(c);
  OctSearchElemsData *si = static_cast<OctSearchElemsData*>(y);


  // It might make sense to do something here to trim down the
  // number of candidates beyond just those that intersect the
  // minmax box of the search element. However, I'm not sure
  // that there is anything that would be more efficient than
  // just gathering them all and letting the clipping code
  // handle the detection of true intersection as is what
  // is currently being done.

  sr->dst_elems.push_back(si->meshB_elem);
  si->found=true;

  // Keep searching
  return 0;
}

// The main routine
// This constructs the list of meshB elements which intersects with each meshA element and returns
// this list in result. Each search_result in result contains a meshA element in elem and a list of intersecting meshB
 // elements in elem  This function is symmertrical with regard to meshA or meshB, and when used
// for regrid either src or dest mesh may be used for either
  void MBMesh_Search_EToE(MBMesh *mbmAp, int unmappedactionA, MBMesh *mbmBp, int unmappedactionB,
                      double stol, MBMesh_Search_EToE_Result_List &result) {
   Trace __trace("OctSearchElems(const Mesh &meshA, const Mesh &meshB, UInt meshB_obj_type, SearchResult &result, double stol, std::vector<const MeshObj*> *to_investigate");

  if (mbmAp->sdim != mbmBp->sdim) {
    Throw() << "Meshes must have same spatial dim for search";
  }

  // MOAB error
  int merr;

  // Get a bounding box for the meshB mesh.
  // TODO: NEED TO MAKE BOUNDING BOX ONLY DEPEND ON NON-MASKED ELEMENTS
  MBMesh_BBox meshBBBox(mbmBp);

  // declare some variables
  OTree *box=NULL;
  const double normexp = 0.15;
  const double meshBint = 1e-8;

  // Dimension of meshB
  UInt sdim = mbmBp->sdim;

  //Get MOAB Mesh
  Interface *moab_meshA=mbmAp->mesh;
  Interface *moab_meshB=mbmBp->mesh;


  /// Consstruct list of all meshB objects

  // Get a range containing all elements
  Range meshB_range_elem;
  merr=mbmBp->mesh->get_entities_by_dimension(0,mbmBp->pdim,meshB_range_elem);
  if (merr != MB_SUCCESS) {
    Throw() << "MOAB ERROR:: "<<moab::ErrorCodeStr[merr];
  }

  // Create vector
  std::vector<EntityHandle> meshB_elist;

  // Put into list depending if they are masked
  if (mbmBp->has_elem_mask){
     for(Range::iterator it=meshB_range_elem.begin(); it !=meshB_range_elem.end(); it++) {
      EntityHandle elem=*it;

        // Get elem mask value
        int masked;
        merr=mbmBp->mesh->tag_get_data(mbmBp->elem_mask_tag, &elem, 1, &masked);
        if (merr != MB_SUCCESS) {
          Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
        }

        // Add if not masked
        if (!masked) {
          meshB_elist.push_back(elem);
        }
    }
  } else {
    for(Range::iterator it=meshB_range_elem.begin(); it !=meshB_range_elem.end(); it++) {
      EntityHandle elem=*it;
      meshB_elist.push_back(elem);
    }
  }

  // Leave if nothing to search
  if (meshB_elist.size() == 0) return;

  // Count number of elements in tree
  int num_box = num_intersecting_elems(mbmAp, meshBBBox, meshBint, normexp);

  // Construct box tree
  box=new OTree(num_box);

  // Construct search result list
  result.reserve(num_box);

  // Fill tree with search result structs to fill
  // with intesecting elements
  populate_box_elems(box, result, mbmAp, meshBBBox, meshBint, normexp);
  box->commit();


  // Loop the mesh B elements, find the corresponding mesh A elements
  bool meshB_elem_not_found=false;
  for (UInt p = 0; p < meshB_elist.size(); ++p) {
    EntityHandle elem=meshB_elist[p];

    MBMesh_BBox meshB_bbox(mbmBp, elem, normexp);

    double min[3], max[3];
    min[0] = meshB_bbox.getMin()[0] - stol;
    min[1] = meshB_bbox.getMin()[1] - stol;
    if (sdim >2) min[2] = meshB_bbox.getMin()[2] - stol;
    else min[2] = - stol;

    max[0] = meshB_bbox.getMax()[0] + stol;
    max[1] = meshB_bbox.getMax()[1] + stol;
    if (sdim >2) max[2] = meshB_bbox.getMax()[2] + stol;
    else  max[2] = stol;

    OctSearchElemsData si;
    si.meshB_elem=elem;
    si.found=false;

    box->runon(min, max, found_func_elems, (void*)&si);

    if (!si.found) {
      meshB_elem_not_found=true;
    }

  } // for mesh B elems

  // Check for meshB elements which haven't been intersected
  if (meshB_elem_not_found) {
    if (unmappedactionB == ESMCI_UNMAPPEDACTION_ERROR) {
      Throw() << " Some mesh B elements do not intersect with mesh A";  
    } else if (unmappedactionB == ESMCI_UNMAPPEDACTION_IGNORE) {
      // don't do anything
    } else {
      Throw() << " Unknown unmappedaction option";
    }
  }

   // Get rid of box tree
  if (box != NULL) delete box;
}

#endif // ESMF_MOAB
