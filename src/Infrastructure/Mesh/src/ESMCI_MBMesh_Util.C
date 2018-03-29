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

#include "ESMCI_Macros.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"
#include <Mesh/include/ESMCI_Exception.h>
#include <Mesh/include/ESMCI_MBMesh.h>
#include <Mesh/include/ESMCI_WMat.h>
#include <Mesh/include/ESMCI_MathUtil.h>
#include <Mesh/include/ESMCI_MBMesh_BBox.h>
#include <Mesh/include/ESMCI_MBMesh_Search.h>
#include "Mesh/include/ESMCI_MeshTypes.h"

#include <iostream>
#include <iterator>
#include <iomanip>
#include <cmath>
#include <vector>

#include <ESMCI_VM.h>

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif


using namespace ESMCI;

// Get ids
void MBMesh_get_gid(MBMesh *mbmp, EntityHandle eh, int *gid) {
  int merr;

  merr=mbmp->mesh->tag_get_data(mbmp->gid_tag, &eh, 1, gid);
  if (merr != MB_SUCCESS) {
    Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
  }
}

// Get the coordinates of all the corners of an element
void MBMesh_get_elem_coords(MBMesh *mbmp, EntityHandle elem, int max_num_nodes, int *num_nodes, double *coords) {

  // MOAB Error
  int merr;

  // Coordinate dimension
  int sdim=mbmp->sdim;

  // Get Verts in element
  int num_verts;
  const EntityHandle *verts;
  merr=mbmp->mesh->get_connectivity(elem,verts,num_verts);
  if (merr != MB_SUCCESS) {
    Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
  }


  // make sure that we're not bigger than max size
  if (num_verts > max_num_nodes) {
    Throw() << "Element exceeds maximum poly size";
  }

  // Loop over verts
  int k=0;
  for(int i=0; i<num_verts; i++) {
    // Get vert coords
    double c[3];
    merr=mbmp->mesh->get_coords(verts+i,1,c);
    if (merr != MB_SUCCESS) {
      Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
    }

    // Load coords
    for (int j=0; j<sdim; j++) {
      coords[k]=c[j];
      k++;
    }
  }

  // Get number of nodes
  *num_nodes=num_verts;
}


// Get coords, but flip so always counter clockwise
// Also gets rid of degenerate edges
// This version only works for elements of parametric_dimension = 2 and spatial_dim=3
void MBMesh_get_elem_coords_3D_ccw(MBMesh *mbmp, EntityHandle elem,
                                   int max_num_nodes, double *tmp_coords,
                                   int *num_nodes, double *coords) {
    int num_tmp_nodes;

    // Get element coords
    MBMesh_get_elem_coords(mbmp, elem, max_num_nodes, &num_tmp_nodes, tmp_coords);

    // Remove degenerate edges
    remove_0len_edges3D(&num_tmp_nodes, tmp_coords);

    // Check if degenerate
    // (if there's less than 3 no notion of CCW or CW)
    if (num_tmp_nodes <3) {
      int j=0;
      for (int i=0; i<num_tmp_nodes; i++) {
        coords[j]=tmp_coords[j];
        j++;
        coords[j]=tmp_coords[j];
        j++;
        coords[j]=tmp_coords[j];
        j++;
      }
      *num_nodes=num_tmp_nodes;

      return;
    }

    // Get elem rotation
    bool left_turn;
    bool right_turn;
    rot_2D_3D_sph(num_tmp_nodes, tmp_coords, &left_turn, &right_turn);

    // Copy to output array swapping if necessary
    if (left_turn) {
      // Don't Swap
      int j=0;
      for (int i=0; i<num_tmp_nodes; i++) {
        coords[j]=tmp_coords[j];
        j++;
        coords[j]=tmp_coords[j];
        j++;
        coords[j]=tmp_coords[j];
        j++;
      }
    } else {
      // Swap
      int j=0; int k=3*(num_tmp_nodes-1);
      for (int i=0; i<num_tmp_nodes; i++) {
        coords[j]=tmp_coords[k];
        coords[j+1]=tmp_coords[k+1];
        coords[j+2]=tmp_coords[k+2];
        j+=3; k-=3;
      }
    }


  // Output num nodes
  *num_nodes=num_tmp_nodes;
}

// Compute centroid by averaging points
// centroid - the output centroid. Needs to be allocated to the spatial dimension of the mesh.
void MBMesh_get_elem_centroid(MBMesh *mbmp, EntityHandle elem, double *centroid) {

  // MOAB Error
  int merr;

  // Coordinate dimension
  int sdim=mbmp->sdim;

  // Get Verts in element
  int num_verts;
  const EntityHandle *verts;
  merr=mbmp->mesh->get_connectivity(elem,verts,num_verts);
  if (merr != MB_SUCCESS) {
    Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
  }

  // Init centroid to 0.0
  for (int d=0; d<sdim; d++) {
    centroid[d]=0.0;
  }

  // Loop over verts
  for(int i=0; i<num_verts; i++) {

    // Get vert coords
    double c[3];
    merr=mbmp->mesh->get_coords(verts+i,1,c);
    if (merr != MB_SUCCESS) {
      Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
    }

    // Add coords
    for (int d=0; d<sdim; d++) {
      centroid[d] += c[d];
    }
  }

  // Divide by number of points
  double inv_num=1.0/((double)num_verts);
  for (int d=0; d<sdim; d++) {
    centroid[d] *= inv_num;
  }
}

// Get elem gids owned by this processor and sorted in order of their original position
void MBMesh_get_local_elem_gids(MBMesh *mbmp, std::vector<UInt> &egids) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_get_local_elem_gids()"

  // Get localPet
  int localrc;
  int localPet = VM::getCurrent(&localrc)->getLocalPet();
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

  //Get MOAB Mesh
  Interface *moab_mesh=mbmp->mesh;

  // MOAB error
  int merr;


  // Get a range containing all elements
  Range range_elem;
  merr=moab_mesh->get_entities_by_dimension(0,mbmp->pdim,range_elem);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
       moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
  }

  // Loop through elements putting into list
  std::vector<std::pair<int,int> > pos_and_gids;
  for(Range::iterator it=range_elem.begin(); it !=range_elem.end(); it++) {
    const EntityHandle *elemp=(&*it);

    // Get owner
    int owner;
    merr=moab_mesh->tag_get_data(mbmp->owner_tag, elemp, 1, &owner);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                       moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
    }

    // If owned by this processor, put in list
    if (owner==localPet) {
      // Get gid
      int gid;
      merr=moab_mesh->tag_get_data(mbmp->gid_tag, elemp, 1, &gid);
      if (merr != MB_SUCCESS) {
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                         moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
      }

      // Get orig_pos
      int orig_pos;
      merr=moab_mesh->tag_get_data(mbmp->orig_pos_tag, elemp, 1, &orig_pos);
      if (merr != MB_SUCCESS) {
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                         moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
      }

      // Stick in list
      pos_and_gids.push_back(std::make_pair(orig_pos,gid));
    }
  }

  // Put in order by original pos
  std::sort(pos_and_gids.begin(), pos_and_gids.end());

  // Fill array of element gids
  egids.clear();
  for (int i = 0; i<pos_and_gids.size(); ++i) {
    egids.push_back((UInt)pos_and_gids[i].second);

    // printf("pos=%d egids=%d\n",pos_and_gids[i].first,pos_and_gids[i].second);

  }
}

// expects pcoords to have 3 elements
// expects pcoords in domain [-1,1] and translates to [0,1]
// useful for translating pcoords from MOAB to ESMF domain
void translate(double *pcoords) {
#undef  ESMC_METHOD
#define ESMC_METHOD "translate"

  pcoords[0] = (pcoords[0]+1)/2;
  pcoords[1] = (pcoords[1]+1)/2;
  pcoords[2] = (pcoords[2]+1)/2;

}


// This method converts a Mesh to a PointList
 ESMCI::PointList *MBMesh_to_PointList(MBMesh *mesh, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMeshToPointList()"

  ESMCI::PointList *plp = NULL;

  int localrc;
//  MEField<> *cfield;
//  MEField<> *src_mask_val;
//  Mesh::MeshObjIDMap::const_iterator mb,mi,me;

  // Get localPet
  int localPet = VM::getCurrent(&localrc)->getLocalPet();
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

  //Get MOAB Mesh
  Interface *moab_mesh=mesh->mesh;

  // MOAB error
  int merr;

/* disable masking for now
  if (meshLoc == ESMC_MESHLOC_NODE) {

    //     cfield = GetCoordField();
    cfield = GetField("coordinates");
    if (cfield == NULL) {
      int localrc;
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
         "- mesh node coordinates unavailable",
         ESMC_CONTEXT, &localrc)) throw localrc;
    }
    mb = map_begin(MeshObj::NODE);
    me = map_end(MeshObj::NODE);

    src_mask_val = GetField("node_mask_val");

  } else if (meshLoc == ESMC_MESHLOC_ELEMENT) {

    //need check here to see that elem coordinates are set
    cfield = GetField("elem_coordinates");
    if (cfield == NULL) {

      MBMesh_get_elem_coords(MBMesh *mbmp, EntityHandle elem, int max_num_nodes, int *num_nodes, double *coords);

      int localrc;
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
         "- mesh element coordinates unavailable",
         ESMC_CONTEXT, &localrc)) throw localrc;
    }
    mb = map_begin(MeshObj::ELEMENT);
    me = map_end(MeshObj::ELEMENT);

    src_mask_val = GetField("elem_mask_val");

  } else {
    //unknown meshLoc
    int localrc;
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
       "- illegal value specified for mesh location",
       ESMC_CONTEXT, &localrc)) throw localrc;
  }
*/

  //int numMaskValues;
  //int *ptrMaskValues;

  //if (src_mask_val==NULL) {         //no masking info in mesh

    Range range_node;
    merr=moab_mesh->get_entities_by_dimension(0,0,range_node);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
        moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
    }

    int num_local_pts=0;
    for(Range::iterator it=range_node.begin(); it !=range_node.end(); it++) {
      const EntityHandle *nodep=(&*it);

      // Get owner
      int owner;
      merr=moab_mesh->tag_get_data(mesh->owner_tag, nodep, 1, &owner);
      if (merr != MB_SUCCESS) {
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
          moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
      }
      if (owner == localPet) num_local_pts++;
    }

    // number of nodes
    int num_nodes;
    merr = moab_mesh->get_number_entities_by_dimension(0, 0, num_nodes);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
        moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
    }

    // Create PointList
    // RLO: now this is using a potential different number of nodes
    plp = new PointList(num_local_pts, mesh->sdim);

    // Loop through adding local nodes
    for(Range::iterator it=range_node.begin(); it !=range_node.end(); it++) {
      const EntityHandle *nodep=(&*it);

      // Get the owner again..
      int owner;
      merr=moab_mesh->tag_get_data(mesh->owner_tag, nodep, 1, &owner);
      if (merr != MB_SUCCESS)
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
          moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;

      // if this node is owned, add it to the pointlist
      double c[3];
      //vector<EntityHandle> nodev;
      if (owner == localPet) {

        merr = moab_mesh->get_coords(nodep, 1, c);
        if (merr != MB_SUCCESS)
          if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
            moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
        int id = moab_mesh->id_from_handle(*nodep);
        
        plp->add(id, c);

      }
    }


  /* masking disable for now *** else {
  }

    if (present(maskValuesArg)) {
      numMaskValues=maskValuesArg->extent[0];
      ptrMaskValues=&(maskValuesArg->array[0]);
    } else {
      numMaskValues=0;
      ptrMaskValues = NULL;
    }

    int num_local_pts=0;
    for (mi=mb; mi != me; ++mi) {
      const MeshObj &obj = *mi;

      if (GetAttr(obj).is_locally_owned()) {
        double *mv=src_mask_val->data(*mi);
        int mv_int = (int)((*mv)+0.5);

        // Only put objects in if they're not masked
        bool mask=false;
        for (int i=0; i<numMaskValues; i++) {
          int mvi=ptrMaskValues[i];

          if (mv_int == mvi) {
            mask=true;
            break;
          }
        }
        if (!mask)
          num_local_pts++;
      }
    }

    // Create PointList
    plp = new PointList(num_local_pts,mesh->sdim);

    // Loop through adding local nodes
    for (mi=mb; mi != me; ++mi) {
      const MeshObj &obj = *mi;

      if (GetAttr(obj).is_locally_owned()) {
        double *mv=src_mask_val->data(*mi);
        int mv_int = (int)((*mv)+0.5);

        // Only put objects in if they're not masked
        bool mask=false;
        for (int i=0; i<numMaskValues; i++) {
          int mvi=ptrMaskValues[i];

          if (mv_int == mvi) {
            mask=true;
            break;
          }
        }
        if (!mask) {
          double *coords=cfield->data(obj);
          plp->add(obj.get_id(),coords);
        }
      }
    }
  }*/

  if (rc!=NULL) *rc=ESMF_SUCCESS;
  return plp;

}


#endif // ESMF_MOAB
