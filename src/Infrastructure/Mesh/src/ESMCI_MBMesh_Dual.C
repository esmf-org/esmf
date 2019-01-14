// $Id: ESMCI_MeshRedist.C,v 1.23 2012/01/06 20:17:51 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 
// 2002-2019, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================


#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"


#if defined ESMF_MOAB
#include "moab/Core.hpp"
#include "MBTagConventions.hpp"
#include "moab/ParallelComm.hpp"
using namespace moab;

#include <Mesh/include/ESMCI_MBMesh.h>
#include <Mesh/include/ESMCI_MBMesh_Dual.h>
#include <Mesh/include/ESMCI_MBMesh_Glue.h>
#include <Mesh/include/ESMCI_MBMesh_Types.h>

#include <Mesh/include/ESMCI_MathUtil.h>
#endif


#include <iostream>
#include <fstream>
#include <algorithm>
#include <iterator>
#include <ostream>
#include <set>
#include <limits>
#include <vector>

using namespace std;

// #define DEBUG_SPLIT
// #define DEBUG_CONNECTIVITY
// #define DEBUG_CONNECTIVITY_ADJACENCIES
// #define DEBUG_MASK
// #define DEBUG_WRITE_MESH

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_MeshDual.C,v 1.23 2012/01/06 20:17:51 svasquez Exp $";
//-----------------------------------------------------------------------------


 
namespace ESMCI {
  
#if defined ESMF_MOAB

  struct MDSS {
    double angle;
    int id;

    MDSS() {
      angle=0.0;
      id=0;
    }

    MDSS &operator= (const MDSS &rhs) {
      angle=rhs.angle;
      id=rhs.id;
      return *this;
    }

    bool operator< (const MDSS &rhs) const {
      return angle < rhs.angle;
    }

  };

  void add_ghost_elems_to_split_orig_id_map(MBMesh *mesh);

  void get_unique_elems_around_node(const EntityHandle *node, MBMesh *mesh, 
                                    MDSS *tmp_mdss, int *_num_ids, int *ids);

  void mb_triangulate(int sdim, int num_p, double *p, double *td, int *ti, int *tri_ind, 
                   double *tri_frac);

  // Create a dual of the input Mesh 
  // This adds ghostcells to the input mesh, 
  // it also creates ghostcells for the dual mesh
  void MBMeshDual(MBMesh *src_mesh, MBMesh **_dual_mesh, int *rc) {

#undef  ESMC_METHOD
#define ESMC_METHOD "MBMeshDual()"

  Trace __trace("MeshDual(MBMesh *src_mesh, MBMesh **dual_mesh)");

  int merr, localrc;

  // Do this for now instead of initiating mesh parallel stuff
  // TODO: MAYBE EVENTUALLY PUT THIS INTO MBMesh???
  MPI_Comm mpi_comm;
  mpi_comm=VM::getCurrent(&localrc)->getMpi_c();
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

  // Get localPet
  int localPet = VM::getCurrent(&localrc)->getLocalPet();
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

  // Don't currently support duals of 3D Meshes
  if (src_mesh->pdim>2) {
    Throw() <<" Creation of a dual mesh isn't supported for Meshes of parametric dim greater than 3.\n";
  }

  // Need element coordinates
  if (!src_mesh->has_elem_coords) {
    Throw() <<" Creation of a dual mesh requires element coordinates. \n";
  }

  // TODO: add elem mask fields and mask_val fields   
  src_mesh->CreateGhost();

#ifdef DEBUG_WRITE_MESH
  {void *mbptr = (void *) src_mesh;
  int len = 16; char fname[len];
  sprintf(fname, "meshsrcghost_%d", Par::Rank());
  MBMesh_write(&mbptr, fname, rc, len);}
#endif

#ifdef DEBUG_CONNECTIVITY_ADJACENCIES
  {
  // Get a range containing all nodes
  Range range_node;
  merr=src_mesh->mesh->get_entities_by_dimension(0,0,range_node);
  MBMESH_CHECK_ERR(merr, localrc);

  for(Range::iterator it=range_node.begin(); it !=range_node.end(); it++) {
    const EntityHandle *node=&(*it);

    Range adjs;
    merr = src_mesh->mesh->get_adjacencies(node, 1, src_mesh->pdim, false, adjs);
    MBMESH_CHECK_ERR(merr, localrc);

    {int nid;
    merr=src_mesh->mesh->tag_get_data(src_mesh->gid_tag, node, 1, &nid);
    MBMESH_CHECK_ERR(merr, localrc);
    printf("%d# mesh node id %d, adjacencies %d [", Par::Rank(), nid, adjs.size());
    for(Range::iterator it=adjs.begin(); it !=adjs.end(); it++) {
      const EntityHandle *elem=&(*it);
      
      // Get element id
      int elem_id;
      merr = src_mesh->mesh->tag_get_data(src_mesh->gid_tag, elem, 1, &elem_id);
      MBMESH_CHECK_ERR(merr, localrc);
      
      printf("%d, ", elem_id);
    }
    printf("]\n");}
    
  }
  }
#endif


  // If src_mesh is split, add newly created ghost elements to split_to_orig map
  if (src_mesh->is_split) add_ghost_elems_to_split_orig_id_map(src_mesh);

#ifdef DEBUG_SPLIT
  printf("%d# split_to_orig_id map [", Par::Rank());
  map<int, int>::iterator it=src_mesh->split_to_orig_id.begin();
  for (it; it!=src_mesh->split_to_orig_id.end(); ++it)
    printf("%d:%d, ", it->first, it->second);
  printf("]\n");
#endif

  // Get some useful info
  int sdim=src_mesh->sdim;
  int pdim=src_mesh->pdim;

  // Create Mesh
  MBMesh *dual_mesh=NULL;
  void *dmp=NULL;
  ESMC_CoordSys_Flag cs = ESMC_COORDSYS_CART;
  if (src_mesh->sdim != src_mesh->orig_sdim) cs = ESMC_COORDSYS_SPH_DEG;
  
  MBMesh_create(&dmp, &pdim, &src_mesh->orig_sdim, &cs, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

  // dual_mesh = dynamic_cast<MBMesh *> (dmp);
  dual_mesh = (MBMesh *) (dmp);

  // set some flags that are normally set in the addnodes and addelements calls
  dual_mesh->has_node_orig_coords = false;
  dual_mesh->has_node_mask = false;
  dual_mesh->has_elem_frac = false;
  dual_mesh->has_elem_mask = false;
  dual_mesh->has_elem_area = false;
  dual_mesh->has_elem_coords = false;
  dual_mesh->has_elem_orig_coords = false;
  // is_split too?

  // Iterate through all src elements counting the number and creating a map

  // Get a range containing all elements
  Range range_elem;
  merr=src_mesh->mesh->get_entities_by_dimension(0,src_mesh->pdim,range_elem);
  MBMESH_CHECK_ERR(merr, localrc);

  std::map<int,int> id_to_index;
  int pos=0;
  for(Range::iterator it=range_elem.begin(); it !=range_elem.end(); it++) {
    const EntityHandle *elem=&(*it);

    // Get element id
    int elem_id;
    merr=src_mesh->mesh->tag_get_data(src_mesh->gid_tag, elem, 1, &elem_id);
    MBMESH_CHECK_ERR(merr, localrc);

    // Translate id if split
    if ((src_mesh->is_split) && (elem_id > src_mesh->max_non_split_id)) {
      std::map<int,int>::iterator soi =  src_mesh->split_to_orig_id.find(elem_id);
#ifdef DEBUG_SPLIT
      printf("%d# split elem id %d > max_non_split_id %d\n", Par::Rank(), elem_id, src_mesh->max_non_split_id);
#endif
      if (soi != src_mesh->split_to_orig_id.end()) {
        elem_id=soi->second;
      } else {
#ifdef DEBUG_SPLIT
        printf("%d# split elem id %d NOT FOUND\n", Par::Rank(), elem_id);
#endif
        Throw() << "split elem id not found in map";
      }
    }

    // Use insert because once a key is in the map, it doesn't change the entry.
    // This means with a split elem, the orig elem id always points to the first
    // split elem encountered. This make things a bit clearer and slighly more efficient
    // than having it move around. Its possible that it may also work the other way, 
    // but I haven't tested it. 
    id_to_index.insert(std::make_pair(elem_id,pos));
    
    // Next pos
    pos++;
  }

#ifdef DEBUG_CONNECTIVITY
  {printf("%d# idtoindex list [", Par::Rank());
  std::map<int, int>::iterator it = id_to_index.begin();
  while (it != id_to_index.end()) {
    printf("%d, ", it->first);
    it++;
  }
  printf("]\n");}
#endif

  // Number of local nodes
  int num_nodes=pos;

  // Allocate vector to record which nodes are used
  int *nodes_used=NULL;
  if (num_nodes >0) nodes_used=new int[num_nodes];
  for (int i=0; i<num_nodes; i++) {
    nodes_used[i]=0;
  }


  // Iterate through src nodes counting sizes
  // Note that the elems around the node are the maximum possible, it
  // could be less when actually counted and uniqued. 
  int max_num_elems=0;
  int max_num_elemConn=0;
  int max_num_node_elems=0;
  // Get a range containing all nodes
  Range range_node;
  merr=src_mesh->mesh->get_entities_by_dimension(0,0,range_node);
  MBMESH_CHECK_ERR(merr, localrc);

  for(Range::iterator it=range_node.begin(); it !=range_node.end(); it++) {
    const EntityHandle *node=&(*it);
    
    // Only do local nodes
    // ALSO DO NON-LOCAL NODES, BECAUSE OTHERWISE YOU 
    // CAN END UP NOT MAKING AN ELEM AS A HOME FOR 
    // A NODE THAT'S NEEDED ON ANOTHER PROC
    //if (!GetAttr(node).is_locally_owned()) continue;
    
    // Get number of elems
    int num_node_elems=0;
    // get_num_elems_around_node(&node, &num_node_elems);
    // pdim instead of sdim here, for spherical cases
    Range adjs;
    merr = src_mesh->mesh->get_adjacencies(node, 1, pdim, false, adjs);
    MBMESH_CHECK_ERR(merr, localrc);
    num_node_elems = adjs.size();

#ifdef DEBUG_CONNECTIVITY_ADJACENCIES
    {int nid;
    merr=src_mesh->mesh->tag_get_data(src_mesh->gid_tag, node, 1, &nid);
    MBMESH_CHECK_ERR(merr, localrc);
    printf("%d# mesh node id %d, adjacencies %d [", Par::Rank(), nid, num_node_elems);
    for(Range::iterator it=adjs.begin(); it !=adjs.end(); it++) {
      const EntityHandle *elem=&(*it);
      
      // Get element id
      int elem_id;
      merr = src_mesh->mesh->tag_get_data(src_mesh->gid_tag, elem, 1, &elem_id);
      MBMESH_CHECK_ERR(merr, localrc);
      
      printf("%d, ", elem_id);
    }
    printf("]\n");}
#endif
    
    // If less than 3 (a triangle) then don't make an element
    if (num_node_elems < 3) continue;
    
    // maximum number of elems per node
    if (num_node_elems > max_num_node_elems) max_num_node_elems = num_node_elems;
    
    // Count number of elements
    max_num_elems++;
    
    // Count number of connections
    max_num_elemConn += num_node_elems;
  }
#ifdef DEBUG_CONNECTIVITY
  printf("%d# max_num_elems %d max_num_elemConn %d\n", Par::Rank(), max_num_elems, max_num_elemConn);
#endif

  // Create temp arrays for getting ordered elem ids
  MDSS *tmp_mdss=NULL;
  int *elems_around_node_ids=NULL;
  if (max_num_node_elems > 0) {
    tmp_mdss=new MDSS[max_num_node_elems];
    elems_around_node_ids=new int[max_num_node_elems];
  }

  // Create element lists
  int *elemType=NULL;
  int *elemId=NULL;
  int *elemOwner=NULL;
  if (max_num_elems>0) {
    elemType=new int[max_num_elems];
    elemId=new int[max_num_elems];
    elemOwner=new int[max_num_elems];
  }
  int *elemConn=NULL;
  if (max_num_elemConn >0) {
    elemConn=new int[max_num_elemConn];
  }

  // Iterate through src nodes creating elements
  int num_elems=0;
  int conn_pos=0;
  for(Range::iterator it=range_node.begin(); it !=range_node.end(); it++) {
    const EntityHandle *node=&(*it);
    
    // Only do local nodes
    // ALSO DO NON-LOCAL NODES, BECAUSE OTHERWISE YOU 
    // CAN END UP NOT MAKING AN ELEM AS A HOME FOR 
    // A NODE THAT'S NEEDED ON ANOTHER PROC
    //  if (!GetAttr(node).is_locally_owned()) continue;
    

    // Get list of element ids
    int num_elems_around_node_ids=0;
    get_unique_elems_around_node(node, src_mesh, tmp_mdss,
                          &num_elems_around_node_ids,
                          elems_around_node_ids);
    
    // If less than 3 (a triangle) then don't make an element
    if (num_elems_around_node_ids < 3) continue;
    
    // Save elemType/number of connections 
    elemType[num_elems]=num_elems_around_node_ids;
    
    // Save elemId
    int elem_id;
    merr=src_mesh->mesh->tag_get_data(src_mesh->gid_tag, node, 1, &elem_id);
    MBMESH_CHECK_ERR(merr, localrc);
    elemId[num_elems]=elem_id;

    // Save owner
    int owner;
    merr=src_mesh->mesh->tag_get_data(src_mesh->owner_tag, node, 1, &owner);
    MBMESH_CHECK_ERR(merr, localrc);
    elemOwner[num_elems]=owner;
    
    // Next elem
    num_elems++;

#ifdef DEBUG_CONNECTIVITY
    printf("  %d# elem %d, nodes [", localPet, elem_id);
    for (int i = 0; i < num_elems_around_node_ids; ++i)
      printf("%d,", elems_around_node_ids[i]);
    printf("]\n");
#endif

    // Loop elements attached to node and build connection list
    for (int i=0; i<num_elems_around_node_ids; i++) {

      // Get elem id
      int elem_id2=elems_around_node_ids[i];
      
      // Get index of this element
      int node_index=id_to_index[elem_id2];
      
      // Record that this node was used
      nodes_used[node_index]=1;

      // Push connection
      elemConn[conn_pos]=node_index;

#ifdef DEBUG_CONNECTIVITY
  printf("%d# %d of %d: eani %d -> id2i %d [%d]\n", localPet, i, num_elems_around_node_ids, elem_id2, node_index, conn_pos);
#endif

      // Next connection
      conn_pos++;
    }

    // printf("\n");
  }
  
#ifdef DEBUG_CONNECTIVITY
  printf("%d# elemConn[", localPet);
  for (int i = 0; i < conn_pos; ++i) printf("%d,", elemConn[i]);
  printf("]\n");
#endif

  // Free tmp arrays
  if (tmp_mdss != NULL) delete [] tmp_mdss;
  if (elems_around_node_ids != NULL) delete [] elems_around_node_ids;



////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// BIG CHANGES ////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

  // Iterate through all src elements creating nodes
  EntityHandle *nodes=NULL;
  if (num_nodes>0) {
    nodes=new EntityHandle[num_nodes];
    dual_mesh->verts=nodes;
  }
  dual_mesh->num_verts = num_nodes;

#ifdef DEBUG_CONNECTIVITY
  {Range elemss;
  merr=src_mesh->mesh->get_entities_by_dimension(0, src_mesh->pdim, elemss);
  MBMESH_CHECK_ERR(merr, localrc);
  printf("%d# range size = %d\n", localPet, elemss.size());}
#endif

  
  pos=0;
  int data_index=0;
  for(Range::iterator it=range_elem.begin(); it !=range_elem.end(); it++) {
    const EntityHandle *elem=&(*it);

      // Get element id
      int elem_id;
      merr=src_mesh->mesh->tag_get_data(src_mesh->gid_tag, elem, 1, &elem_id);
      MBMESH_CHECK_ERR(merr, localrc);

      // Translate id if split
      if ((src_mesh->is_split) && (elem_id > src_mesh->max_non_split_id)) {
        std::map<int,int>::iterator soi =  src_mesh->split_to_orig_id.find(elem_id);
        if (soi != src_mesh->split_to_orig_id.end()) {
          elem_id=soi->second;
        } else {
          Throw() << "split elem id not found in map";
        }
      }

      // Get owner
      int owner;
      merr=src_mesh->mesh->tag_get_data(src_mesh->owner_tag, elem, 1, &owner);
      MBMESH_CHECK_ERR(merr, localrc);

      // Translate owner if split
      // (Interestingly we DON'T have to translate the owner for
      // split elems (in a rare example of not having to do extra work :-))
      // the reason is if elem is split, then it will have the same owner
      // as the original elem/id that it was split from, so just getting owner from elem works
      // even if it's split)


      // Only create if used
      // (Note we are also using nodes_used to skip collapsed split elems, so unused here 
      //  might also mean it was a split elem, that's not the original elem)
      if (nodes_used[pos]) {

        // get coords of this element to put onto the new node
        double c[3];
        // merr = src_mesh->mesh->get_coords(elem, 1, c);
        merr=src_mesh->mesh->tag_get_data(src_mesh->elem_coords_tag, elem, 1, c);
        MBMESH_CHECK_ERR(merr, localrc);

        // Add vertex
        EntityHandle new_node = 0;
        merr=dual_mesh->mesh->create_vertex(c,new_node);
        MBMESH_CHECK_ERR(merr, localrc);
        nodes[pos] = new_node;

#ifdef DEBUG_CONNECTIVITY
        printf("PET %d add vertex %d [%d] at [%f,%f,%f], owner %d\n", localPet, elem_id, pos, c[0], c[1], c[2], owner);
#endif
        // Set Ids
        merr=dual_mesh->mesh->tag_set_data(dual_mesh->gid_tag, &new_node, 
          1, &elem_id);
        MBMESH_CHECK_ERR(merr, localrc);

        // Set Owners
        merr=dual_mesh->mesh->tag_set_data(dual_mesh->owner_tag, 
          &new_node, 1, &owner);
        MBMESH_CHECK_ERR(merr, localrc);

        // Set orig_pos
        merr=dual_mesh->mesh->tag_set_data(dual_mesh->orig_pos_tag, 
          &new_node, 1, &data_index);
        MBMESH_CHECK_ERR(merr, localrc);

        // Set original coords
        if (dual_mesh->has_node_orig_coords) {
          // Set original coords
          merr=dual_mesh->mesh->tag_set_data(dual_mesh->node_orig_coords_tag, 
            &new_node, 1, c);
          MBMESH_CHECK_ERR(merr, localrc);
        }
        
        // masking
        if (src_mesh->has_elem_mask) {
          // if the dual_mesh node mask is not yet set up, intialize to unmasked
          if (dual_mesh->has_node_mask == false) {
            int int_def_val=0; // So things are by default not masked
            merr=dual_mesh->mesh->tag_get_handle("node_mask", 1, MB_TYPE_INTEGER,
              dual_mesh->node_mask_tag, MB_TAG_EXCL|MB_TAG_DENSE, &int_def_val);
            MBMESH_CHECK_ERR(merr, localrc);
            
            int_def_val=0; // So things are by default not masked
            merr=dual_mesh->mesh->tag_get_handle("node_mask_val", 1, MB_TYPE_INTEGER, 
              dual_mesh->node_mask_val_tag, MB_TAG_EXCL|MB_TAG_DENSE, &int_def_val);
            MBMESH_CHECK_ERR(merr, localrc);
            
            // node mask is now initialized
            dual_mesh->has_node_mask = true;
          }
          
          // now set individual node mask values, based on elem mask
          int elem_mask;
          merr=src_mesh->mesh->tag_get_data(src_mesh->elem_mask_tag, elem, 1, &elem_mask);
          MBMESH_CHECK_ERR(merr, localrc);
          
          int elem_mask_val;
          merr=src_mesh->mesh->tag_get_data(src_mesh->elem_mask_val_tag, elem, 1, &elem_mask_val);
          MBMESH_CHECK_ERR(merr, localrc);
          
          // set the elem mask value in the node mask
          merr=dual_mesh->mesh->tag_set_data(dual_mesh->node_mask_tag, 
            &new_node, 1, &elem_mask);
          MBMESH_CHECK_ERR(merr, localrc);

          merr=dual_mesh->mesh->tag_set_data(dual_mesh->node_mask_val_tag, 
            &new_node, 1, &elem_mask_val);
          MBMESH_CHECK_ERR(merr, localrc);
        }
        
        data_index++;
      }

      // Next pos
      pos++;
    }

////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// BIG CHANGES ////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

#ifdef DEBUG_MASK
  if (dual_mesh->has_node_mask) {
    Range nodes;
    merr=dual_mesh->mesh->get_entities_by_dimension(0, 0, nodes);
    MBMESH_CHECK_ERR(merr, localrc);

    int num_verts = nodes.size();
    int src_node_mask[num_verts];
    int node_id[num_verts];

  printf("%d# - has_node_mask == %s\n", Par::Rank(), dual_mesh->has_node_mask ? "true" : "false");

    merr=dual_mesh->mesh->tag_get_data(dual_mesh->node_mask_val_tag, nodes, &src_node_mask);
    MBMESH_CHECK_ERR(merr, localrc);

    merr=dual_mesh->mesh->tag_get_data(dual_mesh->gid_tag, nodes, &node_id);
    MBMESH_CHECK_ERR(merr, localrc);

    printf("%d# src_node_id = [", Par::Rank());
    for (int i = 0; i < num_verts; ++i)
      printf("%d, ", node_id[i]);
    printf("]\n");

    printf("%d# src_node_mask = [", Par::Rank());
    for (int i = 0; i < num_verts; ++i)
      printf("%d, ", src_node_mask[i]);
    printf("]\n");
  }
#endif



    // Check for Split 
    // Count the number of extra elements we need for splitting
    int num_extra_elem=0;
    int max_num_conn=0;
    for (int e = 0; e < num_elems; ++e) {
      if (elemType[e] >4) {
        num_extra_elem += (elemType[e]-3); // Original elem + # sides-2
      }
      
      if (elemType[e] > max_num_conn) max_num_conn=elemType[e];
    }

    int tot_num_extra_elem=0;
    MPI_Allreduce(&num_extra_elem,&tot_num_extra_elem,1,MPI_INT,MPI_SUM,mpi_comm);

    // If there's num_extra_elem than it's a split mesh
    if (tot_num_extra_elem>0) {
      dual_mesh->is_split=true;
    } else {
      dual_mesh->is_split=false;
    }


  // Compute the extra element ranges
  int beg_extra_ids=0;
  if (dual_mesh->is_split) {      
    // get maximum local elem id      
    int max_id=0;
    for (int e = 0; e < num_elems; ++e) {
      if (elemId[e] > max_id) {
        max_id=elemId[e];
      }
    }

    // Calc global max id
    int global_max_id=0;
    MPI_Allreduce(&max_id,&global_max_id,1,MPI_INT,MPI_MAX,mpi_comm);
    
    // Set maximum of non-split ids
    dual_mesh->max_non_split_id=global_max_id;

    // Calc our range of extra elem ids
    beg_extra_ids=0;
    MPI_Scan(&num_extra_elem,&beg_extra_ids,1,MPI_INT,MPI_SUM,mpi_comm);
    
    // Remove this processor's number from the sum to get the beginning
    beg_extra_ids=beg_extra_ids-num_extra_elem;
    
    // Start 1 up from max
    beg_extra_ids=beg_extra_ids+global_max_id+1;
  }


  // Generate connectivity list with split elements
  // TODO: MAYBE EVENTUALLY PUT EXTRA SPLIT ONES AT END
  int num_elems_wsplit=0;
  int *elemConn_wsplit=NULL;
  int *elemType_wsplit=NULL;
  int *elemId_wsplit=NULL;
  int *elemOwner_wsplit=NULL;

  //  int *elemMaskIIArray_wsplit=NULL;
  //InterArray *elemMaskII_wsplit=NULL;

  if (dual_mesh->is_split) {
    // New number of elements
    num_elems_wsplit=num_elems+num_extra_elem;

    // Allocate arrays to hold split lists
    elemConn_wsplit=new int[max_num_elemConn+3*num_extra_elem];
    elemType_wsplit=new int[num_elems_wsplit];
    elemId_wsplit=new int[num_elems_wsplit];
    elemOwner_wsplit=new int[num_elems_wsplit];

#if 0
      //// Setup for split mask
      int *elemMaskIIArray=NULL;
      if (elemMaskII != NULL) { 

        // Get mask value array
        elemMaskIIArray=elemMaskII->array;

        int extent[1];
        elemMaskIIArray_wsplit=new int[num_elems_wsplit];

        extent[0]=num_elems_wsplit;
        elemMaskII_wsplit=new InterArray(elemMaskIIArray_wsplit,1,extent);
      }
#endif

    // Allocate some temporary variables for splitting
    double *polyCoords=new double[3*max_num_conn];
    double *polyDblBuf=new double[3*max_num_conn];
    int    *polyIntBuf=new int[max_num_conn];
    int    *triInd=new int[3*(max_num_conn-2)];
    double *triFrac=new double[max_num_conn-2];

    // new id counter
    int curr_extra_id=beg_extra_ids;

    // Loop through elems generating split elems if necessary
    int conn_pos = 0;
    int split_conn_pos = 0;
    int split_elem_pos = 0;
    for (int e = 0; e < num_elems; ++e) {

      // More than 4 side, split
      if (elemType[e]>4) {

        // Get coordinates
        int crd_pos=0;
        for (int i=0; i<elemType[e]; i++) {
          EntityHandle node=nodes[elemConn[conn_pos+i]];

#ifdef DEBUG_CONNECTIVITY
          // get node id
          int nid;
          merr=dual_mesh->mesh->tag_get_data(dual_mesh->gid_tag, &node, 1, &nid);
          MBMESH_CHECK_ERR(merr, localrc);

          printf("PET %d node id %d of %d, node %d [%d]\n", Par::Rank(), nid, elemType[e], elemConn[conn_pos+i], conn_pos+i);
#endif
          double crd[3];
          merr = dual_mesh->mesh->get_coords(&node, 1, crd);
          MBMESH_CHECK_ERR(merr, localrc);
          
          //double *crd=dm_node_coord->data(*node);

          for (int j=0; j<sdim; j++) {
            polyCoords[crd_pos]=crd[j];
            crd_pos++;
          }

          //printf("%d# id=%d c=%d coord=%f %f \n",Par::Rank(),elemId[e],elemConn[conn_pos+i],polyCoords[crd_pos-2],polyCoords[crd_pos-1]);

        }

        // Triangulate polygon
        mb_triangulate(sdim, elemType[e], polyCoords, polyDblBuf, polyIntBuf, 
                    triInd, triFrac); 
        

        // Create split element list
        int tI_pos=0;
        for (int i=0; i<elemType[e]-2; i++) {
          // First id is same, others are from new ids
          if (i==0) {
            elemId_wsplit[split_elem_pos]=elemId[e];
            dual_mesh->split_id_to_frac[elemId[e]]=triFrac[i];
          } else {
            elemId_wsplit[split_elem_pos]=curr_extra_id;
            dual_mesh->split_to_orig_id[curr_extra_id]=elemId[e]; // Store map of split to original id
            dual_mesh->split_id_to_frac[curr_extra_id]=triFrac[i];
            curr_extra_id++;
          }

          // Owner is the same
          elemOwner_wsplit[split_elem_pos]=elemOwner[e];

          // Type is triangle
          elemType_wsplit[split_elem_pos]=3; 

          // Set mask (if it exists)
          //  if (elemMaskIIArray !=NULL) elemMaskIIArray_wsplit[split_elem_pos]=elemMaskIIArray[e];

          // Next split element
          split_elem_pos++;

          // Set triangle corners based on triInd
          elemConn_wsplit[split_conn_pos]=elemConn[conn_pos+triInd[tI_pos]];
          elemConn_wsplit[split_conn_pos+1]=elemConn[conn_pos+triInd[tI_pos+1]];
          elemConn_wsplit[split_conn_pos+2]=elemConn[conn_pos+triInd[tI_pos+2]];

          //printf("%d eid=%d seid=%d %d %d %d %f\n",i,elemId[e],elemId_wsplit[split_elem_pos-1],elemConn_wsplit[split_conn_pos],elemConn_wsplit[split_conn_pos+1],elemConn_wsplit[split_conn_pos+2],triFrac[i]);
          split_conn_pos +=3;
          tI_pos +=3;

        }

        // Advance to next elemConn position 
        conn_pos +=elemType[e];

      } else { // just copy
        elemId_wsplit[split_elem_pos]=elemId[e];
        elemOwner_wsplit[split_elem_pos]=elemOwner[e];
        elemType_wsplit[split_elem_pos]=elemType[e];
        // if (elemMaskIIArray !=NULL) elemMaskIIArray_wsplit[split_elem_pos]=elemMaskIIArray[e];
        split_elem_pos++;
        for (int i=0; i<elemType[e]; i++) {
          elemConn_wsplit[split_conn_pos]=elemConn[conn_pos];
          split_conn_pos++;
          conn_pos++;
        }
      }
    }
    
    
    // Delete some temporary variables for splitting
    if (polyCoords != NULL) delete [] polyCoords;
    if (polyDblBuf != NULL) delete [] polyDblBuf;
    if (polyIntBuf != NULL) delete [] polyIntBuf;
    if (triInd != NULL) delete [] triInd;
    if (triFrac !=NULL) delete [] triFrac;


      // Delete original element information
    if (elemType !=NULL) delete [] elemType;
    if (elemId !=NULL) delete [] elemId;
    if (elemOwner !=NULL) delete [] elemOwner;
    if (elemConn !=NULL) delete [] elemConn;

      // Use the new split list for the connection lists below
      num_elems=num_elems_wsplit;
      elemConn=elemConn_wsplit;
      elemType=elemType_wsplit;
      elemId=elemId_wsplit;
      elemOwner=elemOwner_wsplit;

#if 0
      if (elemMaskII != NULL) { 
        elemMaskII=elemMaskII_wsplit;
      }
#endif
    }   

#ifdef DEBUG_CONNECTIVITY
    printf("PET %d verts [", localPet);
    for (int i=0; i<dual_mesh->num_verts; ++i) {
      int nid;
      merr=dual_mesh->mesh->tag_get_data(dual_mesh->gid_tag, &dual_mesh->verts[i], 1, &nid);
      printf("%d, ", nid);
    }
    printf("]\n");
#endif

    // Now loop the elements and add them to the mesh.
    int cur_conn = 0;
    for (int e = 0; e < num_elems; ++e) {

      // Get number of nodes in element
      int num_elem_verts=ElemType2NumNodes(dual_mesh->pdim,
                                           dual_mesh->sdim,
                                           elemType[e]);

      // Define the maximum number of verts
#define MAX_ELEM_VERTS 20
       if (num_elem_verts >MAX_ELEM_VERTS) {
        int localrc;
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          "- element contains more nodes than are currently supported ",
                                         ESMC_CONTEXT, &localrc)) throw localrc;
      }

      // Connectivity array
      EntityHandle elem_verts[MAX_ELEM_VERTS];
#undef MAX_ELEM_VERTS

      // Fill the connectivity array
      for (int n = 0; n < num_elem_verts; ++n) {

        // Get 0-based vert index
        int vert_index=elemConn[cur_conn];

        // Setup connectivity list
        elem_verts[n] = dual_mesh->verts[vert_index];

        // Advance to next
        cur_conn++;
      }

      // Get number of nodes in element
      EntityType etype=get_entity_type(dual_mesh->pdim, elemType[e]);

      EntityHandle new_elem;
      merr=dual_mesh->mesh->create_element(etype,elem_verts,num_elem_verts,new_elem);
      MBMESH_CHECK_ERR(merr, localrc);

      // printf("PET %d add an element with %d verts\n", localPet, num_elem_verts);

       // Set global id
      merr=dual_mesh->mesh->tag_set_data(dual_mesh->gid_tag, &new_elem, 1, elemId+e);
      MBMESH_CHECK_ERR(merr, localrc);

      // Set Position
      merr=dual_mesh->mesh->tag_set_data(dual_mesh->orig_pos_tag, &new_elem, 1, &e);
      MBMESH_CHECK_ERR(merr, localrc);

      // Set Owner to the current pet
      // TODO: is this a good idea?
      merr=dual_mesh->mesh->tag_set_data(dual_mesh->owner_tag, &new_elem, 1, &elemOwner[e]);
      MBMESH_CHECK_ERR(merr, localrc);


      // // Set elem mask value
      // if (dual_mesh->has_elem_mask) {
      //   merr=dual_mesh->mesh->tag_set_data(dual_mesh->elem_mask_val_tag, &new_elem, 1,
      //                                elemMaskII->array+e);
      //   MBMESH_CHECK_ERR(merr, localrc);
      // }

#if 0
      // Set elem coords in the current elem
      merr=dual_mesh->mesh->tag_set_data(dual_mesh->elem_coords_tag, &new_elem, 1, elem_coords);
      MBMESH_CHECK_ERR(merr, localrc);
#endif

    } // for e

    // Set number of local elems
    dual_mesh->num_elems=num_elems;

    //// Setup parallel sharing ///

    // setup parallel comm, destroyed in MBMesh destructor
    ParallelComm *pcomm= new ParallelComm(dual_mesh->mesh, mpi_comm);
  
    Range elems_dual;
    merr=dual_mesh->mesh->get_entities_by_dimension(0, dual_mesh->pdim, elems_dual);
    MBMESH_CHECK_ERR(merr, localrc);
    
    // Resolve object sharing like in Mesh->Commit()
    merr = pcomm->resolve_shared_ents(0, elems_dual, dual_mesh->pdim, dual_mesh->pdim-1);
    MBMESH_CHECK_ERR(merr, localrc);

    // Output 
    *_dual_mesh=dual_mesh;

    localrc = ESMF_SUCCESS;
    *rc = localrc;
    }


// triangulate > 4 sided
// sdim = spatial dim
// num_p = number of points in poly
// p     = poly coords size=num_p*sdim
// td    = temporary buffer size=num_p*sdim
// ti    = temporary integer buffer size = num_p
// tri_ind = output array  size = 3*(nump-2)
// tri_frac = fraction each triangle is of whole poly size=(num_p-2)
void mb_triangulate(int sdim, int num_p, double *p, double *td, int *ti, int *tri_ind, 
                 double *tri_frac) {
          int localrc;
          

          // Call into triagulation routines
          int ret;
          if (sdim==2) {
            ret=triangulate_poly<GEOM_CART2D>(num_p, p, td,
                                              ti, tri_ind);
          } else if (sdim==3) {
            ret=triangulate_poly<GEOM_SPH2D3D>(num_p, p, td, 
                                               ti, tri_ind);
          } else {
            Throw() <<" - triangulate can't be used for polygons with spatial dimension not equal to 2 or 3";
          }
          

          // Check return code
          if (ret != ESMCI_TP_SUCCESS) {
            if (ret == ESMCI_TP_DEGENERATE_POLY) {
              Throw() << " - can't triangulate a polygon with less than 3 sides"; 
            } else if (ret == ESMCI_TP_CLOCKWISE_POLY) {
              Throw() <<" - there was a problem with triangulation (e.g. repeated points, clockwise poly, etc.)";
            } else {
              Throw() <<" - unknown error in triangulation";
            }
          }


          // Calculate triangule areas
          double tot_area=0.0;
          int ti_pos=0;
          for (int i=0; i<num_p-2; i++) {
            // Copy triangle coordinates into td
            int td_pos=0;
            for (int j=0; j<3; j++) {
              double *pnt=p+sdim*tri_ind[ti_pos+j];
              for (int k=0; k<sdim; k++) {
                td[td_pos]=pnt[k];
                td_pos++;
              }
            }

            // compute area of triangle
            double tri_area;
            if (sdim == 2) {
              tri_area = area_of_flat_2D_polygon(3, td);
            } else if (sdim == 3) {
              tri_area = great_circle_area(3, td);
            } // Other sdim caught above

            // Save areas to use for computing fractions
            tri_frac[i]=tri_area;
            
            // compute total
            tot_area += tri_area;

            // Advance to next triangle
            ti_pos +=3;
          }

          // Calculate triangle fractions
          for (int i=0; i<num_p-2; i++) {
            if (tot_area >0.0) tri_frac[i]=tri_frac[i]/tot_area;
            else tri_frac[i]=0.0;
          }

    return;
}

  // sort MDSS by id
  bool mb_less_by_ids(MDSS a, MDSS b) {
    return (a.id < b.id);
  }

  bool mb_equal_by_ids(MDSS a, MDSS b) {
    return (a.id == b.id);
  }

  // Get the element ids around a node
  // the ids should be in order around the node
  // Right now the algorithm that this uses for the ordering of the nodes is to 
  // calculate the angle around the center and then to sort by that. This could fail for 
  // for some very rare concave cases. Another method would be to walk through the mesh 
  // around the node. The problem is this fails for some more common cases. E.g. where
  // there aren't elems completely surrounding the node. Eventually, maybe some comb. of
  // the methods could be used?
  // Note that the list of ids returned her might be smaller than the number of elements around node
  // (and the number returned by get_num_elems_around_node()) due to split elements merging.
  // tmp_mdss = temporary list of structures used to sort elems (needs to be allocated large enough to hold all the ids)
  // _num_ids = the number of ids
  // _ids = where the ids will be put (needs to be allocated large enough to hold all the ids)
  void get_unique_elems_around_node(const EntityHandle *node, MBMesh *mesh, 
                                    MDSS *tmp_mdss, int *_num_ids, int *ids) {
    
    int merr, localrc;
    
    // Get useful info
    int sdim=mesh->sdim;
    int pdim=mesh->pdim;

    // Need element coordinates
    if (!mesh->has_elem_coords) {
      Throw() <<" Creation of a dual mesh requires element coordinates. \n";
    }
    
    // Need node coordinates
    // if (!mesh->has_node_orig_coords) {
    //   Throw() <<" Creation of a dual mesh requires node coordinates. \n";
    // }

     // Center coordinates
    // NOTE: Mostly treat as 3D to avoid lots of if (sdim=...)
    double center[3];
    double nc[3];
    // merr=mesh->mesh->tag_get_data(mesh->node_orig_coords_tag, 
    //           node, 1, nc);
    merr = mesh->mesh->get_coords(node, 1, nc);
    MBMESH_CHECK_ERR(merr, localrc);
    
    center[0]=nc[0];
    center[1]=nc[1];
    center[2]= sdim > 2 ? nc[2]:0.0;

    // Normalized vector through center and out of sphere
    // For 2D don't do norm to prevent div by 0.0
    double center_norm[3];
    if (sdim > 2) {
      double len_center=MU_LEN_VEC3D(center);
      MU_DIV_BY_SCALAR_VEC3D(center_norm,center,len_center);
    }

    // Range of elements around a node
    Range range_elem;
    merr = mesh->mesh->get_adjacencies(node, 1, pdim, false, range_elem);
    MBMESH_CHECK_ERR(merr, localrc);
    
#ifdef DEBUG_CONNECTIVITY_ADJACENCIES
    {int nid;
    merr=mesh->mesh->tag_get_data(mesh->gid_tag, node, 1, &nid);
    MBMESH_CHECK_ERR(merr, localrc);
    printf("%d# unique algorithm - mesh node id %d, adjacencies %d [", Par::Rank(), nid, range_elem.size());
    for(Range::iterator it=range_elem.begin(); it !=range_elem.end(); it++) {
      const EntityHandle *elem=&(*it);
      
      // Get element id
      int elem_id;
      merr = mesh->mesh->tag_get_data(mesh->gid_tag, elem, 1, &elem_id);
      MBMESH_CHECK_ERR(merr, localrc);
      
      printf("%d, ", elem_id);
    }
    printf("]\n");}
#endif

    // // No elements so leave
    // if (el == node->Relations.end() || el->obj->get_type() != MeshObj::ELEMENT){
    //   *_num_ids=0;
    //   return;
    // }
    
    // Get coords from elem with max id to make things consistent
    // on different processors
    // Loop the rest of the elements 
    int max_elem_id=0; // Init to 0 to watch for nothing ever being selected
    double max_elem_coords[3];
    for(Range::iterator it=range_elem.begin(); it !=range_elem.end(); it++) {
      const EntityHandle *elem=&(*it);
      
      // Get element id
      int elem_id;
      merr = mesh->mesh->tag_get_data(mesh->gid_tag, elem, 1, &elem_id);
      MBMESH_CHECK_ERR(merr, localrc);

      // Translate id if split
      if ((mesh->is_split) && (elem_id > mesh->max_non_split_id)) {
        std::map<int,int>::iterator soi =  mesh->split_to_orig_id.find(elem_id);
        if (soi != mesh->split_to_orig_id.end()) {
          elem_id=soi->second;
        } else {
          Throw() << "split elem id not found in map";
        }
      }
 
      // Check if max id if so switch max id and coordinates 
      if (elem_id > max_elem_id) {
        double ec[3];
        merr=mesh->mesh->tag_get_data(mesh->elem_coords_tag, elem, 1, ec);
        // merr = mesh->mesh->get_coords(elem, 1, ec);
        MBMESH_CHECK_ERR(merr, localrc);
        double tmp_coords[3];
        tmp_coords[0]=ec[0];
        tmp_coords[1]=ec[1];
        tmp_coords[2]= sdim > 2 ? ec[2]:0.0;
  
        // If at the center, so would be a zero vector skip...
        if ((tmp_coords[0]==center[0]) &&
            (tmp_coords[1]==center[1]) &&
            (tmp_coords[2]==center[2])) continue;
   
        // Otherwise make this the new point
        max_elem_id=elem_id;
        max_elem_coords[0]=tmp_coords[0];
        max_elem_coords[1]=tmp_coords[1];
        max_elem_coords[2]=tmp_coords[2];
      }
    }

     // If this is a  cell with everything at the center, then just use the center
    // this'll result in a degenerate cell which'll be handled later in the regridding with the flag. 
    if (max_elem_id==0) {
      max_elem_coords[0]=center[0];
      max_elem_coords[1]=center[1];
      max_elem_coords[2]=center[2];
    }

    // Get vector to first element 
    // NOTE: Mostly treat as 3D to avoid lots of if (sdim=...)
    double v1[3];
    MU_SUB_VEC3D(v1,max_elem_coords,center);

    // If this is a zero length vector complain
    // DON'T COMPLAIN JUST LEAVE IT BE DEGENERATE AND HANDLE IT LATER WITH DEGENERATE FLAG...
    //if ((v1[0] == 0.0) &&
    //    (v1[1] == 0.0) &&
    //    (v1[2] == 0.0)) {
    //  Throw() << " Can't order points in dual creation using a 0-vector";
    //}

    // Start over looping through elems attached to node, calculating angles
    int num_ids=0;
    for(Range::iterator it=range_elem.begin(); it !=range_elem.end(); it++) {
      const EntityHandle *elem=&(*it);
      
      // Get element id
      int elem_id;
      merr = mesh->mesh->tag_get_data(mesh->gid_tag, elem, 1, &elem_id);
      MBMESH_CHECK_ERR(merr, localrc);

      // Translate id if split
      if ((mesh->is_split) && (elem_id > mesh->max_non_split_id)) {
        std::map<int,int>::iterator soi =  mesh->split_to_orig_id.find(elem_id);
        if (soi != mesh->split_to_orig_id.end()) {
          elem_id=soi->second;
        } else {
          Throw() << "split elem id not found in map";
        }
      }

      // Get vector to current element 
      // NOTE: Mostly treat as 3D to avoid lots of if (sdim=...)
      double vcurr[3];
      double ec[3];
      merr=mesh->mesh->tag_get_data(mesh->elem_coords_tag, elem, 1, ec);
      // merr = mesh->mesh->get_coords(elem, 1, ec);
      MBMESH_CHECK_ERR(merr, localrc);
      vcurr[0]=ec[0];
        vcurr[1]=ec[1];
      vcurr[2]= sdim > 2 ? ec[2]:0.0;
      MU_SUB_VEC3D(vcurr,vcurr,center);

      // Calculate angle 
      // Do differentiate between 2D and 3D here to prevent inaccuracies
      double angle;
      if (sdim==2) {
        angle=calc_angle<GEOM_CART2D>(v1, vcurr, center_norm);
      } else if (sdim==3) {
        angle=calc_angle<GEOM_SPH2D3D>(v1, vcurr, center_norm);
      } else {
        Throw() <<" angle calc can't be used for vecs with spatial dimension not equal to 2 or 3";
      }

      // Put this into the list
      tmp_mdss[num_ids].id=elem_id;
      tmp_mdss[num_ids].angle=angle;
      num_ids++;
    }

    // Take out repeats due to split elements
     //// Sort by id
    std::sort(tmp_mdss, tmp_mdss+num_ids, mb_less_by_ids);

    //// Unique by id
    int prev_id=tmp_mdss[0].id;
    int new_num_ids=1;
    for (int i=1; i<num_ids; i++) {
 
      // if it has a different id, store it
      if (tmp_mdss[i].id != prev_id) {
        tmp_mdss[new_num_ids].id=tmp_mdss[i].id;
        tmp_mdss[new_num_ids].angle=tmp_mdss[i].angle;
        prev_id=tmp_mdss[i].id;
        new_num_ids++;
      }
    }
    num_ids=new_num_ids;

    // Now Sort the uniqued list by angle
    std::sort(tmp_mdss, tmp_mdss+num_ids);

    // Output
    *_num_ids=num_ids;
    for (int i=0; i< num_ids; i++) {
      ids[i]=tmp_mdss[i].id;
    }
  }


  // Add the elements in the ghost to the local split_orig_id map
  void add_ghost_elems_to_split_orig_id_map(MBMesh *mesh) {

    int merr, localrc;
    
    // Only do this if mesh is split
    if (!mesh->is_split) return;

    // Do this for now instead of initiating mesh parallel stuff
    // TODO: MAYBE EVENTUALLY PUT THIS INTO MBMesh???
    MPI_Comm mpi_comm;
    mpi_comm=VM::getCurrent(&localrc)->getMpi_c();
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception

    // Get localPet
    int localPet = VM::getCurrent(&localrc)->getLocalPet();
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception


    // Get a range containing all elements
    Range range_elem;
    merr = mesh->mesh->get_entities_by_dimension(0, mesh->pdim, range_elem);
    MBMESH_CHECK_ERR(merr, localrc);

    // Get number of split elements
    int num_gids=0;
    for(Range::iterator it=range_elem.begin(); it !=range_elem.end(); it++) {
      const EntityHandle *elem=&(*it);
    
      // Only do local 
      int owner;
      merr = mesh->mesh->tag_get_data(mesh->owner_tag, elem, 1, &owner);
      MBMESH_CHECK_ERR(merr, localrc);
      if (owner != localPet) continue;

      // Get element id
      int elem_id;
      merr=mesh->mesh->tag_get_data(mesh->gid_tag, elem, 1, &elem_id);
      MBMESH_CHECK_ERR(merr, localrc);

      // If it's less than or equal to the maximum non split id then its not split
      if (elem_id <=  mesh->max_non_split_id) continue;

      // It's split, so count this one     
      num_gids++;
    }

    // Get list of split and orig element gids
    UInt *gids_split=NULL;
    UInt *gids_orig=NULL;
    if (num_gids>0) {
      
      // Allocate space
      gids_split= new UInt[num_gids];
      gids_orig= new UInt[num_gids];
      
      // Loop through list putting into arrays
      int pos=0;
      for(Range::iterator it=range_elem.begin(); it !=range_elem.end(); it++) {
        const EntityHandle *elem=&(*it);
        
        // Only do local 
        int owner;
        merr = mesh->mesh->tag_get_data(mesh->owner_tag, elem, 1, &owner);
        MBMESH_CHECK_ERR(merr, localrc);
        if (owner != localPet) continue;
        
        // Get element id
        int elem_id;
        merr=mesh->mesh->tag_get_data(mesh->gid_tag, elem, 1, &elem_id);
        MBMESH_CHECK_ERR(merr, localrc);
        
        // If it's less than or equal to the maximum non split id then its not  split
        if (elem_id <= mesh->max_non_split_id) continue;
        
        // See if this is a split id
        std::map<int, int>::iterator soi=mesh->split_to_orig_id.find(elem_id);
        
        // If this is a split set it to the original, otherwise just set it to the elem id
        int orig_id;
        if (soi != mesh->split_to_orig_id.end()) {
          orig_id=soi->second;
        } else {
          Throw() << "split id not in split id to orig id map!";
        }
        
        // Put into arrays
        gids_orig[pos] = static_cast<UInt> (orig_id);
        gids_split[pos] = static_cast<UInt> (elem_id);
        
        // Next
        pos++;
      }
    }
    
    // Put into a DDir
  DDir<> id_map_dir;
  id_map_dir.Create(num_gids,gids_split,gids_orig);
 
  // Clean up 
  if (num_gids>0) {
    if (gids_split!= NULL) delete [] gids_split;
    if (gids_orig != NULL) delete [] gids_orig;
  }

  // Get number of ghost split elements
  int num_ghost_gids=0;
  for(Range::iterator it=range_elem.begin(); it !=range_elem.end(); it++) {
    const EntityHandle *elem=&(*it);
  
    // Only do local 
    int owner;
    merr = mesh->mesh->tag_get_data(mesh->owner_tag, elem, 1, &owner);
    MBMESH_CHECK_ERR(merr, localrc);
    if (owner == localPet) continue;

    // Get element id
    int elem_id;
    merr=mesh->mesh->tag_get_data(mesh->gid_tag, elem, 1, &elem_id);
    MBMESH_CHECK_ERR(merr, localrc);

    // If it's less than or equal to the maximum non split id then its not split
    if (elem_id <=  mesh->max_non_split_id) continue;

    // It's split, so count this one     
    num_ghost_gids++;
  }
  
  // Allocate array to hold ghost gids
  UInt *ghost_gids=NULL;
  if (num_ghost_gids>0) {
    ghost_gids=new UInt[num_ghost_gids];
  }
  
  // Get ghost gids
  int pos=0;
  for(Range::iterator it=range_elem.begin(); it !=range_elem.end(); it++) {
    const EntityHandle *elem=&(*it);
  
    // Only do local 
    int owner;
    merr = mesh->mesh->tag_get_data(mesh->owner_tag, elem, 1, &owner);
    MBMESH_CHECK_ERR(merr, localrc);
    if (owner == localPet) continue;

    // Get element id
    int elem_id;
    merr=mesh->mesh->tag_get_data(mesh->gid_tag, elem, 1, &elem_id);
    MBMESH_CHECK_ERR(merr, localrc);

    // If it's less than or equal to the maximum non split id then its not split
    if (elem_id <=  mesh->max_non_split_id) continue;

    //    printf("%d# p=%d ghost gid=%d\n",Par::Rank(),pos,elem_id);

    // Put in list
    ghost_gids[pos] = static_cast<UInt> (elem_id);

    // It's split, so count this one
    pos++;
  }
 
  // Do a look up of the input ids
  std::vector<DDir<>::dentry> lookups;
  id_map_dir.RemoteGID(num_ghost_gids, ghost_gids, lookups);
  
  // Don't need anymore so clean up 
  if (num_ghost_gids>0) {
    if (ghost_gids != NULL) delete [] ghost_gids;
  }

  // Loop through lookups and add to map
  for (UInt i=0; i<lookups.size(); i++) {
    
    // If split put into map
    if (lookups[i].gid != lookups[i].origin_lid) {
      mesh->split_to_orig_id[lookups[i].gid]=lookups[i].origin_lid;
    }
  }
  
  }

#endif

  } // namespace
