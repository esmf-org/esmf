// $Id: ESMCI_MeshRedist.C,v 1.23 2012/01/06 20:17:51 svasquez Exp $
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
#include <Mesh/include/ESMCI_MeshRedist.h>
#include <Mesh/include/Legacy/ESMCI_MeshTypes.h>
#include <Mesh/include/Legacy/ESMCI_MeshObjTopo.h>
#include <Mesh/include/Legacy/ESMCI_MeshObjConn.h>
#include <Mesh/include/Regridding/ESMCI_Mapping.h>
#include <Mesh/include/Legacy/ESMCI_MeshObj.h>
#include <Mesh/include/ESMCI_Mesh.h>
#include <Mesh/include/Legacy/ESMCI_MeshUtils.h>
#include "Mesh/include/Legacy/ESMCI_DDir.h"
#include <Mesh/include/Legacy/ESMCI_ParEnv.h>
#include <Mesh/include/Legacy/ESMCI_CommReg.h>

#include "ESMCI_TraceMacros.h"  // for profiling

#include <iostream>
#include <fstream>
#include <algorithm>
#include <iterator>

#include <ostream>

#include <set>

#include <limits>
#include <vector>

#include <cstdio>

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_MeshRedist.C,v 1.23 2012/01/06 20:17:51 svasquez Exp $";
//-----------------------------------------------------------------------------



namespace ESMCI {

  void send_mesh_fields(Mesh *src_mesh, Mesh *dst_mesh, CommReg &stod_comm);

  void redist_elems(Mesh *src_mesh, DDir<> edir,
                    Mesh *output_mesh,  CommReg *_elemComm);


  void redist_nohome_nodes(Mesh *src_mesh, int num_node_gids, int *node_gids,
                           Mesh *output_mesh, CommReg *_nhComm);

  void register_fields(Mesh *src_mesh, Mesh *output_mesh);

  void set_elem_owners(Mesh *output_mesh,   DDir<> edir);

  void set_node_owners(Mesh *output_mesh,   DDir<> ndir);

  void set_node_data_indices(Mesh *output_mesh, int num_node_gids, int *node_gids);

  void set_node_data_indices_wo_list(Mesh *output_mesh);

  void set_elem_data_indices(Mesh *output_mesh, int num_elem_gids, int *elem_gids);

   void set_node_owners_wo_list(Mesh *output_mesh);

  void set_elem_data_indices_wo_list(Mesh *output_mesh);

  void set_elem_owners_wo_list(Mesh *output_mesh);


  void set_split_orig_id_map(Mesh *src_mesh, Mesh *output_mesh);

  struct MRN_Search {
  public:
    MRN_Search() : gid(0), proc(0), elem(NULL) {}
    MRN_Search(int _gid, int _proc, MeshObj *_elem) :
      gid(_gid), proc(_proc), elem(_elem) {}

    bool operator<(const MRN_Search &rhs) const
    { return (gid < rhs.gid) || (gid==rhs.gid && proc<rhs.proc); }

    int gid;
    int proc;
    MeshObj *elem;
  };


  void add_other_split_elems(Mesh *mesh, int gid, int proc,
                             std::multimap<UInt,MeshObj *> orig_id_to_split_elem,
                             std::set<MRN_Search> *to_snd);

  void redist_elems_from_set(Mesh *src_mesh, std::set<MRN_Search> to_snd,
                             Mesh *output_mesh,  CommReg *_elemComm);


  // Redist Mesh with specific destinations for just elements
  void MeshRedistNode(Mesh *src_mesh, int num_node_gids, int *node_gids,
                  Mesh **_output_mesh) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MeshRedistNode()"

    Trace __trace("MeshRedistNode()");

    ESMCI_MESHREDIST_TRACE_ENTER("NativeMesh ddir initialization");

    // Create a distributed directory to figure out where
    // the nodes should go.
    DDir<> ndir;

    std::vector<UInt> n_lids(num_node_gids, 0);
    std::vector<UInt> n_gids(num_node_gids, 0);

    for (int i=0; i<num_node_gids; i++) {
      n_lids[i]=i;
      if (node_gids[i]>=0) {
        n_gids[i]=node_gids[i];
      } else {
        n_gids[i]=0;
      }
    }

    if (num_node_gids) {
      ndir.Create(num_node_gids, &n_gids[0], &n_lids[0]);
    } else {
      ndir.Create(0, (UInt*) NULL, (UInt *)NULL);
    }

    // Make a node id to proc map
    // Build map of node id to proc destination
    std::map<int,int> src_node_id_to_proc;
    {// beg. of block to get rid of memory for search vectors (e.g. src_gids)

      // Get a list of the Mesh nodes with gids
      MeshDB::iterator ni = src_mesh->node_begin(), ne = src_mesh->node_end();
      std::vector<UInt> src_gids;
      src_gids.reserve(src_mesh->num_nodes());
      std::vector<MeshObj *> src_nodes;
      src_nodes.reserve(src_mesh->num_nodes());
      for (; ni != ne; ++ni) {
        MeshObj &node=*ni;

        // DO ALL NODES, BECAUSE NEEDED LATER FOR UNUSED ELEM MOVEMENT
        // If not local, then go on to next node
        // if (!GetAttr(node).is_locally_owned()) continue;

        // Add info to lists
        src_gids.push_back(node.get_id());
        src_nodes.push_back(&node);
      }

      // Allocate arrays for search
      UInt num_src_gids=src_gids.size();
      std::vector<UInt> src_gids_proc(num_src_gids, 0);
      std::vector<UInt> src_gids_lids(num_src_gids, 0);

      // Get where each nodes is to go
      if (num_src_gids) {
        ndir.RemoteGID(num_src_gids, &src_gids[0], &src_gids_proc[0], &src_gids_lids[0]);
      } else {
        ndir.RemoteGID(0, (UInt *)NULL, (UInt *)NULL, (UInt *)NULL);
      }

      for (int i=0; i< num_src_gids; i++) {
        // Get node
        MeshObj *node=src_nodes[i];
        int proc=src_gids_proc[i];

        src_node_id_to_proc[node->get_id()]=proc;
      }
    } // end. of block to get rid of memory for search vectors (e.g. src_gids)
    ESMCI_MESHREDIST_TRACE_EXIT("NativeMesh ddir initialization");

    ESMCI_MESHREDIST_TRACE_ENTER("NativeMesh split id preprocessing");
    // Invert split to orig id map
    std::multimap<UInt, MeshObj *> orig_id_to_split_elem;
    if (src_mesh->is_split) {
      MeshDB::iterator ei = src_mesh->elem_begin(), ee = src_mesh->elem_end();
      for (; ei != ee; ++ei) {
        MeshObj &elem=*ei;

        // get gid
        int gid=elem.get_id();

        // If this is a split element
        std::map<UInt,double>::iterator sitf = src_mesh->split_id_to_frac.find(gid);
        if (sitf != src_mesh->split_id_to_frac.end()) {
          // Translate split id to original
          int orig_id;
          std::map<UInt,UInt>::iterator soi = src_mesh->split_to_orig_id.find(gid);
          if (soi == src_mesh->split_to_orig_id.end()) {
            orig_id=gid;
          } else {
            orig_id=soi->second;
          }

          // Add to multimap
          orig_id_to_split_elem.insert(std::pair<UInt,MeshObj *>(orig_id,&elem));
        }
      }
    }
    ESMCI_MESHREDIST_TRACE_EXIT("NativeMesh split id preprocessing");


    ESMCI_MESHREDIST_TRACE_ENTER("NativeMesh ddir processing");
    // Find out what element to send to which proc to satisfy node requirement
    std::set<MRN_Search> to_snd;
    MeshDB::iterator ni = src_mesh->node_begin(), ne = src_mesh->node_end();
    for (; ni != ne; ++ni) {
      MeshObj &node=*ni;

      // If not local, then go on to next node
      if (!GetAttr(node).is_locally_owned()) continue;

      // Get proc that node is going to
      std::map<int,int>::iterator sni = src_node_id_to_proc.find(node.get_id());
      if (sni == src_node_id_to_proc.end()) {
        Throw() << "Node id not found in map!";
      }
      int proc=sni->second;


      // Loop through all elements attached to the node
      MRN_Search to_use;
      bool found=false;
      MeshObjRelationList::const_iterator el = MeshObjConn::find_relation(node, MeshObj::ELEMENT);
      while (el != node.Relations.end() && el->obj->get_type() == MeshObj::ELEMENT){
        MeshObj &elem=*(el->obj);

        // Build search struct
        MRN_Search curr(elem.get_id(), proc, &elem);

        // See if this element is already going to the proc, if so
        // go to next node
        if (to_snd.find(curr) != to_snd.end()) {
          found=true;
          break;
        }

        // Save to use
        to_use=curr;

        // Next element
        ++el;
      }

      // If not found add to set
      if (!found) {
        to_snd.insert(to_use);

        // If split mesh then add others
        if (src_mesh->is_split) {
          add_other_split_elems(src_mesh,
                                to_use.gid, to_use.proc,
                                orig_id_to_split_elem,
                                &to_snd);
        }
      }
    }

    // Go through source mesh and put elements that haven't been used yet someplace
    MeshDB::iterator ei = src_mesh->elem_begin(), ee = src_mesh->elem_end();
    for (; ei != ee; ++ei) {
      MeshObj &elem=*ei;

      // See if this elem is in to_snd list
      //// make lowest element with this gid
      MRN_Search lowest(elem.get_id(), 0, &elem);

      //// find element either equiv. to lowest or above
      std::set<MRN_Search>::iterator lsi=to_snd.lower_bound(lowest);

      //// If lower bound has same gid then elem in to_snd, otherwise not.
      bool elem_in_to_snd=false;
      if (lsi !=to_snd.end()) {
        MRN_Search mnr=*lsi;

        if (mnr.gid == elem.get_id()) {
          elem_in_to_snd=true;
        }
      }


      // If elem not being sent anyplace yet, then send it
      if (!elem_in_to_snd) {

        // Get a node in the element
        MeshObj *node;
        MeshObjRelationList::const_iterator nr = MeshObjConn::find_relation(elem, MeshObj::NODE);
        if (nr != elem.Relations.end() && nr->obj->get_type() == MeshObj::NODE){
          node=nr->obj;
        } else {
          Throw() << "This element has no associated node!";
        }

        // Get where that node is going
        std::map<int,int>::iterator sni = src_node_id_to_proc.find(node->get_id());
        if (sni == src_node_id_to_proc.end()) {
          Throw() << "Node id not found in map!";
        }
        int proc=sni->second;

        // Send this element to where one of it's nodes is going
        MRN_Search curr(elem.get_id(), proc, &elem);
        to_snd.insert(curr);

        // If split mesh then add others
        if (src_mesh->is_split) {
          add_other_split_elems(src_mesh,
                                curr.gid, curr.proc,
                                orig_id_to_split_elem,
                                &to_snd);
        }
      }
    }
    ESMCI_MESHREDIST_TRACE_EXIT("NativeMesh ddir processing");


#if 0
      {
    // print out what is going where for debugging
    std::set<MRN_Search>::iterator si=to_snd.begin(), se=to_snd.end();
    for (; si != se; ++si) {
      MRN_Search mnr=*si;

      printf("%d# elem=%d going to proc=%d\n",Par::Rank(),mnr.gid,mnr.proc);

    }
      }
#endif

    ESMCI_MESHREDIST_TRACE_ENTER("NativeMesh element communication");
    // Create Output Mesh
    Mesh *output_mesh=new Mesh();

    // Set Mesh dimensions
    output_mesh->set_spatial_dimension(src_mesh->spatial_dim());
    output_mesh->set_parametric_dimension(src_mesh->parametric_dim());
    output_mesh->orig_spatial_dim=src_mesh->orig_spatial_dim;

    // Send elements from src_mesh to output_mesh
    CommReg elemComm;
    redist_elems_from_set(src_mesh, to_snd,
                          output_mesh,  &elemComm);
    ESMCI_MESHREDIST_TRACE_EXIT("NativeMesh element communication");


    ESMCI_MESHREDIST_TRACE_ENTER("NativeMesh split id postprocessing");
    // Set the split information in output_mesh
    // NOTE: that this is done outside the MeshRedist function
    //       in other MeshRedist cases, but it was more efficient
    //       to do it here because it's needed to assign elem owners.
    output_mesh->is_split=src_mesh->is_split;
    if (output_mesh->is_split) {
      set_split_orig_id_map(src_mesh, output_mesh);
    }
    ESMCI_MESHREDIST_TRACE_EXIT("NativeMesh split id postprocessing");

    ESMCI_MESHREDIST_TRACE_ENTER("NativeMesh post processing");
    // Assign element owners
    set_elem_owners_wo_list(output_mesh);

    // Assign element data indices
    set_elem_data_indices_wo_list(output_mesh);

    // Assign node owners
    set_node_owners(output_mesh, ndir);

    // Set node data indexes
    set_node_data_indices(output_mesh, num_node_gids, node_gids);
    ESMCI_MESHREDIST_TRACE_EXIT("NativeMesh post processing");

    ESMCI_MESHREDIST_TRACE_ENTER("NativeMesh communication");
    // Assume Contexts
    output_mesh->AssumeContexts(*src_mesh);

    // Register fields
    register_fields(src_mesh, output_mesh);

    // Setup Sym communication for ghosting
    output_mesh->build_sym_comm_rel(MeshObj::NODE);

    // Commit Mesh
    output_mesh->Commit();

    // Send mesh fields (coords, etc) between src_mesh and output_mesh using elemComm
    send_mesh_fields(src_mesh, output_mesh, elemComm);
    ESMCI_MESHREDIST_TRACE_EXIT("NativeMesh communication");

#if 0
  {
     // Get a list of the Mesh nodes with gids
    MeshDB::iterator ni = output_mesh->node_begin(), ne = output_mesh->node_end();
    for (; ni != ne; ++ni) {
      MeshObj &node=*ni;

      printf("#%d ON node id=%d owner=%d is_local=%d data_index=%d \n",Par::Rank(),node.get_id(),node.get_owner(),GetAttr(node).is_locally_owned(),node.get_data_index());
    }

  }

#endif


#if 0


  {
     // Get a list of the Mesh elems with gids
    MeshDB::iterator ei = src_mesh->elem_begin(), ee = src_mesh->elem_end();
    for (; ei != ee; ++ei) {
      MeshObj &elem=*ei;

      printf("#%d SE elem id=%d owner=%d is_local=%d data_index=%d\n",Par::Rank(),elem.get_id(),elem.get_owner(),GetAttr(elem).is_locally_owned(),elem.get_data_index());
    }

  }
#endif


#if 0
  {
     // Get a list of the Mesh elems with gids
    MeshDB::iterator ei = output_mesh->elem_begin(), ee = output_mesh->elem_end();
    for (; ei != ee; ++ei) {
      MeshObj &elem=*ei;

      printf("#%d OE elem id=%d owner=%d is_local=%d data_index=%d\n",Par::Rank(),elem.get_id(),elem.get_owner(),GetAttr(elem).is_locally_owned(),elem.get_data_index());
    }

  }
#endif

    // Output
    *_output_mesh=output_mesh;

    }



  // Redist Mesh with specific destinations for elements and nodes
  void MeshRedist(Mesh *src_mesh, int num_node_gids, int *node_gids,
                                  int num_elem_gids, int *elem_gids,
                  Mesh **_output_mesh) {

  Trace __trace("MeshRedist(const Mesh &src_mesh, int num_node_gids, int *node_gids, Mesh **output_mesh)");


#if 0
  for (int i=0; i<num_elem_gids; i++) {
    printf("%d#    %d :: e_gids=%d \n",Par::Rank(),i,elem_gids[i]);
  }

  printf("\n");

  for (int i=0; i<num_node_gids; i++) {
    printf("%d#    %d :: n_gids=%d \n",Par::Rank(),i,node_gids[i]);
  }
#endif

  // Create Mesh
  Mesh *output_mesh=new Mesh();

  // Set Mesh dimensions
  output_mesh->set_spatial_dimension(src_mesh->spatial_dim());
  output_mesh->set_parametric_dimension(src_mesh->parametric_dim());
  output_mesh->orig_spatial_dim=src_mesh->orig_spatial_dim;

  // Create a distributed directory to figure out where the elems should go.
  DDir<> edir;

  std::vector<UInt> e_lids(num_elem_gids, 0);
  std::vector<UInt> e_gids(num_elem_gids, 0);

  for (int i=0; i<num_elem_gids; i++) {
    e_lids[i]=i;
    if (elem_gids[i]>=0) {
      e_gids[i]=elem_gids[i];
    } else {
      e_gids[i]=0;
    }
  }

 if (num_elem_gids) {
    edir.Create(num_elem_gids, &e_gids[0], &e_lids[0]);
  } else {
    edir.Create(0, (UInt*) NULL, (UInt *)NULL);
 }


 // Redist elems based on distributed directory edir
 CommReg elemComm;
 redist_elems(src_mesh, edir, output_mesh, &elemComm);


 // Redist nodes from node list (node_gids) which haven't been moved yet
 // from src_mesh to output_mesh
 CommReg nhComm;
 redist_nohome_nodes(src_mesh, num_node_gids, node_gids,
                     output_mesh, &nhComm);

 // Assign elem owners
 set_elem_owners(output_mesh, edir);

  // Set element data indexes
 set_elem_data_indices(output_mesh, num_elem_gids, elem_gids);


 // Create a distributed directory to figure out where each node should go.
 DDir<> ndir;

 std::vector<UInt> n_lids(num_node_gids, 0);
 std::vector<UInt> n_gids(num_node_gids, 0);

 for (int i=0; i<num_node_gids; i++) {
   n_lids[i]=i;
   if (node_gids[i]>=0) {
     n_gids[i]=node_gids[i];
   } else {
     n_gids[i]=0;
   }
 }

 if (num_node_gids) {
   ndir.Create(num_node_gids, &n_gids[0], &n_lids[0]);
 } else {
   ndir.Create(0, (UInt*) NULL, (UInt *)NULL);
 }

 // Assign node owners
 set_node_owners(output_mesh, ndir);

  // Set node data indexes
 set_node_data_indices(output_mesh, num_node_gids, node_gids);

  // Assume Contexts
  output_mesh->AssumeContexts(*src_mesh);

  // Register fields
  register_fields(src_mesh, output_mesh);

  // Setup Sym communication for ghosting
  output_mesh->build_sym_comm_rel(MeshObj::NODE);

   // Commit Mesh
  output_mesh->Commit();

  // Send mesh fields (coords, etc) between src_mesh and output_mesh using elemComm
  send_mesh_fields(src_mesh, output_mesh, elemComm);

  // Send mesh fields (coords, etc) between src_mesh and output_mesh using nhComm
  send_mesh_fields(src_mesh, output_mesh, nhComm);


#if 0
  {
     // Get a list of the Mesh nodes with gids
    MeshDB::iterator ni = output_mesh->node_begin(), ne = output_mesh->node_end();
    for (; ni != ne; ++ni) {
      MeshObj &node=*ni;

      printf("#%d E node id=%d owner=%d is_local=%d data_index=%d \n",Par::Rank(),node.get_id(),node.get_owner(),GetAttr(node).is_locally_owned(),node.get_data_index());
    }

  }


  {
     // Get a list of the Mesh nodes with gids
    MeshDB::iterator ei = output_mesh->elem_begin(), ee = output_mesh->elem_end();
    for (; ei != ee; ++ei) {
      MeshObj &elem=*ei;

      printf("#%d E elem id=%d owner=%d is_local=%d data_index=%d\n",Par::Rank(),elem.get_id(),elem.get_owner(),GetAttr(elem).is_locally_owned(),elem.get_data_index());
    }

  }
#endif



  // Output
  *_output_mesh=output_mesh;
}


  // Redist Mesh with specific destinations for just elements
  void MeshRedistElem(Mesh *src_mesh, int num_elem_gids, int *elem_gids,
                  Mesh **_output_mesh) {

  Trace __trace("MeshRedistElem()");


  // Create Mesh
  Mesh *output_mesh=new Mesh();

  // Set Mesh dimensions
  output_mesh->set_spatial_dimension(src_mesh->spatial_dim());
  output_mesh->set_parametric_dimension(src_mesh->parametric_dim());
  output_mesh->orig_spatial_dim=src_mesh->orig_spatial_dim;

  // Create a distributed directory to figure out where the elems should go.
  DDir<> edir;

  std::vector<UInt> e_lids(num_elem_gids, 0);
  std::vector<UInt> e_gids(num_elem_gids, 0);

  for (int i=0; i<num_elem_gids; i++) {
    e_lids[i]=i;
    if (elem_gids[i]>=0) {
      e_gids[i]=elem_gids[i];
    } else {
      e_gids[i]=0;
    }
  }

 if (num_elem_gids) {
    edir.Create(num_elem_gids, &e_gids[0], &e_lids[0]);
  } else {
    edir.Create(0, (UInt*) NULL, (UInt *)NULL);
 }


 // Redist elems based on distributed directory edir
 CommReg elemComm;
 redist_elems(src_mesh, edir, output_mesh, &elemComm);

 // Assign elem owners
 set_elem_owners(output_mesh, edir);

  // Set element data indexes
 set_elem_data_indices(output_mesh, num_elem_gids, elem_gids);

 // Set node owners
 set_node_owners_wo_list(output_mesh);

 // Set Node data indices
 set_node_data_indices_wo_list(output_mesh);

  // Assume Contexts
  output_mesh->AssumeContexts(*src_mesh);

  // Register fields
  register_fields(src_mesh, output_mesh);

  // Setup Sym communication for ghosting
  output_mesh->build_sym_comm_rel(MeshObj::NODE);

   // Commit Mesh
  output_mesh->Commit();


  // Send mesh fields (coords, etc) between src_mesh and output_mesh using elemComm
  send_mesh_fields(src_mesh, output_mesh, elemComm);


#if 0
  /// DO I NEED TO DO THIS????
  /// (I DON'T THINK SO, BUT JUST IN CASE LEAVE FOR NOW)

  // Send mesh fields (coords, etc) between src_mesh and output_mesh using nhComm
  send_mesh_fields(src_mesh, output_mesh, nhComm);
#endif


#if 0
  {
     // Get a list of the Mesh nodes with gids
    MeshDB::iterator ni = output_mesh->node_begin(), ne = output_mesh->node_end();
    for (; ni != ne; ++ni) {
      MeshObj &node=*ni;

      printf("#%d E node id=%d owner=%d is_local=%d data_index=%d \n",Par::Rank(),node.get_id(),node.get_owner(),GetAttr(node).is_locally_owned(),node.get_data_index());
    }

  }

#endif

#if 0


  {
     // Get a list of the Mesh elems with gids
    MeshDB::iterator ei = output_mesh->elem_begin(), ee = output_mesh->elem_end();
    for (; ei != ee; ++ei) {
      MeshObj &elem=*ei;

      printf("#%d E elem id=%d owner=%d is_local=%d data_index=%d\n",Par::Rank(),elem.get_id(),elem.get_owner(),GetAttr(elem).is_locally_owned(),elem.get_data_index());
    }

  }
#endif


  // Output
  *_output_mesh=output_mesh;
}


  // Set Node indices based on node_gids list
  void set_node_data_indices(Mesh *output_mesh, int num_node_gids, int *node_gids) {

    Trace __trace("set_node_data_indices()");


    // Set to -1 to mark which ones aren't set next
    MeshDB::iterator ni = output_mesh->node_begin(), ne = output_mesh->node_end();
    for (; ni != ne; ++ni) {
      MeshObj &node=*ni;

      *(node.get_data_index_ptr())=-1;
    }

    for (int i=0; i<num_node_gids; i++) {
      Mesh::MeshObjIDMap::iterator mi =  output_mesh->map_find(MeshObj::NODE, node_gids[i]);
      if (mi != output_mesh->map_end(MeshObj::NODE)) {

        // Get the node
        MeshObj &node = *mi;

        // Set it to it's position in the list
        *(node.get_data_index_ptr())=i;
      }
    }

    // Set non-local to after local
    int index=num_node_gids;
    ni = output_mesh->node_begin();
    for (; ni != ne; ++ni) {
      MeshObj &node=*ni;

      if (node.get_data_index()==-1) {
        *(node.get_data_index_ptr())=index;
        index++;
      }
    }
  }


  // Set Node indices without list
  void set_node_data_indices_wo_list(Mesh *output_mesh) {

    Trace __trace("set_node_data_indices_wo_list()");

    // Set to -1 to mark which ones aren't set next
    MeshDB::iterator ni = output_mesh->node_begin(), ne = output_mesh->node_end();
    int index=0;
    for (; ni != ne; ++ni) {
      MeshObj &node=*ni;

      *(node.get_data_index_ptr())=index;
      index++;
    }
  }


  // Set elem indices based on elem_gids list
  void set_elem_data_indices(Mesh *output_mesh, int num_elem_gids, int *elem_gids) {

    Trace __trace("set_elem_data_indices()");

    // Set to -1 to mark which ones aren't set next
    MeshDB::iterator ei = output_mesh->elem_begin(), ee = output_mesh->elem_end();
    for (; ei != ee; ++ei) {
      MeshObj &elem=*ei;

      *(elem.get_data_index_ptr())=-1;
    }

    for (int i=0; i<num_elem_gids; i++) {
      Mesh::MeshObjIDMap::iterator mi =  output_mesh->map_find(MeshObj::ELEMENT, elem_gids[i]);
      if (mi != output_mesh->map_end(MeshObj::ELEMENT)) {

        // Get the elem
        MeshObj &elem = *mi;

        // Set it to it's position in the list
        *(elem.get_data_index_ptr())=i;
      }
    }


    // Set non-local to after local
    int index=num_elem_gids;
    ei = output_mesh->elem_begin();
    for (; ei != ee; ++ei) {
      MeshObj &elem=*ei;

      if (elem.get_data_index()==-1) {
        *(elem.get_data_index_ptr())=index;
        index++;
      }
    }
  }



  // Set Elem indices without list
  void set_elem_data_indices_wo_list(Mesh *output_mesh) {

    Trace __trace("set_elem_data_indices_wo_list()");

    // Set data indices
    MeshDB::iterator ei = output_mesh->elem_begin(), ee = output_mesh->elem_end();
    int index=0;
    for (; ei != ee; ++ei) {
      MeshObj &elem=*ei;

      *(elem.get_data_index_ptr())=index;
      index++;
    }
  }


  // Assign node owners in output_mesh using ndir
  void set_node_owners(Mesh *output_mesh,   DDir<> ndir) {
    Trace __trace("set_node_owners()");


    // Get a list of the Mesh nodes with gids
    MeshDB::iterator ni = output_mesh->node_begin(), ne = output_mesh->node_end();

    std::vector<UInt> gids;
    gids.reserve(output_mesh->num_nodes());
    std::vector<MeshObj *> nodes;
    nodes.reserve(output_mesh->num_nodes());

    for (; ni != ne; ++ni) {
      MeshObj &node=*ni;

      gids.push_back(node.get_id());
      nodes.push_back(&node);
    }

    // Get number of gids
    UInt num_src_gids=gids.size();
    std::vector<UInt> src_gids_proc(num_src_gids, 0);
    std::vector<UInt> src_gids_lids(num_src_gids, 0);

    // Get where each element is to go
    if (num_src_gids) {
      ndir.RemoteGID(num_src_gids, &gids[0], &src_gids_proc[0], &src_gids_lids[0]);
    } else {
      ndir.RemoteGID(0, (UInt *)NULL, (UInt *)NULL, (UInt *)NULL);
    }

    // Loop setting owner 
    for (int i=0; i<num_src_gids; i++) {
      MeshObj &node=*(nodes[i]);

      // Set owner
      node.set_owner(src_gids_proc[i]);

      // Setup for changing attribute
      const Context &ctxt = GetMeshObjContext(node);
      Context newctxt(ctxt);

      // Set OWNED_ID appropriately
      if (src_gids_proc[i]==Par::Rank()) {
        newctxt.set(Attr::OWNED_ID);
      } else {
        newctxt.clear(Attr::OWNED_ID);
      }

      // If attribute has changed change in node
      if (newctxt != ctxt) {
        Attr attr(GetAttr(node), newctxt);
        output_mesh->update_obj(&node, attr);
      }
    }
  }

  // Assign node owners in output_mesh using ndir
  void set_node_owners_wo_list(Mesh *output_mesh) {
    Trace __trace("set_node_owners_wo_list()");

    // Get a list of the Mesh nodes with gids
    MeshDB::iterator ni = output_mesh->node_begin(), ne = output_mesh->node_end();

    std::vector<UInt> gids;
    gids.resize(output_mesh->num_nodes(),0);
    std::vector<UInt> lids; // Actually the number of associated elements
    lids.resize(output_mesh->num_nodes(),0);
    std::vector<UInt> owner; // The owner proc
    owner.resize(output_mesh->num_nodes(),0);
    std::vector<MeshObj *> nodes;
    nodes.resize(output_mesh->num_nodes());

    int i=0;
    for (; ni != ne; ++ni) {
      MeshObj &node=*ni;

      // Set GID
      gids[i]=node.get_id();

      // Set node
      nodes[i]=&node;

      // Count the number of associated elems
      int num_elems=0;
      MeshObjRelationList::const_iterator el = MeshObjConn::find_relation(node, MeshObj::ELEMENT);
      while (el != node.Relations.end() && el->obj->get_type() == MeshObj::ELEMENT){
        num_elems++;
        ++el;
      }

      // Set the number of associated elements as the lids
      lids[i]=num_elems;

      // Next thing in list
      i++;
    }

    // Create a distributed directory with the above information
    DDir<> dir;

    if (gids.size ()) {
      dir.Create(gids.size(), &gids[0], &lids[0]);
    } else {
      dir.Create(0, (UInt*) NULL, 0);
    }

 /* XMRKX */

     std::vector<DDir<>::dentry> lookups;
     if (gids.size())
       dir.RemoteGID(gids.size(), &gids[0], lookups);
     else
       dir.RemoteGID(0, (UInt *) NULL, lookups);


     // Loop through the results.
     int curr_pos=0;
     UInt curr_gid=0;
     UInt curr_lid_best=0;
     UInt curr_proc_best=0;
     bool first_time=true;
     std::vector<DDir<>::dentry>::iterator ri = lookups.begin(), re = lookups.end();
     for (; ri != re; ++ri) {
       DDir<>::dentry &dent = *ri;

       // Get info for this entry gid
       UInt gid=dent.gid;
       UInt lid=dent.origin_lid;
       UInt proc=dent.origin_proc;

       // first time
       if (first_time) {
         // If this doesn't match throw error
         if (gids[curr_pos] != gid) {
           printf("Error: first time gid[curr_pos]=%d gid=%d\n",gids[curr_pos],gid);

           Throw() << " Error: gid "<<gid<<" missing from search list!";
         }

         // Set intial values
         curr_gid=gids[curr_pos];
         curr_lid_best=lid;
         curr_proc_best=proc;

         first_time=false;
       }


       // See if we're still looking at the same gid, if not move to  next
       if (curr_gid != gid) {
         // Set owner of gid before moving on
         owner[curr_pos]=curr_proc_best;

         // Move to next gid
         curr_pos++;

         // If this doesn't match throw error
         if (gids[curr_pos] != gid) {
           printf("Error: gid[curr_pos]=%d gid=%d\n",gids[curr_pos],gid);

           Throw() << " Error: gid "<<gid<<" missing from search list!";
         }

         // Get info
         curr_gid=gids[curr_pos];
         curr_lid_best=lid;
         curr_proc_best=proc;
       } else {
         // Still the same gid so see if the proc is better
         if (lid > curr_lid_best) {
           curr_lid_best=lid;
           curr_proc_best=proc;
         } else if (lid == curr_lid_best) {
           // Same lid, so chose the lowest proc
           if (proc < curr_proc_best) {
             curr_lid_best=lid;
             curr_proc_best=proc;
           }
         }
       }

       // Print out
       //  printf("%d# gid=%d lid=%d orig_proc=%d \n",Par::Rank(),gid,lid,proc);

     } // ri


     // Set owner of last gid before moving on
     // (could use gids.size() in if here also, but
     //  owner.size seemed clearer...)
     if (owner.size()) {
       owner[curr_pos]=curr_proc_best;
     }

     // printf("Last curr_pos=%d gids.size()=%d\n",curr_pos,gids.size());


    // Loop setting owner
     for (int i=0; i<gids.size(); i++) {
      MeshObj &node=*(nodes[i]);

      // Set owner
      node.set_owner(owner[i]);

      // Setup for changing attribute
      const Context &ctxt = GetMeshObjContext(node);
      Context newctxt(ctxt);

      // Set OWNED_ID appropriately
      if (owner[i]==Par::Rank()) {
        newctxt.set(Attr::OWNED_ID);
      } else {
        newctxt.clear(Attr::OWNED_ID);
      }

      // If attribute has changed change in node
      if (newctxt != ctxt) {
        Attr attr(GetAttr(node), newctxt);
        output_mesh->update_obj(&node, attr);
      }
     }
  }


  // Assign element owners in output_mesh using edir
  void set_elem_owners(Mesh *output_mesh,   DDir<> edir) {
    Trace __trace("set_elem_owners()");

    // Get a list of the Mesh elem with gids
    MeshDB::iterator ei = output_mesh->elem_begin(), ee = output_mesh->elem_end();

    std::vector<UInt> gids;
    gids.reserve(output_mesh->num_elems());
    std::vector<MeshObj *> elems;
    elems.reserve(output_mesh->num_elems());
    for (; ei != ee; ++ei) {
      MeshObj &elem=*ei;

      gids.push_back(elem.get_id());
      elems.push_back(&elem);
    }


    // Get number of gids
    UInt num_src_gids=gids.size();
    std::vector<UInt> src_gids_proc(num_src_gids, 0);
    std::vector<UInt> src_gids_lids(num_src_gids, 0);

    // Get where each element is to go
    if (num_src_gids) {
      edir.RemoteGID(num_src_gids, &gids[0], &src_gids_proc[0], &src_gids_lids[0]);
    } else {
      edir.RemoteGID(0, (UInt *)NULL, (UInt *)NULL, (UInt *)NULL);
    }

    // Loop setting owner and OWNER_ID
    for (int i=0; i<num_src_gids; i++) {
      MeshObj &elem=*(elems[i]);

      // Set owner
      elem.set_owner(src_gids_proc[i]);

      // Setup for changing attribute
      const Context &ctxt = GetMeshObjContext(elem);
      Context newctxt(ctxt);

      // Set OWNED_ID appropriately
      if (src_gids_proc[i]==Par::Rank()) {
        newctxt.set(Attr::OWNED_ID);
      } else {
        newctxt.clear(Attr::OWNED_ID);
      }

      // If attribute has changed change in elem
      if (newctxt != ctxt) {
        Attr attr(GetAttr(elem), newctxt);
        output_mesh->update_obj(&elem, attr);
      }
    }
  }

  // Register Fields on output mesh  from src_mesh
  void register_fields(Mesh *src_mesh, Mesh *output_mesh) {

    MEField<> *scoord = src_mesh->GetCoordField();
    if (scoord != NULL) {
      output_mesh->RegisterField("coordinates", scoord->GetMEFamily(),
                                 MeshObj::ELEMENT, scoord->GetContext(), scoord->dim());
    }

    MEField<> *scoord_orig = src_mesh->GetField("orig_coordinates");
    if (scoord_orig != NULL) {
      output_mesh->RegisterField("orig_coordinates", scoord_orig->GetMEFamily(),
                                 MeshObj::ELEMENT, scoord_orig->GetContext(), scoord_orig->dim());
    }


    MEField<> *smask = src_mesh->GetField("mask");
    if (smask != NULL) {
      output_mesh->RegisterField("mask", smask->GetMEFamily(),
                                 MeshObj::ELEMENT, smask->GetContext(), smask->dim());
    }

    MEField<> *smask_val = src_mesh->GetField("node_mask_val");
    if (smask_val != NULL) {
      output_mesh->RegisterField("node_mask_val", smask_val->GetMEFamily(),
                                 MeshObj::ELEMENT, smask_val->GetContext(), smask_val->dim());
    }

    MEField<> *src_elem_mask = src_mesh->GetField("elem_mask");
    if (src_elem_mask != NULL) {
      output_mesh->RegisterField("elem_mask", src_elem_mask->GetMEFamily(),
                                MeshObj::ELEMENT, src_elem_mask->GetContext(), src_elem_mask->dim());
    }

    MEField<> *src_elem_mask_val = src_mesh->GetField("elem_mask_val");
    if (src_elem_mask_val != NULL) {
      output_mesh->RegisterField("elem_mask_val", src_elem_mask_val->GetMEFamily(),
                                MeshObj::ELEMENT, src_elem_mask_val->GetContext(), src_elem_mask_val->dim());
    }

    MEField<> *src_elem_area = src_mesh->GetField("elem_area");
    if (src_elem_area != NULL) {
      output_mesh->RegisterField("elem_area", src_elem_area->GetMEFamily(),
       MeshObj::ELEMENT, src_elem_area->GetContext(), src_elem_area->dim());
    }

    MEField<> *src_elem_frac2 = src_mesh->GetField("elem_frac2");
    if (src_elem_frac2 != NULL) {
      output_mesh->RegisterField("elem_frac2", src_elem_frac2->GetMEFamily(),
       MeshObj::ELEMENT, src_elem_frac2->GetContext(), src_elem_frac2->dim());
    }

    MEField<> *src_elem_frac = src_mesh->GetField("elem_frac");
    if (src_elem_frac != NULL) {
      output_mesh->RegisterField("elem_frac", src_elem_frac->GetMEFamily(),
       MeshObj::ELEMENT, src_elem_frac->GetContext(), src_elem_frac->dim());
    }

    MEField<> *scoord_elem = src_mesh->GetField("elem_coordinates");
    if (scoord_elem != NULL) {
      output_mesh->RegisterField("elem_coordinates", scoord_elem->GetMEFamily(),
                                 MeshObj::ELEMENT, scoord_elem->GetContext(), scoord_elem->dim());
    }

    MEField<> *scoord_orig_elem = src_mesh->GetField("elem_orig_coordinates");
    if (scoord_orig_elem != NULL) {
      output_mesh->RegisterField("elem_orig_coordinates", scoord_orig_elem->GetMEFamily(),
                                 MeshObj::ELEMENT, scoord_orig_elem->GetContext(), scoord_orig_elem->dim());
    }
  }


  // Redist nodes from a list of nodes (node_gids) which haven't been moved yet from src_mesh to output_mesh
  void redist_nohome_nodes(Mesh *src_mesh, int num_node_gids, int *node_gids,
                           Mesh *output_mesh, CommReg *_nhComm) {


  // Get list of nodes that don't have homes yet
  // Get number of gids
  std::vector<UInt> nohome_node_gids; nohome_node_gids.reserve(num_node_gids);
  std::vector<UInt> nohome_node_lids; nohome_node_lids.reserve(num_node_gids);
  for (int i=0; i<num_node_gids; i++) {

    // Put in node gid
    nohome_node_gids.push_back(node_gids[i]);

    // Mark lid based on presence
    Mesh::MeshObjIDMap::iterator mi =  output_mesh->map_find(MeshObj::NODE, node_gids[i]);

    // Didn't find in local output_mesh
    if (mi == output_mesh->map_end(MeshObj::NODE)) {
        nohome_node_lids.push_back(7);
    } else {
        nohome_node_lids.push_back(0);
    }
  }

#if 0
  // output list
  for (int i=0; i<nohome_node_gids.size(); i++) {
    printf("#%d nohome_node_gid=%d\n",Par::Rank(),nohome_node_gids[i]);
  }
#endif


  // Create DDir for nohome nodes
  DDir<> nhdir;

  if (!nohome_node_gids.empty()) {
    nhdir.Create(nohome_node_gids.size(), &nohome_node_gids[0], &nohome_node_lids[0]);
  } else {
    nhdir.Create(0, (UInt*) NULL, (UInt *)NULL);
  }

  // Get a list of the Mesh nodes with gids
  MeshDB::iterator ni = src_mesh->node_begin(), ne = src_mesh->node_end();
  std::vector<UInt> sn_gids;
  sn_gids.reserve(src_mesh->num_nodes());
  std::vector<MeshObj *> sn_ptrs;
  sn_ptrs.reserve(src_mesh->num_nodes());

  for (; ni != ne; ++ni) {
    MeshObj &node=*ni;
    sn_gids.push_back(node.get_id());
    sn_ptrs.push_back(&node);
  }

  // Get number of gids
  UInt num_sn_gids=sn_gids.size();
  std::vector<UInt> sn_gids_proc(num_sn_gids, 0);
  std::vector<UInt> sn_gids_lids(num_sn_gids, 0);


  // Get where each element is to go
  if (num_sn_gids) {
    nhdir.RemoteGID(num_sn_gids, &sn_gids[0], &sn_gids_proc[0], &sn_gids_lids[0]);
  } else {
    nhdir.RemoteGID(0, (UInt *)NULL, (UInt *)NULL, (UInt *)NULL);
  }


#if 0
  // Get list of destinations
  for (int i=0; i<num_sn_gids; i++) {
    if (sn_gids_lids[i]) {
      printf("#%d nohome gid=%d dest_proc=%d\n",Par::Rank(),sn_gids[i],sn_gids_proc[i]);
    }
  }
#endif

  /////// Construct second send list to fill in missing nodes /////

    // Setup list of mig nodes and destinations
    std::vector<CommRel::CommNode> nhmignode;

    // Get list of destinations
    for (int i=0; i<num_sn_gids; i++) {
      if (sn_gids_lids[i]) {
        MeshObj &node=*sn_ptrs[i];

        // Get one of the elements off this node
        MeshObj *elem;
        MeshObjRelationList::const_iterator er = MeshObjConn::find_relation(node, MeshObj::ELEMENT);
        if (er != node.Relations.end() && er->obj->get_type() == MeshObj::ELEMENT){
          elem=er->obj;
        } else {
          Throw() << "This node has no associated element!";
        }

        CommRel::CommNode cn(elem, sn_gids_proc[i]);
        nhmignode.push_back(cn);
      }
    }

    // Build Comm
    CommReg nhComm;
    CommRel &nhmigration = nhComm.GetCommRel(MeshObj::ELEMENT);
    nhmigration.Init("migration", *src_mesh, *output_mesh, false);
    nhmigration.add_domain(nhmignode);

    // Now flush out the comm with lower hierarchy
    nhmigration.dependants(nhComm.GetCommRel(MeshObj::NODE), MeshObj::NODE);
    nhmigration.dependants(nhComm.GetCommRel(MeshObj::EDGE), MeshObj::EDGE);
    nhmigration.dependants(nhComm.GetCommRel(MeshObj::FACE), MeshObj::FACE);

    // And now the destination
    nhComm.GetCommRel(MeshObj::NODE).build_range();
    nhComm.GetCommRel(MeshObj::EDGE).build_range();
    nhComm.GetCommRel(MeshObj::FACE).build_range();
    nhComm.GetCommRel(MeshObj::ELEMENT).build_range();

    // Output Comm
    *_nhComm=nhComm;
}




  // Redist Elems from src_mesh to output_mesh based on edir
  void redist_elems(Mesh *src_mesh, DDir<> edir,
                    Mesh *output_mesh,  CommReg *_elemComm) {

  // Get a list of the Mesh elem with gids
  MeshDB::iterator ei = src_mesh->elem_begin(), ee = src_mesh->elem_end();

  std::vector<UInt> gids;
  gids.reserve(src_mesh->num_elems());
  std::vector<MeshObj *> elems;
  elems.reserve(src_mesh->num_elems());

  for (; ei != ee; ++ei) {
    MeshObj &elem=*ei;

    gids.push_back(elem.get_id());
    elems.push_back(&elem);
  }


  // Figure out where each element should go
  UInt num_src_gids=gids.size();
  std::vector<UInt> src_gids_proc(num_src_gids, 0);
  std::vector<UInt> src_gids_lids(num_src_gids, 0);

  // Get where each element is to go
  if (num_src_gids) {
    edir.RemoteGID(num_src_gids, &gids[0], &src_gids_proc[0], &src_gids_lids[0]);
  } else {
    edir.RemoteGID(0, (UInt *)NULL, (UInt *)NULL, (UInt *)NULL);
  }


  // Setup migration pattern for elems
  std::vector<CommRel::CommNode> mignode;
  for (int i=0; i<num_src_gids; i++) {
    CommRel::CommNode cn(elems[i],src_gids_proc[i]);
    mignode.push_back(cn);
  }


  // Build Comm
  CommReg elemComm;
  CommRel &migration = elemComm.GetCommRel(MeshObj::ELEMENT);
  migration.Init("migration", *src_mesh, *output_mesh, false);
  migration.add_domain(mignode);

  // Now flush out the comm with lower hierarchy
  migration.dependants(elemComm.GetCommRel(MeshObj::NODE), MeshObj::NODE);
  migration.dependants(elemComm.GetCommRel(MeshObj::EDGE), MeshObj::EDGE);
  migration.dependants(elemComm.GetCommRel(MeshObj::FACE), MeshObj::FACE);


  // And now the destination
  elemComm.GetCommRel(MeshObj::NODE).build_range();
  elemComm.GetCommRel(MeshObj::EDGE).build_range();
  elemComm.GetCommRel(MeshObj::FACE).build_range();
  elemComm.GetCommRel(MeshObj::ELEMENT).build_range();

  // Output elemComm
  *_elemComm=elemComm;
}






  // Pack up and send fields from source to dest mesh using stod_comm
  void send_mesh_fields(Mesh *src_mesh, Mesh *dst_mesh, CommReg &stod_comm) {
    int num_snd=0;
    MEField<> *snd[20],*rcv[20];

    MEField<> *dc = src_mesh->GetCoordField();
    MEField<> *dc_r = dst_mesh->GetCoordField();

    // load coordinate fields
    snd[num_snd]=dc;
    rcv[num_snd]=dc_r;
    num_snd++;

    // Do original coordinates if necessary
    MEField<> *doc = src_mesh->GetField("orig_coordinates");
    if (doc != NULL) {
      MEField<> *doc_r = dst_mesh->GetField("orig_coordinates");

      // load coord fields
      snd[num_snd]=doc;
      rcv[num_snd]=doc_r;
      num_snd++;
    }

    // Do masks if necessary
    MEField<> *dm = src_mesh->GetField("mask");
    if (dm != NULL) {
      MEField<> *dm_r = dst_mesh->GetField("mask");

      // load mask fields
      snd[num_snd]=dm;
      rcv[num_snd]=dm_r;
      num_snd++;
    }

    MEField<> *dnmv = src_mesh->GetField("node_mask_val");
    if (dnmv != NULL) {
      MEField<> *dnmv_r = dst_mesh->GetField("node_mask_val");

      // load mask fields
      snd[num_snd]=dnmv;
      rcv[num_snd]=dnmv_r;
      num_snd++;
    }

    // Do elem masks if necessary
    MEField<> *dem = src_mesh->GetField("elem_mask");
    if (dem != NULL) {
      MEField<> *dem_r = dst_mesh->GetField("elem_mask");

      // load mask fields
      snd[num_snd]=dem;
      rcv[num_snd]=dem_r;
      num_snd++;
    }

    // Do elem masks if necessary
    MEField<> *demv = src_mesh->GetField("elem_mask_val");
    if (dem != NULL) {
      MEField<> *demv_r = dst_mesh->GetField("elem_mask_val");

      // load mask fields
      snd[num_snd]=demv;
      rcv[num_snd]=demv_r;
      num_snd++;
    }


    // Do elem area if necessary
    MEField<> *dea = src_mesh->GetField("elem_area");
    if (dea != NULL) {
      MEField<> *dea_r = dst_mesh->GetField("elem_area");

      // load mask fields
      snd[num_snd]=dea;
      rcv[num_snd]=dea_r;
      num_snd++;
    }

    // Do elem creeped frac if necessary
    MEField<> *def = src_mesh->GetField("elem_frac2");
    if (def != NULL) {
      MEField<> *def_r = dst_mesh->GetField("elem_frac2");

      // load mask fields
      snd[num_snd]=def;
      rcv[num_snd]=def_r;
      num_snd++;
    }

    // Do elem original coordinates if necessary
    MEField<> *dec = src_mesh->GetField("elem_coordinates");
    if (dec != NULL) {
      MEField<> *dec_r = dst_mesh->GetField("elem_coordinates");

      // load coord fields
      snd[num_snd]=dec;
      rcv[num_snd]=dec_r;
      num_snd++;
    }

    // Do elem original coordinates if necessary
    MEField<> *doec = src_mesh->GetField("elem_orig_coordinates");
    if (doec != NULL) {
      MEField<> *doec_r = dst_mesh->GetField("elem_orig_coordinates");

      // load coord fields
      snd[num_snd]=doec;
      rcv[num_snd]=doec_r;
      num_snd++;
    }

    // Actually communicate fields
     stod_comm.SendFields(num_snd, snd, rcv);
  }




  // Redist Elems from src_mesh to output_mesh based on info in set
  void redist_elems_from_set(Mesh *src_mesh, std::set<MRN_Search> to_snd,
                    Mesh *output_mesh,  CommReg *_elemComm) {


    // Setup migration pattern for elems
    std::vector<CommRel::CommNode> mignode;
    mignode.reserve(to_snd.size());

    // print out what is going where for debugging
    std::set<MRN_Search>::iterator si=to_snd.begin(), se=to_snd.end();
    for (; si != se; ++si) {
      MRN_Search mnr=*si;

      CommRel::CommNode cn(mnr.elem,mnr.proc);
      mignode.push_back(cn);
    }


    // Build Comm
    CommReg elemComm;
    CommRel &migration = elemComm.GetCommRel(MeshObj::ELEMENT);
    migration.Init("migration", *src_mesh, *output_mesh, false);
    migration.add_domain(mignode);

    // Now flush out the comm with lower hierarchy
    migration.dependants(elemComm.GetCommRel(MeshObj::NODE), MeshObj::NODE);
    migration.dependants(elemComm.GetCommRel(MeshObj::EDGE), MeshObj::EDGE);
    migration.dependants(elemComm.GetCommRel(MeshObj::FACE), MeshObj::FACE);


    // And now the destination
    elemComm.GetCommRel(MeshObj::NODE).build_range();
    elemComm.GetCommRel(MeshObj::EDGE).build_range();
    elemComm.GetCommRel(MeshObj::FACE).build_range();
    elemComm.GetCommRel(MeshObj::ELEMENT).build_range();

    // Output elemComm
    *_elemComm=elemComm;
  }


 /* XMRKX */

  // Assign elem owners in output_mesh
  // WARNING: needs is_split set in output_mesh, and if is_split==true then needs
  //          split_to_orig_id map to be correct in output_mesh.
  void set_elem_owners_wo_list(Mesh *output_mesh) {
    Trace __trace("set_elem_owners_wo_list()");

    // Count number of split and non-split elements
    int num_non_split=0;
    int num_split=0;
    if (output_mesh->is_split) {
      MeshDB::iterator ei = output_mesh->elem_begin(), ee = output_mesh->elem_end();
      for (; ei != ee; ++ei) {
        MeshObj &elem=*ei;

        // If this is an elem created as the result of a split, then skip
        std::map<UInt,UInt>::iterator soi = output_mesh->split_to_orig_id.find(elem.get_id());
        if (soi == output_mesh->split_to_orig_id.end()) {
          num_non_split++;
        } else {
          num_split++;
        }
      }
    } else {
      num_non_split=output_mesh->num_elems();
    }


    // Get a list of the Mesh nodes with gids
    std::vector<UInt> gids;
    gids.resize(num_non_split,0);
    std::vector<UInt> lids; // Actually the number of associated elements
    lids.resize(num_non_split,0);
    std::vector<UInt> owner; // The owner proc
    owner.resize(num_non_split,0);
    std::vector<MeshObj *> elems;
    elems.resize(num_non_split);

    int i=0;
    MeshDB::iterator ei = output_mesh->elem_begin(), ee = output_mesh->elem_end();
    for (; ei != ee; ++ei) {
      MeshObj &elem=*ei;

      // If this is an elem created as the result of a split, then skip
      if (output_mesh->is_split) {
        std::map<UInt,UInt>::iterator soi = output_mesh->split_to_orig_id.find(elem.get_id());
        if (soi != output_mesh->split_to_orig_id.end()) {
          continue;
        }
      }

      // Set GID
      gids[i]=elem.get_id();

      // Set elem
      elems[i]=&elem;

      // Count the number of local nodes associated with elem
      int num_loc_nodes=0;
      MeshObjRelationList::const_iterator nl = MeshObjConn::find_relation(elem, MeshObj::NODE);
      while (nl != elem.Relations.end() && nl->obj->get_type() == MeshObj::NODE){
        MeshObj &node = *(nl->obj);
        if (GetAttr(node).is_locally_owned()) num_loc_nodes++;
        ++nl;
      }


      // Set the number of associated local nodes as the lids
      lids[i]=num_loc_nodes;

      // Next thing in list
      i++;
    }

    // Create a distributed directory with the above information
    DDir<> dir;

    if (gids.size ()) {
      dir.Create(gids.size(), &gids[0], &lids[0]);
    } else {
      dir.Create(0, (UInt*) NULL, 0);
    }


    // Lookup elem gids
    std::vector<DDir<>::dentry> lookups;
    if (gids.size())
      dir.RemoteGID(gids.size(), &gids[0], lookups);
    else
      dir.RemoteGID(0, (UInt *) NULL, lookups);


     // Loop through the results.
     int curr_pos=0;
     UInt curr_gid=0;
     UInt curr_lid_best=0;
     UInt curr_proc_best=0;
     bool first_time=true;
     std::vector<DDir<>::dentry>::iterator ri = lookups.begin(), re = lookups.end();
     for (; ri != re; ++ri) {
       DDir<>::dentry &dent = *ri;

       // Get info for this entry gid
       UInt gid=dent.gid;
       UInt lid=dent.origin_lid;
       UInt proc=dent.origin_proc;

       // first time
       if (first_time) {
         // If this doesn't match throw error
         if (gids[curr_pos] != gid) {
           printf("Error: first time gid[curr_pos]=%d gid=%d\n",gids[curr_pos],gid);

           Throw() << " Error: gid "<<gid<<" missing from search list!";
         }

         // Set intial values
         curr_gid=gids[curr_pos];
         curr_lid_best=lid;
         curr_proc_best=proc;

         first_time=false;
       }


       // See if we're still looking at the same gid, if not move to  next
       if (curr_gid != gid) {
         // Set owner of gid before moving on
         owner[curr_pos]=curr_proc_best;

         // Move to next gid
         curr_pos++;

         // If this doesn't match throw error
         if (gids[curr_pos] != gid) {
           printf("Error: gid[curr_pos]=%d gid=%d\n",gids[curr_pos],gid);

           Throw() << " Error: gid "<<gid<<" missing from search list!";
         }

         // Get info
         curr_gid=gids[curr_pos];
         curr_lid_best=lid;
         curr_proc_best=proc;
       } else {
         // Still the same gid so see if the proc is better
         if (lid > curr_lid_best) {
           curr_lid_best=lid;
           curr_proc_best=proc;
         } else if (lid == curr_lid_best) {
           // Same lid, so chose the lowest proc
           if (proc < curr_proc_best) {
             curr_lid_best=lid;
             curr_proc_best=proc;
           }
         }
       }

       // Print out
       //  printf("%d# gid=%d lid=%d orig_proc=%d \n",Par::Rank(),gid,lid,proc);

     } // ri


     // Set owner of last gid before moving on
     // (could use gids.size() in if here also, but
     //  owner.size seemed clearer...)
     if (owner.size()) {
       owner[curr_pos]=curr_proc_best;
     }

     // printf("Last curr_pos=%d gids.size()=%d\n",curr_pos,gids.size());


    // Loop setting owner and OWNER_ID
     for (int i=0; i<gids.size(); i++) {
      MeshObj &elem=*(elems[i]);

      // Set owner
      elem.set_owner(owner[i]);

      // Setup for changing attribute
      const Context &ctxt = GetMeshObjContext(elem);
      Context newctxt(ctxt);

      // Set OWNED_ID appropriately
      if (owner[i]==Par::Rank()) {
        newctxt.set(Attr::OWNED_ID);
      } else {
        newctxt.clear(Attr::OWNED_ID);
      }

      // If attribute has changed change in node
      if (newctxt != ctxt) {
        Attr attr(GetAttr(elem), newctxt);
        output_mesh->update_obj(&elem, attr);
      }
    }


     // Do split elems
     if (output_mesh->is_split) {
       // Create list of split elems
       std::vector<MeshObj *> split_elems;
       split_elems.resize(num_split);

       // Fill list of split elems
       int pos=0;
       ei = output_mesh->elem_begin();
       for (; ei != ee; ++ei) {
         MeshObj &elem=*ei;

         // If this is an elem created as the result of a split, then skip
         std::map<UInt,UInt>::iterator soi = output_mesh->split_to_orig_id.find(elem.get_id());
         if (soi == output_mesh->split_to_orig_id.end()) {
           continue;
         }

         // Set elem
         split_elems[pos]=&elem;

         // Next in list
         pos++;
       }

       // Loop setting owner and OWNER_ID
       for (int i=0; i<split_elems.size(); i++) {
         MeshObj &elem=*(split_elems[i]);

         // Get orig id
         std::map<UInt,UInt>::iterator soi = output_mesh->split_to_orig_id.find(elem.get_id());
         if (soi == output_mesh->split_to_orig_id.end()) {
           Throw() << " split element not found in split_to_orig_id map";
         }
         int orig_id=soi->second;

         // Get original element
         Mesh::MeshObjIDMap::iterator mi=output_mesh->map_find(MeshObj::ELEMENT, orig_id);
         if (mi == output_mesh->map_end(MeshObj::ELEMENT)) {
           Throw() << " elem id not found in element map";
         }
         MeshObj &orig_elem=*mi;

         // Split element owner is original elements owner
         int owner=orig_elem.get_owner();

         // Set owner
         elem.set_owner(owner);

         // Setup for changing attribute
         const Context &ctxt = GetMeshObjContext(elem);
         Context newctxt(ctxt);

         // Set OWNED_ID appropriately
         if (owner==Par::Rank()) {
           newctxt.set(Attr::OWNED_ID);
         } else {
           newctxt.clear(Attr::OWNED_ID);
         }

         // If attribute has changed change in node
         if (newctxt != ctxt) {
           Attr attr(GetAttr(elem), newctxt);
           output_mesh->update_obj(&elem, attr);
         }
       }
     }
  }


#if 0
  // BEFORE ADDING SPLIT ELEMS

  // Assign elem owners in output_mesh
  void set_elem_owners_wo_list(Mesh *output_mesh) {
    Trace __trace("set_elem_owners_wo_list()");

    // Get a list of the Mesh nodes with gids
    std::vector<UInt> gids;
    gids.resize(output_mesh->num_elems(),0);
    std::vector<UInt> lids; // Actually the number of associated elements
    lids.resize(output_mesh->num_elems(),0);
    std::vector<UInt> owner; // The owner proc
    owner.resize(output_mesh->num_elems(),0);
    std::vector<MeshObj *> elems;
    elems.resize(output_mesh->num_elems());

    int i=0;
    MeshDB::iterator ei = output_mesh->elem_begin(), ee = output_mesh->elem_end();
    for (; ei != ee; ++ei) {
      MeshObj &elem=*ei;

      // Set GID
      gids[i]=elem.get_id();

      // Set node
      elems[i]=&elem;

      // Count the number of local nodes associated with elem
      int num_loc_nodes=0;
      MeshObjRelationList::const_iterator nl = MeshObjConn::find_relation(elem, MeshObj::NODE);
      while (nl != elem.Relations.end() && nl->obj->get_type() == MeshObj::NODE){
        MeshObj &node = *(nl->obj);
        if (GetAttr(node).is_locally_owned()) num_loc_nodes++;
        ++nl;
      }


      // Set the number of associated local nodes as the lids
      lids[i]=num_loc_nodes;

      // Next thing in list
      i++;
    }

    // Create a distributed directory with the above information
    DDir<> dir;

    if (gids.size ()) {
      dir.Create(gids.size(), &gids[0], &lids[0]);
    } else {
      dir.Create(0, (UInt*) NULL, 0);
    }


    // Lookup elem gids
    std::vector<DDir<>::dentry> lookups;
    if (gids.size())
      dir.RemoteGID(gids.size(), &gids[0], lookups);
    else
      dir.RemoteGID(0, (UInt *) NULL, lookups);


     // Loop through the results.
     int curr_pos=0;
     UInt curr_gid=0;
     UInt curr_lid_best=0;
     UInt curr_proc_best=0;
     bool first_time=true;
     std::vector<DDir<>::dentry>::iterator ri = lookups.begin(), re = lookups.end();
     for (; ri != re; ++ri) {
       DDir<>::dentry &dent = *ri;

       // Get info for this entry gid
       UInt gid=dent.gid;
       UInt lid=dent.origin_lid;
       UInt proc=dent.origin_proc;

       // first time
       if (first_time) {
         // If this doesn't match throw error
         if (gids[curr_pos] != gid) {
           printf("Error: first time gid[curr_pos]=%d gid=%d\n",gids[curr_pos],gid);

           Throw() << " Error: gid "<<gid<<" missing from search list!";
         }

         // Set intial values
         curr_gid=gids[curr_pos];
         curr_lid_best=lid;
         curr_proc_best=proc;

         first_time=false;
       }


       // See if we're still looking at the same gid, if not move to  next
       if (curr_gid != gid) {
         // Set owner of gid before moving on
         owner[curr_pos]=curr_proc_best;

         // Move to next gid
         curr_pos++;

         // If this doesn't match throw error
         if (gids[curr_pos] != gid) {
           printf("Error: gid[curr_pos]=%d gid=%d\n",gids[curr_pos],gid);

           Throw() << " Error: gid "<<gid<<" missing from search list!";
         }

         // Get info
         curr_gid=gids[curr_pos];
         curr_lid_best=lid;
         curr_proc_best=proc;
       } else {
         // Still the same gid so see if the proc is better
         if (lid > curr_lid_best) {
           curr_lid_best=lid;
           curr_proc_best=proc;
         } else if (lid == curr_lid_best) {
           // Same lid, so chose the lowest proc
           if (proc < curr_proc_best) {
             curr_lid_best=lid;
             curr_proc_best=proc;
           }
         }
       }

       // Print out
       //  printf("%d# gid=%d lid=%d orig_proc=%d \n",Par::Rank(),gid,lid,proc);

     } // ri


     // Set owner of last gid before moving on
     // (could use gids.size() in if here also, but
     //  owner.size seemed clearer...)
     if (owner.size()) {
       owner[curr_pos]=curr_proc_best;
     }

     // printf("Last curr_pos=%d gids.size()=%d\n",curr_pos,gids.size());


    // Loop setting owner and OWNER_ID
     for (int i=0; i<gids.size(); i++) {
      MeshObj &elem=*(elems[i]);

      // Set owner
      elem.set_owner(owner[i]);

      // Setup for changing attribute
      const Context &ctxt = GetMeshObjContext(elem);
      Context newctxt(ctxt);

      // Set OWNED_ID appropriately
      if (owner[i]==Par::Rank()) {
        newctxt.set(Attr::OWNED_ID);
      } else {
        newctxt.clear(Attr::OWNED_ID);
      }

      // If attribute has changed change in node
      if (newctxt != ctxt) {
        Attr attr(GetAttr(elem), newctxt);
        output_mesh->update_obj(&elem, attr);
      }
    }
  }
#endif

  // For a split mesh add the other parts of a split element
 void add_other_split_elems(Mesh *mesh, int gid, int proc,
                        std::multimap<UInt,MeshObj *> orig_id_to_split_elem,
                        std::set<MRN_Search> *to_snd) {
    // If this is a split element
    std::map<UInt,double>::iterator sitf =  mesh->split_id_to_frac.find(gid);
    if (sitf != mesh->split_id_to_frac.end()) {
      // Translate split id to original
      int orig_id;
      std::map<UInt,UInt>::iterator soi = mesh->split_to_orig_id.find(gid);
      if (soi == mesh->split_to_orig_id.end()) {
        orig_id=gid;
      } else {
        orig_id=soi->second;
      }

      // Loop through and add other elements from original
      std::pair <std::multimap<UInt,MeshObj *>::iterator, std::multimap<UInt,MeshObj *>::iterator> ret;
      ret=orig_id_to_split_elem.equal_range(orig_id);
      for (std::multimap<UInt,MeshObj *>::iterator it=ret.first; it!=ret.second; ++it) {
        // Split elem
        MeshObj *split_elem=it->second;

        // Only add if not the one that's been added before
        if (split_elem->get_id() != gid) {

          // Add to send list
          MRN_Search curr(split_elem->get_id(), proc, split_elem);
          to_snd->insert(curr);
        }
      }
    }
  }

  // Set the split_orig_id_map in a redisted mesh from the src mesh
  void set_split_orig_id_map(Mesh *src_mesh, Mesh *output_mesh) {

  // Get number of elements
  int num_gids=src_mesh->num_elems();

  // Get list of split and orig element gids
  UInt *gids_split=NULL;
  UInt *gids_orig=NULL;
  if (num_gids>0) {

    // Allocate space
    gids_split= new UInt[num_gids];
    gids_orig= new UInt[num_gids];

    // Loop through list putting into arrays
     int pos=0;
    Mesh::iterator ei = src_mesh->elem_begin(), ee = src_mesh->elem_end();
    for (; ei != ee; ++ei) {
      MeshObj &elem = *ei;

      // Only do local
      if (!GetAttr(elem).is_locally_owned()) continue;

      // Get element id
      UInt split_eid=elem.get_id();

      // See if this is a split id
      std::map<UInt,UInt>::iterator soi=src_mesh->split_to_orig_id.find(split_eid);

      // If this is a split set it to the original, otherwise just set it to the elem id
      UInt orig_eid;
      if (soi==src_mesh->split_to_orig_id.end()) {
        orig_eid=split_eid;
      } else {
        orig_eid=soi->second;
      }

      // Put into arrays
      gids_orig[pos]=orig_eid;
      gids_split[pos]=split_eid;

      // Next
      pos++;
    }
  }

  // Put into a DDir
  DDir<> id_map_dir;
  id_map_dir.Create(num_gids,gids_orig,gids_split);

  // Clean up
  if (num_gids>0) {
    if (gids_split!= NULL) delete [] gids_split;
    if (gids_orig != NULL) delete [] gids_orig;
  }


  // STOPPED HERE
  // LOOK UP OUTPUT_MESH ELEMS HERE PUT INTO elem_gids_u
  int num_elem_gids=output_mesh->num_elems();

  // Copy input array to UInt
  UInt *elem_gids_u=NULL;
  if (num_elem_gids>0) {
    elem_gids_u= new UInt[num_elem_gids];
  }

  // Loop through and collect output_mesh element ids
  int om_pos=0;
  Mesh::iterator om_ei = output_mesh->elem_begin(), om_ee = output_mesh->elem_end();
  for (; om_ei != om_ee; ++om_ei) {
    MeshObj &elem = *om_ei;

    elem_gids_u[om_pos]=elem.get_id();
    om_pos++;
  }


  // Do a look up of the input ids
  std::vector<DDir<>::dentry> lookups;
  id_map_dir.RemoteGID(num_elem_gids, elem_gids_u, lookups);

  // Don't need anymore so clean up
   if (num_elem_gids>0) {
    if (elem_gids_u != NULL) delete [] elem_gids_u;
  }

  // Loop through lookups and generate new list
    for (int i=0; i<lookups.size(); i++) {

      // If split put into map
      if (lookups[i].gid != lookups[i].origin_lid) {
        output_mesh->split_to_orig_id[lookups[i].origin_lid]=lookups[i].gid;
      }
    }

  }

} // namespace
