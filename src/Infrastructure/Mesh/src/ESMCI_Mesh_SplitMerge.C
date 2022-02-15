// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2022, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

#include <Mesh/include/ESMCI_Mesh_SplitMerge.h>

#include <iostream>
#include <fstream>
#include <map>
#include <cstdio>

using namespace ESMCI;

void native_get_elem_merged_connlist(const Mesh &mesh, 
                              std::vector<OSE>::iterator beg_elem_range, 
                              std::vector<OSE>::iterator end_elem_range, 
                              std::vector<int> &used, 
                              std::vector<int> &elem_merged_nids,
                              bool debug) {
#undef  ESMC_METHOD
#define ESMC_METHOD "native_get_elem_merged_connlist()"

    // DEBUG output
    if (debug) {
      printf("\norig_elem=%d ::",beg_elem_range->orig_elem->get_id());
      std::vector<OSE>::iterator osei=beg_elem_range;
      std::vector<OSE>::iterator osee=end_elem_range;
      for (; osei != osee; ++osei) {
        printf("%d ",osei->split_elem->get_id());
      }    
      printf("\n");
    }

    // Get number of items in range
    int size_range=std::distance(beg_elem_range, end_elem_range);

    // Resize used and set to 0
    used.clear();
    used.resize(size_range,0);

    // Clear and then reserve elem_merged_nids
    elem_merged_nids.clear();
    elem_merged_nids.reserve(size_range+2);

    // Add first element to connection list
    const MeshObj *first_split_elem=beg_elem_range->split_elem;
    const ESMCI::MeshObjTopo *topo = ESMCI::GetMeshObjTopo(*first_split_elem);

    if (debug) std::cout << "first split elem " 
                         << first_split_elem->get_id() <<" with nodes ";
    for (ESMCI::UInt n = 0; n < topo->num_nodes; ++n) {
        const MeshObj *node = first_split_elem->Relations[n].obj;

        // Add node id to connection list
        elem_merged_nids.push_back(node->get_id());
        
        if (debug) std::cout << node->get_id() << " ";

    }

    if (debug) std::cout << std::endl;

    // Mark the first elem as used
    used[0]=1;

    // Merge the rest of the elements into the list
    bool all_used=false;
    while (!all_used) {

      // Loop through the list of split elements
      all_used=true;
      std::vector<OSE>::iterator osei=beg_elem_range;
      int i=0;
      for (; osei != end_elem_range; ++i, ++osei) {
        
        // Skip if used 
        if (used[i]) continue;

        // Set all used to false
        all_used=false;

        // See if we can merge this element into the connection list
        const MeshObj *split_elem=(*osei).split_elem;
        const ESMCI::MeshObjTopo *topo = ESMCI::GetMeshObjTopo(*split_elem);
        
        // Make sure that this is a triangle, because a split elem should be
        if (topo->num_nodes != 3) Throw() <<"split elements should always be triangles.";

        // std::cout << "merging nodes of elem " << split_elem->get_id() << " nodes ";
        // Store Node ids
        int node_ids[3];
        if (debug) std::cout << "iterate nodes_on_elem (size = 3) " << std::flush;
        for (ESMCI::UInt n = 0; n < topo->num_nodes; ++n) {
          if (debug) std::cout << n << " " << std::flush;
          const MeshObj *node = split_elem->Relations[n].obj;
          node_ids[n]=node->get_id();
        }
        if (debug) {
          std::cout << "node_ids " << std::flush;
          for (const auto id : node_ids) 
            std::cout << id << " " << std::flush;
          std::cout<<std::endl;
        }

        // Loop through ids 
        bool found_node_id_to_merge=false;
        int node_id_to_merge=-1;
        int pos_to_merge_before=-1;
        for (int n=0; n<3; n++) {
          for (int s=0; s<elem_merged_nids.size(); s++) {
            // Look for a matching node id
            if (elem_merged_nids[s] == node_ids[n]) {
              // See if the next merged id matches the id after next
              int next_pos_in_merged=(s+1)%elem_merged_nids.size();
              if (elem_merged_nids[next_pos_in_merged] == node_ids[(n+2)%3]) {
                if (debug) std::cout << "node " 
                          << node_ids[n] << " matched, so merging "
                          << node_ids[(n+1)%3] << " at position "
                          << next_pos_in_merged << " (before " 
                          << elem_merged_nids[next_pos_in_merged] 
                          << ")" << std::endl;
                found_node_id_to_merge=true;
                node_id_to_merge=node_ids[(n+1)%3]; // Merge the node id in the middle
                pos_to_merge_before=next_pos_in_merged;
                break;
              }
            }
            // If we've found a place to merge then leave
            if (found_node_id_to_merge) break;
          }
        }


        // If found, then merge and mark used
        if (found_node_id_to_merge) {
          if (debug) std::cout << "inserting node " 
                               << node_id_to_merge << " at position " 
                               << pos_to_merge_before << std::endl;

          // std::cout << "merging " << node_id_to_merge << std::endl;
          elem_merged_nids.insert(elem_merged_nids.begin()+pos_to_merge_before,node_id_to_merge);
          used[i]=1;
        }

      } // Loop over split elem range 
    } // while not all used

    // now that we have a connectivity list for an original element
    // put the first created node at the first position of the list
    
    std::vector<std::pair<int,int> > sorted_elem_merged_nids;
    for (const auto i : elem_merged_nids) {
      //  Find the corresponding Mesh node
      Mesh::MeshObjIDMap::const_iterator mi =  mesh.map_find(MeshObj::NODE, i);
      if (mi == mesh.map_end(MeshObj::NODE)) {
        Throw() << "Node not in mesh";
      }

      sorted_elem_merged_nids.push_back(std::make_pair(mi->get_data_index(), i));
    }

    // sort the pairs by original creation order and get id of oldest node
    std::sort(sorted_elem_merged_nids.begin(), sorted_elem_merged_nids.end());
    int start_id = sorted_elem_merged_nids.begin()->second;
  
    // maintain order, but start with oldest node (O(n) operation)
    while ( start_id != *elem_merged_nids.begin())
      std::rotate(elem_merged_nids.begin(), elem_merged_nids.begin() + 1, 
                  elem_merged_nids.end());

    // DEBUG
    if (debug) {
      printf("orig_elem=%d :: nids= ",beg_elem_range->orig_elem->get_id());    
      for (int i=0; i<elem_merged_nids.size(); i++) {
        printf(" %d",elem_merged_nids[i]);
      }
      printf("\n");
    }
  }

  // Create a connection list for a mesh that has the original >4 sided connections
void native_get_mesh_merged_connlist(const Mesh &mesh, 
  std::vector<int> &num_merged_nids, std::vector<int> &merged_nids, 
  bool debug) {
#undef  ESMC_METHOD
#define ESMC_METHOD "native_get_mesh_merged_connlist()"
    
    // Clear output arrays
    num_merged_nids.clear();
    merged_nids.clear();

    // Create list of elems with their originals
    std::vector<OSE> ose_sorted;
    ose_sorted.reserve(mesh.num_elems());
    Mesh::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
    for (; ei != ee; ++ei) {
      const MeshObj *elem = &(*ei);

      // If it's split, then find original
      const MeshObj *orig_elem,*split_elem;
      if (elem->get_id() > mesh.max_non_split_id) {

        // Get original id
        std::map<UInt,UInt>::const_iterator soi =  mesh.split_to_orig_id.find(elem->get_id());
        if (soi == mesh.split_to_orig_id.end()) {
          Throw() <<"Split element id not found in split_to_orig map.";
        } 

        // Get original id
        UInt  orig_id=soi->second;

        //  Find the corresponding Mesh element
        Mesh::MeshObjIDMap::const_iterator mi =  mesh.map_find(MeshObj::ELEMENT, orig_id);
        if (mi == mesh.map_end(MeshObj::ELEMENT)) {
          Throw() << "Element not in mesh";
        }

        // Set orig and split elem
        orig_elem=&(*mi);
        split_elem=elem;

      } else { // Otherwise, both are the same...
        orig_elem=elem;
        split_elem=elem;
      }
      
      // Add to vector
      OSE tmp_ose(orig_elem,split_elem);
      ose_sorted.push_back(tmp_ose);
    } 

    // Sort vector to put all split elems next to each other
    std::sort(ose_sorted.begin(), ose_sorted.end());

    // DEBUG OUTPUT
    for (int i=0; i<ose_sorted.size(); i++) {
      printf("orig elem index=%d orig_elem=%d split_elem=%d\n",ose_sorted[i].orig_elem->get_data_index(),ose_sorted[i].orig_elem->get_id(),ose_sorted[i].split_elem->get_id());
    }

    // Put these outside loop so we allocate/deallocate less
    std::vector<int> used;
    std::vector<int> elem_merged_nids;


    // Go through list processing ranges that all correspond to one original elem
    std::vector<OSE>::iterator beg_elem_range=ose_sorted.begin(),osei;
    std::vector<OSE>::iterator osee=ose_sorted.end();
    for (osei=beg_elem_range; osei != osee; ++osei) {

      // Stop and process range if the orig element has changed
      if (beg_elem_range->orig_elem->get_id() != osei->orig_elem->get_id()) {

        // Turn range of split elems into merged conn list
        native_get_elem_merged_connlist(mesh, 
                                 beg_elem_range, osei, 
                                 used,elem_merged_nids, debug);

        // Add to the outgoing lists
        num_merged_nids.push_back(elem_merged_nids.size());
        for (int i=0; i<elem_merged_nids.size(); i++) {
          merged_nids.push_back(elem_merged_nids[i]);
        }

        // Start a new range
        beg_elem_range=osei;
      }
    }

    // Do the last range
    native_get_elem_merged_connlist(mesh, 
                              beg_elem_range, osee, 
                              used, elem_merged_nids, debug);
    
    // Add to the outgoing lists
    num_merged_nids.push_back(elem_merged_nids.size());
    for (int i=0; i<elem_merged_nids.size(); i++) {
      merged_nids.push_back(elem_merged_nids[i]);
    }
  }
