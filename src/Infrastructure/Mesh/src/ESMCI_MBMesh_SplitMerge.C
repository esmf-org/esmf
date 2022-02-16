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

#include <Mesh/include/ESMCI_MBMesh_SplitMerge.h>

#include <iostream>
#include <fstream>
#include <map>
#include <cstdio>
#include <vector>

using namespace ESMCI;

bool mbsm_dbg = true;

void mbmesh_get_elem_merged_connlist(const MBMesh &mesh, 
                              std::vector<MB_OSE>::iterator beg_elem_range, 
                              std::vector<MB_OSE>::iterator end_elem_range, 
                              std::vector<int> &used, 
                              std::vector<int> &elem_merged_nids,
                              bool debug) {
#undef  ESMC_METHOD
#define ESMC_METHOD "mbmesh_get_elem_merged_connlist()"

    int merr;

    // DEBUG output
    if (mbsm_dbg) {
      printf("\norig_elem=%d ::",beg_elem_range->orig_id);
      std::vector<MB_OSE>::iterator osei=beg_elem_range;
      std::vector<MB_OSE>::iterator osee=end_elem_range;
      for (; osei != osee; ++osei) {
        printf("%d ",osei->split_id);
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
    const EntityHandle *first_split_elem=beg_elem_range->split_elem;
    

    int eid;
    merr=mesh.mesh->tag_get_data(mesh.gid_tag, first_split_elem, 1, &eid);
    ESMC_CHECK_MOAB_THROW(merr)
    std::cout << "first_split_elem " << eid << " nodes " << std::flush;
    std::vector<EntityHandle> nodes_on_elem2;
    merr=mesh.mesh->get_connectivity(first_split_elem, 1, nodes_on_elem2);
    ESMC_CHECK_MOAB_THROW(merr)
    for (const auto noe : nodes_on_elem2) {
      EntityHandle node = noe;
      int nid;
      merr=mesh.mesh->tag_get_data(mesh.gid_tag, &node, 1, &nid);
      ESMC_CHECK_MOAB_THROW(merr)
      std::cout << nid << " " << std::flush;
    }
    std::cout << std::endl;

    
    // Get topology of element (ordered)
    // const ESMCI::MeshObjTopo *topo = ESMCI::GetMeshObjTopo(*first_split_elem);
    std::vector<EntityHandle> nodes_on_elem;
    merr=mesh.mesh->get_connectivity(first_split_elem, 1, nodes_on_elem);
    ESMC_CHECK_MOAB_THROW(merr)


    if (mbsm_dbg) std::cout << "first split elem " 
                         << beg_elem_range->split_id <<" with nodes ";
    // for (ESMCI::UInt n = 0; n < topo->num_nodes; ++n) {
    for (int i=0; i<nodes_on_elem.size(); ++i) {
        // const MeshObj *node = first_split_elem->Relations[n].obj;
        int nid;
        merr=mesh.mesh->tag_get_data(mesh.gid_tag, &nodes_on_elem.at(i), 1, &nid);
        ESMC_CHECK_MOAB_THROW(merr)
        
        // Add node id to connection list
        elem_merged_nids.push_back(nid);
        
        if (mbsm_dbg) std::cout << nid << " ";

    }

    if (mbsm_dbg) std::cout << std::endl;

    // Mark the first elem as used
    used[0]=1;

    // Merge the rest of the elements into the list
    bool all_used=false;
    int stop_ind = 0;
    while (!all_used) {
      stop_ind++;
      if (stop_ind > 10) break;
      
      // Loop through the list of split elements
      all_used=true;
      std::vector<MB_OSE>::iterator osei=beg_elem_range;
      int i=0;
      for (; osei != end_elem_range; ++i, ++osei) {
        
        // Skip if used 
        if (used[i]) continue;

        // Set all used to false
        all_used=false;

        // See if we can merge this element into the connection list
        const EntityHandle *split_elem=(*osei).split_elem;
        // Get topology of element (ordered)
        // const ESMCI::MeshObjTopo *topo = ESMCI::GetMeshObjTopo(*split_elem);
        std::vector<EntityHandle> nodes_on_elem;
        merr=mesh.mesh->get_connectivity(split_elem, 1, nodes_on_elem);
        ESMC_CHECK_MOAB_THROW(merr)
        
        // Make sure that this is a triangle
        if (nodes_on_elem.size() != 3) 
          Throw() <<"split elements should always be triangles.";

        // Store Node ids
        std::vector<EntityHandle>::const_iterator it=nodes_on_elem.begin();
        std::vector<EntityHandle>::const_iterator et=nodes_on_elem.end();
        int node_ids[3];
        if (mbsm_dbg) std::cout << "iterate nodes_on_elem (size = " 
                             << nodes_on_elem.size() << ") " << std::flush;
        int j = 0;
        for (; it !=et; ++it) {
          if (mbsm_dbg) std::cout << j << " " << std::flush;
        // for (ESMCI::UInt n = 0; n < topo->num_nodes; ++n) {
          // const MeshObj *node = split_elem->Relations[n].obj;
          int id;
          const EntityHandle node = *it;
          merr=mesh.mesh->tag_get_data(mesh.gid_tag, &node, 1, &id);
          node_ids[j] = id;
          j++;
        }
        if (mbsm_dbg) std::cout << "node_ids " << std::flush;
        for (const auto id : node_ids) std::cout << id << " " << std::flush;
        std::cout<<std::endl;

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
                if (mbsm_dbg) std::cout << "node " 
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
          if (mbsm_dbg) std::cout << "inserting node " 
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
      // Mesh::MeshObjIDMap::const_iterator mi =  mesh.map_find(MeshObj::NODE, i);
      // if (mi == mesh.map_end(MeshObj::NODE)) {
      //   Throw() << "Node not in mesh";
      // }
      
      
      // double loop to find the original element, moab::handle_from_id doesn't work
      Range nodes;
      merr=mesh.mesh->get_entities_by_dimension(0, 0, nodes);
      ESMC_CHECK_MOAB_THROW(merr)
      for (Range::const_iterator it2=nodes.begin(); it2 != nodes.end(); it2++) {
        EntityHandle node2=*it2;
        int node_id2;
        merr=mesh.mesh->tag_get_data(mesh.gid_tag, &node2, 1, &node_id2);
        
        if (node_id2 == i) {

          // EntityHandle *node;
          // merr = mesh.mesh->handle_from_id(MBVERTEX, i, *node);
          // ESMC_CHECK_MOAB_THROW(merr);
          
          int orig_pos;
          merr=mesh.mesh->tag_get_data(mesh.orig_pos_tag, &node2, 1, &orig_pos);
          ESMC_CHECK_MOAB_THROW(merr);
          
          sorted_elem_merged_nids.push_back(std::make_pair(orig_pos, i));
          break;
        }
      }

    }

    // sort the pairs by original creation order and get id of oldest node
    std::sort(sorted_elem_merged_nids.begin(), sorted_elem_merged_nids.end());
    int start_id = sorted_elem_merged_nids.begin()->second;
  
    // maintain order, but start with oldest node (O(n) operation)
    while ( start_id != *elem_merged_nids.begin())
      std::rotate(elem_merged_nids.begin(), elem_merged_nids.begin() + 1, 
                  elem_merged_nids.end());

    // DEBUG
    if (mbsm_dbg) {
      printf("orig_elem=%d :: nids= ",beg_elem_range->orig_id);    
      for (int i=0; i<elem_merged_nids.size(); i++) {
        printf(" %d",elem_merged_nids[i]);
      }
      printf("\n");
    }
  }

  // Create a connection list for a mesh that has the original >4 sided connections
void mbmesh_get_mesh_merged_connlist(const MBMesh &mesh, 
  std::vector<int> &num_merged_nids, std::vector<int> &merged_nids, 
  bool debug) {
#undef  ESMC_METHOD
#define ESMC_METHOD "mbmesh_get_mesh_merged_connlist()"
    
    int merr;
    
    // Clear output arrays
    num_merged_nids.clear();
    merged_nids.clear();

    Range elems;
    // this should probably only use owned elements right? 
    //   - do we want connectivity for ghost elements?
    merr=mesh.mesh->get_entities_by_dimension(0, mesh.pdim, elems);
    ESMC_CHECK_MOAB_THROW(merr)

    // Create list of elems with their originals
    std::vector<MB_OSE> ose_sorted;
    ose_sorted.reserve(elems.size());
    
    // std::cout << "split_to_orig_id map" << std::endl;
    // for (const auto dealio : mesh.split_to_orig_id)
    //   std::cout << dealio.first << " " << dealio.second << std::endl;
    
    
    for (Range::const_iterator it=elems.begin(); it != elems.end(); it++) {
      EntityHandle elem=*it;

      // If it's split, then find original
      EntityHandle *orig_elem,*split_elem;
      int elem_id;
      merr=mesh.mesh->tag_get_data(mesh.gid_tag, &elem, 1, &elem_id);
      ESMC_CHECK_MOAB_THROW(merr);
      if (elem_id > mesh.max_non_split_id) {

        // Get original id
        std::map<int,int>::const_iterator soi =  mesh.split_to_orig_id.find(elem_id);
        if (soi == mesh.split_to_orig_id.end()) {
          Throw() <<"Split element id not found in split_to_orig map.";
        } 

        // Get original id
        int orig_id=soi->second;

        //  Find the corresponding Mesh element
        // Mesh::MeshObjIDMap::const_iterator mi =  mesh.map_find(MeshObj::ELEMENT, orig_id);
        // if (mi == mesh.map_end(MeshObj::ELEMENT)) {
        //   Throw() << "Element not in mesh";
        // }
        
        // merr = mesh.mesh->handle_from_id(MBMAXTYPE, orig_id, *orig_elem);
        // ESMC_CHECK_MOAB_THROW(merr);
        
        // double loop to find the original element, moab::handle_from_id doesn't work
        Range elems2;
        merr=mesh.mesh->get_entities_by_dimension(0, mesh.pdim, elems2);
        ESMC_CHECK_MOAB_THROW(merr)
        for (Range::const_iterator it2=elems2.begin(); it2 != elems2.end(); it2++) {
          EntityHandle elem2=*it2;
          int elem_id2;
          merr=mesh.mesh->tag_get_data(mesh.gid_tag, &elem2, 1, &elem_id2);
          
          if (elem_id2 == orig_id) {
            orig_elem = &elem2;
            break;
          }
        }
        
        // int elem_id_check;
        // merr=mesh.mesh->tag_get_data(mesh.gid_tag, orig_elem, 1, &elem_id_check);
        // 
        // std::cout << "Elem " << elem_id << " --> " 
        //                      << orig_id << " == " << elem_id_check 
        //                      << "?" << std::endl;
        
        // Set orig and split elem
        split_elem=&elem;

      } else { // Otherwise, both are the same...
        orig_elem=&elem;
        split_elem=&elem;
      }
      
      int eid;
      merr=mesh.mesh->tag_get_data(mesh.gid_tag, orig_elem, 1, &eid);
      ESMC_CHECK_MOAB_THROW(merr)
      std::cout << "orig_elem " << eid << " nodes " << std::flush;
      std::vector<EntityHandle> nodes_on_elem2;
      merr=mesh.mesh->get_connectivity(orig_elem, 1, nodes_on_elem2);
      ESMC_CHECK_MOAB_THROW(merr)
      for (const auto noe : nodes_on_elem2) {
        EntityHandle node = noe;
        int nid;
        merr=mesh.mesh->tag_get_data(mesh.gid_tag, &node, 1, &nid);
        ESMC_CHECK_MOAB_THROW(merr)
        std::cout << nid << " " << std::flush;
      }
      std::cout << std::endl;
      
      
      eid;
      merr=mesh.mesh->tag_get_data(mesh.gid_tag, split_elem, 1, &eid);
      ESMC_CHECK_MOAB_THROW(merr)
      std::cout << "split_elem " << eid << " nodes " << std::flush;
      std::vector<EntityHandle> nodes_on_elem3;
      merr=mesh.mesh->get_connectivity(split_elem, 1, nodes_on_elem3);
      ESMC_CHECK_MOAB_THROW(merr)
      for (const auto noe : nodes_on_elem3) {
        EntityHandle node = noe;
        int nid;
        merr=mesh.mesh->tag_get_data(mesh.gid_tag, &node, 1, &nid);
        ESMC_CHECK_MOAB_THROW(merr)
        std::cout << nid << " " << std::flush;
      }
      std::cout << std::endl;

      // Add to vector
      MB_OSE tmp_ose(&mesh, orig_elem, split_elem);
      ose_sorted.push_back(tmp_ose);
    } 

    std::cout << "ose_sorted 1" << std::endl;
    for (const auto doo : ose_sorted)
      std::cout << doo.orig_pos << " " << doo.orig_id <<  " " 
                << doo.split_id << std::endl;

    // // Sort vector to put all split elems next to each other
    // std::sort(ose_sorted.begin(), ose_sorted.end());
    // 
    // std::cout << "ose_sorted 2" << std::endl;
    // for (const auto doo : ose_sorted)
    //   std::cout << doo.orig_pos << " " << doo.orig_id <<  " " 
    //             << doo.split_id << std::endl;
    // 
    // // DEBUG OUTPUT
    // for (int i=0; i<ose_sorted.size(); i++) {
    //   printf("orig elem index=%d orig_elem=%d split_elem=%d\n",ose_sorted[i].orig_pos,ose_sorted[i].orig_id,ose_sorted[i].split_id);
    // }

    // Put these outside loop so we allocate/deallocate less
    std::vector<int> used;
    std::vector<int> elem_merged_nids;


    // Go through list processing ranges that all correspond to one original elem
    std::vector<MB_OSE>::iterator beg_elem_range=ose_sorted.begin(),osei;
    std::vector<MB_OSE>::iterator osee=ose_sorted.end();
    for (osei=beg_elem_range; osei != osee; ++osei) {

      // Stop and process range if the orig element has changed
      if (beg_elem_range->orig_id != osei->orig_id) {

        // Turn range of split elems into merged conn list
        mbmesh_get_elem_merged_connlist(mesh, 
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
    mbmesh_get_elem_merged_connlist(mesh, 
                              beg_elem_range, osee, 
                              used, elem_merged_nids, debug);
    
    // Add to the outgoing lists
    num_merged_nids.push_back(elem_merged_nids.size());
    for (int i=0; i<elem_merged_nids.size(); i++) {
      merged_nids.push_back(elem_merged_nids[i]);
    }
  }
