// $Id: ESMCI_MeshRedist.C,v 1.23 2012/01/06 20:17:51 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2012, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/ESMCI_MeshRedist.h>
#include <Mesh/include/ESMCI_MeshTypes.h>
#include <Mesh/include/ESMCI_MeshObjTopo.h>
#include <Mesh/include/ESMCI_MeshOBjConn.h>
#include <Mesh/include/ESMCI_Mapping.h>
#include <Mesh/include/ESMCI_MeshObj.h>
#include <Mesh/include/ESMCI_Mesh.h>
#include <Mesh/include/ESMCI_MeshUtils.h>
#include "Mesh/include/ESMCI_DDir.h" 
#include <Mesh/include/ESMCI_ParEnv.h>
#include <Mesh/include/ESMCI_CommReg.h>

#include <iostream>
#include <fstream>
#include <algorithm>
#include <iterator>

#include <ostream>

#include <set>

#include <limits>
#include <vector>


//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_MeshRedist.C,v 1.23 2012/01/06 20:17:51 svasquez Exp $";
//-----------------------------------------------------------------------------



namespace ESMCI {

  void send_mesh_fields(Mesh *src_mesh, Mesh *dst_mesh, CommReg &stod_comm);


  void MeshRedist(Mesh *src_mesh, int num_node_gids, int *node_gids, 
                                  int num_elem_gids, int *elem_gids, 
                  Mesh **_output_mesh) {

  Trace __trace("MeshRedist(const Mesh &src_mesh, int num_node_gids, int *node_gids, Mesh **output_mesh)");

#if 0
  for (int i=0; i<num_elem_gids; i++) {
    printf("%d :: e_gids=%d \n",i,elem_gids[i]);
  }
#endif

  // Create Mesh
  Mesh *output_mesh=new Mesh();

  // Set Mesh dimensions
  output_mesh->set_spatial_dimension(src_mesh->spatial_dim());
  output_mesh->set_parametric_dimension(src_mesh->parametric_dim());


    
    
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

 


  // Assign elem owners
  {
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


  // Assign node owners
  {
    // Create a distributed directory to figure out where teach node should go.
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

    // Get a list of the Mesh elem with gids 
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

    // Loop setting owner and OWNER_ID
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



  // Set node data indexes
  {
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


  // Set element data indexes
  {
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


  // Contexts
  // ...this is another part of prep_meshes
  output_mesh->AssumeContexts(*src_mesh);
  

  // Register Fields on output mesh    
  {
    MEField<> *scoord = src_mesh->GetCoordField();
    if (scoord != NULL) {
      output_mesh->RegisterField("coordinates", scoord->GetMEFamily(),
                                 MeshObj::ELEMENT, scoord->GetContext(), scoord->dim());
    }

    MEField<> *smask = src_mesh->GetField("mask");
    if (smask != NULL) {
      output_mesh->RegisterField("mask", smask->GetMEFamily(),
                                 MeshObj::ELEMENT, smask->GetContext(), smask->dim());
    }
       
    MEField<> *src_elem_mask = src_mesh->GetField("elem_mask");
    if (src_elem_mask != NULL) {
      output_mesh->RegisterField("elem_mask", src_elem_mask->GetMEFamily(),
                                MeshObj::ELEMENT, src_elem_mask->GetContext(), src_elem_mask->dim());
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
  }


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


  // Pack up and send fields from source to dest mesh using stod_comm
  void send_mesh_fields(Mesh *src_mesh, Mesh *dst_mesh, CommReg &stod_comm) {
    int num_snd=0;
    MEField<> *snd[5],*rcv[5];

    MEField<> *dc = src_mesh->GetCoordField();
    MEField<> *dc_r = dst_mesh->GetCoordField();

    // load coordinate fields
    snd[num_snd]=dc;
    rcv[num_snd]=dc_r;
    num_snd++;            

    // Do masks if necessary
    MEField<> *dm = src_mesh->GetField("mask");
    if (dm != NULL) {
      MEField<> *dm_r = dst_mesh->GetField("mask");

      // load mask fields
      snd[num_snd]=dm;
      rcv[num_snd]=dm_r;
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

    // Actually communicate fields
     stod_comm.SendFields(num_snd, snd, rcv);
  } 



  
} // namespace
