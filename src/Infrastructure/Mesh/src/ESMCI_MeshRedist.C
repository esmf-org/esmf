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


  void MeshRedist(Mesh *src_mesh, int num_node_gids, int *node_gids, 
                                  int num_elem_gids, int *elem_gids, 
                  Mesh **_output_mesh) {

  Trace __trace("MeshRedist(const Mesh &src_mesh, int num_node_gids, int *node_gids, Mesh **output_mesh)");

#if 0
  for (int i=0; i<num_elem_gids; i++) {
    printf("%d :: e_gids=%d \n",i,elem_gids[i]);
  }
#endif
    
  // Create a distributed directory to figure out where the node should go.
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


  // Setup list of mig nodes and destinations
  std::vector<CommRel::CommNode> mignode;
  for (int i=0; i<num_src_gids; i++) {    
    CommRel::CommNode cn(elems[i],src_gids_proc[i]);
    mignode.push_back(cn);
  }
  
  
  // Create Mesh
  Mesh *output_mesh=new Mesh();

  // This is part of prep_meshes
  output_mesh->set_spatial_dimension(src_mesh->spatial_dim());
  output_mesh->set_parametric_dimension(src_mesh->parametric_dim());


  // Build Comm
  // RENAME THIS LATER WHEN UNDERSTAND BETTER
  CommReg dstComm;

  // SHOULD THE BELOW BE ELEMENT OR SHOULD NODE BE IN THE COMM FLUSH BELOW???
  CommRel &migration = dstComm.GetCommRel(MeshObj::ELEMENT); 
  migration.Init("migration", *src_mesh, *output_mesh, false);
  migration.add_domain(mignode); 

  // Now flush out the comm with lower hierarchy
  migration.dependants(dstComm.GetCommRel(MeshObj::NODE), MeshObj::NODE); 
  migration.dependants(dstComm.GetCommRel(MeshObj::EDGE), MeshObj::EDGE);
  migration.dependants(dstComm.GetCommRel(MeshObj::FACE), MeshObj::FACE);

  // Contexts
  // ...this is another part of prep_meshes
  output_mesh->AssumeContexts(*src_mesh);
  

  // Register Fields on output mesh    
  // ...this is the other part of prep_meshes
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


  // And now the destination
  dstComm.GetCommRel(MeshObj::NODE).build_range();
  dstComm.GetCommRel(MeshObj::EDGE).build_range();
  dstComm.GetCommRel(MeshObj::FACE).build_range();
  dstComm.GetCommRel(MeshObj::ELEMENT).build_range();


  {
    for (int i=0; i<num_elem_gids; i++) {
      
      Mesh::MeshObjIDMap::iterator mi =  output_mesh->map_find(MeshObj::ELEMENT, elem_gids[i]);
      if (mi != output_mesh->map_end(MeshObj::ELEMENT)) {
        // Get the element                                                        
        MeshObj &elem = *mi;
       
        // Set as local 
        elem.set_owner(Par::Rank());

        // Update to locally owned, if needed
        const Context &ctxt = GetMeshObjContext(elem);
        Context newctxt(ctxt);
        newctxt.set(Attr::OWNED_ID);

        if (newctxt != ctxt) {
          Attr attr(GetAttr(elem), newctxt);
          output_mesh->update_obj(&elem, attr);
        }
      }   
    }
  }


  // Assign node owners
  {
    // Get a list of the Mesh elem with gids 
    MeshDB::iterator ni = output_mesh->node_begin(), ne = output_mesh->node_end();
    
    std::vector<UInt> gids;
    gids.reserve(output_mesh->num_nodes());
    std::vector<MeshObj *> nodes;
    nodes.reserve(src_mesh->num_nodes());
    
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


#if 0
  {

printf(" AFTER build range\n");
     // Get a list of the Mesh nodes with gids 
    MeshDB::iterator ei = output_mesh->elem_begin(), ee = output_mesh->elem_end();
    for (; ei != ee; ++ei) {
      MeshObj &elem=*ei;
     
      printf("#%d A elem id=%d owner=%d is_local=%d \n",Par::Rank(),elem.get_id(),elem.get_owner(),GetAttr(elem).is_locally_owned());
    }

  }
#endif



   // Commit Mesh
  output_mesh->Commit();
  
  // Pack up and send fields
  // Can these be combined with the above

  {
    int num_snd=0;
    MEField<> *snd[5],*rcv[5];

    MEField<> *dc = src_mesh->GetCoordField();
    MEField<> *dc_r = output_mesh->GetCoordField();

    // load coordinate fields
    snd[num_snd]=dc;
    rcv[num_snd]=dc_r;
    num_snd++;            

    // Do masks if necessary
    MEField<> *dm = src_mesh->GetField("mask");
    if (dm != NULL) {
      MEField<> *dm_r = output_mesh->GetField("mask");

      // load mask fields
      snd[num_snd]=dm;
      rcv[num_snd]=dm_r;
      num_snd++;            
    }

    // Do elem masks if necessary
    MEField<> *dem = src_mesh->GetField("elem_mask");
    if (dem != NULL) {
      MEField<> *dem_r = output_mesh->GetField("elem_mask");

      // load mask fields
      snd[num_snd]=dem;
      rcv[num_snd]=dem_r;
      num_snd++;            
    }

    // Do elem area if necessary
    MEField<> *dea = src_mesh->GetField("elem_area");
    if (dea != NULL) {
      MEField<> *dea_r = output_mesh->GetField("elem_area");

      // load mask fields
      snd[num_snd]=dea;
      rcv[num_snd]=dea_r;
      num_snd++;            
    }

    // Do elem creeped frac if necessary
    MEField<> *def = src_mesh->GetField("elem_frac2");
    if (def != NULL) {
      MEField<> *def_r = output_mesh->GetField("elem_frac2");

      // load mask fields
      snd[num_snd]=def;
      rcv[num_snd]=def_r;
      num_snd++;            
    }

    // Actually communicate fields
     dstComm.SendFields(num_snd, snd, rcv);
  } 


#if 0
  {
     // Get a list of the Mesh nodes with gids 
    MeshDB::iterator ni = output_mesh->node_begin(), ne = output_mesh->node_end();
    for (; ni != ne; ++ni) {
      MeshObj &node=*ni;
     
      printf("#%d E node id=%d owner=%d is_local=%d \n",Par::Rank(),node.get_id(),node.get_owner(),GetAttr(node).is_locally_owned());
    }

  }


  {
     // Get a list of the Mesh nodes with gids 
    MeshDB::iterator ei = output_mesh->elem_begin(), ee = output_mesh->elem_end();
    for (; ei != ee; ++ei) {
      MeshObj &elem=*ei;
     
      printf("#%d E elem id=%d owner=%d is_local=%d \n",Par::Rank(),elem.get_id(),elem.get_owner(),GetAttr(elem).is_locally_owned());
    }

  }
#endif



  // Output 
  *_output_mesh=output_mesh;
}



  
} // namespace
