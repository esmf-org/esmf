// $Id: ESMC_GridToMesh.C,v 1.12 2008/02/05 16:47:43 oehmke Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMC_GridToMesh.C"
//==============================================================================
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// Create a mesh from a given grid.
//
//-----------------------------------------------------------------------------

// ESMF's fun include strategy requires a specific order to includes for
// compilation to successfully complete.
#include <GridUtil/include/ESMC_GridToMesh.h>
#include <Grid/include/ESMCI_Grid.h>
#include "ESMC_VM.h"

#include "ESMC_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"             // for LogErr

#include "ESMC_Ptypes.h"
#include <Mesh/include/ESMC_Mesh.h>
#include <Mesh/include/ESMC_IOField.h>
#include <Mesh/include/ESMC_ParEnv.h>
#include <Mesh/include/ESMC_DDir.h>

#include <limits>
#include <iostream>

using namespace ESMC;

namespace ESMCI {

// *** Convert a grid to a mesh.  The staggerLoc should describe
// whether the mesh is at the dual location or coincident with the
// grid itself.  I am not sure how this is going to work, though,
// since the dual of a nice multi-patch grid could be a very bad
// object, not representable by a mesh.
//
//   o------------o-------------o
//   |     :      |      :      |
//   |     :      |      :      |
//   |.....x......|......x......|
//   |     :      |      :      |
//   |     :      |      :      |
//   o------------o-------------o
//   |     :      |      :      |
//   |     :      |      :      |
//   |.....x......|......x......|
//   |     :      |      :      |
//   |     :      |      :      |
//   o------------o-------------o
//   
//   E.G. mesh o---o, dual mesh x....x
//
// I think there is work here.  
// For the moment, we should at least be able to represent the grid itself,
// and, maybe, for simple single patch grids with a periodic component,
// the dual, which is not so bad.  This will put us equivalent with
// SCRIP.  
void GridToMesh(const Grid &grid_, int staggerLoc, ESMC::Mesh &mesh) {
#ifdef NOWAYMAN

  // Initialize the parallel environment for mesh (if not already done)
  ESMC::Par::Init("MESHLOG");

  Grid &grid = const_cast<Grid&>(grid_);

 try {

   // *** Grid error checking here ***


 
   // *** Set some meta-data ***
     // We set the topological dimension of the mesh (quad = 2, hex = 3, etc...)
   UInt pdim = grid.getRank();
   mesh.set_parametric_dimension(pdim);
  
     // In what dimension is the grid embedded?? (sphere = 3, simple rectangle = 2, etc...)
   // At this point the topological and spatial dim of the Grid is the same this should change soon
   UInt sdim = grid.getRank();
   mesh.set_spatial_dimension(sdim);
  

   // We save the nodes in a linear list so that we can access then as such
   // for cell creation.
   //TODO: NEED MAX LID METHOD SOON ->Bob
   std::vector<MeshObj*> nodevect(1000);


   // Set the id of this processor here (me)
   int localrc;
   int rc;
   int me = VM::getCurrent(&localrc)->getLocalPet();
//   if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)) // How should I handle ESMF LogError?
//      return;

   // Keep track of locally owned, shared and shared, not-locally-owned
   std::vector<UInt> owned_shared;
   std::vector<UInt> notowned_shared;


   // *** Create the Nodes ***
     // Loop nodes of the grid.  Here we loop all nodes, both owned and not.
   ESMCI::GridIter *gni=new ESMCI::GridIter(&grid,staggerLoc,true);

   // Put Local in first, so we don't have to search for duplicates for every interation, 
   // after locals are in put in non-local

   // loop through all LOCAL nodes in the Grid owned by cells
   for(gni->toBeg(); !gni->isDone(); gni->adv()) {   

     // Only operate on Local Nodes
     if (gni->isLocal()) {
       MeshObj *node;
       
       // get the global id of this Grid node
       int gid=gni->getGlobalID(); 
       
       // get the local id of this Grid node
       int lid=gni->getLocalID(); 

       // Create new node in mesh object       
       node = new MeshObj(MeshObj::NODE,     // node...
                                   gid,               // unique global id
                                   lid                // local ID for boostrapping field data
                                   );
       
       node->set_owner(me);  // Set owner to this proc
       
       UInt nodeset = 1;   // Do we need to partition the nodes in any sets?
       mesh.add_node(node, nodeset);
       
       // If Shared add to list to use DistDir on
       if (gni->isShared()) {
         
         // Put gid in list
         std::vector<UInt>::iterator lb =
           std::lower_bound(owned_shared.begin(), owned_shared.end(), gid);    
         
         if (lb == owned_shared.end() || *lb != gid)
           owned_shared.insert(lb, gid);
       }
       
       // Put node into vector
       nodevect[lid]=node;    
     }
     
   } // gni


   // loop through all NON-LOCAL nodes in the Grid owned by cells
   for(gni->toBeg(); !gni->isDone(); gni->adv()) {   

     // Only operate on non-local nodes
     if (!gni->isLocal()) {
       MeshObj *node;
       
       // get the global id of this Grid node
       int gid=gni->getGlobalID(); 
       
       // get the local id of this Grid node
       int lid=gni->getLocalID(); 
       
       // If Grid node is not already in the mesh then add 
       Mesh::MeshObjIDMap::iterator mi =  mesh.map_find(MeshObj::NODE, gid);
       if (mi == mesh.map_end(MeshObj::NODE)) {
         node = new MeshObj(MeshObj::NODE,    // node...
                            gid,              // unique global id
                            lid               // local ID for boostrapping field data
                            );
         
         node->set_owner(std::numeric_limits<UInt>::max());  // Set owner to unknown (will have to ghost later)
         
         UInt nodeset = 1;   // Do we need to partition the nodes in any sets?
         mesh.add_node(node, nodeset);
         
         // Node must be shared
         std::vector<UInt>::iterator lb =
           std::lower_bound(notowned_shared.begin(), notowned_shared.end(), gid);    
         
         if (lb == notowned_shared.end() || *lb != gid)
           notowned_shared.insert(lb, gid);
         
       } else {
         node=&*mi; 
       }
       
       // Put node into vector
       nodevect[lid]=node;    
     }
   } // gni
  

   // Use DistDir to fill node owners for non-local nodes 
   // TODO: Use nodes which are gni->isLocal() && gni->isShared() to get owners of
   //       non-local nodes ->David


   // *** Create the Cells ***  

   // Presumably, for a structured grid there is only the
   // 'hypercube' topology, i.e. a quadrilateral for 2d
   // and a hexahedron for 3d
   const MeshObjTopo *ctopo = pdim == 2 ? GetTopo("QUAD") :
                                         (pdim == 3 ? GetTopo("HEX") : 0);
   if (!ctopo)
     Throw() << "Could not get topology for pdim=" << pdim;

   // Allocate vector to hold nodes for translation
   std::vector<MeshObj*> nodes(ctopo->num_nodes);


   // Loop Cells of the grid. 
   ESMCI::GridCellIter *gci=new ESMCI::GridCellIter(&grid,staggerLoc);

   for(gci->toBeg(); !gci->isDone(); gci->adv()) {   
     MeshObj *cell = new MeshObj(MeshObj::ELEMENT,     // Mesh equivalent of Cell
                                 gci->getGlobalID(),   // unique global id
                                 gci->getLocalID()    // index for bootstrapping field data
                                 );
  
     // Set Owner
     cell->set_owner(me);
   
     // Get Local Ids of Corners
     int cnrCount;
     int cnrList[16]; // ONLY WORKS FOR UP TO 4D
     gci->getCornersCellNodeLocalID(&cnrCount, cnrList);
     ThrowRequire(cnrCount == ctopo->num_nodes);

     // Get Nodes via Local IDs
     for (UInt n = 0; n < ctopo->num_nodes; ++n) {
       nodes[n] = nodevect[cnrList[n]];
     } // n


     UInt block_id = 1;  // Any reason to use different sets for cells?

     mesh.add_element(cell, nodes, block_id, ctopo);
     
   } // ci


     // Now set up the nodal coordinates
   IOField<NodalField> *node_coord = mesh.RegisterNodalField(mesh, "coordinates", sdim);

   // Loop through Mesh nodes setting up coordinates
   MeshDB::iterator ni = mesh.node_begin(), ne = mesh.node_end();

   for (; ni != ne; ++ni) {
     double *c = node_coord->data(*ni);

     UInt idx = ni->get_data_index(); // we set this above when creating the node

     // Move to corresponding grid node
    gni->moveToLocalID(idx);      

    // If local fill in coords
    if (gni->isLocal()) {
     gni->getCoord(c);
    } else { // set to Null value to be ghosted later
      for (int i=0; i<sdim; i++) {
        c[i]=std::numeric_limits<double>::max();
      }
    }

    printf("%d :: %f %f\n",gni->getGlobalID(),c[0],c[1]);

   } // ni


   // Now go back and resolve the ownership for shared nodes.  We create a distdir
   // from the owned, shared nodes, then look up the shared, not owned.
   {
     DDir<> dir;
   
     std::vector<UInt> lids(owned_shared.size(), 0);
     dir.Create(owned_shared.size(), &owned_shared[0], &lids[0]);
  
     std::vector<DDir<>::dentry> lookups;
     dir.RemoteGID(notowned_shared.size(), &notowned_shared[0], lookups);

     // Loop through the results.  Do a map lookup to find nodes--since the shared 
     // porition of the mesh is a hypersurface, this should be cheap enough to do.
     std::vector<DDir<>::dentry>::iterator ri = lookups.begin(), re = lookups.end();
     for (; ri != re; ++ri) {

       DDir<>::dentry &dent = *ri;

       Mesh::MeshObjIDMap::iterator mi =  mesh.map_find(MeshObj::NODE, dent.gid);
       ThrowRequire(mi != mesh.map_end(MeshObj::NODE));
 
       mi->set_owner(dent.origin_proc);

     } // ri

   } // ddir lookup

   // ** That's it.  The mesh is now in the pre-commit phase, so other
   // fields can be registered, sides/faces can be turned on, etc, or one
   // can simply call mesh.Commit() and then proceed.
   char buf[512];
   //std::sprintf(buf, "g2m.%05d.txt", )
   mesh.Print(Par::Out());


 } // try catch block
  catch (std::exception &x) {

  // Set any ESMF error codes/messages

 } // catch
  catch(...) {

  // Set any ESMF error codes/messages

 }

#endif
}


} // namespace

