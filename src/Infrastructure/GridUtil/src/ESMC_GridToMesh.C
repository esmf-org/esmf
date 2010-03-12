// $Id: ESMC_GridToMesh.C,v 1.40.2.2 2010/03/12 04:35:15 oehmke Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
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
#include "ESMCI_VM.h"

#include "ESMCI_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"             // for LogErr

#include "ESMC_Ptypes.h"
#include <Mesh/include/ESMCI_Mesh.h>
#include <Mesh/include/ESMCI_IOField.h>
#include <Mesh/include/ESMCI_ParEnv.h>
#include <Mesh/include/ESMCI_DDir.h>

#include <limits>
#include <iostream>
#include <vector>
#include <map>
#include <cmath>


// Some xlf compilers don't define this
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

//#define G2M_DBG

// using namespace ESMCI;

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

void GridToMesh(const Grid &grid_, int staggerLoc, ESMCI::Mesh &mesh, const std::vector<ESMCI::Array*> &arrays, ESMCI::InterfaceInt *maskValuesArg) {
#undef  ESMC_METHOD
#define ESMC_METHOD "GridToMesh()" 
  Trace __trace("GridToMesh(const Grid &grid_, int staggerLoc, ESMCI::Mesh &mesh)");

 int localrc;
 int rc;

  // Initialize the parallel environment for mesh (if not already done)
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,ESMF_ERR_PASSTHRU,NULL))
   throw localrc;  // bail out with exception

  Grid &grid = const_cast<Grid&>(grid_);


 bool is_sphere = grid.isSphere();

 // *** Grid error checking here ***
 if (!grid.hasCoordStaggerLoc(staggerLoc))
   Throw() << "Grid being used in Regrid call does not contain coordinates at appropriate staggerloc";

 if (grid.getIndexFlag() != ESMF_INDEX_GLOBAL) {
   Throw() << "Currently the Grid must be created with indexflag=ESMF_INDEX_GLOBAL to use this functionality";
  }

 
 // *** Set some meta-data ***
 // We set the topological dimension of the mesh (quad = 2, hex = 3, etc...)
 UInt pdim = grid.getDimCount();
 mesh.set_parametric_dimension(pdim);
 
 // In what dimension is the grid embedded?? (sphere = 3, simple rectangle = 2, etc...)
 // At this point the topological and spatial dim of the Grid is the same this should change soon
 UInt sdim = grid.getDimCount();
 
 
 if (is_sphere) {
   //std::cout << "g2m, is sphere=1" << std::endl;
   sdim = 3;
 }

 mesh.set_spatial_dimension(sdim);
 
 // see if grid has a mask field
 bool hasMask=false;
 int numMaskValues=0;
 int *ptrMaskValues;
 if (grid.hasItemStaggerLoc(staggerLoc, ESMC_GRIDITEM_MASK)) {
   if (maskValuesArg != NULL) {
     // Error check mask values
     if (maskValuesArg->dimCount != 1) {
       Throw() << " Mask values must be of rank 1.";
     }
     
     // Get mask values
     numMaskValues=maskValuesArg->extent[0];
     ptrMaskValues=&(maskValuesArg->array[0]);
     
     // Turn on masking
     hasMask=true;
   } else {
     hasMask=false;
   }
 }
 
 // We save the nodes in a linear list so that we can access then as such
 // for cell creation.
 //TODO: NEED MAX LID METHOD SOON ->Bob
 std::vector<MeshObj*> nodevect(100000);
 std::map<UInt,UInt> ngid2lid;
 
 UInt local_node_num = 0, local_elem_num = 0;
 
 
 // Set the id of this processor here (me)
 int me = VM::getCurrent(&localrc)->getLocalPet();
 if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,ESMF_ERR_PASSTHRU,NULL))
   throw localrc;  // bail out with exception

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

#ifdef G2M_DBG
Par::Out() << "GID=" << gid << ", LID=" << lid << std::endl;
#endif

       // Create new node in mesh object       
       node = new MeshObj(MeshObj::NODE,     // node...
                                   gid,               // unique global id
                                   local_node_num
                                   );

       ngid2lid[gid] = lid;
       local_node_num++;
       
       
       node->set_owner(me);  // Set owner to this proc
       
       UInt nodeset = is_sphere ? gni->getPoleID() : 0;   // Do we need to partition the nodes in any sets?
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
                            local_node_num   // local ID for boostrapping field data
                            );

         ngid2lid[gid] = lid;
         local_node_num++;
         
         node->set_owner(std::numeric_limits<UInt>::max());  // Set owner to unknown (will have to ghost later)
         
         UInt nodeset = is_sphere ? gni->getPoleID() : 0;   // Do we need to partition the nodes in any sets?
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
   const MeshObjTopo *ctopo = 0;
   if (pdim == 2) {
   
     ctopo = sdim == 2 ? GetTopo("QUAD") : GetTopo("QUAD_3D");
                                         
   } else if (pdim == 3) {

     ThrowRequire(sdim == 3);
     ctopo = GetTopo("HEX");

   } else Throw() << "Invalid parametric dim:" << pdim;

   if (!ctopo)
     Throw() << "Could not get topology for pdim=" << pdim;

   // Allocate vector to hold nodes for translation
   std::vector<MeshObj*> nodes(ctopo->num_nodes);


   // Loop Cells of the grid. 
   ESMCI::GridCellIter *gci=new ESMCI::GridCellIter(&grid,staggerLoc);

   for(gci->toBeg(); !gci->isDone(); gci->adv()) {   
     MeshObj *cell = new MeshObj(MeshObj::ELEMENT,     // Mesh equivalent of Cell
                                 gci->getGlobalID(),   // unique global id
                                 local_elem_num++
                                 );

#ifdef G2M_DBG
Par::Out() << "Cell:" << cell->get_id() << " uses nodes:";
#endif
  
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
#ifdef G2M_DBG
Par::Out() << nodes[n]->get_id() << " ";
#endif
     } // n
#ifdef G2M_DBG
Par::Out() << std::endl;
#endif


     UInt block_id = 1;  // Any reason to use different sets for cells?

     mesh.add_element(cell, nodes, block_id, ctopo);
     
   } // ci

    // Remove any superfluous nodes
    mesh.remove_unused_nodes();
    mesh.linearize_data_index();

     // Now set up the nodal coordinates
   IOField<NodalField> *node_coord = mesh.RegisterNodalField(mesh, "coordinates", sdim);

   // Create whatever fields the user wants
   std::vector<IOField<NodalField>*> nfields;
   for (UInt i = 0; i < arrays.size(); ++i) {
     char buf[512];
     std::sprintf(buf, "array_%03d", i);
     nfields.push_back(
             mesh.RegisterNodalField(mesh, buf, 1)
                      );
     nfields.back()->set_output_status(true);
   }

   // Setup the mask field if necessary
   IOField<NodalField> *node_mask;
   if (hasMask) {
     node_mask = mesh.RegisterNodalField(mesh, "mask", 1);
   }


#ifdef G2M_DBG
   IOField<NodalField> *de_field = mesh.RegisterNodalField(mesh, "de", 1);
     de_field->set_output_status(true);
#endif

   // Loop through Mesh nodes setting up coordinates
   MeshDB::iterator ni = mesh.node_begin(), ne = mesh.node_end();

   for (; ni != ne; ++ni) {
     double *c = node_coord->data(*ni);

     double fdata;

     UInt lid = ngid2lid[ni->get_id()]; // we set this above when creating the node

//Par::Out() << "node:" << ni->get_id() << ", lid=" << lid;

     // Move to corresponding grid node
    gni->moveToLocalID(lid);      

    // If local fill in coords
    if (gni->isLocal()) {

     gni->getCoord(c);

     double DEG2RAD = M_PI/180.0;
     if (is_sphere) {

         double lon = c[0];
          double lat = c[1];
          double ninety = 90.0;
          double theta = DEG2RAD*lon, phi = DEG2RAD*(ninety-lat);
          c[0] = std::cos(theta)*std::sin(phi);
          c[1] = std::sin(theta)*std::sin(phi);
          c[2] = std::cos(phi);

 
     }

    } else { // set to Null value to be ghosted later
      for (int i=0; i<sdim; i++) {
   //     c[i]=std::numeric_limits<double>::max();
        c[i]=-10;
      }
    }

    // Other arrays
    for (UInt i = 0; i < arrays.size(); ++i) {
      gni->getArrayData(arrays[i], &fdata);
      double *data = nfields[i]->data(*ni);
      ThrowRequire(data);
      data[0] = fdata;
    }

#ifdef G2M_DBG
    // De field
    double *data = de_field->data(*ni);

    ThrowRequire(data);
    data[0] = gni->getDE();
#endif

    //printf("%d :: %f %f\n",gni->getGlobalID(),c[0],c[1]);

   } // ni


   // Loop through Mesh nodes setting up mask
   if (hasMask) {
     for (ni = mesh.node_begin(); ni != ne; ++ni) {
       double *m = node_mask->data(*ni);
       
       UInt lid = ngid2lid[ni->get_id()]; // we set this above when creating the node
       
       // Move to corresponding grid node
       gni->moveToLocalID(lid);      
       
       // If local fill in coords
       if (gni->isLocal()) {
	 ESMC_I4 gm;

	 // Get Mask value from grid
	 gni->getItem(ESMC_GRIDITEM_MASK, &gm);
	 
	 // See if gm matches any mask values
	 bool mask=false;
         for (int i=0; i<numMaskValues; i++) {
	   ESMC_I4 mv=ptrMaskValues[i];
	   if (mv==gm) {
	     mask=true;
	     break;
	   }
	 }

	 // Set Mask based on grid mask value
	 if (mask) {
	   *m=1.0;
	 } else {
	   *m=0.0;
	 }

       } else { // set to Null value to be ghosted later
	 *m=0.0;
       }
     } // ni
   }

   // Now go back and resolve the ownership for shared nodes.  We create a distdir
   // from the owned, shared nodes, then look up the shared, not owned.
   {
     DDir<> dir;
   
     std::vector<UInt> lids(owned_shared.size(), 0);
     if (owned_shared.size ())
       dir.Create(owned_shared.size(), &owned_shared[0], &lids[0]);
     else
       dir.Create(0, (UInt*) NULL, 0);
  
     std::vector<DDir<>::dentry> lookups;
     if (notowned_shared.size ())
       dir.RemoteGID(notowned_shared.size(), &notowned_shared[0], lookups);
     else
       dir.RemoteGID(0, (UInt *) NULL, lookups);

     // Loop through the results.  Do a map lookup to find nodes--since the shared 
     // porition of the mesh is a hypersurface, this should be cheap enough to do.
     std::vector<DDir<>::dentry>::iterator ri = lookups.begin(), re = lookups.end();
     for (; ri != re; ++ri) {

       DDir<>::dentry &dent = *ri;

#ifdef G2M_DBG
Par::Out() << "Finding owner for gid" << dent.gid << " = " << dent.origin_proc << std::endl;
#endif

       Mesh::MeshObjIDMap::iterator mi =  mesh.map_find(MeshObj::NODE, dent.gid);
     //  ThrowRequire(mi != mesh.map_end(MeshObj::NODE));
       // May have been deleted as an unused node.  
       if (mi == mesh.map_end(MeshObj::NODE)) {
#ifdef G2M_DBG
Par::Out() << "\tnot in mesh!!" << std::endl;
#endif
         continue;
       }
 
       mi->set_owner(dent.origin_proc);

     } // ri

   } // ddir lookup


   // Can now build the communication pattern.
   mesh.build_sym_comm_rel(MeshObj::NODE);

   // ** That's it.  The mesh is now in the pre-commit phase, so other
   // fields can be registered, sides/faces can be turned on, etc, or one
   // can simply call mesh.Commit() and then proceed.
   char buf[512];
   //std::sprintf(buf, "g2m.%05d.txt", )
#ifdef G2M_DBG
   mesh.Print(Par::Out());
#endif

   mesh.Commit();

   {
     std::vector<MEField<>*> fds;

     Mesh::FieldReg::MEField_iterator fi = mesh.Field_begin(), fe = mesh.Field_end();

     for (; fi != fe; ++fi) fds.push_back(&*fi);

     mesh.HaloFields(fds.size(), &fds[0]);
 
/*
     MEField<> *cptr = mesh.GetCoordField();
     mesh.HaloFields(1, &cptr);
*/
   }


   // delete Grid Iters
   delete gni;
   delete gci;

}
#undef  ESMC_METHOD

} // namespace

