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
#define ESMC_FILENAME "ESMCI_GridToMesh.C"
//==============================================================================
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// Create a mesh from a given grid.
//
//-----------------------------------------------------------------------------

#include "ESMCI_GridToMesh.h"

#include "ESMCI_Grid.h"
#include "ESMCI_VM.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Ptypes.h"
#include "Mesh/include/ESMCI_Mesh.h"
#include "Mesh/include/ESMCI_IOField.h"
#include "Mesh/include/ESMCI_ParEnv.h"
#include "Mesh/include/ESMCI_DDir.h"
#include "Mesh/include/ESMCI_MathUtil.h"
#include "Mesh/include/ESMCI_Phedra.h"

#include "PointList/include/ESMCI_PointList.h"

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

  //// MOVED TO ESMCI_Mesh_GToM_Glue.C ////
 #if 0

  extern bool grid_debug;

// *** Convert a grid to a mesh.  The staggerLoc should describe
// whether the mesh is at the dual location or coincident with the
// grid itself.  I am not sure how this is going to work, though,
// since the dual of a nice multi-tile grid could be a very bad
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
// and, maybe, for simple single tile grids with a periodic component,
// the dual, which is not so bad.  This will put us equivalent with
// SCRIP.

void GridToMesh(const Grid &grid_, int staggerLoc, ESMCI::Mesh &mesh,
  const std::vector<ESMCI::Array*> &arrays, ESMCI::InterArray<int> *maskValuesArg,
  int *regridConserve) {
#undef  ESMC_METHOD
#define ESMC_METHOD "GridToMesh()"
  Trace __trace("GridToMesh(const Grid &grid_, int staggerLoc, ESMCI::Mesh &mesh)");

 int localrc;
 int rc;

printf("HERE IN GTOM\n");

  // Initialize the parallel environment for mesh (if not already done)
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
   throw localrc;  // bail out with exception

  Grid &grid = const_cast<Grid&>(grid_);

 bool is_sphere = grid.isSphere();


 // *** Grid error checking here ***
 if (!grid.hasCoordStaggerLoc(staggerLoc)) {
   ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
            "- Grid being used in Regrid call does not contain coordinates at appropriate staggerloc ", ESMC_CONTEXT, &localrc);
    throw localrc;
 }



 // *** Set some meta-data ***
 // We set the topological dimension of the mesh (quad = 2, hex = 3, etc...)
 UInt pdim = grid.getDimCount();
 mesh.set_parametric_dimension(pdim);

 // In what dimension is the grid embedded?? (sphere = 3, simple rectangle = 2, etc...)
 UInt sdim = grid.getCartCoordDimCount();
 // if ((sdim<3)&&is_sphere) Throw()<<"Sphere's not supported with less than 3 dimesnions";

 mesh.set_spatial_dimension(sdim);

 // Tell Mesh if it's a sphere
 ESMC_CoordSys_Flag coordSys=grid.getCoordSys();
 if ((coordSys==ESMC_COORDSYS_SPH_DEG) || (coordSys==ESMC_COORDSYS_SPH_RAD)) {
   mesh.is_sph=true;
 } else {
   mesh.is_sph=false;
 }

 // See if this is for conservative regridding
 bool isConserve=false;
 if (*regridConserve == ESMC_REGRID_CONSERVE_ON) isConserve=true;

 // see if grid has a mask field
 bool hasMask=false;
 if (isConserve) {
   // If conservative check for masking on center stagger
   if (grid.hasItemStaggerLoc(0, ESMC_GRIDITEM_MASK)) {
     hasMask=true;
   } else {
     hasMask=false;
   }
 } else {
   if (grid.hasItemStaggerLoc(staggerLoc, ESMC_GRIDITEM_MASK)) {
       hasMask=true;
   } else {
     hasMask=false;
   }
 }

 // Get Mask values if necessary
 int numMaskValues=0;
 int *ptrMaskValues;
 if (hasMask) {
   if (present(maskValuesArg)) {
     // Error check mask values
     if (maskValuesArg->dimCount != 1) {
       Throw() << " Mask values must be of rank 1.";
     }

     // Get mask values
     numMaskValues=maskValuesArg->extent[0];
     ptrMaskValues=&(maskValuesArg->array[0]);
    }
 }


 // See if grid has area field
 // (Area only useful for conservative)
 bool hasArea=false;
 if (isConserve) {
   // If conservative check for masking on center stagger
   if (grid.hasItemStaggerLoc(0, ESMC_GRIDITEM_AREA)) {
     hasArea=true;
   } else {
     hasArea=false;
   }
 }


 // We save the nodes in a linear list so that we can access then as such
 // for cell creation.
 std::map<UInt,MeshObj*> nodemap;
 std::map<UInt,UInt> ngid2lid;

 UInt local_node_num = 0, local_elem_num = 0;


 // Set the id of this processor here (me)
 int me = VM::getCurrent(&localrc)->getLocalPet();
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
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

       //       UInt nodeset = is_sphere ? gni->getPoleID() : 0;   // Do we need to partition the nodes in any sets?
       UInt nodeset = gni->getPoleID();   // Do we need to partition the nodes in any sets?
       mesh.add_node(node, nodeset);


       // If Shared add to list to use DistDir on
       if (gni->isShared()) {

         // Put gid in list
         std::vector<UInt>::iterator lb =
           std::lower_bound(owned_shared.begin(), owned_shared.end(), gid);

         if (lb == owned_shared.end() || *lb != gid)
           owned_shared.insert(lb, gid);
       }

       // Put node into map
       nodemap[lid]=node;
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

         //         UInt nodeset = is_sphere ? gni->getPoleID() : 0;   // Do we need to partition the nodes in any sets?
         UInt nodeset = gni->getPoleID();   // Do we need to partition the nodes in any sets?
         mesh.add_node(node, nodeset);

         // Node must be shared
         std::vector<UInt>::iterator lb =
           std::lower_bound(notowned_shared.begin(), notowned_shared.end(), gid);

         if (lb == notowned_shared.end() || *lb != gid)
           notowned_shared.insert(lb, gid);

       } else {
         node=&*mi;
       }

       // Put node into map
       nodemap[lid]=node;
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

   // Allocate vector to hold nodes for translation
   std::vector<int> uniq_node_ids(ctopo->num_nodes);

   // Loop Cells of the grid.
   ESMCI::GridCellIter *gci=new ESMCI::GridCellIter(&grid,staggerLoc);

   for(gci->toBeg(); !gci->isDone(); gci->adv()) {

     // Get Local Ids of Corners
     int cnrCount;
     int cnrList[16]; // ONLY WORKS FOR UP TO 4D
     gci->getCornersCellNodeLocalID(&cnrCount, cnrList);
     ThrowRequire(cnrCount == ctopo->num_nodes);

     // Get Nodes via Local IDs
     for (UInt n = 0; n < ctopo->num_nodes; ++n) {
       nodes[n] = nodemap[cnrList[n]];
     } // n

     // If cell is degenerate then don't create.
     // If there are less than 3 unique nodes
     // then it's just a line
     int num_uniq_node_ids=0;
     for (UInt n = 0; n < ctopo->num_nodes; ++n) {
       bool is_uniq=true;
       for (int i=0; i<num_uniq_node_ids; i++) {
         if (nodes[n]->get_id()==uniq_node_ids[i]) {
           is_uniq=false;
         }
       }

       if (is_uniq) {
         uniq_node_ids[num_uniq_node_ids]=nodes[n]->get_id();
         num_uniq_node_ids++;
       }
     } // n

     // TODO: Make the number depend on the parametric dimension
     // TOD0: If the number of nodes is smaller than ctopo->num_nodes make a different topo?
     if (num_uniq_node_ids<3) {
       continue;
     }

     // Create Cell
     MeshObj *cell = new MeshObj(MeshObj::ELEMENT,     // Mesh equivalent of Cell
                                 gci->getGlobalID(),   // unique global id
                                 local_elem_num++
                                 );

#ifdef G2M_DBG
     Par::Out() << "Cell:" << cell->get_id() << " uses nodes:";
#endif

     // Set Owner
     cell->set_owner(me);

     UInt block_id = 1;  // Any reason to use different sets for cells?

     mesh.add_element(cell, nodes, block_id, ctopo);

   } // ci

    // Remove any superfluous nodes
    mesh.remove_unused_nodes();
    mesh.linearize_data_index();

     // Now set up the nodal coordinates
   IOField<NodalField> *node_coord = mesh.RegisterNodalField(mesh, "coordinates", sdim);

#if 0
  if (*regridConserve == ESMC_REGRID_CONSERVE_ON) {
    // Register the iwts field
    // TODO:  This should be in a more standard location,
    //        iwts should not be on every mesh, need a flag
    Context ctxt; ctxt.flip();
    MEField<> *iwts = mesh.RegisterField("iwts",
      MEFamilyStd::instance(), MeshObj::ELEMENT, ctxt, 1, true);
  }
#endif

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

    // If local fill in cartesian coords
    if (gni->isLocal()) {
     gni->getCartCoord(c);
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

   // Setup the fraction field
   if (isConserve) {
     // Register mask field on elems
     Context ctxt; ctxt.flip();
     MEField<> *elem_frac = mesh.RegisterField("elem_frac",
                        MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);
     MEField<> *elem_frac2 = mesh.RegisterField("elem_frac2",
                        MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);
   }



   // Setup the mask field if necessary
   if (hasMask) {
     if (isConserve) {
       // Register mask field on elems
       Context ctxt; ctxt.flip();
       MEField<> *elem_mask = mesh.RegisterField("elem_mask",
                  MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);

     } else {
       IOField<NodalField> *node_mask;
       node_mask = mesh.RegisterNodalField(mesh, "mask", 1);
     }
   }

   // Setup the area field if necessary
   if (hasArea) {
     if (isConserve) {
       // Register area field on elems
       Context ctxt; ctxt.flip();
       MEField<> *elem_area = mesh.RegisterField("elem_area",
                  MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);
       }
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

   // Setup the mask field if necessary
   if (hasMask) {
     if (isConserve) {
       // Get mask field on elems
      MEField<> *elem_mask=mesh.GetField("elem_mask");

       // Loop elemets of the grid.  Here we loop all elements, both owned and not.
       for(gci->toBeg(); !gci->isDone(); gci->adv()) {

         // get the global id of this Grid node
         int gid=gci->getGlobalID();

         //  Find the corresponding Mesh element
         Mesh::MeshObjIDMap::iterator mi =  mesh.map_find(MeshObj::ELEMENT, gid);
         if (mi == mesh.map_end(MeshObj::ELEMENT)) {
           Throw() << "Grid entry not in mesh";
         }

         // Get the element
         const MeshObj &elem = *mi;

         // Get mask data
         double *m=elem_mask->data(elem);

         // Init in case is not locally owned
         *m=0.0;

         // Only put it in if it's locally owned
         if (!GetAttr(elem).is_locally_owned()) continue;

         // Get mask from the Item Array
         ESMC_I4 gm;

         // Get Mask value from grid
         gci->getItem(ESMC_GRIDITEM_MASK, &gm);

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
       }
     } else {
       // Get mask field on elems
      MEField<> *node_mask=mesh.GetField("mask");

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
   }



   // Setup the area field if necessary
   if (hasArea) {
     if (isConserve) {
       // Register area field on elems
       // Get area field on elems
       MEField<> *elem_area=mesh.GetField("elem_area");

       // Loop elemets of the grid.  Here we loop all elements, both owned and not.
       for(gci->toBeg(); !gci->isDone(); gci->adv()) {

         // get the global id of this Grid node
         int gid=gci->getGlobalID();

         //  Find the corresponding Mesh element
         Mesh::MeshObjIDMap::iterator mi =  mesh.map_find(MeshObj::ELEMENT, gid);
         if (mi == mesh.map_end(MeshObj::ELEMENT)) {
           Throw() << "Grid entry not in mesh";
         }

         // Get the element
         const MeshObj &elem = *mi;

         // Get mask data
         double *a=elem_area->data(elem);

         // Init in case is not locally owned
         *a=0.0;

         // Only put it in if it's locally owned
         if (!GetAttr(elem).is_locally_owned()) continue;

         // Get mask from the Item Array
         ESMC_R8 ga;

         // Get Mask value from grid
         gci->getItem(ESMC_GRIDITEM_AREA, &ga);

         // Set value
         *a=ga;
       }
     }
   }

  if(isConserve) { // set up frac2 field
    MEField<> *frac2=mesh.GetField("elem_frac2");
    MeshDB::iterator ei=mesh.elem_begin(), ee=mesh.elem_end();
    for (; ei!=ee; ei++) {
      double *e=frac2->data(*ei);
      *e = 1.0;
    }
  }


#if 0
   if (hasMask) {
   // Test getting getting mask data
   MEField<> *tst_field_ptr=mesh.GetField("elem_mask");

   printf("tst_field_ptr=%d \n",(int )tst_field_ptr);

   MeshDB::iterator ei=mesh.elem_begin(), ee=mesh.elem_end();

   for (; ei!=ee; ei++) {
     double *e=tst_field_ptr->data(*ei);
     if (*e >0.5) printf("%d %f \n",ei->get_id(),*e);
   }

   }
#endif


   // Halo Fields
   {
     std::vector<MEField<>*> fds;

     Mesh::FieldReg::MEField_iterator fi = mesh.Field_begin(), fe = mesh.Field_end();

     for (; fi != fe; ++fi) fds.push_back(&*fi);

     mesh.HaloFields(fds.size(), &fds[0]);
   }

   // delete Grid Iters
   delete gni;
   delete gci;

}
#undef  ESMC_METHOD


  // Only works for scalar data right now, but would be pretty easy to add more dimensions
void CpMeshDataToArray(Grid &grid, int staggerLoc, ESMCI::Mesh &mesh, ESMCI::Array &array, MEField<> *dataToArray) {
#undef  ESMC_METHOD
#define ESMC_METHOD "CpMeshDataToArray()"
  Trace __trace("CpMeshDataToArray()");

 int localrc;
 int rc;

  // Initialize the parallel environment for mesh (if not already done)
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
   throw localrc;  // bail out with exception

 bool is_sphere = grid.isSphere();

 // Loop nodes of the grid.  Here we loop all nodes, both owned and not.
   ESMCI::GridIter *gni=new ESMCI::GridIter(&grid,staggerLoc,true);

   // loop through all nodes in the Grid
   for(gni->toBeg(); !gni->isDone(); gni->adv()) {
     if(!gni->isLocal()) continue;

       // get the global id of this Grid node
       int gid=gni->getGlobalID();

       //  Find the corresponding Mesh node
       Mesh::MeshObjIDMap::iterator mi =  mesh.map_find(MeshObj::NODE, gid);
       if (mi == mesh.map_end(MeshObj::NODE)) {
         Throw() << "Grid entry not in mesh";
       }

       // Get the node
        const MeshObj &node = *mi;

       // Get the data
        double *data = dataToArray->data(node);

       // Put it into the Array
      gni->setArrayData(&array, *data);
   }


   // delete Grid Iters
   delete gni;

}
#undef  ESMC_METHOD


  // Assumes array is on center staggerloc of grid
  void CpMeshElemDataToArray(Grid &grid, int staggerloc, ESMCI::Mesh &mesh, ESMCI::Array &array, MEField<> *dataToArray) {
#undef  ESMC_METHOD
#define ESMC_METHOD "CpMeshElemDataToArray()"
  Trace __trace("CpMeshElemDataToArray()");

 int localrc;
 int rc;


  // Initialize the parallel environment for mesh (if not already done)
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
   throw localrc;  // bail out with exception


    // Loop elemets of the grid.  Here we loop all elements, both owned and not.
    ESMCI::GridCellIter *gci=new ESMCI::GridCellIter(&grid,staggerloc);

    // loop through all nodes in the Grid
    for(gci->toBeg(); !gci->isDone(); gci->adv()) {

      // get the global id of this Grid node
      int gid=gci->getGlobalID();

      //  Find the corresponding Mesh element
      Mesh::MeshObjIDMap::iterator mi =  mesh.map_find(MeshObj::ELEMENT, gid);
      if (mi == mesh.map_end(MeshObj::ELEMENT)) {
        Throw() << "Grid entry not in mesh";
      }

      // Get the element
      const MeshObj &elem = *mi;

      // Only put it in if it's locally owned
      if (!GetAttr(elem).is_locally_owned()) continue;


       // Get the data
        double *data = dataToArray->data(elem);

        // DEBUG:  printf("G2M %d %f \n",gid,*data);

       // Put it into the Array
      gci->setArrayData(&array, *data);
   }

   // delete Grid Iters
   delete gci;
}





  void PutElemAreaIntoArray(Grid &grid, int staggerLoc, ESMCI::Mesh &mesh, ESMCI::Array &array) {
#undef  ESMC_METHOD
#define ESMC_METHOD "PutElemAreaIntoArray()"
    Trace __trace("PutElemAreaIntoArray()");

    int localrc;
    int rc;

#define  MAX_NUM_POLY_COORDS  60
#define  MAX_NUM_POLY_NODES_2D  30  // MAX_NUM_POLY_COORDS/2
#define  MAX_NUM_POLY_NODES_3D  20  // MAX_NUM_POLY_COORDS/3

    int num_poly_nodes;
    double poly_coords[MAX_NUM_POLY_COORDS];
    double tmp_coords[MAX_NUM_POLY_COORDS];


    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception


    // Setup interator to Loop elemets of the grid.  Here we loop all elements, both owned and not.
    ESMCI::GridCellIter *gci=new ESMCI::GridCellIter(&grid,staggerLoc);


    // If an area field exists use that instead
    // TODO: replace this with something that doesn't require building a mesh first
    MEField<> *area_field = mesh.GetField("elem_area");
    if (area_field) {

      // loop through all nodes in the Grid
      for(gci->toBeg(); !gci->isDone(); gci->adv()) {

        // get the global id of this Grid node
        int gid=gci->getGlobalID();

        //  Find the corresponding Mesh element
        Mesh::MeshObjIDMap::iterator mi =  mesh.map_find(MeshObj::ELEMENT, gid);
        if (mi == mesh.map_end(MeshObj::ELEMENT)) {
          Throw() << "Grid entry not in mesh";
        }

        // Get the element
        const MeshObj &elem = *mi;

        // Only put it in if it's locally owned
        if (!GetAttr(elem).is_locally_owned()) continue;

        // Get area from field
        double *area=area_field->data(elem);

        // Put it into the Array
        gci->setArrayData(&array, *area);
      }

      return;
    }


    ////// Otherwise calculate areas.....

    // Get coord field
    MEField<> *cfield = mesh.GetCoordField();

    // Get dimensions
    int sdim=mesh.spatial_dim();
    int pdim=mesh.parametric_dim();

    // loop through all nodes in the Grid
    for(gci->toBeg(); !gci->isDone(); gci->adv()) {

      // get the global id of this Grid node
      int gid=gci->getGlobalID();

      //  Find the corresponding Mesh element
      Mesh::MeshObjIDMap::iterator mi =  mesh.map_find(MeshObj::ELEMENT, gid);
      if (mi == mesh.map_end(MeshObj::ELEMENT)) {
        Throw() << "Grid entry not in mesh";
      }

      // Get the element
      const MeshObj &elem = *mi;

      // Only put it in if it's locally owned
      if (!GetAttr(elem).is_locally_owned()) continue;

      // Get area depending on dimensions
      double area;

      if (pdim==2) {
        if (sdim==2) {
          get_elem_coords_2D_ccw(&elem, cfield, MAX_NUM_POLY_NODES_2D, tmp_coords, &num_poly_nodes, poly_coords);
          remove_0len_edges2D(&num_poly_nodes, poly_coords);
          area=area_of_flat_2D_polygon(num_poly_nodes, poly_coords);
        } else if (sdim==3) {
          get_elem_coords_3D_ccw(&elem, cfield, MAX_NUM_POLY_NODES_3D, tmp_coords, &num_poly_nodes, poly_coords);
          remove_0len_edges3D(&num_poly_nodes, poly_coords);
          area=great_circle_area(num_poly_nodes, poly_coords);
        }
      } else if (pdim==3) {
        if (sdim==3) {
          Phedra tmp_phedra=create_phedra_from_elem(&elem, cfield);
          area=tmp_phedra.calc_volume();
        } else {
          Throw() << "Meshes with parametric dimension == 3, but spatial dim != 3 not supported for computing areas";
        }
      } else {
        Throw() << "Meshes with parametric dimension != 2 or 3 not supported for computing areas";
      }


       // Put it into the Array
      gci->setArrayData(&array, area);
   }

   // delete Grid Iters
   delete gci;

  }

#undef  ESMC_METHOD

 /* XMRKX */


#endif

  // Convert Grid To PointList
  void GridToPointList(Grid &grid, ESMC_StaggerLoc staggerLoc, ESMCI::InterArray<int> *maskValuesArg, ESMCI::PointList **_pl, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "GridToPointList()"
    Trace __trace("GridToPointList()");

    int localrc;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,rc))
      throw localrc;  // bail out with exception

    // Loop nodes of the grid.  Here we loop all nodes, both owned and not.
    ESMCI::GridIter *gni=new ESMCI::GridIter(&grid,staggerLoc,true);

    if (grid.hasItemStaggerLoc(staggerLoc,ESMC_GRIDITEM_MASK)) {    //masking
      int numMaskValues;
      int *ptrMaskValues;

      if (present(maskValuesArg)) {
        numMaskValues=maskValuesArg->extent[0];
        ptrMaskValues=&(maskValuesArg->array[0]);
      } else {
        numMaskValues=0;
        ptrMaskValues = NULL;
      }


      // Count all local pnts in the Grid
      int num_local_pts=0;
      for(gni->toBeg(); !gni->isDone(); gni->adv()) {
        if(!gni->isLocal()) continue;

        // skip if masked
        ESMC_I4 gm;
        gni->getItem(ESMC_GRIDITEM_MASK,&gm);

        bool mask=false;
        for (int i=0; i<numMaskValues; i++) {
          int mvi=ptrMaskValues[i];
          if (gm == mvi) {
            mask=true;
            break;
          }
        }
        if (!mask)
          num_local_pts++;
        
      }

      // Create PointList
      // (Put Cartesian coordinates in list)
      ESMCI::PointList *pl=new PointList(num_local_pts, grid.getCartCoordDimCount());

      // loop through all nodes in the Grid
      for(gni->toBeg(); !gni->isDone(); gni->adv()) {
        if(!gni->isLocal()) continue;

        // skip if masked
        ESMC_I4 gm;
        gni->getItem(ESMC_GRIDITEM_MASK,&gm);

        bool mask=false;
        for (int i=0; i<numMaskValues; i++) {
          int mvi=ptrMaskValues[i];
          if (gm == mvi) {
            mask=true;
            break;
          }
        }
        if (!mask) {
          // get the global id of this Grid node
          int gid=gni->getGlobalID();

          // get cartesian coordinates
          double cart_coord[ESMF_MAXDIM];
          gni->getCartCoord(cart_coord);

          // Add Point
          pl->add(gid,cart_coord);
        }
      }

      // Output point list
      *_pl=pl;

    } else {                                   //no masking
      // Count all local pnts in the Grid
      int num_local_pts=0;
      for(gni->toBeg(); !gni->isDone(); gni->adv()) {
        if(!gni->isLocal()) continue;

        num_local_pts++;
      }

      // Create PointList
      // (Put Cartesian coordinates in list)
      ESMCI::PointList *pl=new PointList(num_local_pts, grid.getCartCoordDimCount());

      // loop through all nodes in the Grid
      for(gni->toBeg(); !gni->isDone(); gni->adv()) {
        if(!gni->isLocal()) continue;

        // get the global id of this Grid node
        int gid=gni->getGlobalID();

        // get cartesian coordinates
        double cart_coord[ESMF_MAXDIM];
        gni->getCartCoord(cart_coord);

        // Add Point
        pl->add(gid,cart_coord);
      }
      // Output point list
      *_pl=pl;
    }

    // delete Grid Iters
    delete gni;

    if (rc!=NULL) *rc=ESMF_SUCCESS;

  }

#undef  ESMC_METHOD

} // namespace

