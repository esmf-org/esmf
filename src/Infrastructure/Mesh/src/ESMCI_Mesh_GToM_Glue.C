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

#include "ESMCI_Mesh_GToM_Glue.h"

#include "ESMCI_Grid.h"
#include "ESMCI_VM.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Ptypes.h"
#include "Mesh/include/ESMCI_Mesh.h"
#include "Mesh/include/ESMCI_MeshRegrid.h"
#include "Mesh/include/ESMCI_IOField.h"
#include "Mesh/include/ESMCI_ParEnv.h"
#include "Mesh/include/ESMCI_DDir.h"
#include "Mesh/include/ESMCI_MathUtil.h"
#include "Mesh/include/ESMCI_Phedra.h"

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

  typedef struct {
    int node_gids[4];
  } MM_ELEM;

  void _get_missing_multitile_elems(Grid *grid, int staggerLoc, std::vector<MM_ELEM> *missing_elems);
  void _add_missing_multitile_elems(Mesh *mesh, std::vector<MM_ELEM> *missing_elems, int me, int max_local_elem_gid, int local_elem_num);
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

void ESMCI_GridToMesh(const Grid &grid_, int staggerLoc,
                      const std::vector<ESMCI::Array*> &arrays,
                      ESMCI::InterArray<int> *maskValuesArg,
                      int *regridConserve, Mesh **out_meshpp, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "GridToMesh()"
  Trace __trace("GridToMesh(const Grid &grid_, int staggerLoc, ESMCI::Mesh &mesh)");

  try {
     // local error code
 int localrc;

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


 // Create Mesh
 Mesh *meshp = new Mesh();

 // Set output mesh
 *out_meshpp=meshp;

 // Make reference
 Mesh &mesh = *meshp;

 // *** Set some meta-data ***
 // We set the topological dimension of the mesh (quad = 2, hex = 3, etc...)
 UInt pdim = grid.getDimCount();
 mesh.set_parametric_dimension(pdim);

 // In what dimension is the grid embedded?? (sphere = 3, simple rectangle = 2, etc...)
 UInt sdim = grid.getCartCoordDimCount();
 // if ((sdim<3)&&is_sphere) Throw()<<"Sphere's not supported with less than 3 dimesnions";
 mesh.set_spatial_dimension(sdim);


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
   int max_elem_gid=-1;
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

     // Get global id
     int elem_gid=gci->getGlobalID();

     // Create Cell
     MeshObj *cell = new MeshObj(MeshObj::ELEMENT,    // Mesh equivalent of Cell
                                 elem_gid,            // unique global id
                                 local_elem_num++
                                 );

     // Calc max cell gid
     if (elem_gid > max_elem_gid) max_elem_gid=elem_gid;

#ifdef G2M_DBG
     Par::Out() << "Cell:" << cell->get_id() << " uses nodes:";
#endif

     // Set Owner
     cell->set_owner(me);

     UInt block_id = 1;  // Any reason to use different sets for cells?

     mesh.add_element(cell, nodes, block_id, ctopo);

   } // ci

   // Fix problem with multi-tile on center stagger //
   if ((grid.getTileCount() > 1) && (staggerLoc == 0))  {
     std::vector<MM_ELEM> missing_elems;
     _get_missing_multitile_elems(&grid, staggerLoc, &missing_elems);
     _add_missing_multitile_elems(&mesh, &missing_elems, me, max_elem_gid, local_elem_num);
   }

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


  } catch(std::exception &x) {
    // catch Mesh exception return code
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                          x.what(), ESMC_CONTEXT,rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                          "UNKNOWN", ESMC_CONTEXT,rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                  "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }



  // SET OUTPUT MESH BY WHERE ALLOCATED
  // Set output mesh
  // *out_meshpp=&mesh;

  // Set successful return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}

#define NUM_QUAD_CORNERS 4

  void _get_which_tile_edges_lDE_is_on(DistGrid *staggerDistgrid, int localDE, int *lower, int *upper) {
    int localrc;

    // Get de
    const int *localDEList= staggerDistgrid->getDELayout()->getLocalDeToDeMap();
    int de=localDEList[localDE];

    // Get DE bound information
    const int *deMin=staggerDistgrid->getMinIndexPDimPDe(de, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception

    const int *deMax=staggerDistgrid->getMaxIndexPDimPDe(de, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception


    // Get tile
    const int *DETileList = staggerDistgrid->getTileListPDe();
    int tile=DETileList[de];

    // Get tile bound information
    const int *tileMin=staggerDistgrid->getMinIndexPDimPTile(tile, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception

    const int *tileMax=staggerDistgrid->getMaxIndexPDimPTile(tile, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception


    // Get Grid dimCount
    int dimCount=staggerDistgrid->getDimCount();


    // Figure out where this lDE is on the tile
    for (int i=0; i<dimCount; i++) {

      // Init to 0
      lower[i]=0;
      upper[i]=0;

      // Figure out location
      if (deMin[i] == tileMin[i]) lower[i]=1;
      if (deMax[i] == tileMax[i]) upper[i]=1;
    }

    //printf("DE=%d tile=%d tileMin=%d %d lbnd=%d %d \n",localDE, tile,tileMin[0],tileMin[1],deMin[0],deMin[1]);
    //printf("DE=%d tile=%d tileMax=%d %d ubnd=%d %d \n",localDE, tile,tileMax[0],tileMax[1],deMax[0],deMax[1]);

  }

 /* XMRKX */
  void _where_is_seqind_on_tile(int seq_ind, DistGrid *staggerDistgrid, int *lower, int *upper) {

    // Get where the seq index lies on its tile
    int tile=0;
    std::vector<int> indexTuple(2,-1);
    int localrc=staggerDistgrid->getIndexTupleFromSeqIndex(seq_ind, indexTuple, tile);
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception

    // Get tile bound information
    const int *tileMin=staggerDistgrid->getMinIndexPDimPTile(tile, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception

    const int *tileMax=staggerDistgrid->getMaxIndexPDimPTile(tile, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception

    // Get dimCount
    int dimCount=staggerDistgrid->getDimCount();

    // Loop figuring out where the point lies on the tile
    for (int i=0; i<dimCount; i++) {
      // init
      lower[i]=0;
      upper[i]=0;

      // check min
      if (indexTuple[i] == tileMin[i]) lower[i]=1;

      // check max
      if (indexTuple[i] == tileMax[i]) upper[i]=1;
    }

    // printf("seq_ind=%d tile=%d index=%d %d tileMin=%d %d tileMax=%d %d\n",seq_ind,tile,indexTuple[0],indexTuple[1],tileMin[0],tileMin[1],tileMax[0],tileMax[1]);
    // printf("sq=%d lower=%d %d upper=%d %d\n",seq_ind,lower[0],lower[1],upper[0],upper[1]);

  }


 /* XMRKX */
  void _maybe_add_cell(int corners[NUM_QUAD_CORNERS][2], int mine[NUM_QUAD_CORNERS], int localDE, DistGrid *staggerDistgrid,
                       int *protrude_lower, int *protrude_upper, std::vector<MM_ELEM> *missing_elems) {

    // Compute sequence indices of corners
    int seq_ind[NUM_QUAD_CORNERS];
    for (int i=0; i<NUM_QUAD_CORNERS; i++) {
      int localrc;
      std::vector<int> seqIndex;
      localrc=staggerDistgrid->getSequenceIndexLocalDe(localDE,
        corners[i],seqIndex);
      if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
        throw localrc;  // bail out with exception

      if (seqIndex.size() > 0)
        seq_ind[i] = seqIndex[0];
      else
        seq_ind[i] = -1; // invalidate

      //  printf("tile=%d localDE=%d tmp=%d %d seqInd=%d \n",tile, localDE, tmp[0],tmp[1],seq_ind[i]);

    }

    // If points are unmapped then leave
    for (int i=0; i<NUM_QUAD_CORNERS; i++) {
      if (seq_ind[i] < 0) return;
    }

    // See if we should own it
    bool owned=false;
    int max_gid=-1;
    for (int i=0; i<NUM_QUAD_CORNERS; i++) {
      if (seq_ind[i] > max_gid) max_gid=seq_ind[i];
    }

    for (int i=0; i<NUM_QUAD_CORNERS; i++) {
      if ((seq_ind[i] == max_gid) && mine[i]) {
        owned=true;
        break;
      }
    }

    // If we don't own it then leave
    if (!owned) return;


    // Get dimCount
    int dimCount=staggerDistgrid->getDimCount();

    // See if someone else should have created it
    for (int i=0; i<NUM_QUAD_CORNERS; i++) {
      int lower[ESMF_MAXDIM];
      int upper[ESMF_MAXDIM];
      _where_is_seqind_on_tile(seq_ind[i], staggerDistgrid, lower, upper);

      // Check to see if it matches where protruding cells are created on the tiles
      bool match=true;
      for (int d=0; d<dimCount; d++) {
        if (!((lower[d] && protrude_lower[d]) ||
              (upper[d] && protrude_upper[d]))) {
          match=false;
          break;
        }
      }

      //printf("seq_ind=%d match=%d upper= %d %d\n",seq_ind[i],match, upper[0],upper[1]);

      // If it matches then leave, because someone else would have made it
      if (match) return;
    }


    // Create missing elem struct
    MM_ELEM tmp;
    for (int i=0; i<NUM_QUAD_CORNERS; i++) {
      tmp.node_gids[i]=seq_ind[i];
    }

    // Add to list
    missing_elems->push_back(tmp);
  }

  void _get_tile_sides_with_protruding_cells(Grid *grid, int staggerLoc, int *lower, int *upper) {

    // Get Grid dimCount
    int dimCount=grid->getDimCount();

    // Get Alignment for staggerloc
    const int *staggerAlign= grid->getStaggerAlign(staggerLoc);

    // loop deciding on protruding
    for (int i=0; i<dimCount; i++) {

      // Init to no protruding
      upper[i]=0;
      lower[i]=0;

      // if aligned with bottom or center of cell then protrudes upward, otherwise downward
      // (This is based on what happens in GridCellIter, so if that changes, then so should this)
      if (staggerAlign[i] < 1) {
        upper[i]=1;
      } else {
        lower[i]=1;
      }
    }
  }

  // Get multi-tile cells missing because they are on a side where the cells aren't iterated over by
  // GridCellIter, but where there is a connection.
  // + Only works for 2D right now
  // + TODO: make sure that this handles gaps in other kinds of multi-tile cases besides cubed-sphere
  void _get_missing_multitile_elems(Grid *grid, int staggerLoc, std::vector<MM_ELEM> *missing_elems) {

    // Get distgrid for this staggerloc
    DistGrid *staggerDistgrid;
    grid->getStaggerDistgrid(staggerLoc, &staggerDistgrid);

    // Get Grid dimCount
    int dimCount=grid->getDimCount();

    // Only supporting 2D right now
    if (dimCount != 2) return;

    // Get localDECount
    int localDECount=staggerDistgrid->getDELayout()->getLocalDeCount();

    // Get how the cells stick out of tiles
    int protrude_lower[ESMF_MAXDIM];
    int protrude_upper[ESMF_MAXDIM];
    _get_tile_sides_with_protruding_cells(grid, staggerLoc, protrude_lower, protrude_upper);

    // Loop over DEs
    for (int lDE=0; lDE < localDECount; lDE++) {
      // Get DE bounds
      int ubnd[ESMF_MAXDIM];
      int lbnd[ESMF_MAXDIM];
      grid->getDistExclusiveUBound(staggerDistgrid, lDE, ubnd);
      grid->getDistExclusiveLBound(staggerDistgrid, lDE, lbnd);

      // Modify to be DE-based (so DE lower bound is always 0).
      // (this is needed by the seqence index calculating func in _maybe_add_cell)
      for (int i=0; i<dimCount; i++) {
        ubnd[i]=ubnd[i]-lbnd[i];
        lbnd[i]=0;
      }

      // See which edges lDE is on
      int lower[ESMF_MAXDIM];
      int upper[ESMF_MAXDIM];
      _get_which_tile_edges_lDE_is_on(staggerDistgrid, lDE, lower, upper);

      // Based on which edges add additional elements
      int corners[NUM_QUAD_CORNERS][2];
      int mine[NUM_QUAD_CORNERS];
      if (lower[0] && lower[1]) {
        //printf("ll\n");

        // set corner indices
        corners[0][0]=lbnd[0];   corners[0][1]=lbnd[1];   mine[0]=1;
        corners[1][0]=lbnd[0]-1; corners[1][1]=lbnd[1];   mine[1]=0;
        corners[2][0]=lbnd[0]-1; corners[2][1]=lbnd[1]-1; mine[2]=0;
        corners[3][0]=lbnd[0];   corners[3][1]=lbnd[1]-1; mine[3]=0;

        // Add cell
        _maybe_add_cell(corners, mine, lDE, staggerDistgrid,
                        protrude_lower, protrude_upper, missing_elems);
      }

      if (lower[0] && upper[1]) {
        //printf("lu\n");
        // set corner indices
        corners[0][0]=lbnd[0];   corners[0][1]=ubnd[1];   mine[0]=1;
        corners[1][0]=lbnd[0];   corners[1][1]=ubnd[1]+1; mine[1]=0;
        corners[2][0]=lbnd[0]-1; corners[2][1]=ubnd[1]+1; mine[2]=0;
        corners[3][0]=lbnd[0]-1; corners[3][1]=ubnd[1];   mine[3]=0;

        // Add cell
        _maybe_add_cell(corners, mine, lDE, staggerDistgrid,
                        protrude_lower, protrude_upper, missing_elems);
      }

      if (upper[0] && lower[1]) {
        //printf("ul\n");
        // set corner indices
        corners[0][0]=ubnd[0];   corners[0][1]=lbnd[1];   mine[0]=1;
        corners[1][0]=ubnd[0];   corners[1][1]=lbnd[1]-1; mine[1]=0;
        corners[2][0]=ubnd[0]+1; corners[2][1]=lbnd[1]-1; mine[2]=0;
        corners[3][0]=ubnd[0]+1; corners[3][1]=lbnd[1];   mine[3]=0;

        // Add cell
        _maybe_add_cell(corners, mine, lDE, staggerDistgrid,
                        protrude_lower, protrude_upper, missing_elems);
      }

      if (upper[0] && upper[1]) {
        //printf("uu\n");
        // set corner indices
        corners[0][0]=ubnd[0];   corners[0][1]=ubnd[1];   mine[0]=1;
        corners[1][0]=ubnd[0]+1; corners[1][1]=ubnd[1];   mine[1]=0;
        corners[2][0]=ubnd[0]+1; corners[2][1]=ubnd[1]+1; mine[2]=0;
        corners[3][0]=ubnd[0];   corners[3][1]=ubnd[1]+1; mine[3]=0;

        // Add cell
        _maybe_add_cell(corners, mine, lDE, staggerDistgrid,
                        protrude_lower, protrude_upper, missing_elems);
      }

    } // lDE
  }

  void _MM_ELEM_to_nodes(Mesh *mesh, MM_ELEM *mme, std::vector<MeshObj*> *nodes) {

#if 0
    // Don't unique nodes right now, because they aren't for the other corner triangles.
    // It's probably good that all the corners match.
    // If you ever do it for the others above, then do it here also
    // (will also have to change how topo's are generated below).

    // Get unique gids
    int num_unique_gids=0;
    int unique_gids[NUM_QUAD_CORNERS];
    for (int i=0; i<NUM_QUAD_CORNERS; i++) {

   // See if it's already in the list
      bool is_unique=true;
      for (int j=0; j<num_unique_gids; j++) {
        if (mme->node_gids[i]==unique_gids[j]) {
          is_unique=false;
          break;
        }
      }

      // Add it to the list, if it's unique
      if (is_unique) {
        unique_gids[num_unique_gids]=mme->node_gids[i];
        num_unique_gids++;
      }
    }

    // Map gids to nodes
    nodes->clear();
    for (int i=0; i<num_unique_gids; i++) {
      // find node
      Mesh::MeshObjIDMap::iterator mi =  mesh->map_find(MeshObj::NODE, unique_gids[i]);
      if (mi != mesh->map_end(MeshObj::NODE)) {
        nodes->push_back(&(*mi));
      } else {
        Throw() << "node with that gid not found in local mesh";
      }
    }
#endif

    // Map gids to nodes
    nodes->clear();
    for (int i=0; i<NUM_QUAD_CORNERS; i++) {
      // find node
      Mesh::MeshObjIDMap::iterator mi =  mesh->map_find(MeshObj::NODE, mme->node_gids[i]);
      if (mi != mesh->map_end(MeshObj::NODE)) {
        nodes->push_back(&(*mi));
      } else {
        Throw() << "node with that gid not found in local mesh";
      }
    }
  }

  // Add the missing elems to the mesh
  void _add_missing_multitile_elems(Mesh *mesh, std::vector<MM_ELEM> *missing_elems, int me, int max_local_elem_gid, int local_elem_num) {

    //// Calc gid range for missing elements ////

    // Calc global max id
    int max_global_elem_gid=0;
    MPI_Allreduce(&max_local_elem_gid,&max_global_elem_gid,1,MPI_INT,MPI_MAX,Par::Comm());

    // Calculate start of our extra elem gids
    int num_extra_elems=missing_elems->size();
    int start_extra_elem_gids=0;
    MPI_Scan(&num_extra_elems,&start_extra_elem_gids,1,MPI_INT,MPI_SUM,Par::Comm());

    // Remove this processor's number from the sum to get the beginning
    start_extra_elem_gids=start_extra_elem_gids-num_extra_elems;

    // Start 1 up from max
    start_extra_elem_gids=start_extra_elem_gids+max_global_elem_gid+1;


    //// Create missing elements ////

    // Get topo
    const MeshObjTopo *ctopo = 0;
    if (mesh->spatial_dim() == 2) {
      ctopo = GetTopo("QUAD");
    } else if (mesh->spatial_dim() == 3) {
      ctopo = GetTopo("QUAD_3D");
    } else {
      Throw() << " unsupported spatial dimension";
    }

    // Create vector for nodes and reserve space
    std::vector<MeshObj*> nodes;
    nodes.reserve(NUM_QUAD_CORNERS);

    // Loop through missing elems
    for (int i=0; i < missing_elems->size(); i++) {
        // Get struct
        MM_ELEM mme=(*missing_elems)[i];

        //  printf("%d elem_gid=%d gids=%d %d %d %d\n",i,start_extra_elem_gids+i,mme.node_gids[0],mme.node_gids[1],mme.node_gids[2],mme.node_gids[3]);

        // get nodes
        _MM_ELEM_to_nodes(mesh, &mme, &nodes);

        // new element gid
        UInt elem_gid=start_extra_elem_gids+i;

        // Create new element
        MeshObj *new_elem = new MeshObj(MeshObj::ELEMENT,    // Mesh equivalent of Cell
                                    elem_gid,            // unique global id
                                    local_elem_num++
                                    );
        // Set Owner
        new_elem->set_owner(me);

        // Add the new element
        UInt block_id = 1;  // Any reason to use different sets for elements?
        mesh->add_element(new_elem, nodes, block_id, ctopo);
      }
    }
#undef  ESMC_METHOD

  //================== GTOMCELL ========================

#undef  ESMC_METHOD
#define ESMC_METHOD "GridToMeshCell()"
 /* XMRKX */


  // Get global id
  // if there is one returns true, if there isn't returns false
#define BAD_ID -1

  static bool _get_global_id(DistGrid *distgrid, int localDE, int *index,
                             int *_gid, bool *_is_local) {

    // determine sequence index
    std::vector<int> seqIndex;
    int localrc=distgrid->getSequenceIndexLocalDe(localDE,index,seqIndex);
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception

    // If there's just 1, then use that as the id, otherwise use the min id
    int gid=BAD_ID;
    bool is_local=false;
    if (seqIndex.size()==1) {
      gid = seqIndex[0];
      is_local=true;
    } else if (seqIndex.size() > 0) {
      // If there's a degenarcy then chose the min id
      gid= std::numeric_limits<int>::max();
      for (int i=0; i<seqIndex.size(); i++) {
        if (seqIndex[i]<gid) gid=seqIndex[i];
      }
      if (gid==seqIndex[0]) is_local=true;
    } else {
      // if it's 0 then return false
      return false;
    }

    // Do output
    *_gid=gid;
    *_is_local=is_local;
    return true;
  }


  //// DON'T NEED NODE FIELDS RIGHT NOW, SO SAVE UNTIL YOU HAVE MORE TIME ///
#if 0
#define GTOM_NFIELD_MASK 0
#define GTOM_NFIELD_MASK_VAL 1
#define GTOM_NFIELD_ORIG_COORD 2
#define GTOM_NFIELD_NUM 3

  static void create_nfields(Grid *grid, Mesh *mesh,
                             IOField<NodalField> *nfields[GTOM_NFIELD_NUM]) {

    // Init field array to null
    for (int i=0; i<GTOM_NFIELD_NUM; i++) {
      nfields[i]=NULL;
    }

    // Masks
   if (grid->hasItemStaggerLoc(ESMCI_STAGGERLOC_CORNER, ESMC_GRIDITEM_MASK)) {
     nfields[GTOM_NFIELD_MASK] = mesh->RegisterNodalField(*mesh, "mask", 1);
     nfields[GTOM_NFIELD_MASK_VAL] = mesh->RegisterNodalField(*mesh, "node_mask_val", 1);
   }

   // Original coords
   // How do I tell if I need these? Maybe leave them for now

  }
#endif


  /// Element fields
#define GTOM_EFIELD_MASK 0
#define GTOM_EFIELD_MASK_VAL 1
#define GTOM_EFIELD_AREA     2
#define GTOM_EFIELD_COORD 3
#define GTOM_EFIELD_ORIG_COORD 4
#define GTOM_EFIELD_FRAC 5
#define GTOM_EFIELD_FRAC2 6
#define GTOM_EFIELD_NUM 7

  // Create element fields on mesh
  static void create_efields(Grid *grid, Mesh *mesh,
                             MEField<> *efields[GTOM_EFIELD_NUM]) {

    // Init field array to null
    for (int i=0; i<GTOM_EFIELD_NUM; i++) {
      efields[i]=NULL;
    }

    // Set context
    Context ctxt; ctxt.flip();


    // Mask
    if (grid->hasItemStaggerLoc(ESMCI_STAGGERLOC_CENTER, ESMC_GRIDITEM_MASK)) {
      efields[GTOM_EFIELD_MASK] = mesh->RegisterField("elem_mask",
                          MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);

      efields[GTOM_EFIELD_MASK_VAL] = mesh->RegisterField("elem_mask_val",
                          MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);
    }

    // Area
    if (grid->hasItemStaggerLoc(ESMCI_STAGGERLOC_CENTER, ESMC_GRIDITEM_AREA)) {
      efields[GTOM_EFIELD_AREA] = mesh->RegisterField("elem_area",
                          MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);
    }


    // DO LATER
#if 0
    // COORDS
    if (grid->hasCoordStaggerLoc(ESMCI_STAGGERLOC_CENTER)) {

    }
#endif

    // Fracs are always there
    efields[GTOM_EFIELD_FRAC] = mesh->RegisterField("elem_frac",
                                             MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);

    efields[GTOM_EFIELD_FRAC2] = mesh->RegisterField("elem_frac2",
                                             MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);
  }

  // Set element fields in mesh
  static void set_efields(Grid *grid, Mesh *mesh,
                          MEField<> *efields[GTOM_EFIELD_NUM]) {
    int localrc;

    // Get distgrid for the center staggerloc
    DistGrid *centerDistgrid;
    grid->getStaggerDistgrid(ESMCI_STAGGERLOC_CENTER, &centerDistgrid);

    // Get localDECount
    int localDECount=centerDistgrid->getDELayout()->getLocalDeCount();

    // Loop setting fields on each element
    for (int lDE=0; lDE < localDECount; lDE++) {

      // Get Center DE bounds
      int ubnd[ESMF_MAXDIM];
      int lbnd[ESMF_MAXDIM];
      grid->getDistExclusiveUBound(centerDistgrid, lDE, ubnd);
      grid->getDistExclusiveLBound(centerDistgrid, lDE, lbnd);

      // Loop over bounds
      int index[2];
      int de_index[2];
      for (int i0=lbnd[0]; i0<=ubnd[0]; i0++){
        for (int i1=lbnd[1]; i1<=ubnd[1]; i1++){

          // Set index
          index[0]=i0;
          index[1]=i1;

          // De based index
          de_index[0]=i0-lbnd[0];
          de_index[1]=i1-lbnd[1];

          // Get elem global id
          int elem_gid;
          bool is_local;
          if (!_get_global_id(centerDistgrid, lDE, de_index,
                              &elem_gid, &is_local)) {
            continue; // If we can't find a global id, then just skip
          }

          // Only set elem information if this is the owner
          if (!is_local) continue;

          // Get associated elem
          Mesh::MeshObjIDMap::iterator mi =  mesh->map_find(MeshObj::ELEMENT, elem_gid);

          // If it doesn't exist then go to next
          if (mi == mesh->map_end(MeshObj::ELEMENT)) {
            continue;
          }

          // Get elem
          MeshObj &elem=*mi;


          // Mask
          // Init to 0 (set later in ESMF_FieldRegridStore()
          if (efields[GTOM_EFIELD_MASK]) {
            double *d=efields[GTOM_EFIELD_MASK]->data(elem);
            *d=0.0;
          }

          // Mask Val
          // Get data from grid
          if (efields[GTOM_EFIELD_MASK_VAL]) {
            double *d=efields[GTOM_EFIELD_MASK_VAL]->data(elem);
            localrc=grid->getItemInternalConvert(ESMCI_STAGGERLOC_CENTER,
                                                 ESMC_GRIDITEM_MASK,
                                                 lDE, index, d);
            if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
              throw localrc;  // bail out with exception
          }


          // Area
          // Get data from grid
          if (efields[GTOM_EFIELD_AREA]) {
            double *d=efields[GTOM_EFIELD_AREA]->data(elem);
            localrc=grid->getItemInternalConvert(ESMCI_STAGGERLOC_CENTER,
                                                 ESMC_GRIDITEM_AREA,
                                                 lDE, index, d);
            if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
              throw localrc;  // bail out with exception
          }

#if 0
          // COORDS
          if (grid->hasCoordStaggerLoc(ESMCI_STAGGERLOC_CENTER)) {

          }
#endif

          // Init fracs
          if (efields[GTOM_EFIELD_FRAC]) {
            double *d=efields[GTOM_EFIELD_FRAC]->data(elem);
            *d=1.0;
          }

          if (efields[GTOM_EFIELD_FRAC2]) {
            double *d=efields[GTOM_EFIELD_FRAC2]->data(elem);
            *d=1.0;
          }
        }

      }
    }
  }



  static void _calc_corner_offset(Grid *grid, int corner_offset[NUM_QUAD_CORNERS][2]) {
    int default_offset[NUM_QUAD_CORNERS][2]={{0,0},{1,0},{1,1},{0,1}};

    // Get Alignment for staggerloc
    const int *staggerAlign= grid->getStaggerAlign(ESMCI_STAGGERLOC_CORNER);

    // Get off set due to alignment
    int align_off[2];
    for (int i=0; i<2; i++) {
      if (staggerAlign[i] < 1) align_off[i]=0;
      else align_off[i]=-1;
    }

    // Change default offset to correspond with alignment
    for (int i=0; i<NUM_QUAD_CORNERS; i++) {
      for (int j=0; j<2; j++) {
        corner_offset[i][j]=default_offset[i][j]+align_off[j];
      }
    }
  }

#define BAD_PROC (std::numeric_limits<UInt>::max())

  static void _get_quad_corner_nodes(Mesh *mesh, DistGrid *cnrDistgrid, int localDE, int index[2], int cnr_offset[NUM_QUAD_CORNERS][2],
                                     int *local_node_index, std::vector<MeshObj*> *cnr_nodes, bool *_all_nodes_ok) {
  // Init output
  *_all_nodes_ok=true;


  // Loop getting global ids
  int cnr_gids[NUM_QUAD_CORNERS];
  for (int i=0; i<NUM_QUAD_CORNERS; i++) {

    // calc index of corner
    int cnr_index[2];
    cnr_index[0]=index[0]+cnr_offset[i][0];
    cnr_index[1]=index[1]+cnr_offset[i][1];

    // Get global id, if we can't then leave
    bool is_local;
    if (!_get_global_id(cnrDistgrid, localDE, cnr_index,
                        cnr_gids+i, &is_local)) {
      *_all_nodes_ok=false;
      return;
    }
  }

  // Convert gids to nodes
  for (int i=0; i<NUM_QUAD_CORNERS; i++) {

    // get gid of this corner
    int gid=cnr_gids[i];

    // declare node that we're looking for
    MeshObj *node;

    // If a node with this gid already exists, then put into list
    Mesh::MeshObjIDMap::iterator mi =  mesh->map_find(MeshObj::NODE, gid);
    if (mi != mesh->map_end(MeshObj::NODE)) {

      // Get node pointer
      node=&*mi;

      // Put into list
      (*cnr_nodes)[i]=node;

      // Go to next iteration
      continue;
    }

    // If we didn't find it then we need to make a new one

    // Create new node in mesh object
    node = new MeshObj(MeshObj::NODE,     // node...
                                   gid,               // unique global id
                                   *local_node_index
                                   );
    // Advance to next node index
    (*local_node_index)++;

    // Set owner to bad proc value and change later
    node->set_owner(BAD_PROC);

    // Eventually need to figure out pole id stuff
    UInt nodeset=0; // UInt nodeset = gni->getPoleID();   // Do we need to partition the nodes in any sets?
    mesh->add_node(node, nodeset);


    // Put into list
    (*cnr_nodes)[i]=node;
  }
}

  void _gid_to_proc(int gid, DistGrid *distgrid, int *_proc) {

    // Init to bad value
    int proc=BAD_PROC;

    // Calc tile and index from gid
    int tile=0;
    std::vector<int> index(2,-1);
    int localrc=distgrid->getIndexTupleFromSeqIndex(gid, index, tile);
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception

    // Get DeLayout
    DELayout *delayout=distgrid->getDELayout();

    // Get number of DE's
   int deCount=delayout->getDeCount();

   // Get de Index lists
   int const *minIndexPDimPDE=distgrid->getMinIndexPDimPDe();
   int const *maxIndexPDimPDE=distgrid->getMaxIndexPDimPDe();


    // Get tile
    const int *DETileList = distgrid->getTileListPDe();

    // Loop through DEs
    for (int de=0; de<deCount; de++) {

      // If de isn't on the correct tile, then go on to next
      if (tile != DETileList[de]) continue;

      // Get De minIndex
      int const *minIndex=minIndexPDimPDE+2*de;

      // check if we're smaller than min
      if ((index[0] < minIndex[0]) ||
          (index[1] < minIndex[1])) continue;


      // Get De maxIndex
      int const *maxIndex=maxIndexPDimPDE+2*de;

      // check if we're bigger than max
      if ((index[0] > maxIndex[0]) ||
          (index[1] > maxIndex[1])) continue;

      // This point is on this de, so tranlate to proc and exit
      proc=delayout->getPet(de);
      break;
    }

    //    if (proc == BAD_PROC) {
    //  printf("gid=%d index=%d %d tile=%d proc=%d\n",gid,index[0],index[1],tile,proc);
    //}

    // Do output
    *_proc=proc;
  }

 /* XMRKX */

void ESMCI_GridToMeshCell(const Grid &grid_,
                          const std::vector<ESMCI::Array*> &arrays,
                          Mesh **out_meshpp, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "GridToMeshCell()"
  Trace __trace("GridToMeshCell(const Grid &grid_, ESMCI::Mesh &mesh)");

  try {
  // local error code
  int localrc;

  // Initialize the parallel environment for mesh (if not already done)
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
   throw localrc;  // bail out with exception

 // Get grid pointer
 Grid *grid = &(const_cast<Grid&>(grid_));

 // The Grid needs to have corner coordinates
 if (!grid->hasCoordStaggerLoc(ESMCI_STAGGERLOC_CORNER)) {
   ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
            "- Grid being used in Regrid call does not contain coordinates at corner staggerloc ", ESMC_CONTEXT, &localrc);
   throw localrc;
 }

 // Create Mesh
 Mesh *mesh = new Mesh();

 // Set output mesh
 *out_meshpp=mesh;

 // Get dimCount
 int dimCount=grid->getDimCount();

 // Only supporting 2D right now
 if (dimCount != 2) {
   Throw() << "Currently only supports 2D Grids";
 }

 // *** Set some meta-data ***
 // We set the topological dimension of the mesh (quad = 2, hex = 3, etc...)
 int pdim=dimCount;
 mesh->set_parametric_dimension(pdim);

 // In what dimension is the grid embedded?? (sphere = 3, simple rectangle = 2, etc...)
 UInt sdim = grid->getCartCoordDimCount();
 mesh->set_spatial_dimension(sdim);

 // Get distgrid for the center staggerloc
 DistGrid *centerDistgrid;
 grid->getStaggerDistgrid(ESMCI_STAGGERLOC_CENTER, &centerDistgrid);

 // Get distgrid for the corner staggerloc
 DistGrid *cnrDistgrid;
 grid->getStaggerDistgrid(ESMCI_STAGGERLOC_CORNER, &cnrDistgrid);

 // Setup topology
 const MeshObjTopo *elem_topo = 0;
 elem_topo = sdim == 2 ? GetTopo("QUAD") : GetTopo("QUAD_3D");
 if (!elem_topo) Throw() << "Could not get topology for sdim=" << sdim;


 // Get index offsets for corners
 int cnr_offset[NUM_QUAD_CORNERS][2];
 _calc_corner_offset(grid, cnr_offset);

 // Space for corner nodes
 std::vector<MeshObj*> cnr_nodes(NUM_QUAD_CORNERS);


 // printf("offset=[%d %d] [%d %d]  [%d %d]  [%d %d]\n",cnr_offset[0][0],
 //cnr_offset[0][1],cnr_offset[1][0],cnr_offset[1][1],cnr_offset[2][0],cnr_offset[2][1],
 //cnr_offset[3][0], cnr_offset[3][1]);

 // Get localDECount
 int localDECount=centerDistgrid->getDELayout()->getLocalDeCount();

 // Loop over DEs
 int local_node_index=0;
 int local_elem_index=0;
 for (int lDE=0; lDE < localDECount; lDE++) {

   // Get Center DE bounds
   int ubnd[ESMF_MAXDIM];
   int lbnd[ESMF_MAXDIM];
   grid->getDistExclusiveUBound(centerDistgrid, lDE, ubnd);
   grid->getDistExclusiveLBound(centerDistgrid, lDE, lbnd);

   // Get Corner DE bounds
   int cnr_ubnd[ESMF_MAXDIM];
   int cnr_lbnd[ESMF_MAXDIM];
   grid->getDistExclusiveUBound(cnrDistgrid, lDE, cnr_ubnd);
   grid->getDistExclusiveLBound(cnrDistgrid, lDE, cnr_lbnd);

   //printf("lDE=%d CENTER lbnd=%d %d ubnd=%d %d\n",lDE,lbnd[0],lbnd[1],ubnd[0],ubnd[1]);
   //printf("lDE=%d CORNER lbnd=%d %d ubnd=%d %d\n",lDE,cnr_lbnd[0],cnr_lbnd[1],cnr_ubnd[0],cnr_ubnd[1]);

   // Loop over bounds
   for (int i0=lbnd[0]; i0<=ubnd[0]; i0++){
     for (int i1=lbnd[1]; i1<=ubnd[1]; i1++){

       // Set index
       int index[2];
       index[0]=i0;
       index[1]=i1;

       // De based index
       int de_index[2];
       de_index[0]=i0-lbnd[0];
       de_index[1]=i1-lbnd[1];


       // Get Element corner nodes
       bool all_nodes_ok=true;
       _get_quad_corner_nodes(mesh, cnrDistgrid, lDE, de_index, cnr_offset, &local_node_index,
                              &cnr_nodes, &all_nodes_ok);

       // If we didn't get all the nodes, then go to next element
       if (!all_nodes_ok) continue;

       // Get element id
       int elem_gid;
       bool is_local;
       if (!_get_global_id(centerDistgrid, lDE, de_index,
                           &elem_gid, &is_local)) {
         continue; // If we can't find a global id, then just skip
       }


       // Create Element
       MeshObj *elem = new MeshObj(MeshObj::ELEMENT,    // Mesh equivalent of Cell
                                   elem_gid,            // unique global id
                                   local_elem_index++
                                   );

       // Set owner to the current processor
       elem->set_owner(Par::Rank());

       // Add element
       UInt block_id = 1;  // Any reason to use different sets for cells?
       mesh->add_element(elem, cnr_nodes, block_id, elem_topo);

       // DEBUG
       //if (elem_gid==1) {
       //  printf("gid=%d i=%d %d lDE=%d nodes=%d %d %d %d \n",elem_gid,i0,i1,lDE,cnr_nodes[0]->get_id(),cnr_nodes[1]->get_id(),cnr_nodes[2]->get_id(),cnr_nodes[3]->get_id());
       //}
     }
   }
 }


 // Set up the node coordinate field
 IOField<NodalField> *node_coord = mesh->RegisterNodalField(*mesh, "coordinates", sdim);

 // Loop again adding information to nodes
 for (int lDE=0; lDE < localDECount; lDE++) {

   // Get Corner DE bounds
   int cnr_ubnd[ESMF_MAXDIM];
   int cnr_lbnd[ESMF_MAXDIM];
   grid->getDistExclusiveUBound(cnrDistgrid, lDE, cnr_ubnd);
   grid->getDistExclusiveLBound(cnrDistgrid, lDE, cnr_lbnd);


   // Loop over bounds
   int index[2];
   int nonde_index[2];
   for (int i0=cnr_lbnd[0]; i0<=cnr_ubnd[0]; i0++){
     for (int i1=cnr_lbnd[1]; i1<=cnr_ubnd[1]; i1++){

       // Set index
       index[0]=i0;
       index[1]=i1;

       // De based index
       int de_index[2];
       de_index[0]=i0-cnr_lbnd[0];
       de_index[1]=i1-cnr_lbnd[1];

       // Get node global id
       int node_gid;
       bool is_local;
       if (!_get_global_id(cnrDistgrid, lDE, de_index,
                           &node_gid, &is_local)) {
         continue; // If we can't find a global id, then just skip
       }

       // Only set node information if this is the owner
       if (!is_local) continue;

       // Get associated node
       Mesh::MeshObjIDMap::iterator mi =  mesh->map_find(MeshObj::NODE, node_gid);

       // If it doesn't exist then go to next
       if (mi == mesh->map_end(MeshObj::NODE)) {
         continue;
       }

       // Get node pointer
       MeshObj *node=&*mi;

       // Set owner to the current processor
       node->set_owner(Par::Rank());

       // Put in coordinates
       double *coord = node_coord->data(*node);

       // get orig coordinates
       double orig_coord[ESMF_MAXDIM];
       localrc=grid->getCoordInternalConvert(ESMCI_STAGGERLOC_CORNER, lDE,
                                             index, orig_coord);
       if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                         NULL)) throw localrc;

       // Call into coordsys method to convert to Cart
       localrc=ESMCI_CoordSys_ConvertToCart(grid->getCoordSys(),
                                            grid->getDimCount(),
                                            orig_coord,  // Input coordinates
                                            coord);
       if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                         NULL)) throw localrc;
#if 0
       // DEBUG
       if ((node->get_id()==1) ||
           (node->get_id()==2) ||
           (node->get_id()==13) ||
           (node->get_id()==12)) {
         printf("node gid=%d i=%d %d lDE=%d %20.17f %20.17f %20.17f oc=%20.17f %20.17f\n",node->get_id(),i0,i1,lDE,coord[0],coord[1],coord[2],orig_coord[0],orig_coord[1]);
       }
#endif

     }
   }
 }

 // Loop through Mesh nodes setting the remaining owners
 MeshDB::iterator ni = mesh->node_begin(), ne = mesh->node_end();
 for (; ni != ne; ++ni) {
   MeshObj *node=&*ni;

   // Get node global id
   int gid=node->get_id();

   // get node owner
   int owner=node->get_owner();

   // If the owner is already set then skip
   if (owner != BAD_PROC) continue;

   // Get proc based on gid
   int proc;
   _gid_to_proc(gid, cnrDistgrid, &proc);

   // Set owner
   node->set_owner(proc);
 }

 // Add element Fields
 MEField<> *efields[GTOM_EFIELD_NUM];
 create_efields(grid, mesh, efields);

 // Finalize the Mesh
 mesh->build_sym_comm_rel(MeshObj::NODE);
 mesh->Commit();

 // set element fields
 set_efields(grid, mesh, efields);

 // Halo fields, so entities with an owner on another processor
 // will have the correct value
 {
   std::vector<MEField<>*> fds;

   Mesh::FieldReg::MEField_iterator fi = mesh->Field_begin(), fe = mesh->Field_end();

   for (; fi != fe; ++fi) fds.push_back(&*fi);

   mesh->HaloFields(fds.size(), &fds[0]);
 }


  } catch(std::exception &x) {
    // catch Mesh exception return code
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                          x.what(), ESMC_CONTEXT,rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                          "UNKNOWN", ESMC_CONTEXT,rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                  "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }


// Set successful return code
 if (rc!=NULL) *rc = ESMF_SUCCESS;
}

  // Assumes array is on center staggerloc of grid and was used to create the mesh
  void CpMeshElemDataToArrayCell(Grid *grid, ESMCI::Mesh *mesh, ESMCI::Array *array, MEField<> *dataToArray) {
#undef  ESMC_METHOD
#define ESMC_METHOD "CpMeshElemDataToArrayCell()"
  Trace __trace("CpMeshElemDataToArray()Cell");

  int localrc;
  int rc;

  // Initialize the parallel environment for mesh (if not already done)
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
   throw localrc;  // bail out with exception

 // Get dimCount
 int dimCount=grid->getDimCount();

 // Only supporting 2D right now
 if (dimCount != 2) {
   Throw() << "Currently only supports 2D Grids";
 }

 // Check typekind
 if (array->getTypekind() != ESMC_TYPEKIND_R8) {
   int rc;
   ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
       "- currently only ESMC_TYPEKIND_R8 Arrays supported.", ESMC_CONTEXT, &rc);
   throw rc;
 }



 // Get distgrid for the center staggerloc
 DistGrid *centerDistgrid;
 grid->getStaggerDistgrid(ESMCI_STAGGERLOC_CENTER, &centerDistgrid);

 // Get localDECount
 int localDECount=centerDistgrid->getDELayout()->getLocalDeCount();

 // Loop over DEs
 for (int lDE=0; lDE < localDECount; lDE++) {

   // Get Center DE bounds
   int ubnd[ESMF_MAXDIM];
   int lbnd[ESMF_MAXDIM];
   grid->getDistExclusiveUBound(centerDistgrid, lDE, ubnd);
   grid->getDistExclusiveLBound(centerDistgrid, lDE, lbnd);

   // Get LocalArray cooresponding to localDE
   LocalArray *localArray=array->getLocalarrayList()[lDE];

   // Loop over bounds
   for (int i0=lbnd[0]; i0<=ubnd[0]; i0++){
     for (int i1=lbnd[1]; i1<=ubnd[1]; i1++){

       // Set index
       int index[2];
       index[0]=i0;
       index[1]=i1;

       // De based index
       int de_index[2];
       de_index[0]=i0-lbnd[0];
       de_index[1]=i1-lbnd[1];

       // Get element id
       int elem_gid;
       bool is_local;
       if (!_get_global_id(centerDistgrid, lDE, de_index,
                           &elem_gid, &is_local)) {
         continue; // If we can't find a global id, then just skip
       }

      //  Find the corresponding Mesh element
      Mesh::MeshObjIDMap::iterator mi =  mesh->map_find(MeshObj::ELEMENT, elem_gid);
      if (mi == mesh->map_end(MeshObj::ELEMENT)) {
        Throw() << "Grid entry not in mesh";
      }

      // Get the element
      const MeshObj &elem = *mi;

      // Only put it in if it's locally owned
      if (!GetAttr(elem).is_locally_owned()) continue;

       // Get the data from mesh
      double *data = dataToArray->data(elem);

      //// Put data into the Array
      localArray->setData(index, *data);
     }
   }
 }
}


  void PutElemAreaIntoArrayCell(Grid *grid, ESMCI::Mesh *mesh, ESMCI::Array *array) {
#undef  ESMC_METHOD
#define ESMC_METHOD "PutElemAreaIntoArrayCell()"
    Trace __trace("PutElemAreaIntoArrayCell()");

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


    // Get dimCount
    int dimCount=grid->getDimCount();

    // Only supporting 2D right now
    if (dimCount != 2) {
      Throw() << "Currently only supports 2D Grids";
    }

    // Check typekind
    if (array->getTypekind() != ESMC_TYPEKIND_R8) {
      int rc;
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
            "- currently only ESMC_TYPEKIND_R8 Arrays supported.", ESMC_CONTEXT, &rc);
      throw rc;
    }

    // If an area field exists use that instead
    // TODO: replace this with something that doesn't require building a mesh first
    MEField<> *area_field = mesh->GetField("elem_area");
    if (area_field) {
      CpMeshElemDataToArrayCell(grid, mesh, array, area_field);
      return;
    }


    ////// Otherwise calculate areas.....

    // Get coord field
    MEField<> *cfield = mesh->GetCoordField();

    // Get dimensions
    int sdim=mesh->spatial_dim();
    int pdim=mesh->parametric_dim();

    // Get distgrid for the center staggerloc
    DistGrid *centerDistgrid;
    grid->getStaggerDistgrid(ESMCI_STAGGERLOC_CENTER, &centerDistgrid);

    // Get localDECount
    int localDECount=centerDistgrid->getDELayout()->getLocalDeCount();

    // Loop over DEs
    for (int lDE=0; lDE < localDECount; lDE++) {

      // Get Center DE bounds
      int ubnd[ESMF_MAXDIM];
      int lbnd[ESMF_MAXDIM];
      grid->getDistExclusiveUBound(centerDistgrid, lDE, ubnd);
      grid->getDistExclusiveLBound(centerDistgrid, lDE, lbnd);

      // Get LocalArray cooresponding to localDE
      LocalArray *localArray=array->getLocalarrayList()[lDE];

      // Loop over bounds
      for (int i0=lbnd[0]; i0<=ubnd[0]; i0++){
        for (int i1=lbnd[1]; i1<=ubnd[1]; i1++){

          // Set index
          int index[2];
          index[0]=i0;
          index[1]=i1;

          // De based index
          int de_index[2];
          de_index[0]=i0-lbnd[0];
          de_index[1]=i1-lbnd[1];

          // Get element id
          int elem_gid;
          bool is_local;
          if (!_get_global_id(centerDistgrid, lDE, de_index,
                              &elem_gid, &is_local)) {
            continue; // If we can't find a global id, then just skip
          }

          //  Find the corresponding Mesh element
          Mesh::MeshObjIDMap::iterator mi =  mesh->map_find(MeshObj::ELEMENT, elem_gid);
          if (mi == mesh->map_end(MeshObj::ELEMENT)) {
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
#if 0
              // DEBUG
              if (elem_gid==1) {
                printf("gid=%d num_poly_nodes=%d area=%20.17f\n",elem_gid,num_poly_nodes, area);
                printf("1 %20.17f %20.17f %20.17f\n",poly_coords[0],poly_coords[1],poly_coords[2]);
                printf("2 %20.17f %20.17f %20.17f\n",poly_coords[3],poly_coords[4],poly_coords[5]);
                printf("3 %20.17f %20.17f %20.17f\n",poly_coords[6],poly_coords[7],poly_coords[8]);
                printf("4 %20.17f %20.17f %20.17f\n",poly_coords[9],poly_coords[10],poly_coords[11]);
              }
#endif

            }
          } else {
            Throw() << "Meshes with parametric dimension != 2 not supported for computing areas for multi-tile grids";
          }

          //// Put data into the Array
          localArray->setData(index, area);
        }
      }
    }

  }



} // namespace
