// $Id: ESMCI_MeshRedist.C,v 1.23 2012/01/06 20:17:51 svasquez Exp $
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
#include <Mesh/include/ESMCI_MeshRedist.h>
#include <Mesh/include/ESMCI_MeshTypes.h>
#include <Mesh/include/ESMCI_MeshObjTopo.h>
#include <Mesh/include/ESMCI_MeshOBjConn.h>
#include <Mesh/include/ESMCI_Mapping.h>
#include <Mesh/include/ESMCI_MeshObj.h>
#include <Mesh/include/ESMCI_Mesh.h>
#include <Mesh/include/ESMCI_MeshUtils.h>
#include <Mesh/include/ESMCI_MathUtil.h>
#include "Mesh/include/ESMCI_DDir.h" 
#include <Mesh/include/ESMCI_ParEnv.h>
#include <Mesh/include/ESMCI_CommReg.h>
#include <Mesh/include/ESMCI_MeshVTK.h>

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
static const char *const version = "$Id: ESMCI_MeshDual.C,v 1.23 2012/01/06 20:17:51 svasquez Exp $";
//-----------------------------------------------------------------------------


 
namespace ESMCI {

  const MeshObjTopo *ElemType2Topo(int pdim, int sdim, int etype);

  void triangulate(int sdim, int num_p, double *p, double *td, int *ti, int *tri_ind, 
                   double *tri_frac);
  
  void get_num_elems_around_node(MeshObj *node, int *_num_ids);

  void add_ghost_elems_to_split_orig_id_map(Mesh *mesh);

  struct MDSS {
    double angle;
    UInt id;

    MDSS() {
      angle=0.0;
      id=0;
    }

    MDSS &operator= (const MDSS &rhs) {
      angle=rhs.angle;
       id=rhs.id;
    }

    bool operator< (const MDSS &rhs) const {
      return angle < rhs.angle;
    }

  };

  void get_unique_elems_around_node(MeshObj *node, Mesh *mesh, MDSS *tmp_mdss,
                                int *_num_ids, UInt *ids);



  // Create a dual of the input Mesh 
  // This adds ghostcells to the input mesh, 
  // it also creates ghostcells for the dual mesh
  void MeshDual(Mesh *src_mesh, Mesh **_dual_mesh) {

  Trace __trace("MeshDual(Mesh *src_mesh, Mesh **dual_mesh)");

  // Don't currently support duals of 3D Meshes
  if (src_mesh->parametric_dim()>2) {
    Throw() <<" Creation of a dual mesh isn't supported for Meshes of parametric dim greater than 3.\n";
  }

  // Need element coordinates
  if (!src_mesh->GetField("elem_coordinates")) {
    Throw() <<" Creation of a dual mesh requires element coordinates. \n";
  }


  // Add ghostcells to source mesh, because we need the surrounding 
  // cells
   {
    int num_snd=0;
    MEField<> *snd[10],*rcv[10];
    
    // Load coord field
    MEField<> *psc = src_mesh->GetCoordField();
    snd[num_snd]=psc;
    rcv[num_snd]=psc;
    num_snd++;

    // Load element mask value field
    MEField<> *psm = src_mesh->GetField("elem_mask_val");
    if (psm != NULL) {
      snd[num_snd]=psm;
      rcv[num_snd]=psm;
      num_snd++;
    }

    // Load element mask field
    MEField<> *psem = src_mesh->GetField("elem_mask");
    if (psem != NULL) {
      snd[num_snd]=psem;
      rcv[num_snd]=psem;
      num_snd++;
    }

    // Load element coordinate field
    MEField<> *psec = src_mesh->GetField("elem_coordinates");
    if (psec != NULL) {
      snd[num_snd]=psec;
      rcv[num_snd]=psec;
      num_snd++;
    }

    // TODO: add elem mask fields and mask_val fields   
    src_mesh->CreateGhost();
    src_mesh->GhostComm().SendFields(num_snd, snd, rcv);

    // If src_mesh is split, add newly created ghost elements to split_to_orig map
    if (src_mesh->is_split) add_ghost_elems_to_split_orig_id_map(src_mesh);
  }

 
  // Get some useful info
  int sdim=src_mesh->spatial_dim();
  int pdim=src_mesh->parametric_dim();

  // Create Mesh
  Mesh *dual_mesh=new Mesh();

  // Set Mesh dimensions
  dual_mesh->set_spatial_dimension(sdim);
  dual_mesh->set_parametric_dimension(pdim);

  // Get element coordinates
  MEField<> *elem_coords=src_mesh->GetField("elem_coordinates"); 
  if (!elem_coords) {
    Throw() <<" Creation of a dual mesh requires element coordinates. \n";
  }


  // Iterate through all src elements counting the number and creating a map
  std::map<UInt,UInt> id_to_index;
   int pos=0;
  MeshDB::iterator ei = src_mesh->elem_begin_all(), ee = src_mesh->elem_end_all();
  for (; ei != ee; ++ei) {
    MeshObj &elem=*ei;

    // Get element id
    UInt elem_id=elem.get_id();

    // Translate id if split
    if ((src_mesh->is_split) && (elem_id > src_mesh->max_non_split_id)) {
      std::map<UInt,UInt>::iterator soi =  src_mesh->split_to_orig_id.find(elem_id);
      if (soi != src_mesh->split_to_orig_id.end()) {
        elem_id=soi->second;
      } else {
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
  MeshDB::iterator ni = src_mesh->node_begin(), ne = src_mesh->node_end();
  for (; ni != ne; ++ni) {
    MeshObj &node=*ni;
    
    // Only do local nodes
    // ALSO DO NON-LOCAL NODES, BECAUSE OTHERWISE YOU 
    // CAN END UP NOT MAKING AN ELEM AS A HOME FOR 
    // A NODE THAT'S NEEDED ON ANOTHER PROC
    //if (!GetAttr(node).is_locally_owned()) continue;

    // Get number of elems
    int num_node_elems=0;
    get_num_elems_around_node(&node, &num_node_elems);    
    
    // If less than 3 (a triangle) then don't make an element
    if (num_node_elems < 3) continue;
    
    // maximum number of elems per node
    if (num_node_elems > max_num_node_elems) max_num_node_elems = num_node_elems;

    // Count number of elements
    max_num_elems++;

    // Count number of connections
    max_num_elemConn += num_node_elems;
  }
 

  // Create temp arrays for getting ordered elem ids
  MDSS *tmp_mdss=NULL;
  UInt *elems_around_node_ids=NULL;
  if (max_num_node_elems > 0) {
    tmp_mdss=new MDSS[max_num_node_elems];
    elems_around_node_ids=new UInt[max_num_node_elems];
  }

  // Create element lists
  int *elemType=NULL;
  UInt *elemId=NULL;
  UInt *elemOwner=NULL;
  if (max_num_elems>0) {
    elemType=new int[max_num_elems];
    elemId=new UInt[max_num_elems];
    elemOwner=new UInt[max_num_elems];
  }
  int *elemConn=NULL;
  if (max_num_elemConn >0) {
    elemConn=new int[max_num_elemConn];
  }

  // Iterate through src nodes creating elements
  int num_elems=0;
  int conn_pos=0;
  ni = src_mesh->node_begin();
  for (; ni != ne; ++ni) {
    MeshObj &node=*ni;
    
    // Only do local nodes
    // ALSO DO NON-LOCAL NODES, BECAUSE OTHERWISE YOU 
    // CAN END UP NOT MAKING AN ELEM AS A HOME FOR 
    // A NODE THAT'S NEEDED ON ANOTHER PROC
    //  if (!GetAttr(node).is_locally_owned()) continue;
    

    // Get list of element ids
    int num_elems_around_node_ids=0;
    get_unique_elems_around_node(&node, src_mesh, tmp_mdss,
                          &num_elems_around_node_ids,
                          elems_around_node_ids);
    
    // If less than 3 (a triangle) then don't make an element
    if (num_elems_around_node_ids < 3) continue;
    
    // Save elemType/number of connections 
    elemType[num_elems]=num_elems_around_node_ids;
    
    // Save elemId
    elemId[num_elems]=node.get_id();

    // Save owner
    elemOwner[num_elems]=node.get_owner();

    // printf("%d# eId=%d eT=%d ::",Par::Rank(),elemId[num_elems],elemType[num_elems]);
    
    // Next elem
    num_elems++;

    //    printf("Elem id=%d max=%d num=%d :: ",node.get_id(),max_num_node_elems,num_elems_around_node_ids);

    // Loop elements attached to node and build connection list
    for (int i=0; i<num_elems_around_node_ids; i++) {

      // Get elem id
      UInt elem_id=elems_around_node_ids[i];
      
      // printf(" %d ",elem_id);

      // Get index of this element
      int node_index=id_to_index[elem_id];
      
      // Record that this node was used
      nodes_used[node_index]=1;

      // Push connection
      elemConn[conn_pos]=node_index;

      //double *ec=elem_coords->data(*elem);
      //printf("[%d %f %f] ",elem_id,ec[0],ec[1]);
  
      // Next connection
      conn_pos++;
    }

    // printf("\n");
  }

  // Free tmp arrays
  if (tmp_mdss != NULL) delete [] tmp_mdss;
  if (elems_around_node_ids != NULL) delete [] elems_around_node_ids;


  // Iterate through all src elements creating nodes
  MeshObj **nodes=NULL;
  if (num_nodes>0) nodes=new MeshObj *[num_nodes];
  pos=0;
   int data_index=0;
  ei = src_mesh->elem_begin_all();
  for (; ei != ee; ++ei) {
      MeshObj &elem=*ei;

      // Get element id
      UInt elem_id=elem.get_id();

      // Translate id if split
      if ((src_mesh->is_split) && (elem_id > src_mesh->max_non_split_id)) {
        std::map<UInt,UInt>::iterator soi =  src_mesh->split_to_orig_id.find(elem_id);
        if (soi != src_mesh->split_to_orig_id.end()) {
          elem_id=soi->second;
        } else {
          Throw() << "split elem id not found in map";
        }
      }

      // Get owner
      UInt owner=elem.get_owner();

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

        // Create node  
        MeshObj *node = new MeshObj(MeshObj::NODE, elem_id, data_index);
        node->set_owner(owner);
        dual_mesh->add_node(node, 0);
        data_index++;

        //printf("%d# node=%d owner=%d islocal=%d \n",Par::Rank(),node->get_id(),node->get_owner(),GetAttr(*node).is_locally_owned());


        // Record nodes
        nodes[pos]=node;
      }

      // Next pos
      pos++;
    }

    // Register Node fields
    IOField<NodalField> *dm_node_coord = dual_mesh->RegisterNodalField(*dual_mesh, "coordinates", sdim);

    IOField<NodalField> *dm_node_mask_val=NULL;
    MEField<> *elem_mask_val=src_mesh->GetField("elem_mask_val"); 
    if (elem_mask_val != NULL) {
      dm_node_mask_val = dual_mesh->RegisterNodalField(*dual_mesh, "node_mask_val", 1);
    }

    IOField<NodalField> *dm_node_mask=NULL;
    MEField<> *elem_mask=src_mesh->GetField("elem_mask"); 
    if (elem_mask != NULL) {
      dm_node_mask = dual_mesh->RegisterNodalField(*dual_mesh, "mask", 1);
    }


    // Iterate through all src elements putting in node coords
    // NOTE: this only works because the two src element loops are in the same order
    pos=0;
    ei = src_mesh->elem_begin_all();
    for (; ei != ee; ++ei) {

      // Only do if used
      if (nodes_used[pos]) {
        // Get element
        MeshObj &elem=*ei;

        // Get node
        MeshObj &node=*(nodes[pos]);

        // Get elem coord pointer
        double *ec=elem_coords->data(elem);

        // Get node coord pointer
        double *nc=dm_node_coord->data(node);

        // Copy coords from elem to node
        for (int i=0; i<sdim; i++) {
          nc[i]=ec[i];
        }
        // printf("%d# H1 id=%d pos=%d nc=%f %f ec=%f %f\n",Par::Rank(),node->get_id(),pos,nc[0],nc[1],ec[0],ec[1]);

        // Copy mask 
        if ((elem_mask_val != NULL) && (dm_node_mask_val != NULL)) {
          double *emv=elem_mask_val->data(elem);
          double *nmv=dm_node_mask_val->data(node);
          *nmv=*emv;
        }  

        // Copy mask 
        if ((elem_mask != NULL) && (dm_node_mask != NULL)) {
          double *em=elem_mask->data(elem);
          double *nm=dm_node_mask->data(node);
          *nm=*em;
        }  

      }
      // Next pos
      pos++;
    }


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
    MPI_Allreduce(&num_extra_elem,&tot_num_extra_elem,1,MPI_INT,MPI_SUM,Par::Comm());

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
    MPI_Allreduce(&max_id,&global_max_id,1,MPI_INT,MPI_MAX,Par::Comm());
    
    // Set maximum of non-split ids
    dual_mesh->max_non_split_id=global_max_id;

    // Calc our range of extra elem ids
    beg_extra_ids=0;
    MPI_Scan(&num_extra_elem,&beg_extra_ids,1,MPI_INT,MPI_SUM,Par::Comm());
    
    // Remove this processor's number from the sum to get the beginning
    beg_extra_ids=beg_extra_ids-num_extra_elem;
    
    // Start 1 up from max
    beg_extra_ids=beg_extra_ids+global_max_id+1;
    
    // printf("%d# beg_extra_ids=%d end=%d\n",Par::Rank(),beg_extra_ids,beg_extra_ids+num_extra_elem-1);
  }



  // Generate connectivity list with split elements
  // TODO: MAYBE EVENTUALLY PUT EXTRA SPLIT ONES AT END
  int num_elems_wsplit=0;
  int *elemConn_wsplit=NULL;
  int *elemType_wsplit=NULL;
  UInt *elemId_wsplit=NULL;
  UInt *elemOwner_wsplit=NULL;

  //  int *elemMaskIIArray_wsplit=NULL;
  //InterArray *elemMaskII_wsplit=NULL;

  if (dual_mesh->is_split) {
    // New number of elements
    num_elems_wsplit=num_elems+num_extra_elem;

    // Allocate arrays to hold split lists
    elemConn_wsplit=new int[max_num_elemConn+3*num_extra_elem];
    elemType_wsplit=new int[num_elems_wsplit];
    elemId_wsplit=new UInt[num_elems_wsplit];
    elemOwner_wsplit=new UInt[num_elems_wsplit];

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
            MeshObj *node=nodes[elemConn[conn_pos+i]];
            double *crd=dm_node_coord->data(*node);
            for (int j=0; j<sdim; j++) {
              polyCoords[crd_pos]=crd[j];
              crd_pos++;
            }

            //printf("%d# id=%d c=%d coord=%f %f \n",Par::Rank(),elemId[e],elemConn[conn_pos+i],polyCoords[crd_pos-2],polyCoords[crd_pos-1]);

          }

          // Triangulate polygon
          triangulate(sdim, elemType[e], polyCoords, polyDblBuf, polyIntBuf, 
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


    // Build elements
    // Now loop the elements and add them to the mesh.
    int cur_conn = 0;
    for (int e = 0; e < num_elems; ++e) {

      // Get/deduce the element topology
      const MeshObjTopo *topo = ElemType2Topo(pdim,
                                              sdim, 
                                              elemType[e]);

      int nnodes = topo->num_nodes;
      std::vector<MeshObj*> nconnect(nnodes, static_cast<MeshObj*>(0));

      // The object
      UInt eid = elemId[e];
      MeshObj *elem = new MeshObj(MeshObj::ELEMENT, eid, e);
      elem->set_owner(elemOwner[e]);

      for (int n = 0; n < nnodes; ++n) {
      
        // Get 0-based node index
        int node_index=elemConn[cur_conn];

        // Setup connectivity list
        nconnect[n] = nodes[node_index];

        // Advance to next
        cur_conn++;
      }

      dual_mesh->add_element(elem, nconnect, topo->number, topo);


    } // for e

    // Clean up
    if (nodes_used !=NULL) delete [] nodes_used;
    if (elemType !=NULL) delete [] elemType;
    if (elemId !=NULL) delete [] elemId;
    if (elemOwner !=NULL) delete [] elemOwner;
    if (elemConn !=NULL) delete [] elemConn;
    if (nodes !=NULL) delete [] nodes;

    // The main goal for this is to use it for non-conserve on centers, so
    // I'm not moving some of the node information to elem (e.g. masking)
    // do that eventually for full generality. 
    // However, I am registering thw elem frac field, because that's the one
    // that should always be there. 
    {
      Context ctxt; ctxt.flip();
      MEField<> *elem_frac = dual_mesh->RegisterField("elem_frac",
                  MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);
    }

    // Commit Mesh
    dual_mesh->build_sym_comm_rel(MeshObj::NODE);
    dual_mesh->Commit();


    
    // Output 
    *_dual_mesh=dual_mesh;
}



// Get the element topology
const MeshObjTopo *ElemType2Topo(int pdim, int sdim, int etype) {

  if (pdim==2) {
    if (sdim==2) {
      if (etype==3) {
        return GetTopo("TRI3");
      } else if (etype==4) {
        return GetTopo("QUAD");
      } else {
        Throw()<< "- for a mesh with parametric dimension 2 topo types must be either triangles or quadrilaterals\n";
      }
    } else if (sdim==3) {
      if (etype==3) {
        return GetTopo("SHELL3");
      } else if (etype==4) {
        return GetTopo("SHELL");
      } else {
        Throw()<<"- for a mesh with parametric dimension 2 topo types must be either triangles or quadrilaterals\n";
      }
    }
  } else if (pdim==3) {
    return Vtk2Topo(sdim, etype);
  }
}



// triangulate > 4 sided
// sdim = spatial dim
// num_p = number of points in poly
// p     = poly coords size=num_p*sdim
// td    = temporary buffer size=num_p*sdim
// ti    = temporary integer buffer size = num_p
// tri_ind = output array  size = 3*(nump-2)
// tri_frac = fraction each triangle is of whole poly size=(num_p-2)
void triangulate(int sdim, int num_p, double *p, double *td, int *ti, int *tri_ind, 
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



  // Get the number of element ids around a node
  // Useful for precalculating size for get_elem_ids_around_node()
  void get_num_elems_around_node(MeshObj *node, int *_num_ids) {
    
    // Loop elements attached to node and build connection list
    int num_ids=0;
    MeshObjRelationList::const_iterator el = MeshObjConn::find_relation(*node, MeshObj::ELEMENT);
    while (el != node->Relations.end() && el->obj->get_type() == MeshObj::ELEMENT){
      MeshObj *elem=el->obj;
      
      // Get elem id
      UInt elem_id=elem->get_id();      

      // increase size
      num_ids++;
     
      // Next element
      ++el;
    }

    // Output
    *_num_ids=num_ids;

  }

  // sort MDSS by id
  bool less_by_ids(MDSS a, MDSS b) {
    return (a.id < b.id);
  }

  bool equal_by_ids(MDSS a, MDSS b) {
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
  void get_unique_elems_around_node(MeshObj *node, Mesh *mesh, MDSS *tmp_mdss,
                                    int *_num_ids, UInt *ids) {

    // Get useful info
    int sdim=mesh->spatial_dim();
    int pdim=mesh->parametric_dim();

    MEField<> *elem_coords=mesh->GetField("elem_coordinates"); 
    if (!elem_coords) {
      Throw() <<" Creation of a dual mesh requires element coordinates. \n";
    }

    MEField<> *node_coords=mesh->GetCoordField();
    if (!node_coords) {
      Throw() <<" Creation of a dual mesh requires node coordinates. \n";
    }

     // Center coordinates
    // NOTE: Mostly treat as 3D to avoid lots of if (sdim=...)
    double center[3];
    double *nc=node_coords->data(*node);
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

    // Get interator for looping elements around node
    MeshObjRelationList::const_iterator el = MeshObjConn::find_relation(*node, MeshObj::ELEMENT);

    // No elements so leave
    if (el == node->Relations.end() || el->obj->get_type() != MeshObj::ELEMENT){
      *_num_ids=0;
      return;
    }

    // Get coords from elem with max id to make things consistent
    // on different processors
    // Loop the rest of the elements 
    UInt max_elem_id=0; // Init to 0 to watch for nothing ever being selected
    double max_elem_coords[3];
    while (el != node->Relations.end() && el->obj->get_type() == MeshObj::ELEMENT){
      MeshObj *elem=el->obj;

      // Get id 
      int elem_id=elem->get_id();

      // Translate id if split
      if ((mesh->is_split) && (elem_id > mesh->max_non_split_id)) {
        std::map<UInt,UInt>::iterator soi =  mesh->split_to_orig_id.find(elem_id);
        if (soi != mesh->split_to_orig_id.end()) {
          elem_id=soi->second;
        } else {
          Throw() << "split elem id not found in map";
        }
      }
 
      // Check if max id if so switch max id and coordinates 
      if (elem_id > max_elem_id) {
        double *ec=elem_coords->data(*elem);
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
            
      // Next element
      ++el;
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
    el = MeshObjConn::find_relation(*node, MeshObj::ELEMENT);
    while (el != node->Relations.end() && el->obj->get_type() == MeshObj::ELEMENT){
      MeshObj *elem=el->obj;

      // Get id 
      int elem_id=elem->get_id();

      // Translate id if split
      if ((mesh->is_split) && (elem_id > mesh->max_non_split_id)) {
        std::map<UInt,UInt>::iterator soi =  mesh->split_to_orig_id.find(elem_id);
        if (soi != mesh->split_to_orig_id.end()) {
          elem_id=soi->second;
        } else {
          Throw() << "split elem id not found in map";
        }
      }

      // Get vector to current element 
      // NOTE: Mostly treat as 3D to avoid lots of if (sdim=...)
      double vcurr[3];
      double *ec=elem_coords->data(*elem);
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
      
      // Next element
      ++el;
    }



    // Take out repeats due to split elements
     //// Sort by id
    std::sort(tmp_mdss, tmp_mdss+num_ids, less_by_ids);

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
  void add_ghost_elems_to_split_orig_id_map(Mesh *mesh) {

    // Only do this if mesh is split
    if (!mesh->is_split) return;

    // Get number of split elements
    int num_gids=0;
    Mesh::iterator ei = mesh->elem_begin(), ee = mesh->elem_end();
    for (; ei != ee; ++ei) {
      MeshObj &elem = *ei;
 
      // Only do local 
      if (!GetAttr(elem).is_locally_owned()) continue;

      // Get element id
      UInt elem_id=elem.get_id();

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
      ei = mesh->elem_begin();
      for (; ei != ee; ++ei) {
        MeshObj &elem = *ei;
        
        // Only do local
        if (!GetAttr(elem).is_locally_owned()) continue;
        
        // Get element id
        UInt elem_id=elem.get_id();
        
        // If it's less than or equal to the maximum non split id then its not split
        if (elem_id <=  mesh->max_non_split_id) continue;
        
        // See if this is a split id
        std::map<UInt,UInt>::iterator soi=mesh->split_to_orig_id.find(elem_id);
        
        // If this is a split set it to the original, otherwise just set it to the elem id
        UInt orig_id;
        if (soi != mesh->split_to_orig_id.end()) {
          orig_id=soi->second;
        } else {
          Throw() << "split id not in split id to orig id map!";
        }
        
        // Put into arrays
        gids_orig[pos]=orig_id;
        gids_split[pos]=elem_id;
        
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
  ei = mesh->elem_begin_all(), ee = mesh->elem_end_all();
  for (; ei != ee; ++ei) {
    MeshObj &elem = *ei;
    
    // Only do non-local
    if (GetAttr(elem).is_locally_owned()) continue;
    
    // Get element id
    UInt elem_id=elem.get_id();
    
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
  ei = mesh->elem_begin_all(), ee = mesh->elem_end_all();
  for (; ei != ee; ++ei) {
    MeshObj &elem = *ei;
    
    // Only do non-local
    if (GetAttr(elem).is_locally_owned()) continue;
    
    // Get element id
    UInt elem_id=elem.get_id();
    
    // If it's less than or equal to the maximum non split id then its not split
    if (elem_id <=  mesh->max_non_split_id) continue;

    //    printf("%d# p=%d ghost gid=%d\n",Par::Rank(),pos,elem_id);

    // Put in list
    ghost_gids[pos]=elem_id;

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
  for (int i=0; i<lookups.size(); i++) {
    
    // If split put into map
    if (lookups[i].gid != lookups[i].origin_lid) {
      mesh->split_to_orig_id[lookups[i].gid]=lookups[i].origin_lid;
    }
  }
  
  }


  } // namespace
