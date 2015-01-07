// $Id: ESMCI_MeshRedist.C,v 1.23 2012/01/06 20:17:51 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2015, University Corporation for Atmospheric Research, 
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


  // Create a dual of the input Mesh 
  void MeshDual(Mesh *src_mesh, Mesh **_dual_mesh) {

  Trace __trace("MeshDual(Mesh *src_mesh, Mesh **dual_mesh)");

  // Don't currently support duals of 3D Meshes
  if (src_mesh->parametric_dim()>2) {
    Throw() <<" Creation of a dual mesh isn't supported for Meshes of parametric dim greater than 3.\n";
  }

  // Don't currently support duals of 3D Meshes
  if (src_mesh->is_split) {
    Throw() <<" Creation of a dual mesh isn't supported when original Mesh contains cells with >4 sides.\n";
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
    
    // id to index map
    id_to_index[elem.get_id()]=pos;
    
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


  // Sizes
  int num_elems=0;
  int num_elemConn=0;

  // Iterate through src nodes counting sizes
  MeshDB::iterator ni = src_mesh->node_begin(), ne = src_mesh->node_end();
  for (; ni != ne; ++ni) {
    MeshObj &node=*ni;
    
    // Only do local nodes
    if (!GetAttr(node).is_locally_owned()) continue;
    
    // Count elems attached to node
    int num_conn=0;
    MeshObjRelationList::const_iterator el = MeshObjConn::find_relation(node, MeshObj::ELEMENT);
    while (el != node.Relations.end() && el->obj->get_type() == MeshObj::ELEMENT){
      // Count connection for this node
      num_conn++;
      
      // Next element
      ++el;
    }
    
    // If less than 3 (a triangle) then don't make an element
    if (num_conn < 3) continue;
    
    // Count number of elements
    num_elems++;

    // Count number of connections
    num_elemConn += num_conn;
  }


  // Create element lists
  int *elemType=NULL;
  UInt *elemId=NULL;
  if (num_elems>0) {
    elemType=new int[num_elems];
    elemId=new UInt[num_elems];
  }
  int *elemConn=NULL;
  if (num_elemConn >0) {
    elemConn=new int[num_elemConn];
  }

  // Iterate through src nodes creating elements
  int elem_pos=0;
  int conn_pos=0;
  ni = src_mesh->node_begin();
  for (; ni != ne; ++ni) {
    MeshObj &node=*ni;
    
    // Only do local nodes
    if (!GetAttr(node).is_locally_owned()) continue;
    
    // Count elems attached to node
    int num_conn=0;
    MeshObjRelationList::const_iterator el = MeshObjConn::find_relation(node, MeshObj::ELEMENT);
    while (el != node.Relations.end() && el->obj->get_type() == MeshObj::ELEMENT){
      // Count connection for this node
      num_conn++;
      
      // Next element
      ++el;
    }
    
    // If less than 3 (a triangle) then don't make an element
    if (num_conn < 3) continue;
    
    // Save elemType/number of connections 
    elemType[elem_pos]=num_conn;
    
    // Save elemId
    elemId[elem_pos]=node.get_id();

    // printf("%d# eId=%d eT=%d ::",Par::Rank(),elemId[elem_pos],elemType[elem_pos]);
    
    // Next elem
    elem_pos++;



    // Loop elements attached to node and build connection list
    el = MeshObjConn::find_relation(node, MeshObj::ELEMENT);
    while (el != node.Relations.end() && el->obj->get_type() == MeshObj::ELEMENT){
      MeshObj *elem=el->obj;
      
      // Get elem id
      UInt elem_id=elem->get_id();
      
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
    
      // Next element
      ++el;
    }
  }


  // Iterate through all src elements creating nodes
  MeshObj **nodes=NULL;
  if (num_nodes>0) nodes=new MeshObj *[num_nodes];
  pos=0;
  int data_index=0;
  ei = src_mesh->elem_begin_all();
  for (; ei != ee; ++ei) {
      MeshObj &elem=*ei;

      //      double *coords=elem_coords->data(elem);
      //printf("#%d E elem id=%d coords=%f %f owner=%d is_local=%d data_index=%d\n",Par::Rank(),elem.get_id(),coords[0],coords[1],elem.get_owner(),GetAttr(elem).is_locally_owned(),
      //     elem.get_data_index());

      // Only create if used
      if (nodes_used[pos]) {
        // Create node  
        MeshObj *node = new MeshObj(MeshObj::NODE, elem.get_id(), data_index);
        node->set_owner(elem.get_owner());
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
  //  int *elemMaskIIArray_wsplit=NULL;
  //InterfaceInt *elemMaskII_wsplit=NULL;

  if (dual_mesh->is_split) {
    // New number of elements
    num_elems_wsplit=num_elems+num_extra_elem;

    // Allocate arrays to hold split lists
    elemConn_wsplit=new int[num_elemConn+3*num_extra_elem];
    elemType_wsplit=new int[num_elems_wsplit];
    elemId_wsplit=new UInt[num_elems_wsplit];


#if 0
      //// Setup for split mask
      int *elemMaskIIArray=NULL;
      if (elemMaskII != NULL) { 

        // Get mask value array
        elemMaskIIArray=elemMaskII->array;

        int extent[1];
        elemMaskIIArray_wsplit=new int[num_elems_wsplit];

        extent[0]=num_elems_wsplit;
        elemMaskII_wsplit=new InterfaceInt(elemMaskIIArray_wsplit,1,extent);
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
    if (elemConn !=NULL) delete [] elemConn;

      // Use the new split list for the connection lists below
      num_elems=num_elems_wsplit;
      elemConn=elemConn_wsplit;
      elemType=elemType_wsplit;
      elemId=elemId_wsplit;

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
    if (elemConn !=NULL) delete [] elemConn;
    if (nodes !=NULL) delete [] nodes;


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
              Throw() << " - clockwise polygons not supported in triangulation routine";
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

  
} // namespace

