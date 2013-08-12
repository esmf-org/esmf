// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

#include <Mesh/include/ESMCI_Integrate.h>
#include <Mesh/include/ESMCI_Interp.h>
#include <Mesh/include/ESMCI_Mesh.h>
#include <Mesh/include/ESMCI_ParEnv.h>
#include <Mesh/include/ESMCI_Rebalance.h>
#include <Mesh/include/ESMCI_GlobalIds.h>

#include <set>
#include <map>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

namespace ESMCI {

// constructor
Integrate::Integrate(Mesh &_mesh) : 
mesh(_mesh),
is_parallel(Par::Size() > 1)
{
//  mesh(_mesh);
}

// destructor
Integrate::~Integrate() {
}


/*
 * There is an ASSUMPTION here that the field is nodal, both sides
 */
void Integrate::intWeights(MEField<> *iwts) {
          Trace __trace("Integrate::intWeights(MEField<> *iwts)");

  if (is_parallel)
    int_weights_parallel(iwts);
  else int_weights_serial(iwts);

}

 void Integrate::clearWeights(MEField<> *iwts) {
    Trace __trace("int_weights_serial()");
    
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
// Initialize integration weights
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
  Mesh::iterator ni =mesh.node_begin(), ne = mesh.node_end();
  for (; ni != ne; ++ni) {
    double *data = iwts->data(*ni);
    *data = 0;
  }
 }


  // Calculate weights based on great circle areas
  void weights_great_circle_same(int n, double *pnts, double *weights);
  
  void Integrate::int_weights_serial(MEField<> *iwts) {
    Trace __trace("int_weights_serial()");
    
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
// Generating integration weights
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
  // Get coord field
  MEField<> *cfield = mesh.GetCoordField();

  // If we're on a sphere then calculate using spherical metric 
  // based on great circle areas
  if ((mesh.spatial_dim() == 3) && (mesh.parametric_dim() == 2)) { 
    int sdim=mesh.spatial_dim();
    std::vector<double> node_coords;
    node_coords.reserve(4*3); // make big enough to hold biggest num coords in elem
                             // will also resize if for some reason something bigger
    std::vector<double> node_weights;
    node_weights.reserve(4); // make big enough to hold biggest num coords in elem
                             // will also resize if for some reason something bigger
    Mesh::iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
    for (; ei != ee; ++ei) {
      MeshObj &elem = *ei;
            
      // Get number of nodes in element
      const ESMCI::MeshObjTopo *topo = ESMCI::GetMeshObjTopo(elem);

      // make sure coord vector is big enough
      node_coords.resize(topo->num_nodes*sdim,0.0);

      // Get coords of element
      int k=0;
      for (ESMCI::UInt s = 0; s < topo->num_nodes; ++s){
	const MeshObj &node = *(elem.Relations[s].obj);
	double *c = cfield->data(node);
        for (int i=0; i<sdim; i++) {
	  node_coords[k]=c[i];
          k++;
	}
      }

      // make sure weight vector is big enough
      node_weights.resize(topo->num_nodes,0.0);

      // compute great circle weights
      weights_great_circle_same(topo->num_nodes,&(node_coords[0]),&(node_weights[0]));

      // Put weights into field
      for (ESMCI::UInt s = 0; s < topo->num_nodes; ++s){
	const MeshObj &node = *(elem.Relations[s].obj);
	double *data = iwts->data(node);
        *data += node_weights[s];
      }

    }
  } else {
    MEValues<> mev(iwts->GetMEFamily(),mesh.GetCoordField());
    Mesh::iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
    for (; ei != ee; ++ei) {
      MeshObj &elem = *ei;
      // Setup
      mev.Setup(elem, MEV::update_jxw, GetIntg(elem));
      mev.ReInit(elem);
      
      const ESMCI::MeshObjTopo *topo = ESMCI::GetMeshObjTopo(elem);
      
      ESMCI::MeshObjRelationList::iterator fi = ESMCI::MeshObjConn::
	find_relation(elem,ESMCI::MeshObj::NODE,0,ESMCI::MeshObj::USES);
      
      double weight;
      for (ESMCI::UInt s = 0; s < topo->num_nodes; ++s, ++fi){
	weight = mev.GetJxW(s);
	const MeshObj &node = *((*fi).obj);
	double *data = iwts->data(node);
	
	*data += weight;
      }
    }
  }
}

void Integrate::int_weights_parallel(MEField<> *iwts) {
  Trace __trace("int_weights_parallel()");

  int id;
  MPI_Comm_rank(MPI_COMM_WORLD, &id);

  /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
  // Generating integration weights
  /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
  // Get coord field
  MEField<> *cfield = mesh.GetCoordField();

  // If we're on a sphere then calculate using spherical metric 
  // based on great circle areas
  if ((mesh.spatial_dim() == 3) && (mesh.parametric_dim() == 2)) { 
    int sdim=mesh.spatial_dim();
    std::vector<double> node_coords;
    node_coords.reserve(4*3); // make big enough to hold biggest num coords in elem
                             // will also resize if for some reason something bigger
    std::vector<double> node_weights;
    node_weights.reserve(4); // make big enough to hold biggest num coords in elem
                             // will also resize if for some reason something bigger
    Mesh::iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
    for (; ei != ee; ++ei) {
      MeshObj &elem = *ei;
            
      // Get number of nodes in element
      const ESMCI::MeshObjTopo *topo = ESMCI::GetMeshObjTopo(elem);

      // make sure coord vector is big enough
      node_coords.resize(topo->num_nodes*sdim,0.0);

      // Get coords of element
      int k=0;
      for (ESMCI::UInt s = 0; s < topo->num_nodes; ++s){
	const MeshObj &node = *(elem.Relations[s].obj);
	double *c = cfield->data(node);
        for (int i=0; i<sdim; i++) {
	  node_coords[k]=c[i];
          k++;
	}
      }

      // make sure weight vector is big enough
      node_weights.resize(topo->num_nodes,0.0);

      // compute great circle weights
      weights_great_circle_same(topo->num_nodes,&(node_coords[0]),&(node_weights[0]));

      // Put weights into field
      for (ESMCI::UInt s = 0; s < topo->num_nodes; ++s){
	const MeshObj &node = *(elem.Relations[s].obj);
	double *data = iwts->data(node);
        *data += node_weights[s];
      }

    }
  } else { 
    MEValues<> mev(iwts->GetMEFamily(),mesh.GetCoordField());
    Mesh::iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
    for (; ei != ee; ++ei) {
      MeshObj &elem = *ei;
      
      mev.Setup(elem, MEV::update_jxw, GetIntg(elem));
      mev.ReInit(elem);
      const ESMCI::MeshObjTopo *topo = ESMCI::GetMeshObjTopo(elem);
      ESMCI::MeshObjRelationList::iterator fi = ESMCI::MeshObjConn::
	find_relation(elem,ESMCI::MeshObj::NODE,0,ESMCI::MeshObj::USES);
      for (ESMCI::UInt s = 0; s < topo->num_nodes; ++s, ++fi){
	// get node and check that it's local
	//const MeshObj &node = *(elem.Relations[s].obj);
	const MeshObj &node = *((*fi).obj);
        double weight = mev.GetJxW(s);
        // get the data of src_iwts field that corresponds to this node
        double *data = iwts->data(node);
        *data += weight;
      }
    }
  }

  // swap_add both meshes
  mesh.SwapOp<double>(1,&iwts,CommRel::OP_SUM);

}



//    w
//    | \
//    |   \
//    |     \
//    u------v
//    ^
//    | --- angle being calculated
//      
  double angle_gc(double *u, double *v, double *w) {
  
  double cosa=u[0]*v[0]+u[1]*v[1]+u[2]*v[2];

  double cosb=u[0]*w[0]+u[1]*w[1]+u[2]*w[2];

  double cosc=v[0]*w[0]+v[1]*w[1]+v[2]*w[2];

  //  printf("a=%f b=%f c=%f \n",cosa,cosb,cosc);

  double sina=sqrt(1.0-cosa*cosa);

  double sinb=sqrt(1.0-cosb*cosb);


  return acos((cosc-(cosa*cosb))/(sina*sinb));

}


// Computes all of the weights as the same

// For 3D points in counter-clockwise order
// For n<3 this gives undefined results
void weights_great_circle_same(int n, double *pnts, double *weights) {

  // sum angles around polygon
  double sum=0.0;
  for (int i=0; i<n; i++) {
    // points that make up a side of polygon
    double *pnt0=pnts+3*i;
    double *pnt1=pnts+3*((i+1)%n);
    double *pnt2=pnts+3*((i+2)%n);

    // compute angle for pnt1
    sum += angle_gc(pnt1, pnt2, pnt0);
  }

  // Compute weight
  double w=(sum-(n-2)*M_PI)/((double) n);

  // return weights
  for (int i=0; i<n; i++) {
    weights[i]=w;
  }
}



// Version that doesn't move nodes around
  void Integrate::AddPoleWeights(Mesh &mesh,  UInt node_id, MEField<> *iwts)
{

  UInt rank = Par::Rank();


  // Get all the local nodes and elements around pole
  std::set<MeshObj*> elems, nodes;
    // Loop the nodes with given node_id
    KernelList::iterator ki = mesh.set_begin(), ke = mesh.set_end();
  
    for (; ki != ke; ++ki) {
    
      Kernel &ker = *ki;


      if (ker.type() == MeshObj::NODE && ker.key() == node_id && ker.is_owned()) {

        Kernel::obj_iterator oi = ker.obj_begin(), oe = ker.obj_end();
      
        for (; oi != oe; ++oi) {

          MeshObj &node = *oi;

          nodes.insert(&node);

          // Push all connected elements onto list
          MeshObjRelationList::iterator ei = node.Relations.begin(), ee = node.Relations.end();
          for (; ei != ee; ++ei) {

            if (ei->type == MeshObj::USED_BY && ei->obj->get_type() == MeshObj::ELEMENT) {
              elems.insert(ei->obj);
            }

          }


        } // oi

      } 

    } // ki



   
  // Figure out global number of nodes, if none then leave
  int local_num_nodes = nodes.size();
  int num_nodes=0;


  MPI_Allreduce(&local_num_nodes, &num_nodes, 1, MPI_INT, MPI_SUM, Par::Comm());
  if (num_nodes == 0) return; // No nodes so exit

  //// Figure out pole coordinates ////////
  // Get coordinate Field
  MEField<> &coords = *mesh.GetCoordField();

  // Figure out global coordinate sum
  double local_pole_coords[3] = {0, 0, 0};

  std::set<MeshObj*>::iterator ni = nodes.begin(), ne = nodes.end();
  for (; ni != ne; ++ni) {
      double *c = coords.data(**ni);
      local_pole_coords[0] += c[0]; 
      local_pole_coords[1] += c[1]; 
      local_pole_coords[2] += c[2];
  }

  double pole_coords[3] = {0, 0, 0};

  MPI_Allreduce(local_pole_coords, pole_coords, 3, MPI_DOUBLE, MPI_SUM, Par::Comm());

  // Compute average
  pole_coords[0] /= num_nodes;
  pole_coords[1] /= num_nodes;
  pole_coords[2] /= num_nodes;

  // Move coords onto the spehere
  double rr = 1.0/std::sqrt(pole_coords[0]*pole_coords[0] +
			    pole_coords[1]*pole_coords[1] +
			    pole_coords[2]*pole_coords[2]);

  pole_coords[0] *= rr;
  pole_coords[1] *= rr;
  pole_coords[2] *= rr;



  // Now add the triangles;
  // 1) Get the elements that surround the pole 'gap'.
  // 2) Get the two nodes on that element that are on the gap.
  // 3) Create a triangle that uses the two nodes and the pole.

  // Get spatial dimension
  int sdim=mesh.spatial_dim();
  
  // Loop generating coord information
  std::vector<double> node_coords;
  node_coords.resize(sdim*3,0.0);

  // Space for weights
  std::vector<double> node_weights;
  node_weights.resize(3,0.0);
  
             
  // Loop the elements
  std::set<MeshObj*>::iterator ei = elems.begin(), ee = elems.end();
  
  for (; ei != ee; ++ei) {
        
    MeshObj &elem = **ei;
    
    // Get side nodes with poleward boundary
    const MeshObjTopo *etopo = GetMeshObjTopo(elem);
    
    ThrowRequire(etopo->num_nodes == 4);
    
    int pole_side = -1;
    
    for (UInt s = 0; pole_side < 0 && s < etopo->num_sides; s++) {
      
      const int *side_nodes = etopo->get_side_nodes(s);
          
      // Check; if this is the side, all nodes on side should have the boundary context;
      bool is_pole_side = true;
      for (UInt sn = 0; is_pole_side && sn < (UInt) etopo->num_side_nodes; sn++) {
	
	is_pole_side = elem.Relations[side_nodes[sn]].obj->GetKernel()->key() == node_id;
        
      }
      
      if (is_pole_side) pole_side = s; 
      
    } // for s
    
    ThrowRequire(pole_side >= 0); // need to have found a side
    
    const int *side_nodes = etopo->get_side_nodes(pole_side);
    
    
    //// Loop through setting node coords
    ///// Node 0
    int k=0;
    {
      const MeshObj &node = *elem.Relations[side_nodes[1]].obj;
      double *c = coords.data(node);
      for (int j=0; j<sdim; j++) {
	node_coords[k]=c[j];
	k++;
      }
    }

    ///// Node 1
    {
      const MeshObj &node = *elem.Relations[side_nodes[0]].obj;
      double *c = coords.data(node);
      for (int j=0; j<sdim; j++) {
	node_coords[k]=c[j];
	k++;
      }
    }
    
    ///// Pole
    {
	for (int j=0; j<sdim; j++) {
	  node_coords[k]=pole_coords[j];
	  k++;
	}
    }

    // compute great circle weights
    weights_great_circle_same(3,&(node_coords[0]),&(node_weights[0]));
      
#if 0
    for (int i = 0; i < 3; ++i){
      printf(" %f ",node_weights[i]);
    }
    printf("\n");
#endif
    
    //// Put weights into nodes
    {
      const MeshObj &node = *elem.Relations[side_nodes[1]].obj;
      double *data = iwts->data(node);
      *data += (node_weights[0]+0.5*node_weights[2]); // Since no pole weight put onto node
      //      *data += node_weights[0];
    }

    {
      const MeshObj &node = *elem.Relations[side_nodes[0]].obj;
      double *data = iwts->data(node);
      *data += (node_weights[1]+0.5*node_weights[2]); // Since no pole weight put onto node
      //     *data += node_weights[1];
    }
        
    } // for ei

#if 0 
    double sum=0.0;
  for (ni = nodes.begin(); ni != ne; ++ni) {
      double *c = iwts->data(**ni);
      sum += *c;
      printf("%d %20.17f \n",(*ni)->get_id(),*c);
  }
      printf("NOMV SUM= %20.17f \n",sum);
#endif
     
}







}
