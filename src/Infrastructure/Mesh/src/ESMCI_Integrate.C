// $Id: ESMCI_Integrate.C,v 1.2 2010/04/30 23:14:15 rokuingh Exp $
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

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMCI_Integrate.C,v 1.2 2010/04/30 23:14:15 rokuingh Exp $";
//-----------------------------------------------------------------------------

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

void Integrate::int_weights_serial(MEField<> *iwts) {
  Trace __trace("int_weights_serial()");

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
// Initialize integration weights
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
  Mesh::iterator ni = mesh.node_begin(), ne = mesh.node_end();
  for (; ni != ne; ++ni) {
    double *data = iwts->data(*ni);
    *data = 0;
  }

  MEField<> *cfield = mesh.GetCoordField();

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
// Generating integration weights
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
//!!!!!!!!  WATCH THIS, MAY BE CHANGING COORD FIELD!!!!!!!!!!!!!!
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

void Integrate::int_weights_parallel(MEField<> *iwts) {
  Trace __trace("int_weights_parallel()");

  /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
  // Initialize integration weights
  /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
  Mesh::iterator ni = mesh.node_begin(), ne = mesh.node_end();
  for (; ni != ne; ++ni) {
    double *data = iwts->data(*ni);
    *data = 0;
  }

  int id;
  MPI_Comm_rank(MPI_COMM_WORLD, &id);

  /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
  // Generating integration weights
  /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
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

  // swap_add both meshes
  mesh.SwapOp<double>(1,&iwts,CommRel::OP_SUM);

  }
}
