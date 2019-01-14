// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------

// Take out if MOAB isn't being used
#if defined ESMF_MOAB

#include <string>
#include <ostream>
#include <iterator>
#include <vector>

#include "ESMCI_Macros.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"
#include "ESMCI_CoordSys.h"

#include "Mesh/include/ESMCI_MBMesh.h"
#include "Mesh/include/ESMCI_MBMesh_Glue.h"

#include "moab/ParallelComm.hpp"
#include "MBParallelConventions.h"

#include "VM/include/ESMC_VM.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
// static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


using namespace ESMCI;
using namespace std;

// #define DEBUG_WRITE_MESH
// #define DEBUG_MOAB_GHOST_EXCHANGE


#define ESMF
#define ESMC_METHOD "MBMesh::func()"

MBMesh::MBMesh() : sdim(0),pdim(0),mesh(NULL),verts(NULL) {

} 

MBMesh::~MBMesh() {

  // get the indexed pcomm object from the interface
  // index 0 should return the one created inside MBMesh_addelements
  ParallelComm *pcomm = ParallelComm::get_pcomm(this->mesh, 0);
  delete pcomm;
  
  // Get rid of MOAB mesh
  if (mesh != NULL) delete mesh;

  // Get rid of list of verts
  if (verts != NULL) delete [] verts;
  
} 

void MBMesh::CreateGhost() {

  int merr, localrc;

  // Do this for now instead of initiating mesh parallel stuff
  // TODO: MAYBE EVENTUALLY PUT THIS INTO MBMesh???
  ESMC_VM vm;
  vm = ESMC_VMGetCurrent(&localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

  int localPet, petCount;
  MPI_Comm mpi_comm;
  localrc = ESMC_VMGet(vm, &localPet, &petCount, NULL, &mpi_comm, NULL, NULL);
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

  // get the indexed pcomm object from the interface
  // pass index 0, it will the one created inside MBMesh_addelements
  ParallelComm *pcomm = ParallelComm::get_pcomm(this->mesh, 0);
  
  // this is already called in MBMesh_addelements
  // merr = pcomm->resolve_shared_ents(0, elems, this->pdim, this->pdim-1);
  // MBMESH_CHECK_ERR(merr, localrc);

#ifdef DEBUG_MOAB_GHOST_EXCHANGE
  Range shared_ents;
  // Get entities shared with all other processors
  merr = pcomm->get_shared_entities(-1, shared_ents);
  MBMESH_CHECK_ERR(merr, localrc);
  
  // Filter shared entities with not not_owned, which means owned
  Range owned_entities;
  merr = pcomm->filter_pstatus(shared_ents, PSTATUS_NOT_OWNED, PSTATUS_NOT, -1, &owned_entities);
  MBMESH_CHECK_ERR(merr, localrc);
    
  unsigned int nums[4] = {0}; // to store the owned entities per dimension
  for (int i = 0; i < 4; i++)
    // nums[i] = (nt)shared_ents.num_of_dimension(i);
    nums[i] = (int)owned_entities.num_of_dimension(i);
    
  vector<int> rbuf(nprocs*4, 0);
  MPI_Gather(nums, 4, MPI_INT, &rbuf[0], 4, MPI_INT, 0, mpi_comm);
  // Print the stats gathered:
  if (0 == rank) {
    for (int i = 0; i < nprocs; i++)
      cout << " Shared, owned entities on proc " << i << ": " << rbuf[4*i] << " verts, " <<
          rbuf[4*i + 1] << " edges, " << rbuf[4*i + 2] << " faces, " << rbuf[4*i + 3] << " elements" << endl;
  }
#endif

  // Now exchange 1 layer of ghost elements, using vertices as bridge
  merr = pcomm->exchange_ghost_cells(this->pdim, // int ghost_dim
                                     0, // int bridge_dim
                                     1, // int num_layers
                                     0, // int addl_ents
                                     true);// bool store_remote_handles
  MBMESH_CHECK_ERR(merr, localrc);

  vector<Tag> node_tags;
  vector<Tag> elem_tags;
  
  node_tags.push_back(this->gid_tag);
  node_tags.push_back(this->orig_pos_tag);
  node_tags.push_back(this->owner_tag);
  if (this->has_node_orig_coords) node_tags.push_back(this->node_orig_coords_tag);
  if (this->has_node_mask) {
    node_tags.push_back(this->node_mask_tag);
    node_tags.push_back(this->node_mask_val_tag);
  }
  
  elem_tags.push_back(this->gid_tag);
  elem_tags.push_back(this->orig_pos_tag);
  elem_tags.push_back(this->owner_tag);
  if (this->has_elem_frac) elem_tags.push_back(this->elem_frac_tag);
  if (this->has_elem_mask) {
    elem_tags.push_back(this->elem_mask_tag);
    elem_tags.push_back(this->elem_mask_val_tag);
  }
  if (this->has_elem_area) elem_tags.push_back(this->elem_area_tag);
  if (this->has_elem_coords) elem_tags.push_back(this->elem_coords_tag);
  if (this->has_elem_orig_coords) elem_tags.push_back(this->elem_orig_coords_tag);
  
  // pcomm->set_debug_verbosity(4);

  Range nodes;
  merr=this->mesh->get_entities_by_dimension(0, 0, nodes);
  MBMESH_CHECK_ERR(merr, localrc);

  merr = pcomm->exchange_tags(node_tags, node_tags, nodes);
  MBMESH_CHECK_ERR(merr, localrc);

  Range elems;
  merr=this->mesh->get_entities_by_dimension(0, this->pdim, elems);
  MBMESH_CHECK_ERR(merr, localrc);

  merr = pcomm->exchange_tags(elem_tags, elem_tags, elems);
  MBMESH_CHECK_ERR(merr, localrc);

#ifdef DEBUG_WRITE_MESH
  {void *mbptr = (void *) this;
  int rc;
  int len = 12; char fname[len];
  sprintf(fname, "meshdual_%d", localPet);
  MBMesh_write(&mbptr, fname, &rc, len);}
#endif

#ifdef DEBUG_MOAB_GHOST_EXCHANGE
  // Repeat the reports, after ghost exchange
  shared_ents.clear();
  owned_entities.clear();
  merr = pcomm->get_shared_entities(-1, shared_ents);
  MBMESH_CHECK_ERR(merr, localrc);
  
  merr = pcomm->filter_pstatus(shared_ents, PSTATUS_NOT_OWNED, PSTATUS_NOT, -1, &owned_entities);
  MBMESH_CHECK_ERR(merr, localrc);
  
  // Find out how many shared entities of each dimension are owned on this processor
  for (int i = 0; i < 4; i++)
    // nums[i] = (int)shared_ents.num_of_dimension(i);
    nums[i] = (int)owned_entities.num_of_dimension(i);
  
  // Gather the statistics on processor 0
  MPI_Gather(nums, 4, MPI_INT, &rbuf[0], 4, MPI_INT, 0, mpi_comm);
  if (0 == rank) {
    cout << " \n\n After exchanging one ghost layer: \n";
    for (int i = 0; i < nprocs; i++) {
      cout << " Shared, owned entities on proc " << i << ": " << rbuf[4*i] << " verts, " <<
          rbuf[4*i + 1] << " edges, " << rbuf[4*i + 2] << " faces, " << rbuf[4*i + 3] << " elements" << endl;
    }
  }
#endif

}

#endif // ESMF_MOAB



