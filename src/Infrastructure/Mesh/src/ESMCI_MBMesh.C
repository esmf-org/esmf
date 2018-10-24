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

#include "moab/ParallelComm.hpp"
#include "MBParallelConventions.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
// static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


using namespace ESMCI;
using namespace std;

#define ESMF
#define ESMC_METHOD "MBMesh::func()"

MBMesh::MBMesh() : sdim(0),pdim(0),mesh(NULL),verts(NULL) {

} 

MBMesh::~MBMesh() {

  // Get rid of MOAB mesh
  if (mesh != NULL) delete mesh;

  // Get rid of list of verts
  if (verts != NULL) delete [] verts;
} 

void MBMesh::CreateGhost() {

  int merr, localrc;

  Interface *mb = this->mesh;

  // Do this for now instead of initiating mesh parallel stuff
  // TODO: MAYBE EVENTUALLY PUT THIS INTO MBMesh???
  MPI_Comm mpi_comm;
  mpi_comm=VM::getCurrent(&localrc)->getMpi_c();
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

  // Get the ParallelComm instance
  ParallelComm* pcomm = new ParallelComm(mb, mpi_comm);
  int nprocs = pcomm->proc_config().proc_size();
  int rank = pcomm->proc_config().proc_rank();

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
    nums[i] = (int)owned_entities.num_of_dimension(i);
  
  vector<int> rbuf(nprocs*4, 0);
  MPI_Gather(nums, 4, MPI_INT, &rbuf[0], 4, MPI_INT, 0, mpi_comm);
  // Print the stats gathered:
  if (0 == rank) {
    for (int i = 0; i < nprocs; i++)
      cout << " Shared, owned entities on proc " << i << ": " << rbuf[4*i] << " verts, " <<
          rbuf[4*i + 1] << " edges, " << rbuf[4*i + 2] << " faces, " << rbuf[4*i + 3] << " elements" << endl;
  }

  // Now exchange 1 layer of ghost elements, using vertices as bridge
  // (we could have done this as part of reading process, using the PARALLEL_GHOSTS read option)
  merr = pcomm->exchange_ghost_cells(3, // int ghost_dim
                                     0, // int bridge_dim
                                     1, // int num_layers
                                     0, // int addl_ents
                                     true);// bool store_remote_handles
  MBMESH_CHECK_ERR(merr, localrc);


  // Repeat the reports, after ghost exchange
  shared_ents.clear();
  owned_entities.clear();
  merr = pcomm->get_shared_entities(-1, shared_ents);
  MBMESH_CHECK_ERR(merr, localrc);

  merr = pcomm->filter_pstatus(shared_ents, PSTATUS_NOT_OWNED, PSTATUS_NOT, -1, &owned_entities);
  MBMESH_CHECK_ERR(merr, localrc);

  // Find out how many shared entities of each dimension are owned on this processor
  for (int i = 0; i < 4; i++)
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


}

#endif // ESMF_MOAB



