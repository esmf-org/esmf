// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
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

#include <string>
#include <ostream>
#include <iterator>
#include <algorithm>

#include "ESMCI_Macros.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"
#include "ESMCI_CoordSys.h"
#include "ESMCI_Array.h"
#include "ESMC_Util.h"

#include "ESMCI_TraceMacros.h"  // for profiling

#include "Mesh/include/ESMCI_Mesh.h"
#include "Mesh/include/Legacy/ESMCI_MeshRead.h"
#include "Mesh/include/Regridding/ESMCI_MeshRegrid.h" //only for the conservative flag in add_elements
#include "Mesh/include/Legacy/ESMCI_MeshVTK.h"
#include "Mesh/include/Legacy/ESMCI_ParEnv.h"
#include "Mesh/include/Legacy/ESMCI_MeshUtils.h"
#include "Mesh/include/Legacy/ESMCI_GlobalIds.h"
#include "Mesh/include/ESMCI_MeshRedist.h"
#include "Mesh/include/ESMCI_MeshDual.h"
#include "Mesh/include/ESMCI_Mesh_Glue.h"
#include "IO/include/ESMCI_PIO_Handler.h"

#ifdef ESMF_PNETCDF
# define _PNETCDF
#include <pnetcdf.h>
# elif ESMF_NETCDF
# define _NETCDF
# include <netcdf.h>
#endif
#include <pio.h>
//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id$";
//-----------------------------------------------------------------------------
using namespace ESMCI;

struct NODE_INFO {
  int node_id;
  int local_elem_conn_pos;

  bool operator< (const NODE_INFO &rhs) const {
    return node_id < rhs.node_id;
  }

};


// Note that local_elem_conn is base-1 (as expected by mesh create routines)
void convert_global_elem_conn_to_local_node_and_elem_info(int num_local_elem, int tot_num_elem_conn, int *num_elem_conn, int *global_elem_conn,
                                                                  int &num_node, int*& node_ids, int*& local_elem_conn) {
  // Init output
  num_node=0;
  node_ids=NULL;
  local_elem_conn=NULL;

  // If nothing to do leave
  if (tot_num_elem_conn < 1) return;

  // Allocate conversion list
  NODE_INFO *convert_list=new NODE_INFO[tot_num_elem_conn];

  // Copy global elem connection info into conversion list
  for (int i=0; i<tot_num_elem_conn; i++) {
    convert_list[i].node_id=global_elem_conn[i];
    convert_list[i].local_elem_conn_pos=i;
  }

  // Sort list by node_id, to make it easy to find unique node_ids
  std::sort(convert_list,convert_list+tot_num_elem_conn);

  // Count number of unique node ids in  convert_list
  int num_unique_node_ids=1;                 // There has to be at least 1, 
  int prev_node_id=convert_list[0].node_id;  // because we leave if < 1 above
  for (int i=1; i<tot_num_elem_conn; i++) {

    // If not the same as the last one count a new one
    if (convert_list[i].node_id != prev_node_id) {
      num_unique_node_ids++;
      prev_node_id=convert_list[i].node_id;
    }
  }

  // Allocate node_ids
  node_ids=new int[num_unique_node_ids];

  // Set output number of nodes
  num_node=num_unique_node_ids;

  // Allocate local elem conn
  local_elem_conn=new int[tot_num_elem_conn];

  // Translate convert_list to node_ids and local_elem_conn
  int node_ids_pos=0;                             // There has to be at least 1, 
  node_ids[node_ids_pos]=convert_list[0].node_id; // because we leave if < 1 above
  local_elem_conn[convert_list[0].local_elem_conn_pos]=node_ids_pos+1; // +1 to make base-1
  for (int i=1; i<tot_num_elem_conn; i++) {

    // If not the same as the last one add a new one
    if (convert_list[i].node_id != node_ids[node_ids_pos]) {
      node_ids_pos++;
      node_ids[node_ids_pos]=convert_list[i].node_id; 
    }

    // Add an entry for this in local_elem_conn
    local_elem_conn[convert_list[i].local_elem_conn_pos]=node_ids_pos+1; // +1 to make base-1
  }


  // Get rid of conversion list
  delete [] convert_list;    
}


// Divide num_ids as evenly as possible across pet_count pets, return min_id and max_id as the part 
// of the range on local_pet. Note that the ids start on 1, so the global range of ids is 1 to num_ids.
// If pet_count > num_ids, then the empty PETs will have min_id > max_id
void divide_ids_evenly_as_possible(int num_ids, int local_pet, int pet_count, int &min_id, int &max_id) {

  // Approx. number per PET
  int num_per_pet=num_ids/pet_count;
  
  // Remainder from even division
  int remainder=num_ids-num_per_pet*pet_count;

  // Figure out tentative range for this PET
  min_id=local_pet*num_per_pet+1;
  max_id=(local_pet+1)*num_per_pet;

  // Add in remainder (1 per PET) to bottom remainder PETs
  if (local_pet < remainder) min_id += local_pet;
  else min_id += remainder;

  if (local_pet < remainder) max_id += local_pet+1;
  else max_id += remainder;
}


// This gets a list of ids from a distgrid 
void get_ids_from_distgrid(ESMCI::DistGrid *distgrid, std::vector<int> &ids) {
#undef ESMC_METHOD
#define ESMC_METHOD "get_ids_from_distgrid()"

  // Sanity check
  ThrowRequire(distgrid != NULL);

  // Declare some useful variables
  int localrc;

  // Currently only support distgrids with 1 localDE
  if (distgrid->getDELayout()->getLocalDeCount() != 1) {
    if (ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                      " Currently only distgrids with 1 DE per PET are supported",
                                      ESMC_CONTEXT, &localrc)) throw localrc;
  }
  
  // Get seqIndexList
  // TODO: right now assumes 1 localDE, fix this
  localrc=distgrid->fillSeqIndexList(ids, 0);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                    &localrc)) throw localrc;
}



// This generates a list of ids divided as evenly as possible across pets and returns a 
// vector containing the list of the ones on the local pet.
void get_ids_divided_evenly_across_pets(int num_ids, int local_pet, int pet_count, std::vector<int> &ids) {
#undef ESMC_METHOD
#define ESMC_METHOD "get_ids_divided_evenly_across_pets()"

  // No distgrid provided so divide things up equally
  int min_id, max_id;
  divide_ids_evenly_as_possible(num_ids, local_pet, pet_count, min_id, max_id);
  
  //printf("%d# min,max ids=%d %d num=%d\n",local_pet,min_id,max_id,max_id-min_id+1);
  
  // Reserve space for ids
  ids.reserve(max_id-min_id+1);
  
  // Fill ids
  for (int id=min_id; id <= max_id; id++) {
    ids.push_back(id);
  }
}


