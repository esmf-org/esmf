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
#include "Mesh/include/ESMCI_Raster.h"


//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id$";
//-----------------------------------------------------------------------------
using namespace ESMCI;




// INPUTS: 
//
// OUTPUTS:
//   out_mesh - the new mesh created from the raster
//   rc       - the return code
//
void ESMCI_mesh_create_from_raster(Grid *raster_grid,
                                   Array *raster_array,
                                   InterArray<int> *raster_mask_values,
                                   Mesh **out_mesh,
                                   int *rc) {
                                   
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI_mesh_create_from_raster()"

  // Init out mesh
  *out_mesh=NULL;

  
  // Try-catch block around main part of method
  try {
    // local return code
    int localrc;

    printf("In mesh create from raster!! \n");
    
    // Convert Raster information into Mesh create information
    int orig_sdim;
    int pdim;
    ESMC_CoordSys_Flag coordSys;
    int num_nodes;
    int *node_ids;
    double *node_coords;
    int *node_owners;
    int num_elems;
    int *elem_ids;
    int *elem_num_conns;
    int *elem_conns;
    ESMCI_raster_to_mesh_create_info(raster_grid,
                                     raster_array,
                                     raster_mask_values,
                                     pdim, orig_sdim, coordSys,
                                     num_nodes,
                                     node_ids,
                                     node_coords,
                                     node_owners,
                                     num_elems,
                                     elem_ids,
                                     elem_num_conns,
                                     elem_conns);

    // Debug output
    printf("MCFR: pdim=%d\n",pdim);
    printf("MCFR: orig_sdim=%d\n",orig_sdim);
    printf("MCFR: coordSys=%d\n",coordSys);
    printf("MCFR: num_nodes=%d\n",num_nodes);
    printf("MCFR: num_elems=%d\n",num_elems);

    // Debug output node coords
    // for (int i=0; i< num_nodes; i++) {
    //  printf("%d ids=%d  coords=%f %f \n",i,node_ids[i],node_coords[2*i],node_coords[2*i+1]);
    //}

    
    // Create mesh object
    Mesh *mesh;
    ESMCI_meshcreate(&mesh,
                     &pdim, &orig_sdim, &coordSys, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;

    // Wrap node owners in an InterArray
    InterArray<int> node_owners_IA(node_owners,num_nodes);

    // Add nodes
    ESMCI_meshaddnodes(&mesh,
                       &num_nodes, node_ids, node_coords, &node_owners_IA, NULL,
                       &coordSys, &orig_sdim, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;
    

    // Free node info
    delete [] node_ids;
    delete [] node_coords;
    delete [] node_owners;


    // Calculate total number of connections
    int tot_elem_num_conns=0;
    for (auto i=0; i<num_elems; i++) {
      tot_elem_num_conns += elem_num_conns[i];
    }

    // Add elements
    int areaPresent=0;
    int elemCoordsPresent=0;
    ESMCI_meshaddelements(&mesh,
                          &num_elems, elem_ids, elem_num_conns, NULL,
                          &areaPresent, NULL,
                          &elemCoordsPresent, NULL,
                          &tot_elem_num_conns, elem_conns, 
                          &coordSys, &orig_sdim, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;

      
    // Free elem info
    delete [] elem_ids;
    delete [] elem_num_conns;
    delete [] elem_conns;

    
    // Not finished yet
    //    Throw() << "Not finished yet!!";
    

    // Return final mesh
    *out_mesh=mesh;

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
      "Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

  // We've gotten to bottom successfully, so return success
  if(rc != NULL) *rc = ESMF_SUCCESS;
}

