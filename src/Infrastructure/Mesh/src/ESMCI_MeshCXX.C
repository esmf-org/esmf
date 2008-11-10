//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
//------------------------------------------------------------------------------
// INCLUDES    
//------------------------------------------------------------------------------
#include "ESMCI_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"             // for LogErr
#include "ESMCI_F90Interface.h"

#include <Mesh/include/ESMCI_MeshCXX.h>

namespace ESMCI {

MeshCXX::MeshCXX() : Mesh(){
}
MeshCXX::~MeshCXX(){
}
MeshCXX* MeshCXX::create( int *pdim, int *sdim, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "MeshCXX::create()"

   MeshCXX* meshp;
   try {
    int localrc;

    //Initialize return code
    localrc = ESMF_SUCCESS;

    if (*pdim > *sdim) throw;

    meshp = new MeshCXX();

    (meshp)->set_parametric_dimension(*pdim);
    (meshp)->set_spatial_dimension(*sdim);

    ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));

    
    *rc = localrc;
   } catch(...) {
     *rc = ESMF_FAILURE;
   }
   return meshp;
} // MeshCXX::create
 

int MeshCXX::addElements(int *numElems, int *elemId, 
                         int *elemType, int *elemConn){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::MeshCXX::addElements()"

   int localrc;
   //Initialize localrc; assume routine not implemented
   localrc = ESMC_RC_NOT_IMPL;
   

   return localrc;
} // MeshCXX::addElements


int MeshCXX::addNodes(int *numNodes, int *nodeId, double *nodeCoord,
                      int *nodeOwner){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::MeshCXX::addNodes()"

   int localrc;
   //Initialize localrc; assume routine not implemented
   localrc = ESMC_RC_NOT_IMPL;


   return localrc;
} // MeshCXX::addNodes


int MeshCXX::createDistGrids(int *numLNodes, int *numLElems){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::MeshCXX::createDistGrids()"

   int localrc;
   //Initialize localrc; assume routine not implemented
   localrc = ESMC_RC_NOT_IMPL;

   // Local variables
   int* ngrids;
   int* egrids;

   return localrc;
} // MeshCXX::createDistGrids


int MeshCXX::destroy(){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::MeshCXX::destroy()"

   int localrc;
   //Initialize localrc; assume routine not implemented
   localrc = ESMC_RC_NOT_IMPL;
  
   return localrc;
} // MeshCXX::destroy


int MeshCXX::freeMemory(){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::MeshCXX::freeMemory()"
// Free Mesh memory, but leave the rest of MeshCXX members intact.

   int localrc;
   //Initialize localrc; assume routine not implemented
   localrc = ESMC_RC_NOT_IMPL;

   return localrc;
} // MeshCXX::freeMemory


} // namespace ESMCI
