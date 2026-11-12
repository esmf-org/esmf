// $Id$
//
// Earth System Modeling Framework
// Copyright (c) 2002-2026, University Corporation for Atmospheric Research,
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
#include <vector>
#include <utility>
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
#include "Mesh/include/ESMCI_FileIO_Util.h"
#include "Mesh/include/ESMCI_ESMFMesh_Util.h"
#include "Mesh/include/ESMCI_UGRID_Util.h"
#include "IO/include/ESMC_IOScrip2ESMF.h"

#ifdef ESMF_PNETCDF
# define _PNETCDF
#include <pnetcdf.h>
# elif ESMF_NETCDF
# define _NETCDF
# include <netcdf.h>
#endif

// Only use if PIO is available
#ifdef ESMF_PIO
#include <pio.h>
#include "IO/include/ESMCI_PIO_Handler.h"
#endif

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id$";
//-----------------------------------------------------------------------------
using namespace ESMCI;

// These internal functions will only be used if PIO is avaiable
#ifdef ESMF_PIO

// Prototypes of per format mesh creates from below
void ESMCI_mesh_create_from_ESMFMesh_file(int pioSystemDesc,
                                          char *filename, 
                                          bool add_user_area, 
                                          ESMC_CoordSys_Flag coord_sys, 
                                          ESMCI::DistGrid *elem_distgrid, 
                                          Mesh **out_mesh);

void ESMCI_mesh_create_from_UGRID_file(int pioSystemDesc,
                                       char *filename, 
                                       bool add_user_area, 
                                       ESMC_CoordSys_Flag coord_sys, 
                                       ESMC_MeshLoc_Flag maskFlag, 
                                       char *maskVarName,
                                       ESMCI::DistGrid *elem_distgrid, 
                                       Mesh **out_mesh);

void ESMCI_mesh_create_from_SCRIP_file(int pioSystemDesc,
                                       char *filename, 
                                       bool add_user_area, 
                                       ESMC_CoordSys_Flag coord_sys, 
                                       ESMCI::DistGrid *elem_distgrid, 
                                       Mesh **out_mesh);

void ESMCI_mesh_create_redist_mesh(Mesh *in_mesh, 
                                   ESMCI::DistGrid *node_distgrid, 
                                   ESMCI::DistGrid *elem_distgrid, 
                                   Mesh **out_mesh);
#endif // ifdef ESMF_PIO




// INPUTS: 
//  filename - file name in NULL delimited form
//  fileformat - the format of the file
//  convert_to_dual - specifies if mesh should be converted to dual before returning
//  add_user_area - specifies if areas should be added to mesh. 
//  coord_sys - The coordsys to create the mesh with. If ESMC_COORDSYS_UNINIT, then 
//              use the one from the file.
//  node_distgrid - If not NULL, redist so nodes are on this distgrid
//  elem_distgrid - If not NULL, redist so elems are on this distgrid
//
// OUTPUTS:
//   out_mesh - the new mesh created from the file
//   rc       - the return code
//
void ESMCI_mesh_create_from_file(char *filename, 
                                 ESMC_FileFormat_Flag fileformat, 
                                 bool convert_to_dual, bool add_user_area, 
                                 ESMC_CoordSys_Flag coord_sys, 
                                 ESMC_MeshLoc_Flag maskFlag, 
                                 char *maskVarName,
                                 ESMCI::DistGrid *node_distgrid, 
                                 ESMCI::DistGrid *elem_distgrid, 
                                 Mesh **out_mesh, int *rc){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI_mesh_create_from_file()"

// Will only work if PIO is available
#ifdef ESMF_PIO
  
  //  printf("in new scalable mesh create from file filename=%s\n",filename);


  // Try-catch block around main part of method
  try {
    // local return code
    int localrc;


    //// Error checking of arguments
    //// (More is done in format specific methods) 

    // Can't convert to dual and add_user_areas
    // (Conversion to dual swaps elements and nodes and nodes don't have areas)
    if (convert_to_dual && add_user_area) {
      if (ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                        "Can't simultaneously use areas from file in mesh and convert the mesh to its dual.",
                                        ESMC_CONTEXT, &localrc)) throw localrc;
    }




    //// Set up PIO

    // Get VM 
    ESMCI::VM *vm=VM::getCurrent(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      &localrc)) throw localrc;
    
    // Get VM info
    int local_pet = vm->getLocalPet();  
    MPI_Comm mpi_comm = vm->getMpi_c();  
    int pet_count = vm->getPetCount();
    int pets_per_Ssi = vm->getSsiMaxPetCount();

    // Initialize IO system
    int num_iotasks = pet_count/pets_per_Ssi;
    int stride = pets_per_Ssi;
    int pioSystemDesc;
    int piorc;

    piorc = PIOc_Init_Intracomm(mpi_comm, num_iotasks, stride, 0, PIO_REARR_SUBSET, &pioSystemDesc);
    if (!CHECKPIOERROR(piorc, std::string("Unable to init PIO Intracomm for file: ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;


    // Since we are swapping nodes and elems in convert_to_dual, 
    // can't use elem_distgrid to read from file in that case. 
    ESMCI::DistGrid *elem_distgrid_for_file_read=NULL;
    if (!convert_to_dual) elem_distgrid_for_file_read=elem_distgrid;


    // Create Mesh based on the file format
    Mesh *tmp_mesh;
    if (fileformat == ESMC_FILEFORMAT_ESMFMESH) {
      ESMCI_mesh_create_from_ESMFMesh_file(pioSystemDesc, filename, 
                                           add_user_area, coord_sys, 
                                           elem_distgrid_for_file_read, 
                                           &tmp_mesh);

    } else if (fileformat == ESMC_FILEFORMAT_UGRID) {
      ESMCI_mesh_create_from_UGRID_file(pioSystemDesc, filename, 
                                        add_user_area, coord_sys, 
                                        maskFlag, maskVarName,
                                        elem_distgrid_for_file_read, 
                                        &tmp_mesh);

    } else if (fileformat == ESMC_FILEFORMAT_SCRIP) {
      ESMCI_mesh_create_from_SCRIP_file(pioSystemDesc, filename, 
                                        add_user_area, coord_sys, 
                                        elem_distgrid_for_file_read, 
                                        &tmp_mesh);
    } else {
      if (ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
         " Unrecognized file format.",
           ESMC_CONTEXT, &localrc)) throw localrc;
    }    



    // Free IO system
    piorc = PIOc_free_iosystem(pioSystemDesc);
    if (!CHECKPIOERROR(piorc, std::string("Error freeing pio file system description "),
                      ESMF_RC_FILE_OPEN, localrc)) throw localrc;;



    // If requested, create dual from read in file
    if (convert_to_dual) {

      // Create dual from tmp_mesh created from file above
      Mesh *dual_mesh;
      ESMCI_meshcreatedual(&tmp_mesh, &dual_mesh, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                        &localrc)) throw localrc;
      
      // Get rid of tmp_mesh
      ESMCI_meshdestroy(&tmp_mesh, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                        &localrc)) throw localrc;
      
      // Swap in dual mesh
      tmp_mesh=dual_mesh;
    }


    // If requested, redist to final distribution
    // (Don't need to redist if just elem_distgrid, because
    //  that happens during the reads (if not convert to dual))
    if ((node_distgrid != NULL) ||
        ((elem_distgrid != NULL) && convert_to_dual)) {

      Mesh *redist_mesh;
      ESMCI_mesh_create_redist_mesh(tmp_mesh, 
                                    node_distgrid, 
                                    elem_distgrid, 
                                    &redist_mesh);
      
      // Get rid of tmp_mesh
      ESMCI_meshdestroy(&tmp_mesh, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                        &localrc)) throw localrc;
      
      // Swap in redist mesh
      tmp_mesh=redist_mesh;
    }


    // Return final mesh
    *out_mesh=tmp_mesh;
    

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

#else
     ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT,
      "This functionality requires ESMF to be built with the PIO library enabled." ,
      ESMC_CONTEXT, rc);
#endif
}

// These internal functions will only be used if PIO is available
#ifdef ESMF_PIO


// This method checks to see if optional pole info is in the file, and
// if it is, then it uses it to mark the pole edges in the passed in mesh
void ESMCI_mesh_mark_poles_from_ESMFMesh_file(int pioFileDesc, char *filename, Mesh *mesh) {
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI_mesh_mark_poles_from_ESMFMesh_file()"

  // Handy declarations
  int localrc;

  // Check if original grid rank is there, if it is, get it
  bool has_origGridRank;
  PIO_Offset origGridRank;
  get_origGridRank_from_ESMFMesh_file(pioFileDesc, filename, has_origGridRank, origGridRank);

  // If it's present, then get the dims
  if (has_origGridRank) {

    // We only handle rank 2 right now
    if (origGridRank != 2) {
      if (ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                        " Only original grid rank 2 is currently supported.",
                                        ESMC_CONTEXT, &localrc)) throw localrc;
    }

    // Get original grid dims
    bool has_origGridDims;
    int origGridDims[ESMF_MAXDIM];
    get_origGridDims_from_ESMFMesh_file(pioFileDesc, filename, has_origGridDims, origGridDims);

    // If has original grid dims, then mark poles
    if (has_origGridDims) {

      // Declare pole info variables
      int pole_val;
      int pole_obj_type;
      int min_pole_gid;
      int max_pole_gid;

      // Setup pole info for bottom pole
      pole_val=4;
      pole_obj_type=1; // Set elements
      min_pole_gid=1;
      max_pole_gid=origGridDims[0];
 
      // Mark bottom pole
      ESMCI_meshsetpoles(&mesh, &pole_obj_type, &pole_val, &min_pole_gid, &max_pole_gid, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                        &localrc)) throw localrc;

      // Setup pole info for top pole
      pole_val=5;
      pole_obj_type=1; // Set elements
      min_pole_gid=origGridDims[0]*origGridDims[1]-origGridDims[0]+1;
      max_pole_gid=origGridDims[0]*origGridDims[1];
 
      // Mark bottom pole
      ESMCI_meshsetpoles(&mesh, &pole_obj_type, &pole_val, &min_pole_gid, &max_pole_gid, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                        &localrc)) throw localrc;

    }
  }
}




//
// Create a Mesh from an ESMFMesh format file
//
// INPUTS: 
//  filename - file name in NULL delimited form
//  add_user_area - specifies if areas should be added to mesh. 
//  coord_sys - The coordsys to create the mesh with. If ESMC_COORDSYS_UNINIT, then 
//              use the one from the file.
//  elem_distgrid - If not NULL, redist so elems are on this distgrid
//
// OUTPUTS:
//   out_mesh - the new mesh created from the file
//
void ESMCI_mesh_create_from_ESMFMesh_file(int pioSystemDesc,
                                          char *filename, 
                                          bool add_user_area, 
                                          ESMC_CoordSys_Flag coord_sys, 
                                          ESMCI::DistGrid *elem_distgrid, 
                                          Mesh **out_mesh){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI_mesh_create_from_ESMFMesh_file()"

  // Declare some handy variables
  int localrc;
  int rc;
  int piorc;

  // Init output
  *out_mesh=NULL;


  // Try-catch block around main part of method
  try {

    //// Open file via PIO

    // Set pio_type based on what's available
#ifdef ESMF_PNETCDF
    int pio_type = PIO_IOTYPE_PNETCDF;
#else
    int pio_type = PIO_IOTYPE_NETCDF;
#endif
    // the return from this call is the previous setting of error handling
    // a non-zero value does not indicate an error
    piorc = PIOc_Set_IOSystem_Error_Handling(pioSystemDesc, PIO_BCAST_ERROR);

    // Open file
    int pioFileDesc;
    int mode = 0;
    piorc = PIOc_openfile(pioSystemDesc, &pioFileDesc, &pio_type, filename, mode);
    // if the file was created with netcdf4, it cannot be opened with pnetcdf
    if (piorc == PIO_EINVAL || piorc == PIO_ENOTBUILT){
        pio_type = PIO_IOTYPE_NETCDF;
        piorc = PIOc_openfile(pioSystemDesc, &pioFileDesc, &pio_type, filename, mode);
    }
    if (!CHECKPIOERROR(piorc, std::string("Unable to open existing file: ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;


    //// Get VM Info

    // Get VM 
    ESMCI::VM *vm=VM::getCurrent(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      &localrc)) throw localrc;
    
    // Get info
    int local_pet = vm->getLocalPet();  
    int pet_count = vm->getPetCount();



    //// Get information about element distribution 

    // Get global elementCount
    PIO_Offset elementCount;
    int dimid;
    get_elementCount_from_ESMFMesh_file(pioFileDesc, filename, elementCount);

    // Get positions at which to read element information
    std::vector<int> elem_ids_vec;
    if (elem_distgrid == NULL) {
      get_ids_divided_evenly_across_pets(elementCount, local_pet, pet_count, elem_ids_vec);
    } else {
      // Have elem_distgrid, so get ids from that
      get_ids_from_distgrid(elem_distgrid, elem_ids_vec);
    }

    // Assign vector info to pointer, because PIO and mesh calls don't accept vectors
    int num_elems=0;
    int *elem_ids=NULL;
    if (!elem_ids_vec.empty()) {
      num_elems=elem_ids_vec.size();
      elem_ids=&elem_ids_vec[0];
    } 



    //// Create Mesh 

    // Get coordsys from file
    ESMC_CoordSys_Flag coord_sys_file;
    get_coordsys_from_ESMFMesh_file(pioFileDesc, filename, coord_sys_file);

    // Decide which coord_sys the mesh should be created with
    ESMC_CoordSys_Flag coord_sys_mesh;
    if (coord_sys == ESMC_COORDSYS_UNINIT) {
      coord_sys_mesh = coord_sys_file;
    } else {
      coord_sys_mesh = coord_sys;
    }


    // Get coordDim
    PIO_Offset coordDim;
    get_coordDim_from_ESMFMesh_file(pioFileDesc, filename, coordDim);

    // Convert file coordDim into pdim and orig_sdim to use in mesh create
    int pdim, orig_sdim;
    if (coordDim == 2) {
      pdim=orig_sdim=2;
    } else if (coordDim == 3) {
      pdim=orig_sdim=3;
    } else {
      Throw() << "Meshes can only be created with dim=2 or 3.";
    }

    // Create Mesh    
    ESMCI_meshcreate(out_mesh,
                     &pdim, &orig_sdim, &coord_sys_mesh, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      &localrc)) throw localrc;



    //// Add nodes to Mesh

    // Get element connection info
    int totNumElementConn=0;
    int *numElementConn=NULL, *elementConn=NULL;
    get_elemConn_info_from_ESMFMesh_file(pioSystemDesc, pioFileDesc, filename, elementCount, num_elems, elem_ids, 
                                         totNumElementConn, numElementConn, elementConn);

    // Convert global elem info into node info
    int num_nodes;
    int *node_ids=NULL;
    int *local_elem_conn=NULL;
    convert_global_elem_conn_to_local_node_and_elem_info(num_elems, totNumElementConn, numElementConn, elementConn,
                                                          num_nodes, node_ids, local_elem_conn);

    // Convert numElementsConn to elementTypes
    int *elementType=NULL;
    convert_numElementConn_to_elementType(pdim, num_elems, numElementConn, elementType);

    // Free element connection info, because we don't need it any more
    delete [] numElementConn;
    delete [] elementConn;

    // Get global nodeCount
    PIO_Offset nodeCount;
    get_nodeCount_from_ESMFMesh_file(pioFileDesc, filename, nodeCount);


    // Get nodeCoords at positions indicated by node_ids
    double *nodeCoords;
    get_nodeCoords_from_ESMFMesh_file(pioSystemDesc, pioFileDesc, filename, 
                                      nodeCount, coordDim, 
                                      num_nodes, node_ids, 
                                      nodeCoords);

    // If file in different coordinate system than mesh, convert
    if (coord_sys_file != coord_sys_mesh) {
      convert_coords_between_coord_sys(coord_sys_file, coord_sys_mesh, 
                                       coordDim, num_nodes, nodeCoords);
    }

    // Get nodeMask
    // (If not present in file, nodeMask variable will be NULL)
    int *nodeMask=NULL;
    get_nodeMask_from_ESMFMesh_file(pioSystemDesc, pioFileDesc, filename, 
                                    nodeCount, 
                                    num_nodes, node_ids, 
                                    nodeMask);


    // Set up InterArray variable for nodeMask
    // Note that present() for InterArray checks if the array is NULL, so it works to just create the InterArray and pass that
    InterArray<int> nodeMaskIA(nodeMask, num_nodes);

    // Add nodes
    ESMCI_meshaddnodes(out_mesh, &num_nodes, node_ids,
                       nodeCoords, NULL, &nodeMaskIA,
                       &coord_sys_mesh, &orig_sdim,
                       &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      &localrc)) throw localrc;

    // Get rid of things used for adding nodes
    delete [] node_ids;
    delete [] nodeCoords;
    if (nodeMask != NULL) delete [] nodeMask;



    //// Add elements to Mesh and finish it up

    // Get elementMask
    // (If not present in file, elementMask variable will be NULL)
    int *elementMask=NULL;
    get_elementMask_from_ESMFMesh_file(pioSystemDesc, pioFileDesc, filename, 
                                       elementCount, 
                                       num_elems, elem_ids, 
                                       elementMask);

    // Set up InterArray variable for elementMask
    // Note, that present() for InterArray checks if the array is NULL, so it works to just create the InterArray and pass that
    InterArray<int> elementMaskIA(elementMask, num_elems);


    // Get elementArea, if requested and if present in file
    double *elementArea=NULL;
    int areaPresent=0;
    if (add_user_area) {
      get_elementArea_from_ESMFMesh_file(pioSystemDesc, pioFileDesc, filename, 
                                         elementCount, 
                                         num_elems, elem_ids, 
                                         areaPresent, elementArea);
    }


    // Get centerCoords, if present in file
    double *centerCoords=NULL;
    int centerCoordsPresent=0;
    get_centerCoords_from_ESMFMesh_file(pioSystemDesc, pioFileDesc, filename, 
                                        elementCount, coordDim, 
                                        num_elems, elem_ids, 
                                        centerCoordsPresent, centerCoords);

    // If center coords exist and
    //  file in different coordinate system than mesh, convert
    if ((centerCoords != NULL) && (coord_sys_file != coord_sys_mesh)) {
      convert_coords_between_coord_sys(coord_sys_file, coord_sys_mesh, 
                                       coordDim, num_elems, centerCoords);
    }
    

    // Add elements
    ESMCI_meshaddelements(out_mesh,
                          &num_elems, elem_ids, elementType,
                          &elementMaskIA,
                          &areaPresent, elementArea,
                          &centerCoordsPresent, centerCoords, 
                          &totNumElementConn, local_elem_conn, 
                          &coord_sys_mesh, &orig_sdim, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      &localrc)) throw localrc;

    // Free things used for element creation
    delete [] elementType;
    if (elementMask != NULL) delete [] elementMask;
    if (elementArea != NULL) delete [] elementArea;
    if (centerCoords != NULL) delete [] centerCoords;
    delete [] local_elem_conn;


    //// Mark poles (if information is in the file)
    ESMCI_mesh_mark_poles_from_ESMFMesh_file(pioFileDesc, filename, *out_mesh);


    //// Close file using PIO
    piorc = PIOc_closefile(pioFileDesc);
    if (!CHECKPIOERROR(piorc, std::string("Error closing file ") + filename,
                      ESMF_RC_FILE_OPEN, localrc)) throw localrc;;



  } catch(std::exception &x) {

    // catch Mesh exception return code
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                          x.what(), ESMC_CONTEXT,&rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                          "UNKNOWN", ESMC_CONTEXT,&rc);
    }
    throw rc; // To be caught one level up so we know where the error came from

  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,&rc);
    throw rc; // To be caught one level up so we know where the error came from

  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught unknown exception", ESMC_CONTEXT, &rc);
    throw rc; // To be caught one level up so we know where the error came from
  }
}

//
// Get from a UGRID file the edges which are a side of at least one of the elements
// local to this PET. (This means that, like nodes, an edge on the boundary between
// PETs will end up on each of the PETs that have an element which contains it.)
//
// In 2D the sides of an element are given by consecutive nodes in its connection
// list. In 3D the edges of an element depend on its type, so tables giving the
// edges of the supported types (hexahedrons and tetrahedrons) are used.
//
// INPUTS:
//  edgeConn_id    - the id of the edge connection variable in the file
//  pdim           - the parametric dimension of the mesh (2 or 3)
//  num_elems      - the number of elements on this PET
//  numElementConn - the number of connections for each element on this PET
//  elementConn    - the element connections in terms of global node ids (may
//                   contain MESH_POLYBREAK_IND)
//
// OUTPUTS:
//  edge_ids       - the global ids (1-based position in the file) of the edges
//                   on this PET, in ascending order
//  edge_node_ids  - the global ids of the 2 nodes of each of the edges in
//                   edge_ids, in the order in which they appear in the file
//                   (i.e. the orientation of the edge in the file is preserved)
//
static void get_local_edges_from_UGRID_file(int pioFileDesc, char *filename, int edgeConn_id,
                                            int pdim,
                                            int num_elems, int *numElementConn, int *elementConn,
                                            std::vector<int> &edge_ids,
                                            std::vector<int> &edge_node_ids) {
#undef ESMC_METHOD
#define ESMC_METHOD "get_local_edges_from_UGRID_file()"

  // The edges of the supported 3D element types in terms of (0-based) positions
  // in the element's connection list. These follow the ESMF node ordering in
  // which the first 4 nodes of a hexahedron go around one face and the last 4
  // go around the opposite face, with node 5 across from node 1, etc.
  static const int hex_edges[12][2]={{0,1},{1,2},{2,3},{3,0},
                                     {4,5},{5,6},{6,7},{7,4},
                                     {0,4},{1,5},{2,6},{3,7}};
  static const int tet_edges[6][2]={{0,1},{1,2},{2,0},
                                    {0,3},{1,3},{2,3}};

  // Init output
  edge_ids.clear();
  edge_node_ids.clear();

  //// Construct a sorted list of the sides (edges in 3D) of the elements on this PET
  //// (Each side is stored with the smaller node id first, so that it can be
  ////  matched with an edge irrespective of the edge's orientation.)
  std::vector<std::pair<int,int> > sides;
  for (int e=0, pos=0; e<num_elems; e++) {

    // Number of connections in this elem
    int num_conn=numElementConn[e];

    if (pdim == 2) {

      // Loop through the polygons that make up this elem
      // (There is usually just 1, but there can be more if there are polygon breaks)
      int poly_beg=0;
      for (int j=0; j<=num_conn; j++) {

        // If not at the end of a polygon, then keep going
        if ((j < num_conn) && (elementConn[pos+j] != MESH_POLYBREAK_IND)) continue;

        // Add the sides of the polygon that just ended
        int num_poly_nodes=j-poly_beg;
        for (int k=0; k<num_poly_nodes; k++) {
          int n1=elementConn[pos+poly_beg+k];
          int n2=elementConn[pos+poly_beg+(k+1)%num_poly_nodes];
          if (n1 == n2) continue; // skip degenerate sides
          if (n1 < n2) sides.push_back(std::make_pair(n1,n2));
          else sides.push_back(std::make_pair(n2,n1));
        }

        // Next polygon starts after break
        poly_beg=j+1;
      }

    } else if (pdim == 3) {

      // Get the table of edges for this type of elem
      int num_elem_edges=0;
      const int (*elem_edges)[2]=NULL;
      if (num_conn == 8) {
        num_elem_edges=12;
        elem_edges=hex_edges;
      } else if (num_conn == 4) {
        num_elem_edges=6;
        elem_edges=tet_edges;
      } else {
        Throw() << "Unrecognized 3D element type with "<<num_conn<<" nodes.";
      }

      // Add the edges of the elem
      for (int k=0; k<num_elem_edges; k++) {
        int n1=elementConn[pos+elem_edges[k][0]];
        int n2=elementConn[pos+elem_edges[k][1]];
        if (n1 == n2) continue; // skip degenerate edges
        if (n1 < n2) sides.push_back(std::make_pair(n1,n2));
        else sides.push_back(std::make_pair(n2,n1));
      }

    } else {
      Throw() << "Meshes can only be created with dim=2 or 3.";
    }

    // Advance to next elem
    pos += num_conn;
  }

  // Sort and get rid of repeats (interior sides will be there twice)
  std::sort(sides.begin(), sides.end());
  sides.erase(std::unique(sides.begin(), sides.end()), sides.end());


  //// Loop through the edges in the file in blocks keeping the ones that match a side
  //// Note that this has to be done even if there are no sides on this PET,
  //// because the reads are collective.

  // Get global edgeCount
  PIO_Offset edgeCount;
  get_edgeCount_from_UGRID_file(pioFileDesc, filename, edgeConn_id, edgeCount);

  // Max number of edges to read at one time
  const PIO_Offset max_block_size=1048576;

  // Allocate buffer to hold a block
  PIO_Offset buff_size=(edgeCount < max_block_size) ? edgeCount : max_block_size;
  std::vector<int> block_edgeConn(2*buff_size+1); // +1 so never 0 sized

  // Loop through blocks
  for (PIO_Offset block_beg=0; block_beg<edgeCount; block_beg += max_block_size) {

    // Size of this block
    PIO_Offset block_size=edgeCount-block_beg;
    if (block_size > max_block_size) block_size=max_block_size;

    // Read block
    get_edgeConn_block_from_UGRID_file(pioFileDesc, filename, edgeConn_id,
                                       block_beg, block_size, &block_edgeConn[0]);

    // Keep the edges that are a side of a local elem
    for (PIO_Offset i=0; i<block_size; i++) {
      int n1=block_edgeConn[2*i];
      int n2=block_edgeConn[2*i+1];

      std::pair<int,int> side = (n1 < n2) ? std::make_pair(n1,n2) : std::make_pair(n2,n1);

      if (std::binary_search(sides.begin(), sides.end(), side)) {
        edge_ids.push_back((int)(block_beg+i+1)); // +1 to make 1-based
        edge_node_ids.push_back(n1);
        edge_node_ids.push_back(n2);
      }
    }
  }
}


//
// Create a Mesh from a UGRID format file
//
// INPUTS: 
//  filename - file name in NULL delimited form.
//  add_user_area - specifies if areas should be added to mesh. 
//  coord_sys - The coordsys to create the mesh with. If ESMC_COORDSYS_UNINIT, then 
//              use the one from the file.
//  maskFlag  - gives the location of the mask.
//  maskVarName - gives the name of the variable from which to create the mask.
//  elem_distgrid - If not NULL, redist so elems are on this distgrid
//
// OUTPUTS:
//   out_mesh - the new mesh created from the file
//
void ESMCI_mesh_create_from_UGRID_file(int pioSystemDesc,
                                       char *filename, 
                                       bool add_user_area, 
                                       ESMC_CoordSys_Flag coord_sys, 
                                       ESMC_MeshLoc_Flag maskFlag, 
                                       char *maskVarName,
                                       ESMCI::DistGrid *elem_distgrid, 
                                       Mesh **out_mesh){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI_mesh_create_from_UGRID_file()"

  // Declare some handy variables
  int localrc;
  int rc;
  int piorc;

  // Init output
  *out_mesh=NULL;

  // Try-catch block around main part of method
  try {

    //// Open file via PIO

    // Set pio_type based on what's available
#ifdef ESMF_PNETCDF
    int pio_type = PIO_IOTYPE_PNETCDF;
#else
    int pio_type = PIO_IOTYPE_NETCDF;
#endif

    piorc = PIOc_Set_IOSystem_Error_Handling(pioSystemDesc, PIO_BCAST_ERROR);
    // piorc in this case is the previous setting of ERROR HANDLER, it's value does not indicate an error
    //if (!CHECKPIOERROR(piorc, std::string("Unable to set PIO error handling for file: ") + filename,
    //                   ESMF_RC_FILE_OPEN, localrc)) throw localrc;

    // Open file
    int pioFileDesc;
    int mode = 0;
    piorc = PIOc_openfile(pioSystemDesc, &pioFileDesc, &pio_type, filename, mode);
    // if the file was created with netcdf4, it cannot be opened with pnetcdf
    if (piorc == PIO_EINVAL){
        pio_type = PIO_IOTYPE_NETCDF;
        piorc = PIOc_openfile(pioSystemDesc, &pioFileDesc, &pio_type, filename, mode);
    }
    if (!CHECKPIOERROR(piorc, std::string("Unable to open existing file: ") + filename,
                       ESMF_RC_FILE_OPEN, localrc)) throw localrc;


    //// Get VM Info

    // Get VM 
    ESMCI::VM *vm=VM::getCurrent(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      &localrc)) throw localrc;
    
    // Get info
    int local_pet = vm->getLocalPet();  
    int pet_count = vm->getPetCount();


    // Get id of mesh topo variable
    int mesh_topo_id;
    get_mesh_topo_id_from_UGRID_file(pioFileDesc, filename, mesh_topo_id);


    // Get dim of mesh in file
    // (This dim is both pdim and orig_sdim)
    int dim;
    get_dim_from_UGRID_file(pioFileDesc, filename, mesh_topo_id, dim);


    // Get the element connection array id from the file
    int elementConn_id;
    get_elementConn_id_from_UGRID_file(pioFileDesc, filename, mesh_topo_id, dim, elementConn_id);


    //// Get information about element distribution 

    // Get global elementCount
    PIO_Offset elementCount;
    get_elementCount_from_UGRID_file(pioFileDesc, filename, elementConn_id, elementCount);

    // Get positions at which to read element information
    std::vector<int> elem_ids_vec;
    if (elem_distgrid == NULL) {
      get_ids_divided_evenly_across_pets(elementCount, local_pet, pet_count, elem_ids_vec);
    } else {
      // Have elem_distgrid, so get ids from that
      get_ids_from_distgrid(elem_distgrid, elem_ids_vec);
    }

    // Assign vector info to pointer, because PIO and mesh calls don't accept vectors
    int num_elems=0;
    int *elem_ids=NULL;
    if (!elem_ids_vec.empty()) {
      num_elems=elem_ids_vec.size();
      elem_ids=&elem_ids_vec[0];
    } 


    //// Create Mesh 

    // Get the variable ids of the node coord variables
    int nodeCoord_ids[ESMF_MAXDIM];
    get_nodeCoord_ids_from_UGRID_file(pioFileDesc, filename, mesh_topo_id, dim, nodeCoord_ids);    

    // Get coordsys from file
    ESMC_CoordSys_Flag coord_sys_file=ESMC_COORDSYS_SPH_DEG;
    get_coordsys_from_UGRID_file(pioFileDesc, filename, dim, nodeCoord_ids, coord_sys_file);

    // Decide which coord_sys the mesh should be created with
    ESMC_CoordSys_Flag coord_sys_mesh;
    if (coord_sys == ESMC_COORDSYS_UNINIT) {
      coord_sys_mesh = coord_sys_file;
    } else {
      coord_sys_mesh = coord_sys;
    }

    // Convert mesh dim from file into pdim and orig_sdim to use in mesh create
    int pdim, orig_sdim;
    if (dim == 2) {
      pdim=orig_sdim=2;
    } else if (dim == 3) {
      pdim=orig_sdim=3;
    } else {
      Throw() << "Meshes can only be created with dim=2 or 3.";
    }

    // Create Mesh    
    ESMCI_meshcreate(out_mesh,
                     &pdim, &orig_sdim, &coord_sys_mesh, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      &localrc)) throw localrc;



    //// Add nodes to Mesh

    // Get element connection info
    int totNumElementConn=0;
    int *numElementConn=NULL, *elementConn=NULL;
    get_elementConn_info_from_UGRID_file(pioSystemDesc, pioFileDesc, filename, elementConn_id, 
                                      elementCount, num_elems, elem_ids, 
                                      totNumElementConn, numElementConn, elementConn);


    // Convert global elem info into node info
    int num_nodes;
    int *node_ids=NULL;
    int *local_elem_conn=NULL;
    convert_global_elem_conn_to_local_node_and_elem_info(num_elems, totNumElementConn, numElementConn, elementConn,
                                                          num_nodes, node_ids, local_elem_conn);

    // Convert numElementsConn to elementType
    int *elementType=NULL;
    convert_numElementConn_to_elementType(pdim, num_elems, numElementConn, elementType);

    // Get the edges on this PET, if edges are present in the file
    // (This needs to be done while we still have the global element connection info.)
    int edgesPresent=0;
    int edgeConn_id;
    std::vector<int> edge_ids_vec;
    std::vector<int> edge_node_ids_vec;
    get_edgeConn_id_from_UGRID_file(pioFileDesc, filename, mesh_topo_id, edgeConn_id, edgesPresent);
    if (edgesPresent) {
      get_local_edges_from_UGRID_file(pioFileDesc, filename, edgeConn_id,
                                      pdim,
                                      num_elems, numElementConn, elementConn,
                                      edge_ids_vec, edge_node_ids_vec);
    }

    // Free element connection info, because we don't need it any more
    delete [] numElementConn;
    delete [] elementConn;

    // Get global nodeCount
    PIO_Offset nodeCount;
    get_nodeCount_from_UGRID_file(pioFileDesc, filename, dim, nodeCoord_ids, nodeCount);

    // Get nodeCoords at positions indicated by node_ids
    double *nodeCoords=NULL;
    get_coords_from_UGRID_file(pioSystemDesc, pioFileDesc, filename, 
                               coord_sys_file, dim, nodeCoord_ids, 
                               nodeCount, 
                               num_nodes, node_ids, 
                               nodeCoords);
    

    // If file in different coordinate system than mesh, convert
    if (coord_sys_file != coord_sys_mesh) {
      convert_coords_between_coord_sys(coord_sys_file, coord_sys_mesh, 
                                       dim, num_nodes, nodeCoords);
    }


    // Get nodeMask
    // (If not present in file, nodeMask variable will be NULL)
    int *nodeMask=NULL;
    if (maskFlag == ESMC_MESHLOC_NODE) {
      get_mask_from_UGRID_file(pioSystemDesc, pioFileDesc, filename, 
                               maskVarName, 
                               nodeCount, 
                               num_nodes, node_ids, 
                               nodeMask);
    }

    // Set up InterArray variable for nodeMask
    // Note that present() for InterArray checks if the array is NULL, so it works to just create the InterArray and pass that
    InterArray<int> nodeMaskIA(nodeMask, num_nodes);


    // Add nodes
    ESMCI_meshaddnodes(out_mesh, &num_nodes, node_ids,
                       nodeCoords, NULL, &nodeMaskIA,
                       &coord_sys_mesh, &orig_sdim,
                       &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      &localrc)) throw localrc;

    //// Add edges to Mesh (if present in file)
    if (edgesPresent) {

      // Number of edges on this PET
      int num_edges=edge_ids_vec.size();

      // Use dummy variables when there aren't any edges on this PET, so that
      // there are still valid pointers to pass into the calls below
      int dummy_int=0;
      double dummy_double=0.0;
      int *edge_ids=&dummy_int;
      if (num_edges > 0) edge_ids=&edge_ids_vec[0];

      // Convert the global node ids of the edge connections into 1-based indices into
      // the list of nodes added above (which is what's expected by the mesh).
      // node_ids is sorted, so this can be done using a binary search. 
      int edgeConn_size=2*num_edges;
      int *edgeConn=&dummy_int;
      if (num_edges > 0) edgeConn=&edge_node_ids_vec[0];
      for (int i=0; i<edgeConn_size; i++) {
        int *pos=std::lower_bound(node_ids, node_ids+num_nodes, edgeConn[i]);
        if ((pos == node_ids+num_nodes) || (*pos != edgeConn[i])) {
          Throw() << "Node id "<<edgeConn[i]<<" in edge connection not found in list of nodes on this PET.";
        }
        edgeConn[i]=(int)(pos-node_ids)+1; // +1 to make 1-based
      }

      // Get edgeCoords, if present in file
      double *edgeCoords=NULL;
      int edgeCoord_ids[ESMF_MAXDIM];
      int edgeCoordsPresent=0;
      get_edgeCoord_ids_from_UGRID_file(pioFileDesc, filename, mesh_topo_id,
                                        dim, edgeCoord_ids, edgeCoordsPresent);
      if (edgeCoordsPresent) {

        // Get global edgeCount
        PIO_Offset edgeCount;
        get_edgeCount_from_UGRID_file(pioFileDesc, filename, edgeConn_id, edgeCount);

        // Get edgeCoords at positions indicated by edge_ids
        get_coords_from_UGRID_file(pioSystemDesc, pioFileDesc, filename,
                                   coord_sys_file, dim, edgeCoord_ids,
                                   edgeCount,
                                   num_edges, edge_ids,
                                   edgeCoords);

        // If edge coords exist and
        // file in different coordinate system than mesh, convert
        if ((edgeCoords != NULL) && (coord_sys_file != coord_sys_mesh)) {
          convert_coords_between_coord_sys(coord_sys_file, coord_sys_mesh,
                                           dim, num_edges, edgeCoords);
        }
      }

      // Set up InterArray variables for optional edge info
      // Note that present() for InterArray checks if the array is NULL
      // Owners aren't in the file, so pass NULL to have them calculated.
      InterArray<int> edgeOwnersIA((int *)NULL, 0);

      // If the file has edge coords pass a non-NULL pointer even if there aren't any edges
      // on this PET, so that the presence of edge coords is the same across PETs.
      double *edgeCoordsArg=edgeCoords;
      if (edgeCoordsPresent && (edgeCoordsArg == NULL)) edgeCoordsArg=&dummy_double;
      int edgeCoords_size=0;
      if (edgeCoordsPresent) edgeCoords_size=num_edges*orig_sdim;
      InterArray<ESMC_R8> edgeCoordsIA(edgeCoordsArg, edgeCoords_size);

      // Add edges
      ESMCI_meshaddedges(out_mesh, &num_edges, edge_ids,
                         &edgeOwnersIA, &edgeCoordsIA,
                         &edgeConn_size, edgeConn,
                         &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                        &localrc)) throw localrc;

      // Get rid of things used for adding edges
      if (edgeCoords != NULL) delete [] edgeCoords;
    }


    // Get rid of things used for adding nodes
    delete [] node_ids;
    delete [] nodeCoords;
    if (nodeMask != NULL) delete [] nodeMask;



    //// Add elements to Mesh and finish it up

    // Get elementMask
    // (If not present in file, elementMask variable will be NULL)
    int *elementMask=NULL;
    if (maskFlag == ESMC_MESHLOC_ELEMENT) {
      get_mask_from_UGRID_file(pioSystemDesc, pioFileDesc, filename, 
                                    maskVarName, 
                                    elementCount, 
                                    num_elems, elem_ids, 
                                    elementMask);
    }

    // Set up InterArray variable for elementMask
    // Note, that present() for InterArray checks if the array is NULL, so it works to just create the InterArray and pass that
    InterArray<int> elementMaskIA(elementMask, num_elems);

    // Get elementArea, if requested and if present in file
    double *elementArea=NULL;
    int areaPresent=0;
    if (add_user_area) {
      // User areas are not supported in UGRID format
      ESMC_LogDefault.Write("UGRID format does not support element areas, so no user areas will be read or used.",ESMC_LOGMSG_WARN);
    }


    // Get centerCoords, if present in file
    double *centerCoords=NULL;
    int elemCoord_ids[ESMF_MAXDIM];
    int centerCoordsPresent=0;
    get_elemCoord_ids_from_UGRID_file(pioFileDesc, filename, mesh_topo_id, 
                                      dim, elemCoord_ids, centerCoordsPresent);
    if (centerCoordsPresent) {
      get_coords_from_UGRID_file(pioSystemDesc, pioFileDesc, filename, 
                                 coord_sys_file, dim, elemCoord_ids, 
                                 elementCount, 
                                 num_elems, elem_ids, 
                                 centerCoords);
      
      // If center coords exist and
      // file in different coordinate system than mesh, convert
      if ((centerCoords != NULL) && (coord_sys_file != coord_sys_mesh)) {
        convert_coords_between_coord_sys(coord_sys_file, coord_sys_mesh, 
                                         dim, num_elems, centerCoords);
      }
    }
    

    // Add elements
    ESMCI_meshaddelements(out_mesh,
                          &num_elems, elem_ids, elementType,
                          &elementMaskIA,
                          &areaPresent, elementArea,
                          &centerCoordsPresent, centerCoords, 
                          &totNumElementConn, local_elem_conn, 
                          &coord_sys_mesh, &orig_sdim, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      &localrc)) throw localrc;

    // Free things used for element creation
    if (elementMask != NULL) delete [] elementMask;
    if (elementArea != NULL) delete [] elementArea;
    if (centerCoords != NULL) delete [] centerCoords;
    delete [] local_elem_conn;


    //// Close file using PIO
    piorc = PIOc_closefile(pioFileDesc);
    if (!CHECKPIOERROR(piorc, std::string("Error closing file ") + filename,
                      ESMF_RC_FILE_OPEN, localrc)) throw localrc;;

    //    Throw() << "Not finished with UGRID yet!";


  } catch(std::exception &x) {

    // catch Mesh exception return code
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                          x.what(), ESMC_CONTEXT,&rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                          "UNKNOWN", ESMC_CONTEXT,&rc);
    }
    throw rc; // To be caught one level up so we know where the error came from

  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,&rc);
    throw rc; // To be caught one level up so we know where the error came from

  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught unknown exception", ESMC_CONTEXT, &rc);
    throw rc; // To be caught one level up so we know where the error came from
  }
}

//
// Create a Mesh from a SCRIP format file
//
// INPUTS: 
//  filename - file name in NULL delimited form
//  add_user_area - specifies if areas should be added to mesh. 
//  coord_sys - The coordsys to create the mesh with. If ESMC_COORDSYS_UNINIT, then 
//              use the one from the file.
//  elem_distgrid - If not NULL, redist so elems are on this distgrid
//
// OUTPUTS:
//   out_mesh - the new mesh created from the file
//
void ESMCI_mesh_create_from_SCRIP_file(int pioSystemDesc,
                                      char *filename, 
                                      bool add_user_area, 
                                      ESMC_CoordSys_Flag coord_sys, 
                                      ESMCI::DistGrid *elem_distgrid, 
                                      Mesh **out_mesh){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI_mesh_create_from_SCRIP_file()"

  // Declare some handy variables
  int localrc;
  int rc;
  int piorc;

  // Init output
  *out_mesh=NULL;


  // Try-catch block around main part of method
  try {


    // Get VM 
    ESMCI::VM *vm=VM::getCurrent(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      &localrc)) throw localrc;
    
    // Get VM info
    int local_pet = vm->getLocalPet();  
    int pet_count = vm->getPetCount();

    // ESMF Mesh format file name
    char *esmfmesh_filename=".esmf.nc";

    // Call into serial conversion subroutine on PET 0 
    // to convert SCRIP to ESMF Mesh format
    if (local_pet == 0) {
      int dualflag=0; // Don't do dual
      ESMCI_convert_SCRIP_to_ESMFMesh(filename,
                                      esmfmesh_filename,
                                      &dualflag,
                                      &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                        &localrc)) throw localrc;
    }

    // Wait for conversion to finish
    vm->barrier();

    // Call into ESMFMesh format read to create mesh from converted file
    ESMCI_mesh_create_from_ESMFMesh_file(pioSystemDesc,
                                         esmfmesh_filename, 
                                         add_user_area, 
                                         coord_sys, 
                                         elem_distgrid, 
                                         out_mesh);

    // Wait for read to finish
    vm->barrier();
    
    // Get rid of converted file on PET 0
    if (local_pet == 0) {
      int ret=remove(esmfmesh_filename);
      if (ret != 0) {
        if (ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                          "Error deleting temporary file.",
                                          ESMC_CONTEXT, &localrc)) throw localrc;        
      }
    }

  } catch(std::exception &x) {

    // catch Mesh exception return code
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                          x.what(), ESMC_CONTEXT,&rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                          "UNKNOWN", ESMC_CONTEXT,&rc);
    }
    throw rc; // To be caught one level up so we know where the error came from

  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,&rc);
    throw rc; // To be caught one level up so we know where the error came from

  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught unknown exception", ESMC_CONTEXT, &rc);
    throw rc; // To be caught one level up so we know where the error came from
  }
}

void ESMCI_mesh_create_redist_mesh(Mesh *in_mesh, 
                                   ESMCI::DistGrid *node_distgrid, 
                                   ESMCI::DistGrid *elem_distgrid, 
                                   Mesh **out_mesh){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI_mesh_create_redist_mesh()"

  // Return codess
  int localrc, rc;

  // Try-catch block around main part of method
  try {

    // Get VM 
    ESMCI::VM *vm=VM::getCurrent(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      &localrc)) throw localrc;
    
    // Get VM info
    int local_pet = vm->getLocalPet();  
    int pet_count = vm->getPetCount();


    // Get node ids from distgrid (if present)
    int num_nodes=0;
    int *node_ids=NULL;
    std::vector<int> node_ids_vec;
    if (node_distgrid != NULL) {      

      // Get node ids from distgrid
      get_ids_from_distgrid(node_distgrid, node_ids_vec);

      // Get array to pass into redist code
      if (!node_ids_vec.empty()) {
        num_nodes=node_ids_vec.size();
        node_ids=&node_ids_vec[0];
      } 
    }


    // Get elem ids from distgrid (if present)
    int num_elems=0;
    int *elem_ids=NULL;
    std::vector<int> elem_ids_vec;
    if (elem_distgrid != NULL) {
      
      // Get elem ids from distgrid
      get_ids_from_distgrid(elem_distgrid, elem_ids_vec);
      
      // Get elem array to pass into redist code
      if (!elem_ids_vec.empty()) {
        num_elems=elem_ids_vec.size();
        elem_ids=&elem_ids_vec[0];
      } 
    }


    // Redist based on which disgrids are present
    if (node_distgrid != NULL) {      
      if (elem_distgrid != NULL) {
        
        // Both node and elem distgrids present
        ESMCI_meshcreateredist(&in_mesh, 
                               &num_nodes, node_ids,
                               &num_elems, elem_ids,  
                               out_mesh, &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                          &localrc)) throw localrc;        
      } else {
        
        // Only node distgrid present
        ESMCI_meshcreateredistnodes(&in_mesh, 
                                    &num_nodes, node_ids,
                                    out_mesh, &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                          &localrc)) throw localrc;
      }
    } else {
      if (elem_distgrid != NULL) {
                
        // Only elem distgrid present
        ESMCI_meshcreateredistelems(&in_mesh, 
                                    &num_elems, elem_ids,
                                    out_mesh, &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                          &localrc)) throw localrc;
        
      } else {
        // Neither present, so nothing to do  
      }
    }

    
  } catch(std::exception &x) {

    // catch Mesh exception return code
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                          x.what(), ESMC_CONTEXT,&rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                          "UNKNOWN", ESMC_CONTEXT,&rc);
    }
    throw rc; // To be caught one level up so we know where the error came from

  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,&rc);
    throw rc; // To be caught one level up so we know where the error came from

  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught unknown exception", ESMC_CONTEXT, &rc);
    throw rc; // To be caught one level up so we know where the error came from
  }

}

#endif // ifdef ESMF_PIO
