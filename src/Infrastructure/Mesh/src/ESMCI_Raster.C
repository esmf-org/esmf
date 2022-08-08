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
#define ESMC_FILENAME "ESMCI_Raster.C"
//==============================================================================
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// Create a mesh from a given grid.
//
//-----------------------------------------------------------------------------

#include "ESMCI_Grid.h"
#include "ESMCI_VM.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Ptypes.h"
#include "Mesh/include/ESMCI_Mesh.h"
#include "Mesh/include/Regridding/ESMCI_MeshRegrid.h"
#include "Mesh/include/Legacy/ESMCI_IOField.h"
#include "Mesh/include/Legacy/ESMCI_ParEnv.h"
#include "Mesh/include/Legacy/ESMCI_DDir.h"
#include "Mesh/include/ESMCI_MathUtil.h"
#include "Mesh/include/Legacy/ESMCI_Phedra.h"

#include <limits>
#include <iostream>
#include <vector>
#include <map>
#include <cmath>


// Some xlf compilers don't define this
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

//#define G2M_DBG

// using namespace ESMCI;

using namespace ESMCI;

using namespace std;


// TODO: Better name for Globber and Glob....

class Globber {

  class Glob {

  public:
    // Number of ids and ids in glob
    vector<int> num_ids;
    vector<int> ids;
    
  public:
    
    // Add another polygon to glob
    void add(int num_poly_ids, int *poly_ids) {
      
      // Add extra space for num_ids
      num_ids.reserve(num_ids.size()+1);
      
      // Add another count
      num_ids.push_back(num_poly_ids);
      
      // Add extra space for ids
      ids.reserve(ids.size()+num_poly_ids);
      
      // Add ids
      for (int i=0; i<num_poly_ids; i++) {
        ids.push_back(poly_ids[i]);
      }
    }
    
  };

  
  // Map to hold globs that map to the same value
  map<int, Glob*> glob_map;

public:
  
  // Add a new polygon representing a region filled with value
  void add(int value, int num_ids, int *ids) {

    // Get glob that matches value
    map<int,Glob *>::iterator vtgi = glob_map.find(value);
    
    // Either get the glob or add a new one
    Glob *glob;
    if (vtgi == glob_map.end()) {

      // Make a new glob
      glob = new Glob;

      // Add it to the list
      glob_map[value]=glob;

    } else {
      glob = vtgi-> second;
    }

    // Add ids to glob
    glob->add(num_ids, ids);    
  }

  void get_mesh_conn_info(int &num_nodes, int *&node_ids, int *&node_owners, int &num_elems, int *&elem_ids, int *&elem_num_conns, int *&elem_conns) {

    struct NODE_INFO {
      int node_id;
      int elem_conns_pos;
      
      bool operator< (const NODE_INFO &rhs) const {
        return node_id < rhs.node_id;
      }
      
    };


    // Iterators to loop through globs
    map<int,Glob *>::iterator gmi;
    map<int,Glob *>::iterator gme  = glob_map.end();
    
    
    // Init Output
    num_nodes=0;
    node_ids=NULL;
    node_owners=NULL;
    num_elems=0;
    elem_ids=NULL;
    elem_num_conns=NULL;
    elem_conns=NULL;

    // Get number of elems
    num_elems=glob_map.size();
    
    // Allocate space for elem ids
    elem_ids=new int[num_elems];

    // Fill elem ids
    // Right now is just value, eventually should have an option to generate
    // them independant of the value
    int id_pos=0;
    for (gmi  = glob_map.begin(); gmi != gme; gmi++) {
      elem_ids[id_pos]=gmi->first;
      id_pos++;
    }    
    
    // Allocate space for number of connections for each elem
    elem_num_conns=new int[num_elems];

    // Fill number of connections for each elem
    int tot_elem_num_conns=0;
    int nec_pos=0;
    for (gmi  = glob_map.begin(); gmi != gme; gmi++) {

      // Number of connetions is the total number of ids for this elem
      int num_conns_for_this_elem=gmi->second->ids.size();

      // Plus 1 polybreak indicator after each polygon except the last one (-1) 
      num_conns_for_this_elem+=gmi->second->num_ids.size()-1;

      // Add to total
      tot_elem_num_conns += num_conns_for_this_elem;
      
      // Add to list
      elem_num_conns[nec_pos]=num_conns_for_this_elem;

      // Next slot in array
      nec_pos++;
    }    

    
    // Count total number of connections
    int num_conn=0;
    for (gmi=glob_map.begin(); gmi != gme; gmi++) {
      num_conn += gmi->second->ids.size();
    }

    // If empty, leave
    if (num_conn < 1) return;

    // Allocate conversion list
    NODE_INFO *convert_list=new NODE_INFO[num_conn];

    // Copy ids to conversion list
    int conn_pos=0;
    int cl_pos=0;
    for (gmi=glob_map.begin(); gmi != gme; gmi++) {
      Glob *glob=gmi->second;
      
      // Loop through polygons
      int glob_pos=0;
      for (int p=0; p<glob->num_ids.size(); p++) {

        // Load polygon ids into list
        for (int i=0; i<glob->num_ids[p]; i++) {                
          convert_list[cl_pos].node_id=gmi->second->ids[glob_pos];
          convert_list[cl_pos].elem_conns_pos=conn_pos;
          cl_pos++;
          glob_pos++;
          conn_pos++;
        }

        // If not at end of glob, add an extra slot for polybreak indicator
        if (p < glob->num_ids.size()-1) conn_pos++;
      }
    }
  
  // Sort list by node_id, to make it easy to find unique node_ids
  std::sort(convert_list,convert_list+num_conn);

  // Count number of unique node ids in  convert_list
  int num_unique_node_ids=1;                 // There has to be at least 1, 
  int prev_node_id=convert_list[0].node_id;  // because we leave if < 1 above
  for (int i=1; i<num_conn; i++) {

    // If not the same as the last one count a new one
    if (convert_list[i].node_id != prev_node_id) {
      num_unique_node_ids++;
      prev_node_id=convert_list[i].node_id;
    }
  }

  // Allocate node_ids
  node_ids=new int[num_unique_node_ids];

  // Set output number of nodes
  num_nodes=num_unique_node_ids;

  // Allocate local elem conn
  elem_conns=new int[tot_elem_num_conns];
  
  // Set to polybreak value so that it's in the correct places
  // after the code below fills in the node connection values
  for (int i=0; i<tot_elem_num_conns; i++) {
    elem_conns[i]=MESH_POLYBREAK_IND;
  }
  
  // Translate convert_list to node_ids and elem_conns
  int node_ids_pos=0;                             // There has to be at least 1, 
  node_ids[node_ids_pos]=convert_list[0].node_id; // because we leave if < 1 above
  elem_conns[convert_list[0].elem_conns_pos]=node_ids_pos+1; // +1 to make base-1
  for (int i=1; i<num_conn; i++) {
    
    // If not the same as the last one add a new one
    if (convert_list[i].node_id != node_ids[node_ids_pos]) {
      node_ids_pos++;
      node_ids[node_ids_pos]=convert_list[i].node_id; 
    }
    
    // Add an entry for this in elem_conns
    elem_conns[convert_list[i].elem_conns_pos]=node_ids_pos+1; // +1 to make base-1
  }
  
  // Get rid of conversion list
  delete [] convert_list;

  // Allocate node_owners
  node_owners=new int[num_nodes];

  // For now just init to 0 (since we're just doing serial)
  for (auto i=0; i<num_nodes; i++) {
    node_owners[i]=0;
  }
  
}
  
};



#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_raster_to_mesh_create_info()"
  
  /* XMRKX */

// Some handy defines
#define NUM_QUAD_CORNERS 4

//// TODO: Merge the below methods with ones in GridToMesh() so we only have one copy of them!!!!!

  // Calculate index offsets for corners around a given center
  static void _calc_corner_offset(Grid *grid, int corner_offset[NUM_QUAD_CORNERS][2]) {
    int default_offset[NUM_QUAD_CORNERS][2]={{0,0},{1,0},{1,1},{0,1}};

    // Get Alignment for staggerloc
    const int *staggerAlign= grid->getStaggerAlign(ESMCI_STAGGERLOC_CORNER);

    // Get off set due to alignment
    int align_off[2];
    for (int i=0; i<2; i++) {
      if (staggerAlign[i] < 1) align_off[i]=0;
      else align_off[i]=-1;
    }

    // Change default offset to correspond with alignment
    for (int i=0; i<NUM_QUAD_CORNERS; i++) {
      for (int j=0; j<2; j++) {
        corner_offset[i][j]=default_offset[i][j]+align_off[j];
      }
    }
  }

  // Get global id
  // if there is one returns true, if there isn't returns false
#define BAD_ID -1

  // Calc global id given:
  // tile  - tile number (1,.., tileCount)
  // index - tile-specific absolute index tuple 
  static bool _get_global_id_from_tile(DistGrid *distgrid, int tile, int *index,
                                       int *_gid, bool *_is_local) {

    // determine sequence index
    std::vector<int> seqIndex;
    int localrc=distgrid->getSequenceIndexTile(tile,index,seqIndex);
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception

    // If there's just 1, then use that as the id, otherwise use the min id
    int gid=BAD_ID;
    bool is_local=false;
    if (seqIndex.size()==1) {
      gid = seqIndex[0];
      is_local=true;
    } else if (seqIndex.size() > 0) {
      // If there's a degenarcy then chose the min id
      gid= std::numeric_limits<int>::max();
      for (int i=0; i<seqIndex.size(); i++) {
        if (seqIndex[i]<gid) gid=seqIndex[i];
      }
      if (gid==seqIndex[0]) is_local=true;
    } else {
      // if it's 0 then return false
      return false;
    }

    // Do output
    *_gid=gid;
    *_is_local=is_local;
    return true;
  }

  static void _get_quad_corner_ids_from_tile(DistGrid *cnrDistgrid, int tile, int index[2], int cnr_offset[NUM_QUAD_CORNERS][2], int quad_cnr_ids[NUM_QUAD_CORNERS]) {

    // Loop getting global ids
    int cnr_gids[NUM_QUAD_CORNERS];
    for (int i=0; i<NUM_QUAD_CORNERS; i++) {
      
      // calc index of corner
      int cnr_index[2];
      cnr_index[0]=index[0]+cnr_offset[i][0];
      cnr_index[1]=index[1]+cnr_offset[i][1];
      
      // DEBUG
      //   if (gqcn_debug) {
      //  printf("%d# in _get_quad_corner_nodes BEFORE cnr_index=%d %d \n",Par::Rank(),cnr_index[0],cnr_index[1]);
      //}
      
      // Get global id, if we can't then leave
      bool is_local;
      if (!_get_global_id_from_tile(cnrDistgrid, tile, cnr_index,
                                    quad_cnr_ids+i, &is_local)) {
        Throw() << "Can't find global id for corner "<<i<<" of quad.";
      }
    }
  
  }


  static void _convert_localDE_to_tile_info(DistGrid *distgrid, int localDE, int *de_index, int *_tile, int *tile_index) {
    // Get DELayout
    DELayout *delayout=distgrid->getDELayout();
    
    // Get DE from localDE
    const int *localDeToDeMap = delayout->getLocalDeToDeMap();
    int DE=localDeToDeMap[localDE];

    // Get tile from DE
    int const *tileListPDe=distgrid->getTileListPDe();
    *_tile=tileListPDe[DE];
    if (*_tile<1) {
      Throw()<<"tile<1 which may indicate that localDE is empty and thus not on a tile.";
    }    

    // Get tile index from local DE index
    int dimCount=distgrid->getDimCount();
    int const *minIndexPDimPDe=distgrid->getMinIndexPDimPDe();
    for (int d=0; d<dimCount; d++) {
      tile_index[d] = minIndexPDimPDe[DE*dimCount+d] + de_index[d];
    }

#if 0
    // DO WE NEED TO SUPPORT NON-CONTIG DIMS? IF SO, USE THIS INSTEAD, BUT NEED
    // TO HAVE GT ADD A WAY TO GET pointer to indexListPDimPLocalDe 
    int const *contigFlagPDimPDe=distgrid->getContigFlagPDimPDe();
    for (int d=0; d<dimCount; d++) {
      if (contigFlagPDimPDe[DE*dimCount+d]) {
        tile_index[d] = minIndexPDimPDe[DE*dimCount+d] + index[d];
      } else {
        tile_index[d] = indexListPDimPLocalDe[localDE*dimCount+d][index[d]];
      }
    }
#endif

  }


  static void _get_quad_corner_ids_from_localDE(DistGrid *cnrDistgrid, int localDE, int de_index[2], int cnr_offset[NUM_QUAD_CORNERS][2], int quad_cnr_ids[NUM_QUAD_CORNERS]) {

    //// Translate localDE info into tile info ////
    int tile;
    int tile_index[ESMF_MAXDIM];
    _convert_localDE_to_tile_info(cnrDistgrid, localDE, de_index, &tile, tile_index);

    // Call into get get quad corner nodes from tile
    _get_quad_corner_ids_from_tile(cnrDistgrid, tile, tile_index, cnr_offset,
                                   quad_cnr_ids);
}


// Prototype for method that gets coords at seqinds in Grid
void get_grid_coords_at_seqinds(Grid *grid, int staggerloc, int num_seqinds, int *seqinds, double *seqind_coords);


  
 /* XMRKX */

  void ESMCI_raster_to_mesh_create_info(Grid *raster_grid,
                                        Array *raster_array,
                                        InterArray<int> *raster_mask_values,
                                        int &mesh_pdim, int &mesh_orig_sdim,
                                        ESMC_CoordSys_Flag &mesh_coordSys,
                                        int &mesh_num_nodes,
                                        int *&mesh_node_ids,
                                        double *&mesh_node_coords,
                                        int *&mesh_node_owners,
                                        int &mesh_num_elems,
                                        int *&mesh_elem_ids,
                                        int *&mesh_elem_num_conns,
                                        int *&mesh_elem_conns) {
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_raster_to_mesh_create_info()"
    
    try {
      // local error code
      int localrc;
      
      //      printf("In ESMCI_raster_to_mesh_create_info\n");


      // The Grid needs to have corner coordinates
      if (!raster_grid->hasCoordStaggerLoc(ESMCI_STAGGERLOC_CORNER)) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
           "To use this method the Grid must contain coordinates at corner staggerloc.", ESMC_CONTEXT, &localrc);
        throw localrc;
      }

      // The Grid currently can't be arbitrarily distributed
      if (raster_grid->getDecompType() != ESMC_GRID_NONARBITRARY) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                                      "To use this method the Grid can't be arbitrarily distributed.", ESMC_CONTEXT, &localrc);
        throw localrc;
      }

      // Get Grid dimCount
      int grid_dimCount=raster_grid->getDimCount();

      // Only supporting 2D right now
      if (grid_dimCount != 2) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                                      "This method currently only supports 2D Grids", ESMC_CONTEXT, &localrc);
        throw localrc;
      }


      // Currently only support I4
      if (raster_array->getTypekind() != ESMC_TYPEKIND_I4) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                                      "Currently only typekind ESMC_TYPEKIND_I4 is supported for raster Arrays.",
                                      ESMC_CONTEXT, &localrc);
        throw localrc;
      }


      
      // Get Mesh dimension and coordinate info
      mesh_orig_sdim=grid_dimCount;
      mesh_pdim=grid_dimCount;
      mesh_coordSys=raster_grid->getCoordSys();
      

      // Get distgrid for the center staggerloc
      DistGrid *centerDistgrid;
      raster_grid->getStaggerDistgrid(ESMCI_STAGGERLOC_CENTER, &centerDistgrid);
      
      // Get centerLocalDECount
      int centerLocalDECount=centerDistgrid->getDELayout()->getLocalDeCount();
 
      // Get distgrid for the corner staggerloc
      DistGrid *cnrDistgrid;
      raster_grid->getStaggerDistgrid(ESMCI_STAGGERLOC_CORNER, &cnrDistgrid);
      
      // Get cnrLocalDECount
      int cnrLocalDECount=cnrDistgrid->getDELayout()->getLocalDeCount();

      // Get index offsets for the corners around a center
      int cnr_offset[NUM_QUAD_CORNERS][2];
      _calc_corner_offset(raster_grid, cnr_offset);

      // DO I NEED THIS??
      // Get index offsets for the centers around a corner
      //      int center_offset[NUM_QUAD_CORNERS][2];
      //_calc_center_offset(cnr_offset, center_offset);

      // DEBUG OUTPUT
      // printf("offset=[%d %d] [%d %d]  [%d %d]  [%d %d]\n",cnr_offset[0][0],
      //cnr_offset[0][1],cnr_offset[1][0],cnr_offset[1][1],cnr_offset[2][0],cnr_offset[2][1],
      //cnr_offset[3][0], cnr_offset[3][1]);

      // Get raster mask information
      int num_mask_vals=0;
      int *mask_vals;
      if (present(raster_mask_values)) {
        
        // Error check mask values
        if (raster_mask_values->dimCount != 1) {
          Throw() << " Mask values must be of rank 1.";
        }
        
        // Get mask values
        num_mask_vals=raster_mask_values->extent[0];
        mask_vals=&(raster_mask_values->array[0]);
      }

      
      // Create structure to accumulate polygons and use them to create mesh connection info
      Globber globber;
      
      // Loop over center DEs adding cells
      for (int lDE=0; lDE < centerLocalDECount; lDE++) {

        // Get Center DE bounds
        int ubnd[ESMF_MAXDIM];
        int lbnd[ESMF_MAXDIM];
        raster_grid->getDistExclusiveUBound(centerDistgrid, lDE, ubnd);
        raster_grid->getDistExclusiveLBound(centerDistgrid, lDE, lbnd);
        
        // Get Corner DE bounds
        int cnr_ubnd[ESMF_MAXDIM];
        int cnr_lbnd[ESMF_MAXDIM];
        raster_grid->getDistExclusiveUBound(cnrDistgrid, lDE, cnr_ubnd);
        raster_grid->getDistExclusiveLBound(cnrDistgrid, lDE, cnr_lbnd);
        
        // DEBUG
        //        printf("%d# lDE=%d CENTER lbnd=%d %d ubnd=%d %d\n",Par::Rank(),lDE,lbnd[0],lbnd[1],ubnd[0],ubnd[1]);
        //printf("%d# lDE=%d CORNER lbnd=%d %d ubnd=%d %d\n",Par::Rank(),lDE,cnr_lbnd[0],cnr_lbnd[1],cnr_ubnd[0],cnr_ubnd[1]);

        // Get localArray for lDE
        LocalArray *localArray = raster_array->getLocalarrayList()[lDE];
        
        // Loop over bounds
        for (int i0=lbnd[0]; i0<=ubnd[0]; i0++){
          for (int i1=lbnd[1]; i1<=ubnd[1]; i1++){
            
            // Set index
            int index[2];
            index[0]=i0;
            index[1]=i1;
            
            // De based index
            int de_index[2];
            de_index[0]=i0-lbnd[0];
            de_index[1]=i1-lbnd[1];
            
            // Get corner ids for quad
            int quad_cnr_ids[NUM_QUAD_CORNERS];
            _get_quad_corner_ids_from_localDE(cnrDistgrid, lDE, de_index, cnr_offset,
                                              quad_cnr_ids);


            // Get value in array
            ESMC_I4 value;
            localArray->getDataInternal(index, &value);
            
            // DEBUG OUTPUT       
            // printf("elem [%d %d] value=%d corners = %d %d %d %d \n",i0,i1,value,quad_cnr_ids[0],quad_cnr_ids[1],quad_cnr_ids[2],quad_cnr_ids[3]);

            // Skip if masked
            bool masked=false;     
            for (int m=0; m<num_mask_vals; m++) {
              if (mask_vals[m] == value) {
                masked=true;
                break;
              }
            }
            if (masked) continue;
            
                 
            // Add to globber
            globber.add(value, NUM_QUAD_CORNERS, quad_cnr_ids);
            
          }
        }
      }


      // Get mesh info
      globber.get_mesh_conn_info(mesh_num_nodes, mesh_node_ids, mesh_node_owners,
                                 mesh_num_elems, mesh_elem_ids, mesh_elem_num_conns, mesh_elem_conns);

      // Allocate space for node coords
      mesh_node_coords=new double[mesh_orig_sdim*mesh_num_nodes];
      
      // Fill node coords from Grid
      get_grid_coords_at_seqinds(raster_grid, ESMCI_STAGGERLOC_CORNER, mesh_num_nodes, mesh_node_ids, mesh_node_coords);
      
/* XMRKX */
      
            
    } catch(std::exception &x) {
      // catch Mesh exception return code
      int rc;
      if (x.what()) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                      x.what(), ESMC_CONTEXT,&rc);
      } else {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                      "UNKNOWN", ESMC_CONTEXT,&rc);
      }
      
      // Throw to be caught one level up so we know where the error came from 
      throw rc; 
      
    }catch(int localrc){
      // catch standard ESMF return code
      int rc;
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc);
      
      // Throw to be caught one level up so we know where the error came from 
      throw rc;
      
    } catch(...){
      
      // catch standard anything else
      int rc;
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                    "Caught unknown exception", ESMC_CONTEXT, &rc);
      
      // Throw to be caught one level up so we know where the error came from 
      throw rc;    
    }
    
  }

// Go from a sequence indice in a distgrid to it's location in that distgrid in
// terms of pet, localde, index, etc.
// If not local, then localDE = -1
// NOTE: for efficiency's sake this method currently only works for 2D distgrids
void get_location_info_from_seqind_2D(int seqind, DistGrid *distgrid,
                                   int &pet, int &localDE, int *index) {

  // Local return code
  int localrc;
  
  // This method only supports 2D Distgrids
  if (distgrid->getDimCount() != 2) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                                  "This method only works with 2D Distgrids.",
                                  ESMC_CONTEXT, &localrc);
    throw localrc;
  }
 
  // Calc tile and index from gid
  int tile=0;
  std::vector<int> vec_index(2,-1);
  localrc=distgrid->getIndexTupleFromSeqIndex(seqind, vec_index, tile);
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

    // Get DeLayout
    DELayout *delayout=distgrid->getDELayout();

    // Get number of DE's
   int deCount=delayout->getDeCount();

   // Get de Index lists
   int const *minIndexPDimPDE=distgrid->getMinIndexPDimPDe();
   int const *maxIndexPDimPDE=distgrid->getMaxIndexPDimPDe();

   
    // Get tile
    const int *DETileList = distgrid->getTileListPDe();

    // Loop through DEs
    bool found_de=false;
    int de=-1;
    for (int d=0; d<deCount; d++) {

      // If de isn't on the correct tile, then go on to next
      if (tile != DETileList[d]) continue;

      // Get De minIndex
      int const *minIndex=minIndexPDimPDE+2*d;

      // Get De maxIndex
      int const *maxIndex=maxIndexPDimPDE+2*d;

      // If de is empty, then skip
      if ((maxIndex[0] < minIndex[0]) ||
          (maxIndex[1] < minIndex[1])) continue;


      // check if we're smaller than min
      if ((vec_index[0] < minIndex[0]) ||
          (vec_index[1] < minIndex[1])) continue;

      // check if we're bigger than max
      if ((vec_index[0] > maxIndex[0]) ||
          (vec_index[1] > maxIndex[1])) continue;

      // This point is on this de, so record de and exit loop
      de=d;
      found_de=true;
      break;
    }

    // if not found then, error
    if (!found_de) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                                    "Sequence index unexpectedly not found in Distgrid.",
                                    ESMC_CONTEXT, &localrc);
      throw localrc;
    }

    
    //// Return information

    // Get pet
    pet=delayout->getPet(de);

    // Get localDE (will be set to -1 if not local)
    const int *deList=delayout->getDeList();
    localDE=deList[de];
    
    // Get index
    index[0]=vec_index[0];
    index[1]=vec_index[1];
}


// This method takes a list of sequence inds (ids) and a corresponding array of doubles (id_coords). It then fills the
// array of doubles with coords from the grid locations indicated by the ids.
//
// NOTE: Currently only supports 2D Grids, but wouldn't be hard to expand to 3D
// 
// Inputs:
//   grid       - the grid to get the coordinates from
//   staggerloc - the staggerloc in the grid to get the coordinates from
//   num_seqinds    - the number of seqinds in ids
//   seqinds        - the list of sequence inds (of size num_seqinds)
// Outputs:
//    seqinds_coords - the output coordinates (of size num_seqinds * the coordinate dimension (i.e. dimCount) of the Grid)
//
void get_grid_coords_at_seqinds(Grid *grid, int staggerloc, int num_seqinds, int *seqinds, double *seqind_coords) {
#undef  ESMC_METHOD
#define ESMC_METHOD "get_grid_coords_at_seqinds()"
  
  // Declare some useful variables
  int localrc;

  // Get grid dimCount
  int dimCount=grid->getDimCount();  

  // Currently only supports 2D grids
  if (dimCount != 2) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                                  "This method currently only supports 2D Grids.",
                                  ESMC_CONTEXT, &localrc);
    throw localrc;
  }
  
  // Check if grid contains coordinates at the staggerloc
  if (!grid->hasCoordStaggerLoc(staggerloc)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
       "To use this method the Grid must contain coordinates at the requested staggerloc.", ESMC_CONTEXT, &localrc);
    throw localrc;
  }

  // Get distgrid at staggerloc
  DistGrid *distgrid=NULL;
  grid->getStaggerDistgrid(staggerloc, &distgrid);
  
  // Loop over points and fill
  for (int i=0; i<num_seqinds; i++) {

    // Translate seqind to proc, localde, and indices
    int pet=-1;
    int localDE=-1;
    int index[2]={-1,-1};
    get_location_info_from_seqind_2D(seqinds[i], distgrid,
                                     pet, localDE, index);

    // If local, get coordinates
    if (localDE > -1) {
      // pointer to coords for current seqind
      double *coords=seqind_coords+dimCount*i;

      // Get coords at this seqind
      localrc=grid->getCoordInternalConvert(staggerloc,
                                            localDE, index, coords);
      if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
        throw localrc;  // bail out with exception
      
    } else { // Until we have parallel, this is an error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                                    "This method can currently only get coordinates for local seqInds.", ESMC_CONTEXT, &localrc);
      throw localrc;
    }
  }
}



// NOT SURE IF I NEED THESE YET
#if 0



  static bool _get_global_id_from_localDE(DistGrid *distgrid, int localDE, int *index,
                             int *_gid, bool *_is_local) {

    //// Translate localDE info into tile info ////
    int tile;
    int tile_index[ESMF_MAXDIM];
    _convert_localDE_to_tile_info(distgrid, localDE, index, &tile, tile_index);

    // Call into get global id from tile
    return _get_global_id_from_tile(distgrid, tile, tile_index,
                                    _gid, _is_local);
  }




  //// Node Fields
#define GTOM_NFIELD_MASK 0
#define GTOM_NFIELD_MASK_VAL 1
#define GTOM_NFIELD_COORD 2
#define GTOM_NFIELD_ORIG_COORD 3
#define GTOM_NFIELD_NUM 4
  
  static void create_nfields(Grid *grid, Mesh *mesh,
                             IOField<NodalField> *nfields[GTOM_NFIELD_NUM]) {

    // Init field array to null
    for (int i=0; i<GTOM_NFIELD_NUM; i++) {
      nfields[i]=NULL;
    }

    // Masks
   if (grid->hasItemStaggerLoc(ESMCI_STAGGERLOC_CORNER, ESMC_GRIDITEM_MASK)) {
     nfields[GTOM_NFIELD_MASK] = mesh->RegisterNodalField(*mesh, "mask", 1);
     nfields[GTOM_NFIELD_MASK_VAL] = mesh->RegisterNodalField(*mesh, "node_mask_val", 1);
   }

    // Coords
    if (grid->hasCoordStaggerLoc(ESMCI_STAGGERLOC_CORNER)) {
                                           
      // Add node coord field
      nfields[GTOM_NFIELD_COORD]=mesh->RegisterNodalField(*mesh, "coordinates", mesh->spatial_dim()); 

      // If not cartesian then add original coordinates field
      if (grid->getCoordSys() != ESMC_COORDSYS_CART) {
        nfields[GTOM_NFIELD_ORIG_COORD]=mesh->RegisterNodalField(*mesh, "orig_coordinates", grid->getDimCount()); 
      }      
    } else {
      int localrc;
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
         "- Grid does not contain coordinates at staggerloc=ESMF_STAGGERLOC_CORNER. ", ESMC_CONTEXT, &localrc);
      throw localrc;
    }

  }


  // Set node fields in mesh
  static void set_nfields(Grid *grid, Mesh *mesh,
                          IOField<NodalField> *nfields[GTOM_NFIELD_NUM]) {
    int localrc;

    // Get distgrid for the center staggerloc
    DistGrid *cnrDistgrid;
    grid->getStaggerDistgrid(ESMCI_STAGGERLOC_CORNER, &cnrDistgrid);

    // Get localDECount
    int localDECount=cnrDistgrid->getDELayout()->getLocalDeCount();
                       
    // Loop again adding information to nodes
    for (int lDE=0; lDE < localDECount; lDE++) {

      // Get Corner DE bounds
      int cnr_ubnd[ESMF_MAXDIM];
      int cnr_lbnd[ESMF_MAXDIM];
      grid->getDistExclusiveUBound(cnrDistgrid, lDE, cnr_ubnd);
      grid->getDistExclusiveLBound(cnrDistgrid, lDE, cnr_lbnd);


      // Loop over bounds
      int index[2];
      int nonde_index[2];
      for (int i0=cnr_lbnd[0]; i0<=cnr_ubnd[0]; i0++){
        for (int i1=cnr_lbnd[1]; i1<=cnr_ubnd[1]; i1++){

          // Set index
          index[0]=i0;
          index[1]=i1;

          // De based index
          int de_index[2];
          de_index[0]=i0-cnr_lbnd[0];
          de_index[1]=i1-cnr_lbnd[1];

          // Get node global id
          int node_gid;
          bool is_local;
          if (!_get_global_id_from_localDE(cnrDistgrid, lDE, de_index,
                              &node_gid, &is_local)) {
            continue; // If we can't find a global id, then just skip
          }

          // Only set node information if this is the owner
          if (!is_local) continue;

          // Get associated node
          Mesh::MeshObjIDMap::iterator mi =  mesh->map_find(MeshObj::NODE, node_gid);

          // If it doesn't exist then go to next
          if (mi == mesh->map_end(MeshObj::NODE)) {
            continue;
          }

          // Get node 
          MeshObj &node=*mi;

          // Mask
          // Init to 0 (set later in ESMF_FieldRegridStore()
          if (nfields[GTOM_NFIELD_MASK]) {
            double *d=nfields[GTOM_NFIELD_MASK]->data(node);
            *d=0.0;
          }

          // Mask Val
          // Get data from grid
          if (nfields[GTOM_NFIELD_MASK_VAL]) {
            double *d=nfields[GTOM_NFIELD_MASK_VAL]->data(node);
            localrc=grid->getItemInternalConvert(ESMCI_STAGGERLOC_CORNER,
                                                 ESMC_GRIDITEM_MASK,
                                                 lDE, index, d);
            if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
              throw localrc;  // bail out with exception
          }


          // (Cart) Coords
          // Get data from grid
          if (nfields[GTOM_NFIELD_COORD]) {

            // Get original coord
            double orig_coord[ESMF_MAXDIM];
            localrc=grid->getCoordInternalConvert(ESMCI_STAGGERLOC_CORNER,
                                                  lDE, index, orig_coord);
            if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
              throw localrc;  // bail out with exception

            // Get data field in mesh
            double *d=nfields[GTOM_NFIELD_COORD]->data(node);

            // Call into coordsys method to convert to Cart
            localrc=ESMCI_CoordSys_ConvertToCart(grid->getCoordSys(),
                                                 grid->getDimCount(),
                                                 orig_coord,  // Input coordinates 
                                                 d);
            if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
              throw localrc;  // bail out with exception
          }


          // Original Coords
          // Get data from grid
          if (nfields[GTOM_NFIELD_ORIG_COORD]) {
            double *d=nfields[GTOM_NFIELD_ORIG_COORD]->data(node);
            localrc=grid->getCoordInternalConvert(ESMCI_STAGGERLOC_CORNER,
                                                  lDE, index, d);
            if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
              throw localrc;  // bail out with exception
          }
          
        }
      }
    } 
  }


  /// Element fields
#define GTOM_EFIELD_MASK 0
#define GTOM_EFIELD_MASK_VAL 1
#define GTOM_EFIELD_AREA     2
#define GTOM_EFIELD_COORD 3
#define GTOM_EFIELD_ORIG_COORD 4
#define GTOM_EFIELD_FRAC 5
#define GTOM_EFIELD_FRAC2 6
#define GTOM_EFIELD_NUM 7

  // Create element fields on mesh
  static void create_efields(Grid *grid, Mesh *mesh,
                             MEField<> *efields[GTOM_EFIELD_NUM]) {

    // Init field array to null
    for (int i=0; i<GTOM_EFIELD_NUM; i++) {
      efields[i]=NULL;
    }

    // Set context
    Context ctxt; ctxt.flip();


    // Mask
    if (grid->hasItemStaggerLoc(ESMCI_STAGGERLOC_CENTER, ESMC_GRIDITEM_MASK)) {
      efields[GTOM_EFIELD_MASK] = mesh->RegisterField("elem_mask",
                          MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);

      efields[GTOM_EFIELD_MASK_VAL] = mesh->RegisterField("elem_mask_val",
                          MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);
    }

    // Area
    if (grid->hasItemStaggerLoc(ESMCI_STAGGERLOC_CENTER, ESMC_GRIDITEM_AREA)) {
      efields[GTOM_EFIELD_AREA] = mesh->RegisterField("elem_area",
                          MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);
    }


    // COORDS
    if (grid->hasCoordStaggerLoc(ESMCI_STAGGERLOC_CENTER)) {
                                           
      // Add element coords field
      efields[GTOM_EFIELD_COORD] = mesh->RegisterField("elem_coordinates",
                 MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, mesh->spatial_dim(), true);

      // If not cartesian then add original coordinates field
      if (grid->getCoordSys() != ESMC_COORDSYS_CART) {
        efields[GTOM_EFIELD_ORIG_COORD]= mesh->RegisterField("elem_orig_coordinates",
                   MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, grid->getDimCount(), true);
      }      
    }


    // Fracs are always there
    efields[GTOM_EFIELD_FRAC] = mesh->RegisterField("elem_frac",
                                             MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);

    efields[GTOM_EFIELD_FRAC2] = mesh->RegisterField("elem_frac2",
                                             MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);
  }

  // Set element fields in mesh
  static void set_efields(Grid *grid, Mesh *mesh,
                          MEField<> *efields[GTOM_EFIELD_NUM]) {
    int localrc;

    // Get distgrid for the center staggerloc
    DistGrid *centerDistgrid;
    grid->getStaggerDistgrid(ESMCI_STAGGERLOC_CENTER, &centerDistgrid);

    // Get localDECount
    int localDECount=centerDistgrid->getDELayout()->getLocalDeCount();

    // Loop setting fields on each element
    for (int lDE=0; lDE < localDECount; lDE++) {

      // Get Center DE bounds
      int ubnd[ESMF_MAXDIM];
      int lbnd[ESMF_MAXDIM];
      grid->getDistExclusiveUBound(centerDistgrid, lDE, ubnd);
      grid->getDistExclusiveLBound(centerDistgrid, lDE, lbnd);

      // Loop over bounds
      int index[2];
      int de_index[2];
      for (int i0=lbnd[0]; i0<=ubnd[0]; i0++){
        for (int i1=lbnd[1]; i1<=ubnd[1]; i1++){

          // Set index
          index[0]=i0;
          index[1]=i1;

          // De based index
          de_index[0]=i0-lbnd[0];
          de_index[1]=i1-lbnd[1];

          // Get elem global id
          int elem_gid;
          bool is_local;
          if (!_get_global_id_from_localDE(centerDistgrid, lDE, de_index,
                              &elem_gid, &is_local)) {
            continue; // If we can't find a global id, then just skip
          }

          // Only set elem information if this is the owner
          if (!is_local) continue;

          // Get associated elem
          Mesh::MeshObjIDMap::iterator mi =  mesh->map_find(MeshObj::ELEMENT, elem_gid);

          // If it doesn't exist then go to next
          if (mi == mesh->map_end(MeshObj::ELEMENT)) {
            continue;
          }

          // Get elem
          MeshObj &elem=*mi;


          // Mask
          // Init to 0 (set later in ESMF_FieldRegridStore()
          if (efields[GTOM_EFIELD_MASK]) {
            double *d=efields[GTOM_EFIELD_MASK]->data(elem);
            *d=0.0;
          }

          // Mask Val
          // Get data from grid
          if (efields[GTOM_EFIELD_MASK_VAL]) {
            double *d=efields[GTOM_EFIELD_MASK_VAL]->data(elem);
            localrc=grid->getItemInternalConvert(ESMCI_STAGGERLOC_CENTER,
                                                 ESMC_GRIDITEM_MASK,
                                                 lDE, index, d);
            if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
              throw localrc;  // bail out with exception
          }


          // Area
          // Get data from grid
          if (efields[GTOM_EFIELD_AREA]) {
            double *d=efields[GTOM_EFIELD_AREA]->data(elem);
            localrc=grid->getItemInternalConvert(ESMCI_STAGGERLOC_CENTER,
                                                 ESMC_GRIDITEM_AREA,
                                                 lDE, index, d);
            if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
              throw localrc;  // bail out with exception
          }


          // (Cart) Coords
          // Get data from grid
          if (efields[GTOM_EFIELD_COORD]) {

            // Get original coord
            double orig_coord[ESMF_MAXDIM];
            localrc=grid->getCoordInternalConvert(ESMCI_STAGGERLOC_CENTER,
                                                  lDE, index, orig_coord);
            if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
              throw localrc;  // bail out with exception

            // Get data field in mesh
            double *d=efields[GTOM_EFIELD_COORD]->data(elem);

            // Call into coordsys method to convert to Cart
            localrc=ESMCI_CoordSys_ConvertToCart(grid->getCoordSys(),
                                                 grid->getDimCount(), 
                                                 orig_coord,  // Input coordinates 
                                                 d);
            if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
              throw localrc;  // bail out with exception

            //printf("id=%d orig_coords=%g %g coords=%g %g %g\n",elem.get_id(),orig_coord[0],orig_coord[1],d[0],d[1],d[2]);
          }


          // Original Coords
          // Get data from grid
          if (efields[GTOM_EFIELD_ORIG_COORD]) {
            double *d=efields[GTOM_EFIELD_ORIG_COORD]->data(elem);
            localrc=grid->getCoordInternalConvert(ESMCI_STAGGERLOC_CENTER,
                                                  lDE, index, d);
            if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
              throw localrc;  // bail out with exception
          }


          // Init fracs
          if (efields[GTOM_EFIELD_FRAC]) {
            double *d=efields[GTOM_EFIELD_FRAC]->data(elem);
            *d=1.0;
          }

          if (efields[GTOM_EFIELD_FRAC2]) {
            double *d=efields[GTOM_EFIELD_FRAC2]->data(elem);
            *d=1.0;
          }
        }

      }
    }
  }


  // Calculate index offsets for centers around a given corner
  // Since a given corner is one of the corners of the centers
  // surrounding it just invert the corner offsets
  static void _calc_center_offset(int corner_offset[NUM_QUAD_CORNERS][2], int center_offset[NUM_QUAD_CORNERS][2]) {

    // Invert corner offsets
    for (int i=0; i<NUM_QUAD_CORNERS; i++) {
      for (int j=0; j<2; j++) {
        center_offset[i][j]=-corner_offset[i][j];
      }
    }
  }


#define BAD_PROC (std::numeric_limits<UInt>::max())
  bool gqcn_debug=false;


  void _gid_to_proc(int gid, DistGrid *distgrid, int *_proc) {

    // Init to bad value
    int proc=BAD_PROC;

    // Calc tile and index from gid
    int tile=0;
    std::vector<int> index(2,-1);
    int localrc=distgrid->getIndexTupleFromSeqIndex(gid, index, tile);
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception

    // Get DeLayout
    DELayout *delayout=distgrid->getDELayout();

    // Get number of DE's
   int deCount=delayout->getDeCount();

   // Get de Index lists
   int const *minIndexPDimPDE=distgrid->getMinIndexPDimPDe();
   int const *maxIndexPDimPDE=distgrid->getMaxIndexPDimPDe();


    // Get tile
    const int *DETileList = distgrid->getTileListPDe();

    // Loop through DEs
    for (int de=0; de<deCount; de++) {

      // If de isn't on the correct tile, then go on to next
      if (tile != DETileList[de]) continue;

      // Get De minIndex
      int const *minIndex=minIndexPDimPDE+2*de;

      // Get De maxIndex
      int const *maxIndex=maxIndexPDimPDE+2*de;

      // If de is empty, then skip
      if ((maxIndex[0] < minIndex[0]) ||
          (maxIndex[1] < minIndex[1])) continue;


      // check if we're smaller than min
      if ((index[0] < minIndex[0]) ||
          (index[1] < minIndex[1])) continue;

      // check if we're bigger than max
      if ((index[0] > maxIndex[0]) ||
          (index[1] > maxIndex[1])) continue;

      // This point is on this de, so tranlate to proc and exit
      proc=delayout->getPet(de);
      break;
    }

    //    if (proc == BAD_PROC) {
    //  printf("gid=%d index=%d %d tile=%d proc=%d\n",gid,index[0],index[1],tile,proc);
    //}

    // Do output
    *_proc=proc;
  }

  // Add a node that you need on this PET, but that doesn't necessarily have a local element to host it
  // ...although don't add it if there isn't a cell anyplace to host it.
  static void _force_add_node(int node_gid, int cnr_de_index[2], Mesh *mesh, int localDE, 
                              int center_offset[NUM_QUAD_CORNERS][2], DistGrid *centerDistgrid, 
                              int cnr_offset[NUM_QUAD_CORNERS][2], DistGrid *cnrDistgrid,
                              int *local_node_index, int *local_elem_index, std::vector<MeshObj*> *cnr_nodes,
                              const MeshObjTopo *elem_topo, bool *added_node) {

    // Init indicator variable for whether node was successfully added
    *added_node=false;

    // Need to use tile version of _get_global_id() because if the localDE is empty (as
    // is likely the case with the center localDE here, then _get_global_id_from_localDE() 
    // won't work. Therefore, find tile & tile index for corner localDE and index
    int tile;
    int cnr_tile_index[ESMF_MAXDIM];
    _convert_localDE_to_tile_info(cnrDistgrid, localDE, cnr_de_index, &tile, cnr_tile_index);

    // Loop to find a suitable element host
    for (int i=0; i<NUM_QUAD_CORNERS; i++) {

      // calc index of corner
      int center_index[2];
      center_index[0]=cnr_tile_index[0]+center_offset[i][0]; 
      center_index[1]=cnr_tile_index[1]+center_offset[i][1];

      // Get global id, if we can't then not a suitable element
      bool is_local;
      int elem_gid=BAD_ID;
      if (!_get_global_id_from_tile(centerDistgrid, tile, center_index,
                          &elem_gid, &is_local)) continue;
    
      // DEBUG
      //if (node_gid == 7) {
      //  printf("%d# in _force_add id=%d elem_id=%d\n",Par::Rank(),node_gid,elem_gid);
      //  gqcn_debug=true;
      // }


      // If didn't find a good one, then move onto next 
      if (elem_gid == BAD_ID) continue;
    
      // Assuming that element isn't already in mesh, because otherwise it's nodes
      // (e.g. the node to be added) would already be in the mesh. Therefore, 
      // not checking to see if element is in mesh.
      
      // Add the node here?? Or just trust that _get_quad_corner_nodes will do it????
      // LET _get_quad_... do it, that way if it can't find all the nodes it won't 
      // add the element or the forced node. 

      // Get quad corner nodes for this element, this should add the node (among others)
      bool all_nodes_ok=true;
      _get_quad_corner_nodes_from_tile(mesh, cnrDistgrid, tile, center_index, cnr_offset,
                             local_node_index, cnr_nodes, &all_nodes_ok);

      // DEBUG
      //if (node_gid == 7) {
      //  printf("%d# in _force_add id=%d all_nodes_ok=%d\n",Par::Rank(),node_gid,all_nodes_ok);
      //  gqcn_debug=false;
      //}


      // If we didn't get all the nodes, then move onto next 
      if (!all_nodes_ok) continue;

      // Create Element
      MeshObj *elem = new MeshObj(MeshObj::ELEMENT,    // Mesh equivalent of Cell
                                  elem_gid,            // unique global id
                                  (*local_elem_index)++
                                  );

      // Add element
      UInt block_id = 1;  // Any reason to use different sets for cells?
      mesh->add_element(elem, *cnr_nodes, block_id, elem_topo);

      // Get proc based on gid
      int proc;
      _gid_to_proc(elem_gid, centerDistgrid, &proc);

      // Set proc
      elem->set_owner(proc);

      // If not local, turn off OWNED_ID, ACTIVE_ID, and turn on SHARED_ID. This is 
      // what happens for ghost elems, so it makes sense to do that here. 
      // (This seems to happen automatically for nodes, but not elems)
      if (proc != Par::Rank()) {
        // Setup for changing attribute
        const Context &ctxt = GetMeshObjContext(*elem);
        Context newctxt(ctxt);

        // Clear OWNED_ID since we're not owned
        newctxt.clear(Attr::OWNED_ID);

        // Clear ACTIVE_ID since we're not locally owned 
        newctxt.clear(Attr::ACTIVE_ID);

        // turn on SHARED_ID since owned someplace else 
        newctxt.set(Attr::SHARED_ID);

        // If attribute has changed change in elem
        if (newctxt != ctxt) {
          Attr attr(GetAttr(*elem), newctxt);
          mesh->update_obj(elem, attr);
        }
      }

      // DEBUG OUTPUT
      //printf("%d# in _force_add elem_id=%d proc=%d owner=%d is_local=%d\n",Par::Rank(),elem_gid,proc,elem->get_owner(),GetAttr(*elem).is_locally_owned());

      // Indicate that we've successfully added the node (via adding the elem that contains it) 
      *added_node=true;

      // If we've successfully added the element, then leave
      break;
    }
  }

#endif




  /* XMRKX */
  
  // OLD CODE - KEEP TO USE PARTS OF WHILE BUILDING ABOVE
#if 0

void ESMCI_GridToMeshCell(const Grid &grid_,
                          const std::vector<ESMCI::Array*> &arrays,
                          Mesh **out_meshpp, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_GridToMeshCall()"

  try {
    // local error code
    int localrc;


 // Get grid pointer
 Grid *grid = &(const_cast<Grid&>(grid_));

 // The Grid needs to have corner coordinates
 if (!grid->hasCoordStaggerLoc(ESMCI_STAGGERLOC_CORNER)) {
   ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
    "To use this method the Grid must contain coordinates at corner staggerloc.", ESMC_CONTEXT, &localrc);
   throw localrc;
 }

 // The Grid currently can't be arbitrarily distributed
 if (grid->getDecompType() != ESMC_GRID_NONARBITRARY) {
   ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
    "To use this method the Grid can't be arbitrarily distributed.", ESMC_CONTEXT, &localrc);
   throw localrc;
 }

 // Create Mesh
 Mesh *mesh = new Mesh();

 // Set output mesh
 *out_meshpp=mesh;

 // Get dimCount
 int dimCount=grid->getDimCount();

 // Only supporting 2D right now
 if (dimCount != 2) {
   Throw() << "This method currently only supports 2D Grids";
 }

 // *** Set some meta-data ***
 // We set the topological dimension of the mesh (quad = 2, hex = 3, etc...)
 int pdim=dimCount;
 mesh->set_parametric_dimension(pdim);

 // In what dimension is the grid embedded?? (sphere = 3, simple rectangle = 2, etc...)
 UInt sdim = grid->getCartCoordDimCount();
 mesh->set_spatial_dimension(sdim);

 // original spatial dim is the same as grid dimension
 mesh->orig_spatial_dim=pdim;
 mesh->coordsys=grid->getCoordSys();

 // Get distgrid for the center staggerloc
 DistGrid *centerDistgrid;
 grid->getStaggerDistgrid(ESMCI_STAGGERLOC_CENTER, &centerDistgrid);

 // Get centerLocalDECount
 int centerLocalDECount=centerDistgrid->getDELayout()->getLocalDeCount();

 // Get distgrid for the corner staggerloc
 DistGrid *cnrDistgrid;
 grid->getStaggerDistgrid(ESMCI_STAGGERLOC_CORNER, &cnrDistgrid);

 // Get cnrLocalDECount
 int cnrLocalDECount=cnrDistgrid->getDELayout()->getLocalDeCount();

 // Setup topology
 const MeshObjTopo *elem_topo = 0;
 elem_topo = sdim == 2 ? GetTopo("QUAD") : GetTopo("QUAD_3D");
 if (!elem_topo) Throw() << "Could not get topology for sdim=" << sdim;

 // Get index offsets for the corners around a center
 int cnr_offset[NUM_QUAD_CORNERS][2];
 _calc_corner_offset(grid, cnr_offset);

 // Get index offsets for the centers around a corner
 int center_offset[NUM_QUAD_CORNERS][2];
 _calc_center_offset(cnr_offset, center_offset);

 // Space for corner nodes
 std::vector<MeshObj*> cnr_nodes(NUM_QUAD_CORNERS);

 // printf("offset=[%d %d] [%d %d]  [%d %d]  [%d %d]\n",cnr_offset[0][0],
 //cnr_offset[0][1],cnr_offset[1][0],cnr_offset[1][1],cnr_offset[2][0],cnr_offset[2][1],
 //cnr_offset[3][0], cnr_offset[3][1]);


 // Loop over center DEs adding cells
 int local_node_index=0;
 int local_elem_index=0;
 for (int lDE=0; lDE < centerLocalDECount; lDE++) {

   // Get Center DE bounds
   int ubnd[ESMF_MAXDIM];
   int lbnd[ESMF_MAXDIM];
   grid->getDistExclusiveUBound(centerDistgrid, lDE, ubnd);
   grid->getDistExclusiveLBound(centerDistgrid, lDE, lbnd);

   // Get Corner DE bounds
   int cnr_ubnd[ESMF_MAXDIM];
   int cnr_lbnd[ESMF_MAXDIM];
   grid->getDistExclusiveUBound(cnrDistgrid, lDE, cnr_ubnd);
   grid->getDistExclusiveLBound(cnrDistgrid, lDE, cnr_lbnd);

   // DEBUG
   //printf("%d# lDE=%d CENTER lbnd=%d %d ubnd=%d %d\n",Par::Rank(),lDE,lbnd[0],lbnd[1],ubnd[0],ubnd[1]);
   //printf("%d# lDE=%d CORNER lbnd=%d %d ubnd=%d %d\n",Par::Rank(),lDE,cnr_lbnd[0],cnr_lbnd[1],cnr_ubnd[0],cnr_ubnd[1]);

   // Loop over bounds
   for (int i0=lbnd[0]; i0<=ubnd[0]; i0++){
     for (int i1=lbnd[1]; i1<=ubnd[1]; i1++){

       // Set index
       int index[2];
       index[0]=i0;
       index[1]=i1;

       // De based index
       int de_index[2];
       de_index[0]=i0-lbnd[0];
       de_index[1]=i1-lbnd[1];


       // Get Element corner nodes
       bool all_nodes_ok=true;
       _get_quad_corner_nodes_from_localDE(mesh, cnrDistgrid, lDE, de_index, cnr_offset, &local_node_index,
                                           &cnr_nodes, &all_nodes_ok);

       // If we didn't get all the nodes, then go to next element
       if (!all_nodes_ok) continue;

       // Get element id
       int elem_gid;
       bool is_local;
       if (!_get_global_id_from_localDE(centerDistgrid, lDE, de_index,
                           &elem_gid, &is_local)) {
         continue; // If we can't find a global id, then just skip
       }


       // Create Element
       MeshObj *elem = new MeshObj(MeshObj::ELEMENT,    // Mesh equivalent of Cell
                                   elem_gid,            // unique global id
                                   local_elem_index++
                                   );

       // Add element
       UInt block_id = 1;  // Any reason to use different sets for cells?
       mesh->add_element(elem, cnr_nodes, block_id, elem_topo);

       // Set owner to the current processor
       elem->set_owner(Par::Rank());

       // DEBUG
       //if (elem_gid==9) {
       //   printf("gid=%d i=%d %d lDE=%d nodes=%d %d %d %d \n",elem_gid,i0,i1,lDE,cnr_nodes[0]->get_id(),cnr_nodes[1]->get_id(),cnr_nodes[2]->get_id(),cnr_nodes[3]->get_id());
       //}
     }
   }
 }

 // Loop adding local nodes that weren't added as part of local cell creation
 for (int lDE=0; lDE < cnrLocalDECount; lDE++) {

   // Get Corner DE bounds
   int cnr_ubnd[ESMF_MAXDIM];
   int cnr_lbnd[ESMF_MAXDIM];
   grid->getDistExclusiveUBound(cnrDistgrid, lDE, cnr_ubnd);
   grid->getDistExclusiveLBound(cnrDistgrid, lDE, cnr_lbnd);


   // Loop over bounds
   int index[2];
   int nonde_index[2];
   for (int i0=cnr_lbnd[0]; i0<=cnr_ubnd[0]; i0++){
     for (int i1=cnr_lbnd[1]; i1<=cnr_ubnd[1]; i1++){

       // Set index
       index[0]=i0;
       index[1]=i1;

       // De based index
       int de_index[2];
       de_index[0]=i0-cnr_lbnd[0];
       de_index[1]=i1-cnr_lbnd[1];

       // Get node global id
       int node_gid;
       bool is_local;
       if (!_get_global_id_from_localDE(cnrDistgrid, lDE, de_index,
                           &node_gid, &is_local)) {
         continue; // If we can't find a global id, then just skip
       }

       // Only set node information if this is the owner
       if (!is_local) continue;

       // Get associated node
       Mesh::MeshObjIDMap::iterator mi =  mesh->map_find(MeshObj::NODE, node_gid);

       // If it exists then go to next
       if (mi != mesh->map_end(MeshObj::NODE)) continue;

       // Otherwise add
       bool added_node=false;
       _force_add_node(node_gid, de_index, mesh, lDE, 
                       center_offset, centerDistgrid, 
                       cnr_offset, cnrDistgrid,
                       &local_node_index, &local_elem_index, &cnr_nodes,
                       elem_topo, &added_node);

     }
   }
 }

 // Loop setting local node owners
 for (int lDE=0; lDE < cnrLocalDECount; lDE++) {

   // Get Corner DE bounds
   int cnr_ubnd[ESMF_MAXDIM];
   int cnr_lbnd[ESMF_MAXDIM];
   grid->getDistExclusiveUBound(cnrDistgrid, lDE, cnr_ubnd);
   grid->getDistExclusiveLBound(cnrDistgrid, lDE, cnr_lbnd);


   // Loop over bounds
   int index[2];
   int nonde_index[2];
   for (int i0=cnr_lbnd[0]; i0<=cnr_ubnd[0]; i0++){
     for (int i1=cnr_lbnd[1]; i1<=cnr_ubnd[1]; i1++){

       // Set index
       index[0]=i0;
       index[1]=i1;

       // De based index
       int de_index[2];
       de_index[0]=i0-cnr_lbnd[0];
       de_index[1]=i1-cnr_lbnd[1];

       // Get node global id
       int node_gid;
       bool is_local;
       if (!_get_global_id_from_localDE(cnrDistgrid, lDE, de_index,
                           &node_gid, &is_local)) {
         continue; // If we can't find a global id, then just skip
       }

       // Only set node information if this is the owner
       if (!is_local) continue;

       // Get associated node
       Mesh::MeshObjIDMap::iterator mi =  mesh->map_find(MeshObj::NODE, node_gid);

       // If it doesn't exist then go to next
       if (mi == mesh->map_end(MeshObj::NODE)) {
         continue;
       }

       // Get node pointer
       MeshObj *node=&*mi;

       // Set owner to the current processor
       node->set_owner(Par::Rank());

     }
   }
 }

 // Loop through Mesh nodes setting the remaining owners
 MeshDB::iterator ni = mesh->node_begin(), ne = mesh->node_end();
 for (; ni != ne; ++ni) {
   MeshObj *node=&*ni;

   // Get node global id
   int gid=node->get_id();

   // get node owner
   int owner=node->get_owner();

   // If the owner is already set then skip
   if (owner != BAD_PROC) continue;

   // Get proc based on gid
   int proc;
   _gid_to_proc(gid, cnrDistgrid, &proc);

   // Set owner
   node->set_owner(proc);
 }

 // Add node Fields
 IOField<NodalField> *nfields[GTOM_NFIELD_NUM];
 create_nfields(grid, mesh, nfields);

 // Set node fields
 // (This has to happen before mesh is committed below)
 set_nfields(grid, mesh, nfields);

 // Add element Fields
 MEField<> *efields[GTOM_EFIELD_NUM];
 create_efields(grid, mesh, efields);

 // Finalize the Mesh
 mesh->build_sym_comm_rel(MeshObj::NODE);
 mesh->Commit();

 // Set element fields
 set_efields(grid, mesh, efields);

 // Halo fields, so entities with an owner on another processor
 // will have the correct value
 {
   std::vector<MEField<>*> fds;

   Mesh::FieldReg::MEField_iterator fi = mesh->Field_begin(), fe = mesh->Field_end();

   for (; fi != fe; ++fi) fds.push_back(&*fi);

   mesh->HaloFields(fds.size(), &fds[0]);
 }


#if 0
 // DEBUG OUTPUT
  {
     // Output list of nodes with info
    MeshDB::iterator ni = mesh->node_begin(), ne = mesh->node_end();
    for (; ni != ne; ++ni) {
      MeshObj &node=*ni;

      //      printf("%d# node: id=%d owner=%d is_local=%d data_index=%d \n",Par::Rank(),node.get_id(),node.get_owner(),GetAttr(node).is_locally_owned(),node.get_data_index());
      printf("%d# node: id=%d owner=%d is_local=%d is_active=%d is_shared=%d data_index=%d\n",Par::Rank(),node.get_id(),node.get_owner(),GetAttr(node).is_locally_owned(),GetAttr(node).GetContext().is_set(Attr::ACTIVE_ID), GetAttr(node).is_shared(),node.get_data_index());

    }

   printf("\n");

     // Output list of elems with info
    MeshDB::iterator ei = mesh->elem_begin_all(), ee = mesh->elem_end_all();
    for (; ei != ee; ++ei) {
      MeshObj &elem=*ei;

      printf("%d# elem: id=%d owner=%d is_local=%d is_active=%d is_shared=%d data_index=%d\n",Par::Rank(),elem.get_id(),elem.get_owner(),GetAttr(elem).is_locally_owned(),GetAttr(elem).GetContext().is_set(Attr::ACTIVE_ID), GetAttr(elem).is_shared(),elem.get_data_index());
    }
  }
#endif



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
                                  "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }


// Set successful return code
 if (rc!=NULL) *rc = ESMF_SUCCESS;
}  
#endif

  
 
