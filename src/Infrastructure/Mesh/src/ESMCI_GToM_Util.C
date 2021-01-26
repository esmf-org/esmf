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
#define ESMC_FILENAME "ESMCI_GridToMesh.C"
//==============================================================================
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// Create a mesh from a given grid.
//
//-----------------------------------------------------------------------------


#include "Mesh/include/ESMCI_GToM_Util.h"
#include "ESMCI_Grid.h"
#include "ESMCI_VM.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Ptypes.h"
#include "Mesh/include/Legacy/ESMCI_ParEnv.h"

#include <limits>
#include <iostream>
#include <vector>
#include <map>
#include <cmath>


namespace ESMCI {


#undef  ESMC_METHOD
#define ESMC_METHOD "GToM_Util()"
 /* XMRKX */


  // Get global id
  // if there is one returns true, if there isn't returns false
  // Calc global id given:
  // tile  - tile number (1,.., tileCount)
  // index - tile-specific absolute index tuple 
  bool get_global_id_from_tile(DistGrid *distgrid, int tile, int *index,
                                       int *_gid, bool *_is_local) {

    // determine sequence index
    std::vector<int> seqIndex;
    int localrc=distgrid->getSequenceIndexTile(tile,index,seqIndex);
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception

    // If there's just 1, then use that as the id, otherwise use the min id
    int gid=GTOM_BAD_ID;
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


  void convert_localDE_to_tile_info(DistGrid *distgrid, int localDE, int *de_index, int *_tile, int *tile_index) {
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


  bool get_global_id_from_localDE(DistGrid *distgrid, int localDE, int *index,
                             int *_gid, bool *_is_local) {

    //// Translate localDE info into tile info ////
    int tile;
    int tile_index[ESMF_MAXDIM];
    convert_localDE_to_tile_info(distgrid, localDE, index, &tile, tile_index);

    // Call into get global id from tile
    return get_global_id_from_tile(distgrid, tile, tile_index,
                                    _gid, _is_local);
  }



  // Calculate index offsets for corners around a given center
 void calc_corner_offset(Grid *grid, int corner_offset[NUM_QUAD_CORNERS][2]) {
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

  // Calculate index offsets for centers around a given corner
  // Since a given corner is one of the corners of the centers
  // surrounding it just invert the corner offsets
  void calc_center_offset(int corner_offset[NUM_QUAD_CORNERS][2], int center_offset[NUM_QUAD_CORNERS][2]) {

    // Invert corner offsets
    for (int i=0; i<NUM_QUAD_CORNERS; i++) {
      for (int j=0; j<2; j++) {
        center_offset[i][j]=-corner_offset[i][j];
      }
    }
  }

  void gid_to_proc(int gid, DistGrid *distgrid, int *_proc) {

    // Init to bad value
    int proc=GTOM_BAD_PROC;

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

    //    if (proc == GTOM_BAD_PROC) {
    //  printf("gid=%d index=%d %d tile=%d proc=%d\n",gid,index[0],index[1],tile,proc);
    //}

    // Do output
    *_proc=proc;
  }

} // namespace
