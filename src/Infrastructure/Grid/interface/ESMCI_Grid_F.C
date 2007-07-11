// $Id: ESMCI_Grid_F.C,v 1.5 2007/07/11 20:50:41 oehmke Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMC_Grid_F.C"
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "ESMC_Start.h"
#include "ESMC_Base.h"
#include "ESMC_VM.h"
#include "ESMC_DistGrid.h"
#include "ESMC_RHandle.h"
#include "ESMC_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"             // for LogErr
#include "ESMCI_Grid.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt Grid} class functions.
//
//EOP
//-------------------------------------------------------------------------



// the interface subroutine names MUST be in lower case by ESMF convention
extern "C" {

  // - ESMF-public methods:        
  void FTN(c_esmc_gridcreateempty)(ESMCI::Grid **ptr, 
					  int *rc){
    int localrc;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridcreatefromdistgrid()"

    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;

    // call into C++
    *ptr = ESMCI::GridCreateEmpty(&localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
}


  // - ESMF-public methods:        
  void FTN(c_esmc_gridcreatefromdistgrid)(ESMCI::Grid **ptr, 
    int *nameLen, char *name, ESMC_TypeKind *coordTypeKind, 
					  ESMCI::DistGrid **distgrid,
					  ESMCI::InterfaceInt **dimmapArg, 
					  ESMCI::InterfaceInt **lboundsArg,
					  ESMCI::InterfaceInt **uboundsArg,
					  ESMCI::InterfaceInt **coordRanksArg,
					  ESMCI::InterfaceInt **coordDimMapArg,		  
					  ESMC_IndexFlag *indexflag,
					  int *gridtype,
					  int *rc){
    int localrc;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridcreatefromdistgrid()"

    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;

    // call into C++
    *ptr = ESMCI::GridCreate(*nameLen, ESMC_NOT_PRESENT_FILTER(name),
      ESMC_NOT_PRESENT_FILTER(coordTypeKind), *distgrid, *dimmapArg,
      *lboundsArg, *uboundsArg, *coordRanksArg, *coordDimMapArg,
      ESMC_NOT_PRESENT_FILTER(indexflag), ESMC_NOT_PRESENT_FILTER(gridtype),
      &localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
}

  void FTN(c_esmc_gridget)(ESMCI::Grid **_grid, 
                           ESMC_TypeKind *_coordTypeKind,
                           int *_rank, int *_tileCount,
			   ESMCI::DistGrid **_distgrid,
                           int *_staggerLocCount, 
			   ESMCI::InterfaceInt **_dimmap, 
			   ESMCI::InterfaceInt **_lbounds,
			   ESMCI::InterfaceInt **_ubounds,
			   ESMCI::InterfaceInt **_coordRanks,
			   ESMCI::InterfaceInt **_coordDimMap,		  
			   ESMC_IndexFlag *_indexflag,
			   int *_gridtype,
			   int *_rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridget()"

    int localrc;
    int distRank,undistRank,rank;
    ESMCI::Grid *grid;

    // Get Grid pointer
    grid=*_grid;

    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;
    if (_rc!=NULL) *_rc = ESMC_RC_NOT_IMPL;

    // Use grid access methods to retrieve information seperately
    // I'm a little leary of putting this much logic in an interface call, but
    // it makes things less convoluted to do it this way. 

    // get some useful info
    distRank=grid->getDistRank();
    undistRank=grid->getUndistRank();
    rank = grid->getRank();


    // coordTypeKind
    if (ESMC_NOT_PRESENT_FILTER(_coordTypeKind) != ESMC_NULL_POINTER)
      *_coordTypeKind = grid->getTypeKind();

    // rank
    if (ESMC_NOT_PRESENT_FILTER(_rank) != ESMC_NULL_POINTER)
      *_rank = rank;

    // tileCount
    if (ESMC_NOT_PRESENT_FILTER(_tileCount) != ESMC_NULL_POINTER)
      *_tileCount = grid->getTileCount();

    // distgrid
    if (ESMC_NOT_PRESENT_FILTER(_distgrid) != ESMC_NULL_POINTER)
      *_distgrid = (ESMCI::DistGrid *)grid->getDistGrid();

    // staggerLocCount
    if (ESMC_NOT_PRESENT_FILTER(_staggerLocCount) != ESMC_NULL_POINTER)
      *_staggerLocCount = grid->getStaggerLocCount();

    // get dimmap 
    if (*_dimmap != NULL){
      // Error check
      if ((*_dimmap)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- dimmap array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_dimmap)->extent[0] < distRank){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- dimmap array must be of size = the distributed rank of the Grid", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      // fill in dimmap
      memcpy((*_dimmap)->array, grid->getDimMap(), sizeof(int) * distRank);
    }


    // get lbounds
    if (*_lbounds != NULL){
      // Error check
      if ((*_lbounds)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- lbounds array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_lbounds)->extent[0] < undistRank){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- lbounds array must be of size = the undistributed rank of the Grid", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      // fill in lbounds
      memcpy((*_lbounds)->array, grid->getLbounds(), sizeof(int) * undistRank);
    }

    // get ubounds
    if (*_ubounds != NULL){
      // Error check
      if ((*_ubounds)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- ubounds array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_ubounds)->extent[0] < undistRank){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- ubounds array must be of size = the undistributed rank of the Grid", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      // fill in ubounds
      memcpy((*_ubounds)->array, grid->getUbounds(), sizeof(int) * undistRank);
    }

    // get coordRanks
    if (*_coordRanks != NULL){
      // Error check
      if ((*_coordRanks)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- coordRanks array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_coordRanks)->extent[0] < rank){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- coordRanks array must be of size = the rank of the Grid", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      // fill in coordRanks
      memcpy((*_coordRanks)->array, grid->getCoordRanks(), sizeof(int) * rank);
    }

    // get coordDimMap
    if (*_coordDimMap != NULL){
      // Error check
      if ((*_coordDimMap)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- coordDimMap array must be of rank 2", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if (((*_coordDimMap)->extent[0] < rank) || ((*_coordDimMap)->extent[1] < rank)){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- coordDimMap array must be of size = the rank of the Grid", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      // fill in coordDimMap
      int k=0;
      int **coordDimMap=grid->getCoordDimMap();
      for (int i=0; i<rank; i++) {
	for (int j=0; j<rank; j++) {
           // Note: order of i,j is reversed because of F vs. C array ordering
	  (*_coordDimMap)->array[k]=coordDimMap[j][i];
	  k++;
	}
      } 
    }

    // indexflag
    if (ESMC_NOT_PRESENT_FILTER(_indexflag) != ESMC_NULL_POINTER)
      *_indexflag = grid->getIndexFlag();

    // gridtype
    if (ESMC_NOT_PRESENT_FILTER(_gridtype) != ESMC_NULL_POINTER)
      *_gridtype = grid->getGridType();

    // return success
    if (_rc!=NULL) *_rc = ESMF_SUCCESS;
}



  void FTN(c_esmc_gridgetcoordintoarray)(ESMCI::Grid **grid, 
                                         int *staggerloc, 
                                         int *coord, 
                                         ESMCI::Array **array,
                                         ESMC_DataCopy *docopy, 
                                         int *rc) {
    int localrc;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridgetcoordintoarray()"

    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;

    // call into C++
    localrc= ESMCI::gridGetCoordIntoArray(*grid, ESMC_NOT_PRESENT_FILTER(staggerloc),
      ESMC_NOT_PRESENT_FILTER(coord), array, ESMC_NOT_PRESENT_FILTER(docopy));
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
}



  void FTN(c_esmc_gridsetcoordfromarray)(ESMCI::Grid **grid, 
                                         int *staggerloc, 
                                         int *coord, 
                                         ESMCI::Array **array,
                                         ESMC_DataCopy *docopy, 
                                         ESMCI::InterfaceInt **coordAlignArg, 
                                         int *rc) {
    int localrc;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridsetfromarray()"

    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;

    // call into C++
    localrc= ESMCI::gridSetCoordFromArray(*grid, ESMC_NOT_PRESENT_FILTER(staggerloc),
      ESMC_NOT_PRESENT_FILTER(coord), *array, ESMC_NOT_PRESENT_FILTER(docopy),
      *coordAlignArg);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
}

#if 0
  
  void FTN(c_esmc_gridgetlocaltileinfo)(ESMCI::Grid **_grid, int *_tile, int *_coord,
					int *_localDE, int *_staggerloc,  
                                        ESMCI::InterfaceInt **_exclusiveLBound,
                                        ESMCI::InterfaceInt **_exclusiveUBound,
                                        ESMCI::InterfaceInt **_computationalLBound,
                                        ESMCI::InterfaceInt **_computationalUBound,
                                        ESMCI::InterfaceInt **_totalLBound, 
                                        ESMCI::InterfaceInt **_totalUBound,
                                        ESMCI::InterfaceInt **_computationalLWidth,
                                        ESMCI::InterfaceInt **_computationalUWidth,
                                        ESMCI::InterfaceInt **_totalLWidth, 
                                        ESMCI::InterfaceInt **_totalUWidth,
                                        int *_rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridgetlocaltileinfo()"
    int localrc;
    int distRank,undistRank,rank;
    int tile, coord,localDE,staggerloc;
    ESMCI::Grid *grid;
    ESMCI::Array *array;

    // Get Grid pointer
    grid=*_grid;

    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;
    if (_rc!=NULL) *_rc = ESMC_RC_NOT_IMPL;

    // get some useful info
    distRank=grid->getDistRank();
    undistRank=grid->getUndistRank();
    rank = grid->getRank();

    // tile
    if (ESMC_NOT_PRESENT_FILTER(_tile) == ESMC_NULL_POINTER) {
      tile=0;  // default 1st tile
    } else {
      tile=*_tile-1; // convert to 0 based
    }

    // coord
    if (ESMC_NOT_PRESENT_FILTER(_coord) == ESMC_NULL_POINTER) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
          "- must pass in valid coord", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
    } else {
      coord=*_coord-1; // convert to 0 based
    }

    // localDE
    if (ESMC_NOT_PRESENT_FILTER(_localDE) == ESMC_NULL_POINTER) {
      localDE=0;
    } else {
      localDE=*_localDE-1; // convert to 0 based (TODO: MAKE SURE I HAVE TO DO THIS) 
    }


    // staggerloc
    if (ESMC_NOT_PRESENT_FILTER(_staggerloc) == ESMC_NULL_POINTER) {
      staggerloc=0;
    } else {
      staggerloc=*_staggerloc; // already 0-based
    }


    // Input Error Checking
    if ((localDE < 0) || (localDE >= grid->getDistGrid()->getDelayout()->getLocalDeCount())) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
          "- localDE outside range on this processor", ESMC_NOT_PRESENT_FILTER(_rc));
        return;

    }


    //TODO: more input error checking (have grid functions for checking values (e.g. coord)?)
    ///     Have a grid status check function? 

    // CHECK OVER THIS NEXT PART

    // Get Array From Grid
    localrc=grid->getCoordArray(staggerloc, coord, &array);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(_rc));
    if (array == ESMC_NULL_POINTER) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_BAD,
          "- unset coord array", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
    }

    // fill exclusiveLBound
    if (*_exclusiveLBound != NULL){
      // exclusiveLBound was provided -> do some error checking
      if ((*_exclusiveLBound)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- exclusiveLBound array must be of rank 1", rc);
        return;
      }
      if ((*_exclusiveLBound)->extent[0] < rank){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- exclusiveLBound must at least be the same rank as the the grid'", rc);
        return;
      }
       memcpy((*_exclusiveLBound)->array,
          &(array->getExclusiveLBound())[localDE*distRank]), sizeof(int)*distRank);
    }





  // STOPPED HERE

  // DO I NEED TO COMBINE WITH UNDISTRIBUTED DIMENSIONS????????????? 



    // fill exclusiveUBound
    if (*exclusiveUBound != NULL){
      // exclusiveUBound was provided -> do some error checking
      if ((*exclusiveUBound)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- exclusiveUBound array must be of rank 2", rc);
        return;
      }
      if ((*exclusiveUBound)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of exclusiveUBound must be of size 'dimCount'", rc);
        return;
      }
      if ((*exclusiveUBound)->extent[1] < localDeCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dimension of exclusiveUBound must be of size 'localDeCount'",
          rc);
        return;
      }
      // fill in the values: The interface allows to pass in exclusiveUBound
      // arrays which are larger than dimCount x localDeCount. Consequently it
      // is necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the exclusiveUBound array.
      for (int i=0; i<localDeCount; i++)
        memcpy(&((*exclusiveUBound)->array[i*(*exclusiveUBound)->extent[0]]),
          &(((*ptr)->getExclusiveUBound())[i*dimCount]), sizeof(int)*dimCount);
    }
    // fill computationalLBound
    if (*computationalLBound != NULL){
      // computationalLBound was provided -> do some error checking
      if ((*computationalLBound)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- computationalLBound array must be of rank 2", rc);
        return;
      }
      if ((*computationalLBound)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dim of computationalLBound must be of size 'dimCount'", rc);
        return;
      }
      if ((*computationalLBound)->extent[1] < localDeCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dim of computationalLBound must be of size 'localDeCount'", 
          rc);
        return;
      }
      // fill in the values: The interface allows to pass in computationalLBound
      // arrays which are larger than dimCount x localDeCount. Consequently it
      // is necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the computationalLBound
      // array.
      for (int i=0; i<localDeCount; i++)
        memcpy(&((*computationalLBound)->
          array[i*(*computationalLBound)->extent[0]]),
          &(((*ptr)->getComputationalLBound())[i*dimCount]), 
          sizeof(int)*dimCount);
    }
    // fill computationalUBound
    if (*computationalUBound != NULL){
      // computationalUBound was provided -> do some error checking
      if ((*computationalUBound)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- computationalUBound array must be of rank 2", rc);
        return;
      }
      if ((*computationalUBound)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dim of computationalUBound must be of size 'dimCount'", rc);
        return;
      }
      if ((*computationalUBound)->extent[1] < localDeCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dim of computationalUBound must be of size 'localDeCount'",
          rc);
        return;
      }
      // fill in the values: The interface allows to pass in computationalUBound
      // arrays which are larger than dimCount x localDeCount. Consequently it
      // is necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the computationalUBound
      // array.
      for (int i=0; i<localDeCount; i++)
        memcpy(&((*computationalUBound)->
          array[i*(*computationalUBound)->extent[0]]),
          &(((*ptr)->getComputationalUBound())[i*dimCount]), 
          sizeof(int) * dimCount);
    }
    // fill totalLBound
    if (*totalLBound != NULL){
      // totalLBound was provided -> do some error checking
      if ((*totalLBound)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- totalLBound array must be of rank 2", rc);
        return;
      }
      if ((*totalLBound)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of totalLBound must be of size 'dimCount'", rc);
        return;
      }
      if ((*totalLBound)->extent[1] < localDeCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dimension of totalLBound must be of size 'localDeCount'",
          rc);
        return;
      }
      // fill in the values: The interface allows to pass in totalLBound
      // arrays which are larger than dimCount x localDeCount. Consequently it
      // is necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the totalLBound array.
      for (int i=0; i<localDeCount; i++)
        memcpy(&((*totalLBound)->array[i*(*totalLBound)->extent[0]]),
          &(((*ptr)->getTotalLBound())[i*dimCount]), sizeof(int)*dimCount);
    }
    // fill totalUBound
    if (*totalUBound != NULL){
      // totalUBound was provided -> do some error checking
      if ((*totalUBound)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- totalUBound array must be of rank 2", rc);
        return;
      }
      if ((*totalUBound)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of totalUBound must be of size 'dimCount'", rc);
        return;
      }
      if ((*totalUBound)->extent[1] < localDeCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dimension of totalUBound must be of size 'localDeCount'",
          rc);
        return;
      }
      // fill in the values: The interface allows to pass in totalUBound
      // arrays which are larger than dimCount x localDeCount. Consequently it
      // is necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the totalUBound array.
      for (int i=0; i<localDeCount; i++)
        memcpy(&((*totalUBound)->array[i*(*totalUBound)->extent[0]]),
          &(((*ptr)->getTotalUBound())[i*dimCount]), sizeof(int)*dimCount);
    }
    // fill computationalLWidth
    if (*computationalLWidth != NULL){
      // computationalLWidth was provided -> do some error checking
      if ((*computationalLWidth)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- computationalLWidth array must be of rank 2", rc);
        return;
      }
      if ((*computationalLWidth)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of computationalLWidth must be of size 'dimCount'",
          rc);
        return;
      }
      if ((*computationalLWidth)->extent[1] < localDeCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dimension of computationalLWidth must be of size"
          " 'localDeCount'", rc);
        return;
      }
      // fill in values
      for (int i=0; i<localDeCount; i++)
        for (int j=0; j<dimCount; j++)
          (*computationalLWidth)->array[i*(*computationalLWidth)->extent[0]+j] =
            ((*ptr)->getExclusiveLBound())[i*dimCount+j] -
            ((*ptr)->getComputationalLBound())[i*dimCount+j];
    }
    // fill computationalUWidth
    if (*computationalUWidth != NULL){
      // computationalUWidth was provided -> do some error checking
      if ((*computationalUWidth)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- computationalUWidth array must be of rank 2", rc);
        return;
      }
      if ((*computationalUWidth)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of computationalUWidth must be of size 'dimCount'",
          rc);
        return;
      }
      if ((*computationalUWidth)->extent[1] < localDeCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dimension of computationalUWidth must be of size"
          " 'localDeCount'", rc);
        return;
      }
      // fill in values
      for (int i=0; i<localDeCount; i++)
        for (int j=0; j<dimCount; j++)
          (*computationalUWidth)->array[i*(*computationalUWidth)->extent[0]+j] =
            ((*ptr)->getComputationalUBound())[i*dimCount+j] -
            ((*ptr)->getExclusiveUBound())[i*dimCount+j];
    }
    // fill totalLWidth
    if (*totalLWidth != NULL){
      // totalLWidth was provided -> do some error checking
      if ((*totalLWidth)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- totalLWidth array must be of rank 2", rc);
        return;
      }
      if ((*totalLWidth)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of totalLWidth must be of size 'dimCount'",
          rc);
        return;
      }
      if ((*totalLWidth)->extent[1] < localDeCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dimension of totalLWidth must be of size"
          " 'localDeCount'", rc);
        return;
      }
      // fill in values
      for (int i=0; i<localDeCount; i++)
        for (int j=0; j<dimCount; j++)
          (*totalLWidth)->array[i*(*totalLWidth)->extent[0]+j] =
            ((*ptr)->getExclusiveLBound())[i*dimCount+j] -
            ((*ptr)->getTotalLBound())[i*dimCount+j];
    }
    // fill totalUWidth
    if (*totalUWidth != NULL){
      // totalUWidth was provided -> do some error checking
      if ((*totalUWidth)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- totalUWidth array must be of rank 2", rc);
        return;
      }
      if ((*totalUWidth)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of totalUWidth must be of size 'dimCount'",
          rc);
        return;
      }
      if ((*totalUWidth)->extent[1] < localDeCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dimension of totalUWidth must be of size"
          " 'localDeCount'", rc);
        return;
      }
      // fill in values
      for (int i=0; i<localDeCount; i++)
        for (int j=0; j<dimCount; j++)
          (*totalUWidth)->array[i*(*totalUWidth)->extent[0]+j] =
            ((*ptr)->getTotalUBound())[i*dimCount+j] -
            ((*ptr)->getExclusiveUBound())[i*dimCount+j];
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  
  }

#endif



  
  void FTN(c_esmc_griddestroy)(ESMCI::Grid **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_griddestroy()"
    //Initialize return code
    *rc = ESMC_RC_NOT_IMPL;
    // call into C++
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMCI::GridDestroy(ptr),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  } 
  
#undef  ESMC_METHOD
}


