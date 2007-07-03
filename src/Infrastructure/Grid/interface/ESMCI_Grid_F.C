// $Id: ESMCI_Grid_F.C,v 1.3 2007/07/03 21:28:19 oehmke Exp $
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
  void FTN(c_esmc_gridcreatefromdistgrid)(ESMCI::Grid **ptr, 
    int nameLen, char *name, ESMC_TypeKind *coordTypeKind, 
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
    *ptr = ESMCI::GridCreate(nameLen, ESMC_NOT_PRESENT_FILTER(name),
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
      const int **coordDimMap=grid->getCoordDimMap();
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


