// $Id: ESMCI_Grid_F.C,v 1.19 2007/10/30 19:31:55 oehmke Exp $
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

  ///////////////////////////////////////////////////////////////////////////////////

  void FTN(c_esmc_gridcommit)(ESMCI::Grid **grid, 
                                         int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridcommit()"

    int localrc;

    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;

    // call into C++
    localrc=(*grid)->commit();
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
}

  ///////////////////////////////////////////////////////////////////////////////////

  void FTN(c_esmc_gridcreateempty)(ESMCI::Grid **ptr, 
					  int *rc){
    int localrc;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridcreateempty()"

    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;

    // call into C++
    *ptr = ESMCI::Grid::create(&localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
}

  ///////////////////////////////////////////////////////////////////////////////////

  void FTN(c_esmc_gridcreatefromdistgrid)(ESMCI::Grid **ptr, 
    int *nameLen, char *name, ESMC_TypeKind *coordTypeKind, 
					  ESMCI::DistGrid **distgrid,
					  ESMCI::InterfaceInt **dimmapArg, 
					  ESMCI::InterfaceInt **lboundsArg,
					  ESMCI::InterfaceInt **uboundsArg,
					  ESMCI::InterfaceInt **coordRankArg,
					  ESMCI::InterfaceInt **coordDimMapArg,		  
					  ESMCI::InterfaceInt **gridEdgeLWidthArg,    	  
					  ESMCI::InterfaceInt **gridEdgeUWidthArg,    	  
					  ESMCI::InterfaceInt **gridAlignArg,		  
					  ESMC_IndexFlag *indexflag,
					  int *rc){
    int localrc;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridcreatefromdistgrid()"

    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;

    // call into C++
    *ptr = ESMCI::Grid::create(*nameLen, ESMC_NOT_PRESENT_FILTER(name),
      ESMC_NOT_PRESENT_FILTER(coordTypeKind), *distgrid, 
      *gridEdgeLWidthArg, *gridEdgeUWidthArg, *gridAlignArg, *dimmapArg,
      *lboundsArg, *uboundsArg, *coordRankArg, *coordDimMapArg,
      ESMC_NOT_PRESENT_FILTER(indexflag),
      &localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
}

  ///////////////////////////////////////////////////////////////////////////////////

  void FTN(c_esmc_gridget)(ESMCI::Grid **_grid, 
                           ESMC_TypeKind *_coordTypeKind,
                           int *_rank, int *_tileCount,
			   ESMCI::DistGrid **_distgrid,
                           int *_staggerLocCount, 
			   ESMCI::InterfaceInt **_dimmap, 
			   ESMCI::InterfaceInt **_lbounds,
			   ESMCI::InterfaceInt **_ubounds,
			   ESMCI::InterfaceInt **_coordRank,
			   ESMCI::InterfaceInt **_coordDimMap,		  
			   ESMCI::InterfaceInt **_gridEdgeLWidth, 	  
			   ESMCI::InterfaceInt **_gridEdgeUWidth,   
			   ESMCI::InterfaceInt **_gridAlign,		  
			   ESMC_IndexFlag *_indexflag,
                           int *_localDECount, int *_distRank, int *_undistRank, 
			   int *_rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridget()"

    int localrc;
    int distRank,undistRank,rank;
    ESMCI::Grid *grid;
    const int *dimmap;

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

    // distrank
    if (ESMC_NOT_PRESENT_FILTER(_distRank) != ESMC_NULL_POINTER)
      *_distRank = distRank;

    // undistrank
    if (ESMC_NOT_PRESENT_FILTER(_undistRank) != ESMC_NULL_POINTER)
      *_undistRank = undistRank;

    // undistrank
    if (ESMC_NOT_PRESENT_FILTER(_localDECount) != ESMC_NULL_POINTER)
      *_localDECount = grid->getDistGrid()->getDELayout()->getLocalDeCount();

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
      // fill in dimmap, and convert it to 1-based
      dimmap=grid->getDimMap();
      for (int i=0; i<distRank; i++) {
	(*_dimmap)->array[i]=dimmap[i]+1;
      }
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

    // get coordRank
    if (*_coordRank != NULL){
      // Error check
      if ((*_coordRank)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- coordRank array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_coordRank)->extent[0] < rank){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- coordRank array must be of size = the rank of the Grid", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      // fill in coordRank
      memcpy((*_coordRank)->array, grid->getCoordRank(), sizeof(int) * rank);
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
	  (*_coordDimMap)->array[k]=coordDimMap[j][i]+1; // Convert back to 1-based
	  k++;
	}
      } 
    }

    // get gridEdgeLWidth
    if (*_gridEdgeLWidth != NULL){
      // Error check
      if ((*_gridEdgeLWidth)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- gridEdgeLWidth array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_gridEdgeLWidth)->extent[0] < rank){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- gridEdgeLWidth array must be of size = the rank of the Grid", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      // fill in gridEdgeLWidth
      memcpy((*_gridEdgeLWidth)->array, grid->getGridEdgeLWidth(), sizeof(int) * rank);
    }

    // get gridEdgeUWidth
    if (*_gridEdgeUWidth != NULL){
      // Error check
      if ((*_gridEdgeUWidth)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- gridEdgeUWidth array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_gridEdgeUWidth)->extent[0] < rank){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- gridEdgeUWidth array must be of size = the rank of the Grid", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      // fill in gridEdgeUWidth
      memcpy((*_gridEdgeUWidth)->array, grid->getGridEdgeUWidth(), sizeof(int) * rank);
    }

    // get gridAlign
    if (*_gridAlign != NULL){
      // Error check
      if ((*_gridAlign)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- gridAlign array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_gridAlign)->extent[0] < rank){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- gridAlign array must be of size = the rank of the Grid", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      // fill in gridAlign
      memcpy((*_gridAlign)->array, grid->getGridAlign(), sizeof(int) * rank);
    }



    // indexflag
    if (ESMC_NOT_PRESENT_FILTER(_indexflag) != ESMC_NULL_POINTER)
      *_indexflag = grid->getIndexFlag();


    // return success
    if (_rc!=NULL) *_rc = ESMF_SUCCESS;
}



  ///////////////////////////////////////////////////////////////////////////////////

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
    *array = (*grid)->getCoordArray(ESMC_NOT_PRESENT_FILTER(staggerloc),
      *coord, ESMC_NOT_PRESENT_FILTER(docopy), 
      ESMC_NOT_PRESENT_FILTER(rc));
}


  ///////////////////////////////////////////////////////////////////////////////////

  void FTN(c_esmc_gridalloccoord)(ESMCI::Grid **grid, 
                                         int *staggerloc, 
                                         ESMCI::InterfaceInt **staggerLWidthArg, 
                                         ESMCI::InterfaceInt **staggerUWidthArg, 
                                         ESMCI::InterfaceInt **staggerAlignArg, 
                                         int *rc) {
    int localrc;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridalloccoord()"

    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;

    // call into C++
    localrc= (*grid)->allocCoordArray(ESMC_NOT_PRESENT_FILTER(staggerloc),
      *staggerLWidthArg, *staggerUWidthArg, *staggerAlignArg);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
}

  ///////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////

  void FTN(c_esmc_gridsetcoordfromarray)(ESMCI::Grid **grid, 
                                         int *staggerloc, 
                                         int *coord, 
                                         ESMCI::Array **array,
                                         ESMC_DataCopy *docopy, 
                                         int *rc) {
    int localrc;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridsetfromarray()"

    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;

    // call into C++
    localrc= (*grid)->setCoordArray(ESMC_NOT_PRESENT_FILTER(staggerloc),
      ESMC_NOT_PRESENT_FILTER(coord), *array, ESMC_NOT_PRESENT_FILTER(docopy));
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
}

  ///////////////////////////////////////////////////////////////////////////////////


  // - ESMF-public methods:        
  void FTN(c_esmc_gridsetfromdistgrid)(ESMCI::Grid **grid, 
    int *nameLen, char *name, ESMC_TypeKind *coordTypeKind, 
					  ESMCI::DistGrid **distgrid,
					  ESMCI::InterfaceInt **dimmapArg, 
					  ESMCI::InterfaceInt **lboundsArg,
					  ESMCI::InterfaceInt **uboundsArg,
					  ESMCI::InterfaceInt **coordRankArg,
					  ESMCI::InterfaceInt **coordDimMapArg,		  
					  ESMCI::InterfaceInt **gridEdgeLWidthArg,    	  
					  ESMCI::InterfaceInt **gridEdgeUWidthArg,    	  
					  ESMCI::InterfaceInt **gridAlignArg,		  
					  ESMC_IndexFlag *indexflag,
					  int *rc){
    int localrc;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridsetfromdistgrid()"
    ESMCI::DistGrid *tmp_distgrid;

    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;

    // translate optional distgrid
    if (ESMC_NOT_PRESENT_FILTER(distgrid)==ESMC_NULL_POINTER) {
      tmp_distgrid=ESMC_NULL_POINTER;
    } else {
      tmp_distgrid=*distgrid;

    }
    
    // call into C++
    localrc = (*grid)->set(*nameLen, ESMC_NOT_PRESENT_FILTER(name),
      ESMC_NOT_PRESENT_FILTER(coordTypeKind), tmp_distgrid, 
      *gridEdgeLWidthArg, *gridEdgeUWidthArg, *gridAlignArg, *dimmapArg,
      *lboundsArg, *uboundsArg, *coordRankArg, *coordDimMapArg,
      ESMC_NOT_PRESENT_FILTER(indexflag));
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
}

  ///////////////////////////////////////////////////////////////////////////////////
  
  void FTN(c_esmc_gridgetcoordbounds)(ESMCI::Grid **_grid, int *_localDE,
                                        int *_coord, int *_staggerloc,  
                                        ESMCI::InterfaceInt **_exclusiveLBound,
                                        ESMCI::InterfaceInt **_exclusiveUBound,
                                        ESMCI::InterfaceInt **_exclusiveCount,
                                        ESMCI::InterfaceInt **_computationalLBound,
                                        ESMCI::InterfaceInt **_computationalUBound,
                                        ESMCI::InterfaceInt **_computationalCount,
                                        ESMCI::InterfaceInt **_totalLBound, 
                                        ESMCI::InterfaceInt **_totalUBound,
                                        ESMCI::InterfaceInt **_totalCount,
                                        int *_rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridgetcoordbounds()"
    int localrc;
    int distRank,undistRank,rank;
    int tile, coord,localDE,staggerloc;
    const int *arrayLbounds,*arrayUbounds;
    const int *gridLBounds,*gridUBounds;
    const int *arrayLBnd, *arrayUBnd;
    const int *coordRank;
    int **coordDimMap;
    int *gridMapDim, **coordMapDim;
    bool *gridIsDist, **coordIsDist;
    ESMCI::Grid *grid;
    ESMCI::Array *array;
    int offsetU[ESMF_MAXDIM];
    int offsetL[ESMF_MAXDIM];
    int gridExLBnd[ESMF_MAXDIM];
    int gridExUBnd[ESMF_MAXDIM];
    ESMC_DataCopy docopy;

    // Get Grid pointer
    grid=*_grid;

    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;
    if (_rc!=NULL) *_rc = ESMC_RC_NOT_IMPL;

    // get some useful info
    rank = grid->getRank();
    distRank = grid->getDistRank();
    coordRank=grid->getCoordRank();
    coordDimMap=grid->getCoordDimMap();
    coordIsDist=grid->getCoordIsDist();
    coordMapDim=grid->getCoordMapDim();
    gridIsDist=grid->getGridIsDist();
    gridMapDim=grid->getGridMapDim();
    gridLBounds=grid->getLbounds();
    gridUBounds=grid->getUbounds();

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
      localDE=*_localDE; // already 0 based 
    }


    // staggerloc
    if (ESMC_NOT_PRESENT_FILTER(_staggerloc) == ESMC_NULL_POINTER) {
      staggerloc=0;
    } else {
      staggerloc=*_staggerloc; // already 0-based
    }


    // Input Error Checking
    if ((localDE < 0) || (localDE >=grid->getDistGrid()->getDELayout()->getLocalDeCount())) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
          "- localDE outside range on this processor", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
    }

   if ((coord < 0) || (coord >= rank)) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
          "- coord outside of range for grid", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
    }

   if ((staggerloc < 0) || (staggerloc >=  grid->getStaggerLocCount())) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
          "- staggerloc outside of range for grid", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
    }

   if (grid->getStatus() < ESMC_GRIDSTATUS_SHAPE_READY) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_WRONG,
          "- grid not ready for this operation ", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
    }


    // Get Array From Grid (only needed for computational and total bounds)
   if ((*_computationalLBound != NULL) ||
       (*_computationalUBound != NULL) ||
       (*_totalLBound != NULL) ||
       (*_totalUBound != NULL)) { 
     docopy=ESMC_DATA_REF;
     array=grid->getCoordArray(&staggerloc, coord+1, &docopy, &localrc);
     if(ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
                                              ESMC_NOT_PRESENT_FILTER(_rc))) return;
     
     if (array == ESMC_NULL_POINTER) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_BAD,
             "- unset coord array", ESMC_NOT_PRESENT_FILTER(_rc));
       return;
     }

    // Array undistributed bounds
    arrayLbounds=array->getLBounds();
    arrayUbounds=array->getUBounds();
   }

   // get grid exclusive bounds
   localrc=grid->getDistExclusiveLBound(localDE,gridExLBnd);
   if(ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
                                   ESMC_NOT_PRESENT_FILTER(_rc))) return;

   localrc=grid->getDistExclusiveUBound(localDE,gridExUBnd);
   if(ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
                                   ESMC_NOT_PRESENT_FILTER(_rc))) return;


    // fill exclusiveLBound
    if (*_exclusiveLBound != NULL){
      // exclusiveLBound was provided -> do some error checking
      if ((*_exclusiveLBound)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- exclusiveLBound array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_exclusiveLBound)->extent[0] < coordRank[coord]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- exclusiveLBound must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Fill in the output array
      for (int i=0; i<coordRank[coord]; i++) {
	if (coordIsDist[coord][i]) {
	  (*_exclusiveLBound)->array[i]=gridExLBnd[coordMapDim[coord][i]];
	} else {
	  (*_exclusiveLBound)->array[i]=gridLBounds[gridMapDim[coordDimMap[coord][i]]];;
	}
      }
    }



    // fill exclusiveUBound
    if (*_exclusiveUBound != NULL){
      // exclusiveUBound was provided -> do some error checking
      if ((*_exclusiveUBound)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- exclusiveUBound array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_exclusiveUBound)->extent[0] < coordRank[coord]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- exclusiveUBound must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Fill in the output array
      for (int i=0; i<coordRank[coord]; i++) {
	if (coordIsDist[coord][i]) {
	  (*_exclusiveUBound)->array[i]=gridExUBnd[coordMapDim[coord][i]];
	} else {
	  (*_exclusiveUBound)->array[i]=gridUBounds[gridMapDim[coordDimMap[coord][i]]];
	}
      }
    }


    // fill exclusiveCount
    if (*_exclusiveCount != NULL){
      // exclusiveCount was provided -> do some error checking
      if ((*_exclusiveCount)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- exclusiveCount array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_exclusiveCount)->extent[0] < coordRank[coord]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- exclusiveCount must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Fill in the output array
      for (int i=0; i<coordRank[coord]; i++) {
	if (coordIsDist[coord][i]) {
	  (*_exclusiveCount)->array[i]=gridExUBnd[coordMapDim[coord][i]]-gridExLBnd[coordMapDim[coord][i]]+1;
	} else {
	  (*_exclusiveCount)->array[i]=gridUBounds[gridMapDim[coordDimMap[coord][i]]]-
            gridLBounds[gridMapDim[coordDimMap[coord][i]]]+1; 
	}
      }
    }



    // fill computationalLBound
    if (*_computationalLBound != NULL){
      // computationalLBound was provided -> do some error checking
      if ((*_computationalLBound)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- computationalLBound array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_computationalLBound)->extent[0] < coordRank[coord]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- computationalLBound must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Get stagger offset
     localrc=grid->getLDEStaggerLOffset(staggerloc, localDE, offsetL);
     if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
					       ESMC_NOT_PRESENT_FILTER(_rc))) return; 

      // Fill in the output array
      for (int i=0; i<coordRank[coord]; i++) {
        int gi=coordDimMap[coord][i];
	if (coordIsDist[coord][i]) {
	  (*_computationalLBound)->array[i]=gridExLBnd[coordMapDim[coord][i]]+offsetL[gi];
	} else {
	  (*_computationalLBound)->array[i]=gridLBounds[gridMapDim[gi]]+offsetL[gi];
	}
      }
    }


    // fill computationalUBound
    if (*_computationalUBound != NULL){
      // computationalUBound was provided -> do some error checking
      if ((*_computationalUBound)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- computationalUBound array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_computationalUBound)->extent[0] < coordRank[coord]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- computationalUBound must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Get stagger offset
      localrc=grid->getLDEStaggerUOffset(staggerloc, localDE, offsetU);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
						ESMC_NOT_PRESENT_FILTER(_rc))) return; 


      for (int i=0; i<coordRank[coord]; i++) {
        int gi=coordDimMap[coord][i];
	if (coordIsDist[coord][i]) {
	  (*_computationalUBound)->array[i]=gridExUBnd[coordMapDim[coord][i]]-offsetU[gi];
	} else {
	  (*_computationalUBound)->array[i]=gridUBounds[gridMapDim[gi]]-offsetU[gi];
	}
      }
    }


    // fill computationalCount
    if (*_computationalCount != NULL){
      // computationalCount was provided -> do some error checking
      if ((*_computationalCount)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- computationalCount array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_computationalCount)->extent[0] < coordRank[coord]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- computationalCount must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Get stagger lower offset
     localrc=grid->getLDEStaggerLOffset(staggerloc, localDE, offsetL);
     if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
					       ESMC_NOT_PRESENT_FILTER(_rc))) return; 

      // Get stagger upper offset
      localrc=grid->getLDEStaggerUOffset(staggerloc, localDE, offsetU);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
						ESMC_NOT_PRESENT_FILTER(_rc))) return; 

      for (int i=0; i<coordRank[coord]; i++) {
        int gi=coordDimMap[coord][i];
	if (coordIsDist[coord][i]) {
	  (*_computationalCount)->array[i]=(gridExUBnd[coordMapDim[coord][i]]-offsetU[gi])-
                                           (gridExLBnd[coordMapDim[coord][i]]+offsetL[gi])+1;
	} else {
	  (*_computationalCount)->array[i]=(gridUBounds[gridMapDim[gi]]-offsetU[gi])-
                                           (gridLBounds[gridMapDim[gi]]+offsetL[gi])+1;
	}
      }
    }


    // fill totalLBound
    if (*_totalLBound != NULL){
      // totalLBound was provided -> do some error checking
      if ((*_totalLBound)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- totalLBound array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_totalLBound)->extent[0] < coordRank[coord]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- totalLBound must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // now that we have the array get the total bounds of the localDE
      arrayLBnd=array->getTotalLBound()+localDE*distRank;

      for (int i=0; i<coordRank[coord]; i++) {
	if (coordIsDist[coord][i]) {
	  (*_totalLBound)->array[i]=arrayLBnd[coordMapDim[coord][i]];
	} else {
	  (*_totalLBound)->array[i]=arrayLbounds[coordMapDim[coord][i]];
	}
      }
    }

    // fill totalUBound
    if (*_totalUBound != NULL){
      // totalUBound was provided -> do some error checking
      if ((*_totalUBound)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- totalUBound array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_totalUBound)->extent[0] < coordRank[coord]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- totalUBound must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // now that we have the array get the total bounds of the localDE
      arrayUBnd=array->getTotalUBound()+localDE*distRank;

      for (int i=0; i<coordRank[coord]; i++) {
	if (coordIsDist[coord][i]) {
	  (*_totalUBound)->array[i]=arrayUBnd[coordMapDim[coord][i]];
	} else {
	  (*_totalUBound)->array[i]=arrayUbounds[coordMapDim[coord][i]];
	}
      }
    }

    // fill totalCount
    if (*_totalCount != NULL){
      // totalCount was provided -> do some error checking
      if ((*_totalCount)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- totalCount array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_totalCount)->extent[0] < coordRank[coord]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- totalCount must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // now that we have the array get the total bounds of the localDE
      arrayLBnd=array->getTotalLBound()+localDE*distRank;

      // now that we have the array get the total bounds of the localDE
      arrayUBnd=array->getTotalUBound()+localDE*distRank;

      for (int i=0; i<coordRank[coord]; i++) {
	if (coordIsDist[coord][i]) {
	  (*_totalCount)->array[i]=arrayUBnd[coordMapDim[coord][i]]-arrayLBnd[coordMapDim[coord][i]]+1;
	} else {
	  (*_totalCount)->array[i]=arrayUbounds[coordMapDim[coord][i]]-arrayLbounds[coordMapDim[coord][i]]+1;
	}
      }
    }


    // return successfully
    if (_rc!=NULL) *_rc = ESMF_SUCCESS;
  
  }

  ///////////////////////////////////////////////////////////////////////////////////



  ///////////////////////////////////////////////////////////////////////////////////
  
  void FTN(c_esmc_gridgetplocaldepsloc)(ESMCI::Grid **_grid, 
					int *_localDE, int *_staggerloc,  
                                        ESMCI::InterfaceInt **_exclusiveLBound,
                                        ESMCI::InterfaceInt **_exclusiveUBound,
                                        ESMCI::InterfaceInt **_exclusiveCount,
                                        ESMCI::InterfaceInt **_computationalLBound,
                                        ESMCI::InterfaceInt **_computationalUBound,
                                        ESMCI::InterfaceInt **_computationalCount,
                                        int *_rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridgetplocaldepsloc()"
    int localrc;
    int distRank,undistRank,rank;
    int localDE,staggerloc;
    const int *gridLBounds,*gridUBounds;
    int *gridMapDim;
    bool *gridIsDist;
    ESMCI::Grid *grid;
    int offsetL[ESMF_MAXDIM];
    int offsetU[ESMF_MAXDIM];
    int gridExLBnd[ESMF_MAXDIM];
    int gridExUBnd[ESMF_MAXDIM];

    // Get Grid pointer
    grid=*_grid;

    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;
    if (_rc!=NULL) *_rc = ESMC_RC_NOT_IMPL;

    // get some useful info
    rank = grid->getRank();
    distRank = grid->getDistRank();
    gridIsDist=grid->getGridIsDist();
    gridMapDim=grid->getGridMapDim();
    gridLBounds=grid->getLbounds();
    gridUBounds=grid->getUbounds();


    // localDE
    if (ESMC_NOT_PRESENT_FILTER(_localDE) == ESMC_NULL_POINTER) {
      localDE=0;
    } else {
      localDE=*_localDE; // already 0 based 
    }


    // staggerloc
    if (ESMC_NOT_PRESENT_FILTER(_staggerloc) == ESMC_NULL_POINTER) {
      staggerloc=0;
    } else {
      staggerloc=*_staggerloc; // already 0-based
    }


    // Input Error Checking
    if ((localDE < 0) || (localDE >=grid->getDistGrid()->getDELayout()->getLocalDeCount())) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
          "- localDE outside range on this processor", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
    }

   if ((staggerloc < 0) || (staggerloc >=  grid->getStaggerLocCount())) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
          "- staggerloc outside of range for grid", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
    }

   if (grid->getStatus() < ESMC_GRIDSTATUS_SHAPE_READY) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_WRONG,
          "- grid not ready for this operation ", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
    }


   // get grid exclusive bounds
   localrc=grid->getDistExclusiveLBound(localDE,gridExLBnd);
   if(ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
                                   ESMC_NOT_PRESENT_FILTER(_rc))) return;

   localrc=grid->getDistExclusiveUBound(localDE,gridExUBnd);
   if(ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
                                   ESMC_NOT_PRESENT_FILTER(_rc))) return;


    // fill exclusiveLBound
    if (*_exclusiveLBound != NULL){
      // exclusiveLBound was provided -> do some error checking
      if ((*_exclusiveLBound)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- exclusiveLBound array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_exclusiveLBound)->extent[0] < rank){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- exclusiveLBound must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Fill in the output array
      for (int i=0; i<rank; i++) {
	if (gridIsDist[i]) {
	  (*_exclusiveLBound)->array[i]=gridExLBnd[gridMapDim[i]];
	} else {
	  (*_exclusiveLBound)->array[i]=gridLBounds[gridMapDim[i]];
	}
      }
    }



    // fill exclusiveUBound
    if (*_exclusiveUBound != NULL){
      // exclusiveUBound was provided -> do some error checking
      if ((*_exclusiveUBound)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- exclusiveUBound array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_exclusiveUBound)->extent[0] < rank){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- exclusiveUBound must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Fill in the output array
      for (int i=0; i<rank; i++) {
	if (gridIsDist[i]) {
	  (*_exclusiveUBound)->array[i]=gridExUBnd[gridMapDim[i]];
	} else {
	  (*_exclusiveUBound)->array[i]=gridUBounds[gridMapDim[i]];
	}
      }
    }

    // fill exclusiveCount
    if (*_exclusiveCount != NULL){
      // exclusiveCount was provided -> do some error checking
      if ((*_exclusiveCount)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- exclusiveCount array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_exclusiveCount)->extent[0] < rank){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- exclusiveCount must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Fill in the output array
      for (int i=0; i<rank; i++) {
	if (gridIsDist[i]) {
	  (*_exclusiveCount)->array[i]=gridExUBnd[gridMapDim[i]]-gridExLBnd[gridMapDim[i]]+1;
	} else {
	  (*_exclusiveCount)->array[i]=gridUBounds[gridMapDim[i]]-gridLBounds[gridMapDim[i]]+1;
	}
      }
    }




    // fill computationalLBound
    if (*_computationalLBound != NULL){
      // computationalLBound was provided -> do some error checking
      if ((*_computationalLBound)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- computationalLBound array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_computationalLBound)->extent[0] < rank){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- computationalLBound must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Get stagger offset
     localrc=grid->getLDEStaggerLOffset(staggerloc, localDE, offsetL);
     if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
					       ESMC_NOT_PRESENT_FILTER(_rc))) return; 

      // Fill in the output array
      for (int i=0; i<rank; i++) {
	if (gridIsDist[i]) {
	  (*_computationalLBound)->array[i]=gridExLBnd[gridMapDim[i]]+offsetL[i];
	} else {
	  (*_computationalLBound)->array[i]=gridLBounds[gridMapDim[i]]+offsetL[i];
	}
      }
    }


    // fill computationalUBound
    if (*_computationalUBound != NULL){
      // computationalUBound was provided -> do some error checking
      if ((*_computationalUBound)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- computationalUBound array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_computationalUBound)->extent[0] < rank){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- computationalUBound must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Get stagger offset
      localrc=grid->getLDEStaggerUOffset(staggerloc, localDE, offsetU);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
						ESMC_NOT_PRESENT_FILTER(_rc))) return; 


      for (int i=0; i<rank; i++) {
	if (gridIsDist[i]) {
	  (*_computationalUBound)->array[i]=gridExUBnd[gridMapDim[i]]-offsetU[i];
	} else {
	  (*_computationalUBound)->array[i]=gridUBounds[gridMapDim[i]]-offsetU[i];
	}
      }
    }


    // fill computationalCount
    if (*_computationalCount != NULL){
      // computationalCount was provided -> do some error checking
      if ((*_computationalCount)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- computationalCount array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_computationalCount)->extent[0] < rank){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- computationalCount must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Get lower stagger offset
     localrc=grid->getLDEStaggerLOffset(staggerloc, localDE, offsetL);
     if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
					       ESMC_NOT_PRESENT_FILTER(_rc))) return; 

      // Get upper stagger offset
     localrc=grid->getLDEStaggerUOffset(staggerloc, localDE, offsetU);
     if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
					       ESMC_NOT_PRESENT_FILTER(_rc))) return; 

      // Fill in the output array
      for (int i=0; i<rank; i++) {
	if (gridIsDist[i]) {
	  (*_computationalCount)->array[i]=(gridExUBnd[gridMapDim[i]]-offsetU[i])-(gridExLBnd[gridMapDim[i]]+offsetL[i])+1;
	} else {
	  (*_computationalCount)->array[i]=(gridUBounds[gridMapDim[i]]-offsetU[i])-(gridLBounds[gridMapDim[i]]+offsetL[i])+1;
	}
      }
    }


    // return successfully
    if (_rc!=NULL) *_rc = ESMF_SUCCESS;
  
  }

  ///////////////////////////////////////////////////////////////////////////////////




  ///////////////////////////////////////////////////////////////////////////////////
  
  void FTN(c_esmc_gridgetpsloc)(ESMCI::Grid **_grid, 
				int *_staggerloc,  
                                ESMCI::InterfaceInt **_computationalEdgeLWidth,
                                ESMCI::InterfaceInt **_computationalEdgeUWidth,
                                ESMCI::InterfaceInt **_lbound,
                                ESMCI::InterfaceInt **_ubound,
                                int *_rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridgetpsloc()"
    int localrc;
    int distRank,undistRank,rank;
    int staggerloc;
    const int *gridLBounds,*gridUBounds;
    const int *staggerLWidth,*staggerUWidth;
    const int *gridEdgeLWidth,*gridEdgeUWidth;
    int *gridMapDim;
    bool *gridIsDist;
    ESMCI::Grid *grid;
    int offsetL[ESMF_MAXDIM];
    int offsetU[ESMF_MAXDIM];
    int gridExLBnd[ESMF_MAXDIM];
    int gridExUBnd[ESMF_MAXDIM];

    // Get Grid pointer
    grid=*_grid;

    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;
    if (_rc!=NULL) *_rc = ESMC_RC_NOT_IMPL;

    // get some useful info
    rank = grid->getRank();
    distRank = grid->getDistRank();
    undistRank = grid->getUndistRank();
    gridIsDist=grid->getGridIsDist();
    gridMapDim=grid->getGridMapDim();
    gridLBounds=grid->getLbounds();
    gridUBounds=grid->getUbounds();


    // staggerloc
    if (ESMC_NOT_PRESENT_FILTER(_staggerloc) == ESMC_NULL_POINTER) {
      staggerloc=0;
    } else {
      staggerloc=*_staggerloc; // already 0-based
    }


   if ((staggerloc < 0) || (staggerloc >=  grid->getStaggerLocCount())) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
          "- staggerloc outside of range for grid", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
    }

   if (grid->getStatus() < ESMC_GRIDSTATUS_SHAPE_READY) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_WRONG,
          "- grid not ready for this operation ", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
    }

   // Get Grid Edge Widths
   gridEdgeLWidth=grid->getGridEdgeLWidth();
   gridEdgeUWidth=grid->getGridEdgeUWidth();

    // fill computationalEdgeLWidth
    if (*_computationalEdgeLWidth != NULL){
      // computationalEdgeLWidth was provided -> do some error checking
      if ((*_computationalEdgeLWidth)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- computationalEdgeLWidth array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_computationalEdgeLWidth)->extent[0] < distRank){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- computationalEdgeLWidth must at least be the same rank as the grid's distgrid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Get stagger width
     staggerLWidth=grid->getStaggerLWidth(staggerloc);

      // Fill in the output array
      // NOTE: - is because of direction of computationalEdgeWidth in Array
      for (int i=0; i<rank; i++) {
	if (gridIsDist[i]) {
	  (*_computationalEdgeLWidth)->array[gridMapDim[i]]=-(gridEdgeLWidth[i]-staggerLWidth[i]);
	}
      }
    }


    // fill computationalEdgeUWidth
    if (*_computationalEdgeUWidth != NULL){
      // computationalEdgeUWidth was provided -> do some error checking
      if ((*_computationalEdgeUWidth)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- computationalEdgeUWidth array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_computationalEdgeUWidth)->extent[0] < distRank){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- computationalEdgeUWidth must at least be the same rank as the grid's distgrid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Get stagger width
     staggerUWidth=grid->getStaggerUWidth(staggerloc);

      // Fill in the output array
      // NOTE: - is because of direction of computationalEdgeWidth in Array
      for (int i=0; i<rank; i++) {
	if (gridIsDist[i]) {
	  (*_computationalEdgeUWidth)->array[gridMapDim[i]]=-(gridEdgeUWidth[i]-staggerUWidth[i]);
	}
      }
    }


    // fill _lbound
    if (*_lbound != NULL){
      // lbound was provided -> do some error checking
      if ((*_lbound)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- lbound array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_lbound)->extent[0] < undistRank){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- lbound must at least be the same rank as the undistributed part of the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Get stagger width
      staggerLWidth=grid->getStaggerLWidth(staggerloc);

      for (int i=0; i<rank; i++) {
	if (!gridIsDist[i]) {
	  (*_lbound)->array[gridMapDim[i]]=gridLBounds[gridMapDim[i]]+(gridEdgeLWidth[i]-staggerLWidth[i]);
	}
      }
    }


    // fill _ubound
    if (*_ubound != NULL){
      // ubound was provided -> do some error checking
      if ((*_ubound)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- ubound array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_ubound)->extent[0] < undistRank){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- ubound must at least be the same rank as the undistributed part of the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Get stagger width
      staggerUWidth=grid->getStaggerUWidth(staggerloc);

      for (int i=0; i<rank; i++) {
	if (!gridIsDist[i]) {
	  (*_ubound)->array[gridMapDim[i]]=gridUBounds[gridMapDim[i]]-(gridEdgeUWidth[i]-staggerUWidth[i]);
	}
      }
    }

    // return successfully
    if (_rc!=NULL) *_rc = ESMF_SUCCESS;
  
  }




  ///////////////////////////////////////////////////////////////////////////////////
  
  void FTN(c_esmc_griddestroy)(ESMCI::Grid **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_griddestroy()"

    //Initialize return code
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;

    // call into C++
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMCI::Grid::destroy(ptr),
         ESMF_ERR_PASSTHRU, ESMC_NOT_PRESENT_FILTER(rc));
  } 

  ///////////////////////////////////////////////////////////////////////////////////



  // - ESMF-public methods:        
  void FTN(c_esmc_gridluadefault)(int *rank, 
                                  ESMCI::InterfaceInt **gridEdgeLWidthIn,    	  
                                  ESMCI::InterfaceInt **gridEdgeUWidthIn,    	  
                                  ESMCI::InterfaceInt **gridAlignIn,		  
                                  ESMCI::InterfaceInt **gridEdgeLWidthOut,    	  
                                  ESMCI::InterfaceInt **gridEdgeUWidthOut,    	  
                                  ESMCI::InterfaceInt **gridAlignOut,		  
                                  int *rc){
    int localrc;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridluadefault()"
    ESMCI::DistGrid *tmp_distgrid;

    //Initialize return code
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;

    // make sure input variables are present
    if (*gridEdgeLWidthOut == NULL){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
          "- gridEdgeLWidthOut must be present", ESMC_NOT_PRESENT_FILTER(rc));
        return;
      }

    if (*gridEdgeUWidthOut == NULL){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
          "- gridEdgeUWidthOut must be present", ESMC_NOT_PRESENT_FILTER(rc));
        return;
      }

    if (*gridAlignOut == NULL){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
          "- gridAlignOut must be present", ESMC_NOT_PRESENT_FILTER(rc));
        return;
      }


    // call into C++
   localrc=setGridDefaultsLUA(*rank,
     *gridEdgeLWidthIn, *gridEdgeUWidthIn, *gridAlignIn,
     (*gridEdgeLWidthOut)->array, (*gridEdgeUWidthOut)->array, (*gridAlignOut)->array);
   ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
                   ESMC_NOT_PRESENT_FILTER(rc));


    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
}





  ///////////////////////////////////////////////////////////////////////////////////


  
  void FTN(c_esmc_gridvalidate)(ESMCI::Grid **grid, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridvalidate()"

    //Initialize return code
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;

    // Check status
   if ((*grid)->getStatus() < ESMC_GRIDSTATUS_SHAPE_READY) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_WRONG,
          "- grid status below ESMC_GRIDSTATUS_SHAPE_READY ", ESMC_NOT_PRESENT_FILTER(rc));
        return;
    }

    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  } 

  ///////////////////////////////////////////////////////////////////////////////////


  
#undef  ESMC_METHOD
}


