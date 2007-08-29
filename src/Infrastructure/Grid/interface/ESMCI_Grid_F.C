// $Id: ESMCI_Grid_F.C,v 1.12 2007/08/29 17:30:10 oehmke Exp $
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
					  ESMC_IndexFlag *indexflag,
					  int *gridtype,
					  int *rc){
    int localrc;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridcreatefromdistgrid()"

    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;

    // call into C++
    *ptr = ESMCI::Grid::create(*nameLen, ESMC_NOT_PRESENT_FILTER(name),
      ESMC_NOT_PRESENT_FILTER(coordTypeKind), *distgrid, *dimmapArg,
      *lboundsArg, *uboundsArg, *coordRankArg, *coordDimMapArg,
      ESMC_NOT_PRESENT_FILTER(indexflag), ESMC_NOT_PRESENT_FILTER(gridtype),
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
			   ESMC_IndexFlag *_indexflag,
			   int *_gridtype,
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

    // indexflag
    if (ESMC_NOT_PRESENT_FILTER(_indexflag) != ESMC_NULL_POINTER)
      *_indexflag = grid->getIndexFlag();

    // gridtype
    if (ESMC_NOT_PRESENT_FILTER(_gridtype) != ESMC_NULL_POINTER)
      *_gridtype = grid->getGridType();

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
					  ESMC_IndexFlag *indexflag,
					  int *gridtype,
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
      ESMC_NOT_PRESENT_FILTER(coordTypeKind), tmp_distgrid, *dimmapArg,
      *lboundsArg, *uboundsArg, *coordRankArg, *coordDimMapArg,
      ESMC_NOT_PRESENT_FILTER(indexflag), ESMC_NOT_PRESENT_FILTER(gridtype));
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
}

  ///////////////////////////////////////////////////////////////////////////////////
  
  void FTN(c_esmc_gridgetlocaltileinfo)(ESMCI::Grid **_grid, int *_coord,
					int *_localDE, int *_staggerloc,  
                                        ESMCI::InterfaceInt **_exclusiveLBound,
                                        ESMCI::InterfaceInt **_exclusiveUBound,
                                        ESMCI::InterfaceInt **_staggerLBound,
                                        ESMCI::InterfaceInt **_staggerUBound,
                                        ESMCI::InterfaceInt **_computationalLBound,
                                        ESMCI::InterfaceInt **_computationalUBound,
                                        ESMCI::InterfaceInt **_totalLBound, 
                                        ESMCI::InterfaceInt **_totalUBound,
                                        int *_rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridgetlocaltileinfo()"
    int localrc;
    int distRank,undistRank,rank;
    int tile, coord,localDE,staggerloc;
    const int *arrayLbounds,*arrayUbounds;
    const int *gridLBounds,*gridUBounds;
    const int *arrayBnd;
    const int *coordRank;
    int **coordDimMap;
    int *gridMapDim, **coordMapDim;
    bool *gridIsDist, **coordIsDist;
    ESMCI::Grid *grid;
    ESMCI::Array *array;
    int offset[ESMF_MAXDIM];
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


    // Get Array From Grid
   docopy=ESMC_DATA_REF;
   array=grid->getCoordArray(&staggerloc, coord+1, &docopy, &localrc);
   if(ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
                                            ESMC_NOT_PRESENT_FILTER(_rc))) return;
 
   if (array == ESMC_NULL_POINTER) {
     ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_BAD,
          "- unset coord array", ESMC_NOT_PRESENT_FILTER(_rc));
     return;
   }

    // Array info
    arrayLbounds=array->getLBounds();
    arrayUbounds=array->getUBounds();


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

      // now that we have the array get the exclusive bounds of the localDE
      arrayBnd=array->getExclusiveLBound()+localDE*distRank;

      // Fill in the output array
      for (int i=0; i<coordRank[coord]; i++) {
	if (coordIsDist[coord][i]) {
	  (*_exclusiveLBound)->array[i]=arrayBnd[coordMapDim[coord][i]];
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

      // now that we have the array get the exclusive bounds of the localDE
      arrayBnd=array->getExclusiveUBound()+localDE*distRank;

      // Fill in the output array
      for (int i=0; i<coordRank[coord]; i++) {
	if (coordIsDist[coord][i]) {
	  (*_exclusiveUBound)->array[i]=arrayBnd[coordMapDim[coord][i]];
	} else {
	  (*_exclusiveUBound)->array[i]=gridUBounds[gridMapDim[coordDimMap[coord][i]]];
	}
      }
    }


    // fill staggerLBound
    if (*_staggerLBound != NULL){
      // staggerLBound was provided -> do some error checking
      if ((*_staggerLBound)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- staggerLBound array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_staggerLBound)->extent[0] < coordRank[coord]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- staggerLBound must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // now that we have the array get the stagger bounds of the localDE
      arrayBnd=array->getExclusiveLBound()+localDE*distRank;

      // Get stagger offset
     localrc=grid->getStaggerLWidth(staggerloc, localDE, offset);
     if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
					       ESMC_NOT_PRESENT_FILTER(_rc))) return; 

      // Fill in the output array
      for (int i=0; i<coordRank[coord]; i++) {
        int gi=coordDimMap[coord][i];
	if (coordIsDist[coord][i]) {
	  (*_staggerLBound)->array[i]=arrayBnd[coordMapDim[coord][i]]-offset[gi];
	} else {
	  (*_staggerLBound)->array[i]=gridLBounds[gridMapDim[gi]]-offset[gi];
	}
      }
    }


    // fill staggerUBound
    if (*_staggerUBound != NULL){
      // staggerUBound was provided -> do some error checking
      if ((*_staggerUBound)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- staggerUBound array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_staggerUBound)->extent[0] < coordRank[coord]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- staggerUBound must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // now that we have the array get the stagger bounds of the localDE
      arrayBnd=array->getExclusiveUBound()+localDE*distRank;

      // Get stagger offset
      localrc=grid->getStaggerUWidth(staggerloc, localDE, offset);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
						ESMC_NOT_PRESENT_FILTER(_rc))) return; 


      for (int i=0; i<coordRank[coord]; i++) {
        int gi=coordDimMap[coord][i];
	if (coordIsDist[coord][i]) {
	  (*_staggerUBound)->array[i]=arrayBnd[coordMapDim[coord][i]]+offset[gi];
	} else {
	  (*_staggerUBound)->array[i]=gridUBounds[gridMapDim[gi]]+offset[gi];
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

      // now that we have the array get the computational bounds of the localDE
      arrayBnd=array->getComputationalLBound()+localDE*distRank;

      for (int i=0; i<coordRank[coord]; i++) {
	if (coordIsDist[coord][i]) {
	  (*_computationalLBound)->array[i]=arrayBnd[coordMapDim[coord][i]];
	} else {
	  (*_computationalLBound)->array[i]=arrayLbounds[coordMapDim[coord][i]];
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

      // now that we have the array get the computational bounds of the localDE
      arrayBnd=array->getComputationalUBound()+localDE*distRank;

      for (int i=0; i<coordRank[coord]; i++) {
	if (coordIsDist[coord][i]) {
	  (*_computationalUBound)->array[i]=arrayBnd[coordMapDim[coord][i]];
	} else {
	  (*_computationalUBound)->array[i]=arrayUbounds[coordMapDim[coord][i]];
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
      arrayBnd=array->getTotalLBound()+localDE*distRank;

      for (int i=0; i<coordRank[coord]; i++) {
	if (coordIsDist[coord][i]) {
	  (*_totalLBound)->array[i]=arrayBnd[coordMapDim[coord][i]];
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
      arrayBnd=array->getTotalUBound()+localDE*distRank;

      for (int i=0; i<coordRank[coord]; i++) {
	if (coordIsDist[coord][i]) {
	  (*_totalUBound)->array[i]=arrayBnd[coordMapDim[coord][i]];
	} else {
	  (*_totalUBound)->array[i]=arrayUbounds[coordMapDim[coord][i]];
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
    *rc = ESMC_RC_NOT_IMPL;
    // call into C++
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMCI::Grid::destroy(ptr),
         ESMF_ERR_PASSTHRU, ESMC_NOT_PRESENT_FILTER(rc));
  } 

  ///////////////////////////////////////////////////////////////////////////////////
  
#undef  ESMC_METHOD
}


