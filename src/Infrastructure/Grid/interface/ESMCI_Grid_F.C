// $id: ESMCI_Grid_F.C,v 1.35 2008/02/28 00:42:23 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2012, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_Grid_F.C"
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------

#include <string.h>                       // for memcpy()

#include "ESMCI_Macros.h"
#include "ESMCI_DistGrid.h"

#include "ESMCI_Grid.h"

#include "ESMCI_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"             // for LogErr

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
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
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
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
}

  ///////////////////////////////////////////////////////////////////////////////////

  void FTN(c_esmc_gridcreatefromdistgrid)(ESMCI::Grid **ptr, 
    int *nameLen, char *name, ESMC_TypeKind *coordTypeKind, 
					  ESMCI::DistGrid **distgrid,
					  ESMCI::InterfaceInt **distgridToGridMapArg, 
                                          ESMC_CoordSys *coordSys, 
					  ESMCI::InterfaceInt **coordDimCountArg,
					  ESMCI::InterfaceInt **coordDimMapArg,		  
					  ESMCI::InterfaceInt **gridEdgeLWidthArg,    	  
					  ESMCI::InterfaceInt **gridEdgeUWidthArg,    	  
					  ESMCI::InterfaceInt **gridAlignArg,		  
					  ESMCI::InterfaceInt **gridMemLBoundArg,		  
					  ESMC_IndexFlag *indexflag,
                                          int *destroyDistgridArg, 
                                          int *destroyDELayoutArg, 
					  int *rc,
                                          ESMCI_FortranStrLenArg name_l){
    int localrc;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridcreatefromdistgrid()"
    bool destroyDistgrid, *destroyDistgridPtr;
    bool destroyDELayout, *destroyDELayoutPtr;

    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;

    // process destroyDistgrid
    if (ESMC_NOT_PRESENT_FILTER(destroyDistgridArg) != ESMC_NULL_POINTER) {
      destroyDistgrid=(*destroyDistgridArg==1)?true:false;
      destroyDistgridPtr=&destroyDistgrid;
    } else {
      destroyDistgridPtr=ESMC_NULL_POINTER;
    }

    // process destroyDELayout
    if (ESMC_NOT_PRESENT_FILTER(destroyDELayoutArg) != ESMC_NULL_POINTER) {
      destroyDELayout=(*destroyDELayoutArg==1)?true:false;
      destroyDELayoutPtr=&destroyDELayout;
    } else {
      destroyDELayoutPtr=ESMC_NULL_POINTER;
    }


    // call into C++
    *ptr = ESMCI::Grid::create(*nameLen, ESMC_NOT_PRESENT_FILTER(name),
                               ESMC_NOT_PRESENT_FILTER(coordTypeKind), *distgrid,
                               *gridEdgeLWidthArg, *gridEdgeUWidthArg, *gridAlignArg, *distgridToGridMapArg,
                                ESMC_NOT_PRESENT_FILTER(coordSys), *coordDimCountArg, *coordDimMapArg,
                               *gridMemLBoundArg, ESMC_NOT_PRESENT_FILTER(indexflag), 
                               destroyDistgridPtr, destroyDELayoutPtr, &localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
}

  ///////////////////////////////////////////////////////////////////////////////////

  void FTN(c_esmc_gridcreatedistgridarb)(ESMCI::Grid **ptr, 
                                         int *nameLen, char *name, ESMC_TypeKind *coordTypeKind, 
                                         ESMCI::DistGrid **distgrid,
                                         ESMCI::InterfaceInt **distDimArg, 
                                         int *arbDim,   
                                         ESMC_CoordSys *coordSys, 
                                         ESMCI::InterfaceInt **coordDimCountArg,
                                         ESMCI::InterfaceInt **coordDimMapArg,		  
                                         ESMCI::InterfaceInt **minIndexArg, 
                                         ESMCI::InterfaceInt **maxIndexArg,
                                         ESMCI::InterfaceInt **localArbIndexArg, 
                                         int *localArbIndexCount, 
                                         int *destroyDistgridArg, 
                                         int *destroyDELayoutArg, 
                                         int *rc,
                                         ESMCI_FortranStrLenArg name_l){
    int localrc;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridcreatedistgridarb()"
    bool destroyDistgrid, *destroyDistgridPtr;
    bool destroyDELayout, *destroyDELayoutPtr;

    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;

    // process destroyDistgrid
    if (ESMC_NOT_PRESENT_FILTER(destroyDistgridArg) != ESMC_NULL_POINTER) {
      destroyDistgrid=(*destroyDistgridArg==1)?true:false;
      destroyDistgridPtr=&destroyDistgrid;
    } else {
      destroyDistgridPtr=ESMC_NULL_POINTER;
    }

    // process destroyDELayout
    if (ESMC_NOT_PRESENT_FILTER(destroyDELayoutArg) != ESMC_NULL_POINTER) {
      destroyDELayout=(*destroyDELayoutArg==1)?true:false;
      destroyDELayoutPtr=&destroyDELayout;
    } else {
      destroyDELayoutPtr=ESMC_NULL_POINTER;
    }

    // call into C++
    *ptr = ESMCI::Grid::create(*nameLen, ESMC_NOT_PRESENT_FILTER(name),
      ESMC_NOT_PRESENT_FILTER(coordTypeKind), *distgrid, 
      *minIndexArg, *maxIndexArg, *localArbIndexArg, *localArbIndexCount,
      *distDimArg, *arbDim, ESMC_NOT_PRESENT_FILTER(coordSys), *coordDimCountArg, *coordDimMapArg,
      destroyDistgridPtr, destroyDELayoutPtr, &localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
}

  ///////////////////////////////////////////////////////////////////////////////////

  void FTN(c_esmc_gridget)(ESMCI::Grid **_grid, 
                           ESMC_TypeKind *_coordTypeKind,
                           int *_dimCount, 
			   int *_tileCount,
			   ESMCI::DistGrid **_distgrid,
                           int *_staggerLocCount, 
			   ESMCI::InterfaceInt **_distgridToGridMap, 
			   ESMCI::InterfaceInt **_coordDimCount,
			   int *_arbDim,
			   int *_rank,
			   int *_arbDimCount,
			   ESMCI::InterfaceInt **_coordDimMap,		  
			   ESMCI::InterfaceInt **_gridEdgeLWidth, 	  
			   ESMCI::InterfaceInt **_gridEdgeUWidth,   
			   ESMCI::InterfaceInt **_gridAlign,		  
			   ESMC_IndexFlag *_indexflag,
                           int *_localDECount,
			   int *_rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridget()"

    int localrc;
    int localArbIndexCount;
    int dimCount, distDimCount, dimCount1;
    ESMCI::Grid *grid;
    const int *distgridToGridMap;
    ESMC_GridDecompType decompType;

    // Get Grid pointer
    grid=*_grid;

    decompType = grid->getDecompType();

    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;
    if (_rc!=NULL) *_rc = ESMC_RC_NOT_IMPL;

    // make sure status is correct
    if (grid->getStatus() < ESMC_GRIDSTATUS_SHAPE_READY) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_WRONG,
              "- grid not ready for this operation ", ESMC_NOT_PRESENT_FILTER(_rc));
      return;
    }

    // Use grid access methods to retrieve information seperately
    // I'm a little leary of putting this much logic in an interface call, but
    // it makes things less convoluted to do it this way. 

    // get some useful info
    distDimCount=grid->getDistDimCount();
    dimCount = grid->getDimCount();


    // coordTypeKind
    if (ESMC_NOT_PRESENT_FILTER(_coordTypeKind) != ESMC_NULL_POINTER)
      *_coordTypeKind = grid->getTypeKind();

    // dimCount
    if (ESMC_NOT_PRESENT_FILTER(_dimCount) != ESMC_NULL_POINTER)
      *_dimCount = dimCount;

    // localDeCount
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

    // get distgridToGridMap 
    if (*_distgridToGridMap != NULL){
      // Error check
      if ((*_distgridToGridMap)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- distgridToGridMap array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      if (decompType == ESMC_GRID_NONARBITRARY) {
	  if ((*_distgridToGridMap)->extent[0] < dimCount){
	      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
	       "- distgridToGridMap array must be of size = the distributed rank of the Grid", ESMC_NOT_PRESENT_FILTER(_rc));
	      return;
	  }
	  // fill in distgridToGridMap, and convert it to 1-based
	  distgridToGridMap=grid->getDistgridToGridMap();
	  for (int i=0; i<dimCount; i++) {
	      (*_distgridToGridMap)->array[i]=distgridToGridMap[i]+1;
	  }
      } else {
	int totaldim = (*_distgridToGridMap)->extent[0];
	  if (totaldim < distDimCount){
	      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
	       "- distgridToGridMap array must be of size = the distributed rank of the Grid", ESMC_NOT_PRESENT_FILTER(_rc));
	      return;
	  }
	  // fill in distgridToGridMap, and convert it to 1-based
	  distgridToGridMap=grid->getDistgridToGridMap();
	  for (int i=0; i<distDimCount; i++) {
	      (*_distgridToGridMap)->array[i]=distgridToGridMap[i]+1;
	  }
	  for (int i=distDimCount; i<totaldim; i++) {
	    (*_distgridToGridMap)->array[i]=0;
	  }
      }
    }

    // get coordDimCount
    if (*_coordDimCount != NULL){
      // Error check
      if ((*_coordDimCount)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- coordDimCount array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_coordDimCount)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- coordDimCount array must be of size = the rank of the Grid", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      // fill in coordDimCount
      memcpy((*_coordDimCount)->array, grid->getCoordDimCount(), sizeof(int) * dimCount);
    }

    // get coordDimMap
    if (*_coordDimMap != NULL){
      // Error check
      if ((*_coordDimMap)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- coordDimMap array must be of rank 2", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if (((*_coordDimMap)->extent[0] < dimCount) || ((*_coordDimMap)->extent[1] < dimCount)){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- coordDimMap array must be of size = the rank of the Grid", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      // fill in coordDimMap
      int k=0;
      int **coordDimMap=grid->getCoordDimMap();
      for (int i=0; i<dimCount; i++) {
	for (int j=0; j<dimCount; j++) {
           // Note: order of i,j is reversed because of F vs. C array ordering
	  (*_coordDimMap)->array[k]=coordDimMap[j][i]+1; // Convert back to 1-based
	  k++;
	}
      } 
    }


    // find number of dimensions distributed arbitrarily
    dimCount1 = grid->getDistGrid()->getDimCount();
    distDimCount = dimCount - dimCount1 + 1;

    // get arbDim
    if (ESMC_NOT_PRESENT_FILTER(_arbDim) != ESMC_NULL_POINTER) {
      *_arbDim = grid->getArbDim();
    }

    // get rank -- same as distGrid dimCount
    if (ESMC_NOT_PRESENT_FILTER(_rank) != ESMC_NULL_POINTER) {
      *_rank = dimCount1;
    }

    // get arbDimCount 
    if (ESMC_NOT_PRESENT_FILTER(_arbDimCount) != ESMC_NULL_POINTER) {
      if (decompType == ESMC_GRID_NONARBITRARY) {
	*_arbDimCount = 0;
      } else {
	*_arbDimCount = distDimCount;
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
      if ((*_gridEdgeLWidth)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- gridEdgeLWidth array must be of size = the rank of the Grid", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      // fill in gridEdgeLWidth
      memcpy((*_gridEdgeLWidth)->array, grid->getGridEdgeLWidth(), sizeof(int) * dimCount);
    }

    // get gridEdgeUWidth
    if (*_gridEdgeUWidth != NULL){
      // Error check
      if ((*_gridEdgeUWidth)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- gridEdgeUWidth array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_gridEdgeUWidth)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- gridEdgeUWidth array must be of size = the rank of the Grid", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      // fill in gridEdgeUWidth
      memcpy((*_gridEdgeUWidth)->array, grid->getGridEdgeUWidth(), sizeof(int) * dimCount);
    }

    // get gridAlign
    if (*_gridAlign != NULL){
      // Error check
      if ((*_gridAlign)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- gridAlign array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_gridAlign)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- gridAlign array must be of size = the rank of the Grid", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      // fill in gridAlign
      memcpy((*_gridAlign)->array, grid->getGridAlign(), sizeof(int) * dimCount);
    }



    // indexflag
    if (ESMC_NOT_PRESENT_FILTER(_indexflag) != ESMC_NULL_POINTER)
      *_indexflag = grid->getIndexFlag();


    // return success
    if (_rc!=NULL) *_rc = ESMF_SUCCESS;
}

  ///////////////////////////////////////////////////////////////////////////////////

  void FTN(c_esmc_gridgetdecomptype)(ESMCI::Grid **grid, 
				     ESMC_GridDecompType *decompType,
				     int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridgetdecomptype()"
    *decompType = (*grid)->getDecompType();
    *rc = ESMF_SUCCESS;
  }
  ///////////////////////////////////////////////////////////////////////////////////

  void FTN(c_esmc_gridgetindex)(ESMCI::Grid **grid, 
                                int *tile, 
				ESMCI::InterfaceInt **minIndex,
				ESMCI::InterfaceInt **maxIndex,
                                int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridgetindex()"

    int localrc;
    int dimCount;   
    dimCount = (*grid)->getDimCount();
	
    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;
    
    // Get minIndex
    if (*minIndex != NULL) {
      // Error check
      if ((*minIndex)->dimCount != 1){
	ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
              "- minIndex array must be of rank 1", ESMC_NOT_PRESENT_FILTER(rc));
	return;
      }
      if ((*minIndex)->extent[0] < dimCount){
	ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
              "- minIndex array must be of size = the dimCount of the Grid", ESMC_NOT_PRESENT_FILTER(rc));
	return;
      }
      // fill in gridAlign
      memcpy((*minIndex)->array, (*grid)->getMinIndex(*tile), sizeof(int) * dimCount);
    }
	
    // Get maxIndex
    // Error check
    if ((*maxIndex)->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
	   "- maxIndex array must be of rank 1", ESMC_NOT_PRESENT_FILTER(rc));
      return;
    }
    if ((*maxIndex)->extent[0] < dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- maxIndex array must be of size = the dimCount of the Grid", ESMC_NOT_PRESENT_FILTER(rc));
      return;
    }
    // fill in gridAlign
    memcpy((*maxIndex)->array, (*grid)->getMaxIndex(*tile), sizeof(int) * dimCount);
    
    return;
  }

  ///////////////////////////////////////////////////////////////////////////////////

  void FTN(c_esmc_gridconvertindex)(ESMCI::Grid **grid, 
				    ESMCI::InterfaceInt **gridindex,
				    int *index1D,
				    int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridconvertindex()"

    int localrc;
    int dimCount;
    int distDimCount;
    int *localIndex;   

    dimCount = (*grid)->getDimCount();
    distDimCount = (*grid)->getDistDimCount();
	
    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;

    if ((*gridindex)->dimCount != 1) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
	   "- index array must be of rank 1", ESMC_NOT_PRESENT_FILTER(rc));
      return;
    }
    if ((*gridindex)->extent[0] != dimCount) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- index array must be of size = the distDimCount of the Grid", ESMC_NOT_PRESENT_FILTER(rc));
      return;
    }
    localIndex = new int[dimCount];
    memcpy(localIndex, (*gridindex)->array, dimCount*sizeof(int));
    // call into C++
    *index1D = (*grid)->convertIndex(localIndex);

    if (*index1D == -1) 
       *rc=ESMF_FAILURE;
    else
       *rc = ESMF_SUCCESS;
    delete [] localIndex;
    return;
  }


  ///////////////////////////////////////////////////////////////////////////////////

  void FTN(c_esmc_gridgetstatus)(ESMCI::Grid **_grid, 
                           ESMC_GridStatus *_status){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridgetstatus()"

    // Get Grid pointer
    *_status=(*_grid)->getStatus();

}


  ///////////////////////////////////////////////////////////////////////////////////

  void FTN(c_esmc_gridgetcoordintoarray)(ESMCI::Grid **grid, 
                                         int *staggerloc, 
                                         int *coord, 
                                         ESMCI::Array **array,
                                         ESMCI::CopyFlag *docopy, 
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

  void FTN(c_esmc_gridgetitemintoarray)(ESMCI::Grid **grid, 
                                         int *staggerloc, 
                                         int *item, 
                                         ESMCI::Array **array,
                                         ESMCI::CopyFlag *docopy, 
                                         int *rc) {
    int localrc;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridgetitemintoarray()"

    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;

    // call into C++
    *array = (*grid)->getItemArray(ESMC_NOT_PRESENT_FILTER(staggerloc),
      ESMC_NOT_PRESENT_FILTER(item),
      ESMC_NOT_PRESENT_FILTER(docopy), 
      ESMC_NOT_PRESENT_FILTER(rc));
}



  ///////////////////////////////////////////////////////////////////////////////////

  void FTN(c_esmc_gridaddcoord)(ESMCI::Grid **grid, 
                                         int *staggerloc, 
                                         ESMCI::InterfaceInt **staggerEdgeLWidthArg, 
                                         ESMCI::InterfaceInt **staggerEdgeUWidthArg, 
                                         ESMCI::InterfaceInt **staggerAlignArg, 
                                         ESMCI::InterfaceInt **staggerMemLBoundArg, 
                                         int *rc) {
    int localrc;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridaddcoord()"

    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;

    // call into C++
    localrc= (*grid)->addCoordArray(ESMC_NOT_PRESENT_FILTER(staggerloc),
      *staggerEdgeLWidthArg, *staggerEdgeUWidthArg, *staggerAlignArg, *staggerMemLBoundArg);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
}

  ///////////////////////////////////////////////////////////////////////////////////



  void FTN(c_esmc_gridaddcoordarb)(ESMCI::Grid **grid, 
                                         int *staggerloc, 
                                         int *rc) {
    int localrc;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridalloccoordarb()"

    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;

    // call into C++
    localrc= (*grid)->addCoordArrayArb(ESMC_NOT_PRESENT_FILTER(staggerloc));
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
}


  ///////////////////////////////////////////////////////////////////////////////////

  void FTN(c_esmc_gridaddcoordarraylist)(ESMCI::Grid **grid, 
                                         int *staggerloc, 
                                         int *arrayCount, 
                                         ESMCI::Array **arrayList,
                                         ESMCI::CopyFlag *docopy, 
                                         ESMCI::InterfaceInt **staggerEdgeLWidthArg, 
                                         ESMCI::InterfaceInt **staggerEdgeUWidthArg, 
                                         ESMCI::InterfaceInt **staggerAlignArg, 
                                         int *rc) {
    int localrc;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridaddcoordarraylist()"

    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;

    // call into C++
    localrc= (*grid)->addCoordFromArrayList(ESMC_NOT_PRESENT_FILTER(staggerloc),
                    *arrayCount, arrayList, ESMC_NOT_PRESENT_FILTER(docopy),
            *staggerEdgeLWidthArg, *staggerEdgeUWidthArg, *staggerAlignArg);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
}



  ///////////////////////////////////////////////////////////////////////////////////



  ///////////////////////////////////////////////////////////////////////////////////

  void FTN(c_esmc_gridadditem)(ESMCI::Grid **grid, 
                                         int *staggerloc, 
                                         int *item,
			                 ESMC_TypeKind *itemTypeKind,   
                                         ESMCI::InterfaceInt **staggerEdgeLWidthArg, 
                                         ESMCI::InterfaceInt **staggerEdgeUWidthArg, 
                                         ESMCI::InterfaceInt **staggerAlignArg, 
			       ESMCI::InterfaceInt **staggerMemLBoundArg, 
                                         int *rc) {
    int localrc;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridadditem()"

    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;

    // call into C++
    localrc= (*grid)->addItemArray(ESMC_NOT_PRESENT_FILTER(staggerloc),
      ESMC_NOT_PRESENT_FILTER(item), ESMC_NOT_PRESENT_FILTER(itemTypeKind),
				   *staggerEdgeLWidthArg, *staggerEdgeUWidthArg,
                                   *staggerAlignArg, *staggerMemLBoundArg);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
}

  ///////////////////////////////////////////////////////////////////////////////////

  void FTN(c_esmc_gridadditemarb)(ESMCI::Grid **grid, 
                                         int *staggerloc, 
                                         int *item,
			                 ESMC_TypeKind *itemTypeKind,   
                                         int *rc) {
    int localrc;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridadditemarb()"

    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;

    // call into C++
    localrc= (*grid)->addItemArrayArb(ESMC_NOT_PRESENT_FILTER(staggerloc),
      ESMC_NOT_PRESENT_FILTER(item), ESMC_NOT_PRESENT_FILTER(itemTypeKind));
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
}

  ///////////////////////////////////////////////////////////////////////////////////

  void FTN(c_esmc_gridsetcoordfromarray)(ESMCI::Grid **grid, 
                                         int *staggerloc, 
                                         int *coord, 
                                         ESMCI::Array **array,
                                         ESMCI::CopyFlag *docopy, 
                                         int *rc) {
    int localrc;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridsetfromarray()"

    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;

    // call into C++
    localrc= (*grid)->setCoordArray(ESMC_NOT_PRESENT_FILTER(staggerloc),
      ESMC_NOT_PRESENT_FILTER(coord), *array, ESMC_NOT_PRESENT_FILTER(docopy));
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
}

  ///////////////////////////////////////////////////////////////////////////////////


  // - ESMF-public methods:        
  void FTN(c_esmc_gridsetfromdistgrid)(ESMCI::Grid **grid, 
    int *nameLen, char *name, ESMC_TypeKind *coordTypeKind, 
                                       ESMCI::DistGrid **distgrid,
                                       ESMCI::InterfaceInt **distgridToGridMapArg, 
                                       ESMCI::InterfaceInt **distDimArg, 
                                       ESMC_CoordSys *coordSys, 
                                       ESMCI::InterfaceInt **coordDimCountArg,
                                       ESMCI::InterfaceInt **coordDimMapArg,		  
                                       ESMCI::InterfaceInt **minIndexArg,		  
                                       ESMCI::InterfaceInt **maxIndexArg,		  
                                       ESMCI::InterfaceInt **localArbIndexArg,		  
                                       int *localArbIndexCount,		  
                                       ESMCI::InterfaceInt **gridEdgeLWidthArg,    	  
                                       ESMCI::InterfaceInt **gridEdgeUWidthArg,    	  
                                       ESMCI::InterfaceInt **gridAlignArg,		  
                                       ESMCI::InterfaceInt **gridMemLBoundArg,		  
                                       ESMC_IndexFlag *indexflag,
                                       int *destroyDistgridArg, 
                                       int *destroyDELayoutArg, 
                                       int *rc,
                                       ESMCI_FortranStrLenArg name_l){
    int localrc;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridsetfromdistgrid()"
    ESMCI::DistGrid *tmp_distgrid;
    bool destroyDistgrid, *destroyDistgridPtr;
    bool destroyDELayout, *destroyDELayoutPtr;

    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;

    // process destroyDistgrid
    if (ESMC_NOT_PRESENT_FILTER(destroyDistgridArg) != ESMC_NULL_POINTER) {
      destroyDistgrid=(*destroyDistgridArg==1)?true:false;
      destroyDistgridPtr=&destroyDistgrid;
    } else {
      destroyDistgridPtr=ESMC_NULL_POINTER;
    }

    // process destroyDELayout
    if (ESMC_NOT_PRESENT_FILTER(destroyDELayoutArg) != ESMC_NULL_POINTER) {
      destroyDELayout=(*destroyDELayoutArg==1)?true:false;
      destroyDELayoutPtr=&destroyDELayout;
    } else {
      destroyDELayoutPtr=ESMC_NULL_POINTER;
    }

    // translate optional distgrid
    if (ESMC_NOT_PRESENT_FILTER(distgrid)==ESMC_NULL_POINTER) {
      tmp_distgrid=ESMC_NULL_POINTER;
    } else {
      tmp_distgrid=*distgrid;
    }
    
    // call into C++
    localrc = (*grid)->set(*nameLen, ESMC_NOT_PRESENT_FILTER(name),
      ESMC_NOT_PRESENT_FILTER(coordTypeKind), tmp_distgrid, 
      *gridEdgeLWidthArg, *gridEdgeUWidthArg, *gridAlignArg, 
      *distgridToGridMapArg, *distDimArg,
      *minIndexArg, *maxIndexArg, *localArbIndexArg, ESMC_NOT_PRESENT_FILTER(localArbIndexCount),
      ESMC_NOT_PRESENT_FILTER(coordSys),*coordDimCountArg, *coordDimMapArg,
      *gridMemLBoundArg, ESMC_NOT_PRESENT_FILTER(indexflag), 
       destroyDistgridPtr, destroyDELayoutPtr);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
}



  ///////////////////////////////////////////////////////////////////////////////////

  void FTN(c_esmc_gridsetitemfromarray)(ESMCI::Grid **grid, 
					int *staggerloc, 
					int *item, 
                                        ESMCI::Array **array,
					ESMCI::CopyFlag *docopy, 
                                        int *rc) {
    int localrc;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridsetitemfromarray()"

    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;

    // call into C++
    localrc= (*grid)->setItemArray(
      ESMC_NOT_PRESENT_FILTER(staggerloc), 
      ESMC_NOT_PRESENT_FILTER(item), 
      *array,
      ESMC_NOT_PRESENT_FILTER(docopy));
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
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
    int tile, coord,localDE,staggerloc,dimCount,distDimCount;
    const int *arrayUndistLBound,*arrayUndistUBound;
    const int *arrayLBnd, *arrayUBnd;
    const int *coordDimCount;
    int **coordDimMap;
    int **coordMapDim;
    bool **coordIsDist;
    ESMCI::Grid *grid;
    ESMCI::Array *array;
    int lBnd[ESMF_MAXDIM];
    int uBnd[ESMF_MAXDIM];
    int offsetU[ESMF_MAXDIM];
    int offsetL[ESMF_MAXDIM];
    int gridExLBnd[ESMF_MAXDIM];
    int gridExUBnd[ESMF_MAXDIM];
    int userIndexOffset[ESMF_MAXDIM];
    ESMCI::CopyFlag docopy;
    ESMC_GridDecompType decompType;
 
    // Get Grid pointer
    grid=*_grid;
    
    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;
    if (_rc!=NULL) *_rc = ESMC_RC_NOT_IMPL;
    
    // Check grid status
    if (grid->getStatus() < ESMC_GRIDSTATUS_SHAPE_READY) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_WRONG,
         "- grid not ready for this operation ", ESMC_NOT_PRESENT_FILTER(_rc));
      return;
    }
    
    // get some useful info
   dimCount=grid->getDimCount();
   distDimCount=grid->getDistDimCount();
   coordDimCount=grid->getCoordDimCount();
   coordDimMap=grid->getCoordDimMap();
   coordMapDim=grid->getCoordMapDim();
   coordIsDist=grid->getCoordIsDist();
   decompType=grid->getDecompType();

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
      if (grid->getDistGrid()->getDELayout()->getLocalDeCount()>1) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
         "- Must provide localDE if localDeCount >1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      } else {
        localDE=0;
      }
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

   if ((coord < 0) || (coord >= dimCount)) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
          "- coord outside of range for grid", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
    }

   if ((staggerloc < 0) || (staggerloc >=  grid->getStaggerLocCount())) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
          "- staggerloc outside of range for grid", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
    }

   // get grid exclusive bounds
   localrc=grid->getExclusiveLBound(staggerloc,localDE,gridExLBnd);
   if(ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                   ESMC_NOT_PRESENT_FILTER(_rc))) return;

   localrc=grid->getExclusiveUBound(staggerloc,localDE,gridExUBnd);
   if(ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                   ESMC_NOT_PRESENT_FILTER(_rc))) return;

   // Get Array
   array = ESMC_NULL_POINTER;   
   if (!grid->isEmptyCoordArray(staggerloc, coord)) {
     // Get Array    
     docopy=ESMCI::DATA_REF;
     array=grid->getCoordArray(&staggerloc, coord+1, &docopy, &localrc);
     if(ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
 					    ESMC_NOT_PRESENT_FILTER(_rc))) return;
     // Array undistributed bounds
     arrayUndistLBound=array->getUndistLBound();
     arrayUndistUBound=array->getUndistUBound();
   }

   // Compute offset if ESMF_INDEX_USER
   if (grid->getIndexFlag()==ESMF_INDEX_USER) {
     
     // Get staggerMemLBound
     const int *staggerMemLBound=grid->getStaggerMemLBound(staggerloc);

     // Compute offset as (gridMemLBound-Exclusive Lower Bounds)
     // Because in the Grid the computational bounds are always within
     // exclusive bounds, the exclusive lower bounds will always
     // be the lowest (unless there are non-zero totalwidths, but
     // since these bounds don't actually represent an Array there won't be).
	for (int i=0; i<coordDimCount[coord]; i++) {
	    userIndexOffset[i]=staggerMemLBound[coordDimMap[coord][i]]- 
                               gridExLBnd[coordDimMap[coord][i]];
	}
     
   } else {
     for (int i=0; i<coordDimCount[coord]; i++) {
       userIndexOffset[i]=0;
     }
   } 

    // fill exclusiveLBound
    if (*_exclusiveLBound != NULL){
      // exclusiveLBound was provided -> do some error checking
      if ((*_exclusiveLBound)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- exclusiveLBound array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_exclusiveLBound)->extent[0] < coordDimCount[coord]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- exclusiveLBound must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }



      // Get bounds depending on whether array exists
      if (array == ESMC_NULL_POINTER) {	
	// Fill in the output array
	for (int i=0; i<coordDimCount[coord]; i++) {
	    (*_exclusiveLBound)->array[i]=gridExLBnd[coordDimMap[coord][i]]+userIndexOffset[i];
	}
	
      } else {
	// given the array get the exclusive bounds of the localDE
	arrayLBnd=array->getExclusiveLBound()+localDE*coordDimCount[coord];
	
	for (int i=0; i<coordDimCount[coord]; i++) {
	    (*_exclusiveLBound)->array[i]=arrayLBnd[i];
	}	
      }

#if 0
	for (int i=0; i<coordDimCount[coord]; i++) {
	  printf("Coord=%d LDE=%d ELB[%d]=%d \n",coord,localDE,i,(*_exclusiveLBound)->array[i]);
	}
#endif

    }

    // fill exclusiveUBound
    if (*_exclusiveUBound != NULL){
      // exclusiveUBound was provided -> do some error checking
      if ((*_exclusiveUBound)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- exclusiveUBound array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_exclusiveUBound)->extent[0] < coordDimCount[coord]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- exclusiveUBound must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Get bounds depending on whether array exists
      if (array == ESMC_NULL_POINTER) {	
	// Fill in the output array
	for (int i=0; i<coordDimCount[coord]; i++) {
	    (*_exclusiveUBound)->array[i]=gridExUBnd[coordDimMap[coord][i]]+userIndexOffset[i];
	}
      } else {
	// given the array get the exclusive bounds of the localDE
	arrayUBnd=array->getExclusiveUBound()+localDE*coordDimCount[coord];
	
	for (int i=0; i<coordDimCount[coord]; i++) {
	    (*_exclusiveUBound)->array[i]=arrayUBnd[i];
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
      if ((*_exclusiveCount)->extent[0] < coordDimCount[coord]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- exclusiveCount must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }


      // Get bounds depending on whether array exists
      if (array == ESMC_NULL_POINTER) {	
	for (int i=0; i<coordDimCount[coord]; i++) {
	    (*_exclusiveCount)->array[i]=gridExUBnd[coordDimMap[coord][i]]-gridExLBnd[coordDimMap[coord][i]]+1;
	}
      } else {

	// given the array get the exclusive bounds of the localDE
	arrayLBnd=array->getExclusiveLBound()+localDE*coordDimCount[coord];
	arrayUBnd=array->getExclusiveUBound()+localDE*coordDimCount[coord];
	
	for (int i=0; i<coordDimCount[coord]; i++) {
	    (*_exclusiveCount)->array[i]=arrayUBnd[i]-arrayLBnd[i]+1;
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
      if ((*_computationalLBound)->extent[0] < coordDimCount[coord]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- computationalLBound must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Get bounds depending on whether array exists
      if (array == ESMC_NULL_POINTER) {	
         // Fill in the output array
         if (decompType == ESMC_GRID_NONARBITRARY) {	
           // Map to coordinate ordering		
	   for (int i=0; i<coordDimCount[coord]; i++) {
	     (*_computationalLBound)->array[i]=gridExLBnd[coordDimMap[coord][i]]+userIndexOffset[i];
	   }	  

         } else { // arbitrary grid
           // Map to coordinate ordering		
	    for (int i=0; i<coordDimCount[coord]; i++) {
 	          (*_computationalLBound)->array[i]=gridExLBnd[coordDimMap[coord][i]]+userIndexOffset[i];
	    }		
         }	  
      } else {
	// given the array get the exclusive bounds of the localDE
	arrayLBnd=array->getComputationalLBound()+localDE*coordDimCount[coord];
	
	for (int i=0; i<coordDimCount[coord]; i++) {
	    (*_computationalLBound)->array[i]=arrayLBnd[i];
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
      if ((*_computationalUBound)->extent[0] < coordDimCount[coord]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- computationalUBound must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Get bounds depending on whether array exists
      if (array == ESMC_NULL_POINTER) {	
         if (decompType == ESMC_GRID_NONARBITRARY) {
   	    // Map to coordinate ordering
	   for (int i=0; i<coordDimCount[coord]; i++) {
	         (*_computationalUBound)->array[i]=gridExUBnd[coordDimMap[coord][i]]+userIndexOffset[i];
	   }
         } else { // arbitrary grid
           /// TODO: figure out userIndexOffset for ARB grid
	   for (int i=0; i<coordDimCount[coord]; i++) {
	         (*_computationalUBound)->array[i]=gridExUBnd[coordDimMap[coord][i]]+userIndexOffset[i];
           }
         }
      } else {
	// given the array get the exclusive bounds of the localDE
	arrayUBnd=array->getComputationalUBound()+localDE*coordDimCount[coord];
		
	for (int i=0; i<coordDimCount[coord]; i++) {
	    (*_computationalUBound)->array[i]=arrayUBnd[i];
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
      if ((*_computationalCount)->extent[0] < coordDimCount[coord]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- computationalCount must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Get bounds depending on whether array exists
      if (array == ESMC_NULL_POINTER) {	
         if (decompType == ESMC_GRID_NONARBITRARY) {
   	    // Map to coordinate ordering
	   for (int i=0; i<coordDimCount[coord]; i++) {
	     (*_computationalCount)->array[i]=gridExUBnd[coordDimMap[coord][i]]-gridExLBnd[coordDimMap[coord][i]]+1;
	   }
         } else { // arbitrary grid
   	    // Map to coordinate ordering
	   for (int i=0; i<coordDimCount[coord]; i++) {
	         (*_computationalCount)->array[i]=gridExUBnd[coordDimMap[coord][i]]-gridExLBnd[coordDimMap[coord][i]]+1;
           }
         }
      } else {
	// given the array get the computational bounds of the localDE
	arrayLBnd=array->getComputationalLBound()+localDE*coordDimCount[coord];
	arrayUBnd=array->getComputationalUBound()+localDE*coordDimCount[coord];
		
	for (int i=0; i<coordDimCount[coord]; i++) {
	    (*_computationalCount)->array[i]=arrayUBnd[i]-arrayLBnd[i]+1;
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
      if ((*_totalLBound)->extent[0] < coordDimCount[coord]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- totalLBound must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Get bounds depending on whether array exists
      if (array == ESMC_NULL_POINTER) {	
         // Fill in the output array
         if (decompType == ESMC_GRID_NONARBITRARY) {	
           // Map to coordinate ordering		
	   for (int i=0; i<coordDimCount[coord]; i++) {
	     (*_totalLBound)->array[i]=gridExLBnd[coordDimMap[coord][i]]+userIndexOffset[i];
	   }	  

         } else { // arbitrary grid
           // Map to coordinate ordering		
	    for (int i=0; i<coordDimCount[coord]; i++) {
 	          (*_totalLBound)->array[i]=gridExLBnd[coordDimMap[coord][i]]+userIndexOffset[i];
	    }		
         }	  
      } else {
	// given the array get the exclusive bounds of the localDE
	arrayLBnd=array->getTotalLBound()+localDE*coordDimCount[coord];
	
	for (int i=0; i<coordDimCount[coord]; i++) {
	    (*_totalLBound)->array[i]=arrayLBnd[i];
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
      if ((*_totalUBound)->extent[0] < coordDimCount[coord]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- totalUBound must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Get bounds depending on whether array exists
      if (array == ESMC_NULL_POINTER) {	
         if (decompType == ESMC_GRID_NONARBITRARY) {
   	    // Map to coordinate ordering
	   for (int i=0; i<coordDimCount[coord]; i++) {
	         (*_totalUBound)->array[i]=gridExUBnd[coordDimMap[coord][i]]+userIndexOffset[i];
	   }
         } else { // arbitrary grid
           /// TODO: figure out userIndexOffset for ARB grid
	   for (int i=0; i<coordDimCount[coord]; i++) {
	         (*_totalUBound)->array[i]=gridExUBnd[coordDimMap[coord][i]]+userIndexOffset[i];
           }
         }
      } else {
	// now that we have the array get the total bounds of the localDE
	arrayUBnd=array->getTotalUBound()+localDE*coordDimCount[coord];
	
	for (int i=0; i<coordDimCount[coord]; i++) {
	  (*_totalUBound)->array[i]=arrayUBnd[i];
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
      if ((*_totalCount)->extent[0] < coordDimCount[coord]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- totalCount must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Get bounds depending on whether array exists
      if (array == ESMC_NULL_POINTER) {	
         if (decompType == ESMC_GRID_NONARBITRARY) {
   	    // Map to coordinate ordering
	   for (int i=0; i<coordDimCount[coord]; i++) {
	     (*_totalCount)->array[i]=gridExUBnd[coordDimMap[coord][i]]-gridExLBnd[coordDimMap[coord][i]]+1;
	   }
         } else { // arbitrary grid
   	    // Map to coordinate ordering
	   for (int i=0; i<coordDimCount[coord]; i++) {
	       (*_totalCount)->array[i]=gridExUBnd[coordDimMap[coord][i]]-gridExLBnd[coordDimMap[coord][i]]+1;
           }
         }
      } else {
	// given the array get the computational bounds of the localDE
	arrayLBnd=array->getTotalLBound()+localDE*coordDimCount[coord];
	arrayUBnd=array->getTotalUBound()+localDE*coordDimCount[coord];
		
	for (int i=0; i<coordDimCount[coord]; i++) {
	    (*_totalCount)->array[i]=arrayUBnd[i]-arrayLBnd[i]+1;
	}	
      }
    }

    // return successfully
    if (_rc!=NULL) *_rc = ESMF_SUCCESS;
  
  }



  ///////////////////////////////////////////////////////////////////////////////////
  
  void FTN(c_esmc_gridgetitembounds)(ESMCI::Grid **_grid, int *_localDE,
   				        int *_staggerloc, int *_item, 
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
#define ESMC_METHOD "c_esmc_gridgetitembounds()"
    int localrc;
    int item,localDE,staggerloc,dimCount;
    const int *arrayLBnd, *arrayUBnd;
    ESMCI::Grid *grid;
    ESMCI::Array *array;
    int gridExLBnd[ESMF_MAXDIM];
    int gridExUBnd[ESMF_MAXDIM];
    int userIndexOffset[ESMF_MAXDIM];
    ESMCI::CopyFlag docopy;
    ESMC_GridDecompType decompType;
 
    // Get Grid pointer
    grid=*_grid;
    
    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;
    if (_rc!=NULL) *_rc = ESMC_RC_NOT_IMPL;
    
    // Check grid status
    if (grid->getStatus() < ESMC_GRIDSTATUS_SHAPE_READY) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_WRONG,
         "- grid not ready for this operation ", ESMC_NOT_PRESENT_FILTER(_rc));
      return;
    }
    
    // get some useful info
   dimCount=grid->getDimCount();
   decompType=grid->getDecompType();

    // coord
    if (ESMC_NOT_PRESENT_FILTER(_item) == ESMC_NULL_POINTER) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
          "- must pass in valid item", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
    } else {
      item=*_item;
    }

    // localDE
    if (ESMC_NOT_PRESENT_FILTER(_localDE) == ESMC_NULL_POINTER) {
      if (grid->getDistGrid()->getDELayout()->getLocalDeCount()>1) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
         "- Must provide localDE if localDeCount >1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      } else {
        localDE=0;
      }
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

   if ((item < 0) || (item >= ESMC_GRIDITEM_COUNT)) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
          "- not a valid item", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
    }

   if ((staggerloc < 0) || (staggerloc >=  grid->getStaggerLocCount())) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
          "- staggerloc outside of range for grid", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
    }

   // get grid exclusive bounds
   localrc=grid->getExclusiveLBound(staggerloc,localDE,gridExLBnd);
   if(ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                   ESMC_NOT_PRESENT_FILTER(_rc))) return;

   localrc=grid->getExclusiveUBound(staggerloc,localDE,gridExUBnd);
   if(ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                   ESMC_NOT_PRESENT_FILTER(_rc))) return;

   // Get Array
   array = ESMC_NULL_POINTER;   
   if (grid->hasItemStaggerLoc(staggerloc, item)) {
     // Get Array    
     docopy=ESMCI::DATA_REF;
     array=grid->getItemArray(&staggerloc, &item, &docopy, &localrc);
     if(ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
 					    ESMC_NOT_PRESENT_FILTER(_rc))) return;
   }

   // Compute offset if ESMF_INDEX_USER
   if (grid->getIndexFlag()==ESMF_INDEX_USER) {
     
     // Get staggerMemLBound
     const int *staggerMemLBound=grid->getStaggerMemLBound(staggerloc);
     
     // Compute offset as (gridMemLBound-Exclusive Lower Bounds)
     // Because in the Grid the computational bounds are always within
     // exclusive bounds, the exclusive lower bounds will always
     // be the lowest (unless there are non-zero totalwidths, but
     // since these bounds don't actually represent an Array there won't be).
     for (int i=0; i<dimCount; i++) {
       userIndexOffset[i]=staggerMemLBound[i]-gridExLBnd[i];
     }
     
   } else {
     for (int i=0; i<dimCount; i++) {
       userIndexOffset[i]=0;
     }
   } 

    // fill exclusiveLBound
    if (*_exclusiveLBound != NULL){
      // exclusiveLBound was provided -> do some error checking
      if ((*_exclusiveLBound)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- exclusiveLBound array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_exclusiveLBound)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- exclusiveLBound must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Get bounds depending on whether array exists
      if (array == ESMC_NULL_POINTER) {	
	// Fill in the output array
	for (int i=0; i<dimCount; i++) {
	    (*_exclusiveLBound)->array[i]=gridExLBnd[i]+userIndexOffset[i];
	}
	
      } else {
	// given the array get the exclusive bounds of the localDE
	arrayLBnd=array->getExclusiveLBound()+localDE*dimCount;
	
	for (int i=0; i<dimCount; i++) {
	    (*_exclusiveLBound)->array[i]=arrayLBnd[i];
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
      if ((*_exclusiveUBound)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- exclusiveUBound must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Get bounds depending on whether array exists
      if (array == ESMC_NULL_POINTER) {	
	// Fill in the output array
	for (int i=0; i<dimCount; i++) {
	    (*_exclusiveUBound)->array[i]=gridExUBnd[i]+userIndexOffset[i];
	}
      } else {
	// given the array get the exclusive bounds of the localDE
	arrayUBnd=array->getExclusiveUBound()+localDE*dimCount;
	
	for (int i=0; i<dimCount; i++) {
	    (*_exclusiveUBound)->array[i]=arrayUBnd[i];
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
      if ((*_exclusiveCount)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- exclusiveCount must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }


      // Get bounds depending on whether array exists
      if (array == ESMC_NULL_POINTER) {	
	for (int i=0; i<dimCount; i++) {
	    (*_exclusiveCount)->array[i]=gridExUBnd[i]-gridExLBnd[i]+1;
	}
      } else {

	// given the array get the exclusive bounds of the localDE
	arrayLBnd=array->getExclusiveLBound()+localDE*dimCount;
	arrayUBnd=array->getExclusiveUBound()+localDE*dimCount;
	
	for (int i=0; i<dimCount; i++) {
	    (*_exclusiveCount)->array[i]=arrayUBnd[i]-arrayLBnd[i]+1;
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
      if ((*_computationalLBound)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- computationalLBound must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Get bounds depending on whether array exists
      if (array == ESMC_NULL_POINTER) {	
         // Fill in the output array
         if (decompType == ESMC_GRID_NONARBITRARY) {	
           // Map to coordinate ordering		
	   for (int i=0; i<dimCount; i++) {
	     (*_computationalLBound)->array[i]=gridExLBnd[i]+userIndexOffset[i];
	   }	  

         } else { // arbitrary grid
           // Map to coordinate ordering		
	    for (int i=0; i<dimCount; i++) {
 	          (*_computationalLBound)->array[i]=gridExLBnd[i]+userIndexOffset[i];
	    }		
         }	  
      } else {
	// given the array get the exclusive bounds of the localDE
	arrayLBnd=array->getComputationalLBound()+localDE*dimCount;
	
	for (int i=0; i<dimCount; i++) {
	    (*_computationalLBound)->array[i]=arrayLBnd[i];
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
      if ((*_computationalUBound)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- computationalUBound must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Get bounds depending on whether array exists
      if (array == ESMC_NULL_POINTER) {	
         if (decompType == ESMC_GRID_NONARBITRARY) {
   	    // Map to coordinate ordering
	   for (int i=0; i<dimCount; i++) {
	         (*_computationalUBound)->array[i]=gridExUBnd[i]+userIndexOffset[i];
	   }
         } else { // arbitrary grid
           /// TODO: figure out userIndexOffset for ARB grid
	   for (int i=0; i<dimCount; i++) {
	         (*_computationalUBound)->array[i]=gridExUBnd[i]+userIndexOffset[i];
           }
         }
      } else {
	// given the array get the exclusive bounds of the localDE
	arrayUBnd=array->getComputationalUBound()+localDE*dimCount;
		
	for (int i=0; i<dimCount; i++) {
	    (*_computationalUBound)->array[i]=arrayUBnd[i];
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
      if ((*_computationalCount)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- computationalCount must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Get bounds depending on whether array exists
      if (array == ESMC_NULL_POINTER) {	
         if (decompType == ESMC_GRID_NONARBITRARY) {
   	    // Map to coordinate ordering
	   for (int i=0; i<dimCount; i++) {
	     (*_computationalCount)->array[i]=gridExUBnd[i]-gridExLBnd[i]+1;
	   }
         } else { // arbitrary grid
   	    // Map to coordinate ordering
	   for (int i=0; i<dimCount; i++) {
	         (*_computationalCount)->array[i]=gridExUBnd[i]-gridExLBnd[i]+1;
           }
         }
      } else {
	// given the array get the computational bounds of the localDE
	arrayLBnd=array->getComputationalLBound()+localDE*dimCount;
	arrayUBnd=array->getComputationalUBound()+localDE*dimCount;
		
	for (int i=0; i<dimCount; i++) {
	    (*_computationalCount)->array[i]=arrayUBnd[i]-arrayLBnd[i]+1;
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
      if ((*_totalLBound)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- totalLBound must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Get bounds depending on whether array exists
      if (array == ESMC_NULL_POINTER) {	
         // Fill in the output array
         if (decompType == ESMC_GRID_NONARBITRARY) {	
           // Map to coordinate ordering		
	   for (int i=0; i<dimCount; i++) {
	     (*_totalLBound)->array[i]=gridExLBnd[i]+userIndexOffset[i];
	   }	  

         } else { // arbitrary grid
           // Map to coordinate ordering		
	    for (int i=0; i<dimCount; i++) {
 	          (*_totalLBound)->array[i]=gridExLBnd[i]+userIndexOffset[i];
	    }		
         }	  
      } else {
	// given the array get the exclusive bounds of the localDE
	arrayLBnd=array->getTotalLBound()+localDE*dimCount;
	
	for (int i=0; i<dimCount; i++) {
	    (*_totalLBound)->array[i]=arrayLBnd[i];
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
      if ((*_totalUBound)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- totalUBound must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Get bounds depending on whether array exists
      if (array == ESMC_NULL_POINTER) {	
         if (decompType == ESMC_GRID_NONARBITRARY) {
   	    // Map to coordinate ordering
	   for (int i=0; i<dimCount; i++) {
	         (*_totalUBound)->array[i]=gridExUBnd[i]+userIndexOffset[i];
	   }
         } else { // arbitrary grid
           /// TODO: figure out userIndexOffset for ARB grid
	   for (int i=0; i<dimCount; i++) {
	         (*_totalUBound)->array[i]=gridExUBnd[i]+userIndexOffset[i];
           }
         }
      } else {
	// now that we have the array get the total bounds of the localDE
	arrayUBnd=array->getTotalUBound()+localDE*dimCount;
	
	for (int i=0; i<dimCount; i++) {
	  (*_totalUBound)->array[i]=arrayUBnd[i];
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
      if ((*_totalCount)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- totalCount must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Get bounds depending on whether array exists
      if (array == ESMC_NULL_POINTER) {	
         if (decompType == ESMC_GRID_NONARBITRARY) {
   	    // Map to coordinate ordering
	   for (int i=0; i<dimCount; i++) {
	     (*_totalCount)->array[i]=gridExUBnd[i]-gridExLBnd[i]+1;
	   }
         } else { // arbitrary grid
   	    // Map to coordinate ordering
	   for (int i=0; i<dimCount; i++) {
	       (*_totalCount)->array[i]=gridExUBnd[i]-gridExLBnd[i]+1;
           }
         }
      } else {
	// given the array get the computational bounds of the localDE
	arrayLBnd=array->getTotalLBound()+localDE*dimCount;
	arrayUBnd=array->getTotalUBound()+localDE*dimCount;
		
	for (int i=0; i<dimCount; i++) {
	    (*_totalCount)->array[i]=arrayUBnd[i]-arrayLBnd[i]+1;
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
    int dimCount;
    int localDE,staggerloc;
    ESMCI::Grid *grid;
    int gridExLBnd[ESMF_MAXDIM];
    int gridExUBnd[ESMF_MAXDIM];
    int userIndexOffset[ESMF_MAXDIM];

    // Get Grid pointer
    grid=*_grid;

    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;
    if (_rc!=NULL) *_rc = ESMC_RC_NOT_IMPL;


    // Check grid status
   if (grid->getStatus() < ESMC_GRIDSTATUS_SHAPE_READY) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_WRONG,
          "- grid not ready for this operation ", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
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

   // Get Grid DimCount
   dimCount=grid->getDimCount();

   // get grid exclusive bounds
   localrc=grid->getExclusiveLBound(staggerloc,localDE,gridExLBnd);
   if(ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                   ESMC_NOT_PRESENT_FILTER(_rc))) return;

   localrc=grid->getExclusiveUBound(staggerloc,localDE,gridExUBnd);
   if(ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                   ESMC_NOT_PRESENT_FILTER(_rc))) return;

   // Compute offset if ESMF_INDEX_USER
   if (grid->getIndexFlag()==ESMF_INDEX_USER) {

     // Get staggerMemLBound
     const int *staggerMemLBound=grid->getStaggerMemLBound(staggerloc);

     // Compute offset as (gridMemLBound-Exclusive Lower Bounds)
     // Because in the Grid the computational bounds are always within
     // exclusive bounds, the exclusive lower bounds will always
     // be the lowest (unless there are non-zero totalwidths, but
     // since these bounds don't actually represent an Array there won't be).
     for (int i=0; i<dimCount; i++) {
	 userIndexOffset[i]=staggerMemLBound[i]-gridExLBnd[i];
     }     
   } else {
     for (int i=0; i< dimCount; i++) {
       userIndexOffset[i]=0;
     }
   } 
   

    // fill exclusiveLBound
    if (*_exclusiveLBound != NULL){
      // exclusiveLBound was provided -> do some error checking
      if ((*_exclusiveLBound)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- exclusiveLBound array must be of rank 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_exclusiveLBound)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- exclusiveLBound must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Fill in the output array
      for (int i=0; i<dimCount; i++) {
	(*_exclusiveLBound)->array[i]=gridExLBnd[i]+userIndexOffset[i];
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
      if ((*_exclusiveUBound)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- exclusiveUBound must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Fill in the output array
      for (int i=0; i<dimCount; i++) {     
	(*_exclusiveUBound)->array[i]=gridExUBnd[i]+userIndexOffset[i];
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
      if ((*_exclusiveCount)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- exclusiveCount must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Fill output array
      for (int i=0; i<dimCount; i++) {
	(*_exclusiveCount)->array[i]=gridExUBnd[i]-gridExLBnd[i]+1;
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
      if ((*_computationalLBound)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- computationalLBound must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Fill output array
      for (int i=0; i<dimCount; i++) {
	(*_computationalLBound)->array[i]=gridExLBnd[i]+userIndexOffset[i];
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
      if ((*_computationalUBound)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- computationalUBound must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Fill output array
      for (int i=0; i<dimCount; i++) {
	(*_computationalUBound)->array[i]=gridExUBnd[i]+userIndexOffset[i];
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
      if ((*_computationalCount)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- computationalCount must at least be the same rank as the the grid'", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      // Fill output array
      for (int i=0; i<dimCount; i++) {
	(*_computationalCount)->array[i]=gridExUBnd[i]-gridExLBnd[i]+1;
      }
    }
    

    // return successfully
    if (_rc!=NULL) *_rc = ESMF_SUCCESS;
  
  }

  ///////////////////////////////////////////////////////////////////////////////////



  ///////////////////////////////////////////////////////////////////////////////////
   void FTN(c_esmc_gridgetplocalde)(ESMCI::Grid **_grid, 
				   int *_localDE, 
				   int *_dimCount,
                                   int *_isLBound,
                                   int *_isUBound,
                                   int *_localArbIndexCount,			
                                   ESMCI::InterfaceInt **_localArbIndex,
                                   int *_rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridgetplocaldepsloc()"
    int localrc;
    int dimCount;
    int localDE,staggerloc;
    ESMCI::Grid *grid;


    // Get Grid pointer
    grid=*_grid;

    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;
    if (_rc!=NULL) *_rc = ESMC_RC_NOT_IMPL;


    // Check grid status
   if (grid->getStatus() < ESMC_GRIDSTATUS_SHAPE_READY) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_WRONG,
          "- grid not ready for this operation ", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
    }

    // localDE
    if (ESMC_NOT_PRESENT_FILTER(_localDE) == ESMC_NULL_POINTER) {
      localDE=0;
    } else {
      localDE=*_localDE; // already 0 based 
    }


     // Input Error Checking
    if ((localDE < 0) || (localDE >=grid->getDistGrid()->getDELayout()->getLocalDeCount())) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
          "- localDE outside range on this processor", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
    }

  
  // Get Grid DimCount
  dimCount=grid->getDimCount();
  
  // Make sure dimCount match
  if (*_dimCount != dimCount) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
      "- input arrays not the correct size to hold results ", ESMC_NOT_PRESENT_FILTER(_rc));
    return;
  }
  
    // get some useful info
    ESMC_GridDecompType decompType = grid->getDecompType();

    if (decompType == ESMC_GRID_NONARBITRARY) {
      // Fill in the output arrays
      for (int i=0; i<dimCount; i++) {
        if (grid->isLBndNT(localDE,i)) _isLBound[i]=1;
        else  _isLBound[i]=0;
        if (grid->isUBndNT(localDE,i)) _isUBound[i]=1;
        else  _isUBound[i]=0;
      }
    }
  

  // get localArbIndexCount
  if (ESMC_NOT_PRESENT_FILTER(_localArbIndexCount) != ESMC_NULL_POINTER) {
      *_localArbIndexCount = grid->getLocalIndexCount();
  }

    // find number of dimensions distributed arbitrarily
    int dimCount1 = grid->getDistGrid()->getDimCount();
    int distDimCount = dimCount - dimCount1 + 1;

    // get localArbIndex
    if (*_localArbIndex != NULL) {
      int localArbIndexCount = grid->getLocalIndexCount();
      if ((*_localArbIndex)->dimCount != 2) {
	ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
	    "- localArbIndex array must be of rank 2", ESMC_NOT_PRESENT_FILTER(_rc));
	return;
      }
      if ((*_localArbIndex)->extent[0] < localArbIndexCount){
	ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
	      "- 1st dimension of localArbIndex array must be of size = localArbIndexCount", ESMC_NOT_PRESENT_FILTER(_rc));
	return;
      }

      if ((*_localArbIndex)->extent[1] < distDimCount){
	ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
	      "- 2nd dimension of localArbIndex array must match the arbitrarily distributed dimensions", ESMC_NOT_PRESENT_FILTER(_rc));
	return;
      }
     
      // fill in localArbIndex		
      int **localArbIndex = grid->getLocalIndices();
      int k=0;
      for (int j=0; j<distDimCount; j++) {
	for (int i=0; i<localArbIndexCount; i++) {
           // Note: order of i,j is reversed because of F vs. C array ordering
	  (*_localArbIndex)->array[k]=localArbIndex[i][j]; 
	  k++;
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
   			        ESMCI::DistGrid **_staggerdistgrid,
                                int *_rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridgetpsloc()"
    int localrc;
    int distDimCount,dimCount;
    int staggerloc;
    ESMCI::Grid *grid;
    const ESMCI::DistGrid *distgrid;
    ESMC_GridDecompType decompType;   

    // Get Grid pointer
    grid=*_grid;

    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;
    if (_rc!=NULL) *_rc = ESMC_RC_NOT_IMPL;

    // Check grid status
   if (grid->getStatus() < ESMC_GRIDSTATUS_SHAPE_READY) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_WRONG,
          "- grid not ready for this operation ", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
    }

    // staggerloc
    if (ESMC_NOT_PRESENT_FILTER(_staggerloc) == ESMC_NULL_POINTER) {
      staggerloc=0;
    } else {
      staggerloc=*_staggerloc; // already 0-based
    }

    // get some useful info
    decompType = grid->getDecompType();

    if (decompType == ESMC_GRID_NONARBITRARY) {
      if ((staggerloc < 0) || (staggerloc >=  grid->getStaggerLocCount())) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
	      "- staggerloc outside of range for grid", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
    } else {
      if (staggerloc != 0) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
	      "- arbitrary grid only supports center staggerloc", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
    }

   if (grid->getStatus() < ESMC_GRIDSTATUS_SHAPE_READY) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_WRONG,
          "- grid not ready for this operation ", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
    }


   // Get stagger distgrid
   if (ESMC_NOT_PRESENT_FILTER(_staggerdistgrid) != ESMC_NULL_POINTER) {
     localrc=grid->getStaggerDistgrid(staggerloc, _staggerdistgrid);
     if(ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
			         ESMC_NOT_PRESENT_FILTER(_rc))) return;
   }
  
    // return successfully
    if (_rc!=NULL) *_rc = ESMF_SUCCESS;
  
  }


  ///////////////////////////////////////////////////////////////////////////////////
  
  void FTN(c_esmc_gridgetpslocptile)(ESMCI::Grid **_grid, 
                                int *_tile,
				int *_staggerloc,  
                                ESMCI::InterfaceInt **_minIndex,
                                ESMCI::InterfaceInt **_maxIndex,
                                int *_rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridgetpslocptile()"
    int localrc;
    int distDimCount,dimCount;
    int staggerloc;
    const int *staggerEdgeLWidth,*staggerEdgeUWidth;
    const int *gridEdgeLWidth,*gridEdgeUWidth;
    int *gridMapDim;
    ESMCI::Grid *grid;
    ESMCI::DistGrid *staggerdistgrid;
    int offsetL[ESMF_MAXDIM];
    int offsetU[ESMF_MAXDIM];
    int gridExLBnd[ESMF_MAXDIM];
    int gridExUBnd[ESMF_MAXDIM];
    ESMC_GridDecompType decompType;   
    const int *minIndex, *maxIndex;
    int tile0b;

    // Get Grid pointer
    grid=*_grid;


    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;
    if (_rc!=NULL) *_rc = ESMC_RC_NOT_IMPL;

    // Check grid status
   if (grid->getStatus() < ESMC_GRIDSTATUS_SHAPE_READY) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_WRONG,
          "- grid not ready for this operation ", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
    }

    // get some useful info
    dimCount = grid->getDimCount();
    decompType = grid->getDecompType();
    minIndex = grid->getMinIndex(0);
    maxIndex = grid->getMaxIndex(0);
    gridMapDim =grid->getGridMapDim();

    // staggerloc
    if (ESMC_NOT_PRESENT_FILTER(_staggerloc) == ESMC_NULL_POINTER) {
      staggerloc=0;
    } else {
      staggerloc=*_staggerloc; // already 0-based
    }


    if (decompType == ESMC_GRID_NONARBITRARY) {
      if ((staggerloc < 0) || (staggerloc >=  grid->getStaggerLocCount())) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
	      "- staggerloc outside of range for grid", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
    } else {
      if (staggerloc != 0) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
	      "- arbitrary grid only supports center staggerloc", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
    }


    // tile
    if (ESMC_NOT_PRESENT_FILTER(_tile) == ESMC_NULL_POINTER) {
      tile0b=0;
    } else {
      tile0b=*_tile-1; 
    }



    // Check tile
    if ((tile0b < 0) || (tile0b >=grid->getTileCount())) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
       "- tile outside of range for grid", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
    }
    
    
    // Get stagger distgrid
    localrc=grid->getStaggerDistgrid(staggerloc, &staggerdistgrid);
    if(ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                             ESMC_NOT_PRESENT_FILTER(_rc))) return;


   // fill minIndex
   if (*_minIndex != NULL){
     if ((*_minIndex)->dimCount != 1){
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- minIndex array must be of dimCount 1", ESMC_NOT_PRESENT_FILTER(_rc));
       return;
     }
     if ((*_minIndex)->extent[0] < dimCount){
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- minIndex must at least be the same dimCount as the grid's", ESMC_NOT_PRESENT_FILTER(_rc));
       return;
     }

     if (decompType == ESMC_GRID_NONARBITRARY) {
       // get the minIndex of the tile
       const int *minIndexDG=staggerdistgrid->getMinIndexPDimPTile()+tile0b*dimCount;


       // Fill in the output array
       for (int i=0; i<dimCount; i++) {
         (*_minIndex)->array[i]=minIndexDG[gridMapDim[i]];
       }

     } else {
       for (int i=0; i<dimCount; i++) {
	 (*_minIndex)->array[i]=minIndex[i];
       }
     }
   }

    // fill maxIndex
    if (*_maxIndex != NULL){
      if ((*_maxIndex)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- minIndex array must be of dimCount 1", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }
      if ((*_maxIndex)->extent[0] < dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- minIndex must at least be the same dimCount as the grid's", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
      }

      if (decompType == ESMC_GRID_NONARBITRARY) {

	// get the minIndex of the tile
	const int *maxIndexDG=staggerdistgrid->getMaxIndexPDimPTile()+tile0b*dimCount;

	// Fill in the output array
	for (int i=0; i<dimCount; i++) {
	  (*_maxIndex)->array[i]=maxIndexDG[gridMapDim[i]];
	}
      } else {
	for (int i=0; i<dimCount; i++) {
	  (*_maxIndex)->array[i]=maxIndex[i];
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
         ESMCI_ERR_PASSTHRU, ESMC_NOT_PRESENT_FILTER(rc));
  } 

  ///////////////////////////////////////////////////////////////////////////////////



  // - ESMF-public methods:        
  void FTN(c_esmc_gridluadefault)(int *dimCount, 
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
    localrc=setGridDefaultsLUA(*dimCount,
       *gridEdgeLWidthIn, *gridEdgeUWidthIn, *gridAlignIn,
       (*gridEdgeLWidthOut)->array, (*gridEdgeUWidthOut)->array, (*gridAlignOut)->array);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
					  ESMC_NOT_PRESENT_FILTER(rc));


    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }


  void FTN(c_esmc_gridserialize)(ESMCI::Grid **grid, char *buf, int *length,
    int *offset, ESMC_AttReconcileFlag *attreconflag,
    ESMC_InquireFlag *inquireflag, int *rc,
    ESMCI_FortranStrLenArg buf_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridserialize()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*grid)->serialize(
      buf, length, offset, *attreconflag, *inquireflag),
      ESMCI_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_griddeserialize)(ESMCI::Grid **grid, char *buf,
    int *offset, ESMC_AttReconcileFlag *attreconflag, int *rc,
    ESMCI_FortranStrLenArg buf_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_griddeserialize()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    *grid = new ESMCI::Grid(-1);  // prevent baseID counter increment
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*grid)->deserialize(
      buf, offset, *attreconflag),
      ESMCI_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }




  void FTN(c_esmc_gridmatch)(ESMCI::Grid **ptr1, ESMCI::Grid **ptr2,
    int *matchResult, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridmatch()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    // Check if Grids match
    bool match=ESMCI::Grid::match(*ptr1,*ptr2, &localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
					  ESMC_NOT_PRESENT_FILTER(rc));  

    // Return result
    if (match) *matchResult=1;
    else *matchResult=0;
   
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }


  ///////////////////////////////////////////////////////////////////////////////////



  void FTN(c_esmc_gridvalidate)(ESMCI::Grid **_grid, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridvalidate()"

    //Initialize return code
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;

    // Check status
   if ((*_grid)->getStatus() < ESMC_GRIDSTATUS_SHAPE_READY) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_WRONG,
          "- grid status below ESMC_GRIDSTATUS_SHAPE_READY ", ESMC_NOT_PRESENT_FILTER(rc));
        return;
    }



#if 0
  
   ESMCI::Grid *grid;

   // Get grid
   grid=*_grid;

   // turn on sphere
   grid->setSphere();

#if 0
   // Grid Node Iterator
   ESMCI::GridIter *iter=new ESMCI::GridIter(grid,0,true);
   ESMCI::GridIter *iter2=new ESMCI::GridIter(grid,0,false);

   printf("----------- \n");
   int i=0;
   for(iter->toBeg(); !iter->isDone(); iter->adv()) {

#if 0
     ESMC_R8 coord[2];

       iter->getCoord(coord);

       printf("%d :: GID=%d LID=%d local=%d  shared=%d (%f,%f) \n",i,iter->getGlobalID(),iter->getLocalID(),iter->isLocal(),iter->isShared(),coord[0],coord[1]);
#endif

       printf("%d :: GID=%d poleID=%d \n",i,iter->getGlobalID(),iter->getPoleID());



#if 0 
     int lid;

     lid=iter->getLocalID();

    if (iter2->moveToLocalID(lid)->getLocalID() != lid) {
       print("ERROR :: %d \n",lid);      
     } else {
        printf("%d :: %d == %d \n",i,lid,iter2->moveToLocalID(lid)->getLocalID());      
     }
       printf(">>>> %d :: %d == %d \n",i,lid,iter2->moveToLocalID(lid)->getLocalID());      
#endif


     i++;
   }
   printf("%d ----------- \n",i);
#endif

#if 0
   // Grid Cell Iterator
   ESMCI::GridIter *ni=new ESMCI::GridIter(grid,0,true);

   ESMCI::GridCellIter *iter=new ESMCI::GridCellIter(grid,0);
   ESMCI::GridCellIter *iter2=new ESMCI::GridCellIter(grid,0);

   printf("Grid Cells ----------- \n");
   int i=0;
   for(iter->toBeg(); !iter->isDone(); iter->adv()) {

     int num, cnr[4], ngid[4];

     iter->getCornersCellNodeLocalID(&num, cnr);
     for (int i=0; i<4; i++) {
       ngid[i]=ni->moveToLocalID(cnr[i])->getGlobalID();
     }
   

     printf("%d :: GID=%d >>  %d %d %d %d \n",i,iter->getGlobalID(),ngid[0],ngid[1],ngid[2],ngid[3]);

#if 0
     int lid;

     lid=iter->getLocalID();

    if (iter2->moveToLocalID(lid)->getLocalID() != lid) {
       printf("ERROR :: %d \n",lid);      
     } else {
        printf("%d :: %d == %d \n",i,lid,iter2->moveToLocalID(lid)->getLocalID());      
     }
       printf(">>>> %d :: %d == %d \n",i,lid,iter2->moveToLocalID(lid)->getLocalID());      
#endif


     i++;
   }
   printf("%d ----------- \n",i);
#endif
#endif

    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  } 

  ///////////////////////////////////////////////////////////////////////////////////



  ///////////////////////////////////////////////////////////////////////////////////
  
  void FTN(c_esmc_gridgetcoordr8)(ESMCI::Grid **_grid, 
		       		int *_localDE, int *_staggerloc,  
                                int *index, ESMC_R8 *coord, int *_rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridgetcoordr8()"
    int localrc;
    int dimCount;
    ESMCI::Grid *grid;

    // Get Grid pointer
    grid=*_grid;

    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;
    if (_rc!=NULL) *_rc = ESMC_RC_NOT_IMPL;


    // Check grid status
   if (grid->getStatus() < ESMC_GRIDSTATUS_SHAPE_READY) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_WRONG,
          "- grid not ready for this operation ", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
    }

   // TODO: Make sure coords are within the correct bounds

   // TODO: make sure _localDE and _staggerloc are not NULL?

    // Input Error Checking
    if ((*_localDE < 0) || (*_localDE >=grid->getDistGrid()->getDELayout()->getLocalDeCount())) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
          "- localDE outside range on this processor", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
    }

   if ((*_staggerloc < 0) || (*_staggerloc >=  grid->getStaggerLocCount())) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
          "- staggerloc outside of range for grid", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
    }


   // Get Coords 
   grid->getCoord(*_staggerloc, *_localDE, index, coord);


    // return successfully
    if (_rc!=NULL) *_rc = ESMF_SUCCESS;
  
  }



  ///////////////////////////////////////////////////////////////////////////////////
  
  void FTN(c_esmc_gridgetcoordr4)(ESMCI::Grid **_grid, 
		       		int *_localDE, int *_staggerloc,  
                                int *index, ESMC_R4 *coord, int *_rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridgetcoordr4()"
    int localrc;
    int dimCount;
    ESMCI::Grid *grid;

    // Get Grid pointer
    grid=*_grid;

    //Initialize return code
    localrc = ESMC_RC_NOT_IMPL;
    if (_rc!=NULL) *_rc = ESMC_RC_NOT_IMPL;


    // Check grid status
   if (grid->getStatus() < ESMC_GRIDSTATUS_SHAPE_READY) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_WRONG,
          "- grid not ready for this operation ", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
    }

   // TODO: Make sure coords are within the correct bounds

   // TODO: make sure _localDE and _staggerloc are not NULL?

    // Input Error Checking
    if ((*_localDE < 0) || (*_localDE >=grid->getDistGrid()->getDELayout()->getLocalDeCount())) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
          "- localDE outside range on this processor", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
    }

   if ((*_staggerloc < 0) || (*_staggerloc >=  grid->getStaggerLocCount())) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
          "- staggerloc outside of range for grid", ESMC_NOT_PRESENT_FILTER(_rc));
        return;
    }


   // Get Coords 
   grid->getCoord(*_staggerloc, *_localDE, index, coord);


    // return successfully
    if (_rc!=NULL) *_rc = ESMF_SUCCESS;
  
  }

  ///////////////////////////////////////////////////////////////////////////////////

#if 0 // DEBUG
  void FTN(c_esmc_gridtest)(ESMCI::Grid **_grid, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridvalidate()"
    int localrc;

    //Initialize return code
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;

    // Check status
   if ((*_grid)->getStatus() < ESMC_GRIDSTATUS_SHAPE_READY) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_WRONG,
          "- grid status below ESMC_GRIDSTATUS_SHAPE_READY ", ESMC_NOT_PRESENT_FILTER(rc));
        return;
    }


   int dimCount=2;

   // Create ESMCI::InterfaceInts holding stagger padding
   int extent[1];
   extent[0]=dimCount;
   int *minIndexIntIntArray=new int[dimCount];
   minIndexIntIntArray[0]=1;
   minIndexIntIntArray[1]=1;
   ESMCI::InterfaceInt *minIndexIntInt=new ESMCI::InterfaceInt(minIndexIntIntArray,1,extent);
   
   extent[0]=dimCount;
   int *maxIndexIntIntArray=new int[dimCount];
   maxIndexIntIntArray[0]=20;
   maxIndexIntIntArray[1]=20;
   ESMCI::InterfaceInt *maxIndexIntInt=new ESMCI::InterfaceInt(maxIndexIntIntArray,1,extent);

   extent[0]=dimCount;
   int *regDecompIntIntArray=new int[dimCount];
   regDecompIntIntArray[0]=4;
   regDecompIntIntArray[1]=1;
   ESMCI::InterfaceInt *regDecompIntInt=new ESMCI::InterfaceInt(regDecompIntIntArray,1,extent);
   
   ESMC_IndexFlag indexflag=ESMF_INDEX_GLOBAL;

   ESMCI::DistGrid *distgrid=ESMCI::DistGrid::create(
  minIndexIntInt,                 // (in)
  maxIndexIntInt,                 // (in)
  regDecompIntInt,                // (in)
  (ESMCI::DecompFlag *)NULL,                 // (in)
  0,                    // (in)
  (ESMCI::InterfaceInt *)NULL,      // (in)
  (ESMCI::InterfaceInt *)NULL,       // (in)
  (ESMCI::InterfaceInt *)NULL,              // (in)
  &indexflag,              // (in)
  (ESMCI::InterfaceInt *)NULL,           // (in)
  (ESMCI::DELayout *)NULL,                     // (in)
  (ESMCI::VM *)NULL,                                 // (in)
  &localrc);
 if(ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                          ESMC_NOT_PRESENT_FILTER(rc))) return;

 int index[2];
 
 for (int i=1; i<20; i++) { 
   index[0]=4;
   index[1]=i;
   int id=distgrid->getSequenceIndexTile(
                                        1,                        // in  - tile = {1, ..., tileCount}
                                        index,                 // in  - tile-specific absolute index tuple
                                        3,                        // in  - depth of recursive search
                                        &localrc);                           // out - return code
   if(ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                          ESMC_NOT_PRESENT_FILTER(rc))) return;
   printf(" %d id=%d \n",i,id);
 }
    

// Create ESMCI::InterfaceInts holding stagger padding
 extent[0]=dimCount;
 int *edgeLWidthIntIntArray=new int[dimCount];
 edgeLWidthIntIntArray[0]=1;
 edgeLWidthIntIntArray[1]=1;
 ESMCI::InterfaceInt *edgeLWidthIntInt=new ESMCI::InterfaceInt(edgeLWidthIntIntArray,1,extent);

 extent[0]=dimCount;
 int *edgeUWidthIntIntArray=new int[dimCount];
 edgeUWidthIntIntArray[0]=0;
 edgeUWidthIntIntArray[1]=0;
 ESMCI::InterfaceInt *edgeUWidthIntInt=new ESMCI::InterfaceInt(edgeUWidthIntIntArray,1,extent);

 
 // Create stagger distgrid w no poles with this padding
 //      staggerDistgridList[staggerloc]=DistGrid::create(distgrid_wo_poles,
 ESMCI::DistGrid *distgrid2=ESMCI::DistGrid::create(distgrid,
                                                     edgeLWidthIntInt, 
                                                     edgeUWidthIntInt, 
                                                     &indexflag,
                                                     NULL,    
                                                     &localrc);
   if(ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                          ESMC_NOT_PRESENT_FILTER(rc))) return;


   printf("Number 2: \n");
   int old_id=0;
 for (int i=1; i<21; i++) { 
   index[0]=4;
   index[1]=i;
   int id=distgrid2->getSequenceIndexTile(
                                        1,                        // in  - tile = {1, ..., tileCount}
                                        index,                 // in  - tile-specific absolute index tuple
                                        3,                        // in  - depth of recursive search
                                        &localrc);                           // out - return code
   if(ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                          ESMC_NOT_PRESENT_FILTER(rc))) return;
   printf(" %d id=%d delta=%d \n",i,id,id-old_id);
   old_id=id;
 }



 
 // Create stagger distgrid w no poles with this padding
 //      staggerDistgridList[staggerloc]=DistGrid::create(distgrid_wo_poles,
 ESMCI::DistGrid *distgrid3=ESMCI::DistGrid::create(distgrid2,
                                                    NULL,
                                                    NULL,
                                                     &indexflag,
                                                     NULL,    
                                                     &localrc);
   if(ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                          ESMC_NOT_PRESENT_FILTER(rc))) return;


   printf("Number 3: \n");

 for (int i=1; i<21; i++) { 
   index[0]=4;
   index[1]=i;
   int id=distgrid3->getSequenceIndexTile(
                                        1,                        // in  - tile = {1, ..., tileCount}
                                        index,                 // in  - tile-specific absolute index tuple
                                        3,                        // in  - depth of recursive search
                                        &localrc);                           // out - return code
   if(ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                          ESMC_NOT_PRESENT_FILTER(rc))) return;
   printf(" %d id=%d \n",i,id);
 }



#if 0
   // Test getItem
   ESMCI::Grid *grid;

   // Get grid
   grid=*_grid;
  
   int index[2];
   ESMC_I4 val;



   ESMCI::GridIter *iter=new ESMCI::GridIter(grid,0,false);
   int i=0;
   for(iter->toBeg(); !iter->isDone(); iter->adv()) {

       iter->getItem(ESMC_GRIDITEM_MASK,&val);

       printf("%d :: GID=%d mask=%d \n",i,iter->getGlobalID(),val);

       i++;
   }


#endif

#if 0
  
   ESMCI::Grid *grid;

   // Get grid
   grid=*_grid;

   // turn on sphere
   grid->setSphere();

#if 0
   // Grid Node Iterator
   ESMCI::GridIter *iter=new ESMCI::GridIter(grid,0,true);
   ESMCI::GridIter *iter2=new ESMCI::GridIter(grid,0,false);

   printf("----------- \n");
   int i=0;
   for(iter->toBeg(); !iter->isDone(); iter->adv()) {

#if 0
     ESMC_R8 coord[2];

       iter->getCoord(coord);

       printf("%d :: GID=%d LID=%d local=%d  shared=%d (%f,%f) \n",i,iter->getGlobalID(),iter->getLocalID(),iter->isLocal(),iter->isShared(),coord[0],coord[1]);
#endif

       printf("%d :: GID=%d poleID=%d \n",i,iter->getGlobalID(),iter->getPoleID());



#if 0 
     int lid;

     lid=iter->getLocalID();

    if (iter2->moveToLocalID(lid)->getLocalID() != lid) {
       printf("ERROR :: %d \n",lid);      
     } else {
        printf("%d :: %d == %d \n",i,lid,iter2->moveToLocalID(lid)->getLocalID());      
     }
       printf(">>>> %d :: %d == %d \n",i,lid,iter2->moveToLocalID(lid)->getLocalID());      
#endif


     i++;
   }
   printf("%d ----------- \n",i);
#endif

#if 1
   // Grid Cell Iterator
   ESMCI::GridIter *ni=new ESMCI::GridIter(grid,0,true);

   ESMCI::GridCellIter *iter=new ESMCI::GridCellIter(grid,0);
   ESMCI::GridCellIter *iter2=new ESMCI::GridCellIter(grid,0);

   printf("Grid Cells ----------- \n");
   int i=0;
   for(iter->toBeg(); !iter->isDone(); iter->adv()) {

     int num, cnr[4], ngid[4];

     iter->getCornersCellNodeLocalID(&num, cnr);
     for (int i=0; i<4; i++) {
       ngid[i]=ni->moveToLocalID(cnr[i])->getGlobalID();
     }
   

     printf("%d :: GID=%d >>  %d %d %d %d \n",i,iter->getGlobalID(),ngid[0],ngid[1],ngid[2],ngid[3]);

#if 0
     int lid;

     lid=iter->getLocalID();

    if (iter2->moveToLocalID(lid)->getLocalID() != lid) {
       printf("ERROR :: %d \n",lid);      
     } else {
        printf("%d :: %d == %d \n",i,lid,iter2->moveToLocalID(lid)->getLocalID());      
     }
       printf(">>>> %d :: %d == %d \n",i,lid,iter2->moveToLocalID(lid)->getLocalID());      
#endif


     i++;
   }
   printf("%d ----------- \n",i);
#endif
#endif

    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  } 
#endif


  ///////////////////////////////////////////////////////////////////////////////////

  void FTN(c_esmc_gridsetdestroydistgrid)(ESMCI::Grid **gridArg, 
                                          int *destroyDistgridArg
					  ){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridsetdestroydistgridflag()"

   ESMCI::Grid *grid;

   // Get grid
   grid=*gridArg;

   if (*destroyDistgridArg==1) {
     grid->setDestroyDistgrid(true);
   } else {
     grid->setDestroyDistgrid(false);
   }
}


  void FTN(c_esmc_gridsetdestroydelayout)(ESMCI::Grid **gridArg, 
                                          int *destroyDelayoutArg
					  ){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridsetdestroydelayoutflag()"

   ESMCI::Grid *grid;

   // Get grid
   grid=*gridArg;

   if (*destroyDelayoutArg==1) {
     grid->setDestroyDELayout(true);
   } else {
     grid->setDestroyDELayout(false);
   }
}



  ///////////////////////////////////////////////////////////////////////////////////


  
#undef  ESMC_METHOD
}
