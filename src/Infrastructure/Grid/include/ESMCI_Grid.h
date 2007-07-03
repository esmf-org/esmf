// $Id: ESMCI_Grid.h,v 1.3 2007/07/03 21:28:19 oehmke Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMCI Grid include file for C++

// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMCI_Grid_H
#define ESMCI_Grid_H

//-------------------------------------------------------------------------
//BOP
// !CLASS: ESMCI_Grid - Grid
//
// !DESCRIPTION:
//
// The code in this file defines the C++ {\tt Grid} members and method
// signatures (prototypes).  The companion file {\tt ESMC\_Grid.C}
// contains the full code (bodies) for the {\tt Grid} methods.
//
///EOP
//-------------------------------------------------------------------------


#include "ESMC_Base.h"
#include "ESMC_DistGrid.h"
#include "ESMC_Array.h"


// Eventually move this to ESMCI_Util.h
enum ESMC_GridStatus {ESMC_GRIDSTATUS_NOT_READY=1,
		      ESMC_GRIDSTATUS_SHAPE_READY
};

// Eventually move this to ESMCI_Util.h
//enum ESMC_CopyFlag {ESMC_DATA_REF=1,
//		    ESMC_DATA_COPY
//};

// Start name space
namespace ESMCI {  

class Grid;

// class definition
class Grid : public ESMC_Base {    // inherits from ESMC_Base class
  private:
  // Grid Status
  ESMC_GridStatus status;

  // global information
  ESMC_TypeKind typekind;
  
  int distRank;
  int *dimmap;    // size of dimmap = distRank
  
  int undistRank;
  int *lbounds; // size of lbounds = undistRank
  int *ubounds; // size of ubounds = undistRank
  
  int rank;
  int *coordRanks; // size of coordRanks = rank
  int **coordDimMap; // size of coordDimMap = rankxrank  [coord][dim of coord array]
  
  int staggerLocCount;
  Array ***coordArrayList; // size of coordArrayList = staggerLocCountxrank [staggerLoc][coord]
  int   ***coordAlignList;     // hold alignment info [staggerloc][coord][dim]
  bool  **didIAllocList;        // if true, I allocated this Array [staggerloc][coord]
  
  int gridType;
  ESMC_IndexFlag indexflag;
  DistGrid *distgrid;
  
 public:

  // accessor methods
  const int getRank(void) const {return rank;}
  const int getDistRank(void) const {return distRank;}
  const int getUndistRank(void) const {return undistRank;}
  const int getTileCount(void) const {return 1;}
  const int getStaggerLocCount(void) const {return staggerLocCount;}
  const int getGridType(void) const {return gridType;}
  const ESMC_IndexFlag getIndexFlag(void) const {return indexflag;}
  const ESMC_TypeKind getTypeKind(void) const {return typekind;}
  const DistGrid *getDistGrid(void) const {return distgrid;}
  const int *getDimMap(void) const {return dimmap;}
  const int *getLbounds(void) const {return lbounds;}
  const int *getUbounds(void) const {return ubounds;}
  const int *getCoordRanks(void) const {return coordRanks;}
  const int **getCoordDimMap(void) const {return coordDimMap;}


  // Set Grid default values
  friend void  _GridSetDefaults(Grid *grid);

  
  // external methods:  
  
  // Grid Construct (grid usable after construction)
  Grid(
       int nameLen, 
       char *name, 
       ESMC_TypeKind typekindArg,              // (in)
       DistGrid *distgridArg,             // (in)
       int distRankArg,                        // (in)
       int *dimmapArg,                         // (in)
       int undistRankArg,                      // (in)
       int *lboundsArg,                        // (in)
       int *uboundsArg,                        // (in)
       int rankArg,                            // (in)
       int *coordRanksArg,                     // (in)
       int **coordDimMapArg,                   // (in)
       ESMC_IndexFlag indexflagArg,             // (in)
       int gridTypeArg                          // (in)
       );
  
  // Grid Construct (grid NOT usable after construction)
  Grid();

  // Grid Destruct
  ~Grid();

  int Grid::setCoordArray(
			  int _staggerloc, // (in)
			  int _coord,      // (in)
			  Array *_array,   // (in)
			  int *coordAlign,  // (in)
			  bool _self_alloc // (in)
			  );


  int Grid::getCoordArray(
			  int _staggerloc, // (in)
			  int _coord,      // (in)
			  Array **_array   // (in)
			  );


  
};  // end class ESMC_Grid
 
 
 Grid *GridCreate(
		  int nameLen,                                // (in)
		  char *name,                                 // (in)
		  ESMC_TypeKind *typekind,                    // (in)
		  DistGrid *distgrid,                  // (in)
		  InterfaceInt *dimmap,                  // (in)
		  InterfaceInt *lbounds,                 // (in)
		  InterfaceInt *ubounds,                 // (in)
		  InterfaceInt *coordRanks,              // (in)
		  InterfaceInt *coordDimMap,             // (in)
		  ESMC_IndexFlag *indexflag,                  // (in)
		  int *gridType,                              // (in)
		  int *rc                                     // (out) return code
		  );
 

int gridSetCoordFromArray(
  Grid *_grid, 
  int *_staggerloc,
  int *_coord,
  Array *_array,
  ESMC_DataCopy *_docopy,
  InterfaceInt *_coordAlign
  );


 int gridGetCoordIntoArray(			  
			   Grid *_grid, 
			   int *_staggerloc,
			   int *_coord,
			   Array **_array,
			   ESMC_DataCopy *_docopy
			   );


 int gridGet(
	     Grid *_grid, //(in)
	     ESMC_TypeKind *_typekind,
	     int *_rank,
	     int *_tileCount,
	     DistGrid *_distgrid,
	     int *_staggerLocsCount,
	     InterfaceInt *_dimmap,   
	     InterfaceInt *_lbounds,  
	     InterfaceInt *_ubounds,  
	     InterfaceInt *_coordRanks,
	     InterfaceInt *_coordDimMap,
	     ESMC_IndexFlag *_indexflag, 
	     int *_gridType              
	     );


 
 int GridDestroy(Grid **grid);
 
} // END ESMCI namespace

#endif  // ESMC_GridI_H

