// $Id: ESMCI_Grid.h,v 1.1 2007/06/26 20:08:38 oehmke Exp $
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
		      ESMC_GRIDSTATUS_SHAPE_READY,
};

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
  
  int gridType;
  ESMC_IndexFlag indexflag;
  DistGrid *distgrid;
  
 public:

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
 
 
 int GridDestroy(Grid **grid);
 
} // END ESMCI namespace

#endif  // ESMC_GridI_H

