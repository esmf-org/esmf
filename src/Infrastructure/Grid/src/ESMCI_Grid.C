// $Id: ESMCI_Grid.C,v 1.1 2007/06/26 20:09:26 oehmke Exp $
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
#define ESMC_FILENAME "ESMCI_Grid.C"
//==============================================================================
//
// ESMC Grid method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ Array methods declared
// in the companion file ESMCI_Grid.h
//
//-----------------------------------------------------------------------------

// insert any higher level, 3rd party or system includes here
#include <stdio.h>
#include <string.h>

#include "ESMC_Start.h"

// associated class definition file
#include "ESMCI_Grid.h"

// LogErr headers
#include "ESMC_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"             // for LogErr
//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
  static const char *const version = "$Id: ESMCI_Grid.C,v 1.1 2007/06/26 20:09:26 oehmke Exp $";
//-----------------------------------------------------------------------------

#define VERBOSITY             (1)       // 0: off, 10: max

//
//
// TODO: Set everything up with Try-Catch Error handling
//
//


//-----------------------------------------------------------------------------


// Set up ESMCI name space for these methods
namespace ESMCI{  


//-----------------------------------------------------------------------------
//
//  File Local Routines
//
//-----------------------------------------------------------------------------
// Compute the number of stagger locations from a Grid's rank
static int _NumStaggerLocsFromRank(int rank)
{
  return 0x1<<rank;
}


//-----------------------------------------------------------------------------
//
// external Create and Destroy functions
//
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridCreate()"
//BOP
// !IROUTINE:  GridCreate
//
// !INTERFACE:
Grid *GridCreate(
//
// !RETURN VALUE:
//    Grid * to newly allocated ESMC_Grid
//
// !ARGUMENTS:
//
  int _nameLen,                                // (in)
  char *_name,                                 // (in)
  ESMC_TypeKind *_typekind,                    // (in)
  DistGrid *_distgrid,                    // (in)
  InterfaceInt *_dimmap,                  // (in)
  InterfaceInt *_lbounds,                 // (in)
  InterfaceInt *_ubounds,                 // (in)
  InterfaceInt *_coordRanks,              // (in)
  InterfaceInt *_coordDimMap,             // (in)
  ESMC_IndexFlag *_indexflag,                  // (in)
  int *_gridType,                              // (in)
  int *_rc                                     // (out) return code
  ){
//
// !DESCRIPTION:
//    Create an {\tt ESMC\_Grid} object from a DistGrid.
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int status;                 // local error status
  int rank;
  int distRank;
  int undistRank;
  int *dimmap;
  ESMC_TypeKind typekind;
  int *ubounds;
  int *lbounds;
  int *coordRanks;
  int **coordDimMap;
  ESMC_IndexFlag indexflag;
  int gridType;


  // TODO: Change to get rid of return codes in favor of try-catch

  // initialize return code; assume routine not implemented
  status = ESMC_RC_NOT_IMPL;
  if (_rc!=NULL)
    *_rc = ESMC_RC_NOT_IMPL;
  
  // check the input and get the information together to call GridConstruct
  // Must have a valid distgrid
  if (_distgrid == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to distgrid argument", _rc);
    return ESMC_NULL_POINTER;
  }

  // Process typekind
  if (_typekind==NULL) {
    typekind=ESMC_TYPEKIND_R8;  // Default
  } else {
    typekind=*_typekind;
  }

  // Get Rank of Distributed Dimensions
  distRank = _distgrid->getDimCount();

  // Process _lbounds and _ubounds
  // process these first to be able to calculate rank before dimmap processing

  // Process ubounds 
  undistRank=0; // default to 0
  ubounds = NULL; // default to NULL
  if (_ubounds != NULL){
    if (_ubounds->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- ubounds array must be of rank 1", _rc);
      return ESMC_NULL_POINTER;
    }
    if (_ubounds->extent[0] < 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- ubounds array must at least be of size 1", _rc);
      return ESMC_NULL_POINTER;
    }
    undistRank=_ubounds->extent[0]; 
    ubounds = _ubounds->array;
  }

  // Process lbounds 
  lbounds = NULL; // reset
  if (_lbounds != NULL){
    if (_ubounds==NULL){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- can't have ubounds without lbounds", _rc);
      return ESMC_NULL_POINTER;
    }
    if (_lbounds->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- lbounds array must be of rank 1", _rc);
      return ESMC_NULL_POINTER;
    }
    if (_lbounds->extent[0] != undistRank){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- lbounds, ubounds size mismatch", _rc);
      return ESMC_NULL_POINTER;
    }
    // set lbounds from argument
    lbounds=new int[undistRank];
    for (int i=0; i<undistRank; i++)
	 lbounds[i]=_lbounds->array[i];
  } else if (_ubounds != NULL) {
    // default lbounds to (1,1,1,...)
    lbounds=new int[undistRank];
    for (int i=0; i<undistRank; i++)
	 lbounds[i]=1;  // default to a bottom of 1
  }


  // Compute Rank
  rank=distRank+undistRank;

  // error check rank
  if (rank<1) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- Grid must have rank >=1 ", _rc);
      return ESMC_NULL_POINTER;
  }

  // Process dimmap
  dimmap = new int[distRank];
  if (_dimmap == NULL) {
    for (int i=0; i<distRank; i++)
      dimmap[i] = i+1; // set dimmap to default (1,2,3..)
  } else {
    if (_dimmap->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- dimmap array must be of rank 1", _rc);
      return ESMC_NULL_POINTER;
    }
    if (_dimmap->extent[0] != distRank){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- dimmap and distgrid mismatch", _rc);
      return ESMC_NULL_POINTER;
    }
    for (int i=0; i<distRank; i++){
      if (_dimmap->array[i] < 1 || _dimmap->array[i] > rank){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- dimmap / rank mismatch", _rc);
        return ESMC_NULL_POINTER;
      }
      dimmap[i] = _dimmap->array[i];  // copy dimmap array element
    }
  } 

  // Process _coordRanks
  coordRanks=new int[rank];
  if (_coordRanks == NULL) {
    for (int i=0; i<rank; i++)
      coordRanks[i] = rank; // set coordRanks to default all curvilinear
  } else {
    if (_coordRanks->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- coordRanks array must be of rank 1", _rc);
      return ESMC_NULL_POINTER;
    }
    if (_coordRanks->extent[0] != rank){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- coordRanks and distgrid (and perhaps ubounds) mismatch", _rc);
      return ESMC_NULL_POINTER;
    }
    for (int i=0; i<rank; i++){
      if (_coordRanks->array[i] < 1 || _coordRanks->array[i] > rank){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- coordRanks / rank mismatch", _rc);
        return ESMC_NULL_POINTER;
      }
      coordRanks[i] = _coordRanks->array[i];  // copy coordRanks array element
    }
  } 
  
  // Process _coordDimMap
  //TODO: DO THIS BETTER IN FUTURE, SO THE MEMORY IS IN ONE CHUNK (use TEMPLATES?)
  coordDimMap = new int *[rank];
  for(int i=0; i<rank; i++) {
    coordDimMap[i]=new int[rank];
    for (int j=0; j<rank; j++) {
      coordDimMap[i][j]=0;  // initialize to 0
    }
  }

  if (_coordDimMap == NULL) {
    for(int i=0; i<rank; i++) {
      for (int j=0; j<coordRanks[i]; j++) {
	coordDimMap[i][j]=j+1;  // initialize to a default (TODO: WORK ON A BETTER ONE)
      }
    }
  } else {
    if (_coordDimMap->dimCount != 2){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- coordDimMap array must be of rank 2", _rc);
      return ESMC_NULL_POINTER;
    }
    if ((_coordDimMap->extent[0] != rank) || 
        (_coordDimMap->extent[1] != rank)) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- coordDimMap and distgrid (and perhaps ubounds) mismatch", _rc);
      return ESMC_NULL_POINTER;
    }
    int k=0;
    for (int i=0; i<rank; i++){
      for (int j=0; j<coordRanks[i]; j++) {
	if (_coordDimMap->array[k] < 1 || _coordDimMap->array[k] > rank){
	  ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
			      "- coordDimMap / rank mismatch", _rc);
	  return ESMC_NULL_POINTER;
	}
        // TODO: check order of [i][j] may need to reverse because of F vs. C array ordering
	coordDimMap[i][j] = _coordDimMap->array[k];  // copy coordDimMap array element

        k++; // increment to next position in input array
      }
    }
  }  
 
  // Process _indexflag
  if (_indexflag==NULL) {
    indexflag=ESMF_INDEX_DELOCAL;  // default
  } else {
    indexflag=*_indexflag;
  }


  // Process _gridType
  if (_gridType==NULL) {
    gridType=0; // default
  } else {
    gridType=*_gridType;
  }

  // allocate the new Grid object
  Grid *grid=ESMC_NULL_POINTER;
  try{
    grid = new Grid(_nameLen, _name, typekind, _distgrid, 
             distRank, dimmap, 
             undistRank, lbounds, ubounds,
             rank, coordRanks, coordDimMap, 
	     indexflag, gridType);
  }catch(...){
     // allocation error
     ESMC_LogDefault.ESMC_LogMsgAllocError("for new ESMC_Grid.", _rc);  
     return ESMC_NULL_POINTER;
  }


  // Dellocate temporay arrays
  if (_ubounds != NULL)  delete [] lbounds;
 
  delete [] dimmap;

  delete [] coordRanks;

  for(int i=0; i<rank; i++)
    delete [] coordDimMap[i];
  delete [] coordDimMap;

  // return successfully
  *_rc = ESMF_SUCCESS;
  return grid;
  }
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridDestroy()"
//BOP
// !IROUTINE:  ~Grid
//
// !INTERFACE:
int GridDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
 Grid **_grid){  // in - Grid to destroy
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                // automatic variable for local return code

  // TODO: Change to get rid of return codes in favor of try-catch

  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // return with errors for NULL pointer
  if (_grid == ESMC_NULL_POINTER || *_grid == ESMC_NULL_POINTER){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to Grid",&localrc);
    return localrc;
  }

  // destruct and delete Grid object
  try{
     delete *_grid;
  }catch(...){
     // TODO: Change to get rid of return codes
     // deallocation error
     ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_WRONG,
      "- Error occurred in ~Grid ",&localrc);
    return localrc;
  }

  *_grid = ESMC_NULL_POINTER;
  
  // return successfully
  return ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
//
//  Grid Constructors and Destructors:
//
//-----------------------------------------------------------------------------


//----------------------------------------------------------------------------
//
//  Set the Grid default values in one place, so they're consistant across constructors
//
void _GridSetDefaults(Grid *_grid)
{
  // default status not ready
  _grid->status=ESMC_GRIDSTATUS_NOT_READY;

  _grid->typekind = ESMC_TYPEKIND_R8;
  _grid->distRank = 0;
  _grid->dimmap = ESMC_NULL_POINTER; 
  
  _grid->undistRank = 0;
  _grid->lbounds = ESMC_NULL_POINTER; 
  _grid->ubounds = ESMC_NULL_POINTER; 
  
  _grid->rank=0;
  _grid->coordRanks = ESMC_NULL_POINTER; 
  _grid->coordDimMap = ESMC_NULL_POINTER; 
  
  _grid->staggerLocCount=0;
  _grid->coordArrayList = ESMC_NULL_POINTER; 
  
  
  _grid->gridType=0;
  _grid->indexflag=ESMF_INDEX_DELOCAL;
  _grid->distgrid= ESMC_NULL_POINTER; 
  
    // TODO: Set default name (if you can easily change it)
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid()"
//BOPI
// !IROUTINE:  GridConstruct
//
// !INTERFACE:
Grid::Grid(
//
// !RETURN VALUE:
//    Pointer to a new grid
//
// !ARGUMENTS:
//
  int _nameLen, 
  char *_name, 
  ESMC_TypeKind _typekind,              // (in)
  DistGrid *_distgrid,                  // (in)
  int _distRank,                        // (in)
  int *_dimmap,                         // (in)
  int _undistRank,                      // (in)
  int *_lbounds,                        // (in)
  int *_ubounds,                        // (in)
  int _rank,                            // (in)
  int *_coordRanks,                     // (in)
  int **_coordDimMap,                   // (in)
  ESMC_IndexFlag _indexflag,             // (in)
  int _gridType                          // (in)
  ){
//
// !DESCRIPTION:
//    Construct the internal information structure of an ESMC\_Grid object.
//    No error checking wrt consistency of input arguments is needed because
//    GridConstruct() is only to be called by GridCreate() interfaces which
//    are responsible for providing consistent arguments to this layer.
//
//EOPI
//-----------------------------------------------------------------------------

  // TODO: Change to get rid of return codes in favor of try-catch

  // Set Grid Defaults
  _GridSetDefaults(this);

  // TODO: error checking here? OR trust the GridCreate methods?


  // Put input parameters into grid object
  typekind = _typekind;

  distgrid = _distgrid;

  distRank=_distRank;

  undistRank=_undistRank;

  rank = _rank;

  staggerLocCount=_NumStaggerLocsFromRank(rank); // Set the number of stagger locations from the grid rank

  indexflag=_indexflag;

  gridType=_gridType;


  // if there are distributed dimensions - allocate and copy
  if (distRank) {
     dimmap = new int[distRank];
     memcpy(dimmap, _dimmap, distRank * sizeof(int));
  }

  // if there are distributed dimensions - allocate and copy
  if (undistRank) {
    lbounds = new int[undistRank];
    memcpy(lbounds, _lbounds, undistRank * sizeof(int));
    
    ubounds = new int[undistRank];
    memcpy(ubounds, _ubounds, undistRank * sizeof(int));
  }

  // if there are any dimensions - allocate and copy
  if (rank) {
    coordRanks = new int[rank];
    memcpy(coordRanks, _coordRanks, rank * sizeof(int));
    
    //// DO THIS BETTER IN FUTURE, SO THE MEMORY IS IN ONE CHUNK (use TEMPLATES? or STL?)
    coordDimMap = new int *[rank];
    for(int i=0; i<rank; i++)
      coordDimMap[i]=new int[rank];
    for(int i=0; i<rank; i++)
      for(int j=0; j<rank; j++)
	coordDimMap[i][j]=_coordDimMap[i][j];
    
    // Allocate coordinate array storage
    coordArrayList = new Array **[staggerLocCount];
    for(int i=0; i<rank; i++)
      coordArrayList[i]=new Array *[rank];
  }


  // TODO: work on name later
  // invalidate the name for this Grid object in the Base class
  ESMC_BaseSetName(NULL, "Grid");

  // Grid is now ready to be used in grid methods, so set status appropriately
  status=ESMC_GRIDSTATUS_SHAPE_READY;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid()"
//BOPI
// !IROUTINE:  GridConstruct
//
// !INTERFACE:
Grid::Grid(
//
// !RETURN VALUE:
//    Pointer to a new grid
//
// !ARGUMENTS:
//  none
  ){
//
// !DESCRIPTION:
//    Construct the internal information structure of an ESMC\_Grid object.
//    No error checking wrt consistency of input arguments is needed because
//    GridConstruct() is only to be called by GridCreate() interfaces which
//    are responsible for providing consistent arguments to this layer.
//
//EOPI
//-----------------------------------------------------------------------------

  // TODO: Change to get rid of return codes in favor of try-catch

  // Set Grid Defaults
  _GridSetDefaults(this);

}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::~Grid()"
//BOPI
// !IROUTINE:  ~Grid
//
// !INTERFACE:
 Grid::~Grid(void){
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
// none
//
// !DESCRIPTION:
//  Destructor for Grid, deallocates all internal memory, etc. 
//
//EOPI
//-----------------------------------------------------------------------------

   // TODO: Change to get rid of return codes in favor of try-catch

   // TODO: When CreateEmpty functionality is added this may need to be changed to be more flexible

   // delete internal storage
   if (distRank) {
     delete [] dimmap;
   }

   if (undistRank) {
     delete [] lbounds;
     delete [] ubounds;
   }

   if (rank) {
     delete [] coordRanks;

     for(int i=0; i<rank; i++)
       delete [] coordDimMap[i];
     delete [] coordDimMap;

     for(int i=0; i<rank; i++)
       delete [] coordArrayList[i];
     delete [] coordArrayList;
   }

  // TODO: Do I need to do something to the base because of the name?    
}

} // END ESMCI name space
//-----------------------------------------------------------------------------
