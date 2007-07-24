// $Id: ESMCI_Grid.C,v 1.14 2007/07/24 19:33:05 oehmke Exp $
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
  static const char *const version = "$Id: ESMCI_Grid.C,v 1.14 2007/07/24 19:33:05 oehmke Exp $";
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


// At some point consider replacing the following templated subroutines
// with a whole templated multidimensional class.

// Allocate a 2D array in one chunk of memory
template <class Type>
static  Type **_allocate2D(int sizeDim1, int sizeDim2)
  {
    Type **array,*p;

    // allocate enough space for pointers to rows and rows
    array=(Type **)malloc(sizeDim1*sizeof(Type *)+(sizeDim1*sizeDim2*sizeof(Type)));

    // fill in row pointers
    p=((Type *)(array+sizeDim1));
    for (int i=0; i<sizeDim1; i++) {
      array[i]=p;
      p=p+sizeDim2;  // advance to beginning of next row
    }

    return array;
  }

// Deallocate a 2D array of ints in one chunk of memory
template <class Type>
static  void _free2D(Type ***array)
  {
    if (*array==ESMC_NULL_POINTER) return;
    free(*array);  
    *array=ESMC_NULL_POINTER;
  }


// Allocate a 3D array in one chunk of memory
template <class Type>
static  Type ***_allocate3D(int sizeDim1, int sizeDim2, int sizeDim3)
  {
    Type ***array,**p1, *p2;

    // allocate enough space for pointers to pointers and pointers to data and data
    array=(Type ***)malloc(sizeDim1*sizeof(Type **)+sizeDim1*sizeDim2*sizeof(Type *)+
                           sizeDim1*sizeDim2*sizeDim3*sizeof(Type));

    // fill in pointers to arrays of pointers to actual data
    p1=((Type **)(array+sizeDim1));
    for (int i=0; i<sizeDim1; i++) {
      array[i]=p1;
      p1=p1+sizeDim2;  // advance to next row in pointers to pointers
    }

    // starting with pointer to beginning of data storage fill in row pointers
    p2=(Type *)p1;
    for (int i=0; i<sizeDim1; i++) {
      for (int j=0; j<sizeDim2; j++) {
	array[i][j]=p2;
	p2=p2+sizeDim3;  // advance to beginning of next row
      }
    }

    return array;
  }

// Deallocate a 3D array of ints in one chunk of memory
template <class Type>
static  void _free3D(Type ****array)
  {
    if (*array==ESMC_NULL_POINTER) return;
    free(*array);  
    *array=ESMC_NULL_POINTER;
  }


  static InterfaceInt *_copyInterfaceInit(InterfaceInt *in) {

    // calc size of array
    int size=1;
    for (int i=0; i<in->dimCount; i++) {
      size=size*(in->extent[i]);
    }

    // allocate new storage
    int *array;
    if (size>0) {
      array=new int[size];
      memcpy(array,in->array,size*sizeof(int));
    } else {
      array=ESMC_NULL_POINTER;
    }

    return new InterfaceInt(array,in->dimCount,in->extent);
  }

  static void _freeInterfaceInit(InterfaceInt **in) {

    // make sure its not a null pointer
    if (in==ESMC_NULL_POINTER) return;
    if (*in==ESMC_NULL_POINTER) return;

    // delete internal memory
    if ((*in)->array !=ESMC_NULL_POINTER) {
      delete [] ((*in)->array);  
    }

    // delete object
    delete *in;

    // set pointer
    *in=ESMC_NULL_POINTER;
  }
#if 1
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::_createIsBnd()"
  static int _createIsDEBnd(char **_isDELBnd, char **_isDEUBnd, DistGrid *distgrid,int *dimmap) {
    char *isDELBnd,*isDEUBnd;
    int rc,localrc;

    // get dimCount;
    int dimCount=distgrid->getDimCount();

    // Get the DELayout
    DELayout *delayout=distgrid->getDELayout();

    // Get the number of local DEs
    const int localDECount=delayout->getLocalDeCount();

    // Get map between local and global DEs
    const int *localDEList=delayout->getLocalDeList();
   
    // Get map between DEs and patches
    const int *DEPatchList = distgrid->getPatchListPDe();

    // Get list of patch min and maxs
    const int *patchMinIndexList = distgrid->getMinIndexPDimPPatch();
    const int *patchMaxIndexList = distgrid->getMaxIndexPDimPPatch();

    // Get Extents of index lists
    const int *deIndexListExtentList=distgrid->getIndexCountPDimPDe();

    // allocate Bnds
    isDELBnd=new char[localDECount];
    isDEUBnd=new char[localDECount];

    // loop through local DE's setting flags
    for (int lDE=0; lDE<localDECount; lDE++) {

      //// get global de
      int gDE=localDEList[lDE];

      //// get patch
      int patch=DEPatchList[gDE];

      //// get the extents for this de
      const int *deExtent=deIndexListExtentList+gDE*dimCount;

      //// get patch min/max
      const int *patchMin=distgrid->getMinIndexPDimPPatch(patch, &localrc);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
			      ESMF_ERR_PASSTHRU, &rc)) return rc;
      const int *patchMax=distgrid->getMaxIndexPDimPPatch(patch, &localrc);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
			      ESMF_ERR_PASSTHRU, &rc)) return rc;

      //// Init flags
      isDELBnd[lDE]=0xff;
      isDEUBnd[lDE]=0xff;

      //// loop setting flags
      for (int d=0; d<dimCount; d++) {

	////// make sure is contiguous         
	const int contig=distgrid->getContigFlagPDimPDe(gDE, d+1, &localrc);
	if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
			      ESMF_ERR_PASSTHRU, &rc)) return rc;
	if (!contig) {
	  ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_IMPL,
		     "- doesn't handle non-contiguous DEs yet ", &rc);
	  return rc;
	}

        // get indices of DE
	const int *indexList=distgrid->getIndexListPDimPLocalDe(lDE, d+1,
          &localrc);
	if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
			      ESMF_ERR_PASSTHRU, &rc)) return rc;

        // if we're not at the min then we're not a lower bound so turn off the bit
        if (indexList[0] != patchMin[d]) {
	  isDELBnd[lDE] &= ~(0x1<<dimmap[d]);
	} 

	// if we're at the min then we're a lower bound
        if (indexList[deExtent[d]-1]!=patchMax[d]) {
	  isDEUBnd[lDE] &= ~(0x1<<dimmap[d]);
	}
      }

      // DEBUG
      //      printf(" %d : LBND %d %d %d ",lDE,(int)(isDELBnd[lDE]&0x1),(int)(isDELBnd[lDE]&0x2),(int)(isDELBnd[lDE]&0x4));
      // printf(" :: UBND %d %d %d \n",(int)(isDEUBnd[lDE]&0x1),(int)(isDEUBnd[lDE]&0x2),(int)(isDEUBnd[lDE]&0x4));
    }

    // set output variables
    *_isDELBnd=isDELBnd;
    *_isDEUBnd=isDEUBnd;

    // return success
    return ESMF_SUCCESS;
  }
#endif

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridActivate()"
//BOP
// !IROUTINE:  GridActivate
//
// !INTERFACE:
int GridActivate(
//
// !RETURN VALUE:
//   error code
//
// !ARGUMENTS:
//
  Grid *_grid, 
  int _nameLen,                                // (in)
  char *_name,                                 // (in)
  ESMC_TypeKind *_typekind,                    // (in)
  DistGrid *_distgrid,                    // (in)
  InterfaceInt *_dimmap,                  // (in)
  InterfaceInt *_lbounds,                 // (in)
  InterfaceInt *_ubounds,                 // (in)
  InterfaceInt *_coordRank,              // (in)
  InterfaceInt *_coordDimMap,             // (in)
  ESMC_IndexFlag *_indexflag,                  // (in)
  int *_gridType
  ){
//
// !DESCRIPTION:
//    Create an {\tt ESMC\_Grid} object from a DistGrid.
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                 // local error status
  int rc;                 // error status
  int rank;
  int distRank;
  int undistRank;
  int *dimmap;
  ESMC_TypeKind typekind;
  int *ubounds;
  int *lbounds;
  int *coordRank;
  int **coordDimMap;
  ESMC_IndexFlag indexflag;
  int gridType;
  int ind;
  char *name;  

  // TODO: Change to get rid of return codes in favor of try-catch

  // initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  // make sure grid is inactive
  if (_grid->getStatus() != ESMC_GRIDSTATUS_NOT_READY) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
      "- grid must be status 'not ready' to be activated ", &rc);
    return rc;
  }  
  
  // check the input and get the information together to call GridConstruct
  // Must have a valid distgrid
  if (_distgrid == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to distgrid argument", &rc);
    return rc;
  }

  // process name
  name = ESMC_F90toCstring(_name, _nameLen);
  if (!name && _nameLen){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
					  "- Not a valid string", &rc);
    return rc;
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
        "- ubounds array must be of rank 1", &rc);
      return rc;
    }
    if (_ubounds->extent[0] < 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- ubounds array must at least be of size 1", &rc);
      return rc;
    }
    undistRank=_ubounds->extent[0]; 
    ubounds = _ubounds->array;
  }

  // Process lbounds 
  lbounds = NULL; // reset
  if (_lbounds != NULL){
    if (_ubounds==NULL){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- can't have ubounds without lbounds", &rc);
      return rc;
    }
    if (_lbounds->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- lbounds array must be of rank 1", &rc);
      return rc;
    }
    if (_lbounds->extent[0] != undistRank){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- lbounds, ubounds size mismatch", &rc);
      return rc;
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
        "- Grid must have rank >=1 ", &rc);
      return rc;
  }

  // Process dimmap
  dimmap = new int[distRank];
  if (_dimmap == NULL) {
    for (int i=0; i<distRank; i++)
      dimmap[i] = i; // set dimmap to default (0,1,2..)
  } else {
    if (_dimmap->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- dimmap array must be of rank 1", &rc);
      return rc;
    }
    if (_dimmap->extent[0] != distRank){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- dimmap and distgrid mismatch", &rc);
      return rc;
    }
    for (int i=0; i<distRank; i++){
      if (_dimmap->array[i] < 1 || _dimmap->array[i] > rank){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- dimmap / rank mismatch", &rc);
        return rc;
      }
      dimmap[i] = _dimmap->array[i]-1;  // copy dimmap array element and make it zero based
    }
  } 

  // Process _coordRank
  coordRank=new int[rank];
  if (_coordRank == NULL) {
    for (int i=0; i<rank; i++)
      coordRank[i] = rank; // set coordRank to default all curvilinear
  } else {
    if (_coordRank->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- coordRank array must be of rank 1", &rc);
      return rc;
    }
    if (_coordRank->extent[0] != rank){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- coordRank and distgrid (and perhaps ubounds) mismatch", &rc);
      return rc;
    }
    for (int i=0; i<rank; i++){
      if (_coordRank->array[i] < 1 || _coordRank->array[i] > rank){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- coordRank / rank mismatch", &rc);
        return rc;
      }
      // TODO: take this out when Array Factorization works
      if (_coordRank->array[i] != rank){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_IMPL,
          "- Array and thus Grid don't currently support factorization", &rc);
        return rc;
      }
      coordRank[i] = _coordRank->array[i];  // copy coordRank array element
    }
  } 
  
  // Process _coordDimMap
  // Allocate a 2D array of integers
  coordDimMap=_allocate2D<int>(rank,rank);
  // initialize array to 0
  for(int i=0; i<rank; i++) {
    for (int j=0; j<rank; j++) {
      coordDimMap[i][j]=0;  
    }
  }

  if (_coordDimMap == NULL) {
    for(int i=0; i<rank; i++) {
      for (int j=0; j<coordRank[i]; j++) {
	coordDimMap[i][j]=j;  // initialize to a default (TODO: WORK ON A BETTER ONE)
      }
    }
  } else {
    if (_coordRank == NULL){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- if coordDimMap is specified then a corresponding coordRank must also be specified", &rc);
      return rc;
    }
    if (_coordDimMap->dimCount != 2){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- coordDimMap array must be of rank 2", &rc);
      return rc;
    }
    if ((_coordDimMap->extent[0] != rank) || 
        (_coordDimMap->extent[1] != rank)) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- coordDimMap and distgrid (and perhaps ubounds) mismatch", &rc);
      return rc;
    }
    for (int i=0; i<rank; i++){
      for (int j=0; j<coordRank[i]; j++) {
        // Note: order of i,j is because of F vs. C array ordering
        ind=j*rank+i;

        // Check to make sure data is correct
	if (_coordDimMap->array[ind] < 1 || _coordDimMap->array[ind] > rank){
	  ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
			      " - invalid coordDimMap value", &rc);
	  return rc;
	}

	// copy coordDimMap array element
	coordDimMap[i][j] = _coordDimMap->array[ind]-1; // switch to 0-based  
      }
    }

    /* DEBUG
    // test coordDimMap Order
    for (int i=0; i<rank; i++){
      for (int j=0; j<rank; j++) {
        printf("[%d][%d]=%d\n",i,j,coordDimMap[i][j]);
      }
    }
    */

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

  // activate the Grid object
  localrc=_grid->activate(name, typekind, _distgrid, 
             distRank, dimmap, 
             undistRank, lbounds, ubounds,
             rank, coordRank, coordDimMap, 
	     indexflag, gridType);
   if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
	    ESMF_ERR_PASSTHRU, &rc)) return rc;
	
  // Dellocate temporay arrays
  if (_ubounds != NULL)  delete [] lbounds;

  if (name) delete [] name;

  delete [] dimmap;

  delete [] coordRank;

  _free2D<int>(&coordDimMap);

  return ESMF_SUCCESS;
  }
//-----------------------------------------------------------------------------





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
  InterfaceInt *_coordRank,              // (in)
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
  int localrc;                 // local error status

  // TODO: Change to get rid of return codes in favor of try-catch

  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  if (_rc!=NULL)
    *_rc = ESMC_RC_NOT_IMPL;

  // create the grid
  Grid *grid=ESMC_NULL_POINTER;
  try{
    grid = new Grid();
  }catch(...){
     // allocation error
     ESMC_LogDefault.ESMC_LogMsgAllocError("for new ESMC_Grid.", _rc);  
     return ESMC_NULL_POINTER;
  }

  // activate the grid
  localrc=GridActivate(grid, _nameLen, _name,_typekind, _distgrid, _dimmap, 
                       _lbounds, _ubounds, _coordRank, _coordDimMap, _indexflag, 
                       _gridType);
   if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
	    ESMF_ERR_PASSTHRU, _rc)) return ESMC_NULL_POINTER;        

  // return successfully
  *_rc = ESMF_SUCCESS;
  return grid;
}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridCreateEmpty()"
//BOP
// !IROUTINE:  GridCreateEmpty
//
// !INTERFACE:
Grid *GridCreateEmpty(
//
// !RETURN VALUE:
//    Grid * to newly allocated ESMC_Grid
//
// !ARGUMENTS:
//
  int *_rc                                     // (out) return code
  ){
//
// !DESCRIPTION:
//    Create an {\tt ESMC\_Grid} object from a DistGrid.
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                 // local error status

  // TODO: Change to get rid of return codes in favor of try-catch

  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  if (_rc!=NULL)
    *_rc = ESMC_RC_NOT_IMPL;
  
  // allocate the new Grid object
  Grid *grid=ESMC_NULL_POINTER;
  try{
    grid = new Grid();
  }catch(...){
     // allocation error
     ESMC_LogDefault.ESMC_LogMsgAllocError("for new ESMC_Grid.", _rc);  
     return ESMC_NULL_POINTER;
  }

  // Add a protogrid to hold the set information
  localrc=grid->addProtoGrid();
   if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
	    ESMF_ERR_PASSTHRU, _rc)) return ESMC_NULL_POINTER;        

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
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::gridSetCoordFromArray()"
//BOP
// !IROUTINE:  gridSetCoordFromArray
//
// !INTERFACE:
int gridSetCoordFromArray(

//
// !RETURN VALUE:
//   Return code
//
// !ARGUMENTS:
//
  Grid *_grid, 
  int *_staggerloc,
  int *_coord,
  Array *_array,
  ESMC_DataCopy *_docopy
  ) {
//
// !DESCRIPTION:
//    Add coordinates to a grid from an array
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;
  int rc;
  int staggerloc;
  int coord;
  ESMC_DataCopy docopy;
  int rank;
  const int *arrayDimMap, *arrayLBounds, *arrayUBounds;
  const int *gridLBounds, *gridUBounds;
  int  *gridMapDim, **coordMapDim;
  bool *gridIsDist, **coordIsDist;     
  const int *coordRank;
  int **coordDimMap;
  bool ok;  

  // TODO: Change to get rid of return codes in favor of try-catch

  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  rc = ESMC_RC_NOT_IMPL;
  
  // check the input and get the information together to add array
  if (_grid == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to grid argument", &rc);
    return rc;
  }

  // make sure grid is inactive
  if (_grid->getStatus() < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
      "- grid not of correct status to perform this operation", &rc);
    return rc;
  }  


  // Process staggerloc
  if (_staggerloc==NULL) {
    staggerloc=0;  // default
  } else {
    staggerloc=*_staggerloc;
  }

  // Process coord
  if (_coord==NULL) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Must pass in coord argument", &rc);
    return rc;

  } else {
    coord=(*_coord)-1; // translate from 1 based to 0 based
  }

  if (_array == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to array argument", &rc);
    return rc;
  }

  // Process staggerloc
  if (_docopy==NULL) {
    docopy=ESMC_DATA_REF;  // default
  } else {
    docopy=*_docopy;
  }

  // Don't support copy right now
  if (docopy==ESMC_DATA_COPY) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_IMPL,
        "- Data Copy Flag not implemented yet", &rc);
  }

  // Get some coord information
  coordRank=_grid->getCoordRank();
  coordDimMap=_grid->getCoordDimMap();
 
  // check rank
  if (coordRank[coord] != _array->getRank()){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- Array and Grid coord rank mismatch ", &rc);
      return rc;
    }

  // check typekind
  if (_grid->getTypeKind() != _array->getTypekind()){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- Array and Grid TypeKind mismatch ", &rc);
      return rc;
    }

  // check distgrid
  if (_grid->getDistGrid() != _array->getDistGrid()){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- Array and Grid DistGrid mismatch ", &rc);
      return rc;
    }

  // check IndexFlag
  if (_grid->getIndexFlag() != _array->getIndexflag()){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- Array and Grid Indexflag mismatch ", &rc);
      return rc;
    }


  // Get some useful info
  coordIsDist=_grid->getCoordIsDist();
  coordMapDim=_grid->getCoordMapDim();
  gridIsDist=_grid->getGridIsDist();
  gridMapDim=_grid->getGridMapDim();

  // Check that the array's dimmap is consistant with this coord
  arrayDimMap=_array->getDimmap();
  ok=true;
  for (int i=0; i<coordRank[coord]; i++) {
    if (coordIsDist[coord][i]) {
      if (arrayDimMap[coordMapDim[coord][i]]-1 != i) { // arrayDimMap 1-based
	ok=false;
	break;
      }
    }
  } 
  if (!ok) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- Array and Grid Coord dimmap mismatch ", &rc);
      return rc;
  }

  
  // Check and make sure the array's computational bounds are
  // big enough for the stagger padding
  arrayLBounds=_array->getLBounds();
  arrayUBounds=_array->getUBounds();
  gridLBounds=_grid->getLbounds();
  gridUBounds=_grid->getUbounds();
  int offset[ESMF_MAXDIM];
  int staggerLBnd[ESMF_MAXDIM];
  int staggerUBnd[ESMF_MAXDIM];
  int compLBnd[ESMF_MAXDIM];
  int compUBnd[ESMF_MAXDIM];
  int distRank=_array->getDistGrid()->getDimCount();
  int localDECount=_grid->getDistGrid()->getDELayout()->getLocalDeCount();
  for (int lDE=0; lDE < localDECount; lDE++) {
    
    //// Calculate the stagger lower bounds from the array
    ////// Get array exclusive bounds
   const int *arrayExLBnd=_array->getExclusiveLBound()+lDE*distRank;

    ////// Get stagger Lbnd offset
    localrc=_grid->getStaggerLWidth(staggerloc, lDE, offset);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
					      &rc)) return rc; 
    ////// Fill in the staggerLBnd array
    for (int i=0; i<coordRank[coord]; i++) {
      int gi=coordDimMap[coord][i];
      if (coordIsDist[coord][i]) {
	staggerLBnd[i]=arrayExLBnd[coordMapDim[coord][i]]-offset[gi];
      } else {
	staggerLBnd[i]=gridLBounds[gridMapDim[gi]]-offset[gi];
      }
    }

    //// get computationalLBound from the array
    ///// get the computational bounds of the localDE
    const int *arrayCompLBnd=_array->getComputationalLBound()+lDE*distRank;

    ///// Fill in the compLBnd array
    for (int i=0; i<coordRank[coord]; i++) {
      if (coordIsDist[coord][i]) {
	compLBnd[i]=arrayCompLBnd[coordMapDim[coord][i]];
      } else {
	compLBnd[i]=arrayLBounds[coordMapDim[coord][i]];
      }
    }

    //// Make sure the grid staggerLbounds fit within the array computational L bounds
    for (int i=0; i<coordRank[coord]; i++) {
      if (compLBnd[i] > staggerLBnd[i]) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
		   "- Array computationalLBound or LBounds insufficient to hold grid+stagger bounds ", &rc);
      return rc;

      }
    }


    //// Calculate the stagger upper bounds from the array
    ////// Get array exclusive bounds
   const int *arrayExUBnd=_array->getExclusiveUBound()+lDE*distRank;

    ////// Get stagger Ubnd offset
    localrc=_grid->getStaggerUWidth(staggerloc, lDE, offset);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
					      &rc)) return rc; 
    ////// Fill in the staggerLBnd array
    for (int i=0; i<coordRank[coord]; i++) {
      int gi=coordDimMap[coord][i];
      if (coordIsDist[coord][i]) {
	staggerUBnd[i]=arrayExUBnd[coordMapDim[coord][i]]+offset[gi];
      } else {
	staggerUBnd[i]=gridUBounds[gridMapDim[gi]]+offset[gi];
      }
    }
    

    //// get computationalUBound from the array
    ///// get the computational bounds of the localDE
    const int *arrayCompUBnd=_array->getComputationalUBound()+lDE*distRank;

    ///// Fill in the compLBnd array
    for (int i=0; i<coordRank[coord]; i++) {
      if (coordIsDist[coord][i]) {
	compUBnd[i]=arrayCompUBnd[coordMapDim[coord][i]];
      } else {
	compUBnd[i]=arrayUBounds[coordMapDim[coord][i]];
      }
    }

    //// Make sure the grid staggerLbounds fit within the array computational L bounds
    for (int i=0; i<coordRank[coord]; i++) {
      if (compUBnd[i] < staggerUBnd[i]) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
		   "- Array computationalUBound or UBounds insufficient to hold grid+stagger bounds ", &rc);
      return rc;

      }
    }
  }
 
  // Put _array into grid 
  rc=_grid->setCoordArray(staggerloc, coord, _array, false);

  // return what setCoordArray returned
  return rc;
  }
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::gridAllocCoord()"
//BOP
// !IROUTINE:  gridAllocCoord
//
// !INTERFACE:
int gridAllocCoord(

//
// !RETURN VALUE:
//   Return code
//
// !ARGUMENTS:
//
  Grid *_grid, 
  int *_staggerloc,
  InterfaceInt *_coordLWidth,
  InterfaceInt *_coordUWidth,
  InterfaceInt *_coordAlign
  ) {
//
// !DESCRIPTION:
//    Add coordinates to a grid from an array
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int status;                 // local error status
  int rc, localrc;
  int staggerloc;
  int coord;
  ESMC_DataCopy docopy;
  int *coordAlign;
  int *coordLWidth;
  int *coordUWidth;
  int rank;
  const int *arrayDimMap, *arrayLBounds, *arrayUBounds;
  const int *gridLBounds, *gridUBounds;
  int  *gridMapDim, **coordMapDim;
  bool *gridIsDist, **coordIsDist;     
  const int *coordRank;
  int **coordDimMap;
  Array *array;
  ESMC_IndexFlag indexflag;
  int extent[1];

  // TODO: Change to get rid of return codes in favor of try-catch

  // initialize return code; assume routine not implemented
  status = ESMC_RC_NOT_IMPL;
  rc = ESMC_RC_NOT_IMPL;
  
  // check the input and get the information together to add array
  if (_grid == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to grid argument", &rc);
    return rc;
  }

  // make sure grid is inactive
  if (_grid->getStatus() < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
      "- grid not of correct status to perform this operation", &rc);
    return rc;
  }  


  // Process staggerloc
  if (_staggerloc==NULL) {
    staggerloc=0;  // default
  } else {
    staggerloc=*_staggerloc;
  }


  // get the grid rank
  rank=_grid->getRank();

  // Process coordAlign (first so we can use the value to set the widths if necessary)
  coordAlign = new int[rank];
  if (_coordAlign == NULL) {
    for (int i=0; i<rank; i++)
      coordAlign[i] = -1; // set coordAlign to default (-1,-1,-1...)
  } else {
    if (_coordAlign->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- coordAlign array must be of rank 1", &rc);
      return rc;
    }
    if (_coordAlign->extent[0] != rank){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- coordAlign size and Grid rank mismatch ", &rc);
      return rc;
    }
    for (int i=0; i<rank; i++){
      coordAlign[i] = _coordAlign->array[i];  // copy coordAlign array element
    }
  }

  // Clip the Alignment against the stagger (ignore centered dimensions)
  for (int i=0; i<rank; i++) {
    if (!(staggerloc & (0x1<<i))) {
      coordAlign[i]=0;
    }
  }

  // Process coordLWidth
  coordLWidth = new int[rank];
  if (_coordLWidth == NULL) {
    if (_coordUWidth == NULL) {
      for (int i=0; i<rank; i++) {
	if (coordAlign[i]>0) {
	  coordLWidth[i] = 1; 
	} else {
	  coordLWidth[i] = 0; 
	}
      }
    } else {
      for (int i=0; i<rank; i++) {
	coordLWidth[i] = 0; 
      }
    }
  } else {
    if (_coordLWidth->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- coordLWidth array must be of rank 1", &rc);
      return rc;
    }
    if (_coordLWidth->extent[0] != rank){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- coordLWidth size and Grid rank mismatch ", &rc);
      return rc;
    }
    for (int i=0; i<rank; i++){
      coordLWidth[i] = _coordLWidth->array[i];  // copy coordLWidth array element
    }
  }

  // Process coordUWidth
  coordUWidth = new int[rank];
  if (_coordUWidth == NULL) {
    if (_coordLWidth == NULL) {
      for (int i=0; i<rank; i++) {
	if (coordAlign[i]<0) {
	  coordUWidth[i] = 1; 
	} else {
	  coordUWidth[i] = 0; 
	}
      }
    } else {
      for (int i=0; i<rank; i++) {
	coordUWidth[i] = 0; 
      }
    }
  } else {
    if (_coordUWidth->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
		   "- coordUWidth array must be of rank 1", &rc);
      return rc;
    }
    if (_coordUWidth->extent[0] != rank){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- coordUWidth size and Grid rank mismatch ", &rc);
      return rc;
    }
    for (int i=0; i<rank; i++){
      coordUWidth[i] = _coordUWidth->array[i];  // copy coordUWidth array element
    }
  }

  // Get some useful info
  coordRank=_grid->getCoordRank();
  coordDimMap=_grid->getCoordDimMap();
  coordIsDist=_grid->getCoordIsDist();
  coordMapDim=_grid->getCoordMapDim();
  gridIsDist=_grid->getGridIsDist();
  gridMapDim=_grid->getGridMapDim();
  gridLBounds=_grid->getLbounds();
  gridUBounds=_grid->getUbounds();

  // get IndexFlag
  indexflag=_grid->getIndexFlag();

  // construct ArraySpec
  ESMC_ArraySpec *arrayspec= new ESMC_ArraySpec();     
    
  // Construct DimMap & bounds
  int *lboundsArray=new int[rank];
  extent[0]=rank;
  InterfaceInt *lbounds=new InterfaceInt(lboundsArray,1,extent);// setup default, this'll need to be changed
                                                                // before being used as input
  int *uboundsArray=new int[rank];
  extent[0]=rank;
  InterfaceInt *ubounds=new InterfaceInt(uboundsArray,1,extent); // setup default, this'll need to be changed
                                                                // before being used as input
  int *dimmapArray=new int[rank];
  extent[0]=rank;
  InterfaceInt *dimmap=new InterfaceInt(dimmapArray,1,extent); // setup default, this'll need to be changed
                                                                  // before being used as input

  int *compLWidthArray=new int[rank];
  extent[0]=rank;
  InterfaceInt *compLWidth=new InterfaceInt(compLWidthArray,1,extent); // setup default, this'll need 
                                                                       // to be changed
                                                                       // before being used as input

  int *compUWidthArray=new int[rank];
  extent[0]=rank;
  InterfaceInt *compUWidth=new InterfaceInt(compUWidthArray,1,extent); // setup default, this'll need 
                                                                       // to be changed
                                                                       // before being used as input
  ////////////
  ///// Loop Constructing all the coordinate arrays
  ///////////
  for (coord=0; coord<rank; coord++) {
    
    // Fill in ArraySpec
    arrayspec->ESMC_ArraySpecSetRank(coordRank[coord]);
    arrayspec->ESMC_ArraySpecSetTypeKind(_grid->getTypeKind());
    
    //// fill in arrays
    int coordDistRank=0;
    int coordUndistRank=0;
    for (int i=0; i<coordRank[coord]; i++) {
      int gi=coordDimMap[coord][i];
      if (coordIsDist[coord][i]) {
	dimmapArray[coordMapDim[coord][i]]=i+1; // convert to 1-based
	coordDistRank++;
      } else {
	lboundsArray[coordMapDim[coord][i]]=gridLBounds[gridMapDim[gi]];  
	uboundsArray[coordMapDim[coord][i]]=gridUBounds[gridMapDim[gi]];  
	coordUndistRank++;
      }
    }
    
    // make dimmap InterfaceInit
    if (coordDistRank) {
      dimmap->extent[0]=coordDistRank;
    } else {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
					    "- Coordinate must have a distributed portion", &rc);
      return rc;
    }
    
    // set bounds sizes
    if (coordUndistRank) {
      lbounds->extent[0]=coordUndistRank;
      ubounds->extent[0]=coordUndistRank;
    }
    
    // Init ComputationalLWidth
    for (int i=0; i<coordDistRank; i++) {
      compLWidthArray[i]=0; // init to 0.
    }
    compLWidth->extent[0]=coordDistRank;
    
    // Init ComputationalUWidth
    for (int i=0; i<coordDistRank; i++) {
      compUWidthArray[i]=0; // init to 0.
    }
    compUWidth->extent[0]=coordDistRank;
    
    //// fill in arrays
    for (int i=0; i<coordRank[coord]; i++) {
      int gi=coordDimMap[coord][i];
      if (coordIsDist[coord][i]) {
	compUWidthArray[coordMapDim[coord][i]] = coordUWidth[gi]; 
	compLWidthArray[coordMapDim[coord][i]] = coordLWidth[gi];
	
      } else {
	uboundsArray[coordMapDim[coord][i]] += coordUWidth[gi];
	lboundsArray[coordMapDim[coord][i]] -= coordLWidth[gi];
      }
    }

    // Create an Array to hold the coords
    if (coordUndistRank) { // only pass in lbounds, ubounds if there are undistributed dims
      array=Array::create(arrayspec, (DistGrid *)_grid->getDistGrid(),
			  dimmap, compLWidth, compUWidth, 
			  (InterfaceInt *)ESMC_NULL_POINTER,
			  (InterfaceInt *)ESMC_NULL_POINTER,
			  &indexflag, &staggerloc, ESMC_NULL_POINTER, 
			  lbounds, ubounds, &localrc);
    } else {
      array=Array::create(arrayspec, (DistGrid *)_grid->getDistGrid(),
			  dimmap, compLWidth, compUWidth, 
			  (InterfaceInt *)ESMC_NULL_POINTER,
			  (InterfaceInt *)ESMC_NULL_POINTER,
			  &indexflag, &staggerloc, ESMC_NULL_POINTER,
			  (InterfaceInt *)ESMC_NULL_POINTER,
			  (InterfaceInt *)ESMC_NULL_POINTER, 
			  &localrc);
    }


      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
						ESMF_ERR_PASSTHRU, &rc)) return rc;        

    // Set Coord Array
    localrc=_grid->setCoordArray(staggerloc, coord, array, true);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
					      ESMF_ERR_PASSTHRU, &rc)) return rc;        
    
  } // end of coord loop


  // Set Coord Info
  localrc=_grid->setCoordInfo(staggerloc, coordAlign, coordLWidth, coordUWidth);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
	    ESMF_ERR_PASSTHRU, &rc)) return rc;        


  // Dellocate temporay arrays
  delete arrayspec;     
  delete [] dimmapArray;
  delete dimmap;
  delete [] compLWidthArray;
  delete compLWidth;
  delete [] compUWidthArray;
  delete compUWidth;
  if (lboundsArray != ESMC_NULL_POINTER) delete [] lboundsArray;
  if (uboundsArray != ESMC_NULL_POINTER) delete [] uboundsArray;
  if (lbounds != ESMC_NULL_POINTER) delete lbounds;
  if (ubounds != ESMC_NULL_POINTER) delete ubounds;
  if (coordUWidth != ESMC_NULL_POINTER) delete [] coordUWidth;
  if (coordLWidth != ESMC_NULL_POINTER) delete [] coordLWidth;

  // return ESMF_SUCCESS
  return ESMF_SUCCESS;
  }
//-----------------------------------------------------------------------------




//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridCommit()"
//BOP
// !IROUTINE:  GridCommit
//
// !INTERFACE:
int GridCommit(
//
// !RETURN VALUE:
//   Error code
//
// !ARGUMENTS:
//
  Grid *_grid
 ){
//
// !DESCRIPTION:
//    Create an {\tt ESMC\_Grid} object from a DistGrid.
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int rc;
  int localrc;                 // local error status
  ProtoGrid *proto;

  // TODO: Change to get rid of return codes in favor of try-catch

  // initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  // check the input and get the information together to add array
  if (_grid == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to grid argument", &rc);
    return rc;
  }

  // Make sure that we haven't been created
  if (_grid->getStatus() != ESMC_GRIDSTATUS_NOT_READY) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_BAD,
      "- Can't use commit on an already created object", &rc);
    return rc;
  }

 // Get the protoGrid
  proto=_grid->getProtoGrid();
  if (proto == ESMC_NULL_POINTER) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_BAD,
      "- Null protoGrid ", &rc);
    return rc;
  }

  // activate the grid
  localrc=GridActivate(_grid, proto->nameLen, proto->name, proto->typekind, 
                       proto->distgrid, proto->dimmap, proto->lbounds,
                       proto->ubounds, proto->coordRank, proto->coordDimMap,
                       proto->indexflag,  proto->gridType);
   if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
	    ESMF_ERR_PASSTHRU, &rc)) return rc;        

   // Now that we don't need it anymore, remove the protogrid from the grid
   localrc=_grid->delProtoGrid();
   if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
		       ESMF_ERR_PASSTHRU, &rc)) return rc;        

  // return successfully
  return ESMF_SUCCESS;
  }
//-----------------------------------------------------------------------------




//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GridSetFromDistGrid()"
//BOP
// !IROUTINE:  GridSetFromDistGrid
//
// !INTERFACE:
int GridSetFromDistGrid(
//
// !RETURN VALUE:
//   Error code
//
// !ARGUMENTS:
//
  Grid *_grid,
  int _nameLen,                                // (in)
  char *_name,                                 // (in)
  ESMC_TypeKind *_typekind,                    // (in)
  DistGrid *_distgrid,                    // (in)
  InterfaceInt *_dimmap,                  // (in)
  InterfaceInt *_lbounds,                 // (in)
  InterfaceInt *_ubounds,                 // (in)
  InterfaceInt *_coordRank,              // (in)
  InterfaceInt *_coordDimMap,             // (in)
  ESMC_IndexFlag *_indexflag,                  // (in)
  int *_gridType                             // (in)
  ){
//
// !DESCRIPTION:
//    Set Values in a Grid in preperation for a later commit.
///   TODO: eventually seperate this into a bunch of seperate sets to allow easier access from C.
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int rc;
  int localrc;                 // local error status
  ProtoGrid *proto;

  // TODO: Change to get rid of return codes in favor of try-catch

  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // check the input and get the information together to add array
  if (_grid == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to grid argument", &rc);
    return rc;
  }

  // Make sure that we haven't been created
  if (_grid->getStatus() != ESMC_GRIDSTATUS_NOT_READY) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_WRONG,
      "- Can't use set on an already created object", &localrc);
    return localrc;
  }

  
  // Get the protoGrid
  proto=_grid->getProtoGrid();
  if (proto == ESMC_NULL_POINTER) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_BAD,
      "- Null protoGrid ", &localrc);
    return localrc;
  }

  // name 
  if (_name != ESMC_NULL_POINTER) {
    proto->nameLen=_nameLen;
    proto->name= new char[_nameLen];
    memcpy(proto->name, _name, _nameLen * sizeof(char));
  } 

  //  typekind
  if (_typekind != ESMC_NULL_POINTER) {
    if (proto->typekind == ESMC_NULL_POINTER) proto->typekind= new ESMC_TypeKind;
    *(proto->typekind)=*_typekind;
  }

  // distgrid
  if (_distgrid != ESMC_NULL_POINTER) {
    proto->distgrid=_distgrid;
  }

  // dimmap
  if (_dimmap != ESMC_NULL_POINTER) { 
    // if present get rid of the old data
    if (proto->dimmap !=ESMC_NULL_POINTER) _freeInterfaceInit(&proto->dimmap);

    // record the new data
    proto->dimmap=_copyInterfaceInit(_dimmap);
  }

  // lbounds
  if (_lbounds != ESMC_NULL_POINTER) { 
    // if present get rid of the old data
    if (proto->lbounds !=ESMC_NULL_POINTER) _freeInterfaceInit(&proto->lbounds);

    // record the new data
    proto->lbounds=_copyInterfaceInit(_lbounds);
  }

  // ubounds
  if (_ubounds != ESMC_NULL_POINTER) { 
    // if present get rid of the old data
    if (proto->ubounds !=ESMC_NULL_POINTER) _freeInterfaceInit(&proto->ubounds);

    // record the new data
    proto->ubounds=_copyInterfaceInit(_ubounds);
  }

  // coordRank
  if (_coordRank != ESMC_NULL_POINTER) { 
    // if present get rid of the old data
    if (proto->coordRank !=ESMC_NULL_POINTER) _freeInterfaceInit(&proto->coordRank);

    // record the new data
    proto->coordRank=_copyInterfaceInit(_coordRank);
  }

  // coordDimMap
  if (_coordDimMap != ESMC_NULL_POINTER) { 
    // if present get rid of the old data
    if (proto->coordDimMap !=ESMC_NULL_POINTER) _freeInterfaceInit(&proto->coordDimMap);

    // record the new data
    proto->coordDimMap=_copyInterfaceInit(_coordDimMap);
  }

  // indexflag
  if (_indexflag!=NULL) {
    if (proto->indexflag == ESMC_NULL_POINTER) proto->indexflag= new ESMC_IndexFlag;
    *(proto->indexflag)=*_indexflag;
  }

  // gridType
  if (_gridType!=NULL) {
    if (proto->gridType == ESMC_NULL_POINTER) proto->gridType= new int;
    *(proto->gridType)=*_gridType;
  }
 
  // return successfully
  return ESMF_SUCCESS;
  }
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::gridGetCoordIntoArray()"
//BOP
// !IROUTINE:  gridGetCoordFromArray
//
// !INTERFACE:
int gridGetCoordIntoArray(

//
// !RETURN VALUE:
//   Return code
//
// !ARGUMENTS:
//
  Grid *_grid, 
  int *_staggerloc,
  int *_coord,
  Array **_array,
  ESMC_DataCopy *_docopy
  ) {
//
// !DESCRIPTION:
//    Add coordinates to a grid from an array
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int status;                 // local error status
  int rc;
  int staggerloc;
  int coord;
  ESMC_DataCopy docopy;
  int rank;

  // TODO: Change to get rid of return codes in favor of try-catch

  // initialize return code; assume routine not implemented
  status = ESMC_RC_NOT_IMPL;
  rc = ESMC_RC_NOT_IMPL;

  // check the input and get the information together to add array
  if (_grid == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to grid argument", &rc);
    return rc;
  }

  // make sure grid is inactive
  if (_grid->getStatus() < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
      "- grid not of correct status to perform this operation", &rc);
    return rc;
  }  
  
  // Process staggerloc
  if (_staggerloc==NULL) {
    staggerloc=0;  // default
  } else {
    staggerloc=*_staggerloc;
  }

  // Process coord
  if (_coord==NULL) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Must pass in coord argument", &rc);
    return rc;

  } else {
    coord=(*_coord)-1; // translate from 1 based to 0 based
  }

  if (_array == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to pass array out through", &rc);
    return rc;
  }

  // TODO: make sure array has the same distgrid as the grid

  // Process staggerloc
  if (_docopy==NULL) {
    docopy=ESMC_DATA_REF;  // default
  } else {
    docopy=*_docopy;
  }

  if (docopy==ESMC_DATA_COPY) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_IMPL,
        "- Data Copy Flag not implemented yet", &rc);
  }

  // Get Coord Array
  // (note that _array is already a ** to Array)
  rc=_grid->getCoordArray(staggerloc, coord, _array);

   // return what getCoordArray returned
  return rc;
  }
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//
//  Grid Constructors and Destructors:
//
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::activate()"
//BOPI
// !IROUTINE:  activate
//
// !INTERFACE:
int Grid::activate(
//
// !RETURN VALUE:
//    error code
//
// !ARGUMENTS:
//
  char *_name,                          // (in)
  ESMC_TypeKind _typekind,              // (in)
  DistGrid *_distgrid,                  // (in)
  int _distRank,                        // (in)
  int *_dimmap,                         // (in)
  int _undistRank,                      // (in)
  int *_lbounds,                        // (in)
  int *_ubounds,                        // (in)
  int _rank,                            // (in)
  int *_coordRank,                     // (in)
  int **_coordDimMap,                   // (in)
  ESMC_IndexFlag _indexflag,             // (in)
  int _gridType                          // (in)
  ){
//
// !DESCRIPTION:
//    Construct the internal information structure of an ESMC\_Grid object.
//    No error checking wrt consistency of input arguments is needed because
//    activate() is only to be called by GridActivate() interfaces which
//    are responsible for providing consistent arguments to this layer.
//
//EOPI
//-----------------------------------------------------------------------------

  // TODO: Change to get rid of return codes in favor of try-catch

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
    coordRank = new int[rank];
    memcpy(coordRank, _coordRank, rank * sizeof(int));
    
  // Allocate a 2D array of integers
    coordDimMap=_allocate2D<int>(rank,rank);
    for(int i=0; i<rank; i++)
      for(int j=0; j<rank; j++)
	coordDimMap[i][j]=_coordDimMap[i][j];
    
    // Allocate coordinate array storage
    coordArrayList=_allocate2D<Array *>(staggerLocCount,rank);
    for(int i=0; i<staggerLocCount; i++)
      for(int j=0; j<rank; j++)
	coordArrayList[i][j]=ESMC_NULL_POINTER;

    // Allocate coordinate Lower Width storage
    coordLWidthList=_allocate2D<int>(staggerLocCount,rank);


    // Allocate coordinate Upper Width storage
    coordUWidthList=_allocate2D<int>(staggerLocCount,rank);

    // Allocate coordinate Align5Bment storage
    coordAlignList=_allocate2D<int>(staggerLocCount,rank);

    // Set Defaults 
    for(int i=0; i<staggerLocCount; i++) {
      for(int j=0; j<rank; j++) {
	if (i & (0x1<<j)) {   // Set defaults based on the stagger location
	  coordUWidthList[i][j]=1;
	  coordLWidthList[i][j]=0;
	  coordAlignList[i][j]=-1;
	} else {
	  coordUWidthList[i][j]=0;
	  coordLWidthList[i][j]=0;
	  coordAlignList[i][j]=0;
	}
      }
    }

    // Allocate coordinate array storage
    didIAllocList=_allocate2D<bool>(staggerLocCount,rank);

    // setup grid dimension map
    //// Allocate 
    gridIsDist=new bool[rank];
    gridMapDim=new int[rank];

    //// Init to undistributed
    for (int i=0; i<rank; i++) {
      gridIsDist[i]=false;
    } 

    //// fill in distributed dimensions
    for(int i=0; i<distRank; i++) {
      gridMapDim[dimmap[i]]=i;
      gridIsDist[dimmap[i]]=true;
    }

    //// fill in undistributed dimensions
    int j=0;
    for(int i=0; i<rank; i++) {
      if (!gridIsDist[i]) {
	gridMapDim[i]=j;
	j++;
      }
    }

    // setup coord dimension map
    //// Allocate 
    coordIsDist=_allocate2D<bool>(rank,rank);
    coordMapDim=_allocate2D<int>(rank,rank);

    //// Fill in per coord
    for(int c=0; c<rank; c++) {
      int k=0;
      for (int i=0; i<coordRank[c]; i++) {
	int gi=coordDimMap[c][i]; // get grid dim corresponding to coord dim
	if (gridIsDist[gi]) {
	  coordMapDim[c][i]=gridMapDim[gi];  // coord dim maps to the one its grid dim maps to
	  coordIsDist[c][i]=true;
	} else {
	  coordMapDim[c][i]=k; // else the undist dim occur in order
	  coordIsDist[c][i]=false;
	  k++;
	}
      }
    }

  }
 
  // allocate and fill isDELBnd isDEUBnd
  _createIsDEBnd(&isDELBnd,&isDEUBnd, distgrid, dimmap);

  // Set the name for this Grid object in the Base class
  ESMC_BaseSetName(_name, "Grid");

  // Grid is now ready to be used in grid methods, so set status appropriately
  status=ESMC_GRIDSTATUS_SHAPE_READY;

  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::addProtoGrid()"
//BOPI
// !IROUTINE:  addProtoGrid
//
// !INTERFACE:
int Grid::addProtoGrid(
//
// !RETURN VALUE:
//    error code
//
// !ARGUMENTS:
//
  ){
//
// !DESCRIPTION:
//   Adds a protogrid to a grid. The protogrid is to hold data for the set/commit paradigm
//
//EOPI
//-----------------------------------------------------------------------------
  int rc;

  if (status != ESMC_GRIDSTATUS_NOT_READY) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_WRONG,
      "- Can't add a protogrid to an already created Grid", &rc);
    return rc;
  }

  if (proto != ESMC_NULL_POINTER) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_WRONG,
      "- this Grid already has a protogrid", &rc);
    return rc;
  }

  proto=new ProtoGrid();

  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::delProtoGrid()"
//BOPI
// !IROUTINE:  delProtoGrid
//
// !INTERFACE:
int Grid::delProtoGrid(
//
// !RETURN VALUE:
//    error code
//
// !ARGUMENTS:
//
  ){
//
// !DESCRIPTION:
//   Removes a protogrid from a grid
//
//EOPI
//-----------------------------------------------------------------------------
  int rc;

  // this shouldn't cause problems, but its weird enough to 
  // ring alarm bells, so if necessary remove this check 
  if (status == ESMC_GRIDSTATUS_NOT_READY) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_WRONG,
	    "- removing a protogrid from an uncreated Grid", &rc); 
    return rc;
  }

  // If present delete ProtoGrid
  if (proto != ESMC_NULL_POINTER) delete proto;

  // Set to NULL so we can tell that proto has been deleted 
  proto = ESMC_NULL_POINTER;

  return ESMF_SUCCESS;
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

  // This is a little shakey with the default status setting, but there isn't
  // a good other option
   proto = ESMC_NULL_POINTER; 

  // default status not ready
   status=ESMC_GRIDSTATUS_NOT_READY;

   typekind = ESMC_TYPEKIND_R8;
   distRank = 0;
   dimmap = ESMC_NULL_POINTER; 
  
   undistRank = 0;
   lbounds = ESMC_NULL_POINTER; 
   ubounds = ESMC_NULL_POINTER; 
  
   rank=0;
   coordRank = ESMC_NULL_POINTER; 
   coordDimMap = ESMC_NULL_POINTER; 
  
   staggerLocCount=0;
   coordArrayList = ESMC_NULL_POINTER;
   coordLWidthList = ESMC_NULL_POINTER;
   coordUWidthList = ESMC_NULL_POINTER;

   gridIsDist = ESMC_NULL_POINTER;
   gridMapDim = ESMC_NULL_POINTER;

   coordIsDist = ESMC_NULL_POINTER;
   coordMapDim = ESMC_NULL_POINTER;

 
#if 1
   isDELBnd = ESMC_NULL_POINTER;
   isDEUBnd = ESMC_NULL_POINTER;
#endif  
   gridType=0;
   indexflag=ESMF_INDEX_DELOCAL;
   distgrid= ESMC_NULL_POINTER; 

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

   // If present delete ProtoGrid
   if (proto != ESMC_NULL_POINTER) delete proto;

   // delete internal storage
   if (distRank) {
     delete [] dimmap;
   }

   if (undistRank) {
     delete [] lbounds;
     delete [] ubounds;
   }

   if (rank) {
     delete [] coordRank;
     _free2D<int>(&coordDimMap);
     _free2D<Array *>(&coordArrayList);
     _free2D<int>(&coordLWidthList);
     _free2D<int>(&coordUWidthList);
     _free2D<int>(&coordAlignList);
     _free2D<bool>(&didIAllocList);
     delete [] gridIsDist;
     delete [] gridMapDim;
     _free2D<bool>(&coordIsDist); 
     _free2D<int>(&coordMapDim); 
  }

  if (isDELBnd != ESMC_NULL_POINTER) delete [] isDELBnd;
  if (isDEUBnd != ESMC_NULL_POINTER) delete [] isDEUBnd;
}



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::setCoordArray()"
//BOPI
// !IROUTINE:  Grid::setCoordArray
//
// !INTERFACE:
int Grid::setCoordArray(
//
// !RETURN VALUE:
//   return code
//
// !ARGUMENTS:
//
  int _staggerloc, // (in)
  int _coord,      // (in)
  Array *_array,   // (in)
  bool didIAlloc // (in)
  ){
//
// !DESCRIPTION:
//   Set a coordinate array in the grid structure
//
//EOPI
//-----------------------------------------------------------------------------
  int rc;

  // initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  
  // Check status
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Grid not fully created", &rc);
    return rc;
  }

  // Check staggerloc
  if ((_staggerloc < 0) || (_staggerloc >= staggerLocCount)) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- stagger location out of range", &rc);
    return rc;
  }

  // Check coord
  if ((_coord < 0) || (_coord >= rank)) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- coord out of range", &rc);
    return rc;
  }

  // Set array in list
  coordArrayList[_staggerloc][_coord] = _array;

  // Set alloc
  didIAllocList[_staggerloc][_coord]=didIAlloc;


  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::setCoordInfo()"
//BOPI
// !IROUTINE:  Grid::setCoordInfo
//
// !INTERFACE:
int Grid::setCoordInfo(
//
// !RETURN VALUE:
//   return code
//
// !ARGUMENTS:
//
  int _staggerloc, // (in)
  int *_coordAlign, // (in)
  int *_coordLWidth, // (in)
  int *_coordUWidth // (in)
  ){
//
// !DESCRIPTION:
//   Set a coordinate array in the grid structure
//
//EOPI
//-----------------------------------------------------------------------------
  int rc;

  // initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  
  // Check status
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Grid not fully created", &rc);
    return rc;
  }

  // Check staggerloc
  if ((_staggerloc < 0) || (_staggerloc >= staggerLocCount)) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- stagger location out of range", &rc);
    return rc;
  }

  // Set coordAlign
  for (int i=0; i<rank; i++) {
    coordAlignList[_staggerloc][i]=_coordAlign[i];
   }

  // Set coordLWidth
  for (int i=0; i<rank; i++) {
    coordLWidthList[_staggerloc][i]=_coordLWidth[i];
   }

  // Set coordUWidth
  for (int i=0; i<rank; i++) {
    coordUWidthList[_staggerloc][i]=_coordUWidth[i];
   }

  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::getStaggerLWidth()"
//BOPI
// !IROUTINE:  Grid::getStaggerLWidth()"
//
// !INTERFACE:
int Grid::getStaggerLWidth(
//
// !RETURN VALUE:
//   return code
//
// !ARGUMENTS:
//
  int _staggerloc,
  int _localDE, 
  int *_LWidth // needs to be of the same size as the grid rank
  ){
//
// !DESCRIPTION:
//   Returns the amount the Lower end of this DE should be shifted
//   to add the stagger padding specified by the coordLWidth.
//
//EOPI
//-----------------------------------------------------------------------------
  int rc;

  // initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  
  // Check status
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Grid not fully created", &rc);
    return rc;
  }

  // Check staggerloc
  if ((_staggerloc < 0) || (_staggerloc >= staggerLocCount)) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- stagger location out of range", &rc);
    return rc;
  }

  // Input Error Checking
  if ((_localDE < 0) || (_localDE >=distgrid->getDELayout()->getLocalDeCount())) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
          "- localDE outside range on this processor", &rc);
        return rc;
  }

  // Loop through grid rank dimensions 
  for (int i=0; i<rank; i++) {
    if (isDELBnd[_localDE] & (0x1 << i)) {
      _LWidth[i]=coordLWidthList[_staggerloc][i];
    } else {
      _LWidth[i]=0;
    }
  }

  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::getStaggerUWidth()"
//BOPI
// !IROUTINE:  Grid::getStaggerUWidth()"
//
// !INTERFACE:
int Grid::getStaggerUWidth(
//
// !RETURN VALUE:
//   return code
//
// !ARGUMENTS:
//
  int _staggerloc,
  int _localDE, 
  int *_UWidth // needs to be of the same size as the grid rank
  ){
//
// !DESCRIPTION:
//   Returns the amount the Lower end of this DE should be shifted
//   to add the stagger padding specified by the coordUWidth.
//
//EOPI
//-----------------------------------------------------------------------------
  int rc;

  // initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  
  // Check status
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Grid not fully created", &rc);
    return rc;
  }

  // Check staggerloc
  if ((_staggerloc < 0) || (_staggerloc >= staggerLocCount)) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- stagger location out of range", &rc);
    return rc;
  }

  // Input Error Checking
  if ((_localDE < 0) || (_localDE >=distgrid->getDELayout()->getLocalDeCount())) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
          "- localDE outside range on this processor", &rc);
        return rc;
  }

  // Loop through grid rank dimensions 
  for (int i=0; i<rank; i++) {
    if (isDEUBnd[_localDE] & (0x1 << i)) {
      _UWidth[i]=coordUWidthList[_staggerloc][i];
    } else {
      _UWidth[i]=0;
    }
  }

  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------




//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::getCoordArray()"
//BOPI
// !IROUTINE:  Grid::getCoordArray
//
// !INTERFACE:
int Grid::getCoordArray(
//
// !RETURN VALUE:
//   return code
//
// !ARGUMENTS:
//
  int _staggerloc, // (in)
  int _coord,      // (in)
  Array **_array   // (in)
  ){
//
// !DESCRIPTION:
//   Get a coordinate array from the grid structure
//
//EOPI
//-----------------------------------------------------------------------------
  int rc;
  Array *array;

  // initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  
  // Check status
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Grid not fully created", &rc);
    return rc;
  }

  // Check staggerloc
  if ((_staggerloc < 0) || (_staggerloc >= staggerLocCount)) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- stagger location out of range", &rc);
    return rc;
  }

  // Check coord
  if ((_coord < 0) || (_coord >= rank)) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- coord out of range", &rc);
    return rc;
  }

  // get Array pointer from List
  array=coordArrayList[_staggerloc][_coord];

  // Check if array has been set
  if (array==ESMC_NULL_POINTER) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- accessing unset coord array", &rc);
    return rc;
  }

  // output array
  *_array=array;

  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------





//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ProtoGrid()"
//BOPI
// !IROUTINE:  ProtoGridConstruct
//
// !INTERFACE:
ProtoGrid::ProtoGrid(
//
// !RETURN VALUE:
//    Pointer to a new proto grid
//
// !ARGUMENTS:
//  none
  ){
//
// !DESCRIPTION:
//    Construct the internal information structure of an ESMC\_ProtoGrid object.
//
//EOPI
//-----------------------------------------------------------------------------

  nameLen=0; 
  name=ESMC_NULL_POINTER;  
  typekind=ESMC_NULL_POINTER;
  distgrid=ESMC_NULL_POINTER;     
  dimmap=ESMC_NULL_POINTER;   
  lbounds=ESMC_NULL_POINTER;  
  ubounds=ESMC_NULL_POINTER;  
  coordRank=ESMC_NULL_POINTER;  
  coordDimMap=ESMC_NULL_POINTER; 
  indexflag=ESMC_NULL_POINTER; 
  gridType=ESMC_NULL_POINTER; 
}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::~ProtoGrid()"
//BOPI
// !IROUTINE:  ~ProtoGrid
//
// !INTERFACE:
 ProtoGrid::~ProtoGrid(void){
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
// none
//
// !DESCRIPTION:
//  Destructor for ProtoGrid, deallocates all internal memory, etc. 
//
//EOPI
//-----------------------------------------------------------------------------

  if (name != ESMC_NULL_POINTER) delete [] name;  
  if (typekind != ESMC_NULL_POINTER) delete typekind;
  // don't delete distgrid
  if (dimmap != ESMC_NULL_POINTER) _freeInterfaceInit(&dimmap);
  if (lbounds != ESMC_NULL_POINTER) _freeInterfaceInit(&lbounds);
  if (ubounds != ESMC_NULL_POINTER) _freeInterfaceInit(&ubounds);
  if (coordRank != ESMC_NULL_POINTER) _freeInterfaceInit(&coordRank);
  if (coordDimMap != ESMC_NULL_POINTER) _freeInterfaceInit(&coordDimMap);
  if (indexflag != ESMC_NULL_POINTER) delete indexflag; 
  if (gridType != ESMC_NULL_POINTER) delete gridType; 
}




} // END ESMCI name space
//-----------------------------------------------------------------------------
