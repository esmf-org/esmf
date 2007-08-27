// $Id: ESMCI_Grid.C,v 1.20 2007/08/27 21:04:32 oehmke Exp $
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
static const char *const version = "$Id: ESMCI_Grid.C,v 1.20 2007/08/27 21:04:32 oehmke Exp $";
//-----------------------------------------------------------------------------

#define VERBOSITY             (1)       // 0: off, 10: max


//-----------------------------------------------------------------------------


// Set up ESMCI name space for these methods
namespace ESMCI{  

//  File Local Prototypes (actual implementation at end of file)
static int _NumStaggerLocsFromRank(int rank);

template <class Type>
static  Type **_allocate2D(int sizeDim1, int sizeDim2);

template <class Type>
static  void _free2D(Type ***array);

template <class Type>
static  Type ***_allocate3D(int sizeDim1, int sizeDim2, int sizeDim3);

template <class Type>
static  void _free3D(Type ****array);

static InterfaceInt *_copyInterfaceInt(InterfaceInt *in);
 
static void _freeInterfaceInt(InterfaceInt **in);

static int _createIsDEBnd(char **_isDELBnd, char **_isDEUBnd, DistGrid *distgrid,int *dimmap);

int construct(Grid *_grid, int _nameLen, char *_name, ESMC_TypeKind *_typekind,
              DistGrid *_distgrid, InterfaceInt *_dimmap, InterfaceInt *_lbounds,
              InterfaceInt *_ubounds, InterfaceInt *_coordRank, 
              InterfaceInt *_coordDimMap, ESMC_IndexFlag *_indexflag,
              int *_gridType);


//-----------------------------------------------------------------------------
//
// Public Interfaces
//
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::allocCoordArray()"
//BOP
// !IROUTINE:  allocCoordArray
//
// !INTERFACE:
int Grid::allocCoordArray(

//
// !RETURN VALUE:
//   Return code
//
// !ARGUMENTS:
//
                          Grid *gridArg,                  // (in) 
                          int *staggerlocArg,             // (in) optional
                          InterfaceInt *staggerLWidthArg, // (in) optional
                          InterfaceInt *staggerUWidthArg, // (in) optional
                          InterfaceInt *staggerAlignArg   // (in) optional 
  ) {
//
// !DESCRIPTION:
//   Create the storage (ESMF Array objects) to hold the coordinates for a stagger
// location. Note that this subroutine creates an Array for each coordinate component
// in the stagger location. The Arrays are, of course, created with the correct size
// and shape to hold the coordinates. 
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int status;                 // local error status
  int rc, localrc;
  int staggerloc;
  int coord;
  ESMC_DataCopy docopy;
  int *staggerAlign;
  int *staggerLWidth;
  int *staggerUWidth;
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

  // initialize return code; assume routine not implemented
  status = ESMC_RC_NOT_IMPL;
  rc = ESMC_RC_NOT_IMPL;
  
  // Make sure we're passing in a valid Grid pointer
  if (gridArg == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to grid argument", &rc);
    return rc;
  }

  // Make sure the grid has the correct status for this action
  if (gridArg->getStatus() < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
      "- grid not of correct status to perform this operation", &rc);
    return rc;
  }  

  // If staggerloc hasn't been passed in use a default, otherwise copy it. 
  if (staggerlocArg==NULL) {
    staggerloc=0;  // default
  } else {
    staggerloc=*staggerlocArg;
  }

  // get the grid rank
  rank=gridArg->getRank();

  // If staggerAlignArg hasn't been passed in then set a default, otherwise
  // error check and copy the passed in value.  
  staggerAlign = new int[rank];
  if (staggerAlignArg == NULL) {
    for (int i=0; i<rank; i++)
      staggerAlign[i] = -1; // set staggerAlign to default (-1,-1,-1...)
  } else {
    //// Ensure staggerAlign has the correct rank
    if (staggerAlignArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- staggerAlign array must be of rank 1", &rc);
      return rc;
    }
    //// Ensure staggerAlign has the correct size
    if (staggerAlignArg->extent[0] != rank){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- staggerAlign size and Grid rank mismatch ", &rc);
      return rc;
    }
    for (int i=0; i<rank; i++){
      //// Ensure staggerAlign values are -1,0,1
      if ((staggerAlignArg->array[i] < -1) || (staggerAlignArg->array[i] > 1)) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                 "- staggerAlign must be either -1, 0, or 1", &rc);
        return rc;
      }
      staggerAlign[i] = staggerAlignArg->array[i];  // copy staggerAlign array element
    }
  }

  // Clip the Alignment against the stagger (ignore centered dimensions)
  for (int i=0; i<rank; i++) {
    if (!(staggerloc & (0x1<<i))) {
      staggerAlign[i]=0;
    }
  }

  // If staggerLWidth & staggerUWidth haven't been passed in then set staggerLWidth
  // based on staggerAlign. If staggerLWidth hasn't been passed in, but staggerUWidth has
  // then set staggerLWidth to 0. If staggerLWidth has been passed in then error check and
  // copy.
  staggerLWidth = new int[rank];
  if (staggerLWidthArg == NULL) {
    if (staggerUWidthArg == NULL) {
      for (int i=0; i<rank; i++) {
        if (staggerAlign[i]>0) {
          staggerLWidth[i] = 1; 
        } else {
          staggerLWidth[i] = 0; 
        }
      }
    } else {
      for (int i=0; i<rank; i++) {
        staggerLWidth[i] = 0; 
      }
    }
  } else {
    //// Ensure staggerLWidth is of the correct rank 
    if (staggerLWidthArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- staggerLWidth array must be of rank 1", &rc);
      return rc;
    }
    //// Ensure staggerUWidth is of the correct size
    if (staggerLWidthArg->extent[0] != rank){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- staggerLWidth size and Grid rank mismatch ", &rc);
      return rc;
    }
    for (int i=0; i<rank; i++){
      staggerLWidth[i] = staggerLWidthArg->array[i];  // copy staggerLWidth array element
    }
  }

  // If staggerLWidth & staggerUWidth haven't been passed in then set staggerUWidth
  // based on staggerAlign. If staggerUWidth hasn't been passed in, but staggerLWidth has
  // then set staggerUWidth to 0. If staggerUWidth has been passed in then error check and
  // copy.
  staggerUWidth = new int[rank];
  if (staggerUWidthArg == NULL) {
    if (staggerLWidthArg == NULL) {
      for (int i=0; i<rank; i++) {
        if (staggerAlign[i]<0) {
          staggerUWidth[i] = 1; 
        } else {
          staggerUWidth[i] = 0; 
        }
      }
    } else {
      for (int i=0; i<rank; i++) {
        staggerUWidth[i] = 0; 
      }
    }
  } else {
    //// Ensure staggerUWidth is of the correct rank
    if (staggerUWidthArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
                   "- staggerUWidth array must be of rank 1", &rc);
      return rc;
    }
    //// Ensure staggerUWidth is of the correct size
    if (staggerUWidthArg->extent[0] != rank){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- staggerUWidth size and Grid rank mismatch ", &rc);
      return rc;
    }
    for (int i=0; i<rank; i++){
      staggerUWidth[i] = staggerUWidthArg->array[i];  // copy staggerUWidth array element
    }
  }


  // Get the mapping from the Grid dimensions to 
  // the distgrid and bounds array
  gridIsDist=gridArg->getGridIsDist();
  gridMapDim=gridArg->getGridMapDim();

  // Get the mapping from the coordinate array dimensions to 
  // the Grids distributed and undistributed dimensions
  coordIsDist=gridArg->getCoordIsDist();
  coordMapDim=gridArg->getCoordMapDim();

  // Get the coordinate rank and the mapping of the coordinate
  // dimensions to those of the grid.
  coordRank=gridArg->getCoordRank();
  coordDimMap=gridArg->getCoordDimMap();

  // Get the grid's undistributed dimension's bounds. 
  gridLBounds=gridArg->getLbounds();
  gridUBounds=gridArg->getUbounds();

  // get IndexFlag
  indexflag=gridArg->getIndexFlag();

  // construct ArraySpec for using to call Array::create
  ESMC_ArraySpec *arrayspec= new ESMC_ArraySpec();     
    
  // Construct empty InterfaceInt structures for using to call Array::create
  int *lboundsArray=new int[rank];
  extent[0]=rank;
  InterfaceInt *lbounds=new InterfaceInt(lboundsArray,1,extent);

  int *uboundsArray=new int[rank];
  extent[0]=rank;
  InterfaceInt *ubounds=new InterfaceInt(uboundsArray,1,extent); 

  int *dimmapArray=new int[rank];
  extent[0]=rank;
  InterfaceInt *dimmap=new InterfaceInt(dimmapArray,1,extent); 

  int *compLWidthArray=new int[rank];
  extent[0]=rank;
  InterfaceInt *compLWidth=new InterfaceInt(compLWidthArray,1,extent); 

  int *compUWidthArray=new int[rank];
  extent[0]=rank;
  InterfaceInt *compUWidth=new InterfaceInt(compUWidthArray,1,extent); 


  ////////////
  ///// Loop Constructing all the coordinate arrays
  ///////////
  for (coord=0; coord<rank; coord++) {
    
    // fill in ArraySpec with information describing coordinate
    arrayspec->ESMC_ArraySpecSetRank(coordRank[coord]);
    arrayspec->ESMC_ArraySpecSetTypeKind(gridArg->getTypeKind());
    

    //// fill in dimmap, lbounds, and ubounds for use in Array::create
    //// dimmap - computed by inverting how coords dims map to distgrid
    //// bounds - computed by matching up an undistributed coord dimension
    ////          with the bound values at the corresponding grid dimension 
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
    
    // set size of dimmap (number of coord distributed dimensions)
    if (coordDistRank) {
      dimmap->extent[0]=coordDistRank;
    } else {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
                                            "- Coordinate must have a distributed portion", &rc);
      return rc;
    }
    
    // set size of bounds (number of coord undistributed dimensions)
    if (coordUndistRank) {
      lbounds->extent[0]=coordUndistRank;
      ubounds->extent[0]=coordUndistRank;
    }
    
    // Init ComputationalLWidth
    for (int i=0; i<coordDistRank; i++) {
      compLWidthArray[i]=0; // init to 0.
    }
    compLWidth->extent[0]=coordDistRank;
    
    // init ComputationalUWidth to 0
    for (int i=0; i<coordDistRank; i++) {
      compUWidthArray[i]=0; // init to 0.
    }
    compUWidth->extent[0]=coordDistRank;
    
    //// Expand the boundaries of the computational region of the Array 
    //// (distributed and undistributed) to hold the stagger padding
    for (int i=0; i<coordRank[coord]; i++) {
      int gi=coordDimMap[coord][i];
      if (coordIsDist[coord][i]) {
        compUWidthArray[coordMapDim[coord][i]] = staggerUWidth[gi]; 
        compLWidthArray[coordMapDim[coord][i]] = staggerLWidth[gi];
        
      } else {
        uboundsArray[coordMapDim[coord][i]] += staggerUWidth[gi];
        lboundsArray[coordMapDim[coord][i]] -= staggerLWidth[gi];
      }
    }

    // Create an Array to hold the coords 
    if (coordUndistRank) { 
      // Pass in lbounds, ubounds if there are undistributed dims...
      array=Array::create(arrayspec, (DistGrid *)gridArg->getDistGrid(),
                          dimmap, compLWidth, compUWidth, 
                          (InterfaceInt *)ESMC_NULL_POINTER,
                          (InterfaceInt *)ESMC_NULL_POINTER,
                          &indexflag, &staggerloc, ESMC_NULL_POINTER, 
                          lbounds, ubounds, &localrc);
    } else {
      // ...otherwise pass in NUll
      array=Array::create(arrayspec, (DistGrid *)gridArg->getDistGrid(),
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

    // Set newly created Array into Grid
    localrc=gridArg->setCoordArrayInternal(staggerloc, coord, array, true);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
                                              ESMF_ERR_PASSTHRU, &rc)) return rc;        
    
  } // end of coord loop


  // Set information about this stagger's coordinates into the Grid
  localrc=gridArg->setStaggerInfo(staggerloc, staggerAlign, staggerLWidth, staggerUWidth);
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
  if (staggerUWidth != ESMC_NULL_POINTER) delete [] staggerUWidth;
  if (staggerLWidth != ESMC_NULL_POINTER) delete [] staggerLWidth;

  // return ESMF_SUCCESS
  return ESMF_SUCCESS;
  }
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::commit()"
//BOP
// !IROUTINE:  commit
//
// !INTERFACE:
int Grid::commit(
//
// !RETURN VALUE:
//   Error code
//
// !ARGUMENTS:
//
                 Grid *gridArg  // (in)
 ){
//
// !DESCRIPTION:
//   This call is the final step of the create empty/set/commit incremental
// method for creating a Grid. The \gridArg parameter passed in here should
// have been created with create(rc), then filled with information using set set().
// After this call the grid object will be usable in other methods, but may no longer
// be the subject of set().
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int rc;
  int localrc;                 // local error status
  ProtoGrid *proto;

  // initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  // make sure the gridArg is a valid pointer
  if (gridArg == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to grid argument", &rc);
    return rc;
  }

  // Make sure that we're the correct status
  if (gridArg->getStatus() != ESMC_GRIDSTATUS_NOT_READY) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_BAD,
      "- Can't use commit on an already created object", &rc);
    return rc;
  }

 // Get the protoGrid which holds the information from set()
  proto=gridArg->getProtoGrid();
  if (proto == ESMC_NULL_POINTER) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_BAD,
      "- Null protoGrid ", &rc);
    return rc;
  }

  // setup the grid's internal structures 
  localrc=construct(gridArg, proto->nameLen, proto->name, proto->typekind, 
                       proto->distgrid, proto->dimmap, proto->lbounds,
                       proto->ubounds, proto->coordRank, proto->coordDimMap,
                       proto->indexflag,  proto->gridType);
   if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
            ESMF_ERR_PASSTHRU, &rc)) return rc;        

   // Now that we don't need it anymore, remove the protogrid from the grid
   localrc=gridArg->delProtoGrid();
   if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
                       ESMF_ERR_PASSTHRU, &rc)) return rc;        

  // return successfully
  return ESMF_SUCCESS;
  }
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::create()"
//BOP
// !IROUTINE:  create
//
// !INTERFACE:
Grid *Grid::create(
//
// !RETURN VALUE:
//    Grid * to newly allocated ESMC_Grid
//
// !ARGUMENTS:
//
  int nameLenArg,                           // (in) 
  char *nameArg,                            // (in) optional
  ESMC_TypeKind *typekindArg,               // (in) optional
  DistGrid *distgridArg,                    // (in) optional
  InterfaceInt *dimmapArg,                  // (in) optional
  InterfaceInt *lboundsArg,                 // (in) optional
  InterfaceInt *uboundsArg,                 // (in) optional
  InterfaceInt *coordRankArg,               // (in) optional
  InterfaceInt *coordDimMapArg,             // (in) optional
  ESMC_IndexFlag *indexflagArg,             // (in) optional
  int *gridTypeArg,                         // (in) optional
  int *rcArg                                // (out) return code optional
  ){
//
// !DESCRIPTION:
//    Create an {\tt ESMC\gridArg} object from a DistGrid. This method sets up
//  the internal structures of the Grid. After calling this create the 
//  returned Grid object may be used in most other Grid methods. 
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                 // local error status

  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  if (rcArg!=NULL)
    *rcArg = ESMC_RC_NOT_IMPL;

  // allocate the grid object
  Grid *grid=ESMC_NULL_POINTER;
  try{
    grid = new Grid();
  }catch(...){
     // allocation error
     ESMC_LogDefault.ESMC_LogMsgAllocError("for new ESMC_Grid.", rcArg);  
     return ESMC_NULL_POINTER;
  }

  // setup the grids internal structure using the passed in paramters. 
  localrc=construct(grid, nameLenArg, nameArg,typekindArg, distgridArg, dimmapArg, 
                       lboundsArg, uboundsArg, coordRankArg, coordDimMapArg, indexflagArg, 
                       gridTypeArg);
   if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
            ESMF_ERR_PASSTHRU, rcArg)) return ESMC_NULL_POINTER;        

  // return successfully
  *rcArg = ESMF_SUCCESS;
  return grid;
}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::create()"
//BOP
// !IROUTINE:  createEmpty
//
// !INTERFACE:
Grid *Grid::create(
//
// !RETURN VALUE:
//    Grid * to newly allocated ESMC_Grid
//
// !ARGUMENTS:
//
                   int *rcArg  // (out) optional return code 
  ){
//
// !DESCRIPTION:
//    Create an empty {\tt ESMC\_Grid} object. The returned Grid object may only
// be used with set and commit. To make this object usable employ {\tt set} to fill
// the object with parameters and then {\tt commit} to construct a usable Grid based
// on those paramters. 
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                 // local error status

  // TODO: Change to get rid of return codes in favor of try-catch

  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  if (rcArg!=NULL)
    *rcArg = ESMC_RC_NOT_IMPL;
  
  // allocate the new Grid object
  Grid *grid=ESMC_NULL_POINTER;
  try{
    grid = new Grid();
  }catch(...){
     // allocation error
     ESMC_LogDefault.ESMC_LogMsgAllocError("for new ESMC_Grid.", rcArg);  
     return ESMC_NULL_POINTER;
  }

  // Add a protogrid to hold the information that will eventually be used
  // by commit to construct the internal structures of the Grid
  localrc=grid->addProtoGrid();
   if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
            ESMF_ERR_PASSTHRU, rcArg)) return ESMC_NULL_POINTER;        

  // return successfully
  *rcArg = ESMF_SUCCESS;

  return grid;
  }
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::destroy()"
//BOP
// !IROUTINE:  destroy
//
// !INTERFACE:
int Grid::destroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
 Grid **gridArg){  // in - Grid to destroy
//
// !DESCRIPTION:
// Deallocate a Grid's internal memory and then deallocate the Grid object itself. 
// The grid parameter is set to ESMC_NULL_POINTER. 
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                // automatic variable for local return code

  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // return with error for NULL pointer
  if (gridArg == ESMC_NULL_POINTER || *gridArg == ESMC_NULL_POINTER){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to Grid",&localrc);
    return localrc;
  }

  // destruct and delete Grid object
  try{
     delete *gridArg;
  }catch(...){
     // TODO: Change to get rid of return codes
     // deallocation error
     ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_WRONG,
      "- Error occurred in ~Grid ",&localrc);
    return localrc;
  }

  // set gridArg to Null
  *gridArg = ESMC_NULL_POINTER;
  
  // return successfully
  return ESMF_SUCCESS;
}



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::getCoordArray()"
//BOP
// !IROUTINE:  gridCoordArray
//
// !INTERFACE:
int Grid::getCoordArray(

//
// !RETURN VALUE:
//   Return code
//
// !ARGUMENTS:
//
                        Grid *gridArg,            // (in)
                        int *staggerlocArg,       // (in) optional
                        int *coordArg,            // (in)
                        Array **arrayArg,         // (out)
                        ESMC_DataCopy *docopyArg  // (in) optional
  ) {
//
// !DESCRIPTION:
//    Get the Array object holding the coordinates values for stagger location
//    {\tt staggerloc} and coordinate component {\tt coord}.
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int status;                 // local error status
  int rc;
  int staggerloc;
  int coord;
  ESMC_DataCopy docopy;
  int rank;

  // initialize return code; assume routine not implemented
  status = ESMC_RC_NOT_IMPL;
  rc = ESMC_RC_NOT_IMPL;

  // make sure we're operating on a valid Grid pointer
  if (gridArg == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to grid argument", &rc);
    return rc;
  }

  // make sure grid is the correct status for this action
  if (gridArg->getStatus() < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
      "- grid not of correct status to perform this operation", &rc);
    return rc;
  }  
  
  // If staggerlocArg wasn't passed in, use default, else copy the value.
  if (staggerlocArg==NULL) {
    staggerloc=0;  // default center
  } else {
    staggerloc=*staggerlocArg;
  }

  // Make sure they passed in the coordinate component, and if so
  // translate to C++ base-0.
  if (coordArg==NULL) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Must pass in coord argument", &rc);
    return rc;
  } else {
    coord=(*coordArg)-1; // translate from F90 1 based to 0 based
  }

  // make sure arrayArg pointer is valid
  if (arrayArg == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to pass array out through", &rc);
    return rc;
  }

  // If docopyArg wasn't passed in, use default, else copy the value
  if (docopyArg==NULL) {
    docopy=ESMC_DATA_REF;  // default
  } else {
    docopy=*docopyArg;
  }

  // Copy option isn't working for now
  if (docopy==ESMC_DATA_COPY) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_IMPL,
        "- Data Copy Flag not implemented yet", &rc);
      return rc;
  }

  // Get Coord Array
  // (note that arrayArg is already a ** to Array)
  rc=gridArg->getCoordArrayInternal(staggerloc, coord, arrayArg);

   // return what getCoordArray returned
  return rc;
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
  int staggerlocArg,  // (in)
  int localDEArg,     // (in)
  int *LWidthArg      // (out) needs to be of the same size as the grid rank
  ){
//
// !DESCRIPTION:
//   Returns the amount the Lower end of this DE should be shifted
//   to add the stagger padding specified by the staggerLWidth in the Grid
//   for this stagger location.
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
  if ((staggerlocArg < 0) || (staggerlocArg >= staggerLocCount)) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- stagger location out of range", &rc);
    return rc;
  }

  // Ensure localDEArg isn't out of range for this PET
  if ((localDEArg < 0) || (localDEArg >=distgrid->getDELayout()->getLocalDeCount())) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
          "- localDE outside range on this processor", &rc);
        return rc;
  }

  // Loop through grid rank dimensions adding the staggerLwidth
  // if we're on a lower boundary 
  for (int i=0; i<rank; i++) {
    if (isDELBnd[localDEArg] & (0x1 << i)) {
      LWidthArg[i]=staggerLWidthList[staggerlocArg][i];
    } else {
      LWidthArg[i]=0;
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
  int staggerlocArg, // (in)
  int localDEArg,    // (in)
  int *UWidthArg     // (out) needs to be of the same size as the grid rank
  ){
//
// !DESCRIPTION:
//   Returns the amount the Lower end of this DE should be shifted
//   to add the stagger padding specified by the staggerUWidth set in the 
//   Grid for this stagger location.
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
  if ((staggerlocArg < 0) || (staggerlocArg >= staggerLocCount)) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- stagger location out of range", &rc);
    return rc;
  }

  // Input Error Checking
  if ((localDEArg < 0) || (localDEArg >=distgrid->getDELayout()->getLocalDeCount())) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
          "- localDE outside range on this processor", &rc);
        return rc;
  }

  // Loop through grid rank dimensions setting the staggerUWidth
  // for dimensions for which this local de is on the upper boundary
  for (int i=0; i<rank; i++) {
    if (isDEUBnd[localDEArg] & (0x1 << i)) {
      UWidthArg[i]=staggerUWidthList[staggerlocArg][i];
    } else {
      UWidthArg[i]=0;
    }
  }

  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::set()"
//BOP
// !IROUTINE:  set
//
// !INTERFACE:
int Grid::set(
//
// !RETURN VALUE:
//   Error code
//
// !ARGUMENTS:
//
  int nameLenArg,                // (in) optional
  char *nameArg,                 // (in) optional
  ESMC_TypeKind *typekindArg,    // (in) optional
  DistGrid *distgridArg,         // (in) optional
  InterfaceInt *dimmapArg,       // (in) optional
  InterfaceInt *lboundsArg,      // (in) optional
  InterfaceInt *uboundsArg,      // (in) optional
  InterfaceInt *coordRankArg,    // (in) optional
  InterfaceInt *coordDimMapArg,  // (in) optional
  ESMC_IndexFlag *indexflagArg,  // (in) optional
  int *gridTypeArg               // (in) optional
  ){
//
// !DESCRIPTION:
//   As the second part of the create empty/set/commit incremental grid creation 
//   paradigm, this subroutine is used to set values in a Grid in preperation for 
//   a later commit. This method may be called multiple times to set different
//   sets of parameters. If the same parameter is set twice, the second value
//   overwrites the first. 
//   
//   TODO: eventually seperate this into a bunch of seperate sets to allow easier access from C.
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int rc;
  int localrc;                 // local error status

  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Make sure that we haven't been created
  if (status != ESMC_GRIDSTATUS_NOT_READY) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_WRONG,
      "- Can't use set on an already created object", &localrc);
    return localrc;
  }
  
  // Make sure the protoGrid exists
  if (proto == ESMC_NULL_POINTER) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_BAD,
      "- Null protoGrid ", &localrc);
    return localrc;
  }
  
  // if passed in, set name 
  if (nameArg != ESMC_NULL_POINTER && nameLenArg > 0) {
    // if present get rid of the old data
    if (proto->name!=ESMC_NULL_POINTER) delete [] proto->name;

    // record name
    proto->nameLen=nameLenArg;
    proto->name= new char[nameLenArg];
    memcpy(proto->name, nameArg, nameLenArg * sizeof(char));
  } 

  //  if passed in, set typekind
  if (typekindArg != ESMC_NULL_POINTER) {
    if (proto->typekind == ESMC_NULL_POINTER) proto->typekind= new ESMC_TypeKind;
    *(proto->typekind)=*typekindArg;
  }

  // if passed in, set distgrid
  if (distgridArg != ESMC_NULL_POINTER) {
    proto->distgrid=distgridArg;
  }

  // if passed in, set dimmap
  if (dimmapArg != ESMC_NULL_POINTER) { 
    // if present get rid of the old data
    if (proto->dimmap !=ESMC_NULL_POINTER) _freeInterfaceInt(&proto->dimmap);

    // record the new data
    proto->dimmap=_copyInterfaceInt(dimmapArg);
  }

  // if passed in, set lbounds
  if (lboundsArg != ESMC_NULL_POINTER) { 
    // if present get rid of the old data
    if (proto->lbounds !=ESMC_NULL_POINTER) _freeInterfaceInt(&proto->lbounds);

    // record the new data
    proto->lbounds=_copyInterfaceInt(lboundsArg);
  }

  // if passed in, set ubounds
  if (uboundsArg != ESMC_NULL_POINTER) { 
    // if present get rid of the old data
    if (proto->ubounds !=ESMC_NULL_POINTER) _freeInterfaceInt(&proto->ubounds);

    // record the new data
    proto->ubounds=_copyInterfaceInt(uboundsArg);
  }

  // if passed in, set coordRank
  if (coordRankArg != ESMC_NULL_POINTER) { 
    // if present get rid of the old data
    if (proto->coordRank !=ESMC_NULL_POINTER) _freeInterfaceInt(&proto->coordRank);

    // record the new data
    proto->coordRank=_copyInterfaceInt(coordRankArg);
  }

  // if passed in, set coordDimMap
  if (coordDimMapArg != ESMC_NULL_POINTER) { 
    // if present get rid of the old data
    if (proto->coordDimMap !=ESMC_NULL_POINTER) _freeInterfaceInt(&proto->coordDimMap);

    // record the new data
    proto->coordDimMap=_copyInterfaceInt(coordDimMapArg);
  }

  // if passed in, set indexflag
  if (indexflagArg!=NULL) {
    if (proto->indexflag == ESMC_NULL_POINTER) proto->indexflag= new ESMC_IndexFlag;
    *(proto->indexflag)=*indexflagArg;
  }

  // if passed in, set gridType
  if (gridTypeArg!=NULL) {
    if (proto->gridType == ESMC_NULL_POINTER) proto->gridType= new int;
    *(proto->gridType)=*gridTypeArg;
  }
 
  // return successfully
  return ESMF_SUCCESS;
  }
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::setCoordArray()"
//BOP
// !IROUTINE:  setCoordArray
//
// !INTERFACE:
int Grid::setCoordArray(

//
// !RETURN VALUE:
//   Return code
//
// !ARGUMENTS:
//
                        Grid *gridArg,             // (in)
                        int *staggerlocArg,        // (in) optional
                        int *coordArg,             // (in) 
                        Array *arrayArg,           // (in)
                        ESMC_DataCopy *docopyArg   // (in) optional
  ) {
//
// !DESCRIPTION:
//    Set {\tt arrayArg} as the coordinate Array for stagger location {\tt staggerlocArg}
//  and coordinate component {\tt coordArg}. Use either a copy or a direct reference
//  depending on the value of {\tt docopyArg}.
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

   // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  rc = ESMC_RC_NOT_IMPL;
  
  // Make sure is a valid grid pointer
  if (gridArg == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to grid argument", &rc);
    return rc;
  }

  // make sure grid is active
  if (gridArg->getStatus() < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
      "- grid not of correct status to perform this operation", &rc);
    return rc;
  }  


  // If staggerlocArg not passed in, then set a default otherwise copy
  if (staggerlocArg==NULL) {
    staggerloc=0;  // default to center
  } else {
    staggerloc=*staggerlocArg;
  }

  // Make sure a valid coordinate component has been passed in
  // and then translate to 0-based. 
  if (coordArg==NULL) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Must pass in coord argument", &rc);
    return rc;

  } else {
    coord=(*coordArg)-1; // translate from 1 based to 0 based
  }

  // Make sure arrayArg is a valid pointer
  if (arrayArg == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to array argument", &rc);
    return rc;
  }

  // If docopyArg hasn't been passed in use a default otherwise, copy it. 
  if (docopyArg==NULL) {
    docopy=ESMC_DATA_REF;  // default
  } else {
    docopy=*docopyArg;
  }

  // Don't support copy right now
  if (docopy==ESMC_DATA_COPY) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_IMPL,
        "- Data Copy Flag not implemented yet", &rc);
      return rc;
  }

  // Get information about the rank of the coordinate compoments and
  // their mapping to the grid dimensions
  coordRank=gridArg->getCoordRank();
  coordDimMap=gridArg->getCoordDimMap();
 
  // Ensure the passed in array has the correct rank
  if (coordRank[coord] != arrayArg->getRank()){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- Array and Grid coord rank mismatch ", &rc);
      return rc;
    }

  // Ensure the passed in array has the correct typekind
  if (gridArg->getTypeKind() != arrayArg->getTypekind()){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- Array and Grid TypeKind mismatch ", &rc);
      return rc;
    }

  // Ensure the passed in array has the correct distgrid
  if (gridArg->getDistGrid() != arrayArg->getDistGrid()){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- Array and Grid DistGrid mismatch ", &rc);
      return rc;
    }

  // Ensure the passed in array has the correct indexflag
  if (gridArg->getIndexFlag() != arrayArg->getIndexflag()){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- Array and Grid Indexflag mismatch ", &rc);
      return rc;
    }

  // Get the mapping from the Grid dimensions to 
  // the distgrid and bounds array
  gridIsDist=gridArg->getGridIsDist();
  gridMapDim=gridArg->getGridMapDim();

  // Get the mapping from the coordinate array dimensions to 
  // the Grids distributed and undistributed dimensions
  coordIsDist=gridArg->getCoordIsDist();
  coordMapDim=gridArg->getCoordMapDim();

  // Check that the passed in Array's dimmap is consistant with this coord's
  arrayDimMap=arrayArg->getDimmap();
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
  arrayLBounds=arrayArg->getLBounds();
  arrayUBounds=arrayArg->getUBounds();
  gridLBounds=gridArg->getLbounds();
  gridUBounds=gridArg->getUbounds();
  int offset[ESMF_MAXDIM];
  int staggerLBnd[ESMF_MAXDIM];
  int staggerUBnd[ESMF_MAXDIM];
  int compLBnd[ESMF_MAXDIM];
  int compUBnd[ESMF_MAXDIM];
  int distRank=arrayArg->getDistGrid()->getDimCount();
  int localDECount=gridArg->getDistGrid()->getDELayout()->getLocalDeCount();
  for (int lDE=0; lDE < localDECount; lDE++) {
    
    //// Calculate the stagger lower bounds from the array
    ////// Get exclusive bounds (since the grid and array have
    ////// the same distgrid their exclusive bounds are the same)
   const int *exLBnd=arrayArg->getExclusiveLBound()+lDE*distRank;

    ////// Get stagger Lbnd offset
    localrc=gridArg->getStaggerLWidth(staggerloc, lDE, offset);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
                                              &rc)) return rc; 
    ////// Fill in the staggerLBnd array
    ////// distributed bounds are exclusive bounds minus stagger padding
    ////// undistributed bounds are lbounds minus stagger padding
    for (int i=0; i<coordRank[coord]; i++) {
      int gi=coordDimMap[coord][i];
      if (coordIsDist[coord][i]) {
        staggerLBnd[i]=exLBnd[coordMapDim[coord][i]]-offset[gi];
      } else {
        staggerLBnd[i]=gridLBounds[gridMapDim[gi]]-offset[gi];
      }
    }

    //// get computationalLBound from the array
    ///// get the computational bounds of the localDE
    const int *arrayCompLBnd=arrayArg->getComputationalLBound()+lDE*distRank;

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
    ////// Get array exclusive bounds (since the grid and array have
    ////// the same distgrid their exclusive bounds are the same)
   const int *exUBnd=arrayArg->getExclusiveUBound()+lDE*distRank;

    ////// Get stagger Ubnd offset
    localrc=gridArg->getStaggerUWidth(staggerloc, lDE, offset);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
                                              &rc)) return rc; 
    ////// Fill in the staggerLBnd array
    ////// distributed bounds are exclusive bounds plus stagger padding
    ////// undistributed bounds are ubounds plus stagger padding
    for (int i=0; i<coordRank[coord]; i++) {
      int gi=coordDimMap[coord][i];
      if (coordIsDist[coord][i]) {
        staggerUBnd[i]=exUBnd[coordMapDim[coord][i]]+offset[gi];
      } else {
        staggerUBnd[i]=gridUBounds[gridMapDim[gi]]+offset[gi];
      }
    }
    
    //// get computationalUBound from the array
    ///// get the computational bounds of the localDE
    const int *arrayCompUBnd=arrayArg->getComputationalUBound()+lDE*distRank;

    ///// Fill in the compLBnd array
    for (int i=0; i<coordRank[coord]; i++) {
      if (coordIsDist[coord][i]) {
        compUBnd[i]=arrayCompUBnd[coordMapDim[coord][i]];
      } else {
        compUBnd[i]=arrayUBounds[coordMapDim[coord][i]];
      }
    }

    //// Make sure the grid's stagger upper bounds fit within the array computational upper bounds
    for (int i=0; i<coordRank[coord]; i++) {
      if (compUBnd[i] < staggerUBnd[i]) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
                   "- Array computationalUBound or UBounds insufficient to hold grid+stagger bounds ", &rc);
      return rc;

      }
    }
  }
 
  // If we've reached this point then arrayArg is of the right size and shape
  // to hold the coordinates in coord, so put it in. 
  rc=gridArg->setCoordArrayInternal(staggerloc, coord, arrayArg, false);

  // return what setCoordArrayInternal returned
  return rc;
  }
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
//  Grid Class Internal Routines:
//
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
#define ESMC_METHOD "ESMCI::constructInternal()"
//BOPI
// !IROUTINE:  constructInternal
//
// !INTERFACE:
int Grid::constructInternal(
//
// !RETURN VALUE:
//    error code
//
// !ARGUMENTS:
//
  char *nameArg,                          // (in)
  ESMC_TypeKind typekindArg,              // (in)
  DistGrid *distgridArg,                  // (in)
  int distRankArg,                        // (in)
  int *dimmapArg,                         // (in)
  int undistRankArg,                      // (in)
  int *lboundsArg,                        // (in)
  int *uboundsArg,                        // (in)
  int rankArg,                            // (in)
  int *coordRankArg,                      // (in)
  int **coordDimMapArg,                   // (in)
  ESMC_IndexFlag indexflagArg,            // (in)
  int gridTypeArg                         // (in)
  ){
//
// !DESCRIPTION:
//    Construct the internal information structure of an ESMC\_Grid object.
//    No error checking wrt consistency of input arguments is needed because
//    constructInternal() is only to be called by construct() interfaces which
//    are responsible for providing consistent arguments to this layer.
//
//EOPI
//-----------------------------------------------------------------------------

  // Copy values into the grid object
  typekind = typekindArg;

  distgrid = distgridArg;

  distRank=distRankArg;

  undistRank=undistRankArg;

  rank = rankArg;

  indexflag=indexflagArg;

  gridType=gridTypeArg;

  // Set the number of stagger locations from the grid rank
  staggerLocCount=_NumStaggerLocsFromRank(rank); 

  // if there are distributed dimensions - allocate and copy dimmap
  if (distRank) {
     dimmap = new int[distRank];
     memcpy(dimmap, dimmapArg, distRank * sizeof(int));
  }

  // if there are undistributed dimensions - allocate and copy bounds
  if (undistRank) {
    lbounds = new int[undistRank];
    memcpy(lbounds, lboundsArg, undistRank * sizeof(int));
    
    ubounds = new int[undistRank];
    memcpy(ubounds, uboundsArg, undistRank * sizeof(int));
  }

  // if there are any dimensions 
  if (rank) {
    //// record coordRank
    coordRank = new int[rank];
    memcpy(coordRank, coordRankArg, rank * sizeof(int));
    
    //// record coordDimMap
    coordDimMap=_allocate2D<int>(rank,rank);
    for(int i=0; i<rank; i++)
      for(int j=0; j<rank; j++)
        coordDimMap[i][j]=coordDimMapArg[i][j];
    
    //// allocate coordinate array storage
    coordArrayList=_allocate2D<Array *>(staggerLocCount,rank);
    for(int i=0; i<staggerLocCount; i++)
      for(int j=0; j<rank; j++)
        coordArrayList[i][j]=ESMC_NULL_POINTER;

    //// allocate coordinate Lower Width storage
    staggerLWidthList=_allocate2D<int>(staggerLocCount,rank);

    //// allocate coordinate Upper Width storage
    staggerUWidthList=_allocate2D<int>(staggerLocCount,rank);

    //// allocate coordinate Alignment storage
    staggerAlignList=_allocate2D<int>(staggerLocCount,rank);

    //// set defaults for stagger alignment and stagger width
    for(int i=0; i<staggerLocCount; i++) {
      for(int j=0; j<rank; j++) {
        if (i & (0x1<<j)) {   // Set defaults based on the stagger location
          staggerUWidthList[i][j]=1;
          staggerLWidthList[i][j]=0;
          staggerAlignList[i][j]=-1;
        } else {
          staggerUWidthList[i][j]=0;
          staggerLWidthList[i][j]=0;
          staggerAlignList[i][j]=0;
        }
      }
    }

    //// allocate storage for array allocation flag
    didIAllocList=_allocate2D<bool>(staggerLocCount,rank);

    //// setup map from Grid dimensions to distgrid or ubounds/lbounds 
    //// dimensions 
    ////// allocate storage for mapping
    gridIsDist=new bool[rank];
    gridMapDim=new int[rank];

    ////// init to undistributed
    for (int i=0; i<rank; i++) {
      gridIsDist[i]=false;
    } 

    ////// fill in map to distributed dimensions
    for(int i=0; i<distRank; i++) {
      gridMapDim[dimmap[i]]=i;
      gridIsDist[dimmap[i]]=true;
    }

    ////// fill in map to undistributed dimensions
    int j=0;
    for(int i=0; i<rank; i++) {
      if (!gridIsDist[i]) {
        gridMapDim[i]=j;
        j++;
      }
    }

    //// setup map from coord dimensions to either distgrid
    //// dimensions or the coordinate Array's ubounds/lbounds dimensions
    ////// Allocate storage
    coordIsDist=_allocate2D<bool>(rank,rank);
    coordMapDim=_allocate2D<int>(rank,rank);

    ////// Fill in per coord
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
 
  // allocate and fill isDELBnd and isDEUbnd
  // These record if the local de is on the top or bottom
  // boundary in each dimension
  _createIsDEBnd(&isDELBnd,&isDEUBnd, distgrid, dimmap);

  // Set the name for this Grid object in the Base class
  ESMC_BaseSetName(nameArg, "Grid");

  // Grid is now ready to be used in grid methods, so set status appropriately
  status=ESMC_GRIDSTATUS_SHAPE_READY;

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
//   Removes a protogrid from a grid. The protogrid is used to record
//   values for the incremental grid creation paradigm 
//   (create empty/set/commit).
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
#define ESMC_METHOD "ESMCI::Grid::getCoordArrayInternal()"
//BOPI
// !IROUTINE:  Grid::getCoordArrayInternal
//
// !INTERFACE:
int Grid::getCoordArrayInternal(
//
// !RETURN VALUE:
//   return code
//
// !ARGUMENTS:
//
  int staggerlocArg, // (in)
  int coordArg,      // (in)
  Array **arrayArg   // (out)
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
  if ((staggerlocArg < 0) || (staggerlocArg >= staggerLocCount)) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- stagger location out of range", &rc);
    return rc;
  }

  // Check coord
  if ((coordArg < 0) || (coordArg >= rank)) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- coord out of range", &rc);
    return rc;
  }

  // get Array pointer from List
  array=coordArrayList[staggerlocArg][coordArg];

  // Check if array has been set
  if (array==ESMC_NULL_POINTER) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- accessing unset coord array", &rc);
    return rc;
  }

  // output array
  *arrayArg=array;

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
//    Because of the possible use of incremental create this just
//    sets default values, the real construction of the internal
//    grid structures is done in constructInternal.
//
//EOPI
//-----------------------------------------------------------------------------

  // Set default values for grid members
  proto = ESMC_NULL_POINTER; 
  
  status=ESMC_GRIDSTATUS_NOT_READY; // default status not ready
  
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
  staggerLWidthList = ESMC_NULL_POINTER;
  staggerUWidthList = ESMC_NULL_POINTER;
  
  gridIsDist = ESMC_NULL_POINTER;
  gridMapDim = ESMC_NULL_POINTER;
  
  coordIsDist = ESMC_NULL_POINTER;
  coordMapDim = ESMC_NULL_POINTER;
  
  isDELBnd = ESMC_NULL_POINTER;
  isDEUBnd = ESMC_NULL_POINTER;
  
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

   // If present delete ProtoGrid
   if (proto != ESMC_NULL_POINTER) delete proto;

   // delete distributed dimension stuff
   if (distRank) {
     delete [] dimmap;
   }

   // delete undistributed dimension stuff
   if (undistRank) {
     delete [] lbounds;
     delete [] ubounds;
   }

   // delete all dimension stuff
   if (rank) {
     delete [] coordRank;
     _free2D<int>(&coordDimMap);
     _free2D<Array *>(&coordArrayList);
     _free2D<int>(&staggerLWidthList);
     _free2D<int>(&staggerUWidthList);
     _free2D<int>(&staggerAlignList);
     _free2D<bool>(&didIAllocList);
     delete [] gridIsDist;
     delete [] gridMapDim;
     _free2D<bool>(&coordIsDist); 
     _free2D<int>(&coordMapDim); 
  }

   // delete local de bounds indicators
  if (isDELBnd != ESMC_NULL_POINTER) delete [] isDELBnd;
  if (isDEUBnd != ESMC_NULL_POINTER) delete [] isDEUBnd;
}




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
  if (dimmap != ESMC_NULL_POINTER) _freeInterfaceInt(&dimmap);
  if (lbounds != ESMC_NULL_POINTER) _freeInterfaceInt(&lbounds);
  if (ubounds != ESMC_NULL_POINTER) _freeInterfaceInt(&ubounds);
  if (coordRank != ESMC_NULL_POINTER) _freeInterfaceInt(&coordRank);
  if (coordDimMap != ESMC_NULL_POINTER) _freeInterfaceInt(&coordDimMap);
  if (indexflag != ESMC_NULL_POINTER) delete indexflag; 
  if (gridType != ESMC_NULL_POINTER) delete gridType; 
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::setCoordArrayInternal()"
//BOPI
// !IROUTINE:  Grid::setCoordArrayInternal
//
// !INTERFACE:
int Grid::setCoordArrayInternal(
//
// !RETURN VALUE:
//   return code
//
// !ARGUMENTS:
//
  int staggerlocArg, // (in)
  int coordArg,      // (in)
  Array *arrayArg,   // (in)
  bool didIAlloc   // (in)
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
  if ((staggerlocArg < 0) || (staggerlocArg >= staggerLocCount)) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- stagger location out of range", &rc);
    return rc;
  }

  // Check coord
  if ((coordArg < 0) || (coordArg >= rank)) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- coord out of range", &rc);
    return rc;
  }

  // Set array in list
  coordArrayList[staggerlocArg][coordArg] = arrayArg;

  // Set alloc
  didIAllocList[staggerlocArg][coordArg]=didIAlloc;


  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::setStaggerInfo()"
//BOPI
// !IROUTINE:  Grid::setStaggerInfo
//
// !INTERFACE:
int Grid::setStaggerInfo(
//
// !RETURN VALUE:
//   return code
//
// !ARGUMENTS:
//
  int staggerlocArg,     // (in)
  int *staggerAlignArg,  // (in)
  int *staggerLWidthArg, // (in)
  int *staggerUWidthArg  // (in)
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
  if ((staggerlocArg < 0) || (staggerlocArg >= staggerLocCount)) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- stagger location out of range", &rc);
    return rc;
  }

  // Set staggerAlign
  for (int i=0; i<rank; i++) {
    staggerAlignList[staggerlocArg][i]=staggerAlignArg[i];
   }

  // Set staggerLWidth
  for (int i=0; i<rank; i++) {
    staggerLWidthList[staggerlocArg][i]=staggerLWidthArg[i];
   }

  // Set staggerUWidth
  for (int i=0; i<rank; i++) {
    staggerUWidthList[staggerlocArg][i]=staggerUWidthArg[i];
   }

  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
//  File Local Routines
//
//-----------------------------------------------------------------------------
// Compute the total number of stagger locations from a Grid's rank
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


  // Make a copy of an interface int, allocating a new chunk of memory for its
  // internal array
  static InterfaceInt *_copyInterfaceInt(InterfaceInt *in) {

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

  // Deallocate an interfaceInt which was created with _copyInterfaceInt 
  static void _freeInterfaceInt(InterfaceInt **in) {

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

  // Create arrays (isDEUBnd and isDELBnd) which tell if a particular DE is on the edge of a tile.
  // If bit r of isDEUBnd is 1 then the DE is on the upper boundary in dimension r
  // If bit r of isDELBnd is 1 then the DE is on the lower boundary in dimension r
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


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::construct()"
//BOP
// !IROUTINE:  construct
//
// !INTERFACE:
int construct(
//
// !RETURN VALUE:
//   error code
//
// !ARGUMENTS:
//
  Grid *gridArg, 
  int nameLenArg,                           // (in)
  char *nameArg,                            // (in) optional
  ESMC_TypeKind *typekindArg,               // (in) optional
  DistGrid *distgridArg,                    // (in) 
  InterfaceInt *dimmapArg,                  // (in) optional
  InterfaceInt *lboundsArg,                 // (in) optional
  InterfaceInt *uboundsArg,                 // (in) optional
  InterfaceInt *coordRankArg,               // (in) optional
  InterfaceInt *coordDimMapArg,             // (in) optional
  ESMC_IndexFlag *indexflagArg,             // (in) optional
  int *gridTypeArg                          // (in) optional
  ){
//
// !DESCRIPTION:
//   Take an existing {\tt ESMCI_Grid} object and setup its internal structure
//   so that its usable in other Grid methods. Note that this routine
//   does error checking of input parameters and sets a default if an optional
//   parameter isn't passed in. (A non-present optionl paramters is passed with 
//   the value NULL).
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

  // initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  // To prevent erasing an existing grid, make sure grid is inactive
  if (gridArg->getStatus() != ESMC_GRIDSTATUS_NOT_READY) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
      "- grid must be status 'not ready' to be activated ", &rc);
    return rc;
  }  
  
  // Need a DistGrid to create a Grid, so error if not passed in
  if (distgridArg == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to distgrid argument", &rc);
    return rc;
  }

  // Convert F90 name string to C++ string 
  name = ESMC_F90toCstring(nameArg, nameLenArg);
  if (!name && nameLenArg){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
                                          "- Not a valid string", &rc);
    return rc;
  }

  // If typekind wasn't passed in then use default, otherwise copy passed in value
  if (typekindArg==NULL) {
    typekind=ESMC_TYPEKIND_R8;  // Default
  } else {
    typekind=*typekindArg;
  }

  // Get Rank of Distributed Dimensions
  distRank = distgridArg->getDimCount();


  // Process lboundsArg and uboundsArg
  // process these first to be able to calculate rank before dimmap processing

  // If uboundsArg paramter hasn't been passed in then the grid doesn't have undistributed dimensions 
  // (undistRank=0), if it has been then error check and copy it
  undistRank=0; // default to 0
  ubounds = NULL; // default to NULL
  if (uboundsArg != NULL){
    if (uboundsArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- ubounds array must be of rank 1", &rc);
      return rc;
    }
    if (uboundsArg->extent[0] < 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- ubounds array must at least be of size 1", &rc);
      return rc;
    }
    undistRank=uboundsArg->extent[0]; 
    ubounds = uboundsArg->array;
  }

  // If lboundsArg have been passed in, then copy it, unless ubounds isn't present in which
  // case there's an error (no ubounds -> no undist. dims in grid). If lbounds isn't present 
  // and ubounds is then set a default, otherwise error check and copy lboundsArg. 
  lbounds = NULL; // reset
  if (lboundsArg != NULL){
    if (uboundsArg==NULL){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- can't have ubounds without lbounds", &rc);
      return rc;
    }
    if (lboundsArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- lbounds array must be of rank 1", &rc);
      return rc;
    }
    if (lboundsArg->extent[0] != undistRank){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- lbounds, ubounds size mismatch", &rc);
      return rc;
    }
    // set lbounds from argument
    lbounds=new int[undistRank];
    for (int i=0; i<undistRank; i++)
         lbounds[i]=lboundsArg->array[i];
  } else if (uboundsArg != NULL) {
    // default lbounds to (1,1,1,...)
    lbounds=new int[undistRank];
    for (int i=0; i<undistRank; i++)
         lbounds[i]=1;  // default to a bottom of 1
  }


  // Compute grid rank (the sum of the distributed and undistributed ranks)
  rank=distRank+undistRank;

  // Grid must have positve rank
  if (rank<1) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- Grid must have rank >=1 ", &rc);
      return rc;
  }

  // If the dimmapArg parameter has been passed in then error check and copy it, otherwise
  // set a default.
  dimmap = new int[distRank];
  if (dimmapArg == NULL) {
    for (int i=0; i<distRank; i++)
      dimmap[i] = i; // set dimmap to default (0,1,2..)
  } else {
    if (dimmapArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- dimmap array must be of rank 1", &rc);
      return rc;
    }
    if (dimmapArg->extent[0] != distRank){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- dimmap and distgrid mismatch", &rc);
      return rc;
    }
    for (int i=0; i<distRank; i++){
      if (dimmapArg->array[i] < 1 || dimmapArg->array[i] > rank){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- dimmap / rank mismatch", &rc);
        return rc;
      }
      dimmap[i] = dimmapArg->array[i]-1;  // copy dimmap array element and make it zero based
    }
  } 

  // If the coordRankArg parameter has been passed in then error check and copy it, otherwise
  // set a default.
  coordRank=new int[rank];
  if (coordRankArg == NULL) {
    for (int i=0; i<rank; i++)
      coordRank[i] = rank; // set coordRank to default all curvilinear
  } else {
    if (coordRankArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- coordRank array must be of rank 1", &rc);
      return rc;
    }
    if (coordRankArg->extent[0] != rank){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- coordRank and distgrid (and perhaps ubounds) mismatch", &rc);
      return rc;
    }
    for (int i=0; i<rank; i++){
      if (coordRankArg->array[i] < 1 || coordRankArg->array[i] > rank){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- coordRank / rank mismatch", &rc);
        return rc;
      }
      // TODO: take this out when Array Factorization works
      if (coordRankArg->array[i] != rank){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_IMPL,
          "- Array and thus Grid don't currently support factorization", &rc);
        return rc;
      }
      coordRank[i] = coordRankArg->array[i];  // copy coordRank array element
    }
  } 
  
  // If the coordDimMapArg parameter has been passed in then error check and copy it, otherwise
  // set a default.
  coordDimMap=_allocate2D<int>(rank,rank);
  // initialize array to 0
  for(int i=0; i<rank; i++) {
    for (int j=0; j<rank; j++) {
      coordDimMap[i][j]=0;  
    }
  }

  if (coordDimMapArg == NULL) {
    for(int i=0; i<rank; i++) {
      for (int j=0; j<coordRank[i]; j++) {
        coordDimMap[i][j]=j;  // initialize to a default
      }
    }
  } else {
    if (coordRankArg == NULL){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- if coordDimMap is specified then a corresponding coordRank must also be specified", &rc);
      return rc;
    }
    if (coordDimMapArg->dimCount != 2){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- coordDimMap array must be of rank 2", &rc);
      return rc;
    }
    if ((coordDimMapArg->extent[0] != rank) || 
        (coordDimMapArg->extent[1] != rank)) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- coordDimMap and distgrid (and perhaps ubounds) mismatch", &rc);
      return rc;
    }
    for (int i=0; i<rank; i++){
      for (int j=0; j<coordRank[i]; j++) {
        // Note: order of i,j is because of F vs. C array ordering
        ind=j*rank+i;

        // Check to make sure data is correct
        if (coordDimMapArg->array[ind] < 1 || coordDimMapArg->array[ind] > rank){
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                              " - invalid coordDimMap value", &rc);
          return rc;
        }

        // copy coordDimMap array element
        coordDimMap[i][j] = coordDimMapArg->array[ind]-1; // switch to 0-based  
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
 
  // If indexflag wasn't passed in then use default, otherwise copy passed in value
  if (indexflagArg==NULL) {
    indexflag=ESMF_INDEX_DELOCAL;  // default
  } else {
    indexflag=*indexflagArg;
  }


  // If gridType wasn't passed in then use default, otherwise copy passed in value
  if (gridTypeArg==NULL) {
    gridType=0; // default
  } else {
    gridType=*gridTypeArg;
  }

  // construct the Grid object using the massaged parameter values
  localrc=gridArg->constructInternal(name, typekind, distgridArg, 
             distRank, dimmap, 
             undistRank, lbounds, ubounds,
             rank, coordRank, coordDimMap, 
             indexflag, gridType);
   if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
            ESMF_ERR_PASSTHRU, &rc)) return rc;
        
  // Dellocate temporay arrays
  if (uboundsArg != NULL)  delete [] lbounds;

  if (name) delete [] name;

  delete [] dimmap;

  delete [] coordRank;

  _free2D<int>(&coordDimMap);

  return ESMF_SUCCESS;
  }
//-----------------------------------------------------------------------------


} // END ESMCI name space
//-----------------------------------------------------------------------------










