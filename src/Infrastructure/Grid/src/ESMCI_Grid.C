// $Id: ESMCI_Grid.C,v 1.36.2.23 2009/01/21 21:25:21 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
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
static const char *const version = "$Id: ESMCI_Grid.C,v 1.36.2.23 2009/01/21 21:25:21 cdeluca Exp $";
//-----------------------------------------------------------------------------

#define VERBOSITY             (1)       // 0: off, 10: max


//-----------------------------------------------------------------------------


// Set up ESMCI name space for these methods
namespace ESMCI{  

//  File Local Prototypes (actual implementation at end of file)
static int _NumStaggerLocsFromDimCount(int dimCount);

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

static int _createIsDEBnd(char **_isDELBnd, char **_isDEUBnd, 
                          DistGrid *distgrid,int *distgridToGridMap);

int construct(Grid *_grid, int _nameLen, char *_name, ESMC_TypeKind *_typekind,
              DistGrid *_distgrid, InterfaceInt *_gridEdgeLWidth, 
              InterfaceInt *_gridEdgeUWidth, InterfaceInt *_gridAlign,
               InterfaceInt *_distgridToGridMap,
              InterfaceInt *_undistLBound, InterfaceInt *_undistUBound, 
              InterfaceInt *_coordDimCount, InterfaceInt *_coordDimMap,
              ESMC_IndexFlag *_indexflag, bool destroyDistgrid,
              bool destroyDELayout);

int setDefaultsLUA(int dimCount,
                   InterfaceInt *lWidthIn, InterfaceInt *uWidthIn, InterfaceInt *alignIn,
                   int *lWidthDefault, int *uWidthDefault, int *alignDefault, 
                   int *lWidthOut, int *uWidthOut, int *alignOut);


//-----------------------------------------------------------------------------
//
// Public Interfaces
//
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::addCoordArray()"
//BOP
// !IROUTINE:  addCoordArray
//
// !INTERFACE:
int Grid::addCoordArray(

//
// !RETURN VALUE:
//   Return code
//
// !ARGUMENTS:
//
                          int *staggerlocArg,             // (in) optional
                          InterfaceInt *staggerEdgeLWidthArg, // (in) optional
                          InterfaceInt *staggerEdgeUWidthArg, // (in) optional
                          InterfaceInt *staggerAlignArg,   // (in) optional 
                          InterfaceInt *staggerMemLBoundArg   // (in) optional 
  ) {
//
// !DESCRIPTION:
//   Create the storage (ESMF Array objects) to hold the coordinates for a
// stagger location. Note that this subroutine creates an Array for each 
// coordinate component in the stagger location. The Arrays are, of course,
// created with the correct size and shape to hold the coordinates. 
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int rc, localrc;
  int staggerloc;
  int coord;
  ESMC_DataCopy docopy;
  int *staggerAlign;
  int *staggerMemLBound;
  int *staggerEdgeLWidth;
  int *staggerEdgeUWidth;
  const int *distgridToArrayMap, *arrayUndistLBound, *arrayUndistUBound;
  const int *gridUndistLBound, *gridUndistUBound;
  Array *array;
  int extent[1];

  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  rc = ESMC_RC_NOT_IMPL;
  
  // Make sure the grid has the correct status for this action
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
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


  // Error check LWidth
  if (staggerEdgeLWidthArg != NULL) {
    //// Ensure staggerEdgeLWidth is of the correct dimCount 
    if (staggerEdgeLWidthArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
                    "- staggerEdgeLWidth array must be of dimCount 1", &rc);
      return rc;
    }
    //// Ensure staggerEdgeLWidth is of the correct size
    if (staggerEdgeLWidthArg->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
                  "- staggerEdgeLWidth size and Grid dimCount mismatch ", &rc);
      return rc;
    }
    //// Ensure staggerEdgeLWidthArg values fit within gridEdgeLWidth
    for (int i=0; i<dimCount; i++){
      if ((staggerEdgeLWidthArg->array[i] < 0) || (staggerEdgeLWidthArg->array[i] > gridEdgeLWidth[i])) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                 "- staggerAlign must be either -1, 0, or 1", &rc);
        return rc;
      }
    }
  }

  // Error check UWidth
  if (staggerEdgeUWidthArg != NULL) {
    //// Ensure staggerEdgeUWidth is of the correct dimCount 
    if (staggerEdgeUWidthArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
                    "- staggerEdgeUWidth array must be of dimCount 1", &rc);
      return rc;
    }
    //// Ensure staggerEdgeUWidth is of the correct size
    if (staggerEdgeUWidthArg->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
                  "- staggerEdgeUWidth size and Grid dimCount mismatch ", &rc);
      return rc;
    }
    //// Ensure staggerEdgeUWidthArg values fit within gridEdgeUWidth    
    for (int i=0; i<dimCount; i++){
      if ((staggerEdgeUWidthArg->array[i] < 0) || (staggerEdgeUWidthArg->array[i] > gridEdgeUWidth[i])) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                 "- staggerAlign must be either -1, 0, or 1", &rc);
        return rc;
      }
    }
  }

  // Error check Align
  if (staggerAlignArg != NULL) {
    //// Ensure staggerAlign has the correct dimCount
    if (staggerAlignArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- staggerAlign array must be of dimCount 1", &rc);
      return rc;
    }
    //// Ensure staggerAlign has the correct size
    if (staggerAlignArg->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- staggerAlign size and Grid dimCount mismatch ", &rc);
      return rc;
    }
    for (int i=0; i<dimCount; i++){
      //// Ensure staggerAlign values are -1,0,1
      if ((staggerAlignArg->array[i] < -1) || (staggerAlignArg->array[i] > 1)) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                 "- staggerAlign must be either -1, 0, or 1", &rc);
        return rc;
      }
    }
  }

  // Error check Align
  if (staggerMemLBoundArg != NULL) {
    //// Ensure staggerMemLBoundArg has the correct dimCount
    if (staggerMemLBoundArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- staggerMemLBound array must be of dimCount 1", &rc);
      return rc;
    }
    //// Ensure staggerAlign has the correct size
    if (staggerMemLBoundArg->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- staggerAlign size and Grid dimCount mismatch ", &rc);
      return rc;
    }
  }


  // Allocate lWidth, uWidth, and Align based on inputs and defaults
  staggerEdgeLWidth = new int[dimCount];
  staggerEdgeUWidth = new int[dimCount];
  staggerAlign = new int[dimCount];
  staggerMemLBound = new int[dimCount];


  // Set lWidth, uWidth, and Align based on inputs and defaults
  localrc=setDefaultsLUA(dimCount,
          staggerEdgeLWidthArg, staggerEdgeUWidthArg, staggerAlignArg,
          staggerEdgeLWidthList[staggerloc], staggerEdgeUWidthList[staggerloc], staggerAlignList[staggerloc], 
          staggerEdgeLWidth, staggerEdgeUWidth, staggerAlign);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
                           ESMF_ERR_PASSTHRU, &rc)) return rc;        

  // Set staggerMemLBound 
  if (staggerMemLBoundArg != NULL) {
    for (int i=0; i<dimCount; i++) {
      staggerMemLBound[i]=staggerMemLBoundArg->array[i];
    }
  } else {
    for (int i=0; i<dimCount; i++) {
      staggerMemLBound[i]=staggerMemLBoundList[staggerloc][i];
    }
  }
  

 // Rename Grid UndistLBound, UndistUBound to distinguish them from the Array versions 
  gridUndistLBound=undistLBound;
  gridUndistUBound=undistUBound;

  // construct ArraySpec for using to call Array::create
  ArraySpec *arrayspec= new ArraySpec();     
    
  // Construct empty InterfaceInt structures for using to call Array::create
  int *undistLBoundIntIntArray=new int[dimCount];
  extent[0]=dimCount;
  InterfaceInt *undistLBoundIntInt=new InterfaceInt(undistLBoundIntIntArray,1,extent);

  int *undistUBoundIntIntArray=new int[dimCount];
  extent[0]=dimCount;
  InterfaceInt *undistUBoundIntInt=new InterfaceInt(undistUBoundIntIntArray,1,extent); 

  int *distgridToArrayMapIntIntArray=new int[dimCount];
  extent[0]=dimCount;
  InterfaceInt *distgridToArrayMapIntInt=new InterfaceInt(distgridToArrayMapIntIntArray,1,extent); 

  int *compLWidthIntIntArray=new int[dimCount];
  extent[0]=dimCount;
  InterfaceInt *compLWidthIntInt=new InterfaceInt(compLWidthIntIntArray,1,extent); 

  int *compUWidthIntIntArray=new int[dimCount];
  extent[0]=dimCount;
  InterfaceInt *compUWidthIntInt=new InterfaceInt(compUWidthIntIntArray,1,extent); 

  InterfaceInt *staggerMemLBoundIntInt=(InterfaceInt *)ESMC_NULL_POINTER;
  int *staggerMemLBoundIntIntArray=(int *)ESMC_NULL_POINTER;

  // Only setup membounds if index flag is user
  if (indexflag==ESMF_INDEX_USER) {
    staggerMemLBoundIntIntArray=new int[dimCount];
    extent[0]=dimCount;
    staggerMemLBoundIntInt=new InterfaceInt(staggerMemLBoundIntIntArray,1,extent); 
  }

  ////////////
  ///// Loop Constructing all the coordinate arrays
  ///////////
  for (coord=0; coord<dimCount; coord++) {
    
    // fill in ArraySpec with information describing coordinate
    arrayspec->set(coordDimCount[coord], typekind);
   
    //// Initialize distgridToArrayMap array to 0 to make all unspecified dimensions
    //// replicated
    for (int i=0; i<dimCount; i++) {
      distgridToArrayMapIntIntArray[i]=0;
    }

    //// fill in distgridToArrayMap, undistLBound, and undistUBound for use in Array::create
    //// distgridToArrayMap - computed by inverting how coords dims map to distgrid
    //// bounds - computed by matching up an undistributed coord dimension
    ////          with the bound values at the corresponding grid dimension 
    int coordDistDimCount=0;
    int coordUndistDimCount=0;
    for (int i=0; i<coordDimCount[coord]; i++) {
      int gi=coordDimMap[coord][i];
      if (coordIsDist[coord][i]) {
        distgridToArrayMapIntIntArray[coordMapDim[coord][i]]=i+1; // convert to 1-based
        coordDistDimCount++;
      } else {
        undistLBoundIntIntArray[coordMapDim[coord][i]]=gridUndistLBound[gridMapDim[gi]];  
        undistUBoundIntIntArray[coordMapDim[coord][i]]=gridUndistUBound[gridMapDim[gi]];  
        coordUndistDimCount++;
      }
    }

    // Make sure there are distributed dimensions
    //    if (!coordDistDimCount) {
    // ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
    //             "- Coordinate must have a distributed portion", &rc);
    //  return rc;
    // }
    
    // set size of distgridToArrayMap 
    // (needs to be total dimCount of distGrid even if coord dimCount < distgrid dimCount)
    // (0's indicate unused dimensions)
    distgridToArrayMapIntInt->extent[0]=distDimCount;
    
    // set size of bounds (number of coord undistributed dimensions)
    if (coordUndistDimCount) {
      undistLBoundIntInt->extent[0]=coordUndistDimCount;
      undistUBoundIntInt->extent[0]=coordUndistDimCount;
    }
    
    // Init ComputationalLWidth
    for (int i=0; i<coordDistDimCount; i++) {
      compLWidthIntIntArray[i]=0; // init to 0.
    }

    // set size of computational lower bound 
    compLWidthIntInt->extent[0]=coordDimCount[coord];
    
    // init ComputationalUWidth to 0
    for (int i=0; i<coordDistDimCount; i++) {
      compUWidthIntIntArray[i]=0; // init to 0.
    }

    // set size of computational upper bound 
    compUWidthIntInt->extent[0]=coordDimCount[coord];;
    
    //// Expand the boundaries of the computational region of the Array 
    //// (distributed and undistributed) to hold the stagger padding
    int j=0;
    for (int i=0; i<coordDimCount[coord]; i++) {
      int gi=coordDimMap[coord][i];
      int offsetU=gridEdgeUWidth[gi]-staggerEdgeUWidth[gi];
      int offsetL=gridEdgeLWidth[gi]-staggerEdgeLWidth[gi];
      if (coordIsDist[coord][i]) {
        compUWidthIntIntArray[j] = -offsetU;
        compLWidthIntIntArray[j] = -offsetL;
        j++;
      } else {
        undistUBoundIntIntArray[coordMapDim[coord][i]] -= offsetU;
        undistLBoundIntIntArray[coordMapDim[coord][i]] += offsetL;
      }
    }

    /* DEBUG 
    printf("compUWidth=");
    for (int i=0; i<coordDistDimCount; i++) {
      printf("%d ", compUWidthIntIntArray[i]);
    }
    printf("\n");

    printf("compLWidth=");
    for (int i=0; i<coordDistDimCount; i++) {
      printf("%d ", compLWidthIntIntArray[i]);
    }
    printf("\n");
      DEBUG */

    //// Optionally fix the lower memory bounds of each DE's memory chunk
    if (indexflag==ESMF_INDEX_USER) {
      // Set size of array based on coord dim
      staggerMemLBoundIntInt->extent[0]=coordDimCount[coord];

      // Fill Array
      j=0;
      for (int i=0; i<coordDimCount[coord]; i++) {
	int gi=coordDimMap[coord][i];
	if (coordIsDist[coord][i]) {
	  staggerMemLBoundIntIntArray[j] = staggerMemLBound[gi];
	  j++;
	} 
      }
    }


    // Create an Array to hold the coords 
    if (coordUndistDimCount) { 
      // Pass in undistLBound, undistUBound if there are undistributed dims...
      array=Array::create(arrayspec, distgrid,
                          distgridToArrayMapIntInt,
                          compLWidthIntInt, compUWidthIntInt, 
                          (InterfaceInt *)ESMC_NULL_POINTER,
                          (InterfaceInt *)ESMC_NULL_POINTER,
                          (InterfaceInt *)ESMC_NULL_POINTER,
                          (InterfaceInt *)ESMC_NULL_POINTER,
                          &indexflag, staggerMemLBoundIntInt, 
                          &staggerloc, ESMC_NULL_POINTER, 
                          undistLBoundIntInt, undistUBoundIntInt, &localrc);
    } else {
      // ...otherwise pass in NUll
      array=Array::create(arrayspec, distgrid,
                          distgridToArrayMapIntInt,
                          compLWidthIntInt, compUWidthIntInt, 
                          (InterfaceInt *)ESMC_NULL_POINTER,
                          (InterfaceInt *)ESMC_NULL_POINTER,
                          (InterfaceInt *)ESMC_NULL_POINTER,
                          (InterfaceInt *)ESMC_NULL_POINTER,
                          &indexflag, staggerMemLBoundIntInt, 
                          &staggerloc, ESMC_NULL_POINTER,
                          (InterfaceInt *)ESMC_NULL_POINTER,
                          (InterfaceInt *)ESMC_NULL_POINTER, 
                          &localrc);
    }
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
                                  ESMF_ERR_PASSTHRU, &rc)) return rc;        

    // Set newly created Array into Grid
    localrc=this->setCoordArrayInternal(staggerloc, coord, array, true);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
                                 ESMF_ERR_PASSTHRU, &rc)) return rc;        
    
  } // end of coord loop


  // Set information about this stagger's coordinates into the Grid
  localrc=this->setStaggerInfo(staggerloc, staggerAlign, staggerEdgeLWidth, 
			       staggerEdgeUWidth,staggerMemLBound);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
            ESMF_ERR_PASSTHRU, &rc)) return rc;        


  // Dellocate temporay arrays
  delete arrayspec;     
  delete [] distgridToArrayMapIntIntArray;
  delete distgridToArrayMapIntInt;
  delete [] compLWidthIntIntArray;
  delete compLWidthIntInt;
  delete [] compUWidthIntIntArray;
  delete compUWidthIntInt;
  if (undistLBoundIntIntArray != ESMC_NULL_POINTER) delete [] undistLBoundIntIntArray;
  if (undistUBoundIntIntArray != ESMC_NULL_POINTER) delete [] undistUBoundIntIntArray;
  if (undistLBoundIntInt != ESMC_NULL_POINTER) delete undistLBoundIntInt;
  if (undistUBoundIntInt != ESMC_NULL_POINTER) delete undistUBoundIntInt;
  if (staggerEdgeUWidth != ESMC_NULL_POINTER) delete [] staggerEdgeUWidth;
  if (staggerEdgeLWidth != ESMC_NULL_POINTER) delete [] staggerEdgeLWidth;
  if (staggerAlign != ESMC_NULL_POINTER) delete [] staggerAlign;
  if (indexflag==ESMF_INDEX_USER) {
    delete [] staggerMemLBoundIntIntArray;
    delete staggerMemLBoundIntInt;
  }



  // return ESMF_SUCCESS
  return ESMF_SUCCESS;
  }
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::addCoordFromArrayList()"
//BOP
// !IROUTINE:  addCoordFromArrayList
//
// !INTERFACE:
int Grid::addCoordFromArrayList(

//
// !RETURN VALUE:
//   Return code
//
// !ARGUMENTS:
//
                        int *staggerlocArg,        // (in) optional
                        int arrayCount,             // (in) 
                        Array **arrayList,           // (in)
                        ESMC_DataCopy *docopyArg,   // (in) optional
                        InterfaceInt *staggerEdgeLWidthArg, // (in) optional
                        InterfaceInt *staggerEdgeUWidthArg, // (in) optional
                        InterfaceInt *staggerAlignArg   // (in) optional 
  ) {
//
// !DESCRIPTION:
//    Set {\tt arrayArg} as the coordinate Array for stagger location 
// {\tt staggerlocArg} and coordinate component {\tt coordArg}. Use either
// a copy or a direct reference depending on the value of {\tt docopyArg}.
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;
  int rc;
  int staggerloc;
  int coord;
  ESMC_DataCopy docopy;
  int *staggerAlign;
  int *staggerEdgeLWidth;
  int *staggerEdgeUWidth;
  const int *arrayDimMap, *arrayLBounds, *arrayUBounds;
  const int *gridLBounds, *gridUBounds;
  bool ok;  


   // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL; 
  rc = ESMC_RC_NOT_IMPL;
  
  // make sure grid is active
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
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

  // Make sure enough arrays have been passed in
  if (arrayCount != dimCount) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- Must pass in the same number of coord arrays as the dimCount of the Grid", &rc);
      return rc;
  }


  // Error check LWidth
  if (staggerEdgeLWidthArg != NULL) {
    //// Ensure staggerEdgeLWidth is of the correct dimCount 
    if (staggerEdgeLWidthArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
                    "- staggerEdgeLWidth array must be of rank 1", &rc);
      return rc;
    }
    //// Ensure staggerEdgeLWidth is of the correct size
    if (staggerEdgeLWidthArg->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
                  "- staggerEdgeLWidth size and Grid dimCount mismatch ", &rc);
      return rc;
    }
    //// Ensure staggerEdgeLWidthArg values fit within gridEdgeLWidth
    for (int i=0; i<dimCount; i++){
      if ((staggerEdgeLWidthArg->array[i] < 0) || (staggerEdgeLWidthArg->array[i] > gridEdgeLWidth[i])) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                 "- staggerEdgeAlign must be either -1, 0, or 1", &rc);
        return rc;
      }
    }
  }

  // Error check UWidth
  if (staggerEdgeUWidthArg != NULL) {
    //// Ensure staggerEdgeUWidth is of the correct dimCount 
    if (staggerEdgeUWidthArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
                    "- staggerEdgeUWidth array must be of dimCount 1", &rc);
      return rc;
    }
    //// Ensure staggerEdgeUWidth is of the correct size
    if (staggerEdgeUWidthArg->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
                  "- staggerEdgeUWidth size and Grid rank mismatch ", &rc);
      return rc;
    }
    //// Ensure staggerEdgeUWidthArg values fit within gridEdgeUWidth    
    for (int i=0; i<dimCount; i++){
      if ((staggerEdgeUWidthArg->array[i] < 0) || (staggerEdgeUWidthArg->array[i] > gridEdgeUWidth[i])) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                 "- staggerEdgeAlign must be either -1, 0, or 1", &rc);
        return rc;
      }
    }
  }

  // Error check Align
  if (staggerAlignArg != NULL) {
    //// Ensure staggerEdgeAlign has the correct dimCount
    if (staggerAlignArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- staggerAlign array must be of rank 1", &rc);
      return rc;
    }
    //// Ensure staggerAlign has the correct size
    if (staggerAlignArg->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- staggerAlign size and Grid rank mismatch ", &rc);
      return rc;
    }
    for (int i=0; i<dimCount; i++){
      //// Ensure staggerAlign values are -1,0,1
      if ((staggerAlignArg->array[i] < -1) || (staggerAlignArg->array[i] > 1)) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                 "- staggerAlign must be either -1, 0, or 1", &rc);
        return rc;
      }
    }
  }

  // Allocate lWidth, uWidth, and Align based on inputs and defaults
  staggerEdgeLWidth = new int[dimCount];
  staggerEdgeUWidth = new int[dimCount];
  staggerAlign = new int[dimCount];

  // Set lWidth, uWidth, and Align based on inputs and defaults
  localrc=setDefaultsLUA(dimCount,
          staggerEdgeLWidthArg, staggerEdgeUWidthArg, staggerAlignArg,
          staggerEdgeLWidthList[staggerloc], staggerEdgeUWidthList[staggerloc], staggerAlignList[staggerloc], 
          staggerEdgeLWidth, staggerEdgeUWidth, staggerAlign);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
                           ESMF_ERR_PASSTHRU, &rc)) return rc;        


  // Loop through all the coords
  for (int coord=0; coord<dimCount; coord++) {
    
    // Get an array to work on
    Array *arrayArg=arrayList[coord];
    
    // Make sure arrayArg is a valid pointer
    if (arrayArg == NULL){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
                                            "- Not a valid pointer to array argument", &rc);
      return rc;
    }
    
    
    // Ensure the passed in array has the correct dimCount
    if (coordDimCount[coord] != arrayArg->getRank()){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
                                            "- Array and Grid coord rank mismatch ", &rc);
      return rc;
    }
    
    // Ensure the passed in array has the correct typekind
    if (typekind != arrayArg->getTypekind()){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
                                            "- Array and Grid TypeKind mismatch ", &rc);
      return rc;
    }
    
    // Ensure the passed in array has the correct distgrid
    if (distgrid != arrayArg->getDistGrid()){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
                                            "- Array and Grid DistGrid mismatch ", &rc);
      return rc;
    }
    
    // Ensure the passed in array has the correct indexflag
    if (indexflag != arrayArg->getIndexflag()){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
                                            "- Array and Grid Indexflag mismatch ", &rc);
      return rc;
    }
    
    
    // Check that the passed in Array's dimmap is consistant with this coord's
    int tmpDimMap[ESMF_MAXDIM];
    
    //// Init coordDimMap (loop over entire distGrid dimCount)
    for (int i=0; i<distDimCount; i++) {
      tmpDimMap[i]=0;
    }
    
    //// Construct coordinate analog to array dimmap (will be 1-based)
    for (int i=0; i<coordDimCount[coord]; i++) {
      if (coordIsDist[coord][i]) {
        tmpDimMap[coordMapDim[coord][i]]=i+1; // convert to 1-based
      }
    }
    
    //// get the Array's dimmap 
    arrayDimMap=arrayArg->getDistGridToArrayMap();
    
    //// Check if the Array's dimmap matches what we expect for the coord
    ok=true;
    for (int i=0; i<distDimCount; i++) {
      if (tmpDimMap[i] != arrayDimMap[i]) ok=false;
    }
    if (!ok) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
                                            "- Array and Grid Coord dimmap mismatch ", &rc);
      return rc;
    }
    
    // Check and make sure the array's computational bounds are
    // big enough for the stagger padding
    arrayLBounds=arrayArg->getUndistLBound();
    arrayUBounds=arrayArg->getUndistUBound();
    gridLBounds=undistLBound;
    gridUBounds=undistUBound;
    int offset[ESMF_MAXDIM];
    int staggerLBnd[ESMF_MAXDIM];
    int staggerUBnd[ESMF_MAXDIM];
    int compLBnd[ESMF_MAXDIM];
    int compUBnd[ESMF_MAXDIM];
    int localDECount=distgrid->getDELayout()->getLocalDeCount();
    for (int lDE=0; lDE < localDECount; lDE++) {
      
      //// Calculate the stagger lower bounds from the array
      ////// Get exclusive bounds (since the grid and array have
      ////// the same distgrid their exclusive bounds are the same)
      const int *exLBnd=arrayArg->getExclusiveLBound()+lDE*distDimCount;
      
      ////// Get stagger Lbnd offset
      for (int i=0; i<dimCount; i++) {
        if (isDELBnd[lDE] & (0x1 << i)) {
          offset[i]=gridEdgeLWidth[i]-staggerEdgeLWidth[i];
        } else {
          offset[i]=0;
        }
      }

      
      ////// Fill in the staggerLBnd array
      ////// distributed bounds are exclusive bounds minus stagger padding
      ////// undistributed bounds are undistLBound minus stagger padding
      for (int i=0; i<coordDimCount[coord]; i++) {
        int gi=coordDimMap[coord][i];
        if (coordIsDist[coord][i]) {
          staggerLBnd[i]=exLBnd[coordMapDim[coord][i]]+offset[gi];
        } else {
          staggerLBnd[i]=gridLBounds[gridMapDim[gi]]+offset[gi];
        }
      }
      
      //// get computationalLBound from the array
      ///// get the computational bounds of the localDE
      const int *arrayCompLBnd=arrayArg->getComputationalLBound()+lDE*distDimCount;
      
      ///// Fill in the compLBnd array
      for (int i=0; i<coordDimCount[coord]; i++) {
        if (coordIsDist[coord][i]) {
          compLBnd[i]=arrayCompLBnd[coordMapDim[coord][i]];
        } else {
          compLBnd[i]=arrayLBounds[coordMapDim[coord][i]];
        }
      }

      //// Make sure the grid staggerLbounds fit within the array computational L bounds
      for (int i=0; i<coordDimCount[coord]; i++) {
        if (compLBnd[i] != staggerLBnd[i]) {
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
                                                "- Array computationalLBound or LBounds insufficient to hold grid+stagger bounds ", &rc);
          return rc;
          
        }
      }
      
      
      //// Calculate the stagger upper bounds from the array
      ////// Get array exclusive bounds (since the grid and array have
      ////// the same distgrid their exclusive bounds are the same)
      const int *exUBnd=arrayArg->getExclusiveUBound()+lDE*distDimCount;
      
      ////// Get stagger Ubnd offset
      for (int i=0; i<dimCount; i++) {
        if (isDEUBnd[lDE] & (0x1 << i)) {
          offset[i]=gridEdgeUWidth[i]-staggerEdgeUWidth[i];
        } else {
          offset[i]=0;
        }
      }

      ////// Fill in the staggerLBnd array
      ////// distributed bounds are exclusive bounds plus stagger padding
      ////// undistributed bounds are ubounds plus stagger padding
      for (int i=0; i<coordDimCount[coord]; i++) {
      int gi=coordDimMap[coord][i];
      if (coordIsDist[coord][i]) {
        staggerUBnd[i]=exUBnd[coordMapDim[coord][i]]-offset[gi];
      } else {
        staggerUBnd[i]=gridUBounds[gridMapDim[gi]]-offset[gi];
      }
      }
      
      //// get computationalUBound from the array
      ///// get the computational bounds of the localDE
      const int *arrayCompUBnd=arrayArg->getComputationalUBound()+lDE*distDimCount;
      
      ///// Fill in the compLBnd array
      for (int i=0; i<coordDimCount[coord]; i++) {
        if (coordIsDist[coord][i]) {
          compUBnd[i]=arrayCompUBnd[coordMapDim[coord][i]];
        } else {
          compUBnd[i]=arrayUBounds[coordMapDim[coord][i]];
        }
      }
      
      //// Make sure the grid's stagger upper bounds fit within the array computational upper bounds
      for (int i=0; i<coordDimCount[coord]; i++) {
        if (compUBnd[i] != staggerUBnd[i]) {
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
                                                "- Array computationalUBound or UBounds insufficient to hold grid+stagger bounds ", &rc);
          return rc;
          
        }
      }
    }
    
    // If we've reached this point then arrayArg is of the right size and shape
    // to hold the coordinates in coord, so put it in. 
    localrc=this->setCoordArrayInternal(staggerloc, coord, arrayArg, false);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
                                              &rc)) return rc; 
  }
  

  // Set information about this stagger's coordinates into the Grid
  localrc=this->setStaggerInfo(staggerloc, staggerAlign, staggerEdgeLWidth, 
			       staggerEdgeUWidth, 
			       staggerMemLBoundList[staggerloc]); // TODO: this is just using existing
                                                                  // MemLBound need to change this
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
            ESMF_ERR_PASSTHRU, &rc)) return rc;        


  // deallocate temporary arrays
  if (staggerEdgeUWidth != ESMC_NULL_POINTER) delete [] staggerEdgeUWidth;
  if (staggerEdgeLWidth != ESMC_NULL_POINTER) delete [] staggerEdgeLWidth;
  if (staggerAlign != ESMC_NULL_POINTER) delete [] staggerAlign;

  // return Success
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
 ){
//
// !DESCRIPTION:
//   This call is the final step of the create empty/set/commit incremental
// method for creating a Grid. The \gridArg parameter passed in here should
// have been created with create(rc), then filled with information using set 
// set().
// After this call the grid object will be usable in other methods, but may
// no longer be the subject of set().
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int rc;
  int localrc;                 // local error status

  // initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  // Make sure that we're the correct status
  if (status != ESMC_GRIDSTATUS_NOT_READY) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_BAD,
      "- Can't use commit on an already created object", &rc);
    return rc;
  }

 // Get the protoGrid which holds the information from set()
  if (proto == ESMC_NULL_POINTER) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_BAD,
      "- Null protoGrid ", &rc);
    return rc;
  }

  // setup the grid's internal structures 
  localrc=construct(this, proto->nameLen, proto->name, proto->typekind, 
                       proto->distgrid, 
                       proto->gridEdgeLWidth, proto->gridEdgeUWidth,
                       proto->gridAlign,
                       proto->distgridToGridMap, 
                       proto->undistLBound, proto->undistUBound, 
                       proto->coordDimCount, proto->coordDimMap,
                       proto->gridMemLBound,
		       proto->indexflag,
                       proto->destroyDistgrid, proto->destroyDELayout);
   if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
            ESMF_ERR_PASSTHRU, &rc)) return rc;        

   // Now that we don't need it anymore, remove the protogrid from the grid
   localrc=this->delProtoGrid();
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
  InterfaceInt *gridEdgeLWidthArg,           // (in) optional
  InterfaceInt *gridEdgeUWidthArg,           // (in) optional
  InterfaceInt *gridAlignArg,                // (in) optional
  InterfaceInt *distgridToGridMapArg,                  // (in) optional
  InterfaceInt *undistLBoundArg,                 // (in) optional
  InterfaceInt *undistUBoundArg,                 // (in) optional
  InterfaceInt *coordDimCountArg,               // (in) optional
  InterfaceInt *coordDimMapArg,             // (in) optional
  InterfaceInt *gridMemLBoundArg,          // (in) optional
  ESMC_IndexFlag *indexflagArg,             // (in) optional
  bool *destroyDistgridArg,
  bool *destroyDELayoutArg,
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
  localrc=construct(grid, nameLenArg, nameArg, typekindArg, distgridArg, 
                    gridEdgeLWidthArg,gridEdgeUWidthArg, gridAlignArg,
                    distgridToGridMapArg, undistLBoundArg, undistUBoundArg, 
                    coordDimCountArg, coordDimMapArg, gridMemLBoundArg, indexflagArg,
                    destroyDistgridArg, destroyDELayoutArg);
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
// !IROUTINE:  create empty
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
//    Create an empty {\tt ESMC\_Grid} object. The returned Grid object may
// only be used with set and commit. To make this object usable employ
// {\tt set} to fill the object with parameters and then {\tt commit} to
// construct a usable Grid based on those paramters. 
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                 // local error status

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
// Deallocate a Grid's internal memory and then deallocate the Grid object 
// itself. The grid parameter is set to ESMC_NULL_POINTER. 
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int rc;                // automatic variable for local return code

  // initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  // return with error for NULL pointer
  if (gridArg == ESMC_NULL_POINTER || *gridArg == ESMC_NULL_POINTER){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to Grid",&rc);
    return rc;
  }

  // destruct and delete Grid object
  try{
     delete *gridArg;
  }catch(...){
     // TODO: Change to get rid of return codes
     // deallocation error
     ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_WRONG,
      "- Error occurred in ~Grid ",&rc);
    return rc;
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
Array *Grid::getCoordArray(

//
// !RETURN VALUE:
//   The requested array
//
// !ARGUMENTS:
//
                        int *staggerlocArg,        // (in) optional
                        int coordArg,              // (in) base-1
                        ESMC_DataCopy *docopyArg,  // (in) optional
                        int *rcArg                 // (out) optional return code 
  ) {
//
// !DESCRIPTION:
//    Get the Array object holding the coordinates values for stagger location
//    {\tt staggerloc} and coordinate component {\tt coord} (coord is 1 based).
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                 // local error status
  int staggerloc;
  int coord;
  ESMC_DataCopy docopy;
  int dimCount;
  Array *array;

  // initialize return code; assume routine not implemented
  if (rcArg != ESMC_NULL_POINTER) *rcArg = ESMC_RC_NOT_IMPL;

  // make sure grid is the correct status for this action
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
      "- grid not of correct status to perform this operation", rcArg);

    return ESMC_NULL_POINTER;
  }  
  
  // If staggerlocArg wasn't passed in, use default, else copy the value.
  if (staggerlocArg==NULL) {
    staggerloc=0;  // default center
  } else {
    staggerloc=*staggerlocArg;
  }

  // translate coord to C++ base-0.
  coord=coordArg-1; // translate from F90 1 based to 0 based

  // TODO: make sure coord is within 0 to dimCount-1


  // If docopyArg wasn't passed in, use default, else copy the value
  if (docopyArg==NULL) {
    docopy=ESMC_DATA_REF;  // default
  } else {
    docopy=*docopyArg;
  }

  // Copy option isn't working for now
  if (docopy==ESMC_DATA_COPY) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_IMPL,
        "- Data Copy Flag not implemented yet", rcArg);
      return ESMC_NULL_POINTER;
  }

  // Get Coord Array
  localrc=this->getCoordArrayInternal(staggerloc, coord, &array);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
            ESMF_ERR_PASSTHRU, rcArg)) return ESMC_NULL_POINTER;        

  // return SUCCESS
  if (rcArg != ESMC_NULL_POINTER) *rcArg = ESMF_SUCCESS;  

   // return what getCoordArray returned
  return array;
  }
//-----------------------------------------------------------------------------

//// NEED TO THINK ABOUT THE FOLLOWING VVVVV
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::getDistExclusiveLBound()"
//BOPI
// !IROUTINE:  Grid::getDistExclusiveLBound()"
//
// !INTERFACE:
int Grid::getDistExclusiveLBound(
//
// !RETURN VALUE:
//   return code
//
// !ARGUMENTS:
//
  int localDEArg,     // (in)
  int *lBndArg      // (out) needs to be of size > distDimCount
  ){
//
// !DESCRIPTION:
//  The exclusive lower bound for this localde.
// TODO: eventually this should return all the grid bounds, not just
//       the distributed ones.
//
//EOPI
//-----------------------------------------------------------------------------
  int rc,localrc;

  // initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  
  // Check status
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Grid not fully created", &rc);
    return rc;
  }


  // Ensure localDEArg isn't out of range for this PET
  if ((localDEArg < 0) || (localDEArg >=distgrid->getDELayout()->getLocalDeCount())) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
          "- localDE outside range on this processor", &rc);
        return rc;
  }

  // Set lower bound based on indexflag
  if ((indexflag==ESMF_INDEX_DELOCAL) || (indexflag==ESMF_INDEX_USER)) {
    for (int i=0; i<distDimCount; i++)
      lBndArg[i] = 1; // excl. region starts at (1,1,1...) 
  } else {
    // Get some useful information
    const int *localDeList = distgrid->getDELayout()->getLocalDeList();

    // Get the Global DE from the local DE
    int de = localDeList[localDEArg];

    // Set Bound based on distgrid info
    for (int i=0; i<distDimCount; i++){
        
      // obtain indexList for this DE and dim
      const int *indexList =
        distgrid->getIndexListPDimPLocalDe(localDEArg, i+1, &localrc);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,ESMF_ERR_PASSTHRU, &rc))
        return rc;
      
      // make sure this dimension is contiguous         
      const int contig=distgrid->getContigFlagPDimPDe(de, i+1, &localrc);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
                                                ESMF_ERR_PASSTHRU, &rc)) return rc;
      if (!contig) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_IMPL,
                     "- doesn't handle non-contiguous DEs yet ", &rc);
        return rc;
      }
      
      // Set lower bounds of exclusive region to match indexList[0]
      lBndArg[i] = indexList[0];
    } // i
  }
  
  // tell the calling subroutine that we've had a successful outcome
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::getDistExclusiveUBound()"
//BOPI
// !IROUTINE:  Grid::getDistExclusiveUBound()"
//
// !INTERFACE:
int Grid::getDistExclusiveUBound(
//
// !RETURN VALUE:
//   return code
//
// !ARGUMENTS:
//
  int localDEArg,     // (in)
  int *uBndArg      // (out) needs to be of size > distDimCount
  ){
//
// !DESCRIPTION:
//  The exclusive upper bound for this localde
// TODO: eventually this should return all the grid bounds, not just
//       the distributed ones.
//
//EOPI
//-----------------------------------------------------------------------------
  int rc,localrc;

  // initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  
  // Check status
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Grid not fully created", &rc);
    return rc;
  }


  // Ensure localDEArg isn't out of range for this PET
  if ((localDEArg < 0) || (localDEArg >=distgrid->getDELayout()->getLocalDeCount())) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
          "- localDE outside range on this processor", &rc);
        return rc;
  }

  // Get some useful information
  const int *localDeList = distgrid->getDELayout()->getLocalDeList();
  const int *indexCountPDimPDe = distgrid->getIndexCountPDimPDe();

  // Get the Global DE from the local DE
  int de = localDeList[localDEArg];

  // exlc. region for each DE ends at indexCountPDimPDe of the associated
  // DistGrid
    for (int i=0; i<distDimCount; i++)
      uBndArg[i]=indexCountPDimPDe[de*distDimCount+i];

  // Set upper bound based on indexflag
  if (indexflag==ESMF_INDEX_GLOBAL) {

      for (int i=0; i<distDimCount; i++){

        // obtain indexList for this DE and dim
        const int *indexList =
          distgrid->getIndexListPDimPLocalDe(localDEArg, i+1, &localrc);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,ESMF_ERR_PASSTHRU, &rc))
          return rc;

        // make sure is contiguous         
        const int contig=distgrid->getContigFlagPDimPDe(de, i+1, &localrc);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
                              ESMF_ERR_PASSTHRU, &rc)) return rc;
        if (!contig) {
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_IMPL,
                     "- doesn't handle non-contiguous DEs yet ", &rc);
          return rc;
        }

        // shift bounds of exclusive region to match indexList[0]
        uBndArg[i] += indexList[0] - 1;
      } // i
  }

  // tell the calling subroutine that we've had a successful outcome
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::getLDEStaggerLOffset()"
//BOPI
// !IROUTINE:  Grid::getLDEStaggerLOffset()"
//
// !INTERFACE:
int Grid::getLDEStaggerLOffset(
//
// !RETURN VALUE:
//   return code
//
// !ARGUMENTS:
//
  int staggerlocArg,  // (in)
  int localDEArg,     // (in)
  int *LWidthArg      // (out) needs to be of the same size as the grid dimCount
  ){
//
// !DESCRIPTION:
//   Returns the amount the lower end of the exclusive region of
//   this local DE should be shifted upward
//   to match the lower side of the computational region for this stagger location.
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

  // Loop through grid dimCount dimensions adding the staggerLwidth
  // if we're on a lower boundary 
  for (int i=0; i<dimCount; i++) {
    if (isDELBnd[localDEArg] & (0x1 << i)) {
      LWidthArg[i]=gridEdgeLWidth[i]-staggerEdgeLWidthList[staggerlocArg][i];
    } else {
      LWidthArg[i]=0;
    }
  }

  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::getLDEStaggerUOffset()"
//BOPI
// !IROUTINE:  Grid::getLDEStaggerUOffset()"
//
// !INTERFACE:
int Grid::getLDEStaggerUOffset(
//
// !RETURN VALUE:
//   return code
//
// !ARGUMENTS:
//
  int staggerlocArg, // (in)
  int localDEArg,    // (in)
  int *UWidthArg     // (out) needs to be of the same size as the grid dimCount
  ){
//
// !DESCRIPTION:
//   Returns the amount the Upper end of the exclusive region of
//   this local DE should be shifted downward
//   to get to the upper side of the computational region for this stagger location.
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

  // Loop through grid dimCount dimensions setting the staggerEdgeUWidth
  // for dimensions for which this local de is on the upper boundary
  for (int i=0; i<dimCount; i++) {
    if (isDEUBnd[localDEArg] & (0x1 << i)) {
      UWidthArg[i]=gridEdgeUWidth[i]-staggerEdgeUWidthList[staggerlocArg][i];
    } else {
      UWidthArg[i]=0;
    }
  }

  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------

//// NEED TO THINK ABOUT THE PROCEEDING ^^^^^


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
  InterfaceInt *gridEdgeLWidthArg,  // (in) optional
  InterfaceInt *gridEdgeUWidthArg,  // (in) optional
  InterfaceInt *gridAlignArg,       // (in) optional
  InterfaceInt *distgridToGridMapArg,       // (in) optional
  InterfaceInt *undistLBoundArg,      // (in) optional
  InterfaceInt *undistUBoundArg,      // (in) optional
  InterfaceInt *coordDimCountArg,    // (in) optional
  InterfaceInt *coordDimMapArg,  // (in) optional
  InterfaceInt *gridMemLBoundArg,          // (in)
  ESMC_IndexFlag *indexflagArg,   // (in) optional
  bool *destroyDistgridArg,
  bool *destroyDELayoutArg
  ){
//
// !DESCRIPTION:
//   As the second part of the create empty/set/commit incremental grid 
//  creation paradigm, this subroutine is used to set values in a Grid in
//  preperation for  a later commit. This method may be called multiple times 
//  to set different sets of parameters. If the same parameter is set twice,
//  the second value overwrites the first. 
//   
//   TODO: eventually seperate this into a bunch of seperate sets to allow 
//         easier access from C.
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int rc;
  int localrc;                 // local error status

  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  rc = ESMC_RC_NOT_IMPL;

  // Make sure that we haven't been created
  if (status != ESMC_GRIDSTATUS_NOT_READY) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_WRONG,
      "- Can't use set on an already created object", &rc);
    return rc;
  }
  
  // Make sure the protoGrid exists
  if (proto == ESMC_NULL_POINTER) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_BAD,
      "- Null protoGrid ", &rc);
    return rc;
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

  // if passed in, set gridEdgeLWidth
  if (gridEdgeLWidthArg != ESMC_NULL_POINTER) { 
    // if present get rid of the old data
    if (proto->gridEdgeLWidth !=ESMC_NULL_POINTER) _freeInterfaceInt(&proto->gridEdgeLWidth);

    // record the new data
    proto->gridEdgeLWidth=_copyInterfaceInt(gridEdgeLWidthArg);
  }

  // if passed in, set gridEdgeUWidth
  if (gridEdgeUWidthArg != ESMC_NULL_POINTER) { 
    // if present get rid of the old data
    if (proto->gridEdgeUWidth !=ESMC_NULL_POINTER) _freeInterfaceInt(&proto->gridEdgeUWidth);

    // record the new data
    proto->gridEdgeUWidth=_copyInterfaceInt(gridEdgeUWidthArg);
  }

  // if passed in, set gridAlign
  if (gridAlignArg != ESMC_NULL_POINTER) { 
    // if present get rid of the old data
    if (proto->gridAlign !=ESMC_NULL_POINTER) _freeInterfaceInt(&proto->gridAlign);

    // record the new data
    proto->gridAlign=_copyInterfaceInt(gridAlignArg);
  }


  // if passed in, set distgridToGridMap
  if (distgridToGridMapArg != ESMC_NULL_POINTER) { 
    // if present get rid of the old data
    if (proto->distgridToGridMap !=ESMC_NULL_POINTER) _freeInterfaceInt(&proto->distgridToGridMap);

    // record the new data
    proto->distgridToGridMap=_copyInterfaceInt(distgridToGridMapArg);
  }

  // if passed in, set undistLBound
  if (undistLBoundArg != ESMC_NULL_POINTER) { 
    // if present get rid of the old data
    if (proto->undistLBound !=ESMC_NULL_POINTER) _freeInterfaceInt(&proto->undistLBound);

    // record the new data
    proto->undistLBound=_copyInterfaceInt(undistLBoundArg);
  }

  // if passed in, set undistUBound
  if (undistUBoundArg != ESMC_NULL_POINTER) { 
    // if present get rid of the old data
    if (proto->undistUBound !=ESMC_NULL_POINTER) _freeInterfaceInt(&proto->undistUBound);

    // record the new data
    proto->undistUBound=_copyInterfaceInt(undistUBoundArg);
  }

  // if passed in, set coordDimCount
  if (coordDimCountArg != ESMC_NULL_POINTER) { 
    // if present get rid of the old data
    if (proto->coordDimCount !=ESMC_NULL_POINTER) _freeInterfaceInt(&proto->coordDimCount);

    // record the new data
    proto->coordDimCount=_copyInterfaceInt(coordDimCountArg);
  }

  // if passed in, set coordDimMap
  if (coordDimMapArg != ESMC_NULL_POINTER) { 
    // if present get rid of the old data
    if (proto->coordDimMap !=ESMC_NULL_POINTER) _freeInterfaceInt(&proto->coordDimMap);

    // record the new data
    proto->coordDimMap=_copyInterfaceInt(coordDimMapArg);
  }

  // if passed in, set gridMemLBoundArg
  if (gridMemLBoundArg != ESMC_NULL_POINTER) { 
    // if present get rid of the old data
    if (proto->gridMemLBound !=ESMC_NULL_POINTER) _freeInterfaceInt(&proto->gridMemLBound);

    // record the new data
    proto->gridMemLBound=_copyInterfaceInt(gridMemLBoundArg);
  }

  // if passed in, set indexflag
  if (indexflagArg!=NULL) {
    if (proto->indexflag == ESMC_NULL_POINTER) proto->indexflag= new ESMC_IndexFlag;
    *(proto->indexflag)=*indexflagArg;
  }
 
  // if passed in, set destroyDistgrid
  if (destroyDistgridArg!=NULL) {
    if (proto->destroyDistgrid == ESMC_NULL_POINTER) proto->destroyDistgrid= new bool;
    *(proto->destroyDistgrid)=*destroyDistgridArg;
  }

  // if passed in, set destroyDistgrid
  if (destroyDELayoutArg!=NULL) {
    if (proto->destroyDELayout == ESMC_NULL_POINTER) proto->destroyDELayout= new bool;
    *(proto->destroyDELayout)=*destroyDELayoutArg;
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
                        int *staggerlocArg,        // (in) optional
                        int *coordArg,             // (in) 
                        Array *arrayArg,           // (in)
                        ESMC_DataCopy *docopyArg   // (in) optional
  ) {
//
// !DESCRIPTION:
//    Set {\tt arrayArg} as the coordinate Array for stagger location 
// {\tt staggerlocArg} and coordinate component {\tt coordArg}. Use either
// a copy or a direct reference depending on the value of {\tt docopyArg}.
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;
  int rc;
  int staggerloc;
  int coord;
  ESMC_DataCopy docopy;
  const int *distgridToArrayMap, *arrayUndistLBound, *arrayUndistUBound;
  const int *gridUndistLBound, *gridUndistUBound;
  bool ok;  

   // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL; 
  rc = ESMC_RC_NOT_IMPL;
  
  // make sure grid is active
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
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

 
  // Ensure the passed in array has the correct dimCount
  if (coordDimCount[coord] != arrayArg->getRank()){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- Array and Grid coord dimCount mismatch ", &rc);
      return rc;
    }

  // Ensure the passed in array has the correct typekind
  if (typekind != arrayArg->getTypekind()){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- Array and Grid TypeKind mismatch ", &rc);
      return rc;
    }

  // Ensure the passed in array has the correct distgrid
  if (distgrid != arrayArg->getDistGrid()){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- Array and Grid DistGrid mismatch ", &rc);
      return rc;
    }

  // Ensure the passed in array has the correct indexflag
  if (indexflag != arrayArg->getIndexflag()){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- Array and Grid Indexflag mismatch ", &rc);
      return rc;
    }


  // Check that the passed in Array's dimmap is consistant with this coord's
  int distgridToCoordMap[ESMF_MAXDIM];

  //// Init coordDimMap (loop over entire distGrid dimCount)
  for (int i=0; i<distDimCount; i++) {
    distgridToCoordMap[i]=0;
  }

  //// Construct coordinate analog to array dimmap (will be 1-based)
  for (int i=0; i<coordDimCount[coord]; i++) {
    if (coordIsDist[coord][i]) {
      distgridToCoordMap[coordMapDim[coord][i]]=i+1; // convert to 1-based
    }
  }

  //// get the Array's dimmap 
  distgridToArrayMap=arrayArg->getDistGridToArrayMap();

  //// Check if the Array's dimmap matches what we expect for the coord
  ok=true;
  for (int i=0; i<distDimCount; i++) {
    if (distgridToCoordMap[i] != distgridToArrayMap[i]) ok=false;
  }
  if (!ok) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- Array and Grid Coord dimension mapping mismatch ", &rc);
      return rc;
  }
  
  // Check and make sure the array's computational bounds are
  // big enough for the stagger padding
  arrayUndistLBound=arrayArg->getUndistLBound();
  arrayUndistUBound=arrayArg->getUndistUBound();
  gridUndistLBound=undistLBound;
  gridUndistUBound=undistUBound;
  int offset[ESMF_MAXDIM];
  int staggerLBnd[ESMF_MAXDIM];
  int staggerUBnd[ESMF_MAXDIM];
  int compLBnd[ESMF_MAXDIM];
  int compUBnd[ESMF_MAXDIM];
  int localDECount=distgrid->getDELayout()->getLocalDeCount();
  for (int lDE=0; lDE < localDECount; lDE++) {
    
    //// Calculate the stagger lower bounds from the array
    ////// Get exclusive bounds (since the grid and array have
    ////// the same distgrid their exclusive bounds are the same)
   const int *exLBnd=arrayArg->getExclusiveLBound()+lDE*coordDimCount[coord];

    ////// Get stagger Lbnd offset
    localrc=this->getLDEStaggerLOffset(staggerloc, lDE, offset);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
                                              &rc)) return rc; 

    ////// Fill in the staggerLBnd array
    ////// distributed bounds are exclusive bounds minus stagger padding
    ////// undistributed bounds are undistLBound minus stagger padding
    for (int i=0; i<coordDimCount[coord]; i++) {
        int gi=coordDimMap[coord][i];
        staggerLBnd[i]=exLBnd[i]+offset[gi];
    }

    //// get computationalLBound from the array
    ///// get the computational bounds of the localDE
    const int *arrayCompLBnd=arrayArg->getComputationalLBound()+lDE*coordDimCount[coord];

    ///// Fill in the compLBnd array
    for (int i=0; i<coordDimCount[coord]; i++) {
        compLBnd[i]=arrayCompLBnd[i];
    }


    //// Make sure the grid staggerUndistLBound fit within the array computational L bounds
    for (int i=0; i<coordDimCount[coord]; i++) {
      if (compLBnd[i] != staggerLBnd[i]) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
                   "- Array computationalLBound or UndistLBound insufficient to hold grid+stagger bounds ", &rc);
      return rc;

      }
    }

    //// Calculate the stagger upper bounds from the array
    ////// Get array exclusive bounds (since the grid and array have
    ////// the same distgrid their exclusive bounds are the same)
   const int *exUBnd=arrayArg->getExclusiveUBound()+lDE*coordDimCount[coord];

    ////// Get stagger Ubnd offset
    localrc=this->getLDEStaggerUOffset(staggerloc, lDE, offset);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
                                              &rc)) return rc; 
    ////// Fill in the staggerLBnd array
    ////// distributed bounds are exclusive bounds plus stagger padding
    ////// undistributed bounds are undistUBound plus stagger padding
    for (int i=0; i<coordDimCount[coord]; i++) {
      int gi=coordDimMap[coord][i];
        staggerUBnd[i]=exUBnd[i]-offset[gi];
    }
    
    //// get computationalUBound from the array
    ///// get the computational bounds of the localDE
    const int *arrayCompUBnd=arrayArg->getComputationalUBound()+lDE*coordDimCount[coord];

    ///// Fill in the compLBnd array
    for (int i=0; i<coordDimCount[coord]; i++) {
        compUBnd[i]=arrayCompUBnd[i];
    }

    //// Make sure the grid's stagger upper bounds fit within the array computational upper bounds
    for (int i=0; i<coordDimCount[coord]; i++) {
      if (compUBnd[i] != staggerUBnd[i]) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
                   "- Array computationalUBound or UndistUBound insufficient to hold grid+stagger bounds ", &rc);
      return rc;

      }
    }
  }
 
  // If we've reached this point then arrayArg is of the right size and shape
  // to hold the coordinates in coord, so put it in. 
  rc=this->setCoordArrayInternal(staggerloc, coord, arrayArg, false);

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
//   Adds a protogrid to a grid. The protogrid is to hold data for the 
// set/commit paradigm
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
  int distDimCountArg,                        // (in)
  int *distgridToGridMapArg,                         // (in)
  int undistDimCountArg,                      // (in)
  int *undistLBoundArg,                        // (in)
  int *undistUBoundArg,                        // (in)
  int dimCountArg,                            // (in)
  int *gridEdgeLWidthArg,                       // (in)
  int *gridEdgeUWidthArg,                       // (in)
  int *gridAlignArg,                       // (in)
  int *coordDimCountArg,                      // (in)
  int **coordDimMapArg,                   // (in)
  int *gridMemLBoundArg,                      // (in)
  ESMC_IndexFlag indexflagArg,             // (in)
  bool destroyDistgridArg, 
  bool destroyDELayoutArg 
  ){
//
// !DESCRIPTION:
//   Construct the internal information structure of an ESMC\_Grid object.
//  No error checking wrt consistency of input arguments is needed because
//  constructInternal() is only to be called by construct() interfaces which
//  are responsible for providing consistent arguments to this layer.
//
//EOPI
//-----------------------------------------------------------------------------
  int rc,localrc;

  // Copy values into the grid object
  typekind = typekindArg;

  distgrid = distgridArg;

  distDimCount=distDimCountArg;

  undistDimCount=undistDimCountArg;

  dimCount = dimCountArg;

  indexflag=indexflagArg;

  destroyDistgrid=destroyDistgridArg; 
  destroyDELayout=destroyDELayoutArg;


  // Set the number of stagger locations from the grid dimCount
  staggerLocCount=_NumStaggerLocsFromDimCount(dimCount); 

  // if there are distributed dimensions - allocate and copy distgridToGridMap
  if (distDimCount) {
     distgridToGridMap = new int[distDimCount];
     memcpy(distgridToGridMap, distgridToGridMapArg, distDimCount * sizeof(int));
  }

  // if there are undistributed dimensions - allocate and copy bounds
  if (undistDimCount) {
    undistLBound = new int[undistDimCount];
    memcpy(undistLBound, undistLBoundArg, undistDimCount * sizeof(int));
    
    undistUBound = new int[undistDimCount];
    memcpy(undistUBound, undistUBoundArg, undistDimCount * sizeof(int));
  }

  // if there are any dimensions 
  if (dimCount) {
    //// record gridEdgeLWidth
    gridEdgeLWidth = new int[dimCount];
    memcpy(gridEdgeLWidth, gridEdgeLWidthArg, dimCount * sizeof(int));

    //// record gridEdgeUWidth
    gridEdgeUWidth = new int[dimCount];
    memcpy(gridEdgeUWidth, gridEdgeUWidthArg, dimCount * sizeof(int));

    //// record gridAlign
    gridAlign = new int[dimCount];
    memcpy(gridAlign, gridAlignArg, dimCount * sizeof(int));

    //// record coordDimCount
    coordDimCount = new int[dimCount];
    memcpy(coordDimCount, coordDimCountArg, dimCount * sizeof(int));
    
    //// record coordDimMap
    coordDimMap=_allocate2D<int>(dimCount,dimCount);
    for(int i=0; i<dimCount; i++)
      for(int j=0; j<dimCount; j++)
        coordDimMap[i][j]=coordDimMapArg[i][j];
    
    //// allocate coordinate array storage
    coordArrayList=_allocate2D<Array *>(staggerLocCount,dimCount);
    for(int i=0; i<staggerLocCount; i++)
      for(int j=0; j<dimCount; j++)
        coordArrayList[i][j]=ESMC_NULL_POINTER;

    //// allocate coordinate Lower Width storage
    staggerEdgeLWidthList=_allocate2D<int>(staggerLocCount,dimCount);

    //// allocate coordinate Upper Width storage
    staggerEdgeUWidthList=_allocate2D<int>(staggerLocCount,dimCount);

    //// allocate coordinate Alignment storage
    staggerAlignList=_allocate2D<int>(staggerLocCount,dimCount);

    //// allocate coordinate memLBound storage
    staggerMemLBoundList=_allocate2D<int>(staggerLocCount,dimCount);

    //// set defaults for stagger alignment and stagger width
    for(int i=0; i<staggerLocCount; i++) {
      for(int j=0; j<dimCount; j++) {
        if (i & (0x1<<j)) {   // Set defaults based on the stagger location
          staggerEdgeLWidthList[i][j]=gridEdgeLWidth[j];
          staggerEdgeUWidthList[i][j]=gridEdgeUWidth[j];
          staggerAlignList[i][j]=gridAlign[j];
        } else {
          staggerEdgeLWidthList[i][j]=0;
          staggerEdgeUWidthList[i][j]=0;
          staggerAlignList[i][j]=0;
        }
	staggerMemLBoundList[i][j]=gridMemLBoundArg[j];
      }
    }

    //// allocate storage for array allocation flag
    didIAllocList=_allocate2D<bool>(staggerLocCount,dimCount);
    //// set default
    for(int i=0; i<staggerLocCount; i++) {
      for(int j=0; j<dimCount; j++) {
          didIAllocList[i][j]=false;
      }
    }

    //// setup map from Grid dimensions to distgrid or undistUBound/undistLBound 
    //// dimensions 
    ////// allocate storage for mapping
    gridIsDist=new bool[dimCount];
    gridMapDim=new int[dimCount];

    ////// init to undistributed
    for (int i=0; i<dimCount; i++) {
      gridIsDist[i]=false;
    } 

    ////// fill in map to distributed dimensions
    for(int i=0; i<distDimCount; i++) {
      gridMapDim[distgridToGridMap[i]]=i;
      gridIsDist[distgridToGridMap[i]]=true;
    }

    ////// fill in map to undistributed dimensions
    int j=0;
    for(int i=0; i<dimCount; i++) {
      if (!gridIsDist[i]) {
        gridMapDim[i]=j;
        j++;
      }
    }

    //// setup map from coord dimensions to either distgrid
    //// dimensions or the coordinate Array's undistUBound/undistLBound dimensions
    ////// Allocate storage
    coordIsDist=_allocate2D<bool>(dimCount,dimCount);
    coordMapDim=_allocate2D<int>(dimCount,dimCount);

    ////// Fill in per coord
    for(int c=0; c<dimCount; c++) {
      int k=0;
      for (int i=0; i<coordDimCount[c]; i++) {
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
  localrc=_createIsDEBnd(&isDELBnd,&isDEUBnd, distgrid, distgridToGridMap);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
      return rc;

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
  if ((coordArg < 0) || (coordArg >= dimCount)) {
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
  distDimCount = 0;
  distgridToGridMap = ESMC_NULL_POINTER; 
  
  undistDimCount = 0;
  undistLBound = ESMC_NULL_POINTER; 
  undistUBound = ESMC_NULL_POINTER; 
  
  dimCount=0;
  gridEdgeLWidth = ESMC_NULL_POINTER; 
  gridEdgeUWidth = ESMC_NULL_POINTER; 
  gridAlign = ESMC_NULL_POINTER; 
  coordDimCount = ESMC_NULL_POINTER; 
  coordDimMap = ESMC_NULL_POINTER; 
  
  staggerLocCount=0;
  coordArrayList = ESMC_NULL_POINTER;
  staggerEdgeLWidthList = ESMC_NULL_POINTER;
  staggerEdgeUWidthList = ESMC_NULL_POINTER;
  staggerAlignList = ESMC_NULL_POINTER;
  staggerMemLBoundList = ESMC_NULL_POINTER;
  didIAllocList = ESMC_NULL_POINTER;
  
  gridIsDist = ESMC_NULL_POINTER;
  gridMapDim = ESMC_NULL_POINTER;
  
  coordIsDist = ESMC_NULL_POINTER;
  coordMapDim = ESMC_NULL_POINTER;
  
  isDELBnd = ESMC_NULL_POINTER;
  isDEUBnd = ESMC_NULL_POINTER;
  
  indexflag=ESMF_INDEX_DELOCAL;
  distgrid= ESMC_NULL_POINTER; 

  destroyDistgrid=false; 
  destroyDELayout=false;

  
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

   // Delete external class contents of Grid before deleting Grid
   //// Delete Arrays
   for(int i=0; i<staggerLocCount; i++) {
     for(int j=0; j<dimCount; j++) {
       if (didIAllocList[i][j] && (coordArrayList[i][j]!=ESMC_NULL_POINTER)) {
         Array::destroy(&coordArrayList[i][j]);
       }
     }
   }

   // Get tmpDELayout if we need it later
   // (also if prevents us from getting layout from empty grid)
   DELayout *tmpDELayout;
   if (destroyDELayout) {
     tmpDELayout=distgrid->getDELayout();
   }

   // delete distgrid
   if (destroyDistgrid) {
     DistGrid::destroy(&distgrid);
   }

   // delete delayout
   if (destroyDELayout) {
     DELayout::destroy(&tmpDELayout);
   }

   // If present delete ProtoGrid
   if (proto != ESMC_NULL_POINTER) delete proto;

   // delete distributed dimension stuff
   if (distDimCount) {
     if (distgridToGridMap !=ESMC_NULL_POINTER) delete [] distgridToGridMap;
   }

   // delete undistributed dimension stuff
   if (undistDimCount) {
     if (undistLBound !=ESMC_NULL_POINTER) delete [] undistLBound;
     if (undistUBound !=ESMC_NULL_POINTER) delete [] undistUBound;
   }

   // delete all dimension stuff
   if (dimCount) {
     if (gridEdgeLWidth !=ESMC_NULL_POINTER) delete [] gridEdgeLWidth;
     if (gridEdgeUWidth !=ESMC_NULL_POINTER) delete [] gridEdgeUWidth;
     if (gridAlign !=ESMC_NULL_POINTER) delete [] gridAlign;
     if (coordDimCount !=ESMC_NULL_POINTER) delete [] coordDimCount;
     _free2D<int>(&coordDimMap);
     _free2D<Array *>(&coordArrayList);
     _free2D<int>(&staggerEdgeLWidthList);
     _free2D<int>(&staggerEdgeUWidthList);
     _free2D<int>(&staggerAlignList);
     _free2D<int>(&staggerMemLBoundList);
     _free2D<bool>(&didIAllocList);
     if (gridIsDist !=ESMC_NULL_POINTER) delete [] gridIsDist;
     if (gridMapDim !=ESMC_NULL_POINTER) delete [] gridMapDim;
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
  gridEdgeLWidth=ESMC_NULL_POINTER;   
  gridEdgeUWidth=ESMC_NULL_POINTER;   
  gridAlign=ESMC_NULL_POINTER;   
  gridMemLBound=ESMC_NULL_POINTER;   
  distgridToGridMap=ESMC_NULL_POINTER;   
  undistLBound=ESMC_NULL_POINTER;  
  undistUBound=ESMC_NULL_POINTER;  
  coordDimCount=ESMC_NULL_POINTER;  
  coordDimMap=ESMC_NULL_POINTER; 
  indexflag=ESMC_NULL_POINTER; 
  destroyDistgrid=ESMC_NULL_POINTER; 
  destroyDELayout=ESMC_NULL_POINTER; 
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
  if (gridEdgeLWidth != ESMC_NULL_POINTER) _freeInterfaceInt(&gridEdgeLWidth);
  if (gridEdgeUWidth != ESMC_NULL_POINTER) _freeInterfaceInt(&gridEdgeUWidth);
  if (gridAlign != ESMC_NULL_POINTER) _freeInterfaceInt(&gridAlign);
  if (gridMemLBound != ESMC_NULL_POINTER) _freeInterfaceInt(&gridMemLBound);
  if (distgridToGridMap != ESMC_NULL_POINTER) _freeInterfaceInt(&distgridToGridMap);
  if (undistLBound != ESMC_NULL_POINTER) _freeInterfaceInt(&undistLBound);
  if (undistUBound != ESMC_NULL_POINTER) _freeInterfaceInt(&undistUBound);
  if (coordDimCount != ESMC_NULL_POINTER) _freeInterfaceInt(&coordDimCount);
  if (coordDimMap != ESMC_NULL_POINTER) _freeInterfaceInt(&coordDimMap);
  if (indexflag != ESMC_NULL_POINTER) delete indexflag; 
  if (destroyDistgrid != ESMC_NULL_POINTER) delete destroyDistgrid; 
  if (destroyDELayout != ESMC_NULL_POINTER) delete destroyDELayout; 
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
  if ((coordArg < 0) || (coordArg >= dimCount)) {
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
  int *staggerEdgeLWidthArg, // (in)
  int *staggerEdgeUWidthArg,  // (in)
  int *staggerMemLBoundArg  // (in)
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
  for (int i=0; i<dimCount; i++) {
    staggerAlignList[staggerlocArg][i]=staggerAlignArg[i];
   }

  // Set staggerEdgeLWidth
  for (int i=0; i<dimCount; i++) {
    staggerEdgeLWidthList[staggerlocArg][i]=staggerEdgeLWidthArg[i];
   }

  // Set staggerEdgeUWidth
  for (int i=0; i<dimCount; i++) {
    staggerEdgeUWidthList[staggerlocArg][i]=staggerEdgeUWidthArg[i];
   }

  // Set staggerMemLBound
  for (int i=0; i<dimCount; i++) {
    staggerMemLBoundList[staggerlocArg][i]=staggerMemLBoundArg[i];
   }

  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//
// serialize() and deserialize()
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::serialize()"
//BOPI
// !IROUTINE:  ESMCI::Grid::serialize - Turn Grid into a byte stream
//
// !INTERFACE:
int Grid::serialize(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
  char *buffer,          // inout - byte stream to fill
  int *length,           // inout - buf length
  int *offset)           // inout - original offset, updated to point 
                         //         to first free byte after current obj info
{
                         
//
// !DESCRIPTION:
//    Turn info in grid class into a stream of bytes.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  bool cp, done;
  int loffset,r;
  int **arrayExists;

  // Define serialization macros
#define SERIALIZE_VAR(cp,bufptr,loff,var,t) \
  if (cp) *((t *)(bufptr+loff))=var;    \
  loff += (sizeof(t));  

  // if (cp) memcpy(bufptr+loff,&var,sizeof(t));    \


#define SERIALIZE_VAR1D(cp,bufptr,loff,varptr,s1,t)    \
  if (cp) memcpy(bufptr+loff,varptr,(s1*sizeof(t)));       \
  loff += (s1*sizeof(t));  

#define SERIALIZE_VAR2D(cp,bufptr,loff,varptr,s1,s2,t) \
  if (cp) memcpy(bufptr+loff,((t **)varptr)+s1,(s1*s2*sizeof(t))); \
  loff += (s1*s2*sizeof(t));  


  // Check status
  if (status < ESMC_GRIDSTATUS_SHAPE_READY) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Grid not fully created", &rc);
    return rc;
  }

  // Create list of which Arrays exist
  arrayExists=_allocate2D<int>(staggerLocCount,dimCount);
  for (int s=0; s<staggerLocCount; s++) {
    for (int c=0; c<dimCount; c++) {
      if (coordArrayList[s][c] != ESMC_NULL_POINTER) {
	arrayExists[s][c]=1;
      } else {
	arrayExists[s][c]=0;
      }
    }
  }

  // Run twice:
  //    1. check the sizes
  //    2. do the actual copies
  cp=false;
  done=false;
  while (!done) {
    // get localoffset
    loffset=*offset;

    // First, serialize the base class,
    localrc = ESMC_Base::ESMC_Serialize(buffer, length, &loffset);

    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
      return rc;
    
    // Since we're not allowing the serialization of 
    // non-ready Grids don't worry about serializing
    // the protogrid

    // Don't do status since we're changing it anyway
    SERIALIZE_VAR(cp, buffer,loffset,typekind,ESMC_TypeKind);

    SERIALIZE_VAR(cp, buffer,loffset,indexflag,ESMC_IndexFlag);
    
    // Don't serialize these because after deserailizing distgrid and delayout are local
    //  destroyDistgrid 
    //  destroyDELayout

    SERIALIZE_VAR(cp, buffer,loffset,distDimCount,int);    

    SERIALIZE_VAR1D(cp, buffer,loffset,distgridToGridMap,distDimCount,int);

    SERIALIZE_VAR(cp, buffer,loffset,undistDimCount,int);

    SERIALIZE_VAR1D(cp, buffer,loffset,undistLBound,undistDimCount,int);
    SERIALIZE_VAR1D(cp, buffer,loffset,undistUBound,undistDimCount,int);
    
    SERIALIZE_VAR(cp, buffer,loffset,dimCount,int);    

    SERIALIZE_VAR1D(cp, buffer,loffset,gridEdgeLWidth,dimCount,int);
    SERIALIZE_VAR1D(cp, buffer,loffset,gridEdgeUWidth,dimCount,int);
    SERIALIZE_VAR1D(cp, buffer,loffset,gridAlign,dimCount,int);

    SERIALIZE_VAR1D(cp, buffer,loffset,coordDimCount,dimCount,int);

    SERIALIZE_VAR2D(cp, buffer,loffset,coordDimMap,dimCount,dimCount,int);

    SERIALIZE_VAR(cp, buffer,loffset,staggerLocCount,int);

    SERIALIZE_VAR2D(cp, buffer,loffset,staggerAlignList,staggerLocCount,dimCount,int);
    SERIALIZE_VAR2D(cp, buffer,loffset,staggerEdgeLWidthList,staggerLocCount,dimCount,int);
    SERIALIZE_VAR2D(cp, buffer,loffset,staggerEdgeUWidthList,staggerLocCount,dimCount,int);
    SERIALIZE_VAR2D(cp, buffer,loffset,staggerMemLBoundList,staggerLocCount,dimCount,int);

    // Don't serialize didIAllocList since this proxy grid won't
    // have Array's allocated 

    SERIALIZE_VAR1D(cp, buffer,loffset,gridIsDist,dimCount,bool);
    SERIALIZE_VAR1D(cp, buffer,loffset,gridMapDim,dimCount,int);

    SERIALIZE_VAR2D(cp, buffer,loffset,coordIsDist,dimCount,dimCount,bool);
    SERIALIZE_VAR2D(cp, buffer,loffset,coordMapDim,dimCount,dimCount,int);

    // Don't do isDEBnds because a proxy object isn't on a valid DE

    // make sure loffset is aligned correctly
    r=loffset%8;
    if (r!=0) loffset += 8-r;

    // Serialize the Array exists array
    SERIALIZE_VAR2D(cp, buffer,loffset,arrayExists,staggerLocCount,dimCount,int);

    // make sure loffset is aligned correctly
    r=loffset%8;
    if (r!=0) loffset += 8-r;

    // Serialize the Coord Arrays 
    for (int s=0; s<staggerLocCount; s++) {
      for (int c=0; c<dimCount; c++) {
	if (arrayExists[s][c]) {
           //// Serialize the Array
	  localrc = coordArrayList[s][c]->serialize(buffer, length, &loffset);
	  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, 
				      ESMF_ERR_PASSTHRU, &rc)) return rc;  
	}
      }
    }
    
    r=loffset%8;
    if (r!=0) loffset += 8-r;

    // Serialize the DistGrid
    localrc = distgrid->serialize(buffer, length, &loffset);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
     return rc;  

    // make sure loffset is aligned correctly
    r=loffset%8;
    if (r!=0) loffset += 8-r;

    // Check if buffer has enough free memory to hold object
    if (*length < loffset){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                      "Buffer too short to add a Grid object", &rc);
      return rc;
    }

    // If we've done the copy then we're done
    if (cp) {
      done=true;
    } else { 
      // if we haven't done the the copy,
      // then loop through again and do the copy
      cp=true;
    }
  }

  // free arrayExists
  _free2D<int>(&arrayExists);

  // output localoffset
  *offset=loffset;

  // Undefine serialization macros, so they don't cause troubles elsewhere
#undef SERIALIZE_VAR
#undef SERIALIZE_VAR1D
#undef SERIALIZE_VAR2D
 
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::deserialize()"
//BOPI
// !IROUTINE:  ESMCI::Grid::deserialize - Turn a byte stream into an Grid
//
// !INTERFACE:
int Grid::deserialize(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
  char *buffer,          // in - byte stream to read
  int *offset){          // inout - original offset, updated to point 
                         //         to first free byte after current obj info
//
// !DESCRIPTION:
//    Turn a stream of bytes into an object.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  int loffset,r;
  int **arrayExists;

  // Define serialization macros
#define DESERIALIZE_VAR(bufptr,loff,var,t) \
  var=*((t *)(bufptr+loff));    \
  loff += (sizeof(t));  

#define DESERIALIZE_VAR1D(bufptr,loff,varptr,s1,t)  \
  varptr = new t[s1];           \
  memcpy(varptr,bufptr+loff,(s1*sizeof(t)));      \
  loff += (s1*sizeof(t));  

#define DESERIALIZE_VAR2D(bufptr,loff,varptr,s1,s2,t) \
  varptr=_allocate2D<t>(s1,s2);         \
  memcpy(((t **)varptr)+s1,bufptr+loff,(s1*s2*sizeof(t))); \
  loff += (s1*s2*sizeof(t));  

  // get localoffset
  loffset=*offset;

  // First, deserialize the base class
  localrc = ESMC_Base::ESMC_Deserialize(buffer, &loffset);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  
  // Since we're not allowing the serialization of 
  // non-ready Grids don't worry about serializing
  // the protogrid

  // Set status (instead of reading it)
  status =  ESMC_GRIDSTATUS_SHAPE_READY;

  DESERIALIZE_VAR( buffer,loffset,typekind,ESMC_TypeKind);
  
  DESERIALIZE_VAR( buffer,loffset,indexflag,ESMC_IndexFlag);

  // Don't deserialize, but set 
  destroyDistgrid=true;  // distgrid is Grid's after deserialize
  destroyDELayout=false; // delayot belongs to DistGrid

  DESERIALIZE_VAR( buffer,loffset,distDimCount,int);    
  
  DESERIALIZE_VAR1D( buffer,loffset,distgridToGridMap,distDimCount,int);
  
  DESERIALIZE_VAR( buffer,loffset,undistDimCount,int);
  
  DESERIALIZE_VAR1D( buffer,loffset,undistLBound,undistDimCount,int);
  DESERIALIZE_VAR1D( buffer,loffset,undistUBound,undistDimCount,int);
  
  DESERIALIZE_VAR( buffer,loffset,dimCount,int);    
  
  DESERIALIZE_VAR1D( buffer,loffset,gridEdgeLWidth,dimCount,int);
  DESERIALIZE_VAR1D( buffer,loffset,gridEdgeUWidth,dimCount,int);
  DESERIALIZE_VAR1D( buffer,loffset,gridAlign,dimCount,int);
  
  DESERIALIZE_VAR1D( buffer,loffset,coordDimCount,dimCount,int);
  DESERIALIZE_VAR2D( buffer,loffset,coordDimMap,dimCount,dimCount,int);
    
  DESERIALIZE_VAR( buffer,loffset,staggerLocCount,int);

  DESERIALIZE_VAR2D( buffer,loffset,staggerAlignList,staggerLocCount,dimCount,int);
  DESERIALIZE_VAR2D( buffer,loffset,staggerEdgeLWidthList,staggerLocCount,dimCount,int);
  DESERIALIZE_VAR2D( buffer,loffset,staggerEdgeUWidthList,staggerLocCount,dimCount,int);
  DESERIALIZE_VAR2D( buffer,loffset,staggerMemLBoundList,staggerLocCount,dimCount,int);

  DESERIALIZE_VAR1D( buffer,loffset,gridIsDist,dimCount,bool);
  DESERIALIZE_VAR1D( buffer,loffset,gridMapDim,dimCount,int);

  DESERIALIZE_VAR2D( buffer,loffset,coordIsDist,dimCount,dimCount,bool);
  DESERIALIZE_VAR2D( buffer,loffset,coordMapDim,dimCount,dimCount,int);

  isDELBnd=ESMC_NULL_POINTER;
  isDEUBnd=ESMC_NULL_POINTER;

  // make sure loffset is aligned correctly
  r=loffset%8;
  if (r!=0) loffset += 8-r;

  // Deserialize the Array exists array
  DESERIALIZE_VAR2D( buffer,loffset,arrayExists,staggerLocCount,dimCount,int);
  
  // make sure loffset is aligned correctly
  r=loffset%8;
  if (r!=0) loffset += 8-r;
  
  // Deserialize the Coord Arrays 
  coordArrayList=_allocate2D<Array *>(staggerLocCount,dimCount);
  for (int s=0; s<staggerLocCount; s++) {
    for (int c=0; c<dimCount; c++) {
      if (arrayExists[s][c]) {
	//// Deserialize the Array
	coordArrayList[s][c]=new Array;
	coordArrayList[s][c]->deserialize(buffer, &loffset);
      } else {
	coordArrayList[s][c]=ESMC_NULL_POINTER;
      }
    }
  }
  
  
  // Setup didIAllocList if the arrayExists then deallocate it
  //// allocate storage for array allocation flag
  didIAllocList=_allocate2D<bool>(staggerLocCount,dimCount);
  //// set to all false since we're a proxy Grid
  for(int i=0; i<staggerLocCount; i++) {
    for(int j=0; j<dimCount; j++) {
      if (arrayExists[i][j]) {
	didIAllocList[i][j]=true;
      } else {
	didIAllocList[i][j]=false;
      }
    }
  }

  // make sure loffset is aligned correctly
  r=loffset%8;
  if (r!=0) loffset += 8-r;

  // Deserialize the DistGrid
  distgrid = DistGrid::deserialize(buffer, &loffset);

    // make sure loffset is aligned correctly
    r=loffset%8;
    if (r!=0) loffset += 8-r;

  // free arrayExists
  _free2D<int>(&arrayExists);

  // output localoffset
  *offset=loffset;


  // Undefine serialization macros, so they don't cause troubles elsewhere
#undef DESERIALIZE_VAR
#undef DESERIALIZE_VAR1D
#undef DESERIALIZE_VAR2D
 
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}


//-----------------------------------------------------------------------------
//
//  File Local Routines
//
//-----------------------------------------------------------------------------
// Compute the total number of stagger locations from a Grid's dimCount
static int _NumStaggerLocsFromDimCount(int dimCount)
{
  return 0x1<<dimCount;
}

// At some point consider replacing the following templated subroutines
// with a whole templated multidimensional class.

// Allocate a 2D array in one chunk of memory
template <class Type>
static  Type **_allocate2D(int sizeDim1, int sizeDim2)
  {
    Type **array,*p;

    // allocate enough space for pointers to rows and rows
    array=(Type **)malloc(sizeDim1*sizeof(Type *)+
                          sizeDim1*sizeDim2*sizeof(Type));

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

    // allocate enough space for pointers to pointers and pointers 
    // to data and data
    array=(Type ***)malloc(sizeDim1*sizeof(Type **)+
                           sizeDim1*sizeDim2*sizeof(Type *)+
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

  // Create arrays (isDEUBnd and isDELBnd) which tell if a particular DE is on
  // the edge of a tile.
  // If bit r of isDEUBnd is 1 then the DE is on the upper boundary in dim. r
  // If bit r of isDELBnd is 1 then the DE is on the lower boundary in dim. r
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::_createIsBnd()"
  static int _createIsDEBnd(char **_isDELBnd, char **_isDEUBnd, 
                            DistGrid *distgrid, int *distgridToGridMap) {
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
    if (localDECount > 0) {
      isDELBnd=new char[localDECount];
      isDEUBnd=new char[localDECount];
    } else {
      isDELBnd=ESMC_NULL_POINTER;
      isDEUBnd=ESMC_NULL_POINTER;
    }

    // loop through local DE's setting flags
    for (int lDE=0; lDE<localDECount; lDE++) {

      //// get global de
      int gDE=localDEList[lDE];

      //// get patch
      int patch=DEPatchList[gDE];

      //// Avoid patch 0 because they're 0 sized
      if (patch != 0) {
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
          
          // if we're not at the min then we're not a lower bound 
          // so turn off the bit
          if (indexList[0] != patchMin[d]) {
            isDELBnd[lDE] &= ~(0x1<<distgridToGridMap[d]);
          } 
          
          // if we're at the min then we're a lower bound
          if (indexList[deExtent[d]-1]!=patchMax[d]) {
            isDEUBnd[lDE] &= ~(0x1<<distgridToGridMap[d]);
          }
        }
      } else { // If we're empty then we're not on a boundary
        isDELBnd[lDE]=0x0;
        isDEUBnd[lDE]=0x0;
      }
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
  InterfaceInt *gridEdgeLWidthArg,             // (in) optional
  InterfaceInt *gridEdgeUWidthArg,             // (in) optional
  InterfaceInt *gridAlignArg,             // (in) optional
  InterfaceInt *distgridToGridMapArg,                  // (in) optional
  InterfaceInt *undistLBoundArg,                 // (in) optional
  InterfaceInt *undistUBoundArg,                 // (in) optional
  InterfaceInt *coordDimCountArg,               // (in) optional
  InterfaceInt *coordDimMapArg,             // (in) optional
  InterfaceInt *gridMemLBoundArg,             // (in) optional
  ESMC_IndexFlag *indexflagArg,              // (in) optional
  bool *destroyDistgridArg,
  bool *destroyDELayoutArg
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
  int dimCount;
  int distDimCount;
  int undistDimCount;
  int *distgridToGridMap;
  ESMC_TypeKind typekind;
  int *undistUBound;
  int *undistLBound;
  int *coordDimCount;
  int **coordDimMap;
  int *gridEdgeLWidth;
  int *gridEdgeUWidth;
  int *gridAlign;
  int *gridMemLBound;
  ESMC_IndexFlag indexflag;
  int ind;
  char *name;  
  bool destroyDistgrid;
  bool destroyDELayout;


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

  // If typekind wasn't passed in then use default, otherwise 
  // copy passed in value
  if (typekindArg==NULL) {
    typekind=ESMC_TYPEKIND_R8;  // Default
  } else {
    typekind=*typekindArg;
  }


  // If indexflag wasn't passed in then use default, otherwise 
  // copy passed in value
  if (indexflagArg==NULL) {
    indexflag=ESMF_INDEX_DELOCAL;  // default
  } else {
    indexflag=*indexflagArg;
  }


  // Get DimCount of Distributed Dimensions
  distDimCount = distgridArg->getDimCount();

  // Process undistLBoundArg and undistUBoundArg
  // process these first to be able to calculate dimCount before distgridToGridMap processing

  // If undistUBoundArg paramter hasn't been passed in then the grid doesn't have
  // undistributed dimensions 
  // (undistDimCount=0), if it has been then error check and copy it
  undistDimCount=0; // default to 0
  undistUBound = NULL; // default to NULL
  if (undistUBoundArg != NULL){
    if (undistUBoundArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- undistUBound array must be of dimCount 1", &rc);
      return rc;
    }
    if (undistUBoundArg->extent[0] < 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- undistUBound array must at least be of size 1", &rc);
      return rc;
    }
    undistDimCount=undistUBoundArg->extent[0]; 
    undistUBound = undistUBoundArg->array;
  }

  // If undistLBoundArg have been passed in, then copy it, unless undistUBound isn't
  // present in which
  // case there's an error (no undistUBound -> no undist. dims in grid). 
  // If undistLBound isn't present and undistUBound is then set a default, 
  // otherwise error check and copy undistLBoundArg. 
  undistLBound = NULL; // reset
  if (undistLBoundArg != NULL){
    if (undistUBoundArg==NULL){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- can't have undistUBound without undistLBound", &rc);
      return rc;
    }
    if (undistLBoundArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- undistLBound array must be of dimCount 1", &rc);
      return rc;
    }
    if (undistLBoundArg->extent[0] != undistDimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- undistLBound, undistUBound size mismatch", &rc);
      return rc;
    }
    // set undistLBound from argument
    undistLBound=new int[undistDimCount];
    for (int i=0; i<undistDimCount; i++)
         undistLBound[i]=undistLBoundArg->array[i];
  } else if (undistUBoundArg != NULL) {
    // default undistLBound to (1,1,1,...)
    undistLBound=new int[undistDimCount];
    for (int i=0; i<undistDimCount; i++)
         undistLBound[i]=1;  // default to a bottom of 1
  }


  // Compute grid dimCount (the sum of the distributed and undistributed dimCounts)
  dimCount=distDimCount+undistDimCount;

  // Grid must have positve dimCount
  if (dimCount<1) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- Grid must have dimCount >=1 ", &rc);
      return rc;
  }

  // Error check gridEdgeLWidthArg
  if (gridEdgeLWidthArg != NULL) {
    if (gridEdgeLWidthArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- gridEdgeLWidth array must be of dimCount 1", &rc);
      return rc;
    }
    if (gridEdgeLWidthArg->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- gridEdgeLWidth must be the same size as the dimCount of the Grid", &rc);
      return rc;
    }
    for (int i=0; i<dimCount; i++){
      if (gridEdgeLWidthArg->array[i] < 0){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- gridEdgeLWidth entries must be 0 or greater", &rc);
        return rc;
      }
    }
  } 

  // Error check gridEdgeUWidthArg
  if (gridEdgeUWidthArg != NULL) {
    if (gridEdgeUWidthArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- gridEdgeUWidth array must be of dimCount 1", &rc);
      return rc;
    }
    if (gridEdgeUWidthArg->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- gridEdgeUWidth must be the same size as the dimCount of the Grid", &rc);
      return rc;
    }
    for (int i=0; i<dimCount; i++) {
      if (gridEdgeUWidthArg->array[i] < 0){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- gridEdgeUWidth entries must be 0 or greater", &rc);
        return rc;
      }
    }
  }

  // Error check gridAlignArg
  if (gridAlignArg != NULL) {
    if (gridAlignArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- gridAlign array must be of dimCount 1", &rc);
      return rc;
    }
    if (gridAlignArg->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- gridAlign must be the same size as the dimCount of the Grid", &rc);
      return rc;
    }
    for (int i=0; i<dimCount; i++) {
      if ((gridAlignArg->array[i] != 1) && (gridAlignArg->array[i] != -1)){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- gridAlign entries must be +1, or -1", &rc);
        return rc;
      }
    }
  } 

  // Allocate lWidth, uWidth, and Align based on inputs and defaults
  gridEdgeLWidth = new int[dimCount];
  gridEdgeUWidth = new int[dimCount];
  gridAlign = new int[dimCount];

  localrc=setGridDefaultsLUA(dimCount,
          gridEdgeLWidthArg, gridEdgeUWidthArg, gridAlignArg,
          gridEdgeLWidth, gridEdgeUWidth, gridAlign);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
                           ESMF_ERR_PASSTHRU, &rc)) return rc;        


  // Error check gridMemLBound and fill in value
  gridMemLBound=new int[dimCount];
  if (gridMemLBoundArg != NULL) {
    if (indexflag != ESMF_INDEX_USER){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
        "- if gridMemLBound is set then indexflag must be ESMF_INDEX_USER", &rc);
      return rc;
    }
    if (gridMemLBoundArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- gridMemLBound array must be of dimCount 1", &rc);
      return rc;
    }
    if (gridMemLBoundArg->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- gridMemLBound must be the same size as the dimCount of the Grid", &rc);
      return rc;
    }
    for (int i=0; i<dimCount; i++) {
      gridMemLBound[i]=gridMemLBoundArg->array[i];
    }
  } else {
    if (indexflag == ESMF_INDEX_USER){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
        "- if indexflag=ESMF_INDEX_USER then gridMemLBound must be set", &rc);
      return rc;
    }
    for (int i=0; i<dimCount; i++) {
      gridMemLBound[i]=1;
    }
  }


  // If the distgridToGridMapArg parameter has been passed in then error check 
  // and copy it, otherwise set a default.
  distgridToGridMap = new int[distDimCount];
  if (distgridToGridMapArg == NULL) {
    for (int i=0; i<distDimCount; i++)
      distgridToGridMap[i] = i; // set distgridToGridMap to default (0,1,2..)
  } else {
    if (distgridToGridMapArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- distgridToGridMap array must be of dimCount 1", &rc);
      return rc;
    }
    if (distgridToGridMapArg->extent[0] != distDimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- distgridToGridMap and distgrid mismatch", &rc);
      return rc;
    }
    for (int i=0; i<distDimCount; i++){
      if (distgridToGridMapArg->array[i] < 1 || distgridToGridMapArg->array[i] > dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- distgridToGridMap / dimCount mismatch", &rc);
        return rc;
      }
      distgridToGridMap[i] = distgridToGridMapArg->array[i]-1;  // copy distgridToGridMap and make zero based
    }
  } 

  // If the coordDimCountArg parameter has been passed in then error check and 
  // copy it, otherwise set a default.
  coordDimCount=new int[dimCount];
  if (coordDimCountArg == NULL) {
    for (int i=0; i<dimCount; i++)
      coordDimCount[i] = dimCount; // set coordDimCount to default all curvilinear
  } else {
    if (coordDimCountArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- coordDimCount array must be of dimCount 1", &rc);
      return rc;
    }
    if (coordDimCountArg->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- coordDimCount and distgrid (and perhaps undistUBound) mismatch", &rc);
      return rc;
    }
    for (int i=0; i<dimCount; i++){
      if (coordDimCountArg->array[i] < 1 || coordDimCountArg->array[i] > dimCount){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- coordDimCount / dimCount mismatch", &rc);
        return rc;
      }
      // // TODO: take this out when Array Factorization works
      // if (coordDimCountArg->array[i] != dimCount){
      //  ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_IMPL,
      //    "- Array and thus Grid don't currently support factorization", &rc);
      //  return rc;
      // }
      coordDimCount[i] = coordDimCountArg->array[i];  // copy coordDimCount array element
    }
  } 
  
  // If the coordDimMapArg parameter has been passed in then error check and
  // copy it, otherwise set a default.
  coordDimMap=_allocate2D<int>(dimCount,dimCount);
  // initialize array to 0
  for(int i=0; i<dimCount; i++) {
    for (int j=0; j<dimCount; j++) {
      coordDimMap[i][j]=0;  
    }
  }

  if (coordDimMapArg == NULL) {
    for(int i=0; i<dimCount; i++) {
      for (int j=0; j<coordDimCount[i]; j++) {
        coordDimMap[i][j]=j;  // initialize to a default
      }
    }
  } else {
    if (coordDimCountArg == NULL){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- if coordDimMap is specified then a corresponding coordDimCount must also be specified", &rc);
      return rc;
    }
    if (coordDimMapArg->dimCount != 2){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- coordDimMap array must be of dimCount 2", &rc);
      return rc;
    }
    if ((coordDimMapArg->extent[0] != dimCount) || 
        (coordDimMapArg->extent[1] != dimCount)) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- coordDimMap and distgrid (and perhaps undistUBound) mismatch", &rc);
      return rc;
    }
    for (int i=0; i<dimCount; i++){
      for (int j=0; j<coordDimCount[i]; j++) {
        // Note: order of i,j is because of F vs. C array ordering
        ind=j*dimCount+i;

        // Check to make sure data is correct
       if (coordDimMapArg->array[ind] < 1 || coordDimMapArg->array[ind] > dimCount){
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
    for (int i=0; i<dimCount; i++){
      for (int j=0; j<dimCount; j++) {
        printf("[%d][%d]=%d\n",i,j,coordDimMap[i][j]);
      }
    }
    */

  }  
 





  // If destroyDistgrid wasn't passed in then use default, otherwise 
  // copy passed in value
  if (destroyDistgridArg==NULL) {
    destroyDistgrid=false;  // default
  } else {
    destroyDistgrid=*destroyDistgridArg;
  }

  // If destroyDELayout wasn't passed in then use default, otherwise 
  // copy passed in value
  if (destroyDELayoutArg==NULL) {
    destroyDELayout=false;  // default
  } else {
    destroyDELayout=*destroyDELayoutArg;
  }


  // construct the Grid object using the massaged parameter values
  localrc=gridArg->constructInternal(name, typekind, distgridArg, 
				     distDimCount, distgridToGridMap, 
				     undistDimCount, undistLBound, undistUBound,
				     dimCount, gridEdgeLWidth, gridEdgeUWidth,
				     gridAlign, coordDimCount, coordDimMap, 
				     gridMemLBound, indexflag, destroyDistgrid, 
				     destroyDELayout);
   if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
            ESMF_ERR_PASSTHRU, &rc)) return rc;
        
  // Dellocate temporay arrays
  if (undistUBoundArg != NULL)  delete [] undistLBound;

  if (name) delete [] name;

  delete [] gridEdgeLWidth;

  delete [] gridEdgeUWidth;

  delete [] gridAlign;

  delete [] gridMemLBound;

  delete [] distgridToGridMap;

  delete [] coordDimCount;

  _free2D<int>(&coordDimMap);

  return ESMF_SUCCESS;
  }


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::setGridDefaultsLUA()"
//BOPI
// !IROUTINE:  Grid::setGridDefaultsLUA
//
// !INTERFACE:
int setGridDefaultsLUA(
//
// !RETURN VALUE:
//   return code
//
// !ARGUMENTS:
//
                       int dimCount,                // Size of the input arrays
                       InterfaceInt *gridEdgeLWidthIn,  // (in) optional
                       InterfaceInt *gridEdgeUWidthIn,  // (in) optional
                       InterfaceInt *gridAlignIn,   // (in) optional
                       int *gridEdgeLWidthOut,          // (out)
                       int *gridEdgeUWidthOut,          // (out)
                       int *gridAlignOut            // (out)
                       ){
//
// !DESCRIPTION:
//   This routine takes in gridEdgeLWidth, gridEdgeUWidth and gridAlignment 
// information provided by the user and sets the output variables appropriately.
// If the user provides a value its used
// (or an error may be generated), otherwise a sensible default is used.
// This logic is encapsulated in one place so it'll be consistent everywhere. 
//
// NOTE: all the input arrays must at least be of size dimCount. (not present, optional,
//       interfaceInts need not obey this)
//  
//EOPI
//-----------------------------------------------------------------------------
  static int gridEdgeLWidthDefault[ESMF_MAXDIM]={0,0,0,0,0,0,0};
  static int gridEdgeUWidthDefault[ESMF_MAXDIM]={1,1,1,1,1,1,1};
  static int gridAlignDefault[ESMF_MAXDIM]={-1,-1,-1,-1,-1,-1,-1};
  int localrc, rc;

  // initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  
  localrc= setDefaultsLUA(dimCount,
                   gridEdgeLWidthIn, gridEdgeUWidthIn, gridAlignIn,
                   gridEdgeLWidthDefault, gridEdgeUWidthDefault, gridAlignDefault,
                   gridEdgeLWidthOut, gridEdgeUWidthOut, gridAlignOut);
   if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
            ESMF_ERR_PASSTHRU, &rc)) return rc;

  // return success
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Grid::setDefaultsLUA()"
//BOPI
// !IROUTINE:  Grid::process
//
// !INTERFACE:
int setDefaultsLUA(
//
// !RETURN VALUE:
//   return code
//
// !ARGUMENTS:
//
                   int dimCount,                // all of the input arrays must be of at least this size 
                   InterfaceInt *lWidthIn,  // (in) optional
                   InterfaceInt *uWidthIn,  // (in) optional
                   InterfaceInt *alignIn,   // (in) optional
                   int *lWidthDefault,      // (in)
                   int *uWidthDefault,      // (in)
                   int *alignDefault,       // (in)
                   int *lWidthOut,          // (out)
                   int *uWidthOut,          // (out)
                   int *alignOut            // (out)
 ){
//
// !DESCRIPTION:
//   This routine takes in LWidth, UWidth and Alignment information provided by the user
// and sets the output variables appropriately. If the user provides a value its used
// (or an error may be generated), otherwise a (hopefully) sensible default is used.
// This logic is encapsulated in one place so it'll be consistent everywhere. 
//
// NOTE: all the input arrays must at least be of size dimCount. (not present, optional,
//       interfaceInts need not obey this)
//  
// NOTE: This routine only does minimal error checking of the inputs.
//
//EOPI
//-----------------------------------------------------------------------------
  int rc;

  // initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  
  // At least make sure the input arrays are the right size
  if (lWidthIn != ESMC_NULL_POINTER) {
    if (lWidthIn->extent[0] < dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
                                  "- LWidth size too small ", &rc);
      return rc;
    }
  }

  if (uWidthIn != ESMC_NULL_POINTER) {
    if (uWidthIn->extent[0] < dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
                                  "- UWidth size too small ", &rc);
      return rc;
    }
  }

  if (alignIn != ESMC_NULL_POINTER) {
    if (alignIn->extent[0] < dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- align size too small ", &rc);
      return rc;
    }
  }

  // set outputs based on presence of inputs
  if (lWidthIn == ESMC_NULL_POINTER) { // lWidthIn NOT present
    if (uWidthIn == ESMC_NULL_POINTER) { // uWidthIn NOT present
      if (alignIn == ESMC_NULL_POINTER) { // alignIn NOT present
        // present:       ,       ,
        memcpy(lWidthOut,lWidthDefault,dimCount*sizeof(int));        
        memcpy(uWidthOut,uWidthDefault,dimCount*sizeof(int));        
        memcpy(alignOut,alignDefault,dimCount*sizeof(int));        
    
      } else { // alignIn present
        // present:       ,       , alignIn        
        for (int i=0; i<dimCount; i++) {
          if (alignIn->array[i] > 0) {
            lWidthOut[i]= lWidthDefault[i];
            uWidthOut[i]= 0;
          } else if (alignIn->array[i] < 0) {
            lWidthOut[i]= 0;
            uWidthOut[i]= uWidthDefault[i];
          } else {
            lWidthOut[i]= 0;
            uWidthOut[i]= 0;
          }
        }        
        memcpy(alignOut,alignIn->array,dimCount*sizeof(int));        
        
      }
    } else { // uWidthIn present
      if (alignIn == ESMC_NULL_POINTER) { // alignIn NOT present
        // present:       , UWidth,
        for (int i=0; i<dimCount; i++)
          lWidthOut[i]=0;
        memcpy(uWidthOut,uWidthIn->array,dimCount*sizeof(int));        
        for (int i=0; i<dimCount; i++)
          alignOut[i]=-1;
      } else { // alignIn present
        // present:       , UWidth, alignIn
        for (int i=0; i<dimCount; i++)
          lWidthOut[i]=0;
        memcpy(uWidthOut,uWidthIn->array,dimCount*sizeof(int));        
        memcpy(alignOut,alignIn->array,dimCount*sizeof(int));                
      }
    }
  } else { // lWidthIn present
    if (uWidthIn == ESMC_NULL_POINTER) { // uWidthIn NOT present
      if (alignIn == ESMC_NULL_POINTER) { // alignIn NOT present
        // present: lWidth,       ,
        memcpy(lWidthOut,lWidthIn->array,dimCount*sizeof(int));        
        for (int i=0; i<dimCount; i++)
          uWidthOut[i]=0;
        for (int i=0; i<dimCount; i++)
          alignOut[i]=1;
   
      } else { // alignIn present
        // present: lWidth,       , alignIn
        memcpy(lWidthOut,lWidthIn->array,dimCount*sizeof(int));        
        for (int i=0; i<dimCount; i++)
          uWidthOut[i]=0;
        memcpy(alignOut,alignIn->array,dimCount*sizeof(int));                
     
      }      
    } else { // uWidthIn present
      if (alignIn == ESMC_NULL_POINTER) { // alignIn NOT present
        // present: lWidth, UWidth, 
        memcpy(lWidthOut,lWidthIn->array,dimCount*sizeof(int));        
        memcpy(uWidthOut,uWidthIn->array,dimCount*sizeof(int));        
        for (int i=0; i<dimCount; i++) {
          if (lWidthIn->array[i] <= uWidthIn->array[i]) {
            alignOut[i]=-1;
          } else {
            alignOut[i]=1;
          }
        }
      } else { // alignIn present
        // present: lWidth, UWidth, alignIn
        memcpy(lWidthOut,lWidthIn->array,dimCount*sizeof(int));        
        memcpy(uWidthOut,uWidthIn->array,dimCount*sizeof(int));        
        memcpy(alignOut,alignIn->array,dimCount*sizeof(int));                

      }      
    }
  }

  // return success
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------





//-----------------------------------------------------------------------------


} // END ESMCI name space
//-----------------------------------------------------------------------------










