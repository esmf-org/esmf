// $Id$
//
// Earth System Modeling Framework
// Copyright (c) 2002-2024, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC interface routines

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
//
//-----------------------------------------------------------------------------
//
// insert any higher level, 3rd party or system includes here

#include <cstring>
using namespace std;

#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_DistGrid.h"
#include "ESMCI_Array.h"
#include "ESMCI_CoordSys.h"

#ifdef ESMF_GDAL
#include <ogr_api.h>
#include "ESMCI_GDAL_Util.h"
#include "ESMCI_FileIO_Util.h"
#endif

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version =
             "$Id$";
//-----------------------------------------------------------------------------

extern "C" {
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the LocStream routines
//
//

void FTN_X(c_esmc_gdal_getnfeatures)(
                                         char *filename,
					 int *local_pet,
					 int *pet_count,
					 int *nfeatures,
					 int *locfeatures,
					 int *min_id, 
					 int *max_id,
                                         int *rc,
                                         ESMCI_FortranStrLenArg filename_l) {


#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gdal_getnfeatures()"

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_GDAL
  // Open file and create datasource (DS)
  OGRDataSourceH hDS;
  if (access(filename, F_OK) == 0) {
    OGRRegisterAll(); // register all the drivers
    hDS = GDALOpenEx(filename, GDAL_OF_VECTOR | GDAL_OF_READONLY, NULL, NULL, NULL); //OGROpen( filename, FALSE, NULL );
    if( hDS == NULL )
      {
	printf( "Open failed on pet %d: %s, %d\n", *local_pet, CPLGetLastErrorMsg(), CPLGetLastErrorNo() );
      }
    } else if (*local_pet == 0) {
    printf("Cannot access shapefile %s\n",filename);
    return;
  }
  
  // GET DA DEETS!
  OGRLayerH hLayer = OGR_DS_GetLayer( hDS, 0 );
  *nfeatures = OGR_L_GetFeatureCount(hLayer,1);

//  int min_id, max_id;
  divide_ids_evenly_as_possible(*nfeatures, *local_pet, *pet_count, *min_id, *max_id);

  *locfeatures = *max_id-*min_id+1;

  printf("--- nFeatures: %d\n", *nfeatures);

  // Cleanup
  GDALClose( hDS );
#endif

  // return success
  if (rc) *rc = ESMF_SUCCESS;

  return;
}

void FTN_X(c_esmc_gdal_getglobal_fids)(
                                         char *filename,
					 int *nfeatures,
					 int *gFIDs,
                                         int *rc,
                                         ESMCI_FortranStrLenArg filename_l) {


#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gdal_getglobal_fids()"

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_GDAL
  // Open file and create datasource (DS)
  OGRDataSourceH hDS;
  if (access(filename, F_OK) == 0) {
    OGRRegisterAll(); // register all the drivers
    hDS = GDALOpenEx(filename, GDAL_OF_VECTOR | GDAL_OF_READONLY, NULL, NULL, NULL); //OGROpen( filename, FALSE, NULL );
    if( hDS == NULL )
      {
	printf( "Open failed on pet %d: %s, %d\n", 0, CPLGetLastErrorMsg(), CPLGetLastErrorNo() );
	return;
      }
  } else {
    printf("Cannot access shapefile %s\n",filename);
    return;
  }
  
  OGRLayerH hLayer = OGR_DS_GetLayer( hDS, 0 );
  for (int i=0;i<*nfeatures;i++) {
    OGRFeatureH hFeature = OGR_L_GetFeature(hLayer,i);
    gFIDs[i] = OGR_F_GetFID(hFeature);
    OGR_F_Destroy( hFeature );
  }

  // Cleanup
  GDALClose( hDS );
#endif

  // return success
  if (rc) *rc = ESMF_SUCCESS;

  return;
}

void FTN_X(c_esmc_gdal_shpinquire)(
                                         char *filename,
                                         int *local_pet,
                                         int *pet_count,
                                         int *localpoints,
                                         int *totaldims,
					 int *totfeatures,
					 int *gFIDs,
					 int *locfeatures,
					 int *FIDs,
                                         int *rc,
                                         ESMCI_FortranStrLenArg filename_l) {


#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gdal_shpinquire()"

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  *totaldims = 2; // This is fixed for now!!!

#ifdef ESMF_GDAL
  // Open file and create datasource (DS)
  OGRDataSourceH hDS;
  if (access(filename, F_OK) == 0) {
    OGRRegisterAll(); // register all the drivers
    hDS = GDALOpenEx(filename, GDAL_OF_VECTOR | GDAL_OF_READONLY, NULL, NULL, NULL); //OGROpen( filename, FALSE, NULL );
    if( hDS == NULL )
      {
	printf( "Open failed on pet %d: %s, %d\n", *local_pet, CPLGetLastErrorMsg(), CPLGetLastErrorNo() );
      }
    } else if (*local_pet == 0) {
    printf("Cannot access shapefile %s\n",filename);
    return;
  }
  
  OGRLayerH hLayer = OGR_DS_GetLayer( hDS, 0 );
  // Get positions at which to read element information
  std::vector<int> feature_ids_vec;
  get_ids_divided_evenly_across_pets(*totfeatures, *local_pet, *pet_count, feature_ids_vec);
  
//  printf("--- info: %d\n", feature_ids_vec.size());

  // Assign vector info to pointer
  *localpoints = 0;
  if (*locfeatures != 0) {
    // Get the total points in features on local PET (I don't wanna do this here, but I will and then will add it to things to fix)
    *localpoints = 0;
    for (int i=0;i<*locfeatures;i++) {
      FIDs[i] = gFIDs[feature_ids_vec[i]-1];
//      printf("gFID %d vec %d FID %d on PET %d\n", gFIDs[i], feature_ids_vec[i]-1, FIDs[i], *local_pet);  
      OGRFeatureH hFeature = OGR_L_GetFeature(hLayer,FIDs[i]);
      OGRGeometryH hGeom = OGR_F_GetGeometryRef(hFeature);
      *localpoints += OGR_G_GetPointCount(hGeom);
      OGR_F_Destroy( hFeature );
    }
  }

  // Cleanup
  GDALClose( hDS );
#endif

  // return success
  if (rc) *rc = ESMF_SUCCESS;

  return;
}

void FTN_X(c_esmc_gdal_shpgetcoords)(
                                         char *filename,
                                         int *local_pet,
                                         int *numFeatures,
					 int *FIDs,
                                         int *localcount,
					 double *coordX,
					 double *coordY,
                                         int *rc,
                                         ESMCI_FortranStrLenArg filename_l) {


#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gdal_shpgetcoords()"

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_GDAL
  // Open file and create datasource (DS)
  OGRDataSourceH hDS;
  if (access(filename, F_OK) == 0) {
    OGRRegisterAll(); // register all the drivers
    hDS = GDALOpenEx( filename, GDAL_OF_VECTOR | GDAL_OF_READONLY, NULL, NULL, NULL ); //OGROpen( filename, FALSE, NULL );
    if( hDS == NULL )
      {
	printf( "Open failed on pet %d: %s, %d\n", *local_pet, CPLGetLastErrorMsg(), CPLGetLastErrorNo() );
      }
  } else if (*local_pet == 0) {
    printf("Cannot access shapefile %s\n",filename);
    return;
  } else
    printf("hDS open\n");
  
  // GET DA DEETS!
  int num_nodes;
  int num_elems=0;
  int totNumElemConn=0;

  std::vector<double> XCoords;
  std::vector<double> YCoords;
  OGRLayerH    hLayer = OGR_DS_GetLayer( hDS, 0 );
  for (int f = 0; f < *localcount; f++) {
    OGRFeatureH hFeature = OGR_L_GetFeature(hLayer, FIDs[f]);
    if( hFeature == NULL )
	printf( "NULL Feature on PET %d with ID %: : %s, %d\n", *local_pet, FIDs[f], CPLGetLastErrorMsg(), CPLGetLastErrorNo() );
    OGRGeometryH   hGeom = OGR_F_GetGeometryRef(hFeature);
    OGRGeometryH  Cpoint = OGR_G_CreateGeometry(wkbPoint);
    if (wkbFlatten(OGR_G_GetGeometryType(hGeom)) == wkbPoint){ // just in case
      int nFTRpoints    = OGR_G_GetPointCount(hGeom);
      
      for (int i = nFTRpoints-1; i >=0; i--) {
	XCoords.push_back( OGR_G_GetX(hGeom, i) );
	YCoords.push_back( OGR_G_GetY(hGeom, i) );
      }
      OGR_G_DestroyGeometry(Cpoint); 
    }
    OGR_F_Destroy( hFeature );
  }
//  printf("<<>> Pet/localcounts: %d/%d\n", *local_pet, *localcount);

  printf("NOTE: ASSUMING DEG. CONVERTING TO RADIANS!!!\n");
  for (int i=0;i<*localcount;i++) {
    coordX[i]=XCoords[i]*ESMC_CoordSys_Deg2Rad;
    coordY[i]=YCoords[i]*ESMC_CoordSys_Deg2Rad;
  }

  // Cleanup
  if( hDS != NULL ) GDALClose( hDS );
#endif

  // return success
  if (rc) *rc = ESMF_SUCCESS;

  return;
}

// non-method functions
void FTN_X(c_esmc_locstreamgetkeybnds)(ESMCI::Array **_array,
                                     int *_localDE,
                                     int *exclusiveLBound,
                                     int *exclusiveUBound,
                                     int *exclusiveCount,
                                     int *computationalLBound,
                                     int *computationalUBound,
                                     int *computationalCount,
                                     int *totalLBound,
                                     int *totalUBound,
                                     int *totalCount,
                                     int *rc){

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_locstreamgetkeybnds()"

  ESMCI::Array *array;
  int localDE;
  int localrc;

  // Initialize return code; assume routine not implemented
  if (rc != NULL) *rc = ESMC_RC_NOT_IMPL;

  // Dereference variables
  array=*_array;

  // localDE
  if (ESMC_NOT_PRESENT_FILTER(_localDE) == ESMC_NULL_POINTER) {
    if (array->getDistGrid()->getDELayout()->getLocalDeCount()>1) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
                                    "- Must provide localDE if localDeCount >1",
                                    ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));
      return;
    } else {
      localDE=0;
    }
  } else {
    localDE=*_localDE; // already 0 based

    // Input Error Checking
    if ((localDE < 0) || (localDE >=array->getDistGrid()->getDELayout()->getLocalDeCount())) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
        "- localDE outside range on this processor", ESMC_CONTEXT,
        ESMC_NOT_PRESENT_FILTER(rc));
      return;
    }
  }

  // ExclusiveLBound
  if (ESMC_NOT_PRESENT_FILTER(exclusiveLBound) != ESMC_NULL_POINTER) {
    *exclusiveLBound=*(array->getExclusiveLBound()+localDE);
  }

  // ExclusiveUBound
  if (ESMC_NOT_PRESENT_FILTER(exclusiveUBound) != ESMC_NULL_POINTER) {
    *exclusiveUBound=*(array->getExclusiveUBound()+localDE);
  }

  // ExclusiveCount
  if (ESMC_NOT_PRESENT_FILTER(exclusiveCount) != ESMC_NULL_POINTER) {
    *exclusiveCount=*(array->getExclusiveUBound()+localDE) -
                    *(array->getExclusiveLBound()+localDE) + 1;
  }

  // ComputationalLBound
  if (ESMC_NOT_PRESENT_FILTER(computationalLBound) != ESMC_NULL_POINTER) {
    *computationalLBound=*(array->getComputationalLBound()+localDE);
  }

  // ComputationalUBound
  if (ESMC_NOT_PRESENT_FILTER(computationalUBound) != ESMC_NULL_POINTER) {
    *computationalUBound=*(array->getComputationalUBound()+localDE);
  }

  // ComputationalCount
  if (ESMC_NOT_PRESENT_FILTER(computationalCount) != ESMC_NULL_POINTER) {
    *computationalCount=*(array->getComputationalUBound()+localDE) -
                        *(array->getComputationalLBound()+localDE) + 1;
  }


  // TotalLBound
  if (ESMC_NOT_PRESENT_FILTER(totalLBound) != ESMC_NULL_POINTER) {
    *totalLBound=*(array->getTotalLBound()+localDE);
  }

  // TotalUBound
  if (ESMC_NOT_PRESENT_FILTER(totalUBound) != ESMC_NULL_POINTER) {
    *totalUBound=*(array->getTotalUBound()+localDE);
  }

  // TotalCount
  if (ESMC_NOT_PRESENT_FILTER(totalCount) != ESMC_NULL_POINTER) {
    *totalCount=*(array->getTotalUBound()+localDE) -
                *(array->getTotalLBound()+localDE) + 1;
  }



  // Return ESMF_SUCCESS
  if (rc != NULL) *rc = ESMF_SUCCESS;

  return;
}

// non-method functions
void FTN_X(c_esmc_locstreamgetelbnd)(ESMCI::DistGrid **_distgrid,
                                 int *_localDE,
                                 ESMC_IndexFlag *_indexflag,
                                 int *exclusiveLBound,
                                 int *rc){

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_locstreamgetelbnd()"

  ESMCI::DistGrid *distgrid;
  int localDE;
  int localrc;
  ESMC_IndexFlag indexflag;

  // Initialize return code; assume routine not implemented
  if (rc != NULL) *rc = ESMC_RC_NOT_IMPL;

  // Dereference variables
  distgrid=*_distgrid;
  indexflag=*_indexflag;

  // localDE
  if (ESMC_NOT_PRESENT_FILTER(_localDE) == ESMC_NULL_POINTER) {
    if (distgrid->getDELayout()->getLocalDeCount()>1) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
                                    "- Must provide localDE if localDeCount >1",
                                    ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));
      return;
    } else {
      localDE=0;
    }
  } else {
    localDE=*_localDE; // already 0 based

    // Input Error Checking
    if ((localDE < 0) || (localDE >=distgrid->getDELayout()->getLocalDeCount())) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
        "- localDE outside range on this processor", ESMC_CONTEXT,
        ESMC_NOT_PRESENT_FILTER(rc));
      return;
    }
  }

  if (indexflag==ESMC_INDEX_DELOCAL) {
    *exclusiveLBound = 1; // excl. region starts at (1,1,1...)
  } else {

    // Get some useful information
    const int *localDeToDeMap = distgrid->getDELayout()->getLocalDeToDeMap();

    // Get the Global DE from the local DE
    int de = localDeToDeMap[localDE];

    // obtain min index for this DE
    int const *index_min=distgrid->getMinIndexPDimPDe(de,&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      ESMC_NOT_PRESENT_FILTER(rc))) return;

    // Set lower bound of exclusive region
    *exclusiveLBound = *index_min;
  }

  // Return ESMF_SUCCESS
  if (rc != NULL) *rc = ESMF_SUCCESS;
  return;
}


void FTN_X(c_esmc_locstreamgeteubnd)(ESMCI::DistGrid **_distgrid,
                                 int *_localDE,
                                 ESMC_IndexFlag *_indexflag,
                                 int *exclusiveUBound,
                                 int *rc){

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_locstreamgeteubnd()"

  ESMCI::DistGrid *distgrid;
  int localDE;
  int localrc;
  ESMC_IndexFlag indexflag;

  // Initialize return code; assume routine not implemented
  if (rc != NULL) *rc = ESMC_RC_NOT_IMPL;

  // Dereference variables
  distgrid=*_distgrid;
  indexflag=*_indexflag;

  // localDE
  if (ESMC_NOT_PRESENT_FILTER(_localDE) == ESMC_NULL_POINTER) {
    if (distgrid->getDELayout()->getLocalDeCount()>1) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
                                    "- Must provide localDE if localDeCount >1",
                                    ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));
      return;
    } else {
      localDE=0;
    }
  } else {
    localDE=*_localDE; // already 0 based

    // Input Error Checking
    if ((localDE < 0) || (localDE >=distgrid->getDELayout()->getLocalDeCount())) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
        "- localDE outside range on this processor", ESMC_CONTEXT,
        ESMC_NOT_PRESENT_FILTER(rc));
      return;
    }
  }

  // Get some useful information
  const int *localDeToDeMap = distgrid->getDELayout()->getLocalDeToDeMap();
  const int *indexCountPDimPDe = distgrid->getIndexCountPDimPDe();

  // Get the Global DE from the local DE
  int de = localDeToDeMap[localDE];

  // Set upper bound based on indexflag
  if (indexflag==ESMC_INDEX_DELOCAL) {
    *exclusiveUBound = indexCountPDimPDe[de];
  } else {

    // obtain max index for this DE
    int const *index_max=distgrid->getMaxIndexPDimPDe(de,&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      ESMC_NOT_PRESENT_FILTER(rc))) return;

    // Set upper bound of exclusive region
    *exclusiveUBound = *index_max;
  }
  // Return ESMF_SUCCESS
  if (rc != NULL) *rc = ESMF_SUCCESS;

  return;
}



#if 1
// non-method functions
void FTN_X(c_esmc_locstreamserialize)(ESMC_IndexFlag *indexflag,
                                      int *keyCount, ESMC_CoordSys_Flag *coordSys,
                                      char *buffer, int *length, int *offset,
                                      ESMC_InquireFlag *inquireflag,
                                      int *rc,
                                      ESMCI_FortranStrLenArg buffer_l){


    // Initialize return code; assume routine not implemented
    if (rc) *rc = ESMC_RC_NOT_IMPL;

    // Calc size to store locstream info
    int size = sizeof(ESMC_IndexFlag)+sizeof(int)+sizeof(ESMC_CoordSys_Flag);

    // If not just inquiring save info to buffer
    //  Verify length > vars.
    if (*inquireflag != ESMF_INQUIREONLY) {

      // Make sure info will fit
      if ((*length - *offset) < size) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                "Buffer too short to add a LocStream object", ESMC_CONTEXT, rc);
        return;
      }


      // Save indexflag
      ESMC_IndexFlag *ifp = (ESMC_IndexFlag *)(buffer + *offset);
      *ifp++ = *indexflag;

      // Save keyCount
      int *ip= (int *)ifp;
      *ip++ = *keyCount;

      // Save coordSys
      ESMC_CoordSys_Flag *csp= (ESMC_CoordSys_Flag *)ip;
      *csp++ = *coordSys;
    }

    // Increase offset by size
    *offset += size;

    // return success
    if (rc) *rc = ESMF_SUCCESS;

    return;
}


void FTN_X(c_esmc_locstreamdeserialize)(ESMC_IndexFlag *indexflag,
                                        int *keyCount, ESMC_CoordSys_Flag *coordSys,
                                        char *buffer, int *offset,
                                        int *rc,
                                        ESMCI_FortranStrLenArg buffer_l){

    
    // Initialize return code; assume routine not implemented
    if (rc) *rc = ESMC_RC_NOT_IMPL;

    // Get indexflag
    ESMC_IndexFlag *ifp = (ESMC_IndexFlag *)(buffer + *offset);
    *indexflag=*ifp++;

    // Get keyCount
    int *ip= (int *)ifp;
    *keyCount=*ip++;

    // Get CoordSys
    ESMC_CoordSys_Flag *csp= (ESMC_CoordSys_Flag *)ip;
    *coordSys=*csp++;

    // Adjust offset
    *offset += sizeof(ESMC_IndexFlag)+sizeof(int)+sizeof(ESMC_CoordSys_Flag);

    // return success
    if (rc) *rc = ESMF_SUCCESS;

    return;
}

// non-method functions
void FTN_X(c_esmc_locstreamkeyserialize)(
                                       int *keyNameLen, char *keyName,
                                       int *unitsLen, char *units,
                                       int *longNameLen, char *longName,
                char *buffer, int *length, int *offset,
                ESMC_InquireFlag *inquireflag,
                int *rc,
                ESMCI_FortranStrLenArg keyName_l,
                ESMCI_FortranStrLenArg units_l,
                ESMCI_FortranStrLenArg longName_l,
                ESMCI_FortranStrLenArg buffer_l) {

  ESMC_InquireFlag linquireflag = *inquireflag;
  int *ip;
  char *cp;
  int r;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  // TODO: verify length > vars.
  int size = *keyNameLen + *unitsLen + *longNameLen;
  if (*inquireflag != ESMF_INQUIREONLY) {
    if ((*length - *offset) < size) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
         "Buffer too short to add a LocStream object", ESMC_CONTEXT, rc);
      return;
    }
  }

  // Get pointer to memory
  ip = (int *)(buffer + *offset);

  // Save string lengths
  if (linquireflag != ESMF_INQUIREONLY) {
    *ip++ = *keyNameLen;
    *ip++ = *unitsLen;
    *ip++ = *longNameLen;
  }

  // Switch to char pointer
  cp = (char *)ip;

  // Save keyNames
  if (linquireflag != ESMF_INQUIREONLY)
    memcpy((void *)cp, (const void *)keyName, *keyNameLen*sizeof(char));
  cp += *keyNameLen*sizeof(char);

  // Save units
  if (linquireflag != ESMF_INQUIREONLY)
    memcpy((void *)cp, (const void *)units, *unitsLen*sizeof(char));
  cp += *unitsLen*sizeof(char);

  // Save longName
  if (linquireflag != ESMF_INQUIREONLY)
    memcpy((void *)cp, (const void *)longName, *longNameLen*sizeof(char));
  cp += *longNameLen*sizeof(char);

  // Adjust offset
  *offset += 3*sizeof(int)+*keyNameLen + *unitsLen + *longNameLen;

  // Adjust alignment
  r=*offset%8;
  if (r!=0) *offset += 8-r;

  // return success
  if (rc) *rc = ESMF_SUCCESS;

  return;
}


void FTN_X(c_esmc_locstreamkeydeserialize)(
                                         char *keyName,
                                         char *units,
                                         char *longName,
                                         char *buffer,
                                         int *offset,
                                         int *rc,
                                         ESMCI_FortranStrLenArg keyName_l,
                                         ESMCI_FortranStrLenArg units_l,
                                         ESMCI_FortranStrLenArg longName_l,
                                         ESMCI_FortranStrLenArg buffer_l) {

  int *ip;
  char *cp;
  int r, keyNameLen, unitsLen, longNameLen;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  // Get pointer to memory
  ip = (int *)(buffer + *offset);

  // Save string lengths
  keyNameLen = *ip++;
  unitsLen = *ip++;
  longNameLen = *ip++;

  // Switch to char pointer
  cp = (char *)ip;

  // Save keyNames
  // First fill with spaces (NOTE THAT THIS ASSUMES THAT keyName is of size ESMF_MAXSTR)
  memset((void *)keyName,' ', ESMF_MAXSTR*sizeof(char));
  memcpy((void *)keyName, (const void *)cp, keyNameLen*sizeof(char));
  cp += keyNameLen*sizeof(char);

  // Save units
  // First fill with spaces (NOTE THAT THIS ASSUMES THAT units is of size ESMF_MAXSTR)
  memset((void *)units,' ', ESMF_MAXSTR*sizeof(char));
  memcpy((void *)units, (const void *)cp, unitsLen*sizeof(char));
  cp += unitsLen*sizeof(char);

  // Save longName
  // First fill with spaces (NOTE THAT THIS ASSUMES THAT longName is of size ESMF_MAXSTR)
  memset((void *)longName,' ', ESMF_MAXSTR*sizeof(char));
  memcpy((void *)longName, (const void *)cp, longNameLen*sizeof(char));
  cp += longNameLen*sizeof(char);

  // Adjust offset
  *offset += 3*sizeof(int)+keyNameLen + unitsLen + longNameLen;

  // Adjust alignment
  r=*offset%8;
  if (r!=0) *offset += 8-r;

  // return success
  if (rc) *rc = ESMF_SUCCESS;

  return;
}

#endif
  

}
