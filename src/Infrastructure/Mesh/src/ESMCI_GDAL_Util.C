// $Id$
//
// Earth System Modeling Framework
// Copyright (c) 2002-2023, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------

#include <string>
#include <ostream>
#include <iterator>
#include <algorithm>

#include "ESMCI_Macros.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"
#include "ESMCI_CoordSys.h"
#include "ESMCI_Array.h"
#include "ESMC_Util.h"

#include "ESMCI_TraceMacros.h"  // for profiling

#include "Mesh/include/ESMCI_Mesh.h"
#include "Mesh/include/Legacy/ESMCI_MeshRead.h"
#include "Mesh/include/Regridding/ESMCI_MeshRegrid.h" //only for the conservative flag in add_elements
#include "Mesh/include/Legacy/ESMCI_MeshVTK.h"
#include "Mesh/include/Legacy/ESMCI_ParEnv.h"
#include "Mesh/include/Legacy/ESMCI_MeshUtils.h"
#include "Mesh/include/Legacy/ESMCI_GlobalIds.h"
#include "Mesh/include/ESMCI_MeshRedist.h"
#include "Mesh/include/ESMCI_MeshDual.h"
#include "Mesh/include/ESMCI_Mesh_Glue.h"
#include "Mesh/include/ESMCI_FileIO_Util.h"

// These internal functions can only be used if PIO is available
#ifdef ESMF_GDAL

// TODO: SWITCH THIS TO SHAPELIB, WHEN WE KNOW WHAT IT'S CALLED
#include <ogr_api.h>
#include <gdal.h>
#include <ogr_srs_api.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id$";
//-----------------------------------------------------------------------------
using namespace ESMCI;

int elm       = 0;
int totpoints = 0;

//int numNodes, numElems;
//double *nodeXCoords,*nodeYCoords;

// Local Routines
int processPolygon(OGRGeometryH fGeom, int runtyp, double *nodeXCoords, double *nodeYCoords, int *nodeIDs, int *elemIDs, int *numElemConn, int *elemConn);
int processMultiPolygon(OGRGeometryH hGeom, int runtyp, double *nodeXCoords, double *nodeYCoords, int *nodeIDs, int *elemIDs, int *numElemConn, int *elemConn);

// Get the dimension of the mesh in the SHP file
// (This dimension is both the pdim and orig_sdim of the mesh)
void ESMCI_GDAL_SHP_get_dim_from_file(OGRDataSourceH hDS, char *filename, int &dim) {
#undef ESMC_METHOD
#define ESMC_METHOD "get_dim_from_SHP_file()"

  // value fixed for now. Just assume horizontal dims.
  // Geometries will be 'flattened' if they are 3D.
  // -- MSL 5/31/2023
  dim = 2; 

  return;
}

void ESMCI_GDAL_SHP_get_feature_info(OGRDataSourceH hDS, int *nFeatures, int *&FeatureIDs) {
  OGRLayerH hLayer;
  OGRFeatureH hFeature;

  // Access the layer (associate the handle)
  // Assume that index 0 is the layer we want.
  hLayer = OGR_DS_GetLayer( hDS, 0 );

  // Get the number of elements
  *nFeatures = OGR_L_GetFeatureCount(hLayer,1);

  // -- elemIDs
  FeatureIDs   = (int *)malloc(*nFeatures*sizeof(int));

  for (int i=0;i<*nFeatures;i++) {
    hFeature = OGR_L_GetNextFeature(hLayer);
    FeatureIDs[i] = OGR_F_GetFID(hFeature);
    printf("FEATURE ID %d\n", FeatureIDs[i]);
    OGR_F_Destroy( hFeature );
  }


  return;
}

void ESMCI_GDAL_process_shapefile_serial(
// inputs
		       OGRDataSourceH hDS, 
// outputs
		       double *&nodeCoords, 
		       int *&nodeIDs, 
		       int *&elemIDs, 
		       int *&elemConn,
		       int *&numElemConn, 
		       int *totNumElemConn, 
		       int *numNodes, 
		       int *numElems) {
  int nElements;
//  int *nodeIDs;
//  int *elemConn;//,*numElemConn;
  double *nodeXCoords,*nodeYCoords;

//  OGRRegisterAll(); // register all the drivers

//  OGRDataSourceH hDS;
  OGRLayerH hLayer;
  OGRFeatureH hFeature;

  char filename[200];

  // Step 1

  // Access the layer (associate the handle)
  // Assume that index 0 is the layer we want.
  hLayer = OGR_DS_GetLayer( hDS, 0 );

  // Get the number of elements
  nElements = OGR_L_GetFeatureCount(hLayer,1);

  printf("Number of shapefile elements: %d\n\n",nElements);

  // Step 2:

  // Rewind to the beginning, just in case
  OGR_L_ResetReading(hLayer);

  // Loop through features in layer and establish extents for allocation
  while( (hFeature = OGR_L_GetNextFeature(hLayer)) != NULL ) {
    OGRGeometryH hGeom,fGeom;
    
    // Get geometry handles
    hGeom = OGR_F_GetGeometryRef(hFeature); // looks like this should be a polygon
    fGeom = OGR_G_GetGeometryRef(hGeom,0);  // and this should be linestring
    
    printf("Feature %d geom: %d (Polygon is %d)\n",elm+1,wkbFlatten(OGR_G_GetGeometryType(hGeom)),wkbPolygon);
    
    // ADD POLYGON
    if (wkbFlatten(OGR_G_GetGeometryType(hGeom)) == wkbPolygon) {
      processPolygon(fGeom,0, NULL, NULL, NULL, NULL, NULL, NULL);
    }
    // BREAK DOWN MULTIPOLYGON AND ADD SUB-POLYGONS
    else if (wkbFlatten(OGR_G_GetGeometryType(hGeom)) == wkbMultiPolygon){
      processMultiPolygon(hGeom,0, NULL, NULL, NULL, NULL, NULL, NULL);
    }

    // Cleanup
    OGR_F_Destroy( hFeature );
  }

  // Step 3:
  // Allocate vars
  // -- numNodes - there are as many connections as nodes
  *numNodes   = totpoints; // At this point, un-trimmed. Points will repeat
  *totNumElemConn = totpoints;
  // -- numElems
  *numElems   = elm; // This is an assumption!
  // -- nodeIDs
  nodeIDs = new int[totpoints];
//  nodeIDs    = (int *)malloc(totpoints*sizeof(int));
  // -- node[X,Y]Coords
  nodeXCoords = (double *)malloc(*numNodes*sizeof(double));
  nodeYCoords = (double *)malloc(*numNodes*sizeof(double));
  // -- elemConn
  elemConn    = (int *)malloc(*numNodes*sizeof(int)); //<- nodeIDs is already this, right?
  // -- elemIDs
  elemIDs   = (int *)malloc(*numElems*sizeof(int));
  // -- numElemConn
  numElemConn   = (int *)malloc(*numElems*sizeof(int));

  // Reset counters
  elm       = 0; // Zero features
  totpoints = 0; // Zero number of points

  // Rewind to the beginning, just in case
  OGR_L_ResetReading(hLayer);

  // Loop through features in layer and establish extents for allocation
  while( (hFeature = OGR_L_GetNextFeature(hLayer)) != NULL ) {
    OGRGeometryH hGeom,fGeom;
    
    // Get geometry handles
    hGeom = OGR_F_GetGeometryRef(hFeature); // looks like this should be a polygon
    fGeom = OGR_G_GetGeometryRef(hGeom,0);  // and this should be linestring
    
    // ADD POLYGON
    if (wkbFlatten(OGR_G_GetGeometryType(hGeom)) == wkbPolygon) {
      processPolygon(fGeom,1, nodeXCoords, nodeYCoords, nodeIDs, elemIDs, numElemConn, elemConn);
    }
    // BREAK DOWN MULTIPOLYGON AND ADD SUB-POLYGONS
    else if (wkbFlatten(OGR_G_GetGeometryType(hGeom)) == wkbMultiPolygon){
      processMultiPolygon(hGeom,1, nodeXCoords, nodeYCoords, nodeIDs, elemIDs, numElemConn, elemConn);
    }

    // Cleanup
    OGR_F_Destroy( hFeature );
  }

//  printf("\n");
//  printf("N Features: %d\n",elm);
//  printf("N Points:   %d\n",totpoints);
//  printf("numNodes:   %d\n",numNodes);

  nodeCoords= new double[2*totpoints];

  // Pass OGR Values to Mesh arrays
  printf("Coords: \n");
  int j = 0;
  for (int i=0;i<totpoints;i++) {
//    printf("%d: %.2f, %.2f\n",nodeIDs[i],nodeXCoords[i],nodeYCoords[i]);
    nodeCoords[j]   = nodeXCoords[i];
    nodeCoords[j+1] = nodeYCoords[i];
    j+=2;
  }

//  printf("--\n");
//  printf("elemID: numElemConn\n");
//
//  for (i=0;i<elm;i++) {
//    printf("%d: %d\n", elemIDs[i],numElemConn[i]);
//  }
//  printf("--\n");

  free(nodeXCoords);
  free(nodeYCoords);

  return ;
}
void ESMCI_GDAL_process_shapefile_distributed(
// inputs
		       OGRDataSourceH hDS, 
		       int *nFeatures,
		       int *&featureIDs,
		       int *&globFeatureIDs,
// outputs
		       double *&nodeCoords, 
		       int *&nodeIDs, 
		       int *&elemIDs, 
		       int *&elemConn,
		       int *&numElemConn, 
		       int *totNumElemConn, 
		       int *numNodes, 
		       int *numElems) {
  double *nodeXCoords,*nodeYCoords;
  OGRLayerH hLayer;
  OGRFeatureH hFeature;

  char filename[200];

  // Step 1

  // Access the layer (associate the handle)
  // Assume that index 0 is the layer we want.
  hLayer = OGR_DS_GetLayer( hDS, 0 );

  // Step 2:

  // Rewind to the beginning, just in case
  OGR_L_ResetReading(hLayer);

  // Loop through features in layer and establish extents for allocation
  for (int i = 0; i < *nFeatures; i++) {

    OGRGeometryH hGeom,fGeom;
    
//    printf("Feature %d, ID %d, i %d\n", globFeatureIDs[featureIDs[i]-1],featureIDs[i]-1,i);
    hFeature = OGR_L_GetFeature(hLayer, globFeatureIDs[featureIDs[i]-1]);

    // Get geometry handles
    hGeom = OGR_F_GetGeometryRef(hFeature); // looks like this should be a polygon
    fGeom = OGR_G_GetGeometryRef(hGeom,0);  // and this should be linestring
    
    // ADD POLYGON
    if (wkbFlatten(OGR_G_GetGeometryType(hGeom)) == wkbPolygon) {
      processPolygon(fGeom,0, NULL, NULL, NULL, NULL, NULL, NULL);
    }
    // BREAK DOWN MULTIPOLYGON AND ADD SUB-POLYGONS
    else if (wkbFlatten(OGR_G_GetGeometryType(hGeom)) == wkbMultiPolygon){
      processMultiPolygon(hGeom,0, NULL, NULL, NULL, NULL, NULL, NULL);
    }

    // Cleanup
    OGR_F_Destroy( hFeature );
  }

  // Step 3:
  // Allocate vars
  // -- numNodes - there are as many connections as nodes
  *numNodes   = totpoints; // At this point, un-trimmed. Points will repeat
  *totNumElemConn = totpoints;
  // -- numElems
  *numElems   = elm; // This is an assumption!
  // -- nodeIDs
  nodeIDs = new int[totpoints];
  // -- node[X,Y]Coords
  nodeXCoords = (double *)malloc(*numNodes*sizeof(double));
  nodeYCoords = (double *)malloc(*numNodes*sizeof(double));
  // -- elemConn
  elemConn    = (int *)malloc(*numNodes*sizeof(int)); //<- nodeIDs is already this, right?
  // -- elemIDs
  elemIDs   = (int *)malloc(*numElems*sizeof(int));
  // -- numElemConn
  numElemConn   = (int *)malloc(*numElems*sizeof(int));

  // Reset counters
  elm       = 0; // Zero features
  totpoints = 0; // Zero number of points

  // Rewind to the beginning, just in case
  OGR_L_ResetReading(hLayer);

  // Loop through features in layer and establish extents for allocation
  for (int i = 0; i < *nFeatures; i++) {

    OGRGeometryH hGeom,fGeom;
    
    hFeature = OGR_L_GetFeature(hLayer, globFeatureIDs[featureIDs[i]-1]);

    if ( hFeature == NULL) { 
      printf("NULL feature %d\n", i);
    }

    // Get geometry handles
    hGeom = OGR_F_GetGeometryRef(hFeature); // looks like this should be a polygon
    fGeom = OGR_G_GetGeometryRef(hGeom,0);  // and this should be linestring

    if ( hGeom == NULL || fGeom == NULL ) { 
      printf("NULL Geometry %d\n", i);
    }
    
    // ADD POLYGON
    if (wkbFlatten(OGR_G_GetGeometryType(hGeom)) == wkbPolygon) {
      processPolygon(fGeom,1, nodeXCoords, nodeYCoords, nodeIDs, elemIDs, numElemConn, elemConn);
    }
    // BREAK DOWN MULTIPOLYGON AND ADD SUB-POLYGONS
    else if (wkbFlatten(OGR_G_GetGeometryType(hGeom)) == wkbMultiPolygon){
      processMultiPolygon(hGeom,1, nodeXCoords, nodeYCoords, nodeIDs, elemIDs, numElemConn, elemConn);
    }

    // Cleanup
    OGR_F_Destroy( hFeature );
  }

  nodeCoords= new double[2*totpoints];

  // Pass OGR Values to Mesh arrays
  int j = 0;
  for (int i=0;i<totpoints;i++) {
    nodeCoords[j]   = nodeXCoords[i];
    nodeCoords[j+1] = nodeYCoords[i];
    j+=2;
  }

  free(nodeXCoords);
  free(nodeYCoords);

  return ;
}
int processPolygon(OGRGeometryH fGeom, int runtyp, double *nodeXCoords, double *nodeYCoords,
		   int *nodeIDs, int *elemIDs, int *numElemConn, int *elemConn) {
  // runtyp: if 0, just counting things. 
  //         if 1, populating arrays
  int points;
//  double *Xcoords,*Ycoords;

  points    = OGR_G_GetPointCount(fGeom);
  
  // Points of a polygon should start and end the same.
  // This will determine the element params and serve
  // as a check.
  if( OGR_G_GetX(fGeom,0) != OGR_G_GetX(fGeom,points-1) &&
      OGR_G_GetY(fGeom,0) != OGR_G_GetY(fGeom,points-1) )
    {
      printf( "Incomplete polygon. (X,Y)i != (X,Y)e\n" );
      return 1;
    }
  
  // If populating, (i.e. if runtyp = 1), then populate
  if ( runtyp == 1 ) {
    // Set nodeIDs & nodeCoords
    // assumes OGR reads a clockwise ring. This loop reverses it.
    int ii=0;
    for (int i = points-2; i >=0; i--) {
      nodeXCoords[totpoints+ii] = OGR_G_GetX(fGeom, i);
      nodeYCoords[totpoints+ii] = OGR_G_GetY(fGeom, i);
      nodeIDs[totpoints+ii]     = totpoints+ii+1; // increment from 1, not 0
      elemConn[totpoints+ii]    = totpoints+ii+1;
      ii++;
    }    
    numElemConn[elm]         = points-1;
    elemIDs[elm]             = elm+1;
  }
  
  // Successful query
  elm++;                          // Increment the number of features.
  totpoints = totpoints+points-1; // Increment total number of points

  return 0;
}

int processMultiPolygon(OGRGeometryH hGeom, int runtyp, double *nodeXCoords, double *nodeYCoords, 
			int *nodeIDs, int *elemIDs, int *numElemConn, int *elemConn) {
  // runtyp: if 0, just counting things. 
  //         if 1, populating arrays
  int j;
  int nGeom;
  OGRGeometryH fGeom,mGeom;

  // get number of geometries in MP
  nGeom = OGR_G_GetGeometryCount(hGeom);
//  printf( "MultiPolygon: %d\n", nGeom );
  
  // Loop over nGeom
  for (j = 0; j < nGeom; j++) {
    fGeom = OGR_G_GetGeometryRef(hGeom,j);
    mGeom = OGR_G_GetGeometryRef(fGeom,0);
    
    // ADD POLYGON
    if (wkbFlatten(OGR_G_GetGeometryType(fGeom)) == wkbPolygon) {
      processPolygon(mGeom,runtyp, nodeXCoords, nodeYCoords, nodeIDs, elemIDs, numElemConn, elemConn);
    } // Or RECURSE INTO SUB MULTIPOLYGON
    else if(wkbFlatten(OGR_G_GetGeometryType(fGeom)) == wkbMultiPolygon) {
      processMultiPolygon(fGeom,runtyp, nodeXCoords, nodeYCoords, nodeIDs, elemIDs, numElemConn, elemConn);
    }
  }
//  printf("\n");

  return 0;
}

#endif // ifdef ESMF_GDAL

