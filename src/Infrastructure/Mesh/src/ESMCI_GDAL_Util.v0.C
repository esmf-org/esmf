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
#include <vector>
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

// These internal functions can only be used if GDAL is available
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
int processPolygon(OGRGeometryH fGeom, std::vector<double> &polyXCoords, std::vector<double> &polyYCoords, std::vector<int> 
		   &polyConn, std::vector<int> &numPolyConn, std::vector<int> &polyNodeIDs, int *nPpoints);
int processMultiPolygon(OGRGeometryH hGeom, std::vector<double> &mpolyXCoords, std::vector<double> &mpolyYCoords, std::vector<int> &mpolyConn, 
			std::vector<int> &nPolyConn, std::vector<int> &polyNodeIDs, int *nMPpoints, int *nMPconn);
int getLayerInfo( OGRLayerH hL, int *nPoints, int *nGeom);

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
//    printf("<<>> FeatureID[%d]: %d\n",i,FeatureIDs[i]);
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
//>>>>  int nElements;
//>>>>//  int *nodeIDs;
//>>>>//  int *elemConn;//,*numElemConn;
//>>>>  double *nodeXCoords,*nodeYCoords;
//>>>>
//>>>>//  OGRRegisterAll(); // register all the drivers
//>>>>
//>>>>//  OGRDataSourceH hDS;
//>>>>  OGRLayerH hLayer;
//>>>>  OGRFeatureH hFeature;
//>>>>
//>>>>  char filename[200];
//>>>>
//>>>>  // Step 1
//>>>>
//>>>>  // Access the layer (associate the handle)
//>>>>  // Assume that index 0 is the layer we want.
//>>>>  hLayer = OGR_DS_GetLayer( hDS, 0 );
//>>>>
//>>>>  // Get the number of elements
//>>>>  nElements = OGR_L_GetFeatureCount(hLayer,1);
//>>>>
//>>>>  printf("Number of shapefile elements: %d\n\n",nElements);
//>>>>
//>>>>  // Step 2:
//>>>>
//>>>>  // Rewind to the beginning, just in case
//>>>>  OGR_L_ResetReading(hLayer);
//>>>>
//>>>>  // Loop through features in layer and establish extents for allocation
//>>>>  while( (hFeature = OGR_L_GetNextFeature(hLayer)) != NULL ) {
//>>>>    OGRGeometryH hGeom,fGeom;
//>>>>    
//>>>>    // Get geometry handles
//>>>>    hGeom = OGR_F_GetGeometryRef(hFeature); // looks like this should be a polygon
//>>>>    fGeom = OGR_G_GetGeometryRef(hGeom,0);  // and this should be linestring
//>>>>    
//>>>>    printf("Feature %d geom: %d (Polygon is %d)\n",elm+1,wkbFlatten(OGR_G_GetGeometryType(hGeom)),wkbPolygon);
//>>>>    
//>>>>    // ADD POLYGON
//>>>>    if (wkbFlatten(OGR_G_GetGeometryType(hGeom)) == wkbPolygon) {
//>>>>      processPolygon(fGeom,0, NULL, NULL, NULL, NULL, NULL, NULL);
//>>>>    }
//>>>>    // BREAK DOWN MULTIPOLYGON AND ADD SUB-POLYGONS
//>>>>    else if (wkbFlatten(OGR_G_GetGeometryType(hGeom)) == wkbMultiPolygon){
//>>>>      processMultiPolygon(hGeom,0, NULL, NULL, NULL, NULL, NULL, NULL);
//>>>>    }
//>>>>
//>>>>    // Cleanup
//>>>>    OGR_F_Destroy( hFeature );
//>>>>  }
//>>>>
//>>>>  // Step 3:
//>>>>  // Allocate vars
//>>>>  // -- numNodes - there are as many connections as nodes
//>>>>  *numNodes   = totpoints; // At this point, un-trimmed. Points will repeat
//>>>>  *totNumElemConn = totpoints;
//>>>>  // -- numElems
//>>>>  *numElems   = elm; // This is an assumption!
//>>>>  // -- nodeIDs
//>>>>  nodeIDs = new int[totpoints];
//>>>>//  nodeIDs    = (int *)malloc(totpoints*sizeof(int));
//>>>>  // -- node[X,Y]Coords
//>>>>  nodeXCoords = (double *)malloc(*numNodes*sizeof(double));
//>>>>  nodeYCoords = (double *)malloc(*numNodes*sizeof(double));
//>>>>  // -- elemConn
//>>>>  elemConn    = (int *)malloc(*numNodes*sizeof(int)); //<- nodeIDs is already this, right?
//>>>>  // -- elemIDs
//>>>>  elemIDs   = (int *)malloc(*numElems*sizeof(int));
//>>>>  // -- numElemConn
//>>>>  numElemConn   = (int *)malloc(*numElems*sizeof(int));
//>>>>
//>>>>  // Reset counters
//>>>>  elm       = 0; // Zero features
//>>>>  totpoints = 0; // Zero number of points
//>>>>
//>>>>  // Rewind to the beginning, just in case
//>>>>  OGR_L_ResetReading(hLayer);
//>>>>
//>>>>  // Loop through features in layer and establish extents for allocation
//>>>>  while( (hFeature = OGR_L_GetNextFeature(hLayer)) != NULL ) {
//>>>>    OGRGeometryH hGeom,fGeom;
//>>>>    
//>>>>    // Get geometry handles
//>>>>    hGeom = OGR_F_GetGeometryRef(hFeature); // looks like this should be a polygon
//>>>>    fGeom = OGR_G_GetGeometryRef(hGeom,0);  // and this should be linestring
//>>>>    
//>>>>    // ADD POLYGON
//>>>>    if (wkbFlatten(OGR_G_GetGeometryType(hGeom)) == wkbPolygon) {
//>>>>      processPolygon(fGeom,1, nodeXCoords, nodeYCoords, nodeIDs, elemIDs, numElemConn, elemConn);
//>>>>    }
//>>>>    // BREAK DOWN MULTIPOLYGON AND ADD SUB-POLYGONS
//>>>>    else if (wkbFlatten(OGR_G_GetGeometryType(hGeom)) == wkbMultiPolygon){
//>>>>      processMultiPolygon(hGeom,1, nodeXCoords, nodeYCoords, nodeIDs, elemIDs, numElemConn, elemConn);
//>>>>    }
//>>>>
//>>>>    // Cleanup
//>>>>    OGR_F_Destroy( hFeature );
//>>>>  }
//>>>>
//>>>>//  printf("\n");
//>>>>//  printf("N Features: %d\n",elm);
//>>>>//  printf("N Points:   %d\n",totpoints);
//>>>>//  printf("numNodes:   %d\n",numNodes);
//>>>>
//>>>>  nodeCoords= new double[2*totpoints];
//>>>>
//>>>>  // Pass OGR Values to Mesh arrays
//>>>>  printf("Coords: \n");
//>>>>  int j = 0;
//>>>>  for (int i=0;i<totpoints;i++) {
//>>>>//    printf("%d: %.2f, %.2f\n",nodeIDs[i],nodeXCoords[i],nodeYCoords[i]);
//>>>>    nodeCoords[j]   = nodeXCoords[i];
//>>>>    nodeCoords[j+1] = nodeYCoords[i];
//>>>>    j+=2;
//>>>>  }
//>>>>
//>>>>//  printf("--\n");
//>>>>//  printf("elemID: numElemConn\n");
//>>>>//
//>>>>//  for (i=0;i<elm;i++) {
//>>>>//    printf("%d: %d\n", elemIDs[i],numElemConn[i]);
//>>>>//  }
//>>>>//  printf("--\n");
//>>>>
//>>>>  free(nodeXCoords);
//>>>>  free(nodeYCoords);
//>>>>
//>>>>  return ;
}

void ESMCI_GDAL_process_shapefile_distributed(
// inputs
		       OGRDataSourceH hDS, 
		       int *nFeatures,
		       int *&featureIDs,
		       int *&globFeatureIDs,
// outputs
		       double *&nodeCoords, 
		       std::vector<int> &nodeIDs, 
		       std::vector<int> &elemIDs, 
		       std::vector<int> &elemConn,
		       std::vector<double> &elemCoords,
		       std::vector<int> &numElemConn, 
		       int *totNumElemConn, 
		       int *nNodes, 
		       int *nElems) {

  std::vector<double> XCoords;
  std::vector<double> YCoords;

  OGRRegisterAll(); // register all the drivers

  OGRLayerH hLayer;
  OGRFeatureH hFeature;

  // Step 1
  // Access the layer (associate the handle)
  // Assume that index 0 is the layer we want.
  hLayer = OGR_DS_GetLayer( hDS, 0 );

  // Step 2: Get layer extends (number of elements, points & geometries)

  // Get the number of elements
  *nElems = OGR_L_GetFeatureCount(hLayer,1);

  int nPoints = 0; 
  int nGeom   = 0;
  int ierr    = getLayerInfo(hLayer, &nPoints, &nGeom);

  // Rewind to the beginning, just in case
  OGR_L_ResetReading(hLayer);

  // Step 3:
  totpoints = 0; // Zero number of points

  // Rewind to the beginning, just in case
  OGR_L_ResetReading(hLayer);

  // Loop through features in layer to build grid structures and establish coords & connectivity
  std::vector<int> FTRconn, nFTRconn;//, nodeIDs;
  for (int i = 0; i < *nFeatures; i++) { // Distributed, but still over all features (e.g. nElems)

    OGRGeometryH hGeom,fGeom;
    int nFTRpoints;
    
    OGRGeometryH Cpoint = OGR_G_CreateGeometry(wkbPoint);

    hFeature = OGR_L_GetFeature(hLayer, globFeatureIDs[featureIDs[i]-1]); // Distributed
//    printf("Feature ID glob/local: %d/%d\n",globFeatureIDs[featureIDs[i]-1],featureIDs[i]-1);

    // Get geometry handles
    hGeom = OGR_F_GetGeometryRef(hFeature); // looks like this should be a polygon
    fGeom = OGR_G_GetGeometryRef(hGeom,0);  // and this should be linestring

    // Get element coordinates
    // ASSUME: 2D
    OGR_G_Centroid(hGeom,Cpoint);
    elemCoords.push_back(OGR_G_GetX(Cpoint, 0));
    elemCoords.push_back(OGR_G_GetY(Cpoint, 0));
//    printf("X,Y: %f,%f\n",OGR_G_GetX(Cpoint, 0),OGR_G_GetY(Cpoint, 0));
  
    // ADD POLYGON
    if (wkbFlatten(OGR_G_GetGeometryType(hGeom)) == wkbPolygon) {
      processPolygon(fGeom, XCoords, YCoords, elemConn, numElemConn, nodeIDs, &nFTRpoints);
    }
    // BREAK DOWN MULTIPOLYGON AND ADD SUB-POLYGONS
    else if (wkbFlatten(OGR_G_GetGeometryType(hGeom)) == wkbMultiPolygon){
      processMultiPolygon(hGeom, XCoords, YCoords, elemConn, numElemConn, nodeIDs, &nFTRpoints, NULL);
    }
    totpoints += nFTRpoints;

    // Checks
    //printf("Xcoords: %d (%d/%d) %d\n",polyXCoords.size(),nPpoints,nMPp,mpolyXCoords.size());

    // Cleanup
    OGR_F_Destroy( hFeature );
  }

  nodeCoords= new double[2*totpoints];

  // Pass data to output variables
  // Pass OGR Values to Mesh arrays
  int j = 0;
  for (int i=0;i<totpoints;i++) {
    nodeCoords[j]   = XCoords[i];
    nodeCoords[j+1] = YCoords[i];
//    printf("<<>> %d: %.2f, %.2f\n",nodeIDs[i],XCoords[i],YCoords[i]);
    j+=2;
  }
  *nNodes = totpoints;
  // Get total number of connections on this PET
  *totNumElemConn=0;
  for (int i=0; i<(int)numElemConn.size(); i++) {
    *totNumElemConn += numElemConn[i];
  }
//  *totNumElemConn = numElemConn.size();

  // Cleanup
  //int rc = GDALClose( hDS );

  return ;
}

int processPolygon(
  // Just return specs of a given geometry handle. Should be a linestring or polygon.
  // We don't add to the global spec arrays here because we may need to wedge in
  // polybreaks for ESMF in the case we're in a multipolygon loop.
   OGRGeometryH fGeom,  // Geometry handle for this polygon/linestring
   std::vector<double> &polyXCoords, // X coords in this polygon/linestring.
   std::vector<double> &polyYCoords, // X coords in this polygon/linestring.
   std::vector<int> &polyConn,
   std::vector<int> &numPolyConn,
   std::vector<int> &polyNodeIDs,
   int    *nPpoints)   // Number of points in this polygon/linestring
{

  *nPpoints    = OGR_G_GetPointCount(fGeom)-1;

  // Points of a polygon should start and end the same.
  // This will determine the element params and serve
  // as a check.
  if( OGR_G_GetX(fGeom,0) != OGR_G_GetX(fGeom,*nPpoints) &&
      OGR_G_GetY(fGeom,0) != OGR_G_GetY(fGeom,*nPpoints) )
    {
      printf( "Incomplete polygon. (X,Y)i != (X,Y)e\n" );
      return 1;
    }
  
  // Set polyConns & polyCoords
  for (int i = 0; i < *nPpoints; i++) {
    polyConn.push_back(polyXCoords.size()+i+1); 
    polyNodeIDs.push_back(polyXCoords.size()+i+1);
  }
  numPolyConn.push_back(*nPpoints);

  // -- Set coords: 
  //    this is done this way because it appears GDAL reads these clockwise
  //    and we need it to be counterclockwise. So the loop is reversed.
  //    Haven't found a way to test for this using GDAL, so this is
  //    a hardwire
  for (int i = *nPpoints-1; i >=0; i--) {
    polyXCoords.push_back( OGR_G_GetX(fGeom, i) );
    polyYCoords.push_back( OGR_G_GetY(fGeom, i) );
  }

// Success (currently, there's no error checking)
  return 0;
}

int processMultiPolygon(
  OGRGeometryH hGeom, 
  std::vector<double> &mpolyXCoords, 
  std::vector<double> &mpolyYCoords, 
  std::vector<int> &polyConn, 
  std::vector<int> &nPolyConn, 
  std::vector<int> &polyNodeIDs,
  int    *nMPpoints,
  int    *nMPconn)
{ 
  int j;
  int nGeom;
  int nPpoints, nPconn;
  int nMPp = 0;

//  double *polyXCoords, *polyYCoords;
  std::vector<double> polyXCoords;
  std::vector<double> polyYCoords;

  OGRGeometryH fGeom,mGeom;

//  nMPpoints = 0;

  // get number of geometries in MP
  nGeom = OGR_G_GetGeometryCount(hGeom);
  
  // Loop over nGeom in the multipolygon
  for (j = 0; j < nGeom; j++) {
    fGeom = OGR_G_GetGeometryRef(hGeom,j);
    mGeom = OGR_G_GetGeometryRef(fGeom,0);
    
    // ADD POLYGON
    if (wkbFlatten(OGR_G_GetGeometryType(fGeom)) == wkbPolygon) {
      processPolygon(mGeom, mpolyXCoords, mpolyYCoords, polyConn, nPolyConn, polyNodeIDs, &nPpoints);
      nMPp += nPpoints; // Updated number of total points in MP
    } // Or RECURSE INTO SUB MULTIPOLYGON
    else if(wkbFlatten(OGR_G_GetGeometryType(fGeom)) == wkbMultiPolygon) {
      // To be done. For multiPolygons made of multiPolygons.
      printf("Multipolygon of multipolygons!!");
      exit(1);
      // processMultiPolygon(fGeom,runtyp, mpolyXCoords, mpolyYCoords);
    }

    // Now, we need to expand the arrays to fit the new polygon info, and
    // include a polybreak.
    //
    // -- append a polybreak to the element connectivity vectors
    if (j < nGeom-1) {
      polyConn.push_back(MESH_POLYBREAK_IND); // THIS SHOULD BE MESH_POLYBREAK_END
      nPolyConn.back() = nPolyConn.back()+1;
      printf("<<>><<>> ADDING POLYBREAK %d %d\n",polyConn.back(),nPolyConn.back());
    }

    // Need to collapse the number of connectivity entries, since this is a multipolygon.
    // processPolygon() treats them like individual entries.  
    if ( j > 0 ) {
      nPolyConn[nPolyConn.size()-2] += nPolyConn.back();
      nPolyConn.pop_back();
    }

    // -- append the coordinate vectors
    //mpolyXCoords.insert(mpolyXCoords.end(),polyXCoords.begin(),polyXCoords.end());
    //mpolyYCoords.insert(mpolyYCoords.end(),polyYCoords.begin(),polyYCoords.end());

    // printf("Xcoords: %d (%d/%d) %d\n",polyXCoords.size(),nPpoints,nMPp,mpolyXCoords.size());
    // The above print statement and nMPp are just for checking that sizes are correct.

    polyXCoords.clear();
    polyYCoords.clear();

  }
  *nMPpoints = nMPp;

  return 0;
}

int getLayerInfo( OGRLayerH hL, int *nPoints, int *nGeom) 
{
  int nGeom_tmp = 0;
  int nPnts_tmp = 0;
  OGRFeatureH hFeature;

  // Rewind to the beginning, just in case
  OGR_L_ResetReading(hL);

  // Loop through features in layer and establish extents for allocation
  while( (hFeature = OGR_L_GetNextFeature(hL)) != NULL ) {
    OGRGeometryH hGeom1,hGeom2,hGeom3;
    
    // Get geometry handles
    hGeom1 = OGR_F_GetGeometryRef(hFeature); // looks like this should be a polygon
    
    // Process POLYGON
    if (wkbFlatten(OGR_G_GetGeometryType(hGeom1)) == wkbPolygon) {
      hGeom2 = OGR_G_GetGeometryRef(hGeom1,0);  // and this should be linestring
      nGeom_tmp++;
      *nGeom=nGeom_tmp;
      nPnts_tmp = OGR_G_GetPointCount(hGeom2);
      *nPoints += nPnts_tmp-1;
    } // or process a MULTIPOLYGON
    else if(wkbFlatten(OGR_G_GetGeometryType(hGeom1)) == wkbMultiPolygon) {
      int nGeomMP = OGR_G_GetGeometryCount(hGeom1);
      for (int j = 0; j < nGeomMP; j++) {
	hGeom2 = OGR_G_GetGeometryRef(hGeom1,j); // this should be a polygon
	// Process sub-polygon
	if (wkbFlatten(OGR_G_GetGeometryType(hGeom2)) == wkbPolygon) {
	  hGeom3 = OGR_G_GetGeometryRef(hGeom2,0);  // and this should be linestring
	  nGeom_tmp++;
	  *nGeom=nGeom_tmp;
	  nPnts_tmp = OGR_G_GetPointCount(hGeom3);
	  *nPoints += nPnts_tmp-1; // subtract one so we don't repeat the point that closes the ring
	} // Or RECURSE INTO SUB MULTIPOLYGON
	else if(wkbFlatten(OGR_G_GetGeometryType(hGeom2)) == wkbMultiPolygon) {
	  // To be done. For multiPolygons made of multiPolygons.
	  printf("Multipolygon of multipolygons!!");
	  exit(1);
	}
      }
    }
    OGR_F_Destroy(hFeature);
  }
  return 0;
}

struct NODE_INFO {
  int node_id;
  int local_elem_conn_pos;

  bool operator< (const NODE_INFO &rhs) const {
    return node_id < rhs.node_id;
  }

};

// Note that local_elem_conn is base-1 (as expected by mesh create routines)
void convert_global_elem_conn_to_local_elem_info(int num_local_elem, int tot_num_elem_conn, int *num_elem_conn, int *global_elem_conn, int*& local_elem_conn) {
  // Init output
  local_elem_conn=NULL;

  // If nothing to do leave
  if (tot_num_elem_conn < 1) return;

  // Allocate conversion list
  NODE_INFO *convert_list=new NODE_INFO[tot_num_elem_conn];

  // Copy global elem connection info into conversion list
  int num_node_conn=0; // Number of connections that are nodes (vs. polybreak)
  for (int i=0; i<tot_num_elem_conn; i++) {

    // Skip polygon break entries 
    if (global_elem_conn[i] == MESH_POLYBREAK_IND) continue;
    
    // Add node entiries to conversion list
    convert_list[num_node_conn].node_id=global_elem_conn[i];
    convert_list[num_node_conn].local_elem_conn_pos=i;
    num_node_conn++;
  }

  // Sort list by node_id, to make it easy to find unique node_ids
  std::sort(convert_list,convert_list+num_node_conn);

  // Count number of unique node ids in  convert_list
  int num_unique_node_ids=1;                 // There has to be at least 1, 
  int prev_node_id=convert_list[0].node_id;  // because we leave if < 1 above
  for (int i=1; i<num_node_conn; i++) {

    // If not the same as the last one count a new one
    if (convert_list[i].node_id != prev_node_id) {
      num_unique_node_ids++;
      prev_node_id=convert_list[i].node_id;
    }
  }

  // Allocate local elem conn
  local_elem_conn=new int[tot_num_elem_conn];

  // Set to polybreak value so that it's in the correct places
  // after the code below fills in the node connection values
  for (int i=0; i<tot_num_elem_conn; i++) {
    local_elem_conn[i]=MESH_POLYBREAK_IND;
  }
  
  // Translate convert_list to node_ids and local_elem_conn
  int node_ids_pos=0;                             // There has to be at least 1, 
  local_elem_conn[convert_list[0].local_elem_conn_pos]=node_ids_pos+1; // +1 to make base-1
  for (int i=1; i<num_node_conn; i++) {

    // Add an entry for this in local_elem_conn
    local_elem_conn[convert_list[i].local_elem_conn_pos]=node_ids_pos+1; // +1 to make base-1
  }


  // Get rid of conversion list
  delete [] convert_list;    
}

#endif // ifdef ESMF_GDAL

