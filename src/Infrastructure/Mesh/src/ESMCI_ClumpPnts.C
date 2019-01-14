// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_F90Interface.h"
#include "Mesh/include/Legacy/ESMCI_Exception.h"
#include "Mesh/include/ESMCI_OTree.h"
#include "Mesh/include/ESMCI_ClumpPnts.h"
#include "ESMCI_VM.h"
#include "ESMCI_CoordSys.h"

using std::cerr;
using std::endl;

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ClumpPnts()"

////
// More ESMF-y version of call with log error and return code
//
// Inputs:
//   num_pnt - the number of input points
//   pnt_lon - the longitudes in degrees of each point (array is of size num_pnt)
//   pnt_lat - the latitudes in degrees of each point (array is of size num_pnt)
//   tol     - the tolerence with which to clump points (measured in Cartesian space on a radius=1.0 sphere)
// Outputs:
//   pnt_cl_ind        - for each point, the index into _cl_lon and _cl_lat indicating which point it's clumped with
//                   (array is of size num_pnt)
//   num_cl       - the number of points after clumping
//   cl_lon       - the longitudes in degrees of each clump point (array is of size num_cl)
//   cl_lat       - the latitudes in degrees of each clump point (array is of size num_cl)
//   max_size_cl  - the maximum number of a points in a clump
//   rc            - ESMF return code
//
/////
void ClumpPnts(int num_pnt, double *pnt_lon, double *pnt_lat, double tol, int *pnt_cl_ind,
               int *num_cl, double **cl_lon, double **cl_lat, int *max_size_cl, int *rc) {

  // Initialize return code; assume routine not implemented
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;

  // Wrap ESMF LogError Error handling around internal exception-based implementation
  try {

     // Call into internal implementation
     clump_pnts(num_pnt, pnt_lon, pnt_lat, tol, pnt_cl_ind, num_cl, cl_lon, cl_lat, max_size_cl);

  } catch(std::exception &x) {
    // catch Mesh exception return code
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                    x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                    "UNKNOWN", ESMC_CONTEXT, rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                           "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}

struct SEARCH_DATA {
  double pnt[3];
  int closest_index;
  double closest_dist2;
};

struct PNT_DATA {
  double pnt[3];
  int orig_index;
  int clump_index;
};


int intersect_func(void *_pd, void *_sd) {

  // Dereference
  SEARCH_DATA *sd=(SEARCH_DATA *)_sd;
  PNT_DATA *pd=(PNT_DATA *)_pd;

  // See how close the points are
  double dist2=((sd->pnt[0]-pd->pnt[0])*(sd->pnt[0]-pd->pnt[0]))+
               ((sd->pnt[1]-pd->pnt[1])*(sd->pnt[1]-pd->pnt[1]))+
               ((sd->pnt[2]-pd->pnt[2])*(sd->pnt[2]-pd->pnt[2]));

  // If less than previous then set as the new
  if (dist2<sd->closest_dist2) {
    sd->closest_dist2=dist2;
    sd->closest_index=pd->orig_index;
  }

  return 0;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::clump_pnts()"

// Inputs:
//   num_pnt - the number of input points
//   pnt_lon - the longitudes in deg of each point (array is of size num_pnt)
//   pnt_lat - the latitudes in deg of each point (array is of size num_pnt)
//   tol     - the tolerence with which to clump points (measured in Cartesian space on a radius=1.0 sphere)
// Outputs:
//   pnt_cl_ind        - for each point, the index into _cl_lon and _cl_lat indicating which point it's clumped with
//                   (array is of size num_pnt)
//   _num_cl       - the number of points after clumping
//   _cl_lon       - the longitudes in deg of each point (array is of size num_cl)
//   _cl_lat       - the latitudes in deg of each point (array is of size num_cl)
//   _max_size_cl  - the maximum number of a points in a clump
void clump_pnts(int num_pnt, double *pnt_lon, double *pnt_lat, double tol, int *pnt_cl_ind,
                  int *_num_cl, double **_cl_lon, double **_cl_lat, int *_max_size_cl) {

  // Init outputs
  *_cl_lon=NULL;
  *_cl_lat=NULL;
  *_num_cl=0;
  *_max_size_cl=0;

  // If there are no points then nothing to do, so leave
  if (num_pnt <= 0) return;

  // Create Tree
  OTree *tree= new OTree(num_pnt);

  // Commit empty tree, more points will be add committed later
  tree->commit();

  // Create list of point structures
  PNT_DATA *pd_list=new PNT_DATA[num_pnt];

  // Loop and create point list
  for (int i=0; i<num_pnt; i++) {

    // Get lon, lat
    double sph_coord[2];
    sph_coord[0]=pnt_lon[i];
    sph_coord[1]=pnt_lat[i];

    // Convert to 3D
    double cart_coord[3];
    ESMCI_CoordSys_ConvertToCart(ESMC_COORDSYS_SPH_DEG, 2,
                                 sph_coord, cart_coord);
    // Get point structure
    PNT_DATA *pd=pd_list+i;

    // Set pnt data
    pd->pnt[0]=cart_coord[0];
    pd->pnt[1]=cart_coord[1];
    pd->pnt[2]=cart_coord[2];
    pd->orig_index=i;
    pd->clump_index=-1; // Init to bad value
  }

  // Scramble to reduce chance of degenerate trees
  std::random_shuffle(pd_list,pd_list+num_pnt);

  // Loop and clump points
  for (int i=0; i<num_pnt; i++) {

    // Get point structure
    PNT_DATA *pd=pd_list+i;

    // Set user data for seach
    SEARCH_DATA sd;
    sd.pnt[0]=pd->pnt[0];
    sd.pnt[1]=pd->pnt[1];
    sd.pnt[2]=pd->pnt[2];
    sd.closest_index=-1;
    sd.closest_dist2=tol*tol;

    // Expand to min-max box using tol
    double min[3],max[3];

    min[0]=pd->pnt[0]-tol;
    min[1]=pd->pnt[1]-tol;
    min[2]=pd->pnt[2]-tol;

    max[0]=pd->pnt[0]+tol;
    max[1]=pd->pnt[1]+tol;
    max[2]=pd->pnt[2]+tol;


    // See if point should be clumped with any in the tree
    // (First time through tree will be empty, but then runon()
    //  will just return and be a no-op.)
    tree->runon(min, max, intersect_func, (void *)(&sd));

    // If not clumped, then add to tree
    if (sd.closest_index==-1) {
      tree->add_commit(min, max, (void *)pd);
      pd->clump_index=pd->orig_index;
    } else {
      pd->clump_index=sd.closest_index;
    }
  }

  // Init pnt_cl_ind to maximum, so minimums can be calc. below
  for (int i=0; i<num_pnt; i++) {
    pnt_cl_ind[i]=num_pnt;
  }

  // Calc min index in each clump
  for (int i=0; i<num_pnt; i++) {
    PNT_DATA *pd=pd_list+i;
    if (pd->orig_index < pnt_cl_ind[pd->clump_index]) pnt_cl_ind[pd->clump_index]=pd->orig_index;
  }

  // Switch to min index in each clump
  // This is so output is closer to old way
  // of clumping points when creating from SCRIP grids
  for (int i=0; i<num_pnt; i++) {
    PNT_DATA *pd=pd_list+i;
    pd->clump_index=pnt_cl_ind[pd->clump_index];
  }

  //// Compress points down to just those needed ////

  // Init pnt_cl_ind to 0 to compress points down to just what is needed
  for (int i=0; i<num_pnt; i++) {
    pnt_cl_ind[i]=0;
  }

  // Here we are killing two birds with one stone:
  // + Marking the points that are used
  // + Counting the number of points in each clump
  //   in prep. to calculate the max clump size
  for (int i=0; i<num_pnt; i++) {
    PNT_DATA *pd=pd_list+i;

    pnt_cl_ind[pd->clump_index]=pnt_cl_ind[pd->clump_index]+1;
  }

  // Figure out the maximum clump size
  int max_size_cl=0;
  for (int i=0; i<num_pnt; i++) {
    if (pnt_cl_ind[i] > max_size_cl) max_size_cl=pnt_cl_ind[i];
  }

  // Figure out the collapsed indices and the total number
  // of clumps
  int num_cl=0;
  for (int i=0; i<num_pnt; i++) {
    if (pnt_cl_ind[i] > 0) {
      num_cl++;
    }
  }

  // Allocate collapsed coordinate lists
  double *cl_lon=new double [num_cl];
  double *cl_lat=new double [num_cl];

  // Copy coordinates into collapsed lists
  // and set the collapsed indices
  int j=0;
  for (int i=0; i<num_pnt; i++) {
    if (pnt_cl_ind[i] > 0) {
      // copy coordinate
      cl_lon[j]=pnt_lon[i];
      cl_lat[j]=pnt_lat[i];

      // set collapsed indices
      pnt_cl_ind[i]=j;

      // Next collapsed
      j++;
    }
  }


  // Switch to indices into collapsed clumps
  for (int i=0; i<num_pnt; i++) {
    PNT_DATA *pd=pd_list+i;
    pd->clump_index=pnt_cl_ind[pd->clump_index];
  }

  // Finally load clump indices into collapsed
  // point lists
  for (int i=0; i<num_pnt; i++) {
    PNT_DATA *pd=pd_list+i;
    pnt_cl_ind[pd->orig_index]=pd->clump_index;
  }

  // clean up point data
  if (pd_list) delete [] pd_list;

  // Clean up tree
  if (tree) delete tree;


  // Output
  // pnt_cl_ind is already filled
  *_cl_lon=cl_lon;
  *_cl_lat=cl_lat;
  *_num_cl=num_cl;
  *_max_size_cl=max_size_cl;
}



#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ClumpPntsLL()"

////
// More ESMF-y version of call with log error and return code
//
// Inputs:
//   num_pnt - the number of input points
//   pnt_lon - the longitudes in degrees of each point (array is of size num_pnt)
//   pnt_lat - the latitudes in degrees of each point (array is of size num_pnt)
//   tol     - the tolerence with which to clump points (measured in degrees)
// Outputs:
//   pnt_cl_ind        - for each point, the index into _cl_lon and _cl_lat indicating which point it's clumped with
//                   (array is of size num_pnt)
//   num_cl       - the number of points after clumping
//   cl_lon       - the longitudes in degrees of each clump point (array is of size num_cl)
//   cl_lat       - the latitudes in degrees of each clump point (array is of size num_cl)
//   max_size_cl  - the maximum number of a points in a clump
//   rc            - ESMF return code
//
/////
void ClumpPntsLL(int num_pnt, double *pnt_lon, double *pnt_lat, double tol, int *pnt_cl_ind,
                 int *num_cl, double **cl_lon, double **cl_lat, int *max_size_cl, double start_lat,
                 double end_lat, int *rc) {

  // Initialize return code; assume routine not implemented
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;

  // Wrap ESMF LogError Error handling around internal exception-based implementation
  try {

     // Call into internal implementation
    clump_pnts_ll(num_pnt, pnt_lon, pnt_lat, tol, pnt_cl_ind, num_cl, cl_lon, cl_lat, max_size_cl,start_lat,end_lat);

  } catch(std::exception &x) {
    // catch Mesh exception return code
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                    x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                    "UNKNOWN", ESMC_CONTEXT, rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                           "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}



struct SEARCH_DATA_LL {
  double ll[2];
  int closest_index;
  double closest_dist2;
};

struct PNT_DATA_LL {
  double ll[2];
  int orig_index;
  int clump_index;
};


int intersect_func_ll(void *_pd, void *_sd) {

  // Dereference
  SEARCH_DATA_LL *sd=(SEARCH_DATA_LL *)_sd;
  PNT_DATA_LL *pd=(PNT_DATA_LL *)_pd;

  // Calculate longitude difference
  double dist_lon=std::abs(sd->ll[0]-pd->ll[0]);

  // Handle case where points are bigger than 360.0 apart
  // take out all the 360's, could use fmod(), but
  // it sounds like it can be sensitive, so do via ints.
  if (dist_lon > 360.0) {
    int d=((int)dist_lon)/((int)360);
    dist_lon=dist_lon-360.0*((double)d);
  }

  // The furthest apart two points can be
  // in longitude is 180. If bigger than
  // than then measure the shorter way around
  if (dist_lon > 180.0) {
    dist_lon=360.0-dist_lon;
  }

  // See how close the points are
  double dist2=dist_lon*dist_lon+
              ((sd->ll[1]-pd->ll[1])*(sd->ll[1]-pd->ll[1]));

  // If less than previous then set as the new
  if (dist2<sd->closest_dist2) {
    sd->closest_dist2=dist2;
    sd->closest_index=pd->orig_index;
  }

  return 0;
}

// Inputs:
//   num_pnt - the number of input points
//   pnt_lon - the longitudes in deg of each point (array is of size num_pnt)
//   pnt_lat - the latitudes in deg of each point (array is of size num_pnt)
//   tol     - the tolerence with which to clump points (in degrees)
// Outputs:
//   pnt_cl_ind        - for each point, the index into _cl_lon and _cl_lat indicating which point it's clumped with
//                   (array is of size num_pnt)
//   _num_cl       - the number of points after clumping
//   _cl_lon       - the longitudes in deg of each point (array is of size num_cl)
//   _cl_lat       - the latitudes in deg of each point (array is of size num_cl)
//   _max_size_cl  - the maximum number of a points in a clump
void clump_pnts_ll(int num_pnt, double *pnt_lon, double *pnt_lat, double tol, int *_pnt_cl_ind,
                   int *_num_cl, double **_cl_lon, double **_cl_lat, int *_max_size_cl, double start_lat, double end_lat) {

  // Init outputs
  *_cl_lon=NULL;
  *_cl_lat=NULL;
  *_num_cl=0;
  *_max_size_cl=0;

  // If there are no points then nothing to do, so leave
  if (num_pnt <= 0) return;

  // Convert tolerence in degrees to one in distance on a radius 1.0 sphere
  // (using distance at the equator, this will be an overestimate for other places)
  const double ESMC_CoordSys_Deg2Rad= 0.01745329251994329547437;
  double tol_3D=tol*ESMC_CoordSys_Deg2Rad;

  // Create list of point structures
  PNT_DATA_LL *pd_list1=new PNT_DATA_LL[num_pnt];

  // Loop and create point list
  int ii=0;
  for (int i=0; i<num_pnt; i++) {

    if ((pnt_lat[i]>= start_lat) && (pnt_lat[i]< end_lat)) {
      // Get point structure
      PNT_DATA_LL *pd=pd_list1+ii;
      // Set pnt data
      pd->ll[0]=pnt_lon[i];
      pd->ll[1]=pnt_lat[i];
      pd->orig_index=i;
      pd->clump_index=-1; // Init to bad value
      ii++;
    }
  }
  PNT_DATA_LL *pd_list;
  int num_pnt_total = num_pnt;
  if (ii < num_pnt) {
    pd_list=new PNT_DATA_LL[ii];
    memcpy(pd_list, pd_list1, sizeof(PNT_DATA_LL)*ii);
    delete [] pd_list1;
    num_pnt = ii;
  } else {
    pd_list = pd_list1;
  }

  int  *orig_ind = new int[num_pnt];
  for (int i=0; i<num_pnt; i++) {
    orig_ind[i]=pd_list[i].orig_index;
    pd_list[i].orig_index = i;
  }

  // Create Tree
  ESMCI::OTree *tree= new ESMCI::OTree(num_pnt);

  // Commit empty tree, more points will be add committed later
  tree->commit();

  // Scramble to reduce chance of degenerate trees
  std::random_shuffle(pd_list,pd_list+num_pnt);

  // Loop and clump points
  for (int i=0; i<num_pnt; i++) {

    // Get point structure
    PNT_DATA_LL *pd=pd_list+i;


    // Convert to 3D
    // CHANGE THIS TO USE STANDARD CONVERSION METHOD
    // Get lon, lat
    double lon=pd->ll[0];
    double lat=pd->ll[1];

    // Calculate 3D Cartesian point
    double pnt_3D[3];
    const double ninety = 90.0;
    double theta = lon*ESMC_CoordSys_Deg2Rad;
    double phi   = (ninety-lat)*ESMC_CoordSys_Deg2Rad;
    pnt_3D[0] = std::cos(theta)*std::sin(phi);
    pnt_3D[1] = std::sin(theta)*std::sin(phi);
    pnt_3D[2] = std::cos(phi);

    // Expand to min-max box using tol
    double min[3],max[3];

    min[0]=pnt_3D[0]-tol_3D;
    min[1]=pnt_3D[1]-tol_3D;
    min[2]=pnt_3D[2]-tol_3D;

    max[0]=pnt_3D[0]+tol_3D;
    max[1]=pnt_3D[1]+tol_3D;
    max[2]=pnt_3D[2]+tol_3D;


    // Set user data for seach
    SEARCH_DATA_LL sd;
    sd.ll[0]=pd->ll[0];
    sd.ll[1]=pd->ll[1];
    sd.closest_index=-1;
    sd.closest_dist2=tol*tol;

    // See if point should be clumped with any in the tree
    // (First time through tree will be empty, but then runon()
    //  will just return and be a no-op.)
    tree->runon(min, max, intersect_func_ll, (void *)(&sd));

    // If not clumped, then add to tree
    if (sd.closest_index==-1) {
      tree->add_commit(min, max, (void *)pd);
      pd->clump_index=pd->orig_index;
    } else {
      pd->clump_index=sd.closest_index;
    }
  }

  int *pnt_cl_ind = new int[num_pnt];
  // Init pnt_cl_ind to maximum, so minimums can be calc. below
  for (int i=0; i<num_pnt; i++) {
    pnt_cl_ind[i]=num_pnt;
  }

  // Calc min index in each clump
  for (int i=0; i<num_pnt; i++) {
    PNT_DATA_LL *pd=pd_list+i;
    if (pd->orig_index < pnt_cl_ind[pd->clump_index]) pnt_cl_ind[pd->clump_index]=pd->orig_index;
  }

  // Switch to min index in each clump
  // This is so output is closer to old way
  // of clumping points when creating from SCRIP grids
  for (int i=0; i<num_pnt; i++) {
    PNT_DATA_LL *pd=pd_list+i;
    pd->clump_index=pnt_cl_ind[pd->clump_index];
  }

  //// Compress points down to just those needed ////

  // Init pnt_cl_ind to 0 to compress points down to just what is needed
  for (int i=0; i<num_pnt; i++) {
    pnt_cl_ind[i]=0;
  }

  // Here we are killing two birds with one stone:
  // + Marking the points that are used
  // + Counting the number of points in each clump
  //   in prep. to calculate the max clump size
  for (int i=0; i<num_pnt; i++) {
    PNT_DATA_LL *pd=pd_list+i;

    pnt_cl_ind[pd->clump_index]=pnt_cl_ind[pd->clump_index]+1;
  }

  // Figure out the maximum clump size
  int max_size_cl=0;
  for (int i=0; i<num_pnt; i++) {
    if (pnt_cl_ind[i] > max_size_cl) max_size_cl=pnt_cl_ind[i];
  }

  // Figure out the collapsed indices and the total number
  // of clumps
  int num_cl=0;
  for (int i=0; i<num_pnt; i++) {
    if (pnt_cl_ind[i] > 0) {
      num_cl++;
    }
  }

  // Allocate collapsed coordinate lists
  double *cl_lon=new double [num_cl];
  double *cl_lat=new double [num_cl];

  // Copy coordinates into collapsed lists
  // and set the collapsed indices
  int j=0;
  for (int i=0; i<num_pnt; i++) {
    if (pnt_cl_ind[i] > 0) {
      // copy coordinate
      cl_lon[j]=pnt_lon[orig_ind[i]];
      cl_lat[j]=pnt_lat[orig_ind[i]];

      // set collapsed indices
      pnt_cl_ind[i]=j;

      // Next collapsed
      j++;
    }
  }

  // Switch to indices into collapsed clumps
  for (int i=0; i<num_pnt; i++) {
    PNT_DATA_LL *pd=pd_list+i;
    pd->clump_index=pnt_cl_ind[pd->clump_index];
  }

  // Finally load clump indices into collapsed
  // point lists
  for (int i=0; i<num_pnt; i++) {
    PNT_DATA_LL *pd=pd_list+i;
    pnt_cl_ind[pd->orig_index]=pd->clump_index;
  }
  if (num_pnt_total > num_pnt) {
    for (int i=0; i<num_pnt_total; i++) {
      _pnt_cl_ind[i]=-1;
    }
    for (int i=0; i<num_pnt; i++) {
      _pnt_cl_ind[orig_ind[i]]=pnt_cl_ind[i];
    }
  } else {
    for (int i=0; i<num_pnt; i++) {
      _pnt_cl_ind[i]=pnt_cl_ind[i];
    }
  }
  // clean up point data
  if (pd_list) delete [] pd_list;
  delete [] pnt_cl_ind;
  delete [] orig_ind;

  // Clean up tree
  if (tree) delete tree;


  // Output
  // pnt_cl_ind is already filled
  *_cl_lon=cl_lon;
  *_cl_lat=cl_lat;
  *_num_cl=num_cl;
  *_max_size_cl=max_size_cl;
}

} // namespace ESMCI
