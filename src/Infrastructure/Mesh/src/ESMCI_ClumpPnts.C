// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2014, University Corporation for Atmospheric Research,
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
#include "ESMCI_Exception.h"
#include "Mesh/include/ESMCI_PntClumper.h"
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
  ESMCI::OTree *tree= new ESMCI::OTree(num_pnt);

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
  bool first_time=true;
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
    if (!first_time) {
      tree->runon(min, max, intersect_func, (void *)(&sd));
    } else {
      tree->add_commit(min, max, (void *)pd);    
      first_time=false;
    }

    // If not clumped, then add to tree
    // TODO: ADD add_commit to OTree
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

} // namespace ESMCI
