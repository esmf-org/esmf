// $Id$
//
// Earth System Modeling Framework
// Copyright (c) 2002-2025, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/Regridding/ESMCI_Search.h>
#include <Mesh/include/Regridding/ESMCI_SpaceDir.h>
#include <Mesh/include/ESMCI_OTree.h>
#include <Mesh/include/ESMCI_RegridConstants.h>
#include <Mesh/include/ESMCI_MathUtil.h>

#include <Mesh/include/Legacy/ESMCI_ParEnv.h>
// #include <Mesh/include/Legacy/ESMCI_BBox.h>

#include "PointList/include/ESMCI_PointList.h"


#include <iostream>
#include <fstream>
#include <algorithm>
#include <iterator>

#include <ostream>

#include <set>

#include <limits>
#include <vector>

using std::vector;


//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {


bool sn_debug=false;

struct SearchData {
  int sdim;
  double dst_pnt[3];

  double closest_dist2;  // closest distance squared
  double closest_coord[3];

  int closest_src_id;
  const PointList *srcpointlist;
};

#define SN_BAD_ID -1

  static int nearest_func(void *n, void *y, double *min, double *max) {

    point *this_pt = static_cast<point*>(n);

    SearchData *sd = static_cast<SearchData*>(y);

    // Get source node coords
    const double *c=this_pt->coords;

    // Convert to 3D point
    double src_pnt[3];
    src_pnt[0] = c[0];
    src_pnt[1] = c[1];
    src_pnt[2] = (sd->sdim == 3 ? c[2] : 0.0);


    // Calculate squared distance
    double dist2=(sd->dst_pnt[0]-src_pnt[0])*(sd->dst_pnt[0]-src_pnt[0])+
      (sd->dst_pnt[1]-src_pnt[1])*(sd->dst_pnt[1]-src_pnt[1])+
      (sd->dst_pnt[2]-src_pnt[2])*(sd->dst_pnt[2]-src_pnt[2]);

    // If this node is closer then make it the closest node
    if (dist2 < sd->closest_dist2) {
      sd->closest_src_id=this_pt->id;
      sd->closest_dist2=dist2;

      // compute a new min-max box
      double dist=sqrt(dist2);

      min[0]=sd->dst_pnt[0]-dist;
      min[1]=sd->dst_pnt[1]-dist;
      min[2]=sd->dst_pnt[2]-dist;

      max[0]=sd->dst_pnt[0]+dist;
      max[1]=sd->dst_pnt[1]+dist;
      max[2]=sd->dst_pnt[2]+dist;

      sd->closest_coord[0]=src_pnt[0];
      sd->closest_coord[1]=src_pnt[1];
      sd->closest_coord[2]=src_pnt[2];

    } else if (dist2 == sd->closest_dist2) {

      // If there wasn't a closer point or if the new point has a smaller id, 
      // then use the new point instead
      if ((sd->closest_src_id == SN_BAD_ID) ||
          (this_pt->id < sd->closest_src_id))  {
        
        // Change to new id
        sd->closest_src_id=this_pt->id;
        
        // Change to new point
        sd->closest_coord[0]=src_pnt[0];
        sd->closest_coord[1]=src_pnt[1];
        sd->closest_coord[2]=src_pnt[2];

        // Don't need to change the closest_dist2  because at exactly the same dist   
        
        // Don't need to adjust the min-max box because at exactly the same dist
      }
    }

    // Don't know if this is the closest, so search further
    return 0;
  }



// The main routine
  void SearchNearestSrcToDst(const PointList &src_pl, const PointList &dst_pl, int unmappedaction, SearchResult &result, bool set_dst_status, WMat &dst_status) {
  Trace __trace("SearchNearestSrcToDst(PointList &src_pl, PointList &dst_pl, int unmappedaction, SearchResult &result)");


  // Get spatial dim and make sure both have the same
  UInt sdim=src_pl.get_coord_dim();
  if (sdim != dst_pl.get_coord_dim()) {
    Throw() << "src and dst must have same spatial dim for search";
  }

  int num_nodes_to_search=src_pl.get_curr_num_pts();

  // Create search tree
  OTree *tree=new OTree(num_nodes_to_search);

  // Add unmasked nodes to search tree
  double pnt[3];
  for (UInt p = 0; p < num_nodes_to_search; ++p) {

    const point *point_ptr=src_pl.get_point(p);

    // Set coord value in 3D point
    pnt[0] = point_ptr->coords[0];
    pnt[1] = point_ptr->coords[1];
    pnt[2] = sdim == 3 ? point_ptr->coords[2] : 0.0;

    tree->add(pnt, pnt, (void*)point_ptr);
  }

  // Commit tree
  tree->commit();


  // Get big/small numbers
  //// Use sqrt, so if it's squared it doesn't overflow
  double huge=sqrt(std::numeric_limits<double>::max());
  double min=-huge;
  double max=huge;


  // Set initial search box to the largest possible
  double pmin[3],pmax[3];
  pmin[0]=min;
  pmin[1]=min;
  pmin[2]=min;
  pmax[0]=max;
  pmax[1]=max;
  pmax[2]=max;

  // Setup search structure
  SearchData sd;
  sd.sdim=sdim;
  // sd.closest_src_node=NULL;
  sd.closest_dist2=huge;
  // sd.src_coord=NULL;
  sd.closest_src_id=SN_BAD_ID;
  sd.srcpointlist=&src_pl;

  // Loop the destination points, find hosts.
  int dst_size=dst_pl.get_curr_num_pts();
  for (UInt p = 0; p < dst_size; ++p) {

    const double *pnt_crd=dst_pl.get_coord_ptr(p);
    int pnt_id=dst_pl.get_id(p);

    // Set dst point coords in search structure
    sd.dst_pnt[0] = pnt_crd[0];
    sd.dst_pnt[1] = pnt_crd[1];
    sd.dst_pnt[2] = (sdim == 3 ? pnt_crd[2] : 0.0);

    // Note we're not resetting the closest src info because starting with last
    // nearest point as first quess

    // If a closest point exists from the last loop then use as initial guess
    if (sd.closest_src_id != SN_BAD_ID) {
    // Calculate distance squared
      double dist2=(sd.dst_pnt[0]-sd.closest_coord[0])*(sd.dst_pnt[0]-sd.closest_coord[0])+
                   (sd.dst_pnt[1]-sd.closest_coord[1])*(sd.dst_pnt[1]-sd.closest_coord[1])+
                   (sd.dst_pnt[2]-sd.closest_coord[2])*(sd.dst_pnt[2]-sd.closest_coord[2]);

      // set closest dist squared
      sd.closest_dist2=dist2;

      // Calc new search box
      double dist=sqrt(dist2);

      pmin[0]=sd.dst_pnt[0]-dist;
      pmin[1]=sd.dst_pnt[1]-dist;
      pmin[2]=sd.dst_pnt[2]-dist;

      pmax[0]=sd.dst_pnt[0]+dist;
      pmax[1]=sd.dst_pnt[1]+dist;
      pmax[2]=sd.dst_pnt[2]+dist;
    }


    // Find closest source node to this destination node
    tree->runon_mm_chng(pmin, pmax, nearest_func, (void *)&sd);

    // If we've found a nearest source point, then add to the search results list...
    if (sd.closest_src_id != SN_BAD_ID) {
      Search_result *sr=new Search_result();
      sr->dst_gid=pnt_id;
      sr->src_gid=sd.closest_src_id;
      result.push_back(sr);

      // If necessary, set dst status
      if (set_dst_status) {
        // Set col info
        WMat::Entry col(ESMC_REGRID_STATUS_MAPPED,
                        0, 0.0, 0);

        // Set row info
        WMat::Entry row(pnt_id, 0, 0.0, 0);

        // Put weights into weight matrix
        dst_status.InsertRowMergeSingle(row, col);
      }

    } else { // ...otherwise deal with the unmapped point
      // If necessary, set dst status
      if (set_dst_status) {
        // Set col info
        WMat::Entry col(ESMC_REGRID_STATUS_OUTSIDE,
                        0, 0.0, 0);

        // Set row info
        WMat::Entry row(pnt_id, 0, 0.0, 0);

        // Put weights into weight matrix
        dst_status.InsertRowMergeSingle(row, col);
      }

      if (unmappedaction == ESMCI_UNMAPPEDACTION_ERROR) {
        Throw() << " Some destination points cannot be mapped to the source grid";
      } else if (unmappedaction == ESMCI_UNMAPPEDACTION_IGNORE) {
        // don't do anything
      } else {
        Throw() << " Unknown unmappedaction option";
      }
    }

  } // for dst nodes


  // Get rid of tree
  if (tree) delete tree;
  }



  void create_OTree_from_plist(const PointList *plist, OTree *&otree) {

    // Get dimension
    int sdim=plist->get_coord_dim();
    
    // Get size point list
   int plist_num_pnts = plist->get_curr_num_pts();

    // Create search tree
    otree=new OTree(plist_num_pnts);

    // Add points to search tree
    for (UInt p = 0; p < plist_num_pnts; ++p) {
      
      const point *point_ptr=plist->get_point(p);
      
      // Set coord value in 3D point
      double pnt[3];
      pnt[0] = point_ptr->coords[0];
      pnt[1] = point_ptr->coords[1];
      pnt[2] = sdim == 3 ? point_ptr->coords[2] : 0.0;
      
      otree->add(pnt, pnt, (void*)point_ptr);
    }
    
    // Commit tree
    otree->commit();
  }


  
void build_mig_plist(const PointList *plist,
                      std::vector<int> &plist_snd_loc, std::vector<int> &plist_snd_pet, 
                      PointList *&new_plist) {
  
  // Make sure plist vectors are of the same size
  ThrowRequire(plist_snd_loc.size() == plist_snd_pet.size());

  // Get parallel information
  int num_procs=Par::Size();
  int myrank=Par::Rank();

  // Get coordinate dimension
  int sdim=plist->get_coord_dim();
  
  // Calculate how many we're sending to each PET
  std::vector<int> pet_counts;
  pet_counts.resize(num_procs,0);
  for (const int &pet: plist_snd_pet) {
    pet_counts[pet]++;
  }
  
  // Construct communication pattern information
  int snd_size=(sdim+1)*sizeof(double);
  int num_snd_procs=0;
  std::vector<int> snd_pets;
  std::vector<int> snd_sizes;
  std::vector<int> snd_counts;
  for (int i=0; i<num_procs; i++) {
    if (pet_counts[i] > 0) {
      num_snd_procs++;
      snd_pets.push_back(i);
      snd_sizes.push_back(pet_counts[i]*snd_size);
      snd_counts.push_back(pet_counts[i]);
    }
  }

  // Setup pattern and sizes
  SparseMsg comm;
  if (num_snd_procs >0) {
    comm.setPattern(num_snd_procs, (const UInt *)&(snd_pets[0]));
    comm.setSizes((UInt *)&(snd_sizes[0]));
  } else {
    comm.setPattern(0, (const UInt *)NULL);
    comm.setSizes((UInt *)NULL);
  }

  // Reset buffers
  comm.resetBuffers();

  // Pack points into buffers
  for (int i=0; i < plist_snd_pet.size(); i++) {

    // Get which loc and where it's to be sent
    int loc=plist_snd_loc[i];
    int pet=plist_snd_pet[i];

    // Get buffer
    SparseMsg:: buffer *b=comm.getSendBuffer(pet);

    // Get point coords and id
    const double *pnt_coords = plist->get_coord_ptr(loc);
    int pnt_id = plist->get_id(loc);

    // pack buf
    double buf[4]; // 4 is biggest this should be (i.e. 3D+id)
    buf[0]=pnt_coords[0];
    buf[1]=pnt_coords[1];
    if (sdim < 3)
      buf[2]=pnt_id;  //passing an int through a double...inefficient but ok?
    else {
      buf[2]=pnt_coords[2];
      buf[3]=pnt_id;  //passing an int through a double...inefficient but ok?
    }

    // Push buf onto send struct
    b->push((const UChar *)buf, (UInt)snd_size);
  }

  // Communicate point information
  comm.communicate();

  // Calculate the number of points received
  int num_rcv_pnts=0;
  for (std::vector<UInt>::iterator p = comm.inProc_begin(); p != comm.inProc_end(); ++p) {
    UInt proc = *p;
    SparseMsg::buffer *b = comm.getRecvBuffer(proc);

    // num messages
    int num_pnts_this_buf = b->msg_size()/snd_size;

    // Add it to total
    num_rcv_pnts += num_pnts_this_buf;    
  }
  

  // Create new PointList 
  new_plist = new ESMCI::PointList(num_rcv_pnts,sdim);

  // Unpack recv'd information and add to pointlist
  for (std::vector<UInt>::iterator p = comm.inProc_begin(); p != comm.inProc_end(); ++p) {
    UInt pet = *p;
    SparseMsg::buffer *b = comm.getRecvBuffer(pet);
    
    // Unpack everything from this processor
    while (!b->empty()) {
      double buf[4]; // 4 is biggest this should be
      
      // Get information
      b->pop((UChar *)buf, (UInt)snd_size);
      
      // Unpack buf
      double pnt_coords[3]={0.0,0.0,0.0};
      int pnt_id;      
      pnt_coords[0]=buf[0];
      pnt_coords[1]=buf[1];
      if (sdim < 3) {
        pnt_id=(int)buf[2];
      } else {
        pnt_coords[2]=buf[2];
        pnt_id=(int)buf[3];
      }

      // Add to pointlist
      new_plist->add(pnt_id,pnt_coords);      
    }   
  }
  
}


  /// Create a new point list such that any points that lie within local_min & local_max are
  /// on this proc (points may repeat)
  void create_plist_using_minmax(const PointList *plist,
                               double *local_min, double *local_max,
                               PointList *&new_plist) {

  // Get dimension
  int sdim=plist->get_coord_dim();
  
  // Create SpaceDir
  SpaceDir *spacedir=new SpaceDir(local_min, local_max, NULL, true);

  // Map src points to destination procs by min-max box
  std::vector<int> pet_list;
  std::vector<int> plist_snd_loc, plist_snd_pet;
  for (UInt loc = 0; loc < plist->get_curr_num_pts(); ++loc) {

    const point *pnt_ptr=plist->get_point(loc);
    
    // Set coord value in 3D point
    double src_pnt[3];
    src_pnt[0] = pnt_ptr->coords[0];
    src_pnt[1] = pnt_ptr->coords[1];
    src_pnt[2] = sdim == 3 ? pnt_ptr->coords[2] : 0.0;

    // Expand by a tiny tol to give a small box
    double src_pnt_min[3], src_pnt_max[3];
    MU_SUB_SCALAR_VEC3D(src_pnt_min,src_pnt,1.0E-10);
    MU_ADD_SCALAR_VEC3D(src_pnt_max,src_pnt,1.0E-10);
    
    // Init vector
    pet_list.clear();

    // Get PETs where src_pnt falls in dst min/max box
    spacedir->get_procs(src_pnt_min, src_pnt_max, &(pet_list));

    // Loop adding pets and loc to migration lists
    for (const int &pet: pet_list) {
      plist_snd_pet.push_back(pet);
      plist_snd_loc.push_back(loc);    
    } 
  }

  // Get rid of first space tree
  delete spacedir;
 
  // Build srcplist_local
  build_mig_plist(plist,
                  plist_snd_loc, plist_snd_pet, 
                  new_plist);
}


  double calc_dist2_between_mm(double *mm1, double *mm2) {

    // Dim distance
    double ddim=0.0;
    double tot_dist2=0.0;
    
    //// Dim 0
    
    // If min1 is lower than min2
    if (mm1[0] < mm2[0]) {

      // If max1 is lower than min2
      if (mm1[3] < mm2[0]) {
        ddim=mm2[0]-mm1[3];
      } else {
        ddim=0.0;
      }      
    } else {
      // If min1 is lower than max2
      if (mm1[0] < mm2[3]) {
        ddim=0.0;
      } else {
        ddim=mm1[0]-mm2[3];
      }      
    }

    tot_dist2 += ddim*ddim;

    //// Dim 1
    
    // If min1 is lower than min2
    if (mm1[1] < mm2[1]) {

      // If max1 is lower than min2
      if (mm1[4] < mm2[1]) {
        ddim=mm2[1]-mm1[4];
      } else {
        ddim=0.0;
      }      
    } else {
      // If min1 is lower than max2
      if (mm1[1] < mm2[4]) {
        ddim=0.0;
      } else {
        ddim=mm1[1]-mm2[4];
      }      
    }

    tot_dist2 += ddim*ddim;

    //// Dim 2
    
    // If min1 is lower than min2
    if (mm1[2] < mm2[2]) {

      // If max1 is lower than min2
      if (mm1[5] < mm2[2]) {
        ddim=mm2[2]-mm1[5];
      } else {
        ddim=0.0;
      }      
    } else {
      // If min1 is lower than max2
      if (mm1[2] < mm2[5]) {
        ddim=0.0;
      } else {
        ddim=mm1[2]-mm2[5];
      }      
    }

    tot_dist2 += ddim*ddim;

    // Output dist2
    return tot_dist2;        
  }
  


  /// Create a new point list of points closest to each minmax
  void create_plist_closest_to_minmax(const PointList *plist,
                               double *local_min, double *local_max,
                               PointList *&new_plist) {

  // Get dimension
  int sdim=plist->get_coord_dim();

  // Get huge values to use to init min and max   
  double huge=sqrt(std::numeric_limits<double>::max()); //// Use sqrt, so if it's squared it doesn't overflow
  double neg_huge=-huge;
  

   // Distribute min-max box of each proc
   // Pack up local minmax
   vector<double> local_minmax(6,0.0);
   local_minmax[0]=local_min[0];
   local_minmax[1]=local_min[1];
   if (sdim > 2) local_minmax[2]=local_min[2];
   local_minmax[3]=local_max[0];
   local_minmax[4]=local_max[1];
   if (sdim > 2) local_minmax[5]=local_max[2];

   
   // Number of procs
   int num_procs = Par::Size();
   if (num_procs <1)     Throw() << "Error: number of procs is less than 1";

   // Allocate buffer for global minmax list
   vector<double> dst_minmax(6*num_procs, 0.0);

   // Do all gather to get info from all procs
   MPI_Allgather(&local_minmax[0], 6, MPI_DOUBLE, &dst_minmax[0], 6, MPI_DOUBLE, Par::Comm());


  // Get min/max of plist
  ////// Get local min/max of plist
  double plist_pnt[3]={0.0,0.0,0.0};
  double plist_min[3]={huge,huge,huge};
  double plist_max[3]={neg_huge,neg_huge,neg_huge};

  // Loop calculating local min/max
  for (UInt p = 0; p < plist->get_curr_num_pts(); ++p) {

    const point *pnt_ptr=plist->get_point(p);

    // Set coord value in 3D point
    plist_pnt[0] = pnt_ptr->coords[0];
    plist_pnt[1] = pnt_ptr->coords[1];
    plist_pnt[2] = sdim == 3 ? pnt_ptr->coords[2] : 0.0;

    // compute proc min max
    MU_SET_MIN_VEC3D(plist_min,plist_pnt);
    MU_SET_MAX_VEC3D(plist_max,plist_pnt);
  }

  // Set plist min and max in buffer
  local_minmax[0]=plist_min[0];
  local_minmax[1]=plist_min[1];
  if (sdim > 2) local_minmax[2]=plist_min[2];
  local_minmax[3]=plist_max[0];
  local_minmax[4]=plist_max[1];
  if (sdim > 2) local_minmax[5]=plist_max[2];
  
  // Allocate buffer for global minmax list
  vector<double> plist_minmax(6*num_procs, 0.0);
  
  // Do all gather to get info from all procs
  MPI_Allgather(&local_minmax[0], 6, MPI_DOUBLE, &plist_minmax[0], 6, MPI_DOUBLE, Par::Comm());
  
  // Loop figuring out if I'm the closest
  std::vector<int> plist_snd_loc, plist_snd_pet;
  for (int dp=0; dp<num_procs; dp++) {

    // get minmax
    double *dp_minmax=(&dst_minmax[0])+6*dp;
    
    // Init variables
    double min_dist2=huge;
    int min_proc=-1;
    
    for (int pp=0; pp<num_procs; pp++) {

      // get minmax
      double *pp_minmax=(&plist_minmax[0])+6*pp;

      // get dist
      double dist2=calc_dist2_between_mm(pp_minmax, dp_minmax);      

      // If less than min, then save
      if (dist2 < min_dist2) {
        min_dist2=dist2;
        min_proc=pp;
      }
    }

    // If I'm the closest, then add a point to the list
    if (min_proc == Par::Rank()) {
      plist_snd_pet.push_back(dp);
      plist_snd_loc.push_back(0);    // Just send 0 for now
    }
  }
 
  // Build new plist
  build_mig_plist(plist,
                  plist_snd_loc, plist_snd_pet, 
                  new_plist);
}
  
  
struct CommDataOpt {
  double closest_dist2;
  int closest_src_gid;
  int proc;
};

  /* XMRKX */
  void ParSearchNearestSrcToDstOpt(const PointList &src_pl_ref, const PointList &dst_pl_ref, int unmappedaction, SearchResult &result, bool set_dst_status, WMat &dst_status) {
    Trace __trace("ParSearchNearestSrcToDst(const PointList &src_pl, const PointList &dst_pl, int unmappedaction, SearchResult &result)");
  //int FindPnts(const Mesh &mesh, int unmappedaction, int dim_pnts, int num_pnts, double *pnts, int *procs, int *gids) {
  //  Trace __trace("FindPnts()");


    printf("%d# Opt Beg\n",Par::Rank());
    
  // Get pointers to keep things consistent until we switch everything to refs
  const PointList *src_pl=&src_pl_ref;
  const PointList *dst_pl=&dst_pl_ref;

    
  // Get spatial dim and make sure both have the same
  int sdim=src_pl->get_coord_dim();
  if (sdim != dst_pl->get_coord_dim()) {
    Throw() << "src and dst must have same spatial dim for search";
  }

  // Get huge values to use to init min and max   
  double huge=sqrt(std::numeric_limits<double>::max()); //// Use sqrt, so if it's squared it doesn't overflow
  double neg_huge=-huge;
  
  // Get size of dst
  int dst_pl_num_pnts = dst_pl->get_curr_num_pts();

  // Allocate space to hold information for dst points
  vector<int> dst_closest_src_gid(dst_pl_num_pnts,SN_BAD_ID);
  vector<double> dst_closest_dist2(dst_pl_num_pnts,huge);
  vector<int> dst_done(dst_pl_num_pnts,0);

  // Get min/max of dst
  ////// Get local min/max of dst
  double dst_pnt[3]={0.0,0.0,0.0};
  double dst_min[3]={huge,huge,huge};
  double dst_max[3]={neg_huge,neg_huge,neg_huge};

  // Loop calculating local min/max
  for (UInt p = 0; p < dst_pl->get_curr_num_pts(); ++p) {

    const point *pnt_ptr=dst_pl->get_point(p);

    // Set coord value in 3D point
    dst_pnt[0] = pnt_ptr->coords[0];
    dst_pnt[1] = pnt_ptr->coords[1];
    dst_pnt[2] = sdim == 3 ? pnt_ptr->coords[2] : 0.0;

    // compute proc min max
    MU_SET_MIN_VEC3D(dst_min,dst_pnt);
    MU_SET_MAX_VEC3D(dst_max,dst_pnt);
  }


  // Expand box by a factor in each direction
  double factor=0.1; // Expand by 10%
  double d;
  d=dst_max[0]-dst_min[0];
  dst_min[0]=dst_min[0]-d*factor;
  dst_max[0]=dst_max[0]+d*factor;
  d=dst_max[1]-dst_min[1];
  dst_min[1]=dst_min[1]-d*factor;
  dst_max[1]=dst_max[1]+d*factor;
  d=dst_max[2]-dst_min[2];
  dst_min[2]=dst_min[2]-d*factor;
  dst_max[2]=dst_max[2]+d*factor;
  
  

  // Create a new pointlist (src_pl_local) where
  // any point that lies within dst_min & dst_max is on this PET
  PointList *src_pl_local;
  create_plist_using_minmax(src_pl,
                            dst_min, dst_max,
                            src_pl_local);
  

#if 0
  // Check if we don't have any local points
  if (src_pl_local->get_curr_num_pts() == 0) {
    printf("BOB: WARNING proc %d has no src_pl_local points!!!\n",Par::Rank());
  }
#endif 
  
  //// First search tree of src points in local proximity ////

  // Create tree
  OTree *src_pl_local_tree;
  create_OTree_from_plist(src_pl_local, src_pl_local_tree);

 
  // Setup search structure
  SearchData sd;
  sd.sdim=sdim;
  // sd.closest_src_node=NULL;
  sd.closest_dist2=huge;
  // sd.src_coord=NULL;
  sd.closest_src_id=SN_BAD_ID;
  sd.srcpointlist=src_pl_local;

  // Loop the destination points, find hosts.
  for (UInt p = 0; p < dst_pl_num_pnts; ++p) {

    const double *pnt_crd=dst_pl->get_coord_ptr(p);

    // Set dst point coords in search structure
    sd.dst_pnt[0] = pnt_crd[0];
    sd.dst_pnt[1] = pnt_crd[1];
    sd.dst_pnt[2] = (sdim == 3 ? pnt_crd[2] : 0.0);

    // Init search box to max possible and then maybe change below
    double pmin[3]={neg_huge,neg_huge,neg_huge};
    double pmax[3]={huge,huge,huge};
    
    // If a closest point exists from the last loop then use as initial guess
    if (sd.closest_src_id != SN_BAD_ID) {
    // Calculate distance
      double dist2=(sd.dst_pnt[0]-sd.closest_coord[0])*(sd.dst_pnt[0]-sd.closest_coord[0])+
                   (sd.dst_pnt[1]-sd.closest_coord[1])*(sd.dst_pnt[1]-sd.closest_coord[1])+
                   (sd.dst_pnt[2]-sd.closest_coord[2])*(sd.dst_pnt[2]-sd.closest_coord[2]);

      // set closest dist squared
      sd.closest_dist2=dist2;
      // The sd.closest_src_id is still set from last time
      
      // Calc new search box
      double dist=sqrt(dist2);

      pmin[0]=sd.dst_pnt[0]-dist;
      pmin[1]=sd.dst_pnt[1]-dist;
      pmin[2]=sd.dst_pnt[2]-dist;

      pmax[0]=sd.dst_pnt[0]+dist;
      pmax[1]=sd.dst_pnt[1]+dist;
      pmax[2]=sd.dst_pnt[2]+dist;
    }

    // Find closest source node to this destination node
    src_pl_local_tree->runon_mm_chng(pmin, pmax, nearest_func, (void *)&sd);

    // If we've found a nearest source point, then add to the search results list...
    if (sd.closest_src_id != SN_BAD_ID) {
      dst_closest_src_gid[p]=sd.closest_src_id;
      dst_closest_dist2[p]=sd.closest_dist2;      
    }
  }

  // Get rid of local tree
  delete src_pl_local_tree;
  
  
  // For debugging see if there are any unmapped points
  bool has_unmapped=false;
  for (UInt p = 0; p < dst_pl_num_pnts; ++p) {
    if (dst_closest_src_gid[p] == SN_BAD_ID) {
      has_unmapped=true;
    }
  }

  if (has_unmapped) {
    printf("%d# has unmapped dst_min=%f %f %f dst_max=%f %f %f\n",Par::Rank(),MU_LST_VEC3D(dst_min),MU_LST_VEC3D(dst_max));
  }


  // Get a nearest point, just to cover our bases
  PointList *src_pl_near;
  create_plist_closest_to_minmax(src_pl,
                                 dst_min, dst_max,
                                 src_pl_near);


  // Use this point to update dst information
  if (src_pl_near->get_curr_num_pts() > 0) {

    // Get point
    const double *near_pnt_crd=src_pl_near->get_coord_ptr(0);
    double near_pnt[3];
    near_pnt[0] = near_pnt_crd[0];
    near_pnt[1] = near_pnt_crd[1];
    near_pnt[2] = (sdim == 3 ? near_pnt_crd[2] : 0.0);

    // Get id
    int near_id=src_pl_near->get_id(0);

    // Loop updating
    for (UInt p = 0; p < dst_pl_num_pnts; ++p) {
      // If not filled
      if (dst_closest_src_gid[p] == SN_BAD_ID) {

        // Get dst point coords
        const double *pnt_crd=dst_pl->get_coord_ptr(p);
        double dst_pont[3];
        dst_pnt[0] = pnt_crd[0];
        dst_pnt[1] = pnt_crd[1];
        dst_pnt[2] = (sdim == 3 ? pnt_crd[2] : 0.0);
                
        // Calculate distance        
        double dist2=(dst_pnt[0]-near_pnt[0])*(dst_pnt[0]-near_pnt[0])+
          (dst_pnt[1]-near_pnt[1])*(dst_pnt[1]-near_pnt[1])+
          (dst_pnt[2]-near_pnt[2])*(dst_pnt[2]-near_pnt[2]);

        // Update list
        dst_closest_src_gid[p]=near_id;
        dst_closest_dist2[p]=dist2;
      }
    }
  }  

  
  // For debugging see if there are any unmapped points
  bool still_has_unmapped=false;
  for (UInt p = 0; p < dst_pl_num_pnts; ++p) {
    if (dst_closest_src_gid[p] == SN_BAD_ID) {
      still_has_unmapped=true;
    }
  }
  
  if (still_has_unmapped) {
    printf("%d# still has unmapped src_pl num=%d\n",Par::Rank(),src_pl->get_curr_num_pts());
  }
  
  
  
  // Check to see if we are done with any points
  for (UInt p = 0; p < dst_pl_num_pnts; ++p) {

    // If this has a closest point, see if it's within our mm box
    if (dst_closest_src_gid[p] != SN_BAD_ID) {

      // Get dst point
      const double *pnt_crd=dst_pl->get_coord_ptr(p);

      // Set dst point coords in search structure
      double dst_pnt[3];
      dst_pnt[0] = pnt_crd[0];
      dst_pnt[1] = pnt_crd[1];
      dst_pnt[2] = (sdim == 3 ? pnt_crd[2] : 0.0);

      // Get closest dist
      double dist=sqrt(dst_closest_dist2[p]);

      // See if we are within box
      bool within=true;
      if ((dst_pnt[0] + dist) > dst_max[0]) within=false;
      if ((dst_pnt[1] + dist) > dst_max[1]) within=false;
      if ((dst_pnt[2] + dist) > dst_max[2]) within=false;
      if ((dst_pnt[0] - dist) < dst_min[0]) within=false;
      if ((dst_pnt[1] - dist) < dst_min[1]) within=false;
      if ((dst_pnt[2] - dist) < dst_min[2]) within=false;

      // If within, then we are done
      if (within) dst_done[p]=1;
    }
  }

  

  //// Given the distances for the dst points, calculate a wider min/max
  // Get min/max of dst
  ////// Get local min/max of dst
  double dst_min_final[3]={huge,huge,huge};
  double dst_max_final[3]={neg_huge,neg_huge,neg_huge};

  // Loop calculating local min/max
  for (UInt p = 0; p < dst_pl->get_curr_num_pts(); ++p) {

    // Skip if already done
    // What happens if there's nothing...
    // TODO: Detect if there is nothing and have a way to tell SpaceDir to not add a box for this PET
    //    if (dst_done[p]) continue;

    // Error out if not found yet
    //    if (dst_closest_src_gid[p] == SN_BAD_ID) Throw() << "Point "<<p<<" not mapped yet!";
    
    // Get dst_pnt
    const point *pnt_ptr=dst_pl->get_point(p);

    // Set coord value in 3D point
    double dst_pnt[3]={0.0,0.0,0.0};
    dst_pnt[0] = pnt_ptr->coords[0];
    dst_pnt[1] = pnt_ptr->coords[1];
    dst_pnt[2] = sdim == 3 ? pnt_ptr->coords[2] : 0.0;

    // Get closest dist
    double dist=sqrt(dst_closest_dist2[p]);

    // Get min max box necessary to search that distance
    double dst_pnt_min[3], dst_pnt_max[3];
    MU_SUB_SCALAR_VEC3D(dst_pnt_min,dst_pnt,dist);
    MU_ADD_SCALAR_VEC3D(dst_pnt_max,dst_pnt,dist);
        
    // compute proc min max
    MU_SET_MIN_VEC3D(dst_min_final,dst_pnt_min);
    MU_SET_MAX_VEC3D(dst_max_final,dst_pnt_max);
  }

  //printf("%d# dst_min_final=%f %f %f dst_max_final=%f %f %f\n",Par::Rank(),MU_LST_VEC3D(dst_min_final),MU_LST_VEC3D(dst_max_final));

  printf("%d# Opt Before final\n",Par::Rank());
  

  // Create a new pointlist (src_pl_final) where
  // any point that lies within dst_min_final & dst_max_final is on this PET
  PointList *src_pl_final;
  create_plist_using_minmax(src_pl,
                            dst_min_final, dst_max_final,
                            src_pl_final);
 
  
  // Create tree for the final list
  OTree *src_pl_final_tree;
  create_OTree_from_plist(src_pl_final, src_pl_final_tree);

  // Init search structure
  //SearchData sd;
  sd.sdim=sdim;
  sd.closest_dist2=huge;
  sd.closest_src_id=SN_BAD_ID;
  sd.srcpointlist=src_pl_final;
  MU_SET_TO_SCALAR_VEC3D(sd.dst_pnt,0.0);
  
  // Loop the destination points, find hosts.
  for (UInt p = 0; p < dst_pl_num_pnts; ++p) {

    // Skip if done
    if (dst_done[p]) continue;

    // Get point
    const double *pnt_crd=dst_pl->get_coord_ptr(p);

    // Set dst point coords in search structure
    sd.dst_pnt[0] = pnt_crd[0];
    sd.dst_pnt[1] = pnt_crd[1];
    sd.dst_pnt[2] = (sdim == 3 ? pnt_crd[2] : 0.0);

    // Init search box to max possible and then maybe change below
    double pmin[3]={-huge, -huge, -huge};
    double pmax[3]={huge, huge, huge};
    
    // If we have a valid previous nearest point, then use that
    if (dst_closest_src_gid[p] !=  SN_BAD_ID) {
      // Fill search struct
      sd.closest_dist2=dst_closest_dist2[p];
      sd.closest_src_id=dst_closest_src_gid[p];

      // Calc pmin/pmax
      double dist=sqrt(dst_closest_dist2[p]);
      MU_SUB_SCALAR_VEC3D(pmin,sd.dst_pnt,dist);
      MU_ADD_SCALAR_VEC3D(pmax,sd.dst_pnt,dist);      
    } else {
      // Use the last searched point as a guess
      if (sd.closest_src_id != SN_BAD_ID) {
        // Calculate distance        
        double dist2=(sd.dst_pnt[0]-sd.closest_coord[0])*(sd.dst_pnt[0]-sd.closest_coord[0])+
          (sd.dst_pnt[1]-sd.closest_coord[1])*(sd.dst_pnt[1]-sd.closest_coord[1])+
          (sd.dst_pnt[2]-sd.closest_coord[2])*(sd.dst_pnt[2]-sd.closest_coord[2]);
        
        // set closest dist squared
        sd.closest_dist2=dist2;
        // The sd.closest_src_id is still set from last time
        
        // Calc new search box
        double dist=sqrt(dist2);
        MU_SUB_SCALAR_VEC3D(pmin,sd.dst_pnt,dist);
        MU_ADD_SCALAR_VEC3D(pmax,sd.dst_pnt,dist);      
      }
    }

    // Find closest source node to this destination node
    src_pl_final_tree->runon_mm_chng(pmin, pmax, nearest_func, (void *)&sd);

    // If we've found a nearer source point, then add to the search results list...
    if (sd.closest_src_id != SN_BAD_ID) {      
      dst_closest_src_gid[p]=sd.closest_src_id;
      dst_closest_dist2[p]=sd.closest_dist2;      
    }
  }
  
  // Do output based on CommDataOpt
  result.clear();
  for (int i=0; i<dst_pl_num_pnts; i++) {
    // Get dst id of point
    int dst_id=dst_pl->get_id(i);

    // Do stuff depending on if nearest point was found
    if (dst_closest_src_gid[i] != SN_BAD_ID) {

      // We've found a nearest source point, so add to results list
      Search_result *sr=new Search_result();

      sr->dst_gid=dst_id;
      sr->src_gid=dst_closest_src_gid[i];

      result.push_back(sr);

      // If necessary, set dst status
      if (set_dst_status) {
        // Set col info
        WMat::Entry col(ESMC_REGRID_STATUS_MAPPED,
                        0, 0.0, 0);

        // Set row info
        WMat::Entry row(dst_id, 0, 0.0, 0);

        // Put weights into weight matrix
        dst_status.InsertRowMergeSingle(row, col);
      }

    } else {
      // If necessary, set dst status
      if (set_dst_status) {
        // Set col info
        WMat::Entry col(ESMC_REGRID_STATUS_OUTSIDE,
                        0, 0.0, 0);

        // Set row info
        WMat::Entry row(dst_id, 0, 0.0, 0);

        // Put weights into weight matrix
        dst_status.InsertRowMergeSingle(row, col);
      }

      //// check possible error condition
      if (unmappedaction == ESMCI_UNMAPPEDACTION_ERROR) {
        Throw() << " Some destination points cannot be mapped to source grid";
      } else if (unmappedaction == ESMCI_UNMAPPEDACTION_IGNORE) {
        // don't do anything
      } else {
        Throw() << " Unknown unmappedaction option";
      }
    }
  }

  printf("%d# Opt End\n",Par::Rank());
  
#if 0
  for (int i=0; i<num_rcv_pets; i++) {
    printf(" %d# to %d pnt # = ",Par::Rank(),rcv_pets[i]);
    for (int j=0; j<rcv_results[i].size(); j++) {
      printf(" %d ",rcv_results[i][j].elem_id);
    }
    printf(" \n");
  }
#endif

}




  ///// OLDWAY ////////////
struct CommData {
  double closest_dist2;
  int closest_src_gid;
  int proc;
};

  
  void ParSearchNearestSrcToDst(const PointList &src_pl, const PointList &dst_pl, int unmappedaction, SearchResult &result, bool set_dst_status, WMat &dst_status) {
    Trace __trace("ParSearchNearestSrcToDst(const PointList &src_pl, const PointList &dst_pl, int unmappedaction, SearchResult &result)");
  //int FindPnts(const Mesh &mesh, int unmappedaction, int dim_pnts, int num_pnts, double *pnts, int *procs, int *gids) {
  //  Trace __trace("FindPnts()");

  // Get spatial dim and make sure both have the same
  int sdim=src_pl.get_coord_dim();
  if (sdim != dst_pl.get_coord_dim()) {
    Throw() << "src and dst must have same spatial dim for search";
  }

  // Get some useful info
  int dst_size = dst_pl.get_curr_num_pts();
  int num_nodes_to_search=src_pl.get_curr_num_pts();

  // Create search tree
  OTree *tree=new OTree(num_nodes_to_search);

  // Get universal min-max
  //// Use sqrt, so if it's squared it doesn't overflow
  double huge=sqrt(std::numeric_limits<double>::max());
  double min=-huge;
  double max=huge;

  // Add unmasked nodes to search tree
  // and calculate proc min-max
  double pnt[3];
  double proc_min[3];
  double proc_max[3];

  proc_min[0]=max; proc_min[1]=max; proc_min[2]=max;
  proc_max[0]=min; proc_max[1]=min; proc_max[2]=min;

  // Add unmasked nodes to search tree
  for (UInt p = 0; p < num_nodes_to_search; ++p) {

    const point *point_ptr=src_pl.get_point(p);

    // Set coord value in 3D point
    pnt[0] = point_ptr->coords[0];
    pnt[1] = point_ptr->coords[1];
    pnt[2] = sdim == 3 ? point_ptr->coords[2] : 0.0;

    tree->add(pnt, pnt, (void*)point_ptr);

    // compute proc min max
    if (pnt[0] < proc_min[0]) proc_min[0]=pnt[0];
    if (pnt[1] < proc_min[1]) proc_min[1]=pnt[1];
    if (pnt[2] < proc_min[2]) proc_min[2]=pnt[2];

    if (pnt[0] > proc_max[0]) proc_max[0]=pnt[0];
    if (pnt[1] > proc_max[1]) proc_max[1]=pnt[1];
    if (pnt[2] > proc_max[2]) proc_max[2]=pnt[2];
  }


  // Commit tree
  tree->commit();

  // Create SpaceDir
  SpaceDir *spacedir=new SpaceDir(proc_min, proc_max, tree, false);


  //// Find the closest point locally ////

  // Set initial search box to the largest possible
  double pmin[3], pmax[3];
  pmin[0] = min;  pmin[1] = min;  pmin[2] = min;
  pmax[0] = max;  pmax[1] = max;  pmax[2] = max;

  // Allocate space to hold closest gids, dist
  vector<int> closest_src_gid(dst_size,-1);
  vector<double> closest_dist2(dst_size,huge);

  // Setup search structure
  SearchData sd;
  sd.sdim=sdim;
  // sd.closest_src_node=NULL;
  sd.closest_dist2=huge;
  // sd.src_coord=NULL;
  sd.closest_src_id=SN_BAD_ID;
  sd.srcpointlist=&src_pl;

  // Loop the destination points, find hosts.
  for (UInt p = 0; p < dst_size; ++p) {

    const double *pnt_crd=dst_pl.get_coord_ptr(p);

    // Set dst point coords in search structure
    sd.dst_pnt[0] = pnt_crd[0];
    sd.dst_pnt[1] = pnt_crd[1];
    sd.dst_pnt[2] = (sdim == 3 ? pnt_crd[2] : 0.0);

    // If a closest point exists from the last loop then use as initial guess
    if (sd.closest_src_id != SN_BAD_ID) {
    // Calculate distance
      double dist2=(sd.dst_pnt[0]-sd.closest_coord[0])*(sd.dst_pnt[0]-sd.closest_coord[0])+
                   (sd.dst_pnt[1]-sd.closest_coord[1])*(sd.dst_pnt[1]-sd.closest_coord[1])+
                   (sd.dst_pnt[2]-sd.closest_coord[2])*(sd.dst_pnt[2]-sd.closest_coord[2]);

      // set closest dist squared
      sd.closest_dist2=dist2;

      // Calc new search box
      double dist=sqrt(dist2);

      pmin[0]=sd.dst_pnt[0]-dist;
      pmin[1]=sd.dst_pnt[1]-dist;
      pmin[2]=sd.dst_pnt[2]-dist;

      pmax[0]=sd.dst_pnt[0]+dist;
      pmax[1]=sd.dst_pnt[1]+dist;
      pmax[2]=sd.dst_pnt[2]+dist;
    }

    // Find closest source node to this destination node
    tree->runon_mm_chng(pmin, pmax, nearest_func, (void *)&sd);

    // If we've found a nearest source point, then add to the search results list...
    if (sd.closest_src_id != SN_BAD_ID) {
      closest_src_gid[p]=sd.closest_src_id;
      closest_dist2[p]=sd.closest_dist2;
    }
  }

  // Get list of procs where a point can be located
  vector< vector<int> > proc_lists;  // List of procs
  proc_lists.resize(dst_size);

  for (int i=0; i<dst_size; i++) {

    const double *pnt_crd=dst_pl.get_coord_ptr(i);

    // Get search box based on what we've found so far
    double pnt_min[3], pnt_max[3];

    // If we've found a point, then search box is based on distance to it
    if (closest_src_gid[i] != -1) {
      double dist=sqrt(closest_dist2[i]);

      pnt_min[0] = pnt_crd[0]-dist;
      pnt_min[1] = pnt_crd[1]-dist;
      pnt_min[2] = (sdim == 3) ? pnt_crd[2]-dist : 0.0;

      pnt_max[0] = pnt_crd[0]+dist;
      pnt_max[1] = pnt_crd[1]+dist;
      pnt_max[2] = (sdim == 3) ? pnt_crd[2]+dist : 0.0;
    } else {  // ...nothing so far, so just make maxium
      pnt_min[0] = min;  pnt_min[1] = min;  pnt_min[2] = min;
      pnt_max[0] = max;  pnt_max[1] = max;  pnt_max[2] = max;
    }

    //// Init vector
    proc_lists[i].clear();

    //// Search on point, plus or minus search_tolerence
    spacedir->get_procs(pnt_min, pnt_max, &(proc_lists[i])); // Definitely need () around proc_lists

    //  if (node.get_id()==110581) {
    //printf("%d# BOB FOUND %d other procs for node=%d\n",Par::Rank(),proc_lists[i].size(),node.get_id());
    // }


#if 0
    printf(" %d# Pnt=[%f %f %f] sending to = ",Par::Rank(),pnt_crd[0],pnt_crd[1],pnt_crd[2]);
    for (int j=0; j<proc_lists[i].size(); j++) {
      printf(" %d ",proc_lists[i][j]);
    }
    printf(" \n");
#endif
  }


  // Get the number of processors used below
  int num_procs=Par::Size();

  // Figure out which procs we're sending to
  int num_snd_procs;
  vector<int> snd_pets;
  vector< vector<int> > snd_inds;
  // {
    vector< vector<int> > tmp_snd_inds;
    tmp_snd_inds.resize(num_procs);
    for (int i=0; i<dst_size; i++) {
      for (int j=0; j<proc_lists[i].size(); j++) {
        tmp_snd_inds[proc_lists[i][j]].push_back(i);
      }
    }

    // Collapse lists to just processors with non-empty lists
    num_snd_procs=0;
    for (int i=0; i<num_procs; i++) {
      if (!tmp_snd_inds[i].empty()) num_snd_procs++;
    }

    // reserve space in lists
    snd_pets.resize(num_snd_procs,-1);
    snd_inds.resize(num_snd_procs);

    int k=0;
    for (int i=0; i<num_procs; i++) {
      if (!tmp_snd_inds[i].empty()) {
        snd_pets[k]=i;
        snd_inds[k].reserve(tmp_snd_inds[i].size());
        snd_inds[k]=tmp_snd_inds[i];
        k++;
      }
    }
    //  } // Get rid of tmp_snd_inds


    // send size: point+dist
    int snd_size=(sdim+1)*sizeof(double);


    // Calculate send sizes
    vector<int> snd_sizes;
    snd_sizes.resize(num_snd_procs,0); // resize and init to 0
    for (int i=0; i< num_snd_procs; i++) {
      snd_sizes[i]=snd_size*snd_inds[i].size();
    }

#if 0
  // Debug output
  for (int i=0; i<num_snd_procs; i++) {
    printf(" %d# to %d pnt id# = ",Par::Rank(),snd_pets[i]);
    for (int j=0; j<snd_inds[i].size(); j++) {
      printf(" %d ",snd_inds[i][j]);
    }
    printf(" \n");
  }
#endif


  // Create communication structure
  SparseMsg comm;

  // Setup pattern and sizes
  if (num_snd_procs >0) {
    comm.setPattern(num_snd_procs, (const UInt *)&(snd_pets[0]));
    comm.setSizes((UInt *)&(snd_sizes[0]));
  } else {
    comm.setPattern(0, (const UInt *)NULL);
    comm.setSizes((UInt *)NULL);
  }

  // Reset buffers
  comm.resetBuffers();

  ///// THINK ABOUT MAKING A STRUCT AND SENDING PNT ID AND getting rid of snd_inds? and tmp_snd_inds
  ///// ALSO MAYBE BREAK INTO A FEW SUBROUTINES

  // Pack points into buffers
  for (int i=0; i< num_snd_procs; i++) {
    SparseMsg:: buffer *b=comm.getSendBuffer(snd_pets[i]);
    for (int j=0; j<snd_inds[i].size(); j++) {
      // Get index of node
      int ind=snd_inds[i][j];

      const double *pnt_crd=dst_pl.get_coord_ptr(ind);

      // pack buf
      double buf[4]; // 4 is biggest this should be (i.e. 3D+dist2)
      buf[0]=pnt_crd[0];
      buf[1]=pnt_crd[1];
      if (sdim < 3) buf[2]=closest_dist2[ind];
      else {
        buf[2]=pnt_crd[2];
        buf[3]=closest_dist2[ind];
      }

      // Push buf onto send struct
      b->push((const UChar *)buf, (UInt)snd_size);
    }
  }


  // Communicate point information
  comm.communicate();

  // Calculate the number of recv. pets
  int num_rcv_pets=0;
  for (std::vector<UInt>::iterator p = comm.inProc_begin(); p != comm.inProc_end(); ++p) {
    num_rcv_pets++;
  }

  // Hold rcv results
  int *rcv_pets=NULL;
  int *rcv_results_size=NULL;
  CommData **rcv_results_array=NULL;

  if (num_rcv_pets>0) {
    rcv_pets=new int[num_rcv_pets];
    rcv_results_size=new int[num_rcv_pets];
    rcv_results_array=new CommData*[num_rcv_pets];
  }

  int ip=0;
  for (std::vector<UInt>::iterator p = comm.inProc_begin(); p != comm.inProc_end(); ++p) {
    UInt proc = *p;
    SparseMsg::buffer *b = comm.getRecvBuffer(proc);

    // Add proc
    rcv_pets[ip]=proc;

    // Figure out how many messages we have
    int num_msgs=b->msg_size()/snd_size;

    // Setup for results
    rcv_results_size[ip]=num_msgs;
    rcv_results_array[ip]=NULL;

    // Skip to next buffer if empty
    if (num_msgs==0) continue;

    // Allocate space for results
    rcv_results_array[ip]=new CommData[num_msgs];


    // Unpack everything from this processor
    int jp=0;
    while (!b->empty()) {
      double buf[4]; // 4 is biggest this should be (i.e. 3D+dist)

 /* XMRKX */
      b->pop((UChar *)buf, (UInt)snd_size);


      // Unpack buf
      double pnt[3]={0.0,0.0,0.0};
      double dist2=0.0;

      pnt[0]=buf[0];
      pnt[1]=buf[1];
      if (sdim < 3) dist2=buf[2];
      else {
        pnt[2]=buf[2];
        dist2=buf[3];
      }

      
      //// Point min max we expand the point by the distance to the closest point
      double pmin[3], pmax[3];
      
      double dist=sqrt(dist2); // Get actual dist, so we can compute minmax box

      pmin[0] = pnt[0]-dist;
      pmin[1] = pnt[1]-dist;
      pmin[2] = (sdim == 3) ? pnt[2]-dist : 0.0;

      pmax[0] = pnt[0]+dist;
      pmax[1] = pnt[1]+dist;
      pmax[2] = (sdim == 3) ? pnt[2]+dist : 0.0;


      // Setup search structure
      SearchData sd;
      sd.sdim=sdim;
      sd.dst_pnt[0] = pnt[0];
      sd.dst_pnt[1] = pnt[1];
      sd.dst_pnt[2] = (sdim == 3 ? pnt[2] : 0.0);
      sd.closest_dist2=dist2;

      sd.closest_src_id=SN_BAD_ID;
      sd.srcpointlist=&src_pl;

      // Find closest source node to this destination node
      tree->runon_mm_chng(pmin, pmax, nearest_func, (void *)&sd);

      // Fill in structure to be sent
      CommData cd;
      if (sd.closest_src_id != SN_BAD_ID) {
        cd.closest_dist2=sd.closest_dist2;
        cd.closest_src_gid=sd.closest_src_id;

        //      printf("#%d c_s_g=%d \n", Par::Rank(),cd.closest_src_gid);

      } else {
        cd.closest_dist2=huge;
        cd.closest_src_gid=SN_BAD_ID;
      }
      cd.proc=Par::Rank();

      // Save results
      rcv_results_array[ip][jp]=cd;

      jp++;
    }

    ip++;
  }

  // Calculate size to send back to pnt's home proc
  vector<int> rcv_sizes;
  rcv_sizes.resize(num_rcv_pets,0); // resize and init to 0
  for (int i=0; i< num_rcv_pets; i++) {
    rcv_sizes[i]=sizeof(CommData)*rcv_results_size[i];
  }


  // Create communication structure spatial to home
  SparseMsg comm_to_home;


  // Setup pattern and sizes
  if (num_rcv_pets>0) {
    comm_to_home.setPattern(num_rcv_pets, (const UInt *)&(rcv_pets[0]));
    comm_to_home.setSizes((UInt *)&(rcv_sizes[0]));
  } else {
    comm_to_home.setPattern(0, (const UInt *)NULL);
    comm_to_home.setSizes((UInt *)NULL);
  }


  // Reset buffers
  comm_to_home.resetBuffers();

  // Pack points into buffers
  for (int i=0; i< num_rcv_pets; i++) {
    SparseMsg:: buffer *b=comm_to_home.getSendBuffer(rcv_pets[i]);
    for (int j=0; j<rcv_results_size[i]; j++) {
      b->push((const UChar *)&(rcv_results_array[i][j]), (UInt)sizeof(CommData));
    }
  }

  // Communicate point information
  comm_to_home.communicate();

  // Get rid of rcv results
  if (num_rcv_pets>0) {
    for (int i=0; i<num_rcv_pets; i++) {
      if (rcv_results_array[i]!=NULL) delete [] rcv_results_array[i];
    }
    if (rcv_results_array!=NULL) delete [] rcv_results_array;
    if (rcv_pets!=NULL) delete [] rcv_pets;
    if (rcv_results_size!=NULL) delete [] rcv_results_size;
  }

  // Unpack CommData and generate results
  for (std::vector<UInt>::iterator p = comm_to_home.inProc_begin(); p != comm_to_home.inProc_end(); ++p) {
    UInt proc = *p;
    SparseMsg::buffer *b = comm_to_home.getRecvBuffer(proc);

    // Unpack everything from this processor
    int j=0;
    while (!b->empty()) {
      CommData cd;

      // Get sent data
      b->pop((UChar *)&cd, (UInt)sizeof(CommData));

      // Get loc of point
      //// Can't use snd_inds because first index isn't necesarily in same order
      //// between send and receive
      int pnt_ind=tmp_snd_inds[proc][j];

      // Compare and set
      if (cd.closest_src_gid > -1) {
        //      printf("#%d LAST c_s_g=%d \n", Par::Rank(),cd.closest_src_gid);

        if (cd.closest_dist2 < closest_dist2[pnt_ind]) {
          closest_dist2[pnt_ind]=cd.closest_dist2;
          closest_src_gid[pnt_ind]=cd.closest_src_gid;
        } else if (cd.closest_dist2 == closest_dist2[pnt_ind]) {
          // If exactly the same distance chose the point with the smallest id
          // (To make things consistent when running on different numbers of procs)
          if (cd.closest_src_gid < closest_src_gid[pnt_ind]) {
            closest_dist2[pnt_ind]=cd.closest_dist2;
            closest_src_gid[pnt_ind]=cd.closest_src_gid;
          }
        }
      }

      // next result
      j++;
    }
  }

  // Do output based on CommData
  result.clear();
  for (int i=0; i<dst_size; i++) {
    // Get dst id of point
    int dst_id=dst_pl.get_id(i);

    // Do stuff depending on if nearest point was found
    if (closest_src_gid[i] > -1) {

      // We've found a nearest source point, so add to results list
      Search_result *sr=new Search_result();

      sr->dst_gid=dst_id;
      sr->src_gid=closest_src_gid[i];

      result.push_back(sr);

      // If necessary, set dst status
      if (set_dst_status) {
        // Set col info
        WMat::Entry col(ESMC_REGRID_STATUS_MAPPED,
                        0, 0.0, 0);

        // Set row info
        WMat::Entry row(dst_id, 0, 0.0, 0);

        // Put weights into weight matrix
        dst_status.InsertRowMergeSingle(row, col);
      }

    } else {
      // If necessary, set dst status
      if (set_dst_status) {
        // Set col info
        WMat::Entry col(ESMC_REGRID_STATUS_OUTSIDE,
                        0, 0.0, 0);

        // Set row info
        WMat::Entry row(dst_id, 0, 0.0, 0);

        // Put weights into weight matrix
        dst_status.InsertRowMergeSingle(row, col);
      }

      //// check possible error condition
      if (unmappedaction == ESMCI_UNMAPPEDACTION_ERROR) {
        Throw() << " Some destination points cannot be mapped to source grid";
      } else if (unmappedaction == ESMCI_UNMAPPEDACTION_IGNORE) {
        // don't do anything
      } else {
        Throw() << " Unknown unmappedaction option";
      }
    }
  }

#if 0
  for (int i=0; i<num_rcv_pets; i++) {
    printf(" %d# to %d pnt # = ",Par::Rank(),rcv_pets[i]);
    for (int j=0; j<rcv_results[i].size(); j++) {
      printf(" %d ",rcv_results[i][j].elem_id);
    }
    printf(" \n");
  }
#endif

}
  
  
#undef SN_BAD_ID

} // namespace
