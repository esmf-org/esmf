// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/Regridding/ESMCI_Search.h>
#include <Mesh/include/Legacy/ESMCI_MeshTypes.h>
#include <Mesh/include/Legacy/ESMCI_MeshObjTopo.h>
#include <Mesh/include/Regridding/ESMCI_Mapping.h>
#include <Mesh/include/Legacy/ESMCI_MeshObj.h>
#include <Mesh/include/ESMCI_Mesh.h>
#include <Mesh/include/Legacy/ESMCI_MeshUtils.h>
#include <Mesh/include/ESMCI_OTree.h>
#include <Mesh/include/Legacy/ESMCI_Mask.h>
#include <Mesh/include/Legacy/ESMCI_ParEnv.h>
#include <Mesh/include/Regridding/ESMCI_MeshRegrid.h>
#include <Mesh/include/ESMCI_MathUtil.h>

#include "PointList/include/ESMCI_PointList.h"

#include <iostream>
#include <fstream>
#include <algorithm>
#include <iterator>

#include <ostream>

#include <set>

#include <limits>
#include <vector>

#include <Mesh/include/Legacy/ESMCI_BBox.h>
#include <Mesh/include/Regridding/ESMCI_SpaceDir.h>

using std::vector;



//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

  bool snn_debug=false;

#define SN_BAD_ID -1

struct SearchDataPnt {
  double dist2;  // closest distance squared
  int src_id;
  double coord[3];

  SearchDataPnt() : dist2(std::numeric_limits<double>::max()), src_id(SN_BAD_ID) {
    coord[0]=0.0;
    coord[1]=0.0;
    coord[2]=0.0;
  }

  // Operators don't consider coord
  bool operator<(const SearchDataPnt &rhs) const {
    if (dist2 != rhs.dist2) return dist2 < rhs.dist2;
    return src_id < rhs.src_id;
  }

  bool operator==(const SearchDataPnt &rhs) {
    return (dist2 == rhs.dist2 && src_id == rhs.src_id);
  }

};

struct SearchData {
  int sdim;
  double dst_pnt[3];
  double max_dist2;

  int max_num_pnts;
  int num_valid_pnts;
  SearchDataPnt *pnts;

  SearchData() : sdim(0),
                 max_dist2(std::numeric_limits<double>::max()),
                 max_num_pnts(0), num_valid_pnts(0), pnts(NULL) {
    dst_pnt[0]=0.0;
    dst_pnt[1]=0.0;
    dst_pnt[2]=0.0;
  }

  SearchData(int _sdim, double *_dst_pnt, int _max_num_pnts) :
    sdim(_sdim),  max_dist2(std::numeric_limits<double>::max()),
    max_num_pnts(_max_num_pnts), num_valid_pnts(0), pnts(NULL) {

    // Set dst point coords in search structure
    dst_pnt[0] = _dst_pnt[0];
    dst_pnt[1] = _dst_pnt[1];
    dst_pnt[2] = (sdim == 3 ? _dst_pnt[2] : 0.0);

    // Set up point list
    pnts=NULL;
    if (max_num_pnts > 0) pnts=new SearchDataPnt[max_num_pnts];
  }

  // Copy
  SearchData &operator=(const SearchData &rhs) {
      if (this == &rhs) return *this;
      sdim = rhs.sdim;
      dst_pnt[0]=rhs.dst_pnt[0];
      dst_pnt[1]=rhs.dst_pnt[1];
      dst_pnt[2]=rhs.dst_pnt[2];
      max_dist2=rhs.max_dist2;
      max_num_pnts=rhs.max_num_pnts;
      num_valid_pnts=rhs.num_valid_pnts;

      // Get rid of points list, if it exists
      if (pnts != NULL) delete [] pnts;

      // Set up new point list
      pnts=NULL;
      if (max_num_pnts > 0) pnts=new SearchDataPnt[max_num_pnts];

      // Copy points list
      for(int i=0; i<num_valid_pnts; i++) {
        pnts[i]=rhs.pnts[i];
      }

      return *this;
    }

  // Explicity set max_dist2, this will be adjusted automatically if the box is full when
  // things are added.
  void set_max_dist2(double new_max_dist2) {
    max_dist2=new_max_dist2;
  }

  void swap_dst_pnt(const double *new_dst_pnt) {

    // Set dst point coords in search structure
    dst_pnt[0] = new_dst_pnt[0];
    dst_pnt[1] = new_dst_pnt[1];
    dst_pnt[2] = (sdim == 3 ? new_dst_pnt[2] : 0.0);

    // If a closest points exist from the last loop then use as initial guess
    for (int i=0; i<num_valid_pnts; i++) {
      SearchDataPnt *pnt=pnts+i;

      // Calculate distance squared
      double dist2=
        (dst_pnt[0]-pnt->coord[0])*(dst_pnt[0]-pnt->coord[0])+
        (dst_pnt[1]-pnt->coord[1])*(dst_pnt[1]-pnt->coord[1])+
        (dst_pnt[2]-pnt->coord[2])*(dst_pnt[2]-pnt->coord[2]);

      // set distance squared
      pnt->dist2=dist2;
    }

    // Sort points in order
    std::sort(pnts,pnts+num_valid_pnts);

    // If necessary adjust max distance
    if (num_valid_pnts == max_num_pnts) {
      // Get max distance squared of points
      // (which is distance of furthest point)
      max_dist2=pnts[max_num_pnts-1].dist2;
    }
  }

  // See if the pointlist is full, if so then the search box
  // will start changing as things are added, etc.
  bool is_full() {
    return (num_valid_pnts == max_num_pnts);
  }

  // Attempt to add a new point
  // Try adding a new point to the search structure. If added returns true, else returns false
  // If added it updates all the other info (e.g. max_dist2, etc) to correspond
  bool add_pnt(int id, double *pnt) {

    // Convert to 3D point
    double new_pnt[3];
    new_pnt[0] = pnt[0];
    new_pnt[1] = pnt[1];
    new_pnt[2] = (sdim == 3 ? pnt[2] : 0.0);

    // Calculate squared distance
    double dist2=
      (dst_pnt[0]-new_pnt[0])*(dst_pnt[0]-new_pnt[0])+
      (dst_pnt[1]-new_pnt[1])*(dst_pnt[1]-new_pnt[1])+
      (dst_pnt[2]-new_pnt[2])*(dst_pnt[2]-new_pnt[2]);

    // Leave if this is bigger than the max distance that we already have
    if (dist2 > max_dist2) return false;

    // Leave if we already have it
    for (int i=0; i<num_valid_pnts; i++) {
      if (pnts[i].src_id == id) return false;
    }

    // variable to watch for adding
    bool was_added=false;

    // make tmp point
    SearchDataPnt tmp_sdp;
    tmp_sdp.dist2=dist2;
    tmp_sdp.src_id=id;
    MU_ASSIGN_VEC3D(tmp_sdp.coord, new_pnt);

    // Insert it into the list
    for (int i=0; i<num_valid_pnts; i++) {

      if (tmp_sdp < pnts[i]) {
        SearchDataPnt swap_sdp;
        swap_sdp=pnts[i];
        pnts[i]=tmp_sdp;
        tmp_sdp=swap_sdp;
        was_added=true;
      }
    }

    // Insert tmp_sdp at end if there's room
    if (num_valid_pnts < max_num_pnts) {
      pnts[num_valid_pnts]=tmp_sdp;
      num_valid_pnts++;
      was_added=true;
    }

    // If we're full get max distance squared of points
    if (num_valid_pnts == max_num_pnts) {
      max_dist2=pnts[max_num_pnts-1].dist2;
    }

#if 0
    // DEBUG
    if (snn_debug) {
      printf(" nnf2: dst_id=%d :: ",573);
      for (int i=0; i<num_valid_pnts; i++) {
        printf(" %d ",pnts[i].src_id);
      }
      printf("\n");
    }
#endif

    // return was_added status
    return was_added;
  }

  // Get the min-max box that should be searched to add new points
  // If the structure isn't full this will be infinite, otherwise
  // it's the maximum distance out
  void get_search_min_max(double *min, double *max) {

    // If max_dist2 isn't infinite then compute search box
    if (max_dist2 < std::numeric_limits<double>::max()) {

      // get distance
      double dist=sqrt(max_dist2);

      // Compute new search box
      min[0]=dst_pnt[0]-dist;
      min[1]=dst_pnt[1]-dist;
      min[2]=dst_pnt[2]-dist;

      max[0]=dst_pnt[0]+dist;
      max[1]=dst_pnt[1]+dist;
      max[2]=dst_pnt[2]+dist;
    } else {
      // Set to infinite
      min[0]=-std::numeric_limits<double>::max();
      min[1]=-std::numeric_limits<double>::max();
      min[2]=-std::numeric_limits<double>::max();

      max[0]=std::numeric_limits<double>::max();
      max[1]=std::numeric_limits<double>::max();
      max[2]=std::numeric_limits<double>::max();
    }
  }


  ~SearchData() {
    // Get rid of points list
    if (pnts != NULL) delete [] pnts;
  }
};


  static int nearest_func(void *n, void *y, double *min, double *max) {

    point *this_pt = static_cast<point*>(n);

    SearchData *sd = static_cast<SearchData*>(y);

    // Try adding point, if not added then leave, because nothing to update
    if (!sd->add_pnt(this_pt->id, this_pt->coords)) return 0;

    // If there is still room, leave because we can't restrict things yet
    if (sd->num_valid_pnts < sd->max_num_pnts) return 0;

    // We are full, so search box may have changed with point added above,
    // so get the new search box
    if (sd->is_full()) {
      sd->get_search_min_max(min,max);
    }

    // Don't know if this is the closest, so search further
    return 0;
  }



// The main routine
  void SearchNearestSrcToDstNPnts(const PointList &src_pl, const PointList &dst_pl, int num_pnts, int unmappedaction, SearchResult &result, bool set_dst_status, WMat &dst_status) {
  Trace __trace("Search(PointList &src_pl, PointList &dst_pl, int unmappedaction, SearchResult &result)");

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
  double min,max;
  if (std::numeric_limits<double>::has_infinity) {
    min= -std::numeric_limits<double>::infinity();
    max= std::numeric_limits<double>::infinity();
  } else {
    min = -std::numeric_limits<double>::max();
    max = std::numeric_limits<double>::max();
  }

  // Set initial search box to the largest possible
  double pmin[3],pmax[3];
  MU_SET_TO_SCALAR_VEC3D(pmin,min);
  MU_SET_TO_SCALAR_VEC3D(pmax,max);

  // Setup empty search structure
  double tmp_pnt[3]={0.0,0.0,0.0};
  SearchData sd(sdim, tmp_pnt, num_pnts);

  // Loop the destination points, find hosts.
  int dst_size=dst_pl.get_curr_num_pts();
  for (UInt p = 0; p < dst_size; ++p) {

    // Get point from the point list
    const double *pnt_crd=dst_pl.get_coord_ptr(p);
    int pnt_id=dst_pl.get_id(p);

    // Swap to a new point in the search structure
    sd.swap_dst_pnt(pnt_crd);

    // Get the new search box
    sd.get_search_min_max(pmin,pmax);

    // Find closest source node to this destination node
    tree->runon_mm_chng(pmin, pmax, nearest_func, (void *)&sd);

    // If we've found a nearest source point, then add to the search results list...
    if (sd.num_valid_pnts > 0) {

      // New search result
      Search_result *sr=new Search_result();

      // Fill search results
      sr->dst_gid=p;  // save the location in the dst point list, so we can pull info out
      sr->nodes.reserve(sd.num_valid_pnts);
      for (int i=0; i<sd.num_valid_pnts; i++) {
        SearchDataPnt *pnt=sd.pnts+i;

        // Fill in tmp_snr
        Search_node_result tmp_snr;
        tmp_snr.node=NULL;
        tmp_snr.dst_gid=pnt->src_id; // Yeah this is ugly, but it seems a shame to add a new member
                                       // TODO: rename these members to be more generic
        MU_ASSIGN_VEC3D(tmp_snr.pcoord,pnt->coord);

        // Add it to search results
        sr->nodes.push_back(tmp_snr);
      }

      // Add to results list
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

struct CommDataOut {
  int loc;
  double pnt[3];
  double dist;
};


struct CommDataBack {
  int loc;
  double pnt[3];
  int id;
  int proc;
};


  void ParSearchNearestSrcToDstNPnts(const PointList &src_pl, const PointList &dst_pl, int num_pnts,  int unmappedaction, SearchResult &result, bool set_dst_status, WMat &dst_status) {
    Trace __trace("Search(const PointList &src_pl, const PointList &dst_pl, int unmappedaction, SearchResult &result)");
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
   double min,max;
  if (std::numeric_limits<double>::has_infinity) {
    min= -std::numeric_limits<double>::infinity();
    max= std::numeric_limits<double>::infinity();
  } else {
    min = -std::numeric_limits<double>::max();
    max = std::numeric_limits<double>::max();
  }

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

  // Allocate space to hold search structs for each point
  vector<SearchData> sd_list(dst_size);

  // Set initial search box to the largest possible
  double pmin[3], pmax[3];
  MU_SET_TO_SCALAR_VEC3D(pmin,min);
  MU_SET_TO_SCALAR_VEC3D(pmax,max);

  // Setup empty search structure
  double tmp_pnt[3]={0.0,0.0,0.0};
  SearchData sd(sdim, tmp_pnt, num_pnts);

  // Loop the destination points, find hosts.
  for (UInt p = 0; p < dst_size; ++p) {

    // Get point from the point list
    const double *pnt_crd=dst_pl.get_coord_ptr(p);
    int pnt_id=dst_pl.get_id(p);

    // Swap to a new point in the search structure
    sd.swap_dst_pnt(pnt_crd);

    // Get the new search box
    sd.get_search_min_max(pmin,pmax);

    // Find closest source node to this destination node
    tree->runon_mm_chng(pmin, pmax, nearest_func, (void *)&sd);

    // Copy search results into global list
    sd_list[p] = sd;
  }

  // Get list of procs where a point can be located
  vector< vector<int> > proc_lists;  // List of procs
  proc_lists.resize(dst_size);
  for (int i=0; i<dst_size; i++) {

    // Declare search box
    double pnt_min[3], pnt_max[3];

    // Get search box based on what we've found so far
    sd_list[i].get_search_min_max(pnt_min, pnt_max);

    // Init vector
    proc_lists[i].clear();

    // Search on point, plus or minus search_tolerence
    spacedir->get_procs(pnt_min, pnt_max, &(proc_lists[i])); // Definitely need () around proc_lists

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


    // send size
    int snd_size=sizeof(CommDataOut);

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


  // Pack points into buffers
  for (int i=0; i< num_snd_procs; i++) {
    SparseMsg:: buffer *b=comm.getSendBuffer(snd_pets[i]);
    for (int j=0; j<snd_inds[i].size(); j++) {
      // Get index of node
      int ind=snd_inds[i][j];

      // Get point and max distance
      const double *pnt_crd=dst_pl.get_coord_ptr(ind);

      // Get max distance if there is one, otherwise infinity
      double max_dist;
      if (sd_list[ind].is_full()) {
        max_dist=sqrt(sd_list[ind].max_dist2);
      } else {
        max_dist=std::numeric_limits<double>::max();
      }

      // pack comm data out
      CommDataOut cdo;
      cdo.loc=ind;
      cdo.pnt[0]=pnt_crd[0];
      cdo.pnt[1]=pnt_crd[1];
      cdo.pnt[2] = (sdim == 3 ? pnt_crd[2] : 0.0);
      cdo.dist=max_dist;

      // Push buf onto send struct
      b->push((const UChar *)&cdo, (UInt)snd_size);
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
  //  int *rcv_results_size=NULL;
  vector<CommDataBack> *rcv_results_array=NULL;

  if (num_rcv_pets>0) {
    rcv_pets=new int[num_rcv_pets];
    //rcv_results_size=new int[num_rcv_pets];
    rcv_results_array=new vector<CommDataBack>[num_rcv_pets];
  }


  int ip=0;
  for (std::vector<UInt>::iterator p = comm.inProc_begin(); p != comm.inProc_end(); ++p) {
    UInt proc = *p;
    SparseMsg::buffer *b = comm.getRecvBuffer(proc);

    // Add proc
    rcv_pets[ip]=proc;

    // Figure out how many messages we have
    int num_msgs=b->msg_size()/snd_size;

    // Skip to next buffer if empty
    if (num_msgs==0) continue;

    // Allocate space for results
    //    rcv_results_array[ip]=new CommDataBack[num_msgs];
    rcv_results_array[ip].clear();

    // Unpack everything from this processor
    int jp=0;
    while (!b->empty()) {

      // Comm Data Out
      CommDataOut cdo;

      // Pop one piece of info off of the buffer
      b->pop((UChar *)&cdo, (UInt)snd_size);

      // Unpack info
      int loc;
      double pnt[3]={0.0,0.0,0.0};
      double dist=0.0;

      loc=cdo.loc;
      MU_ASSIGN_VEC3D(pnt, cdo.pnt);
      dist=cdo.dist;


      // TODO: INSTEAD OF THIS, TRY USING sd.get_search_min_max() below for consistencies sake
      //// Point min max we expand the point by the distance to the closest point
      double pmin[3], pmax[3];

      pmin[0] = pnt[0]-dist;
      pmin[1] = pnt[1]-dist;
      pmin[2] = (sdim == 3) ? pnt[2]-dist : 0.0;

      pmax[0] = pnt[0]+dist;
      pmax[1] = pnt[1]+dist;
      pmax[2] = (sdim == 3) ? pnt[2]+dist : 0.0;


      // Setup search structure
      SearchData sd(sdim, pnt, num_pnts);
      sd.set_max_dist2(dist*dist);

      // Find closest source nodes to this destination node
      tree->runon_mm_chng(pmin, pmax, nearest_func, (void *)&sd);

      // Fill in CommDataBack structure
      for (int i=0; i<sd.num_valid_pnts; i++) {
        SearchDataPnt *pnt=sd.pnts+i;

        CommDataBack cd;
        cd.loc=loc;
        MU_ASSIGN_VEC3D(cd.pnt,pnt->coord);
        cd.id=pnt->src_id;
        cd.proc=Par::Rank(); // Do we need this??

        // Add results to list to send back
        rcv_results_array[ip].push_back(cd);
      }

      jp++;
    }

    ip++;
  }

  // Calculate size to send back to pnt's home proc
  vector<int> rcv_sizes;
  rcv_sizes.resize(num_rcv_pets,0); // resize and init to 0
  for (int i=0; i< num_rcv_pets; i++) {
    rcv_sizes[i]=sizeof(CommDataBack)*rcv_results_array[i].size();
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
    for (int j=0; j<rcv_results_array[i].size(); j++) {
      b->push((const UChar *)&(rcv_results_array[i][j]), (UInt)sizeof(CommDataBack));
    }
  }

  // Communicate point information
  comm_to_home.communicate();

  // Get rid of rcv results
  if (num_rcv_pets>0) {
    //    for (int i=0; i<num_rcv_pets; i++) {
    //  if (rcv_results_array[i]!=NULL) delete [] rcv_results_array[i];
    //}
    if (rcv_results_array!=NULL) delete [] rcv_results_array;
    if (rcv_pets!=NULL) delete [] rcv_pets;
    //    if (rcv_results_size!=NULL) delete [] rcv_results_size;
  }

  // Unpack CommDataBack and generate results
  for (std::vector<UInt>::iterator p = comm_to_home.inProc_begin(); p != comm_to_home.inProc_end(); ++p) {
    UInt proc = *p;
    SparseMsg::buffer *b = comm_to_home.getRecvBuffer(proc);

    // Unpack everything from this processor
    while (!b->empty()) {
      CommDataBack cd;

      // Get sent data
      b->pop((UChar *)&cd, (UInt)sizeof(CommDataBack));

      // Get location in search data list
      int loc=cd.loc;

      // Try adding this point to the corresponding search structure
      sd_list[loc].add_pnt(cd.id, cd.pnt);
    }
  }

  // Do output based on CommDataBack
  result.clear();
  for (int p=0; p<dst_size; p++) {

    // Get dst id of point
    int dst_id=dst_pl.get_id(p);

    // If we've found a nearest source point, then add to the search results list...
    if (sd_list[p].num_valid_pnts > 0) {

      // We've found a nearest source point, so add to results list
      Search_result *sr=new Search_result();

      // Fill search results
      sr->dst_gid=p;  // save the location in the dst point list, so we can pull info out
      sr->nodes.reserve(sd_list[p].num_valid_pnts);
      for (int i=0; i<sd_list[p].num_valid_pnts; i++) {
        SearchDataPnt *pnt=sd_list[p].pnts+i;

        // Fill in tmp_snr
        Search_node_result tmp_snr;
        tmp_snr.node=NULL;
        tmp_snr.dst_gid=pnt->src_id; // Yeah this is ugly, but it seems a shame to add a new member
        // TODO: rename these members to be more generic
        MU_ASSIGN_VEC3D(tmp_snr.pcoord,pnt->coord);

        // Add it to search results
        sr->nodes.push_back(tmp_snr);
      }

      // Add to results list
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
