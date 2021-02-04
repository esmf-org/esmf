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
#include <Mesh/include/Legacy/ESMCI_FindPnts.h>
#include <Mesh/include/Legacy/ESMCI_MeshTypes.h>
#include <Mesh/include/Legacy/ESMCI_MeshObjTopo.h>
#include <Mesh/include/Regridding/ESMCI_Mapping.h>
#include <Mesh/include/Legacy/ESMCI_MeshObj.h>
#include <Mesh/include/ESMCI_Mesh.h>
#include <Mesh/include/Legacy/ESMCI_MeshUtils.h>
#include <Mesh/include/ESMCI_OTree.h>
#include <Mesh/include/Legacy/ESMCI_BBox.h>
#include <Mesh/include/Legacy/ESMCI_ParEnv.h>
#include <Mesh/include/Legacy/ESMCI_SparseMsg.h>
#include <Mesh/include/Regridding/ESMCI_SpaceDir.h>
#include "stdlib.h"
#include <Mesh/include/Legacy/ESMCI_Exception.h>

#include <algorithm>
#include <iterator>

#include <ostream>

#include <set>

#include <limits>
#include <vector>

using std::set;
using std::vector;


//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

static void make_search_info_from_mesh_elems(const Mesh &src, double nexp,
                                             OTree **o_tree, double *proc_min, double *proc_max) {

  // Count number of elems to go into Tree
  int num_elems = 0;

  /////  For now just put all active elements in ///
  KernelList::const_iterator ki = src.set_begin(), ke = src.set_end();

  for (; ki != ke; ++ki) {
    const Kernel &ker = *ki;

    if (ker.type() != MeshObj::ELEMENT || !ker.is_active()) continue;

    Kernel::obj_const_iterator ei = ker.obj_begin(), ee = ker.obj_end();

    for (; ei != ee; ++ei) {
      num_elems++;
    }

  }

  // Create Tree
  OTree *tree=new OTree(num_elems);

  // Init proc min and max
  proc_min[0]=std::numeric_limits<double>::max();
  proc_min[1]=std::numeric_limits<double>::max();
  proc_min[2]=std::numeric_limits<double>::max();

  proc_max[0]=std::numeric_limits<double>::min();
  proc_max[1]=std::numeric_limits<double>::min();
  proc_max[2]=std::numeric_limits<double>::min();


  // Load elems into tree and calc min/max box of elems
  MEField<> &coord_field = *src.GetCoordField();

  // Get spatial dim of mesh
  UInt sdim = src.spatial_dim();

  // Loop through elems adding them to the tree
  for (ki = src.set_begin(); ki != ke; ++ki) {
    const Kernel &ker = *ki;

    if (ker.type() != MeshObj::ELEMENT || !ker.is_active()) continue;

    Kernel::obj_const_iterator ei = ker.obj_begin(), ee = ker.obj_end();

    MasterElement<> &cme = *GetME(coord_field, ker)(METraits<>());

    std::vector<double> node_coord(cme.num_functions()*src.spatial_dim());

    for (; ei != ee; ++ei) {
      const MeshObj &elem = *ei;

      // Calculate min and max of element
       BBox bounding_box(coord_field, elem, nexp);

       double min[3], max[3];

       min[0] = bounding_box.getMin()[0];
       min[1] = bounding_box.getMin()[1];
       if (sdim >2) min[2] = bounding_box.getMin()[2];
       else min[2] = 0.0;

       max[0] = bounding_box.getMax()[0];
       max[1] = bounding_box.getMax()[1];
       if (sdim >2) max[2] = bounding_box.getMax()[2];
       else  max[2] = 0.0;

       //       printf("ELEM id=%d min=%20.17f %20.17f %20.17f max=%20.17f %20.17f %20.17f \n",elem.get_id(),min[0],min[1],min[2],max[0],max[1],max[2]);

       // Add element to search tree
       tree->add(min, max, (void*)&elem);

       // Compute proc min and max
       if (min[0] < proc_min[0]) proc_min[0]=min[0];
       if (min[1] < proc_min[1]) proc_min[1]=min[1];
       if (min[2] < proc_min[2]) proc_min[2]=min[2];

       if (max[0] > proc_max[0]) proc_max[0]=max[0];
       if (max[1] > proc_max[1]) proc_max[1]=max[1];
       if (max[2] > proc_max[2]) proc_max[2]=max[2];

    }
  }

  // Commit tree
  tree->commit();

  // Output tree
  *o_tree=tree;
}

struct CommData {
  bool investigated;
  double best_dist;
  int elem_id;
  bool is_in;
  bool elem_masked;
  int proc;
};


  bool is_better_CommData(CommData *new_cd, CommData *old_cd) {

  if (!new_cd->investigated) return false;
  if (!old_cd->investigated) return true;

  // At this point they're both investigated = true

  // If we already have a candiate then choose if new_cd guy is better
  if (old_cd->is_in) {

    // Only if the new_cd guy is also in is he possibly better
    if (new_cd->is_in) {
      // Only if the new_cd guy isn't masked and has a smaller id is he better
      if (!new_cd->elem_masked && (new_cd->elem_id < old_cd->elem_id)) return true;
      else return false;
    } else {
      return false;
    }
  } else {

    // If new guy is in then he's better
    if (new_cd->is_in) {
      return true;
    } else {
      // noone is_in so pick guy with smaller distance
      if (new_cd->best_dist < old_cd->best_dist) return true;
      else return false;
    }

    return false;
  }



}


struct SearchData {

bool investigated;
double coords[3];
double best_dist;
MEField<> *coord_field;
MEField<> *mask_field_ptr;
MeshObj *elem;
bool is_in;
bool elem_masked;
};


  static int found_func(void *c, void *y) {
  MeshObj &elem = *static_cast<MeshObj*>(c);
  SearchData &si = *static_cast<SearchData*>(y);

  // if we already have some one, then make sure this guy has a smaller id
  if (si.is_in && (elem.get_id()>si.elem->get_id())) return 0;


  // Get kernel
  const Kernel &ker = *elem.GetKernel();

    // Setup for source masks, if used
  std::vector<double> src_node_mask;
  MasterElement<> *mme;
  MEField<> *mask_field_ptr = si.mask_field_ptr;
  if (mask_field_ptr != NULL) {
    mme=GetME(*mask_field_ptr, ker)(METraits<>());
    src_node_mask.resize(mme->num_functions(),0.0);
  }

    // Set src mask status
    bool elem_masked=false;
    if (mask_field_ptr != NULL && !src_node_mask.empty()) {
      GatherElemData<>(*mme, *mask_field_ptr, elem, &src_node_mask[0]);
      for (int i=0; i< mme->num_functions(); i++) {
        if (src_node_mask[i] > 0.5) {
          elem_masked=true;
          break;
        }
      }
    }


    // If we're masked and we already have someone then continue
    // NOTE: if we ever care about finding the lowest gid masked elem
    //       will have to change this
    if (elem_masked && si.is_in) return 0;


    // Do the is_in calculation
  const MappingBase &map = GetMapping(elem);
  double pcoord[3];
  double dist;

  const MeshObjTopo *etopo = GetMeshObjTopo(elem);

  MasterElement<> &cme = *GetME(*si.coord_field, ker)(METraits<>());

  std::vector<double> node_coord(cme.num_functions()*etopo->spatial_dim);

  GatherElemData<>(cme, *si.coord_field, elem, &node_coord[0]);

  bool in = map.is_in_cell(&node_coord[0], si.coords, &pcoord[0], &dist);


  // We've looked at this, so mark it as so
  si.investigated = true;

  // Set info based on search
  if (in) {
    //   std::copy(pcoord, pcoord+etopo->spatial_dim, &si.snr.pcoord[0]);
    si.best_dist = 0.0;
    si.elem = &elem;
    si.is_in=true;
    si.elem_masked=elem_masked;
  } else if (!si.is_in && (dist < si.best_dist)) {
    // Set up fallback candidate.
    //   std::copy(pcoord, pcoord+etopo->spatial_dim, &si.snr.pcoord[0]);
    si.best_dist = dist;
    si.elem = &elem;
    si.elem_masked=elem_masked;
  }

  //  return in&&!elem_masked ? 1 : 0;
  return 0;
}


bool is_found(CommData *cd, double search_tol) {

  // If we haven't investigated then we haven't found anything
  if (!cd->investigated) return false;

  // If what we found is masked then we haven't found anything
  if (cd->elem_masked) return false;

  // We're in an unmasked element
  if (cd->is_in) return true;

  // We're within tolerence to an unmasked element
  if (cd->best_dist <= search_tol) return true;

  // We must not have found anything, oh well...
  return false;
}



// The main routine
int FindPnts(const Mesh &mesh, int unmappedaction, int dim_pnts, int num_pnts, double *pnts, int *procs, int *gids) {
    Trace __trace("FindPnts()");

  // Set some parameters for seach, eventually move these to .h or get rid of
  const double normexp = 0.15;
  const double search_tol = 1e-8;


  // Some error checking
  if (mesh.spatial_dim() != dim_pnts) {
    return ESMCI_FINDPNT_DIM_MISMATCH;
    // Throw having problems: Throw() << "Meshes must have same spatial dim for search";
  }


  // Get field info
  MEField<> *coord_field_ptr = mesh.GetCoordField();
  MEField<> *mask_field_ptr = mesh.GetField("mask");


  // Create search tree and proc min/max from mesh elements
  OTree *tree=NULL;
  double proc_min[3], proc_max[3];
  make_search_info_from_mesh_elems(mesh, normexp, &tree, proc_min, proc_max);


  // Create SpaceDir
  SpaceDir *spacedir=new SpaceDir(proc_min, proc_max, tree);


  // Get list of procs where a point can be located
  vector< vector<int> > proc_lists;  // List of procs
  proc_lists.resize(num_pnts);

  for (int i=0; i<num_pnts; i++) {

    //// Init vector
    proc_lists[i].clear();

    //// point
    double *pnt=pnts+(i*dim_pnts);

    //// Point min max we expand the point by the search tolerence
    double pnt_min[3], pnt_max[3];

    pnt_min[0] = pnt[0]-search_tol;
    pnt_min[1] = pnt[1]-search_tol;
    pnt_min[2] = (dim_pnts == 3) ? pnt[2]-search_tol : -search_tol;

    pnt_max[0] = pnt[0]+search_tol;
    pnt_max[1] = pnt[1]+search_tol;
    pnt_max[2] = (dim_pnts == 3) ? pnt[2]+search_tol : +search_tol;


    //// Search on point, plus or minus search_tolerence
    spacedir->get_procs(pnt_min, pnt_max, &(proc_lists[i])); // Definitely need () around proc_lists

#if 0
    printf(" %d# Pnt=[%f %f %f] sending to = ",Par::Rank(),pnt[0],pnt[1],pnt[2]);
    for (int j=0; j<proc_lists[i].size(); j++) {
      printf(" %d ",proc_lists[i][j]);
    }
    printf(" \n");
#endif
  }

  // Now that we're not using it anymore get rid of SpaceDir
  delete spacedir;

  // Get the number of processors used below
  int num_procs=Par::Size();

  // Figure out which procs we're sending to
  int num_snd_procs;
  vector<int> snd_pets;
  vector< vector<int> > snd_inds;
  // {
    vector< vector<int> > tmp_snd_inds;
    tmp_snd_inds.resize(num_procs);
    for (int i=0; i<num_pnts; i++) {
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


  // Calculate send sizes
  vector<int> snd_sizes;
  snd_sizes.resize(num_snd_procs,0); // resize and init to 0
  for (int i=0; i< num_snd_procs; i++) {
    snd_sizes[i]=dim_pnts*sizeof(double)*snd_inds[i].size();
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
      double *pnt=pnts+(dim_pnts*snd_inds[i][j]);
      b->push((const UChar *)pnt, (UInt)dim_pnts*sizeof(double));
    }
  }


  // Communicate point information
  comm.communicate();


  // Unpack point info and call function
  vector<int> rcv_pets;
  vector< vector<CommData> > rcv_results;
  for (std::vector<UInt>::iterator p = comm.inProc_begin(); p != comm.inProc_end(); ++p) {
    UInt proc = *p;
    SparseMsg::buffer *b = comm.getRecvBuffer(proc);

    // Add proc
    rcv_pets.push_back(proc);

    //    printf("%d # From %d pnts=",Par::Rank(),proc);

    // Figure out how many messages we have
    int num_msgs=b->msg_size()/(dim_pnts*sizeof(double));

    // build temporary vector
    vector<CommData> tmp;
    tmp.reserve(num_msgs); // reserve to save allocation time

    // Unpack everything from this processor
    while (!b->empty()) {
      double pnt[3]={0.0,0.0,0.0};

      b->pop((UChar *)pnt, (UInt)dim_pnts*sizeof(double));
      //      printf(" [%f %f %f], ",pnt[0],pnt[1],pnt[2]);


      // Setup to run on intersecting boxes
      //// Create minmax box from pnt
      double pnt_min[3], pnt_max[3];

      pnt_min[0] = pnt[0]-search_tol;
      pnt_min[1] = pnt[1]-search_tol;
      pnt_min[2] = (dim_pnts == 3) ? pnt[2]-search_tol : -search_tol;

      pnt_max[0] = pnt[0]+search_tol;
      pnt_max[1] = pnt[1]+search_tol;
      pnt_max[2] = (dim_pnts == 3) ? pnt[2]+search_tol : +search_tol;


      /// Setup search data
      SearchData sd;
      sd.investigated = false;
      sd.best_dist = std::numeric_limits<double>::max();
      sd.coord_field = coord_field_ptr;
      sd.mask_field_ptr = mask_field_ptr;
      sd.is_in=false;
      sd.elem_masked=false;
      sd.elem=NULL;
      sd.coords[0] = pnt[0];
      sd.coords[1] = pnt[1];
      sd.coords[2] = pnt[2];


      // do search
      tree->runon(pnt_min, pnt_max, found_func, (void*)&sd);

      // Fill in structure to be sent
      CommData cd;
      cd.investigated = sd.investigated;
      cd.best_dist = sd.best_dist;
      if (sd.investigated) cd.elem_id=sd.elem->get_id();
      else cd.elem_id=-1;
      cd.is_in=sd.is_in;
      cd.elem_masked=sd.elem_masked;
      cd.proc=Par::Rank();

      //     printf("PNT pnt_min=%20.17f %20.17f %20.17f pnt_max=%20.17f %20.17f %20.17f \n",pnt_min[0],pnt_min[1],pnt_min[2],pnt_max[0],pnt_max[1],pnt_max[2]);


      // Put into temporary list
      tmp.push_back(cd);

    }
    //    printf(" \n ");

    // Put temporary list into long list
    rcv_results.push_back(tmp);
  }


  // Now that we're not using it anymore get rid of tree
  if (tree != NULL) delete tree;

  // Calculate size to send back to pnt's home proc
  vector<int> rcv_sizes;
  rcv_sizes.resize(rcv_pets.size(),0); // resize and init to 0
  for (int i=0; i< rcv_pets.size(); i++) {
    rcv_sizes[i]=sizeof(CommData)*rcv_results[i].size();
  }


  // Create communication structure spatial to home
  SparseMsg comm_to_home;

  // Setup pattern and sizes
  if (!rcv_pets.empty()) {
    comm_to_home.setPattern(rcv_pets.size(), (const UInt *)&(rcv_pets[0]));
    comm_to_home.setSizes((UInt *)&(rcv_sizes[0]));
  } else {
    comm_to_home.setPattern(0, (const UInt *)NULL);
    comm_to_home.setSizes((UInt *)NULL);
  }

  // Reset buffers
  comm_to_home.resetBuffers();

  // Pack points into buffers
  for (int i=0; i< rcv_pets.size(); i++) {
    SparseMsg:: buffer *b=comm_to_home.getSendBuffer(rcv_pets[i]);
    for (int j=0; j<rcv_results[i].size(); j++) {
      b->push((const UChar *)&(rcv_results[i][j]), (UInt)sizeof(CommData));
    }
  }

  // Communicate point information
  comm_to_home.communicate();


#if 1
  // No points so can skip rest of processing
  // FROM NOW ON CAN ASSUME num_pnts > 0
  if (num_pnts <= 0)  return ESMCI_FINDPNT_SUCCESS;


  ///// Unpack CommData and generate results /////
  // Allocate results array
  CommData *pnt_results = NULL;
  pnt_results = new CommData[num_pnts];

  // Init structures
  for (int i=0; i<num_pnts; i++) {
    CommData *cd=pnt_results+i;

    cd->investigated = false;
    cd->best_dist = std::numeric_limits<double>::max();
    cd->is_in=false;
    cd->elem_masked=false;
    cd->elem_id=-2;
    cd->proc=-1;
  }

  // Loop processing CommData from other processors
  for (std::vector<UInt>::iterator p = comm_to_home.inProc_begin(); p != comm_to_home.inProc_end(); ++p) {
    UInt proc = *p;
    SparseMsg::buffer *b = comm_to_home.getRecvBuffer(proc);

    // Figure out how many messages we have
    int num_msgs=b->msg_size()/(dim_pnts*sizeof(double));

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
      if (is_better_CommData(&cd,&(pnt_results[pnt_ind]))) pnt_results[pnt_ind]=cd;

      // next result
      j++;
    }
  }


#else
  // OLD WAY
  // Unpack CommData and generate results
  vector<CommData> pnt_results;
  CommData init_cd;
  init_cd.investigated = false;
  init_cd.best_dist = std::numeric_limits<double>::max();
  init_cd.is_in=false;
  init_cd.elem_masked=false;
  init_cd.elem_id=-2;
  init_cd.proc=-1;

  pnt_results.resize(num_pnts,init_cd); // allocate space for pnt_results and initialize

  for (std::vector<UInt>::iterator p = comm_to_home.inProc_begin(); p != comm_to_home.inProc_end(); ++p) {
    UInt proc = *p;
    SparseMsg::buffer *b = comm_to_home.getRecvBuffer(proc);

    // Figure out how many messages we have
    int num_msgs=b->msg_size()/(dim_pnts*sizeof(double));

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
      if (is_better_CommData(&cd,&(pnt_results[pnt_ind]))) pnt_results[pnt_ind]=cd;

      // next result
      j++;
    }

  }
#endif


  // Do output based on CommData
  for (int i=0; i<num_pnts; i++) {
    // record info about search
    if (is_found(&(pnt_results[i]),search_tol)) {
      if (procs) procs[i]=pnt_results[i].proc;
      if (gids) gids[i]=pnt_results[i].elem_id;
    } else {
      if (procs) procs[i]=-1;
      if (gids) gids[i]=-1;

      //// check possible error condition
      if (unmappedaction == ESMCI_UNMAPPEDACTION_ERROR) {
        return ESMCI_FINDPNT_PNT_NOT_FOUND;
        // Throw having problems: Throw() << " Some destination points cannot be mapped to source grid";
      } else if (unmappedaction == ESMCI_UNMAPPEDACTION_IGNORE) {
        // don't do anything
      } else {
        return ESMCI_FINDPNT_UNKNOWN;
        // Throw having problems: Throw() << " Unknown unmappedaction option";
      }
    }
  }

#if 0
  for (int i=0; i<rcv_pets.size(); i++) {
    printf(" %d# to %d pnt # = ",Par::Rank(),rcv_pets[i]);
    for (int j=0; j<rcv_results[i].size(); j++) {
      printf(" %d ",rcv_results[i][j].elem_id);
    }
    printf(" \n");
  }
#endif

  // Cleanup
  if (pnt_results != NULL) delete [] pnt_results;

  // return success
  return ESMCI_FINDPNT_SUCCESS;
}


} // namespace
