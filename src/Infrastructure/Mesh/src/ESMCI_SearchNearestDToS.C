// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/ESMCI_Search.h>
#include <Mesh/include/ESMCI_MeshTypes.h>
#include <Mesh/include/ESMCI_MeshObjTopo.h>
#include <Mesh/include/ESMCI_Mapping.h>
#include <Mesh/include/ESMCI_MeshObj.h>
#include <Mesh/include/ESMCI_Mesh.h>
#include <Mesh/include/ESMCI_MeshUtils.h>
#include <Mesh/include/ESMCI_OTree.h>
#include <Mesh/include/ESMCI_Mask.h>
#include <Mesh/include/ESMCI_ParEnv.h>
 
#include <iostream>
#include <fstream>
#include <algorithm>
#include <iterator>

#include <ostream>

#include <set>

#include <limits>
#include <vector>

#include <Mesh/include/ESMCI_BBox.h>
#include <Mesh/include/ESMCI_SpaceDir.h>

using std::vector;


//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {



struct SearchData {
  int sdim;
  double src_pnt[3];

  const MeshObj *closest_dst_node;
  double closest_dist2;  // closest distance squared  
  _field *dst_coord;
};

  static int nearest_func(void *n, void *y, double *min, double *max) {
  MeshObj *dst_node = static_cast<MeshObj*>(n);
  SearchData *sd = static_cast<SearchData*>(y);

 
  // Get dst node coords
  double *c=sd->dst_coord->data(*dst_node);  

  // Convert to 3D point
  double dst_pnt[3];
  dst_pnt[0] = c[0];
  dst_pnt[1] = c[1];
  dst_pnt[2] = (sd->sdim == 3 ? c[2] : 0.0);

  // Calculate squared distance
 double dist2=(sd->src_pnt[0]-dst_pnt[0])*(sd->src_pnt[0]-dst_pnt[0])+
              (sd->src_pnt[1]-dst_pnt[1])*(sd->src_pnt[1]-dst_pnt[1])+
              (sd->src_pnt[2]-dst_pnt[2])*(sd->src_pnt[2]-dst_pnt[2]);


  // If this node is closer than make it the closest node
  if (dist2 < sd->closest_dist2) {
    sd->closest_dst_node=dst_node;
    sd->closest_dist2=dist2;

    // compute a new min-max box
    double dist=sqrt(dist2);

    min[0]=sd->src_pnt[0]-dist;
    min[1]=sd->src_pnt[1]-dist;
    min[2]=sd->src_pnt[2]-dist;

    max[0]=sd->src_pnt[0]+dist;
    max[1]=sd->src_pnt[1]+dist;
    max[2]=sd->src_pnt[2]+dist;
  } else if (dist2 == sd->closest_dist2) {
    // In ParSearchNearest, this can happen when sd->closest_dst_node is NULL
    // so check for that first. 
    if (sd->closest_dst_node != NULL) {
      // If exactly the same distance chose the point with the smallest id
      // (To make things consistent when running on different numbers of procs)
      if (dst_node->get_id() < sd->closest_dst_node->get_id()) {
        sd->closest_dst_node=dst_node;
      }
    } else { // If there is no closest yet, then just use the one you found. 
      sd->closest_dst_node=dst_node;
    }

    // Don't need to change the closest_dist2  because at exactly the same dist 
    
    // Don't need to adjust the min-max box because at exactly the same dist 

  }

  // Don't know if this is the closest, so search further
  return 0;
}

// The main routine
  void SearchNearestDstToSrc(const Mesh &src, const Mesh &dst, int unmappedaction, SearchResult &result) {
  Trace __trace("Search(const Mesh &src, const Mesh &dst, int unmappedaction, SearchResult &result)");


  // Get src fields
  MEField<> *src_coord = src.GetCoordField();


  MEField<> *src_mask = src.GetField("mask");

  
  // Get dst fields
  // NOTE: these are only _field, not an MEField, since there are no master elements.
  _field *dst_coord = dst.Getfield("coordinates_1");
  _field *dst_mask = dst.Getfield("mask_1");


  // Get spatial dim and make sure both have the same
  int sdim=src.spatial_dim();
  if (sdim != dst.spatial_dim()) {
    Throw() << "Meshes must have same spatial dim for search";
  }
  

  // Count unmasked src nodes
  int num_nodes_to_search=0;
  if (dst_mask==NULL) {
    num_nodes_to_search=dst.num_nodes();
  } else {
    MeshDB::const_iterator ni=dst.node_begin(), ne=dst.node_end();
    for (; ni != ne; ++ni) {
      // Get mask value
      double *m=dst_mask->data(*ni);
      
      // Only put objects in if they're not masked
      if (!IS_MASKED(*m)) {
        num_nodes_to_search++;
      }
    }
  }

 
  // Create search tree
  OTree *tree=new OTree(num_nodes_to_search); 

  // Add unmasked nodes to search tree
  double pnt[3];
  if (dst_mask==NULL) {
    MeshDB::const_iterator ni=dst.node_begin(), ne=dst.node_end();
    for (; ni != ne; ++ni) {
      const MeshObj &node = *ni;

      // Get coord value
      double *c=dst_coord->data(node);
      
      // Set coord value in 3D point
      pnt[0] = c[0];
      pnt[1] = c[1];
      pnt[2] = sdim == 3 ? c[2] : 0.0;

      // Add node to search tree
      tree->add(pnt, pnt, (void*)&node);
    }
  } else {
    MeshDB::const_iterator ni=dst.node_begin(), ne=dst.node_end();
    for (; ni != ne; ++ni) {
      const MeshObj &node = *ni;

      // Get mask value
      double *m=dst_mask->data(node);
      
      // Only put objects in if they're not masked
      if (!IS_MASKED(*m)) {

        // Get coord value
        double *c=dst_coord->data(node);
        
        // Set coord value in 3D point
        pnt[0] = c[0];
        pnt[1] = c[1];
        pnt[2] = sdim == 3 ? c[2] : 0.0;
        
        // Add node to search tree
        tree->add(pnt, pnt, (void*)&node);
      }
    }
  }

  // Commit tree
  tree->commit();


  // Load the destination objects into a list
  std::vector<const MeshObj*> src_nlist;
  if (src_mask==NULL) {
    MeshDB::const_iterator ni=src.node_begin(), ne=src.node_end();
    for (; ni != ne; ++ni) {
      const MeshObj &node = *ni;

      src_nlist.push_back(&node);
    }
  } else {
    MeshDB::const_iterator ni=src.node_begin(), ne=src.node_end();
    for (; ni != ne; ++ni) {
      const MeshObj &node = *ni;

      // Get mask value
      double *m=src_mask->data(node);
      
      // Only put objects in if they're not masked
      if (!IS_MASKED(*m)) {
        src_nlist.push_back(&node);
      }
    }
  }


  // Set initial search box to the largest possible
  double pmin[3], pmax[3];

  double min,max;
  if (std::numeric_limits<double>::has_infinity) {
    min= -std::numeric_limits<double>::infinity();
    max= std::numeric_limits<double>::infinity();
  } else {
    min = -std::numeric_limits<double>::max();
    max = std::numeric_limits<double>::max();
  }

  pmin[0] = min;
  pmin[1] = min;
  pmin[2] = min;
  
  pmax[0] = max;
  pmax[1] = max;
  pmax[2] = max;
   
  // Loop the destination points, find hosts.
  for (UInt p = 0; p < src_nlist.size(); ++p) {
    
   const MeshObj &src_node = *src_nlist[p];

    // Get destination node coords
    double *c = src_coord->data(src_node);
    
    // Setup search structure
    SearchData sd;
    sd.sdim=sdim;
    sd.src_pnt[0] = c[0];
    sd.src_pnt[1] = c[1];
    sd.src_pnt[2] = (sdim == 3 ? c[2] : 0.0);
    sd.closest_dst_node=NULL;
    sd.closest_dist2=std::numeric_limits<double>::max();
    sd.dst_coord=dst_coord;

    // Find closest source node to this destination node
    tree->runon_mm_chng(pmin, pmax, nearest_func, (void *)&sd);
    
    // If we've found a nearest source point, then add to the search results list...
    if (sd.closest_dst_node != NULL) {
      Search_result *sr=new Search_result();       
      sr->src_gid=src_node.get_id();
      sr->dst_gid=sd.closest_dst_node->get_id();
      result.push_back(sr);
    } else { // ...otherwise deal with the unmapped point

      if (unmappedaction == ESMCI_UNMAPPEDACTION_ERROR) {
        Throw() << " Some destination points cannot be mapped to the source grid";
      } else if (unmappedaction == ESMCI_UNMAPPEDACTION_IGNORE) {
        // don't do anything
      } else {
        Throw() << " Unknown unmappedaction option";
      }
    }
     
  } // for src nodes
  

  // Get rid of tree
  if (tree) delete tree;
  }



struct CommData {
  double closest_dist;
  int closest_dst_gid;
  int proc;
};


  

// The main routine
  void ParSearchNearestDstToSrc(const Mesh &src, const Mesh &dst, int unmappedaction, SearchResult &result) {
  Trace __trace("Search(const Mesh &dst, const Mesh &src, int unmappedaction, SearchResult &result)");
  //int FindPnts(const Mesh &mesh, int unmappedaction, int dim_pnts, int num_pnts, double *pnts, int *procs, int *gids) {
  //  Trace __trace("FindPnts()");


  //  printf("#%d BEFORE\n",Par::Rank());

  // Get src fields
  MEField<> *src_coord = src.GetCoordField();

  //  printf("#%d AFTER\n",Par::Rank());


  MEField<> *src_mask = src.GetField("mask");

  
  // Get dst fields
  // NOTE: these are only _field, not an MEField, since there are no master elements.
  _field *dst_coord = dst.Getfield("coordinates_1");
  _field *dst_mask = dst.Getfield("mask_1");


  // Get spatial dim and make sure both have the same
  int sdim=dst.spatial_dim();
  if (sdim != src.spatial_dim()) {
    Throw() << "Meshes must have same spatial dim for search";
  }
  

  // Count unmasked source nodes
  int num_nodes_to_search=0;
  if (dst_mask==NULL) {
    num_nodes_to_search=dst.num_nodes();
  } else {
    MeshDB::const_iterator ni=dst.node_begin(), ne=dst.node_end();
    for (; ni != ne; ++ni) {
      // Get mask value
      double *m=dst_mask->data(*ni);
      
      // Only put objects in if they're not masked
      if (!IS_MASKED(*m)) {
        num_nodes_to_search++;
      }
    }
  }

 
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

  if (dst_mask==NULL) {
    MeshDB::const_iterator ni=dst.node_begin(), ne=dst.node_end();
    for (; ni != ne; ++ni) {
      const MeshObj &node = *ni;

      // Get coord value
      double *c=dst_coord->data(node);
      
      // Set coord value in 3D point
      pnt[0] = c[0];
      pnt[1] = c[1];
      pnt[2] = sdim == 3 ? c[2] : 0.0;

      // Add node to search tree
      tree->add(pnt, pnt, (void*)&node);

      // compute proc min max
      if (pnt[0] < proc_min[0]) proc_min[0]=pnt[0];
      if (pnt[1] < proc_min[1]) proc_min[1]=pnt[1];
      if (pnt[2] < proc_min[2]) proc_min[2]=pnt[2];
      
      if (pnt[0] > proc_max[0]) proc_max[0]=pnt[0];
      if (pnt[1] > proc_max[1]) proc_max[1]=pnt[1];
      if (pnt[2] > proc_max[2]) proc_max[2]=pnt[2];
    }
  } else {
    MeshDB::const_iterator ni=dst.node_begin(), ne=dst.node_end();
    for (; ni != ne; ++ni) {
      const MeshObj &node = *ni;

      // Get mask value
      double *m=dst_mask->data(node);
      
      // Only put objects in if they're not masked
      if (!IS_MASKED(*m)) {

        // Get coord value
        double *c=dst_coord->data(node);
        
        // Set coord value in 3D point
        pnt[0] = c[0];
        pnt[1] = c[1];
        pnt[2] = sdim == 3 ? c[2] : 0.0;
        
        // Add node to search tree
        tree->add(pnt, pnt, (void*)&node);

        // compute proc min max
        if (pnt[0] < proc_min[0]) proc_min[0]=pnt[0];
        if (pnt[1] < proc_min[1]) proc_min[1]=pnt[1];
        if (pnt[2] < proc_min[2]) proc_min[2]=pnt[2];
        
        if (pnt[0] > proc_max[0]) proc_max[0]=pnt[0];
        if (pnt[1] > proc_max[1]) proc_max[1]=pnt[1];
        if (pnt[2] > proc_max[2]) proc_max[2]=pnt[2];
      }
    }
  }

  // Commit tree
  tree->commit();


  // Create SpaceDir
  SpaceDir *spacedir=new SpaceDir(proc_min, proc_max, tree);


  // Load the destination objects into a list
  std::vector<const MeshObj*> src_nlist;
  if (src_mask==NULL) {
    MeshDB::const_iterator ni=src.node_begin(), ne=src.node_end();
    for (; ni != ne; ++ni) {
      const MeshObj &node = *ni;

      src_nlist.push_back(&node);
    }
  } else {
    MeshDB::const_iterator ni=src.node_begin(), ne=src.node_end();
    for (; ni != ne; ++ni) {
      const MeshObj &node = *ni;

      // Get mask value
      double *m=src_mask->data(node);
      
      // Only put objects in if they're not masked
      if (!IS_MASKED(*m)) {
        src_nlist.push_back(&node);
      }
    }
  }
  

  //// Find the closest point locally ////

  // Set initial search box to the largest possible
  double pmin[3], pmax[3];
  pmin[0] = min;  pmin[1] = min;  pmin[2] = min;
  pmax[0] = max;  pmax[1] = max;  pmax[2] = max;
   
  // Allocate space to hold closest gids, dist
  vector<int> closest_dst_gid(src_nlist.size(),-1);
  vector<double> closest_dist(src_nlist.size(),std::numeric_limits<double>::max());
  // Loop the destination points, find hosts.
  for (UInt p = 0; p < src_nlist.size(); ++p) {
    
   const MeshObj &node = *src_nlist[p];

    // Get destination node coords
    double *c = src_coord->data(node);
    
    // Setup search structure
    SearchData sd;
    sd.sdim=sdim;
    sd.src_pnt[0] = c[0];
    sd.src_pnt[1] = c[1];
    sd.src_pnt[2] = (sdim == 3 ? c[2] : 0.0);
    sd.closest_dst_node=NULL;
    sd.closest_dist2=std::numeric_limits<double>::max();
    sd.dst_coord=dst_coord;

    // Find closest source node to this destination node
    tree->runon_mm_chng(pmin, pmax, nearest_func, (void *)&sd);
    
    // If we've found a nearest source point, then add to the search results list...
    if (sd.closest_dst_node != NULL) {
      closest_dst_gid[p]=sd.closest_dst_node->get_id();
      closest_dist[p]=sqrt(sd.closest_dist2);
    }
  }

  
  // Get list of procs where a point can be located
  vector< vector<int> > proc_lists;  // List of procs
  proc_lists.resize(src_nlist.size());

  for (int i=0; i<src_nlist.size(); i++) {
    const MeshObj &node = *src_nlist[i];

    // Get destination node pnt
    double *pnt = src_coord->data(node);

    // Get minimum distance found so far
    double dist=closest_dist[i];

    //// Point min max we expand the point by the
    double pnt_min[3], pnt_max[3];
    
    pnt_min[0] = pnt[0]-dist;
    pnt_min[1] = pnt[1]-dist;
    pnt_min[2] = (sdim == 3) ? pnt[2]-dist : 0.0;

    pnt_max[0] = pnt[0]+dist;
    pnt_max[1] = pnt[1]+dist;
    pnt_max[2] = (sdim == 3) ? pnt[2]+dist : 0.0;

    //// Init vector
    proc_lists[i].clear();

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

  // Get the number of processors used below
  int num_procs=Par::Size();

  // Figure out which procs we're sending to
  int num_snd_procs;
  vector<int> snd_pets;       
  vector< vector<int> > snd_inds; 
  // {
    vector< vector<int> > tmp_snd_inds; 
    tmp_snd_inds.resize(num_procs);
    for (int i=0; i<src_nlist.size(); i++) {
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

      // Get coords of node
      const MeshObj &node = *src_nlist[ind];
      double *pnt = src_coord->data(node);

      // pack buf
      double buf[4]; // 4 is biggest this should be (i.e. 3D+dist)
      buf[0]=pnt[0];
      buf[1]=pnt[1];
      if (sdim < 3) buf[2]=closest_dist[ind];
      else {
        buf[2]=pnt[2];
        buf[3]=closest_dist[ind];
      }

      // Push buf onto send struct
      b->push((const UChar *)buf, (UInt)snd_size);
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
    int num_msgs=b->msg_size()/snd_size;

    // build temporary vector
    vector<CommData> tmp;
    tmp.reserve(num_msgs); // reserve to save allocation time
   
    // Unpack everything from this processor
    while (!b->empty()) {
      double buf[4]; // 4 is biggest this should be (i.e. 3D+dist)

 /* XMRKX */
      b->pop((UChar *)buf, (UInt)snd_size);
      //      printf(" [%f %f %f], ",pnt[0],pnt[1],pnt[2]);

      // Unpack buf
      double pnt[3]={0.0,0.0,0.0};
      double dist=0.0;

      pnt[0]=buf[0];
      pnt[1]=buf[1];
      if (sdim < 3) dist=buf[2];
      else {
        pnt[2]=buf[2];
        dist=buf[3];
      }

      //// Point min max we expand the point by the distance to the closest point
      double pmin[3], pmax[3];
      
      pmin[0] = pnt[0]-dist;
      pmin[1] = pnt[1]-dist;
      pmin[2] = (sdim == 3) ? pnt[2]-dist : 0.0;
      
      pmax[0] = pnt[0]+dist;
      pmax[1] = pnt[1]+dist;
      pmax[2] = (sdim == 3) ? pnt[2]+dist : 0.0;
      

      // Setup search structure
      SearchData sd;
      sd.sdim=sdim;
      sd.src_pnt[0] = pnt[0];
      sd.src_pnt[1] = pnt[1];
      sd.src_pnt[2] = (sdim == 3 ? pnt[2] : 0.0);
      sd.closest_dst_node=NULL;
      sd.closest_dist2=dist*dist;
      sd.dst_coord=dst_coord;

      // Find closest source node to this destination node
      tree->runon_mm_chng(pmin, pmax, nearest_func, (void *)&sd);
      
      // Fill in structure to be sent
      CommData cd;
      if (sd.closest_dst_node != NULL) {
        cd.closest_dist=sqrt(sd.closest_dist2);
        cd.closest_dst_gid=sd.closest_dst_node->get_id();
      } else {
        cd.closest_dist=std::numeric_limits<double>::max();;
        cd.closest_dst_gid=-1;
      }
      cd.proc=Par::Rank();
      
      // Put into temporary list
      tmp.push_back(cd);
    }
    //    printf(" \n ");

    // Put temporary list into long list
    rcv_results.push_back(tmp);
  }

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


  // Unpack CommData and generate results
  vector<CommData> pnt_results;  
  CommData init_cd;
  init_cd.closest_dist = std::numeric_limits<double>::max();
  init_cd.closest_dst_gid=-2;
  init_cd.proc=-1;
  pnt_results.resize(src_nlist.size(),init_cd); // allocate space for pnt_results and initialize
  for (std::vector<UInt>::iterator p = comm_to_home.inProc_begin(); p != comm_to_home.inProc_end(); ++p) {
    UInt proc = *p;
    SparseMsg::buffer *b = comm_to_home.getRecvBuffer(proc);

    // Figure out how many messages we have
    int num_msgs=b->msg_size()/snd_size; // ??? IS THIS RIGHT ???
   
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
      if (cd.closest_dst_gid > -1) {
        if (cd.closest_dist < closest_dist[pnt_ind]) {
          closest_dist[pnt_ind]=cd.closest_dist;
          closest_dst_gid[pnt_ind]=cd.closest_dst_gid;
        } else if (cd.closest_dist == closest_dist[pnt_ind]) {
          // If exactly the same distance chose the point with the smallest id
          // (To make things consistent when running on different numbers of procs)
          if (cd.closest_dst_gid < closest_dst_gid[pnt_ind]) {
            closest_dist[pnt_ind]=cd.closest_dist;
            closest_dst_gid[pnt_ind]=cd.closest_dst_gid;
          }
        }
      }

      // next result
      j++;
    }
    //    printf(" \n ");

  }


  // Do output based on CommData
  result.clear();
  for (int i=0; i<src_nlist.size(); i++) {
    if (closest_dst_gid[i] > -1) {
      const MeshObj &src_node = *src_nlist[i];

      //      printf("#%d src id=%d %d closest dst id=%d \n",Par::Rank(),src_node.get_id(),GetAttr(src_node).is_locally_owned(),closest_dst_gid[i]);

      // We've found a nearest source point, so add to results list
      Search_result *sr=new Search_result();       

      sr->src_gid=src_node.get_id();
      sr->dst_gid=closest_dst_gid[i];

      result.push_back(sr);

    } else {  

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
  for (int i=0; i<rcv_pets.size(); i++) {
    printf(" %d# to %d pnt # = ",Par::Rank(),rcv_pets[i]);
    for (int j=0; j<rcv_results[i].size(); j++) {
      printf(" %d ",rcv_results[i][j].elem_id);
    }
    printf(" \n");
  }
#endif

}



} // namespace
