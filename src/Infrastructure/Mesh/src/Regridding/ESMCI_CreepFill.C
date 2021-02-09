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
#include "Mesh/include/Legacy/ESMCI_MeshObjConn.h"

#include <iostream>
#include <fstream>
#include <algorithm>
#include <iterator>

#include <ostream>

#include <map>
#include <limits>
#include <vector>
using std::vector;
using std::map;




//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

  struct CreepNode;

  // Prototypes used below
  static void _calc_level_1_weights_from_CreepNode(CreepNode *cnode, std::vector<int>& wgt_ids, std::vector<double>& wgt_vals);
  static void _calc_level_gt1_weights_from_CreepNode(CreepNode *cnode, std::vector<int>& wgt_ids, std::vector<double>& wgt_vals);

  //#define ESMF_REGRID_DEBUG_CREEP_NODE 11454348

  //int max_packed_buff_size=0;

  bool creep_debug=false;

  struct CreepNode {    
    int level; 

    int gid;
    double pnt[3];

    MeshObj *node;

    // Donors
    vector<CreepNode *> donors;

    // Weight information
    // TODO: once this is filled, can probably get rid of donors
    vector<int> wgt_ids;
    vector<double> wgt_vals;

    // Constructors
    CreepNode() : level(-1), gid(-1), node(NULL) {
      pnt[0]=0.0;
      pnt[1]=0.0;
      pnt[2]=0.0;
    }


    CreepNode(int sdim, MEField<> *cfield, int _level, MeshObj *_node) : level(_level), node(_node) {

      // Get id of node
      gid=_node->get_id();

      // Get coordinates of node
      double *c=cfield->data(*_node);

      // Set point coords 
      pnt[0] = c[0];
      pnt[1] = c[1];
      pnt[2] = (sdim == 3 ? c[2] : 0.0);
    }


    CreepNode(UChar *buff, Mesh &mesh, map<int,CreepNode> &creep_map){

      // Offset
      UInt off=0;      

      // Level
      level=*((int *)(buff+off));
      off +=sizeof(int);

      // gid
      gid=*((int *)(buff+off));
      off +=sizeof(int);

      // point
      pnt[0]=*((double *)(buff+off));
      off +=sizeof(double);

      pnt[1]=*((double *)(buff+off));
      off +=sizeof(double);

      pnt[2]=*((double *)(buff+off));
      off +=sizeof(double);

      // Can't send node pointer, so get it from mesh, or set to NULL
      node=NULL;
      Mesh::MeshObjIDMap::iterator mi =  mesh.map_find(MeshObj::NODE, gid);
      if (mi != mesh.map_end(MeshObj::NODE)) {
        node=&*mi; 
      }

      // number of donors
      int num_donors=*((int *)(buff+off));
      off +=sizeof(int);

      // resize vector to hold donors
      donors.reserve(num_donors);

      // Get donors
      for (int i=0; i<num_donors; i++) {

        // Get donor gid
        int donor_gid=*((int *)(buff+off));
        off +=sizeof(int);

        // Get creep node from map using donor_gid
        map<int,CreepNode>::iterator mi = creep_map.find(donor_gid);
        if (mi == creep_map.end()) {
          Throw() << "donor creep node not present when it should have already been added."; 
        }
        
        // Get pointer to donor
        CreepNode *donor_cnode=&(mi->second);

        // Add to vector
        donors.push_back(donor_cnode);
      }


      //// Unpack weights ////

      // number of weights
      int num_wgt=*((int *)(buff+off));
      off +=sizeof(int);

      // resize vector to hold weights
      wgt_ids.resize(num_wgt);
      wgt_vals.resize(num_wgt);

      // Loop unpacking weights
      for (int w=0; w<num_wgt; w++) {

          // Get weight id
          int wgt_id=*((int *)(buff+off));
          off +=sizeof(int);

          // Get weight val
          double wgt_val=*((double *)(buff+off));
          off +=sizeof(double);

          // Add to weight vectors
          wgt_ids[w]=wgt_id;
          wgt_vals[w]=wgt_val;
      }

    }

    UInt packed_size() {

      // Init
      UInt size=0;
      
      // Level
      size += sizeof(int);

      // gid
      size += sizeof(int);

      // Point
      size += 3*sizeof(double);

      // Don't pack node pointer
      
      // Number of donors 
      size += sizeof(int);

      // Donor size 
      size += sizeof(int)*donors.size();

      // Number of weights
      size +=sizeof(int);

      // Size of weights
      size += sizeof(int)*wgt_ids.size();
      size += sizeof(double)*wgt_ids.size(); // wgt_ids.size() == wgt_vals.size()

      // Output size
      return size;
    }

     void pack(UChar *buff) {

      // Offset
      UInt off=0;      

      // Level
      *((int *)(buff+off))=level;
      off +=sizeof(int);

      // gid
      *((int *)(buff+off))=gid;
      off +=sizeof(int);

      // point
      *((double *)(buff+off))=pnt[0];
      off +=sizeof(double);

      *((double *)(buff+off))=pnt[1];
      off +=sizeof(double);

      *((double *)(buff+off))=pnt[2];
      off +=sizeof(double);

      // Don't pack node pointer

      // number of donors at this level
      *((int *)(buff+off))=donors.size();
      off +=sizeof(int);

      // donor gids at this level
      for (int i=0; i<donors.size(); i++) {
        *((int *)(buff+off))=donors[i]->gid;
        off +=sizeof(int);
      }

      // number of weights
      *((int *)(buff+off))=wgt_ids.size();
      off +=sizeof(int);

      // Loop packing weights
      for (int i=0; i<wgt_ids.size(); i++) {
        *((int *)(buff+off))=wgt_ids[i];
        off +=sizeof(int);
        *((double *)(buff+off))=wgt_vals[i];
        off +=sizeof(double);
      }

     }

    static UInt packed_size_from_buff(UChar *buff) {

      // size
      UInt size=0;      

      // Level
      size += sizeof(int);

      // gid
      size += sizeof(int);

      // point
      size += sizeof(double);
      size += sizeof(double);
      size += sizeof(double);

      // Don't pack node pointer

      // number of donors
      int num_donors=*((int *)(buff+size));
      size += sizeof(int);

      // donor ids
      size += num_donors*sizeof(int);
      
      // number of weights
      int num_wgt=*((int *)(buff+size));
      size +=sizeof(int);
      
      // Size of weights
      size += num_wgt*sizeof(int);
      size += num_wgt*sizeof(double); 
      
      // return size
      return size;
    }
    
    static int gid_from_buff(UChar *buff) {
      
      // Offset
      UInt off=0;      

      // Level
      off +=sizeof(int);

      // gid
      int gid=*((int *)(buff+off));

      // return gid
      return gid;
    }

    void add_donor(CreepNode *cn) {

      // If the creep node is already in the list, then leave
      for (int i=0; i<donors.size(); i++) {
        if (cn->gid == donors[i]->gid) return;
      }

#ifdef ESMF_REGRID_DEBUG_CREEP_NODE
      if (gid == ESMF_REGRID_DEBUG_CREEP_NODE) {
        printf("%d# cn id=%d adding donor cn id=%d \n",Par::Rank(),gid,cn->gid);
      }
#endif

      // If not there, add to list
      donors.push_back(cn);
    } 

    void convert_donors_to_weights() {

      // Calc weights based on level
      if (level < 1) {
        // If level 0, skip (there will be no weights)
        return;       
      } else if (level == 1) {
        // If level 1, calc weights just from donor info
        _calc_level_1_weights_from_CreepNode(this, wgt_ids, wgt_vals);
      } else {
        // If level >1, calc weights from the donor's weights
        _calc_level_gt1_weights_from_CreepNode(this, wgt_ids, wgt_vals);
      }

      // Get rid of donors (because we've used them for everything we need them for)
      // THIS ALSO HAS THE NICE EFFECT OF STOPPING THE PACKING AT ONE LEVEL OF DONORS
      // INSTEAD OF IT RECURSING ALL THE WAY DOWN, SAVING MEMORY FOR THAT BUFFER
      vector<CreepNode *>().swap(donors);
    }

    void add_weights_to_WMat(WMat &wts) {

      // If level 0 skip (there will be no weights)
      if (level < 1) return;

      // Set row info (i.e. the destination id associated with the above weight)
      IWeights::Entry row(gid, 0, 0.0, 0);

      // Convert internal weights to WMat cols
      std::vector<IWeights::Entry> cols;
      cols.reserve(wgt_ids.size());      

      for (int i=0; i<wgt_ids.size(); i++) {
        // Set col entry info 
        IWeights::Entry col_entry(wgt_ids[i], 0, wgt_vals[i], 0);

        // Push into cols
        cols.push_back(col_entry);    
      }

      // Add to weight matrix
      wts.InsertRow(row, cols);
    }



  };

  // Prototypes for local subroutines
  static void _write_level(const char *filename, Mesh &mesh, vector<CreepNode *> &level);
  static void _get_node_nbrs_in_elem(MeshObj *node, MeshObj *elem, MeshObj **nbr_node1, MeshObj **nbr_node2);
  static void _propagate_level_to_other_procs(Mesh &mesh, vector<CreepNode *> &level, map<int,CreepNode> &creep_map);
  static void _convert_creep_levels_to_WMat(int num_creep_levels, vector <CreepNode *> *creep_levels, WMat &wts);
  static void _convert_creep_levels_to_dst_status(int num_creep_levels, vector <CreepNode *> *creep_levels, WMat &dst_status);



  /* XMRKX */
  // Creep unmasked points creep_level levels into masked points yielding wts. 
  // If set_dst_status is true, then also update dst_status.
  void CreepFill(Mesh &mesh, vector<int> &valid_gids, int num_creep_levels, int num_donor_levels, WMat &wts, bool set_dst_status, WMat &dst_status) {
    
    //printf("CreepFill creep_levels=%d num_donor_levels=%d num valid gids=%d\n",num_creep_levels, num_donor_levels, valid_gids.size());

    // Error check
    if (num_creep_levels < 0) Throw() << "Number of creep levels must be positive.";
    if (num_donor_levels < 0) Throw() << "Number of donor levels must be positive.";
    if (mesh.parametric_dim() != 2) Throw() << "Creep fill extrapolation currenly only supported for 2D Grids or Meshes.";

    // Leave if nothing to be done
    if (num_creep_levels == 0) return;
    if (num_donor_levels == 0) return;

    // Add one creep level so that a creep level of 1 gives one level beyond the valid gids which are stored in 0
    num_creep_levels++;

    // Get handy Mesh info
    MEField<> *cfield = mesh.GetCoordField();
    MEField<> *mskfield = mesh.GetField("mask");
    int sdim=mesh.spatial_dim();

    // Creep map 
    map<int,CreepNode> creep_map;

    // Levels
    vector<CreepNode *> *creep_levels=NULL;

    // Allocate a vector for each level
    creep_levels = new vector<CreepNode *>[num_creep_levels];

    //// Add initial set of valid points
    //// TODO: to save memory eventually just add the ones that are one away. 
    for (int i=0; i<valid_gids.size(); i++) {

      // get one gid
      int gid=valid_gids[i];

      // Get node
      Mesh::MeshObjIDMap::iterator mi =  mesh.map_find(MeshObj::NODE, gid);
      if (mi == mesh.map_end(MeshObj::NODE)) {
        Throw() << "No node with this gid found in the mesh.";
      }

      // Get node
      MeshObj *node=&*mi;

      // Skip if it's masked
      if (mskfield) {
        double *m=mskfield->data(*node);
        if (*m > 0.5) continue;
      }

      // Add to map
      std::pair< map<int,CreepNode>::iterator,bool> ret;
      ret=creep_map.insert(std::pair<int,CreepNode>(gid, CreepNode(sdim, cfield, 0, node)));
 
      // Add to level 0
      creep_levels[0].push_back(&(ret.first->second));

    }

    // Debug output
    //_write_level("creep_0level",mesh, creep_levels[0]);

    /// Loop connecting one level to nodes in the last one
    for (int l=1; l<num_creep_levels; l++) {


      //printf("%d# Level %d Beg\n",Par::Rank(),l);


      // Propagate prev level info to other procs
      // (TODO: Figure out for sure if I need to propogate the info at the
      //        last iteration, I don't think I need to.)
      _propagate_level_to_other_procs(mesh, creep_levels[l-1], creep_map);
      
      // Loop through prev level
      for (int i=0; i<creep_levels[l-1].size(); i++) {

        // Get creep node
        CreepNode *creep_node=creep_levels[l-1][i];

        // Get node from last level
        MeshObj *node_ll=creep_node->node;

        // If null, skip
        if (node_ll==NULL) continue;

        // Loop through all the nodes connected to node
        // TODO: See if you can just loop through nodes around a node??
        MeshObjRelationList::const_iterator el = MeshObjConn::find_relation(*node_ll, MeshObj::ELEMENT);
        while (el != node_ll->Relations.end() && el->obj->get_type() == MeshObj::ELEMENT){
          MeshObj *elem=el->obj;

#ifdef ESMF_REGRID_DEBUG_CREEP_NODE
          if (node_ll->get_id() == ESMF_REGRID_DEBUG_CREEP_NODE) {
            printf("%d# node id=%d elem id=%d Beg\n",Par::Rank(),node_ll->get_id(),elem->get_id());
          }
#endif

          // Get the nbrs of the node in the element
          MeshObj *nbr_node1, *nbr_node2;
          _get_node_nbrs_in_elem(node_ll, elem, &nbr_node1, &nbr_node2);


          // Process neighor 1

          // check for masking
          bool nbr_node1_masked=false;
          if (mskfield) {
            double *m=mskfield->data(*nbr_node1);
            if (*m > 0.5) nbr_node1_masked=true;
          }

#ifdef ESMF_REGRID_DEBUG_CREEP_NODE
          if (node_ll->get_id() == ESMF_REGRID_DEBUG_CREEP_NODE) {
            printf("%d# node id=%d elem id=%d n1_id=%d n1_msk=%d\n",Par::Rank(),node_ll->get_id(),elem->get_id(),nbr_node1->get_id(), nbr_node1_masked);
          }
#endif

          // Only do if not masked
          if (!nbr_node1_masked) { 

            // Get node gid
            int nbr_node1_gid=nbr_node1->get_id();
            
            // See if this node is in the map, then add or add too
            map<int,CreepNode>::iterator mi1 = creep_map.find(nbr_node1_gid);
            if (mi1 == creep_map.end()) {
              // Not in the map, so add a new one
              
              // Add new creep node to map
              std::pair< map<int,CreepNode>::iterator,bool> ret;
              ret=creep_map.insert(std::pair<int,CreepNode>(nbr_node1_gid, CreepNode(sdim, cfield, l, nbr_node1)));
              
              // Add donor to newly added creep node
              ret.first->second.add_donor(creep_node);
              
              // Add newly added creep node to this level
              creep_levels[l].push_back(&(ret.first->second));
              
            } else {
              // In the map, so if 1 level away add to donors
              
              // Get pointer to creep node
              CreepNode *found_cn=&(mi1->second);
              
              // If found node at this level, then add to donors
              if (found_cn->level==l) {
                found_cn->add_donor(creep_node);
              }
            }
          }

          // Process neighor 2

          // check for masking
          bool nbr_node2_masked=false;
          if (mskfield) {
            double *m=mskfield->data(*nbr_node2);
            if (*m > 0.5) nbr_node2_masked=true;
          }

#ifdef ESMF_REGRID_DEBUG_CREEP_NODE
          if (node_ll->get_id() == ESMF_REGRID_DEBUG_CREEP_NODE) {
            printf("%d# node id=%d elem id=%d n2_id=%d n2_msk=%d\n",Par::Rank(),node_ll->get_id(),elem->get_id(),nbr_node2->get_id(), nbr_node2_masked);
          }
#endif
          // Only do if not masked
          if (!nbr_node2_masked) { 
            
            // Get node gid
            int nbr_node2_gid=nbr_node2->get_id();
            
            // See if this node is in the map, then add or add too
            map<int,CreepNode>::iterator mi2 = creep_map.find(nbr_node2_gid);
            if (mi2 == creep_map.end()) {
              // Not in the map, so add a new one
              
              // Add new creep node to map
              std::pair< map<int,CreepNode>::iterator,bool> ret;
              ret=creep_map.insert(std::pair<int,CreepNode>(nbr_node2_gid, CreepNode(sdim, cfield, l, nbr_node2)));
              
              // Add donor to newly added creep node
              ret.first->second.add_donor(creep_node);
              
              // Add newly added creep node to this level
              creep_levels[l].push_back(&(ret.first->second));
              
            } else {
              // In the map, so if 1 level away add to donors
              
              // Get pointer to creep node
              CreepNode *found_cn=&(mi2->second);
              
              // If found node at this level, then add to donors
              if (found_cn->level==l) {
                found_cn->add_donor(creep_node);
              }
            }
          }
          
#ifdef ESMF_REGRID_DEBUG_CREEP_NODE
          if (node_ll->get_id() == ESMF_REGRID_DEBUG_CREEP_NODE) {
            printf("%d# node id=%d elem id=%d End\n",Par::Rank(),node_ll->get_id(),elem->get_id());
          }
#endif

          // next element around node
          ++el;
        }

        // Convert donor information to weights
        creep_node->convert_donors_to_weights();
      }

      // Debug output level
      // char new_filename[1000];
      //sprintf(new_filename,"creep_%dlevel",l);
      //_write_level(new_filename,mesh, creep_levels[l]);

      // Add other donor levels to cnodes in creep_levels[l]
      // (Level 0 is already filled above)
      //      for (int dl=1; dl<num_donor_levels; dl++) {

        // Loop through prev level
      //  for (int i=0; i<creep_levels[l].size(); i++) {
          //// STOPPED HERE

      // } // num creep nodes in level l
      //} //  num_donor_levels - dl

      //printf("%d# Level %d End\n",Par::Rank(),l);
    }

    // Loop through last level adding weights
    for (int i=0; i<creep_levels[num_creep_levels-1].size(); i++) {
      // Get creep node
      CreepNode *creep_node=creep_levels[num_creep_levels-1][i];

      // Convert donor information to weights
      creep_node->convert_donors_to_weights();
    }

    // Convert weights in all the creep nodes in all the levels into a WMat
    _convert_creep_levels_to_WMat(num_creep_levels, creep_levels, wts);

    // Set destination status (if asked to)
    if (set_dst_status) {
      _convert_creep_levels_to_dst_status(num_creep_levels, creep_levels, dst_status);
    }

    // DEBUG 
    //  printf("%d# max packed buff size=%d\n",Par::Rank(),max_packed_buff_size);


    // Get rid of levels structure
    if (creep_levels != NULL) delete [] creep_levels;
  }



// Construct weights from creep information
// TODO: THINK IF THIS SHOULD ONLY DO LOCAL NODES???
  static void _convert_creep_levels_to_WMat(int num_creep_levels, vector <CreepNode *> *creep_levels, WMat &wts) {

  // make sure there is at least one level
  if (num_creep_levels < 2) return;

  // Iterate through creep levels   - A Oehmke
  for (int l=1; l<num_creep_levels; l++) {

    // Loop through level
    for (int i=0; i<creep_levels[l].size(); i++) {

      // Get creep node
      CreepNode *cnode=creep_levels[l][i];      

      // Add to WMat
      cnode->add_weights_to_WMat(wts);
    }
  }


#ifdef ESMF_REGRID_DEBUG_CREEP_NODE
  // DEBUG: Loop through weights and look at specific one
  WMat::WeightMap::iterator wi = wts.begin_row(), we = wts.end_row();
  for (; wi != we; ++wi) {
    const WMat::Entry &w = wi->first;

    std::vector<WMat::Entry> &wcol = wi->second;
    
    if (w.id == ESMF_REGRID_DEBUG_CREEP_NODE) {

      printf("weights  dst node id=%d :: ",w.id);

      // Loop through and write out information
      for (UInt j = 0; j < wcol.size(); ++j) {
        const WMat::Entry &wc = wcol[j];

        printf("  %d %g",wc.id,wc.value);
      }
      printf("\n");
    }
  }
#endif

  }


    // Version of weight calc. that divides weights equally //
  static void _calc_level_1_weights_from_CreepNode(CreepNode *cnode, std::vector<int>& wgt_ids, std::vector<double>& wgt_vals) {

    // RESERVE MEMORY BASED ON LEVEL 0. 
    // IN FUTURE, RESERVE BASED ON NUMBER OF DONORS AT EVERY LEVEL TOTALLED
    wgt_ids.reserve(cnode->donors.size());
    wgt_vals.reserve(cnode->donors.size());


    // Version of weight calc. that divides weights equally //

    // compute evenly divided weight
    double even_weight=0.0;
    if (cnode->donors.size() > 0) {
      even_weight=1.0/((double)(cnode->donors.size()));
    }

    // Add a weight for each donor
    for (int d=0; d<cnode->donors.size(); d++) {
      
      // Get donor creep node
      CreepNode *dnr=cnode->donors[d];

#ifdef ESMF_REGRID_DEBUG_CREEP_NODE
      if (cnode->gid==ESMF_REGRID_DEBUG_CREEP_NODE) {
        printf("gid=%d lvl1 d=%d adding donor %d with weight %f\n",cnode->gid,d,dnr->gid,even_weight);
      }
#endif
      
      // Add weights
      wgt_ids.push_back(dnr->gid);
      wgt_vals.push_back(even_weight);
    }


    // OLD CODE THAT CALCULATES WEIGHTS BASED ON DIST SAVE IN CASE WE NEED
#if 0
    // Get destination coordinate info
    double dst_pnt[3];

    // See if there are any 0.0 dist
    bool no_zero_dist=true;
    for (int d=0; d<cnode->num_donor_levels; d++) {
          
      // Get donor creep node
      CreepNode *dnr=cnode->donors[d];
      
      // Get coordinates of donor point
      double *dnr_pnt[3];
      
      // Calculate distance
      double dist=sqrt((dst_pnt[0]-dnr_pnt[0])*(dst_pnt[0]-dnr_pnt[0])+
                       (dst_pnt[1]-dnr_pnt[1])*(dst_pnt[1]-dnr_pnt[1])+
                       (dst_pnt[2]-dnr_pnt[2])*(dst_pnt[2]-dnr_pnt[2]));
      
      // There is a 0.0 dist, so record that fact and leave
      if (dist == 0.0) {
        no_zero_dist=false;
        break;
      }
    }
    
    // Loop calculating weights
    double tot=0.0;
    if (no_zero_dist) {
      int dl=0; // donor level for now just 0
      for (int d=0; d<cnode->donors[dl].size(); d++) {
        
        // Get donor creep node
        CreepNode *dnr=cnode->donors[dl][d];
        
        // Get coordinates of donor point
        double *dnr_pnt[3];
        
        // Calculate distance
        double dist=sqrt((dst_pnt[0]-dnr_pnt[0])*(dst_pnt[0]-dnr_pnt[0])+
                         (dst_pnt[1]-dnr_pnt[1])*(dst_pnt[1]-dnr_pnt[1])+
                         (dst_pnt[2]-dnr_pnt[2])*(dst_pnt[2]-dnr_pnt[2]));
        
        // This shouldn't happen, so complain
        if (dist == 0.0) {
          Throw() << " zero distance in part of weight calc that's for nonzero.";
        }
        
        // DON'T DO POWER FOR NOW
#if 0
        // 1 over dist raised to a power
        double inv_dist=1.0/pow(dist,dist_exponent_dbl);
#else
        // 1 over dist
        double inv_dist=1.0/dist;
#endif
        // Sum total weights
        tot += inv_dist;
        
        // Set col entry info
        // NOTE: dst_gid actually contains src_gid
        IWeights::Entry col_entry(sr.nodes[i].dst_gid, 0, inv_dist, 0);
        
        // Push into
        cols.push_back(col_entry);
      }
    } else {
      // There are 0.0 dist, so just count those
      int dl=0; // donor level for now just 0
      for (int d=0; d<cnode->donors[dl].size(); d++) {

        // Get donor creep node
        CreepNode *dnr=cnode->donors[dl][d];
        
        // Get coordinates of donor point
        double *dnr_pnt[3];
        
        // Calculate distance
        double dist=sqrt((dst_pnt[0]-dnr_pnt[0])*(dst_pnt[0]-dnr_pnt[0])+
                         (dst_pnt[1]-dnr_pnt[1])*(dst_pnt[1]-dnr_pnt[1])+
                         (dst_pnt[2]-dnr_pnt[2])*(dst_pnt[2]-dnr_pnt[2]));
        

        // This is 0.0, so just add that
        if (dist == 0.0) {
          
          // Set col entry info using 1.0 as weight
          // NOTE: dst_gid actually contains src_gid
          IWeights::Entry col_entry(sr.nodes[i].dst_gid, 0, 1.0, 0);

          // Sum total weights
          tot += 1.0;
          
          // Push into
          cols.push_back(col_entry);
        }
      }
    }

    // Loop dividing by tot
    for (int i=0; i<cols.size(); i++) {
      cols[i].value=cols[i].value/tot;
    }
#endif
  }

    // Version of weight calc. that divides weights equally //
  static void _calc_level_gt1_weights_from_CreepNode(CreepNode *cnode, std::vector<int>& wgt_ids, std::vector<double>& wgt_vals) {


    // Sum up size of donor weights
    int num_wgts=0;
    for (int d=0; d<cnode->donors.size(); d++) {
      num_wgts += cnode->donors[d]->wgt_ids.size();
    }
    

    // Reserve number of weights total across all donors
    // IN FUTURE, RESERVE BASED ON NUMBER OF DONORS AT EVERY LEVEL TOTALLED
    wgt_ids.reserve(num_wgts);
    wgt_vals.reserve(num_wgts);


    // compute evenly divided weight
    double even_weight=0.0;
    if (cnode->donors.size() > 0) {
      even_weight=1.0/((double)(cnode->donors.size()));
    }

    // Add a weight for each donor's weight
    for (int d=0; d<cnode->donors.size(); d++) {
      
      // Get donor creep node
      CreepNode *dnr=cnode->donors[d];

      // Loop over donors weights
      for (int w=0; w<dnr->wgt_ids.size(); w++) {

        // Get one donor weight
        int dnr_wgt_id=dnr->wgt_ids[w];
        double dnr_wgt_val=dnr->wgt_vals[w];

#ifdef ESMF_REGRID_DEBUG_CREEP_NODE
        if (cnode->gid==ESMF_REGRID_DEBUG_CREEP_NODE) {
          printf("gid=%d lvl gt 1 d=%d w=%d adding donor %d with weight %f\n",cnode->gid,d,w,dnr_wgt_id,even_weight*dnr_wgt_val);
        }
#endif

        // See if weight is already there
        int wgt_loc=-1;
        for (int j=0; j<wgt_ids.size(); j++) {
          if (wgt_ids[j]==dnr_wgt_id) {
            wgt_loc=j;
            break;
          }
        }

      
        // Add new weight based on if it's already in 
        if (wgt_loc == -1) { // Not in so add to end
          wgt_ids.push_back(dnr_wgt_id);
          wgt_vals.push_back(even_weight*dnr_wgt_val);
        } else { // Else just sum weight
          wgt_vals[wgt_loc] +=even_weight*dnr_wgt_val;
        }

      }
    }
  }



// Construct weights from creep information
  static void _convert_creep_levels_to_dst_status(int num_creep_levels, vector <CreepNode *> *creep_levels, WMat &dst_status) {

    // Put these outside loop, so it doesn't keep allocating memory every time
    std::vector<IWeights::Entry> cols;

    // make sure there is at least one level
    if (num_creep_levels < 2) return;

    // Iterate through creep levels
    for (int l=1; l<num_creep_levels; l++) {

      // Loop through levels
      for (int i=0; i<creep_levels[l].size(); i++) {

        // Get creep node
        CreepNode *cnode=creep_levels[l][i];      
        
        // Set row info (i.e. the destination id associated with the above weight)
        IWeights::Entry row(cnode->gid, 0, 0.0, 0);
      
        // Set col info
        WMat::Entry col(ESMC_REGRID_STATUS_EXTRAP_MAPPED, 0, 0.0, 0);
        
        // Put info into status matrix
        dst_status.InsertRowMergeSingle(row, col);
      }
    }
  }


 static  void _get_node_nbrs_in_elem(MeshObj *node, MeshObj *elem, MeshObj **nbr_node1, MeshObj **nbr_node2) {

   // Get number of nodes in element
   const ESMCI::MeshObjTopo *topo = ESMCI::GetMeshObjTopo(*elem);

   // Find the node around elem, that matches node
   int node_ind=-1;
   for (int s = 0; s < topo->num_nodes; s++) {
     MeshObj *elem_node = elem->Relations[s].obj;

     // If this is the node, then get neighbors and leave
     if (elem_node->get_id() == node->get_id()) {
       node_ind=s;
       break;
     }
   }

   // If no matching node was found complain
   if (node_ind == -1) {
     Throw() << "node not found in element where it was expected.";
   }

   // Get neighbor before
   if (node_ind==0) {
     *nbr_node1 = elem->Relations[topo->num_nodes-1].obj;
   } else {
     *nbr_node1 = elem->Relations[node_ind-1].obj;
   }

   // Get neighbor after
   if (node_ind==topo->num_nodes-1) {
     *nbr_node2 = elem->Relations[0].obj;
   } else {
     *nbr_node2 = elem->Relations[node_ind+1].obj;
   }
 }

//////////

// TODO:
//  - MAKE send_procs a set, so not repeating things???

  static void _recursively_add_CreepNode_to_snd_lists(CreepNode *cnode, UInt proc, 
                                         vector<CreepNode *> *snd_to_procs) {
    // Add current node
    snd_to_procs[proc].push_back(cnode);

    // Add donor nodes
    for (int d=0; d<cnode->donors.size(); d++) {
      _recursively_add_CreepNode_to_snd_lists(cnode->donors[d],
                                              proc, snd_to_procs);
    }
  }



/* XMRKX */
  static void _propagate_level_to_other_procs(Mesh &mesh, vector<CreepNode *> &level, map<int,CreepNode> &creep_map) {

    // Get number of procs
    int num_procs=Par::Size();

    // If serial, leave
    if (num_procs == 1) return;

    // send_to
    vector<CreepNode *> *snd_to_procs=NULL;

    // Allocate a vector for each proc
    snd_to_procs = new vector<CreepNode *>[num_procs];

    //// Send from non-local copies to owned version ////

    // Loop through this level
    for (int i=0; i<level.size(); i++) {

      // Get creep node
      CreepNode *cnode=level[i];

#ifdef ESMF_REGRID_DEBUG_CREEP_NODE
      // DEBUG output
      if (cnode->gid == ESMF_REGRID_DEBUG_CREEP_NODE) {
        printf("%d# node id=%d no->o checking for send node ptr=%p \n",Par::Rank(),cnode->gid,cnode->node);
      }
#endif

      // Get mesh node
      MeshObj *node=cnode->node;

      // If null, then skip
      if (node == NULL) continue;

#ifdef ESMF_REGRID_DEBUG_CREEP_NODE
      // DEBUG output
      if (cnode->gid == ESMF_REGRID_DEBUG_CREEP_NODE) {
        printf("%d# node id=%d no->o checking for send local=%d shared=%d \n",Par::Rank(),cnode->gid,GetAttr(*node).is_locally_owned(),GetAttr(*node).is_shared());
      }
#endif

      // Only sending non-local, so if local skip
      if (GetAttr(*node).is_locally_owned()) continue;

      // Get Owner
      UInt owner=node->get_owner();

#ifdef ESMF_REGRID_DEBUG_CREEP_NODE
      // DEBUG output
      if (node->get_id() == ESMF_REGRID_DEBUG_CREEP_NODE) {
        printf("%d# node id=%d no->o sending to pets %d \n",Par::Rank(),node->get_id(),owner);
      }
#endif

      // Add to send lists
      _recursively_add_CreepNode_to_snd_lists(cnode, owner, snd_to_procs);
    }


    // Collapse to just non-empty lists
    int num_nonempty_procs=0;
    for (int p=0; p<num_procs; p++) {
      if (!snd_to_procs[p].empty()) num_nonempty_procs++;
    }

    // Get comm pattern information
    vector<UInt> snd_procs;
    snd_procs.resize(num_nonempty_procs);
    vector<UInt> snd_sizes;
    snd_sizes.resize(num_nonempty_procs);

    int k=0;
    for (int p=0; p<num_procs; p++) {
      if (!snd_to_procs[p].empty()) {

        // record proc
        snd_procs[k]=(UInt)p;

        // Calculate size
        UInt size=0;
        for (int i=0; i<snd_to_procs[p].size(); i++) {
          size += snd_to_procs[p][i]->packed_size();
        }
        snd_sizes[k]=size;

        // Next slot
        k++;
      }
    }

    // Create communication structure
    SparseMsg comm;

    // Setup pattern and sizes
    if (num_nonempty_procs > 0) {
      comm.setPattern(num_nonempty_procs, (const UInt *)&(snd_procs[0]));
      comm.setSizes((UInt *)&(snd_sizes[0]));
    } else {
      comm.setPattern(0, (const UInt *)NULL);
      comm.setSizes((UInt *)NULL);
    }

    // Reset buffers
    comm.resetBuffers();

    // Setup packed buff
    int packed_buff_size=1; // init to some size
    //    int packed_buff_size=2048; // init to some size
    UChar *packed_buff=NULL;
    packed_buff=new UChar[packed_buff_size];


    // Pack points into buffers
    for (int p=0; p<num_nonempty_procs; p++) {
      UInt proc=snd_procs[p];

      // Get buffer for proc
      SparseMsg:: buffer *b=comm.getSendBuffer(proc);

      // Loop and pack CreepNodes into buffer
      // Do it in reverse order so when unpacking donor
      // nodes are unpacked first
      for (int j=snd_to_procs[proc].size()-1; j>=0; j--) {
        CreepNode *cnode=snd_to_procs[proc][j];

        // get size
        UInt packed_size=cnode->packed_size();

        // Expand packed buff if necessary
        if (packed_size > packed_buff_size) {
          packed_buff_size=packed_size;
          if (packed_buff != NULL) delete[] packed_buff;
          packed_buff=new UChar[packed_buff_size];
        }

        // Pack 
        cnode->pack(packed_buff);

        // Push buf onto send struct
        b->push(packed_buff, packed_size);
      }
    }

    // Communicate information
    comm.communicate();

    // Go through received buffers and add creep nodes to structures
    for (std::vector<UInt>::iterator p = comm.inProc_begin(); p != comm.inProc_end(); ++p) {

      // Get this procs buffer
      UInt proc = *p;
      SparseMsg::buffer *b = comm.getRecvBuffer(proc);

      // Loop unpacking buffer until empty
      while (!b->empty()) {

        // Look at the buffer to figure out what size to unpack
        UInt packed_size=CreepNode::packed_size_from_buff((UChar *)(b->get_current()));

        // Expand packed buff if necessary
        if (packed_size > packed_buff_size) {
          packed_buff_size=packed_size;
          if (packed_buff != NULL) delete[] packed_buff;
          packed_buff=new UChar[packed_buff_size];
        }

        // Get one CreepNode's info out of buffer
        b->pop(packed_buff, packed_size);

        // Get gid
        int gid=CreepNode::gid_from_buff(packed_buff);

#ifdef ESMF_REGRID_DEBUG_CREEP_NODE
        if (gid == ESMF_REGRID_DEBUG_CREEP_NODE) {
          printf("%d# no->o node id=%d received \n",Par::Rank(),gid);
        }
#endif

        // If it's already here, then skip
        map<int,CreepNode>::iterator mi = creep_map.find(gid);
        if (mi != creep_map.end()) continue;

#ifdef ESMF_REGRID_DEBUG_CREEP_NODE
        if (gid == ESMF_REGRID_DEBUG_CREEP_NODE) {
          printf("%d# no->o node id=%d making a new creep node \n",Par::Rank(),gid);
        }
#endif

        // Make new creep node
        CreepNode tmp_cnode(packed_buff, mesh, creep_map);

        // Add new creep node to map
        std::pair< map<int,CreepNode>::iterator,bool> ret;
        ret=creep_map.insert(std::pair<int,CreepNode>(gid, tmp_cnode));

        // Add new creep node to level
       level.push_back(&(ret.first->second));
      }
    }

    //// Send from owned version to non-local copies ////

    // Zero everything
    for (int p=0; p<num_procs; p++) {
      snd_to_procs[p].resize(0);
    }

    // Loop through this level
    for (int i=0; i<level.size(); i++) {

      // Get creep node
      CreepNode *cnode=level[i];

#ifdef ESMF_REGRID_DEBUG_CREEP_NODE
      // DEBUG output
      if (cnode->gid == ESMF_REGRID_DEBUG_CREEP_NODE) {
        printf("%d# o->no node id=%d checking for send node ptr=%p \n",Par::Rank(),cnode->gid,cnode->node);
      }
#endif

      // Get mesh node
      MeshObj *node=cnode->node;

      // If null, then skip
      if (node == NULL) continue;

#ifdef ESMF_REGRID_DEBUG_CREEP_NODE
      // DEBUG output
      if (cnode->gid == ESMF_REGRID_DEBUG_CREEP_NODE) {
        printf("%d# o->no node id=%d checking for send local=%d shared=%d \n",Par::Rank(),cnode->gid,GetAttr(*node).is_locally_owned(),GetAttr(*node).is_shared());
      }
#endif

      // If not local, then skip
      if (!GetAttr(*node).is_locally_owned()) continue;

      // If not shared, then skip
      if (!GetAttr(*node).is_shared()) continue;

      // Figure out which other processors this node's information should be sent to
      std::vector<UInt> shared_procs;
      MeshObjConn::get_node_sharing(*node, mesh.GetSymNodeRel(), shared_procs);

#ifdef ESMF_REGRID_DEBUG_CREEP_NODE
      // DEBUG output
      if (node->get_id() == ESMF_REGRID_DEBUG_CREEP_NODE) {
        printf("%d# o->no node id=%d sending to pets= ",Par::Rank(),node->get_id());
        for (int p=0; p<shared_procs.size(); p++) {
          printf(" %d ",shared_procs[p]);
        }
        printf("\n");

      }
#endif

      // Add to send lists
      for (int p=0; p<shared_procs.size(); p++) {
        _recursively_add_CreepNode_to_snd_lists(cnode, shared_procs[p], snd_to_procs);
      }
    }


    // Collapse to just non-empty lists
    num_nonempty_procs=0;
    for (int p=0; p<num_procs; p++) {
      if (!snd_to_procs[p].empty()) num_nonempty_procs++;
    }

    // Get comm pattern information
    snd_procs.resize(num_nonempty_procs);
    snd_sizes.resize(num_nonempty_procs);

    k=0;
    for (int p=0; p<num_procs; p++) {
      if (!snd_to_procs[p].empty()) {

        // record proc
        snd_procs[k]=(UInt)p;

        // Calculate size
        UInt size=0;
        for (int i=0; i<snd_to_procs[p].size(); i++) {
          size += snd_to_procs[p][i]->packed_size();
        }
        snd_sizes[k]=size;

        // Next slot
        k++;
      }
    }

    // Setup pattern and sizes
    if (num_nonempty_procs > 0) {
      comm.setPattern(num_nonempty_procs, (const UInt *)&(snd_procs[0]));
      comm.setSizes((UInt *)&(snd_sizes[0]));
    } else {
      comm.setPattern(0, (const UInt *)NULL);
      comm.setSizes((UInt *)NULL);
    }

    // Reset buffers
    comm.resetBuffers();

    // Pack points into buffers
    for (int p=0; p<num_nonempty_procs; p++) {
      UInt proc=snd_procs[p];

      // Get buffer for proc
      SparseMsg:: buffer *b=comm.getSendBuffer(proc);

      // Loop and pack CreepNodes into buffer
      // Do it in reverse order so when unpacking donor
      // nodes are unpacked first
      for (int j=snd_to_procs[proc].size()-1; j>=0; j--) {
        CreepNode *cnode=snd_to_procs[proc][j];

        // get size
        UInt packed_size=cnode->packed_size();

        // Expand packed buff if necessary
        if (packed_size > packed_buff_size) {
          packed_buff_size=packed_size;
          if (packed_buff != NULL) delete[] packed_buff;
          packed_buff=new UChar[packed_buff_size];
        }

        // Pack 
        cnode->pack(packed_buff);

        // Push buf onto send struct
        b->push(packed_buff, packed_size);
      }
    }

    // Communicate information
    comm.communicate();

    // Go through received buffers and add creep nodes to structures
    for (std::vector<UInt>::iterator p = comm.inProc_begin(); p != comm.inProc_end(); ++p) {

      // Get this procs buffer
      UInt proc = *p;
      SparseMsg::buffer *b = comm.getRecvBuffer(proc);

      // Loop unpacking buffer until empty
      while (!b->empty()) {

        // Look at the buffer to figure out what size to unpack
        UInt packed_size=CreepNode::packed_size_from_buff((UChar *)(b->get_current()));

        // Expand packed buff if necessary
        if (packed_size > packed_buff_size) {
          packed_buff_size=packed_size;
          if (packed_buff != NULL) delete[] packed_buff;
          packed_buff=new UChar[packed_buff_size];
        }

        // Get one CreepNode's info out of buffer
        b->pop(packed_buff, packed_size);

        // Get gid
        int gid=CreepNode::gid_from_buff(packed_buff);

#ifdef ESMF_REGRID_DEBUG_CREEP_NODE
        if (gid == ESMF_REGRID_DEBUG_CREEP_NODE) {
          printf("%d# o->no node id=%d received \n",Par::Rank(),gid);
        }
#endif

        // If it's already here, then skip
        map<int,CreepNode>::iterator mi = creep_map.find(gid);
        if (mi != creep_map.end()) continue;

#ifdef ESMF_REGRID_DEBUG_CREEP_NODE
        if (gid == ESMF_REGRID_DEBUG_CREEP_NODE) {
          printf("%d# o->no node id=%d making a new creep node \n",Par::Rank(),gid);
        }
#endif

        // Make new creep node
        CreepNode tmp_cnode(packed_buff, mesh, creep_map);

        // Add new creep node to map
        std::pair< map<int,CreepNode>::iterator,bool> ret;
        ret=creep_map.insert(std::pair<int,CreepNode>(gid, tmp_cnode));

        // Add new creep node to level
       level.push_back(&(ret.first->second));
      }
    }

    // DEBUG: update max buff size 
    //if (packed_buff_size > max_packed_buff_size) max_packed_buff_size=packed_buff_size;


    // Deallocate memory
    // TODO: move these out of this subroutine so allocate/deallocate doesn't
    //       happen every time
    if (snd_to_procs != NULL) delete [] snd_to_procs;
    if (packed_buff != NULL) delete[] packed_buff;
  }

 static void _write_level(const char *filename, 
                           Mesh &mesh, vector<CreepNode *> &level) {

    // Coord Pointer
    MEField<> *coord_ptr = mesh.GetCoordField();

    ESMCI::PointList pl(level.size(), mesh.spatial_dim());
    for (int i=0; i<level.size(); i++) {
      
      // Get node
      MeshObj *node=level[i]->node;

      // Get pointer to coords of this node
      double *coords=coord_ptr->data(*node);

      // Add point
      pl.add(node->get_id(), coords);
    }

    // Write out
    pl.WriteVTK(filename);
  }


/* XMRKX */
// Construct and add weights from one creep node
// TODO: Get rid of this subroutine!!!
  static void _add_weights_from_CreepNode(CreepNode *cnode, WMat &wts) {

#ifdef ESMF_REGRID_DEBUG_CREEP_NODE
    // DEBUG Look at one creep node and make sure that it's ok
    if (cnode->gid == ESMF_REGRID_DEBUG_CREEP_NODE) {
      printf("%d# donors  node id=%d :: ",Par::Rank(),cnode->gid);
      for (int j=0; j<cnode->donors.size(); j++) {
        printf(" %d ",cnode->donors[j]->gid);
      }
      printf("\n");
    }
#endif
    
    // Convert donor information to weights
    cnode->convert_donors_to_weights();

    // Add weights from creep node to output matrix
    cnode->add_weights_to_WMat(wts);
  }


  } // namespace
