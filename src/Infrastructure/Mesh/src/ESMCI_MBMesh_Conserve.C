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

// Take out if MOAB isn't being used
#ifdef ESMF_MOAB

#include <Mesh/include/ESMCI_Exception.h>
#include <Mesh/include/ESMCI_MeshOBjConn.h>
#include <Mesh/include/ESMCI_MeshUtils.h>
#include <Mesh/include/ESMCI_MEValues.h>
#include <Mesh/include/ESMCI_Polynomial.h>
#include <Mesh/include/ESMCI_MeshField.h>
#include <Mesh/include/ESMCI_MeshTypes.h>
#include <Mesh/include/ESMCI_MathUtil.h>
#include <Mesh/include/ESMCI_Ftn.h>
#include <Mesh/include/ESMCI_ParEnv.h>
#include <Mesh/include/ESMCI_Sintdnode.h>
#include <Mesh/include/ESMCI_XGridUtil.h>
#include <Mesh/include/ESMCI_Phedra.h>
#include <Mesh/include/ESMCI_MBMesh.h>
#include <Mesh/include/ESMCI_WMat.h>
#include <Mesh/include/ESMCI_MBMesh_BBox.h>
#include <Mesh/include/ESMCI_MBMesh_Search.h>
#include <Mesh/include/ESMCI_MBMesh_Util.h>


#include <iostream>
#include <iterator>
#include <iomanip>
#include <cmath>
#include <vector>

#include <ESMCI_VM.h>
#include <ESMCI_LogErr.h>



//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

          
using namespace ESMCI;

 bool debug=false;

  
  // Intersects between the line a and the seqment s
  // returns true if we should add point p.
  // sin is the end of the segment inside the polygon
  // sout is the end of the segment outside the polygon
  // This subroutine is set up to be used with the poly intersect 
  // code below, and has a number of tweaks for special cases
  // which might make it odd to be used as a general intesect code.
  bool line_with_seg2D(double *a1, double *a2, double *sin, double *sout,
		       double *p) {
    
    // Calculate thing to divide both line equations by
    double ttdb= 
      a1[0]*(sout[1] - sin[1]) +
      a2[0]*(sin[1] - sout[1]) +
      sin[0]*(a1[1] - a2[1]) + 
      sout[0]*(a2[1] - a1[1]);
    
     
    // if ttdb is 0.0 then the lines are parallel, this
    // shouldn't happen, but if it does it makes the
    // most sense to add the out point
    if (ttdb == 0.0) {
      p[0]=sout[0];
      p[1]=sout[1];
      return true;
    }
    
    // Calculate t
    double t=
      -(a1[0]*(sin[1]-a2[1]) +
	a2[0]*(a1[1]-sin[1]) +
	sin[0]*(a2[1]-a1[1]))/ttdb;
    
    // We shouldn't be off the ends, but
    // if we are because of rounding then
    // do what makes sense
    
    // if we're off the in end, then don't add
    if (t<0.0) return false;
    
    // if we're off the out end, then just add the
    // out point
    if (t>=1.0) {
      p[0]=sout[0];
      p[1]=sout[1];
      return true;
    }
    
    // Otherwise calculate point of intersection
    // and add that
    p[0]=sin[0] + t*(sout[0]-sin[0]);
    p[1]=sin[1] + t*(sout[1]-sin[1]);
    return true;
    
  }
  
  // intersects 2D convex polygons whose vertices are stored in counter clockwise
  // order.
  // p should be of size 2*num_p
  // q should be of size 2*num_q
  // tmp and out should both be allocated to be at least of size 2*(num_p+num_q)
  void intersect_convex_poly2D(int num_p, double *p,
			       int num_q, double *q,
			       double *tmp,
			       int *num_out, double *out) 
  {
    
#define CLIP_EQUAL_TOL 1.0e-20
     
    // If p is empty then leave
    if (num_p==0) {
      *num_out=0;
    }
    
    // If q is empty then leave
    if (num_q==0) {
      *num_out=0;
    }
    
    // INSTEAD OF TMP USE T???
    
    // Copy q into tmp
    double *end_q=q+2*num_q;
    for (double *q_i=q, *tmp_i=tmp; q_i<end_q; q_i++, tmp_i++) {
      *tmp_i=*q_i;
    }
    int num_tmp=num_q;
    
    // Setup alias for output array
    int num_o=0;
    double *o=out;
    
    // Loop through p
    for (int ip=0; ip<num_p; ip++) {
      // Get points of current edge of p
      double *p1=p+2*ip;
      double *p2=p+2*((ip+1)%num_p);
      
    // calc p_vec (vector along the current edge of p)
      double p_vec[2];
      p_vec[0]=p2[0]-p1[0];
      p_vec[1]=p2[1]-p1[1];
      
      // Set initial t1
      double *t1=tmp+2*(num_tmp-1);
      double inout1=p_vec[0]*(t1[1]-p1[1]) - p_vec[1]*(t1[0]-p1[0]);
      
      // Make sure we don't have a degenerate polygon after clipping
      bool in_but_not_on_p_vec=false;
      
      // Init output poly
      num_o=0;
      
      // Loop through other polygon
      for (int it=0; it<num_tmp; it++) {
	double *t2=tmp+2*it;
	
	// calculate variable which says if t2 is in or out of polygon p
	// (the cross product of p_vec with the vector from p1 to t2
 	double inout2=p_vec[0]*(t2[1]-p1[1]) - p_vec[1]*(t2[0]-p1[0]);
	
	// process point
	if (inout2 > CLIP_EQUAL_TOL) { // t2 inside 
	  if (inout1 < 0.0) { //  t1 outside
	    double intersect_pnt[2];        
	    
	    // Do intersection and add that point
	    if (line_with_seg2D(p1, p2, t2, t1, intersect_pnt)) {
	      o[2*num_o]=intersect_pnt[0];
	      o[2*num_o+1]=intersect_pnt[1];
	      num_o++;
	    }
	  }
	  
	  // Add t2 point because it's inside
	  o[2*num_o]=t2[0];
	  o[2*num_o+1]=t2[1];
	  num_o++;
	  
	  // record the fact that a point isn't on p_vec, but is in
	  in_but_not_on_p_vec=true;
	  
	} else if (inout2 < 0.0) { // t2 outside
	  
	  if (inout1 > CLIP_EQUAL_TOL) {  //  t1 inside (this excludes the EQUAL region, because 
	    double intersect_pnt[2];      //             if a point was added in there we don't
	    //             want to add another one right next to it)
	    
	    // Do intersection and add that point
	    if (line_with_seg2D(p1, p2, t1, t2, intersect_pnt)) {
	      o[2*num_o]=intersect_pnt[0];
	      o[2*num_o+1]=intersect_pnt[1];
	      num_o++;
	    }
	  }
	  
	} else {  // t2 on edge
	  // Just add point because it's on the edge
	  o[2*num_o]=t2[0];
	  o[2*num_o+1]=t2[1];
	  num_o++;
	}
	
	// old t2 becomes the new t1
	t1=t2;
	inout1=inout2;  
      }
      
      // if only on p_vec then degenerate and get rid of output poly
      if (!in_but_not_on_p_vec) num_o=0;
       
      // if poly is empty then leave
      if (num_o==0) break;

      // if not on the last cycle then copy out poly back to tmp
      if (ip != num_p-1) {
	double *end_o=o+2*num_o;
	for (double *o_i=o, *tmp_i=tmp; o_i<end_o; o_i++, tmp_i++) {
	  *tmp_i=*o_i;
	}
	num_tmp=num_o;
      }
    }
    
  // Do output
    *num_out=num_o;
    // o is an alias for out so don't need to copy
    
#undef CLIP_EQUAL_TOL
  }



  //////////////// BEGIN CALC 2D 2D  WEIGHTS ////////////////
void MBMesh_calc_1st_order_weights_2D_2D_cart(MBMesh *srcmbmp, EntityHandle src_elem, MBMesh *dstmbmp, 
                                                   std::vector<EntityHandle> dst_elems,
                                                   double *src_elem_area,
                                                   std::vector<int> *valid, std::vector<double> *wgts, 
                                                   std::vector<double> *sintd_areas_out, std::vector<double> *dst_areas_out) {



// Maximum size for a supported polygon
// Since the elements are of a small 
// limited size. Fixed sized buffers seem 
// the best way to handle them

#define  MAX_NUM_POLY_NODES 40
#define  MAX_NUM_POLY_COORDS_2D (2*MAX_NUM_POLY_NODES) 

    // Declaration for src polygon
    int num_src_nodes;
    double src_coords[MAX_NUM_POLY_COORDS_2D];
    double tmp_coords[MAX_NUM_POLY_COORDS_2D];


    // Get src coords
    MBMesh_get_elem_coords(srcmbmp, src_elem,  MAX_NUM_POLY_NODES, &num_src_nodes, src_coords);

    // if no nodes then exit
    if (num_src_nodes<1) return;

    // Get rid of degenerate edges
    remove_0len_edges2D(&num_src_nodes, src_coords);

    // If less than a triangle invalidate everything and leave because it won't results in weights
    // Decision about returning error for degeneracy is made above this subroutine
    if (num_src_nodes<3) {
      *src_elem_area=0.0;    
      for (int i=0; i<dst_elems.size(); i++) {
        (*valid)[i]=0;
        (*wgts)[i]=0.0;
        (*sintd_areas_out)[i]=0.0;
        (*dst_areas_out)[i]=0.0;
      }
      return;
    }

    // If a smashed quad invalidate everything and leave because it won't results in weights
    // Decision about returning error for degeneracy is made above this subroutine
    if (is_smashed_quad2D(num_src_nodes, src_coords)) {
      *src_elem_area=0.0;    
      for (int i=0; i<dst_elems.size(); i++) {
        (*valid)[i]=0;
        (*wgts)[i]=0.0;
        (*sintd_areas_out)[i]=0.0;
        (*dst_areas_out)[i]=0.0;
      }
      return;
    }

    // calculate dst area
    double src_area=area_of_flat_2D_polygon(num_src_nodes, src_coords); 

    // If src area is 0.0 invalidate everything and leave because it won't results in weights
    // Decision about returning error for degeneracy is made above this subroutine
    if (src_area == 0.0) {
      *src_elem_area=0.0;    
      for (int i=0; i<dst_elems.size(); i++) {
        (*valid)[i]=0;
        (*wgts)[i]=0.0;
        (*sintd_areas_out)[i]=0.0;
        (*dst_areas_out)[i]=0.0;
      }
      return;
    }

    // Output src_elem_area
    *src_elem_area=src_area;    

    // Declaration for dst polygon
    int num_dst_nodes;
    double dst_coords[MAX_NUM_POLY_COORDS_2D];

    // Declaration for intersection polygon
    int num_sintd_nodes;
    double sintd_coords[MAX_NUM_POLY_COORDS_2D];


    // Allocate something to hold areas
    std::vector<double> sintd_areas;
    sintd_areas.resize(dst_elems.size(),0.0);

    std::vector<double> dst_areas;
    dst_areas.resize(dst_elems.size(),0.0);

    // Loop intersecting and computing areas of intersection
    for (int i=0; i<dst_elems.size(); i++) {
      EntityHandle dstelem = dst_elems[i];
      

      // Get dst coords
      MBMesh_get_elem_coords(dstmbmp, dstelem,  MAX_NUM_POLY_NODES, &num_dst_nodes, dst_coords);

      // if no nodes then go to next
      if (num_dst_nodes<1) {
	(*valid)[i]=0;
	(*wgts)[i]=0.0;
        sintd_areas[i]=0.0;
	continue;
      }

      // Get rid of degenerate edges
      remove_0len_edges2D(&num_dst_nodes, dst_coords);
      
      // if less than a triangle skip
      if (num_dst_nodes<3) {
        (*valid)[i]=0;
        (*wgts)[i]=0.0;
        sintd_areas[i]=0.0;
        continue;
      }

      // if a smashed quad skip
      if (is_smashed_quad2D(num_dst_nodes, dst_coords)) {
        (*valid)[i]=0;
        (*wgts)[i]=0.0;
        sintd_areas[i]=0.0;
        continue;
      }
      
      // calculate dst area
     dst_areas[i]=area_of_flat_2D_polygon(num_dst_nodes, dst_coords); 

     // if destination area is 0.0, invalidate and go to next
     if (dst_areas[i]==0.0) {
       (*valid)[i]=0;
       (*wgts)[i]=0.0;
       sintd_areas[i]=0.0;
       continue;
     }
     
     // Make sure that we aren't going to go over size of tmp buffers
     if ((num_src_nodes + num_dst_nodes) > MAX_NUM_POLY_NODES) {
       Throw() << " src and dst poly size too big for temp buffer";
     }
     
     
     // Intersect src with dst element
      // Intersect src with dst element
      intersect_convex_poly2D(num_dst_nodes, dst_coords,
                              num_src_nodes, src_coords,
                              tmp_coords,
                              &num_sintd_nodes, sintd_coords); 
     
#if 0
      if (((dst_elem->get_id()==173) && (src_elem->get_id()==409)) ||
	  ((dst_elem->get_id()==171) && (src_elem->get_id()==406))) {
	printf("%d %d dst: ",dst_elem->get_id(),src_elem->get_id());
	for (int j=0; j<num_dst_nodes; j++) {
	  printf(" [%f,%f] ",dst_coords[2*j],dst_coords[2*j+1]);
	}
	printf("\n");
      }
#endif

      // Get rid of degenerate edges
      remove_0len_edges2D(&num_sintd_nodes, sintd_coords);

      // if intersected element isn't a complete polygon then go to next
      if (num_sintd_nodes < 3) {
	(*valid)[i]=0;
	(*wgts)[i]=0.0;
        sintd_areas[i]=0.0;
	continue;
      }

      // calculate intersection area
      sintd_areas[i]=area_of_flat_2D_polygon(num_sintd_nodes, sintd_coords); 

      (*valid)[i]=1;
    }


    // Loop calculating weights
    for (int i=0; i<dst_elems.size(); i++) {
      if ((*valid)[i]==1) {
        // calc weight
        double weight=sintd_areas[i]/dst_areas[i];
        
        // If weight is slightly bigger than one because of round off then push it back
        // if it's way over let it go, so we see it. 
        if ((weight > 1.0) && (weight < 1.0+1.0E-10)) weight = 1.0;
        
        // return weight
        (*wgts)[i]=weight;
      }
    }


    // Loop setting areas
    for (int i=0; i<dst_elems.size(); i++) {
      if ((*valid)[i]==1) {
        (*sintd_areas_out)[i]=sintd_areas[i];
        (*dst_areas_out)[i]=dst_areas[i];
      }
    }

#undef  MAX_NUM_POLY_NODES
#undef  MAX_NUM_POLY_COORDS_2D    
  }




// Search for ELEMS BEGIN --------------------------------
// NOTE::This finds the list of meshB elements which intersect with each meshA element and returns
//       it in sres

static int num_intersecting_elems(MBMesh *mbmp, const MBMesh_BBox &meshBBBox, double btol, double nexp) {
  
  int ret = 0;

  //Get MOAB Mesh
  Interface *moab_mesh=mbmp->mesh;

  // MOAB error
  int merr;

  // Get a range containing all elements
  Range range_elem;
  merr=moab_mesh->get_entities_by_dimension(0,mbmp->pdim,range_elem);
  if (merr != MB_SUCCESS) {
    Throw() << "MOAB ERROR:: "<<moab::ErrorCodeStr[merr];                                     
  }     

  // Loop over elements
  for(Range::iterator it=range_elem.begin(); it !=range_elem.end(); it++) {
    const EntityHandle elem=*it;
   
     const MBMesh_BBox bounding_box(mbmp, elem, nexp);
  
     // First check to see if the box even intersects the meshB mesh bounding
     // box.  
     if (MBMesh_BBoxIntersect(meshBBBox, bounding_box, btol)) ++ret;
  }

  return ret;
}

  static void populate_box_elems(OTree *box, MBMesh_SearchResult &result, MBMesh *mbmp, const MBMesh_BBox &meshBBBox, double btol, double nexp) {

  // Get spatial dim of mesh
  int sdim = mbmp->sdim;

  //Get MOAB Mesh
  Interface *moab_mesh=mbmp->mesh;

  // MOAB error
  int merr;

  // Get a range containing all elements
  Range range_elem;
  merr=moab_mesh->get_entities_by_dimension(0,mbmp->pdim,range_elem);
  if (merr != MB_SUCCESS) {
    Throw() << "MOAB ERROR:: "<<moab::ErrorCodeStr[merr];                                     
  }     

  // Loop over elements
  for(Range::iterator it=range_elem.begin(); it !=range_elem.end(); it++) {
    const EntityHandle elem=*it;
   
     MBMesh_BBox bounding_box(mbmp, elem, nexp);
  
     // First check to see if the box even intersects the meshB mesh bounding
     // box.  
     if (MBMesh_BBoxIntersect(meshBBBox, bounding_box, btol)) {
         
       // Create Search result
       MBMesh_Search_result *sr=new MBMesh_Search_result();       
       sr->src_elem=elem;
       sr->dst_elems.clear();
       
       // Add it to results list
       result.push_back(sr);
       
       // Add it to tree
       double min[3], max[3];
       
       min[0] = bounding_box.getMin()[0] - btol;
       min[1] = bounding_box.getMin()[1] - btol;
       if (sdim >2) min[2] = bounding_box.getMin()[2] - btol;
       else min[2] = - btol;

       max[0] = bounding_box.getMax()[0] + btol;
       max[1] = bounding_box.getMax()[1] + btol;
       if (sdim >2) max[2] = bounding_box.getMax()[2] + btol;
       else  max[2] = btol;
    
       /*
       if (elem.get_id() == 2426) {
         std::cout << "elem 2426, bbox=" << bounding_box << std::endl;
       }*/

       // Add element to search tree
       box->add(min, max, (void*)sr);
     }
  
    }
    
  }


struct OctSearchElemsData {
  EntityHandle meshB_elem;
  bool found;
};

static int found_func_elems(void *c, void *y) {
  MBMesh_Search_result *sr = static_cast<MBMesh_Search_result*>(c);
  OctSearchElemsData *si = static_cast<OctSearchElemsData*>(y);


  // It might make sense to do something here to trim down the 
  // number of candidates beyond just those that intersect the 
  // minmax box of the search element. However, I'm not sure
  // that there is anything that would be more efficient than
  // just gathering them all and letting the clipping code
  // handle the detection of true intersection as is what
  // is currently being done. 

  sr->dst_elems.push_back(si->meshB_elem);
  si->found=true;

  // Keep searching
  return 0;
}

// The main routine
// This constructs the list of meshB elements which intersects with each meshA element and returns
// this list in result. Each search_result in result contains a meshA element in elem and a list of intersecting meshB
// elements in elem  This function is symmertrical with regard to meshA or meshB, and when used
// for regrid either src or dest mesh may be used for either

  void MBMesh_OctSearchElems(MBMesh *mbmAp, int unmappedactionA, MBMesh *mbmBp, int unmappedactionB, 
                      double stol, MBMesh_SearchResult &result) {
  Trace __trace("OctSearchElems(const Mesh &meshA, const Mesh &meshB, UInt meshB_obj_type, SearchResult &result, double stol, std::vector<const MeshObj*> *to_investigate");

  if (mbmAp->sdim != mbmBp->sdim) {
    Throw() << "Meshes must have same spatial dim for search";
  }


  // Get a bounding box for the meshB mesh.
  // TODO: NEED TO MAKE BOUNDING BOX ONLY DEPEND ON NON-MASKED ELEMENTS
  MBMesh_BBox meshBBBox(mbmBp);
  
  // declare some variables
  OTree *box=NULL;  
  const double normexp = 0.15;
  const double meshBint = 1e-8;
 
  // EVENTUALLY SKIP MASKED ELEMENTS WHEN ADDING SOURCE TO TREE
 
  // Count number of elements in tree
  int num_box = num_intersecting_elems(mbmAp, meshBBBox, meshBint, normexp);
  
  // Construct box tree
  box=new OTree(num_box); 
  
  // Construct search result list 
  result.reserve(num_box);
  
  // Fill tree with search result structs to fill
  // with intesecting elements
  populate_box_elems(box, result, mbmAp, meshBBBox, meshBint, normexp);
  box->commit();
  
  // Dimension of meshB
  UInt sdim = mbmBp->sdim;
    
  //Get MOAB Mesh
  Interface *moab_meshA=mbmAp->mesh;
  Interface *moab_meshB=mbmBp->mesh;

  // MOAB error
  int merr;

  // Get a range containing all elements
  Range range_elem;
  merr=moab_meshB->get_entities_by_dimension(0,mbmBp->pdim,range_elem);
  if (merr != MB_SUCCESS) {
    Throw() << "MOAB ERROR:: "<<moab::ErrorCodeStr[merr];                                     
  }     
  
  // Loop the mesh B elements, find the corresponding mesh A elements
  bool meshB_elem_not_found=false;
  for(Range::iterator it=range_elem.begin(); it !=range_elem.end(); it++) {
    const EntityHandle elem=*it;
    
    MBMesh_BBox meshB_bbox(mbmBp, elem, normexp);

    double min[3], max[3];       
    min[0] = meshB_bbox.getMin()[0] - stol;
    min[1] = meshB_bbox.getMin()[1] - stol;
    if (sdim >2) min[2] = meshB_bbox.getMin()[2] - stol;
    else min[2] = - stol;

    max[0] = meshB_bbox.getMax()[0] + stol;
    max[1] = meshB_bbox.getMax()[1] + stol;
    if (sdim >2) max[2] = meshB_bbox.getMax()[2] + stol;
    else  max[2] = stol;

    OctSearchElemsData si;
    si.meshB_elem=elem;
    si.found=false;

    box->runon(min, max, found_func_elems, (void*)&si);

    if (!si.found) {
      meshB_elem_not_found=true;
    } 

  } // for mesh B elems
  
  // Check for meshB elements which haven't been intersected
  if (meshB_elem_not_found) {
    if (unmappedactionB == ESMCI_UNMAPPEDACTION_ERROR) {
      Throw() << " Some mesh B elements do not intersect with mesh A";	
    } else if (unmappedactionB == ESMCI_UNMAPPEDACTION_IGNORE) {
      // don't do anything
    } else {
      Throw() << " Unknown unmappedaction option";
    }
  }

#if 0
  // Check for meshA elements which haven't been intersected
  // MIGHT BE MORE EFFICIENT TO CHECK IF MATRIX ROW SUMS TO 1.0
  if (unmappedactionA == ESMCI_UNMAPPEDACTION_ERROR) {
    SearchResult::iterator sb = result.begin(), se = result.end();
    for (; sb != se; sb++) {
      Search_result &sr = **sb;

      if (sr.elems.empty()) {
	Throw() << " Some mesh A elements do not intersect with mesh B";	
      }
    }
  }
#endif

   // Get rid of box tree
  if (box != NULL) delete box;
}


void calc_conserve_mat_serial_2D_2D_cart(MBMesh *srcmbmp, MBMesh *dstmbmp, MBMesh_SearchResult &sres, IWeights &iw, IWeights &src_frac, IWeights &dst_frac) {
  Trace __trace("calc_conserve_mat_serial(Mesh &srcmesh, Mesh &dstmesh, SearchResult &sres, IWeights &iw)");
    

  // determine if we should use the dst_frac variable
  bool use_dst_frac=false;

  // MOAB error
  int merr;

  // Get MOAB Meshes
  Interface *src_moab_mesh=srcmbmp->mesh;
  Interface *dst_moab_mesh=dstmbmp->mesh;

  // Declare vectors to hold weight and auxilary information
  std::vector<int> valid;
  std::vector<double> wgts;
  std::vector<double> areas;
  std::vector<double> dst_areas;
  std::vector<int> dst_gids;

  // Temporary buffers for concave case, 
  // so there isn't lots of reallocation
  std::vector<int> tmp_valid;
  std::vector<double> tmp_areas;
  std::vector<double> tmp_dst_areas;

  // Find maximum number of dst elements in search results
  int max_num_dst_elems=0;
  MBMesh_SearchResult::iterator sb = sres.begin(), se = sres.end();
  for (; sb != se; sb++) {
    // NOTE: sr.elem is a src element and sr.elems is a list of dst elements
    MBMesh_Search_result &sr = **sb;

    // If there are no associated dst elements then skip it
    if (sr.dst_elems.size() > max_num_dst_elems) max_num_dst_elems=sr.dst_elems.size();
  }


  // Allocate space for weight calc output arrays
  valid.resize(max_num_dst_elems,0);
  wgts.resize(max_num_dst_elems,0.0);
  areas.resize(max_num_dst_elems,0.0);
  dst_areas.resize(max_num_dst_elems,0.0);
  dst_gids.resize(max_num_dst_elems,0.0);

  // Loop through search results
  for (sb = sres.begin(); sb != se; sb++) {
    
    // NOTE: sr.elem is a dst element and sr.elems is a list of src elements
    MBMesh_Search_result &sr = **sb;

    // If there are no associated dst elements then skip it
    if (sr.dst_elems.size() == 0) continue;

    // Get src elem gid
    int src_gid;
    merr=src_moab_mesh->tag_get_data(srcmbmp->gid_tag, &sr.src_elem, 1, &src_gid);
    if (merr != MB_SUCCESS) {
      Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
    }

    // Get dst elem gids
    for (int i=0; i<sr.dst_elems.size(); i++) {
      merr=dst_moab_mesh->tag_get_data(dstmbmp->gid_tag, &(sr.dst_elems[i]), 1, &(dst_gids[i]));
      if (merr != MB_SUCCESS) {
        Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
      }
    }


 /* XMRKX */

#if 0
    // If this source element is masked then skip it
    if (src_mask_field) {
        const MeshObj &src_elem = *sr.elem;
        double *msk=src_mask_field->data(src_elem);
        if (*msk>0.5) {
          continue; // if this is masked, then go to next search result
          // TODO: put code in ESMCI_Search.C, so the masked source elements, don't get here
        }
    }


    // If this source element is creeped out during merging then skip it
    double src_frac2=1.0;
    if(src_frac2_field){
      const MeshObj &src_elem = *sr.elem;
      src_frac2=*(double *)(src_frac2_field->data(src_elem));
      if (src_frac2 == 0.0) continue; 
    }
#endif


    // Declare src_elem_area
    double src_elem_area;
  
    // Calculate weights
    std::vector<sintd_node *> tmp_nodes;  
    std::vector<sintd_cell *> tmp_cells;  
    MBMesh_calc_1st_order_weights_2D_2D_cart(srcmbmp, sr.src_elem, dstmbmp, sr.dst_elems,
                                             &src_elem_area, &valid, &wgts, &areas, &dst_areas);



#if 0
    // Invalidate masked destination elements
    if (dst_mask_field) {
      for (int i=0; i<sr.elems.size(); i++) {
        const MeshObj &dst_elem = *sr.elems[i];
        double *msk=dst_mask_field->data(dst_elem);
        if (*msk>0.5) {
          valid[i]=0;
        }
      }
    }
    // Invalidate creeped out dst element
    if(dst_frac2_field){
      for (int i=0; i<sr.elems.size(); i++) {
        const MeshObj &dst_elem = *sr.elems[i];
        double *dst_frac2=dst_frac2_field->data(dst_elem);
        if (*dst_frac2 == 0.0){
          valid[i] = 0;
          continue;
        }
      }
    }
#endif

    // Count number of valid weights
    int num_valid=0;
    for (int i=0; i<sr.dst_elems.size(); i++) {
      if (valid[i]==1) num_valid++;
    }

    // If none valid, then don't add weights
    if (num_valid < 1) continue;

#if 0
    // Append only valid nodes/cells
    std::copy(tmp_nodes.begin(), tmp_nodes.end(), std::back_inserter(sintd_nodes));
    std::copy(tmp_cells.begin(), tmp_cells.end(), std::back_inserter(sintd_cells));
#endif

      // Temporary empty col with negatives so unset values
      // can be detected if they sneak through
      IWeights::Entry col_empty(-1, 0, -1.0, 0);

      // Insert fracs into src_frac
      {
        // Allocate column of empty entries
        std::vector<IWeights::Entry> col;
        col.resize(num_valid,col_empty);
        
        // Put weights into column
        int j=0;
        for (int i=0; i<sr.dst_elems.size(); i++) {
          if (valid[i]==1) {
            col[j].id=dst_gids[i];
            col[j].value=areas[i]/src_elem_area;
            j++;
          }
        }
        
        // Set row info
        IWeights::Entry row(src_gid, 0, 0.0, 0);
        
        // Put weights into weight matrix
        src_frac.InsertRowMerge(row, col);       
      }

#if 0
      // Put weights into dst_frac and then add
      // Don't do this if there are no user areas
      if (use_dst_frac) {
        for (int i=0; i<sr.dst_elems.size(); i++) {
          if (valid[i]==1) {
            // Set col info
            IWeights::Entry col(sr.src_elem->get_id(), 0, 
                                src_frac2*wgts[i], 0);

            // Set row info
            IWeights::Entry row(sr.dst_elems[i]->get_id(), 0, 0.0, 0);

            // Put weights into weight matrix
            dst_frac.InsertRowMergeSingle(row, col);  
          }
        }
      }
#endif      



      // Calculate source user area adjustment
      double src_user_area_adj=1.0;
#if 0
      if (src_area_field) {
          const MeshObj &src_elem = *sr.elem;
          double *area=src_area_field->data(src_elem);
          src_user_area_adj=*area/src_elem_area;
      }
#endif
      
      // Put weights into row column and then add
      for (int i=0; i<sr.dst_elems.size(); i++) {
        if (valid[i]==1) {

          // Calculate dest user area adjustment
          double dst_user_area_adj=1.0;
#if 0
          if (dst_area_field) {
            const MeshObj &dst_elem = *(sr.dst_elems[i]);
            double *area=dst_area_field->data(dst_elem);
            if (*area==0.0) Throw() << "0.0 user area in destination grid";
            dst_user_area_adj=dst_areas[i]/(*area);
          }
#endif

          // Set col info
          IWeights::Entry col(src_gid, 0, 
                              src_user_area_adj*dst_user_area_adj*wgts[i], 0);
          //                              src_user_area_adj*dst_user_area_adj*src_frac2*wgts[i], 0);

          // Set row info
          IWeights::Entry row(dst_gids[i], 0, 0.0, 0);

          // Put weights into weight matrix
          iw.InsertRowMergeSingle(row, col);  
        }
      }
} // for searchresult

}


void calc_conserve_mat(MBMesh *srcmbmp, MBMesh *dstmbmp, MBMesh_SearchResult &sres, IWeights &iw, IWeights &src_frac, IWeights &dst_frac) {
  Trace __trace("calc_conserve_mat_serial(Mesh &srcmesh, Mesh &dstmesh, SearchResult &sres, IWeights &iw)");

  // both meshes have to have the same dimensions
  if (srcmbmp->pdim != dstmbmp->pdim) {
    Throw() << "src and dst mesh must have the same parametric dimension for conservative regridding";
  }

  if (srcmbmp->sdim != dstmbmp->sdim) {
    Throw() << "src and dst mesh must have the same spatial dimension for conservative regridding";
  }

  // Get dimension, because they're the same can just get one
  int sdim=srcmbmp->sdim;
  int pdim=srcmbmp->pdim;

  // Get weights depending on dimension
  if (pdim==2) {
    if (sdim==2) {
      calc_conserve_mat_serial_2D_2D_cart(srcmbmp, dstmbmp, sres, iw, src_frac, dst_frac);
    } else if (sdim==3) {
      //      calc_conserve_mat_serial_2D_3D_sph(srcmesh, dstmesh, midmesh, sres, iw, src_frac, dst_frac, zz);
    }
  } else if (pdim==3) {
    if (sdim==3) {
      // calc_conserve_mat_serial_3D_3D_cart(srcmesh, dstmesh, midmesh, sres, iw, src_frac, dst_frac, zz);
    } else {
      Throw() << "Meshes with parametric dim == 3, but spatial dim !=3 not supported for conservative regridding";
    }
  } else {
    Throw() << "Meshes with parametric dimension != 2 or 3 not supported for conservative regridding";
  }

}


void calc_cnsrv_regrid_wgts(MBMesh *srcmbmp, MBMesh *dstmbmp, IWeights &wts) {
#define ESMC_METHOD "calc_cnsrc_regrid_wgts()"

  // Do search
  MBMesh_SearchResult result;
  MBMesh_OctSearchElems(srcmbmp, ESMCI_UNMAPPEDACTION_IGNORE, dstmbmp, ESMCI_UNMAPPEDACTION_IGNORE, 
                 1.0E-8, result);

  IWeights src_frac;
  IWeights dst_frac;
  calc_conserve_mat(srcmbmp, dstmbmp, result, wts, src_frac, dst_frac);

}

#endif // ESMF_MOAB
