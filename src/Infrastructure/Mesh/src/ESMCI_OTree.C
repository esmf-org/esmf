// $Id: ESMCI_OTree.C,v 1.2.2.1 2010/02/05 19:59:44 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_OTree.C"
//==============================================================================
//
// ESMC OTree method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ spatial search methods declared
// in ESMCI_OTree.h. This code/algorithm developed by Bob Oehmke. Note to self,
// put ref here when article is done making its way through publication process. 
//
//-----------------------------------------------------------------------------

// include associated header file
#include <Mesh/include/ESMCI_OTree.h>
#include "stdlib.h"

#include <algorithm>

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_OTree.C,v 1.2.2.1 2010/02/05 19:59:44 svasquez Exp $";
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------


// Set up ESMCI name space for these methods
namespace ESMCI{  


//-----------------------------------------------------------------------------
//
// Public Interfaces
//
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::OTree()"
//BOPI
// !IROUTINE:  OTree
//
// !INTERFACE:
OTree::OTree(
//
// !RETURN VALUE:
//    Pointer to a new OTree
//
// !ARGUMENTS:

	     int max_size

  ){
//
// !DESCRIPTION:
//   Construct OTree
//EOPI
//-----------------------------------------------------------------------------
   Trace __trace("OTree::OTree()");

  // allocate node mem
  mem=NULL;
  mem=new ONode[max_size];

  // Set values
  max_size_mem=max_size;
  curr_size_mem=0;
  root=NULL;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::~OTree()"
//BOPI
// !IROUTINE:  ~OTree
//
// !INTERFACE:
 OTree::~OTree(void){
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
// none
//
// !DESCRIPTION:
//  Destructor for OTree, deallocates all internal memory, etc. 
//
//EOPI
//-----------------------------------------------------------------------------
   Trace __trace("OTree::~OTree()");

   // Reset default values
   max_size_mem=0;
   curr_size_mem=0;
   root=NULL;
   
   // Deallocate memory
   if (mem!=NULL) delete [] mem;
   mem=NULL;   
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::OTree::add()"
//BOP
// !IROUTINE:  add
//
// !INTERFACE:
void OTree::add(

//
// !RETURN VALUE:
//  none
// 
// !ARGUMENTS:
//
	       double min[3],
	       double max[3],
	       void *data
  ) {
//
// !DESCRIPTION:
// Add an item to the OTree min,max gives the boundarys of the item and data 
// represents the item.
//EOP
//-----------------------------------------------------------------------------
   Trace __trace("OTree::add()");

  // Error check
  if (curr_size_mem > max_size_mem-1) {
    Throw() << "OTree full";
  }

  // Add item
  mem[curr_size_mem].min[0]=min[0];
  mem[curr_size_mem].min[1]=min[1];
  mem[curr_size_mem].min[2]=min[2];

  mem[curr_size_mem].max[0]=max[0];
  mem[curr_size_mem].max[1]=max[1];
  mem[curr_size_mem].max[2]=max[2];

  mem[curr_size_mem].data=data;

  // Set defaults
  mem[curr_size_mem].children=NULL;
  mem[curr_size_mem].next=NULL;

  // Advance to next position
  curr_size_mem++;

}
//-----------------------------------------------------------------------------


#define CALC_ITYPE_2D(nmin,nmax,tmin,tmax)                             \
          (((nmin[0] > tmin[0]))      | ((nmax[0] > tmax[0]) << 1) | \
	   ((nmin[1] > tmin[1]) << 2) | ((nmax[1] > tmax[1]) << 3))

#define CALC_ITYPE_1D(nmin,nmax,tmin,tmax)                             \
  (((nmin > tmin) | ((nmax > tmax) << 1))+16)

  void _add_onode(ONode *root, ONode *to_be_added) {
   Trace __trace("OTree::_add_onode()");
    
    // Root is first curr_node
    ONode *curr_node=root;
    
    // if tree is empty return error     
    if (curr_node==NULL)  Throw() << "Bad commit";
 
    // Loop until node has been added
    int done=0;
    while (!done) {
      //// calculate intersection type relative to curr_node
      const unsigned int one=0x1;
      unsigned int curr_itype;
      curr_itype=(one<<CALC_ITYPE_2D(to_be_added->min,
					to_be_added->max,
					curr_node->min,
					curr_node->max));
      curr_itype|=(one<<CALC_ITYPE_1D(to_be_added->min[2],
					  to_be_added->max[2],
					  curr_node->min[2],
					  curr_node->max[2]));
      
      //// Look for curr_itype among curr_nodes children
      ONode *found_node=NULL;
      for (ONode *chn=curr_node->children; chn!=NULL; chn=chn->next) {
	if (chn->itype==curr_itype) {
	  found_node=chn;
	  break;
	}
      }

      //// if found then search down that branch, otherwise add it as a child
      //// NOTE: this automatically handles the case where 
      ////       curr_node->children is empty
      if (found_node) curr_node=found_node;
      else {
	to_be_added->itype=curr_itype;
	to_be_added->next=curr_node->children;
	curr_node->children=to_be_added;
	done=1;
      }
    }
  }
  
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::OTree::commit()"
//BOP
// !IROUTINE:  commit
//
// !INTERFACE:
void OTree::commit(

//
// !RETURN VALUE:
//  none
//
// !ARGUMENTS:
//  none
  ) {
//
// !DESCRIPTION:
// Build tree from previously added items
//EOP
//-----------------------------------------------------------------------------
   Trace __trace("OTree::commit()");

  // Reset root
  root=NULL;

  // Scramble to reduce chance of degenerate trees
  std::random_shuffle (mem,mem+curr_size_mem);  

  // Make first node root
  if (curr_size_mem > 0) root=mem;
  else return; // no nodes, so leave

  // Add rest of nodes
  for (int i=1; i<curr_size_mem; i++) {
    //// Add
    _add_onode(root, mem+i);
  }
  
}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
  typedef struct {
    double min[3];
    double max[3];
    int (*func)(void *data,void *func_data);
    void *func_data;
  } RUNON_INFO;


  unsigned int search_code_to_itype_list_2D[16]={
    0x0, 0x0,    0x0,    0x0, 
    0x0, 0xcc00, 0x5500, 0xff00, 
    0x0, 0xc0c,  0x505,  0xf0f, 
    0x0, 0xcccc, 0x5555, 0xffff};

  unsigned int search_code_to_itype_list_1D[4]={
    0x0,0xc0000,0x50000,0xf0000};

#define INTERSECTION_SEARCH_CODE_2D 15
#define INTERSECTION_SEARCH_CODE_1D 3

#define CALC_SEARCH_CODE_2D(qmin,qmax,tmin,tmax)                         \
          ((qmax[0] >= tmin[0]))      | ((qmin[0] <= tmax[0]) << 1) | \
          ((qmax[1] >= tmin[1]) << 2) | ((qmin[1] <= tmax[1]) << 3)

#define CALC_SEARCH_CODE_1D(qmin,qmax,tmin,tmax)                         \
          ((qmax >= tmin)   | ((qmin <= tmax) << 1))


  // do intersection search on a node (and recurse down children if necessary)
  int _runon_onode(ONode *node, RUNON_INFO *ri) {
    //  BECAUSE THERE IS NO THROW IN THIS FUNC, COMMENT THIS OUT FOR EFFICIENCY
    //  Trace __trace("OTree::_runon_onode()");

    // Calculate our search code based on search min-max and node min-max
    int search_code_2D=CALC_SEARCH_CODE_2D(ri->min,ri->max,node->min,node->max);
    int search_code_1D=CALC_SEARCH_CODE_1D(ri->min[2],ri->max[2],node->min[2],node->max[2]);
    
    // if this node intersects, run function on data
    if (search_code_2D==INTERSECTION_SEARCH_CODE_2D &&
	search_code_1D==INTERSECTION_SEARCH_CODE_1D) {
      int rc=ri->func(node->data,ri->func_data);
      if (rc) return rc;  // if return code is non-zero then return
    }

    // Get the search itype list based on the search code
    unsigned int it_list_2D=search_code_to_itype_list_2D[search_code_2D];
    unsigned int it_list_1D=search_code_to_itype_list_1D[search_code_1D];
    
    // Loop through children, searching them if necessary
    for (ONode *chn=node->children; chn!=NULL; chn=chn->next) {
      if ((chn->itype&it_list_2D) && (chn->itype&it_list_1D)) {
	int rc=_runon_onode(chn, ri);
	if (rc) return rc; // if return code is non-zero then return
      }
    }    
    
    return 0;
  }

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::OTree::runon()"
//BOP
// !IROUTINE:  runon
//
// !INTERFACE:
int OTree::runon(

//
// !RETURN VALUE:
//  user func return
//
// !ARGUMENTS:
//
	       double min[3],
	       double max[3],
	       int (*func)(void *data,void *func_data),
	       void *func_data
  ) {
//
// !DESCRIPTION:
// Run func on each object in the tree whose min-max box overlaps the input min-max. 
// If func returns anything but 0, then the process stops and runon returns what func returned. 
//EOP
//-----------------------------------------------------------------------------
  Trace __trace("OTree::runon()");

  RUNON_INFO ri;

  // if tree empty return
  if (root==NULL) {
   return 0;
  }


  // Load search info
  ri.min[0]=min[0];
  ri.min[1]=min[1];
  ri.min[2]=min[2];
  ri.max[0]=max[0];
  ri.max[1]=max[1];
  ri.max[2]=max[2];
  ri.func=func;
  ri.func_data=func_data;

  // Start search with root
  return _runon_onode(root, &ri);

}
//-----------------------------------------------------------------------------



} // END ESMCI name space
//-----------------------------------------------------------------------------










