/*****************************************************
 *   NAME: BOX3D
 *
 *   PURPOSE: Min Max Box search tree
 *   
 *   AUTHOR: Bob Oehmke 6/8/2007
 *
 ****************************************************/
#include <Mesh/include/ESMCI_OctBox3d.h>
#include <stdlib.h>
#include <math.h>


extern "C" {

/*****************************************************************
 *   NAME: Create_BOX3D
 *
 *   PURPOSE: Create a Min Max Box search tree
 *   
 *   INPUT:
 *       num_boxes - number of boxes which will be added to tree
 *
 *   OUTPUT:
 *       otree - pointer to newly created search tree
 *
 *   RETURNS:
 *       0 ->failure 1->otherwise
 *         
 *   AUTHOR: Bob Oehmke 6/8/2007
 *
*****************************************************************/
int Create_BOX3D(BOX3D **otree, int num_boxes)
{
  BOX3D *tree;

  /* Allocate tree */
  tree=(BOX3D *)calloc(1,sizeof(BOX3D));
   
  /* Initialize Info */
  tree->num_nodes=num_boxes;
  tree->nodes=(BOX3DNODE *)calloc(num_boxes,sizeof(BOX3DNODE));
  tree->curr_node=0;

 /* set output */
  *otree=tree;

  return 1;
}

/*****************************************************************
 *   NAME: Add_BOX3D
 *
 *   PURPOSE: Add a box to the search tree (note tree has to be finalized
 *            before it can be used.
 *   
 *   INPUT:
 *       tree  - the search tree to add to.
 *       min.max - the extents of the box to add
 *       contents - the data to add to this box (this data will be
 *                  passed to the user function during RunOn).
 *
 *   OUTPUT:
 *
 *   RETURNS:
 *       0 ->failure 1->otherwise
 *         
 *   AUTHOR: Bob Oehmke 6/8/2007
 *
*****************************************************************/
int Add_BOX3D(BOX3D *tree, double min[3], double max[3], void *contents)
{
  BOX3DNODE *node;

  /* Check for errors */
   if (tree->curr_node >= tree->num_nodes)
   {
      return 0;
   }


   /* Get current node */
   node=tree->nodes+tree->curr_node;

     /* Fill node */
   node->min[0]=min[0];
   node->min[1]=min[1];
   node->min[2]=min[2];

   node->max[0]=max[0];
   node->max[1]=max[1];
   node->max[2]=max[2];

   node->contents=contents;

     /* advance to next node */
   tree->curr_node++;

   return 1;
}

/* Macros for node location in tree */
#define QUADRANT(minit,maxit,minnp,maxnp) (((minnp)[0]>(minit)[0])|(((maxnp)[0]>(maxit)[0])<<1));
#define HEXTANT(minit,maxit,minnp,maxnp) (((minnp)[1]>(minit)[1])|(((maxnp)[1]>(maxit)[1])<<1)|(((minnp)[2]>(minit)[2])<<2)|(((maxnp)[2]>(maxit)[2])<<3));

static int _add_node(BOX3D *tree, BOX3DNODE *n)
{
  int q;
  long h;
  BOX3DNODE *tmp,*last,*lc;

  /* if there's no root make this the root */
  if (!tree->root)
    {
      tree->root=n;
      return 0;
    }

  /* Loop to find node's place in tree */
  tmp=tree->root;
  while (tmp)
    {
      last=tmp;

      q=QUADRANT(tmp->min,tmp->max,n->min,n->max);
      h=0x1<<HEXTANT(tmp->min,tmp->max,n->min,n->max);
      
      for (lc=tmp->child[q]; lc; lc=lc->next)
	{
	  if (lc->whoami&h) break;
	}

      tmp=lc;
    }

  /* Add the new node to its place */
  n->whoami=h;
  n->next=last->child[q];  /* Possible performance improvement */
  last->child[q]=n;        /* add the new node at the end of the list */

  return 0;
}


/*****************************************************************
 *   NAME: Finalize_BOX3D(BOX3D *tree)
 *
 *   PURPOSE: Called after all the boxes have been added to set the 
 *            tree up for use. 
 *   
 *   INPUT:
 *       tree  - the search tree to add to.
 *
 *   OUTPUT:
 *
 *   RETURNS:
 *       0 ->failure 1->otherwise
 *         
 *   AUTHOR: Bob Oehmke 6/8/2007
 *
*****************************************************************/
int Finalize_BOX3D(BOX3D *tree)
{
  int i,j;  
  unsigned long r,max;
  float rf,maxf;
  BOX3DNODE tmp;  

  /* Scramble nodes (Add a pseudorandom element to guard against pathological orderings) */
  /* (In the future I will come up with a cleaner way to do this) */
  max=0x1<<31;
  maxf=max;
  for (i=0; i<tree->curr_node; i++)
    {
       /* get a random location in the node list */
#if defined (ESMF_OS_MinGW)
// TODO: Look for a Standard-conforming way to do random numbers that
// gives more digits than rand().  For example, see Stroustrup 3rd
// edition, section 22.7.
       r = 0;
#else
       r=random();
#endif
       rf=r;
       j=tree->curr_node*rf/maxf;

       /* Swap'em */
       tmp=tree->nodes[i];
       tree->nodes[i]=tree->nodes[j];
       tree->nodes[j]=tmp;
    }

  /* Add Nodes to tree */
  for (i=0; i<tree->curr_node; i++) 
    {
      _add_node(tree, tree->nodes+i);
    }

  return 1;
}

/* search struct */
typedef struct _SS
{
double min[3];
double max[3];
  int (*ufunc)(BOX3DNODE *,void *, void *);
void *user_data;
} SS;

/* Macros search */
#define WHICH_QUADRANT(minit,maxit,smin,smax) (((smax)[0]>(minit)[0])|(((smin)[0]>(maxit)[0])<<1));
#define WHICH_HEXTANT(minit,maxit,smin,smax) (((smax)[1]>(minit)[1])|(((smin)[1]>(maxit)[1])<<1)|(((smax)[2]>(minit)[2])<<2)|(((smin)[2]>(maxit)[2])<<3));

static int branches_to_search[16]={0x0505, 0x0f0f, 0x0000, 0x0c0c,
                                   0x5555, 0xffff, 0x0000, 0xcccc,
                                   0x0000, 0x0000, 0x0000, 0x0000,
                                   0x5500, 0xff00, 0x0000, 0xcc00};


static int _search_further(BOX3DNODE *n, SS *s)
{
  int ret;
  int q;
  long h, branches;
  BOX3DNODE *lc;

  /* initialize return */
  ret=0;

  /* find which parts to search */
  q=WHICH_QUADRANT(n->min,n->max,s->min,s->max);
  h=WHICH_HEXTANT(n->min,n->max,s->min,s->max);
  branches=branches_to_search[h];

  /* search this node */
  switch(q)
    {
    case 0:
      for (lc=n->child[0]; lc; lc=lc->next)
	{
	  if (lc->whoami&branches)
	    {
	      if (ret=_search_further(lc,s)) return ret; 
	    }
	}

      for (lc=n->child[2]; lc; lc=lc->next)
	{
	  if (lc->whoami&branches)
	    {
	      if (ret=_search_further(lc,s)) return ret; 
	    }
	}
      break;

    case 1:
      for (lc=n->child[0]; lc; lc=lc->next)
	{
	  if (lc->whoami&branches)
	    {
	      if (ret=_search_further(lc,s)) return ret; 
	    }
	}

      for (lc=n->child[1]; lc; lc=lc->next)
	{
	  if (lc->whoami&branches)
	    {
	      if (ret=_search_further(lc,s)) return ret; 
	    }
	}

      for (lc=n->child[2]; lc; lc=lc->next)
	{
	  if (lc->whoami&branches)
	    {
	      if (ret=_search_further(lc,s)) return ret; 
	    }
	}

      for (lc=n->child[3]; lc; lc=lc->next)
	{
	  if (lc->whoami&branches)
	    {
	      if (ret=_search_further(lc,s)) return ret; 
	    }
	}
      break;

    case 2:

      break;

    case 3:
      for (lc=n->child[2]; lc; lc=lc->next)
	{
	  if (lc->whoami&branches)
	    {
	      if (ret=_search_further(lc,s)) return ret; 
	    }
	}

      for (lc=n->child[3]; lc; lc=lc->next)
	{
	  if (lc->whoami&branches)
	    {
	      if (ret=_search_further(lc,s)) return ret; 
	    }
	}
      break;


    default:
      /* should never happen */
    break;
    }

  /* does this node intersect, if so run function */
  if ((h==5) && (q==1)) 
    {
      ret=(*s->ufunc)(n, n->contents,s->user_data);
    }

  return ret;
}

/*****************************************************************
 *   NAME: Runon_intersecting_BOX3D
 *
 *   PURPOSE: Runs a user function on every box in the tree that intersects
 *            the passed in one.  
 *   
 *   INPUT:
 *       tree  - the search tree to search.
 *       min,max - the extents of the box to search on
 *       ufunc   - the user function to call on intersecting boxes.
 *                 ufunc is called like ufunc(box_in_tree_data, yours).
 *                 NOTE: if ufunc returns a non-zero value the search
 *                 ends and that value is returned from Runon_....
 *       yours   - user data to pass to ufunc
 *
 *   OUTPUT:
 *
 *   RETURNS:
 *       0-> error
 *       !0 -> the return value of ufunc.
 *         
 *   AUTHOR: Bob Oehmke 6/8/2007
 *
*****************************************************************/
int Runon_intersecting_BOX3D(BOX3D *tree, double min[3], double max[3], int (*ufunc)(BOX3DNODE *n, void *, void *), void *yours)
{
  SS s;

  /* If tree is empty return */
  if (!(tree->root)) return 0;

  /* setup search structure 
   EQUAL_TOL_BOX3D is to remove problems with min=max
   and box edges being equal */
  s.min[0]=min[0]-EQUAL_TOL_BOX3D;
  s.min[1]=min[1]-EQUAL_TOL_BOX3D;
  s.min[2]=min[2]-EQUAL_TOL_BOX3D;
  s.max[0]=max[0]+EQUAL_TOL_BOX3D;
  s.max[1]=max[1]+EQUAL_TOL_BOX3D;
  s.max[2]=max[2]+EQUAL_TOL_BOX3D;

  s.ufunc=ufunc;
  s.user_data=yours;

  /* do search */
  return _search_further(tree->root, &s);
}



/*****************************************************************
 *   NAME: Destroy_BOX3D
 *
 *   PURPOSE: Free's a box tree and sets the pointer to null.
 *   
 *   INPUT:
 *       iotree - tree to deallocate.
 *   OUTPUT:
 *
 *   RETURNS:
 *       0 ->failure 1->otherwise
 *         
 *   AUTHOR: Bob Oehmke 6/8/2007
 *
*****************************************************************/
int Destroy_BOX3D(BOX3D **iotree)
{
  BOX3D *tree;

  /* local version of tree */
  tree=*iotree;

  /* Deallocate nodes */
  free(tree->nodes);

  /* Deallocate tree */
  free(tree);
   
  /* Set Tree to Null */
  *iotree=0;

  return 1;
}

} // extern "C"
