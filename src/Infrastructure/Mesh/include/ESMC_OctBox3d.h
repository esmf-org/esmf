/*****************************************************
 *   NAME: BOX3D
 *
 *   PURPOSE: Min Max Box search tree
 *   
 *   AUTHOR: Bob Oehmke 6/8/2007
 *
 ****************************************************/
#ifndef BOX3D_H
#define BOX3D_H


#ifdef __cplusplus
 extern "C" {
#endif

typedef struct _BOX3DNODE
{
  long whoami;

  double max[3];
  double min[3];

  struct _BOX3DNODE *child[4];

  void *contents;

  struct _BOX3DNODE *next;

} BOX3DNODE;

typedef struct _BOX3D
{
  struct _BOX3DNODE *root;

  int num_nodes;
  struct _BOX3DNODE *nodes;

  int curr_node;
   
} BOX3D;

int Create_BOX3D(BOX3D **otree, int num_boxes);
int Add_BOX3D(BOX3D *tree, double min[3], double max[3], void *contents);
int Finalize_BOX3D(BOX3D *tree);
int Runon_intersecting_BOX3D(BOX3D *tree, double min[3], double max[3], int (*ufunc)(BOX3DNODE *n, void *, void *), void *yours);
int Destroy_BOX3D(BOX3D **iotree);

#ifdef __cplusplus
 }
#endif

#define EQUAL_TOL_BOX3D 0.00000001

#endif
