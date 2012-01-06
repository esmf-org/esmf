// $Id: ESMCI_XGridUtil.h,v 1.5 2012/01/06 20:17:47 svasquez Exp $
// Earth System Modeling Framework
// Copyright 2002-2012, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_XGridUtil_h
#define ESMCI_XGridUtil_h

#include <vector>
#include <Mesh/include/ESMCI_Mesh.h>
#include <Mesh/include/ESMCI_Sintdnode.h>
#include <Mesh/include/ESMCI_Interp.h>


namespace ESMCI {

void compute_midmesh(std::vector<sintd_node *> & sintd_nodes, std::vector<sintd_cell *> & sintd_cells, 
  int pdim, int sdim, Mesh *midmesh);
void compute_sintd_nodes_cells(double area, int num_sintd_nodes, double * sintd_coords, int pdim, int sdim, 
  std::vector<sintd_node *> * sintd_nodes, std::vector<sintd_cell *> * sintd_cells, struct Zoltan_Struct * zz);
void construct_sintd(double area, int num_sintd_nodes, double * sintd_coords, int pdim, int sdim, 
  std::vector<sintd_node *> * sintd_nodes, std::vector<sintd_cell *> * sintd_cells);
int online_regrid_xgrid(Mesh &srcmesh, Mesh &dstmesh, Mesh * midmesh, IWeights &wts,
  int *regridConserve, int *regridMethod, int *regridScheme,
  int *unmappedaction);

// Weiler Atherton Algorithm O(nlogn), Sutherland–Hodgman O(n**2)

struct xpoint{
  double c[2];
  bool visited;
  char label;
  xpoint() : visited(false) {}
  xpoint(double x_, double y_, char label_) : visited(false), label(label_) {
    c[0] = x_; c[1] = y_;
  }

  bool operator == (const xpoint & that) const{
    return (this->c[0] == that.c[0] && this->c[1] == that.c[1]);
  }
};

struct xpoint_equal{

  bool operator () (const xpoint & rhs, const xpoint & lhs){
    return (rhs.c[0] == lhs.c[0] && rhs.c[1] == lhs.c[1]);
  }
};

struct xedge{
  xpoint *s,*e;
  xpoint norm; // in CCW sense
};

struct polygon{
  std::list<xpoint> points;
};

// Compute the difference polygons: p-q
// p: subject
// q: clip
int weiler_clip_2D_2D(int num_p, double *p, int num_q, double *q, std::vector<polygon> & difference);

} // namespace

#endif
