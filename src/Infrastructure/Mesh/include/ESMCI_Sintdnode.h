// $Id$
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_Sintdnode_h
#define ESMCI_Sintdnode_h

#include <string>
#include <sstream>
#include <vector>
#include <cstdio>
#include <Mesh/include/ESMCI_MeshObjTopo.h>
#include <Mesh/include/ESMCI_MeshObj.h>
#include <Mesh/include/ESMCI_Exception.h>
#include <Mesh/include/ESMCI_MathUtil.h>
#include <Mesh/src/Zoltan/zoltan.h>


namespace ESMCI {

class sintd_cell;

// sintd_node represents the intersection point
class sintd_node {

  private:
  int sdim;          // spatial dimension
  double *coords;    // coordinate of this node in space
  sintd_cell * cell; // genesis cell
  MeshObj * node;    // final MeshObj node

  public:
  sintd_node(int _sdim, double *c): sdim(_sdim) {
    coords = new double[sdim];
    for(int i = 0; i < sdim; i ++)
      coords[i] = c[i];
  }
  ~sintd_node(){
    delete[] coords;
  }
  // copy and assignment
  sintd_node(const sintd_node & src) : sdim(src.sdim){
    coords = new double[sdim];
    for(int i = 0; i < sdim; i ++)
      coords[i] = src.coords[i];
  }
  sintd_node & operator = (const sintd_node & src) {
    this->sdim = src.sdim;
    delete[] coords;
    this->coords = new double [sdim];
    for(int i = 0; i < sdim; i ++)
      this->coords[i] = src.coords[i];
    return *this;
  }
  // access operator
  double operator [](int i) const {
    if(i < 0 || i >= sdim) Throw() << "sintd_node: access index out of range.\n";
    return coords[i];
  }
  double * get_coord() const { return coords; }
  int get_dim() const { return sdim; }
  // operators for std::vector comparisons
  bool operator < (const sintd_node & that) const{
    for(int i = 0; i < sdim; i ++){
      if(coords[i] < that.coords[i]) return true;
      if(coords[i] ==that.coords[i]) continue;
      else return false;
    }
    return false;
  }
  bool operator == (const sintd_node & that) const{
    for(int i = 0; i < sdim; i ++)
      if(coords[i] != that.coords[i]) return false;
    return true;
  }

  // cross reference the node
  MeshObj * get_node() const { return node; }
  void set_node(MeshObj * node) { this->node = node; }
  sintd_cell * get_cell() const { return cell; }
  void set_cell(sintd_cell * cell) { this->cell = cell; }
  
  // debug purpose
  void print() const{
    printf("intersection point: (");
    for(int i = 0; i < sdim; i ++)
      printf("%g, ", coords[i]);
    printf(")\n");
  }
  
  std::string toString(){
    std::ostringstream os; os << "(";
    for(int i = 0; i < sdim-1; i ++)
      os << coords[i] << ", "; 
    os << coords[sdim-1];
    os << ")";
    return os.str();
  }
};

class sintd_node_less{

  public:
  bool operator ()(const sintd_node * const a, const sintd_node * const b) const {
    return (*a).operator<(*b);
  }
};

class sintd_node_equal{

  public:
  bool operator ()(const sintd_node * const a, const sintd_node * const b) const {
    return (*a).operator==(*b);
  }
};

// sintd_cell represents the intersection cell
class sintd_cell {
  private:
    // nodes enclosing this cell
    double area;
    std::vector<sintd_node *> nodes;
  public:
    sintd_cell(double _area, const std::vector<sintd_node *> & _nodes) : 
      area(_area), nodes(_nodes) {}

    int num_edges() const { return nodes.size(); }

    // leave hook for 3d clipping
    MeshObjTopo * get_topo(int sdim, int pdim) const {
      if(pdim == 2){
        if(sdim == 2){
          if(nodes.size() == 3) return GetTopo("TRI3");
          if(nodes.size() == 4) return GetTopo("QUAD");
        } 
        if(sdim == 3){
          if(nodes.size() == 3) return GetTopo("TRI3_3D");
          if(nodes.size() == 4) return GetTopo("QUAD_3D");
        }
      }
      Throw() << "Invalid cell found.\n";
    }
    sintd_node * operator [](int i) const{
      if(i < 0 || i >= nodes.size()) Throw() << "sintd_cell: access index out of range.\n";
      return nodes[i];
    }
    void replace_node(sintd_node * node){
      std::vector<sintd_node *>::iterator it = nodes.begin();
      for(; it != nodes.end(); it++)
        if(**it == *node) { *it = node; break; }
    }

    double get_area() { return area;   }

    void get_centroid(double * centroid, int sdim, int pdim);

    void print(int me, int gid, int lid){
      printf("Cell (%d,%d,%d): { ", me, gid, lid+1);
      std::vector<sintd_node *>::iterator it = nodes.begin();
      for(; it != nodes.end(); it++)
        printf("%s, ", (*it)->toString().c_str());
      printf(" }\n");
    }

};

struct proc_count {
  int proc;
  int count;
  proc_count(int _proc, int _count): proc(_proc), count(_count) {}
};

struct proc_count_less {
  bool operator () (const proc_count & src, const proc_count & dst){
    return (src.count < dst.count);
  }
};

struct proc_count_equal {
  int me;
  proc_count_equal(int _me) : me(_me) {}
  bool operator () (const proc_count & src){
    return (src.proc == me);
  }
};

} // namespace

#endif
