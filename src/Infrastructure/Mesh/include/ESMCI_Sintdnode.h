// $Id: ESMCI_Sintdnode.h,v 1.2 2011/04/27 12:16:32 feiliu Exp $
// Earth System Modeling Framework
// Copyright 2002-2011, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_Sintdnode_h
#define ESMCI_Sintdnode_h

#include <exception>
#include <string>
#include <vector>
#include <Mesh/include/ESMCI_MeshObjTopo.h>
#include <Mesh/include/ESMCI_MeshObj.h>


namespace ESMCI {

class sintd_cell;

// sintd_node represents the intersection point
class sintd_node {

  private:
  int pdim;          // parametric dimension
  double *coords;    // coordinate of this node in space
  sintd_cell * cell; // genesis cell
  MeshObj * node;    // final MeshObj node

  public:
  sintd_node(int _pdim, double *c): pdim(_pdim) {
    coords = new double[pdim];
    for(int i = 0; i < pdim; i ++)
      coords[i] = c[i];
  }
  ~sintd_node(){
    delete coords;
  }
  // copy and assignment
  sintd_node(const sintd_node & src) : pdim(src.pdim){
    coords = new double[pdim];
    for(int i = 0; i < pdim; i ++)
      coords[i] = src.coords[i];
  }
  sintd_node & operator = (const sintd_node & src) {
    this->pdim = src.pdim;
    this->coords = new double [pdim];
    for(int i = 0; i < pdim; i ++)
      this->coords[i] = src.coords[i];
    return *this;
  }
  // access operator
  double operator [](int i) const {
    if(i < 0 || i >= pdim) throw std::string("sintd_node: access index out of range");
    return coords[i];
  }
  // operators for std::vector comparisons
  bool operator < (const sintd_node & that) const{
    for(int i = 0; i < pdim; i ++){
      if(coords[i] < that.coords[i]) return true;
      if(coords[i] ==that.coords[i]) continue;
      else return false;
    }
    return false;
  }
  bool operator == (const sintd_node & that) const{
    for(int i = 0; i < pdim; i ++)
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
    for(int i = 0; i < pdim; i ++)
      printf("%g, ", coords[i]);
    printf(")\n");
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
    std::vector<sintd_node *> nodes;
  public:
    sintd_cell(const std::vector<sintd_node *> & _nodes) : nodes(_nodes) {}
    int num_edges() const { return nodes.size(); }
    // TODO: do cell split
    MeshObjTopo * get_topo() const {
      if(nodes.size() == 3) return GetTopo("TRI3");
      if(nodes.size() == 4) return GetTopo("QUAD");
      throw std::string("Invalid cell found");
    }
    sintd_node * operator [](int i) const{
      if(i < 0 || i >= nodes.size()) throw std::string("sintd_cell: access index out of range");
      return nodes[i];
    }
    void replace_node(sintd_node * node){
      std::vector<sintd_node *>::iterator it = nodes.begin();
      for(; it != nodes.end(); it++)
        if(**it == *node) { *it = node; break; }
    }
};

} // namespace

#endif
