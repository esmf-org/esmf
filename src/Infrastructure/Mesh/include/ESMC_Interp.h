// $Id: ESMC_Interp.h,v 1.3 2007/11/28 16:23:21 dneckels Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMC_Interp_h
#define ESMC_Interp_h

#include <mesh/ESMC_MEField.h>
#include <mesh/ESMC_Search.h>
#include <mesh/ESMC_GeomRendezvous.h>
#include <mesh/ESMC_Migrator.h>
#include <mesh/ESMC_SparseMsg.h>

#include <vector>
#include <ostream>
#include <map>

namespace ESMC {
  
class CommRel;
class MeshObj;

/*
 * Provides interpolation for serial and parallel meshes.  For 
 * serial the interpolatin is a simple search/application.  For parallel
 * we use the Rendezvous algorithm of Stewart et al.  The interface
 * is the same for both cases.
 */

class Mesh;
class Geom;

// Store/Migrate interp weights
class IWeights {

public:

  /*
   * Matrix entry object.  Entries are not linearized at this
   * point (that is delayed).  Instead, they are indexed by object id
   * and index on object.
   */
  struct Entry {
    Entry() : id(0), idx(0), value(0.0) {}
    Entry(long _id, char _idx = 0, double _value = 0.0) :
      id(_id), idx(_idx), value(_value) {}
    
    Entry(const Entry &rhs) :
     id(rhs.id), idx(rhs.idx), value(rhs.value)
    {
    }
    
    Entry &operator=(const Entry &rhs) {
      if (this == &rhs) return *this;
      id = rhs.id;
      idx = rhs.idx;
      value = rhs.value;
      return *this;
    }
    
    typedef UInt id_type;
    typedef char idx_type;
    typedef float value_type;
    
    id_type id;  // MeshObj id
    idx_type idx; // field index
    value_type value; // weight
    
    bool operator<(const Entry &rhs) const {
      if (id != rhs.id) return id < rhs.id;
      return idx < rhs.idx;
    }
    
    // Equality does not consider value
    bool operator==(const Entry &rhs) {
      return (id == rhs.id && idx == rhs.idx);
    }
    
  };  
  
  IWeights();
  
  IWeights(const IWeights &);
  
  IWeights &operator=(const IWeights &);
  
    
  void InsertRow(const Entry &row, const std::vector<Entry> &cols);
  
  void GetRowGIDS(std::vector<UInt> &gids);
  
  void GetColGIDS(std::vector<UInt> &gids);
  
  void Print(std::ostream &);
  
  /*
   * Migrate the matrix to the row decomposition given by
   * mesh.
   */
  void Migrate(Mesh &mesh);
  
  // Return the number of rows that use this id
  UInt NumRows(long id) const;
  
  void clear();
  
  /*
   * Gather the right hand side matrix rows to the column space of this
   * weightset.
   */
  void GatherToCol(IWeights &rhs);

  /*
   * Gather the tangent vectors for the mesh.  Template class TVECT supports
   * tv()(const MeshObj &node, const MEField<> &coords, double U[], double V[]) 
   * to return the
   * tangent vectors.  The vectors are returned as (U V), a 3x2 matrix, unless
   * transpose=true, where they we be a 2x3 matrix, (U V)^T.
   */ 
  template <typename TVECT>
  void GatherTangentVectors(const Mesh &mesh, TVECT tv, bool transpose = false); 
  
  /*
   * Removes the columns referencing the constraint row entries.  
   * Converts current column references to a constrained object into
   * a reference to non-constrained objects by assimilating the constrained
   * objects sensitivities.
   */
  void AssimilateConstraints(const IWeights &constraints);
  
  /*
   * Change a 3d->3d vector matrix into a matrix mapping u,v -> u,v
   * coordinates.
   */
  void ChangeCoords(const IWeights &src_uv, const IWeights &dst_uc);
  
  /*
   * Remove any rows that have a mask value < 1.  Also remove any rows assigned to
   * a node that is not locally owned.
   */
  void Prune(const Mesh &mesh, const MEField<> &mask);
  
  typedef std::map<Entry, std::vector<Entry> > WeightMap;
  
  WeightMap::iterator begin_row() { return weights.begin(); }
  WeightMap::iterator end_row() { return weights.end(); }
  WeightMap::const_iterator begin_row() const { return weights.begin(); }
  WeightMap::const_iterator end_row() const { return weights.end(); }
  
  void InsertRow(WeightMap::value_type &row) {
    InsertRow(row.first, row.second);
  }

  std::pair<int, int> count_matrix_entries() const;
  
  WeightMap weights;

};

// Migration Traits for IWeights

template <>
class SparsePack<IWeights::WeightMap::value_type> {
public:
  SparsePack(SparseMsg::buffer &b, IWeights::WeightMap::value_type &t);
  static UInt size(IWeights::WeightMap::value_type &t);
};

template <>
class SparseUnpack<IWeights::WeightMap::value_type> {
public:
  SparseUnpack(SparseMsg::buffer &b, IWeights::WeightMap::value_type &t);
};


template <>
struct MigTraits<IWeights> {
  
  typedef IWeights::WeightMap::iterator element_iterator;

  typedef IWeights::WeightMap::value_type element_type;
  
  typedef SparsePack<element_type> element_pack;
  
  static UInt element_pack_size(element_type &t) { return element_pack::size(t); }
  
  typedef SparseUnpack<element_type> element_unpack;
  
  static UInt get_id(element_type &t) { return t.first.id; }
    
  static element_iterator element_begin(IWeights &t) { return t.begin_row(); }
  
  static element_iterator element_end(IWeights &t) { return t.end_row(); }
  
  static void insert_element(IWeights & t, UInt , element_type &el) { t.InsertRow(el); }
  
  static void resize_object(IWeights &, UInt) {}
  
  static void clear_object(IWeights &t) { t.clear(); }
  
};
 
// Tangent vector support
template <typename TVECT>
void IWeights::GatherTangentVectors(const Mesh &mesh, TVECT tv, bool transpose) {
  
  clear();
  
  Mesh::const_iterator ni = mesh.node_begin(), ne = mesh.node_end();
  
  const MEField<> &coords = *mesh.GetCoordField();
  
  for (; ni != ne; ++ni) {
    
    const MeshObj &node = *ni;
    
    double U[3], V[3];
    
    tv(node, coords, U, V);

    if (!transpose) {
      
      for (UInt r = 0; r < 3; r++) {
        Entry row(node.get_id(), r);
        std::vector<Entry> col(2);
        col[0] = Entry(node.get_id(), 0, U[r]);
        col[1] = Entry(node.get_id(), 1, V[r]);
        InsertRow(row, col);
      } // r
      
    } else {
      for (UInt r = 0; r < 2; r++) {
            Entry row(node.get_id(), r);
            std::vector<Entry> col(3);
            double *val = r == 0 ? U : V;
            col[0] = Entry(node.get_id(), 0, val[0]);
            col[1] = Entry(node.get_id(), 1, val[1]);
            col[2] = Entry(node.get_id(), 2, val[2]);
            InsertRow(row, col);
          } // r
    }
  } // ni
  
}

std::ostream &operator <<(std::ostream &os, const IWeights::Entry &ent);
   
/*
 * Hold the necessary state to perform an interpolation (either parallel
 * or serial).  The interpolated fields must have a certain uniform nature.
 * In particular, they must all be defined on the same part of the meshes,
 * and the destination fields must all be either nodal or interpolatory,
 * but not mixed.
 * Note: if INTERP_PATCH is used, it is up to the caller to make sure they
 * have ghosting enabled on the mesh.  It will also be their responsibility
 * to make sure the data is halo'ed to the ghosts before the Interp() operation.
 */ 

class Interp {
public:

  enum {INTERP_STD = 0, INTERP_PATCH};
  
  struct FieldPair {
  FieldPair(MEField<> *_sF, MEField<> *_dF, UChar _idata=INTERP_STD, UChar _patch_order=2) :
    first(_sF), second(_dF), idata(_idata), patch_order(_patch_order) {
  }
  MEField<> *first, *second;
  UChar idata; // interpolation specification.
  UChar patch_order;
  };
  
  /* 
   * Build the interpolation object.  The MEFields must be compatible in the
   * sense that they are all element based, or node based, etc...
   */
  Interp(Mesh &src, Mesh &dest, const std::vector<FieldPair> &Fields);
  
  ~Interp();
  
  // Actually process the interpolation
  void operator()();
  
  // Form a matrix of the interpolant for the fpair_num field
  void operator()(int fpair_num, IWeights &iw);
  
  private:
  
  void transfer_serial();
  
  void transfer_parallel();
  
  void mat_transfer_serial(int fpair_num, IWeights &);
  
  void mat_transfer_parallel(int fpair_num, IWeights &);
  
  SearchResult sres;
  GeomRend grend;
  std::vector<FieldPair> fpairs;
  bool is_parallel;
  std::vector<MEField<>*> srcF;
  std::vector<MEField<>*> dstF;
  std::vector<_field*> dstf; // interp fields
  std::vector<int> iflag;
  bool has_std; // true if a standard interpolation exists
  bool has_patch; // true if a patch interp exists
  Mesh &srcmesh;
  Mesh &dstmesh;
};


  
} // namespace

#endif /*ESMC_INTERP_H_*/
