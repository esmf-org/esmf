// $Id: ESMC_MeshDB.h,v 1.3.2.2 2009/01/21 21:25:22 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.


// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_MeshDB_h
#define ESMC_MeshDB_h

#include <ESMC_MeshObj.h>
#include <ESMC_Context.h>
#include <ESMC_MeshContext.h>
#include <ESMC_Attr.h>
#include <ESMC_Kernel.h>
#include <ESMC_MeshObjTopo.h>
#include <ESMC_Tree.h>

#include <map>
#include <set>
#include <functional>
#include <iostream>

namespace ESMCI {
namespace MESH {

class MEFieldBase;

/*********** Iterators *********/
/**
 * Iterator for mesh objects within a mesh.  Can use selected attributes
 * to loop certain parts of the mesh.
 */
template <class _T, typename Ref, typename Ptr, typename MSet_iterator, typename obj_iterator>
class mesh_obj_iterator {
public:
  typedef std::forward_iterator_tag iterator_category;
  typedef std::ptrdiff_t difference_type;
  typedef Ptr pointer;
  typedef Ref reference;
  typedef _T value_type;

  typedef mesh_obj_iterator<_T, _T&, _T*, typename MSet_iterator::iterator,
                 typename obj_iterator::iterator> iterator;
  typedef mesh_obj_iterator<_T, const _T&, const _T*,
        typename MSet_iterator::const_iterator, typename obj_iterator::const_iterator> const_iterator;
  typedef mesh_obj_iterator<_T, Ref, Ptr, MSet_iterator, obj_iterator> self;

  template<typename A, typename B, typename C, typename D, typename E> friend class mesh_obj_iterator;


// By default, only iterate the active objects.
mesh_obj_iterator() : attr() {}

mesh_obj_iterator(const MSet_iterator &_cur,
         const MSet_iterator &_end_set, const Attr &_attr = Attr()) :
cur(_cur),
end_set(_end_set),
cur_obj(_cur == _end_set ? 
  obj_iterator() :
  _cur->objects.begin()),
attr(_attr)
{
  // This object may not be valid, so advance to first good object.
  next_good_object();
}

mesh_obj_iterator(const iterator &rhs) :
cur(rhs.cur),
end_set(rhs.end_set),
cur_obj(rhs.cur_obj),
attr(rhs.attr)
{
}

mesh_obj_iterator &operator=(const mesh_obj_iterator &rhs) {
  if (this == &rhs) return *this;
  cur     = rhs.cur;
  cur_obj = rhs.cur_obj;
  end_set = rhs.end_set;
  attr    = rhs.attr;
  return *this;
}

bool operator==(const mesh_obj_iterator &rhs) const {
  // If at end, don't look at the object (invalid)
  if (cur == end_set || rhs.cur == end_set)
    return cur == rhs.cur;

  // both are valid
  return (cur_obj == rhs.cur_obj);
}

bool operator!=(const mesh_obj_iterator &rhs) const {
  // dont need to check cur, since an object
  // is only kept in one (and only one) KernelList
  return (!(*this == rhs));
}

void next_good_object() {

  // If at end set, fine.
  // If we point to objects end or if we are not a subset
  while (cur != end_set &&
           (cur_obj == cur->objects.end() || !attr.subset(cur->GetAttr()))) {
    cur++;
    if (cur != end_set) cur_obj = cur->objects.begin();
  }
}

self &operator++() {
  // Advance to either the next good object
  // or end.
  if (cur == end_set) return *this;
  
  if (cur_obj != cur->objects.end())
    cur_obj++;
  next_good_object();
  return *this;
}

self operator++(int) {
  mesh_obj_iterator tmp = *this;
  this->operator++();
  return tmp;
}

reference operator*() const {
  return *cur_obj;
}

pointer operator->() const {
  return cur_obj.operator->();
}

MSet_iterator mesh_set() const {
  return cur;
}

private:
MSet_iterator cur;
MSet_iterator end_set;
obj_iterator cur_obj;
Attr attr;
};


class Mesh;

/**
 * The Mesh database.  Stores all mesh objects, and orchestrates basic object creation,
 * insertion into the mesh, etc...
 */
class MeshDB {
public:

friend void WriteExMesh(const Mesh &mesh, const std::string &filename,int,double);
friend void WriteExMeshTimeStep(int,double,const Mesh&,const std::string&);
friend void LoadExMesh(Mesh &mesh, const std::string &filename,int);
MeshDB();
virtual ~MeshDB();

/**
 * Forces all sides and edges to be present in the mesh.  Must be called
 * prior to commit 
 */
void UseSides(bool val);

typedef mesh_obj_iterator<MeshObj,MeshObj&,MeshObj*,KernelList::iterator,List<MeshObj>::iterator> iterator;
typedef mesh_obj_iterator<MeshObj,const MeshObj&,const MeshObj*,
          KernelList::const_iterator, List<MeshObj>::const_iterator> const_iterator;

// Update the attribute for a given object.
void update_obj(MeshObj*obj, const Attr &attr);

// Set a new id
void update_obj(MeshObj::id_type id, MeshObj*obj, const Attr &attr);

// Add objects with global id's
void add_node(MeshObj*node, UInt nodeSet);
void add_node(MeshObj*node, const Attr &attr);

// Add a node given a supporting topology and its nodes
void add_node_local(MeshObj *node,
                    const MeshObjTopo *topo, // supporting topo
                    UInt ordinal,    // ordinal within supporting topo
                    std::vector<MeshObj*> &support,
                    const Attr &attr);

void add_element(MeshObj*elem, std::vector<MeshObj*> &nodevect, UInt block,
          const MeshObjTopo *);
void add_element(MeshObj*elem, std::vector<MeshObj*> &nodevect, const Attr &attr,
      const MeshObjTopo *);
void add_side(MeshObj &side, MeshObj &element, int ordinal, UInt sideSet, const MeshObjTopo *,
      bool local=false, const Context &context = Context());
void add_edge(MeshObj &edge, MeshObj &element, int ordinal, UInt edgeSet, const MeshObjTopo *,
      bool local=false, const Context &context = Context());

// Add an object with relations already in place.  We just
// have to add the appropriate back relations.
void add_object(MeshObj *obj, const Attr &attr, const MeshObjTopo *);

// Add objects with local id's (resolve numbering later).
// ctxt = extra context bits to set
void add_side_local(MeshObj &side, MeshObj &element, int ordinal, UInt sideSet, const MeshObjTopo *, const Context &ctxt = Context());

void add_edge_local(MeshObj &edge, MeshObj &element, int ordinal, UInt edgeSet, const MeshObjTopo *, const Context &ctxt = Context());

// Remove nodes that have no relations.
void remove_unused_nodes();

// Connect the element to nodes, vice versa.  nodes is an index into array nodevect that
// describes which nodes to use.

// Iterators through ACTIVE objects
MeshDB::const_iterator elem_begin() const {
  return obj_begin(MeshObj::ELEMENT); }
MeshDB::const_iterator elem_end() const {
  return obj_end(MeshObj::ELEMENT); }

MeshDB::const_iterator node_begin() const {
  return obj_begin(MeshObj::NODE); }
MeshDB::const_iterator node_end() const {
  return obj_end(MeshObj::NODE); }

MeshDB::const_iterator side_begin() const {
  return obj_begin(side_type()); }
MeshDB::const_iterator side_end() const {
  return obj_end(side_type()); }

MeshDB::iterator elem_begin() {
  return obj_begin(MeshObj::ELEMENT); }
MeshDB::iterator elem_end() {
  return obj_end(MeshObj::ELEMENT); }

MeshDB::iterator side_begin() {
  return obj_begin(side_type()); }
MeshDB::iterator side_end() {
  return obj_end(side_type()); }

MeshDB::iterator node_begin() {
  return obj_begin(MeshObj::NODE); }
MeshDB::iterator node_end() {
  return obj_end(MeshObj::NODE); }

// Iterators through ACTIVE and INACTIVE
MeshDB::const_iterator elem_begin_all() const {
  return obj_begin_all(MeshObj::ELEMENT); }
MeshDB::const_iterator elem_end_all() const {
  return obj_end_all(MeshObj::ELEMENT); }

MeshDB::const_iterator node_begin_all() const {
  return obj_begin_all(MeshObj::NODE); }
MeshDB::const_iterator node_end_all() const {
  return obj_end_all(MeshObj::NODE); }

MeshDB::const_iterator side_begin_all() const {
  return obj_begin_all(side_type()); }
MeshDB::const_iterator side_end_all() const {
  return obj_end_all(side_type()); }

MeshDB::iterator elem_begin_all() {
  return obj_begin_all(MeshObj::ELEMENT); }
MeshDB::iterator elem_end_all() {
  return obj_end_all(MeshObj::ELEMENT); }

MeshDB::iterator side_begin_all() {
  return obj_begin_all(side_type()); }
MeshDB::iterator side_end_all() {
  return obj_end_all(side_type()); }

MeshDB::iterator node_begin_all() {
  return obj_begin_all(MeshObj::NODE); }
MeshDB::iterator node_end_all() {
  return obj_end_all(MeshObj::NODE); }

// Sets up numbering tables for data indexing
void Commit(UInt nFields, MEFieldBase **Fields, UInt nfields, _field **fields);

// Set linear data index 
void linearize_data_index();

void Print(std::ostream & os, bool summary_only = false) const;

void set_spatial_dimension(int dim) {spatial_dimension = dim;}
void set_parametric_dimension(int dim) {parametric_dimension = dim;}
int spatial_dim() const { return spatial_dimension;}
int parametric_dim() const { return parametric_dimension;}
UInt num_nodes() const { return num_objs(MeshObj::NODE);}
UInt num_elems() const { return num_objs(MeshObj::ELEMENT);}
UInt num_elems(UInt blk) const;
UInt num_sides(UInt blk) const;
UInt num_nodes(UInt blk) const;
int num_blocks() const { return num_globalKeys(MeshObj::ELEMENT); }
int num_side_sets() const { return num_globalKeys(side_type()); }
int num_node_sets() const { return num_globalKeys(MeshObj::NODE); }
const std::string& filename() const { return fname;}
void set_filename(const std::string &f) {fname = f;}

// Id map
//typedef std::map<MeshObj::id_type, const MeshObj*> MeshObjIDMap;
typedef Tree<MeshObj::id_type,MeshObj> MeshObjIDMap;

MeshObjIDMap &get_map(UInt);
const MeshObjIDMap &get_map(UInt) const;

MeshObjIDMap::const_iterator map_begin(UInt) const;
MeshObjIDMap::const_iterator map_end(UInt) const;
MeshObjIDMap::const_iterator map_find(UInt, MeshObj::id_type) const;
MeshObjIDMap::iterator map_find(UInt, MeshObj::id_type);

// Get the minimum and maximum ids for an object type
MeshObj::id_type get_min_id(UInt obj_type) const;
MeshObj::id_type get_max_id(UInt obj_type) const;

// Get a local id for the obj.  id will be negative.  
MeshObj::id_type get_new_local_id(UInt obj_type) const;

UInt DefineContext(const std::string &cname);
UInt GetContext(const std::string &cname);

MeshObj::MeshObjType side_type() const { return parametric_dim() == 3 ? MeshObj::FACE : MeshObj::EDGE;}

// Find rosters with the local numbering bit set and
// find global numbers for them.  The default (serial) implementation
// just removes the local bit from the context.
void ResolvePendingDelete(int obj_type = MeshObj::ANY);
void ResolvePendingCreate();


// Is the mesh parallel?  Obviously the parallel mesh
// implements this and returns true;
virtual bool IsParallel() const { return false;}


UInt num_objs(UInt obj_type) const;
UInt num_globalKeys(UInt obj_type) const;

void remove_unused_kernels();


// Iterators through active and inactive objs.
MeshDB::iterator obj_begin_all(const Attr &attr = Attr(MeshObj::ANY, Context())) {
  return iterator(setRoster.begin(),
                  setRoster.end(), attr);
}
MeshDB::const_iterator obj_begin_all(const Attr &attr = Attr(MeshObj::ANY, Context())) const {
  return const_iterator(setRoster.begin(),
                        setRoster.end(), attr);
}

MeshDB::iterator obj_end_all(const Attr &attr = Attr(MeshObj::ANY, Context())) {
  KernelList::iterator me = setRoster.end();
  return iterator(me, me, attr);
}

MeshDB::const_iterator obj_end_all(const Attr &attr = Attr(MeshObj::ANY, Context())) const {
  KernelList::const_iterator me = setRoster.end();
  return const_iterator(me, me, attr);
}

// These default iterators are through ACTIVE objects
MeshDB::iterator obj_begin(const Attr &attr = Attr()) {
  return iterator(setRoster.begin(),
                  setRoster.end(), attr);
}
MeshDB::const_iterator obj_begin(const Attr &attr = Attr()) const {
  return const_iterator(setRoster.begin(),
                        setRoster.end(), attr);
}

MeshDB::iterator obj_end(const Attr &attr = Attr()) {
  KernelList::iterator me = setRoster.end();
  return iterator(me, me, attr);
}

MeshDB::const_iterator obj_end(const Attr &attr = Attr()) const {
  KernelList::const_iterator me = setRoster.end();
  return const_iterator(me, me, attr);
}

MeshDB::const_iterator obj_begin(UInt obj_type) const;
MeshDB::iterator obj_begin(UInt obj_type);
MeshDB::const_iterator obj_end(UInt obj_type) const;
MeshDB::iterator obj_end(UInt obj_type);

// Same as above, but for INACTIVE and ACTIVE
MeshDB::const_iterator obj_begin_all(UInt obj_type) const;
MeshDB::iterator obj_begin_all(UInt obj_type);
MeshDB::const_iterator obj_end_all(UInt obj_type) const;
MeshDB::iterator obj_end_all(UInt obj_type);

KernelList::iterator set_begin() {
  return setRoster.begin();
}
KernelList::const_iterator set_begin() const {
  return setRoster.begin();
}
KernelList::iterator set_end() {
  return setRoster.end();
}
KernelList::const_iterator set_end() const {
  return setRoster.end();
}

// Set up the contexts of this map to be identical to the rhs contextmap.
// This is used, for instance, when creating a mirror image of this mesh.
// Shipping objects to the new mesh, they can reuse their context's if the
// context map of the two meshes is the same.
void AssumeContexts(const MeshDB &rhs);

bool AllSides() const { return use_sides; }

protected:

protected:
MeshDB(const MeshDB &rhs);
MeshObj &operator=(const MeshDB &rhs);

// Create faces/edges locally
void CreateAllSides();

// Push MeshObj onto roster.  Push into correct meshset, update meshset.  Gets set
// by querying get_int(key).
void push_back_sorted(MeshObj &obj, const Attr &attr, const MeshObjTopo*);
void element_connect(MeshObj &element, std::vector<MeshObj*> &nodevect);

//MeshObjList &side_roster() { return parametric_dim() == 3 ? faces : edges;}
// Context Map
typedef std::map<std::string, UInt> ContextMapType;

// Map of contexts
ContextMapType contexts;

// Special context bits reserved by mesh.
UInt curContext;  // The next free context bit

KernelList setRoster;


MeshObjIDMap node_map;
MeshObjIDMap element_map;
MeshObjIDMap edge_map;
MeshObjIDMap face_map;

int spatial_dimension;
int parametric_dimension;
int max_mapping_data;
std::string fname;
bool committed;
bool use_sides; // includes edges
std::vector<MEFieldBase*> Fields;
std::vector<_field*> fields;
};


} // namespace DVD
} // namespace DVD

#endif
