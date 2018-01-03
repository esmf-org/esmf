// $Id$
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_MeshDB_h
#define ESMCI_MeshDB_h

#include <Mesh/include/ESMCI_MeshObj.h>
#include <Mesh/include/ESMCI_Context.h>
#include <Mesh/include/ESMCI_MeshContext.h>
#include <Mesh/include/ESMCI_Attr.h>
#include <Mesh/include/ESMCI_Kernel.h>
#include <Mesh/include/ESMCI_MeshObjTopo.h>
#include <Mesh/include/ESMCI_Tree.h>

#include <map>
#include <set>
#include <functional>
#include <iostream>

/**
 * @defgroup meshdatabase
 * 
 * This module contains the basic routines to implement a topological,
 * relations database of mesh objects, with access methods that enable
 * quick, efficient finite element/volume computations.
 * 
 * @ingroup mesh
 */

namespace ESMCI {

class MEFieldBase;

/**
 * Iterators for mesh objects, based on their relation database keys, the Attr.
 * @ingroup meshdatabase
 */
template <class TT, typename Ref, typename Ptr, typename MSet_iterator, typename obj_iterator>
class mesh_obj_iterator {
public:
  typedef std::forward_iterator_tag iterator_category;
  typedef std::ptrdiff_t difference_type;
  typedef Ptr pointer;
  typedef Ref reference;
  typedef TT value_type;

  typedef mesh_obj_iterator<TT, TT&, TT*, typename MSet_iterator::iterator,
                 typename obj_iterator::iterator> iterator;
  typedef mesh_obj_iterator<TT, const TT&, const TT*,
        typename MSet_iterator::const_iterator, typename obj_iterator::const_iterator> const_iterator;
  typedef mesh_obj_iterator<TT, Ref, Ptr, MSet_iterator, obj_iterator> self;

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
 * @ingroup meshdatabase
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
 * prior to commit.
 * In 3D, this does NOT active edges!  See UseEdges, below. 
 */
void UseSides(bool val);
bool AllSides() const { return use_sides; }

/*
 * In 3d, we must active edges separately from sides.
 */
void UseEdges(bool val);
bool AllEdges() const { return use_edges; }

/*
 * Indicate the mesh has been skinned.
 */
void SetSkinned() { skinned = true; }

/*
 * The field storage may become fragmented upon mesh modification.
 * This routine loops kernels and compacts the data.  The next routine
 * gives a report of the storage ratios.
 */
void CompactData();
void DataStoreInfo(std::ostream &);

/*
 * Basic iterator definitions for looping mesh objects stored in
 * this mesh database.
 */
typedef mesh_obj_iterator<MeshObj,MeshObj&,MeshObj*,KernelList::iterator,List<MeshObj>::iterator> iterator;
typedef mesh_obj_iterator<MeshObj,const MeshObj&,const MeshObj*,
          KernelList::const_iterator, List<MeshObj>::const_iterator> const_iterator;

          
/*
 * A whole suite of routines for adding mesh objects to the database
 * and for updating the attributes of mesh objects.
 */
          
/*
 * Update (change) the attribute for a given object.
 * This operation can be expensive, because it requires a copy
 * of all of the field values for an object into the new kernel
 * (if they support the fields).
 * This function can be called prior to commit, in which case it
 * is very fast (just a few link pointers are switched).
 */
void update_obj(MeshObj*obj, const Attr &attr);

/*
 * Same as above, but reset the id as well.
 */
void update_obj(MeshObj::id_type id, MeshObj*obj, const Attr &attr);

/*
 * Add a globaly consistent node to the mesh, either with default
 * attrs, or user specified.
 */
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

// Remove nodes that have no relations.  if node_del = true, then
// actually take out of mesh.  Otherwise they are just put on pending delete.
void remove_unused_nodes(bool node_del=true);

// Connect the element to nodes, vice versa.  nodes is an index into array nodevect that
// describes which nodes to use.

/*
 * A suite return various types of iterators.
 */

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


/*
 * Iterators through more generic sets of objects.
 */
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

/*
 * Iterator through the mesh kernels.
 */
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


/*
 * Sets up storage for the mesh fields.  All fields must be registered 
 * prior to calling this function.  After this call, updating attr's
 * of mesh objects incurs the cost of a field move.
 */
void Commit(UInt nFields, MEFieldBase **Fields, UInt nfields, _field **fields);

/*
 * The data_index is used by bootsrap fields prior to commit.  This
 * sequentializes the index.
 */ 
void linearize_data_index();

/*
 * Print the mesh database to an ostream.
 */
void Print(std::ostream & os, bool summary_only = false) const;

/*
 * Define the spatial dimension of the mesh.
 */
void set_spatial_dimension(int dim) {spatial_dimension = dim;}

/*
 * Define the topological dimension of the mesh.
 */
void set_parametric_dimension(int dim) {parametric_dimension = dim;}

int spatial_dim() const { return spatial_dimension;}
int parametric_dim() const { return parametric_dimension;}

/*
 * Return various ordinality information, all for ACTIVE
 * objects only.
 */
UInt num_nodes() const { return num_objs(MeshObj::NODE);}
UInt num_elems() const { return num_objs(MeshObj::ELEMENT);}

/*
 * Ordinatlity information for objects in certain exodus-type
 * blocks.
 */
UInt num_elems(UInt blk) const;
UInt num_sides(UInt blk) const;
UInt num_nodes(UInt blk) const;
int num_blocks() const { return num_globalKeys(MeshObj::ELEMENT); }
int num_side_sets() const { return num_globalKeys(side_type()); }
int num_node_sets() const { return num_globalKeys(MeshObj::NODE); }

const std::string& filename() const { return fname;}
void set_filename(const std::string &f) {fname = f;}

/*
 * Objects are store in a global map for log n access.
 * Each object type is in its own map.
 */
typedef Tree<MeshObj::id_type,MeshObj> MeshObjIDMap;

MeshObjIDMap &get_map(UInt);
const MeshObjIDMap &get_map(UInt) const;

MeshObjIDMap::const_iterator map_begin(UInt) const;
MeshObjIDMap::const_iterator map_end(UInt) const;
MeshObjIDMap::const_iterator map_find(UInt, MeshObj::id_type) const;
MeshObjIDMap::iterator map_find(UInt, MeshObj::id_type);

/*
 * Get min/max is's used for the current (and local) mesh.
 */
MeshObj::id_type get_min_id(UInt obj_type) const;
MeshObj::id_type get_max_id(UInt obj_type) const;

/*
 * Attain an id that is locally unique.  The id will be negative,
 * which specifies that it must be resolved globally.
 */  
MeshObj::id_type get_new_local_id(UInt obj_type) const;

/*
 * Create a mesh context.  This context may be used to partion
 * the mesh.
 */
UInt DefineContext(const std::string &cname);

/*
 * Retrieve a context previously defined.
 */
UInt GetContext(const std::string &cname);

/*
 * Return EDGE for parametic dim==2, FACE for pdim == 3
 */
MeshObj::MeshObjType side_type() const { return parametric_dim() == 3 ? MeshObj::FACE : MeshObj::EDGE;}

/*
 * Remove pending delete objects from the mesh database.
 * Before this function is called, the delete must be globally
 * consistent, i.e. do not delete an object that is required to be
 * ghosted.
 */
void ResolvePendingDelete(int obj_type = MeshObj::ANY);

/*
 * Remove the pending create bit.
 */
void ResolvePendingCreate();


// Is the mesh parallel?  Obviously the parallel mesh
// implements this and returns true;
virtual bool IsParallel() const { return false;}


UInt num_objs(UInt obj_type) const;
UInt num_globalKeys(UInt obj_type) const;

/*
 * After delete's, it is possible that a kernel may contain
 * no objects.  This removes those kernels.
 */
void remove_unused_kernels();


/*
 *  Set up the contexts of this map to be identical to the rhs contextmap.
 * This is used, for instance, when creating a mirror image of this mesh.
 * Shipping objects to the new mesh, they can reuse their context's if the
 * context map of the two meshes is the same.
 */
void AssumeContexts(const MeshDB &rhs);


protected:

protected:
MeshDB(const MeshDB &rhs);
MeshObj &operator=(const MeshDB &rhs);

/*
 *  Create faces/edges locally.
 */
void CreateAllSides();
void CreateAllEdges();

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
bool use_sides;
bool use_edges;
bool skinned;
std::vector<MEFieldBase*> Fields;
std::vector<_field*> fields;
};


} // namespace ESMCI

#endif
