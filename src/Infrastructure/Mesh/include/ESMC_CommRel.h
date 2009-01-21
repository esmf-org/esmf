// $Id: ESMC_CommRel.h,v 1.3.2.2 2009/01/21 21:25:22 cdeluca Exp $
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

#ifndef ESMC_CommRel
#define ESMC_CommRel

#include <vector>
#include <iostream>

#include <ESMC_MeshDB.h>
#include <ESMC_Meshfield.h>
#include <ESMC_SparseMsg.h>

namespace ESMCI {
namespace MESH {

/**
 * A class to describe the communication path
 * Each processor has the (local) objects that are involved in the comm relation.
 * The range is sorted by processor number, so one is guarenteed that looping the
 * range processors will be encountered in blocks.  Domain is not guarenteed to be
 * sorted.
*/
class CommRel {
public:

// Store a particular parallel connection
struct CommNode {
  CommNode(std::pair<MeshObj*, UInt> &val) : obj(val.first), processor(val.second) {}
  CommNode(MeshObj* _obj, UInt _proc) : obj(_obj), processor(_proc) {}
  
  // Sort as obj <, proc
  bool operator< (const CommNode &other) const {
    return obj != other.obj ?
       obj->get_id() < other.obj->get_id() 
     : processor < other.processor;
  }

  bool operator==(const CommNode &other) const {
    if (obj != other.obj) return false;
    if (processor != other.processor) return false;
    return true;
  }

  bool operator!=(const CommNode &rhs) const {
    return !(*this == rhs);
  }

  MeshObj *obj;
  UInt processor;
  friend std::ostream &operator<<(std::ostream &os, const CommNode &cn);
};

typedef std::vector<CommNode> MapType;

// Sort commrel's by proc only
class range_sort : public std::binary_function<CommRel::CommNode,CommRel::CommNode,bool> {
public:
  range_sort() {}
  bool operator()(const CommRel::CommNode &l, const CommRel::CommNode &r) {
    return l.processor < r.processor;
  }
};

CommRel();

void Init(const std::string &name, MeshDB &dom, MeshDB &ran, bool sym);

// Create an empty map with dom, range = dom
CommRel(const std::string &name, MeshDB &dom, bool _sym = false);

// Nonsymmetric (different range) map.  Range is not const, since
// We will likely construct it.
CommRel(const std::string &name, MeshDB &dom, MeshDB &ran);

// Pack and send the field values.  Field is from domain mesh.  A field
// of the same name and size must exist on the other end.  If multiple copies
// arrive, this function copies over.  sfields[i] -> rfields[i]
void send_fields(UInt nfields, _field *const *sfield, _field *const *rfields);

// Copy fields from owner to ghosted copies.  Assumes sym spec
void halo_fields(UInt nfields, _field **sfield) const;

enum {OP_SUM=0, OP_MIN=1, OP_MAX=2};

template <typename VTYPE, typename FTYPE>
void swap_op(UInt nfields, FTYPE **sfield, int op) const;

// Create a symmetric shared commspec from a one sided commspec (Each shared
// object knows the proeces that owns it only).  To create, send in the list
// of comm nodes for the local domain.
// In the end, each local node will be connected to the same node on every processor
// that has a copy.
CommRel(const std::string &name, MeshDB &dom, const std::vector<CommNode> &obj);

// Build up a sym spec from locally declaring objects whose owner
// is not this proc.
void BuildFromOwner(MeshDB &dom, const std::vector<CommNode> &obj);

// Create a non symmetric map with domain/range mesh
//CommRel(const MeshDB &dom, const MeshDB &ran);

~CommRel();

// Add items to domain (obj, proc)
void add_domain(const std::vector<CommNode> &obj);

// Take items out of domain.  Objects should be unique.
void remove_domain(std::vector<MeshObj*> &robjs);

void domain_insert(MapType::iterator lb, CommNode &cnode);

// Add the comm rhs to this.  For now, requires symmetric comm.
void Append(const CommRel &rhs);

// Sort the domain objects.  Should be done, for instance, with symmetric
// comms, since this will guarantee the objects line up on each processor.
// If comm is not symmetric, this should only be done before building the
// range.  TODO: if range exists, sort it as well.
void sort_domain();

// Add items to range
void add_range(const std::vector<MeshObj*> &obj);

MeshDB *DomainMesh() const { return domMesh; }
const MeshDB *RangeMesh() const { return ranMesh; }

// Send the domain id's over to the range.  We assume these objects exist on those
// processors, and we complete the range by adding these guys in the range.
void complete_range();

// Create the range mesh objects by marshalling the domain objects
// over to the (different) range mesh.  This should proceed by first sending
// a nodal spec, then by sending an element spec.
void build_range(bool ghosting = false);

// Delete the range of the spec (only if objects not used or children of others)
void delete_range();

// For an element (or side) domain CommRel (range need not be built yet), build the
// domain side object for the dependents, i.e. nodes.  Names this the same as
// parent, but adds "_dep".
CommRel &dependants(CommRel &comm, UInt obj_type);

// Create the parents of some nodes
CommRel &ancestors(CommRel &comm);

// Transpose the communication spec.  Simply interchanges the domain and range,
// then sorts the range by processor (for easier unpacking, maintaining the conformal
// sorting within the processors
void transpose();

// Commit range indices.  Rid should line up directly with the local domain
// objects.  The indices are forwarded to the range, they are looked up in
// the mesh, and then inserted into the range.  If necessary, they will be
// Sorted by processor (keeping the within-processor ordering the same).
void commit_range(const std::vector<MeshObj::id_type> &rid);

// Verify that a 'so-stated' symmetric comm is just that by swapping
// and checking id's
bool verify_symmetric_comm();

MapType::iterator domain_begin() {return domain.begin();}
MapType::iterator domain_end() {return domain.end();}
MapType::const_iterator domain_begin() const {return domain.begin();}
MapType::const_iterator domain_end() const {return domain.end();}

MapType::iterator range_begin() { return range.begin();}
MapType::iterator range_end() {return range.end();}
MapType::const_iterator range_begin() const {return range.begin();}
MapType::const_iterator range_end() const {return range.end();}

// This is the list of processors I intend to send to
std::vector<UInt>::iterator domain_processors_begin() { return domain_processors.begin();}
std::vector<UInt>::iterator domain_processors_end() { return domain_processors.end();}
std::vector<UInt>::const_iterator domain_processors_begin() const { return domain_processors.begin();}
std::vector<UInt>::const_iterator domain_processors_end() const { return domain_processors.end();}

std::vector<UInt>::iterator range_processors_begin() { return range_processors.begin();}
std::vector<UInt>::iterator range_processors_end() { return range_processors.end();}
std::vector<UInt>::const_iterator range_processors_begin() const { return range_processors.begin();}
std::vector<UInt>::const_iterator range_processors_end() const { return range_processors.end();}

void Print(std::ostream &) const;

// Is a symmetric spec sane globally, i.e. is there
// one and only one owner?
bool is_sym_sane() const;

void clear();
private:

// sort and unique to get range procs
void build_range_procs();
// sort and unique to get domain procs
void build_domain_procs();
MapType domain;
MapType range;

std::vector<UInt> domain_processors;
std::vector<UInt> range_processors;

bool symmetric;
MeshDB *domMesh;
MeshDB *ranMesh;
std::string comm_name;
bool transposed;
}; // CommRel

std::ostream &operator<<(std::ostream &os, const CommRel::CommNode &cn);

// Provide this function as a template so that different field types may
// be used.
// A swap helper
template<typename T>
struct SwapIt {
  static T value(T a, T b, int op) {
    switch(op) {
      case CommRel::OP_SUM:
        return a + b;
      case CommRel::OP_MIN:
        return std::min(a,b);
      case CommRel::OP_MAX:
        return std::max(a,b);
      default:
        Throw() << "Unknown swap type:" << op;
    }
  }
};
template<typename VTYPE, typename FTYPE>
void CommRel::swap_op(UInt nfields, FTYPE **sfields, int op) const {
  if (!symmetric)
    throw Ex() << "halo is only implemented for symmetric spec, spec=" << comm_name;
  SparseMsg msg;
  UInt ndproc = domain_processors.size();
  UInt csize = msg.commSize();

  // Use same fields for range
  FTYPE **rfields = sfields;
  
  if (ndproc > 0) {
    msg.setPattern(ndproc, &domain_processors[0]);
  } else {
    msg.setPattern(ndproc, NULL);
  }

  // Sizes.  For each domain object, we send the id_type and object type
  MapType::const_iterator ci = domain_begin(), ce = domain_end();
  if (ndproc > 0) {
    std::vector<UInt> send_size_all(csize, 0); // need vector proc can look up into (Map?)
    std::vector<UInt> sizes(ndproc, 0);
  
    for (; ci != ce; ++ci) {
      UInt proc = ci->processor;
      // Loop fields
      for (UInt f = 0; f < nfields; f++) {
        if (sfields[f]->OnObj(*ci->obj))
          send_size_all[proc] += SparsePack<VTYPE>().size()*sfields[f]->dim();
      }
    }
  
    for (UInt i = 0 ; i < ndproc; i++) {
      sizes[i] = send_size_all[domain_processors[i]];
    }
  
    // Communicate the sizes
    msg.setSizes(&sizes[0]);
  } else
    msg.setSizes(NULL);

  // Fill buffers
  ci = domain_begin();
  for (; ci != ce; ++ci) {
    UInt proc = ci->processor;
    SparseMsg::buffer &b = *msg.getSendBuffer(proc);
    const MeshObj &obj = *ci->obj;

    for (UInt f = 0; f < nfields; f++) {
      if (sfields[f]->OnObj(obj)) {
        VTYPE *data = sfields[f]->data(obj);
        for (UInt d = 0; d < sfields[f]->dim(); d++) {
          SparsePack<VTYPE>(b, data[d]);
        }
      }
    }
  }
  if (!msg.filled()) throw("complete range, send buffer not filled");


  // Send messages
  msg.communicate();

  MapType::const_iterator ri = domain_begin(); // assuming symmetric.
                                      // also assuming sorted by range proc.

  for (; ri != domain_end(); ri++) {
    UInt proc = ri->processor;
    SparseMsg::buffer &b = *msg.getRecvBuffer(proc);
    const CommNode &cn = *ri;
    const MeshObj &obj = *cn.obj;
      
    // Coming from the owner, so unpack
    for (UInt f = 0; f < nfields; f++) {
      if (rfields[f]->OnObj(obj)) {
        VTYPE *data = rfields[f]->data(obj);
        for (UInt d = 0; d < rfields[f]->dim(); d++) {
          VTYPE val;
          SparseUnpack<VTYPE>(b, val);
          data[d] = SwapIt<VTYPE>::value(data[d], val, op); 
        }
      }
    }

  }
  if (!msg.empty()) throw("halo, CommRel, didn't use up buffer!");
}

} // namespace 
} // namespace 

#endif
