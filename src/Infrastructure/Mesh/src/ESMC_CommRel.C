// $Id: ESMC_CommRel.C,v 1.3.2.2 2009/01/21 21:25:22 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <ESMC_CommRel.h>
#include <ESMC_SparseMsg.h>
#include <ESMC_MeshObjTopo.h>
#include <ESMC_MeshField.h>
#include <ESMC_Kernel.h>
#include <ESMC_MeshObjPack.h>
#include <ESMC_ParEnv.h>
#include <ESMC_MeshObjConn.h>

#include <iostream>
#include <iterator>

#include <limits>
#include <algorithm>
#include <mpi.h>

namespace ESMCI {
namespace MESH {


CommRel::CommRel() :
domain(),
range(),
symmetric(false),
domMesh(NULL),
ranMesh(NULL),
comm_name(),
transposed(false)
{
}

void CommRel::Init(const std::string &name, MeshDB &dom, MeshDB &ran, bool sym) {
  comm_name = name;
  domMesh = &dom;
  ranMesh = &ran;
  symmetric = sym;
}

CommRel::CommRel(const std::string &name, MeshDB  &dom, bool _sym) :
 domain(),
 range(),
 symmetric(_sym),
 domMesh(&dom),
 ranMesh(&dom),
 comm_name(name),
 transposed(false)
{
}

CommRel::CommRel(const std::string &name, MeshDB  &dom, MeshDB &ran) :
 domain(),
 range(),
 symmetric(false),
 domMesh(&dom),
 ranMesh(&ran),
 comm_name(name),
 transposed(false)
{
}

CommRel::CommRel(const std::string &name, MeshDB  &dom, const std::vector<CommNode> &obj) :
 domain(),
 range(),
 symmetric(true),
 domMesh(&dom),
 ranMesh(&dom),
 comm_name(name),
 transposed(false)
{
  BuildFromOwner(*domMesh, obj);
}

void CommRel::BuildFromOwner(MeshDB  &dom, const std::vector<CommNode> &obj) 
{
  Trace __trace("CommRel::BuildFromOwner(MeshDB  &dom, const std::vector<CommNode> &obj)");

  // Wipe out any old state.
  symmetric = true;
  MapType().swap(domain);
  MapType().swap(range);
  std::vector<UInt>().swap(domain_processors);
  std::vector<UInt>().swap(range_processors);
  transposed = false;

  domMesh = &dom; ranMesh = &dom;

  // Okay.  Create a temporary spec
  CommRel bootStrap("__bootstrap_sym", dom, false);

  bootStrap.add_domain(obj);
  bootStrap.complete_range();
  bootStrap.transpose();


  // Okay.  For each domain processor, get the list of procs that sent to it
  typedef std::map<MeshObj::id_type, std::vector<UInt> > CountMapType;
  CountMapType counts;

  MapType::iterator di = bootStrap.domain_begin(), de = bootStrap.domain_end();
  for (; di != de; ++di) {
    MeshObj::id_type id = di->obj->get_id();
//std::cout << "id=" << id << std::endl;
    std::pair<CountMapType::iterator,bool> ci =
      counts.insert(std::pair<MeshObj::id_type, std::vector<UInt> >(id, std::vector<UInt>(1, di->processor)));
    if (ci.second == false) { // already there
//std::cout << "found:" << ci.first->first << std::endl;
      std::vector<UInt> &pr = ci.first->second;
      pr.push_back(di->processor);
    }
//std::cout << "ci=" << ci.first->first << std::endl;
  }

  // Send all the affliate processors back to the original senders
  SparseMsg msg;

  UInt ndproc = bootStrap.domain_processors.size();
  UInt csize = msg.commSize();
  
  msg.setPattern(ndproc, &bootStrap.domain_processors[0]);

  // Sizes.  For each domain object, we send the id_type and object type
  std::vector<UInt> send_size_all(csize, 0); // need vector proc can look up into (Map?)
  std::vector<UInt> sizes(ndproc, 0);

  MapType::iterator ci = bootStrap.domain_begin(), ce = bootStrap.domain_end();
  for (; ci != ce; ++ci) {
    UInt proc = ci->processor;
    // First the int telling how many for this object
    send_size_all[proc] += SparsePack<UInt>().size();
    const std::vector<UInt> &proclist = counts[ci->obj->get_id()];
    send_size_all[proc] += proclist.size()*SparsePack<UInt>().size();
  }

  for (UInt i = 0 ; i < ndproc; i++) {
    sizes[i] = send_size_all[bootStrap.domain_processors[i]];
  }

  // Communicate the sizes
  msg.setSizes(&sizes[0]);

  // Fill buffers
  ci = bootStrap.domain_begin();
  for (; ci != ce; ++ci) {
    UInt proc = ci->processor;
    SparseMsg::buffer &b = *msg.getSendBuffer(proc);
    const MeshObj &obj = *ci->obj;
//std::cout << "Proc:" << msg.commRank() << ", obj:" << ci->obj->get_id()
 // << "sending procs:";
    const std::vector<UInt> &proclist = counts[obj.get_id()];
    UInt num = proclist.size();
    SparsePack<UInt>(b, num);
    for (UInt i = 0; i < proclist.size(); i++) {
//std::cout << proclist[i] << ", ";
      SparsePack<UInt>(b, proclist[i]);
    }
//std::cout << " to proc" << ci->processor << std::endl;
  }
  if (!msg.filled()) Throw() << "complete range, send buffer not filled";


  // Send messages
  msg.communicate();

//MPI_Barrier(MPI_COMM_WORLD);

/*
  std::cout << "Proc:" << msg.commRank() << ", msg.inproc:";
  for (UInt *p = msg.inProc_begin(); p != msg.inProc_end(); p++) {
    std::cout << *p << ", ";
  }
  std::cout << std::endl;
  std::cout << "P:" << msg.commRank() << "rangeproc:"; std::copy(bootStrap.range_begin(), bootStrap.range_end(), std::ostream_iterator<CommNode>(std::cout, ", "));
  std::cout << std::endl;
MPI_Barrier(MPI_COMM_WORLD);
*/

  MapType new_domain_objs;
  MapType::iterator ri = bootStrap.range_begin();
  // And now unpack the ids.  Since we don't have a range yet, we use buffer to unpack
  for (UInt *p = msg.inProc_begin(); p != msg.inProc_end(); p++) {
    UInt proc = *p;
    SparseMsg::buffer &b = *msg.getRecvBuffer(proc);

    // Loop range items for this proc
    while (proc == ri->processor && ri != bootStrap.range_end() ) {
      if (ri == bootStrap.range_end()) {
        std::cout << "P:" << msg.commRank() << " hit end too quickly" << std::endl;
        throw("CommRel hit end of range prematruely");
      }
      const CommNode &cn = *ri;
      UInt num; // number for this object 
      SparseUnpack<UInt>(b, num);
//std::cout << "P:" << msg.commRank() << " num to unpack:" << num << std::endl;
    
      for (UInt i = 0; i < num; i++) {
        UInt ptmp;
        SparseUnpack<UInt>(b,ptmp);
        if (ptmp != msg.commRank()) { // Already have myself in list
          new_domain_objs.push_back(CommNode(cn.obj, ptmp));
        }
      }
        ri++;
    }
  }
   if (ri != bootStrap.range_end()) 
     throw("unpacking CommRel, didn't hit end of range:");

   if (!msg.empty()) throw("CommRel, didn't use up buffer!");

   // for now, just report result
  // Now to create this guy we add all items, range, domain, new_domain
  domain.clear(); range.clear();
  domain_processors.clear(); range_processors.clear();

  // We are lucky when adding that we don't need to worry about order.  We will simply sort
  // everyone by id later
  std::copy(bootStrap.domain_begin(), bootStrap.domain_end(), std::back_inserter(domain));
  std::copy(bootStrap.range_begin(), bootStrap.range_end(), std::back_inserter(domain));
  std::copy(new_domain_objs.begin(), new_domain_objs.end(), std::back_inserter(domain));

  std::sort(domain.begin(), domain.end());

  // Update the unique domain processors
  build_domain_procs();

  // No range to build

  // Done, thanks bootStrap

}

CommRel::~CommRel()
{
}

void CommRel::add_domain(const std::vector<CommNode> &obj)
{
  Trace __trace("CommRel::add_domain(const std::vector<CommNode> &obj)");
  // Push these guys on end of list
  std::copy(obj.begin(), obj.end(), std::back_inserter(domain));

  // Update the unique domain processors
  build_domain_procs();

}

void CommRel::domain_insert(MapType::iterator lb, CommNode &cnode) {
  
  domain.insert(lb, cnode);
  
}

class robj_sort : public std::binary_function<const MeshObj*, const MeshObj*, bool> {
public:
  robj_sort() {}
  bool operator()(const MeshObj *l, const MeshObj *r) {
    return l->get_id() < r->get_id();
  }
};

class id_sort : public std::binary_function<CommRel::CommNode,MeshObj *,bool> {
public:
  id_sort() {}
  bool operator()(const CommRel::CommNode &l, const MeshObj *r) {
    return l.obj->get_id() < r->get_id();
  }
};

void CommRel::remove_domain(std::vector<MeshObj*> &robjs) {
  Trace __trace("CommRel::remove_domain(std::vector<MeshObj*> &robjs)");

  ThrowRequire(symmetric);

  // Sort objs to delete by id
  std::sort(robjs.begin(), robjs.end(), robj_sort());
 
  MapType::iterator cdel = domain.begin(), cdele = domain.begin(), cdelv = domain.end();
  for (UInt i = 0; i < robjs.size(); i++) {
    MeshObj &dobj = *robjs[i];
    
    // Find the object, starting at self
    cdele = cdel = std::lower_bound(cdel, cdelv, &dobj, id_sort());

    if (cdel == cdelv) break;

    while (cdele != cdelv && cdele->obj == &dobj) ++cdele;

    cdelv = std::copy(cdele, cdelv, cdel); // wipe entries with this object, track new end of list
  }

  domain.erase(cdelv, domain.end());

  build_domain_procs();
}

void CommRel::build_range_procs() {
  Trace __trace("CommRel::build_range_procs()");

  range_processors.clear();
  MapType::iterator di = range.begin(), de = range.end();
  for (; di != de; ++di) {
    range_processors.push_back(di->processor);
  }

  // Remove duplicates
  std::sort(range_processors.begin(), range_processors.end(), std::less<UInt>());
  range_processors.erase(std::unique(range_processors.begin(), range_processors.end()),
                range_processors.end());

  // Also, sort range processors (list does stable sort)
  std::stable_sort(range.begin(), range.end(), range_sort());
}

void CommRel::build_domain_procs() {
  Trace __trace("CommRel::build_domain_procs()");

  domain_processors.clear();
  MapType::iterator di = domain.begin(), de = domain.end();
  for (; di != de; ++di) {
    domain_processors.push_back(di->processor);
  }

  // Remove duplicates
  std::sort(domain_processors.begin(), domain_processors.end(), std::less<UInt>());
  domain_processors.erase(std::unique(domain_processors.begin(), domain_processors.end()),
                domain_processors.end());
}

void CommRel::sort_domain()
{
  std::sort(domain.begin(), domain.end());
}

void CommRel::Append(const CommRel &rhs) {
  Trace __trace("CommRel::Append(const CommRel &rhs)");

  ThrowRequire(symmetric);
  // Loop the comm, inserting objects
  MapType::const_iterator ri = rhs.domain_begin(), re = rhs.domain_end();

  for (;ri != re; ++ri) {
    MapType::iterator lb = std::lower_bound(domain_begin(), domain_end(), *ri);
    if (lb == domain.end() || *lb != *ri)
      domain.insert(lb, *ri);
    // else repeat.
  }

  build_domain_procs();
}

void CommRel::build_range(bool ghosting) {
  Trace __trace("CommRel::build_range(bool ghosting)");

  SparseMsg msg;
  UInt ndproc = domain_processors.size();
  UInt csize = msg.commSize();

  // If we are calling this function we mean
  // to change the range mesh, so it should not be const.
  MeshDB *rmesh = const_cast<MeshDB*>(ranMesh);
 
  // ** Pattern
  ndproc > 0 ? msg.setPattern(ndproc, &domain_processors[0]) :
               msg.setPattern(ndproc, NULL);

  
  // ** Sizes.  For each domain object, we send the id_type and object type
  MapType::iterator ci = domain_begin(), ce = domain_end();
  std::vector<UInt> send_size_all(csize, 0); // need vector proc can look up into (Map?)
  if (ndproc > 0) {
    for (UInt i = 0; i < domain_processors.size(); i++) {
      // Say how many objects to unpack
      send_size_all[domain_processors[i]] += SparsePack<UInt>().size();
    }

    std::vector<UInt> sizes(ndproc, 0);
  
    for (; ci != ce; ++ci) {
      UInt proc = ci->processor;
      MeshObj &obj = *ci->obj;
      
      UInt psize = MeshObjPackSize(obj);
      send_size_all[proc] += psize;
//Par::Out() << "obj:" << obj.get_id() << " to proc:" << proc << " psize:" << psize << std::endl;
        
    }
  
    for (UInt i = 0 ; i < ndproc; i++) {
      sizes[i] = send_size_all[domain_processors[i]];
//Par::Out() << "size =" << sizes[i] << std::endl;
    }
  
    // Communicate the sizes
    msg.setSizes(&sizes[0]);
  } else
    msg.setSizes(NULL);


  // ** Fill buffers
  // First, for each domain proc, put in the number of items to marhsal
  for (UInt i = 0; i < csize; i++) send_size_all[i] = 0;
  ci = domain_begin();
  for (; ci != ce; ++ci) {
    send_size_all[ci->processor]++;
  }
  // Loop buffers, put in size
  for (UInt i = 0; i < domain_processors.size(); i++) {
    SparseMsg::buffer &b = *msg.getSendBuffer(domain_processors[i]);
    SparsePack<UInt>(b, send_size_all[domain_processors[i]]);
/*
std::cout << "P:" << msg.commRank() << " putting in nid=" << send_size_all[domain_processors[i]] << 
  " for proc:" << domain_processors[i] << std::endl;
*/
  }

  // Now the objects themselves
  ci = domain_begin();
  for (; ci != ce; ++ci) {
    UInt proc = ci->processor;
    SparseMsg::buffer &b = *msg.getSendBuffer(proc);
    MeshObj &obj = *ci->obj;
 
//UInt loc = b.loc();
    MeshObjPack(b, obj, ghosting);
//Par::Out() << "obj:" << obj.get_id() << " size packed:" << b.loc() - loc << std::endl;

  }
  if (!msg.filled()) 
       Throw() << "P:" << Par::Rank() << ", build range, send buffer not filled, comm:" << comm_name;

  // ** Marhshal data!!!
  msg.communicate();


  // ** Unapck; And now unpack the ids.  Since we don't have a range yet, we use buffer to unpack
  for (UInt *p = msg.inProc_begin(); p != msg.inProc_end(); p++) {
    UInt proc = *p;
    SparseMsg::buffer &b = *msg.getRecvBuffer(proc);

    // We read the number of objects sent in this message.
    UInt nid;
    SparseUnpack<UInt>(b, nid);

    // Loop id's, get the associated mesh object
    for (UInt i = 0; i < nid; i++) {
      MeshObj *robj;
      MeshObjUnpack(*ranMesh, b, robj);
  
      range.push_back(CommNode(robj, proc));
    }
  }
  if (!msg.empty()) throw("build_range, didn't use up buffer!");

  // Update the unique domain processors
  build_range_procs();

  // Some things for range mesh, now;  See if coord field exists, else create
  // and populate.
  rmesh->set_spatial_dimension(domMesh->spatial_dim());

}

void CommRel::delete_range() {

  UInt obj_type = 0;

  CommRel::MapType::iterator oi = domain_begin(), oe = domain_end();
  
  for (; oi != oe; ++oi) {
    MeshObj &obj = *oi->obj;
    
    obj_type = obj.get_type();

    // Only delete if object not used or child of ELEMENT (think child node hosted on a proc by element)
    bool ok_delete = true;
    if (obj_type != MeshObj::ELEMENT) {
      MeshObjRelationList::iterator ri = obj.Relations.begin(), re = obj.Relations.end();
      
      for (; ok_delete && ri != re; ++ri) {
        if (ri->type == MeshObj::USED_BY || 
          (ri->type == MeshObj::PARENT && ri->obj->get_type() == MeshObj::ELEMENT))
          ok_delete = false;
      }
    }
    
    if (ok_delete) {
      const Attr &oattr = GetAttr(obj);
      const Context &ctxt = GetMeshObjContext(obj);
      Context newctxt(ctxt);
      newctxt.set(Attr::PENDING_DELETE_ID);
      if (newctxt != ctxt) {
        Attr attr(oattr, newctxt);
        ranMesh->update_obj(&obj, attr);
      }
    }
  }

  // Go straight to delete; no parallel resolution needed, since we are handling this explicitly
  if (obj_type != 0) ranMesh->MeshDB::ResolvePendingDelete(obj_type);
  
  // Clean up range of commrel
  MapType().swap(range);
  std::vector<UInt>().swap(range_processors);
}

void CommRel::Print(std::ostream &os) const {
  os << "CommRel name:" << comm_name << std::endl;
  os << "Domain: is_sym=" << symmetric << std::endl;
  os << "Domain procs:(";
  std::copy(domain_processors.begin(), domain_processors.end(), std::ostream_iterator<UInt>(os, ", "));
  os << ")" << std::endl;
  std::copy(domain.begin(), domain.end(), std::ostream_iterator<CommNode>(os, "\n"));
  os << "Range:" << std::endl;
  os << "Range procs:(";
  std::copy(range_processors.begin(), range_processors.end(), std::ostream_iterator<UInt>(os, ", "));
  os << ")" << std::endl;
  std::copy(range.begin(), range.end(), std::ostream_iterator<CommNode>(os, "\n"));
}

void CommRel::complete_range() {
  Trace __trace("CommRel::complete_range()");

  SparseMsg msg;
  UInt ndproc = domain_processors.size();
  UInt csize = msg.commSize();
  
  if (ndproc > 0) {
    msg.setPattern(ndproc, &domain_processors[0]);
  } else {
    msg.setPattern(ndproc, NULL);
  }

  // Sizes.  For each domain object, we send the id_type and object type
  MapType::iterator ci = domain_begin(), ce = domain_end();
  if (ndproc > 0) {
    std::vector<UInt> send_size_all(csize, 0); // need vector proc can look up into (Map?)
    std::vector<UInt> sizes(ndproc, 0);
  
    for (; ci != ce; ++ci) {
      UInt proc = ci->processor;
      send_size_all[proc] += SparsePack<MeshObj::id_type>().size();
      send_size_all[proc] += SparsePack<UInt>().size();
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
    SparsePack<MeshObj::id_type>(b, obj.get_id());
    UInt type = (UInt) obj.get_type();
    SparsePack<UInt>(b, type);
//std::cout << "proc:" << msg.commRank() << "Sending domain item:" << CommNode(&obj,proc) << std::endl;
  }
  if (!msg.filled()) throw("complete range, send buffer not filled");


  // Send messages
  msg.communicate();


  // And now unpack the ids.  Since we don't have a range yet, we use buffer to unpack
  for (UInt *p = msg.inProc_begin(); p != msg.inProc_end(); p++) {
    UInt proc = *p;
    SparseMsg::buffer &b = *msg.getRecvBuffer(proc);

    // We deduce how many id's we are receiving
    UInt nid = b.msg_size() /
       (SparsePack<MeshObj::id_type>().size() + SparsePack<UInt>().size());

    // Loop id's, get the associated mesh object
    for (UInt i = 0; i < nid; i++) {
      MeshObj::id_type id;
      SparseUnpack<MeshObj::id_type>(b, id);
      UInt type;
      SparseUnpack<UInt>(b, type);
  
      MeshDB::MeshObjIDMap::iterator ro = ranMesh->map_find(type, id);
      if (ro == ranMesh->map_end(type)) {
       std::cerr << "P:" << Par::Rank() << "Error, complete range,couldn't find object!  Commname:" << comm_name << std::endl;
       std::cerr << " object =" << MeshObjTypeString(type) << ", id=" << id << std::endl;
       Throw() << "In complete range, could not find object!!";
      }
      
      MeshObj *robj = &*ro;
  
//std::cout << "proc:" << msg.commRank() << "Adding range item:" << CommNode(robj,proc) << std::endl;
      range.push_back(CommNode(robj, proc));
    }
  }
  if (!msg.empty()) throw("complete_range, didn't use up buffer!");
  

  // Update the unique domain processors
  build_range_procs();
}

// Nastiness below: deals with various types that a field may represent.
static int field_pack_size(_field &f) {
  if (f.tinfo() == typeid(double)) {
    return SparsePack<double>::size();
  } else if (f.tinfo() == typeid(int)) {
    return SparsePack<int>::size();
  } else if (f.tinfo() == typeid(float)) {
    return SparsePack<float>::size();
  } else if (f.tinfo() == typeid(long)) {
    return SparsePack<long>::size();
  } else if (f.tinfo() == typeid(char)) {
    return SparsePack<char>::size();
  } else if (f.tinfo() == typeid(UChar)) {
    return SparsePack<UChar>::size();
  } else Throw() << "Unknown data type, skipping ";

}

static void field_pack(SparseMsg::buffer &b, _field &f, const MeshObj &obj) {
  if (f.tinfo() == typeid(double)) {
    double *data = f.data(obj);
    for (UInt d = 0; d < f.dim(); d++) {
      SparsePack<double>(b, data[d]);
    }
  } else if (f.tinfo() == typeid(int)) {
    int *data = f.data(obj);
    for (UInt d = 0; d < f.dim(); d++) {
      SparsePack<int>(b, data[d]);
    }
  } else if (f.tinfo() == typeid(float)) {
    float *data = f.data(obj);
    for (UInt d = 0; d < f.dim(); d++) {
      SparsePack<float>(b, data[d]);
    }
  } else if (f.tinfo() == typeid(long)) {
    long *data = f.data(obj);
    for (UInt d = 0; d < f.dim(); d++) {
      SparsePack<long>(b, data[d]);
    }
  } else if (f.tinfo() == typeid(char)) {
    char *data = f.data(obj);
    for (UInt d = 0; d < f.dim(); d++) {
      SparsePack<char>(b, data[d]);
    }
  } else if (f.tinfo() == typeid(UChar)) {
    UChar *data = f.data(obj);
    for (UInt d = 0; d < f.dim(); d++) {
      SparsePack<UChar>(b, data[d]);
    }
  } else Throw() << "Unknown data type, skipping ";

}

static void field_unpack(SparseMsg::buffer &b, _field &f, const MeshObj &obj) {
  if (f.tinfo() == typeid(double)) {
    double *data = f.data(obj);
    for (UInt d = 0; d < f.dim(); d++) {
      SparseUnpack<double>(b, data[d]);
    }
  } else if (f.tinfo() == typeid(int)) {
    int *data = f.data(obj);
    for (UInt d = 0; d < f.dim(); d++) {
      SparseUnpack<int>(b, data[d]);
    }
  } else if (f.tinfo() == typeid(float)) {
    float *data = f.data(obj);
    for (UInt d = 0; d < f.dim(); d++) {
      SparseUnpack<float>(b, data[d]);
    }
  } else if (f.tinfo() == typeid(long)) {
    long *data = f.data(obj);
    for (UInt d = 0; d < f.dim(); d++) {
      SparseUnpack<long>(b, data[d]);
    }
  } else if (f.tinfo() == typeid(char)) {
    char *data = f.data(obj);
    for (UInt d = 0; d < f.dim(); d++) {
      SparseUnpack<char>(b, data[d]);
    }
  } else if (f.tinfo() == typeid(UChar)) {
    UChar *data = f.data(obj);
    for (UInt d = 0; d < f.dim(); d++) {
      SparseUnpack<UChar>(b, data[d]);
    }
  } else Throw() << "Unknown data type, skipping ";

}

void CommRel::send_fields(UInt _nfields, _field *const *_sfields, _field *const *_rfields) {
  SparseMsg msg;
  UInt ndproc = domain_processors.size();
  UInt csize = msg.commSize();
  
  // Keep track of the pairs we have processed; don't process a pair twice (there
  // is no guarantee the user won't send a pair twice).
  std::set<std::string> pairs_proc;
  UInt nfields = 0;
  std::vector<_field*> sfields, rfields;
  for (UInt f = 0; f < _nfields; f++) {
    std::string fpair_name = _sfields[f]->name() + "_" + _rfields[f]->name();
    std::pair<std::set<std::string>::iterator,bool> si = pairs_proc.insert(fpair_name);
    
    if (si.second) {
      sfields.push_back(_sfields[f]);
      rfields.push_back(_rfields[f]);
      nfields++;
    } //else std::cout << "Found duplicate pair:" << fpair_name << std::endl;
  } 

  if (ndproc > 0) {
    msg.setPattern(ndproc, &domain_processors[0]);
  } else {
    msg.setPattern(ndproc, NULL);
  }


  // Sizes.  For each domain object, we send the id_type and object type
  MapType::iterator ci = domain_begin(), ce = domain_end();
  if (ndproc > 0) {
    std::vector<UInt> send_size_all(csize, 0); // need vector proc can look up into (Map?)
    std::vector<UInt> sizes(ndproc, 0);
  
    for (; ci != ce; ++ci) {
      UInt proc = ci->processor;
      // Loop fields
      for (UInt f = 0; f < nfields; f++) {
        // If object is on this object, add size
        ThrowRequire(rfields[f]->dim() == sfields[f]->dim());
        if (sfields[f]->OnObj(*ci->obj))
          send_size_all[proc] += field_pack_size(*sfields[f])*sfields[f]->dim();
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
      if (sfields[f]->OnObj(*ci->obj)) {
        field_pack(b, *sfields[f], obj);
      }
    }
  }
  if (!msg.filled()) throw("complete range, send buffer not filled");


  // Send messages
  msg.communicate();

  MapType::iterator ri = range_begin();
  // And now unpack the fields
  for (UInt *p = msg.inProc_begin(); p != msg.inProc_end(); p++) {
    UInt proc = *p;
    SparseMsg::buffer &b = *msg.getRecvBuffer(proc);

    while (proc == ri->processor && ri != range_end() ) {
      if (ri == range_end()) {
        std::cout << "P:" << msg.commRank() << " hit end too quickly" << std::endl;
        throw("send fields, CommRel hit end of range prematruely");
      }
      const CommNode &cn = *ri;
      const MeshObj &obj = *cn.obj;
      
      for (UInt f = 0; f < nfields; f++) {
        if (rfields[f]->OnObj(obj)) {
          field_unpack(b, *rfields[f], obj);
        }
      }
        ri++;
    }
  }
   if (ri != range_end()) 
     throw("send_fields, unpacking CommRel, didn't hit end of range:");

   if (!msg.empty()) {
    std::ostringstream ex;
    ex << "send_fields, CommRel, didn't use up buffer!";
    ex << "Fields are:";
    for (UInt f = 0; f < nfields; f++) ex << sfields[f]->name();
    Throw() << ex.str();
   }
}


void CommRel::halo_fields(UInt nfields, _field **sfields) const {
  if (!symmetric)
    throw Ex() << "halo is only implemented for symmetric spec, spec=" << comm_name;
  SparseMsg msg;
  UInt ndproc = domain_processors.size();
  UInt csize = msg.commSize();

  // Use same fields for range
  _field **rfields = sfields;
  
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
      // 1 if owner, 0 if not
      send_size_all[proc] += SparsePack<UInt>().size();
      if (GetAttr(*ci->obj).is_locally_owned()) {
        for (UInt f = 0; f < nfields; f++) {
            if (sfields[f]->OnObj(*ci->obj))
              send_size_all[proc] += SparsePack<double>().size()*sfields[f]->dim();
        }
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
    UInt own = GetAttr(obj).is_locally_owned() ? 1 : 0;
    SparsePack<UInt>(b, own);
    if (GetAttr(obj).is_locally_owned()) {
      for (UInt f = 0; f < nfields; f++) {
        if (sfields[f]->OnObj(obj)) {
          double *data = sfields[f]->data(obj);
          for (UInt d = 0; d < sfields[f]->dim(); d++) {
            SparsePack<double>(b, data[d]);
          }
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
      
    UInt own;
    SparseUnpack<UInt>(b, own);
    if (own) {
      // Coming from the owner, so unpack
      for (UInt f = 0; f < nfields; f++) {
        if (rfields[f]->OnObj(obj)) {
          double *data = rfields[f]->data(obj);
          for (UInt d = 0; d < rfields[f]->dim(); d++)
            SparseUnpack<double>(b, data[d]);
        }
      }
    } else {
      // Nor from the owner, so no data.
    }
  }
  if (!msg.empty()) throw("halo, CommRel, didn't use up buffer!");
}

bool CommRel::is_sym_sane() const {
  bool res = true;
  if (!symmetric)
    throw Ex() << "is sym_sane is only implemented for symmetric spec!";
  SparseMsg msg;
  UInt ndproc = domain_processors.size();
  UInt csize = msg.commSize();

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
      // send 1 if own, 0 if not
      send_size_all[proc] += SparsePack<UInt>().size();
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
    UInt own = 0;
    if (GetAttr(obj).is_locally_owned()) own = 1;
    SparsePack<UInt>(b, own);
  }
  if (!msg.filled()) throw("is sane, send buffer not filled");


  // Send messages
  msg.communicate();

  MapType::const_iterator ri = domain_begin(); // assuming symmetric.
                                      // also assuming sorted by range proc.

  std::map<MeshObj::id_type, UInt> owners;
  for (; ri != domain_end(); ri++) {
    UInt proc = ri->processor;
    SparseMsg::buffer &b = *msg.getRecvBuffer(proc);
    const CommNode &cn = *ri;
    const MeshObj &obj = *cn.obj;
 
    UInt own;
    SparseUnpack<UInt>(b, own);
      
    // Count the owners
    std::map<MeshObj::id_type, UInt>::iterator mi =
      owners.find(obj.get_id());
    if (mi == owners.end())
      owners[obj.get_id()] = own;
    else
      mi->second += own;


    if (GetAttr(obj).is_locally_owned()) {
      if (proc != msg.commRank() && own) {
        std::cout << "is_sane error, obj:" << obj.get_id() <<
        " is owned locally, P:" << msg.commRank() << " and by P"
        << proc << std::endl;
        res = false;
      }
    } else {
    }


  }

  // Loop and see if there is a single owner for all
   ri = domain_begin(); // assuming symmetric.
  for (; ri != domain_end(); ri++) {
    const CommNode &cn = *ri;
    const MeshObj &obj = *cn.obj;
 
    if (GetAttr(obj).is_locally_owned() && owners[obj.get_id()] != 0) {
        std::cout << "is_sane error, obj:" << obj.get_id() <<
        " has " << 1 + owners[obj.get_id()] << " owners!!" << std::endl;
        res = false;
    } else if (!GetAttr(obj).is_locally_owned() && owners[obj.get_id()] != 1) {
        std::cout << "is_sane error, obj:" << obj.get_id() <<
        " has " << owners[obj.get_id()] << " owners!!" << std::endl;
        res = false;
    }
  }


  if (!msg.empty()) throw("is sym sane, CommRel, didn't use up buffer!");

  return res;
}

void CommRel::transpose() {
  // swap the domain/range
  domain_processors.swap(range_processors);

  domain.swap(range);

  // Now we maintain the invariant of having the range sorted by processor
  // list does stable sort.
  std::stable_sort(range.begin(), range.end(), range_sort());

  std::swap(domMesh, ranMesh);

  transposed = !transposed;

}

void CommRel::clear() {
  MapType().swap(domain);
  MapType().swap(range);
  domMesh = ranMesh = NULL;
  comm_name = "";
}

CommRel &CommRel::dependants(CommRel &dcom, UInt obj_type) {
  // TODO tayor to USES type relations.  For now, just loop nodes
  dcom.clear();

  std::vector<CommNode> comm;

  MapType::iterator di = domain_begin(), de = domain_end();
  for (; di != de; ++di) {
    CommNode &cn = *di;
    
    std::vector<MeshObj*> dep_objs;
    MeshObjConn::common_objs(&cn.obj, &cn.obj + 1, MeshObj::USES, obj_type, dep_objs);
//Par::Out()<< "found " << dep_objs.size() << " of type" << MeshObjTypeString(obj_type) << std::endl;
    for (UInt n = 0; n < dep_objs.size(); n++) {
      comm.push_back(CommNode(dep_objs[n], cn.processor));
    }
  }

  // Unique the map
  std::sort(comm.begin(), comm.end(), std::less<CommNode>());
  comm.erase(std::unique(comm.begin(), comm.end()),
                comm.end());

  dcom.Init(comm_name + "_dep", *domMesh, *ranMesh, false);
  dcom.add_domain(comm);

  return dcom;
}

CommRel &CommRel::ancestors(CommRel &acom) {

  acom.clear();

  // TODO tayor to USES type relations.  For now, just loop nodes
  std::vector<CommNode> comm;

  MapType::iterator di = domain_begin(), de = domain_end();
  for (; di != de; ++di) {
    CommNode &cn = *di;
    
    const MeshObj &node = *cn.obj;
    MeshObjRelationList::const_iterator rbeg = node.Relations.begin(),
                 rend = node.Relations.end();
    for (; rbeg != rend; ++rbeg) {
      const MeshObj::Relation &rel = *rbeg;
      if (rel.obj->get_type() != MeshObj::ELEMENT) continue;
      MeshObj &elem = *rel.obj;
      comm.push_back(CommNode(&elem, cn.processor));
    }
  }

  // Unique the map
  std::sort(comm.begin(), comm.end(), std::less<CommNode>());
  comm.erase(std::unique(comm.begin(), comm.end()),
                comm.end());


  acom.Init(comm_name + "_anc", *domMesh, *ranMesh, false);

  acom.add_domain(comm);
  return acom;
}

bool CommRel::verify_symmetric_comm() {
  Trace __trace("CommRel::verify_symmetric_comm()");


  ThrowRequire(symmetric);


  UInt nproc = domain_processors.size();
  std::vector<UInt> send_sizes_all(Par::Size(), 0);

  // Send our id out
  SparseMsg msg;
  MapType::iterator di = domain_begin(), de = domain_end();

  // Pack sizes
  for (; di != de; ++di) {
    UInt proc = di->processor;

    send_sizes_all[proc] += SparsePack<MeshObj::id_type>::size();

  } // for di

  msg.setPattern(nproc, nproc == 0 ? NULL : &domain_processors[0]);

  std::vector<UInt> send_sizes(nproc, 0);
  for (UInt i = 0; i < nproc; i++) send_sizes[i] = send_sizes_all[domain_processors[i]];

  msg.setSizes(nproc == 0 ? NULL : &send_sizes[0]);

  // Packing loop
  for (di = domain_begin(); di != de; ++di) {
    MeshObj &obj = *di->obj;
    UInt proc = di->processor;

    SparseMsg::buffer &b = *msg.getSendBuffer(proc);


    SparsePack<MeshObj::id_type>(b, obj.get_id());

  } // for di

  if (!msg.filled())
    Throw() << "verfify sym comm message not filled!!";

  msg.communicate();

  // Sort the domain for unpacking from processors
  std::stable_sort(domain_begin(), domain_end(), range_sort());
  // Unpack message
  di = domain_begin();
  for (; di != de; ++di) {
    UInt proc = di->processor;
    SparseMsg::buffer &b = *msg.getRecvBuffer(proc);

    MeshObj &obj = *di->obj;

    MeshObj::id_type id;

    SparseUnpack<MeshObj::id_type>(b, id);

    if (id != obj.get_id()) {
      Par::Out() << "Error: expected (proc,id):(" << di->processor << ", " << obj.get_id()
            << "), but received id:" << id << " from proc " << proc << ", otype=" << MeshObjTypeString(obj.get_type()) << std::endl;
      return false;
    }

  } // unpack by proc

  if (!msg.empty())
    Throw() << "Message not emptied in verify sym comm!!";

  // Put the domain map back in standard order
  std::sort(domain_begin(), domain_end());

  return true;

}

std::ostream &operator<<(std::ostream &os, const CommRel::CommNode &cn) {
  int rank = Par::Rank();
  os << "(" << cn.obj->get_id() << ", P:" << rank << ", {" << GetAttr(*cn.obj) << "}, " << cn.processor << ")";
  return os;
}


} // namespace
} // namespace
