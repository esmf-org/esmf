// $Id: ESMC_CommReg.C,v 1.2.2.2 2009/01/21 21:25:22 cdeluca Exp $
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
#include <ESMC_CommReg.h>
#include <ESMC_Meshfield.h>
#include <ESMC_MeshObjPack.h>
#include <ESMC_SparseMsg.h>
#include <ESMC_ParEnv.h>
#include <ESMC_Mesh.h>


namespace ESMCI {
namespace MESH {

CommReg::CommReg() :
node_rel(),
edge_rel(),
face_rel(),
elem_rel(),
dom(NULL),
ran(NULL)
{
}

CommReg::CommReg(const std::string &_name, Mesh &domainMesh, Mesh &rangeMesh) :
node_rel(_name, domainMesh, rangeMesh),
edge_rel(_name, domainMesh, rangeMesh),
face_rel(_name, domainMesh, rangeMesh),
elem_rel(_name, domainMesh, rangeMesh),
dom(&domainMesh),
ran(&rangeMesh)
{
}

CommReg::~CommReg()
{
}

void CommReg::Transpose() {
  node_rel.transpose();
  edge_rel.transpose();
  face_rel.transpose();
  elem_rel.transpose();
}

const CommRel &CommReg::GetCommRel(UInt objtype) const {
  switch (objtype) {
    case MeshObj::NODE:
      return node_rel;
    case MeshObj::EDGE:
      return edge_rel;
    case MeshObj::FACE:
      return face_rel;
    case MeshObj::ELEMENT:
      return elem_rel;
    default:
      Throw() << "Unknown mesh boj type:" << objtype;
  }
}

CommRel &CommReg::GetCommRel(UInt objtype) {
  return const_cast<CommRel&>(static_cast<const CommReg*>(this)->GetCommRel(objtype));
}

void CommReg::SendFields(UInt nfields, MEField<> *const *sfields, MEField<> *const *rfields) {
  // Get all the subfields and send out over the specs.
  std::vector<_field*> sf;
  std::vector<_field*> rf;
  UInt obj_type = 0;
  for (UInt i = 0; i < nfields; i++) {
    if (&sfields[i]->GetMEFamily() != &rfields[i]->GetMEFamily())
      throw Ex() << "Send fields, me for " << sfields[i]->name() << " does not match rfield:"
                 << rfields[i]->name();
    sfields[i]->Getfields(sf);
    rfields[i]->Getfields(rf);
  }

  for (UInt j = 0; j < sf.size(); j++)
    obj_type |= sf[j]->GetAttr().get_type();


/*
std::cout << "sf size=" << sf.size() << ". _fields are:";
for (UInt i = 0; i < sf.size(); i++) {
  std::cout << sf[i]->name() << ", dim=" << sf[i]->dim() << std::endl;
}
std::cout << "rf size=" << rf.size() << ". _fields are:";
for (UInt i = 0; i < rf.size(); i++) {
  std::cout << rf[i]->name() << ", dim=" << rf[i]->dim() << std::endl;
}
*/
  ThrowRequire(sf.size() == rf.size());
  // Now send via the spec(s)
  // TODO: be smarter: select only the relevant spec to send each field.
  if ((obj_type & MeshObj::NODE)) node_rel.send_fields(sf.size(), &sf[0], &rf[0]);
  if ((obj_type & MeshObj::EDGE)) edge_rel.send_fields(sf.size(), &sf[0], &rf[0]);
  if ((obj_type & MeshObj::FACE)) face_rel.send_fields(sf.size(), &sf[0], &rf[0]);
  if ((obj_type & MeshObj::ELEMENT)) elem_rel.send_fields(sf.size(), &sf[0], &rf[0]);
}

void CommReg::HaloFields(UInt nfields, MEField<> **sfields) {
  Trace __trace("CommReg::HaloFields(UInt nfields, MEField<> **sfields)");
  ThrowRequire(dom==ran);
  // Get all the subfields and send out over the specs.
  std::vector<_field*> sf;
  UInt obj_type = 0;
  for (UInt i = 0; i < nfields; i++) {
    sfields[i]->Getfields(sf);
  }
  for (UInt j = 0; j < sf.size(); j++)
    obj_type |= sf[j]->GetAttr().get_type();

  // Now send via the spec(s)
  // TODO: be smarter: select only the relevant spec to send each field.
  if (obj_type & MeshObj::NODE) node_rel.halo_fields(sf.size(), &sf[0]);
  if (obj_type & MeshObj::EDGE) edge_rel.halo_fields(sf.size(), &sf[0]);
  if (obj_type & MeshObj::FACE) face_rel.halo_fields(sf.size(), &sf[0]);
// doesnt make sense for halo  if (obj_type & MeshObj::ELEMENT) elem_rel.halo_fields(sf.size(), &sf[0]);
}

void static sync(CommRel &comm) {
  Trace __trace("sync(CommRel &comm)");
  UInt csize = Par::Size();

  SparseMsg msg;

  // Sizing loop
  std::vector<UInt> send_sizes_all(csize, 0);
  std::vector<UInt> to_proc;
  std::vector<UInt> to_sizes;

  CommRel::MapType::iterator di = comm.domain_begin(), de = comm.domain_end();
  for (; di != de; ++di) {
    UInt proc = di->processor;

    std::vector<UInt>::iterator lb =
        std::lower_bound(to_proc.begin(), to_proc.end(), proc);
    if (lb == to_proc.end() || *lb != proc) 
      to_proc.insert(lb, proc);

    // Attr
    send_sizes_all[proc] += SparsePack<Attr>::size();

  } //sizes

  UInt nsend = to_proc.size();
  msg.setPattern(nsend, nsend == 0 ? NULL : &to_proc[0]);

  to_sizes.resize(nsend, 0);
 for (UInt i = 0; i < nsend; i++)
    to_sizes[i] = send_sizes_all[to_proc[i]];

  msg.setSizes(nsend == 0 ? NULL : &to_sizes[0]);

  // Pack loop
  di = comm.domain_begin();
  for (; di != de; ++di) {
   UInt proc = di->processor;
   MeshObj &obj = *di->obj;
   SparseMsg::buffer &b = *msg.getSendBuffer(proc);

   // Attr
   SparsePack<Attr>(b, GetAttr(obj));

  }

  if (!msg.filled())
    Throw() << "Message not full in sync attr!";

  msg.communicate();

  // Create an object to context map so that objects are
  // only updated in the mesh onces.
  typedef std::map<MeshObj*, Context> Obj_To_Ctxt_Type;
  Obj_To_Ctxt_Type obj_to_ctxt;

  // Unpack
  di = comm.domain_begin();
  for (; di != de; ++di) {
    MeshObj &obj = *di->obj;
    UInt proc = di->processor;
    SparseMsg::buffer &b = *msg.getRecvBuffer(proc);

    Attr a;
    SparseUnpack<Attr>(b, a);

    // Now the kernel.  Or all attributes except the shared ones
    // (ownership, etc...)
    const Attr &oa = GetAttr(obj);

    // For sanity:
    ThrowRequire(a.get_type() == oa.get_type());
    ThrowRequire(a.get_key() == oa.get_key());
    
    // Now merge the contexts
    Context c = a.get_context();
    const Context &oc = oa.get_context();
    Context nc(oc);

    // More sanity checks

    //ThrowRequire(c.is_set(Attr::ACTIVE_ID) == oc.is_set(Attr::ACTIVE_ID));
    if (!(c.is_set(Attr::ACTIVE_ID) == oc.is_set(Attr::ACTIVE_ID))) {
      Par::Out() << "Error, ACTIVE_ID incongruence, obj:" << obj;
      Par::Out() << "Incoming ctxt:" << c << std::endl;
      Throw();
    }
    ThrowRequire(c.is_set(Attr::SHARED_ID) && oc.is_set(Attr::SHARED_ID));
    ThrowRequire(c.is_set(Attr::GENESIS_ID) == oc.is_set(Attr::GENESIS_ID));

    // Both can't claim to own object
    //ThrowRequire(!(c.is_set(Attr::OWNED_ID) && oc.is_set(Attr::OWNED_ID)));
    if ((c.is_set(Attr::OWNED_ID) && oc.is_set(Attr::OWNED_ID))) {
      Par::Out() << "Error, OWNED_ID incongruence, obj:" << obj;
      Par::Out() << "Incoming attr:" << a << std::endl;
      Par::Out() << "From processor:" << proc << std::endl;
      Throw();
    }

    // Clear the bits not to merge
    c.clear(Attr::SHARED_ID);
    c.clear(Attr::OWNED_ID);
    c.clear(Attr::ACTIVE_ID);
    c.clear(Attr::GENESIS_ID);

    // Or the rest
    nc |= c;

    // Add or OR in the new context, depending on whether object is in map.
    std::pair<Obj_To_Ctxt_Type::iterator, bool> otci = 
      obj_to_ctxt.insert(std::make_pair(&obj, nc));
    if (otci.second == false) { // already there
      otci.first->second |= nc;
    }
  }

  // One last loop through the object map, updating the mesh
  Obj_To_Ctxt_Type::iterator oi = obj_to_ctxt.begin(), oe = obj_to_ctxt.end();
  for (; oi != oe; ++oi) {
    MeshObj &obj = *oi->first;
    Context &nc = oi->second;
    if (nc != GetMeshObjContext(obj)) {
      Attr oa(GetAttr(obj), nc);
      comm.DomainMesh()->update_obj(&obj, oa); 
    }
  }
}

void CommReg::SyncAttributes() {
  Trace __trace("CommReg::SyncAttributes()");
  sync(node_rel);
  sync(edge_rel);
  sync(face_rel);
  sync(elem_rel);
}

template<typename VTYPE>
void CommReg::SwapOp(UInt nfields, MEField<> **sfields, int op) {
  ThrowRequire(dom==ran);
  // Get all the subfields and send out over the specs.
  std::vector<_field*> sf;
  UInt obj_type = 0;
  for (UInt i = 0; i < nfields; i++) {
    sfields[i]->Getfields(sf);
  }
  for (UInt j = 0; j < sf.size(); j++)
    obj_type |= sf[j]->GetAttr().get_type();

  // Now send via the spec(s)
  // TODO: be smarter: select only the relevant spec to send each field.
  if (obj_type & MeshObj::NODE) node_rel.swap_op<VTYPE,_field>(sf.size(), &sf[0], op);
  if (obj_type & MeshObj::EDGE) edge_rel.swap_op<VTYPE,_field>(sf.size(), &sf[0], op);
  if (obj_type & MeshObj::FACE) face_rel.swap_op<VTYPE,_field>(sf.size(), &sf[0], op);
// doesnt make sense for halo  if (obj_type & MeshObj::ELEMENT) elem_rel.halo_fields(sf.size(), &sf[0]);
}

bool CommReg::VerifySymComm() {
  if (!node_rel.verify_symmetric_comm()) return false;
  if (!edge_rel.verify_symmetric_comm()) return false;
  if (!face_rel.verify_symmetric_comm()) return false;
  return true;
}

void CommReg::CommPrint(std::ostream &os) {
  node_rel.Print(os);
  edge_rel.Print(os);
  face_rel.Print(os);
  elem_rel.Print(os);
}

void CommReg::clear() {
  node_rel.clear();
  edge_rel.clear();
  face_rel.clear();
  elem_rel.clear();
}

template void CommReg::SwapOp<double>(UInt nfields, MEField<> **sfields,int);
template void CommReg::SwapOp<int>(UInt nfields, MEField<> **sfields,int);
template void CommReg::SwapOp<char>(UInt nfields, MEField<> **sfields,int);

} // namespace 
} // namespace 
