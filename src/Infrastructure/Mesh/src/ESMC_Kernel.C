// $Id: ESMC_Kernel.C,v 1.3.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
#include <ESMC_Kernel.h>
#include <ESMC_MEField.h>
#include <ESMC_MEImprint.h>
#include <ESMC_MeshObjConn.h>
#include <iterator>


namespace ESMCI {
namespace MESH {

const MeshObjTopo *GetMeshObjTopo(const MeshObj &obj) {
  return obj.GetKernel()->GetTopo();
}

std::ostream &operator<<(std::ostream &os, const MeshObj &obj) {
  os << obj.GetKernel()->GetAttr() << std::endl;
  os << " id=" << obj.get_id() << std::endl;
  os << " owner=" << obj.get_owner() << std::endl;
  obj.printrelations(os);

  // Print virtual node relations
  if (obj.get_type() == MeshObj::EDGE || obj.get_type() == MeshObj::EDGE) {
    std::vector<MeshObj*> nodes;
    if (MeshObjConn::get_obj_nodes(obj, nodes, false)) {
      for (UInt n = 0; n < nodes.size(); n++) {
        os << "\tVNODE: " << nodes[n]->get_id() << ", ord=" << n << std::endl;
      }
    }
  }

  return os;
}

Kernel::Kernel(MeshDB &_mesh, const Attr &rhs) :
    ListNode<Kernel>(),
    objects(),
    stores(),
    next_open_store(NULL), 
    topo(NULL),
    count(0),
    my_attr(rhs),
    mesh(_mesh),
    fields(),
    is_committed(false),
    mapping(NULL),
    irule(NULL)
{
  // Just make sure context is not zero (could cause trouble)
  if (!my_attr.get_context().any(my_attr.get_context())) 
    Throw() << "Zero context in kernel, attr=" << my_attr;
}

void Kernel::AddME(UInt f_ord, MasterElementBase *me) {
  MEFieldme[f_ord] = me;
}

void Kernel::Commit(UInt nFields, MEFieldBase **fds, UInt nfields, _field **_fields) {
  Trace __trace("Kernel::Commit(UInt nFields, MEFieldBase **fds)");

  if (is_committed)
    Throw() << "Kernel already committed!!";
  // Save list of fields
  Fields.reserve(nFields);
  std::copy(fds, fds + nFields, std::back_inserter(Fields)); // for imprinting later

  UInt nstore = (objects.size() + _fieldStore::NOBJS-1) / _fieldStore::NOBJS;

  // Extract out the low level fields for the data stores
  MEFieldme.resize(nFields, (MasterElementBase*)(0));
  for (UInt i = 0; i < nFields; i++) {
    MEField<> *meF = static_cast<MEField<>*>(fds[i]);
    ThrowRequire(meF);

    // Also, store of the Master Elements
    if (this->type() == meF->GetType() && this->GetContext().any(meF->GetContext())) {
      ThrowRequire(topo);
      const MEFamily &Fam = meF->GetMEFamily();
          MasterElement<> &me = *Fam.getME(topo->name);
          this->AddME(meF->GetOrdinal(), &me);
    } // if field matches
  }
  
  std::copy(_fields, _fields + nfields, std::back_inserter(fields));
    
  for (UInt i = 0; i < nstore; i++) {
    _fieldStore *s = new _fieldStore();
    stores.push_back(*s);
   s->Create(fields.size(), &fields[0], my_attr);
  }

  // Assign MeshObjs to stores.  Do this in reverse so that
  // the non-full bucket(s) is always at the front.
  // Pending create/delete do not have data
  if (!(my_attr.get_context().is_set(Attr::PENDING_CREATE_ID))) {
    obj_iterator oi = objects.begin(), oe = objects.end();
    store_iterator se = stores.end(), sb = stores.begin();

    for (se--; oi!=oe; ++oi) {
       MeshObj &obj = *oi;
       if (se->Full()) {
         if (se == sb)
           Throw() << "Not enough fieldStores for objects in kernel::Commit()!";
         --se;
       }
       _fieldStore &s = *se;
  
       UInt idx = s.insert();
       obj.AssignStore(&s, idx);
    }
  }

  is_committed = true;

}

Kernel::~Kernel() {
  //std::cout << "Deleting kernel:" << my_attr << std::endl;
  if (is_committed) {

    // delete the stores.  Dance a bit, since deleting
    // object removes it from the list.
    store_iterator si = stores.begin(), se = stores.end();
    for (; si != se; ) {
      _fieldStore *s = &*si;
      si++;
      delete s;
    }
  }
}

void Kernel::ReleaseStore(_fieldStore *store, UInt idx) {
  store->remove(idx);
  // Now put store at beginning of list since it has free data.
  stores.erase(*store);
  stores.push_front(*store);
}

void Kernel::AssimilateObject(MeshObj &obj) {
  // Move object into subroster

  objects.push_back(obj);
  count++;

  Kernel *old_kernel = obj.GetKernel();
  obj.meshset = this;

  // Now take care of data (if kernel committed)
  if (is_committed == false) return;

  /*---------------------------------------------*/
  // Don't get rid of old store until after CopyIn below!!!
  const std::pair<_fieldStore*,UInt> &st = obj.GetStore();

  if (my_attr.get_context().is_set(Attr::PENDING_CREATE_ID)) {

//std::cout << "Assimilate pending create obj:" << MeshObjTypeString(obj.get_type()) << " id:" << obj.get_id() << std::endl;
    // if object had data, free it
    if (st.first != NULL)
      old_kernel->ReleaseStore(st.first, st.second);

    obj.AssignStore(NULL,0); // wipe data
    return;
  }


  // Get a slot in current object.  First object always has space.
 {
    // Possible when adding a new kernel with no object
    if (stores.begin() == stores.end()) {
      _fieldStore *ns = new _fieldStore();
      ns->Create(fields.size(), &fields[0], my_attr);
      stores.push_front(*ns);
    }

    _fieldStore &s = *stores.begin();
    
    if (s.Full()) {
      // Move store to end.  This allows possible nonfull stores to bubble up.
      //  If next store is Full, then create a new store.
      stores.erase(s);
      stores.push_back(s);
      if (stores.begin()->Full()) {
        // Must create a new store
        _fieldStore *ns = new _fieldStore();
        ns->Create(fields.size(), &fields[0], my_attr);
        stores.push_front(*ns);
      } // else ok
    }
  }

  _fieldStore &s = *stores.begin(); // guarenteed now to have space

  UInt idx = s.insert();
//std::cout << "idx:" << idx << std::endl;

  // Copy data over, if needed
  if (st.first != NULL) {
    ThrowRequire(old_kernel);  // must have had a kernel
    // copy data from old store to new
    s.CopyIn(fields.size(), &fields[0], obj, idx);
    // take out of old store.
    old_kernel->ReleaseStore(st.first, st.second);
  } else {
    // zero data out
    s.Zero(fields.size(), &fields[0], idx);
//std::cout << "zeroing: store:" << (int) &s << ", idx=" << idx << std::endl;
  }

  // A last detail:  If we have filled the store, we must move
  // it to the back in case a delete pushes a free store in front of
  // us.  In this case, when it fills we will 'hide' the free blocks behind us.
  if (s.Full()) {
    // Move store to end.  This allows possible nonfull stores to bubble up.
    //  If next store is full, then create a new store.
    stores.erase(s);
    stores.push_back(s);
  }

//std::cout << "obj:" << MeshObjTypeString(obj.get_type()) << " id:" << obj.get_id() << " idx:" << idx << " store:" << (int) &s << std::endl;
  obj.AssignStore(&s, idx);
  
}

void Kernel::Imprint(MeshObj &obj) const {
  for (UInt i = 0; i < MEFieldme.size(); i++) {
    MEFieldBase &mef = *Fields[i];
    if (MEFieldme[mef.GetOrdinal()]) 
      MEImprint(mef.name(), obj, *MEFieldme[mef.GetOrdinal()]);
  }
}

void Kernel::PrintStoreInfo() const {
  store_const_iterator si = stores.begin(), se = stores.end();
  
  std::cout << "Kernel store report: ";

  for (; si != se; ++si) {
    
    std::cout << si->FillRatio()*100.0 << "% ";
  }
  std::cout << std::endl;
}

} // namespace
} // namespace
