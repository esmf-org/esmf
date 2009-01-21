// $Id: ESMC_Kernel.h,v 1.2.2.2 2009/01/21 21:25:22 cdeluca Exp $
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

#ifndef ESMC_Kernel_h
#define ESMC_Kernel_h

#include <ESMC_Attr.h>
#include <ESMC_MeshObj.h>

namespace ESMCI {
namespace MESH {

class MeshDB;
class MeshObjTopo;

class MEFieldBase;
class MasterElementBase;
class _field;

class MappingBase;
class intgRule;

/**
 * Class to hold and manage a group of homogeneous mesh objects.
 * All such objects have the exact same fields, type, etc...
*/
class Kernel : public ListNode<Kernel> {
public:
  friend class FieldReg;
  friend class Mesh;
  friend class MeshDB;
  friend MasterElementBase &GetME(const MEFieldBase &field, const MeshObj &obj);
  friend MasterElementBase &GetME(const MEFieldBase &field, const Kernel &obj);
  typedef List<MeshObj>::iterator obj_iterator;
  typedef List<MeshObj>::const_iterator obj_const_iterator;
  Kernel(MeshDB &_mesh, const Attr &rhs);
  ~Kernel();

  // Amongst (possibley) other things, set aside field storage.
  // Send the list of all fields (whether they live here or not)
  void Commit(UInt nFields, MEFieldBase **Fields, UInt nfields, _field **fields);

  UInt type() const { return my_attr.get_type();}
  bool empty() const { return count == 0; }
  UInt key() const { return my_attr.get_key();}
  const Attr &GetAttr() const { return my_attr;}
  const Context &GetContext() const { return my_attr.get_context();}
  UInt NumObjects() { return count; }
  obj_iterator obj_begin() { 
    return objects.begin(); }
  obj_iterator obj_end() { 
    return objects.end(); }
  obj_const_iterator obj_begin() const { 
    return objects.begin(); }
  obj_const_iterator obj_end() const { 
    return objects.end(); }


  UInt Count() const { return count;}

  // Take object out of meshset.  Does not (yet) release the
  // field store, in case object needs to copy data over into a new
  // kernel.  This is done as a second step, below:
  void erase(MeshObj &obj) {
    objects.erase(obj);
    count--;
  }

  // Called when an object is to free data
  void ReleaseStore(_fieldStore *store, UInt idx);


  void PrintStoreInfo() const;

  List<MeshObj> objects;
  List<_fieldStore> stores;
  typedef List<_fieldStore>::iterator store_iterator;
  typedef List<_fieldStore>::const_iterator store_const_iterator;
  // Keep a pointer to the next store with space.
  _fieldStore *next_open_store; 
  // Sorts by attr;
  bool operator<(const Kernel &rhs) const {
    return my_attr < rhs.my_attr;
  }
  const MeshDB &get_mesh() const { return mesh;}
  MeshDB &get_mesh() { return mesh;}
  const MeshObjTopo *GetTopo() const { return topo;}

  void AssimilateObject(MeshObj &obj);

  bool is_active() const { return GetContext().is_set(Attr::ACTIVE_ID); }
  bool is_shared() const { return GetContext().is_set(Attr::SHARED_ID); }
  bool is_exposed() const { return GetContext().is_set(Attr::EXPOSED_BOUNDARY_ID); }
  bool is_genesis() const { return GetContext().is_set(Attr::GENESIS_ID); }

  const MappingBase &GetMapping() const { ThrowRequire(mapping); return *mapping;}

  const intgRule *GetIntg() const { return irule;}

  // Imprint an object with the ME's store on this kernel;
  void Imprint(MeshObj &obj) const;

private:
  const MeshObjTopo *topo;
  int count;  // how many items
  void AddME(UInt f_ord, MasterElementBase *me);
  MasterElementBase *GetME(UInt ordinal) const { ThrowAssert(ordinal < MEFieldme.size()); return MEFieldme[ordinal];}
  Kernel(const Kernel &rhs);
  Kernel &operator=(const Kernel &rhs);
  Attr my_attr;
  MeshDB &mesh;
  std::vector<MEFieldBase*> Fields;
  std::vector<_field*> fields;
  std::vector<MasterElementBase *> MEFieldme;
  bool is_committed;
  MappingBase *mapping;
  intgRule *irule; // default irule
};

typedef List<Kernel> KernelList;

const MeshObjTopo *GetMeshObjTopo(const MeshObj &obj);

inline const Attr &GetAttr(const MeshObj &obj) {
  const Kernel *md = obj.GetKernel();
  return md->GetAttr();
}

inline const Context &GetMeshObjContext(const MeshObj &obj) {
  const Kernel *md = obj.GetKernel();
  return md->GetAttr().get_context();
}

inline const MeshDB &GetMeshObjMesh(const MeshObj &obj) {
  return obj.GetKernel()->get_mesh();
}

inline MeshDB &GetMeshObjMesh(MeshObj &obj) {
  return obj.GetKernel()->get_mesh();
}

inline bool IsExposed(const MeshObj &obj) {
  return obj.GetKernel()->is_exposed();
}

std::ostream &operator<<(std::ostream &os, const MeshObj &obj);

} // namespace
} // namespace

#endif
