// $Id: ESMC_FieldReg.C,v 1.2 2007/08/07 20:46:00 dneckels Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <ESMC_FieldReg.h>
#include <ESMC_MEImprint.h>
#include <algorithm>

namespace ESMCI {
namespace MESH {

FieldReg::FieldReg() :
is_committed(false),
fmap(),
_fmap(),
nfields(0),
fields(),
ndfields(),
efields()
{
}

FieldReg::~FieldReg() {
  // Erase all fields
  // MEFields
  {
    FMapType::iterator fi = fmap.begin(), fe = fmap.end();
    for (; fi!=fe; ++fi) {
      delete fi->second;
    }
  }
  // _fields
  {
    fMapType::iterator fi = _fmap.begin(), fe = _fmap.end();
    for (; fi!=fe; ++fi) {
      delete fi->second;
    }
  }
}

MEField<> *FieldReg::GetCoordField() const {
  MEField<> *cf = GetField("coordinates");
  ThrowAssert(cf);
  return cf;
}

void FieldReg::MatchFields(UInt nfields, MEField<> **fds, std::vector<MEField<>*> &res) const {
  for (UInt i = 0; i < nfields; i++) {
    MEField<> *mf = GetField(fds[i]->name());
    ThrowRequire(mf);
    res.push_back(mf);
  }
}

MEField<> *FieldReg::RegisterField(const std::string &name, const MEFamily &mef,
        UInt obj_type, const Context &ctxt, UInt dim, bool out, const _fieldTypeBase &ftype)
{

  if (is_committed)
    Throw() << "FieldReg is already committed when registering:" << name;

  FMapType::iterator fi = fmap.lower_bound(name);

  MEField<> *nf;
  if (fi == fmap.end() || fi->first != name) {
    // Create the new field
    nf = new MEField<>(name, mef, obj_type, ctxt, dim, out, ftype);
    fmap.insert(fi, std::make_pair(name, nf));
    Fields.push_back(nf);
  } else {
    Throw() << "MEField name:" << name << " already registered";
  }

  return nf;
}

_field *FieldReg::Registerfield(const std::string &name,
                                const Attr &attr,
                                const _fieldTypeBase &_ftype,
                                UInt dim) 
{

  _field *fd;

  fMapType::iterator fi = _fmap.lower_bound(name);
  if (fi == _fmap.end() || fi->first != name) {
    // Create the new field
    fd = new _field(name, attr, _ftype, dim);
    _fmap.insert(fi, std::make_pair(fd->name(), fd));
  } else
    Throw() << "_field:" << name << " already registered!";

  return fd;
}

MEField<> *FieldReg::GetField(const std::string &fname) const {
  FMapType::const_iterator fi = fmap.find(fname);

  return fi == fmap.end() ? NULL : fi->second;
}

void FieldReg::CreateDBFields() {


  // Nodal fields become lagrange fields
  Context ctxt;
  ctxt.flip();
  {
    UInt n = ndfields.size();
    // Use the standard lagrange 
    for (UInt i = 0; i < n; i++) {
      IOField<NodalField> &nf = *ndfields[i];
      RegisterField(nf.name(), MEFamilyStd::instance(), MeshObj::ELEMENT,
         ctxt, nf.dim(), nf.output_status());
    }
  }

  // Element fields become lagrange fields
  {
    UInt n = efields.size();
    // Use the standard lagrange 
    for (UInt i = 0; i < n; i++) {
      IOField<ElementField> &ef = *efields[i];
      RegisterField(ef.name(), MEFamilyDG0::instance(), MeshObj::ELEMENT,
              ctxt, ef.dim(), ef.output_status());
    }
  }
}

void FieldReg::PopulateDBFields(MeshDB &mesh) {
  // Nodal field first
  {
    UInt n = ndfields.size();
    // Use the standard lagrange 
    for (UInt i = 0; i < n; i++) {
      IOField<NodalField> &nf = *ndfields[i];
      MEField<> *mf = GetField(nf.name());
      if (mf == NULL) Throw() << "Populate, could not get nodal field:" << nf.name();

      if (mf->GetMEFamily().is_nodal() != true) Throw() << "Nodal field should have nodal ME";

      UInt fdim = nf.dim();
      if (fdim != mf->dim()) Throw() << "PopDB, fields dims do not match:(" << fdim << ", " << mf->dim() << ")";
      _field *llf = mf->GetNodalfield();
      if (llf == NULL) Throw() << "No primaryfield dof field in nodal Populate";

      // Loop mesh, assigning values.  We assume that all nodes are represented
      MeshDB::iterator ni = mesh.node_begin(), ne = mesh.node_end();
      for (; ni != ne; ++ni) {
        if (ni->get_data_index() < 0) Throw() << "Node:" << *ni << " has unassigned data index!!";
        double *d = nf.data(*ni);
        double *newd = llf->data(*ni);
        if (newd == NULL) Throw() << "Null data!!" << *ni;
        double *endd = newd + fdim;
        while (newd != endd) *newd++ = *d++;
      } // ni
      
    } // nfields
  }
  

  { // Element fields
    UInt n = efields.size();
    // Use the standard lagrange 
    for (UInt i = 0; i < n; i++) {
      IOField<ElementField> &ef = *efields[i];
      MEField<> *mf = GetField(ef.name());
      if (mf == NULL) Throw() << "Populate, could not get element field:" << ef.name();

      if (mf->GetMEFamily().is_elemental() != true) Throw() << "Element field should have elemental ME";

      UInt fdim = ef.dim();
      if (fdim != mf->dim()) Throw() << "PopDB, fields dims do not match:(" << fdim << ", " << mf->dim() << ")";
      _field *llf = mf->GetElementfield();
      if (llf == NULL) Throw() << "No 1 dof field in element Populate";

      // Loop mesh, assigning values.  We assume that all nodes are represented
      MeshDB::iterator ni = mesh.elem_begin(), ne = mesh.elem_end();
      for (; ni != ne; ++ni) {
        if (ni->get_data_index() < 0) Throw() << "Node:" << *ni << " has unassigned data index!!";
        double *d = ef.data(*ni);
        double *newd = llf->data(*ni);
        if (newd == NULL) Throw() << "Null data!!";
        double *endd = newd + fdim;
        while (newd != endd) *newd++ = *d++;
      } // ni
      
    } // nfields
  }
}

void FieldReg::ReleaseDBFields() {
  {
    UInt n = ndfields.size();
    for (UInt i = 0; i < n; i++) {
      delete ndfields[i];
    }
    std::vector<IOField<NodalField>*>().swap(ndfields);
  }
  {
    UInt n = efields.size();
    for (UInt i = 0; i < n; i++) {
      delete efields[i];
    }
    std::vector<IOField<ElementField>*>().swap(efields);
  }
}

void FieldReg::Commit(MeshDB &mesh) {

  // Step 1: Imprint the objects for low level fields
  {
    FMapType::iterator fi = fmap.begin(), fe = fmap.end();
    UInt ord = 0; // number MEFields
    for ( ;fi !=fe; ++fi) {
      std::vector<UInt> nvalSet; // keep track of sizes of _fields
      std::vector<UInt> nvalSetObj; // keep track of sizes of _fields
      MEField<> &f = *fi->second;
//std::cout << "Imprinting MEField:" << f.name() << std::endl;
      f.ordinal = ord++;
      // Loop obj type
      KernelList::iterator ki = mesh.set_begin(), ke = mesh.set_end(), kn;
      for (; ki != ke; ) {
        kn = ki; ++kn; // manage iterators in case imprint changes kernel list
        Kernel &ker = *ki;
        // if kernel wrong type or context doesnt match, move on
        if (ker.type() == f.GetType() && ker.GetContext().any(f.GetContext())) {
  
          const MeshObjTopo *otopo = ker.GetTopo();
          if (!otopo)
            Throw() << "Field " << f.name() << " has no topo on matching kernel";
          const MEFamily &mef = f.GetMEFamily();
          MasterElement<> &me = *mef.getME(otopo->name);
  //std::cout << "topo:" << otopo->name << " yields me:" << me.name << std::endl;
          // loop objects, imprint
          Kernel::obj_iterator oi = ker.obj_begin(), oe = ker.obj_end(), on;
          for (; oi != oe; ) {
            on = oi; ++on;
            MEImprint(f.name(), *oi, me, nvalSet, nvalSetObj);
            oi = on;
          } // oi
        }
  
        ki = kn;
      } // for k
      // Register low level fields
      for (UInt i = 0; i < nvalSet.size(); i++) {
        UInt nval = nvalSet[i];
        Context ctxt;
        char buf[1024];
        std::sprintf(buf, "%s_%d", f.name().c_str(), nval);
        UInt c_id = mesh.GetContext(buf);
        ctxt.set(c_id);
        Attr fatt(nvalSetObj[nval], ctxt);
        f.Addfield(Registerfield(buf, fatt, f.FType(), nval*f.dim()), nval);
//std::cout << "Creating subfield:" << buf << std::endl;
      }
    }
   }

  // Linearize fields.  Use the ordering implied by MEField, since this is the order
  // kernel commit uses later.
 for (UInt i = 0; i < Fields.size(); i++) {
    MEField<> *meF = static_cast<MEField<>*>(Fields[i]);
    meF->Getfields(fields); // will be unique fields
 }

 nfields = fields.size();
 {
   for (UInt i = 0; i < nfields; i++) 
     fields[i]->set_ordinal(i);
 }

  is_committed = true;
}


IOField<NodalField> *FieldReg::RegisterNodalField(const MeshDB &mesh, const std::string &name, UInt dim) {
  ndfields.push_back(new IOField<NodalField>(mesh, name, dim));
  return ndfields.back();
}

IOField<ElementField> *FieldReg::RegisterElementField(const MeshDB &mesh, const std::string &name, UInt dim) {
  efields.push_back(new IOField<ElementField>(mesh, name, dim));
  return efields.back();
}

} // namespace
} // namespace
