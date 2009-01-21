// $Id: ESMC_FieldReg.C,v 1.5.2.2 2009/01/21 21:25:22 cdeluca Exp $
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
#include <ESMC_FieldReg.h>
#include <ESMC_MEImprint.h>
#include <ESMC_ParEnv.h>

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
        UInt obj_type, const Context &ctxt, UInt dim, bool out, bool interp, const _fieldTypeBase &ftype)
{

  if (is_committed)
    Throw() << "FieldReg is already committed when registering:" << name;

  FMapType::iterator fi = fmap.lower_bound(name);

  MEField<> *nf;
  if (fi == fmap.end() || fi->first != name) {
    // Create the new field
    nf = new MEField<>(name, mef, obj_type, ctxt, dim, out, interp, ftype);
    fmap.insert(fi, std::make_pair(name, nf));
    Fields.push_back(nf);
  } else {
    // Check. If specs match, then just return the fields, otherwise
    // throw error.
    nf = fi->second;
    
    // TODO: a more thourough check of specs (context, etc...)
    if (obj_type != nf->ObjType() || dim != nf->dim())
      Throw() << "MEField name:" << name << " already registered, with different specs";
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

_field *FieldReg::Getfield(const std::string &fname) const {
  fMapType::const_iterator fi = _fmap.find(fname);
  
  return fi == _fmap.end() ? NULL : fi->second;
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
  UInt nnodes = mesh.num_nodes();

  if (nnodes == 0) return;

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

// Union the two sets in parallel
static void parallel_union_field_info(std::vector<UInt> &nvalSet, std::vector<UInt> &nvalSetObj) {

  UInt csize = Par::Size();

  if (csize == 1) return; 

  std::vector<UInt> nvs(nvalSet);
  std::vector<UInt> nvso(nvalSetObj);


  // First share how many each processor wishes to send
  std::vector<int> num_val(csize, 0);
  UInt num_val_l = nvalSet.size();

  MPI_Allgather(&num_val_l, 1, MPI_UNSIGNED, &num_val[0], 1, MPI_UNSIGNED, Par::Comm());

  std::vector<int> rdisp(csize+1, 0);
  for (UInt i = 0; i < csize; i++) {
    rdisp[i+1] = rdisp[i] + num_val[i];
  }

  std::vector<UInt> allval(rdisp[csize], 0);

  MPI_Allgatherv(&nvs[0], nvs.size(), MPI_UNSIGNED, &allval[0],
      &num_val[0], &rdisp[0], MPI_UNSIGNED, Par::Comm());

  // Now send out the valsetobj
  // First copy to a contiguous buffer
  std::vector<UInt> contig_nvso(nvs.size(), 0);
  for (UInt i = 0; i < nvs.size(); i++)
    contig_nvso[i] = nvso[nvs[i]];

  std::vector<UInt> allvalo(rdisp[csize], 0);

  MPI_Allgatherv(&contig_nvso[0], nvs.size(), MPI_UNSIGNED, &allvalo[0],
      &num_val[0], &rdisp[0], MPI_UNSIGNED, Par::Comm());

  // Loop through results
  for (UInt i = 0; i < (UInt) rdisp[csize]; i++) {
    std::vector<UInt>::iterator lb = 
      std::lower_bound(nvs.begin(), nvs.end(), allval[i]);

    if (lb == nvs.end() || *lb != allval[i])
      nvs.insert(lb, allval[i]);

    if (allval[i] >= nvso.size()) nvso.resize(allval[i]+1);

    nvso[allval[i]] |= allvalo[i];

  }

  // Finally, update the return vals
  nvalSet = nvs;
  nvalSetObj = nvso;
  
}

void FieldReg::Commit(MeshDB &mesh) {
  Trace __trace("FieldReg::Commit(MeshDB &mesh)");

  // Step 0: Get the imprint contexts that will be used; share in parallel
  // and define these contexts.
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
            MEImprintValSets(f.name(), *oi, me, nvalSet, nvalSetObj);
            oi = on;
          } // oi
        }
  
        ki = kn;
      } // for k

      // Must parallel union nvalSet and nvalSetObj
      parallel_union_field_info(nvalSet, nvalSetObj);

      // Register low level fields and define the contexts
      for (UInt i = 0; i < nvalSet.size(); i++) {
        UInt nval = nvalSet[i];
        Context ctxt;
        char buf[1024];
        std::sprintf(buf, "%s_%d", f.name().c_str(), nval);
        UInt c_id = mesh.DefineContext(buf);
        ctxt.set(c_id);
        Attr fatt(nvalSetObj[nval], ctxt);
        f.Addfield(Registerfield(buf, fatt, f.FType(), nval*f.dim()), nval);
//std::cout << "Creating subfield:" << buf << std::endl;
      }
      
      // If field is to be interpolated, register an interpolant field.  This should be parallel
      // safe.
      if (f.has_interp()) {
        
        // IF the field is nodal, we will simply use the nodal field for interpolations,
        // otherwise we must create a special field for interpolation.
        if (!f.is_nodal()) {
          char buf[1024];
          std::sprintf(buf, "_interp_%s", f.name().c_str());
          UInt c_id = mesh.DefineContext(buf);
          Context ctxt;
          ctxt.set(c_id);
          Attr fatt(MeshObj::NODE | MeshObj::INTERP, ctxt);
          f.SetInterp(Registerfield(buf, fatt, f.FType(), f.dim()));
        } else {
          f.SetInterp(f.GetNodalfield());
        }
      }
      
    } // for fi
   }




  // Step 1: Imprint the objects for low level fields
  {
    FMapType::iterator fi = fmap.begin(), fe = fmap.end();
    for ( ;fi !=fe; ++fi) {
      MEField<> &f = *fi->second;
//std::cout << "Imprinting MEField:" << f.name() << std::endl;
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
            MEImprint(f.name(), *oi, me);
            oi = on;
          } // oi
        }
  
        ki = kn;
      } // for k

    } // for f
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
 
 // Loop through _fields; there may be some that are not associated with MEFields,
 // so we need to count and number these.
 {
  fMapType::iterator fi = _fmap.begin(), fe = _fmap.end();
  
  for (; fi != fe; ++fi) {
    _field *_f = fi->second;
    
    if (_f->GetOrdinal() < 0) {
      _f->set_ordinal(nfields++);
      fields.push_back(_f);
    }
  }
  
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
