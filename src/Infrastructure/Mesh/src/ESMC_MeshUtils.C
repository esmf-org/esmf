// $Id: ESMC_MeshUtils.C,v 1.3.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
#include <ESMC_MeshUtils.h>

#include <ESMC_MeshObjTopo.h>
#include <ESMC_Mapping.h>
#include <ESMC_MeshObjConn.h>
#include <sacado/Sacado.hpp>
#include <ESMC_MEValues.h>
#include <ESMC_MeshField.h>

#include <iostream>
#include <iomanip>

namespace ESMCI {
namespace MESH {

MasterElementBase &GetME(const MEFieldBase &field, const MeshObj&obj) {
 MasterElementBase *me = obj.GetKernel()->GetME(field.GetOrdinal());
 ThrowRequire(me);
 return *me;
}

MasterElementBase &GetME(const MEFieldBase &field, const Kernel &ker) {
 MasterElementBase *me = ker.GetME(field.GetOrdinal());
 ThrowRequire(me);
 return *me;
}

const MappingBase &GetMapping(const MeshObj &obj) {
  return obj.GetKernel()->GetMapping();
}

const intgRule *GetIntg(const MeshObj &obj) {
  return obj.GetKernel()->GetIntg();
}

template<typename METRAITS, typename FTYPE>
void GatherElemData(const MasterElement<METRAITS> &me, const FTYPE &f,
     const MeshObj &obj, typename METRAITS::field_type res[]) 
{

  // Make sure meptr and field are the same
  ThrowAssert(me(METraits<>()) == f.GetMEFamily().getME(GetMeshObjTopo(obj)->name));

  UInt fdim = f.dim();
  // Gather from the nodes
  if (me.is_nodal()) {
    for (UInt n = 0; n < me.num_functions(); n++) {
      const MeshObj &node = *obj.Relations[n].obj;
      for (UInt d = 0; d < f.dim(); d++) {
        res[n*fdim+d] = ((typename METRAITS::field_type*)f.data(node))[d];
      } // dim
    } // for nfunc
  } else {
    const int *ddl = me.GetDofDescription(0);
    MeshObjRelationList::const_iterator ri = 
      MeshObjConn::find_relation(obj, dof2mtype(ddl[0]), ddl[1]);
    ThrowRequire(ri != obj.Relations.end());
    const int *dd;
    const MeshObj *gobj = ri->obj;
    UInt n = 0;
    bool pol = ri->polarity;
    int rot = ri->rotation;
    for (UInt i = 0; i < me.num_functions(); i++) {
      dd = me.GetDofDescription(i);
      if ((dd[1] != ddl[1]) || (dd[0] != ddl[0])) {
        if (dof2mtype(dd[0]) != MeshObj::ELEMENT) {
          ri = MeshObjConn::find_relation(obj, dof2mtype(dd[0]), dd[1]);
          if (ri == obj.Relations.end())
            Throw() << "Could not get object:" << MeshObjTypeString(dof2mtype(dd[0])) << ", ord:" << dd[1]
                    << ", obj=" << obj;
          gobj = ri->obj; pol = ri->polarity; rot = ri->rotation;
        } else {
           gobj = &obj; pol = true; rot = 0; // don't need topo
        }
        ddl = dd;
      }

      UInt nval = me.GetDofValSet(i);
      const typename FTYPE::field_type &llf = *f.Getfield(nval);

      if (dd[0] == DOF_NODE || dd[0] == DOF_ELEM || nval == 1 ) {
        // no polarity/rotation to worry about
        for (UInt d = 0; d < fdim; d++) {
          res[n++] = ((typename METRAITS::field_type*)llf.data(*gobj))[dd[2]*fdim+d];
        }
      } else {
        //int idx = pol ? (dd[2]-rot) % nval : (nval -1 - dd[2] + rot) % nval ;
        for (UInt d = 0; d < fdim; d++) {
          res[n++] = pol ? ((typename METRAITS::field_type*)llf.data(*gobj))[dd[2]*fdim+d] :
                     dd[3]*((typename METRAITS::field_type*)llf.data(*gobj))[dd[2]*fdim+d];
        }
      }
    } // i
  } // is nodal
}

template void GatherElemData(const MasterElement<METraits<> > &me, const MEField<> &f,
     const MeshObj &obj, METraits<>::field_type res[]);
template void GatherElemData(const MasterElement<METraits<fad_type,double> > &me, const MEField<SField> &f,
     const MeshObj &obj, METraits<fad_type,double>::field_type res[]);


// Scatter data to element.  Change polarity as necessary
template<typename METRAITS, typename FTYPE>
void ScatterElemData(const MasterElement<METRAITS> &me, const FTYPE &f,
     const MeshObj &obj, const double mcoef[]) 
{

  // Make sure meptr and field are the same
  ThrowAssert(me(METraits<>()) == f.GetMEFamily().getME(GetMeshObjTopo(obj)->name));

  UInt fdim = f.dim();
  // Gather from the nodes
  if (me.is_nodal()) {
    for (UInt n = 0; n < me.num_functions(); n++) {
      const MeshObj &node = *obj.Relations[n].obj;
      for (UInt d = 0; d < f.dim(); d++) {
        ((typename METRAITS::field_type*)f.data(node))[d] = mcoef[n*fdim+d];
      } // dim
    } // for nfunc
  } else {
    const int *ddl = me.GetDofDescription(0);
    MeshObjRelationList::const_iterator ri = 
      MeshObjConn::find_relation(obj, dof2mtype(ddl[0]), ddl[1]);
    ThrowRequire(ri != obj.Relations.end());
    const int *dd;
    const MeshObj *gobj = ri->obj;
    UInt n = 0;
    bool pol = ri->polarity;
    int rot = ri->rotation;
    for (UInt i = 0; i < me.num_functions(); i++) {
      dd = me.GetDofDescription(i);
      if ((dd[1] != ddl[1]) || (dd[0] != ddl[0])) {
        if (dof2mtype(dd[0]) != MeshObj::ELEMENT) {
          ri = MeshObjConn::find_relation(obj, dof2mtype(dd[0]), dd[1]);
          if (ri == obj.Relations.end())
            Throw() << "Could not get object:" << MeshObjTypeString(dof2mtype(dd[0])) << ", ord:" << dd[1]
                    << ", obj=" << obj;
          gobj = ri->obj; pol = ri->polarity; rot = ri->rotation;
        } else {
           gobj = &obj; pol = true; rot = 0; // don't need topo
        }
        ddl = dd;
      }

      UInt nval = me.GetDofValSet(i);
      const typename FTYPE::field_type &llf = *f.Getfield(nval);

      if (dd[0] == DOF_NODE || dd[0] == DOF_ELEM || nval == 1 ) {
        // no polarity/rotation to worry about
        for (UInt d = 0; d < fdim; d++) {
          ((typename METRAITS::field_type*)llf.data(*gobj))[dd[2]*fdim+d] = mcoef[n++];
        }
      } else {
        //int idx = pol ? (dd[2]-rot) % nval : (nval -1 - dd[2] + rot) % nval ;
        for (UInt d = 0; d < fdim; d++) {
          ((typename METRAITS::field_type*)llf.data(*gobj))[dd[2]*fdim+d] = pol ? mcoef[n++] : dd[3]*mcoef[n++];
        }
      }
    } // i
  } // is nodal

}

template void ScatterElemData(const MasterElement<METraits<> > &me, const MEField<> &f,
     const MeshObj &obj, const double mecoef[]);


// results(npe,fdim)
template<typename FIELD, typename Real>
void getNodeData(const FIELD &nfield, const MeshObj &elem, Real result[]) {
  UInt npe = GetMeshObjTopo(elem)->num_nodes;
  UInt fdim = nfield.dim();
  for (UInt n = 0; n < npe; n++) {
    const MeshObj &node = *(elem.Relations[n].obj);
    const Real *fd = nfield.data(node);
    for (UInt d = 0; d < fdim; d++) {
      result[n*fdim+d] = fd[d];
    }
   }
}
//template void getNodeData<>(const Field<NodalField> &nfield, const MeshObj &elem, double result[]);
//template void getNodeData<>(const SField &nfield, const MeshObj &elem, Sacado::Fad::DFad<double> result[]);

template<typename FIELD>
void elemCentroid(const FIELD &nfield, const MeshObj &elem, double result[]) {
  UInt sdim = GetMeshObjTopo(elem)->spatial_dim;
  for (UInt d = 0; d < sdim; d++) {
    result[d] = 0.0;
  }

  UInt npe = GetMeshObjTopo(elem)->num_nodes;
  for (UInt n = 0; n < npe; n++) {
    const MeshObj &node = *(elem.Relations[n].obj);
    const double *fd = nfield.data(node);
    for (UInt d = 0; d < sdim; d++) {
      result[d] += fd[d];
    }
   }

  double OneOvN = 1.0/GetMeshObjTopo(elem)->num_nodes;
  for (UInt d = 0; d < sdim; d++) {
    result[d] *= OneOvN;
  }
}
template void elemCentroid<>(const MEField<> &nfield, const MeshObj &elem, double result[]);

// DEPREC
/*
void getMeshCoords(const MeshDB &mesh, std::vector<double> &x, std::vector<double> &y, std::vector<double> &z) {

 int nnodes = mesh.num_nodes();
//std::cout << "nnodes=" << nnodes << std::endl;
 x.resize(nnodes); y.resize(nnodes); z.resize(nnodes);
 Field<NodalField> *cfield = get_coord_field(mesh);
 if (!cfield) Throw() << "MeshUTil getMeshCoord.  No coord field!!!";
 UInt dim = mesh.spatial_dim();

 int i = 0;
 MeshDB::const_iterator mit = mesh.node_begin(), mite = mesh.node_end();
//std::cout << "mit=" << (*mit)->get_id() << ", mite=" << (*mite)->get_id() << std::endl;
 for (; mit != mite; ++mit) {
   const MeshObj &node = (*mit);
   double *coord = cfield->data(node);
//std::cout << "coords:" << coord[0] << ", " << coord[1] << std::endl;
   x[i] = coord[0]; y[i] = coord[1];
   if (dim == 3) z[i] = coord[2]; else z[i] = 0.0;
   i++;
 }
}
*/

void GetMeshCoords(const Mesh &mesh, std::vector<double> &x, std::vector<double> &y, std::vector<double> &z) {

 int nnodes = mesh.num_nodes();
//std::cout << "nnodes=" << nnodes << std::endl;
 x.resize(nnodes); y.resize(nnodes); z.resize(nnodes);
 MEField<> *cfield = mesh.GetCoordField();
 if (!cfield) Throw() << "MeshUTil GetMeshCoord.  No coord field!!!";

 // TODO: implement using mevalues??
 if (cfield->GetMEFamily().is_nodal()) {
   _field *cf = cfield->GetNodalfield();

   UInt dim = mesh.spatial_dim();
  
   int i = 0;
   MeshDB::const_iterator mit = mesh.node_begin(), mite = mesh.node_end();
  //std::cout << "mit=" << (*mit)->get_id() << ", mite=" << (*mite)->get_id() << std::endl;
   for (; mit != mite; ++mit) {
     const MeshObj &node = (*mit);
     double *coord = cf->data(node);
  //std::cout << "coords:" << coord[0] << ", " << coord[1] << std::endl;
     x[i] = coord[0]; y[i] = coord[1];
     if (dim == 3) z[i] = coord[2]; else z[i] = 0.0;
     i++;
   }

 } else Throw() << "GetMeshCoords for non nodal coord field not yet implemented!! MEfamily=" << cfield->GetMEFamily().name();

}

UInt numDecimal(UInt n) {
  UInt a = 10;

  UInt res = 1;

  if (n == 0) return 1;

  // Find smalled number so n / a > 1
  while (n / a > 0) {
    res++;
    a *= 10;
  }

  return res;

}

MCoord *getMCoordNode(const MEField<> &nfield, const MeshObj &node) {
  if (node.get_type() != MeshObj::NODE) Throw() << "getMcoord, MeshObj not node!";


  // Get the element around
  std::set<MeshObj*> elems;
  // Get the patch of elements
  MeshObjRelationList::const_iterator el = MeshObjConn::find_relation(node, MeshObj::ELEMENT);
  
  // Stack elements in
  while (el != node.Relations.end() && el->obj->get_type() == MeshObj::ELEMENT){
    elems.insert(el->obj);
    el++;
  } 

  UInt num_elems = elems.size();

  const MeshDB &mesh = GetMeshObjMesh(node);
  UInt sdim = mesh.spatial_dim();
  UInt pdim = mesh.parametric_dim();
  double pc[] = {0,0,0};
  arbq pintg(pdim, 1, pc);
  std::vector<double> n(sdim,0);
  std::vector<double> tn(sdim,0);
  UInt e = 0;
  std::set<MeshObj*>::iterator ei = elems.begin(), ee = elems.end();
  for (; ei != ee; ++ei) {
    const MeshObj &elem = **ei;
    MEValues<> mev(nfield.GetMEFamily(), nfield);
    mev.Setup(*elem.GetKernel(), 0, &pintg);
    mev.ReInit(elem);
    mev.GetNormals(&tn[0]);
    for (UInt i = 0; i < sdim; i++) n[i] += tn[i];
    e++;
  }

  double o_ov_ne = 1.0/num_elems;
  for (UInt i = 0; i < sdim; i++) n[i] *= o_ov_ne; // normalize

  
  double *ct = nfield.data(node);
//std::cout << "Center:"; std::copy(&ct[0], &ct[3], std::ostream_iterator<double>(std::cout, " ")); std::cout << std::endl;

  return new MCoord(ct, &n[0]);
}

void setToAverage(const MeshObj &node, const MEField<> &field) {
 std::set<MeshObj*> elems;
  // Get the patch of elements
  elems.clear();
  MeshObjRelationList::const_iterator el = MeshObjConn::find_relation(node, MeshObj::ELEMENT);

  // Stack elements in
  while (el != node.Relations.end() && el->obj->get_type() == MeshObj::ELEMENT){
    elems.insert(el->obj);
    el++;
  }

  // Figure out the number of samples.  Even though this is an extra loop,
  // it saves transposing matrices, rhs later;
  std::set<MeshObj*>::iterator ei = elems.begin(), ee = elems.end();
  std::set<const MeshObj*> nodes;
  for (; ei != ee; ++ei) {
    const MeshObj &selem = **ei;
    MasterElementBase &meb = GetME(field, selem);
    MasterElement<> &me = *meb(METraits<>());
    // Loop the nodes as described by the rme
    for (UInt i = 0; i < me.num_functions(); i++) {
      const MeshObj &snode = *selem.Relations[i].obj;
      nodes.insert(&snode);
    }
  }
  nodes.erase(&node);
  UInt nsamples = nodes.size();
  if (nsamples == 0) Throw() << "Nsamples = 0 in setToAverage!";

  double o_ov_nn = 1.0/nsamples;
  double *data = field.data(node);
  for (UInt f = 0; f < field.dim(); f++) {
    data[f] = 0.0;
    std::set<const MeshObj*>::iterator si = nodes.begin(), se = nodes.end();
    for (; si != se; ++si) {
      double *fd = field.data(**si);
      data[f] += fd[f];
    }
    data[f] *= o_ov_nn;
  }

}

UInt dof2mtype(UInt dof) {
  switch (dof) {
    case DOF_NODE:
      return MeshObj::NODE;
    case DOF_EDGE:
      return MeshObj::EDGE;
    case DOF_FACE:
      return MeshObj::FACE;
    case DOF_ELEM:
      return MeshObj::ELEMENT;
    default:
      Throw() << " bad dof type:" << dof;
  }
}

} //namespace
} //namespace
