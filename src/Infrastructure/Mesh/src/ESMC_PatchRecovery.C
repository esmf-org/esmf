// $Id: ESMC_PatchRecovery.C,v 1.6.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
#include <ESMC_PatchRecovery.h>
#include <ESMC_Exception.h>
#include <ESMC_MeshObjConn.h>
#include <ESMC_MeshUtils.h>
#include <ESMC_MEValues.h>
#include <ESMC_Polynomial.h>
#include <ESMC_MeshField.h>


#include <set>
#include <iostream>
#include <iterator>
#include <iomanip>
#include <cmath>

#include <sacado/Sacado.hpp>
#include <cstdlib>



#ifdef ESMC_LAPACK
extern "C" void FTN(dgelsy)(int *,int *,int*,double*,int*,double*,int*,int*,double*,int*,double*,int*,int*);
#endif
          

namespace ESMCI {
namespace MESH {

template<typename NFIELD, typename Real>
UInt PatchRecov<NFIELD,Real>::get_ncoeff(UInt dim, UInt deg) {

  UInt res = 1;
  for (UInt i = 0; i < dim; i++) 
    res *= (deg+1);
  return res;
}

template<typename NFIELD, typename Real>
void PatchRecov<NFIELD,Real>::eval_poly(UInt nsamples, UInt sample, UInt ldb, UInt cur_rhs, 
          UInt dim, std::vector<double> &mat, double coord[],
                          std::vector<Real> &rhs, const Real fvals[], UInt fdim) {
  if (pdeg == 0) {
    mat[0*nsamples + sample] = 1;
  } else if (dim == 2) {
    UInt k = 0;
    for (UInt i = 0; i <= pdeg; i++) {
      for (UInt j = 0; j <= pdeg; j++) {
        Monomial<double> m1(i), m2(j);
        TensorPolynomial<Monomial<double> > tp(&m1, &m2);
//std::cout << "x = " << coord[0] << ", y = " << coord[1] << ", i,j=" << i << ", " << j << ", val=" <<
     //EvalTensorPoly(dim, coord, tp) << std::endl;
        mat[k++*nsamples + sample] = 
          EvalTensorPoly(dim, coord, tp);
      }
    }
  } else if (dim == 3) {
    UInt l = 0;
    for (UInt i = 0; i <= pdeg; i++) {
      for (UInt j = 0; j <= pdeg; j++) {
        for (UInt k = 0; k <= pdeg; k++) {
          Monomial<double> m1(i), m2(j), m3(k);
          TensorPolynomial<Monomial<double> > tp(&m1, &m2, &m3);
          mat[l++*nsamples + sample] = 
            EvalTensorPoly(dim, coord, tp);
        }
      }
    }
  } else Throw() << "eval poly wrong dim=" << dim << ", deg=" << pdeg;

  // rhs
  for (UInt i = 0; i < fdim; i++) {
    rhs[(cur_rhs+i)*ldb+sample] = fvals[i];
//std::cout << "eq:i=" << i << ", fval[i]=" << fvals[i] << std::endl;
  }
}

template<typename NFIELD, typename Real>
PatchRecov<NFIELD,Real>::PatchRecov(const PatchRecov &rhs) :
pdeg(rhs.pdeg),
idim(rhs.idim),
ncoeff(rhs.ncoeff),
patch_ok(rhs.patch_ok),
coeff(rhs.coeff),
mc(rhs.mc)
{
}

template<typename NFIELD, typename Real>
PatchRecov<NFIELD,Real> &PatchRecov<NFIELD,Real>::operator=(const PatchRecov &rhs) {
  if (this == &rhs) return *this;
  pdeg = rhs.pdeg;
  idim=rhs.idim;
  ncoeff = rhs.ncoeff;
  patch_ok=rhs.patch_ok;
  coeff=rhs.coeff;
  mc = rhs.mc;
  return *this;
}

template<typename NFIELD, typename Real>
PatchRecov<NFIELD,Real>::PatchRecov() :
idim(0),
ncoeff(0),
patch_ok(false),
coeff()
{
}

template<typename NFIELD, typename Real>
void PatchRecov<NFIELD,Real>::CreatePatch(
           UInt _pdeg,
           const MeshDB &mesh,
           const MeshObj &node,
           UInt numfields,
           NFIELD **rfield,
           UInt threshold,           
           const MEField<> &coord,
           const MCoord *_mc
           ) 
{
#ifdef ESMC_LAPACK
  patch_ok = true; // used below
  pdeg = _pdeg;
  mc = _mc;
  UInt pdim = mesh.parametric_dim();
  UInt sdim = mesh.spatial_dim();

  if (coord.dim() != sdim)
    Throw() << "Patch:sdim=" << sdim << ", but fdim=" << coord.dim();

  if (mc) {
    if (mc->ManifoldDim() != pdim) Throw() << "MCoord manifold dim not = pdim";
    idim = pdim;
  } else idim = sdim;

  UInt ncoef = get_ncoeff(idim, pdeg);
  ncoeff = ncoef;
  std::vector<double> mat;
  std::vector<Real> rhs;

  std::set<const MeshObj*> elems;
  MeshObjRelationList::const_iterator el = MeshObjConn::find_relation(node, MeshObj::ELEMENT);

  // Stack elements in
  // Use integration rule for field 0
  UInt nsamples = 0;
  while (el != node.Relations.end() && el->obj->get_type() == MeshObj::ELEMENT){
    elems.insert(el->obj);
    const MeshObj &selem = *el->obj;
    MasterElement<METraits<Real> > *me = GetME(*rfield[0], selem)(METraits<Real>());
    const intgRule *ir = GetIntg(selem)->ChangeOrder(2);
    if (ir == NULL) Throw() << "Patch, no intg rule for me:" << me->name;
    nsamples += ir->npoints();
    el++;
  }

//std::cout << "nsamples=" << nsamples << std::endl;
/*
  if (nsamples < threshold) {
    patch_ok = false;
    return;
  }
*/

  

  if (elems.size() == 0) Throw() << "PatchRecovery, found no elements, node:" << node.get_id();

  // Loop elements, subloop intg points.
  // evaluate matrix and rhs;
  int m = nsamples, n = ncoef, nrhs = 0, info=0, rank, ldb;
  ldb = std::max(std::max(m,n),1);
  mat.resize(nsamples*ncoef);
  for (UInt f = 0; f < numfields; f++) {
  const NFIELD &field = *rfield[f];
  nrhs += field.dim();
  }
  rhs.resize(ldb*nrhs);

  // size coeff
  coeff.resize(nrhs*ncoef, 0);

  // If nsamples not the right amount, use a constant interpolant
  if ((UInt)std::abs((int)(nsamples - ncoef)) > threshold) {
std::cout << "threshold  tripped.  nsamples=" << nsamples << ", ncoef=" << ncoeff << std::endl;
    patch_ok = false;
    return;
  }

  UInt cur_rhs = 0;
  for (UInt f = 0; f < numfields; f++) {
    const NFIELD &field = *rfield[f];
  
    // subloop elements
    std::set<const MeshObj*>::iterator ei = elems.begin(), ee = elems.end();
    UInt sample = 0;
    std::vector<double> cdata(idim);
    for (; ei != ee; ++ei) {
      // Loop Gauss points
      const MeshObj &selem = **ei;
      // Gather field data for element.
      MEValues<METraits<Real,double>, METraits<>, NFIELD> mev(field.GetMEFamily(), coord);
      const intgRule *ir = GetIntg(selem)->ChangeOrder(2);
      mev.Setup(selem, 0, ir);
      mev.ReInit(selem);
     
      std::vector<Real> felem(field.dim()*mev.GetNumFunctions());
      std::vector<double> celem(sdim*mev.GetNumCoordFunctions());

      std::vector<double> cdatas(sdim*mev.GetNQPoints());
      std::vector<Real> fdata(field.dim()*mev.GetNQPoints());
      mev.GetCoordinateValues(&cdatas[0]);
      mev.GetFunctionValues(field, &fdata[0]);
      for (UInt q = 0; q < mev.GetNQPoints(); q++) {
        double *cd = &cdatas[q*sdim]; // default here
        if (mc) {
          mc->Transform(&cdatas[q*sdim], &cdata[0]);
          cd = &cdata[0]; // point to transformed point
        }
      
        eval_poly(nsamples, sample++, ldb, cur_rhs,
              idim, mat, cd, rhs, &fdata[q*field.dim()], field.dim());
      } // for qpoint
    } // for elem
      cur_rhs += field.dim();
  } // f

/*
std::cout << "mat:" << std::endl;
for (UInt i = 0; i < nsamples; i++) {
for (UInt j = 0; j < ncoef; j++) {
  std::cout << std::setw(8) << std::setprecision(4) <<  mat[j*nsamples+i] << " ";
}
std::cout << std::endl;
}
*/

#ifdef RHS
std::cout << "rhs:" << std::endl;
for (UInt s = 0; s < (UInt) nrhs; s++) {
std::copy(&rhs[s*ldb], &rhs[s*ldb+nsamples], std::ostream_iterator<Real>(std::cout, ", ")); std::cout << std::endl;
std::cout << std::endl;
}
#endif

  std::vector<int> jpvt(ncoef, 0);
  //int lwork = std::max(std::min(m,n)+2*n+1, 2*std::min(m,n)+nrhs);
  // TODO figure this out
  int lwork = 4*4028;
  std::vector<double> work(lwork, 0);
  double rcond=0.0000000000001;

#ifdef RESIDUALS
std::vector<double> saverhs = rhs;
std::vector<double> matsav = mat;
#endif

#ifdef OLDWAY
  FTN(dgelsy)(
    &m, &n, &nrhs, &mat[0], &m, &rhs[0], &ldb, &jpvt[0], &rcond, &rank, &work[0], &lwork, &info);
#else
  std::vector<double> id_rhs(ldb*ldb, 0);
  // Set up B=I
  for (int i = 0; i < m; i++) id_rhs[i*ldb + i] = 1.0;

  FTN(dgelsy)(
    &m, &n, &m, &mat[0], &m, &id_rhs[0], &ldb, &jpvt[0], &rcond, &rank, &work[0], &lwork, &info);
#ifdef RESIDUALS
std::cout << "A(" << m << "," << n << ")=" << std::endl;
for (UInt i = 0; i < m; i++) {
for (UInt j = 0; j < n; j++) {
std::cout << std::setw(10) << matsav[j*nsamples+i] << " ";
}
std::cout << std::endl;
}
std::cout << "Pinv(A)=" << std::endl;
for (UInt i = 0; i < n; i++) {
for (UInt j = 0; j < m; j++) {
std::cout << std::setw(10) << id_rhs[j*ldb+i] << " ";
}
std::cout << std::endl;
}
//std::cout << "jpvt:";
//std::copy(jpvt.begin(), jpvt.end(), std::ostream_iterator<int>(std::cout, " "));
#endif

// Apply the pseudo inverse
  std::vector<Real> b = rhs; // ughh
  for (int i = 0; i < n; i++) {
    for (int nr = 0; nr < nrhs; nr++) rhs[nr*ldb+i] = 0;
    for (int j = 0; j < m; j++) {
      for (int nr = 0; nr < nrhs; nr++) {
         rhs[nr*ldb+i] += id_rhs[j*ldb+i]*b[nr*ldb+j];
      }
    }
  }

#endif

//std::cout << "info=" << info << std::endl;

  // copy data into the field
  //std::copy(rhs.begin(), rhs.end(), coef.data(node));
/*
std::cout << "answer=" << std::endl;
std::copy(rhs.begin(), rhs.end(), std::ostream_iterator<double>(std::cout, ", ")); std::cout << std::endl;
*/

#ifdef RESIDUALS
// Wierd ; rhs comes back (Nxnrhs), so no ldb, just nsamples
std::cout << "rcond=" << rcond << std::endl;
std::vector<double> tst(nsamples,0);
double err = 0;
for (UInt s = 0; s < (UInt) nrhs; s++) {
std::cout << "A*x=" << std::endl;
    for (UInt i = 0; i < nsamples; i++) {
      tst[i] = 0;
      for (UInt j = 0; j < ncoef; j++) {
        tst[i] += matsav[j*nsamples + i]*rhs[s*ldb+j];
      }
std::cout << tst[i] << " ";
      err += (tst[i]-saverhs[s*ldb+i])*(tst[i]-saverhs[s*ldb+i]);
    }
std::cout << std::endl;
}
std::cout << "err=" << err << std::endl;
if (err > 1e-10) std::cout << "\tnsamples=" << nsamples << std::endl;
#endif
/*
std::cout << "A * answerr=" << std::endl;
std::copy(tst.begin(), tst.end(), std::ostream_iterator<double>(std::cout, ", ")); std::cout << std::endl;
*/

  // fill return array
  for (UInt r = 0; r < (UInt) nrhs; r++) {
    for (UInt c = 0; c < ncoef; c++) {
      coeff[r*ncoef+c] = rhs[r*ldb+c];
    }
  }
  //std::copy(&rhs[0], &rhs[nrhs*ncoef], coeff.begin());

  patch_ok = true;
#endif
}

template<typename NFIELD, typename Real>
Real PatchRecov<NFIELD,Real>::EvalPatch(UInt nfield, const double coords[]) const {
  const double *cd = coords;
  std::vector<double> mcoords(mc ? mc->ManifoldDim() : 0);
  if (mc) {
    mc->Transform(coords, &mcoords[0]);
    cd = &mcoords[0];
  }
  const Real *coef = &coeff[nfield*ncoeff];
  if (pdeg == 0) {
    return coef[0];
  } else if (idim == 2) {
    Real res = 0;
    UInt k = 0;
    for (UInt i = 0; i <= pdeg; i++) {
      for (UInt j = 0; j <= pdeg; j++) {
        Monomial<double> m1(i), m2(j);
        TensorPolynomial<Monomial<double> > tp(&m1, &m2);
        res += coef[k++]*EvalTensorPoly(idim, cd, tp);
      }
    }
    return res;
  } else if (idim == 3) {
    Real res = 0;
    UInt l = 0;
    for (UInt i = 0; i <= pdeg; i++) {
      for (UInt j = 0; j <= pdeg; j++) {
        for (UInt k = 0; k <= pdeg; k++) {
          Monomial<double> m1(i), m2(j), m3(k);
          TensorPolynomial<Monomial<double> > tp(&m1, &m2, &m3);
          res += coef[l++]*EvalTensorPoly(idim, cd, tp);
        }
      }
    }
    return res;
  } else Throw() << "dim, deg=" << idim << ", " << pdeg << " not yet supported";
}



// ******* ElemPatch ************
template <typename NFIELD, typename Real>
ElemPatch<NFIELD,Real>::ElemPatch() :
patches(),
mcs(),
pelem(NULL),
pmesh(NULL),
pcfield(NULL),
flen(0)
{
}

template <typename NFIELD, typename Real>
ElemPatch<NFIELD,Real>::~ElemPatch() 
{
  for (UInt n = 0; n < mcs.size(); n++) 
    delete mcs[n];
}

template <typename NFIELD, typename Real>
void ElemPatch<NFIELD,Real>::CreateElemPatch(UInt pdeg,
           UInt ptype,
           const MeshObj &elem,  // the elem in question
           const MEField<> &cfield, 
           UInt numfields,
           NFIELD **rfield,
           UInt threshold)       // How far from num dofs to invalidate.  If the
{
  // Set some things up
  pmesh = &GetMeshObjMesh(elem);
  pcfield = &cfield;
  flen = 0;
  pelem = &elem;
  // total number of field dimensions
  for (UInt i = 0; i < numfields; i++) 
    flen += rfield[i]->dim();

  bool use_mc = (pcfield->dim() > (UInt)pmesh->parametric_dim());


  const MeshObjTopo *topo = GetMeshObjTopo(elem);
  UInt nv = topo->num_vertices;

  patches.resize(nv);
  if (use_mc)
    mcs.resize(nv, NULL);

  // Loop vertices creating the appropriate patch
  std::vector<MCoord *> mcs(nv, NULL);
  for (UInt n = 0; n < nv; n++) {
    const MeshObj &node = *elem.Relations[n].obj;
    if (use_mc) mcs[n] = getMCoordNode(*pcfield, node);

    if (GetMeshObjContext(node).is_set(Attr::EXPOSED_BOUNDARY_ID)) {
      patches[n].MarkPatchBad();
//std::cout << "node:" << node.get_id() << " is boundary!" << std::endl;
    } else {
      if (ptype == NODAL_PATCH) {
        // TODO, fix as nodal integration rule
        patches[n].CreatePatch(pdeg, *pmesh, node, numfields, rfield,
                         700, *pcfield, mcs[n]);
      } else {
        patches[n].CreatePatch(pdeg, *pmesh, node, numfields, rfield,
                         700, *pcfield, mcs[n]);
      }
    }
  } // for nv


  // Resolve bad (boundary) patches
  UInt nok = 0;
  UInt ok_idx = 0;
  for (UInt n = 0; n < nv; n++) {
    nok += patches[n].PatchOk() ? 1 : 0;
    if (patches[n].PatchOk()) ok_idx = n;
  }
  if (nok == 0) Throw() << "Transfer, found no ok nodes";
//std::cout << "nok=" << nok << " , ok_idx=" << ok_idx<< std::endl;

  // do the copying
  for (UInt n = 0; n < nv; n++) {
    if (patches[n].PatchOk() == false) {
      patches[n] = patches[ok_idx];
    }
  } // n

  // Okay, good patches for each node.

}

template <typename NFIELD, typename Real>
void ElemPatch<NFIELD,Real>::Eval(UInt npts,
                       const double pcoord[],
                       Real result[]) const
{
  // Step 1: get physical coords
  UInt sdim = pmesh->spatial_dim();
  UInt pdim = pmesh->parametric_dim();
  std::vector<double> rcoord(sdim*npts);
  
  const MeshObjTopo *topo = GetMeshObjTopo(*pelem);
  const UInt nv = topo->num_vertices;
  // View pcoord as integration points so MEValues may be used.
  arbq pintg(pdim, npts, pcoord);

  // Now the linear weights to combine nodal patches
  MEValues<METraits<Real,double>, METraits<>, NFIELD, MEField<> > mevl(MEFamilyLow::instance(), // bilinear POU
                  *pcfield);
                  

  // View pcoord as integration points so MEValues may be used.
  mevl.Setup(*pelem, MEV::update_sf, &pintg);
  mevl.ReInit(*pelem); // shape values are now query'able 

  mevl.GetCoordinateValues(&rcoord[0]);


  // Loop the points, interpolating fields
  for (UInt p = 0; p < npts; p++) {
    for (UInt f = 0; f < flen; f++) {
      result[p*flen+f] = 0;
      for (UInt n = 0; n < nv; n++) {
        result[p*flen+f] += mevl.GetShapeValue(p, n)*
          patches[n].EvalPatch(f, &rcoord[p*sdim]);
      } 
    } // f
  } // npoints

}



// ********** Instantiations ************

template class PatchRecov<MEField<>, double>;
template class PatchRecov<MEField<SField> ,fad_type>;

template class ElemPatch<MEField<> ,double>;
template class ElemPatch<MEField<SField> ,fad_type>;


} // namespace
} // namespace
