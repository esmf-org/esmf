// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/ESMCI_PatchRecovery.h>
#include <Mesh/include/ESMCI_Exception.h>
#include <Mesh/include/ESMCI_MeshOBjConn.h>
#include <Mesh/include/ESMCI_MeshUtils.h>
#include <Mesh/include/ESMCI_MEValues.h>
#include <Mesh/include/ESMCI_Polynomial.h>
#include <Mesh/include/ESMCI_MeshField.h>
#include <Mesh/include/ESMCI_MeshTypes.h>
#include <Mesh/include/ESMCI_Ftn.h>
#include <Mesh/include/ESMCI_ParEnv.h>


#include <set>
#include <iostream>
#include <iterator>
#include <iomanip>
#include <cmath>

#include <Mesh/include/sacado/Sacado.hpp>
#include <cstdlib>




#ifdef ESMF_LAPACK
extern "C" void FTN(dgelsy)(int *,int *,int*,double*,int*,double*,int*,int*,double*,int*,double*,int*,int*);

extern "C" void FTN(dgelsd)(int *,int *, int*, double*, int*, double*, int*,double*, double*, int*, double *, int*, int *, int *);

extern "C" void FTN(f_esmf_lapack_iworksize)(int *,int *);
#endif

          

namespace ESMCI {


template<typename NFIELD, typename Real>
UInt PatchRecov<NFIELD,Real>::get_ncoeff(UInt dim, UInt deg) {

  UInt res = 1;
  for (UInt i = 0; i < dim; i++) 
    res *= (deg+1);
  return res;
}

/**
 * Needs a buffer of size buf(2*(pdeg+1)]
 */
void eval_monomial2(UInt nsamples, UInt sample, UInt pdeg, const double coord[], double res[], double buf[]) {
  // Lay out 1 x x^2 x^2 ... 1 y y^2 y
  buf[0] = 1.0;
  for (UInt i = 1; i <= pdeg; i++) {
    buf[i] = coord[0];
  }
  for (UInt i = 2; i <= pdeg; i++) {
    for (UInt j = i; j <= pdeg; j++)
      buf[j] *= coord[0];
  }
 
  // Fill out 1 y y^2 ...
  UInt off = pdeg+1;
  buf[off] = 1.0;
  for (UInt i = 1; i <= pdeg; i++) {
    buf[off+i] = coord[1];
  }
  for (UInt i = 2; i <= pdeg; i++) {
    for (UInt j = i; j <= pdeg; j++)
      buf[off+j] *= coord[1];
  }

  // Now tensor product
  UInt k = 0;
  for (UInt i = 0; i <= pdeg; i++) {
    for (UInt j = 0; j <= pdeg; j++) {
      res[k++*nsamples+sample] = buf[i]*buf[off+j];
    }
  }

} 

/**
 * Needs a buffer of size buf(3*(pdeg+1)]
 */
void eval_monomial3(UInt nsamples, UInt sample, UInt pdeg, const double coord[], double res[], double buf[]) {
  // Lay out 1 x x^2 x^2 ... 1 y y^2 y
  buf[0] = 1.0;
  for (UInt i = 1; i <= pdeg; i++) {
    buf[i] = coord[0];
  }
  for (UInt i = 2; i <= pdeg; i++) {
    for (UInt j = i; j <= pdeg; j++)
      buf[j] *= coord[0];
  }
 
  // Fill out 1 y y^2 ...
  UInt off = pdeg+1;
  buf[off] = 1.0;
  for (UInt i = 1; i <= pdeg; i++) {
    buf[off+i] = coord[1];
  }
  for (UInt i = 2; i <= pdeg; i++) {
    for (UInt j = i; j <= pdeg; j++)
      buf[off+j] *= coord[1];
  }

  // 1 z z^2 ...
  UInt off1 = 2*(pdeg+1);
  buf[off1] = 1.0;
  for (UInt i = 1; i <= pdeg; i++) {
    buf[off1+i] = coord[2];
  }
  for (UInt i = 2; i <= pdeg; i++) {
    for (UInt j = i; j <= pdeg; j++)
      buf[off1+j] *= coord[2];
  }

  // Now tensor product
  UInt l = 0;
  for (UInt i = 0; i <= pdeg; i++) {
    for (UInt j = 0; j <= pdeg; j++) {
      for (UInt k = 0; k <= pdeg; k++) {
        res[l++*nsamples+sample] = buf[i]*buf[off+j]*buf[off1+k];
      }
    }
  }

} 

template<typename NFIELD, typename Real>
void PatchRecov<NFIELD,Real>::eval_poly(UInt nsamples, UInt sample, UInt ldb, UInt cur_rhs, 
          UInt dim, std::vector<double> &mat, double coord[],
                          std::vector<Real> &rhs, const Real fvals[], UInt fdim) {
  if (pdeg == 0) {
    mat[0*nsamples + sample] = 1;
  } else if (dim == 2) {
/*
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
*/
    std::vector<double> buf(2*(pdeg+1));
    eval_monomial2(nsamples,sample,pdeg,coord,&mat[0],&buf[0]);
    
  } else if (dim == 3) {
/*
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
*/

    std::vector<double> buf(3*(pdeg+1));

    eval_monomial3(nsamples,sample,pdeg,coord,&mat[0],&buf[0]);

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
mc(rhs.mc),
use_mc(rhs.use_mc)
{
}

template<typename NFIELD, typename Real>
PatchRecov<NFIELD,Real> PatchRecov<NFIELD,Real>::operator*(double val) {
  
  PatchRecov pv(*this);
  
  for (UInt i = 0; i < coeff.size(); i++)
    pv.coeff[i] *= val;
  
  return pv;
}

template<typename NFIELD, typename Real>
void PatchRecov<NFIELD,Real>::operator+=(const PatchRecov &rhs)
{
  ThrowRequire(ncoeff == 0 || ncoeff == rhs.ncoeff);
  if (ncoeff == 0) {
    // assume the zero patch
    *this = rhs;
  } else {
    for (UInt i = 0; i < coeff.size(); i++)
      coeff[i] += rhs.coeff[i];
  }
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
  use_mc = rhs.use_mc;
  return *this;
}

template<typename NFIELD, typename Real>
PatchRecov<NFIELD,Real>::PatchRecov() :
pdeg(0),
idim(0),
ncoeff(0),
patch_ok(false),
coeff(),
mc()
{
}

//#define RESIDUALS
// XXX

/**
 * The default creates the pseudo-inverse and applies, in case we
 * need sensitivities of coef wrt field values.
 */
template <typename Real>
struct DGELSD_Solver {
void operator()(UInt ncoef, int ldb, int m, int n, int nrhs, std::vector<double> &mat, std::vector<Real> &rhs, Real coeff[])
{

#ifdef ESMF_LAPACK

#ifdef RESIDUALS
Par::Out() << "A(" << m << "," << n << ")=" << std::endl;
for (UInt i = 0; i < m; i++) {
for (UInt j = 0; j < n; j++) {
Par::Out() << std::setw(10) << mat[j*m+i] << " ";
}
Par::Out() << std::endl;
}
#endif

 // variables for solver call
 int info, rank;

 // Set condition number (how bad of a matrix to accept)
 double rcond=1.0/10000000.0;
 
 // TODO: much of this memory will be the same size every time, it would be good to have
 //       a scheme to hold onto it and allow its reuse

 // Set up B=I for calculating pseudo-inverse
 std::vector<double> id_rhs(ldb*ldb, 0);
 for (int i = 0; i < m; i++) id_rhs[i*ldb + i] = 1.0;

  // calculate minimum of m and n
  int minmn=std::min(m,n);

  // Allocate s matrix
  std::vector<double> s(minmn, 0);

  // calculate iworksize
  int iworksize=0;
  FTN(f_esmf_lapack_iworksize)(&minmn, &iworksize);

  // Allocate iwork buffer
  std::vector<int> iwork(iworksize, 0);
 
  // calculate work size, by using solver with lwork = -1
  int tmplwork=-1;
  double tmpwork=0;
  FTN(dgelsd)(
	      &m, &n, &m, &mat[0], &m, &id_rhs[0], &ldb, &s[0], &rcond, &rank, &tmpwork, &tmplwork, &iwork[0], &info); 
  int worksize = int(tmpwork);


  // allocate work vector
  std::vector<double> work(worksize, 0);
  

  // Call solver
  FTN(dgelsd)(
	      &m, &n, &m, &mat[0], &m, &id_rhs[0], &ldb, &s[0], &rcond, &rank, &work[0], &worksize, &iwork[0], &info);
  if (info !=0) Throw() << "Bad dgelsd solve, info=" << info;


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

#ifdef RESIDUALS
Par::Out() << "PAInv=" << std::endl;
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < m; j++) {
Par::Out() << std::setw(15) << id_rhs[j*ldb+i];
    }
Par::Out() << std::endl;
  }

Par::Out() << "B=" << std::endl;
    for (int j = 0; j < m; j++) {
      for (int nr = 0; nr < nrhs; nr++) {
         Par::Out() << std::setw(15) << b[nr*ldb+j].val();
      }
      Par::Out() << std::endl;
    }
#endif

  // fill return array
  for (UInt r = 0; r < (UInt) nrhs; r++) {
    for (UInt c = 0; c < ncoef; c++) {
      coeff[r*ncoef+c] = rhs[r*ldb+c];
    }
  }

#else
  Throw() << "Please reconfigure with lapack enabled";
#endif

}

};

/**
 * A specialization for double that does not build the full pseudo inverse, but just
 * solves the system 
 */
template <>
struct DGELSD_Solver<double> {
void operator()(UInt ncoef, int ldb, int m, int n, int nrhs, std::vector<double> &mat, std::vector<double> &rhs, double coeff[])
{

#ifdef ESMF_LAPACK

#ifdef RESIDUALS
std::vector<double> saverhs = rhs;
std::vector<double> matsav = mat;
#endif

 // variables for solver call
 int info, rank;

 // Set condition number (how bad of a matrix to accept)
 double rcond=1.0/10000000.0;
 
 // TODO: much of this memory will be the same size every time, it would be good to have
 //       a scheme to hold onto it and allow its reuse


  // calculate minimum of m and n
  int minmn=std::min(m,n);

  // Allocate s matrix
  std::vector<double> s(minmn, 0);

  // calculate iworksize
  int iworksize=0;
  FTN(f_esmf_lapack_iworksize)(&minmn, &iworksize);

  // Allocate iwork buffer
  std::vector<int> iwork(iworksize, 0);
 
  // calculate work size, by using solver with lwork = -1
  int tmplwork=-1;
  double tmpwork=0;
  FTN(dgelsd)(
	      &m, &n, &nrhs, &mat[0], &m, &rhs[0], &ldb, &s[0], &rcond, &rank, &tmpwork, &tmplwork, &iwork[0], &info);
  
  int worksize = int(tmpwork);

  // allocate work vector
  std::vector<double> work(worksize, 0);
  
  // Call solver
  FTN(dgelsd)(
	      &m, &n, &nrhs, &mat[0], &m, &rhs[0], &ldb, &s[0], &rcond, &rank, &work[0], &worksize, &iwork[0], &info);

  if (info !=0) Throw() << "Bad dgelsd solve, info=" << info;


#ifdef RESIDUALS
Par::Out() << "A(" << m << "," << n << ")=" << std::endl;
for (UInt i = 0; i < m; i++) {
for (UInt j = 0; j < n; j++) {
Par::Out() << std::setw(10) << matsav[j*m+i] << " ";
}
Par::Out() << std::endl;
}
#endif

#ifdef RESIDUALS
// Weird ; rhs comes back (Nxnrhs), so no ldb, just nsamples
Par::Out() << "rcond=" << rcond << std::endl;
std::vector<double> tst(m,0);

Par::Out() << "rhs:" << std::endl;
for (UInt s = 0; s < (UInt) nrhs; s++) {
  for (UInt i = 0; i < m; i++) {
    Par::Out() << saverhs[s*ldb+i] << " ";
  }
  Par::Out() << std::endl;
}

double err = 0;
for (UInt s = 0; s < (UInt) nrhs; s++) {
Par::Out() << "A*x=" << std::endl;
    for (UInt i = 0; i < m; i++) {
      tst[i] = 0;
      for (UInt j = 0; j < ncoef; j++) {
        tst[i] += matsav[j*m + i]*rhs[s*ldb+j];
      }
Par::Out() << tst[i] << " ";
      err += (tst[i]-saverhs[s*ldb+i])*(tst[i]-saverhs[s*ldb+i]);
    }
Par::Out() << std::endl;
}
Par::Out() << "err=" << err << std::endl;
if (err > 1e-10) Par::Out() << "\tnsamples=" << m << std::endl;
#endif

  for (UInt r = 0; r < (UInt) nrhs; r++) {
    for (UInt c = 0; c < ncoef; c++) {
      coeff[r*ncoef+c] = rhs[r*ldb+c];
    }
  }

#else
  Throw() << "Please reconfigure with lapack enabled";
#endif

}

};



/**
 * The default creates the pseudo-inverse and applies, in case we
 * need sensitivities of coef wrt field values.
 */
template <typename Real>
struct DGELSY_Solver {
void operator()(UInt ncoef, int ldb, int m, int n, int nrhs, std::vector<double> &mat, int rank, double work[], int lwork, int info, std::vector<Real> &rhs, Real coeff[])
{

  std::vector<int> jpvt(ncoef, 0);
  //int lwork = std::max(std::min(m,n)+2*n+1, 2*std::min(m,n)+nrhs);
  // TODO figure this out
  double rcond=0.0000000000001;

  std::vector<double> id_rhs(ldb*ldb, 0);
  // Set up B=I
  for (int i = 0; i < m; i++) id_rhs[i*ldb + i] = 1.0;

#ifdef RESIDUALS
Par::Out() << "A(" << m << "," << n << ")=" << std::endl;
for (UInt i = 0; i < m; i++) {
for (UInt j = 0; j < n; j++) {
Par::Out() << std::setw(10) << mat[j*m+i] << " ";
}
Par::Out() << std::endl;
}
#endif

#ifdef ESMF_LAPACK
  FTN(dgelsy)(
    &m, &n, &m, &mat[0], &m, &id_rhs[0], &ldb, &jpvt[0], &rcond, &rank, &work[0], &lwork, &info);

  if (info !=0) Throw() << "Bad dgelsy solve, info=" << info;
#else
  Throw() << "Please reconfigure with lapack enabled";
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
#ifdef RESIDUALS
Par::Out() << "PAInv=" << std::endl;
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < m; j++) {
Par::Out() << std::setw(15) << id_rhs[j*ldb+i];
    }
Par::Out() << std::endl;
  }

Par::Out() << "B=" << std::endl;
    for (int j = 0; j < m; j++) {
      for (int nr = 0; nr < nrhs; nr++) {
         Par::Out() << std::setw(15) << b[nr*ldb+j].val();
      }
      Par::Out() << std::endl;
    }
#endif

  // fill return array
  for (UInt r = 0; r < (UInt) nrhs; r++) {
    for (UInt c = 0; c < ncoef; c++) {
      coeff[r*ncoef+c] = rhs[r*ldb+c];
    }
  }
}

};

/**
 * A specialization for double that does not build the full pseudo inverse, but just
 * solves the system 
 */
template <>
struct DGELSY_Solver<double> {
void operator()(UInt ncoef, int ldb, int m, int n, int nrhs, std::vector<double> &mat, int rank, double work[], int lwork, int info, std::vector<double> &rhs, double coeff[])
{

  std::vector<int> jpvt(ncoef, 0);
  //int lwork = std::max(std::min(m,n)+2*n+1, 2*std::min(m,n)+nrhs);
  // TODO figure this out
  double rcond=0.0000000000001;

#ifdef RESIDUALS
std::vector<double> saverhs = rhs;
std::vector<double> matsav = mat;
#endif

#ifdef ESMF_LAPACK
  FTN(dgelsy)(
    &m, &n, &nrhs, &mat[0], &m, &rhs[0], &ldb, &jpvt[0], &rcond, &rank, &work[0], &lwork, &info);
#else
  Throw() << "Please recompile with ESMF_LAPACK enabled";
#endif

#ifdef RESIDUALS
Par::Out() << "A(" << m << "," << n << ")=" << std::endl;
for (UInt i = 0; i < m; i++) {
for (UInt j = 0; j < n; j++) {
Par::Out() << std::setw(10) << matsav[j*m+i] << " ";
}
Par::Out() << std::endl;
}
#endif

#ifdef RESIDUALS
// Wierd ; rhs comes back (Nxnrhs), so no ldb, just nsamples
Par::Out() << "rcond=" << rcond << std::endl;
std::vector<double> tst(m,0);

Par::Out() << "rhs:" << std::endl;
for (UInt s = 0; s < (UInt) nrhs; s++) {
  for (UInt i = 0; i < m; i++) {
    Par::Out() << saverhs[s*ldb+i] << " ";
  }
  Par::Out() << std::endl;
}

double err = 0;
for (UInt s = 0; s < (UInt) nrhs; s++) {
Par::Out() << "A*x=" << std::endl;
    for (UInt i = 0; i < m; i++) {
      tst[i] = 0;
      for (UInt j = 0; j < ncoef; j++) {
        tst[i] += matsav[j*m + i]*rhs[s*ldb+j];
      }
Par::Out() << tst[i] << " ";
      err += (tst[i]-saverhs[s*ldb+i])*(tst[i]-saverhs[s*ldb+i]);
    }
Par::Out() << std::endl;
}
Par::Out() << "err=" << err << std::endl;
if (err > 1e-10) Par::Out() << "\tnsamples=" << m << std::endl;
#endif

  for (UInt r = 0; r < (UInt) nrhs; r++) {
    for (UInt c = 0; c < ncoef; c++) {
      coeff[r*ncoef+c] = rhs[r*ldb+c];
    }
  }

}

};

template<typename NFIELD, typename Real>
void PatchRecov<NFIELD,Real>::CreatePatch(
           UInt _pdeg,
           const MeshDB &mesh,
           const MeshObj &node,
           const MeshObj *elem_hint,
           UInt numfields,
           NFIELD **rfield,
           UInt threshold,           
           const MEField<> &coord,
           const MCoord *_mc,
           MEField<> *src_mask_ptr
           ) 
{
  patch_ok = true; // used below
  pdeg = _pdeg;
  use_mc = _mc == NULL ? false : true;
  if (use_mc) mc = *_mc;
  UInt pdim = mesh.parametric_dim();
  UInt sdim = mesh.spatial_dim();

  if (coord.dim() != sdim)
    Throw() << "Patch:sdim=" << sdim << ", but fdim=" << coord.dim();

  if (use_mc) {
    if (mc.ManifoldDim() != pdim) Throw() << "MCoord manifold dim:" << mc.ManifoldDim() << " is  not = pdim:" << pdim;
    idim = pdim;
  } else idim = sdim;

  UInt ncoef = get_ncoeff(idim, pdeg);
  ncoeff = ncoef;
  std::vector<double> mat;
  std::vector<Real> rhs;

  // Get elements to use for generating patch

  std::set<const MeshObj*> elems;
  MeshObjRelationList::const_iterator el = MeshObjConn::find_relation(node, MeshObj::ELEMENT);

  // If there's a mask then use mask to avoid bad elements
  if (src_mask_ptr != NULL) {
    while (el != node.Relations.end() && el->obj->get_type() == MeshObj::ELEMENT){
      MeshObj &elem=*(el->obj);

      // See if masked out
      //      printf(" %d  :: ",elem.get_id());
      bool masked=false;
      MeshObjRelationList::const_iterator nl = MeshObjConn::find_relation(elem, MeshObj::NODE);
      while (nl != elem.Relations.end() && nl->obj->get_type() == MeshObj::NODE){
	MeshObj &node = *(nl->obj);
	double *m = src_mask_ptr->data(node);
	// printf(" %f ",*m);
	if (*m > 0.5) {
	  masked=true;
	  break;
	}
	++nl;
      }
      //      printf(" :: %d \n",masked);
      // If not masked out then add to list
      if (!masked) elems.insert(el->obj);

      ++el;
    }

  } else { // Otherwise jsut put all elements in
    while (el != node.Relations.end() && el->obj->get_type() == MeshObj::ELEMENT){
      elems.insert(el->obj);
      ++el;
    }
  }
  
  // Stack elements in
  // Use integration rule for field 0
  UInt nsamples = 0;
  std::set<const MeshObj*>::iterator esi = elems.begin(), ese = elems.end();

  UInt num_elems = elems.size();
  
  for (; esi != ese; ++esi) {
    const MeshObj &selem = **esi;
    MasterElement<METraits<Real> > *me = GetME(*rfield[0], selem)(METraits<Real>());

    // Find an integration rule that just overconstrains the problem
    
    const intgRule *ir = 0;
    // Need to have enough points to fully constrain matrix
    // start with 2, because symmetry of 1 at poles (the only place 1 will be used) causes problems with l.s. solver
    // Come up with a better max than pdeg+4
     for (UInt q = 2; q <= pdeg+4; q++) {
      ir = GetIntg(selem)->ChangeOrder(q);
      if (num_elems*ir->npoints() >= ncoef) {
/*
Par::Out() << "Patch irule deg=" << q <<", pdeg:" << pdeg << ", nelem:" << num_elems << ", ncoef:" << ncoef << ", irpoints:" << ir->npoints() << std::endl;
*/
        break;
      }
    }

    if (ir == NULL) Throw() << "Patch, no intg rule for me:" << me->name;
    nsamples += ir->npoints();
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
      MEValues<METraits<Real,double>, NFIELD> mev(field.GetMEFamily(), &coord);

      const intgRule *ir = 0;
      // Need to have enough points to fully constrain matrix
      // start with 2, because symmetry of 1 at poles (the only place 1 will be used) causes problems with l.s. solver
      // Come up with a better max than pdeg+4
      for (UInt q = 2; q <= pdeg+4; q++) {
        ir = GetIntg(selem)->ChangeOrder(q);
        if (num_elems*ir->npoints() >= ncoef) {
          break;
        }
      }
      ThrowRequire(ir);

      mev.Setup(selem, MEV::update_sf | MEV::update_map, ir);
      mev.ReInit(selem);
     
      std::vector<Real> felem(field.dim()*mev.GetNumFunctions());
      std::vector<double> celem(sdim*mev.GetNumCoordFunctions());

      std::vector<double> cdatas(sdim*mev.GetNQPoints());
      std::vector<Real> fdata(field.dim()*mev.GetNQPoints());
      mev.GetCoordinateValues(&cdatas[0]);
      mev.GetFunctionValues(field, &fdata[0]);
      for (UInt q = 0; q < mev.GetNQPoints(); q++) {
        double *cd = &cdatas[q*sdim]; // default here
        if (use_mc) {
          mc.Transform(&cdatas[q*sdim], &cdata[0]);
          cd = &cdata[0]; // point to transformed point
#ifdef RESIDUALS
Par::Out() << "Coords:" << std::endl;
std::copy(&cdatas[q*sdim], &cdatas[(q+1)*sdim], std::ostream_iterator<double>(Par::Out(), " "));
Par::Out() << "-->" << std::endl;
std::copy(cd, cd + idim, std::ostream_iterator<double>(Par::Out(), " "));
Par::Out() << std::endl;
#endif
        }
      
        eval_poly(nsamples, sample++, ldb, cur_rhs,
              idim, mat, cd, rhs, &fdata[q*field.dim()], field.dim());
      } // for qpoint
    } // for elem
      cur_rhs += field.dim();
  } // f


  // Do least squares solve to get coefficients
  DGELSD_Solver<Real> s;
  s(ncoef, ldb, m, n, nrhs, mat, rhs, &coeff[0]);


  // Patch is ok if we've gotten this far
  patch_ok = true;
}

template <typename NFIELD, typename Real>
void PatchRecov<NFIELD,Real>::CreateConstantPatch(const MeshObj &node,
                                                  UInt numfields,
                                                  NFIELD **rfield)
{
  pdeg = 0;
  
  ncoeff = 1;
  
  for (UInt i = 0; i < numfields; i++) {
    NFIELD *nf = rfield[i];
    
    Real *data = nf->data(node);
    
    for (UInt f = 0; f < nf->dim(); f++) {
      
      coeff.push_back(data[f]);
      
    }
    
  }
  
  patch_ok = true;
  
}

/**
 * Needs a buffer of size buf(2*(pdeg+1)]
 */
template <typename Real>
Real EvalMonomial2(UInt pdeg, const double coord[], const Real coef[], double buf[]) {
  // Lay out 1 x x^2 x^2 ... 1 y y^2 y
  buf[0] = 1.0;
  for (UInt i = 1; i <= pdeg; i++) {
    buf[i] = coord[0];
  }
  for (UInt i = 2; i <= pdeg; i++) {
    for (UInt j = i; j <= pdeg; j++)
      buf[j] *= coord[0];
  }
 
  // Fill out 1 y y^2 ...
  UInt off = pdeg+1;
  buf[off] = 1.0;
  for (UInt i = 1; i <= pdeg; i++) {
    buf[off+i] = coord[1];
  }
  for (UInt i = 2; i <= pdeg; i++) {
    for (UInt j = i; j <= pdeg; j++)
      buf[off+j] *= coord[1];
  }

  // Now tensor product
  Real res = 0;
  UInt k = 0;
  for (UInt i = 0; i <= pdeg; i++) {
    for (UInt j = 0; j <= pdeg; j++) {
      res += coef[k++]*buf[i]*buf[off+j];
    }
  }

  return res;

} 

/** 
 * needs a buffer of size 2*(pdeg+2)
 */
template <typename Real>
Real EvalMonomialDeriv2(UInt pdeg, const double coord[], const Real coef[], Real res[], double buf[], bool eval_func) {
  // Lay out 0 1 x x^2 x^2 ... 1 y y^2 y
  buf[0] = 0.0; buf[1] = 1.0;
  for (UInt i = 1; i <= pdeg; i++) {
    buf[1+i] = coord[0];
  }
  for (UInt i = 2; i <= pdeg; i++) {
    for (UInt j = i; j <= pdeg; j++)
      buf[j+1] *= coord[0];
  }
 
  // Fill out 1 y y^2 ...
  UInt off = 1+pdeg+1;
  buf[off] = 0.0; buf[off+1] = 1.0;
  for (UInt i = 1; i <= pdeg; i++) {
    buf[1+off+i] = coord[1];
  }
  for (UInt i = 2; i <= pdeg; i++) {
    for (UInt j = i; j <= pdeg; j++)
      buf[1+off+j] *= coord[1];
  }

  // Now tensor product
  res[0] = 0;
  UInt k = 0;
  for (UInt i = 0; i <= pdeg; i++) {
    for (UInt j = 0; j <= pdeg; j++) {
      res[0] += coef[k++]*i*buf[i]*buf[1+off+j];
    }
  }

  res[1] = 0;
  k = 0;
  for (UInt i = 0; i <= pdeg; i++) {
    for (UInt j = 0; j <= pdeg; j++) {
      res[1] += coef[k++]*buf[1+i]*j*buf[off+j];
    }
  }

  Real pres = 0;
  if (eval_func) {
    k = 0;
    for (UInt i = 0; i <= pdeg; i++) {
      for (UInt j = 0; j <= pdeg; j++) {
        pres += coef[k++]*buf[1+i]*buf[1+off+j];
      }
    }
  } 

  return pres;

} 

/**
 * Needs a buffer of size buf(3*(pdeg+1)]
 */
template <typename Real>
Real EvalMonomial3(UInt pdeg, const double coord[], const Real coef[], double buf[]) {
  // Lay out 1 x x^2 x^2 ... 1 y y^2 y
  buf[0] = 1.0;
  for (UInt i = 1; i <= pdeg; i++) {
    buf[i] = coord[0];
  }
  for (UInt i = 2; i <= pdeg; i++) {
    for (UInt j = i; j <= pdeg; j++)
      buf[j] *= coord[0];
  }
 
  // Fill out 1 y y^2 ...
  UInt off = pdeg+1;
  buf[off] = 1.0;
  for (UInt i = 1; i <= pdeg; i++) {
    buf[off+i] = coord[1];
  }
  for (UInt i = 2; i <= pdeg; i++) {
    for (UInt j = i; j <= pdeg; j++)
      buf[off+j] *= coord[1];
  }

  // 1 z z^2 ...
  UInt off1 = 2*(pdeg+1);
  buf[off1] = 1.0;
  for (UInt i = 1; i <= pdeg; i++) {
    buf[off1+i] = coord[2];
  }
  for (UInt i = 2; i <= pdeg; i++) {
    for (UInt j = i; j <= pdeg; j++)
      buf[off1+j] *= coord[2];
  }

  // Now tensor product
  UInt l = 0;
  Real res = 0;
  for (UInt i = 0; i <= pdeg; i++) {
    for (UInt j = 0; j <= pdeg; j++) {
      for (UInt k = 0; k <= pdeg; k++) {
        res += coef[l++]*buf[i]*buf[off+j]*buf[off1+k];
      }
    }
  }

  return res;

} 

/**
 * needs a buffer of size 3((pdeg+2)
 */
template <typename Real>
Real EvalMonomialDeriv3(UInt pdeg, const double coord[], const Real coef[], Real res[], double buf[], bool eval_func) {
  // Lay out 0 1 x x^2 x^2 ... 1 y y^2 y
  buf[0] = 0.0; buf[1] = 1.0;
  for (UInt i = 1; i <= pdeg; i++) {
    buf[1+i] = coord[0];
  }

  for (UInt i = 2; i <= pdeg; i++) {
    for (UInt j = i; j <= pdeg; j++)
      buf[1+j] *= coord[0];
  }
 
  // Fill out 0 1 y y^2 ...
  UInt off = 1+pdeg+1;
  buf[off] = 0.0; buf[off+1] = 1.0;
  for (UInt i = 1; i <= pdeg; i++) {
    buf[1+off+i] = coord[1];
  }
  for (UInt i = 2; i <= pdeg; i++) {
    for (UInt j = i; j <= pdeg; j++)
      buf[1+off+j] *= coord[1];
  }

  // 1 z z^2 ...
  UInt off1 = 2+2*(pdeg+1);
  buf[off1] = 0.0; buf[off1+1] = 1.0;
  for (UInt i = 1; i <= pdeg; i++) {
    buf[1+off1+i] = coord[2];
  }
  for (UInt i = 2; i <= pdeg; i++) {
    for (UInt j = i; j <= pdeg; j++)
      buf[1+off1+j] *= coord[2];
  }

  // Now tensor product
  UInt l = 0;
  res[0] = 0;
  for (UInt i = 0; i <= pdeg; i++) {
    for (UInt j = 0; j <= pdeg; j++) {
      for (UInt k = 0; k <= pdeg; k++) {
        res[0] += coef[l++]*i*buf[i]*buf[1+off+j]*buf[1+off1+k];
      }
    }
  }

  l = 0;
  res[1] = 0;
  for (UInt i = 0; i <= pdeg; i++) {
    for (UInt j = 0; j <= pdeg; j++) {
      for (UInt k = 0; k <= pdeg; k++) {
        res[1] += coef[l++]*buf[1+i]*j*buf[off+j]*buf[1+off1+k];
      }
    }
  }

  l = 0;
  res[2] = 0;
  for (UInt i = 0; i <= pdeg; i++) {
    for (UInt j = 0; j <= pdeg; j++) {
      for (UInt k = 0; k <= pdeg; k++) {
        res[2] += coef[l++]*buf[1+i]*buf[1+off+j]*k*buf[off1+k];
      }
    }
  }

  Real pres = 0;
  if (eval_func) {
    l = 0;
    for (UInt i = 0; i <= pdeg; i++) {
      for (UInt j = 0; j <= pdeg; j++) {
        for (UInt k = 0; k <= pdeg; k++) {
          pres += coef[l++]*buf[1+i]*buf[1+off+j]*buf[1+off1+k];
        }
      }
    }
  } 

  return pres;

} 

template<typename NFIELD, typename Real>
Real PatchRecov<NFIELD,Real>::EvalPatch(UInt nfield, const double coords[]) const {
  const double *cd = coords;
  std::vector<double> mcoords(use_mc ? mc.ManifoldDim() : 0);
  if (use_mc) {
    mc.Transform(coords, &mcoords[0]);
    cd = &mcoords[0];
  }
  const Real *coef = &coeff[nfield*ncoeff];
  if (pdeg == 0) {
    return coef[0];
  } else if (idim == 2) {

    std::vector<double> buf(2*(pdeg+1));
    return EvalMonomial2(pdeg, cd, coef, &buf[0]);

  } else if (idim == 3) {

    std::vector<double> buf(3*(pdeg+1));
    return EvalMonomial3(pdeg, cd, coef, &buf[0]);

  } else Throw() << "dim, deg=" << idim << ", " << pdeg << " not yet supported";
}

template<typename NFIELD, typename Real>
void PatchRecov<NFIELD,Real>::EvalPatchGrad(UInt nfield, const double coords[], Real result[]) const {

  if (use_mc) Throw() << "EvalPatchGrad can not use mcoords";

  const double *cd = coords;

  for (UInt d = 0; d < idim; d++) result[d] = 0;

  const Real *coef = &coeff[nfield*ncoeff];
  if (pdeg == 0) {
    for (UInt d = 0; d < idim; d++) result[d] = coef[0];
  } else if (idim == 2) {

    std::vector<double> buf(2*(pdeg+2));
    EvalMonomialDeriv2(pdeg, cd,coef, result, &buf[0], false);
/*
    UInt k = 0;
    for (UInt i = 0; i <= pdeg; i++) {
      for (UInt j = 0; j <= pdeg; j++) {
        Monomial<double> m1(i), m2(j);
        TensorPolynomial<Monomial<double> > tp(&m1, &m2);

        EvalTensorPolyDeriv(idim, cd, tp, tpd);
//std::cout << "tpd=" << tpd[0] << ", " << tpd[1] << ", coef=" << coef[k] << std::endl;

        for (UInt d = 0; d < idim; d++ ) {
          result[d] += coef[k]*tpd[d];
//std::cout << "result=" << result[d] << std::endl;
        }
        ++k;
      }
    }
*/
  } else if (idim == 3) {

    std::vector<double> buf(3*(pdeg+2));
    EvalMonomialDeriv3(pdeg, cd,coef, result, &buf[0], false);

/*
    UInt l = 0;
    for (UInt i = 0; i <= pdeg; i++) {
      for (UInt j = 0; j <= pdeg; j++) {
        for (UInt k = 0; k <= pdeg; k++) {
          Monomial<double> m1(i), m2(j), m3(k);
          TensorPolynomial<Monomial<double> > tp(&m1, &m2, &m3);

          EvalTensorPolyDeriv(idim, cd, tp, tpd);

          for (UInt d = 0; d < idim; d++) {
            result[d] += coef[l]*tpd[d];
          }
          ++l;
        }
      }
    }
*/
  } else Throw() << "dim, deg=" << idim << ", " << pdeg << " not yet supported";
}

template<typename NFIELD, typename Real>
Real PatchRecov<NFIELD,Real>::EvalPatchAndGrad(UInt nfield, const double coords[], Real result[]) const {

  if (use_mc) Throw() << "EvalPatchGrad can not use mcoords";

  const double *cd = coords;

  for (UInt d = 0; d < idim; d++) result[d] = 0;

  const Real *coef = &coeff[nfield*ncoeff];
  if (pdeg == 0) {
    for (UInt d = 0; d < idim; d++) result[d] = coef[0];
  } else if (idim == 2) {

    std::vector<double> buf(2*(pdeg+2));
    return EvalMonomialDeriv2(pdeg, cd,coef, result, &buf[0], true);
/*
    UInt k = 0;
    for (UInt i = 0; i <= pdeg; i++) {
      for (UInt j = 0; j <= pdeg; j++) {
        Monomial<double> m1(i), m2(j);
        TensorPolynomial<Monomial<double> > tp(&m1, &m2);

        EvalTensorPolyDeriv(idim, cd, tp, tpd);
//std::cout << "tpd=" << tpd[0] << ", " << tpd[1] << ", coef=" << coef[k] << std::endl;

        for (UInt d = 0; d < idim; d++ ) {
          result[d] += coef[k]*tpd[d];
//std::cout << "result=" << result[d] << std::endl;
        }
        ++k;
      }
    }
*/
  } else if (idim == 3) {

    std::vector<double> buf(3*(pdeg+2));
    return EvalMonomialDeriv3(pdeg, cd,coef, result, &buf[0], true);

/*
    UInt l = 0;
    for (UInt i = 0; i <= pdeg; i++) {
      for (UInt j = 0; j <= pdeg; j++) {
        for (UInt k = 0; k <= pdeg; k++) {
          Monomial<double> m1(i), m2(j), m3(k);
          TensorPolynomial<Monomial<double> > tp(&m1, &m2, &m3);

          EvalTensorPolyDeriv(idim, cd, tp, tpd);

          for (UInt d = 0; d < idim; d++) {
            result[d] += coef[l]*tpd[d];
          }
          ++l;
        }
      }
    }
*/
  }
 Throw() << "dim, deg=" << idim << ", " << pdeg << " not yet supported";
}


// ******* ElemPatch ************
template <typename NFIELD, typename Real>
ElemPatch<NFIELD,Real>::ElemPatch() :
patches(),
mcs(),
pelem(NULL),
pmesh(NULL),
pcfield(NULL),
flen(0),
nmap(0),
pdeg(0)
{
}

template <typename NFIELD, typename Real>
ElemPatch<NFIELD,Real>::ElemPatch(NodePatchMap &_nmap) :
patches(),
mcs(),
pelem(NULL),
pmesh(NULL),
pcfield(NULL),
flen(0),
nmap(&_nmap)
{
}

/*
template <typename NFIELD, typename Real>
ElemPatch<NFIELD,Real>::ElemPatch(const ElemPatch &rhs) :
  patches(rhs.patches),
  mcs(rhs.mcs),
  pelem(rhs.pelem),
  pmesh(rhs.pmesh),
  pcfield(rhs.pcfield),
  flen(rhs.flen),
  nmap(rhs.nmap)
{
}

template <typename NFIELD, typename Real>
ElemPatch<NFIELD,Real> &ElemPatch<NFIELD,Real>::operator=(const ElemPatch &rhs)
{
  if (this == &rhs) return *this;
  
  patches = rhs.patches;
  mcs = rhs.mcs;
  pelem = rhs.pelem;
  pmesh = rhs.pmesh;
  pcfield = rhs.pcfield;
  flen = rhs.flen;
  nmap = rhs.nmap;
  
  return *this;
}
*/


template <typename NFIELD, typename Real>
ElemPatch<NFIELD,Real>::~ElemPatch() 
{
  if (!nmap) {
    for (UInt p = 0; p < patches.size(); ++p) {
      delete patches[p];
    }
  }
}

template <typename NFIELD, typename Real>
void ElemPatch<NFIELD,Real>::FreePatchMap(NodePatchMap &nmap) {

  typename NodePatchMap::iterator mi = nmap.begin(), me = nmap.end();
  for (; mi != me; ++mi) {
    delete mi->second;
  }

  NodePatchMap().swap(nmap);

}

template <typename NFIELD, typename Real>
void ElemPatch<NFIELD,Real>::CreateElemPatch(UInt _pdeg,
           UInt ptype,
           const MeshObj &elem,  // the elem in question
           const MEField<> &cfield, 
	   MEField<> *src_mask_ptr,
           UInt numfields,
           NFIELD **rfield,
           UInt threshold, bool boundary_ok)       // How far from num dofs to invalidate.  If the
{
  // Set some things up
  pdeg = _pdeg;
  pmesh = &GetMeshObjMesh(elem);
  pcfield = &cfield;
  flen = 0;
  pelem = &elem;
  // total number of field dimensions
  for (UInt i = 0; i < numfields; i++) 
    flen += rfield[i]->dim();

  bool use_mc = (pcfield->dim() > (UInt)pmesh->parametric_dim());
  //bool use_mc = false;


  const MeshObjTopo *topo = GetMeshObjTopo(elem);
  UInt nv = topo->num_vertices;

  patches.resize(nv, static_cast<PatchRecov<NFIELD,Real>*>(0));

  if (use_mc)
    mcs.resize(nv);

  // Loop vertices creating the appropriate patch
  for (UInt n = 0; n < nv; n++) {
    const MeshObj &node = *elem.Relations[n].obj;

    // If using a nodal map, try to find the patch
    bool patch_found = false;
    if (nmap) {
  
      typename NodePatchMap::iterator lb =
        nmap->lower_bound(node.get_id());

      if (lb != nmap->end() && lb->first == node.get_id()) {
        patches[n] = lb->second;
        patch_found = true;
      } else {
        patches[n] = new PatchRecov<NFIELD,Real>();
        nmap->insert(lb, std::make_pair(node.get_id(), patches[n]));
      }

    } else patches[n] = new PatchRecov<NFIELD,Real>();
    
    //if (use_mc) mcs[n] = getMCoordNode(*pcfield, node);
    
    // Center coordinate system at center so we can average patches if not
    // all nodes are interior (otherwise patches have different coordinate systems)
    if (use_mc) mcs[n] = getMCoordElem(*pcfield, elem);

    if (!patch_found) {
    if (!boundary_ok && GetMeshObjContext(node).is_set(Attr::EXPOSED_BOUNDARY_ID)) {
      
      //patches[n].CreateConstantPatch(node, numfields, rfield);
      patches[n]->MarkPatchBad();
      

    } else
      patches[n]->CreatePatch(pdeg, *pmesh, node, &elem, numfields, rfield,
                     700000, *pcfield, use_mc ? &mcs[n] : NULL, src_mask_ptr);
    }

    if (boundary_ok && !patches[n]->PatchOk()) {
      patches[n]->CreatePatch(pdeg, *pmesh, node, &elem, numfields, rfield,
                     700000, *pcfield, use_mc ? &mcs[n] : NULL, src_mask_ptr);
    }
    
  } // for nv


  // Resolve bad (boundary) patches
  UInt nok = 0;
  std::vector<int> ok(nv,0);
  for (UInt n = 0; n < nv; n++) {
    nok += patches[n]->PatchOk() ? 1 : 0;
    if (patches[n]->PatchOk()) ok[n] = 1;
  }

  if (nok == 0) {
    Par::Out() <<"Warning, elem:" << elem.get_id() << ", no good nodes, turning on boundary";

    ElemPatch<NFIELD,Real>::CreateElemPatch( _pdeg,
           ptype,
           elem,  // the elem in question
           cfield, 
           src_mask_ptr,
           numfields,
           rfield,
           threshold,true);       // How far from num dofs to invalidate.  If the
    return;
    
  } 
//std::cout << "nok=" << nok << " , ok_idx=" << ok_idx<< std::endl;

  // do the copying
  double avg = 1.0 / nok;

//if (nok < nv) std::cout << "avg=" << avg << std::endl;
  
  for (UInt n = 0; n < nv; n++) {
    if (ok[n] == 0) {
      for (UInt n1 = 0; n1 < nv; n1++)
      {
        if (ok[n1] == 1)
          patches[n] = patches[n1];
          //*patches[n] += (*patches[n1])*avg;
      }
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
  MEValues<METraits<Real,double>, NFIELD> mevl(MEFamilyLow::instance(), // bilinear POU
                  pcfield);
                  

  // View pcoord as integration points so MEValues may be used.
  mevl.Setup(*pelem, MEV::update_sf | MEV::update_map, &pintg);
  mevl.ReInit(*pelem); // shape values are now query'able 

  mevl.GetCoordinateValues(&rcoord[0]);

  //const double one_o_nv = 1.0/nv;

  // Loop the points, interpolating fields
  for (UInt p = 0; p < npts; p++) {
    for (UInt f = 0; f < flen; f++) {
      result[p*flen+f] = 0;
      for (UInt n = 0; n < nv; n++) {
        result[p*flen+f] += mevl.GetShapeValue(p, n)*
          patches[n]->EvalPatch(f, &rcoord[p*sdim]);
      } 
    } // f
  } // npoints

}

template <typename NFIELD, typename Real>
void ElemPatch<NFIELD,Real>::EvalGrad(UInt npts,
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
  MEValues<METraits<Real,double>, NFIELD> mevl(MEFamilyLow::instance(), // bilinear POU
                  pcfield);
                  

  // View pcoord as integration points so MEValues may be used.
  mevl.Setup(*pelem, MEV::update_sf | MEV::update_sfg, &pintg);
  mevl.ReInit(*pelem); // shape values are now query'able 

  mevl.GetCoordinateValues(&rcoord[0]);

  Real pg[] = {0,0,0};

  // Loop the points, interpolating fields
  for (UInt p = 0; p < npts; p++) {
    for (UInt f = 0; f < flen; f++) {
      for (UInt d = 0; d < sdim; d++) {
        result[(p*flen+f)*sdim+d] = 0;
        for (UInt n = 0; n < nv; n++) {

          Real pval = patches[n]->EvalPatchAndGrad(f, &rcoord[p*sdim], pg);

            result[(p*flen+f)*sdim+d] += mevl.GetShapeValue(p, n)*pg[d] +
             mevl.GetShapeGrads(p, n)[d]*pval;
        } //nv
      } // d
    } // f
  } // npoints

}



// ********** Instantiations ************

template class PatchRecov<MEField<>, double>;
template class PatchRecov<MEField<SField> ,fad_type>;

template class ElemPatch<MEField<> ,double>;
template class ElemPatch<MEField<SField> ,fad_type>;

} // namespace
