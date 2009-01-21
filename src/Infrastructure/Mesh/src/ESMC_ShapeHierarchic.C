// $Id: ESMC_ShapeHierarchic.C,v 1.4.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
#include <ESMC_ShapeHierarchic.h>

#include <ESMC_Quadrature.h>

#include <iostream>


#ifdef ESMC_LAPACK
extern "C" void FTN(dgelsy)(int *,int *,int*,double*,int*,double*,int*,int*,double*,int*,double*,int*,int*);
#endif

namespace ESMCI {
namespace MESH {

static void solve_sys(UInt nsamples, UInt ncoef, const double vals[],
    double mat[], double coef[])
{
#ifdef ESMC_LAPACK
  int m = nsamples, n = ncoef, nrhs = 1, info = 0, rank, ldb;
  ldb = std::max(std::max(m,n),1);
  std::vector<double> rhs(ldb);

  for (UInt i = 0; i < nsamples; i++) {
    rhs[i] = vals[i]; // sizing might not be right, so copy
  }

  std::vector<int> jpvt(ncoef, 0);
  //int lwork = std::max(std::min(m,n)+2*n+1, 2*std::min(m,n)+nrhs);
  // TODO figure this out
  int lwork = 4028;
  std::vector<double> work(lwork, 0);
  double rcond=0.0000000000001;

  FTN(dgelsy)(
    &m, &n, &nrhs, &mat[0], &m, &rhs[0], &ldb, &jpvt[0], &rcond, &rank, &work[0], &lwork, &info);

  for (UInt i = 0; i < ncoef; i++) coef[i] = rhs[i];

#endif
}

// ************* Base Shape Hier ************

void ShapeHier::do_Interpolate(UInt pdim, const double ipoints[], const double fvals[], double mcoef[]) const {
  UInt nnodes = NumNodes();
  for (UInt i = 0; i < nnodes; i++)
    mcoef[i] = fvals[i];

  UInt pofs = nnodes; // keep track of dof index

  UInt nedges = NumEdges();
  UInt total_efunc = 0;
  {
    for (UInt e = 0; e < nedges; e++) {
      UInt nefunc = NumEFunc(e);
      // Form the matrix (nsamples, sfuncs)
      std::vector<double> matT(nefunc*nefunc);
      const double *pcoord = &ipoints[pofs*pdim];
      shape_edge(e, nefunc, nefunc, pcoord, &matT[0]);
      // Annoying, but must transpose
      std::vector<double> mat(nefunc*nefunc);
      for (UInt i = 0; i < nefunc; i++) {
        for (UInt j = 0; j < nefunc; j++) {
          mat[i*nefunc+j] = matT[j*nefunc+i];
        }
      }
      std::vector<double> linvals(nnodes*nefunc);
      std::vector<double> fevals(nefunc);
      // subtract off linear part
      shape_node(nnodes, nefunc, pcoord, &linvals[0]);
      for (UInt i = 0; i < nefunc; i++) {
        double fv = 0;
        for (UInt j = 0; j < nnodes; j++) {
          fv += linvals[i*nnodes + j]*fvals[j];
        }
        fevals[i] = fvals[pofs + i] - fv; // F - Fv
      }
      solve_sys(nefunc, nefunc, &fevals[0], 
        &mat[0], &mcoef[pofs]);

      pofs += nefunc; // advance at each edge
      total_efunc += nefunc;
    } // e
  }

  // For 3d, TODO: Add some face garbage.  Must be added below, since f - fv - fe - ff is used to fit bubbles.

  // And now, sigh, fit the bubbles
  const double *bpoints = &ipoints[pofs*pdim];
  UInt nbubble = NumBubble();
  std::vector<double> matT(nbubble*nbubble);
  shape_elem(nbubble, nbubble, bpoints, &matT[0]);
  std::vector<double> linvals(nnodes*nbubble); // lins at bubble ip
  std::vector<double> evals(total_efunc*nbubble); // edge funcs at bubble ip

  shape_node(nnodes, nbubble, bpoints, &linvals[0]);
  UInt lofs = 0;
  std::vector<int> eofs(nedges+1, 0);
  for (UInt e = 0; e < nedges; e++) {
    UInt nefunc = NumEFunc(e);
    shape_edge(e, nefunc, nbubble, bpoints, &evals[lofs*nbubble]);
    lofs += nefunc;
    eofs[e+1] = eofs[e] + nefunc*nbubble;
  }

  // values of f -fv - fe
  std::vector<double> fevals(nbubble);
  // Form fv+fe
  for (UInt i = 0; i < nbubble; i++) {
    double fv = 0;
    // linear part
    for (UInt j = 0; j < nnodes; j++) {
      fv += linvals[i*nnodes + j]*fvals[j];
    }
    // edge part. Sum over all edges
    lofs = 0;
    for (UInt e = 0; e < nedges; e++) {
      UInt nefunc = NumEFunc(e);
      for (UInt j = 0; j < nefunc; j++) {
        // We use the function values that we computed above here!!
        fv += evals[eofs[e]+i*nefunc+j]*mcoef[nnodes+lofs+j];
      }
      lofs += nefunc;
    }
    fevals[i] = fvals[pofs + i] - fv;
  }
  std::vector<double> mat(nbubble*nbubble);
  // Must transpose
  for (UInt i = 0; i < nbubble; i++) {
    for (UInt j = 0; j < nbubble; j++) {
      mat[i*nbubble+j] = matT[j*nbubble+i];
    }
  }
  solve_sys(nbubble, nbubble, &fevals[0], 
        &mat[0], &mcoef[pofs]);
  
}

// ************ Quadrilateral ************
static void build_quad_dtable(UInt q, std::vector<int> &table) {
  // Int dofs first
  for (UInt i = 0; i < 4; i++) {
    table.push_back(DOF_NODE);
    table.push_back(i); // ordinal
    table.push_back(0); // index
    table.push_back(1); // polarity
  }

  // And now edge dofs
  for (UInt i = 0; i < 4; i++) {
    // q -1 on each edge
    for (UInt j = 2; j <= q; j++) {
      table.push_back(DOF_EDGE);
      table.push_back(i); // ordinal
      table.push_back(j-2);
      (j & 0x01) ?  table.push_back(-1) : table.push_back(1); // orientation
    }
  }

  // And, finally, the interior dofs
  UInt idx = 0;
  for (UInt i = 2; i <= q; i++) {
    for (UInt j = 2; j <= q; j++) {
      table.push_back(DOF_ELEM);
      table.push_back(0); // no ordinal for element
      table.push_back(idx++);
      table.push_back(1); // index
    }
  }
}

static void build_quad_itable(UInt nfunc, UInt q, std::vector<double> &ip) {
  // First the nodes:
  ip.clear(); ip.resize(nfunc*2, 0);
  ip[0] = -1; ip[1] = -1;
  ip[2] = 1; ip[3] = -1;
  ip[4] = 1; ip[5] = 1;
  ip[6] = -1; ip[7] = 1;

  // Now q-1 integration points along the edges.
  UInt ofs = 8;
  std::vector<double> tmp(q-1,0);
  gauss_legendre(q-1, &tmp[0]);

  // edge 0
  for (UInt i = 0; i < q-1; i++) {
    ip[ofs++] = tmp[i];
    ip[ofs++] = -1;
  }
  // edge 1
  for (UInt i = 0; i < q-1; i++) {
    ip[ofs++] = 1;
    ip[ofs++] = tmp[i];
  }
  // edge 2
  for (UInt i = 0; i < q-1; i++) {
    ip[ofs++] = tmp[i];
    ip[ofs++] = 1;
  }
  // edge 3
  for (UInt i = 0; i < q-1; i++) {
    ip[ofs++] = -1;
    ip[ofs++] = tmp[i];
  }

  // And now, the beloved interior (tensor product
  for (UInt i = 2; i <= q; i++) {
    for (UInt j = 2; j <= q; j++) {
      ip[ofs++] = tmp[i-2];
      ip[ofs++] = tmp[j-2];
    }
  }
  ThrowRequire(ofs == nfunc*2);
}

std::map<UInt,QuadHier*> QuadHier::qhMap;

QuadHier *QuadHier::instance(UInt _q) {
  std::map<UInt,QuadHier*>::iterator qi =
    qhMap.find(_q);
  QuadHier *qp;
  if (qi == qhMap.end()) {
    qp = new QuadHier(_q);
    qhMap[_q] = qp;
  } else qp = qi->second;
  return qp;
}

QuadHier::QuadHier(UInt _q) :
q(_q),
dtable(),
nfunc(0),
m_name(),
ild(_q+1),
ilf(_q+1),
iPoints()
{
  build_quad_dtable(q, dtable);
  nfunc = dtable.size() / 4;
  build_quad_itable(nfunc, q, iPoints);
  char buf[512];
  sprintf(buf, "QuadHier_%d", q);
  m_name = std::string(buf);

  // Initialize the integrated legendre polys
  for (UInt i = 0; i <= q; i++) {
    ild[i].Init(i);
    ilf[i].Init(i);
  }
}

// Eval shape funcs by node, edge, etc.
// Note that the stride is ndofs, so these can interleave into a full array of points.
template<typename Real>
void qh_shape_node(UInt pdim, UInt ndofs, const std::vector<ILegendre<Real> > &func,
              UInt npts, const Real pcoord[], Real results[]) {
  for (UInt p = 0; p < npts; p++) {
    Real xi = pcoord[p*pdim], eta = pcoord[p*pdim+1];

    // Nodes
    results[p*ndofs] = EvalPoly<ILegendre<Real> >()(func[0], xi)*
                       EvalPoly<ILegendre<Real> >()(func[0], eta);
    results[p*ndofs+1] = EvalPoly<ILegendre<Real> >()(func[1], xi)*
                       EvalPoly<ILegendre<Real> >()(func[0], eta);
    results[p*ndofs+2] = EvalPoly<ILegendre<Real> >()(func[1], xi)*
                       EvalPoly<ILegendre<Real> >()(func[1], eta);
    results[p*ndofs+3] = EvalPoly<ILegendre<Real> >()(func[0], xi)*
                       EvalPoly<ILegendre<Real> >()(func[1], eta);
  }
}

template<typename Real>
void qh_shape_edge(UInt edge, UInt pdim, UInt ndofs, const std::vector<ILegendre<Real> > &func,
              UInt npts, const Real pcoord[], Real results[]) {
  UInt q = func.size()-1;
  for (UInt p = 0; p < npts; p++) {
    Real xi = pcoord[p*pdim], eta = pcoord[p*pdim+1];

    // Edges
    UInt ofs = 0; // offset
    int ftab[] = {0,1,1,0}; // which blending func
     Real pc[2];
     pc[1] = (edge & 0x01) ? eta : xi;
     pc[0] = (edge & 0x01) ? xi : eta;
     const ILegendre<Real> &f1 = func[ftab[edge]];
     for (UInt j = 2; j <= q; j++) {
       TensorPolynomial<ILegendre<Real> > tp(&f1, &func[j]);
       // Make it so the left hump of the odds is always the
       // same sign so that the orientation doesn't depend on
       // which edge is hitting which edge.
       results[p*ndofs+ ofs++] = (edge >= 2 && j & 0x01) ? -1*EvalTensorPoly(2, pc, tp) :
                          EvalTensorPoly(2, pc, tp) ;
     }
  }
}

template<typename Real>
void qh_shape_bubble(UInt pdim, UInt ndofs, const std::vector<ILegendre<Real> > &func,
              UInt npts, const Real pcoord[], Real results[]) {
  UInt q = func.size()-1;
  for (UInt p = 0; p < npts; p++) {
    UInt ofs = 0;
    // Bubbles
    for (UInt i = 2; i <= q; i++) {
      for (UInt j = 2; j <= q; j++) {
        TensorPolynomial<ILegendre<Real> > tp(&func[i], &func[j]);
        results[p*ndofs + ofs++] = EvalTensorPoly(2, &pcoord[p*pdim], tp);
      }
    }

  } // npts
}

template<typename Real>
void qh_shape(UInt pdim, UInt ndofs, const std::vector<ILegendre<Real> > &func,
              UInt npts, const Real pcoord[], Real results[]) {
  UInt q = func.size()-1;

  qh_shape_node(pdim, ndofs, func, npts, pcoord, &results[0]);
  qh_shape_edge(0, pdim, ndofs, func, npts, pcoord, &results[4]);
  qh_shape_edge(1, pdim, ndofs, func, npts, pcoord, &results[4+(q-1)]);
  qh_shape_edge(2, pdim, ndofs, func, npts, pcoord, &results[4+2*(q-1)]);
  qh_shape_edge(3, pdim, ndofs, func, npts, pcoord, &results[4+3*(q-1)]);
  qh_shape_bubble(pdim, ndofs, func, npts, pcoord, &results[4+4*(q-1)]);

}

void QuadHier::shape(UInt npts, const double pcoord[], double results[]) const {
  qh_shape(ParametricDim(), NumFunctions(), ild, npts, pcoord, results);
}

void QuadHier::shape(UInt npts, const fad_type pcoord[], fad_type results[]) const {
  qh_shape(ParametricDim(), NumFunctions(), ilf, npts, pcoord, results);
}

// Return shape values at node
void QuadHier::shape_node(UInt stride, // how to stride when storing results
                          UInt npts, // how many points to evaluate at
                          const double *pcoord, // parametric coords of point
                          double res[] 
                         ) const
{
  qh_shape_node(ParametricDim(), stride, ild, npts, pcoord, res);
}

// Return shape values at edge points
void QuadHier::shape_edge(UInt edge, // which edge
                          UInt stride, // how to stride when storing results
                          UInt npts, // how many points to evaluate at
                          const double *pcoord, // parametric coords of point
                          double res[] 
                         ) const
{
  qh_shape_edge(edge, ParametricDim(), stride, ild, npts, pcoord, res);
}

// Return shape values at face points
void QuadHier::shape_face(UInt face, // which face
                          UInt stride, // how to stride when storing results
                          UInt npts, // how many points to evaluate at
                          const double *pcoord, // parametric coords of point
                          double res[] 
                         ) const
{
  Throw() << "No face on Quad hier, please dont call";
}

// Return shape values at face points
void QuadHier::shape_elem(UInt stride, // how to stride when storing results
                          UInt npts, // how many points to evaluate at
                          const double *pcoord, // parametric coords of point
                          double res[] 
                         ) const
{
  qh_shape_bubble(ParametricDim(), stride, ild, npts, pcoord, res);
}

void QuadHier::Interpolate(const double fvals[], double mcoef[]) const {
  do_Interpolate(ParametricDim(), &iPoints[0], fvals, mcoef);
}

template<typename Real>
void qh_shape_grads(UInt pdim, UInt ndofs, const std::vector<ILegendre<Real> > &func,
              UInt npts, const Real pcoord[], Real results[]) {
  UInt q = func.size()-1;
  for (UInt p = 0; p < npts; p++) {
    Real xi = pcoord[p*pdim], eta = pcoord[p*pdim+1];
    // Nodes
    EvalTensorPolyDeriv(pdim, &pcoord[p*pdim],
               TensorPolynomial<ILegendre<Real> >(&func[0], &func[0]),
               &results[p*ndofs*pdim]);
    EvalTensorPolyDeriv(pdim, &pcoord[p*pdim],
               TensorPolynomial<ILegendre<Real> >(&func[1], &func[0]),
               &results[p*(ndofs+1)*pdim]);
    EvalTensorPolyDeriv(pdim, &pcoord[p*pdim],
               TensorPolynomial<ILegendre<Real> >(&func[1], &func[1]),
               &results[p*(ndofs+2)*pdim]);
    EvalTensorPolyDeriv(pdim, &pcoord[p*pdim],
               TensorPolynomial<ILegendre<Real> >(&func[0], &func[1]),
               &results[p*(ndofs+3)*pdim]);

    // Edges
    UInt ofs = 4; // offset
    int ftab[] = {0,1,1,0}; // which blending func
    for (UInt i = 0; i < 4; i++) {
      const ILegendre<Real> &f1 = func[ftab[i]];
      for (UInt j = 2; j <= q; j++) {
        Real pc[2];
        pc[1] = (j & 0x01) ? eta : xi;
        pc[0] = (j & 0x01) ? xi : eta;
        TensorPolynomial<ILegendre<Real> > tp(&f1, &func[j]);
        EvalTensorPolyDeriv(2, pc, tp, &results[p*(ndofs+ofs++)*pdim]);
      }
    }

    // Bubbles
    for (UInt i = 2; i <= q; i++) {
      for (UInt j = 2; j <= q; j++) {
        TensorPolynomial<ILegendre<Real> > tp(&func[i], &func[j]);
        EvalTensorPolyDeriv(2, &pcoord[p*pdim], tp, &results[p*(ndofs+ofs++)*pdim]);
      }
    }
   
  }

}

void QuadHier::shape_grads(UInt npts, const double pcoord[], double results[]) const {
  qh_shape_grads(ParametricDim(), NumFunctions(), ild, npts, pcoord, results);
}

void QuadHier::shape_grads(UInt npts, const fad_type pcoord[], fad_type results[]) const {
  qh_shape_grads(ParametricDim(), NumFunctions(), ilf, npts, pcoord, results);
}


// ************ Triangle ************
static void build_tri_dtable(UInt q, std::vector<int> &table) {
  // Int dofs first
  for (UInt i = 0; i < 3; i++) {
    table.push_back(DOF_NODE);
    table.push_back(i); // ordinal
    table.push_back(0); // index
    table.push_back(1); // polarity
  }

  // And now edge dofs
  for (UInt i = 0; i < 3; i++) {
    // q -1 on each edge
    for (UInt j = 2; j <= q; j++) {
      table.push_back(DOF_EDGE);
      table.push_back(i); // ordinal
      table.push_back(j-2);
      (j & 0x01) ?  table.push_back(-1) : table.push_back(1); // orientation
    }
  }

  // And, finally, the interior dofs
  UInt idx = 0;
  for (UInt i = 1; i <= q; i++) {
    for (UInt j = 1; j <= q; j++) {
      if ((i+j) < q) {
        table.push_back(DOF_ELEM);
        table.push_back(0); // no ordinal for element
        table.push_back(idx++);
        table.push_back(1); // index
      }
    }
  }
}

static void build_tri_itable(UInt nfunc, UInt q, std::vector<double> &ip) {
  // First the nodes:
  ip.clear(); ip.resize(nfunc*2, 0);
  ip[0] = 0; ip[1] = 0;
  ip[2] = 1; ip[3] = 0;
  ip[4] = 0; ip[5] = 1;

  // Now q-1 integration points along the edges.
  UInt ofs = 6;
  std::vector<double> tmp(q-1,0);
  gauss_legendre(q-1, &tmp[0]);

  // Scale into 0,1
  for (UInt i = 0; i < q-1; i++)
    tmp[i] = 0.5*(tmp[i] + 1);

  // edge 0
  for (UInt i = 0; i < q-1; i++) {
    ip[ofs++] = tmp[i];
    ip[ofs++] = 0;
  }
  // edge 1
  for (UInt i = 0; i < q-1; i++) {
    ip[ofs++] = 1 - tmp[i];
    ip[ofs++] = tmp[i];
  }
  // edge 2
  for (UInt i = 0; i < q-1; i++) {
    ip[ofs++] = 0;
    ip[ofs++] = tmp[i];
  }

  // And now, the beloved interior (tensor product
  for (UInt i = 1; i <= q; i++) {
    for (UInt j = 1; j <= q; j++) {
      if ((i+j) < q) {
        ip[ofs++] = tmp[i-1];
        ip[ofs++] = tmp[j-1];
std::cout << ip[ofs-2] << " " << ip[ofs-1] << " +=" << ip[ofs-2] + ip[ofs-1] << std::endl;
      }
    }
  }
  ThrowRequire(ofs == nfunc*2);
}

std::map<UInt,TriHier*> TriHier::qhMap;

TriHier *TriHier::instance(UInt _q) {
  std::map<UInt,TriHier*>::iterator qi =
    qhMap.find(_q);
  TriHier *qp;
  if (qi == qhMap.end()) {
    qp = new TriHier(_q);
    qhMap[_q] = qp;
  } else qp = qi->second;
  return qp;
}

TriHier::TriHier(UInt _q) :
q(_q),
dtable(),
nfunc(0),
m_name(),
ild(_q-1),
ilf(_q-1),
iPoints()
{
  build_tri_dtable(q, dtable);
  nfunc = dtable.size() / 4;
  build_tri_itable(nfunc, q, iPoints);
  char buf[512];
  sprintf(buf, "TriHier_%d", q);
  m_name = std::string(buf);

  // Initialize the integrated legendre polys
  for (UInt i = 0; i <= q-2; i++) {
    ild[i].Init(i);
    ilf[i].Init(i);
  }
}

template<typename Real>
void th_shape_node(UInt pdim, UInt stride, UInt npts, const Real pcoord[], Real results[]) {
  for (UInt p = 0; p < npts; p++) {
    Real xi = pcoord[p*pdim], eta = pcoord[p*pdim+1];

    // Nodes
    results[p*stride] = 1 - xi - eta;
                      
    results[p*stride+1] = xi;
                       
    results[p*stride+2] = eta;
  }
}

template<typename Real>
void th_shape_edge(UInt edge, UInt pdim, UInt stride, const std::vector<ILKernel<Real> > &func,
              UInt npts, const Real pcoord[], Real results[]) {
  UInt q = func.size()+1;
  for (UInt p = 0; p < npts; p++) {
    Real xi = pcoord[p*pdim], eta = pcoord[p*pdim+1];

    // Edges
    UInt ofs = 0; // offset
    Real blend[] = {-(1-xi-eta)*xi,-xi*eta,-(1-xi-eta)*eta};
    Real aval[] = {xi-(1-xi-eta), eta-xi, (1-xi-eta) -eta};
      //Real blend = i == 0 ? (1-xi-eta)*xi : (i == 1 ? xi*eta : (1-xi-eta)*eta); 
      //Real aval = i == 0 ? (xi-(1-xi-eta)) : (i == 1 ? eta - xi : (1-xi-eta)-eta); 
      for (UInt j = 2; j <= q; j++) {
        // Make it so the left hump of the odds is always the
        // same sign so that the orientation doesn't depend on
        // which edge is hitting which edge.
        results[p*stride+ofs++] = blend[edge]*EvalPoly<ILKernel<Real> >()(func[j-2], aval[edge]);
      }
  } // npts
}

template<typename Real>
void th_shape_bubble(UInt pdim, UInt stride, const std::vector<ILKernel<Real> > &func,
              UInt npts, const Real pcoord[], Real results[]) {
  UInt q = func.size()+1;
  for (UInt p = 0; p < npts; p++) {
    Real xi = pcoord[p*pdim], eta = pcoord[p*pdim+1];

    // Bubbles
    UInt ofs = 0;
    for (UInt i = 1; i <= q; i++) {
      for (UInt j = 1; j <= q; j++) {
        if ((i+j) < q) {
          results[p*stride + ofs++] = (1-xi-eta)*xi*eta*
              EvalPoly<ILKernel<Real> >()(func[i-1], (xi-(1-xi-eta)))*EvalPoly<ILKernel<Real> >()(func[j-1], ((1-xi-eta)-eta));
        }
      }
    }

  } // npts
}

template<typename Real>
void th_shape(UInt pdim, UInt ndofs, const std::vector<ILKernel<Real> > &func,
              UInt npts, const Real pcoord[], Real results[]) {

  UInt q = func.size()+1;
  th_shape_node(pdim, ndofs, npts, pcoord, &results[0]);
  th_shape_edge(0, pdim, ndofs, func, npts, pcoord, &results[3]);
  th_shape_edge(1, pdim, ndofs, func, npts, pcoord, &results[3+(q-1)]);
  th_shape_edge(2, pdim, ndofs, func, npts, pcoord, &results[3+2*(q-1)]);
  th_shape_bubble(pdim, ndofs, func, npts, pcoord, &results[3+3*(q-1)]);

}

void TriHier::shape(UInt npts, const double pcoord[], double results[]) const {
  th_shape(ParametricDim(), NumFunctions(), ild, npts, pcoord, results);
}

void TriHier::shape(UInt npts, const fad_type pcoord[], fad_type results[]) const {
  th_shape(ParametricDim(), NumFunctions(), ilf, npts, pcoord, results);
}

template<typename Real>
void th_shape_grads(UInt pdim, UInt ndofs, const std::vector<ILKernel<Real> > &func,
              UInt npts, const Real pcoord[], Real results[]) {

  Throw() << "TriHier grads not implemented (yet)";
}

void TriHier::shape_grads(UInt npts, const double pcoord[], double results[]) const {
  th_shape_grads(ParametricDim(), NumFunctions(), ild, npts, pcoord, results);
}

void TriHier::shape_grads(UInt npts, const fad_type pcoord[], fad_type results[]) const {
  th_shape_grads(ParametricDim(), NumFunctions(), ilf, npts, pcoord, results);
}
// Return shape values at node
void TriHier::shape_node(UInt stride, // how to stride when storing results
                          UInt npts, // how many points to evaluate at
                          const double *pcoord, // parametric coords of point
                          double res[] 
                         ) const
{
  th_shape_node(ParametricDim(), stride, npts, pcoord, res);
}

// Return shape values at edge points
void TriHier::shape_edge(UInt edge, // which edge
                          UInt stride, // how to stride when storing results
                          UInt npts, // how many points to evaluate at
                          const double *pcoord, // parametric coords of point
                          double res[] 
                         ) const
{
  th_shape_edge(edge, ParametricDim(), stride, ild, npts, pcoord, res);
}

// Return shape values at face points
void TriHier::shape_face(UInt face, // which face
                          UInt stride, // how to stride when storing results
                          UInt npts, // how many points to evaluate at
                          const double *pcoord, // parametric coords of point
                          double res[] 
                         ) const
{
  Throw() << "No face on Tri hier, please dont call";
}

// Return shape values at face points
void TriHier::shape_elem(UInt stride, // how to stride when storing results
                          UInt npts, // how many points to evaluate at
                          const double *pcoord, // parametric coords of point
                          double res[] 
                         ) const
{
  th_shape_bubble(ParametricDim(), stride, ild, npts, pcoord, res);
}

void TriHier::Interpolate(const double fvals[], double mcoef[]) const {
  do_Interpolate(ParametricDim(), &iPoints[0], fvals, mcoef);
}


// ************ Hexahedron ************
static void build_hex_dtable(UInt q, std::vector<int> &table) {
  // Int dofs first
  for (UInt i = 0; i < 8; i++) {
    table.push_back(DOF_NODE);
    table.push_back(i); // ordinal
    table.push_back(0); // index
    table.push_back(1); // polarity
  }

  // And now edge dofs
  for (UInt i = 0; i < 12; i++) {
    // q -1 on each edge
    for (UInt j = 2; j <= q; j++) {
      table.push_back(DOF_EDGE);
      table.push_back(i); // ordinal
      table.push_back(j-2);
      (j & 0x01) ?  table.push_back(-1) : table.push_back(1); // orientation
    }
  }

  // Faces
  UInt idx = 0;
  for (UInt f = 0; f < 6; f++) {
    for (UInt i = 2; i <= q; i++) {
      for (UInt j = 2; j <= q; j++) {
        table.push_back(DOF_ELEM);
        table.push_back(f); 
        table.push_back(idx++);
        if (((i + j) % 2 ) == 0) table.push_back(1); else table.push_back(-1); // negative orders
      }
    }
  } // face

  // And, lastly, bubbles
  for (UInt i = 2; i <= q; i++) {
    for (UInt j = 2; j <= q; j++) {
      for (UInt k = 2; k <= q; k++) {
        table.push_back(DOF_ELEM);
        table.push_back(0); 
        table.push_back(idx++);
        table.push_back(1);
      }
    }
  } 
}

static void build_hex_itable(UInt nfunc, UInt q, std::vector<double> &ip) {
  
  // First the nodes:
  ip.clear(); ip.resize(nfunc*2, 0);
  ip[0] = -1; ip[1] = -1;
  ip[2] = 1; ip[3] = -1;
  ip[4] = 1; ip[5] = 1;
  ip[6] = -1; ip[7] = 1;

  // Now q-1 integration points along the edges.
  UInt ofs = 8;
  std::vector<double> tmp(q-1,0);
  gauss_legendre(q-1, &tmp[0]);

  // edge 0
  for (UInt i = 0; i < q-1; i++) {
    ip[ofs++] = tmp[i];
    ip[ofs++] = -1;
  }
  // edge 1
  for (UInt i = 0; i < q-1; i++) {
    ip[ofs++] = 1;
    ip[ofs++] = tmp[i];
  }
  // edge 2
  for (UInt i = 0; i < q-1; i++) {
    ip[ofs++] = tmp[i];
    ip[ofs++] = 1;
  }
  // edge 3
  for (UInt i = 0; i < q-1; i++) {
    ip[ofs++] = -1;
    ip[ofs++] = tmp[i];
  }

  // And now, the beloved interior (tensor product
  for (UInt i = 2; i <= q; i++) {
    for (UInt j = 2; j <= q; j++) {
      ip[ofs++] = tmp[i-2];
      ip[ofs++] = tmp[j-2];
    }
  }
  ThrowRequire(ofs == nfunc*2);
}

std::map<UInt,HexHier*> HexHier::qhMap;

HexHier *HexHier::instance(UInt _q) {
  std::map<UInt,HexHier*>::iterator qi =
    qhMap.find(_q);
  HexHier *qp;
  if (qi == qhMap.end()) {
    qp = new HexHier(_q);
    qhMap[_q] = qp;
  } else qp = qi->second;
  return qp;
}

HexHier::HexHier(UInt _q) :
q(_q),
dtable(),
nfunc(0),
m_name(),
ild(_q+1),
ilf(_q+1),
iPoints()
{
  build_hex_dtable(q, dtable);
  nfunc = dtable.size() / 4;
  build_hex_itable(nfunc, q, iPoints);
  char buf[512];
  sprintf(buf, "HexHier_%d", q);
  m_name = std::string(buf);

  // Initialize the integrated legendre polys
  for (UInt i = 0; i <= q; i++) {
    ild[i].Init(i);
    ilf[i].Init(i);
  }
}

// Eval shape funcs by node, edge, etc.
// Note that the stride is ndofs, so these can interleave into a full array of points.
template<typename Real>
void hh_shape_node(UInt pdim, UInt ndofs, const std::vector<ILegendre<Real> > &func,
              UInt npts, const Real pcoord[], Real results[]) {
  for (UInt p = 0; p < npts; p++) {
    Real xi = pcoord[p*pdim], eta = pcoord[p*pdim+1];

    // Nodes
    results[p*ndofs] = EvalPoly<ILegendre<Real> >()(func[0], xi)*
                       EvalPoly<ILegendre<Real> >()(func[0], eta);
    results[p*ndofs+1] = EvalPoly<ILegendre<Real> >()(func[1], xi)*
                       EvalPoly<ILegendre<Real> >()(func[0], eta);
    results[p*ndofs+2] = EvalPoly<ILegendre<Real> >()(func[1], xi)*
                       EvalPoly<ILegendre<Real> >()(func[1], eta);
    results[p*ndofs+3] = EvalPoly<ILegendre<Real> >()(func[0], xi)*
                       EvalPoly<ILegendre<Real> >()(func[1], eta);
  }
}

template<typename Real>
void hh_shape_edge(UInt edge, UInt pdim, UInt ndofs, const std::vector<ILegendre<Real> > &func,
              UInt npts, const Real pcoord[], Real results[]) {
  UInt q = func.size()-1;
  for (UInt p = 0; p < npts; p++) {
    Real xi = pcoord[p*pdim], eta = pcoord[p*pdim+1];

    // Edges
    UInt ofs = 0; // offset
    int ftab[] = {0,1,1,0}; // which blending func
     Real pc[2];
     pc[1] = (edge & 0x01) ? eta : xi;
     pc[0] = (edge & 0x01) ? xi : eta;
     const ILegendre<Real> &f1 = func[ftab[edge]];
     for (UInt j = 2; j <= q; j++) {
       TensorPolynomial<ILegendre<Real> > tp(&f1, &func[j]);
       // Make it so the left hump of the odds is always the
       // same sign so that the orientation doesn't depend on
       // which edge is hitting which edge.
       results[p*ndofs+ ofs++] = (edge >= 2 && j & 0x01) ? -1*EvalTensorPoly(2, pc, tp) :
                          EvalTensorPoly(2, pc, tp) ;
     }
  }
}

template<typename Real>
void hh_shape_bubble(UInt pdim, UInt ndofs, const std::vector<ILegendre<Real> > &func,
              UInt npts, const Real pcoord[], Real results[]) {
  UInt q = func.size()-1;
  for (UInt p = 0; p < npts; p++) {
    UInt ofs = 0;
    // Bubbles
    for (UInt i = 2; i <= q; i++) {
      for (UInt j = 2; j <= q; j++) {
        TensorPolynomial<ILegendre<Real> > tp(&func[i], &func[j]);
        results[p*ndofs + ofs++] = EvalTensorPoly(2, &pcoord[p*pdim], tp);
      }
    }

  } // npts
}

template<typename Real>
void hh_shape(UInt pdim, UInt ndofs, const std::vector<ILegendre<Real> > &func,
              UInt npts, const Real pcoord[], Real results[]) {
  UInt q = func.size()-1;

  qh_shape_node(pdim, ndofs, func, npts, pcoord, &results[0]);
  qh_shape_edge(0, pdim, ndofs, func, npts, pcoord, &results[4]);
  qh_shape_edge(1, pdim, ndofs, func, npts, pcoord, &results[4+(q-1)]);
  qh_shape_edge(2, pdim, ndofs, func, npts, pcoord, &results[4+2*(q-1)]);
  qh_shape_edge(3, pdim, ndofs, func, npts, pcoord, &results[4+3*(q-1)]);
  qh_shape_bubble(pdim, ndofs, func, npts, pcoord, &results[4+4*(q-1)]);

}

void HexHier::shape(UInt npts, const double pcoord[], double results[]) const {
  hh_shape(ParametricDim(), NumFunctions(), ild, npts, pcoord, results);
}

void HexHier::shape(UInt npts, const fad_type pcoord[], fad_type results[]) const {
  hh_shape(ParametricDim(), NumFunctions(), ilf, npts, pcoord, results);
}

// Return shape values at node
void HexHier::shape_node(UInt stride, // how to stride when storing results
                          UInt npts, // how many points to evaluate at
                          const double *pcoord, // parametric coords of point
                          double res[] 
                         ) const
{
  hh_shape_node(ParametricDim(), stride, ild, npts, pcoord, res);
}

// Return shape values at edge points
void HexHier::shape_edge(UInt edge, // which edge
                          UInt stride, // how to stride when storing results
                          UInt npts, // how many points to evaluate at
                          const double *pcoord, // parametric coords of point
                          double res[] 
                         ) const
{
  hh_shape_edge(edge, ParametricDim(), stride, ild, npts, pcoord, res);
}

// Return shape values at face points
void HexHier::shape_face(UInt face, // which face
                          UInt stride, // how to stride when storing results
                          UInt npts, // how many points to evaluate at
                          const double *pcoord, // parametric coords of point
                          double res[] 
                         ) const
{
  
}

// Return shape values at face points
void HexHier::shape_elem(UInt stride, // how to stride when storing results
                          UInt npts, // how many points to evaluate at
                          const double *pcoord, // parametric coords of point
                          double res[] 
                         ) const
{
  hh_shape_bubble(ParametricDim(), stride, ild, npts, pcoord, res);
}

void HexHier::Interpolate(const double fvals[], double mcoef[]) const {
  do_Interpolate(ParametricDim(), &iPoints[0], fvals, mcoef);
}

template<typename Real>
void hh_shape_grads(UInt pdim, UInt ndofs, const std::vector<ILegendre<Real> > &func,
              UInt npts, const Real pcoord[], Real results[]) {
  UInt q = func.size()-1;
  for (UInt p = 0; p < npts; p++) {
    Real xi = pcoord[p*pdim], eta = pcoord[p*pdim+1];
    // Nodes
    EvalTensorPolyDeriv(pdim, &pcoord[p*pdim],
               TensorPolynomial<ILegendre<Real> >(&func[0], &func[0]),
               &results[p*ndofs*pdim]);
    EvalTensorPolyDeriv(pdim, &pcoord[p*pdim],
               TensorPolynomial<ILegendre<Real> >(&func[1], &func[0]),
               &results[p*(ndofs+1)*pdim]);
    EvalTensorPolyDeriv(pdim, &pcoord[p*pdim],
               TensorPolynomial<ILegendre<Real> >(&func[1], &func[1]),
               &results[p*(ndofs+2)*pdim]);
    EvalTensorPolyDeriv(pdim, &pcoord[p*pdim],
               TensorPolynomial<ILegendre<Real> >(&func[0], &func[1]),
               &results[p*(ndofs+3)*pdim]);

    // Edges
    UInt ofs = 4; // offset
    int ftab[] = {0,1,1,0}; // which blending func
    for (UInt i = 0; i < 4; i++) {
      const ILegendre<Real> &f1 = func[ftab[i]];
      for (UInt j = 2; j <= q; j++) {
        Real pc[2];
        pc[1] = (j & 0x01) ? eta : xi;
        pc[0] = (j & 0x01) ? xi : eta;
        TensorPolynomial<ILegendre<Real> > tp(&f1, &func[j]);
        EvalTensorPolyDeriv(2, pc, tp, &results[p*(ndofs+ofs++)*pdim]);
      }
    }

    // Bubbles
    for (UInt i = 2; i <= q; i++) {
      for (UInt j = 2; j <= q; j++) {
        TensorPolynomial<ILegendre<Real> > tp(&func[i], &func[j]);
        EvalTensorPolyDeriv(2, &pcoord[p*pdim], tp, &results[p*(ndofs+ofs++)*pdim]);
      }
    }
   
  }

}

void HexHier::shape_grads(UInt npts, const double pcoord[], double results[]) const {
  hh_shape_grads(ParametricDim(), NumFunctions(), ild, npts, pcoord, results);
}

void HexHier::shape_grads(UInt npts, const fad_type pcoord[], fad_type results[]) const {
  hh_shape_grads(ParametricDim(), NumFunctions(), ilf, npts, pcoord, results);
}

} // namespace
} // namespace
