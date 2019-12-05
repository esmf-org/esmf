// Copyright (c) 2010, Lawrence Livermore National Security, LLC. Produced at
// the Lawrence Livermore National Laboratory. LLNL-CODE-443211. All Rights
// reserved. See file COPYRIGHT for details.
//
// This file is part of the MFEM library. For more information and source code
// availability see http://mfem.org.
//
// MFEM is free software; you can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License (as published by the Free
// Software Foundation) version 2.1 dated February 1999.

#include "HypreParVector.hpp"
#include "HypreParMatrix.hpp"

#ifdef MOAB_HAVE_MPI

#include <fstream>
#include <iostream>
#include <cmath>
#include <cstdlib>
#include <cassert>

using namespace std;

namespace moab
{
#define moab_hypre_assert(a,b) {}
#define moab_hypre_assert_t(a,b) { \
    if (!a) { \
      std::cout << "HYPRE Error: " << b << std::endl; \
      exit (-1); \
    }


  HypreParVector::HypreParVector(moab::ParallelComm *p_comm) : pcomm(p_comm)
  {
    x = NULL;
    initialized = size = gsize = rstart = rend = 0;
  }

  HypreParVector::HypreParVector(moab::ParallelComm *p_comm, HYPRE_Int glob_size,
                                 HYPRE_Int p_irstart, HYPRE_Int p_irend) : rstart(p_irstart), rend(p_irend), pcomm(p_comm)
  {
    HYPRE_IJVectorCreate(pcomm->comm(), rstart, rend, &x);
    HYPRE_IJVectorSetObjectType(x, HYPRE_PARCSR);
    HYPRE_IJVectorInitialize(x);
    HYPRE_IJVectorAssemble(x);
    HYPRE_IJVectorGetObject(x, (void **) &x_par);
    size = rstart - rend;
    gsize = glob_size;
    own_ParVector = 1;
    initialized = 1;
  }


  HypreParVector::HypreParVector(const HypreParVector &y)
  {
    pcomm = y.pcomm;
    rstart = y.rstart;
    rend = y.rend;
    size = y.size;
    gsize = y.gsize;
    HYPRE_IJVectorCreate(pcomm->comm(), y.rstart, y.rend, &x);
    HYPRE_IJVectorSetObjectType(x, HYPRE_PARCSR);
    HYPRE_IJVectorInitialize(x);
    HYPRE_IJVectorAssemble(x);
    HYPRE_IJVectorGetObject(x, (void **) &x_par);
    HYPRE_Complex    *array=NULL;
    HYPRE_IJVectorGetValues(y.x, y.size, NULL, array);
    HYPRE_IJVectorSetValues(x, size, NULL, array);
    array = NULL;
    own_ParVector = 1;
    initialized = 1;
  }

  HypreParVector::HypreParVector(HypreParMatrix &A, int tr)
  {
    pcomm = A.GetParallelCommunicator();
    int *part;

    if (tr) {
      part = A.ColPart();

    } else part = A.RowPart();

    HYPRE_IJVectorCreate(pcomm->comm(), part[0], part[1], &x);
    HYPRE_IJVectorSetObjectType(x, HYPRE_PARCSR);
    HYPRE_IJVectorInitialize(x);
    HYPRE_IJVectorAssemble(x);
    HYPRE_IJVectorGetObject(x, (void **) &x_par);
    // if (!tr)
    // {
    //    x_par = hypre_ParVectorInDomainOf(static_cast<hypre_ParCSRMatrix*>(A.A_parcsr));
    // }
    // else
    // {
    //    x_par = hypre_ParVectorInRangeOf(static_cast<hypre_ParCSRMatrix*>(A.A_parcsr));
    // }
    // comm = hypre_ParVectorComm(x);
    rstart = part[0];
    rend = part[1];
    size = rstart - rend;
    own_ParVector = 1;
    initialized = 1;
  }


  HypreParVector::operator HYPRE_IJVector() const
  {
    return x;
  }

// #ifndef HYPRE_PAR_VECTOR_STRUCT
  HypreParVector::operator HYPRE_ParVector() const
  {
    return x_par;
  }
// #endif


  HypreParVector& HypreParVector::operator=(double d)
  {
    hypre_ParVectorSetConstantValues(x_par,d);
    return *this;
  }

  HypreParVector &HypreParVector::operator=(const HypreParVector &y)
  {
#ifndef NDEBUG

    if (this->GlobalSize() != y.GlobalSize() ) { // || local_size != y.local_size) {
      MB_SET_ERR_RET_VAL("HypreParVector::operator failed. Incompatible vector sizes", *this);
    }

#endif
    pcomm = y.pcomm;
    rstart = y.rstart;
    rend = y.rend;
    size = y.size;

    if (x) {
      HYPRE_IJVectorDestroy(x);
    }

    x = y.x;
    x_par = y.x_par;
    own_ParVector = 0;
    initialized = y.initialized;
    return *this;
  }

  HYPRE_Int HypreParVector::resize(HYPRE_Int glob_size, HYPRE_Int p_irstart, HYPRE_Int p_irend)
  {
    if (initialized ||
        x != NULL) MB_SET_ERR_RET_VAL("Vector is already initialized and partitioned", -1);

    HYPRE_IJVectorCreate(this->pcomm->comm(), p_irstart, p_irend, &x);
    HYPRE_IJVectorSetObjectType(x, HYPRE_PARCSR);
    HYPRE_IJVectorInitialize(x);
    HYPRE_IJVectorAssemble(x);
    HYPRE_IJVectorGetObject(x, (void **) &x_par);
    rstart = p_irstart;
    rend = p_irend;
    size = rstart - rend;
    own_ParVector = 1;
    initialized = 1;
    return 0;
  }

  HYPRE_Int HypreParVector::SetData(HYPRE_Complex *p_data, HYPRE_Int *p_col)
  {
    return HYPRE_IJVectorSetValues(x, size, p_col, p_data);
  }

  HYPRE_Int HypreParVector::AddData(HYPRE_Complex *p_data, HYPRE_Int *p_col)
  {
    return HYPRE_IJVectorAddToValues(x, size, p_col, p_data);
  }

  HYPRE_Int HypreParVector::GetValues(const int ndata, const HYPRE_Int *indices,
                                      HYPRE_Complex *const _data) const
  {
    return HYPRE_IJVectorGetValues(x, ndata, indices, _data);
  }

  HYPRE_Int HypreParVector::SetValues(const int ndata, const HYPRE_Int *indices,
                                      const HYPRE_Complex *const _data)
  {
    return HYPRE_IJVectorSetValues(x, ndata, indices, _data);
  }

  HYPRE_Int HypreParVector::AddValues(const int ndata, const HYPRE_Int *indices,
                                      const HYPRE_Complex *const _data)
  {
    return HYPRE_IJVectorAddToValues(x, ndata, indices, _data);
  }

  HYPRE_Int HypreParVector::GetValue(HYPRE_Int index, HYPRE_Complex *const _data) const
  {
    return HYPRE_IJVectorGetValues(x, 1, &index, _data);
  }

  HYPRE_Int HypreParVector::SetValue(const HYPRE_Int index, const HYPRE_Complex _data)
  {
    return HYPRE_IJVectorSetValues(x, 1, &index, &_data);
  }

  HYPRE_Int HypreParVector::AddValue(const HYPRE_Int index, const HYPRE_Complex _data)
  {
    return HYPRE_IJVectorAddToValues(x, 1, &index, &_data);
  }

  HYPRE_Int HypreParVector::FinalizeAssembly()
  {
    return HYPRE_IJVectorAssemble(x);
  }

  HYPRE_Int HypreParVector::verbosity(const HYPRE_Int level)
  {
    return HYPRE_IJVectorSetPrintLevel(x, level);
  }

  void HypreParVector::Print(const char *fname) const
  {
    HYPRE_IJVectorPrint(x, fname);
  }

  HypreParVector::~HypreParVector()
  {
    if (own_ParVector) {
      HYPRE_IJVectorDestroy(x);
    }
  }

  double HypreParVector::InnerProduct(HypreParVector &x, HypreParVector &y)
  {
    return hypre_ParVectorInnerProd(x.x_par, y.x_par);
  }

}

#endif
