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

/// This HYPRE library interface has been taken originally from MFEM and modified
/// to suit the needs for the MOAB library.
/// Modified by: Vijay Mahadevan

#ifndef MOAB_HYPREPARVECTOR
#define MOAB_HYPREPARVECTOR

#include "moab/MOABConfig.h"
#include "moab/Core.hpp"

#ifdef MOAB_HAVE_EIGEN
#include <Eigen/Core>
#include <Eigen/Sparse>
#else
#error Configure with Eigen3 enabled
#endif

#ifdef MOAB_HAVE_MPI
#include "moab/ParallelComm.hpp"

// hypre header files
#include "HYPRE.h"
#include "HYPRE_IJ_mv.h"
#include "_hypre_IJ_mv.h"
#include "HYPRE_parcsr_ls.h"
// #include "seq_mv.h"
#include "temp_multivector.h"

#ifdef HYPRE_COMPLEX
#error "MOAB does not work with HYPRE's complex numbers support"
#endif

#include "hypre_parcsr.hpp"

namespace moab
{

  class HypreParMatrix;
  class HypreSolver;

/// Wrapper for hypre's parallel vector class
  class HypreParVector
  {
    private:
      int own_ParVector, rstart, rend, size, gsize;

      /// The actual object
      hypre_ParVector *x_par;
      HYPRE_IJVector x;

      friend class HypreParMatrix;
      friend class HypreSolver;

      moab::ParallelComm *pcomm;
      char initialized;

    public:
      /** Creates an empty vector with given global comm. */
      HypreParVector(moab::ParallelComm* p_comm);
      /** Creates vector with given global size and partitioning of the columns.
          Processor P owns columns [col[P],col[P+1]) */
      HypreParVector(moab::ParallelComm* p_comm, HYPRE_Int glob_size, HYPRE_Int rstart, HYPRE_Int rend);
      /// Creates vector compatible with y
      HypreParVector(const HypreParVector &y);
      /// Creates vector compatible with (i.e. in the domain of) A or A^T
      HypreParVector(HypreParMatrix &A, int tr = 0);


      int resize(HYPRE_Int glob_size, HYPRE_Int rstart, HYPRE_Int rend);

      /// MPI communicator
      moab::ParallelComm* GetParallelCommunicator() { return pcomm; }

      /// Returns the row partitioning
      inline HYPRE_Int *Partitioning() { return x->partitioning; }

      /// Returns the global number of rows
      inline HYPRE_Int GlobalSize() const { return x->global_num_rows; }

      /// Typecasting to hypre's HYPRE_IJVector*
      operator HYPRE_IJVector() const;
// #ifndef HYPRE_PAR_VECTOR_STRUCT
      /// Typecasting to hypre's HYPRE_ParVector, a.k.a. void *
      operator HYPRE_ParVector() const;
// #endif
      /// Changes the ownership of the the vector
      HYPRE_IJVector StealParVector() { own_ParVector = 0; return x; }

      /// Sets ownership of the internal HYPRE_IJVector
      void SetOwnership(int own) { own_ParVector = own; }

      /// Gets ownership of the internal HYPRE_IJVector
      int GetOwnership() const { return own_ParVector; }

      /// Define '=' for hypre vectors.
      HypreParVector &operator= (double d);
      HypreParVector &operator= (const HypreParVector &y);

      HYPRE_Int GetValues(const int ndata, const HYPRE_Int *indices, HYPRE_Complex *const _data) const;

      HYPRE_Int SetValues(const int ndata, const HYPRE_Int *indices, const HYPRE_Complex *const _data);

      HYPRE_Int AddValues(const int ndata, const HYPRE_Int *indices, const HYPRE_Complex *const _data);

      HYPRE_Int GetValue(const HYPRE_Int index, HYPRE_Complex *const _data) const;

      HYPRE_Int SetValue(const HYPRE_Int index, const HYPRE_Complex _data);

      HYPRE_Int AddValue(const HYPRE_Int index, const HYPRE_Complex _data);

      /** Sets the data of the HYPRE_IJVector to _data.
          Must be the same length as the current local size of the vector.
          If not, this can lead to an inconsistent vector setup. */
      HYPRE_Int SetData(double *p_data, HYPRE_Int *p_col=NULL);

      /** Add the data of the HYPRE_IJVector to _data.
          Must be the same length as the current local size of the vector.
          If not, this can lead to an inconsistent vector setup. */
      HYPRE_Int AddData(double *p_data, HYPRE_Int *p_col=NULL);

      HYPRE_Int verbosity(const HYPRE_Int level);

      HYPRE_Int FinalizeAssembly();

      // const HYPRE_Complex& operator()(int index) const;

      // HYPRE_Complex& operator()(int index);

      /// Prints the locally owned rows in parallel
      void Print(const char *fname) const;

      /// Calls hypre's destroy function
      ~HypreParVector();

      static double InnerProduct(HypreParVector &x, HypreParVector &y);
  };

/// Returns the inner product of x and y
  double InnerProduct(HypreParVector &x, HypreParVector &y);
  double InnerProduct(HypreParVector *x, HypreParVector *y);

}

#endif // MOAB_HAVE_MPI

#endif
