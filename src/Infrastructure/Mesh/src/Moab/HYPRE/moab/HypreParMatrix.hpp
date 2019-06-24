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

#ifndef MOAB_HYPREPARMATRIX
#define MOAB_HYPREPARMATRIX

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
// #include "HYPRE.h"
// #include "HYPRE_parcsr_ls.h"
// #include "HYPRE_MatvecFunctions.h"

#include "seq_mv.h"
#include "HypreParVector.hpp"
#include "_hypre_parcsr_mv.h"
#include "_hypre_parcsr_ls.h"
#include "interpreter.h"
#include "HYPRE_MatvecFunctions.h"

#include "temp_multivector.h"

#ifdef HYPRE_COMPLEX
#error "MOAB does not work with HYPRE's complex numbers support"
#endif

#include "hypre_parcsr.hpp"

namespace moab
{

  namespace internal
  {

// Convert a HYPRE_Int to int
    inline int to_int(HYPRE_Int i)
    {
#ifdef HYPRE_BIGINT
      MFEM_ASSERT(HYPRE_Int(int(i)) == i, "overflow converting HYPRE_Int to int");
#endif
      return int(i);
    }

  }


/// Wrapper for hypre's ParCSR matrix class
  class HypreParMatrix
  {
    private:
      /// The actual object
      HYPRE_IJMatrix A;
      hypre_ParCSRMatrix *A_parcsr;

      mv_InterfaceInterpreter *interpreter;

      // Does the object own the pointer A?
      char ParCSROwner;

      // Store the dimensions of the matrix
      int height, gnrows;
      int width, gncols;

      moab::ParallelComm *pcomm;
      char initialized;

      // Initialize with defaults. Does not initialize inherited members.
      void Init();

      // Delete all owned data. Does not perform re-initialization with defaults.
      void Destroy();

      friend class HypreSolver;

    public:
      typedef Eigen::VectorXd Vector;
      typedef Eigen::SparseMatrix<double, Eigen::RowMajor> MOABSparseMatrix;
      //typedef template Eigen::ArrayXX<T> Array2D<T>;

    public:
      /// An empty matrix to be used as a reference to an existing matrix
      HypreParMatrix(moab::ParallelComm *p_comm);

      /// Converts hypre's format to HypreParMatrix
      HypreParMatrix(HYPRE_IJMatrix a)
      {
        Init();
        A = a;
        height = GetNumRows();
        width = GetNumCols();
      }

      /** Creates block-diagonal square parallel matrix. Diagonal is given by diag
          which must be in CSR format (finalized). The new HypreParMatrix does not
          take ownership of any of the input arrays. */
      HypreParMatrix(moab::ParallelComm *p_comm, HYPRE_Int glob_size, HYPRE_Int *row_starts,
                     HYPRE_Int nnz_pr_diag = 0);

      /** Creates block-diagonal rectangular parallel matrix. Diagonal is given by
          diag which must be in CSR format (finalized). The new HypreParMatrix does
          not take ownership of any of the input arrays. */
      HypreParMatrix(moab::ParallelComm *p_comm,
                     HYPRE_Int global_num_rows,
                     HYPRE_Int global_num_cols,
                     HYPRE_Int *row_starts,
                     HYPRE_Int *col_starts,
                     HYPRE_Int nnz_pr_diag = 0,
                     HYPRE_Int onz_pr_diag = 0,
                     HYPRE_Int nnz_pr_offdiag = 0);

      /** Creates a general parallel matrix from a local CSR matrix on each
          processor described by the I, J and data arrays. The local matrix should
          be of size (local) nrows by (global) glob_ncols. The new parallel matrix
          contains copies of all input arrays (so they can be deleted). */
      // HypreParMatrix(MPI_Comm comm, int nrows, HYPRE_Int glob_nrows,
      //                HYPRE_Int glob_ncols, int *I, HYPRE_Int *J,
      //                double *data, HYPRE_Int *rows, HYPRE_Int *cols);

      /// Make this HypreParMatrix a reference to 'master'
      void MakeRef(const HypreParMatrix &master);

      /// MPI communicator
      moab::ParallelComm* GetParallelCommunicator() const { return pcomm; }

      /// Typecasting to hypre's HYPRE_IJMatrix*
      operator HYPRE_IJMatrix() { return A; }
      /// Typecasting to hypre's HYPRE_ParCSRMatrix, a.k.a. void *
      operator HYPRE_ParCSRMatrix() { return (HYPRE_ParCSRMatrix) A_parcsr; }

      /// Changes the ownership of the matrix
      HYPRE_IJMatrix StealData();
      /// Changes the ownership of the matrix to A
      void StealData(HypreParMatrix &A);

      /** If the HypreParMatrix does not own the row-starts array, make a copy of
          it that the HypreParMatrix will own. If the col-starts array is the same
          as the row-starts array, col-starts is also replaced. */
      // void CopyRowStarts();
      /** If the HypreParMatrix does not own the col-starts array, make a copy of
          it that the HypreParMatrix will own. If the row-starts array is the same
          as the col-starts array, row-starts is also replaced. */
      // void CopyColStarts();

      /// Returns the global number of nonzeros
      inline HYPRE_Int NNZ() { return A_parcsr->num_nonzeros; }
      /// Returns the row partitioning
      inline HYPRE_Int *RowPart() { return A->row_partitioning; }
      /// Returns the column partitioning
      inline HYPRE_Int *ColPart() { return A->col_partitioning; }
      /// Returns the global number of rows
      inline HYPRE_Int M() { return A->global_num_rows; }
      /// Returns the global number of columns
      inline HYPRE_Int N() { return A->global_num_cols; }
      /// Returns the global number of rows
      inline HYPRE_Int FirstRow() { return A->global_first_row; }
      /// Returns the global number of columns
      inline HYPRE_Int FirstCol() { return A->global_first_col; }

      /// Get the local diagonal of the matrix.
      void GetDiag(Vector &diag) const;
      /// Get the local diagonal block. NOTE: 'diag' will not own any data.
      void GetDiag(MOABSparseMatrix &diag) const;
      /// Get the local off-diagonal block. NOTE: 'offd' will not own any data.
      void GetOffd(MOABSparseMatrix &offd, HYPRE_Int *&cmap) const;

      /** Split the matrix into M x N equally sized blocks of parallel matrices.
          The size of 'blocks' must already be set to M x N. */
      // void GetBlocks(Eigen::Array<HypreParMatrix*,Eigen::Dynamic,Eigen::Dynamic> &blocks,
      //                bool interleaved_rows = false,
      //                bool interleaved_cols = false) const;

      /// Returns the transpose of *this
      // HypreParMatrix * Transpose();

      /** Destroy and resize block-diagonal square parallel matrix. Diagonal is given by diag
          which must be in CSR format (finalized). The new HypreParMatrix does not
          take ownership of any of the input arrays. */
      void resize(HYPRE_Int glob_size,
                  HYPRE_Int *row_starts,
                  HYPRE_Int nnz_pr_diag = 0,
                  HYPRE_Int nnz_pr_offdiag = 0);

      /** Destroy and resize block-diagonal rectangular parallel matrix. Diagonal is given by
          diag which must be in CSR format (finalized). The new HypreParMatrix does
          not take ownership of any of the input arrays. */
      void resize(HYPRE_Int global_num_rows,
                  HYPRE_Int global_num_cols,
                  HYPRE_Int *row_starts,
                  HYPRE_Int *col_starts,
                  HYPRE_Int *nnz_pr_diag = NULL,
                  HYPRE_Int *onz_pr_diag = NULL,
                  HYPRE_Int nnz_pr_offdiag = 0);

      /// Returns the number of rows in the diagonal block of the ParCSRMatrix
      int GetNumRows() const
      {
        return internal::to_int(
                 hypre_CSRMatrixNumRows(hypre_ParCSRMatrixDiag(A_parcsr)));
      }

      /// Returns the number of columns in the diagonal block of the ParCSRMatrix
      int GetNumCols() const
      {
        return internal::to_int(
                 hypre_CSRMatrixNumCols(hypre_ParCSRMatrixDiag(A_parcsr)));
      }

      /// Computes y = alpha * A * x + beta * y
      HYPRE_Int Mult(HypreParVector &x, HypreParVector &y,
                     double alpha = 1.0, double beta = 0.0);
      /// Computes y = alpha * A * x + beta * y
      HYPRE_Int Mult(HYPRE_ParVector x, HYPRE_ParVector y,
                     double alpha = 1.0, double beta = 0.0);
      /// Computes y = alpha * A^t * x + beta * y
      HYPRE_Int MultTranspose(HypreParVector &x, HypreParVector &y,
                              double alpha = 1.0, double beta = 0.0);

      void Mult(double a, const HypreParVector &x, double b, HypreParVector &y) const;
      void MultTranspose(double a, const HypreParVector &x, double b, HypreParVector &y) const;

      // virtual void Mult(const Vector &x, Vector &y) const
      // { Mult(1.0, x, 0.0, y); }
      // virtual void MultTranspose(const Vector &x, Vector &y) const
      // { MultTranspose(1.0, x, 0.0, y); }

      /** The "Boolean" analog of y = alpha * A * x + beta * y, where elements in
          the sparsity pattern of the matrix are treated as "true". */
      // void BooleanMult(int alpha, int *x, int beta, int *y)
      // {
      //    internal::hypre_ParCSRMatrixBooleanMatvec(A_parcsr, alpha, x, beta, y);
      // }

      /** Multiply A on the left by a block-diagonal parallel matrix D. Return
          a new parallel matrix, D*A. If D has a different number of rows than A,
          D's row starts array needs to be given (as returned by the methods
          GetDofOffsets/GetTrueDofOffsets of ParFiniteElementSpace). The new matrix
          D*A uses copies of the row-, column-starts arrays, so "this" matrix and
          "row_starts" can be deleted.
          NOTE: this operation is local and does not require communication. */
      // HypreParMatrix* LeftDiagMult(const MOABSparseMatrix &D,
      //                              HYPRE_Int* row_starts = NULL) const;

      /// Scale the local row i by s(i).
      void ScaleRows(const Vector &s);
      /// Scale the local row i by 1./s(i)
      void InvScaleRows(const Vector &s);
      /// Scale all entries by s: A_scaled = s*A.
      void operator*=(double s);

      /// Remove values smaller in absolute value than some threshold
      void Threshold(double threshold = 0.0);

      /// If a row contains only zeros, set its diagonal to 1.
      void EliminateZeroRows() { hypre_ParCSRMatrixFixZeroRows(A_parcsr); }

      /** Eliminate rows and columns from the matrix, and rows from the vector B.
          Modify B with the BC values in X. */
      void EliminateRowsCols(const std::vector<HYPRE_Int> &rows_cols, const HypreParVector &X,
                             HypreParVector &B);

      /** Eliminate rows and columns from the matrix and store the eliminated
          elements in a new matrix Ae (returned), so that the modified matrix and
          Ae sum to the original matrix. */
      HypreParMatrix *EliminateRowsCols(const std::vector<HYPRE_Int> &rows_cols);

      HYPRE_Int GetValues(const HYPRE_Int nrows, HYPRE_Int *ncols, HYPRE_Int *rows, HYPRE_Int *cols,
                          HYPRE_Complex *values);
      HYPRE_Int SetValues(const HYPRE_Int nrows, HYPRE_Int *ncols, const HYPRE_Int *rows,
                          const HYPRE_Int *cols, const HYPRE_Complex *values);
      HYPRE_Int AddToValues(const HYPRE_Int nrows, HYPRE_Int *ncols, const HYPRE_Int *rows,
                            const HYPRE_Int *cols, const HYPRE_Complex *values);

      HYPRE_Int FinalizeAssembly();

      HYPRE_Int verbosity(const HYPRE_Int level);

      /// Prints the locally owned rows in parallel
      void Print(const char *fname, HYPRE_Int offi = 0, HYPRE_Int offj = 0);
      /// Reads the matrix from a file
      void Read(const char *fname);

      /// Calls hypre's destroy function
      virtual ~HypreParMatrix() { Destroy(); }

      /// Returns the matrix A * B
      friend HypreParMatrix *ParMult(HypreParMatrix *A, HypreParMatrix *B);

      /// Returns the matrix P^t * A * P
      friend HypreParMatrix *RAP(HypreParMatrix *A, HypreParMatrix *P);
      /// Returns the matrix Rt^t * A * P
      friend HypreParMatrix *RAP(HypreParMatrix *Rt, HypreParMatrix *A, HypreParMatrix *P);

      /** Eliminate essential BC specified by 'ess_dof_list' from the solution X to
          the r.h.s. B. Here A is a matrix with eliminated BC, while Ae is such that
          (A+Ae) is the original (Neumann) matrix before elimination. */
      friend void EliminateBC(HypreParMatrix &A, HypreParMatrix &Ae,
                              const std::vector<int> &ess_dof_list,
                              const HypreParVector &X, HypreParVector &B);

      friend class HypreSolver;

  };

}

#endif // MOAB_HAVE_MPI

#endif
