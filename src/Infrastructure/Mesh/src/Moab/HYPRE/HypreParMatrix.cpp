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

#include "hypre_parcsr.hpp"

#include <fstream>
#include <iostream>
#include <cmath>
#include <cstdlib>
#include <cassert>

using namespace std;

#define MOAB_DEBUG

namespace moab
{
#define moab_hypre_assert(a,b) {}
#define moab_hypre_assert_t(a,b) { \
    if (!a) { \
      std::cout << "HYPRE Error: " << b << std::endl; \
      exit (-1); \
    } \
  }


  template<typename TargetT, typename SourceT>
  static TargetT *DuplicateAs(const SourceT *array, int size,
                              bool cplusplus = true)
  {
    TargetT *target_array = cplusplus ? new TargetT[size]
                            /*     */ : hypre_TAlloc(TargetT, size);

    for (int i = 0; i < size; i++) {
      target_array[i] = array[i];
    }

    return target_array;
  }

  void HypreParMatrix::Init()
  {
    A = NULL;
    A_parcsr = NULL;
    ParCSROwner = 1;
  }

  HypreParMatrix::HypreParMatrix(moab::ParallelComm* p_comm) : pcomm(p_comm)
  {
    Init();
    height = width = 0;
  }

// Square block-diagonal constructor
  HypreParMatrix::HypreParMatrix(moab::ParallelComm* p_comm, HYPRE_Int glob_size,
                                 HYPRE_Int *row_starts, HYPRE_Int nnz_pr) : pcomm(p_comm)
  {
    Init();
    resize(glob_size, row_starts, nnz_pr);
  }


  void HypreParMatrix::resize(HYPRE_Int glob_size,
                              HYPRE_Int *row_starts,
                              HYPRE_Int nnz_pr_diag,
                              HYPRE_Int nnz_pr_offdiag)
  {
    /* Create the matrix.
        Note that this is a square matrix, so we indicate the row partition
        size twice (since number of rows = number of cols) */
    HYPRE_IJMatrixCreate(pcomm->comm(), row_starts[0], row_starts[1], row_starts[0], row_starts[1], &A);
    /* Choose a parallel csr format storage (see the User's Manual) */
    HYPRE_IJMatrixSetObjectType(A, HYPRE_PARCSR);

    int locsize = row_starts[1]-row_starts[0]+1;
    int num_procs, rank;
    MPI_Comm_size(pcomm->comm(), &num_procs);
    MPI_Comm_rank(pcomm->comm(), &rank);

    if (nnz_pr_diag != 0 || nnz_pr_offdiag != 0) {
      if (num_procs > 1) {
        std::vector<HYPRE_Int> m_nnz_pr_diag(locsize, nnz_pr_diag);
        std::vector<HYPRE_Int> m_onz_pr_diag(locsize, nnz_pr_offdiag);
        HYPRE_IJMatrixSetDiagOffdSizes(A, &m_nnz_pr_diag[0], &m_onz_pr_diag[0]);
      }
      else {
        std::vector<HYPRE_Int> m_nnz_pr_diag(locsize, nnz_pr_diag);
        HYPRE_IJMatrixSetRowSizes(A, &m_nnz_pr_diag[0]);
      }
    }

    /* Initialize before setting coefficients */
    HYPRE_IJMatrixInitialize(A);
    /* Get the parcsr matrix object to use */
    HYPRE_IJMatrixGetObject(A, (void **) &A_parcsr);

    /* define an interpreter for the ParCSR interface */
    HYPRE_MatvecFunctions matvec_fn;
    interpreter = hypre_CTAlloc(mv_InterfaceInterpreter, 1);
    HYPRE_ParCSRSetupInterpreter(interpreter);
    HYPRE_ParCSRSetupMatvec(&matvec_fn);
    gnrows = glob_size;
    gncols = glob_size;
    height = GetNumRows();
    width = GetNumCols();
  }

  void HypreParMatrix::resize(HYPRE_Int global_num_rows,
                              HYPRE_Int global_num_cols,
                              HYPRE_Int *row_starts,
                              HYPRE_Int *col_starts,
                              HYPRE_Int *nnz_pr_diag,
                              HYPRE_Int *onz_pr_diag,
                              HYPRE_Int nnz_pr_offdiag)
  {
    /* Create the matrix.
        Note that this is a square matrix, so we indicate the row partition
        size twice (since number of rows = number of cols) */
    HYPRE_IJMatrixCreate(pcomm->comm(), row_starts[0], row_starts[1], col_starts[0], col_starts[1], &A);
    /* Choose a parallel csr format storage (see the User's Manual) */
    HYPRE_IJMatrixSetObjectType(A, HYPRE_PARCSR);

    int num_procs;
    MPI_Comm_size(pcomm->comm(), &num_procs);

    /* Set the matrix pre-allocation data */
    if (num_procs > 1) {
      if (nnz_pr_diag == NULL && onz_pr_diag == NULL) {
        std::cout << "Parameter nnz_pr_diag and onz_pr_diag cannot be NULL.\n";
        moab_hypre_assert_t(nnz_pr_diag, true);
      }

      HYPRE_IJMatrixSetDiagOffdSizes(A, nnz_pr_diag, onz_pr_diag);
    }
    else {
      HYPRE_IJMatrixSetRowSizes(A, nnz_pr_diag);
    }

    if (nnz_pr_offdiag) {
      HYPRE_IJMatrixSetMaxOffProcElmts(A, nnz_pr_offdiag);
    }

    /* Initialize before setting coefficients */
    HYPRE_IJMatrixInitialize(A);
    /* Get the parcsr matrix object to use */
    HYPRE_IJMatrixGetObject(A, (void **) &A_parcsr);

    /* define an interpreter for the ParCSR interface */
    HYPRE_MatvecFunctions matvec_fn;
    interpreter = hypre_CTAlloc(mv_InterfaceInterpreter, 1);
    HYPRE_ParCSRSetupInterpreter(interpreter);
    HYPRE_ParCSRSetupMatvec(&matvec_fn);
    gnrows = global_num_rows;
    gncols = global_num_cols;
    height = GetNumRows();
    width = GetNumCols();
  }


// Rectangular block-diagonal constructor
  HypreParMatrix::HypreParMatrix(moab::ParallelComm *p_comm,
                                 HYPRE_Int global_num_rows,
                                 HYPRE_Int global_num_cols,
                                 HYPRE_Int *row_starts,
                                 HYPRE_Int *col_starts,
                                 HYPRE_Int nnz_pr_diag,
                                 HYPRE_Int onz_pr_diag,
                                 HYPRE_Int nnz_pr_offdiag) : pcomm(p_comm)
  {
    Init();
    std::vector<HYPRE_Int> m_nnz_pr_diag(row_starts[1]-row_starts[0], nnz_pr_diag);
    std::vector<HYPRE_Int> m_onz_pr_diag(row_starts[1]-row_starts[0], onz_pr_diag);
    resize(global_num_rows, global_num_cols, row_starts, col_starts, &m_nnz_pr_diag[0], &m_onz_pr_diag[0], nnz_pr_offdiag);
  }

  void HypreParMatrix::MakeRef(const HypreParMatrix &master)
  {
    Destroy();
    Init();
    A = master.A;
    A_parcsr = master.A_parcsr;
    ParCSROwner = 0;
    height = master.GetNumRows();
    width = master.GetNumCols();
  }

  HYPRE_IJMatrix HypreParMatrix::StealData()
  {
    // Only safe when (diagOwner == -1 && offdOwner == -1 && colMapOwner == -1)
    // Otherwise, there may be memory leaks or hypre may destroy arrays allocated
    // with operator new.
    moab_hypre_assert(diagOwner == -1 && offdOwner == -1 && colMapOwner == -1, "");
    moab_hypre_assert(ParCSROwner, "");
    HYPRE_IJMatrix R = A;
    A = NULL;
    ParCSROwner = 0;
    Destroy();
    Init();
    return R;
  }

  void HypreParMatrix::StealData(HypreParMatrix &X)
  {
    // Only safe when (diagOwner == -1 && offdOwner == -1 && colMapOwner == -1)
    // Otherwise, there may be memory leaks or hypre may destroy arrays allocated
    // with operator new.
    moab_hypre_assert(diagOwner == -1 && offdOwner == -1 && colMapOwner == -1, "");
    moab_hypre_assert(ParCSROwner, "");
    moab_hypre_assert(X.diagOwner == -1 && X.offdOwner == -1 && X.colMapOwner == -1, "");
    moab_hypre_assert(X.ParCSROwner, "");
    A = X.A;
    A_parcsr = X.A_parcsr;
    ParCSROwner = 1;
    X.A = NULL;
    X.A_parcsr = NULL;
    X.ParCSROwner = 0;
    X.Destroy();
    X.Init();
  }

  void HypreParMatrix::GetDiag(Eigen::VectorXd &diag) const
  {
    int size = this->height;
    diag.resize(size);

    for (int j = 0; j < size; j++) {
      diag(j) = A_parcsr->diag->data[A_parcsr->diag->i[j]];
      moab_hypre_assert(A_parcsr->diag->j[A_parcsr->diag->i[j]] == j,
                        "the first entry in each row must be the diagonal one");
    }
  }

  static void MakeWrapper(const hypre_CSRMatrix *mat,
                          Eigen::SparseMatrix<double, Eigen::RowMajor> &wrapper)
  {
    HYPRE_Int nr = hypre_CSRMatrixNumRows(mat);
    HYPRE_Int nc = hypre_CSRMatrixNumCols(mat);
    Eigen::SparseMatrix<double, Eigen::RowMajor> tmp(nr, nc);
    typedef Eigen::Triplet<double> T;
    HYPRE_Int nnz = hypre_CSRMatrixNumNonzeros(mat);
    int *rindx, *cindx;
    double *dindx;
#ifndef HYPRE_BIGINT
    rindx = hypre_CSRMatrixI(mat);
    cindx = hypre_CSRMatrixJ(mat);
    dindx = hypre_CSRMatrixData(mat);
#else
    rindx = DuplicateAs<int>(hypre_CSRMatrixI(mat), nr + 1);
    cindx = DuplicateAs<int>(hypre_CSRMatrixJ(mat), nnz);
    dindx = hypre_CSRMatrixData(mat);
#endif
    std::vector<T> tripletList(nnz);

    for (int i = 0; i < nnz; ++i) {
      tripletList.push_back(T(rindx[i], cindx[i], dindx[i]));
    }

    tmp.setFromTriplets(tripletList.begin(), tripletList.end());
    wrapper.swap(tmp);
  }

  void HypreParMatrix::GetDiag(Eigen::SparseMatrix<double, Eigen::RowMajor> &diag) const
  {
    MakeWrapper(A_parcsr->diag, diag);
  }

  void HypreParMatrix::GetOffd(Eigen::SparseMatrix<double, Eigen::RowMajor> &offd,
                               HYPRE_Int *&cmap) const
  {
    MakeWrapper(A_parcsr->offd, offd);
    cmap = A_parcsr->col_map_offd;
  }

// void HypreParMatrix::GetBlocks(Eigen::Array<HypreParMatrix*,Eigen::Dynamic,Eigen::Dynamic> &blocks,
//                                bool interleaved_rows,
//                                bool interleaved_cols) const
// {
//    int nr = blocks.rows();
//    int nc = blocks.cols();

//    hypre_ParCSRMatrix **hypre_blocks = new hypre_ParCSRMatrix*[nr * nc];
//    internal::hypre_ParCSRMatrixSplit(A, nr, nc, hypre_blocks,
//                                      interleaved_rows, interleaved_cols);

//    for (int i = 0; i < nr; i++)
//    {
//       for (int j = 0; j < nc; j++)
//       {
//          blocks[i][j] = new HypreParMatrix(hypre_blocks[i*nc + j]);
//       }
//    }

//    delete [] hypre_blocks;
// }

// HypreParMatrix * HypreParMatrix::Transpose()
// {
//    hypre_ParCSRMatrix * At;
//    hypre_ParCSRMatrixTranspose(A_parcsr, &At, 1);
//    hypre_ParCSRMatrixSetNumNonzeros(At);

//    hypre_MatvecCommPkgCreate(At);

//    return new HypreParMatrix(At);
// }

  HYPRE_Int HypreParMatrix::Mult(HypreParVector &x, HypreParVector &y,
                                 double a, double b)
  {
    return hypre_ParCSRMatrixMatvec(a, A_parcsr, x.x_par, b, y.x_par);
  }

  void HypreParMatrix::Mult(double a, const HypreParVector &x, double b, HypreParVector &y) const
  {
    hypre_ParCSRMatrixMatvec(a, A_parcsr, x.x_par, b, y.x_par);
  }

  void HypreParMatrix::MultTranspose(double a, const HypreParVector &x,
                                     double b, HypreParVector &y) const
  {
    hypre_ParCSRMatrixMatvecT(a, A_parcsr, y.x_par, b, x.x_par);
  }

  HYPRE_Int HypreParMatrix::Mult(HYPRE_ParVector x, HYPRE_ParVector y,
                                 double a, double b)
  {
    return hypre_ParCSRMatrixMatvec(a, A_parcsr, (hypre_ParVector *) x, b,
                                    (hypre_ParVector *) y);
  }

  HYPRE_Int HypreParMatrix::MultTranspose(HypreParVector &x, HypreParVector &y,
                                          double a, double b)
  {
    return hypre_ParCSRMatrixMatvecT(a, A_parcsr, x.x_par, b, y.x_par);
  }

// HypreParMatrix* HypreParMatrix::LeftDiagMult(const Eigen::SparseMatrix<double, Eigen::RowMajor> &D,
//                                              HYPRE_Int* row_starts) const
// {
//    const bool assumed_partition = HYPRE_AssumedPartitionCheck();
//    const bool same_rows = (D.rows() == hypre_CSRMatrixNumRows(A_parcsr->diag));

//    int part_size;
//    if (assumed_partition)
//    {
//       part_size = 2;
//    }
//    else
//    {
//       MPI_Comm_size(GetComm(), &part_size);
//       part_size++;
//    }

//    HYPRE_Int global_num_rows;
//    if (same_rows)
//    {
//       row_starts = hypre_ParCSRMatrixRowStarts(A_parcsr);
//       global_num_rows = hypre_ParCSRMatrixGlobalNumRows(A_parcsr);
//    }
//    else
//    {
//       // MFEM_VERIFY(row_starts != NULL, "the number of rows in D and A is not "
//       //             "the same; row_starts must be given (not NULL)");
//       // Here, when assumed_partition is true we use row_starts[2], so
//       // row_starts must come from the GetDofOffsets/GetTrueDofOffsets methods
//       // of ParFiniteElementSpace (HYPRE's partitions have only 2 entries).
//       global_num_rows =
//          assumed_partition ? row_starts[2] : row_starts[part_size-1];
//    }

//    HYPRE_Int *col_starts = hypre_ParCSRMatrixColStarts(A_parcsr);
//    HYPRE_Int *col_map_offd;

//    // get the diag and offd blocks as SparseMatrix wrappers
//    Eigen::SparseMatrix<double, Eigen::RowMajor> A_diag, A_offd;
//    GetDiag(A_diag);
//    GetOffd(A_offd, col_map_offd);

//    // multiply the blocks with D and create a new HypreParMatrix
//    Eigen::SparseMatrix<double, Eigen::RowMajor> DA_diag = (D * A_diag);
//    Eigen::SparseMatrix<double, Eigen::RowMajor> DA_offd = (D * A_offd);

//    HypreParMatrix* DA =
//       new HypreParMatrix(GetComm(),
//                          global_num_rows, hypre_ParCSRMatrixGlobalNumCols(A),
//                          DuplicateAs<HYPRE_Int>(row_starts, part_size, false),
//                          DuplicateAs<HYPRE_Int>(col_starts, part_size, false),
//                          &DA_diag, &DA_offd,
//                          DuplicateAs<HYPRE_Int>(col_map_offd, A_offd.cols()));

//    // When HYPRE_BIGINT is defined, we want DA_{diag,offd} to delete their I and
//    // J arrays but not their data arrays; when HYPRE_BIGINT is not defined, we
//    // don't want DA_{diag,offd} to delete anything.
// // #ifndef HYPRE_BIGINT
// //    DA_diag.LoseData();
// //    DA_offd.LoseData();
// // #else
// //    DA_diag.SetDataOwner(false);
// //    DA_offd.SetDataOwner(false);
// // #endif

//    // delete DA_diag;
//    // delete DA_offd;

//    hypre_ParCSRMatrixSetRowStartsOwner(DA->A, 1);
//    hypre_ParCSRMatrixSetColStartsOwner(DA->A, 1);

//    DA->diagOwner = DA->offdOwner = 3;
//    DA->colMapOwner = 1;

//    return DA;
// }

  void HypreParMatrix::ScaleRows(const Eigen::VectorXd &diag)
  {
    if (hypre_CSRMatrixNumRows(A_parcsr->diag) != hypre_CSRMatrixNumRows(A_parcsr->offd)) {
      MB_SET_ERR_RET("Row does not match");
    }

    if (hypre_CSRMatrixNumRows(A_parcsr->diag) != diag.size()) {
      MB_SET_ERR_RET("Note the Eigen::VectorXd diag is not of compatible dimensions with A\n");
    }

    int size = this->height;
    double     *Adiag_data   = hypre_CSRMatrixData(A_parcsr->diag);
    HYPRE_Int  *Adiag_i      = hypre_CSRMatrixI(A_parcsr->diag);
    double     *Aoffd_data   = hypre_CSRMatrixData(A_parcsr->offd);
    HYPRE_Int  *Aoffd_i      = hypre_CSRMatrixI(A_parcsr->offd);
    double val;
    HYPRE_Int jj;

    for (int i(0); i < size; ++i) {
      val = diag[i];

      for (jj = Adiag_i[i]; jj < Adiag_i[i + 1]; ++jj) {
        Adiag_data[jj] *= val;
      }

      for (jj = Aoffd_i[i]; jj < Aoffd_i[i + 1]; ++jj) {
        Aoffd_data[jj] *= val;
      }
    }
  }

  void HypreParMatrix::InvScaleRows(const Eigen::VectorXd &diag)
  {
    if (hypre_CSRMatrixNumRows(A_parcsr->diag) != hypre_CSRMatrixNumRows(A_parcsr->offd)) {
      MB_SET_ERR_RET("Row does not match");
    }

    if (hypre_CSRMatrixNumRows(A_parcsr->diag) != diag.size()) {
      MB_SET_ERR_RET("Note the Eigen::VectorXd diag is not of compatible dimensions with A_parcsr\n");
    }

    int size = this->height;
    double     *Adiag_data   = hypre_CSRMatrixData(A_parcsr->diag);
    HYPRE_Int  *Adiag_i      = hypre_CSRMatrixI(A_parcsr->diag);
    double     *Aoffd_data   = hypre_CSRMatrixData(A_parcsr->offd);
    HYPRE_Int  *Aoffd_i      = hypre_CSRMatrixI(A_parcsr->offd);
    double val;
    HYPRE_Int jj;

    for (int i(0); i < size; ++i) {
#ifdef MOAB_DEBUG

      if (0.0 == diag(i)) {
        MB_SET_ERR_RET("HypreParMatrix::InvDiagScale : Division by 0");
      }

#endif
      val = 1. / diag(i);

      for (jj = Adiag_i[i]; jj < Adiag_i[i + 1]; ++jj) {
        Adiag_data[jj] *= val;
      }

      for (jj = Aoffd_i[i]; jj < Aoffd_i[i + 1]; ++jj) {
        Aoffd_data[jj] *= val;
      }
    }
  }

  void HypreParMatrix::operator*=(double s)
  {
    if (hypre_CSRMatrixNumRows(A_parcsr->diag) != hypre_CSRMatrixNumRows(A_parcsr->offd)) {
      MB_SET_ERR_RET("Row does not match");
    }

    HYPRE_Int size = hypre_CSRMatrixNumRows(A_parcsr->diag);
    HYPRE_Int jj;
    double     *Adiag_data   = hypre_CSRMatrixData(A_parcsr->diag);
    HYPRE_Int  *Adiag_i      = hypre_CSRMatrixI(A_parcsr->diag);

    for (jj = 0; jj < Adiag_i[size]; ++jj) {
      Adiag_data[jj] *= s;
    }

    double     *Aoffd_data   = hypre_CSRMatrixData(A_parcsr->offd);
    HYPRE_Int  *Aoffd_i      = hypre_CSRMatrixI(A_parcsr->offd);

    for (jj = 0; jj < Aoffd_i[size]; ++jj) {
      Aoffd_data[jj] *= s;
    }
  }


  HYPRE_Int HypreParMatrix::GetValues(const HYPRE_Int nrows, HYPRE_Int *ncols, HYPRE_Int *rows,
                                      HYPRE_Int *cols, HYPRE_Complex *values)
  {
    return HYPRE_IJMatrixGetValues(A, nrows, ncols, rows, cols, values);
  }

  HYPRE_Int HypreParMatrix::SetValues(const HYPRE_Int nrows, HYPRE_Int *ncols, const HYPRE_Int *rows,
                                      const HYPRE_Int *cols, const HYPRE_Complex *values)
  {
    return HYPRE_IJMatrixSetValues(A, nrows, ncols, rows, cols, values);
  }

  HYPRE_Int HypreParMatrix::AddToValues(const HYPRE_Int nrows, HYPRE_Int *ncols,
                                        const HYPRE_Int *rows, const HYPRE_Int *cols, const HYPRE_Complex *values)
  {
    return HYPRE_IJMatrixAddToValues(A, nrows, ncols, rows, cols, values);
  }

  HYPRE_Int HypreParMatrix::verbosity(const HYPRE_Int level)
  {
    return HYPRE_IJMatrixSetPrintLevel(A, level);
  }

  HYPRE_Int HypreParMatrix::FinalizeAssembly()
  {
    return HYPRE_IJMatrixAssemble(A);
  }


  static void get_sorted_rows_cols(const std::vector<HYPRE_Int> &rows_cols,
                                   std::vector<HYPRE_Int> &hypre_sorted)
  {
    hypre_sorted.resize(rows_cols.size());
    std::copy(rows_cols.begin(), rows_cols.end(), hypre_sorted.begin());
    std::sort(hypre_sorted.begin(), hypre_sorted.end());
  }

  void HypreParMatrix::Threshold(double threshold)
  {
    int  ierr = 0;
    int num_procs;
    hypre_CSRMatrix *csr_A;
    hypre_CSRMatrix *csr_A_wo_z;
    hypre_ParCSRMatrix *parcsr_A_ptr;
    int *row_starts = NULL;
    int *col_starts = NULL;
    int row_start = -1;
    int row_end = -1;
    int col_start = -1;
    int col_end = -1;
    MPI_Comm_size(pcomm->comm(), &num_procs);
    ierr += hypre_ParCSRMatrixGetLocalRange(A_parcsr,
                                            &row_start, &row_end,
                                            &col_start, &col_end);
    row_starts = hypre_ParCSRMatrixRowStarts(A_parcsr);
    col_starts = hypre_ParCSRMatrixColStarts(A_parcsr);
    parcsr_A_ptr = hypre_ParCSRMatrixCreate(pcomm->comm(), row_starts[num_procs],
                                            col_starts[num_procs], row_starts,
                                            col_starts, 0, 0, 0);
    csr_A = hypre_MergeDiagAndOffd(A_parcsr);
    csr_A_wo_z =  hypre_CSRMatrixDeleteZeros(csr_A, threshold);

    /* hypre_CSRMatrixDeleteZeros will return a NULL pointer rather than a usable
       CSR matrix if it finds no non-zeros */
    if (csr_A_wo_z == NULL) {
      csr_A_wo_z = csr_A;

    } else {
      ierr += hypre_CSRMatrixDestroy(csr_A);
    }

    ierr += GenerateDiagAndOffd(csr_A_wo_z, parcsr_A_ptr,
                                col_start, col_end);
    ierr += hypre_CSRMatrixDestroy(csr_A_wo_z);
    ierr += hypre_ParCSRMatrixDestroy(A_parcsr);
    hypre_IJMatrixObject(A) = parcsr_A_ptr;
    /* Get the parcsr matrix object to use */
    HYPRE_IJMatrixGetObject(A, (void **) A_parcsr);
  }

  void HypreParMatrix::EliminateRowsCols(const std::vector<HYPRE_Int> &rows_cols,
                                         const HypreParVector &X,
                                         HypreParVector &B)
  {
    std::vector<HYPRE_Int> rc_sorted;
    get_sorted_rows_cols(rows_cols, rc_sorted);
    internal::hypre_ParCSRMatrixEliminateAXB(
      A_parcsr, rc_sorted.size(), rc_sorted.data(), X.x_par, B.x_par);
  }

  HypreParMatrix *HypreParMatrix::EliminateRowsCols(const std::vector<HYPRE_Int> &rows_cols)
  {
    std::vector<HYPRE_Int> rc_sorted;
    get_sorted_rows_cols(rows_cols, rc_sorted);
    hypre_ParCSRMatrix *Ae;
    internal::hypre_ParCSRMatrixEliminateAAe(
      A_parcsr, &Ae, rc_sorted.size(), rc_sorted.data());
    HypreParMatrix *tmpMat = new HypreParMatrix(pcomm, M(), N(), RowPart(), ColPart(), 0, 0);
    hypre_IJMatrixObject(tmpMat->A) = Ae;
    /* Get the parcsr matrix object to use */
    HYPRE_IJMatrixGetObject(tmpMat->A, (void **) tmpMat->A_parcsr);
    return tmpMat;
  }

  void HypreParMatrix::Print(const char *fname, HYPRE_Int offi, HYPRE_Int offj)
  {
    hypre_ParCSRMatrixPrintIJ(A_parcsr, offi, offj, fname);
  }

  void HypreParMatrix::Read(const char *fname)
  {
    Destroy();
    Init();
    HYPRE_Int base_i, base_j;
    hypre_ParCSRMatrixReadIJ(pcomm->comm(), fname, &base_i, &base_j, &A_parcsr);
    hypre_ParCSRMatrixSetNumNonzeros(A_parcsr);
    hypre_MatvecCommPkgCreate(A_parcsr);
    height = GetNumRows();
    width = GetNumCols();
  }

  void HypreParMatrix::Destroy()
  {
    if (interpreter) {
      hypre_TFree(interpreter);
    }

    if (A == NULL) { return; }

    else {
      HYPRE_IJMatrixDestroy(A);
    }
  }

  HypreParMatrix *ParMult(HypreParMatrix *A, HypreParMatrix *B)
  {
    hypre_ParCSRMatrix *ab;
    ab = hypre_ParMatmul(A->A_parcsr, B->A_parcsr);
    hypre_MatvecCommPkgCreate(ab);
    HypreParMatrix *tmpMat = new HypreParMatrix(A->pcomm, A->M(), A->N(), A->RowPart(), A->ColPart(), 0,
        0);
    hypre_IJMatrixObject(tmpMat->A) = ab;
    /* Get the parcsr matrix object to use */
    HYPRE_IJMatrixGetObject(tmpMat->A, (void **) tmpMat->A_parcsr);
    return tmpMat;
  }

  HypreParMatrix *RAP(HypreParMatrix *A, HypreParMatrix *P)
  {
    HYPRE_Int P_owns_its_col_starts =
      hypre_ParCSRMatrixOwnsColStarts((hypre_ParCSRMatrix *)(P->A_parcsr));
    hypre_ParCSRMatrix *rap;
    hypre_BoomerAMGBuildCoarseOperator(P->A_parcsr, A->A_parcsr, P->A_parcsr, &rap);
    hypre_ParCSRMatrixSetNumNonzeros(rap);
    // hypre_MatvecCommPkgCreate(rap);
    /* Warning: hypre_BoomerAMGBuildCoarseOperator steals the col_starts
       from P (even if it does not own them)! */
    hypre_ParCSRMatrixSetRowStartsOwner(rap, 0);
    hypre_ParCSRMatrixSetColStartsOwner(rap, 0);

    if (P_owns_its_col_starts) {
      hypre_ParCSRMatrixSetColStartsOwner(P->A_parcsr, 1);
    }

    HypreParMatrix *tmpMat = new HypreParMatrix(A->pcomm, A->M(), A->N(), A->RowPart(), A->ColPart(), 0,
        0);
    hypre_IJMatrixObject(tmpMat->A) = rap;
    /* Get the parcsr matrix object to use */
    HYPRE_IJMatrixGetObject(tmpMat->A, (void **) tmpMat->A_parcsr);
    return tmpMat;
  }

  HypreParMatrix *RAP(HypreParMatrix *Rt, HypreParMatrix *A, HypreParMatrix *P)
  {
    HYPRE_Int P_owns_its_col_starts =
      hypre_ParCSRMatrixOwnsColStarts((hypre_ParCSRMatrix *)(P->A_parcsr));
    HYPRE_Int Rt_owns_its_col_starts =
      hypre_ParCSRMatrixOwnsColStarts((hypre_ParCSRMatrix *)(Rt->A_parcsr));
    hypre_ParCSRMatrix *rap;
    hypre_BoomerAMGBuildCoarseOperator(Rt->A_parcsr, A->A_parcsr, P->A_parcsr, &rap);
    hypre_ParCSRMatrixSetNumNonzeros(rap);

    // hypre_MatvecCommPkgCreate(rap);
    if (!P_owns_its_col_starts) {
      /* Warning: hypre_BoomerAMGBuildCoarseOperator steals the col_starts
         from P (even if it does not own them)! */
      hypre_ParCSRMatrixSetColStartsOwner(rap, 0);
    }

    if (!Rt_owns_its_col_starts) {
      /* Warning: hypre_BoomerAMGBuildCoarseOperator steals the col_starts
         from P (even if it does not own them)! */
      hypre_ParCSRMatrixSetRowStartsOwner(rap, 0);
    }

    HypreParMatrix *tmpMat = new HypreParMatrix(A->pcomm, A->M(), A->N(), A->RowPart(), A->ColPart(), 0,
        0);
    hypre_IJMatrixObject(tmpMat->A) = rap;
    /* Get the parcsr matrix object to use */
    HYPRE_IJMatrixGetObject(tmpMat->A, (void **) tmpMat->A_parcsr);
    return tmpMat;
  }

  void EliminateBC(HypreParMatrix &A, HypreParMatrix &Ae,
                   const std::vector<int> &ess_dof_list,
                   const HypreParVector &X, HypreParVector &B)
  {
    // B -= Ae*X
    Ae.Mult(-1.0, X, 1.0, B);
    hypre_CSRMatrix *A_diag = hypre_ParCSRMatrixDiag((hypre_ParCSRMatrix *)A.A_parcsr);
    double *data = hypre_CSRMatrixData(A_diag);
    HYPRE_Int *I = hypre_CSRMatrixI(A_diag);
#ifdef MOAB_DEBUG
    HYPRE_Int    *J   = hypre_CSRMatrixJ(A_diag);
    hypre_CSRMatrix *A_offd = hypre_ParCSRMatrixOffd((hypre_ParCSRMatrix *)A.A_parcsr);
    HYPRE_Int *I_offd = hypre_CSRMatrixI(A_offd);
    double *data_offd = hypre_CSRMatrixData(A_offd);
#endif
    std::vector<HYPRE_Complex> bdata(ess_dof_list.size()), xdata(ess_dof_list.size());
    B.GetValues(ess_dof_list.size(), ess_dof_list.data(), bdata.data());
    X.GetValues(ess_dof_list.size(), ess_dof_list.data(), xdata.data());

    for (size_t i = 0; i < ess_dof_list.size(); i++) {
      int r = ess_dof_list[i];
      bdata[r] = data[I[r]] * xdata[r];
#ifdef MOAB_DEBUG

      // Check that in the rows specified by the ess_dof_list, the matrix A has
      // only one entry -- the diagonal.
      // if (I[r+1] != I[r]+1 || J[I[r]] != r || I_offd[r] != I_offd[r+1])
      if (J[I[r]] != r) {
        MB_SET_ERR_RET("the diagonal entry must be the first entry in the row!");
      }

      for (int j = I[r] + 1; j < I[r + 1]; j++) {
        if (data[j] != 0.0) {
          MB_SET_ERR_RET("all off-diagonal entries must be zero!");
        }
      }

      for (int j = I_offd[r]; j < I_offd[r + 1]; j++) {
        if (data_offd[j] != 0.0) {
          MB_SET_ERR_RET("all off-diagonal entries must be zero!");
        }
      }

#endif
    }
  }

// Taubin or "lambda-mu" scheme, which alternates between positive and
// negative step sizes to approximate low-pass filter effect.

  int ParCSRRelax_Taubin(hypre_ParCSRMatrix *A, // matrix to relax with
                         hypre_ParVector *f,    // right-hand side
                         double lambda,
                         double mu,
                         int N,
                         double max_eig,
                         hypre_ParVector *u,    // initial/updated approximation
                         hypre_ParVector *r     // another temp vector
                        )
  {
    hypre_CSRMatrix *A_diag = hypre_ParCSRMatrixDiag(A);
    HYPRE_Int num_rows = hypre_CSRMatrixNumRows(A_diag);
    double *u_data = hypre_VectorData(hypre_ParVectorLocalVector(u));
    double *r_data = hypre_VectorData(hypre_ParVectorLocalVector(r));

    for (int i = 0; i < N; i++) {
      // get residual: r = f - A*u
      hypre_ParVectorCopy(f, r);
      hypre_ParCSRMatrixMatvec(-1.0, A, u, 1.0, r);
      double coef;
      (0 == (i % 2)) ? coef = lambda : coef = mu;

      for (HYPRE_Int j = 0; j < num_rows; j++) {
        u_data[j] += coef * r_data[j] / max_eig;
      }
    }

    return 0;
  }

// FIR scheme, which uses Chebyshev polynomials and a window function
// to approximate a low-pass step filter.

  int ParCSRRelax_FIR(hypre_ParCSRMatrix *A, // matrix to relax with
                      hypre_ParVector *f,    // right-hand side
                      double max_eig,
                      int poly_order,
                      double *fir_coeffs,
                      hypre_ParVector *u,    // initial/updated approximation
                      hypre_ParVector *x0,   // temporaries
                      hypre_ParVector *x1,
                      hypre_ParVector *x2,
                      hypre_ParVector *x3)

  {
    hypre_CSRMatrix *A_diag = hypre_ParCSRMatrixDiag(A);
    HYPRE_Int num_rows = hypre_CSRMatrixNumRows(A_diag);
    double *u_data = hypre_VectorData(hypre_ParVectorLocalVector(u));
    double *x0_data = hypre_VectorData(hypre_ParVectorLocalVector(x0));
    double *x1_data = hypre_VectorData(hypre_ParVectorLocalVector(x1));
    double *x2_data = hypre_VectorData(hypre_ParVectorLocalVector(x2));
    double *x3_data = hypre_VectorData(hypre_ParVectorLocalVector(x3));
    hypre_ParVectorCopy(u, x0);
    // x1 = f -A*x0/max_eig
    hypre_ParVectorCopy(f, x1);
    hypre_ParCSRMatrixMatvec(-1.0, A, x0, 1.0, x1);

    for (HYPRE_Int i = 0; i < num_rows; i++) {
      x1_data[i] /= -max_eig;
    }

    // x1 = x0 -x1
    for (HYPRE_Int i = 0; i < num_rows; i++) {
      x1_data[i] = x0_data[i] - x1_data[i];
    }

    // x3 = f0*x0 +f1*x1
    for (HYPRE_Int i = 0; i < num_rows; i++) {
      x3_data[i] = fir_coeffs[0] * x0_data[i] + fir_coeffs[1] * x1_data[i];
    }

    for (int n = 2; n <= poly_order; n++) {
      // x2 = f - A*x1/max_eig
      hypre_ParVectorCopy(f, x2);
      hypre_ParCSRMatrixMatvec(-1.0, A, x1, 1.0, x2);

      for (HYPRE_Int i = 0; i < num_rows; i++) {
        x2_data[i] /= -max_eig;
      }

      // x2 = (x1-x0) +(x1-2*x2)
      // x3 = x3 +f[n]*x2
      // x0 = x1
      // x1 = x2

      for (HYPRE_Int i = 0; i < num_rows; i++) {
        x2_data[i] = (x1_data[i] - x0_data[i]) + (x1_data[i] - 2 * x2_data[i]);
        x3_data[i] += fir_coeffs[n] * x2_data[i];
        x0_data[i] = x1_data[i];
        x1_data[i] = x2_data[i];
      }
    }

    for (HYPRE_Int i = 0; i < num_rows; i++) {
      u_data[i] = x3_data[i];
    }

    return 0;
  }


}

#endif
