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

#include "HypreSolver.hpp"
#include "HypreParMatrix.hpp"

#ifdef MOAB_HAVE_MPI

#include "hypre_parcsr.hpp"

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


  HypreSolver::HypreSolver(bool iterative) : AbstractSolver(iterative)
  {
    A = NULL;
    setup_called = 0;
    B = X = NULL;
  }

  HypreSolver::HypreSolver(HypreParMatrix *_A, bool iterative)
    : AbstractSolver(iterative)
  {
    A = _A;
    setup_called = 0;
    B = X = NULL;
  }

  HYPRE_Int HypreSolver::Solve(const HypreParVector &b, HypreParVector &x) const
  {
    HYPRE_Int err;

    if (A == NULL) {
      MB_SET_ERR_RET_VAL("HypreSolver::Solve (...) : HypreParMatrix A is missing", 1);
    }

    if (!setup_called) {
      err = SetupFcn()(*this, *A, b.x_par, x.x_par);
      setup_called = 1;
    }

    if (!iterative_mode) {
      x = 0.0;
    }

    err = SolveFcn()(*this, *A, b.x_par, x.x_par);
    return err;
  }

  HypreSolver::~HypreSolver()
  {
    if (B) { delete B; }

    if (X) { delete X; }
  }


  HyprePCG::HyprePCG(HypreParMatrix &_A) : HypreSolver(&_A, true)
  {
    iterative_mode = true;
    HYPRE_ParCSRPCGCreate(A->GetParallelCommunicator()->comm(), &pcg_solver);
  }

  void HyprePCG::SetTol(double tol)
  {
    HYPRE_PCGSetTol(pcg_solver, tol);
  }

  void HyprePCG::SetMaxIter(int max_iter)
  {
    HYPRE_PCGSetMaxIter(pcg_solver, max_iter);
  }

  void HyprePCG::SetLogging(int logging)
  {
    HYPRE_PCGSetLogging(pcg_solver, logging);
  }

  void HyprePCG::Verbosity(int print_lvl)
  {
    HYPRE_ParCSRPCGSetPrintLevel(pcg_solver, print_lvl);
  }

  void HyprePCG::SetPreconditioner(HypreSolver &precond)
  {
    HYPRE_ParCSRPCGSetPrecond(pcg_solver,
                              precond.SolveFcn(),
                              precond.SetupFcn(),
                              precond);
  }

  void HyprePCG::SetResidualConvergenceOptions(int res_frequency, double rtol)
  {
    HYPRE_PCGSetTwoNorm(pcg_solver, 1);

    if (res_frequency > 0) {
      HYPRE_PCGSetRecomputeResidualP(pcg_solver, res_frequency);
    }

    if (rtol > 0.0) {
      HYPRE_PCGSetResidualTol(pcg_solver, rtol);
    }
  }

  HYPRE_Int HyprePCG::Solve(const HypreParVector &b, HypreParVector &x) const
  {
    HYPRE_Int err;
    int myid;
    HYPRE_Int time_index = 0;
    HYPRE_Int num_iterations;
    double final_res_norm;
    moab::ParallelComm* pcomm;
    HYPRE_Int print_level;
    HYPRE_PCGGetPrintLevel(pcg_solver, &print_level);
    pcomm = A->GetParallelCommunicator();

    if (!setup_called) {
      if (print_level > 0) {
#ifdef HYPRE_TIMING
        time_index = hypre_InitializeTiming("PCG Setup");
        hypre_BeginTiming(time_index);
#endif
      }

      err = HYPRE_ParCSRPCGSetup(pcg_solver, *A, (HYPRE_ParVector)b, (HYPRE_ParVector)x);
      setup_called = 1;

      if (err != 0) {
        cout << "PCG Setup failed with error = " << err << endl;
        return err;
      }

      if (print_level > 0) {
#ifdef HYPRE_TIMING
        hypre_EndTiming(time_index);
        hypre_PrintTiming("Setup phase times", pcomm->comm());
        hypre_FinalizeTiming(time_index);
        hypre_ClearTiming();
#endif
      }
    }

    if (print_level > 0) {
#ifdef HYPRE_TIMING
      time_index = hypre_InitializeTiming("PCG Solve");
      hypre_BeginTiming(time_index);
#endif
    }

    if (!iterative_mode) {
      x = 0.0;
    }

    err = HYPRE_ParCSRPCGSolve(pcg_solver, *A, (HYPRE_ParVector)b, (HYPRE_ParVector)x);

    if (print_level > 0) {
#ifdef HYPRE_TIMING
      hypre_EndTiming(time_index);
      hypre_PrintTiming("Solve phase times", pcomm->comm());
      hypre_FinalizeTiming(time_index);
      hypre_ClearTiming();
#endif
      HYPRE_ParCSRPCGGetNumIterations(pcg_solver, &num_iterations);
      HYPRE_ParCSRPCGGetFinalRelativeResidualNorm(pcg_solver,
          &final_res_norm);
      MPI_Comm_rank(pcomm->comm(), &myid);

      if (myid == 0) {
        cout << "PCG Iterations = " << num_iterations << endl
             << "Final PCG Relative Residual Norm = " << final_res_norm
             << endl;
      }
    }

    return err;
  }

  HyprePCG::~HyprePCG()
  {
    HYPRE_ParCSRPCGDestroy(pcg_solver);
  }


  HypreGMRES::HypreGMRES(HypreParMatrix &_A) : HypreSolver(&_A, true)
  {
    MPI_Comm comm;
    int k_dim    = 50;
    int max_iter = 100;
    double tol   = 1e-6;
    iterative_mode = true;
    HYPRE_ParCSRMatrixGetComm(*A, &comm);
    HYPRE_ParCSRGMRESCreate(comm, &gmres_solver);
    HYPRE_ParCSRGMRESSetKDim(gmres_solver, k_dim);
    HYPRE_ParCSRGMRESSetMaxIter(gmres_solver, max_iter);
    HYPRE_ParCSRGMRESSetTol(gmres_solver, tol);
  }

  void HypreGMRES::SetTol(double atol, double rtol)
  {
    HYPRE_GMRESSetAbsoluteTol(gmres_solver, atol);
    HYPRE_GMRESSetTol(gmres_solver, rtol);
  }

  void HypreGMRES::SetMaxIter(int max_iter)
  {
    HYPRE_GMRESSetMaxIter(gmres_solver, max_iter);
  }

  void HypreGMRES::SetKDim(int k_dim)
  {
    HYPRE_GMRESSetKDim(gmres_solver, k_dim);
  }

  void HypreGMRES::SetLogging(int logging)
  {
    HYPRE_GMRESSetLogging(gmres_solver, logging);
  }

  void HypreGMRES::Verbosity(int print_lvl)
  {
    HYPRE_GMRESSetPrintLevel(gmres_solver, print_lvl);
  }

  void HypreGMRES::SetPreconditioner(HypreSolver &precond)
  {
    HYPRE_ParCSRGMRESSetPrecond(gmres_solver,
                                precond.SolveFcn(),
                                precond.SetupFcn(),
                                precond);
  }

  HYPRE_Int HypreGMRES::Solve(const HypreParVector &b, HypreParVector &x) const
  {
    HYPRE_Int err;
    int myid;
    HYPRE_Int time_index = 0;
    HYPRE_Int num_iterations;
    double final_res_norm;
    MPI_Comm comm;
    HYPRE_Int print_level;
    HYPRE_GMRESGetPrintLevel(gmres_solver, &print_level);
    HYPRE_ParCSRMatrixGetComm(*A, &comm);

    if (!setup_called) {
      if (print_level > 0) {
#ifdef HYPRE_TIMING
        time_index = hypre_InitializeTiming("GMRES Setup");
        hypre_BeginTiming(time_index);
#endif
      }

      err = HYPRE_ParCSRGMRESSetup(gmres_solver, *A, b, x);
      setup_called = 1;

      if (err != 0) {
        cout << "PCG Setup failed with error = " << err << endl;
        return err;
      }

      if (print_level > 0) {
#ifdef HYPRE_TIMING
        hypre_EndTiming(time_index);
        hypre_PrintTiming("Setup phase times", comm);
        hypre_FinalizeTiming(time_index);
        hypre_ClearTiming();
#endif
      }
    }

    if (print_level > 0) {
#ifdef HYPRE_TIMING
      time_index = hypre_InitializeTiming("GMRES Solve");
      hypre_BeginTiming(time_index);
#endif
    }

    if (!iterative_mode) {
      x = 0.0;
    }

    err = HYPRE_ParCSRGMRESSolve(gmres_solver, *A, b, x);

    if (print_level > 0) {
#ifdef HYPRE_TIMING
      hypre_EndTiming(time_index);
      hypre_PrintTiming("Solve phase times", comm);
      hypre_FinalizeTiming(time_index);
      hypre_ClearTiming();
#endif
      HYPRE_ParCSRGMRESGetNumIterations(gmres_solver, &num_iterations);
      HYPRE_ParCSRGMRESGetFinalRelativeResidualNorm(gmres_solver,
          &final_res_norm);
      MPI_Comm_rank(comm, &myid);

      if (myid == 0) {
        cout << "GMRES Iterations = " << num_iterations << endl
             << "Final GMRES Relative Residual Norm = " << final_res_norm
             << endl;
      }
    }

    return err;
  }

  HypreGMRES::~HypreGMRES()
  {
    HYPRE_ParCSRGMRESDestroy(gmres_solver);
  }



  HypreParaSails::HypreParaSails(HypreParMatrix &A) : HypreSolver(&A)
  {
    MPI_Comm comm;
    int    sai_max_levels = 1;
    double sai_threshold  = 0.1;
    double sai_filter     = 0.1;
    int    sai_sym        = 0;
    double sai_loadbal    = 0.0;
    int    sai_reuse      = 0;
    int    sai_logging    = 1;
    HYPRE_ParCSRMatrixGetComm(A, &comm);
    HYPRE_ParaSailsCreate(comm, &sai_precond);
    HYPRE_ParaSailsSetParams(sai_precond, sai_threshold, sai_max_levels);
    HYPRE_ParaSailsSetFilter(sai_precond, sai_filter);
    HYPRE_ParaSailsSetSym(sai_precond, sai_sym);
    HYPRE_ParaSailsSetLoadbal(sai_precond, sai_loadbal);
    HYPRE_ParaSailsSetReuse(sai_precond, sai_reuse);
    HYPRE_ParaSailsSetLogging(sai_precond, sai_logging);
  }

  void HypreParaSails::SetSymmetry(int sym)
  {
    HYPRE_ParaSailsSetSym(sai_precond, sym);
  }

  HypreParaSails::~HypreParaSails()
  {
    HYPRE_ParaSailsDestroy(sai_precond);
  }


  HypreBoomerAMG::HypreBoomerAMG()
  {
    HYPRE_BoomerAMGCreate(&amg_precond);
    SetDefaultOptions();
  }

  HypreBoomerAMG::HypreBoomerAMG(HypreParMatrix &A) : HypreSolver(&A)
  {
    HYPRE_BoomerAMGCreate(&amg_precond);
    SetDefaultOptions();
  }

  void HypreBoomerAMG::SetDefaultOptions()
  {
    // AMG coarsening options:
    int coarsen_type = 10;   // 10 = HMIS, 8 = PMIS, 6 = Falgout, 0 = CLJP
    int agg_levels   = 2;    // number of aggressive coarsening levels
    double theta     = 0.25; // strength threshold: 0.25, 0.5, 0.8
    // AMG interpolation options:
    int interp_type  = 6;    // 6 = extended+i, 0 = classical
    int Pmax         = 4;    // max number of elements per row in P
    // AMG relaxation options:
    int relax_type   = 10;    // 8 = l1-GS, 6 = symm. GS, 3 = GS, 18 = l1-Jacobi
    int relax_sweeps = 1;    // relaxation sweeps on each level
    // Additional options:
    int print_level  = 1;    // print AMG iterations? 1 = no, 2 = yes
    int max_levels   = 25;   // max number of levels in AMG hierarchy
    HYPRE_BoomerAMGSetCoarsenType(amg_precond, coarsen_type);
    HYPRE_BoomerAMGSetAggNumLevels(amg_precond, agg_levels);
    HYPRE_BoomerAMGSetRelaxType(amg_precond, relax_type);
    HYPRE_BoomerAMGSetNumSweeps(amg_precond, relax_sweeps);
    HYPRE_BoomerAMGSetStrongThreshold(amg_precond, theta);
    HYPRE_BoomerAMGSetInterpType(amg_precond, interp_type);
    HYPRE_BoomerAMGSetPMaxElmts(amg_precond, Pmax);
    HYPRE_BoomerAMGSetPrintLevel(amg_precond, print_level);
    HYPRE_BoomerAMGSetMaxLevels(amg_precond, max_levels);
    // Use as a preconditioner (one V-cycle, zero tolerance)
    HYPRE_BoomerAMGSetMaxIter(amg_precond, 1);
    HYPRE_BoomerAMGSetTol(amg_precond, 0.0);
  }

  void HypreBoomerAMG::ResetAMGPrecond()
  {
    HYPRE_Int coarsen_type;
    HYPRE_Int agg_levels;
    HYPRE_Int relax_type;
    HYPRE_Int relax_sweeps;
    HYPRE_Real theta;
    HYPRE_Int interp_type;
    HYPRE_Int Pmax;
    HYPRE_Int print_level;
    HYPRE_Int dim;
    hypre_ParAMGData *amg_data = (hypre_ParAMGData *)amg_precond;
    // read options from amg_precond
    HYPRE_BoomerAMGGetCoarsenType(amg_precond, &coarsen_type);
    agg_levels = hypre_ParAMGDataAggNumLevels(amg_data);
    relax_type = hypre_ParAMGDataUserRelaxType(amg_data);
    relax_sweeps = hypre_ParAMGDataUserNumSweeps(amg_data);
    HYPRE_BoomerAMGGetStrongThreshold(amg_precond, &theta);
    hypre_BoomerAMGGetInterpType(amg_precond, &interp_type);
    HYPRE_BoomerAMGGetPMaxElmts(amg_precond, &Pmax);
    HYPRE_BoomerAMGGetPrintLevel(amg_precond, &print_level);
    HYPRE_BoomerAMGGetNumFunctions(amg_precond, &dim);
    HYPRE_BoomerAMGDestroy(amg_precond);
    HYPRE_BoomerAMGCreate(&amg_precond);
    HYPRE_BoomerAMGSetCoarsenType(amg_precond, coarsen_type);
    HYPRE_BoomerAMGSetAggNumLevels(amg_precond, agg_levels);
    HYPRE_BoomerAMGSetRelaxType(amg_precond, relax_type);
    HYPRE_BoomerAMGSetNumSweeps(amg_precond, relax_sweeps);
    HYPRE_BoomerAMGSetMaxLevels(amg_precond, 25);
    HYPRE_BoomerAMGSetTol(amg_precond, 0.0);
    HYPRE_BoomerAMGSetMaxIter(amg_precond, 1); // one V-cycle
    HYPRE_BoomerAMGSetStrongThreshold(amg_precond, theta);
    HYPRE_BoomerAMGSetInterpType(amg_precond, interp_type);
    HYPRE_BoomerAMGSetPMaxElmts(amg_precond, Pmax);
    HYPRE_BoomerAMGSetPrintLevel(amg_precond, print_level);
    HYPRE_BoomerAMGSetNumFunctions(amg_precond, dim);
  }

  void HypreBoomerAMG::SetOperator(const HypreParMatrix &op)
  {
    if (A) { ResetAMGPrecond(); }

    // update base classes: Operator, Solver, HypreSolver
    A = const_cast<HypreParMatrix *>(&op);
    setup_called = 0;
    delete X;
    delete B;
    B = X = NULL;
  }

  void HypreBoomerAMG::SetSystemsOptions(int dim)
  {
    HYPRE_BoomerAMGSetNumFunctions(amg_precond, dim);
    // More robust options with respect to convergence
    HYPRE_BoomerAMGSetAggNumLevels(amg_precond, 0);
    HYPRE_BoomerAMGSetStrongThreshold(amg_precond, dim * 0.25);
  }


  HypreBoomerAMG::~HypreBoomerAMG()
  {
    HYPRE_BoomerAMGDestroy(amg_precond);
  }

}

#endif
