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

#ifndef MOAB_HYPRESOLVER
#define MOAB_HYPRESOLVER

#include "moab/MOABConfig.h"
#include "moab/Core.hpp"

#ifdef MOAB_HAVE_MPI
#include <mpi.h>

// Enable internal hypre timing routines
#ifndef HYPRE_TIMING
#define HYPRE_TIMING
#endif

#include "HypreParVector.hpp"
#include "HypreParMatrix.hpp"

namespace moab
{

/// Base class for solvers
  class AbstractSolver
  {
    protected:
      const Eigen::SparseMatrix<double> *m_operator;

    public:
      /// If true, use the second argument of Mult as an initial guess
      bool iterative_mode;

      /// Initialize a Solver
      explicit AbstractSolver(bool iter_mode)
      { iterative_mode = iter_mode; }

      /// Set/update the solver for the given operator
      virtual void SetOperator(const HypreParMatrix &op) = 0;
  };



/// Abstract class for hypre's solvers and preconditioners
  class HypreSolver : public AbstractSolver
  {
    public:
      typedef HypreParVector Vector;

    protected:
      /// The linear system matrix
      HypreParMatrix *A;

      /// Right-hand side and solution vector
      mutable HypreParVector *B, *X;

      /// Was hypre's Setup function called already?
      mutable int setup_called;

    public:
      HypreSolver(bool iterative = true);

      HypreSolver(HypreParMatrix *_A, bool iterative = true);

      /// Typecast to HYPRE_Solver -- return the solver
      virtual operator HYPRE_Solver() const = 0;

      virtual void Verbosity(int /*print_level*/)
      { /* nothing to do */ }

      /// Set the hypre solver to be used as a preconditioner
      virtual void SetPreconditioner(HypreSolver &/*precond*/)
      { /* Nothing to do */ }

      /// hypre's internal Setup function
      virtual HYPRE_PtrToParSolverFcn SetupFcn() const = 0;
      /// hypre's internal Solve function
      virtual HYPRE_PtrToParSolverFcn SolveFcn() const = 0;

      virtual void SetOperator(const HypreParMatrix &/*op*/)
      { MB_SET_ERR_RET("HypreSolvers do not support SetOperator!"); }

      virtual void GetFinalResidualNorm(double &normr) = 0;

      virtual void GetNumIterations(int &num_iterations) = 0;

      /// Solve the linear system Ax=b
      virtual HYPRE_Int Solve(const HypreParVector &b, HypreParVector &x) const;

      virtual ~HypreSolver();
  };

/// PCG solver in hypre
  class HyprePCG : public HypreSolver
  {
    private:
      HYPRE_Solver pcg_solver;

    public:
      HyprePCG(HypreParMatrix &_A);

      void SetTol(double tol);
      void SetMaxIter(int max_iter);
      void SetLogging(int logging);
      virtual void Verbosity(int print_lvl);

      /// Set the hypre solver to be used as a preconditioner
      virtual void SetPreconditioner(HypreSolver &precond);

      /** Use the L2 norm of the residual for measuring PCG convergence, plus
          (optionally) 1) periodically recompute true residuals from scratch; and
          2) enable residual-based stopping criteria. */
      void SetResidualConvergenceOptions(int res_frequency = -1, double rtol = 0.0);

      /// non-hypre setting
      void SetZeroInintialIterate() { iterative_mode = false; }

      virtual void GetFinalResidualNorm(double &normr)
      {
        HYPRE_ParCSRPCGGetFinalRelativeResidualNorm(pcg_solver,
            &normr);
      }

      virtual void GetNumIterations(int &num_iterations)
      {
        HYPRE_Int num_it;
        HYPRE_ParCSRPCGGetNumIterations(pcg_solver, &num_it);
        num_iterations = internal::to_int(num_it);
      }

      /// The typecast to HYPRE_Solver returns the internal pcg_solver
      virtual operator HYPRE_Solver() const { return pcg_solver; }

      /// PCG Setup function
      virtual HYPRE_PtrToParSolverFcn SetupFcn() const
      { return (HYPRE_PtrToParSolverFcn) HYPRE_ParCSRPCGSetup; }
      /// PCG Solve function
      virtual HYPRE_PtrToParSolverFcn SolveFcn() const
      { return (HYPRE_PtrToParSolverFcn) HYPRE_ParCSRPCGSolve; }

      /// Solve Ax=b with hypre's PCG
      virtual HYPRE_Int Solve(const HypreParVector &b, HypreParVector &x) const;
      using HypreSolver::Solve;

      virtual ~HyprePCG();
  };

/// GMRES solver in hypre
  class HypreGMRES : public HypreSolver
  {
    private:
      HYPRE_Solver gmres_solver;

    public:
      HypreGMRES(HypreParMatrix &_A);

      void SetTol(double atol, double rtol = 1e-20);
      void SetMaxIter(int max_iter);
      void SetKDim(int dim);
      void SetLogging(int logging);
      virtual void Verbosity(int print_lvl);

      /// Set the hypre solver to be used as a preconditioner
      virtual void SetPreconditioner(HypreSolver &precond);

      /** Use the L2 norm of the residual for measuring PCG convergence, plus
          (optionally) 1) periodically recompute true residuals from scratch; and
          2) enable residual-based stopping criteria. */
      void SkipRealResidualCheck()
      {
        HYPRE_GMRESSetSkipRealResidualCheck(gmres_solver, 1);
      }

      /// non-hypre setting
      void SetZeroInintialIterate() { iterative_mode = false; }

      virtual void GetFinalResidualNorm(double &normr)
      {
        HYPRE_ParCSRGMRESGetFinalRelativeResidualNorm(gmres_solver,
            &normr);
      }

      virtual void GetNumIterations(int &num_iterations)
      {
        HYPRE_Int num_it;
        HYPRE_GMRESGetNumIterations(gmres_solver, &num_it);
        num_iterations = internal::to_int(num_it);
      }

      /// The typecast to HYPRE_Solver returns the internal gmres_solver
      virtual operator HYPRE_Solver() const  { return gmres_solver; }

      /// GMRES Setup function
      virtual HYPRE_PtrToParSolverFcn SetupFcn() const
      { return (HYPRE_PtrToParSolverFcn) HYPRE_ParCSRGMRESSetup; }
      /// GMRES Solve function
      virtual HYPRE_PtrToParSolverFcn SolveFcn() const
      { return (HYPRE_PtrToParSolverFcn) HYPRE_ParCSRGMRESSolve; }

      /// Solve Ax=b with hypre's GMRES
      virtual HYPRE_Int Solve(const HypreParVector &b, HypreParVector &x) const;
      using HypreSolver::Solve;

      virtual ~HypreGMRES();
  };

/// The identity operator as a hypre solver
  class HypreIdentity : public HypreSolver
  {
    public:
      virtual operator HYPRE_Solver() const { return NULL; }

      virtual HYPRE_PtrToParSolverFcn SetupFcn() const
      { return (HYPRE_PtrToParSolverFcn) hypre_ParKrylovIdentitySetup; }
      virtual HYPRE_PtrToParSolverFcn SolveFcn() const
      { return (HYPRE_PtrToParSolverFcn) hypre_ParKrylovIdentity; }

      virtual void GetNumIterations(int &num_iterations)
      { num_iterations = 1; }

      virtual void GetFinalResidualNorm(double &normr)
      { normr = 0.0; }

      virtual ~HypreIdentity() { }
  };

/// Jacobi preconditioner in hypre
  class HypreDiagScale : public HypreSolver
  {
    public:
      HypreDiagScale() : HypreSolver() { }
      explicit HypreDiagScale(HypreParMatrix &A) : HypreSolver(&A) { }
      virtual operator HYPRE_Solver() const { return NULL; }

      virtual HYPRE_PtrToParSolverFcn SetupFcn() const
      { return (HYPRE_PtrToParSolverFcn) HYPRE_ParCSRDiagScaleSetup; }
      virtual HYPRE_PtrToParSolverFcn SolveFcn() const
      { return (HYPRE_PtrToParSolverFcn) HYPRE_ParCSRDiagScale; }

      virtual void GetNumIterations(int &num_iterations)
      { num_iterations = 1; }

      virtual void GetFinalResidualNorm(double &normr)
      { normr = 0.0; }

      HypreParMatrix *GetData() { return A; }
      virtual ~HypreDiagScale() { }
  };


/// The ParaSails preconditioner in hypre
  class HypreParaSails : public HypreSolver
  {
    private:
      HYPRE_Solver sai_precond;

    public:
      HypreParaSails(HypreParMatrix &A);

      void SetSymmetry(int sym);

      virtual void GetNumIterations(int &num_iterations)
      { num_iterations = 1; }

      virtual void GetFinalResidualNorm(double &normr)
      { normr = 0.0; }

      /// The typecast to HYPRE_Solver returns the internal sai_precond
      virtual operator HYPRE_Solver() const { return sai_precond; }

      virtual HYPRE_PtrToParSolverFcn SetupFcn() const
      { return (HYPRE_PtrToParSolverFcn) HYPRE_ParaSailsSetup; }
      virtual HYPRE_PtrToParSolverFcn SolveFcn() const
      { return (HYPRE_PtrToParSolverFcn) HYPRE_ParaSailsSolve; }

      virtual ~HypreParaSails();
  };


/// The BoomerAMG solver in hypre
  class HypreBoomerAMG : public HypreSolver
  {
    private:
      HYPRE_Solver amg_precond;

      /// Default, generally robust, BoomerAMG options
      void SetDefaultOptions();

      // If amg_precond is NULL, allocates it and sets default options.
      // Otherwise saves the options from amg_precond, destroys it, allocates a new
      // one, and sets its options to the saved values.
      void ResetAMGPrecond();

    public:
      HypreBoomerAMG();

      HypreBoomerAMG(HypreParMatrix &A);

      virtual void SetOperator(const HypreParMatrix &op);

      virtual void GetNumIterations(int &num_iterations)
      { num_iterations = 1; }

      virtual void GetFinalResidualNorm(double &normr)
      { normr = 0.0; }

      /** More robust options for systems, such as elasticity. Note that BoomerAMG
          assumes Ordering::byVDIM in the finite element space used to generate the
          matrix A. */
      void SetSystemsOptions(int dim);

      virtual void Verbosity(int print_level)
      { HYPRE_BoomerAMGSetPrintLevel(amg_precond, print_level); }

      /// The typecast to HYPRE_Solver returns the internal amg_precond
      virtual operator HYPRE_Solver() const { return amg_precond; }

      virtual HYPRE_PtrToParSolverFcn SetupFcn() const
      { return (HYPRE_PtrToParSolverFcn) HYPRE_BoomerAMGSetup; }
      virtual HYPRE_PtrToParSolverFcn SolveFcn() const
      { return (HYPRE_PtrToParSolverFcn) HYPRE_BoomerAMGSolve; }

      virtual ~HypreBoomerAMG();
  };

}

#endif // MOAB_HAVE_MPI

#endif
