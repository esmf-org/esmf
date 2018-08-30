#ifndef ESMCI_Solver_H
#define ESMCI_Solver_H

#include <vector>
#include <algorithm>
#include <functional>
#include "ESMCI_Poly.h"
#include "ESMCI_PolyUV.h"
#include "ESMCI_PolyTwoV.h"
#include "ESMCI_LPolyMV.h"
#include "ESMCI_PolyTwoDV.h"
#include "ESMCI_PolyDer.h"
#include "ESMCI_Mat.h"

namespace ESMCI{
  namespace MapperUtil{

    /* Solver class
     * The solver class is used to minimize a set of input values
     * wrt user specified constaint functions
     */
    template<typename T>
    class SESolver{
      public:
        /* A function object used to generate vals in an under-constrained system */
        class UConstraintValGenerator{
          public:
            virtual std::vector<T> get_vals(const std::vector<T> &available_vals) const { assert(0); }
            virtual ~UConstraintValGenerator() = default;
        };
        virtual ~SESolver() = default;
        SESolver(const std::vector<std::string> &vnames, const std::vector<T> &init_vals, const std::vector<TwoVIDPoly<T> > &funcs);
        SESolver(const std::vector<std::string> &vnames, const std::vector<T> &init_vals, const std::vector<TwoDVIDPoly<T> > &two_dvid_funcs, const std::vector<MVIDLPoly<T> > &mvid_lpoly_funcs);
        SESolver(const std::vector<std::string> &vnames, const std::vector<T> &init_vals, const std::vector<TwoVIDPoly<T> > &two_vid_funcs, const std::vector<MVIDLPoly<T> > &mvid_lpoly_funcs);
        void set_init_vals(const std::vector<T> &init_vals);
        void set_funcs(const std::vector<TwoVIDPoly<T> > &funcs);
        void set_funcs(const std::vector<TwoDVIDPoly<T> > &dfuncs);
        void set_funcs(const std::vector<MVIDLPoly<T> > &mvid_lpoly_funcs);
        void set_niters(int niters);
        std::vector<T> minimize(const UConstraintValGenerator &uc_vgen) const;

      private:
        static const int DEFAULT_NITERS = 10;
        int niters_;
        std::vector<std::string> vnames_;
        std::vector<T> init_vals_;
        std::vector<TwoVIDPoly<T> > funcs_;
        std::vector<TwoDVIDPoly<T> > dfuncs_;
        std::vector<MVIDLPoly<T> > mvid_lpoly_funcs_;
    }; //class SESolver


    template<typename T>
    inline SESolver<T>::SESolver(const std::vector<std::string> &vnames, const std::vector<T> &init_vals, const std::vector<TwoVIDPoly<T> > &funcs):niters_(DEFAULT_NITERS),vnames_(vnames),init_vals_(init_vals),funcs_(funcs)
    {}

    template<typename T>
    inline SESolver<T>::SESolver(const std::vector<std::string> &vnames,
      const std::vector<T> &init_vals,
      const std::vector<TwoVIDPoly<T> > &two_vid_funcs,
      const std::vector<MVIDLPoly<T> > &mvid_lpoly_funcs)
        :niters_(DEFAULT_NITERS),
        vnames_(vnames),
        init_vals_(init_vals),
        funcs_(two_vid_funcs),
        mvid_lpoly_funcs_(mvid_lpoly_funcs)
    {}

    template<typename T>
    inline SESolver<T>::SESolver(const std::vector<std::string> &vnames,
      const std::vector<T> &init_vals,
      const std::vector<TwoDVIDPoly<T> > &two_dvid_funcs,
      const std::vector<MVIDLPoly<T> > &mvid_lpoly_funcs)
        :niters_(DEFAULT_NITERS),
        vnames_(vnames),
        init_vals_(init_vals),
        dfuncs_(two_dvid_funcs),
        mvid_lpoly_funcs_(mvid_lpoly_funcs)
    {}

    template<typename T>
    inline void SESolver<T>::set_init_vals(const std::vector<T> &init_vals)
    {
      init_vals_ = init_vals;
    }

    template<typename T>
    inline void SESolver<T>::set_funcs(const std::vector<TwoVIDPoly<T> > &funcs)
    {
      funcs_ = funcs;
    }

    template<typename T>
    inline void SESolver<T>::set_funcs(const std::vector<TwoDVIDPoly<T> > &funcs)
    {
      dfuncs_ = funcs;
    }

    template<typename T>
    inline void SESolver<T>::set_funcs(const std::vector<MVIDLPoly<T> > &mvid_lpoly_funcs)
    {
      mvid_lpoly_funcs_ = mvid_lpoly_funcs;
    }

    template<typename T>
    inline void SESolver<T>::set_niters(int niters)
    {
      niters_ = niters;
    }

    /* Some utils used by the solver is included in a separate 
     * namespace
     */
    namespace SESolverUtils{
      /* Calculate the Jacobian for the user constraints 
       * specified via funcs wrt the variables specified in vnames
       * Each row, i, in the Jacobian is the partial derivative
       * of funcs[i] wrt each of the variables in vnames
       * J[i][j] = d(funcs[i])/d(vnames[j])
       */  
      template<typename T>
      std::vector<std::vector<GenPoly<T, int> *> > calc_jacobian(
          const std::vector<std::string> &vnames,
          const std::vector<TwoVIDPoly<T> > &funcs,
          const std::vector<TwoDVIDPoly<T> > &dfuncs,
          const std::vector<MVIDLPoly<T> > &mvid_lpoly_funcs)
      {
        //int nrows = funcs.size() + mvid_lpoly_funcs;
        int ncols = vnames.size();
        std::vector<std::vector<GenPoly<T, int> *> > JF;
        for(int i=0; i<static_cast<int>(funcs.size()); i++){
          std::vector<GenPoly<T, int> *> JFrow;
          for(int j=0; j<ncols; j++){
            TwoVIDPoly<T> *pdf = new TwoVIDPoly<T>();
            int ret = FindPDerivative(funcs[i], vnames[j], *pdf);
            assert(ret == 0);
            std::cout << "d(" << funcs[i] << ")/d" << vnames[j].c_str()
                      << " = " << *pdf << "\n";
            JFrow.push_back(pdf);
          }
          JF.push_back(JFrow);
        }
        for(int i=0; i<static_cast<int>(dfuncs.size()); i++){
          std::vector<GenPoly<T, int> *> JFrow;
          for(int j=0; j<ncols; j++){
            TwoDVIDPoly<T> *pdf = new TwoDVIDPoly<T>();
            int ret = FindPDerivative(dfuncs[i], vnames[j], *pdf);
            assert(ret == 0);
            std::cout << "d(" << dfuncs[i] << ")/d" << vnames[j].c_str()
                      << " = " << *pdf << "\n";
            JFrow.push_back(pdf);
          }
          JF.push_back(JFrow);
        }
        for(int i=0; i<static_cast<int>(mvid_lpoly_funcs.size()); i++){
          std::vector<GenPoly<T, int> *> JFrow;
          for(int j=0; j<ncols; j++){
            MVIDLPoly<T> *pdf = new MVIDLPoly<T>();
            int ret = FindPDerivative(mvid_lpoly_funcs[i], vnames[j], *pdf);
            assert(ret == 0);
            std::cout << "d(" << mvid_lpoly_funcs[i] << ")/d" << vnames[j].c_str()
                      << " = " << *pdf << "\n";
            JFrow.push_back(pdf);
          }
          JF.push_back(JFrow);
        }
        return JF;
      }

      template<typename T>
      void free_jacobian(
        std::vector<std::vector<GenPoly<T, int> *> > &j)
      {
        for(typename std::vector<std::vector<GenPoly<T, int> *> >::iterator iter =
              j.begin();
            iter != j.end(); ++iter){
          for(typename std::vector<GenPoly<T, int> *>::iterator piter = (*iter).begin();
              piter != (*iter).end(); ++piter){
            delete(*piter);
          }
        }
      }

      /* Evaluate the Jacobian, j, using values of variables
       * provided in vvals (& corresponding to vnames)
       * Each value in index i of vvals is the value of the variable
       * with name vnames[i].
       * Note that a function/polynomial, j[i][j], in j need
       * not contain all the variables in vnames/vvals vectors.
       * Returns a 2D matrix (mxn) of values of type T
       */
      template<typename T>
      Matrix<T> eval_jacobian(
        const std::vector<std::vector<GenPoly<T, int> *> > &j,
        const std::vector<std::string> &vnames,
        const std::vector<T> &vvals)
      {
        int nrows = j.size();
        int ncols = vnames.size();

        assert(vnames.size() == vvals.size());
        std::vector<T> res_data;
        for(typename std::vector<std::vector<GenPoly<T, int> *> >::const_iterator
              jciter = j.cbegin(); jciter != j.cend(); ++jciter){
          std::vector<GenPoly<T, int> *> jrow = *jciter;
          for(typename std::vector<GenPoly<T, int> *>::const_iterator
              jrciter = jrow.cbegin(); jrciter != jrow.cend(); ++jrciter){
            GenPoly<T, int> *pp = *jrciter;
            assert(pp);
            std::vector<std::string> pvnames = pp->get_vnames();
            std::vector<T> pvvals;
            for(std::vector<std::string>::const_iterator pvciter = pvnames.cbegin();
                pvciter != pvnames.cend(); ++pvciter){
              int i=0;
              for(std::vector<std::string>::const_iterator vciter = vnames.cbegin();
                vciter != vnames.cend(); ++vciter, i++){
                if(*vciter == *pvciter){
                  pvvals.push_back(vvals[i]);
                }
              }
            }
            res_data.push_back(pp->eval(pvvals));
          }
        }

        std::vector<int> dims = {nrows, ncols};
        Matrix<T> res(dims, res_data);
        return res;
      }

      /* Evaluate a vector of functions, funcs, using values of
       * variables provided in vvals (& corresponding to vnames)
       * Each value in index i of vvals is the value of the variable
       * with name vnames[i].
       * Note that a function/polynomial, funcs[k], in funcs need
       * not contain all the variables in vnames/vvals vectors.
       * Returns a 1D matrix (mx1) of values of type T
       */
      template<typename T>
      Matrix<T> eval_funcs(const std::vector<const GenPoly<T, int> *>& funcs,
        const std::vector<std::string> &vnames,
        const std::vector<T> &vvals)
      {
        int nrows = funcs.size();
        int ncols = 1;
        std::vector<T> res_data;

        assert(vnames.size() == vvals.size());
        for(typename std::vector<const GenPoly<T, int> *>::const_iterator
              cfiter = funcs.cbegin(); cfiter != funcs.cend(); ++cfiter){
          const GenPoly<T, int> *pp = *cfiter;
          std::vector<std::string> pvnames = pp->get_vnames();
          std::vector<T> pvvals;
          for(std::vector<std::string>::const_iterator pvciter = pvnames.cbegin();
              pvciter != pvnames.cend(); ++pvciter){
            int i=0;
            for(std::vector<std::string>::const_iterator vciter = vnames.cbegin();
              vciter != vnames.cend(); ++vciter, i++){
              if(*vciter == *pvciter){
                pvvals.push_back(vvals[i]);
              }
            }
          }
          res_data.push_back(pp->eval(pvvals));
        }

        std::vector<int> dims = {nrows, ncols};
        Matrix<T> res(dims, res_data);
        return res;
      }
    } // SESolverUtils

    template<typename T>
    std::vector<T> SESolver<T>::minimize(
      const UConstraintValGenerator &uc_vgen) const
    {
      enum Sol_Constraint_type{
        SOL_UNDER_CONSTRAINED,
        SOL_NORMAL_CONSTRAINED,
        SOL_OVER_CONSTRAINED
      };
      Sol_Constraint_type sc_type = SOL_NORMAL_CONSTRAINED;
      std::vector<int> Xi_dims = {static_cast<int>(vnames_.size()), 1};
      Matrix<T> Xi(Xi_dims, init_vals_);
      std::vector<T> Xj_init_vals(vnames_.size(), 0);
      Matrix<T> Xj(Xi_dims, Xj_init_vals);

      std::vector<const GenPoly<T, int> *> all_funcs;
      for(typename std::vector<TwoVIDPoly<T> >::const_iterator iter = funcs_.cbegin();
          iter != funcs_.cend(); ++iter){
        all_funcs.push_back(&(*iter));
      }
      for(typename std::vector<TwoDVIDPoly<T> >::const_iterator iter = dfuncs_.cbegin();
          iter != dfuncs_.cend(); ++iter){
        all_funcs.push_back(&(*iter));
      }
      for(typename std::vector<MVIDLPoly<T> >::const_iterator iter = mvid_lpoly_funcs_.cbegin();
          iter != mvid_lpoly_funcs_.cend(); ++iter){
        all_funcs.push_back(&(*iter));
      }

      std::vector<std::string> jvnames(vnames_.cbegin(), vnames_.cend());
      if(all_funcs.size() < vnames_.size()){
        sc_type = SOL_UNDER_CONSTRAINED;
        int nvars_in_solver = static_cast<int>(all_funcs.size());
        jvnames.erase(jvnames.begin() + nvars_in_solver, jvnames.end());
      }
      else if(all_funcs.size() > vnames_.size()){
        sc_type = SOL_OVER_CONSTRAINED;
      }

      /* C = Sum of all input variables */
      T C = std::accumulate(init_vals_.cbegin(), init_vals_.cend(), 0);

      /* Calculate the Jacobian matrix for the user constraint functions */
      std::vector<std::vector<GenPoly<T, int> *> > JF =
        SESolverUtils::calc_jacobian(jvnames, funcs_, dfuncs_, mvid_lpoly_funcs_);

      /* Solve for new values of Xi such that we minimize the user 
       * specified constraints (specified via funcs_ )
       * A basic Newton Raphson solver solving the eqn:
       * Xj = Xi - Jinv * Feval; where
       * Xi => Vector with current values of the input vars
       * Feval => Vector that contains funcs_, user specified constraints
       *          evaluated for Xi
       * Jinv => Inverse of the Jacobian of user specified constraint funcs
       * Xj => The next (after one iteration) value of Xi
       */
      for(int i=0; i<niters_; i++){
        Matrix<T> J = SESolverUtils::eval_jacobian(JF, vnames_, Xi.get_data());
        //std::cout << "Jacobian :\n";
        //std::cout << J << "\n";
        int ncols = vnames_.size();
        std::vector<T> J_data = J.get_data();
        std::vector<int> J_dims = J.get_dims();
        Matrix<T> Jinv = J;
        /* If there are more variables than functions we add a last row of ones
         * in the Jacobian. This row corresponds to a constraint that all of
         * the input variables need to add up to a constant
         */
        //bool needs_last_row = (vnames_.size() > static_cast<size_t>(J_dims[0])) ?
        //                      true : false;
        bool needs_last_row = false;
        if(needs_last_row){
          assert(vnames_.size() == static_cast<size_t>(J_dims[0]) + 1);
          J_dims[0] += 1;
          for(int i=0; i<ncols; i++){
            J_data.push_back(1);
          }

          Matrix<T> J_with_last_rones(J_dims, J_data); 
          std::cout << "vnames sz = " << vnames_.size() << "\n";
          for(int i=0; i<J_dims.size(); i++){
            std::cout << J_dims[i] << ", ";
          }
          std::cout << "\n";
          std::cout << " Jacobian : \n" << J_with_last_rones << "\n";
          Jinv = J_with_last_rones.inv();
        }
        else{
          if((sc_type == SOL_OVER_CONSTRAINED) ||
              (sc_type == SOL_UNDER_CONSTRAINED)){
            Jinv = J.pinv();
          }
          else{
            Jinv = J.inv();
          }
        }

        Matrix<T> Feval = SESolverUtils::eval_funcs(all_funcs, vnames_, Xi.get_data());
        /* If there are more variables than functions we add a last row.
         * The last row corresponds to a constraint that the sum of the input
         * variables need to remain constant (see variable C above,
         * i.e., Sum (Xi) - C = 0)
         */
        if(needs_last_row){
          std::vector<T> Feval_data = Feval.get_data();
          std::vector<int> Feval_dims = Feval.get_dims();
          Feval_dims[0] += 1;
          std::vector<T> Xi_data = Xi.get_data();
          T Ci = std::accumulate(Xi_data.cbegin(), Xi_data.cend(), 0);
          Feval_data.push_back(Ci - C);
          Matrix<T> Feval_with_last_row(Feval_dims, Feval_data);
          Feval = Feval_with_last_row;
        }

        if(sc_type == SOL_UNDER_CONSTRAINED){
          int nv_in_solver = static_cast<int>(all_funcs.size());
          std::vector<int> X_uc_dims = {nv_in_solver, 1};
          std::vector<T> Xi_uc_data = Xi.get_data();
          Xi_uc_data.erase(Xi_uc_data.begin() + nv_in_solver, Xi_uc_data.end());
          std::vector<T> Xj_uc_data = Xj.get_data();
          Xj_uc_data.erase(Xj_uc_data.begin() + nv_in_solver, Xj_uc_data.end());
          Matrix<T> Xj_uc(X_uc_dims, Xj_uc_data);
          Matrix<T> Xi_uc(X_uc_dims, Xi_uc_data);

          Xj_uc = Xi_uc - Jinv * Feval;
          std::vector<T> Xj_uc_vals = Xj_uc.get_data();
          Xj = Matrix<T>(Xi_dims, uc_vgen.get_vals(Xj_uc_vals));
        }
        else{
          Xj = Xi - Jinv * Feval;
        }
        //std::cout << "Xj = \n" << Xj << "\n";
        Xi = Xj;
      }
  
      SESolverUtils::free_jacobian(JF);

      return Xj.get_data();
    }

  } // namespace MapperUtil
} // namespace ESMCI


#endif // ESMCI_Solver_H
