#ifndef ESMCI_Solver_H
#define ESMCI_Solver_H

#include <vector>
#include <algorithm>
#include <functional>
#include <cmath>
#include <numeric>
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
        /* Set the initial values for the solver */
        void set_init_vals(const std::vector<T> &init_vals);
        /* Set the constraint functions for the solver */
        void set_funcs(const std::vector<TwoVIDPoly<T> > &funcs);
        void set_funcs(const std::vector<TwoDVIDPoly<T> > &dfuncs);
        void set_funcs(const std::vector<MVIDLPoly<T> > &mvid_lpoly_funcs);
        /* Set reshape functions for the solver. These functions are used
         * to reshape the solution in the solver when the solutions 
         * diverge from user constraints (e.g. Ensure that Sum(Xi) = C )
         */
        void set_reshape_funcs(const std::vector<MVIDLPoly<T> >
          &reshape_mvid_lpoly_funcs);
        /* Scale and center the functions for the given vals */
        void scale_and_center_funcs(const std::vector<T> &vals);
        /* Set the maximum number of iterations for the solver */
        void set_niters(int niters);
        /* Minimize the values based on the user specified constraints */
        std::vector<T> minimize(const UConstraintValGenerator &uc_vgen);

      private:
        /* The solver is,
         * Under constrained : No enough equations to solve the variables
         * Normal constrained : Same number of equations as the variables
         * Over constrained : More equations than variables
         */
        enum Sol_Constraint_type{
          SOL_UNDER_CONSTRAINED,
          SOL_NORMAL_CONSTRAINED,
          SOL_OVER_CONSTRAINED
        };
        static const int DEFAULT_NITERS = 10;
        int niters_;
        std::vector<std::string> vnames_;
        std::vector<T> init_vals_;
        std::vector<TwoVIDPoly<T> > funcs_;
        std::vector<TwoDVIDPoly<T> > dfuncs_;
        std::vector<MVIDLPoly<T> > mvid_lpoly_funcs_;
        std::vector<MVIDLPoly<T> > reshape_mvid_lpoly_funcs_;
        
        void reshape_solution(T *val, std::size_t val_sz); 
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
    inline void SESolver<T>::set_reshape_funcs(const std::vector<MVIDLPoly<T> >
      &reshape_mvid_lpoly_funcs)
    {
      reshape_mvid_lpoly_funcs_ = reshape_mvid_lpoly_funcs;
    }

    /* Some utils used by the solver is included in a separate 
     * namespace
     */
    namespace SESolverUtils{
      template<typename T>
      inline T feval(const GenPoly<T, int> &f,
        const std::vector<T> &vals, const std::vector<std::string> &vnames)
      {
        std::vector<T> fvals;
        std::vector<std::string> fvnames = f.get_vnames();
        std::size_t i = 0;
        assert(vals.size() == vnames.size());
        for(std::vector<std::string>::const_iterator vnames_iter = vnames.cbegin();
              vnames_iter != vnames.cend(); ++vnames_iter, i++){
          for(std::vector<std::string>::const_iterator fvnames_iter = fvnames.cbegin();
                fvnames_iter != fvnames.cend(); ++fvnames_iter){
            if(*vnames_iter == *fvnames_iter){
              fvals.push_back(vals[i]);
            }
          }
        }
        return f.eval(fvals);
      }

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
            //std::cout << "d(" << funcs[i] << ")/d" << vnames[j].c_str()
            //          << " = " << *pdf << "\n";
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
            //std::cout << "d(" << dfuncs[i] << ")/d" << vnames[j].c_str()
            //          << " = " << *pdf << "\n";
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
            //std::cout << "d(" << mvid_lpoly_funcs[i] << ")/d" << vnames[j].c_str()
            //          << " = " << *pdf << "\n";
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

      template<typename T>
      class StrIntCmp{
        public:
          bool operator()(const std::pair<std::string, T> &a,
                        const std::pair<std::string, T> &b) const
          {
            return a.first < b.first;
          }
      };

      /* Map function variables to global variables
       * Pre-computing these maps in the solver reduces the execution time
       * of the solver
       */
      template<typename T>
      void map_fv2gv(
        const std::vector<std::vector<GenPoly<T, int> *> > &j,
        const std::vector<std::string> &gvnames,
        std::vector<std::vector<std::vector<std::size_t > > > &j_map_fv2gv)
      {
        std::vector<std::pair<std::string, T> > vinfos;
        for(std::size_t i=0; i<gvnames.size(); i++){
          vinfos.push_back(
            std::pair<std::string, T>(gvnames[i], i));
        }
        StrIntCmp<std::size_t> cmp;
        std::sort(vinfos.begin(), vinfos.end(), cmp);

        for(typename std::vector<std::vector<GenPoly<T, int> *> >::const_iterator
              jciter = j.cbegin(); jciter != j.cend(); ++jciter){
          std::vector<GenPoly<T, int> *> jrow = *jciter;
          std::vector<std::vector<std::size_t > > jrow_map_fv2gv;
          for(typename std::vector<GenPoly<T, int> *>::const_iterator
              jrciter = jrow.cbegin(); jrciter != jrow.cend(); ++jrciter){
            GenPoly<T, int> *pp = *jrciter;
            assert(pp);
            std::vector<std::size_t > func_map_fv2gv;
            std::vector<std::string> pvnames = pp->get_vnames();
            std::vector<T> pvvals;
            for(std::vector<std::string>::const_iterator pvciter = pvnames.cbegin();
                pvciter != pvnames.cend(); ++pvciter){
              std::pair<std::string, T> pvinfo(*pvciter, static_cast<T>(0));
              typename std::vector<std::pair<std::string, T> >::iterator vinfos_iter
                = std::lower_bound(vinfos.begin(), vinfos.end(), pvinfo);
              if(vinfos_iter != vinfos.end()){
                if((*vinfos_iter).first == *pvciter){
                  func_map_fv2gv.push_back((*vinfos_iter).second);
                }
              }
            }
            assert(!func_map_fv2gv.empty());
            jrow_map_fv2gv.push_back(func_map_fv2gv);
          }
          j_map_fv2gv.push_back(jrow_map_fv2gv);
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
        const std::vector<T> &vvals,
        const std::vector<std::vector<std::vector<size_t> > > &j_map_fv2gv)
      {
        int nrows = j.size();
        int ncols = vnames.size();

        assert(vnames.size() == vvals.size());
        assert(j.size() == j_map_fv2gv.size());

        std::vector<T> res_data;
        res_data.reserve(j.size() * vnames.size());
        std::vector<T> pvvals;
        pvvals.reserve(vnames.size());
        typename std::vector<std::vector<GenPoly<T, int> *> >::const_iterator
          jciter = j.cbegin();
        typename std::vector<std::vector<std::vector<std::size_t> > >::const_iterator
          j_map_fv2gv_citer = j_map_fv2gv.cbegin();
        for(; (jciter != j.cend()) && (j_map_fv2gv_citer != j_map_fv2gv.cend());
              ++jciter, ++j_map_fv2gv_citer){
          //std::vector<GenPoly<T, int> *> jrow = *jciter;
          //std::vector<std::vector<std::size_t> > jrow_map_fv2gv = *j_map_fv2gv_citer;
          typename std::vector<GenPoly<T, int> *>::const_iterator
            jrciter = (*jciter).cbegin();
          typename std::vector<std::vector<std::size_t > >::const_iterator 
            jrow_map_fv2gv_citer = (*j_map_fv2gv_citer).cbegin();
          for(; (jrciter != (*jciter).cend()) 
                  && (jrow_map_fv2gv_citer != (*j_map_fv2gv_citer).cend());
                ++jrciter, ++jrow_map_fv2gv_citer){
            GenPoly<T, int> *pp = *jrciter;
            //std::vector<std::size_t > func_map_fv2gv = *jrow_map_fv2gv_citer;
            assert(pp);
            //std::vector<std::string> pvnames = pp->get_vnames();
            pvvals.clear();
            for(std::vector<std::size_t>::const_iterator func_map_fv2gv_iter = 
                  (*jrow_map_fv2gv_citer).cbegin();
                  func_map_fv2gv_iter != (*jrow_map_fv2gv_citer).cend();
                  ++func_map_fv2gv_iter){
              assert((*func_map_fv2gv_iter >= 0) &&
                      (*func_map_fv2gv_iter < vvals.size()));
              pvvals.push_back(vvals[*func_map_fv2gv_iter]);
            }
            res_data.push_back(pp->eval(pvvals));
          }
        }

        std::vector<int> dims = {nrows, ncols};
        Matrix<T> res(dims, res_data);
        return res;
      }

      /* Map function variables to global variables
       * Each function has an associated vector of indices that map from
       * the function variables to the global variables
       */
      template<typename T>
      void map_fv2gv(const std::vector<const GenPoly<T, int> *>& funcs,
        const std::vector<std::string> &gvnames,
        std::vector<std::vector<std::size_t> > &funcs_map_fv2gv)
      {
        assert(funcs_map_fv2gv.empty());

        std::vector<std::pair<std::string, T> > vinfos;
        for(std::size_t i=0; i<gvnames.size(); i++){
          vinfos.push_back(
            std::pair<std::string, T>(gvnames[i], i));
        }
        StrIntCmp<std::size_t> cmp;
        std::sort(vinfos.begin(), vinfos.end(), cmp);

        for(typename std::vector<const GenPoly<T, int> *>::const_iterator
              cfiter = funcs.cbegin(); cfiter != funcs.cend(); ++cfiter){
          const GenPoly<T, int> *pp = *cfiter;
          std::vector<std::string> pvnames = pp->get_vnames();
          std::vector<T> pvvals;
          std::vector<std::size_t > func_map_fv2gv;
          for(std::vector<std::string>::const_iterator pvciter = pvnames.cbegin();
              pvciter != pvnames.cend(); ++pvciter){
            std::pair<std::string, T> pvinfo(*pvciter, static_cast<T>(0));
            typename std::vector<std::pair<std::string, T> >::iterator vinfos_iter
              = std::lower_bound(vinfos.begin(), vinfos.end(), pvinfo);
            if(vinfos_iter != vinfos.end()){
              if((*vinfos_iter).first == *pvciter){
                pvvals.push_back((*vinfos_iter).second);
                func_map_fv2gv.push_back((*vinfos_iter).second);
              }
            }
          }
          assert(!func_map_fv2gv.empty());
          funcs_map_fv2gv.push_back(func_map_fv2gv);
        }
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
        const std::vector<T> &vvals,
        const std::vector<std::vector<std::size_t> > &funcs_map_fv2gv)
      {
        int nrows = funcs.size();
        int ncols = 1;
        std::vector<T> res_data;

        assert(funcs.size() == funcs_map_fv2gv.size());
        assert(vnames.size() == vvals.size());

        std::vector<T> pvvals;
        pvvals.reserve(vnames.size());
        res_data.reserve(funcs.size());
        typename std::vector<const GenPoly<T, int> *>::const_iterator
          cfiter = funcs.cbegin();
        typename std::vector<std::vector<std::size_t> >::const_iterator
          funcs_map_citer = funcs_map_fv2gv.cbegin();
        for(;(cfiter != funcs.cend()) && (funcs_map_citer != funcs_map_fv2gv.cend());
              ++cfiter, ++funcs_map_citer){
          const GenPoly<T, int> *pp = *cfiter;
          //std::vector<std::size_t> func_map_fv2gv = *funcs_map_citer;
          pvvals.clear();
          for(std::vector<std::size_t>::const_iterator citer = 
                (*funcs_map_citer).cbegin();
                citer != (*funcs_map_citer).cend(); ++citer){
            assert((*citer >= 0) && (*citer < vvals.size()));
            pvvals.push_back(vvals[*citer]);
          }

          res_data.push_back(pp->eval(pvvals));
        }

        std::vector<int> dims = {nrows, ncols};
        Matrix<T> res(dims, res_data);
        return res;
      }

      /* Used to sort an array of indices to a vector */
      template<typename T>
      class CmpIndexByVal{
        public:
          CmpIndexByVal(const std::vector<T> &arr);
          CmpIndexByVal(const T *arr, std::size_t arr_sz);
          bool operator()(const std::size_t &idx_a, const std::size_t &idx_b) const;
        private:
          const T *arr_;
          std::size_t arr_sz_;
      };

      template<typename T>
      CmpIndexByVal<T>::CmpIndexByVal(const std::vector<T> &arr):
        arr_(&arr[0]), arr_sz_(arr.size())
      {
      }

      template<typename T>
      CmpIndexByVal<T>::CmpIndexByVal(const T *arr, std::size_t arr_sz):
        arr_(arr), arr_sz_(arr_sz)
      {
      }

      template<typename T>
      bool CmpIndexByVal<T>::operator()(const std::size_t &idx_a,
            const std::size_t &idx_b) const
      {
        assert((idx_a >= 0) && (idx_a < arr_sz_));
        assert((idx_b >= 0) && (idx_b < arr_sz_));
        return (arr_[idx_a] < arr_[idx_b]);
      }

    } // SESolverUtils

    /* Scale and center the constraint functions based on vals */
    template<typename T>
    inline void SESolver<T>::scale_and_center_funcs(const std::vector<T> &vals)
    {
      std::vector<T> fevals;
      for(typename std::vector<TwoVIDPoly<T> >::const_iterator iter = funcs_.cbegin();
          iter != funcs_.cend(); ++iter){
        T val = SESolverUtils::feval(*iter, vals, vnames_);
        if(val == 0){
          /* FIXME: Is this a correct strategy ? */
          /* Use the constant value in the equation as the func value */
          std::vector<T> zvals(vals.size(), static_cast<T>(0));
          val = SESolverUtils::feval(*iter, zvals, vnames_);
        }
        fevals.push_back(val);
      }
      for(typename std::vector<TwoDVIDPoly<T> >::const_iterator iter = dfuncs_.cbegin();
          iter != dfuncs_.cend(); ++iter){
        T val = SESolverUtils::feval(*iter, vals, vnames_);
        if(val == 0){
          /* FIXME: Is this a correct strategy ? */
          /* Use the constant value in the equation as the func value */
          std::vector<T> zvals(vals.size(), static_cast<T>(0));
          val = SESolverUtils::feval(*iter, zvals, vnames_);
        }
        fevals.push_back(val);
      }
      for(typename std::vector<MVIDLPoly<T> >::const_iterator iter = mvid_lpoly_funcs_.cbegin();
          iter != mvid_lpoly_funcs_.cend(); ++iter){
        T val = SESolverUtils::feval(*iter, vals, vnames_);
        if(val == 0){
          /* FIXME: Is this a correct strategy ? */
          /* Use the constant value in the equation as the func value */
          std::vector<T> zvals(vals.size(), static_cast<T>(0));
          val = SESolverUtils::feval(*iter, zvals, vnames_);
        }
        fevals.push_back(val);
      }

      if(fevals.size() > 1){
        /* Find mean and standard deviation */
        T init_val = static_cast<T>(0);
        T mean = init_val;
        T stddev = init_val;

        mean = std::accumulate(fevals.begin(), fevals.end(), init_val)/fevals.size();

        std::vector<T> fevals_minus_mean_sq(fevals.size(), 0);
        typename std::vector<T>::iterator fmms_vals_iter = fevals_minus_mean_sq.begin();
        for(typename std::vector<T>::const_iterator
              fevals_citer = fevals.cbegin();
              (fevals_citer != fevals.cend())
                && (fmms_vals_iter != fevals_minus_mean_sq.end());
              ++fevals_citer, ++fmms_vals_iter){
          *fmms_vals_iter = (*fevals_citer - mean) * (*fevals_citer - mean);
        }
        T fevals_minus_mean_sq_sum =
          std::accumulate(fevals_minus_mean_sq.begin(), fevals_minus_mean_sq.end(),
            init_val);
        stddev =
          sqrt(fevals_minus_mean_sq_sum/static_cast<T>(fevals_minus_mean_sq.size()-1));

        T scale = (stddev != 0) ? stddev : 1;

        /* Scale function coefficients so that all functions are centered around
         * mean and have a standard deviation of 1
         */
        typename std::vector<T>::const_iterator fevals_iter = fevals.cbegin();
        for(typename std::vector<TwoVIDPoly<T> >::iterator iter = funcs_.begin();
            (iter != funcs_.end()) && (fevals_iter != fevals.cend());
              ++iter, ++fevals_iter){
          T sc_ratio;
          std::vector<T> coeffs = (*iter).get_coeffs();
          if((*fevals_iter != 0) && (*fevals_iter != mean)){
            sc_ratio = ((*fevals_iter - mean)/scale)/(*fevals_iter);
          }
          else{
            sc_ratio = static_cast<T>(-1) * (mean/scale);
          }
          assert(sc_ratio != 0);
          for(typename std::vector<T>::iterator coeff_iter = coeffs.begin();
                coeff_iter != coeffs.end(); ++coeff_iter){
            *coeff_iter = (*coeff_iter) * sc_ratio;
          }
          (*iter).set_coeffs(coeffs);
        }
        for(typename std::vector<TwoDVIDPoly<T> >::iterator iter = dfuncs_.begin();
            (iter != dfuncs_.end()) && (fevals_iter != fevals.cend());
            ++iter, ++fevals_iter){
          T sc_ratio;
          std::vector<T> coeffs = (*iter).get_coeffs();
          if((*fevals_iter != 0) && (*fevals_iter != mean)){
            sc_ratio = ((*fevals_iter - mean)/scale)/(*fevals_iter);
          }
          else{
            sc_ratio = static_cast<T>(-1) * (mean/scale);
          }
          assert(sc_ratio != 0);
          for(typename std::vector<T>::iterator coeff_iter = coeffs.begin();
                coeff_iter != coeffs.end(); ++coeff_iter){
            *coeff_iter = (*coeff_iter) * sc_ratio;
          }
          //std::cout << "UnScaled dfunc : " << *iter << "\n";
          (*iter).set_coeffs(coeffs);
          //std::cout << "Scaled dfunc : " << *iter << "\n";
        }
        for(typename std::vector<MVIDLPoly<T> >::iterator
              iter = mvid_lpoly_funcs_.begin();
            (iter != mvid_lpoly_funcs_.end()) && (fevals_iter != fevals.cend());
            ++iter, ++fevals_iter){
          T sc_ratio;
          std::vector<T> coeffs = (*iter).get_coeffs();
          if((*fevals_iter != 0) && (*fevals_iter != mean)){
            sc_ratio = ((*fevals_iter - mean)/scale)/(*fevals_iter);
          }
          else{
            sc_ratio = static_cast<T>(-1) * (mean/scale);
          }
          assert(sc_ratio != 0);
          for(typename std::vector<T>::iterator coeff_iter = coeffs.begin();
                coeff_iter != coeffs.end(); ++coeff_iter){
            *coeff_iter = (*coeff_iter) * sc_ratio;
          }
          //std::cout << "UnScaled mvid_lpoly_func : " << *iter << "\n";
          (*iter).set_coeffs(coeffs);
          //std::cout << "Scaled mvid_lpoly_func : " << *iter << "\n";
        }
      }
    }

    template<typename T>
    inline void SESolver<T>::set_niters(int niters)
    {
      niters_ = niters;
    }


    template<typename T>
    std::vector<T> SESolver<T>::minimize(
      const UConstraintValGenerator &uc_vgen)
    {
      Sol_Constraint_type sc_type = SOL_NORMAL_CONSTRAINED;
      std::vector<int> Xi_dims = {static_cast<int>(vnames_.size()), 1};
      Matrix<T> Xi(Xi_dims, init_vals_);
      std::vector<T> Xj_init_vals(vnames_.size(), 0);
      Matrix<T> Xj(Xi_dims, Xj_init_vals);
      /* Zero matrix */
      const Matrix<T> XZ(Xi_dims, Xj_init_vals);

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
        std::cout << "SOL_UNDER_CONSTRAINED\n";
      }
      else if(all_funcs.size() > vnames_.size()){
        sc_type = SOL_OVER_CONSTRAINED;
        std::cout << "SOL_OVER_CONSTRAINED\n";
      }
      else{
        std::cout << "SOL_NORMAL_CONSTRAINED\n";
      }

      /* C = Sum of all input variables */
      T C = std::accumulate(init_vals_.cbegin(), init_vals_.cend(), 0);

      /* Calculate the Jacobian matrix for the user constraint functions */
      std::vector<std::vector<GenPoly<T, int> *> > JF =
        SESolverUtils::calc_jacobian(jvnames, funcs_, dfuncs_, mvid_lpoly_funcs_);

      std::vector<std::vector<std::vector<std::size_t > > > JF_map_fv2gv;
      SESolverUtils::map_fv2gv(JF, vnames_, JF_map_fv2gv);

      std::vector<std::vector<std::size_t > > eval_funcs_map_fv2gv;
      SESolverUtils::map_fv2gv(all_funcs, vnames_, eval_funcs_map_fv2gv);

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
        Matrix<T> Feval = SESolverUtils::eval_funcs(all_funcs,
          vnames_, Xi.get_data(), eval_funcs_map_fv2gv);
        //std::cout << "Feval :\n";
        //std::cout << Feval << "\n";

        Matrix<T> J = SESolverUtils::eval_jacobian(JF,
          vnames_, Xi.get_data(), JF_map_fv2gv);
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
        if((sc_type == SOL_OVER_CONSTRAINED) ||
            (sc_type == SOL_UNDER_CONSTRAINED)){
          Jinv = J.pinv();
        }
        else{
          Jinv = J.inv();
        }

        /* If there are more variables than functions we add a last row.
         * The last row corresponds to a constraint that the sum of the input
         * variables need to remain constant (see variable C above,
         * i.e., Sum (Xi) - C = 0)
         */

        const T DAMP_CONST = static_cast<T>(0.01);
        if(sc_type == SOL_UNDER_CONSTRAINED){
          int nv_in_solver = static_cast<int>(all_funcs.size());
          std::vector<int> X_uc_dims = {nv_in_solver, 1};
          std::vector<T> Xi_uc_data = Xi.get_data();
          Xi_uc_data.erase(Xi_uc_data.begin() + nv_in_solver, Xi_uc_data.end());
          std::vector<T> Xj_uc_data = Xj.get_data();
          Xj_uc_data.erase(Xj_uc_data.begin() + nv_in_solver, Xj_uc_data.end());
          Matrix<T> Xj_uc(X_uc_dims, Xj_uc_data);
          Matrix<T> Xi_uc(X_uc_dims, Xi_uc_data);

          Xj_uc = Xi_uc - Jinv * Feval * DAMP_CONST;
          std::vector<T> Xj_uc_vals = Xj_uc.get_data();
          Xj = Matrix<T>(Xi_dims, uc_vgen.get_vals(Xj_uc_vals));
        }
        else{
          Xj = Xi - Jinv * Feval * DAMP_CONST;
        }
        // Reset -ve values of Xi, and ensure that Sum(Xi) = C
        //std::cout << "Xj before shaping = \n" << Xj << "\n";
        T *Xj_data = Xj.get_data_by_ref();
        reshape_solution(Xj_data, vnames_.size());
        //std::cout << "Xj after shaping \n" << Xj << "\n";
        //scale_and_center_funcs(Xj.get_data());
        Matrix<T> Xi_diff = Xi - Xj;
        Xi = Xj;
        /* The solution should at least change by 1 PET for the solver to continue */
        double TOL = 0.99;
        if(Xi_diff.equals(XZ, TOL)){
          std::cout << "Solver has converged...\n";
          break;
        }
      }
  
      SESolverUtils::free_jacobian(JF);

      return Xj.get_data();
    }

    /* Reshape solution so that all Xi > 0 and Sum(Xi) = Xi_exp_sum */
    template<typename T>
    void SESolver<T>::reshape_solution(T *Xi, std::size_t Xi_sz)
    {
      assert(Xi_sz == vnames_.size());
      assert(Xi_sz == init_vals_.size());

      for(std::size_t i=0; i<Xi_sz; i++){
        if(Xi[i] < static_cast<T>(1)){
          Xi[i] = static_cast<T>(1);
        }
      }

      if(!reshape_mvid_lpoly_funcs_.empty()){
        for(typename std::vector<MVIDLPoly<T> >::iterator
              iter = reshape_mvid_lpoly_funcs_.begin();
              iter != reshape_mvid_lpoly_funcs_.end(); ++iter){
          std::vector<int> Xi_needs_reshape;
          std::vector<std::string> reshape_fvnames = (*iter).get_vnames();
          std::size_t i=0;
          for(std::vector<std::string>::const_iterator citer = vnames_.cbegin();
              (citer != vnames_.cend()) && (i < Xi_sz); ++citer, i++){
            for(std::vector<std::string>::const_iterator
                  reshape_citer = reshape_fvnames.cbegin();
                  reshape_citer != reshape_fvnames.cend(); ++reshape_citer){
              if(*citer == *reshape_citer){
                Xi_needs_reshape.push_back(i);
              }
            }
          }

          T Xi_exp_sum = static_cast<T>(0);
          T Xi_sum = static_cast<T>(0);
          for(std::vector<int>::const_iterator citer = Xi_needs_reshape.cbegin();
                citer != Xi_needs_reshape.cend(); ++citer){
            Xi_exp_sum += init_vals_[*citer];
            Xi_sum += Xi[*citer];
          }
          //T tol = 1.0;
          //assert((Xi_exp_sum - Xi_sum) < tol);
          assert(Xi_exp_sum > 0);
          assert(Xi_sum > 0);
          for(std::vector<int>::const_iterator citer = Xi_needs_reshape.cbegin();
                citer != Xi_needs_reshape.cend(); ++citer){
            Xi[*citer] = (Xi[*citer] / Xi_sum) * Xi_exp_sum;
            if(Xi[*citer] < static_cast<T>(1)){
              Xi[*citer] = static_cast<T>(1);
            }
          }
        }
      }
      else{
        T Xi_exp_sum = std::accumulate(init_vals_.cbegin(), init_vals_.cend(), 0);

        T Xi_sum = std::accumulate(Xi, Xi+Xi_sz, static_cast<T>(0));
        for(std::size_t i=0; i<Xi_sz; i++){
          Xi[i] = (Xi[i] / Xi_sum) * Xi_exp_sum;
          if(Xi[i] < static_cast<T>(1)){
            Xi[i] = static_cast<T>(1);
          }
        }
      }

      /* Turn all doubles into ints in Xi */
      for(std::size_t i=0; i<Xi_sz; i++){
        Xi[i] = static_cast<int>(Xi[i]);
      }

      T C_exp = std::accumulate(init_vals_.cbegin(), init_vals_.cend(), 0);
      T C = std::accumulate(Xi, Xi+Xi_sz, static_cast<T>(0));
      int rem = static_cast<int>(C_exp - C);

      if(rem != 0){
        /* Process array from smallest to largest, use an array of indices
         * instead of sorting the array itself, since we need to maintain
         * the ordering in Xi
         */
        std::vector<std::size_t> Xi_idx(Xi_sz, 0);
        for(std::size_t i=0; i<Xi_sz; i++){
          Xi_idx[i] = i;
        }
        SESolverUtils::CmpIndexByVal<T> cmp(Xi, Xi_sz);
        std::sort(Xi_idx.begin(), Xi_idx.end(), cmp);

        /* Note rem can be -ve or +ve */
        int cur_rem = rem;
        for(std::vector<std::size_t>::const_iterator idx_iter = Xi_idx.cbegin();
              idx_iter != Xi_idx.cend(); ++idx_iter){
          int ntry_rem = static_cast<int>(static_cast<T>(rem) * Xi[*idx_iter]/C);
          if(ntry_rem != 0){
            if(abs(ntry_rem) >= static_cast<int>(Xi[*idx_iter])){
              /* Leave at least one PET for the component */
              cur_rem -= static_cast<int>(Xi[*idx_iter]) - 1;
              Xi[*idx_iter] = 1;
            }
            else{
              cur_rem -= ntry_rem;
              Xi[*idx_iter] += ntry_rem;
            }
          }
          else{
            /* Ignore */
            continue;
          }
        }
        if(cur_rem != 0){
          /* Use the largest Xi to fix the remaining */
          Xi[Xi_idx[Xi_idx.size()-1]] += cur_rem;
        }
      }

    }

  } // namespace MapperUtil
} // namespace ESMCI


#endif // ESMCI_Solver_H
