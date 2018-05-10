#ifndef ESMCI_Solver_H
#define ESMCI_Solver_H

#include <vector>
#include <algorithm>
#include <functional>
#include "ESMCI_Poly.h"
#include "ESMCI_PolyUV.h"
#include "ESMCI_PolyTwoV.h"
#include "ESMCI_PolyDer.h"
#include "ESMCI_Mat.h"

namespace ESMCI{
  namespace MapperUtil{

    template<typename T>
    class SESolver{
      public:
        virtual ~SESolver() = default;
        SESolver(const std::vector<std::string> &vnames, const std::vector<T> &init_vals, const std::vector<TwoVIDPoly<T> > &funcs);
        void set_init_vals(const std::vector<T> &init_vals);
        void set_funcs(const std::vector<TwoVIDPoly<T> > &funcs);
        void set_niters(int niters);
        std::vector<T> minimize(void ) const;

      private:
        static const int DEFAULT_NITERS = 10;
        int niters_;
        std::vector<std::string> vnames_;
        std::vector<T> init_vals_;
        std::vector<TwoVIDPoly<T> > funcs_;
    }; //class SESolver


    template<typename T>
    inline SESolver<T>::SESolver(const std::vector<std::string> &vnames, const std::vector<T> &init_vals, const std::vector<TwoVIDPoly<T> > &funcs):niters_(DEFAULT_NITERS),vnames_(vnames),init_vals_(init_vals),funcs_(funcs)
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
    inline void SESolver<T>::set_niters(int niters)
    {
      niters_ = niters;
    }

    namespace SESolverUtils{
      template<typename T>
      std::vector<std::vector<TwoVIDPoly<T> > > calc_jacobian(
          const std::vector<std::string> &vnames,
          const std::vector<TwoVIDPoly<T> > &funcs)
      {
        /*
        std::vector<std::string> vnames;
        for(typename std::vector<TwoVIDPoly<T> >::const_iterator citer = funcs.cbegin();
            citer != funcs.cend(); ++citer){
          std::vector<std::string> tmp_vnames = (*citer).get_vnames();
          for(std::vector<std::string>::const_iterator
                tmp_vnames_citer = tmp_vnames.cbegin();
              tmp_vnames_citer != tmp_vnames.cend(); ++tmp_vnames_citer){
            vnames.push_back(*tmp_vnames_citer);
          }
        }
        std::sort(vnames.begin(), vnames.end());
        std::vector<std::string>::iterator new_end_iter =
          std::unique(vnames.begin(), vnames.end());
        vnames.resize(std::distance(vnames.begin(), new_end_iter));
        */

        int nrows = funcs.size();
        int ncols = vnames.size();
        std::vector<std::vector<TwoVIDPoly<T> > > JF;
        for(int i=0; i<nrows; i++){
          std::vector<TwoVIDPoly<T> > JFrow;
          for(int j=0; j<ncols; j++){
            TwoVIDPoly<T> df;
            int ret = FindPDerivative(funcs[i], vnames[j], df);
            assert(ret == 0);
            JFrow.push_back(df);
          }
          JF.push_back(JFrow);
        }
        return JF;
      }

      template<typename T>
      Matrix<T> eval_jacobian(
        const std::vector<std::vector<TwoVIDPoly<T> > > &j,
        const std::vector<std::string> &vnames,
        const std::vector<T> &vvals)
      {
        int nrows = j.size();
        int ncols = vnames.size();

        assert(vnames.size() == vvals.size());
        std::vector<T> res_data;
        for(typename std::vector<std::vector<TwoVIDPoly<T> > >::const_iterator
              jciter = j.cbegin(); jciter != j.cend(); ++jciter){
          std::vector<TwoVIDPoly<T> > jrow = *jciter;
          for(typename std::vector<TwoVIDPoly<T> >::const_iterator
              jrciter = jrow.cbegin(); jrciter != jrow.cend(); ++jrciter){
            TwoVIDPoly<T> p = *jrciter;
            std::vector<std::string> pvnames = p.get_vnames();
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
            res_data.push_back(p.eval(pvvals));
          }
        }

        std::vector<int> dims = {nrows, ncols};
        Matrix<T> res(dims, res_data);
        return res;
      }

      template<typename T>
      Matrix<T> eval_funcs(const std::vector<TwoVIDPoly<T> >& funcs,
        const std::vector<std::string> &vnames,
        const std::vector<T> &vvals)
      {
        int nrows = funcs.size();
        int ncols = 1;
        std::vector<T> res_data;

        for(typename std::vector<TwoVIDPoly<T> >::const_iterator
              cfiter = funcs.cbegin(); cfiter != funcs.cend(); ++cfiter){
          TwoVIDPoly<T> p = *cfiter;
          std::vector<std::string> pvnames = p.get_vnames();
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
          res_data.push_back(p.eval(pvvals));
        }

        std::vector<int> dims = {nrows, ncols};
        Matrix<T> res(dims, res_data);
        return res;
      }
    } // SESolverUtils

    template<typename T>
    std::vector<T> SESolver<T>::minimize(void ) const
    {
      std::vector<int> Xi_dims = {static_cast<int>(vnames_.size()), 1};
      Matrix<T> Xi(Xi_dims, init_vals_);
      std::vector<T> Xj_init_vals(vnames_.size(), 0);
      Matrix<T> Xj(Xi_dims, Xj_init_vals);

      T C = std::accumulate(init_vals_.cbegin(), init_vals_.cend(), 0);

      std::vector<std::vector<TwoVIDPoly<T> > > JF =
        SESolverUtils::calc_jacobian(vnames_,funcs_);

      for(int i=0; i<niters_; i++){
        Matrix<T> J = SESolverUtils::eval_jacobian(JF, vnames_, Xi.get_data());
        int ncols = vnames_.size();
        std::vector<T> J_data = J.get_data();
        std::vector<int> J_dims = J.get_dims();
        Matrix<T> Jinv = J;
        bool needs_last_row = (vnames_.size() > static_cast<size_t>(J_dims[0])) ?
                              true : false;
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
          Jinv = J_with_last_rones.inv();
        }
        else{
          Jinv = J.inv();
        }

        Matrix<T> Feval = SESolverUtils::eval_funcs(funcs_, vnames_, Xi.get_data());
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

        Xj = Xi - Jinv * Feval;
        Xi = Xj;
      }

      return Xj.get_data();
    }

  } // namespace MapperUtil
} // namespace ESMCI


#endif // ESMCI_Solver_H
