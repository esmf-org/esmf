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
        SESolver(const std::vector<T> &init_vals, const std::vector<TwoVIDPoly<T> > &funcs);
        void set_init_vals(const std::vector<T> &init_vals);
        void set_funcs(const std::vector<TwoVIDPoly<T> > &funcs);
        void set_niters(int niters);
        std::vector<T> minimize(void ) const;

      private:
        static const int DEFAULT_NITERS = 10;
        int niters_;
        std::vector<T> init_vals_;
        std::vector<TwoVIDPoly<T> > funcs_;
    }; //class SESolver


    template<typename T>
    inline SESolver<T>::SESolver(const std::vector<T> &init_vals, const std::vector<TwoVIDPoly<T> > &funcs):niters_(DEFAULT_NITERS),init_vals_(init_vals),funcs_(funcs)
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

    template<typename T>
    std::vector<T> SESolver<T>::minimize(void ) const
    {
      // Matrix<T> Xi(init_vals_);
      // Matrix<T> Xj;

      // Xj = Xi - inv(J) * F
      return init_vals_;
    }

  } // namespace MapperUtil
} // namespace ESMCI


#endif // ESMCI_Solver_H
