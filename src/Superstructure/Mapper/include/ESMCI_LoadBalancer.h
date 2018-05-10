#ifndef ESMCI_LoadBalancer_H
#define ESMCI_LoadBalancer_H

#include <vector>
#include <algorithm>
#include <functional>
#include <iostream>
#include <sstream>
#include "ESMCI_Poly.h"
#include "ESMCI_PolyUV.h"
#include "ESMCI_PolyTwoV.h"
#include "ESMCI_PolyDer.h"
#include "ESMCI_PolyFit.h"
#include "ESMCI_Mat.h"
#include "ESMCI_Solver.h"

namespace ESMCI{
  namespace MapperUtil{
    template<typename T>
    class LoadBalancer{
      public:
        LoadBalancer(int ncomps);
        LoadBalancer(int ncomps, const std::vector<T> &wexec_times, const std::vector<T>& serial_exec_times, const std::vector<int> &npets);
        void set_lb_info(const std::vector<T> &wexec_times, const std::vector<T> &serial_exec_times, const std::vector<int> &npets);
        std::vector<int> optimize(void );
      private:
        void rec_exec_info(void );
        int ncomps_;
        std::vector<T> wexec_times_;
        std::vector<std::vector<T> > past_wexec_times_;
        std::vector<T> serial_exec_times_;
        std::vector<std::vector<T> > past_serial_exec_times_;
        std::vector<int> npets_;
        std::vector<std::vector<int> > past_npets_;
    }; //class LoadBalancer

    template<typename T>
    inline LoadBalancer<T>::LoadBalancer(int ncomps, const std::vector<T>& wexec_times, const std::vector<T>& serial_exec_times, const std::vector<int> &npets):ncomps_(ncomps), wexec_times_(wexec_times), serial_exec_times_(serial_exec_times), npets_(npets)
    {
      assert(static_cast<int>(wexec_times_.size()) == ncomps);
      assert(static_cast<int>(serial_exec_times_.size()) == ncomps);
      assert(static_cast<int>(npets_.size()) == ncomps);
    }

    template<typename T>
    inline void LoadBalancer<T>::set_lb_info(const std::vector<T> &wexec_times, const std::vector<T> &serial_exec_times, const std::vector<int> &npets)
    {
      wexec_times_ = wexec_times;
      assert(static_cast<int>(wexec_times_.size()) == ncomps_);
      serial_exec_times_ = serial_exec_times;
      assert(static_cast<int>(serial_exec_times_.size()) == ncomps_);
      npets_ = npets;
      assert(static_cast<int>(npets_.size()) == ncomps_);
    }

    template<typename T>
    inline void LoadBalancer<T>::rec_exec_info(void )
    {
      if(past_wexec_times_.size() == 0){
        for(int i=0; i<ncomps_; i++){
          std::vector<T> wexec_times = { wexec_times_[i] };
          std::vector<T> serial_exec_times = { serial_exec_times_[i] };
          std::vector<int> npets = { npets_[i] };
          past_wexec_times_.push_back(wexec_times);
          past_serial_exec_times_.push_back(serial_exec_times);
          past_npets_.push_back(npets);
        }
      }
      else{
        assert(static_cast<int>(past_wexec_times_.size()) == ncomps_);
        assert(static_cast<int>(wexec_times_.size()) == ncomps_);
        for(int i=0; i<ncomps_; i++){
          past_wexec_times_[i].push_back(wexec_times_[i]);
          past_serial_exec_times_[i].push_back(serial_exec_times_[i]);
          past_npets_[i].push_back(npets_[i]);
        }
      }
    }

    template<typename T>
    inline std::vector<int> LoadBalancer<T>::optimize(void )
    {
      rec_exec_info();

      int total_pets = std::accumulate(npets_.cbegin(),
                        npets_.cend(), 0);

      std::vector<int> res(npets_.size(), total_pets);

      T total_time = std::accumulate(serial_exec_times_.cbegin(),
                      serial_exec_times_.cend(), 0);

      if(past_wexec_times_.size() < 3){
        // Use weighted average for the solution
        /*
        std::transform(serial_exec_times_.cbegin(), serial_exec_times_.cend(),
          res.begin(), std::multiplies<int>());
        std::transform(res.begin(), res.end(), res.begin(),
          std::bind2nd(std::divides<int>(), total_time));
        */
        for(unsigned int i=0; i<res.size(); i++){
          res[i] = static_cast<int> (res[i] * serial_exec_times_[i] / total_time);
        }
      }
      else{
        std::vector<TwoVIDPoly<T> > sfuncs;
        std::vector<T> npets(npets_.begin(), npets_.end());
        for(int i=0; i<ncomps_; i++){
          UVIDPoly<T> p;
          int max_deg = 2;
          std::vector<T> past_comp_npets(past_npets_[i].cbegin(), past_npets_[i].cend());
          int ret = PolyFit(POLY_FIT_LS_LAPACK, max_deg, past_comp_npets, past_wexec_times_[i], p);
          assert(ret == 0);
          sfuncs.push_back(p);
        }

        std::vector<std::string> vnames;
        for(int i=0; i<ncomps_; i++){
          std::ostringstream ostr;
          ostr << "x" << i << "_" << 1;

          vnames.push_back(ostr.str());
        }

        std::vector<TwoVIDPoly<T> > ifuncs;
        for(int i=1; i<ncomps_; i++){
          TwoVIDPoly<T> p = sfuncs[0] - sfuncs[i];
          std::vector<std::string> tmp_vnames;
          tmp_vnames.push_back(vnames[0]);
          tmp_vnames.push_back(vnames[i]);
          p.set_vnames(tmp_vnames);
          ifuncs.push_back(p);
        }
        /*
        std::vector<T> last_ifunc_vals(vnames.size(), 1);
        last_ifunc_vals.push_back(-1 * total_pets);

        TwoVIDPoly last_ifunc(last_ifunc_vals);
        std::vector<std::string> tmp_vnames;
        for(int i=0; i<ncomps; i++){
          tmp_vnames.push_back(vnames[i]);
        }
        */

        SESolver<T> solver(vnames, npets, ifuncs);
        std::vector<T> new_pets = solver.minimize();
        for(int i=0; i<ncomps_; i++){
          res.push_back(static_cast<int>(new_pets[i]));
        }
      }

      return res;
    }

  } // MapperUtil
} //namespace ESMCI
#endif // ESMCI_LoadBalancer_H
