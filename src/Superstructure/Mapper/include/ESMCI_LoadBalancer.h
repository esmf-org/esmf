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
    /* Load Balancer class
     * The load balancer class is used to balance the load between different
     * components. The load is balanced by minimizing the idle time, the
     * difference between wallclock times, between the different components
     * and reallocating PETs (number of PETs for each component) accordingly.
     */ 
    template<typename T>
    class LoadBalancer{
      public:
        LoadBalancer(int ncomps);
        LoadBalancer(int ncomps, const std::vector<T> &wexec_times, const std::vector<T>& serial_exec_times, const std::vector<int> &npets);
        void set_lb_info(const std::vector<T> &wexec_times, const std::vector<T> &serial_exec_times, const std::vector<int> &npets);
        T optimize(std::vector<int> &opt_npets);
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

    /* Set execution info for the load balancer
     * wxec_times => Wallclock times for each component
     * serial_exec_times => serial execution times for each component
     * npets => Number of PETs used by each component
     */
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

    /* Record the execution info provided by the user
     * The execution info is recorded everytime user calls optimize()
     * This information is used for finding the scaling function for
     * each component (eventually used to calculate user constraints
     * for the solver)
     */
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

    /* Optimize the user specified PET list by redistributing
     * PETs between the different components such that it
     * reduces the idle time between the different components
     * Idle time is the difference between the execution times
     * (wallclock times) of two components
     */
    template<typename T>
    inline T LoadBalancer<T>::optimize(std::vector<int> &opt_npets)
    {
      //T opt_wtime = static_cast<T>(0);
      std::vector<T> opt_wtimes(npets_.size(), static_cast<T>(0));
      rec_exec_info();

      int total_pets = std::accumulate(npets_.cbegin(),
                        npets_.cend(), 0);

      //std::vector<int> res(npets_.size(), total_pets);
      if(opt_npets.empty()){
        opt_npets.resize(npets_.size());
        for(std::vector<int>::iterator iter = opt_npets.begin();
            iter != opt_npets.end(); ++iter){
          *iter = total_pets;
        }
      }

      T total_time = std::accumulate(serial_exec_times_.cbegin(),
                      serial_exec_times_.cend(), 0);

      const int MIN_VALS_REQD_FOR_POLY_FIT = 3;
      if((past_wexec_times_.size() > 0)
          && (past_wexec_times_[0].size() < MIN_VALS_REQD_FOR_POLY_FIT)){
        /* We don't have enough data to calculate a polynomial fit for the
         * scaling function for the components. Assume that all components
         * scale linearly and redistribute PETs based on the weighted
         * (weighted on the serial execution time) average
         */ 
        for(unsigned int i=0; i< static_cast<int>(opt_npets.size()); i++){
          opt_npets[i] =
            static_cast<int> (opt_npets[i] * serial_exec_times_[i] / total_time);
          opt_wtimes[i] = static_cast<T>(serial_exec_times_[i]/opt_npets[i]);
        }
      }
      else{
        std::vector<UVIDPoly<T> > sfuncs;
        std::vector<T> npets(npets_.begin(), npets_.end());

        std::vector<std::string> vnames;
        for(int i=0; i<ncomps_; i++){
          std::ostringstream ostr;
          ostr << "x" << i << "_" << 1;

          vnames.push_back(ostr.str());
        }

        /* Calculate a polynomial scaling function for each component */
        for(int i=0; i<ncomps_; i++){
          UVIDPoly<T> p;
          int max_deg = 2;
          std::vector<T> past_comp_npets(past_npets_[i].cbegin(), past_npets_[i].cend());
          for(int j=0; j<past_comp_npets.size(); j++){
            std::cout << "fit data : " << past_comp_npets[j] << " : "
              << past_wexec_times_[i][j] << "\n";
          }
          int ret = PolyFit(POLY_FIT_LS_LAPACK, max_deg, past_comp_npets, past_wexec_times_[i], p);
          assert(ret == 0);
          std::vector<std::string> tmp_vnames(1, vnames[i]);
          p.set_vnames(tmp_vnames);
          std::cout << "uvidp" << i << " = " << p << "\n";
          sfuncs.push_back(p);
        }
      
        /* Calculate the user constraint functions for the solver.
         * The user contraint function, ifuncs[i], is the square of the
         * function that defines the idle time between component 0 and
         * component i.
         * The idle time between two components is calculated by finding
         * the difference between the wallclock time taken by the 
         * components
         */
        std::vector<TwoVIDPoly<T> > ifuncs;
        for(int i=1; i<ncomps_; i++){
          TwoVIDPoly<T> p;
          std::vector<std::string> tmp_vnames;
          tmp_vnames.push_back(vnames[0]);
          tmp_vnames.push_back(vnames[i]);
          p.set_vnames(tmp_vnames);
          /* Get scaling functions for component 0 and component i */
          TwoVIDPoly<T> p1(sfuncs[0], tmp_vnames);
          TwoVIDPoly<T> p2(sfuncs[i], tmp_vnames);
          /* Calculate idle time between component 0 and component i */
          p = p1 - p2;
          /* User constraint function is the square of the idle time function */
          TwoVIDPoly<T> psq = p * p;
          ifuncs.push_back(psq);
        }

        /* Use the solver to optimize the number of PETs */
        SESolver<T> solver(vnames, npets, ifuncs);
        std::vector<T> new_pets = solver.minimize();
        for(int i=0; i<ncomps_; i++){
          opt_npets[i] = static_cast<int>(new_pets[i]);
          opt_wtimes[i] = static_cast<T>(serial_exec_times_[i]/opt_npets[i]);
        }
      }

      typename std::vector<T>::iterator opt_wtime_iter = std::max_element(
                                                  opt_wtimes.begin(), opt_wtimes.end());
      if(opt_wtime_iter != opt_wtimes.end()){
        return *opt_wtime_iter;
      }
      else{
        return static_cast<T>(0);
      }
    }

  } // MapperUtil
} //namespace ESMCI
#endif // ESMCI_LoadBalancer_H
