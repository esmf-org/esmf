#ifndef ESMCI_LoadBalancer_H
#define ESMCI_LoadBalancer_H

#include <vector>
#include <algorithm>
#include <functional>
#include "ESMCI_Poly.h"
#include "ESMCI_PolyUV.h"
#include "ESMCI_PolyTwoV.h"
#include "ESMCI_PolyDer.h"
#include "ESMCI_Mat.h"
#include "ESMCI_Solver.h"

namespace ESMCI{
  namespace MapperUtil{
    template<typename T>
    class LoadBalancer{
      public:
        LoadBalancer(int ncomps);
        LoadBalancer(int ncomps, const std::vector<T>& serial_exec_times, const std::vector<int> &npets);
        void set_lb_info(const std::vector<T> &serial_exec_times, const std::vector<int> &npets);
        std::vector<int> optimize(void );
      private:
        int ncomps_;
        std::vector<T> serial_exec_times_;
        std::vector<std::vector<T> > past_exec_times_;
        std::vector<int> npets_;
        std::vector<std::vector<int> > past_npets_;
    }; //class LoadBalancer

    template<typename T>
    inline LoadBalancer<T>::LoadBalancer(int ncomps):ncomps_(ncomps)
    {}

    template<typename T>
    inline LoadBalancer<T>::LoadBalancer(int ncomps, const std::vector<T>& serial_exec_times, const std::vector<int> &npets):ncomps_(ncomps), serial_exec_times_(serial_exec_times), npets_(npets)
    {}

    template<typename T>
    inline void LoadBalancer<T>::set_lb_info(const std::vector<T> &serial_exec_times, const std::vector<int> &npets)
    {
      serial_exec_times_ = serial_exec_times;
      npets_ = npets;
    }

    template<typename T>
    inline std::vector<int> LoadBalancer<T>::optimize(void )
    {
      past_exec_times_.push_back(serial_exec_times_);
      past_npets_.push_back(npets_);

      int total_pets = std::accumulate(npets_.cbegin(),
                        npets_.cend(), 0);

      std::vector<int> res(npets_.size(), total_pets);

      T total_time = std::accumulate(serial_exec_times_.cbegin(),
                      serial_exec_times_.cend(), 0);

      if(past_exec_times_.size() < 3){
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
        assert(0);
      }

      return res;
    }

  } // MapperUtil
} //namespace ESMCI
#endif // ESMCI_LoadBalancer_H
