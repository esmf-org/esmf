#ifndef ESMCI_ExecSim_H
#define ESMCI_ExecSim_H

#include <iostream>
#include <tuple>
#include <iterator>
#include <vector>
#include <algorithm>
#include "ESMCI_CompInfo.h"

namespace ESMCI{
  namespace MapperUtil{
    namespace PetRangeTupleUtils{
      enum PetRangeTupleIdx{
        START_PET = 0,
        END_PET = 1,
        COMP_INFO_IDX = 2
      };
      /* Pet range info comparator */
      class PetRangeInfoCmp{
        public:
          bool operator()(const std::tuple<int, int, std::size_t> &a,
                          const std::tuple<int, int, std::size_t> &b){
            if(std::get<START_PET>(a) == std::get<START_PET>(b)){
              return std::get<END_PET>(a) < std::get<END_PET>(b);
            }
            else{
              return std::get<START_PET>(a) < std::get<START_PET>(b);
            }
          }
      };

      /* Find the range of affected tuples, for pet range provided by pet_range
       * svec is assumed to be sorted
       */
      inline void find_affected_range(
            std::vector<std::tuple<int, int, std::size_t> >::iterator svec_begin,
            std::vector<std::tuple<int, int, std::size_t> >::iterator svec_end,
            std::pair<int, int> pet_range,
            std::vector<std::tuple<int, int, std::size_t> >::iterator &affected_item,
            std::vector<std::tuple<int, int, std::size_t> >::iterator &affected_range_begin,
            std::vector<std::tuple<int, int, std::size_t> >::iterator &affected_range_end)
      {
        PetRangeInfoCmp cmp;
        std::tuple<int, int, std::size_t>
          pet_tuple(pet_range.first, pet_range.second, -1);
        affected_item = std::lower_bound(svec_begin, svec_end, pet_tuple, cmp);
        assert(affected_item != svec_end);

        std::vector<std::tuple<int, int, std::size_t> >::reverse_iterator svec_riter_start
          = std::reverse_iterator<std::vector<std::tuple<int, int, std::size_t> >::iterator>(affected_item);
        std::vector<std::tuple<int, int, std::size_t> >::reverse_iterator svec_riter_end
          = std::reverse_iterator<std::vector<std::tuple<int, int, std::size_t> >::iterator>(svec_begin);

        std::vector<std::tuple<int, int, std::size_t> >::reverse_iterator
          affected_range_rbegin = svec_riter_start;
        /* Search elements in front of the item */
        for(std::vector<std::tuple<int, int, std::size_t> >::reverse_iterator
              riter = svec_riter_start;
              riter != svec_riter_end; ++riter, ++affected_range_rbegin){
          if(std::get<START_PET>(*affected_item) > std::get<END_PET>(*riter)){
            break;
          }
        }
        affected_range_begin = affected_range_rbegin.base();
        affected_range_end = affected_item + 1;
        /* Search elements after the item */
        for(std::vector<std::tuple<int, int, std::size_t> >::iterator
              iter = affected_item+1;
              iter != svec_end; ++iter, ++affected_range_end){
          if(std::get<END_PET>(*affected_item) < std::get<START_PET>(*iter)){
            break;
          }
        }
      }

    } // namespace PetRangeTupleUtils
    /*
     * The comp infos have a new set of PET ranges (a new layout), and the user 
     * provides the wallclock time for each component via comp_winfo_wtimes
     * The funcion does a simulated run, and updates the comp info time intervals
     * and returns the wallclock time for the application
     * The components are run in the order the components are listed in comp_infos
     * (i.e., the ordering of components in comp_infos reflects the order in which
     * the components appear in the run sequence, or a suggested more optimal
     * run sequence)
     */
    template<typename T>
    T MapperSimRun(std::vector<CompInfo<T> > &comp_infos,
        const std::vector<T> &comp_info_wtimes)
    {
      std::vector<std::tuple<int, int, std::size_t> > pet_range_infos;
      int comp_info_idx = 0;
      for(typename std::vector<CompInfo<T> >::const_iterator citer = comp_infos.cbegin();
            citer != comp_infos.cend(); ++citer, comp_info_idx++){
        std::pair<int, int> pet_range = (*citer).get_pet_range();
        pet_range_infos.push_back(std::tuple<int, int, std::size_t>(
          pet_range.first, pet_range.second, comp_info_idx));
      }

      PetRangeTupleUtils::PetRangeInfoCmp cmp;
      std::sort(pet_range_infos.begin(), pet_range_infos.end(), cmp);

      T opt_wtime = static_cast<T>(0);
      std::vector<T> end_wtimes(comp_infos.size(), static_cast<T>(-1));
      /* For each component assign the time interval based on "execution" */
      for(typename std::vector<CompInfo<T> >::iterator iter = comp_infos.begin();
            iter != comp_infos.end(); ++iter){
        std::pair<T, T> time_intvl;
        std::pair<int, int> pet_range = (*iter).get_pet_range();
        std::vector<std::tuple<int, int, std::size_t> >::iterator affected_item;
        std::vector<std::tuple<int, int, std::size_t> >::iterator affected_range_begin;
        std::vector<std::tuple<int, int, std::size_t> >::iterator affected_range_end;
        std::vector<T> affected_range_wtimes;
        
        PetRangeTupleUtils::find_affected_range(
                            pet_range_infos.begin(), pet_range_infos.end(),
                            pet_range,
                            affected_item, affected_range_begin, affected_range_end);

        for(std::vector<std::tuple<int, int, std::size_t> >::iterator
              titer = affected_range_begin;
              titer != affected_range_end; ++titer){
          std::size_t cidx = std::get<PetRangeTupleUtils::COMP_INFO_IDX>(*titer);
          assert((cidx >= 0) && (cidx < end_wtimes.size()));
          affected_range_wtimes.push_back(end_wtimes[cidx]);
        }
        
        typename std::vector<T>::iterator
          max_iter = std::max_element(affected_range_wtimes.begin(),
                                        affected_range_wtimes.end());
        assert(max_iter != affected_range_wtimes.end());
        T max_wtime = *max_iter;
        std::size_t cidx = std::get<PetRangeTupleUtils::COMP_INFO_IDX>(*affected_item);
        if(max_wtime > static_cast<T>(0)){
          time_intvl.first = max_wtime + static_cast<T>(1);
        }
        else{
          time_intvl.first = static_cast<T>(0);
        }
        assert((cidx >= 0) && (cidx < comp_info_wtimes.size()) &&
                (cidx < end_wtimes.size()));
        time_intvl.second = time_intvl.first + comp_info_wtimes[cidx];
        (*iter).set_time_interval(time_intvl);

        end_wtimes[cidx] = time_intvl.second;
        opt_wtime = std::max(opt_wtime, end_wtimes[cidx]);
      }

      return opt_wtime;
    }

  }  // namepsape MapperUtil
} //namespace ESMCI

#endif // ESMCI_ExecSim_H
