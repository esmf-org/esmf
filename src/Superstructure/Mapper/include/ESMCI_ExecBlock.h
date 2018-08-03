#ifndef ESMCI_ExecBlock_H
#define ESMCI_ExecBlock_H

#include <string>
#include <vector>
#include <utility>
#include <algorithm>
#include <iostream>
#include <initializer_list>
#include <cassert>
#include <cmath>
#include "ESMCI_CompInfo.h"
#include "ESMCI_CompInfoUtils.h"
#include "ESMCI_PolyUV.h"
#include "ESMCI_PolyFit.h"
#include "ESMC_Macros.h"

namespace ESMCI{
  namespace MapperUtil{

    template<typename T>
    class ExecBlock{
      public:
        ExecBlock();
        ExecBlock(const std::vector<CompInfo<T> > &comp_phases);
        void add_comp_phase(const CompInfo<T> &comp_info);
        void rem_comp_phase(const CompInfo<T> &comp_info);
        void rem_comp_phase(const std::string &comp_phase_name);
        enum PetCmpInfo{
          PET_INR_LE,
          PET_INR_GT,
          SPET_INR,
          EPET_INR,
          PET_NIR
        };
        PetCmpInfo cmp_pet_range(const CompInfo<T> &comp_info) const;
        PetCmpInfo cmp_pet_range(const ExecBlock<T> &exec_block) const;
        bool get_scaling_function(UVIDPoly<T> &f);
        template<typename U>
        friend std::ostream &operator<<(std::ostream &ostr, const ExecBlock<U> &exec_block);
      private:
        std::vector<CompInfo<T> > comp_phases_;
        std::pair<int, int> pet_range_;
        std::pair<T, T> time_intvl_;
        bool sfunc_is_valid_;
        UVIDPoly<T> sfunc_;
        int exec_id_;
    };

    template<typename T>
    std::ostream &operator<<(std::ostream &ostr, const ExecBlock<T> &exec_block)
    {
      ostr << "[";
      for(typename std::vector<CompInfo<T> >::const_iterator
          citer = exec_block.comp_phases_.cbegin();
          citer != exec_block.comp_phases_.cend(); ++citer){
        ostr << *citer << ", ";
      }
      ostr << "]\n";
      return ostr;
    }

    template<typename T>
    inline ExecBlock<T>::ExecBlock():sfunc_is_valid_(false), exec_id_(0)
    {
    }

    template<typename T>
    inline ExecBlock<T>::ExecBlock(const std::vector<CompInfo<T> > &comp_phases):
            comp_phases_(comp_phases)
    {
      for(typename std::vector<CompInfo<T> >::const_iterator citer =
              comp_phases_.cbegin();
            citer != comp_phases_.cend(); ++citer){
        std::pair<int, int> comp_pet_range = citer->get_pet_range();
        std::pair<T, T> comp_time_intvl = citer->get_time_interval();

        pet_range_.first = std::min(pet_range_.first, comp_pet_range.first);
        pet_range_.second = std::max(pet_range_.second, comp_pet_range.second);

        time_intvl_.first = std::min(time_intvl_.first, comp_time_intvl.first);
        time_intvl_.second = std::max(time_intvl_.second, comp_time_intvl.second);   
      }
    }

    template<typename T>
    void ExecBlock<T>::add_comp_phase(const CompInfo<T> &comp_info)
    {
      // FIXME: Check for dups
      comp_phases_.push_back(comp_info);
      std::pair<int, int> comp_pet_range = comp_info.get_pet_range();
      std::pair<T, T> comp_time_intvl = comp_info.get_time_interval();

      pet_range_.first = std::min(pet_range_.first, comp_pet_range.first);
      pet_range_.second = std::max(pet_range_.second, comp_pet_range.second);

      time_intvl_.first = std::min(time_intvl_.first, comp_time_intvl.first);
      time_intvl_.second = std::max(time_intvl_.second, comp_time_intvl.second);   
    }

    template<typename T>
    void ExecBlock<T>::rem_comp_phase(const CompInfo<T> &comp_info)
    {
      rem_comp_phase(comp_info.get_name());
    }

    template<typename T>
    void ExecBlock<T>::rem_comp_phase(const std::string &comp_phase_name)
    {
      typename std::vector<CompInfo<T> >::iterator iter = comp_phases_.begin();
      for(;iter != comp_phases_.end(); ++iter){
        if(iter->get_name() == comp_phase_name){
          break;
        }
      }
      if(iter != comp_phases_.end()){
        comp_phases_.erase(iter);
      }

      if(!comp_phases_.empty()){
        CompInfoCmpBySPet<T> cmp_spet;
        pet_range_.first = (std::min_element(comp_phases_.begin(),
                            comp_phases_.end(), cmp_spet))->first;
        CompInfoCmpByEPet<T> cmp_epet;
        pet_range_.second = (std::max_element(comp_phases_.begin(),
                              comp_phases_.end(), cmp_epet))->second;

        CompInfoCmpBySTime<T> cmp_stime;
        time_intvl_.first = (std::min_element(comp_phases_.begin(),
                              comp_phases_.end(), cmp_stime))->first;
        CompInfoCmpByETime<T> cmp_etime;
        time_intvl_.second = (std::max_element(comp_phases_.begin(),
                              comp_phases_.end(), cmp_etime))->second;
      }
      else{
        pet_range_ = std::pair<int, int>(0, 0);
        time_intvl_ = std::pair<T, T>(0, 0);
      }
    }
    
    template<typename T>
    typename ExecBlock<T>::PetCmpInfo ExecBlock<T>::cmp_pet_range(const CompInfo<T> &comp_info) const
    {
      std::pair<int, int> comp_pet_range = comp_info.get_pet_range();
      if((pet_range_.first <= comp_pet_range.first) &&
          (pet_range_.second >= comp_pet_range.second)){
        return PET_INR_LE;
      }
      else if((pet_range_.first >= comp_pet_range.first) &&
          (pet_range_.second <= comp_pet_range.second)){
        return PET_INR_GT;
      }
      else if((pet_range_.first <= comp_pet_range.first) &&
          (pet_range_.second >= comp_pet_range.first)){
        return SPET_INR;
      }
      else if((pet_range_.first >= comp_pet_range.first) &&
          (pet_range_.first <= comp_pet_range.second)){
        return EPET_INR;
      }
      else{
        return PET_NIR;
      }
    }
    
    template<typename T>
    typename ExecBlock<T>::PetCmpInfo ExecBlock<T>::cmp_pet_range(const ExecBlock<T> &eb) const
    {
      if((pet_range_.first <= eb.pet_range_.first) &&
          (pet_range_.second >= eb.pet_range_.second)){
        return PET_INR_LE;
      }
      else if((pet_range_.first >= eb.pet_range_.first) &&
          (pet_range_.second <= eb.pet_range_.second)){
        return PET_INR_GT;
      }
      else if((pet_range_.first <= eb.pet_range_.first) &&
          (pet_range_.second >= eb.pet_range_.first)){
        return SPET_INR;
      }
      else if((pet_range_.first >= eb.pet_range_.first) &&
          (pet_range_.first <= eb.pet_range_.second)){
        return EPET_INR;
      }
      else{
        return PET_NIR;
      }
    }

    template<typename T>
    bool ExecBlock<T>::get_scaling_function(UVIDPoly<T> &f)
    {
      if(sfunc_is_valid_){
        f = sfunc_;
      }
      else{
        CompInfoStore<T> *cinfo_store = CompInfoStore<T>::get_instance();
        assert(cinfo_store);

        std::vector<std::vector<std::pair<int, int> > > comp_past_pet_ranges;
        std::vector<std::vector<std::pair<T, T> > > comp_past_time_intvls;
        int npast_comp_times = 0;
        /* Get the past wtime and npets for each component phase
         * - each row is past info for component i
         */
        for(typename std::vector<CompInfo<T> >::const_iterator citer =
              comp_phases_.cbegin();
            citer != comp_phases_.cend(); ++citer){
          std::vector<std::pair<int, int> > past_pet_ranges =
            cinfo_store->get_past_pet_ranges(*citer);
          comp_past_pet_ranges.push_back(past_pet_ranges);
          comp_past_time_intvls.push_back(cinfo_store->get_past_time_intervals(*citer));
          if(npast_comp_times == 0){
            npast_comp_times = past_pet_ranges.size();
          }
          else{
            npast_comp_times = std::min(npast_comp_times,
                                static_cast<int>(past_pet_ranges.size()));
          }
        }

        /* Reorder all past wtime and npets by execution
         * - each row is past info for an execution 
         */
        std::vector<std::vector<std::pair<int, int> > > comp_past_pet_ranges_by_exec;
        std::vector<std::vector<std::pair<T, T> > > comp_past_time_intvls_by_exec;
        for(int i=0; i<npast_comp_times; i++){
          std::vector<std::pair<int, int> > comp_past_pet_ranges_by_exec_row;
          for(int j=0; j< static_cast<int>(comp_past_pet_ranges.size()); j++){
            comp_past_pet_ranges_by_exec_row.push_back(comp_past_pet_ranges[j][i]);
          }
          comp_past_pet_ranges_by_exec.push_back(comp_past_pet_ranges_by_exec_row);
          std::vector<std::pair<T, T> > comp_past_time_intvls_by_exec_row;
          for(int j=0; j< static_cast<int>(comp_past_time_intvls.size()); j++){
            comp_past_time_intvls_by_exec_row.push_back(comp_past_time_intvls[j][i]);
          }
          comp_past_time_intvls_by_exec.push_back(comp_past_time_intvls_by_exec_row);
        }

        std::vector<T> exec_block_past_npets;
        std::vector<T> exec_block_past_wtimes;
        /* FIXME: We are ignoring holes in PET ranges for the execution block */
        for(std::vector<std::vector<std::pair<int, int> > >::const_iterator citer =
                comp_past_pet_ranges_by_exec.cbegin();
              citer != comp_past_pet_ranges_by_exec.cend(); ++citer){
          int min_pet = 0;
          int max_pet = 0;
          for(std::vector<std::pair<int, int> >::const_iterator pet_citer =
                  (*citer).cbegin();
                pet_citer != (*citer).cend(); ++pet_citer){
            min_pet = std::min(min_pet, pet_citer->first);
            max_pet = std::max(max_pet, pet_citer->second);
          }
          exec_block_past_npets.push_back(max_pet - min_pet + 1);
        }

        for(typename std::vector<std::vector<std::pair<T, T> > >::const_iterator citer =
                comp_past_time_intvls_by_exec.cbegin();
              citer != comp_past_time_intvls_by_exec.cend(); ++citer){
          T min_time = static_cast<T>(0);
          T max_time = static_cast<T>(0);
          for(typename std::vector<std::pair<T, T> >::const_iterator tciter =
                  (*citer).cbegin();
                tciter != (*citer).cend(); ++tciter){
            min_time = std::min(min_time, tciter->first);
            max_time = std::max(max_time, tciter->second);
          }
          assert(max_time > min_time);
          exec_block_past_wtimes.push_back(max_time - min_time);
        }

        const int MIN_VALS_REQD_FOR_POLYFIT = 3;
        const int MAX_DEG = 2;
        if(npast_comp_times >= MIN_VALS_REQD_FOR_POLYFIT){
          int ret = PolyFit(POLY_FIT_LS_LAPACK, MAX_DEG,
                      exec_block_past_npets, exec_block_past_wtimes, sfunc_);
          if(ret != ESMF_SUCCESS){
            std::cerr << "Error: finding poly fit for exec block\n";
          }
          else{
            sfunc_is_valid_ = true;
          }
        }
      }

      return sfunc_is_valid_;
    }

  } // namespace MapperUtil
} //namespace ESMCI
#endif //ESMCI_ExecBlock_H
