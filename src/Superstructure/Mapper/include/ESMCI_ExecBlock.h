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
        template<typename U>
        friend std::ostream &operator<<(std::ostream &ostr, const ExecBlock<U> &exec_block);
      private:
        std::vector<CompInfo<T> > comp_phases_;
        std::pair<int, int> pet_range_;
        std::pair<T, T> time_intvl_;
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
    inline ExecBlock<T>::ExecBlock()
    {
    }

    template<typename T>
    inline ExecBlock<T>::ExecBlock(const std::vector<CompInfo<T> > &comp_phases):
            comp_phases_(comp_phases)
    {
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
    

  } // namespace MapperUtil
} //namespace ESMCI
#endif //ESMCI_ExecBlock_H
