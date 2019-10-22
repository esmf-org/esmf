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
#include "ESMCI_LPolyMV.h"
#include "ESMCI_LFit.h"
#include "ESMC_Macros.h"

namespace ESMCI{
  namespace MapperUtil{

    /* The class used to represent an execution block.
     *
     * An execution block consists of multiple component phases
     * that execute on the same range of PETs
     *
     * Users can add/remove component phases, retrieve scaling
     * function for the execution block and compare the
     * execution block with other execution blocks or component
     * infos (The comparision is based on temporal order. For
     * entities that run during the same time interval, the tie
     * is broken using the starting PETs)
     */
    template<typename T>
    class ExecBlock{
      public:
        ExecBlock();
        ExecBlock(const std::vector<CompInfo<T> > &comp_phases);
        /* Add/rem component phases in the execution block */
        void add_comp_phase(const CompInfo<T> &comp_info);
        void rem_comp_phase(const CompInfo<T> &comp_info);
        void rem_comp_phase(const std::string &comp_phase_name);
        /* PET range comparison info
         * PET_INR_LE : The provided component phase/execution block
         *              runs within the PET range of this execution
         *              block
         * PET_INR_GT : This execution block runs within the range
         *              of the provided component phase/execution
         *              block
         * SPET_INR   : The starting pet of the provided component
         *              phase/execution block is inside the PET
         *              range of this execution block
         * EPET_INR   : The ending pet of the provided component
         *              phase/execution block is inside the PET
         *              range of this execution block
         * PET_NIR    : The provided component phase/execution block
         *              is not within the PET range of this
         *              execution block
         */
        enum PetCmpInfo{
          PET_INR_LE,
          PET_INR_GT,
          SPET_INR,
          EPET_INR,
          PET_NIR
        };
        PetCmpInfo cmp_pet_range(const CompInfo<T> &comp_info) const;
        PetCmpInfo cmp_pet_range(const ExecBlock<T> &exec_block) const;
        /* Get PET and time info for this execution block */
        std::pair<int, int> get_pet_range(void ) const;
        int get_npets(void ) const;
        std::pair<T, T> get_time_interval(void ) const;
        T get_wtime(void ) const;
        /* Get scaling function for this execution block */
        bool get_scaling_function(UVIDPoly<T> &f);
        /* Get scaling function for this execution block. Additional
         * constraints on the execution block is also provided by
         * the user via constraint functions
         * The scaling function will include these additional constraints
         */
        bool get_scaling_function(UVIDPoly<T> &f, std::vector<MVIDLPoly<T> >&constraint_funcs);
        bool get_scaling_function(UVIDPoly<T> &f, MVIDLPoly<T> &constraint_func);
        /* Print out the execution block to the provided stream */
        template<typename U>
        friend std::ostream &operator<<(std::ostream &ostr, const ExecBlock<U> &exec_block);
      private:
        int get_next_exec_id(void ) const;
        std::vector<CompInfo<T> > comp_phases_;
        std::pair<int, int> pet_range_;
        std::pair<T, T> time_intvl_;
        bool sfunc_is_valid_;
        /* scaling function for this execution block */
        UVIDPoly<T> sfunc_;
        /* Constraint functions for this execution block */
        std::vector<MVIDLPoly<T> > sfunc_cfuncs_;
        MVIDLPoly<T> sfunc_cfunc_;
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
      exec_id_ = get_next_exec_id();
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

      if(comp_phases_.size() > 1){
        pet_range_.first = std::min(pet_range_.first, comp_pet_range.first);
        pet_range_.second = std::max(pet_range_.second, comp_pet_range.second);

        time_intvl_.first = std::min(time_intvl_.first, comp_time_intvl.first);
        time_intvl_.second = std::max(time_intvl_.second, comp_time_intvl.second);
      }
      else{
        // comp_phases_.size() == 1, first comp phase added to the exec block
        pet_range_ = comp_pet_range;
        time_intvl_ = comp_time_intvl;
      }
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
    std::pair<int, int> ExecBlock<T>::get_pet_range(void ) const
    {
      return pet_range_;
    }

    template<typename T>
    int ExecBlock<T>::get_npets(void ) const
    {
      return pet_range_.second - pet_range_.first + 1;
    }

    template<typename T>
    std::pair<T, T> ExecBlock<T>::get_time_interval(void ) const
    {
      return time_intvl_;
    }

    template<typename T>
    T ExecBlock<T>::get_wtime(void ) const
    {
      return time_intvl_.second - time_intvl_.first;
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
        std::vector<std::pair<std::string, int> > comp_phase_npets;
        for(typename std::vector<CompInfo<T> >::const_iterator citer =
              comp_phases_.cbegin();
            citer != comp_phases_.cend(); ++citer){
          /* Get current number of pets */
          int cur_npets = (*citer).get_npets();
          UVIDPoly<T> comp_phase_sfunc;
          bool ret = cinfo_store->get_scaling_function(*citer, comp_phase_sfunc);
          if(!ret){
            return false;
          }
          std::vector<std::string> comp_phase_sfunc_vnames = 
            comp_phase_sfunc.get_vnames();
          assert(comp_phase_sfunc_vnames.size() == 1);
          comp_phase_npets.push_back(
            std::pair<std::string, int>(comp_phase_sfunc_vnames[0], cur_npets));
          
          /* Get past (including current) comp phase info */
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
        const int MIN_VALS_REQD_FOR_LFIT = 2;
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
        else if(npast_comp_times == MIN_VALS_REQD_FOR_LFIT){
          int ret = LinearFit(exec_block_past_npets, exec_block_past_wtimes, sfunc_);
          if(ret != ESMF_SUCCESS){
            std::cerr << "Error: Finding linear fit for exec block\n";
          }
          else{
            sfunc_is_valid_ = true;
          }
        }
        if(sfunc_is_valid_){
          /* Set the variable name for the scaling function */
          const std::string ENAME_PREFIX("e");
          std::ostringstream ostr;
          ostr << ENAME_PREFIX.c_str() << exec_id_;

          std::vector<std::string> vnames(1, ostr.str());
          sfunc_.set_vnames(vnames);

          /* Set the constraint functions for the scaling function
           * The constraint functions are the constraints imposed by
           * the individual components (the number of pets) that
           * compose the execution block
           * If Exec block with npets e1 is composed of comp phases with
           * npets x1 and x2
           * e1 - k1 * x1 = 0
           * e1 - k2 * x2 = 0
           */
          for(std::vector<std::pair<std::string, int> >::const_iterator citer = 
                comp_phase_npets.cbegin(); citer != comp_phase_npets.cend(); ++citer){
            int eblock_npets = pet_range_.second - pet_range_.first + 1;
            T ratio = static_cast<T>(eblock_npets)/static_cast<T>((*citer).second);
            std::vector<T> sfunc_cfunc_coeffs;
            sfunc_cfunc_coeffs.push_back(1);
            sfunc_cfunc_coeffs.push_back(static_cast<T>(-1) * ratio);
            sfunc_cfunc_coeffs.push_back(0);
            std::vector<std::string> sfunc_cfunc_vnames;
            sfunc_cfunc_vnames.push_back(vnames[0]);
            sfunc_cfunc_vnames.push_back((*citer).first);

            MVIDLPoly<T> sfunc_cfunc(sfunc_cfunc_coeffs);
            sfunc_cfunc.set_vnames(sfunc_cfunc_vnames);
            sfunc_cfuncs_.push_back(sfunc_cfunc);

            /* Cumulative constraint function - provides dependency between
             * e1 = Sum(kixi)
             */
            std::vector<T> sfunc_ccfunc_coeffs;
            sfunc_ccfunc_coeffs.push_back(ratio/comp_phase_npets.size());
            sfunc_ccfunc_coeffs.push_back(0);
            std::vector<std::string> sfunc_ccfunc_vnames;
            sfunc_ccfunc_vnames.push_back((*citer).first);
            MVIDLPoly<T> sfunc_ccfunc(sfunc_ccfunc_coeffs);
            sfunc_ccfunc.set_vnames(sfunc_ccfunc_vnames);
            sfunc_cfunc_ = sfunc_cfunc_ + sfunc_ccfunc;
          }
        }
      }

      return sfunc_is_valid_;
    }


    template<typename T>
    bool ExecBlock<T>::get_scaling_function(UVIDPoly<T> &f,
          std::vector<MVIDLPoly<T> > &constraint_funcs)
    {
      if(!sfunc_is_valid_){
        bool ret = get_scaling_function(f);
        if(!ret){
          return ret;
        }
      }

      f = sfunc_;
      constraint_funcs = sfunc_cfuncs_;

      return true;
    }

    template<typename T>
    bool ExecBlock<T>::get_scaling_function(UVIDPoly<T> &f,
          MVIDLPoly<T> &constraint_func)
    {
      if(!sfunc_is_valid_){
        bool ret = get_scaling_function(f);
        if(!ret){
          return ret;
        }
      }

      f = sfunc_;
      constraint_func = sfunc_cfunc_;

      return true;
    }

    template<typename T>
    int ExecBlock<T>::get_next_exec_id(void ) const
    {
      static int next_exec_id = 0;
      return next_exec_id++;
    }

  } // namespace MapperUtil
} //namespace ESMCI
#endif //ESMCI_ExecBlock_H
