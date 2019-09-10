#ifndef ESMCI_CompInfoUtils_H
#define ESMCI_CompInfoUtils_H

#include <string>
#include <vector>
#include <map>
#include <utility>
#include <algorithm>
#include <iostream>
#include <cassert>

#include "ESMCI_PolyUV.h"
#include "ESMCI_PolyFit.h"
#include "ESMCI_LFit.h"
#include "ESMCI_CompInfo.h"
#include "ESMCI_Macros.h"

namespace ESMCI{
  namespace MapperUtil{

    /* Class to store information about component phases
     *
     * The user can get an instance of this class and use it
     * to store information about component phases. There is
     * only one instance of this class, and the information
     * stored can be retrieved even after the associated
     * component info object is destroyed.
     *
     * The user can retrieve the stored number of PETs and
     * timing info associated with any component. The user
     * can also get a scaling function associated with the
     * component (phase), if available.
     */
    template<typename T>
    class CompInfoStore{
      public:
          /* Get the component info store object */
          static CompInfoStore *get_instance(void );
          /* Add information about component into the store */
          void add_comp_info(const CompInfo<T> &comp_info);
          /* Retrieve information stored in the store */
          std::vector<std::pair<T, T> > get_past_time_intervals(
            const CompInfo<T> &comp_info) const;
          std::vector<std::pair<int, int> > get_past_pet_ranges(
            const CompInfo<T> &comp_info) const;
          std::vector<int> get_past_npets(const CompInfo<T> &comp_info) const;
          std::vector<T> get_past_wtimes(const CompInfo<T> &comp_info) const;
          std::vector<T> get_past_stimes(const CompInfo<T> &comp_info) const;
          /* Get a scaling function, if available, for this component phase */
          bool get_scaling_function(const CompInfo<T> &comp_info, UVIDPoly<T> &f) const;
          /* Reset (delete all) information in the store */
          static void reset(void );
          /* Finalize/delete the store */
          static void finalize(void );
      private:
          /* Internal class to backup information about component phases */
          class CompBackupInfo{
            public:
              CompBackupInfo(const std::string &comp_name,
                              const std::string &comp_phase_name,
                              int comp_id);
              void add_info(const CompInfo<T> &comp_info);
              std::vector<std::pair<T, T> > get_past_time_intervals(void ) const;
              std::vector<std::pair<int, int> > get_past_pet_ranges(void ) const;
              std::vector<int> get_past_npets(void ) const;
              std::vector<T> get_past_wtimes(void ) const;
              std::vector<T> get_past_stimes(void ) const;
              void get_past_times(std::vector<T> &past_wtimes,
                std::vector<T> &past_stimes) const;
              bool get_scaling_function(UVIDPoly<T> &f) const;
            private:
              std::string comp_name_;
              std::string comp_phase_name_;
              int comp_id_;
              std::vector<std::pair<int, int> > past_pet_ranges_;
              std::vector<std::pair<T, T> > past_time_intvls_;
              std::vector<T> past_npets_;
              std::vector<T> past_wtimes_;
              std::vector<T> past_stimes_;
              bool sfunc_is_valid_;
              UVIDPoly<T> sfunc_;
          };
          std::map<std::string, CompBackupInfo> backup_info_;
          int next_comp_id_;
          static CompInfoStore<T> *store_instance_;
          /* Make sure that users cannot create instances of this store */
          CompInfoStore();
    };

    /* Component info store instance */
    template<typename T>
    CompInfoStore<T> *CompInfoStore<T>::store_instance_ = NULL;

    /* Get the component info store instance */
    template<typename T>
    CompInfoStore<T> *CompInfoStore<T>::get_instance(void )
    {
      if(store_instance_ == NULL){
        store_instance_ = new CompInfoStore<T>();
      }
      return store_instance_;
    }

    /* Add component info */
    template<typename T>
    void CompInfoStore<T>::add_comp_info(const CompInfo<T> &comp_info)
    {
      std::string backup_info_key = comp_info.get_comp_name() +
                                    comp_info.get_comp_phase_name();
      typename std::map<std::string, CompBackupInfo>::iterator iter =
        backup_info_.find(backup_info_key);
      if(iter != backup_info_.end()){
        iter->second.add_info(comp_info);
      }
      else{
        CompBackupInfo binfo(comp_info.get_comp_name(),
                              comp_info.get_comp_phase_name(),
                              next_comp_id_++);
        binfo.add_info(comp_info);
        backup_info_.insert(std::pair<std::string, CompBackupInfo>(
                    backup_info_key, binfo));
      }
    }

    /* Get the stored time intervals for a component phase */
    template<typename T>
    std::vector<std::pair<T, T> > CompInfoStore<T>::get_past_time_intervals(
            const CompInfo<T> &comp_info) const
    {
      std::string backup_info_key = comp_info.get_comp_name() +
                                    comp_info.get_comp_phase_name();
      typename std::map<std::string, CompBackupInfo>::const_iterator iter =
        backup_info_.find(backup_info_key);
      if(iter != backup_info_.end()){
        return iter->second.get_past_time_intervals();
      }
      else{
        std::vector<std::pair<T, T> > tmp;
        return tmp;
      }
    }

    /* Get store pet ranges for a component phase */
    template<typename T>
    std::vector<std::pair<int, int> > CompInfoStore<T>::get_past_pet_ranges(
            const CompInfo<T> &comp_info) const
    {
      std::string backup_info_key = comp_info.get_comp_name() +
                                    comp_info.get_comp_phase_name();
      typename std::map<std::string, CompBackupInfo>::const_iterator iter =
        backup_info_.find(backup_info_key);
      if(iter != backup_info_.end()){
        return iter->second.get_past_pet_ranges();
      }
      else{
        std::vector<std::pair<int, int> > tmp;
        return tmp;
      }
    }

    /* Get store number of PETs for a component phase */
    template<typename T>
    std::vector<int> CompInfoStore<T>::get_past_npets(
      const CompInfo<T> &comp_info) const
    {
      std::string backup_info_key = comp_info.get_comp_name() +
                                    comp_info.get_comp_phase_name();
      typename std::map<std::string, CompBackupInfo>::const_iterator iter =
        backup_info_.find(backup_info_key);
      if(iter != backup_info_.end()){
        return iter->second.get_past_npets();
      }
      else{
        std::vector<int> tmp;
        return tmp;
      }
    }

    /* Get stored wallclock times for a component phase */
    template<typename T>
    std::vector<T> CompInfoStore<T>::get_past_wtimes(
      const CompInfo<T> &comp_info) const
    {
      std::string backup_info_key = comp_info.get_comp_name() +
                                    comp_info.get_comp_phase_name();
      typename std::map<std::string, CompBackupInfo>::const_iterator iter =
        backup_info_.find(backup_info_key);
      if(iter != backup_info_.end()){
        return iter->second.get_past_wtimes();
      }
      else{
        std::vector<T> tmp;
        return tmp;
      }
    }

    /* Get stored start times for a component phase */
    template<typename T>
    std::vector<T> CompInfoStore<T>::get_past_stimes(
      const CompInfo<T> &comp_info) const
    {
      std::string backup_info_key = comp_info.get_comp_name() +
                                    comp_info.get_comp_phase_name();
      typename std::map<std::string, CompBackupInfo>::const_iterator iter =
        backup_info_.find(backup_info_key);
      if(iter != backup_info_.end()){
        return iter->second.get_past_stimes();
      }
      else{
        std::vector<T> tmp;
        return tmp;
      }
    }

    /* Get the scaling function (based on store number of PETs and
     * wallclock times) for a component phase
     */
    template<typename T>
    bool CompInfoStore<T>::get_scaling_function(
      const CompInfo<T> &comp_info, UVIDPoly<T> &f) const
    {
      std::string backup_info_key = comp_info.get_comp_name() +
                                    comp_info.get_comp_phase_name();
      typename std::map<std::string, CompBackupInfo>::const_iterator iter =
        backup_info_.find(backup_info_key);
      if(iter != backup_info_.end()){
        return iter->second.get_scaling_function(f);
      }
      else{
        return false;
      }
    }

    template<typename T>
    CompInfoStore<T>::CompInfoStore():next_comp_id_(0)
    {
    }

    /* Reset a component info store. This function deletes all stored info.*/
    template<typename T>
    void CompInfoStore<T>::reset(void )
    {
      finalize();
    }

    /* Finalize a component info store. This function deletes the store
     * instance
     */
    template<typename T>
    void CompInfoStore<T>::finalize(void )
    {
      if(store_instance_){
        delete(store_instance_);
        store_instance_ = NULL;
      }
    }

    /* CompBackupInfo class functions */
    template<typename T>
    CompInfoStore<T>::CompBackupInfo::CompBackupInfo(const std::string &comp_name,
                              const std::string &comp_phase_name,
                              int comp_id):
                                comp_name_(comp_name),
                                comp_phase_name_(comp_phase_name),
                                comp_id_(comp_id),
                                sfunc_is_valid_(false)
    {
    }

    template<typename T>
    void CompInfoStore<T>::CompBackupInfo::add_info(const CompInfo<T> &comp_info)
    {
      assert(comp_name_ == comp_info.get_comp_name());
      assert(comp_phase_name_ == comp_info.get_comp_phase_name());
      std::pair<int, int> pet_range = comp_info.get_pet_range();
      std::pair<T, T> time_intvl = comp_info.get_time_interval();
      T wtime = time_intvl.second - time_intvl.first;
      T stime = comp_info.get_stime();

      past_pet_ranges_.push_back(pet_range);
      past_time_intvls_.push_back(time_intvl);
      past_npets_.push_back(pet_range.second - pet_range.first + 1);
      past_wtimes_.push_back(wtime);
      past_stimes_.push_back(stime);

      /* The scaling function is a 2nd degree polynomial */
      const int MAX_DEG = 2;
      const int MIN_VALS_REQD_FOR_POLYFIT = 3;
      const int MIN_VALS_REQD_FOR_LFIT = 2;
      if(past_npets_.size() >= MIN_VALS_REQD_FOR_POLYFIT){
        int ret = PolyFit(POLY_FIT_LS_LAPACK, MAX_DEG, past_npets_, past_wtimes_, sfunc_);
        if(ret != ESMF_SUCCESS){
          std::cerr << "Error : finding poly fit for component " << comp_name_.c_str()
                    << "\n";
        }
        else{
          sfunc_is_valid_ = true;
        }
      }
      else if(past_npets_.size() >= MIN_VALS_REQD_FOR_LFIT){
        /* Linear fit is disabled for now */
        /*
        int ret = LinearFit(past_npets_, past_wtimes_, sfunc_);
        if(ret != ESMF_SUCCESS){
          std::cerr << "Error : Finding linear fit for component " << comp_name_.c_str()
                    << "\n";
        }
        else{
          sfunc_is_valid_ = true;
        }
        */
      }

      if(sfunc_is_valid_){
        /* Create a unique variable name for each component phase, using comp id */
        const std::string VNAME_PREFIX("x");
        std::ostringstream ostr;
        ostr << VNAME_PREFIX.c_str() << comp_id_;

        std::vector<std::string> vnames(1, ostr.str());
        sfunc_.set_vnames(vnames);
      }
    }

    template<typename T>
    std::vector<std::pair<T, T> > CompInfoStore<T>::CompBackupInfo::get_past_time_intervals(void ) const
    {
      return past_time_intvls_;
    }

    template<typename T>
    std::vector<std::pair<int, int> > CompInfoStore<T>::CompBackupInfo::get_past_pet_ranges(void ) const
    {
      return past_pet_ranges_;
    }

    template<typename T>
    std::vector<int> CompInfoStore<T>::CompBackupInfo::get_past_npets(void ) const
    {
      return past_npets_;
    }

    template<typename T>
    std::vector<T> CompInfoStore<T>::CompBackupInfo::get_past_wtimes(void ) const
    {
      return past_wtimes_;
    }

    template<typename T>
    std::vector<T> CompInfoStore<T>::CompBackupInfo::get_past_stimes(void ) const
    {
      return past_stimes_;
    }

    template<typename T>
    void CompInfoStore<T>::CompBackupInfo::get_past_times(std::vector<T> &past_wtimes,
                std::vector<T> &past_stimes) const
    {
      past_wtimes = past_wtimes_;
      past_stimes = past_stimes_;
    }

    template<typename T>
    bool CompInfoStore<T>::CompBackupInfo::get_scaling_function(UVIDPoly<T> &f) const
    {
      if(sfunc_is_valid_){
        f = sfunc_;
      }
      return sfunc_is_valid_;
    }
  } // namespace MapperUtil
} // namespace ESMCI

#endif // ESMCI_CompInfoUtils_H
