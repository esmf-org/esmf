#ifndef ESMCI_LoadBalancer_H
#define ESMCI_LoadBalancer_H

#include <string>
#include <queue>
#include <vector>
#include <set>
#include <algorithm>
#include <functional>
#include <iostream>
#include <sstream>
#include "ESMCI_Poly.h"
#include "ESMCI_PolyUV.h"
#include "ESMCI_PolyTwoV.h"
#include "ESMCI_PolyTwoDV.h"
#include "ESMCI_PolyDer.h"
#include "ESMCI_PolyFit.h"
#include "ESMCI_Mat.h"
#include "ESMCI_Solver.h"
#include "ESMCI_CompInfo.h"
#include "ESMCI_CompInfoUtils.h"
#include "ESMCI_ExecBlock.h"
#include "ESMCI_ExecBlockUtils.h"

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
        typedef enum{
          LBAL_ALG_MIN_IDLE_TIME = 1
        } LoadBalancerAlg;
        LoadBalancer();
        LoadBalancer(const std::vector<CompInfo<T> > &comp_infos);
        LoadBalancerAlg set_opt_method(const LoadBalancerAlg &opt_alg);
        void set_lb_info(const std::vector<CompInfo<T> > &comp_infos);
        bool optimize(std::vector<int> &opt_npets,
                    std::vector<std::pair<int, int> > &opt_pet_ranges,
                    T &opt_wtime);
        bool get_optimal(std::vector<int> &opt_npets,
                    std::vector<std::pair<int, int> > &opt_pet_ranges,
                    T &opt_wtime);
      private:
        /* Load Balancer backup info
         * This is the information stored in the LoadBalancer corresponding
         * to comp_infos_
         */
        class LoadBalancerBackupInfo{
          public:
            LoadBalancerBackupInfo();
            LoadBalancerBackupInfo(
              const std::vector<int> &opt_npets,
              const std::vector<std::pair<int, int> > &opt_pet_ranges,
              T opt_wtime);
            void get_info(std::vector<int> &opt_npets,
                  std::vector<std::pair<int, int> > &opt_pet_ranges,
                  T &opt_wtime) const;
            void set_info(const std::vector<int> &opt_npets,
                  const std::vector<std::pair<int, int> > &opt_pet_ranges,
                  T opt_wtime);
            bool operator<(const LoadBalancerBackupInfo &other_info) const;
            bool operator==(const LoadBalancerBackupInfo &other_info) const;
            bool is_valid(void ) const;
          private:
            bool is_valid_;
            std::vector<int> opt_npets_;
            std::vector<std::pair<int, int> > opt_pet_ranges_;
            T opt_wtime_;
            T opt_wtime_pred_err_;
        }; // class LoadBalancerBackupInfo

        std::vector<CompInfo<T> > comp_infos_;
        LoadBalancerAlg opt_alg_;
        std::vector<LoadBalancerBackupInfo> backup_infos_;
        LoadBalancerBackupInfo optimal_info_;
        void update_backup_info(const std::vector<CompInfo<T> > &comp_infos);
        bool get_constraint_funcs(
          std::vector<std::vector<ExecBlock<T> > > &pexec_blocks,
          std::vector<TwoDVIDPoly<T> > &twodvidp_cfuncs,
          std::vector<MVIDLPoly<T> > &mvidlp_cfuncs,
          std::vector<MVIDLPoly<T> > &reshape_mvidlp_cfuncs,
          std::vector<std::string> &cfuncs_vnames,
          std::vector<int> &cfuncs_vivals);
    }; //class LoadBalancer

    template<typename T>
    inline LoadBalancer<T>::LoadBalancer():opt_alg_(LBAL_ALG_MIN_IDLE_TIME)
    {
    }

    template<typename T>
    inline LoadBalancer<T>::LoadBalancer(
            const std::vector<CompInfo<T> > &comp_infos):
              comp_infos_(comp_infos),
              opt_alg_(LBAL_ALG_MIN_IDLE_TIME)
    {
    }

    template<typename T>
    inline typename LoadBalancer<T>::LoadBalancerAlg LoadBalancer<T>::set_opt_method(
      const typename LoadBalancer<T>::LoadBalancerAlg &opt_alg)
    {
      LoadBalancerAlg old_opt_alg = opt_alg_;
      opt_alg_ = opt_alg;
      return old_opt_alg;
    }

    /* Set execution info for the load balancer
     */
    template<typename T>
    inline void LoadBalancer<T>::set_lb_info(const std::vector<CompInfo<T> > &comp_infos)
    {
      assert(comp_infos_.empty() || (comp_infos_.size() == comp_infos.size()));
      comp_infos_ = comp_infos;
      update_backup_info(comp_infos);
    }

    template<typename T>
    bool LoadBalancer<T>::get_constraint_funcs(
      std::vector<std::vector<ExecBlock<T> > > &pexec_blocks,
      std::vector<TwoDVIDPoly<T> > &twodvidp_cfuncs,
      std::vector<MVIDLPoly<T> > &mvidlp_cfuncs,
      std::vector<MVIDLPoly<T> > &reshape_mvidlp_cfuncs,
      std::vector<std::string> &cfuncs_vnames,
      std::vector<int> &cfuncs_vivals)
    {
      if(opt_alg_ == LBAL_ALG_MIN_IDLE_TIME){
        const float DAMP_CONST = 1.0;
        /* Create the list of variable names and ivals, component info
         * variables and values are added first, in the order they are
         * stored in the load balancer. Then variable names and ivals
         * corresponding to the execution block are added
         */
        CompInfoStore<T> *cinfo_store = CompInfoStore<T>::get_instance();
        assert(cinfo_store);
        for(typename std::vector<CompInfo<T> >::const_iterator citer =
              comp_infos_.cbegin();
              citer != comp_infos_.cend(); ++citer){
          UVIDPoly<T> f;
          bool ret = cinfo_store->get_scaling_function(*citer, f);
          if(!ret){
            return ret;
          }

          std::vector<std::string> cinfo_sfunc_vnames = f.get_vnames();
          assert(cinfo_sfunc_vnames.size() == 1);
          cfuncs_vnames.push_back(cinfo_sfunc_vnames[0]);
          cfuncs_vivals.push_back((*citer).get_npets());
        }
        std::vector<T> cfunc_template_coeffs(cfuncs_vnames.size()+1, static_cast<T>(0));
        MVIDLPoly<T> cfunc_template(cfunc_template_coeffs);
        cfunc_template.set_vnames(cfuncs_vnames);
        std::set<TwoDVIDPoly<T> > utwodvidp_cfuncs;
        /* The constraint functions are the square of the idle times between the
         * pairs of execution blocks
         * The idle time between a pair of execution blocks is obtained by 
         * finding the difference between the scaling functions of the two
         * execution blocks
         * The contraint functions also include the relationship between the
         * number of pets (variables) used by the execution blocks
         */
        std::vector<std::pair<std::string, int> > eblock_vnames_and_ivals;
        std::vector<MVIDLPoly<T> > exec_block_list_cfuncs;
        for(typename std::vector<std::vector<ExecBlock<T> > >::iterator iter = 
              pexec_blocks.begin(); iter != pexec_blocks.end(); ++iter){
          /* Each (*citer) is a list of parallel execution blocks */
          assert(!(*iter).empty());
          /* At least two parallel execution blocks are needed for calculating
           * idle time functions
           */
          if((*iter).size() < 2){
            continue;
          }
          int exec_block_list_npets = 0;
          MVIDLPoly<T> exec_block_list_cfunc;
          for(typename std::vector<ExecBlock<T> >::iterator iter_exec_block_list = 
              (*iter).begin(); iter_exec_block_list != (*iter).end();
              ++iter_exec_block_list){
            /* Get the scaling function and constraints for the first exec block */
            UVIDPoly<T> sfunc_first_exec_block;
            std::vector<std::string> sfunc_first_exec_block_vnames;
            MVIDLPoly<T> sfunc_cfunc_first_exec_block;
            bool ret = iter_exec_block_list->get_scaling_function(
                                          sfunc_first_exec_block,
                                          sfunc_cfunc_first_exec_block);
            if(!ret){
              /* Not enough data to get a scaling function */
              return ret;
            }
            exec_block_list_npets += iter_exec_block_list->get_npets();
            exec_block_list_cfunc = exec_block_list_cfunc + sfunc_cfunc_first_exec_block;
            std::cout << "Cumulative cfunc : " << sfunc_cfunc_first_exec_block << " : Number of PETs in cur exec block : " << iter_exec_block_list->get_npets() << "\n";
            sfunc_first_exec_block_vnames = sfunc_first_exec_block.get_vnames();
            assert(sfunc_first_exec_block_vnames.size() == 1);

            if(std::distance(iter_exec_block_list, (*iter).end()) < 2){
              /* No more pairs of execution blocks available in the list */
              break;
            }
            /* For all possible combinations of execution block pairs find the
             * idle time square -> one set of constraint functions
             */
            for(typename std::vector<ExecBlock<T> >::iterator next_iter = 
              iter_exec_block_list + 1; next_iter != (*iter).end(); ++next_iter){
              UVIDPoly<T> sfunc_iexec_block;
              std::vector<std::string> sfunc_iexec_block_vnames;
              MVIDLPoly<T> sfunc_cfunc_iexec_block;
              bool ret = next_iter->get_scaling_function(
                                            sfunc_iexec_block,
                                            sfunc_cfunc_iexec_block);
              if(!ret){
                /* Not enough data to get a scaling function */
                return ret;
              }
              sfunc_iexec_block_vnames = sfunc_iexec_block.get_vnames();
              assert(sfunc_iexec_block_vnames.size() == 1);

              /* Find idle time function */
              std::vector<std::string> twodvid_sfunc_vnames;
              twodvid_sfunc_vnames.push_back(sfunc_first_exec_block_vnames[0]);
              twodvid_sfunc_vnames.push_back(sfunc_iexec_block_vnames[0]);

              TwoDVIDPoly<T> twodvid_sfunc_first_exec_block(sfunc_first_exec_block,
                              twodvid_sfunc_vnames);
              TwoDVIDPoly<T> twodvid_sfunc_iexec_block(sfunc_iexec_block,
                              twodvid_sfunc_vnames);
              TwoDVIDPoly<T> idle_time_func = twodvid_sfunc_iexec_block -
                                              twodvid_sfunc_first_exec_block;
              MVIDLPoly<T> idle_time_dfunc_first_exec_block =
                cfunc_template + sfunc_cfunc_first_exec_block;
              MVIDLPoly<T> idle_time_dfunc_iexec_block =
                cfunc_template + sfunc_cfunc_iexec_block;
              std::vector<MVIDLPoly<T> > idle_time_dfuncs;
              idle_time_dfuncs.push_back(idle_time_dfunc_first_exec_block);
              idle_time_dfuncs.push_back(idle_time_dfunc_iexec_block);

              /* Add the square of the idle time function as a constraint */
              TwoDVIDPoly<T> idle_time_func_sq = idle_time_func * idle_time_func;
              idle_time_func_sq.set_dfuncs(idle_time_dfuncs);
              utwodvidp_cfuncs.insert(DAMP_CONST * idle_time_func_sq);
            }
          }
          /* Final constraint Sum(ei) - C = 0
           * This is computed by adding up the cumulative dependent
           * functions for each ei
           */
          exec_block_list_cfunc = exec_block_list_cfunc - 
                                    static_cast<T>(exec_block_list_npets);
          //mvidlp_cfuncs.push_back(DAMP_CONST * exec_block_list_cfunc);
          //mvidlp_cfuncs.push_back(exec_block_list_cfunc);
          exec_block_list_cfuncs.push_back(exec_block_list_cfunc);
          reshape_mvidlp_cfuncs.push_back(exec_block_list_cfunc);
        }

        /* Add all exec block constraint functions */
        for(typename std::set<TwoDVIDPoly<T> >::iterator iter = utwodvidp_cfuncs.begin();
              iter != utwodvidp_cfuncs.end(); ++iter){
          twodvidp_cfuncs.push_back(*iter);
        }

        /* Add up all cumulative constraint functions from exec blocks
         * and add as the final constraint function for the solver
         */
        if(!exec_block_list_cfuncs.empty()){
          MVIDLPoly<T> final_constraint = cfunc_template;
          for(typename std::vector<MVIDLPoly<T> >::iterator
                  iter = exec_block_list_cfuncs.begin();
                iter != exec_block_list_cfuncs.end(); ++iter){
            final_constraint = final_constraint + *iter;
          }
          mvidlp_cfuncs.push_back(final_constraint);
        }

        /*
        for(std::vector<std::pair<std::string, int> >::const_iterator citer = 
              eblock_vnames_and_ivals.cbegin();
              citer != eblock_vnames_and_ivals.cend(); ++citer){
          cfuncs_vnames.push_back((*citer).first);
          cfuncs_vivals.push_back((*citer).second);
        }
        */

        return (!twodvidp_cfuncs.empty());
      }
    
      return false;
    }

    /* Optimize the user specified PET list by redistributing
     * PETs between the different components such that it
     * reduces the idle time between the different components
     * Idle time is the difference between the execution times
     * (wallclock times) of two components
     */
    template<typename T>
    inline bool LoadBalancer<T>::optimize(
                  std::vector<int> &opt_npets,
                  std::vector<std::pair<int, int> > &opt_pet_ranges,
                  T &opt_wtime)
    {
      std::vector<TwoDVIDPoly<T> > twodvidp_cfuncs;
      std::vector<MVIDLPoly<T> > mvidlp_cfuncs;
      std::vector<MVIDLPoly<T> > reshape_mvidlp_cfuncs;
      std::vector<std::string> cfuncs_vnames;
      std::vector<int> cfuncs_vivals;

      int ncomps = static_cast<int>(comp_infos_.size());
      std::vector<T> opt_wtimes(ncomps, static_cast<T>(0));
      opt_npets.resize(ncomps);
      opt_npets.assign(ncomps, 0);

      std::vector<int> comp_npets;
      std::vector<T> comp_stimes;
      int total_pets = 0;
      T total_stime = static_cast<T>(0);
      for(typename std::vector<CompInfo<T> >::const_iterator citer = comp_infos_.cbegin();
          citer != comp_infos_.cend(); ++citer){
        int compi_npets = (*citer).get_npets();
        T compi_stime = (*citer).get_stime();
        comp_npets.push_back(compi_npets);
        comp_stimes.push_back(compi_stime);
        total_pets += compi_npets;
        total_stime += compi_stime;
      }

      /* Find the multiple pairs of execution blocks for the component
       * infos
       */
      std::vector<std::vector<ExecBlock<T> > > pexec_blocks;
      bool found_constraint_funcs = false;
      bool found_pexec_blocks = find_parallel_exec_blocks(comp_infos_, pexec_blocks);
      if(found_pexec_blocks && !pexec_blocks.empty()){
        found_constraint_funcs = get_constraint_funcs(
                                        pexec_blocks,
                                        twodvidp_cfuncs,
                                        mvidlp_cfuncs,
                                        reshape_mvidlp_cfuncs,
                                        cfuncs_vnames, cfuncs_vivals);
      }

      if(!found_constraint_funcs){
        for(int i=0; i<ncomps; i++){
          opt_npets[i] = static_cast<int>(total_pets * comp_stimes[i] / total_stime);
          if(opt_npets[i] <= 0){
            return false;
          }
          opt_wtimes[i] = static_cast<T>(comp_stimes[i]/opt_npets[i]);
        }
      }
      else{
        /* FIXME: We should get rid of this conversion */
        std::vector<T> tmp_cfuncs_vivals;
        for(std::vector<int>::const_iterator citer = cfuncs_vivals.cbegin();
            citer != cfuncs_vivals.cend(); ++citer){
          tmp_cfuncs_vivals.push_back(static_cast<T>(*citer));
        }
        /* Use the solver to optimize the number of PETs */
        const int SOLVER_MAX_ITERS = 1000;
        SESolver<T> solver(cfuncs_vnames, tmp_cfuncs_vivals,
                            twodvidp_cfuncs, mvidlp_cfuncs);
        solver.set_reshape_funcs(reshape_mvidlp_cfuncs);
        solver.scale_and_center_funcs(tmp_cfuncs_vivals);
        solver.set_niters(SOLVER_MAX_ITERS);
        typename SESolver<T>::UConstraintValGenerator uc_vgen;
        std::vector<T> new_pets = solver.minimize(uc_vgen);
        for(int i=0; i<ncomps; i++){
          opt_npets[i] = static_cast<int>(new_pets[i]);
          /* The solver is returning us -ve PET values, cannot minimize further */
          if(opt_npets[i] <= 0){
            return false;
          }
          opt_wtimes[i] = static_cast<T>(comp_stimes[i]/opt_npets[i]);
        }
      }

      typename std::vector<T>::iterator opt_wtime_iter = std::max_element(
                                                  opt_wtimes.begin(), opt_wtimes.end());
      if(opt_wtime_iter != opt_wtimes.end()){
        opt_wtime = *opt_wtime_iter;
      }
      else{
        opt_wtime = static_cast<T>(0);
        return false;
      }

      //std::cout << "Saving npets = " << opt_npets[0] << "\n";
      LoadBalancerBackupInfo backup_info(opt_npets, opt_pet_ranges, opt_wtime);
      backup_infos_.push_back(backup_info);

      /* Find and store the minimal backup info */
      if(optimal_info_.is_valid()){
        if(backup_info < optimal_info_){
          optimal_info_ = backup_info;
        }
      }
      else{
        optimal_info_ = backup_info;
      }
      return true;
    }

    template<typename T>
    bool LoadBalancer<T>::get_optimal(std::vector<int> &opt_npets,
                    std::vector<std::pair<int, int> > &opt_pet_ranges,
                    T &opt_wtime)
    {
      if(optimal_info_.is_valid()){
        optimal_info_.get_info(opt_npets, opt_pet_ranges, opt_wtime);
        return true;
      }

      return false;
    }

    template<typename T>
    void LoadBalancer<T>::update_backup_info(
      const std::vector<CompInfo<T> > &comp_infos)
    {
      if(backup_infos_.empty()){
        return;
      }

      std::vector<int> comp_npets;
      std::vector<std::pair<int, int> > comp_pet_ranges;
      T min_app_wtime = static_cast<T>(0);
      T max_app_wtime = static_cast<T>(0);
      for(typename std::vector<CompInfo<T> >::const_iterator citer = comp_infos.cbegin();
          citer != comp_infos.cend(); ++citer){
        comp_npets.push_back((*citer).get_npets());
        std::pair<T, T> comp_time_intvl = (*citer).get_time_interval();
        if(min_app_wtime != 0){
          min_app_wtime = std::min(min_app_wtime, comp_time_intvl.first);
        }
        else{
          min_app_wtime = comp_time_intvl.first;
        }

        if(max_app_wtime != 0){
          max_app_wtime = std::max(max_app_wtime, comp_time_intvl.second);
        }
        else{
          max_app_wtime = comp_time_intvl.second;
        }
        // Ignoring pet range for now
      }

      T app_wtime = max_app_wtime - min_app_wtime;
      assert(app_wtime > static_cast<T>(0));
      LoadBalancerBackupInfo app_info(comp_npets, comp_pet_ranges, app_wtime);
      /* We usually end up updating the last entry in backup_infos_ */
      for(typename std::vector<LoadBalancerBackupInfo>::reverse_iterator riter =
            backup_infos_.rbegin();
          riter != backup_infos_.rend(); ++riter){
        if(app_info == *riter){
          (*riter).set_info(comp_npets, comp_pet_ranges, app_wtime);
          break;
        }
      }
      if(app_info == optimal_info_){
        /* The backup infos are already updated, pick a new minimum/optimal */
        typename std::vector<LoadBalancerBackupInfo>::iterator min_iter =
          std::min_element(backup_infos_.begin(), backup_infos_.end());
        /* Backup infos is not empty() - see first check in the function */
        optimal_info_ = *min_iter;
      }
      else if(app_info < optimal_info_){
        optimal_info_ = app_info;
      }
    }

    template<typename T>
    LoadBalancer<T>::LoadBalancerBackupInfo::LoadBalancerBackupInfo():
      is_valid_(false),
      opt_wtime_(static_cast<T>(0)),
      opt_wtime_pred_err_(static_cast<T>(0))
    {
    }

    template<typename T>
    LoadBalancer<T>::LoadBalancerBackupInfo::LoadBalancerBackupInfo(
      const std::vector<int> &opt_npets,
      const std::vector<std::pair<int, int> > &opt_pet_ranges,
      T opt_wtime):
        is_valid_(true),
        opt_npets_(opt_npets),
        opt_pet_ranges_(opt_pet_ranges),
        opt_wtime_(opt_wtime),
        opt_wtime_pred_err_(static_cast<T>(0))
    {
    }

    template<typename T>
    bool LoadBalancer<T>::LoadBalancerBackupInfo::operator<(
      const LoadBalancerBackupInfo &other_info) const
    {
      assert(is_valid_ && other_info.is_valid_);
      return (opt_wtime_ < other_info.opt_wtime_);
    }

    template<typename T>
    bool LoadBalancer<T>::LoadBalancerBackupInfo::operator==(
      const LoadBalancerBackupInfo &other_info) const
    {
      if(!is_valid_ || !other_info.is_valid_){
        return false;
      }
      return ((opt_npets_ == other_info.opt_npets_) &&
              (opt_pet_ranges_ == other_info.opt_pet_ranges_));
    }

    template<typename T>
    void LoadBalancer<T>::LoadBalancerBackupInfo::get_info(
      std::vector<int> &opt_npets,
      std::vector<std::pair<int, int> > &opt_pet_ranges,
      T &opt_wtime) const
    {
      assert(is_valid_);
      opt_npets = opt_npets_;
      opt_pet_ranges = opt_pet_ranges_;
      opt_wtime = opt_wtime_;
    }

    template<typename T>
    void LoadBalancer<T>::LoadBalancerBackupInfo::set_info(
      const std::vector<int> &opt_npets,
      const std::vector<std::pair<int, int> > &opt_pet_ranges,
      T opt_wtime)
    {
      opt_npets_ = opt_npets;
      opt_pet_ranges_ = opt_pet_ranges;
      /* Store the latest prediction error */
      opt_wtime_pred_err_ = opt_wtime_ - opt_wtime;
      //std::cout << "Solution app walltime prediction error = "
      //  << opt_wtime_pred_err_ << "\n";
      opt_wtime_ = opt_wtime;
      is_valid_ = true;
    }

    template<typename T>
    bool LoadBalancer<T>::LoadBalancerBackupInfo::is_valid(void ) const
    {
      return is_valid_;
    }

  } // MapperUtil
} //namespace ESMCI

#endif // ESMCI_LoadBalancer_H
