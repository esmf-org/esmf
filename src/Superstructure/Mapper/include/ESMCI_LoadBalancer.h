#ifndef ESMCI_LoadBalancer_H
#define ESMCI_LoadBalancer_H

#include <string>
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
      private:
        std::vector<CompInfo<T> > comp_infos_;
        LoadBalancerAlg opt_alg_;
        bool get_constraint_funcs(
          std::vector<std::vector<ExecBlock<T> > > &pexec_blocks,
          std::vector<TwoVIDPoly<T> > &twovidp_cfuncs,
          std::vector<MVIDLPoly<T> > &mvidlp_cfuncs,
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
      comp_infos_ = comp_infos;
    }

    template<typename T>
    bool LoadBalancer<T>::get_constraint_funcs(
      std::vector<std::vector<ExecBlock<T> > > &pexec_blocks,
      std::vector<TwoVIDPoly<T> > &twovidp_cfuncs,
      std::vector<MVIDLPoly<T> > &mvidlp_cfuncs,
      std::vector<std::string> &cfuncs_vnames,
      std::vector<int> &cfuncs_vivals)
    {
      if(opt_alg_ == LBAL_ALG_MIN_IDLE_TIME){
        /* The constraint functions are the square of the idle times between the
         * pairs of execution blocks
         * The idle time between a pair of execution blocks is obtained by 
         * finding the difference between the scaling functions of the two
         * execution blocks
         * The contraint functions also include the relationship between the
         * number of pets (variables) used by the execution blocks
         */
        std::vector<std::pair<std::string, int> > eblock_vnames_and_ivals;
        for(typename std::vector<std::vector<ExecBlock<T> > >::iterator citer = 
              pexec_blocks.begin(); citer != pexec_blocks.end(); ++citer){
          assert(!(*citer).empty());
          typename std::vector<ExecBlock<T> >::iterator citer_exec_block_list = 
            (*citer).begin();
          std::vector<std::string> exec_block_list_vnames;
          int exec_block_list_npets = 0;
          UVIDPoly<T> sfunc_first_exec_block;
          std::vector<MVIDLPoly<T> > sfunc_cfuncs_first_exec_block;
          int first_exec_block_npets = 0;
          if(citer_exec_block_list != (*citer).end()){
            bool ret = citer_exec_block_list->get_scaling_function(
                                        sfunc_first_exec_block,
                                        sfunc_cfuncs_first_exec_block);
            first_exec_block_npets = citer_exec_block_list->get_npets();
            exec_block_list_npets += first_exec_block_npets;
            assert(ret);
            ++citer_exec_block_list;
          }
          else{
            /* Zero execution blocks in the list, we need at least two parallel
             * execution blocks in a list to minimize idle time between the 
             * parallel execution blocks. Ignore this list.
             */
            continue;
          }
          if(citer_exec_block_list != (*citer).end()){
            /* Add all constraint functions for the first execution block */
            for(typename std::vector<MVIDLPoly<T> >::const_iterator cfunc_citer = 
                  sfunc_cfuncs_first_exec_block.cbegin();
                  cfunc_citer != sfunc_cfuncs_first_exec_block.cend();
                  ++cfunc_citer){
              mvidlp_cfuncs.push_back(*cfunc_citer);
              /*
              std::vector<std::string> sfunc_cfuncs_first_exec_block_vnames =
                (*cfunc_citer).get_vnames();
              for(std::vector<std::string>::const_iterator citer_vnames = 
                    sfunc_cfuncs_first_exec_block_vnames.cbegin();
                    citer_vnames != sfunc_cfuncs_first_exec_block_vnames.cend();
                    ++citer_vnames){
                cfuncs_vnames.push_back(*citer_vnames);
              }
              */
            }
            /* Add variable names to cfunc_vnames */
            std::vector<std::string> sfunc_first_exec_block_vnames = 
              sfunc_first_exec_block.get_vnames();
            assert(sfunc_first_exec_block_vnames.size() == 1);
            eblock_vnames_and_ivals.push_back(
              std::pair<std::string, int>(sfunc_first_exec_block_vnames[0],
                                          first_exec_block_npets));
            exec_block_list_vnames.push_back(sfunc_first_exec_block_vnames[0]);
            /* At least two execution blocks are available
             * Find constraint functions for pairs consisting of the first
             * execution block and the ith execution block
             */
            for(;citer_exec_block_list != (*citer).end(); ++citer_exec_block_list){
              UVIDPoly<T> sfunc_iexec_block;
              std::vector<MVIDLPoly<T> > sfunc_cfuncs_iexec_block;
              bool ret = citer_exec_block_list->get_scaling_function(
                                    sfunc_iexec_block,
                                    sfunc_cfuncs_iexec_block);
              assert(ret);
              int iexec_block_npets = citer_exec_block_list->get_npets();
              std::vector<std::string> sfunc_iexec_block_vnames = 
                sfunc_iexec_block.get_vnames();
              assert(sfunc_iexec_block_vnames.size() == 1);
              eblock_vnames_and_ivals.push_back(
                std::pair<std::string, int>(sfunc_iexec_block_vnames[0],
                                            iexec_block_npets));
              exec_block_list_vnames.push_back(sfunc_iexec_block_vnames[0]);
              exec_block_list_npets += citer_exec_block_list->get_npets();
              for(typename std::vector<MVIDLPoly<T> >::const_iterator cfunc_citer = 
                    sfunc_cfuncs_iexec_block.cbegin();
                    cfunc_citer != sfunc_cfuncs_iexec_block.cend();
                    ++cfunc_citer){
                mvidlp_cfuncs.push_back(*cfunc_citer);
                /*
                std::vector<std::string> sfunc_cfuncs_iexec_block_vnames =
                  (*cfunc_citer).get_vnames();
                for(std::vector<std::string>::const_iterator citer_vnames = 
                      sfunc_cfuncs_iexec_block_vnames.cbegin();
                      citer_vnames != sfunc_cfuncs_iexec_block_vnames.cend();
                      ++citer_vnames){
                  cfuncs_vnames.push_back(*citer_vnames);
                }
                */
              }

              /* Find idle time function */
              std::vector<std::string> twovid_sfunc_vnames;
              twovid_sfunc_vnames.push_back(sfunc_first_exec_block_vnames[0]);
              twovid_sfunc_vnames.push_back(sfunc_iexec_block_vnames[0]);

              TwoVIDPoly<T> twovid_sfunc_first_exec_block(sfunc_first_exec_block,
                              twovid_sfunc_vnames);
              TwoVIDPoly<T> twovid_sfunc_iexec_block(sfunc_iexec_block,
                              twovid_sfunc_vnames);
              TwoVIDPoly<T> idle_time_func = twovid_sfunc_iexec_block -
                                              twovid_sfunc_first_exec_block;

              /* Add the square of the idle time function as a constraint */
              TwoVIDPoly<T> idle_time_func_sq = idle_time_func * idle_time_func;
              twovidp_cfuncs.push_back(idle_time_func_sq);

              assert(exec_block_list_vnames.size() == (*citer).size());
              /* Also add the constraint that Sum(ei) - C = 0 */
              std::vector<T> final_constraint_coeffs((*citer).size(), 1);
              final_constraint_coeffs.push_back(
                static_cast<T>(-1) * exec_block_list_npets);
              MVIDLPoly<T> final_cfunc(final_constraint_coeffs);
              final_cfunc.set_vnames(exec_block_list_vnames);

              mvidlp_cfuncs.push_back(final_cfunc);
            }
          }
          else{
            /* Only one execution block in the list, we need at least two parallel
             * execution blocks in a list to minimize idle time between the 
             * parallel execution blocks. Ignore this list.
             */
            continue;
          }
        }

        /* Get unique list of variable names by removing dups */
        /*
        std::sort(cfunc_vnames.begin(), cfunc_vnames.end());
        cfunc_vnames.erase(std::unique(cfunc_vnames.begin(), cfunc_vnames.end()),
          cfunc_vnames.end());
        */

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
        for(std::vector<std::pair<std::string, int> >::const_iterator citer = 
              eblock_vnames_and_ivals.cbegin();
              citer != eblock_vnames_and_ivals.cend(); ++citer){
          cfuncs_vnames.push_back((*citer).first);
          cfuncs_vivals.push_back((*citer).second);
        }

        return (!twovidp_cfuncs.empty());
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
      std::vector<TwoVIDPoly<T> > twovidp_cfuncs;
      std::vector<MVIDLPoly<T> > mvidlp_cfuncs;
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
                                        twovidp_cfuncs,
                                        mvidlp_cfuncs,
                                        cfuncs_vnames, cfuncs_vivals);
      }

      if(!found_constraint_funcs){
        for(int i=0; i<ncomps; i++){
          opt_npets[i] = static_cast<int>(total_pets * comp_stimes[i] / total_stime);
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
        SESolver<T> solver(cfuncs_vnames, tmp_cfuncs_vivals,
                            twovidp_cfuncs, mvidlp_cfuncs);
        std::vector<T> new_pets = solver.minimize();
        for(int i=0; i<ncomps; i++){
          opt_npets[i] = static_cast<int>(new_pets[i]);
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
      return true;
    }

  } // MapperUtil
} //namespace ESMCI
#endif // ESMCI_LoadBalancer_H
