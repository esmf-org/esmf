#include "ESMCI_Mapper.h"
#include <vector>
#include <algorithm>

namespace ESMCI{

  Mapper::Mapper(ESMCI::VM &vm):vm_(vm), lbal_max_iters_(DEFAULT_LBAL_MAX_ITERS)
  {
    comp_info_store_ = MapperUtil::CompInfoStore<double>::get_instance();
  }

  Mapper::Mapper(ESMCI::VM &vm, const std::string &rseq_fname)
    :vm_(vm), lbal_max_iters_(DEFAULT_LBAL_MAX_ITERS), rseq_fname_(rseq_fname)
  {
    comp_info_store_ = MapperUtil::CompInfoStore<double>::get_instance();

    /* Create a run sequence dependency graph by reading the run sequence */
    int rc = MapperUtil::CreateDGraphFromRSeq(rseq_fname_, rseq_dgraph_);
    /* FIXME: Throw an exception instead */
    assert(rc == ESMF_SUCCESS);
    rseq_dgraph_.print_to_file("./RSeqDgraph.dot");
  }

  void Mapper::set_comp_info(
        const std::vector<MapperUtil::CompInfo<double> > &comp_infos)
  {
    comp_infos_ = comp_infos;
    for(std::vector<MapperUtil::CompInfo<double> >::const_iterator citer = 
          comp_infos.cbegin(); citer != comp_infos.cend(); ++citer){
      comp_info_store_->add_comp_info(*citer);
    }
    lb_.set_lb_info(comp_infos_, true);
  }

  void Mapper::add_constraint(const MapperConstraint &constraint)
  {
    /* Not implemented yet */
    assert(0);
  }

  void Mapper::rem_constraint(const MapperConstraint &constraint)
  {
    /* Not implemented yet */
    assert(0);
  }

  void Mapper::add_opt_method(MapperOptAlg opt_alg)
  {
    if(opt_alg == MAPPER_OPT_MIN_IDLE_TIME){
      use_load_balancer_ = true;
    }
    else if(opt_alg == MAPPER_OPT_USE_RSEQ_CONN_DEP){
      use_rseq_dgraph_dep_ = true;
      assert(!rseq_fname_.empty());
    }
  }

  void Mapper::rem_opt_method(MapperOptAlg opt_alg)
  {
    if(opt_alg == MAPPER_OPT_MIN_IDLE_TIME){
      use_load_balancer_ = false;
    }
    else if(opt_alg == MAPPER_OPT_USE_RSEQ_CONN_DEP){
      use_rseq_dgraph_dep_ = false;
    }
  }

  void Mapper::add_opt_method(const std::vector<MapperOptAlg> &opt_algos)
  {
    for(std::vector<MapperOptAlg>::const_iterator citer = opt_algos.cbegin();
        citer != opt_algos.cend(); ++citer){
      add_opt_method(*citer);
    }
  }

  bool Mapper::optimize(std::vector<int> &opt_npets,
                        std::vector<std::pair<int, int> > &opt_pet_ranges,
                        double &opt_wtime)
  {
    if(use_rseq_dgraph_dep_){
      std::vector<std::vector<MapperUtil::CompInfo<double> > > opt_layouts;
      get_rseq_opt_layouts(opt_layouts);
      if(use_load_balancer_){
        for(std::vector<std::vector<MapperUtil::CompInfo<double> > >::const_iterator
              citer_list = opt_layouts.cbegin();
              citer_list != opt_layouts.cend(); ++citer_list){
          lb_.set_lb_info(*citer_list);
          lb_.optimize(opt_npets, opt_pet_ranges, opt_wtime);
        }
        bool opt_pets_available = lb_.get_next_optimal_candidate(opt_npets,
                                    opt_pet_ranges, opt_wtime);
        if(!opt_pets_available){
          return lb_.get_optimal(opt_npets, opt_pet_ranges, opt_wtime);
        }
        return true;
      }
      else{
        /* Find the layout with the min opt_wtime among opt_layouts */
        assert(0);
      }
      return false;
    }
    else if(use_load_balancer_){
      lb_.set_lb_info(comp_infos_);
      lb_.optimize(opt_npets, opt_pet_ranges, opt_wtime);
      bool opt_pets_available = lb_.get_next_optimal_candidate(opt_npets,
                                  opt_pet_ranges, opt_wtime);
      if(!opt_pets_available){
        lb_.get_optimal(opt_npets, opt_pet_ranges, opt_wtime);
      }

      return true;
    }
    return false;
  }

  bool Mapper::get_optimal(std::vector<int> &opt_npets,
                        std::vector<std::pair<int, int> > &opt_pet_ranges,
                        double &opt_wtime)
  {
    return lb_.get_optimal(opt_npets, opt_pet_ranges, opt_wtime);
  }

  Mapper::~Mapper()
  {
    MapperUtil::CompInfoStore<double>::finalize();
  }

  /* Analyse dependency graph and generate different layouts */
  void Mapper::get_rseq_opt_layouts(
    std::vector<std::vector<MapperUtil::CompInfo<double> > > &opt_layouts)
  {
    /* Add the current layout */
    opt_layouts.push_back(comp_infos_);

    assert(!comp_infos_.empty());

    std::vector<MapperUtil::CompInfo<double> > rseq_opt_layout = 
      rseq_dgraph_.get_opt_layout(comp_infos_);

    opt_layouts.push_back(rseq_opt_layout);
  }

} // namespace ESMCI
