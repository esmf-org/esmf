#ifndef ESMCI_Mapper_H
#define ESMCI_Mapper_H

#include "ESMCI_VM.h"
#include "ESMCI_MapperUtils.h"

namespace ESMCI{

  /*
   * Algos to use for load balancer, run seq deendency analyzer
   * Number of iterations for the solver used by the load balancer
   * Dependencies between components
   * Hints on which components to optimize/ignore
   */
  class MapperConstraint{
  }; // class MapperConstraint

  class Mapper{
    public:
      typedef enum {
        MAPPER_OPT_MIN_IDLE_TIME,
        MAPPER_OPT_USE_RSEQ_CONN_DEP
      } MapperOptAlg;
      Mapper(ESMCI::VM &vm); 
      Mapper(ESMCI::VM &vm, const std::string &rseq_fname); 
      void set_comp_info(const std::vector<MapperUtil::CompInfo<float> > &comp_infos);
      void add_constraint(const MapperConstraint &constraint);
      void rem_constraint(const MapperConstraint &constraint);
      void add_opt_method(MapperOptAlg opt_alg);
      void rem_opt_method(MapperOptAlg opt_alg);
      void add_opt_method(const std::vector<MapperOptAlg> &opt_algos);
      bool optimize(std::vector<int> &opt_npets,
                    std::vector<std::pair<int, int> > &opt_pet_ranges,
                    float &opt_wtime);
      ~Mapper();
    private:
      ESMCI::VM &vm_;
      bool use_load_balancer_;
      bool use_rseq_dgraph_dep_;
      const int DEFAULT_LBAL_MAX_ITERS = 10;
      int lbal_max_iters_;
      std::vector<MapperUtil::CompInfo<float> > comp_infos_;
      MapperUtil::CompInfoStore<float> *comp_info_store_;
      MapperUtil::LoadBalancer<float> lb_;
      std::string rseq_fname_;
      MapperUtil::RunSeqDGraph rseq_dgraph_;

      void get_rseq_opt_layouts(
        std::vector<std::vector<MapperUtil::CompInfo<float> > > &opt_layouts);
  };

} //namespace ESMCI

#endif // ESMCI_Mapper_H
