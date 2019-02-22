#ifndef ESMCI_Mapper_H
#define ESMCI_Mapper_H

#include "ESMCI_VM.h"
#include "ESMCI_MapperUtils.h"
#include "mpi.h"

namespace ESMCI{

  /*
   * Algos to use for load balancer, run seq deendency analyzer
   * Number of iterations for the solver used by the load balancer
   * Dependencies between components
   * Hints on which components to optimize/ignore
   */
  class MapperConstraint{
  }; // class MapperConstraint

  /* The Mapper class
   *
   * The mapper class is used to obtain an optimal PET layout for
   * a given set of components. The user provides information
   * about each component (phase) including the range of PETs
   * currently used by the component and the wallclock (start and
   * end) times. The mapper can then be used to obtain an optimal
   * PET layout for the components (phases)
   */
  class Mapper{
    public:
      /* The different optimization algorithms that the user can
       * choose. The user can choose multiple optimization algorithms
       * while optimizing a PET layout
       * MAPPER_OPT_MIN_IDLE_TIME : Minimize idle time between potential
       *  execution blocks (component phases that run on the same PETs)
       * MAPPER_OPT_USE_RSEQ_CONN_DEP : Optimize using dependency information
       *  from a given NUOPC run sequence
       */
      typedef enum {
        MAPPER_OPT_MIN_IDLE_TIME,
        MAPPER_OPT_USE_RSEQ_CONN_DEP
      } MapperOptAlg;
      Mapper(ESMCI::VM &vm); 
      /* rseq_fname : The name of the file containing the NUOPC run sequence */
      Mapper(ESMCI::VM &vm, const std::string &rseq_fname);
      /* Set information about the components by providing a list of
       * component info objects
       */
      void set_comp_info(const std::vector<MapperUtil::CompInfo<double> > &comp_infos);
      /* Add/remove constraints to the Mapper */
      void add_constraint(const MapperConstraint &constraint);
      void rem_constraint(const MapperConstraint &constraint);
      /* Add/remove optimization method to be used by the mapper */
      void add_opt_method(MapperOptAlg opt_alg);
      void rem_opt_method(MapperOptAlg opt_alg);
      void add_opt_method(const std::vector<MapperOptAlg> &opt_algos);
      /* Optimize the PET layout */
      bool optimize(std::vector<int> &opt_npets,
                    std::vector<std::pair<int, int> > &opt_pet_ranges,
                    double &opt_wtime);
      /* Get the optimal PET layout */
      bool get_optimal(std::vector<int> &opt_npets,
                    std::vector<std::pair<int, int> > &opt_pet_ranges,
                    double &opt_wtime);
      ~Mapper();
    private:
      ESMCI::VM &vm_;
      MPI_Comm comm_;
      bool is_root_proc_;
      const int ROOT_PROC = 0;
      bool use_load_balancer_;
      bool use_rseq_dgraph_dep_;
      const int DEFAULT_LBAL_MAX_ITERS = 10;
      int lbal_max_iters_;
      std::vector<MapperUtil::CompInfo<double> > comp_infos_;
      MapperUtil::CompInfoStore<double> *comp_info_store_;
      MapperUtil::LoadBalancer<double> lb_;
      std::string rseq_fname_;
      MapperUtil::RunSeqDGraph rseq_dgraph_;

      void get_rseq_opt_layouts(
        std::vector<std::vector<MapperUtil::CompInfo<double> > > &opt_layouts);
      bool sync_opt_info(std::vector<int> &opt_npets,
                    std::vector<std::pair<int, int> > &opt_pet_ranges,
                    double &opt_wtime);
  };

} //namespace ESMCI

/* The Mapper C interfaces */
extern "C"{
  /* Create a mapper
   * vm : The ESMF VM
   * config_fname_len : The length of the NUOPC Run sequence file name
   * config_fname : NUOPC Run sequence file name
   * rc : Return code
   */
  ESMCI::Mapper *ESMCI_MapperCreate(ESMCI::VM *vm,  int config_fname_len, const char *config_fname, int *rc);
  /* Set component info corresponding to an execution.
   * mapper : The ESMF Mapper
   * comp_name_len : The length of the component name string
   * comp_name : The component name
   * phase_name_len : The length of the component phase name string
   * phase_name : The component phase name
   * comp_pet_range_start : The start PET for the component phase
   * comp_pet_range_end : The end PET for the component phase
   * comp_time_intvl_start : The start wallclock time for the component phase
   * comp_time_intvl_end : The end wallclock time for the component phase
   */
  int ESMCI_MapperSetCompInfo(ESMCI::Mapper *mapper,
        int comp_name_len, const char *comp_name,
        int phase_name_len, const char *phase_name,
        int comp_pet_range_start, int comp_pet_range_end,
        double comp_time_intvl_start, double comp_time_intvl_end);
  /* Optimize the PET layout using the Mapper
   * mapper : The ESMF Mapper
   * opt_threshold_reached : Is set to 1 if the Mapper has reached the
   *  optimization threshold (The mapper cannot optimize further)
   */
  int ESMCI_MapperOptimize(ESMCI::Mapper *mapper, int *opt_threshold_reached);
  /* Get the optimized component information, PET range, from the mapper
   * mapper : The ESMF Mapper
   * comp_name_len : The length of the component name string
   * comp_name : The component name
   * phase_name_len : The length of the component phase name string
   * phase_name : The component phase name
   * comp_pet_range_start : The start PET for the optimized PET layout for the
   *  component phase (defined by comp_name and comp_phase_name)
   * comp_pet_range_end : The end PET for the optimized PET layout for the
   *  component phase (defined by comp_name and comp_phase_name)
   */
  int ESMCI_MapperGetCompInfo(ESMCI::Mapper *mapper,
        int comp_name_len, const char *comp_name,
        int phase_name_len, const char *phase_name,
        int *comp_pet_range_start, int *comp_pet_range_end);
  /* Destroy/Finalize the mapper */
  int ESMCI_MapperDestroy(ESMCI::Mapper *mapper);
} // extern "C"


#endif // ESMCI_Mapper_H
