#include "ESMCI_Mapper.h"
#include "ESMC_VM.h"
#include <string>
#include <vector>
#include <map>
#include <utility>
#include <algorithm>

namespace ESMCI{

  namespace MapperStaticInfo{
    // FIXME: We assume that there is only one mapper instance
    static std::map<std::string, ESMCI::MapperUtil::CompInfo<double > > comp_infos_map;
    static std::vector<ESMCI::MapperUtil::CompInfo<double> > comp_infos;
  } // namespace MapperStaticInfo

  namespace MapperUtil{
    template<typename T>
    class CompInfoCmpByName{
      public:
        CompInfoCmpByName(const std::string &comp_name,
                          const std::string &phase_name):
                          comp_name_(comp_name), phase_name_(phase_name)
        {}
        bool operator()(const ESMCI::MapperUtil::CompInfo<T> &comp_info)
        {
          return ((comp_info.get_comp_name() == comp_name_) &&
                  (comp_info.get_comp_phase_name() == phase_name_));
        }
      private:
        std::string comp_name_;
        std::string phase_name_;
    };
  } // namespace MapperUtil

  Mapper::Mapper(ESMCI::VM &vm):vm_(vm), comm_(MPI_COMM_NULL), is_root_proc_(false), use_load_balancer_(true), use_rseq_dgraph_dep_(false), lbal_max_iters_(DEFAULT_LBAL_MAX_ITERS)
  {
    comp_info_store_ = MapperUtil::CompInfoStore<double>::get_instance();

    /* FIXME: We need to get the communicator from VM, instead of using
     * comm world
     */
    /* MPI_Comm comm = vm.getMpi_c();
    if(comm == MPI_COMM_NULL){
      comm = MPI_COMM_WORLD;
    }
    */
    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Comm_dup(comm, &comm_);
    int rank;
    MPI_Comm_rank(comm_, &rank);
    if(rank == ROOT_PROC){
      is_root_proc_ = true;
    }
  }

  Mapper::Mapper(ESMCI::VM &vm, const std::string &rseq_fname)
    :vm_(vm), comm_(MPI_COMM_NULL), is_root_proc_(false), use_load_balancer_(true), use_rseq_dgraph_dep_(true), lbal_max_iters_(DEFAULT_LBAL_MAX_ITERS), rseq_fname_(rseq_fname)
  {
    comp_info_store_ = MapperUtil::CompInfoStore<double>::get_instance();

    /* Create a run sequence dependency graph by reading the run sequence */
    int rc = MapperUtil::CreateDGraphFromRSeq(rseq_fname_, rseq_dgraph_);
    /* FIXME: Throw an exception instead */
    assert(rc == ESMF_SUCCESS);
    rseq_dgraph_.print_to_file("./RSeqDgraph.dot");
    //int ret = ESMC_VMGet(vm, NULL, NULL, NULL, &comm_, NULL, NULL);
    //assert(ret == ESMF_SUCCESS);
    /* FIXME: We need to get the communicator from VM, instead of using
     * comm world
     */
    /*
    MPI_Comm comm = vm.getMpi_c();
    if(comm == MPI_COMM_NULL){
      comm = MPI_COMM_WORLD;
    }
    */
    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Comm_dup(comm, &comm_);
    int rank;
    MPI_Comm_rank(comm_, &rank);
    if(rank == ROOT_PROC){
      is_root_proc_ = true;
    }
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
    bool retval = false;
    if(is_root_proc_){
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
            retval =  lb_.get_optimal(opt_npets, opt_pet_ranges, opt_wtime);
          }
          else{
            retval = true;
          }
        }
        else{
          /* Find the layout with the min opt_wtime among opt_layouts */
          assert(0);
          retval = false;
        }
      }
      else if(use_load_balancer_){
        lb_.set_lb_info(comp_infos_);
        lb_.optimize(opt_npets, opt_pet_ranges, opt_wtime);
        bool opt_pets_available = lb_.get_next_optimal_candidate(opt_npets,
                                    opt_pet_ranges, opt_wtime);
        if(!opt_pets_available){
          lb_.get_optimal(opt_npets, opt_pet_ranges, opt_wtime);
        }

        retval = true;
      }
    }

    // This can be removed later, but is useful in debugging
    MPI_Bcast(&retval, 1, MPI_BYTE, ROOT_PROC, comm_);
    if(!retval){
      return retval;
    }

    sync_opt_info(opt_npets, opt_pet_ranges, opt_wtime);

    return retval;
  }

  bool Mapper::get_optimal(std::vector<int> &opt_npets,
                        std::vector<std::pair<int, int> > &opt_pet_ranges,
                        double &opt_wtime)
  {
    int retval = false;
    if(is_root_proc_){
      retval = lb_.get_optimal(opt_npets, opt_pet_ranges, opt_wtime);
    }

    // This can be removed later, but is useful in debugging
    MPI_Bcast(&retval, 1, MPI_BYTE, ROOT_PROC, comm_);
    if(!retval){
      return retval;
    }

    sync_opt_info(opt_npets, opt_pet_ranges, opt_wtime);

    return retval;
  }

  Mapper::~Mapper()
  {
    MapperUtil::CompInfoStore<double>::finalize();
    MPI_Comm_free(&comm_);
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

  bool Mapper::sync_opt_info(std::vector<int> &opt_npets,
                        std::vector<std::pair<int, int> > &opt_pet_ranges,
                        double &opt_wtime)
  {
    int retval = false;
    // Send opt PET info to non-root processes
    int nopt_npets = static_cast<int>(opt_npets.size());
    MPI_Bcast(&nopt_npets, 1, MPI_INT, ROOT_PROC, comm_);

    std::vector<int> opt_pet_ranges_cbuf(2 * nopt_npets, 0);
    if(is_root_proc_){
      // Pack the ranges to send to a contiguous buffer
      for(std::size_t i=0,j=0;
          (i < opt_pet_ranges.size()) && (j < opt_pet_ranges_cbuf.size());
          i++, j+=2){
        opt_pet_ranges_cbuf[j] = opt_pet_ranges[i].first;
        assert(j+1 < opt_pet_ranges_cbuf.size());
        opt_pet_ranges_cbuf[j+1] = opt_pet_ranges[i].second;
      }
    }
  
    if(!is_root_proc_){
      // Allocate mem to receive PET info from root process
      opt_npets.resize(nopt_npets);
      opt_pet_ranges.resize(nopt_npets);
    }

    MPI_Bcast(&(opt_npets[0]), nopt_npets, MPI_INT, ROOT_PROC, comm_);
    MPI_Bcast(&(opt_pet_ranges_cbuf[0]), 2 * nopt_npets, MPI_INT, ROOT_PROC, comm_);

    if(!is_root_proc_){
      // UnPack the ranges from the contiguous buffer
      for(std::size_t i=0,j=0;
          (i < opt_pet_ranges.size()) && (j < opt_pet_ranges_cbuf.size());
          i++, j+=2){
        opt_pet_ranges[i].first = opt_pet_ranges_cbuf[j];
        assert(j+1 < opt_pet_ranges_cbuf.size());
        opt_pet_ranges[i].second = opt_pet_ranges_cbuf[j+1];
      }
    }
    MPI_Bcast(&opt_wtime, 1, MPI_DOUBLE, ROOT_PROC, comm_);
    return retval;
  }

} // namespace ESMCI

extern "C"{

  ESMCI::Mapper *ESMCI_MapperCreate(ESMCI::VM *vm, int config_fname_len,
    const char *config_fname, int *rc)
  {
    assert(vm);
    if(rc){
      *rc = ESMF_SUCCESS;
    }
    if(config_fname_len == 0){
      return new ESMCI::Mapper(*vm);
    }
    else{
      return new ESMCI::Mapper(*vm, config_fname); 
    }
  }

  int ESMCI_MapperSetCompInfo(ESMCI::Mapper *mapper,
        int comp_name_len, const char *ccomp_name,
        int phase_name_len, const char *cphase_name,
        int comp_pet_range_start, int comp_pet_range_end,
        double comp_time_intvl_start, double comp_time_intvl_end)
  {
    std::string comp_name(ccomp_name, ccomp_name+comp_name_len);
    std::string phase_name(cphase_name, cphase_name+phase_name_len);

    ESMCI::MapperUtil::CompInfo<double> comp_info(
      comp_name, phase_name,
      std::pair<int, int>(comp_pet_range_start, comp_pet_range_end),
      std::pair<double, double>(comp_time_intvl_start, comp_time_intvl_end));

    std::string comp_infos_map_key = comp_name + phase_name;
    std::cout << "Comp info map key : " << comp_infos_map_key.c_str() << "\n";
    std::map<std::string, ESMCI::MapperUtil::CompInfo<double> >::iterator
      map_iter = ESMCI::MapperStaticInfo::comp_infos_map.find(comp_infos_map_key);
    if(map_iter == ESMCI::MapperStaticInfo::comp_infos_map.end()){
      ESMCI::MapperStaticInfo::comp_infos_map.insert(
        std::pair<std::string, ESMCI::MapperUtil::CompInfo<double> > (
          comp_infos_map_key, comp_info));
      ESMCI::MapperStaticInfo::comp_infos.push_back(comp_info);
    }
    else{
      (*map_iter).second = comp_info;
      ESMCI::MapperUtil::CompInfoCmpByName<double> cmp(
        std::string((comp_name_len > 0) ? comp_name : ""),
        std::string((phase_name_len > 0) ? phase_name : ""));
      std::vector<ESMCI::MapperUtil::CompInfo<double> >::iterator
        viter = std::find_if(ESMCI::MapperStaticInfo::comp_infos.begin(),
                  ESMCI::MapperStaticInfo::comp_infos.end(),
                  cmp);
        assert(viter != ESMCI::MapperStaticInfo::comp_infos.end());
        *viter = comp_info;
    }

    return ESMF_SUCCESS;
  }

  int ESMCI_MapperOptimize(ESMCI::Mapper *mapper, int *opt_threshold_reached)
  {
    (*mapper).set_comp_info(ESMCI::MapperStaticInfo::comp_infos);

    std::vector<int> opt_npets;
    std::vector<std::pair<int, int> > opt_pet_ranges;
    double opt_wtime;
    bool opt_pets_available =
      (*mapper).optimize(opt_npets, opt_pet_ranges, opt_wtime);
    if(opt_threshold_reached){
      *opt_threshold_reached = (opt_pets_available) ? 0 : 1;
    }

    int i=0;
    for(std::vector<ESMCI::MapperUtil::CompInfo<double> >::iterator
        iter = ESMCI::MapperStaticInfo::comp_infos.begin();
        iter != ESMCI::MapperStaticInfo::comp_infos.end(); ++iter, i++){
      // Not modifying the vector of comp_infos for now, just the
      // comp infos map
      assert(i < static_cast<int>(opt_pet_ranges.size()));
      std::string comp_infos_map_key = (*iter).get_comp_name() +
                                      (*iter).get_comp_phase_name();
      ESMCI::MapperUtil::CompInfo<double> comp_info = 
        ESMCI::MapperStaticInfo::comp_infos_map.at(comp_infos_map_key);
      ESMCI::MapperUtil::CompInfo<double> opt_comp_info(
        comp_info.get_comp_name(), comp_info.get_comp_phase_name(),
        opt_pet_ranges[i], comp_info.get_time_interval());
      ESMCI::MapperStaticInfo::comp_infos_map.at(comp_infos_map_key) = 
        opt_comp_info;
    }

    return ESMF_SUCCESS;
  }

  int ESMCI_MapperGetCompInfo(ESMCI::Mapper *mapper,
        int comp_name_len, const char *ccomp_name,
        int phase_name_len, const char *cphase_name,
        int *comp_pet_range_start, int *comp_pet_range_end)
  {
    std::string comp_name(ccomp_name, ccomp_name+comp_name_len);
    std::string phase_name(cphase_name, cphase_name+phase_name_len);
    std::string comp_infos_map_key = comp_name + phase_name;
    ESMCI::MapperUtil::CompInfo<double> comp_info =
      ESMCI::MapperStaticInfo::comp_infos_map.at(comp_infos_map_key);
    
    std::pair<int, int> comp_pet_range = comp_info.get_pet_range();
    if(comp_pet_range_start){
      *comp_pet_range_start = comp_pet_range.first;
    }
    if(comp_pet_range_end){
      *comp_pet_range_end = comp_pet_range.second;
    }

    return ESMF_SUCCESS;
  }
  
  int ESMCI_MapperDestroy(ESMCI::Mapper *mapper)
  {
    if(mapper){
      delete mapper;
    }
    return ESMF_SUCCESS;
  }

} // extern "C"
