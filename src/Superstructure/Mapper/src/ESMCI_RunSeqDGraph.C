#include <string>
#include <vector>
#include <map>
#include <iostream>
#include "ESMCI_Graph.h"
#include "ESMCI_GraphUtils.h"
#include "ESMCI_RunSeqDGraph.h"

namespace ESMCI{
  namespace MapperUtil{

    // RunSeqDGraphNode functions
    RunSeqDGraph::RunSeqDGraphNode::RunSeqDGraphNode(const std::string &comp_name, const std::string &phase_name, int line_num, int iter_num):comp_name_(comp_name),phase_name_(phase_name),line_num_(line_num),iter_num_(iter_num),vkey_(INVALID_VERTEX_KEY)
    {
    }

    std::string RunSeqDGraph::RunSeqDGraphNode::get_comp_name(void ) const
    {
      return comp_name_;
    }

    std::string RunSeqDGraph::RunSeqDGraphNode::get_phase_name(void ) const
    {
      return phase_name_;
    }

    void RunSeqDGraph::RunSeqDGraphNode::set_phase_name(const std::string &phase_name)
    {
      phase_name_ = phase_name;
    }

    int RunSeqDGraph::RunSeqDGraphNode::get_line_num(void ) const
    {
      return line_num_;
    }

    int RunSeqDGraph::RunSeqDGraphNode::get_iter_num(void ) const
    {
      return iter_num_;
    }

    void RunSeqDGraph::RunSeqDGraphNode::set_vertex_key(const vertex_key &vkey)
    {
      vkey_ = vkey;
    }

    RunSeqDGraph::RunSeqDGraphNode::vertex_key
      RunSeqDGraph::RunSeqDGraphNode::get_vertex_key(void ) const
    {
      assert(vkey_ != INVALID_VERTEX_KEY);
      return vkey_;
    }

    std::ostream& operator<<(std::ostream &ostr, const RunSeqDGraph::RunSeqDGraphNode &node)
    {
      ostr << "("
            << node.get_comp_name().c_str()
            << ", " << node.get_phase_name().c_str()
            << ", " << node.get_line_num()
            << ", " << node.get_iter_num()
            << ")";
      return ostr;
    }

    // RunSeqDGraph functions
    RunSeqDGraph::vertex_key RunSeqDGraph::add_node(const std::string &comp_name, const std::string &phase_name, int line_num, int iter_num)
    {
      vertex_key v = g_.add_node(RunSeqDGraph::RunSeqDGraphNode(comp_name, phase_name, line_num, iter_num));
      g_.get_val(v).set_vertex_key(v);
      // Cache the first instance of this component (ignoring phase etc) in the 
      // dependency graph
      first_comp_instances_.insert(std::make_pair(comp_name, v));
      return v;
    }

    void RunSeqDGraph::add_edge(const vertex_key &from, const vertex_key &to)
    {
      g_.add_edge(from, to);
    }

    RunSeqDGraph::vertex_key RunSeqDGraph::add_root(const std::string &comp_name, const std::string &phase_name, int line_num, int iter_num)
    {
      vertex_key v = g_.add_root(RunSeqDGraph::RunSeqDGraphNode(comp_name, phase_name, line_num, iter_num));
      g_.get_val(v).set_vertex_key(v);
      // Cache the first instance of this component (ignoring phase etc) in the 
      // dependency graph
      first_comp_instances_.insert(std::make_pair(comp_name, v));
      return v;
    }

    void RunSeqDGraph::inverse(void )
    {
      g_ = g_.inverse();
    }

    bool RunSeqDGraph::has_dependency(const std::string &comp_name, const std::string &parent_comp_name)
    {
      std::map<std::string, DGraph<RunSeqDGraphNode>::vertex_key>::iterator comp_viter = first_comp_instances_.find(comp_name);
      if(comp_viter == first_comp_instances_.end()){
        return false;
      }
      std::map<std::string, DGraph<RunSeqDGraphNode>::vertex_key>::iterator parent_comp_viter = first_comp_instances_.find(parent_comp_name);
      if(parent_comp_viter == first_comp_instances_.end()){
        return false;
      }
      DGraph<RunSeqDGraphNode>::vertex_key comp_v = comp_viter->second;
      //DGraph<RunSeqDGraphNode>::vertex_key parent_comp_v = parent_comp_viter->second;

      // Perform a DFS to find if parent_comp_v is a parent of comp_v
      DGraph<RunSeqDGraphNode>::ColorMap cmap = g_.create_color_map();
      RunSeqDGraphCompDetector vis(g_, cmap, parent_comp_name);

      DGraph_DFS(g_, vis, comp_v);

      return vis.is_present();
    }

    bool RunSeqDGraph::has_dependency(const std::string &comp_name, const std::vector<std::string> &parent_comp_names)
    {
      for(std::vector<std::string>::const_iterator citer = parent_comp_names.cbegin();
          citer != parent_comp_names.cend(); ++citer){
        bool has_dep = has_dependency(comp_name, *citer);
        if(has_dep){
          return true;
        }
      }

      return false;
    }

    void RunSeqDGraph::fuse_merge_phases(void )
    {
      DGraph<RunSeqDGraphNode>::ColorMap cmap = g_.create_color_map();
      RunSeqDGraphCompPhaseMerger vis(g_, cmap);
      DGraph<RunSeqDGraphNode>::vertex_iterator nodes_begin = g_.begin();
      DGraph<RunSeqDGraphNode>::vertex_iterator nodes_end = g_.end();
      for(DGraph<RunSeqDGraphNode>::vertex_iterator node_iter = nodes_begin;
          node_iter != nodes_end; ++node_iter){
        ESMCI::MapperUtil::DGraph_MBFS(g_, vis, *node_iter);
      }
    }

    void RunSeqDGraph::print_to_file(const std::string &fname) const
    {
      g_.print_to_file(fname);
    }

    std::vector<CompInfo<double> > RunSeqDGraph::get_opt_layout(
      const std::vector<CompInfo<double> > &comp_infos)
    {
      int min_pet = 0, max_pet = 0;
      std::vector<MapperUtil::CompInfo<double> >::const_iterator
        citer = comp_infos.cbegin();
      min_pet = (*citer).get_pet_range().first;
      max_pet = (*citer).get_pet_range().second;
      for(;citer != comp_infos.cend(); ++citer){
        min_pet = std::min(min_pet, (*citer).get_pet_range().first);
        max_pet = std::max(max_pet, (*citer).get_pet_range().second);
      }
      int npets = max_pet - min_pet + 1;
      assert(npets > 0);

      /* Create a copy of the dep graph and process it to get a layout */
      MapperUtil::RunSeqDGraph tmp_rseq_dgraph = *this;
      tmp_rseq_dgraph.fuse_merge_phases();

      /* Add an "invisible root" - that is not used when finding the layout */
      std::string root_comp_name, root_phase_name;
      int root_line_num, root_iter_num;
      get_invisible_root_node_info(root_comp_name, root_phase_name,
        root_line_num, root_iter_num);
      vertex_key root_vkey = tmp_rseq_dgraph.add_root(root_comp_name, root_phase_name,
                                                        root_line_num, root_iter_num);

      tmp_rseq_dgraph.inverse();

      DGraph<RunSeqDGraphNode>::ColorMap cmap = tmp_rseq_dgraph.g_.create_color_map();
      RunSeqDGraphLayoutGenerator vis(tmp_rseq_dgraph.g_, this->g_, cmap, npets);

      MapperUtil::DGraph_BFS(tmp_rseq_dgraph.g_, vis, root_vkey);
      std::vector<CompInfo<double> > opt_layout = vis.get_optimal_layout();

      std::vector<double> row_max_times(opt_layout.size(), 0.0);
      std::vector<double> row_start_times(opt_layout.size(), 0.0);
      for(std::vector<CompInfo<double> >::iterator opt_iter = opt_layout.begin();
            opt_iter != opt_layout.end(); ++opt_iter){
        for(std::vector<CompInfo<double> >::const_iterator citer = comp_infos.cbegin();
              citer != comp_infos.end(); ++citer){
          if((*opt_iter).get_comp_name() == (*citer).get_comp_name()){
            std::pair<double, double> prev_time_intvl = (*citer).get_time_interval();
            double prev_wtime = prev_time_intvl.second - prev_time_intvl.first;
            assert(prev_wtime > 0.0);
            int prev_npets = (*citer).get_npets();
            int opt_npets = (*opt_iter).get_npets();

            double opt_wtime = 
              (static_cast<double>(prev_npets)/static_cast<double>(opt_npets))
                * prev_wtime;
            std::pair<double, double> opt_time_intvl = (*opt_iter).get_time_interval();
            int row_comp = static_cast<int>(opt_time_intvl.first);
            row_max_times[row_comp] = std::max(row_max_times[row_comp], opt_wtime);
            opt_time_intvl.second = opt_wtime;
            (*opt_iter).set_time_interval(opt_time_intvl); 
          }
        }
      }

      assert(!opt_layout.empty());
      std::vector<double>::iterator row_start_times_iter = row_start_times.begin();
      *row_start_times_iter = 0.0;
      ++row_start_times_iter;
      double prev_start_time = 0.0;
      const double TIME_INTVL_BTW_ROWS = 1.0;
      for(std::vector<double>::const_iterator
            row_max_times_citer = row_max_times.cbegin();
            (row_max_times_citer != row_max_times.cend()) &&
            (row_start_times_iter != row_start_times.end());
            ++row_max_times_citer, ++row_start_times_iter){
        *row_start_times_iter = prev_start_time + *row_max_times_citer +
                                TIME_INTVL_BTW_ROWS;
        prev_start_time = *row_start_times_iter;
      }

      for(std::vector<CompInfo<double> >::iterator opt_iter = opt_layout.begin();
            opt_iter != opt_layout.end(); ++opt_iter){
        std::pair<double, double> opt_time_intvl = (*opt_iter).get_time_interval();
        int row_comp = static_cast<int>(opt_time_intvl.first);
        double opt_start_time = row_start_times[row_comp];
        opt_time_intvl.first = opt_start_time;
        opt_time_intvl.second += opt_start_time;
        (*opt_iter).set_time_interval(opt_time_intvl);
      }
      return opt_layout;
    }

    void RunSeqDGraph::get_invisible_root_node_info(
      std::string &comp_name, std::string &phase_name, int &line_num, int &iter_num)
    {
      const std::string INVISIBLE_COMP_NAME("DEFAULT_ROOT_COMP_");
      const std::string INVISIBLE_PHASE_NAME("DEFAULT_ROOT_PHASE_");
      const int INVISIBLE_LINE_NUM = -1;
      const int INVISIBLE_ITER_NUM = -1;

      comp_name = INVISIBLE_COMP_NAME;
      phase_name = INVISIBLE_PHASE_NAME;
      line_num = INVISIBLE_LINE_NUM;
      iter_num = INVISIBLE_ITER_NUM;
    }

    // RunSeqDGraphCompDetector functions
    RunSeqDGraph::RunSeqDGraphCompDetector::RunSeqDGraphCompDetector(DGraph<RunSeqDGraphNode> &g, DGraph<RunSeqDGraphNode>::ColorMap &cmap, const std::string &comp_name):DGraphVisitor<RunSeqDGraphNode>(g, cmap), comp_name_(comp_name), is_present_(false)
    {
    }

    void RunSeqDGraph::RunSeqDGraphCompDetector::on_node(const DGraph<RunSeqDGraphNode>::vertex_key &v, RunSeqDGraphNode &val)
    {
      if(val.get_comp_name() == comp_name_){
        is_present_ = true;
      }
    }

    bool RunSeqDGraph::RunSeqDGraphCompDetector::is_present(void ) const
    {
      return is_present_;
    }

    // RunSeqDGraphCompPhaseMerger functions
    RunSeqDGraph::RunSeqDGraphCompPhaseMerger::RunSeqDGraphCompPhaseMerger(
      DGraph<RunSeqDGraphNode> &g, DGraph<RunSeqDGraphNode>::ColorMap &cmap):
        DGraphPVisitor<RunSeqDGraphNode>(g, cmap),g_(g)
    {
    }

    void RunSeqDGraph::RunSeqDGraphCompPhaseMerger::on_node(
      const DGraph<RunSeqDGraphNode>::vertex_key &v,
      const DGraph<RunSeqDGraphNode>::vertex_key &pv,
      RunSeqDGraphNode &val, RunSeqDGraphNode &pval)
    {
      //DGraphPVisitor<RunSeqDGraphNode>::on_node(v, pv, val, pval);
      const std::string MULTIPLE_PHASES("*");
      std::cout << val.get_comp_name().c_str() << ", " 
                << pval.get_comp_name().c_str() << "\n";
      if(val.get_comp_name() == pval.get_comp_name()){
        /* FIXME: We should consider not modifying the graph here */
        std::cout << "Fusing nodes : " << pv << ", " << v << "\n";
        DGraphPVisitor<RunSeqDGraphNode>::on_node(v, pv, val, pval);
        g_.fuse_nodes(pv, v);
        pval.set_phase_name(MULTIPLE_PHASES);
      }
    }

    // RunSeqDGraphLayoutGenerator functions
    RunSeqDGraph::RunSeqDGraphLayoutGenerator::RunSeqDGraphLayoutGenerator(
      DGraph<RunSeqDGraphNode> &g,
      DGraph<RunSeqDGraphNode> &g_inv,
      DGraph<RunSeqDGraphNode>::ColorMap &cmap,
      int npets):
        DGraphPVisitor<RunSeqDGraphNode>(g, cmap),g_(g),g_inv_(g_inv),npets_(npets)
    {
      std::string invisible_node_phase_name;
      int line_num, iter_num;
      // Cache the invisible root comp name that we ignore when traversing
      // the graph
      get_invisible_root_node_info(invisible_node_comp_name_,
        invisible_node_phase_name, line_num, iter_num);
    }

    void RunSeqDGraph::RunSeqDGraphLayoutGenerator::on_node(
      const DGraph<RunSeqDGraphNode>::vertex_key &v,
      const DGraph<RunSeqDGraphNode>::vertex_key &pv,
      RunSeqDGraphNode &cval, RunSeqDGraphNode &pval)
    {
      std::cout << cval.get_comp_name().c_str() << ", " 
                << pval.get_comp_name().c_str() << "\n";
      DGraphPVisitor<RunSeqDGraphNode>::on_node(v, pv, cval, pval);
      // Ignore the "invisible" root when computing layouts
      if(cval.get_comp_name() == invisible_node_comp_name_){
        return;
      }

      std::map<std::string, comp_rc_info_t >::iterator iter = 
        comp_infos_idx_.find(cval.get_comp_name());
      if(iter != comp_infos_idx_.end()){
        std::cerr << "DUPLICATE node in rseq dgraph, ignoring...\n";
        return;
      }

      std::list<RunSeqDGraphNode> comps_to_process(comps_with_missing_parents_);
      comps_to_process.push_front(cval);

      while(!comps_to_process.empty()){
        RunSeqDGraphNode val = comps_to_process.front();
        comps_to_process.pop_front();

        /* Find all parents of this val */
        std::vector<DGraph<RunSeqDGraphNode>::vertex_key > parents =
          g_inv_.get_parents(val.get_vertex_key());

        int parents_row_ub = 0;
        int parents_col_min = -1;
        int parents_col_max = -1;
        bool val_has_parents = !parents.empty();
        bool val_has_1parent = (parents.size() == 1) ? true : false;
        bool val_has_nparents = (parents.size() > 1) ? true : false;
        if(val_has_parents){
          /* if not all parents are processes put this comp in missing
           * parents queue/list
           */
          for(std::vector<DGraph<RunSeqDGraphNode>::vertex_key >::iterator
                piter = parents.begin(); piter != parents.end(); ++piter){
            std::map<std::string, comp_rc_info_t >::iterator iter = 
              comp_infos_idx_.find(g_inv_.get_val(*piter).get_comp_name());
            if(iter == comp_infos_idx_.end()){
              comps_with_missing_parents_.push_back(val);
              return;
            }
            else{
              if(parents_col_min != -1){
                parents_row_ub = std::max(parents_row_ub, (*iter).second.row + 1);
                parents_col_min = std::min(parents_col_min, (*iter).second.scol);
                parents_col_max = std::max(parents_col_max, (*iter).second.ecol);
              }
              else{
                /* First parent  - try putting comp info in the next row to 
                 * parent, in the same column
                 */
                parents_row_ub = (*iter).second.row + 1;
                parents_col_min = (*iter).second.scol;
                parents_col_max = (*iter).second.ecol;
              }
            }
          }
        }
        else{
          /* No parents, try putting the comp info in next avail row, in the
           * first available col
           */
        }

        std::pair<int, int> pet_range(0, 0);
        std::pair<double, double> time_intvl(0.0, 0.0);
        CompInfo<double> val_comp_info(val.get_comp_name(),
          val.get_phase_name(), pet_range, time_intvl);

        if(!val_has_parents){
          assert(comp_infos_.size() <= 1);
          if(comp_infos_.empty()){
            std::vector<CompInfo<double> > tmp_row;
            comp_infos_.push_back(tmp_row);
          }
          assert(comp_infos_.size() == 1);
          comp_infos_[0].push_back(val_comp_info);
          /* (row, start col, end col) for this comp_info */
          comp_rc_info_t rc_info = 
            {0, static_cast<int>(comp_infos_[0].size() - 1),
                static_cast<int>(comp_infos_[0].size() - 1)};
          comp_infos_idx_.insert(std::pair<std::string, comp_rc_info_t >(
            val.get_comp_name(), rc_info));
        }
        else{
          assert(parents_row_ub >= 1);
          int last_row = static_cast<int>(comp_infos_.size() - 1);
          assert(last_row >= 0);

          assert((parents_row_ub == last_row + 1) ||
                  (parents_row_ub <= last_row));

          assert(parents_col_min >= 0);
          assert(parents_col_max >= 0);
          assert(parents_col_min <= parents_col_max);
          if(parents_row_ub == last_row + 1){
            /* Add component in the next new row */
            std::vector<CompInfo<double> > tmp_row;
            comp_infos_.push_back(tmp_row);
            comp_infos_[parents_row_ub].push_back(val_comp_info);

            /* (row, col) for this comp info */
            comp_rc_info_t rc_info = 
              {parents_row_ub, parents_col_min, parents_col_max};
            comp_infos_idx_.insert(std::pair<std::string, comp_rc_info_t >(
              val.get_comp_name(), rc_info));
          }
          else if(val_has_1parent){
            /* This component needs to go in the same col as the parent, start
             * looking for available rows starting with the immediate next row
             */
            /* Try fitting val in parents_row_ub in same col as parent
             * if that fails, add a new row and add val there in the 
             * same col as parent */
            assert(parents_row_ub <= last_row);
            /* parents_row_ub is a valid row - check if we can fit val there */
            bool can_fit_in_parents_row_ub = true;
            for(std::vector<CompInfo<double> >::const_iterator
                  citer = comp_infos_[parents_row_ub].cbegin();
                  citer != comp_infos_[parents_row_ub].cend(); ++citer){
              comp_rc_info_t rc_info = comp_infos_idx_.at((*citer).get_comp_name());
              /* If there is a comp that already streches across parent comp col
               * we cannot fit the val in the parent_row_ub row
               */
              assert(rc_info.row == parents_row_ub);
              if((rc_info.scol < parents_col_min) && 
                  (rc_info.ecol > parents_col_max)){
                can_fit_in_parents_row_ub = false;
                break;
              }
            }

            if(can_fit_in_parents_row_ub){
              comp_infos_[parents_row_ub].push_back(val_comp_info);
              comp_rc_info_t rc_info = {parents_row_ub, parents_col_min, parents_col_max};
              comp_infos_idx_.insert(std::pair<std::string, comp_rc_info_t>(
                val.get_comp_name(), rc_info));
            }
            else{
              /* Try fitting in a new row in the parent col */
              std::vector<CompInfo<double> > tmp_row;
              comp_infos_.push_back(tmp_row);
              comp_infos_[last_row+1].push_back(val_comp_info);

              comp_rc_info_t rc_info = {last_row+1, parents_col_min, parents_col_max};
              comp_infos_idx_.insert(std::pair<std::string, comp_rc_info_t>(
                val.get_comp_name(), rc_info));
            }
          }
          else{
            assert(val_has_nparents);
            assert(parents_row_ub <= last_row);
            bool can_fit_in_parents_row_ub = true;
            for(std::vector<CompInfo<double> >::const_iterator
                  citer = comp_infos_[parents_row_ub].cbegin();
                  citer != comp_infos_[parents_row_ub].cend(); ++citer){
              comp_rc_info_t rc_info = comp_infos_idx_.at((*citer).get_comp_name());
              assert(rc_info.row == parents_row_ub);
              /* If there is a comp that already streches across parent comp col
               * we cannot fit the val in the parent_row_ub row
               */
              int cur_comp_scol = rc_info.scol;
              int cur_comp_ecol = rc_info.ecol;

              bool val_end_inside_cur_comp =
                    (parents_col_min < cur_comp_scol) && 
                    (parents_col_max >= cur_comp_scol) &&
                    (parents_col_max <= cur_comp_ecol);
              bool val_inside_cur_comp =
                    (cur_comp_scol <= parents_col_min) &&
                    (parents_col_min <= cur_comp_ecol) &&
                    (parents_col_max <= cur_comp_ecol);
              bool val_start_inside_cur_comp =
                    (parents_col_min >= cur_comp_scol) &&
                    (parents_col_min <= cur_comp_ecol) &&
                    (parents_col_max > cur_comp_ecol);

              bool val_intersects_cur_comp = val_end_inside_cur_comp ||
                                              val_inside_cur_comp ||
                                              val_start_inside_cur_comp;

              if(val_intersects_cur_comp){
                can_fit_in_parents_row_ub = false;
                break;
              }
            }

            if(can_fit_in_parents_row_ub){
              comp_infos_[parents_row_ub].push_back(val_comp_info);
              comp_rc_info_t rc_info = {parents_row_ub, parents_col_min, parents_col_max};
              comp_infos_idx_.insert(std::pair<std::string, comp_rc_info_t>(
                val.get_comp_name(), rc_info));
            }
            else{
              /* Try fitting in a new row in the parent col */
              std::vector<CompInfo<double> > tmp_row;
              comp_infos_.push_back(tmp_row);
              comp_infos_[last_row+1].push_back(val_comp_info);

              comp_rc_info_t rc_info = {last_row+1, parents_col_min, parents_col_max};
              comp_infos_idx_.insert(std::pair<std::string, comp_rc_info_t>(
                val.get_comp_name(), rc_info));
            }
          }
        }
      }
    }

    std::vector<CompInfo<double> >
      RunSeqDGraph::RunSeqDGraphLayoutGenerator::get_optimal_layout(void )
    {
      std::vector<CompInfo<double> > res;
      if(!comp_infos_.empty()){
        // Max number of columns will be the number of comps in the first
        // row
        int max_ncols = static_cast<int>(comp_infos_[0].size());

        // Evenly distribute number of PETs among all columns
        int npets_per_col = 
          static_cast<int> (static_cast<double>(npets_)/static_cast<double>(max_ncols));

        CompInfoComparator cmp(comp_infos_idx_);
        for(std::vector<std::vector<CompInfo<double> > >::iterator
              list_iter = comp_infos_.begin();
              list_iter != comp_infos_.end(); ++list_iter){

          std::vector<int> ncomps_per_col(max_ncols, 0);

          // Sort each row based on col 
          std::sort((*list_iter).begin(), (*list_iter).end(), cmp);

          // Count the number of comps per col
          for(std::vector<CompInfo<double> >::iterator
                iter = (*list_iter).begin(); iter != (*list_iter).end(); ++iter){
            comp_rc_info_t rc_info = comp_infos_idx_.at((*iter).get_comp_name());
            for(int i=rc_info.scol; i<= rc_info.ecol; i++){
              ncomps_per_col[i]++;
            }
          }

          std::vector<int> npets_in_col(max_ncols, npets_per_col);
          int i=0;
          for(std::vector<int>::iterator iter = npets_in_col.begin();
                (iter != npets_in_col.end()) && (i < max_ncols); ++iter, i++){
            *iter = static_cast<int>(
                      static_cast<double>(*iter)/static_cast<double>(ncomps_per_col[i]));
          }

          int ipet = 0;
          for(std::vector<CompInfo<double> >::iterator
                iter = (*list_iter).begin(); iter != (*list_iter).end(); ++iter){
            int epet = ipet;
            comp_rc_info_t rc_info = comp_infos_idx_.at((*iter).get_comp_name());
            for(int i=rc_info.scol; i<= rc_info.ecol; i++){
              epet += npets_in_col[i] - 1;
            }
            std::pair<int, int> pet_range(ipet, epet);
            (*iter).set_pet_range(pet_range);
            // Time start and end contains the row numbers, will be used later
            // to infer the start timings for the different rows
            std::pair<double, double> time_intvl(static_cast<double>(rc_info.row),
              static_cast<double>(rc_info.row));
            (*iter).set_time_interval(time_intvl);
            res.push_back(*iter);
            ipet = epet + 1;
          }
        }
      }
      return res;  
    }

    // CompInfoComparator functions
    RunSeqDGraph::RunSeqDGraphLayoutGenerator::CompInfoComparator::CompInfoComparator(
      std::map<std::string, comp_rc_info_t> &cidx):
        cidx_(cidx)
    {
    }

    bool RunSeqDGraph::RunSeqDGraphLayoutGenerator::CompInfoComparator::operator()(
      const CompInfo<double> &a, const CompInfo<double> &b)
    {
      comp_rc_info_t a_rc_info = cidx_.at(a.get_comp_name());
      comp_rc_info_t b_rc_info = cidx_.at(b.get_comp_name());
      if(a_rc_info.scol == b_rc_info.scol){
        return a_rc_info.ecol < b_rc_info.ecol;
      }
      else{
        return a_rc_info.scol < b_rc_info.scol;
      }
    }
  } // namespace MapperUtil
} // namespace ESMCI
