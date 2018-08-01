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
    RunSeqDGraph::RunSeqDGraphNode::RunSeqDGraphNode(const std::string &comp_name, const std::string &phase_name, int line_num, int iter_num):comp_name_(comp_name),phase_name_(phase_name),line_num_(line_num),iter_num_(iter_num)
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
      // Cache the first instance of this component (ignoring phase etc) in the 
      // dependency graph
      first_comp_instances_.insert(std::make_pair(comp_name, v));
      return v;
    }

    void RunSeqDGraph::add_edge(const vertex_key &from, const vertex_key &to)
    {
      g_.add_edge(from, to);
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

  } // namespace MapperUtil
} // namespace ESMCI
