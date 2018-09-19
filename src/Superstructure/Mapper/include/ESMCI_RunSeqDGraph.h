#ifndef ESMCI_RunSeqDGraph_H
#define ESMCI_RunSeqDGraph_H

#include <string>
#include <vector>
#include <map>
#include <iostream>
#include "ESMCI_Graph.h"
#include "ESMCI_GraphUtils.h"

namespace ESMCI{
  namespace MapperUtil{

    class RunSeqDGraph{
      public:
        class RunSeqDGraphNode{
          public:
            typedef DGraph<RunSeqDGraphNode>::vertex_key vertex_key;
            RunSeqDGraphNode(const std::string &comp_name, const std::string &phase_name, int line_num, int iter_num);
            std::string get_comp_name(void ) const;
            std::string get_phase_name(void ) const;
            void set_phase_name(const std::string &phase_name);
            int get_line_num(void ) const;
            int get_iter_num(void ) const;
            void set_vertex_key(const vertex_key &vkey);
            vertex_key get_vertex_key(void ) const;
          private:
            const int INVALID_VERTEX_KEY = -1;
            const std::string comp_name_;
            std::string phase_name_;
            const int line_num_;
            const int iter_num_;
            vertex_key vkey_;
        };
        typedef RunSeqDGraphNode::vertex_key vertex_key;
        vertex_key add_node(const std::string &comp_name, const std::string &phase_name, int line_num, int iter_num);
        void add_edge(const vertex_key &from, const vertex_key &to);
        bool has_dependency(const std::string &child_comp_name, const std::string &parent_comp_name);
        bool has_dependency(const std::string &child_comp_name, const std::vector<std::string> &parent_comp_names);
        void fuse_merge_phases(void );
        void print_to_file(const std::string &fname) const;
      private:
        class RunSeqDGraphCompDetector : public DGraphVisitor<RunSeqDGraphNode>{
          public:
            RunSeqDGraphCompDetector(DGraph<RunSeqDGraphNode> &g, DGraph<RunSeqDGraphNode>::ColorMap &cmap, const std::string &comp_name);
            void on_node(const typename DGraph<RunSeqDGraphNode>::vertex_key &v, RunSeqDGraphNode &val); 
            bool is_present(void ) const;
          private:
            const std::string &comp_name_;
            bool is_present_;
        };
        class RunSeqDGraphCompPhaseMerger : public DGraphPVisitor<RunSeqDGraphNode>{
          public:
            RunSeqDGraphCompPhaseMerger(DGraph<RunSeqDGraphNode> &g, DGraph<RunSeqDGraphNode>::ColorMap &cmap);
            void on_node(const typename DGraph<RunSeqDGraphNode>::vertex_key &v, 
              const typename DGraph<RunSeqDGraphNode>::vertex_key &pv,
              RunSeqDGraphNode &val, RunSeqDGraphNode &pval); 
          private:
            DGraph<RunSeqDGraphNode> &g_;
        };
        DGraph<RunSeqDGraphNode> g_;
        std::map<std::string, DGraph<RunSeqDGraphNode>::vertex_key > first_comp_instances_;
    };

  } // namespace MapperUtil
} // namespace ESMCI

#endif // ESMCI_RunSeqDGraph_H
