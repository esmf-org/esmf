#ifndef ESMCI_RunSeqDGraph_H
#define ESMCI_RunSeqDGraph_H

#include <string>
#include <vector>
#include <map>
#include <list>
#include <iostream>
#include "ESMCI_Graph.h"
#include "ESMCI_GraphUtils.h"
#include "ESMCI_CompInfo.h"

namespace ESMCI{
  namespace MapperUtil{

    /* The Run sequence dependency graph
     *
     * This class stores the dependency graph gleaned from a NUOPC
     * run sequence
     */
    class RunSeqDGraph{
      public:
        /* Internal class to represent a run sequence dgraph node */
        class RunSeqDGraphNode{
          public:
            typedef DGraph<RunSeqDGraphNode>::vertex_key vertex_key;
            /* Create a dgraph node to store component name, phase name
             * and line/iteration number from the NUOPC run sequence
             */
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
        vertex_key add_root(const std::string &comp_name, const std::string &phase_name, int line_num, int iter_num);
        void inverse(void );
        bool has_dependency(const std::string &child_comp_name, const std::string &parent_comp_name);
        bool has_dependency(const std::string &child_comp_name, const std::vector<std::string> &parent_comp_names);
        void fuse_merge_phases(void );
        void print_to_file(const std::string &fname) const;
        std::vector<CompInfo<double> > get_opt_layout(
          const std::vector<CompInfo<double> > &comp_infos);
      private:
        /* A visitor class to detect components phases in a run sequence graph */
        class RunSeqDGraphCompDetector : public DGraphVisitor<RunSeqDGraphNode>{
          public:
            RunSeqDGraphCompDetector(DGraph<RunSeqDGraphNode> &g, DGraph<RunSeqDGraphNode>::ColorMap &cmap, const std::string &comp_name);
            void on_node(const typename DGraph<RunSeqDGraphNode>::vertex_key &v, RunSeqDGraphNode &val); 
            bool is_present(void ) const;
          private:
            const std::string &comp_name_;
            bool is_present_;
        };
        /* A visitor to merge component phases in a rseq dgraph */
        class RunSeqDGraphCompPhaseMerger : public DGraphPVisitor<RunSeqDGraphNode>{
          public:
            RunSeqDGraphCompPhaseMerger(DGraph<RunSeqDGraphNode> &g, DGraph<RunSeqDGraphNode>::ColorMap &cmap);
            void on_node(const typename DGraph<RunSeqDGraphNode>::vertex_key &v, 
              const typename DGraph<RunSeqDGraphNode>::vertex_key &pv,
              RunSeqDGraphNode &val, RunSeqDGraphNode &pval); 
          private:
            DGraph<RunSeqDGraphNode> &g_;
        };
        /* A visitor to generate a PET layout from a rseq dgraph */
        class RunSeqDGraphLayoutGenerator : public DGraphPVisitor<RunSeqDGraphNode>{
          public:
            RunSeqDGraphLayoutGenerator(
              DGraph<RunSeqDGraphNode> &g,
              DGraph<RunSeqDGraphNode> &g_inv,
              DGraph<RunSeqDGraphNode>::ColorMap &cmap,
              int npets);
            void on_node(const typename DGraph<RunSeqDGraphNode>::vertex_key &v, 
              const typename DGraph<RunSeqDGraphNode>::vertex_key &pv,
              RunSeqDGraphNode &val, RunSeqDGraphNode &pval);
            // FIXME: Ideally we should templatize the class
            std::vector<CompInfo<double> > get_optimal_layout(void);
          private:
            typedef struct comp_rc_info_{
              int row;
              int scol;
              int ecol;
            } comp_rc_info_t;
            class CompInfoComparator{
              public:
                  CompInfoComparator(std::map<std::string,
                    comp_rc_info_t> &cidx);
                  bool operator()(const CompInfo<double> &a,
                                  const CompInfo<double> &b);
              private:
                  std::map<std::string, comp_rc_info_t> &cidx_;
            };
            DGraph<RunSeqDGraphNode> &g_;
            DGraph<RunSeqDGraphNode> &g_inv_;
            std::string invisible_node_comp_name_;
            int npets_;
            // FIXME: Ideally we should templatize the class
            std::vector<std::vector<CompInfo<double> > > comp_infos_;
            std::map<std::string, comp_rc_info_t > comp_infos_idx_;
            std::list<RunSeqDGraphNode> comps_with_missing_parents_;
        };
        DGraph<RunSeqDGraphNode> g_;
        std::map<std::string, DGraph<RunSeqDGraphNode>::vertex_key > first_comp_instances_;
        static void get_invisible_root_node_info(std::string &comp_name,
          std::string &phase_name, int &line_num, int &iter_num);
    };

  } // namespace MapperUtil
} // namespace ESMCI

#endif // ESMCI_RunSeqDGraph_H
