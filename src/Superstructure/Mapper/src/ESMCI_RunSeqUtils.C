#include <iostream>
#include <string>
#include <cstring>
#include <cassert>
#include <cstdlib>
#include <fstream>
#include <vector>
#include <regex>
#include <stack>
#include <algorithm>
#include "ESMCI_Macros.h"
#include "ESMCI_Util.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Graph.h"
#include "ESMCI_GraphUtils.h"
#include "ESMCI_RunSeqDGraph.h"

namespace ESMCI{
  namespace MapperUtil{

    /* NUOPC Runsequence parser tokens */
    typedef enum {
      ESMF_TIMELOOP_BEGIN_TOKEN = 1,
      ESMF_TIMELOOP_END_TOKEN,
      ESMF_COMP_PHASE_TOKEN,
      ESMF_CONN_TOKEN,
      ESMF_RUNSEQ_BEGIN_TOKEN,
      ESMF_RUNSEQ_END_TOKEN,
      ESMF_OTHER_TOKEN
    } RunSeqTokenType;

    struct RunSeqTokenizedLine{
      int lnum;
      int inum;
      RunSeqTokenType type;
      std::vector<std::string> tokens; 
    };

    /* Tokenize a NUOPC run sequence */
    int TokenizeRunSeq(const std::vector<std::string> &rseq_lines, std::vector<RunSeqTokenizedLine> &tokenized_rseq_lines)
    {
      // Ignore comments
      std::regex comment_rgx("[[:space:]]*#.*");
      
      int line_num = 0;
      for(std::vector<std::string>::const_iterator citer = rseq_lines.cbegin();
          citer != rseq_lines.cend(); ++citer, line_num++){
        if(std::regex_match(*citer, comment_rgx)){
          //std::cout << "Line " << line_num << " is a comment\n";
          continue;
        }
        std::string line = *citer;
        RunSeqTokenizedLine line_tok;
        line_tok.lnum = line_num;
        line_tok.inum = -1;

        // Run sequence begin
        std::regex esmf_runseq_begin_token_rgx("[[:space:]]*runSeq::[[:space:]]*");
        if(std::regex_match(line, esmf_runseq_begin_token_rgx)){
          //std::cout << "line " << line_num << " ESMF_RUNSEQ_BEGIN_TOKEN\n";
          continue;
        }
        // Run sequence end
        std::regex esmf_runseq_end_token_rgx("[[:space:]]*::[[:space:]]*");
        if(std::regex_match(line, esmf_runseq_end_token_rgx)){
          //std::cout << "line " << line_num << " ESMF_RUNSEQ_END_TOKEN\n";
          continue;
        }

        // Remove comments and misc characters (remapMethod etc) at the
        // end of the line
        std::regex line_with_ignore_chars_at_end("([^#:]*)[#:](.*)");
        std::smatch match;
        if(std::regex_search(line, match, line_with_ignore_chars_at_end)){
          if(match.size() > 1){
            line = match.str(1);
          }
        }

        // Timeloop begin
        std::regex esmf_timeloop_begin_token_rgx("[[:space:]]*@([0-9.]*)[[:space:]]*");
        if(std::regex_search(line, match, esmf_timeloop_begin_token_rgx)){
          if(match.size() > 1){
            //std::cout << match.str(1).c_str() << "\n";
            std::string tmp_str = match.str(1);
            tmp_str.erase(std::remove_if(tmp_str.begin(), tmp_str.end(), isspace), tmp_str.end());
            if(tmp_str.size() > 0){
              line_tok.type = ESMF_TIMELOOP_BEGIN_TOKEN;
              line_tok.tokens.push_back(tmp_str);
              float time = atof(tmp_str.c_str());
              //std::cout << "line " << line_num << " ESMF_TIMELOOP_BEGIN_TOKEN : " << time << "\n";
            }
            else{
              line_tok.type = ESMF_TIMELOOP_END_TOKEN;
              //std::cout << "line " << line_num << " ESMF_TIMELOOP_END_TOKEN\n";
            }
          }
          tokenized_rseq_lines.push_back(line_tok);
          continue;
        }

        std::regex esmf_conn_token_rgx("[[:space:]]*(.+)->(.+)[[:space:]]*");
        if(std::regex_search(line, match, esmf_conn_token_rgx)){
          //std::cout << "line " << line_num << " ESMF_CONN_TOKEN \n";
          if(match.size() > 2){
            //std::cout << "From " << match.str(1).c_str() << " to " << match.str(2).c_str() << "\n";
            line_tok.type = ESMF_CONN_TOKEN;
            std::string from = match.str(1);
            from.erase(std::remove_if(from.begin(), from.end(), isspace), from.end());
            std::string to = match.str(2);
            to.erase(std::remove_if(to.begin(), to.end(), isspace), to.end());
            line_tok.tokens.push_back(from);
            line_tok.tokens.push_back(to);
            tokenized_rseq_lines.push_back(line_tok);
          }
          continue;
        }

        std::regex esmf_comp_phase_token_rgx("[[:space:]]*([^[:space:]]+)[[:space:]]+([^[:space:]]+)[[:space:]]*");
        if(std::regex_search(line, match, esmf_comp_phase_token_rgx)){
          //std::cout << "line " << line_num << " ESMF_COMP_PHASE_TOKEN\n";
          if(match.size() > 2){
            line_tok.type = ESMF_COMP_PHASE_TOKEN;
            std::string cname = match.str(1);
            cname.erase(std::remove_if(cname.begin(), cname.end(), isspace), cname.end());
            std::string pname = match.str(2);
            pname.erase(std::remove_if(pname.begin(), pname.end(), isspace), pname.end());
            line_tok.tokens.push_back(cname);
            line_tok.tokens.push_back(pname);
            //std::cout << cname.c_str() << " & " << pname.c_str() << "\n";
            tokenized_rseq_lines.push_back(line_tok);
          }
          continue;
        }

        std::regex esmf_comp_token_rgx("[[:space:]]*([^[:space:]]+)[[:space:]]*");
        if(std::regex_search(line, match, esmf_comp_token_rgx)){
          //std::cout << "line " << line_num << " ESMF_COMP_PHASE_TOKEN\n";
          if(match.size() > 1){
            line_tok.type = ESMF_COMP_PHASE_TOKEN;
            std::string cname = match.str(1);
            cname.erase(std::remove_if(cname.begin(), cname.end(), isspace), cname.end());
            line_tok.tokens.push_back(cname);
            //std::cout << cname.c_str() << "\n";
            tokenized_rseq_lines.push_back(line_tok);
          }
          continue;
        }

        std::cout << " WARNING: Couldn't process LINE : "  << line << "\n";
      }
      return ESMF_SUCCESS;
    }

    int UnwindLoopsInRunSeq(std::vector<RunSeqTokenizedLine> &tokenized_rseq_lines)
    {
      std::vector<RunSeqTokenizedLine> unrolled_rseq_lines;
      typedef struct LoopInfo_{
        float time;
        int lcount;
        int sline;
      } LoopInfo;

      // std::pair<time, loopCount>
      std::stack<LoopInfo> linfo;

      for(int cur_line = 0; cur_line < tokenized_rseq_lines.size();)
      {
        if(tokenized_rseq_lines[cur_line].type == ESMF_TIMELOOP_BEGIN_TOKEN){
          if(linfo.size() == 0){
            /* Parsing the first TIMELOOP_BEGIN_TOKEN for the first time */
            LoopInfo info;
            assert(tokenized_rseq_lines[cur_line].tokens.size() == 1);
            info.time = atof(tokenized_rseq_lines[cur_line].tokens[0].c_str());
            info.lcount = 1;
            info.sline = cur_line;
            linfo.push(info);
            cur_line++;
          }
          else if(linfo.top().sline != cur_line){
            /* Parsing the other TIMELOOP_BEGIN_TOKENs for the first time */
            LoopInfo info;
            assert(tokenized_rseq_lines[cur_line].tokens.size() == 1);
            info.time = atof(tokenized_rseq_lines[cur_line].tokens[0].c_str());
            info.lcount = static_cast<int>(linfo.top().time/info.time);
            info.sline = cur_line;
            linfo.push(info);
            cur_line++;
          }
          else{
            /* Already in a loop */
            cur_line++;
          }
        }
        else if(tokenized_rseq_lines[cur_line].type == ESMF_TIMELOOP_END_TOKEN){
          assert(linfo.size() > 0);
          linfo.top().lcount--;
          if(linfo.top().lcount > 0){
            cur_line = linfo.top().sline;
          }
          else{
            linfo.pop();
            cur_line++;
          }
        }
        else{
          if(linfo.size() > 0){
            tokenized_rseq_lines[cur_line].inum = linfo.top().lcount;
          }
          else{
            tokenized_rseq_lines[cur_line].inum = 1;
          }
          unrolled_rseq_lines.push_back(tokenized_rseq_lines[cur_line]);
          cur_line++;
        }
      }

      tokenized_rseq_lines = unrolled_rseq_lines;
      for(int i=0; i<tokenized_rseq_lines.size(); i++){
        //std::cout << "Unrolled line : " << unrolled_rseq_lines[i].lnum << " : " << unrolled_rseq_lines[i].inum << "\n";
      }

      return ESMF_SUCCESS;
    }

    class RunSeqDGraphNodeInfo{
      public:
        RunSeqDGraphNodeInfo(const std::string &comp_name, const std::string &phase_name, int line_num, int iter_num);
        RunSeqDGraph::RunSeqDGraphNode &get_node(void );
        void set_vertex_key(const RunSeqDGraph::vertex_key &node_key);
        RunSeqDGraph::vertex_key get_vertex_key(void ) const;
        void add_dependency(const std::vector<RunSeqDGraphNodeInfo> &infos, int dep_idx);
        std::vector<RunSeqDGraph::vertex_key> get_dependencies(const std::vector<RunSeqDGraphNodeInfo> &infos) const;
      private:
        friend std::ostream &operator<<(std::ostream &ostr, const RunSeqDGraphNodeInfo &info);
        RunSeqDGraph::RunSeqDGraphNode node_;
        bool node_key_is_valid_;
        RunSeqDGraph::vertex_key node_key_;
        std::vector<int> dep_indices_;
    };

    std::ostream &operator<<(std::ostream &ostr, const RunSeqDGraphNodeInfo &info)
    {
      std::cout << info.node_.get_comp_name().c_str() << ":"
                << info.node_.get_phase_name().c_str();
      std::cout << " , deps = ";
      for(int i=0; i < info.dep_indices_.size(); i++){
        std::cout << info.dep_indices_[i] << ", ";
      }
      std::cout << "\n";
      return ostr;
    }

    bool isRunSeqDGraphNodesEquiv(const RunSeqDGraph::RunSeqDGraphNode &node1, const RunSeqDGraph::RunSeqDGraphNode &node2)
    {
      if(node1.get_comp_name() != node2.get_comp_name()){
        return false;
      }
      if(node1.get_phase_name() != node2.get_phase_name()){
        return false;
      }
      if(node1.get_line_num() != node2.get_line_num()){
        return false;
      }

      return true;
    }

    RunSeqDGraphNodeInfo::RunSeqDGraphNodeInfo(const std::string &comp_name, const std::string &phase_name, int line_num, int iter_num):node_(comp_name, phase_name, line_num, iter_num), node_key_is_valid_(false)
    {
    }

    RunSeqDGraph::RunSeqDGraphNode &RunSeqDGraphNodeInfo::get_node(void )
    {
      return node_;
    }

    void RunSeqDGraphNodeInfo::set_vertex_key(const RunSeqDGraph::vertex_key &node_key)
    {
      node_key_ = node_key;
      node_key_is_valid_ = true;
    }

    RunSeqDGraph::vertex_key RunSeqDGraphNodeInfo::get_vertex_key(void ) const
    {
      assert(node_key_is_valid_);
      return node_key_;
    }

    void RunSeqDGraphNodeInfo::add_dependency(const std::vector<RunSeqDGraphNodeInfo> &infos, int dep_idx)
    {
      assert(dep_idx < infos.size());
      for(std::vector<int>::const_iterator citer = dep_indices_.cbegin();
          citer != dep_indices_.cend(); ++citer){
        if(*citer == dep_idx){
          return;
        }
        if(isRunSeqDGraphNodesEquiv(infos[*citer].node_, infos[dep_idx].node_)){
          return;
        }
      }
      dep_indices_.push_back(dep_idx);
    }

    std::vector<RunSeqDGraph::vertex_key> RunSeqDGraphNodeInfo::get_dependencies(const std::vector<RunSeqDGraphNodeInfo> &infos) const
    {
      std::vector<RunSeqDGraph::vertex_key> res;
      for(std::vector<int>::const_iterator citer = dep_indices_.cbegin();
          citer != dep_indices_.cend(); ++citer){
        assert(*citer < infos.size());
        res.push_back(infos[*citer].get_vertex_key());
      }
      return res;
    }

    int FindDependencies(const std::vector<RunSeqTokenizedLine> &tokenized_rseq_lines, std::vector<RunSeqDGraphNodeInfo> &graph_node_infos)
    {
      std::vector<int> map_line_num_trl2gni;
      /* For each line find the dependencies */
      for(int i = 0, idx_trl2gni=0; i < tokenized_rseq_lines.size(); i++){
        /* Only comp phases will be present in the final graph node info collection */
        if(tokenized_rseq_lines[i].type == ESMF_COMP_PHASE_TOKEN){
          assert(tokenized_rseq_lines[i].tokens.size() > 0);
          map_line_num_trl2gni.push_back(idx_trl2gni);
          idx_trl2gni++;

          std::string comp_name = tokenized_rseq_lines[i].tokens[0];
          std::string phase_name = "";
          if(tokenized_rseq_lines[i].tokens.size() > 1){
            phase_name = tokenized_rseq_lines[i].tokens[1];
          }
          RunSeqDGraphNodeInfo info(comp_name, phase_name, tokenized_rseq_lines[i].lnum, tokenized_rseq_lines[i].inum);
          graph_node_infos.push_back(info);
          /* Two kinds of dependencies here,
           * 1. Prev execution of the same component (same or different phase)
           * 2. A dependent component via a connector
           */
          bool found_prev_execution_dep = false;
          for(int j=i-1; j>=0; j--){
            if(tokenized_rseq_lines[j].type == ESMF_COMP_PHASE_TOKEN){
              if(!found_prev_execution_dep){
                assert(tokenized_rseq_lines[j].tokens.size() > 0);
                if(tokenized_rseq_lines[j].tokens[0] == comp_name){
                  graph_node_infos.back().add_dependency(graph_node_infos, map_line_num_trl2gni[j]);
                  found_prev_execution_dep = true;
                  /* Once a previous execution dependency node is found, stop searching
                   * for further dependencies since it would have been taken care
                   * by that node
                   */
                  break;
                }
              }
            }
            else if(tokenized_rseq_lines[j].type == ESMF_CONN_TOKEN){
              assert(tokenized_rseq_lines[j].tokens.size() == 2);
              if(tokenized_rseq_lines[j].tokens[1] == comp_name){
                for(int k=j-1; k >= 0; k--){
                  if(tokenized_rseq_lines[k].type == ESMF_COMP_PHASE_TOKEN){
                    if(tokenized_rseq_lines[k].tokens[0] == tokenized_rseq_lines[j].tokens[0]){
                      graph_node_infos.back().add_dependency(graph_node_infos, map_line_num_trl2gni[k]);
                      break;
                    }
                  }
                }
              }
            }
          }
        }
        else{
          /* Connectors and misc tokens are not in graph node info collection */
          map_line_num_trl2gni.push_back(-1);
        }
      }
      return ESMF_SUCCESS;
    }

    int InitRunSeqDGraph(std::vector<RunSeqDGraphNodeInfo> &graph_node_infos, RunSeqDGraph &g)
    {
      //for(int i=0; i<graph_node_infos.size(); i++){
      //  std::cout << "RSeqDepGraphNode " << i << "\n";
      //  std::cout << graph_node_infos[i];
      //}

      // Add the nodes into the graph
      for(std::vector<RunSeqDGraphNodeInfo>::iterator iter = graph_node_infos.begin();
          iter != graph_node_infos.end(); ++iter){
        RunSeqDGraph::RunSeqDGraphNode node = iter->get_node();
        RunSeqDGraph::vertex_key v = g.add_node(node.get_comp_name(), node.get_phase_name(), node.get_line_num(), node.get_iter_num());
        iter->set_vertex_key(v); 
      }

      // Add edges to dependencies
      for(std::vector<RunSeqDGraphNodeInfo>::const_iterator citer = graph_node_infos.cbegin();
          citer != graph_node_infos.cend(); ++citer){
        std::vector<RunSeqDGraph::vertex_key> deps = citer->get_dependencies(graph_node_infos);
        RunSeqDGraph::vertex_key v1 = citer->get_vertex_key();
        for(std::vector<RunSeqDGraph::vertex_key>::const_iterator deps_citer = deps.cbegin();
            deps_citer != deps.cend(); ++deps_citer){
          RunSeqDGraph::vertex_key v2 = *deps_citer;
          g.add_edge(v1, v2);
        }
      }

      return ESMF_SUCCESS;
    }

    /* Create dependency graph from run sequence */
    int CreateDGraphFromRSeq(const std::string &rseq_fname, RunSeqDGraph &g)
    {
      int ret = ESMF_SUCCESS;
      assert(rseq_fname.size() > 0);
      std::ifstream rseq_fstr(rseq_fname);

      // Read the file
      std::vector<std::string> rseq_lines;
      if(rseq_fstr.is_open()){
        std::string line;
        while(getline(rseq_fstr, line)){
          rseq_lines.push_back(line);
          //std::cout << " line : " << rseq_lines.size() - 1 << " : " << line << "\n";
        }
      }
      else{
        std::cout << "Error opening file : " << rseq_fname.c_str() << "\n";
        return ESMF_FAILURE;
      }

      // Tokenize the contents
      std::vector<RunSeqTokenizedLine> tokenized_rseq_lines;
      ret = TokenizeRunSeq(rseq_lines, tokenized_rseq_lines);
      if(ret != ESMF_SUCCESS){
        std::cout << "Tokenizing run sequence failed\n";
        return ESMF_FAILURE;
      }
      // Unwind the loops
      ret = UnwindLoopsInRunSeq(tokenized_rseq_lines);
      if(ret != ESMF_SUCCESS){
        std::cout << "Unwinding loops in run sequence failed\n";
        return ESMF_FAILURE;
      }
      // Find dependecies between the component phases
      std::vector<RunSeqDGraphNodeInfo> graph_node_infos;
      ret = FindDependencies(tokenized_rseq_lines, graph_node_infos);
      if(ret != ESMF_SUCCESS){
        std::cout << "Finding dependecies between component phases failed\n";
        return ESMF_FAILURE;
      }
      // Init the runsequence graph
      ret = InitRunSeqDGraph(graph_node_infos, g);
      if(ret != ESMF_SUCCESS){
        std::cout << "Unable to init the run sequence dependency graph\n";
        return ESMF_FAILURE;
      }

      return ESMF_SUCCESS;
    }

  } // namespace MapperUtil
} // namespace ESMCI
