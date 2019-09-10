#ifndef ESMCI_GraphUtils_H
#define ESMCI_GraphUtils_H

#include <iostream>
#include <vector>
#include <queue>
#include <map>
#include "ESMCI_Graph.h"

namespace ESMCI{
  namespace MapperUtil{

    /* The base class for all dependency graph visitors */
    template<typename T>
    class DGraphVisitor{
      public:
        DGraphVisitor(DGraph<T> &g, typename DGraph<T>::ColorMap &cmap);
        virtual void on_node(const typename DGraph<T>::vertex_key &v, T &val);
        bool has_visited(const typename DGraph<T>::vertex_key &v);
        void reset(void );
      private:
        DGraph<T> &g_;
        typename DGraph<T>::ColorMap &cmap_;
    };

    template<typename T>
    DGraphVisitor<T>::DGraphVisitor(DGraph<T> &g, typename DGraph<T>::ColorMap &cmap):g_(g),cmap_(cmap)
    {
      cmap_.reset();
    }

    template<typename T>
    void DGraphVisitor<T>::on_node(const typename DGraph<T>::vertex_key &v, T &val)
    {
      cmap_.set(v);
    }

    template<typename T>
    bool DGraphVisitor<T>::has_visited(const typename DGraph<T>::vertex_key &v)
    {
      return cmap_.is_set(v);
    }

    template<typename T>
    void DGraphVisitor<T>::reset(void )
    {
      cmap_.reset();
    }

    /* A visitor for printing a dgraph */
    template<typename T>
    class PrintDGraphVisitor : public DGraphVisitor<T>{
      public:
        PrintDGraphVisitor(DGraph<T> &g, typename DGraph<T>::ColorMap &cmap);
        void on_node(const typename DGraph<T>::vertex_key &v, T &val);
    };

    template<typename T>
    PrintDGraphVisitor<T>::PrintDGraphVisitor(DGraph<T> &g, typename DGraph<T>::ColorMap &cmap):DGraphVisitor<T>(g, cmap)
    {
    }

    template<typename T>
    void PrintDGraphVisitor<T>::on_node(const typename DGraph<T>::vertex_key &v, T &val)
    {
      DGraphVisitor<T>::on_node(v, val);
      std::cout << val << ",";
    }

    /* A visitor for validating nodes in a graph.
     * This visitor can be used to make sure that we don't have duplicate
     * visits (in a traversal)
     */
    template<typename T>
    class NodeValidatorVisitor : public DGraphVisitor<T>{
      public:
        NodeValidatorVisitor(DGraph<T> &g, typename DGraph<T>::ColorMap &cmap);
        virtual void on_node(const typename DGraph<T>::vertex_key &v, T &val);
        bool  has_dup_visits(void ) const;
        int nnodes_visited(void ) const;
      private:
        bool has_dup_visits_;
        int nnodes_visited_;
        std::map<typename DGraph<T>::vertex_key, bool> visited_nodes_;
    };

    template<typename T>
    NodeValidatorVisitor<T>::NodeValidatorVisitor(DGraph<T> &g,
      typename DGraph<T>::ColorMap &cmap):DGraphVisitor<T>(g, cmap), has_dup_visits_(false), nnodes_visited_(0)
    {
    }

    template<typename T>
    void NodeValidatorVisitor<T>::on_node(const typename DGraph<T>::vertex_key &v,T &val)
    {
      DGraphVisitor<T>::on_node(v, val);
      nnodes_visited_++;
      std::pair<typename std::map<typename DGraph<T>::vertex_key, bool>::iterator, bool> res = visited_nodes_.insert(std::make_pair(v, true));
      has_dup_visits_ = has_dup_visits_ || !res.second; 
    }

    template<typename T>
    bool NodeValidatorVisitor<T>::has_dup_visits(void ) const
    {
      return has_dup_visits_;
    }

    template<typename T>
    int NodeValidatorVisitor<T>::nnodes_visited(void ) const
    {
      return nnodes_visited_;
    }

    /* A visitor class that includes information on the parent of a
     * node
     */
    template<typename T>
    class DGraphPVisitor : public DGraphVisitor<T>{
      public:
        DGraphPVisitor(DGraph<T> &g, typename DGraph<T>::ColorMap &cmap);
        void on_node(const typename DGraph<T>::vertex_key &v,
                      T &val);
        virtual void on_node(const typename DGraph<T>::vertex_key &v,
                      const typename DGraph<T>::vertex_key &pv,
                      T &val, T &pval);
    };

    template<typename T>
    DGraphPVisitor<T>::DGraphPVisitor(DGraph<T> &g, typename DGraph<T>::ColorMap &cmap):DGraphVisitor<T>(g, cmap)
    {
    }

    template<typename T>
    void DGraphPVisitor<T>::on_node(const typename DGraph<T>::vertex_key &v, T &val)
    {
      DGraphVisitor<T>::on_node(v, val);
    }

    template<typename T>
    void DGraphPVisitor<T>::on_node(const typename DGraph<T>::vertex_key &v,
      const typename DGraph<T>::vertex_key &pv, T &val, T &pval)
    {
      DGraphVisitor<T>::on_node(v, val);
    }

    /* Graph algorithms : All traversal algorithms call the appropriate
     * trigger functions (on_node etc) on the visitor provided by the
     * user
     */
    /* Breadth first search algorithm using regular Dgraph visitors */
    template<typename T>
    void DGraph_BFS(DGraph<T> &g, DGraphVisitor<T> &vis, const typename DGraph<T>::vertex_key &v)
    {
      std::queue<typename DGraph<T>::vertex_key> nodes;
      if(!vis.has_visited(v)){
        nodes.push(v);
      }
      while(!nodes.empty()){
        typename DGraph<T>::vertex_key cv = nodes.front();
        nodes.pop();
        if(!vis.has_visited(cv)){
          vis.on_node(cv, g.get_val(cv));
        }
        std::vector<typename DGraph<T>::vertex_key> neighbors = g.get_neighbors(cv);
        for(typename std::vector<typename DGraph<T>::vertex_key>::iterator iter = neighbors.begin();
            iter != neighbors.end(); ++iter){
          if(!vis.has_visited(*iter)){
            nodes.push(*iter);
          }
        }
      }
    }

    /* Depth first search algorithm for dgraphs */
    template<typename T>
    void DGraph_DFS(DGraph<T> &g, DGraphVisitor<T> &vis, const typename DGraph<T>::vertex_key &v)
    {
      if(!vis.has_visited(v)){
        vis.on_node(v, g.get_val(v));
        std::vector<typename DGraph<T>::vertex_key> neighbors = g.get_neighbors(v);
        for(typename std::vector<typename DGraph<T>::vertex_key>::iterator iter = neighbors.begin();
            iter != neighbors.end(); ++iter){
          if(!vis.has_visited(*iter)){
            DGraph_DFS<T>(g, vis, *iter);
          }
        }
      }
    }

    /* Breadth first search algorithm using Dgraph visitors with information
     * on parents to each node
     */
    template<typename T>
    void DGraph_BFS(DGraph<T> &g, DGraphPVisitor<T> &vis, const typename DGraph<T>::vertex_key &v)
    {
      std::queue<typename DGraph<T>::vertex_key> nodes;
      std::queue<typename DGraph<T>::vertex_key> parents;
      if(!vis.has_visited(v)){
        nodes.push(v);
      }
      while(!nodes.empty()){
        typename DGraph<T>::vertex_key cv = nodes.front();
        nodes.pop();

        bool cv_has_parent = !parents.empty();
        if(cv_has_parent){
          typename DGraph<T>::vertex_key pcv = parents.front();
          parents.pop();
          if(!vis.has_visited(cv)){
            vis.on_node(cv, pcv, g.get_val(cv), g.get_val(pcv));
          }
        }
        else{
          if(!vis.has_visited(cv)){
            vis.on_node(cv, g.get_val(cv));
          }
        }
        std::vector<typename DGraph<T>::vertex_key> neighbors = g.get_neighbors(cv);
        for(typename std::vector<typename DGraph<T>::vertex_key>::iterator iter = neighbors.begin();
            iter != neighbors.end(); ++iter){
          if(!vis.has_visited(*iter)){
            nodes.push(*iter);
            parents.push(cv);
          }
        }
      }
    }

    /* A modified breadth first search algorithm where on_node() trigger
     * is not called for nodes that don't have parents
     */
    template<typename T>
    void DGraph_MBFS(DGraph<T> &g, DGraphPVisitor<T> &vis, const typename DGraph<T>::vertex_key &v)
    {
      std::queue<typename DGraph<T>::vertex_key> nodes;
      std::queue<typename DGraph<T>::vertex_key> parents;
      if(!vis.has_visited(v)){
        nodes.push(v);
      }
      while(!nodes.empty()){
        typename DGraph<T>::vertex_key cv = nodes.front();
        nodes.pop();

        bool cv_has_parent = !parents.empty();
        if(cv_has_parent){
          typename DGraph<T>::vertex_key pcv = parents.front();
          parents.pop();
          if(!vis.has_visited(cv)){
            vis.on_node(cv, pcv, g.get_val(cv), g.get_val(pcv));
          }
        }
        else{
          /*
          if(!vis.has_visited(cv)){
            vis.on_node(cv, g.get_val(cv));
          }
          */
        }
        std::vector<typename DGraph<T>::vertex_key> neighbors = g.get_neighbors(cv);
        for(typename std::vector<typename DGraph<T>::vertex_key>::iterator iter = neighbors.begin();
            iter != neighbors.end(); ++iter){
          if(!vis.has_visited(*iter) && (*iter != cv)){
            nodes.push(*iter);
            parents.push(cv);
          }
        }
      }
    }

  } // namespace MapperUtil
} // namespace ESMCI

#endif //ESMCI_GraphUtils_H
