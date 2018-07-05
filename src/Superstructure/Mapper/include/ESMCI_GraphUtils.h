#ifndef ESMCI_GraphUtils_H
#define ESMCI_GraphUtils_H

#include <vector>
#include <queue>
#include "ESMCI_Graph.h"

namespace ESMCI{
  namespace MapperUtil{

    template<typename T>
    class BFSVisitor{
      public:
        BFSVisitor(DGraph<T> &g, typename DGraph<T>::ColorMap &cmap);
        virtual void on_node(const typename DGraph<T>::vertex_key &v, T &val);
        bool has_visited(const typename DGraph<T>::vertex_key &v);
        void reset(void );
      private:
        DGraph<T> &g_;
        typename DGraph<T>::ColorMap &cmap_;
    };

    template<typename T>
    BFSVisitor<T>::BFSVisitor(DGraph<T> &g, typename DGraph<T>::ColorMap &cmap):g_(g),cmap_(cmap)
    {
      cmap_.reset();
    }

    template<typename T>
    void BFSVisitor<T>::on_node(const typename DGraph<T>::vertex_key &v, T &val)
    {
      cmap_.set(v);
    }

    template<typename T>
    bool BFSVisitor<T>::has_visited(const typename DGraph<T>::vertex_key &v)
    {
      return cmap_.is_set(v);
    }

    template<typename T>
    void BFSVisitor<T>::reset(void )
    {
      cmap_.reset();
    }

    template<typename T>
    class PrintBFSVisitor : public BFSVisitor<T>{
      public:
        PrintBFSVisitor(DGraph<T> &g, typename DGraph<T>::ColorMap &cmap);
        void on_node(const typename DGraph<T>::vertex_key &v, T &val);
    };

    template<typename T>
    PrintBFSVisitor<T>::PrintBFSVisitor(DGraph<T> &g, typename DGraph<T>::ColorMap &cmap):BFSVisitor<T>(g, cmap)
    {
    }

    template<typename T>
    void PrintBFSVisitor<T>::on_node(const typename DGraph<T>::vertex_key &v, T &val)
    {
      BFSVisitor<T>::on_node(v, val);
      std::cout << val << ",";
    }

    template<typename T>
    void DGraph_BFS(DGraph<T> &g, BFSVisitor<T> &vis, const typename DGraph<T>::vertex_key &v)
    {
      std::queue<typename DGraph<T>::vertex_key> nodes;
      if(!vis.has_visited(v)){
        nodes.push(v);
      }
      while(!nodes.empty()){
        typename DGraph<T>::vertex_key cv = nodes.front();
        nodes.pop();
        vis.on_node(cv, g.get_val(cv));
        std::vector<typename DGraph<T>::vertex_key> neighbors = g.get_neighbors(cv);
        for(typename std::vector<typename DGraph<T>::vertex_key>::iterator iter = neighbors.begin();
            iter != neighbors.end(); ++iter){
          if(!vis.has_visited(*iter)){
            nodes.push(*iter);
          }
        }
      }
    }

  } // namespace MapperUtil
} // namespace ESMCI

#endif //ESMCI_GraphUtils_H
