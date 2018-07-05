#ifndef ESMCI_Graph_H
#define ESMCI_Graph_H

#include <vector>

namespace ESMCI{
  namespace MapperUtil{

    template<typename T>
    class DGraph{
      public:
        typedef int vertex_key;
        class ColorMap{
          public:
            ColorMap(const DGraph<T> &g);
            void set(const vertex_key &v);
            bool is_set(const vertex_key &v);
            void unset(const vertex_key &v);
            void reset(void );
          private:
            std::vector<bool> cmap_;
        };
        DGraph();
        vertex_key add_node(const T& val);
        void rem_node(const vertex_key &v);
        void melt_node(const vertex_key &v);
        void fuse_nodes(const vertex_key &v1, const vertex_key &v2);
        void add_edge(const vertex_key &from, const vertex_key &to);
        void rem_edge(const vertex_key &from, const vertex_key &to);
        std::vector<vertex_key> get_neighbors(const vertex_key &v) const;
        T &get_val(const vertex_key &v);
        std::size_t size(void ) const;
        ColorMap create_color_map(void ) const;
      private:
        class GraphNode{
          public:
            typedef std::vector<vertex_key>::iterator vertex_key_iterator;
            GraphNode(const T &val);
            T &get_val(void );
            void add_iedge(const vertex_key &from);
            void add_oedge(const vertex_key &to);
            void rem_iedge(const vertex_key &from);
            void rem_oedge(const vertex_key &to);
            vertex_key_iterator ibegin(void ) const;
            vertex_key_iterator iend(void ) const;
            vertex_key_iterator obegin(void ) const;
            vertex_key_iterator oend(void ) const;
            std::vector<vertex_key> get_neighbors(void ) const;
            bool is_valid(void ) const;
            void mark_invalid(void );
          private:
            T val_;
            bool is_valid_;
            std::vector<vertex_key> iedges_;
            std::vector<vertex_key> oedges_;
        };
        bool is_valid(const vertex_key &v) const;
        std::size_t get_capacity(void ) const;
        std::vector<GraphNode> nodes_;
        std::size_t nvalid_nodes_;
    };

    // ColorMap definitions
    template<typename T>
    void DGraph<T>::ColorMap::set(const vertex_key &v)
    {
      assert((v >= 0) && (v < static_cast<int>(cmap_.size())));
      cmap_[v] = true;
    }

    template<typename T>
    bool DGraph<T>::ColorMap::is_set(const vertex_key &v)
    {
      assert((v >= 0) && (v < static_cast<int>(cmap_.size())));
      return cmap_[v];
    }

    template<typename T>
    void DGraph<T>::ColorMap::unset(const vertex_key &v)
    {
      assert((v >= 0) && (v < static_cast<int>(cmap_.size())));
      cmap_[v] = false;
    }

    template<typename T>
    void DGraph<T>::ColorMap::reset(void )
    {
      cmap_.assign(cmap_.size(), false);
    }

    template<typename T>
    DGraph<T>::ColorMap::ColorMap(const DGraph<T> &g)
    {
      cmap_.resize(g.get_capacity(), false);
    }

    // GraphNode definitions
    template<typename T>
    DGraph<T>::GraphNode::GraphNode(const T &val):val_(val), is_valid_(true)
    {
    }

    template<typename T>
    T &DGraph<T>::GraphNode::get_val(void )
    {
      return val_;
    }

    template<typename T>
    void DGraph<T>::GraphNode::add_iedge(const vertex_key &from)
    {
      iedges_.push_back(from);
    }

    template<typename T>
    void DGraph<T>::GraphNode::add_oedge(const vertex_key &to)
    {
      oedges_.push_back(to);
    }

    template<typename T>
    void DGraph<T>::GraphNode::rem_iedge(const vertex_key &from)
    {
      vertex_key_iterator iter = iedges_.begin();
      for(;iter != iedges_.end(); ++iter){
        if(*iter == from){
          break;
        }
      }
      if(iter != iedges_.end()){
        iedges_.erase(iter);
      }
    }

    template<typename T>
    void DGraph<T>::GraphNode::rem_oedge(const vertex_key &to)
    {
      vertex_key_iterator iter = oedges_.begin();
      for(;iter != oedges_.end(); ++iter){
        if(*iter == to){
          break;
        }
      }
      if(iter != oedges_.end()){
        oedges_.erase(iter);
      }
    }

    template<typename T>
    typename DGraph<T>::GraphNode::vertex_key_iterator DGraph<T>::GraphNode::ibegin(void ) const
    {
      return iedges_.begin();
    }

    template<typename T>
    typename DGraph<T>::GraphNode::vertex_key_iterator DGraph<T>::GraphNode::iend(void ) const
    {
      return iedges_.end();
    }

    template<typename T>
    typename DGraph<T>::GraphNode::vertex_key_iterator DGraph<T>::GraphNode::obegin(void ) const
    {
      return oedges_.begin();
    }

    template<typename T>
    typename DGraph<T>::GraphNode::vertex_key_iterator DGraph<T>::GraphNode::oend(void ) const
    {
      return oedges_.end();
    }

    template<typename T>
    std::vector<typename DGraph<T>::vertex_key> DGraph<T>::GraphNode::get_neighbors(void ) const
    {
      return oedges_;
    }

    template<typename T>
    bool DGraph<T>::GraphNode::is_valid(void ) const
    {
      return is_valid_;
    }

    template<typename T>
    void DGraph<T>::GraphNode::mark_invalid(void )
    {
      is_valid_ = false;
    }

    // DGraph functions
    template<typename T>
    DGraph<T>::DGraph():nvalid_nodes_(0)
    {
    }
    
    template<typename T>
    bool DGraph<T>::is_valid(const DGraph<T>::vertex_key &v) const
    {
      if((v < 0) || (v > static_cast<int>(nodes_.size()))){
        return false;
      }
      return nodes_[v].is_valid();
    }

    template<typename T>
    typename DGraph<T>::vertex_key DGraph<T>::add_node(const T &val)
    {
      nodes_.push_back(GraphNode(val));
      nvalid_nodes_++;
      return nodes_.size() - 1;
    }

    template<typename T>
    void DGraph<T>::rem_node(const DGraph<T>::vertex_key &v)
    {
      assert(is_valid(v));
      // Delete edges from/to this node
      for(typename std::vector<DGraph<T>::GraphNode>::iterator iter = nodes_.begin();
          iter != nodes_.end(); ++iter){
        iter->rem_iedge(v);
        iter->rem_oedge(v);
      }
      // Mark node as invalid
      nodes_[v].mark_invalid();
      nvalid_nodes_--;
    }

    template<typename T>
    void DGraph<T>::melt_node(const vertex_key &v)
    {
      // Not implemented yet
      assert(0);
    }

    template<typename T>
    void DGraph<T>::fuse_nodes(const vertex_key &v1, const vertex_key &v2)
    {
      // Not implemented yet
      assert(0);
    }

    template<typename T>
    void DGraph<T>::add_edge(const DGraph<T>::vertex_key &from,
                              const DGraph<T>::vertex_key &to)
    {
      assert(is_valid(from) && is_valid(to));

      nodes_[from].add_oedge(to);
      nodes_[to].add_iedge(from);
    }

    template<typename T>
    void DGraph<T>::rem_edge(const DGraph<T>::vertex_key &from,
                              const DGraph<T>::vertex_key &to)
    {
      assert(is_valid(from) && is_valid(to));

      nodes_[from].rem_oedge(to);
      nodes_[to].rem_iedge(from);
    }

    template<typename T>
    std::vector<typename DGraph<T>::vertex_key> DGraph<T>::get_neighbors(const vertex_key &v) const
    {
      assert(is_valid(v));
      return nodes_[v].get_neighbors();
    }

    template<typename T>
    T &DGraph<T>::get_val(const vertex_key &v)
    {
      assert(is_valid(v));
      return nodes_[v].get_val();
    }    

    template<typename T>
    std::size_t DGraph<T>::size(void ) const
    {
      return nvalid_nodes_;
    }    

    template<typename T>
    typename DGraph<T>::ColorMap DGraph<T>::create_color_map(void ) const
    {
      return (ColorMap(*this));
    }

    template<typename T>
    std::size_t DGraph<T>::get_capacity(void ) const
    {
      return nodes_.size();
    }
  } //namespace MapperUtil
} //namespace ESMCI

#endif // ESMCI_Graph_H
