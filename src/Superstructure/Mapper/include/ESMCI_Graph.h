#ifndef ESMCI_Graph_H
#define ESMCI_Graph_H

#include <vector>
#include <cassert>
#include <sstream>
#include "ESMCI_GraphDotUtils.h"

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
        class GraphNodeIterator{
          public:
            friend class DGraph;
            GraphNodeIterator &operator++();
            GraphNodeIterator operator++(int );
            bool operator==(const GraphNodeIterator &other) const;
            bool operator!=(const GraphNodeIterator &other) const;
            /* vertex_key get_vertex_key(void ) const; */
            vertex_key &operator*();
          private:
            GraphNodeIterator(
              const DGraph<T> &g,
              typename std::vector<typename DGraph<T>::GraphNode>::const_iterator iter_begin,
              typename std::vector<typename DGraph<T>::GraphNode>::const_iterator iter_end,
              const vertex_key vkey_begin);
            const DGraph<T> &g_;
            typename std::vector<typename DGraph<T>::GraphNode>::const_iterator iter_;
            typename std::vector<typename DGraph<T>::GraphNode>::const_iterator iter_begin_;
            typename std::vector<typename DGraph<T>::GraphNode>::const_iterator iter_end_;
            int vkey_;
        };
        typedef GraphNodeIterator vertex_iterator;
        friend class GraphNodeIterator;
        DGraph();
        vertex_key add_node(const T& val);
        void rem_node(const vertex_key &v);
        void melt_node(const vertex_key &v);
        void fuse_nodes(const vertex_key &v1, const vertex_key &v2);
        void add_edge(const vertex_key &from, const vertex_key &to);
        void rem_edge(const vertex_key &from, const vertex_key &to);
        std::vector<vertex_key> get_neighbors(const vertex_key &v) const;
        std::vector<vertex_key> get_parents(const vertex_key &v) const;
        T &get_val(const vertex_key &v);
        std::size_t size(void ) const;
        ColorMap create_color_map(void ) const;
        vertex_iterator begin(void ) const;
        vertex_iterator end(void ) const;
        void print_to_file(const std::string &fname) const;
        DGraph<T> inverse(void ) const;
        vertex_key add_root(const T& val);
      private:
        class GraphNode{
          public:
            typedef std::vector<vertex_key>::const_iterator vertex_key_iterator;
            GraphNode(const T &val);
            T &get_val(void );
            void add_iedge(const vertex_key &from);
            void add_oedge(const vertex_key &to);
            bool rem_iedge(const vertex_key &from);
            bool rem_oedge(const vertex_key &to);
            vertex_key_iterator ibegin(void ) const;
            vertex_key_iterator iend(void ) const;
            vertex_key_iterator obegin(void ) const;
            vertex_key_iterator oend(void ) const;
            std::vector<vertex_key> get_neighbors(void ) const;
            std::vector<vertex_key> get_parents(void ) const;
            bool has_neighbors(void ) const;
            bool is_valid(void ) const;
            void mark_invalid(void );
            std::string to_string(void ) const;
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

    // GraphNode iterator definitions
    template<typename T>
    DGraph<T>::GraphNodeIterator::GraphNodeIterator(
      const DGraph<T> &g,
      typename std::vector<GraphNode>::const_iterator iter_begin,
      typename std::vector<GraphNode>::const_iterator iter_end,
      const DGraph<T>::vertex_key vkey_begin):
        g_(g),
        iter_(iter_begin), iter_begin_(iter_begin), iter_end_(iter_end),
        vkey_(vkey_begin)
    {
      /* Skip invalid nodes */
      while((iter_ != iter_end) && !g_.is_valid(vkey_)){
        ++iter_;
        vkey_++;
      }
    }

    template<typename T>
    typename DGraph<T>::GraphNodeIterator &DGraph<T>::GraphNodeIterator::operator++()
    {
      ++iter_;
      vkey_++;
      /* Skip invalid nodes */
      while((iter_ != iter_end_) && !g_.is_valid(vkey_)){
        ++iter_;
        vkey_++;
      }

      return *this;
    }

    template<typename T>
    typename DGraph<T>::GraphNodeIterator DGraph<T>::GraphNodeIterator::operator++(int )
    {
      ++iter_;
      vkey_++;
      /* Skip invalid nodes */
      while((iter_ != iter_end_) && !g_.is_valid(vkey_)){
        ++iter_;
        vkey_++;
      }

      return *this;
    }

    template<typename T>
    bool DGraph<T>::GraphNodeIterator::operator==(const typename DGraph<T>::GraphNodeIterator &other) const
    {
      return (iter_ == other.iter_);
    }

    template<typename T>
    bool DGraph<T>::GraphNodeIterator::operator!=(const typename DGraph<T>::GraphNodeIterator &other) const
    {
      return (iter_ != other.iter_);
    }

    /*
    template<typename T>
    DGraph<T>::vertex_key get_vertex_key(void ) const
    {
      return vkey_;
    }

    template<typename T>
    DGraph<T>::GraphNode &operator*()
    {
      return nodes_[vkey_];
    }
    */
    template<typename T>
    typename DGraph<T>::vertex_key &DGraph<T>::GraphNodeIterator::operator*()
    {
      assert(g_.is_valid(vkey_));
      return vkey_;
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
    bool DGraph<T>::GraphNode::rem_iedge(const vertex_key &from)
    {
      bool edge_removed = false;
      vertex_key_iterator iter = iedges_.begin();
      for(;iter != iedges_.end(); ++iter){
        if(*iter == from){
          break;
        }
      }
      if(iter != iedges_.end()){
        iedges_.erase(iter);
        edge_removed = true;
      }
      return edge_removed;
    }

    template<typename T>
    bool DGraph<T>::GraphNode::rem_oedge(const vertex_key &to)
    {
      bool edge_removed = false;
      vertex_key_iterator iter = oedges_.begin();
      for(;iter != oedges_.end(); ++iter){
        if(*iter == to){
          break;
        }
      }
      if(iter != oedges_.end()){
        oedges_.erase(iter);
        edge_removed = true;
      }
      return edge_removed;
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
    std::vector<typename DGraph<T>::vertex_key> DGraph<T>::GraphNode::get_parents(void ) const
    {
      return iedges_;
    }

    template<typename T>
    bool DGraph<T>::GraphNode::has_neighbors(void ) const
    {
      return !(oedges_.empty());
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

    template<typename T>
    std::string DGraph<T>::GraphNode::to_string(void ) const
    {
      std::ostringstream ostr;
      ostr << val_;
      return ostr.str();
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
      assert(is_valid(v1) && is_valid(v2));
      if(v1 == v2){
        return;
      }
      for(typename std::vector<DGraph<T>::GraphNode>::iterator iter = nodes_.begin();
          iter != nodes_.end(); ++iter){
        bool edge_removed;
        edge_removed = iter->rem_iedge(v2);
        if(edge_removed){
          iter->add_iedge(v1);
        }
        edge_removed = iter->rem_oedge(v2);
        if(edge_removed){
          iter->add_oedge(v1);
        }
      }
      // Mark node as invalid
      nodes_[v2].mark_invalid();
      nvalid_nodes_--;
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
      if(is_valid(v)){
        return nodes_[v].get_neighbors();
      }
      else{
        std::vector<typename DGraph<T>::vertex_key> tmp_vec;
        return tmp_vec;
      }
    }

    template<typename T>
    std::vector<typename DGraph<T>::vertex_key> DGraph<T>::get_parents(const vertex_key &v) const
    {
      if(is_valid(v)){
        return nodes_[v].get_parents();
      }
      else{
        std::vector<typename DGraph<T>::vertex_key> tmp_vec;
        return tmp_vec;
      }
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
    typename DGraph<T>::vertex_iterator DGraph<T>::begin(void ) const
    {
      return GraphNodeIterator(*this, nodes_.cbegin(), nodes_.cend(), 0);
    }

    template<typename T>
    typename DGraph<T>::vertex_iterator DGraph<T>::end(void ) const
    {
      return GraphNodeIterator(*this, nodes_.cend(), nodes_.cend(), nodes_.size());
    }

    template<typename T>
    void DGraph<T>::print_to_file(const std::string &fname) const
    {
      DGraphPrinter out_str(fname);
      for(typename std::vector<GraphNode>::const_iterator cniter = nodes_.cbegin();
          cniter != nodes_.cend(); ++cniter){
        if(cniter->is_valid()){
          std::string val1 = cniter->to_string();
          for(typename GraphNode::vertex_key_iterator oiter = cniter->obegin();
                oiter != cniter->oend(); ++oiter){
            std::string val2 = nodes_[*oiter].to_string();
            out_str.print_edge(val1, val2);
          }
        }
      }
    }

    template<typename T>
    DGraph<T> DGraph<T>::inverse(void ) const
    {
      DGraph<T> g = *this;
      for(typename std::vector<GraphNode>::iterator iter = g.nodes_.begin();
          iter != g.nodes_.end(); ++iter){
        if((*iter).is_valid()){
          /* Cache input and output edges for the node */
          std::vector<vertex_key> onodes;
          std::vector<vertex_key> inodes;
          for(typename GraphNode::vertex_key_iterator oiter = (*iter).obegin();
                oiter != (*iter).oend(); ++oiter){
            onodes.push_back(*oiter);
          }
          for(typename GraphNode::vertex_key_iterator iiter = (*iter).ibegin();
                iiter != (*iter).iend(); ++iiter){
            inodes.push_back(*iiter);
          }

          /* Delete input and output edges for the node */
          for(std::vector<vertex_key>::const_iterator citer = onodes.cbegin();
                citer != onodes.cend(); ++citer){
            (*iter).rem_oedge(*citer);
          }
          for(std::vector<vertex_key>::const_iterator citer = inodes.cbegin();
                citer != inodes.cend(); ++citer){
            (*iter).rem_iedge(*citer);
          }

          /* Add outgoing edges as incoming and vice versa */
          for(std::vector<vertex_key>::const_iterator citer = onodes.cbegin();
                citer != onodes.cend(); ++citer){
            (*iter).add_iedge(*citer);
          }
          for(std::vector<vertex_key>::const_iterator citer = inodes.cbegin();
                citer != inodes.cend(); ++citer){
            (*iter).add_oedge(*citer);
          }
        }
      }
      return g;
    }

    template<typename T>
    typename DGraph<T>::vertex_key DGraph<T>::add_root(const T &val)
    {
      nodes_.push_back(GraphNode(val));
      nvalid_nodes_++;

      vertex_key val_vkey = nodes_.size() - 1;
      vertex_key node_vkey = 0;
      /* This new node is the root for all the existing roots */
      for(typename std::vector<GraphNode>::iterator iter = nodes_.begin();
          iter != nodes_.end(); ++iter, node_vkey++){
        if(node_vkey == val_vkey){
          break;
        }
        if((*iter).is_valid() && !((*iter).has_neighbors())){
          (*iter).add_oedge(val_vkey);
          nodes_[val_vkey].add_iedge(node_vkey);
        }
      }

      return val_vkey;
    }

    template<typename T>
    std::size_t DGraph<T>::get_capacity(void ) const
    {
      return nodes_.size();
    }
  } //namespace MapperUtil
} //namespace ESMCI

#endif // ESMCI_Graph_H
