// $Id: ESMC_Tree.h,v 1.1.2.2 2009/01/21 21:25:22 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.


// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

/*
Red-black tree class, designed for use in implementing STL
associative containers (set, multiset, map, and multimap). The
insertion and deletion algorithms are based on those in Cormen,
Leiserson, and Rivest, Introduction to Algorithms (MIT Press, 1990),
except that

(1) the header cell is maintained with links not only to the root
but also to the leftmost node of the tree, to enable constant time
begin(), and to the rightmost node of the tree, to enable linear time
performance when used with the generic set algorithms (set_union,
etc.);

(2) when a node being deleted has two children its successor node is
relinked into its place, rather than copied, so that the only
iterators invalidated are those referring to the deleted node.

*/

/*
 *
 * Copyright (c) 1996
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 *
 * Copyright (c) 1994
 * Hewlett-Packard Company
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Hewlett-Packard Company makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 *
 */

#ifndef ESMC_Tree_h
#define ESMC_Tree_h


#include <ESMC_Exception.h>

#include <cstddef>
#include <algorithm>
#include <iterator>
#include <functional>

namespace ESMCI {
namespace MESH {

typedef bool _Tree_color_type;
const _Tree_color_type _Tree_red = false;
const _Tree_color_type _Tree_black = true;

// Class to be 'Listified' inherits from TreeNode. It 
// must implement the KeyOfValue function, which returns a key.


/**
 * Basic tree object derives from this node class.
*/
template <class Key>
class TreeNode
{
public:
  typedef _Tree_color_type color_type;
  typedef TreeNode* base_ptr;
  typedef base_ptr link_type;
  
//protected:
  TreeNode(Key _key) : key(_key) {} 
  ~TreeNode() {}
//private:

  color_type color; 
  base_ptr parent;
  base_ptr left;
  base_ptr right;

  static base_ptr minimum(base_ptr x)
  {
    while (x->left != 0) x = x->left;
    return x;
  }

  static base_ptr maximum(base_ptr x)
  {
    while (x->right != 0) x = x->right;
    return x;
  }

  void clear() {
    parent = left = right = NULL;
  }

  const Key& KeyOfValue() const { return key;}
  Key key;

  private:
  TreeNode(const TreeNode &);
  TreeNode &operator=(const TreeNode &);
};


template <class Key>
struct _Tree_base_iterator
{
  typedef typename TreeNode<Key>::base_ptr base_ptr;
  typedef std::bidirectional_iterator_tag iterator_category;
  typedef std::ptrdiff_t difference_type;
  base_ptr node;

  void increment()
  {
    if (node->right != 0) {
      node = node->right;
      while (node->left != 0)
        node = node->left;
    }
    else {
      base_ptr y = node->parent;
      while (node == y->right) {
        node = y;
        y = y->parent;
      }
      if (node->right != y)
        node = y;
    }
  }

  void decrement()
  {
    if (node->color == _Tree_red &&
        node->parent->parent == node)
      node = node->right;
    else if (node->left != 0) {
      base_ptr y = node->left;
      while (y->right != 0)
        y = y->right;
      node = y;
    }
    else {
      base_ptr y = node->parent;
      while (node == y->left) {
        node = y;
        y = y->parent;
      }
      node = y;
    }
  }
};

template <class _T, typename Ref, typename Ptr, typename Key>
struct _Tree_iterator : public _Tree_base_iterator<Key>
{
  typedef _T value_type;
  typedef Ref reference;
  typedef Ptr pointer;
  typedef _Tree_iterator             iterator;
  typedef _Tree_iterator<_T, const _T&, const _T*, Key> const_iterator;
  typedef _Tree_iterator<_T, Ref, Ptr, Key>                   self;
  typedef TreeNode<Key>* link_type;

  _Tree_iterator() {}
  _Tree_iterator(link_type x) { this->node = x; }
  _Tree_iterator(const iterator& it) { this->node = it.node; }

  reference operator*() const { return static_cast<reference>(*this->node); }
  pointer operator->() const { return &(operator*()); }

  self& operator++() { this->increment(); return *this; }
  self operator++(int) {
    self tmp = *this;
    this->increment();
    return tmp;
  }
    
  self& operator--() { this->decrement(); return *this; }
  self operator--(int) {
    self tmp = *this;
    this->decrement();
    return tmp;
  }
};

template <typename Key>
inline bool operator==(const _Tree_base_iterator<Key>& x,
                       const _Tree_base_iterator<Key>& y) {
  return x.node == y.node;
}

template <typename Key>
inline bool operator!=(const _Tree_base_iterator<Key>& x,
                       const _Tree_base_iterator<Key>& y) {
  return x.node != y.node;
}

template <typename Key>
inline void 
_Tree_rotate_left(TreeNode<Key>* x, TreeNode<Key>*& root)
{
  TreeNode<Key>* y = x->right;
  x->right = y->left;
  if (y->left !=0)
    y->left->parent = x;
  y->parent = x->parent;

  if (x == root)
    root = y;
  else if (x == x->parent->left)
    x->parent->left = y;
  else
    x->parent->right = y;
  y->left = x;
  x->parent = y;
}

template <typename Key>
inline void 
_Tree_rotate_right(TreeNode<Key>* x, TreeNode<Key>*& root)
{
  TreeNode<Key>* y = x->left;
  x->left = y->right;
  if (y->right != 0)
    y->right->parent = x;
  y->parent = x->parent;

  if (x == root)
    root = y;
  else if (x == x->parent->right)
    x->parent->right = y;
  else
    x->parent->left = y;
  y->right = x;
  x->parent = y;
}

template <typename Key>
inline void 
_Tree_rebalance(TreeNode<Key>* x, TreeNode<Key>*& root)
{
  x->color = _Tree_red;
  while (x != root && x->parent->color == _Tree_red) {
    if (x->parent == x->parent->parent->left) {
      TreeNode<Key>* y = x->parent->parent->right;
      if (y && y->color == _Tree_red) {
        x->parent->color = _Tree_black;
        y->color = _Tree_black;
        x->parent->parent->color = _Tree_red;
        x = x->parent->parent;
      }
      else {
        if (x == x->parent->right) {
          x = x->parent;
          _Tree_rotate_left(x, root);
        }
        x->parent->color = _Tree_black;
        x->parent->parent->color = _Tree_red;
        _Tree_rotate_right(x->parent->parent, root);
      }
    }
    else {
      TreeNode<Key>* y = x->parent->parent->left;
      if (y && y->color == _Tree_red) {
        x->parent->color = _Tree_black;
        y->color = _Tree_black;
        x->parent->parent->color = _Tree_red;
        x = x->parent->parent;
      }
      else {
        if (x == x->parent->left) {
          x = x->parent;
          _Tree_rotate_right(x, root);
        }
        x->parent->color = _Tree_black;
        x->parent->parent->color = _Tree_red;
        _Tree_rotate_left(x->parent->parent, root);
      }
    }
  }
  root->color = _Tree_black;
}

template <typename Key>
inline TreeNode<Key>*
_Tree_rebalance_for_erase(TreeNode<Key>* z,
                              TreeNode<Key>*& root,
                              TreeNode<Key>*& leftmost,
                              TreeNode<Key>*& rightmost)
{
  TreeNode<Key>* y = z;
  TreeNode<Key>* x = 0;
  TreeNode<Key>* x_parent = 0;
  if (y->left == 0)             // z has at most one non-null child. y == z.
    x = y->right;               // x might be null.
  else
    if (y->right == 0)          // z has exactly one non-null child.  y == z.
      x = y->left;              // x is not null.
    else {                      // z has two non-null children.  Set y to
      y = y->right;             //   z's successor.  x might be null.
      while (y->left != 0)
        y = y->left;
      x = y->right;
    }
  if (y != z) {                 // relink y in place of z.  y is z's successor
    z->left->parent = y; 
    y->left = z->left;
    if (y != z->right) {
      x_parent = y->parent;
      if (x) x->parent = y->parent;
      y->parent->left = x;      // y must be a left child
      y->right = z->right;
      z->right->parent = y;
    }
    else
      x_parent = y;  
    if (root == z)
      root = y;
    else if (z->parent->left == z)
      z->parent->left = y;
    else 
      z->parent->right = y;
    y->parent = z->parent;
    std::swap(y->color, z->color);
    y = z;
    // y now points to node to be actually deleted
  }
  else {                        // y == z
    x_parent = y->parent;
    if (x) x->parent = y->parent;   
    if (root == z)
      root = x;
    else 
      if (z->parent->left == z)
        z->parent->left = x;
      else
        z->parent->right = x;
    if (leftmost == z) 
      if (z->right == 0)        // z->left must be null also
        leftmost = z->parent;
    // makes leftmost == header if z == root
      else
        leftmost = TreeNode<Key>::minimum(x);
    if (rightmost == z)  
      if (z->left == 0)         // z->right must be null also
        rightmost = z->parent;  
    // makes rightmost == header if z == root
      else                      // x == z->left
        rightmost = TreeNode<Key>::maximum(x);
  }
  if (y->color != _Tree_red) { 
    while (x != root && (x == 0 || x->color == _Tree_black))
      if (x == x_parent->left) {
        TreeNode<Key>* w = x_parent->right;
        if (w->color == _Tree_red) {
          w->color = _Tree_black;
          x_parent->color = _Tree_red;
          _Tree_rotate_left(x_parent, root);
          w = x_parent->right;
        }
        if ((w->left == 0 || w->left->color == _Tree_black) &&
            (w->right == 0 || w->right->color == _Tree_black)) {
          w->color = _Tree_red;
          x = x_parent;
          x_parent = x_parent->parent;
        } else {
          if (w->right == 0 || w->right->color == _Tree_black) {
            if (w->left) w->left->color = _Tree_black;
            w->color = _Tree_red;
            _Tree_rotate_right(w, root);
            w = x_parent->right;
          }
          w->color = x_parent->color;
          x_parent->color = _Tree_black;
          if (w->right) w->right->color = _Tree_black;
          _Tree_rotate_left(x_parent, root);
          break;
        }
      } else {                  // same as above, with right <-> left.
        TreeNode<Key>* w = x_parent->left;
        if (w->color == _Tree_red) {
          w->color = _Tree_black;
          x_parent->color = _Tree_red;
          _Tree_rotate_right(x_parent, root);
          w = x_parent->left;
        }
        if ((w->right == 0 || w->right->color == _Tree_black) &&
            (w->left == 0 || w->left->color == _Tree_black)) {
          w->color = _Tree_red;
          x = x_parent;
          x_parent = x_parent->parent;
        } else {
          if (w->left == 0 || w->left->color == _Tree_black) {
            if (w->right) w->right->color = _Tree_black;
            w->color = _Tree_red;
            _Tree_rotate_left(w, root);
            w = x_parent->left;
          }
          w->color = x_parent->color;
          x_parent->color = _Tree_black;
          if (w->left) w->left->color = _Tree_black;
          _Tree_rotate_right(x_parent, root);
          break;
        }
      }
    if (x) x->color = _Tree_black;
  }
  return y;
}

/**
 * A red black tree, borrowed from the HP STL implementation.  Provides an
 * 'emedded' map object, avoiding the necessity for storing an extra pointer
 * to objects that we want to categorize; also allows for instance deletes of
 * objects, given pointers to them.
*/
template <class Key, class _T, class Compare = std::less<Key> >
class Tree {
protected:
    typedef void* void_pointer;
    typedef TreeNode<Key>* base_ptr;
    typedef _Tree_color_type color_type;
public:
    typedef Key key_type;
    typedef _T value_type;
    typedef value_type* pointer;
    typedef const value_type* const_pointer;
    typedef value_type& reference;
    typedef const value_type& const_reference;
    typedef TreeNode<Key>* link_type;
    typedef size_t size_type;
    typedef ptrdiff_t difference_type;
protected:
    link_type get_node() {
      return new TreeNode<Key>(Key());
    }
    void put_node(link_type p) {
      delete p;
    }

protected:
    size_type node_count; // keeps track of size of tree
    link_type header;  
    Compare key_compare;

    link_type& root() const { return (link_type&) header->parent; }
    link_type& leftmost() const { return (link_type&) header->left; }
    link_type& rightmost() const { return (link_type&) header->right; }

    static link_type& left(link_type x) { return (link_type&)(x->left); }
    static link_type& right(link_type x) { return (link_type&)(x->right); }
    static link_type& parent(link_type x) { return (link_type&)(x->parent); }
    static reference value(link_type x) { return static_cast<reference>(*x); }
    static const Key& key(link_type x) { return static_cast<reference>(*x).KeyOfValue(); }
    static color_type& color(link_type x) { return (color_type&)(x->color); }

    static link_type minimum(link_type x) { 
        return (link_type)  TreeNode<Key>::minimum(x);
    }
    static link_type maximum(link_type x) {
        return (link_type) TreeNode<Key>::maximum(x);
    }

public:
    typedef _Tree_iterator<value_type, reference, pointer, Key> iterator;
    typedef _Tree_iterator<value_type, const_reference, const_pointer, Key> 
            const_iterator;

    typedef std::reverse_iterator<const_iterator> const_reverse_iterator;
    typedef std::reverse_iterator<iterator> reverse_iterator;
private:
    iterator __insert(base_ptr x, base_ptr y, value_type& v);
    void __erase(link_type x);
    void init() {
        header = get_node();
        color(header) = _Tree_red; // used to distinguish header from 
                                       // root, in iterator.operator++
        root() = 0;
        leftmost() = header;
        rightmost() = header;
    }
public:
                                // allocation/deallocation
    Tree(const Compare& comp = Compare())
      : node_count(0), key_compare(comp) { init(); }

private:
    // disallow
    Tree(const Tree<Key, _T, Compare>& x); 
    Tree<Key, _T, Compare>& 
        operator=(const Tree<Key, _T, Compare>& x);
public:
    ~Tree() {
        clear();
        put_node(header);
    }
public:    
                                // accessors:
    Compare key_comp() const { return key_compare; }
    iterator begin() { return leftmost(); }
    const_iterator begin() const { return leftmost(); }
    iterator end() { return header; }
    const_iterator end() const { return header; }
    reverse_iterator rbegin() { return reverse_iterator(end()); }
    const_reverse_iterator rbegin() const { 
        return const_reverse_iterator(end()); 
    }
    reverse_iterator rend() { return reverse_iterator(begin()); }
    const_reverse_iterator rend() const { 
        return const_reverse_iterator(begin());
    } 
    bool empty() const { return node_count == 0; }
    size_type size() const { return node_count; }
    size_type max_size() const { return size_type(-1); }
    
public:
                                // insert/erase
    
    std::pair<iterator,bool> insert_unique(value_type& x);
    iterator insert_equal(value_type& x);

    std::pair<iterator,bool> insert(value_type& x) {
      return insert_unique(x);
    }

    iterator insert_unique(iterator position, value_type& x);
    iterator insert_equal(iterator position, value_type& x);

    iterator insert(iterator position, value_type& x) {
      return insert_unique(position, x);
    }

    template <class InputIterator>
    void insert_unique(InputIterator first, InputIterator last);
    template <class InputIterator>
    void insert_equal(InputIterator first, InputIterator last);

    void erase(iterator position);
    size_type erase(const key_type& x);
    void erase(iterator first, iterator last);
    void erase(const key_type* first, const key_type* last);
    void clear() {
      if (node_count != 0) {
        __erase(root());
        leftmost() = header;
        root() = 0;
        rightmost() = header;
        node_count = 0;
      }
    }      

public:
                                // set operations:
    iterator find(const key_type& x);
    const_iterator find(const key_type& x) const;
    size_type count(const key_type& x) const;
    iterator lower_bound(const key_type& x);
    const_iterator lower_bound(const key_type& x) const;
    iterator upper_bound(const key_type& x);
    const_iterator upper_bound(const key_type& x) const;
    std::pair<iterator,iterator> equal_range(const key_type& x);
    std::pair<const_iterator, const_iterator> equal_range(const key_type& x) const;

public:

                                // Debugging.
  bool __rb_verify() const;
};

template <class Key, class _T, class Compare>
inline bool operator==(const Tree<Key, _T, Compare>& x, 
                       const Tree<Key, _T, Compare>& y) {
    return x.size() == y.size() && equal(x.begin(), x.end(), y.begin());
}

template <class Key, class _T, class Compare>
inline bool operator<(const Tree<Key, _T, Compare>& x, 
                      const Tree<Key, _T, Compare>& y) {
    return std::lexicographical_compare(x.begin(), x.end(), y.begin(), y.end());
}

template <class Key, class _T, class Compare>
typename Tree<Key, _T, Compare>::iterator
Tree<Key, _T, Compare>::
__insert(base_ptr x_, base_ptr y_, _T& v) {
  link_type x = (link_type) x_;
  link_type y = (link_type) y_;
  link_type z = &v;

  if (y == header || x != 0 || key_compare(v.KeyOfValue(), key(y))) {
    left(y) = z;                // also makes leftmost() = z when y == header
    if (y == header) {
      root() = z;
      rightmost() = z;
    }
    else if (y == leftmost())
      leftmost() = z;           // maintain leftmost() pointing to min node
  }
  else {
    right(y) = z;
    if (y == rightmost())
      rightmost() = z;          // maintain rightmost() pointing to max node
  }
  parent(z) = y;
  left(z) = 0;
  right(z) = 0;
  _Tree_rebalance(z, header->parent);
  ++node_count;
  return iterator(z);
}

template <class Key, class _T, class Compare>
typename Tree<Key, _T, Compare>::iterator
Tree<Key, _T, Compare>::insert_equal( value_type& v)
{
    link_type y = header;
    link_type x = root();
    while (x != 0) {
        y = x;
        x = key_compare(v.KeyOfValue(), key(x)) ? left(x) : right(x);
    }
    return __insert(x, y, v);
}


template <class Key, class _T, class Compare>
typename std::pair<typename Tree<Key, _T, Compare>::iterator, bool>
Tree<Key, _T, Compare>::insert_unique(value_type& v)
{
    link_type y = header;
    link_type x = root();
    bool comp = true;
    while (x != 0) {
        y = x;
        comp = key_compare(v.KeyOfValue(), key(x));
        x = comp ? left(x) : right(x);
    }
    iterator j = iterator(y);   
    if (comp)
        if (j == begin())     
            return std::pair<iterator,bool>(__insert(x, y, v), true);
        else
            --j;
    if (key_compare(key(j.node), v.KeyOfValue()))
        return std::pair<iterator,bool>(__insert(x, y, v), true);
    return std::pair<iterator,bool>(j, false);
}


template <class Key, class _T, class Compare>
typename Tree<Key, _T, Compare>::iterator 
Tree<Key, _T, Compare>::insert_unique(iterator position,
                                                             value_type& v) {
    if (position.node == header->left) // begin()
        if (size() > 0 && key_compare(v.KeyOfValue(), key(position.node)))
            return __insert(position.node, position.node, v);
                                // first argument just needs to be non-null 
        else
            return insert_unique(v).first;
    else if (position.node == header) // end()
        if (key_compare(key(rightmost()), v.KeyOfValue()))
            return __insert(0, rightmost(), v);
        else
            return insert_unique(v).first;
    else {
        iterator before = position;
        --before;
        if (key_compare(key(before.node), v.KeyOfValue())
            && key_compare(v.KeyOfValue(), key(position.node)))
            if (right(before.node) == 0)
                return __insert(0, before.node, v); 
            else
                return __insert(position.node, position.node, v);
                                // first argument just needs to be non-null 
        else
            return insert_unique(v).first;
    }
}

template <class Key, class _T, class Compare>
typename Tree<Key, _T, Compare>::iterator 
Tree<Key, _T, Compare>::insert_equal(iterator position,
                                                            value_type& v) {
    if (position.node == header->left) // begin()
        if (size() > 0 && key_compare(v.KeyOfValue(), key(position.node)))
            return __insert(position.node, position.node, v);
            // first argument just needs to be non-null 
        else
            return insert_equal(v);
    else if (position.node == header) // end()
        if (!key_compare(v.KeyOfValue(), key(rightmost())))
            return __insert(0, rightmost(), v);
        else
            return insert_equal(v);
    else {
        iterator before = position;
        --before;
        if (!key_compare(v.KeyOfValue(), key(before.node))
            && !key_compare(key(position.node), v.KeyOfValue()))
            if (right(before.node) == 0)
                return __insert(0, before.node, v); 
            else
                return __insert(position.node, position.node, v);
                // first argument just needs to be non-null 
        else
            return insert_equal(v);
    }
}


template <class K, class _T, class Cmp> template<class II>
void Tree<K, _T, Cmp>::insert_equal(II first, II last) {
  for ( ; first != last; ++first)
    insert_equal(*first);
}

template <class K, class _T, class Cmp> template<class II>
void Tree<K, _T, Cmp>::insert_unique(II first, II last) {
  for ( ; first != last; ++first)
    insert_unique(*first);
}

         
template <class Key, class _T, class Compare>
inline void
Tree<Key, _T, Compare>::erase(iterator position) {
  link_type y = (link_type) _Tree_rebalance_for_erase(position.node,
                                                          header->parent,
                                                          header->left,
                                                          header->right);
  y->clear();
  --node_count;
}

template <class Key, class _T, class Compare>
typename Tree<Key, _T, Compare>::size_type 
Tree<Key, _T, Compare>::erase(const Key& x) {
    std::pair<iterator,iterator> p = equal_range(x);
    size_type n = 0;
    distance(p.first, p.second, n);
    erase(p.first, p.second);
    return n;
}

template <class Key, class _T, class Compare>
void Tree<Key, _T, Compare>::__erase(link_type x) {
                                // erase without rebalancing
  while (x != 0) {
    __erase(right(x));
    link_type y = left(x);
    x->clear();
    x = y;
  }
}

template <class Key, class _T, class Compare>
void Tree<Key, _T, Compare>::erase(iterator first, 
                                                            iterator last) {
    if (first == begin() && last == end())
        clear();
    else
        while (first != last) erase(first++);
}

template <class Key, class _T, class Compare>
void Tree<Key, _T, Compare>::erase(const Key* first, 
                                                            const Key* last) {
    while (first != last) erase(*first++);
}

template <class Key, class _T, class Compare>
typename Tree<Key, _T, Compare>::iterator 
Tree<Key, _T, Compare>::find(const Key& k) {
   link_type y = header;        // Last node which is not less than k. 
   link_type x = root();        // Current node. 

   while (x != 0) 
     if (!key_compare(key(x), k))
       y = x, x = left(x);
     else
       x = right(x);

   iterator j = iterator(y);   
   return (j == end() || key_compare(k, key(j.node))) ? end() : j;
}

template <class Key, class _T, class Compare>
typename Tree<Key, _T, Compare>::const_iterator 
Tree<Key, _T, Compare>::find(const Key& k) const {
   link_type y = header; /* Last node which is not less than k. */
   link_type x = root(); /* Current node. */

   while (x != 0) {
     if (!key_compare(key(x), k))
       y = x, x = left(x);
     else
       x = right(x);
   }
   const_iterator j = const_iterator(y);   
   return (j == end() || key_compare(k, key(j.node))) ? end() : j;
}

template <class Key, class _T, class Compare>
typename Tree<Key, _T, Compare>::size_type 
Tree<Key, _T, Compare>::count(const Key& k) const {
    std::pair<const_iterator, const_iterator> p = equal_range(k);
    size_type n = 0;
    distance(p.first, p.second, n);
    return n;
}

template <class Key, class _T, class Compare>
typename Tree<Key, _T, Compare>::iterator 
Tree<Key, _T, Compare>::lower_bound(const Key& k) {
   link_type y = header; /* Last node which is not less than k. */
   link_type x = root(); /* Current node. */

   while (x != 0) 
     if (!key_compare(key(x), k))
       y = x, x = left(x);
     else
       x = right(x);

   return iterator(y);
}

template <class Key, class _T, class Compare>
typename Tree<Key, _T, Compare>::const_iterator 
Tree<Key, _T, Compare>::lower_bound(const Key& k) const {
   link_type y = header; /* Last node which is not less than k. */
   link_type x = root(); /* Current node. */

   while (x != 0) 
     if (!key_compare(key(x), k))
       y = x, x = left(x);
     else
       x = right(x);

   return const_iterator(y);
}

template <class Key, class _T, class Compare>
typename Tree<Key, _T, Compare>::iterator 
Tree<Key, _T, Compare>::upper_bound(const Key& k) {
  link_type y = header; /* Last node which is greater than k. */
  link_type x = root(); /* Current node. */

   while (x != 0) 
     if (key_compare(k, key(x)))
       y = x, x = left(x);
     else
       x = right(x);

   return iterator(y);
}

template <class Key, class _T, class Compare>
typename Tree<Key, _T, Compare>::const_iterator 
Tree<Key, _T, Compare>::upper_bound(const Key& k) const {
  link_type y = header; /* Last node which is greater than k. */
  link_type x = root(); /* Current node. */

   while (x != 0) 
     if (key_compare(k, key(x)))
       y = x, x = left(x);
     else
       x = right(x);

   return const_iterator(y);
}

template <class Key, class _T, class Compare>
inline typename std::pair<typename Tree<Key, _T, Compare>::iterator,
            typename Tree<Key, _T, Compare>::iterator>
Tree<Key, _T, Compare>::equal_range(const Key& k) {
    return std::pair<iterator, iterator>(lower_bound(k), upper_bound(k));
}

template <class Key, class _T, class Compare>
inline typename std::pair<typename Tree<Key, _T, Compare>::const_iterator,
            typename Tree<Key, _T, Compare>::const_iterator>
Tree<Key, _T, Compare>::equal_range(const Key& k) const {
    return std::pair<const_iterator,const_iterator>(lower_bound(k), upper_bound(k));
}

template <typename Key>
inline int __black_count(TreeNode<Key>* node, TreeNode<Key>* root)
{
  if (node == 0)
    return 0;
  else {
    int bc = node->color == _Tree_black ? 1 : 0;
    if (node == root)
      return bc;
    else
      return bc + __black_count(node->parent, root);
  }
}

template <class Key, class _T, class Compare>
bool 
Tree<Key, _T, Compare>::__rb_verify() const
{
  if (node_count == 0 || begin() == end())
    return node_count == 0 && begin() == end() &&
      header->left == header && header->right == header;
  
  int len = __black_count(leftmost(), root());
  for (const_iterator it = begin(); it != end(); ++it) {
    link_type x = (link_type) it.node;
    link_type L = left(x);
    link_type R = right(x);

    if (x->color == _Tree_red)
      if ((L && L->color == _Tree_red) ||
          (R && R->color == _Tree_red))
        return false;

    if (L && key_compare(key(x), key(L)))
      return false;
    if (R && key_compare(key(R), key(x)))
      return false;

    if (!L && !R && __black_count(x, root()) != len)
      return false;
  }

  if (leftmost() != TreeNode<Key>::minimum(root()))
    return false;
  if (rightmost() != TreeNode<Key>::maximum(root()))
    return false;

  return true;
}

} // namespace
} // namespace

#endif 
