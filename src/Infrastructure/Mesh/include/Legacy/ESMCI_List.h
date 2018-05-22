// $Id$
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_List_h
#define ESMCI_List_h

#include <Mesh/include/Legacy/ESMCI_Exception.h>

#include <cstddef>
#include <iterator>
#include <iostream>
#include <iomanip>
#include <algorithm>
#include <typeinfo>

namespace ESMCI {

// A 'self aware' list class.  Objects themselves carry connectivity,
// which allows them to 'self delete'.
// List is singly linked to reduce memory overhead for these
// large object lists.

template <typename> class List;
template<typename A,typename B,typename C> class _List_iterator;

/**
 * Node in the 'embedded list' object.
 *
*/
template<typename TT>
class ListNode {
public:
typedef TT derived_type;
friend class List<TT>;
friend class _List_iterator<TT,TT&,TT*>;
friend class _List_iterator<TT,const TT&,const TT*>;
private:
ListNode(const ListNode&);
ListNode &operator=(const ListNode &);
ListNode *prev;
ListNode *next;
protected:
ListNode() : prev(NULL), next(NULL) {}
~ListNode() {
  // If in a list, splice the link
  if (prev && next) {
    prev->next = next;
    next->prev = prev;
  }
}
};

template<class TT, typename Ref, typename Ptr>
class _List_iterator {
public:
friend class List<TT>;
typedef std::bidirectional_iterator_tag iterator_category;
typedef std::ptrdiff_t difference_type;
typedef Ptr pointer;
typedef Ref reference;
typedef TT value_type;
typedef std::size_t size_type;
typedef _List_iterator<TT,TT&,TT*> iterator;
typedef _List_iterator<TT,const TT&,const TT*> const_iterator;
typedef _List_iterator<TT,Ref,Ptr> self;
template<typename A,typename B,typename C> friend class _List_iterator;

_List_iterator() : cur(NULL) {}
_List_iterator(ListNode<TT> *nd) : cur(nd) {}
_List_iterator(const iterator &rhs) {
cur = rhs.cur;
}

self &operator=(const _List_iterator &rhs) {
  if (this == &rhs) return *this;
  cur = rhs.cur;
  return *this;
}

bool operator==(const _List_iterator &rhs) const {
  return cur == rhs.cur;
}

bool operator!=(const _List_iterator &rhs) const {
  return cur != rhs.cur;
}

self &operator--() {
  cur = cur->prev;
  return *this;
}

self &operator++() {
  cur = cur->next;
  return *this;
}

self operator--(int) {
  _List_iterator tmp = *this;
  this->operator--();
  return tmp;
}

self operator++(int) {
  _List_iterator tmp = *this;
  this->operator++();
  return tmp;
}

reference
operator*() const {
return static_cast<reference>(*cur);
}

pointer
operator->() const {
return static_cast<pointer>(cur);
}
private:
ListNode<TT> *cur;
}; // class iterator 


/**
 * An embedded list.  The object to place in the list derives
 * from the ListNode class above, and is stored using the links
 * provided by that class.  This allows for very quick deletion
 * from a list just by having a handle to the object.  It also
 * reduces storage overhead of having to keep a list of pointers
 * to objects.
 * First_node and end_node are allocated by the list.
 * The beginning of the list is at first->next.  This allows
 * a node to self delete without changing where the first node poinnts
 * to (it sets del->prev->next = del->next.
 * The list is slightly odd.  We must manually insert nodes into
 * a particular list, but they delete themselves automatically from
 * whatever list they happen to be in.  This saves us from having to
 * loop the list to find the correct iterator to perform a delete.
*/
template <typename TT>
class List {
public:
List();
~List();

typedef TT value_type;
typedef TT* pointer;
typedef TT& reference;
//typedef iterator iterator;
//typedef const_iterator const_iterator;

typedef _List_iterator<TT,TT&,TT*> iterator;
typedef _List_iterator<TT,const TT&,const TT*> const_iterator;


// List methods
void push_back(value_type &rhs);

void push_front(value_type &rhs);

void erase(value_type &rhs);

iterator find(const value_type &rhs);

const_iterator find(const  value_type &rhs) const;

iterator begin() {
  return iterator(first_node->next);
}

const_iterator begin() const {
  return const_iterator(first_node->next);
}

iterator end() {
  return iterator(end_node);
}

const_iterator end() const {
  return const_iterator(end_node);
}

std::size_t size() {
  return std::distance(begin(), end());
}

iterator insert(iterator _where, value_type& _x);

private:
List &operator=(const List &rhs);
List(const List &rhs);

// Long story, but these need to be TT, not ListNode<TT>.  Reason?
// Nested lists.  We need the roster to contain the nested rosters
// in TT so we can reference their begin/end pointers.
ListNode<TT> *first_node; // a location for the end of list
ListNode<TT> *end_node; // a location for the end of list
const std::type_info &list_type;
};

template<typename TT>
List<TT>::List() :
first_node(new ListNode<TT>()),
end_node(new ListNode<TT>()),
list_type(typeid(TT))
//first_node(new TT()),
//end_node(new TT())
{
  // So that begin() == end()
  first_node->next = end_node;
  end_node->prev = first_node;
  //end_node->prev = NULL; constructor does this
//std::cout << "In List constructor" << std::endl;
//std::cout << "first_node:" << std::hex << (int) first_node << std::endl;
}

template<typename TT>
List<TT>::~List() {
  delete end_node;
  delete first_node;
}

template<typename TT>
void List<TT>::push_back(value_type &rhs) {
//std::cout << "Inserting node:" << rhs.get_id() << std::endl;
//std::cout << "Inserting node:" << std::hex << (int) this << std::endl;
  if (rhs.next != NULL || rhs.prev != NULL)
    throw Ex() << "List node has non-null next,prev.  Is this value already in a list?";

  end_node->prev->next = &rhs;
  rhs.next = end_node;
  rhs.prev = end_node->prev;
  end_node->prev = &rhs;
}

template<typename TT>
void List<TT>::push_front(value_type &rhs) {
//std::cout << "Inserting node:" << rhs.get_id() << std::endl;
//std::cout << "Inserting node:" << std::hex << (int) this << std::endl;
  if (rhs.next != NULL || rhs.prev != NULL)
    throw Ex() << "List node has non-null next,prev.  Is this value already in a list?";

  rhs.next = first_node->next;
  rhs.prev = first_node;
  rhs.next->prev = &rhs;
  first_node->next = &rhs;
}

template<typename TT>
typename List<TT>::iterator List<TT>::insert(iterator _where, value_type& _x) {
  if (_x.prev || _x.next)
    throw Ex() << "insert, prev or next non null!!";
  ListNode<TT> *cur = _where.cur;
  ThrowRequire(cur != first_node); // can't insert before begin!!
  cur->prev->next = &_x;
  _x.prev = cur->prev;
  _x.next = cur;
  cur->prev = &_x;

  return iterator(&_x);
}

template<typename TT>
typename List<TT>::iterator List<TT>::find(const value_type &rhs) {
  ListNode<TT> *f = first_node->next;
  while (f != end_node && static_cast<value_type&>(*f) != rhs) {
    f = f->next;
  }
  return iterator(f);
}

template<typename TT>
typename List<TT>::const_iterator List<TT>::find(const value_type &rhs) const {
  ListNode<TT> *f = first_node->next;
  while (f != end_node && static_cast<value_type&>(*f) != rhs) {
    f = f->next;
  }
  return iterator(f);
}

// Take the 0bject out of the list.
template<typename TT>
void List<TT>::erase(value_type &rhs) {
  // Make sure node is of correct type
  rhs.prev->next = rhs.next;
  rhs.next->prev = rhs.prev;
  rhs.next = rhs.prev = NULL;
}

} // namespace

#endif
