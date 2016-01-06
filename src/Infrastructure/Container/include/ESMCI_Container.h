// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMCI_Container_H
#define ESMCI_Container_H

#include <utility>
#include <map>
#include <vector>
#include <list>
#include <iostream>

#include "ESMCI_LogErr.h"

namespace ESMCI {

  template <typename Key, typename T>
  class Container : public std::multimap<Key, 
    typename std::list<std::pair<Key,T> >::iterator>{
    std::list<std::pair<Key,T> > orderedList;
    bool garbageActive;
    std::vector<T> garbage;
   public:
    Container(){
      garbageOff();   // by default no garbage is kept
    }
    void add(Key k, T t, bool multi=false, bool relaxed=false);
    void addReplace(Key k, T t);
    void clear();
    T get(Key k)const;
    void get(Key k, std::vector<T> &v,
      ESMC_ItemOrder_Flag itemorderflag=ESMC_ITEMORDER_ABC)const;
    int getCount(Key k)const;
    void getVector(std::vector<T> &v,
      ESMC_ItemOrder_Flag itemorderflag=ESMC_ITEMORDER_ABC)const;
    void getKeyVector(std::vector<Key> &v,
      ESMC_ItemOrder_Flag itemorderflag=ESMC_ITEMORDER_ABC)const;
    bool isPresent(Key k)const{
      if (this->find(k)!=this->end())
        return true;  // key found
      return false;   // key not found
    }
    void remove(Key k, bool multi=false, bool relaxed=false);
    void replace(Key k, T t, bool multi=false, bool relaxed=false);
    void garbageOn(){
      garbageActive = true;
    }
    void garbageOff(){
      garbageActive = false;
    }
    void garbageClear(){
      garbage.clear();  // clear the garbage vector
    }
    void garbageGet(std::vector<T> &v){
      v = garbage;      // copy the contents of the garbage vector
    }
    int garbageCount()const{
      return garbage.size();
    }
    void print()const;
  };
  
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Container::add()"
  // Add an element to the container. By default the method executes in strict
  // mode, where it is an error if an element with the same key already exists.
  // In relaxed mode this condition turns this method into a no-op and no
  // error is thrown.
  // However, in multi mode, items with the same name are added to the container
  // and the relaxed flag is irrelevant.
  template <typename Key, typename T>
  void Container<Key, T>::add(Key k, T t, bool multi, bool relaxed){
    int rc = ESMC_RC_NOT_IMPL;              // final return code
    typename Container::iterator pos = this->lower_bound(k);
    typename std::list<std::pair<Key,T> >::iterator lastElement;
    if (pos != this->end() && pos->first == k){
      // key already exists
      if (multi){
        orderedList.push_back(std::pair<Key,T>(k, t)); // append to list
        lastElement = orderedList.end();
        this->insert(pos, std::pair<Key, 
          typename std::list<std::pair<Key,T> >::iterator>
          (k, --lastElement));      // store the iterator in multimap
      }else{
        if (!relaxed){
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
            "key already exists", ESMC_CONTEXT, &rc);
          throw rc;  // bail out with exception
        }
        if (garbageActive)
          garbage.push_back(t); // not added object goes into garbage
      }
    }else{
      // this is a new key
      orderedList.push_back(std::pair<Key,T>(k, t)); // append to list
      lastElement = orderedList.end();
      this->insert(pos, std::pair<Key, 
        typename std::list<std::pair<Key,T> >::iterator>
        (k, --lastElement));      // store the iterator in multimap
    }
  }
  
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Container::addReplace()"
  // Add an element to the container if no element with the specified key
  // exists, or replace an existing element with the same key.
  template <typename Key, typename T>
  void Container<Key, T>::addReplace(Key k, T t){
    typename Container::iterator pos = this->find(k);
    typename std::list<std::pair<Key,T> >::iterator lastElement;
    if (pos!=this->end()){
      // key already exists
      if (garbageActive)
        garbage.push_back(pos->second->second); // replaced object into garbage
      pos->second->second = t; // store new object in the same list element
    }else{
      // this is a new key
      orderedList.push_back(std::pair<Key,T>(k, t)); // append to list
      lastElement = orderedList.end();
      this->insert(pos, std::pair<Key, 
        typename std::list<std::pair<Key,T> >::iterator>
        (k, --lastElement));      // store the iterator in multimap
    }
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Container::clear()"
  // Clear the entire contents of the container.
  template <typename Key, typename T>
  void Container<Key, T>::clear(){
    int rc = ESMC_RC_NOT_IMPL;              // final return code
    typename std::list<std::pair<Key,T> >::iterator pos;
    for (pos = orderedList.begin(); pos != orderedList.end(); ++pos)
      garbage.push_back(pos->second); // object goes into garbage
    orderedList.clear();   // clear the orderedList part of the container
    std::multimap<Key, typename std::list<std::pair<Key,T> >::iterator>
      ::clear(); // clear the multimap part of the container
  }
  
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Container::get()"
  // Access an element by key. It is an error if no element with the specified
  // key exists.
  template <typename Key, typename T>
  T Container<Key, T>::get(Key k)const{
    int rc = ESMC_RC_NOT_IMPL;              // final return code
    std::pair<typename Container::const_iterator,
      typename Container::const_iterator> range;
    range = this->equal_range(k);
    if (range.first == range.second){
      // does not exist -> error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "key does not exist", ESMC_CONTEXT, &rc);
      throw rc;  // bail out with exception
    }
    if (range.first != --range.second){
      // key is not unique -> error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "key is not unique", ESMC_CONTEXT, &rc);
      throw rc;  // bail out with exception
    }
    return range.first->second->second;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Container::get()"
  // Access all elements that match the key.
  template <typename Key, typename T>
  void Container<Key, T>::get(Key k, std::vector<T> &v,
    ESMC_ItemOrder_Flag itemorderflag)const{
    v.clear();
    if (itemorderflag == ESMC_ITEMORDER_ABC){
      std::pair<typename Container::const_iterator,
        typename Container::const_iterator> range;
      range = this->equal_range(k);
      typename Container::const_iterator pos;
      for (pos=range.first; pos!=range.second; ++pos)
        v.push_back(pos->second->second);
    }else if (itemorderflag == ESMC_ITEMORDER_ADDORDER){
      typename std::list<std::pair<Key,T> >::const_iterator pos;
      for (pos = orderedList.begin(); pos != orderedList.end(); ++pos)
        if (pos->first == k)
          v.push_back(pos->second);
    }else{
      v.resize(0);
    }
  }
    
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Container::getCount()"
  // Number of items that match the key.
  template <typename Key, typename T>
  int Container<Key, T>::getCount(Key k)const{
    std::pair<typename Container::const_iterator,
      typename Container::const_iterator> range;
    range = this->equal_range(k);
    typename Container::const_iterator pos;
    int count = 0;  // initialize
    for (pos=range.first; pos!=range.second; ++pos)
      ++count;
    return count;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Container::getVector()"
  // Access the entire contents of the container in form of a vector.
  template <typename Key, typename T>
  void Container<Key, T>::getVector(std::vector<T> &v,
    ESMC_ItemOrder_Flag itemorderflag)const{
    int rc = ESMC_RC_NOT_IMPL;              // final return code
    v.clear();
    if (itemorderflag == ESMC_ITEMORDER_ABC){
      v.resize(this->size());
      typename Container::const_iterator pos;
      int i = 0;
      for (pos = this->begin(); pos != this->end(); ++pos)
        v[i++] = pos->second->second;
    }else if (itemorderflag == ESMC_ITEMORDER_ADDORDER){
      v.resize(orderedList.size());
      int i = 0;
      typename std::list<std::pair<Key,T> >::const_iterator pos;
      for (pos = orderedList.begin(); pos != orderedList.end(); ++pos)
        v[i++] = pos->second;
    }else{
      v.resize(0);
    }
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Container::getKeyVector()"
  // Access the keys of the entire contents of the container in form of a
  // vector.
  template <typename Key, typename T>
  void Container<Key, T>::getKeyVector(std::vector<Key> &v,
    ESMC_ItemOrder_Flag itemorderflag)const{
    int rc = ESMC_RC_NOT_IMPL;              // final return code
    v.clear();
    if (itemorderflag == ESMC_ITEMORDER_ABC){
      v.resize(this->size());
      int i = 0;
      typename Container::const_iterator pos;
      for (pos = this->begin(); pos != this->end(); ++pos)
        v[i++] = pos->first;
    }else if (itemorderflag == ESMC_ITEMORDER_ADDORDER){
      v.resize(orderedList.size());
      int i = 0;
      typename std::list<std::pair<Key,T> >::const_iterator pos;
      for (pos = orderedList.begin(); pos != orderedList.end(); ++pos)
        v[i++] = pos->first;
    }else{
      v.resize(0);
    }
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Container::print()"
  // Print the contents of the container.
  template <typename Key, typename T>
  void Container<Key, T>::print()const{
    int rc = ESMC_RC_NOT_IMPL;              // final return code
    int i = 0;
    typename Container::const_iterator pos;
    for (pos = this->begin(); pos != this->end(); ++pos)
      std::cout << "Container::print() multipmap:item="<<i++<<
        " key="<<pos->first <<" value="<<pos->second->second<<"\n";
    i = 0;  // reset
    typename std::list<std::pair<Key,T> >::const_iterator posl;
    for (posl = orderedList.begin(); posl != orderedList.end(); ++posl)
      std::cout << "Container::print()      list:item="<<i++<<
        " key="<<posl->first <<" value="<<posl->second<<"\n";
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Container::remove()"
  // Remove the element with specified key. By default the method executes in
  // strict mode, where it is an error if no element with the specified key
  // exists. In relaxed mode this condition turns this method into a no-op
  // and no error is thrown.
  // Further, for multi==false, the relaxed flag also covers the case where
  // there are multiple items in the container that match the key. Again the 
  // relaxed mode turns this into a no-op, and no error is thrown. None of
  // the multiple matching elements are removed under this condition.
  // With multi==true the latter condition isn't an error anyway, instead
  // all items that match the key are removed.
  template <typename Key, typename T>
  void Container<Key, T>::remove(Key k, bool multi, bool relaxed){
    int rc = ESMC_RC_NOT_IMPL;              // final return code
    std::pair<typename Container::iterator,
      typename Container::iterator> range;
    typename Container::iterator pos;
    range = this->equal_range(k);
    if (range.first == range.second){
      // key does not exist
      if (!relaxed){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "key does not exist", ESMC_CONTEXT, &rc);
        throw rc;  // bail out with exception
      }
    }
    pos = range.second;
    if (range.first != --pos){
      // key is not unique
      if (!multi){
        if (!relaxed){
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
            "key is not unique", ESMC_CONTEXT, &rc);
          throw rc;  // bail out with exception
        }
        return; // bail out without exception
      }
    }
    for (pos=range.first; pos!=range.second; ++pos){
      if (garbageActive)
        garbage.push_back(pos->second->second); // removed object into garbage
      orderedList.erase(pos->second); // remove entry from orderedList
    }
    this->erase(range.first, range.second); // remove entries from multimap part
  }
  
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Container::replace()"
  // Replace the element that has matching key with the specified element. By
  // default the method executes in strict mode, where it is an error if no
  // element with the same key as the specified element exists. In relaxed mode
  // this condition turns this method into a no-op and no error is thrown.
  // Further, for multi==false, the relaxed flag also covers the case where
  // there are multiple items in the container that match the key. Again the 
  // relaxed mode turns this into a no-op, and no error is thrown. Also none of 
  // the multiple items with matching key are replaced.
  // With multi==true the latter condition isn't an error anyway, instead
  // all items that match the key are replaced.
  template <typename Key, typename T>
  void Container<Key, T>::replace(Key k, T t, bool multi, bool relaxed){
    int rc = ESMC_RC_NOT_IMPL;              // final return code
    std::pair<typename Container::iterator,
      typename Container::iterator> range;
    typename Container::iterator pos;
    range = this->equal_range(k);
    if (range.first == range.second){
      // does not exist
      if (!relaxed){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "key does not exist", ESMC_CONTEXT, &rc);
        throw rc;  // bail out with exception
      }
      garbage.push_back(t); // object not used to replace item goes into garbage
      return; // bail out without exception
    }
    pos = range.second;
    if (range.first != --pos){
      // key is not unique
      if (!multi){
        if (!relaxed){
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
            "key is not unique", ESMC_CONTEXT, &rc);
          throw rc;  // bail out with exception
        }
        garbage.push_back(t); // object not used to replace item into garbage
        return; // bail out without exception
      }
    }
    for (pos=range.first; pos!=range.second; ++pos){
      if (garbageActive)
        garbage.push_back(pos->second->second); // removed object into garbage
      if (pos!=range.first){
        orderedList.erase(pos->second); // remove entry from orderedList
      }
    }
    pos = range.first;
    this->erase(++pos, range.second);
    range.first->second->second = t; // fill orderedList element with new value
  }

} // namespace ESMCI

#endif  // ESMCI_Container_H
