// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

#ifndef ESMCI_REGIONSUMMARY_H
#define ESMCI_REGIONSUMMARY_H

#include <cstddef>
#include <vector>
#include <math.h>
#include <algorithm>
#include <string>
#include <stdexcept>

#include "ESMCI_LogErr.h"

#define UINT64T_BIG 18446744073709551615ULL

using std::vector;
using std::sort;
using std::string;

namespace ESMCI {

  class RegionSummary {
    
  public:

  RegionSummary(RegionSummary *parent):
    _parent(parent), _name(""),
      _pet_count(0), _count_each(0), _counts_match(true),
      _total_sum(0),
      _total_min(UINT64T_BIG), _total_min_pet(-1),
      _total_max(0), _total_max_pet(-1) {}
    
    ~RegionSummary() {
      while (!_children.empty()) {
        RegionSummary *toDel = _children.back();
        _children.pop_back();
        delete toDel;
      }
    }
    
    RegionSummary *getParent() const {
      return _parent;
    }

    RegionSummary *addChild(string name) {
      RegionSummary *newNode = new RegionSummary(this);
      newNode->setName(name);
      _children.push_back(newNode);
      return newNode;
    }

    RegionSummary *getChild(string name) {
      for (unsigned i = 0; i < _children.size(); i++) {
        if (_children.at(i)->getName() == name) {
          return _children.at(i);
        }
      }
      return NULL;
    }
    
    RegionSummary *getOrAddChild(string name) {
      RegionSummary *child = getChild(name);
      if (child == NULL) {
	child = addChild(name);
      }
      return child;
    }

    bool operator<(const RegionSummary &rhs) const {
      return getTotalSum() < rhs.getTotalSum();
    }

    bool operator>(const RegionSummary &rhs) const {
      return getTotalSum() > rhs.getTotalSum();
    }

    uint64_t getTotalSum() const {
      return _total_sum;
    }

    size_t getCountEach() const {
      return _count_each;
    }

    bool getCountsMatch() const {
      return _counts_match;
    }

    double getTotalMean() const {
      if (_pet_count > 0) {
	return _total_sum / _pet_count;
      }
      else {
	return 0.0;
      }
    }

    uint64_t getTotalMin() const {
      return _total_min;
    }

    int getTotalMinPet() const {
      return _total_min_pet;
    }

    uint64_t getTotalMax() const {
      return _total_max;
    }
    
    int getTotalMaxPet() const {
      return _total_max_pet;
    }

    size_t getPetCount() const {
      return _pet_count;
    }

    /*
    uint64_t getSelfTime() const {
      uint64_t st = _total;
      for (unsigned i = 0; i < _children.size(); i++) {
        st -= _children.at(i)->getTotal();
      }
      return st;
    }
    */

    vector<RegionSummary *> getChildren() const {
      return _children;
    }

    void sortChildren() {
      sort(_children.begin(), _children.end(), RegionSummaryCompare());
    }

    void setName(string name) {
      _name = name;
    }

    string getName() const {
      return _name;
    }

    /*
     * Add a set of PET region timings to the summary
     */
    void merge(const RegionNode &rn, int pet) {

      _pet_count++;
      if (_pet_count == 1) {
	_count_each = rn.getCount();
      }
      else if (_count_each != rn.getCount()) {
	_counts_match = false;
      }
         
      _total_sum += rn.getTotal();
      if (_total_min > rn.getTotal()) {
	_total_min = rn.getTotal();
	_total_min_pet = pet;
      }
      if (_total_max < rn.getTotal()) {
	_total_max = rn.getTotal();
	_total_max_pet = pet;
      }
      
      //recursively merge child nodes
      mergeChildren(rn, pet);
    }

  private:

    void mergeChildren(const RegionNode &other, int pet) {
      for (unsigned i = 0; i < other.getChildren().size(); i++) {
	RegionSummary *child = getOrAddChild(other.getChildren().at(i)->getName());
	child->merge(*(other.getChildren().at(i)), pet);
      }     
    }
    
    struct RegionSummaryCompare {
      bool operator()(const RegionSummary* l, const RegionSummary* r) {
        return *l > *r;
      }
    };

    RegionSummary *_parent;
    string _name;

    vector<RegionSummary *> _children;

    size_t _pet_count;       //number of PETs reporting for this region

    size_t _count_each;      //count on each PET (typically these will match)
    bool _counts_match;      //whether the counts all match
    uint64_t _total_sum;     //sum of all totals
    uint64_t _total_min;     //min of all totals
    int      _total_min_pet; //PET with min total
    uint64_t _total_max;     //max of all totals
    int      _total_max_pet; //PET with max total
    
  };

}

#endif
