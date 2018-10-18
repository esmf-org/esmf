// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

#ifndef ESMCI_REGIONNODE_H
#define ESMCI_REGIONNODE_H

#include <cstddef>
#include <vector>
#include <math.h>
#include <algorithm>

#include "ESMCI_LogErr.h"

#define UINT64T_BIG 18446744073709551615ULL

using std::vector;
using std::sort;

namespace ESMCI {

  static const float NANOS_TO_MILLIS = 1 / 1000.0 / 1000.0;  /* conversion factor */
  static const float NANOS_TO_SECS = 1 / 1000.0 / 1000.0 / 1000.0;
  
  class RegionNode {
    
  public:
        
  RegionNode(RegionNode *parent, uint16_t id, bool isUserRegion):
    _parent(parent), _id(id), _isUserRegion(isUserRegion),
      _count(0), _total(0), _min(UINT64T_BIG), _max(0),
      _mean(0.0), _variance(0.0), _last_entered(0),
      _time_mpi_start(0), _time_mpi(0), _count_mpi(0) {}
    
  RegionNode(uint16_t id):
    _parent(NULL), _id(id), _isUserRegion(false),
      _count(0), _total(0), _min(UINT64T_BIG), _max(0),
      _mean(0.0), _variance(0.0), _last_entered(0),
      _time_mpi_start(0), _time_mpi(0), _count_mpi(0) {}

    ~RegionNode() {
      while (!_children.empty()) {
        RegionNode *toDel = _children.back();
        _children.pop_back();
        delete toDel;
      }
    }

    bool operator==(const RegionNode &other) const {
      return _id == other._id;
    }
    
    bool operator!=(const RegionNode &other) const {
      return !(*this == other);
    }

    bool operator<(const RegionNode &rhs) const {
      return getTotal() < rhs.getTotal();
    }

    bool operator>(const RegionNode &rhs) const {
      return getTotal() > rhs.getTotal();
    }
    
    uint16_t getId() const {
      return _id;
    }
    
    RegionNode *getParent() const {
      return _parent;
    }

    uint16_t getParentId() const {
      if (_parent != NULL)
        return _parent->getId();
      else
        return 0;
    } 
    
    bool isUserRegion() const {
      return _isUserRegion;
    }

    RegionNode *getOrAddChild(int id) {
      return getOrAddChild(id, false);
    }

    RegionNode *getOrAddChild(int id, bool isUserRegion) {
      for (unsigned i = 0; i < _children.size(); i++) {
        if (_children.at(i)->getId() == id) {
          return _children.at(i);
        }
      }
      //no match found
      RegionNode *newNode = new RegionNode(this, id, isUserRegion);
      _children.push_back(newNode);
      return newNode;
    }

    void entered(uint64_t ts) {
      _last_entered = ts;
    }

    void exited(uint64_t ts) {
      uint64_t val = ts - _last_entered;
      _count++;
      _total += val;
      if (val < _min) {
        _min = val;
      }
      if (val > _max) {
        _max = val;
      }

      double delta = val - _mean;
      _mean += delta / _count;
      _variance += delta * (val - _mean) ;
    }

#define STATLINE 256
    
    void printProfile(bool printToLog) {
      char strbuf[STATLINE];
      snprintf(strbuf, STATLINE, "%-20s %-6s %-11s %-11s %-11s %-11s %-11s %-11s",
               "Region", "Count", "Total (ms)", "Self (ms)", "Mean (ms)", "Min (ms)", "Max (ms)", "Std. Dev. (ms)");
      if (printToLog) {
        ESMC_LogDefault.Write("**************** Region Timings *******************", ESMC_LOGMSG_INFO);
        ESMC_LogDefault.Write(strbuf, ESMC_LOGMSG_INFO);
      }
      else {
        std::cout << std::string(strbuf) << "\n";
      }
      printProfile(printToLog, "");
    }

    void printProfile(bool printToLog, std::string prefix) {
      if (getParent() != NULL) {
        char strname[50];
        char strbuf[STATLINE];

        //TODO: replace with name
        //snprintf(strname, 50, "%d,%d,%d,%d", _vmid, _baseid, _method, _phase);
        snprintf(strname, 10, "%d", _id);
        std::string name(strname);
        name.insert(0, prefix);
        
        snprintf(strbuf, STATLINE, "%-20s %-6lu %-11.4f %-11.4f %-11.4f %-11.4f %-11.4f %-11.4f",
                 name.c_str(), getCount(), getTotal()*NANOS_TO_MILLIS,
                 getSelfTime()*NANOS_TO_MILLIS, getMean()*NANOS_TO_MILLIS,
                 getMin()*NANOS_TO_MILLIS, getMax()*NANOS_TO_MILLIS, getStdDev()*NANOS_TO_MILLIS);
        if (printToLog) {
          ESMC_LogDefault.Write(strbuf, ESMC_LOGMSG_INFO);
        }
        else {
          std::cout << std::string(strbuf) << "\n";
        }
      } 
      for (unsigned i = 0; i < _children.size(); i++) {
        _children.at(i)->printProfile(printToLog, prefix + "  ");
      }
    }
    
    uint64_t getTotal() const {
      return _total;
    }

    size_t getCount() const {
      return _count;
    }

    double getMean() const {
      return _mean;
    }

    double getStdDev() const {
      if (_count > 2) {
        return sqrt(_variance / _count);
      }
      else {
        return 0.0;
      }
    }

    uint64_t getMin() const {
      return _min;
    }

    uint64_t getMax() const {
      return _max;
    }
    
    uint64_t getSelfTime() const {
      uint64_t st = _total;
      for (unsigned i = 0; i < _children.size(); i++) {
        st -= _children.at(i)->getTotal();
      }
      return st;
    }

    vector<RegionNode *> getChildren() const {
      return _children;
    }

    void sortChildren() {
      sort(_children.begin(), _children.end(), RegionNodeCompare());
    }

    ///// MPI //////
    void enteredMPI(uint64_t start) {
      _time_mpi_start = start;
    }

    void exitedMPI(uint64_t stop) {
      //std::cout << "enter: " << _time_mpi_start << " exit: " << stop << "\n";
      _time_mpi += (stop - _time_mpi_start);
      _count_mpi++;
    }

    uint64_t getTotalMPI() const {
      return _time_mpi;
    }

    size_t getCountMPI() const {
      return _count_mpi;
    }
    
  private:

    struct RegionNodeCompare {
      bool operator()(const RegionNode* l, const RegionNode* r) {
        return *l > *r;
      }
    };
    
    RegionNode *_parent;
    uint16_t _id;
    bool _isUserRegion;
   
    vector<RegionNode *> _children;

    size_t _count;
    uint64_t _total;
    uint64_t _min;
    uint64_t _max;
    double _mean;  
    double _variance; 
    
    uint64_t _last_entered;

    uint64_t _time_mpi_start;
    uint64_t _time_mpi;
    size_t _count_mpi;
    
  };

}

#endif
