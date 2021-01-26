// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research, 
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
#include <string>
#include <stdexcept>

#include "ESMCI_LogErr.h"

#define UINT64T_BIG 18446744073709551615ULL
#define REGION_MAX_COUNT 65500

using std::vector;
using std::sort;
using std::string;

namespace ESMCI {

  /* conversion factors */
  static const float NANOS_TO_MILLIS = 1 / 1000.0 / 1000.0;
  static const float NANOS_TO_SECS = 1 / 1000.0 / 1000.0 / 1000.0;
  
  class RegionNode {
    
  public:
        
  RegionNode(RegionNode *parent, uint16_t local_id, bool isUserRegion):
    _parent(parent), _global_id(next_global_id()),
      _local_id(local_id), _isUserRegion(isUserRegion),
      _count(0), _total(0), _min(UINT64T_BIG), _max(0),
      _mean(0.0), _variance(0.0), _last_entered(0),
      _time_mpi_start(0), _time_mpi(0), _count_mpi(0) {}
    
  RegionNode():
    _parent(NULL), _global_id(next_global_id()),
      _local_id(0), _isUserRegion(false),
      _count(0), _total(0), _min(UINT64T_BIG), _max(0),
      _mean(0.0), _variance(0.0), _last_entered(0),
      _time_mpi_start(0), _time_mpi(0), _count_mpi(0) {}

  RegionNode(bool nextGlobalId):
    _parent(NULL), _global_id(0),
      _local_id(0), _isUserRegion(false),
      _count(0), _total(0), _min(UINT64T_BIG), _max(0),
      _mean(0.0), _variance(0.0), _last_entered(0),
      _time_mpi_start(0), _time_mpi(0), _count_mpi(0) {      
      if (nextGlobalId) {
	_global_id = next_global_id();
      }           
    }

  RegionNode(char *deserializeBuffer, size_t bufferSize):
    _parent(NULL), _global_id(0),
      _local_id(0), _isUserRegion(false),
      _count(0), _total(0), _min(UINT64T_BIG), _max(0),
      _mean(0.0), _variance(0.0), _last_entered(0),
      _time_mpi_start(0), _time_mpi(0), _count_mpi(0) {
      
      deserialize(deserializeBuffer, bufferSize);
      
    }
  
  RegionNode(RegionNode *parent, RegionNode *toClone):
    _parent(parent), _global_id(next_global_id()),
      _local_id(toClone->getLocalId()),
      _name(toClone->getName()),
      _isUserRegion(toClone->isUserRegion()),
      _count(toClone->getCount()), _total(toClone->getTotal()),
      _min(toClone->getMin()), _max(toClone->getMax()),
      _mean(toClone->getMean()), _variance(toClone->_variance),
      _last_entered(0), _time_mpi_start(0),
      _time_mpi(toClone->getTotalMPI()),
      _count_mpi(toClone->getCountMPI())  {

      //deep clone children
      for (unsigned i = 0; i < toClone->_children.size(); i++) {
	addChild(toClone->_children.at(i));
      }
    }
            
    ~RegionNode() {
      while (!_children.empty()) {
        RegionNode *toDel = _children.back();
        _children.pop_back();
        delete toDel;
      }
    }
    
    bool operator==(const RegionNode &other) const {
      return _global_id == other._global_id;
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

    uint16_t getLocalId() const {
      return _local_id;
    }
    
    uint16_t getGlobalId() const {
      return _global_id;
    }
    
    RegionNode *getParent() const {
      return _parent;
    }

    uint16_t getParentGlobalId() const {
      if (_parent != NULL)
        return _parent->getGlobalId();
      else
        return 0;
    } 
    
    bool isUserRegion() const {
      return _isUserRegion;
    }

    
    RegionNode *getOrAddChild(uint16_t local_id, bool &wasAdded) {
      return getOrAddChild(local_id, false, wasAdded);
    }

    RegionNode *getOrAddChild(uint16_t local_id, bool isUserRegion, bool &wasAdded) {
      for (unsigned i = 0; i < _children.size(); i++) {
        if (_children.at(i)->getLocalId() == local_id) {
          wasAdded = false;
          return _children.at(i);
        }
      }
      //no match found
      RegionNode *newNode = new RegionNode(this, local_id, isUserRegion);
      _children.push_back(newNode);
      wasAdded = true;
      return newNode;
    }
    
    /* used by TESTING only */
    RegionNode *addChild(string name) {
      RegionNode *newNode = addChild();
      newNode->setName(name);
      return newNode;
    }

    RegionNode *addChild() {
      RegionNode *newNode = new RegionNode(this, 0, true);
      _children.push_back(newNode);
      return newNode;
    }
    
    RegionNode *addChild(RegionNode *toClone) {
      RegionNode *newNode = new RegionNode(this, toClone);
      _children.push_back(newNode);
      return newNode;
    }
    
    RegionNode *getChild(uint16_t local_id) {
      for (unsigned i = 0; i < _children.size(); i++) {
        if (_children.at(i)->getLocalId() == local_id) {
          return _children.at(i);
        }
      }
      return NULL;
    }
    
    RegionNode *getChild(string name) {
      for (unsigned i = 0; i < _children.size(); i++) {
        if (_children.at(i)->getName() == name) {
          return _children.at(i);
        }
      }
      return NULL;
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

    void setName(string name) {
      _name = name;
    }

    string getName() const {
      return _name;
    }

    void merge(const RegionNode &other) {
      
      size_t old_count = _count;
      double old_mean = _mean;
         
      _count += other.getCount();
      _total += other.getTotal();
      if (_min > other.getMin()) {
	_min = other.getMin();
      }
      if (_max < other.getMax()) {
	_max = other.getMax();
      }

      //weighted average
      _mean = ((old_count * old_mean) + (other.getMean() * other.getCount())) / _count;

      double old_mean_sq = old_mean * old_mean;
      double other_mean_sq = other.getMean() * other.getMean();
      double merge_mean_sq = _mean * _mean;
      double old_var = _variance / (old_count - 1.0);
      double other_var = other._variance / (other.getCount() - 1.0);
      _variance = ((old_var + old_mean_sq - merge_mean_sq) * (old_count - 1.0) +
		   (other_var + other_mean_sq - merge_mean_sq) * (other.getCount() - 1.0));     

      //recursively merge child nodes
      mergeChildren(other);
    }

    // total number of nodes in tree
    size_t size() {
      size_t s=1; //this node
      for (unsigned i = 0; i < _children.size(); i++) {
	s += _children.at(i)->size();
      }
      return s;
    }
    
    char *serialize(size_t *bufSize) {
      size_t bufferSize = sizeof(size_t); // node count
      if (bufferSize % 8 > 0) {
        bufferSize += (8 - (bufferSize % 8));
      }
      serializeSize(&bufferSize);
      if (bufSize != NULL) *bufSize = bufferSize;      

      //std::cout << "computed buffer size needed = " << bufferSize << "\n";
      
      char *buffer = (char *) malloc(bufferSize);
      if (buffer==NULL) {
        throw std::bad_alloc();
      }
      size_t offset = 0;
      memset(buffer, 0, bufferSize);

      //first integer is number of nodes
      size_t nodeCount = size();
      memcpy(buffer, (const void *) &nodeCount, sizeof(nodeCount));
      offset += sizeof(nodeCount);
      
      serialize(buffer, &offset, bufferSize, true);
      return buffer;
    }

  private:

    /*
     * serialize the object to the byte array
     * and update offset to the end of the serialized object
     */
    char *serialize(char *buffer, size_t *offset, size_t bufferSize, bool recursive) {
      
      //align offset at 8 bytes
      if (*offset % 8 > 0) {
        *offset += (8 - (*offset % 8));
      }

      if (*offset + localSerializeSize() > bufferSize) {
        std::stringstream errMsg;
        errMsg << "Buffer too small to serialize trace region: ";
        errMsg << "buffer size = " << bufferSize;
        errMsg << " expected: " << (*offset + localSerializeSize());
        throw std::runtime_error(errMsg.str());
      }

      //std::cout << "serialize new record offset: " << *offset << "\n";      
      
      memcpy(buffer+(*offset), (const void *) &_global_id, sizeof(_global_id));
      *offset += sizeof(_global_id);

      uint16_t parentId = getParentGlobalId();
      memcpy(buffer+(*offset), (const void *) &parentId, sizeof(parentId));
      *offset += sizeof(parentId);

      memcpy(buffer+(*offset), (const void *) &_local_id, sizeof(_local_id));
      *offset += sizeof(_local_id);

      memcpy(buffer+(*offset), (const void *) &_total, sizeof(_total));
      *offset += sizeof(_total);

      memcpy(buffer+(*offset), (const void *) &_count, sizeof(_count));
      *offset += sizeof(_count);

      memcpy(buffer+(*offset), (const void *) &_min, sizeof(_min));
      *offset += sizeof(_min);
      
      memcpy(buffer+(*offset), (const void *) &_max, sizeof(_max));
      *offset += sizeof(_max);

      memcpy(buffer+(*offset), (const void *) &_mean, sizeof(_mean));
      *offset += sizeof(_mean);

      memcpy(buffer+(*offset), (const void *) &_variance, sizeof(_variance));
      *offset += sizeof(_variance);

      int userRegion = 0;
      if (_isUserRegion) userRegion = 1;

      memcpy(buffer+(*offset), (const void *) &userRegion, sizeof(userRegion));
      *offset += sizeof(userRegion);

      size_t nameSize = strlen(_name.c_str()) + 1;
      memcpy(buffer+(*offset), (const void *) &nameSize, sizeof(nameSize));
      *offset += sizeof(nameSize);

      if (nameSize > 0) {
        memcpy(buffer+(*offset), (const void *) _name.c_str(), nameSize);
        *offset += nameSize;
      }

      // serialize children recusively
      if (recursive) {
        for (unsigned i = 0; i < _children.size(); i++) {
          _children.at(i)->serialize(buffer, offset, bufferSize, recursive);
        }
      }

      return buffer;
    }

    /* deserialize from root of tree */
    void deserialize(char *buffer, size_t bufferSize) {
      size_t offset = 0;
      size_t totalNodes = 0;
            
      if (sizeof(totalNodes) > bufferSize) {
        throw std::runtime_error("Buffer size too small when deserializing trace region.");
      }
      
      memcpy( (void *) &totalNodes, buffer, sizeof(totalNodes) );
      offset += sizeof(totalNodes);

      //sanity check
      if (totalNodes > 10000000) {
        throw std::runtime_error("Unexpected node count when deserializing trace region.");
      }

      if (totalNodes > 0) {
        deserializeLocal(buffer, &offset, bufferSize);
        size_t completed = 1;
        deserializeChildren(buffer, &offset, bufferSize, totalNodes, &completed);
      }
    }

   
    void deserializeChildren(char *buffer, size_t *offset, size_t bufferSize,
                             size_t totalNodes, size_t *completed) {

      while (*completed < totalNodes) {

        //align offset at 8 bytes
        if (*offset % 8 > 0) {
          *offset += (8 - (*offset % 8));
        }

        //look ahead at parent id of next node
        uint16_t global_id = 0;
        uint16_t parent_id = 0;
        
        if (*offset + sizeof(global_id) + sizeof(parent_id) > bufferSize) {
          std::stringstream errMsg;
          errMsg << "Buffer too small to deserialize trace region: ";
          errMsg << "buffer size = " << bufferSize;
          errMsg << " expected: " << (*offset + sizeof(global_id) + sizeof(parent_id));
          throw std::runtime_error(errMsg.str());
        }
        
        memcpy( (void *) &global_id, buffer+(*offset), sizeof(global_id) );
        memcpy( (void *) &parent_id, buffer+(*offset)+sizeof(global_id), sizeof(parent_id) );

        //std::cout << "working at offset: " << *offset << " my id = " << getGlobalId() << " parent id = " << parent_id << "\n";        
        
        //check to see if next node is my child
        if (parent_id == getGlobalId()) {
          RegionNode *child = new RegionNode(false);
	  _children.push_back(child);
	  child->_parent = this;
          child->deserializeLocal(buffer, offset, bufferSize);
          *completed = *completed + 1;
          child->deserializeChildren(buffer, offset, bufferSize, totalNodes, completed);
        }
        else {
          return;
        }
      }
      
    }
    
    void deserializeLocal(char *buffer, size_t *offset, size_t bufferSize) { 

      //align offset at 8 bytes
      if (*offset % 8 > 0) {
        *offset += (8 - (*offset % 8));
      }

      //the check below is meaningless because
      //we don't yet know the size of the region name (dynamic)
      /*
      if (*offset + localSerializeSize() > bufferSize) {
        std::stringstream errMsg;
        errMsg << "Buffer too small to deserialize local trace region: ";
        errMsg << "buffer size = " << bufferSize;
        errMsg << " expected: " << (*offset + localSerializeSize());
        throw std::runtime_error(errMsg.str());
      }
      */
      //std::cout << "DEserialize new record offset: " << *offset << "\n";      
      
      memcpy( (void *) &_global_id, buffer+(*offset), sizeof(_global_id) );
      *offset += sizeof(_global_id);
      
      uint16_t parentId = 0;
      memcpy( (void *) &parentId, buffer+(*offset), sizeof(parentId) );
      *offset += sizeof(parentId);
      
      memcpy( (void *) &_local_id, buffer+(*offset), sizeof(_local_id) );
      *offset += sizeof(_local_id);
      
      memcpy( (void *) &_total, buffer+(*offset), sizeof(_total) );
      *offset += sizeof(_total);
      
      memcpy( (void *) &_count, buffer+(*offset), sizeof(_count) );
      *offset += sizeof(_count);

      memcpy( (void *) &_min, buffer+(*offset), sizeof(_min) );
      *offset += sizeof(_min);

      memcpy( (void *) &_max, buffer+(*offset), sizeof(_max) );
      *offset += sizeof(_max);

      memcpy( (void *) &_mean, buffer+(*offset), sizeof(_mean) );
      *offset += sizeof(_mean);

      memcpy( (void *) &_variance, buffer+(*offset), sizeof(_variance) );
      *offset += sizeof(_variance);

      int userRegion = 0;
      memcpy( (void *) &userRegion, buffer+(*offset), sizeof(userRegion) );
      *offset += sizeof(userRegion);
      _isUserRegion = (userRegion == 1);

      size_t nameSize = 0;
      memcpy( (void *) &nameSize, buffer+(*offset), sizeof(nameSize) );
      *offset += sizeof(nameSize);

      if (nameSize > 0) {
        char *copyName = (char *) malloc(nameSize);
        if (copyName == NULL) throw std::bad_alloc();
        
        memcpy( (void *) copyName, buffer+(*offset), nameSize );
        *offset += nameSize;
        _name = string(copyName);
        free(copyName);
      }
                  
    }

    /*
     * returns length required to serialize this object
     * does not taking 8 byte alignment into account
     */
    size_t localSerializeSize() {
      size_t localSize =
        sizeof(_global_id) +
        sizeof(_global_id) + // parent id
        sizeof(_local_id) + 
        sizeof(_total) +
        sizeof(_count) +
        sizeof(_min) +
        sizeof(_max) +
        sizeof(_mean) +
        sizeof(_variance) +
        sizeof(int) + // isUserRegion flag
        sizeof(size_t) +  // records length of name
        strlen(_name.c_str()) + 1;  // length of name
      return localSize;
    }
    
    void mergeChildren(const RegionNode &other) {
      for (unsigned i = 0; i < other._children.size(); i++) {
	RegionNode *child = getChild(other._children.at(i)->getName());
	if (child != NULL) {
	  child->merge(*(other._children.at(i)));
	}
	else {
	  child = addChild(other._children.at(i));
	}
      }     
    }

    /*
     * returns size to serialize entire tree
     */
    void serializeSize(size_t *offset) {
      if (*offset % 8 > 0) {
        *offset += (8 - (*offset % 8));
      }
      *offset += localSerializeSize();
      for (unsigned i = 0; i < _children.size(); i++) {
      	_children.at(i)->serializeSize(offset);
      }      
    }
    
    struct RegionNodeCompare {
      bool operator()(const RegionNode* l, const RegionNode* r) {
        return *l > *r;
      }
    };

    static uint16_t next_global_id() {
      static uint16_t next = 1;
      if (next > REGION_MAX_COUNT) {
	throw std::range_error("Out of space for trace regions");
      }
      return next++;
    } 
    
    RegionNode *_parent;
    uint16_t _global_id;

    //allows each node to be referenced by
    //an integer (for speed) or string (by name)
    uint16_t _local_id;
    string _name;

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
