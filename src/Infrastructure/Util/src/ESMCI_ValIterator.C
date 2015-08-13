// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2015, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_ValIterator.C"
//==============================================================================

#include "ESMCI_ValIterator.h"
#include <cassert>
#include <cstddef>
#include <iostream>

namespace ESMCI{

// FIXME: Make sure that 0 is a valid value for sizes (vec size, range size etc)
ValIterator::ValIterator(int start, int end, int stride):val_idx_(0), val_iter_count_(0), value_(start), stride_(stride), vec_range_dist_(0), nvals_(0), nrem_vals_(0)
{
  assert(start <= end);
  assert(stride != 0);

  val_.reserve(1);
  val_[0] = start;

  val_iter_offset_.reserve(1);
  val_iter_offset_.assign(1, 0);

  value_ = val_[0];
  //nvals_ = (end + 1 - start)/stride;
  nvals_ = (end - start)/stride + 1;
  nrem_vals_ = nvals_;
}

ValIterator::ValIterator(int *vec, std::size_t vec_sz, int nvals): val_idx_(0), val_iter_count_(0), value_(0), stride_(0), vec_range_dist_(0), nvals_(nvals), nrem_vals_(0)
{
  assert(vec != NULL);
  assert(vec_sz > 0);
  assert(nvals > 0);

  val_.reserve(vec_sz);
  val_.assign(vec, vec + vec_sz);

  val_iter_offset_.reserve(vec_sz);
  val_iter_offset_.assign(vec_sz, 0);

  value_ = val_[0];
  nvals_ = nvals;
  nrem_vals_ = nvals;
}

ValIterator::ValIterator(int *vec, std::size_t vec_sz, std::size_t vec_range_dist, std::size_t iter_range_dist):val_idx_(0), val_iter_count_(0), stride_(0), vec_range_dist_(vec_range_dist), nvals_(0), nrem_vals_(0)
{
  assert(vec != NULL);
  assert(vec_sz > 0);
  assert(vec_range_dist > 0);
  assert(iter_range_dist > 0);
  assert(iter_range_dist >= vec_range_dist);

  val_.reserve(vec_sz);
  val_.assign(vec, vec + vec_sz);

  val_iter_offset_.reserve(vec_sz);
  val_iter_offset_.assign(vec_sz, vec_range_dist);

  value_ = val_[0];

  int ncomplete_ranges = iter_range_dist/vec_range_dist;
  int vals_in_complete_ranges = ncomplete_ranges * vec_sz;
  int vals_in_part_ranges = 0;

  int part_range_dist = iter_range_dist % vec_range_dist;
  if(part_range_dist > 0){
    int part_range_begin = ncomplete_ranges * vec_range_dist;
    for(int i=0; i<vec_sz; i++){
      // We stop iterating when we encounter the first value
      // that is greater than the iterator range distance
      if((part_range_begin + vec[i]) >= iter_range_dist){
        break;
      }
      vals_in_part_ranges++;
    }
  }
  
  nvals_ = vals_in_complete_ranges + vals_in_part_ranges;
  nrem_vals_ = nvals_;
}

void ValIterator::reset(void )
{
  val_idx_ = 0;
  val_iter_count_ = 0;   
  if(val_iter_offset_.size() > 0){
    val_iter_offset_.assign(val_iter_offset_.size(), vec_range_dist_);
  }
  value_ = val_[0];
  nrem_vals_ = nvals_;
}

ValIterator &ValIterator::operator++()
{
  val_idx_++;
  if(val_idx_ >= val_.size()){
    val_idx_ = 0;
    val_iter_count_++;
  }
  value_ = val_[val_idx_] 
            + stride_ * val_iter_count_ 
            + val_iter_offset_[val_idx_] * val_iter_count_;
  nrem_vals_--;

  return *this;
}

ValIterator ValIterator::operator++(int )
{
  ValIterator tmp(*this);
  operator++();
  return tmp;
}
  
} //namespace ESMCI
