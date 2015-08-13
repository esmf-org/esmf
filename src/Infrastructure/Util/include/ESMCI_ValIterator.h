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
// ESMF Val Iterator include file
//
//-----------------------------------------------------------------------------
//

#ifndef ESMCI_VALITERATOR_H_INCLUDED
#define ESMCI_VALITERATOR_H_INCLUDED

#include <vector>

namespace ESMCI{

class ValIterator{
  public:
    // Monotonically strictly increasing value iterator
    ValIterator(int start, int end, int stride);
    // Custom value iterator specified via a vector
    // The values in vec repeat (reset+continue) if nvals > vec_sz
    ValIterator(int *vec, std::size_t vec_sz, int nvals);
    // Custom value iterator specified via a vector
    // vec_range_dist defines the distance covered by the range of
    // values in the vector
    // iter_range_dist defines the distance covered by the iterator
    // So if iter_range_dist > vec_range_dist, the returned values
    // are values in vec extrapolated to the iterating ranges
    ValIterator(int *vec, std::size_t vec_sz, 
                std::size_t vec_range_dist, std::size_t iter_range_dist);
    // Since a vector value iterator need not be monotonically
    // increasing we cannot have a begin/end iteration-style
    // hasMore() - has more values to iterate?
    inline bool hasMore(void ) const;
    // Get current value
    inline int value(void ) const;
    // Reset the iterator
    void reset(void );
    // Number of values traversed by the iterator
    // Note that this is != distance of the iterator
    inline std::size_t size(void ) const;

    // Increment the iterator
    ValIterator &operator++();
    ValIterator operator++(int );
  private:
    // The value vector
    std::vector<int> val_;
    // Index to current stride in the value vec
    int val_idx_;
    // Iteration count in the val_ array
    // i.e., How many times we iterated the val
    // vector completely
    int val_iter_count_;
    // Iteration offset for the val array
    // This offset is added to val_ to get
    // the value_
    std::vector<int> val_iter_offset_;
    // The current val
    int value_;
    // Stride
    int stride_;
    // The value vector range distance
    int vec_range_dist_;
    // Number of vals
    int nvals_;
    // Number of remaining vals
    int nrem_vals_;
};

bool ValIterator::hasMore(void ) const
{
  return (nrem_vals_ > 0);
}

int ValIterator::value(void ) const
{
  return value_;
}

std::size_t ValIterator::size(void ) const
{
  return nvals_;
}

} // namespace ESMCI

typedef class ESMCI::ValIterator * ESMCI_ValIteratorHnd;
#define ESMCI_VAL_ITERATOR_HND_INVALID NULL

#endif // ESMCI_VALITERATOR_H_INCLUDED
