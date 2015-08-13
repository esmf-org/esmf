#ifndef ESMCI_VALITERATORCINTERFACE_H_INCLUDED
#define ESMCI_VALITERATORCINTERFACE_H_INCLUDED

#include "ESMCI_ValIterator.h"
#include <stdlib.h>

// C interfaces to value iterator functions
extern "C"{
  /*
   * Returns handle to a value iterator that iterates over the
   * range [start, end] with the specified stride
   */
  ESMCI_ValIteratorHnd CreateRangeValIterator(int start, int end, int stride);
  /*
   * Returns handle to a value iterator that iterates over values
   * specified in the vector, vec, provided in the input arguments.
   * The iterator iterates over 'nvals' values.
   * If nvals > vec_sz (size of vec), the iterator resets to the
   * beginning after iterating every vec_sz values 
   */
  ESMCI_ValIteratorHnd CreateRepVecValIterator(int *vec, size_t vec_sz, int nvals);
  /*
   * Returns handle to a value iterator that iterates over values
   * determined by the vector, vec, passed in the input arguments.
   * The values in 'vec' are assumed to belong to the range
   * [0, vec_range_dist-1], considered as one of the sub-ranges
   * of the entire range, of distance 'iter_range_dist', of values
   * traversed by the iterator. The values in 'vec' is interpolated
   * to the current sub-range traversed by the iterator.
   */
  ESMCI_ValIteratorHnd CreateIncVecValIterator(int *vec, size_t vec_sz,
                    size_t vec_range_dist, size_t iter_range_dist);
  /* 
   * Reset a value iterator  
   * Return value == 0 => success, failure otherwise
   */
  int ResetValIterator(ESMCI_ValIteratorHnd hnd);
  /*
   * Print the contents of the value iterator
   */
  void PrintValIterator(ESMCI_ValIteratorHnd hnd);
  /*
   * Delete a value iterator
   */
  void DeleteValIterator(ESMCI_ValIteratorHnd hnd);
  /*
   * Get the size (number of values iterated by) of the iterator
   */
  std::size_t GetValIteratorSize(ESMCI_ValIteratorHnd hnd);
};

#endif /* ESMCI_VALITERATORCINTERFACE_H_INCLUDED */
