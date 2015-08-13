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
#define ESMC_FILENAME "ESMCI_VecUtils.C"
//==============================================================================

#include "ESMCI_ValIterator.h"
#include "ESMCI_VecUtils.h"
#include <cstddef>
#include <cstdlib>
#include <iostream>

int *create_vec(std::size_t buf_sz, ESMCI_ValIteratorHnd val_desc_hnd)
{
  int *buf = NULL;
  if(val_desc_hnd == NULL){
    std::cerr << "Error: Invalid (NULL) val iterator provided\n";
    return NULL;
  }
  buf = static_cast<int *>(calloc(buf_sz, sizeof(int)));
  if(buf == NULL){
    std::cerr << "Error: malloc() failed\n";
    return NULL;
  }
  for(int i=0; (i < buf_sz) && (val_desc_hnd->hasMore()); i++, ++(*val_desc_hnd)){
    buf[i] = val_desc_hnd->value();
  }
  return buf;
}

int init_vec(int *buf, std::size_t buf_sz,
            ESMCI_ValIteratorHnd buf_index_iter_hnd,
            ESMCI_ValIteratorHnd buf_val_desc_hnd)
{
  ESMCI::ValIterator default_index_iter(0, (int )buf_sz-1, 1);
  if(buf == NULL){
    std::cerr << "Error: Invalid (NULL) user buffer specified\n";
    return -1;
  }
  if(buf_sz <= 0){
    std::cerr << "Error: Invalid user buffer length ("<< buf_sz << ") provided\n";
    return -1;
  }
  if(buf_index_iter_hnd == NULL){
    std::cout << "Warning: No buffer index iterator provided, iterating one by one now\n";
    buf_index_iter_hnd = &default_index_iter;
  }
  if(buf_val_desc_hnd == NULL){
    std::cerr << "Error: Invalid (NULL) Value description iterator provided\n";
    return -1;
  }

  for(;(buf_index_iter_hnd->hasMore()) && (buf_val_desc_hnd->hasMore());
      ++(*buf_index_iter_hnd), ++(*buf_val_desc_hnd)){
    int index = buf_index_iter_hnd->value();
    int val = buf_val_desc_hnd->value();
    //std::cout << "DEBUG: (index, val) = ("<< index << "," << val <<")\n";
    if(index >= buf_sz){
      std::cerr << "Error: Buffer access outofbounds, aborting\n";
      return -1;
    }
    buf[index] = val;
  }
  return 0;
}

int split_vec(int *buf, std::size_t buf_sz,
              int *split_vecs[],
              ESMCI_ValIteratorHnd split_vec_index_iters[],
              int num_split_vecs)
{
  if(buf == NULL){
    std::cerr << "Error: Invalid (NULL) user buffer specified\n";
    return -1;
  }
  if(buf_sz <= 0){
    std::cerr << "Error: Invalid user buffer length ("<< buf_sz << ") provided\n";
    return -1;
  }
  if(split_vecs == NULL){
    std::cerr << "Error: Invalid (NULL) array for holding the split vectors\n";
    return -1;
  }
  if(num_split_vecs <= 0){
    std::cerr << "Error: Invalid number of split vectors (" 
              << num_split_vecs << ") specified\n";
    return -1;
  }
  for(int i=0; i<num_split_vecs; i++){
    if(split_vec_index_iters[i] == NULL){
      std::cerr << "Error: Invalid (NULL) split vector index iterator at split_vec_index_iters[" << i << "]\n";
      return -1;
    }
    if(split_vecs[i] == NULL){
      std::cerr << "Error: Invalid (NULL) split vector buffer at split_vecs[" << i << "]\n";
      return -1;
    }
  }
  for(int i=0; i<num_split_vecs; i++){
    std::size_t split_vec_sz = split_vec_index_iters[i]->size();
    for(int j=0;  (j < split_vec_sz) &&
                  (split_vec_index_iters[i]->hasMore());
                  j++, ++(*split_vec_index_iters[i])){
      std::size_t buf_index = split_vec_index_iters[i]->value();
      if(buf_index >= buf_sz){
        std::cerr << "Error: Buffer access outofbounds, aborting\n";
        return -1;
      }
      int value = buf[buf_index];
      (split_vecs[i])[j] = value;
    }
  }
  return 0;
}

int join_vec(int *buf, std::size_t buf_sz,
              int *sub_vecs[],
              ESMCI_ValIteratorHnd sub_vec_join_index_iters[],
              int num_sub_vecs)
{
  if(buf == NULL){
    std::cerr << "Error: Invalid (NULL) join user buffer specified\n";
    return -1;
  }
  if(buf_sz <= 0){
    std::cerr << "Error: Invalid join user buffer length ("<< buf_sz << ") provided\n";
    return -1;
  }
  if(sub_vecs == NULL){
    std::cerr << "Error: Invalid (NULL) array of sub vectors provided\n";
    return -1;
  }
  if(num_sub_vecs <= 0){
    std::cerr << "Error: Invalid number of sub vectors (" 
              << num_sub_vecs << ") specified\n";
    return -1;
  }
  for(int i=0; i<num_sub_vecs; i++){
    if(sub_vec_join_index_iters[i] == NULL){
      std::cerr << "Error: Invalid (NULL) join vector index iterator at sub_vec_join_index_iters[" << i << "]\n";
      return -1;
    }
    if(sub_vecs[i] == NULL){
      std::cerr << "Error: Invalid (NULL) sub vector buffer at sub_vecs[" << i << "]\n";
      return -1;
    }
  }
  for(int i=0; i<num_sub_vecs; i++){
    std::size_t sub_vec_sz = sub_vec_join_index_iters[i]->size();
    for(int j=0;(j < sub_vec_sz) &&
            (sub_vec_join_index_iters[i]->hasMore());
            j++, ++(*sub_vec_join_index_iters[i])){
      std::size_t buf_index = sub_vec_join_index_iters[i]->value();
      if(buf_index >= buf_sz){
        std::cerr << "Error: Buffer access outofbounds, aborting\n";
        return -1;
      }
      buf[buf_index] = (sub_vecs[i])[j];
    }
  }
  return 0;
}

void delete_vec(int *buf)
{
  free(buf);
}

