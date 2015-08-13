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
#define ESMC_FILENAME "ESMCI_ValUtils.C"
//==============================================================================

#include "ESMCI_ValIterator.h"
#include "ESMCI_ValUtils.h"
#include <cstddef>
#include <cstdlib>
#include <iostream>
#include <set>
#include <map>

// FIXME: Util functions that read a val handle desc should reset
// it before returning - so that the user does not have to explicitly
// reset it.
int split_val_iterator(ESMCI_ValIteratorHnd val_desc_hnd,
                        int split_color_vec[],
                        std::size_t split_color_vec_sz,
                        ESMCI_ValIteratorHnd split_val_hnds[],
                        int num_split_val_hnds)
{
  if(val_desc_hnd == NULL){
    std::cerr << "Invalid (NULL) value descriptor handle provided\n";
    return -1;
  }
  if(split_color_vec_sz <= 0){
    std::cerr << "Invalid split color vec, size=" << split_color_vec_sz << "\n";
    return -1;
  }
  if(num_split_val_hnds <= 0){
    std::cerr << "Invalid split val hnd array, size=" << num_split_val_hnds << "\n";
    return -1;
  }

  std::set<int> unprocessed_colors;
  std::map<int, int> color_to_hnd_array;
  std::size_t split_color_vec_idx =0;
  std::size_t split_val_hnds_idx = 0; 
  std::vector<std::vector<int> > split_val_vecs(num_split_val_hnds);
  while(val_desc_hnd->hasMore()){
    std::map<int, int>::iterator color_to_hnd_array_iter;
    std::size_t color_to_hnd_array_idx = 0;
    
    color_to_hnd_array_iter = color_to_hnd_array.find(split_color_vec[split_color_vec_idx]);
    bool is_unprocessed_color = false;
    if(color_to_hnd_array_iter != color_to_hnd_array.end()){
      color_to_hnd_array_idx = color_to_hnd_array_iter->second; 
    }
    else{
      // See if we can find a free index
      if(split_val_hnds_idx < num_split_val_hnds){
        color_to_hnd_array_idx = split_val_hnds_idx++;
        // Add the new index to the color to hnd map
        color_to_hnd_array[split_color_vec[split_color_vec_idx]] = color_to_hnd_array_idx;
        std::vector<int> tmp(0);
        split_val_vecs.push_back(tmp);
      }
      else{
        is_unprocessed_color = true;
      }
    }
    if(!is_unprocessed_color){
      split_val_vecs[color_to_hnd_array_idx].push_back(val_desc_hnd->value());
    }
    else{
      unprocessed_colors.insert(split_color_vec[split_color_vec_idx]);
    }

    split_color_vec_idx++;
    if(split_color_vec_idx >= split_color_vec_sz){
      // Reset
      split_color_vec_idx = 0;
    } 
    ++(*val_desc_hnd);
  }

  // Convert the split val vecs to val desc
  for(int i=0; i<num_split_val_hnds; i++){
    if(!split_val_vecs[i].empty()){
      split_val_hnds[i] = new ESMCI::ValIterator(&((split_val_vecs[i])[0]),
                                split_val_vecs[i].size(),
                                split_val_vecs[i].size());
    }
    else{
      break;
    }
  }

  if(!unprocessed_colors.empty()){
    std::cerr << "The following colors were not processed:\n";
    for(std::set<int>::const_iterator iter = unprocessed_colors.begin();
        iter != unprocessed_colors.end();
        ++iter){
      std::cerr << *iter << ", ";
    }
    std::cerr << "\n";
    return -1;
  }
  else{
    return 0;
  }
}

int split_val_iterator(ESMCI_ValIteratorHnd val_desc_hnd,
                        int split_color_vec[],
                        std::size_t split_color_vec_sz,
                        int *split_vecs[],
                        int num_split_vecs)
{
  std::cerr << "Not implemented...\n";
  return -1;  
}
