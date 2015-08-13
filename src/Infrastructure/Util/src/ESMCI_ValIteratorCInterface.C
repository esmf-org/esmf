#include "ESMCI_ValIteratorCInterface.h"
#include <stdio.h>
#include <stdlib.h>

ESMCI_ValIteratorHnd CreateRangeValIterator(int start, int end, int stride)
{
  return (new ESMCI::ValIterator(static_cast<int>(start), end, stride));
}

ESMCI_ValIteratorHnd CreateRepVecValIterator(int *vec, size_t vec_sz, int nvals)
{
  return (new ESMCI::ValIterator(static_cast<int *>(vec), 
                                static_cast<std::size_t>(vec_sz), nvals));
}

ESMCI_ValIteratorHnd CreateIncVecValIterator(int *vec, size_t vec_sz,
                    size_t vec_range_dist, size_t iter_range_dist)
{
  return (new ESMCI::ValIterator(vec, static_cast<std::size_t>(vec_sz),
                                static_cast<std::size_t>(vec_range_dist),
                                static_cast<std::size_t>(iter_range_dist)));
}

int ResetValIterator(ESMCI_ValIteratorHnd hnd)
{
  if(hnd == NULL){
    printf("ERROR: Invalid (NULL) value iterator, cannot reset...\n");
    return -1;
  }
  hnd->reset();
  return 0;
}

void PrintValIterator(ESMCI_ValIteratorHnd hnd)
{
  int ret = 0;
  if(hnd == NULL){
    printf("ERROR: Invalid (NULL) value iterator, cannot reset...\n");
    return;
  }
  hnd->reset();
  printf("\n");
  while(hnd->hasMore()){
    printf("%d, ",hnd->value());
    ++(*hnd);
  }
  printf("\n");
  hnd->reset();
}

void DeleteValIterator(ESMCI_ValIteratorHnd hnd)
{
  if(hnd != NULL){
    delete hnd;
  }
}

std::size_t GetValIteratorSize(ESMCI_ValIteratorHnd hnd)
{
  if(hnd == NULL){
    printf("ERROR: Invalid (NULL) value iterator, cannot find size...\n");
    return 0;
  }
  return hnd->size();
}
