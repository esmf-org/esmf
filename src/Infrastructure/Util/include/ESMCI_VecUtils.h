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
// ESMF Vector/buffer Utils include file
//
//-----------------------------------------------------------------------------
//

#ifndef ESMCI_VECUTILS_H_INCLUDED
#define ESMCI_VECUTILS_H_INCLUDED

extern "C"{
int *create_vec(std::size_t buf_sz, ESMCI_ValIteratorHnd val_desc_hnd);
int init_vec(int *buf, std::size_t buf_sz, 
            ESMCI_ValIteratorHnd buf_index_iter_hnd,
            ESMCI_ValIteratorHnd buf_val_desc_hnd);
int split_vec(int *buf, std::size_t buf_sz,
              int *split_vecs[],
              ESMCI_ValIteratorHnd split_vec_index_iters[],
              int num_split_vecs);
int join_vec(int *buf, std::size_t buf_sz,
              int *sub_vecs[],
              ESMCI_ValIteratorHnd sub_vec_join_index_iters[],
              int num_sub_vecs);
void delete_vec(int *buf);

} // extern "C"

#endif // ESMCI_VECUTILS_H_INCLUDED
