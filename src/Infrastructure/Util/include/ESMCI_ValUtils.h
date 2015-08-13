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
// ESMF Val Iterator Utils include file
//
//-----------------------------------------------------------------------------
//

#ifndef ESMCI_VALUTILS_H_INCLUDED
#define ESMCI_VALUTILS_H_INCLUDED

extern "C"{

int split_val_iterator(ESMCI_ValIteratorHnd val_desc_hnd,
                        int split_color_vec[],
                        std::size_t split_color_vec_sz,
                        ESMCI_ValIteratorHnd split_val_hnds[],
                        int num_split_val_hnds);

/*
int split_val_iterator(ESMCI_ValIteratorHnd val_desc_hnd,
                        int split_color_vec[],
                        std::size_t split_color_vec_sz,
                        int *split_vecs[],
                        int num_split_vecs);
*/

} // extern "C"

#endif // ESMCI_VALUTILS_H_INCLUDED
