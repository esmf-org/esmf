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
