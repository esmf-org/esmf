MODULE ESMF_ValUtilsMod
  INTERFACE
    FUNCTION SplitValIterator(val_desc_hnd, split_color_vec, &
                              split_color_vec_sz, split_val_hnds, &
                              num_split_val_hnds) &
      BIND(C, NAME='split_val_iterator')
      USE ISO_C_BINDING
      IMPLICIT NONE
      TYPE(C_PTR), VALUE :: val_desc_hnd
      INTEGER(C_INT) :: split_color_vec(*)
      INTEGER(C_SIZE_T), VALUE :: split_color_vec_sz
      TYPE(C_PTR) :: split_val_hnds(*)
      INTEGER(C_INT), VALUE :: num_split_val_hnds
      INTEGER(C_INT) :: SplitValIterator
    END FUNCTION SplitValIterator
  END INTERFACE
END MODULE ESMF_ValUtilsMod
