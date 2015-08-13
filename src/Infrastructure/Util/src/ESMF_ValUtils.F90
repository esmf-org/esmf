! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2015, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
#define ESMF_FILENAME "ESMF_ValUtils.F90"

!
! ESMF ValUtils Module
!
! (all lines between the !BOP and !EOP markers will be included in the
! automated document processing.)
!------------------------------------------------------------------------------
! one blank line for protex processing - in case all routines here are
! marked internal (BOPI/EOPI), the output file will still have contents.

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
