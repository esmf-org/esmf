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
#define ESMF_FILENAME "ESMF_VecUtils.F90"

!
! ESMF VecUtils Module
!
! (all lines between the !BOP and !EOP markers will be included in the
! automated document processing.)
!------------------------------------------------------------------------------
! one blank line for protex processing - in case all routines here are
! marked internal (BOPI/EOPI), the output file will still have contents.

MODULE ESMF_VecUtilsMod
  INTERFACE
    FUNCTION CreateVec(buf_sz, val_desc_hnd) &
      BIND(C, NAME='create_vec')
      USE ISO_C_BINDING
      IMPLICIT NONE
      INTEGER (C_SIZE_T), VALUE :: buf_sz
      TYPE(C_PTR), VALUE :: val_desc_hnd
      TYPE(C_PTR) :: CreateVec
    END FUNCTION CreateVec
  END INTERFACE

  INTERFACE
    FUNCTION InitVec(buf, buf_sz, buf_index_iter_hnd, buf_val_desc_hnd) &
      BIND(C, NAME='init_vec')
      USE ISO_C_BINDING
      IMPLICIT NONE
      TYPE(C_PTR), VALUE :: buf
      INTEGER (C_SIZE_T), VALUE :: buf_sz
      TYPE(C_PTR), VALUE :: buf_index_iter_hnd
      TYPE(C_PTR), VALUE :: buf_val_desc_hnd
      INTEGER(C_INT) :: InitVec
    END FUNCTION InitVec
  END INTERFACE

  INTERFACE
    FUNCTION SplitVec(buf, buf_sz, split_vecs, split_vec_index_iters, num_split_vecs) &
      BIND(C, NAME='split_vec')
      USE ISO_C_BINDING
      IMPLICIT NONE
      TYPE(C_PTR), VALUE :: buf
      INTEGER (C_SIZE_T), VALUE :: buf_sz
      TYPE(C_PTR) :: split_vecs(*)
      TYPE(C_PTR) :: split_vec_index_iters(*)
      INTEGER(C_INT), VALUE :: num_split_vecs 
      INTEGER(C_INT) :: SplitVec 
    END FUNCTION SplitVec
  END INTERFACE

  INTERFACE
    FUNCTION JoinVec(buf, buf_sz, sub_vecs, sub_vec_join_index_iters, num_sub_vecs) &
      BIND(C, NAME='join_vec')
      USE ISO_C_BINDING
      IMPLICIT NONE
      TYPE(C_PTR), VALUE :: buf
      INTEGER (C_SIZE_T), VALUE :: buf_sz
      TYPE(C_PTR) :: sub_vecs(*)
      TYPE(C_PTR) :: sub_vec_join_index_iters(*)
      INTEGER(C_INT), VALUE :: num_sub_vecs 
      INTEGER(C_INT) :: JoinVec 
    END FUNCTION JoinVec
  END INTERFACE

  INTERFACE
    SUBROUTINE DeleteVec(buf) &
      BIND(C, NAME='delete_vec')
      USE ISO_C_BINDING
      IMPLICIT NONE
      TYPE(C_PTR), VALUE :: buf
    END SUBROUTINE DeleteVec
  END INTERFACE
END MODULE ESMF_VecUtilsMod
