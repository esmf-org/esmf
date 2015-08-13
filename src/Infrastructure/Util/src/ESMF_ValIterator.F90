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
#define ESMF_FILENAME "ESMF_ValIterator.F90"

!
! ESMF ValIterator Module
!
! (all lines between the !BOP and !EOP markers will be included in the
! automated document processing.)
!------------------------------------------------------------------------------
! one blank line for protex processing - in case all routines here are
! marked internal (BOPI/EOPI), the output file will still have contents.

MODULE ESMF_ValIteratorMod
  INTERFACE
    FUNCTION CreateRangeValIterator(start, end, stride) &
      BIND(C, NAME='CreateRangeValIterator')
      USE ISO_C_BINDING
      IMPLICIT NONE
      INTEGER (C_INT), VALUE :: start
      INTEGER (C_INT), VALUE :: end
      INTEGER (C_INT), VALUE :: stride
      TYPE(C_PTR) :: CreateRangeValIterator
    END FUNCTION CreateRangeValIterator
  END INTERFACE

  INTERFACE
    FUNCTION CreateRepVecValIterator(vec, vec_sz, nvals) &
      BIND(C, NAME='CreateRepVecValIterator')
      USE ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE :: vec
      INTEGER (C_SIZE_T), VALUE :: vec_sz
      INTEGER (C_INT), VALUE :: nvals
      TYPE(C_PTR) :: CreateRepVecValIterator
    END FUNCTION CreateRepVecValIterator
  END INTERFACE

  INTERFACE
    FUNCTION CreateIncVecValIterator(vec, vec_sz, vec_range_dist, iter_range_dist) &
      BIND(C, NAME='CreateIncVecValIterator')
      USE ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE :: vec
      INTEGER (C_SIZE_T), VALUE :: vec_sz
      INTEGER (C_SIZE_T), VALUE :: vec_range_dist
      INTEGER (C_SIZE_T), VALUE :: iter_range_dist
      TYPE(C_PTR) :: CreateIncVecValIterator
    END FUNCTION CreateIncVecValIterator
  END INTERFACE

  INTERFACE
    SUBROUTINE PrintValIterator(hnd) &
      BIND(C, NAME='PrintValIterator')
      USE ISO_C_BINDING
      IMPLICIT NONE
      TYPE(C_PTR), VALUE :: hnd
    END SUBROUTINE PrintValIterator
  END INTERFACE

  INTERFACE
    SUBROUTINE DeleteValIterator(hnd) &
      BIND(C, NAME='DeleteValIterator')
      USE ISO_C_BINDING
      IMPLICIT NONE
      TYPE(C_PTR), VALUE :: hnd
    END SUBROUTINE DeleteValIterator
  END INTERFACE

  INTERFACE
    FUNCTION GetValIteratorSize(hnd) &
      BIND(C, NAME='GetValIteratorSize')
      USE ISO_C_BINDING
      IMPLICIT NONE
      TYPE(C_PTR), VALUE :: hnd
      INTEGER(C_SIZE_T) :: GetValIteratorSize
    END FUNCTION GetValIteratorSize
  END INTERFACE
END MODULE ESMF_ValIteratorMod
