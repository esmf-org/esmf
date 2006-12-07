!
! Earth System Modeling Framework
! Copyright 2002-2008, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
! F77 interface files for C++ layer calling into F90 implementation layer.
! This cannot use any F90 syntax in the API such as F90 array syntax,
! intent() or optional attributes!
!
!==============================================================================
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!==============================================================================
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
!      character(*), parameter, private :: version = &
!      '$Id: ESMF_LocalAlloc_C.F90,v 1.8 2006/12/07 23:23:18 theurich Exp $'
!==============================================================================
subroutine f_esmf_localarrayf90allocate(array, rank, type, kind, counts, &
  lbounds, ubounds, rc)
  use ESMF_UtilTypesMod     ! ESMF base class
  use ESMF_BaseMod          ! ESMF base class
  use ESMF_LocalArrayMod

  type(ESMF_LocalArray) :: array
  integer :: rank
  type(ESMF_DataType) :: type
  type(ESMF_DataKind) :: kind
  integer :: counts(rank)
  integer :: lbounds(rank)
  integer :: ubounds(rank)
  integer, intent(out), optional :: rc     

  ! Beware - these args are not in the same order
  call ESMF_LocalArrConstrF90Ptr(array, counts, rank, type, kind, &
    lbounds, ubounds, rc=rc)
end subroutine f_esmf_localarrayf90allocate


subroutine f_esmf_localarrayf90deallocate(array, rank, type, kind, rc)
  use ESMF_UtilTypesMod     ! ESMF base class
  use ESMF_BaseMod          ! ESMF base class
  use ESMF_LocalArrayMod

  type(ESMF_LocalArray) :: array
  integer :: rank
  type(ESMF_DataType) :: type
  type(ESMF_DataKind) :: kind
  integer, intent(out), optional :: rc     

  call ESMF_LocalArrayF90Deallocate(array, rank, type, kind, rc=rc)
end subroutine f_esmf_localarrayf90deallocate


subroutine f_esmf_localarrayadjust(array, rank, type, kind, counts, &
  lbounds, ubounds, rc)
  use ESMF_UtilTypesMod    ! ESMF base class
  use ESMF_BaseMod    ! ESMF base class
  use ESMF_LocalArrayMod
  
  type(ESMF_LocalArray) :: array
  integer :: rank
  type(ESMF_DataType) :: type
  type(ESMF_DataKind) :: kind
  integer :: counts(rank)
  integer :: lbounds(rank)
  integer :: ubounds(rank)
  integer, intent(out) :: rc     

  call ESMF_LocalArrayAdjust(array, counts, rank, type, kind, &
    lbounds, ubounds,rc=rc)
end subroutine f_esmf_localarrayadjust
