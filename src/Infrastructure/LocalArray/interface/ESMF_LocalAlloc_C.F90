!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research, 
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
!      '$Id: ESMF_LocalAlloc_C.F90,v 1.12.2.4 2009/01/21 21:25:22 cdeluca Exp $'
!==============================================================================
subroutine f_esmf_localarrayf90allocate(array, rank, kind, counts, &
  lbounds, ubounds, rc)
  use ESMF_UtilTypesMod     ! ESMF base class
  use ESMF_BaseMod          ! ESMF base class
  use ESMF_LocalArrayMod

  type(ESMF_LocalArray) :: array
  integer :: rank
  type(ESMF_TypeKind) :: kind
  integer :: counts(rank)
  integer :: lbounds(rank)
  integer :: ubounds(rank)
  integer :: rc

  ! Beware - these args are not in the same order
  call ESMF_LocalArrConstrF90Ptr(array, counts, rank, kind, &
    lbounds, ubounds, rc=rc)
end subroutine f_esmf_localarrayf90allocate


subroutine f_esmf_localarrayf90deallocate(array, rank, kind, rc)
  use ESMF_UtilTypesMod     ! ESMF base class
  use ESMF_BaseMod          ! ESMF base class
  use ESMF_LocalArrayMod

  type(ESMF_LocalArray) :: array
  integer :: rank
  type(ESMF_TypeKind) :: kind
  integer :: rc

  call ESMF_LocalArrayF90Deallocate(array, rank, kind, rc=rc)
end subroutine f_esmf_localarrayf90deallocate


subroutine f_esmf_localarrayadjust(array, rank, kind, counts, &
  lbounds, ubounds, rc)
  use ESMF_UtilTypesMod    ! ESMF base class
  use ESMF_BaseMod    ! ESMF base class
  use ESMF_LocalArrayMod
  
  type(ESMF_LocalArray) :: array
  integer :: rank
  type(ESMF_TypeKind) :: kind
  integer :: counts(rank)
  integer :: lbounds(rank)
  integer :: ubounds(rank)
  integer :: rc

  call ESMF_LocalArrayAdjust(array, counts, rank, kind, &
    lbounds, ubounds,rc=rc)
end subroutine f_esmf_localarrayadjust


subroutine f_esmf_localarraycopyf90ptr(arrayInArg, arrayOutArg, rc)
  use ESMF_UtilTypesMod    ! ESMF base class
  use ESMF_BaseMod    ! ESMF base class
  use ESMF_LocalArrayMod
  
  type(ESMF_LocalArray) :: arrayInArg
  type(ESMF_LocalArray) :: arrayOutArg
  integer :: rc
  
  type(ESMF_LocalArray) :: arrayIn
  type(ESMF_LocalArray) :: arrayOut
  
  ! Very important: the pointers passed from C and used as references for
  ! arrayInArg and arrayOutArg are simple pointers to pointers from the C side.
  ! This means that there is no memory for what the F90 INITMACROS are using
  ! at that location! In order to deal with this C<->F90 difference local
  ! F90 variables are necessary to work on the F90 side and this glue code will
  ! copy the "this" member in the derived type which is the part that actually
  ! needs to be passed between C and F90.
  
  arrayIn%this = arrayInArg%this    ! only access "this" member
  ! need to set the valid init code 
  ESMF_INIT_SET_CREATED(arrayIn)
  
  arrayOut%this = arrayOutArg%this  ! only access "this" member

  ! do the actual copy, allocating the required memory
  call ESMF_LocalArrayCopyF90Ptr(arrayIn, arrayOut, rc=rc)
  
end subroutine f_esmf_localarraycopyf90ptr
