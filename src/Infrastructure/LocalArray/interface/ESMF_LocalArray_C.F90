! $Id$
!
! Earth System Modeling Framework
! Copyright (c) 2002-2025, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_LocalArray_C.F90"
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
!      '$Id$'
!==============================================================================

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_localarrayf90allocate"
subroutine f_esmf_localarrayf90allocate(arrayPtr, rank, typekind, counts, &
  lbounds, ubounds, rc)
  use ESMF_UtilTypesMod     ! ESMF base class
  use ESMF_BaseMod          ! ESMF base class
  use ESMF_LogErrMod        ! ESMF error logging
  use ESMF_LocalArrayMod
  
  implicit none

  type(ESMF_Pointer) :: arrayPtr
  integer :: rank
  type(ESMF_TypeKind_Flag) :: typekind
  integer :: counts(rank)
  integer :: lbounds(rank)
  integer :: ubounds(rank)
  integer :: rc

  type(ESMF_LocalArray) :: array

  array%this = arrayPtr
  ESMF_INIT_SET_CREATED(array)

  ! Beware - these args are not in the same order
  call ESMF_LocalArrConstrF90Ptr(array, counts, typekind, rank, &
    lbounds, ubounds, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, &
    ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT)) return

end subroutine f_esmf_localarrayf90allocate


#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_localarrayf90deallocate"
subroutine f_esmf_localarrayf90deallocate(arrayPtr, rank, typekind, rc)
  use ESMF_UtilTypesMod     ! ESMF base class
  use ESMF_BaseMod          ! ESMF base class
  use ESMF_LogErrMod        ! ESMF error logging
  use ESMF_LocalArrayMod

  implicit none

  type(ESMF_Pointer) :: arrayPtr
  integer :: rank
  type(ESMF_TypeKind_Flag) :: typekind
  integer :: rc

  type(ESMF_LocalArray) :: array

  array%this = arrayPtr
  ESMF_INIT_SET_CREATED(array)

  call ESMF_LocalArrayF90Deallocate(array, typekind, rank, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, &
    ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT)) return

end subroutine f_esmf_localarrayf90deallocate


#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_localarrayadjust"
subroutine f_esmf_localarrayadjust(arrayPtr, rank, typekind, counts, &
  lbounds, ubounds, rc)
  use ESMF_UtilTypesMod     ! ESMF base class
  use ESMF_BaseMod          ! ESMF base class
  use ESMF_LogErrMod        ! ESMF error logging
  use ESMF_LocalArrayMod
  
  implicit none

  type(ESMF_Pointer) :: arrayPtr
  integer :: rank
  type(ESMF_TypeKind_Flag) :: typekind
  integer :: counts(rank)
  integer :: lbounds(rank)
  integer :: ubounds(rank)
  integer :: rc

  type(ESMF_LocalArray) :: array

  array%this = arrayPtr
  ESMF_INIT_SET_CREATED(array)

  call ESMF_LocalArrayAdjust(array, counts, typekind, rank, &
    lbounds, ubounds,rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, &
    ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT)) return

end subroutine f_esmf_localarrayadjust


#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_localarrayslice"
subroutine f_esmf_localarrayslice(arrayPtr, trailingTensorSlice, rankIn, rankOut, rc)
  use ESMF_UtilTypesMod     ! ESMF base class
  use ESMF_BaseMod          ! ESMF base class
  use ESMF_LogErrMod        ! ESMF error logging
  use ESMF_LocalArrayMod
  use ESMF_F90InterfaceMod

  implicit none

  type(ESMF_Pointer) :: arrayPtr
  type(ESMF_InterArray) :: trailingTensorSlice
  integer :: rankIn, rankOut
  integer :: rc

  type(ESMF_LocalArray) :: array

  array%this = arrayPtr         ! the incoming LocalArray
  ESMF_INIT_SET_CREATED(array)

  call ESMF_LocalArraySlice(array, trailingTensorSlice, rankIn, rankOut, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, &
    ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT)) return

  arrayPtr = array%this         ! the outgoing LocalArray

end subroutine f_esmf_localarrayslice


#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_localarraycopyf90ptr"
subroutine f_esmf_localarraycopyf90ptr(arrayInPtr, arrayOutPtr, datacopyflag, rc)
  use ESMF_UtilTypesMod     ! ESMF base class
  use ESMF_BaseMod          ! ESMF base class
  use ESMF_LogErrMod        ! ESMF error logging
  use ESMF_LocalArrayMod
  
  implicit none

  type(ESMF_Pointer)        :: arrayInPtr
  type(ESMF_Pointer)        :: arrayOutPtr
  type(ESMF_DataCopy_Flag)  :: datacopyflag
  integer                   :: rc
  
  type(ESMF_LocalArray) :: arrayIn
  type(ESMF_LocalArray) :: arrayOut
  
  ! Very important: the pointers passed from C and used as references for
  ! arrayInArg and arrayOutArg are simple pointers to pointers from the C side.
  ! This means that there is no memory for what the F90 INITMACROS are using
  ! at that location! In order to deal with this C<->F90 difference local
  ! F90 variables are necessary to work on the F90 side and this glue code will
  ! copy the "this" member in the derived type which is the part that actually
  ! needs to be passed between C and F90.

  arrayIn%this = arrayInPtr
  ESMF_INIT_SET_CREATED(arrayIn)

  arrayOut%this = arrayOutPtr

  ! do the actual copy, allocating the required memory
  call ESMF_LocalArrayCopyF90Ptr(arrayIn, arrayOut, datacopyflag=datacopyflag, &
    rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, &
    ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT)) return
  
end subroutine f_esmf_localarraycopyf90ptr


#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_localarrayctof90"
subroutine f_esmf_localarrayctof90(arrayPtr, cptr, rank, typekind, counts, &
  lbounds, ubounds, rc)
  use ESMF_UtilTypesMod     ! ESMF base class
  use ESMF_BaseMod          ! ESMF base class
  use ESMF_LogErrMod        ! ESMF error logging
  use ESMF_LocalArrayMod
  use ISO_C_BINDING
  
  implicit none

  type(ESMF_Pointer)        :: arrayPtr
  type(C_PTR)               :: cptr
  integer                   :: rank
  type(ESMF_TypeKind_Flag)  :: typekind
  integer                   :: counts(rank)
  integer                   :: lbounds(rank)
  integer                   :: ubounds(rank)
  integer                   :: rc

  type(ESMF_LocalArray)     :: array

  array%this = arrayPtr
  ESMF_INIT_SET_CREATED(array)

  ! Beware - these args are not in the same order
  call ESMF_LocalArrCToF90Ptr(array, cptr, counts, typekind, rank, &
    lbounds, ubounds, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, &
    ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT)) return

end subroutine f_esmf_localarrayctof90
