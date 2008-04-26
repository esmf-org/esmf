! $Id: ESMF_F95PtrUTest.F90,v 1.1 2008/04/26 23:13:15 w6ws Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_F95PTRUTest

!------------------------------------------------------------------------------

#include <ESMF_Macros.inc>

!==============================================================================
!BOP
! !PROGRAM: ESMF_F95PtrUTest - Check the sizes of F95 pointers for consistency
!
! !DESCRIPTION:
!
! The code in this file checks the sizes of several F95 pointers to different
! typed objects to ensure the sizes do not unexpectedly change.  Some places
! within ESMF have space allocated to store F95 pointers, and this test ensures
! that the sizes are consistent - regardless of which derived types the pointers
! point to.
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF_Mod
  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id: ESMF_F95PtrUTest.F90,v 1.1 2008/04/26 23:13:15 w6ws Exp $'
!------------------------------------------------------------------------------

  integer, parameter :: int8_k = selected_int_kind (12)		! 8-byte integer
  
  ! cumulative result: count failures; no failures equals "all pass"
  
  integer :: result = 0

  ! individual test result code
  integer :: rc

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name
  
  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  !-----------------------------------------------------------------------------

  call ptr_size_test ()

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

contains

  subroutine ptr_size_test ()

! Compare F90 pointer sizes for simple base variables vs simple UDTs (those
! which only have base variable types) vs UDTs with pointers in them.

! Note that technically, a pointer to a simple scalar variable is a
! F2003 feature.

    integer, parameter :: chars_l = 256
    integer, parameter :: pad_k = int8_k	! 8-byte integer

    integer(pad_k) :: real_begin
    real, pointer :: real_ptr
    character :: real_chars(chars_l), real_endchar
    equivalence (real_begin, real_chars)
    common /realcom/ real_begin, real_ptr, real_endchar

    integer(pad_k) :: char_begin
    character(32), pointer :: char_ptr
    character :: char_chars(chars_l), char_endchar
    equivalence (char_begin, char_chars)
    common /charcom/ char_begin, char_ptr, char_endchar

    type simple_udt
      sequence
      real :: a, b, c
      integer :: i, j, k
    end type

    integer(pad_k) :: udt_begin
    type(simple_udt), pointer :: udt_ptr
    character :: udt_chars(chars_l), udt_endchar
    equivalence (udt_begin, udt_chars)
    common /udtcom/ udt_begin, udt_ptr, udt_endchar

    type bigger_udt
      sequence
      real :: a, b, c
      integer :: i, j, k
      type(simple_udt) :: udt
      type(simple_udt), pointer :: udt_p
      character(100) :: string
    end type

    integer(pad_k) :: biggerudt_begin
    type(bigger_udt), pointer :: biggerudt_ptr
    character :: biggerudt_chars(chars_l), biggerudt_endchar
    equivalence (biggerudt_begin, biggerudt_chars)
    common /biggerudtcom/ biggerudt_begin, biggerudt_ptr, biggerudt_endchar

#if defined (ENABLE_EMSF_UDT_TEST)
    integer(pad_k) :: vm_begin
    type (ESMF_VM), pointer :: vm_ptr
    character :: vm_chars(chars_l), vm_endchar
    equivalence (vm_begin, vm_chars)
    common /vmcom/ vm_begin, vm_ptr, vm_endchar

    integer(pad_k) :: base_begin
    type (ESMF_Base), pointer :: base_ptr
    character :: base_chars(chars_l), base_endchar
    equivalence (base_begin, base_chars)
    common /basecom/ base_begin, base_ptr, base_endchar
#endif

    integer :: realptr_l, charptr_l, udtptr_l, biggerudtptr_l
    integer :: vmptr_l, baseptr_l


  !-----------------------------------------------------------------------------
  ! First obtain a simple F95 pointer for comparison purposes
  
    print *, 'pointer to scalar REAL:'
    real_chars = achar (0)
    real_endchar = achar (1)
    realptr_l = maxloc (iachar (real_chars), dim=1) - 9
    print *, '  F95 pointer-to-scalar length =', realptr_l

  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
    write(name, *) "Pointer to CHARACTER string"
    write(failMsg, *) "Pointer size changed!"    
    char_chars = achar (0)
    char_endchar = achar (1)
    charptr_l = maxloc (iachar (char_chars), dim=1) - 9
    call ESMF_Test((charptr_l == realptr_l), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
    write(name, *) "Pointer to simple Plain Old Data derived type"
    write(failMsg, *) "Pointer size changed!"
    udt_chars = achar (0)
    udt_endchar = achar (1)
    udtptr_l = maxloc (iachar (udt_chars), dim=1) - 9
    call ESMF_Test((udtptr_l == realptr_l), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
    write(name, *) "Pointer to more complex derived type"
    write(failMsg, *) "Pointer size changed!"
    biggerudt_chars = achar (0)
    biggerudt_endchar = achar (1)
    biggerudtptr_l = maxloc (iachar (biggerudt_chars), dim=1) - 9
    call ESMF_Test((biggerudtptr_l == realptr_l), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

#if defined (ENABLE_ESMF_UDT_TEST)
! Note that these currently do not work on some compilers because they contain
! F95 component initialization, so can not be placed into COMMON blocks.  What
! is strange is that we only want to place pointers to the derived types, but
! the compilers are rejecting them anyway.  It is still TBD whether this is a
! constraint in the Standard, or an oversight in the compilers.

  !-----------------------------------------------------------------------------
  !NEX_UTest
    write(name, *) "Pointer to ESMF_VM type"
    write(failMsg, *) "Pointer size changed!"
    vm_chars = achar (0)
    vm_endchar = achar (1)
    vmptr_l = maxloc (iachar (vm_chars), dim=1) - 9
    call ESMF_Test((vmptr_l == realptr_l), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
    write(name, *) "Pointer to ESMF_Base type"
    write(failMsg, *) "Pointer size changed!"
    base_chars = achar (0)
    base_endchar = achar (1)
    baseptr_l = maxloc (iachar (base_chars), dim=1) - 9
    call ESMF_Test((baseptr_l == realptr_l), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
#endif
  
  end subroutine
  
end program
