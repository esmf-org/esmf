! $Id: ESMF_F95PtrUTest.F90,v 1.2.2.8 2009/01/21 21:25:25 cdeluca Exp $
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
program ESMF_F95PTRUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"

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
    '$Id: ESMF_F95PtrUTest.F90,v 1.2.2.8 2009/01/21 21:25:25 cdeluca Exp $'
!------------------------------------------------------------------------------

  integer, parameter :: int8_k = selected_int_kind (12)		! 8-byte integer
  
  ! cumulative result: count failures; no failures equals "all pass"
  
  integer :: result = 0

  ! individual test result code
  integer :: rc

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name
 
  ! Make sure the BLOCK DATA gets linked in

  external :: ESMF_F95PtrBData
 
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

! The COMMON blocks have been initialized in BLOCK DATA with CHARACTER
! arrays initialized to contain byte offsets.  To measure the size of
! a pointer, place it in the common block.  Then follow the pointer with
! a single character.  Thanks to storage association, the trailing
! character will contain the length of the preceding object.

    real, pointer :: real_ptr
    character :: real_endchar
    common /realcom/ real_ptr, real_endchar

    character(32), pointer :: char_ptr
    character :: char_endchar
    common /charcom/ char_ptr, char_endchar

    type simple_udt
      sequence
      real :: a, b, c
      integer :: i, j, k
    end type

    type(simple_udt), pointer :: udt_ptr
    character :: udt_endchar
    common /udtcom/ udt_ptr, udt_endchar

    type bigger_udt
      sequence
      real :: a, b, c
      integer :: i, j, k
      type(simple_udt) :: udt
      type(simple_udt), pointer :: udt_p
      character(100) :: string
    end type

    type(bigger_udt), pointer :: biggerudt_ptr
    character :: biggerudt_endchar
    common /biggerudtcom/ biggerudt_ptr, biggerudt_endchar

#if defined (ENABLE_ESMF_UDT_TEST)
    type (ESMF_VM), pointer :: vm_ptr
    character :: vm_endchar
    common /vmcom/ vm_ptr, vm_endchar

    type (ESMF_Base), pointer :: base_ptr
    character :: base_endchar
    common /basecom/ base_ptr, base_endchar
#endif

    integer :: realptr_l, charptr_l, udtptr_l, biggerudtptr_l
#if defined (ENABLE_ESMF_UDT_TEST)
    integer :: vmptr_l, baseptr_l
#endif

  !-----------------------------------------------------------------------------
  ! NEX_disable_UTest
  ! First obtain a simple F95 pointer for comparison purposes
  
    write (name,*) 'pointer to scalar REAL'
    write (failMsg,*) 'Pointer length is not a positive, non-zero, integer!'
    realptr_l = ichar (real_endchar)
    print *, '  F95 pointer-to-scalar length =', realptr_l
    call ESMF_Test ((realptr_l > 0), name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_notest_UTest
    write(name, *) "Pointer to CHARACTER string"
    write(failMsg, *) "Pointer size changed!"    
    charptr_l = ichar (char_endchar)
    print *, '  F95 pointer-to-characterString length =', charptr_l
    !call ESMF_Test((charptr_l == realptr_l), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_notest_UTest
    write(name, *) "Pointer to simple Plain Old Data derived type"
    write(failMsg, *) "Pointer size changed!"
    udtptr_l = ichar (udt_endchar)
    print *, '  F95 pointer-to-simpleUDT length =', udtptr_l
    !call ESMF_Test((udtptr_l == realptr_l), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_disable_UTest
    write(name, *) "Compare pointer size between simple and bigger UDT"
    write(failMsg, *) "Pointer size changed between UDTs!"
    biggerudtptr_l = ichar (biggerudt_endchar)
    print *, '  F95 pointer-to-biggerUDT length =', biggerudtptr_l
    call ESMF_Test((biggerudtptr_l == udtptr_l), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

#if defined (ENABLE_ESMF_UDT_TEST)
  ! These currently do not work on some compilers because the ESMF derived
  ! types either contain F95 component initialization, or do not have a
  ! SEQUENCE statement.  This prevents them from being placed into
  ! COMMON blocks.  Note that the Standard does not even allow a pointer to
  ! such types to reside in COMMON.  (See Constraint 589 in §5.5.2 of F2003.)

  !-----------------------------------------------------------------------------
  !NEX_disabled_UTest
    write(name, *) "Pointer to ESMF_VM type"
    write(failMsg, *) "Pointer size changed!"
    vmptr_l = ichar (vm_endchar)
    call ESMF_Test((vmptr_l == realptr_l), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_disabled_UTest
    write(name, *) "Pointer to ESMF_Base type"
    write(failMsg, *) "Pointer size changed!"
    baseptr_l = ichar (base_endchar)
    call ESMF_Test((baseptr_l == realptr_l), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
#endif
  
  end subroutine
  
end program
