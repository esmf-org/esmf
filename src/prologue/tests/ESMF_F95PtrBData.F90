! $Id: ESMF_F95PtrBData.F90,v 1.1.2.2 2009/01/21 21:25:25 cdeluca Exp $
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

block data ESMF_F95PtrBData
  implicit none

  !-----------------------------------------------------------------------------
  ! Initialize the common blocks for ESMF_F95PtrUTest.
  !
  ! The common blocks are defined here as arrays of CHARACTER.  Each
  ! element of the array is initialized to a value corresponding to
  ! its position in the array.
  !
  ! Note that the CHARACTER types used here are intentionally different
  ! than how the common blocks are used in the main program, so that
  ! the sizes of other objects can be measured by looking at the value
  ! of the byte following the object.

  integer, parameter :: char_max = 256

  integer :: i

  character :: real_chars(char_max) = (/ (char (i),i=0, char_max-1) /)
  common /realcom/ real_chars

  character :: char_chars(char_max) = (/ (char (i),i=0, char_max-1) /)
  common /charcom/ char_chars

  character :: udt_chars(char_max) = (/ (char (i), i=0, char_max-1) /)
  common /udtcom/ udt_chars

  character :: biggerudt_chars(char_max) = (/ (char (i),i=0, char_max-1) /)
  common /biggerudtcom/ biggerudt_chars

#if defined (ENABLE_ESMF_UDT_TEST)
  character :: vm_chars(char_max) = (/ (char (i), i=0, char_max-1) /)
  common /vmcom/ vm_chars

  character :: base_chars(char_max) = (/ (char (i), i=0, char_max-1) /)
  common /basecom/ base_chars
#endif

end block data
