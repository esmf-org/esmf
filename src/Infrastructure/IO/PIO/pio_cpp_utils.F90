#include "ESMFPIO.h"
#define __PIO_FILE__ "pio_cpp_utils.F90"
! ---------------------------------------------------------------------

!  utility procedures for use by cpp binding functions
module pio_cpp_utils

  use pio_kinds, only: i4, r4, r8, pio_offset
  use pio_types,   only : iosystem_desc_t

  implicit none

   !  explicit export

  private

  ! Utility functions for managing C handles for iosystem_desc_t instances

  type, public :: PIO_C_HANDLE_NODE
    integer :: c_handle_start
    type(iosystem_desc_t), pointer :: PIO_descriptors(:)
    type(PIO_C_HANDLE_NODE), pointer :: next
   end type PIO_C_HANDLE_NODE

   type(PIO_C_HANDLE_NODE), private, save, pointer :: PIO_Intracom_handles => null ()
   integer, private :: PIO_c_handle_num = 0

   ! public interface

  public :: f_chars
  public :: c_chars
  public :: c_len
  public :: max_string_len
  public :: max_path_len

  public :: new_pio_iosys_handles
  public :: get_pio_iosys_handle
  public :: delete_pio_iosys_handle
  !  public :: delete_all_pio_iosys_handles

  !  constants

  integer, parameter :: max_string_len = 1024
  integer, parameter :: max_path_len = 1024

! ---------------------------------------------------------------------
!  library

 contains

! ---------------------------------------------------------------------
!  remove the null that ends a C string

pure subroutine f_chars(fc, cs)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_char

  !  dummy arguments

  character(kind= c_char, len= *), intent(in) :: cs
  character(len= *), intent(out) :: fc

  !  local
  integer :: i

  !  text
  continue

  convert_kind: do i = 1, min(len(fc), len(cs))
    fc(i: i) = char(ichar(cs(i: i)))
  end do convert_kind

  return

end subroutine f_chars

! ---------------------------------------------------------------------

! ---------------------------------------------------------------------

!  copy a Fortran string to C, inserting a NULL

pure subroutine c_chars(cs, fc)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_char, C_NULL_CHAR

  !  dummy arguments

  character(len= *), intent(in) :: fc
  character(kind= c_char, len= *), intent(out) :: cs

  !  local
  integer :: i
  integer :: len_fc

  !  text
  continue

  len_fc = len_trim(fc)
  convert_kind: do i = 1, len_fc
    cs(i: i) = fc(i: i)
  end do convert_kind
  if (len_fc < len (cs)) then
    cs(len_fc+1:len_fc+1) = C_NULL_CHAR
  else
    cs(len (cs):len (cs)) = C_NULL_CHAR
  end if

  return

end subroutine c_chars

! ---------------------------------------------------------------------

!  return the length of the character data in a C string

pure function c_len(cs) result(cl)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_char, c_null_char

  !  result
  integer :: cl

  !  dummy arguments
  character(kind= c_char, len= *), intent(in) :: cs

  !  text
  continue

  cl = index(cs, c_null_char) - 1

  return

end function c_len

! ---------------------------------------------------------------------
!  Obtain a PIO iosystem_desc_t object given its integer handle

subroutine new_pio_iosys_handles(iosystem_handles, iosystem)

  use pio_types,   only : iosystem_desc_t
  use pio_support, only : piodie, debug

  !  dummy arguments
  integer, intent(inout) :: iosystem_handles(:)
  type(iosystem_desc_t), pointer :: iosystem(:)

  ! local
  type(PIO_C_HANDLE_NODE), pointer :: new_pio_c_handle_node
  type(PIO_C_HANDLE_NODE), pointer :: pio_handle_node
  integer :: stat
  integer :: num_handles
  integer :: i

  num_handles = size(iosystem_handles, 1)

  ! First, create a new iosystem handle node
  allocate(new_pio_c_handle_node, stat=stat)
  if (stat .ne. 0) then
    call piodie(__PIO_FILE__,__LINE__,       &
         'unable to allocate PIO_C_HANDLE_NODE')
  endif
  nullify(new_pio_c_handle_node%next)
  ! Now, create the new iosystem_desc_t array
  allocate(new_pio_c_handle_node%PIO_descriptors(num_handles), stat=stat)
  if (stat .ne. 0) then
    deallocate(new_pio_c_handle_node)
    call piodie(__PIO_FILE__,__LINE__,'unable to allocate iosystem_desc_t')
  endif
  ! Fill in C starting handle number and increment
  new_pio_c_handle_node%c_handle_start = PIO_c_handle_num
  PIO_c_handle_num = PIO_c_handle_num + num_handles
  ! Find the end of the chain and insert new node
  if (.not. associated(PIO_Intracom_handles)) then
    PIO_Intracom_handles => new_pio_c_handle_node
  else
    pio_handle_node => PIO_Intracom_handles
    do while (associated(pio_handle_node%next))
      pio_handle_node => pio_handle_node%next
    end do
    pio_handle_node%next => new_pio_c_handle_node
  end if
  ! Set the PIO iosystem_desc_t output
  iosystem => new_pio_c_handle_node%PIO_descriptors
  ! Fill in the c handle numbers
  do i = 1, num_handles
    iosystem_handles(i) = new_pio_c_handle_node%c_handle_start + i
  end do

end subroutine new_pio_iosys_handles

!  Obtain a PIO iosystem_desc_t object given its integer handle

subroutine get_pio_iosys_handle(iosystem_handle, iosystem)

  use pio_types,   only : iosystem_desc_t
  use pio_support, only : piodie, debug

  !  dummy arguments
  integer, intent(in) :: iosystem_handle
  type(iosystem_desc_t), pointer :: iosystem

  ! local
  logical :: found_handle
  type(PIO_C_HANDLE_NODE), pointer :: pio_handle_node
  integer :: num_handles
  integer :: handle0

  continue
  ! Search for a structure with the correct handle number
  pio_handle_node => PIO_Intracom_handles
  found_handle = .false.
  do while (associated(pio_handle_node))
    num_handles = size(pio_handle_node%PIO_descriptors, 1)
    handle0 = pio_handle_node%c_handle_start
    if ((iosystem_handle .gt. handle0) .and.   &
         (iosystem_handle .le. (handle0 + num_handles))) then
      iosystem => pio_handle_node%PIO_descriptors(iosystem_handle - handle0)
      found_handle = .true.
      exit
    elseif (associated(pio_handle_node%next)) then
      pio_handle_node => pio_handle_node%next
    else
      nullify(pio_handle_node)
    end if
  end do

  if (.not. found_handle) then
    print *,__PIO_FILE__,__LINE__,'No descriptor for ',iosystem_handle
    call piodie(__PIO_FILE__,__LINE__,'Could not find descriptor')
  end if

end subroutine get_pio_iosys_handle

!  Delete a node containing a PIO iosystem object and corresponding C handle

subroutine delete_pio_iosys_handle(iosystem_handle)

  use pio_types,   only : iosystem_desc_t
  use pio_support, only : piodie, debug

  !  dummy arguments
  integer, intent(in) :: iosystem_handle
  logical :: found_handle

  ! local
  type(PIO_C_HANDLE_NODE), pointer :: pio_handle_node
  type(PIO_C_HANDLE_NODE), pointer :: prev_handle_node
  integer :: stat
  integer :: num_handles
  integer :: handle0

  nullify(prev_handle_node)
  ! Search for a structure with the correct handle number
  pio_handle_node => PIO_Intracom_handles
  found_handle = .false.
  do while (associated(pio_handle_node))
    num_handles = size(pio_handle_node%PIO_descriptors, 1)
    handle0 = pio_handle_node%c_handle_start
    if ((iosystem_handle .gt. handle0) .and.   &
         (iosystem_handle .le. (handle0 + num_handles))) then
      if (num_handles .gt. 1) then
        print *,__PIO_FILE__,__LINE__,'WARNING: deleting ',   &
             num_handles,'handles'
      end if
      ! Remove node from list
      if (associated(prev_handle_node)) then
        prev_handle_node%next => pio_handle_node%next
      else
        PIO_Intracom_handles => pio_handle_node%next
      end if
      ! Clear the starting handle (probably unneccessary)
      pio_handle_node%c_handle_start = -1
      ! Now, delete the iosystem_desc_t array
      deallocate(pio_handle_node%PIO_descriptors, stat=stat)
      if (stat .ne. 0) then
        print *,__PIO_FILE__,__LINE__,'Error ',stat,  &
             ' deallocating iosystem descriptor(s)'
      endif
      ! Finally, delete the iosystem handle node
      deallocate(pio_handle_node, stat=stat)
      if (stat .ne. 0) then
        print *,__PIO_FILE__,__LINE__,'Error ',stat,  &
             ' deallocating iosystem node'
      endif
      found_handle = .true.
      exit
    else
      prev_handle_node => pio_handle_node
      pio_handle_node => pio_handle_node%next
    end if
  end do

  if (.not. found_handle) then
    print *,__PIO_FILE__,__LINE__,'No descriptor for ',iosystem_handle
  end if

end subroutine delete_pio_iosys_handle

end module pio_cpp_utils
