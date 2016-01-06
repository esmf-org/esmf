#include "ESMFPIO.h"
#define __PIO_FILE__ "pio_cpp_binding.F90"
! ---------------------------------------------------------------------

!  procedures for cpp bindings to functions in piolib_mod

! ---------------------------------------------------------------------
!  the extern "C" functions()

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_init_intracom(int comp_rank, int comp_comm,
!                                        int num_tasks, int num_aggregator,
!                                        int stride, int rearr,
!                                        int* iosystem, int base);

subroutine pio_cpp_init_intracom_int(comp_rank, comp_comm, num_iotasks,      &
                                     num_aggregator, stride, rearr,          &
                                     iosystem_handle, base) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr

  !  get the required utilities
  use pio_cpp_utils, only: new_pio_iosys_handles

  !  import pio kinds
  use pio_kinds, only: i4

  !  import pio types
  use pio_types, only: iosystem_desc_t

  !  import pio procedure signatures
  use piolib_mod, only: pio_init

  implicit none

  !  dummy arguments
  integer(c_int), value :: comp_rank
  integer(c_int), value :: comp_comm
  integer(c_int), value :: num_iotasks
  integer(c_int), value :: num_aggregator
  integer(c_int), value :: stride
  integer(c_int), value :: rearr
  integer(c_int), intent(inout) :: iosystem_handle
  integer(c_int), value :: base

  !  local

  type(iosystem_desc_t), pointer :: iosystem_desc_p(:)
  integer, target :: iosystem_handles(1)

  !  text
  continue

  !  get a new iosystem_desc_t for this connection
  call new_pio_iosys_handles(iosystem_handles, iosystem_desc_p)

  !  call the Fortran procedure
  call pio_init(int(comp_rank, i4), int(comp_comm, i4),              &
       int(num_iotasks, i4), int(num_aggregator, i4),       &
       int(stride, i4), int(rearr, i4), iosystem_desc_p(1), &
       int(base, i4))

  ! Set the output C handle
  iosystem_handle = iosystem_handles(1)

  return

end subroutine pio_cpp_init_intracom_int

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_init_intercom(int component_count, int peer_comm,
!                                        int* comp_comms, int io_comm,
!                                        int** iosystem);

subroutine pio_cpp_init_intercom_int(component_count, peer_comm, comp_comms,  &
                                     io_comm, iosystem_handles) bind(c)

  !  get the required utilities
  use esmfpio_cpp_utils, only: new_pio_iosys_handles

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr

  !  import pio types
  use esmfpio_types, only: iosystem_desc_t

  !  import pio procedure signatures
  use esmfpiolib_mod, only: pio_init

  implicit none

  !  dummy arguments
  integer(c_int), value :: component_count
  integer(c_int), value :: peer_comm
  integer(c_int), dimension(component_count), intent(in) :: comp_comms
  integer(c_int), value :: io_comm
  integer(c_int), intent(inout) :: iosystem_handles(component_count)

  !  local

  integer, allocatable, target :: iosystem_handle_array(:)
  type(iosystem_desc_t), pointer :: iosystem_desc_p(:)
  integer :: i

  !  text
  continue

  !  get a new iosystem_desc_t for this connection
  allocate (iosystem_handle_array(int (component_count)))
  call new_pio_iosys_handles(iosystem_handle_array, iosystem_desc_p)

  !  call the Fortran procedure
  call pio_init(int(component_count), int(peer_comm), int(comp_comms),        &
       int(io_comm), iosystem_desc_p)

  ! Set the output C handles
  do i = 1, component_count
    iosystem_handles(i) = iosystem_handle_array(i)
  end do

  return

end subroutine pio_cpp_init_intercom_int

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_finalize(int* iosystem, int* ierror);

subroutine pio_cpp_finalize(iosystem_handle, ierr) bind(c)

  !  get the required utilities
  use esmfpio_cpp_utils, only: get_pio_iosys_handle, delete_pio_iosys_handle

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr

  !  import pio kinds
  use esmfpio_kinds, only: i4

  !  import pio types
  use esmfpio_types, only: iosystem_desc_t

  !  import pio procedure signatures
  use esmfpiolib_mod, only: pio_finalize

  implicit none

  !  dummy arguments
  integer(c_int), intent(inout) :: iosystem_handle
  integer(c_int), intent(out) :: ierr

  !  local
  integer(i4) :: ierror
  type(iosystem_desc_t), pointer :: iosystem_desc_p

  !  text
  continue

  !  get the iosystem_desc_t for this connection
  call get_pio_iosys_handle(iosystem_handle, iosystem_desc_p)

!  call MPI_Barrier(iosystem_desc_p%comp_comm, ierror);
  !  call the Fortran procedure
  call pio_finalize(iosystem_desc_p, ierror)

  ! Delete the iosystem descriptor
!  call MPI_Barrier(iosystem_desc_p%comp_comm, ierror);
  call delete_pio_iosys_handle(iosystem_handle)

  !  convert the arguments back to C
  ierr = int(ierror, c_int)
  iosystem_handle = -1

  !  return to the cpp caller
  return

end subroutine pio_cpp_finalize

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_initdecomp_dof_io(int* iosystem, int basepiotype,
!                                            int* dims, int ndims,
!                                            pio_dof_t* compdof, int ncompdof,
!                                            void* iodesc, pio_dof_t* iostart,
!                                            int niostart, pio_dof_t* iocount,
!                                            int niocount);

subroutine pio_cpp_initdecomp_dof_io(iosystem_handle, basepiotype, dims,      &
                                     ndims, compdof, ncompdof, iodesc,        &
                                     iostart, niostart, iocount, niocount)    &
                                     bind(c)

  !  get the required utilities
  use esmfpio_cpp_utils, only: get_pio_iosys_handle
  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_int64_t, c_ptr, c_f_pointer

  !  import pio kinds
  use esmfpio_kinds, only: i4, pio_offset

  !  import pio types
  use esmfpio_types, only: iosystem_desc_t, io_desc_t

  !  import pio procedure signatures
  use esmfpiolib_mod, only: pio_initdecomp

  implicit none

  !  dummy arguments
  integer(c_int), intent(in) :: iosystem_handle
  integer(c_int), value :: basepiotype
  type(c_ptr), value :: dims
  integer(c_int), value :: ndims
  type(c_ptr), value :: compdof
  integer(c_int), value :: ncompdof
  type(c_ptr), value :: iodesc
  type(c_ptr), value :: iostart
  integer(c_int), value :: niostart
  type(c_ptr), value :: iocount
  integer(c_int), value :: niocount

  !  local
  type(iosystem_desc_t), pointer :: iosystem_desc_p
  integer(c_int), dimension(:), pointer :: as_dims
  integer(c_int64_t), dimension(:), pointer :: as_compdof
  type(io_desc_t), pointer :: iodesc_desc
  integer(c_int64_t), dimension(:), pointer :: as_iostart
  integer(c_int64_t), dimension(:), pointer :: as_iocount

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(dims, as_dims, shape= [ ndims ])
  call c_f_pointer(compdof, as_compdof, shape= [ ncompdof ])
  call c_f_pointer(iodesc, iodesc_desc)
  call c_f_pointer(iostart, as_iostart, shape= [ niostart ])
  call c_f_pointer(iocount, as_iocount, shape= [ niocount ])

  !  get the iosystem_desc_t for this connection
  call get_pio_iosys_handle(iosystem_handle, iosystem_desc_p)

  !  call the Fortran procedure (passing optional arguments if passed in)
  call pio_initdecomp(iosystem_desc_p, int(basepiotype, i4),                  &
                      int(as_dims, i4), int(as_compdof, pio_offset),          &
                      iodesc_desc, int(as_iostart, pio_offset),               &
                      int(as_iocount, pio_offset))

  !  return to the cpp caller
  return

end subroutine pio_cpp_initdecomp_dof_io

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_initdecomp_dof(int* iosystem, int basepiotype,
!                                         int* dims, int ndims,
!                                         pio_dof_t* compdof, int ncompdof,
!                                         void* iodesc);

subroutine pio_cpp_initdecomp_dof(iosystem_handle, basepiotype, dims,         &
                                  ndims, compdof, ncompdof, iodesc)           &
                                  bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_int64_t, c_ptr, c_f_pointer

  !  get the required utilities
  use esmfpio_cpp_utils, only: get_pio_iosys_handle

  !  import pio kinds
  use esmfpio_kinds, only: i4, pio_offset

  !  import pio types
  use esmfpio_types, only: iosystem_desc_t, io_desc_t

  !  import pio procedure signatures
  use esmfpiolib_mod, only: pio_initdecomp

  implicit none

  !  dummy arguments
  integer(c_int), intent(in) :: iosystem_handle
  integer(c_int), value      :: basepiotype
  type(c_ptr),    value      :: dims
  integer(c_int), value      :: ndims
  type(c_ptr),    value      :: compdof
  integer(c_int), value      :: ncompdof
  type(c_ptr),    value      :: iodesc

  !  local
  type(iosystem_desc_t), pointer :: iosystem_desc_p
  integer(c_int),        pointer :: as_dims(:)
  integer(c_int64_t),    pointer :: as_compdof(:)
  type(io_desc_t),       pointer :: iodesc_desc

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(dims, as_dims, shape= [ ndims ])
  call c_f_pointer(compdof, as_compdof, shape= [ ncompdof ])
  call c_f_pointer(iodesc, iodesc_desc)

  !  get the iosystem_desc_t for this connection
  call get_pio_iosys_handle(iosystem_handle, iosystem_desc_p)

  !  call the Fortran procedure (passing optional arguments if passed in)
  call pio_initdecomp(iosystem_desc_p, int(basepiotype, i4),                  &
       int(as_dims, i4), int(as_compdof, pio_offset),          &
       iodesc_desc)

  !  return to the cpp caller
  return

end subroutine pio_cpp_initdecomp_dof

#if 0
! ****** Not yet implemented  *****
! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_initdecomp_dof_dof(int* iosystem, int basepiotype,
!                                             int* dims, int ndims,
!                                             int* compdof, int ncompdof,
!                                             void* iodesc,
!                                             int* iodof, int niodof);

subroutine pio_cpp_initdecomp_dof_dof(iosystem_handle, basepiotype, dims,     &
                                      ndims, compdof, ncompdof, iodesc,       &
                                      iodof, niodof) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

  !  get the required utilities
  use esmfpio_cpp_utils, only: get_pio_iosys_handle

  !  import pio kinds
  use esmfpio_kinds, only: i4, pio_offset

  !  import pio types
  use esmfpio_types, only: iosystem_desc_t, io_desc_t

  !  import pio procedure signatures
  use esmfpiolib_mod, only: pio_initdecomp

  implicit none

  !  dummy arguments
  integer(c_int), intent(in) :: iosystem_handle
  integer(c_int), value :: basepiotype
  type(c_ptr), value :: dims
  integer(c_int), value :: ndims
  type(c_ptr), value :: compdof
  integer(c_int), value :: ncompdof
  type(c_ptr), value :: iodesc
  type(c_ptr), value :: iodof
  integer(c_int), value :: niodof

  !  local
  type(iosystem_desc_t), pointer :: iosystem_desc_p
  integer(c_int), dimension(:), pointer :: as_dims
  integer(c_int), dimension(:), pointer :: as_compdof
  type(io_desc_t), pointer :: iodesc_desc
  integer(c_int), dimension(:), pointer :: as_iodof

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(dims, as_dims, shape= [ ndims ])
  call c_f_pointer(compdof, as_compdof, shape= [ ncompdof ])
  call c_f_pointer(iodesc, iodesc_desc)
  call c_f_pointer(iodof, as_iodof, shape= [ niodof ])

  !  get the iosystem_desc_t for this connection
  call get_pio_iosys_handle(iosystem_handle, iosystem_desc_p)

  !  call the Fortran procedure
  call pio_initdecomp(iosystem_desc_p, int(basepiotype, i4),                  &
       int(as_dims, i4), int(as_compdof, pio_offset),          &
       iodesc_desc, int(as_iodof, pio_offset))

  !  return to the cpp caller
  return

end subroutine pio_cpp_initdecomp_dof_dof
#endif
! ****** Not yet implemented  *****

! ---------------------------------------------------------------------

!  extern "C" int pio_cpp_openfile(int* iosystem, void* file, int iotype,
!                                  char* fname, int mode);

function pio_cpp_openfile(iosystem_handle, file, iotype, fname, mode)         &
                          result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_f_pointer

  !  import pio types
  use esmfpio_types, only: iosystem_desc_t, file_desc_t

  !  import pio procedure signatures
  use esmfpiolib_mod, only: pio_openfile
  use esmfpio_cpp_utils, only: f_chars, c_len, max_path_len, get_pio_iosys_handle

  implicit none

  !  function result
  integer(c_int) :: ierr

  !  dummy arguments
  integer(c_int), intent(in) :: iosystem_handle
  type(c_ptr), value :: file
  integer(c_int), value :: iotype
  type(c_ptr), value :: fname
  integer(c_int), value :: mode

  !  local
  integer :: ierror

  type(iosystem_desc_t), pointer :: iosystem_desc_p
  type(file_desc_t), pointer :: file_desc
  character(kind= c_char, len= max_path_len), pointer :: c_filename
#ifdef ALLOC_CHARLEN_OK
  character(len= :), allocatable :: filename
#else
  character(len= max_path_len) :: filename
#endif

  integer :: clen

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(fname, c_filename)

  !  get the iosystem_desc_t for this connection
  call get_pio_iosys_handle(iosystem_handle, iosystem_desc_p)

  !  convert the C string to Fortran characters
  clen = c_len(c_filename)

#ifdef ALLOC_CHARLEN_OK
  allocate(filename, mold= filename(1: clen))
  call f_chars(filename, c_filename(1: clen))
#else
  filename = c_filename(1:clen)
#endif

  !  call the Fortran procedure
  ierror = pio_openfile(iosystem_desc_p, file_desc, int(iotype),              &
       trim(filename), int(mode))

  !  convert the arguments back to C
  ierr = int(ierror, c_int)

  !  return to the cpp caller
  return

end function pio_cpp_openfile

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_syncfile(void* file);

subroutine pio_cpp_syncfile(file) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer

  !  import pio types
  use esmfpio_types, only: file_desc_t

  !  import pio procedure signatures
  use esmfpiolib_mod, only: pio_syncfile

  implicit none

  !  dummy arguments
  type(c_ptr), value :: file

  !  local
  type(file_desc_t), pointer :: file_desc

  !  text
  continue

  !  convert the C pointer to a Fortran pointer
  call c_f_pointer(file, file_desc)

  !  call the Fortran procedure
  call pio_syncfile(file_desc)

  !  return to the cpp caller
  return

end subroutine pio_cpp_syncfile

! ---------------------------------------------------------------------
!  extern "C" int pio_cpp_createfile(int* iosystem, void* file, int iotype,
!                                    char* fname, int amode_in);

function pio_cpp_createfile(iosystem_handle, file, iotype, fname, amode_in)   &
                            result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_f_pointer

  !  import pio types
  use esmfpio_types, only: iosystem_desc_t, file_desc_t

  !  import pio procedure signatures
  use esmfpiolib_mod, only: pio_createfile
  use esmfpio_cpp_utils, only: f_chars, c_len, max_path_len, get_pio_iosys_handle

  implicit none

  !  function result
  integer(c_int) :: ierr

  !  dummy arguments
  integer(c_int), intent(in) :: iosystem_handle
  type(c_ptr), value :: file
  integer(c_int), value :: iotype
  type(c_ptr), value :: fname
  integer(c_int), value :: amode_in

  !  local
  integer :: ierror

  type(iosystem_desc_t), pointer :: iosystem_desc_p
  type(file_desc_t), pointer :: file_desc
  character(kind= c_char, len= max_path_len), pointer :: c_filename
#ifdef ALLOC_CHARLEN_OK
  character(len= :), allocatable :: filename
#else
  character(len= max_path_len) :: filename
#endif

  integer :: clen

  !  text
  continue

  !  convert the C pointers to Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(fname, c_filename)

  !  get the iosystem_desc_t for this connection
  call get_pio_iosys_handle(iosystem_handle, iosystem_desc_p)

  !  convert the C string to Fortran characters
  clen = c_len(c_filename)

#ifdef ALLOC_CHARLEN_OK
  allocate(filename, mold= filename(1: clen))
  call f_chars(filename, c_filename(1: clen))
#else
  filename = c_filename(1:clen)
#endif

  !  call the Fortran procedure
  ierror = pio_createfile(iosystem_desc_p, file_desc, int(iotype),            &
       trim(filename), int(amode_in))

  !  convert the arguments back to C
  ierr = int(ierror, c_int)

  !  return to the cpp caller
  return

end function pio_cpp_createfile

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_closefile(void* file);

subroutine pio_cpp_closefile(file) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer

  !  import pio types
  use esmfpio_types, only: file_desc_t

  !  import pio procedure signatures
  use esmfpiolib_mod, only: pio_closefile

  implicit none

  !  dummy arguments
  type(c_ptr), value :: file

  !  local
  type(file_desc_t), pointer :: file_desc

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)

  !  call the Fortran procedure
  call pio_closefile(file_desc)

  !  return to the cpp caller
  return

end subroutine pio_cpp_closefile

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_setiotype(void* file, int iotype, int rearr);

subroutine pio_cpp_setiotype(file, iotype, rearr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

  !  import pio kinds
  use esmfpio_kinds, only: i4

  !  import pio types
  use esmfpio_types, only: file_desc_t

  !  import pio procedure signatures
  use esmfpiolib_mod, only: pio_setiotype

  implicit none

  !  dummy arguments
  type(c_ptr), value :: file
  integer(c_int), value :: iotype
  integer(c_int), value :: rearr

  !  local
  type(file_desc_t), pointer :: file_desc

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)

  !  call the Fortran procedure
  call pio_setiotype(file_desc, int(iotype, i4), int(rearr, i4))

  !  return to the cpp caller
  return

end subroutine pio_cpp_setiotype

! ---------------------------------------------------------------------
!  extern "C" int pio_cpp_numtoread(void* iodesc);

function pio_cpp_numtoread(iodesc) result(num) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

  !  import pio types
  use esmfpio_types, only: io_desc_t

  !  import pio procedure signatures
  use esmfpiolib_mod, only: pio_numtoread

  implicit none

  !  function result
  integer(c_int) :: num

  !  dummy arguments
  type(c_ptr), value :: iodesc

  !  local
  integer :: n
  type(io_desc_t), pointer :: io_desc

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(iodesc, io_desc)

  !  call the Fortran procedure
  n = pio_numtoread(io_desc)

  !  convert the arguments back to C
  num = int(n, c_int)

  !  return to the cpp caller
  return

end function pio_cpp_numtoread

! ---------------------------------------------------------------------
!  extern "C" int pio_cpp_numtowrite(void* iodesc);

function pio_cpp_numtowrite(iodesc) result(num) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

  !  import pio types
  use esmfpio_types, only: io_desc_t

  !  import pio procedure signatures
  use esmfpiolib_mod, only: pio_numtowrite

  implicit none

  !  function result
  integer(c_int) :: num

  !  dummy arguments
  type(c_ptr), value :: iodesc

  !  local
  integer :: n
  type(io_desc_t), pointer :: io_desc

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(iodesc, io_desc)

  !  call the Fortran procedure
  n = pio_numtowrite(io_desc)

  !  convert the arguments back to C
  num = int(n, c_int)

  !  return to the cpp caller
  return

end function pio_cpp_numtowrite

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_setframe(void* vardesc, int frame);

subroutine pio_cpp_setframe(vardesc, frame) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

  !  import pio kinds
  use esmfpio_kinds, only: pio_offset

  !  import pio types
  use esmfpio_types, only: var_desc_t

  !  import pio procedure signatures
  use esmfpiolib_mod, only: pio_setframe

  implicit none

  !  dummy arguments
  type(c_ptr), value :: vardesc
  integer(c_int), value :: frame

  !  local
  type(var_desc_t), pointer :: var_desc

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(vardesc, var_desc)

  !  call the Fortran procedure
  call pio_setframe(var_desc, int(frame, pio_offset))

  !  return to the cpp caller
  return

end subroutine pio_cpp_setframe

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_advanceframe(void* vardesc);

subroutine pio_cpp_advanceframe(vardesc) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer

  !  import pio types
  use esmfpio_types, only: var_desc_t

  !  import pio procedure signatures
  use esmfpiolib_mod, only: pio_advanceframe

  implicit none

  !  dummy arguments
  type(c_ptr), value :: vardesc

  !  local
  type(var_desc_t), pointer :: var_desc

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(vardesc, var_desc)

  !  call the Fortran procedure
  call pio_advanceframe(var_desc)

  !  return to the cpp caller
  return

end subroutine pio_cpp_advanceframe

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_setdebuglevel(int level);

subroutine pio_cpp_setdebuglevel(level) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int

  !  import pio kinds
  use esmfpio_kinds, only: i4

  !  import pio procedure signatures
  use esmfpiolib_mod, only: pio_setdebuglevel

  implicit none

  !  dummy arguments
  integer(c_int), value :: level

  !  text
  continue

  !  call the Fortran procedure
  call pio_setdebuglevel(int(level, i4))

  !  return to the cpp caller
  return

end subroutine pio_cpp_setdebuglevel

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_seterrorhandlingf(void* file, int method);

subroutine pio_cpp_seterrorhandlingf(file, method) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

  !  import pio types
  use esmfpio_types, only: file_desc_t

  !  import pio procedure signatures
  use esmfpiolib_mod, only: pio_seterrorhandling

  implicit none

  !  dummy arguments
  type(c_ptr), value :: file
  integer(c_int), value :: method

  !  local
  type(file_desc_t), pointer :: file_desc

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)

  !  call the Fortran procedure
  call pio_seterrorhandling(file_desc, int(method))

  !  return to the cpp caller
  return

end subroutine pio_cpp_seterrorhandlingf

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_seterrorhandlingi(void* ios, int method);

subroutine pio_cpp_seterrorhandlingi(iosystem_handle, method) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

  !  get the required utilities
  use esmfpio_cpp_utils, only: get_pio_iosys_handle

  !  import pio types
  use esmfpio_types, only: iosystem_desc_t

  !  import pio procedure signatures
  use esmfpiolib_mod, only: pio_seterrorhandling

  implicit none

  !  dummy arguments
  integer(c_int), intent(in) :: iosystem_handle
  integer(c_int), value :: method

  !  local
  type(iosystem_desc_t), pointer :: iosystem_desc_p

  !  text
  continue

  !  get the iosystem_desc_t for this connection
  call get_pio_iosys_handle(iosystem_handle, iosystem_desc_p)

  !  call the Fortran procedure
  call pio_seterrorhandling(iosystem_desc_p, int(method))

  !  return to the cpp caller
  return

end subroutine pio_cpp_seterrorhandlingi

! ---------------------------------------------------------------------
!  extern "C" int pio_cpp_get_local_array_size(void* iodesc);

function pio_cpp_get_local_array_size(iodesc) result(siz) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

  !  import pio types
  use esmfpio_types, only: io_desc_t

  !  import pio procedure signatures
  use esmfpiolib_mod, only: pio_get_local_array_size

  implicit none

  !  function result
  integer(c_int) :: siz

  !  dummy arguments
  type(c_ptr), value :: iodesc

  !  local
  integer :: s
  type(io_desc_t), pointer :: io_desc

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(iodesc, io_desc)

  !  call the Fortran procedure
  s = pio_get_local_array_size(io_desc)

  !  convert the arguments back to C
  siz = int(s, c_int)

  !  return to the cpp caller
  return

end function pio_cpp_get_local_array_size

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_freedecomp_ios(int* iosystem, void* iodesc);

subroutine pio_cpp_freedecomp_ios(iosystem_handle, iodesc) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

  !  get the required utilities
  use esmfpio_cpp_utils, only: get_pio_iosys_handle

  !  import pio types
  use esmfpio_types, only: iosystem_desc_t, io_desc_t

  !  import pio procedure signatures
  use esmfpiolib_mod, only: pio_freedecomp

  implicit none

  !  dummy arguments
  integer(c_int), intent(in) :: iosystem_handle
  type(c_ptr), value :: iodesc

  !  local
  type(iosystem_desc_t), pointer :: iosystem_desc_p
  type(io_desc_t), pointer :: io_desc

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(iodesc, io_desc)

  !  get the iosystem_desc_t for this connection
  call get_pio_iosys_handle(iosystem_handle, iosystem_desc_p)

  ! Make sure we are all ready
!  call MPI_Barrier(iosystem_desc_p%comp_comm, ierror);

  !  call the Fortran procedure
  call pio_freedecomp(iosystem_desc_p, io_desc)

  ! Make sure we are all done
!  call MPI_Barrier(iosystem_desc_p%comp_comm, ierror);

  !  return to the cpp caller
  return

end subroutine pio_cpp_freedecomp_ios

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_freedecomp_file(void* file, void* iodesc);

subroutine pio_cpp_freedecomp_file(file, iodesc) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer

  !  import pio types
  use esmfpio_types, only: file_desc_t, io_desc_t

  !  import pio procedure signatures
  use esmfpiolib_mod, only: pio_freedecomp

  implicit none

  !  dummy arguments
  type(c_ptr), value :: file
  type(c_ptr), value :: iodesc

  !  local
  type(file_desc_t), pointer :: file_desc
  type(io_desc_t), pointer :: io_desc

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(iodesc, io_desc)

  !  call the Fortran procedure
  call pio_freedecomp(file_desc, io_desc)

  !  return to the cpp caller
  return

end subroutine pio_cpp_freedecomp_file

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_dupiodesc(void* src, void* dest);

subroutine pio_cpp_dupiodesc(src, dest) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer

  !  import pio types
  use esmfpio_types, only: io_desc_t

  !  import pio procedure signatures
  use esmfpiolib_mod, only: pio_dupiodesc

  !  dummy arguments
  type(c_ptr), value :: src
  type(c_ptr), value :: dest

  !  local
  type(io_desc_t), pointer :: src_desc
  type(io_desc_t), pointer :: dest_desc

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(src, src_desc)
  call c_f_pointer(dest, dest_desc)

  !  call the Fortran procedure
  call pio_dupiodesc(src_desc, dest_desc)

  !  return to the cpp caller
  return

end subroutine pio_cpp_dupiodesc

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_getnumiotasks(int* iosystem, int* numiotasks);

subroutine pio_cpp_getnumiotasks(iosystem_handle, numiotasks) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr

  !  get the required utilities
  use esmfpio_cpp_utils, only: get_pio_iosys_handle

  !  import pio kinds
  use esmfpio_kinds, only: i4

  !  import pio types
  use esmfpio_types, only: iosystem_desc_t

  !  import pio procedure signatures
  use esmfpiolib_mod, only: pio_getnumiotasks

  implicit none

  !  dummy arguments
  integer(c_int), intent(in) :: iosystem_handle
  integer(c_int) :: numiotasks

  !  local
  type(iosystem_desc_t), pointer :: iosystem_desc_p
  integer(i4) :: nt

  !  text
  continue

  !  get the iosystem_desc_t for this connection
  call get_pio_iosys_handle(iosystem_handle, iosystem_desc_p)

  !  call the Fortran procedure
  call pio_getnumiotasks(iosystem_desc_p, nt)

  !  convert the arguments back to C
  numiotasks = int(nt, c_int)

  !  return to the cpp caller
  return

end subroutine pio_cpp_getnumiotasks

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_set_hint(int* iosystem, void* hint, void* hintval);

subroutine pio_cpp_set_hint(iosystem_handle, hint, hintval) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_char, c_ptr, c_f_pointer

  !  import pio types
  use esmfpio_types, only: iosystem_desc_t

  !  import pio procedure signatures
  use esmfpiolib_mod, only: pio_set_hint
  use esmfpio_cpp_utils, only: f_chars, c_len, max_string_len, get_pio_iosys_handle

  implicit none

  !  dummy arguments
  integer(c_int), intent(in) :: iosystem_handle
  type(c_ptr), value :: hint
  type(c_ptr), value :: hintval

  !  local
  type(iosystem_desc_t), pointer :: iosystem_desc_p
  character(kind= c_char, len= max_string_len), pointer :: c_hint
  character(kind= c_char, len= max_string_len), pointer :: c_hintval

#ifdef ALLOC_CHARLEN_OK
  character(len= :), allocatable :: hint_str
  character(len= :), allocatable :: hintval_str
#else
  character(len= max_string_len) :: hint_str
  character(len= max_string_len) :: hintval_str
#endif

  integer :: clen

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(hint, c_hint)
  call c_f_pointer(hintval, c_hintval)

  !  get the iosystem_desc_t for this connection
  call get_pio_iosys_handle(iosystem_handle, iosystem_desc_p)

  !  convert the C string to Fortran characters
  clen = c_len(c_hint)
#ifdef ALLOC_CHARLEN_OK
  allocate(hint_str, mold= hint_str(1: clen))
  call f_chars(hint_str, c_hint(1: clen))
#else
  hint_str = c_hint(1:clen)
#endif

  clen = c_len(c_hintval)
#ifdef ALLOC_CHARLEN_OK
  allocate(hintval_str, mold= hintval_str(1: clen))
  call f_chars(hintval_str, c_hintval(1: clen))
#else
  hintval_str = c_hintval(1:clen)
#endif

  !  call the Fortran procedure
  call pio_set_hint(iosystem_desc_p, trim(hint_str), trim(hintval_str))

  !  return to the cpp caller
  return

end subroutine pio_cpp_set_hint

! ---------------------------------------------------------------------
!  extern "C" int pio_cpp_getnum_ost(int* iosystem);

function pio_cpp_getnum_ost(iosystem_handle) result(numost) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr

  !  get the required utilities
  use esmfpio_cpp_utils, only: get_pio_iosys_handle

  !  import pio types
  use esmfpio_types, only: iosystem_desc_t

  !  import pio procedure signatures
  use esmfpiolib_mod, only: pio_getnum_ost

  implicit none

  !  function result
  integer(c_int) :: numost

  !  dummy arguments
  integer(c_int), intent(in) :: iosystem_handle

  !  local
  type(iosystem_desc_t), pointer :: iosystem_desc_p
  integer :: n

  !  text

  continue

  !  get the iosystem_desc_t for this connection
  call get_pio_iosys_handle(iosystem_handle, iosystem_desc_p)

  !  call the Fortran procedure
  n = pio_getnum_ost(iosystem_desc_p)

  !  convert the arguments back to C
  numost = int(n, c_int)

  !  return to the cpp caller
  return

end function pio_cpp_getnum_ost

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_setnum_ost(int* iosystem, int numost);

subroutine pio_cpp_setnum_ost(iosystem_handle, numost) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr

  !  get the required utilities
  use esmfpio_cpp_utils, only: get_pio_iosys_handle

  !  import pio kinds
  use esmfpio_kinds, only: i4

  !  import pio types
  use esmfpio_types, only: iosystem_desc_t

  !  import pio procedure signatures
  use esmfpiolib_mod, only: pio_setnum_ost

  implicit none

  !  dummy arguments
  integer(c_int), intent(in) :: iosystem_handle
  integer(c_int) :: numost

  !  local
  type(iosystem_desc_t), pointer :: iosystem_desc_p

  !  text
  continue

  !  get the iosystem_desc_t for this connection
  call get_pio_iosys_handle(iosystem_handle, iosystem_desc_p)

  !  call the Fortran procedure
  call pio_setnum_ost(iosystem_desc_p, int(numost, i4))

  !  return to the cpp caller
  return

end subroutine pio_cpp_setnum_ost

! ---------------------------------------------------------------------

!  extern "C" int pio_cpp_file_is_open(void* file);

function pio_cpp_file_is_open(file) result(is_open) bind(c)

  !  bind to C
#if !defined (ESMF_PGI_C_BOOL_BUG)
  use, intrinsic :: iso_c_binding, only: c_bool, c_ptr, c_f_pointer
#else
  use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer
#endif

  !  import pio types
  use esmfpio_types, only: file_desc_t

  !  import pio procedure signatures
  use esmfpiolib_mod, only: pio_file_is_open

  implicit none

  !  function result
#if !defined (ESMF_PGI_C_BOOL_BUG)
  logical(c_bool) :: is_open
#else
  integer(4) :: is_open
#endif

  !  dummy arguments
  type(c_ptr), value :: file

  !  local
  logical :: o
  type(file_desc_t), pointer :: file_desc

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)

  !  call the Fortran procedure
  o = pio_file_is_open(file_desc)

  !  convert the arguments back to C
#if !defined (ESMF_PGI_C_BOOL_BUG)
  is_open = logical(o, c_bool)
#else
  is_open = merge (1, 0, o)
#endif

  !  return to the cpp caller
  return

end function pio_cpp_file_is_open

! ---------------------------------------------------------------------
