#define __PIO_FILE__ "pio_cpp_binding.F90"
! ---------------------------------------------------------------------

!  procedures for a cpp binding to pio

module pio_cpp_binding

   use pio_types, only: iosystem_desc_t

#ifndef _MPISERIAL
   use mpi, only: MPI_COMM_NULL   ! _EXTERNAL
#endif

   implicit none

#ifdef _MPISERIAL
   include 'mpif.h'      ! _EXTERNAL
#endif

   !  explicit export

   private

   ! public interface

   public :: pio_cpp_init_intracom_int
   public :: pio_cpp_init_intercom_int
   public :: pio_cpp_finalize
   public :: pio_cpp_initdecomp_dof_io
   public :: pio_cpp_initdecomp_dof
#if 0
   public :: pio_cpp_initdecomp_dof_dof
#endif
   public :: pio_cpp_openfile
   public :: pio_cpp_syncfile
   public :: pio_cpp_createfile
   public :: pio_cpp_closefile
   public :: pio_cpp_setiotype
   public :: pio_cpp_numtoread
   public :: pio_cpp_numtowrite
   public :: pio_cpp_setframe
   public :: pio_cpp_advanceframe
   public :: pio_cpp_setdebuglevel
   public :: pio_cpp_seterrorhandlingf
   public :: pio_cpp_seterrorhandlingi
   public :: pio_cpp_get_local_array_size
   public :: pio_cpp_freedecomp_ios
   public :: pio_cpp_freedecomp_file
   public :: pio_cpp_dupiodesc
   public :: pio_cpp_getnumiotasks
   public :: pio_cpp_set_hint
   public :: pio_cpp_getnum_ost
   public :: pio_cpp_setnum_ost
   public :: pio_cpp_file_is_open

   ! Utility functions for managing C handles for iosystem_desc_t instances

   type, private :: PIO_C_HANDLE_NODE
      integer :: c_handle_start
      type(iosystem_desc_t), pointer :: PIO_descriptors(:)
      type(PIO_C_HANDLE_NODE), pointer :: next
   end type PIO_C_HANDLE_NODE

   type(PIO_C_HANDLE_NODE), private, save, pointer :: PIO_Intracom_handles
   integer, private :: PIO_c_handle_num = 0

   private :: new_pio_iosys_handles
   private :: get_pio_iosys_handle
   private :: delete_pio_iosys_handle
   !  private :: delete_all_pio_iosys_handles

   !  constants

   ! ---------------------------------------------------------------------

   !  library

 contains

   ! ---------------------------------------------------------------------

   !  Obtain a PIO iosystem_desc_t object given its integer handle

subroutine new_pio_iosys_handles(iosystem_handles, iosystem)

   use pio_support, only : piodie, debug

   !  dummy arguments
   integer, intent(inout) :: iosystem_handles(:)
   type(iosystem_desc_t), pointer :: iosystem(:)

   ! local
   type(PIO_C_HANDLE_NODE), pointer :: new_pio_c_handle_node(:)
   type(PIO_C_HANDLE_NODE), pointer :: pio_handle_node
   integer :: stat
   integer :: num_handles
   integer :: i

   num_handles = size(iosystem_handles, 1)

   ! First, create a new iosystem handle node
   allocate(new_pio_c_handle_node(1), stat=stat)
   if (stat .ne. 0) then
      call piodie(__PIO_FILE__,__LINE__,       &
                 'unable to allocate PIO_C_HANDLE_NODE')
   endif
   nullify(new_pio_c_handle_node(1)%next)
   ! Now, create the new iosystem_desc_t array
   allocate(new_pio_c_handle_node(1)%PIO_descriptors(num_handles), stat=stat)
   if (stat .ne. 0) then
      deallocate(new_pio_c_handle_node)
      call piodie(__PIO_FILE__,__LINE__,'unable to allocate iosystem_desc_t')
   endif
   ! Fill in C starting handle number and increment
   new_pio_c_handle_node(1)%c_handle_start = PIO_c_handle_num
   PIO_c_handle_num = PIO_c_handle_num + num_handles
   ! Find the end of the chain and insert new node
   if (.not. associated(PIO_Intracom_handles)) then
      PIO_Intracom_handles => new_pio_c_handle_node(1)
   else
      pio_handle_node => PIO_Intracom_handles
      do while (associated(pio_handle_node%next))
         pio_handle_node => pio_handle_node%next
      end do
      pio_handle_node%next => new_pio_c_handle_node(1)
   end if
   ! Set the PIO iosystem_desc_t output
  iosystem => new_pio_c_handle_node(1)%PIO_descriptors
  ! Fill in the c handle numbers
  do i = 1, num_handles
     iosystem_handles(i) = new_pio_c_handle_node(1)%c_handle_start + i
  end do

end subroutine new_pio_iosys_handles

  !  Obtain a PIO iosystem_desc_t object given its integer handle

subroutine get_pio_iosys_handle(iosystem_handle, iosystem)

  use pio_support, only : piodie, debug

  !  dummy arguments
  integer, intent(in) :: iosystem_handle
  type(iosystem_desc_t), pointer, intent(out) :: iosystem

  ! local
  logical :: found_handle = .false.
  type(PIO_C_HANDLE_NODE), pointer :: pio_handle_node
  integer :: num_handles
  integer :: handle0

  continue
  ! Search for a structure with the correct handle number
  pio_handle_node => PIO_Intracom_handles
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

  use pio_support, only : piodie, debug

  !  dummy arguments
  integer, intent(in) :: iosystem_handle
  logical :: found_handle = .false.

  ! local
  type(PIO_C_HANDLE_NODE), pointer :: pio_handle_node
  type(PIO_C_HANDLE_NODE), pointer :: prev_handle_node
  integer :: stat
  integer :: num_handles
  integer :: handle0

  nullify(prev_handle_node)
  ! Search for a structure with the correct handle number
  pio_handle_node => PIO_Intracom_handles
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

  !  import pio kinds
  use pio_kinds, only: i4

  !  import pio types
  use pio_types, only: iosystem_desc_t

  !  import pio procedure signatures
  use piolib_mod, only: pio_init

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

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr

  !  import pio types
  use pio_types, only: iosystem_desc_t

  !  import pio procedure signatures
  use piolib_mod, only: pio_init

  !  dummy arguments
  integer(c_int), value :: component_count
  integer(c_int), value :: peer_comm
  integer(c_int), dimension(component_count), intent(in) :: comp_comms
  integer(c_int), value :: io_comm
  integer(c_int), intent(inout) :: iosystem_handles(component_count)

  !  local

  integer, target :: iosystem_handle_array(component_count)
  type(iosystem_desc_t), pointer :: iosystem_desc_p(:)
  integer :: i

  !  text
  continue

  !  get a new iosystem_desc_t for this connection
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

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr

  !  import pio kinds
  use pio_kinds, only: i4

  !  import pio types
  use pio_types, only: iosystem_desc_t

  !  import pio procedure signatures
  use piolib_mod, only: pio_finalize

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

  call MPI_Barrier(iosystem_desc_p%comp_comm, ierror);
  !  call the Fortran procedure
  call pio_finalize(iosystem_desc_p, ierror)

  ! Delete the iosystem descriptor
  call MPI_Barrier(iosystem_desc_p%comp_comm, ierror);
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

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_int64_t, c_ptr, c_f_pointer

  !  import pio kinds
  use pio_kinds, only: i4, pio_offset

  !  import pio types
  use pio_types, only: iosystem_desc_t, io_desc_t

  !  import pio procedure signatures
  use piolib_mod, only: pio_initdecomp

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

  !  import pio kinds
  use pio_kinds, only: i4, pio_offset

  !  import pio types
  use pio_types, only: iosystem_desc_t, io_desc_t

  !  import pio procedure signatures
  use piolib_mod, only: pio_initdecomp

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

  !  import pio kinds
  use pio_kinds, only: i4, pio_offset

  !  import pio types
  use pio_types, only: iosystem_desc_t, io_desc_t

  !  import pio procedure signatures
  use piolib_mod, only: pio_initdecomp

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
  use pio_types, only: iosystem_desc_t, file_desc_t

  !  import pio procedure signatures
  use piolib_mod, only: pio_openfile
  use pio_cpp_utils, only: f_chars, c_len, max_path_len

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
  use pio_types, only: file_desc_t

  !  import pio procedure signatures
  use piolib_mod, only: pio_syncfile

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
  use pio_types, only: iosystem_desc_t, file_desc_t

  !  import pio procedure signatures
  use piolib_mod, only: pio_createfile
  use pio_cpp_utils, only: f_chars, c_len, max_path_len

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
 use pio_types, only: file_desc_t

 !  import pio procedure signatures
 use piolib_mod, only: pio_closefile

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
  use pio_kinds, only: i4

  !  import pio types
  use pio_types, only: file_desc_t

  !  import pio procedure signatures
  use piolib_mod, only: pio_setiotype

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
  use pio_types, only: io_desc_t

  !  import pio procedure signatures
  use piolib_mod, only: pio_numtoread

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
  use pio_types, only: io_desc_t

  !  import pio procedure signatures
  use piolib_mod, only: pio_numtowrite

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
  use pio_kinds, only: pio_offset

  !  import pio types
  use pio_types, only: var_desc_t

  !  import pio procedure signatures
  use piolib_mod, only: pio_setframe

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
  use pio_types, only: var_desc_t

  !  import pio procedure signatures
  use piolib_mod, only: pio_advanceframe

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
  use pio_kinds, only: i4

  !  import pio procedure signatures
  use piolib_mod, only: pio_setdebuglevel

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
  use pio_types, only: file_desc_t

  !  import pio procedure signatures
  use piolib_mod, only: pio_seterrorhandling

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

  !  import pio types
  use pio_types, only: iosystem_desc_t

  !  import pio procedure signatures
  use piolib_mod, only: pio_seterrorhandling

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
  use pio_types, only: io_desc_t

  !  import pio procedure signatures
  use piolib_mod, only: pio_get_local_array_size

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

  !  import pio types
  use pio_types, only: iosystem_desc_t, io_desc_t

  !  import pio procedure signatures
  use piolib_mod, only: pio_freedecomp

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

  !  call the Fortran procedure
  call pio_freedecomp(iosystem_desc_p, io_desc)

  !  return to the cpp caller
  return

end subroutine pio_cpp_freedecomp_ios

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_freedecomp_file(void* file, void* iodesc);

subroutine pio_cpp_freedecomp_file(file, iodesc) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t, io_desc_t

  !  import pio procedure signatures
  use piolib_mod, only: pio_freedecomp

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
  use pio_types, only: io_desc_t

  !  import pio procedure signatures
  use piolib_mod, only: pio_dupiodesc

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

  !  import pio kinds
  use pio_kinds, only: i4

  !  import pio types
  use pio_types, only: iosystem_desc_t

  !  import pio procedure signatures
  use piolib_mod, only: pio_getnumiotasks

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
  use pio_types, only: iosystem_desc_t

  !  import pio procedure signatures
  use piolib_mod, only: pio_set_hint
  use pio_cpp_utils, only: f_chars, c_len, max_string_len

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

  !  import pio types
  use pio_types, only: iosystem_desc_t

  !  import pio procedure signatures
  use piolib_mod, only: pio_getnum_ost

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

  !  import pio kinds
  use pio_kinds, only: i4

  !  import pio types
  use pio_types, only: iosystem_desc_t

  !  import pio procedure signatures
  use piolib_mod, only: pio_setnum_ost

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
  use, intrinsic :: iso_c_binding, only: c_bool, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t

  !  import pio procedure signatures
  use piolib_mod, only: pio_file_is_open

  !  function result
  logical(c_bool) :: is_open

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
  is_open = logical(o, c_bool)

  !  return to the cpp caller
  return

end function pio_cpp_file_is_open

! ---------------------------------------------------------------------

end module pio_cpp_binding
