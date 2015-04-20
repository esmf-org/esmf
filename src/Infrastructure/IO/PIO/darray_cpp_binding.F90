#include "ESMFPIO.h"
#define __PIO_FILE__ "darray_cpp_binding.F90"
! ---------------------------------------------------------------------

!  procedures for a cpp binding to PIO's read/write darray interface functions

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_read_darray_int(void *file, void *varDesc,         &
!                                          void *ioDesc, int *array,          &
!                                          int *shape, int rank,              &
!                                          int *iostat);

subroutine pio_cpp_read_darray_int(file, varDesc, ioDesc, array, shape,       &
                                   rank, iostat) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

  !  import pio types
  use pio_kinds, only: i4
  use pio_types, only: file_desc_t, io_desc_t, var_desc_t

  !  import pio procedure signatures
  use piodarray, only: pio_read_darray

  !  dummy arguments
  type(c_ptr),             value :: file
  type(c_ptr),             value :: varDesc
  type(c_ptr),             value :: ioDesc
  type(c_ptr),             value :: array
  type(c_ptr),             value :: shape
  integer(c_int),          value :: rank
  integer(c_int),    intent(out) :: iostat

  !  local
  type(file_desc_t),     pointer :: file_desc
  type(var_desc_t),      pointer :: var_desc
  type(io_desc_t),       pointer :: iodesc_desc
  integer(i4),           pointer :: as_shape(:)
  integer(i4),           pointer :: as_array1(:)
  integer(i4),           pointer :: as_array2(:,:)
  integer(i4),           pointer :: as_array3(:,:,:)
  integer(i4),           pointer :: as_array4(:,:,:,:)
  integer(i4),           pointer :: as_array5(:,:,:,:,:)
  integer(i4),           pointer :: as_array6(:,:,:,:,:,:)
  integer(i4),           pointer :: as_array7(:,:,:,:,:,:,:)

  integer(i4)                    :: iostatus

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(varDesc, var_desc)
  call c_f_pointer(ioDesc, iodesc_desc)
  call c_f_pointer(shape, as_shape, shape= [ rank ])

  select case (rank)
     case (1)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array1, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_read_darray(file_desc, var_desc, iodesc_desc,                &
                             as_array1, iostatus)
     case(2)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array2, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_read_darray(file_desc, var_desc, iodesc_desc,                &
                             as_array2, iostatus)
     case(3)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array3, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_read_darray(file_desc, var_desc, iodesc_desc,                &
                             as_array3, iostatus)
     case(4)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array4, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_read_darray(file_desc, var_desc, iodesc_desc,                &
                             as_array4, iostatus)
     case(5)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array5, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_read_darray(file_desc, var_desc, iodesc_desc,                &
                             as_array5, iostatus)
     case(6)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array6, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_read_darray(file_desc, var_desc, iodesc_desc,                &
                             as_array6, iostatus)
     case(7)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array7, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_read_darray(file_desc, var_desc, iodesc_desc,                &
                             as_array7, iostatus)
  end select

  !  convert the arguments back to C
  iostat = int(iostatus, c_int)

  !  return to the cpp caller
  return

end  subroutine pio_cpp_read_darray_int

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_read_darray_real(void *file, void *varDesc,        &
!                                           void *ioDesc, int *array,         &
!                                           int *shape, int rank, int *iostat);

subroutine pio_cpp_read_darray_real(file, varDesc, ioDesc, array, shape,      &
                                    rank, iostat) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

  !  import pio types
  use esmfpio_types, only: file_desc_t, io_desc_t, var_desc_t
  use esmfpio_kinds, only: i4, r4

  !  import pio procedure signatures
  use esmfpiodarray, only: pio_read_darray

  !  dummy arguments
  type(c_ptr),    value       :: file
  type(c_ptr),    value       :: varDesc
  type(c_ptr),    value       :: ioDesc
  type(c_ptr),    value       :: array
  type(c_ptr),    value       :: shape
  integer(c_int), value       :: rank
  integer(c_int), intent(out) :: iostat

  !  local
  type(file_desc_t),  pointer :: file_desc
  type(var_desc_t),   pointer :: var_desc
  type(io_desc_t),    pointer :: iodesc_desc
  integer(i4),        pointer :: as_shape(:)
  real(r4),           pointer :: as_array1(:)
  real(r4),           pointer :: as_array2(:,:)
  real(r4),           pointer :: as_array3(:,:,:)
  real(r4),           pointer :: as_array4(:,:,:,:)
  real(r4),           pointer :: as_array5(:,:,:,:,:)
  real(r4),           pointer :: as_array6(:,:,:,:,:,:)
  real(r4),           pointer :: as_array7(:,:,:,:,:,:,:)

  integer(i4)                 :: iostatus

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(varDesc, var_desc)
  call c_f_pointer(ioDesc, iodesc_desc)
  call c_f_pointer(shape, as_shape, shape= [ rank ])

  select case (rank)
     case (1)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array1, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_read_darray(file_desc, var_desc, iodesc_desc,                &
                             as_array1, iostatus)
     case(2)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array2, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_read_darray(file_desc, var_desc, iodesc_desc,                &
                             as_array2, iostatus)
     case(3)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array3, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_read_darray(file_desc, var_desc, iodesc_desc,                &
                             as_array3, iostatus)
     case(4)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array4, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_read_darray(file_desc, var_desc, iodesc_desc,                &
                             as_array4, iostatus)
     case(5)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array5, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_read_darray(file_desc, var_desc, iodesc_desc,                &
                             as_array5, iostatus)
     case(6)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array6, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_read_darray(file_desc, var_desc, iodesc_desc,                &
                             as_array6, iostatus)
     case(7)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array7, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_read_darray(file_desc, var_desc, iodesc_desc,                &
                             as_array7, iostatus)
  end select

  !  convert the arguments back to C
  iostat = int(iostatus, c_int)

  !  return to the cpp caller
  return

end subroutine pio_cpp_read_darray_real

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_read_darray_double(void *file, void *varDesc,      &
!                                              void *ioDesc, int *array,      &
!                                              int *shape, int rank,          &
!                                              int *iostat);

subroutine pio_cpp_read_darray_double(file, varDesc, ioDesc, array,           &
                                      shape, rank, iostat) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

  !  import pio types
  use esmfpio_types, only: file_desc_t, io_desc_t, var_desc_t
  use esmfpio_kinds, only: i4, r8

  !  import pio procedure signatures
  use esmfpiodarray, only: pio_read_darray

  !  dummy arguments
  type(c_ptr),    value       :: file
  type(c_ptr),    value       :: varDesc
  type(c_ptr),    value       :: ioDesc
  type(c_ptr),    value       :: array
  type(c_ptr),    value       :: shape
  integer(c_int), value       :: rank
  integer(c_int), intent(out) :: iostat

  !  local
  type(file_desc_t),  pointer :: file_desc
  type(var_desc_t),   pointer :: var_desc
  type(io_desc_t),    pointer :: iodesc_desc
  integer(i4),        pointer :: as_shape(:)
  real(r8),           pointer :: as_array1(:)
  real(r8),           pointer :: as_array2(:,:)
  real(r8),           pointer :: as_array3(:,:,:)
  real(r8),           pointer :: as_array4(:,:,:,:)
  real(r8),           pointer :: as_array5(:,:,:,:,:)
  real(r8),           pointer :: as_array6(:,:,:,:,:,:)
  real(r8),           pointer :: as_array7(:,:,:,:,:,:,:)

  integer(i4)                 :: iostatus

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(varDesc, var_desc)
  call c_f_pointer(ioDesc, iodesc_desc)
  call c_f_pointer(shape, as_shape, shape= [ rank ])

  select case (rank)
     case (1)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array1, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_read_darray(file_desc, var_desc, iodesc_desc,                &
                             as_array1, iostatus)
     case(2)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array2, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_read_darray(file_desc, var_desc, iodesc_desc,                &
                             as_array2, iostatus)
     case(3)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array3, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_read_darray(file_desc, var_desc, iodesc_desc,                &
                             as_array3, iostatus)
     case(4)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array4, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_read_darray(file_desc, var_desc, iodesc_desc,                &
                             as_array4, iostatus)
     case(5)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array5, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_read_darray(file_desc, var_desc, iodesc_desc,                &
                             as_array5, iostatus)
     case(6)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array6, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_read_darray(file_desc, var_desc, iodesc_desc,                &
                             as_array6, iostatus)
     case(7)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array7, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_read_darray(file_desc, var_desc, iodesc_desc,                &
                             as_array7, iostatus)
  end select

  !  convert the arguments back to C
  iostat = int(iostatus, c_int)

  !  return to the cpp caller
  return

end subroutine pio_cpp_read_darray_double

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_write_darray_int(void *file,                       &
!                                              void *varDesc,                 &
!                                              void *ioDesc, int *array,      &
!                                              int *shape, int rank,          &
!                                              int *iostat);

subroutine pio_cpp_write_darray_int(file, varDesc, ioDesc, array,             &
                                    shape, rank, iostat) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

  !  import pio types
  use esmfpio_types, only: file_desc_t, io_desc_t, var_desc_t
  use esmfpio_kinds, only: i4

  !  import pio procedure signatures
  use esmfpiodarray, only: pio_write_darray

  !  dummy arguments
  type(c_ptr),          value :: file
  type(c_ptr),          value :: varDesc
  type(c_ptr),          value :: ioDesc
  type(c_ptr),          value :: array
  type(c_ptr),          value :: shape
  integer(c_int),       value :: rank
  integer(c_int), intent(out) :: iostat

  !  local
  type(file_desc_t),  pointer :: file_desc
  type(var_desc_t),   pointer :: var_desc
  type(io_desc_t),    pointer :: iodesc_desc
  integer(i4),        pointer :: as_shape(:)
  integer(i4),        pointer :: as_array1(:)
  integer(i4),        pointer :: as_array2(:,:)
  integer(i4),        pointer :: as_array3(:,:,:)
  integer(i4),        pointer :: as_array4(:,:,:,:)
  integer(i4),        pointer :: as_array5(:,:,:,:,:)
  integer(i4),        pointer :: as_array6(:,:,:,:,:,:)
  integer(i4),        pointer :: as_array7(:,:,:,:,:,:,:)

  integer(i4)                 :: iostatus

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(varDesc, var_desc)
  call c_f_pointer(ioDesc, iodesc_desc)
  call c_f_pointer(shape, as_shape, shape= [ rank ])

  select case (rank)
     case (1)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array1, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array1, iostatus)
     case(2)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array2, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array2, iostatus)
     case(3)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array3, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array3, iostatus)
     case(4)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array4, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array4, iostatus)
     case(5)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array5, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array5, iostatus)
     case(6)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array6, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array6, iostatus)
     case(7)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array7, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array7, iostatus)
  end select

  !  convert the arguments back to C
  iostat = int(iostatus, c_int)

  !  return to the cpp caller
  return

end  subroutine pio_cpp_write_darray_int

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_write_darray_int_fill(void *file,                  &
!                                                void *varDesc,               &
!                                                void *ioDesc, int *array,    &
!                                                int *shape, int rank,        &
!                                                int *iostat, int fillval);

subroutine pio_cpp_write_darray_int_fill(file, varDesc, ioDesc, array,        &
                                         shape, rank, iostat, fillval) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

  !  import pio types
  use esmfpio_types, only: file_desc_t, io_desc_t, var_desc_t
  use esmfpio_kinds, only: i4

  !  import pio procedure signatures
  use esmfpiodarray, only: pio_write_darray

  !  dummy arguments
  type(c_ptr),    value       :: file
  type(c_ptr),    value       :: varDesc
  type(c_ptr),    value       :: ioDesc
  type(c_ptr),    value       :: array
  type(c_ptr),    value       :: shape
  integer(c_int), value       :: rank
  integer(c_int), intent(out) :: iostat
  integer(c_int), value       :: fillval

  !  local
  type(file_desc_t), pointer  :: file_desc
  type(var_desc_t),  pointer  :: var_desc
  type(io_desc_t),   pointer  :: iodesc_desc
  integer(i4),       pointer  :: as_shape(:)
  integer(i4),       pointer  :: as_array1(:)
  integer(i4),       pointer  :: as_array2(:,:)
  integer(i4),       pointer  :: as_array3(:,:,:)
  integer(i4),       pointer  :: as_array4(:,:,:,:)
  integer(i4),       pointer  :: as_array5(:,:,:,:,:)
  integer(i4),       pointer  :: as_array6(:,:,:,:,:,:)
  integer(i4),       pointer  :: as_array7(:,:,:,:,:,:,:)

  integer(i4)                 :: iostatus

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(varDesc, var_desc)
  call c_f_pointer(ioDesc, iodesc_desc)
  call c_f_pointer(shape, as_shape, shape= [ rank ])

  select case (rank)
     case (1)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array1, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array1, iostatus, fillval)
     case(2)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array2, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array2, iostatus, fillval)
     case(3)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array3, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array3, iostatus, fillval)
     case(4)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array4, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array4, iostatus, fillval)
     case(5)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array5, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array5, iostatus, fillval)
     case(6)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array6, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array6, iostatus, fillval)
     case(7)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array7, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array7, iostatus, fillval)
  end select

  !  convert the arguments back to C
  iostat = int(iostatus, c_int)

  !  return to the cpp caller
  return

end  subroutine pio_cpp_write_darray_int_fill

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_write_darray_float(void *file,                     &
!                                                void *varDesc,               &
!                                                void *ioDesc, float *array,  &
!                                                int *shape, int rank,        
!                                                int *iostat);

subroutine pio_cpp_write_darray_real(file, varDesc, ioDesc, array,         &
                                     shape, rank, iostat) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

  !  import pio types
  use esmfpio_types, only: file_desc_t, io_desc_t, var_desc_t
  use esmfpio_kinds, only: i4, r4

  !  import pio procedure signatures
  use esmfpiodarray, only: pio_write_darray

  !  dummy arguments
  type(c_ptr),    value       :: file
  type(c_ptr),    value       :: varDesc
  type(c_ptr),    value       :: ioDesc
  type(c_ptr),    value       :: array
  type(c_ptr),    value       :: shape
  integer(c_int), value       :: rank
  integer(c_int), intent(out) :: iostat

  !  local
  type(file_desc_t), pointer  :: file_desc
  type(var_desc_t),  pointer  :: var_desc
  type(io_desc_t),   pointer  :: iodesc_desc
  integer(i4),       pointer  :: as_shape(:)
  real(r4),          pointer  :: as_array1(:)
  real(r4),          pointer  :: as_array2(:,:)
  real(r4),          pointer  :: as_array3(:,:,:)
  real(r4),          pointer  :: as_array4(:,:,:,:)
  real(r4),          pointer  :: as_array5(:,:,:,:,:)
  real(r4),          pointer  :: as_array6(:,:,:,:,:,:)
  real(r4),          pointer  :: as_array7(:,:,:,:,:,:,:)

  integer(i4) :: iostatus

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(varDesc, var_desc)
  call c_f_pointer(ioDesc, iodesc_desc)
  call c_f_pointer(shape, as_shape, shape= [ rank ])

  select case (rank)
     case (1)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array1, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array1, iostatus)
     case(2)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array2, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array2, iostatus)
     case(3)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array3, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array3, iostatus)
     case(4)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array4, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array4, iostatus)
     case(5)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array5, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array5, iostatus)
     case(6)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array6, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array6, iostatus)
     case(7)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array7, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array7, iostatus)
  end select

  !  convert the arguments back to C
  iostat = int(iostatus, c_int)

  !  return to the cpp caller
  return

end  subroutine pio_cpp_write_darray_real

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_write_darray_real_fill(void *file,                 &
!                                                    void *varDesc,           &
!                                                    void *ioDesc,            &
!                                                    float *array,            &
!                                                    int *shape, int rank,    &
!                                                    int *iostat,             &
!                                                    float fillval);

subroutine pio_cpp_write_darray_real_fill(file, varDesc, ioDesc, array,       &
                                          shape, rank, iostat, fillval) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_float, c_ptr, c_f_pointer

  !  import pio types
  use esmfpio_types, only: file_desc_t, io_desc_t, var_desc_t
  use esmfpio_kinds, only: i4, r4

  !  import pio procedure signatures
  use esmfpiodarray, only: pio_write_darray

  !  dummy arguments
  type(c_ptr),    value       :: file
  type(c_ptr),    value       :: varDesc
  type(c_ptr),    value       :: ioDesc
  type(c_ptr),    value       :: array
  type(c_ptr),    value       :: shape
  integer(c_int), value       :: rank
  integer(c_int), intent(out) :: iostat
  real(c_float),  value       :: fillval

  !  local
  type(file_desc_t), pointer  :: file_desc
  type(var_desc_t),  pointer  :: var_desc
  type(io_desc_t),   pointer  :: iodesc_desc
  integer(i4),       pointer  :: as_shape(:)
  real(r4),          pointer  :: as_array1(:)
  real(r4),          pointer  :: as_array2(:,:)
  real(r4),          pointer  :: as_array3(:,:,:)
  real(r4),          pointer  :: as_array4(:,:,:,:)
  real(r4),          pointer  :: as_array5(:,:,:,:,:)
  real(r4),          pointer  :: as_array6(:,:,:,:,:,:)
  real(r4),          pointer  :: as_array7(:,:,:,:,:,:,:)

  integer(i4)                 :: iostatus

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(varDesc, var_desc)
  call c_f_pointer(ioDesc, iodesc_desc)
  call c_f_pointer(shape, as_shape, shape= [ rank ])

  select case (rank)
     case (1)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array1, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array1, iostatus, fillval)
     case(2)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array2, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array2, iostatus, fillval)
     case(3)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array3, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array3, iostatus, fillval)
     case(4)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array4, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array4, iostatus, fillval)
     case(5)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array5, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array5, iostatus, fillval)
     case(6)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array6, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array6, iostatus, fillval)
     case(7)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array7, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array7, iostatus, fillval)
  end select

  !  convert the arguments back to C
  iostat = int(iostatus, c_int)

  !  return to the cpp caller
  return

end  subroutine pio_cpp_write_darray_real_fill

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_write_darray_double(void *file,                    &
!                                                 void *varDesc,              &
!                                                 void *ioDesc,               &
!                                                 double *array,              &
!                                                 int *shape, int rank,       &
!                                                 int *iostat);

subroutine pio_cpp_write_darray_double(file, varDesc, ioDesc, array,          &
                                       shape, rank, iostat) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

  !  import pio types
  use esmfpio_types, only: file_desc_t, io_desc_t, var_desc_t
  use esmfpio_kinds, only: i4, r8

  !  import pio procedure signatures
  use esmfpiodarray, only: pio_write_darray

  !  dummy arguments
  type(c_ptr),    value       :: file
  type(c_ptr),    value       :: varDesc
  type(c_ptr),    value       :: ioDesc
  type(c_ptr),    value       :: array
  type(c_ptr),    value       :: shape
  integer(c_int), value       :: rank
  integer(c_int), intent(out) :: iostat

  !  local
  type(file_desc_t), pointer  :: file_desc
  type(var_desc_t),  pointer  :: var_desc
  type(io_desc_t),   pointer  :: iodesc_desc
  integer(i4),       pointer  :: as_shape(:)
  real(r8),          pointer  :: as_array1(:)
  real(r8),          pointer  :: as_array2(:,:)
  real(r8),          pointer  :: as_array3(:,:,:)
  real(r8),          pointer  :: as_array4(:,:,:,:)
  real(r8),          pointer  :: as_array5(:,:,:,:,:)
  real(r8),          pointer  :: as_array6(:,:,:,:,:,:)
  real(r8),          pointer  :: as_array7(:,:,:,:,:,:,:)

  integer(i4)                 :: iostatus

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(varDesc, var_desc)
  call c_f_pointer(ioDesc, iodesc_desc)
  call c_f_pointer(shape, as_shape, shape= [ rank ])

  select case (rank)
     case (1)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array1, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array1, iostatus)
     case(2)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array2, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array2, iostatus)
     case(3)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array3, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array3, iostatus)
     case(4)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array4, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array4, iostatus)
     case(5)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array5, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array5, iostatus)
     case(6)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array6, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array6, iostatus)
     case(7)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array7, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array7, iostatus)
  end select

  !  convert the arguments back to C
  iostat = int(iostatus, c_int)

  !  return to the cpp caller
  return

end  subroutine pio_cpp_write_darray_double

! ---------------------------------------------------------------------
!  extern "C" void pio_cpp_write_darray_double_fill(void *file,               &
!                                                      void *varDesc,         &
!                                                      void *ioDesc,          &
!                                                      double *array,         &
!                                                      int *shape, int rank,  &
!                                                      int *iostat,           &
!                                                      double fillval);

subroutine pio_cpp_write_darray_double_fill(file, varDesc, ioDesc, array,     &
                                            shape, rank, iostat, fillval)     &
                                            bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_double, c_ptr, c_f_pointer

  !  import pio types
  use esmfpio_types, only: file_desc_t, io_desc_t, var_desc_t
  use esmfpio_kinds, only: i4, r8

  !  import pio procedure signatures
  use esmfpiodarray, only: pio_write_darray

  !  dummy arguments
  type(c_ptr),    value       :: file
  type(c_ptr),    value       :: varDesc
  type(c_ptr),    value       :: ioDesc
  type(c_ptr),    value       :: array
  type(c_ptr),    value       :: shape
  integer(c_int), value       :: rank
  integer(c_int), intent(out) :: iostat
  real(c_double), value       :: fillval

  !  local
  type(file_desc_t), pointer  :: file_desc
  type(var_desc_t) , pointer  :: var_desc
  type(io_desc_t)  , pointer  :: iodesc_desc
  integer(i4),       pointer  :: as_shape(:)
  real(r8),          pointer  :: as_array1(:)
  real(r8),          pointer  :: as_array2(:,:)
  real(r8),          pointer  :: as_array3(:,:,:)
  real(r8),          pointer  :: as_array4(:,:,:,:)
  real(r8),          pointer  :: as_array5(:,:,:,:,:)
  real(r8),          pointer  :: as_array6(:,:,:,:,:,:)
  real(r8),          pointer  :: as_array7(:,:,:,:,:,:,:)

  integer(i4)                 :: iostatus

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(varDesc, var_desc)
  call c_f_pointer(ioDesc, iodesc_desc)
  call c_f_pointer(shape, as_shape, shape= [ rank ])

  select case (rank)
     case (1)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array1, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array1, iostatus, fillval)
     case(2)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array2, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array2, iostatus, fillval)
     case(3)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array3, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array3, iostatus, fillval)
     case(4)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array4, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array4, iostatus, fillval)
     case(5)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array5, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array5, iostatus, fillval)
     case(6)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array6, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array6, iostatus, fillval)
     case(7)
        ! convert the input array to be of the correct shape
        call c_f_pointer(array, as_array7, shape= [ as_shape ])
        !  call the Fortran procedure
        call pio_write_darray(file_desc, var_desc, iodesc_desc,               &
                              as_array7, iostatus, fillval)
  end select

  !  convert the arguments back to C
  iostat = int(iostatus, c_int)

  !  return to the cpp caller
  return

end  subroutine pio_cpp_write_darray_double_fill

! ---------------------------------------------------------------------
