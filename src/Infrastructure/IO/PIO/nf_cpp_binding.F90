#include "ESMFPIO.h"
#define __PIO_FILE__ "nf_cpp_binding.F90"
! ---------------------------------------------------------------------

!  procedures for a cpp binding to PIO's NetCDF interface functions

!  the extern "C" functions()

! ---------------------------------------------------------------------

! extern "C" int pio_cpp_inquire(pio_file_desc_t file, int *nDimensions,      &
!                                int *nVariables, int *nAttributes,           &
!                                int *unlimitedDimID);

function pio_cpp_inquire_int(File, nDimensions, nVariables, nAttributes,      &
                             unlimitedDimID) result(ierr) bind(c)
  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t
  !  import nf procedure signatures
  use nf_mod, only: pio_inquire

  !  function result
  integer(c_int)              :: ierr

  !  dummy arguments
  type(c_ptr), value          :: file
  integer(c_int), intent(out) :: nDimensions
  integer(c_int), intent(out) :: nVariables
  integer(c_int), intent(out) :: nAttributes
  integer(c_int), intent(out) :: unlimitedDimID

  !  local
  integer                     :: ierror
  type(file_desc_t), pointer  :: file_desc
  integer                     :: nDimLocal
  integer                     :: nVarLocal
  integer                     :: nAttrLocal
  integer                     :: unlDimIdLocal

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)

  !  call the Fortran procedure
  ierror = pio_inquire(file_desc, nDimLocal, nVarLocal, nAttrLocal,           &
                       unlDimIdLocal)

  !  convert the arguments back to C
  nDimensions = int(nDimLocal, c_int)
  nVariables = int(nVarLocal, c_int)
  nAttributes = int(nAttrLocal, c_int)
  unlimitedDimID = int(unlDimIdLocal, c_int)
  ierr = int(ierror, c_int)

  !  return to the cpp caller
  return
end function pio_cpp_inquire_int

! ---------------------------------------------------------------------

! extern "C" int pio_cpp_inq_att_vid(pio_file_desc_t file, int varid,         &
!                                    const char *name, int *xtype, int *len);

! ---------------------------------------------------------------------

function pio_cpp_inq_att_vid(file, varid, name, xtype, len)                   &
         result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t

  !  import nf procedure signatures
  use nf_mod, only: pio_inq_att
  use pio_cpp_utils, only: f_chars, c_len, max_path_len

  !  function result
  integer(c_int)              :: ierr

  !  dummy arguments
  type(c_ptr),    value       :: file
  integer(c_int), value       :: varid
  type(c_ptr),    value       :: name
  integer(c_int), intent(out) :: xtype
  integer(c_int), intent(out) :: len

  !  local
  integer                     :: ierror
  integer                     :: xtype_local
  integer                     :: len_local

  type(file_desc_t), pointer :: file_desc
  character(kind= c_char, len= max_path_len), pointer :: c_filename
#ifdef ALLOC_CHARLEN_OK
  character(len= :), allocatable :: filename
#else
  character(len= max_path_len)   :: filename
#endif
  integer                        :: clen

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(name, c_filename)

  !  convert the C string to Fortran characters
  clen = c_len(c_filename)

#ifdef ALLOC_CHARLEN_OK
  allocate(filename, mold= filename(1: clen))
  call f_chars(filename, c_filename(1: clen))
#else
  filename = c_filename(1:clen)
#endif

  !  call the Fortran procedure
  ierror = pio_inq_att(file_desc, int(varid), trim(filename),                 &
                       xtype_local, len_local)

  !  convert the arguments back to C
  xtype = int(xtype_local, c_int)
  len = int(len_local, c_int)
  ierr = int(ierror, c_int)

  !  return to the cpp caller
  return
end function pio_cpp_inq_att_vid

! extern "C" int pio_cpp_inq_att_vdesc(pio_file_desc_t file,                  &
!                                      pio_var_desc_t vardesc,                &
!                                      const char *name, int *xtype, int *len);

! ---------------------------------------------------------------------

function pio_cpp_inq_att_vdesc(file, vardesc, name, xtype, len)               &
         result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t, var_desc_t

  !  import nf procedure signatures
  use nf_mod, only: pio_inq_att
  use pio_cpp_utils, only: f_chars, c_len, max_path_len

  !  function result
  integer(c_int)              :: ierr

  !  dummy arguments
  type(c_ptr),    value       :: file
  type(c_ptr),    value       :: vardesc
  type(c_ptr),    value       :: name
  integer(c_int), intent(out) :: xtype
  integer(c_int), intent(out) :: len

  !  local
  integer                     :: ierror
  integer                     :: xtype_local
  integer                     :: len_local

  type(file_desc_t), pointer :: file_desc
  type(var_desc_t), pointer :: var_desc
  character(kind= c_char, len= max_path_len), pointer :: c_filename
#ifdef ALLOC_CHARLEN_OK
  character(len= :), allocatable :: filename
#else
  character(len= max_path_len)   :: filename
#endif
  integer                        :: clen

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(vardesc, var_desc)
  call c_f_pointer(name, c_filename)

  !  convert the C string to Fortran characters
  clen = c_len(c_filename)

#ifdef ALLOC_CHARLEN_OK
  allocate(filename, mold= filename(1: clen))
  call f_chars(filename, c_filename(1: clen))
#else
  filename = c_filename(1:clen)
#endif

  !  call the Fortran procedure
  ierror = pio_inq_att(file_desc, var_desc, trim(filename),                   &
                       xtype_local, len_local)

  !  convert the arguments back to C
  xtype = int(xtype_local, c_int)
  len = int(len_local, c_int)
  ierr = int(ierror, c_int)

  !  return to the cpp caller
  return
end function pio_cpp_inq_att_vdesc

! ---------------------------------------------------------------------

! extern "C" int pio_cpp_inq_attlen_vid(pio_file_desc_t file, int varid,      &
   !                                    const char *name, int *len);

! ---------------------------------------------------------------------

function pio_cpp_inq_attlen_vid(file, varid, name, len) result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t

  !  import nf procedure signatures
  use nf_mod, only: pio_inq_attlen
  use pio_cpp_utils, only: f_chars, c_len, max_path_len

  !  function result
  integer(c_int)              :: ierr

  !  dummy arguments
  type(c_ptr),    value       :: file
  integer(c_int), value       :: varid
  type(c_ptr),    value       :: name
  integer(c_int), intent(out) :: len

  !  local
  integer                     :: ierror
  integer                     :: len_local

  type(file_desc_t), pointer :: file_desc
  character(kind= c_char, len= max_path_len), pointer :: c_filename
#ifdef ALLOC_CHARLEN_OK
  character(len= :), allocatable :: filename
#else
  character(len= max_path_len)   :: filename
#endif
  integer                        :: clen

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(name, c_filename)

  !  convert the C string to Fortran characters
  clen = c_len(c_filename)

#ifdef ALLOC_CHARLEN_OK
  allocate(filename, mold= filename(1: clen))
  call f_chars(filename, c_filename(1: clen))
#else
  filename = c_filename(1:clen)
#endif

  !  call the Fortran procedure
  ierror = pio_inq_attlen(file_desc, int(varid), trim(filename),              &
                          len_local)

  !  convert the arguments back to C
  len = int(len_local, c_int)
  ierr = int(ierror, c_int)

  !  return to the cpp caller
  return
end function pio_cpp_inq_attlen_vid

! extern "C" int pio_cpp_inq_attlen_vdesc(pio_file_desc_t file,               &
!                                         pio_var_desc_t vardesc,             &
!                                         const char *name, int *len);

! ---------------------------------------------------------------------

function pio_cpp_inq_attlen_vdesc(file, vardesc, name, len)                   &
         result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t, var_desc_t

  !  import nf procedure signatures
  use nf_mod, only: pio_inq_attlen
  use pio_cpp_utils, only: f_chars, c_len, max_path_len

  !  function result
  integer(c_int)              :: ierr

  !  dummy arguments
  type(c_ptr),    value       :: file
  type(c_ptr),    value       :: vardesc
  type(c_ptr),    value       :: name
  integer(c_int), intent(out) :: len

  !  local
  integer                     :: ierror
  integer                     :: len_local

  type(file_desc_t), pointer :: file_desc
  type(var_desc_t), pointer :: var_desc
  character(kind= c_char, len= max_path_len), pointer :: c_filename
#ifdef ALLOC_CHARLEN_OK
  character(len= :), allocatable :: filename
#else
  character(len= max_path_len)   :: filename
#endif
  integer                        :: clen

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(vardesc, var_desc)
  call c_f_pointer(name, c_filename)

  !  convert the C string to Fortran characters
  clen = c_len(c_filename)

#ifdef ALLOC_CHARLEN_OK
  allocate(filename, mold= filename(1: clen))
  call f_chars(filename, c_filename(1: clen))
#else
  filename = c_filename(1:clen)
#endif

  !  call the Fortran procedure
  ierror = pio_inq_attlen(file_desc, var_desc, trim(filename),                &
                          len_local)

  !  convert the arguments back to C
  len = int(len_local, c_int)
  ierr = int(ierror, c_int)

  !  return to the cpp caller
  return
end function pio_cpp_inq_attlen_vdesc

! ---------------------------------------------------------------------

! extern "C" int pio_cpp_inq_attname_vid(pio_file_desc_t file, int varid,     &
!                                        int attnum, char *name);

! ---------------------------------------------------------------------

function pio_cpp_inq_attname_vid(file, varid, attnum, name)                   &
         result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_char, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t

  !  import nf procedure signatures
  use nf_mod, only: pio_inq_attname
  use pio_cpp_utils, only: c_chars, c_len, max_path_len

  !  function result
  integer(c_int)              :: ierr

  !  dummy arguments
  type(c_ptr),    value       :: file
  integer(c_int), value       :: varid
  integer(c_int), value       :: attnum
  type(c_ptr),    value       :: name

  !  local
  integer                     :: ierror

  type(file_desc_t), pointer :: file_desc
  character(kind= c_char, len= max_path_len), pointer :: c_filename
  character(len= max_path_len)   :: name_local

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(name, c_filename)

  !  call the Fortran procedure
  ierror = pio_inq_attname(file_desc, int(varid), int(attnum), name_local)

  !  convert the arguments back to C
  call c_chars(c_filename, name_local)
  ierr = int(ierror, c_int)

  !  return to the cpp caller
  return
end function pio_cpp_inq_attname_vid

! ---------------------------------------------------------------------

! extern "C" int pio_cpp_inq_attname_vdesc(pio_file_desc_t file,              &
!                                              pio_var_desc_t vardesc,        &
!                                              int attnum, char *name);

! ---------------------------------------------------------------------

function pio_cpp_inq_attname_vdesc(file, vardesc, attnum, name)               &
         result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_char, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t, var_desc_t

  !  import nf procedure signatures
  use nf_mod, only: pio_inq_attname
  use pio_cpp_utils, only: c_chars, c_len, max_path_len

  !  function result
  integer(c_int)              :: ierr

  !  dummy arguments
  type(c_ptr),    value       :: file
  type(c_ptr), value       :: vardesc
  integer(c_int), value       :: attnum
  type(c_ptr),    value       :: name

  !  local
  integer                     :: ierror

  type(file_desc_t), pointer  :: file_desc
  type(var_desc_t), pointer   :: var_desc
  character(len= max_path_len)   :: name_local
  character(kind= c_char, len= max_path_len), pointer :: c_filename

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(vardesc, var_desc)
  call c_f_pointer(name, c_filename)

  !  call the Fortran procedure
  ierror = pio_inq_attname(file_desc, var_desc, int(attnum), name_local)

  !  convert the arguments back to C
  call c_chars(c_filename, name_local)
  ierr = int(ierror, c_int)

  !  return to the cpp caller
  return
end function pio_cpp_inq_attname_vdesc

! ---------------------------------------------------------------------

! extern "C" int pio_cpp_inq_varid_vid(pio_file_desc_t file,                  &
!                                      const char *name, int *varid);

! ---------------------------------------------------------------------

function pio_cpp_inq_varid_vid(file, name, varid) result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_char, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t

  !  import nf procedure signatures
  use nf_mod, only: pio_inq_varid
  use pio_cpp_utils, only: f_chars, c_len, max_path_len

  !  function result
  integer(c_int)              :: ierr

  !  dummy arguments
  type(c_ptr),    value       :: file
  type(c_ptr),    value       :: name
  integer(c_int), intent(out) :: varid

  !  local
  integer                     :: ierror
  integer                     :: varid_local

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
  call c_f_pointer(name, c_filename)

  clen = c_len(c_filename)

#ifdef ALLOC_CHARLEN_OK
  allocate(filename, mold= filename(1: clen))
  call f_chars(filename, c_filename(1: clen))
#else
  filename = c_filename(1:clen)
#endif

  !  call the Fortran procedure
  ierror = pio_inq_varid(file_desc, trim(filename), varid_local)

  !  convert the arguments back to C
  ierr = int(ierror, c_int)
  varid = int(varid_local, c_int)

  !  return to the cpp caller
  return
end function pio_cpp_inq_varid_vid

! ---------------------------------------------------------------------

! extern "C" int pio_cpp_inq_varid_vdesc(pio_file_desc_t file,                &
!                                        const char *name,                    &
!                                        pio_vdesc_t vardesc);

! ---------------------------------------------------------------------

function pio_cpp_inq_varid_vdesc(file, name, vardesc) result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_char, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t, var_desc_t

  !  import nf procedure signatures
  use nf_mod, only: pio_inq_varid
  use pio_cpp_utils, only: f_chars, c_len, max_path_len

  !  function result
  integer(c_int)              :: ierr

  !  dummy arguments
  type(c_ptr),    value       :: file
  type(c_ptr),    value       :: name
  type(c_ptr),    value       :: vardesc

  !  local
  integer                     :: ierror

  type(file_desc_t), pointer  :: file_desc
  type(var_desc_t), pointer   :: var_desc
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
  call c_f_pointer(name, c_filename)
  call c_f_pointer(vardesc, var_desc)

  clen = c_len(c_filename)

#ifdef ALLOC_CHARLEN_OK
  allocate(filename, mold= filename(1: clen))
  call f_chars(filename, c_filename(1: clen))
#else
  filename = c_filename(1:clen)
#endif

  !  call the Fortran procedure
  ierror = pio_inq_varid(file_desc, filename, var_desc)

  !  convert the arguments back to C
  ierr = int(ierror, c_int)

  !  return to the cpp caller
  return
end function pio_cpp_inq_varid_vdesc

! ---------------------------------------------------------------------

! extern "C" int pio_cpp_inq_varname_vdesc(pio_file_desc_t file,              &
!                                          pio_var_desc_t vardesc,            &
!                                          char *name);

! ---------------------------------------------------------------------

function pio_cpp_inq_varname_vdesc(file, vardesc, name) result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_char, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t, var_desc_t

  !  import nf procedure signatures
  use nf_mod, only: pio_inq_varname
  use pio_cpp_utils, only: c_chars, c_len, max_path_len

  !  function result
  integer(c_int)              :: ierr

  !  dummy arguments
  type(c_ptr),    value       :: file
  type(c_ptr),    value       :: vardesc
  type(c_ptr),    value       :: name

  !  local
  integer                     :: ierror

  type(file_desc_t), pointer  :: file_desc
  type(var_desc_t), pointer   :: var_desc
  character(len= max_path_len)   :: name_local
  character(kind= c_char, len= max_path_len), pointer :: c_filename

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(vardesc, var_desc)
  call c_f_pointer(name, c_filename)

  !  call the Fortran procedure
  ierror = pio_inq_varname(file_desc, var_desc, name_local)

  !  convert the arguments back to C
  call c_chars(c_filename, name_local)
  ierr = int(ierror, c_int)

  !  return to the cpp caller
  return
end function pio_cpp_inq_varname_vdesc

! ---------------------------------------------------------------------

! extern "C" int pio_cpp_inq_varname_vid(pio_file_desc_t file, int varid,     &
!                                        char *name);

! ---------------------------------------------------------------------

function pio_cpp_inq_varname_vid(file, varid, name) result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_char, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t

  !  import nf procedure signatures
  use nf_mod, only: pio_inq_varname
  use pio_cpp_utils, only: c_chars, c_len, max_path_len

  !  function result
  integer(c_int)              :: ierr

  !  dummy arguments
  type(c_ptr),    value       :: file
  integer(c_int), value       :: varid
  type(c_ptr),    value       :: name

  !  local
  integer                     :: ierror

  type(file_desc_t), pointer :: file_desc
  character(kind= c_char, len= max_path_len), pointer :: c_filename
  character(len= max_path_len)   :: name_local

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(name, c_filename)

  !  call the Fortran procedure
  ierror = pio_inq_varname(file_desc, int(varid), name_local)

  !  convert the arguments back to C
  call c_chars(c_filename, name_local)
  ierr = int(ierror, c_int)

  !  return to the cpp caller
  return
end function pio_cpp_inq_varname_vid

! ---------------------------------------------------------------------

! extern "C" int pio_cpp_inq_varndims_vid(pio_file_desc_t file,               &
!                                         int varid, int *ndims);

! ---------------------------------------------------------------------

function pio_cpp_inq_varndims_vid(file, varid, ndims) result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t

  !  import nf procedure signatures
  use nf_mod, only: pio_inq_varndims

  !  function result
  integer(c_int)              :: ierr

  !  dummy arguments
  type(c_ptr),    value       :: file
  integer(c_int), value       :: varid
  integer(c_int), intent(out) :: ndims

  !  local
  integer                     :: ierror
  integer                     :: ndims_local

  type(file_desc_t), pointer :: file_desc

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)

  !  call the Fortran procedure
  ierror = pio_inq_varndims(file_desc, int(varid), ndims_local)

  !  convert the arguments back to C
  ierr = int(ierror, c_int)
  ndims = int(ndims_local, c_int)

  !  return to the cpp caller
  return
end function pio_cpp_inq_varndims_vid

! ---------------------------------------------------------------------

! extern "C" int pio_cpp_inq_varndims_vdesc(pio_file_desc_t file,             &
!                                           pio_var_desc_t vardesc,           &
!                                           int *ndims);

! ---------------------------------------------------------------------

function pio_cpp_inq_varndims_vdesc(file, vardesc, ndims) result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t, var_desc_t

  !  import nf procedure signatures
  use nf_mod, only: pio_inq_varndims

  !  function result
  integer(c_int)              :: ierr

  !  dummy arguments
  type(c_ptr),    value       :: file
  type(c_ptr),    value       :: vardesc
  integer(c_int), intent(out) :: ndims

  !  local
  integer                     :: ierror
  integer                     :: ndims_local

  type(file_desc_t), pointer :: file_desc
  type(var_desc_t),  pointer :: var_desc

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(vardesc, var_desc)

  !  call the Fortran procedure
  ierror = pio_inq_varndims(file_desc, var_desc, ndims_local)

  !  convert the arguments back to C
  ierr = int(ierror, c_int)
  ndims = int(ndims_local, c_int)

  !  return to the cpp caller
  return
end function pio_cpp_inq_varndims_vdesc

! ---------------------------------------------------------------------

! extern "C" int pio_cpp_inq_vartype_vid(pio_file_desc_t file,               &
!                                         int varid, int *type);

! ---------------------------------------------------------------------

function pio_cpp_inq_vartype_vid(file, varid, type) result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t

  !  import nf procedure signatures
  use nf_mod, only: pio_inq_vartype

  !  function result
  integer(c_int)              :: ierr

  !  dummy arguments
  type(c_ptr),    value       :: file
  integer(c_int), value       :: varid
  integer(c_int), intent(out) :: type

  !  local
  integer                     :: ierror
  integer                     :: type_local

  type(file_desc_t), pointer :: file_desc

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)

  !  call the Fortran procedure
  ierror = pio_inq_vartype(file_desc, int(varid), type_local)

  !  convert the arguments back to C
  ierr = int(ierror, c_int)
  type = int(type_local, c_int)

  !  return to the cpp caller
  return
end function pio_cpp_inq_vartype_vid

! ---------------------------------------------------------------------

! extern "C" int pio_cpp_inq_vardimid_vid(pio_file_desc_t file,               &
!                                         int varid, int *dimids, int ndims);

! ---------------------------------------------------------------------

function pio_cpp_inq_vardimid_vid(file, varid, dimids, ndims)                 &
         result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t

  !  import nf procedure signatures
  use nf_mod, only: pio_inq_vardimid

  !  function result
  integer(c_int)              :: ierr

  !  dummy arguments
  type(c_ptr),    value       :: file
  integer(c_int), value       :: varid
  type(c_ptr),    value       :: dimids
  integer(c_int), value       :: ndims

  !  local
  integer                     :: ierror
  type(file_desc_t), pointer  :: file_desc
  integer(c_int),    pointer  :: as_dims(:)

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(dimids, as_dims, shape= [ ndims ])

  !  call the Fortran procedure
  ierror = pio_inq_vardimid(file_desc, int(varid), as_dims)

  !  convert the arguments back to C
  ierr = int(ierror, c_int)

  !  return to the cpp caller
  return
end function pio_cpp_inq_vardimid_vid

! ---------------------------------------------------------------------

! extern "C" int pio_cpp_inq_vardimid_vdesc(pio_file_desc_t file,             &
!                                           int vardesc, int *dimids,         &
!                                           int ndims);

! ---------------------------------------------------------------------

function pio_cpp_inq_vardimid_vdesc(file, vardesc, dimids, ndims)             &
         result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t, var_desc_t

  !  import nf procedure signatures
  use nf_mod, only: pio_inq_vardimid

  !  function result
  integer(c_int)              :: ierr

  !  dummy arguments
  type(c_ptr),    value       :: file
  type(c_ptr),    value       :: vardesc
  type(c_ptr),    value       :: dimids
  integer(c_int), value       :: ndims

  !  local
  integer                     :: ierror
  integer(c_int),    pointer  :: as_dims(:)
  type(file_desc_t), pointer  :: file_desc
  type(var_desc_t),  pointer  :: var_desc

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(vardesc, var_desc)
  call c_f_pointer(dimids, as_dims, shape= [ ndims ])

  !  call the Fortran procedure
  ierror = pio_inq_vardimid(file_desc, var_desc, as_dims)

  !  convert the arguments back to C
  ierr = int(ierror, c_int)

  !  return to the cpp caller
  return
end function pio_cpp_inq_vardimid_vdesc

! ---------------------------------------------------------------------

! extern "C" int pio_cpp_inq_vartype_vdesc(pio_file_desc_t file,             &
!                                           pio_var_desc_t vardesc,           &
!                                           int *type);

! ---------------------------------------------------------------------

function pio_cpp_inq_vartype_vdesc(file, vardesc, type) result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t, var_desc_t

  !  import nf procedure signatures
  use nf_mod, only: pio_inq_vartype

  !  function result
  integer(c_int)              :: ierr

  !  dummy arguments
  type(c_ptr),    value       :: file
  type(c_ptr),    value       :: vardesc
  integer(c_int), intent(out) :: type

  !  local
  integer                     :: ierror
  integer                     :: type_local

  type(file_desc_t), pointer :: file_desc
  type(var_desc_t),  pointer :: var_desc

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(vardesc, var_desc)

  !  call the Fortran procedure
  ierror = pio_inq_vartype(file_desc, var_desc, type_local)

  !  convert the arguments back to C
  ierr = int(ierror, c_int)
  type = int(type_local, c_int)

  !  return to the cpp caller
  return
end function pio_cpp_inq_vartype_vdesc

! ---------------------------------------------------------------------

! extern "C" int pio_cpp_inq_varnatts_vid(pio_file_desc_t file,               &
!                                         int varid, int *natts);

! ---------------------------------------------------------------------

function pio_cpp_inq_varnatts_vid(file, varid, natts) result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t

  !  import nf procedure signatures
  use nf_mod, only: pio_inq_varnatts

  !  function result
  integer(c_int)              :: ierr

  !  dummy arguments
  type(c_ptr),    value       :: file
  integer(c_int), value       :: varid
  integer(c_int), intent(out) :: natts

  !  local
  integer                     :: ierror
  integer                     :: natts_local

  type(file_desc_t), pointer :: file_desc

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)

  !  call the Fortran procedure
  ierror = pio_inq_varnatts(file_desc, int(varid), natts_local)

  !  convert the arguments back to C
  ierr = int(ierror, c_int)
  natts = int(natts_local, c_int)

  !  return to the cpp caller
  return
end function pio_cpp_inq_varnatts_vid

! ---------------------------------------------------------------------

! extern "C" int pio_cpp_inq_varnatts_vdesc(pio_file_desc_t file,             &
!                                           pio_var_desc_t vardesc,           &
!                                           int *natts);

! ---------------------------------------------------------------------

function pio_cpp_inq_varnatts_vdesc(file, vardesc, natts) result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t, var_desc_t

  !  import nf procedure signatures
  use nf_mod, only: pio_inq_varnatts

  !  function result
  integer(c_int)              :: ierr

  !  dummy arguments
  type(c_ptr),    value       :: file
  type(c_ptr),    value       :: vardesc
  integer(c_int), intent(out) :: natts

  !  local
  integer                     :: ierror
  integer                     :: natts_local

  type(file_desc_t), pointer :: file_desc
  type(var_desc_t),  pointer :: var_desc

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(vardesc, var_desc)

  !  call the Fortran procedure
  ierror = pio_inq_varnatts(file_desc, var_desc, natts_local)

  !  convert the arguments back to C
  ierr = int(ierror, c_int)
  natts = int(natts_local, c_int)

  !  return to the cpp caller
  return
end function pio_cpp_inq_varnatts_vdesc

! ---------------------------------------------------------------------

! extern "C" int pio_cpp_inq_dimid(pio_file_desc_t file,                      &
!                                  const char *name, int *dimid);

! ---------------------------------------------------------------------

function pio_cpp_inq_dimid(file, name, dimid) result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_char, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t

  !  import nf procedure signatures
  use nf_mod, only: pio_inq_dimid
  use pio_cpp_utils, only: f_chars, c_len, max_path_len

  !  function result
  integer(c_int)              :: ierr

  !  dummy arguments
  type(c_ptr),    value       :: file
  type(c_ptr),    value       :: name
  integer(c_int), intent(out) :: dimid

  !  local
  integer                     :: ierror
  integer                     :: dimid_local

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
  call c_f_pointer(name, c_filename)

  clen = c_len(c_filename)

#ifdef ALLOC_CHARLEN_OK
  allocate(filename, mold= filename(1: clen))
  call f_chars(filename, c_filename(1: clen))
#else
  filename = c_filename(1:clen)
#endif

  !  call the Fortran procedure
  ierror = pio_inq_dimid(file_desc, filename, dimid_local)

  !  convert the arguments back to C
  ierr = int(ierror, c_int)
  dimid = int(dimid_local, c_int)

  !  return to the cpp caller
  return
end function pio_cpp_inq_dimid

! ---------------------------------------------------------------------

! extern "C" int pio_cpp_inq_dimname(pio_file_desc_t file, int dimid,         &
!                                    char *name);

! ---------------------------------------------------------------------

function pio_cpp_inq_dimname(file, dimid, name) result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_char, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t

  !  import nf procedure signatures
  use nf_mod, only: pio_inq_dimname
  use pio_cpp_utils, only: c_chars, c_len, max_path_len

  !  function result
  integer(c_int)              :: ierr

  !  dummy arguments
  type(c_ptr),    value       :: file
  integer(c_int), value       :: dimid
  type(c_ptr),    value       :: name

  !  local
  integer                     :: ierror

  type(file_desc_t), pointer :: file_desc
  character(kind= c_char, len= max_path_len), pointer :: c_filename
  character(len= max_path_len)   :: name_local

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(name, c_filename)

  !  call the Fortran procedure
  ierror = pio_inq_dimname(file_desc, int(dimid), name_local)

  !  convert the arguments back to C
  call c_chars(c_filename, name_local)
  ierr = int(ierror, c_int)

  !  return to the cpp caller
  return
end function pio_cpp_inq_dimname

! ---------------------------------------------------------------------

! extern "C" int pio_cpp_inq_dimlen(pio_file_desc_t file,                     &
!                                   int dimid, int *dimlen);

! ---------------------------------------------------------------------

function pio_cpp_inq_dimlen(file, dimid, dimlen) result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t

  !  import nf procedure signatures
  use nf_mod, only: pio_inq_dimlen

  !  function result
  integer(c_int)              :: ierr

  !  dummy arguments
  type(c_ptr),    value       :: file
  integer(c_int), value       :: dimid
  integer(c_int), intent(out) :: dimlen

  !  local
  integer                     :: ierror
  integer                     :: dimlen_local

  type(file_desc_t), pointer :: file_desc

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)

  !  call the Fortran procedure
  ierror = pio_inq_dimlen(file_desc, int(dimid), dimlen_local)

  !  convert the arguments back to C
  ierr = int(ierror, c_int)
  dimlen = int(dimlen_local, c_int)

  !  return to the cpp caller
  return
end function pio_cpp_inq_dimlen

! ---------------------------------------------------------------------

! extern "C" int pio_cpp_def_dim(pio_file_desc_t file, const char *name,      &
!                                int len, int *dimid);

function pio_cpp_def_dim(file, name, len, dimid) result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t

  !  import nf procedure signatures
  use nf_mod, only: pio_def_dim
  use pio_cpp_utils, only: f_chars, c_len, max_path_len

  !  function result
  integer(c_int) :: ierr

  !  dummy arguments
  type(c_ptr), value :: file
  type(c_ptr), value :: name
  integer(c_int), value :: len
  integer(c_int), intent(out) :: dimid

  !  local
  integer :: ierror

  type(file_desc_t), pointer :: file_desc
  character(kind= c_char, len= max_path_len), pointer :: c_filename
#ifdef ALLOC_CHARLEN_OK
  character(len= :), allocatable :: filename
#else
  character(len= max_path_len) :: filename
#endif

  integer :: clen
  integer :: dim_id

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(name, c_filename)

  !  convert the C string to Fortran characters
  clen = c_len(c_filename)

#ifdef ALLOC_CHARLEN_OK
  allocate(filename, mold= filename(1: clen))
  call f_chars(filename, c_filename(1: clen))
#else
  filename = c_filename(1:clen)
#endif

  !  call the Fortran procedure
  ierror = pio_def_dim(file_desc, trim(filename), int(len), dim_id)

  !  convert the arguments back to C
  dimid = int(dim_id, c_int)
  ierr = int(ierror, c_int)

  !  return to the cpp caller
  return
end function pio_cpp_def_dim

! ---------------------------------------------------------------------

! extern "C" int pio_cpp_enddef(pio_file_desc_t file);

function pio_cpp_enddef(file) result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t

  !  import nf procedure signatures
  use nf_mod, only: pio_enddef

  !  function result
  integer(c_int) :: ierr

  !  dummy arguments
  type(c_ptr), value :: file

  !  local
  integer :: ierror

  type(file_desc_t), pointer :: file_desc

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)

  !  call the Fortran procedure
  ierror = pio_enddef(file_desc)

  !  convert the arguments back to C
  ierr = int(ierror, c_int)

  !  return to the cpp caller
  return
end function pio_cpp_enddef

! ---------------------------------------------------------------------

! extern "C" int pio_cpp_redef(pio_file_desc_t file);

function pio_cpp_redef(file) result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t

  !  import nf procedure signatures
  use nf_mod, only: pio_redef

  !  function result
  integer(c_int) :: ierr

  !  dummy arguments
  type(c_ptr), value :: file

  !  local
  integer :: ierror

  type(file_desc_t), pointer :: file_desc

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)

  !  call the Fortran procedure
  ierror = pio_redef(file_desc)

  !  convert the arguments back to C
  ierr = int(ierror, c_int)

  !  return to the cpp caller
  return
end function pio_cpp_redef

!  extern "C" int pio_cpp_def_var_0d(pio_file_desc_t file, const char *name,  &
!                                    int type, pio_var_desc_t vardesc);

function pio_cpp_def_var_0d(file, name, type, vardesc) result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: var_desc_t, file_desc_t

  !  import nf procedure signatures
  use nf_mod, only: pio_def_var
  use pio_cpp_utils, only: f_chars, c_len, max_path_len

  !  function result
  integer(c_int) :: ierr

  !  dummy arguments
  type(c_ptr), value :: file
  type(c_ptr), value :: name
  integer(c_int), value :: type
  type(c_ptr), value :: vardesc

  !  local
  integer :: ierror

  type(file_desc_t), pointer :: file_desc
  character(kind= c_char, len= max_path_len), pointer :: c_filename
#ifdef ALLOC_CHARLEN_OK
  character(len= :), allocatable :: filename
#else
  character(len= max_path_len) :: filename
#endif
  type(Var_desc_t), pointer :: var_desc

  integer :: clen

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(name, c_filename)
  call c_f_pointer(vardesc, var_desc)

  !  convert the C string to Fortran characters
  clen = c_len(c_filename)

#ifdef ALLOC_CHARLEN_OK
  allocate(filename, mold= filename(1: clen))
  call f_chars(filename, c_filename(1: clen))
#else
  filename = c_filename(1:clen)
#endif

  !  call the Fortran procedure
  ierror = pio_def_var(file_desc, trim(filename), int(type), var_desc)

  !  convert the arguments back to C
  ierr = int(ierror, c_int)

  !  return to the cpp caller
  return

end function pio_cpp_def_var_0d

! extern "C" int pio_cpp_def_var_md(pio_file_desc_t file, const char *name,   &
!                                   int type, int *dimds, int ndimds,         &
!                                   pio_var_desc_t vardesc);

function pio_cpp_def_var_md(file, name, type, dimds, ndimds, vardesc)         &
         result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: var_desc_t, file_desc_t

  !  import nf procedure signatures
  use nf_mod, only: pio_def_var
  use pio_cpp_utils, only: f_chars, c_len, max_path_len

  !  function result
  integer(c_int) :: ierr

  !  dummy arguments
  type(c_ptr), value :: file
  type(c_ptr), value :: name
  integer(c_int), value :: type
  type(c_ptr), value :: dimds
  integer(c_int), value :: ndimds
  type(c_ptr), value :: vardesc

  !  local
  integer :: ierror

  type(file_desc_t), pointer :: file_desc
  character(kind= c_char, len= max_path_len), pointer :: c_filename
#ifdef ALLOC_CHARLEN_OK
  character(len= :), allocatable :: filename
#else
  character(len= max_path_len) :: filename
#endif
  integer(c_int), dimension(:), pointer :: as_dimds
  type(Var_desc_t), pointer :: var_desc

  integer :: clen

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(name, c_filename)
  call c_f_pointer(dimds, as_dimds, shape= [ ndimds ])
  call c_f_pointer(vardesc, var_desc)

  !  convert the C string to Fortran characters
  clen = c_len(c_filename)

#ifdef ALLOC_CHARLEN_OK
  allocate(filename, mold= filename(1: clen))
  call f_chars(filename, c_filename(1: clen))
#else
  filename = c_filename(1:clen)
#endif

  !  call the Fortran procedure
  ierror = pio_def_var(file_desc, trim(filename), int(type),                  &
                       int(as_dimds), var_desc)

  !  convert the arguments back to C
  ierr = int(ierror, c_int)

  !  return to the cpp caller
  return

end function pio_cpp_def_var_md

! extern "C" int pio_cpp_copy_att(pio_file_desc_t infile, int invarid,        &
!                                 const char *name,                           &
!                                 pio_file_desc_t outfile, int outvarid);

function pio_cpp_copy_att(infile, invarid, name, outfile, outvarid)           &
         result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_f_pointer

  !  import pio types
  use pio_types, only: file_desc_t

  !  import nf procedure signatures
  use nf_mod, only: pio_copy_att
  use pio_cpp_utils, only: f_chars, c_len, max_path_len

  !  function result
  integer(c_int) :: ierr

  !  dummy arguments
  type(c_ptr), value             :: infile
  type(c_ptr), value             :: outfile
  type(c_ptr), value             :: name
  integer(c_int), value          :: invarid
  integer(c_int), value          :: outvarid

  !  local
  integer :: ierror

  type(file_desc_t), pointer     :: infile_desc
  type(file_desc_t), pointer     :: outfile_desc
  character(kind= c_char, len= max_path_len), pointer :: c_attname
#ifdef ALLOC_CHARLEN_OK
  character(len= :), allocatable :: attname
#else
  character(len= max_path_len)   :: attname
#endif

  integer                        :: clen

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(infile, infile_desc)
  call c_f_pointer(infile, outfile_desc)
  call c_f_pointer(name, c_attname)

  !  convert the C string to Fortran characters
  clen = c_len(c_attname)

#ifdef ALLOC_CHARLEN_OK
  allocate(attname, mold= attname(1: clen))
  call f_chars(attname, c_attname(1: clen))
#else
  attname = c_attname(1:clen)
#endif

  !  call the Fortran procedure
  ierror = pio_copy_att(infile_desc, int(invarid), trim(attname),             &
                        outfile_desc, int(outvarid))

  !  convert the arguments back to C
  ierr = int(ierror, c_int)

  !  return to the cpp caller
  return

end function pio_cpp_copy_att

! ---------------------------------------------------------------------

! extern "C" int pio_cpp_put_att_string (pio_file_desc_t file, pio_var_desc_t varDesc,
!                              const char *name, const char *value);

function pio_cpp_put_att_string (file, vardesc, name, value) result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_f_pointer
  use pio_cpp_utils, only: f_chars, c_len, max_path_len

  !  import pio types
  use pio_types, only: file_desc_t, Var_desc_t, PIO_global

  !  import nf procedure signatures
  use pionfatt_mod, only: put_att

  !  function result
  integer(c_int) :: ierr

  !  dummy arguments
  type(c_ptr),    value :: file
  type(c_ptr),    value :: vardesc
  type(c_ptr),    value :: name
  type(c_ptr),    value :: value

  !  local
  integer :: ierror

  type(file_desc_t), pointer :: file_desc
  type(Var_desc_t),  pointer :: var_desc
  character(kind= c_char, len= max_path_len), pointer :: c_attname, c_attvalue
#ifdef ALLOC_CHARLEN_OK
  character(len= :), allocatable :: attname, attvalue
#else
  character(len= max_path_len)   :: attname, attvalue
#endif
  integer :: clen

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(vardesc, var_desc)
  call c_f_pointer(name, c_attname)
  call c_f_pointer(value, c_attvalue)

  clen = c_len(c_attname)
#ifdef ALLOC_CHARLEN_OK
  allocate(attname, mold= attname(1: clen))
  call f_chars(attname, c_attname(1: clen))
#else
  attname = c_attname(1:clen)
#endif

  clen = c_len(c_attvalue)
#ifdef ALLOC_CHARLEN_OK
  allocate(attvalue, mold= attvalue(1: clen))
  call f_chars(attvalue, c_attvalue(1: clen))
#else
  attvalue = c_attvalue(1:clen)
#endif

  !  call the Fortran procedure
  if (associated (var_desc)) then
    ierror = put_att (file_desc, var_desc%varID, attname, attvalue(:clen))
  else
    ierror = put_att (file_desc, PIO_global, attname, attvalue(:clen))
  end if

  !  convert the arguments back to C
  ierr = int(ierror, c_int)

  !  return to the cpp caller
  return
end function pio_cpp_put_att_string

! ---------------------------------------------------------------------

! extern "C" int pio_cpp_put_att_ints (pio_file_desc_t file, pio_var_desc_t varDesc,
!                              const char *name, const int *values, const int nvalues);

function pio_cpp_put_att_ints (file, vardesc, name, values, nvalues) result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_char, c_int, c_float, c_ptr, c_f_pointer
  use pio_cpp_utils, only: f_chars, c_len, max_path_len

  !  import pio types
  use pio_types, only: file_desc_t, Var_desc_t, PIO_global

  !  import nf procedure signatures
  use pionfatt_mod, only: put_att

  !  function result
  integer(c_int) :: ierr

  !  dummy arguments
  type(c_ptr),    value :: file
  type(c_ptr),    value :: vardesc
  type(c_ptr),    value :: name
  integer(c_int)        :: values(*)
  integer(c_int), value :: nvalues

  !  local
  integer :: ierror

  type(file_desc_t), pointer :: file_desc
  type(Var_desc_t),  pointer :: var_desc
  character(kind= c_char, len= max_path_len), pointer :: c_attname
#ifdef ALLOC_CHARLEN_OK
  character(len= :), allocatable :: attname
#else
  character(len= max_path_len)   :: attname
#endif
  integer :: attvalues(int (nvalues))
  integer :: clen

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(vardesc, var_desc)
  call c_f_pointer(name, c_attname)

  clen = c_len(c_attname)
#ifdef ALLOC_CHARLEN_OK
  allocate(attname, mold= attname(1: clen))
  call f_chars(attname, c_attname(1: clen))
#else
  attname = c_attname(1:clen)
#endif

  attvalues = values(:int (nvalues))

  !  call the Fortran procedure
  if (associated (var_desc)) then
    ierror = put_att (file_desc, var_desc%varID, attname, attvalues)
  else
    ierror = put_att (file_desc, PIO_global, attname, attvalues)
  end if

  !  convert the arguments back to C
  ierr = int(ierror, c_int)

  !  return to the cpp caller
  return
end function pio_cpp_put_att_ints

! ---------------------------------------------------------------------

! extern "C" int pio_cpp_put_att_floats (pio_file_desc_t file, pio_var_desc_t varDesc,
!                              const char *name, const float *values, const int nvalues);

function pio_cpp_put_att_floats (file, vardesc, name, values, nvalues) result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_char, c_int, c_float, c_ptr, c_f_pointer
  use pio_cpp_utils, only: f_chars, c_len, max_path_len

  !  import pio types
  use pio_types, only: file_desc_t, Var_desc_t, PIO_global

  !  import nf procedure signatures
  use pionfatt_mod, only: put_att

  !  function result
  integer(c_int) :: ierr

  !  dummy arguments
  type(c_ptr),    value :: file
  type(c_ptr),    value :: vardesc
  type(c_ptr),    value :: name
  real(c_float)         :: values(*)
  integer(c_int), value :: nvalues

  !  local
  integer :: ierror

  type(file_desc_t), pointer :: file_desc
  type(Var_desc_t),  pointer :: var_desc
  character(kind= c_char, len= max_path_len), pointer :: c_attname
#ifdef ALLOC_CHARLEN_OK
  character(len= :), allocatable :: attname
#else
  character(len= max_path_len)   :: attname
#endif
  real :: attvalues(int (nvalues))
  integer :: clen

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(vardesc, var_desc)
  call c_f_pointer(name, c_attname)

  clen = c_len(c_attname)
#ifdef ALLOC_CHARLEN_OK
  allocate(attname, mold= attname(1: clen))
  call f_chars(attname, c_attname(1: clen))
#else
  attname = c_attname(1:clen)
#endif

  attvalues = values(:int (nvalues))

  !  call the Fortran procedure
  if (associated (var_desc)) then
    ierror = put_att (file_desc, var_desc%varID, attname, attvalues)
  else
    ierror = put_att (file_desc, PIO_global, attname, attvalues)
  end if

  !  convert the arguments back to C
  ierr = int(ierror, c_int)

  !  return to the cpp caller
  return
end function pio_cpp_put_att_floats

! ---------------------------------------------------------------------

! extern "C" int pio_cpp_put_att_doubles (pio_file_desc_t file, pio_var_desc_t varDesc,
!                              const char *name, const double *values, const int nvalues);

function pio_cpp_put_att_doubles (file, vardesc, name, values, nvalues) result(ierr) bind(c)

  !  bind to C
  use, intrinsic :: iso_c_binding, only: c_char, c_int, c_double, c_ptr, c_f_pointer
  use pio_cpp_utils, only: f_chars, c_len, max_path_len

  !  import pio types
  use pio_types, only: file_desc_t, Var_desc_t, PIO_global

  !  import nf procedure signatures
  use pionfatt_mod, only: put_att

  !  function result
  integer(c_int) :: ierr

  !  dummy arguments
  type(c_ptr),    value :: file
  type(c_ptr),    value :: vardesc
  type(c_ptr),    value :: name
  real(c_double)        :: values(*)
  integer(c_int), value :: nvalues

  !  local
  integer :: ierror

  type(file_desc_t), pointer :: file_desc
  type(Var_desc_t),  pointer :: var_desc
  character(kind= c_char, len= max_path_len), pointer :: c_attname
#ifdef ALLOC_CHARLEN_OK
  character(len= :), allocatable :: attname
#else
  character(len= max_path_len)   :: attname
#endif
  real(kind (0.0d0)) :: attvalues(int (nvalues))
  integer :: clen

  !  text
  continue

  !  convert the C pointers to a Fortran pointers
  call c_f_pointer(file, file_desc)
  call c_f_pointer(vardesc, var_desc)
  call c_f_pointer(name, c_attname)

  clen = c_len(c_attname)
#ifdef ALLOC_CHARLEN_OK
  allocate(attname, mold= attname(1: clen))
  call f_chars(attname, c_attname(1: clen))
#else
  attname = c_attname(1:clen)
#endif

  attvalues = values(:int (nvalues))

  !  call the Fortran procedure
  if (associated (var_desc)) then
    ierror = put_att (file_desc, var_desc%varID, attname, attvalues)
  else
    ierror = put_att (file_desc, PIO_global, attname, attvalues)
  end if

  !  convert the arguments back to C
  ierr = int(ierror, c_int)

  !  return to the cpp caller
  return
end function pio_cpp_put_att_doubles

! ---------------------------------------------------------------------
