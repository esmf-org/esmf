!
! Stub replacement for netcdf library when netcdf isn't available.
! Note: this file has changed to reflect the structure of the
!       netcdf.inc file included in the same directory
!

!
! miscellaneous routines:
!
function nf_inq_libvers() result(status)
  character*80 :: status
  status=''
end function nf_inq_libvers

function nf_strerror() result(status)
  character*80 :: status
  status=''
end function nf_strerror

function nf_issyserr() result(status)
  logical :: status
  status=.false.
end function nf_issyserr
!
! control routines:
!
function nf_inq_base_pe() result(status)
  integer :: status
  status=0
end function nf_inq_base_pe

function nf_set_base_pe() result(status)
  integer :: status
  status=0
end function nf_set_base_pe

function nf_create() result(status)
  integer :: status
  status=0
end function nf_create

function nf__create() result(status)
  integer :: status
  status=0
end function nf__create

function nf__create_mp() result(status)
  integer :: status
  status=0
end function nf__create_mp

function nf_open() result(status)
  integer :: status
  status=0
end function nf_open

function nf__open() result(status)
  integer :: status
  status=0
end function nf__open

function nf__open_mp() result(status)
  integer :: status
  status=0
end function nf__open_mp

function nf_set_fill() result(status)
  integer :: status
  status=0
end function nf_set_fill

function nf_redef() result(status)
  integer :: status
  status=0
end function nf_redef

function nf_enddef() result(status)
  integer :: status
  status=0
end function nf_enddef

function nf__enddef() result(status)
  integer :: status
  status=0
end function nf__enddef

function nf_sync() result(status)
  integer :: status
  status=0
end function nf_sync

function nf_abort() result(status)
  integer :: status
  status=0
end function nf_abort

function nf_close() result(status)
  integer :: status
  status=0
end function nf_close

function nf_delete() result(status)
  integer :: status
  status=0
end function nf_delete
!
! general inquiry routines:
!
function nf_inq() result(status)
  integer :: status
  status=0
end function nf_inq

function nf_inq_ndims() result(status)
  integer :: status
  status=0
end function nf_inq_ndims

function nf_inq_nvars() result(status)
  integer :: status
  status=0
end function nf_inq_nvars

function nf_inq_natts() result(status)
  integer :: status
  status=0
end function nf_inq_natts

function nf_inq_unlimdim() result(status)
  integer :: status
  status=0
end function nf_inq_unlimdim
!
! dimension routines:
!
function nf_def_dim() result(status)
  integer :: status
  status=0
end function nf_def_dim

function nf_inq_dimid() result(status)
  integer :: status
  status=0
end function nf_inq_dimid

function nf_inq_dim() result(status)
  integer :: status
  status=0
end function nf_inq_dim

function nf_inq_dimname() result(status)
  integer :: status
  status=0
end function nf_inq_dimname

function nf_inq_dimlen() result(status)
  integer :: status
  status=0
end function nf_inq_dimlen

function nf_rename_dim() result(status)
  integer :: status
  status=0
end function nf_rename_dim
!
! general attribute routines:
!
function nf_inq_att() result(status)
  integer :: status
  status=0
end function nf_inq_att

function nf_inq_attid() result(status)
  integer :: status
  status=0
end function nf_inq_attid

function nf_inq_atttype() result(status)
  integer :: status
  status=0
end function nf_inq_atttype

function nf_inq_attlen() result(status)
  integer :: status
  status=0
end function nf_inq_attlen

function nf_inq_attname() result(status)
  integer :: status
  status=0
end function nf_inq_attname

function nf_copy_att() result(status)
  integer :: status
  status=0
end function nf_copy_att

function nf_rename_att() result(status)
  integer :: status
  status=0
end function nf_rename_att

function nf_del_att() result(status)
  integer :: status
  status=0
end function nf_del_att
!
! attribute put/get routines:
!
function nf_put_att_text() result(status)
  integer :: status
  status=0
end function nf_put_att_text

function nf_get_att_text() result(status)
  integer :: status
  status=0
end function nf_get_att_text

function nf_put_att_int1() result(status)
  integer :: status
  status=0
end function nf_put_att_int1

function nf_get_att_int1() result(status)
  integer :: status
  status=0
end function nf_get_att_int1

function nf_put_att_int2() result(status)
  integer :: status
  status=0
end function nf_put_att_int2

function nf_get_att_int2() result(status)
  integer :: status
  status=0
end function nf_get_att_int2

function nf_put_att_int() result(status)
  integer :: status
  status=0
end function nf_put_att_int

function nf_get_att_int() result(status)
  integer :: status
  status=0
end function nf_get_att_int

function nf_put_att_real() result(status)
  integer :: status
  status=0
end function nf_put_att_real

function nf_get_att_real() result(status)
  integer :: status
  status=0
end function nf_get_att_real

function nf_put_att_double() result(status)
  integer :: status
  status=0
end function nf_put_att_double

function nf_get_att_double() result(status)
  integer :: status
  status=0
end function nf_get_att_double
!
! general variable routines:
!
function nf_def_var() result(status)
  integer :: status
  status=0
end function nf_def_var

function nf_inq_var() result(status)
  integer :: status
  status=0
end function nf_inq_var

function nf_inq_varid() result(status)
  integer :: status
  status=0
end function nf_inq_varid

function nf_inq_varname() result(status)
  integer :: status
  status=0
end function nf_inq_varname

function nf_inq_vartype() result(status)
  integer :: status
  status=0
end function nf_inq_vartype

function nf_inq_varndims() result(status)
  integer :: status
  status=0
end function nf_inq_varndims

function nf_inq_vardimid() result(status)
  integer :: status
  status=0
end function nf_inq_vardimid

function nf_inq_varnatts() result(status)
  integer :: status
  status=0
end function nf_inq_varnatts

function nf_rename_var() result(status)
  integer :: status
  status=0
end function nf_rename_var

function nf_copy_var() result(status)
  integer :: status
  status=0
end function nf_copy_var
!
! entire variable put/get routines:
!
function nf_put_var_text() result(status)
  integer :: status
  status=0
end function nf_put_var_text

function nf_get_var_text() result(status)
  integer :: status
  status=0
end function nf_get_var_text

function nf_put_var_int1() result(status)
  integer :: status
  status=0
end function nf_put_var_int1

function nf_get_var_int1() result(status)
  integer :: status
  status=0
end function nf_get_var_int1

function nf_put_var_int2() result(status)
  integer :: status
  status=0
end function nf_put_var_int2

function nf_get_var_int2() result(status)
  integer :: status
  status=0
end function nf_get_var_int2

function nf_put_var_int() result(status)
  integer :: status
  status=0
end function nf_put_var_int

function nf_get_var_int() result(status)
  integer :: status
  status=0
end function nf_get_var_int

function nf_put_var_real() result(status)
  integer :: status
  status=0
end function nf_put_var_real

function nf_get_var_real() result(status)
  integer :: status
  status=0
end function nf_get_var_real

function nf_put_var_double() result(status)
  integer :: status
  status=0
end function nf_put_var_double

function nf_get_var_double() result(status)
  integer :: status
  status=0
end function nf_get_var_double
!
! single variable put/get routines:
!
function nf_put_var1_text() result(status)
  integer :: status
  status=0
end function nf_put_var1_text

function nf_get_var1_text() result(status)
  integer :: status
  status=0
end function nf_get_var1_text

function nf_put_var1_int1() result(status)
  integer :: status
  status=0
end function nf_put_var1_int1

function nf_get_var1_int1() result(status)
  integer :: status
  status=0
end function nf_get_var1_int1

function nf_put_var1_int2() result(status)
  integer :: status
  status=0
end function nf_put_var1_int2

function nf_get_var1_int2() result(status)
  integer :: status
  status=0
end function nf_get_var1_int2

function nf_put_var1_int() result(status)
  integer :: status
  status=0
end function nf_put_var1_int

function nf_get_var1_int() result(status)
  integer :: status
  status=0
end function nf_get_var1_int

function nf_put_var1_real() result(status)
  integer :: status
  status=0
end function nf_put_var1_real

function nf_get_var1_real() result(status)
  integer :: status
  status=0
end function nf_get_var1_real

function nf_put_var1_double() result(status)
  integer :: status
  status=0
end function nf_put_var1_double

function nf_get_var1_double() result(status)
  integer :: status
  status=0
end function nf_get_var1_double
!
! variable array put/get routines:
!
function nf_put_vara_text() result(status)
  integer :: status
  status=0
end function nf_put_vara_text

function nf_get_vara_text() result(status)
  integer :: status
  status=0
end function nf_get_vara_text

function nf_put_vara_int1() result(status)
  integer :: status
  status=0
end function nf_put_vara_int1

function nf_get_vara_int1() result(status)
  integer :: status
  status=0
end function nf_get_vara_int1

function nf_put_vara_int2() result(status)
  integer :: status
  status=0
end function nf_put_vara_int2

function nf_get_vara_int2() result(status)
  integer :: status
  status=0
end function nf_get_vara_int2

function nf_put_vara_int() result(status)
  integer :: status
  status=0
end function nf_put_vara_int

function nf_get_vara_int() result(status)
  integer :: status
  status=0
end function nf_get_vara_int

function nf_put_vara_real() result(status)
  integer :: status
  status=0
end function nf_put_vara_real

function nf_get_vara_real() result(status)
  integer :: status
  status=0
end function nf_get_vara_real

function nf_put_vara_double() result(status)
  integer :: status
  status=0
end function nf_put_vara_double

function nf_get_vara_double() result(status)
  integer :: status
  status=0
end function nf_get_vara_double
!
! strided variable put/get routines:
!
function nf_put_vars_text() result(status)
  integer :: status
  status=0
end function nf_put_vars_text

function nf_get_vars_text() result(status)
  integer :: status
  status=0
end function nf_get_vars_text

function nf_put_vars_int1() result(status)
  integer :: status
  status=0
end function nf_put_vars_int1

function nf_get_vars_int1() result(status)
  integer :: status
  status=0
end function nf_get_vars_int1

function nf_put_vars_int2() result(status)
  integer :: status
  status=0
end function nf_put_vars_int2

function nf_get_vars_int2() result(status)
  integer :: status
  status=0
end function nf_get_vars_int2

function nf_put_vars_int() result(status)
  integer :: status
  status=0
end function nf_put_vars_int

function nf_get_vars_int() result(status)
  integer :: status
  status=0
end function nf_get_vars_int

function nf_put_vars_real() result(status)
  integer :: status
  status=0
end function nf_put_vars_real

function nf_get_vars_real() result(status)
  integer :: status
  status=0
end function nf_get_vars_real

function nf_put_vars_double() result(status)
  integer :: status
  status=0
end function nf_put_vars_double

function nf_get_vars_double() result(status)
  integer :: status
  status=0
end function nf_get_vars_double
!
! mapped variable put/get routines:
!
function nf_put_varm_text() result(status)
  integer :: status
  status=0
end function nf_put_varm_text

function nf_get_varm_text() result(status)
  integer :: status
  status=0
end function nf_get_varm_text

function nf_put_varm_int1() result(status)
  integer :: status
  status=0
end function nf_put_varm_int1

function nf_get_varm_int1() result(status)
  integer :: status
  status=0
end function nf_get_varm_int1

function nf_put_varm_int2() result(status)
  integer :: status
  status=0
end function nf_put_varm_int2

function nf_get_varm_int2() result(status)
  integer :: status
  status=0
end function nf_get_varm_int2

function nf_put_varm_int() result(status)
  integer :: status
  status=0
end function nf_put_varm_int

function nf_get_varm_int() result(status)
  integer :: status
  status=0
end function nf_get_varm_int

function nf_put_varm_real() result(status)
  integer :: status
  status=0
end function nf_put_varm_real

function nf_get_varm_real() result(status)
  integer :: status
  status=0
end function nf_get_varm_real

function nf_put_varm_double() result(status)
  integer :: status
  status=0
end function nf_put_varm_double

function nf_get_varm_double() result(status)
  integer :: status
  status=0
end function nf_get_varm_double

#ifdef NETCDF2_API

!
! Older, shorter netcdf function names:
!
function nccre() result(status)
  integer :: status
  status=0
end function nccre

function ncopn() result(status)
  integer :: status
  status=0
end function ncopn

function ncddef() result(status)
  integer :: status
  status=0
end function ncddef

function ncdid() result(status)
  integer :: status
  status=0
end function ncdid

function ncvdef() result(status)
  integer :: status
  status=0
end function ncvdef

function ncvid() result(status)
  integer :: status
  status=0
end function ncvid

function nctlen() result(status)
  integer :: status
  status=0
end function nctlen

function ncsfil() result(status)
  integer :: status
  status=0
end function ncsfil
!
! ????????
!
function ncdinq() result(status)
  integer :: status
  status=0
end function ncdinq

function ncvgt1() result(status)
  integer :: status
  status=0
end function ncvgt1

function ncclos() result(status)
  integer :: status
  status=0
end function ncclos

function ncvgt() result(status)
  integer :: status
  status=0
end function ncvgt

#endif


