!
! Stub replacement for netcdf library when netcdf isn't available.
!

function nf_create() result(status)
  integer :: status
  status=0
end function nf_create

function nf_strerror() result(status)
  character*80 :: status
  status=''
end function nf_strerror

function nf_open() result(status)
  integer :: status
  status=0
end function nf_open

function nf_close() result(status)
  integer :: status
  status=0
end function nf_close

function nf_get_att_double() result(status)
  integer :: status
  status=0
end function nf_get_att_double

function nf_get_att_int() result(status)
  integer :: status
  status=0
end function nf_get_att_int

function nf_get_att_real() result(status)
  integer :: status
  status=0
end function nf_get_att_real

function nf_get_att_text() result(status)
  integer :: status
  status=0
end function nf_get_att_text

function nf_get_var_int() result(status)
  integer :: status
  status=0
end function nf_get_var_int

function nf_get_var_text() result(status)
  integer :: status
  status=0
end function nf_get_var_text

function nf_get_var_double() result(status)
  integer :: status
  status=0
end function nf_get_var_double

function nf_get_vara_double() result(status)
  integer :: status
  status=0
end function nf_get_vara_double

function nf_get_vara_int() result(status)
  integer :: status
  status=0
end function nf_get_vara_int

function nf_get_vara_real() result(status)
  integer :: status
  status=0
end function nf_get_vara_real

function nf_get_vara_text() result(status)
  integer :: status
  status=0
end function nf_get_vara_text

function nf_put_att_double() result(status)
  integer :: status
  status=0
end function nf_put_att_double

function nf_put_att_int() result(status)
  integer :: status
  status=0
end function nf_put_att_int

function nf_put_att_real() result(status)
  integer :: status
  status=0
end function nf_put_att_real

function nf_put_att_text() result(status)
  integer :: status
  status=0
end function nf_put_att_text

function nf_put_vara_int() result(status)
  integer :: status
  status=0
end function nf_put_vara_int

function nf_put_vara_real() result(status)
  integer :: status
  status=0
end function nf_put_vara_real

function nf_put_vara_double() result(status)
  integer :: status
  status=0
end function nf_put_vara_double

function nf_put_vara_text() result(status)
  integer :: status
  status=0
end function nf_put_vara_text

function nf_put_var1_int() result(status)
  integer :: status
  status=0
end function nf_put_var1_int

function nf_put_var1_double() result(status)
  integer :: status
  status=0
end function nf_put_var1_double

function nf_put_var_int() result(status)
  integer :: status
  status=0
end function nf_put_var_int

function nf_inq_att() result(status)
  integer :: status
  status=0
end function nf_inq_att

function nf_inq_nvars() result(status)
  integer :: status
  status=0
end function nf_inq_nvars

function nf_inq_var() result(status)
  integer :: status
  status=0
end function nf_inq_var

function nf_inq_varname() result(status)
  integer :: status
  status=0
end function nf_inq_varname

function nf_inq_vartype() result(status)
  integer :: status
  status=0
end function nf_inq_vartype

function nf_inq_varid() result(status)
  integer :: status
  status=0
end function nf_inq_varid

function nf_inq_libvers() result(status)
  character*80 :: status
  status=''
end function nf_inq_libvers

function nf_def_var() result(status)
  integer :: status
  status=0
end function nf_def_var

function nf_def_var1d() result(status)
  integer :: status
  status=0
end function nf_def_var1d

function nf_def_dim() result(status)
  integer :: status
  status=0
end function nf_def_dim

function nf_redef() result(status)
  integer :: status
  status=0
end function nf_redef

function nf_enddef() result(status)
  integer :: status
  status=0
end function nf_enddef

function nf_sync() result(status)
  integer :: status
  status=0
end function nf_sync

function nf_put_var_double() result(status)
  integer :: status
  status=0
end function nf_put_var_double

function nf_put_var_text() result(status)
  integer :: status
  status=0
end function nf_put_var_text

function nf_inq_dimid() result(status)
  integer :: status
  status=0
end function nf_inq_dimid

function nf_inq_varndims() result(status)
  integer :: status
  status=0
end function nf_inq_varndims

function nf_inq_vardimid() result(status)
  integer :: status
  status=0
end function nf_inq_vardimid

function nf_inq_dimlen() result(status)
  integer :: status
  status=0
end function nf_inq_dimlen

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Older, shorter netcdf function names:
!

function ncopn() result(status)
  integer :: status
  status=0
end function ncopn

function ncdid() result(status)
  integer :: status
  status=0
end function ncdid

function ncdinq() result(status)
  integer :: status
  status=0
end function ncdinq

function ncvid() result(status)
  integer :: status
  status=0
end function ncvid

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


