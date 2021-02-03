! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
!     ESMF LogErr Module
module ESMF_LogPublicMod

#include "ESMF_ErrReturnCodes.inc"

  implicit none

  public  ! everything in this file is public

  character(*), parameter:: ESMF_LOGERR_PASSTHRU = "Passing error in return code"

  ! The list if RC constants must be kept in sync with the macros defined
  ! in file ESMF_ErrReturnCodes.inc
  integer, parameter:: esmf_rc_OBJ_BAD          = ESMF_RC_OBJ_BAD
  integer, parameter:: esmf_rc_OBJ_INIT         = ESMF_RC_OBJ_INIT
  integer, parameter:: esmf_rc_OBJ_CREATE       = ESMF_RC_OBJ_CREATE
  integer, parameter:: esmf_rc_OBJ_COR          = ESMF_RC_OBJ_COR
  integer, parameter:: esmf_rc_OBJ_WRONG        = ESMF_RC_OBJ_WRONG
  integer, parameter:: esmf_rc_ARG_BAD          = ESMF_RC_ARG_BAD
  integer, parameter:: esmf_rc_ARG_RANK         = ESMF_RC_ARG_RANK
  integer, parameter:: esmf_rc_ARG_SIZE         = ESMF_RC_ARG_SIZE
  integer, parameter:: esmf_rc_ARG_VALUE        = ESMF_RC_ARG_VALUE
  integer, parameter:: esmf_rc_ARG_DUP          = ESMF_RC_ARG_DUP
  integer, parameter:: esmf_rc_ARG_SAMETYPE     = ESMF_RC_ARG_SAMETYPE
  integer, parameter:: esmf_rc_ARG_SAMECOMM     = ESMF_RC_ARG_SAMECOMM
  integer, parameter:: esmf_rc_ARG_INCOMP       = ESMF_RC_ARG_INCOMP
  integer, parameter:: esmf_rc_ARG_CORRUPT      = ESMF_RC_ARG_CORRUPT
  integer, parameter:: esmf_rc_ARG_WRONG        = ESMF_RC_ARG_WRONG
  integer, parameter:: esmf_rc_ARG_OUTOFRANGE   = ESMF_RC_ARG_OUTOFRANGE
  integer, parameter:: esmf_rc_ARG_OPT          = ESMF_RC_ARG_OPT
  integer, parameter:: esmf_rc_NOT_IMPL         = ESMF_RC_NOT_IMPL
  integer, parameter:: esmf_rc_FILE_OPEN        = ESMF_RC_FILE_OPEN
  integer, parameter:: esmf_rc_FILE_CREATE      = ESMF_RC_FILE_CREATE
  integer, parameter:: esmf_rc_FILE_READ        = ESMF_RC_FILE_READ
  integer, parameter:: esmf_rc_FILE_WRITE       = ESMF_RC_FILE_WRITE
  integer, parameter:: esmf_rc_FILE_UNEXPECTED  = ESMF_RC_FILE_UNEXPECTED
  integer, parameter:: esmf_rc_FILE_CLOSE       = ESMF_RC_FILE_CLOSE
  integer, parameter:: esmf_rc_FILE_ACTIVE      = ESMF_RC_FILE_ACTIVE
  integer, parameter:: esmf_rc_PTR_NULL         = ESMF_RC_PTR_NULL
  integer, parameter:: esmf_rc_PTR_BAD          = ESMF_RC_PTR_BAD
  integer, parameter:: esmf_rc_PTR_NOTALLOC     = ESMF_RC_PTR_NOTALLOC
  integer, parameter:: esmf_rc_PTR_ISALLOC      = ESMF_RC_PTR_ISALLOC
  integer, parameter:: esmf_rc_MEM              = ESMF_RC_MEM
  integer, parameter:: esmf_rc_MEM_ALLOCATE     = ESMF_RC_MEM_ALLOCATE
  integer, parameter:: esmf_rc_MEM_DEALLOCATE   = ESMF_RC_MEM_DEALLOCATE
  integer, parameter:: esmf_rc_MEMC             = ESMF_RC_MEMC
  integer, parameter:: esmf_rc_DUP_NAME         = ESMF_RC_DUP_NAME
  integer, parameter:: esmf_rc_LONG_NAME        = ESMF_RC_LONG_NAME
  integer, parameter:: esmf_rc_LONG_STR         = ESMF_RC_LONG_STR
  integer, parameter:: esmf_rc_COPY_FAIL        = ESMF_RC_COPY_FAIL
  integer, parameter:: esmf_rc_DIV_ZERO         = ESMF_RC_DIV_ZERO
  integer, parameter:: esmf_rc_CANNOT_GET       = ESMF_RC_CANNOT_GET
  integer, parameter:: esmf_rc_CANNOT_SET       = ESMF_RC_CANNOT_SET
  integer, parameter:: esmf_rc_NOT_FOUND        = ESMF_RC_NOT_FOUND
  integer, parameter:: esmf_rc_NOT_VALID        = ESMF_RC_NOT_VALID
  integer, parameter:: esmf_rc_INTNRL_LIST      = ESMF_RC_INTNRL_LIST
  integer, parameter:: esmf_rc_INTNRL_INCONS    = ESMF_RC_INTNRL_INCONS
  integer, parameter:: esmf_rc_INTNRL_BAD       = ESMF_RC_INTNRL_BAD
  integer, parameter:: esmf_rc_SYS              = ESMF_RC_SYS
  integer, parameter:: esmf_rc_BUSY             = ESMF_RC_BUSY
  integer, parameter:: esmf_rc_LIB              = ESMF_RC_LIB
  integer, parameter:: esmf_rc_LIB_NOT_PRESENT  = ESMF_RC_LIB_NOT_PRESENT
  integer, parameter:: esmf_rc_ATTR_UNUSED      = ESMF_RC_ATTR_UNUSED
  integer, parameter:: esmf_rc_OBJ_NOT_CREATED  = ESMF_RC_OBJ_NOT_CREATED
  integer, parameter:: esmf_rc_OBJ_DELETED      = ESMF_RC_OBJ_DELETED
  integer, parameter:: esmf_rc_NOT_SET          = ESMF_RC_NOT_SET
  integer, parameter:: esmf_rc_VAL_WRONG        = ESMF_RC_VAL_WRONG
  integer, parameter:: esmf_rc_VAL_ERRBOUND     = ESMF_RC_VAL_ERRBOUND
  integer, parameter:: esmf_rc_VAL_OUTOFRANGE   = ESMF_RC_VAL_OUTOFRANGE
  integer, parameter:: esmf_rc_ATTR_NOTSET      = ESMF_RC_ATTR_NOTSET
  integer, parameter:: esmf_rc_ATTR_WRONGTYPE   = ESMF_RC_ATTR_WRONGTYPE
  integer, parameter:: esmf_rc_ATTR_ITEMSOFF    = ESMF_RC_ATTR_ITEMSOFF
  integer, parameter:: esmf_rc_ATTR_LINK        = ESMF_RC_ATTR_LINK
  integer, parameter:: esmf_rc_BUFFER_SHORT     = ESMF_RC_BUFFER_SHORT
  integer, parameter:: esmf_rc_TIMEOUT          = ESMF_RC_TIMEOUT
  integer, parameter:: esmf_rc_NETCDF_ERROR     = ESMF_RC_NETCDF_ERROR

end module ESMF_LogPublicMod
