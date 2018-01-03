! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

module ESMF_FeatureTR15581Subr_mod
    
#include "ESMF.h"

  use ESMF
  implicit none

  type ESMF_AllocDType
    ! F2003 and F95+TR15581 allocatable components
    real,         allocatable :: a(:)
    integer,      allocatable :: indicies(:)
    logical,      allocatable :: tfs(:)
    character(1), allocatable :: chars(:)
  end type

contains

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FeatureAllocArg"
  subroutine ESMF_FeatureAllocArg (n, a, indicies, tfs, dts, rc)
    integer,                   intent(in)  :: n
    ! F2003 and F95+TR15581 allocatable dummy arguments except for strings
    real,         allocatable, intent(out) :: a(:)
    integer,      allocatable, intent(out) :: indicies(:)
    logical,      allocatable, intent(out) :: tfs(:)
    type(ESMF_AllocDType), allocatable, intent(out) :: dts(:)
    integer,                   intent(out) :: rc

    integer :: i
    integer :: memstat

    allocate (a(n), indicies(n), tfs(n), dts(n), stat=memstat)
    if (ESMF_LogFoundAllocError (memstat,  &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    do, i=1, n
      a(i) = i
      indicies(i) = i
      tfs = mod (i, 1) == 1
      allocate (dts(i)%a(10), dts(i)%indicies(20), dts(i)%tfs(20), dts(i)%chars(32), stat=memstat)
      if (ESMF_LogFoundAllocError (memstat,  &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
      dts(i)%a = 0.0
      dts(i)%indicies = 0
      dts(i)%tfs = .false.
      dts(i)%chars = ' '
    end do

    rc = ESMF_SUCCESS

  end subroutine ESMF_FeatureAllocArg

#if !defined (ESMF_NO_F2003_ALLOC_STRING_LENS)
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FeatureAllocArgStr"
  subroutine ESMF_FeatureAllocArgStr (n, str_dllen, str_pilen, str_dllensize, str_pilensize, rc)
    integer,                   intent(in)  :: n
    ! F2003 and F95+TR15581 allocatable string dummy arguments
    character(:), allocatable, intent(out), optional :: str_dllen        ! Scalar w/deferred-length
    character(*), allocatable, intent(out), optional :: str_pilen        ! Scalar w/passed-in length
    character(:), allocatable, intent(out), optional :: str_dllensize(:) ! Array w/deferred-length
    character(*), allocatable, intent(out), optional :: str_pilensize(:) ! Array w/passed-in length
    integer,                   intent(out) :: rc

    integer :: i, j
    integer :: memstat

    if (present (str_dllen)) then
      allocate (character(n)::str_dllen, stat=memstat)
      if (ESMF_LogFoundAllocError (memstat,  &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return

      do, i=1, n
        str_dllen(i:i) = achar (mod (i, 127))
      end do
    end if

    if (present (str_pilen)) then
      allocate (str_pilen, stat=memstat)
      if (ESMF_LogFoundAllocError (memstat,  &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return

      do, i=1, len (str_pilen)
        str_pilen(i:i) = achar (mod (i, 127))
      end do
    end if

    if (present (str_pilensize)) then
      allocate (str_pilensize(n), stat=memstat)
      if (ESMF_LogFoundAllocError (memstat,  &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return

      do, i=1, n
        str_pilensize(i) = achar (mod (i, 127))
      end do
    end if

    if (present (str_dllensize)) then
      allocate (character(n)::str_dllensize(n), stat=memstat)
      if (ESMF_LogFoundAllocError (memstat,  &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return

      do, j=1, n
        do, i=1, len (str_dllensize)
          str_dllensize(j)(i:i) = achar (mod (i, 127))
        end do
      end do
    end if

    rc = ESMF_SUCCESS

  end subroutine ESMF_FeatureAllocArgStr
#endif

  function ESMF_FeatureAllocFRet (n, rc) result (ret)
    integer, intent(in)  :: n
    integer, intent(out) :: rc
    ! F2003 and F95+TR15581 allocatable function result
    real, allocatable :: ret(:)

    integer :: i
    integer :: memstat

    allocate (ret(n), stat=memstat)
    if (memstat /= 0) then
      rc = ESMF_RC_MEM_ALLOCATE
      return
    end if

    do, i=1, n
      ret(i) = i
    end do

    rc = ESMF_SUCCESS

  end function  ESMF_FeatureAllocFRet

end module ESMF_FeatureTR15581Subr_mod

