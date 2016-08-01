! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

module ESMF_FeatureTR15581Subr_mod
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

  subroutine ESMF_FeatureAllocArg (n, a, indicies, tfs, strings, dts, rc)
    integer,                   intent(in)  :: n
    ! F2003 and F95+TR15581 allocatable dummy arguments
    real,         allocatable, intent(out) :: a(:)
    integer,      allocatable, intent(out) :: indicies(:)
    logical,      allocatable, intent(out) :: tfs(:)
#if defined (ALLOC_STRING_TEST)
    character(*), allocatable, intent(out) :: strings(:)
#else
    ! TODO: Absoft doesn't support character(*) allocatable dummy args in v11.1.
    character(*),              intent(out) :: strings(:)
#endif
    type(ESMF_AllocDType), allocatable, intent(out) :: dts(:)
    integer,                   intent(out) :: rc

    integer :: i
    integer :: memstat

    allocate (a(n), indicies(n), tfs(n), dts(n), stat=memstat)
    if (memstat /= 0) then
      rc = ESMF_RC_MEM_ALLOCATE
      return
    end if

#if defined (ALLOC_STRING_TEST)
    allocate (strings(n), stat=memstat)
    if (memstat /= 0) then
      rc = ESMF_RC_MEM_ALLOCATE
      return
    end if
#endif

    do, i=1, n
      a(i) = i
      indicies(i) = i
      tfs = mod (i, 1) == 1
#if defined (ALLOC_STRING_TEST)
      strings(i) = achar (mod (i, 127))
#endif
    end do

    rc = ESMF_SUCCESS

  end subroutine ESMF_FeatureAllocArg

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

