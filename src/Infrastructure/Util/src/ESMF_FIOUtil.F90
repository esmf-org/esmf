! $Id: ESMF_FIOUtil.F90,v 1.5 2008/10/29 04:42:06 w6ws Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2008, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
#define ESMF_FILENAME "ESMF_FIOUtil.F90"

!
! ESMF FIOUtil Module
!
! (all lines between the !BOP and !EOP markers will be included in the
! automated document processing.)
!------------------------------------------------------------------------------
! one blank line for protex processing - in case all routines here are
! marked internal (BOPI/EOPI), the output file will still have contents.
!BOP

!EOP

!------------------------------------------------------------------------------
! module definition

module ESMF_FIOUtilMod

!BOPI
! !MODULE: ESMF_FIOUtilMod - Fortran I/O utility routines
!
! !DESCRIPTION:
!
!  Fortran I/O-specific utility routines
!
! See the ESMF Developers Guide document for more details.
!
!------------------------------------------------------------------------------

! !USES:

  use ESMF_UtilTypesMod
#include "ESMF.h"

  implicit none
!
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!
! !DESCRIPTION:
!
! !PUBLIC MEMBER SUBROUTINES:
!
  public ESMF_FIOUnitFlush
  public ESMF_FIOUnitGet
  public ESMF_FIOUnitInit

!==============================================================================
!
! MODULE SCOPE DATA
!
!==============================================================================

! Standard I/O Units

! (F2003 TODO: Eventually, for compilers which support the F2003
! ISO_FORTRAN_ENV intrinsic module, these should access the constants
! 'input_unit', 'output_unit', and 'error_unit'.)

  integer, parameter, public :: ESMF_FIOstdin  = 5
  integer, parameter, public :: ESMF_FIOstdout = 6

#ifdef sysHP_UX

! Special setting for HP_UX

  integer, parameter, public :: ESMF_FIOstderr = 7
#else
! Generic setting for UNIX other than HP-UX

  integer, parameter, public :: ESMF_FIOstderr = 0
#endif

! Unit number range for ESMF internal use.

  integer, private :: ESMF_FIOUnitLower = ESMF_LOG_FORT_UNIT_NUMBER
  integer, private :: ESMF_FIOUnitUpper = ESMF_LOG_UPPER

!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
  character(*), parameter, private :: version = &
      '$Id: ESMF_FIOUtil.F90,v 1.5 2008/10/29 04:42:06 w6ws Exp $'
!------------------------------------------------------------------------------

  contains

!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FIOUnitFlush"
!BOPI
! !IROUTINE: ESMF\_FIOUnitFlush - flush output on a unit number
!
! !INTERFACE:
  subroutine ESMF_FIOUnitFlush(unit, rc)
!
! !PARAMETERS:
    integer, intent(in) :: unit
    integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
!   Call the system-dependent routine to force output.

!
! !REQUIREMENTS:

!EOPI
    integer :: status

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    status = ESMF_RC_NOT_IMPL

#if defined (ESMF_HAS_F2003_FLUSH)
    flush (unit, iostat=status)
#elif defined (ESMF_HAS_1ARG_FLUSH)
#if defined (PARCH_aix)
    call flush_ (unit)
#else
    call flush (unit)
#endif
    status = 0
#elif defined (ESMF_HAS_2ARG_FLUSH)
    call flush (unit, status)
#else

#if   defined(PARCH_linux)
    print *, "need to call flush() here"
#elif defined(PARCH_IRIX64)
    call flush(unit, status)
#elif defined(PARCH_aix)
    call flush_(unit, status)
#elif defined(PARCH_darwin)
    print *, "need to call flush() here"
#elif defined(PARCH_sunos)
    print *, "need to call flush() here"
#elif defined(PARCH_osf1)
    call flush(unit, status)
#else
    print *, "unknown architecture in ESMF_IOFlush()"
#endif

#endif

    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_FIOUnitFlush

!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FIOUnitGet"
!BOP
!  !IROUTINE:  ESMF\_FIOUnitGet - Return a free I/O unit number
!
! !INTERFACE:
  subroutine ESMF_FIOUnitGet (unit, rc)
!
! !ARGUMENTS:
    integer, intent(out) :: unit
    integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!   Return a free Fortran I/O unit number for use within ESMF.
!
!     The arguments are:
!     \begin{description}
!     \item[{[unit]}]
!       A Fortran I/O unit number.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!   The Fortran unit which is returned is not reserved in any way.
!   So successive calls without intervening OPEN or CLOSE statements (or
!   other ways of connecting to units), may not return a unique unit
!   number.  It is recommended that an OPEN statement immediately follow
!   the call to ESMF\_FIOUnitGet to activate the unit.
!EOP

    integer :: i, status
    logical :: inuse
  
    if (present(rc)) rc = ESMF_FAILURE
  
    do, i=ESMF_FIOUnitLower, ESMF_FIOUnitUpper
      inquire (unit=i, opened=inuse, iostat=status)
      if (.not. inuse .and. status == 0) exit
    end do
  
    if (i <= ESMF_FIOUnitUpper) then
      unit = i
      if (present (rc)) rc = ESMF_SUCCESS
    else
      unit = -1
    end if

  end subroutine ESMF_FIOUnitGet

!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FIOUnitInit"
!BOPI
!  !IROUTINE:  ESMF_FIOUnitInit - Initialize ESMF Fortran I/O unit number range
!
! !INTERFACE:
  subroutine ESMF_FIOUnitInit (lower, upper, rc)
!
! !ARGUMENTS:
    integer, intent(in), optional :: lower
    integer, intent(in), optional :: upper
    integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!   Initialize non-default range for Fortran I/O unit numbers used
!   within ESMF.
!
!     The arguments are:
!     \begin{description}
!     \item[{[lower]}]
!       A lower bound for Fortran unit numbers.
!     \item[{[upper]}]
!       A upper bound for Fortran unit numbers.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

    if (present(rc)) rc = ESMF_FAILURE

! Sanity checks

    if (present (lower)) then
      if (lower < 0) return
    end if

    if (present (upper)) then
      if (upper < 0) return
    end if

    if (present (lower) .and. .not. present (upper)) then
      if (lower > ESMF_FIOUnitUpper) return
    end if

    if (present (upper) .and. .not. present (lower)) then
      if (upper < ESMF_FIOUnitLower) return
    end if

    if (present (upper) .and. present (lower)) then
      if (upper < lower) return
    end if

!

    if (present (lower)) then
      ESMF_FIOUnitLower = lower
    end if

    if (present (upper)) then
      ESMF_FIOUnitUpper = upper
    end if

    if (present (rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_FIOUnitInit

!------------------------------------------------------------------------------

end module ESMF_FIOUtilMod
