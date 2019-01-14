! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
#define ESMF_FILENAME "ESMF_IOUtil.F90"
!
! ESMF IOUtil Module
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

module ESMF_IOUtilMod

!BOPI
! !MODULE: ESMF_UtilIOUtilMod - Fortran I/O utility routines
!
! !DESCRIPTION:
!
!  Fortran I/O-specific utility routines
!
! See the ESMF Developers Guide document for more details.
!
!------------------------------------------------------------------------------

! !USES:

! can not use ESMF_LogErrMod because it would cause a module circularity
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
  public ESMF_UtilIOUnitFlush
  public ESMF_UtilIOUnitGet
  public ESMF_UtilIOUnitInit

!==============================================================================
!
! MODULE SCOPE DATA
!
!==============================================================================

! Standard I/O Units

! (F2003 TODO: Eventually, for compilers which support the F2003
! ISO_FORTRAN_ENV intrinsic module, these should access the constants
! 'input_unit', 'output_unit', and 'error_unit'.)

  integer, parameter, public :: ESMF_UtilIOStdin  = 5
  integer, parameter, public :: ESMF_UtilIOStdout = 6

#ifdef sysHP_UX

! Special setting for HP_UX

  integer, parameter, public :: ESMF_UtilIOStderr = 7
#else
! Generic setting for UNIX other than HP-UX

  integer, parameter, public :: ESMF_UtilIOStderr = 0
#endif

! Unit number range for ESMF internal use.

  integer, private :: ESMF_UtilIOUnitLower = ESMF_LOG_FORT_UNIT_NUMBER
  integer, private :: ESMF_UtilIOUnitUpper = ESMF_LOG_UPPER

!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
  character(*), parameter, private :: version = &
      '$Id$'
!------------------------------------------------------------------------------

  contains

!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_UtilIOUnitFlush"
!BOP
! !IROUTINE: ESMF_UtilIOUnitFlush - Flush output on a unit number
!
! !INTERFACE:
  subroutine ESMF_UtilIOUnitFlush(unit, keywordEnforcer, rc)
!
! !PARAMETERS:
    integer, intent(in)            :: unit
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer, intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Call the system-dependent routine to force output on a specific
!   Fortran unit number.
!
!     The arguments are:
!     \begin{description}
!     \item[unit]
!       A Fortran I/O unit number.  If the unit is not connected to a file,
!       no flushing occurs.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOP
    integer :: localrc
    integer :: localstat
    logical :: connected


    inquire (unit=unit, opened=connected)
    if (.not. connected) then
      if (present (rc)) then
        rc = ESMF_SUCCESS
      end if
      return
    end if


!   By default, use the F2003 FLUSH statement.  For older compilers,
!   use a macro defined in the configuration-specific ESMF_Conf.inc
!   include file.  This is needed because the name of the flush routine
!   and exact number of its arguements vary between implementations.

#if !defined (ESMF_IOFlushMacro)

    flush (unit, iostat=localstat)

    ! Convert Fortran iostat to ESMF rc

    localrc = merge (ESMF_SUCCESS, ESMF_FAILURE, localstat == 0)

#else
!   Preset localrc in advance, since some library versions of FLUSH do
!   not support a status argument for detecting errors.

    localstat = 0

ESMF_IOFlushMacro(unit, localstat)

    ! Convert status return to ESMF rc

    localrc = merge (ESMF_SUCCESS, ESMF_FAILURE, localstat == 0)

#endif

    if (present(rc)) then
      rc = localrc
    end if

  end subroutine ESMF_UtilIOUnitFlush
!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_UtilIOUnitGet"
!BOP
! !IROUTINE:  ESMF_UtilIOUnitGet - Scan for a free I/O unit number
!
! !INTERFACE:
  subroutine ESMF_UtilIOUnitGet(unit, keywordEnforcer, rc)
!
! !ARGUMENTS:
    integer, intent(out)           :: unit
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer, intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Scan for, and return, a free Fortran I/O unit number.
!   By default, the range of unit numbers returned is between 50 and 99
!   (parameters {\tt ESMF\_LOG\_FORTRAN\_UNIT\_NUMBER} and {\tt ESMF\_LOG\_UPPER}
!   respectively.) When integrating ESMF into an application where these values
!   conflict with other usages, the range of values may be moved by setting the
!   optional {\tt IOUnitLower} and {\tt IOUnitUpper} arguments in the initial
!   {\tt ESMF\_Initialize()} call with values in a safe, alternate, range.
!
!   The Fortran unit number which is returned is not reserved in any way.
!   Successive calls without intervening {\tt OPEN} or {\tt CLOSE} statements
!   (or other means of connecting to units), might not return a unique unit
!   number.  It is recommended that an {\tt OPEN} statement immediately follow
!   the call to {\tt ESMF\_IOUnitGet()} to activate the unit.
!
!     The arguments are:
!     \begin{description}
!     \item[unit]
!       A Fortran I/O unit number.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOP

    integer :: i
    integer :: localstat
    logical :: inuse
  
    if (present(rc)) rc = ESMF_FAILURE
  
    do, i=ESMF_UtilIOUnitLower, ESMF_UtilIOUnitUpper
      inquire (unit=i, opened=inuse, iostat=localstat)
      if (.not. inuse .and. localstat == 0) exit
    end do
  
    if (i <= ESMF_UtilIOUnitUpper) then
      unit = i
      if (present (rc)) rc = ESMF_SUCCESS
    else
      unit = -1
    end if

  end subroutine ESMF_UtilIOUnitGet

!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_UtilIOUnitInit"
!BOPI
!  !IROUTINE:  ESMF_UtilIOUnitInit - Initialize ESMF Fortran I/O unit number range
!
! !INTERFACE:
  subroutine ESMF_UtilIOUnitInit(lower, upper, rc)
!
! !ARGUMENTS:
    integer, intent(in),  optional :: lower
    integer, intent(in),  optional :: upper
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
      if (lower > ESMF_UtilIOUnitUpper) return
    end if

    if (present (upper) .and. .not. present (lower)) then
      if (upper < ESMF_UtilIOUnitLower) return
    end if

    if (present (upper) .and. present (lower)) then
      if (upper < lower) return
    end if

!

    if (present (lower)) then
      ESMF_UtilIOUnitLower = lower
    end if

    if (present (upper)) then
      ESMF_UtilIOUnitUpper = upper
    end if

    if (present (rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_UtilIOUnitInit

!------------------------------------------------------------------------------

end module ESMF_IOUtilMod
