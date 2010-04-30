! $Id: ESMF_IOUtil.F90,v 1.7.2.2 2010/04/30 21:47:01 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research,
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
! !MODULE: ESMF_IOUtilMod - Fortran I/O utility routines
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
#ifdef ESMF_NAG_UNIXIO_MODULE
  use f90_unix_io
#endif

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
  public ESMF_IOUnitFlush
  public ESMF_IOUnitGet
  public ESMF_IOUnitInit

!==============================================================================
!
! MODULE SCOPE DATA
!
!==============================================================================

! Standard I/O Units

! (F2003 TODO: Eventually, for compilers which support the F2003
! ISO_FORTRAN_ENV intrinsic module, these should access the constants
! 'input_unit', 'output_unit', and 'error_unit'.)

  integer, parameter, public :: ESMF_IOstdin  = 5
  integer, parameter, public :: ESMF_IOstdout = 6

#ifdef sysHP_UX

! Special setting for HP_UX

  integer, parameter, public :: ESMF_IOstderr = 7
#else
! Generic setting for UNIX other than HP-UX

  integer, parameter, public :: ESMF_IOstderr = 0
#endif

! Unit number range for ESMF internal use.

  integer, private :: ESMF_IOUnitLower = ESMF_LOG_FORT_UNIT_NUMBER
  integer, private :: ESMF_IOUnitUpper = ESMF_LOG_UPPER

!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
  character(*), parameter, private :: version = &
      '$Id: ESMF_IOUtil.F90,v 1.7.2.2 2010/04/30 21:47:01 theurich Exp $'
!------------------------------------------------------------------------------

  contains

!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IOUnitFlush"
!BOP
! !IROUTINE: ESMF_IOUnitFlush - flush output on a unit number
!
! !INTERFACE:
  subroutine ESMF_IOUnitFlush (unit, rc)
!
! !PARAMETERS:
    integer, intent(in) :: unit
    integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!   Call the system-dependent routine to force output on a specific
!   Fortran unit number.
!
!     The arguments are:
!     \begin{description}
!     \item[{[unit]}]
!       A Fortran I/O unit number.
!     \item[{[rc]}]
!       Return code; Returns either {\tt ESMF\_SUCCESS} or {\tt ESMF\_FAILURE}
!     \end{description}
!EOP
    integer :: localrc

!   By default, use the F2003 FLUSH statement.  For older compilers,
!   use a macro defined in the configuration-specific ESMF_Conf.inc
!   include file.  This is needed because the name of the flush routine
!   and exact number of its arguements vary between implementations.

#if !defined (ESMF_IOFlushMacro)

    flush (unit, iostat=localrc)

#else
!   Preset localrc in advance, since some library versions of FLUSH do
!   not support a status argument for detecting errors.

    localrc = 0

ESMF_IOFlushMacro(unit, localrc)

#endif

    if (present(rc)) then
      rc = merge (ESMF_SUCCESS, ESMF_FAILURE, localrc == 0)
    end if

  end subroutine ESMF_IOUnitFlush
!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IOUnitGet"
!BOP
! !IROUTINE:  ESMF_IOUnitGet - Scan for a free I/O unit number
!
! !INTERFACE:
  subroutine ESMF_IOUnitGet (unit, rc)
!
! !ARGUMENTS:
    integer, intent(out) :: unit
    integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!   Scan for, and return, a free Fortran I/O unit number.
!
!     The arguments are:
!     \begin{description}
!     \item[{[unit]}]
!       A Fortran I/O unit number.
!     \item[{[rc]}]
!       Return code; Returns either {\tt ESMF\_SUCCESS} or {\tt ESMF\_FAILURE}.
!     \end{description}
!
!
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
!EOP

    integer :: i, localrc
    logical :: inuse
  
    if (present(rc)) rc = ESMF_FAILURE
  
    do, i=ESMF_IOUnitLower, ESMF_IOUnitUpper
      inquire (unit=i, opened=inuse, iostat=localrc)
      if (.not. inuse .and. localrc == 0) exit
    end do
  
    if (i <= ESMF_IOUnitUpper) then
      unit = i
      if (present (rc)) rc = ESMF_SUCCESS
    else
      unit = -1
    end if

  end subroutine ESMF_IOUnitGet

!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IOUnitInit"
!BOPI
!  !IROUTINE:  ESMF_IOUnitInit - Initialize ESMF Fortran I/O unit number range
!
! !INTERFACE:
  subroutine ESMF_IOUnitInit (lower, upper, rc)
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
      if (lower > ESMF_IOUnitUpper) return
    end if

    if (present (upper) .and. .not. present (lower)) then
      if (upper < ESMF_IOUnitLower) return
    end if

    if (present (upper) .and. present (lower)) then
      if (upper < lower) return
    end if

!

    if (present (lower)) then
      ESMF_IOUnitLower = lower
    end if

    if (present (upper)) then
      ESMF_IOUnitUpper = upper
    end if

    if (present (rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_IOUnitInit

!------------------------------------------------------------------------------

end module ESMF_IOUtilMod
