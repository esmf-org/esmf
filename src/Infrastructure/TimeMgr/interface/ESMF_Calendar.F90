! $Id: ESMF_Calendar.F90,v 1.44 2003/12/23 00:33:52 eschwab Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
!     ESMF Calendar Module
      module ESMF_CalendarMod
!
!==============================================================================
!
! This file contains the Calendar class definition and all Calendar class
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF_TimeMgr.inc>

!==============================================================================
!BOPI
! !MODULE: ESMF_CalendarMod
!
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation.
!
! Defines F90 wrapper entry points for corresponding
! C++ class { \tt ESMC\_Calendar} implementation.
!
! See {\tt ../include/ESMC\_Calendar.h} for complete description.
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from ESMF base class
      use ESMF_BaseMod

      ! inherit from base time class
      use ESMF_BaseTimeMod

      ! for ReadRestart()/WriteRestart()
      use ESMF_IOMod

      implicit none
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------

      integer, parameter :: MONTHS_PER_YEAR = 12

!------------------------------------------------------------------------------
!     ! ESMF_CalendarType
!
!     ! F90 "enum" type to match C++ ESMC_CalendarType enum

      type ESMF_CalendarType
      sequence
      private
        integer :: calendarType
      end type

      type(ESMF_CalendarType), parameter :: &
                               ESMF_CAL_GREGORIAN =  ESMF_CalendarType(1), &
                               ESMF_CAL_JULIANDAY =  ESMF_CalendarType(2), &
                           ! like Gregorian, except Feb always has 28 days
                               ESMF_CAL_NOLEAP =     ESMF_CalendarType(3), & 
                           ! 12 months, 30 days each
                               ESMF_CAL_360DAY =     ESMF_CalendarType(4), & 
                           ! user defined
                               ESMF_CAL_CUSTOM =     ESMF_CalendarType(5), &
                           ! track base time seconds only
                               ESMF_CAL_NOCALENDAR = ESMF_CalendarType(6)

!------------------------------------------------------------------------------
!     ! ESMF_Calendar
!
!     ! F90 class type to match C++ Calendar class in size only;
!     !  all dereferencing within class is performed by C++ implementation
!
!     ! Equivalent sequence and kind to C++:
!------------------------------------------------------------------------------
!
!     ! ESMF_DaysPerYear
!
      type ESMF_DaysPerYear
      sequence
      private
        integer(ESMF_KIND_I4) :: days  ! whole days per year
        integer(ESMF_KIND_I4) :: daysN ! fractional days per year numerator
        integer(ESMF_KIND_I4) :: daysD ! fractional days per year denominator
                                       ! e.g. for Venus,
                                       !   days=0, daysN=926, daysD=1000
        integer               :: pad1  ! to match C++ alignment
      end type
!
!------------------------------------------------------------------------------
!     ! ESMF_Calendar
!
!     ! F90 class to match C++ Calendar class in size and sequence
!
      type ESMF_Calendar
      sequence
      private
        type(ESMF_CalendarType)             :: type
        integer, dimension(MONTHS_PER_YEAR) :: daysPerMonth
        integer(ESMF_KIND_I4)               :: secondsPerDay
        integer(ESMF_KIND_I4)               :: secondsPerYear
        type(ESMF_DaysPerYear)              :: daysPerYear
      end type
!
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public MONTHS_PER_YEAR
      public ESMF_CalendarType
      public ESMF_CAL_GREGORIAN, ESMF_CAL_JULIANDAY,  ESMF_CAL_NOLEAP, &
             ESMF_CAL_360DAY,    ESMF_CAL_CUSTOM,     ESMF_CAL_NOCALENDAR
      public ESMF_Calendar
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
      public ESMF_CalendarSet
      public ESMF_CalendarSetCustom
      public ESMF_CalendarGet

! Required inherited and overridden ESMF_Base class methods

      public ESMF_CalendarReadRestart
      public ESMF_CalendarWriteRestart
      public ESMF_CalendarValidate
      public ESMF_CalendarPrint
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Calendar.F90,v 1.44 2003/12/23 00:33:52 eschwab Exp $'

!==============================================================================

      contains

!==============================================================================
!
! This section includes the Set methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CalendarSet - Initialize or set the Calendar type

! !INTERFACE:
      subroutine ESMF_CalendarSet(calendar, type, rc)

! !ARGUMENTS:
      type(ESMF_Calendar),     intent(inout)         :: calendar
      type(ESMF_CalendarType), intent(in)            :: type
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Initializes or sets {\tt calendar} to the given {\tt ESMF\_CalendarType}. 
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          The object instance to initialize.
!     \item[type]
!          The {\tt CalendarType}.  Valid values are:
!            {\tt ESMF\_CAL\_GREGORIAN}, {\tt ESMF\_CAL\_JULIAN}, 
!            {\tt ESMF\_CAL\_JULIANDAY}, {\tt ESMF\_CAL\_NOLEAP},
!            {\tt ESMF\_CAL\_360DAY}, {\tt ESMF\_CAL\_CUSTOM}, and
!            {\tt ESMF\_CAL\_NOCALENDAR}.
!          See the "Time Manager Reference" document for a description of
!          each calendar type.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!    
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
    
!     invoke C to C++ entry point
      call c_ESMC_CalendarSet(calendar, type, rc)
    
      end subroutine ESMF_CalendarSet
    
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CalendarSetCustom - Initialize or set a custom Calendar

! !INTERFACE:
      subroutine ESMF_CalendarSetCustom(calendar, daysPerMonth, &
                                        secondsPerDay, &
                                        daysPerYear, daysPerYearDn, &
                                        daysPerYearDd, rc)
! !ARGUMENTS:
      type(ESMF_Calendar),   intent(inout)         :: calendar
      integer, dimension(:), intent(in),  optional :: daysPerMonth
      integer(ESMF_KIND_I4), intent(in),  optional :: secondsPerDay
      integer(ESMF_KIND_I4), intent(in),  optional :: daysPerYear
      integer(ESMF_KIND_I4), intent(in),  optional :: daysPerYearDn
      integer(ESMF_KIND_I4), intent(in),  optional :: daysPerYearDd
      integer,               intent(out), optional :: rc

! !DESCRIPTION:
!     Sets properties in a custom {\tt ESMF\_Calendar}.
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          The object instance to initialize.
!     \item[{[daysPerMonth]}]
!          Integer array of days per month, for each month of the year.
!          The number of months per year is variable and taken from the
!          size of the array.  If unspecified, months per year = 0,
!          with the days array undefined.
!     \item[{[secondsPerDay]}]
!          Integer number of seconds per day.  Defaults to 86400 if not 
!          specified.
!     \item[{[daysPerYear]}]
!          Integer number of days per year.  Use with daysPerYearDn and
!          daysPerYearDd (see below) to specify a days-per-year calendar
!          for any planetary body.  Default = 0
!     \item[{[daysPerYearDn]}]
!          Integer numerator portion of fractional number of days per year
!          (daysPerYearDn/daysPerYearDd).
!          Use with daysPerYear (see above) and daysPerYearDd (see below) to
!          specify a days-per-year calendar for any planetary body.
!          Default = 0
!     \item[{[daysPerYearDd]}]
!          Integer denominator portion of fractional number of days per year
!          (daysPerYearDn/daysPerYearDd).
!          Use with daysPerYear and daysPerYearDn (see above) to
!          specify a days-per-year calendar for any planetary body.
!          Default = 1
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!EOP
! !REQUIREMENTS:
!     TMG2.3.4

      ! initialize number of months per year to zero for not-present
      !   daysPerMonth
      integer :: monthsPerYear = 0

      ! get size of given daysPerMonth array for C++ validation
      if (present(daysPerMonth)) then
        monthsPerYear = size(daysPerMonth)
      end if

!     invoke C to C++ entry point
      call c_ESMC_CalendarSetCustom(calendar, monthsPerYear, daysPerMonth, &
                                    secondsPerDay, daysPerYear, &
                                    daysPerYearDn, daysPerYearDd, rc)
    
      end subroutine ESMF_CalendarSetCustom
    
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CalendarGet - Get Calendar properties

! !INTERFACE:
      subroutine ESMF_CalendarGet(calendar, type, monthsPerYear, &
                                  daysPerMonth, secondsPerDay, secondsPerYear, &
                                  daysPerYear, daysPerYearDn, daysPerYearDd, rc)
! !ARGUMENTS:
      type(ESMF_Calendar),     intent(in)            :: calendar
      type(ESMF_CalendarType), intent(out), optional :: type
      integer,                 intent(out), optional :: monthsPerYear
      integer, dimension(:),   intent(out), optional :: daysPerMonth
      integer(ESMF_KIND_I4),   intent(out), optional :: secondsPerDay
      integer(ESMF_KIND_I4),   intent(out), optional :: secondsPerYear
      integer(ESMF_KIND_I4),   intent(out), optional :: daysPerYear
      integer(ESMF_KIND_I4),   intent(out), optional :: daysPerYearDn
      integer(ESMF_KIND_I4),   intent(out), optional :: daysPerYearDd
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Gets one or more of an {\tt ESMF\_Calendar}'s properties.
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          The object instance to query.
!     \item[{[type]}]
!          The {\tt CalendarType} ESMF\_CAL\_GREGORIAN, ESMF\_CAL\_JULIANDAY,
!          etc.
!     \item[{[monthsPerYear]}]
!          Integer number of months per year.
!     \item[{[daysPerMonth]}]
!          Integer array of days per month, for each month of the year.
!     \item[{[secondsPerDay]}]
!          Integer number of seconds per day.
!     \item[{[secondsPerYear]}]
!          Integer number of seconds per year.
!     \item[{[daysPerYear]}]
!          Integer number of days per year.  For calendars with
!          intercalations, daysPerYear is the number of days for years without
!          an intercalation.  For other calendars, it is the number of days in
!          every year.
!     \item[{[daysPerYearDn]}]
!          Integer fractional number of days per year (numerator).
!          For calendars with intercalations, daysPerYearDn/daysPerYearDd is
!          the average fractional number of days per year (e.g. 25/100 for
!          Julian 4-year intercalation).  For other calendars, it is zero.
!     \item[{[daysPerYearDd]}]
!          Integer fractional number of days per year (denominator).  See
!          daysPerYearDn above.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!EOP
! !REQUIREMENTS:
!     TMGn.n.n

!     invoke C to C++ entry point
      call c_ESMC_CalendarGet(calendar, type, monthsPerYear, &
                              daysPerMonth, secondsPerDay, secondsPerYear, &
                              daysPerYear, daysPerYearDn, daysPerYearDd, rc)
    
      end subroutine ESMF_CalendarGet
    
!------------------------------------------------------------------------------
! 
! This section defines the overridden Read, Write, Validate and Print methods
! from the ESMF_Base class
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_CalendarReadRestart - Restore the contents of a Calendar

! !INTERFACE:
      subroutine ESMF_CalendarReadRestart(calendar, name, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_Calendar), intent(in)            :: calendar
      character (len=*),   intent(in)            :: name
      type(ESMF_IOSpec),   intent(in),  optional :: iospec
      integer,             intent(out), optional :: rc

! !DESCRIPTION:
!     Restores an {\tt ESMF\_Calendar} object from the last call to
!     {\tt ESMF\_CalendarWriteRestart()}.
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          Restore into this {\tt ESMF\_Calendar}.
!     \item[name]
!          Restore from this object name.
!     \item[{[iospec]}]
!          The IO specification of the restart file.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n

      ! get length of given name for C++ validation
      integer :: nameLen
      nameLen = len_trim(name)

!     invoke C to C++ entry point to restore calendar
      call c_ESMC_CalendarReadRestart(calendar, nameLen, name, iospec, rc)

      end subroutine ESMF_CalendarReadRestart

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_CalendarWriteRestart - Save the contents of a Calendar

! !INTERFACE:
      subroutine ESMF_CalendarWriteRestart(calendar, iospec, rc)

! !ARGUMENTS:
      type(ESMF_Calendar), intent(in)            :: calendar
      type(ESMF_IOSpec),   intent(in),  optional :: iospec
      integer,             intent(out), optional :: rc

! !DESCRIPTION:  
!     Saves an {\tt ESMF\_Calendar} object.  Default options are to select the
!     fastest way to save to disk.
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          The object instance to save.  
!     \item[{[iospec]}]
!          The IO specification of the restart file.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n

!     invoke C to C++ entry point 
      call c_ESMC_CalendarWriteRestart(calendar, iospec, rc)

      end subroutine ESMF_CalendarWriteRestart

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_CalendarValidate - Validate a Calendar's properties

! !INTERFACE:
      subroutine ESMF_CalendarValidate(calendar, options, rc)
 
! !ARGUMENTS:
      type(ESMF_Calendar), intent(in)            :: calendar
      character (len=*),   intent(in),  optional :: options
      integer,             intent(out), optional :: rc

! !DESCRIPTION:
!     Check whether a {\tt calendar} is valid.  The options control
!     the type of validation.
! 
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          {\tt ESMF\_Calendar} to be validated.
!     \item[{[options]}]
!          Validation options.  TODO:  To be determined.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
      
!     invoke C to C++ entry point
      call c_ESMC_CalendarValidate(calendar, options, rc)

      end subroutine ESMF_CalendarValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_CalendarPrint - Print the contents of a Calendar

! !INTERFACE:
      subroutine ESMF_CalendarPrint(calendar, options, rc)

! !ARGUMENTS:
      type(ESMF_Calendar), intent(in)            :: calendar
      character (len=*),   intent(in),  optional :: options
      integer,             intent(out), optional :: rc

! !DESCRIPTION:
!     Prints out an {\tt ESMF\_Calendar}'s properties, in support of testing    !     and debugging.  The options control the type of information and level     
!     of detail. 
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          {\tt ESMF\_Calendar} to be printed out.
!     \item[{[options]}]
!          Print options. If none specified, prints all calendar property
!                             values. \\
!          "type"           - print the calendar's type 
!                               (e.g. ESMF\_CAL\_GREGORIAN). \\
!          "daysPerMonth"   - print the array of number of days for
!                               each month. \\
!          "secondsPerDay"  - print the number of seconds in a day. \\
!          "secondsPerYear" - print the number of seconds in a year. \\
!          "daysPerYear"    - print the number of days per year. \\
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
  
!     invoke C to C++ entry point
      call c_ESMC_CalendarPrint(calendar, options, rc)

      end subroutine ESMF_CalendarPrint
      
!------------------------------------------------------------------------------

      end module ESMF_CalendarMod
