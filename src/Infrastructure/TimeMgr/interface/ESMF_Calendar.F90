! $Id: ESMF_Calendar.F90,v 1.24 2003/07/25 23:31:26 eschwab Exp $
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
                               ESMF_CAL_JULIAN =     ESMF_CalendarType(2), &
                           ! like Gregorian, except Feb always has 28 days
                               ESMF_CAL_NOLEAP =     ESMF_CalendarType(3), & 
                           ! 12 months, 30 days each
                               ESMF_CAL_360DAY =     ESMF_CalendarType(4), & 
                           ! user defined
                               ESMF_CAL_GENERIC =    ESMF_CalendarType(5), &
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
        integer :: days  ! whole days per year
        integer :: daysN ! fractional days per year numerator
        integer :: daysD ! fractional days per year denominator
      end type           ! e.g. for Venus, days=0, daysN=926, daysD=1000
!
!------------------------------------------------------------------------------
!     ! ESMF_Calendar
!
!     ! F90 class to match C++ Calendar class in size and sequence
!
      type ESMF_Calendar
      sequence
      private
        type(ESMF_CalendarType) :: type
        integer, dimension(MONTHS_PER_YEAR) :: daysPerMonth
        integer :: secondsPerDay
        integer :: secondsPerYear
        type(ESMF_DaysPerYear) :: daysPerYear
      end type
!
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public MONTHS_PER_YEAR
      public ESMF_CalendarType
      public ESMF_CAL_GREGORIAN, ESMF_CAL_JULIAN, ESMF_CAL_NOLEAP, &
             ESMF_CAL_360DAY, ESMF_CAL_GENERIC, ESMF_CAL_NOCALENDAR
      public ESMF_Calendar
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
      public ESMF_CalendarSet
      public ESMF_CalendarGet
      public ESMF_CalendarSetGeneric
      public ESMF_CalendarGetGeneric

! Required inherited and overridden ESMF_Base class methods

      public ESMF_CalendarReadRestart
      public ESMF_CalendarWriteRestart
      public ESMF_CalendarValidate
      public ESMF_CalendarPrint
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Calendar.F90,v 1.24 2003/07/25 23:31:26 eschwab Exp $'

!==============================================================================

      contains

!==============================================================================
!
! This section includes the Set methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CalendarSet - Initializes the calendar type

! !INTERFACE:
      subroutine ESMF_CalendarSet(calendar, type, rc)

! !ARGUMENTS:
      type(ESMF_Calendar), intent(out) :: calendar
      type(ESMF_CalendarType), intent(in) :: type
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Initializes a {\tt ESMF\_Calendar} to the given {\tt ESMF\_Calendar} type.
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          The object instance to initialize.
!     \item[type]
!          The {\tt CalendarType} ESMF\_CAL\_GREGORIAN, ESMF\_CAL\_JULIAN, etc.
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
! !IROUTINE: ESMF_CalendarGet - Gets the calendar type

! !INTERFACE:
      subroutine ESMF_CalendarGet(calendar, type, rc)

! !ARGUMENTS:
      type(ESMF_Calendar), intent(in) :: calendar
      type(ESMF_CalendarType), intent(out) :: type
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Gets the {\tt ESMF\_Calendar} type associated with the given 
!     {\tt ESMF\_Calendar}.
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          The object instance to get the type from.
!     \item[type]
!          The {\tt CalendarType} ESMF\_CAL\_GREGORIAN, ESMF\_CAL\_JULIAN, etc.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!    
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
    
!     invoke C to C++ entry point
      call c_ESMC_CalendarGet(calendar, type, rc)
    
      end subroutine ESMF_CalendarGet
    
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CalendarSetGeneric - Initialize calendar to user-type

! !INTERFACE:
      subroutine ESMF_CalendarSetGeneric(calendar, daysPerMonth, &
                                         secondsPerDay, daysPerYear, &
                                         daysPerYearDn, daysPerYearDd, rc)
! !ARGUMENTS:
      type(ESMF_Calendar), intent(out) :: calendar
      integer, dimension(MONTHS_PER_YEAR), intent(in) :: daysPerMonth
      integer, intent(in) :: secondsPerDay
      integer, intent(in) :: daysPerYear
      integer, intent(in) :: daysPerYearDn
      integer, intent(in) :: daysPerYearDd
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Initializes a {\tt ESMF\_Calendar} to a user-specified type.
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          The object instance to initialize.
!     \item[daysPerMonth]
!          Integer array of days per month, for each of the 12 months.
!     \item[secondsPerDay]
!          Integer number of seconds per day.
!     \item[daysPerYear]
!          Integer number of days per year.
!     \item[daysPerYearDn]
!          Integer fractional number of days per year (numerator).
!     \item[daysPerYearDd]
!          Integer fractional number of days per year (denominator).
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!EOP
! !REQUIREMENTS:
!     TMGn.n.n

!     invoke C to C++ entry point
      call c_ESMC_CalendarSetGeneric(calendar, daysPerMonth, &
                                     secondsPerDay, daysPerYear, &
                                     daysPerYearDn, daysPerYearDd, rc)
    
      end subroutine ESMF_CalendarSetGeneric
    
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CalendarGetGeneric - Get generic calendar

! !INTERFACE:
      subroutine ESMF_CalendarGetGeneric(calendar, daysPerMonth, &
                                         secondsPerDay, daysPerYear, &
                                         daysPerYearDn, daysPerYearDd, rc)
! !ARGUMENTS:
      type(ESMF_Calendar), intent(in) :: calendar
      integer, dimension(MONTHS_PER_YEAR), intent(out) :: daysPerMonth
      integer, intent(out) :: secondsPerDay
      integer, intent(out) :: daysPerYear
      integer, intent(out) :: daysPerYearDn
      integer, intent(out) :: daysPerYearDd
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Gets the values of a user-specified {\tt ESMF\_Calendar} type.
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          The object instance to get the values from.
!     \item[daysPerMonth]
!          Integer array of days per month, for each of the 12 months.
!     \item[secondsPerDay]
!          Integer number of seconds per day.
!     \item[daysPerYear]
!          Integer number of days per year.
!     \item[daysPerYearDn]
!          Integer fractional number of days per year (numerator).
!     \item[daysPerYearDd]
!          Integer fractional number of days per year (denominator).
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!EOP
! !REQUIREMENTS:
!     TMGn.n.n

!     invoke C to C++ entry point
      call c_ESMC_CalendarGetGeneric(calendar, daysPerMonth, &
                                     secondsPerDay, daysPerYear, &
                                     daysPerYearDn, daysPerYearDd, rc)
    
      end subroutine ESMF_CalendarGetGeneric
    
!------------------------------------------------------------------------------
! 
! This section defines the overridden Read, Write, Validate and Print methods
! from the ESMF_Base class
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_CalendarReadRestart - Restore a calendar's properties

! !INTERFACE:
      subroutine ESMF_CalendarReadRestart(calendar, type, daysPerMonth, &
                                          secondsPerDay, daysPerYear, &
                                          daysPerYearDn, daysPerYearDd, rc)

! !ARGUMENTS:
      type(ESMF_Calendar), intent(out) :: calendar
      type(ESMF_CalendarType), intent(in) :: type
      integer, dimension(MONTHS_PER_YEAR), intent(in) :: daysPerMonth
      integer, intent(in) :: secondsPerDay
      integer, intent(in) :: daysPerYear
      integer, intent(in) :: daysPerYearDn
      integer, intent(in) :: daysPerYearDd
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a restore on a {\tt ESMF\_Calendar}'s properties.
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          {\tt ESMF\_Calendar} to restore.
!     \item[type]
!          The {\tt ESMF\_CalendarType} ESMF\_CAL\_GREGORIAN,
!          ESMF\_CAL\_JULIAN, etc.
!     \item[daysPerMonth]
!          Integer array of days per month, for each of the 12 months.
!     \item[secondsPerDay]
!          Integer number of seconds per day.
!     \item[daysPerYear]
!          Integer number of days per year.
!     \item[daysPerYearDn]
!          Integer fractional number of days per year (numerator).
!     \item[daysPerYearDd]
!          Integer fractional number of days per year (denominator).
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
   
      call c_ESMC_CalendarReadRestart(calendar, type, daysPerMonth, &
                                      secondsPerDay, daysPerYear, &
                                      daysPerYearDn, daysPerYearDd, rc)

      end subroutine ESMF_CalendarReadRestart

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_CalendarWriteRestart - Save a calendar's properties

! !INTERFACE:
      subroutine ESMF_CalendarWriteRestart(calendar, type, daysPerMonth, &
                                           secondsPerDay, daysPerYear, &
                                           daysPerYearDn, daysPerYearDd, rc)

! !ARGUMENTS:
      type(ESMF_Calendar), intent(in) :: calendar
      type(ESMF_CalendarType), intent(out) :: type
      integer, dimension(MONTHS_PER_YEAR), intent(out) :: daysPerMonth
      integer, intent(out) :: secondsPerDay
      integer, intent(out) :: daysPerYear
      integer, intent(out) :: daysPerYearDn
      integer, intent(out) :: daysPerYearDd
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a save on a {\tt ESMF\_Calendar}'s properties.
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          {\tt ESMF\_Calendar} to save.
!     \item[type]
!          The {\tt ESMF\_CalendarType} ESMF\_CAL\_GREGORIAN,
!           ESMF\_CAL\_JULIAN, etc.
!     \item[daysPerMonth]
!          Integer array of days per month, for each of the 12 months.
!     \item[secondsPerDay]
!          Integer number of seconds per day.
!     \item[daysPerYear]
!          Integer number of days per year.
!     \item[daysPerYearDn]
!          Integer fractional number of days per year (numerator).
!     \item[daysPerYearDd]
!          Integer fractional number of days per year (denominator).
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
   
      call c_ESMC_CalendarWriteRestart(calendar, type, daysPerMonth, &
                                       secondsPerDay, daysPerYear, &
                                       daysPerYearDn, daysPerYearDd, rc)

      end subroutine ESMF_CalendarWriteRestart

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_CalendarValidate - Validate a calendar's properties

! !INTERFACE:
      subroutine ESMF_CalendarValidate(calendar, opts, rc)
 
! !ARGUMENTS:
      type(ESMF_Calendar), intent(in) :: calendar
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a validation check on a {\tt ESMF\_Calendar}'s properties
! 
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          {\tt ESMF\_Calendar} to validate
!     \item[{[opts]}]
!          Validate options
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
      
      call c_ESMC_CalendarValidate(calendar, opts, rc)

      end subroutine ESMF_CalendarValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_CalendarPrint - Print out a calendar's properties

! !INTERFACE:
      subroutine ESMF_CalendarPrint(calendar, opts, rc)

! !ARGUMENTS:
      type(ESMF_Calendar), intent(in) :: calendar
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     To support testing/debugging, print out a {\tt ESMF\_Calendar}'s  
!     properties.
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          {\tt ESMF\_Calendar} to print out.
!     \item[{[opts]}]
!          Print options.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
  
      call c_ESMC_CalendarPrint(calendar, opts, rc)

      end subroutine ESMF_CalendarPrint
      
!------------------------------------------------------------------------------

      end module ESMF_CalendarMod
