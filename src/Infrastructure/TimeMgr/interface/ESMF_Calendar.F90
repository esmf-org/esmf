! $Id: ESMF_Calendar.F90,v 1.28 2003/09/03 20:49:00 cdeluca Exp $
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
                               ESMF_CAL_JULIANDAY =  ESMF_CalendarType(2), &
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
        integer(ESMF_IKIND_I4) :: days  ! whole days per year
        integer(ESMF_IKIND_I4) :: daysN ! fractional days per year numerator
        integer(ESMF_IKIND_I4) :: daysD ! fractional days per year denominator
      end type                          ! e.g. for Venus,
                                        !   days=0, daysN=926, daysD=1000
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
        integer(ESMF_IKIND_I4)              :: secondsPerDay
        integer(ESMF_IKIND_I4)              :: secondsPerYear
        type(ESMF_DaysPerYear)              :: daysPerYear
      end type
!
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public MONTHS_PER_YEAR
      public ESMF_CalendarType
      public ESMF_CAL_GREGORIAN, ESMF_CAL_JULIANDAY,  ESMF_CAL_NOLEAP, &
             ESMF_CAL_360DAY,    ESMF_CAL_GENERIC,    ESMF_CAL_NOCALENDAR
      public ESMF_Calendar
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
      public ESMF_CalendarSet
      public ESMF_CalendarSetGeneric
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
      '$Id: ESMF_Calendar.F90,v 1.28 2003/09/03 20:49:00 cdeluca Exp $'

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
!     Initialize or set {\tt calendar} to the given {\tt ESMF\_CalendarType}.  
!     Valid values for {\tt type} are:  {\tt ESMF\_CAL\_GREGORIAN}, 
!     {\tt ESMF\_CAL\_JULIANDAY},
!     {\tt ESMF\_CAL\_NOLEAP}, {\tt ESMF\_CAL\_360DAY}, {\tt ESMF\_CAL\_GENERIC}
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          The object instance to initialize.
!     \item[type]
!          The {\tt CalendarType}, e.g. {\tt ESMF\_CAL\_GREGORIAN}
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
! !IROUTINE: ESMF_CalendarSetGeneric - Set a custom Calendar

! !INTERFACE:
      subroutine ESMF_CalendarSetGeneric(calendar, daysPerMonth, &
                                         secondsPerDay, daysPerYear, &
                                         daysPerYearDn, daysPerYearDd, rc)
! !ARGUMENTS:
      type(ESMF_Calendar),                 intent(inout) :: calendar
      integer, dimension(MONTHS_PER_YEAR), intent(in)    :: daysPerMonth
      integer(ESMF_IKIND_I4),              intent(in)    :: secondsPerDay
      integer(ESMF_IKIND_I4),              intent(in)    :: daysPerYear
      integer(ESMF_IKIND_I4),              intent(in)    :: daysPerYearDn
      integer(ESMF_IKIND_I4),              intent(in)    :: daysPerYearDd
      integer,                             intent(out), optional :: rc

! !DESCRIPTION:
!     Sets values in a custom {\tt ESMF\_Calendar}.
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
! !IROUTINE: ESMF_CalendarGet - Get Calendar properties

! !INTERFACE:
      subroutine ESMF_CalendarGet(calendar, type, &
                                  daysPerMonth, secondsPerDay, secondsPerYear, &
                                  daysPerYear, daysPerYearDn, daysPerYearDd, rc)
! !ARGUMENTS:
      type(ESMF_Calendar),     intent(in)            :: calendar
      type(ESMF_CalendarType), intent(out), optional :: type
      integer, dimension(MONTHS_PER_YEAR), intent(out), optional :: daysPerMonth
      integer(ESMF_IKIND_I4),  intent(out), optional :: secondsPerDay
      integer(ESMF_IKIND_I4),  intent(out), optional :: secondsPerYear
      integer(ESMF_IKIND_I4),  intent(out), optional :: daysPerYear
      integer(ESMF_IKIND_I4),  intent(out), optional :: daysPerYearDn
      integer(ESMF_IKIND_I4),  intent(out), optional :: daysPerYearDd
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Gets an {\tt ESMF\_Calendar}'s properties.
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          The object instance from which to get the properties.
!     \item[{[type]}]
!          The {\tt CalendarType} ESMF\_CAL\_GREGORIAN, ESMF\_CAL\_JULIANDAY,
!          etc.
!     \item[{[daysPerMonth]}]
!          Integer array of days per month, for each month of the year.
!     \item[{[secondsPerDay]}]
!          Integer number of seconds per day.
!     \item[{[secondsPerYear]}]
!          Integer number of seconds per year.
!     \item[{[daysPerYear]}]
!          Integer number of days per year.
!     \item[{[daysPerYearDn]}]
!          Integer fractional number of days per year (numerator).
!     \item[{[daysPerYearDd]}]
!          Integer fractional number of days per year (denominator).
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!EOP
! !REQUIREMENTS:
!     TMGn.n.n

!     invoke C to C++ entry point
      call c_ESMC_CalendarGet(calendar, type, &
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
      subroutine ESMF_CalendarReadRestart(calendar, type, daysPerMonth, &
                                          secondsPerDay, secondsPerYear, &
                                          daysPerYear, daysPerYearDn, &
                                          daysPerYearDd, rc)

! !ARGUMENTS:
      type(ESMF_Calendar),                 intent(out) :: calendar
      type(ESMF_CalendarType),             intent(in)  :: type
      integer, dimension(MONTHS_PER_YEAR), intent(in)  :: daysPerMonth
      integer(ESMF_IKIND_I4),              intent(in)  :: secondsPerDay
      integer(ESMF_IKIND_I4),              intent(in)  :: secondsPerYear
      integer(ESMF_IKIND_I4),              intent(in)  :: daysPerYear
      integer(ESMF_IKIND_I4),              intent(in)  :: daysPerYearDn
      integer(ESMF_IKIND_I4),              intent(in)  :: daysPerYearDd
      integer,                             intent(out), optional :: rc

! !DESCRIPTION:
!     Restores the contents of an {\tt ESMF\_Calendar} for restart.
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          {\tt ESMF\_Calendar} to restore.
!     \item[type]
!          The {\tt ESMF\_CalendarType} ESMF\_CAL\_GREGORIAN,
!          ESMF\_CAL\_JULIANDAY, etc.
!     \item[daysPerMonth]
!          Integer array of days per month, for each of the 12 months.
!     \item[secondsPerDay]
!          Integer number of seconds per day.
!     \item[secondsPerYear]
!          Integer number of seconds per year.
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
      call c_ESMC_CalendarReadRestart(calendar, type, daysPerMonth, &
                                      secondsPerDay, secondsPerYear, &      
                                      daysPerYear, daysPerYearDn, &
                                      daysPerYearDd, rc) 

      end subroutine ESMF_CalendarReadRestart

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_CalendarWriteRestart - Save the contents of a Calendar

! !INTERFACE:
      subroutine ESMF_CalendarWriteRestart(calendar, type, daysPerMonth, &
                                           secondsPerDay, secondsPerYear, &
                                           daysPerYear, daysPerYearDn, &
                                           daysPerYearDd, rc)

! !ARGUMENTS:
      type(ESMF_Calendar),                 intent(in)  :: calendar
      type(ESMF_CalendarType),             intent(out) :: type
      integer, dimension(MONTHS_PER_YEAR), intent(out) :: daysPerMonth
      integer(ESMF_IKIND_I4),              intent(out) :: secondsPerDay
      integer(ESMF_IKIND_I4),              intent(out) :: secondsPerYear
      integer(ESMF_IKIND_I4),              intent(out) :: daysPerYear
      integer(ESMF_IKIND_I4),              intent(out) :: daysPerYearDn
      integer(ESMF_IKIND_I4),              intent(out) :: daysPerYearDd
      integer,                             intent(out), optional :: rc

! !DESCRIPTION:
!     Saves the contents of an {\tt ESMF\_Calendar} for restart.
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          {\tt ESMF\_Calendar} to save.
!     \item[type]
!          The {\tt ESMF\_CalendarType} ESMF\_CAL\_GREGORIAN,
!          ESMF\_CAL\_JULIANDAY, etc.
!     \item[daysPerMonth]
!          Integer array of days per month, for each of the 12 months.
!     \item[secondsPerDay]
!          Integer number of seconds per day.
!     \item[secondsPerYear]
!          Integer number of seconds per year.
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
      call c_ESMC_CalendarWriteRestart(calendar, type, daysPerMonth, &
                                       secondsPerDay, secondsPerYear, &     
                                       daysPerYear, daysPerYearDn, &        
                                       daysPerYearDd, rc) 

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
!     Check whether a {\tt calendar} is valid.
! 
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          {\tt ESMF\_Calendar} to validate
!     \item[{[options]}]
!          Validate options
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
!     To support testing and debugging, this method prints out 
!     the contents of an {\tt ESMF\_Calendar}.
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          {\tt ESMF\_Calendar} to print out.
!     \item[{[options]}]
!          Print options.
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
