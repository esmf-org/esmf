! $Id: ESMF_Time.F90,v 1.24 2003/04/30 07:45:14 eschwab Exp $
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
!     ESMF Time Module
      module ESMF_TimeMod
!
!==============================================================================
!
! This file contains the Time class definition and all Time class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF_TimeMgr.inc>

!==============================================================================
!BOPI
! !MODULE: ESMF_TimeMod
!
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation
!
! Defines F90 wrapper entry points for corresponding
! C++ class ESMC\_Time implementation
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from ESMF base class
      use ESMF_BaseMod

      ! inherit from base time class
      use ESMF_BaseTimeMod

      ! associated derived types
      use ESMF_TimeIntervalMod
      use ESMF_CalendarMod

      implicit none
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMF_Time
!
!     ! F90 class type to match C++ Time class in size only;
!     !  all dereferencing within class is performed by C++ implementation

!     ! Equivalent sequence and kind to C++:

     type ESMF_Time
     sequence                           ! match C++ storage order
     private                            !   (members opaque on F90 side)
       type(ESMF_BaseTime) :: basetime           ! inherit base class
       type(ESMF_Calendar), pointer :: calendar  ! associated calendar
       integer :: timezone                       ! local timezone
       integer :: pad                            ! to satisfy halem compiler
     end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Time
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
      public ESMF_TimeInit
      public ESMF_TimeGet
      public ESMF_TimeSet
      public ESMF_TimeGetCalendar
      public ESMF_TimeSetCalendar
      public ESMF_TimeIsSameCal
      public ESMF_TimeGetTimezone
      public ESMF_TimeSetTimezone
      public ESMF_TimeGetString
      public ESMF_TimeGetDayOfYear
      public ESMF_TimeGetDayOfWeek
      public ESMF_TimeGetDayOfMonth
      public ESMF_TimeGetMidMonth
      public ESMF_TimeGetRealTime

! Required inherited and overridden ESMF_Base class methods

      public ESMF_TimeRead
      public ESMF_TimeWrite
      public ESMF_TimeValidate
      public ESMF_TimePrint

! !PRIVATE MEMBER FUNCTIONS:

      private ESMF_TimeGetDayOfYearDouble
      private ESMF_TimeGetDayOfYearInteger
      private ESMF_TimeGetDayOfYearTimeInt

! Inherited and overloaded from ESMF_BaseTime

      public operator(+)
      private ESMF_TimeInc

      public operator(-)
      private ESMF_TimeDec
      private ESMF_TimeDiff

      public operator(.EQ.)
      private ESMF_TimeEQ

      public operator(.NE.)
      private ESMF_TimeNE

      public operator(.LT.)
      private ESMF_TimeLT

      public operator(.GT.)
      private ESMF_TimeGT

      public operator(.LE.)
      private ESMF_TimeLE

      public operator(.GE.)
      private ESMF_TimeGE
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Time.F90,v 1.24 2003/04/30 07:45:14 eschwab Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface ESMF_TimeGetDayOfYear

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeGetDayOfYearDouble
      module procedure ESMF_TimeGetDayOfYearInteger
      module procedure ESMF_TimeGetDayOfYearTimeInt

! !DESCRIPTION:
!     This interface overloads the ESMF_GetDayOfYear for the {\tt Time} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(+)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeInc

! !DESCRIPTION:
!     This interface overloads the + operator for the {\tt Time} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(-)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeDec

! !DESCRIPTION:
!     This interface overloads the - operator for the {\tt Time} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(-)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeDiff

! !DESCRIPTION:
!     This interface overloads the - operator for the {\tt Time} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.EQ.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeEQ

! !DESCRIPTION:
!     This interface overloads the .EQ. operator for the {\tt Time} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.NE.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeNE

! !DESCRIPTION:
!     This interface overloads the .NE. operator for the {\tt Time} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.LT.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeLT

! !DESCRIPTION:
!     This interface overloads the .LT. operator for the {\tt Time} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.GT.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeGT

! !DESCRIPTION:
!     This interface overloads the .GT. operator for the {\tt Time} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.LE.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeLE

! !DESCRIPTION:
!     This interface overloads the .LE. operator for the {\tt Time} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.GE.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeGE

! !DESCRIPTION:
!     This interface overloads the .GE. operator for the {\tt Time} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------

!==============================================================================

      contains

!==============================================================================
!
! This section includes the Init methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeInit - Initialize via user-specified unit set

! !INTERFACE:
      subroutine ESMF_TimeInit(time, YR, MM, DD, D, H, M, S, MS, US, NS, &
                               d_, h_, m_, s_, ms_, us_, ns_, &
                               Sn, Sd, cal, tz, rc)

! !ARGUMENTS:
      type(ESMF_Time), intent(out) :: time
      integer(ESMF_IKIND_I8), intent(in), optional :: YR
      integer, intent(in), optional :: MM
      integer, intent(in), optional :: DD
      integer(ESMF_IKIND_I8), intent(in), optional :: D
      integer, intent(in), optional :: H
      integer, intent(in), optional :: M
      integer(ESMF_IKIND_I8), intent(in), optional :: S
      integer, intent(in), optional :: MS
      integer, intent(in), optional :: US
      integer, intent(in), optional :: NS
      double precision, intent(in), optional :: d_
      double precision, intent(in), optional :: h_
      double precision, intent(in), optional :: m_
      double precision, intent(in), optional :: s_
      double precision, intent(in), optional :: ms_
      double precision, intent(in), optional :: us_
      double precision, intent(in), optional :: ns_
      integer, intent(in), optional :: Sn
      integer, intent(in), optional :: Sd
      type(ESMF_Calendar), intent(in), optional :: cal
      integer, intent(in), optional :: tz
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Initializes a {\tt ESMF\_Time} with set of user-specified units
!     via F90 optional arguments
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to initialize
!     \item[{[YR]}]
!          Integer year CCYR (64-bit)
!     \item[{[MM]}]
!          Integer month 1-12
!     \item[{[DD]}]
!          Integer day of the month 1-31
!     \item[{[D]}]
!          Integer Julian days (64-bit)
!     \item[{[H]}]
!          Integer hours
!     \item[{[M]}]
!          Integer minutes
!     \item[{[S]}]
!          Integer seconds (64-bit)
!     \item[{[MS]}]
!          Integer milliseconds
!     \item[{[US]}]
!          Integer microseconds
!     \item[{[NS]}]
!          Integer nanoseconds
!     \item[{[d\_]}]
!          Double precision days
!     \item[{[h\_]}]
!          Double precision hours
!     \item[{[m\_]}]
!          Double precision minutes
!     \item[{[s\_]}]
!          Double precision seconds
!     \item[{[ms\_]}]
!          Double precision milliseconds
!     \item[{[us\_]}]
!          Double precision microseconds
!     \item[{[ns\_]}]
!          Double precision nanoseconds
!     \item[{[Sn]}]
!          Integer fractional seconds - numerator
!     \item[{[Sd]}]
!          Integer fractional seconds - denominator
!     \item[{[cal]}]
!          Associated {\tt Calendar}
!     \item[{[tz]}]
!          Associated timezone (hours offset from GMT, e.g. EST = -5)
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP

      ! use optional args for any subset
      call c_ESMC_TimeInit(time, YR, MM, DD, D, H, M, S, MS, US, NS, &
                           d_, h_, m_, s_, ms_, us_, ns_, &
                           Sn, Sd, cal, tz, rc)

      end subroutine ESMF_TimeInit

!------------------------------------------------------------------------------
!
! This section includes the Time Get and Set methods.
!
!------------------------------------------------------------------------------
!
! Generic Get/Set routines which use F90 optional arguments
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeGet - Get value in user-specified units

! !INTERFACE:
      subroutine ESMF_TimeGet(time, YR, MM, DD, D, H, M, S, MS, US, NS, &
                              d_, h_, m_, s_, ms_, us_, ns_, Sn, Sd, rc)

! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time
      integer(ESMF_IKIND_I8), intent(out), optional :: YR
      integer, intent(out), optional :: MM
      integer, intent(out), optional :: DD
      integer(ESMF_IKIND_I8), intent(out), optional :: D
      integer, intent(out), optional :: H
      integer, intent(out), optional :: M
      integer(ESMF_IKIND_I8), intent(out), optional :: S
      integer, intent(out), optional :: MS
      integer, intent(out), optional :: US
      integer, intent(out), optional :: NS
      double precision, intent(out), optional :: d_
      double precision, intent(out), optional :: h_
      double precision, intent(out), optional :: m_
      double precision, intent(out), optional :: s_
      double precision, intent(out), optional :: ms_
      double precision, intent(out), optional :: us_
      double precision, intent(out), optional :: ns_
      integer, intent(out), optional :: Sn
      integer, intent(out), optional :: Sd
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get the value of the {\tt Time} in units specified by the user
!     via F90 optional arguments
!     
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to query
!     \item[{[YR]}]
!          Integer year CCYR (64-bit)
!     \item[{[MM]}]
!          Integer month 1-12
!     \item[{[DD]}]
!          Integer day of the month 1-31
!     \item[{[D]}]
!          Integer Julian days (64-bit)
!     \item[{[H]}]
!          Integer hours
!     \item[{[M]}]
!          Integer minutes
!     \item[{[S]}]
!          Integer seconds (64-bit)
!     \item[{[MS]}]
!          Integer milliseconds
!     \item[{[US]}]
!          Integer microseconds
!     \item[{[NS]}]
!          Integer nanoseconds
!     \item[{[d\_]}]
!          Double precision days
!     \item[{[h\_]}]
!          Double precision hours
!     \item[{[m\_]}]
!          Double precision minutes
!     \item[{[s\_]}]
!          Double precision seconds
!     \item[{[ms\_]}]
!          Double precision milliseconds
!     \item[{[us\_]}]
!          Double precision microseconds
!     \item[{[ns\_]}]
!          Double precision nanoseconds
!     \item[{[Sn]}]
!          Integer fractional seconds - numerator
!     \item[{[Sd]}]
!          Integer fractional seconds - denominator
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG2.1, TMG2.5.1, TMG2.5.6
!EOP

      ! use optional args for any subset
      call c_ESMC_TimeGet(time, YR, MM, DD, D, H, M, S, MS, US, NS, &
                          d_, h_, m_, s_, ms_, us_, ns_, Sn, Sd, rc)
    
      end subroutine ESMF_TimeGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeSet - Set value in user-specified units

! !INTERFACE:
      subroutine ESMF_TimeSet(time, YR, MM, DD, D, H, M, S, MS, US, NS, &
                              d_, h_, m_, s_, ms_, us_, ns_, Sn, Sd, rc)

! !ARGUMENTS:
      type(ESMF_Time), intent(out) :: time
      integer(ESMF_IKIND_I8), intent(in), optional :: YR
      integer, intent(in), optional :: MM
      integer, intent(in), optional :: DD
      integer(ESMF_IKIND_I8), intent(in), optional :: D
      integer, intent(in), optional :: H
      integer, intent(in), optional :: M
      integer(ESMF_IKIND_I8), intent(in), optional :: S
      integer, intent(in), optional :: MS
      integer, intent(in), optional :: US
      integer, intent(in), optional :: NS
      double precision, intent(in), optional :: d_
      double precision, intent(in), optional :: h_
      double precision, intent(in), optional :: m_
      double precision, intent(in), optional :: s_
      double precision, intent(in), optional :: ms_
      double precision, intent(in), optional :: us_
      double precision, intent(in), optional :: ns_
      integer, intent(in), optional :: Sn
      integer, intent(in), optional :: Sd
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set the value of the {\tt Time} in units specified by the user
!     via F90 optional arguments
!     
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to query
!     \item[{[YR]}]
!          Integer year CCYR (64-bit)
!     \item[{[MM]}]
!          Integer month 1-12
!     \item[{[DD]}]
!          Integer day of the month 1-31
!     \item[{[D]}]
!          Integer Julian days (64-bit)
!     \item[{[H]}]
!          Integer hours
!     \item[{[M]}]
!          Integer minutes
!     \item[{[S]}]
!          Integer seconds (64-bit)
!     \item[{[MS]}]
!          Integer milliseconds
!     \item[{[US]}]
!          Integer microseconds
!     \item[{[NS]}]
!          Integer nanoseconds
!     \item[{[d\_]}]
!          Double precision days
!     \item[{[h\_]}]
!          Double precision hours
!     \item[{[m\_]}]
!          Double precision minutes
!     \item[{[s\_]}]
!          Double precision seconds
!     \item[{[ms\_]}]
!          Double precision milliseconds
!     \item[{[us\_]}]
!          Double precision microseconds
!     \item[{[ns\_]}]
!          Double precision nanoseconds
!     \item[{[Sn]}]
!          Integer fractional seconds - numerator
!     \item[{[Sd]}]
!          Integer fractional seconds - denominator
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG2.1, TMG2.5.1, TMG2.5.6
!EOP
    
      ! use optional args for any subset
       call c_ESMC_TimeSet(time, YR, MM, DD, D, H, M, S, MS, US, NS, &
                           d_, h_, m_, s_, ms_, us_, ns_, Sn, Sd, rc)
    
      end subroutine ESMF_TimeSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeGetCalendar - Get associated calendar

! !INTERFACE:
      subroutine ESMF_TimeGetCalendar(time, cal, rc)

! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time
      type(ESMF_Calendar), intent(out) :: cal
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get the associated {\tt Calendar}
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to query
!     \item[{cal}]
!          Associated {\tt Calendar}
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP

      call c_ESMC_TimeGetCalendar(time, cal, rc)
    
      ! TODO: create overloaded ESMF_TimeGetCalendar() which gets calendar
      !       pointer rather than copy

      end subroutine ESMF_TimeGetCalendar

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeSetCalendar - Set associated calendar

! !INTERFACE:
      subroutine ESMF_TimeSetCalendar(time, cal, rc)

! !ARGUMENTS:
      type(ESMF_Time), intent(out) :: time
      type(ESMF_Calendar), intent(in) :: cal
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set the associated {\tt Calendar}
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to set
!     \item[{cal}]
!          Associated {\tt Calendar}
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP

      call c_ESMC_TimeSetCalendar(time, cal, rc)
    
      end subroutine ESMF_TimeSetCalendar

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIsSameCal - Compare calendars of two time instants

! !INTERFACE:
      function ESMF_TimeIsSameCal(time1, time2, rc)

! !RETURN VALUE:
      logical :: ESMF_TimeIsSameCal

! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time1
      type(ESMF_Time), intent(in) :: time2
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Returns true if both {\tt Time}'s {\tt Calendar}s are the same,
!     false otherwise
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          The first object instance to compare
!     \item[time2]
!          The second object instance to compare
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP

      call c_ESMC_TimeIsSameCal(time1, time2, ESMF_TimeIsSameCal, rc)
    
      end function ESMF_TimeIsSameCal

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeGetTimezone - Get time instant's time zone
!
! !INTERFACE:
      subroutine ESMF_TimeGetTimezone(time, Timezone, rc)
!
! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time
      integer, intent(out) :: Timezone
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Get the time zone of the given {\tt Time} instant
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to query
!     \item[{Timezone}]
!          {\tt Time} instant's time zone
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG2.5.1
!EOP

      call c_ESMC_TimeGetTimezone(time, Timezone, rc)

      end subroutine ESMF_TimeGetTimezone

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeSetTimezone - Set time instant's time zone
!
! !INTERFACE:
      subroutine ESMF_TimeSetTimezone(time, Timezone, rc)
!
! !ARGUMENTS:
      type(ESMF_Time), intent(out) :: time
      integer, intent(in) :: Timezone
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Set the time zone of the given {\tt Time} instant
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to set
!     \item[{Timezone}]
!          {\tt Time} instant's time zone
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG2.5.1
!EOP

      call c_ESMC_TimeSetTimezone(time, Timezone, rc)

      end subroutine ESMF_TimeSetTimezone

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeGetString - Get time instant value in string format

! !INTERFACE:
      subroutine ESMF_TimeGetString(time, TimeString, rc)

! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time
      character, dimension(ESMF_MAXSTR), intent(out) :: TimeString
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Convert {\tt Time}'s value into string format
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to convert
!     \item[TimeString]
!          The string to return
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG2.4.7
!EOP

      call c_ESMC_TimeGetString(time, TimeString, rc)

      end subroutine ESMF_TimeGetString

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeGetDayOfYearDouble - Get time instant's day of the year as a floating point value
!
! !INTERFACE:
      subroutine ESMF_TimeGetDayOfYearDouble(time, DayOfYear, rc)
!
! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time
      double precision, intent(out) :: DayOfYear
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Get the day of the year the given {\tt Time} instant falls on (1.x-365.x)
!     Returned as floating point value; fractional part represents the
!     time of day. 
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to query
!     \item[DayOfYear]
!          The {\tt Time} instant's day of the year (1-365)
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG2.5.2
!EOP

      call c_ESMC_TimeGetDayOfYearDouble(time, DayOfYear, rc)

      end subroutine ESMF_TimeGetDayOfYearDouble

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeGetDayOfYearInteger - Get time instant's day of the year as an integer value
!
! !INTERFACE:
      subroutine ESMF_TimeGetDayOfYearInteger(time, DayOfYear, rc)
!
! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time
      integer, intent(out) :: DayOfYear
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Get the day of the year the given {\tt Time} instant falls on (1-365)
!     Returned as an integer value
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to query
!     \item[DayOfYear]
!          The {\tt Time} instant's day of the year (1-365)
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

      call c_ESMC_TimeGetDayOfYearInteger(time, DayOfYear, rc)

      end subroutine ESMF_TimeGetDayOfYearInteger

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeGetDayOfYearTimeInt - Get time instant's day of the year as a Time Interval
!
! !INTERFACE:
      subroutine ESMF_TimeGetDayOfYearTimeInt(time, DayOfYear, rc)
!
! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time
      type(ESMF_TimeInterval), intent(out) :: DayOfYear
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Get the day of the year the given {\tt Time} instant falls on (1-365)
!     Returned as a Time Interval
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to query
!     \item[DayOfYear]
!          The {\tt Time} instant's day of the year as a
!            {\tt ESMC_TimeInterval}
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

      call c_ESMC_TimeGetDayOfYearTimeInt(time, DayOfYear, rc)

      end subroutine ESMF_TimeGetDayOfYearTimeInt

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeGetDayOfWeek - Get time instant's day of the week
!
! !INTERFACE:
      subroutine ESMF_TimeGetDayOfWeek(time, DayOfWeek, rc)
!
! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time
      integer, intent(out) :: DayOfWeek
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Get the day of the week the given time instant falls on.
!     ISO 8601 standard:  Monday = 1 through Sunday = 7
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to query
!     \item[DayOfWeek]
!          The time instant's day of the week (1-7)
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG2.5.3
!EOP
    
      call c_ESMC_TimeGetDayOfWeek(time, DayOfWeek, rc)

      end subroutine ESMF_TimeGetDayOfWeek

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeGetDayOfMonth - Get time instant's day of the month
!
! !INTERFACE:
      subroutine ESMF_TimeGetDayOfMonth(time, DayOfMonth, rc)
!
! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time
      integer, intent(out) :: DayOfMonth
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Get the day of the month the time instant falls on (1-31)
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to query
!     \item[DayOfMonth]
!          The time instant's day of the month (1-31)
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG2.5.4
!EOP

      call c_ESMC_TimeGetDayOfMonth(time, DayOfMonth, rc)

      end subroutine ESMF_TimeGetDayOfMonth

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeGetMidMonth - Get time instant's middle of the month
!
! !INTERFACE:
      subroutine ESMF_TimeGetMidMonth(time, MidMonth, rc)
!
! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time
      type(ESMF_Time), intent(out) :: MidMonth
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Get the middle time instant of the month the given time instant
!     falls on
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to query
!     \item[MidMonth]
!          The given time instant's middle-of-the-month time instant
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG2.5.5
!EOP

      call c_ESMC_TimeGetMidMonth(time, MidMonth, rc)

      end subroutine ESMF_TimeGetMidMonth

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeGetRealTime - Get system real time (wall clock time)
!
! !INTERFACE:
      subroutine ESMF_TimeGetRealTime(time, rc)
!
! !ARGUMENTS:
      type(ESMF_Time), intent(inout) :: time
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Get the system real time (wall clock time), return in given time
!     instant
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to receive the real time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG2.5.7
!EOP

      call c_ESMC_TimeGetRealTime(time, rc)

      end subroutine ESMF_TimeGetRealTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeInc - Increment time instant with a time interval
!
! !INTERFACE:
      function ESMF_TimeInc(time, timeinterval)
!
! !RETURN VALUE:
      type(ESMF_Time) :: ESMF_TimeInc
!
! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time
      type(ESMF_TimeInterval), intent(in) :: timeinterval
!
! !DESCRIPTION:
!     Increment {\tt Time} instant with a {\tt TimeInterval},
!     return resulting {\tt Time} instant
!
!     Maps overloaded (+) operator interface function to
!     {\tt ESMF\_BaseTime} base class
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The given {\tt Time} to increment
!     \item[timeinterval]
!          The {\tt TimeInterval} to add to the given {\tt Time}
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.4, TMG2.4.4, TMG2.4.5, TMG2.4.6, TMG5.1, TMG5.2, TMG7.2
!EOP

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeSum(time, timeinterval, ESMF_TimeInc)

      end function ESMF_TimeInc
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeDec - Decrement time instant with a time interval
!
! !INTERFACE:
      function ESMF_TimeDec(time, timeinterval)
!
! !RETURN VALUE:
      type(ESMF_Time) :: ESMF_TimeDec
!
! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time
      type(ESMF_TimeInterval), intent(in) :: timeinterval
!
! !DESCRIPTION:
!     Decrement {\tt Time} instant with a {\tt TimeInterval},
!     return resulting {\tt Time} instant
!
!     Maps overloaded (-) operator interface function to
!     {\tt ESMF\_BaseTime} base class
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The given {\tt Time} to decrement
!     \item[timeinterval]
!          The {\tt TimeInterval} to subtract from the given {\tt Time}
!     \end{description}
!     
! !REQUIREMENTS:
!     TMG1.5.4, TMG2.4.4, TMG2.4.5, TMG2.4.6, TMG5.1, TMG5.2, TMG7.2
!EOP

      ! call ESMC_BaseTime base class function
       call c_ESMC_BaseTimeDiff(time, timeinterval, ESMF_TimeDec)

      end function ESMF_TimeDec

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeDiff - Return the difference between two time instants
!
! !INTERFACE:
      function ESMF_TimeDiff(time1, time2)
!
! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeDiff
!
! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time1
      type(ESMF_Time), intent(in) :: time2

! !DESCRIPTION:
!     Return the {\tt TimeInterval} difference between two {\tt Time} instants
!
!     Maps overloaded (-) operator interface function to
!     {\tt ESMF\_BaseTime} base class
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          The first {\tt Time} instant
!     \item[time2]
!          The second {\tt Time} instant
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.4, TMG2.4.4, TMG2.4.5, TMG2.4.6, TMG5.1, TMG5.2, TMG7.2
!EOP

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeDiff(time1, time2, ESMF_TimeDiff)

      end function ESMF_TimeDiff

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeEQ - Compare two times for equality
!
! !INTERFACE:
      function ESMF_TimeEQ(time1, time2)
!
! !RETURN VALUE:
      logical :: ESMF_TimeEQ
!
! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time1
      type(ESMF_Time), intent(in) :: time2
!
! !DESCRIPTION:
!     Return true if both given time instants are equal, false otherwise.
!     Maps overloaded (==) operator interface function to {\tt ESMF\_BaseTime}
!     base class.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First time instant to compare
!     \item[time2]
!          Second time instant to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP

      ! invoke C to C++ entry point for ESMF_BaseTime base class function
      call c_ESMC_BaseTimeEQ(time1, time2, ESMF_TimeEQ)

      end function ESMF_TimeEQ

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeNE - Compare two times for non-equality
!
! !INTERFACE:
      function ESMF_TimeNE(time1, time2)
!
! !RETURN VALUE:
      logical :: ESMF_TimeNE
!
! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time1
      type(ESMF_Time), intent(in) :: time2

! !DESCRIPTION:
!     Return true if both given time instants are not equal, false otherwise.
!     Maps overloaded (/=) operator interface function to {\tt ESMF\_BaseTime}
!     base class.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First time instant to compare
!     \item[time2]
!          Second time instant to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeNE(time1, time2, ESMF_TimeNE)

      end function ESMF_TimeNE

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeLT - Time instant 1 less than time instant 2 ?
!
! !INTERFACE:
      function ESMF_TimeLT(time1, time2)
!
! !RETURN VALUE:
      logical :: ESMF_TimeLT
!
! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time1
      type(ESMF_Time), intent(in) :: time2
!
! !DESCRIPTION:
!     Return true if first time instant is less than second time instant,
!     false otherwise.
!     Maps overloaded (<) operator interface function to {\tt ESMF\_BaseTime}
!     base class.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First time instant to compare
!     \item[time2]
!          Second time instant to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeLT(time1, time2, ESMF_TimeLT)

      end function ESMF_TimeLT

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeGT - Time instant 1 greater than time instant 2 ?
!
! !INTERFACE:
      function ESMF_TimeGT(time1, time2)
!
! !RETURN VALUE:
      logical :: ESMF_TimeGT
!
! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time1
      type(ESMF_Time), intent(in) :: time2
!
! !DESCRIPTION:
!     Return true if first time instant is greater than second time instant,
!     false otherwise.
!     Maps overloaded (>) operator interface function to {\tt ESMF\_BaseTime}
!     base class.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First time instant to compare
!     \item[time2]
!          Second time instant to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeGT(time1, time2, ESMF_TimeGT)

      end function ESMF_TimeGT

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeLE - Time instant 1 less than or equal to time instant 2 ?
!
! !INTERFACE:
      function ESMF_TimeLE(time1, time2)
!
! !RETURN VALUE:
      logical :: ESMF_TimeLE
!
! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time1
      type(ESMF_Time), intent(in) :: time2
!
! !DESCRIPTION:
!     Return true if first time instant is less than or equal to second time
!     instant, false otherwise.
!     Maps overloaded (<=) operator interface function to {\tt ESMF\_BaseTime}
!     base class.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First time instant to compare
!     \item[time2]
!          Second time instant to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeLE(time1, time2, ESMF_TimeLE)

      end function ESMF_TimeLE

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeGE - Time instant 1 greater than or equal to time instant 2 ?
!
! !INTERFACE:
      function ESMF_TimeGE(time1, time2)
!
! !RETURN VALUE:
      logical :: ESMF_TimeGE
!
! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time1
      type(ESMF_Time), intent(in) :: time2
!
! !DESCRIPTION:
!     Return true if first time instant is greater than or equal to second
!     time instant, false otherwise.
!     Maps overloaded (>=) operator interface function to {\tt ESMF\_BaseTime}
!     base class.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First time instant to compare
!     \item[time2]
!          Second time instant to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeGE(time1, time2, ESMF_TimeGE)

      end function ESMF_TimeGE

!------------------------------------------------------------------------------
!
! This section defines the overridden Read, Write, Validate and Print methods
! inherited from the ESMF_Base class
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeRead - Restore a time instant's properties

! !INTERFACE:
      subroutine ESMF_TimeRead(time, S, Sn, Sd, cal, tz, rc)

! !ARGUMENTS:
      type(ESMF_Time), intent(out) :: time
      integer(ESMF_IKIND_I8), intent(in) :: S
      integer, intent(in) :: Sn
      integer, intent(in) :: Sd
      type(ESMF_Calendar), intent(in) :: cal
      integer, intent(in) :: tz
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a restore on a {\tt Time}'s properties
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          Time instant to restore
!     \item[S]
!          64-bit integer seconds
!     \item[Sn]
!          Integer fractional seconds - numerator
!     \item[Sd]
!          Integer fractional seconds - denominator
!     \item[cal]
!          Associated {\tt Calendar}
!     \item[tz]
!          Associated timezone (hours offset from GMT, e.g. EST = -5)
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
   
      call c_ESMC_TimeRead(time, S, Sn, Sd, cal, tz, rc)

      end subroutine ESMF_TimeRead

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeWrite - Save a time instant's properties

! !INTERFACE:
      subroutine ESMF_TimeWrite(time, S, Sn, Sd, cal, tz, rc)

! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time
      integer(ESMF_IKIND_I8), intent(out) :: S
      integer, intent(out) :: Sn
      integer, intent(out) :: Sd
      type(ESMF_Calendar), intent(out) :: cal
      integer, intent(out) :: tz
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a save on a {\tt Time}'s properties
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          Time instant to save
!     \item[S]
!          64-bit integer seconds
!     \item[Sn]
!          Integer fractional seconds - numerator
!     \item[Sd]
!          Integer fractional seconds - denominator
!     \item[cal]
!          Associated {\tt Calendar}
!     \item[tz]
!          Associated timezone (hours offset from GMT, e.g. EST = -5)
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
   
      call c_ESMC_TimeWrite(time, S, Sn, Sd, cal, tz, rc)

      end subroutine ESMF_TimeWrite

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeValidate - Validate a time instant's properties

! !INTERFACE:
      subroutine ESMF_TimeValidate(time, opts, rc)

! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a validation check on a {\tt Time}'s properties
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          Time instant to validate
!     \item[{[opts]}]
!          Validation options
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
   
      call c_ESMC_TimeValidate(time, opts, rc)

      end subroutine ESMF_TimeValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimePrint - Print out a time instant's properties

! !INTERFACE:
      subroutine ESMF_TimePrint(time, opts, rc)

! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     To support testing/debugging, print out a {\tt Time}'s
!     properties.
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          Time instant to print out
!     \item[{[opts]}]
!          Print options
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
   
      call c_ESMC_TimePrint(time, opts, rc)

      end subroutine ESMF_TimePrint

!------------------------------------------------------------------------------

      end module ESMF_TimeMod
