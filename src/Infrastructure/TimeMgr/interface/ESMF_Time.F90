! $Id: ESMF_Time.F90,v 1.47 2003/09/04 18:57:57 cdeluca Exp $
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
! Part of Time Manager F90 API wrapper of C++ implemenation.
!
! Defines F90 wrapper entry points for corresponding
! C++ class {\tt ESMC\_Time} implementation.
!
! See {\tt ../include/ESMC\_Time.h} for complete description.
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
     sequence                                    ! match C++ storage order
     private                                     !  (members opaque on F90 side)
       type(ESMF_BaseTime)          :: baseTime  ! inherit base class
       type(ESMF_Calendar), pointer :: calendar  ! associated calendar
       integer                      :: timeZone  ! local timezone
       integer                      :: pad       ! to satisfy halem compiler
     end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Time
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
      public ESMF_TimeSet
      public ESMF_TimeGet

      public ESMF_TimeIsSameCalendar

      public ESMF_TimeSyncToRealTime

! Required inherited and overridden ESMF_Base class methods

      public ESMF_TimeReadRestart
      public ESMF_TimeWriteRestart
      public ESMF_TimeValidate
      public ESMF_TimePrint

! !PRIVATE MEMBER FUNCTIONS:

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
      '$Id: ESMF_Time.F90,v 1.47 2003/09/04 18:57:57 cdeluca Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface operator(+)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeInc

! !DESCRIPTION:
!     This interface overloads the + operator for the {\tt ESMF\_Time} class.
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
!     This interface overloads the - operator for the {\tt ESMF\_Time} class.
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
!     This interface overloads the - operator for the {\tt ESMF\_Time} class.
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
!     This interface overloads the .EQ. operator for the {\tt ESMF\_Time} class.
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
!     This interface overloads the .NE. operator for the {\tt ESMF\_Time} class.
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
!     This interface overloads the .LT. operator for the {\tt ESMF\_Time} class.
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
!     This interface overloads the .GT. operator for the {\tt ESMF\_Time} class.
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
!     This interface overloads the .LE. operator for the {\tt ESMF\_Time} class.
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
!     This interface overloads the .GE. operator for the {\tt ESMF\_Time} class.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------

!==============================================================================

      contains

!==============================================================================
!
! Generic Set/Get routines which use F90 optional arguments
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeSet - Initialize or set a Time

! !INTERFACE:
      subroutine ESMF_TimeSet(time, yr, yr_i8, &
                                    mm, dd, &
                                    d, d_i8, &
                                    h, m, &
                                    s, s_i8, &
                                    ms, us, ns, &
                                    d_r8, h_r8, m_r8, s_r8, &
                                    ms_r8, us_r8, ns_r8, &
                                    sN, sD, calendar, timeZone, rc)

! !ARGUMENTS:
      type(ESMF_Time),        intent(inout)         :: time
      integer(ESMF_KIND_I4), intent(in),  optional :: yr
      integer(ESMF_KIND_I8), intent(in),  optional :: yr_i8
      integer,                intent(in),  optional :: mm
      integer,                intent(in),  optional :: dd
      integer(ESMF_KIND_I4), intent(in),  optional :: d
      integer(ESMF_KIND_I8), intent(in),  optional :: d_i8
      integer(ESMF_KIND_I4), intent(in),  optional :: h
      integer(ESMF_KIND_I4), intent(in),  optional :: m
      integer(ESMF_KIND_I4), intent(in),  optional :: s
      integer(ESMF_KIND_I8), intent(in),  optional :: s_i8
      integer(ESMF_KIND_I4), intent(in),  optional :: ms
      integer(ESMF_KIND_I4), intent(in),  optional :: us
      integer(ESMF_KIND_I4), intent(in),  optional :: ns
      real(ESMF_KIND_R8),    intent(in),  optional :: d_r8
      real(ESMF_KIND_R8),    intent(in),  optional :: h_r8
      real(ESMF_KIND_R8),    intent(in),  optional :: m_r8
      real(ESMF_KIND_R8),    intent(in),  optional :: s_r8
      real(ESMF_KIND_R8),    intent(in),  optional :: ms_r8
      real(ESMF_KIND_R8),    intent(in),  optional :: us_r8
      real(ESMF_KIND_R8),    intent(in),  optional :: ns_r8
      integer(ESMF_KIND_I4), intent(in),  optional :: sN
      integer(ESMF_KIND_I4), intent(in),  optional :: sD
      type(ESMF_Calendar),    intent(in),  optional :: calendar
      integer,                intent(in),  optional :: timeZone
      integer,                intent(out), optional :: rc

! !DESCRIPTION:
!     Initializes an {\tt ESMF\_Time} with a set of user-specified units
!     via F90 optional arguments.
!
!     Time manager represents and manipulates time internally with integers
!     to maintain precision. Hence, user-specified floating point values are
!     converted internally to integers.
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to initialize.
!     \item[{[yr]}]
!          Integer year CCYR (>= 32-bit).
!     \item[{[yr\_i8]}]
!          Integer year CCYR (large, >= 64-bit).
!     \item[{[mm]}]
!          Integer month 1-12.
!     \item[{[dd]}]
!          Integer day of the month 1-31.
!     \item[{[d]}]
!          Integer Julian days (>= 32-bit).
!     \item[{[d\_i8]}]
!          Integer Julian days (large, >= 64-bit).
!     \item[{[h]}]
!          Integer hours.
!     \item[{[m]}]
!          Integer minutes.
!     \item[{[s]}]
!          Integer seconds (>= 32-bit).
!     \item[{[s\_i8]}]
!          Integer seconds (large, >= 64-bit).
!     \item[{[ms]}]
!          Integer milliseconds.
!     \item[{[us]}]
!          Integer microseconds.
!     \item[{[ns]}]
!          Integer nanoseconds.
!     \item[{[d\_r8]}]
!          Double precision days.
!     \item[{[h\_r8]}]
!          Double precision hours.
!     \item[{[m\_r8]}]
!          Double precision minutes.
!     \item[{[s\_r8]}]
!          Double precision seconds.
!     \item[{[ms\_r8]}]
!          Double precision milliseconds.
!     \item[{[us\_r8]}]
!          Double precision microseconds.
!     \item[{[ns\_r8]}]
!          Double precision nanoseconds.
!     \item[{[sN]}]
!          Integer fractional seconds - numerator.
!     \item[{[sD]}]
!          Integer fractional seconds - denominator.
!     \item[{[calendar]}]
!          Associated {\tt Calendar}.
!     \item[{[timeZone]}]
!          Associated timezone (hours offset from GMT, e.g. EST = -5).
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n

      ! use optional args for any subset
      call c_ESMC_TimeSet(time, yr, yr_i8, mm, dd, d, d_i8, &
                          h, m, s, s_i8, ms, us, ns, &
                          d_r8, h_r8, m_r8, s_r8, ms_r8, us_r8, ns_r8, &
                          sN, sD, calendar, timeZone, rc)

      end subroutine ESMF_TimeSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeGet - Get a Time value 

! !INTERFACE:
      subroutine ESMF_TimeGet(time, yr, yr_i8, &
                                    mm, dd, &
                                    d, d_i8, &
                                    h, m, &
                                    s, s_i8, &
                                    ms, us, ns, &
                                    d_r8, h_r8, m_r8, s_r8, &
                                    ms_r8, us_r8, ns_r8, &
                                    sN, sD, &
                                    calendar,   timeZone, &
                                    timeString, dayOfWeek, dayOfMonth, &
                                    midMonth,   dayOfYear, dayOfYear_r8, &
                                    dayOfYear_intvl, rc)

! !ARGUMENTS:
      type(ESMF_Time),         intent(in)            :: time
      integer(ESMF_KIND_I4),  intent(out), optional :: yr
      integer(ESMF_KIND_I8),  intent(out), optional :: yr_i8
      integer,                 intent(out), optional :: mm
      integer,                 intent(out), optional :: dd
      integer(ESMF_KIND_I4),  intent(out), optional :: d
      integer(ESMF_KIND_I8),  intent(out), optional :: d_i8
      integer(ESMF_KIND_I4),  intent(out), optional :: h
      integer(ESMF_KIND_I4),  intent(out), optional :: m
      integer(ESMF_KIND_I4),  intent(out), optional :: s
      integer(ESMF_KIND_I8),  intent(out), optional :: s_i8
      integer(ESMF_KIND_I4),  intent(out), optional :: ms
      integer(ESMF_KIND_I4),  intent(out), optional :: us
      integer(ESMF_KIND_I4),  intent(out), optional :: ns
      real(ESMF_KIND_R8),     intent(out), optional :: d_r8
      real(ESMF_KIND_R8),     intent(out), optional :: h_r8
      real(ESMF_KIND_R8),     intent(out), optional :: m_r8
      real(ESMF_KIND_R8),     intent(out), optional :: s_r8
      real(ESMF_KIND_R8),     intent(out), optional :: ms_r8
      real(ESMF_KIND_R8),     intent(out), optional :: us_r8
      real(ESMF_KIND_R8),     intent(out), optional :: ns_r8
      integer(ESMF_KIND_I4),  intent(out), optional :: sN
      integer(ESMF_KIND_I4),  intent(out), optional :: sD
      type(ESMF_Calendar),     intent(out), optional :: calendar
      integer,                 intent(out), optional :: timeZone
      character (len=*),       intent(out), optional :: timeString
      integer,                 intent(out), optional :: dayOfWeek
      integer,                 intent(out), optional :: dayOfMonth
      type(ESMF_Time),         intent(out), optional :: midMonth
      integer(ESMF_KIND_I4),  intent(out), optional :: dayOfYear
      real(ESMF_KIND_R8),     intent(out), optional :: dayOfYear_r8
      type(ESMF_TimeInterval), intent(out), optional :: dayOfYear_intvl
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Gets the value of {\tt time} in units specified by the user
!     via F90 optional arguments.
!
!     The ESMF Time Manager represents and manipulates time internally with 
!     integers to maintain precision. Hence, user-specified floating point 
!     values are converted internally from integers.
!
!     Units are bound (normalized) to the next larger unit specified.  For
!     example, if a time is defined to be 2:00 am on a particular date, then
!     {\tt ESMF\_TimeGet(h = hours, s = seconds)} would return
!       {\tt hours = 2}, {\tt seconds = 0},
!     whereas {\tt ESMF\_TimeGet(s=seconds)} would return
!       {\tt seconds = 7200}.
!
!     For {\tt timeString}, {\tt dayOfWeek}, {\tt dayOfMonth}, {\tt midMonth}, 
!     {\tt dayOfYear}, {\tt dayOfYear\_r8}, and {\tt dayOfYear\_intvl} described 
!     below, valid calendars are Gregorian, Julian Date, No Leap, 360 Day 
!     and Generic calendars.  Not valid for Julian day or no calendar.
!
!     For timeString, convert {\tt ESMF\_Time}'s value into ISO 8601
!     format YYYY-MM-DDThh:mm:ss.
!     
!     For dayOfWeek, get the day of the week the given {\tt ESMF\_Time}
!     instant falls on.  ISO 8601 standard:  Monday = 1 through Sunday = 7.
!
!     For dayOfMonth, get the day of the month the {\tt ESMF\_Time} instant
!     falls on (1-31).
!
!     For midMonth, get the middle time instant of the month the given
!     {\tt ESMF\_Time} instant falls on.
!
!     For dayOfYear, get the day of the year the given {\tt ESMF\_Time}
!     instant falls on (1-365).  Returned as an integer value.
!
!     For dayOfYear\_r8, get the day of the year the given {\tt ESMF\_Time}
!     instant falls on (1.x-365.x).  Returned as floating point value;
!     fractional part represents the time of day. 
!
!     For dayOfYear\_intvl, get the day of the year the given {\tt ESMF\_Time}
!     instant falls on (1-365).  Returned as an {\tt ESMF\_TimeInterval}.
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to query.
!     \item[{[yr]}]
!          Integer year CCYR (>= 32-bit).
!     \item[{[yr\_i8]}]
!          Integer year CCYR (large, >= 64-bit).
!     \item[{[mm]}]
!          Integer month 1-12.
!     \item[{[dd]}]
!          Integer day of the month 1-31.
!     \item[{[d]}]
!          Integer Julian days (>= 32-bit).
!     \item[{[d\_i8]}]
!          Integer Julian days (large, >= 64-bit).
!     \item[{[h]}]
!          Integer hours.
!     \item[{[m]}]
!          Integer minutes.
!     \item[{[s]}]
!          Integer seconds (>= 32-bit).
!     \item[{[s\_i8]}]
!          Integer seconds (large, >= 64-bit).
!     \item[{[ms]}]
!          Integer milliseconds.
!     \item[{[us]}]
!          Integer microseconds.
!     \item[{[ns]}]
!          Integer nanoseconds.
!     \item[{[d\_r8]}]
!          Double precision days.
!     \item[{[h\_r8]}]
!          Double precision hours.
!     \item[{[m\_r8]}]
!          Double precision minutes.
!     \item[{[s\_r8]}]
!          Double precision seconds.
!     \item[{[ms\_r8]}]
!          Double precision milliseconds.
!     \item[{[us\_r8]}]
!          Double precision microseconds.
!     \item[{[ns\_r8]}]
!          Double precision nanoseconds.
!     \item[{[sN]}]
!          Integer fractional seconds - numerator.
!     \item[{[sD]}]
!          Integer fractional seconds - denominator.
!     \item[{[calendar]}]
!          Associated {\tt Calendar}.
!     \item[{[timeZone]}]
!          Associated timezone (hours offset from GMT, e.g. EST = -5).
!     \item[{[timeString]}]
!          Convert time value to ISO 8601 format string YYYY-MM-DDThh:mm:ss.
!     \item[{[dayOfWeek]}]
!          Get the time instant's day of the week (1-7).
!     \item[{[dayOfMonth]}]
!          The time instant's day of the month (1-31).
!     \item[{[MidMonth]}]
!          The given time instant's middle-of-the-month time instant.
!     \item[{[dayOfYear]}]
!          The {\tt ESMF\_Time} instant's integer day of the year (1-365).
!     \item[{[dayOfYear\_r8]}]
!          The {\tt ESMF\_Time} instant's floating point day of the year
!          (1.x-365.x).
!     \item[{[dayOfYear\_intvl]}]
!          The {\tt ESMF\_Time} instant's day of the year (1-365) as an
!          {\tt ESMF\_TimeInterval}.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG2.1, TMG2.5.1, TMG2.5.6

      ! use optional args for any subset
      call c_ESMC_TimeGet(time, yr, yr_i8, mm, dd, d, d_i8, &
                          h, m, s, s_i8, ms, us, ns, &
                          d_r8, h_r8, m_r8, s_r8, ms_r8, us_r8, ns_r8, &
                          sN, sD, calendar, timeZone, timeString, dayOfWeek, &
                          dayOfMonth, MidMonth, dayOfYear, dayOfYear_r8, &
                          dayOfYear_intvl, rc)
    
      end subroutine ESMF_TimeGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIsSameCalendar - Compare Calendars of two Times

! !INTERFACE:
      function ESMF_TimeIsSameCalendar(time1, time2, rc)

! !RETURN VALUE:
      logical :: ESMF_TimeIsSameCalendar

! !ARGUMENTS:
      type(ESMF_Time), intent(in)            :: time1
      type(ESMF_Time), intent(in)            :: time2
      integer,         intent(out), optional :: rc

! !DESCRIPTION:
!     Returns true if the Calendars in these Times are
!     the same, false otherwise.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          The first Time to compare.
!     \item[time2]
!          The second Time to compare.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n

      call c_ESMC_TimeIsSameCalendar(time1, time2, ESMF_TimeIsSameCalendar, rc)
    
      end function ESMF_TimeIsSameCalendar

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeSyncToRealTime - Get system real time (wall clock time)
!
! !INTERFACE:
      subroutine ESMF_TimeSyncToRealTime(time, rc)
!
! !ARGUMENTS:
      type(ESMF_Time), intent(inout) :: time
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Gets the system real time (wall clock time), and returns it as an
!     {\tt ESMF\_Time}.
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to receive the real time.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG2.5.7

      call c_ESMC_TimeSyncToRealTime(time, rc)

      end subroutine ESMF_TimeSyncToRealTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeInc - Increment a Time with a TimeInterval
!
! !INTERFACE:
      function ESMF_TimeInc(time, timeInterval)
!
! !RETURN VALUE:
      type(ESMF_Time) :: ESMF_TimeInc
!
! !ARGUMENTS:
      type(ESMF_Time),         intent(in) :: time
      type(ESMF_TimeInterval), intent(in) :: timeInterval
!
! !DESCRIPTION:
!     Increments a {\tt time} with a {\tt timeInterval} and
!     returns the result as an {\tt ESMF\_Time}.
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The {\tt ESMF\_Time} to increment.
!     \item[timeInterval]
!          The {\tt ESMF\_TimeInterval} to add to the given {\tt ESMF\_Time}.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.5.4, TMG2.4.4, TMG2.4.5, TMG2.4.6, TMG5.1, TMG5.2, TMG7.2

      ! copy ESMF_Time specific properties (e.g. calendar, timezone) 
      ESMF_TimeInc = time

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeSum(time, timeInterval, ESMF_TimeInc)

      end function ESMF_TimeInc
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeDec - Decrement a Time with a TimeInterval
!
! !INTERFACE:
      function ESMF_TimeDec(time, timeInterval)
!
! !RETURN VALUE:
      type(ESMF_Time) :: ESMF_TimeDec
!
! !ARGUMENTS:
      type(ESMF_Time),         intent(in) :: time
      type(ESMF_TimeInterval), intent(in) :: timeInterval
!
! !DESCRIPTION:
!     Decrements a {\tt time} with a {\tt timeInterval},
!     and returns the result as an {\tt ESMF\_Time}.
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The {\tt ESMF\_Time} to decrement.
!     \item[timeInterval]
!          The {\tt ESMF\_TimeInterval} to subtract from the given
!          {\tt ESMF\_Time}.
!     \end{description}
!     
!EOP
! !REQUIREMENTS:
!     TMG1.5.4, TMG2.4.4, TMG2.4.5, TMG2.4.6, TMG5.1, TMG5.2, TMG7.2

      ! copy ESMF_Time specific properties (e.g. calendar, timezone) 
      ESMF_TimeDec = time

      ! call ESMC_BaseTime base class function
       call c_ESMC_BaseTimeDiff(time, timeInterval, ESMF_TimeDec)

      end function ESMF_TimeDec

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeDiff - Return the difference between two Times
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
!     Returns the difference between {\tt time1} and {\tt time2} as
!     an {\tt ESMF\_TimeInterval}.  It is assumed that {\tt time1} is
!     later than {\tt time2}; if not, the resulting {\tt ESMF\_TimeInterval} 
!     will have a negative value.  This method is overloaded with the
!     (-) operator.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          The first {\tt ESMF\_Time}.
!     \item[time2]
!          The second {\tt ESMF\_Time}.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.5.4, TMG2.4.4, TMG2.4.5, TMG2.4.6, TMG5.1, TMG5.2, TMG7.2

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeDiff(time1, time2, ESMF_TimeDiff)

      end function ESMF_TimeDiff

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeEQ - Time 1 equal to Time 2?
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
!     Returns true if {\tt time1} and {\tt time2} are equal, false
!     otherwise.  This method is overloaded with the (==) operator.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First time instant to compare.
!     \item[time2]
!          Second time instant to compare.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2

      ! invoke C to C++ entry point for ESMF_BaseTime base class function
      call c_ESMC_BaseTimeEQ(time1, time2, ESMF_TimeEQ)

      end function ESMF_TimeEQ

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeNE - Time 1 not equal to Time 2?
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
!     Returns true if {\tt time1} and {\tt time2} are not equal, false
!     otherwise.  This method is overloaded with the (!=) operator.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First time instant to compare.
!     \item[time2]
!          Second time instant to compare.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeNE(time1, time2, ESMF_TimeNE)

      end function ESMF_TimeNE

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeLT - Time 1 less than Time 2?
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
!     Returns true if {\tt time1} is less than {\tt time2}, false 
!     otherwise.  This method is overloaded with the (<) operator.  
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First time instant to compare.
!     \item[time2]
!          Second time instant to compare.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeLT(time1, time2, ESMF_TimeLT)

      end function ESMF_TimeLT

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeGT - Time 1 greater than Time 2?
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
!     Returns true if {\tt time1} is greater than {\tt time2}, false
!     otherwise.  This method is overloaded with the (>) operator.   
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First time instant to compare.
!     \item[time2]
!          Second time instant to compare.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeGT(time1, time2, ESMF_TimeGT)

      end function ESMF_TimeGT

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeLE - Time 1 less than or equal to Time 2?
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
!     Returns true if {\tt time1} is less than or equal to
!     {\tt time2}, false otherwise.  This method is overloaded with
!     the (<=) operator.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First time instant to compare.
!     \item[time2]
!          Second time instant to compare.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeLE(time1, time2, ESMF_TimeLE)

      end function ESMF_TimeLE

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeGE - Time 1 greater than or equal to Time 2?
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
!     Returns true if {\tt time1} is greater than or equal to
!     {\tt time2}, false otherwise.  This method is overloaded with
!     the (>=) operator.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First time instant to compare.
!     \item[time2]
!          Second time instant to compare.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2

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
! !IROUTINE:  ESMF_TimeReadRestart - Restore the contents of a Time

! !INTERFACE:
      subroutine ESMF_TimeReadRestart(time, s, sN, sD, calendar, timeZone, rc)

! !ARGUMENTS:
      type(ESMF_Time),        intent(out) :: time
      integer(ESMF_KIND_I8), intent(in)  :: s
      integer(ESMF_KIND_I4), intent(in)  :: sN
      integer(ESMF_KIND_I4), intent(in)  :: sD
      type(ESMF_Calendar),    intent(in)  :: calendar
      integer,                intent(in)  :: timeZone
      integer,                intent(out), optional :: rc

! !DESCRIPTION:
!     Restores the contents of an {\tt ESMF\_Time} for restart.
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          {\tt ESMF\_Time} instant to restore.
!     \item[s]
!          64-bit integer seconds.
!     \item[sN]
!          Integer fractional seconds - numerator.
!     \item[sD]
!          Integer fractional seconds - denominator.
!     \item[calendar]
!          Associated {\tt ESMF\_Calendar}.
!     \item[timeZone]
!          Associated timezone (hours offset from GMT, e.g. EST = -5).
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
   
      call c_ESMC_TimeReadRestart(time, s, sN, sD, calendar, timeZone, rc)

      end subroutine ESMF_TimeReadRestart

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeWriteRestart - Save the contents of a Time

! !INTERFACE:
      subroutine ESMF_TimeWriteRestart(time, s, sN, sD, calendar, timeZone, rc)

! !ARGUMENTS:
      type(ESMF_Time),        intent(in)  :: time
      integer(ESMF_KIND_I8), intent(out) :: s
      integer(ESMF_KIND_I4), intent(out) :: sN
      integer(ESMF_KIND_I4), intent(out) :: sD
      type(ESMF_Calendar),    intent(out) :: calendar
      integer,                intent(out) :: timeZone
      integer,                intent(out), optional :: rc

! !DESCRIPTION:
!     Saves the contents of an {\tt ESMF\_Time} for restart.
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          {\tt ESMF\_Time} instant to save.
!     \item[s]
!          64-bit integer seconds.
!     \item[sN]
!          Integer fractional seconds - numerator.
!     \item[sD]
!          Integer fractional seconds - denominator.
!     \item[calendar]
!          Associated {\tt ESMF\_Calendar}.
!     \item[timeZone]
!          Associated timezone (hours offset from GMT, e.g. EST = -5).
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
   
      call c_ESMC_TimeWriteRestart(time, s, sN, sD, calendar, timeZone, rc)

      end subroutine ESMF_TimeWriteRestart

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeValidate - Validate a Time

! !INTERFACE:
      subroutine ESMF_TimeValidate(time, options, rc)

! !ARGUMENTS:
      type(ESMF_Time),   intent(in)            :: time
      character (len=*), intent(in),  optional :: options
      integer,           intent(out), optional :: rc

! !DESCRIPTION:
!     Check whether a {\tt time} is valid.
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          {\tt ESMF\_Time} instant to validate.
!     \item[{[options]}]
!          Validation options.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
   
      call c_ESMC_TimeValidate(time, options, rc)

      end subroutine ESMF_TimeValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimePrint - Print the contents of a Time 

! !INTERFACE:
      subroutine ESMF_TimePrint(time, options, rc)

! !ARGUMENTS:
      type(ESMF_Time),   intent(in)            :: time
      character (len=*), intent(in),  optional :: options
      integer,           intent(out), optional :: rc

! !DESCRIPTION:
!     To support testing and debugging, this method prints out 
!     the contents of an {\tt ESMF\_Time}.
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          {\tt ESMF\_Time} instant to print out.
!     \item[{[options]}]
!          Print options.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
   
      call c_ESMC_TimePrint(time, options, rc)

      end subroutine ESMF_TimePrint

!------------------------------------------------------------------------------

      end module ESMF_TimeMod
