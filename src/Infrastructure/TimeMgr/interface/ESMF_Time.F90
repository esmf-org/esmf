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
!==============================================================================
#define ESMF_FILENAME "ESMF_Time.F90"
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
#include "ESMF.h"
#include "ESMF_TimeMgr.inc"

!==============================================================================
!BOPI
! !MODULE: ESMF_TimeMod
!
! !DESCRIPTION:
! Part of Time Manager Fortran API wrapper of C++ implementation.
!
! Defines Fortran wrapper entry points for corresponding
! C++ class {\tt ESMC\_Time} implementation.
!
! See {\tt ../include/ESMC\_Time.h} for complete description.
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from ESMF base class
      use ESMF_BaseMod
      use ESMF_UtilTypesMod
      use ESMF_InitMacrosMod
      use ESMF_LogErrMod
      use ESMF_IOUtilMod

      ! associated derived types
      use ESMF_TimeIntervalTypeMod
      use ESMF_CalendarMod

      ! type definition for this module
      use ESMF_TimeTypeMod

      implicit none
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMF_Time definition in ESMF_TimeTypeMod to resolve mutual
!     ! dependency with ESMF_TimeInterval

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Time
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:
      public operator(+)
      public operator(-)
      public operator(==)
      public operator(/=)
      public operator(<)
      public operator(<=)
      public operator(>)
      public operator(>=)
      public ESMF_TimeGet
      public ESMF_TimeIsLeapYear
      public ESMF_TimeIsSameCalendar
      public ESMF_TimePrint
      public ESMF_TimeReadRestart
      public ESMF_TimeSet
      public ESMF_TimeSyncToRealTime
      public ESMF_TimeValidate
      public ESMF_TimeWriteRestart

! - ESMF-internal methods:
      public ESMF_TimeGetInit
      public ESMF_TimeInit

!EOPI

! !PRIVATE MEMBER FUNCTIONS:
      private ESMF_TimeInc
      private ESMF_TimeDec
      private ESMF_TimeDiff
      private ESMF_TimeEQ
      private ESMF_TimeNE
      private ESMF_TimeLT
      private ESMF_TimeLE
      private ESMF_TimeGT
      private ESMF_TimeGE

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id$'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !IROUTINE:  ESMF_TimeAssignment(=) - Assign a Time to another Time
!
! !INTERFACE:
!     interface assignment(=)
!     time1 = time2
!
! !ARGUMENTS:
!     type(ESMF_Time) :: time1
!     type(ESMF_Time) :: time2
! 
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Set {\tt time1} equal to {\tt time2}.  This is the default Fortran
!     assignment, which creates a complete, independent copy of {\tt time2} 
!     as {\tt time1}.  If {\tt time2} is an invalid {\tt ESMF\_Time} object then
!     {\tt time1} will be equally invalid after the assignment.
!
!     The arguments are:
!     \begin{description} 
!     \item[time1] 
!          The {\tt ESMF\_Time} to be set.
!     \item[time2] 
!          The {\tt ESMF\_Time} to be copied.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
!     None, documentation only, to describe the behavior of the default 
!     Fortran assignment(=).
!
! !REQUIREMENTS:
!     API review 11/2010.
! 
!     end interface
! 
!------------------------------------------------------------------------------
!
!BOP
! !IROUTINE:  ESMF_TimeOperator(+) - Increment a Time by a TimeInterval
!
! !INTERFACE:
      interface operator(+)
!     time2 = time1 + timeinterval      
!
! !RETURN VALUE:   
!     type(ESMF_Time) :: time2
!
! !ARGUMENTS:
!     type(ESMF_Time),         intent(in) :: time1
!     type(ESMF_TimeInterval), intent(in) :: timeinterval
! 
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Overloads the (+) operator for the {\tt ESMF\_Time} class to increment
!     {\tt time1} with {\tt timeinterval} and return the result as an
!     {\tt ESMF\_Time}.
!
!     The arguments are:
!     \begin{description} 
!     \item[time1] 
!          The {\tt ESMF\_Time} to increment.
!     \item[timeinterval] 
!          The {\tt ESMF\_TimeInterval} to add to the given {\tt ESMF\_Time}.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeInc    ! internal implementation
!
! !REQUIREMENTS:
!     TMG2.4.4, TMG2.4.5, TMG5.1, TMG7.2
! 
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeOperator(-) - Decrement a Time by a TimeInterval
!
! !INTERFACE:
      interface operator(-)
!     time2 = time1 - timeinterval      
! 
! !RETURN VALUE:
!     type(ESMF_Time) :: time2
! 
! !ARGUMENTS:
!     type(ESMF_Time),         intent(in) :: time1
!     type(ESMF_TimeInterval), intent(in) :: timeinterval
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Overloads the (-) operator for the {\tt ESMF\_Time} class to decrement
!     {\tt time1} with {\tt timeinterval}, and return the result as an
!     {\tt ESMF\_Time}.
! 
!     The arguments are:      
!     \begin{description}
!     \item[time1]
!          The {\tt ESMF\_Time} to decrement.
!     \item[timeinterval]
!          The {\tt ESMF\_TimeInterval} to subtract from the given
!          {\tt ESMF\_Time}.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeDec    ! internal implementation
!
! !REQUIREMENTS:
!     TMG2.4.4, TMG2.4.5, TMG5.1, TMG7.2
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeOperator(-) - Return the difference between two Times
!
! !INTERFACE:
!     interface operator(-)
!     timeinterval = time1 - time2      
!
! !RETURN VALUE:
!     type(ESMF_TimeInterval) :: timeinterval
! 
! !ARGUMENTS:
!     type(ESMF_Time),         intent(in) :: time1
!     type(ESMF_Time),         intent(in) :: time2
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Overloads the (-) operator for the {\tt ESMF\_Time} class to return the
!     difference between {\tt time1} and {\tt time2} as an
!     {\tt ESMF\_TimeInterval}.  It is assumed that {\tt time1} is later than
!     {\tt time2}; if not, the resulting {\tt ESMF\_TimeInterval} will have a
!     negative value.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          The first {\tt ESMF\_Time} in comparison.
!     \item[time2]
!          The second {\tt ESMF\_Time} in comparison.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeDiff   ! internal implementation
!
! !REQUIREMENTS:
!     TMG2.4.6, TMG5.2, TMG7.2
!
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeOperator(==) - Test if Time 1 is equal to Time 2
!
! !INTERFACE:
      interface operator(==)
!     if (time1 == time2) then ... endif
!                  OR
!     result = (time1 == time2)
!
! !RETURN VALUE:
!     logical :: result
!
! !ARGUMENTS:
!     type(ESMF_Time), intent(in) :: time1
!     type(ESMF_Time), intent(in) :: time2
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Overloads the (==) operator for the {\tt ESMF\_Time} class to return 
!     {\tt .true.} if {\tt time1} and {\tt time2} represent the same instant 
!     in time, and {\tt .false.} otherwise.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First {\tt ESMF\_Time} in comparison.
!     \item[time2]
!          Second {\tt ESMF\_Time} in comparison.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeEQ   ! internal implementation
!
! !REQUIREMENTS:
!     TMG2.4.3, TMG7.2
!
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeOperator(/=) - Test if Time 1 is not equal to Time 2
!
! !INTERFACE:
      interface operator(/=)
!     if (time1 /= time2) then ... endif
!                  OR
!     result = (time1 /= time2)
!
! !RETURN VALUE:
!     logical :: result
!
! !ARGUMENTS:
!     type(ESMF_Time), intent(in) :: time1
!     type(ESMF_Time), intent(in) :: time2
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Overloads the (/=) operator for the {\tt ESMF\_Time} class to return 
!     {\tt .true.} if {\tt time1} and {\tt time2} do not represent the same 
!     instant in time, and {\tt .false.} otherwise.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First {\tt ESMF\_Time} in comparison.
!     \item[time2]
!          Second {\tt ESMF\_Time} in comparison.
!     \end{description}
! 
!EOP
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeNE   ! internal implementation
!
! !REQUIREMENTS:
!     TMG2.4.3, TMG7.2
!
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeOperator(<) - Test if Time 1 is less than Time 2
!
! !INTERFACE:
      interface operator(<)
!     if (time1 < time2) then ... endif
!                  OR
!     result = (time1 < time2)
!
! !RETURN VALUE:
!     logical :: result
!
! !ARGUMENTS:
!     type(ESMF_Time), intent(in) :: time1
!     type(ESMF_Time), intent(in) :: time2
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Overloads the (<) operator for the {\tt ESMF\_Time} class to return 
!     {\tt .true.} if {\tt time1} is earlier in time than {\tt time2}, and 
!     {\tt .false.} otherwise.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First {\tt ESMF\_Time} in comparison.
!     \item[time2]
!          Second {\tt ESMF\_Time} in comparison.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeLT   ! internal implementation
!
! !REQUIREMENTS:
!     TMG2.4.3, TMG7.2
!
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeOperator(<=) - Test if Time 1 is less than or equal to Time 2
!
! !INTERFACE:
      interface operator(<=)
!     if (time1 <= time2) then ... endif
!                  OR
!     result = (time1 <= time2)
!
! !RETURN VALUE:
!     logical :: result
!
! !ARGUMENTS:
!     type(ESMF_Time), intent(in) :: time1
!     type(ESMF_Time), intent(in) :: time2
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Overloads the (<=) operator for the {\tt ESMF\_Time} class to return 
!     {\tt .true.} if {\tt time1} is earlier in time or the same time as 
!     {\tt time2}, and {\tt .false.} otherwise.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First {\tt ESMF\_Time} in comparison.
!     \item[time2]
!          Second {\tt ESMF\_Time} in comparison.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeLE   ! internal implementation
!
! !REQUIREMENTS:
!     TMG2.4.3, TMG7.2
!
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeOperator(>) - Test if Time 1 is greater than Time 2
!
! !INTERFACE:
      interface operator(>)
!     if (time1 > time2) then ... endif
!                  OR
!     result = (time1 > time2)
!
! !RETURN VALUE:
!     logical :: result
!
! !ARGUMENTS:
!     type(ESMF_Time), intent(in) :: time1
!     type(ESMF_Time), intent(in) :: time2
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Overloads the (>) operator for the {\tt ESMF\_Time} class to return 
!     {\tt .true.} if {\tt time1} is later in time than {\tt time2}, and 
!     {\tt .false.} otherwise.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First {\tt ESMF\_Time} in comparison.
!     \item[time2]
!          Second {\tt ESMF\_Time} in comparison.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeGT   ! internal implementation
!
! !REQUIREMENTS:
!     TMG2.4.3, TMG7.2
!
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeOperator(>=) - Test if Time 1 is greater than or equal to Time 2
!
! !INTERFACE:
      interface operator(>=)
!     if (time1 >= time2) then ... endif
!                  OR
!     result = (time1 >= time2)
!
! !RETURN VALUE:
!     logical :: result
!
! !ARGUMENTS:
!     type(ESMF_Time), intent(in) :: time1
!     type(ESMF_Time), intent(in) :: time2
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Overloads the (>=) operator for the {\tt ESMF\_Time} class to return 
!     {\tt .true.} if {\tt time1} is later in time or the same time as 
!     {\tt time2}, and {\tt .false.} otherwise.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First {\tt ESMF\_Time} in comparison.
!     \item[time2]
!          Second {\tt ESMF\_Time} in comparison.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeGE   ! internal implementation
!
! !REQUIREMENTS:
!     TMG2.4.3, TMG7.2
!
      end interface
!
!==============================================================================

      contains

!==============================================================================
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeGet()"
!BOP
! !IROUTINE: ESMF_TimeGet - Get a Time value 

! !INTERFACE:
      subroutine ESMF_TimeGet(time, keywordEnforcer, &
        yy, yy_i8, &
        mm, dd, &
        d, d_i8, &
        h, m, &
        s, s_i8, &
        ms, us, ns, &
        d_r8, h_r8, m_r8, s_r8, &
        ms_r8, us_r8, ns_r8, &
        sN, sN_i8, sD, sD_i8, &
        calendar, calkindflag, timeZone, &
        timeString, timeStringISOFrac, &
        dayOfWeek, midMonth, &
        dayOfYear,  dayOfYear_r8, &
        dayOfYear_intvl, rc)

! !ARGUMENTS:
      type(ESMF_Time),         intent(in)            :: time
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer(ESMF_KIND_I4),   intent(out), optional :: yy
      integer(ESMF_KIND_I8),   intent(out), optional :: yy_i8
      integer,                 intent(out), optional :: mm
      integer,                 intent(out), optional :: dd
      integer(ESMF_KIND_I4),   intent(out), optional :: d
      integer(ESMF_KIND_I8),   intent(out), optional :: d_i8
      integer(ESMF_KIND_I4),   intent(out), optional :: h
      integer(ESMF_KIND_I4),   intent(out), optional :: m
      integer(ESMF_KIND_I4),   intent(out), optional :: s
      integer(ESMF_KIND_I8),   intent(out), optional :: s_i8
      integer(ESMF_KIND_I4),   intent(out), optional :: ms
      integer(ESMF_KIND_I4),   intent(out), optional :: us
      integer(ESMF_KIND_I4),   intent(out), optional :: ns
      real(ESMF_KIND_R8),      intent(out), optional :: d_r8
      real(ESMF_KIND_R8),      intent(out), optional :: h_r8
      real(ESMF_KIND_R8),      intent(out), optional :: m_r8
      real(ESMF_KIND_R8),      intent(out), optional :: s_r8
      real(ESMF_KIND_R8),      intent(out), optional :: ms_r8
      real(ESMF_KIND_R8),      intent(out), optional :: us_r8
      real(ESMF_KIND_R8),      intent(out), optional :: ns_r8
      integer(ESMF_KIND_I4),   intent(out), optional :: sN
      integer(ESMF_KIND_I8),   intent(out), optional :: sN_i8
      integer(ESMF_KIND_I4),   intent(out), optional :: sD
      integer(ESMF_KIND_I8),   intent(out), optional :: sD_i8
      type(ESMF_Calendar),     intent(out), optional :: calendar
      type(ESMF_CalKind_Flag), intent(out), optional :: calkindflag
      integer,                 intent(out), optional :: timeZone ! not imp
      character (len=*),       intent(out), optional :: timeString
      character (len=*),       intent(out), optional :: timeStringISOFrac
      integer,                 intent(out), optional :: dayOfWeek
      type(ESMF_Time),         intent(out), optional :: midMonth
      integer(ESMF_KIND_I4),   intent(out), optional :: dayOfYear
      real(ESMF_KIND_R8),      intent(out), optional :: dayOfYear_r8
      type(ESMF_TimeInterval), intent(out), optional :: dayOfYear_intvl
      integer,                 intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Gets the value of {\tt time} in units specified by the user
!     via Fortran optional arguments.  See {\tt ESMF\_TimeSet()} above for a
!     description of time units and calendars.
!
!     The ESMF Time Manager represents and manipulates time internally with 
!     integers to maintain precision.  Hence, user-specified floating point 
!     values are converted internally from integers.  For example, if a time
!     value is 5 and 3/8 seconds (s=5, sN=3, sD=8), and you want to get it as
!     floating point seconds, you would get 5.375 (s\_r8=5.375).
!
!     Units are bound (normalized) by the next larger unit specified.  For
!     example, if a time is defined to be 2:00 am on February 2, 2004, then
!     {\tt ESMF\_TimeGet(dd=day, h=hours, s=seconds)} would return
!       {\tt day = 2}, {\tt hours = 2}, {\tt seconds = 0},
!     whereas {\tt ESMF\_TimeGet(dd = day, s=seconds)} would return
!       {\tt day = 2}, {\tt seconds = 7200}.
!     Note that {\tt hours} and {\tt seconds} are bound by a day.  If bound
!     by a month,
!     {\tt ESMF\_TimeGet(mm=month, h=hours, s=seconds)} would return
!       {\tt month = 2}, {\tt hours = 26}, {\tt seconds = 0},
!     and {\tt ESMF\_TimeGet(mm = month, s=seconds)} would return
!       {\tt month = 2}, {\tt seconds = 93600} (26 * 3600).
!     Similarly, if bound to a year,
!     {\tt ESMF\_TimeGet(yy=year, h=hours, s=seconds)} would return
!       {\tt year = 2004}, {\tt hours = 770} (32*24 + 2), {\tt seconds = 0},
!     and {\tt ESMF\_TimeGet(yy = year, s=seconds)} would return
!       {\tt year = 2004}, {\tt seconds = 2772000} (770 * 3600).
!
!     For {\tt timeString}, {\tt timeStringISOFrac}, {\tt dayOfWeek},
!     {\tt midMonth}, {\tt dayOfYear}, {\tt dayOfYear\_intvl}, and
!     {\tt dayOfYear\_r8} described below, valid calendars are Gregorian,
!     Julian, No Leap, 360 Day and Custom calendars.  Not valid for
!     Julian Day, Modified Julian Day, or No Calendar. \\
!
!     For {\tt timeString} and {\tt timeStringISOFrac}, YYYY format returns
!     at least 4 digits; years <= 999 are padded on the left with zeroes and
!     years >= 10000 return the number of digits required.
!
!     For timeString, convert {\tt ESMF\_Time}'s value into partial ISO 8601
!     format YYYY-MM-DDThh:mm:ss[:n/d].  See ~\cite{ISO} and ~\cite{ISOnotes}.
!     See also method {\tt ESMF\_TimePrint()}.
!     
!     For timeStringISOFrac, convert {\tt ESMF\_Time}'s value into full ISO 8601
!     format YYYY-MM-DDThh:mm:ss[.f].  See ~\cite{ISO} and ~\cite{ISOnotes}.
!     See also method {\tt ESMF\_TimePrint()}.
!     
!     For dayOfWeek, gets the day of the week the given {\tt ESMF\_Time}
!     instant falls on.  ISO 8601 standard:  Monday = 1 through Sunday = 7.
!     See ~\cite{ISO} and ~\cite{ISOnotes}.
!
!     For midMonth, gets the middle time instant of the month that the given
!     {\tt ESMF\_Time} instant falls on.
!
!     For dayOfYear, gets the day of the year that the given {\tt ESMF\_Time}
!     instant falls on.  See range discussion in argument list below.
!     Return as an integer value.
!
!     For dayOfYear\_r8, gets the day of the year the given {\tt ESMF\_Time}
!     instant falls on.  See range discussion in argument list below.
!     Return as floating point value; fractional part represents the time of
!     day.
!
!     \begin{sloppypar}
!     For dayOfYear\_intvl, gets the day of the year the given {\tt ESMF\_Time}
!     instant falls on.  Return as an {\tt ESMF\_TimeInterval}.
!     \end{sloppypar}
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to query.
!     \item[{[yy]}]
!          Integer year (32-bit).
!     \item[{[yy\_i8]}]
!          Integer year (large, 64-bit).
!     \item[{[mm]}]
!          Integer month.
!     \item[{[dd]}]
!          Integer day of the month.
!     \item[{[d]}]
!          Integer Julian date, or Modified Julian date (32-bit).
!     \item[{[d\_i8]}]
!          Integer Julian date, or Modified Julian date (large, 64-bit).
!     \item[{[h]}]
!          Integer hour.
!     \item[{[m]}]
!          Integer minute.
!     \item[{[s]}]
!          Integer second (32-bit).
!     \item[{[s\_i8]}]
!          Integer second (large, 64-bit).
!     \item[{[ms]}]
!          Integer millisecond.
!     \item[{[us]}]
!          Integer microsecond.
!     \item[{[ns]}]
!          Integer nanosecond.
!     \item[{[d\_r8]}]
!          Double precision day.
!     \item[{[h\_r8]}]
!          Double precision hour.
!     \item[{[m\_r8]}]
!          Double precision minute.
!     \item[{[s\_r8]}]
!          Double precision second.
!     \item[{[ms\_r8]}]
!          Double precision millisecond.
!     \item[{[us\_r8]}]
!          Double precision microsecond.
!     \item[{[ns\_r8]}]
!          Double precision nanosecond.
!     \item[{[sN]}]
!          Integer numerator of fractional second (sN/sD).
!     \item[{[sN\_i8]}]
!          Integer numerator of fractional second (sN\_i8/sD\_i8)
!                                                           (large, <= 64-bit).
!     \item[{[sD]}]
!          Integer denominator of fractional second (sN/sD).
!     \item[{[sD\_i8]}]
!          Integer denominator of fractional second (sN\_i8/sD\_i8)
!                                                           (large, <= 64-bit).
!     \item[{[calendar]}]
!          Associated {\tt Calendar}.
!     \item[{[calkindflag]}]
!          Associated {\tt CalKind\_Flag}.
!     \item[{[timeZone]}]
!          Associated timezone (hours offset from UCT, e.g. EST = -5).
!          (Not implemented yet).
!     \item[{[timeString]}]
!          \begin{sloppypar}
!          Convert time value to format string YYYY-MM-DDThh:mm:ss[:n/d],
!          where n/d is numerator/denominator of any fractional seconds and
!          all other units are in ISO 8601 format.  See ~\cite{ISO} and
!          ~\cite{ISOnotes}.  See also method {\tt ESMF\_TimePrint()}.
!          \end{sloppypar}
!     \item[{[timeStringISOFrac]}]
!          Convert time value to strict ISO 8601 format string
!          YYYY-MM-DDThh:mm:ss[.f], where f is decimal form of any fractional
!          seconds.  See ~\cite{ISO} and ~\cite{ISOnotes}.  See also method
!          {\tt ESMF\_TimePrint()}.
!     \item[{[dayOfWeek]}]
!          The time instant's day of the week [1-7].
!     \item[{[MidMonth]}]
!          The given time instant's middle-of-the-month time instant.
!     \item[{[dayOfYear]}]
!          The {\tt ESMF\_Time} instant's integer day of the year.
!          [1-366] for Gregorian and Julian calendars, [1-365] for No-Leap
!          calendar.  [1-360] for 360-Day calendar.  User-defined range
!          for Custom calendar.
!     \item[{[dayOfYear\_r8]}]
!          The {\tt ESMF\_Time} instant's floating point day of the year.
!          [1.x-366.x] for Gregorian and Julian calendars, [1.x-365.x] for
!          No-Leap calendar.  [1.x-360.x] for 360-Day calendar.  User-defined
!          range for Custom calendar.
!     \item[{[dayOfYear\_intvl]}]
!          The {\tt ESMF\_Time} instant's day of the year as an
!          {\tt ESMF\_TimeInterval}.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG2.1, TMG2.5.1, TMG2.5.6

      ! temp time string for C++ to fill
      character (len=ESMF_MAXSTR) :: tempTimeString, tempTimeStringISOFrac

      ! initialize time string lengths to zero for non-existent time string
      integer :: timeStringLen, timeStringLenISOFrac
      integer :: tempTimeStringLen, tempTimeStringLenISOFrac
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check inputs
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,time,rc)

      timeStringLen = 0     
      timeStringLenISOFrac = 0     
      tempTimeStringLen = 0
      tempTimeStringLenISOFrac = 0

      ! if used, get length of given timeString for C++ validation
      if (present(timeString)) then
        timeStringLen = len(timeString)
      end if
      if (present(timeStringISOFrac)) then
        timeStringLenISOFrac = len(timeStringISOFrac)
      end if

      ! use optional args for any subset
      call c_ESMC_TimeGet(time, yy, yy_i8, mm, dd, d, d_i8, &
                          h, m, s, s_i8, ms, us, ns, &
                          d_r8, h_r8, m_r8, s_r8, ms_r8, us_r8, ns_r8, &
                          sN, sN_i8, sD, sD_i8, &
                          calendar, calkindflag, timeZone, &
                          timeStringLen, tempTimeStringLen, tempTimeString, &
                          timeStringLenISOFrac, tempTimeStringLenISOFrac, &
                          tempTimeStringISOFrac, &
                          dayOfWeek, MidMonth, dayOfYear, dayOfYear_r8, &
                          dayOfYear_intvl, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! copy temp time string back to given time string to restore
      !   native Fortran storage style
      if (present(timeString)) then
        timeString = tempTimeString(1:tempTimeStringLen)
      endif
      if (present(timeStringISOFrac)) then
        timeStringISOFrac = tempTimeStringISOFrac(1:tempTimeStringLenISOFrac)
      endif

      ! mark outputs as successfully initialized
      call ESMF_TimeInit(midMonth)
      call ESMF_TimeIntervalInit(dayOfYear_intvl)
      call ESMF_CalendarSetInitCreated(calendar)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_TimeGet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIsLeapYear()"
!BOP
! !IROUTINE: ESMF_TimeIsLeapYear - Determine if a Time is in a leap year

! !INTERFACE:
      function ESMF_TimeIsLeapYear(time, keywordEnforcer, rc)

! !RETURN VALUE:
      logical :: ESMF_TimeIsLeapYear

! !ARGUMENTS:
      type(ESMF_Time), intent(in)            :: time
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,         intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Returns {\tt .true.} if given time is in a leap year, and {\tt .false.}
!     otherwise.  See also {\tt ESMF\_CalendarIsLeapYear()}.
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The {\tt ESMF\_Time} to check for leap year.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! Initialize output value in case of error
      ESMF_TimeIsLeapYear = .false.

      ! check input
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,time,rc)

      ! invoke C to C++ entry point
      call c_ESMC_TimeIsLeapYear(time, ESMF_TimeIsLeapYear, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_TimeIsLeapYear

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIsSameCalendar()"
!BOP
! !IROUTINE: ESMF_TimeIsSameCalendar - Compare Calendars of two Times

! !INTERFACE:
      function ESMF_TimeIsSameCalendar(time1, time2, keywordEnforcer, rc)

! !RETURN VALUE:
      logical :: ESMF_TimeIsSameCalendar

! !ARGUMENTS:
      type(ESMF_Time), intent(in)            :: time1
      type(ESMF_Time), intent(in)            :: time2
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,         intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Returns {\tt .true.} if the Calendars in these Times are
!     the same, {\tt .false.} otherwise.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          The first {\tt ESMF\_Time} in comparison.
!     \item[time2]
!          The second {\tt ESMF\_Time} in comparison.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! Initialize output value in case of error
      ESMF_TimeIsSameCalendar = .false.

      ! check inputs
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,time1,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,time2,rc)

      ! invoke C to C++ entry point
      call c_ESMC_TimeIsSameCalendar(time1, time2, ESMF_TimeIsSameCalendar, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS    
      end function ESMF_TimeIsSameCalendar

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimePrint()"
!BOP
! !IROUTINE:  ESMF_TimePrint - Print Time information

! !INTERFACE:
      subroutine ESMF_TimePrint(time, keywordEnforcer, options, preString, unit, rc)

! !ARGUMENTS:
      type(ESMF_Time),   intent(in)            :: time
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      character (len=*), intent(in),  optional :: options
      character(*),      intent(in),  optional :: preString
      character(*),      intent(out), optional :: unit
      integer,           intent(out), optional :: rc

!
! !DESCRIPTION:
!     Prints out the contents of an {\tt ESMF\_Time} to {\tt stdout}, in
!     support of testing and debugging.  The options control the type of
!     information and level of detail.  For options "string" and "string
!     isofrac", YYYY format returns at least 4 digits; years <= 999 are
!     padded on the left with zeroes and years >= 10000 return the number
!     of digits required. \\
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The {\tt ESMF\_Time} to be printed out.
!     \item[{[options]}]
!          Print options. If none specified, prints all Time property values. \\
!          "string" - prints {\tt time}'s value in ISO 8601 format for all units
!                     through seconds.  For any non-zero fractional seconds,
!                     prints in integer rational fraction form n/d.  Format is
!                     YYYY-MM-DDThh:mm:ss[:n/d], where [:n/d] is the 
!                     integer numerator and denominator of the fractional
!                     seconds value, if present.  See ~\cite{ISO} and
!                     ~\cite{ISOnotes}.  See also method
!                     {\tt ESMF\_TimeGet(..., timeString= , ...)} \\
!          "string isofrac" - prints {\tt time}'s value in strict ISO 8601
!                     format for all units, including any fractional seconds
!                     part.  Format is YYYY-MM-DDThh:mm:ss[.f] where [.f]
!                     represents fractional seconds in decimal form, if present.
!                     See ~\cite{ISO} and ~\cite{ISOnotes}.  See also method
!                     {\tt ESMF\_TimeGet(..., timeStringISOFrac= , ...)} \\
!     \item[{[preString]}]
!          Optionally prepended string. Default to empty string.
!     \item[{[unit]}]
!          Internal unit, i.e. a string. Default to printing to stdout.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
      integer :: localrc                        ! local return code
      integer :: yy, mm, dd, h, m, s, ms
      logical :: underscore

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL
   
      ! check input
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,time,rc)

      underscore = .false.
      if (present(options)) then
        if (trim(options)=="underscore") underscore = .true.
      endif

      if (present(unit).or.present(preString)) then
        ! simple, single line print format
        call ESMF_TimeGet(time, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, ms=ms, &
          rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        if (present(unit)) then
          if (underscore) then
            if (present(preString)) then
              write (unit, "(A, '_', I4.4, '_', I2.2, '_', I2.2, '_', I2.2, "//&
                "'_', I2.2, '_', I2.2, '_', I3.3)") preString, &
                yy, mm, dd, h, m, s, ms
            else
              write (unit, "(I4.4, '_', I2.2, '_', I2.2, '_', I2.2, "//&
                "'_', I2.2, '_', I2.2, '_', I3.3)") &
                yy, mm, dd, h, m, s, ms
            endif
          else
            if (present(preString)) then
              write (unit, "(A, I4, I3, I3, I3, I3, I3, I4)") preString, &
                yy, mm, dd, h, m, s, ms
            else
              write (unit, "(I4, I3, I3, I3, I3, I3, I4)") &
                yy, mm, dd, h, m, s, ms
            endif
          endif
        else
          if (present(preString)) then
            write (*, "(A, I4, I3, I3, I3, I3, I3, I4)") preString, &
              yy, mm, dd, h, m, s, ms
          else
            ! cannot really reach this branch -> cover this by the deeper
            ! implementation in the bigger else block below.
            write (*, "(I4, I3, I3, I3, I3, I3, I4)") &
              yy, mm, dd, h, m, s, ms
          endif
        endif        
      else
        ! print to STDOUT
        
        ! invoke C to C++ entry point
        call ESMF_UtilIOUnitFlush (ESMF_UtilIOStdout, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

        call c_ESMC_TimePrint(time, options, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      endif

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_TimePrint

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeReadRestart()"
!BOPI
! !IROUTINE:  ESMF_TimeReadRestart - Restore the contents of a Time (not implemented)

! !INTERFACE:
      subroutine ESMF_TimeReadRestart(time, name, keywordEnforcer, rc)
!
! !ARGUMENTS:
      type(ESMF_Time),   intent(inout)         :: time
      character (len=*), intent(in)            :: name
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,           intent(out), optional :: rc

! !DESCRIPTION:
!     Restores an {\tt ESMF\_Time} object from the last call to
!     {\tt ESMF\_TimeWriteRestart()}.  (Not implemented yet).
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          Restore into this {\tt ESMF\_Time}.
!     \item[name]
!          Restore from this object name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:
!     TMGn.n.n

      ! get length of given name for C++ validation
      integer :: nameLen, localrc

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      nameLen = len_trim(name)
   
      ! invoke C to C++ entry point to restore time
      call c_ESMC_TimeReadRestart(time, nameLen, name, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! mark output as successfully initialized
      call ESMF_TimeInit(time)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_TimeReadRestart

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeSet()"
!BOP
! !IROUTINE: ESMF_TimeSet - Initialize or set a Time

! !INTERFACE:
      subroutine ESMF_TimeSet(time, keywordEnforcer, &
        yy, yy_i8, &
        mm, dd, &
        d, d_i8, &
        h, m, &
        s, s_i8, &
        ms, us, ns, &
        d_r8, h_r8, m_r8, s_r8, &
        ms_r8, us_r8, ns_r8, &
        sN, sN_i8, sD, sD_i8, &
        calendar, calkindflag, &
        timeZone, rc)

! !ARGUMENTS:
      type(ESMF_Time),         intent(inout)         :: time
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer(ESMF_KIND_I4),   intent(in),  optional :: yy
      integer(ESMF_KIND_I8),   intent(in),  optional :: yy_i8
      integer,                 intent(in),  optional :: mm
      integer,                 intent(in),  optional :: dd
      integer(ESMF_KIND_I4),   intent(in),  optional :: d
      integer(ESMF_KIND_I8),   intent(in),  optional :: d_i8
      integer(ESMF_KIND_I4),   intent(in),  optional :: h
      integer(ESMF_KIND_I4),   intent(in),  optional :: m
      integer(ESMF_KIND_I4),   intent(in),  optional :: s
      integer(ESMF_KIND_I8),   intent(in),  optional :: s_i8
      integer(ESMF_KIND_I4),   intent(in),  optional :: ms
      integer(ESMF_KIND_I4),   intent(in),  optional :: us
      integer(ESMF_KIND_I4),   intent(in),  optional :: ns
      real(ESMF_KIND_R8),      intent(in),  optional :: d_r8
      real(ESMF_KIND_R8),      intent(in),  optional :: h_r8
      real(ESMF_KIND_R8),      intent(in),  optional :: m_r8
      real(ESMF_KIND_R8),      intent(in),  optional :: s_r8
      real(ESMF_KIND_R8),      intent(in),  optional :: ms_r8
      real(ESMF_KIND_R8),      intent(in),  optional :: us_r8
      real(ESMF_KIND_R8),      intent(in),  optional :: ns_r8
      integer(ESMF_KIND_I4),   intent(in),  optional :: sN
      integer(ESMF_KIND_I8),   intent(in),  optional :: sN_i8
      integer(ESMF_KIND_I4),   intent(in),  optional :: sD
      integer(ESMF_KIND_I8),   intent(in),  optional :: sD_i8
      type(ESMF_Calendar),     intent(in),  optional :: calendar
      type(ESMF_CalKind_Flag), intent(in),  optional :: calkindflag
      integer,                 intent(in),  optional :: timeZone ! not imp
      integer,                 intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Initializes an {\tt ESMF\_Time} with a set of user-specified units
!     via Fortran optional arguments.
!
!     The range of valid values for mm and dd depend on the calendar used.
!     For Gregorian, Julian, and No-Leap calendars, mm is [1-12] and dd is
!     [1-28,29,30, or 31], depending on the value of mm and whether yy or
!     yy\_i8 is a leap year.  For the 360-day calendar, mm is [1-12] and dd is
!     [1-30].  For Julian Day, Modified Julian Day, and No-Calendar,
!     yy, yy\_i8, mm, and dd are invalid inputs, since these calendars do not
!     define them.  When valid, the yy and yy\_i8 arguments should be fully
!     specified, e.g. 2003 instead of 03.  yy and yy\_i8 ranges are only
!     limited by machine word size, except for the Gregorian and Julian
!     calendars, where the lowest (proleptic) date limits are 3/1/-4800 and
!     3/1/-4712, respectively.  This is a limitation of the Gregorian
!     date-to-Julian day and Julian date-to-Julian day conversion algorithms
!     used to convert Gregorian and Julian dates to the internal representation
!     of seconds.  See~\cite{Fli68} for a description of the Gregorian
!     date-to-Julian day algorithm and~\cite{Hat84} for a description of the
!     Julian date-to-Julian day algorithm.  The Custom calendar will have
!     user-defined values for yy, yy\_i8, mm, and dd.
!
!     The Julian day specifier, d or d\_i8, can only be used with the
!     Julian Day and Modified Julian Day calendars, and has a valid range
!     depending on the word size.  For a signed 32-bit d, the range for
!     Julian day is [+/- 24855].  For a signed 64-bit d\_i8, the valid
!     range for Julian day is [+/- 106,751,991,167,300].  The Julian day
!     number system adheres to the conventional standard where the reference
!     day of d=0 corresponds to 11/24/-4713 in the proleptic Gregorian calendar
!     and 1/1/-4712 in the proleptic Julian calendar.  See~\cite{Meyer2} and
!     ~\cite{JDNcalculator}.
!
!     The Modified Julian Day system, introduced by space scientists in the late
!     1950's, is defined as Julian Day - 2400000.5.  See~\cite{MJD}.
!
!     Note that d and d\_i8 are not valid for the No-Calendar.  To remain
!     consistent with non-Earth calendars added to ESMF in the future, ESMF
!     requires a calendar to be planet-specific.  Hence the No-Calendar does
!     not know what a day is; it cannot assume an Earth day of 86400 seconds.
!
!     Hours, minutes, seconds, and sub-seconds can be used with any calendar,
!     since they are standardized units that are the same for any planet.
!
!     Time manager represents and manipulates time internally with integers
!     to maintain precision. Hence, user-specified floating point values are
!     converted internally to integers.  Sub-second values are represented
!     internally with an integer numerator and denominator fraction (sN/sD).
!     The smallest required resolution is nanoseconds (denominator).
!     For example, pi can be represented as s=3,
!     sN=141592654, sD=1000000000.  However, via sN\_i8 and sD\_i8, larger
!     values can be used.  If specifying a constant floating point value, be
!     sure to provide at least 16 digits to take full advantage of double
!     precision, for example s\_r8=2.718281828459045d0 for 'e' seconds.
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to initialize.
!     \item[{[yy]}]
!          Integer year (32-bit).  Default = 0.
!     \item[{[yy\_i8]}]
!          Integer year (large, 64-bit).  Default = 0.
!     \item[{[mm]}]
!          Integer month.  Default = 1.
!     \item[{[dd]}]
!          Integer day of the month.  Default = 1.
!     \item[{[d]}]
!          Integer Julian Day, or Modified Julian Day (32-bit).  Must not be
!          specified with Gregorian calendars.  Default = 0.
!     \item[{[d\_i8]}]
!          Integer Julian Day, or Modified Julian Day (large, 64-bit).  Must not be
!          specified with Gregorian calendars.  Default = 0.
!     \item[{[h]}]
!          Integer hour.  Default = 0.
!     \item[{[m]}]
!          Integer minute.  Default = 0.
!     \item[{[s]}]
!          Integer second (32-bit).  Default = 0.
!     \item[{[s\_i8]}]
!          Integer second (large, 64-bit).  Default = 0.
!     \item[{[ms]}]
!          Integer millisecond.  Default = 0.
!     \item[{[us]}]
!          Integer microsecond.  Default = 0.
!     \item[{[ns]}]
!          Integer nanosecond.  Default = 0.
!     \item[{[d\_r8]}]
!          Double precision day.  Default = 0.0.
!     \item[{[h\_r8]}]
!          Double precision hour.  Default = 0.0.
!     \item[{[m\_r8]}]
!          Double precision minute.  Default = 0.0.
!     \item[{[s\_r8]}]
!          Double precision second.  Default = 0.0.
!     \item[{[ms\_r8]}]
!          Double precision millisecond.  Default = 0.0.
!     \item[{[us\_r8]}]
!          Double precision microsecond.  Default = 0.0.
!     \item[{[ns\_r8]}]
!          Double precision nanosecond.  Default = 0.0.
!     \item[{[sN]}]
!          Integer numerator of fractional second (sN/sD).
!          Default = 0.
!     \item[{[sN\_i8]}]
!          Integer numerator of fractional second (sN\_i8/sD\_i8)
!                                                           (large, 64-bit).
!          Default = 0.
!     \item[{[sD]}]
!          Integer denominator of fractional second (sN/sD).
!          Default = 1.
!     \item[{[sD\_i8]}]
!          Integer denominator of fractional second (sN\_i8/sD\_i8)
!                                                           (large, 64-bit).
!          Default = 1.
!     \item[{[calendar]}]
!          \begin{sloppypar}
!          Associated {\tt Calendar}.  Defaults to calendar
!          {\tt ESMF\_CALKIND\_NOCALENDAR} or default specified in
!          {\tt ESMF\_Initialize()} or {\tt ESMF\_CalendarSetDefault()}.
!          Alternate to, and mutually exclusive with, calkindflag
!          below.  Primarily for specifying a custom calendar kind.
!          \end{sloppypar}
!     \item[{[calkindflag]}]
!          Alternate to, and mutually exclusive with, calendar above.  More
!          convenient way of specifying a built-in calendar kind.
!     \item[{[timeZone]}]
!          Associated timezone (hours offset from UTC, e.g. EST = -5).
!          Default = 0 (UTC).  (Not implemented yet).
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_CalendarGetInit,calendar,rc)

      ! use optional args for any subset
      call c_ESMC_TimeSet(time, yy, yy_i8, mm, dd, d, d_i8, &
                          h, m, s, s_i8, ms, us, ns, &
                          d_r8, h_r8, m_r8, s_r8, ms_r8, us_r8, ns_r8, &
                          sN, sN_i8, sD, sD_i8, &
                          calendar, calkindflag, timeZone, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! mark output variable as successfully initialized
      call ESMF_TimeInit(time)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_TimeSet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeSyncToRealTime()"
!BOP
! !IROUTINE: ESMF_TimeSyncToRealTime - Get system real time (wall clock time)
!
! !INTERFACE:
      subroutine ESMF_TimeSyncToRealTime(time, keywordEnforcer, rc)
!
! !ARGUMENTS:
      type(ESMF_Time), intent(inout) :: time
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer, intent(out), optional :: rc
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Gets the system real time (wall clock time), and returns it as an
!     {\tt ESMF\_Time}.  Accurate to the nearest second.
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
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check input
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,time,rc)

      ! invoke C to C++ entry point
      call c_ESMC_TimeSyncToRealTime(time, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_TimeSyncToRealTime

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeValidate()"
!BOP
! !IROUTINE:  ESMF_TimeValidate - Validate a Time

! !INTERFACE:
      subroutine ESMF_TimeValidate(time, options, rc)

! !ARGUMENTS:
      type(ESMF_Time),   intent(in)            :: time
      character (len=*), intent(in),  optional :: options
      integer,           intent(out), optional :: rc

!
! !DESCRIPTION:
!     Checks whether an {\tt ESMF\_Time} is valid.
!     Must be a valid date/time on a valid calendar.
!     The options control the type of validation.
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          {\tt ESMF\_Time} instant to be validated.
!     \item[{[options]}]
!          Validation options. If none specified, validates all {\tt time} property
!            values. \\
!          "calendar" - validate only the {\tt time}'s calendar. \\
!          "timezone" - validate only the {\tt time}'s timezone. \\
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check input
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,time,rc)
   
      ! invoke C to C++ entry point
      call c_ESMC_TimeValidate(time, options, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_TimeValidate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeWriteRestart()"
!BOPI
! !IROUTINE:  ESMF_TimeWriteRestart - Save the contents of a Time (not implemented)

! !INTERFACE:
      subroutine ESMF_TimeWriteRestart(time, keywordEnforcer, rc)

! !ARGUMENTS:
      type(ESMF_Time),   intent(in)            :: time
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,           intent(out), optional :: rc

! !DESCRIPTION:
!     Saves an {\tt ESMF\_Time} object.  Default options are to select the
!     fastest way to save to disk.  (Not implemented yet).
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to save.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:
!     TMGn.n.n
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check input
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,time,rc)
   
      ! invoke C to C++ entry point
      call c_ESMC_TimeWriteRestart(time, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_TimeWriteRestart

!------------------------------------------------------------------------------
!
! This section includes the inherited ESMC_BaseTime class overloaded operators
! internal, private implementation methods.
! Note:  these functions do not have a return code, since Fortran forbids more
! than 2 arguments for arithmetic overloaded operators
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeInc()"
!BOPI
! !IROUTINE: ESMF_TimeInc - Increment a Time with a TimeInterval
!
! !INTERFACE:
      function ESMF_TimeInc(time, timeinterval)
!
! !RETURN VALUE:
      type(ESMF_Time) :: ESMF_TimeInc
!
! !ARGUMENTS:
      type(ESMF_Time),         intent(in) :: time
      type(ESMF_TimeInterval), intent(in) :: timeinterval
!
! !DESCRIPTION:
!     This method overloads the (+) operator for the {\tt ESMF\_Time} class.
!     See "interface operator(+)" above for complete description.
!
!EOPI
      integer :: localrc

      ! check inputs
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,time,localrc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeIntervalGetInit,timeinterval,localrc)

      ! invoke C to C++ entry point
      call c_ESMC_TimeInc(time, timeinterval, ESMF_TimeInc)

      ! mark output as successfully initialized
      call ESMF_TimeInit(ESMF_TimeInc)

      end function ESMF_TimeInc
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeDec()"
!BOPI
! !IROUTINE: ESMF_TimeDec - Decrement a Time with a TimeInterval
!
! !INTERFACE:
      function ESMF_TimeDec(time, timeinterval)
!
! !RETURN VALUE:
      type(ESMF_Time) :: ESMF_TimeDec
!
! !ARGUMENTS:
      type(ESMF_Time),         intent(in) :: time
      type(ESMF_TimeInterval), intent(in) :: timeinterval
!
! !DESCRIPTION:
!     This method overloads the (-) operator for the {\tt ESMF\_Time} class.
!     See "interface operator(-)" above for complete description.
!
!EOPI
      integer :: localrc

      ! check inputs
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,time,localrc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeIntervalGetInit,timeinterval,localrc)

      ! invoke C to C++ entry point
      call c_ESMC_TimeDec(time, timeinterval, ESMF_TimeDec)

      ! mark output as successfully initialized
      call ESMF_TimeInit(ESMF_TimeDec)

      end function ESMF_TimeDec

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeDiff()"
!BOPI
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
!     This method overloads the (-) operator for the {\tt ESMF\_Time} class.
!     See "interface operator(-)" above for complete description.
!
!EOPI
      integer :: localrc

      ! check inputs
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,time1,localrc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,time2,localrc)

      ! invoke C to C++ entry point
      call c_ESMC_TimeDiff(time1, time2, ESMF_TimeDiff)

      ! mark output as successfully initialized
      call ESMF_TimeIntervalInit(ESMF_TimeDiff)

      end function ESMF_TimeDiff

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeEQ()"
!BOPI
! !IROUTINE: ESMF_TimeEQ - Test if Time 1 is equal to Time 2
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
!     This method overloads the (==) operator for the {\tt ESMF\_Time} class.
!     See "interface operator(==)" above for complete description.
!
!EOPI
      integer :: localrc

      ! Initialize output value in case of error
      ESMF_TimeEQ = .false.

      ! check inputs
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,time1,localrc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,time2,localrc)

      ! invoke C to C++ entry point for ESMC_BaseTime base class function
      call c_ESMC_BaseTimeEQ(time1, time2, ESMF_TimeEQ)

      end function ESMF_TimeEQ

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeNE()"
!BOPI
! !IROUTINE: ESMF_TimeNE - Test if Time 1 is not equal to Time 2
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
!     This method overloads the (/=) operator for the {\tt ESMF\_Time} class.
!     See "interface operator(/=)" above for complete description.
!
!EOPI
      integer :: localrc

      ! Initialize output value in case of error
      ESMF_TimeNE = .true.

      ! check inputs
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,time1,localrc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,time2,localrc)

      ! invoke C to C++ entry point for ESMC_BaseTime base class function
      call c_ESMC_BaseTimeNE(time1, time2, ESMF_TimeNE)

      end function ESMF_TimeNE

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeLT()"
!BOPI
! !IROUTINE: ESMF_TimeLT - Test if Time 1 is less than Time 2
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
!     This method overloads the (<) operator for the {\tt ESMF\_Time} class.
!     See "interface operator(<)" above for complete description.
!
!EOPI
      integer :: localrc

      ! Initialize output value in case of error
      ESMF_TimeLT = .false.

      ! check inputs
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,time1,localrc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,time2,localrc)

      ! invoke C to C++ entry point for ESMC_BaseTime base class function
      call c_ESMC_BaseTimeLT(time1, time2, ESMF_TimeLT)

      end function ESMF_TimeLT

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeLE()"
!BOPI
! !IROUTINE: ESMF_TimeLE - Test if Time 1 is less than or equal to Time 2
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
!     This method overloads the (<=) operator for the {\tt ESMF\_Time} class.
!     See "interface operator(<=)" above for complete description.
!
!EOPI
      integer :: localrc

      ! Initialize output value in case of error
      ESMF_TimeLE = .false.

      ! check inputs
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,time1,localrc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,time2,localrc)

      ! invoke C to C++ entry point for ESMC_BaseTime base class function
      call c_ESMC_BaseTimeLE(time1, time2, ESMF_TimeLE)

      end function ESMF_TimeLE

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeGT()"
!BOPI
! !IROUTINE: ESMF_TimeGT - Test if Time 1 is greater than Time 2
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
!     This method overloads the (>) operator for the {\tt ESMF\_Time} class.
!     See "interface operator(>)" above for complete description.
!
!EOPI
      integer :: localrc

      ! Initialize output value in case of error
      ESMF_TimeGT = .false.

      ! check inputs
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,time1,localrc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,time2,localrc)

      ! invoke C to C++ entry point for ESMC_BaseTime base class function
      call c_ESMC_BaseTimeGT(time1, time2, ESMF_TimeGT)

      end function ESMF_TimeGT

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeGE()"
!BOPI
! !IROUTINE: ESMF_TimeGE - Test if Time 1 is greater than or equal to Time 2
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
!     This method overloads the (>=) operator for the {\tt ESMF\_Time} class.
!     See "interface operator(>=)" above for complete description.
!
!EOPI
      integer :: localrc

      ! Initialize output value in case of error
      ESMF_TimeGE = .false.

      ! check inputs
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,time1,localrc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,time2,localrc)

      ! invoke C to C++ entry point for ESMC_BaseTime base class function
      call c_ESMC_BaseTimeGE(time1, time2, ESMF_TimeGE)

      end function ESMF_TimeGE

!------------------------------------------------------------------------------

      end module ESMF_TimeMod
