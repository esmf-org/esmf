! $Id: ESMF_Time.F90,v 1.38 2003/08/08 00:25:49 eschwab Exp $
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
     sequence                           ! match C++ storage order
     private                            !   (members opaque on F90 side)
       type(ESMF_BaseTime) :: baseTime           ! inherit base class
       type(ESMF_Calendar), pointer :: calendar  ! associated calendar
       integer :: timeZone                       ! local timezone
       integer :: pad                            ! to satisfy halem compiler
     end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Time
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
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

      public ESMF_TimeReadRestart
      public ESMF_TimeWriteRestart
      public ESMF_TimeValidate
      public ESMF_TimePrint

! !PRIVATE MEMBER FUNCTIONS:

      private ESMF_TimeGetCalendarCopy
      private ESMF_TimeGetCalendarPtr
      private ESMF_TimeSetCalendarPtr
      private ESMF_TimeSetCalendarPtrPtr
      private ESMF_TimeGetDayOfYearR
      private ESMF_TimeGetDayOfYearI
      private ESMF_TimeGetDayOfYearTimeIntvl

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
      '$Id: ESMF_Time.F90,v 1.38 2003/08/08 00:25:49 eschwab Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface ESMF_TimeGetCalendar

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeGetCalendarCopy
      module procedure ESMF_TimeGetCalendarPtr

! !DESCRIPTION:
!     This interface overloads the {\tt ESMF\_GetCalendar} method
!     for the {\tt ESMF\_Time} class.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_TimeSetCalendar

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeSetCalendarPtr
      module procedure ESMF_TimeSetCalendarPtrPtr

! !DESCRIPTION:
!     This interface overloads the {\tt ESMF\_SetCalendar} method
!     for the {\tt ESMF\_Time} class.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_TimeGetDayOfYear

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeGetDayOfYearR
      module procedure ESMF_TimeGetDayOfYearI
      module procedure ESMF_TimeGetDayOfYearTimeIntvl

! !DESCRIPTION:
!     This interface overloads the {\tt ESMF\_GetDayOfYear} method
!     for the {\tt ESMF\_Time} class.
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
! Generic Get/Set routines which use F90 optional arguments
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeGet - Get value in user-specified units

! !INTERFACE:
      subroutine ESMF_TimeGet(time, yr_i4, yr_i8, &
                                    mm_i4, &
                                    dd_i4, &
                                    d_i4, d_i8, &
                                    h_i4, &
                                    m_i4, &
                                    s_i4, s_i8, &
                                    ms_i4, &
                                    us_i4, &
                                    ns_i4, &
                                    d_r8, &
                                    h_r8, &
                                    m_r8, &
                                    s_r8, &
                                    ms_r8, &
                                    us_r8, &
                                    ns_r8, &
                                    sN, sD, rc)

! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time
      integer(ESMF_IKIND_I4), intent(out), optional :: yr_i4
      integer(ESMF_IKIND_I8), intent(out), optional :: yr_i8
      integer(ESMF_IKIND_I4), intent(out), optional :: mm_i4
      integer(ESMF_IKIND_I4), intent(out), optional :: dd_i4
      integer(ESMF_IKIND_I4), intent(out), optional :: d_i4
      integer(ESMF_IKIND_I8), intent(out), optional :: d_i8
      integer(ESMF_IKIND_I4), intent(out), optional :: h_i4
      integer(ESMF_IKIND_I4), intent(out), optional :: m_i4
      integer(ESMF_IKIND_I4), intent(out), optional :: s_i4
      integer(ESMF_IKIND_I8), intent(out), optional :: s_i8
      integer(ESMF_IKIND_I4), intent(out), optional :: ms_i4
      integer(ESMF_IKIND_I4), intent(out), optional :: us_i4
      integer(ESMF_IKIND_I4), intent(out), optional :: ns_i4
      real(ESMF_IKIND_R8),    intent(out), optional :: d_r8
      real(ESMF_IKIND_R8),    intent(out), optional :: h_r8
      real(ESMF_IKIND_R8),    intent(out), optional :: m_r8
      real(ESMF_IKIND_R8),    intent(out), optional :: s_r8
      real(ESMF_IKIND_R8),    intent(out), optional :: ms_r8
      real(ESMF_IKIND_R8),    intent(out), optional :: us_r8
      real(ESMF_IKIND_R8),    intent(out), optional :: ns_r8
      integer,                intent(out), optional :: sN
      integer,                intent(out), optional :: sD
      integer,                intent(out), optional :: rc

! !DESCRIPTION:
!     Get the value of the {\tt ESMF\_Time} in units specified by the user
!     via F90 optional arguments.
!
!     Time manager represents and manipulates time internally with integers
!     to maintain precision. Hence, user-specified floating point values are
!     converted internally from integers.
!
!     Units are bound (normalized) to the next larger unit specified.  For
!     example, if a time is defined to be 2:00 am on a particular date, then
!     {\tt ESMF\_TimeGet(h\_i4 = hours, s\_i4 = seconds)} would return
!       {\tt hours = 2}, {\tt seconds = 0},
!     whereas {\tt ESMF\_TimeGet(s\_i4=seconds)} would return
!       {\tt seconds = 7200}.
!
!     See {\tt ../include/ESMC\_BaseTime.h} and {\tt ../include/ESMC\_Time.h} 
!     for complete description.
!     
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to query.
!     \item[{[yr\_i4]}]
!          Integer year CCYR (>= 32-bit).
!     \item[{[yr\_i8]}]
!          Integer year CCYR (large, >= 64-bit).
!     \item[{[mm\_i4]}]
!          Integer month 1-12.
!     \item[{[dd\_i4]}]
!          Integer day of the month 1-31.
!     \item[{[d\_i4]}]
!          Integer Julian days (>= 32-bit).
!     \item[{[d\_i8]}]
!          Integer Julian days (large, >= 64-bit).
!     \item[{[h\_i4]}]
!          Integer hours.
!     \item[{[m\_i4]}]
!          Integer minutes.
!     \item[{[s\_i4]}]
!          Integer seconds (>= 32-bit).
!     \item[{[s\_i8]}]
!          Integer seconds (large, >= 64-bit).
!     \item[{[ms\_i4]}]
!          Integer milliseconds.
!     \item[{[us\_i4]}]
!          Integer microseconds.
!     \item[{[ns\_i4]}]
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
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG2.1, TMG2.5.1, TMG2.5.6

      ! use optional args for any subset
      call c_ESMC_TimeGet(time, yr_i4, yr_i8, mm_i4, dd_i4, d_i4, d_i8, &
                          h_i4, m_i4, s_i4, s_i8, ms_i4, us_i4, ns_i4, &
                          d_r8, h_r8, m_r8, s_r8, ms_r8, us_r8, ns_r8, &
                          sN, sD, rc)
    
      end subroutine ESMF_TimeGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeSet - Initialize via user-specified unit set

! !INTERFACE:
      subroutine ESMF_TimeSet(time, yr_i4, yr_i8, &
                                    mm_i4, &
                                    dd_i4, &
                                    d_i4, d_i8, &
                                    h_i4, &
                                    m_i4, &
                                    s_i4, s_i8, &
                                    ms_i4, &
                                    us_i4, &
                                    ns_i4, &
                                    d_r8, &
                                    h_r8, &
                                    m_r8, &
                                    s_r8, &
                                    ms_r8, &
                                    us_r8, &
                                    ns_r8, &
                                    sN, sD, calendar, timeZone, rc)

! !ARGUMENTS:
      type(ESMF_Time), intent(inout) :: time
      integer(ESMF_IKIND_I4), intent(in), optional :: yr_i4
      integer(ESMF_IKIND_I8), intent(in), optional :: yr_i8
      integer(ESMF_IKIND_I4), intent(in), optional :: mm_i4
      integer(ESMF_IKIND_I4), intent(in), optional :: dd_i4
      integer(ESMF_IKIND_I4), intent(in), optional :: d_i4
      integer(ESMF_IKIND_I8), intent(in), optional :: d_i8
      integer(ESMF_IKIND_I4), intent(in), optional :: h_i4
      integer(ESMF_IKIND_I4), intent(in), optional :: m_i4
      integer(ESMF_IKIND_I4), intent(in), optional :: s_i4
      integer(ESMF_IKIND_I8), intent(in), optional :: s_i8
      integer(ESMF_IKIND_I4), intent(in), optional :: ms_i4
      integer(ESMF_IKIND_I4), intent(in), optional :: us_i4
      integer(ESMF_IKIND_I4), intent(in), optional :: ns_i4
      real(ESMF_IKIND_R8),    intent(in), optional :: d_r8
      real(ESMF_IKIND_R8),    intent(in), optional :: h_r8
      real(ESMF_IKIND_R8),    intent(in), optional :: m_r8
      real(ESMF_IKIND_R8),    intent(in), optional :: s_r8
      real(ESMF_IKIND_R8),    intent(in), optional :: ms_r8
      real(ESMF_IKIND_R8),    intent(in), optional :: us_r8
      real(ESMF_IKIND_R8),    intent(in), optional :: ns_r8
      integer,                intent(in), optional :: sN
      integer,                intent(in), optional :: sD
      type(ESMF_Calendar),    intent(in),  optional :: calendar
      integer,                intent(in),  optional :: timeZone
      integer,                intent(out), optional :: rc

! !DESCRIPTION:
!     Initializes a {\tt ESMF\_Time} with a set of user-specified units
!     via F90 optional arguments.
!
!     Time manager represents and manipulates time internally with integers
!     to maintain precision. Hence, user-specified floating point values are
!     converted internally to integers.
!
!     See {\tt ../include/ESMC\_BaseTime.h and ../include/ESMC\_Time.h} for
!     complete description.
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to initialize.
!     \item[{[yr\_i4]}]
!          Integer year CCYR (>= 32-bit).
!     \item[{[yr\_i8]}]
!          Integer year CCYR (large, >= 64-bit).
!     \item[{[mm\_i4]}]
!          Integer month 1-12.
!     \item[{[dd\_i4]}]
!          Integer day of the month 1-31.
!     \item[{[d\_i4]}]
!          Integer Julian days (>= 32-bit).
!     \item[{[d\_i8]}]
!          Integer Julian days (large, >= 64-bit).
!     \item[{[h\_i4]}]
!          Integer hours.
!     \item[{[m\_i4]}]
!          Integer minutes.
!     \item[{[s\_i4]}]
!          Integer seconds (>= 32-bit).
!     \item[{[s\_i8]}]
!          Integer seconds (large, >= 64-bit).
!     \item[{[ms\_i4]}]
!          Integer milliseconds.
!     \item[{[us\_i4]}]
!          Integer microseconds.
!     \item[{[ns\_i4]}]
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
      call c_ESMC_TimeSet(time, yr_i4, yr_i8, mm_i4, dd_i4, d_i4, d_i8, &
                          h_i4, m_i4, s_i4, s_i8, ms_i4, us_i4, ns_i4, &
                          d_r8, h_r8, m_r8, s_r8, ms_r8, us_r8, ns_r8, &
                          sN, sD, calendar, timeZone, rc)

      end subroutine ESMF_TimeSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeGetCalendarCopy - Get copy of associated calendar

! !INTERFACE:
      subroutine ESMF_TimeGetCalendarCopy(time, calendar, rc)

! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time
      type(ESMF_Calendar), intent(out) :: calendar
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get copy of the associated {\tt ESMF\_Calendar}.
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to query.
!     \item[{calendar}]
!          Associated {\tt ESMF\_Calendar}.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n

      call c_ESMC_TimeGetCalendarCopy(time, calendar, rc)
    
      end subroutine ESMF_TimeGetCalendarCopy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeGetCalendarPtr - Get pointer to associated calendar

! !INTERFACE:
      subroutine ESMF_TimeGetCalendarPtr(time, calendar, rc)

! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time
      type(ESMF_Pointer), intent(out) :: calendar
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get pointer to the associated {\tt ESMF\_Calendar}.
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to query.
!     \item[{calendar}]
!          Pointer to associated {\tt ESMF\_Calendar}.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n

      call c_ESMC_TimeGetCalendarPtr(time, calendar, rc)
    
      end subroutine ESMF_TimeGetCalendarPtr

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeSetCalendarPtr - Set associated calendar

! !INTERFACE:
      subroutine ESMF_TimeSetCalendarPtr(time, calendar, rc)

! !ARGUMENTS:
      type(ESMF_Time), intent(inout) :: time
      type(ESMF_Calendar), intent(in) :: calendar
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set the associated {\tt ESMF\_Calendar} by passing its pointer.
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to set.
!     \item[{calendar}]
!          Associated {\tt ESMF\_Calendar}.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n

      call c_ESMC_TimeSetCalendarPtr(time, calendar, rc)
    
      end subroutine ESMF_TimeSetCalendarPtr

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeSetCalendarPtrPtr - Set associated calendar

! !INTERFACE:
      subroutine ESMF_TimeSetCalendarPtrPtr(time, calendar, rc)

! !ARGUMENTS:
      type(ESMF_Time), intent(inout) :: time
      type(ESMF_Pointer), intent(in) :: calendar
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set the associated {\tt ESMF\_Calendar} by passing the address of
!     its pointer.
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to set.
!     \item[{calendar}]
!          Associated {\tt ESMF\_Calendar} pointer.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n

      call c_ESMC_TimeSetCalendarPtrPtr(time, calendar, rc)
    
      end subroutine ESMF_TimeSetCalendarPtrPtr

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
!     Returns true if both {\tt ESMF\_Time}'s {\tt ESMF\_Calendar}s are
!     the same, false otherwise.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          The first object instance to compare.
!     \item[time2]
!          The second object instance to compare.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n

      call c_ESMC_TimeIsSameCal(time1, time2, ESMF_TimeIsSameCal, rc)
    
      end function ESMF_TimeIsSameCal

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeGetTimezone - Get time instant's time zone
!
! !INTERFACE:
      subroutine ESMF_TimeGetTimezone(time, timeZone, rc)
!
! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time
      integer, intent(out) :: timeZone
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Get the time zone of the given {\tt ESMF\_Time} instant.
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to query.
!     \item[{timeZone}]
!          {\tt ESMF\_Time} instant's time zone.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG2.5.1

      call c_ESMC_TimeGetTimezone(time, timeZone, rc)

      end subroutine ESMF_TimeGetTimezone

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeSetTimezone - Set time instant's time zone
!
! !INTERFACE:
      subroutine ESMF_TimeSetTimezone(time, timeZone, rc)
!
! !ARGUMENTS:
      type(ESMF_Time), intent(inout) :: time
      integer, intent(in) :: timeZone
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Set the time zone of the given {\tt ESMF\_Time} instant.
!     Default is 0 for UTC time zone (Greenwich Mean Time).
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to set.
!     \item[{timeZone}]
!          {\tt ESMF\_Time} instant's time zone.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG2.5.1

      call c_ESMC_TimeSetTimezone(time, timeZone, rc)

      end subroutine ESMF_TimeSetTimezone

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeGetString - Get time instant value in string format

! !INTERFACE:
      subroutine ESMF_TimeGetString(time, timeString, rc)

! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time
      character, intent(out) :: timeString(:)
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Convert {\tt ESMF\_Time}'s value into ISO 8601 format YYYY-MM-DDThh:mm:ss.
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to convert.
!     \item[timeString]
!          The string to return.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG2.4.7

      call c_ESMC_TimeGetString(time, timeString, rc)

      end subroutine ESMF_TimeGetString

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeGetDayOfYearR - Get time instant's day of the year as a floating point value
!
! !INTERFACE:
      subroutine ESMF_TimeGetDayOfYearR(time, dayOfYear, rc)
!
! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time
      real(ESMF_IKIND_R8), intent(out) :: dayOfYear
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Get the day of the year the given {\tt ESMF\_Time} instant falls on
!     (1.x-365.x).  Returned as floating point value; fractional part
!     represents the time of day. 
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to query.
!     \item[dayOfYear]
!          The {\tt ESMF\_Time} instant's day of the year (1-365).
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG2.5.2

      call c_ESMC_TimeGetDayOfYearDouble(time, dayOfYear, rc)

      end subroutine ESMF_TimeGetDayOfYearR

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeGetDayOfYearI - Get time instant's day of the year as an integer value
!
! !INTERFACE:
      subroutine ESMF_TimeGetDayOfYearI(time, dayOfYear, rc)
!
! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time
      integer(ESMF_IKIND_I4), intent(out) :: dayOfYear
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Get the day of the year the given {\tt ESMF\_Time} instant falls on
!     (1-365).  Returned as an integer value.
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to query.
!     \item[dayOfYear]
!          The {\tt ESMF\_Time} instant's day of the year (1-365).
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      call c_ESMC_TimeGetDayOfYearInteger(time, dayOfYear, rc)

      end subroutine ESMF_TimeGetDayOfYearI

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeGetDayOfYearTimeIntvl - Get time instant's day of the year as a Time Interval
!
! !INTERFACE:
      subroutine ESMF_TimeGetDayOfYearTimeIntvl(time, dayOfYear, rc)
!
! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time
      type(ESMF_TimeInterval), intent(out) :: dayOfYear
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Get the day of the year the given {\tt ESMF\_Time} instant falls on
!     (1-365).  Returned as an {\tt ESMF\_TimeInterval}.
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to query.
!     \item[dayOfYear]
!          The {\tt Time} instant's day of the year as a
!            {\tt ESMC\_TimeInterval}.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      call c_ESMC_TimeGetDayOfYearTimeInt(time, dayOfYear, rc)

      end subroutine ESMF_TimeGetDayOfYearTimeIntvl

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeGetDayOfWeek - Get time instant's day of the week
!
! !INTERFACE:
      subroutine ESMF_TimeGetDayOfWeek(time, dayOfWeek, rc)
!
! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time
      integer, intent(out) :: dayOfWeek
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Get the day of the week the given {\tt ESMF\_Time} instant falls on.
!     ISO 8601 standard:  Monday = 1 through Sunday = 7.
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to query.
!     \item[dayOfWeek]
!          The time instant's day of the week (1-7).
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG2.5.3
    
      call c_ESMC_TimeGetDayOfWeek(time, dayOfWeek, rc)

      end subroutine ESMF_TimeGetDayOfWeek

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeGetDayOfMonth - Get time instant's day of the month
!
! !INTERFACE:
      subroutine ESMF_TimeGetDayOfMonth(time, dayOfMonth, rc)
!
! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time
      integer, intent(out) :: dayOfMonth
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Get the day of the month the {\tt ESMF\_Time} instant falls on (1-31).
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to query.
!     \item[dayOfMonth]
!          The time instant's day of the month (1-31).
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG2.5.4

      call c_ESMC_TimeGetDayOfMonth(time, dayOfMonth, rc)

      end subroutine ESMF_TimeGetDayOfMonth

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeGetMidMonth - Get time instant's middle of the month
!
! !INTERFACE:
      subroutine ESMF_TimeGetMidMonth(time, midMonth, rc)
!
! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time
      type(ESMF_Time), intent(out) :: midMonth
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Get the middle time instant of the month the given {\tt ESMF\_Time}
!     instant falls on.
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to query.
!     \item[MidMonth]
!          The given time instant's middle-of-the-month time instant.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG2.5.5

      call c_ESMC_TimeGetMidMonth(time, midMonth, rc)

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
!     Get the system real {\tt ESMF\_Time} (wall clock time), return in
!     given {\tt ESMF\_Time} instant.
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

      call c_ESMC_TimeGetRealTime(time, rc)

      end subroutine ESMF_TimeGetRealTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeInc - Increment time instant with a time interval
!
! !INTERFACE:
      function ESMF_TimeInc(time, timeInterval)
!
! !RETURN VALUE:
      type(ESMF_Time) :: ESMF_TimeInc
!
! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time
      type(ESMF_TimeInterval), intent(in) :: timeInterval
!
! !DESCRIPTION:
!     Increment {\tt ESMF\_Time} instant with a {\tt ESMF\_TimeInterval},
!     return resulting {\tt ESMF\_Time} instant.
!
!     Maps overloaded (+) operator interface function to
!     {\tt ESMF\_BaseTime} base class.
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The given {\tt ESMF\_Time} to increment.
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
! !IROUTINE: ESMF_TimeDec - Decrement time instant with a time interval
!
! !INTERFACE:
      function ESMF_TimeDec(time, timeInterval)
!
! !RETURN VALUE:
      type(ESMF_Time) :: ESMF_TimeDec
!
! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time
      type(ESMF_TimeInterval), intent(in) :: timeInterval
!
! !DESCRIPTION:
!     Decrement {\tt ESMF\_Time} instant with a {\tt ESMF\_TimeInterval},
!     return resulting {\tt ESMF\_Time} instant.
!
!     Maps overloaded (-) operator interface function to
!     {\tt ESMF\_BaseTime} base class.
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The given {\tt ESMF\_Time} to decrement.
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
!     Return the {\tt ESMF\_TimeInterval} difference between two
!     {\tt ESMF\_Time} instants.
!
!     Maps overloaded (-) operator interface function to
!     {\tt ESMF\_BaseTime} base class.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          The first {\tt ESMF\_Time} instant.
!     \item[time2]
!          The second {\tt ESMF\_Time} instant.
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
!     Return true if both given {\tt ESMF\_Time} instants are equal, false
!     otherwise.  Maps overloaded (==) operator interface function to
!     {\tt ESMF\_BaseTime} base class.
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
! !IROUTINE: ESMF_TimeNE - Compare two times for in-equality
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
!     Return true if both given {\tt ESMF\_Time} instants are not equal, false
!     otherwise.  Maps overloaded (/=) operator interface function to
!     {\tt ESMF\_BaseTime} base class.
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
!     Return true if first {\tt ESMF\_Time} instant is less than second
!     {\tt ESMF\_Time} instant, false otherwise.  Maps overloaded (<)
!     operator interface function to {\tt ESMF\_BaseTime} base class.
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
!     Return true if first {\tt ESMF\_Time} instant is greater than second
!     {\tt ESMF\_Time} instant, false otherwise.  Maps overloaded (>) operator
!     interface function to {\tt ESMF\_BaseTime} base class.
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
!     Return true if first {\tt ESMF\_Time} instant is less than or equal to
!     second {\tt ESMF\_Time} instant, false otherwise.  Maps overloaded (<=)
!     operator interface function to {\tt ESMF\_BaseTime} base class.
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
!     Return true if first {\tt ESMF\_Time} instant is greater than or equal to
!     second {\tt ESMF\_Time} instant, false otherwise.  Maps overloaded (>=)
!     operator interface function to {\tt ESMF\_BaseTime} base class.
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
! !IROUTINE:  ESMF_TimeReadRestart - Restore a time instant's properties

! !INTERFACE:
      subroutine ESMF_TimeReadRestart(time, s, sN, sD, calendar, timeZone, rc)

! !ARGUMENTS:
      type(ESMF_Time), intent(out) :: time
      integer(ESMF_IKIND_I8), intent(in) :: s
      integer, intent(in) :: sN
      integer, intent(in) :: sD
      type(ESMF_Calendar), intent(in) :: calendar
      integer, intent(in) :: timeZone
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a restore on a {\tt ESMF\_Time}'s properties.
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
! !IROUTINE:  ESMF_TimeWriteRestart - Save a time instant's properties

! !INTERFACE:
      subroutine ESMF_TimeWriteRestart(time, s, sN, sD, calendar, timeZone, rc)

! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time
      integer(ESMF_IKIND_I8), intent(out) :: s
      integer, intent(out) :: sN
      integer, intent(out) :: sD
      type(ESMF_Calendar), intent(out) :: calendar
      integer, intent(out) :: timeZone
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a save on a {\tt ESMF\_Time}'s properties.
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
! !IROUTINE:  ESMF_TimeValidate - Validate a time instant's properties

! !INTERFACE:
      subroutine ESMF_TimeValidate(time, opts, rc)

! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a validation check on a {\tt ESMF\_Time}'s properties.
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          {\tt ESMF\_Time} instant to validate.
!     \item[{[opts]}]
!          Validation options.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
   
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
!     To support testing/debugging, print out a {\tt ESMF\_Time}'s
!     properties.
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          {\tt ESMF\_Time} instant to print out.
!     \item[{[opts]}]
!          Print options.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
   
      call c_ESMC_TimePrint(time, opts, rc)

      end subroutine ESMF_TimePrint

!------------------------------------------------------------------------------

      end module ESMF_TimeMod
