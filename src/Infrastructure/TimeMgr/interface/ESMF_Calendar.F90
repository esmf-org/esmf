! $Id: ESMF_Calendar.F90,v 1.10 2003/04/07 19:38:16 eschwab Exp $
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
!BOP
! !MODULE: ESMF_CalendarMod
!
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation
!
! Defines F90 wrapper entry points for corresponding
! C++ class ESMC\_Calendar implementation
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from ESMF base class
      use ESMF_BaseMod

      ! inherit from base time class
      use ESMF_BaseTimeMod,     only : ESMF_BaseTime

      implicit none
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------

      integer, parameter :: MONTHSPERYEAR = 12

!------------------------------------------------------------------------------
!     ! ESMF_CalendarType
!
!     ! F90 "enum" type to match C++ ESMC_CalendarType enum

      type ESMF_CalendarType
      sequence
      private
        integer :: caltype
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
!     ! ESMF_DaysPerYear
 
      type ESMF_DaysPerYear
      sequence
      private
        integer :: D     ! whole days per year
        integer :: Dn    ! fractional days per year numerator
        integer :: Dd    ! fractional days per year denominator
      end type           ! e.g. for Venus, D=0, Dn=926, Dd=1000

!------------------------------------------------------------------------------
!     ! ESMF_Calendar
!
!     ! F90 class to match C++ Calendar class in size and sequence

      type ESMF_Calendar
      sequence
      private
        type(ESMF_CalendarType) :: Type
        integer, dimension(MONTHSPERYEAR) :: DaysPerMonth
        integer :: SecondsPerDay
        type(ESMF_DaysPerYear) :: DaysPerYear
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public MONTHSPERYEAR
      public ESMF_CalendarType
      public ESMF_CAL_GREGORIAN, ESMF_CAL_JULIAN, ESMF_CAL_NOLEAP, &
             ESMF_CAL_360DAY, ESMF_CAL_GENERIC, ESMF_CAL_NOCALENDAR
      public ESMF_DaysPerYear
      public ESMF_Calendar
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
      public ESMF_CalendarInit
      public ESMF_CalendarInitGeneric
      public ESMF_CalendarConvertToTime
      public ESMF_CalendarConvertToDate

! Required inherited and overridden ESMF_Base class methods

      public ESMF_CalendarRead
      public ESMF_CalendarWrite
      public ESMF_CalendarValidate
      public ESMF_CalendarPrint
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Calendar.F90,v 1.10 2003/04/07 19:38:16 eschwab Exp $'

!==============================================================================

      contains

!==============================================================================
!
! This section includes the Init methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CalendarInit - Initializes the calendar type

! !INTERFACE:
      subroutine ESMF_CalendarInit(calendar, Type, rc)

! !ARGUMENTS:
      type(ESMF_Calendar), intent(inout) :: calendar
      type(ESMF_CalendarType), intent(in) :: Type
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Initializes a {\tt Calendar} to the given {\tt CalendarType}
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          The object instance to initialize
!     \item[Type]
!          The {\tt CalendarType} ESMF\_CAL\_GREGORIAN, ESMF\_CAL\_JULIAN, etc.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!    
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
    
!     invoke C to C++ entry point
      call c_ESMC_CalendarInit(calendar, Type%caltype, rc)
    
      end subroutine ESMF_CalendarInit
    
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CalendarInitGeneric - Initialize calendar to user-type

! !INTERFACE:
      subroutine ESMF_CalendarInitGeneric(calendar, DaysPerMonth, &
                                          SecondsPerDay, DaysPerYear, &
                                          DaysPerYearDn, DaysPerYearDd, rc)
! !ARGUMENTS:
      type(ESMF_Calendar), intent(inout) :: calendar
      integer, dimension(MONTHSPERYEAR), intent(in) :: DaysPerMonth
      integer, intent(in) :: SecondsPerDay
      integer, intent(in) :: DaysPerYear
      integer, intent(in) :: DaysPerYearDn
      integer, intent(in) :: DaysPerYearDd
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Initializes a {\tt Calendar} to a user-specified type
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          The object instance to initialize
!     \item[DaysPerMonth]
!          Integer array of days per month, for each of the 12 months
!     \item[SecondsPerDay]
!          Integer number of seconds per day
!     \item[DaysPerYear]
!          Integer number of days per year
!     \item[DaysPerYearDn]
!          Integer fractional number of days per year (numerator)
!     \item[DaysPerYearDd]
!          Integer fractional number of days per year (denominator)
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
! !REQUIREMENTS:
!     TMGn.n.n
!EOP

!     invoke C to C++ entry point
!      call c_ESMC_CalendarInitGeneric(calendar, DaysPerMonth, &
!                                      SecondsPerDay, DaysPerYear, &
!                                      DaysPerYearDn, DaysPerYearDd, rc)
    
      end subroutine ESMF_CalendarInitGeneric
    
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CalendarConvertToTime - Convert user time units to ESMF_BaseTime

! !INTERFACE:
      subroutine ESMF_CalendarConvertToTime(YR, MM, DD, D, H, M, S, &
                                            MS, US, NS, &
                                            d_, h_, m_, s_, ms_, us_, ns_, &
                                            Sn, Sd, basetime, rc)

! !ARGUMENTS:
      integer, intent(in), optional :: YR
      integer, intent(in), optional :: MM
      integer, intent(in), optional :: DD
      integer, intent(in), optional :: D
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
      type(ESMF_BaseTime), intent(inout) :: basetime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Convert user-specified time unit set into {\tt ESMF\_BaseTime}.
!     Date <--> Time conversion routines, use F90 optional arguments.
!
!     The arguments are:
!     \begin{description}
!     \item[{[YR]]}]
!          Integer year CCYR
!     \item[{[MM]}]
!          Integer month 1-12
!     \item[{[DD]}]
!          Integer day of the month 1-31
!     \item[{[D]}]
!          Integer Julian days
!     \item[{[H]}]
!          Integer hours
!     \item[{[M]}]
!          Integer minutes
!     \item[{[S]}]
!          64-bit integer seconds
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
!     \item[basetime]
!          Returned {\tt BaseTime} value
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG2.4.5, TMG2.5.6
!EOP

      ! invoke C to C++ entry point
      ! use optional args for any subset
!       call c_ESMC_CalendarConvertToTime(YR, MM, DD, D, H, M, S, &
!                                         MS, US, NS, &
!                                         d_, h_, m_, s_, ms_, us_, ns_, &
!                                         Sn, Sd, basetime, rc)
    
      end subroutine ESMF_CalendarConvertToTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CalendarConvertToDate - Convert ESMF_BaseTime into user units

! !INTERFACE:
      subroutine ESMF_CalendarConvertToDate(basetime, YR, MM, DD, D, H, &
                                            M, S, MS, US, NS, &
                                            d_, h_, m_, s_, ms_, us_, ns_, &
                                            Sn, Sd, rc)

! !ARGUMENTS:
      type(ESMF_BaseTime), intent(in) :: basetime
      integer, intent(out), optional :: YR
      integer, intent(out), optional :: MM
      integer, intent(out), optional :: DD
      integer, intent(out), optional :: D
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
!     Convert {\tt ESMF\_BaseTime} into user-specified time unit set. 
!     Date <--> Time conversion routines, use F90 optional arguments.
!
!     The arguments are:
!     \begin{description}
!     \item[basetime]
!          Given {\tt BaseTime} value
!     \item[{[YR]}]
!          Integer year CCYR
!     \item[{[MM]}]
!          Integer month 1-12
!     \item[{[DD]}]
!          Integer day of the month 1-31
!     \item[{[D]}]
!          Integer Julian days
!     \item[{[H]}]
!          Integer hours
!     \item[{[M]}]
!          Integer minutes
!     \item[{[S]}]
!          64-bit integer seconds
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
!     TMG2.4.5, TMG2.5.6
!EOP

!     invoke C to C++ entry point
      ! use optional args for any subset
!       call c_ESMC_CalendarConvertToDate(basetime, YR, MM, DD, D, H, &
!                                         M, S, MS, US, NS, &
!                                         d_, h_, m_, s_, ms_, us_, ns_, &
!                                         Sn, Sd, rc)
    
      end subroutine ESMF_CalendarConvertToDate

!------------------------------------------------------------------------------
! 
! This section defines the overridden Read, Write, Validate and Print methods
! from the ESMF_Base class
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_CalendarRead - Restore a calendar's properties

! !INTERFACE:
      subroutine ESMF_CalendarRead(calendar, Type, DaysPerMonth, &
                           SecondsPerDay, DaysPerYear, &
                           DaysPerYearDn, DaysPerYearDd, rc)

! !ARGUMENTS:
      type(ESMF_Calendar), intent(inout) :: calendar
      type(ESMF_CalendarType), intent(in) :: Type
      integer, dimension(MONTHSPERYEAR), intent(in) :: DaysPerMonth
      integer, intent(in) :: SecondsPerDay
      integer, intent(in) :: DaysPerYear
      integer, intent(in) :: DaysPerYearDn
      integer, intent(in) :: DaysPerYearDd
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a restore on a {\tt Calendar}'s properties
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          Calendar to restore
!     \item[Type]
!          The {\tt CalendarType} ESMF\_CAL\_GREGORIAN, ESMF\_CAL\_JULIAN, etc.
!     \item[DaysPerMonth]
!          Integer array of days per month, for each of the 12 months
!     \item[SecondsPerDay]
!          Integer number of seconds per day
!     \item[DaysPerYear]
!          Integer number of days per year
!     \item[DaysPerYearDn]
!          Integer fractional number of days per year (numerator)
!     \item[DaysPerYearDd]
!          Integer fractional number of days per year (denominator)
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
   
      call c_ESMC_CalendarRead(calendar, Type, DaysPerMonth, &
                               SecondsPerDay, DaysPerYear, &
                               DaysPerYearDn, DaysPerYearDd, rc)

      end subroutine ESMF_CalendarRead

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_CalendarWrite - Save a calendar's properties

! !INTERFACE:
      subroutine ESMF_CalendarWrite(calendar, Type, DaysPerMonth, &
                            SecondsPerDay, DaysPerYear, &
                            DaysPerYearDn, DaysPerYearDd, rc)

! !ARGUMENTS:
      type(ESMF_Calendar), intent(inout) :: calendar
      type(ESMF_CalendarType), intent(out) :: Type
      integer, dimension(MONTHSPERYEAR), intent(out) :: DaysPerMonth
      integer, intent(out) :: SecondsPerDay
      integer, intent(out) :: DaysPerYear
      integer, intent(out) :: DaysPerYearDn
      integer, intent(out) :: DaysPerYearDd
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a save on a {\tt Calendar}'s properties
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          Calendar to save
!     \item[Type]
!          The {\tt CalendarType} ESMF\_CAL\_GREGORIAN, ESMF\_CAL\_JULIAN, etc.
!     \item[DaysPerMonth]
!          Integer array of days per month, for each of the 12 months
!     \item[SecondsPerDay]
!          Integer number of seconds per day
!     \item[DaysPerYear]
!          Integer number of days per year
!     \item[DaysPerYearDn]
!          Integer fractional number of days per year (numerator)
!     \item[DaysPerYearDd]
!          Integer fractional number of days per year (denominator)
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
   
      call c_ESMC_CalendarWrite(calendar, Type, DaysPerMonth, &
                                SecondsPerDay, DaysPerYear, &
                                DaysPerYearDn, DaysPerYearDd, rc)

      end subroutine ESMF_CalendarWrite

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_CalendarValidate - Validate a calendar's properties

! !INTERFACE:
      subroutine ESMF_CalendarValidate(calendar, opts, rc)
 
! !ARGUMENTS:
      type(ESMF_Calendar), intent(inout) :: calendar
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a validation check on a {\tt Calendar}'s properties
! 
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          Calendar to validate
!     \item[{[opts]}]
!          Validate options
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
      
      call c_ESMC_CalendarValidate(calendar, opts, rc)

      end subroutine ESMF_CalendarValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_CalendarPrint - Print out a calendar's properties

! !INTERFACE:
      subroutine ESMF_CalendarPrint(calendar, opts, rc)

! !ARGUMENTS:
      type(ESMF_Calendar), intent(inout) :: calendar
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     To support testing/debugging, print out a {\tt Calendar}'s  
!     properties.
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          Calendar to print out
!     \item[{[opts]}]
!          Print options
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
  
      call c_ESMC_CalendarPrint(calendar, opts, rc)

      end subroutine ESMF_CalendarPrint
      
!------------------------------------------------------------------------------

      end module ESMF_CalendarMod
