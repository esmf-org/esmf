! $Id: ESMF_Calendar.F90,v 1.91.2.4 2009/01/21 21:25:23 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_Calendar.F90"
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
#include "ESMF_TimeMgr.inc"

!==============================================================================
!BOPI
! !MODULE: ESMF_CalendarMod
!
! !DESCRIPTION:
! Part of Time Manager Fortran API wrapper of C++ implemenation.
!
! Defines Fortran wrapper entry points for corresponding
! C++ class { \tt ESMC\_Calendar} implementation.
!
! See {\tt ../include/ESMC\_Calendar.h} for complete description.
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from ESMF base class
      use ESMF_UtilTypesMod
      use ESMF_BaseMod

      ! for ReadRestart()/WriteRestart()
      use ESMF_IOSpecMod
      use ESMF_InitMacrosMod

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
!     ! Fortran "enum" type to match C++ ESMC_CalendarType enum

      type ESMF_CalendarType
      sequence
      private
        integer :: calendartype
      end type

      type(ESMF_CalendarType), parameter :: &
                               ESMF_CAL_GREGORIAN =  ESMF_CalendarType(1), &
                               ESMF_CAL_JULIAN =     ESMF_CalendarType(2), &
                               ESMF_CAL_JULIANDAY =  ESMF_CalendarType(3), &
                           ! like Gregorian, except Feb always has 28 days
                               ESMF_CAL_NOLEAP =     ESMF_CalendarType(4), & 
                           ! 12 months, 30 days each
                               ESMF_CAL_360DAY =     ESMF_CalendarType(5), & 
                           ! user defined
                               ESMF_CAL_CUSTOM =     ESMF_CalendarType(6), &
                           ! track base time seconds only
                               ESMF_CAL_NOCALENDAR = ESMF_CalendarType(7)

!------------------------------------------------------------------------------
!     ! ESMF_Calendar
!
      type ESMF_Calendar
      sequence
      private
        type(ESMF_Pointer) :: this
        ESMF_INIT_DECLARE
      end type
!
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public MONTHS_PER_YEAR
      public ESMF_CalendarType
      public ESMF_CAL_GREGORIAN, ESMF_CAL_JULIAN, ESMF_CAL_JULIANDAY, &
             ESMF_CAL_NOLEAP,    ESMF_CAL_360DAY, ESMF_CAL_CUSTOM, &
             ESMF_CAL_NOCALENDAR
      public ESMF_Calendar
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
      public operator(==)
      public operator(/=)
      public ESMF_CalendarCreate
      public ESMF_CalendarDestroy
      public ESMF_CalendarFinalize
      public ESMF_CalendarGet
      public ESMF_CalendarInitialize
      public ESMF_CalendarIsLeapYear
      public ESMF_CalendarPrint
      public ESMF_CalendarReadRestart
      public ESMF_CalendarSet
      public ESMF_CalendarSetDefault
      public ESMF_CalendarValidate
      public ESMF_CalendarWriteRestart
      public ESMF_CalendarGetInit       ! For Standardized Initialization
      public ESMF_CalendarSetInitCreated
!EOPI

! !PRIVATE MEMBER FUNCTIONS:
      private ESMF_CalendarEQ
      private ESMF_CalendarNE
      private ESMF_CalendarTypeEQ
      private ESMF_CalendarTypeNE
      private ESMF_CalendarCalAndTypeEQ
      private ESMF_CalendarCalAndTypeNE
      private ESMF_CalendarTypeAndCalEQ
      private ESMF_CalendarTypeAndCalNE
      private ESMF_CalendarCreateBuiltIn
      private ESMF_CalendarCreateCopy
      private ESMF_CalendarCreateCustom
      private ESMF_CalendarIsLeapYearI4
      private ESMF_CalendarIsLeapYearI8
      private ESMF_CalendarSetBuiltIn
      private ESMF_CalendarSetCustom
      private ESMF_CalendarSetDefaultType
      private ESMF_CalendarSetDefaultCal

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Calendar.F90,v 1.91.2.4 2009/01/21 21:25:23 cdeluca Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
! 
!==============================================================================
!BOP
! !IROUTINE:  ESMF_CalendarOperator(==) - Test if Calendar 1 is equal to Calendar 2
!
! !INTERFACE:
      interface operator(==)
!     if (calendar1 == calendar2) then ... endif
!                  OR
!     result = (calendar1 == calendar2)
!
! !RETURN VALUE:
!     logical :: result
!
! !ARGUMENTS:
!     type(ESMF_Calendar), intent(in) :: calendar1
!     type(ESMF_Calendar), intent(in) :: calendar2
!
! !DESCRIPTION:
!     Overloads the (==) operator for the {\tt ESMF\_Calendar} class.
!     Compare two calendar objects for equality; return true if equal,
!     false otherwise.  Comparison is based on the calendar type.
!
!     The arguments are:
!     \begin{description}   
!     \item[calendar1]
!          The first {\tt ESMF\_Calendar} in comparison.
!     \item[calendar2]
!          The second {\tt ESMF\_Calendar} in comparison.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
       module procedure ESMF_CalendarEQ      ! internal implementation
!
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_CalendarOperator(==) - Test if Calendar Type 1 is equal to Calendar Type 2
!
! !INTERFACE:
!     interface operator(==)
!     if (calendartype1 == calendartype2) then ... endif
!                  OR
!     result = (calendartype1 == calendartype2)
!
! !RETURN VALUE:
!     logical :: result
!
! !ARGUMENTS:
!     type(ESMF_CalendarType), intent(in) :: calendartype1
!     type(ESMF_CalendarType), intent(in) :: calendartype2
!
! !DESCRIPTION:
!     Overloads the (==) operator for the {\tt ESMF\_Calendar} class.
!     Compare two calendar types for equality; return true if equal,
!     false otherwise.
!
!     The arguments are:
!     \begin{description}   
!     \item[calendartype1]
!          The first {\tt ESMF\_CalendarType} in comparison.
!     \item[calendartype2]
!          The second {\tt ESMF\_CalendarType} in comparison.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
       module procedure ESMF_CalendarTypeEQ      ! internal implementation
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_CalendarOperator(==) - Test if Calendar is equal to Calendar Type
!
! !INTERFACE:
!     interface operator(==)
!     if (calendar == calendartype) then ... endif
!                  OR
!     result = (calendar == calendartype)
!
! !RETURN VALUE:
!     logical :: result
!
! !ARGUMENTS:
!     type(ESMF_Calendar),     intent(in) :: calendar
!     type(ESMF_CalendarType), intent(in) :: calendartype
!
! !DESCRIPTION:
!     Overloads the (==) operator for the {\tt ESMF\_Calendar} class.
!     Compare a calendar object's type with a given calendar type for equality;
!     return true if equal, false otherwise.
!
!     The arguments are:
!     \begin{description}   
!     \item[calendar]
!          The {\tt ESMF\_Calendar} in comparison.
!     \item[calendartype]
!          The {\tt ESMF\_CalendarType} in comparison.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
       module procedure ESMF_CalendarCalAndTypeEQ      ! internal implementation
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_CalendarOperator(==) - Test if Calendar Type is equal to Calendar
!
! !INTERFACE:
!     interface operator(==)
!     if (calendartype == calendar) then ... endif
!                  OR
!     result = (calendartype == calendar)
!
! !RETURN VALUE:
!     logical :: result
!
! !ARGUMENTS:
!     type(ESMF_CalendarType), intent(in) :: calendartype
!     type(ESMF_Calendar),     intent(in) :: calendar
!
! !DESCRIPTION:
!     Overloads the (==) operator for the {\tt ESMF\_Calendar} class.
!     Compare a calendar type with a given calendar object's type for equality;
!     return true if equal, false otherwise.
!
!     The arguments are:
!     \begin{description}   
!     \item[calendartype]
!          The {\tt ESMF\_CalendarType} in comparison.
!     \item[calendar]
!          The {\tt ESMF\_Calendar} in comparison.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
       module procedure ESMF_CalendarTypeAndCalEQ      ! internal implementation
!
       end interface    
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_CalendarOperator(/=) - Test if Calendar 1 is not equal to Calendar 2
!
! !INTERFACE:
      interface operator(/=)
!     if (calendar1 /= calendar2) then ... endif
!                  OR
!     result = (calendar1 /= calendar2)
!
! !RETURN VALUE:
!     logical :: result
!
! !ARGUMENTS:
!     type(ESMF_Calendar), intent(in) :: calendar1
!     type(ESMF_Calendar), intent(in) :: calendar2
!
! !DESCRIPTION:
!     Overloads the (/=) operator for the {\tt ESMF\_Calendar} class.
!     Compare two calendar objects for inequality; return true if not equal,
!     false otherwise.  Comparison is based on the calendar type.
!
!     The arguments are:
!     \begin{description}   
!     \item[calendar1]
!          The first {\tt ESMF\_Calendar} in comparison.
!     \item[calendar2]
!          The second {\tt ESMF\_Calendar} in comparison.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
       module procedure ESMF_CalendarNE      ! internal implementation
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_CalendarOperator(/=) - Test if Calendar Type 1 is not equal to Calendar Type 2
!
! !INTERFACE:
!     interface operator(/=)
!     if (calendartype1 /= calendartype2) then ... endif
!                  OR
!     result = (calendartype1 /= calendartype2)
!
! !RETURN VALUE:
!     logical :: result
!
! !ARGUMENTS:
!     type(ESMF_CalendarType), intent(in) :: calendartype1
!     type(ESMF_CalendarType), intent(in) :: calendartype2
!
! !DESCRIPTION:
!     Overloads the (/=) operator for the {\tt ESMF\_Calendar} class.
!     Compare two calendar types for inequality; return true if not equal,
!     false otherwise.
!
!     The arguments are:
!     \begin{description}   
!     \item[calendartype1]
!          The first {\tt ESMF\_CalendarType} in comparison.
!     \item[calendartype2]
!          The second {\tt ESMF\_CalendarType} in comparison.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
       module procedure ESMF_CalendarTypeNE      ! internal implementation
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_CalendarOperator(/=) - Test if Calendar is not equal to Calendar Type
!
! !INTERFACE:
!     interface operator(/=)
!     if (calendar /= calendartype) then ... endif
!                  OR
!     result = (calendar /= calendartype)
!
! !RETURN VALUE:
!     logical :: result
!
! !ARGUMENTS:
!     type(ESMF_Calendar),     intent(in) :: calendar
!     type(ESMF_CalendarType), intent(in) :: calendartype
!
! !DESCRIPTION:
!     Overloads the (/=) operator for the {\tt ESMF\_Calendar} class.
!     Compare a calendar object's type with a given calendar type for
!     inequality; return true if equal, false otherwise.
!
!     The arguments are:
!     \begin{description}   
!     \item[calendar]
!          The {\tt ESMF\_Calendar} in comparison.
!     \item[calendartype]
!          The {\tt ESMF\_CalendarType} in comparison.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
       module procedure ESMF_CalendarCalAndTypeNE      ! internal implementation
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_CalendarOperator(/=) - Test if Calendar Type is not equal to Calendar
!
! !INTERFACE:
!     interface operator(/=)
!     if (calendartype /= calendar) then ... endif
!                  OR
!     result = (calendartype /= calendar)
!
! !RETURN VALUE:
!     logical :: result
!
! !ARGUMENTS:
!     type(ESMF_CalendarType), intent(in) :: calendartype
!     type(ESMF_Calendar),     intent(in) :: calendar
!
! !DESCRIPTION:
!     Overloads the (/=) operator for the {\tt ESMF\_Calendar} class.
!     Compare a calendar type with a given calendar object's type for
!     inequality; return true if equal, false otherwise.
!
!     The arguments are:
!     \begin{description}   
!     \item[calendartype]
!          The {\tt ESMF\_CalendarType} in comparison.
!     \item[calendar]
!          The {\tt ESMF\_Calendar} in comparison.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
       module procedure ESMF_CalendarTypeAndCalNE      ! internal implementation
!
       end interface    

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_CalendarCreate - Create an ESMF Calendar
!
! !INTERFACE:
      interface ESMF_CalendarCreate

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_CalendarCreateBuiltIn
      module procedure ESMF_CalendarCreateCopy
      module procedure ESMF_CalendarCreateCustom

! !DESCRIPTION:
!     This interface provides a single entry point for {\tt ESMF\_Calendar}
!     Create methods.
!
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_CalendarIsLeapYear - Determine if given year is a leap year
!
! !INTERFACE:
      interface ESMF_CalendarIsLeapYear

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_CalendarIsLeapYearI4
      module procedure ESMF_CalendarIsLeapYearI8

! !DESCRIPTION:
!     This interface provides a single entry point for {\tt ESMF\_Calendar}
!     IsLeapYear methods.
!
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_CalendarSet - Set properties of an ESMF Calendar
!
! !INTERFACE:
      interface ESMF_CalendarSet

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_CalendarSetBuiltIn
      module procedure ESMF_CalendarSetCustom

! !DESCRIPTION:
!     This interface provides a single entry point for {\tt ESMF\_Calendar}
!     Set methods.
!
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_CalendarSetDefault - Set the default ESMF Calendar
!
! !INTERFACE:
      interface ESMF_CalendarSetDefault

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_CalendarSetDefaultType
      module procedure ESMF_CalendarSetDefaultCal

! !DESCRIPTION:
!     This interface provides a single entry point for {\tt ESMF\_Calendar}
!     Set default methods.
!
!EOPI
      end interface
!
!==============================================================================

      contains

!==============================================================================
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CalendarGetInit"
!BOPI
! !IROUTINE:  ESMF_CalendarGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_CalendarGetInit(d)
!
! !ARGUMENTS:
       type(ESMF_Calendar), intent(in), optional :: d
       ESMF_INIT_TYPE :: ESMF_CalendarGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the Deep class {\tt bundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_Calendar} from which to retreive status.
!     \end{description}
!
!EOPI

       if (present(d)) then
         ESMF_CalendarGetInit = ESMF_INIT_GET(d)
       else
         ESMF_CalendarGetInit = ESMF_INIT_CREATED
       endif

    end function ESMF_CalendarGetInit

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CalendarSetInitCreated()"
!BOPI
! !IROUTINE: ESMF_CalendarSetInitCreated - Set Calendar init code to "CREATED"

! !INTERFACE:
  subroutine ESMF_CalendarSetInitCreated(c, rc)
!
! !ARGUMENTS:
    type(ESMF_Calendar), intent(inout)           :: c
    integer,          intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Set init code in Calendar object to "CREATED".
!
!     The arguments are:
!     \begin{description}
!     \item[c] 
!          Specified {\tt ESMF\_Calendar} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    ! Set return code to not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Set init code
    ESMF_INIT_SET_CREATED(c)

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_CalendarSetInitCreated
!------------------------------------------------------------------------------



!==============================================================================
!BOP
! !IROUTINE: ESMF_CalendarCreate - Create a new ESMF Calendar of built-in type

! !INTERFACE:
      ! Private name; call using ESMF_CalendarCreate()
      function ESMF_CalendarCreateBuiltIn(name, calendartype, rc)

! !RETURN VALUE:
      type(ESMF_Calendar) :: ESMF_CalendarCreateBuiltIn

! !ARGUMENTS:
      character (len=*),       intent(in),  optional :: name
      type(ESMF_CalendarType), intent(in)            :: calendartype
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Creates and sets a {\tt calendar} to the given built-in
!     {\tt ESMF\_CalendarType}. 
!
!     This is a private method; invoke via the public overloaded entry point
!     {\tt ESMF\_CalendarCreate()}.
!
!     The arguments are:
!     \begin{description}
!     \item[{[name]}]
!          The name for the newly created calendar.  If not specified, a
!          default unique name will be generated: "CalendarNNN" where NNN
!          is a unique sequence number from 001 to 999.
!     \item[calendartype]
!          The built-in {\tt ESMF\_CalendarType}.  Valid values are:
!            {\tt ESMF\_CAL\_360DAY}, {\tt ESMF\_CAL\_GREGORIAN},
!            {\tt ESMF\_CAL\_JULIAN}, {\tt ESMF\_CAL\_JULIANDAY},
!            {\tt ESMF\_CAL\_NOCALENDAR}, and {\tt ESMF\_CAL\_NOLEAP}.
!          See Section ~\ref{subsec:Calendar_options} for a description of each
!          calendar type.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!    
!EOP

      ! initialize name length to zero for non-existent name
      integer :: nameLen, localrc

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      nameLen = 0

      ! get length of given name for C++ validation
      if (present(name)) then
        nameLen = len_trim(name)
      end if
    
!     invoke C to C++ entry point
      call c_ESMC_CalendarCreateBuiltIn(ESMF_CalendarCreateBuiltIn, nameLen, &
                                        name, calendartype, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    
      ESMF_INIT_SET_CREATED(ESMF_CalendarCreateBuiltIn)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_CalendarCreateBuiltIn
    
!------------------------------------------------------------------------------
!BOP    
! !IROUTINE: ESMF_CalendarCreate - Create a copy of an ESMF Calendar

! !INTERFACE:
      ! Private name; call using ESMF_CalendarCreate()
      function ESMF_CalendarCreateCopy(calendar, rc)

! !RETURN VALUE:
      type(ESMF_Calendar) :: ESMF_CalendarCreateCopy

! !ARGUMENTS:
      type(ESMF_Calendar), intent(in)            :: calendar
      integer,             intent(out), optional :: rc

! !DESCRIPTION:
!     Creates a copy of a given {\tt ESMF\_Calendar}.
!
!     This is a private method; invoke via the public overloaded entry point
!     {\tt ESMF\_CalendarCreate()}.
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!        The {\tt ESMF\_Calendar} to copy.
!     \item[{[rc]}]
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

!     invoke C to C++ entry point to copy calendar
      call c_ESMC_CalendarCreateCopy(ESMF_CalendarCreateCopy, calendar, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ESMF_INIT_SET_CREATED(ESMF_CalendarCreateCopy)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_CalendarCreateCopy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CalendarCreate - Create a new custom ESMF Calendar

! !INTERFACE:
      ! Private name; call using ESMF_CalendarCreate()
      function ESMF_CalendarCreateCustom(name, daysPerMonth, secondsPerDay, &
                                         daysPerYear, daysPerYearDn, &
                                         daysPerYearDd, rc)
! !RETURN VALUE:
      type(ESMF_Calendar) :: ESMF_CalendarCreateCustom

! !ARGUMENTS:
      character (len=*),     intent(in),  optional :: name
      integer, dimension(:), intent(in),  optional :: daysPerMonth
      integer(ESMF_KIND_I4), intent(in),  optional :: secondsPerDay
      integer(ESMF_KIND_I4), intent(in),  optional :: daysPerYear   ! not implemented
      integer(ESMF_KIND_I4), intent(in),  optional :: daysPerYearDn ! not implemented
      integer(ESMF_KIND_I4), intent(in),  optional :: daysPerYearDd ! not implemented
      integer,               intent(out), optional :: rc

! !DESCRIPTION:
!     Creates a custom {\tt ESMF\_Calendar} and sets its properties.
!
!     This is a private method; invoke via the public overloaded entry point
!     {\tt ESMF\_CalendarCreate()}.
!
!     The arguments are:
!     \begin{description}
!     \item[{[name]}]
!          The name for the newly created calendar.  If not specified, a
!          default unique name will be generated: "CalendarNNN" where NNN
!          is a unique sequence number from 001 to 999.
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
!          for any planetary body.  Default = 0.  (Not implemented yet).
!     \item[{[daysPerYearDn]}]
!          Integer numerator portion of fractional number of days per year
!          (daysPerYearDn/daysPerYearDd).
!          Use with daysPerYear (see above) and daysPerYearDd (see below) to
!          specify a days-per-year calendar for any planetary body.
!          Default = 0.  (Not implemented yet).
!     \item[{[daysPerYearDd]}]
!          Integer denominator portion of fractional number of days per year
!          (daysPerYearDn/daysPerYearDd).
!          Use with daysPerYear and daysPerYearDn (see above) to
!          specify a days-per-year calendar for any planetary body.
!          Default = 1.  (Not implemented yet).
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!EOP

      ! initialize name length to zero for non-existent name
      !   and initialize number of months per year to zero for not-present
      !   daysPerMonth
      integer :: nameLen, monthsPerYear
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      nameLen = 0
      monthsPerYear = 0

      ! get length of given name for C++ validation
      if (present(name)) then
        nameLen = len_trim(name)
      end if

      ! get size of given daysPerMonth array for C++ validation
      if (present(daysPerMonth)) then
        monthsPerYear = size(daysPerMonth)
      end if

!     invoke C to C++ entry point

      if (present(daysPerMonth)) then
        call c_ESMC_CalendarCreateCustom1(ESMF_CalendarCreateCustom, &
                                         nameLen, name, &
                                         daysPerMonth(1), monthsPerYear, &
                                         secondsPerDay, &
                                         daysPerYear, daysPerYearDn, &
                                         daysPerYearDd, localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_CalendarCreateCustom0(ESMF_CalendarCreateCustom, &
                                         nameLen, name, &
                                         monthsPerYear, &
                                         secondsPerDay, &
                                         daysPerYear, daysPerYearDn, &
                                         daysPerYearDd, localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      end if
    
      ESMF_INIT_SET_CREATED(ESMF_CalendarCreateCustom)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_CalendarCreateCustom
    
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CalendarDestroy - Free resources associated with a Calendar
!
! !INTERFACE:
      subroutine ESMF_CalendarDestroy(calendar, rc)
!
! !ARGUMENTS:
      type(ESMF_Calendar) :: calendar
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Releases all resources associated with this {\tt ESMF\_Calendar}.
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!       Destroy contents of this {\tt ESMF\_Calendar}.
!     \item[[rc]]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ESMF_INIT_CHECK_DEEP(ESMF_CalendarGetInit,calendar,rc)

!     invoke C to C++ entry point
      call c_ESMC_CalendarDestroy(calendar, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ESMF_INIT_SET_DELETED(calendar)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_CalendarDestroy

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_CalendarFinalize
!
! !INTERFACE:
      subroutine ESMF_CalendarFinalize(rc)
!
! !ARGUMENTS:
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Releases all internal built-in calendars.
!
!     The arguments are:
!     \begin{description}
!     \item[[rc]]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

!     invoke C to C++ entry point
      call c_ESMC_CalendarFinalize(localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_CalendarFinalize

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CalendarGet - Get Calendar properties

! !INTERFACE:
      subroutine ESMF_CalendarGet(calendar, name, calendartype, &
                                  daysPerMonth, monthsPerYear, &
                                  secondsPerDay, secondsPerYear, &
                                  daysPerYear, &
                                  daysPerYearDn, daysPerYearDd, rc)
! !ARGUMENTS:
      type(ESMF_Calendar),     intent(inout)         :: calendar
      character (len=*),       intent(out), optional :: name
      type(ESMF_CalendarType), intent(out), optional :: calendartype
      integer, dimension(:),   intent(out), optional :: daysPerMonth
      integer,                 intent(out), optional :: monthsPerYear
      integer(ESMF_KIND_I4),   intent(out), optional :: secondsPerDay
      integer(ESMF_KIND_I4),   intent(out), optional :: secondsPerYear
      integer(ESMF_KIND_I4),   intent(out), optional :: daysPerYear   ! not implemented
      integer(ESMF_KIND_I4),   intent(out), optional :: daysPerYearDn ! not implemented
      integer(ESMF_KIND_I4),   intent(out), optional :: daysPerYearDd ! not implemented
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Gets one or more of an {\tt ESMF\_Calendar}'s properties.
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          The object instance to query.
!     \item[{[name]}]
!          The name of this calendar.
!     \item[{[calendartype]}]
!          The {\tt CalendarType} ESMF\_CAL\_GREGORIAN, ESMF\_CAL\_JULIAN,
!          etc.
!     \item[{[daysPerMonth]}]
!          Integer array of days per month, for each month of the year.
!     \item[{[monthsPerYear]}]
!          Integer number of months per year; the size of the
!          daysPerMonth array.
!     \item[{[secondsPerDay]}]
!          Integer number of seconds per day.
!     \item[{[secondsPerYear]}]
!          Integer number of seconds per year.
!     \item[{[daysPerYear]}]
!          Integer number of days per year.  For calendars with
!          intercalations, daysPerYear is the number of days for years without
!          an intercalation.  For other calendars, it is the number of days in
!          every year.  (Not implemented yet).
!     \item[{[daysPerYearDn]}]
!          Integer fractional number of days per year (numerator).
!          For calendars with intercalations, daysPerYearDn/daysPerYearDd is
!          the average fractional number of days per year (e.g. 25/100 for
!          Julian 4-year intercalation).  For other calendars, it is zero.
!          (Not implemented yet).
!     \item[{[daysPerYearDd]}]
!          Integer fractional number of days per year (denominator).  See
!          daysPerYearDn above.  (Not implemented yet).
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!EOP

      ! temp name for C++ to fill
      character (len=ESMF_MAXSTR) :: tempName

      ! initialize name lengths to zero for non-existent name
      !   and initialize daysPerMonth list size to zero for not-present list
      integer :: nameLen
      integer :: tempNameLen
      integer :: sizeofDaysPerMonth
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      nameLen = 0
      tempNameLen = 0
      sizeofDaysPerMonth = 0

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_CalendarGetInit,calendar,rc)

      ! get length of given name for C++ validation
      if (present(name)) then
        nameLen = len(name)
      end if

      ! get size of given daysPerMonth list for C++ validation
      if (present(daysPerMonth)) then   
        sizeofDaysPerMonth = size(daysPerMonth)
      end if

!     invoke C to C++ entry point

      if (present(daysPerMonth)) then
        call c_ESMC_CalendarGet1(calendar, nameLen, tempNameLen, tempName, &
                         calendartype, &
                         daysPerMonth(1), sizeofDaysPerMonth, &
                         monthsPerYear, secondsPerDay, secondsPerYear, &
                         daysPerYear, daysPerYearDn, daysPerYearDd, localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_CalendarGet0(calendar, nameLen, tempNameLen, tempName, &
                         calendartype, &
                         sizeofDaysPerMonth, &
                         monthsPerYear, secondsPerDay, secondsPerYear, &
                         daysPerYear, daysPerYearDn, daysPerYearDd, localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      end if
    
      ! copy temp name back to given name to restore native Fortran
      !   storage style
      if (present(name)) then
        name = tempName(1:tempNameLen)
      endif

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_CalendarGet
    
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_CalendarInitialize - Initialize the default Calendar type

! !INTERFACE:
      subroutine ESMF_CalendarInitialize(calendartype, rc)

! !ARGUMENTS:
      type(ESMF_CalendarType), intent(in),  optional :: calendartype
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Initializes the default {\tt calendar} to the given type.  Subsequent
!     Time Manager operations requiring a calendar where one isn't specified
!     will use the default internal calendar of this type.
!
!     The arguments are:
!     \begin{description}
!     \item[{[calendartype]}]
!          The calendar type to initialize the default to.  If not specified,
!          the default is set to {\tt ESMF\_CAL\_NOCALENDAR}.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!    
!EOPI
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL
    
!     invoke C to C++ entry point
      call c_ESMC_CalendarInitialize(calendartype, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_CalendarInitialize
    
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CalendarIsLeapYear - Determine if given year is a leap year

! !INTERFACE:
      ! Private name; call using ESMF_CalendarIsLeapYear()
      function ESMF_CalendarIsLeapYearI4(calendar, yy, rc)

! !RETURN VALUE:
      logical :: ESMF_CalendarIsLeapYearI4

! !ARGUMENTS:
      type(ESMF_Calendar),   intent(inout)         :: calendar
      integer(ESMF_KIND_I4), intent(in)            :: yy
      integer,               intent(out), optional :: rc

! !DESCRIPTION:
!     Returns true if the given year is a leap year within the given calendar,
!     and false otherwise.  See also {\tt ESMF\_TimeIsLeapYear()}.
!
!     This is a private method; invoke via the public overloaded entry point
!     {\tt ESMF\_CalendarIsLeapYear()}.
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          {\tt ESMF\_Calendar} to determine leap year within.
!     \item[yy]
!          Year to check for leap year.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!    
!EOP
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_CalendarGetInit,calendar,rc)

!     invoke C to C++ entry point
      call c_ESMC_CalendarIsLeapYearI4(calendar, yy, &
                                       ESMF_CalendarIsLeapYearI4, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS    
      end function ESMF_CalendarIsLeapYearI4
    
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CalendarIsLeapYear - Determine if given year is a leap year

! !INTERFACE:
      ! Private name; call using ESMF_CalendarIsLeapYear()
      function ESMF_CalendarIsLeapYearI8(calendar, yy_i8, rc)

! !RETURN VALUE:
      logical :: ESMF_CalendarIsLeapYearI8

! !ARGUMENTS:
      type(ESMF_Calendar),   intent(inout)         :: calendar
      integer(ESMF_KIND_I8), intent(in)            :: yy_i8
      integer,               intent(out), optional :: rc

! !DESCRIPTION:
!     Returns true if the given year is a leap year within the given calendar,
!     and false otherwise.  See also {\tt ESMF\_TimeIsLeapYear()}.
!
!     This is a private method; invoke via the public overloaded entry point
!     {\tt ESMF\_CalendarIsLeapYear()}.
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          {\tt ESMF\_Calendar} to determine leap year within.
!     \item[yy\_i8]
!          Year to check for leap year.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!    
!EOP
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL
    
      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_CalendarGetInit,calendar,rc)

!     invoke C to C++ entry point
      call c_ESMC_CalendarIsLeapYearI8(calendar, yy_i8, &
                                       ESMF_CalendarIsLeapYearI8, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS 
      end function ESMF_CalendarIsLeapYearI8
    
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_CalendarPrint - Print the contents of a Calendar

! !INTERFACE:
      subroutine ESMF_CalendarPrint(calendar, options, rc)

! !ARGUMENTS:
      type(ESMF_Calendar), intent(inout)         :: calendar
      character (len=*),   intent(in),  optional :: options
      integer,             intent(out), optional :: rc

! !DESCRIPTION:
!     Prints out an {\tt ESMF\_Calendar}'s properties to {\tt stdio}, 
!     in support of testing and debugging.  The options control the 
!     type of information and level of detail. 
!
!     Note:  Many {\tt ESMF\_<class>Print} methods are implemented in C++.
!     On some platforms/compilers there is a potential issue with interleaving
!     Fortran and C++ output to {\tt stdout} such that it doesn't appear in
!     the expected order.  If this occurs, it is recommended to use the
!     standard Fortran call {\tt flush(6)} as a workaround until this issue
!     is fixed in a future release. 
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          {\tt ESMF\_Calendar} to be printed out.
!     \item[{[options]}]
!          Print options. If none specified, prints all calendar property
!                             values. \\
!          "calendartype"   - print the calendar's type 
!                               (e.g. ESMF\_CAL\_GREGORIAN). \\
!          "daysPerMonth"   - print the array of number of days for
!                               each month. \\
!          "daysPerYear"    - print the number of days per year
!                             (integer and fractional parts). \\
!          "monthsPerYear"  - print the number of months per year. \\
!          "name"           - print the calendar's name. \\
!          "secondsPerDay"  - print the number of seconds in a day. \\
!          "secondsPerYear" - print the number of seconds in a year. \\
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL
 
      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_CalendarGetInit,calendar,rc)

!     invoke C to C++ entry point
      call c_ESMC_CalendarPrint(calendar, options, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_CalendarPrint
      
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE:  ESMF_CalendarReadRestart - Restore the contents of a Calendar (not implemented)

! !INTERFACE:
      function ESMF_CalendarReadRestart(name, iospec, rc)
! 
! !RETURN VALUE:
      type(ESMF_Calendar) :: ESMF_CalendarReadRestart
!
! !ARGUMENTS:
      character (len=*),   intent(in)            :: name
      type(ESMF_IOSpec),   intent(in),  optional :: iospec
      integer,             intent(out), optional :: rc

! !DESCRIPTION:
!     Restores an {\tt ESMF\_Calendar} object from the last call to
!     {\tt ESMF\_CalendarWriteRestart()}.  (Not implemented yet).
!
!     The arguments are:
!     \begin{description}
!     \item[name]
!          The name of the object instance to restore.
!     \item[{[iospec]}]
!          The IO specification of the restart file.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
      integer :: nameLen, localrc

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! get length of given name for C++ validation
      nameLen = len_trim(name)

!     invoke C to C++ entry point to allocate and restore calendar
      call c_ESMC_CalendarReadRestart(ESMF_CalendarReadRestart, nameLen, name, &
                                      iospec, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ESMF_INIT_SET_CREATED(ESMF_CalendarReadRestart)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS 
      end function ESMF_CalendarReadRestart

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CalendarSet - Set a Calendar to a built-in type

! !INTERFACE:
      ! Private name; call using ESMF_CalendarSet()
      subroutine ESMF_CalendarSetBuiltIn(calendar, name, calendartype, rc)

! !ARGUMENTS:
      type(ESMF_Calendar),     intent(inout)         :: calendar
      character (len=*),       intent(in),  optional :: name
      type(ESMF_CalendarType), intent(in)            :: calendartype
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Sets {\tt calendar} to the given built-in {\tt ESMF\_CalendarType}. 
!
!     This is a private method; invoke via the public overloaded entry point
!     {\tt ESMF\_CalendarSet()}.
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          The object instance to initialize.
!     \item[{[name]}]
!          The new name for this calendar.
!     \item[calendartype]
!          The built-in {\tt CalendarType}.  Valid values are:
!            {\tt ESMF\_CAL\_360DAY}, {\tt ESMF\_CAL\_GREGORIAN},
!            {\tt ESMF\_CAL\_JULIAN}, {\tt ESMF\_CAL\_JULIANDAY},
!            {\tt ESMF\_CAL\_NOCALENDAR}, and {\tt ESMF\_CAL\_NOLEAP}.
!          See Section ~\ref{subsec:Calendar_options} for a description of each
!          calendar type.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!    
!EOP
    
      ! initialize name length to zero for non-existent name
      integer :: nameLen, localrc

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      nameLen = 0

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_CalendarGetInit,calendar,rc)

      ! get length of given name for C++ validation
      if (present(name)) then
        nameLen = len_trim(name)
      end if
    
!     invoke C to C++ entry point
      call c_ESMC_CalendarSetBuiltIn(calendar, nameLen, name, calendartype, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_CalendarSetBuiltIn
    
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CalendarSet - Set properties of a custom Calendar

! !INTERFACE:
      ! Private name; call using ESMF_CalendarSet()
      subroutine ESMF_CalendarSetCustom(calendar, name, daysPerMonth, &
                                        secondsPerDay, &
                                        daysPerYear, daysPerYearDn, &
                                        daysPerYearDd, rc)
! !ARGUMENTS:
      type(ESMF_Calendar),   intent(inout)         :: calendar
      character (len=*),     intent(in),  optional :: name
      integer, dimension(:), intent(in),  optional :: daysPerMonth
      integer(ESMF_KIND_I4), intent(in),  optional :: secondsPerDay
      integer(ESMF_KIND_I4), intent(in),  optional :: daysPerYear   ! not implemented
      integer(ESMF_KIND_I4), intent(in),  optional :: daysPerYearDn ! not implemented
      integer(ESMF_KIND_I4), intent(in),  optional :: daysPerYearDd ! not implemented
      integer,               intent(out), optional :: rc

! !DESCRIPTION:
!     Sets properties in a custom {\tt ESMF\_Calendar}.
!
!     This is a private method; invoke via the public overloaded entry point
!     {\tt ESMF\_CalendarSet()}.
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          The object instance to initialize.
!     \item[{[name]}]
!          The new name for this calendar.
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
!          for any planetary body.  Default = 0.  (Not implemented yet).
!     \item[{[daysPerYearDn]}]
!          Integer numerator portion of fractional number of days per year
!          (daysPerYearDn/daysPerYearDd).
!          Use with daysPerYear (see above) and daysPerYearDd (see below) to
!          specify a days-per-year calendar for any planetary body.
!          Default = 0.  (Not implemented yet).
!     \item[{[daysPerYearDd]}]
!          Integer denominator portion of fractional number of days per year
!          (daysPerYearDn/daysPerYearDd).
!          Use with daysPerYear and daysPerYearDn (see above) to
!          specify a days-per-year calendar for any planetary body.
!          Default = 1.  (Not implemented yet).
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!EOP

      ! initialize name length to zero for non-existent name
      !   and initialize number of months per year to zero for not-present
      !   daysPerMonth
      integer :: nameLen
      integer :: monthsPerYear
      integer :: localrc                        ! local return code

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_CalendarGetInit,calendar,rc)

      nameLen = 0
      monthsPerYear = 0

      ! get length of given name for C++ validation
      if (present(name)) then
        nameLen = len_trim(name)
      end if

      ! get size of given daysPerMonth array for C++ validation
      if (present(daysPerMonth)) then
        monthsPerYear = size(daysPerMonth)
      end if

!     invoke C to C++ entry point

      if (present(daysPerMonth)) then
        call c_ESMC_CalendarSetCustom1(calendar, &
                                      nameLen, name, &
                                      daysPerMonth(1), monthsPerYear, &
                                      secondsPerDay, &
                                      daysPerYear, daysPerYearDn, &
                                      daysPerYearDd, localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_CalendarSetCustom0(calendar, &
                                      nameLen, name, &
                                      monthsPerYear, &
                                      secondsPerDay, &
                                      daysPerYear, daysPerYearDn, &
                                      daysPerYearDd, localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      end if

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_CalendarSetCustom
    
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CalendarSetDefault - Set the default Calendar type

! !INTERFACE:
      ! Private name; call using ESMF_CalendarSetDefault()
      subroutine ESMF_CalendarSetDefaultType(calendartype, rc)

! !ARGUMENTS:
      type(ESMF_CalendarType), intent(in)            :: calendartype
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Sets the default {\tt calendar} to the given type.  Subsequent Time
!     Manager operations requiring a calendar where one isn't specified will
!     use the internal calendar of this type.
!
!     This is a private method; invoke via the public overloaded entry point
!     {\tt ESMF\_CalendarSetDefault()}.
!
!     The arguments are:
!     \begin{description}
!     \item[calendartype]
!          The calendar type to be the default.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!    
!EOP
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL
    
!     invoke C to C++ entry point
      call c_ESMC_CalendarSetDefaultType(calendartype, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_CalendarSetDefaultType
    
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CalendarSetDefault - Set the default Calendar

! !INTERFACE:
      ! Private name; call using ESMF_CalendarSetDefault()
      subroutine ESMF_CalendarSetDefaultCal(calendar, rc)

! !ARGUMENTS:
      type(ESMF_Calendar),     intent(inout)            :: calendar
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Sets the default {\tt calendar} to the one given.  Subsequent Time
!     Manager operations requiring a calendar where one isn't specified will
!     use this calendar.
!
!     This is a private method; invoke via the public overloaded entry point
!     {\tt ESMF\_CalendarSetDefault()}.
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          The object instance to be the default.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!    
!EOP
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL 

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_CalendarGetInit,calendar,rc)

!     invoke C to C++ entry point
      call c_ESMC_CalendarSetDefaultCal(calendar, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS 
      end subroutine ESMF_CalendarSetDefaultCal
    
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_CalendarValidate - Validate a Calendar's properties

! !INTERFACE:
      subroutine ESMF_CalendarValidate(calendar, options, rc)
 
! !ARGUMENTS:
      type(ESMF_Calendar), intent(inout)         :: calendar
      character (len=*),   intent(in),  optional :: options
      integer,             intent(out), optional :: rc

! !DESCRIPTION:
!     Checks whether a {\tt calendar} is valid.  
!     Must be one of the defined calendar types.  daysPerMonth, daysPerYear,
!     secondsPerDay must all be greater than or equal to zero.
! 
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          {\tt ESMF\_Calendar} to be validated.
!     \item[{[options]}]
!          Validation options are not yet supported.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL
      
      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_CalendarGetInit,calendar,rc)

!     invoke C to C++ entry point
      call c_ESMC_CalendarValidate(calendar, options, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_CalendarValidate

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE:  ESMF_CalendarWriteRestart - Save the contents of a Calendar (not implemented)

! !INTERFACE:
      subroutine ESMF_CalendarWriteRestart(calendar, iospec, rc)

! !ARGUMENTS:
      type(ESMF_Calendar), intent(inout)         :: calendar
      type(ESMF_IOSpec),   intent(in),  optional :: iospec
      integer,             intent(out), optional :: rc

! !DESCRIPTION:  
!     Saves an {\tt ESMF\_Calendar} object.  Default options are to select the
!     fastest way to save to disk.  (Not implemented yet).
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
!EOPI
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_CalendarGetInit,calendar,rc)

!     invoke C to C++ entry point 
      call c_ESMC_CalendarWriteRestart(calendar, iospec, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_CalendarWriteRestart

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE:  ESMF_CalendarEQ - Compare two Calendars for equality
!
! !INTERFACE:
      function ESMF_CalendarEQ(calendar1, calendar2)
! 
! !RETURN VALUE:
      logical :: ESMF_CalendarEQ

! !ARGUMENTS:
      type(ESMF_Calendar), intent(in) :: calendar1
      type(ESMF_Calendar), intent(in) :: calendar2

! !DESCRIPTION:
!     This method overloads the (==) operator for the {\tt ESMF\_Calendar}
!     class.  See "interface operator(==)" above for complete description.
!
!EOPI
!     invoke C to C++ entry point
      call c_ESMC_CalendarEQ(calendar1, calendar2, ESMF_CalendarEQ)

      end function ESMF_CalendarEQ

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE:  ESMF_CalendarTypeEQ - Compare two Calendar types for equality
!
! !INTERFACE:
      function ESMF_CalendarTypeEQ(calendartype1, calendartype2)
! 
! !RETURN VALUE:
      logical :: ESMF_CalendarTypeEQ

! !ARGUMENTS:
      type(ESMF_CalendarType), intent(in) :: calendartype1
      type(ESMF_CalendarType), intent(in) :: calendartype2

! !DESCRIPTION:
!     This method overloads the (==) operator for the {\tt ESMF\_Calendar}
!     class.  See "interface operator(==)" above for complete description.
!             
!EOPI
!     invoke C to C++ entry point
      call c_ESMC_CalendarTypeEQ(calendartype1, calendartype2, &
                                 ESMF_CalendarTypeEQ)

      end function ESMF_CalendarTypeEQ

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE:  ESMF_CalendarCalAndTypeEQ - Compare a Calendar and Calendar type for equality
!
! !INTERFACE:
      function ESMF_CalendarCalAndTypeEQ(calendar, calendartype)
! 
! !RETURN VALUE:
      logical :: ESMF_CalendarCalAndTypeEQ

! !ARGUMENTS:
      type(ESMF_Calendar),     intent(in) :: calendar
      type(ESMF_CalendarType), intent(in) :: calendartype

! !DESCRIPTION:
!     This method overloads the (==) operator for the {\tt ESMF\_Calendar}
!     class.  See "interface operator(==)" above for complete description.
!             
!EOPI
!     invoke C to C++ entry point
      call c_ESMC_CalendarCalAndTypeEQ(calendar, calendartype, &
                                       ESMF_CalendarCalAndTypeEQ)

      end function ESMF_CalendarCalAndTypeEQ

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE:  ESMF_CalendarTypeAndCalEQ - Compare a Calendar type and Calendar for equality
!
! !INTERFACE:
      function ESMF_CalendarTypeAndCalEQ(calendartype, calendar)
! 
! !RETURN VALUE:
      logical :: ESMF_CalendarTypeAndCalEQ

! !ARGUMENTS:
      type(ESMF_CalendarType), intent(in) :: calendartype
      type(ESMF_Calendar),     intent(in) :: calendar

! !DESCRIPTION:
!     This method overloads the (==) operator for the {\tt ESMF\_Calendar}
!     class.  See "interface operator(==)" above for complete description.
!             
!EOPI
!     invoke C to C++ entry point
      call c_ESMC_CalendarTypeAndCalEQ(calendartype, calendar, &
                                       ESMF_CalendarTypeAndCalEQ)

      end function ESMF_CalendarTypeAndCalEQ

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE:  ESMF_CalendarNE - Compare two Calendars for inequality
!
! !INTERFACE:
      function ESMF_CalendarNE(calendar1, calendar2)
! 
! !RETURN VALUE:
      logical :: ESMF_CalendarNE

! !ARGUMENTS:
      type(ESMF_Calendar), intent(in) :: calendar1
      type(ESMF_Calendar), intent(in) :: calendar2

! !DESCRIPTION:
!     This method overloads the (/=) operator for the {\tt ESMF\_Calendar}
!     class.  See "interface operator(/=)" above for complete description.
!             
!EOPI
!     invoke C to C++ entry point
      call c_ESMC_CalendarNE(calendar1, calendar2, ESMF_CalendarNE)

      end function ESMF_CalendarNE

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE:  ESMF_CalendarTypeNE - Compare two Calendar types for inequality
!
! !INTERFACE:
      function ESMF_CalendarTypeNE(calendartype1, calendartype2)
! 
! !RETURN VALUE:
      logical :: ESMF_CalendarTypeNE

! !ARGUMENTS:
      type(ESMF_CalendarType), intent(in) :: calendartype1
      type(ESMF_CalendarType), intent(in) :: calendartype2

! !DESCRIPTION:
!     This method overloads the (/=) operator for the {\tt ESMF\_Calendar}
!     class.  See "interface operator(/=)" above for complete description.
!             
!EOPI
!     invoke C to C++ entry point
      call c_ESMC_CalendarTypeNE(calendartype1, calendartype2, &
                                 ESMF_CalendarTypeNE)

      end function ESMF_CalendarTypeNE

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE:  ESMF_CalendarCalAndTypeNE - Compare a Calendar and Calendar type for inequality
!
! !INTERFACE:
      function ESMF_CalendarCalAndTypeNE(calendar, calendartype)
! 
! !RETURN VALUE:
      logical :: ESMF_CalendarCalAndTypeNE

! !ARGUMENTS:
      type(ESMF_Calendar),     intent(in) :: calendar
      type(ESMF_CalendarType), intent(in) :: calendartype

! !DESCRIPTION:
!     This method overloads the (/=) operator for the {\tt ESMF\_Calendar}
!     class.  See "interface operator(/=)" above for complete description.
!             
!EOPI
!     invoke C to C++ entry point
      call c_ESMC_CalendarCalAndTypeNE(calendar, calendartype, &
                                       ESMF_CalendarCalAndTypeNE)

      end function ESMF_CalendarCalAndTypeNE

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE:  ESMF_CalendarTypeAndCalNE - Compare a Calendar type and Calendar for inequality
!
! !INTERFACE:
      function ESMF_CalendarTypeAndCalNE(calendartype, calendar)
! 
! !RETURN VALUE:
      logical :: ESMF_CalendarTypeAndCalNE

! !ARGUMENTS:
      type(ESMF_CalendarType), intent(in) :: calendartype
      type(ESMF_Calendar),     intent(in) :: calendar

! !DESCRIPTION:
!     This method overloads the (/=) operator for the {\tt ESMF\_Calendar}
!     class.  See "interface operator(/=)" above for complete description.
!             
!EOPI
!     invoke C to C++ entry point
      call c_ESMC_CalendarTypeAndCalNE(calendartype, calendar, &
                                       ESMF_CalendarTypeAndCalNE)

      end function ESMF_CalendarTypeAndCalNE

!------------------------------------------------------------------------------

      end module ESMF_CalendarMod
