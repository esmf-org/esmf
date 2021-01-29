! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
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
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_CalendarMod
!
! !DESCRIPTION:
! Part of Time Manager Fortran API wrapper of C++ implementation.
!
! Defines Fortran wrapper entry points for corresponding
! C++ class { \tt ESMC\_Calendar} implementation.
!
! See {\tt ../include/ESMC\_Calendar.h} for complete description.
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from ESMF base class
      use ESMF_BaseMod
      use ESMF_UtilTypesMod
      use ESMF_InitMacrosMod
      use ESMF_LogErrMod
      use ESMF_IOUtilMod

      implicit none
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!     ! ESMF_CalKind_Flag
!
!     ! Fortran "enum" type to match C++ ESMC_CalKind_Flag enum

      ! TODO: add isInit init macro infrastructure to initialize calkindflag ?
      !       to 0 (to detect unset) or possibly 8 (default 
      !       ESMF_CALKIND_NOCALENDAR)
      type ESMF_CalKind_Flag
#ifndef ESMF_NO_SEQUENCE
      sequence
#endif
      private
        integer :: calkindflag
      end type

      type(ESMF_CalKind_Flag), parameter :: &
                            ESMF_CALKIND_GREGORIAN =    ESMF_CalKind_Flag(1), &
                            ESMF_CALKIND_JULIAN =       ESMF_CalKind_Flag(2), &
                            ESMF_CALKIND_JULIANDAY =    ESMF_CalKind_Flag(3), &
                            ESMF_CALKIND_MODJULIANDAY = ESMF_CalKind_Flag(4), &
                           ! like Gregorian, except Feb always has 28 days
                            ESMF_CALKIND_NOLEAP =       ESMF_CalKind_Flag(5), & 
                           ! 12 months, 30 days each
                            ESMF_CALKIND_360DAY =       ESMF_CalKind_Flag(6), & 
                           ! user defined
                            ESMF_CALKIND_CUSTOM =       ESMF_CalKind_Flag(7), &
                           ! track base time seconds only
                            ESMF_CALKIND_NOCALENDAR =   ESMF_CalKind_Flag(8)

!------------------------------------------------------------------------------
!     ! ESMF_Calendar
!
      type ESMF_Calendar
#ifndef ESMF_NO_SEQUENCE
      sequence
#endif
      private
        type(ESMF_Pointer) :: this
        ESMF_INIT_DECLARE
      end type
!
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_CalKind_Flag
      public ESMF_CALKIND_GREGORIAN, ESMF_CALKIND_JULIAN, &
             ESMF_CALKIND_JULIANDAY, ESMF_CALKIND_MODJULIANDAY, &
             ESMF_CALKIND_NOLEAP,    ESMF_CALKIND_360DAY, &
             ESMF_CALKIND_CUSTOM,    ESMF_CALKIND_NOCALENDAR
      public ESMF_Calendar
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:
      public operator(==)
      public operator(/=)
      public assignment(=)
      public ESMF_CalendarCreate
      public ESMF_CalendarDestroy
      public ESMF_CalendarFinalize
      public ESMF_CalendarGet
      public ESMF_CalendarInitialize
      public ESMF_CalendarIsCreated
      public ESMF_CalendarIsLeapYear
      public ESMF_CalendarPrint
      public ESMF_CalendarReadRestart
      public ESMF_CalendarSet
      public ESMF_CalendarSetDefault
      public ESMF_CalendarValidate
      public ESMF_CalendarWriteRestart

! - ESMF-internal methods:
      public ESMF_CalendarGetInit
      public ESMF_CalendarSetInitCreated

!EOPI

! !PRIVATE MEMBER FUNCTIONS:
      private ESMF_CalendarEQ
      private ESMF_CalendarNE
      private ESMF_CalendarKindEQ
      private ESMF_CalendarKindNE
      private ESMF_CalendarCalAndKindEQ
      private ESMF_CalendarCalAndKindNE
      private ESMF_CalendarKindAndCalEQ
      private ESMF_CalendarKindAndCalNE
      private ESMF_CalendarCreateBuiltIn
      private ESMF_CalendarCreateCopy
      private ESMF_CalendarCreateCustom
      private ESMF_CalendarIsLeapYearI4
      private ESMF_CalendarIsLeapYearI8
      private ESMF_CalendarSetBuiltIn
      private ESMF_CalendarSetCustom
      private ESMF_CalendarSetDefaultKind
      private ESMF_CalendarSetDefaultCal

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id$'

!==============================================================================
! 
! INTERFACE BLOCKS
! 
!==============================================================================

interface assignment (=)
  module procedure ESMF_CalKindToInt
  module procedure ESMF_IntToCalKind
end interface

!==============================================================================
!BOP
! !IROUTINE:  ESMF_CalendarAssignment(=) - Assign a Calendar to another Calendar
!
! !INTERFACE:
!     interface assignment(=)
!     calendar1 = calendar2
!
! !ARGUMENTS:
!     type(ESMF_Calendar) :: calendar1
!     type(ESMF_Calendar) :: calendar2
! 
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Assign {\tt calendar1} as an alias to the same {\tt ESMF\_Calendar} 
!     object in memory as {\tt calendar2}. If {\tt calendar2} is invalid, then 
!     {\tt calendar1} will be equally invalid after the assignment.
!
!     The arguments are:
!     \begin{description} 
!     \item[calendar1] 
!          The {\tt ESMF\_Calendar} object on the left hand side of the 
!          assignment.
!     \item[calendar2] 
!          The {\tt ESMF\_Calendar} object on the right hand side of the 
!          assignment.
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
!BOP
! !IROUTINE:  ESMF_CalendarOperator(==) - Test if Calendar argument 1 is equal to Calendar argument 2
!
! !INTERFACE:
      interface operator(==)
!     if (<calendar argument 1> == <calendar argument 2>) then ... endif
!                                 OR
!     result = (<calendar argument 1> == <calendar argument 2>)
!
! !RETURN VALUE:
!     logical :: result
!
! !ARGUMENTS:
!     <calendar argument 1>, see below for supported values
!     <calendar argument 2>, see below for supported values
!
! !DESCRIPTION:
!     \begin{sloppypar}
!     Overloads the (==) operator for the {\tt ESMF\_Calendar} class.
!     Compare an {\tt ESMF\_Calendar} object or {\tt ESMF\_CalKind\_Flag} with
!     another calendar object or calendar kind for equality.  Return
!     {\tt .true.} if equal, {\tt .false.} otherwise.  Comparison is based on
!     calendar kind, which is a property of a calendar object.
!     \end{sloppypar}
!
!     If both arguments are {\tt ESMF\_Calendar} objects, and both are of  
!     type {\tt ESMF\_CALKIND\_CUSTOM}, then all the calendar's properties, 
!     except name, are compared.
!
!     If both arguments are {\tt ESMF\_Calendar} objects, and either of them
!     is not in the {\tt ESMF\_INIT\_CREATED} status, an error will be logged.
!     However, this does not affect the return value, which is {\tt .true.} 
!     when both arguments are in the {\em same} status, and {\tt .false.}
!     otherwise.
!
!     If one argument is an {\tt ESMF\_Calendar} object, and the other is an
!     {\tt ESMF\_CalKind\_Flag}, and the calendar object is not in the
!     {\tt ESMF\_INIT\_CREATED} status, an error will be logged and
!     {\tt .false.} will be returned.
!
!     Supported values for <calendar argument 1> are:
!     \begin{description}
!     \item type(ESMF\_Calendar),     intent(in) :: calendar1
!     \item type(ESMF\_CalKind\_Flag), intent(in) :: calkindflag1
!     \end{description}
!     Supported values for <calendar argument 2> are:
!     \begin{description}
!     \item type(ESMF\_Calendar),     intent(in) :: calendar2
!     \item type(ESMF\_CalKind\_Flag), intent(in) :: calkindflag2
!     \end{description}
!
!     The arguments are:
!     \begin{description}   
!     \item[<calendar argument 1>]
!          The {\tt ESMF\_Calendar} object or {\tt ESMF\_CalKind\_Flag} on the
!          left hand side of the equality operation.
!     \item[<calendar argument 2>]
!          The {\tt ESMF\_Calendar} object or {\tt ESMF\_CalKind\_Flag} on the
!          right hand side of the equality operation.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_CalendarEQ              ! internal implementation
      module procedure ESMF_CalendarKindEQ          ! internal implementation
      module procedure ESMF_CalendarCalAndKindEQ    ! internal implementation
      module procedure ESMF_CalendarKindAndCalEQ    ! internal implementation
!
      end interface
!
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_CalendarOperator(/=) - Test if Calendar argument 1 is not equal to Calendar argument 2
!
! !INTERFACE:
      interface operator(/=)
!     if (<calendar argument 1> /= <calendar argument 2>) then ... endif
!                                 OR
!     result = (<calendar argument 1> /= <calendar argument 2>)
!
! !RETURN VALUE:
!     logical :: result
!
! !ARGUMENTS:
!     <calendar argument 1>, see below for supported values
!     <calendar argument 2>, see below for supported values
!
! !DESCRIPTION:
!     \begin{sloppypar}
!     Overloads the (/=) operator for the {\tt ESMF\_Calendar} class.
!     Compare a {\tt ESMF\_Calendar} object or {\tt ESMF\_CalKind\_Flag} with
!     another calendar object or calendar kind for inequality.  Return
!     {\tt .true.} if not equal, {\tt .false.} otherwise.  Comparison is based
!     on calendar kind, which is a property of a calendar object.
!     \end{sloppypar}
!
!     If both arguments are {\tt ESMF\_Calendar} objects, and both are of  
!     type {\tt ESMF\_CALKIND\_CUSTOM}, then all the calendar's properties,
!     except name, are compared.
!
!     If both arguments are {\tt ESMF\_Calendar} objects, and either of them
!     is not in the {\tt ESMF\_INIT\_CREATED} status, an error will be logged.
!     However, this does not affect the return value, which is {\tt .true.} 
!     when both arguments are {\em not} in the {\em same} status, and
!     {\tt .false.} otherwise.
!
!     If one argument is an {\tt ESMF\_Calendar} object, and the other is an
!     {\tt ESMF\_CalKind\_Flag}, and the calendar object is not in the
!     {\tt ESMF\_INIT\_CREATED} status, an error will be logged and
!     {\tt .true.} will be returned.
!
!     Supported values for <calendar argument 1> are:
!     \begin{description}
!     \item type(ESMF\_Calendar),     intent(in) :: calendar1
!     \item type(ESMF\_CalKind\_Flag), intent(in) :: calkindflag1
!     \end{description}
!     Supported values for <calendar argument 2> are:
!     \begin{description}
!     \item type(ESMF\_Calendar),     intent(in) :: calendar2
!     \item type(ESMF\_CalKind\_Flag), intent(in) :: calkindflag2
!     \end{description}
!
!     The arguments are:
!     \begin{description}   
!     \item[<calendar argument 1>]
!          The {\tt ESMF\_Calendar} object or {\tt ESMF\_CalKind\_Flag} on the
!          left hand side of the non-equality operation.
!     \item[<calendar argument 2>]
!          The {\tt ESMF\_Calendar} object or {\tt ESMF\_CalKind\_Flag} on the
!          right hand side of the non-equality operation.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_CalendarNE              ! internal implementation
      module procedure ESMF_CalendarKindNE          ! internal implementation
      module procedure ESMF_CalendarCalAndKindNE    ! internal implementation
      module procedure ESMF_CalendarKindAndCalNE    ! internal implementation
!
      end interface
!
!
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
      module procedure ESMF_CalendarSetDefaultKind
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

subroutine ESMF_CalKindToInt(lhsInt, rhsCalKind)
  integer,                  intent(out) :: lhsInt
  type(ESMF_CalKind_Flag),  intent(in)  :: rhsCalKind
  lhsInt = rhsCalKind%calkindflag
end subroutine

subroutine ESMF_IntToCalKind(lhsCalKind, rhsInt)
  type(ESMF_CalKind_Flag),  intent(out) :: lhsCalKind
  integer,                  intent(in)  :: rhsInt
  lhsCalKind = ESMF_CalKind_Flag(rhsInt)
end subroutine

!==============================================================================
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CalendarGetInit()"
!BOPI
! !IROUTINE:  ESMF_CalendarGetInit - Get initialization status.

! !INTERFACE:
      function ESMF_CalendarGetInit(d)
!
! !ARGUMENTS:
      type(ESMF_Calendar), intent(in), optional :: d
      ESMF_INIT_TYPE                            :: ESMF_CalendarGetInit
!
! !DESCRIPTION:
!     Get the initialization status of the Deep class {\tt calendar}. 
!
!     The arguments are:
!     \begin{description}
!     \item [{[d]}]
!           {\tt ESMF\_Calendar} from which to retrieve status.
!     \end{description}
!
!EOPI

      if (present(d)) then
        ESMF_CalendarGetInit = ESMF_INIT_GET(d)
      else
        ESMF_CalendarGetInit = ESMF_INIT_CREATED
      endif

      end function ESMF_CalendarGetInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CalendarSetInitCreated()"
!BOPI
! !IROUTINE: ESMF_CalendarSetInitCreated - Set Calendar init code to "CREATED"

! !INTERFACE:
      subroutine ESMF_CalendarSetInitCreated(c, rc)
!
! !ARGUMENTS:
      type(ESMF_Calendar), intent(inout),  optional :: c
      integer,             intent(out),    optional :: rc  
!         
!
! !DESCRIPTION:
!     Set init code in Calendar object to "CREATED".
! 
!     Used by other classes to set the isInit value of a calendar, since it
!     is private to the {\tt ESMF\_Calendar} class.  Within the 
!     {\tt ESMF\_Calendar} class, the macro {\tt ESMF\_INIT\_SET\_CREATED()}
!     is used instead, since the {\tt ESMF\_Calendar} class and its methods
!     are encapsulated within the same module.  This gives the calendar 
!     methods direct access to the private isInit member.  Compare to the 
!     other TimeMgr classes which have their types defined in separate
!     modules from their methods, due to mutual dependencies.  They must
!     call their own {\tt ESMF\_*Init*()} method rather than use the 
!     {\tt ESMF\_INIT\_SET\_*()} macro.
!
!     The arguments are:
!     \begin{description}
!     \item[{[c]}] 
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
      if (present(c)) then
        ESMF_INIT_SET_CREATED(c)
      endif

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
    
      end subroutine ESMF_CalendarSetInitCreated

!------------------------------------------------------------------------------


!==============================================================================
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CalendarCreateBuiltIn()"
!BOP
! !IROUTINE: ESMF_CalendarCreate - Create a new ESMF Calendar of built-in type

! !INTERFACE:
      ! Private name; call using ESMF_CalendarCreate()
      function ESMF_CalendarCreateBuiltIn(calkindflag, keywordEnforcer, &
        name, rc)

! !RETURN VALUE:
      type(ESMF_Calendar) :: ESMF_CalendarCreateBuiltIn

! !ARGUMENTS:
      type(ESMF_CalKind_Flag), intent(in)            :: calkindflag
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      character (len=*),       intent(in),  optional :: name
      integer,                 intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Creates and sets a {\tt calendar} to the given built-in
!     {\tt ESMF\_CalKind\_Flag}. 
!
!     The arguments are:
!     \begin{description}
!     \item[calkindflag]
!          The built-in {\tt ESMF\_CalKind\_Flag}.  Valid values are:
!            \newline
!            {\tt ESMF\_CALKIND\_360DAY}, 
!            \newline
!            {\tt ESMF\_CALKIND\_GREGORIAN},
!            \newline
!            {\tt ESMF\_CALKIND\_JULIAN}, 
!            \newline
!            {\tt ESMF\_CALKIND\_JULIANDAY},
!            \newline
!            {\tt ESMF\_CALKIND\_MODJULIANDAY}, 
!            \newline
!            {\tt ESMF\_CALKIND\_NOCALENDAR},
!            \newline
!            and {\tt ESMF\_CALKIND\_NOLEAP}.
!            \newline
!          See Section ~\ref{subsec:Calendar_options} for a description of each
!          calendar kind.
!     \item[{[name]}]
!          The name for the newly created calendar.  If not specified, a
!          default unique name will be generated: "CalendarNNN" where NNN
!          is a unique sequence number from 001 to 999.
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
    
      ! invoke C to C++ entry point
      call c_ESMC_CalendarCreateBuiltIn(ESMF_CalendarCreateBuiltIn, nameLen, &
                                        name, calkindflag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    
      ! mark output as successfully initialized
      ESMF_INIT_SET_CREATED(ESMF_CalendarCreateBuiltIn)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_CalendarCreateBuiltIn
    
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CalendarCreateCopy()"
!BOP    
! !IROUTINE: ESMF_CalendarCreate - Create a copy of an ESMF Calendar

! !INTERFACE:
      ! Private name; call using ESMF_CalendarCreate()
      function ESMF_CalendarCreateCopy(calendar, keywordEnforcer, rc)

! !RETURN VALUE:
      type(ESMF_Calendar) :: ESMF_CalendarCreateCopy

! !ARGUMENTS:
      type(ESMF_Calendar), intent(in)            :: calendar
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,             intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Creates a complete (deep) copy of a given {\tt ESMF\_Calendar}.
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

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_CalendarGetInit,calendar,rc)

      ! invoke C to C++ entry point to copy calendar
      call c_ESMC_CalendarCreateCopy(ESMF_CalendarCreateCopy, calendar, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! mark output as successfully initialized
      ESMF_INIT_SET_CREATED(ESMF_CalendarCreateCopy)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_CalendarCreateCopy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CalendarCreateCustom()"
!BOP
! !IROUTINE: ESMF_CalendarCreate - Create a new custom ESMF Calendar

! !INTERFACE:
      ! Private name; call using ESMF_CalendarCreate()
      function ESMF_CalendarCreateCustom(keywordEnforcer, &
        daysPerMonth, secondsPerDay, &
        daysPerYear, daysPerYearDn, daysPerYearDd, name, rc)

! !RETURN VALUE:
      type(ESMF_Calendar) :: ESMF_CalendarCreateCustom

! !ARGUMENTS:
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,               intent(in),  optional :: daysPerMonth(:)
      integer(ESMF_KIND_I4), intent(in),  optional :: secondsPerDay
      integer(ESMF_KIND_I4), intent(in),  optional :: daysPerYear
      integer(ESMF_KIND_I4), intent(in),  optional :: daysPerYearDn
      integer(ESMF_KIND_I4), intent(in),  optional :: daysPerYearDd
      character (len=*),     intent(in),  optional :: name
      integer,               intent(out), optional :: rc

!
! !DESCRIPTION:
!     Creates a custom {\tt ESMF\_Calendar} and sets its properties.
!
!     The arguments are:
!     \begin{description}
!     \item[{[daysPerMonth]}]
!          Integer array of days per month, for each month of the year.
!          The number of months per year is variable and taken from the
!          size of the array.  If unspecified, months per year = 0,
!          with the days array undefined.
!     \item[{[secondsPerDay]}]
!          Integer number of seconds per day.  Defaults to 0 if not 
!          specified.
!     \item[{[daysPerYear]}]
!          Integer number of days per year.  Use with daysPerYearDn and
!          daysPerYearDd (see below) to specify a days-per-year calendar
!          for any planetary body.  Default = 0.
!     \item[{[daysPerYearDn]}]
!          \begin{sloppypar}
!          Integer numerator portion of fractional number of days per year
!          (daysPerYearDn/daysPerYearDd).
!          Use with daysPerYear (see above) and daysPerYearDd (see below) to
!          specify a days-per-year calendar for any planetary body.
!          Default = 0.
!          \end{sloppypar}
!     \item[{[daysPerYearDd]}]
!          Integer denominator portion of fractional number of days per year
!          (daysPerYearDn/daysPerYearDd).
!          Use with daysPerYear and daysPerYearDn (see above) to
!          specify a days-per-year calendar for any planetary body.
!          Default = 1.
!     \item[{[name]}]
!          The name for the newly created calendar.  If not specified, a
!          default unique name will be generated: "CalendarNNN" where NNN
!          is a unique sequence number from 001 to 999.
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

      ! invoke C to C++ entry point
      if (present(daysPerMonth)) then
        call c_ESMC_CalendarCreateCustom1(ESMF_CalendarCreateCustom, &
                                         nameLen, name, &
                                         daysPerMonth(1), monthsPerYear, &
                                         secondsPerDay, &
                                         daysPerYear, daysPerYearDn, &
                                         daysPerYearDd, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_CalendarCreateCustom0(ESMF_CalendarCreateCustom, &
                                         nameLen, name, &
                                         monthsPerYear, &
                                         secondsPerDay, &
                                         daysPerYear, daysPerYearDn, &
                                         daysPerYearDd, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      end if
    
      ! mark output as successfully initialized
      ESMF_INIT_SET_CREATED(ESMF_CalendarCreateCustom)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_CalendarCreateCustom
    
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CalendarDestroy()"
!BOP
! !IROUTINE: ESMF_CalendarDestroy - Release resources associated with a Calendar
!
! !INTERFACE:
      subroutine ESMF_CalendarDestroy(calendar, keywordEnforcer, rc)
!
! !ARGUMENTS:
      type(ESMF_Calendar), intent(inout)          :: calendar
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,             intent(out),  optional :: rc
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Releases resources associated with this {\tt ESMF\_Calendar}.
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!       Release resources associated with this {\tt ESMF\_Calendar} and mark the
!       object as invalid.  It is an error to pass this object into any other
!       routines after being destroyed.
!     \item[[rc]]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_CalendarGetInit,calendar,rc)

      ! invoke C to C++ entry point
      call c_ESMC_CalendarDestroy(calendar, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! mark output as successfully deleted
      ESMF_INIT_SET_DELETED(calendar)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_CalendarDestroy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CalendarFinalize()"
!BOPI
! !IROUTINE: ESMF_CalendarFinalize
!
! !INTERFACE:
      subroutine ESMF_CalendarFinalize(keywordEnforcer, rc)
!
! !ARGUMENTS:
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
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

     ! invoke C to C++ entry point
      call c_ESMC_CalendarFinalize(localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_CalendarFinalize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CalendarGet()"
!BOP
! !IROUTINE: ESMF_CalendarGet - Get Calendar properties

! !INTERFACE:
      subroutine ESMF_CalendarGet(calendar, keywordEnforcer, &
        name, calkindflag, daysPerMonth, monthsPerYear, &
        secondsPerDay, secondsPerYear, &
        daysPerYear, daysPerYearDn, daysPerYearDd, rc)

! !ARGUMENTS:
      type(ESMF_Calendar),    intent(in)            :: calendar
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      type(ESMF_CalKind_Flag),intent(out), optional :: calkindflag
      integer,                intent(out), optional :: daysPerMonth(:)
      integer,                intent(out), optional :: monthsPerYear
      integer(ESMF_KIND_I4),  intent(out), optional :: secondsPerDay
      integer(ESMF_KIND_I4),  intent(out), optional :: secondsPerYear
      integer(ESMF_KIND_I4),  intent(out), optional :: daysPerYear
      integer(ESMF_KIND_I4),  intent(out), optional :: daysPerYearDn
      integer(ESMF_KIND_I4),  intent(out), optional :: daysPerYearDd
      character (len=*),      intent(out), optional :: name
      integer,                intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Gets one or more of an {\tt ESMF\_Calendar}'s properties.
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          The object instance to query.
!     \item[{[calkindflag]}]
!          The {\tt CalKind\_Flag} ESMF\_CALKIND\_GREGORIAN, 
!          ESMF\_CALKIND\_JULIAN, etc.
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
!          every year.
!     \item[{[daysPerYearDn]}]
!          \begin{sloppypar}
!          Integer fractional number of days per year (numerator).
!          For calendars with intercalations, daysPerYearDn/daysPerYearDd is
!          the average fractional number of days per year (e.g. 25/100 for
!          Julian 4-year intercalation).  For other calendars, it is zero.
!          \end{sloppypar}
!     \item[{[daysPerYearDd]}]
!          Integer fractional number of days per year (denominator).  See
!          daysPerYearDn above.
!     \item[{[name]}]
!          The name of this calendar.
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

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_CalendarGetInit,calendar,rc)

      nameLen = 0
      tempNameLen = 0
      sizeofDaysPerMonth = 0

      ! get length of given name for C++ validation
      if (present(name)) then
        nameLen = len(name)
      end if

      ! get size of given daysPerMonth list for C++ validation
      if (present(daysPerMonth)) then   
        sizeofDaysPerMonth = size(daysPerMonth)
      end if

      ! invoke C to C++ entry point

      if (present(daysPerMonth)) then
        call c_ESMC_CalendarGet1(calendar, nameLen, tempNameLen, tempName, &
                         calkindflag, &
                         daysPerMonth(1), sizeofDaysPerMonth, &
                         monthsPerYear, secondsPerDay, secondsPerYear, &
                         daysPerYear, daysPerYearDn, daysPerYearDd, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_CalendarGet0(calendar, nameLen, tempNameLen, tempName, &
                         calkindflag, &
                         sizeofDaysPerMonth, &
                         monthsPerYear, secondsPerDay, secondsPerYear, &
                         daysPerYear, daysPerYearDn, daysPerYearDd, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CalendarInitialize()"
!BOPI
! !IROUTINE: ESMF_CalendarInitialize - Initialize the default Calendar kind

! !INTERFACE:
      subroutine ESMF_CalendarInitialize(keywordEnforcer, calkindflag, rc)

! !ARGUMENTS:
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      type(ESMF_CalKind_Flag), intent(in),  optional :: calkindflag
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Initializes the default {\tt calendar} to the given type.  Subsequent
!     Time Manager operations requiring a calendar where one isn't specified
!     will use the default internal calendar of this type.
!
!     The arguments are:
!     \begin{description}
!     \item[{[calkindflag]}]
!          The calendar kind to initialize the default to.  If not specified,
!          the default is set to {\tt ESMF\_CALKIND\_NOCALENDAR}.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!    
!EOPI
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL
    
      ! invoke C to C++ entry point
      call c_ESMC_CalendarInitialize(calkindflag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_CalendarInitialize


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CalendarIsCreated()"
!BOP
! !IROUTINE: ESMF_CalendarIsCreated - Check whether a Calendar object has been created

! !INTERFACE:
  function ESMF_CalendarIsCreated(calendar, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_CalendarIsCreated
!
! !ARGUMENTS:
    type(ESMF_Calendar), intent(in)            :: calendar
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt calendar} has been created. Otherwise return 
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is 
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[calendar]
!     {\tt ESMF\_Calendar} queried.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------    
    ESMF_CalendarIsCreated = .false.   ! initialize
    if (present(rc)) rc = ESMF_SUCCESS
    if (ESMF_CalendarGetInit(calendar)==ESMF_INIT_CREATED) &
      ESMF_CalendarIsCreated = .true.
  end function
!------------------------------------------------------------------------------
    
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CalendarIsLeapYear - Determine if given year is a leap year
!
! !INTERFACE:
!     ! Private name; call using ESMF_CalendarIsLeapYear()
!     function ESMF_CalendarIsLeapYear<kind>(calendar, yy, keywordEnforcer, rc)
!
! !RETURN VALUE:
!     logical :: ESMF_CalendarIsLeapYear<kind>
!
! !ARGUMENTS:
!     type(ESMF_Calendar),       intent(in)            :: calendar
!     integer(ESMF_KIND_<kind>), intent(in)            :: yy
!     type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!     integer,                   intent(out), optional :: rc
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     \begin{sloppypar}
!     Returns {\tt .true.} if the given year is a leap year within the given
!     calendar, and {\tt .false.} otherwise.  Custom calendars do not define
!     leap years, so {\tt .false.} will always be returned in this case;
!     see Section ~\ref{subsec:Calendar_rest}.
!     See also {\tt ESMF\_TimeIsLeapYear()}.
!     \end{sloppypar}
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          {\tt ESMF\_Calendar} to determine leap year within.
!     \item[yy]
!          Year to check for leap year.  The type is integer and the <kind> can
!          be either I4 or I8:  {\tt ESMF\_KIND\_I4} or {\tt ESMF\_KIND\_I8}.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!    
!EOP
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CalendarIsLeapYearI4()"
!BOPI
! !IROUTINE: ESMF_CalendarIsLeapYear - Determine if given year is a leap year

! !INTERFACE:
      ! Private name; call using ESMF_CalendarIsLeapYear()
      function ESMF_CalendarIsLeapYearI4(calendar, yy, keywordEnforcer, rc)

! !RETURN VALUE:
      logical :: ESMF_CalendarIsLeapYearI4

! !ARGUMENTS:
      type(ESMF_Calendar),   intent(in)            :: calendar
      integer(ESMF_KIND_I4), intent(in)            :: yy
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,               intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     \begin{sloppypar}
!     Returns {\tt .true.} if the given year is a leap year within the given
!     calendar, and {\tt .false.} otherwise.  Custom calendars do not define
!     leap years, so {\tt .false.} will always be returned in this case;
!     see Section ~\ref{subsec:Calendar_rest}.
!     See also {\tt ESMF\_TimeIsLeapYear()}.
!     \end{sloppypar}
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
!EOPI
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! Initialize output value in case of error
      ESMF_CalendarIsLeapYearI4 = .false.

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_CalendarGetInit,calendar,rc)

      ! invoke C to C++ entry point
      call c_ESMC_CalendarIsLeapYearI4(calendar, yy, &
                                       ESMF_CalendarIsLeapYearI4, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS    
      end function ESMF_CalendarIsLeapYearI4
    
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CalendarIsLeapYearI8()"
!BOPI
! !IROUTINE: ESMF_CalendarIsLeapYear - Determine if given year is a leap year

! !INTERFACE:
      ! Private name; call using ESMF_CalendarIsLeapYear()
      function ESMF_CalendarIsLeapYearI8(calendar, yy, keywordEnforcer, rc)

! !RETURN VALUE:
      logical :: ESMF_CalendarIsLeapYearI8

! !ARGUMENTS:
      type(ESMF_Calendar),   intent(in)            :: calendar
      integer(ESMF_KIND_I8), intent(in)            :: yy
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,               intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     \begin{sloppypar}
!     Returns {\tt .true.} if the given year is a leap year within the given
!     calendar, and {\tt .false.} otherwise.  Custom calendars do not define
!     leap years, so {\tt .false.} will always be returned in this case;
!     see Section ~\ref{subsec:Calendar_rest}.
!     See also {\tt ESMF\_TimeIsLeapYear()}.
!     \end{sloppypar}
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
!EOPI
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL
 
      ! Initialize output value in case of error
      ESMF_CalendarIsLeapYearI8 = .false.

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_CalendarGetInit,calendar,rc)

      ! invoke C to C++ entry point
      call c_ESMC_CalendarIsLeapYearI8(calendar, yy, &
                                       ESMF_CalendarIsLeapYearI8, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS 
      end function ESMF_CalendarIsLeapYearI8
    
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CalendarPrint()"
!BOP
! !IROUTINE:  ESMF_CalendarPrint - Print Calendar information

! !INTERFACE:
      subroutine ESMF_CalendarPrint(calendar, options, rc)

! !ARGUMENTS:
      type(ESMF_Calendar), intent(in)            :: calendar
      character (len=*),   intent(in),  optional :: options
      integer,             intent(out), optional :: rc

!
! !DESCRIPTION:
!     Prints out an {\tt ESMF\_Calendar}'s properties to {\tt stdio}, 
!     in support of testing and debugging.  The options control the 
!     type of information and level of detail. \\
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          {\tt ESMF\_Calendar} to be printed out.
!     \item[{[options]}]
!          Print options. If none specified, prints all calendar property
!                             values. \\
!          "calkindflag"    - print the calendar's type 
!                               (e.g. ESMF\_CALKIND\_GREGORIAN). \\
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
 
      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_CalendarGetInit,calendar,rc)

      ! invoke C to C++ entry point
      call ESMF_UtilIOUnitFlush (ESMF_UtilIOStdout, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      call c_ESMC_CalendarPrint(calendar, options, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_CalendarPrint
      
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CalendarReadRestart()"
!BOPI
! !IROUTINE:  ESMF_CalendarReadRestart - Restore the contents of a Calendar (not implemented)

! !INTERFACE:
      function ESMF_CalendarReadRestart(name, keywordEnforcer, rc)
! 
! !RETURN VALUE:
      type(ESMF_Calendar) :: ESMF_CalendarReadRestart
!
! !ARGUMENTS:
      character (len=*),   intent(in)            :: name
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,             intent(out), optional :: rc

! !DESCRIPTION:
!     Restores an {\tt ESMF\_Calendar} object from the last call to
!     {\tt ESMF\_CalendarWriteRestart()}.  (Not implemented yet).
!
!     The arguments are:
!     \begin{description}
!     \item[name]
!          The name of the object instance to restore.
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

      ! invoke C to C++ entry point to allocate and restore calendar
      call c_ESMC_CalendarReadRestart(ESMF_CalendarReadRestart, nameLen, name, &
                                      localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! mark output as successfully initialized
      ESMF_INIT_SET_CREATED(ESMF_CalendarReadRestart)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS 
      end function ESMF_CalendarReadRestart

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CalendarSetBuiltIn()"
!BOP
! !IROUTINE: ESMF_CalendarSet - Set a Calendar to a built-in type

! !INTERFACE:
      ! Private name; call using ESMF_CalendarSet()
      subroutine ESMF_CalendarSetBuiltIn(calendar, calkindflag, &
        keywordEnforcer, name, rc)

! !ARGUMENTS:
      type(ESMF_Calendar),     intent(inout)         :: calendar
      type(ESMF_CalKind_Flag), intent(in)            :: calkindflag
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      character (len=*),       intent(in),  optional :: name
      integer,                 intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Sets {\tt calendar} to the given built-in {\tt ESMF\_CalKind\_Flag}. 
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          The object instance to initialize.
!     \item[calkindflag]
!          The built-in {\tt CalKind\_Flag}.  Valid values are:
!            \newline
!            {\tt ESMF\_CALKIND\_360DAY}, 
!            \newline
!            {\tt ESMF\_CALKIND\_GREGORIAN},
!            \newline
!            {\tt ESMF\_CALKIND\_JULIAN}, 
!            \newline
!            {\tt ESMF\_CALKIND\_JULIANDAY},
!            \newline
!            {\tt ESMF\_CALKIND\_MODJULIANDAY}, 
!            \newline
!            {\tt ESMF\_CALKIND\_NOCALENDAR},
!            \newline
!            and {\tt ESMF\_CALKIND\_NOLEAP}.
!            \newline
!          See Section ~\ref{subsec:Calendar_options} for a description of each
!          calendar kind.
!     \item[{[name]}]
!          The new name for this calendar.
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

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_CalendarGetInit,calendar,rc)

      nameLen = 0

      ! get length of given name for C++ validation
      if (present(name)) then
        nameLen = len_trim(name)
      end if
    
      ! invoke C to C++ entry point
      call c_ESMC_CalendarSetBuiltIn(calendar, nameLen, name, calkindflag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_CalendarSetBuiltIn
    
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CalendarSetCustom()"
!BOP
! !IROUTINE: ESMF_CalendarSet - Set properties of a custom Calendar

! !INTERFACE:
      ! Private name; call using ESMF_CalendarSet()
      subroutine ESMF_CalendarSetCustom(calendar, keywordEnforcer, &
        daysPerMonth, secondsPerDay, &
        daysPerYear, daysPerYearDn, daysPerYearDd, name, rc)

! !ARGUMENTS:
      type(ESMF_Calendar),  intent(inout)         :: calendar
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,              intent(in),  optional :: daysPerMonth(:)
      integer(ESMF_KIND_I4),intent(in),  optional :: secondsPerDay
      integer(ESMF_KIND_I4),intent(in),  optional :: daysPerYear
      integer(ESMF_KIND_I4),intent(in),  optional :: daysPerYearDn
      integer(ESMF_KIND_I4),intent(in),  optional :: daysPerYearDd
      character (len=*),    intent(in),  optional :: name
      integer,              intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
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
!          Integer number of seconds per day.  Defaults to 0 if not 
!          specified.
!     \item[{[daysPerYear]}]
!          Integer number of days per year.  Use with daysPerYearDn and
!          daysPerYearDd (see below) to specify a days-per-year calendar
!          for any planetary body.  Default = 0.
!     \item[{[daysPerYearDn]}]
!          Integer numerator portion of fractional number of days per year
!          (daysPerYearDn/daysPerYearDd).
!          Use with daysPerYear (see above) and daysPerYearDd (see below) to
!          specify a days-per-year calendar for any planetary body.
!          Default = 0.
!     \item[{[daysPerYearDd]}]
!          \begin{sloppypar}
!          Integer denominator portion of fractional number of days per year
!          (daysPerYearDn/daysPerYearDd).
!          Use with daysPerYear and daysPerYearDn (see above) to
!          specify a days-per-year calendar for any planetary body.
!          Default = 1.
!          \end{sloppypar}
!     \item[{[name]}]
!          The new name for this calendar.
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

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check input
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

      ! invoke C to C++ entry point

      if (present(daysPerMonth)) then
        call c_ESMC_CalendarSetCustom1(calendar, &
                                      nameLen, name, &
                                      daysPerMonth(1), monthsPerYear, &
                                      secondsPerDay, &
                                      daysPerYear, daysPerYearDn, &
                                      daysPerYearDd, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_CalendarSetCustom0(calendar, &
                                      nameLen, name, &
                                      monthsPerYear, &
                                      secondsPerDay, &
                                      daysPerYear, daysPerYearDn, &
                                      daysPerYearDd, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      end if

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_CalendarSetCustom
    
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CalendarSetDefaultKind()"
!BOP
! !IROUTINE: ESMF_CalendarSetDefault - Set the default Calendar kind

! !INTERFACE:
      ! Private name; call using ESMF_CalendarSetDefault()
      subroutine ESMF_CalendarSetDefaultKind(calkindflag, rc)

! !ARGUMENTS:
      type(ESMF_CalKind_Flag), intent(in)            :: calkindflag
      integer,                 intent(out), optional :: rc

!
! !DESCRIPTION:
!     Sets the default {\tt calendar} to the given type.  Subsequent Time
!     Manager operations requiring a calendar where one isn't specified will
!     use the internal calendar of this type.
!
!     The arguments are:
!     \begin{description}
!     \item[calkindflag]
!          The calendar kind to be the default.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!    
!EOP
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL
    
      ! invoke C to C++ entry point
      call c_ESMC_CalendarSetDefaultKind(calkindflag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_CalendarSetDefaultKind
    
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CalendarSetDefaultCal()"
!BOP
! !IROUTINE: ESMF_CalendarSetDefault - Set the default Calendar

! !INTERFACE:
      ! Private name; call using ESMF_CalendarSetDefault()
      subroutine ESMF_CalendarSetDefaultCal(calendar, rc)

! !ARGUMENTS:
      type(ESMF_Calendar),     intent(in)            :: calendar
      integer,                 intent(out), optional :: rc

!
! !DESCRIPTION:
!     Sets the default {\tt calendar} to the one given.  Subsequent Time
!     Manager operations requiring a calendar where one isn't specified will
!     use this calendar.
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

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_CalendarGetInit,calendar,rc)

      ! invoke C to C++ entry point
      call c_ESMC_CalendarSetDefaultCal(calendar, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS 
      end subroutine ESMF_CalendarSetDefaultCal
    
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CalendarValidate()"
!BOP
! !IROUTINE:  ESMF_CalendarValidate - Validate a Calendar's properties

! !INTERFACE:
      subroutine ESMF_CalendarValidate(calendar, keywordEnforcer, rc)
 
! !ARGUMENTS:
      type(ESMF_Calendar), intent(in)            :: calendar
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,             intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Checks whether a {\tt calendar} is valid.  
!     Must be one of the defined calendar kinds.  daysPerMonth, daysPerYear,
!     secondsPerDay must all be greater than or equal to zero.
! 
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          {\tt ESMF\_Calendar} to be validated.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
      integer :: localrc                        ! local return code
      character :: options ! dummy options

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL
      
      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_CalendarGetInit,calendar,rc)

      ! invoke C to C++ entry point
      call c_ESMC_CalendarValidate(calendar, options, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_CalendarValidate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CalendarWriteRestart()"
!BOPI
! !IROUTINE:  ESMF_CalendarWriteRestart - Save the contents of a Calendar (not implemented)

! !INTERFACE:
      subroutine ESMF_CalendarWriteRestart(calendar, keywordEnforcer, rc)

! !ARGUMENTS:
      type(ESMF_Calendar), intent(in)            :: calendar
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,             intent(out), optional :: rc

! !DESCRIPTION:  
!     Saves an {\tt ESMF\_Calendar} object.  Default options are to select the
!     fastest way to save to disk.  (Not implemented yet).
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          The object instance to save.  
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_CalendarGetInit,calendar,rc)

      ! invoke C to C++ entry point 
      call c_ESMC_CalendarWriteRestart(calendar, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_CalendarWriteRestart

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CalendarEQ()"
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

      ESMF_INIT_TYPE calinit1, calinit2
      integer :: localrc1, localrc2
      logical :: lval1, lval2

      ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain 
      ! init checks on both args, and in the case where both are uninitialized,
      ! to distinguish equality based on uninitialized type (uncreated,
      ! deleted).

      ! TODO: Consider moving this logic to C++: use Base class? status?
      !       Or replicate logic for C interface also.

      ! check inputs
      calinit1 = ESMF_CalendarGetInit(calendar1)
      calinit2 = ESMF_CalendarGetInit(calendar2)

      ! TODO: this line must remain split in two for SunOS f90 8.3 127000-03
      if (calinit1 .eq. ESMF_INIT_CREATED .and. &
          calinit2 .eq. ESMF_INIT_CREATED) then
        ! invoke C to C++ entry point
        call c_ESMC_CalendarEQ(calendar1, calendar2, ESMF_CalendarEQ)
      else
        ! log error, convert to return code, and compare
        lval1 = ESMF_IMErr(calinit1, ESMF_CONTEXT, rc=localrc1)
        lval2 = ESMF_IMErr(calinit2, ESMF_CONTEXT, rc=localrc2)
        ESMF_CalendarEQ = localrc1.eq.localrc2
      endif

      end function ESMF_CalendarEQ

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CalendarKindEQ()"
!BOPI
! !IROUTINE:  ESMF_CalendarKindEQ - Compare two Calendar kinds for equality
!
! !INTERFACE:
      function ESMF_CalendarKindEQ(calkindflag1, calkindflag2)
! 
! !RETURN VALUE:
      logical :: ESMF_CalendarKindEQ

! !ARGUMENTS:
      type(ESMF_CalKind_Flag), intent(in) :: calkindflag1
      type(ESMF_CalKind_Flag), intent(in) :: calkindflag2

! !DESCRIPTION:
!     This method overloads the (==) operator for the {\tt ESMF\_Calendar}
!     class.  See "interface operator(==)" above for complete description.
!             
!EOPI
      ! invoke C to C++ entry point
      call c_ESMC_CalendarKindEQ(calkindflag1, calkindflag2, &
                                 ESMF_CalendarKindEQ)

      end function ESMF_CalendarKindEQ

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CalendarCalAndKindEQ()"
!BOPI
! !IROUTINE:  ESMF_CalendarCalAndKindEQ - Compare a Calendar and Calendar kind for equality
!
! !INTERFACE:
      function ESMF_CalendarCalAndKindEQ(calendar, calkindflag)
! 
! !RETURN VALUE:
      logical :: ESMF_CalendarCalAndKindEQ

! !ARGUMENTS:
      type(ESMF_Calendar),     intent(in) :: calendar
      type(ESMF_CalKind_Flag), intent(in) :: calkindflag

! !DESCRIPTION:
!     This method overloads the (==) operator for the {\tt ESMF\_Calendar}
!     class.  See "interface operator(==)" above for complete description.
!             
!EOPI

      integer :: localrc

      ! Initialize output value in case of error
      ESMF_CalendarCalAndKindEQ = .false.

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_CalendarGetInit,calendar,localrc)

      ! invoke C to C++ entry point
      call c_ESMC_CalendarCalAndKindEQ(calendar, calkindflag, &
                                       ESMF_CalendarCalAndKindEQ)

      end function ESMF_CalendarCalAndKindEQ

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CalendarKindAndCalEQ()"
!BOPI
! !IROUTINE:  ESMF_CalendarKindAndCalEQ - Compare a Calendar kind and Calendar for equality
!
! !INTERFACE:
      function ESMF_CalendarKindAndCalEQ(calkindflag, calendar)
! 
! !RETURN VALUE:
      logical :: ESMF_CalendarKindAndCalEQ

! !ARGUMENTS:
      type(ESMF_CalKind_Flag), intent(in) :: calkindflag
      type(ESMF_Calendar),     intent(in) :: calendar

! !DESCRIPTION:
!     This method overloads the (==) operator for the {\tt ESMF\_Calendar}
!     class.  See "interface operator(==)" above for complete description.
!             
!EOPI

      integer :: localrc

      ! Initialize output value in case of error
      ESMF_CalendarKindAndCalEQ = .false.

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_CalendarGetInit,calendar,localrc)

      ! invoke C to C++ entry point
      call c_ESMC_CalendarKindAndCalEQ(calkindflag, calendar, &
                                       ESMF_CalendarKindAndCalEQ)

      end function ESMF_CalendarKindAndCalEQ

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CalendarNE()"
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

      ESMF_INIT_TYPE calinit1, calinit2
      integer :: localrc1, localrc2
      logical :: lval1, lval2

      ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain 
      ! init checks on both args, and in the case where both are uninitialized,
      ! to distinguish equality based on uninitialized type (uncreated,
      ! deleted).

      ! TODO: Consider moving this logic to C++: use Base class? status?
      !       Or replicate logic for C interface also.

      ! check inputs
      calinit1 = ESMF_CalendarGetInit(calendar1)
      calinit2 = ESMF_CalendarGetInit(calendar2)

      ! TODO: this line must remain split in two for SunOS f90 8.3 127000-03
      if (calinit1 .eq. ESMF_INIT_CREATED .and. &
          calinit2 .eq. ESMF_INIT_CREATED) then
        ! invoke C to C++ entry point
        call c_ESMC_CalendarNE(calendar1, calendar2, ESMF_CalendarNE)
      else
        ! log error, convert to return code, and compare
        lval1 = ESMF_IMErr(calinit1, ESMF_CONTEXT, rc=localrc1)
        lval2 = ESMF_IMErr(calinit2, ESMF_CONTEXT, rc=localrc2)
        ESMF_CalendarNE = localrc1.ne.localrc2
      endif

      end function ESMF_CalendarNE

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CalendarKindNE()"
!BOPI
! !IROUTINE:  ESMF_CalendarKindNE - Compare two Calendar kinds for inequality
!
! !INTERFACE:
      function ESMF_CalendarKindNE(calkindflag1, calkindflag2)
! 
! !RETURN VALUE:
      logical :: ESMF_CalendarKindNE

! !ARGUMENTS:
      type(ESMF_CalKind_Flag), intent(in) :: calkindflag1
      type(ESMF_CalKind_Flag), intent(in) :: calkindflag2

! !DESCRIPTION:
!     This method overloads the (/=) operator for the {\tt ESMF\_Calendar}
!     class.  See "interface operator(/=)" above for complete description.
!             
!EOPI
      ! invoke C to C++ entry point
      call c_ESMC_CalendarKindNE(calkindflag1, calkindflag2, &
                                 ESMF_CalendarKindNE)

      end function ESMF_CalendarKindNE

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CalendarCalAndKindNE()"
!BOPI
! !IROUTINE:  ESMF_CalendarCalAndKindNE - Compare a Calendar and Calendar kind for inequality
!
! !INTERFACE:
      function ESMF_CalendarCalAndKindNE(calendar, calkindflag)
! 
! !RETURN VALUE:
      logical :: ESMF_CalendarCalAndKindNE

! !ARGUMENTS:
      type(ESMF_Calendar),     intent(in) :: calendar
      type(ESMF_CalKind_Flag), intent(in) :: calkindflag

! !DESCRIPTION:
!     This method overloads the (/=) operator for the {\tt ESMF\_Calendar}
!     class.  See "interface operator(/=)" above for complete description.
!             
!EOPI

      integer :: localrc

      ! Initialize output value in case of error
      ESMF_CalendarCalAndKindNE = .true.

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_CalendarGetInit,calendar,localrc)

      ! invoke C to C++ entry point
      call c_ESMC_CalendarCalAndKindNE(calendar, calkindflag, &
                                       ESMF_CalendarCalAndKindNE)

      end function ESMF_CalendarCalAndKindNE

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CalendarKindAndCalNE()"
!BOPI
! !IROUTINE:  ESMF_CalendarKindAndCalNE - Compare a Calendar kind and Calendar for inequality
!
! !INTERFACE:
      function ESMF_CalendarKindAndCalNE(calkindflag, calendar)
! 
! !RETURN VALUE:
      logical :: ESMF_CalendarKindAndCalNE

! !ARGUMENTS:
      type(ESMF_CalKind_Flag), intent(in) :: calkindflag
      type(ESMF_Calendar),     intent(in) :: calendar

! !DESCRIPTION:
!     This method overloads the (/=) operator for the {\tt ESMF\_Calendar}
!     class.  See "interface operator(/=)" above for complete description.
!             
!EOPI

      integer :: localrc

      ! Initialize output value in case of error
      ESMF_CalendarKindAndCalNE = .true.

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_CalendarGetInit,calendar,localrc)

      ! invoke C to C++ entry point
      call c_ESMC_CalendarKindAndCalNE(calkindflag, calendar, &
                                       ESMF_CalendarKindAndCalNE)

      end function ESMF_CalendarKindAndCalNE

!------------------------------------------------------------------------------

      end module ESMF_CalendarMod
