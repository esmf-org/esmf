! $Id: ESMF_DateMod.f,v 1.1 2002/11/15 21:35:09 jwolfe Exp $
	module ESMF_DateMod
!===============================================================================
!BOP
! !MODULE: ESMF_DateMod
!
! !USES:
	use ESMF_TODMod
	use ESMF_CalendarMod
	use ESMF_TimeMod
!
! !PUBLIC TYPES:
      type ESMF_Date
        private
        sequence
        type(ESMF_Calendar) calendar
        integer(8) year
        integer(8) month
        integer(8) day
        type(ESMF_TOD) tod
        integer(8) julianDay
        integer(8) dayOfYear
      end type ESMF_Date
!
! !PUBLIC MEMBER FUNCTIONS:
!     ESMF_DateInit (Interface only)
!     ESMF_DateSet (Interface only)
!     ESMF_DateGet (Interface only)
!     ESMF_DateGetDayOfYear
!     ESMF_DateGetFltDayOfYear
!     ESMF_DateGetCalendarType
!     ESMF_DateCopy 
!     ESMF_DateDiff
!     ESMF_DateIsLater
!     ESMF_DateIncrement
!     ESMF_DateIncrementSec
!     ESMF_DateIncrementDay
!     ESMF_DateIncrementMonth
!     ESMF_DateIncrementYear
!     ESMF_DateDecrement
!     ESMF_DatePrint
!
! !PUBLIC DATA MEMBERS:
!
! !DESCRIPTION:
! Dates are part of the Modeling Framework Time Managment module (ESMF\_TimeMgmt).  
! A date object stores the attributes of a calendar date, including year, 
! month, day, and time of day.  Gregorian (ESMF\_GREGORIAN) and no leap year 
! (ESMF\_NO\_LEAP) calendars are supported.
!
!EOP
!===============================================================================


!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_DateInit
!
! !INTERFACE:
      interface ESMF_DateInit
        module procedure ESMF_DateInitIS,
     &                   ESMF_DateInitUndefined,
     &                   ESMF_DateCopyInit
      end interface

! !DESCRIPTION:
!     Initializes a {\tt ESMF\_Date} object. \\
!     See the explicit interfaces:
!
!     \begin{tabular}{p{2in}p{3.5in}}
!     {\tt ESMF\_DateInitIS}          & initializes a date based on integer seconds \\ 
!     {\tt ESMF\_DateInitUndefined}   & initializes a date with undefined contents \\
!     {\tt ESMF\_DateCopyInit}        & initializes a date based on another date \\
!     \end{tabular}  
!
! SEE ALSO:
!   {\tt ESMF\_DateInitIS, ESMF\_DateInitUndefined, ESMF\_DateCopyInit}
!
!EOP
!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_DateSet
!
! !INTERFACE:
      interface ESMF_DateSet
        module procedure ESMF_DateSetIS
      end interface

! !DESCRIPTION:
!     Sets the attributes of a {\tt ESMF\_Date} object. \\
!     See the explicit interfaces:
!
!     \begin{tabular}{p{2in}p{3.5in}}
!     {\tt ESMF\_DateSetIS}  & sets a date based on integer seconds \\ 
!     \end{tabular}  
!
! SEE ALSO:
!   {\tt ESMF\_DateSetIS}
!
!EOP
!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_DateGet
!
! !INTERFACE:
      interface ESMF_DateGet
        module procedure ESMF_DateGetIS
      end interface

! !DESCRIPTION:
!     Gets the attributes of a {\tt ESMF\_Date} object. \\
!     See the explicit interfaces:
!
!     \begin{tabular}{p{2in}p{3.5in}}
!     {\tt ESMF\_DateGetIS} & gets the value of a date based on integer seconds \\
!     \end{tabular}  
!
! SEE ALSO:
!   {\tt ESMF\_DateGetIS}
!
!EOP
!===============================================================================
	contains
!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_DateInitIS
!
! !INTERFACE:
      function ESMF_DateInitIS(type, yearmmdd, tod, rc)
    
! !RETURN VALUE:
      type(ESMF_Date) :: ESMF_DateInitIS          ! returned date object
    
! !PARAMETERS:
      integer, intent(in) :: type                 ! ESMF_GREGORIAN or ESMF_NO_LEAP
      integer, intent(in) :: yearmmdd             ! calendar date
      integer, intent(in) :: tod                  ! time of day in seconds
      integer, intent(out), optional :: rc        ! return code 

! !DESCRIPTION:
!     Initializes a {\tt ESMF\_Date} object.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_DateInitIS(ESMF_DateInitIS, type, yearmmdd, tod, stub)
      if (present(rc)) rc = stub
    
      end function ESMF_DateInitIS

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_DateInitUndefined
!
! !INTERFACE:
      function ESMF_DateInitUndefined(rc)
    
! !RETURN VALUE:
      type(ESMF_Date) :: ESMF_DateInitUndefined     ! returned date object
    
! !PARAMETERS:
      integer, intent(out), optional :: rc          ! return code

! !DESCRIPTION:
!     Initializes a {\tt ESMF\_Date} object wih undefined contents.  The
!     values of internal attributes are set to {\tt ESMF\_TIME\_UNDEFINED}.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_DateInitUndefined(ESMF_DateInitUndefined, stub)
      if (present(rc)) rc = stub
 
      end function ESMF_DateInitUndefined

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_DateCopyInit
!
! !INTERFACE:
      function ESMF_DateCopyInit(orig, rc)
    
! !RETURN VALUE:
      type(ESMF_Date) :: ESMF_DateCopyInit          ! returned date object
    
! !PARAMETERS:
      type(ESMF_Date), intent(in) :: orig           ! original date
      integer, intent(out), optional :: rc          ! return code

! !DESCRIPTION:
!     Initializes a new date object to the contents of another date.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_DateCopyInit(ESMF_DateCopyInit, orig, stub) 
      if (present(rc)) rc = stub

      end function ESMF_DateCopyInit

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_DateSetIS
!
! !INTERFACE:
      subroutine ESMF_DateSetIS(date, type, yearmmdd, tod, rc)

! !PARAMETERS:
      type(ESMF_Date), intent(out) :: date          ! date object
      integer, intent(in) :: type                   ! ESMF_GREGORIAN or ESMF_NO_LEAP
      integer, intent(in) :: yearmmdd               ! calendar date
      integer, intent(in) :: tod                    ! time of day
      integer, intent(out), optional :: rc          ! return code

! !DESCRIPTION:
!     Sets the attributes of a date object.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_DateSetIS(date, type, yearmmdd, tod, stub)
      if (present(rc)) rc = stub

      end subroutine ESMF_DateSetIS

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_DateGetIS
!
! !INTERFACE:
      subroutine ESMF_DateGetIS(date, yearmmdd, tod, rc)

! !PARAMETERS:
      type(ESMF_Date), intent(in) :: date           ! date object
      integer, intent(out) :: yearmmdd              ! calendar date
      integer, intent(out) :: tod                   ! time of day
      integer, intent(out), optional :: rc          ! return code

! !DESCRIPTION:
!     Returns the attributes of a {\tt date}.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_DateGetIS(date, yearmmdd, tod, stub)
      if (present(rc)) rc = stub

      end subroutine ESMF_DateGetIS

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_DateGetDayOfYear
!
! !INTERFACE:
      function ESMF_DateGetDayOfYear(date, rc)

! !RETURN VALUE:
      integer :: ESMF_DateGetDayOfYear              ! returned day of year

! !PARAMETERS:
      type(ESMF_Date), intent(in) :: date           ! date object      
      integer, intent(out), optional :: rc          ! return code

! !DESCRIPTION:
!     Returns the day of the year corresponding to {\tt date}.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_DateGetDayOfYear(date, ESMF_DateGetDayOfYear, stub)
      if (present(rc)) rc = stub

      end function ESMF_DateGetDayOfYear

!===============================================================================
!BOP
! 
! !IROUTINE:  ESMF_DateGetFltDayOfYear
!
! !INTERFACE:
      function ESMF_DateGetFltDayOfYear(date, rc)
! !RETURN VALUE:
      real(8) :: ESMF_DateGetFltDayOfYear ! Returns calendar day as intdays.seconds
! !PARAMETERS:
      type(ESMF_Date), intent(in) :: date           ! date
      integer, intent(out), optional :: rc          ! return code

! !DESCRIPTION:
!     Returns the calendar day as a float.  The day of year is returned as
!     the integer part and the seconds/milliseconds are returned in the 
!     fractional part.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_DateGetFltDayOfYear(date,
     &	   ESMF_DateGetFltDayOfYear, stub)
      if (present(rc)) rc = stub

      end function ESMF_DateGetFltDayOfYear

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_DateGetCalendarType
!
! !INTERFACE:
      function ESMF_DateGetCalendarType(date, rc)

! !RETURN VALUE:
      integer :: ESMF_DateGetCalendarType           ! returned calendar type

! !PARAMETERS:
      type(ESMF_Date), intent(in) :: date           ! date object
      integer, intent(out), optional :: rc          ! return code

! !DESCRIPTION:
!     Returns the calendar type of {\tt date}.  Valid values are:
!     ESMF\_CALENDAR\_TYPE\_UNDEFINED = 0, ESMF\_NO\_LEAP = 1, 
!     ESMF\_GREGORIAN = 2, ESMF\_360\_DAY = 3
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_DateGetCalendarType(date,
     &	   ESMF_DateGetCalendarType, stub)
      if (present(rc)) rc = stub

      end function ESMF_DateGetCalendarType

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_DateCopy
!
! !INTERFACE:
      subroutine ESMF_DateCopy(date, orig, rc)

! !PARAMETERS:
      type(ESMF_Date), intent(out) :: date          ! copy of original date
      type(ESMF_Date), intent(in) :: orig           ! original date
      integer, intent(out), optional :: rc          ! return code

! !DESCRIPTION:
!     Copies the date {\tt orig} to {\tt date}.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_DateCopy(date, orig, stub)
      if (present(rc)) rc = stub

      end subroutine ESMF_DateCopy

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_DateDiff
!
! !INTERFACE:
      subroutine ESMF_DateDiff(earlyDate, lateDate, diff, isLater, rc)

! !PARAMETERS:
      type(ESMF_Date), intent(in) :: earlyDate     ! earlier date
      type(ESMF_Date), intent(in) :: lateDate      ! later date
      type(ESMF_Time), intent(out) :: diff         ! time difference between dates
      logical, intent(out) :: isLater              ! true if late date is 
                                                   !   actually later
      integer, intent(out), optional :: rc         ! return code

! !DESCRIPTION:
!     Determines the time difference between two dates and returns a 
!     logical indicator as to which date is later.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub;

      call ESMC_DateDiff(earlyDate, lateDate, diff, isLater, stub)
      if (present(rc)) rc = stub

      end subroutine ESMF_DateDiff

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_DateIsLater
!
! !INTERFACE:
       subroutine ESMF_DateIsLater(earlyDate, lateDate, isLater, rc)
    
! !PARAMETERS:                              
       type(ESMF_Date), intent(in) :: earlyDate     ! earlier date
       type(ESMF_Date), intent(in) :: lateDate      ! later date
       logical, intent(out) :: isLater              ! true if late date is
                                                    !   actually later
      integer, intent(out), optional :: rc          ! return code

! !DESCRIPTION:
!      Returns true if {\tt lateDate} is later than {\tt earlyDate}.
!
!EOP
!-------------------------------------------------------------------------------

       integer stub

       call ESMC_DateIsLater(earlyDate, lateDate, isLater, stub)
       if (present(rc)) rc = stub

       end subroutine ESMF_DateIsLater

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_DateIncrement
!
! !INTERFACE:  
      function ESMF_DateIncrement(date, time, rc)

! !RETURN VALUE:
      type(ESMF_Date) :: ESMF_DateIncrement         ! returns incremented date

! !PARAMETERS:
      type(ESMF_Date), intent(in) :: date           ! original date     
      type(ESMF_Time), intent(in) :: time           ! time increment
      integer, intent(out), optional :: rc          ! return code

! !DESCRIPTION:
!     Increments {\tt date} by {\tt time} and returns the result.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_DateInitUndefined(ESMF_DateIncrement, stub)
      if (stub == ESMF_SUCCESS) then
        call ESMC_DateIncrement(date, ESMF_DateIncrement, time, stub)
      end if
      if (present(rc)) rc = stub

      end function ESMF_DateIncrement

!===============================================================================
!BOP
! 
! !IROUTINE:  ESMF_DateIncrementSec
!
! !INTERFACE:
      function ESMF_DateIncrementSec(date, nsecs, rc)

! !RETURN VALUE:
      type(ESMF_Date) :: ESMF_DateIncrementSec      ! returns incremented date

! !PARAMETERS:
      type(ESMF_Date), intent(in) :: date           ! original date
      integer, intent(in) :: nsecs                  ! time increment in seconds
      integer, intent(out), optional :: rc          ! return code

! !DESCRIPTION:
!     Increments {\tt date} by {\tt nsecs} seconds and returns
!     the result.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_DateInitUndefined(ESMF_DateIncrementSec, stub)
      if (stub == ESMF_SUCCESS) then
        call ESMC_DateIncrementSec(date, ESMF_DateIncrementSec, nsecs, 
     &    stub)
      end if
      if (present(rc)) rc = stub

      end function ESMF_DateIncrementSec

!===============================================================================
!BOP
! 
! !IROUTINE:  ESMF_DateIncrementDay
!
! !INTERFACE:
      function ESMF_DateIncrementDay(date, ndays, rc)

! !RETURN VALUE:
      type(ESMF_Date) :: ESMF_DateIncrementDay   ! returns incremented date

! !PARAMETERS:
      type(ESMF_Date), intent(in) :: date        ! original date
      integer, intent(in) :: ndays               ! time increment in days
      integer, intent(out), optional :: rc       ! return code

! !DESCRIPTION:
!     Increments {\tt date} by {\tt ndays} days and returns
!     the result.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_DateInitUndefined(ESMF_DateIncrementDay, stub)
      if (stub == ESMF_SUCCESS) then
        call ESMC_DateIncrementDay(date, ESMF_DateIncrementDay, ndays, 
     &    stub)
      end if
      if (present(rc)) rc = stub

      end function ESMF_DateIncrementDay

!===============================================================================
!BOP
! 
! !IROUTINE:  ESMF_DateIncrementMonth
!
! !INTERFACE:
      function ESMF_DateIncrementMonth(date, nmonths, rc)

! !RETURN VALUE:
      type(ESMF_Date) :: ESMF_DateIncrementMonth ! returns incremented date

! !PARAMETERS:
      type(ESMF_Date), intent(in) :: date        ! original date
      integer, intent(in) :: nmonths             ! time increment in months
      integer, intent(out), optional :: rc       ! return code

! !DESCRIPTION:
!     Increments {\tt date} by {\tt nmonths} months and returns
!     the result.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_DateInitUndefined(ESMF_DateIncrementMonth, stub)
      if (stub == ESMF_SUCCESS) then
        call ESMC_DateIncrementMonth(date, ESMF_DateIncrementMonth, 
     &    nmonths, stub)
      end if
      if (present(rc)) rc = stub

      end function ESMF_DateIncrementMonth

!===============================================================================
!BOP
! 
! !IROUTINE:  ESMF_DateIncrementYear
!
! !INTERFACE:
      function ESMF_DateIncrementYear(date, nyears, rc)

! !RETURN VALUE:
      type(ESMF_Date) :: ESMF_DateIncrementYear  ! returns incremented date

! !PARAMETERS:
      type(ESMF_Date), intent(in) :: date        ! original date
      integer, intent(in) :: nyears              ! time increment in years
      integer, intent(out), optional :: rc       ! return code

! !DESCRIPTION:
!     Increments {\tt date} by {\tt nyears} years and returns
!     the result.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_DateInitUndefined(ESMF_DateIncrementYear, stub)
      if (stub == ESMF_SUCCESS) then
        call ESMC_DateIncrementYear(date,
     &	ESMF_DateIncrementYear, nyears, stub)
      end if
      if (present(rc)) rc = stub

      end function ESMF_DateIncrementYear

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_DateDecrement
!
! !INTERFACE:  
      function ESMF_DateDecrement(date, time, rc)

! !RETURN VALUE:
      type(ESMF_Date) :: ESMF_DateDecrement       ! returns decremented date

! !PARAMETERS:
      type(ESMF_Date), intent(in) :: date         ! original date     
      type(ESMF_Time), intent(in) :: time         ! time increment
      integer, intent(out), optional :: rc        ! return code

! !DESCRIPTION:
!     Decrements {\tt date} by {\tt time} and returns the result.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_DateInitUndefined(ESMF_DateDecrement, stub)
      if (stub == ESMF_SUCCESS) then
        call ESMC_DateDecrement(date, ESMF_DateDecrement, time, stub)
      end if
      if (present(rc)) rc = stub

      end function ESMF_DateDecrement

!===============================================================================
!BOP
! 
! !IROUTINE:  ESMF_DatePrint
!
! !INTERFACE:
      subroutine ESMF_DatePrint(date, rc)

! !PARAMETERS:
      type(ESMF_Date), intent(in) :: date           ! date
      integer, intent(out), optional :: rc          ! return code

! !DESCRIPTION:
!     Prints the contents of {\tt date}.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_DatePrint(date, stub)
      if (present(rc)) rc = stub

      end subroutine ESMF_DatePrint


	end module









