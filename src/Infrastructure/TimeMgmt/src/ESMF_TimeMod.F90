! $Id: ESMF_TimeMod.F90,v 1.1 2002/12/11 16:22:44 nscollins Exp $
      module ESMF_TimeMod
!===============================================================================
!BOP
! !MODULE: ESMF_TimeMod
! !USES:
!jw	use ESMF_BasicUtilMod
! nsc - the return codes are in the following mod now
        use ESMF_TimeMgmtMod
	use ESMF_TODMod
!
! !PUBLIC TYPES:
      implicit none

      type ESMF_Time
        private
        sequence
        integer(8) day
        type(ESMF_TOD) tod
      end type ESMF_Time
!
! !PUBLIC MEMBER FUNCTIONS:
!     ESMF_TimeInit (Interface only)
!     ESMF_TimeSet (Interface only)
!     ESMF_TimeGet (Interface only)
!     ESMF_TimeIncrement (Interface only)
!     ESMF_TimeDecrement (Interface only)
!     ESMF_TimeGetDays
!     ESMF_TimeDiff
!     ESMF_TimePrint
!
! !DESCRIPTION:
! Time objects are part of the Modeling Framework Time Management module 
! ({\tt ESMF\_TimeMgmt}).  A time object represents a time interval as days and a time 
! of day.
!EOP
!===============================================================================
	
!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeInit
!
! !INTERFACE:
      interface ESMF_TimeInit
        module procedure ESMF_TimeInitIS, &
                   ESMF_TimeInitUndefined, &
                   ESMF_TimeCopyInit
      end interface

! !DESCRIPTION:
!     Initializes a {\tt ESMF\_Time} object. \\
!     See the explicit interfaces:
!
!     \begin{tabular}{p{2in}p{3.5in}}
!     {\tt ESMF\_TimeInitIS}        & initializes a time with days and seconds \\
!     {\tt ESMF\_TimeInitUndefined} & initializes a time with undefined contents \\
!     {\tt ESMF\_TimeCopyInit}      & initializes a time based on another time \\
!     \end{tabular}
!  
! SEE ALSO:
!   {\tt ESMF\_TimeInitIS, ESMF\_TimeInitUndefined, ESMF\_TimeCopyInit}
!
!EOP
!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeSet
!
! !INTERFACE:
      interface ESMF_TimeSet
        module procedure ESMF_TimeSetIS
      end interface

! !DESCRIPTION:
!     Sets the value of a {\tt ESMF\_Time} object. \\
!     See the explicit interfaces:
!
!     \begin{tabular}{p{2in}p{3.5in}}
!     {\tt ESMF\_TimeSetIS} & sets a time with days and seconds \\
!     \end{tabular}
!  
! SEE ALSO:
!   {\tt ESMF\_TimeSetIS}
!
!EOP
!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeGet
!
! !INTERFACE:
      interface ESMF_TimeGet
        module procedure ESMF_TimeGetIS
      end interface

! !DESCRIPTION:
!     Gets the value of a {\tt ESMF\_Time} object. \\
!     See the explicit interfaces:
!
!     \begin{tabular}{p{2in}p{3.5in}}
!     {\tt ESMF\_TimeGetIS} & gets a time of the form days and seconds \\
!     \end{tabular}
!  
! SEE ALSO:
!   {\tt ESMF\_TimeGetIS}
!
!EOP
!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeIncrement
!
! !INTERFACE:
      interface ESMF_TimeIncrement
        module procedure ESMF_TimeIncrementIS
      end interface

! !DESCRIPTION:
!     Increments the value of a {\tt ESMF\_Time} object. \\
!     See the explicit interfaces:
!
!     \begin{tabular}{p{2in}p{3.5in}}
!     {\tt ESMF\_TimeIncrementIS} & increments a time of the form days and seconds \\
!     \end{tabular} 
!  
! SEE ALSO:
!   {\tt ESMF\_TimeIncrementIS}
!
!EOP
!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeDecrement
!
! !INTERFACE:
      interface ESMF_TimeDecrement
        module procedure ESMF_TimeDecrementIS
      end interface

! !DESCRIPTION:
!     Decrements the value of a {\tt ESMF\_Time} object. \\
!     See the explicit interfaces:
!
!     \begin{tabular}{p{2in}p{3.5in}}
!     {\tt ESMF\_TimeDecrementIS} & decrements a time of the form days and seconds \\
!     \end{tabular}
!  
! SEE ALSO:
!   {\tt ESMF\_TimeDecrementIS}
!
!EOP
!===============================================================================
	contains
!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeInitIS
!
! !INTERFACE:
      function ESMF_TimeInitIS(days, seconds, rc)

! !PARAMETERS:    
      type(ESMF_Time) :: ESMF_TimeInitIS      ! returned time object
    
      integer, intent(in) :: days             ! days in time
      integer, intent(in) :: seconds          ! seconds in time
      integer, intent(out), optional :: rc    ! return code

! !DESCRIPTION:
!     Initializes a time object that is based on integer seconds.  
!     Acceptable values for days and seconds are non-negative values 
!     and the value {\tt ESMF\_TIME\_UNDEFINED}.  
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_TimeInitIS(ESMF_TimeInitIS, days, seconds, stub)
      if (present(rc)) rc = stub

      end function ESMF_TimeInitIS

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeInitUndefined
!
! !INTERFACE:
      function ESMF_TimeInitUndefined(rc)

! !RETURN VALUE:    
      type(ESMF_Time) :: ESMF_TimeInitUndefined     ! returned time object
    
! !PARAMETERS:
      integer, intent(out), optional :: rc          ! return code

! !DESCRIPTION:
!     Initializes a new time object with undefined contents.  The value of 
!     internal attributes is set to {\tt ESMF\_TIME\_UNDEFINED}.  
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_TimeInitUndefined(ESMF_TimeInitUndefined, stub)
      if (present(rc)) rc = stub
     
      end function ESMF_TimeInitUndefined

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeCopyInit
!
! !INTERFACE:
      function ESMF_TimeCopyInit(orig, rc)

! !RETURN VALUE:    
      type(ESMF_Time) :: ESMF_TimeCopyInit      ! returned time object

! !PARAMETERS:
      type(ESMF_Time), intent(in) :: orig       ! original time    
      integer, intent(out), optional :: rc      ! return code

! !DESCRIPTION:
!     Initializes a new time object to the contents of another time.  
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_TimeCopyInit(ESMF_TimeCopyInit, orig, stub)
      if (present(rc)) rc = stub
  
      end function ESMF_TimeCopyInit

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeSetIS
!
! !INTERFACE:
      subroutine ESMF_TimeSetIS(time, days, seconds, rc)

! !PARAMETERS:
      type(ESMF_Time), intent(out) :: time     ! time
      integer, intent(in) :: days              ! days
      integer, intent(in) :: seconds           ! seconds
      integer, intent(out), optional :: rc     ! return code

! !DESCRIPTION:
!     Sets (or resets) the attributes of {\tt time} to {\tt days} and 
!     {\tt seconds}.  Non-negative values of {\tt days} and {\tt seconds} 
!     are valid; the value {\tt ESMF\_TIME\_UNDEFINED} is not.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_TimeSetIS(time, days, seconds, stub)
      if (present(rc)) rc = stub
    
      end subroutine ESMF_TimeSetIS

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeGetIS
!
! !INTERFACE:
      subroutine ESMF_TimeGetIS(time, days, seconds, rc)

! !PARAMETERS:
      type(ESMF_Time), intent(in) :: time   ! time
      integer, intent(out) :: days          ! returned days
      integer, intent(out) :: seconds       ! returned seconds
      integer, intent(out), optional :: rc  ! return code

! !DESCRIPTION:
!     Returns {\tt time} in the form of integer {\tt days} and {\tt seconds}.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_TimeGetIS(time, days, seconds, stub)
      if (present(rc)) rc = stub
     
      end subroutine ESMF_TimeGetIS

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeGetDays
!
! !INTERFACE:
      function ESMF_TimeGetDays(time, rc)

! !RETURN VALUE:
      real(8) :: ESMF_TimeGetDays               ! returned time value

! !PARAMETERS:
      type(ESMF_Time), intent(in) :: time       ! time
      integer, intent(out), optional :: rc      ! return code

! !DESCRIPTION:
!     Returns {\tt time} in the form of real {\tt days}.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_TimeGetDays(time, ESMF_TimeGetDays, stub)
      if (present(rc)) rc = stub
   
      end function ESMF_TimeGetDays

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeIncrementIS
!
! !INTERFACE:
      function ESMF_TimeIncrementIS(time, days, seconds, rc)

! !RETURN VALUE:
      type(ESMF_Time) :: ESMF_TimeIncrementIS    ! returned incremented time

! !PARAMETERS:
      type(ESMF_Time), intent(in) :: time        ! time
      integer, intent(in) :: days                ! day increment
      integer, intent(in) :: seconds             ! second increment
      integer, intent(out), optional :: rc       ! return code

! !DESCRIPTION:
!     Increments {\tt time} by {\tt days} and {\tt seconds}.  Non-negative
!     values of {\tt days} and {\tt seconds} are valid.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_TimeInitUndefined(ESMF_TimeIncrementIS, stub)
      if (stub == ESMF_SUCCESS) then
        call ESMC_TimeIncrementIS(time, ESMF_TimeIncrementIS, &
                         days, seconds, stub)
      end if
      if (present(rc)) rc = stub
     
      end function ESMF_TimeIncrementIS

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeCopy
!
! !INTERFACE:
      subroutine ESMF_TimeCopy(time, orig, rc)

! !PARAMETERS:
      type(ESMF_Time), intent(in) :: orig        ! original time
      type(ESMF_Time), intent(out) :: time       ! copy
      integer, intent(out), optional :: rc       ! return code

! !DESCRIPTION:
!     Copies the time {\tt orig} into {\tt time}.  Both times must be
!     initialized.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_TimeCopy(time, orig, stub)
      if (present(rc)) rc = stub

      end subroutine ESMF_TimeCopy

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeDiff
!
! !INTERFACE:
      subroutine ESMF_TimeDiff(earlyTime, lateTime, diff, isLater, rc)

! !PARAMETERS:
      type(ESMF_Time), intent(in) :: earlyTime   ! earlier time
      type(ESMF_Time), intent(in) :: lateTime    ! later time
      type(ESMF_Time), intent(out) :: diff       ! difference between earlier
                                                 !   and later times
      logical, intent(out) :: isLater            ! true if later date is in
                                                 !   fact later
      integer, intent(out), optional :: rc       ! return code

! !DESCRIPTION:
!     Takes the difference between two times and returns the difference 
!     in {\tt diff}.  The returned value {\tt isLater} is true if 
!     {\tt lateTime} represents a time quantity greater than or equal to 
!     {\tt earlyTime}.
!     
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_TimeDiff(earlyTime, lateTime, diff, isLater, stub)
      if (present(rc)) rc = stub

      end subroutine ESMF_TimeDiff

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeDecrementIS
!
! !INTERFACE:
      function ESMF_TimeDecrementIS(time, days, seconds, rc)

! !RETURN VALUE:
      type(ESMF_Time) :: ESMF_TimeDecrementIS   ! returned decremented time

! !PARAMETERS:
      type(ESMF_Time), intent(in) :: time       ! time
      integer, intent(in) :: days               ! days
      integer, intent(in) :: seconds            ! seconds
      integer, intent(out), optional :: rc      ! return code 

! !DESCRIPTION:
!     Decrements {\tt time} by {\tt days} and {\tt seconds}.  Non-negative
!     values of {\tt days} and {\tt seconds} are valid.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_TimeInitUndefined(ESMF_TimeDecrementIS, stub)      
      if (stub == ESMF_SUCCESS) then
        call ESMC_TimeDecrementIS(time, ESMF_TimeDecrementIS, days, &
                        seconds, stub)
      end if
      if (present(rc)) rc = stub
     
      end function ESMF_TimeDecrementIS

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimePrint
!
! !INTERFACE:
      subroutine ESMF_TimePrint(time, rc)

! !PARAMETERS:
      type(ESMF_Time), intent(in) :: time           ! time
      integer, intent(out), optional :: rc          ! return code

! !DESCRIPTION:
!     Prints the attributes of {\tt time} to stdout.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_TimePrint(time, stub)
      if (present(rc)) rc = stub
     
      end subroutine ESMF_TimePrint

!===============================================================================

	end module
