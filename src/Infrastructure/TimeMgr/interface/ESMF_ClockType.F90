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
#define ESMF_FILENAME "ESMF_ClockType.F90"
!==============================================================================
!
!     ESMF ClockType Module
      module ESMF_ClockTypeMod
!
!==============================================================================
!
! This file contains the Clock class definition.  The Clock class methods
! are defined in ESMF_Clock.F90.  This split is to resolve mutual method
! dependency with ESMF_Alarm.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF_TimeMgr.inc"

!===============================================================================
!BOPI
!
! !MODULE: ESMF_ClockTypeMod
!
! !DESCRIPTION:
! Part of Time Manager Fortran API wrapper of C++ implementation.
!
! Defines Fortran types for corresponding C++ class {\tt ESMC\_Clock}.
!
! See {\tt ../include/ESMC\_Clock.h} for complete description.
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from ESMF base class
      use ESMF_BaseMod
      use ESMF_UtilTypesMod

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMF_Clock
!
      type ESMF_Clock
#ifndef ESMF_NO_SEQUENCE
      sequence
#endif
      private
        type(ESMF_Pointer) :: this
        ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
!     The types defined in this file are public and propagated up via 
!     ESMF_ClockMod in ESMF_Clock.F90      

      public ESMF_Clock

!------------------------------------------------------------------------------
! !PUBLIC METHODS:
!     The methods defined in this file are public and propagated up via 
!     ESMF_ClockMod in ESMF_Clock.F90      

      public ESMF_ClockGetInit
      public ESMF_ClockSetInitCreated
      public ESMF_ClockSetInitDeleted
      public ESMF_ClockGetThis
      public ESMF_ClockSetThis
      
      public ESMF_ClockEQAlias

!------------------------------------------------------------------------------
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id$'
!------------------------------------------------------------------------------

      contains

!==============================================================================
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockGetInit"
!BOPI
! !IROUTINE:  ESMF_ClockGetInit - Get initialization status.

! !INTERFACE:
      function ESMF_ClockGetInit(d)
!
! !ARGUMENTS:
      type(ESMF_Clock), intent(in), optional :: d
      ESMF_INIT_TYPE                         :: ESMF_ClockGetInit
!
! !DESCRIPTION:
!     Get the initialization status of the Deep class {\tt clock}.
!
!     The arguments are:
!     \begin{description}
!     \item [{[d]}]
!           {\tt ESMF\_Clock} from which to retrieve status.
!     \end{description}
!
!EOPI

      if (present(d)) then
        ESMF_ClockGetInit = ESMF_INIT_GET(d)
      else
        ESMF_ClockGetInit = ESMF_INIT_CREATED
      endif

      end function ESMF_ClockGetInit

!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockSetInitCreated()"
!BOPI
! !IROUTINE: ESMF_ClockSetInitCreated - Set Clock init code to "CREATED"

! !INTERFACE:
      subroutine ESMF_ClockSetInitCreated(clock, rc)
!
! !ARGUMENTS:
      type(ESMF_Clock), intent(inout), optional :: clock
      integer,          intent(out),   optional :: rc  
!         
!
! !DESCRIPTION:
!     Set init code in Clock object to "CREATED".
!
!     The arguments are:
!     \begin{description}
!     \item[{[clock]}] 
!          Specified {\tt ESMF\_Clock} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
      ! Set init code
      if (present(clock)) then
        ESMF_INIT_SET_CREATED(clock)
      endif

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
    
      end subroutine ESMF_ClockSetInitCreated

!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockSetInitDeleted()"
!BOPI
! !IROUTINE: ESMF_ClockSetInitDeleted - Set Clock init code to "DELETED"

! !INTERFACE:
      subroutine ESMF_ClockSetInitDeleted(clock, rc)
!
! !ARGUMENTS:
      type(ESMF_Clock), intent(inout)         :: clock
      integer,          intent(out), optional :: rc  
!         
!
! !DESCRIPTION:
!     Set init code in Clock object to "DELETED".
!
!     The arguments are:
!     \begin{description}
!     \item[clock] 
!          Specified {\tt ESMF\_Clock} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
      ! Set init code
      ESMF_INIT_SET_DELETED(clock)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
    
      end subroutine ESMF_ClockSetInitDeleted

!------------------------------------------------------------------------------

! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockGetThis()"
!BOPI
! !IROUTINE: ESMF_ClockGetThis - Internal access routine for C++ pointer

! !INTERFACE:
      subroutine ESMF_ClockGetThis(clock, this, rc)
!
! !ARGUMENTS:
      type(ESMF_Clock),   intent(in)             :: clock
      type(ESMF_Pointer), intent(out)            :: this
      integer,            intent(out),  optional :: rc  
!         
!
! !DESCRIPTION:
!     Internal access routine for C++ pointer.
!
!     The arguments are:
!     \begin{description}
!     \item[clock] 
!          Specified {\tt ESMF\_Clock} object.
!     \item[this] 
!          C++ pointer.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------

      ! initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
      ! Copy C++ pointer
      this = clock%this

      ! return successfully
      if (present(rc)) rc = ESMF_SUCCESS
    
      end subroutine ESMF_ClockGetThis

!------------------------------------------------------------------------------

! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockSetThis()"
!BOPI
! !IROUTINE: ESMF_ClockSetThis - Set C++ pointer in Clock

! !INTERFACE:
      subroutine ESMF_ClockSetThis(clock, this, rc)
!
! !ARGUMENTS:
      type(ESMF_Clock),   intent(inout)          :: clock
      type(ESMF_Pointer), intent(in)             :: this
      integer,            intent(out),  optional :: rc  
!         
!
! !DESCRIPTION:
!     Set C++ pointer in Clock.
!
!     The arguments are:
!     \begin{description}
!     \item[clock] 
!          Specified {\tt ESMF\_Clock} object.
!     \item[this] 
!          C++ pointer.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------

      ! initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
      ! Copy C++ pointer
      clock%this = this

      ! return successfully
      if (present(rc)) rc = ESMF_SUCCESS
    
      end subroutine ESMF_ClockSetThis

!------------------------------------------------------------------------------

! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockEQAlias()"
!BOPI
! !IROUTINE:  ESMF_ClockEQAlias - Compare two Clocks for equality based on alias
!
! !INTERFACE:
      function ESMF_ClockEQAlias(clock1, clock2)
!
! !RETURN VALUE:
      logical :: ESMF_ClockEQAlias

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock1
      type(ESMF_Clock), intent(in) :: clock2

! !DESCRIPTION:
!     This method overloads the (==) operator for the {\tt ESMF\_Clock}
!     class.  See "interface operator(==)" above for complete description.
!
!EOPI
      ESMF_INIT_TYPE clockinit1, clockinit2
      integer :: localrc1, localrc2
      logical :: lval1, lval2

      ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain
      ! init checks on both args, and in the case where both are uninitialized,
      ! to distinguish equality based on uninitialized type (uncreated,
      ! deleted).

      ! TODO: Consider moving this logic to C++: use Base class? status?
      !       Or replicate logic for C interface also.

      ! check inputs
      clockinit1 = ESMF_ClockGetInit(clock1)
      clockinit2 = ESMF_ClockGetInit(clock2)

      if (clockinit1.eq.ESMF_INIT_CREATED.and. &
        clockinit2.eq.ESMF_INIT_CREATED) then
        ESMF_ClockEQAlias = clock1%this .eq. clock2%this
      else
        ESMF_ClockEQAlias = .false.
      endif

      end function ESMF_ClockEQAlias
!------------------------------------------------------------------------------

      end module ESMF_ClockTypeMod
