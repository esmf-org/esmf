! $Id: ESMF_TimeIntervalType.F90,v 1.8 2006/12/14 22:03:14 oehmke Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2008, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
!     ESMF TimeIntervalType Module
      module ESMF_TimeIntervalTypeMod
!
!==============================================================================
!
! This file contains the TimeInterval class definition.  The TimeInterval class
! methods are defined in ESMF_TimeInterval.F90.  This split is to resolve
! mutual dependency with ESMF_Time.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF_TimeMgr.inc>

!===============================================================================
!BOPI
!
! !MODULE: ESMF_TimeIntervalTypeMod
!
! !DESCRIPTION:
! Part of Time Manager Fortran API wrapper of C++ implemenation.
!
! Defines Fortran types for corresponding C++ class {\tt ESMC\_TimeInterval}.
!
! See {\tt ../include/ESMC\_TimeInterval.h} for complete description.
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from ESMF base class
      use ESMF_UtilTypesMod

      ! inherit from base time class
      use ESMF_BaseTimeMod

      ! associated derived types
      use ESMF_TimeTypeMod
      use ESMF_CalendarMod

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
!     None: all types defined in this file are public and propagated up
!     via ESMF_TimeIntervalMod in ESMF_TimeInterval.F90

!------------------------------------------------------------------------------
!     ! ESMF_TimeInterval
!
!     ! Fortran class type to match C++ TimeInterval class in size only;
!     !  all dereferencing within class is performed by C++ implementation

!     ! Equivalent sequence and kind to C++:

      type ESMF_TimeInterval
      sequence                             ! match C++ storage order
      private                              !   (members opaque on Fortran side)
        type(ESMF_BaseTime)   :: baseTime  ! inherit base class
        type(ESMF_Time)       :: startTime ! start time for absolute calendar
!                                              intervals
        type(ESMF_Time)       :: endTime   ! end time for absolute calendar
!                                              intervals
#if !defined(ESMF_NO_INITIALIZERS) && !defined(ESMF_AIX_8_INITBUG)
        type(ESMF_Calendar), pointer :: calendar => NULL() ! associated calendar
        integer(ESMF_KIND_I8) :: yy = 0    ! calendar interval number of years
        integer(ESMF_KIND_I8) :: mm = 0    ! calendar interval number of months
        integer(ESMF_KIND_I8) :: d  = 0    ! calendar interval number of days
#else
        type(ESMF_Calendar), pointer :: calendar  ! associated calendar
        integer(ESMF_KIND_I8) :: yy        ! calendar interval number of years
        integer(ESMF_KIND_I8) :: mm        ! calendar interval number of months
        integer(ESMF_KIND_I8) :: d         ! calendar interval number of days
#endif
        ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
!     The types defined in this file are public and propagated up via 
!     ESMF_TimeIntervalMod in ESMF_TimeInterval.F90      

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_TimeIntervalType.F90,v 1.8 2006/12/14 22:03:14 oehmke Exp $'
!------------------------------------------------------------------------------

      contains

!==============================================================================

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalGetInit"
!BOPI
! !IROUTINE:  ESMF_TimeIntervalGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_TimeIntervalGetInit(s)
!
! !ARGUMENTS:
       type(ESMF_TimeInterval), intent(in), optional :: s
       ESMF_INIT_TYPE :: ESMF_TimeIntervalGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the shallow class {\tt timeinterval}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_TimeInterval} from which to retreive status.
!     \end{description}
!
!EOPI

       if (present(s)) then
         ESMF_TimeIntervalGetInit = ESMF_INIT_GET(s)
       else
         ESMF_TimeIntervalGetInit = ESMF_INIT_DEFINED
       endif

    end function ESMF_TimeIntervalGetInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalInit"
!BOPI
! !IROUTINE:  ESMF_TimeIntervalInit - Initialize TimeInterval

! !INTERFACE:
    subroutine ESMF_TimeIntervalInit(s)
!
! !ARGUMENTS:
       type(ESMF_TimeInterval) :: s
!
! !DESCRIPTION:
!      Initialize the shallow class {\tt timeinterval}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_TimeInterval} of which being initialized.
!     \end{description}
!
!EOPI
        nullify(s%calendar)
        s%yy = 0
        s%mm = 0
        s%d  = 0
        ESMF_INIT_SET_DEFINED(s)
    end subroutine ESMF_TimeIntervalInit

!------------------------------------------------------------------------------

      end module ESMF_TimeIntervalTypeMod
