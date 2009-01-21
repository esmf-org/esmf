! $Id: ESMF_TimeType.F90,v 1.9.2.3 2009/01/21 21:25:23 cdeluca Exp $
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
#define ESMF_FILENAME "ESMF_TimeType.F90"
!==============================================================================
!
!     ESMF TimeType Module
      module ESMF_TimeTypeMod
!
!==============================================================================
!
! This file contains the Time class definition.  The Time class methods
! are defined in ESMF_Time.F90.  This split is to resolve mutual 
! dependency with ESMF_TimeInterval.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF_TimeMgr.inc"
#include "ESMF.h"
!===============================================================================
!BOPI
!
! !MODULE: ESMF_TimeTypeMod
!
! !DESCRIPTION:
! Part of Time Manager Fortran API wrapper of C++ implemenation.
!
! Defines Fortran types for corresponding C++ class {\tt ESMC\_Time}.
!
! See {\tt ../include/ESMC\_Time.h} for complete description.
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from base time class
      use ESMF_INITMacrosMod
      use ESMF_UtilTypesMod

      ! associated derived types
      use ESMF_CalendarMod

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
!     None: all types defined in this file are public and propagated up
!     via ESMF_TimeMod in ESMF_Time.F90

!------------------------------------------------------------------------------
!     ! ESMF_Time
!
!     ! Fortran class type to match C++ Time class in size only;
!     !  all dereferencing within class is performed by C++ implementation

!     ! Equivalent sequence and kind to C++:

      type ESMF_Time
      sequence                              ! match C++ storage order
      private                               !  (members opaque on Fortran side)
      
      ! first members must match ESMC_BaseTime, i.e. ESMC_Fraction on C++ side
#ifndef ESMF_NO_INITIALIZERS
      integer(ESMF_KIND_I8) :: s    = 0   ! whole seconds
      integer(ESMF_KIND_I4) :: sN   = 0   ! fractional seconds, numerator
      integer(ESMF_KIND_I4) :: sD   = 0   ! fractional seconds, denominator
#else
      integer(ESMF_KIND_I8) :: s          ! whole seconds
      integer(ESMF_KIND_I4) :: sN         ! fractional seconds, numerator
      integer(ESMF_KIND_I4) :: sD         ! fractional seconds, denominator
#endif      
      ! following members must match additional ESMC_Time members on C++ side
#ifndef ESMF_NO_INITIALIZERS
      type(ESMF_Pointer)    :: calendar = ESMF_NULL_POINTER ! associated calendar
      integer               :: timeZone = 0       ! local timezone
#else
      type(ESMF_Pointer)    :: calendar           ! associated calendar
      integer               :: timeZone           ! local timezone
#endif


#ifdef NOSKIP
      type(ESMF_BaseTime)          :: baseTime  ! inherit base class
#ifndef ESMF_NO_INITIALIZERS
      type(ESMF_Calendar), pointer :: calendar => NULL() ! associated calendar
      integer                      :: timeZone = 0 ! local timezone
      integer                      :: pad      = 0 ! to satisfy halem compiler
#else
      type(ESMF_Calendar), pointer :: calendar  ! associated calendar
      integer                      :: timeZone  ! local timezone
      integer                      :: pad       ! to satisfy halem compiler
#endif
#endif

      ESMF_INIT_DECLARE
     end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
!     The types defined in this file are public and propagated up via 
!     ESMF_TimeMod in ESMF_Time.F90      

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_TimeType.F90,v 1.9.2.3 2009/01/21 21:25:23 cdeluca Exp $'
!------------------------------------------------------------------------------

      contains

!==============================================================================
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeGetInit"
!BOPI
! !IROUTINE:  ESMF_TimeGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_TimeGetInit(s)
!
! !ARGUMENTS:
       type(ESMF_Time), intent(in), optional :: s
       ESMF_INIT_TYPE :: ESMF_TimeGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the shallow class {\tt time}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_Time} from which to retreive status.
!     \end{description}
!
!EOPI

       if (present(s)) then
         ESMF_TimeGetInit = ESMF_INIT_GET(s)
       else
         ESMF_TimeGetInit = ESMF_INIT_DEFINED
       endif

    end function ESMF_TimeGetInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeInit"
!BOPI
! !IROUTINE:  ESMF_TimeInit - Initialize Time

! !INTERFACE:
    subroutine ESMF_TimeInit(s)
!
! !ARGUMENTS:
       type(ESMF_Time) :: s
!
! !DESCRIPTION:
!      Initialize the shallow class {\tt time}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_Time} of which being initialized.
!     \end{description}
!
!EOPI
    ! Note: ESMF_TimeType is private
        s%s        = 0
        s%sN       = 0
        s%sD       = 0
        s%calendar = ESMF_NULL_POINTER
        s%timeZone = 0
        ESMF_INIT_SET_DEFINED(s)
    end subroutine ESMF_TimeInit

!------------------------------------------------------------------------------
  
      end module ESMF_TimeTypeMod
