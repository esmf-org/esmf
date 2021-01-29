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
#define ESMF_FILENAME "ESMF_TimeIntervalType.F90"
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
#include "ESMF_TimeMgr.inc"

!===============================================================================
!BOPI
!
! !MODULE: ESMF_TimeIntervalTypeMod
!
! !DESCRIPTION:
! Part of Time Manager Fortran API wrapper of C++ implementation.
!
! Defines Fortran types for corresponding C++ class {\tt ESMC\_TimeInterval}.
!
! See {\tt ../include/ESMC\_TimeInterval.h} for complete description.
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
!     ! ESMF_TimeInterval
!
!     ! Fortran class type to match C++ TimeInterval class in size only;
!     !  all dereferencing within class is performed by C++ implementation

!     ! Equivalent sequence and kind to C++:

      type ESMF_TimeInterval
#ifndef ESMF_NO_SEQUENCE
        sequence
#endif
        private                           !   (members opaque on Fortran side)
        ! allocate enough memory for the TimeInterval class data
        ! (set in C++ side) in 8 byte units: 
        !         3*BaseTime + 2*(6*ESMC_Time) + 1*ESMC_Calendar pointer +
        !         3*ESMC_I8
#ifdef ESMF_NO_INITIALIZERS
        integer(ESMF_KIND_I8), dimension(19) :: shallowMemory
#else
        integer(ESMF_KIND_I8), dimension(19) :: shallowMemory = 0
#endif
        ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
!     The types defined in this file are public and propagated up via 
!     ESMF_TimeIntervalMod in ESMF_TimeInterval.F90      

      public ESMF_TimeInterval

!------------------------------------------------------------------------------
! !PUBLIC METHODS:
!     The methods defined in this file are public and propagated up via 
!     ESMF_TimeIntervalMod in ESMF_TimeInterval.F90      

      public ESMF_TimeIntervalGetInit
      public ESMF_TimeIntervalInit

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
#define ESMF_METHOD "ESMF_TimeIntervalGetInit"
!BOPI
! !IROUTINE:  ESMF_TimeIntervalGetInit - Get initialization status.

! !INTERFACE:
      function ESMF_TimeIntervalGetInit(s)
!
! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in), optional :: s
      ESMF_INIT_TYPE                                :: ESMF_TimeIntervalGetInit
!
! !DESCRIPTION:
!     Get the initialization status of the shallow class {\tt timeinterval}.
!
!     The arguments are:
!     \begin{description}
!     \item [{[s]}]
!           {\tt ESMF\_TimeInterval} from which to retrieve status.
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
      type(ESMF_TimeInterval), intent(inout), optional :: s
!
! !DESCRIPTION:
!     Initialize the shallow class {\tt timeinterval}.
!
!     The arguments are:
!     \begin{description}
!     \item [{[s]}]
!           {\tt ESMF\_TimeInterval} being initialized.
!     \end{description}
!
!EOPI

      if (present(s)) then
        ESMF_INIT_SET_DEFINED(s)
      endif

      end subroutine ESMF_TimeIntervalInit

!------------------------------------------------------------------------------

      end module ESMF_TimeIntervalTypeMod
