! $Id: ESMF_TimeType.F90,v 1.17.2.1 2010/02/05 20:00:36 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research,
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
      use ESMF_InitMacrosMod
      use ESMF_UtilTypesMod

      ! associated derived types

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
        sequence
        private                            !  (members opaque on Fortran side)
#ifdef ESMF_NO_INITIALIZERS
        integer(ESMF_KIND_I8), dimension(6) :: shallowMemory
#else
        integer(ESMF_KIND_I8), dimension(6) :: shallowMemory = 0
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
      '$Id: ESMF_TimeType.F90,v 1.17.2.1 2010/02/05 20:00:36 svasquez Exp $'
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

        s%shallowMemory = 0
        ESMF_INIT_SET_DEFINED(s)

    end subroutine ESMF_TimeInit

!------------------------------------------------------------------------------
  
      end module ESMF_TimeTypeMod
