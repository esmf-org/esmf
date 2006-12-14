! $Id: ESMF_ClockType.F90,v 1.8 2006/12/14 21:45:02 oehmke Exp $
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
#include <ESMF_TimeMgr.inc>

!===============================================================================
!BOPI
!
! !MODULE: ESMF_ClockTypeMod
!
! !DESCRIPTION:
! Part of Time Manager Fortran API wrapper of C++ implemenation.
!
! Defines Fortran types for corresponding C++ class {\tt ESMC\_Clock}.
!
! See {\tt ../include/ESMC\_Clock.h} for complete description.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_UtilTypesMod
      use ESMF_BaseMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
!     None: all types defined in this file are public and propagated up
!     via ESMF_ClockMod in ESMF_Clock.F90

!------------------------------------------------------------------------------
!     ! ESMF_Clock
!
      type ESMF_Clock
      sequence
      private
#if !defined(ESMF_NO_INITIALIZERS) && !defined(ESMF_AIX_8_INITBUG)
        ! opaque pointer to the C++ class data
        type(ESMF_Pointer) :: this = ESMF_NULL_POINTER 
#else
        type(ESMF_Pointer) :: this
#endif
        ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
!     The types defined in this file are public and propagated up via 
!     ESMF_ClockMod in ESMF_Clock.F90      

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_ClockType.F90,v 1.8 2006/12/14 21:45:02 oehmke Exp $'
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
       ESMF_INIT_TYPE :: ESMF_ClockGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the Deep class {\tt bundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_Clock} from which to retreive status.
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
      end module ESMF_ClockTypeMod
