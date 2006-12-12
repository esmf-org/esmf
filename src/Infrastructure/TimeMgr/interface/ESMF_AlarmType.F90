! $Id: ESMF_AlarmType.F90,v 1.7 2006/12/12 22:36:32 samsoncheung Exp $
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
!     ESMF AlarmType Module
      module ESMF_AlarmTypeMod
!
!==============================================================================
!
! This file contains the Alarm class definition.  The Alarm class methods
! are defined in ESMF_Alarm.F90.  This split is to resolve mutual method
! dependency with ESMF_Clock.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF_TimeMgr.inc>

!===============================================================================
!BOPI
!
! !MODULE: ESMF_AlarmTypeMod
!
! !DESCRIPTION:
! Part of Time Manager Fortran API wrapper of C++ implemenation.
!
! Defines Fortran types for corresponding C++ class {\tt ESMC\_Alarm}.
!
! See {\tt ../include/ESMC\_Alarm.h} for complete description.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_UtilTypesMod
      use ESMF_BaseMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
!     None: all types defined in this file are public and propagated up
!     via ESMF_AlarmMod in ESMF_Alarm.F90

!------------------------------------------------------------------------------
!     ! ESMF_AlarmListType
!
!     ! Fortran "enum" type to match C++ ESMC_AlarmListType enum

      type ESMF_AlarmListType
      sequence
      private
        integer :: alarmListType
      end type

      type(ESMF_AlarmListType), parameter :: &
                          ESMF_ALARMLIST_ALL         = ESMF_AlarmListType(1), &
                          ESMF_ALARMLIST_RINGING     = ESMF_AlarmListType(2), &
                          ESMF_ALARMLIST_NEXTRINGING = ESMF_AlarmListType(3), &
                          ESMF_ALARMLIST_PREVRINGING = ESMF_AlarmListType(4)

!------------------------------------------------------------------------------
!     ! ESMF_Alarm
!
      type ESMF_Alarm
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
!     ESMF_AlarmMod in ESMF_Alarm.F90

!------------------------------------------------------------------------------
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_AlarmType.F90,v 1.7 2006/12/12 22:36:32 samsoncheung Exp $'
!------------------------------------------------------------------------------

      contains

!==============================================================================
!BOPI
! !IROUTINE:  ESMF_AlarmGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_AlarmGetInit(d)
!
! !ARGUMENTS:
       type(ESMF_Alarm), intent(inout), optional :: d
       ESMF_INIT_TYPE :: ESMF_AlarmGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the Deep class {\tt alarm}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_Alarm} from which to retreive status.
!     \end{description}
!
!EOPI

       if (present(d)) then
         ESMF_AlarmGetInit = ESMF_INIT_GET(d)
       else
         ESMF_AlarmGetInit = ESMF_INIT_CREATED
       endif

    end function ESMF_AlarmGetInit

!------------------------------------------------------------------------------

      end module ESMF_AlarmTypeMod
