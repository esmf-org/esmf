! $Id: ESMF_AlarmType.F90,v 1.1 2003/10/22 01:10:40 eschwab Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
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

!===============================================================================
!BOPI
!
! !MODULE: ESMF_AlarmTypeMod
!
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation.
!
! Defines F90 types for corresponding C++ class {\tt ESMC\_Alarm}.
!
! See {\tt ../include/ESMC\_Alarm.h} for complete description.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
!     None: all types defined in this file are public and propagated up
!     via ESMF_AlarmMod in ESMF_Alarm.F90

!------------------------------------------------------------------------------
!     ! ESMF_AlarmListType
!
!     ! F90 "enum" type to match C++ ESMC_AlarmListType enum

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
        type(ESMF_Pointer) :: this       ! opaque pointer to the C++ class data
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
      '$Id: ESMF_AlarmType.F90,v 1.1 2003/10/22 01:10:40 eschwab Exp $'
!------------------------------------------------------------------------------

      end module ESMF_AlarmTypeMod
