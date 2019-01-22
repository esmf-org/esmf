! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_AlarmType.F90"
!==============================================================================
!
!     ESMF AlarmType Module
      module ESMF_AlarmTypeMod
!
!==============================================================================
!
! This file contains the Alarm class data definition, as well as internal 
! methods.  The Alarm class methods are defined in ESMF_Alarm.F90.  This split
! is to resolve mutual method dependency with ESMF_Clock.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF_TimeMgr.inc"

!===============================================================================
!BOPI
!
! !MODULE: ESMF_AlarmTypeMod
!
! !DESCRIPTION:
! Part of Time Manager Fortran API wrapper of C++ implementation.
!
! Defines Fortran types for corresponding C++ class {\tt ESMCI\_Alarm}.
!
! See {\tt ../include/ESMCI\_Alarm.h} for complete description.
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
!     ! ESMF_AlarmList_Flag
!
!     ! Fortran "enum" type to match C++ ESMC_AlarmList_Flag enum

      type ESMF_AlarmList_Flag
#ifndef ESMF_NO_SEQUENCE
      sequence
#endif
      private
        integer :: alarmlistflag
      end type

      type(ESMF_AlarmList_Flag), parameter :: &
                          ESMF_ALARMLIST_ALL         = ESMF_AlarmList_Flag(1), &
                          ESMF_ALARMLIST_RINGING     = ESMF_AlarmList_Flag(2), &
                          ESMF_ALARMLIST_NEXTRINGING = ESMF_AlarmList_Flag(3), &
                          ESMF_ALARMLIST_PREVRINGING = ESMF_AlarmList_Flag(4)

!------------------------------------------------------------------------------
!     ! ESMF_Alarm
!
      type ESMF_Alarm
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
!     ESMF_AlarmMod in ESMF_Alarm.F90

      public ESMF_AlarmList_Flag
      public ESMF_ALARMLIST_ALL, ESMF_ALARMLIST_RINGING, &
             ESMF_ALARMLIST_NEXTRINGING, ESMF_ALARMLIST_PREVRINGING
      public ESMF_Alarm

!------------------------------------------------------------------------------
! !PUBLIC METHODS:
!     The methods defined in this file are public and propagated up via 
!     ESMF_AlarmMod in ESMF_Alarm.F90

      public ESMF_AlarmGetInit
      public ESMF_AlarmSetInitCreated
      public ESMF_AlarmSetInitDeleted
      public ESMF_AlarmGetThis
      public ESMF_AlarmSetThis

!------------------------------------------------------------------------------
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id$'
!------------------------------------------------------------------------------

      contains

!==============================================================================
!BOPI
! !IROUTINE:  ESMF_AlarmGetInit - Get initialization status.

! !INTERFACE:
      function ESMF_AlarmGetInit(d)
!
! !ARGUMENTS:
      type(ESMF_Alarm), intent(in), optional :: d
      ESMF_INIT_TYPE                         :: ESMF_AlarmGetInit
!
! !DESCRIPTION:
!     Get the initialization status of the Deep class {\tt alarm}.
!
!     The arguments are:
!     \begin{description}
!     \item [{[d]}]
!           {\tt ESMF\_Alarm} from which to retrieve status.
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

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AlarmSetInitCreated()"
!BOPI
! !IROUTINE: ESMF_AlarmSetInitCreated - Set Alarm init code to "CREATED"

! !INTERFACE:
      subroutine ESMF_AlarmSetInitCreated(alarm, rc)
!
! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout)         :: alarm
      integer,          intent(out), optional :: rc  
!         
!
! !DESCRIPTION:
!     Set init code in Alarm object to "CREATED".
!
!     The arguments are:
!     \begin{description}
!     \item[alarm] 
!          Specified {\tt ESMF\_Alarm} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
      ! Set init code
      ESMF_INIT_SET_CREATED(alarm)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
    
      end subroutine ESMF_AlarmSetInitCreated

!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AlarmSetInitDeleted()"
!BOPI
! !IROUTINE: ESMF_AlarmSetInitDeleted - Set Alarm init code to "DELETED"

! !INTERFACE:
      subroutine ESMF_AlarmSetInitDeleted(alarm, rc)
!
! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout)         :: alarm
      integer,          intent(out), optional :: rc  
!         
!
! !DESCRIPTION:
!     Set init code in Alarm object to "DELETED".
!
!     The arguments are:
!     \begin{description}
!     \item[alarm] 
!          Specified {\tt ESMF\_Alarm} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
      ! Set init code
      ESMF_INIT_SET_DELETED(alarm)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
    
      end subroutine ESMF_AlarmSetInitDeleted

!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_AlarmGetThis - Internal access routine for C++ pointer
! !INTERFACE:
      subroutine ESMF_AlarmGetThis(alarm, this, rc)
!
! !ARGUMENTS:
      type(ESMF_Alarm),   intent(in)            :: alarm
      type(ESMF_Pointer), intent(out)           :: this
      integer,            intent(out), optional :: rc
!
!
! !DESCRIPTION:
!     Internal access routine for C++ pointer.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          Specified {\tt ESMF\_Alarm} object.
!     \item[this]
!          C++ pointer.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Copy C++ pointer
      this = alarm%this

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_AlarmGetThis

!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_AlarmSetThis - Set C++ pointer in Alarm
! !INTERFACE:
      subroutine ESMF_AlarmSetThis(alarm, this, rc)
!
! !ARGUMENTS:
      type(ESMF_Alarm),   intent(inout)          :: alarm
      type(ESMF_Pointer), intent(in)             :: this
      integer,            intent(out), optional  :: rc
!
!
! !DESCRIPTION:
!     Set C++ pointer in Alarm.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          Specified {\tt ESMF\_Alarm} object.
!     \item[this]
!          C++ pointer.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Copy C++ pointer
      alarm%this = this

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_AlarmSetThis

!------------------------------------------------------------------------------

      end module ESMF_AlarmTypeMod
