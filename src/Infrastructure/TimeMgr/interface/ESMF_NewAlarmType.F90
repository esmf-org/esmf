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
#define ESMF_FILENAME "ESMF_NewAlarmType.F90"
!==============================================================================
!
!     ESMF NewAlarmType Module
      module ESMF_NewAlarmTypeMod
!
!==============================================================================
!
! This file contains the NewAlarm class data definition, as well as internal 
! methods.  The NewAlarm class methods are defined in ESMF_NewAlarm.F90.  This split
! is to resolve mutual method dependency with ESMF_Clock.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF_TimeMgr.inc"

!===============================================================================
!BOPI
!
! !MODULE: ESMF_NewAlarmTypeMod
!
! !DESCRIPTION:
! Part of Time Manager Fortran API wrapper of C++ implementation.
!
! Defines Fortran types for corresponding C++ class {\tt ESMCI\_NewAlarm}.
!
! See {\tt ../include/ESMCI\_NewAlarm.h} for complete description.
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
!     ! ESMF_NewAlarmList_Flag
!
!     ! Fortran "enum" type to match C++ ESMC_NewAlarmList_Flag enum

      type ESMF_NewAlarmList_Flag
#ifndef ESMF_NO_SEQUENCE
      sequence
#endif
      private
        integer :: newalarmlistflag
      end type

      type(ESMF_NewAlarmList_Flag), parameter :: &
                          ESMF_NEWALARMLIST_ALL         = ESMF_NewAlarmList_Flag(1), &
                          ESMF_NEWALARMLIST_RINGING     = ESMF_NewAlarmList_Flag(2), &
                          ESMF_NEWALARMLIST_NEXTRINGING = ESMF_NewAlarmList_Flag(3), &
                          ESMF_NEWALARMLIST_PREVRINGING = ESMF_NewAlarmList_Flag(4)

!------------------------------------------------------------------------------
!     ! ESMF_NewAlarm
!
      type ESMF_NewAlarm
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
!     ESMF_NewAlarmMod in ESMF_NewAlarm.F90

      public ESMF_NewAlarmList_Flag
      public ESMF_NEWALARMLIST_ALL, ESMF_NEWALARMLIST_RINGING, &
             ESMF_NEWALARMLIST_NEXTRINGING, ESMF_NEWALARMLIST_PREVRINGING
      public ESMF_NewAlarm

!------------------------------------------------------------------------------
! !PUBLIC METHODS:
!     The methods defined in this file are public and propagated up via 
!     ESMF_NewAlarmMod in ESMF_NewAlarm.F90

      public ESMF_NewAlarmGetInit
      public ESMF_NewAlarmSetInitCreated
      public ESMF_NewAlarmSetInitDeleted
      public ESMF_NewAlarmGetThis
      public ESMF_NewAlarmSetThis

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
! !IROUTINE:  ESMF_NewAlarmGetInit - Get initialization status.

! !INTERFACE:
      function ESMF_NewAlarmGetInit(d)
!
! !ARGUMENTS:
      type(ESMF_NewAlarm), intent(in), optional :: d
      ESMF_INIT_TYPE                         :: ESMF_NewAlarmGetInit
!
! !DESCRIPTION:
!     Get the initialization status of the Deep class {\tt newalarm}.
!
!     The arguments are:
!     \begin{description}
!     \item [{[d]}]
!           {\tt ESMF\_NewAlarm} from which to retrieve status.
!     \end{description}
!
!EOPI

      if (present(d)) then
        ESMF_NewAlarmGetInit = ESMF_INIT_GET(d)
      else
        ESMF_NewAlarmGetInit = ESMF_INIT_CREATED
      endif

      end function ESMF_NewAlarmGetInit

!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NewAlarmSetInitCreated()"
!BOPI
! !IROUTINE: ESMF_NewAlarmSetInitCreated - Set NewAlarm init code to "CREATED"

! !INTERFACE:
      subroutine ESMF_NewAlarmSetInitCreated(newalarm, rc)
!
! !ARGUMENTS:
      type(ESMF_NewAlarm), intent(inout)         :: newalarm
      integer,          intent(out), optional :: rc  
!         
!
! !DESCRIPTION:
!     Set init code in NewAlarm object to "CREATED".
!
!     The arguments are:
!     \begin{description}
!     \item[newalarm] 
!          Specified {\tt ESMF\_NewAlarm} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
      ! Set init code
      ESMF_INIT_SET_CREATED(newalarm)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
    
      end subroutine ESMF_NewAlarmSetInitCreated

!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NewAlarmSetInitDeleted()"
!BOPI
! !IROUTINE: ESMF_NewAlarmSetInitDeleted - Set NewAlarm init code to "DELETED"

! !INTERFACE:
      subroutine ESMF_NewAlarmSetInitDeleted(newalarm, rc)
!
! !ARGUMENTS:
      type(ESMF_NewAlarm), intent(inout)         :: newalarm
      integer,          intent(out), optional :: rc  
!         
!
! !DESCRIPTION:
!     Set init code in NewAlarm object to "DELETED".
!
!     The arguments are:
!     \begin{description}
!     \item[newalarm] 
!          Specified {\tt ESMF\_NewAlarm} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
      ! Set init code
      ESMF_INIT_SET_DELETED(newalarm)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
    
      end subroutine ESMF_NewAlarmSetInitDeleted

!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_NewAlarmGetThis - Internal access routine for C++ pointer
! !INTERFACE:
      subroutine ESMF_NewAlarmGetThis(newalarm, this, rc)
!
! !ARGUMENTS:
      type(ESMF_NewAlarm),   intent(in)            :: newalarm
      type(ESMF_Pointer), intent(out)           :: this
      integer,            intent(out), optional :: rc
!
!
! !DESCRIPTION:
!     Internal access routine for C++ pointer.
!
!     The arguments are:
!     \begin{description}
!     \item[newalarm]
!          Specified {\tt ESMF\_NewAlarm} object.
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
      this = newalarm%this

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_NewAlarmGetThis

!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_NewAlarmSetThis - Set C++ pointer in NewAlarm
! !INTERFACE:
      subroutine ESMF_NewAlarmSetThis(newalarm, this, rc)
!
! !ARGUMENTS:
      type(ESMF_NewAlarm),   intent(inout)          :: newalarm
      type(ESMF_Pointer), intent(in)             :: this
      integer,            intent(out), optional  :: rc
!
!
! !DESCRIPTION:
!     Set C++ pointer in NewAlarm.
!
!     The arguments are:
!     \begin{description}
!     \item[newalarm]
!          Specified {\tt ESMF\_NewAlarm} object.
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
      newalarm%this = this

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_NewAlarmSetThis

!------------------------------------------------------------------------------

      end module ESMF_NewAlarmTypeMod
