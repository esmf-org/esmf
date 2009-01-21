! $Id: ESMF_AlarmType.F90,v 1.14.2.3 2009/01/21 21:25:23 cdeluca Exp $
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
#define ESMF_FILENAME "ESMF_AlarmType.F90"
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
#include "ESMF_TimeMgr.inc"

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
        type(ESMF_Pointer) :: this
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
      '$Id: ESMF_AlarmType.F90,v 1.14.2.3 2009/01/21 21:25:23 cdeluca Exp $'
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


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AlarmSetInitCreated()"
!BOPI
! !IROUTINE: ESMF_AlarmSetInitCreated - Set Alarm init code to "CREATED"

! !INTERFACE:
  subroutine ESMF_AlarmSetInitCreated(alarm, rc)
!
! !ARGUMENTS:
    type(ESMF_Alarm), intent(inout)           :: alarm
    integer,          intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Set init code in Alarm object to "CREATED".
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
    type(ESMF_Alarm), intent(inout)           :: alarm
    integer,          intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Set init code in Alarm object to "DELETED".
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
    type(ESMF_Alarm), intent(in), optional :: alarm
    type(ESMF_Pointer), intent(out) :: this
    integer, intent(out),optional :: rc
!
!
! !DESCRIPTION:
! Internal access routine for C++ pointer.
!
! The arguments are:
! \begin{description}
! \item[alarm]
! Specified {\tt ESMF\_Alarm} object.
! \item[this]
! C++ pointer.
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
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
    type(ESMF_Alarm), intent(inout) :: alarm
    type(ESMF_Pointer), intent(in) :: this
    integer, intent(out), optional :: rc
!
!
! !DESCRIPTION:
! Set C++ pointer in Alarm.
!
! The arguments are:
! \begin{description}
! \item[alarm]
! Specified {\tt ESMF\_Alarm} object.
! \item[this]
! C++ pointer.
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
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
