! $Id: ESMF_ClockType.F90,v 1.14.2.3 2009/01/21 21:25:23 cdeluca Exp $
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
#define ESMF_FILENAME "ESMF_ClockType.F90"
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
#include "ESMF_TimeMgr.inc"

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
        type(ESMF_Pointer) :: this
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
      '$Id: ESMF_ClockType.F90,v 1.14.2.3 2009/01/21 21:25:23 cdeluca Exp $'
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

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockSetInitCreated()"
!BOPI
! !IROUTINE: ESMF_ClockSetInitCreated - Set Clock init code to "CREATED"

! !INTERFACE:
  subroutine ESMF_ClockSetInitCreated(clock, rc)
!
! !ARGUMENTS:
    type(ESMF_Clock), intent(inout)           :: clock
    integer,          intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Set init code in Clock object to "CREATED".
!
!     The arguments are:
!     \begin{description}
!     \item[clock] 
!          Specified {\tt ESMF\_Clock} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------

    ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Set init code
    ESMF_INIT_SET_CREATED(clock)

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_ClockSetInitCreated
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockSetInitDeleted()"
!BOPI
! !IROUTINE: ESMF_ClockSetInitDeleted - Set Clock init code to "DELETED"

! !INTERFACE:
  subroutine ESMF_ClockSetInitDeleted(clock, rc)
!
! !ARGUMENTS:
    type(ESMF_Clock), intent(inout)           :: clock
    integer,          intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Set init code in Clock object to "DELETED".
!
!     The arguments are:
!     \begin{description}
!     \item[clock] 
!          Specified {\tt ESMF\_Clock} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------

    ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Set init code
    ESMF_INIT_SET_DELETED(clock)

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_ClockSetInitDeleted
!------------------------------------------------------------------------------



      end module ESMF_ClockTypeMod
