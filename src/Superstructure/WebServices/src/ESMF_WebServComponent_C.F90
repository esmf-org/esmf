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
#define ESMF_FILENAME "ESMF_WebServComponent_C.F90"
!==============================================================================
!
! ESMF Component module

!
!==============================================================================
! A blank line to keep protex happy because there are no public entry
! points in this file, only internal ones.
!BOP

!EOP
!
! This file provides the Fortran subroutine interfaces to be called by the
! c++ software for the purpose of executing the component initialize, run and 
! finalize routines.
!
!------------------------------------------------------------------------------
! INCLUDES

#include "ESMF.h"


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_processinit()"
!BOPI
! !IROUTINE: f_esmf_processinit 
!
! !INTERFACE:
  subroutine f_esmf_processinit(comp, impstate, expstate, clock, phase, rc)
    use ESMF_CompMod
    use ESMF_GridCompMod
    use ESMF_CplCompMod
    use ESMF_StateMod
    use ESMF_ClockMod
    use ESMF_UtilTypesMod
    use ESMF_LogErrMod
    use ESMF_WebServMod

    implicit none

!
! !ARGUMENTS:
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: impstate
    type(ESMF_State) :: expstate
    type(ESMF_Clock) :: clock
    integer :: phase
    integer, intent(out) :: rc
!
!
! !DESCRIPTION:
!   Processes the request to call the grid component initialization routine.
!
! The arguments are:
! \begin{description}
! \item[{[comp]}]
!   {\tt ESMF\_CplComp} object that represents the Grid Component for which
!   the routine is run.
! \item[{[impstate]}]
!   {\tt ESMF\_State} containing import data for coupling. 
! \item[{[expstate]}]
!   {\tt ESMF\_State} containing export data for coupling. 
! \item[{[clock]}]
!   External {\tt ESMF\_Clock} for passing in time information.
! \item[{[phase]}]
!   Indicates whether routines are {\em single-phase} or {\em multi-phase}.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------

    character :: proctype
    integer :: localrc

    proctype = 'I'
    call ESMF_WebServProcessRequest(comp, impstate, expstate, clock, phase, &
                                    proctype, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_VALID, &
                                 msg="Error while processing init request", &
                                 ESMF_CONTEXT, rcToReturn=rc)
        return
    endif

    call ESMF_GridCompInitialize(comp, exportState=expState, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_VALID, &
                                 msg="Error while calling ESMF Initialize", &
                                 ESMF_CONTEXT, rcToReturn=rc)
        return
    endif

  end subroutine
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_cplcompprocessinit()"
!BOPI
! !IROUTINE: f_esmf_cplcompprocessinit 
!
! !INTERFACE:
  subroutine f_esmf_cplcompprocessinit(comp, impstate, expstate, clock, phase, rc)
    use ESMF_CompMod
    use ESMF_GridCompMod
    use ESMF_CplCompMod
    use ESMF_StateMod
    use ESMF_ClockMod
    use ESMF_UtilTypesMod
    use ESMF_LogErrMod
    use ESMF_WebServMod

    implicit none

!
! !ARGUMENTS:
    type(ESMF_CplComp) :: comp
    type(ESMF_State) :: impstate
    type(ESMF_State) :: expstate
    type(ESMF_Clock) :: clock
    integer :: phase
    integer, intent(out) :: rc
!
!
! !DESCRIPTION:
!   Processes the request to call the grid component initialization routine.
!
! The arguments are:
! \begin{description}
! \item[{[comp]}]
!   {\tt ESMF\_CplComp} object that represents the Grid Component for which
!   the routine is run.
! \item[{[impstate]}]
!   {\tt ESMF\_State} containing import data for coupling. 
! \item[{[expstate]}]
!   {\tt ESMF\_State} containing export data for coupling. 
! \item[{[clock]}]
!   External {\tt ESMF\_Clock} for passing in time information.
! \item[{[phase]}]
!   Indicates whether routines are {\em single-phase} or {\em multi-phase}.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------

    character :: proctype
    integer :: localrc

    proctype = 'I'
    call ESMF_WebServCplCompProcessRqst(comp, impstate, expstate, clock, &
                                           phase, proctype, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_VALID, &
                                 msg="Error while processing init request", &
                                 ESMF_CONTEXT, rcToReturn=rc)
        return
    endif

    call ESMF_CplCompInitialize(comp, exportState=expState, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_VALID, &
                                 msg="Error while calling ESMF Initialize", &
                                 ESMF_CONTEXT, rcToReturn=rc)
        return
    endif

  end subroutine
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_processrun()"
!BOPI
! !IROUTINE: f_esmf_processrun 
!
! !INTERFACE:
  subroutine f_esmf_processrun(comp, impstate, expstate, clock, phase, rc)
    use ESMF_CompMod
    use ESMF_GridCompMod
    use ESMF_StateMod
    use ESMF_ClockMod
    use ESMF_UtilTypesMod
    use ESMF_LogErrMod
    use ESMF_WebServMod

    implicit none

!
! !ARGUMENTS:
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: impstate
    type(ESMF_State) :: expstate
    type(ESMF_Clock) :: clock
    integer :: phase
    integer, intent(out) :: rc
!
!
! !DESCRIPTION:
!   Processes the request to call the grid component run routine.
!
! The arguments are:
! \begin{description}
! \item[{[comp]}]
!   {\tt ESMF\_CplComp} object that represents the Grid Component for which
!   the routine is run.
! \item[{[impstate]}]
!   {\tt ESMF\_State} containing import data for coupling. 
! \item[{[expstate]}]
!   {\tt ESMF\_State} containing export data for coupling. 
! \item[{[clock]}]
!   External {\tt ESMF\_Clock} for passing in time information.
! \item[{[phase]}]
!   Indicates whether routines are {\em single-phase} or {\em multi-phase}.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------

    character :: proctype
    integer :: localrc

    proctype = 'R'
    call ESMF_WebServProcessRequest(comp, impstate, expstate, clock, phase, &
                                    proctype, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_VALID, &
                                 msg="Error while processing run request", &
                                 ESMF_CONTEXT, rcToReturn=rc)
        return
    endif

    call ESMF_GridCompRun(comp, exportState=expState, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_VALID, &
                                 msg="Error while calling ESMF Run", &
                                 ESMF_CONTEXT, rcToReturn=rc)
        return
    endif

  end subroutine
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_cplcompprocessrun()"
!BOPI
! !IROUTINE: f_esmf_cplcompprocessrun 
!
! !INTERFACE:
  subroutine f_esmf_cplcompprocessrun(comp, impstate, expstate, clock, phase, rc)
    use ESMF_CompMod
    use ESMF_CplCompMod
    use ESMF_StateMod
    use ESMF_ClockMod
    use ESMF_UtilTypesMod
    use ESMF_LogErrMod
    use ESMF_WebServMod

    implicit none

!
! !ARGUMENTS:
    type(ESMF_CplComp) :: comp
    type(ESMF_State) :: impstate
    type(ESMF_State) :: expstate
    type(ESMF_Clock) :: clock
    integer :: phase
    integer, intent(out) :: rc
!
!
! !DESCRIPTION:
!   Processes the request to call the grid component run routine.
!
! The arguments are:
! \begin{description}
! \item[{[comp]}]
!   {\tt ESMF\_CplComp} object that represents the Grid Component for which
!   the routine is run.
! \item[{[impstate]}]
!   {\tt ESMF\_State} containing import data for coupling. 
! \item[{[expstate]}]
!   {\tt ESMF\_State} containing export data for coupling. 
! \item[{[clock]}]
!   External {\tt ESMF\_Clock} for passing in time information.
! \item[{[phase]}]
!   Indicates whether routines are {\em single-phase} or {\em multi-phase}.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------

    character :: proctype
    integer :: localrc

    proctype = 'R'
    call ESMF_WebServCplCompProcessRqst(comp, impstate, expstate, clock, &
                                           phase, proctype, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_VALID, &
                                 msg="Error while processing run request", &
                                 ESMF_CONTEXT, rcToReturn=rc)
        return
    endif

    call ESMF_CplCompRun(comp, exportState=expState, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_VALID, &
                                 msg="Error while calling ESMF Run", &
                                 ESMF_CONTEXT, rcToReturn=rc)
        return
    endif

  end subroutine
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_processfinal()"
!BOPI
! !IROUTINE: f_esmf_processfinal 
!
! !INTERFACE:
  subroutine f_esmf_processfinal(comp, impstate, expstate, clock, phase, rc)
    use ESMF_CompMod
    use ESMF_GridCompMod
    use ESMF_StateMod
    use ESMF_ClockMod
    use ESMF_UtilTypesMod
    use ESMF_LogErrMod
    use ESMF_WebServMod

    implicit none

!
! !ARGUMENTS:
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: impstate
    type(ESMF_State) :: expstate
    type(ESMF_Clock) :: clock
    integer :: phase
    integer, intent(out) :: rc
!
!
! !DESCRIPTION:
!   Processes the request to call the grid component finalization routine.
!
! The arguments are:
! \begin{description}
! \item[{[comp]}]
!   {\tt ESMF\_GridComp} object that represents the Grid Component for which
!   the routine is run.
! \item[{[impstate]}]
!   {\tt ESMF\_State} containing import data for coupling. 
! \item[{[expstate]}]
!   {\tt ESMF\_State} containing export data for coupling. 
! \item[{[clock]}]
!   External {\tt ESMF\_Clock} for passing in time information.
! \item[{[phase]}]
!   Indicates whether routines are {\em single-phase} or {\em multi-phase}.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------

    character :: proctype
    integer :: localrc

    proctype = 'F'
    call ESMF_WebServProcessRequest(comp, impstate, expstate, clock, phase, &
                                    proctype, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_VALID, &
                                 msg="Error while processing final request", &
                                 ESMF_CONTEXT, rcToReturn=rc)
        return
    endif

!    call ESMF_GridCompFinalize(comp, exportState=expState, rc=localrc)
!    if (localrc /= ESMF_SUCCESS) then
!        call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_VALID, &
!                                 msg="Error while calling ESMF Finalize", &
!                                 ESMF_CONTEXT, rcToReturn=rc)
!        return
!    endif

  end subroutine
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_cplcompprocessfinal()"
!BOPI
! !IROUTINE: f_esmf_cplcompprocessfinal 
!
! !INTERFACE:
  subroutine f_esmf_cplcompprocessfinal(comp, impstate, expstate, clock, phase, rc)
    use ESMF_CompMod
    use ESMF_CplCompMod
    use ESMF_StateMod
    use ESMF_ClockMod
    use ESMF_UtilTypesMod
    use ESMF_LogErrMod
    use ESMF_WebServMod

    implicit none

!
! !ARGUMENTS:
    type(ESMF_CplComp) :: comp
    type(ESMF_State) :: impstate
    type(ESMF_State) :: expstate
    type(ESMF_Clock) :: clock
    integer :: phase
    integer, intent(out) :: rc
!
!
! !DESCRIPTION:
!   Processes the request to call the grid component finalization routine.
!
! The arguments are:
! \begin{description}
! \item[{[comp]}]
!   {\tt ESMF\_CplComp} object that represents the Grid Component for which
!   the routine is run.
! \item[{[impstate]}]
!   {\tt ESMF\_State} containing import data for coupling. 
! \item[{[expstate]}]
!   {\tt ESMF\_State} containing export data for coupling. 
! \item[{[clock]}]
!   External {\tt ESMF\_Clock} for passing in time information.
! \item[{[phase]}]
!   Indicates whether routines are {\em single-phase} or {\em multi-phase}.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------

    character :: proctype
    integer :: localrc

    proctype = 'F'
    call ESMF_WebServCplCompProcessRqst(comp, impstate, expstate, clock, &
                                           phase, proctype, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_VALID, &
                                 msg="Error while processing final request", &
                                 ESMF_CONTEXT, rcToReturn=rc)
        return
    endif

    call ESMF_CplCompFinalize(comp, exportState=expState, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_VALID, &
                                 msg="Error while calling ESMF Finalize", &
                                 ESMF_CONTEXT, rcToReturn=rc)
        return
    endif

  end subroutine
!------------------------------------------------------------------------------

