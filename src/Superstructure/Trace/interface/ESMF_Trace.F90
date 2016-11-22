! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_Trace.F90"
!==============================================================================
!
!     ESMF Trace module
module ESMF_TraceMod

#include "ESMF.h"

  use ESMF_UtilTypesMod
  use ESMF_LogErrMod 
  use ESMF_CompMod
  use ESMF_GridCompMod
  use ESMF_BaseMod
  use ESMF_VMMod

  implicit none
  private

  public ESMF_TraceOpen
  public ESMF_TraceClose
  public ESMF_TraceEventPhaseEnter
  public ESMF_TraceEventPhaseExit
  public ESMF_TraceEventPhasePrologueEnter
  public ESMF_TraceEventPhasePrologueExit
  public ESMF_TraceEventPhaseEpilogueEnter
  public ESMF_TraceEventPhaseEpilogueExit
  public ESMF_TraceEventComponentInfo

contains

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceOpen()"
  subroutine ESMF_TraceOpen(bufSize, traceDir, streamId, rc)
    integer,          intent(in)             :: bufSize
    character(len=*), intent(in)             :: traceDir
    integer,          intent(in)             :: streamId
    integer,          intent(out), optional  :: rc
    
    call c_esmftrace_open(bufSize, traceDir, streamId, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
  end subroutine ESMF_TraceOpen
  
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceClose()"
  subroutine ESMF_TraceClose(rc)
    integer,          intent(out), optional :: rc    
    
    call c_esmftrace_close(rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
  end subroutine ESMF_TraceClose

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceEventPhaseEnter()"
  subroutine ESMF_TraceEventPhaseEnter(comp, rc)
    type(ESMF_GridComp), intent(in) :: comp
    integer, intent(out), optional  :: rc

    ! locals
    integer :: vmid
    integer :: baseid
    type(ESMF_Method_Flag) :: method
    integer :: method_enum
    integer :: phase

    rc = ESMF_SUCCESS 

    call ESMF_TraceGetCompID(comp, vmid, baseid, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_GridCompGet(comp, currentMethod=method, &
         currentPhase=phase, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    method_enum = ESMF_TraceMethodToEnum(method)

    call c_esmftrace_phase_enter(vmid, baseid, method_enum, phase, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_TraceEventPhaseEnter

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceEventPhaseExit()"
  subroutine ESMF_TraceEventPhaseExit(comp, rc)
    type(ESMF_GridComp), intent(in) :: comp
    integer, intent(out), optional  :: rc

    ! locals
    integer :: vmid
    integer :: baseid
    type(ESMF_Method_Flag) :: method
    integer :: method_enum
    integer :: phase

    rc = ESMF_SUCCESS

    call ESMF_TraceGetCompID(comp, vmid, baseid, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_GridCompGet(comp, currentMethod=method, &
         currentPhase=phase, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    method_enum = ESMF_TraceMethodToEnum(method)

    call c_esmftrace_phase_exit(vmid, baseid, method_enum, phase, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
  end subroutine ESMF_TraceEventPhaseExit

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceEventPhasePrologueEnter()"
  subroutine ESMF_TraceEventPhasePrologueEnter(comp, rc)
    type(ESMF_GridComp), intent(in) :: comp
    integer, intent(out), optional  :: rc

    ! locals
    integer :: vmid
    integer :: baseid
    type(ESMF_Method_Flag) :: method
    integer :: method_enum
    integer :: phase

    rc = ESMF_SUCCESS 

    call ESMF_TraceGetCompID(comp, vmid, baseid, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_GridCompGet(comp, currentMethod=method, &
         currentPhase=phase, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    method_enum = ESMF_TraceMethodToEnum(method)

    call c_esmftrace_phase_prologue_enter(vmid, baseid, method_enum, phase, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_TraceEventPhasePrologueEnter

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceEventPhasePrologueExit()"
  subroutine ESMF_TraceEventPhasePrologueExit(comp, rc)
    type(ESMF_GridComp), intent(in) :: comp
    integer, intent(out), optional  :: rc

    ! locals
    integer :: vmid
    integer :: baseid
    type(ESMF_Method_Flag) :: method
    integer :: method_enum
    integer :: phase

    rc = ESMF_SUCCESS

    call ESMF_TraceGetCompID(comp, vmid, baseid, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_GridCompGet(comp, currentMethod=method, &
         currentPhase=phase, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    method_enum = ESMF_TraceMethodToEnum(method)

    call c_esmftrace_phase_prologue_exit(vmid, baseid, method_enum, phase, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
  end subroutine ESMF_TraceEventPhasePrologueExit

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceEventPhaseEpilogueEnter()"
  subroutine ESMF_TraceEventPhaseEpilogueEnter(comp, rc)
    type(ESMF_GridComp), intent(in) :: comp
    integer, intent(out), optional  :: rc

    ! locals
    integer :: vmid
    integer :: baseid
    type(ESMF_Method_Flag) :: method
    integer :: method_enum
    integer :: phase

    rc = ESMF_SUCCESS 

    call ESMF_TraceGetCompID(comp, vmid, baseid, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_GridCompGet(comp, currentMethod=method, &
         currentPhase=phase, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    method_enum = ESMF_TraceMethodToEnum(method)

    call c_esmftrace_phase_epilogue_enter(vmid, baseid, method_enum, phase, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_TraceEventPhaseEpilogueEnter

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceEventPhaseEpilogueExit()"
  subroutine ESMF_TraceEventPhaseEpilogueExit(comp, rc)
    type(ESMF_GridComp), intent(in) :: comp
    integer, intent(out), optional  :: rc

    ! locals
    integer :: vmid
    integer :: baseid
    type(ESMF_Method_Flag) :: method
    integer :: method_enum
    integer :: phase

    rc = ESMF_SUCCESS

    call ESMF_TraceGetCompID(comp, vmid, baseid, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_GridCompGet(comp, currentMethod=method, &
         currentPhase=phase, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    method_enum = ESMF_TraceMethodToEnum(method)

    call c_esmftrace_phase_epilogue_exit(vmid, baseid, method_enum, phase, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
  end subroutine ESMF_TraceEventPhaseEpilogueExit


#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceEventComponentInfo()"
  subroutine ESMF_TraceEventComponentInfo(comp, rc)
    type(ESMF_GridComp), intent(in) :: comp
    integer, intent(out), optional :: rc    

    ! local
    integer                    :: baseid
    integer                    :: vmid
    character(len=ESMF_MAXSTR) :: name
    
    rc = ESMF_SUCCESS
    
    call ESMF_TraceGetCompID(comp, vmid, baseid, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_GridCompGet(comp, name=name, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
    
    call c_esmftrace_component_info(vmid, baseid, trim(name), rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_TraceEventComponentInfo
  
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceEventGetCompID()"
  subroutine ESMF_TraceGetCompID(comp, vmid, baseid, rc)
    type(ESMF_GridComp), intent(in)            :: comp
    integer,             intent(out)           :: vmid
    integer,             intent(out)           :: baseid
    integer,             intent(out), optional :: rc    
    
    ! local
    type(ESMF_VMId), pointer   :: vmidptr(:)
    character                  :: vmkey
    
    rc = ESMF_SUCCESS
    
    call ESMF_BaseGetID(comp%compp%base, baseid, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    
    allocate(vmidptr(1))
    call ESMF_BaseGetVMId(comp%compp%base, vmidptr(1), rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
    
    call c_ESMCI_VMIdGet (vmidptr(1), vmid, vmkey, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
    
    deallocate(vmidptr)
    
  end subroutine ESMF_TraceGetCompID

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceMethodToEnum()"
  function ESMF_TraceMethodToEnum(method_flag)
    integer :: ESMF_TraceMethodToEnum
    type(ESMF_Method_Flag), intent(in) :: method_flag
    
    if (method_flag == ESMF_METHOD_INITIALIZE .or. &
         method_flag == ESMF_METHOD_INITIALIZEIC) then
       ESMF_TraceMethodToEnum = 0
    elseif (method_flag == ESMF_METHOD_RUN .or. &
         method_flag == ESMF_METHOD_RUNIC) then
       ESMF_TraceMethodToEnum = 1
    elseif (method_flag == ESMF_METHOD_FINALIZE .or. &
         method_flag ==  ESMF_METHOD_FINALIZEIC) then
       ESMF_TraceMethodToEnum = 2
    else
       ESMF_TraceMethodToEnum = -1  ! not supported
    endif

  end function ESMF_TraceMethodToEnum

end module
